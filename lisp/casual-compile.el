;;; casual-compile.el --- Casual Compile Interface     -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alex Bennée

;; Author: Alex Bennée <alex.bennee@linaro.org>
;; Keywords: tools, compile

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Casual Compile Interface
;;
;; This aims to be a batteries included transient interface to
;; `compile' which allows the user to select targets based on feedback
;; from the build tools. It also enables easy selection of env
;; variables which are often used to select build types.
;;

;;; Code:


(require 'casual)
(require 'cl-lib)
(require 'files-x)

(defvar casual-compile-saved-options nil
  "List of default compile options.")

;; c.f. counsel--dominating-file
(defun casual-compile--dominating-file (file &optional dir)
  "Look up directory hierarchy for FILE, starting in DIR.
Like `locate-dominating-file', but DIR defaults to
`default-directory' and the return value is expanded."
  (and (setq dir (locate-dominating-file (or dir default-directory) file))
       (expand-file-name dir)))

;;
;; Project root finders/helpers
;;
;; In a perfect world there would be one source of truth, however this is
;; Emacs so lets make the heuristics both deep and configurable.
;;

(defun casual-compile--projectile-root ()
  "Return root of current projectile project or nil on failure.
Use `projectile-project-root' to determine the root."
  (and (fboundp 'projectile-project-root)
       (projectile-project-root)))

(defun casual-compile--project-current ()
  "Return root of current project or nil on failure.
Use `project-current' to determine the root."
  (let ((proj (and (fboundp 'project-current)
                   (project-current))))
    (cond ((not proj) nil)
          ((fboundp 'project-root)
           (project-root proj))
          ((fboundp 'project-roots)
           (car (project-roots proj))))))

(defun casual-compile--configure-root ()
  "Return root of current project or nil on failure.
Use the presence of a \"configure\" file to determine the root."
  (casual-compile--dominating-file "configure"))

(defun casual-compile--git-root ()
  "Return root of current project or nil on failure.
Use the presence of a \".git\" file to determine the root."
  (casual-compile--dominating-file ".git"))

(defun casual-compile--dir-locals-root ()
  "Return root of current project or nil on failure.
Use the presence of a `dir-locals-file' to determine the root."
  (casual-compile--dominating-file dir-locals-file))

(defun casual-compile--buffer-default-directory ()
  "Return the `default-directory' of the current buffer."
  default-directory)

(defvar casual-compile--root-functions
  '(casual-compile--projectile-root
    casual-compile--project-current
    casual-compile--configure-root
    casual-compile--git-root
    casual-compile--dir-locals-root
    casual-compile--buffer-default-directory)
  "Special hook to find the project root for compile commands.
Each function on this hook is called in turn with no arguments
and should return either a directory, or nil if no root was
found.")

(defun casual-compile--compile-root ()
  "Return root of current project or signal an error on failure.
The root is determined by `casual-compile--root-functions'."
  (or (run-hook-with-args-until-success 'casual-compile--root-functions)
      (error "Couldn't find project root")))

;; We use .dir-locals-2.el as .dir-locals is often a version
;; controlled file in software projects.
(defun casual-compile--dir-local-file()
  "Return the file for casual's project local settings.
We take care to put our dir locals in the project root. We rely on the
  normal .dir-locals code to load the values for other files in the project."
  (concat (casual-compile--compile-root) ".dir-locals-2.el"))

;;
;; Build directory helpers.
;;

(defcustom casual-compile-build-directories
  '("build" "builds" "bld" ".build")
  "List of potential build subdirectory names to check for."
  :type '(repeat directory))

(defun casual-compile--find-build-subdir (srcdir)
  "Return builds subdirectory of SRCDIR, if one exists."
  (cl-some (lambda (dir)
             (setq dir (expand-file-name dir srcdir))
             (and (file-directory-p dir) dir))
           casual-compile-build-directories))

(defun casual-compile--get-build-subdirs (blddir)
  "Return all subdirs under BLDDIR sorted by modification time."
  (let* ((files (directory-files-and-attributes
                 blddir t directory-files-no-dot-files-regexp t))
         (dirs (cl-remove-if-not #'file-directory-p (mapcar #'car files))))
    ;; Include blddir itself if it contains non-directory files.
    (unless (cl-every #'file-directory-p (mapcar #'car files))
      (push blddir dirs))
    (sort dirs (lambda (a b)
                 (file-newer-than-file-p a b))))) ; Newest first

(defun casual-compile-get-build-directories (&optional dir)
  "Return a list of potential build directories."
  (let* ((srcdir (or dir (casual-compile--compile-root)))
         (blddir (casual-compile--find-build-subdir srcdir)))
    (and blddir (casual-compile--get-build-subdirs blddir))))

;;
;; Target helpers
;;

(defvar casual-compile-make-phony-pattern "^\\.PHONY:[\t ]+\\(.+\\)$"
  "Regexp for extracting phony targets from Makefiles.")

(defvar casual-compile-help-pattern
  "\\(?:^\\(\\*\\)?[[:space:]]+\\([^[:space:]]+\\)[[:space:]]+-\\)"
  "Regexp for extracting help targets from a make help call.")

;; This is loosely based on the Bash Make completion code which
;; relies on GNUMake having the following return codes:
;;   0 = no-rebuild, -q & 1 needs rebuild, 2 error
(defun casual-compile--make-targets (build-dir &optional env)
  "Return a list of Make targets for DIR.

Return a single blank target (so we invoke the default target)
if Make exits with an error.  This might happen because some sort
of configuration needs to be done first or the source tree is
pristine and being used for multiple build trees."
  (with-temp-buffer
    (let* ((res (apply '
                 call-process
                 "make"
                 nil t nil
                 "-C" build-dir "-nqp" env))
           targets)
      (if (or (not (numberp res)) (> res 1))
          (list "")
        (goto-char (point-min))
        (while (re-search-forward casual-compile-make-phony-pattern nil t)
          (push (split-string (match-string-no-properties 1)) targets))
        (sort (apply #'nconc targets) #'string-lessp)))))

;; (rx (one-or-more blank) (group (one-or-more (not (in blank ":")))))
(defvar casual-compile-ninja-target-pattern
  "[[:blank:]]+\\([^:[:blank:]]+\\)"
  "Regexp for extracting target names from Ninja query output.")

(defun casual-compile--ninja-targets (build-dir &optional env)
  "Return a list of Make targets for DIR.

Return a single blank target (so we invoke the default target)
if Make exits with an error.  This might happen because some sort
of configuration needs to be done first or the source tree is
pristine and being used for multiple build trees."
  (with-temp-buffer
    (let* ((res (call-process
                 "ninja" nil t nil
                 "-C" build-dir "-t" "query" "all"))
           targets)
      (if (or (not (numberp res)) (> res 1))
          (list "")
        (goto-char (point-min))
        (while (re-search-forward casual-compile-ninja-target-pattern nil t)
          (push (split-string (match-string-no-properties 1)) targets))
        (sort (apply #'nconc targets) #'string-lessp)))))


(defvar casual-compile--target-functions
  '(("make" . casual-compile--make-targets)
    ("ninja" . casual-compile--ninja-targets))
  "Alist of compiler tool names and their target helper functions.
The functions take the build directory as a single argument.")

;;
;; Environment handling helpers
;;

(defvar casual-compile-env nil
  "List of NAME=VAR pairs for the environment variables.")
(make-variable-buffer-local 'casual-compile-env)
(put 'casual-compile-env 'permanent-local t)

(defvar casual-compile-env-history nil
  "History for `casual-compile'")

(defvar casual-compile-env-pattern
  "[_[:digit:][:upper:]]+=[/[:alnum:]]*"
  "Pattern to match valid environment variables.")

(defun casual-compile-env-p (cand)
    "Predicate for testing `CAND' is a valid env value."
    (string-match-p casual-compile-env-pattern cand))

(transient-define-suffix casual-compile-add-env (&optional args)
  "Add environment variable to compile invocation.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let ((new-env
         (completing-read "Env: " nil nil
                          #'casual-compile-env-p nil
                          casual-compile-env-history)))
    (when new-env
      (push new-env casual-compile-env))))

(transient-define-suffix casual-compile-del-env (&optional args)
  "Remove environment variable to compile invocation.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let ((del-env
         (completing-read "Env: " casual-compile-env nil t)))
    (when del-env
      (setq casual-compile-env (delete del-env casual-compile-env)))))

;;
;; Format the final build string
;;
;; Given the tool and directory lets we return the final string that
;; makes up the compile command.
;;

(defcustom casual-compile-make-args (format "-j%d -k " (+ 1 my-core-count))
  "Additional arguments for make.
You may, for example, want to add \"-jN\" for the number of cores
N in your system."
  :type 'string
  :group 'compile)

(defun casual-compile--make-formatter (build-dir &optional target env)
  "Format make command with optional parallelism."
  (format "make %s -C %s %s %s"
          casual-compile-make-args
          (or build-dir default-directory)
          (or target "all")
          (if env
              (mapconcat #'identity env " ")
            "")))

(defvar casual-compile--format-functions
  '(("make" . casual-compile--make-formatter)
    ("ninja" . "ninja -C %s %s"))
  "Alist of compiler tool names and their format functions/strings.
The functions take the build directory as a single argument.")

(defun casual-compile--format-compile (cmd dir &optional target env)
  "Given strings describing `CMD' and the `DIR' return a
formatted string. Lookup the helper from
  `casual-compile--format-functions' which is an alist of the form
  indexed by `tool' and returning either a format string or a function
  that will return a formatted string when called with build-dir"
  (let ((formatter (cdr (assoc cmd casual-compile--format-functions))))
    (if (functionp formatter)
        (funcall formatter dir target env)
      (format formatter dir target env))))

;;
;; Compile command handling
;;
;; The command in this case could be a direct compiler invocation to a
;; invoking a build tool.
;;

; nb: use -invocation so variable not picked up by risky-local-variable-p
(defvar casual-compile-invocation "make"
  "The command we use to compile be it direct compiler or build tool.")
(make-variable-buffer-local 'casual-compile-invocation)
(put 'casual-compile-invocation 'permanent-local t)

(defvar casual-compile-invocation-history nil
  "Command history for `casual-compile'")

;; currently dumb, we can make smarter
(defun casual-compile--get-tools ()
  "Return a list of commands and tools we could use"
  '("make" "ninja"))

(transient-define-suffix casual-compile-get-invocation (&optional args)
  "Read the build command we are going to use.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let ((cmd (completing-read "Command: "
                              (casual-compile--get-tools)
                              nil nil
                              casual-compile-invocation-history)))
    (when cmd
      (setq casual-compile-invocation cmd))))

;;
;; Build directory handling
;;

(defvar casual-compile-directory nil
  "The directory we will be building in.")
(make-variable-buffer-local 'casual-compile-directory)
(put 'casual-compile-directory 'permanent-local t)

(defvar casual-compile-directory-history nil
  "Build directory history for `casual-compile'")

(transient-define-suffix casual-compile-get-dir (&optional args)
  "Read the directory command we are going to build in.
ARGS used for transient arguments."
  :transient t
  (interactive (list (transient-args transient-current-command)))
  (let* ((base-dir
          (or (casual-compile--compile-root)  default-directory))
         (build-dir
          (completing-read
           "Build Directory: "
           (casual-compile-get-build-directories base-dir)
           nil t base-dir casual-compile-directory-history)))
    (when build-dir
      (setq casual-compile-directory build-dir))))

;;
;; Target handling
;;
;; The list of targets will depend on the build tool we are using as
;; we need to query it to get a list of targets.
;;

(defvar casual-compile-target nil
  "The target we want to build.")
(make-variable-buffer-local 'casual-compile-target)
(put 'casual-compile-target 'permanent-local t)

(defvar casual-compile-target-history nil
  "Target history for `casual-compile'")

(transient-define-suffix casual-compile-get-target (&optional args)
  "Read the build command we are going to use.
ARGS used for transient arguments."
  :transient t
  (interactive)
  (let ((helper (cdr (assoc casual-compile-invocation
                       casual-compile--target-functions))))
    (setq casual-compile-target
          (if helper
              (completing-read
               "Command: "
               (funcall helper casual-compile-directory casual-compile-env)
               nil t casual-compile-target-history)
            "all"))))

;;
;; Save/Restore values
;;
;; We actually define buffer-local variables to hold all the details
;; we need. However really these values are "project" wide so we want
;; the same value to hold whatever file in a project we happen to be
;; in when we hit casual-compile.
;;
;; To do this we utilise .dir-locals stored in the project root. We
;; reply on emacs to load the values automatically for each new file
;; in the project and we update the state before we execute each
;; compile.
;;

(defvar casual-compile-project-variable-list
  '(casual-compile-invocation
    casual-compile-directory
    casual-compile-target
    casual-compile-env)
  "List of casual compile variables we save in the project.")

(defun casual-compile--save-project-vars ()
  "Save the casual-compile variables to the project."
  (let ((dir-local (casual-compile--dir-local-file)))
    (mapc
     (lambda (var-symbol)
       (when (symbol-value var-symbol)
           (save-excursion
             (modify-dir-local-variable
              nil
              var-symbol
              (symbol-value var-symbol)
              'add-or-replace
              dir-local))))
          casual-compile-project-variable-list)))

;;
;; These define the immediate helpers and actions for the transient
;; defined bellow. All the options will eventually form to a command
;; which will be executed as a compile.
;;
;; Depending on the build system we will either pass the build
;; directory or cd into it to do the right thing.
;;

(transient-define-suffix casual-compile--do-compile (&optional args)
  "Run compiler. We can command and directory from args."
  (interactive)
  ;; update our dir-locals
  (casual-compile--save-project-vars)

  ;; Grab the *current* buffer's effective values so we can duplicate
  ;; them in the compilation buffer for if the user re-invokes
  ;; casual-compile there.
  (let* ((invocation casual-compile-invocation)
         (directory casual-compile-directory)
         (target casual-compile-target)
         (env casual-compile-env)
         (hook (lambda (_comp-cmd)
                 ;; use the magic of lexical binding...
                 (setq-local casual-compile-invocation invocation)
                 (setq-local casual-compile-directory directory)
                 (setq-local casual-compile-target target)
                 (setq-local casual-compile-env env)
                 (message "Compilation buffer locals set: Cmd=%S, Dir=%S, Target=%S, Env=%S"
                          invocation directory target env))))

  (unwind-protect
      (add-hook 'compilation-start-hook hook)
      (compile (casual-compile--format-compile
                casual-compile-invocation
                casual-compile-directory
                casual-compile-target
                casual-compile-env))
      (remove-hook 'compilation-start-hook hook))))

(defun casual-compile--show-current-compile ()
  "Return a Transient menu headline to indicate the current compile command."
  (message "doing cc-show-current-compile")
  (concat (propertize "Compile: " 'face 'transient-heading)
          (propertize
           (casual-compile--format-compile
            (or casual-compile-invocation "make")
            (or casual-compile-directory default-directory)
            (or casual-compile-target "all")
            casual-compile-env)
           'face 'transient-value)
          "\n"))

;;
;; Finally the root transient itself
;;

;;;###autoload (autoload 'casual-compile "casual-compile" nil t)
(transient-define-prefix casual-compile ()
  "Compile project with a transient interface."

  ;; Add refresh-suffixes to update the display when values change
  :refresh-suffixes t

  ["Compile"
   :description casual-compile--show-current-compile

   ["Settings"
    ("C" "Compile Command" casual-compile-get-invocation)
    ("D" "Build Dir" casual-compile-get-dir)
    ("T" "Target" casual-compile-get-target)]

   ["Environment"
    ("a" "Add Env Var" casual-compile-add-env)
    ("d" "Del Env Var" casual-compile-del-env)

   ]]

  [["Actions"
    ("C-c C-c" "Compile" casual-compile--do-compile)]])

(provide 'casual-compile)
;;; casual-compile.el ends here
