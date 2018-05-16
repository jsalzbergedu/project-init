;; project-init-utils.el -- project-init utilities -*- lexical-binding: t-*-
;;; Commentary:
;; Provides convenience functions for project-init.
;;; Code:
(require 'f)
(require 'cl-lib)

(defvar project-init-use-encoding 'utf-8-unix "Which encoding to use.")

(defun project-init--pushd (dir thunk)
  "Temporarily switch to DIR and run THUNK."
  (with-temp-buffer (cd dir)
                    (funcall thunk)))

(defmacro project-init--withd (dir &rest body)
  "Temporarily switch to DIR and execute BODY."
  (declare (indent defun))
  `(project-init--pushd ,dir (lambda () ,@body)))

(defun project-init--expand-file-names (dir-or-file &rest dirs-or-files)
  "Expand beginning at DIR-OR-FILE and ending at the last DIRS-OR-FILES.
Like this: expand a b c d = a/b/c/d"
  (cl-reduce (lambda (a b) (expand-file-name b a))
             dirs-or-files
             :initial-value dir-or-file))

(defun project-init--write-maybe (filename s)
  "Add a file at FILENAME containing S if FILENAME does not exist."
  (unless (f-exists-p filename)
    (f-write s project-init-use-encoding filename)))

(defun project-init--mkdir-maybe (dir)
  "Add a directory at DIR if DIR does not exist."
  (unless (f-exists-p dir)
    (f-mkdir dir)))

(defun project-init--add-file (project-name filename)
  "Add empty file to PROJECT-NAME at FILENAME, provided FILENAME doesn't exist."
  (unless (f-exists-p (expand-file-name filename project-name))
    (project-init--write-maybe (expand-file-name filename project-name) "")))

(defun project-init--add-dir (project-name dirname)
  "Add empty file to PROJECT-NAME at DIRNAME, provided DIRNAME doesn't exist."
  (unless (f-exists-p (expand-file-name dirname project-name))
    (f-mkdir (expand-file-name dirname project-name))))

(provide 'project-init-utils)
;;; project-init-utils.el ends here
