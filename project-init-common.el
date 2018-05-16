;; project-init-common.el --- common to all projects -*- lexical-binding t -*-
;;; Commentary:
;; Provides a structure and recipie for projects.
;; This is an Emacs oriented structure, and assumes you already
;; have dir-locals.el and .projectile in a global gitignore.
;;; Code:

(defvar project-init-common '()
  "A list of functions to run when a project is initialized.
All of these functions take the name of the project being
initialized as an argument.")

(defun project-init (project-name language-specific)
  "Run the functions in PROJECT-INIT-COMMON using PROJECT-NAME.
Then run LANGUAGE-SPECIFIC functions."
  (mapc (lambda (f) (apply f (list project-name)))
        (append project-init-common language-specific)))

(defun project-init--make-directory-maybe (project-name)
  "If PROJECT-NAME is not already a directory, make it so."
  (unless (f-dir-p project-name) (make-directory project-name)))

(defun project-init--git-init-maybe (project-name)
  "Initialize git if it is not initialized in PROJECT-NAME."
  (project-init--withd project-name
    (unless (f-dir-p ".git") (call-process "git" nil nil nil "init"))))

(defun project-init--add-dirlocals (project-name)
  "Add .dir-locals.el to PROJECT-NAME."
  (project-init--add-file project-name ".dir-locals.el"))

(defun project-init--add-projectile (project-name)
  "Add .projectile to PROJECT-NAME."
  (project-init--add-file project-name ".projectile"))

(push 'project-init--make-directory-maybe project-init-common)
(push 'project-init--git-init-maybe project-init-common)
(push 'project-init--add-dirlocals project-init-common)
(push 'project-init--add-projectile project-init-common)
(setq project-init-common (nreverse project-init-common))

(provide 'project-init-common)
;;; project-init-common ends here
