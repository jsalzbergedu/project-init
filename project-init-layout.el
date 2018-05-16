;;; project-init-layout.el --- operations on layouts -*- lexical-binding: t -*-
;;; Commentary:
;;; Operations on project layouts.
;; All the function definitions needed in order to unpack a project
;; layout and then generate it.
;;; Code:

;; TODO: use something other than primative type
;; predicates to differentiate peices of the project structure
(require 'project-init-utils)
(require 'project-init-settings)

;; Functions for navigating the layout structure:
(defun project-init--layout-get-dirname (layout)
  "Get the dirname at the top level of LAYOUT."
  (car layout))

(defun project-init--layout-get-files (layout)
  "Get the files from LAYOUT."
  (cl-loop for file in (cdr layout)
           while (functionp file)
           collect file))

(defun project-init--layout-get-subdirs (layout)
  "Get a list of subdirectories in LAYOUT."
  (while (not (listp (car layout)))
    (pop layout))
  layout)

;; TODO remove recursion (by flattening recursive structure)
(defun project-init--layout-populate-rec (project-name layout)
  "Populate a project named PROJECT-NAME with LAYOUT.
The initial conditions need to be modified a bit before this is useful."
  (let ((dirname (project-init--layout-get-dirname layout)))
    (project-init--make-directory-maybe dirname)
    (project-init--withd dirname
      (mapc (lambda (a) (apply 'project-init--write-maybe
                          (funcall a project-name)))
            (project-init--layout-get-files layout))
      (mapc (lambda (a) (project-init--layout-populate-rec project-name a))
            (project-init--layout-get-subdirs layout)))))

(defun project-init--layout-populate (project-name layout)
  "Take PROJECT-NAME and LAYOUT, create a project skeleton."
  (project-init--layout-populate-rec project-name (cons project-name layout))
  '())

(provide 'project-init-layout)
;;; project-init-layout.el ends here
