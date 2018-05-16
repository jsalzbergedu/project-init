;;; project-init.el --- initialize projects -*- lexical-binding: t -*-
;;; Commentary:
;; A project initialization scheme
;; aimed at providing recipies and skeletons for projects.
;;; Code:

(require 'cl-lib)

(require 'project-init-utils)
(require 'project-init-common)
(require 'project-init-skeletons)
(require 'project-init-layout)
(require 'project-init-settings)

(defmacro project-init--make-x-project (&rest body)
  "Make language specific functions using BODY.
Return a lambda that can be wrapped by a proper function."
  (declare (indent defun))
  `(lambda (project-name)
     (project-init project-name (list (lambda (project-name) ,@body)))))

;;;###autoload
(defun project-init-make-idris-project (project-name)
  "Using PROJECT-NAME, make an empty idris project.
Mandates a project structure."
  (interactive "sProject name: ")
  (funcall (project-init--make-x-project
             (project-init--layout-populate project-name
                                            project-init--idris-layout!))
           project-name))

;;;###autoload
(defun project-init-make-java-project! (project-name)
  (interactive "sProject name: ")
  (funcall (project-init--make-x-project
             (project-init--withd project-name
               (call-process "gradle" nil nil nil
                             "init" "--type" "java-library"))
             (project-init--java-cleanup! project-name)
             (project-init--layout-populate project-name
                                            project-init--java-layout!))
           project-name))

(provide 'project-init)
;;; project-init.el ends here
