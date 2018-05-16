;;; project-init-settings.el --- project-init options -*- lexical-binding: t -*-

;;; Commentary:

;; This file defines variables like the author's name and email,
;; which will be used in the configuration files of projects.

;;; Code:

;;;###autoload
(defgroup project-init nil
  "Settings for project-init."
  :group 'convenience)

(defcustom project-init-author-name ""
  "The name of the person or people authoring the projects."
  :group 'project-init
  :type 'string)

(defcustom project-init-author-email ""
  "The email of the person or people authoring the projects."
  :group 'project-init
  :type 'string)

(provide 'project-init-settings)
;;; project-init-settings.el ends here
