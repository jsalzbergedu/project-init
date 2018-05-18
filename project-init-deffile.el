;;; project-init-deffile -- macro for defining files -*- lexical-binding: t -*-
;;; Commentary:
;; Only load this with eval-when-compile -- it does not use
;; a clean namespace.
;; This file contains a deffile macro, which will be useful for defining
;; filenames and contents for project skeletons.
;; High level example:
;; (deffile doooooot
;;    :takes-project-name t
;;    :docstring ("Make doot with PROJECT-NAME.")
;;    :filename "doot.txt"
;;    :contents ("a" "b" "c"))
;; Becomes
;; (defun doooooot
;;     (project-name)
;;   (list "doot.txt"
;;         (concat
;;          (concat "a" "
;; ")
;;          (concat
;;           (concat "b" "
;; ")
;;           (concat "c" "")))))
;;
;;; Code:
(require 'dash)

(defun project-init--expand-concats (strings)
  "Expand (concat s1 (concat s2 ...)) on STRINGS."
  (-reduce-r-from (lambda (a b) `(concat ,a ,b))
                  ""
                  strings))

(defun project-init--butlast-newline (strings)
  "Add newlines to all STRINGS but the last."
  (append (-map (lambda (a) `(concat ,a "\n")) (butlast strings))
          (last strings)))

(defun project-init--concat-with-newlines (strings)
  "Concatenate and add newlines to STRINGS."
  (project-init--expand-concats (project-init--butlast-newline strings)))

(defun project-init--concat-with-newlines-f (strings)
  "Concatenate and add newlines to STRINGS, in a function context."
  (-reduce-r-from 'concat "" (append (-map (lambda (a) (concat a "\n"))
                                           (butlast strings))
                                     (last strings))))

(defun project-init--pair-off (list)
  "Pair off every two elemetns in LIST.
Returns nil if the list is not even."
  (when (cl-evenp (length list))
    (let ((list list) pairs)
      (while list
	(push (list (pop list) (pop list)) pairs))
      (nreverse pairs))))

(defun project-init--pair-access (key paired-off)
  "Get KEY from PAIRED-OFF list."
  (let ((filtered
         (-filter (lambda (a) (eq (car a) key))
                  paired-off)))
    (when filtered
      (cadr (car filtered)))))

(defmacro deffile (name &rest keys-and-values)
  "Create a function to return the NAME and contents of a file.
NAME is the name of the function.
KEYS-AND-VALUES include:
:takes-project-name, whose default value is nil.  E.g.
\(deffile Makefile \"\" :takes-project-name t)
If :takes-project-name is t, the name of the project name will
be bound to project-name.
:docstring, which defaults to nil
When the docstring is multiple lines, you can break it up like this:
\(\"line one\"\ \"line two\" \"line three\") etc.
Important note: :docstring can only be strings,
not just any forms evaluating to strings.
:filename, which defaults to NAME.  E.g.
\(deffile gitignore \"\" :filename \".gitignore\")
:filename can be any form evaluating to a string.
:contents, which is a list of lines.
:contents default value is (\"\").
:contents example: \(deffile compile.s
                            :contents \"cc hello.c\")
The elements of :contents can be any forms evaluating to a string."
  (declare (indent defun))
  (let* ((paired (project-init--pair-off keys-and-values))
         (takes-project-name (project-init--pair-access :takes-project-name
                                                       paired))
         (filename-keyval (project-init--pair-access :filename paired))
         (filename (or filename-keyval name))
         (contents-keyval (project-init--pair-access :contents paired))
         (contents (or contents-keyval '("")))
         (docstring-keyval (project-init--pair-access :docstring paired))
         (docstring (when docstring-keyval
                      (project-init--concat-with-newlines-f docstring-keyval))))
    (put name 'function-documentation docstring)
    `(defun ,name ,(if takes-project-name
                       '(project-name)
                     '(&optional _))
       (list ,filename ,(project-init--concat-with-newlines contents)))))

(provide 'project-init-deffile)
;;; project-init-deffile ends here
