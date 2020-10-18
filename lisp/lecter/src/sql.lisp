;;;; ***********************************************************************
;;;;
;;;; Name:          sql.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       utilities for generating SQL
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)


(defvar *template-parser* (cl-strings:make-template-parser "{{" "}}"))

(defun template-environment (bindings-plist)
  (mapcar (lambda (binding)
            (cons (symbol-name (car binding))
                  (cdr binding)))
          (plist-to-alist bindings-plist)))

;;; (template-environment '(:NAME "Fred" :DAY "Tuesday"))

;;; because we use SYMBOL-NAME to convert keywords to template variables,
;;; the template expressions must capitalize the variable names
;;; by convention I also capitalize the keywords for visual congruence
(defmethod parse-template ((template string) &rest bindings)
  (funcall *template-parser* template (template-environment bindings)))

;;; (parse-template "Hello {{NAME}}, it's {{DAY}}" :NAME "Fred" :DAY "Tuesday")
