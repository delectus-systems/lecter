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
(defmethod parse-template ((template string) &rest bindings)
  (funcall *template-parser* template (plist-to-alist bindings)))

;;; (parse-template "Hello {{name}}, it's {{day}}" "name" "Fred" "day" "Tuesday")
