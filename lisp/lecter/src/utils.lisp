;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

(defmacro with-released-string+ptr (&body body)
  (let ((string+ptr (gensym)))
    `(let* ((,string+ptr ,@body)
            (str (first ,string+ptr))
            (ptr (second ,string+ptr)))
       (%release-string ptr)
       str)))

(defun plist-to-alist (plist)
  (loop for tail on plist by #'cddr collect (cons (first tail)(second tail))))

;;; (plist-to-alist '("a" "1" "b" "2"))
