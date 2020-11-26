;;;; ***********************************************************************
;;;;
;;;; Name:          bind.lisp
;;;; Project:       delectus 2
;;;; Purpose:       the BIND macro
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; a more compact alternative to MUTLTIPLE-VALUE-BIND. BIND works
;;; like LET*, but can bind multiple variables to the values returned
;;; from an expression it works to use it exactly like LET*, with
;;; bindings like this: (x (+ 2 3)) but can also bind multiple return
;;; values, like this: (x y z (values 1 2 3))

(defmacro bind (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (let* ((first-binding (first bindings))
             (rest-bindings (rest bindings))
             (binding-count (length first-binding))
             (vars (subseq first-binding 0 (1- binding-count)))
             (val-expr (elt first-binding (1- binding-count))))
        `(multiple-value-bind (,@vars) ,val-expr
           (bind ,rest-bindings ,@body)))))

