;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(defpackage #:lecter
  (:use #:cl #:cffi #:sqlite))
