;;;; ***********************************************************************
;;;;
;;;; Name:          lecter.asd
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem #:lecter
    :description "Lecter: reading Delectus 1.x files using libDelectus"
    :author "mikel evins <mikel@evins.net>"
    :license  "Apache 2.0"
    :version "1.5.2"
    :serial t
    :depends-on (:cffi :sqlite)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "cffi")))))

;;; (asdf:load-system :lecter)
;;; (ql:quickload :lecter)
