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

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#-abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi))

;;; needed in order to load :sqlite on Lispworks
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi))

#+(and lispworks darwin)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (pathname "/usr/local/Cellar/sqlite/3.33.0/lib/")
           cffi:*foreign-library-directories*))

;;; ---------------------------------------------------------------------

;;; needed in order to load :sqlite and :hunchentoot on ABCL:
;;; ---------------------------------------------------------------------
;;; NOTES: need Maven 3.x.x installed to make this work

#+abcl
(setf abcl-asdf:*mvn-libs-directory* "/usr/local/Cellar/maven/3.6.3_1/libexec/lib/")

#+abcl
(asdf:defsystem :jna
  :defsystem-depends-on (abcl-asdf)
  :components ((:mvn "net.java.dev.jna/jna" :version "5.6.0")))

;;; ---------------------------------------------------------------------


(asdf:defsystem #:lecter
    :description "Lecter: reading Delectus 1.x files using libDelectus"
    :author "mikel evins <mikel@evins.net>"
    :license  "Apache 2.0"
    :version "1.5.2"
    :serial t
    :depends-on (:cffi :sqlite :cl-strings :fare-csv :cl-intbytes :binascii :uuid :local-time :jonathan :sxql)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "version")
                                       (:file "cffi")
                                       (:file "utils")
                                       (:file "identities")
                                       (:file "bind")
                                       (:file "sql")
                                       (:file "sqlite")
                                       (:file "conditions")
                                       (:file "delectus1")))))

;;; (asdf:load-system :lecter)
;;; (ql:quickload :lecter)
