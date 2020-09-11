;;;; ***********************************************************************
;;;;
;;;; Name:          lispworks.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       lispworks-specific FFI wrappers
;;;;                included for reference; cffi.lisp makes this code unnecessary
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

;;; ---------------------------------------------------------------------
;;; load and initialize the delectus library
;;; ---------------------------------------------------------------------

(defparameter $libdelectus-path (asdf:system-relative-pathname :lecter "../../libDelectus.dylib"))

(defun load-libDelectus ()
  (fli:register-module $libdelectus-path))

;;; ---------------------------------------------------------------------
;;; Delectus library api functions
;;; ---------------------------------------------------------------------

;;; gambit APIs

(fli:define-foreign-function 
    (release-gambit-string "___release_string" :source)
    ((s (:pointer :char)))
  :result-type :void
  :language :ansi-c)

;;; Delectus APIs

(fli:define-foreign-function 
    (init-delectus "initDelectus" :source)
    ()
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function 
    (finalize-delectus "finalizeDelectus" :source)
    ()
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function 
    (delectus-version-string "version" :source)
    ()
  :result-type (:pointer :char)
  :language :ansi-c)

;;; returns an int that identifies the delectus document in
;;; libDelectus memory that is created when the library reads the file
(fli:define-foreign-function 
    (read-delectus-file "read_delectus_file" :source)
    ((path (:pointer (:unsigned :char))))
  :result-type :int
  :language :ansi-c)

;;; ---------------------------------------------------------------------
;;; higher-level (lispier) wrapper functions
;;; ---------------------------------------------------------------------

(defun get-delectus-version ()
  (let* ((version-char* (delectus-version-string))
         (version-string (fli:convert-from-foreign-string version-char*)))
    (release-gambit-string version-char*)
    version-string))

;;; (load-libDelectus)
;;; (init-delectus)
;;; (get-delectus-version)
;;; (fli:with-foreign-string (str element-count byte-count)"/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus" (read-delectus-file str))



