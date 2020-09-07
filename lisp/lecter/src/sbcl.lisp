;;;; lecter.lisp

(in-package #:lecter)
;;; ---------------------------------------------------------------------
;;; load and initialize the delectus library
;;; ---------------------------------------------------------------------

(defparameter $libdelectus-path (asdf:system-relative-pathname :lecter "../../libDelectus.dylib"))

(define-foreign-library libdelectus
    (:darwin "libDelectus.dylib")
    (t (:default "libDelectus.dylib")))
   
(defun load-libDelectus ()
  (let ((cffi:*foreign-library-directories*
         (cons $libdelectus-path cffi:*foreign-library-directories*)))
    (use-foreign-library libdelectus)))

;;; (load-libDelectus)

;;; ---------------------------------------------------------------------
;;; Delectus library api functions
;;; ---------------------------------------------------------------------

;;; gambit APIs

(defcfun ("___release_string" %release-string) :void (s :string))

;;; Delectus APIs

(defcfun ("initDelectus" %init-delectus) :int)

;;; (%init-delectus)

(defcfun ("finalizeDelectus" %finalize-delectus) :int)

(defcfun ("version" %delectus-version) :string)

;;; (%delectus-version)

(defcfun ("read_delectus_file" %read-delectus-file) :int (path :string))

;;; (setf $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (with-foreign-string (s $movies-path)(%read-delectus-file s))
