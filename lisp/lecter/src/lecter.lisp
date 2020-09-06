;;;; lecter.lisp

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

