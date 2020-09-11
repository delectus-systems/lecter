;;;; ***********************************************************************
;;;;
;;;; Name:          delectus.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       interacting with delectus documents
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

(defvar *delectus-initialized* nil)

(defun init-delectus ()
  (if *delectus-initialized*
      nil
      (progn (load-libDelectus)
             (%init-delectus)
             (setf *delectus-initialized* t))))

(defparameter *pathname->document-id-table* (make-hash-table :test 'equal))
(defparameter *document-id->pathname-table* (make-hash-table :test 'eql))

(defmethod read-delectus-v1-file ((path pathname))
  (read-delectus-v1-file (namestring path)))

(defmethod read-delectus-v1-file ((path string))
  (let ((docid (with-foreign-string (s path)
                 (%read-delectus-file s))))
    (if (equal docid $OBJ_NO_OID)
        (error "Unable to read file: ~S" path)
        (progn (setf (gethash path *pathname->document-id-table*)
                     docid)
               (setf (gethash docid *document-id->pathname-table*)
                     path)
               docid))))

(defmethod pathname->document-id ((path string))
  (gethash path *pathname->document-id-table* nil))

(defmethod pathname->document-id ((path pathname))
  (gethash (namestring path) *pathname->document-id-table* nil))

(defmethod document-id->pathname ((docid integer))
  (gethash docid *document-id->pathname-table* nil))

;;; (init-delectus)
;;; (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (document-id->pathname 2)
