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

(defvar *pathname->document-id-table* (make-hash-table :test 'equal))
(defvar *document-id->pathname-table* (make-hash-table :test 'eql))

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

(defmethod read-delectus-v1-file ((path pathname))
  (read-delectus-v1-file (namestring path)))

(defmethod pathname->document-id ((path string))
  (gethash path *pathname->document-id-table* nil))

(defmethod pathname->document-id ((path pathname))
  (gethash (namestring path) *pathname->document-id-table* nil))

(defmethod document-id->pathname ((docid integer))
  (gethash docid *document-id->pathname-table* nil))

;;; (init-delectus)
;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (document-id->pathname $id)

(defmethod ensure-valid-docid ((id integer))
  (multiple-value-bind (path found?)(document-id->pathname id)
    (assert found? () "The docid ~S was not found" id)
    (assert (uiop:file-pathname-p path)() "The value ~S is not a valid file pathname" path)
    (assert (probe-file path)() "The pathname ~S does not name an existing file" path)
    path))

(defmethod document-id->column-count ((docid integer))
  (ensure-valid-docid docid)
  (%count-columns docid))

;;; (document-id->column-count $id)

(defmethod document-id->columns ((docid integer))
  (assert (stringp (document-id->pathname docid))()
          "No document id ~S" docid)
  (let ((col-count (%count-columns docid)))
    (loop for i from 0 below col-count
       collect (%column-at-index docid i))))

;;; (document-id->columns $id)

