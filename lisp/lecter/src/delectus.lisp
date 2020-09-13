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
      (handler-case (progn (load-libDelectus)
                           (%init-delectus)
                           (setf *delectus-initialized* t))
        (error (err)
          (delectus-error "Failed to load libDelectus"
                          :error err
                          :code $ERR_UNKNOWN_ERROR)))))

(defvar *pathname->document-id-table* (make-hash-table :test 'equal))
(defvar *document-id->pathname-table* (make-hash-table :test 'eql))

(defmethod read-delectus-v1-file ((path string))
  (let ((docid (with-foreign-string (s path)
                 (%read-delectus-file s))))
    (if (equal docid $OBJ_NO_OID)
        (delectus-error (format nil "Unable to read file: ~S" path)
                        $ERR_CANT_READ)
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
;;; (read-delectus-v1-file "~/.emacs")
;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (document-id->pathname $id)

(defun delectus-version ()
  (let* ((version-data (%delectus-version))
         (version-string (first version-data))
         (version-ptr (second version-data)))
    (%release-string version-ptr)
    version-string))

;;; (delectus-version)

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
;;; (macroexpand '(%column-at-index $id 0))

(defmethod document-id->columns ((docid integer))
  (ensure-valid-docid docid)
  (let ((col-count (%count-columns docid)))
    (loop for i from 0 below col-count
       collect (%column-at-index docid i))))

;;; (document-id->columns $id)

(defmethod document-column-index ((docid integer)(column-label string))
  (ensure-valid-docid docid)
  (position column-label (document-id->columns docid) :test #'equal))

;;; (document-column-index $id "NOPE!")

(defmethod document-id->row-count ((docid integer))
  (ensure-valid-docid docid)
  (%count-rows docid))

;;; (document-id->row-count $id)

(defmethod document-value-at ((docid integer)(column-label string)(row-index integer))
  (ensure-valid-docid docid)
  (with-foreign-string (col column-label)
    (%value-at docid col row-index)))

;;; (document-value-at $id "Title" 4)
