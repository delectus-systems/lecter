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

;;; ---------------------------------------------------------------------
;;; init libDelectus
;;; ---------------------------------------------------------------------

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

(defun delectus-version ()
  (with-released-string+ptr
    (%delectus-version)))

;;; (init-delectus)
;;; (delectus-version)

;;; ---------------------------------------------------------------------
;;; read Delectus1 files
;;; ---------------------------------------------------------------------

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
;;; (setf $id (read-delectus-v1-file (asdf:system-relative-pathname :lecter "../../test-data/Movies.delectus")))
;;; (document-id->pathname $id)

(defmethod ensure-valid-docid ((id integer))
  (multiple-value-bind (path found?)(document-id->pathname id)
    (assert found? () "The docid ~S was not found" id)
    (assert (uiop:file-pathname-p path)() "The value ~S is not a valid file pathname" path)
    (assert (probe-file path)() "The pathname ~S does not name an existing file" path)
    path))


;;; ---------------------------------------------------------------------
;;;  get column data
;;; ---------------------------------------------------------------------

(defmethod document-id->column-count ((docid integer))
  (ensure-valid-docid docid)
  (%count-columns docid))

;;; (document-id->column-count $id)

(defmethod document-columns ((docid integer))
  (ensure-valid-docid docid)
  (let ((col-count (%count-columns docid)))
    (loop for i from 0 below col-count
       collect (with-released-string+ptr
                 (%column-at-index docid i)))))

;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (document-columns $id)

(defmethod document-column-index ((docid integer)(column-label string))
  (ensure-valid-docid docid)
  (position column-label (document-columns docid) :test #'equal))

;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (document-column-index $id "NOPE!")
;;; (document-column-index $id "Star")

;;; ---------------------------------------------------------------------
;;;  get row data
;;; ---------------------------------------------------------------------

(defmethod document-id->row-count ((docid integer))
  (ensure-valid-docid docid)
  (%count-rows docid))

;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (document-id->row-count $id)

(defmethod document-row-at ((docid integer)(row-index integer))
  (ensure-valid-docid docid)
  (let ((cols (document-columns docid)))
    (loop for lbl in cols
       collect (with-foreign-string (col lbl)
                 (with-released-string+ptr
                   (%value-at docid col row-index))))))

;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (time (document-row-at $id 900))

(defmethod document-rows ((docid integer) &key (start 0)(end nil))
  (ensure-valid-docid docid)
  (assert (>= start 0)() ":START must be an integer greater than or equal to zero")
  (let ((end (or end (document-id->row-count docid))))
    (assert (>= end 0)() ":END must be an integer greater than or equal to zero")
    (assert (>= end start)() ":END must be an integer greater than or equal to :START")
    (loop for i from start below end
       collect (document-row-at docid i))))

;;; (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus"))
;;; (time (document-rows $id))

;;; (init-delectus)
;;; (time (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/zipcode.delectus")))
;;; (document-id->pathname $id)
;;; (time (progn (setf $rows (document-rows $id)) 'done))
;;; (length $rows)
;;; (elt $rows (random (length $rows)))


;;; ---------------------------------------------------------------------
;;;  read and write fields
;;; ---------------------------------------------------------------------

(defmethod document-value-at ((docid integer)(column-label string)(row-index integer))
  (ensure-valid-docid docid)
  (with-foreign-string (col column-label)
    (with-released-string+ptr
      (%value-at docid col row-index))))

;;; (time (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/zipcode.delectus")))
;;; (document-value-at $id "Title" 40)

;;; ---------------------------------------------------------------------
;;; convert to csv
;;; ---------------------------------------------------------------------

(defmethod write-to-csv ((docid integer)(out-path string))
  (with-foreign-string (out out-path)
    (%write-delectus-csv docid out)))

(defmethod write-to-csv ((docid integer)(out-path pathname))
  (write-to-csv docid (namestring out-path)))

;;; (time (setf $id (read-delectus-v1-file "/Users/mikel/Workshop/src/delectus/test-data/zipcode.delectus")))
;;; (time (write-to-csv $id "/Users/mikel/Desktop/testzips.csv"))
