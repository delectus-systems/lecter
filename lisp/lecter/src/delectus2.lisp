;;;; ***********************************************************************
;;;;
;;;; Name:          delectus2.lisp
;;;; Project:       lecter: Delectus as a library
;;;; Purpose:       interacting with delectus 2 documents
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

;;; =====================================================================
;;; naming conventions:
;;; =====================================================================
;;;
;;; - db-foo:
;;;   A function whose name starts with "db-" operates on a SQLite
;;;   database handle. Thatmeans it must be called within a
;;;   WITH-OPEN-DATABASE form, and within WITH-TRANSACTION if
;;;   transaction protection is needed.
;;;
;;; - foo:
;;;   A function whose name does not start with "db-"  does not
;;;   operate on a database handle, and so needs no special
;;;   protection from enclosing database forms.


;;; =====================================================================
;;; list file format
;;; =====================================================================

(defmethod db-valid-delectus-file? ((db sqlite-handle))
  (bind ((sql vals (sql-delectus-table-exists))
         (found (apply 'execute-single db sql vals)))
    found))

(defmethod valid-delectus-file? ((db-path pathname))
  (assert (probe-file db-path)() "No such file: ~S" db-path)
  (assert (file-pathname-p db-path)() "Not a file: ~S" db-path)
  (with-open-database (db db-path)
    (handler-case (db-valid-delectus-file? db)
      (sqlite-error (err) nil))))

(defmethod valid-delectus-file? ((db-path string))
  (valid-delectus-file? (pathname db-path)))

;;; (defparameter $movies-path (path "~/Desktop/Movies.delectus2"))
;;; (valid-delectus-file? $movies-path)
;;; (valid-delectus-file? (path "~/.emacs"))

;;; =====================================================================
;;; revisions
;;; =====================================================================

;;; return the next revision number to use for the specified op target
;;; method for "listnames", "comments", "columns"
(defmethod db-get-next-revision ((db sqlite-handle) (target string))
  (bind ((sql vals (sql-get-next-revision target))
         (rev (apply 'execute-single db sql vals)))
    (or rev 0)))

;;; method for itemids
(defmethod db-get-next-revision ((db sqlite-handle) (target vector))
  (bind ((sql vals (sql-get-next-revision target))
         (rev (apply 'execute-single db sql vals)))
    (or rev 0)))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path) (db-get-next-revision db "listnames"))


;;; =====================================================================
;;; item orders
;;; =====================================================================

(defmethod db-get-next-item-order ((db sqlite-handle))
  (bind ((sql vals (sql-get-next-item-order))
         (order (apply 'execute-single db sql vals)))
    (or order *minimum-item-order*)))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path) (db-get-next-item-order db))


;;; =====================================================================
;;; columns data
;;; =====================================================================

;;; make sure the columns defined in column-descriptions actually
;;; exist in the columns and items tables
(defmethod db-ensure-columns-exist ((db sqlite-handle) column-descriptions)
  (let* ((supplied-column-labels (mapcar #'column-description-label column-descriptions))
         (columns-column-labels (mapcar 'column-info-name
                                        (db-sqlite-table-column-info db *columns-table-name*)))
         (items-column-labels (mapcar 'column-info-name
                                      (db-sqlite-table-column-info db *items-table-name*)))
         (missing-columns-column-labels (remove-list-elements columns-column-labels supplied-column-labels))
         (missing-items-column-labels (remove-list-elements items-column-labels supplied-column-labels)))
    (when missing-columns-column-labels
      (loop for label in missing-columns-column-labels
         do (bind ((sql vals (sql-add-columns-userdata-column label)))
              (apply 'execute-non-query db sql vals))))
    (when missing-items-column-labels
      (loop for label in missing-items-column-labels
         do (bind ((sql vals (sql-add-items-userdata-column label)))
              (apply 'execute-non-query db sql vals))))))


;;; =====================================================================
;;; created and modified times
;;; =====================================================================

(defmethod db-get-created-time ((db sqlite-handle))
  (bind ((sql vals (sql-get-created-time)))
    (apply 'execute-single db sql vals)))

(defmethod get-created-time ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-created-time db)))

;;; (defparameter $movies-path (path "~/Desktop/Movies.delectus2"))
;;; (delectus-timestamp->local-time (get-created-time $movies-path))

(defmethod db-get-modified-time ((db sqlite-handle))
  (bind ((sql vals (sql-get-modified-time)))
    (apply 'execute-single db sql vals)))

(defmethod get-modified-time ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-modified-time db)))

;;; (defparameter $movies-path (path "~/Desktop/Movies.delectus2"))
;;; (delectus-timestamp->local-time (get-modified-time $movies-path))

(defmethod db-set-modified-time ((db sqlite-handle)(timestamp integer))
  (bind ((sql vals (sql-set-modified-time timestamp)))
    (apply 'execute-single db sql vals)))

(defmethod set-modified-time ((db-path pathname)(timestamp integer))
  (with-open-database (db db-path)
    (db-set-modified-time db timestamp)))

;;; (defparameter $movies-path (path "~/Desktop/Movies.delectus2"))
;;; (delectus-timestamp->local-time (get-modified-time $movies-path))
;;; (set-modified-time $movies-path (delectus-timestamp-now))

;;; =====================================================================
;;; creating the list file
;;; =====================================================================

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (assert (stringp listname) () "Expected a string :LISTNAME parameter, but found ~S" listname)
  (let* ((listid (or listid (make-identity-string))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db listid format)
        (db-create-listnames-table db)
        (db-create-comments-table db)
        (db-create-columns-table db)
        (db-create-items-table db)

        (when create-default-userdata
          (let* ((origin (make-origin (process-identity) db-path))
                 ;; used twice: in the columns op and in the item op
                 (default-column (make-default-column-description :name "Item"))
                 (default-column-label (column-description-label default-column))
                 (default-column-descriptions (list default-column))
                 ;; make a plist of [label value ...]
                 (field-values-map (loop for desc in default-column-descriptions
                                      appending [(column-description-label desc) ""])))

            ;; make sure the columns defined in column-descriptions actually
            ;; exist in the columns and items tables
            (db-ensure-columns-exist db default-column-descriptions)

            ;; insert listname op
            (let ((listname-revision (db-get-next-revision db "listnames")))
              (db-insert-listname-op db :origin origin :revision listname-revision
                                     :timestamp (delectus-timestamp-now) :listname listname))

            ;; insert default comment op
            (let* ((comment-revision (db-get-next-revision db "comments"))
                   (comment-text "A Delectus List"))
              (db-insert-comment-op db :origin origin :revision comment-revision
                                    :timestamp (delectus-timestamp-now) :comment comment-text))

            ;; insert default columns op
            (let ((columns-revision (db-get-next-revision db "columns")))
              (db-insert-columns-op db :origin origin :revision columns-revision
                                    :timestamp (delectus-timestamp-now)
                                    :columns default-column-descriptions))

            ;; insert default item op
            (let* ((item-target (makeid))
                   (item-order (db-get-next-item-order db))
                   (item-revision (db-get-next-revision db item-target)))
              (db-insert-item-op db :origin origin :revision item-revision :itemid item-target
                                 :timestamp (delectus-timestamp-now)
                                 :field-values field-values-map)))))))
  db-path)

(defmethod create-delectus-file ((db-path string)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (create-delectus-file (pathname db-path)
                        :listname listname
                        :listid listid
                        :format format
                        :create-default-userdata create-default-userdata)
  db-path)

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (create-delectus-file $testfile-path :listname "Test List")
;;; (delete-file $testfile-path)
