;;;; ***********************************************************************
;;;;
;;;; Name:          sql.lisp
;;;; Project:       lecter: Delectus as a library
;;;; Purpose:       utilities for generating SQL
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)


;;; =====================================================================
;;; creating tables
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; 'delectus' table
;;; ---------------------------------------------------------------------

(defun sql-create-delectus-table ()
  (yield
   (create-table :delectus
                 ((listid :type 'text)
                  (format :type 'text)
                  (created :type 'integer)
                  (modified :type 'integer)))))

;;; (sql-create-delectus-table)

(defun sql-init-delectus-table (listid
                                &key
                                  (format +delectus-format-version+)
                                  (created (format nil "~A" (now)))
                                  (modified (format nil "~A" (now))))
  (assert (identity-string? listid)() "Not a valid list identity: ~S" listid)
  (yield
   (insert-into :delectus
                (set= :listid listid
                      :format format
                      :created created
                      :modified modified))))

;;; (sql-init-delectus-table (make-identity-string))

(defun sql-delectus-table-exists ()
  (yield
   (select :name
           (from :sqlite_master)
           (where (:and (:= :type "table")
                        (:= :name "delectus"))))))

;;; (sql-delectus-table-exists)

;;; ---------------------------------------------------------------------
;;; 'listnames' table
;;; ---------------------------------------------------------------------

(defun sql-create-listnames-table ()
  (yield
   (create-table :listnames
                 ((origin :type 'blob)
                  (revision :type 'integer)
                  (timestamp :type 'integer)
                  (name :type 'text)))))

;;; (sql-create-listnames-table)

;;; ---------------------------------------------------------------------
;;; 'comments' table
;;; ---------------------------------------------------------------------

(defun sql-create-comments-table ()
  (yield
   (create-table :comments
                 ((origin :type 'blob)
                  (revision :type 'integer)
                  (timestamp :type 'integer)
                  (comment :type 'text)))))

;;; (sql-create-comments-table)


;;; ---------------------------------------------------------------------
;;; 'columns' table
;;; ---------------------------------------------------------------------

(defun sql-create-columns-table ()
  (yield
   (create-table :columns
                 ((origin :type 'blob)
                  (revision :type 'integer)
                  (timestamp :type 'integer)))))

;;; (sql-create-columns-table)


;;; ---------------------------------------------------------------------
;;; 'items' table
;;; ---------------------------------------------------------------------

(defun sql-create-items-table ()
  (yield
   (create-table :items
                 ((origin :type 'blob)
                  (revision :type 'integer)
                  (itemid :type 'blob)
                  (item_order :type 'real)
                  (timestamp :type 'integer)
                  (deleted :type 'integer)))))

;;; (sql-create-items-table)

;;; ---------------------------------------------------------------------
;;; the main items index
;;; ---------------------------------------------------------------------

;; (defun sql-create-items-itemid-timestamp-index ()
;;   (values "CREATE INDEX idx_items_itemid_timestamp on `items` (`itemid`, `timestamp` DESC)"
;;           nil))

(defun sql-create-items-itemid-revision-timestamp-index ()
  (values "CREATE INDEX idx_main_items on `items` (`itemid`, `revision` DESC, `timestamp` DESC, item_order)"
          nil))

;;; =====================================================================
;;; getting and setting times
;;; =====================================================================

(defun sql-get-created-time ()
  (yield
   (select :created
           (from :delectus))))

(defun sql-get-modified-time ()
  (yield
   (select :modified
           (from :delectus))))

(defun sql-set-modified-time (timestamp)
  (yield
   (update :delectus
           (set= :modified timestamp))))

;;; =====================================================================
;;; getting next revision and order
;;; =====================================================================

(defun sql-get-next-revision (target)
  (cond
    ((member target '("listnames" "comments" "columns") :test #'equal)
     (values (format nil "SELECT MAX(revision)+1 FROM `~A`" target)
             nil))
    ((identity? target)
     (yield
      (select ((:+ (:max :revision) 1))
              (from :items)
              (where (:= :itemid target)))))
    (t (error "Unrecognized target: ~S" target))))

;;; (sql-get-next-revision "listnames")
;;; (sql-get-next-revision "columns")
;;; (setf $id (makeid))
;;; (sql-get-next-revision $id)

(defun sql-get-next-item-order ()
  (yield
   (select ((:+ (:max :item_order) *item-order-interval*))
           (from :items))))

;;; (sql-get-next-item-order)

;;; =====================================================================
;;; adding userdata columns
;;; =====================================================================

(defun sql-add-columns-userdata-column (column-label)
  (yield
   (alter-table :columns
                (add-column (as-keyword column-label) :type 'text))))

(defun sql-add-items-userdata-column (column-label)
  (yield
   (alter-table :items
                (add-column (as-keyword column-label) :type 'text))))

;;; =====================================================================
;;; inserting ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; 'listname' op
;;; ---------------------------------------------------------------------

(defun sql-insert-listname-op (origin revision timestamp name-json)
  (yield
   (insert-into :listnames
                (set= :origin origin
                      :revision revision
                      :timestamp timestamp
                      :name name-json))))

;;; ---------------------------------------------------------------------
;;; 'comment' op
;;; ---------------------------------------------------------------------

(defun sql-insert-comment-op (origin revision timestamp comment-json)
  (yield
   (insert-into :comments
                (set= :origin origin
                      :revision revision
                      :timestamp timestamp
                      :comment comment-json))))

;;; ;; (sql-insert-comment-op (makeid) 1 (now) "Foo")

;; ;;; ---------------------------------------------------------------------
;; ;;; 'columns' op
;; ;;; ---------------------------------------------------------------------

;; (defun sql-insert-columns-op (origin revision timestamp columns-data)
;;   (let ((userdata-column-labels (get-plist-keys columns-data))
;;         (userdata-column-data (get-plist-values columns-data)))
;;     (values (format nil "INSERT INTO `columns` (origin, revision, timestamp, ~{~A~^, ~}) VALUES (?, ?, ?, ~{~A~^, ~})"
;;                     userdata-column-labels
;;                     (mapcar (constantly "?") userdata-column-labels))
;;             (append [origin revision timestamp] userdata-column-data))))

;; ;;; (setf $cols [(make-default-column-description :name "Item")])
;; ;;; (sql-insert-columns-op (makeid) 1 (delectus-timestamp-now) (ensure-columns-data $cols))

;;; ---------------------------------------------------------------------
;;; 'item' op
;;; ---------------------------------------------------------------------

(defun sql-insert-item-op (origin revision itemid item-order timestamp field-values-map)
  ;; field-values-map is a plist of [column-label value ...]
  (let ((userdata-column-labels (get-plist-keys field-values-map))
        (userdata-field-values (get-plist-values field-values-map)))
    (values (format nil "INSERT INTO `items` (origin, revision, itemid, item_order, timestamp, ~{~A~^, ~}) VALUES (?, ?, ?, ?, ?, ~{~A~^, ~})"
                    userdata-column-labels
                    (mapcar (constantly "?") userdata-column-labels))
            (append [origin revision itemid item-order timestamp] userdata-field-values))))

;;; (sql-insert-item-op (makeid) 1 (makeid) (now) `(,(make-column-label) "Foo"))


;;; =====================================================================
;;; getting the latest data
;;; =====================================================================

(defun sql-get-latest-listname-op ()
  (values "SELECT * FROM listnames ORDER BY revision DESC, timestamp DESC LIMIT 1"
          nil))

(defun sql-get-latest-comment-op ()
  (values "SELECT * FROM comments ORDER BY revision DESC, timestamp DESC LIMIT 1"
          nil))

(defun sql-get-latest-columns-op ()
  (values "SELECT * FROM columns ORDER BY revision DESC, timestamp DESC LIMIT 1"
          nil))

(defun sql-check-latest-items-table-exists ()
  (values "SELECT * FROM sqlite_temp_master WHERE type='table' AND name='latest_items'"
          nil))

(defun sql-create-latest-items-table ()
  (values
   (trim "
create temporary table latest_items as
select * 
from (select *
      from items
      order by revision DESC, timestamp DESC) 
group by itemid 
order by item_order
")
   nil))


(defun sql-count-latest-items ()
  (yield
   (select ((:count :*))
           (from :|`latest_items`|))))

;;; (sql-count-latest-items)


(defun sql-get-latest-items (&key
                               (order :asc)
                               (offset 0)
                               (limit *default-result-items-per-page*))
  (yield
   (select :*
           (from :latest_items)
           (order-by (case order
                       (:asc '(:asc :item_order))
                       (:desc '(:desc :item_order))
                       (else (error "Unrecognized order ~S" order))))
           (offset offset)
           (limit limit))))

(defun sql-get-latest-filtered-items (&key filter-text column-labels offset limit)
  (let* ((filters (remove nil
                          (mapcar (lambda (lbl)
                                    (if (empty? filter-text)
                                        nil
                                        (format nil "`~A` LIKE '%~A%'" lbl filter-text)))
                                  column-labels)))
         (filter-expression (if filters
                                (str "WHERE (" (join-strings " OR " filters) ")")
                                ""))
         (offset-expression (if (and limit offset) (format nil "OFFSET ~A" offset) ""))
         (limit-expression (if limit (format nil "LIMIT ~A" limit) "")))
    (values  (format nil "SELECT * FROM `latest_items` ~A ~A ~A"
                     filter-expression limit-expression offset-expression)
             nil)))

(defun sql-count-latest-filtered-items (&key filter-text column-labels offset limit)
  (let* ((filters (remove nil
                          (mapcar (lambda (lbl)
                                    (if (empty? filter-text)
                                        nil
                                        (format nil "`~A` LIKE '%~A%'" lbl filter-text)))
                                  column-labels)))
         (filter-expression (if filters
                                (str "WHERE (" (join-strings " OR " filters) ")")
                                ""))
         (offset-expression (if (and limit offset) (format nil "OFFSET ~A" offset) ""))
         (limit-expression (if limit (format nil "LIMIT ~A" limit) "")))
    (values  (format nil "SELECT COUNT(*) FROM `latest_items` ~A ~A ~A"
                     filter-expression limit-expression offset-expression)
             nil)))
