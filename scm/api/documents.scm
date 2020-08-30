;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          documents.scm
;;;; Project:       Delectus
;;;; Purpose:       document API for Delectus data
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; the documents API is the primary means of communication between
;;; UI components and the engine

;;; ----------------------------------------------------------------------
;;; utils
;;; ----------------------------------------------------------------------

(define next-document-id
  (let ((next-doc-id 0))
    (lambda ()
      (set! next-doc-id (+ 1 next-doc-id))
      next-doc-id)))

;;; ----------------------------------------------------------------------
;;; column info cache
;;; ----------------------------------------------------------------------
;;; keep track of which columns are numeric

(define $column-info-cache
  (make-table test: eq?))

(define (column-info . plist)
  plist)

(define (%fetch-column-info col)
  (table-ref $column-info-cache col #f))

(define (%put-column-info! col info)
  (table-set! $column-info-cache col info))

(define (get-column-info col key #!optional (default #f))
  (let ((info (%fetch-column-info col)))
    (if info
        (let ((entry (assoc key info)))
          (if entry (cdr entry) default))
        default)))

(define (set-column-info! col key val)
  (let ((info (%fetch-column-info col)))
    (if info
        (let ((entry (assoc key info)))
          (if entry
              (set-cdr! entry val)
              (%put-column-info! col (cons (cons key val) info))))
        (%put-column-info! col (cons (cons key val) '())))))

;;; ----------------------------------------------------------------------
;;; document data structure
;;; ----------------------------------------------------------------------

(define-type document 
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: %make-document
  (table doc:table)
  (view doc:view doc:set-view!)
  (view-valid doc:view-valid? doc:set-view-valid!)
  (view-description doc:view-description doc:set-view-description!))

(define (doc:make #!key
                  (table (table:make))
                  (view #f)
                  (view-valid #f)
                  (view-description (view:default-description)))
  (%make-document table view view-valid view-description))

(define (update-view! doc)
  (if (doc:view-valid? doc)
      doc
      (let ((tbl (doc:table doc)))
        (doc:set-view! doc
                       (view:create (doc:table doc)
                                    description: (or (doc:view-description doc)
                                                     (view:default-description))))
        (doc:set-view-valid! doc #t)
        (let* ((v (doc:view doc))
               (col-labels (table:column-labels v)))
          (for-each (lambda (lbl)
                      (if (table:numeric-column? v lbl)
                          (begin
                            (set-column-info! (table:column-at v lbl) numeric: #t)
                            (set-column-info! (table:column-at v lbl)
                                              total: 
                                              (reduce + 0.0 (table:column-values-as-numbers v lbl))))
                          (begin
                            (set-column-info! (table:column-at v lbl) numeric: #f)
                            (set-column-info! (table:column-at v lbl)
                                              total: #f))))
                    col-labels))
        doc)))

;;; ----------------------------------------------------------------------
;;; the global table of active documents
;;; ----------------------------------------------------------------------

(define $documents (make-table test: =))

(define (find-document docid)
  (table-ref $documents docid #f))

(define (doc:register! docid doc)
  (table-set! $documents docid doc)
  docid)

;;; ----------------------------------------------------------------------
;;; API
;;; ----------------------------------------------------------------------

(define (new-document)
  (doc:register! (next-document-id)(doc:make)))

(define (get-view docid #!key (description (view:default-description)))
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (doc:set-view-description! doc description)
          (doc:set-view-valid! doc #f)
          docid)
        (error "No such document"))))

(define (count-columns docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((v (doc:view doc)))
            (table:count-columns v)))
        (error "No such document"))))

(define (count-deleted-columns docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (let ((tbl (doc:table doc)))
            (table:count-deleted-columns tbl)))
        (error "No such document"))))

(define (column-at-index docid index)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((v (doc:view doc)))
            (table:column-at-index v index)))
        (error "No such document"))))

(define (sort-column docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((view-desc (doc:view-description doc)))
            (view-description:sort-column view-desc)))
        (error "No such document"))))

(define (sort-order docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((view-desc (doc:view-description doc)))
            (or (view-description:sort-order view-desc)
                $SORT_NONE)))
        (error "No such document"))))

(define (include-deleted? docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((view-desc (doc:view-description doc)))
            (view-description:include-deleted? view-desc)))
        (error "No such document"))))

(define (has-deleted? docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:table doc)))
            (table:has-deleted? tbl)))
        (error "No such document"))))

(define (filter-text docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((view-desc (doc:view-description doc)))
            (view-description:filter-text view-desc)))
        (error "No such document"))))

(define (count-rows docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (table:count-rows tbl)))
        (error "No such document"))))

(define (count-deleted-rows docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (let ((tbl (doc:table doc)))
            (table:count-deleted-rows tbl)))
        (error "No such document"))))

(define (value-at docid column-label row-index)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (col-index (table:column-index v column-label))
                 (val (if col-index 
                          (row:element (table:row-at v row-index) col-index)
                          #f)))
            (if val
                (if (string? val)
                    val
                    $VAL_NO_VALUE)
                $VAL_NO_VALUE)))
        (error "No such document"))))

(define (put-value-at! docid column-label row-index val)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (col-index (table:column-index v column-label)))
            (if col-index
                (row:set-element! (table:row-at v row-index) col-index val)
                (error "No such column"))))
        (error "No such document"))))

(define (row-finished? docid row-index)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((tbl (doc:view doc)))
            (row:finished? (table:row-at tbl row-index))))
        (error "No such document"))))

(define (mark-row-finished! docid row-index finished?)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:view doc)))
            (row:set-finished! (table:row-at tbl row-index) 
                               (if finished? #t #f))
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))

(define (add-row! docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:table doc)))
            (table:add-row! tbl)
            (doc:set-view-valid! doc #f)
            $ERR_NO_ERROR))
        (error "No such document"))))

(define (add-column! docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let ((tbl (doc:table doc)))
            (table:add-column! tbl column-label)
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))

(define (column-deleted? docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (col (table:column-at v column-label)))
            (if col
                (column:deleted? col)
                (error "No such column"))))
        (error "No such document"))))

(define (duplicate-label? docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (let* ((tbl (doc:table doc))
                 (col (table:column-at tbl column-label)))
            (if col
                $VAL_YES
                $VAL_NO)))
        (error "No such document"))))

(define (mark-column-deleted! docid column-label deleted?)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (col (table:column-at v column-label)))
            (if col
                (begin
                  (column:set-deleted! col (if deleted? #t #f))
                  (doc:set-view-valid! doc #f))
                (error "No such column"))))
        (error "No such document"))))

(define (column-has-total? docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (col (table:column-at v column-label)))
            (if col
                (get-column-info col numeric: #f)
                (error "No such column"))))
        (error "No such document"))))

(define (column-total docid column-label)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (col (table:column-at v column-label)))
            (if col
                (get-column-info col total: #f)
                (error "No such column"))))
        (error "No such document"))))

(define (row-deleted? docid row-index)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (row (table:row-at v row-index)))
            (if row
                (row:deleted? row)
                (error "No such row"))))
        (error "No such document"))))

(define (mark-row-deleted! docid row-index deleted?)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (update-view! doc)
          (let* ((v (doc:view doc))
                 (row (table:row-at v row-index)))
            (if row
                (begin
                  (row:set-deleted! row (if deleted? #t #f))
                  (doc:set-view-valid! doc #f))
                (error "No such row"))))
        (error "No such document"))))

(define (compact! docid)
  (let ((doc (find-document docid)))
    (if doc
        (begin
          (let* ((tbl (doc:table doc)))
            (table:compact! tbl)
            (doc:set-view-valid! doc #f)))
        (error "No such document"))))
