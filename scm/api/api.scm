;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.scm
;;;; Project:       Delectus
;;;; Purpose:       the scheme API functions called by the C API
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; error handler
;;; ---------------------------------------------------------------------

(define (%if-error-aux errval thunk)
  (let ((handler (lambda (err) errval)))
    (with-exception-catcher handler thunk)))

(define-macro (if-error errval . body)
  `(%if-error-aux ,errval 
                  (lambda () ,@body)))

(define (typename->test tnm)
  (case tnm
    ((int) integer?)
    ((float) flonum?)
    ((bool) boolean?)
    ((string) (lambda (x) (or (not x)(string? x))))
    (else (error "Invalid API return type" tnm))))

(define-macro (require-type typename form)
  (let ((type-test (gensym))
        (val-var (gensym)))
    `(let ((,type-test (typename->test ,typename))
           (,val-var ,form))
       (if (,type-test ,val-var)
           ,val-var
           (error "Wrong return type" ,form ,typename)))))

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (api:version)
  (require-type 'string
                (if-error $VAL_NO_VALUE "1.0")))

(define (api:new-delectus)
  (require-type 'int
                (if-error $OBJ_NO_OID (eng:new-delectus))))

(define (api:release-delectus id)
  (require-type 'int
                (if-error $ERR_CANT_RELEASE (eng:release-delectus id))))


(define (api:update-view! id include-deleted? sort-column sort-order filter-text)
  (require-type 'int
                (if-error $OBJ_NO_OID
                          (let ((filter-text (if filter-text
                                                 (if (string? filter-text)
                                                     (if (> (string-length filter-text) 0)
                                                         filter-text
                                                         #f)
                                                     #f)
                                                 #f)))
                            (eng:update-view! id
                                              include-deleted: include-deleted?
                                              sort-column: sort-column
                                              sort-order: sort-order
                                              filter-text: filter-text)))))

(define (api:count-rows id)
  (require-type 'int
                (if-error 0 (eng:count-rows id))))

(define (api:count-deleted-rows id)
  (require-type 'int
                (if-error 0 (eng:count-deleted-rows id))))

(define (api:count-columns id)
  (require-type 'int
                (if-error 0 (eng:count-columns id))))

(define (api:count-deleted-columns id)
  (require-type 'int
                (if-error 0 (eng:count-deleted-columns id))))

(define (api:column-at-index id index)
  (require-type 'string
                (if-error $VAL_NO_VALUE
                          (eng:column-at-index id index))))

(define (api:sort-column id)
  (require-type 'string
                (if-error $VAL_NO_VALUE
                          (eng:sort-column id))))

(define (api:sort-order id)
  (require-type 'int
                (if-error $SORT_NONE
                          (or (eng:sort-order id)
                              $SORT_NONE))))

(define (api:include-deleted? id)
  (require-type 'bool
                (if-error $VAL_NO
                          (eng:include-deleted? id))))

(define (api:has-deleted? id)
  (require-type 'bool
                (if-error $VAL_NO
                          (eng:has-deleted? id))))

(define (api:filter-text id)
  (require-type 'string
                (if-error $VAL_NO_VALUE
                          (eng:filter-text id))))

(define (api:value-at id column-label row-index)
  (require-type 'string
                (if-error $VAL_NO_VALUE
                          (eng:value-at id column-label row-index))))

(define (api:put-value-at! id column-label row-index val)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:put-value-at! id column-label row-index val)
                            $ERR_NO_ERROR))))

(define (api:add-row! id)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:add-row! id)
                            $ERR_NO_ERROR))))

(define (api:add-column! id column-label)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:add-column! id column-label)
                            $ERR_NO_ERROR))))

(define (api:rename-column! id old-label new-label)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:rename-column! id old-label new-label)
                            $ERR_NO_ERROR))))

(define (api:column-deleted? id column-label)
  (require-type 'bool
                (if-error $VAL_NO
                          (eng:column-deleted? id column-label))))

(define (api:mark-column-deleted! id column-label deleted?)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:mark-column-deleted! id column-label deleted?)
                            $ERR_NO_ERROR))))

(define (api:duplicate-label? id column-label)
  (require-type 'bool
                (if-error $VAL_YES
                          (eng:duplicate-label? id column-label))))

(define (api:row-deleted? id row-index)
  (require-type 'bool
                (if-error $VAL_NO
                          (eng:row-deleted? id row-index))))

(define (api:mark-row-deleted! id row-index deleted?)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:mark-row-deleted! id row-index deleted?)
                            $ERR_NO_ERROR))))

(define (api:compact-delectus! id)
  (require-type 'int
                (if-error $ERR_CANT_UPDATE
                          (begin
                            (eng:compact! id)
                            $ERR_NO_ERROR))))

(define (api:write-delectus-file id path)
  (require-type 'int
                (if-error $ERR_CANT_WRITE
                          (begin
                            (eng:write-delectus-file id path)
                            $ERR_NO_ERROR))))

(define (api:read-delectus-file path)
  (require-type 'int
                (if-error $OBJ_NO_OID
                          (eng:read-delectus-file path))))

(define (api:write-delectus-csv id path)
  (require-type 'int
                (if-error $ERR_CANT_WRITE
                          (begin
                            (eng:write-csv-file id path)
                            $ERR_NO_ERROR))))

(define (api:read-delectus-csv path)
  (require-type 'int
                (if-error $OBJ_NO_OID
                          (eng:read-csv-file path))))

