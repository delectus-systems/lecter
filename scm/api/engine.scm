;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          engine.scm
;;;; Project:       Delectus
;;;; Purpose:       the scheme client API for the Delectus engine
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (eng:new-delectus)
  (reg:register-delectus! (table:make)))

(define (eng:release-delectus id)
  (reg:unregister-delectus! id))

(define (eng:update-view! id 
                          #!key
                          (include-deleted #f)
                          (sort-column #f)
                          (sort-order #f)
                          (filter-text #f))
  (let ((vw (view:create id 
                         include-deleted: include-deleted
                         sort-column: sort-column
                         sort-order: sort-order
                         filter-text: filter-text)))
    (reg:register-view! id vw)
    id))

(define (eng:count-rows id)
  (let ((vw (reg:find-view id)))
    (if vw
        (view:count-rows vw)
        (error "No such view" id))))

(define (eng:count-deleted-rows id)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((result 0))
                (vector-for-each (lambda (row)(if (row:deleted? row)(set! result (+ result 1))))
                                 (table:rows tbl))
                result)
              (error "No such view" id)))
        (error "No such view" id))))

(define (eng:count-columns id)
  (let ((vw (reg:find-view id)))
    (if vw
        (view:count-columns vw)
        (error "No such view" id))))

(define (eng:count-deleted-columns id)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((result 0))
                (vector-for-each (lambda (col)(if (column:deleted? col)(set! result (+ result 1))))
                                 (column-sequence:columns (table:column-sequence tbl)))
                result)
              (error "No such view" id)))
        (error "No such view" id))))

(define (eng:column-at-index id index)
  (let ((vw (reg:find-view id)))
    (if vw
        (list-ref (view:column-labels vw) index)
        (error "No such view" id))))

(define (eng:sort-column id)
  (let ((vw (reg:find-view id)))
    (if vw
        (view:sort-column vw)
        (error "No such view" id))))

(define (eng:sort-order id)
  (let ((vw (reg:find-view id)))
    (if vw
        (view:sort-order vw)
        (error "No such view" id))))

(define (eng:include-deleted? id)
  (let ((vw (reg:find-view id)))
    (if vw
        (view:include-deleted? vw)
        (error "No such view" id))))

(define (eng:has-deleted? id)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (or (vector-some? column:deleted? (column-sequence:columns (table:column-sequence tbl)))
                  (vector-some? row:deleted? (table:rows tbl)))
              (error "No such view" id)))
        (error "No such view" id))))

(define (eng:filter-text id)
  (let ((vw (reg:find-view id)))
    (if vw
        (view:filter-text vw)
        (error "No such view" id))))

(define (eng:value-at id column-label row-index)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((col-index (table:column-index tbl column-label)))
                (if col-index
                    (let ((rindex (vector-ref (view:row-indexes vw) row-index)))
                      (row:element (vector-ref (table:rows tbl) rindex) col-index))
                    (error "No such column" column-label)))
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:put-value-at! id column-label row-index val)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((col-index (table:column-index tbl column-label)))
                (if col-index
                    (let ((rindex (vector-ref (view:row-indexes vw) row-index)))
                      (row:set-element! (vector-ref (table:rows tbl) rindex) col-index val)
                      $ERR_NO_ERROR)
                    (error "No such column" column-label)))
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:add-row! id)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((include-deleted (view:include-deleted? vw))
                    (sort-column (view:sort-column vw))
                    (sort-order (view:sort-order vw))
                    (filter-text (view:filter-text vw)))
                (table:add-row! tbl)
                (eng:update-view! id 
                                  include-deleted: include-deleted
                                  sort-column: sort-column
                                  sort-order: sort-order
                                  filter-text: filter-text)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:add-column! id column-label)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((include-deleted (view:include-deleted? vw))
                    (sort-column (view:sort-column vw))
                    (sort-order (view:sort-order vw))
                    (filter-text (view:filter-text vw)))
                (table:add-column! tbl column-label)
                (eng:update-view! id 
                                  include-deleted: include-deleted
                                  sort-column: sort-column
                                  sort-order: sort-order
                                  filter-text: filter-text)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:rename-column! id old-label new-label)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((include-deleted (view:include-deleted? vw))
                    (sort-column (view:sort-column vw))
                    (sort-order (view:sort-order vw))
                    (filter-text (view:filter-text vw)))
                (table:rename-column! tbl old-label new-label)
                (eng:update-view! id 
                                  include-deleted: include-deleted
                                  sort-column: #f
                                  sort-order: #f
                                  filter-text: filter-text)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:column-deleted? id column-label)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((col (table:column-at tbl column-label)))
                (if col
                    (column:deleted? col)
                    (error "No such column" column-label)))
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:mark-column-deleted! id column-label deleted?)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((col (table:column-at tbl column-label)))
                (if col
                    (let ((include-deleted (view:include-deleted? vw))
                          (sort-column (view:sort-column vw))
                          (sort-order (view:sort-order vw))
                          (filter-text (view:filter-text vw)))
                      (column:set-deleted! col deleted?)
                      (eng:update-view! id 
                                        include-deleted: include-deleted
                                        sort-column: sort-column
                                        sort-order: sort-order
                                        filter-text: filter-text)
                      $ERR_NO_ERROR)
                    (error "No such column" column-label)))
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:duplicate-label? id column-label)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let ((already-col (table:column-at tbl column-label)))
                (if already-col #t #f))
              (error "No such view" id)))
        (error "No such view" id))))

(define (eng:row-deleted? id row-index)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let* ((rindex (vector-ref (view:row-indexes vw) row-index))
                     (row (table:row-at tbl rindex)))
                (if row
                    (row:deleted? row)
                    (error "No such row" row-index)))
              (error "No such view" id)))
        (error "No such view" id))))

(define (eng:mark-row-deleted! id row-index deleted?)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let* ((include-deleted (view:include-deleted? vw))
                     (sort-column (view:sort-column vw))
                     (sort-order (view:sort-order vw))
                     (filter-text (view:filter-text vw))
                     (rindex (vector-ref (view:row-indexes vw) row-index))
                     (row (table:row-at tbl rindex)))
                (row:set-deleted! row deleted?)
                (eng:update-view! id 
                                  include-deleted: include-deleted
                                  sort-column: sort-column
                                  sort-order: sort-order
                                  filter-text: filter-text)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:compact! id)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (let* ((include-deleted (view:include-deleted? vw))
                     (sort-column (view:sort-column vw))
                     (sort-order (view:sort-order vw))
                     (filter-text (view:filter-text vw)))
                (table:compact! tbl)
                (eng:update-view! id 
                                  include-deleted: include-deleted
                                  sort-column: sort-column
                                  sort-order: sort-order
                                  filter-text: filter-text)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:write-delectus-file id path)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (begin
                (write-delectus-file tbl path)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:read-delectus-file path)
  (read-delectus-file path))

(define (eng:write-csv-file id path)
  (let ((vw (reg:find-view id)))
    (if vw
        (let ((tbl (reg:find-table id)))
          (if tbl
              (begin
                (write-csv-file tbl path)
                $ERR_NO_ERROR)
              (error "No such table" id)))
        (error "No such view" id))))

(define (eng:read-csv-file path)
  (read-csv-file path))

