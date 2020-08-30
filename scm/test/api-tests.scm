;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.scm
;;;; Project:       Delectus
;;;; Purpose:       tests for the api
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $test-input-file "/Users/mikel/Projects/delectus/delectus/test-data/junior-movies.delectus")

(define (test:new-delectus)
  (let ((id (api:new-delectus)))
    (test:expect 'test:new-delectus (integer? id))))

(define (test:update-view!)
  (let ((id (api:read-delectus-file $test-input-file)))
    (api:update-view! id #f "Title" $SORT_ASCENDING "z")
    (test:expect 'test:update-view! (string=? "AEONFlux" (api:value-at id "Title" 0)))))

(define (test:count-rows)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:count-rows (= 993 (api:count-rows id)))))

(define (test:count-deleted-rows)
  (let ((id (api:read-delectus-file $test-input-file)))
    (api:mark-row-deleted! id 0 #t)
    (test:expect 'test:count-deleted-rows (= 992 (api:count-rows id)))
    (test:expect 'test:count-deleted-rows (= 1 (api:count-deleted-rows id)))))

(define (test:count-columns)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:count-columns (= 8 (api:count-columns id)))))

(define (test:count-deleted-columns)
  (let ((id (api:read-delectus-file $test-input-file)))
    (api:mark-column-deleted! id "Title" #t)
    (test:expect 'test:count-deleted-columns (= 7 (api:count-columns id)))
    (test:expect 'test:count-deleted-columns (= 1 (api:count-deleted-columns id)))))

(define (test:column-at-index)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:column-at-index (string-ci=? "Title" (api:column-at-index id 0)))
    (api:mark-column-deleted! id "Title" #t)
    (test:expect 'test:test:column-at-index (not (string-ci=? "Title" (api:column-at-index id 0))))))

(define (test:sort-column)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:sort-column (not (api:sort-column id)))
    (api:update-view! id #f "Title" $SORT_ASCENDING #f)
    (test:expect 'test:sort-column (string=? "Title" (api:sort-column id)))))

(define (test:sort-order)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:sort-order (= $SORT_NONE (api:sort-order id)))
    (api:update-view! id #f "Title" $SORT_ASCENDING #f)
    (test:expect 'test:sort-order (= $SORT_ASCENDING (api:sort-order id)))))

(define (test:include-deleted?)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:include-deleted? (not (api:include-deleted? id)))
    (api:update-view! id #t "Title" $SORT_ASCENDING #f)
    (test:expect 'test:include-deleted? (api:include-deleted? id))))

(define (test:has-deleted?)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:has-deleted? (not (api:has-deleted? id)))
    (api:mark-row-deleted! id 0 #t)
    (test:expect 'test:has-deleted? (api:has-deleted? id))))

(define (test:filter-text)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:filter-text (not (api:filter-text id)))
    (api:update-view! id #f "Title" $SORT_ASCENDING "foo")
    (test:expect 'test:filter-text (string=? "foo" (api:filter-text id)))))

(define (test:value-at)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:value-at (string=? "101 Dalmations" (api:value-at id "Title" 0)))
    (api:put-value-at! id "Title" 0 "Frobnitz")
    (test:expect 'test:value-at (string=? "Frobnitz" (api:value-at id "Title" 0)))))

(define (test:add-row!)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:add-row! (= 993 (api:count-rows id)))
    (api:add-row! id)
    (test:expect 'test:add-row! (= 994 (api:count-rows id)))))

(define (test:add-column!)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:add-column! (= 8 (api:count-columns id)))
    (api:add-column! id "Frob")
    (test:expect 'test:add-column! (= 9 (api:count-columns id)))))

(define (test:column-deleted?)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:column-deleted? (not (api:column-deleted? id "Title")))
    (api:mark-column-deleted! id "Title" #t)
    (test:expect 'test:column-deleted? (api:column-deleted? id "Title"))))

(define (test:duplicate-label?)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:duplicate-label? (not (api:duplicate-label? id "Frob")))
    (api:add-column! id "Frob")
    (test:expect 'test:duplicate-label? (api:duplicate-label? id "Frob"))))

(define (test:row-deleted?)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:row-deleted? (not (api:row-deleted? id 0)))
    (api:mark-row-deleted! id 0 #t)
    (api:update-view! id #t #f #f #f)
    (test:expect 'test:row-deleted? (api:row-deleted? id 0))))

(define (test:compact-delectus!)
  (let ((id (api:read-delectus-file $test-input-file)))
    (test:expect 'test:compact-delectus! (string=? "101 Dalmations" (api:value-at id "Title" 0)))
    (test:expect 'test:compact-delectus! (= 993 (api:count-rows id)))
    (test:expect 'test:compact-delectus! (= 8 (api:count-columns id)))
    (api:mark-column-deleted! id "Star" #t)
    (api:mark-row-deleted! id 1 #t)
    (api:compact-delectus! id)
    (test:expect 'test:compact-delectus! (string=? "101 Dalmations" (api:value-at id "Title" 0)))
    (test:expect 'test:compact-delectus! (= 992 (api:count-rows id)))
    (test:expect 'test:compact-delectus! (= 7 (api:count-columns id)))))

;;; (test:new-delectus)
;;; (test:update-view!)
;;; (test:count-rows)
;;; (test:count-deleted-rows)
;;; (test:count-columns)
;;; (test:count-deleted-columns)
;;; (test:column-at-index)
;;; (test:sort-column)
;;; (test:sort-order)
;;; (test:include-deleted?)
;;; (test:has-deleted?)
;;; (test:filter-text)
;;; (test:value-at)
;;; (test:add-row!)
;;; (test:add-column!)
;;; (test:column-deleted?)
;;; (test:duplicate-label?)
;;; (test:row-deleted?)
;;; (test:compact-delectus!)

(define $d1 (api:new-delectus))
(api:add-column! $d1 "foo")
(api:add-row! $d1)
(api:put-value-at! $d1 "foo" 0 "foobie")
(api:value-at $d1 "foo" 0)
(api:add-column! $d1 "bar")
(api:put-value-at! $d1 "bar" 0 "barbie")
(api:value-at $d1 "bar" 0)
(api:add-column! $d1 "baz")
(api:value-at $d1 "baz" 0)
