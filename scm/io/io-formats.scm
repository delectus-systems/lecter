;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          io-formats.scm
;;;; Project:       Delectus
;;;; Purpose:       conversion from older file formats
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (store:vector-format? x tag)
  (and (vector? x)
       (> (vector-length x) 1)
       (eqv? tag (vector-ref x 0))))

(define (serialized-store? s)(store:vector-format? s 'store))
(define (serialized-store-format s)(vector-ref s 1))

;;; ----------------------------------------------------------------------
;;; Delectus file formats
;;; ----------------------------------------------------------------------

;;; serialized data formats

(define (io:alpha-1-format? data)(store:vector-format? data '<store>))
(define (io:alpha-2-format? data)
  (and (store:vector-format? data 'store)
       (= 4 (vector-length data))))

(define (io:alpha-4-format? data)
  (and (serialized-store? data)
       (= (serialized-store-format data)
          $delectus-format-alpha-4)))

(define (io:beta-2-format? data)
  (and (serialized-store? data)
       (= (serialized-store-format data)
          $delectus-format-beta-2)))

(define (delectus-format data)
  (cond
   ((table? data) $delectus-format-1.0)
   ((io:alpha-1-format? data) $delectus-format-alpha-1)
   ((io:alpha-2-format? data) $delectus-format-alpha-2)
   ((io:alpha-4-format? data) $delectus-format-alpha-4)
   ((io:beta-2-format? data) $delectus-format-beta-2)
   (else #f)))

;;; ----------------------------------------------------------------------
;;; converting delectus formats
;;; ----------------------------------------------------------------------

(define $data-converters (make-table))

(define (io:from-format-alpha-1 data)
  (let* ((columns (map (lambda (c) (column:make (vector-ref c 2))) 
                       (vector-ref data 1)))
         (colseq (%make-column-sequence (list->vector columns)))
         (rows (map (lambda (r)(%make-row (map entry:make (vector-ref r 2)) #f #f)) 
                    (vector-ref data 2))))
    (%make-delectus-table colseq (list->vector rows))))

(define (io:from-format-alpha-2 data)
  (let* ((columns (map (lambda (c)(column:make (vector-ref c 1))) 
                       (vector-ref data 2)))
         (colseq (%make-column-sequence (list->vector columns)))
         (rows (map (lambda (r)
                      (%make-row (map (lambda (f) (entry:make (vector-ref f 1))) ; fields
                                      (vector-ref r 1))
                                 #f #f)) 
                    (vector-ref data 6))))
    (%make-delectus-table colseq (list->vector rows))))

(define (io:from-format-alpha-4 data) 
  (let* ((columns (map (lambda (c)(column:make (vector-ref c 1))) 
                       (vector-ref data 2)))
         (colseq (%make-column-sequence (list->vector columns)))
         (rows (map (lambda (r)
                      (%make-row (map (lambda (f) (entry:make (vector-ref f 1)))
                                      (vector-ref r 1))
                                 #f #f)) 
                    (vector-ref data 6))))
    (%make-delectus-table colseq (list->vector rows))))

(define (io:from-format-beta-2 data) 
  (let* ((format-version (vector-ref data 1))
         (columns-data (vector-ref data 2))
         (columns (map (lambda (cdata)(%make-column (vector-ref cdata 1)(vector-ref cdata 2))) 
                       columns-data))
         (colseq (%make-column-sequence (list->vector columns)))
         (rows-data (vector-ref data 8))
         (rows (map (lambda (rdata) 
                      (let* ((fields-data (vector-ref rdata 1))
                             (entries (map (lambda (fd)(entry:make (vector-ref fd 1)))
                                           fields-data))
                             (rdel? (vector-ref rdata 2)))
                        (%make-row (list->vector entries) #f #f))) 
                    rows-data)))
    (%make-delectus-table colseq (list->vector rows))))

(table-set! $data-converters $delectus-format-alpha-1 io:from-format-alpha-1)
(table-set! $data-converters $delectus-format-alpha-2 io:from-format-alpha-2)
(table-set! $data-converters $delectus-format-alpha-4 io:from-format-alpha-4)
(table-set! $data-converters $delectus-format-beta-2 io:from-format-beta-2)

(define (converter-for-format data)
  (table-ref $data-converters (delectus-format data) #f))

(define (data->table data)
  (let ((converter (converter-for-format data)))
    (converter data)))

