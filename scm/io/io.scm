;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          io.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing Delectus documents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ======================================================================
;;; general utilities
;;; ======================================================================

(define (io:read-binary-file path)
  (let* ((in #f)
         (data-bytes #f)
         (chunks (list))
         (buffer-length 4096)
         (data-buffer (make-u8vector buffer-length 0)))
    (dynamic-wind
        (lambda () (set! in (open-input-file path)))
        (lambda ()
          ;; read bytes from the file
          (do ((byte-count (read-subu8vector data-buffer 0 buffer-length in)
                           (read-subu8vector data-buffer 0 buffer-length in)))
              ((< byte-count buffer-length) (set! chunks (append chunks (list (subu8vector data-buffer 0 byte-count)))))
            (set! chunks (append chunks (list (subu8vector data-buffer 0 byte-count))))))
        (lambda () (close-input-port in)))
    ;; convert the bytes to a scheme object and return it
    (if (not (null? chunks))
        (apply u8vector-append chunks)
        #f)))

;;; ======================================================================
;;; Native Delectus I/O
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-data path)
  (let* ((raw (io:read-binary-file path))
         (data (u8vector->object raw)))
    (if (delectus-table? data)
        data
        (data->table data))))

(define (read-delectus-file path)
  (reg:register-delectus! (read-delectus-data path)))

;;; (define $jr-path "/Users/mikel/Workshop/src/delectus/test-data/junior-movies.delectus")
;;; (define $jr (read-delectus-file $jr-path))

;;; ----------------------------------------------------------------------
;;; writing delectus files
;;; ----------------------------------------------------------------------

(define (write-delectus-file tbl dest-path)
  (let* ((out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () 
          (let* ((bytes (object->u8vector tbl))
                 (bytecount (u8vector-length bytes)))
            (write-subu8vector bytes 0 bytecount out)))
        (lambda () (close-output-port out)))
    dest-path))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $zipid (api:read-delectus-csv $zip-path))
;;; (define $ziptest-path "/Users/mikel/Desktop/ziptest.delectus")
;;; (write-delectus-file (find-document $zipid) $ziptest-path)
;;; (define $ziptest (read-delectus-file $ziptest-path))

;;; ======================================================================
;;; compaction
;;; ======================================================================
;;; creates a copy of a delectus table without deleted data, for
;;; export formats

(define (compacted-delectus-table tbl)
  (let* ((old-data (object->u8vector tbl))
         (new-table (u8vector->object old-data)))
    (table:compact! new-table)
    new-table))

;;; ======================================================================
;;; CSV I/O
;;; ======================================================================

(define (read-csv-file path)
  (let* ((table (csv:read path)))
    (reg:register-delectus! table)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zipid (read-csv-file $zip-path))
;;; (document? (find-document $zipid))

;;; (define $in-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode_10k.csv")
;;; (define $out-path "/Users/mikel/Desktop/testdelectus.csv")
;;; (define $out-path"/private/var/folders/6r/6rDpY9CfEAOBLrIbLcACj++++TI/TemporaryItems/(A Document Being Saved By Delectus 3)/testdelectus.csv")
;;; (define $doc (read-csv-file $in-path))
;;; (api:write-delectus-csv $doc $out-path)


(define (value->csv val)
  (cond ((equal? #t val) "True")
        ((equal? #f val) "")
        ((equal? '() val) "")
        (else val)))

(define (write-columns-csv tbl out)
  (let* ((cols (table:column-labels tbl)))
    (if (null? cols)
        #f
        (begin
          (write (car cols) out)
          (for-each (lambda (col)
                      (write-char #\, out)
                      (write col out))
                    (cdr cols))
          (newline out)))))

(define (print-columns-csv tbl)
  (let* ((cols (table:column-labels tbl)))
    (write (car cols))
    (for-each (lambda (col)
                (write-char #\,)
                (write col))
              (cdr cols))
    (newline)))

(define (write-row-csv r out)
  (let ((eltcount (vector-length (row:entries r))))
    (if (> eltcount 0)
        (begin
          (write (value->csv (row:element r 0)) out)
          (if (> eltcount 1)
              (let loop ((i 1))
                (if (< i eltcount)
                    (begin
                      (write-char #\, out)
                      (write (value->csv (row:element r i)) out)
                      (loop (+ i 1))))))))))

(define (print-row-csv r)
  (let ((eltcount (vector-length (row:entries r))))
    (if (> eltcount 0)
        (begin
          (write (value->csv (row:element r 0)))
          (if (> eltcount 1)
              (let loop ((i 1))
                (if (< i eltcount)
                    (begin
                      (write-char #\,)
                      (write (value->csv (row:element r i)))
                      (loop (+ i 1))))))))))

(define (write-table-csv tbl out)
  (write-columns-csv tbl out)
  (let ((rows (table:rows tbl)))
    (if (> (vector-length rows) 0)
        (vector-for-each (lambda (r)
                           (write-row-csv r out)
                           (newline out))
                         rows)
        (newline out))))

(define (print-table-csv tbl)
  (print-columns-csv tbl)
  (vector-for-each (lambda (r)(print-row-csv r)(newline))
                   (table:rows tbl)))

(define (write-csv-file tbl dest-path)
  (let ((out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () 
          (write-table-csv tbl out))
        (lambda () (close-output-port out))))
  dest-path)

(define (write-csv path)
  (let ((out #f))
    (dynamic-wind
        (lambda () (set! out (open-output-file dest-path)))
        (lambda () 
          (write-table-csv tbl out))
        (lambda () (close-output-port out))))
  dest-path)

;;; (define $ziptest-path "/Users/mikel/Desktop/ziptest.csv")
;;; (write-csv-file (find-document $zipid) $ziptest-path)

;;; =====================================================================
;;; conversions to text output formats
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; CSV
;;; ---------------------------------------------------------------------

(define (delectus->csv src-path)
  (let* ((raw (io:read-binary-file src-path))
         (data (u8vector->object raw))
         (converter (converter-for-format data))
         (tbl (if (delectus-table? data)
                  (compacted-delectus-table data)
                  (compacted-delectus-table (data->table data)))))
    (if tbl
        (print-table-csv tbl)
        (begin (format "~%Not a Delectus 1.x file: ~s" src-path)
               (format "~%No output written.~%")
               $ERR_BAD_FORMAT))))

;;; (define $inpath (string-append $test-data-root "Movies.delectus"))
;;; (delectus->csv $inpath)

(define (delectus->csv-file in-path out-path)
  (let* ((raw (io:read-binary-file in-path))
         (data (u8vector->object raw))
         (converter (converter-for-format data))
         (tbl (if (delectus-table? data)
                  (compacted-delectus-table data)
                  (compacted-delectus-table (data->table data)))))
    (if tbl
        (let ((out #f))
          (dynamic-wind
              (lambda () (set! out (open-output-file out-path)))
              (lambda () 
                (write-table-csv tbl out)
                $ERR_NO_ERROR)
              (lambda () (close-output-port out))))
        (begin (format "~%Not a Delectus 1.x file: ~s" in-path)
               (format "~%No output written.~%")
               $ERR_BAD_FORMAT))))

;;; (define $movies-in (string-append $test-data-root "Movies.delectus"))
;;; (define $movies-out "/Users/mikel/Desktop/Movies.csv")
;;; (delectus->csv-file $movies-in $movies-out)

;;; (define $zips-in (string-append $test-data-root "zipcode.delectus"))
;;; (define $zips-out "/Users/mikel/Desktop/zipcode.csv")
;;; (delectus->csv-file $zips-in $zips-out)


;;; ---------------------------------------------------------------------
;;; output as lisp expressions
;;; ---------------------------------------------------------------------

(define (value->lisp val)
  (cond ((equal? #t val) "T")
        ((equal? #f val) "NIL")
        ((equal? '() val) "NIL")
        (else val)))

(define (columns->lisp tbl)
  (let* ((colseq (table:column-sequence tbl))
         (cols (vector->list (column-sequence:columns colseq))))
    (map (lambda (col)(column:label col))
         cols)))

(define (rows->lisp tbl)
  (map (lambda (row)
         (let* ((entries (vector->list (row:entries row))))
           (map (lambda (entry)(value->lisp (entry:value entry)))
                entries)))
       (vector->list (table:rows tbl))))

(define (table->lisp tbl)
  (let* ((cols (columns->lisp tbl))
         (rows (rows->lisp tbl)))
    (list ':COLUMNS cols
          ':ROWS rows)))

(define (delectus->lisp src-path)
  (let* ((raw (io:read-binary-file src-path))
         (data (u8vector->object raw))
         (converter (converter-for-format data))
         (tbl (if (delectus-table? data)
                  (compacted-delectus-table data)
                  (compacted-delectus-table (data->table data)))))
    (if tbl
        (table->lisp tbl)
        (begin (format "~%Not a Delectus 1.x file: ~s" src-path)
               (format "~%No conversion performed.~%")
               $ERR_BAD_FORMAT))))

(define (write-lisp path)
  (let* ((data (delectus->lisp path))
         (columns-tail (member ':COLUMNS data))
         (columns (if columns-tail (cadr columns-tail) '()))
         (rows-tail (member ':ROWS data))
         (rows (if rows-tail (cadr rows-tail) '())))
    (display "(")
    (display ":DELECTUS :LISP")
    (newline)
    (display ":COLUMNS")
    (newline)
    (display "(")
    (for-each (lambda (column)
                (display " ")
                (write column))
              columns)
    (display " )")
    (newline)
    (display ":ROWS (")
    (for-each (lambda (row)
                (newline)
                (display "(")
                (for-each (lambda (it)
                            (display " ")
                            (write it))
                          row)
                (display " )"))
              rows)
    (display "))")))


;;; (define $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (define $movies (delectus->lisp $movies-path))
