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
;;; MACROS
;;; ======================================================================

(define (%if-error-aux errval thunk)
  (let ((handler (lambda (err) errval)))
    (with-exception-catcher handler thunk)))

(define-macro (if-error errval . body)
  `(%if-error-aux ,errval 
                  (lambda () ,@body)))

;;; ======================================================================
;;; FILE I/O UTILS
;;; ======================================================================

(define (%concat-chunks chunk more-chunks)
  (if (null? more-chunks)
      chunk
      (let ((new-chunk (u8vector-append chunk (car more-chunks))))
        (%concat-chunks new-chunk (cdr more-chunks)))))

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
        (%concat-chunks (car chunks)(cdr chunks))
        #f)))

(define (delectus-format-version src-path)
  (if-error "INVALID"
            (let* ((raw (io:read-binary-file src-path))
                   (data (u8vector->object raw)))
              (delectus-format data))))

;;; (define $inpath "/Users/mikel/Workshop/src/delectus/lecter/test-data/junior-movies.delectus")
;;; (define $data (delectus->lisp $inpath))

(define (file-readable? src-path)
  (and (> (string-length src-path) 0)
       (file-exists? src-path)
       (let* ((userinfo (user-info (user-name)))
              (owner (file-owner src-path))
              (group (file-group src-path))
              (mode (file-mode src-path))
              (group-can-read #b000100000)
              (others-can-read #b000000100)
              (owner-can-read #b100000000))
         (or (and (= owner (user-info-uid userinfo))
                  (not (zero? (bitwise-and mode owner-can-read))))
             (and (= group (user-info-gid userinfo))
                  (not (zero? (bitwise-and mode group-can-read))))
             (not (zero? (bitwise-and mode others-can-read)))))))

(define (error-input-not-readable src-path)
  (error (format "File not readable: ~s" src-path)))

;;; ======================================================================
;;; Native Delectus I/O
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; reading delectus files
;;; ----------------------------------------------------------------------

(define (read-delectus-file path)
  (let* ((raw (io:read-binary-file path))
         (data (u8vector->object raw))
         (tbl (if (delectus-table? data)
                  data
                  (data->table data))))
    (reg:register-delectus! tbl)))

;;; (define $jr-path "/Users/mikel/Workshop/src/delectus/test-data/junior-movies.delectus")
;;; (define $jr (read-delectus-file $jr-path))

(define (delectus->table src-path)
  (let* ((raw (io:read-binary-file src-path))
         (data (u8vector->object raw))
         (converter (converter-for-format data))
         (tbl (if (delectus-table? data)
                  (compacted-delectus-table data)
                  (compacted-delectus-table (data->table data)))))
    (or tbl #f)))

;;; (define $jr-path "/Users/mikel/Workshop/src/delectus/test-data/junior-movies.delectus")
;;; (define $jr (delectus->table $jr-path))

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

(define (value->csv val)
  (cond ((equal? #t val) "True")
        ((equal? #f val) "")
        ((equal? '() val) "")
        (else val)))

(define (write-columns-csv tbl out)
  (let* ((cols (table:column-labels tbl)))
    (write (car cols) out)
    (for-each (lambda (col)
                (write-char #\, out)
                (write col out))
              (cdr cols))
    (newline out)))

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
  (vector-for-each (lambda (r)(write-row-csv r out)(newline out))
                   (table:rows tbl)))

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

;;; (define $ziptest-path "/Users/mikel/Desktop/ziptest.csv")
;;; (write-csv-file (find-document $zipid) $ziptest-path)

;;; ----------------------------------------------------------------------
;;; delectus->csv
;;; ----------------------------------------------------------------------

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

;;; (define $inpath "/Users/mikel/Workshop/src/delectus/delectus2csv/testdata/Movies.delectus")
;;; (define $outpath "/Users/mikel/Desktop/Movies.csv")
;;; (delectus2csv $inpath $outpath)

(define (write-csv path)
  (delectus->csv path))

;;; ======================================================================
;;; LISP I/O
;;; ======================================================================

(define (value->lisp val)
  (cond ((equal? #t val) "True")
        ((equal? #f val) "")
        ((equal? '() val) "")
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
    (list 'COLUMNS cols
          'ROWS rows)))

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

(define (write-sexp path)
  (let* ((data (delectus->lisp path))
         (columns-tail (member 'COLUMNS data))
         (columns (if columns-tail (cadr columns-tail) '()))
         (rows-tail (member 'ROWS data))
         (rows (if rows-tail (cadr rows-tail) '())))
    (display ":DELECTUS :SEXP")
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
    (display ":ROWS")
    (for-each (lambda (row)
                (newline)
                (display "(")
                (for-each (lambda (it)
                            (display " ")
                            (write it))
                          row)
                (display " )"))
              rows)))


;;; (define $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (define $movies (delectus->lisp $movies-path))

;;; ======================================================================
;;; JSON I/O
;;; ======================================================================

(define (%write-json-list-items items #!optional (write-fn write))
  (let ((head (car items))
        (tail (cdr items)))
    (write-fn head)
    (if (not (null? tail))
        (begin (display ", ")
               (%write-json-list-items tail write-fn)))))

(define (%write-json-list items  #!optional (write-fn write))
  (display "[")
  (%write-json-list-items items write-fn)
  (display "]"))

(define (%write-json-rows items  #!optional (write-fn write))
  (let ((head (car items))
        (tail (cdr items)))
    (display "    ")
    (write-fn head)
    (if (not (null? tail))
        (begin (display ", ")(newline)
               (%write-json-rows tail write-fn)))))

(define (write-json path)
  (let* ((data (delectus->lisp path))
         (columns-tail (member 'COLUMNS data))
         (columns (if columns-tail (cadr columns-tail) '()))
         (rows-tail (member 'ROWS data))
         (rows (if rows-tail (cadr rows-tail) '())))
    (display "{ \"columns\": ")
    (%write-json-list columns write)
    (display ",")(newline)
    (display "  \"rows\": [")(newline)
    (%write-json-rows rows %write-json-list)
    (display "]}")))

;;; ======================================================================
;;; CouchDB I/O
;;; ======================================================================

(define (make-couch-listitem delectus-list columns row)
  (let* ((uuid (make-uuid))
         (itemid (string-append "item:" uuid))
         (list-identifier (assoc "_id" delectus-list))
         (listid (if list-identifier (cdr list-identifier) "null"))
         (fields (map cons columns row)))
    `(("_id" . ,itemid)
      ("type" . "ListItem")
      ("list" . ,listid)
      ("fields" . ,fields))))

(define (write-list-elements elts)
  (if (not (null? elts))
      (begin (write (car elts))
             (let loop ((tail (cdr elts)))
               (if (not (null? tail))
                   (begin (display ",")
                          (write (car tail))
                          (loop (cdr tail))))))))

(define (write-list-row listid columns row)
  (let ((id (string-append "item:" (make-uuid)))
        (fields (map cons columns row)))
    (display "{")(write "_id")(display ":")(write id)(display ", ")
    (write "type")(display ":")(write "ListItem")(display ", ")
    (write "List")(display ":")(write listid)(display ", ")
    (if (not (null? fields))
        (let* ((field (car fields))
               (key (car field))
               (value (cdr field)))
          (write key)(display ": ")(write value)
          (let loop ((tail (cdr fields)))
            (if (not (null? tail))
                (let* ((field (car tail))
                       (key (car field))
                       (value (cdr field)))
                  (display ", ")(write key)(display ": ")(write value)
                  (loop (cdr tail)))))))
    (display "}")))

(define (write-list-rows listid columns rows)
  (if (not (null? rows))
      (begin (write-list-row listid columns (car rows))
             (let loop ((tail (cdr rows)))
               (if (not (null? tail))
                   (begin (newline)
                          (write-list-row listid columns (car tail))
                          (loop (cdr tail))))))))

(define (write-couchdb path)
  (let* ((name (path-strip-extension (path-strip-directory path)))
         (data (delectus->lisp path))
         (columns-tail (member 'COLUMNS data))
         (columns (if columns-tail (cadr columns-tail) '()))
         (rows-tail (member 'ROWS data))
         (rows (if rows-tail (cadr rows-tail) '()))
         (list-uuid (make-uuid))
         (listid (string-append "list:" list-uuid)))
    (display "{")
    (write "_id")(display ":")(write listid)(display ",")
    (write "name")(display ":")(write name)(display ",")
    (write "type")(display ":")(write "List")(display ",")
    (write "columns")(display ":[")(write-list-elements columns)(display "]")
    (display "}")(newline)
    (write-list-rows listid columns rows)))
