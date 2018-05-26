;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.scm
;;;; Project:       Delectus
;;;; Purpose:       reading and writing csv files
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; CSV input
;;; ----------------------------------------------------------------------
;;;; Author:        Bradley Lucier
;;;;                https://mercure.iro.umontreal.ca/pipermail/gambit-list/2007-February/001135.html
;;;;                with modifications by mikel evins

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (fixnum)
         (not safe))

(define (read-csv-record . args)

  (define (read-csv sep port)

    (define (add-char-to-field c field)
      (let ((length  (field-length field))
	    (buffer  (field-buffer field)))
	(if (< length (string-length buffer))
	    (begin
	      (string-set! buffer length c)
	      (field-length-set! field (+ length 1))
	      field)
	    (let ((new-buffer (string-append buffer (make-string length))))
	      (string-set! new-buffer length c)
	      (field-length-set! field (+ length 1))
	      (field-buffer-set! field new-buffer)
	      field))))

    (define (extract-string-from-field! field)
      (let ((result (substring (field-buffer field) 0 (field-length  
                                                       field))))
	(reset-field! field)
	result))

    (define (new-field)
      (cons (make-string 800)
	    0))

    (define (field-buffer field)
      (car field))

    (define (field-buffer-set! field value)
      (set-car! field value))

    (define (field-length field)
      (cdr field))

    (define (field-length-set! field value)
      (set-cdr! field value))

    (define (reset-field! field)
      (field-length-set! field 0)
      field)

    (define (add-field! field fields)
      (cons (extract-string-from-field! field) fields))


    (define (start field fields)
      (let ((c (read-char port)))
        (cond ((eof-object? c)
	       (reverse fields))
              ((char=? #\return c)
	       (carriage-return field fields))
              ((char=? #\newline c)
	       (line-feed field fields))
              ((char=? #\" c)
	       (quoted-field field fields))
              ((char=? sep c)
	       (let ((fields (add-field! field fields)))
		 (not-field field fields)))
              (else
	       (unquoted-field (add-char-to-field c field) fields)))))

    (define (not-field field fields)
      (let ((c (read-char port)))
        (cond ((eof-object? c)
	       (cons "" fields))
              ((char=? #\return c)
	       (carriage-return '() (add-field! field fields)))
              ((char=? #\newline c)
	       (line-feed '() (add-field! field fields)))
              ((char=? #\" c)
	       (quoted-field field fields))
              ((char=? sep c)
	       (let ((fields (add-field! field fields)))
		 (not-field field fields)))
	      (else
	       (unquoted-field (add-char-to-field c field) fields)))))

    (define (quoted-field field fields)
      (let ((c (read-char port)))
        (cond ((eof-object? c)
	       (add-field! field fields))
              ((char=? #\" c)
	       (may-be-doubled-quotes field fields))
              (else
	       (quoted-field (add-char-to-field c field) fields)))))

    (define (may-be-doubled-quotes field fields)
      (let ((c (read-char port)))
        (cond ((eof-object? c)
	       (add-field! field fields))
              ((char=? #\return c)
	       (carriage-return '() (add-field! field fields)))
              ((char=? #\newline c)
	       (line-feed '() (add-field! field fields)))
              ((char=? #\" c)
	       (quoted-field (add-char-to-field #\" field) fields))
              ((char=? sep c)
	       (let ((fields (add-field! field fields)))
		 (not-field field fields)))
              (else
	       (unquoted-field (add-char-to-field c field) fields)))))

    (define (unquoted-field field fields)
      (let ((c (read-char port)))
        (cond ((eof-object? c)
	       (add-field! field fields))
              ((char=? #\return c)
	       (carriage-return '() (add-field! field fields)))
              ((char=? #\newline c)
	       (line-feed '() (add-field! field fields)))
              ((char=? sep c)
	       (let ((fields (add-field! field fields)))
		 (not-field field fields)) )
              (else
	       (unquoted-field (add-char-to-field c field) fields)))))

    (define (carriage-return field fields)
      (if (or (eof-object? (peek-char port))
              (char=? #\newline (peek-char port)))
	  (read-char port))
      fields)

    (define (line-feed field fields)
      (if (or (eof-object? (peek-char port))
              (char=? #\return (peek-char port)))
          (read-char port))
      fields)

    (if (eof-object? (peek-char port))
	(peek-char port)
	(reverse (start (new-field) '()))))

  (cond ((null? args)
	 (read-csv #\, (current-input-port)))
        ((and (null? (cdr args))
	      (char? (car args)))
	 (read-csv (car args) (current-input-port)))
        ((and (null? (cdr args))
	      (port? (car args)))
	 (read-csv #\, (car args)))
        ((and (pair? (cdr args)) (null? (cddr args))
              (char? (car args)) (port? (cadr args)))
	 (read-csv (car args) (cadr args)))
        (else
	 (car '()))))

;;; ----------------------------------------------------------------------
;;; CSV API
;;; ----------------------------------------------------------------------

(define (%read-csv-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((ch (peek-char port))
                 (result '()))
        (if (eof-object? ch)
            (reverse (cdr result))
            (loop (peek-char port)
                  (cons (read-csv-record port)
                        result)))))))

(define (csv:read path)
  (let* ((records (%read-csv-file path))
         (cols (car records))
         (rows (cdr records)))
    (table:make columns: cols rows: rows)))

;;; (define $zip-path "/Users/mikel/Projects/delectus/delectus/test-data/zipcode.csv")
;;; (define $zips (csv:read $zip-path))
;;; (delectus-table? $zips)
;;; (table:column-sequence $zips)
;;; (table:value-at $zips "city" 43190)
;;; (table:count-rows $zips)