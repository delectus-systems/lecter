;;;; ***********************************************************************
;;;;
;;;; Name:          lecter.scm
;;;; Project:       Delectus 1->2 conversion utility
;;;; Purpose:       main command-line program
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; USAGE:
;;; lecter --version => returns the version of lecter to stdio
;;; lecter --format-version <pathname> => delectus format version number, or #f
;;; lecter --format-name <pathname> => name of delectus format version, or "INVALID"
;;; lecter --sexp <pathname> => returns s-expression conversion of delectus data to stdio
;;; lecter --csv <pathname> => returns csv conversion of delectus data to stdio
;;; lecter --jsonl <pathname> => returns csv conversion of delectus data to stdio

(define $lecter-version-string "1.5.2d1")

(define (print-lecter-usage)
  (newline)
  (display "USAGE:")(newline)
  (display "  lecter --version # prints the version of lecter")(newline)
  (display "  lecter --uuid # prints a newly-generated v4 UUID")(newline)
  (display "  lecter --format-version PATH # prints the version number of the delectus file format,")(newline)
  (display "                               # or INVALID if it's not a recognized Delectus format")(newline)
  (display "  lecter --format-name PATH # prints the version name of the delectus file format,")(newline)
  (display "                            # or INVALID if it's not a recognized Delectus format")(newline)
  (display "  lecter --lisp PATH # prints the Delectus data to stdio as Common Lisp expressions")(newline)
  (display "  lecter --sexp PATH # prints the Delectus data to stdio as Common Lisp expressions")(newline)
  (display "  lecter --csv PATH # prints the Delectus data to stdio as CSV")(newline)
  (newline))

;;; main command interpreter

(let* ((args (cdr (command-line)))
       (argcount (length args)))
  (cond ((< argcount 1)(print-lecter-usage))
        ((= argcount 1)(let ((option (list-ref args 0)))
                         (cond
                          ;; --version
                          ((equal? option "--version")
                           (begin (newline)
                                  (display $lecter-version-string)
                                  (newline)))
                          ;; --uuid
                          ((equal? option "--uuid")
                           (begin (write (make-uuid))
                                  (newline)))
                          ;; unrecognized option
                          (else (print-lecter-usage)))))
        ((= argcount 2) (let ((option (list-ref args 0))
                              (path (list-ref args 1)))
                          (cond ;; --format-version
                           ((equal? option "--format-version")
                            (let* ((format-number (delectus-format-version path)))
                              (begin (write format-number)
                                     (newline))))
                           ;; --format-name
                           ((equal? option "--format-name")
                            (let* ((format-number (delectus-format-version path)))
                              (if (equal? "INVALID" format-number)
                                  (begin (write format-number)
                                         (newline))
                                  (let ((format-name (delectus-format-number->name format-number)))
                                    (write format-name)
                                    (newline)))))
                           ;; --lisp
                           ((or (equal? option "--lisp")(equal? option "--sexp")) (write-lisp path))
                           ;; --csv
                           ((equal? option "--csv") (delectus->csv path))
                           ;; unrecognized options
                           (else (print-lecter-usage)))))
        (else (print-lecter-usage))))

