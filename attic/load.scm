;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       lecter
;;;; Purpose:       lecter loader for interactive testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the lecter sources are at another pathname

(define $lecter-root "/Users/mikel/Workshop/src/delectus/lecter")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define $load-files
  '("/src/uuid.scm"
    "/src/constants.scm"
    "/src/lists.scm"
    "/src/vectors.scm"
    "/src/strings.scm"
    "/src/Sort.scm"
    "/src/sort-keys.scm"
    "/src/filter-keys.scm"
    "/src/functions.scm"
    "/src/entries.scm"
    "/src/rows.scm"
    "/src/columns.scm"
    "/src/tables.scm"
    "/src/views.scm"
    "/src/registry.scm"
    "/src/csv.scm"
    "/src/io-formats.scm"
    "/src/io.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-lecter)
  (for-each (lambda (f)
              (let ((p (string-append $lecter-root f)))
                (load p)))
            $load-files))

;;; (load-lecter)
;;; (define $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (define $junior-movies-path "/Users/mikel/Workshop/src/delectus/test-data/junior-movies.delectus")
