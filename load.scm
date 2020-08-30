;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Delectus
;;;; Purpose:       system loader for engine development and testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the Delectus sources are at another pathname

(define $delectus-root "/Users/mikel/Workshop/src/lecter/scm/")
(define $test-data-root "/Users/mikel/Workshop/src/delectus/test-data/")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define $load-files
  '(
    "lib/uuid.scm"
    "base/constants.scm"
    "lib/lists.scm"
    "lib/vectors.scm"
    "lib/strings.scm"
    "lib/Sort.scm"
    "lib/sort-keys.scm"
    "lib/filter-keys.scm"
    "lib/functions.scm"
    "api/api.scm"
    "data/entries.scm"
    "data/rows.scm"
    "data/columns.scm"
    "data/tables.scm"
    "data/views.scm"
    "data/registry.scm"
    "api/engine.scm"
    "io/csv.scm"
    "io/io-formats.scm"
    "io/io.scm"
    "test/test-utils.scm"
    "test/tests.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-delectus)
  (for-each (lambda (f)
              (let ((p (string-append $delectus-root f)))
                (load p)))
            $load-files))

;;; (load-delectus)
;;; (run-all-tests)
