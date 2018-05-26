;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          engine.scm
;;;; Project:       Delectus
;;;; Purpose:       the view/table registry
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $tables (make-table test: =))
(define $views (make-table test: =))

(define (reg:register-table! id tbl)
  (table-set! $tables id tbl))

(define (reg:find-table id)
  (table-ref $tables id #f))

(define (reg:register-view! id v)
  (table-set! $views id v))

(define (reg:find-view id)
  (table-ref $views id #f))

(define (reg:register-delectus! tbl)
  (if (delectus-table? tbl)
      (let* ((tblid (object->serial-number tbl)))
        (reg:register-table! tblid tbl)
        (let ((vw (view:create tblid)))
          (reg:register-view! tblid vw)
          tblid))
      (error "invalid argument to reg:register-delectus!" tbl)))



