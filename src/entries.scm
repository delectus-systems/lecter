;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          data.scm
;;;; Project:       Delectus
;;;; Purpose:       data entries
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; entries
;;; ----------------------------------------------------------------------

(define-type entry
  id: DB7F49A3-BBF6-40F3-95A1-038CCACE1D88
  constructor: %make-entry
  (value entry:value %set-entry-value!)
  (number-value %entry-number-value %set-entry-number-value!))

(define (%->entry-number-value thing)
  (if thing
      (if (string? thing)
          (if (string=? thing "")
              #f
              (string->number thing))
          (if (null? thing)
              #f
              (error "Invalid entry value" thing)))
      #f))

(define (entry:make val)
  (%make-entry val (%->entry-number-value val)))

(define (entry:set-value! e val)
  (%set-entry-value! e val)
  (%set-entry-number-value! e (%->entry-number-value val))
  val)

