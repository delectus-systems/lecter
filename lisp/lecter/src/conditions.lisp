;;;; ***********************************************************************
;;;;
;;;; Name:          conditions.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       conditions and error handling
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

(define-condition delectus-error (error)
  ((message :initarg :message :reader message)
   (error-code :initform nil :initarg :code :reader error-code))
  (:report (lambda (condition stream)
             (if (error-code condition)
                 (format stream "~A (error code: ~S, ~S)~&"
                         (message condition)
                         (error-code condition)
                         (delectus-error-message (error-code condition)))
                 (format stream "~A~&" (message condition))))))


(define-condition delectus-unknown-error (delectus-error)
  ((delegated-error :initform nil :initarg :error :reader delegated-error)))

(defmethod delectus-error-message ((code integer))
  (format nil "something went wrong; it signaled the unrecognized error code ~S"
          code))

(defmethod delectus-error-message ((code (eql $ERR_NO_ERROR))) "no error")
(defmethod delectus-error-message ((code (eql $ERR_UNKNOWN_ERROR))) "unknown error")
(defmethod delectus-error-message ((code (eql $ERR_CANT_CREATE))) "can't create a Delectus object")
(defmethod delectus-error-message ((code (eql $ERR_CANT_ADD_ROW))) "can't add a row")
(defmethod delectus-error-message ((code (eql $ERR_CANT_ADD_COLUMN))) "can't add a column")
(defmethod delectus-error-message ((code (eql $ERR_NO_SUCH_COLUMN))) "no such column")
(defmethod delectus-error-message ((code (eql $ERR_INDEX_OUT_OF_RANGE))) "index out of range")
(defmethod delectus-error-message ((code (eql $ERR_CANT_UPDATE))) "can't update a document")
(defmethod delectus-error-message ((code (eql $ERR_CANT_WRITE))) "can't write a file")
(defmethod delectus-error-message ((code (eql $ERR_CANT_READ))) "can't read a file")
(defmethod delectus-error-message ((code (eql $ERR_BAD_FORMAT))) "not a valid Delectus file")
(defmethod delectus-error-message ((code (eql $ERR_NO_DOCUMENT))) "no such document")
(defmethod delectus-error-message ((code (eql $ERR_CANT_RELEASE))) "can't release a Delectus document")

;;; (delectus-error-message $ERR_NO_ERROR)

(defmethod delectus-error ((msg string) (code integer))
  (error 'delectus-unknown-error
         :message msg
         :code code))
