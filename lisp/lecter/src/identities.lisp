;;;; ***********************************************************************
;;;;
;;;; Name:          identities.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus identities
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; functions for creating and converting between Delectus identities
;;; and identity-strings.

;;; ---------------------------------------------------------------------
;;; identities
;;; ---------------------------------------------------------------------
;;; an identity is a v4 UUID byte array.

(in-package #:lecter)

(defparameter +identity-vector-length+ 16)

(defmethod as-identity-vector ((thing vector))
  #+lispworks (coerce thing '(simple-vector 16))
  #+sbcl (coerce thing '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16)))
  #+ccl (coerce thing '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (16))))

(defmethod identity? (thing) nil)

(defmethod identity? ((thing vector))
  (and (= +identity-vector-length+
          (length thing))
       (every (lambda (x)(typep x '(unsigned-byte 8)))
              thing)
       t))

;;; (identity? (makeid))
;;; (identity? "foo")

(defmethod uuid->identity ((u uuid:uuid))
  (as-identity-vector (uuid:uuid-to-byte-array u)))

(defmethod identity->uuid ((identity vector))
  (assert (identity identity)() "Not a valid identity: ~S" identity)
  (uuid:byte-array-to-uuid (coerce identity
                                   '(simple-array (unsigned-byte 8) (16)))))

(defmethod makeid ()
  (as-identity-vector (uuid->identity (uuid:make-v4-uuid))))

;;; (time (makeid))
;;; (setf $u (uuid:make-v4-uuid))
;;; (setf $id (uuid->identity $u))
;;; (setf $u2 (identity->uuid $id))
;;; (uuid:uuid= $u $u2)

;;; ---------------------------------------------------------------------
;;; identity strings
;;; ---------------------------------------------------------------------
;;; an identity-string is the first 26 characters of a
;;; base32hex-encoded identity.

(defparameter +delectus-identity-string-length+ 26)

(defmethod identity->string ((id vector))
  (assert (identity? id)() "Not a valid identity: ~S" id)
  (subseq (binascii:encode-base32hex id)
          0 +delectus-identity-string-length+))

;;; (identity->string (makeid))

(defun make-identity-string ()
  (identity->string (makeid)))

;;; (time (make-identity-string))

(defmethod identity-string? (thing) nil)

(defmethod identity-string? ((thing string))
  (if (not (equal +delectus-identity-string-length+
                  (length thing)))
      nil
      (let ((result t))
        (block checking
          (loop for i from 0 below (length thing)
             do (unless (find (elt thing i) binascii::*base32hex-encode-table*)
                  (setf result nil)
                  (return-from checking nil))))
        result)))

(defmethod string->identity ((identity string))
  (assert (identity-string? identity)() "Not a valid identity string: ~S" identity)
  (as-identity-vector (binascii:decode-base32hex identity)))

;;; (string->identity (identity->string (makeid)))
;;; (string->identity (make-identity-string))
