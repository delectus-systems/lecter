;;; UUID generation
;;; See: http://www.ietf.org/rfc/rfc4122.txt
;;;
;;; Version 4 UUID, see section 4.4
;;;
;;; (taken from the termite distribution but modified by Per Eckerdal
;;; to return a string instead of a symbol to avoid the memory leak of
;;; an unbounded amount of interned symbols)

(define (make-uuid)
  (define hex
    '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))
  (random-source-randomize! default-random-source)
  (let ((n1 (random-integer 65536))
        (n2 (random-integer 65536))
        (n3 (random-integer 65536))
        (n4 (random-integer 65536))
        (n5 (random-integer 65536))
        (n6 (random-integer 65536))
        (n7 (random-integer 65536))
        (n8 (random-integer 65536)))
    (string
     ;; time_lo
     (vector-ref hex (extract-bit-field 4 12 n1))
     (vector-ref hex (extract-bit-field 4  8 n1))
     (vector-ref hex (extract-bit-field 4  4 n1))
     (vector-ref hex (extract-bit-field 4  0 n1))
     (vector-ref hex (extract-bit-field 4 12 n2))
     (vector-ref hex (extract-bit-field 4  8 n2))
     (vector-ref hex (extract-bit-field 4  4 n2))
     (vector-ref hex (extract-bit-field 4  0 n2))
     #\-
     ;; time_mid
     (vector-ref hex (extract-bit-field 4 12 n3))
     (vector-ref hex (extract-bit-field 4  8 n3))
     (vector-ref hex (extract-bit-field 4  4 n3))
     (vector-ref hex (extract-bit-field 4  0 n3))
     #\-
     ;; time_hi_and_version
     (vector-ref hex #b0100)
     (vector-ref hex (extract-bit-field 4  8 n4))
     (vector-ref hex (extract-bit-field 4  4 n4))
     (vector-ref hex (extract-bit-field 4  0 n4))
     #\-
     ;; clock_seq_hi_and_reserved
     (vector-ref hex (bitwise-ior (extract-bit-field 2 12 n5) #b1000))
     (vector-ref hex (extract-bit-field 4  8 n5))
     ;; clock_seq_low
     (vector-ref hex (extract-bit-field 4  4 n5))
     (vector-ref hex (extract-bit-field 4  0 n5))
     #\-
     ;; node
     (vector-ref hex (extract-bit-field 4 12 n6))
     (vector-ref hex (extract-bit-field 4  8 n6))
     (vector-ref hex (extract-bit-field 4  4 n6))
     (vector-ref hex (extract-bit-field 4  0 n6))
     (vector-ref hex (extract-bit-field 4 12 n7))
     (vector-ref hex (extract-bit-field 4  8 n7))
     (vector-ref hex (extract-bit-field 4  4 n7))
     (vector-ref hex (extract-bit-field 4  0 n7))
     (vector-ref hex (extract-bit-field 4 12 n8))
     (vector-ref hex (extract-bit-field 4  8 n8))
     (vector-ref hex (extract-bit-field 4  4 n8))
     (vector-ref hex (extract-bit-field 4  0 n8)))))
