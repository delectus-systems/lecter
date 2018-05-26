;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strings.scm
;;;; Project:       Delectus
;;;; Purpose:       string utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format "Incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
	             ((#\s)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (write-char #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format "Unrecognized escape sequence")))))
              (else (write-char (car format-list) buffer)
                    (loop (cdr format-list) objects)))))))

(define (string-contains-ci? str substr)
  (let ((str-len (string-length str))
        (substr-len (string-length substr)))
    (if (zero? substr-len)
        #t
        (if (zero? str-len)
            #f
            (if (< str-len substr-len)
                #f
                (if (= str-len substr-len)
                    (string-ci=? str substr)
                    (let ((start-search-ch (string-ref substr 0))
                          (search-end-pos (- str-len substr-len)))
                      (let outer-loop ((i 0))
                        (if (> i search-end-pos)
                            #f
                            (if (char-ci=? start-search-ch (string-ref str i))
                                (let inner-loop ((j 0))
                                  (if (>= j substr-len)
                                      #t
                                      (if (char-ci=? (string-ref substr j)
                                                     (string-ref str (+ i j)))
                                          (inner-loop (+ j 1))
                                          (outer-loop (+ i 1)))))
                                (outer-loop (+ i 1))))))))))))

(define (string-every? pred s)
  (let ((strlen (string-length s)))
    (let loop ((i 0))
      (if (< i strlen)
          (if (pred (string-ref s i))
              (loop (+ i 1))
              #f)
          #t))))
