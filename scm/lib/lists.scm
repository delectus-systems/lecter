;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.scm
;;;; Project:       Delectus
;;;; Purpose:       list utilities
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (repeat n val)
  (let loop ((counter n)
             (result '()))
    (if (<= counter 0)
        (reverse result)
        (loop (- counter 1)
              (cons val result)))))

(define (range m n #!key (by 1))
  (let ((step-fn (if (<= m n) + -))
        (test-fn (if (<= m n) >= <=)))
    (let loop ((k m)
               (result '()))
      (if (test-fn k n)
          (reverse result)
          (loop (step-fn k by)(cons k result))))))

(define (range-except exceptions m n #!key (by 1))
  (let ((step-fn (if (<= m n) + -))
        (test-fn (if (<= m n) >= <=)))
    (let loop ((k m)
               (result '()))
      (if (test-fn k n)
          (reverse result)
          (loop (step-fn k by)
                (if (memv k exceptions)
                    result
                    (cons k result)))))))

(define (copy-tree x)
  (if (list? x)
      (map copy-tree x)
      x))

(define (plist->alist pl)
  (let loop ((pl pl)
             (result '()))
    (if (null? pl)
        (reverse result)
        (if (null? (cdr pl))
            (error "Malformed plist")
            (loop (cddr pl)
                  (cons (cons (car pl)
                              (cadr pl))
                        result))))))

(define (find-associated key alist #!key (test eqv?)(default #f))
  (let loop ((entries alist))
    (if (null? entries)
        default
        (let ((entry (car entries)))
          (if (test key (car entry))
              entry
              (loop (cdr entries)))))))

(define (contains? pred ls v)
  (let loop ((ls ls))
    (if (null? ls)
        #f
        (if (pred (car ls) v)
            #t
            (loop (cdr ls))))))

(define (duplicates? pred ls)
  (let loop ((ls ls))
    (if (null? ls)
        #f
        (if (contains? pred (cdr ls) (car ls))
            #t
            (loop (cdr ls))))))

(define (filter pred ls)
  (let loop ((ls ls)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (pred (car ls))
            (loop (cdr ls) (cons (car ls) result))
            (loop (cdr ls) result)))))

(define (remove item ls #!key (test eqv?))
  (let loop ((ls ls)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (test item (car ls))
            (loop (cdr ls) result)
            (loop (cdr ls) (cons (car ls) result))))))

(define (select indexes ls)
  (let loop ((ls ls)
             (j 0)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (contains? = indexes j)
            (loop (cdr ls)(+ j 1)(cons (car ls) result))
            (loop (cdr ls)(+ j 1) result)))))

(define (select-not indexes ls)
  (let loop ((ls ls)
             (j 0)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (contains? = indexes j)
            (loop (cdr ls)(+ j 1) result)
            (loop (cdr ls)(+ j 1)(cons (car ls) result))))))

(define (select-if pred ls)
  (let loop ((ls ls)
             (j 0)
             (result '()))
    (if (null? ls)
        (reverse result)
        (if (pred (car ls))
            (loop (cdr ls)(+ j 1)(cons (car ls) result))
            (loop (cdr ls)(+ j 1) result)))))

(define (position pred ls val)
  (let loop ((tl ls)
             (i 0))
    (if (null? tl)
        #f
        (if (pred (car tl) val)
            i
            (loop (cdr tl)(+ 1 i))))))

(define (interpose sep ls)
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (if (null? (cdr items))
            (if (null? result)
                (reverse (cons (car items) result))
                (reverse (cons (car items) 
                               (cons sep result))))
            (loop (cdr items)
                  (if (null? result)
                      (cons (car items) result)
                      (cons (car items) 
                            (cons sep result))))))))

(define (difference ls1 ls2 #!key (test eqv?))
  (let loop ((items ls1)
             (result '()))
    (if (null? items)
        (reverse result)
        (if (contains? test ls2 (car items))
            (loop (cdr items) result)
            (loop (cdr items)(cons (car items) result))))))

(define (zip ls1 ls2)
  (map cons ls1 ls2))

(define (every? pred ls)
  (if (null? ls)
      #t
      (if (pred (car ls))
          (every? pred (cdr ls))
          #f)))

(define (reduce fn start-val vals)
  (if (null? vals)
      start-val
      (reduce fn (fn start-val (car vals))(cdr vals))))