#lang racket
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-to-set set elem)
  (lambda (elem-to)(if (= elem-to elem)#t
                       (if (null? set)#f
                           (set elem-to)))))

(define (remove-from-set set elem)
  (lambda (elem-to)(if (= elem-to elem)#f
                       (if (null? set)#f
                           (set elem-to)))))

(define (merge-sets set1 set2)
  (lambda (elem)
    (cond [(set1 elem)#t]
          [(set2 elem)#t]
          [else #f])))

(define set1 (add-to-set(add-to-set null 1)2))
(define set2 (add-to-set(add-to-set null 5)3))