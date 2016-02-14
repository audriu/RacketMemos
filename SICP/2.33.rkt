#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map op sequence)
  (accumulate (lambda [x y](cons (op x) y)) '() sequence))

(define (append a1 a2)
  (accumulate cons a2 a1))

(define (length sequence)
  (accumulate (lambda [x y] (+ 1 y)) 0 sequence))
  