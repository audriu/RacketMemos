#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x)(string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(= n 0)(error "list-nth-mod: negative number")]
        [(null? xs)(error "list-nth-mod: empty list")]
        [else (let([i (remainder n (length xs))])
                (define (get-ith ls ith)
                  (if (= 0 ith)
                      (car ls)
                      (get-ith (cdr ls)(- ith 1))))
                (get-ith xs i))]))

(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([res (s)])(cons (car res) (stream-for-n-steps (cdr res)(- n 1))))))

(define (funny-number-stream)
  (define (stream x)(cons (if [= (remainder x 5) 0](* x -1)x) (lambda ()(stream (+ x 1)))))
  (stream 1))

(define (dan-then-dog)
  (define (stream x)(cons x (lambda ()(stream (if (string=? x "dog.jpg")"dan.jpg" "dog.jpg")))))
  (stream "dan.jpg"))

(define (stream-add-zero s)
  (define (wrap stream)
    (lambda () (cons (cons 0(car (s)))(wrap (s)))))
  (wrap s))

(define (cycle-lists l1 l2)
  (define (stream x1 x2)
    (define list1 (if (null? x1)l1 x1))
    (define list2 (if (null? x2)l2 x2))
    (cons (cons (car list1)(car list2)) (lambda ()(stream (cdr list1) (cdr list2)))))
  (lambda()(stream l1 l2)))

(define-syntax while-less
  (syntax-rules ()
    [(while-less e1 do e2) (let ([my-e1 e1])
                             (define (inner-loop)(begin (define my-e2 e2)(if (< my-e1 my-e2)(inner-loop)#t)))
                             (inner-loop)
                             )]))