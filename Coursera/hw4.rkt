#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (λ (xs)(string-append xs suffix)) mp))

(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: negative number")]
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
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (define (stream x)(cons (if [= (remainder x 5) 0](* x -1)x) (lambda ()(stream (+ x 1)))))
  (stream 1))

(define (dan-then-dog)
  (define (dan)(cons "dan.jpg" dog))
  (define (dog)(cons "dog.jpg" dan))
  (dan))

(define (stream-add-zero s)
  (define (wr s)
    (cons (cons 0 (car (s))) (lambda ()(wr (cdr(s))))))
  (lambda () (wr s)))

(define (cycle-lists xs ys)
  (define (aux x)(cons (cons (list-nth-mod xs x)(list-nth-mod ys x))(lambda ()(aux (+ 1 x)))))
  (lambda ()(aux 0)))

(define (vector-assoc var vec)
  (define len (vector-length vec))
  (define (traverse pos)
    (if (> pos len)
        #f
        (if (= var (car(vector-ref vec pos)))
            (vector-ref vec pos)
            (traverse (+ 1 pos)))))
  (traverse 0))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([f1 e1])
       (define (lop)
         (if (>= f1 e2)
             (lop)
             #t))
       (lop))]))
