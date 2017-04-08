#lang racket
(provide (all-defined-out))

;;5th exercise
(define (get-randomly5 ls)

  ;function that gets all the probability sums of the list
  (define (sum-weights l)
    (if (null? l)
        0
        (+ (cdr(car l))(sum-weights(cdr l)))))

  ;just get the random location from the expanded probabitity field
  (define random-location (random(sum-weights ls)))

  ;function that gets the actual random element according to relative probabilities
  (define (get-from-list-by-weight l n)
    (if (> (+ n (cdr(car l))) random-location)
        (car l)
        (get-from-list-by-weight (cdr l)(+ n (cdr(car l))))))

  ;call that function.
  (get-from-list-by-weight ls 0)
)

;list transformation for the 6th exercise
(define (transform-6th l)
    (cond [(null? l)null]
          [(pair? (car l))(cons (car l)(transform-6th (cdr l)))]
          [else (cons (cons(car l)1)(transform-6th (cdr l)))]))

;;6th exercise
(define (get-randomly6 ls)
  (get-randomly5(transform-6th ls)))
