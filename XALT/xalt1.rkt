#lang racket
(require rackunit)

;;5th exercise
(define (get-randomly ls)

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

;data for testing
;Also applying normalisation for the 6th excercise.
(define three-elements (transform-6th(list 'a(cons 'b 2)'c)))

;;empty initial list for random generation.
(define elements '())

;;generating 6000 random elements list
(define ok (do ((x 6000 (- x 1)))
             ((= x 0) elements)
             (set! elements
                   (cons (get-randomly
                          three-elements)
                         elements
                         ))))

;;counting all the elements in the list
(map (lambda(x)
       (cons x (count (lambda(y) (eq? x y)) elements)))
     three-elements)