#lang racket
(require rackunit "xalt1.rkt")

(define test-list-1 (list (cons 'a 2) (cons 'b 3) (cons 'c 4) (cons 'd 7)))
(define test-list-2 '((a . 1) (b . 2) (c . 3)))

;Exercise 5:
;Build the “get-randomly” function where the input is a list (the list is built by cons, see section 6.3.2 of the [R5RS] document).
;The function must return randomly one of the elements of the list (that will be the car) and the cdr will be the relative frequency to get the car of that element.
(get-randomly5 test-list-1)
(get-randomly5 test-list-2)
;ass we can observe - those returns one of the list.


;///////////////////////////////////////////////////////////////////////////////


;Exercise 6:
;Improve the “get-randomly” function in a way that it is able to get as input a single list of elements.
;When the input is given in this way all the elements will have the same relative frequency.
;The function will check if the elements of the list are cons or not. If they are conses if will follow in the way described in the previous exercise.
;If they are not conses then the list will be transformed into a list of conses where the cdr will be the same (showing they will have the same relative frequency),
;the easiest way will be to set the cdr to “1”.

(get-randomly6 test-list-1)
(get-randomly6 '(1 2 3))

;data for testing
;Also applying normalisation for the 6th excercise.
(define three-elements (transform-6th(list 'a(cons 'b 2)'c)))

;;empty initial list for random generation.
(define elements '())

;;generating 6000 random elements list
(define ok (do ((x 6000 (- x 1)))
             ((= x 0) elements)
             (set! elements
                   (cons (get-randomly5
                          three-elements)
                         elements
                         ))))

;;counting all the elements in the list
(map (lambda(x)
       (cons x (count (lambda(y) (eq? x y)) elements)))
     three-elements)
