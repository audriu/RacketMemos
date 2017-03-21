#lang racket
(require rackunit "xalt2.rkt")

(define examples (list (weather "Day1" 'cloudy -5 60 'yes '-)
                       (weather "Day2" 'cloudy 0 30 'no '+)
                       (weather "Day3" 'cloudy 10 45 'no '+)
                       (weather "Day4" 'cloudy 20 60 'yes '-)
                       (weather "Day5" 'cloudy 30 80 'no '+)))

;;tests for Exercise 9
(define argument-for-add-example (weather "Day77" 'cloudy 39 87 'no '+))
(define expected-result (cons argument-for-add-example examples))
(check-equal? (add-example examples argument-for-add-example) expected-result "addind to list is not working?")

;;tests for Exercise 10
(check-equal? (attribute 'name examples) '("Day1" "Day2" "Day3" "Day4" "Day5") "")
(check-equal? (attribute 'perspective examples) '(cloudy cloudy cloudy cloudy cloudy) "")
(check-equal? (attribute 'temperature examples) '(-5 0 10 20 30) "")
(check-equal? (attribute 'humidity examples) '(60 30 45 60 80) "")
(check-equal? (attribute 'wind examples) '(yes no no yes no) "")
(check-equal? (attribute 'class examples) '(- + + - +) "")
