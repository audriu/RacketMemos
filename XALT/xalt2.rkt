#lang racket
;(require 2htdp/batch-io)

(struct weather (name perspective temperature humidity wind class) #:transparent)

;;Exercise 8
#;(define (read-examples file-name)
  (file->lines file-name))

#;(define examples (read-examples "examples.txt"))

(define examples (list (weather "Day1" 'cloudy -5 60 'yes '-)
                       (weather "Day2" 'cloudy 0 30 'no '+)
                       (weather "Day3" 'cloudy 10 45 'no '+)
                       (weather "Day4" 'cloudy 20 60 'yes '-)
                       (weather "Day5" 'cloudy 30 80 'no '+)))

;;Exercise 9
(define (add-example examples example)
  (cons example examples))

;;Exercise 10
(define (attribute whitch examples)
  (define command (eval (string->symbol(string-append "weather-" whitch))))
  (if (null? examples) null
      (cons (command (car examples)) (attribute whitch (cdr examples)))))

