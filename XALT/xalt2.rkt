#lang racket
(provide (all-defined-out))

(struct weather (name perspective temperature humidity wind class) #:transparent)

;;Exercise 8
;;Todo implement
#;(define (read-examples file-name)
  (file->lines file-name))

;;todo uncomment
#;(define examples (read-examples "examples.txt"))

;;Exercise 9
(define (add-example examples example)
  (cons example examples))

;;Exercise 10
(define (attribute which examples)
  (define command (eval (string->symbol(string-append "weather-" (symbol->string which)))))
  (if (null? examples) null
      (cons (command (car examples)) (attribute which (cdr examples)))))

