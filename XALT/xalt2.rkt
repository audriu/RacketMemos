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

;; get-attribute-function : Symbol -> (Weather -> Any)
;; http://stackoverflow.com/questions/42875234/racket-strange-require-semantics
(define (get-attribute attr)
  (case attr
    [(name) weather-name]
    [(perspective) weather-perspective]
    [(temperature) weather-temperature]
    [(humidity) weather-humidity]
    [(wind) weather-wind]
    [(class) weather-class]))

;;Exercise 10
(define (attribute which examples)
  (define command (get-attribute which))
  (if (null? examples) null
      (cons (command (car examples)) (attribute which (cdr examples)))))