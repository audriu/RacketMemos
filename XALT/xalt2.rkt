#lang racket
(provide (all-defined-out))
(require (planet williams/describe/describe))

(struct weather (name perspective temperature humidity wind class) #:transparent)

;;auxiliary function for converting string to "weather struct"
(define (string->weather str)
  (define words(regexp-split " " str))
  (weather
   (list-ref words 0)
   (string->symbol(list-ref words 1))
   (string->number(list-ref words 2))
   (string->number(list-ref words 3))
   (string->symbol(list-ref words 4))
   (string->symbol(list-ref words 5))))

;;Exercise 8
(define (read-examples file-name)
  (map string->weather (filter (lambda (str)(not(string-prefix? str "#")))(file->lines file-name))))

(define examples (read-examples "examples.txt"))

;;Exercise 9
(define (add-example examples example)
  (cons example examples))

;; get-attribute-function : Symbol -> (Weather -> Any)
;; http://stackoverflow.com/questions/42875234/racket-strange-require-semantics
(define (get-attribute attr)
  ;;https://docs.racket-lang.org/reference/case.html
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

;;Exercise 11
(define (mix list1 list2)
  ;;mixes 2 lists assumuming equal sizes.
  (define (get-randomly x y)
    (if (= 1 (random 2)) x y))
  (if (empty? list1)null
      (cons (get-randomly(car list1)(car list2)) (mix (cdr list1)(cdr list2)))))

;;Exercise 12
(define (separate rate list)
  (define num-needed (exact-round(* rate(length list))))
  (take list num-needed))

;;Exercise 13
(define (folds num arg)
  (if (empty? arg) null
      (if (> num (length arg)) (list arg)
          (cons (take arg num)(folds num (drop arg num))))))

;;Exercise 14
;todo write tests
(define stratify folds)
