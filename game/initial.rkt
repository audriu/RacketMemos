#lang racket
(require "consts.rkt")
(provide syntax (all-defined-out))

(define initial-entities (cons(entity 0 20 7)(cons (entity 0 50 5)(cons (entity 0 190 2)null))))

(define initial-terain null)