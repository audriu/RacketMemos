#lang racket
(define zero_ 'zero)
(define [add-one number] (lambda []number))
(define [take-one number](number))

(define [one](lambda [] 'zero))
(define [two](lambda [] one))