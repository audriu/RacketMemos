#lang racket
(provide syntax (all-defined-out))

(struct world (ticks entities))
(struct entity (x y type))
(define WIDTH 600)
(define HEIGHT 500)
