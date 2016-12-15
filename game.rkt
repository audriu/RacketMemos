#lang racket
(require 2htdp/universe 2htdp/image "consts.rkt" "logic.rkt" "graphics.rkt")

(define initial (cons(entity 0 20 7)(cons (entity 0 50 5)null)))

(big-bang (world 0 initial)
          (on-tick step)
          (to-draw render))
