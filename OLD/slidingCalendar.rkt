#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require picturing-programs)

(define (move-right-10 picture)
  (beside (rectangle 10 0 "solid" "white")
          picture))

(big-bang pic:calendar
          (on-draw show-it 500 100)
          (on-tick move-right-10 1/22))

