#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require picturing-programs)

#(big-bang (overlay pic:calendar (rectangle 100 100 "solid" "white"))
          (on-tick rotate-cw 1/2)
          (on-draw show-it))

(define (move-right-10 picture)
  ; picture image
  (beside (rectangle 10 0 "solid" "white")
          picture)
  )

(big-bang pic:calendar
          (on-draw show-it 500 100)
          (on-tick move-right-10 1/22))

