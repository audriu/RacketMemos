#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define (plot func)
  (struct world (ticks pic))
  (define (handle-on-tick w)
    (define t (world-ticks w))
    (define p (world-pic w))
    (define new-ticks (+ t 1))
    (define new-pic
      (overlay/xy (circle 1 "solid" "blue")
                  (- -70 (/ t 1))      ;x coordinate
                  (+(* -70 1) (func t));y coordinate (applies actual function).
                  p))
    (world new-ticks new-pic))
  (big-bang (world 0    (empty-scene 700 150))
            (on-tick    handle-on-tick)
            (to-draw    world-pic))
)

;below is one of samples what you can do with formulas.
(plot (lambda (t)(+ 5(* 40 (sin (/ t 10))))))