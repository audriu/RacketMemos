#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(struct world (ticks pic))

(define (render w)
  (world-pic w))

(define (handle-on-tick w)
  
  (define t (world-ticks w))
  (define p (world-pic w))
  (define new-ticks (+ t 1))
  (define new-pic 
    (overlay/xy (circle 1 "solid" "blue")
                              ;(+(* -70 1) (+ 1(* 40 (cos (/ t 100))))) 
                (- -70 (/ t 1))
                              (+(* -70 1) (+ 5(* 40 (sin (/ t 10)))))
                              p))
  (world new-ticks new-pic))

(big-bang (world 0    (empty-scene 1500 1500))
          (on-tick    handle-on-tick)
          (to-draw    render))