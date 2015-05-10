#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(struct world (ticks pic))

(define pasaulis (empty-scene 1000 1000))

(define (render w)
  (world-pic w))

(define (handle-on-tick w)
  (define t (world-ticks w))
  (define p (world-pic w))
  (define new-ticks (+ t 1))
  (define new-pic (overlay/xy (text (number->string t) 12 "red")
                              (* -10 t) (* -10 t)
                              p))
  (world new-ticks new-pic))


(define (stop? w)
  (= (world-ticks w) 50))

(big-bang (world 0    pasaulis)
          (on-tick    handle-on-tick 1/5)
          (to-draw    render)
          (stop-when  stop?))
