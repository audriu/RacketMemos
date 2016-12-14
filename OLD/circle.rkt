#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define pasaulis (empty-scene 100 100))

(define (trace-circle t)
  (place-image (circle 5 "solid" "blue")
                       (+ 50(* 40 (cos (/ t 100))))
                       (+ 50(* 40 (sin (/ t 100))))
                       pasaulis))


(define (render t)
  (text (number->string t) 12 "red"))

(big-bang 1000000
          (on-tick sub1 1/500)
          (to-draw trace-circle)
          (stop-when zero?)
          (record? true))

