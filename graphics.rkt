#lang racket
(require 2htdp/image "consts.rkt")
(provide syntax (all-defined-out))

(define (render arg)
  (define (proc ent scene)
    (overlay/xy (text(number->string (entity-type ent)) 20 "black")
                (* -1 (entity-x ent))
                (* -1 (entity-y ent))
                scene))
  (foldl proc (empty-scene WIDTH HEIGHT) (world-entities arg)))
