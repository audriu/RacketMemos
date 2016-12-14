#lang racket
(require 2htdp/universe 2htdp/image)

(struct world (ticks entities))
(struct entity (x y type))
(define width 600)
(define height 500)

(define initial (cons(entity 0 20 7)(cons (entity 0 50 5)null)))

(define (step state)
  ;aplies the game logic here
  (define old-games-tate (world-entities state))
  (define new-game-sate (map (lambda(ent)(entity (+ 1(entity-x ent))(entity-y ent)(entity-type ent)))old-games-tate))
  (world (+ 1 (world-ticks state))new-game-sate))

(define (render arg)
  (define (proc ent scene)
    (overlay/xy (text(number->string (entity-type ent)) 20 "black")
                (* -1 (entity-x ent))
                (* -1 (entity-y ent))
                scene))
(foldl proc (empty-scene width height) (world-entities arg)))

(big-bang (world 0 initial)
          (on-tick step)
          (to-draw render))
