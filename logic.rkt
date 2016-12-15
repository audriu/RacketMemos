#lang racket
;All the game logic comes here
(require "consts.rkt")
(provide syntax (all-defined-out))

(define (step state)
  ;aplies the game logic here
  (define old-games-tate (world-entities state))
  (define new-game-sate (map (lambda(ent)(entity (+ 1(entity-x ent))(entity-y ent)(entity-type ent)))old-games-tate))
  (world (+ 1 (world-ticks state))new-game-sate))
