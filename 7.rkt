#lang racket
(require 2htdp/universe
         2htdp/image)

(define gx 0)
(define gy 0.35)

(struct ballstate (x y vx vy) #:transparent)

(define startstate (ballstate 10 590 7 -20))

(define (make-new-state old)
  (define newvx (+ (ballstate-vx old) gx))
  (define newvy (+ (ballstate-vy old) gy))
  (ballstate (+ (ballstate-x old) newvx)
             (+ (ballstate-y old) newvy)
             newvx
             newvy))


(define (main)
  (big-bang startstate
            [on-tick make-new-state]
            [to-draw place-ball-at]
            [on-key reset]))

(define (place-ball-at s)
  (place-image (circle 10 "solid" "red")
               (ballstate-x s)
               (ballstate-y s)
               (empty-scene 800 600)))

(define (reset s ke)
  startstate)

(main)