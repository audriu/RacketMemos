#lang racket
(require 2htdp/universe 2htdp/image "consts.rkt" "logic.rkt" "graphics.rkt" "initial.rkt")

(big-bang (world 0 initial-entities)
          (on-tick step)
          (to-draw render))
