#lang racket/gui
(require 2htdp/image)
(require htdp/draw)	
;(require 2htdp/universe)
(require racket/math)
;(require rsound)
;(draw-circle 50 100 'orange)
; Make a 300 x 300 frame
  (define frame (new frame% [label "Drawing Example"]
                            [width 600]
                            [height 600]))
  ; Make the drawing area
  (define canvas (new canvas% [parent frame]))
  ; Get the canvas's drawing context
  (define dc (send canvas get-dc))
  
  ; Make some pens and brushes
  (define no-pen (make-object pen% "BLACK" 1 'transparent))
  (define no-brush (make-object brush% "BLACK" 'transparent))
  (define blue-brush (make-object brush% "BLUE" 'solid))
  (define yellow-brush (make-object brush% "YELLOW" 'solid))
  (define red-pen (make-object pen% "RED" 2 'solid))
  (define yellow-pen (make-object pen% "YELLOW" 2 'solid))
  
  ; Define a procedure to draw a face
  (define (draw-face dc)
    (send dc set-pen red-pen)
    (send dc set-brush no-brush)
    (define a 200)
    (define r 10)
    (define 2r (* 2 r))
    (define 2a (* 2 a))
    (define 3a (* 3 a))
    (define 3a/2 (/ 3a 2))
    (send dc draw-rectangle a a a a)
    (send dc draw-rectangle (/ a 2) (/ a 2) 2a 2a)
    (send dc draw-rectangle 0 0 3a 3a)
    (send dc draw-line 0 3a/2 a 3a/2)
    (send dc draw-line 3a/2 0 3a/2 a)
    (send dc draw-line 2a 3a/2 3a 3a/2)
    (send dc draw-line 3a/2 2a 3a/2 3a)
    (send dc set-pen no-pen)
    (send dc set-brush blue-brush)
    (send dc draw-ellipse (- a r) (- a r) 2r 2r)
    (send dc set-brush yellow-brush)
    (send dc draw-ellipse (- a r) (- a r) 2r 2r))
  ; Show the frame
  (send frame show #t)
  ; Wait a second to let the window get ready
  (sleep/yield 1)
  ; Draw the face
  (draw-face dc)