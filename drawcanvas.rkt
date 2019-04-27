#lang racket/gui
(require 2htdp/image)
(require htdp/draw)
;(require 2htdp/universe)
(require racket/math)
(require "utils.rkt")
;(require rsound)


;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;valid moves



;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;utility functions

(define (drawball x y color)
  (define colorstring
    (cond [(= color 0) "RED"]
          [(= color 1) "BLUE"]
          [(= color 2) "YELLOW"]
          [else "BLACK"]))
  (define colorbrush (make-object brush% colorstring 'solid))
  (send dc set-brush colorbrush)
  (send dc draw-ellipse (+ (- x r) lx) (+ (- y r) ly) 2r 2r))

(define (in-vicinity? x y index)
  (define d 2r)
  (define point (index-to-point index))
  (define x1 (+ lx (car point)))
  (define y1 (+ ly (cdr point)))
  (and (< (abs (- x x1)) d) (< (abs (- y y1)) d)))


(define (get-closest x y)
  (define closest-index (filter (lambda (z) (in-vicinity? x y z)) all-pos))
  (if (null? closest-index) #f (car closest-index)))

(define (recolor-state vec)
  (map (lambda (x) (begin
                     (define col (3d-vector-ref vec (car x) (cadr x) (caddr x)))
                     (define point (index-to-point x))
                     (drawball (car point) (cdr point) col)))
       all-pos))


(define (index-to-point index)  ;;index is list of indices
	  (define edgelength (/ (* (- 3 (car index)) a) 2))
	  (cons (+ (* (car index) a/2) (* (caddr index) edgelength))
	        (+ (* (car index) a/2) (* (cadr index) edgelength))))

(define (line x1 y1 x2 y2)
  (send dc draw-line (+ lx x1) (+ ly y1) (+ lx x2) (+ lx y2)))
(define (rect a b c d)
  (send dc draw-rectangle (+ lx a) (+ ly b) c d))

(define a 150)
(define r (/ a 10))
(define 2r (* 2 r))
(define a/2 (/ a 2))
(define 2a (* 2 a))
(define 3a (* 3 a))
(define 3a/2 (/ 3a 2))
(define windowsize (* 5 a))
(define lx (/ (- windowsize 3a) 2))
(define ly (/ (- windowsize 3a) 2))
(define frame (new frame% [label "Drawing Example"]
                   [width windowsize]
                   [height windowsize]))
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------

(define (st-trans bx)
  (cond [(= phase 0) (putIt bx)]
        [(= phase 1) (moveIt bx)])
  )

(define my-canvas%
  (class canvas%
    ; Define overriding method to handle mouse events
    (super-new)
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (let* ((xc (send event get-x))
                 (yc (send event get-y))
                 (box (get-closest xc yc)))
            (if (list? box) (begin (st-trans box) (recolor-state state)) (displayln "Click on rectangle region only")))
          (displayln "Click on Left")))))
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;constants

;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
; Make the drawing area
(define canvas (new my-canvas% [parent frame]))
; Get the canvas's drawing context
(define dc (send canvas get-dc))
  
; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
;(define blue-brush (make-object brush% "BLUE" 'solid))
;(define yellow-brush (make-object brush% "YELLOW" 'solid))
;(define white-brush (make-object brush% "WHITE" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
;(define yellow-pen (make-object pen% "YELLOW" 2 'solid))

(define (draw-grid dc)
  (send dc set-pen red-pen)
  (send dc set-brush no-brush)
  (rect a a a a)
  (rect a/2 a/2 2a 2a)
  (rect 0 0 3a 3a)
  (line 0 3a/2 a 3a/2)
  (line 3a/2 0 3a/2 a)
  (line 2a 3a/2 3a 3a/2)
  (line 3a/2 2a 3a/2 3a)
  (send dc set-pen no-pen)
  (send dc set-brush no-brush)
  (recolor-state state)
  'ok)

(send frame show #t)
(sleep/yield 1)
(draw-grid dc)
