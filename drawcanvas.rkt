#lang racket/gui
(require 2htdp/image)
(require htdp/draw)	
;(require 2htdp/universe)
(require racket/math)
;(require rsound)
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;utility functions
(define (gen-all-pos)
  (define thepos '(0 1 2))
  (filter (lambda (x) (not (and (equal? (cadr x) 1) (equal? (caddr x) 1)))) (cartesian-product thepos thepos thepos)))

(define (index-to-point index)  ;;index is list of indices
  (define edgelength (/ (* (- 3 (car index)) a) 2))
  (cons (+ (* (car index) a/2) (* (caddr index) edgelength))
        (+ (* (car index) a/2) (* (cadr index) edgelength))))
  
(define (make-3d-vector a b c initial)
  (build-vector a (lambda (x) (make-2d-vector b c initial))))
(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))
(define (3d-vector-ref vec a b c)
  (vector-ref (vector-ref (vector-ref vec a) b) c))
(define (3d-vector-set! vec a b c val)
  (let* ((l (vector-ref vec a))
         (r (vector-ref l b)))
    (vector-set! r c val)))


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
  (define closest-index (filter (lambda (z) (in-vicinity? x y z)) (gen-all-pos)))
  (if (null? closest-index) #f (car closest-index)))

(define (recolor-state vec)
  (map (lambda (x) (begin
                     (define col (3d-vector-ref vec (car x) (cadr x) (caddr x)))
                     (define point (index-to-point x))
                     (drawball (car point) (cdr point) col)))
       (gen-all-pos)))

(define (line x1 y1 x2 y2)
  (send dc draw-line (+ lx x1) (+ ly y1) (+ lx x2) (+ lx y2)))
(define (rect a b c d)
  (send dc draw-rectangle (+ lx a) (+ ly b) c d))

;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
(define my-canvas%
  (class canvas%
    (define ball-pos (make-3d-vector 3 3 3 0))
    ; Define overriding method to handle mouse events
    (super-new)
    (define/override (on-event event)
      (define xc 0)
      (define yc 0)
      (if (equal? (send event button-changed? 'left) #t)
          (begin
            (set! xc (send event get-x))
            (set! yc (send event get-y))
            (get-closest xc yc))
          #f))))
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
(define a 200)
(define r 10)
(define 2r (* 2 r))
(define a/2 (/ a 2))
(define 2a (* 2 a))
(define 3a (* 3 a))
(define 3a/2 (/ 3a 2))
(define lx (- 300 a))
(define ly (- 300 a))
(define frame (new frame% [label "Drawing Example"]
                   [width 3a]
                   [height 3a]))
; Make the drawing area
(define canvas (new my-canvas% [parent frame]))
; Get the canvas's drawing context
(define dc (send canvas get-dc))
  
; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define white-brush (make-object brush% "WHITE" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define yellow-pen (make-object pen% "YELLOW" 2 'solid))

; Define a procedure to draw a face
(define (draw-face dc)
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
  (define vec (make-3d-vector 3 3 3 0))
  (drawball a a 2)
  (recolor-state vec))
; Show the frame
(send frame show #t)
; Wait a second to let the window get ready
(sleep/yield 1)
; Draw the face
(draw-face dc)


;;junk---------------------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------

;(map (lambda (x)
;      (let ([coor (index-to-point (car x) (cadr x) (caddr x) 3a/2 3a/2 a)])
;                   (send dc draw-bitmap bm (car coor) (cdr coor)))) (gen-all-pos)) ;;insert image at every pos



