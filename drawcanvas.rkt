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
(define (index-to-point a b c cx cy dim)
  (define edgelength (/ (* (- 3 a) dim) 2))
  (cons (+(- cx edgelength) (* b edgelength))
        (+ (- cy edgelength) (* c edgelength))))
  
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
(define (do-func xc yc)
  "hi")
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
            (do-func xc yc))
          #f))))
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
(define a 200)
(define r 10)
(define 2r (* 2 r))
(define 2a (* 2 a))
(define 3a (* 3 a))
(define 3a/2 (/ 3a 2))
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
  (send dc draw-rectangle a a a a)
  (send dc draw-rectangle (/ a 2) (/ a 2) 2a 2a)
  (send dc draw-rectangle 0 0 3a 3a)
  (send dc draw-line 0 3a/2 a 3a/2)
  (send dc draw-line 3a/2 0 3a/2 a)
  (send dc draw-line 2a 3a/2 3a 3a/2)
  (send dc draw-line 3a/2 2a 3a/2 3a)
  (send dc set-pen no-pen)
  (send dc set-brush no-brush)
  ;(send dc draw-ellipse (- a r) (- a r) 2r 2r)
  ;(send dc set-brush yellow-brush)
  ;(define bg (make-object bitmap% "images/board.jpg"))
  ;(send dc draw-bitmap bg 0 0)
  ;(send dc draw-ellipse (- a r) (- a r) 2r 2r)
  (define bm (make-object bitmap% 30 30))
  (send bm load-file "images/blue-30.png" 'png #f)
  (define yeloow (make-object color% "yellow"))
  (send dc set-background yeloow)
  (send dc set-brush white-brush)
  
  (send dc draw-rectangle (/ (- a 30) 2) (/ (- a 30) 2) 30 30)
  
  
  (send dc draw-bitmap bm a a))
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



