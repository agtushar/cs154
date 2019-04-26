#lang racket/gui
(require 2htdp/image)
(require htdp/draw)
;(require 2htdp/universe)
(require racket/math)
;(require rsound)


;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;valid moves

(define phase 0)
(define wait 0)
(define cP 1)
(define cnt 0)

(define (getRV bx)
  (cond [(= 1 (cadr bx)) (map (lambda (x) (3vf state (list x (cadr bx) (caddr bx)))) (list 0 1 2))]
        [else (map (lambda (x) (3vf state (list (car bx) (cadr bx) x))) (list 0 1 2))])
  )

(define (getCV bx)
  (cond [(= 1 (caddr bx)) (map (lambda (x) (3vf state (list x (cadr bx) (caddr bx)))) (list 0 1 2))]
        [else (map (lambda (x) (3vf state (list (car bx) x (caddr bx)))) (list 0 1 2))])
  )

(define (checkS bx v)
  (let ((v1 (getRV bx))
        (v2 (getCV bx)))
    (if (or (equal? v1 (list v v v)) (equal? v2 (list v v v))) #t #f))
  )
(define (neigh bx v)
  (cond [(= (car bx) (car v)) (= 1 (+ (abs (- (cadr bx) (cadr v))) (abs (- (caddr bx) (caddr v)))))]
        [else (and (= (cadr bx) (cadr v)) (= (caddr bx) (caddr v)) (= 1 (abs (- (car bx) (car v))))
                   (or (= 2 (cadr bx)) (= 2 (caddr bx))))])
  )

(define (st-trans bx)
  (cond [(= phase 0) (putIt bx)]
        [(= phase 1) (moveIt bx)])
  )

(define pP (list 0 0 0))

(define (moveIt bx)
  (cond [(= wait 0) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (= clr 1) (begin (set! pP bx) (set! wait 1)) (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (= clr 2) (begin (set! pP bx) (set! wait 1)) (displayln "Not Correct Rectangle")))])]
        [(= wait 1) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (and (= clr 0) (neigh pP bx))
                                          (begin (3vs state pP 0) (3vs state bx 1)
                                                 (if (checkS bx 1) (set! wait 2) (begin (set! wait 0) (set! cP 2)))) (displayln "Not a Valid Move")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (and (= clr 0) (neigh pP bx))
                                          (begin (3vs state pP 0) (3vs state bx 2)
                                                 (if (checkS bx 2) (set! wait 2) (begin (set! wait 0) (set! cP 1)))) (displayln "Not a Valid Move")))])]
        [(= wait 2) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (and (= clr 2) (not (checkS bx 2))) (begin (set! wait 0) (set! cP 2) (3vs state bx 0))
                                          (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (and (= clr 1) (not (checkS bx 1))) (begin (set! wait 0) (set! cP 1) (3vs state bx 0))
                                          (displayln "Not Correct Rectangle")))])])
  )

(define (putIt bx)
  (cond [(= wait 0) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (= clr 0) (begin (3vs state bx 1)
                                                           (if (checkS bx 1) (set! wait 1) (set! cP 2)))
                                          (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (= clr 0) (begin (3vs state bx 2)
                                                           (if (checkS bx 2) (set! wait 1)
                                                               (begin (set! cP 1) (set! cnt (+ 1 cnt)) (if (= cnt 9) (set! phase 1) #t)))
                                                           )
                                          (displayln "Not Correct Rectangle")))])]
        [(= wait 1) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (and (= clr 2) (not (checkS bx 2))) (begin (set! wait 0) (set! cP 2) (3vs state bx 0))
                                          (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (and (= clr 1) (not (checkS bx 1))) (begin (set! wait 0) (set! cP 1) (3vs state bx 0)
                                                                                     (set! cnt (+ 1 cnt))
                                                                                     (if (= cnt 9) (set! phase 1) #t))
                                          (displayln "Not Correct Rectangle")))])])
  )


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


(define (3vf vec l)
  (3d-vector-ref vec (car l) (cadr l) (caddr l)))

(define (3d-vector-set! vec a b c val)
  (let* ((l (vector-ref vec a))
         (r (vector-ref l b)))
    (vector-set! r c val)))

(define (3vs vec l val)
  (3d-vector-set! vec (car l) (cadr l) (caddr l) val))

(define state (make-3d-vector 3 3 3 0))

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
