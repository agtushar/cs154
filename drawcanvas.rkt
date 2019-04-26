#lang racket/gui
(require 2htdp/image)
(require htdp/draw)
;(require 2htdp/universe)
(require racket/math)
;(require rsound)
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;utility functions


(define state (make-3d-vector 3 3 3 0))
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
                                                      (if (checkS bx 2) (set! wait 1) (begin (set! cP 1) (set! cnt (+ 1 cnt))))
                                                      (if (= cnt 9) (set! phase 1) #t))
                                                    (displayln "Not Correct Rectangle")))])]
        [(= wait 1) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (and (= clr 2) (not (checkS bx 2))) (begin (set! wait 0) (set! cP 2) (3vs state bx 0))
                                                    (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (and (= clr 1) (not (checkS bx 1))) (begin (set! wait 0) (set! cP 1) (3vs state bx 0)
                                                                                 (set! cnt (+ 1 cnt))
                                                                                 ((if (= cnt 9) (set! phase 1) #t)))
                                                    (displayln "Not Correct Rectangle")))])])
  )

 
(define (gen-all-pos)
  (define thepos '(0 1 2))
  (filter (lambda (x) (not (and (equal? (cadr x) 1) (equal? (caddr x) 1)))) (cartesian-product thepos thepos thepos)))

  
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

(define my-canvas%
  (class canvas%
   ; Define overriding method to handle mouse events
    (super-new)
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (let ((xc (send event get-x))
                (yc (send event get-y))
                (box (tFun xc yc)))
                (if (list? box) (st-trans box) (displayln "Click on rectangle region only")))
          (displayln "Click on Left")))))
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
(define a 100)
(define cX 250)
(define cY 250)
(define r 10)
(define 2r (* 2 r))
(define 2a (* 2 a))
(define 3a (* 3 a))
(define 3a/2 (/ 3a 2))
(define frame (new frame% [label "Drawing Example"]
                   [width 600]
                   [height 600]))
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
  (send dc draw-rectangle (- cX (/ a 2)) (- cY (/ a 2)) a a)
  (send dc draw-rectangle (- cX (/ 2a 2)) (- cY (/ 2a 2)) 2a 2a)
  (send dc draw-rectangle (- cX (/ 3a 2)) (- cY (/ 3a 2)) 3a 3a)
  (send dc draw-line (- cX (/ 3a 2)) cY (- cX (/ a 2)) cY)
  (send dc draw-line (+ cX (/ 3a 2)) cY (+ cX (/ a 2)) cY)
  (send dc draw-line cX (- cY (/ 3a 2)) cX (- cY (/ a 2)))
  (send dc draw-line cX (+ cY (/ 3a 2)) cX (+ cY (/ a 2)))
  (send dc set-pen no-pen)
  (send dc set-brush no-brush))
; Show the frame
(send frame show #t)
; Wait a second to let the window get ready
(sleep/yield 1)
; Draw the face
(draw-face dc)