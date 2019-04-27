#lang racket/gui
(require 2htdp/image)
(require htdp/draw)
;(require 2htdp/universe)
(require racket/math)
(require "utils.rkt")
(require "tictactoe.rkt")
(require "minimaxAlgo.rkt")
;(require rsound)


;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;valid moves

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


;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;utility functions
(define (st-trans bx)
    (cond [(= phase 0) (putIt bx)]
          [(= phase 1) (moveIt bx)])
      )
(define (st-trans2 bx)
  (cond [(= phase 0) (begin (putIt bx) (recolor-state state) (if (= cP 1) (let* ((nxtMoveP (mini-max state 1 2 0 cnt -10000000 10000000))
                                             (nxtMove (cdr nxtMoveP)))
                                         (cond [(= 4 (length nxtMove)) (begin (putIt (car nxtMove)) (putIt (cdr nxtMove)))]
                                               [else (putIt nxtMove)])) #t))]
        [(= phase 1) (begin (moveIt bx) (recolor-state state) (if (= cP 1) (let* ((nxtMoveP (mini-max state 1 2 1 cnt -10000000 10000000))
                                             (nxtMove1 (cadr nxtMoveP))
                                             (nxtMove2 (cddr nxtMoveP)))
                                         (cond [(= 4 (length nxtMove1)) (begin (moveIt (car nxtMove1)) (moveIt (cdr nxtMove1)) (moveIt nxtMove2))]
                                               [else (moveIt nxtMove1) (moveIt nxtMove2)])) #t))])
  )

(define (st-trans1 bx)
  (cond [(= phase 0) (begin (putIt bx) (recolor-state state) (if (= cP 2) (let* ((nxtMoveP (mini-max state 2 2 0 cnt -10000000 10000000))
                                             (nxtMove (cdr nxtMoveP)))
                                         (cond [(= 4 (length nxtMove)) (begin (putIt (car nxtMove)) (putIt (cdr nxtMove)))]
                                               [else (putIt nxtMove)])) #t))]
        [(= phase 1) (begin (moveIt bx) (recolor-state state) (if (= cP 2) (let* ((nxtMoveP (mini-max state 2 2 1 cnt -10000000 10000000))
                                             (nxtMove1 (cadr nxtMoveP))
                                             (nxtMove2 (cddr nxtMoveP)))
                                         (cond [(= 4 (length nxtMove1)) (begin (moveIt (car nxtMove1)) (moveIt (cdr nxtMove1)) (moveIt nxtMove2))]
                                               [else (moveIt nxtMove1) (moveIt nxtMove2)])) #t))])
  )
(define frame00 (new frame% [label "9-Men-Morris window1"] 
                   [width windowsize]
                   [height windowsize]))
(define frame11 (new frame% [label "9-Men-Morris window2"] 
                   [width windowsize]
                   [height windowsize]))
(define frame22 (new frame% [label "9-Men-Morris window3"] 
                   [width windowsize]
                   [height windowsize]))

(define frame1 (new frame% [label "Choice window1"] 
                   [width 300]
                   [height 200]))

(define frame2 (new frame% [label "Choice window2"] 
                   [width 300]
                   [height 200]))
;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------

(define my-canvas1%
  (class canvas%
    ; Define overriding method to handle mouse events
    (super-new)
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (let* ((xc (send event get-x))
                 (yc (send event get-y))
                 (box (get-closest xc yc)))
            (if (list? box) (begin (st-trans2 box) (recolor-state state)) (displayln "Click on rectangle region only")))
          (display "")
          ))))
(define my-canvas2%
  (class canvas%
    ; Define overriding method to handle mouse events
    (super-new)
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (let* ((xc (send event get-x))
                 (yc (send event get-y))
                 (box (get-closest xc yc)))
            (if (list? box) (begin (st-trans1 box) (recolor-state state)) (displayln "Click on rectangle region only")))
          (display "")
          ))))
(define my-canvas3%
  (class canvas%
    ; Define overriding method to handle mouse events
    (super-new)
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (let* ((xc (send event get-x))
                 (yc (send event get-y))
                 (box (get-closest xc yc)))
            (if (list? box) (begin (st-trans box) (recolor-state state)) (displayln "Click on rectangle region only")))
          (display "")
          ))))

;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
;;constants

;;---------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------
; Make the drawing area
(define canvas1 (new my-canvas1% [parent frame00])) 
; Get the canvas's drawing context
(define dc (send canvas1 get-dc))
(define dc1 (send canvas1 get-dc))
  
(define canvas2 (new my-canvas2% [parent frame11])) 
; Get the canvas's drawing context
(define dc2 (send canvas2 get-dc))

(define canvas3 (new my-canvas3% [parent frame22])) 
; Get the canvas's drawing context
(define dc3 (send canvas3 get-dc))

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

(define game-type 0)
(define game 'u)
(define drawn1 0)
(define drawn2 0)
(send frame1 show #t)

(define myfont (make-object font% 15 'decorative
            'slant))

;(define font1
;  (new font%
;    [style 'slant]   
;               ))
(define (rbox2) (new radio-box%	 
   	 	[label "How do you want to play?"]	 
   	 	[choices (list "No choice" "cpu player1" "cpu player2" "2 player")]	 
   	 	[parent frame2]
                [font myfont] 
                [min-width 100]
                [min-height 100]
   	 	[callback (lambda (b e) (cond [(= drawn2 0) (begin
                                                              (set! drawn2 1)
                                          (define button (send b get-selection))
                                          ;(displayln "Disco") 
                                          (cond [(= button 1) (begin (stCp 2) (3vs state '(0 0 0) 1) (set! game-type 1))]
                                                [(= button 2) (set! game-type 2)] 
                                                [(= button 3) (set! game-type 3)])
                                          (send frame2 show #f)
                                          ;(send frame00 show #t)
                                          ;(draw-grid dc1)
                                          (displayln "T")
                                          (cond [(= game-type 1) (begin (send frame00 show #t)
                                                                        (set! dc dc1)
                                                                        (sleep/yield 0.1)
                                                                        (displayln "1p")
                                                                        (displayln (draw-grid dc1))
                                                                        )]) 
                                          (cond [(= game-type 2) (begin (send frame11 show #t)
                                                                        (sleep/yield 0.1)
                                                                        (set! dc dc2)
                                                                        (displayln (draw-grid dc2))
                                                                        ;(displayln (draw-grid dc2))
                                                                        ;(displayln (draw-grid dc2))
                                                                        (sleep/yield 2)
                                                                        )])
                                          (cond [(= game-type 3) (begin (send frame22 show #t)
                                                                        (sleep/yield 0.1)
                                                                        (displayln "3p")
                                                                        (set! dc dc3)
                                                                        (displayln (draw-grid dc3))
                                                                        (displayln "3p1")
                                                                        )]))
                                          (sleep/yield 1)
                                          ;(displayln "drawn")
                                          ;(draw-grid dc)
                                          (set! drawn2 1)])
                                          )] 
                ))

(define (rbox1) (new radio-box%	 
   	 	[label "Which game?"]	 
   	 	[choices (list "No choice" "Tic-Tac-Toe" "9-Men-Morris")]	 
   	 	[parent frame1]
                [font myfont] 
                [min-width 100]
                [min-height 100]
   	 	[callback (lambda (b e) (cond [(= drawn1 0) (begin
                                                              (set! drawn1 1)
                                          (define button (send b get-selection))
                                          ;(displayln (send b get-font)) 
                                          (cond [(= button 1) (set! game 'ttt)] 
                                                [(= button 2) (set! game 'nmm)])
                                          (send frame1 show #f)
                                          (sleep/yield 2)
                                          (cond [(equal? game 'nmm) (begin (send frame2 show #t)
                                                                      (sleep/yield 0.1)
                                                                      (rbox2)
                                                                      (sleep/yield 1))]
                                                [else (begin (send frame3 show #t) (sleep/yield 1))])
                                          ;(rbox2)
                                          ;(sleep/yield 1)
                                          ;(displayln "1st")
                                          ;(draw-grid dc)
                                          ;(displayln "drawn")
                                          ;(draw-grid dc)
                                          (set! drawn1 1)
                                          )])
                                          )]
                ))	 
;(send an-area-container delete-child child)
;(send frame1 show #t)
(rbox1)
(displayln "Outside")
;(sleep/yield 5)
;(send frame1 show #f)
;(send frame show #t)
;(draw-grid dc)
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