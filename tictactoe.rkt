#lang racket/gui
;(require htdp/draw)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/math)
(require rsound)

(define (b1 x y)
  (cond [(and (>= x 33) (< x 332) (>= y 33) (< y 232)) (cons #t (list 10 10 300 300))]
        [else (cons #f '())]))

(define (b2 x y)
  (cond [(and (>= x 332) (< x 632) (>= y 33) (< y 232)) (cons #t (list 110 10 210 77))]
        [else (cons #f '())]))

(define (b3 x y)
  (cond [(and (>= x 632) (< x 930) (>= y 33) (< y 232)) (cons #t (list 210 10 310 77))]
        [else (cons #f '())]))

(define (b4 x y)
  (cond [(and (>= x 33) (< x 332) (>= y 232) (< y 434)) (cons #t (list 10 77 110 144))]
        [else (cons #f '())]))

(define (b5 x y)
  (cond [(and (>= x 332) (< x 632) (>= y 232) (< y 434)) (cons #t (list 110 77 210 144))]
        [else (cons #f '())]))

(define (b6 x y)
  (cond [(and (>= x 632) (< x 930) (>= y 232) (< y 434)) (cons #t (list 210 77 310 144))]
        [else (cons #f '())]))

(define (b7 x y)
  (cond [(and (>= x 33) (< x 332) (>= y 434) (< y 632)) (cons #t (list 10 144 110 211))]
        [else (cons #f '())]))

(define (b8 x y)
  (cond [(and (>= x 332) (< x 632) (>= y 434) (< y 632)) (cons #t (list 110 144 210 211))]
        [else (cons #f '())]))

(define (b9 x y)
  (cond [(and (>= x 632) (< x 930) (>= y 434) (< y 632)) (cons #t (list 210 144 310 211))]
        [else (cons #f '())]))
                                                   

(define xc 0)
(define yc 0)
(define my 0)
(define flag 0)
;(define x1 0)
;(define x2 0)
;(define y1 0)
;(define y2 0)
(define board (list 'e 'e 'e 'e 'e 'e 'e 'e 'e))
; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (cond
        [(and (= flag 1) (equal? (send event button-changed? 'left) #t))
             (set! flag 2)]
        [(and (= flag 2) (equal? (send event button-changed? 'left) #t))
         (begin
           (play ding)
           (sleep 0.5)
           (send msg1 set-label "Please start a new game!"))]
        [(and (= flag 0) (equal? (send event button-changed? 'left) #t))
      (begin
        ;(displayln "Here")
      (set! xc (send event get-x))
      (set! yc (send event get-y))
      (displayln board)
      (cond [(and (equal? (pos board 1) 'e) (equal? (car (b1 xc yc)) #t)) (begin ;(displayln "U")
                                          (define dc (send cv get-dc))
                                          ;(send dc draw-line 2 2 150 50)
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 10 10 110 77)
                                          (send dc draw-line 10 77 110 10)
                                          (set! board (change board 1 1 'p '())))]
            [(and (equal? (pos board 2) 'e) (equal? (car (b2 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 110 10 210 77)
                                          (send dc draw-line 110 77 210 10)
                                          (set! board (change board 1 2 'p '())))]
            [(and (equal? (pos board 3) 'e) (equal? (car (b3 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 210 10 310 77)
                                          (send dc draw-line 210 77 310 10)
                                          (set! board (change board 1 3 'p '())))]
            [(and (equal? (pos board 4) 'e) (equal? (car (b4 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 10 77 110 144)
                                          (send dc draw-line 10 144 110 77)
                                          (set! board (change board 1 4 'p '())))]
            [(and (equal? (pos board 5) 'e) (equal? (car (b5 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 110 77 210 144)
                                          (send dc draw-line 110 144 210 77)
                                          (set! board (change board 1 5 'p '())))]
            [(and (equal? (pos board 6) 'e) (equal? (car (b6 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 210 77 310 144)
                                          (send dc draw-line 210 144 310 77)
                                          (set! board (change board 1 6 'p '())))]
            [(and (equal? (pos board 7) 'e) (equal? (car (b7 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 10 144 110 211)
                                          (send dc draw-line 10 211 110 144)
                                          (set! board (change board 1 7 'p '())))]
            [(and (equal? (pos board 8) 'e) (equal? (car (b8 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 110 144 210 211)
                                          (send dc draw-line 110 211 210 144)
                                          (set! board (change board 1 8 'p '())))]
            [(and (equal? (pos board 9) 'e) (equal? (car (b9 xc yc)) #t)) (begin
                                          (define dc (send cv get-dc))
                                          (send dc set-pen "red" 1'solid)
                                          (send dc draw-line 210 144 310 211)
                                          (send dc draw-line 210 211 310 144)
                                          (set! board (change board 1 9 'p '())))]) 
      (cond [(win-check board 'p) (begin
                                    (send msg1 set-label "Player Won!")
                                    (set! flag 1)
                                    (play ding)
                                    (sleep 0.5)
                                    (play ding)
                                    (sleep 0.5)
                                    (play ding)
                                    )]
            [(noe board) (begin
                           (send msg1 set-label "Game draw!")
                           (set! flag 1)
                           (play ding)
                           (sleep 0.5)
                           (play ding)
                           (sleep 0.5)
                           (play ding)
                           )])
      (set! my (ttt board)) 
      (cond [(and (= flag 0) (> (count board 'p) (count board 'c))) 
      (cond [(= my 1) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 40 20 50 50)]
            [(= my 2) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 140 20 50 50)]
            [(= my 3) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 240 20 50 50)]
            [(= my 4) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 40 85 50 50)]
            [(= my 5) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 140 85 50 50)]
            [(= my 6) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 240 85 50 50)]
            [(= my 7) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 40 150 50 50)]
            [(= my 8) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 140 150 50 50)]
            [(= my 9) (define dc (send cv get-dc)) (send dc set-pen "blue" 1'solid) (send dc draw-ellipse 240 150 50 50)])
                                          
      (set! board (change board 1 my 'c '()))])
      (cond [(win-check board 'c) (begin
                                    (send msg1 set-label "Player Lost")
                                    (set! flag 1)
                                    (play ding)
                                    (sleep 0.5)
                                    (play ding)
                                    (sleep 0.5)
                                    (play ding)
                                    )]))]
      ))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg1 set-label "Keyboard movement"))
    (super-new)))

(define frame (new frame%                    
                   [label "Credits: 170100082"]
                   [width 1000]
                   [height 300]))

(define msg1 (new message% [parent frame]
                          [label "Game in progress ..."] 
                          [min-height 40]
                          ;[color "black"]
                          [font (make-object font% 30 'default 'normal 'bold)])) 

(define cv (new my-canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                 (send dc set-background (make-object color% "black"))
                ;(cond [(> a 600) (displayln "Yes")])  
                ;(send dc set-canvas-background "yellow")
                ;(send event get-x)
                ;(send msg set-label "Canvas mouse")
                ;(define blue-brush (new brush% [color "blue"])) 
                ;(send dc set-brush blue-brush)
                ;(send dc set-background (make-object color% "black"))
                ;(send dc set-background (make-object color% 220 200 255))
                ;(send blue-brush set-stipple (read-bitmap "water.jpeg"))
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc set-brush "gray"'solid)
                (send dc set-pen "black" 1'solid)
                
                (send dc draw-rectangle
                      10 10   ; Top-left at (0, 10), 10 pixels down from top-left
                      300 201) ; 30 pixels wide and 10 pixels high
                (send dc draw-line 110 10 110 211)
                (send dc draw-line 210 10 210 211)
                (send dc draw-line 10 77 310 77)
                (send dc draw-line 10 144 310 144)
     
                ;(send canvas set-canvas-background "yellow")
                ;(send dc draw-text "Hello World" 50 50)
                )]))

(send frame show #t)

(define (pos-h a loc i)
  ;(displayln i)
  ;(displayln a)
  (cond [(= i loc) (car a)]
        [else (pos-h (cdr a) loc (+ i 1))]))

(define (pos a loc)                          ;element at position of list
  (pos-h a loc 1))

(define (win-check a cand)                   ;winning condition
  (cond [(or (and (equal? (pos a 1) cand)
              (equal? (pos a 4) cand)
              (equal? (pos a 7) cand))
             (and (equal? (pos a 2) cand)
              (equal? (pos a 5) cand)
              (equal? (pos a 8) cand))
             (and (equal? (pos a 3) cand)
              (equal? (pos a 6) cand)
              (equal? (pos a 9) cand))
             (and (equal? (pos a 1) cand)
              (equal? (pos a 2) cand)
              (equal? (pos a 3) cand))
             (and (equal? (pos a 4) cand)
              (equal? (pos a 5) cand)
              (equal? (pos a 6) cand))
             (and (equal? (pos a 7) cand)
              (equal? (pos a 8) cand)
              (equal? (pos a 9) cand))
             (and (equal? (pos a 1) cand)
              (equal? (pos a 5) cand)
              (equal? (pos a 9) cand))
             (and (equal? (pos a 3) cand)
              (equal? (pos a 5) cand)
              (equal? (pos a 7) cand))) cand]
        [else #f]))

(define (change a i cur turn lnew)   ;alter the list based on someone's turn
  (cond [(> i 9) lnew]
        [(= i cur) (change (cdr a) (+ i 1) cur turn (append lnew (list turn)))]
        [else (change (cdr a) (+ i 1) cur turn (append lnew (list (car a))))]))

(define (addone a amod i lnew)     ;list of lists after adding single opponent move at each empty index
  (cond [(null? amod) lnew]
        [(equal? (car amod) 'e) (addone a (cdr amod) (+ i 1) (append lnew (list (change a 1 i 'p '()))))]
        [else (addone a (cdr amod) (+ i 1) lnew)]))

(define (noe a)          ;;no-empty in list
  (cond [(null? a) #t]
        [(equal? (car a) 'e) #f]
        [else (noe (cdr a))]))

(define (ver a)       ;;verify whether child moves are winning, losing or draw
  ;(displayln a)
  (cond [(null? a) (cons #t 'w)]
        [(equal? (win-check (car a) 'p) 'p) (cons #f 'l)]
        [(equal? (win-check (car a) 'c) 'c) (ver (cdr a))]
        [(equal? (noe a) #t) (cons #t 'd)] 
        [(equal? (car (cplay (car a) (car a) 1 0 (cons 0 0))) 'c) (ver (cdr a))]
        [else (cons #f 'l)]))

(define (cplay a suf i val tmp)    ;helper function for next move of computer
  ;(displayln i)
  (cond [(equal? (win-check a 'p) 'p) (cons 'p 0)]
        [(> i 9) (cond [(equal? tmp (cons 0 0)) (cons 'c val)]
                       [else tmp])]
        [(equal? (car suf) 'e) (begin
                                 (define x (change a 1 i 'c '()))
                                 (define z (change a 1 i 'p '()))
                                 ;(displayln "Rajat")
                                 ;(displayln z)
                                 ;(displayln (win-check z 'p))
                                 (cond [(equal? (win-check x 'c) 'c) (cons 'c i)]
                                       [(equal? (win-check z 'p) 'p) (begin
                                                                       (cplay a (cdr suf) (+ i 1) i (cons 'c i)))]
                                       [else (define y (addone x x 1 '()))
                                             ;(displayln (ver y))
                                             (cond [(equal? (ver y) (cons #t 'w)) (cons 'c i)]
                                                   [(equal? (ver y) (cons #t 'd)) (begin
                                                                                    (set! val i)
                                                                                    (cplay a (cdr suf) (+ i 1) i tmp))]
                                                   [else (cplay a (cdr suf) (+ i 1) val tmp)])]))]
        [else (cplay a (cdr suf) (+ i 1) val tmp)])) 

                                             
                                 
                                 
    
(define (ttt a)                 ;returns index of next move for computer
  (cond [(win-check a 'p) #f]
        [else (cdr (cplay a a 1 0 (cons 0 0)))]))

(define (count a ch)    ;count number of fixed character in list
  (cond [(null? a) 0]
        [(equal? (car a) ch) (+ 1 (count (cdr a) ch))]
        [else (count (cdr a) ch)]))

;(define t (list 'p 'e 'p 'p 'c 'p 'c 'e 'c))