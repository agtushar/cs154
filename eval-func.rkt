#lang racket
(require "utils.rkt")

(define (eval-phase vec color phase)

  (define pos (filter (lambda (x) (equal? (3vf vec x) color)) (gen-all-pos)))
  
  (define (no-of-morris)  ;;has number of closed morris
    (define (morris? row)
      (foldl (lambda (index i) (and (= color (3vf vec index)) i)) #t row))
    (count (lambda (x) (morris? x)) all-lines))
  
  (define (blocked-opp-pieces) ;;number of blocked opp pieces
    (define oppcolor (get-opp color))
    (define opp-pos (filter (lambda (x) (equal? (3vf vec x) oppcolor)) (gen-all-pos)))
    (length (filter (lambda (x) (blocked? x vec)) opp-pos)))

  (define (twopiece? row)
      (= (foldl (lambda (index i) (+ i (let ([col-at (3vf vec index)])
                                      (cond [(= color col-at) 1]
                                            [(= col-at 0) 0]
                                            [else 50])))) 0 row) 2))
  
  (define twoliners ;;twoliners should not get computed in phase 1
  (if (not (equal? phase 1)) (filter (lambda (x) (twopiece? x)) all-lines)
      #t))

  (define (no-of-pieces);;number of pieces
    (length pos))

  (define (no-of-2-pieces) ;;number of 2 pieces configuration
    (count twoliners))

  (define (no-of-3-pieces) ;;number of 3 pieces configuration
    (/ (count (lambda (x) (not (null? (list-intersect (car x) (cdr x)))))
               (cartesian-product twoliners twoliners)) 2))
  
  (define (0-rel) 0)

  (define (closed-morris)
    (0-rel))

  (define (opened-morris)
    ...)

  (define (double-morris)
    ...)

  (define (win-conf)
    ...)
  
  (define c ;;coefficient list
    (cond ([(= phase 0) '(18 26 1 6 12 7 0)]
           [(= phase 1) '(14 43 10 8 7 42 1086)]
           [(= phase 2) '(10 1 16 1190 0 0 0)])))
  
  (define r ;;relation list
    (cond ([(= phase 0) '(closed-morris no-of-morris blocked-opp-pieces no-of-pieces no-of-2-pieces no-of-3-pieces 0-rel)]
           [(= phase 1) '(closed-morris no-of-morris blocked-opp-pieces no-of-pieces opened-morris double-morris win-conf)]
           [(= phase 2) '(no-of-2-pieces no-of-3-pieces 0-rel win-conf 0-rel 0-rel 0-rel)])))
  (foldr (lambda (x i) (+ i (* (list-ref c i) ((list-ref r i))))) 0 (build-list 7 (lambda (x) x))))
    
    
  
  







;;utility functions

(define (gen-box-lines)
  (define ans '())
  (for ([i '(0 1 2)])
        (for ([j '(0 2)])
          (set! ans (append ans (list (list (list i j 0) (list i j 1) (list i j 2))
                                (list (list i 0 j) (list i 1 j) (list i 2 j)))))))
  ans)

(define (gen-cross-lines)
  (define ans '())
  (for ([i '(0 2)])
    (set! ans (append ans (list (list (list 0 i 1) (list 1 i 1) (list 2 i 1))
                                (list (list 0 1 i) (list 1 1 i) (list 2 1 i))))))
  ans)

(define all-lines
  (append (gen-box-lines) (gen-cross-lines)))
  
(define (get-opp color)
  color)

(define (blocked? pos vec)
  (define oppcolor (get-opp (3vf vec pos)))
  (define row (getRV pos))
  (define col (getCV pos))
  (define neighpos (filter (lambda (x) (neigh pos x)) (gen-all-pos)))
  (foldl (lambda (x i) (and (= (3vf vec x) oppcolor) i)) #t neighpos))

(define (list-intersect lst1 lst2)
  (set->list
   (set-intersect (list->set lst1)
                  (list->set lst2))))
  
  
    