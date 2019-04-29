#lang racket
(provide (all-defined-out))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))


(define all-list (lc (list x y z) : x <- '(0 1 2) y <- '(0 1 2) z <- '(0 1 2)))

(define allowed-bxs (filter (lambda (x) (not (and (= 1 (cadr x)) (= 1 (caddr x)))))
                                        all-list))

(define (max1 a . args)
  (cond [(null? args) (cons -100000000000000 '())]
        [else (let ((vl (apply max1 args))) (if (> (car a) (car vl)) a vl))])
  )

(define (min1 a . args)
  (cond [(null? args) (cons 100000000000000 '())]
        [else (let ((vl (apply min1 args))) (if (< (car a) (car vl)) a vl))])
  )

(define (vec-copy a b)
  (map (λ(x) (begin (3vs a x (3vf b x)) #t)) all-list)
  )

(define (getRV st bx)
  (cond [(= 1 (cadr bx)) (map (lambda (x) (3vf st (list x (cadr bx) (caddr bx)))) (list 0 1 2))]
        [else (map (lambda (x) (3vf st (list (car bx) (cadr bx) x))) (list 0 1 2))])
  )

(define (getCV st bx)
  (cond [(= 1 (caddr bx)) (map (lambda (x) (3vf st (list x (cadr bx) (caddr bx)))) (list 0 1 2))]
        [else (map (lambda (x) (3vf st (list (car bx) x (caddr bx)))) (list 0 1 2))])
  )

(define (checkS st bx v)
  (let ((v1 (getRV st bx))
        (v2 (getCV st bx)))
    (if (or (equal? v1 (list v v v)) (equal? v2 (list v v v))) #t #f))
  )
(define (neigh bx v)
  (cond [(equal? (car bx) (car v)) (= 1 (+ (abs (- (cadr bx) (cadr v))) (abs (- (caddr bx) (caddr v)))))]
        [else (and (= (cadr bx) (cadr v)) (= (caddr bx) (caddr v)) (= 1 (abs (- (car bx) (car v))))
                   (or (= 1 (cadr bx)) (= 1 (caddr bx))))])
  )

(define (all-in-mill st player)
  (let* ((pl-posit (filter (λ(x) (= player (3vf st x))) allowed-bxs))) (andmap (λ(x) (checkS st x player)) pl-posit))
  )


(define neigh-pairs (filter (λ(x) (neigh (car x) (cdr x))) (lc (cons x y) : x <- all-list y <- all-list)))

(define phase 0)
(define wait 0)
(define cP 1)
(define cnt 0)

(define (stCp v)
  (set! cP v))

(define pP (list 0 0 0))

(define (moveIt bx)
  (cond [(= wait 0) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (= clr 1) (begin (set! pP bx) (set! wait 1)) (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (= clr 2) (begin (set! pP bx) (set! wait 1)) (displayln "Not Correct Rectangle")))])]
        [(= wait 1) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (cond [(and (= clr 0) (neigh pP bx))
                                          (begin (3vs state pP 0) (3vs state bx 1)
                                                 (if (checkS state bx 1) (set! wait 2) (begin (set! wait 0) (set! cP 2))))]
                                            [(= clr 1) (set! pP bx)]
                                            [else (displayln "Not a Valid Move")]))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (cond [(and (= clr 0) (neigh pP bx))
                                          (begin (3vs state pP 0) (3vs state bx 2)
                                                 (if (checkS state bx 2) (set! wait 2) (begin (set! wait 0) (set! cP 1))))]
                                            [(= clr 2) (set! pP bx)]
                                            [else (displayln "Not a Valid Move")]))])]
        [(= wait 2) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (and (= clr 2) (or (all-in-mill state 2) (not (checkS state bx 2)))) (begin (set! wait 0) (set! cP 2) (3vs state bx 0))
                                          (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (and (= clr 1) (or (all-in-mill state 1) (not (checkS state bx 1)))) (begin (set! wait 0) (set! cP 1) (3vs state bx 0))
                                          (displayln "Not Correct Rectangle")))])])
  )

(define (putIt bx)
  (cond [(= wait 0) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (= clr 0) (begin (3vs state bx 1)
                                                           (if (checkS state bx 1) (set! wait 1) (set! cP 2)))
                                          (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (= clr 0) (begin (3vs state bx 2)
                                                           (if (checkS state bx 2) (set! wait 1)
                                                               (begin (set! cP 1) (set! cnt (+ 1 cnt)) (if (= cnt 9) (set! phase 1) #t)))
                                                           )
                                          (displayln "Not Correct Rectangle")))])]
        [(= wait 1) (cond [(= cP 1) (let ((clr (3vf state bx)))
                                      (if (and (= clr 2) (or (all-in-mill state 2) (not (checkS state bx 2)))) (begin (set! wait 0) (set! cP 2) (3vs state bx 0))
                                          (displayln "Not Correct Rectangle")))]
                          [(= cP 2) (let ((clr (3vf state bx)))
                                      (if (and (= clr 1) (or (all-in-mill state 1) (not (checkS state bx 1)))) (begin (set! wait 0) (set! cP 1) (3vs state bx 0)
                                                                                     (set! cnt (+ 1 cnt))
                                                                                     (if (= cnt 9) (set! phase 1) #t))
                                          (displayln "Not Correct Rectangle")))])])
  )


(define (gen-all-pos-func)
  (define thepos '(0 1 2))
  (filter (lambda (x) (not (and (equal? (cadr x) 1) (equal? (caddr x) 1)))) (cartesian-product thepos thepos thepos)))

(define all-pos (gen-all-pos-func))
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