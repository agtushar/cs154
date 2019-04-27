#lang racket
(require "utils.rkt")

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

(define neigh-pairs (filter (λ(x) (neigh (car x) (cdr x))) (lc (cons x y) : x <- all-list y <- all-list)))

(define (vec-copy a b)
  (map (λ(x) (begin (3vs a x (3vf b x)) #t)) all-list)
  )

(define (mini-max state cP depth phase cnt)
  (define (func1 bx)
    (cond [(= cP 1) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state bx 1)
                           (cond [(checkS bx 1) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 2)
                                                                               (not (checkS x 2)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max state 2 (- depth 1) phase cnt))
                                                                              (3vs nw-state x 2)
                                                                              temp)) p2b)))
                                                  (apply max vls))]
                                 [else (mini-max nw-state 2 (- depth 1) phase cnt)]))]
          [(= cP 2) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state bx 2)
                           (cond [(checkS bx 2) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 1)
                                                                               (not (checkS x 1)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max state 1 (- depth 1) (if (> cnt 9) 1 0) (+ 1 cnt)))
                                                                              (3vs nw-state x 1)
                                                                              temp)) p2b)))
                                                  (apply min vls))]
                                 [else (mini-max nw-state 1 (- depth 1) (if (> cnt 9) 1 0) (+ cnt 1))]))]
          )
  )

  (define (func2 move)
    (cond [(= cP 1) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state (car move) 0)
                           (3vs nw-state (cdr move) 1)
                           (cond [(checkS (cdr move) 1) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 2)
                                                                               (not (checkS x 2)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max state 2 (- depth 1) phase cnt))
                                                                              (3vs nw-state x 2)
                                                                              temp)) p2b)))
                                                  (apply max vls))]
                                 [else (mini-max nw-state 2 (- depth 1) phase cnt)]))]
          [(= cP 2) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state (car move) 0)
                           (3vs nw-state (cdr move) 2)
                           (cond [(checkS (cdr move) 2) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 1)
                                                                               (not (checkS x 1)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max state 1 (- depth 1) (if (> cnt 9) 1 0) (+ 1 cnt)))
                                                                              (3vs nw-state x 1)
                                                                              temp)) p2b)))
                                                  (apply min vls))]
                                 [else (mini-max nw-state 1 (- depth 1) (if (> cnt 9) 1 0) (+ cnt 1))]))]
          )
    )
  
  (cond [(= depth 0) (- (eval state 1 phase) (eval state 2 phase))]
        [(= phase 0) (let* ((moves (filter (lambda (x) (let ((clr (3vf state x))) (= clr 0))) allowed-bxs))
                            (vls (map func1 moves)))
                       (cond [(= cP 1) (apply max vls)]
                             [(= cP 2) (apply min vls)]))]
        [(= phase 1) (cond [(= cP 1) (let* ((moves (filter (λ(x) (and (= 1 (3vf state (car x)))
                                                                      (= 0 (3vf state (cdr x))))) neigh-pairs))
                                            (vls (map func2 moves)))
                                       (apply max vls))]
                           [(= cP 2) (let* ((moves (filter (λ(x) (and (= 2 (3vf state (car x)))
                                                                      (= 0 (3vf state (cdr x))))) neigh-pairs))
                                            (vls (map func2 moves)))
                                       (apply min vls))])])
  )
