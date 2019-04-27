#lang racket
(require "utils.rkt")
(require "eval-func.rkt")
(provide (all-defined-out))
(define (mini-max state cP depth phase cnt)
  (define (func1 bx)
    (cond [(= cP 1) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state bx 1)
                           (cond [(checkS nw-state bx 1) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 2)
                                                                               (not (checkS nw-state x 2)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max nw-state 2 (- depth 1) phase cnt))
                                                                              (3vs nw-state x 2)
                                                                              (cons (car temp) (cons bx x)))) p2b)))
                                                  (apply max1 vls))]
                                 [else (cons (car (mini-max nw-state 2 (- depth 1) phase cnt)) bx)]))]
          [(= cP 2) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state bx 2)
                           (cond [(checkS nw-state bx 2) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 1)
                                                                               (not (checkS nw-state x 1)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max nw-state 1 (- depth 1) (if (> cnt 9) 1 0) (+ 1 cnt)))
                                                                              (3vs nw-state x 1)
                                                                              (cons (car temp) (cons bx x)))) p2b)))
                                                  (apply min1 vls))]
                                 [else (cons (car (mini-max nw-state 1 (- depth 1) (if (> cnt 9) 1 0) (+ cnt 1))) bx)]))]
          )
  )

  (define (func2 move)
    (cond [(= cP 1) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state (car move) 0)
                           (3vs nw-state (cdr move) 1)
                           (cond [(checkS nw-state (cdr move) 1) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 2)
                                                                               (not (checkS nw-state x 2)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max nw-state 2 (- depth 1) phase cnt))
                                                                              (3vs nw-state x 2)
                                                                              (cons (car temp) (cons move x)))) p2b)))
                                                  (apply max1 vls))]
                                 [else (cons (car (mini-max nw-state 2 (- depth 1) phase cnt)) move)]))]
          [(= cP 2) (begin (define nw-state (make-3d-vector 3 3 3 0))
                           (vec-copy nw-state state)
                           (3vs nw-state (car move) 0)
                           (3vs nw-state (cdr move) 2)
                           (cond [(checkS nw-state (cdr move) 2) (let* ((p2b (filter (λ(x) (and (= (3vf nw-state x) 1)
                                                                               (not (checkS nw-state x 1)))) all-list))
                                                       (vls (map (λ(x) (begin (3vs nw-state x 0)
                                                                              (define temp (mini-max nw-state 1 (- depth 1) (if (> cnt 9) 1 0) (+ 1 cnt)))
                                                                              (3vs nw-state x 1)
                                                                              (cons (car temp) (cons move x)))) p2b)))
                                                  (apply min1 vls))]
                                 [else (cons (car (mini-max nw-state 1 (- depth 1) (if (> cnt 9) 1 0) (+ cnt 1))) move)]))]
          )
    )
  
  (cond [(= depth 0) (let* ((v1 (- (eval-phase state 1 phase) (eval-phase state 2 phase)))
                            (v2 (displayln v1)))
                       (cons v1 '()))]
        [(= phase 0) (let* ((moves (filter (lambda (x) (let ((clr (3vf state x))) (= clr 0))) allowed-bxs))
                            (vls (map func1 moves)))
                       (cond [(= cP 1) (apply max1 vls)]
                             [(= cP 2) (apply min1 vls)]))]
        [(= phase 1) (cond [(= cP 1) (let* ((moves (filter (λ(x) (and (= 1 (3vf state (car x)))
                                                                      (= 0 (3vf state (cdr x))))) neigh-pairs))
                                            (vls (map func2 moves)))
                                       (apply max1 vls))]
                           [(= cP 2) (let* ((moves (filter (λ(x) (and (= 2 (3vf state (car x)))
                                                                      (= 0 (3vf state (cdr x))))) neigh-pairs))
                                            (vls (map func2 moves)))
                                       (apply min1 vls))])])
  )