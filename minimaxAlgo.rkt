#lang racket
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(define (mini-max state cP depth phase cnt)
  (cond [(= phase 1) (let* ((bxs (filter (lambda (x) (not (and (= 1 (cadr x)) (= 1 (caddr x)))))
                                        (lc (list x y z) : x <- '(0 1 2) y <- '(0 1 2) z <- '(0 1 2))))
                            (vbxs (filter (lambda (x) ()) bxs)))
                       ())]
        [(= phase 2) ()])
  )