#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    ;; TODO: Handle cond
    [(Cond cs e)
     (interp-cond cs e)]
    ;; TODO: Handle case
    [(Case e cs el)
     (interp-case e cs el)]
    ))

(define (interp-cond cs e)
  (match cs
    ['() (interp e)]
    [(list (Clause e1 e2) x ...) (if (interp e1) (interp e2) (interp-cond x e))]))

(define (interp-case e cs el)
  (match cs
    ['() (interp el)]
    [(list (Clause a e) x ...) (if (member a) (interp e) (interp-case e x el))]))


