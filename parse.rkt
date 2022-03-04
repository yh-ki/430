#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1 (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    ;; TODO: Handle abs, - and not
    [(list 'abs e)  (Prim1 'abs (parse e))]
    [(list '- e)  (Prim1 '- (parse e))]
    [(list 'not e) (Prim1 'not (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    ;; TODO: Handle cond
    [(list 'cond x ... (list else e)) (Cond (parse_cond x) (parse e))]
    ;; TODO: Handle case
    [(list 'case a x ... (list else e)) (Case (parse a) (parse_case x) (parse e))]
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (error "parse error")]))

(define (parse_cond l)
  (match l
    ['() '()]
    [(list (list e1 e2) x ...) (cons (Clause (parse e1) (parse e2)) (parse_cond x))]))

(define (parse_case l)
  (match l
    ['() '()]
    [(list (list (list a ...) e) x ...) (cons (Clause a (parse e)) (parse_case x))]))
    



