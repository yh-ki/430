#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim1 p e)       (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    ;; TODO: Handle cond
    [(Cond cs e)       (compile-cond cs e)]
    ;; TODO: Handle case
    ))


;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (value->bits i))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (value->bits b))))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ;; TODO: Handle abs, -, and not
         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ['abs
            (seq (Cmp 'rax 0)
                 (Jg 'abs_done)
                 (Mov 'rbx (value->bits 0))
                 (Sub 'rbx 'rax)
                 (Push 'rbx)
                 (Pop 'rax)
                 (Label 'abs_done))]
         ['-
          (seq (Mov 'rbx (value->bits 0))
               (Sub 'rbx 'rax)
               (Push 'rbx)
               (Pop 'rax))]
         ['not
          (seq (Cmp 'rax val-false)
               (Je 'not_done)
               (Mov 'rax val-false)
               (Ret)
               (Label 'not_done)
               (Mov 'rax val-true))])))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

(define (compile-cond cs e)
  (match cs
    ['() (seq  (compile-e e))]
    [(list (Clause e1 e2) x ...)
     (seq (compile-e e1)
          (Cmp 'rax val-false)
          (Je 'c1)
          (compile-e e2)
          (Ret)
          (Label 'c1)
          (compile-cond x e))]))






