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
    [(Case e cs el)    (compile-case e cs el)]
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
               (Je 't)
               (Mov 'rax val-false)
               (Jmp 'not_done)
               (Label 't)
               (Mov 'rax val-true)
               (Label 'not_done))])))

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

;;[Listof CondClause] Expr -> Asm
(define (compile-cond cs e)
  (match cs
    ['() (seq  (compile-e e))]
    [(list (Clause e1 e2) x ...)
     (let ((c1 (gensym 'cond))
           (c2 (gensym 'cond)))
     (seq (compile-e e1)
          (Cmp 'rax val-false)
          (Je c1)
          (compile-e e2)
          (Jmp c2)
          (Label c1)
          (compile-cond x e)
          (Label c2)))]))

;;Expr [Listof CaseClause] Expr -> Asm
(define (compile-case e cs el)
  (match cs
    ['() (seq  (compile-e el))]
    [(list (Clause a b) x ...)
     (let ((c1 (gensym 'case))
           (c2 (gensym 'case)))
     (seq (compile-e e)
          (Mov 'rbx val-true)
          (contain? a)
          (Cmp 'rbx val-true)
          (Jne c1)
          (compile-e b)
          (Jmp c2)
          (Label c1)
          (compile-case e x el)
          (Label c2)))]))

(define (contain? a)
  (match a
    ['() (seq (Mov 'rbx val-false))]
    [(list x y ...)
     (let ((d1 (gensym 'cont))
           (d2 (gensym 'cont)))
       (seq (Cmp 'rax x)
            (Jne d1)
            (Jmp d2)
            (Label d1)
            (contain? y)
            (Label d2)))]))





