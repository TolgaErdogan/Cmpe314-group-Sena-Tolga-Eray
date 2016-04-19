;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname project6-7) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; λ-expression grammar
;; λ-calc -> v
;; λ-calc -> (λ-calc λ-calc)
;; λ-calc -> (λ v λ-calc)
;; where v is a symbol.

;; λ-calc is an abstract syntax grammar or a parse tree definition for
;; - λ-calc that defined above.
(define-type λ-calc
  (λ-sym (v : symbol))
  (λ-app (l : λ-calc)(r : λ-calc))
  (λ-def (v : symbol)(p : λ-calc))
  )




;; unparse : λ-calc -> s-exp
;; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparse (le : λ-calc)) : s-expression
  (type-case λ-calc le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparse l)(unparse r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparse p))))
    ))


;; substituter : λ-calc  symbol  λ-calc -> λ-calc
(define (substituter [what : λ-calc] [for : symbol] [in : λ-calc]) : λ-calc 
  (type-case λ-calc in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (substituter what for l)
                        (substituter what for r)))
    (λ-def (v p)(λ-def v (substituter what for p)))
    )
  )


;; beta-transformer : ((λ x M) N) --> [M:x=N]
;; λ-calculus beta-transformation naive implementation.
(define (beta-transformer (le : λ-calc)) : λ-calc
  (type-case λ-calc le
    (λ-sym (v) le) 
    (λ-app (l r) (if (λ-def? l)
                     (substituter r (λ-def-v l) (λ-def-p l))
                     (λ-app (beta-transformer l) (beta-transformer r))))
    (λ-def (v p) (λ-def v (beta-transformer p)))))

;; A set represented as a list.
;; union : (listof symbol) (listof symbol) -> (listof symbol)
;; finding the union of two sets.
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
;; To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; free-identifier : λ-calc -> (listof symbol)
;; Purpose : To find free identifiers in given λ expression.
(define (free-identifier (le : λ-calc)) : (listof symbol)
  (type-case λ-calc le
    (λ-sym (v) (list v))
    (λ-app (l r)(union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

(test (union empty empty) empty)
(test (union empty (list 'x)) (list 'x))
(test (union (list 'x)(list 'x 'y)) (list 'x 'y))



(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
      '((λ x x) y))



(define zero '(λ f
  (λ x
    x)))
(define one
  '(λ f
    (λ x
      (f x))))
(define two
  '(λ f
    (λ x
      (f (f x)))))
(define succ '(λ n (λ f (λ x (f ((n f) x))))))
(define (church->number n) ((n add1) 0))
(define add '(λ n (λ m (λ f (λ x ((n f) ((m f) x)))))))

;;(parse two)
;;(define (add one two) '(λ f (λ x ((one f) ((two f) x))))) 
;;(define five (add one two))
;;(beta-transformer (parse five))