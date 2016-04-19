#lang racket
(require plai-typed)


;; Grammar:
;; S -> ± number
;; S -> + S S
;; S -> - S S
;; S -> * S S
;; S -> ^ S S
;; S -> S

;; Data Definition of msl expression
(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-expt (lhs : msl) (rhs : msl)]
  )
(define (expt x y)
  (cond
    ((= y 0) 1)
    (else
     (* x (expt x (- y 1))))))

;Purpose 
;; evaluate an msl expression
(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    [msl-expt (lhs rhs) (expt (eval lhs) (eval rhs))]
    ))
;Tests
 (test(eval (msl-num 7))7)
(test (eval (msl-num 12))  12)
(test(eval (msl-add (msl-num 40) (msl-num 10)))50)
(test(eval (msl-expt (msl-num 5) (msl-num 5)))3125)
(test (eval (msl-sub (msl-num 22) (msl-num 13))) 9)
(test (eval (msl-sub (msl-num 71) (msl-num 70))) 1)
(test (eval (msl-expt (msl-num 4) (msl-num 3))) 64)
(test (eval (msl-expt (msl-num 5) (msl-num 4))) 625)
(test (eval (msl-mul (msl-expt (msl-num 2) (msl-num 5)) (msl-sub (msl-num 12) (msl-num 8)))) 128)
(test (eval (msl-add (msl-sub (msl-num 6) (msl-num 4)) (msl-mul (msl-num 2) (msl-num 7)))) 16)


;; parse infix s-expression -> msl
;; convert a infix arithmetic expression into the equivalent msl form.


(define (in-parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (second sl))
         [(+) (msl-add (in-parse (first sl)) (in-parse (third sl)))]
         [(*) (msl-mul (in-parse (first sl)) (in-parse (third sl)))]
         [(-) (msl-sub (in-parse (first sl)) (in-parse (third sl)))]
         [(**) (msl-expt (in-parse (first sl)) (in-parse (third sl)))]

         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; Tests for in-parse.
(in-parse '(3 + 4))
(test (in-parse '7) (msl-num 7))
(test (in-parse '(3 + 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (in-parse '((3 + 4) + 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))


;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [(-) (msl-sub (parse (second sl)) (parse (third sl)))]
         [(**) (msl-expt (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))



;;defines a nwe data type
; S -> number
; S -> + S S
; S -> - S S
; S -> * S S
; S -> ^ S S
; S -> -1 * S (The best one without problem) (Uniary Minus)
; S -> (S)

(define-type mslS
  [numS (n : number)]
  [plusS (l : mslS) (r : mslS)]
  [bminusS (l : mslS) (r : mslS)]
  [mulS (l : mslS) (r : mslS)]
  [expS (l : mslS) (r : mslS)]
  [uminusS (e : mslS)])

;;pre- parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (numFS 7)
;; '(+ 3 4) -> (plusFS (numFS 3) (numFS 4))
;; '(+ (+ 3 4) 35) -> (plusFS (plusFS (numFS 3) (numFS 4)) (numFS 35))
(define (preS-parse [s : s-expression]) : mslS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
      (cond
        [(= (length sl) 2) (case (s-exp->symbol (first sl)) [(u-) (uminusS (preS-parse (second sl)))]) ]
        [else (case (s-exp->symbol (first sl))
           [(+) (plusS (preS-parse (second sl)) (preS-parse (third sl)))]
           [(*) (mulS (preS-parse (second sl)) (preS-parse (third sl)))]
           [(-) (bminusS (preS-parse (second sl)) (preS-parse (third sl)))]
           [(**) (expS (preS-parse (second sl)) (preS-parse (third sl)))]
         [else (error 'preF-parse "invalid list input")])]))]
    [else (error 'preS-parse "invalid input")]))


(test (preS-parse '7) (numS 7))

(test (preS-parse '(+ 3 4)) (plusS (numS 3) (numS 4)))
(test (preS-parse '(+ (* (u- 7) 5) 7))(plusS (mulS (uminusS (numS 7)) (numS 5)) (numS 7))) 
(test (preS-parse '(u- 7)) (uminusS (numS 7)))
(test (preS-parse '(** 3 7)) (expS (numS 3) (numS 7)))
(test (preS-parse '(** 5 6)) (expS (numS 5) (numS 6)))
(test (preS-parse '(u- 12)) (uminusS (numS 12)))
(test (preS-parse '(u- 23)) (uminusS (numS 23)))

;; desugar mslS -> msl
;; convert new data type to old data type

(define (desugar [as : mslS]) : msl
  (type-case mslS as
    [numS (n)(msl-num n)] 
    [plusS (lhs rhs) (msl-add (desugar lhs) (desugar rhs))]
    [bminusS (lhs rhs) (msl-sub (desugar lhs) (desugar rhs))]
    [mulS (lhs rhs) (msl-mul (desugar lhs) (desugar rhs))]
    [expS (lhs rhs) (msl-expt (desugar lhs) (desugar rhs))]
    [uminusS (e) (msl-mul (msl-num -1) (desugar e))]))



(test (desugar (numS 5)) (msl-num 5))
(test (desugar (plusS (numS 3) (numS 4)))  (msl-add (msl-num 3)(msl-num 4)))
(test (desugar (numS 6)) (msl-num 6))
(test (desugar (plusS (numS 5) (numS 5))) (msl-add (msl-num 5) (msl-num 5)))
(test (desugar (bminusS (numS 7) (numS 7))) (msl-sub (msl-num 7) (msl-num 7)))
(test (desugar (mulS (numS 8) (numS 8))) (msl-mul (msl-num 8) (msl-num 8)))
(test (desugar (plusS (uminusS (numS 3)) (mulS (numS 5) (uminusS (mulS (numS 7) (numS 6))))))
      (plusS (mulS(numS -1) (numS 3)) (mulS (numS 5) (mulS (numS -1) (mulS (numS 7) (numS 6))))))



(define (inS-parse [s : s-expression]) : mslS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
      (cond
        [(= (length sl) 2) (case (s-exp->symbol (first sl)) [(u-) (uminusS (inS-parse (second sl)))]) ]
        [else (case (s-exp->symbol (second sl))
           [(+) (plusS (inS-parse (first sl)) (inS-parse (third sl)))]
           [(*) (mulS (inS-parse (first sl)) (inS-parse (third sl)))]
           [(-) (bminusS (inS-parse (first sl)) (inS-parse (third sl)))]
           [(**) (expS (inS-parse (first sl)) (inS-parse (third sl)))]
         [else (error 'inS-parse "invalid list input")])]))]
    [else (error 'inS-parse "invalid input")]))


(test (inS-parse '(u- 44)) (uminusS (numS 44)))
(test (inS-parse '7) (numS 7))

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l :  ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [mulC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  [ifC (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factC (x : number)]



  )





(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol))  (body : ExprC)])







;; parseE s-expression -> ExprC

;; convert a quoted s expression into the equivalent ArithC form
;; examples

;;  '(+ 23 (+ 23 5)))-> (plusC (numC 23)(plusC (numC 23) (numC 5))))

;; (symbol->s-exp 'x))->  (idC 'x))

;; '(if 1 2 3)->(ifC (numC 1) (numC 2) (numC 3)))

(define (parseE [s :  (listof s-expression)]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'ifC (s-exp->symbol (first sl)))
              (ifC (parseE (second sl))
                       (parseE (third sl))
                       (parseE (fourth sl)))
              (error 'parseE "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parseE (second sl)) (parseE (third sl)))]
            [(*) (mulC (parseE (second sl)) (parseE (third sl)))]
            [(-) (subC (parseE (second sl)) (parseE (third sl)))]
            [(**) (expC (parseE (second sl)) (parseE (third sl)))]
            [else (error 'parseE "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parseE (second sl)))]
         [else (error 'parseE "invalid list input")])
       )]
    [else (error 'parseE "invalid input")]))
;; Tests :
"tests"
(test (parseE '(+ 3 4)) (plusC (numC 3) (numC 4)))

(test (parseE '(* 12 7)) (mulC (numC 12) (numC 7)))


(test (parseE '(+ 23 (+ 23 5)))

      (plusC (numC 23)

            (plusC (numC 23) (numC 5))))

(test (parseE (symbol->s-exp 'x)) (idC 'x))


(test (parseE '(double 13))

      (appC 'double (numC 13)))

;(test(parse '(if 1 2 3))(ifC (numC 1) (numC 2) (numC 3)))


;get-fundef
;;symbol (list of func definitions)-> : FunDefC
;Purpose
;; it takes a symobol and generate a function definition.

(fdC 'double  '(x , y) (plusC (idC  'x) (idC  'y)))

;; FunDefC
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))
 

 ;Subst
 ;; ExprC symbol ExprC -> ExprC
 ;Purpose
 ;; it takes a expression ( numC 7) , argument ('x) and the function it self. It produces the function with changes(numC 7) placed for every 'x in function
 ;;Examples
 ;;(subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) -> (plusC (plusC (numC 7) (numC 7)) (numC 7))

(define (subst [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
     (type-case ExprC in
     [numC (n) in]
     [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
     [appC (f a) (appC f (subst what for a))]
     [plusC (l r) (plusC (subst what for l)
                         (subst what for r))]
 
     [subC (l r) (plusC (subst what for l)
                         (subst what for r))]
     [mulC (l r) (mulC (subst what for l)
                         (subst what for r))]
    [expC (l r) (mulC (subst what for l)
                         (subst what for r))]
 
       [factC (x) (factC (subst what for x))]
     [ifC (exp1 exp2 exp3) (ifC (subst what for exp1) (subst what for exp2) (subst what for exp3))]
    ))


;Tests for substitution
 (test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
 (test (subst(plusC (numC 3) (numC 4)) 'y (plusC (mulC (idC  'y) (idC  'y)) (idC 'y))) (plusC (mulC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
  ;(subst (numC 7  numC 8) '(x  y) (plusC (plusC (idC  'x) (idC  'y)))) 

(test (subst (numC 5) 'x (plusC (idC 'x) (idC 'x)))
      (plusC (numC 5) (numC 5)))
 
(test (subst (numC 5) 'x (mulC (idC 'x) (idC 'x)))
      (mulC (numC 5) (numC 5)))

;; Interp Eager
;; ExprC -> fds -> number
;; it takes an expression and list of function definitions and output 
;; a number
;; Function Application
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
               (interp (subst a
                              (fdC-arg fd)
                              (fdC-body fd))
                       fds))]
   [ifC (exp1 exp2 exp3) (cond
                           [(> (interp exp1 fds) 0) (interp exp2 fds)]
                        [else (interp exp3 fds)])]
   [plusC (l r) (+ (interp l fds) (interp r fds))]
   [subC (l r) (- (interp l fds) (interp r fds))]
   [mulC (l r) (* (interp l fds) (interp r fds))]
   [expC (l r) (expt (interp l fds) (interp r fds))]
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]))


  
 ;Tests 
(test(interp(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
(test(interp(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
(test(interp(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
(test(interp(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
;(test (interp(factaccC 3 1 ) (plusC (multC (idC 'x) ))))

(test (interp (numC 4) (list)) 4)
(test (interp (plusC (numC 1) (numC 2)) (list)) 3)
(test (interp (mulC (numC 5) (numC 2)) (list)) 10)
(test (interp (appC 'double (numC 10)) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 20)
;Interp LAZY
 ;;ExprC -> fds (listof FunDefC) - > number 
 ;Purpose
 ;;it takes an expression and list of function definitions and output a number (Function Application)

(define (interp2 [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp2 "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
                 
               (interp2 (subst a (fdC-arg fd) (fdC-body fd)) fds))]
            
                
   [ifC (exp1 exp2 exp3) (cond
                           [(> (interp2 exp1 fds) 0) (interp2 exp2 fds)]
                        [else (interp2 exp3 fds)])]
   [plusC (l r) (+ (interp2 l fds) (interp2 r fds))]
   [subC (l r) (- (interp2 l fds) (interp2 r fds))]
   [mulC (l r) (* (interp2 l fds) (interp2 r fds))]
   [expC (l r) (* (interp2 l fds) (interp2 r fds))]
   ;; expC is taking * because I didnt write the ** operation
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp2 (factC (- x 1)) fds))])]))




 ;Tests 
(test(interp2(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
(test(interp2(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
(test(interp2(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
(test(interp2(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
;(test (interp(factaccC 3 1 ) (plusC (multC (idC 'x) ))))
(test (interp2 (numC 4) (list)) 4)
(test (interp2 (plusC (numC 1) (numC 2)) (list)) 3)
(test (interp2 (mulC (numC 5) (numC 2)) (list)) 10)
(test (interp2 (appC 'double (numC 10)) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 20)





