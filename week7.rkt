
#lang plai-typed
;; Grammar:
;; S -> ± number
;; S -> + S S
;; S -> - S S
;; S -> * S S
;; S -> ^ S S
;; S -> S
;; S -> - S
;; Data Definition of msl expression
(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-div (lhs : msl) (rhs : msl)]
  [msl-expt (lhs : msl) (rhs : msl)]
  )
(define (expt x y)
  (cond
    ((= y 0) 1)
    (else
     (* x (expt x (- y 1))))))

(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    [msl-div (lhs rhs) (/ (eval lhs) (eval rhs))]
    [msl-expt (lhs rhs) (expt (eval lhs) (eval rhs))]
    ))
    (test( eval ( msl-add ( msl-num 42) ( msl-num 23))) 65)
    (test( eval ( msl-sub ( msl-num 42) ( msl-num 23))) 19)
    (test( eval ( msl-mul ( msl-num 2) ( msl-num 4))) 8)
    (test( eval ( msl-div ( msl-num 42) ( msl-num 21))) 2)
    (test( eval ( msl-expt ( msl-num 2) ( msl-num 3))) 8)
   ;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)

    (define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [(/) (msl-div (parse (second sl)) (parse (third sl)))]
         [(-) (msl-sub (parse (second sl)) (parse (third sl)))]
          [(expt) (msl-expt  (parse (first sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
;;7
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
;;7
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))
(test (parse '(- 10 5)) (msl-sub ( msl-num 10) (msl-num 5)))
(test (parse '(- (- 20 2) 5)) (msl-sub (msl-sub (msl-num 20) (msl-num 2)) (msl-num 5)))
(test (parse '(/ 100 2)) (msl-div (msl-num 100) (msl-num 2)))
(test (parse '(/ (/ 40 5) 2)) (msl-div (msl-div (msl-num 40) (msl-num 5)) (msl-num 2)))
(test (parse '(+ (/ 20 5) 6)) (msl-add (msl-div (msl-num 20) (msl-num 5)) (msl-num 6)))
(test (parse '(* (- 12 1) 3)) (msl-mul (msl-sub (msl-num 12) (msl-num 1)) (msl-num 3)))
(test (parse '(- (** 2 3) 4)) (msl-sub (msl-expt (msl-num 2) (msl-num 3)) (msl-num 4)))
(test (parse '(- (* (- 10 4) 2)3)) (msl-sub (msl-mul (msl-sub (msl-num 10) (msl-num 4)) (msl-num 2)) (msl-num 3)))
(test (parse '(* (/ (/ 24 4) 3)5)) (msl-mul (msl-div (msl-div (msl-num 24) (msl-num 4)) (msl-num 3)) (msl-num 5)))
(test (parse '(/ (+ (* 1 6) 7) 13)) (msl-div (msl-add (msl-mul (msl-num 1) (msl-num 6)) (msl-num 7)) (msl-num 13)))
;; infix

 (define (pars [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (second sl))
         [(+) (msl-add (pars (first sl)) (parse (third sl)))]
         [(*) (msl-mul (pars (first sl)) (parse (third sl)))]
         [(/) (msl-div (pars (first sl)) (parse (third sl)))]
         [(-) (msl-sub (pars (first sl)) (parse (third sl)))]
          [(expt) (msl-expt  (pars (second sl)) (parse (third sl)))]
         [else (error 'pars "invalid list input")]))]
    [else (error 'pars "invalid input")]))
 
 ;; INFIX TESTS
 (test (pars '( 5 - 2)) (msl-sub (msl-num 5) (msl-num 2)))
 ;;3
 (test (pars '( 9 / 3)) (msl-div (msl-num 9) (msl-num 3)))
 ;;3
 (test (pars '( 3 ** 7)) (msl-expt (msl-num 3) (msl-num 7)))
 (test (pars '(( 2 * 5) - 4)) (msl-sub (msl-mul (msl-num 2) (msl-num 5)) (msl-num 4)))
  (test (pars '(( 7 + 13) / 5)) (msl-div (msl-add (msl-num 7) (msl-num 13)) (msl-num 5)))
   (test (pars '(( 4 - 2) * 3)) (msl-mul (msl-sub (msl-num 4) (msl-num 2)) (msl-num 3)))
 (test (pars '(((10 / 2) * 5) - 4)) (msl-sub (msl-mul (msl-div (msl-num 10) (msl-num 2)) (msl-num 5)) (msl-num 4)))
  (test (pars '(((2 + 3) / 1) * 7)) (msl-mul (msl-div (msl-add (msl-num 2) (msl-num 3)) (msl-num 1)) (msl-num 7)))
   (test (pars '(((8 * 3) - 6) + 2)) (msl-add (msl-sub (msl-mul (msl-num 8) (msl-num 3)) (msl-num 6)) (msl-num 2)))
(test (pars '((((20 * 4) / 4) - 6) + 10)) (msl-add (msl-sub (msl-div (msl-mul (msl-num 20) (msl-num 4)) (msl-num 4)) (msl-num 6))(msl-num 10)))
 (test (pars '((((3 + 2) - 1) * 5) / 4)) (msl-div (msl-mul (msl-sub (msl-add (msl-num 3) (msl-num 2)) (msl-num 1)) (msl-num 5))(msl-num 4)))

 ;; output-reverse-polish msl -> list of s-expression
;; output the msl as the reverse polish commands needed to evaluate it
;; examples
;; (msl-num 7) -> '(7)
;; (msl-add (msl-num 3) (msl-num 4)) -> '(4 3 +)
;; (msl-mul (msl-num 3) (msl-num 4)) -> '(4 3 *)
;; (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)) -> '(3 4 * 9 +)
;; (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9))) -> '(3 4 9 + *)
 ( define ( output-reverse-polish [ expr : msl])
   (type-case msl expr
         [msl-num (n) (list (number->s-exp n))]
         [msl-add (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs))
                  (list (symbol->s-exp '+)))]
         [msl-sub (lhs rhs) (append (append(output-reverse-polish lhs) (output-reverse-polish rhs))
                  (list (symbol->s-exp '-)))]
         [msl-mul (lhs rhs) (append (append(output-reverse-polish lhs) (output-reverse-polish rhs))
                  (list (symbol->s-exp '*)))]
         [msl-div (lhs rhs) (append (append(output-reverse-polish lhs) (output-reverse-polish rhs))
                  (list (symbol->s-exp '/)))]
         [msl-expt (lhs rhs) (append (append(output-reverse-polish lhs) (output-reverse-polish rhs))
                  (list (symbol->s-exp '**)))]))    
 ;;REVERSE POLISH TESTS
   (test (output-reverse-polish (msl-num 5)) (s-exp->list '(5)))
   (test (output-reverse-polish (msl-add (msl-num 5) (msl-num 4))) (s-exp->list '(5 4 +)))
    (test (output-reverse-polish (msl-sub (msl-num 5) (msl-num 4))) (s-exp->list '(5 4 -))) 
    (test (output-reverse-polish (msl-div (msl-num 8) (msl-num 4))) (s-exp->list '(8 4 /)))
    (test (output-reverse-polish (msl-mul (msl-num 5) (msl-num 9))) (s-exp->list '(5 9 *)))
    (test (output-reverse-polish (msl-expt (msl-num 5) (msl-num 4))) (s-exp->list '(5 4 **)))
    (test (output-reverse-polish (msl-add (msl-num 5) (msl-div   (msl-num 4) (msl-num 3)))) (s-exp->list '(5 4 3 / + )))
    
    ;;desugar  
(define-type ArithS
      [numS ( n : number)]
      [plusS (l : ArithS) (r : ArithS)]
      [bminusS (l : ArithS) (r : ArithS)]
      [multS (l : ArithS) (r : ArithS)]
      [uminusS (e : ArithS) ])
    (define  (desugar (as : ArithS)) : msl
      (type-case ArithS as
        [numS (n) (msl-num n)]
        [plusS (l r) (msl-add (desugar l)
                              (desugar r))]
        [multS (l r) (msl-mul (desugar l)
                              (desugar r))]
        [bminusS (l r) (msl-add (desugar l)
                            (msl-mul (msl-num -1)  (desugar r)))]
        [uminusS (e) (desugar (bminusS (numS 0) e))]))
    ;;DESUGAR TESTS
    (test (desugar ( numS 5)) (msl-num 5))
    ;;5
    (test (desugar (bminusS (numS 5) (numS 2))) (msl-add (msl-num 5) (msl-mul  (msl-num -1) (msl-num 2))))
     (test (desugar (bminusS (numS 1) (numS 7))) (msl-add (msl-num 1) (msl-mul  (msl-num -1) (msl-num 7))))
    (test (desugar (plusS (numS 3) (numS 7))) (msl-add (msl-num 3) (msl-num 7)))
     (test (desugar (plusS (numS 2) (numS 5))) (msl-add (msl-num 2) (msl-num 5)))
    (test (desugar (uminusS (numS 5))) (desugar (bminusS (numS 0) (numS 5))))
    (test (desugar (uminusS (numS 8))) (desugar (bminusS (numS 0) (numS 8))))
   
(define-type funcF
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : funcF)]
  [plusC (lhs : funcF) (rhs : funcF)]
  [multC (lhs : funcF) (rhs : funcF)])

(define (newpars [s : s-expression]) : funcF
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (newpars (second sl)) (newpars (third sl)))]
            [(*) (multC (newpars (second sl)) (newpars (third sl)))]
            [else (error 'newpars "invalid list input!")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (newpars (second sl)))]
         [else (error 'newpars "invalid number of inputs")]
         ))]
    [else (error 'newpars "invalid input!")]))


(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : funcF)])

(define FuncDefName
  (list
   (fdC 'square 'x (newpars '(* x x)))
   (fdC 'subtract1 'x (newpars '(+ x -1)))
   (fdC 'negative 'x (newpars '(* x -1)))))

;symbol -> FunDefC

(define (get-funcDef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-funcDef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-funcDef n (rest fds))])]))


;;SUBSTITUTE
(define (substitute [what : funcF] [for : symbol] [in : funcF]) : funcF
  (type-case funcF in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (substitute what for a))]
    [plusC (l r) (plusC (substitute what for l)
                        (substitute what for r))]
    [multC (l r) (multC (substitute what for l)
                       (substitute what for r))]))


;Tests


(test (substitute (numC 2) 'x (numC 3))(numC 3))
(test (substitute (numC 5) 'y (newpars '(f (* x x))))(appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 5) 'x (newpars '(* x x)))(multC (numC 5) (numC 5)))
(test (substitute (numC 4) 'x (newpars '(+ x x))) (plusC (numC 4) (numC 4)))
(test (substitute (numC 2) 'x (numC 3))(numC 3))
(test (substitute (numC 3) 'x (newpars '(f (* x x)))) (appC 'f (multC (numC 3) (numC 3))))
(test (substitute (numC 16) 'y (newpars '(f (* x x))))(appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 62) 'x (newpars '(* x x))) (multC (numC 62) (numC 62)))
(test (substitute (numC 4) 'x (newpars '(+ x x))) (plusC (numC 4) (numC 4)))
(test (substitute (numC 1) 'x (numC 2))(numC 2))
(test (substitute (numC 2) 'x (newpars '(f (* x x))))(appC 'f (multC (numC 2) (numC 2))))
(test (substitute (numC 2) 'y (newpars '(f (* x x))))(appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 5) 'x (newpars '(* x x))) (multC (numC 5) (numC 5)))
(test (substitute (numC 3) 'x (newpars '(+ x x)))(plusC (numC 3) (numC 3)))


;;INTERPRE

(define (interpre [e : funcF] [fds : (listof FunDefC)]) : number
  (type-case funcF e
    [numC (n) n]
    [idC (_) (error 'interpre "error")]
    [appC (f a) (local ([define fd (get-funcDef f fds)])
                  (interpre (substitute
                           (numC (interpre a fds)) 
                           (fdC-arg fd)
                           (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interpre l fds) (interpre r fds))]
    [multC (l r) (* (interpre l fds) (interpre r fds))]))

;Interpre Test

;;(+ 40 15) -> 55
(test (interpre (newpars '(+ 40 15)) empty) 55)
(interpre (newpars '(+ 40 15)) empty)
;;( * 3 3) -> 9)
(test (interpre (newpars '(* 3 3)) empty) 9)
(interpre (newpars '(* 3 3)) empty)

(test (interpre (newpars '(square 5)) FuncDefName) 25)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 76)) FuncDefName) -76)
(interpre (newpars '(negative 76)) FuncDefName)
(test (interpre (newpars '(square 9)) FuncDefName) 81)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 10)) FuncDefName) -10)
(interpre (newpars '(negative 10)) FuncDefName)
(test (interpre (newpars '(square 6)) FuncDefName) 36)
(interpre (newpars '(square 6)) FuncDefName)
(test (interpre (newpars '(negative 9)) FuncDefName) -9)
(interpre (newpars '(negative 9)) FuncDefName)
(test (interpre (newpars '(negative 35)) FuncDefName) -35)
(interpre (newpars '(negative 35)) FuncDefName)

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
(define (parsel (sx : s-expression)) : λ-calc
  (cond
    [(s-exp-symbol? sx)(λ-sym (s-exp->symbol sx))]
    [(s-exp-list? sx)
     (let ([sx-list (s-exp->list sx)])
       (cond
         [(= 2 (length sx-list))
          (λ-app (parsel (first sx-list))(parsel (second sx-list)))]
         [(= 3 (length sx-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sx-list)))
                   (s-exp-symbol? (second sx-list)))
              (λ-def (s-exp->symbol(second sx-list))
                     (parsel (third sx-list)))
              (error 'parse "Not valid λ-definition")
              )]
         [else (error 'parse "Not valid length λ-calc")]
         ))]
    [else (error 'parse "Not valid λ-calc")]
    ))


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

(test (parsel (symbol->s-exp 'y))(λ-sym 'y))
(test (parsel '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parsel '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))

(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
      '((λ x x) y))

(test (free-identifier (parsel '(λ x x))) empty)
(test (free-identifier (parsel '(λ x y))) (list 'y))
(test (free-identifier (parsel '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parsel '((λ f y)(λ z z)))) (list 'y))

(test (beta-transformer (parsel '((λ x x) a)))
      (parsel (symbol->s-exp 'a)))

(test (beta-transformer (parsel '((λ x y) a)))
      (parsel (symbol->s-exp 'y)))

(test (beta-transformer (parsel '((λ x (a b)) k)))
      (parsel '(a b)))

(test (beta-transformer (parsel '((λ x (λ x y)) k)))
      (parsel '(λ x y)))

(test (beta-transformer (parsel '((λ x (λ y x)) k)))
      (parsel '(λ y k)))

(test (beta-transformer (parsel '((λ x (λ y (x y))) k)))
      (parsel '(λ y (k y))))

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
(beta-transformer(parsel add))
;;(parse two)
;;(define (add one two) '(λ f (λ x ((one f) ((two f) x))))) 
;;(define five (add one two))
;;(beta-transformer (parse five))







        
        
         
         
 
 


    
 
        
         
         
 
 

