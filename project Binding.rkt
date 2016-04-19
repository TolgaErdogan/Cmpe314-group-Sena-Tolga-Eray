#lang racket
(require plai-typed)


;Formal Grammar

 ; S-> number
 ; S -> symbol
 ; S -> + S S
 ; S -> - S S
 ; S -> * S S
 ; S-> (S)
 ;S->F

; Function definition
; F is a function
; Ls is a list of parameter
; B is body
; F -> (Name)Ls{B}
; Name -> symbol
; B-> S
; Ls-> listOfSymbols

;; Function Application
 ;Fa is function application
 ;Fs is a function symbol
 ;La is a list of arguments
 ;Fa -> FsLa
 ;La  -> listOfSymbols
 ;Fs -> symbol
     

;;Defines datatype for ExprC

(define-type ExprC


[numC (n : number)]

[plusC (lhs : ExprC) (rhs : ExprC)]
[subC (lhs : ExprC) (rhs : ExprC)]

[multC (lhs : ExprC) (rhs : ExprC)]

[ifC (condition : ExprC)(yes : ExprC)(no : ExprC)]

; aplication, with the name of the
; function and the argument
;; Function Application with multiple parameters.
[appC (fun : symbol) (arg : (listof ExprC))]
[idC (s : symbol)] ; identifier for the arguments
[factC (x : number)]
[factaccC (x : number) (acc : number)]
[fibonacciC (x : number)])

;;Defines datatype for function definitions

;;function definitions have a name, one argument, and a body


;; Function Definition with multiple parameters. 
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol))  (body : ExprC)])


;; parse s-expression -> ExprC

;; convert a quoted s expression into the equivalent ArithC form
;; examples

;;  '(+ 23 (+ 23 5)))-> (plusC (numC 23)(plusC (numC 23) (numC 5))))

;; (symbol->s-exp 'x))->  (idC 'x))

;; '(if 1 2 3)->(ifC (numC 1) (numC 2) (numC 3)))

(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'ifC (s-exp->symbol (first sl)))
              (ifC (parse (second sl))
                       (parse (third sl))
                       (parse (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parse (second sl)) (parse (third sl)))]
            [(*) (multC (parse (second sl)) (parse (third sl)))]
            [(-) (subC (parse (second sl)) (parse (third sl)))]
         
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))
;; Tests :
"tests"
(test (parse '(+ 3 4)) (plusC (numC 3) (numC 4)))

(test (parse '(* 12 7)) (multC (numC 12) (numC 7)))


(test (parse '(+ 23 (+ 23 5)))

      (plusC (numC 23)

            (plusC (numC 23) (numC 5))))

(test (parse (symbol->s-exp 'x)) (idC 'x))


(test (parse '(double 13))

      (appC 'double (numC 13)))





;(test(parse '(if 1 2 3))(ifC (numC 1) (numC 2) (numC 3)))


;get-fundef
;;symbol (list of func definitions)-> : FunDefC
;Purpose
;; it takes a symobol and generate a function definition.

(fdC 'double  '(x , y) (plusC (idC  'x) (idC  'y)))


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
     [multC (l r) (multC (subst what for l)
                         (subst what for r))]
    
     [factC (x) (factC (subst what for x))]
     [ifC (exp1 exp2 exp3) (ifC (subst what for exp1) (subst what for exp2) (subst what for exp3))]
    [factaccC (x fact) (factaccC (subst what for x) (subst what for fact))]

       [fibonacciC (x) (fibonacciC (subst what for x))]


       ))


;Tests for substitution
 (test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
 (test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
  ;(subst (numC 7  numC 8) '(x  y) (plusC (plusC (idC  'x) (idC  'y)))) 

(test (subst (numC 5) 'x (plusC (idC 'x) (idC 'x)))
      (plusC (numC 5) (numC 5)))
 
(test (subst (numC 5) 'x (multC (idC 'x) (idC 'x)))
      (multC (numC 5) (numC 5)))


;Interp LAZY
 ;;ExprC -> fds (listof FunDefC) - > number 
 ;Purpose
 ;;it takes an expression and list of function definitions and output a number (Function Application)
 ;;Examples
 ;(numC 7) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) -> 7
 ;(igz(numC -5) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) -> 0


(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
                 
               (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
            
                
   [ifC (exp1 exp2 exp3) (cond
                           [(> (interp exp1 fds) 0) (interp exp2 fds)]
                        [else (interp exp3 fds)])]
   [plusC (l r) (+ (interp l fds) (interp r fds))]
   [subC (l r) (- (interp l fds) (interp r fds))]
   [multC (l r) (* (interp l fds) (interp r fds))]
   
  [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]
  [factaccC (x acc) (cond
                     [ (= x 1) acc]
                       [else ( interp (factaccC (- x 1) (* x acc)) fds )])]

     [fibonacciC (x ) (cond
                     [ (or (= x 1) (= x 2))1]
                       [else (+( interp-env (fibonacciC (- x 1)) (fibonacciC (- x 2)) fds ))])]






     ))

     

 
 
 ;Tests 
;(test(interp(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
;(test(interp(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
;(test(interp(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
;(test(interp(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
;(test (interp(factaccC 3 1 ) (plusC (multC (idC 'x) ))))

(test (interp (numC 4) (list)) 4)
(test (interp (plusC (numC 1) (numC 2)) (list)) 3)
(test (interp (multC (numC 5) (numC 2)) (list)) 10)
(test (interp (appC 'double (numC 10)) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 20)

;;interp1 EAGER
(define (interp1 [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error interp1 "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
               
                
                 (interp1 (subst (numC (interp1 a fds))
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
   [ifC (exp1 exp2 exp3) (cond
                           [(> (interp1 exp1 fds) 0) (interp1 exp2 fds)]
                        [else (interp1 exp3 fds)])]
   [plusC (l r) (+ (interp1 l fds) (interp1 r fds))]
   [subC (l r) (- (interp1 l fds) (interp1 r fds))]
   [multC (l r) (* (interp1 l fds) (interp1 r fds))]
   
  [factC (x) (cond
               [(= x 1) 1]
              [else (* x (interp1 (factC (- x 1)) fds))])]
     [factaccC (x acc) (cond
                     [ (= x 1) acc]
                       [else ( interp (factaccC (- x 1) (* x acc)) fds )])]


[fibonacciC (x ) (cond
                     [ (or (= x 1) (= x 2))1]
                       [else (+( interp-env (fibonacciC (- x 1)) (fibonacciC (- x 2))  fds ))])]


     ))







 ;Tests 
(test(interp1(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
(test(interp1(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
(test(interp1(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
(test(interp1(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
;(test (interp(factaccC 3 1 ) (plusC (multC (idC 'x) ))))

(test (interp1 (numC 4) (list)) 4)
(test (interp1 (plusC (numC 1) (numC 2)) (list)) 3)
(test (interp1 (multC (numC 5) (numC 2)) (list)) 10)
(test (interp1 (appC 'double (numC 10)) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 20)




;Binding

;this function takes symbol as name and value which is number

;to bind any funciton


(define-type Binding [bind (name : symbol) (val : number)])


;; An alias to work easily on Environment.

(define-type-alias Environment (listof Binding))


;; Empty environment.

(define mt-env empty)


;; Extending environment

(define extend-env cons)


;; Example Environment.
(define EnvNameSpace
  (list
   (bind 'x (numC 5))
   (bind 'y (numC 6))
   (bind 'z (numC 7))
   ))
; Example function definition.
(define FuncList
  (list
   (fdC 'sqr (list 'x)(parse '(* x x)))
   (fdC 'sub1 (list 'x)(parse '(+ x -1)))
   (fdC 'neg (list 'x)(parse '(* x -1)))
   (fdC 'double (list 'x)(parse '(+ x x)))
   (fdC 'const5 (list '_)(parse (number->s-exp 5)))
   (fdC 'factorial 'n (parse 
                       '(ifC n 1
                                (* n (factorial (sub1 n))))))


   (fdC 'fibonacci 'n (parse 
                       '(ifC n 1
                                (ifC (- n 1) 1
                                        (ifC (- n 2) 1
                                                (+ (fibonacci (- n 1))
                                                   (fibonacci (- n 2))
                                                   ))))))))



   
 



;;lookup function takes n as a symbol and environment which includes binding values,

;; then it checks wheter this funciton in environment or not?

;;if there is,it produces value otherwise it gives error


(define (lookup [for : symbol] [env : Environment]) : number

  (cond

    [(empty? env) (error 'lookup "name not found")]

    [else (cond

            [(symbol=? for (bind-name (first env)))

             (bind-val (first env))]

            [else (lookup for (rest env))])]))

 (test(lookup 'x EnvNameSpace)( numC 5))
(test (lookup 'y EnvNameSpace) ( numC 6))
(test (lookup 'z EnvNameSpace)( numC 7))

;; interp : ExprC (listof FunDefC) -> number



;; Interpreter 

;; Purpose : To interpreter given ExprC to number

;; Template : 

;(define (interp [expr : ExprC] [env : Environment][fds : (listof FunDefC)]) : number

;  (type-case

;    [n ...]

;    [id ...]

;     [app..]

;    [plusC..]

;     [multC..]






(define (interp-env [expr : ExprC] [env : Environment] [fds : (listof FunDefC)]) : number

  (type-case ExprC expr

    [numC (n) n]

    [idC (n) (lookup n env)]

    [appC (f a) (local ([define fd (get-fundef f fds)])

                  (interp-env (fdC-body fd)

                          (extend-env (bind (fdC-arg fd)

                                            (interp-env a env fds))

                                      mt-env) fds))]

    [plusC (l r) (+ (interp-env l env fds) (interp-env r env fds))]
    [subC (l r) (- (interp-env l env fds) (interp-env r env fds))]

    [multC (l r) (* (interp-env l env fds) (interp-env r env fds))]


    [ifC (pred t f)

            (if (= 0 (interp-env pred env fds))
                 (interp-env t env fds)
                 (interp-env f env fds))]

 [factC (x) (cond
               [(= x 1) 1]
              [else (* x (interp-env (factC (- x 1)) env fds))])]



[factaccC (x acc) (cond
                     [ (= x 1) acc]
                       [else ( interp-env (factaccC (- x 1) (* x acc)) env fds )])]

[fibonacciC (x ) (cond
                     [ (or (= x 1) (= x 2))1]
                       [else (+( interp-env (fibonacciC (- x 1)) (fibonacciC (- x 2)) env fds ))])]

    ))







"TEST of Interp"
;(test (interp-env  (factC (numC 2))
 ;             mt-env
  ;            empty) 8)



(test (interp-env (plusC (numC 10) (appC 'const5 (numC 10)))

              mt-env

              (list (fdC 'const5 '_ (numC 5))))

      15)




(test (interp-env (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))

              mt-env

              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))

      16)


(test (interp-env (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))

              mt-env

              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))

                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))

      22)


(test (interp-env (multC (numC 10 ) (appC 'quadruple (plusC (numC 1) (numC 2))))

              mt-env

              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))

                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))

      120)


(test (interp-env (multC (numC 10 ) (appC 'double (plusC (numC 1) (numC 2))))

              mt-env

              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))

                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))

      60)

(test (interp-env (parse (number->s-exp 3)) mt-env empty) 3)

 


(define (eval (sexp : s-expression)) : number
  
  (interp-env (parse sexp) mt-env FuncList))

(test (eval '(+ 3 4)) 7)
(test (eval '(* 3 4)) 12)
(test (eval '(- 3 4)) -1)

