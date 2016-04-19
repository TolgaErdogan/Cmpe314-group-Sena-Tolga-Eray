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
[factaccC (x : number) (acc : number)])


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

(define (parse [s :  (listof s-expression)]) : ExprC
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
     [factaccC (x fact) (factaccC (subst what for x) (subst what for fact))]))


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
               [else (* x (interp (factC (- x 1)) fds))])]))
   [factaccC (x acc) (cond
                       [ (= x 1) acc]
                       [else ( interp (factaccC (- x 1) (* x acc)) fds )])]))

     

 
 
 ;Tests 
(test(interp(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
(test(interp(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
(test(interp(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
(test(interp(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
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
                       [else ( interp1 (factaccC (- x 1) (* x acc)) fds )])]))


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

