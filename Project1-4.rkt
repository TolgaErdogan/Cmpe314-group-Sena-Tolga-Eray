
#lang plai-typed
;; Grammar:
;; S -> ± number
;; S -> + S S
;; S -> - S S
;; S -> * S S
;; S -> ^ S S
;; S -> S
;; S -> - S
;; S -> - S 1
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
(eval (msl-add (msl-num 40) (msl-num 10)))
(eval (msl-expt (msl-num 5) (msl-num 5)))



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
         [(/) (msl-div (parse (second sl)) (parse (third sl)))]
         [(**) (msl-expt (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))


;; Definition of Exponentiation
;; number -> number
;; examples
;; (^ 3 3) -> 27
;; (^ 2 4) -> 16
(define-type mslS
  [numS (n : number)]
  [plusS (l : mslS) (r : mslS)]
  [subS (l : mslS) (r : mslS)]
  [mulS (l : mslS) (r : mslS)]
  [expS (l : mslS) (r : mslS)]
  [uminusS (e : mslS)])


(define (desugar [as : mslS]) : msl
  (type-case mslS as
    [numS (n)(msl-num n)] 
    [plusS (lhs rhs) (msl-add (desugar lhs) (desugar rhs))]
    [subS (lhs rhs) (msl-sub (desugar lhs) (desugar rhs))]
    [mulS (lhs rhs) (msl-mul (desugar lhs) (desugar rhs))]
    [expS (lhs rhs) (msl-expt (desugar lhs) (desugar rhs))]
    [uminusS (e) (msl-mul (msl-num -1) (desugar e))]))

(plusS (numS 5) (numS 7))
(desugar (plusS (numS 3) (numS 4)))


(test (desugar (numS 6)) (msl-num 6))
(test (desugar (plusS (numS 5) (numS 5))) (msl-add (msl-num 5) (msl-num 5)))
(test (desugar (subS (numS 7) (numS 7))) (msl-sub (msl-num 7) (msl-num 7)))
(test (desugar (mulS (numS 8) (numS 8))) (msl-mul (msl-num 8) (msl-num 8)))





;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 6 8) 40) -> (msl-add (msl-add (msl-num 6) (msl-num 8)) (msl-num 40))



(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 6 8) 40)) (msl-add (msl-add (msl-num 6) (msl-num 8)) (msl-num 40)))





 (msl-add (msl-num 3) (msl-num 4)) 
 (msl-mul (msl-num 3) (msl-num 4)) 
 (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)) 
 (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9)))


;; eval msl -> number
;; evaluate an msl expression
;; examples
;; (msl-num 9) -> 9
;; (msl-add (msl-num 6) (msl-num 7)) -> 13
;; (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)) -> 42



(test (eval (msl-num 9))  9)
(test (eval (msl-add (msl-num 7) (msl-num 7)))  13)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 6 8) 40)) (msl-add (msl-add (msl-num 6) (msl-num 8)) (msl-num 40)))
(test (parse '3) (msl-num 3))
(test (parse '(+ 2 5)) (msl-add (msl-num 2) (msl-num 5)))




;"Parser -> reverse polish output" 
;(output-reverse-polish (parse '(+ 99 (* 5 8))))
;(output-reverse-polish (parse '(- 10 (+ 4 5))))
;(output-reverse-polish (parse '(^ 3 (- 5 5))))
;(output-reverse-polish (parse '(+ (- 99 98) (* 5 (+ 3 8)))))
;"Parser -> evaluation" 
;"(+ 99 (* 5 8)) "(eval (parse '(+ 99 (* 5 8))))
;"(* (+ 4 5) (* 3 5))" (eval (parse '(* (+ 4 5) (* 3 5))))
;"Parser-infix -> valuation"
;"(99 + (5 * 8)) "(eval (parse-infix '(99 + (5 * 8))))
;"((4 + 5) * (3 * 5))" (eval (parse '((4 + 5) * (3 * 5))))

(define-type funcF
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : funcF)]
  [plusC (lhs : funcF) (rhs : funcF)]
  [multC (lhs : funcF) (rhs : funcF)])



;(define (parse [s : s-expression]) : msl
;  (cond
;    [(s-exp-number? s) (msl-num (s-exp->number s))]
;    [(s-exp-list? s)
;     (let ([sl (s-exp->list s)])
;       (case (s-exp->symbol (first sl))
;         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
;         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
;         [(-) (msl-sub (parse (second sl)) (parse (third sl)))]
;         [(^) (msl-exp (parse (second sl)) (parse (third sl)))]
;         [else (error 'parse "invalid list input")]))]
;    [else (error 'parse "invalid input")]))

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

; square->(* x x)
;subtract1->(- x 1)
;negative->(* x -1)
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


;Tests for subtitute:
"Test1"
(test (substitute (numC 2) 'x (numC 4))(numC 4))
"Test2"
(test (substitute (numC 5) 'x (newpars '(f (/ x x))))
      (appC 'f (multC (numC 3) (numC 3))))
"Test3"
(test (substitute (numC 5) 'y (newpars '(f (* x x))))
      (appC 'f (multC (idC 'x) (idC 'x))))
"Test4"
(test (substitute (numC 5) 'x (newpars '(- x x)))
      (multC (numC 5) (numC 5)))
"Test5"
(test(substitute (numC 4) 'x (newpars '(+ x x)))
      (plusC (numC 4) (numC 4)))

"Test6"

(test (substitute (numC 2) 'x (numC 3))(numC 3))
(test (substitute (numC 3) 'x (newpars '(f (* x x))))
      (appC 'f (multC (numC 3) (numC 3))))
(test (substitute (numC 6) 'y (newpars '(f (* x x))))
      (appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 6) 'x (newpars '(* x x)))
      (multC (numC 6) (numC 6)))
(test (substitute (numC 3) 'x (newpars '(+ x x)))
      (plusC (numC 3) (numC 3)))

"Test7"

(test (substitute (numC 1) 'x (numC 2))(numC 2))
(test (substitute (numC 2) 'x (newpars '(f (* x x))))
      (appC 'f (multC (numC 2) (numC 2))))
(test (substitute (numC 2) 'y (newpars '(f (* x x))))
      (appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 2) 'x (newpars '(* x x)))
      (multC (numC 2) (numC 2)))
(test (substitute (numC 3) 'x (newpars '(+ x x)))
      (plusC (numC 3) (numC 3)))



;;interpreter-> funcF
;; name->funcF
;;arg-> FunDefC
;;body-> number
;;example; (+ 4 5)->9
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

;Tests of interpreter:
"Test1"
"Test = ((+ 4 5) -> 9"
(test (interpre (newpars '(+ 4 5)) empty) 9)
(interpre (newpars '(+ 4 5)) empty)
"Test = (( * 2 3) -> 6)"
(test (interpre (newpars '(* 2 3)) empty) 6)
(interpre (newpars '(* 2 3)) empty)
"Test = ((* 5 5) -> 25)"
(test (interpre (newpars '(square 5)) FuncDefName) 25)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 3)) FuncDefName) -3)
(interpre (newpars '(negative 3)) FuncDefName)





"Test2"
"Test = ((+ 3 5) -> 8)"
(test (interpre (newpars '(+ 3 5)) empty) 8)
(interpre (newpars '(+ 2 4)) empty)
"Test = ((* 5 2 ) -> 10)"
(test (interpre (newpars '(* 5 2)) empty) 10)
(interpre (newpars '(* 5 2)) empty)
"Test = ((* 8 8) -> 64)"
(test (interpre (newpars '(square 8)) FuncDefName) 64)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 1)) FuncDefName) -1)
(interpre (newpars '(negative 1)) FuncDefName)



"Test3"
"Test = ((+ 1 2) -> 3)"
(test (interpre (newpars '(+ 1 2)) empty) 3)
(interpre (newpars '(+ 3 5)) empty)
"Test = ((* 3 4 ) -> 12)"
(test (interpre (newpars '(* 3 4)) empty) 12)
(interpre (newpars '(* 3 4)) empty)
"Test = ((* 7 7) -> 49)"
(test (interpre (newpars '(square 7)) FuncDefName) 49)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 6)) FuncDefName) -6)
(interpre (newpars '(negative 6)) FuncDefName)



"Test4"
"Test = ((+ 2 3) -> 5)"
(test (interpre (newpars '(+ 2 3)) empty) 5)
(interpre (newpars '(+ 2 4)) empty)
"Test = ((* 1 5 ) -> 5)"
(test (interpre (newpars '(* 1 5)) empty) 5)
(interpre (newpars '(* 1 5)) empty)
"Test = ((* 9 9) -> 81)"
(test (interpre (newpars '(square 9)) FuncDefName) 81)
(interpre (newpars '(square 2)) FuncDefName)
(test (interpre (newpars '(negative 9)) FuncDefName) -9)
(interpre (newpars '(negative 9)) FuncDefName)

