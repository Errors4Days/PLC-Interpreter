#lang racket
; Elizabeth Shumaker and Justin Lee


(require "simpleParser.rkt")
;;; ABSTRACTION/HELPERS
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define getReturnExpression cadar)
(define operands cdr)
(define member*?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(pair? (car lis)) (or (member*? a (car lis)) (member*? a (cdr lis)))]
      [(eq? a (car lis)) #t]
      [else (member*? a (cdr lis))])))
; Inserts a varaible or a variable number pair
(define insert
  (lambda (og new lis)
    (cond
      [(null? lis) (error 'variable-not-declared)]
      [(member*? og (car lis)) (cons (append (cons og '()) (cons new '())) (cdr lis))]
      [else (cons (car lis) (insert og new (cdr lis)))])))
; Get variable value
(define getValue
  (lambda (varName lis)
    (cond
      [(null? lis) (error 'variable-not-declared)]
      [(and (member*? varName (car lis)) (null? (cdar lis))) (error 'variable-not-assigned)]
      [(member*? varName (car lis)) (cadr (car lis))]
      [else (getValue varName (cdr lis))])))
; Takes in variable or value and returns a value
(define int_val
  (lambda (val vars)
    (cond
      [(number? val) val]
      [(member*? val vars)(getValue val vars)]
      [else val])))
; Takes two atoms and returns them in a list
(define listMaker
  (lambda (a b)
    (cons a (cons b '()))))


;;; Main state operators
; M-integer maps expressions to integer values
; operators: +, -, *, /, %
(define M-integer
  (lambda (expression vars)
    (cond
      [(number? expression) expression]
      [(and (null? (cddr expression)) (eq? (operator expression) '-)) (- 0
                                         (M-integer (int_val(leftoperand expression) vars)vars))]
      [(eq? (operator expression) '+) (+ (M-integer (int_val(leftoperand expression) vars)vars)
                                         (M-integer (int_val(rightoperand expression)vars)vars))]
      [(eq? (operator expression) '-) (- (M-integer (int_val(leftoperand expression) vars)vars)
                                         (M-integer (int_val(rightoperand expression) vars)vars))]
      [(eq? (operator expression) '*) (* (M-integer (int_val(leftoperand expression)vars)vars)
                                         (M-integer (int_val(rightoperand expression)vars)vars))]
      [(eq? (operator expression) '/) (quotient (M-integer (int_val(leftoperand expression)vars)vars)
                                                (M-integer (int_val(rightoperand expression)vars)vars))]
      [(eq? (operator expression) '%) (modulo (M-integer (int_val(leftoperand expression)vars)vars)
                                              (M-integer (int_val(rightoperand expression)vars)vars))]
      [else (error 'bad-math-operator)])))

; M-compare maps comparisons to boolean values
; operators: ==, !=, <, >, <=. >=
(define M-compare
  (lambda (expression vars)
    (cond
      [(boolean? expression) expression]
      [(eq? (operator expression) '<) (< (M-evaluate (int_val(leftoperand expression)vars) vars)
                                         (M-evaluate (int_val(rightoperand expression)vars) vars))]
      [(eq? (operator expression) '<=) (<= (M-evaluate (int_val(leftoperand expression)vars) vars)
                                           (M-evaluate (int_val(rightoperand expression)vars) vars))]
      [(eq? (operator expression) '>) (> (M-evaluate (int_val(leftoperand expression)vars) vars)
                                          (M-evaluate (int_val(rightoperand expression)vars) vars))]
      [(eq? (operator expression) '>=) (>= (M-evaluate (int_val(leftoperand expression)vars) vars)
                                           (M-evaluate (int_val(rightoperand expression)vars) vars))]
      [(eq? (operator expression) '==) (eq? (M-evaluate (int_val(leftoperand expression)vars) vars)
                                           (M-evaluate (int_val(rightoperand expression)vars) vars))]
      [(eq? (operator expression) '!=) (not (eq? (M-evaluate (int_val(leftoperand expression)vars) vars)
                                           (M-evaluate (int_val(rightoperand expression)vars) vars)))]
      [else (error 'bad-compare-operator)])))

; M-bool maps boolean expressions to boolean values and converts strings to booleans
; operators: &&, ||, !
(define M-bool
  (lambda (expression vars)
    (cond
      [(boolean? expression) expression] ; if already boolean, return boolean
      [(eq? expression 'true) #t] ; if true, return #t
      [(eq? expression 'false) #f] ; if false, return #f
      [(and (not (list? expression)) (boolean? (getValue expression vars))) (getValue expression vars)] ; if variable and variable is boolean, return boolean
      [(and (eq? (operator expression) '!) (boolean? (M-evaluate (leftoperand expression) vars)))
       (not (M-bool (leftoperand expression) vars))]  ; if ! statement and boolean value, return opposite
      [(not (boolean? (M-evaluate (leftoperand expression) vars))) (error 'input-not-boolean)] ; if leftoperand not boolean, error
      [(not (boolean? (M-evaluate (rightoperand expression) vars))) (error 'input-not-boolean)] ; if rightoperand not boolean, error
      [(eq? (operator expression) '&&) (and (M-evaluate (leftoperand expression) vars)
                                            (M-evaluate (rightoperand expression) vars))] ; if &&, return (and leftoperand rightoperand)
      [(eq? (operator expression) '||) (or (M-evaluate (leftoperand expression) vars)
                                           (M-evaluate (rightoperand expression) vars))] ; if ||, return (or leftoperand rightoperand)
      [else (error 'bad-boolean-operator)])))

; Variable declaration
; '(var x) or '(var x (expression))
(define M-declare
  (lambda (expression vars)
    (cond
      [(not (null? (cddr expression))) (M-assign (cons '= (listMaker (leftoperand expression) (M-evaluate (rightoperand expression) vars)))
                                                                (M-declare (listMaker 'var (leftoperand expression)) vars))] ; Declaration with an assignment
      [(member*? (leftoperand expression) vars) (error 'redefining-variable)] ; 
      [else (cons (cons (leftoperand expression) '()) vars)])))

; Maps variables with values
; '(= x value/expression)
(define M-assign
  (lambda (expression vars)
    (insert (leftoperand expression) (M-evaluate (rightoperand expression) vars) vars)))

; if statements
; '(if (> x y) (return x)
(define M-if
  (lambda (expression next vars)
    (cond
      [(and (eq? (operator expression) 'if) (M-evaluate (leftoperand expression) vars)) (M-if (rightoperand expression) next vars)] ; if condition is true, run body
      [(and (eq? (operator expression) 'if) (null? (cdddr expression))) (M-state next vars)] ; No else statement
      [(eq? (operator expression) 'if) (M-if (cadddr expression) next vars)] ;Run else/ else if
      [(eq? (operator expression) 'return) (M-return (cadr expression) vars)] ; Finds and runs return function
      [(eq? (operator expression) '=) (M-state next (M-assign expression vars))] ; Executes variable assignment values
      [else (error 'invalid-if-statement)])))

; while statements
; '(while (!= (% y x) 3) (= y (+ y 1)))
(define M-while
  (lambda (expression next vars)
    (cond
      [(and (eq? (operator expression) 'while) (M-evaluate (leftoperand expression) vars))
       (M-while expression next (M-while (rightoperand expression) next vars))] ; Enter loop, run body
      [(eq? (operator expression) 'while) (M-state next vars)] ; Exit loop
      [(eq? (operator expression) '=) (M-assign expression vars)] ; Body has assignment, runs assign
      [else (error 'invalid-while-loop)])))
      

; Passes the expression into the correct function for evaluation
(define M-evaluate
  (lambda (expression vars)
    (cond
      [(or (eq? expression 'false) (eq? expression 'true)) (M-bool expression vars)] ; Given a single boolean
      [(and (not (list? expression)) (number? expression)) expression]
      [(and (not (list? expression)) (boolean? expression)) expression]
      [(not (list? expression)) (getValue expression vars)] ; Given an atom that is a variable or value
      [(member*? (operator expression) '(+ - * / %)) (M-integer expression vars)] ; Math expression
      [(member*? (operator expression) '(== != < > <= >=)) (M-compare expression vars)] ; Comparison expression
      [(member*? (operator expression) '( && || !)) (M-bool expression vars)] ; Boolean expression
      [else (error 'no-valid-operator)])))

; Returns a value or boolean
; '(M-return 'x)
(define M-return
  (lambda (expression vars)
    (cond
      [(list? expression) (M-return (M-evaluate expression vars) vars)] ; Expression hasn't been evaluated
      [(number? expression) expression] ; Given a number
      [(eq? expression #t) 'true] ; Given a boolean
      [(eq? expression #f) 'false] ; Given a boolean
      [else (M-return (getValue expression vars) vars)]))) ; Given a variable

; Variables stored as '((x 3) (y) (i 7)) in vars
(define M-state
  (lambda (expression vars)
    (cond
      [(null? expression) 'void]
      [(eq? (operator (car expression)) 'return) (M-return (getReturnExpression expression) vars)]
      [(eq? (operator (car expression)) 'var) (M-state (cdr expression) (M-declare (car expression) vars))]
      [(eq? (operator (car expression)) '=) (M-state (cdr expression) (M-assign (car expression) vars))]
      [(eq? (operator (car expression)) 'if) (M-if (car expression) (cdr expression) vars)]
      [(eq? (operator (car expression)) 'while) (M-while (car expression) (cdr expression) vars)])))

; Initializes M-state with an empty variable list
(define M-state-start
  (lambda (expression)
    (M-state expression '())))


;Provided Test Cases
#|
(M-state-start (parser "Tests/Test1"))
(M-state-start (parser "Tests/Test2"))
(M-state-start (parser "Tests/Test3"))
(M-state-start (parser "Tests/Test4"))
(M-state-start (parser "Tests/Test5"))
(M-state-start (parser "Tests/Test6"))
(M-state-start (parser "Tests/Test7"))
(M-state-start (parser "Tests/Test8"))
(M-state-start (parser "Tests/Test9"))
(M-state-start (parser "Tests/Test10")) |#

;(M-state-start (parser "Tests/Test11")) ; error using before declaring
; (M-state-start (parser "Tests/Test12")) ; error variable not declared
; (M-state-start (parser "Tests/Test13")) ; error using before assigning
; (M-state-start (parser "Tests/Test14")) ; error redefining variable

#|
(M-state-start (parser "Tests/Test15"))
(M-state-start (parser "Tests/Test16")) 
(M-state-start (parser "Tests/Test17"))
(M-state-start (parser "Tests/Test18"))
(M-state-start (parser "Tests/Test19"))
(M-state-start (parser "Tests/Test20")) |#

