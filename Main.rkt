#lang racket
; Elizabeth Shumaker and Justin Lee

(require "simpleParser.rkt")
;;; *******************************
;;; ABSTRACTION/HELPERS
;;; *******************************
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define getExpression cadar)

(define operands cdr)
(define lastTriple cddr)
(define lastQuadruple cdddr)
(define nextExecute car)
(define remainderExpression cdr)

#|
Changed for part 2 of interpreter proj
insertion, declaration, assign

Still need to change
Everything Else and Assignment requirements
|#

;combines int_val and leftoperand because it's frequency
(define left_val
  (lambda (val vars)
    (int_val (leftoperand val) vars)))
;combines int_val and rightoperand because it's frequency
(define right_val
  (lambda (val vars)
    (int_val (rightoperand val) vars)))

(define member*?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(pair? (car lis)) (or (member*? a (car lis))
                             (member*? a (cdr lis)))]
      [(eq? a (car lis)) #t]
      [else (member*? a (cdr lis))])))
; Inserts a varaible or a variable number pair
; (insert 'x '45 '((a x c) (1 2 3))) => ((a x c) (1 45 3))
(define insert-cps
  (lambda (var value varlis valuelis return)
    (cond
      [(null? varlis) (error 'variable-not-declared)]
      [(null? value) (error 'cannot-assign-null-to-a-variable)]
      ;[(and (eq? var (car varlis)) (null? valuelis)) (return (list value))]
      [(eq? var (car varlis)) (return (cons value (cdr valuelis)))]
      [else (insert-cps var value (cdr varlis) (cdr valuelis)
                       (lambda (v) (return (cons (car valuelis) v))))])))
(define insert
  (lambda (var value lis)
    (insert-cps var value (car lis) (cadr lis) (lambda (v) (cons (car lis) (list v))))))

; Get variable value
(define getValue
  (lambda (varName lis)
    (cond
      [(null? lis) (error 'variable-not-declared)]
      [(and (member*? varName (car lis)) (null? (cdar lis)))
       (error 'variable-not-assigned)]
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


;;; *******************************
;;; MAIN OPERATORS
;;; *******************************

; M-integer maps expressions to integer values
; operators: +, -, *, /, %
(define M-integer
  (lambda (expression vars)
    (cond
      [(number? expression) expression]
      [(and (null? (lastTriple expression)) (eq? (operator expression) '-))
       (- 0 (M-integer (left_val expression vars) vars))]
      [(eq? (operator expression) '+) (+ (M-integer (left_val expression vars) vars)
                                         (M-integer (right_val expression vars) vars))]
      [(eq? (operator expression) '-) (- (M-integer (left_val expression vars) vars)
                                         (M-integer (right_val expression vars) vars))]
      [(eq? (operator expression) '*) (* (M-integer (left_val expression vars) vars)
                                         (M-integer (right_val expression vars) vars))]
      [(eq? (operator expression) '/) (quotient (M-integer (left_val expression vars) vars)
                                                (M-integer (right_val expression vars) vars))]
      [(eq? (operator expression) '%) (modulo (M-integer (left_val expression vars) vars)
                                              (M-integer (right_val expression vars) vars))]
      [else (error 'bad-math-operator)])))

; M-compare maps comparisons to boolean values
; operators: ==, !=, <, >, <=. >=
(define M-compare
  (lambda (expression vars)
    (cond
      [(boolean? expression) expression]
      [(eq? (operator expression) '<) (< (M-evaluate (left_val expression vars) vars)
                                         (M-evaluate (right_val expression vars) vars))]
      [(eq? (operator expression) '<=) (<= (M-evaluate (left_val expression vars) vars)
                                           (M-evaluate (right_val expression vars) vars))]
      [(eq? (operator expression) '>) (> (M-evaluate (left_val expression vars) vars)
                                          (M-evaluate (right_val expression vars) vars))]
      [(eq? (operator expression) '>=) (>= (M-evaluate (left_val expression vars) vars)
                                           (M-evaluate (right_val expression vars) vars))]
      [(eq? (operator expression) '==) (eq? (M-evaluate (left_val expression vars) vars)
                                           (M-evaluate (right_val expression vars) vars))]
      [(eq? (operator expression) '!=) (not (eq? (M-evaluate (left_val expression vars) vars)
                                                 (M-evaluate (right_val expression vars) vars)))]
      [else (error 'bad-compare-operator)])))

; M-bool maps boolean expressions to boolean values and converts strings to booleans
; operators: &&, ||, !
(define M-bool
  (lambda (expression vars)
    (cond
      [(boolean? expression) expression] ; if already boolean, return boolean
      [(eq? expression 'true) #t]        ; if true, return #t
      [(eq? expression 'false) #f]       ; if false, return #f
      [(and (not (list? expression)) (boolean? (getValue expression vars)))
       (getValue expression vars)]       ; if variable and variable is boolean, return boolean
      ; if ! statement and boolean value, return opposite
      [(and (eq? (operator expression) '!) (boolean? (M-evaluate (leftoperand expression) vars)))
                                           (not (M-bool (leftoperand expression) vars))]
      [(not (boolean? (M-evaluate (leftoperand expression) vars)))
       (error 'input-not-boolean)]       ; if leftoperand not boolean, error
      [(not (boolean? (M-evaluate (rightoperand expression) vars)))
       (error 'input-not-boolean)]       ; if rightoperand not boolean, error
      ; if &&, return (and leftoperand rightoperand)
      [(eq? (operator expression) '&&) (and (M-evaluate (leftoperand expression) vars)
                                            (M-evaluate (rightoperand expression) vars))]
      ; if ||, return (or leftoperand rightoperand)
      [(eq? (operator expression) '||) (or (M-evaluate (leftoperand expression) vars)
                                           (M-evaluate (rightoperand expression) vars))]
      [else (error 'bad-boolean-operator)])))

; Variable declaration
; '(var x) or '(var x (expression))
(define M-declare
  (lambda (expression vars)
    (cond
      ; Declaration and assignment
      [(not (null? (cddr expression)))
       (M-assign (cons '= (list (leftoperand expression)
                                     (M-evaluate (rightoperand expression) vars)))
                 (M-declare (list 'var (leftoperand expression)) vars))]
      ; Error redefining
      [(member*? (leftoperand expression) (car vars)) (error 'redefining-variable)]
      ; Standard declaration
      [else (cons (cons (leftoperand expression) (car vars)) (list (cons '() (cadr vars))))])))

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
      [(and (eq? (operator expression) 'if) (M-evaluate (leftoperand expression) vars))
       (M-if (rightoperand expression) next vars)] ; if condition is true, run body
      [(and (eq? (operator expression) 'if) (null? (lastQuadruple expression)))
       (M-state next vars)]                        ; No else statement
      [(eq? (operator expression) 'if)
       (M-if (cadddr expression) next vars)]       ; Run else/ else if
      [(eq? (operator expression) 'return)
       (M-return (cadr expression) vars)]          ; Finds and runs return function
      [(eq? (operator expression) '=)
       (M-state next (M-assign expression vars))]  ; Executes variable assignment values
      [else (error 'invalid-if-statement)])))

; while statements
; '(while (!= (% y x) 3) (= y (+ y 1)))
(define M-while
  (lambda (expression next vars)
    (cond
      [(and (eq? (operator expression) 'while) (M-evaluate (leftoperand expression) vars))
       (M-while expression next (M-while (rightoperand expression) next vars))] ; Enter loop, run body
      [(eq? (operator expression) 'while) (M-state next vars)]                  ; Exit loop
      [(eq? (operator expression) '=) (M-assign expression vars)] ; Body has assignment, runs assign
      [else (error 'invalid-while-loop)])))
      

; Passes the expression into the correct function for evaluation
(define M-evaluate
  (lambda (expression vars)
    (cond
      [(or (eq? expression 'false) (eq? expression 'true)) (M-bool expression vars)] ; Single boolean
      [(and (not (list? expression)) (number? expression)) expression]
      [(and (not (list? expression)) (boolean? expression)) expression]
      [(not (list? expression)) (getValue expression vars)] ; atom that is a variable or value
      [(member*? (operator expression) '(+ - * / %)) (M-integer expression vars)] ; Math expression
      [(member*? (operator expression) '(== != < > <= >=)) (M-compare expression vars)] ; Comparison
      [(member*? (operator expression) '( && || !)) (M-bool expression vars)] ; Boolean expression
      [else (error 'no-valid-operator)])))

; Returns a value or boolean
; '(M-return 'x)
(define M-return
  (lambda (expression vars)
    (cond
      [(list? expression) (M-return (M-evaluate expression vars) vars)] ; Expression not evaluated
      [(number? expression) expression] ; Given a number
      [(or (eq? expression #t) (eq? expression 'true)) 'true] ; Given a boolean
      [(or (eq? expression #f) (eq? expression 'false)) 'false] ; Given a boolean
      [else (M-return (getValue expression vars) vars)]))) ; Given a variable

; Variables stored as '((x 3) (y) (i 7)) in vars
(define M-state
  (lambda (expression vars)
    (cond
      [(null? expression) 'void]
      [(eq? (operator (nextExecute expression)) 'return)
       (M-return (getExpression expression) vars)]
      [(eq? (operator (nextExecute expression)) 'var)
       (M-state (remainderExpression expression) (M-declare (nextExecute expression) vars))]
      [(eq? (operator (nextExecute expression)) '=)
       (M-state (remainderExpression expression) (M-assign (nextExecute expression) vars))]
      [(eq? (operator (nextExecute expression)) 'if)
       (M-if (nextExecute expression) (remainderExpression expression) vars)]
      [(eq? (operator (nextExecute expression)) 'while)
       (M-while (nextExecute expression) (remainderExpression expression) vars)]
      [(eq? (operator (nextExecute expression)) 'begin)
       (M-state (nextExecute expression))])))


;;; *******************************
;;; INTERPRETER FUNCTION
;;; *******************************
(define interpret
  (lambda (filename)
    (M-state (parser filename) '() )))


;;; *******************************
;;; Provided Test Cases
;;; *******************************
(parser "temp.txt")

; Part 1
#|
(interpret "Tests/Test1")
(interpret "Tests/Test2")
(interpret "Tests/Test3")
(interpret "Tests/Test4")
(interpret "Tests/Test5")
(interpret "Tests/Test6")
(interpret "Tests/Test7")
(interpret "Tests/Test8")
(interpret "Tests/Test9")
(interpret "Tests/Test10")|#

; (interpret "Tests/Test11") ; error using before declaring
; (interpret "Tests/Test12") ; error variable not declared
; (interpret "Tests/Test13") ; error using before assigning
; (interpret "Tests/Test14") ; error redefining variable

#|
(interpret "Tests/Test15")
(interpret "Tests/Test16") 
(interpret "Tests/Test17")
(interpret "Tests/Test18")
(interpret "Tests/Test19")
(interpret "Tests/Test20")|#

;;; SELF MADE TEST CASES
#|
(interpret "Tests/Test30")     ;output should be 82
(interpret "Tests/Test31")     ;output should be 100
(interpret "Tests/Test32")     ;output should be 'true
(interpret "Tests/Test33")     ;output should be 'false
(interpret "Tests/Test34")     ;output should be 107
(interpret "Tests/Test35")     ;output should be 0
; (interpret "Tests/Test36")    ;throws error undeclared
; (interpret "Tests/Test37")    ;throws error invalid if
(interpret "Tests/Test38")     ;output should be 100 |#