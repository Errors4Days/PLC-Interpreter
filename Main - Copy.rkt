#lang racket
; Elizabeth Shumaker and Justin Lee
; TODO:
; abstract bottom of M-declare

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

;combines int_val and leftoperand because of it's frequency
(define left_val
  (lambda (val vars)
    (int_val (leftoperand val) vars)))

;combines int_val and rightoperand because of it's frequency
(define right_val
  (lambda (val vars)
    (int_val (rightoperand val) vars)))

; Checks if an atom is in a list
(define member*?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(pair? (car lis)) (or (member*? a (car lis))
                             (member*? a (cdr lis)))]
      [(eq? a (car lis)) #t]
      [else (member*? a (cdr lis))])))

; Checks if a var is declared at all
(define declare?
  (lambda (aVar vars)
    (cond
      [(null? vars) #f]
      [(member*? aVar (unbox (car vars))) #t]
      [else (declare? aVar (cdr vars))])))
; Checks if a var is declared on the lowest layer
(define declareStack?
  (lambda (aVar vars)
    (cond
      [(null? vars) #f]
      [(member*? aVar (unbox (car vars))) #t]
      [else #f])))

; Replaces a variable value
(define insert-cps
  (lambda (var value varlis valuelis return)
    (cond
      ;[(null? varlis) (error 'variable-not-declared)]
      [(null? value) (error 'cannot-assign-null-to-a-variable)]
      [(eq? var (car varlis)) (return (cons value (cdr valuelis)))]
      [else (insert-cps var value (cdr varlis) (cdr valuelis)
                        (lambda (v) (return (cons (car valuelis) v))))])))
(define insert
  (lambda (var value lis)
    (cond
      [(null? lis) (error 'variable-not-declared)]
      [(declareStack? var lis) (insert-cps var value (car (unbox (car lis))) (cadr (unbox (car lis)))
                                           (lambda (v) (cons (box (cons (car (unbox (car lis))) (list v))) (cdr lis))))]
      [else (cons (car lis) (insert var value (cdr lis)))])))
       
; Get variable value
(define getValue
  (lambda (varA vars)
    (cond
      [(null? vars) (error 'variable-not-declared)]
      [(not (null? (getValue-helper varA (unbox (car vars)))))
       (getValue-helper varA (unbox (car vars)))]
      [else (getValue varA (cdr vars))])))
(define getValue-helper
  (lambda (varName lis)
    (cond
      [(null? (car lis)) '()]
      [(and (eq? varName (caar lis)) (null? (caadr lis)))
       (error 'variable-not-assigned)]
      [(eq? varName (caar lis)) (caadr lis)]
      [else (getValue-helper varName (mymap-caller cdr lis))])))

; Applies a function on all elements of a 2d list using cps
(define mymap
  (lambda (func lis return)
    (if (null? lis)
        (return '())
        (mymap func (cdr lis) (lambda (v) (return (append (cons (func (car lis)) '()) v)))))))
(define mymap-caller
  (lambda (func lis)
    (mymap func lis (lambda (v) v))))

; Takes in variable or value and returns a value
(define int_val
  (lambda (val vars)
    (cond
      [(number? val) val]
      [(declare? val vars)(getValue val vars)]
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
       (not (M-evaluate (leftoperand expression) vars))]
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
      ; Error redefining
      [(declareStack? (leftoperand expression) vars) (error 'redefining-variable)]
      ; Declaration and assignment
      [(not (null? (cddr expression)))
       (M-assign (cons '= (list (leftoperand expression)
                                (M-evaluate (rightoperand expression) vars)))
                 (M-declare (list 'var (leftoperand expression)) vars))]
      ; Standard declaration
      [else (cons (box (cons (cons (leftoperand expression) (car (unbox (car vars))))
                             (list (cons '() (cadr (unbox (car vars))))))) (cdr vars))])))

; Maps variables with values
; '(= x value/expression)
(define M-assign
  (lambda (expression vars)
    (insert (leftoperand expression) (M-evaluate (rightoperand expression) vars) vars)))

; if statements
; '(if (> x y) (return x)
(define M-if
  (lambda (expression next outer vars returns)
    (cond
      [(and (eq? (operator expression) 'if) (M-evaluate (leftoperand expression) vars))
       (M-if (rightoperand expression) next outer vars returns)] ; if condition is true, run body
      [(and (eq? (operator expression) 'if) (null? (lastQuadruple expression)))
       (M-state next outer vars returns)]                        ; No else statement
      [(eq? (operator expression) 'if)
       (M-if (cadddr expression) next outer vars returns)]       ; Run else/ else if
      [(eq? (operator expression) 'return)
       (M-return (cadr expression) vars returns)]                ; Runs return function
      [(eq? (operator expression) '=)
       (M-state next outer (M-assign expression vars) returns)]  ; Executes variable assignment values
      [(eq? (operator expression) 'begin)
       (M-state next outer (M-begin expression outer vars returns) returns)] ; Runs a bracket
      [(eq? (operator expression) 'break)
       (M-state '((break)) outer vars returns)]
       [(eq? (operator expression) 'continue)
       (M-state '((continue)) outer vars returns)]
      [else (error 'invalid-if-statement)])))

; while statements
; '(while (!= (% y x) 3) (= y (+ y 1)))
(define M-while
  (lambda (expression next vars returns)
    (cond
      [(and (eq? (operator expression) 'while) (M-evaluate (leftoperand expression) vars))
       (call/cc (lambda (k)
                  (M-while expression next
                           (M-state (list (rightoperand expression)) next vars k) returns)))]; Run body
      [(eq? (operator expression) 'while) (M-state next '() vars returns)]                  ; Exit loop
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
  (lambda (expression vars returns)
    (cond
      [(list? expression) (M-return (M-evaluate expression vars) vars returns)] ; Expression not evaluated
      [(number? expression) (returns expression)] ; Given a number
      [(or (eq? expression #t) (eq? expression 'true)) (returns 'true)] ; Given a boolean
      [(or (eq? expression #f) (eq? expression 'false)) (returns 'false)] ; Given a boolean
      [else (M-return (getValue expression vars) vars returns)]))) ; Given a variable

; Runs the bracket code and discards the lowest layer
(define M-begin
  (lambda (expression outer vars returns)
    (cond
      [(eq? (operator expression) 'begin) (M-begin (cdr expression) outer vars returns)]
      [else (cdr (M-state expression outer (cons (box '(()())) vars) returns))])))

; Variables stored as '((x 3) (y) (i 7)) in vars
(define M-state
  (lambda (expression outer vars returns)
    (cond
      [(null? expression) vars]
      [(and (null? expression) (null? vars) (null? outer)) (error 'break-not-in-loop)]
      [(eq? (operator (nextExecute expression)) 'return)
       (M-return (getExpression expression) vars returns)]
      [(eq? (operator (nextExecute expression)) 'var)
       (M-state (remainderExpression expression) outer
                (M-declare (nextExecute expression) vars) returns)]
      [(eq? (operator (nextExecute expression)) '=)
       (M-state (remainderExpression expression) outer
                (M-assign (nextExecute expression) vars) returns)]
      [(eq? (operator (nextExecute expression)) 'if)
       (M-if (nextExecute expression) (remainderExpression expression) outer vars returns)]
      [(eq? (operator (nextExecute expression)) 'while)
       (M-while (nextExecute expression)(remainderExpression expression) vars returns)]
      [(eq? (operator (nextExecute expression)) 'begin)
       (M-state (remainderExpression expression) outer
                (M-begin (nextExecute expression) outer vars returns) returns)]
      [(eq? (operator (nextExecute expression)) 'break)
       (returns (M-state outer '() (cdr vars) returns))]
      [(eq? (operator (nextExecute expression)) 'continue)
       vars])))

;;; *******************************
;;; INTERPRETER FUNCTION
;;; *******************************
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (returns)
       (M-state (parser filename) '() (list (box '(()()))) returns)))))


;;; *******************************
;;; Provided Test Cases
;;; *******************************
;(interpret "temp.txt")

; Part 1
#|
(interpret "Tests/Test1") ; 150
(interpret "Tests/Test2") ; -4
(interpret "Tests/Test3") ; 10
(interpret "Tests/Test4") ; 16
(interpret "Tests/Test5") ; 220
(interpret "Tests/Test6") ; 5
(interpret "Tests/Test7") ; 6
(interpret "Tests/Test8") ; 10
(interpret "Tests/Test9") ; 5
(interpret "Tests/Test10") ; -39 |#

; (interpret "Tests/Test11") ; error using before declaring
; (interpret "Tests/Test12") ; error variable not declared
; (interpret "Tests/Test13") ; error using before assigning
; (interpret "Tests/Test14") ; error redefining variable

#|
(interpret "Tests/Test15") ; true
(interpret "Tests/Test16") ; 100
(interpret "Tests/Test17") ; false
(interpret "Tests/Test18") ; true
(interpret "Tests/Test19") ; 128
(interpret "Tests/Test20") ; 12 |#

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

;;; TESTS FOR INTERPRETER PT2
#|
(interpret "Tests2/Test1")    ;20
(interpret "Tests2/Test2")    ;164
(interpret "Tests2/Test3")    ;32
(interpret "Tests2/Test4")    ;2
;(interpret "Tests2/Test5")    ;Error
(interpret "Tests2/Test6")    ;25
(interpret "Tests2/Test7")    ;21
(interpret "Tests2/Test8")    ;6
(interpret "Tests2/Test9")    ;-1 |#
(interpret "Tests2/Test10")  ;789
;(interpret "Tests2/Test11")  ;Error
;(interpret "Tests2/Test12")  ;Error
;(interpret "Tests2/Test13")   ;Error
;(interpret "Tests2/Test14")   ;12
#|(interpret "Tests2/Test15")   ;125
(interpret "Tests2/Test16")   ;110
(interpret "Tests2/Test17")   ;2000400
(interpret "Tests2/Test18")   ;101
;(interpret "Tests2/Test19")   ;Error |#