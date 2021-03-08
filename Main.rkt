#lang racket
; Elizabeth Shumaker and Justin Lee


(require "simpleParser.rkt")
; Abstraction and helpers
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define getExpression cadar)
(define member*?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(pair? (car lis)) (or (member*? a (car lis)) (member*? a (cdr lis)))]
      [(eq? a (car lis)) #t]
      [else (member*? a (cdr lis))])))
; Finds a subarray and replaces it (replace '(z) '((a 4) (d 3223) (z 3)))
(define replace
  (lambda (og new lis)
    (cond
      [(null? lis) (cons new '())]
      [(member*? og (car lis)) (cons new (cdr lis))]
      [else (cons (car lis) (replace og new (cdr lis)))])))
; Same as replace but throws an error if the variable can't be found
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
      [(member*? varName (car lis)) (cadr (car lis))]
      [else (getValue varName (cdr lis))])))
; Gets value from variable or value
(define int_val
  (lambda (val vars)
    (cond
      [(number? val) val]
      [(member*? val vars)(getValue val vars)]
      [else val])))



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
      [else (error 'bad-operator)])))

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
      [else (error 'bad-operator)])))

; M-bool maps boolean expressions to boolean values and converts strings to booleans, True -> #t
; operators: &&, ||, !
(define M-bool
  (lambda (expression vars)
    (cond
      [(boolean? expression) expression]
      [(eq? expression 'true) #t]
      [(eq? expression 'false) #f]
      [(and (not (list? expression)) (boolean? (getValue expression vars))) (getValue expression vars)]
      [(and (eq? (operator expression) '!) (boolean? (M-evaluate (leftoperand expression) vars))) (not (M-bool (leftoperand expression) vars))]
      [(not (boolean? (M-evaluate (leftoperand expression) vars))) (error 'input-not-boolean)]
      [(not (boolean? (M-evaluate (rightoperand expression) vars))) (error 'input-not-boolean)]
      [(eq? (operator expression) '&&) (and (M-evaluate (leftoperand expression) vars)
                                            (M-evaluate (rightoperand expression) vars))]
      [(eq? (operator expression) '||) (or (M-evaluate (leftoperand expression) vars)
                                           (M-evaluate (rightoperand expression) vars))]
      [else (error 'bad-operator)])))

; Variable declaration
; '(var x) or '(var x (expression))
(define M-declare
  (lambda (expression vars)
    (cond
      [(not (null? (cddr expression))) (M-assign (cons '= (cons (cadr expression) (cons (M-evaluate (caddr expression) vars)'())))
                                                                (M-declare (cons 'var (cons (cadr expression) '())) vars))] ;Abstract
      [(member*? (leftoperand expression) vars) (error 'redefining-variable)]
      [else (cons (cons (leftoperand expression) '()) vars)])))

; Maps variables with values
; '(= x value/expression)
(define M-assign
  (lambda (expression vars)
    (insert (cadr expression) (M-evaluate (car (cddr expression)) vars) vars)))

; if statements
(define M-if
  (lambda (expression next vars)
    (cond
      ;[(not (boolean? (M-evaluate (car (cdr expression)) vars))) (error 'input-not-boolean)]
      [(and (eq? (car expression) 'if) (M-evaluate (cadr expression) vars)) (M-if (caddr expression) next vars)] ; if statement true
      [(and (eq? (car expression) 'if) (null? (cdddr expression))) (M-state next vars)]
      [(eq? (car expression) 'if) (M-if (cadddr expression) next vars)] ;run else/if
      [(eq? (operator expression) 'return) (M-return (cadr expression) vars)] ; run return function
      [(eq? (operator expression) '=) (M-state next (M-assign expression vars))] ;to assign values
      [else (error 'invalid-if-statement)])))

; Passes the expression into the correct function for evaluation
(define M-evaluate
  (lambda (expression vars)
    (cond
      [(or (eq? expression 'false) (eq? expression 'true)) (M-bool expression vars)]
      [(not (list? expression)) (int_val expression vars)]
      [(member*? (operator expression) '(+ - * / %)) (M-integer expression vars)]
      [(member*? (operator expression) '(== != < > <= >=)) (M-compare expression vars)]
      [(member*? (operator expression) '( && || !)) (M-bool expression vars)]
      [else (error 'no-valid-operator)])))

; Returns a value or boolean
(define M-return
  (lambda (expression vars)
    (cond
      [(list? expression) (M-evaluate expression vars)]
      [(number? expression) expression]
      [(eq? expression #t) 'true]
      [(eq? expression #f) 'false]
      [else (getValue expression vars)])))


(define M-state
  (lambda (expression vars)
    (cond
      [(null? expression) 'void]
      [(eq? (operator (car expression)) 'return) (M-return (getExpression expression) vars)]
      [(eq? (operator (car expression)) 'var) (M-state (cdr expression) (M-declare (car expression) vars))]
      [(eq? (operator (car expression)) '=) (M-state (cdr expression) (M-assign (car expression) vars))]
      [(eq? (operator (car expression)) 'if) (M-if (car expression) (cdr expression) vars)])))

(define M-state-start
  (lambda (expression)
    (M-state expression '())))

(parser "Code.txt")
(M-state-start (parser "Code.txt"))