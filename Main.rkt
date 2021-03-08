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


; M-integer maps expressions to integer values
; operators: +, -, *, /, %
(define M-integer
  (lambda (expression)
    (cond
      [(number? expression) expression]
      [(eq? (operator expression) '+) (+ (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))]
      [(eq? (operator expression) '-) (- (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))]
      [(eq? (operator expression) '*) (* (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))]
      [(eq? (operator expression) '/) (quotient (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))]
      [(eq? (operator expression) '%) (modulo (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))]
      [else (error 'bad-operator)])))

; M-compare maps comparisons to boolean values
; operators: ==, !=, <, >, <=. >=
(define M-compare
  (lambda (expression)
    (cond
      [(boolean? expression) expression]
      [(eq? (operator expression) '<) (< (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '<=) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '>) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '>=) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '==) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '!=) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [else (error 'bad-operator)])))

; M-bool maps boolean expressions to boolean values and converts strings to booleans, True -> #t
; operators: &&, ||, !
(define M-bool
  (lambda (expression)
    (cond
      [(boolean? expression) expression]
      [(eq? expression 'true) #t]
      [(eq? expression 'false) #f]
      [(eq? (operator expression) '&&) (and (M-bool (leftoperand expression)) (M-bool (rightoperand expression)))]
      [(eq? (operator expression) '||) (or (M-bool (leftoperand expression)) (M-bool (rightoperand expression)))]
      [(and (eq? (operator expression) '!) (boolean? (leftoperand expression))) (not (M-bool (leftoperand expression)))]
      [else (error 'bad-operator)])))

; Variable declaration
; '(var x) or '(var x (expression))
(define M-declare
  (lambda (expression vars)
    (cond
      [(not (null? (cddr expression))) (M-assign (cons '= (cons (cadr expression) (cons (M-evaluate (caddr expression))'())))
                                                                (M-declare (cons 'var (cons (cadr expression) '())) vars))] ;Abstract
      [(member*? (leftoperand expression) vars) vars] ;remove old variable
      [else (cons (cons (leftoperand expression) '()) vars)])))

; Maps variables with values
; '(= x value/expression)
(define M-assign
  (lambda (expression vars)
    (insert (cadr expression) (M-evaluate(car (cddr expression))) vars)))

; if statements
(define M-if
  (lambda (expression)
    (cond
      [(and (eq? (caar expression) 'if) (M-compare (cdr (car expression)))) ;M-compare currently doesn't work for this
       (M-if (cddr expression))]
      [(eq? (car expression) 'if) (M-if (cddr expression))]
      [((eq? (car (operator expression)) 'return)) (M-return (getExpression expression))]
      ;[] ;this is for assignment
      [else (error 'invalid expression)])))

; Passes the expression into the correct function for evaluation
(define M-evaluate
  (lambda (expression)
    (cond
      [(not (list? expression)) expression]
      [(member*? (operator expression) '(+ - * / %)) (M-integer expression)]
      [(member*? (operator expression) '(==, !=, <, >, <=. >=)) (M-compare expression)]
      [(member*? (operator expression) '( && || !)) (M-bool expression)]
      [else (error 'no-valid-operator)])))

; Returns a value or boolean
(define M-return
  (lambda (expression vars)
    (cond
      [(list? expression) (M-evaluate expression)]
      [(number? expression) expression]
      [(eq? expression #t) 'true]
      [(eq? expression #f) 'false]
      [else (getValue expression vars)])))


(define M-state
  (lambda (expression vars)
    (cond
      [(eq? (operator (car expression)) 'return) (M-return (getExpression expression) vars)]
      [(eq? (operator (car expression)) 'var) (M-state (cdr expression) (M-declare (car expression) vars))]
      [(eq? (operator (car expression)) '=) (M-state (cdr expression) (M-assign (car expression) vars))])))

(define M-state-start
  (lambda (expression)
    (M-state expression '())))

(parser "Code.txt")