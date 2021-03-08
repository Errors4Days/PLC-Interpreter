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

; I don't think this is actually assigning variable values
(define M-assign
  (lambda (var-name value)
    (cond
      [(number? value) value]
      [else (M-assign var-name (M-integer value))])))

; Passes the expression into the correct function for evaluation
(define M-evaluate
  (lambda (expression)
    (cond
      [(not (list? expression)) expression]
      [(member*? (operator expression) '(+ - * / %)) (M-integer expression)]
      [(member*? (operator expression) '(==, !=, <, >, <=. >=)) (M-compare expression)]
      [(member*? (operator expression) '( && || !)) (M-bool expression)]
      [else (error 'no-valid-operator)])))


(define M-return
  (lambda (expression)
    (cond
      [(list? expression) (M-evaluate expression)]
      [(number? expression) expression]
      [(eq? expression #t) 'true]
      [(eq? expression #f) 'false])))


(define M-state
  (lambda (expression vars)
    (cond
      [(eq? (car (operator expression)) 'return) (M-return (getExpression expression))])))


(define M-state-start
  (lambda (expression)
    (M-state expression '())))

; if, while
; declaration
; assign

(parser "Code.txt")