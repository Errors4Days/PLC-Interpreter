#lang racket
; ABSTRACTION
(define operator cadr)
(define leftoperand car)
(define rightoperand caddr)

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
      [(number? expression) expression]
      [(eq? (operator expression) '<) (< (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '<=) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '>) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '>=) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '==) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [(eq? (operator expression) '!=) (<= (M-compare (leftoperand expression)) (M-compare (rightoperand expression)))]
      [else (error 'bad-operator)])))

; M-bool maps boolean expressions to boolean values
; operators: &&, ||, !.

(M-integer '((5 - 3) *(9 / 3)))
(M-compare '(5 < 3))