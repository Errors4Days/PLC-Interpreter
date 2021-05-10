#lang racket
(require "classParser.rkt")
; Elizabeth and Justin
; Interpreter project part4

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file classname)
    (scheme->language
     (run-main (parser file) classname
               (create-class-closures (parser file) (newenvironment))))))

; Creates class closure
(define create-class-closures
  (lambda (code closure)
    (cond
      [(null? code) '()]
      [else (append (create-class-closures (cdr code)  closure)
                    (cons (class-closure (car code) closure) '())) ])))
; Gets the parent class name from the overall closure (this is for organization)
(define get-extends
  (lambda (class)
    (cond
      [(null? (caddr class)) (cons '() '())]
      [else (cons (cdr (caddr class)) '())])))
; Creates a closure for a class in the given format:
;(classname (parentname) ((methods/vars) (method/var definitons/values)))
(define class-closure
  (lambda (class closure)
    (append (cons (cadr class)
                  (get-extends class))
            (interpret-statement-list (cadddr class) (newenvironment)
                                      (lambda (env) (myerror "no return"))
                                      (lambda (env) (myerror "Break used outside of loop"))
                                      (lambda (env) (myerror "Continue used outside of loop"))
                                      (lambda (v env) (myerror "Uncaught exception thrown"))))))

; Gets the correct field
; '(((a) ((((A) ((10 5)))))))
; '((A () ((main y x) ((() ((var a (new A)) (return (dot a x)))) 10 5))))
(define dot-operation
  (lambda (expr environment closure)
    (cond
      ;top layer of the environment should contain instance closure
      ;So, in the above example, expr contains '(a x)
      [(eq? (car expr) 'this) (myerror "this")];(get-instance-field (getselfclass (car expr)) (cdr expr) environment closure)]
      [(eq? (car expr) 'super) (myerror "super")]
      [else (get-instance-fields (cadr expr)
                                 (get-instance-names (cadr expr) (car expr) closure)
                                 (get-instance-values (car expr) environment))])))

(define get-instance-fields
  (lambda (fieldname nameslist valueslist)
    (cond
      [(or (null? nameslist)(null? valueslist)) (myerror "Error incorrect list of names or values")]
      [(eq? fieldname (car nameslist)) (car valueslist)]
      [else (get-instance-fields fieldname (cdr nameslist) (cdr valueslist))])))
                          
; Gets the list of fields from a class
(define get-instance-names
  (lambda (varname fieldname closure)
    (caadr (get-class 'A closure))))

(define instance-to-class
  (lambda (iname closure environment)
    (caadar (get-instance iname environment))))

; Gets the instance
(define get-instance
  (lambda (iname environment)
    (cond
      [(null? environment) (myerror "Instance missing" iname)]
      [(eq? iname (caaar environment)) (caadar environment)]
      [else (get-instance iname (cdr environment))])))
; Gets the instance values
(define get-instance-values
  (lambda (iname environment)
    (caadar (get-instance iname environment))))


; Creates instance and instance closure
; environment ((A)((1 2 3)))
(define new-instance
  (lambda (classname environment closure)
    (insert classname (extract-values 
                       (cadadr (get-class classname closure))) (newenvironment))))

; Given the closure for a class it will extract the field values
(define extract-values
  (lambda (classclosure)
    (cond
      [(null? classclosure) '()]
      [(not(list? (car classclosure))) (cons (car classclosure) (extract-values (cdr classclosure)))]
      [else (cons 'func (extract-values (cdr classclosure)))])))



; Gets and runs the main function
(define run-main
  (lambda (args classname closure)
    (call/cc
     (lambda (return)
       (interpret-statement-list-main (get-main-class args classname) (newenvironment)
                                      closure return
                                      (lambda (env) (myerror "Break used outside of loop"))
                                      (lambda (env) (myerror "Continue used outside of loop"))
                                      (lambda (v env) (myerror "Uncaught exception thrown")))))))
; Sees if specified class contains the main method                  
(define get-main-class
  (lambda (args class)
    (cond
      [(null? args) (myerror "Class not found")]
      [(eq? class (cadar args)) (cadddr (get-main-body (cadddr (car args))))]
      [else (get-main-class (cdr args) class)])))
; Gets the main body for the specified class from the above method
(define get-main-body
  (lambda (class-body)
    (cond
      [(null? class-body) (myerror "Main method not found")]
      [(and (pair? (car class-body)) (eq? 'main (cadar class-body))) (car class-body)]
      [else (get-main-body (cdr class-body))])))

; This method retrieves the parent closure
(define get-class-parent
  (lambda (classname closure)
    (cond
      [(null? closure) '()]
      [(eq? classname (caar closure)) (cdar closure)]
      [else (get-class classname (cdr closure))])))
    
; This method retrieves the closure based upon the name of the class
; ADD/CONS PARENT CONNECTION.
(define get-class
  (lambda (classname closure)
    (cond
      [(null? closure) (myerror "Missing class: " classname)]
      [(eq? classname (caar closure)) (cdar closure)]
      [else (get-class classname (cdr closure))])))


; Takes in the body of a class and returns the appropriate bindings 
; Mstate (<statement><statement-list>, state) = Mstate(<statement-list>, Mstate(<statement>, state))
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (cond
      [(null? statement-list) environment]
      [else (interpret-statement-list (cdr statement-list)
                                      (interpret-statement-bind (statement-type statement-list)
                                                                environment return break continue throw) return break continue throw)])))

; Creates global variables and function bindings from outside of main. This is the outer layer.
(define interpret-statement-bind
  (lambda (statement environment return break continue throw)
    (cond
      [(eq? 'return (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'var (statement-type statement)) (interpret-declare statement environment '() throw)]
      [(eq? '= (statement-type statement)) (interpret-assign statement environment '() throw)]
      [(eq? 'if (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'while (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'continue (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'break (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'begin (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'throw (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'try (statement-type statement)) (myerror "Cannot have statement outside a function:" (statement-type statement))]
      [(eq? 'function (statement-type statement)) (interpret-function-bind (cdr statement) environment)]
      [(eq? 'static-function (statement-type statement)) (interpret-function-bind (cdr statement) environment)]
      [else (myerror "Unknown statement:" (statement-type statement))])))

; Runs statements from the main function. 
(define interpret-statement-list-main
  (lambda (statement-list environment closure return break continue throw)
    (cond
      [(null? statement-list) environment]
      [else (interpret-statement-list-main (cdr statement-list)
                                           (interpret-statement (statement-type statement-list) environment closure return break continue throw)
                                           closure return break continue throw)])))

; Interpret a statement in the environment from main.
(define interpret-statement
  (lambda (statement environment closure return break continue throw)
    (cond
      [(eq? 'return (statement-type statement)) (interpret-return statement environment closure return throw)]
      [(eq? 'var (statement-type statement)) (interpret-declare statement environment closure throw)]
      [(eq? '= (statement-type statement)) (interpret-assign statement closure environment throw)]
      [(eq? 'if (statement-type statement)) (interpret-if statement environment closure return break continue throw)]
      [(eq? 'while (statement-type statement)) (interpret-while statement environment closure return throw)]
      [(eq? 'continue (statement-type statement)) (continue environment)]
      [(eq? 'break (statement-type statement)) (break environment)]
      [(eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw)]
      [(eq? 'throw (statement-type statement)) (interpret-throw statement environment throw)]
      [(eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw)]
      [(eq? 'function (statement-type statement)) (interpret-function-bind (cdr statement) environment closure)]   
      [(eq? 'funcall (statement-type statement)) (begin (eval-function-call-state statement environment closure throw) environment)]
      [else (myerror "Unknown statement:" (statement-type statement))])))

; Adds a function binding to the enivronment
(define interpret-function-bind
  (lambda (statement environment)
    (insert (statement-type statement) (cdr statement) environment)))

; Helper function for function call
(define eval-function-call-state
  (lambda (expr environment closure throw)
    (call/cc
     (lambda (return)
       (eval-function-call (cdr expr) environment closure
                           return
                           (lambda (env) (myerror "Break used outside of loop"))
                           (lambda (env) (myerror "Continue used outside of loop"))
                           throw)))))

; Evaluates a function call. Extracts the function name, formal parametesr, parameter values, code, and closure
(define eval-function-call
  (lambda (function-list environment closure return break continue throw)
    (cond
      [(eq? 'dot (caar function-list))
       (dot-function function-list environment closure return break continue throw)]
      [else
       (interpret-statement-list-main
        (statement-type-second (lookup-in-env (statement-type function-list) environment))
        (push-frame (interpret-closure-parameters (statement-type function-list)
                                                  (statement-type-first (lookup-in-env (car function-list) environment))
                                                  (cdr function-list)
                                                  (cdr (lookup-in-env (statement-type function-list) environment))
                                                  environment
                                                  (lookup-in-env (statement-type function-list) environment)
                                                  throw))
        return break continue throw)])))

; Runs a function containing the dot operator
; expr input will be of the form '((dot a add) var1 var2)
; sends (add var1 var2) to eval-function-call
(define dot-function
  (lambda (function-list environment closure return break continue throw)
    (interpret-statement-list-main (get-function-code (cdar function-list) closure)
                                   (push-frame (class-to-environment (cadar function-list) environment))
                                   closure break continue throw)))

(define get-function-code-helper
  (lambda (iname fname closure)
    (get-instance iname)))

;Gets function body and parameters
; '((() ((var a (new A)) (return (funcall (dot a add) 10 2)))) ((g h) ((return (+ g h)))))
(define get-function-code
  (lambda (target fnames fbodies)
    (cond
      [(null? fbodies) (myerror "Can't find function " fnames)]
      [(eq? (car fnames) target) (car fbodies)]
      [else (get-function-code target (cdr fnames) (cdr fbodies))])))

(define class-to-environment
  (lambda (iname environment)
    environment))

; Adds the formal parameters value to the closure
(define interpret-closure-parameters
  (lambda (f-name f-parameter-names f-parameter-values closure environment code throw)
    (cond
      [(and (null? f-parameter-names) (null? f-parameter-values))
       (insert f-name code closure)]
      [(null? f-parameter-values) (myerror "Missing input parameters at function:" f-name)]
      [(null? f-parameter-names) (myerror "Extra input parameters at function:" f-name)]
      [else (interpret-closure-parameters f-name (cdr f-parameter-names) (cdr f-parameter-values)
                                          (insert (statement-type f-parameter-names) (eval-expression (statement-type f-parameter-values) environment throw) closure)
                                          environment code throw)])))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment closure return throw)
    (return (eval-expression (get-expr statement) environment closure throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment closure throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment closure throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment closure throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment closure throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment closure return break continue throw)
    (cond
      [(eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment closure return break continue throw)]
      [(exists-else? statement) (interpret-statement (get-else statement) environment closure return break continue throw)]
      [else environment])))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment closure return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment closure throw)
                            (loop condition body (interpret-statement body environment closure return break (lambda (env) (break (loop condition body env))) throw))
                            environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
; Mstate({ <body> }, state) = pop-frame (Mstate (<body>, pushframe(state)))
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list-main (cdr statement)
                                              (push-frame environment)
                                              return
                                              (lambda (env) (break (pop-frame env)))
                                              (lambda (env) (continue (pop-frame env)))
                                              (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value.
(define interpret-throw
  (lambda (statement environment closure throw)
    (throw (eval-expression (get-expr statement) environment closure throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (interpret-statement-list-main 
                                      (get-body catch-statement) 
                                      (insert (catch-var catch-statement) ex (push-frame env))
                                      return 
                                      (lambda (env2) (break (pop-frame env2))) 
                                      (lambda (env2) (continue (pop-frame env2))) 
                                      (lambda (v env2) (throw v (pop-frame env2))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants, variables, and function calls
(define eval-expression
  (lambda (expr environment closure throw)
    (cond
      [(number? expr) expr]
      [(eq? expr 'true) #t]
      [(eq? expr 'false) #f]
      [(not (list? expr)) (lookup expr environment)]
      [(eq? 'funcall (operator expr)) (eval-function-call-state expr environment closure throw)]
      [(eq? 'new (operator expr)) (new-instance (cadr expr) environment closure)]
      [else (eval-operator expr environment closure throw)])))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment closure throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment closure throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment closure throw)))
      ((eq? 'dot (operator expr)) (dot-operation (cdr expr) environment closure))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment closure throw) environment closure throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment closure throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment closure throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment closure throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment closure throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

(define statement-type-first caar)
(define statement-type-second cadar)
(define main-code cdddar)
(define end cdr)

;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (begin (car vallist) vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))


; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

;-----------------
; TESTING
;-----------------


(interpret "Tests4/Test1" 'A) ;15
#|
(interpret "Tests4/Test2" 'A) ;12
(interpret "Tests4/Test3" 'A) ;125
(interpret "Tests4/Test4" 'A) ;36
(interpret "Tests4/Test5" 'A) ;54
(interpret "Tests4/Test6" 'A) ;110
(interpret "Tests4/Test7" 'C) ;26
(interpret "Tests4/Test8" 'Square) ;117
(interpret "Tests4/Test9" 'Square) ;32
(interpret "Tests4/Test10" 'List) ;15
(interpret "Tests4/Test11" 'List) ;123456
(interpret "Tests4/Test12" 'List) ;5285
(interpret "Tests4/Test13" 'C) ;-716
|#
