#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;; 
;; Lab #8
;;
;; Ivan Chuprinov
;; W01334363
;;
;; The purpose of this program is
;; to evaluate an expression
;; (in a form of a list) in a context
;; of an environment (in a form of
;; a list of lists), also taking
;; in account special forms of
;; expressions 
;; (if, cond, let, lambda, letrec)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide lookup evaluate)

;; Mutable data structure closure
(struct closure (vars expr (env #:mutable)))

;; Returns the size of a list
(define list-size
  (lambda (list)
    (let loop ((ls list) (counter 0))
      ;; Initiate a loop on the list
      (if (empty? ls)
          ;; When the expression is empty, we return the size of the list
          counter
          ;; Otherwise, increment the size counter
          (begin
            (set! counter (+ 1 counter))
            ;; Loop is called again on a cdr portion of the list
            (loop (cdr ls) counter))))))

;; Looks up a symbol in an environment (represented as a list of lists)
;; and returns its attached value from the environment.
(define lookup
  (lambda (symbol environment)
    (if (empty? environment)
        (begin
          (display environment)(newline)
          (display symbol)(newline)
          (error "Symbol not found"))
        (if (symbol? symbol)
            ;; if the first parameter is a symbol
            (if (eq? (car (car environment)) symbol)
                ;; if the symbol passed matches the current symbol in the environment,
                ;; we return the meaning of that symbol in the environment
                (car (cdr (car environment)))
                ;; otherwise, look at the next symbol in the environment
                (lookup symbol (cdr environment)))
            ;; if the first parameter is not a symbol, return error
            (error "Not a symbol.")))))


;; Evaluates the expression list given the environment
(define evaluate
  (lambda (expression environment)
    (cond
      ;; A number is returned unchanged.
      ((number? expression) expression)
      ;; For a symbol the return value is looked up in the environment.
      ((symbol? expression) (lookup expression environment))
      ;; If the expression is a list:
      ((list? expression)
       (if(special-form? expression)
          ;; if the expression is in special form, evaluate the special form
          (evaluate-special-form expression environment)
          ;; otherwise, treat it as a normal expression
          (let ((ls empty))
            ;; We make a local variable pointing to an empty list
            (let loop ((exp (cdr expression)))
              ;; Initiate a loop on a cdr portion of the expression
              (if (empty? exp)
                  ;; When the expression is empty, we return the list, loop breaks
                  ls
                  ;; Otherwise, we evaluate the first element of the expression
                  ;; and add it to the list
                  (begin
                    (set! ls (append ls (list (evaluate (car exp) environment))))
                    ;; Loop is called again on a cdr portion of the expression
                    (loop (cdr exp)))))
            ;; We apply a procedure stored in the first element of the expression
            ;; to the list created from the rest of the elements of the expression
            (apply-function (evaluate (car expression) environment) ls))))
      ;; If the expression is anything else, an error is returned.
      (else (error "Wrong expression type.")))))

;; Returns true if the list is in special form, otherwise, returns false.
(define special-form?
  (lambda (expression)
    (if (list? expression)
        (cond ((eq? (car expression) 'cond) #t)
              ((eq? (car expression) 'if) #t)
              ((eq? (car expression) 'let) #t)
              ((eq? (car expression) 'lambda) #t)
              ((eq? (car expression) 'letrec) #t)
              (else #f))
        #f)))

;; Evaluates special form
(define evaluate-special-form
  (lambda (expression environment)
    (if (not (special-form? expression))
        ;; if the expression is not in special form, return error
        (error "Expression is not in special form.")
        ;; otherwise, check, which special form it is, and evaluate accordingly
        (cond
          ((eq? (car expression) 'lambda) (evaluate-lambda (cdr expression) environment))
          ((eq? (car expression) 'cond) (evaluate-cond (cdr expression) environment))
          ((eq? (car expression) 'if) (evaluate-if (cdr expression) environment))
          ((eq? (car expression) 'let) (evaluate-let (cdr expression) environment))
          ((eq? (car expression) 'letrec) (evaluate-letrec (cdr expression) environment))
          ))))

;; Evaluates if special form expression
(define evaluate-if
  (lambda (expression environment)
    ;; if there are exactly 3 elements after if, evaluate the statements
    (if (= (list-size expression) 3)
        ;; if the condition returns true
        (if (evaluate (car expression) environment)
            ;; evaluate the first expression
            (evaluate (car (cdr expression)) environment)
            ;; otherwise, evaluate the second expression
            (evaluate (car (cdr (cdr expression))) environment))
        ;; if there are more or less than 3 elements after if, the syntax is bad
        (error "Bad syntax. Expected 3 things after the if-statement"))))

;; Evaluates a cond special form expression, syntax is assumed to be correct
(define evaluate-cond
  (lambda (expression environment)
    (if (evaluate (caar expression) environment)
        (evaluate (cadar expression) environment)
        (evaluate-cond (cdr expression) environment))))

;; Evaluates a let special form expression, syntax is assumed to be correct
(define evaluate-let
  (lambda (expression OGenv)
    (let ((ls1 empty)
          (ls2 empty))
      (let loop ((assignment-list (car expression)))
        (if (empty? assignment-list)
            ;; stops the loop when assignment list is empty
            (void)
            (begin
              ;;ls1 is the list of cars (variable names)
              ;;ls2 is the list of evaluated cadars (assigned values)
              (set! ls1 (append ls1 (list (caar assignment-list))))
              (set! ls2 (append ls2 (list (evaluate (cadar assignment-list) OGenv))))
              (loop (cdr assignment-list)))))
      ;; give the local environment correct structure
      (let ((local-env empty))
        (let loop ((ls1 ls1) (ls2 ls2))
          (if (empty? ls1)
              ;; stop looping when ls1 is empty (ls1 and ls2 are of the same size)
              (void)
              ;; give local environment the structure '((var1 val1) (var2 val2)...)
              (begin
                (set! local-env (append
                                 local-env
                                 (list (append
                                        (list (car ls1))
                                        (list (car ls2))))))
                (loop (cdr ls1) (cdr ls2)))))
        
        ;; evaluate the expression in the extended environment
        (let ((extended-env (append local-env OGenv)))
          (evaluate (cadr expression) extended-env))))))

;; Evaluates a lambda expression, creating a closure data structure, syntax is assumed to be correct
(define evaluate-lambda
  (lambda (expression env)
    (closure (car expression) (cadr expression) env)))

;; Applies a procedure to a list. If is a closure, calls apply-closure
(define apply-function
  (lambda (proc list)
    (if (closure? proc)
        (print-closure proc)
        (display proc))
    (newline)
    (if (closure? list)
        (print-closure list)
        (display list))
    (newline)
    (cond  ((procedure? proc) (apply proc list))
           ((closure? proc) (apply-closure proc list))
           ;; If the proc variable is not a procedure or a closure, throws an error
           (else (error "Unknown function type")))))

;; Applies a closure to the given list of parameters
(define apply-closure
  (lambda (cls param)
    (if (eq? (list-size (closure-vars cls)) (list-size param))
        ;; if the amount of parameters passed matches(values) the amount of parameters accepted (variables),
        ;; ties each variable to the corresponding value and appends it to the new local environment
        (let loop ((variables (closure-vars cls)) (values param))
            (if (empty? variables)
                (void)
                (begin
                  (set-closure-env! cls (append (list (list (car variables) (car values))) (closure-env cls)))
                  (loop (cdr variables) (cdr values))))
          ;; after that, evaluates the expression part of the closure in that new environment
          (evaluate (closure-expr cls) (closure-env cls)))
        ;; if the amount of values does not match the amount of variables, throws an error
        (error "Arity mismatch"))))

(define evaluate-letrec
  (lambda (expression OGenv)
    (let ((ls1 empty)
          (ls2 empty))
      (let loop ((assignment-list (car expression)))
        (if (empty? assignment-list)
            ;; stops the loop when assignment list is empty
            (void)
            (begin
              ;;ls1 is the list of cars (variable names)
              ;;ls2 is the list of evaluated cadars (assigned values)
              (set! ls1 (append ls1 (list (caar assignment-list))))
              (set! ls2 (append ls2 (list (evaluate (cadar assignment-list) OGenv))))
              (loop (cdr assignment-list)))))
      ;(show expression)
      ;; give the local environment correct structure
      (let ((local-env empty))
        (let loop ((ls1 ls1) (ls2 ls2))
          (if (empty? ls1)
              ;; stop looping when ls1 is empty (ls1 and ls2 are of the same size)
              '()
              ;; give local environment the structure '((var1 val1) (var2 val2)...)
              (begin
                (set! local-env (append
                                 local-env
                                 (list (append
                                        (list (car ls1))
                                        (list (car ls2))))))
                (loop (cdr ls1) (cdr ls2)))))
        
        
        (let loop ((env local-env))
          (if (empty? env)
              (void)
              (if (closure? (cadar env))
                  (begin
                    (set-closure-env! (cadar env) (append env OGenv))
                    (loop (cdr env)))
                  (loop (cdr env)))))
        ;; evaluate the expression in the extended environment              
        (let ((extended-env (append local-env OGenv)))
          ;(show extended-env)
          (display expression)
          (newline)
          (evaluate expression extended-env))))))

(define cl1 (closure '(a b) '(+ ab) '((x 1) (y 2))))



(define show
  (lambda (x)
    (cond ((list? x)
           (if (empty? x)
               (void)
               (begin
                 (show (car x))
                 (show (cdr x)))))
          ((closure? x) (print-closure x))
          (else
           (begin
             (display x)
             (newline))))))

(define print-closure
  (lambda (cl)
    (display (list 'closure (closure-vars cl) (closure-expr cl) (closure-env cl)))
    (newline)))

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1 (map (lambda (x y) (list x y))
                 '(x y z + - * cons car cdr nil = equal? < else  add list)
             (list 2 4 6 + - * cons car cdr '() = equal? < #t    add list)))

(define env '((x 10) (y 5) ('+ +) ('- -) ('* *) ('< <) ('> >)))

(evaluate '(letrec ((f (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))) (f 5)) env)
;(letrec ((f (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))) (f 5)) 