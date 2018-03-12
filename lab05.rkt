#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;; 
;; Lab #5
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
;; expressions (if, cond)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide lookup evaluate)

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
        (error "Symbol not found")
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
          (if (procedure? (evaluate (car expression) environment))
              ;; If the first value of a list is a procedure:
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
                (apply (evaluate (car expression) environment) ls))
              ;; If the first thing in the evaluated list is not a procedure,
              ;; an error is returned.
              (error "First element is not a procedure"))))
      ;; If the expression is anything else, an error is returned.
      (else (error "Wrong expression type.")))))

;; Returns true if the list is in special form, otherwise, returns false.
(define special-form?
  (lambda (expression)
    (if (and (or (eq? (car expression) 'cond) (eq? (car expression) 'if)) (list? expression))
        #t
        #f)))

;; Evaluates special form
(define evaluate-special-form
  (lambda (expression environment)
    (if (not (special-form? expression))
        ;; if the expression is not in special form, return error
        (error "Expression is not in special form.")
        ;; otherwise, check, which special form it is, and evaluate accordingly
        (cond
          ((eq? (car expression) 'cond) (evaluate-cond (cdr expression) environment))
          ((eq? (car expression) 'if) (evaluate-if (cdr expression) environment))))))

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


;; Evaluates a cond special form expression
;; NOTE: Doesn't behave exactly like the actual cond 
;; (doesn't return errors when else is in the middle of the expression, or if no else is present)
(define evaluate-cond
  (lambda (expression environment)
    (if (evaluate (caar expression) environment)
        (evaluate (cadar expression) environment)
        (evaluate-cond (cdr expression) environment))))


