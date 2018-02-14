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
;; to 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide lookup evaluate)

;; Looks up a symbol in an environment (represented as a list of lists)
;; and returns its attached value from the environment.
(define lookup
  (lambda (symbol environment)
    (if (empty? environment)
        (error "Symbol not found:" (display symbol))
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
       (display expression)
       (newline)
       (if(special-form? expression)
              (evaluate-special-form expression environment)
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
    (if (or (eq? (car expression) 'cond) (eq? (car expression) 'if))
        #t
        #f)))

(define evaluate-special-form
  (lambda (expression environment)
    (if (not (special-form? expression))
        (error "Expression is not in special form.")
        (cond
          ((eq? (car expression) 'cond) (evaluate-cond (cdr expression) environment))
          ((eq? (car expression) 'if) (evaluate-if (cdr expression) environment))))))

(define evaluate-if
  (lambda (expression environment)
    (display expression)
    (newline)
    (if (evaluate (car expression) environment)
        (evaluate (car (cdr expression)) environment)
        (evaluate (car (cdr (cdr expression))) environment))))

(define evaluate-cond
  (lambda (expression environment)
    (if (eq? (caar expression) 'else)
        (evaluate (cadar expression) environment)
        (if (evaluate (caar expression) environment)
            (evaluate (cadar expression) environment)
            (evaluate-cond (cdr expression) environment)))))


(define env (list
             (list 'x 5)
             (list '+ +)
             (list '* *)
             (list '- -)
             (list '= =)))

(special-form? '(if (= 1 1) (- (+ 1 1) 5) (- 2 2)))
;;(evaluate '(if (= 3 (+ 1 1)) (- (+ 1 2) 5) (- 2 2)) env)
(evaluate '(cond ((= 1 2) (+ 1 2))
                 ((= 1 1) (+ 1 3))
                 (else (+ 1 5))) env)
















