#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;; 
;; Lab #3
;;
;; Ivan Chuprinov
;; W01334363
;;
;; The purpose of this program is to 
;; build sorted lists of subsets
;; of a list.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide distribute subsets sublists length-ordered? element-ordered?)

;; puts a variable num as the first element of every element in the list ls
;; (ls should be a list of lists for distribute to work as intended)
(define distribute
  (lambda (num ls)
    (if (empty? ls)
        ls
        (cons (cons num (car ls)) (distribute num (cdr ls))))))

;; returns a list of sublists of a list passed as a parameter (unsorted)
(define sublists
  (lambda (ls)
    (if (empty? ls)
        '(())
        (let ((rest-of-subsets (sublists (cdr ls))))
          (append rest-of-subsets (distribute (car ls) rest-of-subsets))))))

;; returns a sorted version of a list of all sublists of a list passed as a parameter
(define subsets
  (lambda (ls)
    (sort (sort (sublists ls) element-ordered?) length-ordered?)))

;; returns true if list x is longer than y, false if shorter,
;; if the lengths are equal, returns the result of element-ordered?
(define length-ordered?
  (lambda (x y)
    (cond
      ((< (length x) (length y)) #t)
      ((> (length x) (length y)) #f)
      ((= (length x) (length y)) (element-ordered? x y)))))

;; determines if the two lists are passed in an ascending order of their values
(define element-ordered?
  (lambda (x y)
    ;; handle the case where x or y is empty
    (cond
      ((empty? x) #t)
      ((empty? y) #f)
      ;; handle the case where the first element in x is greater or less than one in y
      (else (cond
              ((< (car x) (car y)) #t)
              ((> (car x) (car y)) #f)
              ;; handle the case where the first elements in x and y are the same
              (else
               (if (or (empty? (cdr x)) (empty? (cdr y)))
                   #t
                   ;; call recursion if further checking is needed
                   (element-ordered? (cdr x) (cdr y)))))))))

;; returns the length of a list
(define length
  (lambda (ls)
    (if (empty? ls)
        0
        (+ 1 (length (cdr ls))))))