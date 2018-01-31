#lang racket

(define distribute ;;DONE
  (lambda (num ls)
    (if (empty? ls)
        ls
        (cons (cons num (car ls)) (distribute num (cdr ls))))))

(define get-subsets
  (lambda (ls)
    (display ls)
    (newline)
    (if (empty? ls)
        ls
        (distribute (car ls) (cons (car ls) (get-subsets (cdr ls)))))))

(get-subsets (list 1 2 3))

(distribute 0 '((1) (2) (3) (4)))

;(define distribute