#lang sicp

(define (reverse input-items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter input-items '()))

(define (deep-reverse input-tree)
  (define (iter tree result)
    (cond ((null? tree)
             result)
          ((pair? (car tree))
             (iter (cdr tree) (cons (deep-reverse (car tree)) result)))
          (else 
             (iter (cdr tree) (cons (car tree) result)))))
  (iter input-tree '()))
  

(define tree (list 1 2 (list 3 4) (list 5 6) (list (list 7 8) (list 9))))
tree
(deep-reverse tree)
      