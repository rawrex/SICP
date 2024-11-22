#lang sicp

(define (insertion-sort input-numbers)
  (define (insert number sorted-numbers)
    (cond ((null? sorted-numbers) (list number))
          ((< number (car sorted-numbers)) (cons number sorted-numbers))
          (else (cons (car sorted-numbers) (insert number (cdr sorted-numbers))))))
  (if (null? input-numbers)
      '()
      (insert (car input-numbers)
              (insertion-sort (cdr input-numbers)))))

(define test (list 2 -1 3 1 5 0))
(insertion-sort test)