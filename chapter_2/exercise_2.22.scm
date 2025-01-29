#lang sicp

(define (square x)
  (* x x))

(define (square-list-a items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ; Here, we prepend currently reached number from the items
              ; to the already processed numbers (in result)
              ; Thus, we place each new item to front of the result
              (cons (square (car things)) answer))))
  (iter items '()))

(define (square-list-b items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ; Here, answer is a pair from previous step
              ; Hence, we create a pair from a pair and a number
              (cons answer (square (car things))))))
  (iter items '()))

(square-list-a (list 1 2 3 4 5))
(square-list-b (list 1 2 3 4 5))