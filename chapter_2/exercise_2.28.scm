(define tree (list 1 2 (list 3 4) (list 5 6) (list (list 7 8) (list 9))))

(define (flat input-tree)
 (define (iter tree result)
  (cond ((null? tree)
            result)
        ((pair? (car tree))
            (iter (cdr tree) (append result (flat (car tree)))))
        (else
            (iter (cdr tree) (append result (list (car tree)))))))
 (iter input-tree '()))

tree
(flat tree)
