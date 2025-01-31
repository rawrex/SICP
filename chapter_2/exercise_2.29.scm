(define (make-mobile left right)
    (list left right))

(define (make-branch len payload)
    (list len payload))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cdr mobile))

(define (branch-len branch)
    (car branch))

(define (branch-payload branch)
    (cdr branch))

(define (structure? payload)
    (pair? payload))

(define (total-weight mobil)
    (let ((left (left-branch mobil))
          (right (right-branch mobil)))
        (cond ((null? mobil) 0)
              ((and (structure? (branch-payload left)) (structure? (branch-payload  right))) (+ (total-weight left) (total-weight right)))
              ((structure? (branch-payload left)) (+ (total-weight (branch-payload left)) (branch-payload right)))
              ((structure? (branch-payload right)) (+ (total-weight (branch-payload right)) (branch-payload left)))
              (else (+ (branch-payload left) (branch-payload right))))))

; m:
; | \
; b4 b1
; | \
; b2 b3

(define b1 (make-branch 10 2))  ; plain weight
(define b2 (make-branch 5 1))   ; plain weight
(define b3 (make-branch 5 1))   ; plain weight
(define b4 (make-branch 5 (make-mobile b2 b3)))

(define m (make-mobile b4 b1))

(total-weight m)
