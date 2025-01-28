#lang sicp

(define (count-change amount)
  (define us-coins (list 50 25 10 5 1))
  (define uk-coins (list 100 50 20 10 5 2 1 0.5))
  (define ru-coins (list 50 10 5 2 1))
  
  (define (first-denomination coins)
    (car coins))

  (define (but-first-denomination coins)
    (cdr coins))

  (define (no-more? coins)
    (null? coins))
  
  (define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount (but-first-denomination coin-values))
                   (cc (- amount (first-denomination  coin-values))  coin-values)))))
  
  (cc amount us-coins))
  ;(cc amount uk-coins))
  ;(cc amount ru-coins))

(count-change 1)
(count-change 10)
(count-change 100)
(count-change 500)