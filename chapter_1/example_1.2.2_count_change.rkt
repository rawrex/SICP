#lang sicp

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= kinds-of-coins 0) 0)
        (else (+ (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
                 (cc amount (- kinds-of-coins 1))))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 2)
        ((= kinds-of-coins 3) 5)
        ((= kinds-of-coins 4) 10)
        ((= kinds-of-coins 5) 50)))

(count-change 1)
(count-change 10)
(count-change 100)
(count-change 500)