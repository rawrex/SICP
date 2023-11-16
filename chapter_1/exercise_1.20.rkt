#lang sicp

; Iterative implementation of Euclid's Algorithm for finding GCD

(define R remainder)

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (R a b))))

; Normal order
;
; (GCD 206 40)
;
; (GCD 40 (R 206 40))
;  -> when inside GCD, executes R in the predicate -> 6
;
; (GCD
;      (R 206 40)
;      (R 40 (R 206 40))
; -> when inside GCD, executes R in the predicate -> 4
;
; (GCD
;      (R 40 (R 206 40))
;      (R (R 206 40) (R 40 (R 206 40))))
; -> when inside GCD, executes R in the predicate -> 2
;
; (GCD
;      (R (R 206 40) (R 40 (R 206 40)))
;      (R (R 40 (R 206 40)) (R (R 206 40) (R 40 (R 206 40))))))
; -> when inside GCD, executes R in the predicate -> 0
;
; returns 2
; R was called 18 times (11 in GCD calls + 3 in predicates + 4 in final predicate) (???)

; Applicative order
;
; (GCD 206 40)
; (GCD 40 6) -> before GCD, executes (R 206 40) -> 6
; (GCD 6 4) -> before GCD, executes (R 40 6) -> 4
; (GCD 4 2) -> before GCD, executes (R 6 4) -> 2
; (GCD 2 0) -> before GCD, executes (R 4 2) -> 0
; returns 2
;
; R was called four times

; Tests
(GCD 28 16)
(GCD 120 45)
(GCD 206 40)