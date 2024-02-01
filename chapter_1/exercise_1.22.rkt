#lang sicp

(define (square x)
  (* x x))

;; Smallest Divisor Test

(define (prime-GCD? n)
  (define (divides? divisor number)
    (= (remainder number divisor) 0))
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (inc test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= (smallest-divisor n) n))


;; The Profiling function

(define (timed-prime-test n test-function)
  (define (report result elapsed-time)
    (cond (result
           (display "\n\nnumber: ")
           (display n)
           (display "\ntime: ")
           (display elapsed-time)
           (newline)))
          ;; we do not report non-prime
    result)
  (define (start-prime-test n start-time)
    (if (test-function n)
        (report #t (- (runtime) start-time))
        (report #f (- (runtime) start-time))))
  (start-prime-test n (runtime)))


;; Range test

(define (find-primes begin end)
  (cond ((and (< begin end) (even? begin))
         (find-primes (inc begin) end))
        ((< begin end)
         (timed-prime-test begin prime-GCD?)
         (find-primes (+ 2 begin) end))))


;; Find specified number of primes starting from...

(define (find-n-primes from n test-function)
  (cond ((even? from) (find-n-primes (inc from) n test-function))
        ((and (> n 0) (timed-prime-test from test-function)) (find-n-primes (+ from 2) (dec n) test-function))
        ((> n 0) (find-n-primes (inc from) n test-function))))

(define (run-tests)
  (display "GCD test:")
  (find-n-primes 100000 3 prime-GCD?)     ; average time: 50 
  (find-n-primes 1000000 3 prime-GCD?)    ; average time: 150 
  (find-n-primes 10000000 3 prime-GCD?))  ; average time: 385