#lang sicp

(define (square x)
  (* x x))

;; Let's introduce both definitions

(define (old-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (old-expmod base (/ exp 2) m)) m))
        (else (remainder (* base (old-expmod base (- exp 1) m)) m))))

(define (new-expmod base exp m)
  (define (fast-expt base exp)
    (cond ((= exp 0) 1)
          ((even? exp) (square (fast-expt base (/ exp 2))))
          (else (* base (fast-expt base (dec exp))))))
  (remainder (fast-expt base exp) m))

;; Now, let's intorduce the parameterized prime-tester, to which we can pass an expmod
(define (prime-fast? expmod n)
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  (fast-prime? n 3))

(define (old n)
  (prime-fast? old-expmod n))

(define (new n)
  (prime-fast? new-expmod n))

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


;; The primes finder function
(define (find-n-primes from n test-function)
  (cond ((even? from) (find-n-primes (inc from) n test-function))
        ((and (> n 0) (timed-prime-test from test-function)) (find-n-primes (+ from 2) (dec n) test-function))
        ((> n 0) (find-n-primes (inc from) n test-function))))

;; Here, intermediate computations are done on regular "computer" numbers
;; probably always on registers in CPU
(find-n-primes 10000 2 old)

;; Here, the intermediate numbers in exponent computations are so big
;; that cannot be represented in CPU directly, and the computations must be made at a more abstract layer
(find-n-primes 10000 2 new)