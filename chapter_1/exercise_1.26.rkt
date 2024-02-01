#lang sicp

(define (square x)
  (* x x))

(define (halve x)
  (/ x 2))

(define (old-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (old-expmod base (halve exp) m)) m))
        (else (remainder (* base (old-expmod base (dec exp) m)) m))))

(define (new-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;; in this way we can simply ingore squaring and halving
         ;; and just stick to the plain incremental case, as in the "else"
         ;; moreover, we will add up computations this way
         (remainder (* (new-expmod base (halve exp) m)
                       (new-expmod base (halve exp) m))
                    m))
        (else (remainder (* base (new-expmod base (dec exp) m))  m))))


(define (incremental-expmod base exp m)
  (if (= exp 0)
      1
      (remainder (* base (incremental-expmod base (dec exp) m))  m)))


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

(define (old x)
  (prime-fast? old-expmod x))

(define (new x)
  (prime-fast? new-expmod x))

(define (incremental x)
  (prime-fast? incremental-expmod x))

(find-n-primes 1000 3 old)
(find-n-primes 1000 3 incremental)  ;; on average 350
(find-n-primes 1000 3 new)          ;; runs almost thrice as slow, on average 850