#lang sicp

(define (make-counter n)
  (lambda (m)
    (cond
      ((equal? m 'next) (set! n (+ n 1)) n)
      ((equal? m 'show) n)
      ((equal? m 'rand) (set! n (random 300)) n))))

;; Ex 3.1

(define (make-accum sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
           sum)))

;; Ex 3.2

(define (monitor f)
  (define (make-mon f total)
    (lambda (arg)
      (cond ((equal? arg 'calls?) total)
            ((equal? arg 'reset) (set! total 0) total)
            (else (begin (set! total (+ total 1)) (f arg))))))
  (make-mon f 0))


;; Ex 3.3
(define (make-acc init-pass balance)
  (display "Welcome to the Scheme-Bank inc.")
  (define (foo-acc init-pass balance attempts)
    (lambda (pass op amount) ;the interface to the account
      ; Withdraw helper
      (define (w x)
        (if (> balance x)
            (- balance x)
            (error "Trying to withdraw more than you have!")))
      ; Deposit helper
      (define (d x)
        (if (> x 0)
            (+ balance x)
            (error "Negative deposit!" x)))
      ; Security error handler
      (define (call-the-cops x)
        (error "COPS ARE COMING FOR YA! FAILED ATTEMPTS:" x))
      ; Security check
      (define (wrong-pass)
        (if (> attempts 7)
            (call-the-cops attempts)
            (set! attempts (+ 1 attempts))))
      ; MAIN
      (if (equal? pass init-pass)
          (cond ((equal? op 'withdraw) (begin (set! balance (w amount)) balance))
                ((equal? op 'deposit) (begin (set! balance (d amount)) balance))
                (else (error "Unknown operation" op)))
          (wrong-pass))))
  ; Instantiate
  (foo-acc init-pass balance 0))





; 3.1.2

(define (random-init)
  (random 100000))

; flaw in here
(define (rand-update x)
  (+ x (random x)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x)) x)))

;(define (estimate-pi trials)
;  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1) trials-passed
                     x2))))))
  (iter trials 0 initial-x))


;; Ex 3.6
(define random-init 100)

(define (rand-update x)
  (+ x 5))

(define rand
  (let ((init random-init))
    (lambda (m)
      (cond ((equal? m 'reset)
             (lambda (x) (set! init x)))
            ((equal? m 'generate)
             (set! init (rand-update init)) init)
            (else (error "Wrong operation" m))))))



;; Ex 3.7

; as a reference
(define (make-acc init-pass balance)
  (display "Welcome to the Scheme-Bank inc.")
  (define (foo-acc init-pass balance attempts)
    (lambda (pass op amount) ;the interface to the account
      ; Withdraw helper
      (define (w x)
        (if (> balance x)
            (- balance x)
            (error "Trying to withdraw more than you have!")))
      ; Deposit helper
      (define (d x)
        (if (> x 0)
            (+ balance x)
            (error "Negative deposit!" x)))
      ; Security error handler
      (define (call-the-cops x)
        (error "COPS ARE COMING FOR YA! FAILED ATTEMPTS:" x))
      ; Security check
      (define (wrong-pass)
        (if (> attempts 7)
            (call-the-cops attempts)
            (set! attempts (+ 1 attempts))))
      ; MAIN
      (if (equal? pass init-pass)
          (cond ((equal? op 'checkin) #t)
                ((equal? op 'withdraw) (begin (set! balance (w amount)) balance))
                ((equal? op 'deposit) (begin (set! balance (d amount)) balance))
                ((equal? op 'balance) balance)
                (else (error "Unknown operation" op)))
          (wrong-pass))))
  ; Instantiate
  (foo-acc init-pass balance 0))


(define (make-joint acc old-pass new-pass)
  (define (go-joint init-new-pass)
    (lambda (pass op amount)
      ; WITHDRAW operation with balance display
      (define (withdraw)
        (begin (acc old-pass 'withdraw amount)
               (acc old-pass 'balance 0)))
      ; DEPOSIT operation with balance display
      (define (deposit)
        (begin (acc old-pass 'deposit amount)
               (acc old-pass 'balance 0)))
      ; BALANCE
      (define (balance)
        (acc old-pass 'balance 0))
      ; MAIN
      (if (equal? pass init-new-pass)
          (cond ((equal? op 'checkin) #t)
                ((equal? op 'withdraw) (withdraw))
                ((equal? op 'deposit) (deposit))
                ((equal? op 'balance) (balance))
                (else (error "Unknown operation" op))))))
  (cond ((acc 'old-pass 'checkin 0) (go-joint new-pass))
        (else (error "Wrong password for the former account" old-pass))))


;;Ex 3.8

(+ (f 1) (f 0))
(+ (f 0) (f 1))

(define y 100)

(define (f x)
  (set! y x))

(define f 
  (let ((called #f)) 
    (lambda (x) 
      (if called 
          0 
          (begin 
            (set! called #t) 
            x)))))


(define make-count
  (let ((glob 0))
    (lambda ()
      (let ((loc 0))
        (lambda ()
          (set! loc (+ loc 1))
          (set! glob (+ glob 1))
          (list loc glob))))))



(define make-count
  (let ((glob 0))
    (lambda ()
      (let ((loc 0))
        (lambda (msg)
          (cond ((eq? msg 'local)
                 (lambda ()
                   (set! loc (+ loc 1))
                   loc))
                ((eq? msg 'global)
                 (lambda ()
                   (set! glob (+ glob 1)) glob))
                (else (error "No such method" msg)) ))))))



;; Ex 3.13

(define (append! x y)
  (set-cdr! (last-pair x) y) x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (almost-last-pair x)
  (if (null? (cddr x))
      x
      (almost-last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x) x)

(define (make-c x)
  (set-cdr! (almost-last-pair x) x) x)


(define z (make-cycle (list 'a 'b 'c)))


;; Ex 3.14

(define (m x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(eq? (cdar bar) (cddr bar))


(define foo (list (cons 1 2) (cons 3 4) (cons 5 6)))

(set-cdr! (caddr foo) (cdadr foo))




;; Ex 3.18

; The idea is to use eq? on every element of the list
; to see if it is eq? to any other element in the list.

(define (cycle? x)
  (if (null? x)
      #f
      (let ((curr (car x))
            (left (cdr x)))
        (cond ((null? (filter (lambda (y) (eq? curr y)) left))
               (cycle? left))
              (else #t)))))


; ok, that alone doesnt help
; now the idea is to make a copy of the input list
; and before adding any new element we check if it is equal
; to any of the already present ones in thee list

; eq? idea is working, now we only need to correctly lead us to base case

(define (cycle? x)
  (let ((curr (car x))
        (left (cdr x)))
    (cond ((null? (filter (lambda (y) (eq? curr y)) left))
           (cycle? left))
          (else #t))))

; the guess is that the mistake above was in that car of x was always changing
; we need to make iteration of initial car of x over the whole remains of the list

(define (c? x)
  (let ((curr (car x))
        (left (cdr x)))
    (if (any (lambda (y) (eq? curr y)) left)
        #t
        (c? left))))

; the one above is working but it will fail over non cycled list
; since '() will be passed as the argument to cdr when we go through the whole list
      

(define (c? x)
  (let ((curr (car x))
        (left (cdr x)))
    (cond ((any (lambda (y) (eq? curr y)) left) #t)
          ((null? left) #f)
          (else (c? left)))))

; still is not correct...

(define (c? x)
  (let ((curr (car x))
        (left (cdr x)))
    (cond ((any (lambda (y) (eq? x y)) left) #t)
          ((null? left) #f)
          (else (c? left)))))

; this one looks correct



; QUEUE

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))



;; Ex 3.21

(define (print-q q)
  (let ((last (car (rear-ptr q)))
        (l (car q)))
    ; Auxilary function
    (define (iter x end)
      (if (eq? (car x) end)
          x
          (cons (car x) (iter (cdr x) end))))
    ; Execute
    (iter l last)))



;; Ex 3.22

(define (make-queue)
  (let ((front-ptr ...)
        (rear-ptr ...))
    
    (define (dispatch m)
      ...)
    dispatch))


(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))



;; Ex 3.23

(define (make-dq) (cons '() '()))

(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))
(define (set-front-ptr! dq item) (set-car! dq item))
(define (set-rear-ptr! dq item) (set-cdr! dq item))

(define (empty-dq? dq) (null? (front-ptr dq)))

; the idea is to make it doubly pointed list, implemented as two lists
; (one for front-back and one for back-front)
; so that each time we add an item to the front,
; we mutate corresponding head of the front-back list as well as the tail of the back-front list
; each time we add an item to the back we do the corresponding action

; though I have to note that the original implementation doesn't care about the sequence itself
; it only updates the pointers, while the sequnce is left there hanging with all it's ever added elements

; Insertion
; front
(define (in-front-dq! dq item)
  (let ((new-pair (cons item (front-ptr dq))))
    (cond ((empty-dq? dq)
           (set-front-ptr! dq new-pair)
           (set-rear-ptr! dq new-pair)
           dq)
          (else
           ;(set-car! (front-ptr dq) new-pair)
           (cons new-pair dq)
           (set-front-ptr! dq new-pair)
           dq))))

; rear
(define (in-rear-dq! dq item)
  (let ((new-pair (cons item '())))
    (cond ((empty-dq? dq)
           (set-front-ptr! dq new-pair)
           (set-rear-ptr! dq new-pair)
           queqdue)
          (else
           (set-cdr! (rear-ptr dq) new-pair)
           (set-rear-ptr! dq new-pair)
           dq))))

; Deletion
; front
(define (del-front-dq! dq)
  (cond ((empty-dq? dq)
         (error "DELETE! called with an empty queue" dq))
        (else
         (set-front-ptr! dq (cdr (front-ptr dq)))
         dq)))

; rear
(define (del-rear-dq! dq)
  (cond ((empty-dq? dq)
         (error "DELETE! called with an empty queue" dq))
        (else
         ; how to make it O(1)?
         ; if I need to get to the next-to-last element in the sequence
         ;(set-front-ptr! dq (cdr (front-ptr queqdue)))
         queue)))




;; WILL BE BACK TO THIS EXERCISE
;; IT'S GREAT!

; Next time start with drawing box and pointer model
; And it think we should probably go for more object-orinted implementation
; with message passing and else.

; (e.g.)
; (myqueue 'rear 'del)
; (myqueue 'front 'del)
; (myqueue 'front 'ins item)
; (myqueue 'rear 'ins item)

; implementation somewhat like...
; i guess these will be encapsulated in a node, backbone, and dq objects...

; I assume in this model it WILL BE possible to hack through the underlying sequence
; and get it with all the elements ever added to it

(define (make-null-node)
  (cons (cons '() '()) (cons '() '())))

(set-node-front! node x)

(set-node-back! node x)

(define (update-node node x y)
  (set-node-front! node x)
  (set-node-back! node y))

(define (make-node x y)
  (update-node (make-null-node) x y))

(make-backbone) ;...

(define (add-node bb n) ... )

...

(define (ptr-x 'front/'back 'set/'del item)

  (ptr-y))




; DEQUE

; QUEUE for reference
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;;;;;;

(define (make-dq)
  (cons '() '()))

(define (f-ptr dq) ; will return the whole sequence as it is
  (car dq))

(define (r-ptr dq) ; will return the whole sequence stored in backwards
  (cdr dq))

(define (set-f-ptr! dq x)
  (set-car! dq x))

(define (set-r-ptr! dq x)
  (set-cdr! dq x))

(define (empty-dq? dq)
  (null? (f-ptr dq)))

(define (front-dq dq)
  (if (empty-dq? dq)
      (error "DEQUE is empty" dq)
      (car (f-ptr dq))))

(define (rear-dq dq)
  (if (empty-dq? dq)
      (error "DEQUE is empty" dq)
      (car (r-ptr dq))))

(define (in-front! dq item)
  (let ((new-pair (cons item '()))
        (new-x (cons item (car dq)))
        (new-y (append (cdr dq) (cons item '()))))
    (cond ((empty-dq? dq)
           (set-f-ptr! dq new-pair)
           (set-r-ptr! dq new-pair)
           dq)
          (else (set-f-ptr! dq new-x)
                (set-r-ptr! dq new-y)
                dq))))

(define (del-front! dq)
  (cond ((empty-dq? dq)
         (error "DEQUE is empty!" dq))
        (else
         (set-car! (f-ptr dq) '()) ; cleanup 
         (set-f-ptr! dq (cdr (f-ptr dq)))
         dq)))

(define (del-rear! dq)
  (cond ((empty-dq? dq)
         (error "DEQUE is empty!" dq))
        (else
         (set-car! (r-ptr dq) '()) ; cleanup 
         (set-r-ptr! dq (cdr (r-ptr dq)))
         dq)))

(define (in-rear! dq item)
  (let ((new-pair (cons item '()))
        ;(new-x (cons item ' ()))
        (new-y (cons item (cdr dq))))
    (cond ((empty-dq? dq)
           (set-f-ptr! dq new-pair)
           (set-r-ptr! dq new-pair))
          (else (set-cdr! (r-ptr d) (cons (r-ptr d) item))))))

(define (rev x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


(define d (make-dq))

(in-front! d 4)
(in-front! d 3)
(in-front! d 2)
(in-front! d 1)




(define (make-dq)
  (list (list)
        (list)))

(define (null-dq? dq)
  (or (null? (car dq))
      (null? (cdr dq))))


(define (set-f-ptr! dq x)
  (set-car! dq x))

(define (set-r-ptr! dq x)
  (set-cdr! dq x))

(define (f-ptr dq)
  (car dq))

(define (r-ptr dq)
  (cdr dq))

(define (front dq)
  (car (f-ptr dq)))

(define (rear dq)
  (car (r-ptr dq)))


; *-x stands for original (non-reversed) ordering
; *-y stands for the reversed

(define (in-f! dq x)
  (let ((new-x (append (list x) (car dq)))
        (new-y (append (cdr dq) (list x))))
    (cond ((null-dq? dq)
           (set-f-ptr! dq (list x))
           (set-r-ptr! dq (list x))
           dq)
          (else
           (set-f-ptr! dq new-x)
           (set-r-ptr! dq new-y)
           dq))))

(define (in-r! dq x)
  (let ((new-x (append (car dq) (list x)))
        (new-y (append (list x) (cdr dq))))
    (cond ((null-dq? dq)
           (set-f-ptr! dq (list x))
           (set-r-ptr! dq (list x))
           dq)
          (else
           (set-f-ptr! dq new-x)
           (set-r-ptr! dq new-y)
           dq))))

(define (del-f! dq)
  (cond ((null-dq? dq) (error "Empty DQ" dq))
        ((eq? (car (f-ptr dq)) (rear dq))
         (set-f-ptr! dq (list))
         (set-r-ptr! dq (list))
         dq)
        (else
         (set-f-ptr! dq (cdr (car dq)))
         dq)))

(define (del-r! dq)
  (cond ((null-dq? dq) (error "Empty DQ" dq))
        ((eq? (car (r-ptr dq)) (front dq))
         (set-f-ptr! dq (list))
         (set-r-ptr! dq (list))
         dq)
        (else
         (set-r-ptr! dq (cdr (cdr dq)))
         dq)))

(define (print-dq dq)
  ; need to check for eq? every element in every row...
  (cond ((null-dq? dq)
         '())
        ((eq? (caar dq) (rear dq))
         (rear dq))
        ((eq? (cadr dq) (front dq))
         (front dq))
        (else (print-dq ))))






;; TABLES

; one dimension
   
(define (make-table) (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)



; two dimensions

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


(define (lookuper table . keys)
  (if (null? (cdr keys)) ; base case hit
      (let ((curr-key (car keys)))
        (let ((curr-record (assoc curr-key (cdr table))))
          (if curr-record
              (cdr curr-record)
              false)))
      (lookuper (cdadr table) (cdr keys))))
; not quite done.....
              




(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)


; Table as an object

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1
                                              (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Ex 3.24
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (new-assoc key record)
  (cond ((null? records) false)
        ()))




; 3.3.4
; Digital circuits simulation

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
;ok
(and-gate a b c)
;ok
(inverter c e)
;ok
(and-gate d e s)
;ok

; (get-signal <wire>)

; (set-signal! <wire> <new value>)

; (add-action! <wire> <procedure of no arguments>)


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or x y)
  (cond ((or (= x 1) (= y 1)) 1)
        ((and (= x 0) (= y 0)) 0)
        (else (error "Incorect signal" x y))))

(define (or-gate x y output)
  (define (or-action)
    (let (new-value (logical-or (get-signal x) (get-signal y))))
    (after-delay or-gate-delay
                 (lambda () (set-signal! output new-value))))
  (add-action! x or-action)
  (add-cation! y or-action)
  'ok)


(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))


(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))



(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


; Now onto the agenda

; ● (make-agenda)
;    returns a new empty agenda.
;
; ● (empty-agenda? <agenda>)
;    is true if the specified agenda is empty.
;
; ● (first-agenda-item <agenda>)
;    returns the first item on the agenda.
;
; ● (remove-first-agenda-item! <agenda>)
;    modifies the agenda by removing the first item.
;
; ● (add-to-agenda! <time> <action> <agenda>)
;    modifies the agenda by adding the given action procedure
;    to be run at the specified time.
;
; ● (current-time <agenda>)
;    returns the current simulation time.


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action the-agenda))


(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))



; now onto implementing the agenda

; QUEUE for reference
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


; agenda intself

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action) (cdr segments)))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))



(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))






; Lecture notes

(define (animal node)
  (define (type node) (car node))
  (define (question node) (cadr node))
  (define (yespart node) (caddr node))
  (define (nopart node) (cadddr node))
  (define (answer node) (cadr node))
  (define (leaf? node) (eq? (type node) ’leaf))
  (define (branch? node) (eq? (type node) ’branch))
  (define (set-yes! node x)
    (set-car! (cddr node) x))
  (define (set-no! node x)
    (set-car! (cdddr node) x))
  (define (YorN)
    (let ((yn (read)))
      (cond ((eq?
              ((eq?
                (else
                 yn ’yes) #t)
               yn ’no) #f)
             (display "Please type YES or NO") (yorn)))))
  (display (question node))
  (display " ")
  (let ((yn (YorN)) (correct #f) (newquest #f))
    (let ((next (if yn (yespart node) (nopart node))))
      (cond ((branch? next) (animal next))
            (else (display "Is it a ") (display (answer next))
                  (display "? ")
                  (cond ((yorn) "I win!")
                        (else (newline)
                              (display "I give up, what is it? ") (set! correct (read))
                              (newline)
                              (define (make-branch q y n) (list ’branch q y n))
                              (define (make-leaf a) (list ’leaf a))
                              (display "Please tell me a question whose answer ") (display "is YES for a ")
                              (display correct)
                              (newline)
                              (display "and NO for a ") (display (answer next)) (display ".")
                              (newline)
                              (display "Enclose the question in quotation marks.") (newline)
                              (set! newquest (read))
                              (if yn
                                  (set-yes! node (make-branch newquest (make-leaf correct)
                                                              next)) (set-no! node (make-branch newquest
                                                                                                (make-leaf correct)
                                                                                                next))) "Thanks. Now I know better.")))))))
(define animal-list
  (make-branch "Does it have wings?"
               (make-leaf ’parrot) (make-leaf ’rabbit)))





