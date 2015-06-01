;; Modularity, Objects, and State

;; Exercise 3.1
(define (make-accumulator initial)
  (lambda (n) (set! initial (+ initial n)) initial))

;; tests
(define (test3-1)
  ;; accumulators should track state
  (define acc (make-accumulator 5))
  (define A (acc 10))
  (define B (acc 10))
  (define C (acc 10))
  ;; new accumulators should have their own local state
  (define acc-2 (make-accumulator 5))
  (define A2 (acc-2 5))
  (define B2 (acc-2 5))
  (define C2 (acc-2 5))
  (and (equal? '(15 25 35) (list A B C))
       (equal? '(10 15 20) (list A2 B2 C2))))

;; Exercise 3.2
(define (make-monitored fn)
  (let ((count 0))
    (define (monitor x)
      (if (eq? x 'how-many-calls?)
          count
          (begin (set! count (+ count 1))
                 (fn x))))
    monitor))

;; tests
(define (test3-2)
  (define (inc x) (+ x 1))
  ;; make monitored should track the number of calls
  (define m-inc (make-monitored inc))
  (let* ((one (m-inc 0))
         (two (m-inc 1))
         (five (m-inc 4))
         (count-three (m-inc 'how-many-calls?))
         (four (m-inc 3))
         (count-four (m-inc 'how-many-calls?)))
    (equal? '(1 2 5 3 4 4)
            (list one two five count-three four count-four))))

;; Exercise 3.3
(define (make-account balance password)
  ;; internal procedures
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;; route-handler for incoming requests
  (define (router m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else "Unknown Procedure MAKE-ACCOUNT" m)))
  ;; handles unauthorized requests
  (define (deny .x) "Incorrect password")
  ;; authentication module
  (define (auth p m)
    (if (eq? p password)
        (router m)
        deny))
  auth)

;; tests
(define (test3-3)
  (define account (make-account 100 'secret))
  ;; make-account should work if the user supplies the correct password
  (cond ((not (= 40 ((account 'secret 'withdraw) 60))) #f)
        ((not (= 80 ((account 'secret 'deposit) 40))) #f)
        ;; make-account should respond with a message, even if the user
        ;; supplies an additional argument
        ((not (equal? ((account 'oops 'withdraw) 80) "Incorrect password")) #f)
        ;; on a bad request, the balance of the account shouldn't change
        ((not (= 80 ((account 'secret 'withdraw) 0))) #f)
        (else #t)))

;; Exercise 3.4
(define (make-account balance password)
  ;; internal state
  (let ((attempts 0))
    ;; procedures
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    ;; route-handler for incoming requests
    (define (router m)
      ;; resets attempts on successful login
      (set! attempts 0)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else "Unknown Procedure MAKE-ACCOUNT" m)))
    ;; denial procedures
    (define (deny .x) "Incorrect password")
    (define (call-cops .x) "Calling Police!")
    ;; denial router
    (define (deny-handler)
      (set! attempts (+ attempts 1))
      (if (>= attempts 7)
          call-cops
          deny))
    ;; authentication module
    (define (auth p m)
      (if (eq? p password)
          (router m)
          (deny-handler)))
    auth))

;; tests
(define (test3-4)
  (define account (make-account 100 'secret))
  ;; make-account should still pass old tests
  (cond ((not (test3-3)) "Error - Did not pass test 3-3")
        ;; make-account should throw 6 errors and then call cops
        ((not (equal? ((account 'thief 'withdraw) 100) "Incorrect password"))
         "Error - Expected 'Incorrect Password")
        ((not (equal? ((account 'thief 'withdraw) 100) "Incorrect password"))
         "Error - Expected 'Incorrect Password")
        ((not (equal? ((account 'thief 'withdraw) 100) "Incorrect password"))
         "Error - Expected 'Incorrect Password")
        ((not (equal? ((account 'thief 'withdraw) 100) "Incorrect password"))
         "Error - Expected 'Incorrect Password")
        ((not (equal? ((account 'thief 'withdraw) 100) "Incorrect password"))
         "Error - Expected 'Incorrect Password")
        ((not (equal? ((account 'thief 'withdraw) 100) "Incorrect password"))
         "Error - Expected 'Incorrect Password")
         ((not (equal? ((account 'thief 'withdraw) 100) "Calling Police!"))
         "Error - Expected 'Calling Police!'")
         ;; make-account should reset attempt counter on successful login
         ((not (= ((account 'secret 'withdraw) 40) 60))
          "Error - Expected balance of 60")
         ((not (equal? ((account 'thief 'withdraw) 40) "Incorrect password"))
          "Error - succesfull login should have reset attempt counter")
        (else #t)))

;; Exercise 3.5
;; Generic monte carlo simulation runner
;; experiment must be a thunk
(define (monte-carlo trials experiment)
  ;; trials = n:: 0 <= n
  ;; experiment = fn() :: -> bool
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 n)
  ;; P = fn (x,y) :: -> bool
  ;; x1, x2, y1, y2 = n
  (* (* (- x2 x1) (- y2 y1))
     (monte-carlo n (lambda () (P (random-in-range x1 x2)
                                  (random-in-range y1 y2))))))

(define (estimate-pi n)
  (estimate-integral (lambda (x y)
                       (< (+ (square x) (square y)) 1.0))
                     -1.0 1.0 -1.0 1.0 n))

;; tests
(define (test3-5)
  (list (estimate-pi 100)
        (estimate-pi 1000)
        (estimate-pi 2000)))

;; Exercise 3.6
(define (new-rand n)
  ;; uses the linear congruential generator
  (let ((a (expt 2 31))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a n) c) m)))

(define rand
  (let ((x 632))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (new-rand x)) x))
            ((eq? m 'reset)
             (lambda (n) (set! x n)))
            (else (error "Invalid Procedure RAND" n))))))

;; tests
(define (test3-6)
  ;; Rand should be able to produce repeatable sequences of random numbers
  (let* ((a (rand 'generate))
        (b (rand 'generate))
        (c ((rand 'reset) a))
        (d (rand 'generate)))
    (= b d)))

;; Exercise 3.8
(define f
  (let ((continue #t))
    (lambda (n)
      (if continue
          (begin (set! continue #f) n)
          0))))

(define (once fn)
  (let ((continue #t))
    (lambda x
      (if continue
          (begin (set! continue #f)
                 (apply fn x))
          0))))

(define (identity x) x)

;; tests
(define (test3-8)
  (define x (once identity))
  (define a (x 1))
  (define y (once identity))
  (define b (y 0))
  (and (= (+ a (x 0)) 1)
       (= (+ b (y 1)) 0)))
