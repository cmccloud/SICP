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

;; Exercise 3.7

;; The only internal change we have to make to make-account
;; is the introduction of a new route called 'authorized? which
;; always returns 'authorized.
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
    ;; route-handler for authorized requests
    (define (router m)
      ;; resets attempts on successful login
      (set! attempts 0)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'authorized?) 'authorized)
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

;; The make joint procedure is responsible for handling multiple-passwords
;; Additionally, the account returned is itself compatible with make-joint
(define (make-joint account account-password new-password)
  (define (new-account p m)
    (if (eq? p new-password)
        (account account-password m)
        (account p m)))
  (if (eq? (account account-password 'authorized?) 'authorized)
      new-account
      (lambda x "Access to your account was denied.")))

;; tests
(define (test3-7)
  (define paul-account (make-account 100 'secret))
  (define chris-account (make-joint paul-account 'secret 'smile))
  (define dan-account (make-joint chris-account 'smile 'happy))
  (cond ((not (= ((paul-account 'secret 'withdraw) 40) 60)) #f)
        ((not (= ((chris-account 'smile 'withdraw) 60) 0)) #f)
        ((not (= ((dan-account 'happy 'deposit) 100) 100)) #f)
        ((not (= ((dan-account 'smile 'withdraw) 60) 40)) #f)
        (else #t)))

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

;; The Environmental Model of Evaluation

;; The environment model of procedure application can be summarized:
;; A procedure object is applied to a set of arguments by constructing a
;; frame, binding the formal parameters of the procedure to the arguments of
;; the call, and then evaluating the body of the procedure in the context of
;; the new environment constructed. The new frame has as it's enclosing
;; environment the environment part of the procedure object being applied.

;; A procedure is created by evaluating a lambda expression relative to a
;; given environment. The resulting procedure object is a pair consisting of
;; the text of the lambda expression and a pointer to the environment in which
;; the procedure was created.

;; Exercise 3.9
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

(define (fact-iter product counter max)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial n) (fact-iter 1 1 n))

;; Recursive Version
;; (factorial 5) creates a new environment E1 : n = 5 - Enclosing = global
;; Else condition:
;; (factorial 4) creates a new environment E2 : n = 4 - Enclosing = global
;; Else condition:
;; (factorial 3) creates a new environment E3 : n = 3 - Enclosing = global
;; Else condition:
;; (factorial 2) creates a new environment E4 : n = 2 - Enclosing = global
;; Else condition:
;; (factorial 1) creates a new environment E5 : n = 1 - Enclosing = global
;; If condition:
;; 1 is returned into E4, E5 closes
;; (* 2 1) returns 2 into E3, E4 closes
;; (* 3 2) returns 6 into E2, E3 closes
;; (* 4 6) returns 24 into E1, E2 closes
;; (* 5 24) returns 120 into the calling enviroment

;; Iterative Version
;; (factorial 5) creates a new environment E0 : n = 5 - Enclosing = global
;; fact-iter is unbound in E0 and so the interpreter moves to the global
;; environment to locate the procedure
;; (fact-iter 1 1 5) creates a new environment E1 : p = 1, c = 1, max = 5
;; (ignoring tail recursion for now)
;; Else condition:
;; (fact-iter ) creates a new environment E2 : p = 1, c = 2, max = 5
;; Else condition:
;; (fact-iter ) creates a new environment E3 : p = 2, c = 3, max = 5
;; Else condition:
;; (fact-iter ) creates a new environement E4 : p = 6, c = 4, max = 5
;; Else condition:
;; (fact-iter ) creates a new environment E5 : p = 24, c = 5, max = 5
;; Else condition:
;; (fact-iter ) creates a ne environment E6 : p = 120, c = 6, max = 5
;; If condition:
;; 120 is passed into E5, E6 closes
;; 120 is passed into E4, E5 closes
;; 120 is passed into E3, E4 closes
;; 120 is passed into E2, E3 closes
;; 120 is passed into E1, E2 closes
;; 120 is passed into E0, E1 closes
;; 120 is passed into the calling environment, E0 closes

;; Exercise 3.10

;; (let ((<var> <exp>)) <body>) is syntactic sugar for
;; ((lambda (<var>) <body>) <exp>)
;; With this in mind, analyze the following procedure
(define (make-withdraw initial)
  (let ((balance initial))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
;; Translates to:
(define (make-withdraw initial)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds"))) initial))

(define W1 (make-withdraw 100))
;; (make-withdraw 100) creates a new frame E1 : initial = 100
;; lambda is applied to 100, creating a new frame E2 : balance = 100
;; returned computational object has as its code a lambda and as its
;; environment [E2: balance = 100 [E1: initial = 100 [E0 - global ]]]
(W1 50)
;; (W1 50) creates a new frame F1 : amount = 50
;; balance is unbound relative to F1, so we move up environments until it
;; is located in it's first enclosing environment, E2
;; We change the value of balance in E2 to 50
;; and return 50

;; In the second case, there is an additional frame in the environment
;; where the original initial value is maintained, left unchanged

;; Exercise 3.11
;; (define acc (make-account 50))
;; Creates an environment E1 [balance = 50] in which there exist
;; 3 procedures, withdraw, deposit, dispatch, which have E1 as their
;; enclosing environment.
;; ((acc 'deposit) 40)
;; Creates an environment E2 in which dispatch attempts to return
;; the object referenced by deposit. Which is unbound in E2, but
;; is located in E1. Deposit then creates a new environment E3
;; in which amount is bound to 40. When attempting to locate the variable
;; balance, it is unbound in E3, but is found in E1, and set to 90
;; ((acc 'withdraw) 60)
;; Creates an environment E4 in which dispatch attempts to return the
;; object referenced by withdraw, which is unbound in E4, but referenced
;; in E1. Withdraw then creates a new environment E5 in which amount is
;; bound to 60 - it locates the variable balance in E1, and sets it to 30.

;; The local state for account is kept in E1
;; If we defined a second accound
;; (define acc2 (make-account 100))
;; This would create an environment E6 in which balance was bound to 100,
;; and there existed 3 procedures, withdraw, deposit, dispatch.
;; These three procedures share the same code as the procedures created in
;; frame E1, but their environments differ.

;; Exercise 3.12
;; (define x (list 'a 'b))
;; (define y (list 'c 'd))
;; (define z (append x y))
;; z -> (a b c d)
;; (cdr x) -> (b)
;; (define w (append! x y))
;; w -> (a b c d)
;; (cdr x) -> (b c d)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x) x)

(define z (make-cycle (list 'a 'b 'c)))

;; cell1 = [a, cell2]
;; cell2 = [b, cell3]
;; cell3 = [c, cell1]
;; Attempting to call (last-pair z) would result in an infinite loop

;; tests
(define (test3-13)
  (define x (make-cycle '(1 2 3)))
  (define (last-pair-safe x limit)
    (cond ((<= limit 0) "Exceeded Maximum Call Depth")
          ((null? (cdr x)) x)
          (else (last-pair-safe (cdr x) (- limit 1)))))
  (last-pair-safe x 10000))

;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;; Mystery reverses in place
;; v -> (a)
;; w -> (d c b a)
(define (reverse-in-place seq)
  (define (loop seq new-seq)
    (if (null? seq) new-seq
        (let ((temp (cdr seq)))
          (set-cdr! seq new-seq)
          (loop temp seq))))
  (loop seq '()))

;; Exercise 3.15 - diagraming

;; Exercise 3.16
(define (bad-count-pairs x)
  (if (not (pair? x)) 0
      (+ (bad-count-pairs (car x))
         (bad-count-pairs (cdr x))
         1)))
;; tests
(define (test3-16)
  (define a '(1 2 3))
  ;; 3 -> 4
  (define b (cons nil nil))
  (define c (list b b))
  ;; 3 -> 7
  (define d (cons nil nil))
  (define e (cons d d))
  (define f (cons e e))
  ;; infinite loop
  (define g (cons nil nil))
  (define h (cons g g))
  (define i (cons h h))
  (set-cdr! h h)
  (cond ((not (= 3 (bad-count-pairs a))) #f)
        ((not (= 4 (bad-count-pairs c))) #f)
        ((not (= 7 (bad-count-pairs f))) #f)
        ;; Infinite loop not being tested
        (else #t)))

;; Exercise 3.17
(define (contains? seq x)
  (cond ((null? seq) #f)
        ((eq? (car seq) x) #t)
        (else (contains? (cdr seq) x))))

(define (count-pairs x)
  (let ((S '())) ;; Set to store visited values
    (define (traverse x)
      (cond ((not (pair? x)) 0)
            ((contains? S x) 0)
            (else (set! S (cons x S))
                  (+ (traverse (car x))
                     (traverse (cdr x))
                     1))))
    (traverse x)))

;; tests
(define (test3-17)
  ;; With the same structures that broke count pairs before
  (define x (cons nil nil))
  (define y (list x x))
  ;; count-pairs of y should be 3
  (define i (cons nil nil))
  (define j (cons i i))
  (define k (cons j j))
  ;; count-pairs of k should be 3
  (define a (cons nil nil))
  (define b (cons a a))
  (define c (cons b b))
  (set-cdr! c c)
  ;; count-pairs of c should be 3
  (cond ((not (= 3 (count-pairs y))) #f)
        ((not (= 3 (count-pairs k))) #f)
        ((not (= 3 (count-pairs c))) #f)
        (else #t)))

;; Exercise 3.18
;; O(n) space complexity
(define (has-cycle? seq)
  (let ((S '()))
    (define (iterate seq)
      (cond ((null? seq) #f)
            ((contains? S seq) #t)
            (else (set! S (cons seq S))
                  (iterate (cdr seq)))))
    (iterate seq)))

;; Exercise 3.19
;; O(1) space complexity
(define (has-cycle2? seq)
  (let ((trail seq))
    (define (helper seq count)
      (cond ((null? seq) #f)
            ((eq? seq trail) #t)
            ((even? count)
             (set! trail (cdr trail))
             (helper (cdr seq) (+ count 1)))
            (else (helper (cdr seq) (+ count 1)))))
    (helper (cdr seq) 0)))

;; alternative implementation of O(1) solution
(define (has-cycle3? seq)
  (define (helper seq count trailer)
    (cond ((null? seq) #f)
          ((eq? seq trailer) #t)
          ((even? count)
           (helper (cdr seq) (+ count 1) (cdr trailer)))
          (else (helper (cdr seq) (+ count 1) trailer))))
  (helper (cdr seq) 0 seq))

;; tests
(define (test3-18)
  (define loop '(1 2 3))
  (define no-loop '(1 2 3))
  (set-cdr! (cddr loop) loop)
  (and (has-cycle2? loop)
       (has-cycle? loop)
       (has-cycle3? loop)
       (and (not (has-cycle? no-loop))
            (not (has-cycle2? no-loop))
            (not (has-cycle3? no-loop)))))

;; Exercise 3.20 - Diagram

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((node (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue node)
           (set-rear-ptr! queue node)
           queue)
          (else
           (set-cdr! (rear-ptr queue) node)
           (set-rear-ptr! queue node)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;; Exercise 3.21
;; The 'extra' rear item represents the updating pointer to the tail of the queue
;; The because the front of the queue is a series of cons cells, terminating with
;; an empty list, the interpreter prints it as a list.
;; To print only the elements currently in the queue, one would simply print
;; the front-part of the queue.
(define (print-queue queue)
  (display (front-ptr queue)))

;; Exercise 3.22
(define (make-queue)
  ;; This implementation never reveals the pointer to tail for simplified
  ;; displaying of queue.

  ;; internal state
  (let ((head '())
        (tail '()))

    ;; internal procedure definitions
    (define (empty?) (null? head))
    (define (set-head! x) (set! head x))
    (define (set-tail! x) (set! tail x))
    (define (insert! x)
      (let ((node (cons x '())))
        (cond ((empty?)
               (set-head! node)
               (set-tail! node)
               head)
              (else
               (set-cdr! tail node)
               (set-tail! node)
               head))))
    (define (delete!)
      (cond ((empty?) (error "DELETE! called with an empty queue" head))
            (else (set-head! (cdr head))
                  head)))
    ;; interface
    (define (router m)
      (cond ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            (else (lambda x "Unknown Operation MAKE-QUEUE" m))))

    router))

(define (delete-queue! queue)
  ((queue 'delete!)))

(define (insert-queue! queue item)
  ((queue 'insert!) item))

;; tests
(define (test3-22)
  (define Q (make-queue))
  (cond ((not (equal? (insert-queue! Q 'a) '(a))) #f)
        ((not (equal? (insert-queue! Q 'b) '(a b))) #f)
        ((not (equal? (delete-queue! Q) '(b))) #f)
        ((not (equal? (insert-queue! Q 'c) '(b c))) #f)
        ((not (equal? (delete-queue! Q) '(c))) #f)
        ((not (equal? (delete-queue! Q) '())) #f)
        (else #t)))

;; Exercise 3.23
(define (make-deque)
  ;; internal state
  (let ((head '())
        (tail '()))

    ;; internal procedure definitions
    ;; Node Procedures
    (define (make-node val)
      (cons val (cons '() '())))
    (define (next-node node) (cddr node))
    (define (prior-node node) (cadr node))
    (define (set-next-node! node x) (set-cdr! (cdr node) x))
    (define (set-prior-node! node x) (set-car! (cdr node) x))

    ;; Deque Procedures
    (define (empty?) (or (null? head) (null? tail)))
    (define (set-head! x) (set! head x))
    (define (set-tail! x) (set! tail x))
    (define (front-insert! x)
      (let ((node (make-node x)))
        (cond ((empty?)
               ;; set head and tail to the same node
               (set-head! node)
               (set-tail! node)
               head)
              (else
               (set-prior-node! head node)
               (set-next-node! node head)
               (set-head! node)
               head))))
    (define (rear-insert! x)
      (let ((node (make-node x)))
        (cond ((empty?)
               ;; set head and tail to the same node
               (set-head! node)
               (set-tail! node)
               head)
              (else
               (set-next-node! tail node)
               (set-prior-node! node tail)
               (set-tail! node)
               head))))
    (define (front-delete!)
      (cond ((empty?) (error "FRONT-DELETE! called on empty deqeue" head))
            (else
             (set-head! (next-node head))
             (cond ((empty?)
                    ;; if deque is empty, make sure to remove old tail pointers
                    (set-tail! '())
                    head)
                   (else
                    ;; remove old pointers on our new head
                    (set-prior-node! head '())
                    head)))))
    (define (rear-delete!)
      (cond ((empty?) (error "REAR-DELETE! called on empty dequue" head))
            (else
             (set-tail! (prior-node tail))
             (cond ((empty?)
                    ;; if deque is empty, make sure to remove old head pointers
                    (set-head! '())
                    head)
                   (else
                    ;; remove old pointers on our new tail
                    (set-next-node! tail '())
                    head)))))
    ;; interface
    (define (router m)
      (cond ((eq? m 'front-insert!) front-insert!)
            ((eq? m 'rear-insert!) rear-insert!)
            ((eq? m 'front-delete!) front-delete!)
            ((eq? m 'rear-delete!) rear-delete!)
            (else (lambda x "Unknown Operation Deque"))))

    router))

(define (format-deque deque)
  ;; format deque transforms a deque into a list for easy printing
  (if (null? deque) deque
      (cons (car deque)
            (format-deque (cddr deque)))))

(define (front-insert-deque! deque x)
  (format-deque ((deque 'front-insert!) x)))

(define (rear-insert-deque! deque x)
  (format-deque ((deque 'rear-insert!) x)))

(define (front-delete-deque! deque)
  (format-deque ((deque 'front-delete!))))

(define (rear-delete-deque! deque)
  (format-deque ((deque 'rear-delete!))))

;; tests
(define (test3-23)
  (define d (make-deque))
  ;; Insert should work from both the front and the rear
  (cond ((not (equal? (front-insert-deque! d 'a) '(a))) #f)
        ((not (equal? (front-insert-deque! d 'b) '(b a))) #f)
        ((not (equal? (rear-insert-deque! d 'c) '(b a c))) #f)
        ((not (equal? (rear-insert-deque! d 'd) '(b a c d))) #f)
        ;; Delete should work from both the front and the rear
        ((not (equal? (front-delete-deque! d) '(a c d))) #f)
        ((not (equal? (rear-delete-deque! d) '(a c))) #f)
        ((not (equal? (rear-delete-deque! d) '(a))) #f)
        ;; Both front and rear delete should remember to remove old
        ;; pointers when removing final item
        ((not (equal? (front-delete-deque! d) '())) #f)
        ((not (equal? (front-insert-deque! d 'a) '(a))) #f)
        ((not (equal? (rear-delete-deque! d) '())) #f)
        (else #t)))

;; Exercise 3.24
(define (make-table same-key?)
  ;; same-key? :: f (x,y) -> bool
  ;; Internal State
  (let ((local-table (list '*table*)))
    ;; Internal Procedures
    (define (find-in key seq)
      (cond ((null? seq) #f)
            ((same-key? key (caar seq)) (car seq))
            (else (find-in key (cdr seq)))))
    (define (lookup key-1 key-2)
      (let ((subtable
             (find-in key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (find-in key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (find-in key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (find-in key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    ;; interface
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))

    dispatch))

(define (insert-table! table k1 k2 v)
  ((table 'insert-proc!) k1 k2 v))

(define (lookup-table table k1 k2)
  ((table 'lookup-proc) k1 k2))

;; Exercise 3.25
(define (partial f . bound-args)
    (lambda args (apply f (append bound-args args))))

(define (make-table same-key?)
  ;; Internal Procedures
  (define (contains? record key)
    (cond ((null? record) false)
          ((same-key? key (caar record)) (car record))
          (else (contains? (cdr record) key))))
  (define (make-record keys value)
    (if (= (length keys) 1)
        (cons (car keys) value)
        (list (car keys) (make-record (cdr keys) value))))
  (define (lookup table keys)
    (let ((subtable (contains? (cdr table) (car keys))))
      (if subtable
          (if (= (length keys) 1)
              (cdr subtable)
              (lookup subtable (cdr keys)))
          false)))
  (define (insert! table keys value)
    (let ((subtable (contains? (cdr table) (car keys))))
      (if subtable
          (if (= (length keys) 1)
              (set-cdr! subtable value)
              (insert! subtable (cdr keys) value))
          (set-cdr! table
                    (cons (make-record keys value)
                          (cdr table))))))
  ;; Internal State
  (let ((local-table (list '*table*)))
    ;; Interface
    (define (dispatch m)
      (cond ((eq? m 'insert-proc!) (partial insert! local-table))
            ((eq? m 'lookup-proc) (partial lookup local-table))
            (else (error "Unknown procedure TABLE" m))))

    dispatch))

(define (insert-table! table keys value)
  ((table 'insert-proc!) keys value))

(define (lookup-table table keys)
  ((table 'lookup-proc) keys))

;; tests
(define (test3-25)
  (let* ((table (make-table equal?))
        (find (partial lookup-table table))
        (put (partial insert-table! table)))
    (put '(a a a) 'aaa)
    (put '(a a b) 'aab)
    (put '(a a c) 'aac)
    (put '(a b) 'ab)
    (put '(a c) 'ac)
    (put '(b) 'b)
    (put '(c) 'c)
    (cond ((not (equal? (find '(a a a)) 'aaa)) #f)
          ((not (equal? (find '(a a b)) 'aab)) #f)
          ((not (equal? (find '(a a c)) 'aac)) #f)
          ((not (equal? (find '(a b)) 'ab)) #f)
          ((not (equal? (find '(a c)) 'ac)) #f)
          ((not (equal? (find '(b)) 'b)) #f)
          ((not (equal? (find '(c)) 'c)) #f)
          (else #t))))

