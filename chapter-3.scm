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

;; Exercise 3.26
(define (make-binary-tree pred?)
  ;; pred? must be a function which takes in a node value as it's first
  ;; argument and a target value as it's second argument and returns either:
  ;; 0 if the node value is equal to the target
  ;; 1 if the node value is greater than the target value
  ;; -1 if the node value is less than the target value

  ;; Internal Procedures
  (define (make-tree value left right)
    (list value left right))
  (define (tree-value tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (set-value! tree value) (set-car! tree value))
  (define (set-left! tree value) (set-car! (cdr tree) value))
  (define (set-right! tree value) (set-car! (cddr tree) value))
  (define (contains? tree target)
    (if (or (null? tree)
            (null? (tree-value tree))) false
        (let ((comparison (pred? (tree-value tree) target)))
          (cond ((= comparison 0) (tree-value tree))
                ((= comparison 1)
                 (contains? (left-branch tree) target))
                (else (contains? (right-branch tree) target))))))
  (define (insert-tree! tree value)
    (if (null? (tree-value tree)) (set-value! tree value)
        (let ((comparison (pred? (tree-value tree) value)))
          (cond ((= comparison 0) (set-value! tree value))
                ((= comparison 1)
                 (if (null? (left-branch tree))
                     (set-left! tree (make-tree value '() '()))
                     (insert-tree! (left-branch tree) value)))
                (else
                 (if (null? (right-branch tree))
                     (set-right! tree (make-tree value '() '()))
                     (insert-tree! (right-branch tree) value)))))))
  (let ((local-state (make-tree '() '() '())))
    (define (dispatch m)
      (cond ((eq? m 'lookup) (partial contains? local-state))
            ((eq? m 'insert!) (partial insert-tree! local-state))
            ((eq? m 'report) local-state)
            (else (error "Unknown Procedure TREE" m))))

    dispatch))

(define (lookup-tree tree value)
  ((tree 'lookup) value))

(define (insert-tree! tree value)
  ((tree 'insert!) value))

(define (make-table)
  ;; Internal Procedures
  (define (make-record key value)
    (cons key value))
  (define (key-record record) (car record))
  (define (val-record record) (cdr record))
  (define (pred? x y)
    (cond ((= (key-record x) (key-record y)) 0)
          ((> (key-record x) (key-record y)) 1)
          (else -1)))
  ;; Internal State
  (let ((local-state (make-binary-tree pred?)))
    (define (insert! key value)
      (insert-tree! local-state (make-record key value)))
    (define (lookup key)
      (let ((result (lookup-tree local-state (make-record key '()))))
        (if result (val-record result) result)))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'report) (local-state 'report))
            (else (error "Unknown Procedure TABLE" m))))

    dispatch))

(define (table-lookup table key)
  ((table 'lookup) key))
(define (table-insert! table key value)
  ((table 'insert!) key value))

;; tests
(define (test3-26)
  (let* ((my-table (make-table))
         (put (partial table-insert! my-table))
         (find (partial table-lookup my-table)))
    (put 4 'a)
    (put 3 'b)
    (put 6 'c)
    (put 5 'd)
    (put 7 'e)
    (put 1 'f)
    (put 2 'g)
    (put 4 'l)
    (cond ((not (equal? (find 4) 'l)) #f)
          ((not (equal? (find 3) 'b)) #f)
          ((not (equal? (find 6) 'c)) #f)
          ((not (equal? (find 5) 'd)) #f)
          ((not (equal? (find 7) 'e)) #f)
          ((not (equal? (find 1) 'f)) #f)
          ((not (equal? (find 2) 'g)) #f)
          (else #t))))

;; Exercise 3.27 - Diagram
(define (memoize f)
  (let ((table (make-table)))
    (lambda x
      (let ((previous-result (lookup x table)))
        (or previous-result
            (let ((result (apply f x)))
              (insert! x result table)
              result))))))


;; 3.3.4 - Event Driven Digital Circuit Simulator
;; Based on Diagram in Figure 3.25
;; A B S C are arguments to the half-adder - They represent the interface
;; Similarly, in the diagram, it is wires a b s c that extend beyond the
;; boundaries of the half-adder; wires d and e are entirely internal
;; to the procedure
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; A full-adder is a basic circuit element used in adding two binary numbers
;; a and b are the bits at the corresponding positions in the two numbers to
;; be added, and c-in is the carry bit from the addition one place to the right
;; The circuit generates sum, where is the sum bit in the corresponding position
;; and c-out, which is the carry bit to be propogated to the left
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; (get-signal wire) -> returns the current value of the signal on the wire
;; (set-signal! wire new-value) -> changes the value on the signal to new value
;; (add-action! wire proc) -> asserts that the designated procedure of no
;; arguments should be run whenever the signal on the wire changes value
;; (after-delay delay proc) -> runs procedure after given delay
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" (list s1 s2)))))

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" (list s1 s2)))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

;; Truth table
;; (t,f) -> f
;; (f, t) -> f
;; (t, t) -> t
;; (f, f) -> f
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Exercise 3.29
;; Truth Table
;; (t,f) -> t
;; (f,t) -> t
;; (t, t) -> t
;; (f, f) -> f

(define (compound-or-gate a1 a2 output)
  (let ((b1 (make-wire)) (b2 (make-wire)) (b3 (make-wire)))
    ;; if either a1 or a2 changes state
    ;; one inverter delay later b1 or b2 will change state
    ;; if either b1 or b2 change state
    ;; one and-gate delay later b3 will change state
    ;; if b3 changes state
    ;; one inverter delay later output changes state
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 b3)
    (inverter b3 output)))

;; Example:
;; Initial State
;; a1 = 0, a2 = 0
;; b1 = 1, b2 = 1
;; a1 -> 1
;; b1 -> 0
;; (and-gate b1 b2) -> (and-gate 0 1) -> 0
;; b3 -> 0
;; output -> 1

;; Exercise 3.30
;; a b s :: list of wires
;; c :: wire
(define (ripple-carry-adder a b s c)
  (let ((c-in (make-wire)))
    (if (null? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c)))
;; The delay needed to obtains the complete output is
;; equal to (* n full-adder-delay) ::
;; (* n (+ (* 2 half-adder-delay) or-gate-delay))
;; half-adder-delay =
;; (+ and-gate-delay (max (+ and-gate-delay inverter-delay) or-gate-delay))
;; n-bit-ripple-carrier-full-delay =
;; (* n or-gate-delay
;;      (+ and-gate-delay
;;         (max (+ and-gate-delay inverter-delay)
;;              or-gate-delay)))

;; Representing Wires
;; helpers
(define call-each (partial for-each (lambda (p) (p))))

;; wire definition
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures)
                 'done)))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
            (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))

    dispatch))

;; handlers
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; Agenda Definition and specification
;; (make-agenda) returns a new empty agenda
;; (empty-agenda? agenda) -> bool
;; (first-agenda-item agenda) -> returns first item in agenda
;; (remove-first-agenda-item! agenda) -> modifies the agenda by removing the
;; first item
;; (add-to-agenda! time action agenda) -> modifies the agenda by adding the
;; given action procedure to be run at the specified time
;; (current-time agenda) -> returns the current simulation time
(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))
;; Exercise 3.31 - Diagram

;; Implementing the Agenda

;; Time Segments :: (time, queue)
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time segment)
  (car segment))

(define (segment-queue segment)
  (cdr segment))

;; Agenda - One dimensional table of time segments, with time at head
;; (time, segments)
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
;; An agenda is empty if it has no time segments
(define (empty-agenda?)
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
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     rest))
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
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; =============================================================================
;; Project Idea
;; Refactoring the Circuit Simulator to use a generic
;; Events mix-in procedure
;; Mix in procedure will look something like this
(define (events-mix-in object)
  ;; Internal Event Table
  ;; 2d table with each event corresponding to a list of one or more
  ;; procedures to be run in response to that event
  (define (make-event-table) (list '*event-table*))
  (define (add-event! table key procedure)
    (let ((entry (find-in table key)))
      (if entry
          (set-cdr! entry (cons procedure (cdr entry)))
          (set-cdr! table (cons (list key procedure)
                                (cdr table))))))
  (define (find-in table key)
    (define (search entries)
      (cond ((null? entries) #f)
            ((equal? key (car (car entries))) (car entries))
            (else (search (cdr entries)))))
    (search (cdr table)))

  (let ((events (make-event-table)))
    (define (on! event response)
      (add-event! events event response))

    (define (trigger event)
      (let ((handlers (find-in events event)))
        (if handlers
            (for-each (lambda (proc) (proc object)) (cdr handlers)))))

    (define (dispatch m)
      (cond ((eq? m 'on!) on!)
            ((eq? m 'trigger) trigger)
            (else (object m))))

    dispatch))

(define (on! event response . subscribers)
  (for-each (lambda (subscriber) ((subscriber 'on!) event response)) subscribers))

(define (trigger event . publishers)
  (for-each (lambda (publisher) ((publisher 'trigger) event)) publishers))

;; Wires would look something like
(define (make-wire)
  (let ((signal-value 0))
    (define (set-signal! new-value)
      (set! signal-value new-value))
    (define (dispatch m)
      (cond ((= m 'get-signal) signal-value)
            ((= m 'set-signal!) set-signal!)
            (else (error "Unknown operation: WIRE" m))))

    (events-mix-in dispatch)))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  (if (not (= new-value (get-signal wire)))
      (begin ((wire 'set-signal!) new-value)
             (trigger 'signal-change wire))))

;; a function box might look like this
(define (and-gate a1 a2 output)
  (define (and-action-procedure . caller)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (on! 'signal-change and-action-procedure a1 a2)
  (and-action-procedure)
  'ok)

;; the agenda would be left unchanged
;; =============================================================================

;; Exercise 3.32 - TODO

;; 3.3.5 Propagation of Constraints
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  ;; Specifies that the quantities a1, a2, and sum must be related by the
  ;; equation :: a1 + a2 = sum
  (define (has-values? x y)
    (and (has-value? x) (has-value? y)))

  (define (set-sum! augend addend sum caller)
    (set-value! sum (+ (get-value augend) (get-value addend)) caller))

  (define (set-difference! minuend subtrahend difference caller)
    (set-value! difference (- (get-value minuend) (get-value subtrahend)) caller))

  (define (process-new-value)
    (cond ((has-values? a1 a2) (set-sum! a1 a2 sum me))
          ((has-values? a1 sum) (set-difference! sum a1 a2 me))
          ((has-values? a2 sum) (set-difference! sum a2 a1 me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier x y product)
  ;; Specifies that the quantities x, y, and product must be related by the
  ;; equation :: x * y = product
  (define (mul-by-zero? a b)
    (or (and (has-value? a) (= (get-value a) 0))
        (and (has-value? b) (= (get-value b) 0))))

  (define (has-values? x y)
    (and (has-value? x) (has-value? y)))

  (define (set-product! mul1 mul2 product caller)
    (set-value! product (* (get-value mul1) (get-value mul2)) caller))

  (define (set-quotient! numer denom quotient caller)
    (set-value! quotient (/ (get-value numer) (get-value denom)) caller))

  (define (process-new-value)
    (cond ((mul-by-zero? x y) (set-value! product 0 me))
          ((has-values? x y) (set-product! x y product me))
          ((has-values? x product) (set-quotient! product x y me))
          ((has-values? y product) (set-quotient! product y x me))))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! x me)
    (forget-value! y me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))

  (connect x me)
  (connect y me)
  (connect product me)
  me)

(define (constant value connector)
  ;; Specifies that a given connector take a given value and refuses
  ;; requests to alter that value
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)

  me)

(define (probe name connector)
  ;; Prints a message about the setting or unsetting of the designated
  ;; connector
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown requst: PROBE" request))))

  (connect connector me)
  me)

(define (for-each-except exception proc col)
  (cond ((null? col) 'done)
        ((eq? exception (car col)) (for-each-except exception proc (cdr col)))
        (else (proc (car col))
              (for-each-except exception proc (cdr col)))))

(define (make-connector)
  ;; value :: the current value of the connector
  ;; informant :: the object taht set the connector's value
  ;; constraints :: a list of the constraints in which the connector
  ;; participates
  (let ((value false) (informant false) (constraints '()))

    (define (set-my-value new-value setter)
      (cond ((not (has-value? me))
             (set! value new-value)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value new-value))
             (error "Contradiction" (list value new-value)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?) (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))

    me))

(define (has-value? connector) (connector 'has-value?))

(define (get-value connector) (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; Exercise 3.33
;; as new primitive
(define (averager a b c)
  ;; Specifies that the three connectors a, b, and c be related by the equation
  ;; c = (a + b) / 2
  (define (has-values? x y)
    (and (has-value? x) (has-value? y)))

  (define (set-average! n m avg caller)
    (set-value! avg (/ (+ (get-value n) (get-value m)) 2) caller))

  (define (set-participant c b a caller)
    (set-value! a (- (* 2 (get-value c))
                     (get-value b))
                caller))

  (define (process-new-value)
    (cond ((has-values? a b) (set-average! a b c me))
          ((has-values? a c) (set-participant! c a b me))
          ((has-values? b c) (set-participant! c b a me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request AVERAGER" request))))

  (connect a me)
  (connect b me)
  (connect c me)
  me)

;; using multiplier, adder, and consants
(define (averager a b c)
  ;; Specifies that the three connectors, a, b, and c be related by the equation
  ;; (a + b) = 2 * c
  (let ((d (make-connector))
        (e (make-connector)))
    (constant 2 d)
    (adder a b e)
    (multiplier d c e)
    'ok))

;; Exercise 3.34
;; Proposed Squarer (define (squarer a b) (multiplier a a b))
;; Assuming a system in which both a and b are value-less connectors,
;; in the above solution, a change to a would correctly propagate to b
;; however a change to be would not trigger a change in a, because
;; relative to the multiplier, both x and y remain value-less

;; Exercise 3.35

(define (squarer a b)
  ;; Specifies that connectors a and b be related by the equation
  ;; a * a = b
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me)))
    (if (has-value? a)
        (set-value! b (square (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown operation SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;; Exercise 3.36 - Diagram

;; Exercise 3.37
(define (c+ x y)
  ;; x + y = z
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  ;; x - y = c -> c + y = x
  (let ((c (make-connector)))
    (adder c y x)
    c))

(define (c* x y)
  ;; x * y = z
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  ;; x / y = z -> z * y = x
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

;; Run simulation
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe 'C C)
(probe 'F F)
(set-value! F 92 'user)

;; Exercise 3.38
;; a.
;; + 10, - 20, - half :: 45
;; + 10, - half, - 20 :: 35
;; - 20, + 10, - half :: 45
;; - 20, - half, + 10 :: 50
;; - half, + 10, - 20 :: 40
;; - half, - 20, + 10 :: 40

;; b. - Diagram

;; Parallel-execute implementation for concurrency exercises

(define disallow-preempt-current-thread
  (access disallow-preempt-current-thread
	  (->environment '(runtime thread))))

(define allow-preempt-current-thread
  (access allow-preempt-current-thread
	  (->environment '(runtime thread))))

(define (parallel-execute . thunks)
  (let ((my-threads '()))

    ;; Thread Execution
    (define (create-process proc)
      (let ((thread (create-thread #f proc)))
        (detach-thread thread)
        thread))

    (define (run)
      (set! my-threads (map create-process thunks))
      unspecific)

    ;; Thread Terminator
    (define (event) (exit-current-thread 'RIP))

    (define (thread-status thread event)
      (lambda ()
        (case (thread-execution-state thread)
          ((STOPPED) (restart-thread thread #t event))
          ((DEAD) unspecific)
          (else (signal-thread-event thread event)))))

    (define (kill-thread thread)
      (without-interrupts (thread-status thread event)))

    (define (terminator)
      (without-interrupts
       (lambda ()
         (for-each kill-thread my-threads)
         (set! my-threads '())
         unspecific)))

    (without-interrupts run)
    terminator))

;; Tests
(define a 10)
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(allow-preempt-current-thread)
(define t (parallel-execute (lambda () (set! a (fib 30))) (lambda () (set! a 0))))
;; (t) to terminate threads

;; Exercise 3.39
;; 101 :: P[1] sets x to 100 and then P[2] sets x to 101
;; 121 :: P[2] sets x to 11 adn then P[1] sets x to 121
;; 100 :: P[1] computes the value of 100 while x = 10, P[2] then sets x to 11,
;; and finally P[1] sets x to 100

;; Exercise 3.40
;; 1000000 :: P[1] sets x to 100 and then P[2] sets x to 1000000 OR
;; P[2] sets x to 100 and then P[1] sets x to 1000000
;; 100000 :: P[2] accesses x once at a value of 10, and twice at a value of 100
;; and sets to 100000
;; 10000 :: P[1] accesses x once at a value of 10, and then a second time at a value of 1000
;; and sets x to 10000 OR
;; P[2] accesses x twice at 10 and once at 100 and sets x to 10000
;; 1000 :: P[1] sets x to 100 - P[2] accesses x three times at 10
;; and sets x to 1000
;; 100 :: P[1] accesses x twice at 100, P[2] sets x to 1000, and then P[1] sets x to 100

;; Using the serializer, the only possible result is 100000
;; Either 100 * 100 * 100 or 1000 * 1000

;; Exercise 3.41
;; I don't share Ben's concern

;; Exercise 3.42
;; The answer here depends on how exactly the serializer determines what
;; it means for a member of the serialized set to be executing -
;; Using Ben's changes, there will now exist exactly two procedures
;; as members of the serialized set - while it's still true that withdraw
;; and deposit would be unable to run concurrently, it might be the case that
;; multiple calls to either would be able to run simultaneously
;; E.G. given a serialized set S of exactly one member, f
;; one possibility is that, prior to executing f, the procedure
;; checks if f is being executed. Another possibility is that prior
;; to the execution of f, the procedure checks if any other members
;; of S are being executed.

;; We can come up with a test for this:

;; We will attempt to make two parallel calls to test-one
;; The first call to time-varying op (which might represent a network procedure)
;; will execute more slowly than the second.
;; Prior to this, however, the value of x will have already
;; been passed to time-varying-operation for the computation of the result
;; If test-one may not be run multiple times:
;; The result will be that (p1 10) -> 100 -> (p2 100) -> 10000
;; And Ben's proposed changes will be safe, I think.
;; If test-one may be run multiple times:
;; The result will be that (p1 10) (p2 10) -> 100 -> 100
;; And Ben's proposed changes will not be safe.

(define (test3-42)
  (let ((protected (make-serializer))
        (network-issues true))
    (define x 10)
    (define test-one (protected (lambda () (set! x (time-varying-op x)))))
    (define (time-varying-op x)
      (if network-issues
          (begin
            (set! network-issues false)
            (fib 31)
            (display "First call completed!")
            (newline)
            (* x x))
          (begin
            (fib 25)
            (display "Second call completed")
            (newline)
            (* x x))))
    (parallel-execute test-one test-one)))

;; Exercise 3.43 - Diagram

;; Exercise 3.44
;; I don't think we need a more sophisticated implementation in this case
;; The fundamental difference between this and the balance exchanger is that
;; we're no longer querying the local mutable state of the account
;; prior to modifying it.

;; Exercise 3.45
;; Now, a call to serialized exchange procedures a procedure which is blocking
;; for both account1 and account2 - when this procedure executes,
;; it's internal calls to the withdraw procedures from the respective accounts
;; will be unable to proceed.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

;; Exercise 3.46 - Diagram

;; Exercise 3.47
;; a.
(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (count 0))

    (define (acquire)
      ;; If the maximum number of leases has been reached
      ;; We release the mutex and retry the acquire procedure
      (cond ((= count n)
             (the-mutex 'release)
             (the-semaphore 'acquire))
            ;; Otherwise we increment the number of active leases
            ;; And release the mutex
            (else
             (set! count (inc count))
             (the-mutex 'release))))

    (define (release)
      (set! count (max 0 (dec count)))
      (the-mutex 'release))

    (define (the-semaphore m)
      ;; Prior to running any operation we first acquire the mutex
      ;; Each operation is responsible for then releasing the mutex
      ;; on completion
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire)
             (acquire))
            ((eq? m 'release)
             (the-mutex 'acquire)
             (release))
            (else (error "Unknown Procedure SEMAPHORE" m))))

    the-semaphore))
;; b.
(define (make-semaphore n)

  (define (clear! cell)
    (set-car! cell false))

  (define (test-and-set! cell)
    (if (car cell) true
        (begin
          (set-car! cell true)
          false)))

  (let ((cell (list false))
        (count 0))

    (define (acquire)
      (if (test-and-set! cell) (acquire)))

    (define (the-semaphore m)
      (cond ((equal? m 'acquire)
             (acquire)
             (cond ((= count n)
                    (clear! cell)
                    (the-semaphore 'acquire))
                   (else
                    (set! count (inc count))
                    (clear! cell))))
            ((equal? m 'release)
             (acquire)
             (set! count (max 0 (dec count)))
             (clear! cell))))

    the-semaphore))

;; Exercise 3.48 - TODO

;; Exercise 3.49 - TODO

;; 3.5 - Streams
;; Stream Primitives
;; stream-car, stream-cdr, cons-stream, stream-null?, the-empty-stream

(define (_stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (_stream-ref (stream-cdr s) (dec n))))

(define (_stream-map f s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (f (stream-car s))
                   (_stream-map f (stream-cdr s)))))

(define (_stream-for-each f s)
  (if (stream-null? s) nil
      (begin (f (stream-car s))
             (_stream-for-each f (stream-cdr s)))))

(define (display-line x)
  (newline) (display x))

(define (display-m x)
  (display x) (display " "))

(define (display-stream s)
  (display "STREAM : ( ")
  (stream-for-each display-m s)
  (display ")"))

;; At this level of abstraction, the critical distinction between a stream
;; and a list is that with streams, the cdr is evaluated when it is accessed
;; by stream-cdr rather than when the stream is constructed by cons-stream
(define (_cons-stream a b) (cons a (delay b)))

(define (_car-stream s) (car s))

(define (_cdr-stream s) (force (cdr s)))

;; Note that the actual implementation of cons-stream must be a special form
;; in order to prevent the second argument in a call from being evaluated
;; prior to binding it with the formal parameter b. For the same reason
;; force must also be a special form.

(define (stream-range n m)
  (if (> n m) the-empty-stream
      (cons-stream n (stream-range (inc n) m))))

(define (stream-every p? s)
  (cond ((stream-null? s) #t)
        ((p? (stream-car s)) (stream-every p? (stream-cdr s)))
        (else #f)))

(define (_stream-filter p? s)
  (if (stream-null? s) the-empty-stream
      (let ((x (stream-car s)))
        (if (p? x) (cons-stream x (_stream-filter p? (stream-cdr s)))
            (_stream-filter p? (stream-cdr s))))))

;; Example Exercise
(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (prime? n)
  (cond ((<= n 1) #f)
        ((= n 2) #t)
        (else (stream-every (lambda (d) (= (gcd n d) 1))
                     (stream-range 2 (inc (floor (sqrt n))))))))

(define (stream-primes n m)
  ;; Returns a list of all of the primes between n and m, inclusive.
  (stream-filter prime? (stream-range n m)))

(define my-primes (cons-stream 10007 (stream-primes 10008 10100)))

;; my-primes is now (cons 10007 (delay (stream-primes 10008 10100)))
(stream-ref my-primes 1)
;; stream-ref forces the evaluation of the cdr of my-primes
;; (stream-primes 10008 10100)
;; (stream-filter prime? (stream-range 10008 10100))
;; (stream-filter prime? (cons 10008 (delay (stream-range 10009 10100))))
;; (prime? 10008) -> f
;; (stream-filter prime? (stream-range 10009 10100))
;; (stream-filter prime? (cons 10009 (delay (stream-range 10010 10100))))
;; (prime? 10009) -> t :: returns new car to stream-primes
;; (stream-primes 10008 10100) -> (cons 10009 (delay (stream-filter? prime...)))
;; (stream-ref (cons 10009 (delay....)) 0) -> 10009
;; We have done exactly as much work as we needed and no more

;; Implementing delay and force
;; Delay can be a special form such that (delay <exp>) is syntactic sugar for
;; (lambda () <exp>)
;; Force is simply (define (force x) (x))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; Exercise 3.50
(define (stream-map f . streams)
  (if (stream-null? (car streams)) the-empty-stream
      (cons-stream (apply f (map stream-car streams))
                   (apply stream-map
                          (cons f (map stream-cdr streams))))))

;; Exercise 3.51
(define (show x) (display-line x) x)

;; Exercise 3.52 - TODO

(define (integers-from n) (cons-stream n (integers-from (inc n))))
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
        (stream-car stream)
        (sieve (stream-filter
                (lambda (x) (not (divisible? x (stream-car stream))))
                (stream-cdr stream)))))

(define primes (sieve (integers-from 2)))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (pos? n) (> n 0))

(define (neg? n) (< n 0))

;; Exercise 3.53
;; (1 2 4 8 16 32 ..... )
(define (print-stream-until s n)
  (define (iter s n)
    (cond ((= n 1) (display (stream-car s)))
          ((pos? n) (display (stream-car s)) (display " ")
           (iter (stream-cdr s) (dec n)))))
  (display "STREAM: (")
  (iter s n)
  (display ")"))

;; Exercise 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

;; Exercise 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;; Exercise 3.56
(define (merge-stream s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((car-1 (stream-car s1))
               (car-2 (stream-car s2)))
           (cond ((< car-1 car-2)
                  (cons-stream
                   car-1
                   (merge-stream (stream-cdr s1) s2)))
                 ((> car-1 car-2)
                  (cons-stream
                   car-2
                   (merge-stream s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   car-1
                   (merge-stream (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define (partial f . args)
  (lambda x (apply f (append args x))))

(define (scale-stream s factor)
  (stream-map (partial * factor) s))

(define s (cons-stream
           1
           (merge-stream
            (scale-stream s 2)
            (merge-stream
             (scale-stream s 3)
             (scale-stream s 5)))))

;; Exercise 3.57
;; fibs is linear with respect to n
;; we perform n - 2 calculations
;; Had we not memoized the results
;; we would perform n*n calculations

;; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
;; Expand computes division of num by den given a base radix
;; number system.

;; Exercise 3.59
;; a.
(define (divide-streams s1 s2)
  (stream-map / s1 s2))
(define (integrate-series s)
  (divide-streams s integers))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
;; b
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (scale-stream
                                      (integrate-series sine-series)
                                      -1)))

;; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (add-streams (scale-stream (stream-cdr s1)
                                           (stream-car s2))
                             (scale-stream (stream-cdr s2)
                                           (stream-car s1)))
                (cons-stream 0 (mul-series (stream-cdr s1)
                                           (stream-cdr s2))))))

;; tests
(define (test3-60)
  (define (stream-every-till-n n p? s)
    (cond ((= n 0) #t)
          ((stream-null? s) #t)
          ((p? (stream-car s))
           (stream-every-till-n (dec n) p? (stream-cdr s)))
          (else #f)))
  (define one (add-streams (mul-series sine-series sine-series)
                          (mul-series cosine-series cosine-series)))
  (and (= 1 (stream-car one))
       (stream-every-till-n 100 (partial = 0) (stream-cdr one))))

;; Exercise 3.61
(define (invert-unit-series s)
  (define inverted
    (cons-stream
     1 (scale-stream (mul-series inverted (stream-cdr s)) -1)))
  inverted)

;; Exercise 3.62
(define (div-series num den)
  (let ((c (stream-car den)))
    (if (= c 0) (error "Invalid constant term DIV-SERIES" c)
        (scale-stream
         (mul-series num
                     (invert-unit-series
                      (scale-stream den (/ 1 c))))
         (/ 1 c)))))

(define tangent-series (div-series sine-series cosine-series))

;; tests
(define (test3-62)
  (define (stream-every-till-n n p? s)
    (cond ((= n 0) #t)
          ((stream-null? s) #t)
          ((p? (stream-car s))
           (stream-every-till-n (dec n) p? (stream-cdr s)))
          (else #f)))
  (define a (div-series (scale-stream sine-series 10)
                        (scale-stream cosine-series 10)))
  (define b (div-series sine-series cosine-series))
  (define c (stream-map list a b))
  (stream-every-till-n 100 (lambda (x) (= (car x) (cadr x))) c))

;; Exercise 3.63
;; Louis's proposed changes would result in each call generating a new
;; stream object - denying us the benefit of our memoization
;; Excluding that fact, the only loss in performance would be in the
;; creation of new procedure frames and their persistance in memory

;; Exercise 3.64
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map (lambda (g) (sqrt-improve g x)) guesses)))
  guesses)

(define (stream-limit s d)
  (cond ((stream-null? s) #f)
        ((> d (abs (- (stream-car s)
                      (stream-car (stream-cdr s)))))
         (stream-car (stream-cdr s)))
        (else (stream-limit (stream-cdr s) d))))

;; Exercise 3.65
(define neg-pos
  (cons-stream -1 (scale-stream neg-pos -1)))

(define log2
  (cons-stream
   1 (stream-map * neg-pos
                 (stream-map / ones (stream-cdr integers)))))

;; Exercise 3.66
;; The general pattern for make pairs is that a given steam of pairs ((x,y) (x,y +1) (x, y + 2))...
;; occurs approximately half as frequently is the preceeding sequence of ((x-1, y) (x-1, y + 1))...
;; The first occurace of 99 as the first member of a pair, then, will be preceeded by
;; approximately (2 ^ 99) pairs. The second occurance of 99, which corresponds to (99, 100)
;; appears after approximately half again as many pairs, or (2 ^ 98)
;; The first occurance of 100 as the first member of a pair is also the pair of (100, 100)
;; and occurs after approximately (2 ^ 100) other pairs

;; The frequency of pairs where there first member is some number, n, across the infinite
;; sequence of pairs is approximated by 2 ^ -n. From this we can expect that for a sequence
;; of x length, pairs of (1, x) will make up 1/2x. So we would expect to see the 100th 1
;; (1, 100) somewhere around the 200th element
(define (make-ratio p? s n)
  (define (iter count acc s)
    (cond ((stream-null? s) (/ acc (- n count)))
          ((= count 0) (/ acc n))
          ((p? (stream-car s)) (iter (dec count) (inc acc) (stream-cdr s)))
          (else (iter (dec count) acc (stream-cdr s)))))
  (iter n 0 s))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Exercise 3.67

(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (interleave-3 a b c)
  (cond ((stream-null? a) (interleave b c))
        ((stream-null? b) (interleave a c))
        ((stream-null? c) (interleave a b))
        (else (cons-stream
               (stream-car a)
               (cons-stream
                (stream-car b)
                (cons-stream
                 (stream-car c)
                 (interleave-3 (stream-cdr a)
                               (stream-cdr b)
                               (stream-cdr c))))))))

;; Alternative generic interleave
(define (interleave . x)
  (define (interleave-cycle args cycle)
    (cond ((and (null? args) (null? cycle)) the-empty-stream)
          ((null? args) (interleave-cycle (reverse cycle) args))
          ((stream-null? (car args)) (interleave-cycle (cdr args) cycle))
          (else
           (cons-stream
            (stream-car (car args))
            (interleave-cycle (cdr args) (cons (stream-cdr (car args)) cycle))))))
  (interleave-cycle x nil))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave-3
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Exercise 3.68
(define (borken s t)
  (interleave (stream-map (lambda (x) (list (stream-car s) x)) t)
              (borken (stream-cdr s) (stream-cdr t))))

;; Without the call to cons-stream to delay evaluation, borken continues
;; to recur without stop.

;; Exercise 3.69

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u)) ; 1 1 1
   (interleave-3
    (stream-map (lambda (x) (list (stream-car s) (stream-car t) x)) ; 1 1 2
                (stream-cdr u))
    (stream-map (lambda (x) (list (stream-car s) (stream-car (stream-cdr t)) x)) ; 1 2 2
                (stream-cdr u))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))) ; 2 2 2

(define py-triples
  (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x)))
                                (square (caddr x))))
                 (triples integers integers integers)))

;; Exercise 3.70
(define (merge-weighted weight s1 s2)

  (define (ordered-stream head s1 s2)
    (cons-stream head (merge-weighted weight s1 s2)))

  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((a (stream-car s1))
               (b (stream-car s2)))
           (if (<= (weight a) (weight b))
               (ordered-stream a (stream-cdr s1) s2)
               (ordered-stream b s1 (stream-cdr s2)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
    (stream-map (lambda (x) (list (car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))

;; a
(define (weight-a pair)
  (+ (car pair) (cadr pair)))

(define solution-a (weighted-pairs integers integers weight-a))

;; b
(define (weight-b pair)
  (+ (* 2 (car pair))
     (* 3 (cadr pair))
     (* 5 (car pair) (cadr pair))))

(define (divisible? n m) (= (remainder n m) 0))

(define input-b
  (stream-filter
   (lambda (x) (every (lambda (n) (not (divisible? x n))) '(2 3 5)))
   integers))

(define solution-b (weighted-pairs input-b input-b weight-b))

;; Exercise 3.71
(define (weight-c pair)
  (+ (expt (car pair) 3) (expt (cadr pair) 3)))

(define ramanujan-numbers
  (let* ((x (weighted-pairs integers integers weight-c)))
    (stream-filter
     (lambda (x) (= (weight-c (car x)) (weight-c (cadr x))))
     (stream-map list x (stream-cdr x)))))

(define (partial-stream->list n s)
  (if (or (= n 0) (stream-null? s))
      nil
      (cons (stream-car s)
            (partial-stream->list (dec n) (stream-cdr s)))))

(define (first-n-ram n)
  (map (lambda (x)
         (+ (expt (car (car x)) 3) (expt (cadr (car x)) 3)))
       (partial-stream->list n ramanujan-numbers)))

;; Exercise 3.72
;; Made a mistake - Thought the question asked for a stream of all numbers
;; that can be written as the sum of three squares in three difference ways
;; Two would have been much easier.
(define (weight-d triplet)
  (+ (expt (car triplet) 2) (expt (cadr triplet) 2) (expt (caddr triplet) 2)))

(define (weighted-triples weight s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (merge-weighted
    weight
    (stream-map
     (lambda (x) (list (stream-car s) (stream-car t) x)) (stream-cdr u))
    (merge-weighted
     weight
     (stream-map
      (lambda (x) (list (stream-car s) (stream-car
                                        (stream-cdr t)) x)) (stream-cdr u))
     (weighted-triples weight (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define solution-stream
  (let ((x (weighted-triples weight-d integers integers integers)))
    (stream-filter
     (lambda (x) (= (weight-d (car x)) (weight-d (cadr x)) (weight-d (caddr x))))
     (stream-map list x (stream-cdr x) (stream-cdr (stream-cdr x))))))

(define (first-n-solution n)
  (map (lambda (x)
         (let ((y (car x)))
           (let ((z (+ (square (car y)) (square (cadr y)) (square (caddr y)))))
             (cons z x))))
       (partial-stream->list n solution-stream)))

;; correct solution
(define (weight-e pair)
  (+ (square (car pair)) (square (cadr pair))))

(define solution-stream
  (let ((x (weighted-pairs integers integers weight-e)))
    (stream-filter
     (lambda (three-pairs)
       (every (parital = (weight-e (car thee-pairs))) three-pairs))
     (stream-map list x (stream-cdr x) (stream-cdr (stream-cdr x))))))

(define (print-n-solutions n)
  (for-each (lambda (pairs)
              (newline)
              (display "Square 1: ") (display (car pairs)) (newline)
              (display "Square 2: ") (display (cadr pairs)) (newline)
              (display "Square 3: ") (display (caddr pairs)) (newline)
              (display "Value: ") (display (weight-e (car pairs))) (newline))
            (partial-stream->list n solution-stream)))

;; Streams as signals

;; Exercise 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-vaue
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (integral (scale-stream i (/ 1.0 C)) v0 dt)
                 (scale-stream i R))))

(define RC1 (RC 5 1 0.5))

;; Exercise 3.74
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))


;; Exercise 3.75
(define (pos? x) (> x 0))

(define (neg? x) (< x 0))

(define (zero? x) (= x 0))

(define (sign-change-detector y x)
  (cond ((and (pos? x) (neg? y)) -1)
        ((and (neg? x) (pos? y)) 1)
        (else 0)))

(define (make-zero-crossings input-s last-v last-avg)
  (let ((avg (/ (+ (stream-car input-s)
                   last-v)
                2)))
    (cons-stream
     (sign-change-detector avg last-avg)
     (make-zero-crossings
      (stream-cdr input-s) (stream-car input-s) avg))))

;; Exercise 3.76
(define (smooth s)
  ;; smooth takes as its only argument a stream, and returns a stream
  ;; in which each element is the average of two successive input stream
  ;; elements
   (scale-stream (add-streams s (stream-cdr s)) (/ 1 2)))

(define (make-zero-crossings input-s)
  (stream-map sign-change-detector
              (smooth input-s)
              (smooth (cons-stream 0 input-s))))

;; tests
(define (test3-76)
  (define doubles (cons-stream 1 (stream-map (partial * 2) doubles)))
  (define smoothed (smooth doubles))
  (define target (+ (stream-ref doubles 10)
                    (stream-ref doubles 9)))
  (define test-stream (stream 1 2 1.5 1 .5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
  (define target-crossings (list 0 0 0 0 0 -1 0 0 0 0 1 0))
  (define test-crossings (make-zero-crossings test-stream))
  (and (equal? target-crossings (partial-stream->list 13 test-crossings))
       (= target (stream-ref smoothed 10))))

;; Exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((int (force delayed-integrand)))
     (if (stream-null? int)
         the-empty-stream
         (integral (delay (stream-cdr int))
                   (+ (* dt (stream-car int))
                      initial-value)
                   dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; tests
(define (test3-77)
  (= (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
     2.716923932235896))

;; Exercise 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; Exercise 3.79
(define (generalized-solver f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; Exercise 3.80
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define il (integral (delay di) il0 dt))
    (define vc (integral (delay dvc) vc0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define di (add-streams
                (scale-stream vc (/ 1 L))
                (scale-stream il (- (/ R L)))))
    (stream-map cons vc il)))

;; Exercise 3.81
(define (new-rand n)
  ;; uses the linear congruential generator
  (let ((a (expt 2 31))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a n) c) m)))

(define (rand-stream req-stream)
  ;; req stream must be a stream of either nil
  ;; or a new number with which to reset the stream of random-numbers
  (define (rand-dispatch req last-rand)
    (if (null? req) (new-rand last-rand) req))
  (define randoms
    (cons-stream
     (new-rand 123)
     (stream-map rand-dispatch req-stream randoms)))
  randoms)

(define (test3-82)
  (define my-reqs (stream nil nil nil nil nil
                          nil nil nil nil nil 579
                          nil nil nil nil nil
                          nil nil nil nil nil))
  (define my-randoms (rand-stream my-reqs))
  (equal? (stream-head my-randoms 11)
          (stream-head (stream-tail my-randoms 11) 11)))

;; Exercise 3.82 - TODO
