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
