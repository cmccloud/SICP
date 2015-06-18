;; Exercise 4.29
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
;; With Memoization
(define (square x) (* x x))
(square (id 10)) ;; -> 100
count ;; -> 1
;; Without Memoization
(define (square x) (* x x))
(square (id 10)) ;; -> 100
count ;; -> 2
