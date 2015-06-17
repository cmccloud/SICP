;; Exercise 4.21
;; a - fib
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (self n)
      (if (<= n 1) n
          (+ (self self (- n 1))
             (self self (- n 2)))))))
 10)
;; b - even and odd corecursion
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev od n)
     (if (= n 0) true (od od ev (- n 1))))
   (lambda (od ev n)
     (if (= n 0) false (ev ev od (- n 1))))))
