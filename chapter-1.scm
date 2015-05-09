;; Exercise 1.1

;; Exercise 1.2

;; Exercise 1.3
(define (sum-of-largest-two-squared a b c)
  (if (< a b) (sum-of-largest-two-squared b a c)
      (if (< b c) (sum-of-largest-two-squared a c b)
          (+ (* a a) (* b b)))))

;; Exercise 1.4

;; Exercise 1.5
