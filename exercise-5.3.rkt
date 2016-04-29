;; Exercise 5.3
(controller
 (assign x (op read))
 (assign guess (const 1.0))
 test-b
 (test (op good-enough?)
       (reg guess))
 (branch (label sqrt-done))
 (assign guess
         (op improve)
         (reg guess))
 (goto (label test-b))
 sqrt-done)

;; Expanded form
;; (sqrt x)
;; Registers (x guess t)
;; Primitive Operations (read square - abs < / average)
(controller
 (assign x (op read))
 (assign guess (const 1.0))
 sqrt-loop
 (assign t
         (op square)
         (reg guess))
 (assign t
         (op -)
         (reg t)
         (reg x))
 (assign t
         (op abs)
         (reg t))
 (test (op <)
       (reg t)
       (const 0.001))
 (branch (label sqrt-done))
 (assign t
         (op /)
         (reg x)
         (reg guess))
 (assign guess
         (op average)
         (reg guess)
         (reg t))
 (goto (label sqrt-loop))
 sqrt-done)
