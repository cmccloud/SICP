;; Exercise 5.2
(controller
 test-b
 (test (op >) (reg p) (const 1))
 (brnach (label fact-done))
 (assign p (op *) (reg p) (reg c))
 (assign c (op +) (reg c) (const 1))
 (goto (label test-b))
 fact-done)
