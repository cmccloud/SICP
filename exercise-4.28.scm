;; Exercise 4.28
;; Other people have commented that the reason that our evaluator must
;; force the evaluation of operator during procedure application is
;; that not doing so would cause our evaluator to attempt to call
;; 'thunks' on operands, would will obviously throw an exception.
;; This is true, but I don't see why we couldn't modify our evaluation
;; procedure to account for that. My own suspicion is that delayed
;; evaluation of procedure applications, combined with mutation, would
;; could lead to undesireable side effects.
(define (f x) (+ x 1))
(define (g f x) (f x))
(define a (g f 1))
(define (f x) (- x 1))
(really-evaluate a)
;; The fourth define call would have the effect of overwriting the procedure
;; associated with the symbol f - a purely lazy evaluator would only lookup
;; the variables within g at evaluation time - which would mean that a
;; would be 0, rather than 2
