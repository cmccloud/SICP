;; Exercise 4.27
;; (define count 0)
;; (define (id x) (set! count (+ count 1)) x)
;; (define w (id (id 10)))
;; The rules for procedure application in our lazy evaluator are
;; that we force the actual value of the operator which will
;; force a call to eval sequence. The first expression in
;; id is a call to a primitive procedure which has the effect
;; of incrementing count. The second expression is a x,
;; which at this point is simply a promise on the inner id call
;; count -> 1
;; w -> 10
;; In order to print w the full expression must be analysized
;; doing so calls the inner id, which increments count again.
;; count -> 2
