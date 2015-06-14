;; Exercise 4.2
;; a - The evaluator will attempt to apply the procedure define to x 3
;; The particulars of what define could evaluate to in the given environment
;; aside, we know by definition that x will always be undefined given that
;; it could never have been associated in any environment.

;; b
(define (_application? exp) (tagged-list? exp 'call))
(define (_operator exp) (cadr exp))
(define (_operands exp) (cddr exp))

