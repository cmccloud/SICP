;; Exercise 4.30
;; a - Ben is correct because the application calls within for-each
;; are to primitive procedures.
;; b - (p1 1) -> (1 2)
;; (p2 1) -> 1
;; c - Because all we're doing is forcing evaluation of expressions
;; d - We might come up with a new strategy where, during the analysis
;; of a procedure definition, we flag the procedure if it makes use
;; of mutators. During a sequence, any calls to mutators would have to be
;; forced. We would also have to do analysis on procedure calls, to see
;; if an otherwise pure procedure is accepting a mutator as an argument.
;; E.g. (define (f g x) (g x)) -> pure
;; (define (mutates! x) (set! x (cons x '(2)))) -> mutates
;; (f mutates! 1) -> mutates
;; Actually, what we would really need to do in this case would be to force
;; the evaluation of any procedure calls that are either impure, or involve
;; mutators...
;; E.g.
;; (define (f x) 1)
;; (define (g x) (f x))
;; (define (f x) (set! x (cons x '(2))))
;; (g 1)
