;; Exercise 4.24
;; Test-One Results:
;; Simple-Eval: Runtime: 17.35, Realtime: 17.51
;; With-Analysis: Runtime: 10.59, Realtime: 10.588
;; Approximately 35-40% of the time was spent in analysis
;; Note that this particular test should represent close to the
;; maximum performance benefit - one definition and a tremendous number
;; of calls.
(define (test4-24)
  ;; Test must be run in environment in which evaluator exists
  (define global-env (setup-environment))
  (define test-one
    '((define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
      (fib 23)))
  (with-timings
   (lambda ()
     (for-each (lambda (exp) (eval exp global-env)) test-one))
   (lambda (run-time gc-time real-time)
     (display "Run time:") (newline)
     (write (internal-time/ticks->seconds run-time))
     (display "GC time:") (newline)
     (write (internal-time/ticks->seconds gc-time))
     (display "Real time:") (newline)
     (write (internal-time/ticks->seconds real-time)))))
