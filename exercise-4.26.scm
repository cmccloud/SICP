;; Exercise 4.26
;; Ben is certainly correct that introducing unless as a special form
;; is quite simple. We could imagine a first class unless being useful
;; for things like error handling and debugging, in the same way that
;; a first class if would be useful, e.g:
;; (some-fn (unless (invalid? data) (process data) (error "Invalid Data!")))
;; (define error-handler (partial-last unless (error "Invalid Data!)))

;; One could introduce a unless as a special form as follows
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-exception exp) (cadddr exp))
;; as a new special form
(define (analyze-unless exp)
  (let ((pproc (analyze (unless-condition exp)))
        (cproc (analyze (unless-consequent exp)))
        (eproc (analyze (unless-exception exp))))
    (lambda (env)
      (if (pproc env)
          (eproc env)
          (cproc env)))))
;; As a derived expression
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exception exp)
           (unless-conseqeunt exp)))
;; Within analyze
((unless? exp) (analyze (unless->if exp)))
;; or
((unless? exp) (analyze-unless exp))
