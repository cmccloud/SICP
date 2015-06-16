;; Exercise 4.12
;; Scan might look something like this
(define (scan fn frame)
  (if (null? frame)
      false
      (or (fn (first-binding frame))
          (scan fn (rest-bindings frame)))))

;; Env loop something like
(define (env-loop fn env)
  (if (eq? env the-empty-environment)
      false
      (or (fn (first-frame env))
          (env-loop fn (enclosing-environment env)))))

;; helpers
(define (do-if test action binding)
  (if (test (binding-var binding))
      (begin (action binding) true)
      false))

(define (partial f . x)
  (lambda y (apply f (append x y))))

(define (flip f)
  (lambda (x y) (f y x)))

;; lookup
(define (lookup-variable-value var env)
  (define find-target
    (partial do-if (partial eq? var) binding-value))
  (or (env-loop (partial scan find-target) env)
      (error "Unknown variable" var)))

;; set variable
(define (set-variable-value! var val env)
  (define set-target!
    (partial do-if (partial eq? var)
             (partial (flip set-binding-value!) val)))
  (or (env-loop (partial scan set-target!) env)
      (error "Unknown variable" var)))

;; define variable
(define (define-variable! var val env)
  (define set-target!
    (partial do-if (partial eq? var)
             (partial (flip set-binding-value!) val)))
  (or (scan set-target! (first-frame env))
      (add-binding-to-frame! var val (first-frame env))))
