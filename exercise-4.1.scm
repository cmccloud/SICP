;; Exercise 4.1

;; using let special form
(define (list-of-values-left exp env)
  (if (no-operands? exp) '()
      (let ((value-1 (eval (first-operand exps) env)))
        (cons value-1 (list-of-values-left (rest-operands-exps) env)))))

(define (list-of-values-right exp env)
  (if (no-operands? exp) '()
      (let ((rest-values (list-of-values-right (rest-operands-exps) env)))
        (cons (eval (first-operand exps) env) rest-values))))

;; using lambdas
(define (list-of-values-left exp env)
  (if (no-operands? exp) '()
      ((lambda (first-value)
         (cons first-value (list-of-values-left (rest-operands-exps) env)))
       (eval (first-operand exps) env))))

(define (list-of-values-right exp env)
  (if (no-operands? exp) '()
      ((lambda (rest-values)
         (cons (eval (first-operand exp) env)
               rest-values))
       (list-of-values-right (rest-operands-exp) env))))
