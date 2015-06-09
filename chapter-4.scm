;; SICP - Chapter 4

;; 4.1.1 - The Core of the Evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; Procedure arguments
(define (list-of-values exp env)
  (if (no-operands? exps) '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; Conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; Exercise 4.1 - Solution assumes only that the underlying lisp uses
;; applicative order evaluation

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
