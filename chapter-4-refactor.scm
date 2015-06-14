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
        ((and? exp) (eval (and->if (and-predicates exp)) env))
        ((or? exp) (eval (or->if (or-predicates exp) env)))
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
        (else (error "Unknown procedre type: APPLY" procedure))))

(define (list-of-values exps env)
  ;; Takes in a list of operands and procedures a list of
  ;; their corresponding values relative to a given environment.
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps env))
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  ;; Evaluates the predicate part of an if-expression in a given
  ;; environment. That value is then passed to true? which will
  ;; return either true or false depending on our notion of truthfulness
  ;; within our interpreted language.
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  ;; uses by apply to evaluate the sequence of expression in a procedure
  ;; body and by eval to evaluate the sequence of expressions in a begin
  ;; expression. The value returned is the value of the final expression.
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;; formal parameters
                   (cddr exp)))) ;; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (arrow-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (make-recipient clause)
  ;; This implementation of make-recipient doesn't test for the validity
  ;; of the recipient procedure
  (list (cdr (cond-actions clause))
        (cond-predicate clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last: COND->IF"
                          clauses)))
              ((arrow-clause? first)
               (make-if (cond-predicate first)
                        (make-recipient first)
                        (expand-clauses rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))
(define (first-pred preds) (car preds))
(define (rest-preds preds) (cdr preds))
(define (last-pred? preds) (null? (cadr preds)))
(define (and->if preds)
  (cond ((null? preds) 'true)
        ((last-pred? preds) (first-pred preds))
        (else (make-if (first-pred preds)
                       (and->if (rest-preds preds))
                       'false))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))
(define (or->if preds)
  (cond ((null? preds) 'false)
        (else (make-if (first-pred preds)
                       (first-pred preds)
                       (or->if (rest-preds preds))))))

