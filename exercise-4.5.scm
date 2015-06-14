;; Exercise 4.5
;; Installed as a package
(define (extended-cond-package install-proc eval)
  ;; Internal definitions
  (define (cond-actions clause) (cdr clause))

  (define (cond-predicate clause) (car clause))

  (define (cond-clauses exp) (cdr exp))

  (define (arrow-clause? exp) (eq? (car (cond-actions exp)) '=>))

  (define (cond-else-clause? clause) (eq? (cond-predicate clauses) 'else))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

  (define (make-begin seq) (cons 'begin seq))

  ;; Not sure exactly how to implement this part yet
  ;; What we really want to check is whether the recipient
  ;; is a single arity procedure
  (define (valid-recipient? exp env)
    (let ((recipient (eval exp env)))
      (and (lambda? recipient) ;; must be a function
           (pair? (cadr recipient)) ;; check for variadic lambda
           (= (length (cadr recipient)) 1))) ;; check for arity

  (define (else->exp first rest clauses)
    (if (null? rest)
        (sequence->exp (cond-actions first))
        (error "Else clause isn't last: COND->IF" clauses)))

  (define (arrow->if first rest env)
    (if (valid-recipient? (caddr first) env)
        (make-if (cond-predicate first)
                 (list (cadr (cond-actions first))
                       (cond-predicate first))
                 (expand-clauses rest))
        (error "Recipient must be single arity procedure: COND-IF"
               (caddr first))))

  (define (expand-clauses clauses env)
    (if (null? clauses) 'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((cond-else-clause? first) (else->exp first rest clauses))
                ((arrow-clause? first) (arrow->if first rest env))
                (else (make-if (cond-predicate first)
                               (sequence->exp (cond-actions first))
                               (expand-clauses rest)))))))

  (define (cond->if exp env)
    (expand-clauses (cond-clauses exp env)))
  ;; Interface
  (install-proc
   (list
    (list 'eval 'cond (lambda (exp env) (eval (cond->if exp env) env)))))
  'extended-cond-package-installed!)
