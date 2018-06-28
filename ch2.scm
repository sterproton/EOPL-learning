(define base 16)
(define zero '())

(define (is-zero? num)
  (null? num))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (compose f1 f2)
  (lambda (x) #f))

(define (repeated fn num)
  (define (recu cnt composed)
    (cond ((eqv? 1 cnt) composed)
      (else (recu (- cnt 1) (compose fn composed)))))
  (recu num fn))

(define (repeated fn num)
  (define (recu cnt acc)
    (cond ((eqv? 1 cnt) acc)
      (else (recu (- cnt 1) (fn acc)))))
  (lambda (x) (recu num (fn x))))

  (define (successor n)
  (define (recu n)
    (cond ((is-zero? n) (cons 1 n))
      ((eqv? (- base 1) (car n)) (cons 0 (recu (cdr n))))
      (else (cons (+ 1 (car n)) (cdr n)))))
  (reverse (recu (reverse n))))

(define (succ n)
  (cond ((is-zero? n) (cons 1 zero))
    ((eqv? (+ (car n) 1) base) (cons 0 (succ (cdr n))))
    (else (cons (+ (car n) 1) (cdr n)))))

(define (predecessor n)
  (define (recu n)
    (cond ((is-zero? n) #f)
      ((eqv? 0 (car n)) (cons (- base 1) (recu (cdr n))))
      (else (cond ((and (eqv? 1 (car n)) (null? (cdr n))) (cdr n))
        (else (cons (- (car n) 1) (cdr n)))))))
  (reverse (recu (reverse n))))

(define (predec n)
  (cond ((is-zero? n) #f)
    ((eqv? 0 (car n)) (cons (- base 1) (predec (cdr n))))
    (else (cond ((and (null? (cdr n)) (eqv? 1 (car n))) '())
      (else (cons (- (car n) 1) (cdr n)))))))

(define (equal? parameters)
  body)


(define zero '(diff (one) (one)))
(define neg-one '(diff (diff (one) (one)) (one)))
(define pos-one (list 'diff zero neg-one))

(define (is-zero? n)
  (equal? n zero))

(define (succ n)
  (list 'diff n neg-one))

(define (predec n)
  (list 'diff n pos-one))



(define (empty-env)
  '())

(define (is-empty-env? env)
  (null? env))

(define (is-env-valid env)
  (cond ((null? env) #t)
    ((and (pair? (car env)) (symbol? (caar env))) #f)
    (else (is-empty-env? (cdr env)))))

(define (has-binding? env var)
  (let ((variable value))
    ()))

(define (extended-env var val env)
  (cons (cons var val) env))

(define (apply-env env search-var)
  (cond ((is-empty-env? env) (report-no-binding-found search-var))
    (else (let ((saved-var (caar env))
       (saved-val (cdar env))
       (saved-env (cdr env)))
      (cond ((eqv? saved-var search-var) saved-val)
        (else (apply-env saved-env search-var)))))))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error ’apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error ’apply-env "Bad environment: ~s" env)))
