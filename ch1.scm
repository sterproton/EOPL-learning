(define (nth-ele list n)
  (define param-error
    (lambda (_) (eopl:error 'nth-ele'
      "~s does not have ~s elements" list n)))
  (define (recu list n)
    (cond ((null?　list)
      (param-error #f))
      ((= 0 n) (car list))
      (else (recu (cdr list) (- n 1)))))
  (recu list n))


(define (remove ele list)
  (cond ((null? list) '())
    ((eq? ele (car list)) (remove ele (cdr list)))
    (else (cons (car list) (remove ele (cdr list))))))


(define (occurs-free? var exp)
  (cond ((symbol? exp) (eqv? var exp))
    ((eqv? (car exp) 'lambda)
      (and (not (eqv? var (car (cadr exp))))
            (occus-free? var (caddr exp))))
    (else (or
      (occus-free? var (car exp))
      (occus-free? car (cadr exp))))))

(define (sub-set new old list)
  (define (subset-by-exp new old exp)
    (cond ((symbol? exp)
      (cond ((eqv? exp old) new)
        (else exp)))
    (else (sub-set new old exp))))
  (cond ((null? list) '())
    (else (cons (subset-by-exp new old (car list)) (sub-set new old (cdr list))))))

; after inlining
(define (sub-set new old list)
  (cond ((null? list) '())
    ((symbol? (car list))
      (cond ((eqv? (car list) old) (cons new (sub-set new old (cdr list))))
        (else (cons (car list) (sub-set new old (cdr list)))))))

(define (sub-set new old list)
  (define (mapper exp)
    (cond ((symbol? exp)
      (cond ((eqv? exp old) new)
      (else exp)))
        (else (map mapper exp))))
  (map mapper list))
