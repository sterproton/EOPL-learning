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


(define (number-elements-from list n)
  (cond ((null? list) '())
    (else (cons (cons n (car list)) (number-elements-from (cdr list) (+ n 1))))))

(define (number-elements list)
  (number-elements-from list 0))

(define (partial-vector-sum v n)
  (cond ((zero? n) (vector-ref v 0))
    (else (+ (vector-ref v n)
              (partial-vector-sum v (- n 1))))))

(define (vector-sum v)
  (let ((n (vector-length v)))
    (cond ((zero? n) 0)
      (else (partial-vector-sum v (- n 1))))))

(define (duple x y)
  (define (recu cnt list)
    (cond ((zero? cnt) list)
      (else (recu (- cnt 1) (cons y list)))))
  (recu x '()))

(define (invert list)
  (define (reverse-pair pair)
    (cons (cadr pair) (car pair)))
  (cond ((null? list) '())
    (else (cons (reverse-pair (car list)) (invert (cdr list))))))

(define (down list)
  (map (lambda (x) (cons x '())) list))

(define (swapper s1 s2 slist)
  (cond ((null? slist) '())
    ((symbol? (car slist))
      (cond ((eqv? (car slist) s1)(cons s2 (swapper s1 s2 (cdr slist))))
        ((eqv? (car slist) s2) (cons s1 (swapper s1 s2 (cdr slist))))
        (else (cons (car slist) (swapper s1 s2 (cdr slist))))))
    (else (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))))


(define (list-set list n x)
  (define (recu cnt list)
    (cond ((null? list) '())
      ((eqv? n cnt) (cons x (cdr list)))
      (else (cons (car list) (recu (+ cnt 1) (cdr list))))))
  (recu 0 list))

(define (count-occurrences s slist)
  (cond ((null? slist) 0)
    ((symbol? (car slist))
        (cond ((eqv? (car slist) s ) (+ 1 (count-occurrences s (cdr slist))))
          (else (count-occurrences s (cdr slist)))))
    (else (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist))))))

(define (product sos1 sos2)

  (define (productSymbol symbol lst nextlist)
    (cond ((null? lst) nextlist)
      (else (cons (list symbol (car lst))
              (productSymbol symbol (cdr lst) nextlist)))))
  (define (recu lst contain)
    (cond ((null? lst) contain)
      (else (recu (cdr lst) (productSymbol (car lst) sos2 contain)))))
  (recu (reverse sos1) '()))

(define (filter-in predicate lst)
  (cond ((null? lst) '())
    ((predicate (car lst)) (cons (car lst) (filter-in predicate (cdr lst))))
    (else (filter-in predicate (cdr lst)))))

(define (list-index pred lst)
  (define (recu lst cnt)
    (cond ((null? lst) #f)
      ((pred (car lst)) cnt)
      (else (recu (cdr lst) (+ cnt 1)))))
  (recu lst 0))

(define (every? pred lst)
  (cond ((null? lst) #t)
    ((pred (car lst)) (every? pred (cdr lst)))
    (else #f)))


(define (exists? pred lst)
  (cond ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (exists? pred (cdr lst)))))

(define (up lst)
  (cond ((null? lst) '())
    ((symbol? (car lst)) (cons (car lst) (up (cdr lst))))
    (else (append (car lst) (up (cdr lst))))))

(define (flatten slist)
  (define (notAtom? x)
    (define (atom? x)
      (or (symbol? x) (number? x)))
    (not (atom? x)))
  (define (recu lst)
    (cond ((exists? notAtom? lst) (recu (up lst)))
      (else lst)))
  (cond ((null? slist) '())
    (else (recu slist))))


