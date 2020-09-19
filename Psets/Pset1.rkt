#lang racket

(define (bin2dec num)
  (if (empty? num) 0
     (+ (* (car num) (expt 2 (- (length num) 1))) (bin2dec (cdr num)))))

(define (dec2bin n)
  (if (< n 2) (list n)
        (append (dec2bin (exact-floor (/ n 2))) (list (remainder n 2)))))

(define (hex2dec num)
  (if (empty? num) 0
      (if (symbol? (car num)) (let* ([symbols '(a A b B c C d D e E f F)]
                                     [value (+ (exact-floor (/ (index-of symbols (car num)) 2)) 10)])
                                (+ (* value (expt 16 (- (length num) 1))) (hex2dec (cdr num))))
          (+ (* (car num) (expt 16 (- (length num) 1))) (hex2dec (cdr num))))))

(define (dec2hex n)
  (if (< n 16)
      (if (< n 10) (list n)
          (let ([symbols '((10 A) (11 B) (12 C) (13 D) (14 E) (15 F))])
                (list (second (assoc n symbols)))))
      (let ([symbols '((10 A) (11 B) (12 C) (13 D) (14 E) (15 F))])
        (if (< (remainder n 16) 10)
            (append (dec2hex (exact-floor (/ n 16))) (list (remainder n 16)))
            (append (dec2hex (exact-floor (/ n 16))) (list (second (assoc (remainder n 16) symbols))))))))

(define (my-remove value lst [proc equal?])
  (if (empty? lst) lst
      (if (proc value (car lst))
          (if (null? (cdr lst)) '()
              (member (cadr lst) lst))
          (append (list (car lst)) (my-remove value (cdr lst) proc)))))

(define (my-reverse lst)
  (if (empty? lst) lst
      (append (my-reverse (cdr lst)) (list (car lst)))))
  
(define (sorted? lst . compare?)
  (if (or (empty? (cdr lst)) (empty? lst)) #t
      (if (not (empty? compare?))
          (if ((car compare?) (car lst) (cadr lst))
              (sorted? (cdr lst) (car compare?))
              #f)
          (if (<= (car lst) (cadr lst))
              (sorted? (cdr lst))
              #f))))

(define (inflate lst)
  (map (lambda (x) (if (number? x) (+ x 1) x)) lst))

(define (iterate start proc n)
  (if (equal? n 0) '()
      (append (list (proc start)) (iterate (proc start) proc (- n 1)))))

(define (compound start proc test)
  (if (test start) '()
      (append (list (proc start)) (compound (proc start) proc test))))


(define (power-set lst)
  (if (null? lst) (list '())
      (append (power-set (cdr lst)) (map (lambda (x) (cons (car lst) x)) (power-set (cdr lst))))
      ))

(define (primes n)
  (define (sift list p)
    (filter (lambda (n)
              (not (zero? (modulo n p))))
            list))
  (define (iter nums primes)
    (let ((p (car nums)))
      (if (> (* p p) n)
          (append (reverse primes) nums)
          (iter (sift (cdr nums) p) (cons p primes)))))
  (iter (cdr (build-list n add1)) '()))

(define (divides? p q)
  (zero? (modulo q p)))

(define (prime-factors n)
  (let loop ((primes (primes n)))
    (cond ((memq n primes) (list n))
          ((divides? (car primes) n)
           (cons (car primes) (prime-factors (/ n (car primes)))))
          (else (loop (cdr primes))))))

(define (all-factors n)
  (let ([lst (remove-duplicates (power-set (prime-factors n)))])
    (sort (map (lambda (x) (if (null? x) 1 (foldl * 1 x))) lst) <))
  )
  


  
