#lang racket
(require racket/trace)

(define (tail lst [newlst '()])
  (if
    (empty? lst) newlst
    (tail (cdr lst) (cons (car lst) newlst))))

(trace tail)


(define (not-tail lst)
  (if
    (empty? lst) '()
    (append (not-tail (cdr lst)) (list (car lst)))))

(trace not-tail)


(define (make-set name)
  (let ([lst '()])
    (lambda (cmd . args)
      (case cmd
        [(name) name]
        [(add) (begin (set! lst (cons (car args) lst)) (list (car args) 'added 'to 'list))]
        [(pop) (if (empty? lst) #f (let ([var (last lst)]) (begin (set! lst (reverse (cdr (reverse lst)))) var)))]))))

(define (insert x lst)
  (cond
    [(empty? lst) (list x)]
    [(> (car lst) x) (cons x lst)]
    [else (cons (car lst) (insert x (cdr lst)))]))

(define (isort lst)
  (foldl (lambda (x y) (insert x y)) '() lst))


(define (reformat exp)
  (cond
    [(not (list? exp)) exp]
    [else (list (second exp) (reformat (first exp)) (reformat (third exp)))]))
