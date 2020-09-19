#lang racket

(define (kg2lbs mass)
  (define pounds (* mass 2.2))
  (define lbs (exact-floor pounds))
  (define ounces (exact-floor (* 16 (- pounds lbs))))
  (define a "pounds")
  (define b "ounces")
  (if (equal? lbs 1) (set! a (string-trim a "s")) 'done)
  (if (equal? ounces 1) (set! b (string-trim b "s")) 'done)
  (define str (list (number->string lbs) a "and" (number->string ounces) b))
  (print (string-join str))
)


(define (kg2lb mass)
  (define pounds (* mass 2.2))
  (define lbs (exact-floor pounds))
  (define ounces (exact-floor (* 16 (- pounds lbs))))
  (define a 'pounds)
  (define b 'ounces)
  (cond
    [(and (equal? lbs 1) (equal? ounces 1)) (set! a 'pound) (set! b 'ounce)]
    [(equal? lbs 1) (set! a 'pound)]
    [(equal? ounces 1) (set! b 'ounce)]
    )
  (list lbs a ounces b)
)