#lang racket

(provide hours
	 kg2lb
	 nominee)

; Please do not modify the lines above this one.

; ********************************************************
; CS 201 HW #0  DUE Wednesday 9/11/19, 11:59 pm
; ** using the Zoo submit system **

; ********************************************************
; Name: Jalil Morris
; Email address: jalil.morris@yale.edu
; ********************************************************

; This file may be loaded into Racket.  Lines beginning with
; semicolons are comments.

; Homework #0 will be worth 20 points -- other homeworks will be worth
; 100 points (or so).  One purpose of homework #0 is to make sure you
; can use the submit system on the Zoo.  You will receive no credit
; for this assignment until you successfully use the submit system to
; submit it.

; You will be submitting *two* files for homework #0.  Please name
; them: hw0.rkt (for the Racket definitions and procedures)
; response.pdf (for the reading response)

; ********************************************************
; ** problem 0 ** (1 easy point) 

; Replace the number 0 in the definition below to indicate how many
; hours you spent doing this assignment.  Fractions are fine, eg,
; 3.14159.  You will receive no credit for this problem if you leave
; the number as 0.

(define hours 3)

; ********************************************************
; ** problem 1 ** (5 points)

; Write a procedure (kg2lb mass)

; which converts a mass in kilograms to the equivalent mass in pounds
; and ounces.  There are 2.2 pounds per kilogram.  There are 16 ounces
; in a pound.  All values are rounded down to the nearest integer.

; Examples

; (kg2lb .5) => '(1 pound 1 ounce))
; (kg2lb 1) => '(2 pounds 3 ounces))
; (kg2lb 2) => '(4 pounds 6 ounces))
; (kg2lb 3) => '(6 pounds 9 ounces))
; (kg2lb 4) => '(8 pounds 12 ounces))
; (kg2lb 9) => '(19 pounds 12 ounces))
; (kg2lb 10) => '(22 pounds 0 ounces))
; (kg2lb 100) => '(220 pounds 0 ounces))

; ********************************************************

; Replace "empty" in the code below with your procedure.  Make sure it
; is named kg2lb and has one argument.  The name of the argument is
; not important.

; Your procedure will be tested automatically with positive arguments.

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
  (list lbs a ounces b))


; ********************************************************
; ** problem 2 ** (4 points)

; Write a procedure (nominee) that takes no arguments and returns a
; *string* indicating your predicted Democratic presidential nominee.

; Please remember the difference between a procedure call and the
; evaluation of a variable!

; Example (yours will likely be different)

; (nominee) => "Biden"

; ********************************************************

; Replace "empty" in the code below with your procedure.  Make sure it
; is named nominee and takes no arguments.  Your procedure will be
; tested automatically, and will be called only with no arguments.

(define (nominee)
  "Kanye West")

; ********************************************************
; ** problem 3 ** (10 points)

; For this problem, you are asked to find one article (of 2 pages or
; more) in the magazine "Communications of the ACM", in one of the
; issues: June, July, August 2019 See http://cacm.acm.org/

; read the article and answer the following three questions:

;   a. What did you know about the topic
;      prior to reading the article?
;   b. What did you learn from reading the
;      article?
;   c. What more would you like to know
;      about the topic?

; Your answer should be AT MOST 2 pages saved in pdf format, and
; submitted as the file response.pdf for assignment 0.  Please include
; in your file your name and email address and the title and author(s)
; of the article you are responding to.

; Your grade for this problem will be 10 if we can open, print and read
; your submitted pdf file.  It is to help us get acquainted with you
; and your interests -- you won't receive feedback on your answers.

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (if (expected got) 'OK-TEST
				  'FAILED-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))

(test 'kg2lb (kg2lb .5) '(1 pound 1 ounce))
(test 'kg2lb (kg2lb 1) '(2 pounds 3 ounces))
(test 'kg2lb (kg2lb 2) '(4 pounds 6 ounces))
(test 'kg2lb (kg2lb 3) '(6 pounds 9 ounces))
(test 'kg2lb (kg2lb 4) '(8 pounds 12 ounces))
(test 'kg2lb (kg2lb 9) '(19 pounds 12 ounces))
(test 'kg2lb (kg2lb 10) '(22 pounds 0 ounces))
(test 'kg2lb (kg2lb 100) '(220 pounds 0 ounces))

(test 'nominee (nominee) (lambda (x) (string? x)))

; ********************************************************
; ********  end of homework #0
; ********************************************************

