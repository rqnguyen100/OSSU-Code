;; HtDD Design Quiz

;; Rubric:
;; Safe(U/A):
;;  - Acceptable
;; Commit Ready(P/F/G):
;;  - Good
;; Design Completeness(P/F/G):
;;  - Good
;; Internal Quality(P/F/G/E):
;;  - Excellent
;; Problem Satisfied(P/F/G):
;;  - Good

;; ---------------
;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural

; Problem 1:
; 
; Consider the above data definition for the age of a person.
; 
; Design a function called teenager? that determines whether a person
; of a particular age is a teenager (i.e., between the ages of 13 and 19).

;; Age -> Boolean
;; determines if a person is a teenager and outputs true if they are
(check-expect (teenager? 10) false)
(check-expect (teenager? 13)  true)
(check-expect (teenager? 15)  true)
(check-expect (teenager? 19)  true)
(check-expect (teenager? 22) false)

;;(define (teenager? age) false) ; stub

;; <Template from Age>

(define (teenager? age)
  (<= 13 age 19))

; Problem 2:
; 
; Design a data definition called MonthAge to represent a person's age
; in months.

;; MonthAge is Natural
;; interp. person's age in months
(define MA0 216) ;18 year old is 216 months old
(define MA1 300) ;25 year old is 300 months old

#; 
(define (fn-for-month-age a)
  (... a))

;; Template rules used:
;;  - atomic non-distinct: Natural

; Problem 3:
; 
; Design a function called months-old that takes a person's age in years 
; and yields that person's age in months.
; 

;; Age -> MonthAge
;; converts person's age in years to age in months
(check-expect (months-old 18) 216)
(check-expect (months-old 25) 300)

;;(define (months-old age) 12) ; stub

;; <Template from MonthAge>

(define (months-old age)
  (* 12 age))

; Problem 4:
; 
; Consider a video game where you need to represent the health of your
; character. The only thing that matters about their health is:
; 
;   - if they are dead (which is shockingly poor health)
;   - if they are alive then they can have 0 or more extra lives
; 
; Design a data definition called Health to represent the health of your
; character.
; 
; Design a function called increase-health that allows you to increase the
; lives of a character.  The function should only increase the lives
; of the character if the character is not dead, otherwise the character
; remains dead.

;; Data Definition

;; Health is one of:
;;  - false
;;  - Natural
;; interp. false means character is dead, natural is number of extra lives left
(define H1 false) ;character is dead
(define H2     0) ;character has 0 extra lives left
(define H3     3) ;character has 3 extra lives left

#;
(define (fn-for-health health)
  (cond [(false? health)  (...)]
        [(number? health) (... health)]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: false
;;  - atomic non-distinct: Natural

;; ---------------
;; Function

;; Health Natural -> String
;; increases lives of character unless they are dead
(check-expect (increase-health false 2) "Character is dead; cannot increase lives")
(check-expect (increase-health 0 2)     "Character has 2 extra lives")
(check-expect (increase-health 1 2)     "Character has 3 extra lives")

;;(define (increase-health health lives) "") ; stub

;; <Template from Health>

(define (increase-health health lives)
  (cond [(false? health) "Character is dead; cannot increase lives"]
        [(number? health) (string-append "Character has "
                                         (number->string (+ health lives))
                                         " extra lives")]))
