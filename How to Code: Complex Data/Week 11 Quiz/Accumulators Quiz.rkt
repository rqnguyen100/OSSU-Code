;; Rubric:
;; -------
;; Commit Ready (G/F/P):
;;  - Good
;; Design Completeness (G/F/P):
;;  - Good
;; Internal Quality (E/G/F/P):
;;  - Excellent
;; Problem Satisfied (G/F/P):
;;  - Good


;  PROBLEM 1:
;  
;  Assuming the use of at least one accumulator, design a function that consumes a list of strings,
;  and produces the length of the longest string in the list. 
;  

;; (listof String) -> Natural
;; produce the length of the longest string from (listof String)
;;  - ASSUME: (listof String) has at least one element
(check-expect (longest-string (list "a")) 1)
(check-expect (longest-string (list "ab" "c")) 2)
(check-expect (longest-string (list "abcdef" "ghi")) 6)
(check-expect (longest-string (list "ab" "cde" "fg")) 3)

;(define (longest-string los) 0) ;stub

;<Template according to (listof String) + accumulator>
; - acc is context-preserving accumulator

(define (longest-string los0)
  ;; acc: Natural; represents the length of the largest string seen so far
  ;; (longest-string (list "ab" "cde" "fg"))
  ;;
  ;; (longest-string (list "ab" "cde" "fg") 0)
  ;; (longest-string (list      "cde" "fg") 2)
  ;; (longest-string (list            "fg") 3)
  ;; (longest-string (list                ) 3)
  (local [(define (longest-string los acc)
            (cond [(empty? los) acc]
                  [else
                   (if (> (string-length (first los)) acc)
                       (longest-string (rest los) (string-length (first los)))
                       (longest-string (rest los) acc))]))]
    (longest-string los0 0)))

;  PROBLEM 2:
;  
;  The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is 
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to 
;  n-2 + n-1. 
;  
;  Design a function that given a list of numbers at least two elements long, 
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every 
;  element in the list. The sequence does not have to start at zero, so for 
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule. 
;  

;; (listof Numbers) -> Boolean
;; produce true if (listof Numbers) obeys the fibonacci rule, n-2 + n-1 = n
;;  - sequence does not have to start at zero
;;  - ASSUME: (listof Numbers) has at leaast two elements
(check-expect (fibo? (list 0 1 1 2 3 5)) true)
(check-expect (fibo? (list 4 5 9 14 23)) true)

(check-expect (fibo? (list 4 5 9 23 14)) false)
(check-expect (fibo? (list 1 1 2 15))    false)

;(define (fibo? lon) false) ;stub

;<Template according to (listof Numbers) + accumulators>
; - acc1 is context-preserving accumulator
; - acc2 is context-preserving accumulator

(define (fibo? lon0)
  ;; acc1: Number; represents the previous number (n-1)
  ;; (fibo? (list 1 1 2 15))
  ;;
  ;; (fibo? (list   1 2 15) 1)
  ;; (fibo? (list     2 15) 1)
  ;; (fibo? (list       15) 2)
  ;; (fibo? (list         ) 15)
  ;; --------------------------------------------------------------------
  ;; acc2: Number; represents the number before the previous number (n-2)
  ;; (fibo? (list 1 1 2 15))
  ;;
  ;; (fibo? (list     2 15) 1)
  ;; (fibo? (list       15) 1)
  ;; (fibo? (list         ) 2)
  (local [(define (fibo? lon acc1 acc2)
            (cond [(empty? lon) true]
                  [else
                   (if (not (= (+ acc1 acc2) (first lon)))
                       false
                       (fibo? (rest lon) (first lon) acc1))]))]
    (fibo? (rest (rest lon0)) (second lon0) (first lon0))))

;  PROBLEM 3:
;  
;  Refactor the function below to make it tail recursive.  
;  


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)

#;
(define (fact n)
  (cond [(zero? n) 1]
        [else 
         (* n (fact (sub1 n)))]))

;<Template from Natural + accumulator>
; - acc is result-so-far accumulator

(define (fact n0)
  ;; acc: Natural; represent the products of the naturals so far
  ;; (fact 5)
  ;;
  ;; (fact 5 1)
  ;; (fact 4 5)
  ;; (fact 3 20)
  ;; (fact 2 60)
  ;; (fact 1 120)
  ;; (fact 0 120)
  ;; (fact   120)
  (local [(define (fact n acc)
            (cond [(zero? n) acc]
                  [else
                   (fact (sub1 n) (* acc n))]))]
    (fact n0 1)))

;  PROBLEM 4:
;  
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist 
;  accumulator to design a tail recursive function that counts the number of regions 
;  within and including a given region. 
;  So (count-regions CANADA) should produce 7

;; =================
;; Data Definitions:
;; -----------------

(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else 
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))

;; ==========
;; Functions:
;; ----------

;; Region -> Natural
;; produce the number of regions within and including a given region
(check-expect (count-region VANCOUVER) 1)
(check-expect (count-region ALBERTA)   3)
(check-expect (count-region CANADA)    7)

;(define (count-region r) 0) ;stub

;<Template according to Region + accumulators>
; - todo is a worklist accumulator
; - acc is a result-so-far accumulator

(define (count-region r0)
  ;; todo: (listof Region); the list of regions that need to be seen
  ;; (count-region ALBERTA)
  ;;
  ;; (count-region ALBERTA empty)
  ;; (count-region CALGARY (list EDMONTON))
  ;; (count-region EDMONTON empty)
  ;; -------------------------------------------------
  ;; acc: Natural; the quantity of regions seen so far
  ;; (count-region ALBERTA)
  ;;
  ;; (count-region ALBERTA  0)
  ;; (count-region CALGARY  1)
  ;; (count-region EDMONTON 2)
  ;; (count-region          3)
  (local [(define (fn-for-region r todo acc)
            (fn-for-lor (region-subregions r) (if (empty? (region-subregions r))
                                                  todo
                                                  (append todo (rest (region-subregions r)))) (add1 acc)))
          
          (define (fn-for-lor lor todo acc)
            (cond [(empty? todo) acc]
                  [(empty? lor) (fn-for-region (first todo) empty acc)]
                  [else
                   (if (empty? (region-subregions (first lor)))
                       (fn-for-region (first todo) (rest todo) (add1 acc))
                       (fn-for-lor (region-subregions (first lor))
                                   (append todo (rest (region-subregions (first lor))))
                                   (add1 acc)))]))]
    (fn-for-region r0 empty 0)))
