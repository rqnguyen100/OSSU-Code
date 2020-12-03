;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Design Quiz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; Design a function that consumes two images and produces true if the first is larger than second

;; Image Image -> Boolean
;; Input two images and outputs true if area of first larger than second
(check-expect (first_is_larger? (rectangle 10 10 "solid" "red")
                                (rectangle 20 10 "solid" "red")) false)
(check-expect (first_is_larger? (circle 40 "solid" "red")
                                (circle 30 "solid" "red")) true)
(check-expect (first_is_larger? (star 30 "solid" "red")
                                (star 30 "solid" "red")) false)

;(define (first_is_larger? img1 img2) true) ;stub

;(define (first_is_larger? img1 img2) ;template
;         (... img1) (...img2))

(define (first_is_larger? img1 img2)
  (> (* (image-height img1) (image-width img1))
     (* (image-height img2) (image-width img2))))

; Evaluation: [Poor, Fair, Good, Excellent(only for Design Completeness)]
; Commit Ready: Good
; Design Completeness: Good
; Internal Quality: Good
  ;There should be a test for every single case
  ;There are 9 possible cases, so 9 tests are needed
; Problem Satisfied: Good