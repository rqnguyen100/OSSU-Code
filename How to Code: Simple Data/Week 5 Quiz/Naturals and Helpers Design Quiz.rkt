;; Rubric:
;; Commit Ready: (G/F/P)
;;  - Good
;; Design Completeness: (G/F/P)
;;  - Good
;; Internal Quality: (E/G/F/P)
;;  - Excellent
;; Problem Satisfied: (G/F/P)
;;  - Good

(require 2htdp/image)
;; ======================================================================
;; Constants
(define COOKIES [insert image of cookie]) ;images don't copy on GitHub

;; ======================================================================
;; Data Definitions

;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]                   ;Base Case
        [else
         (... n                             ;Natural             
              (fn-for-natural (sub1 n)))])) ;Recursive Case

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: 2 fields
;;  - self-reference: (sub1 n) is Natural

; PROBLEM 1:
; 
; Complete the design of a function called pyramid that takes a natural
; number n and an image, and constructs an n-tall, n-wide pyramid of
; copies of that image.

;; ----------------------
;; Natural Image -> Image
;; produce an n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)          ;0 level pyramid
(check-expect (pyramid 1 COOKIES) COOKIES)              ;1 level pyramid
(check-expect (pyramid 3 COOKIES)                       ;3 level pyramid
              (above/align "center"
                     COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

;(define (pyramid n i) empty-image) ; stub

;;<Template from Natural>

(define (pyramid n img)
  (cond [(zero? n) empty-image]
        [else
         (stack (pyramid (sub1 n) img)
                n)]))

;; ----------------------
;; Image Natural -> Image
;; creates stack of an n-wide pyramid of the given image
(check-expect (stack COOKIES 0) empty-image)          ;0 level pyramid
(check-expect (stack COOKIES 1) COOKIES)              ;1 level pyramid
(check-expect (stack COOKIES 3)                       ;3 level pyramid
              (above/align "center"
                     COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

;(define (stack img n) empty-image) ;stub

;;<Template from Natural>

(define (stack img n)
  (cond [(zero? n) empty-image]    
        [else
         (above/align "center"
                      (stack (level-img img (sub1 n)) (sub1 n))
                      (level-img img n))]))

;; ----------------------
;; Image Natural -> Image
;; creates pyramid level of an n-wide pyramid of the given image
(check-expect (level-img COOKIES 0) empty-image)      ;0 element level
(check-expect (level-img COOKIES 1) COOKIES)          ;1 element level
(check-expect (level-img COOKIES 3)                   ;3 element level
              (beside COOKIES COOKIES COOKIES))

;(define (level-img img n) empty-image) ;stub

;;<Template from Natural>

(define (level-img img n)
  (cond [(zero? n) empty-image]  
        [else
         (beside COOKIES
                 (level-img img (sub1 n)))]))

; Problem 2:
; Consider a test tube filled with solid blobs and bubbles.  Over time the
; solids sink to the bottom of the test tube, and as a consequence the bubbles
; percolate to the top.  Let's capture this idea in BSL.
; 
; Complete the design of a function that takes a list of blobs and sinks each
; solid blob by one. It's okay to assume that a solid blob sinks past any
; neighbor just below it.
; 
; To assist you, we supply the relevant data definitions.

;; ---------------
;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid, or a bubble
;; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 3 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"

;; ---------------------
;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty)                                ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; -----------------------
;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one
(check-expect (sink empty) empty) ;empty list
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))

;(define (sink lob) empty) ; stub

(define (sink lob)
  (sort-lob lob))

;; ------------------------
;; ListOfBlob -> ListOfBlob
;; sorts list and indexs all "solid" down one
(check-expect (sort-lob empty) empty) ;empty list

(check-expect (sort-lob (cons "bubble" empty)) ;one element list
              (cons "bubble" empty))

(check-expect (sort-lob (cons "solid" (cons "bubble" empty))) ;two element list: not sorted
              (cons "bubble" (cons "solid" empty)))
(check-expect (sort-lob (cons "bubble" (cons "solid" empty))) ;two element list: sorted
              (cons "bubble" (cons "solid" empty)))

;(define (sort-lob lob) lob) ;stub

(define (sort-lob lob)
  (cond [(empty? lob) lob]
        [(= (length lob) 1) lob]
        [else
         (switch (first lob)
                 (sort-lob (rest lob)))]))

;; -----------------------------
;; Blob ListOfBlob -> ListOfBlob
;; switches place if solid blob is before bubble blob, else puts in front
;; ASSUME: lob is already sorted
(check-expect (switch "solid" empty) ;empty list
              (cons "solid" empty))

(check-expect (switch "solid" (cons "solid" empty)) ;doesn't switch places
              (cons "solid" (cons "solid" empty)))

(check-expect (switch "solid" (cons "bubble" empty)) ;switches places
              (cons "bubble" (cons "solid" empty)))

(check-expect (switch "solid" (cons "bubble" (cons "bubble" empty))) ;switches places and leaves second element of list alone
              (cons "bubble" (cons "solid" (cons "bubble" empty))))

;(define (switch b lob) lob) ;stub

(define (switch b lob)
  (cond [(empty? lob) (cons b empty)]
        [else
         (if (bubble? (first lob))
             (cons (first lob)
                         (cons b (rest lob)))
             (cons b lob))]))

;; ---------------
;; Blob -> Boolean
;; if "bubble", return true
(check-expect (bubble? "bubble") true) ;is bubble
(check-expect (bubble? "solid") false) ;is not bubble

;(define (bubble? b) true) ;stub

(define (bubble? b)
  (string=? "bubble" b))
