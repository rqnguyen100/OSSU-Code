(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define WIDTH 300)
(define HEIGHT 300)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define BEAR [image of bear]) ;the image could not be transferred to GitHub

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data Definitions:

(define-struct bear (x y z dz))
;; Bear is (make-bear Number[0,WIDTH] Number[0,HEIGHT] Natural[0,360) Natural)
;; interp. a bear at position x,y initially rotated z degrees counter-clockwise (c-cw) with angular velocity dz 
;;         x is x-position (Pixels)
;;         y is y-position (Pixels)
;;         z is degrees initially rotated (Degree)
;;         dz is angular velocity (Degrees per second)

(define BEAR-1 (make-bear 0 0 0 0))          ;a bear at (0,0) rotated 0 degrees c-cw with angular velocity 0
(define BEAR-2 (make-bear CTR-X CTR-Y 90 2)) ;a bear at the center rotated 90 degrees c-cw with angular velocity 2

#;
(define (fn-for-bear b)
  (... (bear-x b)       ;Number[0,WIDTH]
       (bear-y b)       ;Number[0,HEIGHT]
       (bear-z b)       ;Natural[0,360)
       (bear-dz b)))    ;Natural
;; Template rules used:
;;  - compound: 4 fields (make-bear x y z dz)

;; ---------------------
;; ListOfBear is one of:
;;  - empty
;;  - (cons Bear ListOfBear)
;; interp. a list of Bears

(define LOB1 empty)                                                          ;empty list
(define LOB2 (cons (make-bear 0 0 0 0) empty))                               ;list of one bear
(define LOB3 (cons (make-bear 0 0 0 0) (cons (make-bear 90 90 90 1) empty))) ;list of more than one bear

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]                   ;Base Case
        [else (... (first lob)                 ;Bear
                   (fn-for-lob (rest lob)))])) ;Recursive Case
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Bear ListOfBear)
;;  - reference: (first lob) is BEar
;;  - self-reference: (rest lob) is ListOfBear

;; ==========
;; Functions:

;; ListOfBear -> ListOfBear
;; start the world with (main empty)
;; 
(define (main lob)
  (big-bang lob                         ; ListOfBear
            (on-tick   rotate-bear)     ; ListOfBear -> ListOfBear
            (to-draw   render-bear)     ; ListOfBear -> Image
            (on-mouse  handle-mouse)))  ; ListOfBear Integer Integer MouseEvent -> ListOfBear

;; ------------
;; ListOfBear -> ListOfBear
;; rotates the list of bears counter-clockwise with angular velocity dz
(check-expect (rotate-bear empty) empty) ;empty list

(check-expect (rotate-bear (cons (make-bear CTR-X CTR-Y 0 1) empty)) ;one bear rotating
              (cons (make-bear CTR-X CTR-Y 1 1) empty))

(check-expect (rotate-bear (cons (make-bear CTR-X CTR-Y 0 1) (cons (make-bear 0 0 90 2) empty))) ;two bears rotating
              (cons (make-bear CTR-X CTR-Y 1 1) (cons (make-bear 0 0 92 2) empty)))

;(define (rotate-bear lob) empty) ;stub

;;<Template from ListOfBear>

(define (rotate-bear lob)
  (cond [(empty? lob) empty]
        [else
         (cons (make-bear (bear-x (first lob))
                    (bear-y (first lob))
                    (+ (bear-z (first lob)) (bear-dz (first lob)))
                    (bear-dz (first lob)))
         (rotate-bear (rest lob)))]))

;; -------------
;; ListOfBear -> Image
;; renders image of list of bears
(check-expect (render-bear empty) MTS)

(check-expect (render-bear (cons (make-bear CTR-X CTR-Y 0 1) empty)) ;renders one bear
              (place-image BEAR CTR-X CTR-Y MTS))

(check-expect (render-bear (cons (make-bear CTR-X CTR-Y 0 1) (cons (make-bear 0 0 0 0) empty))) ;renders two bears
              (place-images
               (list (rotate 0 BEAR)
                     (rotate 0 BEAR))
               (list (make-posn 0 0)
                     (make-posn CTR-X CTR-Y))
                     MTS))

;(define (render-bear lob) MTS) ;stub

;;<Template from ListOfBear>

(define (render-bear lob)
  (cond [(empty? lob) MTS]
        [else
         (place-image (rotate (bear-z (first lob)) BEAR)
                      (bear-x (first lob))
                      (bear-y (first lob))
         (render-bear (rest lob)))]))

;; ---------------------------------------
;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; when mouse is clicked, place new image of bear at mouse position with initial degree rotated reset to 0
(check-expect (handle-mouse empty 100 100 "button-down") ;mouse is clicked on empty list
              (cons (make-bear 100 100 0 1) empty))


(check-expect (handle-mouse (cons (make-bear 0 0 0 1) empty) 100 100 "button-down") ;mouse is clicked on non-empty list
              (cons (make-bear 100 100 0 1) (cons (make-bear 0 0 0 1) empty)))

(check-expect (handle-mouse (cons (make-bear 0 0 0 1) empty) 200 200 "button-up")  ;mouse is not clicked
              (cons (make-bear 0 0 0 1) empty))

;(define (handle-mouse lob x y me) empty) ;stub

#;
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down") (... ws x y)]
        [else
         (... ws x y)]))

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear x y 0 1) lob)]
        [else
         lob]))
