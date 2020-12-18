;; Rubric
;; -----------
;; Safe(U/A):
;;  - Acceptable
;; Committ Ready(P/F/G):
;;  - Good
;; HtDW(P/F/G/E):
;;  - Excellent
;; HtDD(P/F/G/E):
;;  - Good
;;    - Didn't add template rules
;; HtDF(P/F/G/E):
;;  - Excellent
;; Problem Satisfied(P/F/G/E):
;;  - Excellent


;; ====================
(require 2htdp/image)
(require 2htdp/universe)

;; A simple one-line text editor

;; =========
;; Constants

(define WIDTH 300)
(define HEIGHT 20)

(define TEXT-SIZE 14)
(define TEXT-COLOUR "black")

(define MTS (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 2 14 "solid" "red"))

(define CTR-HEIGHT (/ HEIGHT 2))
(define TEXT-X (+ (/ TEXT-SIZE 2) 1))

;; ================
;; Data Definitions

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))  ;no text before and after cursor
(define E1 (make-editor "a" "")) ;a before cursor and no text after
(define E2 (make-editor "" "b")) ;no text before cursor and b after

#;
(define (fn-for-editor e)
  (... (editor-pre e)    ;String
       (editor-post e))) ;String
;; Template rules used:
;;  - compound: 2 fields

;; =================
;; Functions:

;; Editor -> Editor
;; start the world with (main (make-editor "" ""))
;; 
(define (main e)
  (big-bang e                        ; Editor
            (to-draw   render)       ; Editor -> Image
            (on-key    handle-key))) ; Editor KeyEvent -> Editor

;; -----------
;; Editor -> Image
;; renders text and cursor on MTS starting at the left edge
(check-expect (render (make-editor "" ""))                                 ;no text before and after cursor
                      (place-image CURSOR TEXT-X CTR-HEIGHT MTS))

(check-expect (render (make-editor "a" ""))                                 ;letter "a" before cursor and no text after
                      (place-image (beside (text "a" TEXT-SIZE TEXT-COLOUR)
                                           CURSOR)
                                   TEXT-X CTR-HEIGHT MTS))
(check-expect (render (make-editor "" "b"))                                 ;no text before cursor and letter "b" after
                      (place-image (beside CURSOR
                                           (text "b" TEXT-SIZE TEXT-COLOUR))
                                   (/ TEXT-X 2) CTR-HEIGHT MTS))

(check-expect (render (make-editor "!" ""))                                 ;special character "!" before cursor and no text after
                      (place-image (beside (text "!" TEXT-SIZE TEXT-COLOUR)
                                           CURSOR)
                                   TEXT-X CTR-HEIGHT MTS))
(check-expect (render (make-editor "" "@"))                                 ;no text before cursor and special character "@" after
                      (place-image (beside CURSOR
                                           (text "@" TEXT-SIZE TEXT-COLOUR))
                                   (/ TEXT-X 2) CTR-HEIGHT MTS))

(check-expect (render (make-editor "1" ""))                                 ;number "1" before cursor and no text after
                      (place-image (beside (text "1" TEXT-SIZE TEXT-COLOUR)
                                           CURSOR)
                                   TEXT-X CTR-HEIGHT MTS))
(check-expect (render (make-editor "" "2"))                                 ;no text before cursor and number "2" after
                      (place-image (beside CURSOR
                                           (text "2" TEXT-SIZE TEXT-COLOUR))
                                   (/ TEXT-X 2) CTR-HEIGHT MTS))

(check-expect (render (make-editor "H" "i"))                                ;text before and after cursor
                      (place-image (beside (text "H" TEXT-SIZE TEXT-COLOUR)
                                           CURSOR
                                           (text "i" TEXT-SIZE TEXT-COLOUR))
                                   TEXT-X CTR-HEIGHT MTS))
              
;(define (render e) (text "Hello" TEXT-SIZE TEXT-COLOUR)) ;stub

;;<Template from Editor>

(define (render e)
  (cond [(and (string=? (editor-pre e) "") (string=? (editor-post e) ""))
         (place-image CURSOR TEXT-X CTR-HEIGHT MTS)]
        [(string=? (editor-pre e) "")
         (place-image (beside CURSOR
                              (text (editor-post e) TEXT-SIZE TEXT-COLOUR))
                      (+ (/ (* TEXT-X (string-length (editor-pre e))) 2) (/ TEXT-X 2)) CTR-HEIGHT MTS)]
        [(string=? (editor-post e) "")
         (place-image (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                              CURSOR)
                      (+ (/ (* TEXT-X (string-length (editor-pre e))) 2) (/ TEXT-X 2)) CTR-HEIGHT MTS)]
        [else
         (place-image (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                              CURSOR
                              (text (editor-post e) TEXT-SIZE TEXT-COLOUR))
                      (+ (/ (* TEXT-X (string-length (editor-pre e))) 2) (/ TEXT-X 2)) CTR-HEIGHT MTS)]))

;; -------------------------
;; Editor KeyEvent -> Editor
;; if a letter, number or special character key is pressed, type it out
;; if left or right arrow is pressed, move cursor accordingly
;; if backspace is pressed, delete character left of cursor
(check-expect (handle-key (make-editor "" "") "a") (make-editor "a" "")) ;starts with letter
(check-expect (handle-key (make-editor "" "") "1") (make-editor "1" "")) ;starts with number
(check-expect (handle-key (make-editor "" "") "!") (make-editor "!" "")) ;starts with special character

(check-expect (handle-key (make-editor "a" "") "b") (make-editor "ab" "")) ;continues with letter
(check-expect (handle-key (make-editor "1" "") "2") (make-editor "12" "")) ;continues with number
(check-expect (handle-key (make-editor "!" "") "@") (make-editor "!@" "")) ;continues with special character

(check-expect (handle-key (make-editor "ab" "") "left") (make-editor "a" "b"))  ;left arrow pressed
(check-expect (handle-key (make-editor "" "12") "right") (make-editor "1" "2")) ;right arrow pressed

(check-expect (handle-key (make-editor "!@" "") "\b") (make-editor "!" "")) ;backspace is pressed with no text after cursor
(check-expect (handle-key (make-editor "c" "d") "\b") (make-editor "" "d")) ;backspace is pressed with text before and after cursor

(check-expect (handle-key (make-editor "a" "b") "shift") (make-editor "a" "b")) ;non-acceptable key pressed

;(define (handle-key e key) (make-editor "" "")) ;stub

#;
(define (handle-key ws ke)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))

(define (handle-key e key)
  (cond [(key=? key "left") (make-editor (substring (editor-pre e) 0 (- (string-length (editor-pre e)) 1))
                                         (string-append (substring (editor-pre e) (- (string-length (editor-pre e)) 1)) (editor-post e)))]
        [(key=? key "right") (make-editor (string-append (editor-pre e) (substring (editor-post e) 0 1))
                                          (substring (editor-post e) 1))]
        [(key=? key "\b") (make-editor (substring (editor-pre e) 0 (if (>= (- (string-length (editor-pre e)) 1) 0)
                                                                       (- (string-length (editor-pre e)) 1)
                                                                       0))
                                       (editor-post e))]
        [(and (= (string-length key) 1) (and (not (string=? "\r" key)) (not (string=? "\t" key))))
         (make-editor (string-append (editor-pre e) key) (editor-post e))]
        [else
         (make-editor (editor-pre e) (editor-post e))]))
