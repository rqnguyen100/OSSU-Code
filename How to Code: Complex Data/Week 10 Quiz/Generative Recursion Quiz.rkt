;; Rubric:
;; -------
;; Commit Ready (G/F/P):
;;  - Good
;; Design Completeness (G/F/P):
;;  - Good
;; Internal Quality (E/G/F/P):
;;  - Good
;; Problem Satisfied (G/F/P):
;;  - Good

;; ==================
(require 2htdp/image)
(require racket/list) ;gets list-ref, take and drop

;  PROBLEM 1:
;  
;  In the lecture videos we designed a function to make a Sierpinski triangle fractal. 
;  
;  We want a geometric fractal that is made of circles rather than triangles:
;  
;  Design a function to create this circle fractal of size n and colour c.
;  


(define CUTOFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (circle-fractal CUTOFF "black") (circle CUTOFF "outline" "black")) ;trivial case

(check-expect (circle-fractal (* CUTOFF 2) "blue") (overlay (circle (* CUTOFF 2) "outline" "blue")
                                                            (beside (circle CUTOFF "outline" "blue")
                                                                    (circle CUTOFF "outline" "blue"))))

;(define (circle-fractal n c) empty-image) ;stub

(define (circle-fractal n c)
  (cond [(<= n CUTOFF) (circle n "outline" c)]
        [else
         (local [(define sub (circle-fractal (/ n 2) c))]
           (overlay (circle n "outline" c)
                    (beside sub
                            sub)))]))

;  PROBLEM 2:
;  
;  Below you will find some data definitions for a tic-tac-toe solver. 
;  
;  In this problem we want you to design a function that produces all 
;  possible filled boards that are reachable from the current board. 
;  
;  In actual tic-tac-toe, O and X alternate playing. For this problem
;  you can disregard that. You can also assume that the players keep 
;  placing Xs and Os after someone has won. This means that boards that 
;  are completely filled with X, for example, are valid.
;  
;  Note: As we are looking for all possible boards, rather than a winning 
;  board, your function will look slightly different than the solve function 
;  you saw for Sudoku in the videos, or the one for tic-tac-toe in the 
;  lecture questions. 
;  

;; ==========
;; Constants:

(define BLK false)

;; =================
;; Data Definitions:

;; Value is one of:
;; - BLK
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

#;
(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; -----------------------
;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list BLK BLK BLK
                 BLK BLK BLK
                 BLK BLK BLK))

(define B1 (list BLK "X" "O"   ; a partly finished board
                 "O" "X" "O"
                 BLK BLK "X")) 

(define B2 (list "X" "X" "O"   ; a board where X will win
                 "O" "X" "O"
                 "X" BLK "X"))

(define B3 (list "X" "O" "X"   ; a board where Y will win
                 "O" "O" BLK
                 "X" "X" BLK))

#;
(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))

;; -------------------
;; Pos is Natural[0, 8]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)

;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests

;; ==========
;; Functions:

;; Board -> (listof Board)
;; produce all possible boards from given board
;;  - ASSUME: alternate playing between Xs and 0s is disregarded
;;    - This means that boards that are completely filled with X, for example, are valid
;;  - ASSUME: players keep placing Xs and Os even after someone has already won
(check-expect (possible B2) (list (list "X" "X" "O"  
                                        "O" "X" "O"
                                        "X" "X" "X")
                                  (list "X" "X" "O"  
                                        "O" "X" "O"
                                        "X" "O" "X")))
(check-expect (possible B3) (list (list "X" "O" "X"
                                        "O" "O" "X"
                                        "X" "X" "X")
                                  (list "X" "O" "X"
                                        "O" "O" "X"
                                        "X" "X" "O")
                                  (list "X" "O" "X"
                                        "O" "O" "O"
                                        "X" "X" "X")
                                  (list "X" "O" "X"
                                        "O" "O" "O"
                                        "X" "X" "O")))
(check-expect (member (list "X" "X" "X"
                            "X" "X" "X"
                            "X" "X" "X") (possible B0)) true) ;checks if a board filled with X is in the output

;(define (possible bd) empty) ;stub

(define (possible bd)
  (local [(define (possible--bd bd)
            (cond [(complete? bd) (list bd)]
                  [else
                   (possible--lobd (next-boards bd))]))

          (define (possible--lobd lobd)
            (cond [(empty? lobd) empty]
                  [else 
                   (append (possible--bd (first lobd))
                           (possible--lobd (rest lobd)))]))]
    (possible--bd bd)))

;; ----------------
;; Board -> Boolean
;; produce true if board is completed
;;  - complete: no more blank spaces | no spaces are false
(check-expect (complete? B0) false)
(check-expect (complete? (list "X" "O" "X"
                               "O" "O" "O"
                               "X" "X" "O")) true)

;(define (complete? bd) false) ;stub

(define (complete? bd)
  (andmap string? bd))

;; -----------------------
;; Board -> (listof Board)
;; produce list of next boards from Board
;;  - finds first empty square, fills it with "X"|"O"
(check-expect (next-boards B0) (list (list "X" BLK BLK
                                           BLK BLK BLK
                                           BLK BLK BLK)
                                     (list "O" BLK BLK
                                           BLK BLK BLK
                                           BLK BLK BLK)))
(check-expect (next-boards B2) (list (list "X" "X" "O"   
                                           "O" "X" "O"
                                           "X" "X" "X")
                                     (list "X" "X" "O"   
                                           "O" "X" "O"
                                           "X" "O" "X")))

;(define (next-boards bd) empty) ;stub

(define (next-boards bd)
  (fill-with-X-O (find-blank bd) bd))

;; ------------
;; Board -> Pos
;; produces the position of the first blank square
;;  - ASSUME: the board has at least one blank square
(check-expect (find-blank B0) 0)
(check-expect (find-blank B2) 7)
(check-expect (find-blank B3) 5)

;(define (find-blank bd) 0) ;stub

(define (find-blank bd)
  (cond [(empty? bd) (error "Board doesn't have a blank space")]
        [else
         (if (false? (first bd))
             0
             (+ 1 (find-blank (rest bd))))]))

;; -----------------------
;; Pos Board -> (listof Board)
;; produce 2 boards, with blank filled with "X"|"O"
(check-expect (fill-with-X-O 0 B0) (list (cons "X" (rest B0))
                                         (cons "O" (rest B0))))

;(define (fill-with-X-O pos bd) empty) ;stub

(define (fill-with-X-O pos bd)
  (list (fill-square bd pos "X")
        (fill-square bd pos "O")))

;; ----------------------
;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square B0 (r-c->pos 0 0) "X")
              (cons "X" (rest B0)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))

;  PROBLEM 3:
;  
;  Now adapt your solution to filter out the boards that are impossible if 
;  X and O are alternating turns. You can continue to assume that they keep 
;  filling the board after someone has won though. 
;  
;  You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;  
;  NOTE: make sure you keep a copy of your solution from problem 2 to answer 
;  the questions on edX.
;  

;; ==========
;; Functions:

;; Board -> (listof Board)
;; produce all valid boards from given board with alternate playing between Xs and 0s
;;  - ASSUME: X plays first
;;  - ASSUME: players keep placing Xs and Os even after someone has already won
(check-expect (valid B2) (list (list "X" "X" "O"  
                                     "O" "X" "O"
                                     "X" "O" "X")))
(check-expect (valid B3) (list (list "X" "O" "X"
                                     "O" "O" "X"
                                     "X" "X" "O")
                               (list "X" "O" "X"
                                     "O" "O" "O"
                                     "X" "X" "X")))

;(define (valid bd) empty) ;stub

(define (valid bd)
  (local [(define (valid--bd bd)
            (cond [(complete? bd) (list bd)]
                  [else
                   (valid-lobd (next-valid-boards bd))]))

          (define (valid-lobd lobd)
            (cond [(empty? lobd) empty]
                  [else 
                   (append (valid--bd (first lobd))
                           (valid-lobd (rest lobd)))]))]
    (valid--bd bd)))

;; -----------------------
;; Board -> (listof Board)
;; produce list of the next valid boards from Board
;;  - finds first empty square, fills it with "X"|"O", filter out boards that are impossible with alternating turns
(check-expect (next-valid-boards B0) (list (list "X" BLK BLK
                                                 BLK BLK BLK
                                                 BLK BLK BLK)
                                           (list "O" BLK BLK
                                                 BLK BLK BLK
                                                 BLK BLK BLK)))
(check-expect (next-valid-boards B2) (list (list "X" "X" "O"   
                                                 "O" "X" "O"
                                                 "X" "O" "X"))) ;unlike previous question, only "0" can be placed

;(define (next-valid-boards bd) empty) ;stub

(define (next-valid-boards bd)
  (valid-boards (fill-with-X-O (find-blank bd) bd)))

;; --------------------------------
;; (listof Board) -> (listof Board)
;; filters out the impossible boards
(check-expect (valid-boards (list (list "X" "X" "O"   
                                        "O" "X" "O"
                                        "X" "O" "X")
                                  (list "X" "X" "O"   
                                        "O" "X" "O"
                                        "X" "X" "X")))
              (list (list "X" "X" "O"   
                          "O" "X" "O"
                          "X" "O" "X")))
(check-expect (valid-boards (list (list "O" "O" "O"
                                        "O" "O" BLK
                                        BLK BLK BLK)))
              empty)

;(define (valid-boards lobd) empty) ;stub

(define (valid-boards lobd)
  (local [(define (valid? b)                                                     ;Board -> Boolean
            (if (O-filter (fill-space (keep-only-values b)))
                false
                (not (X-filter (fill-space (keep-only-values b))))))

          (define (keep-only-values b)                                           ;Board -> Board
            (filter string? b))                                                  ;filters out the empty squares

          (define (fill-space b)                                                 ;Board -> (listof Strings)
            (local [(define (W-front n) (string-append "W" (number->string n)))]
            (append b (build-list (- 9 (length b)) W-front))))                   ;replaces empty squares with "W"

          (define (O-filter b)                                                   ;(listof String) -> Boolean
            (string=? (fifth (sort b string<?)) "O"))                            ;produce true if there are more than 4 0s
     
          (define (X-filter b)                                                   ;(listof String) -> Boolean
            (string=? (sixth (sort b string>?)) "X"))]                           ;produce true if there are more than 5 Xs
    (filter valid? lobd)))
