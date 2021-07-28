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

;; Data Definitions:

;; Player is String
;; interp.  the name of a tennis player.
(define P0 "Maria")
(define P2 "Serena")

#;
(define (fn-for-player p)
  (... p))

;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; interp.  a team roster, ordered from best player to worst.
(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Roger" "Matthew" "Phillip" "Daryl"))
(define R3 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))

(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp.  a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))

;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))

;; ==========
;; Functions:
; Problem 1:
; 
; Suppose you have rosters for players on two opposing tennis team, and each
; roster is ordered by team rank, with the best player listed first. When both 
; teams play, the best players of each team play one another,
; and the second-best players play one another, and so on down the line. When
; one team has more players than the other, the lowest ranking players on
; the larger team do not play.
; 
; Design a function that consumes two rosters, and produces true if all players 
; on both teams will play if the teams play each other. 
; 

;; Roster Roster -> Boolean
;; produce true if all players on both teams will play if the teams play each other
;;  - i.e. both teams have the same number of players
(check-expect (all-play? empty empty) true) ;simple cases
(check-expect (all-play? empty R1)   false)
(check-expect (all-play? R1 empty)   false)

(check-expect (all-play? R1 R2)  true) ;teams both have 4 players
(check-expect (all-play? R1 R3) false) ;one team has 4 players and other has 5 players

;(define (all-play? lsta lstb) false) ;stub

(define (all-play? r1 r2)
  (= (length r1) (length r2)))

; Problem 2:
; 
; Now write a function that, given two teams, produces the list of tennis matches
; that will be played. 
; 
; Assume that this function will only be called if the function you designed above
; produces true. In other words, you can assume the two teams have the same number
; of players. 
; 

;; Roster Roster -> ListOfMatch
;; given two teams, produce list of tennis matches that will be played
;;  - assume both teams have same number of players
(check-expect (tennis-matches empty empty) empty) ;simple case

(check-expect (tennis-matches R1 R2)
              (list (make-match "Eugenie" "Roger")
                    (make-match "Gabriela" "Matthew")
                    (make-match "Sharon" "Phillip")
                    (make-match "Aleksandra" "Daryl")))

;(define (tennis-matches r1 r2) empty) ;stub

(define (tennis-matches r1 r2)
  (cond [(empty? r1) empty]
        [else
         (cons (make-match (first r1) (first r2))
               (tennis-matches (rest r1)
                               (rest r2)))]))
