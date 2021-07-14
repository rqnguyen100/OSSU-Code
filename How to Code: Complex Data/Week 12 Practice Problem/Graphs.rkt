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


; 
; PROBLEM:
; 
; Using the following data definition, design a function that produces the room with the most exits 
; (in the case of a tie you can produce any of the rooms in the tie).
; 


;; =================
;; Data Definitions: 
;; -----------------

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))
           
(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist, 
;;           context-preserving accumulator what rooms have we already visited

(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty))) 

;; ==========
;; Functions:
;; ----------

;; Room -> Room
;; produce the room with the most exits
;;  - EXCEPTION: in event of a tie, produce any room from the tie
(check-expect (most-exits H1) (make-room "A" (list (make-room "B" empty))))
(check-expect (most-exits H2)                    H2) ;tie
(check-expect (most-exits H3)                    H3) ;tie
(check-expect (most-exits (shared ((-A- (make-room "A" (list -B-)))
                                       (-B- (make-room "B" (list -C- -A-)))
                                       (-C- (make-room "C" (list -A-))))
                                -A-)) 
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -C- -A-)))
                       (-C- (make-room "C" (list -A-))))
                -B-))
(check-expect (most-exits (shared ((-A- (make-room "A" (list -B- -D-)))
                                   (-B- (make-room "B" (list -C- -E-)))
                                   (-C- (make-room "C" (list -B-)))
                                   (-D- (make-room "D" (list -E-)))
                                   (-E- (make-room "E" (list -F- -A-)))
                                   (-F- (make-room "F" (list))))
                            -A-))
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -A-))

;(define (most-exits r) (make-room "A" empty)) ;stub

(define (most-exits r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; rsf is Room; context preserving accumulator, room with most exits so far
  (local [(define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (update r rsf))))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                rsf)]))

          ;helper
          (define (update r rsf)
            (if (> (length (room-exits r)) (length (room-exits rsf)))
                r
                rsf))]
    (fn-for-room r0 empty empty r0))) 
