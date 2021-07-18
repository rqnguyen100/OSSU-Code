;; Rubric:
;; -------
;; Commit Ready (G/F/P):
;;  - Good
;; Design Completeness (G/F/P):
;;  - Good
;; Internal Quality (E/G/F/P):
;;  - Good
;; Problem Satisfied (G/F/P):
;;  - Fair: one check-expect returns incorrect answer 

;  PROBLEM 1:
;  
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people. 
;  
;  Design a data definition for Chirper, including a template that is tail recursive and avoids 
;  cycles. 
;  
;  Then design a function called most-followers which determines which user in a Chirper Network is 
;  followed by the most people.
;  

;; ============================= Data Defintions


(define-struct chirper (name verified? follows))
;; Chirper is (make-chirper String Boolean (listof User))
;; interp. name of a user, whether or not user is verified, and users they follow

(define C1 (make-chirper "A" true (list (make-chirper "B" false empty))))
(define C2
  (shared ((-A- (make-chirper "A" true  (list -B-)))
           (-B- (make-chirper "B" false (list -A-))))
    -A-))
(define C3
  (shared ((-A- (make-chirper "A" true  (list -B-)))
           (-B- (make-chirper "B" false (list -C-)))
           (-C- (make-chirper "C" true  (list -A-))))
    -A-))
(define C4
  (shared ((-A- (make-chirper "A" true  (list -B- -D-)))
           (-B- (make-chirper "B" false (list -C- -E-)))
           (-C- (make-chirper "C" true  (list -B-)))
           (-D- (make-chirper "D" true  (list -E-)))
           (-E- (make-chirper "E" false (list -F- -A-)))
           (-F- (make-chirper "F" true  (list))))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist, 
;;           context-preserving accumulator what users have we already visited

#;
(define (fn-for-user c0)
  ;; todo is (listof Chirper); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of user already seen
  (local [(define (fn-for-chirper c todo visited) 
            (if (member (chirper-name c) visited)
                (fn-for-loc todo visited)
                (fn-for-loc (append (chirper-follows c) todo)
                            (cons (chirper-name c) visited))))
          (define (fn-for-loc todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-chirper (first todo) 
                                   (rest todo)
                                   visited)]))]
    (fn-for-chirper c0 empty empty))) 


;; ============================= FUNCTIONS


;; Chirper -> Chirper
;; produces the user that is followed by the most people
;;  - EXCEPTION: in event of a tie, produce the user that is seen first
(check-expect (most-followers C1) (make-chirper "B" false empty))
(check-expect (most-followers C2) (shared ((-A- (make-chirper "A" true  (list -B-)))
                                           (-B- (make-chirper "B" false (list -A-))))
                                    -A-))
(check-expect (most-followers C3) (shared ((-A- (make-chirper "A" true  (list -B-)))
                                           (-B- (make-chirper "B" false (list -C-)))
                                           (-C- (make-chirper "C" true  (list -A-))))
                                    -A-))
(check-expect (most-followers C4) (shared ((-A- (make-chirper "A" true  (list -B- -D-)))
                                           (-B- (make-chirper "B" false (list -C- -E-)))
                                           (-C- (make-chirper "C" true  (list -B-)))
                                           (-D- (make-chirper "D" true  (list -E-)))
                                           (-E- (make-chirper "E" false (list -F- -A-)))
                                           (-F- (make-chirper "F" true  (list))))
                                    -B-))

;(define (most-followers c) c) ;stub

;<Template from Chirper>

(define (most-followers c0)
  ;; todo is (listof Chirper); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of user already seen
  ;; UserNFollows is (listof User); context preserving accumulator, number of followers attached to each user seen so far
  (local [(define-struct user (chirper number))
          ;; User is (make-user Chirper Natural)
          ;; interp. a chirper account, and number of followers

          (define (fn-for-chirper c todo visited unf) 
            (cond [(member? (chirper-name c) visited) (fn-for-loc todo visited (update-user c unf))]
                  [(and (not (string=? (chirper-name c) (chirper-name c0))) (not (empty? unf))) (fn-for-loc (append (chirper-follows c) todo)
                                                                                                            (cons (chirper-name c) visited)
                                                                                                            (append unf (list (make-user c 1))))]
                  [else
                   (fn-for-loc (append (chirper-follows c) todo)
                               (cons (chirper-name c) visited)
                               (append unf (list (make-user c 0))))]))
          (define (fn-for-loc todo visited unf)
            (cond [(empty? todo) unf]
                  [else
                   (fn-for-chirper (first todo) 
                                   (rest todo)
                                   visited
                                   unf)]))

          ;updates follower count of user
          (define (update-user c unf)
            ;; new-list is UserNFollows; context preserving acumulator, list of updated users
            (local [(define (fn-for-lounf lounf new-list)
                      (cond [(empty? lounf) new-list]
                            [else
                             (if (string=? (chirper-name c) (chirper-name (user-chirper (first lounf))))
                                 (append (list (make-user c (add1 (user-number (first lounf))))) new-list)
                                 (fn-for-lounf (rest lounf) (append (list (first lounf)) new-list)))]))]
              (fn-for-lounf unf empty)))

          ;finds the user with the most followers
          (define (max-followers c0)
            ;; acc is User; context preserving accumulator, user with most followers
            (local [(define (max-followers c acc)
                      (cond [(empty? c) (user-chirper acc)]
                            [else
                             (if (> (user-number (first c)) (user-number acc))
                                 (max-followers (rest c) (first c))
                                 (max-followers (rest c) acc))]))]
              (max-followers c0 (first c0))))]
    (max-followers (fn-for-chirper c0 empty empty empty))))

;  PROBLEM 2:
;  
;  In UBC's version of How to Code, there are often more than 800 students taking 
;  the course in any given semester, meaning there are often over 40 Teaching Assistants. 
;  
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
;  a program to do it for us! 
;  
;  Below are some data definitions for a simplified version of a TA schedule. There are some 
;  number of slots that must be filled, each represented by a natural number. Each TA is 
;  available for some of these slots, and has a maximum number of shifts they can work. 
;  
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
;  maximum shifts. If no such schedules exist, produce false. 
; 
;  You should supplement the given check-expects and remember to follow the recipe!

;; ============================= Data Defintions


;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

;; -----------------------------
(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))

#;
(define (fn-for-ta ta)
  (... (ta-name ta)
       (ta-max ta)
       (ta-avail ta)))

;; -----------------------------
(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

(define A1 (make-assignment SOBA 1))
(define A2 (make-assignment SOBA 3))

#;
(define (fn-for-agn agn)
  (... (assignment-ta agn)
       (assignment-slot agn)))

;; -----------------------------
;; Schedule is (listof Assignment)

(define S1 (list A1 A2))

#;
(define (fn-for-sch sch)
  (cond [(empty? sch) (...)]
        [else
         (... (first sch)
              (fn-for-sch (rest sch)))]))


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible
;; - ASSUME: Schedule appears in order of (listof TA)

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas (list SOBA) empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) ;solution returns false
              (list
               (make-assignment SOBA 1)
               (make-assignment SOBA 3)
               (make-assignment UDON 4)
               (make-assignment RAMEN 2)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)


;(define (schedule-tas tas slots) empty) ;stub

;<Template according to Generative Recursion, Backtracking Search, Encapsulate w/ Local, Accumulator>

(define (schedule-tas tas0 slots0)
  ; schedule is Schedule; a context-preserving accumulator, schedule so far
  (local [(define (schedule-s slots schedule)
            (cond [(solved? tas0 slots0 schedule) schedule]
                  [(empty? slots) empty]
                  [else 
                   (schedule-los (rest slots) (next-schedules tas0 (first slots) schedule))]))
          
          (define (schedule-los slots los)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (schedule-s slots (first los)))]
                     (if (false? try)
                         (schedule-los slots (rest los))
                         try))]))]
    
    (schedule-s slots0 empty)))

;; -----------------------------

;; (listof TA) (listof Slot) Schedule -> Boolean
;; produce true if schedule is completed and is a valid
;;  - All slots are filled
;;  - TAs are only assigned slots that they are available
;;  - TAs aren't assigned more than the number of slots they can work
;;  - Slots don't overlap
(check-expect (solved? NOODLE-TAs (list 1 2 3 4) (list
                                                  (make-assignment SOBA 1)
                                                  (make-assignment SOBA 3)
                                                  (make-assignment UDON 4)
                                                  (make-assignment RAMEN 2)))
              true)
(check-expect (solved? NOODLE-TAs (list 1 2 3 4) (list
                                                  (make-assignment SOBA 1)
                                                  (make-assignment SOBA 3)
                                                  (make-assignment UDON 4)))
              false)

;(define (solved? tas slots sch) false) ;stub

(define (solved? tas slots sch)
  (if (slots-filled? slots sch)
      (if (assigned-avail-only? sch) (not-overworked? tas sch) (no-overlap? sch))
      false))

;; -----------------------------

;; (listof Slot) Schedule -> Boolean
;; produce true if all slots are filled
(check-expect (slots-filled? (list 1 3) (list (make-assignment SOBA 1)
                                              (make-assignment SOBA 3)))
              true)
(check-expect (slots-filled? (list 1 3) (list (make-assignment SOBA 1)))
              false)

(check-expect (slots-filled? (list 1 3) (list (make-assignment SOBA 1)
                                              (make-assignment UDON 3)))
              true)

;(define (slots-filled? slots sch) false) ;stub

(define (slots-filled? slots sch)
  (= (length slots) (length sch)))

;; -----------------------------

;; Schedule -> Boolean
;; produce true if TAs are only assigned slots that they are available
(check-expect (assigned-avail-only? (list (make-assignment SOBA 1)
                                          (make-assignment SOBA 3)))
              true)
(check-expect (assigned-avail-only? (list (make-assignment SOBA 1)
                                          (make-assignment SOBA 4)))
              false)

(check-expect (assigned-avail-only? (list (make-assignment SOBA 1)
                                          (make-assignment UDON 2)
                                          (make-assignment UDON 3)))
              false)

;(define (assigned-avail-only? sch) false) ;stub

(define (assigned-avail-only? sch)
  (cond [(empty? sch) true]
        [else
         (if (member? (assignment-slot (first sch)) (ta-avail (assignment-ta (first sch))))
             (assigned-avail-only? (rest sch))
             false)]))

;; -----------------------------

;; (listof TA) Schedule -> Boolean
;; produce true if TAs aren't assigned more than the number of slots they can work
(check-expect (not-overworked? (list UDON) (list (make-assignment UDON 3)))
              true)
(check-expect (not-overworked? (list UDON) (list (make-assignment UDON 3)
                                                 (make-assignment UDON 4)))
              false)

(check-expect (not-overworked? (list SOBA UDON) (list (make-assignment SOBA 1)
                                                      (make-assignment SOBA 3)
                                                      (make-assignment UDON 4)))
              true)
(check-expect (not-overworked? NOODLE-TAs (list (make-assignment SOBA 1)
                                                (make-assignment UDON 4)
                                                (make-assignment RAMEN 2)))
              true)

;(define (not-overworked? tas sch) false) ;stub

(define (not-overworked? tas0 sch0)
  ;todo is Schedule; a worklist accumulator
  ;acc is (listof Pair); context-preserving accumulator, the list of how often different TAs appear in a schedule
  ;visited is (listof TA); result so far accumulator, the TAs we've seen so far
  (local [(define-struct pair (ta num))
          ;Pair is (make-pair TA Natural)
          ;interp. a TA and how often they appear in the schedule

          (define (fn-for-ass ass todo visited acc) 
            (cond [(empty? ass) false]
                  [(member? (assignment-ta ass) visited) (fn-for-sch todo visited (update-ta ass acc))]
                  [else
                   (fn-for-sch todo
                               (cons (assignment-ta ass) visited)
                               (append acc (list (make-pair ass 1))))]))
          (define (fn-for-sch todo visited acc)
            (cond [(empty? todo) acc]
                  [else
                   (fn-for-ass (first todo) 
                               (rest todo)
                               visited
                               acc)]))


          ;increases the frequency for a TA
          (define (update-ta ass acc0)
            ;; new-list is (listof TA); context preserving acumulator, list of updated TAs
            (local [(define (fn-for-ass acc new-list)
                      (cond [(empty? acc) new-list]
                            [else
                             (if (string=? (ta-name (assignment-ta ass)) (ta-name (assignment-ta (pair-ta (first acc)))))
                                 (append (list (make-pair (assignment-ta ass) (add1 (pair-num (first acc))))) new-list)
                                 (fn-for-ass (rest acc) (append (list (first acc)) new-list)))]))]
              (fn-for-ass acc0 empty)))

          ;checks if TAs are overworked
          (define (valid? tas pair)
            (cond [(empty? tas) true]
                  [(empty? pair) false]
                  [(false? pair) false]
                  [else
                   (if (>= (ta-max (first tas)) (pair-num (first pair)))
                       (valid? (rest tas) (rest pair))
                       false)]))]
    (valid? tas0 (fn-for-ass (if (empty? sch0)
                                 sch0
                                 (first sch0)) (if (empty? sch0)
                                                   sch0
                                                   (rest sch0)) empty empty))))

;; -----------------------------

;; Schedule -> Boolean
;; produce true if slots don't overlap
(check-expect (no-overlap? (list (make-assignment SOBA 1)
                                 (make-assignment SOBA 3)
                                 (make-assignment UDON 3)))
              false)
(check-expect (no-overlap? (list (make-assignment SOBA 1)
                                 (make-assignment SOBA 3)))
              true)

;(define (no-overlap? sch) false) ;stub

(define (no-overlap? sch0)
  (local [(define (shift-times sch)
            (cond [(empty? sch) empty]
                  [else
                   (cons (assignment-slot (first sch))
                         (shift-times (rest sch)))]))

          ;checks for duplicates
          (define (overlap? lon)
            (cond [(empty? lon) true]
                  [else
                   (if (member (first lon) (rest lon))
                       false
                       (overlap? (rest lon)))]))]
    (overlap? (shift-times sch0))))

;; -----------------------------

;; (listof TA) Slot Schedule -> (listof Schedule)
;; produce list of valid schedules filling up the slot
(check-expect (next-schedules (list SOBA) 1 empty) (list (list (make-assignment SOBA 1))))
(check-expect (next-schedules (list SOBA UDON) 1 (list (make-assignment UDON 3))) (list (list (make-assignment SOBA 1) (make-assignment UDON 3))))
(check-expect (next-schedules (list SOBA UDON) 4 (list (make-assignment UDON 3))) (list ))
(check-expect (next-schedules (list SOBA) 2 empty) (list ))

(define (next-schedules tas slot schedule)
  (local [(define (only-valid los)
            (filter valid? los))
          (define (valid? s)
            (and (no-overlap? s) (assigned-avail-only? s) (not-overworked? tas s)))]
    (only-valid (all-schedules tas slot schedule))))

;; -----------------------------

;; (listof TA) Slot Schedule -> (listof Schedule)
;; produce list of all possible schedules filling up the slot
(check-expect (all-schedules NOODLE-TAs 3 empty) (list (list (make-assignment SOBA 3)) (list (make-assignment UDON 3)) (list (make-assignment RAMEN 3))))
(check-expect (all-schedules NOODLE-TAs 2 (list (make-assignment SOBA 3)))
              (list  (list (make-assignment SOBA 2) (make-assignment SOBA 3))
                     (list (make-assignment UDON 2) (make-assignment SOBA 3))
                     (list (make-assignment RAMEN 2) (make-assignment SOBA 3))))


(define (all-schedules tas slot schedule)
  (cond [(empty? tas) empty]
        [else
         (cons (cons (make-assignment (first tas) slot) schedule)
               (all-schedules (rest tas) slot schedule))]))
