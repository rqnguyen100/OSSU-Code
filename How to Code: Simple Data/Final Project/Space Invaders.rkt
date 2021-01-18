;; Rubric:
;; Commit Ready: (G/F/P)
;;  - Good
;; Design Completeness: (G/F/P)
;;  - Good
;; Internal Quality: (E/G/F/P)
;;  - Excellent
;; Problem Satisfied: (G/F/P)
;;  - Fair
;;    - missiles don't hit invaders when pressing space consecutively
;;    - image of invader bouncing off borders is off

(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =========
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 15)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

;; ================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game ListOfInvader ListofMissiles Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below ListOfMissiles data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

;; -------------------------

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 0))   ;center staying still
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; -----------------------------

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; -------------------------

;; ListOfInvaders is one of:
;;  - empty
;;  - (list Invader ListOfInvaders)
;; interp. a list of invaders

(define LOI-1 empty)                             ;no invaders
(define LOI-2 (list (make-invader 100 100 10)))  ;one invader
(define LOI-3 (list (make-invader 100 100 10)    ;two invaders
                    (make-invader 300 150 -10)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]              ;Base Case
        [else
         (... (first loi)                 ;Invader
              (fn-for-loi (rest loi)))])) ;Recursive Case

;; --------------------------

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; -------------------------

;; ListOfMissiles is one of:
;;  - empty
;;  - (list Missle ListOfMissiles)
;; interp. a list of missiles

(define LOM-1 empty)                                      ;no missiles
(define LOM-2 (list (make-missile 100 100)))              ;one missile
(define LOM-3 (list (make-missile 100 100)                ;two missiles
                    (make-missile (invader-x I1)
                                  (+ (invader-y I1) 10))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]              ;Base Case
        [else
         (... (first lom)                 ;Missile
              (fn-for-lom (rest lom)))])) ;Recursive Case

;; -------------------------
;; Game constants
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty T0))
;; 
(define (main g)
  (big-bang g                              ; Game
            (on-tick   timer-game)         ; Game -> Game
            (to-draw   render-game)        ; Game -> Image
            (on-key    handle-key)         ; Game KeyEvent -> Game
            (stop-when game-over)))        ; Game -> Game

;; ------------
;; Game -> Game
;; spawns an invader randomly and produces the next invaders and missiles
(check-random (timer-game (make-game empty empty T0)) (make-game (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)) empty T0))       ;one invader is always spawned randommly (always fails)

(check-random (timer-game (make-game (list (make-invader 100 0 INVADER-X-SPEED)) empty T0))                                              ;one invader and no missiles
              (make-game (list (make-invader (+ 100 INVADER-X-SPEED) (+ 0 INVADER-Y-SPEED) INVADER-X-SPEED))
                                   
                         empty T0))
(check-random (timer-game (make-game (list (make-invader 75 5 INVADER-X-SPEED) (make-invader 25 150 INVADER-X-SPEED)) empty T1))         ;two invaders and no missiles
              (make-game (list (make-invader (+ 75 INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED) INVADER-X-SPEED)                                    
                               (make-invader (+ 25 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) INVADER-X-SPEED))
                               
                         empty (make-tank 51 1)))
(check-random (timer-game (make-game (list (make-invader 150 50 INVADER-X-SPEED)) (list (make-missile 150 300)) T1))                     ;one invader and one missile
              (make-game (list (make-invader (+ 150 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) INVADER-X-SPEED))
                               
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))                                                               
                         (make-tank 51 1)))
(check-random (timer-game (make-game (list (make-invader 200 75 INVADER-X-SPEED)) (list (make-missile 75 50) (make-missile 80 298)) T2)) ;one invader and two missiles
              (make-game (list (make-invader (+ 200 INVADER-X-SPEED) (+ 75 INVADER-Y-SPEED) INVADER-X-SPEED))
                               
                         (list (make-missile 75 (- 50 MISSILE-SPEED)) 
                               (make-missile 80 (- 298 MISSILE-SPEED)))
                         (make-tank 49 -1)))

(check-random (timer-game (make-game (list (make-invader 80 60 INVADER-X-SPEED)) empty T0))                                              ;invader moving to the right
              (make-game (list (make-invader (+ 80 INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) INVADER-X-SPEED))
                                   
                         empty T0))
(check-random (timer-game (make-game (list (make-invader (- WIDTH 1) 60 INVADER-X-SPEED)) empty T0))                                     ;invader stopping at right border
              (make-game (list (make-invader WIDTH (+ 60 INVADER-Y-SPEED) INVADER-X-SPEED))
                                   
                         empty T0))
(check-random (timer-game (make-game (list (make-invader WIDTH 60 INVADER-X-SPEED)) empty T0))                                           ;invader bouncing off right border
              (make-game (list (make-invader (+ WIDTH (- INVADER-X-SPEED)) (+ 60 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
                                   
                         empty T0))
(check-random (timer-game (make-game (list (make-invader 80 60 (- INVADER-X-SPEED))) empty T0))                                          ;invader moving to the left
              (make-game (list (make-invader (+ 80 (- INVADER-X-SPEED)) (+ 60 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
                                  
                         empty T0))
(check-random (timer-game (make-game (list (make-invader (+ 0 1) 60 (- INVADER-X-SPEED))) empty T0))                                     ;invader stopping at left border
              (make-game (list (make-invader 0 (+ 60 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
                                   
                         empty T0))
(check-random (timer-game (make-game (list (make-invader 0 60 (- INVADER-X-SPEED))) empty T0))                                           ;invader bouncing off left border
              (make-game (list (make-invader (+ 0 INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) INVADER-X-SPEED))
                                   
                         empty T0))

(check-random (timer-game (make-game (list (make-invader 150 250 INVADER-X-SPEED))                                                       ;invader gets hit
                                     (list (make-missile (+ 150 INVADER-X-SPEED) (+ 250 10))) T2))
              (make-game empty                                                               
                         (list (make-missile (+ 150 INVADER-X-SPEED) 250))
                         (make-tank 49 -1)))

;(define (timer-game g) (make-game empty empty T1)) ;stub

;;<Template from Game>

(define (timer-game g)
  (make-game (destroyed? (update-invaders (game-invaders g)) (update-missiles (game-missiles g)))
             (update-missiles (game-missiles g))
             (update-tank (game-tank g))))

;; -----------------------------------------------
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; if missile hits invader, invader is destoryed
(check-random (destroyed? empty empty) empty)                                        ;no invaders

(check-random (destroyed? (list (make-invader 150 250 INVADER-X-SPEED))              ;invader gets hit directly
                          (list (make-missile 150 245)))
              empty)                                                                 
(check-random (destroyed? (list (make-invader 150 250 INVADER-X-SPEED))              ;invader gets hit
                          (list (make-missile 145 250)))
              empty)
(check-random (destroyed? (list (make-invader 150 250 INVADER-X-SPEED))              ;invader doesn't get hit
                          empty)
              (list (make-invader 150 250 INVADER-X-SPEED)))
                    
;(define (destroyed? loi lom) empty) ;stub

#;
(define (destoryed? loi lom)
  (... loi)
  (... lom))

(define (destroyed? loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [(and (<= (abs (- (invader-x (first loi)) (missile-x (first lom)))) 15) (<= (abs (- (invader-y (first loi)) (missile-y (first lom)))) 16)) (append empty (destroyed? (rest loi) lom))]
        [else
         (append (list (first loi))
                 (destroyed? (rest loi) lom))]))

;; ------------------------------
;; ListOfInvaders -> ListOfInvaders
;; updates ListOfInvader
(check-random (update-invaders empty) empty)                                                                             ;no invaders
(check-random (update-invaders (list (make-invader 100 0 INVADER-X-SPEED)))                                              ;one invader
              (list (make-invader (+ 100 INVADER-X-SPEED) (+ 0 INVADER-Y-SPEED) INVADER-X-SPEED)
                    ))
(check-random (update-invaders (list (make-invader 75 5 INVADER-X-SPEED) (make-invader 25 150 INVADER-X-SPEED)))         ;two invaders
              (list (make-invader (+ 75 INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED) INVADER-X-SPEED)
                    (make-invader (+ 25 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) INVADER-X-SPEED)
                    ))

(check-random (update-invaders (list (make-invader 80 60 INVADER-X-SPEED)))                                              ;invader moving to the right
              (list (make-invader (+ 80 INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) INVADER-X-SPEED)
                    ))
(check-random (update-invaders (list (make-invader (- WIDTH 1) 60 INVADER-X-SPEED)))                                     ;invader stopping at right border
              (list (make-invader WIDTH (+ 60 INVADER-Y-SPEED) INVADER-X-SPEED)
                    ))
(check-random (update-invaders (list (make-invader WIDTH 60 INVADER-X-SPEED)))                                           ;invader bouncing off right border
              (list (make-invader (+ WIDTH (- INVADER-X-SPEED)) (+ 60 INVADER-Y-SPEED) (- INVADER-X-SPEED))
                    ))
(check-random (update-invaders (list (make-invader 80 60 (- INVADER-X-SPEED))))                                          ;invader moving to the left
              (list (make-invader (+ 80 (- INVADER-X-SPEED)) (+ 60 INVADER-Y-SPEED) (- INVADER-X-SPEED))
                    ))
(check-random (update-invaders (list (make-invader (+ 0 1) 60 (- INVADER-X-SPEED))))                                     ;invader stopping at left border
              (list (make-invader 0 (+ 60 INVADER-Y-SPEED) (- INVADER-X-SPEED))
                    ))
(check-random (update-invaders (list (make-invader 0 60 INVADER-X-SPEED)))                                               ;invader bouncing off left border
              (list (make-invader (+ 0 INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) INVADER-X-SPEED)
                    ))

;(define (update-invaders loi) empty) ;stub

;;<Template from ListOfInvaders>

(define (update-invaders loi)
  (cond [(empty? loi) (random-invader INVADER-X-SPEED)]
        [else
         (append (list (make-invader (new-invader-x (invader-x (first loi)) (invader-dx (first loi)))
                                     (new-invader-y (invader-y (first loi)))
                                     (change-velocity? (invader-x (first loi)) (invader-dx (first loi)))))
               (update-invaders (rest loi)))]))

;; -----------------
;; Number[INVADER-X-SPEED] -> ListOfInvaders
;; makes an invader a random x position with given speed
(check-random (random-invader INVADER-X-SPEED) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))) ;test fails, random invader is made

;(define (random-invader n) (make-invader 0 0 0)) ;stub

#;
(define (random-invader n)
  (... n))

(define (random-invader n)
  (if (<= (random 100) 2)
      (list (make-invader (random WIDTH) 0 n))
      empty))

;; ------------------
;; Natural[0,WIDTH] Number -> Natural
;; adds invader speed in the x direction to invader x position
(check-expect (new-invader-x 0 INVADER-X-SPEED) (+ 0 INVADER-X-SPEED))             ;positive velocity
(check-expect (new-invader-x 299 INVADER-X-SPEED) WIDTH)                           ;stops at right border
(check-expect (new-invader-x WIDTH INVADER-X-SPEED) (+ WIDTH (- INVADER-X-SPEED))) ;bounces off right border
(check-expect (new-invader-x 50 (- INVADER-X-SPEED)) (+ 50 (- INVADER-X-SPEED)))   ;negative velocity
(check-expect (new-invader-x 1 (- INVADER-X-SPEED)) 0)                             ;stops at left border
(check-expect (new-invader-x 0 (- INVADER-X-SPEED)) (+ 0 INVADER-X-SPEED))         ;bounces off left border

;(define (new-invader-x nat num) nat) ;stub

#;
(define (new-invader-x nat num)
  ((... nat)
   (... num)))

(define (new-invader-x nat num)
  (cond [(and (< 300 (+ nat num)) (not (= 300 nat))) 300]
        [(and (> 0 (+ nat num)) (not (= 0 nat))) 0]
        [else
         (+ nat (change-velocity? nat num))]))

;; ---------------
;; Natural[0,WIDTH] Number -> Number
;; returns a negative version of number if natural number is either 0 for 300 and is going to go past border
(check-expect (change-velocity? 0 (- INVADER-X-SPEED)) INVADER-X-SPEED)     ;left border
(check-expect (change-velocity? 70 INVADER-X-SPEED) INVADER-X-SPEED)        ;middle
(check-expect (change-velocity? WIDTH INVADER-X-SPEED) (- INVADER-X-SPEED)) ;left border
 
;(define (change-velocity? nat num) num) ;stub

#;
(define (change-velocity? nat num)
  ((... nat)
   (... num)))

(define (change-velocity? nat num)
  (cond [(and (= 300 nat) (< 300 (+ nat num))) (- num)]
        [(and (= 0 nat) (> 0 (+ nat num))) (- num)]
        [else
         num]))

;; ------------------
;; Natural[0,HEIGHT] -> Natural
;; adds invader speed in the y direction to invader y position
(check-expect (new-invader-y 0) (+ 0 INVADER-Y-SPEED))     ;top border
(check-expect (new-invader-y 100) (+ 100 INVADER-Y-SPEED)) ;middle
(check-expect (new-invader-y HEIGHT) HEIGHT)               ;bottom border

;(define (new-invader-y n) n) ;stub

#;
(define (new-invader-y n)
  (... n))

(define (new-invader-y n)
  (cond [(< HEIGHT (+ n INVADER-Y-SPEED)) HEIGHT]
        [else
         (+ n INVADER-Y-SPEED)]))

;; ------------------------------
;; ListOfMissiles -> ListOfMissiles
;; updates ListOfMissiles
(check-expect (update-missiles empty) empty)                                      ;empty list
(check-expect (update-missiles (list (make-missile 150 300)))                     ;one missle
              (list (make-missile 150 (- 300 MISSILE-SPEED))))                                                               
(check-expect (update-missiles (list (make-missile 75 50) (make-missile 80 298))) ;two missiles
              (list (make-missile 75 (- 50 MISSILE-SPEED)) 
                    (make-missile 80 (- 298 MISSILE-SPEED))))

;(define (update-missiles lom) empty) ;stub

;;<Template from ListOfMissiles>

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (append (outside? (make-missile (missile-x (first lom)) (new-missile-y (missile-y (first lom)))))
                 (update-missiles (rest lom)))]))

;; ------------------
;; Natural -> Natural
;; adds missile speed in the y direction to missile y position
(check-expect (new-missile-y 300) (- 300 MISSILE-SPEED)) ;bottom
(check-expect (new-missile-y 150) (- 150 MISSILE-SPEED)) ;middle
(check-expect (new-missile-y   0) (- 0 MISSILE-SPEED))   ;top

;(define (new-missile-y n) n) ;stub

#;
(define (new-missile-y n)
  (... n))

(define (new-missile-y n)
  (- n MISSILE-SPEED))

;; --------------------------------
;; Missile -> ListOfMissiles
;; if missile is outside, remove missile       
(check-expect (outside? (make-missile 150 300)) (list (make-missile 150 300))) ;not outside
(check-expect (outside? (make-missile 150 -1)) empty)                          ;outside

;(define (outside? lom) empty) ;stub

;;<Template from Missile>

(define (outside? m)
  (if (< (missile-y m) 0)
      empty
      (list m)))

;; ------------
;; Tank -> Tank
;; adds velocity of tank to x-position of tank
(check-expect (update-tank (make-tank 50 1)) (make-tank 51 1))       ;move to right
(check-expect (update-tank (make-tank 100 -1)) (make-tank 99 -1))    ;move to left

(check-expect (update-tank (make-tank (/ (image-width TANK) 2) -1)) (make-tank (/ (image-width TANK) 2) 0))                ;stops tank from going past left border
(check-expect (update-tank (make-tank (- 300 (/ (image-width TANK) 2)) 1)) (make-tank (- 300 (/ (image-width TANK) 2)) 0)) ;stops tank from going past right border

;(define (update-tank t) t) ;stub

;;<Template from Tank>

(define (update-tank t)
  (cond [(< (+ (tank-x t) (tank-dir t)) (/ (image-width TANK) 2)) (make-tank (/ (image-width TANK) 2) 0)]
        [(> (+ (tank-x t) (tank-dir t)) (- 300 (/ (image-width TANK) 2))) (make-tank (- 300 (/ (image-width TANK) 2)) 0)]
        [else
         (make-tank (+ (tank-x t) (tank-dir t)) (tank-dir t))]))

;; -------------
;; Game -> Image
;; renders image of tank, missiles, invaders, and background
(check-expect (render-game (make-game empty empty T0)) (place-images (list TANK)                                                         ;renders tank
                                                                     (list (make-posn (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2))))
                                                                     BACKGROUND))
(check-expect (render-game (make-game (list (make-invader 100 0 INVADER-X-SPEED))                                                        ;renders one invader and one tank
                                      empty 
                                      T0))
              (place-images
                (list INVADER
                      TANK)
                (list (make-posn 100 0)
                      (make-posn (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2))))
              BACKGROUND))
(check-expect (render-game (make-game (list (make-invader 100 0 INVADER-X-SPEED))                                                        ;renders one invader, one missile, and one tank
                                      (list (make-missile 150 (- 300 MISSILE-SPEED)))
                                      T0))
              (place-images 
                (list INVADER
                      MISSILE
                      TANK)
                (list (make-posn 100 0)
                      (make-posn 150 (- 300 MISSILE-SPEED))
                      (make-posn (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2))))
              BACKGROUND))
(check-expect (render-game (make-game (list (make-invader 75 5 INVADER-X-SPEED) (make-invader 25 150 INVADER-X-SPEED))                   ;renders two invaders, two missiles, and one tank
                                      (list (make-missile 150 (- 300 MISSILE-SPEED)) (make-missile 75 (- 50 MISSILE-SPEED)))
                                      T0))
              (place-images
               (list INVADER
                     INVADER
                     MISSILE
                     MISSILE
                     TANK)
               (list (make-posn 75 5)
                     (make-posn 25 150)
                     (make-posn 150 (- 300 MISSILE-SPEED))
                     (make-posn 75 (- 50 MISSILE-SPEED))
                     (make-posn (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2))))
               BACKGROUND))

;(define (render-game g) BACKGROUND) ;stub

;;<Template from Game>

(define (render-game g)
  (place-images
   (append (num-of-invaders (game-invaders g))
           (num-of-missiles (game-missiles g))
           (list TANK))
   (append (pos-of-invaders (game-invaders g))
           (pos-of-missiles (game-missiles g))
           (pos-of-tank     (game-tank g)))
   BACKGROUND))

;; ------------------------------
;; ListOfInvaders -> ListOfImages
;; takes a list of invaders and outputs the image for every invader
(check-expect (num-of-invaders empty) empty)                                               ;0 invaders
(check-expect (num-of-invaders (list (make-invader 75 5 INVADER-X-SPEED))) (list INVADER)) ;1 invader
(check-expect (num-of-invaders (list (make-invader 75 5 INVADER-X-SPEED)                   ;2 invaders
                                     (make-invader 150 10 INVADER-X-SPEED)))
              (list INVADER INVADER))

;(define (num-of-invaders loi) empty)

;;<Template from ListOfInvaders>

(define (num-of-invaders loi)
  (cond [(empty? loi) empty]     
        [else
         (append (list INVADER)          
                 (num-of-invaders (rest loi)))])) 

;; ------------------------------
;; ListOfMissiles -> ListOfImages
;; takes a list of missiles and outputs the image for every missile
(check-expect (num-of-missiles empty) empty)                               ;0 missiles
(check-expect (num-of-missiles (list (make-missile 75 5))) (list MISSILE)) ;1 missile
(check-expect (num-of-missiles (list (make-missile 75 5)                   ;2 missiles
                                     (make-missile 150 10)))
              (list MISSILE MISSILE))

;(define (num-of-missiles lom) empty)

;;<Template from ListOfInvaders>

(define (num-of-missiles lom)
  (cond [(empty? lom) empty]     
        [else
         (append (list MISSILE)          
                 (num-of-missiles (rest lom)))])) 

;; ------------------------------
;; ListOfInvaders -> ListOfPositions
;; takes a list of invaders and outputs the position for every invader
(check-expect (pos-of-invaders empty) empty)                                                        ;0 invaders
(check-expect (pos-of-invaders (list (make-invader 75 5 INVADER-X-SPEED))) (list (make-posn 75 5))) ;1 invader
(check-expect (pos-of-invaders (list (make-invader 75 5 INVADER-X-SPEED)                            ;2 invaders
                                     (make-invader 150 10 INVADER-X-SPEED)))
              (list (make-posn 75 5)
                    (make-posn 150 10)))

;(define (pos-of-invaders loi) empty)

;;<Template from ListOfInvaders>>

(define (pos-of-invaders loi)
  (cond [(empty? loi) empty]     
        [else
         (append (list (make-posn (invader-x (first loi)) (invader-y (first loi))))          
                 (pos-of-invaders (rest loi)))])) 

;; ------------------------------
;; ListOfMissiles -> ListOfPositions
;; takes a list of missiles and outputs the position for every missile
;; !!!
(check-expect (pos-of-missiles empty) empty)                                        ;0 missiles
(check-expect (pos-of-missiles (list (make-missile 75 5))) (list (make-posn 75 5))) ;1 missiles
(check-expect (pos-of-missiles (list (make-missile 75 5)                            ;2 missile
                                     (make-missile 150 10)))
              (list (make-posn 75 5)
                    (make-posn 150 10)))

;(define (pos-of-missiles lom) empty)

;;<Template from ListOfMissiles>

(define (pos-of-missiles lom)
  (cond [(empty? lom) empty]     
        [else
         (append (list (make-posn (missile-x (first lom)) (missile-y (first lom))))          
                 (pos-of-missiles (rest lom)))])) 

;; ------------------------------
;; Tank -> ListOfPositions
;; takes a tank and outputs the position of tank
(check-expect (pos-of-tank (make-tank (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2)))) (list (make-posn (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2))))) ;1 tank

;(define (pos-of-tank t) empty)

;;<Template from Tank>>

(define (pos-of-tank t)
  (list (make-posn (tank-x t) (- HEIGHT (/ (image-height TANK) 2)))))

;; ---------------------
;; Game KeyEvent -> Game
;; when space is pressed, shoot missile
;; when left or right arrow keys are pressed, move tank accordingly
(check-expect (handle-key (make-game empty empty T0) " ") (make-game empty (list (make-missile (/ WIDTH 2) 480)) T0)) ;space is pressed

(check-expect (handle-key (make-game empty empty T1) "left") (make-game empty empty (make-tank 50 -1))) ;left is pressed

(check-expect (handle-key (make-game empty empty T2) "right") (make-game empty empty (make-tank 50 1))) ;right is pressed

(check-expect (handle-key (make-game empty empty T0) "up") (make-game empty empty T0)) ;space,left, or right is not pressed

;(define (handle-key g key) (make-game empty empty T1)) ;stub

#;
(define (handle-key ws ke)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))

(define (handle-key g key)
  (cond [(key=? key " ") (make-game (game-invaders g) (append (list (make-missile (tank-x (game-tank g)) 480)) (game-missiles g)) (game-tank g))]
        [(key=? key "left")  (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? key "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [else 
         g]))

;; -------------------
;; Game -> Boolean
;; stops game and displays game over once invader reaches bottom
(check-expect (game-over (make-game empty empty T0)) false) ;no invader
(check-expect (game-over (make-game (list (make-invader 75 5 INVADER-X-SPEED)) empty T0)) false) ;invader not at bottom
(check-expect (game-over (make-game (list (make-invader 0 HEIGHT INVADER-X-SPEED)) empty T0)) true)  ;invader at bottom

;(define (game-over g) false) ;stub

;;<Template from ListOfInvaders>>

(define (game-over g)
  (cond [(empty? (game-invaders g)) false]
        [(= (invader-y (first (game-invaders g))) HEIGHT) true]
        [else
         (game-over (make-game (rest (game-invaders g)) (game-missiles g) (game-tank g)))]))
