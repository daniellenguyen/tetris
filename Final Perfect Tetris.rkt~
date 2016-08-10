;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Final Perfect Tetris|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;; Tetris

(require 2htdp/image)
(require 2htdp/universe)

;;; Data Definitions:

(define GRID-SIZE 30) ; This is the width of a game-board square.
(define BOARD-HEIGHT 20) ; This is the height of a game-board in grid squares.
(define BOARD-WIDTH 10) ; This is the width of a game-board in grid squares. 
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT)) ; This is the height of the board in pixels
(define BOARD-WIDTH-PIXELS (* GRID-SIZE BOARD-WIDTH)) ; This is the width of the board in pixels
(define BACKGROUND (empty-scene BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS))

;;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;;; A Tetra is a (make-tetra Posn BSet)
;;; The center point is the point around which the tetra rotates when it spins.
(define-struct tetra (center blocks))

;;; A Set of Blocks (BSet) is one of:
;;; - empty
;;; - (cons Block BSet)
;;; Order does not matter. Repetitions are NOT allowed. 

;;; A World is a (make-world Tetra BSet)
;;; pile is a BSet that represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;; These functions make Tetras:

;;; tetraO: Number Number -> Tetra
;;; Makes an "O" Tetra.
(define (tetraO x y)
  (make-tetra (make-posn (+ 0.5 x) (+ 0.5 y))
              (list (make-block x y "LightGreen")
                    (make-block (+ 1 x) y "LightGreen")
                    (make-block x (+ 1 y) "LightGreen")
                    (make-block (+ 1 x) (+ 1 y) "LightGreen"))))

(check-expect (tetraO 1 1) 
              (make-tetra (make-posn 1.5 1.5)
                          (list (make-block 1 1 "LightGreen")
                                (make-block 2 1 "LightGreen")
                                (make-block 1 2 "LightGreen")
                                (make-block 2 2 "LightGreen"))))

;;; tetraI: Number Number -> Tetra
;;; Makes an "I" Tetra.
(define (tetraI x y)
  (make-tetra (make-posn (+ 1 x) y)
              (list (make-block x y "SteelBlue")
                    (make-block (+ 1 x) y "SteelBlue")
                    (make-block (+ 2 x) y "SteelBlue")
                    (make-block (+ 3 x) y "SteelBlue"))))

(check-expect (tetraI 1 1)
              (make-tetra (make-posn 2 1)
                          (list (make-block 1 1 "SteelBlue")
                                (make-block 2 1 "SteelBlue")
                                (make-block 3 1 "SteelBlue")
                                (make-block 4 1 "SteelBlue"))))

;;; tetraL: Number Number -> Tetra
;;; Makes an "L" Tetra.
(define (tetraL x y)
  (make-tetra (make-posn (+ 1 x) y)
              (list (make-block x y "MediumOrchid")
                    (make-block (+ 1 x) y "MediumOrchid")
                    (make-block (+ 2 x) y "MediumOrchid")
                    (make-block (+ 2 x) (+ 1 y) "MediumOrchid"))))

(check-expect (tetraL 1 1)
              (make-tetra (make-posn 2 1)
                          (list (make-block 1 1 "MediumOrchid")
                                (make-block 2 1 "MediumOrchid")
                                (make-block 3 1 "MediumOrchid")
                                (make-block 3 2 "MediumOrchid"))))

;;; tetraJ: Number Number -> Tetra
;;; Makes a "J" Tetra.
(define (tetraJ x y)
  (make-tetra (make-posn (+ 1 x) y)
              (list (make-block x y "SkyBlue")
                    (make-block (+ 1 x) y "SkyBlue")
                    (make-block (+ 2 x) y "SkyBlue")
                    (make-block x (+ 1 y) "SkyBlue"))))

(check-expect (tetraJ 1 1)
              (make-tetra (make-posn 2 1)
                          (list (make-block 1 1 "SkyBlue")
                                (make-block 2 1 "SkyBlue")
                                (make-block 3 1 "SkyBlue")
                                (make-block 1 2 "SkyBlue"))))

;;; tetraT: Number Number -> Tetra
;;; Makes a "T" Tetra.
(define (tetraT x y)
  (make-tetra (make-posn (+ 1 x) y)
              (list (make-block x y "DarkOrange")
                    (make-block (+ 1 x) y "DarkOrange")
                    (make-block (+ 2 x) y "DarkOrange")
                    (make-block (+ 1 x) (+ 1 y) "DarkOrange"))))

(check-expect (tetraT 1 1) 
              (make-tetra (make-posn 2 1)
                          (list (make-block 1 1 "DarkOrange")
                                (make-block 2 1 "DarkOrange")
                                (make-block 3 1 "DarkOrange")
                                (make-block 2 2 "DarkOrange"))))

;;; tetraZ: Number Number -> Tetra
;;; Makes a "Z" Tetra.
(define (tetraZ x y)
  (make-tetra (make-posn (+ 1 x) y)
              (list (make-block x (+ 1 y) "pink")
                    (make-block (+ 1 x) y "pink")
                    (make-block (+ 2 x) y "pink")
                    (make-block (+ 1 x) (+ 1 y) "pink"))))

(check-expect (tetraZ 1 1)
              (make-tetra (make-posn 2 1)
                          (list (make-block 1 2 "pink")
                                (make-block 2 1 "pink")
                                (make-block 3 1 "pink")
                                (make-block 2 2 "pink"))))

;;; tetraS: Number Number -> Tetra
;;; Makes an "S" Tetra.
(define (tetraS x y)
  (make-tetra (make-posn (+ 1 x) y)
              (list (make-block x y "Firebrick")
                    (make-block (+ 1 x) y "Firebrick")
                    (make-block (+ 1 x) (+ 1 y) "Firebrick")
                    (make-block (+ 2 x) (+ 1 y) "Firebrick"))))

(check-expect (tetraS 1 1)
              (make-tetra (make-posn 2 1)
                          (list (make-block 1 1 "Firebrick")
                                (make-block 2 1 "Firebrick")
                                (make-block 2 2 "Firebrick")
                                (make-block 3 2 "Firebrick"))))

;;; This is example data used later in check-expects:
(define pile1 (list (make-block 5 1 "green")
                    (make-block 5 2 "green")
                    (make-block 6 1 "green")
                    (make-block 6 2 "green")))
(define pile2 (list (make-block 2 5 "red")
                    (make-block 2 4 "red")
                    (make-block 3 4 "red")
                    (make-block 3 3 "red")
                    (make-block 10 7 "orange")
                    (make-block 9 7 "orange")
                    (make-block 8 7 "orange")
                    (make-block 9 6 "orange")
                    (make-block 9 5 "purple")
                    (make-block 8 5 "purple")
                    (make-block 7 5 "purple")
                    (make-block 7 4 "purple")))
(define BSet1 (list (make-block 1 1 "red")
                    (make-block 2 1 "red")
                    (make-block 2 2 "red")
                    (make-block 3 2 "red")))
(define BSet2 (list (make-block 3 4 "red")
                    (make-block 3 5 "red")
                    (make-block 2 5 "red")
                    (make-block 2 4 "red")))
(define BSet3 (list (make-block 1 1 "red")
                    (make-block 2 1 "red")
                    (make-block 3 1 "red")
                    (make-block 4 1 "red")
                    (make-block 5 1 "red")
                    (make-block 6 1 "red")
                    (make-block 7 1 "red")
                    (make-block 8 1 "red")
                    (make-block 9 1 "red")
                    (make-block 10 1 "red")))
(define BSet4 (list (make-block 1 2 "red")
                    (make-block 2 2 "red")
                    (make-block 3 2 "red")
                    (make-block 4 2 "red")
                    (make-block 5 2 "red")
                    (make-block 6 2 "red")
                    (make-block 7 2 "red")
                    (make-block 8 2 "red")
                    (make-block 9 2 "red")
                    (make-block 10 2 "red")
                    (make-block 20 20 "red")
                    (make-block 2 3 "orange")))

(define tetra1 (tetraO 5 9))
(define tetra2 (tetraL 4 2))
(define tetra3 (tetraZ 5 1))
(define tetra4 (tetraT 3 2))

(define block1 (make-block 5 2 "red"))
(define block2 (make-block 9 4 "green"))

(define w1 (make-world tetra1 pile1))
(define w2 (make-world tetra2 pile2))
(define w3 (make-world tetra3 pile2))

;;; These are functions that make the game:

;;; move-tetra : Tetra Number -> Tetra
;;; Moves the tetra in a given direction.
(define (move-tetra tetra x-dir y-dir)
  (local (; move-center: Posn Number Number -> Posn
          ; Moves center of tetra in a given direction.
          (define (move-center center x-dir y-dir)
            (make-posn (+ x-dir (posn-x center)) (+ y-dir (posn-y center))))
          ; block-move : BSet Number Number-> Bset
          ; Moves the blocks of a tetra in the given x or y direction.
          (define (block-move bset x-dir y-dir)
            (map (λ (block) (make-block (+ x-dir (block-x block))
                                        (+ y-dir (block-y block))
                                        (block-color block))) bset)))
    (make-tetra (move-center (tetra-center tetra) x-dir y-dir)
                (block-move (tetra-blocks tetra) x-dir y-dir)))) 

(check-expect (move-tetra (make-tetra (make-posn 2 1)
                                      (list (make-block 1 1 "SteelBlue")
                                            (make-block 2 1 "SteelBlue")
                                            (make-block 3 1 "SteelBlue")
                                            (make-block 4 1 "SteelBlue"))) 1 1) 
              (make-tetra (make-posn 3 2)
                          (list (make-block 2 2 "SteelBlue")
                                (make-block 3 2 "SteelBlue")
                                (make-block 4 2 "SteelBlue")
                                (make-block 5 2 "SteelBlue"))))

;;; collision? : Tetra Bset -> Boolean 
;;; Determines if a tetra has collided with the grid's bottom or another tetra.
(define (collision? t bset)
  (local (; block-collision? : Block BSet -> Boolean
          ; Determines if a block has collided with a block in the pile. 
          (define (block-collision? b p)
            (ormap (λ (block) (and (= (block-x b) (block-x block))
                                   (= (block-y b) (+ 1 (block-y block))))) p))
          ; block-collides-bottom? : BSet -> Boolean
          ; Determines if a block has collided with the bottom of the grid.
          (define (block-collides-bottom? b)
            (ormap (λ (b) (<= (block-y b) 1)) b)))
    (if (or (not (andmap false? (map (λ (x) (block-collision? x bset)) (tetra-blocks t))))
            (block-collides-bottom? (tetra-blocks t))) true false))) 

(check-expect (collision? (make-tetra (make-posn 1.5 1.5) empty) empty) false)
(check-expect (collision? (make-tetra (make-posn 1.5 1.5)
                                      (list (make-block 1 4 "LightGreen")
                                            (make-block 2 1 "LightGreen")
                                            (make-block 1 2 "LightGreen")
                                            (make-block 2 2 "LightGreen")))
                          (list (make-block 2 4 "red")
                                (make-block 1 0 "red")
                                (make-block 3 1 "red"))) true)
(check-expect (collision? (make-tetra (make-posn 2 1)
                                      (list (make-block 1 4 "LightGreen")
                                            (make-block 2 4 "LightGreen")
                                            (make-block 4 2 "LightGreen")
                                            (make-block 5 2 "LightGreen")))
                          (list (make-block 2 3 "red")
                                (make-block 5 0 "red")
                                (make-block 3 1 "red"))) true)

;;; adjust-blocks: Bset Number -> Bset 
;;; Removes a full row and lowers the blocks above it one row.
(define (adjust-blocks pile complete-row)
  (local (; remove-row : Bset Number -> Bset
          ; Removes row at given y value.
          (define (remove-row pile y)
            (filter (λ (b) (not (= (block-y b) y))) pile))
          ; lower-blocks: BSet Number -> Bset
          ; Lowers a list of blocks a given row number. 
          (define (lower-blocks bset y)
            (map (λ (block) (make-block (block-x block) (sub1 (block-y block))
                                        (block-color block))) bset)))
    (lower-blocks (remove-row pile complete-row) complete-row)))

(check-expect (adjust-blocks empty 3) empty)
(check-expect (adjust-blocks BSet1 1) (list (make-block 2 1 "red")
                                            (make-block 3 1 "red")))
(check-expect (adjust-blocks (list (make-block 2 4 "red")
                                   (make-block 1 0 "red")
                                   (make-block 3 1 "red")) 1)(list (make-block 2 3 "red")
                                                                   (make-block 1 -1 "red")))

;;; full-row: LON -> Number 
;;; Determines if a row is full, and if so, which row. 
(define (full-row x ylist)
  (local (; count-number : Number LoN -> Number
          ; Determines how many times a number occurs in a list.
          (define (count-number x ylist)
            (foldr + 0 (map (λ (number) (if (= x number) 1 0)) ylist)))) 
    (cond [(or (empty? ylist) (= x 20)) 0]
          [(>= (count-number x ylist) 10) x]
          [else (full-row (add1 x) ylist)])))

(check-expect (full-row 2 (list 1 2 2 3 5 4 4 4 4 6 7 4 4 4 2 3 4 4 9 4)) 4)
(check-expect (full-row 0 empty) 0)

;;; check-pile: BSet -> BSet 
;;; Checks for full rows and removes them. 
(define (check-pile pile)
  (local (; BSet->ylist: BSet -> LoN 
          ; Makes a list of the y values of a given BSet. 
          (define (BSet->ylist pile)
            (map (λ (block) (block-y block)) pile)))
    (cond [(> (full-row 1 (BSet->ylist pile)) 0)
           (check-pile (adjust-blocks pile (full-row 1 (BSet->ylist pile))))]
          [else pile])))

(check-expect (check-pile empty) empty)
(check-expect (check-pile BSet4) (list (make-block 20 19 "red")
                                       (make-block 2 2 "orange")))

;;; new-tetra : Number -> Tetra
;;; Produces a random tetra at the top of the screen.
(define (new-tetra n)
  (cond [(= n 0) (tetraO 5 21)]
        [(= n 1) (tetraI 5 21)]
        [(= n 2) (tetraL 5 21)]
        [(= n 3) (tetraJ 5 21)]
        [(= n 4) (tetraT 5 21)]
        [(= n 5) (tetraZ 5 21)]
        [(= n 6) (tetraS 5 21)]))

(check-expect (new-tetra 0) (tetraO 5 21))
(check-expect (new-tetra 1) (tetraI 5 21))
(check-expect (new-tetra 2) (tetraL 5 21))
(check-expect (new-tetra 3) (tetraJ 5 21))
(check-expect (new-tetra 4) (tetraT 5 21))
(check-expect (new-tetra 5) (tetraZ 5 21))
(check-expect (new-tetra 6) (tetraS 5 21))

;;; next-tetra-world : World -> World 
;;; Adds a new tetra and adds the old tetra to the pile at the bottom.
(define (next-tetra-world w)
  (make-world (new-tetra (random 7))
              (check-pile (append (tetra-blocks (world-tetra w))
                                  (world-pile w)))))

#|(check-random (next-tetra-world (make-world 
                                (new-tetra (random 7))
                                 (list (make-block 2 3 "red")
                                       (make-block 1 0 "red")
                                       (make-block 3 1 "red"))))
              (make-world (new-tetra (random 7))
                          (list (tetra-blocks (new-tetra (random 7)))
                                (make-block 2 3 "red")
                                (make-block 1 0 "red")
                                (make-block 3 1 "red"))))|#

;;; world->image : World -> Image
;;; Makes an image out of a given world. 
(define (world->image w)
  (local (; blocks+scene : Bset Image -> Image
          ; Places a list of blocks onto a scene.
          (define (blocks+scene bset scene)
            (local (; image->grid : Image Number Number Image -> Image
                    ; Places a shape at the coordinates in an empty-scene. 
                    (define (image->grid image x y scene)
                      (place-image image (- (* GRID-SIZE x) (/ GRID-SIZE 2 ))
                                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE y)) 
                                      (/ GRID-SIZE 2)) scene)))
              (cond [(empty? bset) scene]
                    [else (image->grid (overlay (square (- GRID-SIZE 1) "solid" 
                                                        (block-color (first bset)))
                                                (square GRID-SIZE "solid" "black"))
                                       (block-x (first bset))
                                       (block-y (first bset))
                                       (blocks+scene (rest bset) scene))])))) 
    (blocks+scene (tetra-blocks (world-tetra w))
                  (blocks+scene (world-pile w) BACKGROUND))))

(check-expect 
 (world->image (make-world (make-tetra (make-posn 2 1)
                                       (list (make-block 1 1 "MediumOrchid")
                                             (make-block 2 1 "MediumOrchid")
                                             (make-block 3 1 "MediumOrchid")
                                             (make-block 3 2 "MediumOrchid")))
                           (list (make-block 2 4 "red")
                                 (make-block 1 0 "red")
                                 (make-block 3 1 "red"))))
 (place-images 
  (list (overlay (square 29 "solid" "MediumOrchid") 
                 (square 30 "solid" "black"))
        (overlay (square 29 "solid" "MediumOrchid")
                 (square 30 "solid" "black"))
        (overlay (square 29 "solid" "MediumOrchid")
                 (square 30 "solid" "black"))
        (overlay (square 29 "solid" "MediumOrchid")
                 (square 30 "solid" "black"))
        (overlay (square 29 "solid" "red")
                 (square 30 "solid" "black"))
        (overlay (square 29 "solid" "red")
                 (square 30 "solid" "black"))
        (overlay (square 29 "solid" "red")
                 (square 30 "solid" "black")))
  (list (make-posn (- (* GRID-SIZE 1) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 1)) (/ GRID-SIZE 2)))
        (make-posn (- (* GRID-SIZE 2) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 1)) (/ GRID-SIZE 2)))
        (make-posn (- (* GRID-SIZE 3) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 1)) (/ GRID-SIZE 2)))
        (make-posn (- (* GRID-SIZE 3) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 2)) (/ GRID-SIZE 2)))
        (make-posn (- (* GRID-SIZE 2) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 4)) (/ GRID-SIZE 2)))
        (make-posn (- (* GRID-SIZE 1) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 0)) (/ GRID-SIZE 2)))
        (make-posn (- (* GRID-SIZE 3) (/ GRID-SIZE 2 ))
                   (+ (- BOARD-HEIGHT-PIXELS (* GRID-SIZE 1)) (/ GRID-SIZE 2)))) 
  BACKGROUND))

;;; block-collision-dir?: Block BSet Operator -> Boolean
;;; Checks if a block is next to a wall or a pile.
(define (block-collision-dir? b p op)
  (ormap (λ (pile-block) (if (and (= (op (block-x b) 1) (block-x pile-block))
                                  (= (block-y b) (block-y pile-block))) true false)) p))

(check-expect (block-collision-dir? block1 pile1 -) false)
(check-expect (block-collision-dir? block1 BSet4 -) true)
(check-expect (block-collision-dir? block1 BSet4 +) true)
(check-expect (block-collision-dir? block1 empty +) false)

;;; collide-right?: BSet BSet -> Boolean
;;; Are the blocks within the x boundaries?
(define (collide-right? blocks pile)
  (ormap (λ (block) (if (or (block-collision-dir? block pile +)
                            (>= (block-x block) 10)) true false)) blocks))

(check-expect (collide-right? BSet1 empty) false)
(check-expect (collide-right? BSet3 BSet1) true)

;;; collide-left?: BSet BSet -> Boolean
;;; Are the blocks within the x boundaries?
(define (collide-left? blocks pile)
  (ormap (λ (block) (if (or (block-collision-dir? block pile -)
                            (<= (block-x block) 1)) true false)) blocks))

(check-expect (collide-left? empty pile1) false)
(check-expect (collide-left? BSet1 empty) true)
(check-expect (collide-left? BSet2 pile1) false)

;;; block-rotate-ccw : Posn Block -> Block
;;; Rotates the block 90 degrees counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c) (- (posn-y c) (block-y b)))
              (+ (posn-y c) (- (block-x b) (posn-x c))) (block-color b)))

(check-expect (block-rotate-ccw (make-posn 4 2) block1) (make-block 4 3 "red"))

;;; rotate-ccw: World -> Tetra
;;; Makes a Tetra rotated 90 degrees counterclockwise. 
(define (rotate-ccw w)
  (local ((define (BSet-rotate-ccw c block-set)
            (map (λ (block) (block-rotate-ccw c block)) block-set)))
    (make-tetra (tetra-center (world-tetra w))
                (BSet-rotate-ccw (tetra-center (world-tetra w))
                                 (tetra-blocks (world-tetra w))))))

(check-expect (rotate-ccw w1) (make-tetra (make-posn 5.5 9.5)
                                          (list (make-block 6 9 "LightGreen")
                                                (make-block 6 10 "LightGreen")
                                                (make-block 5 9 "LightGreen")
                                                (make-block 5 10 "LightGreen"))))

;;; rotate-cw: World -> Tetra
;;; Makes a Tetra rotated 90 degrees clockwise.
(define (rotate-cw w)
  (local (; tetra-rotate-cw : Posn BSet -> BSet
          ; Rotates the given block set 90 degrees around the given center point.
          (define (tetra-rotate-cw c block-set)
            (local (; block-rotate-cw : Posn Block -> Block
                    ; Rotates the given block 90 degrees around the given center point. 
                    (define (block-rotate-cw c b)
                      (block-rotate-ccw c
                                        (block-rotate-ccw c
                                                          (make-block
                                                           (+ (posn-x c) (- (posn-y c) (block-y b)))
                                                           (+ (posn-y c) (- (block-x b) (posn-x c)))
                                                           (block-color b))))))
              (map (λ (block-set) (block-rotate-cw c block-set)) block-set))))
    (make-tetra (tetra-center (world-tetra w))
                (tetra-rotate-cw (tetra-center (world-tetra w))
                                 (tetra-blocks (world-tetra w))))))

(check-expect (rotate-cw w1) (make-tetra (make-posn 5.5 9.5)
                                         (list (make-block 5 10 "LightGreen")
                                               (make-block 5 9 "LightGreen")
                                               (make-block 6 10 "LightGreen")
                                               (make-block 6 9 "LightGreen"))))

;;; world-move: Number World -> World
;;; Returns a world with a moved tetra.
(define (world-move x-dir w)
  (make-world (move-tetra (world-tetra w) x-dir 0) (world-pile w)))

(check-expect (world-move 1 w1)
              (make-world
               (make-tetra (make-posn 6.5 9.5) (list
                                                (make-block 6 9 "LightGreen")
                                                (make-block 7 9 "LightGreen")
                                                (make-block 6 10 "LightGreen")
                                                (make-block 7 10 "LightGreen")))
               (list (make-block 5 1 "green")
                     (make-block 5 2 "green") 
                     (make-block 6 1 "green")
                     (make-block 6 2 "green"))))

;;; key-press: Key-event World -> World
;;; Produces a new world based on the given key-event.
;;; - left arrow: Shift the current piece left.
;;; - right arrow: Shift the current piece right.
;;; - s: Rotate the current piece 90 degrees clockwise.
;;; - a: Rotate the current piece 90 degrees counterclockwise.
(define (handle-key w key-event)
  (local [(define (world-collide-left? w)
            (collide-left? (tetra-blocks (world-tetra w))
                           (world-pile w)))
          (define (world-collide-right? w)
            (collide-right? (tetra-blocks (world-tetra w))
                            (world-pile w)))]
    (cond [(and (key=? key-event "right")
                (not (world-collide-right? w))) (world-move 1 w)]
          [(and (key=? key-event "left")
                (not (world-collide-left? w))) (world-move -1 w)]
          [(and (key=? key-event "a")
                (not (world-collide-left? w))
                (not (world-collide-right? w)))
           (make-world (rotate-ccw w) (world-pile w))]
          [(and (key=? key-event "s")
                (not (world-collide-left? w))
                (not (world-collide-right? w)))
           (make-world (rotate-cw w) (world-pile w))]
          [else w])))

(check-expect (handle-key w1 "h") w1)
(check-expect (handle-key w1 "left")
              (make-world
               (make-tetra
                (make-posn 4.5 9.5)
                (list (make-block 4 9 "LightGreen")
                      (make-block 5 9 "LightGreen")
                      (make-block 4 10 "LightGreen")
                      (make-block 5 10 "LightGreen")))
               (list (make-block 5 1 "green")
                     (make-block 5 2 "green")
                     (make-block 6 1 "green")
                     (make-block 6 2 "green"))))
(check-expect (handle-key w1 "right")
              (make-world
               (make-tetra
                (make-posn 6.5 9.5)
                (list (make-block 6 9 "LightGreen")
                      (make-block 7 9 "LightGreen")
                      (make-block 6 10 "LightGreen")
                      (make-block 7 10 "LightGreen")))
               (list (make-block 5 1 "green")
                     (make-block 5 2 "green")
                     (make-block 6 1 "green")
                     (make-block 6 2 "green"))))
(check-expect (handle-key w1 "a")
              (make-world
               (make-tetra
                (make-posn 5.5 9.5)
                (list (make-block 6 9 "LightGreen")
                      (make-block 6 10 "LightGreen")
                      (make-block 5 9 "LightGreen")
                      (make-block 5 10 "LightGreen")))
               (list (make-block 5 1 "green")
                     (make-block 5 2 "green")
                     (make-block 6 1 "green")
                     (make-block 6 2 "green"))))
(check-expect (handle-key w1 "s")
              (make-world
               (make-tetra
                (make-posn 5.5 9.5)
                (list (make-block 5 10 "LightGreen")
                      (make-block 5 9 "LightGreen")
                      (make-block 6 10 "LightGreen")
                      (make-block 6 9 "LightGreen")))
               (list (make-block 5 1 "green")
                     (make-block 5 2 "green")
                     (make-block 6 1 "green")
                     (make-block 6 2 "green"))))

;;; game-over? : World -> Boolean
;;; Ends the game when the player loses.
(define (game-over? w)
  (ormap (λ (x) (if (= (block-y x) BOARD-HEIGHT) true false)) (world-pile w)))

(check-expect (game-over? (make-world tetra1 empty)) false)
(check-expect (game-over? (make-world tetra1 
                                      (list (make-block 3 21 "red") 
                                            (make-block 5 20 "red")))) true)

;;; world->world : World -> World
;;; Produces the next state of the game. 
(define (world->world w)
  (if (collision? (world-tetra w) (world-pile w))(next-tetra-world w)
      (make-world (move-tetra (world-tetra w) 0 -1) (world-pile w))))

#|(check-expect (world->world w1)
              (make-world (make-tetra
                           (make-posn 5.5 8.5)
                           (list (make-block 5 8 "LightGreen")
                                 (make-block 6 8 "LightGreen")
                                 (make-block 5 9 "LightGreen")
                                 (make-block 6 9 "LightGreen")))
                          (list (make-block 5 1 "green")
                                (make-block 5 2 "green")
                                (make-block 6 1 "green")
                                (make-block 6 2 "green"))))|#

(big-bang (make-world (new-tetra (random 7)) empty)
          (to-draw   world->image)          
          (on-tick   world->world 0.25)
          (on-key    handle-key)
          (stop-when game-over?))