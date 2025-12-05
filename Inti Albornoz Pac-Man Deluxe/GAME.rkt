#lang racket
(require 2htdp/image)
(require 2htdp/universe)


;; WORLD STRUCT (added lives)
(define-struct world
  (screen pac-x pac-y direction frame
          red-x red-y red-vx red-vy
          red2-x red2-y red2-vx red2-vy
          blue-x blue-y blue-vx blue-vy
          blue2-x blue2-y blue2-vx blue2-vy
          pink-x pink-y pink-dir
          pink2-x pink2-y pink2-dir
          grid
          level
          level-timer
          lives
          win-frame))

;; CONSTANTS
(define WIDTH 800)
(define HEIGHT 600)
(define BG (rectangle WIDTH HEIGHT "solid" (make-color 20 10 65)))
(define empty (rectangle 1 1 "solid" "transparent"))



;; GRID CONSTANTS
(define TILE 20)
(define GRID-W (quotient WIDTH TILE))
(define GRID-H (quotient HEIGHT TILE))

;; Grid values
(define EMPTY 0)
(define TRAIL 1)
(define WALL  2)
(define FILLED 3)

;; create empty grid
(define (make-empty-grid)
  (let ([g (make-vector GRID-H #f)])
    (for ([r (in-range GRID-H)])
      (vector-set! g r (make-vector GRID-W EMPTY)))
    (add-border-walls! g)
    g))

;; add border walls (top moved down 2 rows -> row index 2)
(define (add-border-walls! grid)
  (for ([c (in-range GRID-W)])
    (vector-set! (vector-ref grid 2) c WALL) ; top moved down two cells
    (vector-set! (vector-ref grid (- GRID-H 1)) c WALL)) ; bottom
  (for ([r (in-range GRID-H)])
    (vector-set! (vector-ref grid r) 0 WALL)
    (vector-set! (vector-ref grid r) (- GRID-W 1) WALL))
  grid)

;; bitmaps / sprites 
(define TITTLE-BUTTON (bitmap "tittle.png"))
(define START-BUTTON  (bitmap "start.png"))
(define QUIT-BUTTON   (bitmap "quit.png"))
(define LEVEL1-IMG (bitmap "level1.png"))
(define LEVEL2-IMG (bitmap "level2.png"))
(define LEVEL3-IMG (bitmap "level3.png"))
(define GAME-OVER-IMG (bitmap "gameover.png"))

(define pac1 (bitmap "pac0.png"))
(define pac2 (bitmap "pac1.png"))
(define pac3 (bitmap "pac2.png"))
(define paclives (bitmap "paclife.png"))

(define red     (bitmap "ghostred.png"))
(define blue    (bitmap "ghostblue.png"))
(define pink    (bitmap "ghostpink.png"))

(define redsmall  (bitmap "redghostgame.png"))
(define bluesmall (bitmap "blueghostgame.png"))
(define pinksmall (bitmap "pinkghostgame.png"))

(define pac1right (bitmap "pac0right.png"))
(define pac2right (bitmap "pac1right.png"))
(define pac3right (bitmap "pac2right.png"))

(define pac1left  (bitmap "pac0left.png"))
(define pac2left  (bitmap "pac1left.png"))
(define pac3left  (bitmap "pac2left.png"))

(define pac1up    (bitmap "pac0up.png"))
(define pac2up    (bitmap "pac1up.png"))
(define pac3up    (bitmap "pac2up.png"))

(define pac1down  (bitmap "pac0down.png"))
(define pac2down  (bitmap "pac1down.png"))
(define pac3down  (bitmap "pac2down.png"))

(define pac-frames (list pac1 pac2 pac3))
(define right-game-pac-frames (list pac1right pac2right pac3right))
(define left-game-pac-frames  (list pac1left  pac2left  pac3left))
(define up-game-pac-frames    (list pac1up    pac2up    pac3up))
(define down-game-pac-frames  (list pac1down  pac2down  pac3down))

;; WIN SCREEN ASSETS
(define win-screen (bitmap "youwin.png"))

(define pacwin1 (bitmap "pacwin1.png"))
(define pacwin2 (bitmap "pacwin2.png"))
(define pacwin3 (bitmap "pacwin3.png"))
(define pacwin4 (bitmap "pacwin4.png"))
(define pacwin5 (bitmap "pacwin5.png"))
(define pacwin6 (bitmap "pacwin6.png"))
(define pacwin7 (bitmap "pacwin7.png"))
(define pacwin8 (bitmap "pacwin8.png"))
(define pacwin9 (bitmap "pacwin9.png"))
(define pacwin10 (bitmap "pacwin10.png"))
(define pacwin11 (bitmap "pacwin11.png"))
(define pacwin12 (bitmap "pacwin12.png"))
(define pacwin13 (bitmap "pacwin13.png"))
(define pacwin14 (bitmap "pacwin14.png"))
(define pacwin15 (bitmap "pacwin15.png"))
(define pacwin16 (bitmap "pacwin16.png"))
(define pacwin17 (bitmap "pacwin17.png"))
(define pacwin18 (bitmap "pacwin18.png"))
(define pacwin19 (bitmap "pacwin19.png"))
(define pacwin20 (bitmap "pacwin20.png"))
(define pacwin21 (bitmap "pacwin21.png"))
(define pacwin22 (bitmap "pacwin22.png"))

(define pac-win-frames
  (list pacwin1 pacwin2 pacwin3 pacwin4 pacwin5 pacwin6 pacwin7 pacwin8
        pacwin9 pacwin10 pacwin11 pacwin12 pacwin13 pacwin14 pacwin15 pacwin16
        pacwin17 pacwin18 pacwin19 pacwin20 pacwin21 pacwin22))

(define DIGIT-IMG
  (vector
   (bitmap "0.png")
   (bitmap "1.png")
   (bitmap "2.png")
   (bitmap "3.png")
   (bitmap "4.png")
   (bitmap "5.png")
   (bitmap "6.png")
   (bitmap "7.png")
   (bitmap "8.png")
   (bitmap "9.png")))

;; Convert a number into a row of images

(define (pad dx dy img)
  (crop 0 0 (max 1 (+ (image-width img) dx)) (max 1 (image-height img)) img))

(define (number->image n)
  (define str
    (cond
      [(number? n) (number->string n)]
      [(string? n) n]
      [else (error "number->image requires number or string" n)]))
 (apply beside/align
       'center
       (map (λ (img) (pad -2 0 img))   ; brings digits 5px closer
            (for/list ([ch (in-string str)])
              (cond
                [(char-numeric? ch)
                 (vector-ref DIGIT-IMG
                             (string->number (string ch)))]
                [(char=? ch #\%)
                 (text "%" 22 "yellow")]
                [else empty])))))

;; pink path (unchanged)
(define pink-path
  (list
   (list 16 16)
   (list (- WIDTH 16) 16)
   (list (- WIDTH 16) (- HEIGHT 16))
   (list 16 (- HEIGHT 16))))

;; GRID COMPLETION
(define (grid-completion-percentage g)
  (let ([total 0] [filled 0])
    (for ([row g])
      (for ([cell row])
        (when (not (= cell WALL))
          (set! total (+ total 1))
          (when (= cell FILLED)
            (set! filled (+ filled 1)))) ))
    (if (= total 0) 0 (/ filled total))))

;; level spawn data (unchanged layout)
(define (level-spawn level)
  (cond
    [(= level 1)
     (list 150 200 6 4 0 0 0 0 350 200 "right" 0 0 0 0 0 0 0)]
    [(= level 2)
     (list 150 200 8 6 0 0 0 0 350 200 "right" 250 150 -8 6 0 0 0 0)]
    [(= level 3)
     (list 0 0 0 0 120 160 10 8 350 200 "right" 0 0 0 0 500 220 -10 8 300 350 "left")]
    [else
     (list 150 200 10 8 0 0 0 0 350 200 "right" 0 0 0 0 0 0 0)]))

;; safe list-ref: devuelve default si idx fuera de rango
(define (spawn-ref spawn idx [default 0])
  (if (< idx (length spawn))
      (list-ref spawn idx)
      default))

;; start-next-level builds a 'level' screen and uses lives preserved
(define (start-next-level w)
  (let* ([next-level (if (number? (world-level w)) (+ (world-level w) 1) 1)]
         [spawn (level-spawn next-level)]
         ;; extract spawn values safely
         [r1x (spawn-ref spawn 0)] [r1y (spawn-ref spawn 1)]
         [r1vx (spawn-ref spawn 2)] [r1vy (spawn-ref spawn 3)]
         [b1x (spawn-ref spawn 4)] [b1y (spawn-ref spawn 5)]
         [b1vx (spawn-ref spawn 6)] [b1vy (spawn-ref spawn 7)]
         [p1x (spawn-ref spawn 8)] [p1y (spawn-ref spawn 9)] [p1dir (spawn-ref spawn 10 0)]
         [r2x (spawn-ref spawn 11)] [r2y (spawn-ref spawn 12)]
         [r2vx (spawn-ref spawn 13)] [r2vy (spawn-ref spawn 14)]
         [b2x (spawn-ref spawn 15)] [b2y (spawn-ref spawn 16)]
         [b2vx (spawn-ref spawn 17)] [b2vy (spawn-ref spawn 18)]
         [p2x (spawn-ref spawn 19)] [p2y (spawn-ref spawn 20)]
         [p2dir (spawn-ref spawn 21 0)])
    (make-world
     "level"
     (world-pac-x w) (world-pac-y w) (world-direction w) (world-frame w)
     ;; red1
     r1x r1y r1vx r1vy
     ;; red2
     r2x r2y r2vx r2vy
     ;; blue1
     b1x b1y b1vx b1vy
     ;; blue2
     b2x b2y b2vx b2vy
     ;; pink1
     p1x p1y p1dir
     ;; pink2
     p2x p2y p2dir
     ;; grid (fresh)
     (make-empty-grid)
     ;; level number
     next-level
     ;; level-timer reset so we wait on level screen
     0
     ;; preserve lives if world had them
     (world-lives w)
     0)))

;; DRAW functions (draw-level, draw-menu unchanged except draw-game will show lives)

(define (draw-level w)
  (cond
    [(= (world-level w) 1)
     (place-image
      LEVEL1-IMG
      400 300 BG)]
    [(= (world-level w) 2)
     (place-image
      LEVEL2-IMG
      400 300 BG)]
    [(= (world-level w) 3)
     (place-image
      LEVEL3-IMG
      400 300 BG)]))

(define (draw w)
  (cond
    [(string=? (world-screen w) "win") (draw-win w)]
    [(string=? (world-screen w) "menu") (draw-menu w)]
    [(string=? (world-screen w) "level") (draw-level w)]
    [(string=? (world-screen w) "game") (draw-game w)]
    [(string=? (world-screen w) "death") (draw-death w)]
    [else BG]))

(define (draw-menu w)
  (place-image
   TITTLE-BUTTON 400 110
   (place-image
    START-BUTTON 400 310
    (place-image
     QUIT-BUTTON 400 360
     (place-image
      (list-ref pac-frames (world-frame w))
      (world-pac-x w)
      (world-pac-y w)
      (place-image
       red (world-red-x w) (world-red-y w)
       (place-image
        blue (world-blue-x w) (world-blue-y w)
        (place-image
         pink (world-pink-x w) (world-pink-y w)
         BG))))))))

(define (draw-grid grid base-img)
  (let ([tile-outline (rectangle TILE TILE "outline" (make-color 100 120 140 60))]
        [tile-trail   (rectangle TILE TILE "solid" "red")]
        [tile-wall    (rectangle TILE TILE "solid" "darkblue")])
    (let ([img base-img])
      (for ([r (in-range GRID-H)])
        (for ([c (in-range GRID-W)])
          (let ([val (vector-ref (vector-ref grid r) c)]
                [cx (+ (* c TILE) (quotient TILE 2))]
                [cy (+ (* r TILE) (quotient TILE 2))])
            (cond
              [(or (= val WALL) (= val FILLED)) (set! img (place-image tile-wall cx cy img))]
              [(= val TRAIL) (set! img (place-image tile-trail cx cy img))]
              [else (void)])
            (set! img (place-image tile-outline cx cy img)))))
      img)))

;; draw-game now shows lives as 3 circles (top-left)
(define (draw-game w)
  (let* ([g (world-grid w)]
         [base (draw-grid g BG)]
         [pac-img
          (cond
            [(string=? (world-direction w) "right") (list-ref right-game-pac-frames (world-frame w))]
            [(string=? (world-direction w) "left")  (list-ref left-game-pac-frames  (world-frame w))]
            [(string=? (world-direction w) "up")    (list-ref up-game-pac-frames    (world-frame w))]
            [(string=? (world-direction w) "down")  (list-ref down-game-pac-frames  (world-frame w))]
            [else (list-ref right-game-pac-frames (world-frame w))])]

         ;; --- NEW: Calculate completion percentage ---
         [completion (grid-completion-percentage g)]
         [pct (exact-round (* completion 100))]
         [pct-str (string-append (number->string pct) "%")]
         [pct-img (number->image pct-str)])

    ;; draw lives
    (let* ([life1 (if (>= (world-lives w) 1) paclives empty)]
           [life2 (if (>= (world-lives w) 2) paclives empty)]
           [life3 (if (>= (world-lives w) 3) paclives empty)]
           [img (place-image life1 40 20 base)]
           [img (place-image life2 70 20 img)]
           [img (place-image life3 100 20 img)]

           ;; --- NEW: place percentage on RIGHT side ---
           [img (place-image pct-img (- WIDTH 60) 30 img)]

           ;; pacman
           [img (place-image pac-img (world-pac-x w) (world-pac-y w) img)]

           ;; ghosts
           [img (if (and (number? (world-red-x w)) (not (= (world-red-x w) 0)))
                    (place-image redsmall (world-red-x w) (world-red-y w) img)
                    img)]
           [img (if (and (number? (world-red2-x w)) (not (= (world-red2-x w) 0)))
                    (place-image redsmall (world-red2-x w) (world-red2-y w) img)
                    img)]
           [img (if (and (number? (world-blue-x w)) (not (= (world-blue-x w) 0)))
                    (place-image bluesmall (world-blue-x w) (world-blue-y w) img)
                    img)]
           [img (if (and (number? (world-blue2-x w)) (not (= (world-blue2-x w) 0)))
                    (place-image bluesmall (world-blue2-x w) (world-blue2-y w) img)
                    img)]
           [img (if (and (number? (world-pink-x w)) (not (= (world-pink-x w) 0)))
                    (place-image pinksmall (world-pink-x w) (world-pink-y w) img)
                    img)]
           [img (if (and (number? (world-pink2-x w)) (not (= (world-pink2-x w) 0)))
                    (place-image pinksmall (world-pink2-x w) (world-pink2-y w) img)
                    img)])

      img)))

(define (draw-death w)
  (place-image
   GAME-OVER-IMG
   390 300 BG))

(define (draw-win w)
  (place-image
   win-screen
   400 200
   (place-image
    (list-ref pac-win-frames (world-win-frame w)) 
    (/ WIDTH 2) (+ (/ HEIGHT 2) 120)   ; adjust Y position if needed
    BG)))

;; Movement helpers (unchanged)
(define (bounce-red-new-state rx ry rvx rvy grid)
  (let* ([new-x (+ rx rvx)]
         [new-y (+ ry rvy)]
         [hit-x (wall-at? grid new-x ry)]
         [hit-y (wall-at? grid rx new-y)]
         [new-vx (if hit-x (- rvx) rvx)]
         [new-vy (if hit-y (- rvy) rvy)]
         [final-x (if hit-x rx new-x)]
         [final-y (if hit-y ry new-y)])
    (list final-x final-y new-vx new-vy)))

(define (bounce-blue-new-state bx by bvx bvy grid)
  (let* ([new-x (+ bx bvx)]
         [new-y (+ by bvy)]
         [hit-x (wall-at? grid new-x by)]
         [hit-y (wall-at? grid bx new-y)]
         [new-vx (if hit-x (- bvx) bvx)]
         [new-vy (if hit-y (- bvy) bvy)]
         [final-x (if hit-x bx new-x)]
         [final-y (if hit-y by new-y)])
    (list final-x final-y new-vx new-vy)))

(define (move-pink-wall-follow px py dir grid)
  (define dirs '("up" "right" "down" "left"))
  (define (next-pos x y d)
    (cond
      [(string=? d "up")    (list x (- y 6))]
      [(string=? d "down")  (list x (+ y 6))]
      [(string=? d "left")  (list (- x 6) y)]
      [(string=? d "right") (list (+ x 6) y)]))
  (define (index-of lst el)
    (let loop ([i 0] [lst lst])
      (cond [(null? lst) 0]
            [(equal? (car lst) el) i]
            [else (loop (+ i 1) (cdr lst))])))
  (define (turn-right d)
    (list-ref dirs (modulo (+ (index-of dirs d) 1) 4)))
  (define (turn-left d)
    (list-ref dirs (modulo (+ (index-of dirs d) 3) 4)))
  (let* ([forward (next-pos px py dir)]
         [fx (first forward)]
         [fy (second forward)])
    (if (not (wall-at? grid fx fy))
        (list fx fy dir)
        (let ([right (turn-right dir)])
          (let ([rpos (next-pos px py right)])
            (if (not (wall-at? grid (first rpos) (second rpos)))
                (list (first rpos) (second rpos) right)
                (let ([left (turn-left dir)])
                  (let ([lpos (next-pos px py left)])
                    (if (not (wall-at? grid (first lpos) (second lpos)))
                        (list (first lpos) (second lpos) left)
                        (let ([rev (turn-right (turn-right dir))])
                          (let ([npos (next-pos px py rev)])
                            (list (first npos) (second npos) rev))))))))))))

;; mark pac tile
(define (mark-pac-tile! grid px py)
  (let* ([col (quotient px TILE)]
         [row (quotient py TILE)])
    (when (and (>= row 0) (< row GRID-H) (>= col 0) (< col GRID-W))
      (when (= (vector-ref (vector-ref grid row) col) EMPTY)
        (vector-set! (vector-ref grid row) col TRAIL)))
    grid))

;; helpers
(define (wall-at? grid px py)
  (let* ([col (quotient px TILE)]
         [row (quotient py TILE)])
    (or (< row 0) (>= row GRID-H)
        (< col 0) (>= col GRID-W)
        (= (vector-ref (vector-ref grid row) col) WALL))))

(define (dist a b)
  (let ([dx (- (first a) (first b))]
        [dy (- (second a) (second b))])
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (any-trail? grid)
  (let loopr ([r 0])
    (cond
      [(>= r GRID-H) #f]
      [else
       (let ([row (vector-ref grid r)])
         (let loopc ([c 0])
           (cond
             [(>= c GRID-W) (loopr (+ r 1))]
             [(= (vector-ref row c) TRAIL) #t]
             [else (loopc (+ c 1))])))])))

;; flood-fill (unchanged)
(define (fill-enclosed-areas! grid)
  (let ([visited (build-vector GRID-H (λ (_) (make-vector GRID-W 0)))])
    (for ([r (in-range GRID-H)])
      (for ([c (in-range GRID-W)])
        (when (and (= (vector-ref (vector-ref grid r) c) EMPTY)
                   (= (vector-ref (vector-ref visited r) c) 0))
          (let ([stack (list (list r c))] [comp '()] [touches-wall? #f])
            (let loop ()
              (if (null? stack)
                  (when (not touches-wall?)
                    (for ([cell comp])
                      (vector-set! (vector-ref grid (first cell)) (second cell) FILLED)))
                  (let* ([cell (car stack)] [rest (cdr stack)] [cr (first cell)] [cc (second cell)])
                    (set! stack rest)
                    (when (= (vector-ref (vector-ref visited cr) cc) 0)
                      (vector-set! (vector-ref visited cr) cc 1)
                      (set! comp (cons (list cr cc) comp))
                      (for ([d '((0 -1) (0 1) (-1 0) (1 0))])
                        (let* ([nr (+ cr (first d))] [nc (+ cc (second d))])
                          (cond
                            [(or (< nr 0) (>= nr GRID-H) (< nc 0) (>= nc GRID-W))
                             (set! touches-wall? #t)]
                            [else
                             (let ([val (vector-ref (vector-ref grid nr) nc)])
                               (cond
                                 [(= val WALL) (set! touches-wall? #t)]
                                 [(and (= val EMPTY)
                                       (= (vector-ref (vector-ref visited nr) nc) 0))
                                  (set! stack (cons (list nr nc) stack))]
                                 [else (void)]))]))))
                    (loop)))))))))
  (for ([r (in-range GRID-H)]) (for ([c (in-range GRID-W)])
                                 (when (= (vector-ref (vector-ref grid r) c) TRAIL)
                                   (vector-set! (vector-ref grid r) c FILLED))))
  grid)

(define (fill-trail! grid)
  (for ([r (in-range GRID-H)]) (for ([c (in-range GRID-W)])
                                 (when (= (vector-ref (vector-ref grid r) c) TRAIL)
                                   (vector-set! (vector-ref grid r) c FILLED))))
  grid)

;; TICK: menu / level / game (with lives handling)
(define (tick w)
  (cond
    [(string=? (world-screen w) "menu")
     (make-world
      "menu"
      (+ (world-pac-x w) 5) (world-pac-y w) (world-direction w) (modulo (+ (world-frame w) 1) 3)
      ;; red1
      (+ (world-red-x w) (world-red-vx w)) (world-red-y w) (world-red-vx w) (world-red-vy w)
      ;; red2
      (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
      ;; blue1
      (+ (world-blue-x w) 5) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
      ;; blue2
      (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
      ;; pink1
      (+ (world-pink-x w) 5) (world-pink-y w) (world-pink-dir w)
      ;; pink2
      (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
      ;; grid
      (world-grid w)
      ;; level
      (world-level w)
      ;; level-timer
      (world-level-timer w)
      ;; lives
      (world-lives w)
      0)]

    [(string=? (world-screen w) "level")
     (let ([timer (+ (world-level-timer w) 0.08)])
       (if (>= timer 2.0)
           (make-world
            "game"
            (world-pac-x w) (world-pac-y w) (world-direction w) (world-frame w)
            ;; red1
            (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
            ;; red2
            (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
            ;; blue1
            (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
            ;; blue2
            (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
            ;; pink1
            (world-pink-x w) (world-pink-y w) (world-pink-dir w)
            ;; pink2
            (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
            ;; grid
            (world-grid w)
            ;; level
            (world-level w)
            ;; level-timer reset
            0
            ;; lives
            (world-lives w)
            0)
           (make-world
            "level"
            (world-pac-x w) (world-pac-y w) (world-direction w) (world-frame w)
            ;; red1
            (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
            ;; red2
            (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
            ;; blue1
            (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
            ;; blue2
            (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
            ;; pink1
            (world-pink-x w) (world-pink-y w) (world-pink-dir w)
            ;; pink2
            (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
            ;; grid
            (world-grid w)
            ;; level
            (world-level w)
            ;; timer
            timer
            ;; lives
            (world-lives w)
            0)))]

    [(string=? (world-screen w) "game")
     (let* ([g (world-grid w)]
            [dx (cond [(string=? (world-direction w) "right") 9]
                      [(string=? (world-direction w) "left") -9]
                      [else 0])]
            [dy (cond [(string=? (world-direction w) "down") 9]
                      [(string=? (world-direction w) "up") -9]
                      [else 0])]
            [old-x (world-pac-x w)]
            [old-y (world-pac-y w)]
            [temp-x (+ old-x dx)]
            [temp-y (+ old-y dy)]
            [col (quotient temp-x TILE)]
            [row (quotient temp-y TILE)]
            [hits-solid? (or (wall-at? g temp-x temp-y)
                             (and (>= row 0) (< row GRID-H)
                                  (>= col 0) (< col GRID-W)
                                  (= (vector-ref (vector-ref g row) col) FILLED)))]
            [new-px (if (wall-at? g temp-x old-y) old-x temp-x)]
            [new-py (if (wall-at? g new-px temp-y) old-y temp-y)]
            [_ (mark-pac-tile! g new-px new-py)]
            [_ (when hits-solid? (fill-trail! g) (fill-enclosed-areas! g))]

            ;; RED1
            [rx (world-red-x w)] [ry (world-red-y w)] [rvx (world-red-vx w)] [rvy (world-red-vy w)]
            [bounce (bounce-red-new-state rx ry rvx rvy g)]
            [new-rx (list-ref bounce 0)] [new-ry (list-ref bounce 1)] [new-rvx (list-ref bounce 2)] [new-rvy (list-ref bounce 3)]

            ;; RED2
            [r2x (world-red2-x w)] [r2y (world-red2-y w)] [r2vx (world-red2-vx w)] [r2vy (world-red2-vy w)]
            [bounce2 (if (and (number? r2x) (not (= r2x 0)))
                         (bounce-red-new-state r2x r2y r2vx r2vy g)
                         (list r2x r2y r2vx r2vy))]
            [new-r2x (list-ref bounce2 0)] [new-r2y (list-ref bounce2 1)] [new-r2vx (list-ref bounce2 2)] [new-r2vy (list-ref bounce2 3)]

            ;; BLUE1
            [bx (world-blue-x w)] [by (world-blue-y w)] [bvx (world-blue-vx w)] [bvy (world-blue-vy w)]
            [blue-bounce (bounce-blue-new-state bx by bvx bvy g)]
            [new-bx (list-ref blue-bounce 0)] [new-by (list-ref blue-bounce 1)] [new-bvx (list-ref blue-bounce 2)] [new-bvy (list-ref blue-bounce 3)]

            ;; BLUE2
            [b2x (world-blue2-x w)] [b2y (world-blue2-y w)] [b2vx (world-blue2-vx w)] [b2vy (world-blue2-vy w)]
            [blue-bounce2 (if (and (number? b2x) (not (= b2x 0)))
                             (bounce-blue-new-state b2x b2y b2vx b2vy g)
                             (list b2x b2y b2vx b2vy))]
            [new-b2x (list-ref blue-bounce2 0)] [new-b2y (list-ref blue-bounce2 1)] [new-b2vx (list-ref blue-bounce2 2)] [new-b2vy (list-ref blue-bounce2 3)]

            ;; PINK1
            [pink-state (move-pink-wall-follow (world-pink-x w) (world-pink-y w) (world-pink-dir w) g)]
            [new-px-pink (list-ref pink-state 0)] [new-py-pink (list-ref pink-state 1)] [new-dir-pink (list-ref pink-state 2)]

            ;; PINK2
            [p2x (world-pink2-x w)] [p2y (world-pink2-y w)] [p2dir (world-pink2-dir w)]
            [pink2-state (if (and (number? p2x) (not (= p2x 0))) (move-pink-wall-follow p2x p2y p2dir g) (list p2x p2y p2dir))]
            [new-p2x (list-ref pink2-state 0)] [new-p2y (list-ref pink2-state 1)] [new-p2dir (list-ref pink2-state 2)]

            ;; collision check (include extras)
            [pac-pos (list new-px new-py)]
            [hit? (or (< (dist pac-pos (list new-rx new-ry)) 22)
                      (< (dist pac-pos (list new-r2x new-r2y)) 22)
                      (< (dist pac-pos (list new-bx new-by)) 22)
                      (< (dist pac-pos (list new-b2x new-b2y)) 22)
                      (< (dist pac-pos (list new-px-pink new-py-pink)) 22)
                      (< (dist pac-pos (list new-p2x new-p2y)) 22))]
            [completion (grid-completion-percentage g)])

       (cond
         ;; Pac hits ghost
         [hit?
          (if (<= (world-lives w) 1)
              ;; last life -> game over
              (make-world
               "death"
               new-px new-py (world-direction w) (world-frame w)
               new-rx new-ry new-rvx new-rvy
               new-r2x new-r2y new-r2vx new-r2vy
               new-bx new-by new-bvx new-bvy
               new-b2x new-b2y new-b2vx new-b2vy
               new-px-pink new-py-pink new-dir-pink
               new-p2x new-p2y new-p2dir
               g
               (world-level w)
               (world-level-timer w)
               0
               0) ; lives 0
              ;; lost a life but still have more -> decrement lives, respawn ghosts and pac, show level screen briefly
              (let* ([cur-level (if (number? (world-level w)) (world-level w) 1)]
       [spawn (level-spawn cur-level)]
       [r1x (spawn-ref spawn 0)] [r1y (spawn-ref spawn 1)] [r1vx (spawn-ref spawn 2)] [r1vy (spawn-ref spawn 3)]
       [b1x (spawn-ref spawn 4)] [b1y (spawn-ref spawn 5)] [b1vx (spawn-ref spawn 6)] [b1vy (spawn-ref spawn 7)]
       [p1x (spawn-ref spawn 8)] [p1y (spawn-ref spawn 9)] [p1dir (spawn-ref spawn 10 0)]
       [r2x (spawn-ref spawn 11)] [r2y (spawn-ref spawn 12)] [r2vx (spawn-ref spawn 13)] [r2vy (spawn-ref spawn 14)]
       [b2x (spawn-ref spawn 15)] [b2y (spawn-ref spawn 16)] [b2vx (spawn-ref spawn 17)] [b2vy (spawn-ref spawn 18)]
       [p2x (spawn-ref spawn 19)] [p2y (spawn-ref spawn 20)] [p2dir (spawn-ref spawn 21 0)]
       [new-lives (sub1 (world-lives w))])
                ;; Show 'level' screen (same level) so player sees message, preserve grid
                (make-world
                 "level"
                 ;; reset pac to starting coords (menu start)
                 40 480 "right" 0
                 ;; red1
                 r1x r1y r1vx r1vy
                 ;; red2
                 r2x r2y r2vx r2vy
                 ;; blue1
                 b1x b1y b1vx b1vy
                 ;; blue2
                 b2x b2y b2vx b2vy
                 ;; pink1
                 p1x p1y p1dir
                 ;; pink2
                 p2x p2y p2dir
                 ;; keep current grid (so level progress stays)
                 g
                 ;; level number (preserve)
                 cur-level
                 ;; level-timer reset
                 0
                 ;; lives (decremented)
                 new-lives
                 0)))]
         

         ;; level complete
[(>= completion 0.75)
 (if (>= (world-level w) 3)
     ;; WIN SCREEN
     (make-world
 "win"
 new-px new-py (world-direction w) (world-frame w)
 new-rx new-ry new-rvx new-rvy
 new-r2x new-r2y new-r2vx new-r2vy
 new-bx new-by new-bvx new-bvy
 new-b2x new-b2y new-b2vx new-b2vy
 new-px-pink new-py-pink new-dir-pink
 new-p2x new-p2y new-p2dir
 g
 (world-level w)
 (world-level-timer w)
 (world-lives w)
 0)
     ;; Otherwise normal level transition
     (start-next-level w))]

         ;; normal update
         [else
          (make-world "game"
                      new-px new-py (world-direction w) (modulo (+ (world-frame w) 1) 3)
                      new-rx new-ry new-rvx new-rvy
                      new-r2x new-r2y new-r2vx new-r2vy
                      new-bx new-by new-bvx new-bvy
                      new-b2x new-b2y new-b2vx new-b2vy
                      new-px-pink new-py-pink new-dir-pink
                      new-p2x new-p2y new-p2dir
                      g
                      (world-level w)
                      (world-level-timer w)
                      (world-lives w)
                      0)]))]
    [(string=? (world-screen w) "win")
 (make-world
  "win"
  (world-pac-x w) (world-pac-y w) (world-direction w) (world-frame w)
  (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
  (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
  (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
  (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
  (world-pink-x w) (world-pink-y w) (world-pink-dir w)
  (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
  (world-grid w)
  (world-level w)
  (world-level-timer w)
  (world-lives w)
  (modulo (add1 (world-win-frame w)) (length pac-win-frames)))]

    [else w]))

;; click detection helpers
(define (clicked-start? x y)
  (and (>= x 275) (<= x 525) (>= y 270) (<= y 350)))
(define (clicked-quit? x y)
  (and (>= x 275) (<= x 525) (>= y 320) (<= y 400)))

;; mouse handler: start, quit, death->menu (reset lives)
(define (mouse w x y event)
  (cond
    [(and (string=? (world-screen w) "win")
      (mouse=? event "button-down"))
 (make-world
  "menu"
  40 480 "right" 0
  ;; red1
  150 480 5 2
  ;; red2
  0 0 0 0
  ;; blue1
  250 480 3 2
  ;; blue2
  0 0 0 0
  ;; pink1
  350 480 "right"
  ;; pink2
  0 0 0
  ;; grid
  (make-empty-grid)
  ;; level
  0
  ;; level timer
  0
  3
  0)]
    [(and (string=? (world-screen w) "menu") (mouse=? event "button-down") (clicked-start? x y))
     (start-next-level w)]
    [(and (string=? (world-screen w) "menu") (mouse=? event "button-down") (clicked-quit? x y))
     (stop-with w)]
    [(and (string=? (world-screen w) "death") (mouse=? event "button-down"))
     (make-world
      "menu"
      0 480 "right" 0
      ;; red1
      150 480 5 2
      ;; red2
      0 0 0 0
      ;; blue1
      250 480 3 2
      ;; blue2
      0 0 0 0
      ;; pink1
      350 480 "right"
      ;; pink2
      0 0 0
      ;; grid
      (make-empty-grid)
      ;; level
      0
      ;; level-timer
      0
      ;; lives reset to 3
      3
      0)]
    [else w]))

;; key handler (preserve fields; unchanged except new lives slot preserved)
(define (key w k)
  (cond
    [(string=? (world-screen w) "game")
     (cond
       [(key=? k "right")
        (make-world "game"
                    (world-pac-x w) (world-pac-y w) "right" (world-frame w)
                    (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
                    (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
                    (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
                    (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
                    (world-pink-x w) (world-pink-y w) (world-pink-dir w)
                    (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
                    (world-grid w)
                    (world-level w)
                    (world-level-timer w)
                    (world-lives w)
                    0)]
       [(key=? k "left")
        (make-world "game"
                    (world-pac-x w) (world-pac-y w) "left" (world-frame w)
                    (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
                    (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
                    (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
                    (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
                    (world-pink-x w) (world-pink-y w) (world-pink-dir w)
                    (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
                    (world-grid w)
                    (world-level w)
                    (world-level-timer w)
                    (world-lives w)
                    0)]
       [(key=? k "up")
        (make-world "game"
                    (world-pac-x w) (world-pac-y w) "up" (world-frame w)
                    (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
                    (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
                    (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
                    (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
                    (world-pink-x w) (world-pink-y w) (world-pink-dir w)
                    (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
                    (world-grid w)
                    (world-level w)
                    (world-level-timer w)
                    (world-lives w)
                    0)]
       [(key=? k "down")
        (make-world "game"
                    (world-pac-x w) (world-pac-y w) "down" (world-frame w)
                    (world-red-x w) (world-red-y w) (world-red-vx w) (world-red-vy w)
                    (world-red2-x w) (world-red2-y w) (world-red2-vx w) (world-red2-vy w)
                    (world-blue-x w) (world-blue-y w) (world-blue-vx w) (world-blue-vy w)
                    (world-blue2-x w) (world-blue2-y w) (world-blue2-vx w) (world-blue2-vy w)
                    (world-pink-x w) (world-pink-y w) (world-pink-dir w)
                    (world-pink2-x w) (world-pink2-y w) (world-pink2-dir w)
                    (world-grid w)
                    (world-level w)
                    (world-level-timer w)
                    (world-lives w)
                    0)]
       [else w])]
    [else w]))

;; START BIG-BANG (initial world)
(big-bang
 (make-world
  "menu"
  ;; pac
  0 480 "right" 0
  ;; red1
  150 480 5 2
  ;; red2
  0 0 0 0
  ;; blue1
  250 480 3 2
  ;; blue2
  0 0 0 0
  ;; pink1
  350 480 "right"
  ;; pink2
  0 0 0
  ;; grid
  (make-empty-grid)
  ;; level
  0
  ;; level-timer
  0
  ;; lives (start with 3)
  3
  0)
 (on-tick tick 0.08)
 (to-draw draw)
 (on-mouse mouse)
 (on-key key))
