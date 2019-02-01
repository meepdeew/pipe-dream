;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pipe-tiles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;; Constants


(define CELL-WIDTH 50)
(define GAME-WIDTH (* CELL-WIDTH 12))
(define GAME-HEIGHT (+ (* CELL-WIDTH 7) (* 4/3 CELL-WIDTH)))

(define BOARD-NUM-ROWS 7)
(define BOARD-NUM-COLS 10)
(define BOARD-NUM-CELLS
  (* BOARD-NUM-ROWS BOARD-NUM-COLS))

(define ORIENTATIONS (list "left" "right" "top" "bottom"))


(define BOARD-LEFT-EDGE   (+ CELL-WIDTH (* 2/3 CELL-WIDTH)))
(define BOARD-RIGHT-EDGE  (- GAME-WIDTH (* 1/3 CELL-WIDTH)))
(define BOARD-TOP-EDGE    CELL-WIDTH)
(define BOARD-BOTTOM-EDGE (- GAME-HEIGHT (* 1/3 CELL-WIDTH)))

; Image -> Image
(define (add-tile-edges tile)
  (overlay (square CELL-WIDTH "outline" "DimGray") tile))

(define CELL-BACKGROUND
  (add-tile-edges (square CELL-WIDTH "solid" "gray")))

(define LOWER-BARRIER (* CELL-WIDTH 0.4))
(define UPPER-BARRIER (* CELL-WIDTH 0.6))
(define WHITESPACE-BREAK (square 3 "solid" "white"))

(define PERCENT-FULL 100)
(define PIPE-WIDTH (* CELL-WIDTH 0.2))

(define MAIN-ICON (bitmap/file "./pd-icon.png"))

(define (linear-pipe-fill-length percent-filled)
  (* CELL-WIDTH (/ percent-filled PERCENT-FULL)))

(define BOTTOM-RIGHT-GREEN-SLICE
  (crop 0 0 UPPER-BARRIER UPPER-BARRIER
        (circle UPPER-BARRIER "solid" "green")))
(define BOTTOM-LEFT-GREEN-SLICE
  (crop UPPER-BARRIER 0 UPPER-BARRIER UPPER-BARRIER
        (circle UPPER-BARRIER "solid" "green")))
(define TOP-LEFT-GREEN-SLICE
  (crop UPPER-BARRIER UPPER-BARRIER UPPER-BARRIER UPPER-BARRIER
        (circle UPPER-BARRIER "solid" "green")))
(define TOP-RIGHT-GREEN-SLICE
  (crop 0 UPPER-BARRIER UPPER-BARRIER UPPER-BARRIER
        (circle UPPER-BARRIER "solid" "green")))

(define TOP-RIGHT-INNER-GRAY
  (crop 0 LOWER-BARRIER LOWER-BARRIER LOWER-BARRIER
        (circle LOWER-BARRIER "solid" "gray")))
(define TOP-LEFT-INNER-GRAY
  (crop LOWER-BARRIER LOWER-BARRIER LOWER-BARRIER LOWER-BARRIER
        (circle LOWER-BARRIER "solid" "gray")))
(define BOTTOM-RIGHT-INNER-GRAY
  (crop 0 0 LOWER-BARRIER LOWER-BARRIER
        (circle LOWER-BARRIER "solid" "gray")))
(define BOTTOM-LEFT-INNER-GRAY
  (crop LOWER-BARRIER 0 LOWER-BARRIER LOWER-BARRIER
        (circle LOWER-BARRIER "solid" "gray")))

(define ARROW-TOP
  (overlay/align
   "middle" "top"
   (triangle 6 "solid" "green")
   (rectangle 1 (/ 50 2) "solid" "green")))

(define ARROW-BOTTOM (rotate 180 ARROW-TOP))
(define ARROW-LEFT (rotate 90 ARROW-TOP))
(define ARROW-RIGHT (rotate 270 ARROW-TOP))


;;; Pipe Content (no background)

; Number[0,100] String -> Image
(define (pipe-horizontal-content percent-filled color)
  (rectangle (linear-pipe-fill-length percent-filled)
             PIPE-WIDTH
             "solid" color))

; Number[0,100] String -> Image
(define (pipe-vertical-content percent-filled color)
  (rectangle PIPE-WIDTH
             (linear-pipe-fill-length percent-filled)
             "solid" color))



;;; Pipe Tile Partially Filled w/ Background

; String Number[0,100] -> Image
(define (pipe-vertical-partial-fill start-position percent-filled)
  (add-tile-edges
   (overlay/align "middle"
                  (if (= 0 percent-filled) "bottom" start-position)
                  (pipe-vertical-content percent-filled "green")
                  (pipe-vertical-content PERCENT-FULL "black")
                  CELL-BACKGROUND)))

; String Number[0,100] -> Image
(define (pipe-horizontal-partial-fill start-position percent-filled)
  (add-tile-edges
   (overlay/align (if (= 0 percent-filled) "left" start-position)
                  "middle"
                  (pipe-horizontal-content percent-filled "green")
                  (pipe-horizontal-content PERCENT-FULL "black")
                  CELL-BACKGROUND)))

; String -> String
(define (opp-orientation posn-string)
  (cond [(string=? "top" posn-string)    "bottom"]
        [(string=? "bottom" posn-string) "top"]
        [(string=? "left" posn-string)   "right"]
        [(string=? "right" posn-string)  "left"]))

; -> Image
(define (start-s-img)
  (overlay
   (text "S" 10 "black")
   (square (/ CELL-WIDTH 2) "solid" "gray")))

; String Number[0,100] -> Image
(define (start-pipe-vertical-overlay
         end-position percent-filled)
  (overlay/align
   "middle" (opp-orientation end-position)
   (pipe-vertical-content (/ percent-filled 2) "green")
   (overlay
    (if (string=? end-position "top")
        ARROW-TOP ARROW-BOTTOM)
    (pipe-vertical-content (/ PERCENT-FULL 2) "black"))))

; String Number[0,100] -> Image
(define (start-pipe-horizontal-overlay
         end-position percent-filled)
  (overlay/align
   (opp-orientation end-position) "middle"
   (pipe-horizontal-content (/ percent-filled 2) "green")
   (overlay
    (if (string=? end-position "left")
        ARROW-LEFT ARROW-RIGHT)
    (pipe-horizontal-content (/ PERCENT-FULL 2) "black"))))

; String Number[0,100] -> Image
(define (letter-next-to-start-pipe-vertical
         end-position percent-filled)
  (if (string=? "top" end-position)
       (above
        (start-pipe-vertical-overlay end-position percent-filled)
        (start-s-img))
       (above
        (start-s-img)
        (start-pipe-vertical-overlay end-position percent-filled))))

; String Number[0,100] -> Image
(define (start-pipe-vertical-partial-fill
         end-position percent-filled)
  (overlay/align
   "middle" end-position
   (letter-next-to-start-pipe-vertical
    end-position percent-filled)
   CELL-BACKGROUND))

; String Number[0,100] -> Image
(define (letter-next-to-start-pipe-horizontal
         end-position percent-filled)
  (if (string=? "left" end-position)
       (beside
        (start-pipe-horizontal-overlay end-position percent-filled)
        (start-s-img))
       (beside
        (start-s-img)
        (start-pipe-horizontal-overlay end-position percent-filled))))

; String Number[0,100] -> Image
(define (start-pipe-horizontal-partial-fill
         end-position percent-filled)
  (overlay/align
   end-position "middle"
   (letter-next-to-start-pipe-horizontal
    end-position percent-filled)
   CELL-BACKGROUND))

; String Number[0,100] -> Image
(define (start-pipe-partial-fill end-position percent-filled)
  (add-tile-edges
   (cond
     [(or (string=? "top" end-position)
          (string=? "bottom" end-position))
      (start-pipe-vertical-partial-fill
       end-position percent-filled)]
     [(or (string=? "left" end-position)
          (string=? "right" end-position))
      (start-pipe-horizontal-partial-fill
       end-position percent-filled)])))



;;; Empties

(define PIPE-VERTICAL-EMPTY
  (add-tile-edges
   (overlay/align "middle" "bottom"
                  (pipe-vertical-content PERCENT-FULL "black")
                  CELL-BACKGROUND)))

(define PIPE-HORIZONTAL-EMPTY
  (add-tile-edges
   (overlay/align "left" "middle"
                  (pipe-horizontal-content PERCENT-FULL "black")
                  CELL-BACKGROUND)))



;;; Corner Sector (quartile)

(define (sector y-place x-place radius color)
  (crop  (cond [(string=? x-place "right") 0]
               [(string=? x-place "left") radius])
         (cond [(string=? y-place "bottom") 0]
               [(string=? y-place "top") radius])
        radius radius
        (circle radius "solid" color)))

(define PIPE-CORNER-BOTTOM-RIGHT-EMPTY
  (add-tile-edges
   (overlay/align "right" "bottom"
                  (sector "bottom" "right" LOWER-BARRIER "gray")
                  (sector "bottom" "right" UPPER-BARRIER "black")
                  CELL-BACKGROUND)))

(define PIPE-CORNER-TOP-LEFT-EMPTY
  (add-tile-edges
   (overlay/align "left" "top"
                  (sector "top" "left" LOWER-BARRIER "gray")
                  (sector "top" "left" UPPER-BARRIER "black")
                  CELL-BACKGROUND)))

(define PIPE-CORNER-TOP-RIGHT-EMPTY
  (add-tile-edges
   (overlay/align "right" "top"
                  (sector "top" "right" LOWER-BARRIER "gray")
                  (sector "top" "right" UPPER-BARRIER "black")
                  CELL-BACKGROUND)))

(define PIPE-CORNER-BOTTOM-LEFT-EMPTY
  (add-tile-edges
   (overlay/align "left" "bottom"
                  (sector "bottom" "left" LOWER-BARRIER "gray")
                  (sector "bottom" "left" UPPER-BARRIER "black")
                  CELL-BACKGROUND)))



; Number -> Number
(define (degrees-to-rad degrees)
  (* degrees (/ pi 180)))

; Natural[0..100] -> Number[0.0 ... 90.0]
(define (percent-to-degrees percent)
  (* 0.9 percent))

; Image -> Image
(define (rotate-ccw img deg)
  (rotate deg img))

; Image -> Image
(define (rotate-cw img deg)
  (rotate (- deg) img))



; Number -> Number
(define (amount-to-cut-bottom degrees)
  (* (cos (degrees-to-rad degrees)) UPPER-BARRIER))

; Number -> Number
(define (amount-to-cut-right degrees)
  (* (cos (degrees-to-rad degrees)) UPPER-BARRIER))

; Number -> Number
(define (amount-to-cut-left degrees)
  (* (cos (degrees-to-rad degrees)) UPPER-BARRIER))

; Number -> Number
(define (amount-to-cut-top degrees)
  (* (cos (degrees-to-rad degrees)) UPPER-BARRIER))



; Image Number -> Image
(define (trim-right img x-amount-to-cut)
  (crop 0 ;x
        0 ;y
        (- (image-width img) x-amount-to-cut) ;width
        (image-height img) ;height
        img))

; Image Number -> Image
(define (trim-left img x-amount-to-cut)
  (crop x-amount-to-cut ;x
        0 ;y
        (- (image-width img) x-amount-to-cut) ;width
        (image-height img) ;height
        img))

; Image Number -> Image
(define (trim-top img y-amount-to-cut)
  (crop 0 ;x
        y-amount-to-cut ;y
        (image-width img) ;width
        (- (image-height img) y-amount-to-cut) ;height
        img))

; Image Number -> Image
(define (trim-bottom img y-amount-to-cut)
  (crop 0
        0 ;y
        (image-width img) ;width
        (- (image-height img) y-amount-to-cut) ;height
        img))



; String Number -> GreenPizzaSlice
; Generate green piece of visual representation of fluid
; progress that can be overlayed on corner background.
(define (green-slice-top-right-partial-fill start-position degrees)
  (cond [(string=? start-position "right")
         (trim-right (rotate-cw TOP-LEFT-GREEN-SLICE degrees)
                     (amount-to-cut-right degrees))]
        [(string=? start-position "top")
         (trim-top (rotate-ccw BOTTOM-RIGHT-GREEN-SLICE degrees)
                   (amount-to-cut-top degrees))]))

; String Number -> GreenPizzaSlice
; Generate green piece of visual representation of fluid
; progress that can be overlayed on corner background.
(define (green-slice-top-left-partial-fill start-position degrees)
  (cond [(string=? start-position "left")
         (trim-left (rotate-ccw TOP-RIGHT-GREEN-SLICE degrees)
                    (amount-to-cut-left degrees))]
        [(string=? start-position "top")
         (trim-top (rotate-cw BOTTOM-LEFT-GREEN-SLICE degrees)
                   (amount-to-cut-top degrees))]))

; String Number -> GreenPizzaSlice
; Generate green piece of visual representation of fluid
; progress that can be overlayed on corner background.
(define (green-slice-bottom-left-partial-fill start-position degrees)
  (cond [(string=? start-position "left")
         (trim-left (rotate-cw BOTTOM-RIGHT-GREEN-SLICE degrees)
                    (amount-to-cut-left degrees))]
        [(string=? start-position "bottom")
         (trim-bottom (rotate-ccw TOP-LEFT-GREEN-SLICE degrees)
                      (amount-to-cut-bottom degrees))]))

; String Number -> GreenPizzaSlice
; Generate green piece of visual representation of fluid
; progress that can be overlayed on corner background.
(define (green-slice-bottom-right-partial-fill start-position degrees)
  (cond [(string=? start-position "right")
         (trim-right (rotate-ccw BOTTOM-LEFT-GREEN-SLICE degrees)
                     (amount-to-cut-right degrees))]
        [(string=? start-position "bottom")
         (trim-bottom (rotate-cw TOP-RIGHT-GREEN-SLICE degrees)
                      (amount-to-cut-bottom degrees))]))



; String Number -> Image
(define (pipe-corner-top-right-partial-fill start-position percent-filled)
  (add-tile-edges
   (overlay/align "right" "top"
                  TOP-RIGHT-INNER-GRAY
                  (green-slice-top-right-partial-fill
                   (if (= 0 percent-filled) "top" start-position)
                   (percent-to-degrees percent-filled))
                  PIPE-CORNER-TOP-RIGHT-EMPTY)))

; String Number -> Image
(define (pipe-corner-top-left-partial-fill start-position percent-filled)
  (add-tile-edges
   (overlay/align "left" "top"
                  TOP-LEFT-INNER-GRAY
                  (green-slice-top-left-partial-fill
                   (if (= 0 percent-filled) "top" start-position)
                   (percent-to-degrees percent-filled))
                  PIPE-CORNER-TOP-LEFT-EMPTY)))

; String Number -> Image
(define (pipe-corner-bottom-right-partial-fill start-position percent-filled)
  (add-tile-edges
   (overlay/align "right" "bottom"
                  BOTTOM-RIGHT-INNER-GRAY
                  (green-slice-bottom-right-partial-fill
                   (if (= 0 percent-filled) "bottom" start-position)
                   (percent-to-degrees percent-filled))
                  PIPE-CORNER-BOTTOM-RIGHT-EMPTY)))

; String Number -> Image
(define (pipe-corner-bottom-left-partial-fill start-position percent-filled)
  (add-tile-edges
   (overlay/align "left" "bottom"
                  BOTTOM-LEFT-INNER-GRAY
                  (green-slice-bottom-left-partial-fill
                   (if (= 0 percent-filled) "bottom" start-position)
                   (percent-to-degrees percent-filled))
                  PIPE-CORNER-BOTTOM-LEFT-EMPTY)))



;;; START-TILES
(beside (start-pipe-partial-fill "left" 0)
        (start-pipe-partial-fill "left" 54)
        (start-pipe-partial-fill "right" 0)
        (start-pipe-partial-fill "right" 39))

(beside (start-pipe-partial-fill "top" 0)
        (start-pipe-partial-fill "top" 40)
        (start-pipe-partial-fill "bottom" 0)
        (start-pipe-partial-fill "bottom" 70))


;;; LINES
(beside (pipe-vertical-partial-fill "unknown" 0)
        (pipe-vertical-partial-fill "bottom" 17)
        (pipe-vertical-partial-fill "top" 27))

(beside (pipe-horizontal-partial-fill "unknown" 0)
        (pipe-horizontal-partial-fill "left" 20)
        (pipe-horizontal-partial-fill "right" 40))



;;; CORNERS
(beside (pipe-corner-top-left-partial-fill "unknown" 0)
        (pipe-corner-top-left-partial-fill "left" 40)
        (pipe-corner-top-left-partial-fill "top" 30))

(beside (pipe-corner-top-right-partial-fill "unknown" 0)
        (pipe-corner-top-right-partial-fill "top" 34)
        (pipe-corner-top-right-partial-fill "right" 26))

(beside (pipe-corner-bottom-right-partial-fill "unknown" 0)
        (pipe-corner-bottom-right-partial-fill "bottom" 28)
        (pipe-corner-bottom-right-partial-fill "right" 22))

(beside (pipe-corner-bottom-left-partial-fill "unknown" 0)
        (pipe-corner-bottom-left-partial-fill "left" 12)
        (pipe-corner-bottom-left-partial-fill "bottom" 37))


(check-expect (add-to-end 0 empty) (cons 0 empty))
(check-expect (add-to-end 6 (cons 5 empty))
              (cons 5 (cons 6 empty)))
(check-expect (add-to-end 3 (cons 1 (cons 2 empty)))
              (cons 1 (cons 2 (cons 3 empty))))



; (listof Any) -> (listof Any)
; add element to end of list
(define (add-to-end item listo)
  (reverse-copy (cons item (reverse-copy listo))))


(check-expect (reverse-copy empty) empty)
(check-expect (reverse-copy (cons 1 empty)) (cons 1 empty))
(check-expect (reverse-copy (cons 1 (cons 2 empty))) (cons 2 (cons 1 empty)))
(check-expect (reverse-copy (cons 1 (cons 2 (cons 3 empty))))
              (cons 3 (cons 2 (cons 1 empty))))

; (listof Any) -> (listof Any)
(define (reverse-copy lst0)
  (local [(define (rev lst acc)
            (cond [(empty? lst) acc]
                  [else  (rev (rest lst)
                              (cons (first lst) acc))]))]
    (rev lst0 empty)))

(check-expect
 (front-to-back
  (list "red" "orange" "yellow" "green" "blue"))
 (list "orange" "yellow" "green" "blue" "red"))

; List-of-Anything -> List-of-Anything
; move the front-most item in a list the end.
(define (front-to-back lst)
  (add-to-end (first lst) (rest lst)))

; A Position is one of:
; - "unknown"
; - "top"
; - "bottom"
; - "left"
; - "right"

; A StateTile is a data representation
; of a tile at any moment in time.
(define-struct state-tile
  [type start-position end-position percent-filled])
; Type is one of:
; - "no-pipe"
; - "start-pipe"
; - "pipe-vertical"
; - "pipe-horizontal"
; - "pipe-corner-top-left"
; - "pipe-corner-top-right"
; - "pipe-corner-bottom-left"
; - "pipe-corner-bottom-right"
; StartPosition is a Position
; EndPosition is a Position
; PercentFilled is a Integer from [0, 100]

; Needs to be able to accept flow from either side
; What about position without a tile?
(define EMPTY-STATE-TILES-LIST
  (list (make-state-tile
         "pipe-vertical" "unknown" "unknown" 0)
        (make-state-tile
         "pipe-horizontal" "unknown" "unknown" 0)
        (make-state-tile
         "pipe-corner-top-left" "unknown" "unknown" 0)
        (make-state-tile
         "pipe-corner-top-right" "unknown" "unknown" 0)
        (make-state-tile
         "pipe-corner-bottom-left" "unknown" "unknown" 0)
        (make-state-tile
         "pipe-corner-bottom-right" "unknown" "unknown" 0)))

;(state-tile->image (make-state-tile "start-pipe" "unknown" "top" 0))
; StateTile -> Image
(define (state-tile->image st)
  (local [(define type (state-tile-type st))
          (define start-position (state-tile-start-position st))
          (define end-position (state-tile-end-position st))
          (define percent-filled (state-tile-percent-filled st))]
    (cond
      [(string=? "no-pipe" type)
       CELL-BACKGROUND]
      [(string=? "start-pipe" type)
       (start-pipe-partial-fill
        end-position percent-filled)]
      [(string=? "pipe-vertical" type)
       (pipe-vertical-partial-fill
        start-position percent-filled)]
      [(string=? "pipe-horizontal" type)
       (pipe-horizontal-partial-fill
        start-position percent-filled)]
      [(string=? "pipe-corner-top-left" type)
       (pipe-corner-top-left-partial-fill
        start-position percent-filled)]
      [(string=? "pipe-corner-top-right" type)
       (pipe-corner-top-right-partial-fill
        start-position percent-filled)]
      [(string=? "pipe-corner-bottom-left" type)
       (pipe-corner-bottom-left-partial-fill
        start-position percent-filled)]
      [(string=? "pipe-corner-bottom-right" type)
       (pipe-corner-bottom-right-partial-fill
        start-position percent-filled)])))


(define exlist
  (list (make-state-tile "pipe-vertical" "bottom" "unknown" 0)
        (make-state-tile "pipe-vertical" "bottom" "unknown" 10)
        (make-state-tile "pipe-vertical" "bottom" "unknown" 20)))

(check-expect
 (create-board-row 0 0 exlist)
 (state-tile->image
  (make-state-tile "pipe-vertical" "bottom" "unknown" 0)))

(check-expect
 (create-board-row 0 1 exlist)
 (beside (state-tile->image
          (make-state-tile "pipe-vertical" "bottom" "unknown" 0))
         (state-tile->image
          (make-state-tile "pipe-vertical" "bottom" "unknown" 10))))

(check-expect
 (create-board-row 0 2 exlist)
 (beside
  (state-tile->image
   (make-state-tile "pipe-vertical" "bottom" "unknown" 0))
  (beside
   (state-tile->image
    (make-state-tile "pipe-vertical" "bottom" "unknown" 10))
   (state-tile->image
    (make-state-tile "pipe-vertical" "bottom" "unknown" 20)))))

; Natural Natural (listof StateTile) -> Image
(define (create-board-row start-idx end-idx lst)
  (cond
    [(<= end-idx start-idx)
     (state-tile->image(list-ref lst start-idx))]
    [else
     (beside (state-tile->image (list-ref lst start-idx))
             (create-board-row (+ 1 start-idx) end-idx lst))]))

; Any -> Image
(define (random-tile-ignore-arg n)
  (random-tile))

; -> Image
(define (random-tile)
  ;; TODO defined a #'select-rand-elt-from
  (list-ref EMPTY-STATE-TILES-LIST
            (random (length EMPTY-STATE-TILES-LIST))))

; Any -> Image
(define (empty-tile-ignore-arg n)
  (empty-tile))

; -> Image
(define (empty-tile)
  (make-state-tile "no-pipe" "unknown" "unknown" 0))


;; TODO:
; A GameState composes two lists for maintaining
; the tile-queue and the game-board
(define-struct game-state [tile-queue tile-board])



(define BLUE-NAV (overlay
    (text "Pipe Dream" 14 "white")
    (rectangle GAME-WIDTH (/ CELL-WIDTH 3) "solid" "blue")))

(define WHITE-NAV (overlay/align
    "left" "bottom"
    (beside (text "Game" 14 "black")
            WHITESPACE-BREAK WHITESPACE-BREAK
            (text "Skill" 14 "black")
            WHITESPACE-BREAK WHITESPACE-BREAK
            (text "Help" 14 "black"))
    (rectangle GAME-WIDTH (/ CELL-WIDTH 3) "solid" "white")))

; GameState -> Image
(define (draw-everything s)
  (above BLUE-NAV
         WHITE-NAV
         (draw-dynamic-portion s)))

; GameState -> Image
(define (draw-dynamic-portion s)
  (above (rectangle GAME-WIDTH (/ CELL-WIDTH 3) "solid" "lightgray")
         (draw-dynamic-middle s)
         (rectangle GAME-WIDTH (/ CELL-WIDTH 3) "solid" "lightgray")))

; GameState -> Image
(define (draw-dynamic-middle s)
  (beside (rectangle (/ CELL-WIDTH 3) (* 7 CELL-WIDTH) "solid" "lightgray")
          (draw-left-content s)
          (rectangle (/ CELL-WIDTH 3) (* 7 CELL-WIDTH) "solid" "lightgray")
          ;(rectangle 400 300 "outline" "red")
          (draw-grid s)
          (rectangle (/ CELL-WIDTH 3) (* 7 CELL-WIDTH) "solid" "lightgray")))

; GameState -> Image
(define (draw-left-content s)
  (above
   (overlay
    MAIN-ICON
    (rectangle CELL-WIDTH (+ CELL-WIDTH 20) "solid" "lightgray"))
   (draw-tile-queue s)
   (rectangle CELL-WIDTH (- CELL-WIDTH 20) "solid" "lightgray")))

; GameState -> Image
(define (draw-grid s)
  (above (create-board-row  0  9 (game-state-tile-board s))
         (create-board-row 10 19 (game-state-tile-board s))
         (create-board-row 20 29 (game-state-tile-board s))
         (create-board-row 30 39 (game-state-tile-board s))
         (create-board-row 40 49 (game-state-tile-board s))
         (create-board-row 50 59 (game-state-tile-board s))
         (create-board-row 60 69 (game-state-tile-board s))))

; GameState -> Image
(define (draw-tile-queue s)
  (above (state-tile->image (fifth (game-state-tile-queue s)))
         (state-tile->image (fourth (game-state-tile-queue s)))
         (state-tile->image (third (game-state-tile-queue s)))
         (state-tile->image (second (game-state-tile-queue s)))
         (state-tile->image (first (game-state-tile-queue s)))))

; GameState Number -> GameState
; Pop head of TileQueue (and replace)
; and place tile in TileBoard
(define (queue-head-tile-to-board s at-board-idx)
  (make-game-state
   (add-to-end (random-tile)
               (rest (game-state-tile-queue s)))
   (replace-nth (first (game-state-tile-queue s))
                at-board-idx
                (game-state-tile-board s))))

; (listof Any) -> (listof Any)
; GameState KeyEvent -> GameState
; remove first element from queue
; add a random tile to back of queue
(define (key-handler s ke)
  (queue-head-tile-to-board s 0))

(check-expect (get-posn 0) (make-posn 0 0))
(check-expect (get-posn 1) (make-posn 0 1))
(check-expect (get-posn 10) (make-posn 1 0))
(check-expect (get-posn 11) (make-posn 1 1))

; Number -> Posn
(define (get-posn idx)
  (make-posn (floor (/ idx 10)) (modulo idx 10)))

(check-expect (get-index (make-posn 0 0)) 0)
(check-expect (get-index (make-posn 0 1)) 1)
(check-expect (get-index (make-posn 1 0)) 10)
(check-expect (get-index (make-posn 1 1)) 11)
(check-expect (get-index (make-posn 2 2)) 22)

; Posn -> Number
(define (get-index p)
  (+ (* 10 (posn-x p)) (posn-y p)))

(check-expect
 (coords-inside-board? (+ 30 BOARD-LEFT-EDGE)
                       (+ 15 BOARD-TOP-EDGE)) #true)
(check-expect
 (coords-inside-board? (+ BOARD-RIGHT-EDGE BOARD-LEFT-EDGE)
                       (+ BOARD-BOTTOM-EDGE BOARD-TOP-EDGE)) #false)

; Number Number -> Boolean
;; if x and y above and below appropriate vals
(define (coords-inside-board? x y)
  (and (> x BOARD-LEFT-EDGE)
       (< x BOARD-RIGHT-EDGE)
       (> y BOARD-TOP-EDGE)
       (< y BOARD-BOTTOM-EDGE)))

(check-expect (get-tile-index 32 25) 0)
(check-expect (get-tile-index 75 25) 1)
(check-expect (get-tile-index 32 78) 10)
(check-expect (get-tile-index 0 0) 0)
(check-expect (get-tile-index 49 49) 0)
(check-expect (get-tile-index 51 51) 11)

; Number Number -> Number
(define (get-tile-index x y)
  (get-index (make-posn (floor (/ y CELL-WIDTH))
                        (floor (/ x CELL-WIDTH)))))

(define (get-adjusted-tile-index x y)
  (get-tile-index (- x BOARD-LEFT-EDGE)
                  (- y BOARD-TOP-EDGE)))

; GameState Number Number MouseEvent -> GameState
(define (mouse-handler s x y me)
  (cond [(and (mouse-event? me)
              (mouse=? "button-up" me)
              (coords-inside-board? x y))
         (queue-head-tile-to-board
          s (get-adjusted-tile-index x y))]
        [else s]))

; GameState -> GameState
(define (main-pd s)
  (big-bang s
    [to-draw draw-everything]
    [on-mouse mouse-handler]
    [on-key key-handler]))

(check-expect
 (replace-nth "f" 2 (cons "a" (cons "b" (cons "c" (cons "d" empty)))))
 (cons "a" (cons "b" (cons "f" (cons "d" empty)))))
(check-expect
 (replace-nth "f" 1 (cons "b" (cons "c" (cons "d" empty))))
 (cons "b" (cons "f" (cons "d" empty))))
(check-expect (replace-nth 555 0 (list "A" "B" "C"))
              (list 555 "B" "C"))

; (listof Any) -> (listof Any)
(define (replace-nth item idx lst)
  (cond [(empty? lst) empty]
        [(= 0 idx) (cons item (rest lst))]
        [else (cons (first lst)
                    (replace-nth item (- idx 1) (rest lst)))]))


;; O|O|O    X|X|X
;; O|O|O -> X|O|X
;; O|O|O    X|X|X

; (listof Naturals) -> (listof Naturals)
(define (exclude-board-edges lst)
  (cond [(empty? lst) empty]
        [else (if (edge-piece? (first lst))
                  (exclude-board-edges (rest lst))
                  (cons (first lst)
                        (exclude-board-edges (rest lst))))]))

; Number -> Boolean
(define (edge-piece? num)
  (cond [(< num BOARD-NUM-COLS) #true]
        [(= (modulo num BOARD-NUM-COLS) 0) #true]
        [(= (modulo num BOARD-NUM-COLS) (- BOARD-NUM-COLS 1)) #true]
        [(>= num (- BOARD-NUM-CELLS BOARD-NUM-COLS)) #true]
        [else #false]))

; (listof Naturals) -> Natural
(define (select-rand-elt-from eligibles)
  (list-ref eligibles (random (length eligibles))))

; (listof Naturals) -> Natural
(define (get-start-tile-idx)
  (select-rand-elt-from
   (exclude-board-edges
    (build-list BOARD-NUM-CELLS identity))))


;; (listof StateTile)
(define TILE-QUEUE
  (build-list 9 random-tile-ignore-arg))

;; (listof StateTile)
(define TILE-BOARD
  (replace-nth
   ;; TODO: Make separate start-tile state and image.
   ;; Should it be able to face any orientation?
   (make-state-tile "start-pipe" "unknown"
                    (select-rand-elt-from ORIENTATIONS) 0)
   (get-start-tile-idx)
   (build-list BOARD-NUM-CELLS empty-tile-ignore-arg)))

;; To be passed to main and big-bang function
(define GAME-STATE (make-game-state TILE-QUEUE TILE-BOARD))

(main-pd GAME-STATE)




;;; TODO: When click
;; check if click was within the tile-board
;; if it was, which pixel locations
;; convert pixel coordinates to grid posn
;; convert grid posn to index in list

;; get the one removed from tile-queue
;; put it in the list in place of whatever was there.



; [0,0] at top left, [row-1, col-1] at bottom right
