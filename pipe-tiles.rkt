(require 2htdp/image)


;;; Constants

(define CELL-WIDTH 50)
(define CELL-BACKGROUND (square CELL-WIDTH "solid" "gray"))

(define LOWER-BARRIER (* CELL-WIDTH 0.4))
(define UPPER-BARRIER (* CELL-WIDTH 0.6))
(define WHITESPACE-BREAK (square 3 "solid" "white"))

(define PERCENT-FULL 100)
(define PIPE-WIDTH (* CELL-WIDTH 0.2))

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

(define TOP-RIGHT-INNER-GRAY (crop 0 LOWER-BARRIER LOWER-BARRIER LOWER-BARRIER
                                   (circle LOWER-BARRIER "solid" "gray")))
(define TOP-LEFT-INNER-GRAY (crop LOWER-BARRIER LOWER-BARRIER LOWER-BARRIER LOWER-BARRIER
                                  (circle LOWER-BARRIER "solid" "gray")))
(define BOTTOM-RIGHT-INNER-GRAY (crop 0 0 LOWER-BARRIER LOWER-BARRIER
                                      (circle LOWER-BARRIER "solid" "gray")))
(define BOTTOM-LEFT-INNER-GRAY (crop LOWER-BARRIER 0 LOWER-BARRIER LOWER-BARRIER
                                     (circle LOWER-BARRIER "solid" "gray")))



;;; Pipe Content (no background)

(define (pipe-horizontal-content percent-filled color)
  (rectangle (linear-pipe-fill-length percent-filled)
             PIPE-WIDTH
             "solid" color))

(define (pipe-vertical-content percent-filled color)
  (rectangle PIPE-WIDTH
             (linear-pipe-fill-length percent-filled)
             "solid" color))



;;; Pipe Tile Partially Filled w/ Background

(define (pipe-vertical-partial-fill start-position percent-filled)
  (overlay/align "middle" start-position
                 (pipe-vertical-content percent-filled "green")
                 (pipe-vertical-content PERCENT-FULL "black")
                 CELL-BACKGROUND))

(define (pipe-horizontal-partial-fill start-position percent-filled)
  (overlay/align start-position "middle"
                 (pipe-horizontal-content percent-filled "green")
                 (pipe-horizontal-content PERCENT-FULL "black")
                 CELL-BACKGROUND))



;;; Empties

(define PIPE-VERTICAL-EMPTY
  (overlay/align "middle" "bottom"
                 (pipe-vertical-content PERCENT-FULL "black")
                 CELL-BACKGROUND))

(define PIPE-HORIZONTAL-EMPTY
  (overlay/align "left" "middle"
                 (pipe-horizontal-content PERCENT-FULL "black")
                 CELL-BACKGROUND))



;;; Corner Content (A [percent-based] slice of a Sector)



;;; Corner Sector (quartile)

(define (sector y-place x-place radius color)
  (crop  (cond [(string=? x-place "right") 0]
               [(string=? x-place "left") radius])
         (cond [(string=? y-place "bottom") 0]
               [(string=? y-place "top") radius])
        radius radius
        (circle radius "solid" color)))


;;; TODO Shouldn't use this function, gross.
(define (overlay-corners y-place x-place)
  (overlay/align x-place y-place
                 (sector y-place x-place LOWER-BARRIER "gray")
                 (sector y-place x-place UPPER-BARRIER "black")))


(define PIPE-CORNER-BOTTOM-RIGHT-EMPTY
  (overlay/align
   "right" "bottom"
   (sector "bottom" "right" LOWER-BARRIER "gray")
   ;+(overlay-corners "bottom" "right")+
   ;;; green slice covering the black (...-content some-percent)
   ;;; even SECTOR should be replaced by a (pipe-corner-bottom-right-content FULL-PERCENT)
   (sector "bottom" "right" UPPER-BARRIER "black")
   CELL-BACKGROUND))

(define PIPE-CORNER-TOP-LEFT-EMPTY
  (overlay/align
   "left" "top"
   (overlay-corners "top" "left")
   CELL-BACKGROUND))

(define PIPE-CORNER-TOP-RIGHT-EMPTY
  (overlay/align
   "right" "top"
   (overlay-corners "top" "right")
   CELL-BACKGROUND))

(define PIPE-CORNER-BOTTOM-LEFT-EMPTY
  (overlay/align
   "left" "bottom"
   (overlay-corners "bottom" "left")
   CELL-BACKGROUND))

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
(define (amount-to-cut-left degrees)
  (* (cos (degrees-to-rad degrees)) UPPER-BARRIER))

; Number -> Number
(define (amount-to-cut-top degrees)
  (* (cos (degrees-to-rad degrees)) UPPER-BARRIER))

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


; String Number -> GreenPizzaSlice
; Generate green piece of visual representation of fluid
; progress that can be overlayed on corner background.
(define (green-slice-top-right-partial-fill start-position percent-filled)
  "TODO")

; String Number -> GreenPizzaSlice
; Generate green piece of visual representation of fluid
; progress that can be overlayed on corner background.
(define (green-slice-top-left-partial-fill start-position percent-filled)
  (cond [(string=? start-position "left")
         (trim-left (rotate-ccw TOP-RIGHT-GREEN-SLICE
                                (percent-to-degrees percent-filled))
                    (amount-to-cut-left (percent-to-degrees percent-filled)))]
        [(string=? start-position "top")
         (trim-top (rotate-cw BOTTOM-LEFT-GREEN-SLICE
                              (percent-to-degrees percent-filled))
                   (amount-to-cut-top (percent-to-degrees percent-filled)))]))



; String Number -> Image
(define (pipe-corner-top-right-partial-fill start-position percent-filled)
  (overlay/align "right" "top"
                 TOP-RIGHT-INNER-GRAY
                 (green-slice-top-right-partial-fill start-position percent-filled)
                 PIPE-CORNER-TOP-RIGHT-EMPTY))

; String Number -> Image
(define (pipe-corner-top-left-partial-fill start-position percent-filled)
  (overlay/align "left" "top"
                 TOP-LEFT-INNER-GRAY
                 (green-slice-top-left-partial-fill start-position percent-filled)
                 PIPE-CORNER-TOP-LEFT-EMPTY))




;;; LINES
(beside PIPE-VERTICAL-EMPTY
        WHITESPACE-BREAK
        (pipe-vertical-partial-fill "bottom" 17)
        WHITESPACE-BREAK
        (pipe-vertical-partial-fill "top" 27))

(beside PIPE-HORIZONTAL-EMPTY
        WHITESPACE-BREAK
        (pipe-horizontal-partial-fill "left" 20)
        WHITESPACE-BREAK
        (pipe-horizontal-partial-fill "right" 40))

;;; CORNERS
(beside PIPE-CORNER-TOP-LEFT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-top-left-partial-fill "left" 40)
        WHITESPACE-BREAK
        (pipe-corner-top-left-partial-fill "top" 30))

#;
(beside PIPE-CORNER-TOP-RIGHT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-top-right-partial-fill "top" 70)
        WHITESPACE-BREAK
        (pipe-corner-top-right-partial-fill "right" 80))

#;
(beside PIPE-CORNER-BOTTOM-RIGHT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-bottom-right-partial-fill "bottom" 10)
        WHITESPACE-BREAK
        (pipe-corner-bottom-right-partial-fill "right" 20))

#;
(beside PIPE-CORNER-BOTTOM-LEFT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-bottom-left-partial-fill "bottom" 30)
        WHITESPACE-BREAK
        (pipe-corner-bottom-left-partial-fill "left" 40))
