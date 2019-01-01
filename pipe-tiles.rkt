(require 2htdp/image)


;;; Constants

(define CELL-WIDTH 100)
(define CELL-BACKGROUND (square CELL-WIDTH "solid" "gray"))

(define LOWER-BARRIER (* CELL-WIDTH 0.4))
(define UPPER-BARRIER (* CELL-WIDTH 0.6))
(define WHITESPACE-BREAK (square 3 "solid" "white"))

(define PERCENT-FULL 100)
(define PIPE-WIDTH (* CELL-WIDTH 0.2))

(define (linear-pipe-fill-length percent-filled)
  (* CELL-WIDTH (/ percent-filled PERCENT-FULL)))



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

#;
(define (pipe-corner-bottom-right-partial-fill start-position percent-filled)
  (overlay/align "right" "bottom"
                 (pipe-corner-bottom-right-content percent-filled "green")
                 (pipe-corner-bottom-right-content PERCENT-FULL "black")
                 CELL-BACKGROUND))



;;; CORNERS
(beside PIPE-CORNER-BOTTOM-RIGHT-EMPTY
        WHITESPACE-BREAK
        PIPE-CORNER-BOTTOM-LEFT-EMPTY)

(beside PIPE-CORNER-TOP-RIGHT-EMPTY
        WHITESPACE-BREAK
        PIPE-CORNER-TOP-LEFT-EMPTY)

;(pipe-corner-bottom-right-partial-fill "bottom" 10)
;(pipe-corner-bottom-right-partial-fill "right" 20)
;(pipe-corner-bottom-left-partial-fill "bottom" 30)
;(pipe-corner-bottom-left-partial-fill "left" 40)

;(pipe-corner-top-left-partial-fill "top" 50)
;(pipe-corner-top-left-partial-fill "left" 60)
;(pipe-corner-top-right-partial-fill "top" 70)
;(pipe-corner-top-right-partial-fill "right" 80)



;;; LINES
(beside
 PIPE-VERTICAL-EMPTY
 WHITESPACE-BREAK
 (pipe-vertical-partial-fill "top" 17)
 WHITESPACE-BREAK
 (pipe-vertical-partial-fill "bottom" 37))

(beside
 PIPE-HORIZONTAL-EMPTY
 WHITESPACE-BREAK
 (pipe-horizontal-partial-fill "left" 20)
 WHITESPACE-BREAK
 (pipe-horizontal-partial-fill "right" 40))

;
;;(rotate -45 (crop 40 40 40 40 (circle 40 "solid" "red")))
;(crop 40 40 40 40 (circle 40 "solid" "red"))
;(rotate -45 (crop 40 40 40 40 (circle 40 "solid" "red")))
;(crop 0 0
;      (/ (image-width (rotate -45 (crop 40 40 40 40 (circle 40 "solid" "red")))) 2)
;      (image-height (rotate -45 (crop 40 40 40 40 (circle 40 "solid" "red"))))
;      (rotate -45 (crop 40 40 40 40 (circle 40 "solid" "red"))))
;(image-width (rotate -45 (crop 40 40 40 40 (circle 40 "solid" "red"))));57
;(image-width (crop 40 40 40 40 (circle 40 "solid" "red")));40


#;
(define (crop-half side param)
  (crop (cond [(string=? side "left") (/ (image-width param) 2)]
              [(string=? side "right") 0]) 0
      (/ (image-width param) 2)
      (image-height param)
      param))

(define (rotate-and-crop degrees)
  (rotate degrees (crop UPPER-BARRIER UPPER-BARRIER
                  UPPER-BARRIER UPPER-BARRIER
                  (circle UPPER-BARRIER "solid" "green"))))

PIPE-CORNER-TOP-LEFT-EMPTY

"top-left-corner rotated"

(overlay/align/offset
 "left" "top"
 (rotate-and-crop -45)
 21 0
 PIPE-CORNER-TOP-LEFT-EMPTY)

(define (degrees-to-rad degrees)
  (* degrees (/ pi 180)))

(define (x-displacement degrees)
  (* (sin (degrees-to-rad (abs degrees))) UPPER-BARRIER))

;;; TODO: works from 0 to -90 but
;;; make top-left specific
;;; (from left or top, and use percent instead of degrees)
(define (overlay-at-angle degrees)
  (overlay/align
   "left" "top"
   (line (x-displacement degrees) 0 "red")

   (overlay/align/offset
    "left" "top"
    (rotate-and-crop degrees)
    (x-displacement degrees)
    0
    PIPE-CORNER-TOP-LEFT-EMPTY)))

"test runs"
(define (test-tile deg)
  (x-displacement deg)
  (overlay-at-angle deg))

(x-displacement 0)
(overlay-at-angle 0)

(x-displacement -15)
(overlay-at-angle -15)

(x-displacement -30); 15
(overlay-at-angle -30)

(x-displacement -45)
(overlay-at-angle -45)

(x-displacement -60); 26
(overlay-at-angle -60)

(x-displacement -75)
(overlay-at-angle -75)

(x-displacement -90)
(overlay-at-angle -90)
