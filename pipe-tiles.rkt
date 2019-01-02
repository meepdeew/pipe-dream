;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pipe-tiles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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



;;; Corner Sector (quartile)

(define (sector y-place x-place radius color)
  (crop  (cond [(string=? x-place "right") 0]
               [(string=? x-place "left") radius])
         (cond [(string=? y-place "bottom") 0]
               [(string=? y-place "top") radius])
        radius radius
        (circle radius "solid" color)))

(define PIPE-CORNER-BOTTOM-RIGHT-EMPTY
  (overlay/align "right" "bottom"
                 (sector "bottom" "right" LOWER-BARRIER "gray")
                 (sector "bottom" "right" UPPER-BARRIER "black")
                 CELL-BACKGROUND))

(define PIPE-CORNER-TOP-LEFT-EMPTY
  (overlay/align "left" "top"
                 (sector "top" "left" LOWER-BARRIER "gray")
                 (sector "top" "left" UPPER-BARRIER "black")
                 CELL-BACKGROUND))

(define PIPE-CORNER-TOP-RIGHT-EMPTY
  (overlay/align "right" "top"
                 (sector "top" "right" LOWER-BARRIER "gray")
                 (sector "top" "right" UPPER-BARRIER "black")
                 CELL-BACKGROUND))

(define PIPE-CORNER-BOTTOM-LEFT-EMPTY
  (overlay/align "left" "bottom"
                 (sector "bottom" "left" LOWER-BARRIER "gray")
                 (sector "bottom" "left" UPPER-BARRIER "black")
                 CELL-BACKGROUND))



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
  (overlay/align "right" "top"
                 TOP-RIGHT-INNER-GRAY
                 (green-slice-top-right-partial-fill start-position (percent-to-degrees percent-filled))
                 PIPE-CORNER-TOP-RIGHT-EMPTY))

; String Number -> Image
(define (pipe-corner-top-left-partial-fill start-position percent-filled)
  (overlay/align "left" "top"
                 TOP-LEFT-INNER-GRAY
                 (green-slice-top-left-partial-fill start-position (percent-to-degrees percent-filled))
                 PIPE-CORNER-TOP-LEFT-EMPTY))

; String Number -> Image
(define (pipe-corner-bottom-right-partial-fill start-position percent-filled)
  (overlay/align "right" "bottom"
                 BOTTOM-RIGHT-INNER-GRAY
                 (green-slice-bottom-right-partial-fill start-position (percent-to-degrees percent-filled))
                 PIPE-CORNER-BOTTOM-RIGHT-EMPTY))

; String Number -> Image
(define (pipe-corner-bottom-left-partial-fill start-position percent-filled)
  (overlay/align "left" "bottom"
                 BOTTOM-LEFT-INNER-GRAY
                 (green-slice-bottom-left-partial-fill start-position (percent-to-degrees percent-filled))
                 PIPE-CORNER-BOTTOM-LEFT-EMPTY))



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

(beside PIPE-CORNER-TOP-RIGHT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-top-right-partial-fill "top" 34)
        WHITESPACE-BREAK
        (pipe-corner-top-right-partial-fill "right" 26))

(beside PIPE-CORNER-BOTTOM-RIGHT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-bottom-right-partial-fill "bottom" 28)
        WHITESPACE-BREAK
        (pipe-corner-bottom-right-partial-fill "right" 22))

(beside PIPE-CORNER-BOTTOM-LEFT-EMPTY
        WHITESPACE-BREAK
        (pipe-corner-bottom-left-partial-fill "left" 12)
        WHITESPACE-BREAK
        (pipe-corner-bottom-left-partial-fill "bottom" 37))
