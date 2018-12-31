(require 2htdp/image)

(define CELL-WIDTH 50)
(define CELL-BACKGROUND (square CELL-WIDTH "solid" "gray"))

(define PERCENT-FULL 100)
(define PIPE-WIDTH (* CELL-WIDTH 0.2))

(define (linear-pipe-fill-length percent-filled)
  (* CELL-WIDTH (/ percent-filled PERCENT-FULL)))

(define (pipe-horizontal-content percent-filled color)
  (rectangle (linear-pipe-fill-length percent-filled)
             PIPE-WIDTH
             "solid" color))

(define (pipe-vertical-content percent-filled color)
  (rectangle PIPE-WIDTH
             (linear-pipe-fill-length percent-filled)
             "solid" color))

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

(define PIPE-VERTICAL-EMPTY
  (overlay/align "middle" "bottom"
                 (pipe-vertical-content PERCENT-FULL "black")
                 CELL-BACKGROUND))

(define PIPE-HORIZONTAL-EMPTY
  (overlay/align "left" "middle"
                 (pipe-horizontal-content PERCENT-FULL "black")
                 CELL-BACKGROUND))

PIPE-VERTICAL-EMPTY
(pipe-vertical-partial-fill "top" 17)
(pipe-vertical-partial-fill "bottom" 37)

PIPE-HORIZONTAL-EMPTY
(pipe-horizontal-partial-fill "left" 20)
(pipe-horizontal-partial-fill "right" 40)
