; List-of-Anything is one of:
; - (cons Any empty)
; - (cons Any List-of-Anything)

(check-error (add-to-end 0 empty))
(check-expect (add-to-end 6 (cons 5 empty))
              (cons 5 (cons 6 empty)))
(check-expect (add-to-end 3 (cons 1 (cons 2 empty)))
              (cons 1 (cons 2 (cons 3 empty))))

; List-of-Anything Any -> List-of-Anything
; add element to end of list

(define (add-to-end item listo)
  (reverse-copy (cons item (reverse-copy listo))))

;(add-to-end 2 (cons 5 (cons 4 (cons 3 empty))))
;->(cons 3 (cons 4 (cons 5 empty)
;->(cons 2 (3 (cons 4 (cons 5 empty))))
;->(cons 5 (cons 4 (cons 3 (cons 2 empty))))


(check-expect (reverse-copy (cons 1 (cons 2 empty)))
              (cons 2 (cons 1 empty)))
(check-expect (reverse-copy (cons 3 (cons 4 (cons 5 empty))))
              (cons 5 (cons 4 (cons 3 empty))))

; List-of-Anything -> List-of-Anything

(define (reverse-copy lst)
  (move-from-1-to-2 lst empty))

(check-expect (move-from-1-to-2 (cons 4 (cons 5 empty)) empty)
              (cons 5 (cons 4 empty)))
(check-expect (move-from-1-to-2 (cons 1 (cons 2 (cons 3 empty))) empty)
              (cons 3 (cons 2 (cons 1 empty))))

(define (move-from-1-to-2 list1 list2)
  (cond [(empty? (rest list1))
         (cons (first list1) list2)]
        [else
         (move-from-1-to-2
          (rest list1)
          (cons (first list1) list2))]))

(check-expect
 (front-to-end (cons "red" (cons "orange" (cons "yellow" (cons "green" (cons "blue" '()))))))
 (cons "orange" (cons "yellow" (cons "green" (cons "blue" (cons "red" '()))))))
; List-of-Anything -> List-of-Anything
; move the front-most item in a list the end.
(define (front-to-back lst)
  (add-to-end (first lst) (rest lst)))

(define things
  (cons "red" (cons "orange" (cons "yellow" (cons "green" (cons "blue" empty))))))
things
(front-to-back things)
