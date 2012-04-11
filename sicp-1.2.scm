; Exercise 1.9
; --------------------------------------
(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (first+ a b)
  (if (= a 0)
      b
      (inc (first+ (dec a) b))))

; Substituting each recursive call to + with the resultant call to inc shows that this procedure is
; essentially incrementing b, a times.  We can also see that the process is recursive, as the call stack is
; required to keep track of how many times we increment b.
; (first+ 4 5)
; (inc (+ (dec 4) 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (second+ a b)
  (if (= a 0)
      b
      (second+ (dec a) (inc b))))

; Each syntactically recursive call to second+ is simply restating the problem with different arguments that
; will yield the correct solution.  As a result, the process does not require a call stack to keep track of
; what is required to return an answer in the same way that first+ does.  Therefore, this process is
; iterative and not recursive.
; (second+ 4 5)
; (second+ (dec 4) (inc 5))
; (second+ 3 6)
; (second+ (dec 3) (inc 6))
; (second+ 2 7)
; (second+ (dec 2) (inc 7))
; (second+ 1 8)
; (second+ (dec 1) (inc 8))
; (second+ 0 9)
; 9
