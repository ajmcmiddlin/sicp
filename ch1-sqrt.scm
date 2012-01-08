#lang racket

(define (is-guess-close-enough guess old-guess max-pc-change)
  (< (* (abs (- 1.0 (/ old-guess guess))) 100.0) max-pc-change))

; Calculates the next square root guess given the previous guess and the radicand (number we're trying to find
; the square root for).  It does this by finding the intercept of the tangent at the old guess and the x axis.
(define (improve-sqrt-guess old-guess radicand)
  (/ (+ old-guess (/ radicand old-guess)) 2.0))

; Calculates an approximation of the square root of n; returning when the percentage change between guesses is
; less than max-pc-change.
(define (sqrt-pc-change n guess old-guess max-pc-change)
  (if (is-guess-close-enough guess old-guess max-pc-change)
      guess
      (sqrt-pc-change n (improve-sqrt-guess guess n) guess max-pc-change)))

(define (sqrt n)
  (sqrt-pc-change n (improve-sqrt-guess 1.0 n) 1.0 1.0))