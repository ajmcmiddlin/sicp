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


; Exercise 1.10
; ----------------------------------------
(define (Ackermann x y)
	(cond ((= y 0) 0)
				((= x 0) (* 2 y))
				((= y 1) 2)
				(else (Ackermann (- x 1)
												 (Ackermann x (- y 1))))))

; (Ackermann 1 10)
; (Ackermann 0 (Ackermann 1 9))
; (Ackermann 0 (Ackermann 0 (Ackermann 1 8)))
; ...
; The pattern above continues so that each substitution results in another recursive call to Ackermann, where
; the arguments are 0 and (Ackermann 1 (- y 1)) until y becomes 1.  As n decrements by one each time we
; perform a substitution, there will be (- n 1) recursive calls on the stack where 0 is the first argument,
; and the second argument of the final call will be (A 1 1).
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; 
; At this point, (Ackermann 1 1) returns 2, leaving us with the list of recursive calls below.  
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;
; Each call to the Ackermann function where the first argument is 0 returns (* 2 y), therefore the list of
; recursive calls has the effect of multiplying the second argument of the last call by 2^(n-1).  As the final
; argument will always be 2 when the first argument of the original call is 1 (we add calls until we get to (A
; 1 1)), any call to the Ackermann function that takes the form (A 1 n) will return 2^n.  Therefore the final
; answer is 2^10 = 1024


; (Ackermann 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; We know that each call to the Ackermann function that takes the form (A 1 n) equals 2^n, so we can progress
; as follows
; 2  ^  (2  ^ (2^2))
; 2^(2^4)
; 2^16 = 65536


; (Ackermann 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 4)
; 65536


; returns 2n
; (define (f n) (Ackermann 0 n))
;
; returns 2^n
; (define (g n) (Ackermann 1 n))
;
; returns 2^2^2... n times - e.g. (h 5) -> 2^2^2^2^2
; (define (h n) (Ackermann 2 n))


; Section 1.2.2 - counting change
; -------------------------------
; 
; Copy the tree-recursive algorithm in the book to see how long it takes.
(define (count-change amount)

  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))

  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

  (cc amount 5))


; Exercise 1.11
; -------------
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iter n)

  ; Internal iteration function
  ;   n: the value we are calculating the function for
  ;   current-n: the value of n that this iteration should evalute f(n) for
  ;   fn-1: the value of f(current-n - 1)
  ;   fn-2: the value of f(current-n - 2)
  ;   fn-3: the value of f(current-n - 3)
  (define (f-internal-iter n current-n fn-1 fn-2 fn-3)
    (if (> current-n n)
        fn-1
        (f-internal-iter n
                         (+ current-n 1)
                         (+ fn-1 (* 2 fn-2) (* 3 fn-3))
                         fn-1
                         fn-2)))

  (if (< n 3)
      n
      (f-internal-iter n 3 2 1 0)))


; Exercise 1.12
; -------------
; pascal returns the number in Pascal's triangle given its row and column (both starting at 1)
(define (pascal row col)
  (cond ((< row 3) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))
