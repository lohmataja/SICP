#lang planet neil/sicp
(define (sq x) (* x x))
;;; ex. 1.3
(define (sum-big-squares a b c)
  (if (> a b)
      (if (> b c) (+ (sq a) (sq b))
          (+ (sq a) (sq c)))
      (if (> a c) (+ (sq b) (sq a))
          (+ (sq b) (sq c)))))
;(sum-big-squares 1 5 -3)

;;; ex. 1.7
; initial sqrt:
(define (good-enough? guess x)
  (< (abs (- (sq guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
; new
(define (new-good-enough? guess x)
  (< (abs (- (sq guess) x)) (* 0.001 x)))
(define (compar-good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) (* 0.001 old-guess)))
(define (rt-iter improve-func guess x)
  (let ((new-guess (improve-func guess x)))
    (if (compar-good-enough? guess new-guess)
         new-guess
         (rt-iter improve-func new-guess x))))
;(newline)
;(display (sqrt 2))
;(newline)
;(display (sqrt-iter 1.0 2))
;(newline)
;(display (rt-iter improve 1.0 2))

;;; ex.1.8 (debug: doesn't work with negative numbers)
(define (improve-cube y x)
  (/ (+ (/ x (sq y)) (* 2 y)) 3))
(define (cuberoot x) (rt-iter improve-cube 1.0 x))
;(newline)
;(display (cuberoot 1000))
;(newline)
;(display (cuberoot 0.008))
;(newline)
;(display (cuberoot -125))

;;; ex. 1.10
; (A 0 n) == 2n
; (A 1 n) == 2^n
; (A 2 n) == 2^2^... (n times)

;;; ex. 1.11
; recursive definition
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
;(display (f 1)) ;1
;(newline)
;(display (f 2)) ;2
;(newline)
;(display (f 3)) ;4
;(newline)
;(display (f 4)) ;11
;(newline)
;(display (f 5)) ;25
;(newline)
;(display (f 6)) ;59
;(newline)

; iterative definition: needs debugging
(define (f-iter n)
  (define (f-i cur n f-1 f-2 f-3)
    (let ((f-cur (+ f-1 (* 2 f-2) (* 3 f-3))))
      (cond ((< n 3) n)
            ((= cur (- n 1)) f-cur)
            (else (f-i (+ cur 1) n f-cur f-2 f-3)))))
  (f-i 2 n 2 1 0))
;(display (f-iter 1)) ;1
;(newline)
;(display (f-iter 2)) ;2
;(newline)
;(display (f-iter 3)) ;4
;(newline)
;(display (f-iter 4)) ;11
;(newline)
;(display (f-iter 5)) ;25
;(newline)
;(display (f-iter 6)) ;59
;(newline)

;;; ex.1.12
(define (pascal row col)
  (cond ((or (< row 1) (< col 1) (> col row)) 0)
        ((or (= row 1) (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1) col)
           (pascal (- row 1) (- col 1))))))
;(display (pascal 1 1))
;(newline)
;(display (pascal 3 2))

;;; ex.1.15
(define (pow b n)
  (define (pow-iter b n accum)
    (cond ((= b 0) 0)
          ((or (= b 1) (= n 0)) 1)
          ((= n 1) (* b accum))
          (else (pow-iter (sq b) (floor (/ n 2)) (* accum (max 1 (* b (modulo n 2))))))))
  (pow-iter b n 1))
;(display (pow 2 2))
;(newline)
;(display (pow 2 3))
;(else (pow-iter b (floor (/ n 2)) (* accum (sq b) (max 1 (* b (modulo n 2))))

;;; ex.1.16
