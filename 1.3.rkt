#lang racket
(require racket/trace)
;;; ex. 1.29
;integral from the book
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (add-h y) (+ h y))
    (define (add-2h y) (+ (* 2 h) y))
    (* (/ h 3)
       (- (+ (* 2 (sum f a add-h b))
             (* 2 (sum f (+ a h) add-2h (- b h))))
          (+ (f a) (f b))))))

;(integral cube 0 1 0.01)
;(simpson cube 0 1. 100)
;(integral cube 0 1 0.001)
;(simpson cube 0 1. 1000)

;;; ex.1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))
(define (inc x) (+ x 1))
;(sum cube 1 inc 10)
;(sum-iter cube 1 inc 10)

;;; ex.1.31
; product
(define (id x) x)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; factorial through product
(define (fact-alt n)
  (product id 1 inc n))
(trace fact-alt)
(fact-alt 5)
(define (fact x)
  (if (<= x 1)
      1
      (* x (fact (- x 1)))))
;(trace fact)
;(fact 5)
; pi estimate through product
(define (num i)
  (+ (* (ceiling (/ i 2))
        2)
     2.0))
(define (denom i)
  (+ (* (floor (/ i 2))
        2)
     3.0))
(define (pi-term i)
  (/ (num i) (denom i)))
(define (pi-est n)
  (* (product pi-term 0 inc n)
     4))
;(pi-est 10)
;(pi-est 100)
;(pi-est 1000)
;(pi-est 10000)

;;; ex.1.32
;recursive
(define (accum combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accum combiner null-value term (next a) next b))))
;(accum + 0 id 1 inc 10)
;(accum * 1 id 1 inc 5)

;iterative
(define (accum-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
;(accum-iter + 0 id 1 inc 10)
;(accum-iter * 1 id 1 inc 5)

;;; ex.1.33
(define (sq n) (* n n))

(define (filtered-accumulate combiner null-value term a next b condition)
  (cond ((> a b) null-value)
        ((condition a) (combiner (term a)
                                 (filtered-accumulate combiner null-value term (next a) next b condition)))
        (else (filtered-accumulate combiner null-value term (next a) next b condition))))
;(filtered-accumulate + 0 sq 1 inc 5 even?) ;20

; sum squares of even numbers
(define (sum-sq-even a b)
  (filtered-accumulate + 0 sq a inc b even?))
;(sum-sq-even 1 10) ;220
;(sum-sq-even 3 7) ;52

;;; ex.1.34
(define (f g)
  (g 2))
;(f f) ;throws an error because it will eventually expand to (2 2) and 2 is not a procedure.

;;; ex.1.42
(define (compose f g)
  (lambda (x) (f (g x))))
;((compose sq inc) 6)

;;; ex.1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
;((repeated sq 2) 5) ;625

;;; ex.1.44
(define (average-3 x y z)
  (/ (+ x y z) 3.))
(define (smoothed f dx)
  (lambda (x) (average-3 (f (- x dx)) (f x) (f (+ x dx)))))
((smoothed sq 1) 3)
(define (smoothed-n-fold f dx n)
  (lambda (x)
    (if (= n 1)
         ((smoothed f dx) x)
         (average-3 ((smoothed-n-fold f dx (- n 1)) (- x dx))
                    ((smoothed-n-fold f dx (- n 1)) x)
                    ((smoothed-n-fold f dx (- n 1)) (+ x dx))))))
;(trace smoothed-n-fold)
;((smoothed-n-fold sq 1 5) 0)
;;; ex.1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess) (if (good-enough? guess)
                      guess
                      ((iterative-improve good-enough? improve) (improve guess)))))
(define (sq-good? x)
  (lambda (guess) (< (abs (- (sq guess) x)) 0.001)))
(define (improve-sq x)
  (lambda (guess) (/ (+ guess (/ x guess)) 2.)))
;((iterative-improve (sq-good? 2.) (improve-sq 2.)) 1)
