;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 09, Problem Bonus
;; ***************************************************
;;

(define (solution? f lon)
  (local
    [(define (solution?/acc f lst acc)
       (cond
         [(empty? lst) true]
         [(= (first lst) (first acc))
          (solution?/acc f (rest lst) (rest acc))]
         [else false]))]
    (cond
      [(empty? lon) true]
      [else
       (solution?/acc f lon (build-list (length lon) f))])))

;;---------------------------------------------------------------------------------------

(define (guess-quadratic lon)
  (local
    [(define (calculate-quadratic lst)
       (local
         [(define c (first lst))
          (define a (/ (+ (third lst) (* -2 (second lst)) c) 2))
          (define b (- (second lst) a c))]
         (lambda (x) (+ (* a (sqr x)) (* b x) c))))]
    (cond
      [(empty? lon) (lambda (x) 0)]
      [(= 1 (length lon))
       (lambda (x) (first lon))]
      [(= 2 (length lon))
       (lambda (x) (+ (* x (- (second lon) (first lon))) (first lon)))]
      [else (calculate-quadratic lon)])))


(define (try-quadratic lon)
  (cond
    [(solution? (guess-quadratic lon) lon) (guess-quadratic lon)]
    [else empty]))

;;---------------------------------------------------------------------------------------

  (define (guess-recursive lon)
    (local
      [(define (b lst)
         (cond
           [(or (and (= (third lst) 0) (= (fourth lst) 0))
                (and (= (third lst) 0) (= (second lst) 0) )) 0]
           [(= (third lst) 0) (/ (fourth lst) (second lst))]
           [(= 0 (- (sqr (second lst)) (* (third lst) (first lst)))) (first lst)]
           [else
            (/ (- (sqr (third lst)) (* (fourth lst) (second lst)))
                    (- (* (third lst) (first lst)) (sqr (second lst))))]))
       (define (a lst)
         (cond
           [(or (and (= (third lst) 0) (= (fourth lst) 0))
                (and (= (second lst) 0) (= (third lst) 0))) 0]
           [(= (third lst) 0) (/ (first lst) (* -1 (second lst)))]
           [else (/ (- (fourth lst) (* (b lst) (second lst))) (third lst))]))
       (define (fast-fib/gen i term-num term-i-2 term-i-1)
         (cond
           [(= i 0) (first lon)]
           [(= i 1) (second lon)]
           [(or (= term-num 0) (= term-num 1))
            (fast-fib/gen i (add1 term-num) (first lon) (second lon))]
           [(= term-num i) (+ (* (a lon) term-i-1) (* (b lon) term-i-2))]
           [else
            (fast-fib/gen i (add1 term-num) term-i-1 (+ (* (a lon) term-i-1) (* (b lon) term-i-2)))]))]
(cond
  [(empty? lon) (lambda (x) 0)]
  [(= (length lon) 1)
   (guess-recursive (append lon (list (first lon)
                                      (* 2 (first lon)) (* 3 (first lon)))))]
  [(= (length lon) 2)
   (guess-recursive (append lon (list (+ (first lon) (second lon))
                                      (+ (first lon) (* 2 (second lon))))))]
  [(= (length lon) 3)
   (guess-recursive (append lon (list (+ (* 2 (third lon)) (* 1 (second lon))))))]
  [else
   (lambda (x) (fast-fib/gen x 0 (first lon) (second lon)))])))


(define (try-recursive lon)
  (cond
    [(solution? (guess-recursive lon) lon) (guess-recursive lon)]
    [else empty]))

;;---------------------------------------------------------------------------------------


(define (guess-linear lon)
  (local
    [(define (calculate-linear lst)
       (local
         [(define b (first lst))
          (define a (- (second lst) (first lst)))]
         (lambda (x) (+ (* a x) b))))]
    (cond
      [(empty? lon) (lambda (x) 0)]
      [(= (length lon) 1) (lambda (x) (first lon))]
      [else (calculate-linear lon)])))

(check-expect (solution? (guess-linear (list 2 4)) (list 2 5)) false)
(check-expect (solution? (guess-linear (list 2 4)) (list 2)) true)
(check-expect (solution? (guess-linear (list 2 4)) (list 1 4)) false)
(check-expect (solution? (guess-linear (list 4)) (list 4)) true)
(check-expect (solution? (guess-linear (list 2 4 5)) (list 2 4 5)) false)

(define (try-linear lon)
  (cond
    [(solution? (guess-linear lon) lon) (guess-linear lon)]
    [else empty]))


;;---------------------------------------------------------------------------------------

(define (guess-cubic lon)
  (local
    [(define (calculate-cubic lst)
       (local
         [(define d (first lst))
          (define a (/ (+ (fourth lst) (* 3 (second lst))
                          (* -3 (third lst)) (* -1 (first lst))) 6))
          (define b (/ (+ (* 4 (third lst)) (* -5 (second lst))
                          (* 2 (first lst)) (* -1 (fourth lst))) 2))
          (define c (- (second lst) d a b))]
         (lambda (x) (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d))))]
    (cond
      [(empty? lon) (lambda (x) 0)]
      [(= 1 (length lon))
       (calculate-cubic (append lon (list 16 64 128)))]
      [(= 2 (length lon))
       (calculate-cubic (append lon (list 1 5)))]
      [(= 3 (length lon))
       (calculate-cubic (append lon (list 1)))]
      [else (calculate-cubic lon)])))

(define (try-cubic lon)
  (cond
    [(solution? (guess-cubic lon) lon)(guess-cubic lon)]
    [else empty]))

(check-expect ((try-cubic '()) empty) 0)
(check-expect ((try-cubic '(1)) 3) 128)
(check-expect (try-cubic '(9 23 73 189 401 735)) empty)


;;---------------------------------------------------------------------------------------

(define (supersolve lon)
  (cond
    [(solution? (guess-linear lon) lon)
     (try-linear lon)]
    [(solution? (guess-quadratic lon) lon)
     (try-quadratic lon)]
    [(solution? (guess-cubic lon) lon)
     (try-cubic lon)]
    [(solution? (guess-recursive lon) lon)
     (try-recursive lon)]
    [else empty]))

(check-expect ((supersolve '(1 2 3 4 5 6 7 8)) 10) 11)
(check-expect ((supersolve '(9 23 73 189 401 739)) 10) 5369)
(check-expect ((supersolve '(2/3 2 6 18 54)) 5) 162)
(check-expect ((supersolve '(1 4 15 40 85)) 3) 40)
(check-expect ((supersolve '(2 3 10 29 66 127 218 345)) 11) 1333)
