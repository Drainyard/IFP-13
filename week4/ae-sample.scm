;;; week-03_arithmetic-expressions-sample.scm
;;; IFP 2016-2016, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 12 Sep 2016

;;;;;;;;;;

(define source-ae0
  '42)

(define source-ae1
  '(+ 1 10))

(define source-ae2
  '(+ (+ 1 10)
      (+ 100 1000)))

(define source-ae3
  '(* (* (* 1 2)
         3)
      (* 4 5)))

(define source-ae4
 '(+ (+ 0 1) (+ 2 3)))

(define source-ae5
  '(+ (+ 0 1) (+ (* 2 3) (* 4 5))))

(define source-ae6
  '(+ (+ 0 1) (+ (+ (* (* 2 3) (* 4 5)) 6) (+ 7 8))))

(define source-ae7
  '(+ (+ 0 1) (+ (+ (* (* 2 (+ (+ 3 33) 333)) (* 4 5)) 6) (+ 7 8))))

(define source-ae8
  '(+ (* 1 10) (* 2 20)))

(define source-ae9
  '(+ (* 1 10) (* 2 20)))

(define source-ae10
  '(* (* (* 1 2)
         0)
      (* 4 5)))

(define source-ae11
  '(* (* (* 1 1)
         1)
      (* 1 1)))

(define source-ae12
  '(+ (* 0 1) (+ (+ (* (* 2 (+ (+ 3 33) (+ 0 333))) (* 4 5)) 6) (+ 7 8))))

(define source-ae12v2
  '(+ (* 0 1) (+ 1 333)))

(define source-ae13
  '(+ (* 1 0) (+ (+ (* (* 2 (+ (+ 3 33) (+ 333 0))) (* 4 5)) 6) (+ 7 8))))

(define source-ae14
  '(* (+ 2 20)
      (+ 200 2000)))

(define source-ae15
  '(* (+ 1 10)
      (+ 100 1000)))

;;; optimizing aes bizarre:

(define source-ae16
  '(+ (+ 0 1) (+ 0 1)))

(define source-ae17
  '(+ (+ 0 1) (+ (+ 0 1) (+ 0 1))))

(define source-ae18
  '(+ (+ (+ (+ (+ 0 1) 1) 1) 1) 1))

(define source-ae19
  '(* (* 1 1) 1))

(define source-ae20
  '(* (* (* (* 1 1) 1) 1) 1))

(define source-ae21
  '(* (* (* (+ 1 1) 1) 1) 1))

(define source-ae22
  '(* (* (* (+ 1 1) (+ 1 1)) 1) 1))

(define source-ae23
  '(* (* (* (+ 1 1) (+ 1 1)) (+ (+ (+ (+ 0 1) 0) 0) 1)) 1))

(define source-ae24
  '(+ 0 1))

(define source-ae25
  '(+ 1 (+ 1 (+ 1 (+ 1 (+ 1 1))))))

;;;;;;;;;;

(define sample-of-arithmetic-expressions
  (list source-ae0
        source-ae1
        source-ae2
        source-ae3
        source-ae4
        source-ae5
        source-ae6
        source-ae7
        source-ae8
        source-ae9
        source-ae10
        source-ae11
        source-ae12
        source-ae13
        source-ae14
        source-ae15
        source-ae16
        source-ae17
        source-ae18
        source-ae19
        source-ae20
        source-ae21
        source-ae22
        source-ae23
        source-ae24
        source-ae25))

;;;;;;;;;;

;;; end of week-03_arithmetic-expressions-sample.scm

"ae-sample.scm"
