;;; week-07_fibonacci.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 17 Oct 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-07_possible-term-projects.html

;;;;;;;;;;;;;;;;;;;;
(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))


(define square
  (lambda (x)
    (* x x)))

;;;;;;;;;;;;;;;;;;;;

(define test-fib
  (lambda (candidate)
    (and (= (candidate 0) 0)
         (= (candidate 1) 1)
         (= (candidate 2) 1)
         (= (candidate 3) 2)
         (= (candidate 4) 3)
         (= (candidate 5) 5)
         (= (candidate 6) 8)
         (= (candidate 7) 13)
         (= (candidate 8) 21)
         (= (candidate 9) 34)
         (= (candidate 10) 55)
         (= (candidate 11) 89)
         (= (candidate 12) 144)
         (= (candidate 20) 6765)
         ;; (= (candidate 42) 267914296)    Takes too long for v0
         ;;; etc.
         )))

(define fib_v0
  (lambda (n_init)
    (letrec ([visit (lambda (n)
                      (cond
                        [(= n 0)
                         0]
                        [(= n 1)
                         1]
                        [else
                         (+ (visit (- n 1)) (visit (- n 2)))]))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (visit n_init)
          (errorf 'fib_v0
                  "Not a non-negative integer: ~s"
                  n_init)))))

(unless (test-fib fib_v0)
  (printf "(test-fib fib_v0) failed~n"))


;;;;;;;;;; Linear

(define fib_lin1
  (lambda (n_init)
    (letrec ([visit (lambda (n a1 a2)
                      (cond
                        [(= n 2)
                         (+ a1 a2)]
                        [else
                         (visit (- n 1) a2 (+ a1 a2))]))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (cond 
            [(= n_init 0)
             0]
            [(= n_init 1)
             1]
            [else
             (visit n_init 0 1)])
          (errorf 'fib_lin1
                  "Not a non-negative integer: ~s"
                  n_init)))))

;;; This first linear version of fib uses two accumulators, which count the 
;;; Fibonacci numbers upwards while n is decremented, so that they match 
;;; fib(n-2) and fib(n-1) when n is down to 2. The cases of n being 0 and 1 are
;;; handled before the visit call, since they would otherwise be conditional 
;;; statements that are checked in every iteration of visit even though they
;;; only match at most once in the very beginning, in which case a visit call
;;; is unnecessary.
;;; The procedure has linear complexity (complexity being measured as the number
;;; of visit calls) because there is one visit call every time n is decremented,
;;; which result in (fewer than) n visit calls overall.

(unless (test-fib fib_lin1)
  (printf "(test-fib fib_lin1) failed~n"))

;;; linear take 2:

(define fib_lin2
  (lambda (n_init)
    (letrec ([visit (lambda (n)
                      (cond
                        [(= n 2)
                         (values 0 1)]
                        [else
                         (let-values ([(f2 f1) (visit (- n 1))])
                           (values f1 (+ f2 f1)))]))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (cond 
            [(= n_init 0)
             0]
            [(= n_init 1)
             1]
            [else
             (let-values ([(r2 r1) (visit n_init)])
               (+ r2 r1))])
          (errorf 'fib_lin2
                  "Not a non-negative integer: ~s"
                  n_init)))))


;;; This version is like the first linear one only with let-values instead of 
;;; accumulators. 
;;; The same arguments both for the two preliminary checks and the complexity
;;; apply.

(unless (test-fib fib_lin2)
  (printf "(test-fib fib_lin2) failed~n"))


;;;;;;;;;; Logarithmic
;;; The given matrix:
;;;    _    _
;;;   |      |
;;;   | 0  1 |
;;;   |      |
;;;   | 1  1 |
;;;   |_    _|
;;;
;;; has the property that the entries correspond to Fibonacci numbers in the
;;; following way:
;;;    _                  _
;;;   |                    |
;;;   | fib(n-1)  fib(n)   |
;;;   |                    |
;;;   | fib(n)    fib(n+1) |
;;;   |_                  _|
;;;
;;; This property holds when exponentiating the matrix and can be proven by 
;;; induction in n. 
;;; This property means we can compute the n'th Fibonacci number by raising the
;;; base matrix to the power (n-1) and returning entry (1, 1), i.e. the bottom
;;; right entry.
;;; Given the hint that matrices can be exponentiated in logarithmic time, this
;;; means the Fibonacci number can be computed in logarithmic time. 

;;; To start with we need a way of representing 2x2 matrices and their 
;;; multiplication. 
;;; Thinking of a 2x2 matrix as a list of two pairs, where each pair represents
;;; a row of the matrix, multiplication can be done as follows:
;;; Given two 2x2 matrices:
;;;            A = ((a00 . a01) (a10 . a11)) 
;;;            B = ((b00 . b01) (b10 . b11))
;;; Their product will be:
;;;           AB = (((+ (* a00 b00) (* a01 b10)) . (+ (* a00 b01) (* a01 b11)))
;;;                 ((+ (* a10 b00) (* a11 b10)) . (+ (* a10 b01) (* a11 b11))))
;;;

;;; In the remaining comments, whenever we refer to a matrix, it is implicitly a
;;; 2x2 matrix.


;;; First some predicates:

(define pair-of-numbers?
  (lambda (p)
    (and (pair? p)
         (number? (car p))
         (number? (cdr p)))))

(define 2x2-matrix?
  (lambda (m)
    (and (proper-list-of-given-length? m 2)
         (pair-of-numbers? (car m))
         (pair-of-numbers? (cadr m)))))

;;; Next a constructor:

(define make-2x2-matrix
  (lambda (x00 x01 x10 x11)
    (list (cons x00 x01)
          (cons x10 x11))))

;;; And a few accessors:

(define x00
  (lambda (m)
    (caar m)))

(define x01
  (lambda (m)
    (cdar m)))

(define x10
  (lambda (m)
    (caadr m)))

(define x11
  (lambda (m)
    (cdadr m)))

;;; Of course a unit test for matrix multiplication:
 
(define test-matrix-mul
   (lambda (candidate)
     (and (equal? (candidate (make-2x2-matrix 0 0 0 0) 
                             (make-2x2-matrix 0 0 0 0))
                  (make-2x2-matrix 0 0 0 0))
          (equal? (candidate (make-2x2-matrix 0 0 0 0) 
                             (make-2x2-matrix 1000 40 20 30))
                  (make-2x2-matrix 0 0 0 0))
          (equal? (candidate (make-2x2-matrix 1000 40 20 30)
                             (make-2x2-matrix 0 0 0 0))
                  (make-2x2-matrix 0 0 0 0))
          (equal? (candidate (make-2x2-matrix 1 1 1 1) 
                             (make-2x2-matrix 1 1 1 1))
                  (make-2x2-matrix 2 2 2 2))
          (equal? (candidate (make-2x2-matrix 0 1 1 1) 
                             (make-2x2-matrix 0 1 1 1))
                  (make-2x2-matrix 1 1 1 2))
          (equal? (candidate (make-2x2-matrix 1 1 1 2) 
                             (make-2x2-matrix 0 1 1 1))
                  (make-2x2-matrix 1 2 2 3))
         ;;; etc.
         )))

;;; Matrix multiplication:

(define matrix-mul
  (lambda (m1 m2)
    (if (and (2x2-matrix? m1)
             (2x2-matrix? m2))
        (list (cons (+ (* (x00 m1) (x00 m2)) (* (x01 m1) (x10 m2)))
                    (+ (* (x00 m1) (x01 m2)) (* (x01 m1) (x11 m2))))
              (cons (+ (* (x10 m1) (x00 m2)) (* (x11 m1) (x10 m2)))
                    (+ (* (x10 m1) (x01 m2)) (* (x11 m1) (x11 m2)))))
        (errorf 'matrix-mul
                "Not a proper 2x2 matrix: ~s or ~s"
                m1 m2))))


(unless (test-matrix-mul matrix-mul)
  (printf "fail: (test-matrix-mul matrix-mul) ~n"))

;;; We will be using this matrix quite a lot:

(define fib-base-matrix
  (make-2x2-matrix 0 1 1 1))

;;; A unit test for matrix exponentiation

(define test-matrix-exp
  (lambda (candidate)
    (and (equal? (candidate (make-2x2-matrix 0 0 0 0) 0)
                 (make-2x2-matrix 1 0 0 1))
         (equal? (candidate (make-2x2-matrix 0 0 0 0) 1)
                 (make-2x2-matrix 0 0 0 0))
         (equal? (candidate (make-2x2-matrix 0 0 0 0) 3)
                 (make-2x2-matrix 0 0 0 0))
         (equal? (candidate (make-2x2-matrix 10 10 10 10) 0)
                 (make-2x2-matrix 1 0 0 1))
         (equal? (candidate fib-base-matrix 1)
                 fib-base-matrix)
         (equal? (candidate fib-base-matrix 2)
                 (make-2x2-matrix 1 1 1 2))
         (equal? (candidate fib-base-matrix 4)
                 (make-2x2-matrix 2 3 3 5))
         )))

;;; The naive implementation of matrix exponentiation:

(define matrix-exp_naive
  (lambda (m e)
    (if (2x2-matrix? m)
        (if (and (number? e)
                 (>= e 0))
            (letrec ([visit (lambda (n)
                              (cond
                                [(= n 1)
                                 m]
                                [else 
                                 (matrix-mul m (visit (1- n)))]))])
              (if (= e 0)
                  (make-2x2-matrix 1 0 0 1)
                  (visit e)))
            (errorf 'matrix-exp_naive
                    "Not a non-negative integer: ~s"
                    e))
        (errorf 'matrix-exp_naive
                "Not a proper 2x2 matrix: ~s"
                m))))

;;; This way of doing matrix exponentiation runs in linear time due to the one
;;; visit call every time n is decremented.

(unless (test-matrix-exp matrix-exp_naive)
  (printf "fail: (test-matrix-exp matrix-exp_naive) ~n"))

;;; fib with naive matrix exponentiation:

(define fib_matrix_naive
  (lambda (n)
    (if (and (integer? n)
             (not (negative? n)))
        (cond
          [(= n 0)
           0]
          [(= n 1)
           1]
          [else
           (x11 (matrix-exp_naive fib-base-matrix (1- n)))])
        (errorf 'fib_matrix_naive
                "Not a non-negative integer: ~s"
                n))))


;;; This version runs in linear time, since the matrix exponentiation takes 
;;; linear time. 

(unless (test-fib fib_matrix_naive)
  (printf "fail: (test-fib fib_matrix_naive) ~n"))


;;; Now to the more efficient matrix exponentiation:

(define matrix-exp
  (lambda (m e)
    (letrec ([visit (lambda (i)
                      (if (even? i)
                          (if (= i 0)
                              (make-2x2-matrix 1 0 0 1)
                              (let ([res (visit (quotient i 2))])
                                (matrix-mul res res)))
                          (if (= i 1)
                              m
                              (let ([res (visit (quotient i 2))])
                                (matrix-mul (matrix-mul res res) m)))))])
      (if (and (integer? e)
               (not (negative? e)))
          (if (2x2-matrix? m)
              (visit e)
              (errorf 'matrix-exp
                      "Not a proper 2x2 matrix: ~s"
                      m))
          (errorf 'matrix-exp
                  "Not a non-negative integer: ~s"
                  e)))))


;;; This way of doing matrix exponentiation has logarithmic time complexity 
;;; because there is one visit call every time the exponent is divided by two.


(unless (test-matrix-exp matrix-exp)
  (printf "fail: (test-matrix-exp matrix-exp) ~n"))

(define fib_matrix
  (lambda (n)
    (if (and (integer? n)
             (not (negative? n)))
        (cond
          [(= n 0)
           0]
          [(= n 1)
           1]
          [else
           (x11 (matrix-exp fib-base-matrix (1- n)))])
        (errorf 'fib_matrix
                "Not a non-negative integer: ~s"
                n))))

;;; This way of computing Fibonacci numbers runs in logarithmic time, because it
;;; is based on the logarithmic matrix exponentiation.


(unless (test-fib fib_matrix)
  (printf "fail: (test-fib fib_matrix) ~n"))


;;; The property of Fibonacci numbers illustrated in the about-fib function 
;;; gives another way of writing the fib function with logarithmic complexity.

;;; fib(p + q + 1) = fib(p + 1) * fib(q + 1) + fib(p) * fib(q) 

;;; If p = q:
;;; fib(2p + 1) = fib(p + 1) * fib(p + 1) + fib(p) * fib(p) 

;;; If p + 1 = q:
;;; fib(2 * (p + 1)) = fib(p + 1) * fib(p + 2) + fib(p) * fib(p + 1)

;;; As exemplified above, this means one can compute the nth Fibonacci number
;;; logarithmically by checking whether n is even or odd and calling recursively
;;; based on this.


(define fib_p-q
  (lambda (n_init)
    (letrec ([visit (lambda (i)
                      (if (even? i)
                          (cond [(= i 0)
                                 0]
                                [(= i 2)
                                 1]
                                [else (let* ([p (1- (quotient i 2))]
                                             [fibp+1 (visit (1+ p))])
                                        (+ (* fibp+1 (visit (+ p 2)))
                                           (* fibp+1 (visit p))))])
                          (if (= i 1)
                              1
                              (let* ([p (quotient (1- i) 2)]
                                     [fibp+1 (visit (1+ p))]
                                     [fibp (visit p)])
                                (+ (square fibp+1)
                                   (square fibp))))))])
    (if (and (integer? n_init)
             (not (negative? n_init)))
        (visit n_init)
        (errorf 'fib_p-q
                "Not a non-negative integer: ~s"
                n_init)))))


;;; This version also has logarithmic time complexity, since the input argument
;;; is roughly halved for each call. The fact that there are two or three 
;;; visit calls only adds a constant factor and therefore does not affect the 
;;; overall complexity.

(unless (test-fib fib_p-q)
  (printf "fail: (test-fib fib_p-q) ~n"))


;;;;;;;;;;

(define power_Magritte
  (lambda (n)
    (let ([base 'x])
      `(lambda (,base)
         ,(letrec ([visit (lambda (i)
                            (if (= i 0)
                                `1
                                `(* ,base ,(visit (1- i)))))])
            (visit n))))))

;;;;;;;;;;

(define binary-power
  (lambda (x n)
    (letrec ([visit (lambda (i)
                      (if (even? i)
                          (if (= i 0)
                              1
                              (square (visit (quotient i 2))))
                          (if (= i 1)
                              x
                              (* (square (visit (quotient i 2))) x))))])
      (visit n))))

(define binary-power_Magritte
  (lambda (n)
    (let ([base 'x])
      `(lambda (,base)
         ,(letrec ([visit (lambda (i)
                            (if (even? i)
                                (if (= i 0)
                                    `1
                                    `(square ,(visit (quotient i 2))))
                                (if (= i 1)
                                    base
                                    `(* (square ,(visit (quotient i 2))) ,base))))])
            (visit n))))))



;;; First, to get in the mood, a magrittified version of the naive fib function
;;; given to us:

(define test-fib_v0_Magritte
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '(+ 1 0))
         (equal? (candidate 3)
                 '(+ (+ 1 0) 1))
         (equal? (candidate 4)
                 '(+ (+ (+ 1 0) 1) (+ 1 0)))
         ; etc
         )))


(define fib_v0_Magritte
  (lambda (n_init)
    (letrec ([visit (lambda (n)
                      (cond
                        [(= n 0)
                         `0]
                        [(= n 1)
                         `1]
                        [else
                         `(+ ,(visit (- n 1)) ,(visit (- n 2)))]))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          `,(visit n_init)
          (errorf 'fib_v0
                  "Not a non-negative integer: ~s"
                  n_init)))))

(unless (test-fib_v0_Magritte fib_v0_Magritte)
  (printf "fail: (test-fib_v0_Magritte fib_v0_Magritte) ~n"))


;;; The first linear version
(define test-fib_lin_Magritte
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '(+ 0 1))
         (equal? (candidate 3)
                 '(+ 1 1))
         (equal? (candidate 4)
                 '(+ 1 2))
         (equal? (candidate 5)
                 '(+ 2 3))
         ; etc
         )))

(define fib_lin1_Magritte
  (lambda (n_init)
    (letrec ([visit (lambda (n a1 a2)
                      (cond
                        [(= n 2)
                         `(+ ,a1 ,a2)]
                        [else
                         (visit (- n 1) a2 (+ a1 a2))]))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (cond 
            [(= n_init 0)
             `0]
            [(= n_init 1)
             `1]
            [else
             `,(visit n_init 0 1)])
          (errorf 'fib_lin1_Magritte
                  "Not a non-negative integer: ~s"
                  n_init)))))

(unless (test-fib_lin_Magritte fib_lin1_Magritte)
  (printf "fail: (test-fib_lin_Magritte fib_lin1_Magritte) ~n"))

;;; The second linear version can use the same test case as the first:

(define fib_lin2_Magritte
  (lambda (n_init)
    (letrec ([visit (lambda (n)
                      (cond
                        [(= n 2)
                         (values 0 1)]
                        [else
                         (let-values ([(f2 f1) (visit (- n 1))])
                           (values f1 (+ f2 f1)))]))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (cond 
            [(= n_init 0)
             `0]
            [(= n_init 1)
             `1]
            [else
             (let-values ([(r2 r1) (visit n_init)])
               `(+ ,r2 ,r1))])
          (errorf 'fib_lin2_Magritte
                  "Not a non-negative integer: ~s"
                  n_init)))))

(unless (test-fib_lin_Magritte fib_lin2_Magritte)
  (printf "fail: (test-fib_lin_Magritte fib_lin2_Magritte) ~n"))

;;; fib using matrices:
(define test-fib_matrix_naive_Magritte
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '(x11 (matrix-exp_naive fib-base-matrix 1)))
         (equal? (candidate 7)
                 '(x11 (matrix-exp_naive fib-base-matrix 6)))
         ; etc
         )))



(define fib_matrix_naive_Magritte
  (lambda (n)
    (if (and (integer? n)
             (not (negative? n)))
        (cond
          [(= n 0)
           `0]
          [(= n 1)
           `1]
          [else
           `(x11 (matrix-exp_naive fib-base-matrix ,(1- n)))])
        (errorf 'fib_matrix_naive_Magritte
                "Not a non-negative integer: ~s"
                n))))

(unless (test-fib_matrix_naive_Magritte fib_matrix_naive_Magritte)
  (printf "fail: (test-fib_matrix_naive_Magritte fib_matrix_naive_Magritte) ~n"))


(define test-fib_matrix_Magritte
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '(x11 (matrix-exp fib-base-matrix 1)))
         (equal? (candidate 7)
                 '(x11 (matrix-exp fib-base-matrix 6)))
         ; etc
         )))


(define fib_matrix_Magritte
  (lambda (n)
    (if (and (integer? n)
             (not (negative? n)))
        (cond
          [(= n 0)
           `0]
          [(= n 1)
           `1]
          [else
           `(x11 (matrix-exp fib-base-matrix ,(1- n)))])
        (errorf 'fib_matrix_Magritte
                "Not a non-negative integer: ~s"
                n))))


(unless (test-fib_matrix_Magritte fib_matrix_Magritte)
  (printf "fail: (test-fib_matrix_Magritte fib_matrix_Magritte) ~n"))


;;; If we want the matrix exponentiation in Magritte style as well:

;;; Tests first:

(define test-matrix-exp_naive_Magritte
  (lambda (candidate)
    (and (equal? (candidate (make-2x2-matrix 0 0 0 0) 0)
                 (make-2x2-matrix 1 0 0 1))
         (equal? (candidate (make-2x2-matrix 0 0 0 0) 1)
                 (make-2x2-matrix 0 0 0 0))
         (equal? (candidate (make-2x2-matrix 0 0 0 0) 3)
                 '(matrix-mul ((0 . 0) (0 . 0))
                              (matrix-mul ((0 . 0) (0 . 0))
                                          ((0 . 0) (0 . 0)))))
         (equal? (candidate (make-2x2-matrix 10 10 10 10) 0)
                 (make-2x2-matrix 1 0 0 1))
         (equal? (candidate fib-base-matrix 1)
                 '((0 . 1) (1 . 1)))
         (equal? (candidate fib-base-matrix 2)
                 '(matrix-mul ((0 . 1) (1 . 1))
                              ((0 . 1) (1 . 1))))
         (equal? (candidate fib-base-matrix 4)
                 ' (matrix-mul ((0 . 1) (1 . 1))
                               (matrix-mul ((0 . 1) (1 . 1))
                                           (matrix-mul ((0 . 1) (1 . 1))
                                                       ((0 . 1) (1 . 1))))))
         ; etc.
         )))




(define matrix-exp_naive_Magritte
  (lambda (m e)
    (if (2x2-matrix? m)
        (if (and (number? e)
                 (>= e 0))
            (letrec ([visit (lambda (n)
                              (cond
                                [(= n 1)
                                 m]
                                [else 
                                 `(matrix-mul ,m ,(visit (1- n)))]))])
              (if (= e 0)
                  (make-2x2-matrix 1 0 0 1)
                  (visit e)))
            (errorf 'matrix-exp_naive_Magritte
                    "Not a non-negative integer: ~s"
                    e))
        (errorf 'matrix-exp_naive_Magritte
                "Not a proper 2x2 matrix: ~s"
                m))))


(unless (test-matrix-exp_naive_Magritte matrix-exp_naive_Magritte)
  (printf "fail: (test-matrix-exp_naive_Magritte matrix-exp_naive_Magritte) ~n"))


(define test-matrix-exp_Magritte
  (lambda (candidate)
    (and (equal? (candidate (make-2x2-matrix 0 0 0 0) 0)
                 (make-2x2-matrix 1 0 0 1))
         (equal? (candidate (make-2x2-matrix 0 0 0 0) 1)
                 (make-2x2-matrix 0 0 0 0))
         (equal? (candidate (make-2x2-matrix 0 0 0 0) 3)
                 '(matrix-mul (matrix-mul ((0 . 0) (0 . 0))
                                          ((0 . 0) (0 . 0)))
                              ((0 . 0) (0 . 0))))
         (equal? (candidate (make-2x2-matrix 10 10 10 10) 0)
                 (make-2x2-matrix 1 0 0 1))
         (equal? (candidate fib-base-matrix 1)
                 '((0 . 1) (1 . 1)))
         (equal? (candidate fib-base-matrix 2)
                 '(matrix-mul ((0 . 1) (1 . 1))
                              ((0 . 1) (1 . 1))))
         (equal? (candidate fib-base-matrix 4)
                 '(matrix-mul (matrix-mul ((0 . 1) (1 . 1))
                                          ((0 . 1) (1 . 1)))
                              (matrix-mul ((0 . 1) (1 . 1))
                                          ((0 . 1) (1 . 1)))))
         (equal? (candidate fib-base-matrix 5)
                 '(matrix-mul (matrix-mul (matrix-mul ((0 . 1) (1 . 1))
                                                      ((0 . 1) (1 . 1)))
                                          (matrix-mul ((0 . 1) (1 . 1))
                                                      ((0 . 1) (1 . 1))))
                              ((0 . 1) (1 . 1))))
         ; etc.
         )))

(define matrix-exp_Magritte
  (lambda (m e)
    (letrec ([visit (lambda (i)
                      (if (even? i)
                          (if (= i 0)
                              (make-2x2-matrix 1 0 0 1)
                              (let ([res (visit (quotient i 2))])
                                `(matrix-mul ,res ,res)))
                          (if (= i 1)
                              m
                              (let ([res (visit (quotient i 2))])
                                `(matrix-mul (matrix-mul ,res ,res) ,m)))))])
      (if (and (integer? e)
               (not (negative? e)))
          (if (2x2-matrix? m)
              (visit e)
              (errorf 'matrix-exp_Magritte
                      "Not a proper 2x2 matrix: ~s"
                      m))
          (errorf 'matrix-exp_Magritte
                  "Not a non-negative integer: ~s"
                  e)))))

(unless (test-matrix-exp_Magritte matrix-exp_Magritte)
  (printf "fail: (test-matrix-exp_Magritte matrix-exp_Magritte) ~n"))


(define test-fib_matrix_naive_Magritte^2
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '(x11 ((0 . 1) (1 . 1))))
         (equal? (candidate 7)
                 '(x11 (matrix-mul
                        ((0 . 1) (1 . 1))
                        (matrix-mul
                         ((0 . 1) (1 . 1))
                         (matrix-mul
                          ((0 . 1) (1 . 1))
                          (matrix-mul
                           ((0 . 1) (1 . 1))
                           (matrix-mul 
                            ((0 . 1) (1 . 1)) 
                            ((0 . 1) (1 . 1)))))))))
         ; etc
         )))


(define fib_matrix_naive_Magritte^2
  (lambda (n)
    (if (and (integer? n)
             (not (negative? n)))
        (cond
          [(= n 0)
           `0]
          [(= n 1)
           `1]
          [else
           `(x11 ,(matrix-exp_naive_Magritte fib-base-matrix (1- n)))])
        (errorf 'fib_matrix_naive_Magritte^2
                "Not a non-negative integer: ~s"
                n))))

(unless (test-fib_matrix_naive_Magritte^2 fib_matrix_naive_Magritte^2)
  (printf "fail: (test-fib_matrix_naive_Magritte^2 fib_matrix_naive_Magritte^2 ~n"))


(define test-fib_matrix_Magritte^2
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '(x11 ((0 . 1) (1 . 1))))
         (equal? (candidate 7)
                 '(x11 (matrix-mul (matrix-mul (matrix-mul ((0 . 1) (1 . 1)) 
                                                           ((0 . 1) (1 . 1)))
                                               ((0 . 1) (1 . 1)))
                                   (matrix-mul (matrix-mul ((0 . 1) (1 . 1)) 
                                                           ((0 . 1) (1 . 1)))
                                    ((0 . 1) (1 . 1))))))
         ; etc
         )))


(define fib_matrix_Magritte^2
  (lambda (n)
    (if (and (integer? n)
             (not (negative? n)))
        (cond
          [(= n 0)
           `0]
          [(= n 1)
           `1]
          [else
           `(x11 ,(matrix-exp_Magritte fib-base-matrix (1- n)))])
        (errorf 'fib_matrix_Magritte^2
                "Not a non-negative integer: ~s"
                n))))


(unless (test-fib_matrix_Magritte^2 fib_matrix_Magritte^2)
  (printf "fail: (test-fib_matrix_Magritte^2 fib_matrix_Magritte^2) ~n"))

;;; Last but not least, fib based on the p-q property:

(define test-fib_p-q_Magritte
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '0)
         (equal? (candidate 1)
                 '1)
         (equal? (candidate 2)
                 '1)
         (equal? (candidate 3)
                 '(+ (square 1) (square 1)))
         (equal? (candidate 4)
                 '(+ (* 1 (+ (square 1) (square 1)))
                                (* 1 1)))
         (equal? (candidate 5)
                 '(+ (square (+ (square 1) (square 1)))
                                (square 1)))
         ; etc
         )))

(define fib_p-q_Magritte
  (lambda (n_init)
    (letrec ([visit (lambda (i)
                      (if (even? i)
                          (cond [(= i 0)
                                 `0]
                                [(= i 2)
                                 `1]
                                [else (let* ([p (1- (quotient i 2))]
                                             [fibp+1 (visit (1+ p))])
                                        `(+ (* ,fibp+1 ,(visit (+ p 2)))
                                            (* ,fibp+1 ,(visit p))))])
                          (if (= i 1)
                              `1
                              (let* ([p (quotient (1- i) 2)]
                                     [fibp+1 (visit (1+ p))]
                                     [fibp (visit p)])
                                `(+ (square ,fibp+1)
                                    (square ,fibp))))))])
    (if (and (integer? n_init)
             (not (negative? n_init)))
        `,(visit n_init)
        (errorf 'fib_p-q_Magritte
                "Not a non-negative integer: ~s"
                n_init)))))

(unless (test-fib_p-q_Magritte fib_p-q_Magritte)
  (printf "fail: (test-fib_p-q_Magritte fib_p-q_Magritte) ~n"))

;;;;;;;;;;

(define about-fib
  (lambda (fib p q)
    (= (fib (+ p q 1))
       (+ (* (fib (+ p 1)) (fib (+ q 1)))
          (* (fib p) (fib q))))))

(define test-about-fib
  (lambda (candidate)
    (let ([p (random 20)]
          [q (random 20)])
      (or (about-fib candidate p q)
          (begin
            (printf "about-fib failed for ~s and ~s~n" p q)
            #f)))))


(unless (test-about-fib fib_v0)
  (printf "fail: (test-about-fib fib_v0) ~n"))

(unless (test-about-fib fib_lin1)
  (printf "fail: (test-about-fib fib_lin1) ~n"))

(unless (test-about-fib fib_lin2)
  (printf "fail: (test-about-fib fib_lin2) ~n"))

(unless (test-about-fib fib_matrix_naive)
  (printf "fail: (test-about-fib fib_matrix_naive) ~n"))

(unless (test-about-fib fib_matrix)
  (printf "fail: (test-about-fib fib_matrix) ~n"))

(unless (test-about-fib fib_p-q)
  (printf "fail: (test-about-fib fib_p-q) ~n"))

;;;;;;;;;;

(define test-index-of-nearest-fibonacci-number
  (lambda (candidate)
    (and (= (candidate  0) 0)   ;;; since fib(0) =       0 <=  0 = fib(0)
         (= (candidate  1) 1)   ;;; since fib(0) =  0 <  1 <=  1 = fib(1)
         (= (candidate  2) 3)   ;;; since fib(2) =  1 <  2 <=  3 = fib(3)
         (= (candidate  3) 4)   ;;; since fib(3) =  2 <  3 <=  3 = fib(4)
         (= (candidate  4) 5)   ;;; since fib(3) =  3 <  4 <=  5 = fib(5)
         (= (candidate  5) 5)   ;;; since fib(4) =  3 <  5 <=  5 = fib(5)
         (= (candidate  6) 6)   ;;; since fib(5) =  5 <  6 <=  8 = fib(6)
         (= (candidate  7) 6)   ;;; since fib(5) =  5 <  7 <=  8 = fib(6)
         (= (candidate  8) 6)   ;;; since fib(5) =  5 <  8 <=  8 = fib(6)
         (= (candidate  9) 7)   ;;; since fib(6) =  8 <  9 <= 13 = fib(7)
         (= (candidate 10) 7)   ;;; since fib(6) =  8 <  7 <= 13 = fib(7)
         (= (candidate 12) 7)   ;;; since fib(6) =  8 < 12 <= 13 = fib(7)
         (= (candidate 13) 7)   ;;; since fib(6) =  8 < 13 <= 13 = fib(7)
         (= (candidate 14) 8)   ;;; since fib(7) = 13 < 14 <= 21 = fib(8)
         (= (candidate 15) 8)   ;;; since fib(7) = 13 < 15 <= 21 = fib(8)
         (= (candidate 20) 8)   ;;; since fib(7) = 13 < 20 <= 21 = fib(8)
         (= (candidate 21) 8)   ;;; since fib(7) = 13 < 21 <= 21 = fib(8)
         (= (candidate 22) 9)   ;;; since fib(8) = 21 < 22 <= 34 = fib(9)
         (= (candidate 23) 9)   ;;; since fib(8) = 21 < 23 <= 34 = fib(9)
         ;;; etc.
         )))


(define index-of-nearest-fibonacci-number_ugly
  (lambda (n)
    (letrec ([visit 
              (lambda (i)
                (if (< (fib_p-q i) n)
                    (visit (1+ i))
                    i))])
      (if (and (number? n)
               (not (negative? n)))
          (visit 0)
          (errorf 'index-of-nearest-fibonacci-number_ugly
                  "Not a non-negative integer: ~s"
                  n)))))
                    
;;; This is a very naive solution, that calls fib for every natural number until
;;; the result is greater than or equal to the input.

(unless (test-index-of-nearest-fibonacci-number index-of-nearest-fibonacci-number_ugly)
  (printf "fail: (test-index-of-nearest-fibonacci-number index-of-nearest-fibonacci-number_ugly) ~n"))


(define index-of-nearest-fibonacci-number_lin1
  (lambda (n)
    (letrec ([visit (lambda (i a1 a2)
                      (if (< (+ a1 a2) n)
                          (visit (1+ i) a2 (+ a1 a2))
                          i))])
      (if (and (number? n)
               (not (negative? n)))
          (cond [(= 0 n)
                 0]
                [(= 1 n)
                 1]
                [else
                 (visit 3 1 1)])
          (errorf 'index-of-nearest-fibonacci-number_lin1
                  "Not a non-negative integer: ~s"
                  n)))))


;;; This version computes the Fibonacci numbers using two accumulators. For each
;;; step the addition of the two accumulators and therefore the i'th Fibonacci
;;; number is compared to the input number.

(unless (test-index-of-nearest-fibonacci-number index-of-nearest-fibonacci-number_lin1)
  (printf "fail: (test-index-of-nearest-fibonacci-number index-of-nearest-fibonacci-number_lin1) ~n"))

;;; Index using matrices:

(define fib-base-matrix^3
  (matrix-exp fib-base-matrix 3))

(define index-of-nearest-fibonacci-number_matrix
  (lambda (n)
    (letrec ([visit (lambda (i m)
                      (if (>= (x11 m) n)
                          (cond [(>= (x00 m) n)
                                 (- i 2)]
                                [(>= (x10 m) n)
                                 (1- i)]
                                [else
                                 i])
                          (visit (+ i 3) (matrix-mul m fib-base-matrix^3))))])
      (if (and (number? n)
               (not (negative? n)))
          (cond [(= 0 n)
                 0]
                [(= 1 n)
                 1]
                [else
                 (visit 5 (matrix-mul fib-base-matrix fib-base-matrix^3))])
          (errorf 'index-of-nearest-fibonacci-number_matrix
                  "Not a non-negative integer: ~s"
                  n)))))

;;; The idea behind this version is very similar to the one with accumulators,
;;; but it uses the Fibonacci matrix instead. The index can be increased by 3
;;; for each step, because the matrix stores 3 consecutive Fibonacci numbers.

;;; When timing the two versions, we note that the one with accumulators is 
;;; faster and uses less space than the one with matrices. We believe this is
;;; due to the fact that a list of two pairs uses more space than two numbers 
;;; and the multiplication of two matrices takes longer than adding two numbers
;;; three times.

;;; Both versions run in linear time. In the case of the one with accumulators
;;; this is due to one visit call for every increment of the index. The matrix
;;; procedure only calls visit for every thirs index, but this is just a
;;; constant factor and therefore irrelevant.

(unless (test-index-of-nearest-fibonacci-number index-of-nearest-fibonacci-number_matrix)
  (printf "fail: (test-index-of-nearest-fibonacci-number index-of-nearest-fibonacci-number_matrix) ~n"))


;;;;;;;;;;;;;;;;;;;;

;;; end of week-07_fibonacci.scm

"week-07_fibonacci.scm"
