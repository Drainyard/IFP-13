;;; week-5_arithmetic-expressions.scm
;;; dProgSprog 2015-2016, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 09 May 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/dProgSprog16/Lecture-notes/week-5_arithmetic-expressions.html

;;;;;;;;;;

(load "ex5.scm")

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

;;;;;;;;;;

;;; constructors:

(define make-literal
  (lambda (n)
    (list 'literal n)))

(define make-plus
  (lambda (e1 e2)
    (list 'plus e1 e2)))

(define make-times
  (lambda (e1 e2)
    (list 'times e1 e2)))

;;;;;;;;;;

;;; predicates:

(define is-literal?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'literal)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-plus?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'plus)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-times?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'times)
         (proper-list-of-given-length? (cdr v) 2))))

;;;;;;;;;;

;;; accessors:

(define literal-1
  (lambda (v)
    (list-ref v 1)))

(define plus-1
  (lambda (v)
    (list-ref v 1)))

(define plus-2
  (lambda (v)
    (list-ref v 2)))

(define times-1
  (lambda (v)
    (list-ref v 1)))

(define times-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

;;; well-formed arithmetic expressions:

(define ae0
  (make-literal 42))

(define ae1
  (make-plus (make-literal 1)
             (make-literal 10)))

(define ae2
  (make-plus ae1
             (make-plus (make-literal 100)
                        (make-literal 1000))))

(define ae3
  (make-times
   (make-times
    (make-times
     (make-literal 1)
     (make-literal 2))
    (make-literal 3))
   (make-times
    (make-literal 4)
    (make-literal 5))))

;;;;;;;;;;

;;; Array of (positive) unit tests:

(define test-check-arithmetic-expression
  (lambda (candidate)
    (and (candidate ae0)
         (candidate ae1)
         (candidate ae2)
         (candidate ae3)
         ;;; add more tests here
         )))

(define test-interpret-arithmetic-expression
  (lambda (candidate)
    (and (= (candidate ae0) 42)
         (= (candidate ae1) 11)
         (= (candidate ae2) 1111)
         (= (candidate ae3) 120)
         ;;; add more tests here
         )))

(define test-compile-arithmetic-expression-and-check-byte-code-program
  (lambda (compile check)
    (and (check (compile ae0))
         (check (compile ae1))
         (check (compile ae2))
         (check (compile ae3))
         ;;; add more tests here
         )))

(define test-compile-arithmetic-expression-and-run-byte-code-program
  (lambda (compile run)
    (and (= (run (compile ae0)) 42)
         (= (run (compile ae1)) 11)
         (= (run (compile ae2)) 1111)
         (= (run (compile ae3)) 120)
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; source syntax checker:

(define check-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (integer? (literal-1 e))]
      [(is-plus? e)
       (and (check-arithmetic-expression (plus-1 e))
            (check-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (and (check-arithmetic-expression (times-1 e))
            (check-arithmetic-expression (times-2 e)))]
      [else
       #f])))

(unless (test-check-arithmetic-expression check-arithmetic-expression)
  (printf "fail: (test-check-arithmetic-expression check-arithmetic-expression)~n"))

;;;;;;;;;;

;;; source interpreter:

(define interpret-arithmetic-expression
  (lambda (e_init)
    (letrec ([process (lambda (e)
                        (cond
                          [(is-literal? e)
                           (literal-1 e)]
                          [(is-plus? e)
                           (+ (process (plus-1 e))
                              (process (plus-2 e)))]
                          [(is-times? e)
                           (* (process (times-1 e))
                              (process (times-2 e)))]
                          [else
                           (errorf 'interpret-arithmetic-expression
                                   "unrecognized expression: ~s"
                                   e)]))])
      (process e_init))))

(unless (test-interpret-arithmetic-expression interpret-arithmetic-expression)
  (printf "fail: (test-interpret-arithmetic-expression interpret-arithmetic-expression)~n"))

;;;;;;;;;;

(define unparse-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-plus? e)
       (list '+
             (unparse-arithmetic-expression (plus-1 e))
             (unparse-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (list '*
             (unparse-arithmetic-expression (times-1 e))
             (unparse-arithmetic-expression (times-2 e)))]
      [else
       (errorf 'unparse-arithmetic-expression
               "unrecognized abstract syntax: ~s"
               e)])))

;;; > (unparse-arithmetic-expression ae1)
;;; (+ 1 10)
;;; > (+ 1 20)
;;; 21
;;; > (interpret-arithmetic-expression ae1)
;;; 11
;;; > 

;;;;;;;;;;

(define parse-arithmetic-expression
  (lambda (v)
    (cond
      [(integer? v)
       (make-literal v)]
      [(proper-list-of-given-length? v 3)
       (case (list-ref v 0)
         [(+)
          (make-plus (parse-arithmetic-expression (list-ref v 1))
                     (parse-arithmetic-expression (list-ref v 2)))]
         [(*)
          (make-times (parse-arithmetic-expression (list-ref v 1))
                      (parse-arithmetic-expression (list-ref v 2)))]
         [else
          (errorf 'parse-arithmetic-expression
                  "unrecognized operator: ~s"
                  v)])]
      [else
       (errorf 'parse-arithmetic-expression
               "unrecognized concrete syntax: ~s"
               v)])))

;;; > (interpret-arithmetic-expression
;;;     (parse-arithmetic-expression
;;;       '(+ 123 4321)))
;;; 4444
;;; > (equal? ae3
;;;           (parse-arithmetic-expression
;;;             (unparse-arithmetic-expression
;;;               ae3)))
;;; #t
;;; > (let ([ae '(+ 1 2)])
;;;     (equal? ae
;;;             (unparse-arithmetic-expression
;;;               (parse-arithmetic-expression
;;;                 ae))))
;;; #t
;;; > 

;;;;;;;;;;

;;; constructors:

(define make-PUSH
  (lambda (n)
    (list 'PUSH n)))

(define make-ADD
  (lambda ()
    (list 'ADD)))

(define make-MUL
  (lambda ()
    (list 'MUL)))

;;;;;;;;;;

;;; predicates:

(define is-PUSH?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'PUSH)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-ADD?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'ADD)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-MUL?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'MUL)
         (proper-list-of-given-length? (cdr v) 0))))

;;;;;;;;;;

;;; accessors:

(define PUSH-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

(define make-byte-code-program
  (lambda (is)
    (list 'byte-code-program is)))

(define is-byte-code-program?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'byte-code-program)
         (proper-list-of-given-length? (cdr v) 1))))

(define byte-code-program-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; well-formed byte-code programs:

(define p0
  (make-byte-code-program '((PUSH 42))))

(define p1
  (make-byte-code-program '((PUSH 20) (PUSH 22) (ADD))))

(define q0
  (make-byte-code-program '((PUSH 1) (PUSH 2))))

(define q1
  (make-byte-code-program '((PUSH 1) (ADD))))

;;;;;;;;;;

;;; target syntax checker:

(define check-byte-code-instruction
  (lambda (v)
    (cond
      [(is-PUSH? v)
       (integer? (PUSH-1 v))]
      [(is-ADD? v)
       #t]
      [(is-MUL? v)
       #t]
      [else
       #f])))

;;;;;;;;;;

;;; positive unit test:

(define test-check-byte-code-programs
  (lambda (candidate)
    (and (candidate p0)
         (candidate p1)
         (candidate q0)
         (candidate q1)
         ;;; add more tests here
         )))

(define test-run-byte-code-programs
  (lambda (candidate)
    (and (= (candidate p0) 42)
         (= (candidate p1) 42)
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; ill-formed byte-code programs:

(define z0
  '())

(define z1
  (make-byte-code-program '((PUSH 42) . whatever)))

(define z2
  (make-byte-code-program '((ADD 42))))

(define z3
  (make-byte-code-program '((PUSH "42"))))

(define z4
  (make-byte-code-program '((PUSH 0) (PUSH 1/2) (ADD))))

;;;;;;;;;;

;;; negative unit test:

(define test-check-byte-code-programs-negatively
  (lambda (candidate)
    (not (or (candidate z0)
             (candidate z1)
             (candidate z2)
             (candidate z3)
             (candidate z4)
             ;;; add more tests here
             ))))

;;;;;;;;;;

;;; target syntax checker:

(define check-byte-code-program
  (lambda (v)
    (if (is-byte-code-program? v)
        (letrec ([loop (lambda (v)
                         (cond
                           [(null? v)
                            #t]
                           [(pair? v)
                            (and (check-byte-code-instruction (car v))
                                 (loop (cdr v)))]
                           [else
                            #f]))])
          (loop (byte-code-program-1 v)))
        #f)))

(unless (test-check-byte-code-programs check-byte-code-program)
  (printf "fail: (test-check-byte-code-programs check-byte-code-program)~n"))

(unless (test-check-byte-code-programs-negatively check-byte-code-program)
  (printf "fail: (test-check-byte-code-programs-negatively check-byte-code-program)~n"))

;;;;;;;;;;

;;; target interpreter (i.e., virtual machine):

(define at-least-two?
  (lambda (vs)
    (and (pair? vs)
         (pair? (cdr vs)))))

(define run-byte-code-program
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (cond
                           [(null? is)
                            vs]
                           [(pair? is)
                            (let ([i (car is)]
                                  [is (cdr is)])
                              (cond
                                [(is-PUSH? i)
                                 (loop is
                                       (cons (PUSH-1 i) vs))]
                                [(is-ADD? i)
                                 (if (at-least-two? vs)
                                     (let* ([operand_1 (car vs)]
                                            [vs (cdr vs)]
                                            [operand_2 (car vs)]
                                            [vs (cdr vs)])
                                       (loop is
                                             (cons (+ operand_1 operand_2)
                                                   vs)))
                                     (errorf 'run-byte-code-program
                                             "stack underflow: ~s"
                                             vs))]
                                [(is-MUL? i)
                                 (if (at-least-two? vs)
                                     (let* ([operand_1 (car vs)]
                                            [vs (cdr vs)]
                                            [operand_2 (car vs)]
                                            [vs (cdr vs)])
                                       (loop is
                                             (cons (* operand_1 operand_2)
                                                   vs)))
                                     (errorf 'run-byte-code-program
                                             "stack underflow: ~s"
                                             vs))]
                                [else
                                 (errorf 'run-byte-code-program
                                         "unrecognized byte code: ~s"
                                         i)]))]
                           [else
                            (errorf 'run-byte-code-program
                                    "ill-formed list of byte-code instructions: ~s"
                                    is)]))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))


(define run-byte-code-program_Magritte
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (cond
                           [(null? is)
                            vs]
                           [(pair? is)
                            (let ([i (car is)]
                                  [is (cdr is)])
                              (cond
                                [(is-PUSH? i)
                                 (loop is
                                       (cons (make-literal (PUSH-1 i)) vs))]
                                [(is-ADD? i)
                                 (if (at-least-two? vs)
                                     (let* ([operand_1 (car vs)]
                                            [vs (cdr vs)]
                                            [operand_2 (car vs)]
                                            [vs (cdr vs)])
                                       (loop is
                                             (cons (make-plus operand_1 operand_2)
                                                   vs)))
                                     (errorf 'run-byte-code-program
                                             "stack underflow: ~s"
                                             vs))]
                                [(is-MUL? i)
                                 (if (at-least-two? vs)
                                     (let* ([operand_1 (car vs)]
                                            [vs (cdr vs)]
                                            [operand_2 (car vs)]
                                            [vs (cdr vs)])
                                       (loop is
                                             (cons (make-times operand_1 operand_2)
                                                   vs)))
                                     (errorf 'run-byte-code-program
                                             "stack underflow: ~s"
                                             vs))]
                                [else
                                 (errorf 'run-byte-code-program
                                         "unrecognized byte code: ~s"
                                         i)]))]
                           [else
                            (errorf 'run-byte-code-program
                                    "ill-formed list of byte-code instructions: ~s"
                                    is)]))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

(define test-Magritte
  (lambda (e)
    (list "This is not an arithmetic expression."
          e
          (run-byte-code-program_Magritte
            (compile-arithmetic-expression
              e)))))

(unless (test-run-byte-code-programs run-byte-code-program)
  (printf "fail: (test-run-byte-code-programs run-byte-code-program)~n"))

;;;;;;;;;;

(define compile-arithmetic-expression
  (lambda (e_init)
    (letrec ([process (lambda (e)
                        (cond
                          [(is-literal? e)
                           (list (make-PUSH (literal-1 e)))]
                          [(is-plus? e)
                           (append (process (plus-1 e))
                                   (process (plus-2 e))
                                   (list (make-ADD)))]
                          [(is-times? e)
                           (append (process (times-1 e))
                                   (process (times-2 e))
                                   (list (make-MUL)))]
                          [else
                           (errorf 'compile-arithmetic-expression
                                   "unrecognized expression: ~s"
                                   e)]))])
      (make-byte-code-program (process e_init)))))


(define compile-arithmetic-expression_fold
  (lambda (e_init)
    (make-byte-code-program
     ((fold-right_arithmetic-expression
       (lambda (e)
         (list (make-PUSH e)))
       (lambda (e1 e2)
         (append e1 e2 (list (make-ADD))))
       (lambda (e1 e2)
         (append e1 e2 (list (make-MUL))))
       (lambda (e)
         (errorf 'compile-arithmetic-expression_fold
                 "unrecognized expression: ~s"
                 e))) e_init))))

(define compile-arithmetic-expression_alt-fold
  (lambda (e_init)
    (make-byte-code-program
     (((fold-right_arithmetic-expression
       (lambda (e)
         (lambda (a)
           (cons (make-PUSH e) a)))
       (lambda (e1 e2)
         (lambda (a)
           (e1 (e2 (cons (make-ADD) a)))))
       (lambda (e1 e2)
         (lambda (a)
           (e1 (e2 (cons (make-MUL) a)))))
       (lambda (e)
         (errorf 'compile-arithmetic-expression_alt-fold
                 "unrecognized expression: ~s"
                 e))) e_init) '()))))

(define compile-arithmetic-expression_alt
  (lambda (e_init)
    (letrec ([process (lambda (e a)
                        (cond
                          [(is-literal? e)
                           (cons (make-PUSH (literal-1 e)) a)]
                          [(is-plus? e)
                           (process (plus-1 e)
                                    (process (plus-2 e) (cons
                                                         (make-ADD) a)))]
                          [(is-times? e)
                           (process (times-1 e)
                                    (process (times-2 e) (cons
                                                          (make-MUL) a)))]
                          [else
                           (errorf 'compile-arithmetic-expression_alt
                                   "unrecognized expression: ~s"
                                   e)]))])
      (make-byte-code-program (process e_init '())))))

;;; The new compiler is faster. Look at the file for the next exercise for 
;;; the missing code.

;; > (time (let ([x (compile-arithmetic-expression foo)]) 1))
;; (time (let ((...)) ...))
;;     105 collections
;;     2715 ms elapsed cpu time, including 1106 ms collecting
;;     2740 ms elapsed real time, including 1135 ms collecting
;;     926379040 bytes allocated, including 915534880 bytes reclaimed
;; 1
;; > (time (let ([x (compile-arithmetic-expression_alt foo)]) 1))
;; (time (let ((...)) ...))
;;     9 collections
;;     1404 ms elapsed cpu time, including 155 ms collecting
;;     1419 ms elapsed real time, including 153 ms collecting
;;     83888496 bytes allocated, including 63156080 bytes reclaimed
;; 1



(unless (test-compile-arithmetic-expression-and-check-byte-code-program compile-arithmetic-expression check-byte-code-program)
  (printf "fail: (test-compile-arithmetic-expression-and-check-byte-code-program compile-arithmetic-expression check-byte-code-program)~n"))

(unless (test-compile-arithmetic-expression-and-run-byte-code-program compile-arithmetic-expression run-byte-code-program)
  (printf "fail: (test-compile-arithmetic-expression-and-run-byte-code-program compile-arithmetic-expression run-byte-code-program)~n"))

;;;;;;;;;;

(define check-verify-byte-code-program
  (lambda (candidate)
    (and (candidate p0)
         (candidate p1)
         ;;; add more tests here
         )))

(define check-verify-byte-code-program-negatively
  (lambda (candidate)
    (not (or (candidate q0)
             (candidate q1)
             ;;; add more tests here
             ))))

;;;;;;;;;;

(define verify-byte-code-program
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is n)
                         (cond
                           [(null? is)
                            n]
                           [(pair? is)
                            (let ([i (car is)]
                                  [is (cdr is)])
                              (cond
                                [(is-PUSH? i)
                                 (loop is (1+ n))]
                                [(is-ADD? i)
                                 (and (>= n 2)
                                      (loop is (1- n)))]
                                [(is-MUL? i)
                                 (and (>= n 2)
                                      (loop is (1- n)))]
                                [else
                                 (errorf 'verify-byte-code-program
                                         "unrecognized byte code: ~s"
                                         i)]))]
                           [else
                            (errorf 'verify-byte-code-program
                                    "ill-formed list of byte-code instructions: ~s"
                                    is)]))])
          (let ([result (loop (byte-code-program-1 p) 0)])
            (and result
                 (= result 1))))
        (errorf 'verify-byte-code-program
                "not a byte-code program: ~s"
                p))))

(unless (check-verify-byte-code-program verify-byte-code-program)
  (printf "fail: (check-verify-byte-code-program verify-byte-code-program)~n"))

(unless (check-verify-byte-code-program-negatively verify-byte-code-program)
  (printf "fail: (check-verify-byte-code-program-negatively verify-byte-code-program)~n"))

(define test-compile-arithmetic-expression-and-verify-byte-code-program
  (lambda (compile verify)
    (and (verify (compile ae0))
         (verify (compile ae1))
         (verify (compile ae2))
         (verify (compile ae3))
         ;;; add more tests here
         )))

(unless (test-compile-arithmetic-expression-and-verify-byte-code-program compile-arithmetic-expression verify-byte-code-program)
  (printf "fail: (test-compile-arithmetic-expression-and-verify-byte-code-program compile-arithmetic-expression verify-byte-code-program)~n"))

;;;;;;;;;;

(define run-verified-byte-code-program
  (lambda (p)
    (letrec ([loop (lambda (is vs)
                     (if (null? is)
                         vs
                         (let ([i (car is)]
                               [is (cdr is)])
                           (cond
                             [(is-PUSH? i)
                              (loop is
                                    (cons (PUSH-1 i) vs))]
                             [(is-ADD? i)
                              (let* ([operand_1 (car vs)]
                                     [vs (cdr vs)]
                                     [operand_2 (car vs)]
                                     [vs (cdr vs)])
                                (loop is
                                      (cons (+ operand_1 operand_2)
                                            vs)))]
                             [else
                              (let* ([operand_1 (car vs)]
                                     [vs (cdr vs)]
                                     [operand_2 (car vs)]
                                     [vs (cdr vs)])
                                (loop is
                                      (cons (* operand_1 operand_2)
                                            vs)))]))))])
      (car (loop (byte-code-program-1 p) '())))))

(unless (test-compile-arithmetic-expression-and-run-byte-code-program compile-arithmetic-expression run-verified-byte-code-program)
  (printf "fail: (test-compile-arithmetic-expression-and-run-byte-code-program compile-arithmetic-expression run-verified-byte-code-program)~n"))

;;;;;;;;;;

(define p100000
  (make-byte-code-program
   (append
    (make-list 100000 '(PUSH 1))
    (make-list 99999 '(ADD)))))

;;; > (make-list 2 33)
;;; (33 33)
;;; > (length (byte-code-program-1 p100000))
;;; 199999
;;; > (verify-byte-code-program p100000)
;;; #t
;;; > (time (run-verified-byte-code-program p100000))
;;; (time (run-verified-byte-code-program p100000))
;;;     5 collections
;;;     222 ms elapsed cpu time, including 6 ms collecting
;;;     222 ms elapsed real time, including 7 ms collecting
;;;     44801024 bytes allocated, including 40661936 bytes reclaimed
;;; 100000
;;; > (time (run-byte-code-program p100000))
;;; (time (run-byte-code-program p100000))
;;;     5 collections
;;;     262 ms elapsed cpu time, including 8 ms collecting
;;;     264 ms elapsed real time, including 7 ms collecting
;;;     44801040 bytes allocated, including 41146160 bytes reclaimed
;;; 100000
;;; > (verify-byte-code-program q1)
;;; #f
;;; > (run-byte-code-program q1)
;;; 
;;; Exception in run-byte-code-program: stack underflow: (1)
;;; Type (debug) to enter the debugger.
;;; > (run-verified-byte-code-program q1)
;;; 
;;; Exception in car: () is not a pair
;;; Type (debug) to enter the debugger.
;;; > (verify-byte-code-program q0)
;;; #f
;;; > (run-byte-code-program q0)
;;; 
;;; Exception in run-byte-code-program: unexpected resulting stack: (2 1)
;;; Type (debug) to enter the debugger.
;;; > (run-verified-byte-code-program q0)
;;; 2
;;; > 

;;;;;;;;;;

(define compile-and-run-arithmetic-expression
  (lambda (e)
    (run-byte-code-program
     (compile-arithmetic-expression
      e))))

(define test-commutation-for-arithmetic-expressions
  (lambda (e)
    (let ([result-1 (interpret-arithmetic-expression e)]
          [result-2 (compile-and-run-arithmetic-expression e)])
      (if (equal? result-1 result-2)
          (list #t result-1)
          (list #f result-1 result-2)))))

(unless (car (test-commutation-for-arithmetic-expressions ae0))
  (printf "fail: (test-commutation-for-arithmetic-expressions ae0)"))

(unless (car (test-commutation-for-arithmetic-expressions ae1))
  (printf "fail: (test-commutation-for-arithmetic-expressions ae1)"))

(unless (car (test-commutation-for-arithmetic-expressions ae2))
  (printf "fail: (test-commutation-for-arithmetic-expressions ae2)"))

;;;;;;;;;;

;;; end of week-5_arithmetic-expressions.scm

"week-5_arithmetic-expressions.scm"
