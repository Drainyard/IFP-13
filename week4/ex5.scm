;;; week-04_the-bizarre-optimizing-compiler.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 20 Sep 2016

;;; was:
;;; week-03_the-40-optimizing-compilers.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 13 Sep 2016

;;; Accompanying material for the lecture note at
;;;   https://users-cs.au.dk/danvy/IFP16/Lecture-notes/the-40-optimizing-compilers.html

;;;;;;;;;;

(load "ae-sample.scm")

;;;;;;;;;;

;;; utilities:

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (1- n))))))

;;;;;;;;;;

;;; implementation of the BNF for arithmetic expressions:

;;; <arithmetic-expression> ::= (literal <number>)
;;;                           | (plus <arithmetic-expression> <arithmetic-expression>)
;;;                           | (times <arithmetic-expression> <arithmetic-expression>)

;;;;;

;;; the constructors:

(define make-literal
  (lambda (n)
    (list 'literal n)))

(define make-plus
  (lambda (e1 e2)
    (list 'plus e1 e2)))

(define make-times
  (lambda (e1 e2)
    (list 'times e1 e2)))

;;; the predicates:

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

;;; the accessors:

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

(define fold-right_arithmetic-expression
  (lambda (case-literal case-plus case-times case-else)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(is-literal? v)
                           (case-literal (literal-1 v))]
                          [(is-plus? v)
                           (case-plus (visit (plus-1 v))
                                      (visit (plus-2 v)))]
                          [(is-times? v)
                           (case-times (visit (times-1 v))
                                       (visit (times-2 v)))]
                          [else
                           (case-else v)]))])
        (visit v_init)))))

;;;;;;;;;;

;;; unparser:

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

;;; parser:

(define parse-arithmetic-expression
  (lambda (v)
    (cond
      [(number? v)
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

;;;;;;;;;;

;;; unit tests:

(define test-interpret-arithmetic-expression
  (lambda (interpret)
    (and (= (interpret (parse-arithmetic-expression source-ae0)) 42)
         (= (interpret (parse-arithmetic-expression source-ae1)) 11)
         (= (interpret (parse-arithmetic-expression source-ae2)) 1111)
         (= (interpret (parse-arithmetic-expression source-ae3)) 120)
         ;;; add more tests here
         )))

(define test-compile-and-run-arithmetic-expression
  (lambda (compile run)
    (and (= (run (compile (parse-arithmetic-expression source-ae0))) 42)
         (= (run (compile (parse-arithmetic-expression source-ae1))) 11)
         (= (run (compile (parse-arithmetic-expression source-ae2))) 1111)
         (= (run (compile (parse-arithmetic-expression source-ae3))) 120)
         ;;; add more tests here
         )))

(define test-interpret-arithmetic-expression_Magritte
  (lambda (interpret_Magritte)
    (and (let ([ae0 (parse-arithmetic-expression source-ae0)])
           (equal? (interpret_Magritte ae0) ae0))
         (let ([ae1 (parse-arithmetic-expression source-ae1)])
           (equal? (interpret_Magritte ae1) ae1))
         (let ([ae2 (parse-arithmetic-expression source-ae2)])
           (equal? (interpret_Magritte ae2) ae2))
         (let ([ae3 (parse-arithmetic-expression source-ae3)])
           (equal? (interpret_Magritte ae3) ae3))
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; interpreter:

(define interpret-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-plus? e)
       (+ (interpret-arithmetic-expression (plus-1 e))
          (interpret-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (* (interpret-arithmetic-expression (times-1 e))
          (interpret-arithmetic-expression (times-2 e)))]
      [else
       (errorf 'interpret-arithmetic-expression
               "unrecognized expression: ~s"
               e)])))

(unless (test-interpret-arithmetic-expression interpret-arithmetic-expression)
  (printf "fail: (test-interpret-arithmetic-expression interpret-arithmetic-expression)~n"))

;;;;;;;;;;

;;; implementation of the BNF for the byte-code instructions:

;;;;;

;;; the constructors:

(define make-PUSH
  (lambda (n)
    (list 'PUSH n)))

(define make-ADD
  (lambda ()
    (list 'ADD)))

(define make-MUL
  (lambda ()
    (list 'MUL)))

;;; the predicates:

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

;;; the accessors:

(define PUSH-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; implementation of the BNF for the byte-code programs:

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

;;; virtual machine:

(define at-least-two?
  (lambda (vs)
    (and (pair? vs)
         (pair? (cdr vs)))))

(define run-byte-code-program
  (lambda (p)
    (if (is-byte-code-program? p)
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
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (+ operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [(is-MUL? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
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
                                          i)]))))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

(unless (equal? (run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD)))) 42)
  (printf "(run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD)))) didn't yield 42~n"))

;;;;;;;;;;

;;; compiler:

(define compile-arithmetic-expression
  (lambda (e)
    (letrec ([visit (lambda (e a)
                      (cond
                        [(is-literal? e)
                         (cons (make-PUSH (literal-1 e)) a)]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (visit (plus-2 e)
                                       (cons (make-ADD) a)))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (visit (times-2 e)
                                       (cons (make-MUL) a)))]
                        [else
                         (errorf 'compile-arithmetic-expression
                                 "unrecognized expression: ~s"
                                 e)]))])
      (make-byte-code-program (visit e '())))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression run-byte-code-program)
  (printf "fail: (test-compile-and-run-arithmetic-expression compile-arithmetic-expression run-byte-code-program)~n"))

;;;;;;;;;;

;;; "just-in-time" compiler:

(define compile-and-run-arithmetic-expression
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression)
  (printf "fail: (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression)~n"))
>>>>>>> 362f30402811219f22cdf75f5111fe1499b4bd06

;;;;;;;;;;

;;; unit test:

(define does_interpret-arithmetic-expression_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression ae)))))

(define test_does_interpret-arithmetic-expression_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_make_the_diagram_commute?)~n"))

;;;;;;;;;;

;;; Magritte virtual machine:

(define run-byte-code-program_Magritte
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (if (null? is)
                             vs
                             (let ([i (car is)]
                                   [is (cdr is)])
                               (cond
                                 [(is-PUSH? i)
                                  (loop is
                                        (cons (make-literal (PUSH-1 i)) vs))]
                                 [(is-ADD? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (make-plus operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [(is-MUL? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
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
                                          i)]))))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

;;; The corresponding "just-in-time" decompiler:

(define compile-and-run-arithmetic-expression_Magritte
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression e))))

(unless (test-interpret-arithmetic-expression_Magritte compile-and-run-arithmetic-expression_Magritte)
  (printf "fail: (test-interpret-arithmetic-expression_Magritte compile-and-run-arithmetic-expression_Magritte)~n"))

;;;;;;;;;;

;;; The Magritte interpreter:

(define interpret-arithmetic-expression_Magritte
  (lambda (e)
    (cond
      [(is-literal? e)
       (make-literal (literal-1 e))]
      [(is-plus? e)
       (make-plus (interpret-arithmetic-expression_Magritte (plus-1 e))
                  (interpret-arithmetic-expression_Magritte (plus-2 e)))]
      [(is-times? e)
       (make-times (interpret-arithmetic-expression_Magritte (times-1 e))
                   (interpret-arithmetic-expression_Magritte (times-2 e)))]
      [else
       (errorf 'interpret-arithmetic-expression_Magritte
               "unrecognized expression: ~s"
               e)])))

(unless (test-interpret-arithmetic-expression_Magritte interpret-arithmetic-expression_Magritte)
  (printf "fail: (test-interpret-arithmetic-expression_Magritte interpret-arithmetic-expression_Magritte)~n"))

;;;;;;;;;;

;;; unit test:

(define does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte ae)
              (compile-and-run-arithmetic-expression_Magritte ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?)~n"))

;;;;;;;;;;;;;;;;;;;;

;;; Bizarre exercise:

(define compile-arithmetic-expression_bizarre
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (list (make-PUSH (literal-1 e)))]
                  [(is-plus? e)
                   (visit-plus (plus-1 e)
                               (lambda (e1 prepend a1)
                                 (prepend e1
                                          (visit-plus (plus-2 e)
                                                      (lambda (e2 prepend a2)
                                                        (prepend e2 a2))
                                                      a1)))
                               (list (make-ADD)))]
                  [(is-times? e)
                   (visit-times (times-1 e)
                                (lambda (e1 prepend a1)
                                  (prepend e1
                                           (visit-times (times-2 e)
                                                        (lambda (e2 prepend a2)
                                                          (prepend e2 a2))
                                                        a1)))
                                (list (make-MUL)))]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-plus
              (lambda (e k a)
                (cond
                  [(is-literal? e)
                   (k (make-PUSH (literal-1 e)) cons a)]
                  [(is-plus? e)
                   (visit-plus (plus-1 e)
                               (lambda (e1 prepend a1)
                                 (prepend e1
                                          (visit-plus (plus-2 e)
                                                      k
                                                      a1)))
                               (cons (make-ADD) a))]
                  [(is-times? e)
                   (k (visit-times (times-1 e)
                                   (lambda (e1 prepend a1)
                                     (prepend e1
                                              (visit-times (times-2 e)
                                                           (lambda (e2 prepend a2)
                                                             (prepend e2 a2))
                                                           a1)))
                                   (list (make-MUL)))
                      append
                      a)]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-times
              (lambda (e k a)
                (cond
                  [(is-literal? e)
                   (k (make-PUSH (literal-1 e)) cons a)]
                  [(is-plus? e)
                   (k (visit-plus (plus-1 e)
                                  (lambda (e1 prepend a1)
                                    (prepend e1
                                             (visit-plus (plus-2 e)
                                                         (lambda (e2 prepend a2)
                                                           (prepend e2 a2))
                                                         a1)))
                                  (list (make-ADD)))
                      append
                      a)]
                  [(is-times? e)
                   (visit-times (times-1 e)
                                (lambda (e1 prepend a1)
                                  (prepend e1
                                           (visit-times (times-2 e)
                                                        k
                                                        a1)))
                                (cons (make-MUL) a))]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))])
      (make-byte-code-program (visit e)))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_bizarre run-byte-code-program)
  (printf "fail: (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_bizarre run-byte-code-program)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_bizarre
  (lambda (e)
    (run-byte-code-program (compile-arithmetic-expression_bizarre e))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_bizarre)
  (printf "fail: (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_bizarre)~n"))

;;;;;;;;;;

;;; Does interpreting an expression give the same result as
;;; compiling this expression and running the compiled program?

(define does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_bizarre ae)))))

(define test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_bizarre
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_bizarre e))))

;;;;;;;;;;

;;; ***
;;; Write the BNF of the output of compile-and-run-arithmetic-expression_Magritte_bizarre
;;; and implement a syntax checker for it:

;;; <arithmetic-expression_bizarre>
;;; ::= (literal <number>)
;;;   | (plus <arithmetic-expression_bizarre-plus> <arithmetic-expression_bizarre>)
;;;   | (times <arithmetic-expression_bizarre-times> <arithmetic-expression_bizarre>)
;;; 
;;; <arithmetic-expression_bizarre-plus>
;;; ::= (literal <number>)
;;;   | (times <arithmetic-expression_bizarre-times> <arithmetic-expression_bizarre>)
;;; 
;;; <arithmetic-expression_bizarre-times>
;;; ::= (literal <number>)
;;;   | (plus <arithmetic-expression_bizarre-plus> <arithmetic-expression_bizarre>)

(define syntax-check-bizarre
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [(is-plus? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [(is-times? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [else
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]))]
             [visit-plus
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [(is-plus? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [(is-times? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [else
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]))]
             [visit-times
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [(is-plus? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [(is-times? e)
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]
                  [else
                   (errorf 'syntax-check-bizarre
                           "not implemented yet")]))])
      (visit e))))

(define test-bizarre-compiler
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-bizarre
               (compile-and-run-arithmetic-expression_Magritte_bizarre
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test-bizarre-compiler)
;;;   (printf "fail: (test-bizarre-compiler)~n"))

;;;;;;;;;;

;;; ***
;;; Write the corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_bizarre
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (make-literal (literal-1 e))]
                  [(is-plus? e)
                   (errorf 'interpret-arithmetic-expression_Magritte_bizarre
                           "not implemented yet")]
                  [(is-times? e)
                   (errorf 'interpret-arithmetic-expression_Magritte_bizarre
                           "not implemented yet")]
                  [else
                   (errorf 'interpret-arithmetic-expression_Magritte_bizarre
                           "illegal input: ~s"
                           e)]))])
      (visit e))))

;;; ***
;;; Is your bizarre Magritte interpreter structurally recursive?
;;; Can you write it with fold-right_arithmetic-expression?

;;; ***
;;; In plain English, which bizarre program transformation is performed?

;;; What: additions and multiplications are re-associated to the right.
;;; How: ???

;;;;;;;;;;

(define does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_bizarre ae)
              (compile-and-run-arithmetic-expression_Magritte_bizarre ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?)
;;;   (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?)~n"))

;;;;;;;;;;

;;; Verify that the bizarre syntax checker
;;; accepts the output of your bizarre Magritte interpreter:

(define test-bizarre-Magritte-interpreter
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-bizarre
               (interpret-arithmetic-expression_Magritte_bizarre
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test-bizarre-Magritte-interpreter)
;;;   (printf "(test-bizarre-Magritte-interpreter) failed~n"))

;;;;;;;;;;

(define is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_bizarre ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_bizarre ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?)
;;;   (printf "fail: (test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?)~n"))

;;;;;;;;;;

;;; ***
;;; Which is more efficient:
;;; interpreting an expression with your bizarre Magritte interpreter, or
;;; compiling it with the bizarre Magritte compiler and decompiling the result?

(define generate-random-arithmetic-expression
  (lambda (depth_init)
    (letrec ([visit (lambda (depth)
                      (if (= depth 0)
                          (make-literal (random 100))
                          (case (random 5)
                            [(0)
                             (make-literal (- (random 100)))]
                            [(1 2)
                             (make-plus (visit (1- depth))
                                        (visit (1- depth)))]
                            [else
                             (make-times (visit (1- depth))
                                         (visit (1- depth)))])))])
      (if (and (integer? depth_init)
               (not (negative? depth_init)))
          (visit depth_init)
          (errorf 'generate-random-arithmetic-expression
                  "not a non-negative integer: ~s"
                  depth_init)))))

(define fold-right_natural-number
  (lambda (zero-case succ-case . rest)
    (let ([else-case (cond
                       [(null? rest)
                        (lambda (v)
                          (errorf 'fold-right_natural-number
                                  "not a non-negative integer: ~s"
                                  v))]
                       [(null? (cdr rest))
                        (car rest)]
                       [else
                        (errorf 'fold-right_natural-number
                                "too many parameters: ~s"
                                rest)])])
      (lambda (n_init)
        (letrec ([visit (lambda (i)
                          (if (zero? i)
                              zero-case
                              (succ-case (visit (- i 1)))))])
          (if (and (integer? n_init)
                   (not (negative? n_init)))
              (visit n_init)
              (else-case n_init)))))))

;;; Exercise 3

(define generate-random-arithmetic-expression_alt
  (lambda (depth_init)
    (if (and (integer? depth_init)
             (not (negative? depth_init)))
        ((fold-right_natural-number (make-literal (random 100))
                                    (lambda (c)
                                      (case (random 5)
                                        [(0)
                                         (make-literal (- (random 100)))]
                                        [(1 2)
                                         (make-plus c c)]
                                        [else
                                         (make-times c c)])))
         depth_init)
        (errorf 'generate-random-arithmetic-expression_alt
                "not a non-negative integer: ~s"
                depth_init))))

;;;;;;;;;;;;;;;;;;;;

;;; This doesn't work as expected, since we didn't think about
;;; Scheme beeing call by value, which results in the calls to the
;;; random procedure only beeing evaluated once.

(define generate-random-arithmetic-expression_alt-fix
  (lambda (depth_init)
    (if (and (integer? depth_init)
             (not (negative? depth_init)))
        (((fold-right_natural-number
           (lambda (x) (make-literal (random 100)))
           (lambda (c)
             (case (random 5)
               [(0)
                (lambda (x) (make-literal (- (random 100))))]
               [(1 2)
                (lambda (x) (make-plus (c 0) (c 0)))]
               [else
                (lambda (x) (make-times (c 0) (c 0)))])))
         depth_init) 0)
    (errorf 'generate-random-arithmetic-expression_alt-fix
            "not a non-negative integer: ~s"
            depth_init))))

;;; We know that the (random 5) call is still only beeing evaluated once,
;;; at every depth.

;;; end of week-04_the-bizarre-optimizing-compiler.scm

"week-04_the-bizarre-optimizing-compiler.scm"
