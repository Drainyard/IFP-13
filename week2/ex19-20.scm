;;; week-02_processing-binary-trees-using-an-accumulator.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 05 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/lecture-notes/week-02_processing-binary-trees-using-an-accumulator.html

;;;;;;;;;;

(define try-candidate
  (lambda (name candidate expected-output . input)
    (or (equal? expected-output
                (apply candidate input))
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

;;;;;;;;;;

;;; <binary-tree> ::= <number>
;;;                 | (<binary-tree> . <binary-tree>)
;;; 
;;; <number> ::= ...any Scheme number...

;;;;;;;;;;

(define test-number-of-leaves
  (lambda (candidate)
    (and-all (try-candidate 'test-number-of-leaves
                            candidate
                            1
                            0)
             (try-candidate 'test-number-of-leaves
                            candidate
                            2
                            (cons 0 0))
             (try-candidate 'test-number-of-leaves
                            candidate
                            4
                            (cons (cons 10 20)
                                  (cons 30 40)))
             ;;; etc.
             )))

(define compose
  (lambda (p q)
    (lambda (x)
      (p (q x)))))

(define number-of-leaves_acc-v0
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         (lambda (a)
                           (1+ a))]
                        [(pair? v)
                         (compose (visit (car v)) (visit (cdr v)))]
                        [else
                         (errorf 'number-of-leaves_acc-v0
                                 "not a binary tree: ~s"
                                 v)]))])
      ((visit v_init) 0))))

(unless (test-number-of-leaves number-of-leaves_acc-v0)
  (printf "fail: (test-number-of-leaves number-of-leaves_acc-v0)~n"))

;;; inline the definition of compose:

(define number-of-leaves_acc-v1
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         (lambda (a)
                           (1+ a))]
                        [(pair? v)
                         (lambda (a)
                           ((visit (car v)) ((visit (cdr v)) a)))]
                        [else
                         (errorf 'number-of-leaves_acc-v1
                                 "not a binary tree: ~s"
                                 v)]))])
      ((visit v_init) 0))))

(unless (test-number-of-leaves number-of-leaves_acc-v1)
  (printf "fail: (test-number-of-leaves number-of-leaves_acc-v1)~n"))

;;; streamline the error branch:

(define number-of-leaves_acc-v2
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         (lambda (a)
                           (1+ a))]
                        [(pair? v)
                         (lambda (a)
                           ((visit (car v)) ((visit (cdr v)) a)))]
                        [else
                         (lambda (a)
                           (errorf 'number-of-leaves_acc-v2
                                   "not a binary tree: ~s"
                                   v))]))])
      ((visit v_init) 0))))

(unless (test-number-of-leaves number-of-leaves_acc-v2)
  (printf "fail: (test-number-of-leaves number-of-leaves_acc-v2)~n"))

;;; factor the lambda-abstraction:

(define number-of-leaves_acc-v3
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (lambda (a)
                        (cond
                          [(number? v)
                           (1+ a)]
                          [(pair? v)
                           ((visit (car v)) ((visit (cdr v)) a))]
                          [else
                           (errorf 'number-of-leaves_acc-v3
                                   "not a binary tree: ~s"
                                   v)])))])
      ((visit v_init) 0))))

(unless (test-number-of-leaves number-of-leaves_acc-v3)
  (printf "fail: (test-number-of-leaves number-of-leaves_acc-v3)~n"))

;;; uncurry:

(define number-of-leaves_acc
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(number? v)
                         (1+ a)]
                        [(pair? v)
                         (visit (car v)
                                (visit (cdr v)
                                       a))]
                        [else
                         (errorf 'number-of-leaves_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init 0))))

(unless (test-number-of-leaves number-of-leaves_acc)
  (printf "fail: (test-number-of-leaves number-of-leaves_acc)~n"))

;;;;;;;;;;

(define test-number-of-nodes
  (lambda (candidate)
    (and-all (try-candidate 'test-number-of-nodes
                            candidate
                            0
                            0)
             (try-candidate 'test-number-of-nodes
                            candidate
                            1
                            (cons 0 0))
             (try-candidate 'test-number-of-nodes
                            candidate
                            3
                            (cons (cons 10 20)
                                  (cons 30 40)))
             ;;; etc.
             )))

(define number-of-nodes_acc
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(number? v)
                         a]
                        [(pair? v)
                         (visit (car v)
                                (visit (cdr v)
                                       (1+ a)))]
                        [else
                         (errorf 'number-of-nodes_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init 0))))

(unless (test-number-of-nodes number-of-nodes_acc)
  (printf "fail: (test-number-of-nodes number-of-nodes_acc)~n"))

;;;;;;;;;;

(define test-weight
  (lambda (candidate)
    (and-all (try-candidate 'test-smallest-leaf
                            candidate
                            0
                            0)
             (try-candidate 'test-smallest-leaf
                            candidate
                            0
                            (cons 0 0))
             (try-candidate 'test-smallest-leaf
                            candidate
                            100
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-smallest-leaf
                            candidate
                            20
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-smallest-leaf
                            candidate
                            150
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define weight_acc
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(number? v)
                         (+ v a)]
                        [(pair? v)
                         (visit (car v)
                                (visit (cdr v)
                                       a))]
                        [else
                         (errorf 'weight_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init 0))))

(unless (test-weight weight_acc)
  (printf "fail: (test-weight weight_acc)~n"))

;;;;;;;;;;


(define height_acc
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(number? v)
                         a]
                        [(pair? v)
                         (max (visit (car v) (1+ a))
                              (visit (cdr v )(1+ a)))]
                        [else
                         (errorf 'height_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init 0))))

(define height_acc_alt
  (lambda (v_init)
    (((fold-right_binary-tree (lambda (v)
                                (lambda (a)
                                  a))
                              (lambda (left right)
                                (lambda (a)
                                  (max (left (1+ a))
                                       (right (1+ a)))))
                              (lambda (v)
                                (errorf 'height_acc_alt
                                        "not a binary tree: ~s"
                                        v))) v_init) 0)))


(unless (test-height height_acc)
  (printf "fail: (test-height height_acc)~n"))

;;;;;;;;;

(define test-flatten
  (lambda (candidate)
    (and-all (try-candidate 'test-flatten
                            candidate
                            '(0)
                            0)
             (try-candidate 'test-flatten
                            candidate
                            '(0 1)
                            (cons 0 1))
             (try-candidate 'test-flatten
                            candidate
                            '(10 20 30 40)
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-flatten
                            candidate
                            '(10 -20 30)
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-flatten
                            candidate
                            '(10 20 30 40 50)
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define flatten_acc
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(number? v)
                         (cons v a)]
                        [(pair? v)
                         (visit (car v) (visit (cdr v) a))]
                        [else
                         (errorf 'flatten_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init '()))))

(define flatten_acc_alt
  (lambda (v_init)
    (((fold-right_binary-tree (lambda (l)
                                (lambda (a)
                                  (cons l a)))
                              (lambda (left right)
                                (lambda (a)
                                  (left (right a))))
                              (lambda (v)
                                (errorf 'flatten_acc_alt
                                        "not a binary tree: ~s"
                                        v))) v_init) '())))



(unless (test-flatten flatten_acc)
  (printf "fail (test-flatten flatten_acc)~n"))

;;;;;;;;;;

"week-02_processing-binary-trees-using-an-accumulator.scm"

;;; end of week-02_processing-binary-trees-using-an-accumulator.scm
