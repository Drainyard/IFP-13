;;; week-02_processing-binary-trees.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 05 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/lecture-notes/week-02_processing-binary-trees.html

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

(define test-check_binary-tree
  (lambda (candidate)
    (and-all ;;; some positive tests:
             (try-candidate 'test-check_binary-tree
                            candidate
                            #t
                            0)
             (try-candidate 'test-check_binary-tree
                            candidate
                            #t
                            (cons 0 0))
             (try-candidate 'test-check_binary-tree
                            candidate
                            #t
                            (cons (cons 10 20)
                                  (cons 30 40)))
             ;;; and some negative tests:
             (try-candidate 'test-check_binary-tree
                            candidate
                            #f
                            (cons (cons 10 20)
                                  (cons 30 "40")))
             (try-candidate 'test-check_binary-tree
                            candidate
                            #f
                            (list 1 2 3))
             (try-candidate 'test-check_binary-tree
                            candidate
                            #f
                            'foo)
             ;;; an incorrect test:
             ;; (try-candidate 'test-check_binary-tree
             ;;                candidate
             ;;                #t
             ;;                'foo)
             ;; (try-candidate 'test-check_binary-tree
             ;;                candidate
             ;;                #f
             ;;                (cons 1 2))
             ;;; etc.
             )))

(define check_binary-tree
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         #t]
                        [(pair? v)
                         (and (visit (car v))
                              (visit (cdr v)))]
                        [else
                         #f]))])
      (visit v_init))))

(unless (test-check_binary-tree check_binary-tree)
  (printf "fail: (test-check_binary-tree check_binary-tree)~n"))

;;; Exercise 3
;;; ----------
;;; 
;;; * Edit the accompanying Scheme file (``C-x C-f
;;;   week-02_processing-binary-trees.scm``), and uncomment the incorrect
;;;   test in the definition of ``test-check_binary-tree``.  Then send the
;;;   content of the file to the Scheme process (``C-c C-b``), and verify
;;;   that the error message ``fail: (test-check_binary-tree
;;;   check_binary-tree)`` is displayed.
;;;   Done

;;; * In the definition of ``test-check_binary-tree``, the commented-out test
;;;   is incorrect in that the candidate syntax checker allegedly accepts an
;;;   ill-formed value.  Add another incorrect test where the candidate
;;;   syntax checker allegedly rejects a well-formed value, and verify that
;;;   the error message ``fail: (test-check_binary-tree check_binary-tree)``
;;;   is displayed when the content of the file is sent to the Scheme
;;;   process.
;;; 
;;; * Finally, to prevent future confusion, clean up your file by commenting
;;;   out the incorrect tests.

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

(define number-of-leaves
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         1]
                        [(pair? v)
                         (+ (visit (car v))
                            (visit (cdr v)))]
                        [else
                         (errorf 'number-of-leaves
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-number-of-leaves number-of-leaves)
  (printf "fail: (test-number-of-leaves number-of-leaves)~n"))

;;; Exercise 4
;;; ----------
;;; 
;;; Add another test in the definition of ``test-number-of-leaves`` and
;;; verify that ``number-of-leaves`` still passes the unit test.

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

(define number-of-nodes
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         0]
                        [(pair? v)
                         (1+ (+ (visit (car v))
                                (visit (cdr v))))]
                        [else
                         (errorf 'number-of-nodes
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-number-of-nodes number-of-nodes)
  (printf "fail: (test-number-of-nodes number-of-nodes)~n"))

;;;;;;;;;;

(define test-smallest-leaf
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
                            10
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-smallest-leaf
                            candidate
                            -20
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-smallest-leaf
                            candidate
                            10
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define smallest-leaf
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (min (visit (car v))
                              (visit (cdr v)))]
                        [else
                         (errorf 'smallest-leaf
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-smallest-leaf smallest-leaf)
  (printf "fail: (test-smallest-leaf smallest-leaf)~n"))

;;; Exercise 5
;;; ----------
;;; 
;;; Reminder: ``max`` denotes the predefined Scheme procedure that returns
;;; the largest of its arguments::
;;; 
;;;     > (max 2 3)
;;;     3
;;;     > (max 2 -3)
;;;     2
;;;     > 
;;; 
;;; What would happen if we were to replace ``min`` by ``max`` in the
;;; definition of ``smallest-leaf``?

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

(define weight
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (+ (visit (car v))
                            (visit (cdr v)))]
                        [else
                         (errorf 'weight
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-weight weight)
  (printf "fail: (test-weight weight)~n"))

;;; Exercise 6
;;; ----------
;;; 
;;; Describe how to compute the weight of a binary tree in English.

;;;;;;;;;;

(define test-height
  (lambda (candidate)
    (and-all (try-candidate 'test-height
                            candidate
                            0
                            0)
             (try-candidate 'test-height
                            candidate
                            1
                            (cons 1
                                  1))
             (try-candidate 'test-height
                            candidate
                            2
                            (cons (cons 2
                                        2)
                                  (cons 2
                                        2)))
             (try-candidate 'test-height
                            candidate
                            2
                            (cons (cons 2
                                        2)
                                  1))
             (try-candidate 'test-height
                            candidate
                            3
                            (cons (cons 2
                                        2)
                                  (cons (cons 3
                                              3)
                                        2)))
             (try-candidate 'test-height
                            candidate
                            4
                            (cons (cons 2
                                        (cons (cons 4
                                                    4)
                                              3))
                                  (cons (cons 3
                                              3)
                                        2)))
             ;;; etc.
             )))

;;; Exercise 7
;;; ----------
;;; 
;;; Describe how to compute the height of a binary tree:
;;; 
;;; * in English, and
;;; The height of a leaf is 0
;;; Given a well-formed tree t1, whose height is h1, and a well-formed tree t2,
;;; whose height is h2, the height of the tree whose left subtree is t1 and
;;; whose right subtree is t2 is the maximum of h1 and h2 +1.
;;; 
;;; * functionally.
;;; For all numbers n, height = 0
;;; For all well-formed binary trees t1 and t2, 
;;; height (cons t1 t2) = 1+ max( height t1, height t2)
;;; For all other values v, height is undefined
;;;
;;; Then define a Scheme procedure ``height`` that given a binary tree,
;;; computes its height.  If it is given another value than a well-formed
;;; binary tree, your procedure should raise an error.  Verify that it passes
;;; the unit test.

(define height
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         0]
                        [(pair? v)
                         (1+ (max (visit (car v))
                                  (visit (cdr v))))]
                        [else
                         (errorf 'height
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-height height)
  (printf "fail: (test-height height)~n"))

;;;;;;;;;;


(define test-width
  (lambda (candidate)
    (and-all (try-candidate 'test-width
                            candidate
                            1
                            1)
             (try-candidate 'test-width
                            candidate
                            2
                            (cons 1
                                  2))
             (try-candidate 'test-width
                            candidate
                            2
                            (cons (cons 11
                                        12)
                                  2))
             (try-candidate 'test-width
                            candidate
                            4
                            (cons (cons 1
                                        2)
                                  (cons 3
                                        4)))
             (try-candidate 'test-width
                            candidate
                            6
                            (cons (cons (cons 1
                                              2)
                                        0)
                                  (cons (cons 3
                                              4)
                                        (cons 5
                                              6))))
             (try-candidate 'test-width
                            candidate
                            6
                            (cons (cons (cons 1
                                              2)
                                        0)
                                  (cons (cons (cons 31
                                                    32)
                                              4)
                                        (cons 5
                                              (cons 61
                                                    62)))))
             ;;; etc.
             )))

;;; Exercise 8
;;; ----------
;;; 
;;; Describe how to compute the width of a binary tree:
;;; 
;;; * in English,
;;; Every node and leaf increments the width at its depth by 1
;;; The width of a binary tree is then the maximum of a list of numbers 
;;; representing the width of every depth
;;; 
;;; * logically, and
;;; We do not know how to do this 
;;;
;;; * functionally.
;;; For all numbers n, width-list = '(1)
;;; For all well-formed binary trees t, with subtrees t1 and t2 whose 
;;; corresponding width-lists are w1 and w2, width-list = '(1 (list-sum w1 w2))
;;; width t = (apply max width-list)
;;;
;;; Then define a Scheme procedure ``width`` that given a binary tree,
;;; computes its width.  If it is given another value than a well-formed
;;; binary tree, your procedure should raise an error.  Verify that it passes
;;; the unit test.



(define list-sum
  (lambda (l1_init l2_init)
    (letrec ([visit (lambda (l1 l2)
                      (cond
                        [(null? l1)
                         (if (null? l2)
                             '()
                             l2)]
                        [(null? l2)
                         (if (null? l1)
                             '()
                             l1)]
                        [else
                         (cons (+ (car l1) 
                                  (car l2)) 
                               (visit (cdr l1) (cdr l2)))]))])
      (visit l1_init l2_init))))


(define width
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         (list 1)
                         ]
                        [(pair? v)
                         (cons 1 (list-sum (visit (car v))
                                   (visit (cdr v))))]
                        [else
                         (errorf 'width
                                 "not a binary tree: ~s"
                                 v)]))])
      (apply max (visit v_init)))))


(unless (test-width width)
  (printf "fail: (test-width width)~n"))

;;;;;;;;;;

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

;;; Exercise 9
;;; ----------
;;; 
;;; Describe how to flatten a binary tree using ``list`` and ``append``:
;;; 
;;; * in English,
;;; The flattening of a leaf is the list containing the leaf itself
;;; Given a well-formed tree t1, whose flattening is f1, and a well-formed tree
;;; t2, whose flattening is f2, the flattening of the tree t whose left subtree
;;; is t1 and whose right subtree is t2, is f1 followed by f2  
;;;
;;; * logically, and
;;; Logically, f is the flattening of the binary tree t whenever the following
;;; judgment holds: (%flatten t f) 
;;; This judgment is defined by the proof rules that can be seen on the attached
;;; picture
;;;
;;;
;;; * functionally.
;;; For all numbers n, flatten n = '(n)
;;; For all well-formed trees t1 and t2, 
;;; flatten (cons t1 t2) = (append (flatten t1) (flatten t2)
;;;
;;; 
;;; Then define a Scheme procedure ``flatten`` that given a binary tree,
;;; flattens it into its list of leaves.  If it is given another value than a
;;; well-formed binary tree, your procedure should raise an error.  Verify
;;; that it passes the unit test.

(define flatten
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         (list v)]
                        [(pair? v)
                         (append (visit (car v))
                                 (visit (cdr v)))]
                      [else
                       (errorf 'flatten
                               "not a binary tree: ~s"
                               v)]))])
    (visit v_init))))

(unless (test-flatten flatten)
  (printf "fail (test-flatten flatten)~n"))

;;;;;;;;;;

;;; Exercise 10
;;; -----------
;;; 
;;; a. Write a unit-test procedure for mobiles.  (NB. The leaf of a mobile
;;;    should be represented by a non-negative integer.)
;;; 
;;; b. Describe how to check whether a binary tree represents a well-balanced
;;;    mobile
;;; 
;;;    * in English
;;; A leaf is well-balanced
;;; A well-formed binary tree is well-balanced if both subtrees have the same
;;; weight
;;; 
;;;    * logically, and
;;; Logically, the  binary tree t is well-balanced whenever the following
;;; judgment holds: (%balanced t b)
;;; This judgment is defined by the proof rules that can be seen on the attached
;;; picture
;;;
;;;    * functionally.
;;; For all numbers n, balanced n = #t
;;; For all well-formed trees t1 and t2, 
;;; balanced (cons t1 t2) = (and (balanced t1) (balanced t2) (= (weight t1) 
;;; (weight t2))
;;;
;;; 
;;; c. Define a Scheme procedure ``well-balanced?`` that given a binary tree,
;;;    determines whether this tree represents a well-balanced mobile.  If it
;;;    is given another value than a well-formed binary tree, your procedure
;;;    should raise an error.  Verify that it passes the unit test.


(define test-mobile
  (lambda (candidate)
    (and-all (try-candidate 'test-mobile
                            candidate
                            #t
                            0)
             (try-candidate 'test-mobile
                            candidate
                            #t
                            (cons 0 0))
             (try-candidate 'test-mobile
                            candidate
                            #f
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-mobile
                            candidate
                            #f
                            (cons (cons 10 20)
                                  30))
             (try-candidate 'test-mobile
                            candidate
                            #t
                            (cons (cons 15 15)
                                  30))
             (try-candidate 'test-mobile
                            candidate
                            #f
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             (try-candidate 'test-mobile
                            candidate
                            #t
                            (cons (cons 50 50)
                                  (cons (cons 25 25)
                                        50)))
             ;;; etc.
             )))


(define well-balanced?
  (lambda (v_init)
    (number? (letrec ([visit (lambda (v)
                               (cond
                                 [(number? v)
                                  v]
                                 [(pair? v)
                                  (let ([n1 (visit (car v))])
                                        (if (number? n1)
                                            (let ([n2 (visit (cdr v))])
                                                  (if (and (number? n2)
                                                           (= n1 n2))
                                                      (+ n1 n2)
                                                      #f))
                                            #f))]
                                 [else
                                  (errorf 'well-balanced
                                          "not a binary tree: ~s"
                                          v)]))])
               (visit v_init)))))


(unless (test-mobile well-balanced?)
  (printf "fail (test-mobile well-balanced?)~n"))

;;;;;;;;;;

(define test-swap
  (lambda (candidate)
    (and-all (try-candidate 'test-swap
                            candidate
                            0
                            0)
             (try-candidate 'test-swap
                            candidate
                            (cons 0 0)
                            (cons 0 0))
             (try-candidate 'test-swap
                            candidate
                            (cons (cons 40 30)
                                  (cons 20 10))
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-swap
                            candidate
                            (cons 30
                                  (cons 20 10))
                            (cons (cons 10 20)
                                  30))
             (try-candidate 'test-swap
                            candidate
                            (cons (cons 50
                                        (cons 40 30))
                                  (cons 20 10))
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

;;; Exercise 11
;;; -----------
;;; 
;;; Describe how to swap each of the nodes of a binary tree:
;;; 
;;; * in English,
;;; 
;;; * logically, and
;;; 
;;; * functionally.
;;; 
;;; Then define a Scheme procedure that given a binary tree, swaps each of
;;; its nodes.  If it is given another value than a well-formed binary tree,
;;; your procedure should raise an error.  Verify that it passes the unit
;;; test.

(define snoc
  (lambda (a d)
    (cons d a)))

(define swap
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (snoc (visit (car v))
                               (visit (cdr v)))]
                        [else
                         (errorf 'swap
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-swap swap)
  (printf "fail: (test-swap swap)~n"))

;;;;;;;;;;

"week-02_processing-binary-trees.scm"

;;; end of week-02_processing-binary-trees.scm
