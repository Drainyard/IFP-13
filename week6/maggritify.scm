;;; week-06_evaluation-and-reification.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 04 Oct 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-06_evaluation-and-reification.html

;;;;;;;;;;;;;;;;;;;;

;;; The usual auxiliary procedures for the unit tests:

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

;;;;;;;;;;;;;;;;;;;;

;;; The simplest implementation of reification

(define reify-first-order-value
  (lambda (v)
    `',v))    ;;; i.e., (list 'quote v)

;;;;;;;;;;

;;; A structural approach to reification

(define reify-number-defensively
  (lambda (v)
    (if (number? v)
        v
        (errorf 'reify-number
                "not a number: ~s"
                v))))

(define reify-number
  (lambda (n)
    n))

(define reify-boolean-defensively
  (lambda (v)
    (if (boolean? v)
        v
        (errorf 'reify-boolean-defensively
                "not a boolean: ~s"
                v))))

(define reify-boolean
  (lambda (b)
    b))

(define reify-char-defensively
  (lambda (v)
    (if (char? v)
        v
        (errorf 'reify-char-defensively
                "not a char: ~s"
                v))))

(define reify-char
  (lambda (c)
    c))

(define reify-string-defensively
  (lambda (v)
    (if (string? v)
        v
        (errorf 'reify-string-defensively
                "not a string: ~s"
                v))))

(define reify-string
  (lambda (s)
    s))

(define reify-symbol-defensively
  (lambda (v)
    (if (symbol? v)
        `',v
        (errorf 'reify-symbol-defensively
                "not a symbol: ~s"
                v))))

(define reify-symbol
  (lambda (x)
    `',x))

(define reify-the-empty-list-defensively
  (lambda (v)
    (if (null? v)
        `',v
        (errorf 'reify-the-empty-list-defensively
                "not the empty list: ~s"
                v))))

(define reify-the-empty-list
  (lambda (nil)
    `',nil))

(define reify-pair-of-symbols-and-numbers-defensively
  (lambda (v)
    (if (pair? v)
        `(cons ,(reify-symbol-defensively (car v))
               ,(reify-number-defensively (cdr v)))
        (errorf 'reify-pair-of-symbols-and-numbers-defensively
                "not a pair: ~s"
                v))))

(define reify-pair-of-symbols-and-numbers
  (lambda (p)
    `(cons ,(reify-symbol (car p))
           ,(reify-number (cdr p)))))

;;;;;;;;;;;;;;;;;;;;

(define alist-mt
  '())

(define alist-extend
  (lambda (name denotable environment)
    (cons (cons name denotable)
          environment)))

(define alist-lookup
  (lambda (name environment found not-found)
    (letrec ([visit (lambda (e)
                      (if (null? e)
                          (not-found)
                          (let ([binding (car e)])
                            (if (equal? name (car binding))
                                (found (cdr binding))
                                (visit (cdr e))))))])
      (visit environment))))

(define test-reify-environment-binding-symbols-to-numbers
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            'alist-mt
                            '())
             (try-candidate name
                            candidate
                            '(alist-extend 'z 30 alist-mt)
                            '((z . 30)))
             (try-candidate name
                            candidate
                            '(alist-extend 'y 20 (alist-extend 'z 30 alist-mt))
                            '((y . 20) (z . 30)))
             (try-candidate name
                            candidate
                            '(alist-extend 'x 10 (alist-extend 'y 20 (alist-extend 'z 30 alist-mt)))
                            '((x . 10) (y . 20) (z . 30)))
               ;;; etc.
             )))

(define test-reify-environment-binding-symbols-to-strings
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            'alist-mt
                            '())
             (try-candidate name
                            candidate
                            '(alist-extend 'z "30" alist-mt)
                            '((z . "30")))
             (try-candidate name
                            candidate
                            '(alist-extend 'y "20" (alist-extend 'z "30" alist-mt))
                            '((y . "20") (z . "30")))
             (try-candidate name
                            candidate
                            '(alist-extend 'x "10" (alist-extend 'y "20" (alist-extend 'z "30" alist-mt)))
                            '((x . "10") (y . "20") (z . "30")))
               ;;; etc.
             )))

(define reify-environment
  (lambda (reify-name reify-denotation)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(null? v)
                           'alist-mt]
                          [(pair? v)
                           (let ([binding (car v)])
                             (if (pair? binding)
                                 `(alist-extend ,(reify-name (car binding))
                                                ,(reify-denotation (cdr binding))
                                                ,(visit (cdr v)))
                                 (errorf 'reify-environment
                                         "not a binding: ~s"
                                         binding)))]
                          [else
                           (errorf 'reify-environment
                                   "not an environment: ~s"
                                  v)]))])
        (visit v_init)))))

(define reify-environment-binding-symbols-to-numbers
  (reify-environment reify-symbol reify-number))

(unless (test-reify-environment-binding-symbols-to-numbers 'reify-environment-binding-symbols-to-numbers reify-environment-binding-symbols-to-numbers)
  (printf "(test-reify-environment-binding-symbols-to-numbers 'reify-environment-binding-symbols-to-numbers reify-environment-binding-symbols-to-numbers) failed~n"))

(define reify-environment-binding-symbols-to-strings
  (reify-environment reify-symbol reify-string))

(unless (test-reify-environment-binding-symbols-to-strings 'reify-environment-binding-symbols-to-strings reify-environment-binding-symbols-to-strings)
  (printf "(test-reify-environment-binding-symbols-to-strings 'reify-environment-binding-symbols-to-strings reify-environment-binding-symbols-to-strings) failed~n"))


;;;;;;;;;;;;;;;;;;
;;; Exercise 4 ;;;
;;;;;;;;;;;;;;;;;;


(define test-reify-environment-binding-symbols-to-symbols
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            'alist-mt
                            '())
             (try-candidate name
                            candidate
                            '(alist-extend 'z 'baz alist-mt)
                            '((z . baz)))
             (try-candidate name
                            candidate
                            '(alist-extend 'y 'bar (alist-extend 'z 'baz alist-mt))
                            '((y . bar) (z . baz)))
             (try-candidate name
                            candidate
                            '(alist-extend 'x 'foo (alist-extend 'y 'bar (alist-extend 'z 'baz alist-mt)))
                            '((x . foo) (y . bar) (z . baz)))
              (try-candidate name
                            candidate
                            '(alist-extend 'foo 'bar (alist-extend 'bar 'baz alist-mt))
                            '((foo . bar) (bar . baz)))
              (try-candidate name
                            candidate
                            '(alist-extend '1a '2b (alist-extend 'b2 'a1 alist-mt))
                            '((1a . 2b) (b2 . a1)))
              (try-candidate name
                            candidate
                            '(alist-extend 'we 'can (alist-extend 'bind 'anything alist-mt))
                            '((we . can) (bind . anything)))
              (try-candidate name
                            candidate
                            '(alist-extend '+ '- (alist-extend '* '/ alist-mt))
                            '((+ . -) (* . /)))
             )))


;;; Building on the code given in the lecture note, the solution to this 
;;; exercise is simply a version of reify-environment, which reifies both the
;;; names and denotations as symbols.


(define reify-environment-binding-symbols-to-symbols
  (reify-environment reify-symbol reify-symbol))

(unless (test-reify-environment-binding-symbols-to-symbols 'reify-environment-binding-symbols-to-symbols reify-environment-binding-symbols-to-symbols)
  (printf "fail: (test-reify-environment-binding-symbols-to-symbols 'reify-environment-binding-symbols-to-symbols reify-environment-binding-symbols-to-symbols) ~n"))



;;;;;;;;;;;;;;;;;;
;;; Exercise 5 ;;;
;;;;;;;;;;;;;;;;;;


(define test-reify-first-path
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            '(lambda (x) (car x))
                            'x 1 '(1))
             (try-candidate name
                            candidate
                            '(lambda (x) (car x))
                            'x '10 '(10 20 30))
             (try-candidate name
                            candidate
                            '(lambda (y) (car (cdr y)))
                            'y '20 '(10 20 30))
             (try-candidate name
                            candidate
                            '(lambda (z) (car (cdr (cdr z))))
                            'z '30 '(10 20 30))
             (try-candidate name
                            candidate
                            #f
                            'z '3 '(10 20 30))
             (try-candidate name
                            candidate
                            '(lambda (nvm) (car (car nvm)))
                            'nvm '10 '((10) (20) (30)))
             (try-candidate name
                            candidate
                            '(lambda (z) (car (car (car z))))
                            'z '10 '(((10)) 10))
             )))


(define reify-first-path_a
  (lambda (symb n tr)
    (letrec ([visit (lambda (tree acc k)
                      (cond 
                       [(null? tree) 
                        (k #f)]
                       [(number? tree)
                        (if (equal? tree n) 
                            (k acc) 
                            (k #f))]
                       [(pair? tree)
                        (visit (car tree) 
                               `(car ,acc)
                               (lambda (left)
                                 (cond
                                  [(boolean? left)
                                   (visit (cdr tree) 
                                          `(cdr ,acc) 
                                          (lambda (right) 
                                            (cond
                                             [(boolean? right)
                                              (k #f)]
                                             [else 
                                              (k right)])))]
                                  [else 
                                   (k left)])))]))])
      (visit tr symb (lambda (x) (if x
                                     `(lambda (,symb) ,x)
                                     #f))))))


;;; This procedure uses both an accumulator and a continuation. It is possible 
;;; to split the continuation into two, which are applied on boolean part-
;;; results and on proper ones respectively.


(unless (test-reify-first-path 'reify-first-path_a reify-first-path_a)
  (printf "fail: (test-reify-first-path 'reify-first-path reify-first-path_a) ~n"))


;;; reify-last-path

(define test-reify-last-path
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            '(lambda (x) (car x))
                            'x 1 '(1))
             (try-candidate name
                            candidate
                            '(lambda (x) (car x))
                            'x '10 '(10 20 30))
             (try-candidate name
                            candidate
                            '(lambda (y) (car (cdr y)))
                            'y '20 '(20 20 30))
             (try-candidate name
                            candidate
                            '(lambda (z) (car (cdr (cdr z))))
                            'z '30 '(30 20 30))
             (try-candidate name
                            candidate
                            #f
                            'z '3 '(10 20 30))
             (try-candidate name
                            candidate
                            '(lambda (nvm) (car (car nvm)))
                            'nvm '10 '((10) (20) (30)))
             (try-candidate name
                            candidate
                            '(lambda (z) (car (car (car z))))
                            'z '10 '(((10)) 20))
             (try-candidate name
                            candidate
                            '(lambda (y) (car (cdr (cdr y))))
                            'y '10 '(10 10 10))
             )))

;;; Reify-last-path is very easy to write after reify-first-path, since the last
;;; occurrence in a depth-first, left to right traversal is the same as the 
;;; first occurrence in a depth-first, right to left traversal.

(define reify-last-path_a
  (lambda (symb n tr)
    (letrec ([visit (lambda (tree acc k)
                      (cond 
                       [(null? tree) 
                        (k #f)]
                       [(number? tree)
                        (if (equal? tree n) 
                            (k acc) 
                            (k #f))]
                       [(pair? tree)
                        (visit (cdr tree) 
                               `(cdr ,acc)
                               (lambda (right)
                                 (cond
                                  [(boolean? right)
                                   (visit (car tree) 
                                          `(car ,acc) 
                                          (lambda (left) 
                                            (cond
                                             [(boolean? left)
                                              (k #f)]
                                             [else 
                                              (k left)])))]
                                  [else 
                                   (k right)])))]))])
      (visit tr symb (lambda (x) (if x
                                     `(lambda (,symb) ,x)
                                     #f))))))



(unless (test-reify-last-path 'reify-last-path_a reify-last-path_a)
  (printf "fail: (test-reify-last-path 'reify-last-path_a reify-last-path_a) ~n"))
  
;;; reify-nth-path

(define test-reify-nth-path
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            '(lambda (x) (car x))
                            'x 1 '(1) 1)
             (try-candidate name
                            candidate
                            '(lambda (x) (car x))
                            'x '10 '(10 20 30) 1)
             (try-candidate name
                            candidate
                            '(lambda (y) (car (cdr y)))
                            'y '20 '(20 20 30) 2)
             (try-candidate name
                            candidate
                            '(lambda (z) (car (cdr (cdr z))))
                            'z '30 '(30 30 30) 3)
             (try-candidate name
                            candidate
                            #f
                            'z '3 '(10 20 30) 1)
             (try-candidate name
                            candidate
                            '(lambda (z) (car (cdr z)))
                            'z '10 '(((10)) 10) 2)
             (try-candidate name
                            candidate
                            #f
                            'z '10 '(((10)) 10) 3)
             )))


;;; Again, this solution is very much based on the reify-first-path procedure. 
;;; The only difference is that we added one extra argument in both the 
;;; recursive function and the accumulator. This argument counts the number of
;;; occurences seen so far and, whenever a new one is found, checks whether 
;;; this is the desired one. 

(define reify-nth-path_a
  (lambda (symb n tr i)
    (letrec ([visit (lambda (tree acc k occ)
                      (cond 
                       [(null? tree) 
                        (k #f occ)]
                       [(number? tree)
                        (if (equal? tree n)
                            (if (equal? occ i)
                                (k acc occ)
                                (k #f (1+ occ)))
                            (k #f occ))]
                       [(pair? tree)
                        (visit (car tree) 
                               `(car ,acc)
                               (lambda (left locc)
                                 (cond
                                  [(boolean? left)
                                   (visit (cdr tree) 
                                          `(cdr ,acc) 
                                          (lambda (right rocc) 
                                            (cond
                                             [(boolean? right)
                                              (k #f rocc)]
                                             [else 
                                              (k right rocc)]))
                                          locc)]
                                  [else 
                                   (k left locc)]))
                               occ)]))])
      (visit tr symb (lambda (x focc) (if x
                                     `(lambda (,symb) ,x)
                                     #f)) 1))))


(unless (test-reify-nth-path 'reify-nth-path_a reify-nth-path_a)
  (printf "fail: (test-reify-nth-path 'reify-nth-path_a reify-nth-path_a) ~n"))

;;;;;;;;;;;;;;;;;;;;
;;; Our understanding of a binary tree of numbers constructed with proper lists is that all of these trees
;;; must, at the very least, be proper lists. This means that they are constructed with nested calls to cons,
;;; where the right-most call is given the empty list as a second argument. So they are always constructed
;;; with pairs, which gives the definition of the base-case non-terminal <binary-tree-numbers-proper-list>
;;; seen below.
;;; Moreover, the left-most call has to have a number as a first argument in order to result in a binary tree
;;; of numbers.
;;; This means we have to differentiate between the left and the right argument of the cons calls.
;;; All in all we get the following BNF.

;;; <binary-tree-numbers-proper-list>       ::= (<binary-tree-numbers-proper-list-left> .
;;;                                              <binary-tree-numbers-proper-list-right>)
;;;
;;; <binary-tree-numbers-proper-list-left>  ::= <binary-tree-numbers-proper-list>
;;;                                           | <number>
;;; <binary-tree-numbers-proper-list-right> ::= <binary-tree-numbers-proper-list>
;;;                                           | <empty-list>

;;; The BNF is sound because every construct derived from it will be a proper list with numbers as the left-
;;; most element of every cons call, which is exactly our definition of a binary tree of numbers constructed
;;; with proper lists.
;;; Any binary tree of numbers constructed with proper lists can be constructed with nested cons calls with
;;; numbers as the left-most argument and proper lists as the right-most argument. Therefore the above
;;; grammar is also complete.
;;; The below fold is derived from this BNF.


(define fold-right_binary-tree-from-proper-lists
  (lambda (nill-case number-case pair-case else-case)
    (lambda (v_init)
      (letrec ([visit-base (lambda (v)
                             (cond
                               [(pair? v)
                                (pair-case (visit-left (car v))
                                           (visit-right (cdr v)))]
                               [else
                                (else-case v)]))]
               [visit-left (lambda (v)
                             (cond
                               [(number? v)
                                (number-case v)]
                               [else
                                (visit-base v)]))]
               [visit-right (lambda (v)
                              (cond
                                [(null? v)
                                 (nill-case v)]
                                [else
                                 (visit-base v)]))])
                           (visit-base v_init)))))


;;; Now we should be able to write new versions of the above procedures:

;;; reify-first-path


(define reify-first-path_b
  (lambda (symb n tr)
    (((fold-right_binary-tree-from-proper-lists
     (lambda (v)
       (lambda (acc k)
         (k #f)))
     (lambda (v)
       (lambda (acc k)
         (if (equal? n v) 
             (k acc) 
             (k #f))))
     (lambda (n1 n2)
       (lambda (acc k)
         (n1 `(car ,acc)
             (lambda (left)
               (cond
                 [(boolean? left)
                  (n2 `(cdr ,acc) 
                      (lambda (right) 
                        (cond
                          [(boolean? right)
                           (k #f)]
                          [else
                           (k right)])))]
                 [else
                  (k left)])))))
     (lambda (v)
       (errorf 'reify-first-path_b
               "Not a proper binary tree from proper lists: ~s"
               tr))) tr) symb (lambda (x) (if x
                                     `(lambda (,symb) ,x)
                                     #f)))))

(unless (test-reify-first-path 'reify-first-path_b reify-first-path_b)
  (printf "fail: (test-reify-first-path 'reify-first-path_b reify-first-path_b) ~n"))

;;; reify-last-path_b

(define reify-last-path_b
  (lambda (symb n tr)
    (((fold-right_binary-tree-from-proper-lists
     (lambda (v)
       (lambda (acc k)
         (k #f)))
     (lambda (v)
       (lambda (acc k)
         (if (equal? n v) 
             (k acc) 
             (k #f))))
     (lambda (n1 n2)
       (lambda (acc k)
         (n2 `(cdr ,acc)
             (lambda (right)
               (cond
                 [(boolean? right)
                  (n1 `(car ,acc) 
                      (lambda (left) 
                        (cond
                          [(boolean? left)
                           (k #f)]
                          [else
                           (k left)])))]
                 [else
                  (k right)])))))
     (lambda (v)
       (errorf 'reify-last-path_b
               "Not a proper binary tree from proper lists: ~s"
               tr))) tr) symb (lambda (x) (if x
                                     `(lambda (,symb) ,x)
                                     #f)))))

(unless (test-reify-last-path 'reify-last-path_b reify-last-path_b)
  (printf "fail: (test-reify-last-path 'reify-last-path_b reify-last-path_b) ~n"))

;;; reify-nth-path_b


(define reify-nth-path_b
  (lambda (symb n tr i)
    (((fold-right_binary-tree-from-proper-lists
     (lambda (v)
       (lambda (acc k occ)
         (k #f occ)))
     (lambda (v)
       (lambda (acc k occ)
         (if (equal? n v) 
             (if (equal? occ i)
                 (k acc occ)
                 (k #f (1+ occ)))
             (k #f occ))))
     (lambda (n1 n2)
       (lambda (acc k occ)
         (n1 `(car ,acc)
             (lambda (left locc)
               (cond
                 [(boolean? left)
                  (n2 `(cdr ,acc) 
                      (lambda (right rocc) 
                        (cond
                          [(boolean? right)
                           (k #f rocc)]
                          [else
                           (k right rocc)]))
                      locc)]
                 [else
                  (k left locc)]))
             occ)))
     (lambda (v)
       (errorf 'reify-nth-path_b
               "Not a proper binary tree from proper lists: ~s"
               tr))) tr) symb (lambda (x focc) (if x
                                     `(lambda (,symb) ,x)
                                     #f)) 1)))

(unless (test-reify-nth-path 'reify-nth-path_b reify-nth-path_b)
  (printf "fail: (test-reify-nth-path 'reify-nth-path_b reify-nth-path_b) ~n"))

;;; end of week-06_evaluation-and-reification.scm

"week-06_evaluation-and-reification.scm"








