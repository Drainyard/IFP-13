;;; ex9.scm
;;; IFP 2016-2017, Q1

(define is-leaf?
  number?)

(define is-node?
  pair?)

(define node-1
  car)

(define node-2
  cdr)


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


(define test-lengths-of-all-distinct-paths
  (lambda (candidate)
    (and-all
     (try-candidate 'test-lengths-of-all-distinct-paths
                    candidate
                    (list 0 1 1)
                    (cons 2 2))
     (try-candidate 'test-lengths-of-all-distinct-paths
                    candidate
                    (list 0 1 2 2 1)
                    (cons (cons 2 4) 28))
     (try-candidate 'test-lengths-of-all-distinct-paths
                    candidate
                    (list 0 1 2 3 3 2 1 2 2)
                    (cons (cons (cons 2 3) 4) (cons 1 2))))))

(define lengths-of-all-distinct-paths
  (lambda (v_init)
    (letrec ([visit (lambda (v n)
                      (cons n
                            (cond
                              [(is-leaf? v)
                               '()]
                              [(is-node? v)
                               (append (visit (node-1 v) (1+ n))
                                       (visit (node-2 v) (1+ n)))]
                              [else
                               (errorf 'lengths-of-all-distinct-paths
                                       "not a binary tree: ~s"
                                       v)])))])
      (visit v_init 0))))


;;;;;;;;;;;;;;;;;;
;;; Exercise 9 ;;;
;;;;;;;;;;;;;;;;;;


;;; If you try the naive method of translating the given procedure to one that 
;;; uses an accumulator, you get the following:


(define lengths-of-all-distinct-paths_acc_naive
  (lambda (v_init)
    (letrec ([visit (lambda (v n a)
                      (cond
                        [(is-leaf? v)
                         a]
                        [(is-node? v)
                         (visit (node-2 v)
                                (1+ n)
                                (cons (1+ n)
                                      (visit
                                       (node-1 v)
                                       (1+ n)
                                       (cons (1+ n) a))))]
                        [else
                         (errorf 'lengths-of-all-distinct-paths_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (reverse (visit v_init 0 '(0))))))


(unless (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_acc_naive)
  (printf "fail: (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_acc_naive)~n"))

;;; This is of course not the intended solution, since you have to reverse the result of the initial visit
;;; call to pass the tests.
;;; In order to be able to use cons properly, we have to reverse the order of execution. We build the
;;; accumulator one element at the time. To ensure the correct order of the result, we have to traverse the
;;; tree from right to left.


(define lengths-of-all-distinct-paths_acc
  (lambda (v_init)
    (letrec ([visit (lambda (v n a)
                      (cons n (cond
                                [(is-leaf? v)
                                 a]
                                [(is-node? v)
                                 (visit (node-1 v)
                                        (1+ n)
                                        (visit
                                         (node-2 v)
                                         (1+ n)
                                         a))]
                                [else
                                 (errorf 'lengths-of-all-distinct-paths_acc
                                         "not a binary tree: ~s"
                                         v)])))])
      (visit v_init 0 '()))))



(unless (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_acc)
  (printf "fail: (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_acc)~n"))

(define fold-right_binary-tree
  (lambda (lea nod err)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(number? v)
                           (lea v)]
                          [(pair? v)
                           (nod (visit (car v))
                                (visit (cdr v)))]
                          [else
                           (err v)]))])
        (visit v_init)))))

;;; By popular demand, now also a version with fold-right.

(define lengths-of-all-distinct-paths_fold
  (lambda (v_init)
    (((fold-right_binary-tree
       (lambda (v)
         (lambda (n a)
           (cons n a)))
       (lambda (v1 v2)
         (lambda (n a)
           (cons n (v1 (1+ n) (v2 (1+ n) a)))))
       (lambda (v)
         (errorf 'lengths-of-all-distinct-paths_fold
                 "not a binary tree: ~s"
                 v))) v_init) 0 '())))

(unless (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_fold)
  (printf "fail: (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_fold)~n"))

;;;;;;;;;;;;;;;;;;;
;;; Exercise 10 ;;;
;;;;;;;;;;;;;;;;;;;

;;; We extended the fold procedure found in the dProgSprog lecture notes of 
;;; week 5 in order to make raising errors easier

(define fold-right_proper-list
  (lambda (nil-case cons-case else-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (cond
                          [(null? ws)
                            nil-case]
                          [(pair? ws)
                           (cons-case (car ws)
                                      (visit (cdr ws)))]
                          [else
                           (else-case ws)]))])
        (visit vs)))))


(define test-Dutch-flag
  (lambda (candidate)
    (and (equal? (candidate '() 10)
                 '(() 0 ()))
         (equal? (candidate '(1 2 3) 10)
                 '((1 2 3) 0 ()))
         (equal? (candidate '(10 10 10) 10)
                 '(() 3 ()))
         (equal? (candidate '(100 200 300) 10)
                 '(() 0 (100 200 300)))
         (equal? (candidate '(1 2 3 10 100 200 300) 10)
                 '((1 2 3) 1 (100 200 300)))
         (equal? (candidate '(10 1 300 3 10 2 100 10 200 1 10) 10)
                 '((1 3 2 1) 4 (300 100 200)))
         (equal? (candidate '(10 10 300 30 10 20 100 10 200 10 10) 10)
                 '(() 6 (300 30 20 100 200)))
         (equal? (candidate '(10 10 3 3 10 2 1 10 2 10 10) 10)
                 '((3 3 2 1 2) 6 ()))
         ;;;
         )))

;;; The only test cases we considered missing were the two where there exist
;;; pivot elements and only larger or smaller elements respectively.

;;; The Dutch-flag procedure should recurse over the input list. If this list
;;; is null, the procedure is done and the result returned.
;;; If the list is not null, the first element of the list has to be compared to
;;; the given pivot and the recursion continues with altered intermediate 
;;; results based on the result of this comparison. 
;;; In order to be able to use cons along the building of the intermediate 
;;; results, the input list is reversed to start with. Alternatively, one 
;;; could reverse the two result list before returning.

(define Dutch-flag_FIRST-draft
  (lambda (xs_init p)
    (letrec ([visit (lambda (xs a1 n a2)
                      (cond 
                        [(null? xs)
                         (list a1 n a2)]
                        [(pair? xs) 
                         (let ([x (car xs)])
                           (if (integer? x)
                               (cond 
                                 [(< x p)
                                  (visit (cdr xs) 
                                         (cons x a1)
                                         n
                                         a2)]
                                 [(> x p)
                                  (visit (cdr xs)
                                         a1
                                         n
                                         (cons x a2))]
                                 [else
                                  (visit (cdr xs)
                                         a1
                                         (1+ n)
                                         a2)])
                               (errorf 'Dutch-flag_FIRST-draft
                                       "Not a number: ~s"
                                       x)))]
                        [else
                         (errorf 'Dutch-flag_FIRST-draft
                                 "Not a proper list: ~s"
                                 xs_init)]))])
      (visit (reverse xs_init) '() 0 '()))))

(unless (test-Dutch-flag Dutch-flag_FIRST-draft)
  (printf "fail: (test-Dutch-flag Dutch-flag_FIRST-draft)~n"))

;;; Traversing the input twice due to the reverse is of course not the prettiest
;;; solution
;;; Alternatively, one could do the recursive call and build up on the result
;;; "on the way back up". In this procedure, the invariant is that the result
;;; of every visit call is the correct result to the Dutch-flag problem with
;;; the (sub-)list given as argument to the visit call. 


(define Dutch-flag_alt
  (lambda (xs_init p)
    (letrec ([visit (lambda (xs)
                      (cond 
                        [(null? xs)
                         (cons '() (cons 0 (cons '() '())))]
                        [(pair? xs) 
                         (let ([x (car xs)])
                           (if (integer? x)
                               (let ([r (visit (cdr xs))])
                                 (cond 
                                   [(< x p)
                                    (cons (cons x (car r)) (cdr r))]
                                   [(> x p)
                                    (cons (car r) 
                                          (cons (cadr r)
                                                (cons (cons x 
                                                            (caddr r)) 
                                                      '())))]
                                   [else
                                    (cons (car r) 
                                          (cons (1+ (cadr r))
                                                (cons (caddr r) 
                                                      '())))]))
                               (errorf 'Dutch-flag_alt
                                       "Not a number: ~s"
                                       x)))]
                        [else
                         (errorf 'Dutch-flag_alt
                                 "Not a proper list: ~s"
                                 xs_init)]))])
      (visit xs_init))))


(unless (test-Dutch-flag Dutch-flag_alt)
  (printf "fail: (test-Dutch-flag Dutch-flag_alt)~n"))

;;; From this version, it is fairly straight-forward to write one using
;;; fold-right_proper-list:


(define Dutch-flag_fold
  (lambda (xs_init p)
    ((fold-right_proper-list
     (list '() 0 '())
     (lambda (x r)
       (if (integer? x)
           (cond 
             [(< x p)
              (cons (cons x (car r)) (cdr r))]
             [(> x p)
              (list (car r) 
                    (cadr r)
                    (cons x (caddr r)))]
             [else
              (list (car r) 
                    (1+ (cadr r))
                    (caddr r))])
           (errorf 'Dutch-flag_fold
                   "Not a number: ~s"
                   x)))
     (lambda (x)
       (errorf 'Dutch-flag_fold
               "Not a proper list: ~s"
               x))) xs_init)))

(unless (test-Dutch-flag Dutch-flag_fold)
  (printf "fail: (test-Dutch-flag Dutch-flag_fold)~n"))


                         

;;;;;;;;;;;;;;;;;;;
;;; Exercise 11 ;;;
;;;;;;;;;;;;;;;;;;;


(define test-run-length
  (lambda (candidate)
    (and (equal? (candidate '())
                 '())
         (equal? (candidate '(a))
                 '((a . 1)))
         (equal? (candidate '(a b b))
                 '((a . 1) (b . 2)))
         (equal? (candidate '(a b b c c c))
                 '((a . 1) (b . 2) (c . 3)))
         (equal? (candidate '(a b b c c c a a a a a a))
                 '((a . 1) (b . 2) (c . 3) (a . 6)))
         (equal? (candidate '(a a a a a a b b c c c a))
                 '((a . 6) (b . 2) (c . 3) (a . 1)))
         (equal? (candidate '(a b b c c c c c c c c c b b a))
                 '((a . 1) (b . 2) (c . 9) (b . 2) (a . 1)))
         (equal? (candidate '(c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c))
                 '((c . 42)))
         ;;; etc.
         )))

;;; We added a test where the counters do not increase throughout the result 
;;; and one with a lot of a single element.

;;; It seems like the recursion needed for this exercise is the same kind as 
;;; the last one, e.g. it is easiest to compute the result bottom-up.
;;; Since we know that the result of the visit call is a well-formed result
;;; of run-length on the remaining list (e.g. either null or a list of pairs), 
;;; we can, after having checked whether it is null, access it with cdar and 
;;; cdr.

(define run-length
  (lambda (xs_init)
    (letrec ([visit (lambda (xs)
                      (cond
                        [(null? xs)
                         '()]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (let ([r (visit (cdr xs))])
                                 (cond
                                   [(null? r)
                                    (cons (cons x 1) '())]
                                   [else
                                    (if (equal? x (caar r))
                                        (cons (cons x (1+ (cdar r))) (cdr r))
                                        (cons (cons x 1) r))]))
                               (errorf 'run-length
                                       "Not a symbol: ~s"
                                       x)))]
                        [else
                         (errorf 'run-length
                                 "Not a proper list: ~s"
                                 xs_init)]))])
             (visit xs_init))))

(unless (test-run-length run-length)
  (printf "(fail: (test-run-length run-length) ~n"))

(define run-length_acc1
  (lambda (xs_init)
    (letrec ([visit (lambda (xs a)
                      (cond
                        [(null? xs)
                         a]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (cond 
                                 [(null? a)
                                  (visit (cdr xs) 
                                         (cons (cons x 1) a))]
                                 [(pair? a)
                                  (if (equal? x (caar a))
                                      (visit (cdr xs) 
                                             (cons (cons x 
                                                         (1+ (cdar a))) 
                                                   (cdr a)))
                                      (visit (cdr xs)
                                             (cons (cons x 1) a)))])
                                  (errorf 'run-length
                                          "Not a symbol: ~s"
                                          x)))]
                        [else
                         (errorf 'run-length
                                 "Not a proper list: ~s"
                                 xs_init)]))])
             (reverse (visit xs_init '())))))



(unless (test-run-length run-length_acc1)
  (printf "(fail: (test-run-length run-length_acc1) ~n"))

;;; We know that the above version is NOT what is expected (due to the reverse),
;;; but we were asked to account for our thoughts, so there you go.


(define run-length_acc2
  (lambda (xs_init)
    (letrec ([visit (lambda (xs as a)
                      (cond
                        [(null? xs)
                         (append as a)]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (cond 
                                 [(null? a)
                                  (visit (cdr xs) 
                                         as
                                         (cons (cons x 1) a))]
                                 [(pair? a)
                                  (if (equal? x (caar a))
                                      (visit (cdr xs) 
                                             as
                                             (cons (cons x 
                                                   (1+ (cdar a))) '()))
                                      (visit (cdr xs)
                                             (append as a)
                                             (cons (cons x 1) '())))])
                               (errorf 'run-length
                                       "Not a symbol: ~s"
                                       x)))]
                        [else
                         (errorf 'run-length
                                 "Not a proper list: ~s"
                                 xs_init)]))])
             (visit xs_init '() '()))))


(unless (test-run-length run-length_acc2)
  (printf "(fail: (test-run-length run-length_acc2) ~n"))


;;; This version uses an accumulator and an extra variable for the last letter
;;; in the input list. It is probably still not the intended solution, since
;;; it builds the result by using append. 

(define run-length_acc3
  (lambda (xs_init)
    (letrec ([visit (lambda (xs a sym n)
                      (cond
                        [(null? xs)
                         (if (not sym)
                             '()
                             (append a (list (cons sym n))))]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (cond 
                                 [(not (symbol? sym))
                                  (visit (cdr xs) 
                                         a
                                         x
                                         1)]
                                 [(symbol? sym)
                                  (if (equal? x sym)
                                      (visit (cdr xs) 
                                             a
                                             x
                                             (1+ n))
                                      (visit (cdr xs)
                                             (append a (list (cons sym n)))
                                             x
                                             1))])
                               (errorf 'run-length
                                       "Not a symbol: ~s"
                                       x)))]
                        [else
                         (errorf 'run-length
                                 "Not a proper list: ~s"
                                 xs_init)]))])
             (visit xs_init '() #f 0))))

(unless (test-run-length run-length_acc3)
  (printf "(fail: (test-run-length run-length_acc3) ~n"))


(define run-length_acc4
  (lambda (xs_init)
    (letrec ([visit (lambda (xs a)
                      (cond
                        [(null? xs)
                         (if (null? a)
                             '()
                             (list a))]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (if (pair? a)
                                  (if (equal? x (car a))
                                      (visit (cdr xs) 
                                             (cons x 
                                                   (1+ (cdr a))))
                                      (cons a (visit (cdr xs)
                                                     (cons x 1))))
                                  (visit (cdr xs) 
                                             (cons x 1)))
                               (errorf 'run-length
                                       "Not a symbol: ~s"
                                       x)))]
                        [else
                         (errorf 'run-length
                                 "Not a proper list: ~s"
                                 xs_init)]))])
             (visit xs_init '()))))


(unless (test-run-length run-length_acc4)
  (printf "(fail: (test-run-length run-length_acc4) ~n"))

;;; This version only has n non-tail-recursive visit calls, as required. We are
;;; not sure about the allocated cons pairs, since we are not sure we understand
;;; your comment correctly.

(define run-length_acc-final
  (lambda (xs_init)
    (letrec ([visit (lambda (xs sym n)
                      (cond
                        [(null? xs)
                         (if (not sym)
                             '()
                             (list (cons sym n)))]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (if sym
                                   (if (equal? x sym)
                                       (visit (cdr xs) 
                                              sym 
                                              (1+ n))
                                       (cons (cons sym n)
                                             (visit (cdr xs) x 1)))
                                   (visit (cdr xs) x 1))
                               (errorf 'run-length
                                       "Not a symbol: ~s"
                                       x)))]
                        [else
                         (errorf 'run-length
                                 "Not a proper list: ~s"
                                 xs_init)]))])
             (visit xs_init #f 0))))

(unless (test-run-length run-length_acc-final)
  (printf "(fail: (test-run-length run-length_acc-final) ~n"))


;;; This one is a combination of version 3 and 4 and our best candidate for the
;;; issued challenge. All visit calls, except for the ones where we have to add
;;; a new pair so the result, are tail calls. These and the base case are also 
;;; the only times we call cons, which will result in at most 2n (as we see it 
;;; 2n-1) calls, as required. It does not really use an accumulator as such 
;;; though.

;;; This version is pretty much the same as the last one.

(define run-length_acc-final1
  (lambda (xs_init)
    (letrec ([visit (lambda (x n xs)
                      (cond
                        [(null? xs)
                         (list (cons x n))]
                        [(pair? xs)
                         (let ([new-x (car xs)])
                           (if (symbol? new-x)
                               (if (equal? x new-x)
                                   (visit x (1+ n) (cdr xs))
                                   (cons (cons x n) (visit new-x 1 (cdr xs))))
                               (errorf 'run-length_acc-final1
                                       "Not a symbol: ~s"
                                       x)))]
                        [else
                         (errorf 'run-length_acc-final1
                                 "Not a proper list: ~s in ~s"
                                 xs
                                 xs_init)]))])
      (cond
        [(null? xs_init)
         '()]
        [(pair? xs_init)
         (let ([x (car xs_init)])
           (if (symbol? x)
               (visit x 1 (cdr xs_init))
               (errorf 'run-length_acc-final1
                       "Not a symbol: ~s"
                       x)))]
        [else
         (errorf 'run-length_acc-final1
                 "Not a proper input list: ~s"
                 xs_init)]))))


(define run-length_acc-final2
  (lambda (xs_init)
    (letrec ([visit-outer 
              (lambda (x xs)
                (letrec 
                    ([visit-inner 
                      (lambda (n xs)
                        (cond
                          [(null? xs)
                           (list (cons x n))]
                          [(pair? xs)
                           (let ([new-x (car xs)])
                             (if (symbol? new-x)
                                 (if (equal? x new-x)
                                     (visit-inner (1+ n) (cdr xs))
                                     (cons (cons x n) 
                                           (visit-outer new-x (cdr xs))))
                                 (errorf 'run-length_acc-final2
                                         "Not a symbol: ~s"
                                         x)))]
                          [else
                           (errorf 'run-length_acc-final2
                                   "Not a proper list: ~s in ~s"
                                   xs
                                   xs_init)]))])
                  (visit-inner 1 xs)))])
      (cond
        [(null? xs_init)
         '()]
        [(pair? xs_init)
         (let ([x (car xs_init)])
           (if (symbol? x)
               (visit-outer x (cdr xs_init))
                (errorf 'run-length_acc-final2
                       "Not a symbol: ~s"
                       x)))]
        [else
         (errorf 'run-length_acc-final2
                 "Not a proper input list: ~s"
                 xs_init)]))))

;;; As you strongly suggested in your feedback, a trace on the outer and inner
;;; visits illustrates clearly that a new layer of procedure calls is started
;;; every time the character changes and therefore "outer" is invoked. 
;;; This results in n non-tail-recursive calls as required. 


;; > (run-length_acc-final2 '(a a a b b b a c c c))
;; |(outer a (a a b b b a c c c))
;; |(inner 1 (a a b b b a c c c))
;; |(inner 2 (a b b b a c c c))
;; |(inner 3 (b b b a c c c))
;; | (outer b (b b a c c c))
;; | (inner 1 (b b a c c c))
;; | (inner 2 (b a c c c))
;; | (inner 3 (a c c c))
;; | |(outer a (c c c))
;; | |(inner 1 (c c c))
;; | | (outer c (c c))
;; | | (inner 1 (c c))
;; | | (inner 2 (c))
;; | | (inner 3 ())
;; | | ((c . 3))
;; | |((a . 1) (c . 3))
;; | ((b . 3) (a . 1) (c . 3))
;; |((a . 3) (b . 3) (a . 1) (c . 3))
;; ((a . 3) (b . 3) (a . 1) (c . 3))

;;; We don not see how this procedure should be lambda-dropped more than it 
;;; already is. This question seems to make more sense with regard to Exercise
;;; 10, where one could omit p in all but the outermost lambda.

;;; Again, it is very straight-forward to write a fold-right version from this
;;; one, since the entire structure is the same, fold-right just abstracts the
;;; let bindings for us.

(define run-length_fold
  (lambda (xs_init)
    ((fold-right_proper-list
      '()
      (lambda (x r)
        (if (symbol? x)
            (cond
              [(null? r)
               (list (cons x 1))]
              [else
               (if (equal? x (caar r))
                   (cons (cons x (1+ (cdar r))) (cdr r))
                   (cons (cons x 1) r))])
            (errorf 'run-length_fold
                    "Not a symbol: ~s"
                    x)))
      (lambda (x)
        (errorf
         'run-length_fold
         "Not a proer list: ~s"
         x))) xs_init)))



(unless (test-run-length run-length_fold)
  (printf "(fail: (test-run-length run-length_fold) ~n"))
;;;;;;;;;;


"ex9-11.scm"

;;; end of "ex9-11.scm"
