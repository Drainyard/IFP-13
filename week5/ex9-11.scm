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
                      (cond
                        [(is-leaf? v)
                         (cons n a)]
                        [(is-node? v)
                         (cons n (visit (node-1 v)
                                        (1+ n)
                                        (visit
                                         (node-2 v)
                                         (1+ n)
                                         a)))]
                        [else
                         (errorf 'lengths-of-all-distinct-paths_acc
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init 0 '()))))


(unless (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_acc)
  (printf "fail: (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_acc)~n"))


;;;;;;;;;;;;;;;;;;;
;;; Exercise 10 ;;;
;;;;;;;;;;;;;;;;;;;

;;; We extended the fold procedure found in the dProgSprog lecture notes of 
;;; week 5 in order to make raising errors easier

(define fold-right_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (cond
                          [(null? ws)
                            nil-case]
                          [(pair? ws)
                           (cons-case (car ws)
                                      (visit (cdr ws)))]
                          [else
                           (errorf 'fold-right_proper-list
                                   "Not a proper list: ~s"
                                   ws)]))])
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

(define Dutch-flag
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
                               (errorf 'Dutch-flag
                                       "Not a number: ~s"
                                       x)))]
                        [else
                         (errorf 'Dutch-flag
                                 "Not a proper list: ~s"
                                 xs_init)]))])
      (visit (reverse xs_init) '() 0 '()))))

(unless (test-Dutch-flag Dutch-flag)
  (printf "fail: (test-Dutch-flag Dutch-flag)~n"))

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
                         (list '() 0 '())]
                        [(pair? xs) 
                         (let ([x (car xs)])
                           (if (integer? x)
                               (let ([r (visit (cdr xs))])
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
                                          (caddr r))]))
                               (errorf 'Dutch-flag
                                       "Not a number: ~s"
                                       x)))]
                        [else
                         (errorf 'Dutch-flag
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
           (errorf 'Dutch-flag
                   "Not a number: ~s"
                   x)))) xs_init)))

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
                                    (list (cons x 1))]
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

;;; We don't see how this procedure should be lambda-dropped more than it 
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
                    x)
            ))) xs_init)))



(unless (test-run-length run-length_fold)
  (printf "(fail: (test-run-length run-length_fold) ~n"))
;;;;;;;;;;


"ex9-11.scm"

;;; end of "ex9-11.scm"
