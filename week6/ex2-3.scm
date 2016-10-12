(define fold-right_proper-list
  (lambda (null pair err)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(null? v)
                           null]
                          [(pair? v)
                           (pair (car v)
                                 (visit (cdr v)))]
                          [else
                           err]))])
        (visit v_init)))))



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



(define Dutch-flag_mul-values
  (lambda (xs_init p)
    (letrec ([visit (lambda (xs)
                      (cond 
                        [(null? xs)
                         (values '() 0 '())]
                        [(pair? xs) 
                         (let ([x (car xs)])
                           (if (integer? x)
                               (let-values ([(l1 n l2) (visit (cdr xs))])
                                 (cond 
                                   [(< x p)
                                    (values (cons x l1) n l2)]
                                   [(> x p)
                                    (values l1 n (cons x l2))]
                                   [else
                                    (values l1 (1+ n) l2)]))
                               (errorf 'Dutch-flag_alt
                                       "Not a number: ~s"
                                       x)))]
                        [else
                         (errorf 'Dutch-flag_alt
                                 "Not a proper list: ~s"
                                 xs_init)]))])
      (let-values ([(r1 r2 r3) (visit xs_init)])
        (list r1 r2 r3)))))

(unless (test-Dutch-flag Dutch-flag_mul-values)
  (printf "fail: (test-Dutch-flag Dutch-flag_mul-values) ~n"))

(define Dutch-flag_mul-values_fold
  (lambda (xs_init p)
    (let-values ([(r1 r2 r3) 
                  (((fold-right_proper-list
                    (lambda () (values '() 0 '()))
                    (lambda (v vs)
                      (if (integer? v)
                          (let-values ([(l1 n l2) (vs)])
                                 (cond 
                                   [(< v p)
                                    (lambda () (values (cons v l1) n l2))]
                                   [(> v p)
                                    (lambda () (values l1 n (cons v l2)))]
                                   [else
                                    (lambda () (values l1 (1+ n) l2))]))
                          (errorf 'Dutch-flag_mul-values_fold
                                  "Not a number: ~s"
                                  v)))
                    (lambda ()
                      (errorf 'Dutch-flag_mul-values_fold
                              "Not a proper list: ~s"
                              xs_init))) xs_init))])
      (list r1 r2 r3))))
                    
(unless (test-Dutch-flag Dutch-flag_mul-values_fold)
  (printf "fail: (test-Dutch-flag Dutch-flag_mul-values_fold) ~n"))


;;; run-length
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


(define run-length_mul-values
  (lambda (xs_init)
    (letrec ([visit (lambda (xs)
                      (cond
                        [(null? xs)
                         (values '() '())]
                        [(pair? xs)
                         (let ([x (car xs)])
                           (if (symbol? x)
                               (let-values ([(p1 r) (visit (cdr xs))])
                                 (cond 
                                   [(pair? p1)
                                    (if (equal? x (car p1))
                                        (values (cons x (1+ (cdr p1))) r)
                                        (values (cons x 1) (cons p1 r)))]
                                   [else
                                    (values (cons x 1) r)]))
                               (errorf 'run-length_mult-values
                                       "Not a symbol: ~s"
                                       x)))]
                        [else (errorf 'run-length_mult-values
                                      "Not a proper list: ~s"
                                      xs_init)]))])
      (if (null? xs_init)
          '()
          (let-values ([(r1 r2) (visit xs_init)])
            (cons r1 r2))))))


;;; In contrast to Dutch-flag with multiple values, this does not look prettier 
;;; to us. We still need to access the intermediate results with car and cdr
;;; because, as far as we could see, you cannot pattern-match deeper with one
;;; let-values binding. Moreover, we have to check for the empty list at first,
;;; since the result in this case has a different structure than the other ones.


(unless (test-run-length run-length_mul-values)
  (printf "fail: (test-run-length run-length_mul-values) ~n"))

"ex2-3.scm"
