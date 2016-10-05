

(define test-list_Magritte
  (lambda (candidate)
    (and
     (equal? (candidate '())
             '()))
     (equal? (candidate '(0 1 1))
             '(cons 0 (cons 1 (cons 1 '()))))
     (equal? (candidate '(0 1 2 2 1))
             '(cons 0 (cons 1 (cons 2 (cons 2 (cons 1 '()))))))
     (equal? (candidate '(0 1 2 3 3 2 1 2 2))
             '(cons 0 (cons 1 (cons 2 (cons 3 (cons 3 (cons 2 (cons 1 (cons 2 (cons 2 '()))))))))))
     (equal? (candidate '(0 1 2 3 4))
             '(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 '()))))))
     (equal? (candidate '(a b c))
             '(cons 'a (cons 'b (cons 'c '()))))))

(define list_Magritte
  (lambda (xs)
    (letrec ([visit
              (lambda (x)
                (cond
                  [(null? x)
                   ''()]
                  [(pair? x)
                   (cond
                     [(symbol? (car x))
                      `(cons ',(car x) ,(visit (cdr x)))]
                     [else
                      `(cons ,(car x) ,(visit (cdr x)))])]
                  [else
                   (errorf 'list_Magritte
                           "Not a proper list: ~s"
                           'x)]))])
      (visit xs))))

(define test-Magrittify-environment
  (lambda (candidate)
    (and (equal? (candidate '())
                 'empty-env)
         (equal? (candidate '((x . 1)))
                 '(extend-env 'x 1 empty-env))
         (equal? (candidate '((y . 2) (x . 1)))
                 '(extend-env 'y 2 (extend-env 'x 1 empty-env)))
         )))

(define Maggritify-environment
  (lambda (x)
    (letrec ([visit
              (lambda (x)
                (cond
                  [(null? x)
                   'empty-env]
                  [(pair? x)
                   (if (pair? (car x))
                       (let ([cx (car x)])
                         `(extend-env ',(car cx) ,(cdr cx) ,(visit (cdr x))))
                       (errorf 'Maggritify-environment
                               "Not a proper environment: ~s"
                               x))]
                  [else
                   (errorf 'Maggritify-environment
                           "Not a proper list: ~s"
                           x)]))])
      (visit x))))









