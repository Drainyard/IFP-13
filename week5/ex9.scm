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


;;; If you try the naive method of translating the given procedure to one that uses an accumulator, you get
;;; the following:


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


;;;;;;;;;;

"ex9.scm"

;;; end of "ex9.scm"
