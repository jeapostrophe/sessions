#lang racket/base
(require racket/list
         racket/match)

(struct e () #:transparent)
(struct e:add e (l r) #:transparent)
(struct e:id e (x) #:transparent)
(struct e:val e (v) #:transparent)

(define (interp-e env e)
  (match e
    [(e:id x)
     (hash-ref env x)]
    [(e:add l r)
     (+ (interp-e env l)
        (interp-e env r))]
    [(e:val v)
     v]))

(struct t () #:transparent)
(struct t:ch t () #:transparent)
(struct t:int t () #:transparent)

(define (type-e gamma e)
  (match e
    [(e:id x)
     (hash-ref gamma x)]
    [(e:add l r)
     (and (t:int? (type-e gamma l))
          (t:int? (type-e gamma r))
          (t:int))]
    [(e:val v)
     (cond
       [(number? v) (t:int)]
       [else #f])]))

(struct pi () #:transparent)
(struct pi:zero pi () #:transparent)
(struct pi:para pi (l r) #:transparent)
(struct pi:newc pi (c k) #:transparent)
(struct pi:send pi (c e k) #:transparent)
(struct pi:recv pi (c x k) #:transparent)
;; extra
(struct pi:ret pi (e) #:transparent)

(struct proc (env p) #:transparent)

(define (snoc l x) (append l (list x)))
(define (step-procs ps)
  (match ps
    [(cons (proc _ (pi:zero)) more)
     (values empty more)]
    [(cons (proc env (pi:para l r)) more)
     (values empty (list* (proc env l) (proc env r) more))]
    [(cons (proc env (pi:newc c k)) more)
     (values empty (cons (proc (hash-set env c (gensym 'c)) k) more))]
    [(cons (proc env (pi:ret e)) more)
     (values (list (interp-e env e)) more)]
    [(cons (proc s-env (pi:send s-c s-e s-k))
           (list before ...
                 (proc r-env (pi:recv r-c r-x r-k))
                 after ...))
     #:when (eq? (interp-e s-env s-c)
                 (interp-e r-env r-c))
     (values empty
             (list* (proc s-env
                          s-k)
                    (proc (hash-set r-env r-x (interp-e s-env s-e))
                          r-k)
                    (append before after)))]
    [(cons (and p (proc env (pi:recv c x k))) more)
     (values empty (snoc more p))]
    [(list)
     (values empty empty)]))

(define (interp-procs rets ps)
  (define-values (mrets nps) (step-procs ps))
  (define nrets (append mrets rets))
  (if (eq? nps ps)
    (values nrets nps)
    (interp-procs nrets nps)))

(define (interp-pi p)
  (interp-procs empty (list (proc (hasheq) p))))

(module+ test
  (require rackunit)
  (define-syntax-rule (check p r ...)
    (let-values ([(rets ps) (interp-pi p)])
      (check-equal? ps empty)
      (check-equal? rets (list r ...))))
  (check (pi:zero))
  (check (pi:para (pi:zero) (pi:zero)))
  (check (pi:newc 'x (pi:para (pi:zero) (pi:zero))))
  (check (pi:ret (e:val 10)) 10)
  (check (pi:newc 'x
                  (pi:para (pi:send (e:id 'x) (e:val 10) (pi:zero))
                           (pi:recv (e:id 'x) 'y (pi:ret (e:id 'y)))))
         10)
  (check (pi:newc 'x
                  (pi:para (pi:recv (e:id 'x) 'y (pi:ret (e:id 'y)))
                           (pi:send (e:id 'x) (e:val 10) (pi:zero))))
         10)
  (check (pi:newc 'x
                  (pi:para (pi:recv (e:id 'x) 'y 
                                    (pi:recv (e:id 'y) 'z (pi:ret (e:id 'z))))
                           (pi:newc 'y
                                    (pi:para
                                     (pi:send (e:id 'x) (e:id 'y) (pi:zero))
                                     (pi:send (e:id 'y) (e:val 10) (pi:zero))))))
         10))
