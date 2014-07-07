#lang racket/base
(require racket/match
         racket/contract/base
         racket/contract/combinator)

;; xxx mu, tests, choice

(struct st () #:transparent)
(struct st:0 st () #:transparent)
(struct st:? st (ce k) #:transparent)
(struct st:! st (ce k) #:transparent)

(define st-dual
  (match-lambda
   [(and s (st:0)) s]
   [(st:? ce k)
    (st:! ce (st-dual k))]
   [(st:! ce k)
    (st:? ce (st-dual k))]))

(define (channel/session st)
  (make-chaperone-contract
   #:name `(channel/session ,st)
   #:first-order channel?
   #:projection
   (λ (b)
     (λ (ch)
       (define current-st st)
       (define (get ch)
         (match current-st
           [(st:? ce k)
            (define ctc (coerce-contract 'ce ce))
            (define proj ((contract-projection ctc) b))
            ;; xxx lock the channel?
            (values ch (λ (x) (set! current-st k) (proj x)))]
           [_
            (let/cc k 
              (raise-blame-error b k "Illegal channel-get in context"))]))
       (define (put ch x)
         (match current-st
           [(st:! ce k)
            (define ctc (coerce-contract 'ce ce))
            (define proj ((contract-projection ctc) b))
            ;; xxx lock the channel?
            (set! current-st k) 
            (proj x)]
           [_
            (let/cc k 
              (raise-blame-error b k "Illegal channel-put in context"))]))
       (chaperone-channel ch get put)))))

(module+ test
  (require racket/contract/base
           racket/contract/region)

  (define st (st:? string? (st:! number? (st:? string? (st:0)))))
  (define ch (make-channel))
  (define/contract left-ch (channel/session st) ch)
  (define/contract right-ch (channel/session (st-dual st)) ch)

  (define left
    (thread
     (λ ()
       (define ch left-ch)
       (channel-put ch (string->number (channel-get ch)))
       (displayln (string-append "Got: " (channel-get ch))))))
  (define right
    (thread
     (λ ()
       (define ch right-ch)
       (channel-put ch "100")
       (channel-put ch (number->string (+ 10 (channel-get ch)))))))

  (thread-wait left)
  (thread-wait right))
