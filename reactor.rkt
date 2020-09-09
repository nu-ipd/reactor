#lang racket/base

(provide (rename-out [make-reactor reactor])
         reactor? reactor-state
         react reactor-stopped? render-reactor run-reactor
         (rename-out [the-time-step time-step])
         key-event mouse-event
         (all-from-out 2htdp/universe))

(require (only-in racket/match match)
         2htdp/universe)
(require (for-syntax racket/base
                     (only-in racket/sequence in-syntax)
                     syntax/parse))

(struct vtable
  (draw
   tick
   key
   ; release
   ; pad
   ; mouse
   stop
   ; last-picture
   name))

(struct reactor
  (state
   vtable))

(struct time-step ())
(struct key-event (key))
(struct mouse-event (x y kind))

(define the-time-step (time-step))

(begin-for-syntax
  (define-syntax-class reactor-clause
    #:attributes (key val)
    #:description "a reactor clause, such as (to-draw ...) or (on-tick ...)"
    (pattern [(~and key (~literal to-draw)) draw:expr]
             #:attr val #'(list draw))
    (pattern [(~and key (~literal to-draw)) draw:expr width:expr height:expr]
             #:attr val #'(list draw width height))
    (pattern [(~and key (~literal on-tick)) tick:expr]
             #:attr val #'(list tick))
    (pattern [(~and key (~literal on-tick)) tick:expr rate:expr]
             #:attr val #'(list tick rate))
    (pattern [(~and key (~literal on-tick)) tick:expr rate:expr limit:expr]
             #:attr val #'(list tick rate limit))
    (pattern [(~and key (~literal on-key)) val:expr])
    (pattern [(~and key (~literal stop-when)) val:expr])
    (pattern [(~and key (~literal name)) val:expr]))

  (define-splicing-syntax-class distinct-reactor-clauses
    #:attributes (assocs)
    #:description
    "some distinct reactor clauses, such as (to-draw ...) and (on-tick ...)"
    (pattern (~seq clause:reactor-clause ...+)
             #:fail-when
             (for/and ([key (in-syntax #'(clause.key ...))])
               (not (eq? 'to-draw (syntax-e key))))
             "missing to-draw clause"
             #:fail-when
             (check-duplicate-identifier
              (syntax->list #'(clause.key ...)))
             "duplicate clause"
             #:attr assocs (make-hasheq
                            (for/list ([key (in-syntax #'(clause.key ...))]
                                       [val (in-syntax #'(clause.val ...))])
                              (cons (syntax-e key) val))))))

(define (default-on-tick state) state)
(define (default-on-key state key-event) state)
(define (default-stop-when state) #false)

(define-syntax (make-reactor stx)
  (syntax-parse stx
    [(_ state:expr clauses:distinct-reactor-clauses)
     (define assocs (attribute clauses.assocs))
     #`(reactor state
                (vtable
                 #,(hash-ref assocs 'to-draw)
                 #,(hash-ref assocs 'on-tick   #'(list default-on-tick))
                 #,(hash-ref assocs 'on-key    #'default-on-key)
                 #,(hash-ref assocs 'stop-when #'default-stop-when)
                 #,(hash-ref assocs 'name      #'"World")))]))


(define (react event r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  (match event
    [(struct time-step ())
     (reactor ((car (vtable-tick v)) q) v)]
    [(struct key-event (key))
     (reactor ((vtable-key v) q key) v)]
    [(struct mouse-event (x y kind))
     (error 'todo!)]))

(define (reactor-stopped? r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  ((vtable-stop v) q))
  
(define (render-reactor r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  ((car (vtable-draw v)) q))

(define (run-reactor r)
  (define q0 (reactor-state r))
  (define v (reactor-vtable r))
  (define q1
   (big-bang q0
     [on-tick   (car (vtable-tick v))]
     [to-draw   (car (vtable-draw v))]
     [on-key    (vtable-key v)]
     [name      (vtable-name v)]
     [stop-when (vtable-stop v)]))
  (reactor q1 v))