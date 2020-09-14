#lang racket/base

(provide reactor reactor? reactor-state
         react reactor-stopped? render-reactor run-reactor
         (rename-out [the-time-step time-step])
         key-event mouse-event
         (all-from-out 2htdp/universe))

(require (only-in racket/match match)
         2htdp/universe)
(require syntax/parse/define
         (for-syntax racket/base
                     (only-in racket/sequence in-syntax)
                     syntax/parse
                     syntax/parse/define
                     (for-syntax racket/base
                                 syntax/parse
                                 syntax/parse/define)))
         

(define (default-on-tick state) state)
(define default-tick-rate 1/28)
(define (default-on-key state key-event) state)
(define (default-stop-when state) #false)
(define default-name "World")

(struct vtable
  (to-draw
   window
   on-tick
   tick-rate
   on-key
   ; release
   ; pad
   ; mouse
   stop-when
   last-picture
   name))

(struct reactor-struct
  (state vtable)
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (display "#<reactor " port)
     (write/mode (reactor-state r) port mode)
     (fprintf port " ~a>" ((vtable-to-draw (reactor-vtable r))
                           (reactor-state r))))])

(define (write/mode v port mode)
  (cond
    [(number? mode)  (print v port mode)]
    [mode            (write v port)]
    [else            (display v port)]))

(define (reactor? r)
  (reactor-struct? r))

(define (reactor-state r)
  (reactor-struct-state r))

(define reactor-vtable
  reactor-struct-vtable)

(struct time-step ())
(struct key-event (key))
(struct mouse-event (x y kind))

(define the-time-step (time-step))

(begin-for-syntax
  (define-syntax-class fun
    (pattern :expr))
  (define (parse-clause whole main cxts clause)
    (define (save-fun key val)
      (define cxt (hash-ref cxts key #f))
      (when cxt
        (raise-syntax-error #f
                            (format "reactor has repeated ~a clause" key)
                            whole clause (list cxt)))
      (hash-set! main key val)
      (hash-set! cxts key clause))
    (define-simple-macro (save key:id val:expr (~seq xkey:id xval:expr) ...)
      (begin
        (save-fun 'key val)
        (hash-set! main 'xkey xval) ...))
    (syntax-parse clause
      [((~literal to-draw) draw:fun)
       (save to-draw      #'draw)]
      [((~literal to-draw) draw:fun width:expr height:expr)
       (save to-draw      #'draw
             window       #'(cons width height))]
      [((~literal on-tick) tick:fun)
       (save on-tick      #'tick)]
      [((~literal on-tick) tick:fun rate:expr)
       (save on-tick      #'tick
             tick-rate    #'rate)]
      [((~literal on-tick) tick:fun rate:expr limit:expr)
       (save on-tick      #'tick
             tick-rate    #'rate
             tick-limit   #'limit)]
      [((~literal on-key) key:fun)
       (save on-key       #'key)]
      [((~literal stop-when) stop:fun)
       (save stop-when    #'stop)]
      [((~literal stop-when) stop:fun last:fun)
       (save stop-when    #'stop
             last-picture #'last)]
      [((~literal name) s:expr)
       (save name         #'s)])))

(define-syntax (reactor stx)
  (syntax-parse stx
    [(_ state:expr clause:expr ...+)
     (define main (make-hasheq))
     (define cxts (make-hasheq))
     (define-simple-macro (get key:id def:expr)
       (hash-ref main 'key def))
     (for ([clause (in-syntax #'(clause ...))])
       (parse-clause stx main cxts clause))
     (define the-to-draw
       (get to-draw
            (λ ()
              (raise-syntax-error #f
                                  "reactor requires a to-draw clause"
                                  stx))))
     #`(reactor-struct
        state
        (vtable #,the-to-draw
                #,(get window        #'#f)
                #,(get on-tick       #'default-on-tick)
                #,(get tick-rate     #'default-tick-rate)
                #,(get on-key        #'default-on-key)
                #,(get stop-when     #'default-stop-when)
                #,(get last-picture  the-to-draw)
                #,(get name          #'default-name)))]))

(define (react event r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  (match event
    [(struct time-step ())
     (reactor-struct ((values (vtable-on-tick v)) q) v)]
    [(struct key-event (key))
     (reactor-struct ((vtable-on-key v) q key) v)]
    [(struct mouse-event (x y kind))
     (error 'todo!)]))

(define (reactor-stopped? r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  ((vtable-stop-when v) q))
  
(define (render-reactor r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  ((vtable-to-draw v) q))

(define (dispatch-big-bang q v)
  (cond
      [(vtable-window v)
       =>
       (λ (window)
         (big-bang q
           [on-tick   (vtable-on-tick v)]
           [to-draw   (vtable-to-draw v) (car window) (cdr window)]
           [on-key    (vtable-on-key v)]
           [name      (vtable-name v)]
           [stop-when (vtable-stop-when v)]))]
      [else
       (big-bang q
         [on-tick   (vtable-on-tick v)]
         [to-draw   (vtable-to-draw v)]
         [on-key    (vtable-on-key v)]
         [name      (vtable-name v)]
         [stop-when (vtable-stop-when v)])]))

(define (run-reactor r)
  (define q (reactor-state r))
  (define v (reactor-vtable r))
  (reactor-struct (dispatch-big-bang q v) v))
