;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname example) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require "reactor.rkt")

; A World is a Nat

(define AMPLITUDE 300)
(define FREQUENCY 0.1)

(define EMPTY-SCENE (empty-scene (* 2 AMPLITUDE) (* 2 AMPLITUDE)))

; : World -> Scene
(define (render-scene n)
  (if (number? n)
      (place-image (render-circle (sin (* FREQUENCY n)))
                   AMPLITUDE AMPLITUDE EMPTY-SCENE)
      EMPTY-SCENE))

; : Unit-Real -> Scene
(define (render-circle phase)
  (circle (* AMPLITUDE (abs phase))
          "solid"
          (if (positive? phase) "green" "red")))

; : World Key-event -> World
(define (q-quits n k)
  (cond
    [(key=? k "q") #false]
    [else n]))

; : Reactor
(define r
  (reactor 0
    [on-tick   add1]
    [to-draw   render-scene]
    [on-key    q-quits]
    [stop-when false?]))


; : Nat -> [List-of Event]
(define (time-steps n)
  (if (zero? n)
      '()
      (cons time-step (time-steps (sub1 n)))))

(check-expect (reactor-state r)
              0)
(check-expect (reactor-state (react time-step r))
              1)
(check-expect (reactor-state (react (key-event "x") r))
              0)
(check-expect (reactor-state (react (key-event "q") r))
              #false)
(check-expect (reactor-state (foldl react r (time-steps 5)))
              5)

(check-expect (render-reactor r)
              EMPTY-SCENE)
(check-expect (render-reactor (react time-step r))
              (render-scene 1))
(check-expect (render-reactor (foldl react r (time-steps 50)))
              (render-scene 50))
(check-expect (render-reactor (react (key-event "q") r))
              EMPTY-SCENE)


;;; try: ;;;
; (run-reactor r)
