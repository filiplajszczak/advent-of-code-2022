#!/usr/bin/guile -s
!#

(use-modules ((ice-9 match) #:select (match))
             ((srfi srfi-1) #:select (first fold last lset-adjoin))
             ((srfi srfi-9 gnu) #:select (define-immutable-record-type))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (flatten product)))

(define-immutable-record-type <bridge>
  (bridge counter head tail)
  bridge?
  (counter bridge-counter set-bridge-counter)
  (head bridge-head set-bridge-head)
  (tail bridge-tail set-bridge-tail))

(define (move-head bridge-record direction)
  (let* ([head (bridge-head bridge-record)]
         [x (car head)]
         [y (cdr head)])
    (match direction
     ["U" (set-bridge-head bridge-record (cons x (+ y 1)))]
     ["D" (set-bridge-head bridge-record (cons x (- y 1)))]
     ["R" (set-bridge-head bridge-record (cons (+ x 1) y))]
     ["L" (set-bridge-head bridge-record (cons (- x 1) y))])))

(define (move-tail moved-head)
  (let* ([head (bridge-head moved-head)]
         [tail (bridge-tail moved-head)]
         [head-x (car head)]
         [head-y (cdr head)]
         [tail-x (car tail)]
         [tail-y (cdr tail)])
    (cond [(= 2 (- head-x tail-x)) (set-bridge-tail moved-head (cons (+ tail-x 1) head-y))]
          [(= -2 (- head-x tail-x)) (set-bridge-tail moved-head (cons (- tail-x 1) head-y))]
          [(= 2 (- head-y tail-y)) (set-bridge-tail moved-head (cons head-x (+ tail-y 1)))]
          [(= -2 (- head-y tail-y)) (set-bridge-tail moved-head (cons head-x (- tail-y 1)))]
          [else moved-head])))

(define (move bridge-record direction)
  (let ([moved-head (move-head bridge-record direction)])
    (move-tail moved-head)))

(define (process-command order bridge-record)
  (let process-step ([bridge-record bridge-record]
                     [direction (car order)]
                     [steps (cdr order)])
    (if (zero? steps)
        bridge-record
        (let ([moved (move bridge-record direction)])
          (process-step (set-bridge-counter moved
                               (lset-adjoin equal?
                                            (bridge-counter moved)
                                            (bridge-tail moved)))
                        direction
                        (- steps 1))))))

(define (part-1 filename)
  (length (bridge-counter (fold process-command (bridge '((0 . 0)) '(0 . 0) '(0 . 0)) (input filename)))))

(define (part-2 filename)
  (input filename))

(define (input filename)
  (map
   (λ (line)
     (let ([splitted (string-split line #\space)])
       (cons (first splitted)
             (string->number (last splitted)))))
   (read-lines filename)))


;; Part 1
(display (part-1 "day09.in"))
(newline)
;; Part 2
;; (display (part-2 "day09.in"))
;; (newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 13 (part-1 "day09-example.in"))
;; (test-equal "Test part 2" 8 (part-2 "day09-example.in"))

(test-end "example")
