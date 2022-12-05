#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (ice-9 pretty-print)
             ((f))
             ((algorithms)))

(define (parse-boxes line)
  (map
   (λ (chunk) (second chunk))
   (chunks-of
    (string->list line)
    4)))

(define (parse-command line)
  (let ([splitted (string-split line #\space)])
    (map string->number (filter
                         (λ (str)
                            (string-every char-numeric? str))
                         splitted))))

(define (parse lines boxes commands)
  (cond
   [(null? lines) (list
                   (map (compose string->list string-trim-right list->string)
                        (apply zip boxes))
                   (reverse commands))]
   [(or
     (string-null? (car lines))
     (char-numeric? (string-ref (car lines) 1)))
    (parse (cdr lines) boxes commands)]
   [(or
     (char-whitespace? (string-ref (car lines) 1))
     (char-upper-case? (string-ref (car lines) 1)))
    (parse
     (cdr lines)
     (cons (parse-boxes (car lines)) boxes)
     commands)]
   [(char=? (string-ref (car lines) 1) #\o)
    (parse (cdr lines) boxes (cons (parse-command (car lines)) commands))]))

(define (execute original-boxes boxes command index proc)
  (let ([move (first command)]
        [from (- (second command) 1)]
        [to (- (third command) 1)])
    (cond [(null? boxes) '()]
          [(eq? index from)
           (cons
            (drop-right (car boxes) move)
            (execute original-boxes (cdr boxes) command (+ index 1) proc))]
          [(eq? index to)
           (cons
            (append (car boxes) (proc (take-right (list-ref original-boxes from) move)))
            (execute original-boxes (cdr boxes) command (+ index 1) proc))]
          [else (cons
                 (car boxes)
                 (execute original-boxes (cdr boxes) command (+ index 1) proc))])))

(define (execute-all boxes commands proc)
  (if (null? commands)
      (list->string (map last boxes))
      (execute-all (execute boxes boxes (car commands) 0 proc) (cdr commands) proc)))

(define (part-1 filename)
  (let ([data (parse (input filename) '() '())])
    (execute-all (first data) (last data) reverse)))

(define (part-2 filename)
  (let ([data (parse (input filename) '() '())])
    (execute-all (first data) (last data) identity)))

(define (input filename)
   (read-lines filename))


;; Part 1
(display (part-1 "day05.in"))
(newline)
;; Part 2
(display (part-2 "day05.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" "CMZ" (part-1 "day05-example.in"))
(test-equal "Test part 2" "MCD" (part-2 "day05-example.in"))

(test-end "example")
