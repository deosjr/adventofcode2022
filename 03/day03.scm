#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (read-input)
  (define (readline input acc)
    (let ((line (get-line input)))
      (if (eq? line #!eof)
        acc
        (readline input (cons (string->list line) acc)))))
  (readline (filehandle 3) '()))

(define input (read-input))

(define (take n s)
  (cond
    [(null? s) (cons '() '())]
    [(= n 0) (cons '() s)]
    [else
      (let ((rec (take (- n 1) (cdr s))))
        (cons (cons (car s) (car rec)) (cdr rec)))]))

(define (contains? s c)
  (cond
    [(null? s) #f]
    [(eq? (car s) c) #t]
    [else (contains? (cdr s) c)]))

(define (overlap a b)
  (cond
    [(null? a) #f]
    [(contains? b (car a)) (car a)]
    [else (overlap (cdr a) b)]))

(define (priority c)
  (let ((n (char->integer c)))
    (if (> n 90) (- n 96) (- n 38))))

(define (part1 in)
  (if (null? in) 0
    (let* ((s (car in))
           (n (length s))
           (split (take (/ n 2) s))
           (c (overlap (car split) (cdr split))))
      (+ (priority c) (part1 (cdr in))))))

(write-part1 (part1 input))
