#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (shape c)
  (let ((n (char->integer c)))
    (if (< n 68) (- n 65) (- n 88))))

(define (read-input)
  (define (readline input acc)
    (let ((line (get-line input)))
      (if (eq? line #!eof)
        acc
        (let-values (((x y) (sscanf line "%c %c")))
          (readline input (cons (cons (shape x) (shape y)) acc))))))
  (readline (filehandle 2) '()))

(define input (read-input))

(define (score you opp)
  (let ((s (+ you 1)))
    (cond
      [(= you opp) (+ s 3)]
      [(= (modulo (+ you 2) 3) opp) (+ s 6)]
      [else s])))

(define (game1 in acc)
  (if (null? in) acc
    (let* ((r (car in)) (you (cdr r)) (opp (car r)))
      (game1 (cdr in) (+ acc (score you opp))))))

(write-part1 (game1 input 0))

(define (get-move out opp)
  (cond
    [(= out 0) (modulo (+ opp 2) 3)]
    [(= out 1) opp]
    [(= out 2) (modulo (+ opp 1) 3)]))

(define (game2 in acc)
  (if (null? in) acc
    (let* ((r (car in)) (out (cdr r)) (opp (car r)))
      (let ((you (get-move out opp)))
        (game2 (cdr in) (+ acc (score you opp)))))))

(write-part2 (game2 input 0))
