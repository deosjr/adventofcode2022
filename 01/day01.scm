#! /usr/bin/scheme --script
(load "lib/lib.scm")

(define (read-input)
  (define (readline input sum sums)
    (let ((line (get-line input)))
      (if (eq? line #!eof)
        sums
        (let ((n (sscanf line "%d")))
          (if (eq? n #f)
            (readline input 0 (cons sum sums))
            (readline input (+ sum n) sums))))))
  (readline (filehandle 1) 0 '()))

(define summed (sort > (read-input)))

(write-part1 (car summed))
(write-part2 (+ (car summed) (cadr summed) (caddr summed)))
