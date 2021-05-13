;;3.1

(define (make-acc sum)
 (define (add-number number)
   (begin (set! sum (+ sum number)) sum))
  add-number)

(define acc (make-acc 0))
