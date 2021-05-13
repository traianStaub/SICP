;;3.1
(define (make-acc sum)
 (define (add-number number)
   (begin (set! sum (+ sum number)) sum))
  add-number)

(define acc (make-acc 0))

;;3.2
(define (make-monitored f)
  (let ((counter 0))
    (lambda (arg)
            (cond ((eq? arg 'show) counter)
                  ((eq? arg'reset) (set! counter 0))
                  (else (set! counter (+ counter 1)) (f arg))))))
