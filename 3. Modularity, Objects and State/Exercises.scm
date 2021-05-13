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

;;3.3 - 3.4
(define (monitor-pass)
    (let ((counter 0))
      (lambda (arg)
        (cond ((eq? arg 'show) counter)
              ((eq? arg 'reset) (set! counter 0))
              (else (set! counter (+ counter 1)))))))

(define (wrong-password a)
  "Wrong Password")

(define (call-the-cops a)
  "ninonononononon")

(define (make-account balance password)
  
  (define monitor (monitor-pass))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (>= (monitor 'show) 7)
        call-the-cops
        (if (not (eq? pass password))
            (if (>= (monitor 'show) 7)
                call-the-cops
                (begin (monitor 1) wrong-password))
            (begin (monitor 'reset) (cond ((eq? m 'withdraw) withdraw)
                                          ((eq? m 'deposit) deposit)
                                          (else (error "Unknown request -- MAKE-ACCOUNT"
                                                       m)))))))
