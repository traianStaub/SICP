(define numbers (list 15 247 8 1 8 6 6))
(car numbers)
;cons create
;car first element
;cdr remaining list

;2.17 - retrun last number
(define (last-pair list1)
  (if (null? (cdr list1))
      (list (car list1))
      (last-pair (cdr list1))))

;2.18 - reverse lists
(define (reverse list1)
  (reversing list1 (list))
  )

(define (reversing list1 list2)
    (if (null? list1)
       list2
       (reversing (cdr list1) (cons (car list1) list2))
  ))

;2.19 - coin change
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc-list amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc-list amount
                (except-first-denomination coin-values))
              (cc-list (- amount (first-denomination-list coin-values))
                  coin-values)))))

(define (no-more? lista) 
  (if (null? lista)
    #t
    #f))

(define (first-denomination-list lista)
  (car lista))

(define (except-first-denomination lista)
  (cdr lista))

(cc-list 100 us-coins)

;2.20 - same parity

(define (same-parity . a) 
  (let ((f (first-parity a)))
  (same-par a (list) f)))

(define (first-parity a)
  (if (= (mod (car a) 2) 0)
  (lambda (x) (= (mod x 2) 0))
  (lambda (x) (not (= (mod x 2) 0)))))


(define (same-par list1 list2 f)
  (if (null? list1)
    list2
    (same-par (cdr list1) (append-last-if list2 (car list1) f) f)))

(define (append-last-if lista a function)
  (if (function a)
    (cons a lista)
    lista))




