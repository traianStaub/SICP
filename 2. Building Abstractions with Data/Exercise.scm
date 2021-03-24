(define numbers (list 15 247 8 1 8 6 6))
(car numbers)
;cons create
;car first element
;cdr remaining list

;2.17
(define (last-pair list1)
  (if (null? (cdr list1))
      (list (car list1))
      (last-pair (cdr list1))))

;2.18
(define (reverse list1)
  (reversing list1 (list))
  )

(define (reversing list1 list2)
    (if (null? list1)
       list2
       (reversing (cdr list1) (cons (car list1) list2))
  ))

;2.19


(reverse numbers)
