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
  (define (reversing list1 list2)
    (if (null? list1)
       list2
       (reversing (cdr list1) (cons (car list1) list2))))
  (reversing list1 (list)))

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
  (if (= (remainder (car a) 2) 0)
  (lambda (x) (= (remainder x 2) 0))
  (lambda (x) (not (= (remainder x 2) 0)))))


(define (same-par list1 list2 f)
  (if (null? list1)
    list2
    (same-par (cdr list1) (append-last-if list2 (car list1) f) f)))

(define (append-last-if lista a function)
  (if (function a)
    (cons a lista)
    lista))

;2.21 -- square a list with cons and with map
(define (square-list-cons items)
(if (null? items)
    nil
    (cons ((lambda (x) (* x x)) (car items)) (square-list-cons (cdr items)))))

  
(define (square-list-map items)
(map (lambda (x) (* x x))items))


;2.23 - the for each expresion
;;we use the cond and else expresion because in the else body we can have multiple expresions
(define (for-each-a proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (for-each-a proc (cdr items)))))

;2.27 - deep reverse: reverse the list that are inside the list
(define (deep-reverse list1)
  (define (deep-reversing list1 list2)
    (cond ((null? list1) list2)
          ;;if the (car list1) is a pair it means that is a list there it then reverses the list and then reverses the elements
          ((pair? (car list1)) (deep-reversing (cdr list1) (cons (deep-reversing-inner-list list1) list2))) 
          ((deep-reversing (cdr list1) (cons (car list1) list2)))))
  (define (deep-reversing-inner-list items)
    (deep-reversing (car items) (list)))
  (deep-reversing list1 (list)))

;;2.28 fringe list - get a tree of lists and return a list tha contains the elements of the list
(define (fringe items)
  (define (fringe-inner lista listb)
  (cond ((null? lista) listb)
        ((pair? (car lista)) (fringe-inner (cdr lista) (fringe-inner (car lista) listb)))
        (else (fringe-inner (cdr lista) (cons (car lista) listb)))))
  (reverse (fringe-inner items '())))

;2.29 mobil and branches

(define (make-mobile left right) (list left right))
(define (make-branch length struct) (list length struct))

;a - getters for mobile and branch
(define (left-branch mobil) (car mobil))
(define (right-branch mobil) (car (cdr mobil)))
(define (branch-length branch) (car branch))
(define (branch-struct branch) (car (cdr branch)))

;b -- total weight
(define (total-weight mobil)
  (+ (total-weight-branch (left-branch mobil))
     (total-weight-branch (right-branch mobil)))) 

(define (total-weight-branch branch)
  (if (pair? (branch-struct branch))
      (total-weight (branch-struct branch))
      (branch-struct branch)))

;c balanced branch
(define (torque-calc length weigth)
  (* length weigth))

(define (branch-torque branch)
  (* (branch-length branch)
     ;;calculates the total length atachet to the branch
     (if (pair? (branch-struct branch))
                       (total-weight-branch branch)
                       (branch-struct branch))))

(define (is-balanced-branch branch)
  (if (not (pair? (branch-struct branch)))
      #t
      (is-balanced-mobil (branch-struct branch))))

;;works but is ineficient because it goes trough the tree 3 times
(define (is-balanced-mobil mobil)
        ;check if the left branch is balanced
  (cond ((not (is-balanced-branch (left-branch mobil))) #f)
        ;check if the right branch is balanced
        ((not (is-balanced-branch (right-branch mobil))) #f)
        ;;check if the weigth are equal
        ((not (= (branch-torque (left-branch mobil)) (branch-torque (right-branch mobil)))) #f)
        ;if the above are false the retur true
        (else #t)))

;d if the definition for mobil changes to:
(define (make-mobile-new left right) (cons left right))
(define (make-branch-new length structure) (cons length structure))

;we need to only change
(define (right-branch-new mobil) (cdr mobil))
(define (branch-struct-new branch) (cdr branch))

;;2.30
(define (square-tree-no-map tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-no-map (car tree))
                    (square-tree-no-map (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda(sub-tree)
         (if (not (pair? sub-tree))
             (* sub-tree sub-tree)
             (square-tree-map sub-tree)))
        tree)) 

;2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub-set)
                            (append (list (car s)) sub-set)) rest)))))

;;2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
(accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (items)
                         (if (pair? items)
                             (count-leaves items)
                             1))
                       t))) 

;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (enumerate-first seqs))
            (accumulate-n op init (remove-first seqs)))))

(define (enumerate-first seq)
  (if (null? seq)
      nil
      (cons (car (car seq)) (enumerate-first (cdr seq)))))

(define (remove-first seq)
  (if (null? seq)
      nil
      (cons (cdr (car seq)) (remove-first (cdr seq)))))

;;the more elegant solution
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;2.37 - matrix operations
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m))) 

;2.40
(define (unique-pairs n) (flatmap (lambda (i)
                              (map(lambda (j) (list i j))
                                  (enumerate-interval 1 i)))
                            (enumerate-interval 1 n)))

;2.41
(define (unique-triplets n) (flatmap (lambda (i) (flatmap (lambda (j)(map (lambda (k) (list i j k))
                                          (enumerate-interval 1 (- j 1)))) 
                      (enumerate-interval 1 (- i 1))))   
         (enumerate-interval 1 n)))

(define (make-triple-sum triple)
  (append triple ( list (accumulate + 0 triple))))

(define (triple-sum-to-n n s) (map make-triple-sum (filter (lambda (items) (>= s (accumulate + 0 items))) (unique-triplets n))))

;;2.54 equality of lists
(define (equal? items1 items2)
  (cond ((and (null? items1) (null? items2)) true)
        ;;if the car of the lists are one element
        ((and (not (pair? (car items1))) (not (pair? (car items2)))) (if (eq? (car items1) (car items2))
                                                                         (equal? (cdr items1) (cdr items2))
                                                                         false))
        ;;if the car of the list are a list
        ((and (pair? (car items1)) (pair? (car items2))) (if (equal? (car items1) (car items2))
                                                             (equal? (cdr items1) (cdr items2))
                                                             false))
        (else false)))

