;;comparison ops
;;eqv?
(define eqv?
  (lambda (x y)
    (if (and (number? x) (number? y))
      (= x y) 
      (eq? x y))))
;;equal
(define (equal? a b)
  (cond ((eqv? a b)#t)
    ((and (pair? a)
    (pair? b)
    (equal? (car a) (car b))
    (equal? (cdr a) (cdr b))) #t)
    (else #f)))

;;n-ary integer comparision ops
;;=
(define (= . l)
  (if (null? l) 0
    (b= (car l)
    (apply = (cdr l)))))
;;<
(define (< . l)
  (if (null? l) 0
    (b> (car l)
    (apply < (cdr l)))))
;;>
(define (> . l)
  (if (null? l) 0
    (b> (car l) 
    (apply > (cdr l)))))
;;<=
(define (<= . l)
  (if (null? l) 0
    (b< (car l) 
    (apply <= (cdr l)))))
;;>=
(define (>= . l)
  (if (null? l) 0
    (b< (car l)
    (apply >= (cdr l)))))

;;test predicates
;;zero?
(define zero?
  (lambda (x)
    (if (= x 0) #t #f)))
;;positive?
(define positive?
  (lambda (x)
    (if (> x 0) #t #f)))
;;negative?
(define negative?
  (lambda (x)
    (if (< x 0) #t #f)))
;;odd?
(define odd?
  (lambda (x)
    (cond ((= x 0) #f)
      ((= x 1) #t)
      ((positive? x) (odd? (- x 2)))
      ((negative? x) (odd? (+ x 2))))))
;;even?
(define even?
  (lambda (x)
    (cond ((= x 0) #t)
      ((= x 1) #f)
      ((positive? x) (even? (- x 2)))
      ((negative? x) (even? (+ x 2))))))
      
;;n-ary arithmetic ops
;;max
(define (max . l)
  (if (= (length l) 1)
    (car l)
    (if (null? l)
      (write "incorrect number of arguments")
      (if (> (car l) (apply max (cdr l)))
        (car l)
        (apply max (cdr l))))))
;;min
(define (min . l)
  (if (= (length l) 1)
    (car l)
    (if (null? l)
      (write "incorrect number of arguments")
      (if (< (car l) (apply min (cdr l)))
        (car l) 
        (apply min (cdr l))))))
;;+
(define (+ . l)
  (if (null? l) 0
    (b+ (car l)
    (apply + (cdr l)))))
;;-
(define (- . l)
  (if (null? l) 0
    (b- (car l)
    (apply + (cdr l)))))
;;*
(define (* . l)
  (if (null? l) 1
    (b* (car l)
    (apply * (cdr l)))))
    
;;boolean ops
;;not
(define not
  (lambda (x)
    (cond (x #f)
    (else #t))))
;;and
(define and
  (lambda (x y)
    (cond (x (cond (y #t)
      (else #f))) 
    (else #f))))
;;or
(define or
  (lambda (x y)
    (cond (x (cond (y #t)
      (else #t)))
    (else #f))))

;;list functions(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
;;List
(define (list . l)
  (if (null? l)
    '()
    l))
;;Length
(define (length x)
 (cond ((null? x) 0)
  ((+ 1 (length (cdr x))))))
;;Append
(define (append list1 list2)
 (cond ((null? list2) list1)
 (cond ((null? list1) list2)
   ((cons (car list1)
     (append (cdr list1) list2)))))
;;Reverse
(define (reverse lis)
 (if (null? lis)
   '()
   (append (reverse (cdr lis))
     (list (car lis)))))
     
;;set and association list ops
;;memq
(define (memq ele lis)
 (if (null? lis)
   #f
   (if (eq? (car lis) ele)
     lis
     (memq ele (cdr lis)))))
;;memv
(define (memv ele lis)
 (if (null? lis)
   #f
   (if (eqv? (car lis) ele)
     lis
     (memv ele (cdr lis)))))
;;member
(define (member ele lis)
 (if (null? lis)
   #f
   (if (equal? (car lis) ele)
     lis
     (member ele (cdr lis)))))
;;assq
(define (assq ele lis)
 (if (null? lis)
   #f
   (if (eq? (car (car lis)) ele)
     (car lis)
     (assq ele (cdr lis)))))
;;assv
(define (assv ele lis)
 (if (null? lis)
   #f
   (if (eqv? (car (car lis)) ele)
     (car lis)
     (assv ele (cdr lis)))))
;;assoc
(define (assoc ele lis)
 (if (null? lis)
   #f
   (if (equal? (car (car lis)) ele)
     (car lis)
     (assoc ele (cdr lis)))))

;;higher-order functions
;;map
(define (map ele lis)
 (cond ((null? lis)
  '())
 ((pair? lis)
  (cons (ele (car lis))
    (map ele (cdr lis))))))
;;for-each
(define (for-each1 ele lis)
   (cond ((null? (cdr lis)) ; one elemnent list
    (ele (car lis)))
   (else
    (ele (car lis))
    (for-each1 ele (cdr lis)))))
