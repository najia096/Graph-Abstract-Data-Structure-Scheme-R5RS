;; Please refer to the Basic_Data_Type report document for relevant information about the functions and proofs

(define (make-empty-set)
  '())

(define (set-insert x s)
  (if (member x s)
      s
      (cons x s)))

(define (set-member? x s)
  (if (member x s) #t #f))

(define (set-union set1 set2)
  (cond ((null? set1) set2)
        ((set-member? (car set1) set2)
         (set-union (cdr set1) set2))
        (else (set-union (cdr set1) (set-insert (car set1) set2)))))

(define (set-intersection set1 set2)
  (cond ((or (null? set1) (null? set2))'())
        ((set-member? (car set1) set2)
         (set-insert (car set1)
                     (set-intersection (cdr set1) set2)))
        (else (set-intersection (cdr set1) set2))))


(define (set-delete x s)
  (if (null? s)
      '()
      (if (eq? x (car s))
          (cdr s)
          (cons (car s) (set-delete x (cdr s))))))


;;;;=============================set testing==============================================

(define s (set-insert 'a (set-insert 'b (set-insert 'c (make-empty-set)))))
(define s1 (set-delete 'b s)) ; returns (c a)
(display "testing insert s :") s
(display "testing delete s1:") s1



(define set-0 (make-empty-set))
(define set-1 (set-insert 'a set-0))
(display "set-1:") set-1

(define set-2 (set-insert 'b set-1))
(display "set-2:") set-2

(define set-3 (set-insert 'c set-2))
(display "set-3:") 
set-3 ;; returns (c b a)

(display "set-2 union set-3:")
(set-union set-2 set-3) ;; returns (c b a)

(display "set-intersection set-1 set-2:")
(set-intersection set-1 set-2)















