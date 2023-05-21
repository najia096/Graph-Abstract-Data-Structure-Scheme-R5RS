(define (make-empty-stack-l)
  '())

(define (push-l x s)
  (cons x s))

(define (top-l s)
  (car s))

(define (pop-l s)
  (cdr s))

(define (stack-empty-l? s)
  (null? s))



