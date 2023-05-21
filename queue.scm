;; Please refer to the Basic_Data_Type report document for relevant information about the functions and proofs

(define (make-empty-q)
  '())

(define (push-q x q)
  (cond ((null? q) (list x))
        (else (cons (car q) (push-q x (cdr q))))))

(define (pop-front q)
  (cdr q))

(define (peek-front q)
  (car q))

(define q (make-empty-q))



;(define q1 (push-q 'a q))
;(display "q1:")q1
;(define q2 (push-q 'b q1))
;(display "q2:")q2
;(define q3 (push-q 'c q2))
;(display "q3:")q3
;(define q4 (push-q 'd q3))
;(display "q4:")q4

;(define q5 (pop-front q4)) q5
;(define q6 (peek-front q5)) q6











