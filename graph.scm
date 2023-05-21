(load "./set.scm")

;; Please refer to the Basic_Data_Type report document for relevant information about the functions and proofs

(define (make-graph V E)
  (list V E))

(define (get-V g)
  (car g))

(define (get-E g)
  (cadr g))

(define (add-V v set)
  (set-insert v set))

(define (add-E e set)
  (set-insert e set))


;;;================================for adjacency list representation===============================

(define (make-node-l val)
  (list val '()))

(define (get-val node)
  (car node))

(define (get-edges-l node)
  (car (cdr node)))

(define (make-edge-l n1 new-node)
  (list (get-val n1) (cons (get-val new-node) (get-edges-l n1))))

(define node1 (make-node-l 1))
(define node2 (make-node-l 2))

;(display "here:")
;(define edge (make-edge-l node1 node2)) edge

(define (get-nodes-l graph)
  (map car graph))

(define (insert-node-l g x)
  (cons (make-node-l x) g))

(define (first-node g)
  (car g))

(define (rest-graph-l g)
  (cdr g))

(define (create-graph-l nodes)
  (cond ((null? nodes) '())
        (else (cons (list (car nodes) '()) (create-graph-l (cdr nodes))))))

(define (add-to-list adj-list x)
  (cond ((member? x adj-list) adj-list)
        (else (cons x adj-list))))


(define (add-to-visited node visited)
  (set-insert node visited))



(define (add-edge-l u v graph)
  (cond ((null? graph) '())
        ((equal? u (car (car graph))) (cons (list u (add-to-list (car (cdr (car graph))) v)) (add-edge-l u v (cdr graph))))
        ((equal? v (car (car graph))) (cons (list v (add-to-list (car (cdr (car graph))) u)) (add-edge-l u v (cdr graph))))
        (else (cons (car graph) (add-edge-l u v (cdr graph))))))


(define (make-graph-l g)
  (define g1 (create-graph-l (car g)))
   (define (make-graph-inner graph edges)
     (cond ((null? edges) graph)
           (else (make-graph-inner (add-edge-l (car (car edges)) (car (cdr (car edges))) graph) (cdr edges)))))
  (make-graph-inner g1 (car (cdr g))))



;;;===================================for adjacency matrix representation============================


(define (change-row row cell)
  (cond ((eq? (car (car row)) cell) (cons (cons cell #t) (cdr row)))
        (else (cons (car row) (change-row (cdr row) cell)))))

(define (add-edge mat edge)
  (cond ((null? mat) '())
        ((eq? (car (car mat)) (car edge)) (cons (list (car edge) (change-row (cadr (car mat)) (cadr edge))) (add-edge (cdr mat) edge)))
        ((eq? (car (car mat)) (cadr edge)) (cons (list (cadr edge)(change-row (cadr (car mat)) (car edge))) (add-edge (cdr mat) edge)))
        (else (cons (car mat) (add-edge (cdr mat) edge)))))

(define (add-all-edges edges mat)
  (cond ((null? edges) mat)
        (else (add-all-edges (cdr edges) (add-edge mat (car edges))))))

(define (make-graph-m gr)
  (define (add-row lbl)
    (cond ((null? lbl) '())
          (else (cons (cons (car lbl) #f) (add-row (cdr lbl))))))
  (define (empty-adj-mat-inner g)
    (cond ((null? g) '())
          (else (cons (list (car g) (add-row (car gr))) (empty-adj-mat-inner (cdr g))))))
  (add-all-edges (cadr gr) (empty-adj-mat-inner (car gr)))
  )




