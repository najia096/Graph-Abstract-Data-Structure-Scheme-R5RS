(load "./graph.scm")
(load "./set.scm")

;; Please refer to the Basic_Data_Type report document for relevant information about the functions and proofs

;;=========computations for normal graph G(V, E)================================
;; neigbors of a node
(define (accumulate-left op init seq)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (op acc (car rest)) (cdr rest))))
  (iter init seq))


(define (get-neighbors graph node)
  (let ((edges (get-E graph)))
    (accumulate-left (lambda (acc e)
                 (let ((u (car e))
                       (v (cadr e)))
                   (cond ((equal? u node) (cons v acc))
                         ((equal? v node) (cons u acc))
                         (else acc))))
               '()
               edges)))





;;=========computations for adjacency list graph================================
;;neighbors/adjacent nodes of a node
(define (find-adj-l g n)
  (cond ((equal? n (get-val (first-node g))) (get-edges-l (first-node g)))
        (else (find-adj-l (rest-graph-l g) n))))


(define (member? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (member? x (cdr lst)))))



;;=========computations for adjacency matrix graph================================
;;neighbors/adjacent nodes of a node

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (extract-left-elements lst)
  (map car (filter (lambda (pair) (eq? (cdr pair) #t)) lst)))

(define (find-adj-matrix graph node)
  (cond ((null? graph) '())
        ((equal? node (caar graph)) (extract-left-elements (cadar graph)))
        (else (find-adj-matrix (cdr graph) node))))




(display "graph-computations and testing:")
(newline)

;;;;;===============For Testing normal-graph G(V,E)====================================================
(define ver (make-empty-set))
(display "insert vertex testing:")
(newline)
(define ver-set (add-V 'a ver)) ver-set
(define ver-set (add-V 'b ver-set)) ver-set
(define ver-set (add-V 'c ver-set)) ver-set


(define edg (make-empty-set))
(display "insert edges testing:")
(newline)
(define edg-set (add-E '(a b) edg)) edg-set
(define edg-set (add-E '(b c) edg-set)) edg-set
(define edg-set (add-E '(c a) edg-set)) edg-set

(display "graph with set of a ver-set and a set of edg-set:")
(define graph-VE (make-graph ver-set edg-set))
graph-VE
;;;;;===========================================================================================





;;;=======input testing for graph with list of V, E and set functionalities=================================
(define V (set-insert 'a (set-insert 'b (set-insert 'c (make-empty-set)))))
(define E (set-insert '(a b) (set-insert '(b c) (make-empty-set))))
(define G (make-graph V E))
(display "initial make-graph G:") G
(get-V G)
(get-E G)
(define V1 (set-insert '1 (set-insert '2 (set-insert '3 (set-insert '4 (set-insert '5 (make-empty-set)))))))
(define E1 (set-insert '(1 2) (set-insert '(1 5) (set-insert '(2 3) (set-insert '(2 5) (make-empty-set))))))
(define G1 (make-graph V1 E1))
(display "initial make-graph G1:") G1
(get-V G1)
(get-E G1)


;;;========================================testing===============================================
(newline)
(define gr-l (make-graph-l G1))
(display "G1 converted to adj-list gr-l:")
gr-l
(define gr-m (make-graph-m G1))
(display "G1 converted to adj-matrix gr-m:")
(newline)
gr-m

(newline)
(display "graph-VE:")
graph-VE
(display "graph-VE adj-list:")
(make-graph-l graph-VE)
(display "graph-VE adj-matrix:")
(make-graph-m graph-VE)

(newline)
(display "testing insert and add-edge of adj-lists")
(newline)
(define gr1 (insert-node-l gr-l 'a))
(define gr2 (insert-node-l gr1 'b)) gr2
(define gr2 (add-edge-l 'b 'a gr2)) gr2
(first-node gr2)
