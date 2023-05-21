(load "./basicdata.scm")

;; Please refer to the Algorithms_Docs report document for relevant information about the functions and proofs


(display "=====================algorithms start here==================================")
(newline)
(define ver (make-empty-set))
(define ver-set (add-V 'a ver))
(define ver-set (add-V 'b ver-set))
(define ver-set (add-V 'c ver-set))

(define edg (make-empty-set))
(define edg-set (add-E '(a b) edg))
(define edg-set (add-E '(b c) edg-set))
(define edg-set (add-E '(c a) edg-set))

;(display "graph with set of a ver-set and a set of edg-set:")
(define G1 (make-graph ver-set edg-set))


(define graph-adj-lst (make-graph-l G1))
;(display "graph-adj-lst:")
;graph-adj-lst

(define graph-adj-matrix (make-graph-m G1))

;; dfs
;; For this function we are providing an identifer along with graph and start node, which identifies the format of the graph
;; To call it for a Graph (G1) with a list of V and E, (dfs G1 'a 'normal-graph)
;; To call it for a Graph (graph-adj-lst) represented as adjaceny list, (dfs graph-adj-lst 'a 'adj-list)
;; To call it for a Graph (graph-adj-matrix) represented as adjaceny matrix, (dfs graph-adj-lst 'a 'adj-matrix)
(define (dfs graph start-node identifier)
  (define visited '())
  (define stack (push-l start-node (make-empty-stack-l)))

  (define (dfs-helper visited stack)
    (cond ((null? stack)
           (reverse visited))
          (else
           (let ((node (top-l stack))
                 (rest-stack (pop-l stack)))
             (if (member node visited)
                 (dfs-helper visited rest-stack)
                 (let ((adjacent-nodes (cond ((equal? identifier 'adj-list)
                                              (find-adj-l graph node))
                                             ((equal? identifier 'adj-matrix)
                                              (find-adj-matrix graph node))
                                             (else
                                              (get-neighbors graph node)))))
                   (dfs-helper (set-insert node visited) (append adjacent-nodes rest-stack))))))))

  (dfs-helper visited stack))




;; bfs
;; For this function we are providing an identifer along with graph and start node, which identifies the format of the graph
;; To call it for a Graph (G1) with a list of V and E, (bfs G1 'a 'normal-graph)
;; To call it for a Graph (graph-adj-lst) represented as adjaceny list, (bfs graph-adj-lst 'a 'adj-list)
;; To call it for a Graph (graph-adj-matrix) represented as adjaceny matrix, (bfs graph-adj-matrix 'a 'adj-matrix)
(define (bfs graph start-node identifier)
  (define visited '())
  (define queue (make-empty-q))

  (define (bfs-helper visited queue)
    (cond ((null? queue)
           (reverse visited))
          (else
           (let ((node (peek-front queue))
                 (rest-queue (pop-front queue)))
             (if (member node visited)
                 (bfs-helper visited rest-queue)
                 (let ((adjacent-nodes (cond ((equal? identifier 'adj-list)
                                              (find-adj-l graph node))
                                             ((equal? identifier 'adj-matrix)
                                              (find-adj-matrix graph node))
                                             (else
                                              (get-neighbors graph node)))))
                   (bfs-helper (set-insert node visited) (append rest-queue adjacent-nodes))))))))

  (bfs-helper visited (push-q start-node queue)))


;; acyclic?
;; For this function we are providing an identifer along with graph, which identifies the format of the graph
;; To call it for a Graph (G1) with a list of V and E, (acyclic? G1 'normal-graph)
;; To call it for a Graph (graph-adj-lst) represented as adjaceny list,(acyclic? graph-adj-lst 'adj-list)
;; To call it for a Graph (graph-adj-matrix) represented as adjaceny matrix,(acyclic? graph-adj-matrix 'adj-matrix)
(define (acyclic? graph identifier)
  (define (dfs-recur visited ancestors node)
    (if (member node visited)
        (member node ancestors)
        (let ((neighbors (cond ((equal? identifier 'adj-list)
                                (find-adj-l graph node))
                               ((equal? identifier 'adj-matrix)
                                (find-adj-matrix graph node))
                               (else
                                (get-neighbors graph node)))))
          (accumulate-left (lambda (acc neighbor)
                             (dfs-recur (set-insert node visited) (set-insert node ancestors) neighbor))
                           #f
                           neighbors))))

  (let ((nodes (if (equal? identifier 'adj-list)
                   (get-nodes-l graph)
                   (get-V graph))))
    (cond ((null? nodes) #t)
          (else
           (let ((visited (dfs-recur '() '() (car nodes))))
             (not visited))))))


;;;;=================================testing for the algorithms=========================================================

(newline)
;;created on line 27
(display "Normal Graph(V, E) G1:") G1
(display "G1 converted to graph-adj-lst:") graph-adj-lst
(display "G1 converted to graph-adj-matrix:") graph-adj-matrix
(newline)

(define graph '((A (B C))
                (B (C D))
                (C ())
                (D (A))
                (E (F))
                (F (E))))


(define graph2 '((A (B C))
                 (B (A C))
                 (C (A B))))

(display "Checkeing acyclic? G1 'normal-graph:")
(acyclic? G1 'normal-graph)

(display "Checkeing acyclic? graph-adj-lst 'adj-list:")
(acyclic? graph-adj-lst 'adj-list)

(display "Checkeing acyclic? graph-adj-matrix 'adj-matrix:")
(acyclic? graph-adj-lst 'adj-list)
(newline)


(display "dfs of G1 from a:")
(dfs G1 'a 'normal-graph)
(display "dfs of graph-adj-lst from a:")
(dfs graph-adj-lst 'a 'adj-list)
(display "dfs of graph-adj-matrix from a:")
(dfs graph-adj-matrix 'a 'adj-matrix)

(newline)
(display "bfs of G1 from a:")
(bfs G1 'a 'normal-graph)
(display "bfs of graph-adj-lst from a:")
(bfs graph-adj-lst 'a 'adj-list)
(display "bfs of graph-adj-matrix from a:")
(bfs graph-adj-matrix 'a 'adj-matrix)
(newline)


(define graph-nums '((0 1 2 3 4) ((0 1) (0 2) (0 3) (2 3) (2 4))))


(display "graph-nums:") graph-nums
(newline)
(display "graph-nums converted to adj-list:")
(make-graph-l graph-nums)
(newline)
(display "graph-nums converted to adj-matrix:")
(newline)
(make-graph-m graph-nums)
(display "dfs of graph1:")
;(dfs graph-nums '1 'adj-list)


(define graph3 '((a (b c d))
                 (b (a))
                 (c (a d e))
                 (d (c a))
                 (e (c))))

(display "graph3:") graph3
(display "dfs of graph3 from b:")
(dfs graph3 'b 'adj-list)







