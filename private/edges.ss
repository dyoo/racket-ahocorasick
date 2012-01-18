#lang racket/base

(require racket/match)

(provide make put! get labels states)
;;; I encapsulate the Edges implementation with a unit here to make
;;; sure that the only way I'm accessing edges is through make-edges,
;;; set-edge, and follow-edge.



;;; Edges: (union DenseEdges SparseEdges)
;;; 
;;; DenseEdges use a hashtable representation
;;; SparseEdges use an association list
(define-struct DenseEdges (table))
(define-struct SparseEdges (table) #:mutable)


;;; The depth of the state that controls if we use a dense or sparse
;;; edge implementation
(define *density-threshold* 3)


;;; make: number -> Edges
;;; 
;;; Constructs a new Edges object.
(define (make depth)
  (if (< depth *density-threshold*)
      (make-DenseEdges (make-hash))
      (make-SparseEdges (list))))


;;; put!: Edges label State -> void
;;; 
;;; Adds a new binding of the label.
(define (put! edges label state)
  (match edges
    [(struct DenseEdges (table))
     (hash-set! table label state)
     (void)]
    [(struct SparseEdges (table))
     (set-SparseEdges-table! edges
                             (cons (cons label state) table))
     (void)]
    [else (raise-type-error 'set-edge "Edges" edges)]))




;;; get: Edges label -> (union State #f)
;;;
;;; If the Edges has a mapped State to the label, returns that
;;; State.  Otherwise, returns void.
(define get
  ;;; Just a fallback value for hashtable lookup  
  (let ((*empty-thunk* (lambda () #f)))
    
    (lambda (edges label)
      (match edges
        [(struct DenseEdges (table))
         (hash-ref table label *empty-thunk*)]
        [(struct SparseEdges (table))
         (let ((lookup (assv label table)))
           (if lookup
               (cdr lookup)
               #f))]
        [else (raise-type-error 'set-edge "Edges" edges)]))))


;;; keys: Edges -> (listof label)
;;; Returns a list of labels
(define (labels edges)
  (match edges
    [(struct DenseEdges (table))
     (hash-map table (lambda (k v) k))]
    [(struct SparseEdges (table))
     (map car table)]
    [else (raise-type-error 'set-edge "Edges" edges)]))


;;; values: Edges -> (listof label)
;;; Returns a list of States
(define (states edges)
  (match edges
    [(struct DenseEdges (table))
     (hash-map table (lambda (k v) v))]
    [(struct SparseEdges (table))
     (map cdr table)]
    [else (raise-type-error 'set-edge "Edges" edges)]))


