;;; State graph implementation specialized for Aho-corasick state
;;; representation.

(module state mzscheme
  (require (prefix e: "edges.ss")
           (prefix q: "queue.ss")
           (lib "plt-match.ss"))

  (provide make
           goto
           fail
           output
           (rename set-State-output! set-output!)
           add-output!
           (rename set-State-fail! set-fail!)
           extend
           extend*
           out-labels
           out-states
           breadth-first)
  

;;; Defines the State structure of an AhoCorasickTree.
  (define-struct State (depth edges fail output))
  

;;; make: number -> State
;;;
;;; Builds a new state with the given depth.
  (define (make depth)
    (make-State depth (e:make 0) (void) (list)))


;;; add-output!: State elt -> (void)
;;;
;;; Adds a new element to the output set of a state.
  (define (add-output! state elt)
    (match state
           [(struct State (depth edges fail output))
            (when (not (member elt output))
              (set-State-output! state (cons elt output)))]
           [else (raise-type-error 'add-output "State" state)]))
  
  
;;; out-labels: State -> (listof label)
;;;
;;; Returns all the labels leading out of this state.  
  (define (out-labels state)
    (match state
           [(struct State (depth edges fail output))
            (e:labels edges)]))


;;; out-states: State -> (listof State)
;;;
;;; Returns all the States leading out of this state.
  (define (out-states state)
    (match state
           [(struct State (depth edges fail output))
            (e:states edges)]))

  
  
;;; goto: State label -> (union void State)
;;;
;;; Follows the edge across the given label.  If no such edge exists,
;;; returns void.
  (define (goto state label)
    (match state
           [(struct State (depth edges fail output))
            (let ((x (e:get edges label)))
              (if x
                  x
                  (if (= depth 0) state (void))))]
           [else (raise-type-error 'goto "State" state)]))


  (define (fail state)
    (match state
           [(struct State (depth edges fail output))
            fail]
           [else (raise-type-error 'fail "State" state)]))
            
  
  (define (output state)
    (match state
           [(struct State (depth edges fail output))
            output]
           [else (raise-type-error 'output "State" state)]))


  
;;; extend: State label -> State
;;;
;;; Extends a new State off an existing state, using the label.  Returns
;;; the new state.
  (define (extend state label)
    (match state
           [(struct State (depth edges fail output))
            (let ((new-state (make (+ depth 1))))
              (e:put! edges label new-state)
              new-state)]
           [else (raise-type-error 'goto "State" state)]))


  
;;; extend*: State (listof label) -> State
;;; 
;;; Extends possibly several states, given a list of labels.  Returns
;;; the last state extended.
  (define (extend* state labels)
    (let loop ((n state)
               (labels labels))
      (if (null? labels)
          n
          (if (e:get (State-edges n) (car labels))
              (loop (goto n (car labels)) (cdr labels))
              (loop (extend n (car labels)) (cdr labels))))))




;;; Depth first search iteration, given an initial queue of states
  (define (breadth-first f Q)
    (let loop ((Q Q))
      (if (q:empty? Q)
          (void)
          (let*-values ([(x Q) (q:remove Q)]
                        [(Q)   (q:insert* Q (out-states x))])
            (begin
              (f x)
              (loop Q))))))


  )
