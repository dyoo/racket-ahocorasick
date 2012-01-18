#lang racket/base
(require (prefix-in a: "ahocorasick.ss")
         (prefix-in s: "state.ss")
         (prefix-in q: "queue.ss"))
(require racket/list)
(provide ahocorasick->sexp)



;; FIXME: turn these into parameters to avoid issues with threading.
(define *state-counter* 0)
(define *state-hash-table* (make-hash))
;; new-state-name: -> symbol
;; generates new unique names for states
(define (new-state-name)
  (set! *state-counter* (add1 *state-counter*))
  (string->symbol (string-append "state-" (number->string *state-counter*))))


;; reset-state-counter!: -> 
;; Resets the state counter back to zero.
(define (reset-state-parameters!)
  (set! *state-counter* 0)
  (set! *state-hash-table* (make-hash)))



;; state-name: AhoCorasickTree state -> symbol
;; Given the state, returns the state name.  We guarantee that the root state is always called 'root.
(define state-name
  (lambda (tree state)
    (let ((lookup (hash-ref *state-hash-table* state (lambda () #f))))
      (when (not lookup)
        (hash-set! *state-hash-table* state
                         (if (eq? state (a:root tree))
                             'root
                             (new-state-name))))
      (hash-ref *state-hash-table* state))))


;; Just a little syntax to make it trivial to add to the front of lists.
(define-syntax set-front!
  (syntax-rules ()
    [(set-front! pair-var val)
     (set! pair-var (cons val pair-var))]))



;; ahocorasick->sexp: AhoCorasickTree -> sexp
;; returns a sexp representation of the states in the given ahocorasick tree.  A little ugly.
;; examples live in test-automaton.ss.
(define (ahocorasick->sexp tree)
  (define collected-state-sexps '())
  
  ;; abbr. for state-name
  (define (s-name state)
    (state-name tree state))
  
  ;; visit each state in turn, and write out their transition table.
  (define (visit state)
    (let [(sexp-rev (list ': (s-name state)))]
      
      ;; add the outputs
      (when (not (empty? (s:output state)))
        (set-front! sexp-rev (list 'outputs (s:output state))))
      
      ;; regular transitions
      (let loop ((outs (s:out-labels state)))
        (when (not (empty? outs))
          (set-front! sexp-rev (list (first outs)
                                     '->
                                     (s-name (s:goto state (first outs)))))
          (loop (rest outs))))
      
      ;; add either the else or fail transitions
      (if (not (eq? (a:root tree) state))
          (set-front! sexp-rev (list 'fail '-> (s-name (s:fail state))))
          (set-front! sexp-rev (list 'else '-> (s-name state))))        
      
      ;; finally, add all those collected annotations to our sexp list.
      (set-front! collected-state-sexps (reverse sexp-rev))))
  
  
  (begin
    (reset-state-parameters!)
    (s:breadth-first visit (q:insert q:empty (a:root tree)))
    (append '(automaton root) (reverse collected-state-sexps))))



