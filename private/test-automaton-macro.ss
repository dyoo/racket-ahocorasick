#lang racket/base
(require "automaton-macro.ss")
(require (prefix-in list: racket/list))



(define simple-automaton (automaton 
                          root 
                          (root : 
                                (#\b -> state-1)
                                (#\a -> state-2)
                                (else -> root))
                          (state-1 :
                                   (#\a -> state-3)
                                   (fail -> root))
                          (state-2 :
                                   (outputs ("a"))
                                   (fail -> root))
                          (state-3 :
                                   (outputs ("a" "ba"))
                                   (fail -> state-2))
                          ))

;; run-automaton: automaton 
(define (run-automaton my-automaton s)
  (let ((results '()))
    (my-automaton (string->list s) 
               (lambda (outs) (set! results (cons outs results)))
               list:empty? list:first list:rest)
    (reverse results)))


(display (run-automaton simple-automaton "aa a bab"))
;; make sure this case is fixed.
;  (test-automaton my-automaton "aa a bab")
;(a)
;(a)
;(a)
;(a ba)
;(a)

