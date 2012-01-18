#lang racket/base
;; test the automaton construction
(require rackunit
         rackunit/text-ui)

(require "automaton.ss")
(require "ahocorasick.ss")

(define automaton-test-suite
  (test-suite
   "Tests for the automaton construction"
   
   (test-case
    "empty case"
    (let ((tree (make)))
      (prepare tree)
      (check-equal? 
       '(automaton root
                   (root : (else -> root)))
       (ahocorasick->sexp tree))))
   
   
   (test-case
    "test with one state"
    (let ((tree (make)))
      (add tree (list #\a) 'ok)
      (prepare tree)
      (check-equal?
       '(automaton root
                   (root : 
                         (#\a -> state-1)
                         (else -> root))
                   (state-1 : 
                            (outputs (ok))
                            (fail -> root)))
       (ahocorasick->sexp tree))))
   
   (test-case
    "test with three states"
    (let ((tree (make)))
      (add tree (string->list "ba") "ba")
      (add tree (string->list "a") "a")
      (prepare tree)
      (check-equal?
       '(automaton 
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
                  (fail -> state-2)))
       (ahocorasick->sexp tree))))
   

;   (test-case
;    "test with canonical example"
;    (let ((tree (make)))
;      (add tree (string->list "he"))
;      (add tree (string->list "she"))
;      (add tree (string->list "his"))
;      (add tree (string->list "hers"))
;      (prepare tree)
;      (check-equal?
;       '(automaton root ...)
;       (ahocorasick->sexp tree))))

   
   
   ))

(run-tests automaton-test-suite)