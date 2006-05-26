;; test the automaton construction
(require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 1)))

(require "automaton.ss")
(require "ahocorasick.ss")

(define automaton-test-suite
  (make-test-suite
   "Tests for the automaton construction"
   
   (make-test-case
    "empty case"
    (let ((tree (make)))
      (prepare tree)
      (assert-equal? 
       '(automaton root
                   (root : (else -> root)))
       (ahocorasick->sexp tree))))
   
   
   (make-test-case
    "test with one state"
    (let ((tree (make)))
      (add tree (list #\a) 'ok)
      (prepare tree)
      (assert-equal?
       '(automaton root
                   (root : 
                         (#\a -> state-1)
                         (else -> root))
                   (state-1 : 
                            (outputs (ok))
                            (fail -> root)))
       (ahocorasick->sexp tree))))
   
   (make-test-case
    "test with three states"
    (let ((tree (make)))
      (add tree (string->list "ba") "ba")
      (add tree (string->list "a") "a")
      (prepare tree)
      (assert-equal?
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
   

;   (make-test-case
;    "test with canonical example"
;    (let ((tree (make)))
;      (add tree (string->list "he"))
;      (add tree (string->list "she"))
;      (add tree (string->list "his"))
;      (add tree (string->list "hers"))
;      (prepare tree)
;      (assert-equal?
;       '(automaton root ...)
;       (ahocorasick->sexp tree))))

   
   
   ))

(test/text-ui automaton-test-suite)