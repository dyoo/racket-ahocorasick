#lang racket/base

(require "ahocorasick.ss")
(require (prefix-in s: "state.ss"))
(require rackunit)
(require rackunit/text-ui)


(define test-ahocorasick-suite
  (test-suite
   "ahocorasick tests"

   (test-case
    "empty tree has size=1"
    (let ((tree (make)))
      (prepare tree)
      (check-equal? 1 (size tree))))

   
   (test-case
    "adding single character keyword"
    (let ((tree (make)))
      (string-add tree "h")
      (prepare tree)
      (check-equal? 2 (size tree))))

   
   (test-case
    "adding word"
    (let ((tree (make)))
      (string-add tree "he")
      (prepare tree)
      (check-equal? 3 (size tree))))


   (test-case
    "adding example from the paper
         Efficient String Matching: An Aid to Bioliographic Search"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (string-add tree "his")
      (string-add tree "hers")
      (prepare tree)
      (check-equal? 10 (size tree))))   


   
   (test-case
    "Test goto, fail, and output functions of small set"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (prepare tree)
      (let* ((state0 (root tree))
             (state1 (s:goto state0 #\h))
             (state2 (s:goto state1 #\e))
             (state3 (s:goto state0 #\s))
             (state4 (s:goto state3 #\h))
             (state5 (s:goto state4 #\e)))
        (check-equal? 6 (size tree))
        
        (check-equal? state0 (s:fail state1))
        (check-equal? state0 (s:fail state2))
        (check-equal? state0 (s:fail state3))
        (check-equal? state1 (s:fail state4))
        (check-equal? state2 (s:fail state5))

        (check-equal? '() (s:output state0))
        (check-equal? '() (s:output state1))
        (check-equal? (list "he") (s:output state2))
        (check-equal? '() (s:output state3))
        (check-equal? '() (s:output state4))
        (check-equal? (list "he" "she") (s:output state5))
        )))
        


   (test-case
    "More comprehensive test of the goto, fail, and output functions."
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (string-add tree "his")
      (string-add tree "hers")
      (prepare tree)
      (let* ((state0 (root tree))
             (state1 (s:goto state0 #\h))
             (state2 (s:goto state1 #\e))
             (state3 (s:goto state0 #\s))
             (state4 (s:goto state3 #\h))
             (state5 (s:goto state4 #\e))
             (state6 (s:goto state1 #\i))
             (state7 (s:goto state6 #\s))
             (state8 (s:goto state2 #\r))
             (state9 (s:goto state8 #\s)))
        (check-equal? 10 (size tree))

        (check-equal? '()
                       (filter void? (list state0 state1 state2
                                           state3 state4 state5
                                           state6 state7 state8
                                           state9)))
        (for-each (lambda (s) (check-equal? state0 (s:fail s)))
                  (list state1 state2 state3 state6 state8))
        (check-equal? state1 (s:fail state4))
        (check-equal? state2 (s:fail state5))
        (check-equal? state3 (s:fail state7))
        (check-equal? state3 (s:fail state9))

        (for-each (lambda (s) (check-equal? '() (s:output s)))
                  (list state1 state3 state4 state6 state8))
        (check-equal? (list "he") (s:output state2))
        (check-equal? (list "he" "she") (string-sort (s:output state5)))
        (check-equal? (list "his") (s:output state7))
        (check-equal? (list "hers") (s:output state9)))))


   (test-case
    "simple negative search"
    (let ((tree (make)))
      (string-add tree "hi")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hola")))
        (check-false first-result))))

   
   
   (test-case
    "simple search"
    (let ((tree (make)))
      (string-add tree "hi")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hi")))
        (check-equal? (list "hi") (result-output first-result)))))


   (test-case
    "simple search with two outputs"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (prepare tree)
      (let* ((first-result (start-string-search tree "shell")))
        (check-equal? (list "he" "she") (string-sort (result-output first-result))))))


   (test-case
    "simple search with false continuation"
    (let ((tree (make)))
      (string-add tree "hi")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hihey")))
        (check-equal? (list "hi") (result-output first-result))
        (check-false (continue-search first-result)))))


   (test-case
    "another simple test with true continuation"
    (let ((tree (make)))
      (string-add tree "hi")
      (string-add tree "hiya")
      (string-add tree "ya")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hiya"))
             (second-result (continue-search first-result))
             (third-result (continue-search second-result)))
        (check-equal? (list "hi") (result-output first-result))
        (check-equal? (list "hiya" "ya") (string-sort (result-output second-result)))
        (check-false third-result))))

   
   (test-case
    "test empty tree usage"
    (let ((tree (make)))
      (prepare tree)
      (check-false (start-string-search tree "zero"))))
   

   (test-case
    "make sure we catch unprepared usage and give a warning"
    (let ((tree (make)))
      (check-exn exn:fail? (lambda () (start-string-search tree "antigone")))))


   (test-case
    "make sure we catch bad usage and give a warning"
    (let ((tree (make)))
      (prepare tree)
      (check-exn exn:fail? (lambda () (string-add tree "antigone")))))


   
   (test-case
    "test position information"
    (let ((tree (make)))
      (string-add tree "adenine")
      (string-add tree "thymine")
      (string-add tree "cytosine")
      (string-add tree "guanine")
      (string-add tree "uracil")
      (prepare tree)
      (let* ((first-result
              (start-string-search
               tree
               "uracil is found only in rna and thymine is found only in dna"))
             (second-result (continue-search first-result))
             (last-result (continue-search second-result)))
        (check-false last-result)
        (check-equal? (list "uracil") (result-output first-result))
        (check-equal? 6 (result-end-position first-result))
        (check-equal? (list "thymine") (result-output second-result))
        (check-equal? 39 (result-end-position second-result)))))

   
   (test-case
    "test string-search"
    (let ((tree (make)))
      (string-add tree "0011")
      (string-add tree "10")
      (prepare tree)
      (check-equal? '(("0011" 0 4)
                       ("10" 3 5)
                       ("10" 5 7))
                     (string-search tree "0011010001"))))

   (test-case
    "test string-search-2"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (prepare tree)
      (check-equal? '(("he" 1 3)
                       ("she" 0 3))
                     (string-search tree "she"))))


   
;;    (test-case
;;     "test end position"
;;     (let ((tree (make)))
;;       (string-add tree "sup")


   
   ))



(define (string-sort strings)
  (sort strings string<?))

(run-tests test-ahocorasick-suite)
