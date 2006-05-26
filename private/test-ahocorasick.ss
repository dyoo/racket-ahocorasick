(require (lib "list.ss"))
(require "ahocorasick.ss")
(require (prefix s: "state.ss"))
(require (planet "test.ss" ("schematics" "schemeunit.plt" 1)))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))


(define test-ahocorasick-suite
  (make-test-suite
   "ahocorasick tests"

   (make-test-case
    "empty tree has size=1"
    (let ((tree (make)))
      (prepare tree)
      (assert-equal? 1 (size tree))))

   
   (make-test-case
    "adding single character keyword"
    (let ((tree (make)))
      (string-add tree "h")
      (prepare tree)
      (assert-equal? 2 (size tree))))

   
   (make-test-case
    "adding word"
    (let ((tree (make)))
      (string-add tree "he")
      (prepare tree)
      (assert-equal? 3 (size tree))))


   (make-test-case
    "adding example from the paper
         Efficient String Matching: An Aid to Bioliographic Search"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (string-add tree "his")
      (string-add tree "hers")
      (prepare tree)
      (assert-equal? 10 (size tree))))   


   
   (make-test-case
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
        (assert-equal? 6 (size tree))
        
        (assert-equal? state0 (s:fail state1))
        (assert-equal? state0 (s:fail state2))
        (assert-equal? state0 (s:fail state3))
        (assert-equal? state1 (s:fail state4))
        (assert-equal? state2 (s:fail state5))

        (assert-equal? () (s:output state0))
        (assert-equal? () (s:output state1))
        (assert-equal? (list "he") (s:output state2))
        (assert-equal? () (s:output state3))
        (assert-equal? () (s:output state4))
        (assert-equal? (list "he" "she") (s:output state5))
        )))
        


   (make-test-case
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
        (assert-equal? 10 (size tree))

        (assert-equal? '()
                       (filter void? (list state0 state1 state2
                                           state3 state4 state5
                                           state6 state7 state8
                                           state9)))
        (for-each (lambda (s) (assert-equal? state0 (s:fail s)))
                  (list state1 state2 state3 state6 state8))
        (assert-equal? state1 (s:fail state4))
        (assert-equal? state2 (s:fail state5))
        (assert-equal? state3 (s:fail state7))
        (assert-equal? state3 (s:fail state9))

        (for-each (lambda (s) (assert-equal? () (s:output s)))
                  (list state1 state3 state4 state6 state8))
        (assert-equal? (list "he") (s:output state2))
        (assert-equal? (list "he" "she") (sort (s:output state5)))
        (assert-equal? (list "his") (s:output state7))
        (assert-equal? (list "hers") (s:output state9)))))


   (make-test-case
    "simple negative search"
    (let ((tree (make)))
      (string-add tree "hi")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hola")))
        (assert-false first-result))))

   
   
   (make-test-case
    "simple search"
    (let ((tree (make)))
      (string-add tree "hi")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hi")))
        (assert-equal? (list "hi") (result-output first-result)))))


   (make-test-case
    "simple search with two outputs"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (prepare tree)
      (let* ((first-result (start-string-search tree "shell")))
        (assert-equal? (list "he" "she") (sort (result-output first-result))))))


   (make-test-case
    "simple search with false continuation"
    (let ((tree (make)))
      (string-add tree "hi")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hihey")))
        (assert-equal? (list "hi") (result-output first-result))
        (assert-false (continue-search first-result)))))


   (make-test-case
    "another simple test with true continuation"
    (let ((tree (make)))
      (string-add tree "hi")
      (string-add tree "hiya")
      (string-add tree "ya")
      (prepare tree)
      (let* ((first-result (start-string-search tree "hiya"))
             (second-result (continue-search first-result))
             (third-result (continue-search second-result)))
        (assert-equal? (list "hi") (result-output first-result))
        (assert-equal? (list "hiya" "ya") (sort (result-output second-result)))
        (assert-false third-result))))

   
   (make-test-case
    "test empty tree usage"
    (let ((tree (make)))
      (prepare tree)
      (assert-false (start-string-search tree "zero"))))
   

   (make-test-case
    "make sure we catch unprepared usage and give a warning"
    (let ((tree (make)))
      (assert-exn exn:fail? (lambda () (start-string-search tree "antigone")))))


   (make-test-case
    "make sure we catch bad usage and give a warning"
    (let ((tree (make)))
      (prepare tree)
      (assert-exn exn:fail? (lambda () (string-add tree "antigone")))))


   
   (make-test-case
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
        (assert-false last-result)
        (assert-equal? (list "uracil") (result-output first-result))
        (assert-equal? 6 (result-end-position first-result))
        (assert-equal? (list "thymine") (result-output second-result))
        (assert-equal? 39 (result-end-position second-result)))))

   
   (make-test-case
    "test string-search"
    (let ((tree (make)))
      (string-add tree "0011")
      (string-add tree "10")
      (prepare tree)
      (assert-equal? '(("0011" 0 4)
                       ("10" 3 5)
                       ("10" 5 7))
                     (string-search tree "0011010001"))))

   (make-test-case
    "test string-search-2"
    (let ((tree (make)))
      (string-add tree "he")
      (string-add tree "she")
      (prepare tree)
      (assert-equal? '(("he" 1 3)
                       ("she" 0 3))
                     (string-search tree "she"))))


   
;;    (make-test-case
;;     "test end position"
;;     (let ((tree (make)))
;;       (string-add tree "sup")


   
   ))



(define (sort strings)
  (quicksort strings string<?))

(test/text-ui test-ahocorasick-suite)
