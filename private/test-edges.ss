(require "edges.ss")
(require (planet "test.ss" ("schematics" "schemeunit.plt" 1)))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))
(require (lib "list.ss"))


;;; sort routine for list of characters
(define (sort list-of-chars)
  (quicksort list-of-chars char<?))


(define (test-edges-suite n)
  (make-test-suite
   (string-append
    "Tests the implementation of edges, using a sample edge of depth "
    (number->string n))

   
   (make-test-case
    (string-append "Simple lookup " (number->string n))
    (let ((edges (make n)))
      (put! edges #\a 'foo)
      (assert-equal? 'foo (get edges #\a))))



   (make-test-case
    (string-append "Another simple lookup " (number->string n))
    (let ((edges (make n)))
      (put! edges #\f 'foo)
      (put! edges #\b 'bar)
      (assert-equal? 'foo (get edges #\f))
      (assert-equal? 'bar (get edges #\b))
      (assert-false (get edges #\a))))
      

   
   (make-test-case
    (string-append "Simple keys " (number->string n))
    (let ((edges (make n)))
      (put! edges #\a 'foo)
      (assert-equal? (list #\a) (labels edges))))

   
   (make-test-case
    (string-append "Many keys " (number->string n))
    (let ((edges (make n)))
      (for-each (lambda (ch) (put! edges ch 'foo)) (string->list "aeiou"))
      (assert-equal? (sort (string->list "aeiou")) (sort (labels edges)))))

   
   ))



(test/text-ui
 (make-test-suite "both"
                  (test-edges-suite 0)
                  (test-edges-suite 50)))
                    
