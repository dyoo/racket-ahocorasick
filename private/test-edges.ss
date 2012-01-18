#lang racket/base
(require "edges.ss")
(require rackunit)
(require rackunit/text-ui)

;;; sort routine for list of characters
(define (char-sort list-of-chars)
  (sort list-of-chars char<?))


(define (test-edges-suite n)
  (test-suite
   (string-append
    "Tests the implementation of edges, using a sample edge of depth "
    (number->string n))

   
   (test-case
    (string-append "Simple lookup " (number->string n))
    (let ((edges (make n)))
      (put! edges #\a 'foo)
      (check-equal? 'foo (get edges #\a))))



   (test-case
    (string-append "Another simple lookup " (number->string n))
    (let ((edges (make n)))
      (put! edges #\f 'foo)
      (put! edges #\b 'bar)
      (check-equal? 'foo (get edges #\f))
      (check-equal? 'bar (get edges #\b))
      (check-false (get edges #\a))))
      

   
   (test-case
    (string-append "Simple keys " (number->string n))
    (let ((edges (make n)))
      (put! edges #\a 'foo)
      (check-equal? (list #\a) (labels edges))))

   
   (test-case
    (string-append "Many keys " (number->string n))
    (let ((edges (make n)))
      (for-each (lambda (ch) (put! edges ch 'foo)) (string->list "aeiou"))
      (check-equal? (char-sort (string->list "aeiou")) (char-sort (labels edges)))))

   
   ))



(run-tests
 (test-suite "both"
             (test-edges-suite 0)
             (test-edges-suite 50)))
                    
