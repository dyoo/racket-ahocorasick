#lang racket/base
(require "state.ss")
(require (only-in rackunit test-suite test-case check-false check-equal?))
(require rackunit/text-ui)


(define test-state-suite
  (test-suite
   "state test suite"

   (test-case
    "test initial state has goto to everything"
    (let ((initial-state (make 0)))
      (check-false (void? (goto initial-state #\b)))
      (check-false (void? (goto initial-state #\o)))
      (check-false (void? (goto initial-state #\r)))
      (check-false (void? (goto initial-state #\e)))))

   
   (test-case
    "test single extend"
    (let ((n (make 0)))
      (check-equal? (list) (out-labels n))
      (extend n #\x)
      (check-equal? (list #\x) (out-labels n))
      (check-false (void? (goto n #\x)))))

   (test-case
    "test single extend*"
    (let ((n (make 0)))
      (check-equal? (list) (out-labels n))
      (extend* n (list #\x))
      (check-equal? (list #\x) (out-labels n))
      (check-false (void? (goto n #\x)))))

   
   (test-case
    "test extend* with longer list"
    (let ((n (make 0)))
      (check-equal? (list) (out-labels n))
      (extend* n (string->list "hola"))
      (check-equal? (list #\h) (out-labels n))
      (check-equal? (list #\o) (out-labels (goto n #\h)))
      (check-equal? (list #\l) (out-labels (goto (goto n #\h) #\o)))
      (check-equal? (list #\a) (out-labels (goto (goto (goto n #\h) #\o) #\l)))
      (check-equal? (list) (out-labels (goto (goto (goto (goto n #\h) #\o) #\l) #\a)))))


   
   
   ))
      

(run-tests test-state-suite)
                  
                    
