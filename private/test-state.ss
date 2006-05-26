(require "state.ss")
(require (planet "test.ss" ("schematics" "schemeunit.plt" 1)))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))


(define test-state-suite
  (make-test-suite
   "state test suite"

   (make-test-case
    "test initial state has goto to everything"
    (let ((initial-state (make 0)))
      (assert-false (void? (goto initial-state #\b)))
      (assert-false (void? (goto initial-state #\o)))
      (assert-false (void? (goto initial-state #\r)))
      (assert-false (void? (goto initial-state #\e)))))

   
   (make-test-case
    "test single extend"
    (let ((n (make 0)))
      (assert-equal? (list) (out-labels n))
      (extend n #\x)
      (assert-equal? (list #\x) (out-labels n))
      (assert-false (void? (goto n #\x)))))

   (make-test-case
    "test single extend*"
    (let ((n (make 0)))
      (assert-equal? (list) (out-labels n))
      (extend* n (list #\x))
      (assert-equal? (list #\x) (out-labels n))
      (assert-false (void? (goto n #\x)))))

   
   (make-test-case
    "test extend* with longer list"
    (let ((n (make 0)))
      (assert-equal? (list) (out-labels n))
      (extend* n (string->list "hola"))
      (assert-equal? (list #\h) (out-labels n))
      (assert-equal? (list #\o) (out-labels (goto n #\h)))
      (assert-equal? (list #\l) (out-labels (goto (goto n #\h) #\o)))
      (assert-equal? (list #\a) (out-labels (goto (goto (goto n #\h) #\o) #\l)))
      (assert-equal? (list) (out-labels (goto (goto (goto (goto n #\h) #\o) #\l) #\a)))))


   
   
   ))
      

(test/text-ui test-state-suite)
                  
                    
