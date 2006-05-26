;;; Copy-and-paste of:
;;; http://schemecookbook.org/Cookbook/FunctionalQueue
(module queue mzscheme
  (provide empty empty? insert insert* remove first)
  
  (define-struct queue (front back) (make-inspector))
  
  (define empty (make-queue '() '()))
  
;;; empty? : queue -> boolean
;;;  is the queue Q empty?
  (define (empty? Q)
    (and (null? (queue-front Q))
         (null? (queue-back Q))))
  
;;; insert : queue elm -> queue
;;; return a new queue holding both the elements
;;;  from the old queue Q and the new element x
  (define (insert Q x)  
    (make-queue (queue-front Q) 
                (cons x (queue-back Q))))

  
;;; insert*: queue (listof elm) -> queue
  (define (insert* Q elms)
    (if (null? elms)
        Q
        (insert* (insert Q (car elms)) (cdr elms))))
                           
  

;;; remove : queue -> (values elm queue)
;;;  return two values, the first a value is the front element of Q
;;;  the second a queue with the same elements as the queue Q except
;;;  for the element in front of the queue
  (define (remove Q)
    (cond
     [(and (null? (queue-front Q)) (null? (queue-back Q)))
      (error "remove: The queue is empty")]
     [(null? (queue-front Q))
      (remove (make-queue (reverse (queue-back Q)) '()))]
     [else
      (values (car (queue-front Q)) 
              (make-queue (cdr (queue-front Q)) (queue-back Q)))]))

  
;;; first : queue -> element
;;;  return the first element in the non-empty queue Q
  (define (first Q)
    (cond
     [(empty? Q)
      (error "first: The queue is empty")]
     [(null? (queue-front Q))
      (first (make-queue (reverse (queue-back Q)) '()))]
     [else
      (car (queue-front Q))]))
  )
