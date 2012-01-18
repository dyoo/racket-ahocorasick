#lang racket/base
(require "ahocorasick.ss"
         profile)


(define (work)
  (let ((tree (make)))
    (with-input-from-file
        "/usr/share/dict/words"
      (lambda ()
        (let loop ((line (read-line))
                   (i 0))
          (if (eof-object? line)
              (begin 
                (printf "Read ~s words\n" i)
                'done)
              (begin
                (add tree (string->list line) 1)
                (loop (read-line) (+ i 1)))))
        (prepare tree)))))

(profile-thunk work
               #:repeat 5)