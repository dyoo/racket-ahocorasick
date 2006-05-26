(require (lib "errortrace.ss" "errortrace"))
(profiling-enabled #t)

;; 
;;(profile-paths-enabled #t) ;; this appears majorly broken in
;;                           mzscheme 299 (See bug 6886 in bugs.plt-scheme.org.  Ugh.

(require "ahocorasick.ss")
(let ((tree (make)))
  (with-input-from-file
      "/usr/share/dict/words"
    (lambda ()
      (let loop ((line (read-line))
                 (i 0))
        (if (or (eof-object? line) (> i 5000))
            'done
            (begin
              (add tree (string->list line) 1)
              (loop (read-line) (+ i 1)))))
      (prepare tree)
      (output-profile-results #t #t)
      )))
