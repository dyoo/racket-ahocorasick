;; Adaptation of Shriram Krishnamurthi's "Automata via Macros"
;; to work with Aho-Corasick finite state automata.
;;
;;
;; Adaptations: the descriptions of each state now contains:
;;
;; (outputs ...)              ;; added
;; (<label> -> state)         ;; same as the paper
;; (fail -> state)            ;; added
;; (else -> state)            ;; added
;;
;; Note that outputs, fail, and else, ->, : are treated as 
;; keywords in the context of the automaton macro. (I've explicitely
;; done symbolic equality of those keywords to avoid lexical capture;
;; not sure if this is kosher since I'm still learning the macro system,
;; so this may change.)
;;
;; The automaton signature has also uglified... er... changed, 
;; to take in a few more parameters:
;;
;; automaton: stream 
;;            (outputs ->)[output-callback]
;;            (stream -> boolean)[empty?] 
;;            (stream -> element)[first]
;;            (stream -> stream)[rest]      -> 
;;
;; so that this should be usable with any sequence type that provides empty?, first and rest.


(module automaton-macro mzscheme
  (provide automaton 
           automaton-from-strings
           automaton-sexp-from-strings
           string-search)
  (require-for-syntax "ahocorasick.ss")
  (require-for-syntax "automaton.ss")
  (require-for-syntax (lib "stx.ss" "syntax"))
  (require (prefix list: (lib "list.ss")))



  ;; string-search: automaton string ->
  ;;     (listof (string[output] number[start-pos] number[end-pos]))
  (define (string-search automaton s)
    (let* ((results ())
           (n 0)
           (rest (lambda (lst)
                   (set! n (+ n 1))
                   (list:rest lst))))
      (automaton (string->list s) 
                 (lambda (outs)
                   (for-each
                    (lambda (o)
                      (set! results
                            (cons (list o (- n (string-length o)) n) results)))
                    outs))
                 list:empty? list:first rest)
      (reverse results)))

  
  
  (define-syntax (automaton-sexp-from-strings stx)
    (syntax-case stx ()
      [(automaton-sexp-from-strings strings ...)
       (let* ((strings (syntax-object->datum (syntax/loc stx (strings ...))))
              (tree (make-from-strings strings)))
         (with-syntax ((sexp
                        (stx-cdr
                         (datum->syntax-object stx (ahocorasick->sexp tree)))))
           (syntax/loc stx (quote (automaton . sexp)))))]))
  
  
  (define-syntax (automaton-from-strings stx)
    (syntax-case stx ()
      [(automaton-from-strings strings ...)
       (let* ((strings (syntax-object->datum (syntax/loc stx (strings ...))))
              (tree (make-from-strings strings)))
         (with-syntax ((sexp
                        (stx-cdr
                         (datum->syntax-object stx (ahocorasick->sexp tree)))))
           (syntax/loc stx (automaton . sexp))))]))
  
  
  
  ;; symbolic-identifier=?: stx stx -> boolean
  ;; returns true if x and y are symbolically the same.
  (define-for-syntax (symbolic-identifier=? x y)
    (and (eq? (syntax-e x)
              (syntax-e y))))
  
  
  (define-syntax (automaton stx)
    (syntax-case* stx (:) symbolic-identifier=?
      [(automaton init-state (state : response ...) ...)
       (syntax/loc stx 
         (letrec ([state
                   (process-outputs response ...)]
                  ...)
           init-state))]))
  
  
  ;; FIXME: figure out how to incorporate units with this, so we don't have to thread so much state!
  (define-syntax (process-outputs stx)
    (syntax-case* stx (outputs) symbolic-identifier=?
      [(process-outputs (outputs (outs ...)) other-transitions ...)
       (syntax/loc stx 
         (case-lambda [(stream output-callback empty? first rest from-fail?)
                       (when (not from-fail?)
                         (output-callback (list outs ...)))
                       (process-transitions stream output-callback 
                                            empty? first rest other-transitions ...)]
                      [(stream output-callback empty? first rest)
                       (process-transitions stream output-callback 
                                            empty? first rest other-transitions ...)]))]
      [(process-outputs other-transitions ...)
       (syntax/loc stx
         (case-lambda [(stream output-callback empty? first rest from-fail?)
                       (process-transitions stream output-callback 
                                            empty? first rest other-transitions ...)]
                      [(stream output-callback empty? first rest)
                       (process-transitions stream output-callback 
                                            empty? first rest other-transitions ...)]))]))
  
  
  
  (define-syntax (process-transitions stx)
    (syntax-case* stx (->) symbolic-identifier=?
      [(process-transitions stream output-callback 
                            empty? first rest (character -> state) ...)
       (syntax
        (if (empty? stream)
            #f
            (let ((peeked (first stream)))
              (process-transition-as-if-chain peeked stream output-callback
                                              empty? first rest (character ...) (state ...)))))]))
  
  
  
  ;; FIXME: clean this up.  Way too ugly for words.
  (define-syntax (process-transition-as-if-chain stx)
    (syntax-case stx ()
      
      [(process-transition-as-if-chain peeked stream output-callback 
                                       empty? first rest (label) (state))
       (cond
         ;; else case: consume, regardless of the current first stream element.
         [(symbolic-identifier=? (syntax label) (syntax else))
          (syntax/loc stx
            (state (rest stream) output-callback empty? first rest #f))]
         ;; fail case: don't consume the stream, but follow the transition.
         [(symbolic-identifier=? (syntax label) (syntax fail))
          (syntax/loc stx
            (state stream output-callback empty? first rest #t))])]
      
      ;; regular case: do the comparison, and either go along the goto or fail transitions
      ;; appropriately.
      [(process-transition-as-if-chain peeked stream output-callback 
                                       empty? first rest 
                                       (label rest-labels ...) (state rest-states ...))
       (syntax/loc stx
         (if (equal? label peeked)
             (state (rest stream) output-callback empty? first rest #f)
             (process-transition-as-if-chain peeked stream output-callback empty? first rest 
                                             (rest-labels ...) (rest-states ...))))]))
  
  )
