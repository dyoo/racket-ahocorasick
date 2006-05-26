(module ahocorasick mzscheme
  (provide make
           make-from-strings
           add
           string-add
           prepare
           size
           
           (rename AhoCorasickTree-root root)
           
           start-search
           start-string-search
           start-port-search
           
           
           result-output
           result-end-position
           continue-search
           
           string-search
           port-search
           )
  
  
  (require (lib "plt-match.ss")
           (prefix s: "state.ss")
           (prefix q: "queue.ss")
           )
  
  
  ;;; An AhoCorasickTree contains an initial root State.
  (define-struct AhoCorasickTree (root initialized?))
  
  
  
  ;; make-from-strings: (listof string) -> AhoCorasickTree
  ;; Builds a prepared tree from a given set of strings.
  (define (make-from-strings strings)
    (let ((tree (make)))
      (let loop ((strings strings))
        (when (not (null? strings))
          (add tree (string->list (car strings)) (car strings))
          (loop (cdr strings))))
      (prepare tree)
      tree))
  
  
  
  
  ;;; make: -> AhoCorasickTree
  ;;; 
  ;;; Constructs a new AhoCorasickTree with an initial zerostate state.
  ;;; Example: (define tree (make-tree))
  (define (make)
    (make-AhoCorasickTree (s:make 0) #f))
  
  
  
  ;;; add: AhoCorasickTree labels [output] -> void
  ;;; 
  ;;; Adds a sequence of labels to the tree.  If the optional output is
  ;;; given, associates a match of the labels with that output.
  ;;; Otherwise, associates the keyword itself as the output.
  ;;;
  ;;; Example:
  ;;;     (let ((tree (make-tree)))
  ;;;       (add tree (string->list "plt"))
  ;;;       (add tree (string->list "scheme") 'oh-yeah))
  (define add
    (case-lambda
      [(tree labels)
       (add tree labels labels)]
      [(tree labels output)
       (match tree
         [(struct AhoCorasickTree (root initialized?))
          ;; if we add to a prepared tree, we want to emit an error.
          (when initialized?
            (error 'add "Can't add to a prepared tree"))
          (let ((state (s:extend* root labels)))
            (s:add-output! state output))]
         [else (raise-type-error 'add-keyword "AhoCorasickTree" tree)])]))
  

  ;;; string-add: AhoCorasickTree string [output] -> 
  ;;; Specialization of ADD specifically for strings.
  (define string-add
    (case-lambda
      [(tree string)
       (add tree (string->list string) string)]
      [(tree string output)
       (add tree (string->list string) output)]))
  
  
  
  
  ;;; size: AhoCorasickTree -> number
  ;;; 
  ;;; Returns the number of states on the tree.
  (define (size tree)
    (define (size-state state)
      (+ 1
         (apply +
                (map (lambda (l) (size-state (s:goto state l)))
                     (s:out-labels state)))))
    (match tree
      [(struct AhoCorasickTree (root initialized?))
       (size-state root)]
      [else (raise-type-error 'size "AhoCorasickTree" tree)]))
  
  
  ;;; prepare: AhoCorasickTree -> void
  ;;;
  ;;; Finish up the construction of the tree    
  (define (prepare tree)
    (initialize-fail-transitions tree)
    (set-AhoCorasickTree-initialized?! tree #t))
  
  
  
  ;;; initialize-fail-transitions: AhoCorasickTree -> void
  ;;;
  ;;; Initializes the fail transitions of the tree.
  (define (initialize-fail-transitions tree)
    (define (init-unit-depth root)
      (for-each (lambda (x) (s:set-fail! x root))
                (s:out-states root)))
    (define (init-state state)
      (for-each
       (lambda (e) (init-helper state (s:goto state e) e))
       (s:out-labels state)))
    ;; r: state of depth d-1
    ;; s: state of depth s
    ;; a: label from r to s along a
    (define (init-helper r s a)         ; used by init-state
      (let loop ((state (s:fail r)))
        (if (void? (s:goto state a))
            (loop (s:fail state))
            (begin
              (s:set-fail! s (s:goto state a))
              (s:set-output! s
                             (append (s:output (s:goto state a))
                                     (s:output s)))))))
    
    (match tree
      [(struct AhoCorasickTree (root initialized?))
       (init-unit-depth root)
       (s:breadth-first init-state
                        (q:insert* q:empty (s:out-states root)))]
      [else (raise-type-error 'initialize-fail-transitions
                              "AhoCorasickTree" tree)]))  
  
  
  
  ;;; A SearchResult collects the result of doing a search
  (define-struct SearchResult (output end-position cont-f))
  
  
  
  ;;; start-search: AhoCorasickTree (-> (union label eof-object)) ->
  ;;;                  (union #f SearchResult)
  ;;;
  ;;; Starts up a search on the tree, given a source-f that emits labels
  ;;; that we follow.  If the source-f is exhaused, it should return
  ;;; eof.  If a result is found, returns a SearchResult object that we
  ;;; can query for results or to continue searching.  If a result isn't
  ;;; found, returns #f.
  (define (start-search tree source-f)
    (define (search-state state)
      (let/cc return
        (let loop ((state state)
                   (next-label (source-f))
                   (i 0))
          (when (not (null? (s:output state)))
            (set!
             return
             (let/cc restart-f
               (return (make-SearchResult (s:output state) i restart-f)))))
          (cond ((eof-object? next-label)
                 (return #f))
                ((not (void? (s:goto state next-label)))
                 (loop (s:goto state next-label) (source-f) (+ i 1)))
                (else
                 (loop (s:fail state) next-label i))))))
    (check-initialized! tree)
    (match tree
      [(struct AhoCorasickTree (root initialized?))
       (search-state root)]
      [else (raise-type-error 'search "AhoCorasickTree" tree)]))
  
  
  ;;; check-initialized!: AhoCorasickTree -> void
  ;;;
  ;;; Just do a check to make sure the tree has fail states ok.  If it
  ;;; isn't initialized, raise an error.
  (define (check-initialized! tree)
    (when (not (AhoCorasickTree-initialized? tree))
      (error 'start-search
             "Can't start a search against unprepared tree.")))
  
  
  
  ;;; start-string-search: AhoCorasickTree string -> (union #f SearchResult)
  ;;;
  ;;; Start a search, using a string as input.
  (define (start-string-search tree str)
    (let ((p (open-input-string str)))
      (start-port-search tree p)))
  
  
  ;;; start-port-search: AhoCorasickTree string -> (union #f SearchResult)
  ;;;
  ;;; Start a search, using a port as input.  
  (define (start-port-search tree port)
    (start-search tree (lambda () (read-char port))))
  
  
  ;;; continue-search: SearchResult -> (union #f SearchResult)
  ;;; 
  ;;; Continues the search where it last ended.
  (define (continue-search last-search-result)
    (let/cc return
      ((SearchResult-cont-f last-search-result) return)))
  
  
  
  
  ;;; port-search: AhoCorasickTree port -> (listof (list output
  ;;;                                                    start-index
  ;;;                                                    end-index))
  ;;;
  ;;; Returns a list of matches, using the port as the input source.
  (define (port-search tree port)
    (let loop ((s (start-port-search tree port))
               (rev-results (list)))
      (if s
          (let ((output (result-output s))
                (end-pos (result-end-position s)))
            (loop (continue-search s)
                  ;; subtle: we have to do a few list reversals to
                  ;; make sure things come out in the right order.
                  (append (map (lambda (o)
                                 (list o
                                       (- end-pos (string-length o))
                                       end-pos))
                               (reverse output))
                          rev-results)))
          (reverse rev-results))))
  
  
  
  
  ;;; string-search: AhoCorasickTree port -> (listof (list output
  ;;;                                                    start-index
  ;;;                                                    end-index))
  ;;;
  ;;; Returns a list of matches, using the string as the input source.
  (define (string-search tree string)
    (let ((port (open-input-string string)))
      (port-search tree port)))
  
  
  
  
  ;;; result-output: SearchResult -> (listof string)
  ;;; 
  ;;; Returns a list of outputs that matched at this particular SearchResult.
  (define (result-output last-search-result)
    (SearchResult-output last-search-result))
  
  
  ;;; Given a SearchResult, returns the end position where the search
  ;;; was successful.
  (define (result-end-position last-search-result)
    (SearchResult-end-position last-search-result))
  
  
  
  
  )
