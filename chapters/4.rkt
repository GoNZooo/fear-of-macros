#lang racket

(require (for-syntax racket/syntax
                     racket/string))

(define-syntax (hyphenize stx)
  (syntax-case stx ()
    [(_ prefix suffix (arguments ...) shoulders body ...)
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'prefix)
                                                         (syntax->datum #'suffix))))
         ()
       [func-name
        #'(define (func-name arguments ...)
            shoulders body ...)])]))
(module+ main
  (hyphenize izno goud () #t)
  (izno-goud))

(define-syntax (slashify stx)
  ;;; First syntax-case matches general function body
  (syntax-case stx ()
    [(_ first second value)
     ;;; We compose a new syntax object out of the ones
     ;;; we have and examine that with a new syntax-case
     ;;; and return proper syntax up to the top.
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a/~a"
                                                         (syntax->datum #'first)
                                                         (syntax->datum #'second))))
         ()
       [var-name
        #'(define var-name value)])]))
(module+ main
  (slashify good shit 5)
  good/shit)

(define-syntax (slashify/with-with-syntax stx)
  (syntax-case stx ()
    [(_ first second value)
     (with-syntax ([var-name (datum->syntax stx
                              (string->symbol (format "~a/~a"
                                                      (syntax->datum #'first)
                                                      (syntax->datum #'second))))])
       #'(define var-name value))]))
(slashify/with-with-syntax awesome stuff 1337)
awesome/stuff

(define-syntax (slashify/with-with-syntax-and-format-id stx)
  (syntax-case stx ()
    [(_ first second value)
     (with-syntax ([var-name (format-id stx "~a/~a" #'first #'second)])
       #'(define var-name value))]))
(module+ main
  (slashify/with-with-syntax-and-format-id ultimate form 42)
  ultimate/form)

(define-syntax (hyphenize* stx)
  (syntax-case stx ()
    [(_ (hyphened-names ...) (func-args ...)
        shoulder
        body ...)
     (let* ([name-symbols (map syntax-e (syntax->list #'(hyphened-names ...)))]
            [name-strings (map symbol->string name-symbols)]
            [final-string (string-join name-strings "-")]
            [final-symbol (string->symbol final-string)])
       (with-syntax ([hyphen-name (datum->syntax stx final-symbol)])
         #'(define (hyphen-name func-args ...)
             shoulder
             body ...)))]))

(module+ main
  (hyphenize* (bond james bond) (agent-id org-id)
              (if (and (equal? agent-id "007") (equal? org-id "mi5"))
                  "Correct!"
                  "Nein!"))
  (bond-james-bond "006" "mi5")
  (bond-james-bond "007" "mi5"))

(define-syntax (create-get/set-hyphenize stx)
  (syntax-case stx ()
    [(_ component-name)
     #'(begin
         (define component-name "")
         (hyphenize* (get component-name) ()
                     component-name)
         (hyphenize* (set component-name) (component-value)
                     (set! component-name component-value)))]))

(module+ main
  (create-get/set-hyphenize hyphen-get-set)
  hyphen-get-set
  (set-hyphen-get-set 5)
  (get-hyphen-get-set))

(define-syntax (create-get/set-funcs stx)
   (syntax-case stx ()
     [(_ component-name)
      (with-syntax ([set-name (format-id stx "set-~a" #'component-name)]
                    [get-name (format-id stx "get-~a" #'component-name)])
        #'(begin
            (define component-name "")
            (define (set-name component-parameter)
              (set! component-name component-parameter))
            (define (get-name)
              component-name)))]))

(module+ main
  (create-get/set-funcs bitchplz)
  bitchplz
  (get-bitchplz)
  (set-bitchplz "Whatever, really.")
  (get-bitchplz))