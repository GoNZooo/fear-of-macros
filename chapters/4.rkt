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

(hyphenize izno goud () #t)
(izno-goud)

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

(slashify good shit 5)
good/shit

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

(slashify/with-with-syntax-and-format-id ultimate form 42)
ultimate/form

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

(hyphenize* (bond james bond) (agent-id org-id)
            (if (and (equal? agent-id "007") (equal? org-id "mi5"))
                "Correct!"
                "Nein!"))
(bond-james-bond "006" "mi5")
(bond-james-bond "007" "mi5")

(define this-cool-thing "")
(hyphenize* (get this-cool-thing) () this-cool-thing)
(hyphenize* (set this-cool-thing) (cool-value) (set! this-cool-thing cool-value))
(set-this-cool-thing "Yay!")
(get-this-cool-thing)

(define-syntax (create-get/set-hyphenize stx)
  (syntax-case stx ()
    [(_ component-name)
     (datum->syntax
      stx
      `(begin
         (define component-name "")
         ,(hyphenize* '(get component-name) ()
                      'component-name)
         ,(hyphenize* '(set component-name) ('component-value)
                      (set! 'component-name 'component-value))))]))

(create-get/set-hyphenize balls)
balls
(get-balls)