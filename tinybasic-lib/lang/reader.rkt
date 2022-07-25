#lang racket/base

(require syntax/strip-context
         "../private/eval.rkt"
         "../private/parser.rkt")

(provide (rename-out [tb-read-syntax read-syntax]))

(define (tb-read-syntax src in)
  (with-syntax ([initial-state (tb-load in)])
    (strip-context
     #'(module tinybasic-runner racket/base
         (define STATE 'initial-state)
         (provide STATE)
         (module+ main
           (require racket/cmdline
                    racket/list
                    racket/port

                    tinybasic/private/parser
                    tinybasic/private/eval
                    tinybasic/private/types)
           (define *list-of-inputs* (make-parameter empty))
           (define *repl* (make-parameter #f))
           (void
            (command-line
             #:once-each
             (["--start-repl"]
              "Load the program and launch the REPL.  Use `RUN' to execute the loaded program."
              (*repl* #t))
             #:args inputs
             (*list-of-inputs* inputs)))
           (define init-state (initialize #:state STATE
                                          #:inputs (*list-of-inputs*)))
           (void (cond [(*repl*)
                        (start-repl init-state)]
                       [else
                        (tb-run init-state)])))))))
