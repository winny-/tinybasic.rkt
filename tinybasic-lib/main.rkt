#lang racket/base

(require racket/cmdline
         racket/function
         racket/match
         racket/list
         racket/port
         readline/pread

         "private/eval.rkt"
         "private/parser.rkt"
         "private/types.rkt")

(provide (all-defined-out))

(define (main)
  (define *file* (make-parameter #f))
  (define *run* (make-parameter #f))
  (define *input-string* (make-parameter #f))
  (define *keep-open* (make-parameter #f))
  (command-line
   #:once-each
   (["-r" "--run"]
    "Run the program and exit upon completion"
    (*run* #t))
   (["-k" "--keep-open"]
    "When -r (--run) is specified, return to REPL after the program runs to  completion."
    (*keep-open* #t))
   (["-i" "--input" "--inputs"]
    s
    "Set data for INPUT to consume."
    (*input-string* s))
   #:args ([filename #f])
   (*file* filename))
  (define base-state (initialize #:load-file (*file*) ; Could be #f
                                 #:inputs (match (*input-string*)
                                            [#f empty]
                                            [(? string? s) (list s)])))
  (define maybe-ran-state (if (*run*)
                              (tb-run base-state)
                              base-state))
  (when (or (not (*run*)) (*keep-open*))
    (start-repl maybe-ran-state))
  (void))

(module+ main
  (main))

