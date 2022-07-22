#lang racket/base

(require syntax/strip-context
         "../private/eval.rkt"
         "../private/parser.rkt")

(provide (rename-out [tb-read-syntax read-syntax]))

(define (tb-read-syntax src in)
  (with-syntax ([initial-state (tb-load in)])
    (strip-context
     #'(module tinybasic-runner racket
         (require tinybasic/private/eval)
         (void (tb-run 'initial-state))))))
