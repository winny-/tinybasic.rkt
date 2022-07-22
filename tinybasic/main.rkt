#lang racket/base

(require racket/cmdline
         racket/function
         racket/match
         readline/pread

         "private/eval.rkt"
         "private/parser.rkt"
         "private/types.rkt")

(provide (all-defined-out))

(define (main)
  (define *file* (make-parameter #f))
  (define *batch* (make-parameter #f))
  (command-line
   #:once-each (["-b" "--batch"]
                "Batch mode (RUN after startup, BYE when program finishes)"
                (*batch* #t))
   #:args ([filename #f])
   (*file* filename))
  (define st clean-state)
  (define (eval what)
    (match what
      [(cons _ the-line)
       (match the-line
         [(and the-line (struct line (#f stmt _)))
          (with-handlers ([exn:fail? (λ (err) (printf "Eval failed: ~a\n" (exn-message err)) st)])
            (set! st (eval-line/direct the-line st)))]
         [(and the-line (struct line (lineno stmt raw)))
          (set! st (struct-copy state st
                                [program (hash-set (state-program st)
                                                   lineno the-line)]))])]
      [idk (void)])) ; Whatever this is

  (when (*file*)
    (set! st (with-input-from-file (*file*) tb-load)))
  (cond
    [(*batch*)
     (tb-run st)]
    [else
     (when (and (terminal-port? (current-input-port)))
       (dynamic-require 'readline #f)
       (keep-duplicates 'unconsecutive))

     (current-prompt-read tb-read)
     (current-eval eval)
     (current-print (thunk* (void)))
     (read-eval-print-loop)])
  (void))

(module+ main
  (main))

