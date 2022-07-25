#lang racket/base

(require racket/bool
         racket/contract/base
         racket/contract/region
         racket/function
         racket/list
         racket/match
         racket/port
         (for-syntax racket/base)

         readline/pread

         "parser.rkt"
         "types.rkt")

(provide (all-defined-out))

(define/contract (eval-expr expr the-state)
  (expression? state? . -> . exact-integer?) ; XXX This should be a more
                                             ; specific numeric type.
  (define (eval-unsigned unsigned)
    (match unsigned
      [(struct unsignedexpr:unary (term)) (eval-term term)]
      [(struct unsignedexpr:add (a b)) (+ (eval-term a) (eval-unsigned b))]
      [(struct unsignedexpr:sub (a b)) (- (eval-term a) (eval-unsigned b))]))
  (define (eval-term term)
    (match term
      [(struct term:unary (factor)) (eval-factor factor)]
      [(struct term:mul (a b)) (* (eval-term a) (eval-factor b))]
      [(struct term:div (a b)) (quotient (eval-term a) (eval-factor b))]))
  (define (eval-factor factor)
    (match (factor-value factor)
      [(and fex (struct* expression ())) (eval-expr fex the-state)]
      [(? number? n) n]
      [(? string? var) (get-var the-state var)]
      [(struct function:rnd (expr)) (random (eval-expr expr the-state))]))
  (match-define (struct expression (sign unsignedexpr)) expr)
  ((if (and sign (symbol=? sign '-)) - +) ; Repeat after me, do not eval the sign.
   (eval-unsigned unsignedexpr)))

(define/contract (eval-line the-line the-state)
  (line? state? . -> . state?)
  (define (e expr)
    (eval-expr expr the-state))
  (match (parse-line the-line)
    [(struct statement:if (relation left right body))
     (define operator (match relation
                        ['= =]
                        ['!= (negate =)]
                        ['< <]
                        ['> >]
                        ['>= >=]
                        ['<= <=]))
     (if (operator (e left) (e right))
         (eval-line (struct-copy line the-line [text body]) the-state)
         (goto next the-state))]
    [(struct statement:clear ()) clean-state]
    [(struct statement:end ())
     (goto #f the-state)]
    [(struct statement:gosub (expr))
     (goto
      (e expr)
      (struct-copy state the-state
                   [gosubs (cons (next-line-in-listing the-state)
                                 (state-gosubs the-state))]))]
    [(struct statement:goto (expr))
     (goto (e expr) the-state)]
    [(struct statement:run ((list _ ..1)))
     (raise-user-error 'eval-line "Cannot use RUN,exprlist from a program")]
    [(struct statement:run ((list)))
     (goto start the-state)]
    [(struct statement:return ())
     (match (state-gosubs the-state)
       [(or (list) (list #f _ ...)) (raise-user-error 'eval-line "RETURN with invalid return address!")]
       [(list no xs ...) (goto no (struct-copy state the-state [gosubs xs]))])]
    [(struct statement:load (filename))
     (with-input-from-file filename (tb-load))]
    [(struct statement:let (var expr))
     (goto next (set-var the-state var (e expr)))]
    [(struct statement:input ((list exprs ...)))
     (goto next
           (for/fold ([st the-state])
                     ([expr exprs])
             (match expr
               [(expression>>var var)
                (set-var-from-input st var)]
               [_ (raise-user-error 'eval-line "Bad inputs element ~v in state object" expr)])))]
    [(struct statement:bye ()) (exit 0)]
    ;; And statements that do not modify program state.
    [stmt
     (match stmt
       [(or (struct* statement:rem ())
            (struct* statement:empty ()))
        (void)]
       [(struct statement:list ((list)))
        (tb-write (state-program the-state))]
       [(struct statement:list ((list expr)))
        (tb-write (state-program the-state)
                  #:what (e expr)) the-state]
       [(struct statement:list ((list a b)))
        (tb-write (state-program the-state)
                  #:what (list (e a) (e b)))]
       [(struct statement:print (printlist))
        (let loop ([ls printlist])
          (match ls
            [(list 'colon) (void)]
            [(list (? string? s) xs ...)
             (display s) (loop xs)]
            [(list (and expr (struct* expression ())) xs ...)
             (display (e expr)) (loop xs)]
            [(list 'semicolon xs ..1) (loop xs)]
            [(list 'comma xs ..1)
             (display #\tab) (loop xs)]
            [(list) (newline)]
            [(list 'semicolon) (void)]
            [(list 'comma)
             (display #\tab)]))]
       [(struct statement:save (filename))
        (with-output-to-file filename
          (thunk (tb-write (state-program the-state)))
          #:mode 'text
          #:exists 'truncate)])
     (goto next the-state)]))

(define/contract (set-var-from-input the-state var)
  (state? var? . -> . state?)
  (set! var (canonicalize-var var))
  (match the-state
    [(struct* state ([inputs (list a as ...)]))
     (define evaluated (match a
                         [(struct* expression ()) (eval-expr a the-state)]))
     (struct-copy state
                  (set-var the-state var evaluated)
                  [inputs as])]
    [_
     (let loop ()
       (match (tb-read-input)
         [(list) ; User pressed return, so prompt again.
          (displayln "oops got no input")
          (loop)]
         [(list a as ...)
          (struct-copy state
                       (set-var the-state var (eval-expr a the-state))
                       [inputs as])]))])) ; inputs was empty previously because
                                          ; the other pattern matches non-empty

(define/contract (eval-line/direct the-line the-state)
  (line? state? . -> . state?)
  (define next-state
    (match (parse-line the-line)
      [(struct statement:input (args)) ; Direct mode INPUT
       (let loop ([st the-state] [args args])
         (match args
           [(list (expression>>var var)
                  args-body ...)
            (match args-body
              [(list (and expr (struct* expression ())) arg-rest ...)
               (loop (set-var st var (eval-expr expr st))
                     args-body)]
              [(list)
               (set-var-from-input st var)])]))
       the-state]
      [(struct statement:run ((list xs ..1))) ; Direct mode RUN,a,b,c,d
       (tb-run the-state xs)]
      [_ (eval-line the-line the-state)])) ; Everything else behaves the same.
  (let loop ([st next-state])
    (cond
      [(batch? st)
       (loop (eval-line (hash-ref (state-program st) (state-lineno st)) st))]
      [else st])))

(define/contract (tb-run st [inputs empty])
  (->* (state?) ((listof any/c)) state?)
  (eval-line/direct (line #f "RUN" "RUN")
                    (struct-copy state st [inputs (append (state-inputs st) inputs)])))

(define/contract (batch? the-state)
  (state? . -> . boolean?)
  (and (state-lineno the-state) #t))

(define/contract (first-line the-state)
  (state? . -> . (or/c #f exact-positive-integer?))
  (for/first ([no (sort (hash-keys (state-program the-state)) <)])
    no))

(define/contract (next-line-in-listing the-state)
  (state? . -> . (or/c #f exact-positive-integer?))
  (define last-line (state-lineno the-state))
  (and (batch? the-state)
       (for/first ([no (sort (hash-keys (state-program the-state)) <)]
                   #:when (> no last-line))
         no)))

(define-syntax goto
  (syntax-rules (start next)
    [(_ start st)
     ;; Use a binding to prevent double-evaluation of the expression "st".
     (let ([st2 st])
       (goto (first-line st2) st2))]
    [(_ next st)
     ;; Use a binding to prevent double-evaluation of the expression "st".
     (let ([st2 st])
       (goto (next-line-in-listing st2) st2))]
    [(_ no st) (struct-copy state st [lineno no])]))


(define/contract (initialize #:inputs [inputs empty]
                             #:load-file [load-file #f]
                             #:state [init-state clean-state])
  (() (#:inputs (listof (or/c expression? string?))
       #:load-file (or/c #f path-string? path?)
       #:state state?)
      . ->* . state?)
  (when (and (terminal-port? (current-input-port)))
    (dynamic-require 'readline #f)
    (keep-duplicates 'unconsecutive))
  (define base-state (if load-file
                         (with-input-from-file load-file (thunk (tb-load #:state init-state)))
                         init-state))
  (define normalized-inputs
    (let loop ([acc empty]
               [inputs inputs])
      (match inputs
        [(list) (reverse acc)]
        [(list (? string? s) xs ...)
         (loop acc (append (with-input-from-string s tb-read-input) xs))]
        [(list (and expr (struct* expression ())) xs ...)
         (loop (cons expr acc) xs)])))
  (struct-copy state base-state [inputs normalized-inputs]))

(define/contract (start-repl [the-state clean-state])
  (() (state?) . ->* . void?)
  (define *state* (make-parameter the-state))
  (define (my-eval what)
    (match what
      [(cons _ the-line)
       (match the-line
         [(struct line (#f stmt _))
          (with-handlers ([exn:fail? (λ (err) (printf "Eval failed: ~a\n" (exn-message err)) (*state*))])
            (*state* (eval-line/direct the-line (*state*))))]
         [(struct line (lineno stmt raw))
          (*state* (struct-copy state (*state*)
                                [program (hash-set (state-program (*state*))
                                                   lineno the-line)]))])]
      [idk (void)]))
  (parameterize ([current-prompt-read tb-read]
                 [current-eval my-eval]
                 [current-print (thunk* (void))])
    (read-eval-print-loop))
  (void))
