#|
Evaluate tinybasic code.
|#
#lang racket/base

(require racket/bool
         racket/contract/base
         racket/contract/region
         racket/function
         racket/list
         racket/match

         "parser.rkt"
         "types.rkt")

(provide (all-defined-out))

(define (eval-expr expr the-state)
  (define (eval-unsigned unsigned)
    (match unsigned
      [(struct unsignedexpr:unary (term)) (eval-term term)]
      [(struct unsignedexpr:add (a b)) (+ (eval-term a) (eval-unsigned b))]
      [(struct unsignedexpr:sub (a b)) (+ (eval-term a) (eval-unsigned b))]))
  (define (eval-term term)
    (match term
      [(struct term:unary (factor)) (eval-factor factor)]
      [(struct term:mul (a b)) (* (eval-term a) (eval-factor b))]
      [(struct term:div (a b)) (quotient (eval-term a) (eval-factor b))]))
  (define (eval-factor factor)
    (match (factor-value factor)
      [(and fex (struct* expression ())) (eval-expr fex the-state)]
      [(? number? n) n]
      [(? string? var) (hash-ref (state-vars the-state) var)]
      [(struct function:rnd (expr)) (random (eval-expr expr the-state))]))
  (match-define (struct expression (sign unsignedexpr)) expr)
  ((if (and sign (symbol=? sign -)) - +) ; Repeat after me, do not eval the sign.
   (eval-unsigned unsignedexpr)))

(define (eval-line the-line the-state)
  (define (e expr)
    (eval-expr expr the-state))
  (match (parse-line the-line)
    [(struct statement:if (relation left right body))
     (define operator (match relation
                        ['= =]
                        ['!= (compose1 not =)]
                        ['< <]
                        ['> >]
                        ['>= >=]
                        ['<= <=]))
     (if (operator (e left) (e right))
         (eval-line (struct-copy line the-line [text body]) the-state)
         (struct-copy state the-state [lineno (next-line-in-listing the-state)]))]

    [(struct statement:end ())
     (struct-copy state the-state [lineno #f])]
    [(struct statement:gosub (expr))
     (struct-copy state the-state [lineno (e expr)] [gosubs (cons (next-line-in-listing the-state)
                                                                  (state-gosubs the-state))])]
    [(struct statement:goto (expr))
     (struct-copy state the-state [lineno (e expr)])]
    [(struct statement:run ((list _ ..1)))
     (raise-user-error 'eval-line "Cannot use RUN,exprlist from a program")]
    [(struct statement:run ((list)))
     (struct-copy state the-state [lineno (first-line the-state)])]
    [(struct statement:return ())
     (match (state-gosubs the-state)
       [(or (list) (list #f _ ...)) (raise-user-error 'eval-line "RETURN with invalid return address!")]
       [(list no xs ...) (struct-copy state the-state [lineno no] [gosubs xs])])]
    [(struct statement:load (filename))
     (with-input-from-file filename (tb-load))]
    [(struct statement:bye ()) (exit 0)]
    [stmt
     (define st (match stmt
                  [(struct statement:rem (_)) the-state]
                  [(struct statement:clear ()) clean-state]
                  [(struct statement:list ((list)))
                   (tb-write (state-program the-state))
                   the-state]
                  [(struct statement:list ((list expr)))
                   (tb-write (state-program the-state) #:what (e expr))
                   the-state]
                  [(struct statement:list ((list a b)))
                   (tb-write (state-program the-state) #:what (list (e a) (e b)))
                   the-state]
                  [(struct statement:print (printlist))
                   (let loop ([ls printlist])
                     (match ls
                       [(list 'colon) (void)]
                       [(list (? string? s) xs ...) (display s) (loop xs)]
                       [(list (and expr (struct* expression ())) xs ...)
                        (display (e expr)) (loop xs)]
                       [(list 'semicolon a xs ...) (loop (cons a xs))]
                       [(list 'comma a xs ...) (display #\tab) (loop (cons a xs))]
                       [(list) (newline)]
                       [(list 'semicolon) (void)]
                       [(list 'comma) (display #\tab)]))
                   the-state]
                  [(struct statement:list ((list expr)))
                   (tb-write (state-program the-state))]
                  [(struct statement:let (var expr))
                   (struct-copy state the-state [vars (hash-set (state-vars the-state) var (e expr))])]
                  [(struct statement:empty ())
                   the-state]
                  [(struct statement:save (filename))
                   (with-output-to-file filename
                     (thunk (tb-write (state-program the-state)))
                     #:mode 'text
                     #:exists 'truncate)
                   the-state]))
     (struct-copy state st [lineno (next-line-in-listing st)])]))

(define (eval-line/direct the-line the-state)
  (define next-state
    (match (parse-line the-line)
      [(struct* statement:input ()) (displayln "TODO interactive INPUT stmt")
                                    the-state]
      [(struct statement:run ((list xs ..1)))
       ;; Save the inputs then rerun the line as a bare RUN statement.
       (eval-line/direct (struct-copy line the-line [text "RUN"])
                         (struct-copy state the-state [inputs (append (state-inputs the-state) xs)]))]
      [_ (eval-line the-line the-state)]))
  (let loop ([st next-state])
    (cond
      [(batch? st)
       (loop (eval-line (hash-ref (state-program st) (state-lineno st)) st))]
      [else st])))

(define/contract (tb-run st [inputs empty])
  (->* (state?) ((listof any/c)) state?)
  (eval-line/direct (line #f "RUN" "RUN")
                    (struct-copy state st [inputs (append (state-inputs st) inputs)])))

(define (batch? the-state)
  (and (state-lineno the-state) #t))

(define (first-line the-state)
  (for/first ([no (sort (hash-keys (state-program the-state)) <)])
    no))

(define (next-line-in-listing the-state)
  (define last-line (state-lineno the-state))
  (and (batch? the-state)
       (for/first ([no (sort (hash-keys (state-program the-state)) <)]
                   #:when (> no last-line))
         no)))
