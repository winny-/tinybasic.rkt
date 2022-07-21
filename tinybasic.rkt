#|

TinyBASIC implementation. Great language to practice with Racket's
lex/yacc clone.

More information:

http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.htm
https://en.wikipedia.org/wiki/Tiny_BASIC

|#

#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         readline/pread)

(require racket/pretty)

(define-lex-trans :ci
  (λ (stx)
    (syntax-case stx ()
      [(_ s)
       (let ([alternations
                      (datum->syntax
                       stx
                       (for/list ([c (in-string (syntax->datum #'s))])
                         `(:+ ,(char-upcase c) ,(char-downcase c))))])
         #`(:: #,@alternations))])))

(define-lex-abbrevs
  (tstring (:: #\" (:* (:~ #\newline #\return #\")) #\"))
  (tdigit (char-range #\0 #\9))
  (tnumber (:+ numeric))
  (trelop (:or #\< #\> #\= "<=" ">=" "<>" "><"))
  (tvar (:or (char-range #\A #\Z) (char-range #\a #\z)))
  (twhitespace (:+ (:or #\tab #\space)))
  (tcr (:or "\r\n" #\newline)))

(define-tokens complex-tokens
  (NUMBER STRING VAR REM))

(define-empty-tokens simple-tokens
  (PRINT IF GOTO INPUT LET GOSUB RETURN CLEAR LIST RUN END EQ LT GT LTE GTE NE ADD SUB DIV MUL COMMA SPACE CLOSE-PAREN OPEN-PAREN RND EOF SEMICOLON COLON THEN BYE))

(define tb-lexer
  (lexer
   [(eof) (token-EOF)]
   [(:: (:ci "REM") (:* (:~ #\newline #\return)))
    (token-REM lexeme)]
   [tvar (token-VAR (string-upcase lexeme))]
   [tnumber (token-NUMBER (string->number lexeme))]
   [tstring (token-STRING (string-replace lexeme "\"" ""))]
   [(:or (:ci "PR") (:ci "PRINT")) (token-PRINT)]
   [(:ci "IF") (token-IF)]
   [(:: (:ci "GO") (:? twhitespace) (:ci "TO")) (token-GOTO)]
   [(:ci "INPUT") (token-INPUT)]
   [(:ci "LET") (token-LET)]
   [(:: (:ci "GO") (:? twhitespace) (:ci "SUB"))  (token-GOSUB)]
   [(:ci "RETURN") (token-RETURN)]
   [(:ci "CLEAR") (token-CLEAR)]
   [(:ci "LIST") (token-LIST)]
   [(:ci "RUN") (token-RUN)]
   [(:ci "END") (token-END)]
   [(:ci "RND") (token-RND)]
   [(:ci "THEN") (token-THEN)]
   [(:ci "BYE") (token-BYE)]
   ["=" (token-EQ)]
   ["<" (token-LT)]
   [">" (token-GT)]
   ["<=" (token-LTE)]
   [">=" (token-GTE)]
   [(:or "<>" "><") (token-NE)]
   ["+" (token-ADD)]
   ["-" (token-SUB)]
   ["/" (token-DIV)]
   ["*" (token-MUL)]
   ["," (token-COMMA)]
   [";" (token-SEMICOLON)]
   [":" (token-COLON)]
   ["(" (token-OPEN-PAREN)]
   [")" (token-CLOSE-PAREN)]
   [twhitespace (tb-lexer input-port)]))

(define (tb-lex/list ip)
  (define (gen) (tb-lexer ip))
  (let loop ([acc empty])
    (match (gen)
      [(? (curry equal? (token-EOF)) e) (reverse (cons e acc))]
      [other (loop (cons other acc))])))

(struct line (number text entire) #:transparent)

(struct statement () #:transparent)
(struct statement:empty statement () #:transparent)
(struct statement:print statement (printlist) #:transparent)
(struct statement:input statement (varlist) #:transparent)
(struct statement:let statement (var expression) #:transparent)
(struct statement:goto statement (expression) #:transparent)
(struct statement:gosub statement (expression) #:transparent)
(struct statement:return statement () #:transparent)
(struct statement:if statement (relation left right body) #:transparent)
(struct statement:rem statement (text) #:transparent)
(struct statement:clear statement () #:transparent)
(struct statement:run statement (exprlist) #:transparent)
(struct statement:list statement (exprlist) #:transparent)
(struct statement:end statement () #:transparent)
(struct statement:bye statement () #:transparent)
(struct function () #:transparent)
(struct function:rnd function (expr) #:transparent)

(struct expression (sign unsignedexpr) #:transparent)
(struct unsignedexpr () #:transparent)
(struct unsignedexpr:unary unsignedexpr (term) #:transparent)
(struct unsignedexpr:add unsignedexpr (a b) #:transparent)
(struct unsignedexpr:sub unsignedexpr (a b) #:transparent)
(struct term () #:transparent)
(struct term:unary term (factor) #:transparent)
(struct term:mul term (a b) #:transparent)
(struct term:div term (a b) #:transparent)
(struct factor (value) #:transparent)

(define (string->line s)
  (match s
    [(regexp #px"([0-9 ]*)(.*)" (list entire num stmt))
     (line (string->number (string-replace num " " "")) stmt entire)]
    [_ #f]))

(define statement-parser
  (parser
   (tokens complex-tokens simple-tokens)
   (start statement)
   (end EOF)
   (precs
    (left ADD SUB)
    (left MUL DIV))
   (error (λ (tok-ok? tok-name tok-value)
            (raise-syntax-error 'tb-parser (format "~a ~a ~a" tok-ok? tok-name tok-value))))
   (grammar
    (statement [(PRINT printlist) (statement:print $2)]
               [(INPUT varlist) (statement:input $2)]
               [(LET var EQ expression) (statement:let $2 $4)]
               [(var EQ expression) (statement:let $1 $3)]
               [(GOTO expression) (statement:goto $2)]
               [(GOSUB expression) (statement:gosub $2)]
               [(RETURN) (statement:return)]
               [(IF expression relop expression THEN statement)
                (statement:if $3 $2 $4 $6)]
               [(IF expression relop expression statement)
                (statement:if $3 $2 $4 $5)]
               [(REM) (statement:rem $1)]
               [(CLEAR) (statement:clear)]
               [(RUN) (statement:run empty)]
               [(RUN COMMA exprlist) (statement:run $3)]
               [(LIST) (statement:list empty)]
               [(LIST expression) (statement:list (list $2))]
               [(LIST expression COMMA expression) (statement:list (list $2 $4))]
               [(END) (statement:end)]
               [(BYE) (statement:bye)]
               [() (statement:empty)])
    (printlist [() empty]
               [(printitem) (list $1)]
               [(printitem COLON) (list $1 'colon)]
               [(printitem separator printlist) (cons $1 (cons $2 $3))])
    (printitem [(expression) $1]
               [(STRING) $1])
    (varlist [(var) (list $1)]
             [(var COMMA varlist) (cons $1 $3)])
    (exprlist [(expression) (list $1)]
              [(expression COMMA exprlist) (cons $1 $3)])
    (expression [(unsignedexpr) (expression #f $1)]
                [(ADD unsignedexpr) (expression '+ $2)]
                [(SUB unsignedexpr) (expression '- $2)])
    (unsignedexpr [(term) (unsignedexpr:unary $1)]
                  [(term ADD unsignedexpr)
                   (unsignedexpr:add $1 $3)]
                  [(term SUB unsignedexpr)
                   (unsignedexpr:sub $1 $3)])
    (term [(factor) (term:unary $1)]
          [(factor MUL term) (term:mul $1 $3)]
          [(factor DIV term) (term:div $1 $3)])
    (factor [(VAR) (factor $1)]
            [(NUMBER) (factor $1)]
            [(OPEN-PAREN expression CLOSE-PAREN) (factor $2)]
            [(function) (factor $1)])
    (function [(RND OPEN-PAREN expression CLOSE-PAREN) (function:rnd $3)])
    (relop [(LT) '<]
           [(GT) '>]
           [(EQ) '=]
           [(LTE) '<=]
           [(GTE) '>=]
           [(NE) '!=])
    (var [(VAR) $1])
    (separator [(COMMA) 'comma]
               [(SEMICOLON) 'semicolon]))))

(define (port->tinybasic-program ip)
  (port->list tb-read ip))

(define (tb-read [ip (current-input-port)])
  (match (read-line ip)
    [(? eof-object? e) e]
    [text
     (match (string->line text)
       [(struct* line ([number 0] [entire entire]))
        (raise-syntax-error 'tb-read "Line number cannot be zero in `~a'" entire)]
       [(? line? li) li]
       [#f (raise-syntax-error 'tb-read "Bad line `~a'" text)])]))

(define (tb-write ast [output-port (current-output-port)] #:what [what 'all])
  (parameterize ([current-output-port output-port])
    (define lines (map cdr (sort (hash->list ast) < #:key car)))
    (define width
      (add1 (inexact->exact (round (log (if (empty? lines) 1000 (line-number (last lines))) 10)))))
    (define (pline li)
      (printf "~a ~a\n"
              (~r (line-number li) #:min-width width)
              (line-text li)))
    (match what
      ['all (for-each pline lines)]
      [(? exact-positive-integer? n)
       (match (hash-ref ast n #f)
         [#f (void)]
         [li (pline li)])]
      [(list a b)
       (for-each pline (filter (match-lambda [(struct* line ([number n])) (<= a n b)]) lines))])))

(define (make-vars)
  (for/hash ([v (in-list (range 26))])
    (values (integer->char (+ v (char->integer #\A))) 0)))

(define (get-var st var)
  (hash-ref (state-vars st) var))

(struct state (vars program gosubs lineno) #:transparent)
(define clean-state (state (make-vars) (hash) empty #f))

(define (eval-expr expr the-state)
  (define (eval-unsigned unsigned)
    (match unsigned
      [(struct unsignedexpr:unary (term)) (eval-term term)]
      [(struct unsignedexpr:add (a b)) (+ (eval-term a) (eval-unsigned b))]
      [(struct unsignedexpr:sub (a b)) (+ (eval-term a) (eval-unsigned b))]))
  (define (eval-term term)
    (match term
      [(struct term:unary (factor)) (eval-factor factor)]
      [(struct term:mul (a b)) (* (eval-factor a) (eval-term b))]
      [(struct term:div (a b)) (quotient (eval-factor a) (eval-term b))]))
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
     (struct-copy state the-state [lineno (e expr)] [gosubs (cons (line-number the-line) (state-gosubs the-state))])]
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
                       [(list (or 'semicolon 'comma)) (void)]
                       ['semicolon (void)]
                       ['comma (display #\tab)]))
                   the-state]
                  [(struct statement:list ((list expr)))
                   (tb-write (state-program the-state))]
                  [(struct statement:let (var expr))
                   (struct-copy state the-state [vars (hash-set (state-vars the-state) var (e expr))])]
                  [(struct statement:bye ())
                   (exit 0)]))
     (struct-copy state st [lineno (next-line-in-listing st)])]))

(define (eval-line/direct the-line the-state)
  (define next-state
    (match (parse-line the-line)
      [(struct* statement:input ()) (displayln "TODO interactive INPUT stmt")
                                    the-state]
      [(struct statement:run ((list xs ..1)))
       (displayln "TODO interactive RUN stmt")
       the-state]
      [_ (eval-line the-line the-state)]))
  (let loop ([st next-state])
    (cond
      [(batch? st)
       (loop (eval-line (hash-ref (state-program st) (state-lineno st)) st))]
      [else st])))

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

(define (main)
  (define st clean-state)
  (define (eval what)
    (match-define (cons _ the-line) what)
    (match the-line
      [(and the-line (struct line (#f stmt _)))
       (with-handlers ([exn:fail? (λ (err) (printf "Eval failed: ~a\n" (exn-message err)) st)])
         (set! st (eval-line/direct the-line st)))]
      [(and the-line (struct line (lineno stmt raw)))
       (set! st (struct-copy state st
                             [program (hash-set (state-program st)
                                                lineno the-line)]))]))
  (current-prompt-read tb-read)
  (current-eval eval)
  (current-print (thunk* (void)))
#;
  (when (terminal-port? (current-input-port))
    (dynamic-require 'readline #f)
    (current-prompt #": ")
    (keep-duplicates 'unconsecutive))
  (read-eval-print-loop))

(module+ main
  (main))

(define (parse-line s)
  (when (line? s) (set! s (line-text s)))
  (if (statement? s)
      s
      (with-input-from-string s
        (thunk
         (statement-parser (thunk (tb-lexer (current-input-port))))))))
