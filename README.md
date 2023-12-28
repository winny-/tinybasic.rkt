# tinybasic.rkt

TinyBASIC implementation. Great language to practice with Racket's
lex/yacc clone.

## More information

- http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.htm
- https://en.wikipedia.org/wiki/Tiny_BASIC

## Installation

```
raco pkg install tinybasic
```

## Usage

Run `tinybasic` or `racket -l tinybasic` to start the tinybasic monitor.  To
run a program and quit, try `tinybasic --batch yourprogram.bas`.  Drop the
`--batch` if you wish to load the program then start the tinybasic monitor.

Alternately, you may also use Racket's `#lang` facility:

```
#lang tinybasic
100 PRINT "Hello, world!"
120 END
```

(Run this via `racket your-hashlang-tinybasic-file.bas`.)

## Status

It mostly works.

### TODO

- `INPUT` should prompt with `? ` if output line is empty
- `racket -l tinybasic/examples/fizzbuzz --repl` should open a tinybasic
  monitor (I think this should also fix DrRacket interaction.)

## Differences

### New statement - `BYE`

Exit TinyBasic monitor.

Example:

```basic
REM Exits tinybasic.rkt and returns control back to shell.
BYE
```

### New statement - `LOAD`

Load **a line-numbered program** from a file.  Direct-mode lines (ones without
a line number) are ignored.

Example:

```basic
REM Loads examples/hello.bas.
LOAD "examples/hello.bas"
```

### New statement `SAVE`

Save a program to a file.

Example:

```basic
REM Saves hello.bas
10 PRINT "Hello, world!"
SAVE "examples/hello.bas"
```

### New statement `RACKET`

Run Racket code with access to the interpreter's variables.

This escape hatch gives the user a way to call native Racket code.  This means
you could **in theory** write useful, modern applications in tinybasic.  Pretty
cool eh?

```racket
10 REM Greet the user
20 RACKET (printf "Hello, ~a.  How are you today?\n" (or (getenv "USER") (getenv "USERNAME")))
30 END
```

If the user `set!`'s a single *upper* case variable, it will copy over to the
Tinybasic environment.  Otherwise, if the user `set!`'s a single *lower* case
variable, it will copy over.  Otherwise the original value is copied over
(default is `0`).

## Contrib

There are examples in the tinybasic-examples package.  If you use org-mode, one
can use ob-basic.el contained in the tinybasic-examples package to evaluate
tinybasic programs in org-mode src blocks.

## License

`tinybasic.rkt` is covered under MIT/X.

Files in `examples/nonfree/` and `TBuserMan.txt` are of unknown license - consider all rights reserved.
