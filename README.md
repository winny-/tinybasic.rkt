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
- Clean up non-free examples/docs

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

## License

`tinybasic.rkt` is covered under MIT/X.

Files in `examples/nonfree/` and `TBuserMan.txt` are of unknown license - consider all rights reserved.
