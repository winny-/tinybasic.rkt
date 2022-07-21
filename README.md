# tinybasic.rkt

TinyBASIC implementation. Great language to practice with Racket's
lex/yacc clone.

## More information

- http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.htm
- https://en.wikipedia.org/wiki/Tiny_BASIC

## Status

It mostly works.

### TODO

- `INPUT`
- `#lang tinybasic`
- Clean up non-free examples/docs

## Differences

### New statements

#### `BYE`

Exit TinyBasic monitor.

Example:

```basic
REM Exits tinybasic.rkt and returns control back to shell.
BYE
```

#### `LOAD`

Load a program from a file.

Example:

```basic
REM Loads examples/hello.bas.
LOAD "examples/hello.bas"
```

#### `SAVE`

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
