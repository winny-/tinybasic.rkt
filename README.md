# tinybasic.rkt

TinyBASIC implementation. Great language to practice with Racket's
lex/yacc clone.

## More information

- http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.htm
- https://en.wikipedia.org/wiki/Tiny_BASIC

## Status

It sorta works.  I think there's a precedence issue with MUL / DIV.  Testcase:

```bash
echo run | cat examples/random.bas - | racket tinybasic.rkt
```

outputs:

```
876166508014850563998993211812996257478583618645656828278906681797551328261158485532314783354491418866361211127119281689656470
```

## Differences

### New statements

#### `BYE`

Exit TinyBasic monitor.

Example:

```basic
REM Exits tinybasic.rkt and returns control back to shell.
BYE
```

## License

`tinybasic.rkt` is covered under MIT/X.

Files in `examples/` and `TBuserMan.txt` are of unknown license - consider all rights reserved.
