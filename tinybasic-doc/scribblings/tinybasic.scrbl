#lang scribble/manual

@(require (for-label racket/base tinybasic))

@title{TinyBASIC}
@author{Winston (winny) Weinert}

@defmodule[tinybasic]

@section{Writing TinyBASIC Programs}

Refer to
@hyperlink["http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.htm"]{The
TINY BASIC User Manual} on ittybittycomputers.com's website.


@section{TinyBASIC Language}

@subsection{Control Flow}

@subsection{Statements}


@section{Running TinyBASIC}

If you put @verbatim{#lang tinybasic} at the top of a source file, you should be able to run your TinyBASIC program via @verbatim{racket yourprogram.rkt}.

Alternately, you may run either @verbatim{tinybasic} or @verbatim{racket -l tinybasic --} to open up an interactive monitor (REPL).  See @verbatim{tinybasic --help} or @verbatim{racket -l tinybasic -- --help} for more information.

@section{Running the examples}

Try one of the following commands:

@itemlist[
  @item{@verbatim{racket -l tinybasic/examples/fizzbuzz}}
  @item{@verbatim{racket -l tinybasic/examples/hello}}
  @item{@verbatim{racket -l tinybasic/examples/pascals-triangle}}
  @item{@verbatim{racket -l tinybasic/examples/guess-number}}
]

Get help with running an example like so:
@verbatim{racket -l tinybasic/examples/guess-number -- --help}.  You need the
@verbatim{--} to tell @verbatim{racket} the flag is for the loaded module, not for the
top level racket executable.

@section{Project Information}

@itemlist[
 @item{MIT/X Licensed}
 @item{@hyperlink["https://github.com/winny-/tinybasic.rkt"]{Source code on GitHub}}
 @item{@hyperlink["https://github.com/winny-/tinybasic.rkt/blob/master/README.md"]{Project README with additional information.}}
 @item{API and Language are NOT stable.  Subject to change.}
]