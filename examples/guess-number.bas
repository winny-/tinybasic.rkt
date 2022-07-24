#lang tinybasic
 80 PRINT "Select upper bound?",
 90 INPUT B
 95 LET D = -1
100 LET N = RND(B)
120 PRINT "Guess?",
140 INPUT X
160 If X = N THEN GOTO 300
165 LET Y = N - X
170 GOSUB 440
180 IF D < 0 THEN GOSUB 400
215 PRINT "You guessed "; X; ".  ";
220 IF D < Y THEN PRINT "Colder";
230 IF D > Y THEN PRINT "Warmer";
235 IF D = Y THEN PRINT "No change";
237 PRINT ".  Try again."
240 LET D = Y
260 GOTO 120
300 PRINT "Hooray!  You guessed correctly!  "; X; " is my secret number."
320 PRINT "Play again (zero to exit, non-zero to play again)?",
340 INPUT G
360 if G = 0 THEN END
380 GOTO 80
400 PRINT "No, sorry, "; X; " is not the correct number.  Try again."
420 GOTO 240
440 REM ABS(Y) -> Y
460 IF Y < 0 THEN LET Y = -Y
480 RETURN
