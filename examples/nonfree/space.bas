#lang tinybasic
REMARK: TO FIND OUT HOW MUCH PROGRAM SPACE 
REM... YOU HAVE LEFT, TYPE: 
LET I=0 
1 LET I=I+2 
2 GOSUB 1 
RUN 
REMARK: AFTER A FEW SECONDS, THIS WILL STOP 
REM... WITH AN ERROR; THEN TYPE: 
END 
PRINT "THERE ARE ";I;" BYTES LEFT"
