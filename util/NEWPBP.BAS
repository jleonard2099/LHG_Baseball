RANDOMIZE TIMER
dim x$(67)
goto 10

open "test.bas" for input as #1
for x=1 to 10
input #1,x$(x)
print x$(x)
next
close 1

open "fc.pbp" as #1 len=30
field #1,30 as q$
for x=1 to 10
lset q$=x$(x)
put #1,x+625
next
close 1

10 cls
KILL "TEST.BAS"
for x =1 to 3169
open "REVISED.pbp" as #1 len=44
field #1,44 as q$
GET #1,x
X$=Q$
open "test.bas" for append as #2
write #2, x$
close 2
15 IF RIGHT$(X$,1)=" " THEN X$=LEFT$(X$,LEN(X$)-1):GOTO 15
PRINT X$
close 1
next