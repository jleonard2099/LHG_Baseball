RANDOMIZE TIMER
dim x$(67)

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

10 open "fc.pbp" as #1 len=30
field #1,30 as q$
GET #1,int(rnd(1)*10)+626
X$=Q$

15 IF RIGHT$(X$,1)=" " THEN X$=LEFT$(X$,LEN(X$)-1):GOTO 15
PRINT X$
close 1
