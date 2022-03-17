
RANDOMIZE TIMER
dim x$(190)

open "test.bas" for input as #1
for x=0 to 0
input #1,x$(x)
print x$(x)
next
close 1

open "revised.pbp" as #1 len=44
field #1,44 as q$
for x=0 to 0
lset q$=x$(x)
put #1,x + 1211
next
close 1

10 open "revised.pbp" as #1 len=44
field #1,44 as q$
GET #1,int(rnd(1)*1)+1211
X$=Q$

15 IF RIGHT$(X$,1)=" " THEN X$=LEFT$(X$,LEN(X$)-1):GOTO 15
PRINT X$
close 1
