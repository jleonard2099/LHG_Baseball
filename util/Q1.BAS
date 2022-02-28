RANDOMIZE TIMER
dim x$(67)

10 open "fc.pbp" as #1 len=30
field #1,30 as q$
GET #1,int(rnd(1)*40)+1010
X$=Q$
15 IF RIGHT$(X$,1)=" " THEN X$=LEFT$(X$,LEN(X$)-1):GOTO 15
PRINT X$
close 1
