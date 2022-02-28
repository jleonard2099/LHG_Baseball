DEFINT A-Z
DECLARE SUB TITLE ()
DECLARE SUB GETKEY ()
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SetCnf.BI'          'queries the system info for MsgBox
   DRIVE = GetDrive%
   DIM B$(100),P$(80),P1(80), B1(100),B0(100,23), P0(80,42), T(22), T1(23)
   DIM D$(3)
   DIM BC(100,18), PC(80,17)
   DIM BL1(18,20) AS SINGLE,BL0$(18,20),PL1(16,21) AS SINGLE,PL0$(80,20)
   REDIM BRS%(15,100,18), PRS%(15,80,17)
  TMR1=2700
  OPEN "DEV.BAS" FOR INPUT AS #1
  FOR I = 0 TO 3
  INPUT #1, D$(I)
  NEXT
  CLOSE #1
3 CLS:COLOR 12:PRINT"CAREER LEADER MENU"
4 COLOR 14:PRINT "(1)  ";:COLOR 15:PRINT"UPDATE LEADERS":PRINT
5 COLOR 14:PRINT "(2)  ";:COLOR 15:PRINT"VIEW/PRINT CAREER LEADERS":PRINT
8 COLOR 14:PRINT "(3)  ";:COLOR 15:PRINT"RETURN TO UTILITIES MENU"
9 I$=INKEY$:IF I$="" THEN 9
10 I = VAL(I$):IF I <1 OR I >3 THEN 9
11 IF I = 1 THEN GOSUB 4990:GOTO 130
12 IF I = 2 THEN GOTO 2000
15 IF I = 3 THEN GOTO 5600

130 FOR I = 0 TO 17:FOR I1=0 TO 19:BL1(I,I1)=0:NEXT:NEXT
131 FOR I = 0 TO 14:FOR I1=0 TO 19:PL1(I,I1)=0:NEXT:NEXT
132 FOR I1 = 0 TO 20:PL1(15,I1)=1000:NEXT
200 Spec$ = D$(1) + "*.CAR"
    Count = FCount%(Spec$)
   IF Count = 0 THEN 4500
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT CAREER FILE TO VIEW": COLOR 15
     SELECTION% = 1
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
    LOCATE 25, 1
    COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS TEAM";
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN 3
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3913 I$ = INKEY$: IF I$ = "" THEN 3913
    IF UCASE$(I$) = "Y" THEN 1310
3911 Z$ = QPRTrim$(F$)
    CLS
201 INPUT "ENTER NUMBER OF AT BATS TO QUALIFY";N1
202 INPUT "ENTER NUMBER OF INNINGS PITCHED TO QUALIFY";N2
203 LOCATE 7:PRINT "ANY CHANGE (Y/N)"
204 I$=INKEY$:IF I$="" THEN 204
205 IF UCASE$(I$)="Y" THEN 200
220 OPEN D$(1)+Z$ FOR INPUT AS #1
221 INPUT #1, NB,NP
222 FOR I = 0 TO 99:INPUT #1, B$(I):NEXT
223 FOR I = 0 TO 79:INPUT #1, P$(I):NEXT
224 FOR J = 1 TO 15:FOR I = 0 TO 99:FOR I1=O TO 17:INPUT #1, BRS%(J,I,I1):NEXT I1:LOCATE 7,35:TMR1=TMR1-1:COLOR 9:PRINT USING "####";TMR1:NEXT I:NEXT J
225 FOR J = 1 TO 15:FOR I = 0 TO 79:FOR I1=0 TO 16:INPUT #1, PRS%(J,I,I1):NEXT I1:LOCATE 7,35:TMR1=TMR1-1:COLOR 9:PRINT USING "####";TMR1:NEXT I:NEXT J
226 CLOSE #1
250 LOCATE 10:COLOR 14:PRINT "COMPUTING LEADER FILE . . . . . . PLEASE WAIT"
400 FOR I = 0 TO 99
401 FOR I1 = 0 TO 14
402 FOR J = 1 TO 15
403 BC(I,I1)=BC(I,I1)+BRS%(J,I,I1)
404 NEXT:NEXT:NEXT
420 FOR I = 0 TO 79
421 FOR I1 = 0 TO 16
422 FOR J = 1 TO 15
423 PC(I,I1)=PC(I,I1)+PRS%(J,I,I1)
424 NEXT:NEXT:NEXT
430 FOR K = 0 TO 99
    FOR X = 0 TO 14
431 IF BC(K,X) > BL1(X,19) THEN 432 ELSE 440
432 BL1(X,19) = BC(K,X):BL0$(X,19)=B$(K)
    FOR I = 1 TO 20
      FOR J = 20 TO I STEP -1
      IF BL1(X,J-1) < BL1(X,J) THEN SWAP BL1(X,J-1), BL1(X,J):SWAP BL0$(X,J-1),BL0$(X,J)
      NEXT J
     NEXT I
440 NEXT X
450 NEXT K
460 FOR K = 0 TO 99
    IF BC(K,0) < N1 OR BC(K, 0) = 0 THEN 469
461 IF BC(K,1)/BC(K,0) > BL1(15,19) THEN 462 ELSE 469
462 BL1(15,19) = BC(K,1)/BC(K,0):BL0$(15,19)=B$(K)
    FOR I = 1 TO 20
      FOR J = 20 TO I STEP -1
      IF BL1(15,J-1) < BL1(15,J) THEN SWAP BL1(15,J-1), BL1(15,J):SWAP BL0$(15,J-1),BL0$(15,J)
      NEXT J
     NEXT I
469 NEXT K
470 FOR K = 0 TO 99
    IF BC(K,0) < N1 OR BC(K, 0) = 0 THEN 479
471 IF (BC(K,1)+BC(K,4)+BC(K,5)*2+BC(K,6)*3)/BC(K,0) > BL1(16,19) THEN 472 ELSE 479
472 BL1(16,19) =(BC(K,1)+BC(K,4)+BC(K,5)*2+BC(K,6)*3)/BC(K,0):BL0$(16,19)=B$(K)
    FOR I = 1 TO 20
      FOR J = 20 TO I STEP -1
      IF BL1(16,J-1) < BL1(16,J) THEN SWAP BL1(16,J-1), BL1(16,J):SWAP BL0$(16,J-1),BL0$(16,J)
      NEXT J
     NEXT I
479 NEXT K
480 FOR K = 0 TO 99
    IF BC(K,0) < N1 OR BC(K, 0)+BC(K, 7)=0 THEN 489
481 IF (BC(K,1)+BC(K,7))/(BC(K,0)+BC(K,7)) > BL1(17,19) THEN 482 ELSE 489
482 BL1(17,19) =(BC(K,1)+BC(K,7))/(BC(K,0)+BC(K,7)):BL0$(17,19)=B$(K)
    FOR I = 1 TO 20
      FOR J = 20 TO I STEP -1
      IF BL1(17,J-1) < BL1(17,J) THEN SWAP BL1(17,J-1), BL1(17,J):SWAP BL0$(17,J-1),BL0$(17,J)
      NEXT J
     NEXT I
489 NEXT K
530 FOR K = 0 TO 79
    FOR X = 0 TO 14
531 IF PC(K,X) > PL1(X,19) THEN 532 ELSE 540
532 PL1(X,19) = PC(K,X):PL0$(X,19)=P$(K)
    FOR I = 1 TO 20
      FOR J = 20 TO I STEP -1
      IF PL1(X,J-1) < PL1(X,J) THEN SWAP PL1(X,J-1), PL1(X,J):SWAP PL0$(X,J-1),PL0$(X,J)
      NEXT J
     NEXT I
540 NEXT X
550 NEXT K
560 FOR K = 0 TO 79
    IF PC(K,0) < N2*3 OR PC(K, 0) = 0 THEN 569
561 IF ((PC(K,3)*9)/PC(K,0))*3 < PL1(15,19) THEN 562 ELSE 569
562 PL1(15,19) =((PC(K,3)*9)/PC(K,0))*3:PL0$(15,19)=P$(K)
    FOR I = 1 TO 20
      FOR J = 20 TO I STEP -1
      IF PL1(15,J) < PL1(15,J-1) THEN SWAP PL1(15,J), PL1(15,J-1):SWAP PL0$(15,J),PL0$(15,J-1)
      NEXT J
     NEXT I
569 NEXT K
600 GOSUB 5000:OPEN D$(1)+LEFT$(Z$,JX)+".TCL" FOR OUTPUT AS #1
601 FOR I = 0 TO 17:FOR I1=0 TO 19:WRITE #1, BL1(I,I1),BL0$(I,I1):NEXT:NEXT
602 FOR I = 0 TO 15:FOR I1=0 TO 19:WRITE #1, PL1(I,I1),PL0$(I,I1):NEXT:NEXT
603 CLOSE #1
610 GOTO 3
2000 CLS
   Spec$ = D$(1) + "*.TCL"
    Count = FCount%(Spec$)
   IF Count = 0 THEN 4500
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)

1311 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT CAREER FILE TO VIEW": COLOR 15
     SELECTION% = 1
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
    LOCATE 25, 1
    COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS TEAM";
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN 3
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3912 I$ = INKEY$: IF I$ = "" THEN 3912
    IF UCASE$(I$) = "Y" THEN 1311
    Z1$ = QPRTrim$(F$)
    CLS
2001 GOSUB 4980
     OPEN D$(1)+Z1$ FOR INPUT AS #1
2002 FOR I = 0 TO 17:FOR I1=0 TO 19:INPUT #1, BL1(I,I1),BL0$(I,I1):NEXT:NEXT
2003 FOR I = 0 TO 15:FOR I1=0 TO 19:INPUT #1, PL1(I,I1),PL0$(I,I1):NEXT:NEXT
2004 CLOSE #1
2010 COLOR 15,0
     CLS
     LOCATE 8:COLOR 14:PRINT "P";:COLOR 15:PRINT "RINT OR ";:COLOR 14:PRINT "V";:COLOR 15:PRINT "IEW"
2011 I$=INKEY$:IF I$="" THEN 2011
2012 IF UCASE$(I$)="P" THEN 2300
2100 CLS:COLOR 12:PRINT TAB(5)"AT BATS                                HITS"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(0,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(0,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(1,I);:COLOR 14:PRINT TAB(60)USING "####";BL1(1,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"RUNS SCORED                            RBI'S"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(2,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(2,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(3,I);:COLOR 14:PRINT TAB(60)USING "####";BL1(3,I):NEXT
     GOSUB 2950
2105 CLS:COLOR 12:PRINT TAB(5)"DOUBLES                                TRIPLES"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(4,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(4,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(5,I);:COLOR 14:PRINT TAB(60)USING "####";BL1(5,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"HOME RUNS                              WALKS"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(6,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(6,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(7,I);:COLOR 14:PRINT TAB(60)USING "####";BL1(7,I):NEXT
     GOSUB 2950
2110 CLS:COLOR 12:PRINT TAB(5)"STRIKE OUTS                            STOLEN BASES"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(8,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(8,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(9,I);:COLOR 14:PRINT TAB(60)USING "####";BL1(9,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"CAUGHT STEALING                        ERRORS"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(10,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(10,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(11,I);:COLOR 14:PRINT TAB(60)USING "####";BL1(11,I):NEXT
     GOSUB 2950
2115 CLS:COLOR 12:PRINT TAB(5)"GAMES PLAYED                           BATTING AVERAGE"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(14,I);:COLOR 14:PRINT TAB(20)USING "####";BL1(14,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(15,I);:COLOR 14:PRINT TAB(60)USING ".###";BL1(15,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"SLUGGING PCT.                          ON-BASE PCT."
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(16,I);:COLOR 14:PRINT TAB(20)USING ".###";BL1(16,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT BL0$(17,I);:COLOR 14:PRINT TAB(60)USING ".###";BL1(17,I):NEXT
     GOSUB 2950
2200 CLS:COLOR 12:PRINT TAB(5)"INNINGS PITCHED                        HITS ALLOWED"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(0,I);:COLOR 14:PRINT TAB(18)USING "###";INT(PL1(0,I)/3);:IF PL1(O,I)-(INT(PL1(O,I)/3)*3)>0 THEN PRINT ".";:PRINT USING "#";PL1(O,I)-(INT(PL1(O,I)/3)*3);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(1,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(1,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"RUNS ALLOWED                           EARNED RUNS ALLOWED"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(2,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(2,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(3,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(3,I):NEXT
     GOSUB 2950
2205 CLS:COLOR 12:PRINT TAB(5)"WALKS                                  STRIKEOUTS"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(4,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(4,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(5,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(5,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"WINS                                   LOSSES"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(6,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(6,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(7,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(7,I):NEXT
     GOSUB 2950
2210 CLS:COLOR 12:PRINT TAB(5)"GAMES                                  GAMES STARTED"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(8,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(8,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(9,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(9,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"COMPLETE GAMES                         SHUTOUTS"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(10,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(10,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(11,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(11,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"SAVES                                  DOUBLE PLAYS THROWN"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(12,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(12,I);:
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(13,I);:COLOR 14:PRINT TAB(60)USING "####";PL1(13,I):NEXT
     GOSUB 2950
     CLS:COLOR 12:PRINT TAB(5)"HOME RUNS ALLOWED                     EARNED RUN AVERAGE"
     FOR I = 0 TO 19
     COLOR 3:LOCATE I+2:PRINT USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(14,I);:COLOR 14:PRINT TAB(20)USING "####";PL1(14,I);
     COLOR 3:PRINT TAB(40)USING "##";I+1;:PRINT ". ";:COLOR 15:PRINT PL0$(15,I);:COLOR 14:PRINT TAB(58)USING "###.##";PL1(15,I):NEXT
     GOSUB 2950
2215 GOTO 3
2300 LPRINT CHR$(15)
2301 WIDTH "LPT1:1",130
     LPRINT TAB(5)"AT BATS" TAB(29)"HITS"TAB(54)"RUNS SCORED" TAB(79)"RBI'S" TAB(104)"DOUBLES"
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(0,I);:LPRINT TAB(18)USING "####";BL1(0,I);:LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(1,I);:LPRINT TAB(43)USING "####";BL1(1,I);
     LPRINT TAB(50)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(2,I);:LPRINT TAB(68)USING "####";BL1(2,I);:LPRINT TAB(75)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(3,I);:LPRINT TAB(93)USING "####";BL1(3,I);
     LPRINT TAB(100)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(4,I);:LPRINT TAB(118)USING "####";BL1(4,I):NEXT
     LPRINT:LPRINT
     LPRINT TAB(5)"TRIPLES" TAB(29)"HOME RUNS"TAB(54)"WALKS" TAB(79)"STRIKE OUTS" TAB(104)"STOLEN BASES"
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(5,I);:LPRINT TAB(18)USING "####";BL1(5,I);:LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(6,I);:LPRINT TAB(43)USING "####";BL1(6,I);
     LPRINT TAB(50)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(7,I);:LPRINT TAB(68)USING "####";BL1(7,I);:LPRINT TAB(75)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(8,I);:LPRINT TAB(93)USING "####";BL1(8,I);
     LPRINT TAB(100)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(9,I);:LPRINT TAB(118)USING "####";BL1(9,I):NEXT
     GOSUB 2980
     LPRINT:LPRINT
     LPRINT TAB(5)"CAUGHT STEALING" TAB(29)"ERRORS"TAB(54)"GAMES PLAYED" TAB(79)"BATTING AVERAGE" TAB(104)"SLUGGING PCT."
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(10,I);:LPRINT TAB(18)USING "####";BL1(10,I);:LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(11,I);:LPRINT TAB(43)USING "####";BL1(11,I);
     LPRINT TAB(50)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(14,I);:LPRINT TAB(68)USING "####";BL1(14,I);:LPRINT TAB(75)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(15,I);:LPRINT TAB(93)USING ".###";BL1(15,I);
     LPRINT TAB(100)USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(16,I);:LPRINT TAB(118)USING ".###";BL1(16,I):NEXT
     LPRINT:LPRINT
     LPRINT TAB(5)"ON-BASE PCT." TAB(29)"INNINGS PITCHED"TAB(54)"HITS ALLOWED" TAB(79)"RUNS ALLOWED" TAB(104)"EARNED RUNS ALLOWED"
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT BL0$(17,I);:LPRINT TAB(18)USING ".###";BL1(17,I);
     LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(0,I);:LPRINT TAB(43)USING "###";INT(PL1(0,I)/3);:IF PL1(O,I)-(INT(PL1(O,I)/3)*3)>0 THEN LPRINT ".";:LPRINT USING "#";PL1(O,I)-(INT(PL1(O,I)/3)*3);
     LPRINT TAB(50)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(1,I);:LPRINT TAB(68)USING "####";PL1(1,I);:LPRINT TAB(75)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(2,I);:LPRINT TAB(93)USING "####";PL1(2,I);
     LPRINT TAB(100)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(3,I);:LPRINT TAB(118)USING "####";PL1(3,I):NEXT
     GOSUB 2980
     LPRINT:LPRINT
     LPRINT TAB(5)"WALKS" TAB(29)"STRIKEOUTS"TAB(54)"WINS" TAB(79)"LOSSES" TAB(104)"GAMES"
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(4,I);:LPRINT TAB(18)USING "####";PL1(4,I);:LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(5,I);:LPRINT TAB(43)USING "####";PL1(5,I);
     LPRINT TAB(50)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(6,I);:LPRINT TAB(68)USING "####";PL1(6,I);:LPRINT TAB(75)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(7,I);:LPRINT TAB(93)USING "####";PL1(7,I);
     LPRINT TAB(100)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(8,I);:LPRINT TAB(118)USING "####";PL1(8,I):NEXT
     LPRINT:LPRINT
     LPRINT TAB(5)"GAMES STARTED" TAB(29)"COMPLETE GAMES"TAB(54)"SHUTOUTS" TAB(79)"SAVES" TAB(104)"DOUBLE PLAYS THROWN"
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(9,I);:LPRINT TAB(18)USING "####";PL1(9,I);:LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(10,I);:LPRINT TAB(43)USING "####";PL1(10,I);
     LPRINT TAB(50)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(11,I);:LPRINT TAB(68)USING "####";PL1(11,I);:LPRINT TAB(75)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(12,I);:LPRINT TAB(93)USING "####";PL1(12,I);
     LPRINT TAB(100)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(13,I);:LPRINT TAB(118)USING "####";PL1(13,I):NEXT
     GOSUB 2980
     LPRINT:LPRINT
     LPRINT TAB(5)"HOME RUNS ALLOWED" TAB(29)"EARNED RUN AVERAGE"
     FOR I = 0 TO 19
     LPRINT USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(14,I);:LPRINT TAB(20)USING "####";PL1(14,I);:LPRINT TAB(25)USING "##";I+1;:LPRINT ". ";:LPRINT PL0$(15,I);:LPRINT TAB(43)USING "###.##";PL1(15,I):NEXT
     GOSUB 2980
2902 GOTO 3
2950 LOCATE 23,22:COLOR 15:PRINT "HIT ANY KEY TO CONTINUE"
2951 I$=INKEY$:IF I$="" THEN 2951
2952 RETURN
2980 LPRINT CHR$(12)
2981 RETURN
4980 CLS
     IF UCASE$(LEFT$(D$(1),1))<>"A" AND UCASE$(LEFT$(D$(1),1))<>"B" THEN 4983
     COLOR 15
     PRINT "MAKE SURE CAREER LEADER DISK IS IN DRIVE ";
     COLOR 12:PRINT D$(1)
     COLOR 15
     CALL GETKEY
4983 RETURN
4990 CLS
     IF UCASE$(LEFT$(D$(1),1))<>"A" AND UCASE$(LEFT$(D$(1),1))<>"B" THEN 4993
     COLOR 15
     PRINT "MAKE SURE CAREER STATS DISK IS IN DRIVE ";
     COLOR 12:PRINT D$(1)
     COLOR 15
     CALL GETKEY
4993 RETURN
5000 D=0
5001 FOR J = 0 TO 8
5002 D=D+1
5003 X$=MID$(Z$,D,1)
5004 IF X$="." THEN JX=D-1
5005 NEXT J
5006 RETURN
5600 IF DRIVE >=67 THEN 5610
     CLS
     PRINT "MAKE SURE PROGRAM DISK #6 IS IN DRIVE"
     CALL GETKEY
5610 RUN "CAREER.EXE"
4500 LOCATE 10, 8
     CALL CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 5600
STATIC SUB CSMsgBox (Message$, Wdth, Cnf AS Config)
    IF LEN(Message$) THEN
       Row = CSRLIN                             'Get the Row Position
       REDIM Temp$(23)                          'Make a temporary array to
                                                '  hold the parsed up lines
       MesLen = LEN(Message$)                   'Get the total length of the
                                                '  help message
       LN = 0                                   'Line number
       Strt = 1                                 'Starting parse position
       '*** Parse the message into lines
       DO
           WHILE MID$(Message$, Strt, 1) = " "  'Skip over blanks
              Strt = Strt + 1
           WEND
           ND = Strt + Wdth                     'Guess at the end of line
                                                'Loop backwards to find
           WHILE MID$(Message$, ND, 1) <> " " AND ND <= MesLen AND ND > Strt
               ND = ND - 1                      '  blank space
           WEND
           LN = LN + 1                          'Increment the Line number
                                                'Pull out the new line
           Temp$(LN) = MID$(Message$, Strt, ND - Strt)
           Strt = ND + 1                        'Reset starting posit. for
                                                '  next line
       LOOP UNTIL ND >= MesLen                  'Get more unless @ end of mes
       BotRow = Row + LN + 1                    'Find the bottom row #
       IF BotRow > 24 THEN BotRow = 24          'No farther than line 24
       ArraySize = 80 * (BotRow - Row + 2)      'Find the array size to use
       REDIM ScrBuf(ArraySize)                  '  for the background screen
       MScrnSave Row, 1, BotRow + 1, 80, ScrBuf(0)
       BoxWdth = Wdth + 4                       'Find the outer box width
       ColPos = 40 - (BoxWdth \ 2)              'Starting posit for centered
                                                '  box
       LOCATE Row, ColPos                       'Print the top of box
       MQPrint "É" + STRING$(Wdth + 2, "Í") + "»", Cnf.MenBox
       Box$ = "º" + SPACE$(Wdth + 2) + "º"      'Make the box sides
       FOR N = 1 TO LN                          'Print message lines
           LOCATE N + Row, ColPos
           MQPrint Box$, Cnf.MenBox
           LOCATE , ColPos + 2
           MQPrint Temp$(N), Cnf.ActivCh
       NEXT
       LOCATE N + Row, ColPos                   'Print the bottom of box
       MQPrint "È" + STRING$(Wdth + 2, "Í") + "¼", Cnf.MenBox
       MPaintBox Row + 1, ColPos + BoxWdth, N + Row, ColPos + BoxWdth + 1, 8
       MPaintBox N + Row + 1, ColPos + 2, N + Row + 1, ColPos + BoxWdth + 1, 8
       Pass = 1                                 'Set flag say we've been here
       ERASE Temp$                              'Don't need this anymore
    ELSEIF Pass = 1 THEN                        'If we've been here before,
                                                '  restore the screen
       MScrnRest Row, 1, BotRow + 1, 80, SEG ScrBuf(0)
       ERASE ScrBuf                             'Clean up memory
       Pass = 0                                 'Reset flag for next call
    END IF
END SUB

SUB GETKEY ()
    LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
1240 I$ = INKEY$: IF I$ = "" THEN 1240
END SUB

SUB TITLE ()
COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 30: COLOR 12
   PRINT "FULL COUNT BASEBALL 6.0"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
   COLOR 10
END SUB

