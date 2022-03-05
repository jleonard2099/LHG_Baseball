DECLARE SUB DRAWBOX ()
DEFINT A-Z
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE SUB PHBATTER ()
DECLARE SUB PrtSc0 (Pr%)
DECLARE SUB PROFILEPRINT ()
DECLARE SUB PHPITCHER ()
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB UPDATEFILE ()
DECLARE SUB PITSTRAT ()
DECLARE SUB PITCHAROUND ()
DECLARE SUB THROWHOME ()
DECLARE SUB BASERUN ()
DECLARE FUNCTION EXIST% (FILENAME$)
DECLARE FUNCTION GetDrive% ()
'$INCLUDE: 'DEFCNF.BI'
5  DIM K9(1) AS LONG, D$(3), Q$(1843), PB$(23), R$(999)
15 DIM B%(22, 44), P%(21, 36), B$(23), P$(21), L%(13), BS$(22), PS$(21)
   DIM N$(22), NP$(21), MG%(1000), X$(12), CZ(23), BTR$(23), BPR(23)
   DIM VV$(500), V%(500), GS%(21, 1), C$(10), AB%(9), YN$(1)
   DIM PT$(1), B1$(4), B3%(9), B7%(9), V1%(1), M%(9), B9%(9)
   DIM ITEM$(14)
   COMMON SHARED MG%(), D$(), T, Q$(), YN$(), R$(), YN$,B$(),P$()
   DIM Y AS LONG
   DIM FRAC AS SINGLE
   '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
   CALL InitMouse(There)
   YN$(0) = "Y": YN$(1) = "N"
   DATA A,B,C,D,E,F,G,H,I,J,K,L,M
   DATA 681,663,645,627,609,591,573,555,537
   FOR I = 0 TO 12: READ X$(I): NEXT
   FOR I = 1 TO 9: READ AB%(I): NEXT
   B$(23) = "PITCHER"
   PT$(1) = "LHP": PT$(0) = "RHP"
   DATA  LP,L,S,R,RP
   DATA dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph
   FOR I = 0 TO 4: READ B1$(I): NEXT
   FOR I = 0 TO 10: READ C$(I): NEXT I
   OPEN "DEV.BAS" FOR INPUT AS #1
   FOR I = 0 TO 3: INPUT #1, D$(I): NEXT I
   CLOSE #1
   DRIVE = GetDrive%
   CLS
   PRINT TAB(32); : COLOR 14, 4: PRINT "COMPUTER MANAGER": COLOR 15, 0
   PRINT
   PRINT
   PRINT "     THIS PROGRAM ALLOWS YOU TO CUSTOMIZE THE COMPUTER MANAGER'S"
   PRINT
   PRINT "     STRATEGY AND PLAYER USAGE FOR EACH INDIVIDUAL TEAM"
30 I$ = INKEY$: IF I$ = "" THEN 30
35 CLS
   IF DRIVE >= 67 THEN 38
   PRINT "INSERT TEAMS DISK TO WORK WITH IN DRIVE"
36 PRINT : PRINT "CURRENT TEAMS DISK IS SET FOR "; D$(0)
   LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE"
37 I$ = INKEY$: IF I$ = "" THEN 37
38 GOSUB 1445
   GOSUB 4512
   IF MG%(0) <> 999 THEN MG%(0) = 999: FOR I = 1 TO 999: MG%(I) = -1: NEXT
45 COLOR , 1: CLS : PRINT : PRINT TAB(23); : COLOR 14, 4: PRINT "FULL COUNT PROGRAMMABLE MANAGER": PRINT
   ITEM$(1) = "SELECT NEW TEAM TO EDIT                                  "
   ITEM$(2) = "SELECT/CHANGE LINEUPS                                    "
   ITEM$(3) = "SELECT/CHANGE ROTATIONS/RELIEVERS                        "
   ITEM$(4) = "SELECT/CHANGE PINCH HITTERS                              "
   ITEM$(5) = "SELECT/CHANGE DEFENSIVE REPLACEMENTS (8TH AND 9TH INNING)"
   ITEM$(6) = "SELECT/CHANGE CLOSING STRATEGY                           "
   ITEM$(7) = "SELECT/CHANGE HITTING STRATEGY                           "
   ITEM$(8) = "SELECT/CHANGE DEFENSIVE STRATEGY                         "
   ITEM$(9) = "SELECT/CHANGE PINCH RUNNING STRATEGY                     "
  ITEM$(10) = "SELECT/CHANGE BASE RUNNING STRATEGY                      "
  ITEM$(11) = "IMPORT MANAGERIAL PROFILE                                "
  ITEM$(12) = "EXPORT MANAGERIAL PROFILE                                "
  ITEM$(13) = "PRINT MANAGERIAL PROFILE                                 "
  ITEM$(14) = "RETURN TO GAME MENU                                      "
     SELECTION% = 1
     Count% = 14
     Start% = 1
     SCANCODE% = 14
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 5
     Column% = 10
     CALL Box0(4, 9, 19, 70, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN 86
     SELECT CASE SELECTION%
        CASE 1
           FOR I = 1 TO 999: MG%(I) = -1: NEXT: GOTO 35
        CASE 2
           GOTO 100
        CASE 3
           GOTO 55
        CASE 4
           GOTO 200
        CASE 5
           GOTO 280
        CASE 6
           CALL PITSTRAT: GOTO 45
        CASE 7
           GOTO 3000
        CASE 8
           GOTO 3300
        CASE 9
           GOTO 3501
        CASE 10
           CALL BASERUN: GOTO 45
        CASE 11
           GOSUB 3900: GOTO 45
        CASE 12
           GOSUB 3800: GOTO 45
        CASE 13
           GOSUB 4000: GOTO 45
        CASE 14
           GOTO 86
     END SELECT
55 CLS : PRINT TAB(22); : COLOR 14, 4: PRINT "PITCHING ROTATION/RELIEVER MANAGEMENT": PRINT : PRINT : COLOR , 0
   ITEM$(1) = "INPUT A NEW STARTING PITCHING ROTATION     "
   ITEM$(2) = "EDIT AN EXISTING STARTING PITCHING ROTATION"
   ITEM$(3) = "SELECT/EDIT MIDDLE RELIEVERS               "
   ITEM$(4) = "SELECT/EDIT SET-UP MEN                     "
   ITEM$(5) = "SELECT/EDIT CLOSERS                        "
   ITEM$(6) = "RETURN TO MAIN MENU                        "
   SELECTION% = 1
   Count% = 6
   Start% = 1
   SCANCODE% = 6
   FileColr% = 31
   HiLiteColr% = 112
   Spacing% = 1
   Row% = 5
   Column% = 19
   CALL Box0(4, 18, 11, 62, 2, 31)
   CALL SHOWCURSOR
   MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
   CALL HIDECURSOR
   COLOR , 0
   Q = SELECTION% - 1
   IF SCANCODE% = 27 OR Q = 5 THEN 45
   IF Q = 0 OR Q = 1 THEN 4500
   IF Q = 2 THEN 5000
   IF Q = 3 THEN 5200
   IF Q = 4 THEN 5300

86 IF DRIVE >= 67 THEN 88
   PRINT "MAKE SURE GAME DISK IS IN DRIVE": PRINT
   LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE"
87 I$ = INKEY$: IF I$ = "" THEN 87
88 RUN "HELLO.EXE"
100 CLS : PRINT TAB(26); : COLOR 14, 4: PRINT "LINEUP STRATEGY OPTIONS": PRINT : PRINT : COLOR , 0

   ITEM$(1) = "SELECT/CHANGE/DISPLAY LINEUPS VS. RHP                  "
   ITEM$(2) = "SELECT/CHANGE/DISPLAY LINEUPS VS. LHP                  "
   ITEM$(3) = "ANALYZE PLAYER USAGE ACCORDING TO YOUR LINEUPS SELECTED"
   ITEM$(4) = "RETURN TO TOP MENU                                     "
   SELECTION% = 1
   Count% = 4
   Start% = 1
   SCANCODE% = 4
   FileColr% = 31
   HiLiteColr% = 112
   Spacing% = 1
   Row% = 5
   Column% = 13
   CALL Box0(4, 12, 9, 68, 2, 31)
   CALL SHOWCURSOR
   MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
   CALL HIDECURSOR
   COLOR , 0
    Q2% = SELECTION% - 1
    IF Q2% = 3 OR SCANCODE% = 27 THEN 45
    IF Q2% = 1 OR Q2% = 0 THEN 480
    IF Q2% = 2 THEN 300

200 REM:SELECT PINCH HITTERS
     IF MG%(540) <> 999 THEN MG%(540) = 999: FOR I = 1 TO 10: MG%(I + 540) = -1: MG%(I + 550) = 0: NEXT
     CLS
     LOCATE 1, 1
     PRINT "# "; : COLOR L%(11), L%(12): PRINT T$;
     COLOR 14, 0: LOCATE , 16: PRINT "B   AB  HR  RBI   AVG"
     I2 = 0
     FOR I = 0 TO 22
     IF B$(I) = "XXX" OR LEFT$(B$(I), 1) = " " OR B$(I) = "" THEN I2 = I2 + 1: GOTO 205
     LOCATE I + 2 - I2, 1
     COLOR 2, 0
     IF I <= 9 THEN PRINT USING "#"; I; : COLOR 15, 0
     IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0
     FOR I1 = 1 TO 10
     IF I = MG%(I1 + 540) THEN COLOR L%(11), L%(12)
     NEXT
     LOCATE , 3: PRINT B$(I);
     COLOR 15, 0
     LOCATE , 16: PRINT B1$(B%(I, 0) + 2);
     LOCATE , 17: PRINT USING "  ###  ##  ###"; B%(I, 4); B%(I, 9); B%(I, 10);
     if b%(i,4)<>0 then PRINT USING "  .###"; B%(I, 6) / B%(I, 4);
     if b%(i,4)=0 then PRINT "  .---";
205 NEXT I
207 LOCATE 1, 48: PRINT "  PINCH HITTERS        "
    LOCATE , 48: PRINT "  ---- VS LHP  ----"
     FOR I = 1 TO 5
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 540) < 0 THEN PRINT : GOTO 210
     PRINT B$(MG%(I + 540))
210 NEXT
    LOCATE , 48: PRINT "  ---- VS RHP  ----"
     FOR I = 6 TO 10
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 540) < 0 THEN PRINT : GOTO 212
     PRINT B$(MG%(I + 540))
212 NEXT

211 LOCATE 15, 48: PRINT "(11) ACCEPT CHOICES"
     LOCATE 16, 48: PRINT "(12) DELETE A HITTER"
215 LOCATE 17, 48: INPUT "ENTER SELECTION (1-12)"; I
     IF I < 1 OR I > 12 THEN 215
     IF I >= 1 AND I <= 10 THEN GOSUB 220: GOTO 200
     IF I = 11 THEN CALL UPDATEFILE: GOTO 45
     IF I = 12 THEN GOSUB 250: GOTO 200
220 LOCATE 18, 48: PRINT USING "SELECT HITTER ##:"; I;
221 I$ = INKEY$: IF I$ = "" THEN 221
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I + 540) = I1
     REM:LOCATE 18,48:PRINT "                     "
     RETURN
250 LOCATE 16, 48: INPUT "SELECT HITTER TO DELETE: "; I
     IF I < 1 OR I > 10 THEN 250
     MG%(I + 540) = -1: MG%(I + 550) = 0
     RETURN
   GOTO 45
280 REM:DEFENSIVE REPLACEMENTS
    CLS : PRINT TAB(26); : COLOR 14, 4: PRINT "DEFENSIVE REPLACEMENT 8TH AND 9TH INNING STRATEGY OPTIONS": PRINT : PRINT : COLOR , 0
    IF MG%(560) <> 999 THEN MG%(560) = 999: FOR I = 1 TO 4: MG%(I + 560) = -1: MG%(I + 564) = 0: MG%(I + 568) = 0: MG%(I + 572) = 0: MG%(I + 576) = 0: NEXT
    CLS
    LOCATE 1, 1
    PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
    LOCATE , 17: PRINT "1 FAVG 2 FAVG 3 FAVG 4 FAVG"
    I1 = 0
    FOR I = 0 TO 22: LOCATE , 1
    IF B$(I) = "XXX" OR LEFT$(B$(I), 1) = " " OR B$(I) = "" THEN I1 = I1 + 1: GOTO 285
    LOCATE I + 2 - I1, 1
    COLOR 2, 0
    IF I < 10 THEN PRINT USING "#"; I; : COLOR 15, 0: GOSUB 1980
    IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0: GOSUB 1980
    LOCATE , 3: PRINT B$(I); : COLOR 15, 0
    LOCATE , 17: IF B%(I, 22) <> -1 THEN PRINT USING "# ####"; B%(I, 22); B%(I, 26);
    LOCATE , 24: IF B%(I, 23) <> -1 THEN PRINT USING "# ####"; B%(I, 23); B%(I, 27);
    LOCATE , 31: IF B%(I, 24) <> -1 THEN PRINT USING "# ####"; B%(I, 24); B%(I, 28);
    LOCATE , 38: IF B%(I, 25) <> -1 THEN PRINT USING "# ####"; B%(I, 25); B%(I, 29);
285 NEXT
    LOCATE 1, 46
    PRINT "# PLAYER     P SUBSTITUTE  %  RA"
    FOR I = 1 TO 4
    LOCATE , 46
    PRINT USING "# "; I;
    IF MG%(I + 560) < 0 THEN PRINT : GOTO 290
    PRINT LEFT$(B$(MG%(I + 560)), 10); TAB(59);
    PRINT USING "# "; MG%(I + 576);
    PRINT LEFT$(B$(MG%(I + 564)), 10); TAB(72); USING "### #"; MG%(I + 568); MG%(I + 572)
290 NEXT
286 LOCATE 7, 46: PRINT "(5) ACCEPT CHOICES"
    LOCATE 8, 46: PRINT "(6) DELETE A CHOICE"
287 LOCATE 9, 46: INPUT "ENTER SELECTION (1-6)"; I
     IF I < 1 OR I > 6 THEN 287
     IF I >= 1 AND I <= 4 THEN GOSUB 295: GOTO 280
     IF I = 5 THEN CALL UPDATEFILE: GOTO 45
     IF I = 6 THEN GOSUB 298: GOTO 280
295  LOCATE 10, 46: PRINT USING "SELECT HITTER #:"; I;
296  I$ = INKEY$: IF I$ = "" THEN 296
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I + 560) = I1
     LOCATE 11, 46: PRINT "POSITION (2-9) ";
281  I$ = INKEY$: IF I$ = "" THEN 281
     I1 = VAL(I$): IF I1 < 2 OR I1 > 9 THEN 281
     MG%(I + 576) = I1
     LOCATE 12, 46: PRINT "SELECT SUB #:";
282 I$ = INKEY$: IF I$ = "" THEN 282
    GOSUB 1290
    I1 = VAL(I$)
    MG%(I + 564) = I1
283 LOCATE 13, 46: INPUT "ENTER % OF TIME TO USE "; MG%(I + 568)
     IF MG%(I + 568) < 0 OR MG%(I + 568) > 100 THEN 283
     LOCATE 14, 46: PRINT "NUMBER OF RUNS AHEAD"
     LOCATE 15, 46: INPUT "0,1,2 OR MORE "; I1
     IF I1 < 0 THEN I1 = 0
     IF I1 > 2 THEN I1 = 2
     MG%(I + 572) = I1
     RETURN
298 LOCATE 10, 46: INPUT "SELECT CHOICE TO DELETE: "; I
     IF I < 1 OR I > 4 THEN 298
     MG%(I + 560) = -1: MG%(I + 564) = 0: MG%(I + 568) = 0: MG%(I + 572) = 0: MG%(I + 576) = 0
     RETURN
300 REM:ANALYZE PLAYER USAGE
    LH% = 0: RH% = 0
    FOR I = 0 TO 22: FOR I1 = 42 TO 44
    B%(I, I1) = 0
    NEXT: NEXT
    FOR I = 10 TO 19
    IF MG%(120 + I * 21) <> 999 THEN 305
    LH% = LH% + MG%(100 + I * 21)
305 NEXT
    FOR I = 0 TO 9
    IF MG%(120 + I * 21) <> 999 THEN 310
    RH% = RH% + MG%(100 + I * 21)
310 NEXT
    FOR I = 0 TO 9
    IF MG%(120 + I * 21) <> 999 THEN 320
    FOR I1 = 1 TO 9
    IF MG%(101 + I1 + I * 21) = 1 THEN 315
    FRAC = MG%(100 + I * 21) / RH%
    B%(MG%(110 + I1 + I * 21), 42) = B%(MG%(110 + I1 + I * 21), 42) + (AB%(I1) * FRAC) * .7
    B%(MG%(110 + I1 + I * 21), 44) = B%(MG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC) * .7
315 NEXT I1
320 NEXT I
    FOR I = 10 TO 19
    IF MG%(120 + I * 21) <> 999 THEN 325
    FOR I1 = 1 TO 9
    IF MG%(101 + I1 + I * 21) = 1 THEN 330
    FRAC = MG%(100 + I * 21) / LH%
    B%(MG%(110 + I1 + I * 21), 43) = B%(MG%(110 + I1 + I * 21), 43) + (AB%(I1) * FRAC) * .3
    B%(MG%(110 + I1 + I * 21), 44) = B%(MG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC) * .3
330 NEXT I1
325 NEXT I
350 CLS : COLOR 14
    PRINT "# "; : COLOR L%(11), L%(12): PRINT T$; : COLOR 14, 0
    LOCATE , 18: PRINT "B  POS  ACTUAL AB  PROJECTED: AB/LHP  AB/RHP  TOT/AB"
    LOCATE 2
    I1 = 0
    FOR I = 0 TO 22: LOCATE , 1
    IF B$(I) = "XXX" OR LEFT$(B$(I), 1) = " " OR B$(I) = "" THEN I1 = I1 + 1: GOTO 355
    COLOR 2, 0
    LOCATE I + 2 - I1, 1
    IF I < 10 THEN PRINT USING "#"; I; : COLOR 15, 0: GOSUB 1980
    IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0: GOSUB 1980
    LOCATE , 3: PRINT B$(I); : COLOR 15, 0: LOCATE , 18: PRINT B1$(B%(I, 0) + 2);
    LOCATE , 21: IF B%(I, 22) <> -1 THEN PRINT USING "#"; B%(I, 22);
    LOCATE , 22: IF B%(I, 23) <> -1 THEN PRINT USING "#"; B%(I, 23);
    LOCATE , 23: IF B%(I, 24) <> -1 THEN PRINT USING "#"; B%(I, 24);
    LOCATE , 24: IF B%(I, 25) <> -1 THEN PRINT USING "#"; B%(I, 25);
    LOCATE , 27
    PRINT USING "   ###                ###     ###      ###"; B%(I, 4); B%(I, 43); B%(I, 42); B%(I, 44);
355 NEXT
    GOSUB 4095
    GOTO 100
480 CLS
    LOCATE 3, 1
    COLOR L%(11), L%(12): PRINT T$; : COLOR 15, 0: PRINT " LINEUPS VS "; PT$(Q2%): PRINT

    ITEM$(1) = "INPUT A NEW LINEUP            "
    ITEM$(2) = "EDIT AN EXISTING LINEUP       "
    ITEM$(3) = "DELETE AN EXISTING LINEUP     "
    ITEM$(4) = "RETURN TO LINEUP STRATEGY MENU"
   SELECTION% = 1
   Count% = 4
   Start% = 1
   SCANCODE% = 4
   FileColr% = 31
   HiLiteColr% = 112
   Spacing% = 1
   Row% = 5
   Column% = 13
   CALL Box0(4, 12, 9, 43, 2, 31)
   CALL SHOWCURSOR
   MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
   CALL HIDECURSOR
   COLOR , 0
    Q3% = SELECTION% - 1
    IF Q3% = 3 OR SCANCODE% = 27 THEN 100
    IF Q3% = 1 OR Q3% = 2 THEN 495
    FOR I = 1 TO 9: B3%(I) = -1: B7%(I) = -1: NEXT
    LOCATE 12, 1: PRINT "IS A DH TO BE USED IN THIS LINEUP (YN)"
492 I$ = INKEY$: IF I$ = "" THEN 492
    D% = 0
    IF UCASE$(I$) = "Y" THEN D% = 1
495 IF Q3% = 0 THEN 496
    GOSUB 2500
    LF% = 0
    FOR I = 0 TO 9
    IF MG%(120 + (I + Q2% * 10) * 21) = 999 THEN LF% = LF% + 1
    NEXT
    IF LF% = 0 THEN LOCATE 23, 1: COLOR 12: PRINT "NO LINEUPS FOUND! HIT ANY KEY TO RETURN TO MENU": COLOR 15: GOSUB 4095: GOTO 480
496 LOCATE 23, 1
    PRINT "CHOOSE LINEUP NUMBER (0-9) OR <ESC> TO EXIT ";
497 I$ = INKEY$: IF I$ = "" THEN 497
    IF I$ = CHR$(27) THEN 480
    LN% = VAL(I$)
    IF LN% < 0 OR LN% > 9 OR Q3% = 1 AND MG%(120 + (LN% + Q2% * 10) * 21) <> 999 THEN 495
    IF Q2% = 1 THEN LN% = LN% + 10
    IF Q3% = 2 THEN MG%(120 + LN% * 21) = 0: MG%(100 + LN% * 21) = 0: GOTO 480
500 IF Q3% <> 1 THEN GOSUB 1100: LOCATE 6, 64: PRINT "VS " + PT$(Q2%): LOCATE 7, 61: PRINT USING "LINEUP ##   POS"; LN% - Q2% * 10: GOSUB 2310: GOSUB 540
510 IF Q3% = 1 THEN GOSUB 1830
520 IF RP = 1 THEN RP = 0: GOTO 480
525 REM:GOSUB 2390
530 W5 = 0: W6 = 1: V%(0) = 1: V1%(0) = 9: V%(1) = 1: V1%(1) = 9
    GOTO 480
540 FOR I = 0 TO 9: M%(I) = 0: NEXT I: GOSUB 1370
550 FOR I = 1 TO 9
560 GOSUB 1010: PRINT "                  "; : GOSUB 1010
561 I$ = INKEY$: IF I$ = "" THEN 561
570 GOSUB 1290
580 I1 = VAL(I$): IF UCASE$(I$) = "X" THEN 540
590 FOR J = 1 TO 9: IF J = I OR B3%(J) = I1 AND B7%(J) = 1 THEN 610
600 IF B3%(J) = I1 AND UCASE$(I$) <> "P" THEN 560
610 NEXT J
620 IF D% = 1 AND UCASE$(I$) = "P" THEN 560
630 IF B%(I1, 21) <> 0 AND UCASE$(I$) <> "P" THEN 560
640 IF UCASE$(I$) = "P" THEN PRINT USING "# "; I; : GOTO 760
650 B3%(I) = I1: PRINT USING "# "; I; : PRINT B$(B3%(I));
660 LOCATE , 74
680 I$ = INKEY$: IF I$ = "" THEN 680
690 IF VAL(I$) = 1 THEN 680
700 FOR J = 1 TO 9: IF J = I THEN 720
710 IF VAL(I$) = B7%(J) THEN 680
720 NEXT J
730 IF D% = 0 AND VAL(I$) = 0 OR UCASE$(I$) = "X" THEN 560
740 PRINT C$(VAL(I$)); : B7%(I) = VAL(I$)
750 B9%(B7%(I)) = B3%(I): M%(VAL(I$)) = 1
751 LOCATE I1 + 2, 2: PRINT "*"; : COLOR L%(11), L%(12): PRINT B$(I1): COLOR 15, 0: GOSUB 1370: GOTO 830
760 FOR J = 1 TO 9: IF J = I THEN 780
770 IF B3%(J) = P1% AND B7%(J) = 1 THEN 560
780 NEXT J
790 B3%(I) = P1%: PRINT B$(23); : M%(VAL(I$)) = 1
800 LOCATE , 72
820 PRINT "  p "; : B7%(I) = 1: B9%(1) = P1%: M%(1) = 1: GOSUB 1370
830 IF C1 = 1 THEN 850
840 NEXT I
850 C1 = 1: LOCATE 25, 1: COLOR 14: PRINT "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
    COLOR 15, 0
860 I$ = INKEY$: IF I$ = "" THEN 860
861 IF UCASE$(I$) = "R" THEN FOR J = 1 TO 9: M%(J) = 0: B7%(J) = -1: B3%(J) = -1: NEXT: C1 = 0: FOR J = 0 TO 22: B%(J, 21) = 0: NEXT: RP = 1: GOTO 1001
    IF UCASE$(I$) = "L" THEN GOSUB 1440: GOTO 850
    IF UCASE$(I$) = "P" THEN GOSUB 1430: GOTO 850
870 I = VAL(I$): IF I < 1 OR I > 9 THEN 890
880 LOCATE B3%(I) + 2 - B4%, 2: PRINT " "; B$(B3%(I)): GOTO 560


890 PRINT : INPUT "WHAT PERCENT OF TIME IS THIS LINEUP TO BE USED (1-100) "; LU%
    IF LU% < 1 THEN LU% = 1
    IF LU% > 100 THEN LU% = 100
    MG%(100 + LN% * 21) = LU%
    MG%(101 + LN% * 21) = D%
    FOR I = 1 TO 9
    MG%(101 + I + LN% * 21) = B7%(I)
    MG%(110 + I + LN% * 21) = B3%(I)
    NEXT
    MG%(120 + LN% * 21) = 999
    MG%(0) = 999
    CALL UPDATEFILE
900 GOSUB 1200
    C1 = 0
    FOR I = 0 TO 9: B3%(I) = -1: B7%(I) = -1: NEXT
910 RETURN

920 FOR I = 1 TO 9: GOSUB 1010
    PRINT USING "# "; I;
940 IF B7%(I) = 1 AND I5 <> 66 THEN PRINT B$(23);
950 IF B7%(I) <> 1 THEN PRINT B$(B3%(I));
960 PRINT TAB(74);
980 PRINT C$(B7%(I));
990 NEXT I
1000 I5 = 0: RETURN
1001 FOR J = 0 TO 22: IF B%(J, 31) <> 0 THEN B%(J, 21) = 99
     NEXT
     GOTO 910
1010 LOCATE I + 7, 59
1050 RETURN
1100 CLS : COLOR 14: PRINT "#"; : COLOR L%(11), L%(12): PRINT T$; : COLOR 14, 0
     LOCATE , 15: PRINT "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"
     LOCATE 2: FOR I = 0 TO 22: LOCATE , 1
1111 IF B$(I) = "XXX" OR LEFT$(B$(I), 1) = " " OR B$(I) = "" THEN B%(I, 21) = 98: GOTO 1191
     COLOR 2, 0
     IF I < 10 THEN PRINT USING "#"; I; : COLOR 15, 0: GOSUB 1980
     IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0: GOSUB 1980
     LOCATE , 3: PRINT B$(I); : COLOR 15, 0: LOCATE , 15: PRINT B1$(B%(I, 0) + 2);
1141 LOCATE , 18: IF B%(I, 22) <> -1 THEN PRINT USING "#"; B%(I, 22);
1142 LOCATE , 19: IF B%(I, 23) <> -1 THEN PRINT USING "#"; B%(I, 23);
1143 LOCATE , 20: IF B%(I, 24) <> -1 THEN PRINT USING "#"; B%(I, 24);
1144 LOCATE , 21: IF B%(I, 25) <> -1 THEN PRINT USING "#"; B%(I, 25);
1160 LOCATE , 23
1161 PRINT USING "### ## ## ## ### ###"; B%(I, 4); B%(I, 7); B%(I, 8); B%(I, 9); B%(I, 11); B%(I, 12);
1170 LOCATE , 44: PRINT USING "#"; B%(I, 16); : PRINT USING "#"; B%(I, 34); : PRINT USING "#"; B%(I, 33);
1180 LOCATE , 47
1181 PRINT USING "### ##"; B%(I, 13); B%(I, 18);
1182 IF B%(I, 6) = B%(I, 4) AND B%(I, 6) <> 0 THEN PRINT "1.000"; : GOTO 1190
1183 IF B%(I, 4) = 0 THEN PRINT " .---"; : GOTO 1190
1184 PRINT USING " .###"; B%(I, 6) / B%(I, 4);
1190 IF I <> 22 THEN PRINT
1191 NEXT I: RETURN
1200 FOR I = 1 TO 9
1210 IF B7%(I) = 1 THEN 1230
1220 REM:B%(B3%(I), 21) = 1
1230 M%(I) = 0: NEXT I
1240 M%(0) = 0: RETURN

1370 LOCATE 25, 1: FOR I1 = 0 TO 9: IF D% = 0 AND I1 = 0 THEN 1410
1380 IF D% = 1 AND I1 = 1 THEN 1410
1390 IF M%(I1) = 0 THEN PRINT I1; " ";
1400 IF M%(I1) = 1 THEN PRINT "  ";
1410 NEXT I1
1420 RETURN

1430 LOCATE 18, 59: PRINT "B.O. PLYR 1 (1-9)";
1431 I$ = INKEY$: IF I$ = "" THEN 1431
     K1 = VAL(I$): IF K1 < 1 OR K1 > 9 THEN 1431
     IF B7%(K1) = 1 THEN 1431
     LOCATE 19, 59: PRINT "B.O. PLYR 2 (1-9)";
1432 I$ = INKEY$: IF I$ = "" THEN 1432
     K2 = VAL(I$): IF K2 < 1 OR K2 > 9 THEN 1432
     IF B7%(K2) = 1 THEN 1432
     SWAP B7%(K2), B7%(K1)
     B9%(B7%(K1)) = B3%(K1)
     B9%(B7%(K2)) = B3%(K2)
     GOSUB 920
     LOCATE 18, 59: PRINT "                 "
     LOCATE 19, 59: PRINT "                 "
     RETURN
1440 LOCATE 18, 59: PRINT "B.O. PLYR 1 (1-9)";
1441 I$ = INKEY$: IF I$ = "" THEN 1441
     K1 = VAL(I$): IF K1 < 1 OR K1 > 9 THEN 1441
     REM:IF B7%(K1)=1 THEN 1441
     LOCATE 19, 59: PRINT "B.O. PLYR 2 (1-9)";
1442 I$ = INKEY$: IF I$ = "" THEN 1442
     K2 = VAL(I$): IF K2 < 1 OR K2 > 9 THEN 1442
     REM:IF B7%(K2)=1 THEN 1442
     SWAP B7%(K2), B7%(K1)
     SWAP B3%(K1), B3%(K2)
     B9%(B7%(K1)) = B3%(K1)
     B9%(B7%(K2)) = B3%(K2)
     GOSUB 920
     LOCATE 18, 59: PRINT "                 "
     LOCATE 19, 59: PRINT "                 "
     RETURN
1830 GOSUB 1100: GOSUB 2310
     LU% = MG%(100 + LN% * 21)
     D% = MG%(101 + LN% * 21)
     FOR I = 1 TO 9
     B3%(I) = MG%(110 + I + LN% * 21)
     B7%(I) = MG%(101 + I + LN% * 21)
     NEXT
1910 FOR I = 1 TO 9: B9%(B7%(I)) = B3%(I)
1912 IF B7%(I) = 1 THEN B3%(I) = P1%: B9%(1) = P1%: GOTO 1925
1920 B%(B3%(I), 21) = 0
1925 NEXT I
1930 GOSUB 2310: GOSUB 920: GOSUB 1940: GOTO 850
1940 FOR I2 = 0 TO 22: LOCATE I2 + 2, 2: FOR I3 = 1 TO 9
1941 IF B3%(I3) = I2 AND B7%(I3) <> 1 THEN PRINT "*"; : COLOR L%(11), L%(12): PRINT B$(I2): COLOR 15, 0
1950 NEXT I3: NEXT I2
1960 FOR I3 = 0 TO 9: M%(I3) = 1: NEXT I3
1970 RETURN
1980 IF B%(I, 21) >= 98 THEN COLOR 0, 7
1990 RETURN
2000 IF P%(I, 15) >= 98 THEN COLOR 0, 7
2010 RETURN


2310 FOR I = 10 TO 22: LOCATE I, 59: PRINT "                     "; : NEXT: RETURN
2500 CLS
     COLOR 12: PRINT TAB(30); "LINEUPS FOUND VS. "; PT$(Q2%): COLOR 14
     PRINT " #0    "; MG%(100 + (0 + Q2% * 10) * 21); "%";
     LOCATE , 17: PRINT " #1    "; MG%(100 + (1 + Q2% * 10) * 21); "%";
     LOCATE , 33: PRINT " #2    "; MG%(100 + (2 + Q2% * 10) * 21); "%";
     LOCATE , 49: PRINT " #3    "; MG%(100 + (3 + Q2% * 10) * 21); "%";
     LOCATE , 65: PRINT " #4    "; MG%(100 + (4 + Q2% * 10) * 21); "%"
     COLOR 15
     FOR I = 0 TO 4
     FOR I1 = 1 TO 9
     IF MG%(120 + (I + Q2% * 10) * 21) <> 999 AND I1 = 1 THEN LOCATE 3, 16 * I + 1: COLOR 2: PRINT "NONE FOUND": COLOR 15: GOTO 2510
     IF MG%(120 + (I + Q2% * 10) * 21) <> 999 AND I1 <> 1 THEN LOCATE 2 + I1, 16 * I + 1: PRINT "         ": GOTO 2510
     LOCATE 2 + I1, 16 * I + 1
     IF MG%(101 + I1 + (I + Q2% * 10) * 21) = 1 THEN PRINT "PITCHER"; : GOTO 2512
     PRINT LEFT$(B$(MG%(110 + I1 + (I + Q2% * 10) * 21)), 10);
2512 LOCATE 2 + I1, 16 * I + 12: PRINT C$(MG%(101 + I1 + (I + Q2% * 10) * 21))
2510 NEXT
     NEXT
     PRINT
     COLOR 14: PRINT " #5    "; MG%(100 + (5 + Q2% * 10) * 21); "%";
     LOCATE , 17: PRINT " #6    "; MG%(100 + (6 + Q2% * 10) * 21); "%";
     LOCATE , 33: PRINT " #7    "; MG%(100 + (7 + Q2% * 10) * 21); "%";
     LOCATE , 49: PRINT " #8    "; MG%(100 + (8 + Q2% * 10) * 21); "%";
     LOCATE , 65: PRINT " #9    "; MG%(100 + (9 + Q2% * 10) * 21); "%"
     COLOR 15
     FOR I = 5 TO 9
     FOR I1 = 1 TO 9
     IF MG%(120 + (I + Q2% * 10) * 21) <> 999 AND I1 = 1 THEN LOCATE 14, 16 * (I - 5) + 1: COLOR 2: PRINT "NONE FOUND": COLOR 15: GOTO 2505
     IF MG%(120 + (I + Q2% * 10) * 21) <> 999 AND I1 <> 1 THEN LOCATE 13 + I1, 16 * (I - 5) + 1: PRINT "         ": GOTO 2505
     LOCATE 13 + I1, 16 * (I - 5) + 1
     IF MG%(101 + I1 + (I + Q2% * 10) * 21) = 1 THEN PRINT "PITCHER"; : GOTO 2507
     PRINT LEFT$(B$(MG%(110 + I1 + (I + Q2% * 10) * 21)), 10);
2507 LOCATE 13 + I1, 16 * (I - 5) + 12: PRINT C$(MG%(101 + I1 + (I + Q2% * 10) * 21))
2505 NEXT
     NEXT
     RETURN


2100 NT = 0
     OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0)
     FOR I1 = 1 TO Y / 4342
     GET #1, I1
     FOR I2 = 1 TO 15
     IF LEFT$(Q$(0),2) = "XX" OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1105
     NEXT I2
     NT = NT + 1
     VV$(NT) = Q$(0): V%(NT) = I1
1105 NEXT I1
     CLOSE 1
1102 SELECTION% = 1
     Count% = NT
     Start% = 1
     FileColr% = 79
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
CALL SHOWCURSOR
MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     T = V%(SELECTION%)
     LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
1195 GOTO 1485

1445 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
    LOCATE 7, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 1102: GOTO 182
    YN$ = NY$
    FOR I = 1 TO 500: VV$(I) = "": V%(I) = 1: NEXT
    GOSUB 2100
182 IF BO% = 1 THEN BO% = 0: GOTO 1445
183 LOCATE 2, 33: COLOR L%(11), L%(12): PRINT T$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 1445
    COLOR 15, 0
    RETURN
1290 IF UCASE$(I$) = "A" THEN I$ = "10"
1300 IF UCASE$(I$) = "B" THEN I$ = "11"
1310 IF UCASE$(I$) = "C" THEN I$ = "12"
1320 IF UCASE$(I$) = "D" THEN I$ = "13"
1330 IF UCASE$(I$) = "E" THEN I$ = "14"
1340 IF UCASE$(I$) = "F" THEN I$ = "15"
1350 IF UCASE$(I$) = "G" THEN I$ = "16"
1351 IF UCASE$(I$) = "H" THEN I$ = "17"
1352 IF UCASE$(I$) = "I" THEN I$ = "18"
1353 IF UCASE$(I$) = "J" THEN I$ = "19"
1354 IF UCASE$(I$) = "K" THEN I$ = "20"
1355 IF UCASE$(I$) = "L" THEN I$ = "21"
1356 IF UCASE$(I$) = "M" THEN I$ = "22"
1360 RETURN
1485  IF YN$ = "" OR NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN BO% = 1: RETURN
      OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
FOR I = 0 TO 22
FIELD #1, 45 + I * 99 AS X$, 15 AS Q$(I * 43 + 15), 2 AS Q$(I * 43 + 16), 2 AS Q$(I * 43 + 17), 2 AS Q$(I * 43 + 18), 2 AS Q$(I * 43 + 19), 2 AS Q$(I * 43 + 20), 2 AS Q$(I * 43 + 21), 2 AS Q$(I * 43 + 22), 2 AS Q$(I * 43 + 23), 2 AS Q$(I * 43 + 24) _
, 2 AS Q$(I * 43 + 25), 2 AS Q$(I * 43 + 26), 4297 - 37 - I * 99 AS X$
FIELD #1, 82 + I * 99 AS X$, 2 AS Q$(I * 43 + 27), 2 AS Q$(I * 43 + 28), 2 AS Q$(I * 43 + 29), 2 AS Q$(I * 43 + 30), 2 AS Q$(I * 43 + 31), 2 AS Q$(I * 43 + 32), 2 AS Q$(I * 43 + 33), 2 AS Q$(I * 43 + 34), 2 AS Q$(I * 43 + 35), 2 AS Q$(I * 43 + 36),  _
2 AS Q$(I * 43 + 37), 2 AS Q$(I * 43 + 38), 4297 - 61 - I * 99 AS X$
FIELD #1, 106 + I * 99 AS X$, 2 AS Q$(I * 43 + 39), 2 AS Q$(I * 43 + 40), 2 AS Q$(I * 43 + 41), 2 AS Q$(I * 43 + 42), 2 AS Q$(I * 43 + 43), 2 AS Q$(I * 43 + 44), 2 AS Q$(I * 43 + 45), 2 AS Q$(I * 43 + 46), 2 AS Q$(I * 43 + 47), 2 AS Q$(I * 43 + 48) _
, 2 AS Q$(I * 43 + 49), 2 AS Q$(I * 43 + 50), 4297 - 85 - I * 99 AS X$
FIELD #1, 130 + I * 99 AS X$, 2 AS Q$(I * 43 + 51), 2 AS Q$(I * 43 + 52), 2 AS Q$(I * 43 + 53), 2 AS Q$(I * 43 + 54), 2 AS Q$(I * 43 + 55), 2 AS Q$(I * 43 + 56), 2 AS Q$(I * 43 + 57), 4297 - 99 - I * 99 AS X$
NEXT
FOR I = 0 TO 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
 AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015), 2020 - 37 - I * 89 AS X$
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027), 2020 - 61 - I * 89 AS X$
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039), 2020 - 85 - I * 89 AS X$
FIELD #1, 2407 + I * 89 AS X$, 2 AS Q$(I * 38 + 1040), 2 AS Q$(I * 38 + 1041), 2020 - 89 - I * 89 AS X$
NEXT
FIELD #1, 4280 AS X$, 40 AS Q$(1840), 15 AS Q$(1841), 5 AS Q$(1842), 2 AS Q$(1843)
GET #1, T
T$ = Q$(0)
1532 IF RIGHT$(T$, 1) = " " THEN T$ = LEFT$(T$, LEN(T$) - 1): GOTO 1532
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT: K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = Q$(I * 43 + 15)
1533 IF RIGHT$(B$(I), 1) = " " THEN B$(I) = LEFT$(B$(I), LEN(B$(I)) - 1): GOTO 1533
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
    IF B%(I, 0) = -1 THEN PB$(I) = "L"
    IF B%(I, 0) = 0 THEN PB$(I) = "S"
    IF B%(I, 0) = 1 THEN PB$(I) = "R"
    IF B%(I, 0) = 2 THEN PB$(I) = "RP"
    IF B%(I, 0) = -2 THEN PB$(I) = "LP"
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = Q$(I * 38 + 1004)
1534 IF RIGHT$(P$(I), 1) = " " THEN P$(I) = LEFT$(P$(I), LEN(P$(I)) - 1): GOTO 1534
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = Q$(1840): M$ = Q$(1841): NN$ = Q$(1842): CK = CVI(Q$(1843))
CLOSE 1
1535 IF RIGHT$(M$, 1) = " " THEN M$ = LEFT$(M$, LEN(M$) - 1): GOTO 1535
1536 IF RIGHT$(S$, 1) = " " THEN S$ = LEFT$(S$, LEN(S$) - 1): GOTO 1536
1537 IF RIGHT$(NN$, 1) = " " THEN NN$ = LEFT$(NN$, LEN(NN$) - 1): GOTO 1537
     RETURN
4000 REM: PRINT MANAGER PROFILE
     LPRINT "MANAGERIAL PROFILE FOR " T$
     LPRINT
     Q2% = 0
     LF% = 0
     FOR I = 0 TO 9
     IF MG%(120 + (I + Q2% * 10) * 21) = 999 THEN LF% = LF% + 1
     NEXT
     IF LF% = 0 THEN LPRINT "NO LINEUPS VS RHP ":GOTO 4010
     GOSUB 2500
     CALL PRTSC0 (1)
4010 Q2% = 1
     LF% = 0
     FOR I = 0 TO 9
     IF MG%(120 + (I + Q2% * 10) * 21) = 999 THEN LF% = LF% + 1
     NEXT
     IF LF% = 0 THEN LPRINT  "NO LINEUPS VS LHP ":GOTO 4020
     GOSUB 2500
     CALL PRTSC0 (1)
4020 LPRINT
     IF MG%(99) <> 999 THEN LPRINT "NO STARTING PITCHING ROTATION DEFINED"
     LPRINT "SIZE OF ROTATION IS "; MG%(1);" MAN"
     LPRINT "    STARTING ROTATION     % STARTS"
     FOR I = 1 TO MG%(1)
     LPRINT USING "(#) "; I;
     IF MG%(I + 2) < 0 THEN LPRINT : GOTO 4021
     LPRINT P$(MG%(I + 2)); TAB(30); MG%(I + 12)
4021 NEXT
     LPRINT
     LPRINT "     SPOT STARTERS         % USED"
     FOR I = 21 TO 36
     LPRINT USING "(##) "; I - 20;
     IF MG%(I) < 0 THEN LPRINT : GOTO 4022
     LPRINT P$(MG%(I)); TAB(30); MG%(I + 20)
4022 NEXT
     LPRINT
     LPRINT "  MIDDLE RELIEVERS      % APPEAR"
     FOR I = 1 TO 10
     LPRINT USING "(##) "; I;
     IF MG%(I + 60) < 0 THEN LPRINT : GOTO 4023
     LPRINT P$(MG%(I + 60)); TAB(35); MG%(I + 70)
4023 NEXT
     LPRINT
     LPRINT "  SET-UP RELIEVERS      % APPEAR"
     LPRINT "   -----  VS LEFT  -------      "
     FOR I = 1 TO 3
     LPRINT USING "(##) "; I;
     IF MG%(I + 80) < 0 THEN LPRINT : GOTO 4050
     LPRINT P$(MG%(I + 80)); TAB(35); MG%(I + 90)
4050 NEXT
     LPRINT "   -----  VS RIGHT  -------      "
     FOR I = 4 TO 6
     LPRINT USING "(##) "; I;
     IF MG%(I + 80) < 0 THEN LPRINT : GOTO 4051
     LPRINT P$(MG%(I + 80)); TAB(35); MG%(I + 90)
4051 NEXT
     LPRINT
     LPRINT "          CLOSERS       % APPEAR"
     LPRINT "   -----  VS LEFT  -------      "
     FOR I = 1 TO 3
     LPRINT USING "(##) "; I;
     IF MG%(I + 520) < 0 THEN LPRINT : GOTO 4024
     LPRINT P$(MG%(I + 520)); TAB(35); MG%(I + 530)
4024 NEXT
     LPRINT "   -----  VS RIGHT  -------      "
     FOR I = 4 TO 6
     LPRINT USING "(##) "; I;
     IF MG%(I + 520) < 0 THEN LPRINT : GOTO 4025
     LPRINT P$(MG%(I + 520)); TAB(35); MG%(I + 530)
4025 NEXT
     LPRINT
     LPRINT "  PINCH HITTERS        "
     LPRINT "  ---- VS LHP  ----"
     FOR I = 1 TO 5
     LPRINT USING "(##) "; I;
     IF MG%(I + 540) < 0 THEN LPRINT : GOTO 4026
     LPRINT B$(MG%(I + 540))
4026 NEXT
     LPRINT "  ---- VS RHP  ----"
     FOR I = 6 TO 10
     LPRINT USING "(##) "; I;
     IF MG%(I + 540) < 0 THEN LPRINT : GOTO 4027
     LPRINT B$(MG%(I + 540))
4027 NEXT
     LPRINT
     LPRINT "DEFENSIVE REPLACEMENTS"
     LPRINT "# PLAYER     P SUBSTITUTE  %  RA"
     FOR I = 1 TO 4
     LPRINT USING "# "; I;
     IF MG%(I + 560) < 0 THEN LPRINT : GOTO 4028
     LPRINT LEFT$(B$(MG%(I + 560)), 10); TAB(19);
     LPRINT USING "# "; MG%(I + 576);
     LPRINT LEFT$(B$(MG%(I + 564)), 10); TAB(32); USING "### #"; MG%(I + 568); MG%(I + 572)
4028 NEXT
     LPRINT
    IF MG%(581) <> 999 THEN LPRINT "NO CLOSING STRATEGY HAS BEEN DEFINED":LPRINT:GOTO 4029
    LPRINT TAB(31); "CLOSING STRATEGY"
    LPRINT
    LPRINT "STRATEGY                                        % OF TIME"
    LPRINT "(1) INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; MG%(588)
    LPRINT "(2) USE BOTH SETUP RELIEVER AND CLOSER "; YN$(MG%(589)): LPRINT
    LPRINT "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
    LPRINT "(3) ";:LPRINT USING "START INNING IF SAVE OPPORTUNITY               ###"; MG%(582)
    LPRINT "(4) ";:LPRINT USING "START INNING IF AHEAD                          ###"; MG%(583)
    LPRINT "(5) ";:LPRINT USING "START INNING IF TIED OR AHEAD                  ###"; MG%(584)
    LPRINT "(6) ";:LPRINT USING "IF CURRENT PITCHER TIRED AND AHEAD             ###"; MG%(585)
    LPRINT "(7) ";:LPRINT USING "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; MG%(586)
    LPRINT "(8) ";:LPRINT USING "WHEN SAVE OPPORTUNITY OCCURS                   ###"; MG%(587)
4029 LPRINT
     IF MG%(590) <> 999 THEN LPRINT "NO PINCH HITTING FOR THE PITCHER STRATEGY DEFINED":GOTO 4030
     LPRINT TAB(26) "PINCH HITTING FOR THE PITCHER"
     LPRINT
     LPRINT "PINCH HIT FOR THE PITCHER FROM 'A' INNING ON"
     LPRINT "IF DOWN BY 'B' OR MORE RUNS AND"
     LPRINT "'C' OR MORE RUNNERS ON BASE AND"
     LPRINT "'D' OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN 'E'"
     LPRINT "'F' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO        A      B      C      D     E     F"
     FOR I = 0 TO 2
     LPRINT USING "   #           ##     ##      #      #    ###   ###"; I + 1; MG%(I * 6 + 591); MG%(I * 6 + 592); MG%(I * 6 + 593); MG%(I * 6 + 594); MG%(I * 6 + 595); MG%(I * 6 + 596)
     NEXT
4030 LPRINT
     IF MG%(609) <> 999 THEN LPRINT "NO BUNTING STRATEGY DEFINED FOR THE PITCHER":GOTO 4031
     LPRINT "SACRIFICE BUNTING FOR THE PITCHER":LPRINT
     LPRINT "'A' OUTS OR LESS"
     LPRINT "IF 'B' OR LESS RUNS AHEAD AND"
     LPRINT "'C' OR LESS RUNS BEHIND"
     LPRINT "'D' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                       A      B     C      D"
     LPRINT "1 - 7TH INNING OR LESS         "; USING "#     ##    ##     ###"; MG%(610); MG%(611); MG%(612); MG%(613)
     LPRINT "    RUNNER ON 1ST           "
     LPRINT "2 - 8TH INNING ON              "; USING "#     ##    ##     ###"; MG%(614); MG%(615); MG%(616); MG%(617)
     LPRINT "    RUNNER ON 1ST           "
     LPRINT "3 - 7TH INNING OR LESS         "; USING "#     ##    ##     ###"; MG%(618); MG%(619); MG%(620); MG%(621)
     LPRINT "    RUNNER ON 2ND           "
     LPRINT "4 - 8TH INNING ON              "; USING "#     ##    ##     ###"; MG%(622); MG%(623); MG%(624); MG%(625)
     LPRINT "    RUNNER ON 2ND           "
     LPRINT "5 - 7TH INNING OR LESS         "; USING "#     ##    ##     ###"; MG%(626); MG%(627); MG%(628); MG%(629)
     LPRINT "    RUNNERS ON 1ST AND 2ND  "
     LPRINT "6 - 8TH INNING ON              "; USING "#     ##    ##     ###"; MG%(630); MG%(631); MG%(632); MG%(633)
     LPRINT "    RUNNERS ON 1ST AND 2ND  "
4031 LPRINT
     IF MG%(634) <> 999 THEN LPRINT "NO BUNTING STRATEGY DEFINED FOR POSITION PLAYERS":GOTO 4032
     LPRINT "SACRIFICE BUNTING FOR THE POSITION PLAYERS":LPRINT
     LPRINT "IF 'A' OR LESS RUNS AHEAD AND"
     LPRINT "'B' OR LESS RUNS BEHIND"
     LPRINT "BATTING AVERAGE IS LESS THAN 'C' AND SLUGGING PCT IS LESS THAN 'D'"
     LPRINT "PITCHERS ERA LESS THAN 'E' , 'F' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                        A     B      C      D      E     F"
     LPRINT "1 - 7TH INNING OR LESS         "; USING "##    ##    ###    ###    ###   ###"; MG%(635); MG%(636); MG%(637); MG%(638); MG%(639); MG%(640)
     LPRINT "    0 OUTS, RUNNER ON 1ST"
     LPRINT "2 - 7TH INNING OR LESS         "; USING "##    ##    ###    ###    ###   ###"; MG%(641); MG%(642); MG%(643); MG%(644); MG%(645); MG%(646)
     LPRINT "    0 OUTS, RUNNER ON 2ND"
     LPRINT "3 - 7TH INNING OR LESS         "; USING "##    ##    ###    ###    ###   ###"; MG%(647); MG%(648); MG%(649); MG%(650); MG%(651); MG%(652)
     LPRINT "    0 OUTS, RUNNERS 1ST AND 2ND"
     LPRINT "4 - 8TH INNING ON              "; USING "##    ##    ###    ###    ###   ###"; MG%(653); MG%(654); MG%(655); MG%(656); MG%(657); MG%(658)
     LPRINT "    0 OUTS, RUNNER ON 1ST"
     LPRINT "5 - 8TH INNING ON              "; USING "##    ##    ###    ###    ###   ###"; MG%(659); MG%(660); MG%(661); MG%(662); MG%(663); MG%(664)
     LPRINT "    0 OUTS, RUNNER ON 2ND"
     LPRINT "6 - 8TH INNING ON              "; USING "##    ##    ###    ###    ###   ###"; MG%(665); MG%(666); MG%(667); MG%(668); MG%(669); MG%(670)
     LPRINT "    0 OUTS, RUNNERS ON 1ST AND 2ND"
4032 LPRINT
     IF MG%(671) <> 999 THEN LPRINT "NO HIT AND RUN STRATEGY DEFINED":GOTO 4033
     LPRINT "HIT AND RUN STRATEGY": LPRINT
     LPRINT "IF 'A' OR LESS RUNS AHEAD AND"
     LPRINT "'B' OR LESS RUNS BEHIND"
     LPRINT "'C' % OF THE TIME"
     LPRINT "'D' AB PER STRIKEOUT"
     LPRINT " SLUGGING PCT LESS THAN OR EQUAL TO 'E'"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "    SCENARIO                          A     B      C   D    E"
     LPRINT "1 - 0 OUTS, RUNNER ON 1ST            "; USING "##    ##    ###  ##  ###"; MG%(672); MG%(673); MG%(674); MG%(866); MG%(872)
     LPRINT "2 - 1 OUT, RUNNER ON 1ST             "; USING "##    ##    ###  ##  ###"; MG%(675); MG%(676); MG%(677); MG%(867); MG%(873)
     LPRINT "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; USING "##    ##    ###  ##  ###"; MG%(678); MG%(679); MG%(680); MG%(868); MG%(874)
     LPRINT "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; USING "##    ##    ###  ##  ###"; MG%(681); MG%(682); MG%(683); MG%(869); MG%(875)
     LPRINT "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; USING "##    ##    ###  ##  ###"; MG%(684); MG%(685); MG%(686); MG%(870); MG%(876)
     LPRINT "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; USING "##    ##    ###  ##  ###"; MG%(687); MG%(688); MG%(689); MG%(871); MG%(877)
4033 LPRINT
     IF MG%(847) <> 999 THEN LPRINT "NO CRITERIA DEFINED FOR PINCH HITTING FOR THE BATTER":GOTO 4034
     LPRINT "PINCH HITTING FOR THE BATTER"
     LPRINT
     LPRINT "PINCH HIT FOR THE BATTER FROM 'A' INNING ON"
     LPRINT "IF DOWN BY 'B' OR MORE RUNS AND"
     LPRINT "'C' RUNNERS ON BASE AND"
     LPRINT "SLUGGING PCT IS LESS THAN 'D'"
     LPRINT "AND BATTING AVERAGE IS LESS THAN 'E'"
     LPRINT "'F' % OF THE TIME"
     LPRINT
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO        A      B      C      D       E     F"
     FOR I = 0 TO 2
     LPRINT USING "   #           ##     ##      #     ###    ###   ###"; I + 1; MG%(I * 6 + 848); MG%(I * 6 + 849); MG%(I * 6 + 850); MG%(I * 6 + 851); MG%(I * 6 + 852); MG%(I * 6 + 853)
     NEXT
4034 LPRINT
     IF MG%(690) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR INFIELD IN WITH 3RD BASE OCCUPIED":GOTO 4035
     LPRINT "INFIELD IN WITH 3RD BASE OCCUPIED": LPRINT
     LPRINT "IF DOWN BY 'A' RUNS"
     LPRINT "AND NOT MORE THAN 'B' RUNS"
     LPRINT "FROM 'C' INNING ON"
     LPRINT "'D' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                              A     B   C     D"
     LPRINT "1 - 0 OUTS                           "; USING "##    ##   #   ###"; MG%(691); MG%(692); MG%(693); MG%(694)
     LPRINT "2 - 0 OUTS                           "; USING "##    ##   #   ###"; MG%(695); MG%(696); MG%(697); MG%(698)
     LPRINT "3 - <2 OUTS, RUNNER ON 1ST           "; USING "##    ##   #   ###"; MG%(699); MG%(700); MG%(701); MG%(702)
     LPRINT "4 - <2 OUTS, RUNNER ON 1ST           "; USING "##    ##   #   ###"; MG%(703); MG%(704); MG%(705); MG%(706)
4035 LPRINT
     CLS
     IF MG%(707) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT":GOTO 4036
     LPRINT "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": LPRINT
     LPRINT "IF BUNT RATING IS >= 'A'"
     LPRINT "'B' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "      A      B"
     LPRINT USING "(1)  ##    ###"; MG%(708); MG%(709)
     LPRINT USING "(2)  ##    ###"; MG%(710); MG%(711)
     LPRINT USING "(3)  ##    ###"; MG%(712); MG%(713)
4036 LPRINT
     IF MG%(714) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SAC HIT SITUATION":GOTO 4037
     LPRINT "CORNERS IN VS HITTER IN SAC HIT SITUATION": LPRINT
     LPRINT "IF AHEAD BY  'A' RUNS OR LESS"
     LPRINT "OR BEHIND BY NOT MORE THAN 'B' RUNS"
     LPRINT "BUNT RATING >= 'C'"
     LPRINT "'D' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                              A     B   C     D"
     LPRINT "1 - 0 OUTS, RUNNER ON 1ST            "; USING "##    ##   #   ###"; MG%(715); MG%(716); MG%(717); MG%(718)
     LPRINT "2 - 0 OUTS, RUNNER ON 1ST            "; USING "##    ##   #   ###"; MG%(719); MG%(720); MG%(721); MG%(722)
     LPRINT "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; USING "##    ##   #   ###"; MG%(723); MG%(724); MG%(725); MG%(726)
     LPRINT "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; USING "##    ##   #   ###"; MG%(727); MG%(728); MG%(729); MG%(730)
4037 LPRINT
     IF MG%(731) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR CORNERS IN VS PITCHER IN SAC HIT SITUATION":GOTO 4038
     LPRINT "CORNERS IN VS PITCHER IN SAC HIT SITUATION": LPRINT
     LPRINT "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO 'A'"
     LPRINT "'B' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                                  A      B"
     LPRINT "1 - <= 1 OUT, RUNNER ON 1ST              "; USING "###   ###"; MG%(732); MG%(733)
     LPRINT "2 - <= 1 OUT, RUNNER ON 1ST              "; USING "###   ###"; MG%(734); MG%(735)
     LPRINT "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; USING "###   ###"; MG%(736); MG%(737)
     LPRINT "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; USING "###   ###"; MG%(738); MG%(739)
4038 LPRINT
     IF MG%(740) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SQUEEZE SITUATION":GOTO 4039
     LPRINT "CORNERS IN VS HITTER IN SQUEEZE SITUATION": LPRINT
     LPRINT "IF AHEAD BY  'A' RUNS OR LESS"
     LPRINT "OR BEHIND BY NOT MORE THAN 'B' RUNS"
     LPRINT "BUNT RATING >= 'C'"
     LPRINT "'D' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                              A     B   C     D"
     LPRINT "1 - 0 OUT, RUNNER ON 3RD             "; USING "##    ##   #   ###"; MG%(741); MG%(742); MG%(743); MG%(744)
     LPRINT "2 - 1 OUT, RUNNER ON 3RD             "; USING "##    ##   #   ###"; MG%(745); MG%(746); MG%(747); MG%(748)
4039 LPRINT
     IF MG%(749) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR GUARDING THE LINES":GOTO 4040
     LPRINT "GUARD LINES STRATEGY": LPRINT
     LPRINT "IF 'A' RUNS AHEAD"
     LPRINT "'B' INNING ON"
     LPRINT "'C' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "     A     B      C"
     LPRINT "1 - "; USING "##    ##    ###"; MG%(750); MG%(751); MG%(752)
     LPRINT "2 - "; USING "##    ##    ###"; MG%(753); MG%(754); MG%(755)
4040 LPRINT
     IF MG%(756) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR PITCHING AROUND HITTERS":GOTO 4041
     LPRINT "PITCH AROUND STRATEGY": LPRINT
     LPRINT "SLUGGING PCT >= 'A'"
     LPRINT "'B' INNING ON"
     LPRINT "'C' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                                               A     B      C"
     LPRINT "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; USING "###    ##    ###"; MG%(757); MG%(758); MG%(759)
     LPRINT "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; USING "###    ##    ###"; MG%(760); MG%(761); MG%(762)
     LPRINT "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; USING "###    ##    ###"; MG%(763); MG%(764); MG%(765)
4041 LPRINT
     IF MG%(766) <> 999 THEN LPRINT "NO STRATEGY DEFINED FOR THROWING TO THE PLATE":GOTO 4042
     LPRINT "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": LPRINT
     LPRINT "FROM 'A' INNING ON"
     LPRINT "'B' OR MORE RUNS AHEAD"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "      A      B"
     LPRINT USING "(1)  ##    ###"; MG%(767); MG%(768)
     LPRINT USING "(2)  ##    ###"; MG%(769); MG%(770)
4042 LPRINT
     LPRINT "# PINCH RUNNER"
     FOR I = 1 TO 6
     LPRINT USING "# "; I;
     IF MG%(I + 771) < 0 THEN LPRINT : GOTO 4043
     LPRINT B$(MG%(I + 771))
4043 NEXT
     LPRINT
     LPRINT "DEFENSIVE SUBS FOR PINCH RUNNERS"
     LPRINT "SCORE TIED OR DOWN BY ONE RUN"
     IF MG%(778) <> 999 THEN LPRINT "NO SUBS DEFINED":GOTO 4044
     LPRINT "# PLAYER     I  SUBSTITUTE  %"
     FOR I = 0 TO 5
     LPRINT USING "# "; I + 1;
     IF MG%(I * 4 + 779) < 0 THEN LPRINT : GOTO 4045
     LPRINT LEFT$(B$(MG%(I * 4 + 779)), 10); TAB(19);
     LPRINT USING "## "; MG%(I * 4 + 780);
     LPRINT LEFT$(B$(MG%(I * 4 + 781)), 10); TAB(33); USING "###"; MG%(I * 4 + 782)
4045 NEXT
4044 LPRINT
     IF MG%(803) <> 999 THEN LPRINT "NO STRATEGY DEFINED TO PINCH RUN FOR PITCHER":GOTO 4046
     LPRINT "PINCH RUN FOR PITCHER": LPRINT
     LPRINT "RUN RATING <= 'A'"
     LPRINT "'B' INNING ON"
     LPRINT "'C' % OF THE TIME"
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                A     B      C"
     LPRINT "1 - TIED OR DOWN BY 1 "; USING "###    ##    ###"; MG%(804); MG%(805); MG%(806)
     LPRINT "2 - TIED OR DOWN BY 1 "; USING "###    ##    ###"; MG%(807); MG%(808); MG%(809)
4046 LPRINT
     IF MG%(810) <> 999 THEN LPRINT "NO BASE RUNNING STRATEGY DEFINED":GOTO 4047
     LPRINT "BASE RUNNING STRATEGY": LPRINT
     LPRINT TAB(32); "TABLE OF VALUES"
     LPRINT "SCENARIO                            %       %          %          %"
     LPRINT "                                  SAFE    NORMAL   AGGRESSIVE   TOTAL"
     LPRINT " 1 - INNING 1-3, WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(811); MG%(812); MG%(813); MG%(811) + MG%(812) + MG%(813)
     LPRINT " 2 - INNING 1-3, WHEN TIED        "; USING "###     ###        ###          ###"; MG%(814); MG%(815); MG%(816); MG%(816) + MG%(815) + MG%(814)
     LPRINT " 3 - INNING 1-3, WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(817); MG%(818); MG%(819); MG%(819) + MG%(818) + MG%(817)
     LPRINT " 4 - INNING 4-6, WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(820); MG%(821); MG%(822); MG%(820) + MG%(821) + MG%(822)
     LPRINT " 5 - INNING 4-6, WHEN TIED        "; USING "###     ###        ###          ###"; MG%(823); MG%(824); MG%(825); MG%(823) + MG%(824) + MG%(825)
     LPRINT " 6 - INNING 4-6, WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(826); MG%(827); MG%(828); MG%(826) + MG%(827) + MG%(828)
     LPRINT " 7 - INNING 7-9, WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(829); MG%(830); MG%(831); MG%(829) + MG%(830) + MG%(831)
     LPRINT " 8 - INNING 7-9, WHEN TIED        "; USING "###     ###        ###          ###"; MG%(832); MG%(833); MG%(834); MG%(832) + MG%(833) + MG%(834)
     LPRINT " 9 - INNING 7-9, WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(835); MG%(836); MG%(837); MG%(835) + MG%(836) + MG%(837)
     LPRINT "10 - INNING 9- , WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(838); MG%(839); MG%(840); MG%(838) + MG%(839) + MG%(840)
     LPRINT "11 - INNING 9- , WHEN TIED        "; USING "###     ###        ###          ###"; MG%(841); MG%(842); MG%(843); MG%(841) + MG%(842) + MG%(843)
     LPRINT "12 - INNING 9- , WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(844); MG%(845); MG%(846); MG%(844) + MG%(845) + MG%(846)
4047 LPRINT
     RETURN

     REM: BRANCH IF ENTERING A NEW ROTATION OR EDITING EXISTING
4500 REM:*** PITCHING ROTATIONS ***
     IF Q = 0 THEN CLS : GOTO 4604
     GOTO 4516
4512 CLS
     IF UCASE$(LEFT$(D$(3), 1)) = "C" THEN 4515
     PRINT "INSERT DISK WITH ROTATION IN "; D$(3); " AND HIT ANY KEY";
4513 I$ = INKEY$: IF I$ = "" THEN 4513
4515 IF NOT EXIST%(D$(3) + "MGR." + YN$) THEN 4505
     OPEN D$(3) + "MGR." + YN$ FOR RANDOM AS #2 LEN = 2000
     FOR I = 0 TO 999
     FIELD #2, I * 2 AS Q$, 2 AS R$(I), 2000 - I * 2 - 2 AS Q$
     NEXT
     GET #2, T
     FOR I = 0 TO 999
     MG%(I) = CVI(R$(I))
     NEXT
     CLOSE 2
4505 RETURN
REM: HERE AND BELOW FROM THE PITCHING STRATEGY
4516 IF MG%(99) <> 999 THEN 4600
4517 GOSUB 4520
     LOCATE 1, 40: PRINT "(0) SIZE OF ROTATION IS "; MG%(1)
     LOCATE 3, 40: PRINT "    STARTING ROTATION     % STARTS"
     FOR I = 1 TO MG%(1)
     LOCATE , 40
     PRINT USING "(#) "; I;
     IF MG%(I + 2) < 0 THEN PRINT : GOTO 4530
     PRINT P$(MG%(I + 2)); TAB(70); MG%(I + 12)
4530 NEXT
     LOCATE 10, 40: PRINT "(6) MOVE TO SPOT STARTERS"
4536 I$ = INKEY$: IF I$ = "" THEN 4536
     I = VAL(I$): IF I < 0 OR I > 6 THEN 4536
     IF I = 0 THEN GOSUB 4550: GOTO 4517
     IF I >= 1 AND I <= MG%(1) THEN GOSUB 4560: GOTO 4517
     IF I = 6 THEN GOSUB 4580
     GOTO 4536
4550 LOCATE 12, 40: INPUT "ENTER ROTATION SIZE (1-5) "; MG%(1)
     IF MG%(1) < 1 OR MG%(1) > 5 THEN 4550
     LOCATE 12, 40: PRINT "                                "
     FOR I = MG%(1) + 1 TO 5: MG%(I + 2) = -1: MG%(I + 12) = -1: NEXT
     RETURN
4560 LOCATE 12, 40: PRINT "SELECT STARTER #"; I
     I$ = INKEY$: IF I$ = "" THEN 4560
     GOSUB 1290: I1 = VAL(I$)
     MG%(I + 2) = I1
     LOCATE 12, 40: PRINT "                     "
4565 LOCATE 12, 40: INPUT "ENTER % OF TIME TO START "; MG%(I + 12)
     IF MG%(I + 12) < 0 OR MG%(I + 12) > 100 THEN 4565
     LOCATE 12, 40: PRINT "                               "
     RETURN
4580 REM:SPOT STARTERS
     GOSUB 4520
     LOCATE 1, 40: PRINT "     SPOT STARTERS         % USED"
     SS% = 0
     FOR I = 21 TO 36
     LOCATE , 40
     PRINT USING "(##) "; I - 20;
     IF MG%(I) < 0 THEN PRINT : GOTO 4585
     PRINT P$(MG%(I)); TAB(70); MG%(I + 20)
     SS% = SS% + MG%(I + 20)
4585 NEXT
     LOCATE 19, 40: PRINT "TOTAL MUST EQUAL 100"; TAB(70); SS%
     LOCATE 20, 40: PRINT "(A)DD SPOT STARTER (E)DIT SPOT %"
     LOCATE 21, 40: PRINT "(D)ELETE STARTER (S)AVE PROFILE"
4586 I$ = INKEY$: IF I$ = "" THEN 4586
     IF UCASE$(I$) = "S" AND SS% = 100 THEN MG%(99) = 999: CALL UPDATEFILE: GOTO 55
     IF UCASE$(I$) = "D" THEN GOSUB 4582: GOTO 4580
     IF UCASE$(I$) = "E" THEN GOSUB 4583: GOTO 4580
     IF UCASE$(I$) = "A" THEN GOSUB 4570: GOTO 4580
     GOTO 4586
4582 LOCATE 20, 40: PRINT "                                "
     LOCATE 21, 40: PRINT "                                "
     LOCATE 20, 40: INPUT "ENTER NUMBER TO DELETE (1-16) "; I1
     IF I1 < 1 OR I1 > 16 THEN 4582
     MG%(I1 + 20) = -1: MG%(I1 + 40) = -1
     RETURN
4583 LOCATE 20, 40: PRINT "                                "
     LOCATE 21, 40: PRINT "                                "
     LOCATE 20, 40: INPUT "ENTER NUMBER TO EDIT (1-16) "; I1
     IF I1 < 1 OR I1 > 16 THEN 4583
4584 LOCATE 20, 40: INPUT "ENTER % OF TIME TO SPOT START "; I2
     IF I2 < 0 OR I2 > 100 THEN 4584
     MG%(I1 + 40) = I2
     RETURN
4570 LOCATE 20, 40: PRINT "                                "
     LOCATE 21, 40: PRINT "                                "
     LOCATE 20, 40: INPUT "ENTER SLOT TO ADD TO (1-16) "; I1
     IF I1 < 1 OR I1 > 16 THEN 4570
     LOCATE 21, 40: PRINT "SELECT PITCHER FROM LIST"
4571 I$ = INKEY$: IF I$ = "" THEN 4571
     GOSUB 1290: I2 = VAL(I$)
     MG%(I1 + 20) = I2
4575 LOCATE 20, 40: PRINT "                                "
     LOCATE 21, 40: PRINT "                                "
     LOCATE 20, 40: INPUT "ENTER % OF TIME TO SPOT START "; I2
     IF I2 < 0 OR I2 > 100 THEN 4575
     MG%(I1 + 40) = I2
     RETURN

4600 CLS : PRINT "ERROR::: ROTATION FILE NOT FOUND"
     PRINT
     PRINT "DO YOU WANT TO ENTER A ROTATION (YN)"
4602 I$ = INKEY$: IF I$ = "" THEN 4602
4603 IF UCASE$(I$) <> "Y" THEN 55
4604 I = 1: GOSUB 6000
     U$ = "COMPUTER DETERMINE ROTATION (YN) "
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
     PRINT
4605 I$ = INKEY$: IF I$ = "" THEN 4605
     IF UCASE$(I$) = "Y" THEN CR% = 1
     INPUT "SEASON LENGTH (154, 162 ,ETC.)";SL%
4610 CLS
     PRINT TAB(20); "SELECT SIZE OF YOUR PITCHING ROTATION": PRINT
     COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "MAN ROTATION    "; : COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "MAN ROTATION"
     COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "MAN ROTATION    "; : COLOR 14: PRINT "(5) "; : COLOR 15: PRINT "MAN ROTATION"
     COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "MAN ROTATION": PRINT
     PRINT "       YOU MAY CONSIDER THE FOLLOWING GUIDELINES:"
     PRINT "MAX GAMES STARTED BY ANY PITCHER           ROTATION SIZE"
     PRINT "          32                                    5"
     PRINT "          41                                    4"
     PRINT "          54                                    3"
     PRINT "          75                                    2"
     PRINT "         ALL                                    1"
     PRINT
     PRINT "ALL PITCHERS AND GAMES STARTED"
     FOR I = 0 TO 6
     LOCATE I + 17, 1
     IF P$(I) = "XXX" THEN 4607
     PRINT P$(I); TAB(18); P%(I, 5);
4607 LOCATE , 25
     IF P$(I + 7) = "XXX" THEN 4608
     PRINT P$(I + 7); TAB(43); P%(I + 7, 5);
4608 LOCATE , 50
     IF P$(I + 14) = "XXX" THEN 4609
     PRINT P$(I + 14); TAB(68); P%(I + 14, 5)
4609 NEXT
4611 I$ = INKEY$: IF I$ = "" THEN 4611
4612 I = VAL(I$): IF I < 1 OR I > 5 THEN 4611
4615 MG%(1) = I: MG%(2) = 0: FOR I = 3 TO 36: MG%(I) = -1: NEXT: FOR I = 41 TO 56: MG%(I) = 0: NEXT
     MG%(0) = 999
     IF CR% = 1 THEN 4690
4620 I% = 99
     GOSUB 4520
4625 FOR I = 0 TO MG%(1) - 1
4626 LOCATE 15, 59: PRINT "STARTER "; I + 1
4628 I$ = INKEY$: IF I$ = "" THEN 4628
4629 GOSUB 1290
4632 I1 = VAL(I$): IF I1 > 21 OR MG%(3) = I1 OR MG%(4) = I1 OR MG%(5) = I1 OR MG%(6) = I1 OR MG%(7) = I1 THEN 4628
4635 LOCATE , 59: PRINT P$(I1)
4636 LOCATE , 59: PRINT "ANY CHANGE (YN)";
4637 I$ = INKEY$: IF I$ = "" THEN 4637
4638 IF UCASE$(I$) = "Y" THEN FOR I2 = 15 TO 17: LOCATE I2, 59: PRINT "                 ": NEXT: GOTO 4626
4640 MG%(I + 3) = I1: FOR I2 = 15 TO 17: LOCATE I2, 59: PRINT "                  ": NEXT
     NEXT
     GOTO 4700
4680 CLS : PRINT "MAKE SURE ROTATION DISK IS IN DRIVE "; D$(3)
4681 LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE";
     IF UCASE$(LEFT$(D$(2), 1)) = "C" THEN 4683
     IF AP = 1 THEN 4683
4682 I$ = INKEY$: IF I$ = "" THEN 4682
4683 RETURN


4690 REM:COMPUTER SET ROTATION
     CR% = 0
     FOR I = 0 TO 21
     IF P$(I) = "XXX" THEN GS%(I, 0) = 0: GS%(I, 1) = -1: GOTO 4691
     GS%(I, 0) = P%(I, 5): GS%(I, 1) = I
4691 NEXT
     FOR Z = 0 TO 20: FOR Z1 = Z + 1 TO 21: IF GS%(Z, 0) >= GS%(Z1, 0) THEN 4695
     SWAP GS%(Z, 0), GS%(Z1, 0): SWAP GS%(Z, 1), GS%(Z1, 1)
4695 NEXT Z1: NEXT Z
     FOR I = 0 TO MG%(1) - 1
     MG%(I + 3) = GS%(I, 1)
     NEXT
     I1 = 0
     FOR I = MG%(1) TO 21
     I1 = I1 + 1
     IF GS%(I, 0) = 0 THEN 4700
     MG%(I1 + 20) = GS%(I, 1)
     NEXT
4700 I1 = I1 - 1: IF MG%(1) = 1 THEN MG%(13) = 100: GOTO 4517
     IF MG%(1) = 2 THEN GS% = 75
     IF MG%(1) = 3 THEN GS% = 54
     IF MG%(1) = 4 THEN GS% = 41
     IF MG%(1) = 5 THEN GS% = 32


     GS% = GS% * SL% / 162


     FOR I = 1 TO MG%(1)
     MG%(I + 12) = INT(P%(MG%(I + 2), 5) * 100 / GS% + .5)
     NEXT
     SS% = 0
     FOR I = 21 TO 36
     IF MG%(I) < 0 OR MG%(I) > 21 THEN 4705
     SS% = SS% + P%(MG%(I), 5)
4705 NEXT
     FOR I = 21 TO 36
     IF MG%(I) < 0 OR MG%(I) > 21 THEN 4710
     MG%(I + 20) = INT(P%(MG%(I), 5) * 100 / SS% + .5)
4710 NEXT
     GOTO 4517
4095 I$ = INKEY$: IF I$ = "" THEN 4095
     RETURN
4520 CLS
4521 LOCATE 1, 1
     PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
     COLOR 14, 0: LOCATE , 16: PRINT "T   G GS   W- L   ERA"
     FOR I = 0 TO 21
     IF P$(I) = "XXX" OR LEFT$(P$(I), 1) = " " OR P$(I) = "" THEN P%(I, 15) = 98
     IF P%(I, 15) = 1 OR P$(I) = "XXX" THEN 4525
     COLOR 2, 0
     IF I <= 9 THEN PRINT USING "#"; I; : COLOR 15, 0
     IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0
     FOR I1 = 1 TO MG%(1)
     IF I = MG%(I1 + 2) THEN COLOR 0, 7
     NEXT
     LOCATE , 3: PRINT P$(I);
     COLOR 15, 0
     LOCATE , 16: PRINT B1$(P%(I, 0) + 2);
     LOCATE , 17: PRINT USING " ### ##"; P%(I, 4); P%(I, 5);
     LOCATE , 24: PRINT USING "  ##"; P%(I, 1); : PRINT "-"; : PRINT USING "## ##.##"; P%(I, 2); P%(I, 10) / 100
4525 NEXT I
     RETURN
5000 REM:MIDDLE RELIEVERS
     IF MG%(60) <> 999 THEN MG%(60) = 999: FOR I = 1 TO 10: MG%(I + 60) = -1: MG%(I + 70) = 0: NEXT
     GOSUB 5100
     CLS
     LOCATE 1, 1
     PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
     COLOR 14, 0: LOCATE , 16: PRINT "T   G GS   W- L %RA   ERA"
     FOR I = 0 TO 21
     IF P$(I) = "XXX" OR LEFT$(P$(I), 1) = " " OR P$(I) = "" THEN 5005
     COLOR 2, 0
     IF I <= 9 THEN PRINT USING "#"; I; : COLOR 15, 0
     IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0
     FOR I1 = 1 TO 10
     IF I = MG%(I1 + 60) THEN COLOR L%(11), L%(12)
     NEXT
     LOCATE , 3: PRINT P$(I);
     COLOR 15, 0
     LOCATE , 16: PRINT B1$(P%(I, 0) + 2);
     LOCATE , 17: PRINT USING " ### ##"; P%(I, 4); P%(I, 5);
     LOCATE , 24: PRINT USING "  ##"; P%(I, 1); : PRINT "-"; : PRINT USING "## ### ##.##"; P%(I, 2); (P%(I, 4) - P%(I, 5) - P%(I, 3)) / RA% * 100; P%(I, 10) / 100
5005 NEXT I
     LOCATE 18, 45: PRINT "%RA - PITCHER'S PERCENTAGE OF TEAM'S"
     LOCATE , 45:   PRINT "RELIEF APPEARANCES MINUS SAVES"
5007 LOCATE 1, 48: PRINT "  MIDDLE RELIEVERS      % APPEAR"
     FOR I = 1 TO 10
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 60) < 0 THEN PRINT : GOTO 5010
     PRINT P$(MG%(I + 60)); TAB(75); MG%(I + 70)
5010 NEXT
5011 LOCATE 13, 48: PRINT "(11) ACCEPT CHOICES"
     LOCATE 14, 48: PRINT "(12) DELETE A RELIEVER"
5015 LOCATE 15, 48: INPUT "ENTER SELECTION (1-12)"; I
     IF I < 1 OR I > 12 THEN 5015
     IF I >= 1 AND I <= 10 THEN GOSUB 5020: GOTO 5000
     IF I = 11 THEN CALL UPDATEFILE: GOTO 55
     IF I = 12 THEN GOSUB 5050: GOTO 5000
5020 LOCATE 16, 48: PRINT USING "SELECT RELIEVER ##:"; I;
5021 I$ = INKEY$: IF I$ = "" THEN 5021
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I + 60) = I1
     REM:LOCATE 16,48:PRINT "                     "
5025 LOCATE 17, 48: INPUT "ENTER % OF TIME TO APPEAR "; MG%(I + 70)
     IF MG%(I + 70) < 0 OR MG%(I + 70) > 100 THEN 5025
     LOCATE 17, 48: PRINT "                               "
     LOCATE 16, 48: PRINT "                               "
     RETURN
5050 LOCATE 16, 48: INPUT "SELECT RELIEVER TO DELETE: "; I
     IF I < 1 OR I > 10 THEN 5050
     MG%(I + 60) = -1: MG%(I + 70) = 0
     RETURN
5100 RA% = 0
     FOR I = 0 TO 21
     IF P$(I) = "XXX" THEN 5105
     RA% = RA% + P%(I, 4) - P%(I, 5) - P%(I, 3)
5105 NEXT
     RETURN
5200 REM:SET-UP RELIEVERS
     IF MG%(90) <> 999 THEN MG%(90) = 999: FOR I = 1 TO 6: MG%(I + 80) = -1: MG%(I + 90) = 0: NEXT
     CLS
     LOCATE 1, 1
     PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
     COLOR 14, 0: LOCATE , 16: PRINT "T   G GS   W- L SV   ERA"
     FOR I = 0 TO 21
     IF P$(I) = "XXX" OR LEFT$(P$(I), 1) = " " OR P$(I) = "" THEN 5205
     COLOR 2, 0
     IF I <= 9 THEN PRINT USING "#"; I; : COLOR 15, 0
     IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0
     FOR I1 = 1 TO 6
     IF I = MG%(I1 + 80) THEN COLOR L%(11), L%(12)
     NEXT
     LOCATE , 3: PRINT P$(I);
     COLOR 15, 0
     LOCATE , 16: PRINT B1$(P%(I, 0) + 2);
     LOCATE , 17: PRINT USING " ### ##"; P%(I, 4); P%(I, 5);
     LOCATE , 24: PRINT USING "  ##"; P%(I, 1); : PRINT "-"; : PRINT USING "## ### ##.##"; P%(I, 2); P%(I, 3); P%(I, 10) / 100
5205 NEXT I
5207 LOCATE 1, 48: PRINT "  SET-UP RELIEVERS      % APPEAR"
     LOCATE , 48: PRINT "   -----  VS LEFT  -------      "
     FOR I = 1 TO 3
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 80) < 0 THEN PRINT : GOTO 5210
     PRINT P$(MG%(I + 80)); TAB(75); MG%(I + 90)
5210 NEXT
     LOCATE , 48: PRINT "   -----  VS RIGHT  -------      "
     FOR I = 4 TO 6
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 80) < 0 THEN PRINT : GOTO 5212
     PRINT P$(MG%(I + 80)); TAB(75); MG%(I + 90)
5212 NEXT

5211 LOCATE 13, 48: PRINT "(7) ACCEPT CHOICES"
     LOCATE 14, 48: PRINT "(8) DELETE A RELIEVER"
5215 LOCATE 15, 48: INPUT "ENTER SELECTION (1-8)"; I
     IF I < 1 OR I > 8 THEN 5215
     IF I >= 1 AND I <= 6 THEN GOSUB 5220: GOTO 5200
     IF I = 7 THEN CALL UPDATEFILE: GOTO 55
     IF I = 8 THEN GOSUB 5250: GOTO 5200
5220 LOCATE 16, 48: PRINT USING "SELECT RELIEVER ##:"; I;
5221 I$ = INKEY$: IF I$ = "" THEN 5221
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I + 80) = I1
     REM:LOCATE 16,48:PRINT "                     "
5225 LOCATE 17, 48: INPUT "ENTER % OF TIME TO APPEAR "; MG%(I + 90)
     IF MG%(I + 90) < 0 OR MG%(I + 90) > 100 THEN 5225
     LOCATE 17, 48: PRINT "                               "
     LOCATE 16, 48: PRINT "                               "
     RETURN
5250 LOCATE 16, 48: INPUT "SELECT RELIEVER TO DELETE: "; I
     IF I < 1 OR I > 6 THEN 5250
     MG%(I + 80) = -1: MG%(I + 90) = 0
     RETURN
5300 REM:CLOSERS
     IF MG%(520) <> 999 THEN MG%(520) = 999: FOR I = 1 TO 6: MG%(I + 520) = -1: MG%(I + 530) = 0: NEXT
     CLS
     LOCATE 1, 1
     PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
     COLOR 14, 0: LOCATE , 16: PRINT "T   G GS   W- L SV   ERA"
     FOR I = 0 TO 21
     IF P$(I) = "XXX" OR LEFT$(P$(I), 1) = " " OR P$(I) = "" THEN 5305
     COLOR 2, 0
     IF I <= 9 THEN PRINT USING "#"; I; : COLOR 15, 0
     IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0
     FOR I1 = 1 TO 6
     IF I = MG%(I1 + 520) THEN COLOR L%(11), L%(12)
     NEXT
     LOCATE , 3: PRINT P$(I);
     COLOR 15, 0
     LOCATE , 16: PRINT B1$(P%(I, 0) + 2);
     LOCATE , 17: PRINT USING " ### ##"; P%(I, 4); P%(I, 5);
     LOCATE , 24: PRINT USING "  ##"; P%(I, 1); : PRINT "-"; : PRINT USING "## ### ##.##"; P%(I, 2); P%(I, 3); P%(I, 10) / 100
5305 NEXT I
5307 LOCATE 1, 48: PRINT "          CLOSERS       % APPEAR"
     LOCATE , 48: PRINT "   -----  VS LEFT  -------      "
     FOR I = 1 TO 3
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 520) < 0 THEN PRINT : GOTO 5310
     PRINT P$(MG%(I + 520)); TAB(75); MG%(I + 530)
5310 NEXT
     LOCATE , 48: PRINT "   -----  VS RIGHT  -------      "
     FOR I = 4 TO 6
     LOCATE , 48
     PRINT USING "(##) "; I;
     IF MG%(I + 520) < 0 THEN PRINT : GOTO 5312
     PRINT P$(MG%(I + 520)); TAB(75); MG%(I + 530)
5312 NEXT

5311 LOCATE 13, 48: PRINT "(7) ACCEPT CHOICES"
     LOCATE 14, 48: PRINT "(8) DELETE A RELIEVER"
5315 LOCATE 15, 48: INPUT "ENTER SELECTION (1-8)"; I
     IF I < 1 OR I > 8 THEN 5315
     IF I >= 1 AND I <= 6 THEN GOSUB 5320: GOTO 5300
     IF I = 7 THEN CALL UPDATEFILE: GOTO 55
     IF I = 8 THEN GOSUB 5350: GOTO 5300
5320 LOCATE 16, 48: PRINT USING "SELECT RELIEVER ##:"; I;
5321 I$ = INKEY$: IF I$ = "" THEN 5321
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I + 520) = I1
     REM:LOCATE 16,48:PRINT "                     "
5325 LOCATE 17, 48: INPUT "ENTER % OF TIME TO APPEAR "; MG%(I + 530)
     IF MG%(I + 530) < 0 OR MG%(I + 530) > 100 THEN 5325
     LOCATE 17, 48: PRINT "                               "
     LOCATE 16, 48: PRINT "                               "
     RETURN
5350 LOCATE 16, 48: INPUT "SELECT RELIEVER TO DELETE: "; I
     IF I < 1 OR I > 6 THEN 5350
     MG%(I + 520) = -1: MG%(I + 530) = 0
     RETURN

3000 REM:HITTING STRATEGIES
     CLS
     PRINT TAB(28); : COLOR 14, 4: PRINT "HITTING STRATEGIES": COLOR 15, 0: PRINT
     ITEM$(1) = "SITUATIONS TO PH FOR PITCHER                 "
     ITEM$(2) = "SITUATIONS FOR PITCHER TO SAC BUNT IN        "
     ITEM$(3) = "SITUATONS FOR POSITION PLAYERS TO SAC BUNT IN"
     ITEM$(4) = "HIT AND RUN SCENARIOS                        "
     ITEM$(5) = "SITUATIONS TO PH FOR POSITION PLAYERS        "
     ITEM$(6) = "RETURN TO MAIN MENU                          "
   SELECTION% = 1
   Count% = 6
   Start% = 1
   SCANCODE% = 6
   FileColr% = 31
   HiLiteColr% = 112
   Spacing% = 1
   Row% = 5
   Column% = 13
   CALL Box0(4, 12, 11, 58, 2, 31)
   CALL SHOWCURSOR
   MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
   CALL HIDECURSOR
   COLOR , 0
   I = SELECTION%
     IF I = 6 OR SCANCODE% = 27 THEN 45
     IF I = 1 THEN CALL PHPITCHER: GOTO 3000
     IF I = 2 THEN 3100
     IF I = 3 THEN 3150
     IF I = 4 THEN 3200
     IF I = 5 THEN CALL PHBATTER: GOTO 3000
3100 REM:SAC BUNT HIT BY PITCHER"
     CLS
     IF MG%(609) <> 999 THEN MG%(609) = 999: FOR I = 1 TO 24: MG%(609 + I) = 0: NEXT
     LOCATE 1, 24: COLOR 14, 2: PRINT "SACRIFICE BUNTING FOR THE PITCHER": COLOR 15, 0: PRINT
     COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " OUTS OR LESS"
     PRINT "IF "; : COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " OR LESS RUNS AHEAD AND"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " OR LESS RUNS BEHIND"
     COLOR 15, 1: PRINT " D "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                       A      B     C      D"
     PRINT "1 - 7TH INNING OR LESS         "; USING "#     ##    ##     ###"; MG%(610); MG%(611); MG%(612); MG%(613)
     PRINT "    RUNNER ON 1ST           "
     PRINT "2 - 8TH INNING ON              "; USING "#     ##    ##     ###"; MG%(614); MG%(615); MG%(616); MG%(617)
     PRINT "    RUNNER ON 1ST           "
     PRINT "3 - 7TH INNING OR LESS         "; USING "#     ##    ##     ###"; MG%(618); MG%(619); MG%(620); MG%(621)
     PRINT "    RUNNER ON 2ND           "
     PRINT "4 - 8TH INNING ON              "; USING "#     ##    ##     ###"; MG%(622); MG%(623); MG%(624); MG%(625)
     PRINT "    RUNNER ON 2ND           "
     PRINT "5 - 7TH INNING OR LESS         "; USING "#     ##    ##     ###"; MG%(626); MG%(627); MG%(628); MG%(629)
     PRINT "    RUNNERS ON 1ST AND 2ND  "
     PRINT "6 - 8TH INNING ON              "; USING "#     ##    ##     ###"; MG%(630); MG%(631); MG%(632); MG%(633)
     PRINT "    RUNNERS ON 1ST AND 2ND  "
     PRINT "7 - ACCEPT CHOICES"
3105 LOCATE 22, 1: INPUT "ENTER SELECTION (1-7) "; I
     IF I < 1 OR I > 7 THEN 3105
     IF I = 7 THEN CALL UPDATEFILE: GOTO 3000
     I = I - 1
     LOCATE 22, 1: INPUT "OUTS OR LESS        "; MG%(I * 4 + 610)
     LOCATE 22, 1: INPUT "RUNS AHEAD OR LESS  "; MG%(I * 4 + 611)
     LOCATE 22, 1: INPUT "RUNS BEHIND OR LESS "; MG%(I * 4 + 612)
     LOCATE 22, 1: INPUT "PERCENT OF TIME     "; MG%(I * 4 + 613)
     GOTO 3100

3150 REM:SAC BUNT HIT BY POSITION PLAYERS"
     CLS
     IF MG%(634) <> 999 THEN MG%(634) = 999: FOR I = 1 TO 36: MG%(634 + I) = 0: NEXT
     LOCATE 1, 24: COLOR 14, 2: PRINT "SACRIFICE BUNTING FOR THE POSITION PLAYERS": COLOR 15, 0: PRINT
     PRINT "IF "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " OR LESS RUNS AHEAD AND"
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " OR LESS RUNS BEHIND"
     PRINT "BATTING AVERAGE IS LESS THAN "; : COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " AND SLUGGING PCT IS LESS THAN "; : COLOR 15, 1: PRINT " D "
     COLOR 15, 0: PRINT "PITCHERS ERA LESS THAN "; : COLOR 15, 1: PRINT " E "; : COLOR 15, 0: PRINT ", "; : COLOR 15, 1: PRINT " F "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                        A     B      C      D      E     F"
     PRINT "1 - 7TH INNING OR LESS         "; USING "##    ##    ###    ###    ###   ###"; MG%(635); MG%(636); MG%(637); MG%(638); MG%(639); MG%(640)
     PRINT "    0 OUTS, RUNNER ON 1ST"
     PRINT "2 - 7TH INNING OR LESS         "; USING "##    ##    ###    ###    ###   ###"; MG%(641); MG%(642); MG%(643); MG%(644); MG%(645); MG%(646)
     PRINT "    0 OUTS, RUNNER ON 2ND"
     PRINT "3 - 7TH INNING OR LESS         "; USING "##    ##    ###    ###    ###   ###"; MG%(647); MG%(648); MG%(649); MG%(650); MG%(651); MG%(652)
     PRINT "    0 OUTS, RUNNERS 1ST AND 2ND"
     PRINT "4 - 8TH INNING ON              "; USING "##    ##    ###    ###    ###   ###"; MG%(653); MG%(654); MG%(655); MG%(656); MG%(657); MG%(658)
     PRINT "    0 OUTS, RUNNER ON 1ST"
     PRINT "5 - 8TH INNING ON              "; USING "##    ##    ###    ###    ###   ###"; MG%(659); MG%(660); MG%(661); MG%(662); MG%(663); MG%(664)
     PRINT "    0 OUTS, RUNNER ON 2ND"
     PRINT "6 - 8TH INNING ON              "; USING "##    ##    ###    ###    ###   ###"; MG%(665); MG%(666); MG%(667); MG%(668); MG%(669); MG%(670)
     PRINT "    0 OUTS, RUNNERS ON 1ST AND 2ND"
     PRINT "7 - ACCEPT CHOICES"
3155 LOCATE 22, 1: INPUT "ENTER SELECTION (1-7) "; I
     IF I < 1 OR I > 7 THEN 3155
     IF I = 7 THEN CALL UPDATEFILE: GOTO 3000
     I = I - 1
     LOCATE 22, 1: INPUT "RUNS AHEAD OR LESS        "; MG%(I * 6 + 635)
     LOCATE 22, 1: INPUT "RUNS BEHIND OR LESS       "; MG%(I * 6 + 636)
     LOCATE 22, 1: INPUT "BATTING AVERAGE LESS THAN "; MG%(I * 6 + 637)
     LOCATE 22, 1: INPUT "SLUGGING PCT LESS THAT    "; MG%(I * 6 + 638)
     LOCATE 22, 1: INPUT "PITCHER'S ERA LESS THAN   "; MG%(I * 6 + 639)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 6 + 640)
     GOTO 3150

3200 REM:HIT AND RUN
     CLS
     IF MG%(671) <> 999 THEN MG%(671) = 999: FOR I = 1 TO 18: MG%(671 + I) = 0: NEXT
     LOCATE 1, 30: COLOR 14, 2: PRINT "HIT AND RUN STRATEGY": COLOR 15, 0: PRINT
     PRINT "IF "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " OR LESS RUNS AHEAD AND"
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " OR LESS RUNS BEHIND"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " % OF THE TIME"
     COLOR 15, 1: PRINT " D "; : COLOR 15, 0: PRINT " AB PER STRIKEOUT"
     PRINT " SLUGGING PCT LESS THAN OR EQUAL TO "; : COLOR 15, 1: PRINT " E ": COLOR 15, 0
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "    SCENARIO                          A     B      C   D    E"
     PRINT "1 - 0 OUTS, RUNNER ON 1ST            "; USING "##    ##    ###  ##  ###"; MG%(672); MG%(673); MG%(674); MG%(866); MG%(872)
     PRINT "2 - 1 OUT, RUNNER ON 1ST             "; USING "##    ##    ###  ##  ###"; MG%(675); MG%(676); MG%(677); MG%(867); MG%(873)
     PRINT "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; USING "##    ##    ###  ##  ###"; MG%(678); MG%(679); MG%(680); MG%(868); MG%(874)
     PRINT "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; USING "##    ##    ###  ##  ###"; MG%(681); MG%(682); MG%(683); MG%(869); MG%(875)
     PRINT "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; USING "##    ##    ###  ##  ###"; MG%(684); MG%(685); MG%(686); MG%(870); MG%(876)
     PRINT "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; USING "##    ##    ###  ##  ###"; MG%(687); MG%(688); MG%(689); MG%(871); MG%(877)
     PRINT "7 - ACCEPT CHOICES"
3205 LOCATE 22, 1: INPUT "ENTER SELECTION (1-7) "; I
     IF I < 1 OR I > 7 THEN 3205
     IF I = 7 THEN CALL UPDATEFILE: GOTO 3000
     I = I - 1
     LOCATE 22, 1: INPUT "RUNS AHEAD OR LESS        "; MG%(I * 3 + 672)
     LOCATE 22, 1: INPUT "RUNS BEHIND OR LESS       "; MG%(I * 3 + 673)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 3 + 674)
     LOCATE 22, 1: INPUT "AB PER STRIKEOUT          "; MG%(I + 866)
     LOCATE 22, 1: INPUT "SLUGGING PCT <=           "; MG%(I + 872)
     GOTO 3200

3300 REM:DEFENSIVE STRATEGIES
     CLS
     PRINT TAB(31); : COLOR 14, 4: PRINT "DEFENSIVE STRATEGIES": COLOR 15, 0: PRINT

     ITEM$(1) = "INFIELD IN WITH 3RD BASE OCCUPIED            "
     ITEM$(2) = "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT"
     ITEM$(3) = "CORNERS IN VS HITTER IN SAC BUNT SITUATION   "
     ITEM$(4) = "CORNERS IN VS PITCHER IN SAC BUNT SITUATION  "
     ITEM$(5) = "CORNERS IN VS HITTER WHO MIGHT SQUEEZE BUNT  "
     ITEM$(6) = "GUARD LINES IN LATE INNINGS                  "
     ITEM$(7) = "PITCH AROUND THE HITTER                      "
     ITEM$(8) = "NOT THROWING HOME WITH RUNNER TRYING TO SCORE"
     ITEM$(9) = "RETURN TO MAIN MENU                          "
   SELECTION% = 1
   Count% = 9
   Start% = 1
   SCANCODE% = 9
   FileColr% = 31
   HiLiteColr% = 112
   Spacing% = 1
   Row% = 5
   Column% = 18
   CALL Box0(4, 17, 14, 63, 2, 31)
   CALL SHOWCURSOR
   MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
   CALL HIDECURSOR
   COLOR , 0
     I = SELECTION%
     IF I = 9 OR SCANCODE% = 27 THEN 45
     IF I = 1 THEN 3350
     IF I = 2 THEN 3375
     IF I = 3 THEN 3400
     IF I = 4 THEN 3425
     IF I = 5 THEN 3450
     IF I = 6 THEN 3475
     IF I = 7 THEN CALL PITCHAROUND: GOTO 3300
     IF I = 8 THEN CALL THROWHOME: GOTO 3300

3350 REM:INFIELD IN
     CLS
     IF MG%(690) <> 999 THEN MG%(690) = 999: FOR I = 1 TO 16: MG%(690 + I) = 0: NEXT
     LOCATE 1, 30: COLOR 14, 2: PRINT "INFIELD IN WITH 3RD BASE OCCUPIED": COLOR 15, 0: PRINT
     PRINT "IF DOWN BY "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " RUNS"
     PRINT "AND NOT MORE THAN "; : COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " RUNS"
     PRINT "FROM "; : COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT "INNING ON"
     COLOR 15, 1: PRINT " D "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                              A     B   C     D"
     PRINT "1 - 0 OUTS                           "; USING "##    ##   #   ###"; MG%(691); MG%(692); MG%(693); MG%(694)
     PRINT "2 - 0 OUTS                           "; USING "##    ##   #   ###"; MG%(695); MG%(696); MG%(697); MG%(698)
     PRINT "3 - <2 OUTS, RUNNER ON 1ST           "; USING "##    ##   #   ###"; MG%(699); MG%(700); MG%(701); MG%(702)
     PRINT "4 - <2 OUTS, RUNNER ON 1ST           "; USING "##    ##   #   ###"; MG%(703); MG%(704); MG%(705); MG%(706)
     PRINT "5 - ACCEPT CHOICES"
3355 LOCATE 22, 1: INPUT "ENTER SELECTION (1-5) "; I
     IF I < 1 OR I > 5 THEN 3355
     IF I = 5 THEN CALL UPDATEFILE: GOTO 3300
     I = I - 1
     LOCATE 22, 1: INPUT "AT LEAST DOWN BY          "; MG%(I * 4 + 691)
     LOCATE 22, 1: INPUT "BUT NOT MORE THAN         "; MG%(I * 4 + 692)
     LOCATE 22, 1: INPUT "THIS INNING ON            "; MG%(I * 4 + 693)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 4 + 694)
     GOTO 3350

3375 REM:DEFENSIVE STRATEGIES"
     CLS
     IF MG%(707) <> 999 THEN MG%(707) = 999: FOR I = 1 TO 6: MG%(707 + I) = 0: NEXT
     LOCATE 1, 22: COLOR 14, 2: PRINT "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": COLOR 15, 0: PRINT
     PRINT "IF BUNT RATING IS >= "; : COLOR 15, 1: PRINT " A ": COLOR 15, 0
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "      A      B"
     PRINT USING "(1)  ##    ###"; MG%(708); MG%(709)
     PRINT USING "(2)  ##    ###"; MG%(710); MG%(711)
     PRINT USING "(3)  ##    ###"; MG%(712); MG%(713)
     PRINT "(4) - ACCEPT CHOICES"
3378 LOCATE 22, 1: INPUT "ENTER SELECTION (1-4) "; I
     IF I < 1 OR I > 4 THEN 3378
     IF I = 4 THEN CALL UPDATEFILE: GOTO 3300
     I = I - 1
     LOCATE 22, 1: INPUT "BUNT RATING >=            "; MG%(I * 2 + 708)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 2 + 709)
     GOTO 3375
3400 REM:HITTER SAC BUNT
     CLS
     IF MG%(714) <> 999 THEN MG%(714) = 999: FOR I = 1 TO 16: MG%(714 + I) = 0: NEXT
     LOCATE 1, 20: COLOR 14, 2: PRINT "CORNERS IN VS HITTER IN SAC HIT SITUATION": COLOR 15, 0: PRINT
     PRINT "IF AHEAD BY  "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " RUNS OR LESS"
     PRINT "OR BEHIND BY NOT MORE THAN "; : COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " RUNS"
     PRINT "BUNT RATING >= "; : COLOR 15, 1: PRINT " C ": COLOR 15, 0
     COLOR 15, 1: PRINT " D "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                              A     B   C     D"
     PRINT "1 - 0 OUTS, RUNNER ON 1ST            "; USING "##    ##   #   ###"; MG%(715); MG%(716); MG%(717); MG%(718)
     PRINT "2 - 0 OUTS, RUNNER ON 1ST            "; USING "##    ##   #   ###"; MG%(719); MG%(720); MG%(721); MG%(722)
     PRINT "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; USING "##    ##   #   ###"; MG%(723); MG%(724); MG%(725); MG%(726)
     PRINT "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; USING "##    ##   #   ###"; MG%(727); MG%(728); MG%(729); MG%(730)
     PRINT "5 - ACCEPT CHOICES"
3405 LOCATE 22, 1: INPUT "ENTER SELECTION (1-5) "; I
     IF I < 1 OR I > 5 THEN 3405
     IF I = 5 THEN CALL UPDATEFILE: GOTO 3300
     I = I - 1
     LOCATE 22, 1: INPUT "AHEAD BY NOT MORE THAN    "; MG%(I * 4 + 715)
     LOCATE 22, 1: INPUT "DOWN BY NOT MORE THAN     "; MG%(I * 4 + 716)
     LOCATE 22, 1: INPUT "BUNT RATING >=            "; MG%(I * 4 + 717)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 4 + 718)
     GOTO 3400

3425 REM:PITCHER SAC BUNT
     CLS
     IF MG%(731) <> 999 THEN MG%(731) = 999: FOR I = 1 TO 8: MG%(731 + I) = 0: NEXT
     LOCATE 1, 20: COLOR 14, 2: PRINT "CORNERS IN VS PITCHER IN SAC HIT SITUATION": COLOR 15, 0: PRINT
     PRINT "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO "; : COLOR 15, 1: PRINT " A "
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                                  A      B"
     PRINT "1 - <= 1 OUT, RUNNER ON 1ST              "; USING "###   ###"; MG%(732); MG%(733)
     PRINT "2 - <= 1 OUT, RUNNER ON 1ST              "; USING "###   ###"; MG%(734); MG%(735)
     PRINT "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; USING "###   ###"; MG%(736); MG%(737)
     PRINT "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; USING "###   ###"; MG%(738); MG%(739)
     PRINT "5 - ACCEPT CHOICES"
3430 LOCATE 22, 1: INPUT "ENTER SELECTION (1-5) "; I
     IF I < 1 OR I > 5 THEN 3430
     IF I = 5 THEN CALL UPDATEFILE: GOTO 3300
     I = I - 1
     LOCATE 22, 1: INPUT "BATTING AVG LESS THAN     "; MG%(I * 2 + 732)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 2 + 733)
     GOTO 3425

3450 REM:HITTER SAC BUNT SQUEEZE
     CLS
     IF MG%(740) <> 999 THEN MG%(740) = 999: FOR I = 1 TO 8: MG%(740 + I) = 0: NEXT
     LOCATE 1, 20: COLOR 14, 2: PRINT "CORNERS IN VS HITTER IN SQUEEZE SITUATION": COLOR 15, 0: PRINT
     PRINT "IF AHEAD BY  "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " RUNS OR LESS"
     PRINT "OR BEHIND BY NOT MORE THAN "; : COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " RUNS"
     PRINT "BUNT RATING >= "; : COLOR 15, 1: PRINT " C ": COLOR 15, 0
     COLOR 15, 1: PRINT " D "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                              A     B   C     D"
     PRINT "1 - 0 OUT, RUNNER ON 3RD             "; USING "##    ##   #   ###"; MG%(741); MG%(742); MG%(743); MG%(744)
     PRINT "2 - 1 OUT, RUNNER ON 3RD             "; USING "##    ##   #   ###"; MG%(745); MG%(746); MG%(747); MG%(748)
     PRINT "3 - ACCEPT CHOICES"
3455 LOCATE 22, 1: INPUT "ENTER SELECTION (1-3) "; I
     IF I < 1 OR I > 3 THEN 3455
     IF I = 3 THEN CALL UPDATEFILE: GOTO 3300
     I = I - 1
     LOCATE 22, 1: INPUT "AHEAD BY NOT MORE THAN    "; MG%(I * 4 + 741)
     LOCATE 22, 1: INPUT "DOWN BY NOT MORE THAN     "; MG%(I * 4 + 742)
     LOCATE 22, 1: INPUT "BUNT RATING >=            "; MG%(I * 4 + 743)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 4 + 744)
     GOTO 3450

3475 REM:GUARD LINES
     CLS
     IF MG%(749) <> 999 THEN MG%(749) = 999: FOR I = 1 TO 6: MG%(749 + I) = 0: NEXT
     LOCATE 1, 30: COLOR 14, 2: PRINT "GUARD LINES STRATEGY": COLOR 15, 0: PRINT
     PRINT "IF "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " RUNS AHEAD"
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " INNING ON"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "     A     B      C"
     PRINT "1 - "; USING "##    ##    ###"; MG%(750); MG%(751); MG%(752)
     PRINT "2 - "; USING "##    ##    ###"; MG%(753); MG%(754); MG%(755)
     PRINT "3 - ACCEPT CHOICES"
3476 LOCATE 22, 1: INPUT "ENTER SELECTION (1-3) "; I
     IF I < 1 OR I > 3 THEN 3476
     IF I = 3 THEN CALL UPDATEFILE: GOTO 3300
     I = I - 1
     LOCATE 22, 1: INPUT "RUNS AHEAD                "; MG%(I * 3 + 750)
     LOCATE 22, 1: INPUT "INNING ON                 "; MG%(I * 3 + 751)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 3 + 752)
     GOTO 3475
3501 REM:PINCH RUNNING STRATEGY
     CLS
     PRINT TAB(31); : COLOR 14, 2: PRINT "PINCH RUNNING STRATEGIES": COLOR 15, 0: PRINT
     ITEM$(1) = "SELECT PINCH RUNNERS                                  "
     ITEM$(2) = "SELECT PLAYERS TO PINCH RUN FOR AND THEIR REPLACEMENTS"
     ITEM$(3) = "SITUATIONS TO PINCH RUN FOR THE PITCHER               "
     ITEM$(4) = "RETURN TO MAIN MENU                                   "
   SELECTION% = 1
   Count% = 4
   Start% = 1
   SCANCODE% = 4
   FileColr% = 31
   HiLiteColr% = 112
   Spacing% = 1
   Row% = 5
   Column% = 13
   CALL Box0(4, 12, 9, 67, 2, 31)
   CALL SHOWCURSOR
   MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
   CALL HIDECURSOR
   COLOR , 0
     I = SELECTION%
     IF I = 4 OR SCANCODE% = 27 THEN 45
     IF I = 1 THEN 3500
     IF I = 2 THEN 3600
     IF I = 3 THEN 3680
3500 REM:PINCH RUNNERS
    CLS : PRINT TAB(26); : COLOR 14, 4: PRINT "PINCH RUNNING STRATEGY OPTIONS": PRINT : PRINT : COLOR , 0
    IF MG%(771) <> 999 THEN MG%(771) = 999: FOR I = 1 TO 6: MG%(I + 771) = -1: NEXT
    CLS
    LOCATE 1, 1
    PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
    LOCATE , 16: PRINT "RUN RATING SB   CS"
    I1 = 0
    FOR I = 0 TO 22: LOCATE , 1
    IF B$(I) = "XXX" OR LEFT$(B$(I), 1) = " " OR B$(I) = "" THEN I1 = I1 + 1: GOTO 3505
    LOCATE I + 2 - I1, 1
    COLOR 2, 0
    IF I < 10 THEN PRINT USING "#"; I; : COLOR 15, 0: GOSUB 1980
    IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0: GOSUB 1980
    LOCATE , 3: PRINT B$(I); : COLOR 15, 0
    LOCATE , 17: PRINT USING "   #     ###  ###"; B%(I, 16); B%(I, 13); B%(I, 14);
3505 NEXT
    LOCATE 1, 46
    PRINT "# PLAYER"
    FOR I = 1 TO 6
    LOCATE , 46
    PRINT USING "# "; I;
    IF MG%(I + 771) < 0 THEN PRINT : GOTO 3510
    PRINT B$(MG%(I + 771))
3510 NEXT
    LOCATE 9, 46: PRINT "(7) ACCEPT CHOICES"
    LOCATE 10, 46: PRINT "(8) DELETE A CHOICE"
3515 LOCATE 11, 46: INPUT "ENTER SELECTION (1-8)"; I
     IF I < 1 OR I > 8 THEN 3515
     IF I >= 1 AND I <= 6 THEN GOSUB 3520: GOTO 3500
     IF I = 7 THEN CALL UPDATEFILE: GOTO 3501
     IF I = 8 THEN GOSUB 3525: GOTO 3500
3520 LOCATE 12, 46: PRINT USING "SELECT RUNNER #:"; I;
3517 I$ = INKEY$: IF I$ = "" THEN 3517
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I + 771) = I1
     RETURN
3525 LOCATE 12, 46: INPUT "SELECT CHOICE TO DELETE: "; I
     IF I < 1 OR I > 6 THEN 3525
     MG%(I + 771) = -1
     RETURN

3600 REM:DEFENSIVE REPLACEMENTS
    CLS
    LOCATE 20, 46: PRINT "DEFENSIVE SUBS FOR PINCH RUNNERS"
    LOCATE 21, 46: PRINT "SCORE TIED OR DOWN BY ONE RUN"
    IF MG%(778) <> 999 THEN MG%(778) = 999: FOR I = 1 TO 24: MG%(I + 778) = -1: NEXT
    LOCATE 1, 1
    PRINT "#"; : COLOR L%(11), L%(12): PRINT T$;
    LOCATE , 17: PRINT "1 FAVG 2 FAVG 3 FAVG 4 FAVG"
    I1 = 0
    FOR I = 0 TO 22: LOCATE , 1
    IF B$(I) = "XXX" OR LEFT$(B$(I), 1) = " " OR B$(I) = "" THEN I1 = I1 + 1: GOTO 3605
    LOCATE I + 2 - I1, 1
    COLOR 2, 0
    IF I < 10 THEN PRINT USING "#"; I; : COLOR 15, 0: GOSUB 1980
    IF I > 9 THEN PRINT X$(I - 10); : COLOR 15, 0: GOSUB 1980
    LOCATE , 3: PRINT B$(I); : COLOR 15, 0
    LOCATE , 17: IF B%(I, 22) <> -1 THEN PRINT USING "# ####"; B%(I, 22); B%(I, 26);
    LOCATE , 24: IF B%(I, 23) <> -1 THEN PRINT USING "# ####"; B%(I, 23); B%(I, 27);
    LOCATE , 31: IF B%(I, 24) <> -1 THEN PRINT USING "# ####"; B%(I, 24); B%(I, 28);
    LOCATE , 38: IF B%(I, 25) <> -1 THEN PRINT USING "# ####"; B%(I, 25); B%(I, 29);
3605 NEXT
    LOCATE 1, 46
    PRINT "# PLAYER     I  SUBSTITUTE  %"
    FOR I = 0 TO 5
    LOCATE , 46
    PRINT USING "# "; I + 1;
    IF MG%(I * 4 + 779) < 0 THEN PRINT : GOTO 3610
    PRINT LEFT$(B$(MG%(I * 4 + 779)), 10); TAB(59);
    PRINT USING "## "; MG%(I * 4 + 780);
    PRINT LEFT$(B$(MG%(I * 4 + 781)), 10); TAB(73); USING "###"; MG%(I * 4 + 782)
3610 NEXT
    LOCATE 9, 46: PRINT "(7) ACCEPT CHOICES"
    LOCATE 10, 46: PRINT "(8) DELETE A CHOICE"
3611 LOCATE 11, 46: INPUT "ENTER SELECTION (1-8)"; I
     IF I < 1 OR I > 8 THEN 3611
     IF I >= 1 AND I <= 6 THEN GOSUB 3615: GOTO 3600
     IF I = 7 THEN CALL UPDATEFILE: GOTO 3501
     IF I = 8 THEN GOSUB 3625: GOTO 3600
3615 I = I - 1
     LOCATE 12, 46: PRINT USING "SELECT HITTER #:"; I;
3616 I$ = INKEY$: IF I$ = "" THEN 3616
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I * 4 + 779) = I1
     LOCATE 13, 46: INPUT "INNING "; I1
     MG%(I * 4 + 780) = I1
     LOCATE 14, 46: PRINT "SELECT SUB #:";
3618 I$ = INKEY$: IF I$ = "" THEN 3618
     GOSUB 1290
     I1 = VAL(I$)
     MG%(I * 4 + 781) = I1
3620 LOCATE 15, 46: INPUT "ENTER % OF TIME TO USE "; MG%(I * 4 + 782)
     IF MG%(I * 4 + 782) < 0 OR MG%(I * 4 + 782) > 100 THEN 3620
     RETURN
3625 LOCATE 12, 46: INPUT "SELECT CHOICE TO DELETE: "; I
     IF I < 1 OR I > 6 THEN 298
     MG%(I * 4 + 779) = -1: MG%(I * 4 + 780) = 0: MG%(I * 4 + 781) = 0: MG%(I * 4 + 782) = 0
     RETURN

3680 REM:PINCH RUN PITCHER
     CLS
     IF MG%(803) <> 999 THEN MG%(803) = 999: FOR I = 1 TO 6: MG%(803 + I) = 0: NEXT
     LOCATE 1, 30: COLOR 14, 2: PRINT "PINCH RUN FOR PITCHER": COLOR 15, 0: PRINT
     PRINT "RUN RATING <= "; : COLOR 15, 1: PRINT " A ": COLOR 15, 0
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " INNING ON"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                A     B      C"
     PRINT "1 - TIED OR DOWN BY 1 "; USING "###    ##    ###"; MG%(804); MG%(805); MG%(806)
     PRINT "2 - TIED OR DOWN BY 1 "; USING "###    ##    ###"; MG%(807); MG%(808); MG%(809)
     PRINT "3 - ACCEPT CHOICES"
3685 LOCATE 22, 1: INPUT "ENTER SELECTION (1-3) "; I
     IF I < 1 OR I > 3 THEN 3685
     IF I = 3 THEN CALL UPDATEFILE: GOTO 3501
     I = I - 1
     LOCATE 22, 1: INPUT "RUN RATING <=             "; MG%(I * 3 + 804)
     LOCATE 22, 1: INPUT "INNING ON                 "; MG%(I * 3 + 805)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 3 + 806)
     GOTO 3680
6000 JJ = 1
   LOCATE 6, 8: COLOR 15, 1
   PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
   FOR II = 1 TO JJ: LOCATE 6 + II, 8
   PRINT " º"; STRING$(61, 32); "º "; : NEXT
   LOCATE 7 + JJ, 8
   PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
   RETURN

3800  GOSUB 1445
      GOSUB 4512
      IF MG%(0) = 999 THEN 3820
REM: NO MANAGER FILE WAS FOUND, THUS UNABLE TO EXPORT ONE
     LOCATE 10, 8
     CALL CSMsgBox("NO MANAGER PROFILE EXISTS FOR THIS TEAM PRESS ANY KEY TO RETURN TO MAIN MENU", 40, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 3895
3820 IF PP% = 1 THEN PP% = 0: GOTO 3850
     CALL DRAWBOX
     LOCATE , 11: INPUT "ENTER 8 CHARACTER NAME FOR PROFILE"; PF$
     FOR X = 1 TO 8
     IF MID$(PF$, X, 1) = " " OR MID$(PF$, X, 1) = "." THEN MID$(PF$, X, 1) = "_"
     NEXT X
     OPEN D$(0) + PF$ + "." + "MGR" FOR OUTPUT AS #1
     FOR I = 0 TO 999
     PRINT #1, MG%(I)
     NEXT
     CLOSE 1
     GOTO 3895
3850 REM: PRINT THE MANAGER PROFILE

3895 RETURN

3900 Spec$ = D$(0) + "*.MGR"
   Count = FCount%(Spec$)
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)

3905 CLS
    COLOR , 1: CLS : LOCATE , 28: COLOR 14
    PRINT "SELECT PROFILE TO IMPORT": COLOR 15
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
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF EXITCODE% = 27 THEN 3925
     F$ = F$(SELECTION%)
     LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
     IF UCASE$(I$) = "Y" THEN 3905
     L% = 0
     CALL DRAWBOX
     LOCATE , 11: PRINT  "IMPORT STRATEGY PERCENTAGES ONLY (YN)"
3909 I$=INKEY$: IF I$="" THEN 3909
     IF UCASE$(I$)="Y" THEN L%=1

    OPEN D$(0) + F$ FOR INPUT AS #1
    IF L%=0 THEN FOR I = 0 TO 999:INPUT #1, MG%(I):NEXT
    IF L%=1 THEN FOR I = 0 TO 580:INPUT #1, L%:NEXT:FOR I = 581 TO 769:INPUT #1, MG%(I):NEXT:FOR I = 770 TO 802:INPUT #1, L%:NEXT:FOR I = 803 TO 999:INPUT #1, MG%(I):NEXT
    CLOSE 1
    CALL UPDATEFILE

3925 RETURN

SUB BASERUN ()
3700 REM:BASE RUNNING STRATEGY
     CLS
     IF MG%(810) <> 999 THEN MG%(810) = 999: FOR I = 1 TO 36: MG%(810 + I) = 0: NEXT
     LOCATE 1, 30: COLOR 14, 2: PRINT "BASE RUNNING STRATEGY": COLOR 15, 0: PRINT
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                            %       %          %          %"
     PRINT "                                  SAFE    NORMAL   AGGRESSIVE   TOTAL"
     PRINT " 1 - INNING 1-3, WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(811); MG%(812); MG%(813); MG%(811) + MG%(812) + MG%(813)
     PRINT " 2 - INNING 1-3, WHEN TIED        "; USING "###     ###        ###          ###"; MG%(814); MG%(815); MG%(816); MG%(816) + MG%(815) + MG%(814)
     PRINT " 3 - INNING 1-3, WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(817); MG%(818); MG%(819); MG%(819) + MG%(818) + MG%(817)
     PRINT " 4 - INNING 4-6, WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(820); MG%(821); MG%(822); MG%(820) + MG%(821) + MG%(822)
     PRINT " 5 - INNING 4-6, WHEN TIED        "; USING "###     ###        ###          ###"; MG%(823); MG%(824); MG%(825); MG%(823) + MG%(824) + MG%(825)
     PRINT " 6 - INNING 4-6, WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(826); MG%(827); MG%(828); MG%(826) + MG%(827) + MG%(828)
     PRINT " 7 - INNING 7-9, WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(829); MG%(830); MG%(831); MG%(829) + MG%(830) + MG%(831)
     PRINT " 8 - INNING 7-9, WHEN TIED        "; USING "###     ###        ###          ###"; MG%(832); MG%(833); MG%(834); MG%(832) + MG%(833) + MG%(834)
     PRINT " 9 - INNING 7-9, WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(835); MG%(836); MG%(837); MG%(835) + MG%(836) + MG%(837)
     PRINT "10 - INNING 9- , WHEN AHEAD       "; USING "###     ###        ###          ###"; MG%(838); MG%(839); MG%(840); MG%(838) + MG%(839) + MG%(840)
     PRINT "11 - INNING 9- , WHEN TIED        "; USING "###     ###        ###          ###"; MG%(841); MG%(842); MG%(843); MG%(841) + MG%(842) + MG%(843)
     PRINT "12 - INNING 9- , WHEN BEHIND      "; USING "###     ###        ###          ###"; MG%(844); MG%(845); MG%(846); MG%(844) + MG%(845) + MG%(846)
     PRINT "13 - ACCEPT CHOICES"
3705 LOCATE 22, 1: INPUT "ENTER SELECTION (1-13) "; I
     IF I < 1 OR I > 13 THEN 3705
     IF I = 13 THEN GOSUB 3810: GOTO 3815
     I = I - 1
     LOCATE 22, 1: INPUT "SAFE                 "; MG%(I * 3 + 811)
     LOCATE 22, 1: INPUT "NORMAL               "; MG%(I * 3 + 812)
     LOCATE 22, 1: INPUT "AGGRESSIVE           "; MG%(I * 3 + 813)
     GOTO 3700
3810 OPEN D$(3) + "MGR." + YN$ FOR RANDOM AS #2 LEN = 2000
     FOR I = 0 TO 999
     FIELD #2, I * 2 AS Q$, 2 AS R$(I), 2000 - I * 2 - 2 AS Q$
     NEXT
     FOR I = 0 TO 999: LSET R$(I) = MKI$(MG%(I)): NEXT
     PUT #2, T: CLOSE 2
     RETURN
3815 END SUB

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

SUB DRAWBOX ()
   JJ = 5
   LOCATE 6, 8: COLOR 15, 1
   PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
   FOR II = 1 TO JJ: LOCATE 6 + II, 8
   PRINT " º"; STRING$(61, 32); "º "; : NEXT
   LOCATE 7 + JJ, 8
   PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
END SUB

SUB PHBATTER ()
3059 REM:PINCH HIT FOR THE PITCHER
     IF MG%(847) <> 999 THEN MG%(847) = 999: FOR I = 848 TO 865: MG%(I) = 0: NEXT
     CLS
     LOCATE 1, 26: COLOR 14, 4: PRINT "PINCH HITTING FOR THE BATTER": COLOR 15, 0
     PRINT
     PRINT "PINCH HIT FOR THE BATTER FROM "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0
     PRINT " INNING ON"
     PRINT "IF DOWN BY "; : COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " OR MORE RUNS AND"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " RUNNERS ON BASE AND"
     PRINT "SLUGGING PCT IS LESS THAN "; : COLOR 15, 1: PRINT " D "; : COLOR 15, 0
     PRINT "AND BATTING AVERAGE IS LESS THAN "; : COLOR 15, 1: PRINT " E "
     PRINT " F "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO        A      B      C      D       E     F"
     FOR I = 0 TO 2
     PRINT USING "   #           ##     ##      #     ###    ###   ###"; I + 1; MG%(I * 6 + 848); MG%(I * 6 + 849); MG%(I * 6 + 850); MG%(I * 6 + 851); MG%(I * 6 + 852); MG%(I * 6 + 853)
     NEXT
     PRINT
     PRINT "   4 - ACCEPT CHOICES"
     PRINT
3057 LOCATE 17, 1: INPUT "ENTER SELECTION (1-4) "; I
     IF I < 1 OR I > 4 THEN 3057
     IF I = 4 THEN CALL UPDATEFILE: GOTO 3058
     I = I - 1
     INPUT "INNING TO BEGIN PINCH HITTING FOR POSITION PLAYERS "; MG%(I * 6 + 848)
     INPUT "DOWN BY HOW MANY RUNS "; MG%(I * 6 + 849)
     INPUT "RUNNERS ON BASE "; MG%(I * 6 + 850)
     INPUT "SLUGGING PCT LESS THAN "; MG%(I * 6 + 851)
     INPUT "BATTING AVERAGE LESS THAN "; MG%(I * 6 + 852)
     INPUT "PERCENT OF TIME "; MG%(I * 6 + 853)
     GOTO 3059
3058 END SUB

SUB PHPITCHER ()
3050 REM:PINCH HIT FOR THE PITCHER
     IF MG%(590) <> 999 THEN MG%(590) = 999: FOR I = 591 TO 608: MG%(I) = 0: NEXT
     CLS
     LOCATE 1, 26: COLOR 14, 4: PRINT "PINCH HITTING FOR THE PITCHER": COLOR 15, 0
     PRINT
     PRINT "PINCH HIT FOR THE PITCHER FROM "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0
     PRINT " INNING ON"
     PRINT "IF DOWN BY "; : COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " OR MORE RUNS AND"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " OR MORE RUNNERS ON BASE AND"
     COLOR 15, 1: PRINT " D "; : COLOR 15, 0
     PRINT " OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN "; : COLOR 15, 1: PRINT " E "
     PRINT " F "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO        A      B      C      D     E     F"
     FOR I = 0 TO 2
     PRINT USING "   #           ##     ##      #      #    ###   ###"; I + 1; MG%(I * 6 + 591); MG%(I * 6 + 592); MG%(I * 6 + 593); MG%(I * 6 + 594); MG%(I * 6 + 595); MG%(I * 6 + 596)
     NEXT
     PRINT
     PRINT "   4 - ACCEPT CHOICES"
     PRINT
3055 LOCATE 17, 1: INPUT "ENTER SELECTION (1-4) "; I
     IF I < 1 OR I > 4 THEN 3055
     IF I = 4 THEN CALL UPDATEFILE: GOTO 3056
     I = I - 1
     INPUT "ENTER INNING TO BEGIN PINCH HITTING FOR PITCHER "; MG%(I * 6 + 591)
     INPUT "DOWN BY HOW MANY RUNS "; MG%(I * 6 + 592)
     INPUT "RUNNERS ON BASE "; MG%(I * 6 + 593)
     INPUT "NUMBER OF OUTS OR LESS "; MG%(I * 6 + 594)
     INPUT "PITCHER HITS LESS THAN "; MG%(I * 6 + 595)
     INPUT "PERCENT OF TIME "; MG%(I * 6 + 596)
     GOTO 3050
3056 END SUB

SUB PITCHAROUND ()
3480 REM:PITCH AROUND
     CLS
     IF MG%(756) <> 999 THEN MG%(756) = 999: FOR I = 1 TO 9: MG%(756 + I) = 0: NEXT
     LOCATE 1, 30: COLOR 14, 2: PRINT "PITCH AROUND STRATEGY": COLOR 15, 0: PRINT
     PRINT "SLUGGING PCT >= "; : COLOR 15, 1: PRINT " A ": COLOR 15, 0
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " INNING ON"
     COLOR 15, 1: PRINT " C "; : COLOR 15, 0: PRINT " % OF THE TIME"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "SCENARIO                                               A     B      C"
     PRINT "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; USING "###    ##    ###"; MG%(757); MG%(758); MG%(759)
     PRINT "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; USING "###    ##    ###"; MG%(760); MG%(761); MG%(762)
     PRINT "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; USING "###    ##    ###"; MG%(763); MG%(764); MG%(765)
     PRINT "4 - ACCEPT CHOICES"
3485 LOCATE 22, 1: INPUT "ENTER SELECTION (1-4) "; I
     IF I < 1 OR I > 4 THEN 3485
     IF I = 4 THEN CALL UPDATEFILE: GOTO 3486
     I = I - 1
     LOCATE 22, 1: INPUT "SLUGGING PCT >=           "; MG%(I * 3 + 757)
     LOCATE 22, 1: INPUT "INNING ON                 "; MG%(I * 3 + 758)
     LOCATE 22, 1: INPUT "PERCENT OF TIME           "; MG%(I * 3 + 759)
     GOTO 3480
3486 END SUB

SUB PITSTRAT ()
400 REM:PITCHING STRATEGY
    CLS
    IF MG%(581) <> 999 THEN MG%(581) = 999: FOR I = 582 TO 587: MG%(I) = 0: NEXT: MG%(588) = 8: MG%(589) = 0
    IF MG%(588) < 8 THEN MG%(588) = 8
    IF MG%(588) > 9 THEN MG%(588) = 9
    PRINT TAB(31); : COLOR 14, 4: PRINT "CLOSING STRATEGY": COLOR 15, 0
    PRINT
    COLOR 12: PRINT "STRATEGY                                        % OF TIME"
    COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; MG%(588)
    COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "USE BOTH SETUP RELIEVER AND CLOSER "; YN$(MG%(589)): PRINT

    COLOR 14:               PRINT "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
    COLOR 14: PRINT "(3) "; : COLOR 15: PRINT USING "START INNING IF SAVE OPPORTUNITY               ###"; MG%(582)
    COLOR 14: PRINT "(4) "; : COLOR 15: PRINT USING "START INNING IF AHEAD                          ###"; MG%(583)
    COLOR 14: PRINT "(5) "; : COLOR 15: PRINT USING "START INNING IF TIED OR AHEAD                  ###"; MG%(584)
    COLOR 14: PRINT "(6) "; : COLOR 15: PRINT USING "IF CURRENT PITCHER TIRED AND AHEAD             ###"; MG%(585)
    COLOR 14: PRINT "(7) "; : COLOR 15: PRINT USING "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; MG%(586)
    COLOR 14: PRINT "(8) "; : COLOR 15: PRINT USING "WHEN SAVE OPPORTUNITY OCCURS                   ###"; MG%(587)
    COLOR 14: PRINT "(9) "; : COLOR 15: PRINT "ACCEPT CHOICES"
405 I$ = INKEY$: IF I$ = "" THEN 405
    I = VAL(I$): IF I < 1 OR I > 9 THEN 405
    IF I = 9 THEN CALL UPDATEFILE: GOTO 415
    IF I = 1 THEN LOCATE 21, 1: INPUT "ENTER INNING "; MG%(588): GOTO 400
    IF I = 2 THEN MG%(589) = 1 - MG%(589): GOTO 400
410 LOCATE 21, 1: INPUT "% OF TIME TO USE "; I1
    IF I1 < 0 OR I1 > 100 THEN 410
    MG%(I - 2 + 581) = I1: GOTO 400
415 END SUB

SUB PROFILEPRINT ()
END SUB

SUB THROWHOME ()
3490 REM:THROW HOME UNDER
     CLS
     IF MG%(766) <> 999 THEN MG%(766) = 999: FOR I = 1 TO 4: MG%(766 + I) = 0: NEXT
     LOCATE 1, 18: COLOR 14, 2: PRINT "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": COLOR 15, 0: PRINT
     PRINT "FROM "; : COLOR 15, 1: PRINT " A "; : COLOR 15, 0: PRINT " INNING ON"
     COLOR 15, 1: PRINT " B "; : COLOR 15, 0: PRINT " OR MORE RUNS AHEAD"
     PRINT TAB(32); : COLOR 14: PRINT "TABLE OF VALUES": COLOR 15
     PRINT "      A      B"
     PRINT USING "(1)  ##    ###"; MG%(767); MG%(768)
     PRINT USING "(2)  ##    ###"; MG%(769); MG%(770)
     PRINT "(3) - ACCEPT CHOICES"
3495 LOCATE 22, 1: INPUT "ENTER SELECTION (1-3) "; I
     IF I < 1 OR I > 3 THEN 3495
     IF I = 3 THEN CALL UPDATEFILE: GOTO 3496
     I = I - 1
     LOCATE 22, 1: INPUT "INNING ON TO USE          "; MG%(I * 2 + 767)
     LOCATE 22, 1: INPUT "AHEAD BY                  "; MG%(I * 2 + 768)
     GOTO 3490
3496 END SUB

SUB UPDATEFILE ()
     MG%(0) = 999
     OPEN D$(3) + "MGR." + YN$ FOR RANDOM AS #2 LEN = 2000
     FOR I = 0 TO 999
     FIELD #2, I * 2 AS Q$, 2 AS R$(I), 2000 - I * 2 - 2 AS Q$
     NEXT
     FOR I = 0 TO 999: LSET R$(I) = MKI$(MG%(I)): NEXT
     PUT #2, T: CLOSE 2
END SUB

