DEFINT A-Z
DECLARE SUB DRAWBOX (J)
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB FCopy (Source$, Dest$, Buffer$, ErrCode%)
TYPE PBP
YY AS STRING * 30
END TYPE
  DIM K9(1) AS SINGLE
  DIM S$(1), A$(1), L%(1, 13), T1(1), B%(1, 22, 80), P%(1, 21, 89), B$(1, 22), P$(1, 21), P8%(1, 21, 41), h$(4)
  DIM B8%(1, 22, 21), P6%(1), P1%(1), S8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
  DIM S%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), C$(11), F$(10)
  DIM VV(1), VV1(1), D3%(1), M%(9), P2%(1), A5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
  DIM X0%(1, 2, 23), D0%(1), P5%(1, 21, 41), B5%(1, 22, 21), TP%(11, 2), L0%(1), T3%(22)
  DIM E%(13)
  DIM W$(162),HT$(162)
  DIM CSS%(1,22,6), SS%(22,6)
  DIM INJ%(1,22), DYS%(1,21)
  DIM MG%(1,999),TS%(1,11),SA%(1,24)
  DIM NG%(18), H0$(1), TM$(4), SC$(4), PC$(4), N$(1), D$(3), M$(1)
  DIM H0%(1), CZ%(1), BP(1), BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2)
  DIM PR%(24), GK%(1)
  DIM DB$(20), TR$(20), HR$(20), E$(20), SB$(20), ING%(162), L$(162), PL$(162), V%(162)
  DIM SX%(1), Q$(23), YN$(1), SA$(1), B2%(1, 22), P3%(1, 21), B6%(22, 21)
  DIM P0%(21, 41), P4%(22), D1%(1), T%(22), H%(162),AP%(2)
COMMON SHARED TS%(),SA%(),P8%(), B8%(), P%(), P$(), B$(), P5%(), B5%(), X0%(), B%(), AP%(), A$()
COMMON SHARED C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
COMMON SHARED S%(), P1%(), S1%(), D$(), S6%(), L%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
COMMON SHARED B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
COMMON SHARED VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), h$(), D0%(), M$(), LB%()
COMMON SHARED F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
COMMON SHARED S$, P, U6, DH%, U9, G4 AS SINGLE, RE, RD
COMMON SHARED TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, CC%, AP%,INNING%,buck as pbp
COMMON SHARED MG%(),CSS%(),D,IJ%,DR%,CGERA%,IJL%,EJ%
DEFLNG A
  DRIVE = GetDrive%
  SPECIAL = 0
  DATA 15,0,15,1,19,4,4,6,3,1,2,3,14
  FOR X = 1 TO 13: READ E%(X): NEXT
'Check for special boxscores
     IF NOT EXIST%("EVENTS.BOX") THEN 102
     OPEN "EVENTS.BOX" FOR INPUT AS #1
     FOR X = 1 TO 13
     INPUT #1, E%(X)
     NEXT
     CLOSE 1
102 IF INNING% > E%(1) THEN GOSUB 7100:GOTO 7105
    IF S6%(1,0) < E%(2) THEN GOSUB 7100:GOTO 7105
    IF S6%(0,0) < E%(2) THEN GOSUB 7100:GOTO 7105
    IF S6%(1,0) > E%(3) THEN GOSUB 7100:GOTO 7105
    IF S6%(0,0) > E%(3) THEN GOSUB 7100:GOTO 7105
    IF S6%(1,1) > E%(5) THEN GOSUB 7100:GOTO 7105
    IF S6%(0,1) > E%(5) THEN GOSUB 7100:GOTO 7105
    IF S6%(1,1) < E%(4) THEN GOSUB 7100:GOTO 7105
    IF S6%(0,1) < E%(4) THEN GOSUB 7100:GOTO 7105

    FOR I=0 TO 1
    FOR I1 = 0 TO 22
    IF B8%(I, I1, 1) > E%(6) THEN GOSUB 7100:GOTO 7105
    IF B8%(I, I1, 2) > E%(7) THEN GOSUB 7100:GOTO 7105
    IF B8%(I, I1, 3) > E%(8) THEN GOSUB 7100:GOTO 7105
    IF B8%(I, I1, 4) > E%(9) THEN GOSUB 7100:GOTO 7105
    IF B8%(I, I1, 5) > E%(10) THEN GOSUB 7100:GOTO 7105
    IF B8%(I, I1, 6) > E%(11) THEN GOSUB 7100:GOTO 7105
    IF B8%(I, I1, 9) > E%(12) THEN GOSUB 7100:GOTO 7105
    NEXT
    NEXT

   FOR I=0 TO 1
   FOR I1 = 0 TO 21
   IF P8%(I, I1, 5) > E%(13) THEN GOSUB 7100:GOTO 7105
   NEXT
   NEXT
   GOTO 7105

7100 Spec$ = D$(1) + "*.SPC"
     Count = FCount%(Spec$)
     COUNT  = COUNT + 1
     SPECIAL = 1
   RETURN

7105 BK$ = "## # # ## # #  # # #  #  # # ## ##"
      FOR I = 0 TO 1: FOR I1 = 0 TO 21: IF P8%(I, I1, 1) < 0 THEN P8%(I, I1, 1) = 0
      NEXT I1: NEXT I
      FOR I = 0 TO 1: FOR I1 = 0 TO 21
     IF P%(I, I1, 15) <> 1 THEN 7250
     IF P8%(I, I1, 0) >= 18 AND P8%(I, I1, 3) <= 3 THEN P8%(I, I1, 21) = 1
7250 NEXT I1
      NEXT I
      FOR I = 0 TO 1: FOR I1 = 0 TO 22
      IF B8%(I, I1, 11) = 0 THEN 7257
      IF B8%(I, I1, 11) = 1 THEN E$(E) = B$(I, I1): GOTO 7256
      E$(E) = B$(I, I1) + STR$(B8%(I, I1, 11))
7256 E$(E) = E$(E) + " (" + RIGHT$(STR$(B5%(I, I1, 11)), (LEN(STR$(B5%(I, I1, 11))) - 1)) + ")": E = E + 1
7257 IF B8%(I, I1, 4) = 0 THEN 7261: REM        ***CHECKING DOUBLES***
     IF B8%(I, I1, 4) = 1 THEN DB$(DB) = B$(I, I1): GOTO 7260
     DB$(DB) = B$(I, I1) + STR$(B8%(I, I1, 4))
7260 DB$(DB) = DB$(DB) + " (" + RIGHT$(STR$(B5%(I, I1, 4)), (LEN(STR$(B5%(I, I1, 4))) - 1)) + ")": DB = DB + 1
7261 IF B8%(I, I1, 5) = 0 THEN 7265: REM        ***CHECKING TRIPLES***
     IF B8%(I, I1, 5) = 1 THEN TR$(TR) = B$(I, I1): GOTO 7264
     TR$(TR) = B$(I, I1) + STR$(B8%(I, I1, 5))
7264 TR$(TR) = TR$(TR) + " (" + RIGHT$(STR$(B5%(I, I1, 5)), (LEN(STR$(B5%(I, I1, 5))) - 1)) + ")": TR = TR + 1
7265 IF B8%(I, I1, 6) = 0 THEN 7269: REM        ***CHECKING HOME RUNS***
     IF B8%(I, I1, 6) = 1 THEN HR$(HR) = B$(I, I1): GOTO 7268
     HR$(HR) = B$(I, I1) + STR$(B8%(I, I1, 6))
7268 HR$(HR) = HR$(HR) + " (" + RIGHT$(STR$(B5%(I, I1, 6)), (LEN(STR$(B5%(I, I1, 6))) - 1)) + ")": HR = HR + 1
7269 IF B8%(I, I1, 9) = 0 THEN 7272: REM        ***CHECKING STOLEN BASES ***
     IF B8%(I, I1, 9) = 1 THEN SB$(SB) = B$(I, I1) + " (" + RIGHT$(STR$(B5%(I, I1, 9)), (LEN(STR$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1: GOTO 7272
     SB$(SB) = B$(I, I1) + STR$(B8%(I, I1, 9)) + " (" + RIGHT$(STR$(B5%(I, I1, 9)), (LEN(STR$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1
7272 NEXT I1: NEXT I: FOR I = 0 TO 1: FOR I1 = 0 TO 21
     IF P8%(I, I1, 34) = 0 THEN 7277
     IF P8%(I, I1, 34) = 1 THEN E$(E) = P$(I, I1): GOTO 7276
     E$(E) = P$(I, I1) + STR$(P8%(I, I1, 34))
7276 E$(E) = E$(E) + " (" + RIGHT$(STR$(P5%(I, I1, 34)), (LEN(STR$(P5%(I, I1, 34))) - 1)) + ")": E = E + 1
7277 IF P8%(I, I1, 27) = 0 THEN 7281: REM        ***CHECKING DOUBLES***
     IF P8%(I, I1, 27) = 1 THEN DB$(DB) = P$(I, I1): GOTO 7280
     DB$(DB) = P$(I, I1) + STR$(P8%(I, I1, 27))
7280 DB$(DB) = DB$(DB) + " (" + RIGHT$(STR$(P5%(I, I1, 27)), (LEN(STR$(P5%(I, I1, 27))) - 1)) + ")": DB = DB + 1
7281 IF P8%(I, I1, 28) = 0 THEN 7285: REM        ***CHECKING TRIPLES***
     IF P8%(I, I1, 28) = 1 THEN TR$(TR) = P$(I, I1): GOTO 7284
     TR$(TR) = P$(I, I1) + STR$(P8%(I, I1, 28))
7284 TR$(TR) = TR$(TR) + " (" + RIGHT$(STR$(P5%(I, I1, 28)), (LEN(STR$(P5%(I, I1, 28))) - 1)) + ")": TR = TR + 1
7285 IF P8%(I, I1, 29) = 0 THEN 7289: REM        ***CHECKING HOME RUNS***
     IF P8%(I, I1, 29) = 1 THEN HR$(HR) = P$(I, I1): GOTO 7288
     HR$(HR) = P$(I, I1) + STR$(P8%(I, I1, 29))
7288 HR$(HR) = HR$(HR) + " (" + RIGHT$(STR$(P5%(I, I1, 29)), (LEN(STR$(P5%(I, I1, 29))) - 1)) + ")": HR = HR + 1
7289 IF P8%(I, I1, 32) = 0 THEN 7292: REM        ***CHECKING STOLEN BASES ***
     IF P8%(I, I1, 32) = 1 THEN SB$(SB) = P$(I, I1) + " (" + RIGHT$(STR$(P5%(I, I1, 32)), (LEN(STR$(P5%(I, I1, 32))) - 1)) + ")": SB = SB + 1: GOTO 7292
     SB$(SB) = P$(I, I1) + STR$(P8%(I, I1, 32)) + " (" + RIGHT$(STR$(P5%(I, I1, 32)), (LEN(STR$(P5%(I, I1, 32))) - 1)) + ")": SB = SB + 1
7292 NEXT I1: NEXT I
7106 GT$ = "2:": ATT = INT(RND(1) * 100): IF ATT < 5 OR ATT > 60 THEN 7106
     ATT = ATT * 1000
7107 MIN = INT(RND(1) * 100): IF MIN > 59 THEN 7107
     IF MIN < 10 THEN MIN$ = "0" + RIGHT$(STR$(MIN), 1): GT$ = GT$ + MIN$ ELSE GT$ = GT$ + RIGHT$(STR$(MIN), 2)
     FOR I = 0 TO 1: FOR I1 = 0 TO 21: IF P%(I, I1, 15) >= 98 THEN P%(I, I1, 15) = 0
     NEXT I1: NEXT I
     FOR I = 0 TO 1: FOR I1 = 0 TO 22: IF B%(I, I1, 21) >= 98 THEN B%(I, I1, 21) = 0
     IF B%(I, I1, 21) <> 0 THEN B8%(I, I1, 14) = 1
     NEXT I1: NEXT I
     CLS : PRINT "PLEASE WAIT..."
     FOR I = 0 TO 1
     M = 23
     L = 22
7363 I6 = 0: I7 = 0
7364 H1 = 0: H2 = 0: H3 = 0
     IF X0%(I, 2, I6) <= X0%(I, 2, I6 + 1) THEN 7370
     H1 = X0%(I, 2, I6): H2 = X0%(I, 1, I6): H3 = X0%(I, 0, I6)
     X0%(I, 0, I6) = X0%(I, 0, I6 + 1): X0%(I, 1, I6) = X0%(I, 1, I6 + 1): X0%(I, 2, I6) = X0%(I, 2, I6 + 1)
     X0%(I, 2, I6 + 1) = H1: X0%(I, 1, I6 + 1) = H2: X0%(I, 0, I6 + 1) = H3
     I7 = I6
7370 I6 = I6 + 1
     IF I6 > L THEN 7373
     GOTO 7364
7373 IF I7 = 0 THEN 7118
     L = I7 - 1: GOTO 7363
7118 NEXT I
     GOTO 7293
7119 IF X = 1 THEN NM$ = "TEMPBOX": GOTO 7121
     IF AP% <> 1 THEN 7120
     OPEN "GAME82" FOR INPUT AS #1
     INPUT #1, FF$
     FOR I = 0 TO 2
     INPUT #1,AP%(I)
     NEXT
     CLOSE 1
REM: NM$ = "BOXSCORE." + MID$(STR$(AP%(0) + AP%(2)), 2, LEN(STR$(AP%(0) + AP%(2))))
     NM$ = "BOX" + MID$(STR$(AP%(0) + AP%(2)), 2, LEN(STR$(AP%(0) + AP%(2))))
     GOTO 7121
7120 CLS : PRINT "ENTER 8-CHARACTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)"; : INPUT NM$:NM$ = NM$ + ".SPC"
7121 OPEN D$(1) + NM$ FOR OUTPUT AS #2
     IF SPECIAL  = 1 THEN GOSUB 7188
     GOSUB 7183
     FOR I = 0 TO 1
     PRINT #2, A$(I) TAB(18)  "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
     FOR I3 = 0 TO 22
     IF X0%(I, 0, I3) = -1 OR X0%(I, 1, I3) = 1 THEN 7127
     I1 = X0%(I, 0, I3)
7124 PRINT #2, B$(I, I1); ", "; C$(X0%(I, 1, I3)); TAB(18);
     PRINT #2, USING BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);
     PRINT #2,"   ";
     IF B5%(I, I1, 0) = 0 OR B5%(I,I1, 1) = 0 THEN PRINT #2, " .000   "; : GOTO 7122
     IF B5%(I, I1, 0) = B5%(I, I1, 1) THEN PRINT #2, "1.000   "; : GOTO 7122
     PRINT #2, USING " .###   "; B5%(I, I1, 1) / B5%(I, I1, 0);
7122 PRINT #2, USING "##  ###";B5%(I, I1, 6);B5%(I, I1, 3)
     FOR I2 = 0 TO 17
     T3%(I2) = T3%(I2) + B8%(I, I1, I2)
     NEXT I2
7127 NEXT I3
     FOR I1 = 0 TO 21: FOR I2 = 0 TO 13: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23): NEXT
     FOR I2 = 14 TO 17: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22): NEXT: NEXT
     FOR I1 = 1 TO P6%(I)
     FOR I2 = 0 TO 21
     IF P%(I, I2, 15) <> I1 THEN 7134
     PRINT #2, P$(I, I2); ", p";
     PRINT #2, TAB(18);
     PRINT #2, USING BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
     PRINT #2, "   ";
     IF P5%(I, I2, 24) = 0 OR P5%(I,I2, 23) = 0 THEN PRINT #2, " .000   "; : GOTO 7128
     IF P5%(I, I2, 24) = P5%(I, I2, 23) THEN PRINT #2, "1.000   "; : GOTO 7128
     PRINT #2, USING " .###   "; P5%(I, I2, 24) / P5%(I, I2, 23);
7128 PRINT #2, USING "##  ###";P5%(I, I2, 29);P5%(I, I2, 26)

7134 NEXT: NEXT
     PRINT #2, "TOTALS"; TAB(17); T3%(0); TAB(22); T3%(1); TAB(27); T3%(4);
     PRINT #2, TAB(32); T3%(6); TAB(36); T3%(7); TAB(42);
     PRINT #2, T3%(10); TAB(46); T3%(16); TAB(50); T3%(17)
     PRINT #2, TAB(20); T3%(2); TAB(24); T3%(3); TAB(29);
     PRINT #2, T3%(5); TAB(34); T3%(8); TAB(39); T3%(9);
     PRINT #2, TAB(44); T3%(11)
     PRINT #2, : FOR I1 = 0 TO 22: T3%(I1) = 0: NEXT I1
     NEXT I


     FOR I = 0 TO 1: PRINT #2, : PRINT #2, A$(I); TAB(23); "       IP   H  R  ER BB SO    ERA   W  L  S  SO"
     FOR I1 = 1 TO P6%(I)
     FOR I2 = 0 TO 21
     IF P%(I, I2, 15) <> I1 THEN 7154
     I4 = 1 - I: PRINT #2, P$(I, I2); " ";
     IF P2%(I) = I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN 7179
7148 GOSUB DSKBOX
     IF P2%(I) = I2 AND S6%(I, 0) - S6%(I4, 0) < 0 THEN 7180
7149 IF P8%(I, I2, 19) > 0 THEN PRINT #2, "(BS,"; : PRINT #2, USING "##"; P5%(I, I2, 19); : PRINT #2, ")";
     IF P8%(I, I2, 22) > 0 THEN PRINT #2, "(H,"; : PRINT #2, USING "##"; P5%(I, I2, 22); : PRINT #2, ")";
     PRINT #2, TAB(29);:PRINT #2, USING "##"; INT(P8%(I, I2, 0) / 3);
     IF P8%(I, I2, 0) - (INT(P8%(I, I2, 0) / 3) * 3) > 0 THEN PRINT #2, "."; : PRINT #2, USING "#"; P8%(I, I2, 0) - (INT(P8%(I, I2, 0) / 3) * 3);
     PRINT #2, TAB(34);
     PRINT #2, USING "## ##  ## ## ##";P8%(I, I2, 1);P8%(I, I2, 2);P8%(I, I2, 3);P8%(I, I2, 4);P8%(I, I2, 5);
     PRINT #2, "   ";
     IF P5%(I,I2,3)>0 AND P5%(I,I2,0)=0 THEN PRINT #2, "99.99 ";:GOTO 7150
     IF P5%(I,I2,0)=0 THEN PRINT #2, "--.-- ";:GOTO 7150
     IF P5%(I,I2,3)=0 THEN PRINT #2, " 0.00 ";:GOTO 7150
     PRINT #2, USING "##.## ";P5%(I,I2,3)/(P5%(I,I2,0)/3)*9;
7150 IF P2%(I)=I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN PRINT #2, USING "## ";P5%(I,I2,6)+1; ELSE PRINT #2, USING "## ";P5%(I,I2,6);
     IF P2%(I)=I2 AND S6%(I, 0) - S6%(I4, 0) < 0 THEN PRINT #2, USING "## ";P5%(I,I2,7)+1; ELSE PRINT #2, USING "## ";P5%(I,I2,7);
     IF S1%(I)=I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN PRINT #2, USING "## ";P5%(I,I2,12)+1; ELSE PRINT #2, USING "## ";P5%(I,I2,12);
     PRINT #2, USING "###";P5%(I,I2,5)
     FOR I4 = 0 TO 5
     T3%(I4) = T3%(I4) + P8%(I, I2, I4)
     NEXT I4
7154 NEXT I2
     NEXT I1
     PRINT #2, "TOTALS"; TAB(28);
     PRINT #2, USING "##"; INT(T3%(0) / 3);
     IF T3%(0) - (INT(T3%(0) / 3) * 3) > 0 THEN PRINT #2, "."; : PRINT #2, USING "#"; T3%(0) - (INT(T3%(0) / 3) * 3);
     PRINT #2, TAB(34);
     PRINT #2, USING "## ##  ## ## ##";T3%(1);T3%(2);T3%(3);T3%(4);T3%(5)
     LYN% = CINT(T3%(0) / 3)
     FOR I1 = 0 TO 5: T3%(I1) = 0: NEXT I1
     NEXT I
     PRINT #2, : PRINT #2, " E-";
     IF E = 0 THEN PRINT #2, "NONE": GOTO 7163
     FOR X1 = 0 TO (E - 1): PRINT #2, E$(X1); ","; : NEXT: PRINT #2,
7163 PRINT #2, "2B-";
     IF DB = 0 THEN PRINT #2, "NONE": GOTO 7166
     FOR X1 = 0 TO (DB - 1): PRINT #2, DB$(X1); ","; : NEXT: PRINT #2,
7166 PRINT #2, "3B-";
     IF TR = 0 THEN PRINT #2, "NONE": GOTO 7169
     FOR X1 = 0 TO (TR - 1): PRINT #2, TR$(X1); ","; : NEXT: PRINT #2,
7169 PRINT #2, "HR-";
     IF HR = 0 THEN PRINT #2, "NONE": GOTO 7172
     FOR X1 = 0 TO (HR - 1): PRINT #2, HR$(X1); ","; : NEXT: PRINT #2,
7172 PRINT #2, "SB-";
     IF SB = 0 THEN PRINT #2, "NONE": GOTO 7175
     FOR X1 = 0 TO (SB - 1): PRINT #2, SB$(X1); ","; : NEXT: PRINT #2,
7175 PRINT #2, "LOB-"; A$(0); STR$(L0%(0)); ","; A$(1); STR$(L0%(1))
     PRINT #2, "DP-"; A$(0); STR$(DP%(0)); ","; A$(1); STR$(DP%(1))
     PRINT #2, "UMPIRES: HP-"; U$(1); "; 1B-"; U$(2); "; 2B-"; U$(3); "; 3B-"; U$(4): PRINT #2, "T: "; GT$; " A: "; ATT
     PRINT #2,
     PRINT #2, "INJURIES-";
     IF IJL% = 0 THEN PRINT #2, "none": GOTO peject
     FOR X1 = 1 TO IJL%: PRINT #2, IJ$(X1); ","; : NEXT: PRINT #2,
peject:
     PRINT #2, "EJECTIONS-";
     IF EJ% = 0 THEN PRINT #2, "none": GOTO pweath
     FOR X1 = 1 TO EJ%: PRINT #2, EJ$(X1); ","; : NEXT: PRINT #2,
pweath:
     PRINT #2, "TEMPERATURE:"; TP%
     PRINT #2, "WIND: ";WI$
     PRINT #2, "SKY: "; SC$(SC%)
     PRINT #2, "PRECIPITATION: "; PC$(PC%)
     PRINT #2, CHR$(12): CLOSE #2

     IF SPECIAL = 1 THEN FCopy D$(1) + NM$, D$(1) + "BOX" + MID$(STR$(COUNT), 2, LEN(STR$(COUNT))) + ".SPC" , SPACE$(4096), ErrCode 'copy the file
     IF X = 1 THEN 7580
     GOTO 7328
7179 PRINT #2, "(W,"; RIGHT$(STR$(P5%(I, I2, 6) + 1), (LEN(STR$(P5%(I, I2, 6) + 1)) - 1)); "-"; RIGHT$(STR$(P5%(I, I2, 7)), (LEN(STR$(P5%(I, I2, 7))) - 1)); ")"; : W$ = P$(I, I2): GOTO 7148
7180 PRINT #2, "(L,"; RIGHT$(STR$(P5%(I, I2, 6) + 0), (LEN(STR$(P5%(I, I2, 6) + 0)) - 1)); "-"; RIGHT$(STR$(P5%(I, I2, 7) + 1), (LEN(STR$(P5%(I, I2, 7) + 1)) - 1)); ")"; : L$ = P$(I, I2): GOTO 7149
     LOCATE , 33: PRINT "-"; : LOCATE , 34: PRINT USING "##"; S6%(I, 0);
     LOCATE , 37: PRINT USING "##"; S6%(I, 1); : LOCATE , 40: PRINT USING "#"; S6%(I, 2): RETURN
7183 IF INNING% < 9 THEN GOSUB 7185
     IF INNING% = 10 AND P = 0 OR INNING% = 9 THEN GOSUB 7187
     IF INNING% = 10 AND P = 1 OR INNING% > 10 AND INNING% < 19 OR INNING% = 19 AND P = 0 THEN GOSUB 7194
     IF INNING% = 19 AND P = 1 OR INNING% > 19 AND INNING% <= 28 THEN GOSUB 7202
     RETURN
7185 FOR I = 0 TO 1
     PRINT #2, A$(I);
     FOR I2 = 1 TO INNING%
     GOSUB 7212
     IF I = 0 THEN PRINT #2, USING "##"; S%(I, I2);
     IF I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = INNING% AND S%(1, INNING%) = 0 THEN PRINT #2, " X";
     IF I = 1 AND S6%(1, 0) < S6%(0, 0) AND I2 = INNING% OR I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = INNING% AND S%(1, INNING%) > 0 THEN PRINT #2, USING "##"; S%(I, I2);
     IF I = 1 AND I2 < INNING% THEN PRINT #2, USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7222
     NEXT I
     RETURN
7187 FOR I = 0 TO 1
     PRINT #2, A$(I);
     FOR I2 = 1 TO 9
     GOSUB 7212
     IF I = 0 THEN PRINT #2, USING "##"; S%(I, I2);
     IF I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = 9 AND S%(1, 9) = 0 THEN PRINT #2, " X";
     IF I = 1 AND S6%(1, 0) < S6%(0, 0) AND I2 = 9 OR I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = 9 AND S%(1, 9) > 0 THEN PRINT #2, USING "##"; S%(I, I2);
     IF I = 1 AND I2 < 9 THEN PRINT #2, USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7222
     NEXT I
     RETURN

7188 IF INNING% > E%(1) THEN PRINT #2, USING "MORE THAN ## INNINGS";E%(1)
     IF S6%(1,0) < E%(2) THEN PRINT #2, A$(1);:PRINT #2,USING " SCORED LESS THAN ## RUN(S)";E%(2)
     IF S6%(0,0) < E%(2) THEN PRINT #2, A$(0);:PRINT #2,USING " SCORED LESS THAN ## RUN(S)";E%(2)
     IF S6%(1,0) > E%(3) THEN PRINT #2, A$(1);:PRINT #2,USING " SCORED MORE THAN ## RUN(S)";E%(3)
     IF S6%(0,0) > E%(3) THEN PRINT #2, A$(0);:PRINT #2,USING " SCORED MORE THAN ## RUN(S)";E%(3)
     IF S6%(1,1) > E%(5) THEN PRINT #2, A$(1);:PRINT #2,USING " HAD MORE THAN ## HIT(S)";E%(5)
     IF S6%(0,1) > E%(5) THEN PRINT #2, A$(0);:PRINT #2,USING " HAD MORE THAN ## HIT(S)";E%(5)
     IF S6%(1,1) < E%(4) THEN PRINT #2, A$(1);:PRINT #2,USING " HAD LESS THAN ## HIT(S)";E%(4)
     IF S6%(0,1) < E%(4) THEN PRINT #2, A$(0);:PRINT #2,USING " HAD LESS THAN ## HIT(S)S";E%(4)

    FOR I=0 TO 1
    FOR I1 = 0 TO 22
    IF B8%(I, I1, 1) > E%(6) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " HAD MORE THAN ## HIT(S)";E%(6)
    IF B8%(I, I1, 2) > E%(7) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " SCORED MORE THAN ## RUN(S)";E%(7)
    IF B8%(I, I1, 3) > E%(8) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " HAD MORE THAN ## RBI(S)";E%(8)
    IF B8%(I, I1, 4) > E%(9) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " HIT MORE THAN ## DOUBLES(S)";E%(9)
    IF B8%(I, I1, 5) > E%(10) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " HIT MORE THAN ## TRIPLE(S)";E%(10)
    IF B8%(I, I1, 6) > E%(11) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " HOMERED MORE THAN ## TIME(S)";E%(11)
    IF B8%(I, I1, 9) > E%(12) THEN PRINT #2, B$(I,I1);:PRINT #2,USING " STOLE MORE THAN ## BASE(S)";E%(12)
    NEXT
    NEXT

   FOR I=0 TO 1
   FOR I1 = 0 TO 21
   IF P8%(I, I1, 5) > E%(13) THEN PRINT #2, P$(I,I1);:PRINT #2,USING " STRUCK OUT MORE THAN ## BATTER(S)";E%(13)
   NEXT
   NEXT
   RETURN
7194 FOR I = 0 TO 1
     PRINT #2, A$(I);
     FOR I2 = 1 TO 9
     GOSUB 7212
     PRINT #2, USING "##"; S%(I, I2);
     NEXT I2
     PRINT #2,
     NEXT I
     PRINT #2,
     IF P = 0 AND INNING% < 19 OR INNING% = 19 THEN INNING% = INNING% - 1
     FOR I = 0 TO 1
     FOR I2 = 10 TO INNING%
     GOSUB 7212: PRINT #2, USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7222
     NEXT I
     RETURN
7202 FOR I = 0 TO 1
     PRINT #2, A$(I);
     FOR I2 = 1 TO 9
     GOSUB 7212
     PRINT #2, USING "##"; S%(I, I2);
     NEXT I2: PRINT #2, : NEXT I
     PRINT #2,
     IF INNING% = 28 OR INNING% < 28 AND P = 0 THEN INNING% = INNING% - 1
     FOR I = 0 TO 1
     FOR I2 = 10 TO 18
     GOSUB 7212
     PRINT #2, USING "##"; S%(I, I2);
     NEXT I2: PRINT #2, : NEXT I
     FOR I = 0 TO 1
     FOR I2 = 19 TO INNING%
     GOSUB 7212
     PRINT #2, USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7222
     NEXT I
     PRINT #2,: RETURN
7212 IF I2 < 4 THEN PRINT #2, TAB(((2 * I2) + 18));
     IF I2 > 3 AND I2 < 7 THEN PRINT #2, TAB(((2 * I2) + 19));
     IF I2 > 6 AND I2 < 10 THEN PRINT #2, TAB(((2 * I2) + 20));
     IF I2 > 9 AND I2 < 13 THEN PRINT #2, TAB(((2 * (I2 - 9)) + 18));
     IF I2 > 12 AND I2 < 16 THEN PRINT #2, TAB(((2 * (I2 - 9)) + 19));
     IF I2 > 15 AND I2 < 19 THEN PRINT #2, TAB(((2 * (I2 - 9)) + 20));
     IF I2 > 18 AND I2 < 22 THEN PRINT #2, TAB(((2 * (I2 - 18)) + 18));
     IF I2 > 21 AND I2 < 25 THEN PRINT #2, TAB(((2 * (I2 - 18)) + 19));
     IF I2 > 24 AND I2 < 28 THEN PRINT #2, TAB(((2 * (I2 - 18)) + 20));
     RETURN
7222 PRINT #2, TAB(40); "-";
     PRINT #2, TAB(41);
     PRINT #2, USING "##"; S6%(I, 0);
     PRINT #2, TAB(44); : PRINT #2, USING "##"; S6%(I, 1);
     PRINT #2, TAB(47); : PRINT #2, USING "#"; S6%(I, 2)
     RETURN
7227 IF AP% = 1 AND U6 = 2 THEN RETURN
     LOCATE 24, 1: PRINT "HIT ANY KEY TO CONTINUE";
7229 I$ = INKEY$: IF I$ = "" THEN 7229
     RETURN
7293 I3 = 0: IF S6%(1, 0) - S6%(0, 0) > 0 THEN I3 = 1
     IF P2%(I3) <> P1%(I3) AND S1%(I3) = -1 AND P8%(I3, P1%(I3), 0) >= 9 AND P8%(I3, P1%(I3), 0) <= 12 AND P6%(I3) > 0 AND P8%(I3, P1%(I3), 3) < 3 THEN S1%(I3) = P1%(I3)
     IF P2%(I3) = S1%(I3) THEN S1%(I3) = -1
     CLS : PRINT TAB(32); : COLOR 12: PRINT "BOXSCORE OPTIONS": PRINT
     COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "SEND BOXSCORE TO THE SCREEN": PRINT
     COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "SEND BOXSCORE TO THE PRINTER": PRINT
     COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "SEND BOXSCORE TO A TEXT FILE"
     IF AP% = 1 AND U6 = 2 THEN X = 2: GOTO 7339
7337 I$ = INKEY$: IF I$ = "" THEN 7337
     X = VAL(I$)
7339 IF X = 1 OR X = 2 THEN 7119
     X = 0
     GOSUB 7491: GOSUB 7344: GOSUB 7342: GOSUB 7342
     FOR I = 0 TO 1

     COLOR L%(I,11),L%(I,12)
     PRINT A$(I);
     COLOR 14,0
     LOCATE , 18: PRINT "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
     COLOR 15,0
     FOR I3 = 0 TO 22
     IF X0%(I, 0, I3) = -1 OR X0%(I, 1, I3) = 1 THEN 7302
     I1 = X0%(I, 0, I3)
7298 PRINT B$(I, I1); ", "; C$(X0%(I, 1, I3));
     LOCATE , 18
     PRINT USING BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);
     PRINT "   ";
     IF B5%(I, I1, 0) = 0 OR B5%(I,I1, 1) = 0 THEN PRINT " .000   "; : GOTO 735
    IF B5%(I, I1, 0) = B5%(I, I1, 1) THEN PRINT "1.000   "; : GOTO 735
     PRINT USING " .###   "; B5%(I, I1, 1) / B5%(I, I1, 0);
735 PRINT USING "##  ###";B5%(I, I1, 6);B5%(I, I1, 3)
     FOR I2 = 0 TO 17
     T3%(I2) = T3%(I2) + B8%(I, I1, I2)
     NEXT I2
7302 NEXT I3
     FOR I1 = 0 TO 21: FOR I2 = 0 TO 13: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23): NEXT
     FOR I2 = 14 TO 17: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22): NEXT: NEXT
     FOR I1 = 1 TO P6%(I): FOR I2 = 0 TO 21
     IF P%(I, I2, 15) <> I1 THEN 7305
     GOTO 7310
7305 NEXT: NEXT:COLOR 14,0:PRINT "TOTALS"; : COLOR 15,0:
     LOCATE , 17: PRINT T3%(0); : LOCATE , 22: PRINT T3%(1); : LOCATE , 27: PRINT T3%(4);
     LOCATE , 32: PRINT T3%(6); : LOCATE , 36: PRINT T3%(7); : LOCATE , 42: PRINT T3%(10); : LOCATE , 46: PRINT T3%(16); : LOCATE , 50: PRINT T3%(17)
     LOCATE , 20: PRINT T3%(2); : LOCATE , 24: PRINT T3%(3); : LOCATE , 29: PRINT T3%(5); : LOCATE , 34: PRINT T3%(8); : LOCATE , 39: PRINT T3%(9); : LOCATE , 44: PRINT T3%(11)
     PRINT : GOSUB 7344: GOSUB 7342: FOR I1 = 0 TO 22: T3%(I1) = 0: NEXT I1
     NEXT I: GOTO 7313
7310 PRINT P$(I, I2); ", p"; : LOCATE , 18
     PRINT USING BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
     PRINT "   ";
     IF P5%(I, I2, 24) = 0 OR P5%(I,I2, 23) = 0 THEN PRINT " .000   "; : GOTO 736
    IF P5%(I, I2, 24) = P5%(I, I2, 23) THEN PRINT "1.000   "; : GOTO 736
     PRINT USING " .###   "; P5%(I, I2, 24) / P5%(I, I2, 23);
736 PRINT USING "##  ###";P5%(I, I2, 29);P5%(I, I2, 26)
     GOTO 7305
7313 FOR I = 0 TO 1
     GOSUB 7342
     COLOR L%(I,11),L%(I,12)
     PRINT A$(I);
     COLOR 14,0:LOCATE , 23
     PRINT "       IP   H  R  ER BB SO    ERA   W  L  S  SO":COLOR 15,0
     FOR I1 = 1 TO P6%(I)
     FOR I2 = 0 TO 21
     IF P%(I, I2, 15) <> I1 THEN 7321
     I4 = 1 - I: LOCATE , 1
     PRINT P$(I, I2); " ";
     IF P2%(I) = I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN 7346
7316 GOSUB 7348: IF P2%(I) = I2 AND S6%(I, 0) - S6%(I4, 0) < 0 THEN 7347
7317 IF P8%(I, I2, 19) > 0 THEN PRINT "(BS,"; : PRINT USING "##"; P5%(I, I2, 19); : PRINT ")";
     IF P8%(I, I2, 22) > 0 THEN PRINT "(H,"; : PRINT USING "##"; P5%(I, I2, 22); : PRINT ")";
     LOCATE , 29: PRINT USING "##"; INT(P8%(I, I2, 0) / 3);
     IF P8%(I, I2, 0) - (INT(P8%(I, I2, 0) / 3) * 3) > 0 THEN PRINT "."; : PRINT USING "#"; P8%(I, I2, 0) - (INT(P8%(I, I2, 0) / 3) * 3);
     LOCATE , 34
     PRINT USING "## ##  ## ## ##";P8%(I, I2, 1);P8%(I, I2, 2);P8%(I, I2, 3);P8%(I, I2, 4);P8%(I, I2, 5);
     PRINT "   ";
     IF P5%(I,I2,3)>0 AND P5%(I,I2,0)=0 THEN PRINT "99.99 ";:GOTO 737
     IF P5%(I,I2,0)=0 THEN PRINT "--.-- ";:GOTO 737
     IF P5%(I,I2,3)=0 THEN PRINT " 0.00 ";:GOTO 737
     PRINT USING "##.## ";P5%(I,I2,3)/(P5%(I,I2,0)/3)*9;
737  IF P2%(I)=I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN PRINT USING "## ";P5%(I,I2,6)+1; ELSE PRINT USING "## ";P5%(I,I2,6);
     IF P2%(I)=I2 AND S6%(I, 0) - S6%(I4, 0) < 0 THEN PRINT USING "## ";P5%(I,I2,7)+1; ELSE PRINT USING "## ";P5%(I,I2,7);
     IF S1%(I)=I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN PRINT USING "## ";P5%(I,I2,12)+1; ELSE PRINT USING "## ";P5%(I,I2,12);
     PRINT USING "###";P5%(I,I2,5)
     FOR I4 = 0 TO 5
     T3%(I4) = T3%(I4) + P8%(I, I2, I4)
     NEXT I4
7321 NEXT I2
     NEXT I1
     COLOR 14,0:PRINT "TOTALS"; :COLOR 15,0: LOCATE , 29: IF (T3%(0) / 3) < 10 THEN PRINT INT(T3%(0) / 3);  ELSE LOCATE (P6%(I) + 2), 28: PRINT INT(T3%(0) / 3);
     IF T3%(0) - (INT(T3%(0) / 3) * 3) > 0 THEN PRINT "."; : PRINT USING "#"; T3%(0) - (INT(T3%(0) / 3) * 3);
     LOCATE , 34
     PRINT USING "## ##  ## ## ##";T3%(1);T3%(2);T3%(3);T3%(4);T3%(5)
     PRINT
     GOSUB 7344: GOSUB 7342
     LYN% = CINT(T3%(0) / 3): FOR I1 = 0 TO 5: T3%(I1) = 0: NEXT I1
     NEXT I
     GOSUB 7340: CLS : GOSUB 7375: GOSUB 7227
7328 COLOR 15, 0: CLS : J = 1: CALL DRAWBOX(J): LOCATE 9, 28: PRINT "DISPLAY STATS AGAIN (YN)"; : COLOR 15, 0: IF AP% = 1 AND U6 = 2 THEN 7331
7329 I$ = INKEY$: IF I$ = "" THEN 7329
     IF UCASE$(I$) = "Y" THEN 7293
7331 GOSUB 7408
     COLOR 15, 0

     CLS : J = 2: CALL DRAWBOX(J)
     IF DRIVE >=67 THEN 7334
     LOCATE 9, 23: PRINT "MAKE SURE GAME DISK 1 IS IN DRIVE"
     LOCATE , 27: PRINT "PRESS ANY KEY TO CONTINUE": COLOR 15, 0
7336 I$ = INKEY$: IF I$ = "" THEN 7336
7334 RUN "HELLO.EXE"
7340 IF X = 1 THEN X = 0
     RETURN
7342 IF X = 0 THEN CLS
     RETURN
7344 IF X = 0 THEN GOSUB 7227
     RETURN
7346 PRINT "(W,"; RIGHT$(STR$(P5%(I, I2, 6) + 1), (LEN(STR$(P5%(I, I2, 6) + 1)) - 1)); "-"; RIGHT$(STR$(P5%(I, I2, 7)), (LEN(STR$(P5%(I, I2, 7))) - 1)); ")"; : W$ = P$(I, I2): GOTO 7316
7347 PRINT "(L,"; RIGHT$(STR$(P5%(I, I2, 6) + 0), (LEN(STR$(P5%(I, I2, 6) + 0)) - 1)); "-"; RIGHT$(STR$(P5%(I, I2, 7) + 1), (LEN(STR$(P5%(I, I2, 7) + 1)) - 1)); ")"; : L$ = P$(I, I2): GOTO 7317
7348 IF S1%(I) = I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN PRINT "(S,"; RIGHT$(STR$(P5%(I, I2, 12) + 1), (LEN(STR$(P5%(I, I2, 12) + 1)) - 1)); ")";
     RETURN
DSKBOX:
     IF S1%(I) = I2 AND S6%(I, 0) - S6%(I4, 0) > 0 THEN PRINT #2, "(S,"; RIGHT$(STR$(P5%(I, I2, 12) + 1), (LEN(STR$(P5%(I, I2, 12) + 1)) - 1)); ")";
     RETURN



7375 COLOR 14,0:PRINT " E-";
     COLOR 15,0
     IF E = 0 THEN PRINT "none": GOTO 7378
     FOR X = 0 TO (E - 1): PRINT E$(X); ","; : NEXT: PRINT
7378 COLOR 14,0:PRINT "2B-";:COLOR 15,0
     IF DB = 0 THEN PRINT "none": GOTO 7381
     FOR X = 0 TO (DB - 1): PRINT DB$(X); ","; : NEXT: PRINT
7381 COLOR 14,0:PRINT "3B-";:COLOR 15,0
     IF TR = 0 THEN PRINT "none": GOTO 7384
     FOR X = 0 TO (TR - 1): PRINT TR$(X); ","; : NEXT: PRINT
7384 COLOR 14,0:PRINT "HR-";:COLOR 15,0
     IF HR = 0 THEN PRINT "none": GOTO 7387
     FOR X = 0 TO (HR - 1): PRINT HR$(X); ","; : NEXT: PRINT
7387 COLOR 14,0:PRINT "SB-";:COLOR 15,0
     IF SB = 0 THEN PRINT "none": GOTO 7390
     FOR X = 0 TO (SB - 1): PRINT SB$(X); ","; : NEXT: PRINT
7390 COLOR 14,0:PRINT "LOB-";:COLOR 15,0:PRINT A$(0); STR$(L0%(0)); ","; A$(1); STR$(L0%(1))
     PRINT "DP-";:COLOR 15,0:PRINT A$(0); STR$(DP%(0)); ","; A$(1); STR$(DP%(1))
     COLOR 14,0:PRINT "INJURIES-";:COLOR 15,0
     IF IJL% = 0 THEN PRINT "none": GOTO eject
     FOR X = 1 TO IJL%: PRINT IJ$(X); ","; : NEXT: PRINT
eject:
     COLOR 14,0:PRINT "EJECTIONS-";:COLOR 15,0
     IF EJ% = 0 THEN PRINT "none": GOTO 7392
     FOR X = 1 TO EJ%: PRINT EJ$(X); ","; : NEXT: PRINT
7392 COLOR 14,0:PRINT "UMPIRES: ";:COLOR 15,0:PRINT "HP-"; U$(1); "; 1B-"; U$(2); "; 2B-"; U$(3); "; 3B-"; U$(4): PRINT "T: "; GT$; " A: "; ATT
     PRINT
     COLOR 14,0:PRINT "TEMPERATURE:";:COLOR 15,0:PRINT TP%
     COLOR 14,0:PRINT "WIND: ";:COLOR 15,0:PRINT WI$
     COLOR 14,0:PRINT "SKY: ";:COLOR 15,0:PRINT SC$(SC%)
     COLOR 14,0:PRINT "PRECIPITATION: ";:COLOR 15,0:PRINT PC$(PC%)
     RETURN
7408 CLS
     COLOR 15, 0: CLS : J = 1: CALL DRAWBOX(J): LOCATE 9, 28: PRINT "SAVE STATS FOR EITHER TEAM (YN)"; : COLOR 15, 0: IF AP% = 1 THEN BG% = 0: BN% = 1: GOTO 7411
7410 I$ = INKEY$: IF I$ = "" THEN 7410
     IF UCASE$(I$) <> "Y" THEN X1 = 3: RETURN
     CLS : COLOR 12: PRINT TAB(33); "COMPILE STATS": PRINT
     COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "BOTH TEAMS": PRINT
     COLOR 14: PRINT "(1) "; : COLOR L%(0, 11), L%(0, 12): PRINT A$(0); " ONLY": COLOR , 0: PRINT
     COLOR 14: PRINT "(2) "; : COLOR L%(1, 11), L%(1, 12): PRINT A$(1); " ONLY": COLOR 15, 0: PRINT
7409 I$ = INKEY$: IF I$ = "" THEN 7409
     X1 = VAL(I$)
     IF X1 < 0 OR X1 > 2 THEN 7409
7411 SX%(0) = -1: SX%(1) = -1
     FOR PX = 0 TO 1
     FOR I = 0 TO 21
     IF P%(PX, I, 15) = 1 THEN SX%(PX) = I: GOTO 7395
     NEXT I
7395 NEXT PX: GOTO 7600
7491 CLS
     IF INNING% < 9 THEN GOSUB 7493
     IF INNING% = 10 AND P = 0 OR INNING% <= 9 THEN GOSUB 7495
     IF INNING% = 10 AND P = 1 OR INNING% > 10 AND INNING% < 19 OR INNING% = 19 AND P = 0 THEN GOSUB 7504
     IF INNING% = 19 AND P = 1 OR INNING% > 19 AND INNING% <= 28 THEN GOSUB 7512
     RETURN
7493 FOR I = 0 TO 1
     COLOR L%(I,11),L%(I,12)
     PRINT A$(I);
     COLOR 14,0
     FOR I2 = 1 TO INNING%
     GOSUB 7522
     IF I = 0 THEN PRINT USING "##"; S%(I, I2);
     IF I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = INNING% AND S%(1, INNING%) = 0 THEN PRINT , " X";
     IF I = 1 AND S6%(1, 0) < S6%(0, 0) AND I2 = INNING% OR I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = INNING% AND S%(1, INNING%) > 0 THEN PRINT USING "##"; S%(I, I2);
     IF I = 1 AND I2 < INNING% THEN PRINT USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7532
     NEXT I
     RETURN
7495 FOR I = 0 TO 1
     COLOR L%(I,11),L%(I,12)
     PRINT A$(I);
     COLOR 14,0
     FOR I2 = 1 TO 9
     GOSUB 7522
     IF I = 0 THEN PRINT USING "##"; S%(I, I2);
     IF I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = 9 AND S%(1, 9) = 0 THEN PRINT " X";
     IF I = 1 AND S6%(1, 0) < S6%(0, 0) AND I2 = 9 OR I = 1 AND S6%(1, 0) > S6%(0, 0) AND I2 = 9 AND S%(1, 9) > 0 THEN PRINT USING "##"; S%(I, I2);
     IF I = 1 AND I2 < 9 THEN PRINT USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7532
     NEXT I
     RETURN
7504 FOR I = 0 TO 1
     COLOR L%(I,11),L%(I,12)
     PRINT A$(I);
     COLOR 14,0
     FOR I2 = 1 TO 9
     GOSUB 7522
     PRINT USING "##"; S%(I, I2);
     NEXT I2
     PRINT
     NEXT I
     PRINT
     IF P = 0 AND INNING% < 19 OR INNING% = 19 THEN INNING% = INNING% - 1
     FOR I = 0 TO 1
     FOR I2 = 10 TO INNING%
     GOSUB 7522
     PRINT USING "##"; S%(I, I2);
     NEXT I2
     GOSUB 7532
     NEXT I
     RETURN
7512 FOR I = 0 TO 1
     COLOR L%(I,11),L%(I,12)
     PRINT A$(I);
     COLOR 14,0
     FOR I2 = 1 TO 9
     GOSUB 7522
     PRINT USING "##"; S%(I, I2);
     NEXT I2
     PRINT
     NEXT I
     PRINT
     IF INNING% = 28 OR INNING% < 28 AND P = 0 THEN INNING% = INNING% - 1
     FOR I = 0 TO 1
     FOR I2 = 10 TO 18
     GOSUB 7522
     PRINT USING "##"; S%(I, I2);
     NEXT I2
     PRINT
     NEXT I
     PRINT
     FOR I = 0 TO 1
     FOR I2 = 19 TO INNING%: GOSUB 7522: PRINT USING "##"; S%(I, I2); : NEXT I2
     GOSUB 7532
     NEXT I
     RETURN

7522 IF I2 < 4 THEN LOCATE , ((2 * I2) + 18)
     IF I2 > 3 AND I2 < 7 THEN LOCATE , ((2 * I2) + 19)
     IF I2 > 6 AND I2 < 10 THEN LOCATE , ((2 * I2) + 20)
     IF I2 > 9 AND I2 < 13 THEN LOCATE , ((2 * (I2 - 9)) + 18)
     IF I2 > 12 AND I2 < 16 THEN LOCATE , ((2 * (I2 - 9)) + 19)
     IF I2 > 15 AND I2 < 19 THEN LOCATE , ((2 * (I2 - 9)) + 20)
     IF I2 > 18 AND I2 < 22 THEN LOCATE , ((2 * (I2 - 18)) + 18)
     IF I2 > 21 AND I2 < 25 THEN LOCATE , ((2 * (I2 - 18)) + 19)
     IF I2 > 24 AND I2 < 28 THEN LOCATE , ((2 * (I2 - 18)) + 20)
     RETURN


7532 LOCATE , 40: PRINT "-"; : LOCATE , 41: PRINT USING "##"; S6%(I, 0);
     LOCATE , 44: PRINT USING "##"; S6%(I, 1); : LOCATE , 47: PRINT USING "#"; S6%(I, 2)
     RETURN

7580 OPEN D$(1)+ NM$ FOR INPUT AS #2
7581 LINE INPUT #2, X$
     IF LEFT$(X$, 1) = CHR$(12) THEN LPRINT X$: CLOSE #2: GOTO 7328
     LPRINT X$: GOTO 7581
7600 FILENAME$ = "GAME82"
     IF NOT EXIST%(FILENAME$) THEN 7630
     OPEN "GAME82" FOR INPUT AS #2
     INPUT #2, FF$
     FOR I = 0 TO 2: INPUT #2, AP%(I): NEXT: CLOSE 2
     OPEN D$(0)+FF$  FOR RANDOM AS #1 LEN = 48
     FOR X = 0 TO 18
     FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 48 - 2 - 2 * X AS X$
     NEXT
     FIELD #1, 38 AS X$, 2 AS Q$(22), 2 AS Q$(23), 3 AS Q$(20), 3 AS Q$(21)
     NG%(13) = 1
     FOR X = 0 TO 18: LSET Q$(X + 1) = MKI$(NG%(X)): NEXT
     FOR X = 0 TO 1: LSET Q$(20 + X) = YN$(X): NEXT
     LSET Q$(22) = MKI$(S6%(0, 0)): LSET Q$(23) = MKI$(S6%(1, 0))
     PUT #1, AP%(0) + AP%(2)
     CLOSE 1
     AP%(2) = AP%(2) + 1
     AP% = 1: IF AP%(1) < AP%(0) + AP%(2) THEN KILL "GAME82": NM = 1: GOTO 7630
     OPEN "GAME82" FOR OUTPUT AS #2
     PRINT #2, FF$
     FOR I = 0 TO 2: PRINT #2, AP%(I): NEXT: CLOSE 2
7630 CLS
     Q = 0
     W2% = 0: L2% = 0
7662 C% = 0: GOSUB 7834: IF C% = 1 THEN 7770
     SA$(Q) = A$(Q): GOSUB 7862
     F$ = MID$(SA$(Q), 4, QQ): R$ = F$ + "." + LEFT$(SA$(Q), 2)
     FILENAME$ = D$(1) + R$ + "D"
     IF NOT EXIST% (FILENAME$) THEN 7663
     OPEN D$(1) + R$ + "D" FOR INPUT AS #1
     FOR I1=0 TO 22
     INPUT #1, INJ%(Q,I1)
     NEXT
     FOR I1 = 0 TO 21
     INPUT #1, DYS%(Q,I1)
     NEXT
     CLOSE 1
7663 IF IJ% = 1 AND DR% = 1 THEN 7666
     IF IJ% = 1 THEN 7664
     FOR I1 = 0 TO 22
     IF INJ%(Q, I1)>0 THEN INJ%(Q,I1) = INJ%(Q,I1)-1
     IF B%(Q,I1,32)>0 THEN INJ%(Q,I1) = B%(Q,I1,32)
     NEXT
7664 IF DR% = 1 THEN 7665
     FOR I1=0 TO 21
     IF DYS%(Q,I1)>0 AND DYS%(Q,I1)<90 THEN DYS%(Q,I1) = DYS%(Q,I1)-1
     IF P8%(Q,I1,0) = 0 AND DYS%(Q,I1)>0 AND DYS%(Q,I1)<90 THEN 7667
     IF P8%(Q,I1,0) = 0 AND (DYS%(Q,I1) = 0 OR DYS%(Q,I1) = 1 OR DYS%(Q,I1) = 91 OR DYS%(Q,I1) = 92) THEN DYS%(Q,I1) = 0:GOTO 7667

     IF P8%(Q,I1,0)>0 AND P8%(Q,I1,0)<=3 AND DYS%(Q,I1) = 0 THEN DYS%(Q,I1) = 91:GOTO 7667
     IF P8%(Q,I1,0)>0 AND P8%(Q,I1,0)<=3 AND DYS%(Q,I1) = 91 THEN DYS%(Q,I1) = 911:GOTO 7667
     IF P8%(Q,I1,0)>0 AND P8%(Q,I1,0)<=3 AND DYS%(Q,I1) = 911 THEN DYS%(Q,I1) = 1:GOTO 7667
     IF P8%(Q,I1,0)>0 AND P8%(Q,I1,0)<=3 AND DYS%(Q,I1) = 92 THEN DYS%(Q,I1) = 1:GOTO 7667
     IF P8%(Q,I1,0)>0 AND P8%(Q,I1,0)<=3 AND DYS%(Q,I1)>0 THEN DYS%(Q,I1) = DYS%(Q,I1) + 1:GOTO 7667


     IF P8%(Q,I1,0)>3 AND P8%(Q,I1,0)<=6 AND DYS%(Q,I1) = 0 THEN DYS%(Q,I1) = 92:GOTO 7667
     IF P8%(Q,I1,0)>3 AND P8%(Q,I1,0)<=6 AND DYS%(Q,I1) = 91 THEN DYS%(Q,I1) = 1:GOTO 7667
     IF P8%(Q,I1,0)>3 AND P8%(Q,I1,0)<=6 AND DYS%(Q,I1) = 92 THEN DYS%(Q,I1) = 1:GOTO 7667
     IF P8%(Q,I1,0)>3 AND P8%(Q,I1,0)<=6 AND DYS%(Q,I1) = 911 THEN DYS%(Q,I1) = 1:GOTO 7667
     IF P8%(Q,I1,0)>3 AND P8%(Q,I1,0)<=6 AND DYS%(Q,I1)>0 THEN DYS%(Q,I1) = DYS%(Q,I1) + 1:GOTO 7667

     IF P8%(Q,I1,0)>6 THEN DYS%(Q,I1) = INT(P8%(Q,I1,0)/3)+1-2

     IF MG%(Q,1)<=0 THEN DYS%(Q,I1) = 3:GOTO 7667
     IF DYS%(Q,I1)>MG%(Q,1)-1 THEN DYS%(Q,I1) = MG%(Q,1)-1

     IF DYS%(Q,I1)<0 THEN DYS%(Q,I1)=0

7667 NEXT
7665 OPEN D$(1) + R$ + "D" FOR OUTPUT AS #1
     FOR I1=0 TO 22
     PRINT #1, INJ%(Q,I1)
     NEXT
     FOR I1 = 0 TO 21
     PRINT #1, DYS%(Q,I1)
     NEXT
     CLOSE 1
7666 FILENAME$ = D$(1) + R$
     IF NOT EXIST%(FILENAME$) THEN 7690
     OPEN D$(1) + R$ FOR INPUT AS #1
     INPUT #1, A$(Q): INPUT #1, W2%: INPUT #1, L2%
     FOR I = 0 TO 22: INPUT #1, B$(Q, I): INPUT #1, B2%(Q, I): NEXT I
     FOR I = 0 TO 21: INPUT #1, P$(Q, I): INPUT #1, P3%(Q, I): NEXT I
     FOR I = 0 TO 22: FOR I1 = 0 TO 21: INPUT #1, B6%(I, I1): NEXT I1: NEXT I
     FOR I = 0 TO 21: FOR I1 = 0 TO 41: INPUT #1, P0%(I, I1): NEXT I1: NEXT I
     INPUT #1, D0%(Q): INPUT #1, D1%(Q): FOR I = 0 TO 21: INPUT #1, T%(I): NEXT I: FOR I = 0 TO 22: INPUT #1, P4%(I): NEXT I
     CLOSE #1
7690 FOR I = 0 TO 22
     B2%(Q, I) = B%(Q, I, 4)
     FOR I1 = 0 TO 11
     B6%(I, I1) = B6%(I, I1) + B8%(Q, I, I1): T%(I1) = T%(I1) + B8%(Q, I, I1)
     NEXT I1
     IF B8%(Q, I, 0) > 0 AND B8%(Q, I, 1) = 0 THEN B6%(I, 12) = 0: GOTO 7698
     IF B8%(Q, I, 0) > 0 AND B8%(Q, I, 1) > 0 THEN B6%(I, 12) = B6%(I, 12) + 1
7698 IF B8%(Q, I, 14) > 0 THEN B6%(I, 14) = B6%(I, 14) + 1
     FOR I1 = 15 TO 21
     B6%(I, I1) = B6%(I, I1) + B8%(Q, I, I1): T%(I1) = T%(I1) + B8%(Q, I, I1)
     NEXT I1
     IF B6%(I, 12) >= B6%(I, 13) THEN B6%(I, 13) = B6%(I, 12)
     NEXT I
     FOR I = 0 TO 21
     FOR I1 = 23 TO 34
     P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): T%(I1 - 23) = T%(I1 - 23) + P8%(Q, I, I1)
     NEXT I1
     IF P8%(Q, I, 23) > 0 AND P8%(Q, I, 24) = 0 THEN P0%(I, 35) = 0: GOTO 7705
     IF P8%(Q, I, 23) > 0 AND P8%(Q, I, 24) > 0 THEN P0%(I, 35) = P0%(I, 35) + 1
7705 FOR I1 = 37 TO 41
     P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): T%(I1 - 22) = T%(I1 - 22) + P8%(Q, I, I1)
     NEXT I1
     IF P0%(I, 35) >= P0%(I, 36) THEN P0%(I, 36) = P0%(I, 35)
     NEXT I
     T%(12) = T%(12) + L0%(Q)
     INNING% = 0
     FOR I = 0 TO 21
     INNING% = INNING% + P8%(Q, I, 0)
     P3%(Q, I) = P%(Q, I, 6)
     FOR I1 = 0 TO 5
     P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): P4%(I1) = P4%(I1) + P8%(Q, I, I1)
     NEXT I1
     FOR I1 = 13 TO 22: P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): P4%(I1) = P4%(I1) + P8%(Q, I, I1)
     NEXT I1
     NEXT I
     IF S6%(Q, 0) - S6%(1 - Q, 0) > 0 THEN P0%(P2%(Q), 6) = P0%(P2%(Q), 6) + 1
     IF S6%(Q, 0) - S6%(1 - Q, 0) < 0 THEN P0%(P2%(Q), 7) = P0%(P2%(Q), 7) + 1
     IF S6%(Q, 0) - S6%(1 - Q, 0) > 0 AND S1%(Q) >= 0 THEN P0%(S1%(Q), 12) = P0%(S1%(Q), 12) + 1: P4%(12) = P4%(12) + 1
     FOR I = 0 TO 21
     IF P%(Q, I, 15) <> 0 THEN P0%(I, 8) = P0%(I, 8) + 1: P4%(8) = P4%(8) + 1
     IF INNING% = P8%(Q, I, 0) THEN P0%(I, 10) = P0%(I, 10) + 1: P4%(10) = P4%(10) + 1
     IF INNING% = P8%(Q, I, 0) AND S6%(1 - Q, 0) = 0 THEN P0%(I, 11) = P0%(I, 11) + 1: P4%(11) = P4%(11) + 1
     NEXT I
     P0%(SX%(Q), 9) = P0%(SX%(Q), 9) + 1: P4%(9) = P4%(9) + 1
     D0%(Q) = D0%(Q) + D3%(Q): D1%(Q) = D1%(Q) + D3%(1 - Q)
     IF S6%(Q, 0) - S6%(1 - Q, 0) > 0 THEN W2% = W2% + 1
     IF S6%(Q, 0) - S6%(1 - Q, 0) < 0 THEN L2% = L2% + 1
     COLOR 15,0
     PRINT ">>>>>": PRINT : PRINT : PRINT "THE STATS ARE BEING SAVED UNDER": PRINT "THE FOLLOWING NAME ::: ";
     COLOR L%(Q,11),L%(Q,12):PRINT R$:COLOR 15,0
     OPEN D$(1) + R$ FOR OUTPUT AS #1
     PRINT #1, A$(Q): PRINT #1, W2%: PRINT #1, L2%
1    FOR I = 0 TO 22: PRINT #1, B$(Q, I): PRINT #1, B2%(Q, I): NEXT I
2    FOR I = 0 TO 21: PRINT #1, P$(Q, I): PRINT #1, P3%(Q, I): NEXT I
3    FOR I = 0 TO 22: FOR I1 = 0 TO 21: PRINT #1, B6%(I, I1): NEXT I1: NEXT I
4    FOR I = 0 TO 21: FOR I1 = 0 TO 41: PRINT #1, P0%(I, I1): NEXT I1: NEXT I
5    PRINT #1, D0%(Q): PRINT #1, D1%(Q)
6    FOR I = 0 TO 21: PRINT #1, T%(I): NEXT I
7    FOR I = 0 TO 22: PRINT #1, P4%(I): NEXT I
8    CLOSE #1: GOTO 7766
9    PRINT "<<<<<"
7766 GOSUB 7780
     GOSUB 7850: GOSUB 7856
7770 Q = Q + 1: IF Q < 2 THEN 7662
     IF AP% = 1 AND NM <> 1 THEN RUN "LOADER.EXE"
     IF DRIVE >=67 THEN 7777
     CLS : PRINT "INSERT GAME DISK & HIT ANY KEY "
7776 I$ = INKEY$: IF I$ = "" THEN 7776
7777 RUN "HELLO.EXE"
7780 FILENAME$ = D$(1) + R$ + "R"
     IF NOT EXIST% (FILENAME$) THEN 7782
     OPEN D$(1) + R$ + "R" FOR INPUT AS #1
     FOR I = 0 TO 22
     FOR I1 = 0 TO 6
     INPUT #1, SS%(I, I1)
     NEXT
     NEXT
     CLOSE 1
7782 FOR I = 0 TO 22
     FOR I1 = 0 TO 6
     SS%(I, I1) = SS%(I, I1) + CSS%(Q, I, I1)
     NEXT
     NEXT
     OPEN D$(1) + R$ + "R" FOR OUTPUT AS #1
     FOR I = 0 TO 22
     FOR I1 = 0 TO 6
     PRINT #1, SS%(I, I1)
     NEXT
     NEXT
     CLOSE 1
     Y% = 0
     FILENAME$ = D$(1) + R$ + "S"
     IF NOT EXIST%(FILENAME$) THEN 7800
     OPEN D$(1) + R$ + "S" FOR INPUT AS #1
     INPUT #1, Y%
10   FOR I = 1 TO 162: INPUT #1, H%(I): NEXT I
11   FOR I = 1 TO 162: INPUT #1, HT$(I): NEXT I
12   FOR I = 1 TO 162: INPUT #1, V%(I): NEXT I
13   FOR I = 1 TO 162: INPUT #1, PL$(I): NEXT I
14   FOR I = 1 TO 162: INPUT #1, W$(I): NEXT I
15   FOR I = 1 TO 162: INPUT #1, L$(I): NEXT I
16   FOR I = 1 TO 162: INPUT #1, ING%(I): NEXT I
     CLOSE #1: GOTO 7806
7800 FOR I = 1 TO 162
     H%(I) = 999: HT$(I) = "XXXXXXX"
     V%(I) = 999: PL$(I) = "X"
     W$(I) = "LYNN": L$(I) = "SUMLER"
     ING%(I) = 999
     NEXT I
7806 Y% = Y% + 1
7807 H%(Y%) = S6%(Q, 0)
7808 HT$(Y%) = A$(1 - Q)
7809 V%(Y%) = S6%(1 - Q, 0)
7810 W$(Y%) = W$: L$(Y%) = L$
7811 ING%(Y%) = LYN%
7812 IF Q = 0 THEN PL$(Y%) = "A": GOTO 7814
7813 PL$(Y%) = "H"
7814 REM
     OPEN D$(1) + R$ + "S" FOR OUTPUT AS #1: PRINT #1, Y%
     FOR I = 1 TO 162: PRINT #1, H%(I): NEXT I
     FOR I = 1 TO 162: PRINT #1, HT$(I): NEXT I
     FOR I = 1 TO 162: PRINT #1, V%(I): NEXT I
     FOR I = 1 TO 162: PRINT #1, PL$(I): NEXT I
     FOR I = 1 TO 162: PRINT #1, W$(I): NEXT I
     FOR I = 1 TO 162: PRINT #1, L$(I): NEXT I
     FOR I = 1 TO 162: PRINT #1, ING%(I): NEXT I
     CLOSE #1
     Y% = 0
     RETURN
7834 IF AP% = 1 THEN C% = 0: GOTO 7848
     IF X1 = 0 OR X1 = 1 AND Q = 0 OR X1 = 2 AND Q = 1 THEN 7840
     IF X1 = 2 AND Q = 0 OR X1 = 1 AND Q = 1 THEN C% = 1: GOTO 7848
7840 CLS : PRINT "INSERT STATS DISK FOR "; A$(Q); " AND HIT ENTER"
     IF DRIVE >=67 THEN 7848
7842 I$ = INKEY$: IF I$ = "" THEN 7842
7848 RETURN
7850 FOR I = 0 TO 22: FOR I1 = 0 TO 21: B6%(I, I1) = 0: NEXT: NEXT
     FOR I = 0 TO 22: FOR I1 = 0 TO 6: SS%(I, I1) = 0:NEXT:NEXT
     FOR I = 0 TO 21: FOR I1 = 0 TO 41: P0%(I, I1) = 0: NEXT: NEXT
     W2% = 0: L2% = 0: FOR I = 0 TO 21: T%(I) = 0: NEXT I: FOR I = 0 TO 22: P4%(I) = 0: NEXT I: RETURN
7856 FOR I = 0 TO 1: FOR I1 = 0 TO 22: B2%(I, I1) = 0: NEXT: NEXT
     FOR I = 0 TO 1: FOR I1 = 0 TO 21: P3%(I, I1) = 0: NEXT: NEXT
     RETURN
7862 FOR XX = 4 TO 11
     IF MID$(SA$(Q), XX, 1) = " " OR MID$(SA$(Q), XX, 1) = "." THEN MID$(SA$(Q), XX, 1) = "_"
     NEXT XX
     QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(SA$(Q), XX, 1) = "_" THEN QQ = XX - 4: GOTO 7870
     GOTO 7872
7870 NEXT XX
7872 RETURN
SUB DRAWBOX (J)
     LOCATE 8, 8: COLOR l%(1, 11), l%(1, 12)
     PRINT " Ö"; STRING$(61, 196); "· ";
     FOR II = 1 TO J
     LOCATE 8 + II, 8
     PRINT " º"; STRING$(61, 32); "º ";
     NEXT
     LOCATE 9 + J, 8: PRINT " Ó"; STRING$(61, 196); "½ ";
END SUB
