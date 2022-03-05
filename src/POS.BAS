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
  '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
DIM D$(4), Q$(1844), FS%(22, 4, 4), TM(40), A$(40), TYN$(40), BB$(23), PB$(22)
DIM B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21), B0%(22, 40)
DIM P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21)
DIM I2 AS SINGLE, I3 AS SINGLE, I4 AS SINGLE, ITEM$(5)
DIM K9(0) AS LONG
OPEN "DEV.BAS" FOR INPUT AS #1
FOR I = 0 TO 3
INPUT #1, D$(I)
NEXT
CLOSE #1

6 CALL TITLE
  ITEM$(1) = "CREATE POSITION SEARCH FILE   "
  ITEM$(2) = "SEARCH FOR PLAYERS BY POSITION"
  ITEM$(3) = "RETURN TO MAIN MENU           "
     SELECTION% = 1
     Count% = 3
     Start% = 1
     SCANCODE% = 3
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 6
     Column% = 22
     CALL Box0(5, 21, 9, 52, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN END
     I = SELECTION%
22 IF I = 1 THEN GOSUB 6005: RUN "POSCR.EXE"
23 IF I = 2 THEN 100
24 IF I = 3 THEN GOSUB 6000: RUN "HELLO.EXE"
100 CLS
    Spec$ = D$(0) + "*.POS"
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
    PRINT "SELECT LEADERS TO VIEW": COLOR 15
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
     IF ExitCode% = 27 THEN 6
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 LGE$ = QPRTrim$(F$)

110 CLS : SS = 0: COLOR 11: LOCATE 5, 30: PRINT "S E L E C T"
111 LOCATE 7, 28: COLOR 12: PRINT "0. "; : COLOR 7: PRINT "Catchers"
112 LOCATE , 28: COLOR 12: PRINT "1. "; : COLOR 7: PRINT "First Basemen"
113 LOCATE , 28: COLOR 12: PRINT "2. "; : COLOR 7: PRINT "Second Basemen"
114 LOCATE , 28: COLOR 12: PRINT "3. "; : COLOR 7: PRINT "Shortstops"
115 LOCATE , 28: COLOR 12: PRINT "4. "; : COLOR 7: PRINT "Third Basemen"
116 LOCATE , 28: COLOR 12: PRINT "5. "; : COLOR 7: PRINT "Left Fielders"
117 LOCATE , 28: COLOR 12: PRINT "6. "; : COLOR 7: PRINT "Center Fielders"
118 LOCATE , 28: COLOR 12: PRINT "7. "; : COLOR 7: PRINT "Right Fielders"
119 LOCATE , 28: COLOR 12: PRINT "8. "; : COLOR 7: PRINT "Pitchers"
120 LOCATE , 28: COLOR 12: PRINT "9. "; : COLOR 7: PRINT "Return To Main Menu"
130 Z$ = INKEY$: IF Z$ = "" THEN 130
131 Z = VAL(Z$): IF Z < 0 OR Z > 9 THEN 130
140 IF Z = 0 THEN PS = 2: PS$ = "CATCHERS": GOTO 200
141 IF Z = 1 THEN PS = 3: PS$ = "FIRST BASEMEN": GOTO 200
142 IF Z = 2 THEN PS = 4: PS$ = "SECOND BASEMEN": GOTO 200
143 IF Z = 3 THEN PS = 6: PS$ = "SHORTSTOPS": GOTO 200
144 IF Z = 4 THEN PS = 5: PS$ = "THIRD BASEMEN": GOTO 200
145 IF Z = 5 THEN PS = 7: PS$ = "LEFT FIELDERS": GOTO 200
146 IF Z = 6 THEN PS = 8: PS$ = "CENTER FIELDERS": GOTO 200
147 IF Z = 7 THEN PS = 9: PS$ = "RIGHT FIELDERS": GOTO 200
148 IF Z = 8 THEN PS = 1: GOTO 650
149 IF Z = 9 THEN GOTO 6
200 LOCATE 18, 28: COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT OR "; : COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW"
201 I$ = INKEY$: IF I$ = "" THEN 201
202 IF UCASE$(I$) = "P" THEN 2300
203 IF UCASE$(I$) = "V" THEN 290
204 IF UCASE$(I$) <> "P" OR UCASE$(I$) <> "V" THEN 201
290 GOSUB 5000
300 OPEN D$(0) + LGE$ FOR INPUT AS #1
301 FOR J = 1 TO 40
    INPUT #1, TM(J), A$(J), TYN$(J)
    NEXT
302 CLOSE #1
305 GOSUB 5100
306 CLS
    LOCATE 1, 35: COLOR 15: PRINT PS$
307 LOCATE 3: COLOR 11: PRINT "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"
350 FOR J = 1 TO 40
351 IF TM(J) = 0 THEN 410
352 GOSUB 5500
370 FOR I = 0 TO 22
    IF B$(I) = "XXX" OR B$(I) = "" THEN 409
371 IF B%(I, 22) = PS THEN I2 = 33: GOTO 372
    IF B%(I, 23) = PS THEN I2 = 35: GOTO 372
    IF B%(I, 24) = PS THEN I2 = 37: GOTO 372
    IF B%(I, 25) = PS THEN I2 = 39: GOTO 372
    GOTO 409
372 SS = SS + 1
373 IF SS = 19 OR SS = 37 OR SS = 55 OR SS = 73 OR SS = 91 OR SS = 109 OR SS = 127 OR SS = 145 OR SS = 163 OR SS = 181 OR SS = 199 OR SS = 217 THEN GOSUB 500: GOSUB 602
374 IF B%(I, 22) = PS THEN 375 ELSE 376
375 COLOR 12: PRINT LEFT$(B$(I), 10); : GOTO 377
376 COLOR 2: PRINT LEFT$(B$(I), 10);
377 COLOR 3: PRINT TAB(12); LEFT$(T$, 2);NN$;
378 IF B%(I, 0) = -1 THEN BB$(I) = "L"
379 IF B%(I, 0) = 1 THEN BB$(I) = "R"
380 IF B%(I, 0) = 0 THEN BB$(I) = "S"
    IF B%(I, 0) = 2 THEN BB$(I) = "RP"
    IF B%(I, 0) = -2 THEN BB$(I) = "LP"
381 COLOR 7: PRINT TAB(19); BB$(I); USING "#"; B%(I, I2);
    REM:PRINT B%(I,
382 PRINT TAB(22); USING "###"; B%(I, 3);
383 PRINT TAB(26); USING "###"; B%(I, 4);
384 PRINT TAB(31); USING "###"; B%(I, 6);
385 PRINT TAB(35); USING "##"; B%(I, 7);
386 PRINT TAB(38); USING "##"; B%(I, 8);
387 PRINT TAB(41); USING "##"; B%(I, 9);
388 PRINT TAB(44); USING "###"; B%(I, 10);
389 PRINT TAB(48); USING "###"; B%(I, 5);
390 PRINT TAB(52); USING "###"; B%(I, 11);
391 PRINT TAB(56); USING "###"; B%(I, 12);
392 PRINT TAB(59); USING "###"; B%(I, 13);
393 PRINT TAB(63); USING "##"; B%(I, 14);
394 I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)
395 IF B%(I, 4) = 0 OR I2 = 0 THEN PRINT TAB(66); ".000";
396 IF I2 > 0 AND I2 < 100 THEN PRINT TAB(66); ".0"; : PRINT USING "##"; I2;
397 IF I2 >= 100 THEN PRINT TAB(66); "."; : PRINT USING "###"; I2;
398 IF B%(I, 4) = 0 OR I3 = 0 THEN PRINT TAB(71); ".000";
399 IF I3 > 0 AND I3 < 100 THEN PRINT TAB(71); ".0"; : PRINT USING "##"; I3;
400 IF I3 >= 100 THEN PRINT TAB(71); "."; : PRINT USING "###"; I3;
401 IF B%(I, 4) = 0 OR I4 = 0 THEN PRINT TAB(76); ".000";
402 IF I4 > 0 AND I4 < 100 THEN PRINT TAB(76); ".0"; : PRINT USING "##"; I4;
403 IF I4 >= 100 THEN PRINT TAB(76); "."; : PRINT USING "###"; I4
409 NEXT I
410 NEXT J
415 LOCATE 23: COLOR 14: PRINT "END OF FILE - - - HIT ANY KEY TO CONTINUE"
416 I$ = INKEY$: IF I$ = "" THEN 416
420 GOTO 110
500 LOCATE 23: COLOR 14: PRINT "HIT ANY KEY TO CONTINUE"
501 I$ = INKEY$: IF I$ = "" THEN 501
601 RETURN
602 CLS : LOCATE 1, 35: COLOR 15: PRINT PS$
603 LOCATE 3: COLOR 11: PRINT "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"
604 RETURN
650 SS = 0: CLS : COLOR 11: LOCATE 5, 30: PRINT "S E L E C T"
651 LOCATE 7, 16: COLOR 12: PRINT "1. "; : COLOR 7: PRINT "Pitchers With Less Than 10 Games Started"
652 LOCATE , 16: COLOR 12: PRINT "2. "; : COLOR 7: PRINT "Pitchers With More Than 10 Games Started"
653 LOCATE , 16: COLOR 12: PRINT "3. "; : COLOR 7: PRINT "Right-Handed Pitchers"
654 LOCATE , 16: COLOR 12: PRINT "4. "; : COLOR 7: PRINT "Left-Handed Pitchers"
660 I$ = INKEY$: IF I$ = "" THEN 660
661 I = VAL(I$): IF I < 1 OR I > 4 THEN 660
662 IF I = 1 THEN PPS = 10: PC = 1: PPS$ = "PITCHERS WITH LESS THAN 10 GAMES STARTED": GOTO 680
663 IF I = 2 THEN PPS = 10: PC = 2: PPS$ = "PITCHERS WITH MORE THAN 10 GAMES STARTED": GOTO 680
664 IF I = 3 THEN THR = 1: PPS$ = "RIGHT-HANDED PITCHERS": GOTO 1680
665 IF I = 4 THEN THR = 2: PPS$ = "LEFT-HANDED PITCHERS": GOTO 1680
680 LOCATE 18, 28: COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT OR "; : COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW"
681 I$ = INKEY$: IF I$ = "" THEN 681
682 IF UCASE$(I$) = "P" THEN 3700
683 IF UCASE$(I$) = "V" THEN 690
684 IF UCASE$(I$) <> "P" OR UCASE$(I$) <> "V" THEN 681
690 GOSUB 5000
700 OPEN D$(0) + LGE$ FOR INPUT AS #1
701 FOR J = 1 TO 40: INPUT #1, TM(J), A$(J), TYN$(J): NEXT
702 CLOSE #1
705 GOSUB 5100
706 CLS : COLOR 15: LOCATE 1, 20: PRINT PPS$
707 LOCATE 3: COLOR 11: PRINT "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
750 FOR J = 1 TO 40
751 IF TM(J) = 0 THEN 810
752 GOSUB 5500
767 FOR I = 0 TO 21
    IF P$(I) = "XXX" OR P$(I) = "" THEN 800
768 IF PC = 1 THEN 769 ELSE 770
769 IF P%(I, 5) < PPS THEN 772 ELSE 800
770 IF P%(I, 5) > PPS THEN 772 ELSE 800
772 SS = SS + 1
773 IF SS = 19 OR SS = 37 OR SS = 55 OR SS = 73 OR SS = 91 OR SS = 109 OR SS = 127 OR SS = 145 OR SS = 163 OR SS = 181 OR SS = 199 OR SS = 217 OR SS = 235 OR SS = 253 OR SS = 271 OR SS = 289 OR SS = 307 OR SS = 325 OR SS = 343 OR SS = 361 OR SS =  _
379 OR SS = 397 OR SS = 415 THEN GOSUB 850: GOSUB 902
775 COLOR 12: PRINT LEFT$(P$(I), 10);
776 COLOR 3: PRINT TAB(12); LEFT$(T$, 2);NN$
777 IF P%(I, 0) = 1 THEN PB$(I) = "R"
778 IF P%(I, 0) = -1 THEN PB$(I) = "L"
779 COLOR 7: PRINT TAB(22); PB$(I);
780 PRINT TAB(25); USING "###"; P%(I, 4);
781 PRINT TAB(30); USING "##"; P%(I, 1);
782 PRINT TAB(34); USING "##"; P%(I, 2);
783 PRINT TAB(39); USING "##"; P%(I, 3);
784 PRINT TAB(43); USING "##"; P%(I, 5);
785 PRINT TAB(47); USING "###"; P%(I, 6);
786 PRINT TAB(52); USING "###"; P%(I, 7);
787 PRINT TAB(56); USING "###"; P%(I, 8);
788 PRINT TAB(61); USING "###"; P%(I, 9);
789 PRINT TAB(65); USING "##.##"; P%(I, 10) / 100;
790 PRINT TAB(73); USING "##"; P%(I, 13)
800 NEXT I
810 NEXT J
815 LOCATE 23: COLOR 14: PRINT "END OF FILE - - - HIT ANY KEY TO CONTINUE"
816 I$ = INKEY$: IF I$ = "" THEN 816
820 GOTO 110
850 LOCATE 23: COLOR 14: PRINT "HIT ANY KEY TO CONTINUE"
851 I$ = INKEY$: IF I$ = "" THEN 851
901 RETURN
902 CLS : COLOR 15: LOCATE 1, 20: PRINT PPS$
903 LOCATE 3: COLOR 11: PRINT "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
904 RETURN
1680 LOCATE 18, 28: COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT OR "; : COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW"
1681 I$ = INKEY$: IF I$ = "" THEN 1681
1682 IF UCASE$(I$) = "P" THEN 4700
1683 IF UCASE$(I$) = "V" THEN 1690
1684 IF UCASE$(I$) <> "P" OR UCASE$(I$) <> "V" THEN 1681
1690 GOSUB 5000
1700 OPEN D$(0) + LGE$ FOR INPUT AS #1
1701 FOR J = 1 TO 40: INPUT #1, TM(J), A$(J), TYN$(J): NEXT
1702 CLOSE #1
1705 GOSUB 5100
1706 CLS : COLOR 15: LOCATE 1, 35: PRINT PPS$
1707 LOCATE 3: COLOR 11: PRINT "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
1750 FOR J = 1 TO 40
1751 IF TM(J) = 0 THEN 1810
1752 GOSUB 5500
1767 FOR I = 0 TO 21
     IF P$(I) = "XXX" OR P$(I) = "" THEN 1800
1768 IF THR = 1 THEN 1769 ELSE 1770
1769 IF P%(I, 0) = 1 THEN 1772 ELSE 1800
1770 IF P%(I, 0) = -1 THEN 1772 ELSE 1800
1772 SS = SS + 1
1773 IF SS = 19 OR SS = 37 OR SS = 55 OR SS = 73 OR SS = 91 OR SS = 109 OR SS = 127 OR SS = 145 OR SS = 163 OR SS = 181 OR SS = 199 OR SS = 217 OR SS = 235 OR SS = 253 OR SS = 271 OR SS = 289 OR SS = 307 OR SS = 325 OR SS = 343 OR SS = 361 OR SS =  _
379 OR SS = 397 OR SS = 415 THEN GOSUB 850: GOSUB 902
1775 COLOR 12: PRINT LEFT$(P$(I), 10);
1776 COLOR 3: PRINT TAB(12); LEFT$(T$, 2);NN$;
1777 IF P%(I, 0) = 1 THEN PB$(I) = "R"
1778 IF P%(I, 0) = -1 THEN PB$(I) = "L"
1779 COLOR 7: PRINT TAB(22); PB$(I);
1780 PRINT TAB(25); USING "###"; P%(I, 4);
1781 PRINT TAB(30); USING "##"; P%(I, 1);
1782 PRINT TAB(34); USING "##"; P%(I, 2);
1783 PRINT TAB(39); USING "##"; P%(I, 3);
1784 PRINT TAB(43); USING "##"; P%(I, 5);
1785 PRINT TAB(47); USING "###"; P%(I, 6);
1786 PRINT TAB(52); USING "###"; P%(I, 7);
1787 PRINT TAB(56); USING "###"; P%(I, 8);
1788 PRINT TAB(61); USING "###"; P%(I, 9);
1789 PRINT TAB(65); USING "##.##"; P%(I, 10) / 100;
1790 PRINT TAB(73); USING "##"; P%(I, 13)
1800 NEXT I
1810 NEXT J
1815 LOCATE 23: COLOR 14: PRINT "END OF FILE - - - HIT ANY KEY TO CONTINUE"
1816 I$ = INKEY$: IF I$ = "" THEN 1816
1820 GOTO 110
1850 LOCATE 23: COLOR 14: PRINT "HIT ANY KEY TO CONTINUE"
1851 I$ = INKEY$: IF I$ = "" THEN 1851
1852 RETURN
1853 COLOR 15: LOCATE 1, 20: PRINT PPS$
1854 CLS : LOCATE 3: COLOR 11: PRINT "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
1855 RETURN
2300 GOSUB 5000
2305 OPEN D$(0) + LGE$ FOR INPUT AS #1
2306 FOR J = 1 TO 40: INPUT #1, TM(J), A$(J), TYN$(J): NEXT
2307 CLOSE #1
2309 GOSUB 5100
2310 LPRINT CHR$(15)
2311 WIDTH "LPT1:1", 130
2313 LPRINT TAB(55); PS$
2314 LPRINT : LPRINT "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"
2350 FOR J = 1 TO 40
2351 IF TM(J) = 0 THEN 2410
2352 GOSUB 5500
2370 FOR I = 0 TO 22
     IF B$(I) = "XXX" OR B$(I) = "" THEN 2409
2371 IF B%(I, 22) = PS OR B%(I, 23) = PS OR B%(I, 24) = PS OR B%(I, 25) = PS THEN 2372 ELSE 2409
2372 SS = SS + 1
2373 IF SS = 51 OR SS = 101 OR SS = 151 OR SS = 201 THEN GOSUB 2600
2374 IF B%(I, 22) = PS THEN 2375 ELSE 2376
2375 LPRINT B$(I); : GOTO 2377
2376 LPRINT TAB(3); B$(I);
2377 LPRINT TAB(16); LEFT$(T$, 2);NN$;
2378 IF B%(I, 0) = -1 THEN BB$(I) = "L"
2379 IF B%(I, 0) = 1 THEN BB$(I) = "R"
2380 IF B%(I, 0) = 0 THEN BB$(I) = "S"
2381 LPRINT TAB(34); BB$(I);
2382 LPRINT TAB(39); USING "###"; B%(I, 3);
2383 LPRINT TAB(45); USING "###"; B%(I, 4);
2384 LPRINT TAB(52); USING "###"; B%(I, 6);
2385 LPRINT TAB(58); USING "##"; B%(I, 7);
2386 LPRINT TAB(63); USING "##"; B%(I, 8);
2387 LPRINT TAB(68); USING "##"; B%(I, 9);
2388 LPRINT TAB(73); USING "###"; B%(I, 10);
2389 LPRINT TAB(80); USING "###"; B%(I, 5);
2390 LPRINT TAB(85); USING "###"; B%(I, 11);
2391 LPRINT TAB(91); USING "###"; B%(I, 12);
2392 LPRINT TAB(96); USING "###"; B%(I, 13);
2393 LPRINT TAB(102); USING "##"; B%(I, 14);
2394 I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)
2395 IF B%(I, 4) = 0 OR I2 = 0 THEN LPRINT TAB(107); ".000";
2396 IF I2 > 0 AND I2 < 100 THEN LPRINT TAB(107); ".0"; : LPRINT USING "##"; I2;
2397 IF I2 >= 100 THEN LPRINT TAB(107); "."; : LPRINT USING "###"; I2;
2398 IF B%(I, 4) = 0 OR I3 = 0 THEN LPRINT TAB(114); ".000";
2399 IF I3 > 0 AND I3 < 100 THEN LPRINT TAB(114); ".0"; : LPRINT USING "##"; I3;
2400 IF I3 >= 100 THEN LPRINT TAB(114); "."; : LPRINT USING "###"; I3;
2401 IF B%(I, 4) = 0 OR I4 = 0 THEN LPRINT TAB(121); ".000";
2402 IF I4 > 0 AND I4 < 100 THEN LPRINT TAB(121); ".0"; : LPRINT USING "##"; I4;
2403 IF I4 >= 100 THEN LPRINT TAB(121); "."; : LPRINT USING "###"; I4
2409 NEXT I
2410 NEXT J
2411 LPRINT CHR$(12)
2412 LPRINT CHR$(18)
2413 WIDTH "LPT1:1", 80
2415 GOTO 110
2600 LPRINT CHR$(12)
2601 LPRINT TAB(55); PS$
2602 LPRINT : LPRINT "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"
2603 RETURN
3700 GOSUB 5000
3705 OPEN D$(0) + LGE$ FOR INPUT AS #1
3706 FOR J = 1 TO 40: INPUT #1, TM(J), A$(J), TYN$(J): NEXT
3707 CLOSE #1
3710 GOSUB 5100
3711 LPRINT CHR$(15)
3712 WIDTH "LPT1:1", 130
3713 LPRINT TAB(50); PPS$
3714 LPRINT : LPRINT "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
3750 FOR J = 1 TO 40
3751 IF TM(J) = 0 THEN 3810
3752 GOSUB 5500
3767 FOR I = 0 TO 21
     IF P$(I) = "XXX" OR P$(I) = "" THEN 3800
3768 IF PC = 1 THEN 3769 ELSE 3770
3769 IF P%(I, 5) < PPS THEN 3772 ELSE 3800
3770 IF P%(I, 5) > PPS THEN 3772 ELSE 3800
3772 SS = SS + 1
3773 IF SS = 51 OR SS = 101 OR SS = 151 OR SS = 201 OR SS = 251 OR SS = 301 OR SS = 351 OR SS = 401 THEN GOSUB 3900
3775 LPRINT P$(I);
3776 LPRINT TAB(19); LEFT$(T$, 2);NN$;
3777 IF P%(I, 0) = 1 THEN PB$(I) = "R"
3778 IF P%(I, 0) = -1 THEN PB$(I) = "L"
3779 LPRINT TAB(35); PB$(I);
3780 LPRINT TAB(39); USING "###"; P%(I, 4);
3781 LPRINT TAB(45); USING "##"; P%(I, 1);
3782 LPRINT TAB(50); USING "##"; P%(I, 2);
3783 LPRINT TAB(56); USING "##"; P%(I, 3);
3784 LPRINT TAB(62); USING "##"; P%(I, 5);
3785 LPRINT TAB(68); USING "###"; P%(I, 6);
3786 LPRINT TAB(73); USING "###"; P%(I, 7);
3787 LPRINT TAB(79); USING "###"; P%(I, 8);
3788 LPRINT TAB(85); USING "###"; P%(I, 9);
3789 LPRINT TAB(90); USING "##.##"; P%(I, 10) / 100;
3790 LPRINT TAB(99); USING "##"; P%(I, 13)
3800 NEXT I
3810 NEXT J
3811 LPRINT CHR$(12)
3812 LPRINT CHR$(18)
3813 WIDTH "LPT1:1", 80
3814 GOTO 110
3900 LPRINT CHR$(12)
3901 LPRINT TAB(50); PPS$
3902 LPRINT : LPRINT "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
3903 RETURN
4700 GOSUB 5000
4705 OPEN D$(0) + LGE$ FOR INPUT AS #1
4706 FOR J = 1 TO 40: INPUT #1, TM(J), A$(J), TYN$(J): NEXT
4707 CLOSE #1
4710 GOSUB 5100
4711 LPRINT CHR$(15)
4712 WIDTH "LPT1:1", 130
4713 LPRINT TAB(50); PPS$
4714 LPRINT : LPRINT "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
4750 FOR J = 1 TO 40
4751 IF TM(J) = 0 THEN 4810
4752 GOSUB 5500
4767 FOR I = 0 TO 21
     IF P$(I) = "XXX" OR P$(I) = "" THEN 4800
4768 IF THR = 1 THEN 4769 ELSE 4770
4769 IF P%(I, 0) = 1 THEN 4772 ELSE 4800
4770 IF P%(I, 0) = -1 THEN 4772 ELSE 4800
4772 SS = SS + 1
4773 IF SS = 51 OR SS = 101 OR SS = 151 OR SS = 201 OR SS = 251 OR SS = 301 OR SS = 351 OR SS = 401 THEN GOSUB 4900
4775 LPRINT P$(I);
4776 LPRINT TAB(19); LEFT$(T$, 2);NN$;
4777 IF P%(I, 0) = 1 THEN PB$(I) = "R"
4778 IF P%(I, 0) = -1 THEN PB$(I) = "L"
4779 LPRINT TAB(35); PB$(I);
4780 LPRINT TAB(39); USING "###"; P%(I, 4);
4781 LPRINT TAB(45); USING "##"; P%(I, 1);
4782 LPRINT TAB(50); USING "##"; P%(I, 2);
4783 LPRINT TAB(56); USING "##"; P%(I, 3);
4784 LPRINT TAB(62); USING "##"; P%(I, 5);
4785 LPRINT TAB(68); USING "###"; P%(I, 6);
4786 LPRINT TAB(73); USING "###"; P%(I, 7);
4787 LPRINT TAB(79); USING "###"; P%(I, 8);
4788 LPRINT TAB(85); USING "###"; P%(I, 9);
4789 LPRINT TAB(90); USING "##.##"; P%(I, 10) / 100;
4790 LPRINT TAB(99); USING "##"; P%(I, 13)
4800 NEXT I
4810 NEXT J
4811 LPRINT CHR$(12)
4812 LPRINT CHR$(18)
4813 WIDTH "LPT1:1", 80
4814 GOTO 110
4900 LPRINT CHR$(12)
4901 LPRINT TAB(50); PPS$
4902 LPRINT : LPRINT "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
4903 RETURN
5000 CLS
     COLOR 15
     IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 5003
     PRINT "MAKE SURE DISK WITH STAT FILES IS IN DRIVE "; : COLOR 12: PRINT D$(1)
     CALL GETKEY
5003 RETURN
5100 CLS
     COLOR 15
     IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 5103
     PRINT "MAKE SURE DISK WITH TEAM FILES IS IN DRIVE "; : COLOR 12: PRINT D$(0)
     CALL GETKEY
5103 RETURN
6000 IF DRIVE >= 67 THEN 6003
     CLS : COLOR 15
     PRINT "MAKE SURE GAME DISK IS IN DRIVE"
     CALL GETKEY
6003 RETURN
6005 IF DRIVE >= 67 THEN RETURN
     CLS
     COLOR 15
     PRINT "MAKE SURE PROGRAM DISK #6 IS IN DRIVE"
     CALL GETKEY
     RETURN
4500 LOCATE 10, 8
     CALL CSMsgBox("NO LEAGUE POSITION FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 6
5500 OPEN D$(0) + "FCTEAMS." + TYN$(J) FOR RANDOM AS #1 LEN = 4342
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
GET #1, TM(J)
T$ = QPRTrim$(Q$(0))
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT: K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = QPRTrim$(Q$(I * 43 + 15))
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = QPRTrim$(Q$(I * 38 + 1004))
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = QPRTrim$(Q$(1840)): M$ = QPRTrim$(Q$(1841)): NN$ = QPRTrim$(Q$(1842)): CK = CVI(Q$(1843))
     CLOSE 1
RETURN

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

