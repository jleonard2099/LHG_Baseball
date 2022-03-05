DECLARE SUB TITLE ()
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST% (FILENAME$)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
  DIM WW(40), LL(40), TML$(40), Z1$(40), EWW(40), RWW(40), HWW(40), ELL(40), RLL(40), HLL(40), LTW(40), LTL(40)
  DIM H%(162), H$(162), V%(162), PL$(162), W$(162), L$(162), I%(162)
  DIM TWS(40), TLS(40), CWS(40), CLT(40), LWS(40), LLS(40)
  DIM D$(2), ITEM$(10)
4 FOR I = 1 TO 40: TWS(I) = 0: TLS(I) = 0: LWS(I) = 0: LLS(I) = 0: CWS(I) = 0: CLT(I) = 0: NEXT
  OPEN "DEV.BAS" FOR INPUT AS #2: FOR I = 0 TO 2: INPUT #2, D$(I): NEXT: CLOSE 2
6 TMR = 40: PV = 0: GOTO 2300
9 FOR KK = 1 TO 40: YY = YY + 1
  LOCATE 7, 20: COLOR 14: TMR = TMR - 1: PRINT USING "###"; TMR
  IF Z1$(KK) = "*" THEN 60 ELSE 10
10 TML$(YY) = Z1$(KK)
11 LOCATE 4: COLOR 15: PRINT "COMPUTING RECORDS . . . . . PLEASE WAIT"
20 OPEN D$(1) + Z1$(KK) + "S" FOR INPUT AS #1
21 INPUT #1, XX%
22 FOR I = 1 TO 162: INPUT #1, H%(I): NEXT I
23 FOR I = 1 TO 162: INPUT #1, H$(I): NEXT I
24 FOR I = 1 TO 162: INPUT #1, V%(I): NEXT I
25 FOR I = 1 TO 162: INPUT #1, PL$(I): NEXT I
26 FOR I = 1 TO 162: INPUT #1, W$(I): NEXT I
27 FOR I = 1 TO 162: INPUT #1, L$(I): NEXT I
28 FOR I = 1 TO 162: INPUT #1, I%(I): NEXT I
29 CLOSE #1
30 FOR I = 1 TO XX%
31 IF H%(I) > V%(I) THEN WW(YY) = WW(YY) + 1
32 IF H%(I) > V%(I) AND I%(I) > 9 THEN EWW(YY) = EWW(YY) + 1
33 IF H%(I) > V%(I) AND PL$(I) = "A" THEN RWW(YY) = RWW(YY) + 1
34 IF H%(I) > V%(I) AND PL$(I) = "H" THEN HWW(YY) = HWW(YY) + 1
35 IF V%(I) > H%(I) THEN LL(YY) = LL(YY) + 1
36 IF V%(I) > H%(I) AND I%(I) > 9 THEN ELL(YY) = ELL(YY) + 1
37 IF V%(I) > H%(I) AND PL$(I) = "A" THEN RLL(YY) = RLL(YY) + 1
38 IF V%(I) > H%(I) AND PL$(I) = "H" THEN HLL(YY) = HLL(YY) + 1
   IF H%(I) > V%(I) THEN LWS(YY) = LWS(YY) + 1: LLS(YY) = 0
   IF LWS(YY) > TWS(YY) THEN TWS(YY) = LWS(YY)
   IF H%(I) < V%(I) THEN LLS(YY) = LLS(YY) + 1: LWS(YY) = 0
   IF LLS(YY) > TLS(YY) THEN TLS(YY) = LLS(YY)
39 IF H%(I) > V%(I) THEN 40
   IF H%(I) < V%(I) THEN 42
40 CWS(YY) = CWS(YY) + 1: CLT(YY) = 0: GOTO 44
42 CLT(YY) = CLT(YY) + 1: CWS(YY) = 0
44 NEXT I
45 IF XX% <= 10 THEN LTL(YY) = LL(YY): LTW(YY) = WW(YY): GOTO 60
46 M = XX% - 9
47 FOR J = M TO XX%
48 IF H%(J) > V%(J) THEN LTW(YY) = LTW(YY) + 1
49 IF V%(J) > H%(J) THEN LTL(YY) = LTL(YY) + 1
50 NEXT J
60 NEXT KK
62 OPEN D$(2) + LG$ + ".STD" FOR OUTPUT AS #1
63 WRITE #1, LGF$, D1$, D2$, D3$, D4$
67 FOR I = 1 TO 40: WRITE #1, TML$(I), WW(I), LL(I), TWS(I), TLS(I), CWS(I), CLT(I), HWW(I), HLL(I), RWW(I), RLL(I), EWW(I), ELL(I), LTW(I), LTL(I): NEXT
69 CLOSE #1
70 GOTO 2300
950 FOR X = AA TO ZZ - 1: FOR Y = X + 1 TO ZZ: IF WW(X) - LL(X) > WW(Y) - LL(Y) THEN 985
955 IF WW(X) - LL(X) = WW(Y) - LL(Y) AND WW(X) <= WW(Y) AND WW(X) > LL(X) THEN 985
960 IF WW(X) - LL(X) = WW(Y) - LL(Y) AND WW(X) >= WW(Y) AND WW(X) < LL(X) THEN 985
965 SWAP TML$(X), TML$(Y)
    SWAP TWS(X), TWS(Y)
    SWAP TLS(X), TLS(Y)
    SWAP CWS(X), CWS(Y)
    SWAP CLT(X), CLT(Y)
    SWAP WW(X), WW(Y)
    SWAP LL(X), LL(Y)
    SWAP EWW(X), EWW(Y)
    SWAP ELL(X), ELL(Y)
    SWAP HLL(X), HLL(Y)
    SWAP RLL(X), RLL(Y)
    SWAP HWW(X), HWW(Y)
    SWAP RWW(X), RWW(Y)
    SWAP LTL(X), LTL(Y)
    SWAP LTW(X), LTW(Y)
985 NEXT Y: NEXT X
    RETURN
2000 FOR NT = 1 TO 4
     AA = NT * 10 - 9: ZZ = NT * 10: GOSUB 950
2011 CLS : LOCATE 1, 35: COLOR 11: PRINT LGF$
     LOCATE 2: COLOR 14: PRINT TAB(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
     LOCATE 3: COLOR 14
     IF NT = 1 THEN PRINT D1$;
     IF NT = 2 THEN PRINT D2$;
     IF NT = 3 THEN PRINT D3$;
     IF NT = 4 THEN PRINT D4$;
     PRINT TAB(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"
     FOR I = AA TO ZZ
2012 IF TML$(I) = "" THEN 2014
2013 COLOR 3: PRINT TML$(I); : COLOR 15: PRINT TAB(14); USING "###"; WW(I) + LL(I); TAB(18);
     PRINT USING "###"; WW(I); TAB(22);
     PRINT USING "###"; LL(I);
     IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN PRINT TAB(26); "1.000"; : GOTO 2015
     IF WW(I) = 0 THEN PRINT TAB(27); ".000"; : GOTO 2015
     IF WW(I) + LL(I) <> 0 THEN PRINT TAB(27); USING ".###"; WW(I) / (WW(I) + LL(I)); : GOTO 2015
     IF WW(I) + LL(I) = 0 THEN PRINT TAB(27); ".000";
2015 IF ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 = 0 THEN PRINT TAB(32); " ---";
     IF ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 <> 0 THEN PRINT TAB(32); USING "##.#"; ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2;
     PRINT TAB(44); USING "##"; TWS(I); TAB(47); TLS(I); TAB(51); CWS(I); TAB(54); CLT(I);
     PRINT TAB(57); USING "##"; HWW(I); : PRINT TAB(60); USING "##"; HLL(I);
     PRINT TAB(63); USING "##"; RWW(I); : PRINT TAB(66); USING "##"; RLL(I);
     PRINT TAB(69); USING "##"; EWW(I); : PRINT TAB(72); USING "##"; ELL(I);
     PRINT TAB(75); USING "##"; LTW(I); : PRINT TAB(78); USING "##"; LTL(I)
2014 NEXT
     LOCATE 4, 38: PRINT USING "###"; 163 - WW(NT * 10 - 9) - LL(NT * 10 - 8)
     LOCATE 23: COLOR 14: PRINT "HIT ANY KEY TO CONTINUE"
2036 I$ = INKEY$: IF I$ = "" THEN 2036
     NEXT NT
2300 CLS
     CALL TITLE
   ITEM$(1) = "UPDATE LEAGUE STANDINGS      "
   ITEM$(2) = "VIEW / PRINT LEAGUE STANDINGS"
   ITEM$(3) = "RETURN TO MAIN MENU          "
     SELECTION% = 1
     Count% = 3
     Start% = 1
     SCANCODE% = 3
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 6
     Column% = 27
     CALL Box0(5, 26, 9, 56, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN SELECTION% = 3
     I = SELECTION%
2306 IF I = 1 THEN 2500
2307 IF I = 2 THEN 2600
2308 IF I = 3 THEN GOSUB 2370:RUN "NEWLDR.EXE"
2370 IF DRIVE >= 67 THEN 2375
     CLS
     PRINT "MAKE SURE PROGRAM DISK 3 IS IN DRIVE"
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
2371 I$ = INKEY$: IF I$ = "" THEN 2371
2375 RETURN
2500 CLS
4000 Spec$ = D$(2) + "*.TMS"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 2300
   REDIM F$(0 TO Count%)
   FOR X = 1 TO Count%
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT LEAGUE TO UPDATE": COLOR 15
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
     IF ExitCode% = 27 THEN 2300
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
2535 I$ = INKEY$: IF I$ = "" THEN 2535
    IF UCASE$(I$) = "Y" THEN 1310
    F$ = QPRTrim$(F$)
    LG$ = LEFT$(F$, LEN(F$) - 4)
2530 OPEN D$(2) + LG$ + ".TMS" FOR INPUT AS #2
2540 INPUT #2, LGF$: INPUT #2, D1$: INPUT #2, D2$: INPUT #2, D3$: INPUT #2, D4$
2550 FOR X = 1 TO 40: INPUT #2, Z1$(X): NEXT
2560 CLOSE 2
2565 GOTO 9
2600 CLS
    Spec$ = D$(2) + "*.STD"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 2300
   REDIM F$(0 TO Count%)
   FOR X = 1 TO Count%
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count%, 0)

1311 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT LEAGUE TO VIEW": COLOR 15
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
     IF ExitCode% = 27 THEN 2300
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
2536 I$ = INKEY$: IF I$ = "" THEN 2536
    IF UCASE$(I$) = "Y" THEN 1310
    F$ = QPRTrim$(F$)
    LGS$ = LEFT$(F$, LEN(F$) - 4)

2602 OPEN D$(2) + LGS$ + ".STD" FOR INPUT AS #1
2603 INPUT #1, LGF$, D1$, D2$, D3$, D4$
2667 FOR I = 1 TO 40: INPUT #1, TML$(I), WW(I), LL(I), TWS(I), TLS(I), CWS(I), CLT(I), HWW(I), HLL(I), RWW(I), RLL(I), EWW(I), ELL(I), LTW(I), LTL(I): NEXT
2669 CLOSE #1
2670 LOCATE 10, 1
     COLOR 12
     PRINT "P"; : COLOR 7: PRINT "RINT OR ";
     COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW STANDINGS"
2671 I$ = INKEY$: IF I$ = "" THEN 2671
2672 IF UCASE$(I$) = "P" GOTO 3000
2673 IF UCASE$(I$) = "V" GOTO 2000
3000 MJ = 0: LPRINT CHR$(15)
3001 WIDTH "LPT1:1", 130
     LPRINT TAB(35); LGF$
     FOR NT = 1 TO 4
     AA = NT * 10 - 9: ZZ = NT * 10: GOSUB 950
     LPRINT TAB(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
     IF NT = 1 THEN LPRINT D1$;
     IF NT = 2 THEN LPRINT D2$;
     IF NT = 3 THEN LPRINT D3$;
     IF NT = 4 THEN LPRINT D4$;
     LPRINT TAB(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"
     FOR I = AA TO ZZ
3012 IF TML$(I) = "" THEN 3014
3013 COLOR 3: LPRINT TML$(I); : COLOR 15: LPRINT TAB(14); USING "###"; WW(I) + LL(I); TAB(18);
     LPRINT USING "###"; WW(I); TAB(22);
     LPRINT USING "###"; LL(I);
     IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN LPRINT TAB(26); "1.000"; : GOTO 3015
     IF WW(I) = 0 THEN LPRINT TAB(27); ".000"; : GOTO 3015
     IF WW(I) + LL(I) <> 0 THEN LPRINT TAB(27); USING ".###"; WW(I) / (WW(I) + LL(I)); : GOTO 3015
     IF WW(I) + LL(I) = 0 THEN LPRINT TAB(27); ".000";
3015 IF ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 = 0 THEN LPRINT TAB(32); " ---";
     IF ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 <> 0 THEN LPRINT TAB(32); USING "##.#"; ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2;
     IF I = NT * 10 - 9 THEN LPRINT TAB(38); USING "###"; 163 - WW(NT * 10 - 9) - LL(NT * 10 - 8);
     LPRINT TAB(44); USING "##"; TWS(I); TAB(47); TLS(I); TAB(51); CWS(I); TAB(54); CLT(I);
     LPRINT TAB(57); USING "##"; HWW(I); : LPRINT TAB(60); USING "##"; HLL(I);
     LPRINT TAB(63); USING "##"; RWW(I); : LPRINT TAB(66); USING "##"; RLL(I);
     LPRINT TAB(69); USING "##"; EWW(I); : LPRINT TAB(72); USING "##"; ELL(I);
     LPRINT TAB(75); USING "##"; LTW(I); : LPRINT TAB(78); USING "##"; LTL(I)
3014 NEXT
     LPRINT : LPRINT
     NEXT NT
     LPRINT CHR$(12)
3096 LPRINT CHR$(18)
3097 WIDTH "LPT1:1", 80
3100 GOTO 2300

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

