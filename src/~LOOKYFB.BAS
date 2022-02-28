DECLARE SUB TITLE ()
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST% (FILENAME$)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DIM O%(120), O$(120), T%(120), P%(1), LC(15), LC$(15, 1), Q$(500), VV$(500)
DIM V%(500), LR(9), LI(9), LI$(9, 1)
DIM K(1, 12), K2(1, 13, 17), Z2(13, 17), Z(38), PA%(5), LP(3), LK(2), PK(2), LF(1)
DIM W6(1, 1), A$(1), A1$(1, 9), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2)
DIM Z1(38), CRD(120), SITE$(120), D$(2), TT(3), II(3), PP(3), YY(3)
DIM A7$(0, 1), QB(3, 10), AM(1, 4), AF(1, 4), LR$(9, 1), LP$(3, 1), LK$(2, 1)
DIM PK$(2, 1), LF$(1)
DIM H$(120), DS$(1, 14), DI$(1, 9), GI(1, 9, 2), GS(1, 14, 1), AP%(1, 120)
OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): INPUT #1, D$(1): CLOSE #1
   C% = GetDrive%
14 CLS : CALL TITLE


   LOCATE 5, 5: COLOR 12: PRINT "**"; : COLOR 14: PRINT " MAKE SURE STATS DISK IS INSERTED IN DRIVE "; D$(1); : COLOR 12: PRINT " **"
   PRINT : PRINT : PRINT : PRINT : J = 6: GOSUB 2075: LOCATE , 30: COLOR 3: PRINT "VIEW/PRINT STATS IN:"
   LOCATE , 30: COLOR 15: PRINT "C"; : COLOR 7: PRINT "OLLEGE FORMAT"
   LOCATE , 30: COLOR 15: PRINT "P"; : COLOR 7: PRINT "RO FORMAT"
   LOCATE , 30: COLOR 15: PRINT "E"; : COLOR 7: PRINT "XIT TO MAIN MENU"
24 I$ = INKEY$: IF I$ = "" THEN 24
   IF UCASE$(I$) = "C" THEN G9 = 0: GOTO 86
   IF UCASE$(I$) = "P" THEN G9 = 1: GOTO 86
   IF UCASE$(I$) = "E" OR I$=CHR$(27) THEN Z$ = "END": GOTO 90
   GOTO 24
86 GOSUB 3056
   B$ = A$: GOSUB 6585
   COLOR 15, 0: CLS : CALL TITLE
   Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
   IF LEFT$(D$(1), 1) <> "C" THEN PRINT "MAKE SURE PROPER STATS DISK IS IN DRIVE "; D$(1): GOSUB 4091
   D% = 0
90 IF UCASE$(Z$) = "END" AND C% >= 67 THEN RUN "HELLO.EXE"
   IF UCASE$(Z$) = "END" AND C% < 67 THEN CLS : PRINT "INSERT GAME DISK IN DRIVE": GOSUB 4091: RUN "HELLO.EXE"
   GOSUB 9700: PX = 0: GOSUB 9800
   IF NOT EXIST%(D$(1) + Z$) THEN 5025
95 OPEN D$(1) + Z$ FOR INPUT AS #1
   INPUT #1, A$(PX)
    FOR I = 0 TO 9: INPUT #1, A1$(PX, I): NEXT I
    FOR I = 0 TO 5: INPUT #1, A2$(PX, I): NEXT I
    FOR I = 0 TO 3: INPUT #1, A3$(PX, I): NEXT I
    FOR I = 0 TO 2: INPUT #1, A4$(PX, I): NEXT I
    FOR I = 0 TO 2: INPUT #1, A5$(PX, I): NEXT I
    INPUT #1, A6$(PX, 0)
    FOR I = 0 TO 1: INPUT #1, A7$(PX, I): NEXT I
    FOR J = 0 TO 13
    FOR K = 0 TO 17: INPUT #1, Z2(J, K): NEXT K
    NEXT J
    FOR L = 0 TO 38: INPUT #1, Z(L): NEXT L
    FOR L = 0 TO 38: INPUT #1, Z1(L): NEXT L
    FOR J = 0 TO 9: INPUT #1, DI$(PX, J): FOR J1 = 0 TO 2: INPUT #1, GI(PX, J, J1): NEXT: NEXT
    FOR J = 0 TO 14: INPUT #1, DS$(PX, J): FOR J1 = 0 TO 1: INPUT #1, GS(PX, J, J1): NEXT: NEXT
    FOR I = 0 TO 9: INPUT #1, LR(I): INPUT #1, LR$(I, 0): INPUT #1, LR$(I, 1): NEXT
    FOR I = 0 TO 15: INPUT #1, LC(I): INPUT #1, LC$(I, 0): INPUT #1, LC$(I, 1): NEXT
    FOR I = 0 TO 3: INPUT #1, LP(I): INPUT #1, LP$(I, 0): INPUT #1, LP$(I, 1): NEXT
    FOR I = 0 TO 2: INPUT #1, LK(I): INPUT #1, LK$(I, 0): INPUT #1, LK$(I, 1): NEXT
    FOR I = 0 TO 2: INPUT #1, PK(I): INPUT #1, PK$(I, 0): INPUT #1, PK$(I, 1): NEXT
    FOR I = 0 TO 1: INPUT #1, LF(I): INPUT #1, LF$(I): FOR I1 = 0 TO 4: INPUT #1, AM(I, I1): INPUT #1, AF(I, I1): NEXT: NEXT
    FOR I = 0 TO 9: INPUT #1, LI(I): INPUT #1, LI$(I, 0): INPUT #1, LI$(I, 1): NEXT
    CLOSE #1
    FOR J = 0 TO 13
    FOR K = 0 TO 17: K2(PX, J, K) = Z2(J, K): NEXT K
    NEXT J
    FOR L = 0 TO 12: K(0, L) = Z(L): NEXT L
    FOR L = 0 TO 12: K(1, L) = Z1(L): NEXT L
    W6(0, 0) = Z(13)
    W6(0, 1) = Z(14)
    W6(1, 0) = Z1(13): W6(1, 1) = Z1(14)
    GOTO 8530

4091 I$ = INKEY$: IF I$ = "" THEN 4091
     RETURN

5025 CLS : PRINT "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
     PRINT TAB(10); "CHECK THAT CORRECT DISK IS IN DRIVE"
     GOSUB 4091
     GOTO 14

8530 REM:*** VIEW STATS ON SCREEN X=0, PRINTER X=1 ***
     DM% = 0
     CLS : CALL TITLE
     J = 4: GOSUB 2075: COLOR 7
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "S"; : COLOR 7: PRINT "CREEN"
     LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "P"; : COLOR 7: PRINT "RINTER"
8651 I$ = INKEY$: IF I$ = "" THEN 8651
     COLOR 14, 0
     IF UCASE$(I$) = "S" THEN X = 0: GOTO 8535
     IF UCASE$(I$) = "P" THEN X = 1: GOTO 9910
     GOTO 8651
8535 GOSUB 8660
     GOSUB 9806
     FOR I = 1 TO Y%
     COLOR 7
     IF I / 2 = INT(I / 2) THEN COLOR 15
     PRINT O$(I); : LOCATE , 19: PRINT USING "###"; O%(I); : PRINT "-"; : PRINT USING "###   "; T%(I);
     IF O%(I) > T%(I) THEN W% = W% + 1: PRINT "W";
     IF O%(I) < T%(I) THEN L% = L% + 1: PRINT "L";
     IF O%(I) = T%(I) THEN X% = X% + 1: PRINT "T";
     LOCATE , 31: PRINT USING "####,###   "; CRD(I); : PRINT H$(I); "  "; LEFT$(SITE$(I), 24);
     LOCATE , 69
     PRINT "("; : PRINT USING "##"; W%; : PRINT "-"; : PRINT USING "##"; L%; : PRINT "-"; : PRINT USING "##"; X%; : PRINT ")"
     P%(0) = P%(0) + O%(I): P%(1) = P%(1) + T%(I)
     IF I = 20 OR I = 40 OR I = 60 OR I = 80 OR I = 100 OR I = 120 THEN gosub 4091:GOSUB 9806
     NEXT I
     IF G9 = 0 THEN LOCATE 24, 1: PRINT USING "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; PW; SS;
     P%(0) = 0: P%(1) = 0: W% = 0: L% = 0: X% = 0
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     LOCATE 2: COLOR 12: PRINT A$(0); " STATISTICS REPORT"
     PRINT
     COLOR 14: PRINT "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
     COLOR 15: PRINT A$(0); : LOCATE , 30: PRINT USING " ###  ###  ###  ###   ###"; Z(32); Z(33); Z(34); Z(35); Z(32) + Z(33) + Z(34) + Z(35) + Z(36)
     PRINT "OPPONENTS"; : LOCATE , 30: PRINT USING " ###  ###  ###  ###   ###"; Z1(32); Z1(33); Z1(34); Z1(35); Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)
     PRINT
     COLOR 14: PRINT "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
     COLOR 15: PRINT A$(0); : LOCATE , 30: PRINT USING " ##.#  ##.#  ##.#  ##.#   ##.#"; Z(32) / Y%; Z(33) / Y%; Z(34) / Y%; Z(35) / Y%; (Z(32) + Z(33) + Z(34) + Z(35) + Z(36)) / Y%
     PRINT "OPPONENTS"; : LOCATE , 30: PRINT USING " ##.#  ##.#  ##.#  ##.#   ##.#"; Z1(32) / Y%; Z1(33) / Y%; Z1(34) / Y%; Z1(35) / Y%; (Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)) / Y%
     PRINT
     HD = 0: AD = 0: ND = 0: TD = 0
     HT = 0: CT = 0: NT = 0: TT = 0
     HA = 0: AA = 0: NA = 0: TA = 0
     FOR I = 1 TO Y%
     IF LEFT$(H$(I), 1) = "H" THEN HD = HD + 1: HT = HT + CRD(I)
     IF LEFT$(H$(I), 1) = "A" THEN AD = AD + 1: CT = CT + CRD(I)
     IF LEFT$(H$(I), 1) = "X" THEN ND = ND + 1: NT = NT + CRD(I)
     TD = TD + 1: TT = TT + CRD(I)
     NEXT
     IF HD <> 0 THEN HA = HT / HD
     IF AD <> 0 THEN AA = CT / AD
     IF ND <> 0 THEN NA = NT / ND
     IF TD <> 0 THEN TA = TT / TD
     COLOR 14: PRINT "ATTENDANCE    DATES      TOTAL    AVERAGE"
     COLOR 15: PRINT USING "HOME            ##    #####,###   ###,###"; HD; HT; HA
     COLOR 15: PRINT USING "AWAY            ##    #####,###   ###,###"; AD; CT; AA
     COLOR 15: PRINT USING "NEUTRAL         ##    #####,###   ###,###"; ND; NT; NA
     COLOR 15: PRINT USING "TOTAL           ##    #####,###   ###,###"; TD; TT; TA
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     LOCATE 2: COLOR 12: PRINT "TEAM STATISTICS CATEGORY"; TAB(41); A$(0); TAB(61); "OPPONENT TOTALS"
     COLOR 7
     PRINT "FIRST DOWNS"; TAB(45); USING "####"; K(0, 0); : PRINT TAB(65); USING "####"; K(1, 0)
     PRINT "RUSHING ATTEMPTS"; TAB(45);
     IF G9 = 0 THEN PRINT USING "####"; K(0, 1);  ELSE PRINT USING "####"; K(0, 1) - W6(0, 0);
     PRINT TAB(65);
     IF G9 = 0 THEN PRINT USING "####"; K(1, 1) ELSE PRINT USING "####"; K(1, 1) - W6(1, 0)
     COLOR 15
     PRINT "NET RUSHING YARDAGE"; TAB(45);
     IF G9 = 0 THEN PRINT USING "####"; K(0, 2);  ELSE PRINT USING "####"; K(0, 2) + W6(0, 1);
     PRINT TAB(65);
     IF G9 = 0 THEN PRINT USING "####"; K(1, 2) ELSE PRINT USING "####"; K(1, 2) + W6(1, 1)
     COLOR 7
     PRINT "YARDS PER RUSH"; TAB(45);
     IF G9 = 0 AND K(0, 1) <> 0 THEN PRINT USING "##.#"; K(0, 2) / K(0, 1);
     IF G9 <> 0 AND K(0, 1) - W6(0, 0) <> 0 THEN PRINT USING "##.#"; (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0));
     PRINT TAB(65);
     IF G9 = 0 AND K(1, 1) <> 0 THEN PRINT USING "##.#"; K(1, 2) / K(1, 1)
     IF G9 <> 0 AND K(1, 1) - W6(1, 0) <> 0 THEN PRINT USING "##.#"; (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0))
     IF G9 = 0 AND K(1, 1) = 0 OR G9 <> 0 AND K(1, 1) - W6(1, 0) = 0 THEN PRINT
     PRINT "RUSHING YARDS PER GAME"; TAB(44);
     IF G9 = 0 THEN PRINT USING "###.#"; K(0, 2) / Y%;  ELSE PRINT USING "###.#"; (K(0, 2) + W6(0, 1)) / Y%;
     PRINT TAB(64);
     IF G9 = 0 THEN PRINT USING "###.#"; K(1, 2) / Y% ELSE PRINT USING "###.#"; (K(1, 2) + W6(1, 1)) / Y%
     PRINT
     PRINT "PASSES ATTEMPTED"; TAB(45); USING "####"; K(0, 3); : PRINT TAB(65); USING "####"; K(1, 3)
     PRINT "PASSES COMPLETED"; TAB(45); USING "####"; K(0, 4); : PRINT TAB(65); USING "####"; K(1, 4)
     PRINT "PASSES HAD INTERCEPTED"; TAB(45); USING "####"; K(0, 5); : PRINT TAB(65); USING "####"; K(1, 5)
     PRINT "PASS COMPLETION PERCENTAGE"; TAB(44);
     IF K(0, 3) <> 0 THEN PRINT USING "###.#"; K(0, 4) / K(0, 3) * 100;
     IF K(1, 3) <> 0 THEN PRINT TAB(64); USING "###.#"; K(1, 4) / K(1, 3) * 100
     IF K(1, 3) = 0 THEN PRINT
     COLOR 15
     PRINT "NET YARDS PASSING"; TAB(45); USING "####"; K(0, 6); : PRINT TAB(65); USING "####"; K(1, 6)
     COLOR 7
     PRINT "YARDS PER PASS ATTEMPT"; TAB(45);
     IF K(0, 3) <> 0 THEN PRINT USING "##.#"; K(0, 6) / K(0, 3);
     IF K(1, 3) <> 0 THEN PRINT TAB(65); USING "##.#"; K(1, 6) / K(1, 3)
     IF K(1, 3) = 0 THEN PRINT
     PRINT "YARDS PER PASS COMPLETION"; TAB(45);
     IF K(0, 4) <> 0 THEN PRINT USING "##.#"; K(0, 6) / K(0, 4);
     IF K(1, 4) <> 0 THEN PRINT TAB(65); USING "##.#"; K(1, 6) / K(1, 4)
     IF K(1, 4) = 0 THEN PRINT
     PRINT "PASSING YARDS PER GAME"; TAB(44); USING "###.#"; K(0, 6) / Y%; : PRINT TAB(64); USING "###.#"; K(1, 6) / Y%
     PRINT

     PRINT "TOTAL PLAYS"; TAB(45);
     IF G9 = 0 THEN PRINT USING "####"; K(0, 1) + K(0, 3);  ELSE PRINT USING "####"; K(0, 1) - W6(0, 0) + K(0, 3);
     PRINT TAB(65);
     IF G9 = 0 THEN PRINT USING "####"; K(1, 1) + K(1, 3) ELSE PRINT USING "####"; K(1, 1) - W6(1, 0) + K(1, 3)
     PRINT "TOTAL PLAYS PER GAME"; TAB(44);
     IF G9 = 0 THEN PRINT USING "###.#"; (K(0, 1) + K(0, 3)) / Y%;  ELSE PRINT USING "###.#"; (K(0, 1) - W6(0, 0) + K(0, 3)) / Y%;
     PRINT TAB(64);
     IF G9 = 0 THEN PRINT USING "###.#"; (K(1, 1) + K(1, 3)) / Y% ELSE PRINT USING "###.#"; (K(1, 1) - W6(1, 0) + K(1, 3)) / Y%
     COLOR 15
     PRINT "TOTAL NET YARDS"; TAB(45);
     IF G9 = 0 THEN PRINT USING "####"; K(0, 2) + K(0, 6);  ELSE PRINT USING "####"; K(0, 2) + W6(0, 1) + K(0, 6);
     PRINT TAB(65);
     IF G9 = 0 THEN PRINT USING "####"; K(1, 2) + K(1, 6) ELSE PRINT USING "####"; K(1, 2) + W6(1, 1) + K(1, 6)
     COLOR 7
     PRINT "YARDS GAINED PER PLAY"; TAB(45);
     IF G9 = 0 THEN PRINT USING "##.#"; (K(0, 2) + K(0, 6)) / (K(0, 1) + K(0, 3));  ELSE PRINT USING "##.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / (K(0, 1) - W6(0, 0) + K(0, 3));
     PRINT TAB(64);
     IF G9 = 0 THEN PRINT USING "##.#"; (K(1, 2) + K(1, 6)) / (K(1, 1) + K(1, 3)) ELSE PRINT USING "##.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / (K(1, 1) - W6(1, 0) + K(1, 3))
     PRINT "YARDS GAINED PER GAME"; TAB(44);
     IF G9 = 0 THEN PRINT USING "###.#"; (K(0, 2) + K(0, 6)) / Y%;  ELSE PRINT USING "###.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / Y%;
     PRINT TAB(64);
     IF G9 = 0 THEN PRINT USING "###.#"; (K(1, 2) + K(1, 6)) / Y% ELSE PRINT USING "###.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / Y%
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     LOCATE 2: COLOR 12: PRINT "TEAM STATISTICS CATEGORY"; TAB(41); A$(0); TAB(61); "OPPONENT TOTALS"
     COLOR 7
     PRINT "KICKOFF RETURNS / KICKOFF RETURN YARDS"; TAB(41);
     PRINT USING "###"; Z(22); : PRINT "/"; : PRINT USING "####"; Z(23);
     PRINT TAB(61); USING "###"; Z1(22); : PRINT "/"; : PRINT USING "####"; Z1(23)
     PRINT "AVERAGE YARDAGE PER KICKOFF RETURN"; TAB(45);
     IF Z(22) <> 0 THEN PRINT USING "##.#"; Z(23) / Z(22);
     IF Z1(22) <> 0 THEN PRINT TAB(65); USING "##.#"; Z1(23) / Z1(22)
     IF Z1(22) = 0 THEN PRINT
     PRINT "KICKOFF RETURNS PER GAME"; TAB(45); : PRINT USING "##.#"; Z(22) / Y%; : PRINT TAB(65); USING "##.#"; Z1(22) / Y%
     PRINT "PUNT RETURNS / PUNT RETURN YARDS"; TAB(41);
     PRINT USING "###"; Z(24); : PRINT "/"; : PRINT USING "####"; Z(25);
     PRINT TAB(61); USING "###"; Z1(24); : PRINT "/"; : PRINT USING "####"; Z1(25)
     PRINT "AVERAGE YARDAGE PER PUNT RETURN"; TAB(45);
     IF Z(24) <> 0 THEN PRINT USING "##.#"; Z(25) / Z(24);
     IF Z1(24) <> 0 THEN PRINT TAB(65); USING "##.#"; Z1(25) / Z1(24)
     IF Z1(24) = 0 THEN PRINT
     PRINT "PUNT RETURNS PER GAME"; TAB(45); : PRINT USING "##.#"; Z(24) / Y%; : PRINT TAB(65); USING "##.#"; Z1(24) / Y%
     PRINT "INTERCEPTION RETURNS / RETURN YARDS"; TAB(41);
     PRINT USING "###"; Z(26); : PRINT "/"; : PRINT USING "####"; Z(27);
     PRINT TAB(61); USING "###"; Z1(26); : PRINT "/"; : PRINT USING "####"; Z1(27)
     PRINT "AVERAGE YARDAGE PER RETURN"; TAB(45);
     IF Z(26) <> 0 THEN PRINT USING "##.#"; Z(27) / Z(26);
     IF Z1(26) <> 0 THEN PRINT TAB(65); USING "##.#"; Z1(27) / Z1(26)
     IF Z1(26) = 0 THEN PRINT
     PRINT "INTERCEPTION RETURNS PER GAME"; TAB(45); : PRINT USING "##.#"; Z(26) / Y%; : PRINT TAB(65); USING "##.#"; Z1(26) / Y%
     PRINT
     PRINT "PUNTS / TOTAL PUNT YARDAGE"; TAB(41);
     PRINT USING "###"; K(0, 7); : PRINT "/"; : PRINT USING "####"; K(0, 8);
     PRINT TAB(61); USING "###"; K(1, 7); : PRINT "/"; : PRINT USING "####"; K(1, 8)
     PRINT "AVERAGE YARDS PER PUNT"; TAB(45);
     IF K(0, 7) <> 0 THEN PRINT USING "##.#"; K(0, 8) / K(0, 7);
     IF K(1, 7) <> 0 THEN PRINT TAB(65); USING "##.#"; K(1, 8) / K(1, 7)
     IF K(1, 7) = 0 THEN PRINT
     PRINT "AVERAGE NUMBER OF PUNTS PER GAME"; TAB(45); : PRINT USING "##.#"; K(0, 7) / Y%; : PRINT TAB(65); USING "##.#"; K(1, 7) / Y%
     PRINT
     PRINT "FUMBLES / FUMBLES LOST"; TAB(41);
     PRINT USING "###"; K(0, 9); : PRINT "/"; : PRINT USING "####"; K(0, 10);
     PRINT TAB(61); USING "###"; K(1, 9); : PRINT "/"; : PRINT USING "####"; K(1, 10)
     PRINT "PENALTIES / YARDS PENALIZED"; TAB(41);
     PRINT USING "###"; K(0, 11); : PRINT "/"; : PRINT USING "####"; K(0, 12);
     PRINT TAB(61); USING "###"; K(1, 11); : PRINT "/"; : PRINT USING "####"; K(1, 12)
     PRINT "AVERAGE YARDS PER PENALTY"; TAB(45);
     IF K(0, 11) <> 0 THEN PRINT USING "##.#"; K(0, 12) / K(0, 11);
     IF K(1, 11) <> 0 THEN PRINT TAB(65); USING "##.#"; K(1, 12) / K(1, 11)
     IF K(1, 11) = 0 THEN PRINT
     PRINT "PENALTIES PER GAME / YARDS PER GAME"; TAB(40);
     PRINT USING "##.#"; K(0, 11) / Y%; : PRINT "/"; : PRINT USING "##.#"; K(0, 12) / Y%;
     PRINT TAB(60); USING "##.#"; K(1, 11) / Y%; : PRINT "/"; : PRINT USING "##.#"; K(1, 12) / Y%
     PRINT "SAFETIES"; TAB(45); : PRINT USING "####"; Z(21); : PRINT TAB(65); USING "####"; Z1(21)
     PRINT "TIMES SACKED / YARDS LOST"; TAB(41);
     PRINT USING "###"; W6(0, 0); : PRINT "/"; : PRINT USING "####"; W6(0, 1);
     PRINT TAB(61); USING "###"; W6(1, 0); : PRINT "/"; : PRINT USING "####"; W6(1, 1)
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     LOCATE 4, 1
     COLOR 12
     PRINT "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"
     I = 0
     GOSUB 8699
     FOR I1 = 0 TO 13
     IF K2(I, I1, 0) = 0 THEN 8560
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     IF I1 <= 9 THEN PRINT A1$(I, I1);
     IF I1 > 9 THEN PRINT A3$(I, I1 - 10);
     PRINT TAB(18); USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 0); K2(I, I1, 1); K2(I, I1, 1) / K2(I, I1, 0); K2(I, I1, 1) / Y%; K2(I, I1, 2);
     IF I1 > 9 THEN PRINT : GOTO 8560
     PRINT USING "###"; LR(I1); : PRINT LR$(I1, 0); TAB(64); LR$(I1, 1)
8560 NEXT I1
     COLOR 14
     PRINT
     PRINT A$(0); TAB(18);
     IF G9 = 0 THEN PRINT USING "####  ####   ##.#      ###.#    ##"; K(0, 1); K(0, 2); K(0, 2) / K(0, 1); K(0, 2) / Y%; Z(15)
     IF G9 <> 0 THEN PRINT USING "####  ####   ##.#      ###.#    ##"; K(0, 1) - W6(0, 0); K(0, 2) + W6(0, 1); (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0)); (K(0, 2) + W6(0, 1)) / Y%; Z(15)
     PRINT "OPPONENT TOTALS"; TAB(18);
     IF G9 = 0 THEN PRINT USING "####  ####   ##.#      ###.#    ##"; K(1, 1); K(1, 2); K(1, 2) / K(1, 1); K(1, 2) / Y%; Z1(15)
     IF G9 <> 0 THEN PRINT USING "####  ####   ##.#      ###.#    ##"; K(1, 1) - W6(1, 0); K(1, 2) + W6(1, 1); (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0)); (K(1, 2) + W6(1, 1)) / Y%; Z1(15)
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     LOCATE 4, 1
     COLOR 12
     PRINT "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"
     I = 0
     FOR I1 = 0 TO 5
     IF K2(I, I1, 6) = 0 THEN 8562
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT A2$(I, I1); TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 6); K2(I, I1, 7); K2(I, I1, 7) / K2(I, I1, 6); K2(I, I1, 7) / Y%; K2(I, I1, 8);
     PRINT USING "###"; LC(I1 + 10); : PRINT LC$(I1 + 10, 0); TAB(64); LC$(I1 + 10, 1)
8562 NEXT I1
     FOR I1 = 0 TO 9
     IF K2(I, I1, 3) = 0 THEN 8578
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT A1$(I, I1); TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 3); K2(I, I1, 4); K2(I, I1, 4) / K2(I, I1, 3); K2(I, I1, 4) / Y%; K2(I, I1, 5);
     PRINT USING "###"; LC(I1); : PRINT LC$(I1, 0); TAB(64); LC$(I1, 1)
8578 NEXT I1
     PRINT
     COLOR 14
     PRINT A$(0); TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##"; K(0, 4); K(0, 6); K(0, 6) / K(0, 4); K(0, 6) / Y%; Z(17)
     PRINT "OPPONENT TOTALS"; TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##"; K(1, 4); K(1, 6); K(1, 6) / K(1, 4); K(1, 6) / Y%; Z1(17)
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     LOCATE 4, 1
     COLOR 12
     PRINT "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"
     FOR I1 = 0 TO 3
     IF K2(I, I1, 9) = 0 THEN 8588
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT A3$(I, I1); TAB(18); USING "####  ### ###.# #### ##.#"; K2(I, I1, 9); K2(I, I1, 10); K2(I, I1, 10) / K2(I, I1, 9) * 100; K2(I, I1, 12); K2(I, I1, 12) / K2(I, I1, 9);
     IF K2(I, I1, 10) = 0 THEN PRINT "   --";  ELSE PRINT USING " ##.#"; K2(I, I1, 12) / K2(I, I1, 10);
     PRINT USING " ###.# ### ### ###.#     "; K2(I, I1, 12) / Y%; K2(I, I1, 13); K2(I, I1, 11); K2(I, I1, 11) / K2(I, I1, 9) * 100;
     IF G9 = 0 THEN GOSUB 2500: PRINT USING "###.#"; RT
     IF G9 = 1 THEN GOSUB 2000: PRINT USING "###.#"; RT
     'PRINT TAB(57); USING "###"; LP(I1); : PRINT LP$(I1,0)
8588 NEXT I1
     PRINT
     COLOR 14
     PRINT A$(0); TAB(18);
     PRINT USING "####  ### ###.# #### ##.#"; K(0, 3); K(0, 4); K(0, 4) / K(0, 3) * 100; K(0, 6); K(0, 6) / K(0, 3);
     PRINT USING " ##.#"; K(0, 6) / K(0, 4);
     PRINT USING " ###.# ### ### ###.#     "; K(0, 6) / Y%; Z(17); K(0, 5); K(0, 5) / K(0, 3) * 100
     PRINT "OPPONENT TOTALS"; TAB(18);
     PRINT USING "####  ### ###.# #### ##.#"; K(1, 3); K(1, 4); K(1, 4) / K(1, 3) * 100; K(1, 6); K(1, 6) / K(1, 3);
     PRINT USING " ##.#"; K(1, 6) / K(1, 4);
     PRINT USING " ###.# ### ### ###.#     "; K(1, 6) / Y%; Z1(17); K(1, 5); K(1, 5) / K(1, 3) * 100
     PRINT
     PRINT
     COLOR 12
     PRINT "PASSING           LONG PASS    AGAINST"
     FOR I1 = 0 TO 3
     IF K2(I, I1, 9) = 0 THEN 8589
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT A3$(I, I1); TAB(22); USING "###"; LP(I1); : PRINT LP$(I1, 0); TAB(32); LP$(I1, 1)
8589 NEXT
     GOSUB 8670: GOSUB 8660: CALL TITLE
     LOCATE 3, 1
     COLOR 12
     PRINT "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
     I = 0
     FOR I1 = 0 TO 2
     IF K2(I, I1, 14) = 0 THEN 8593
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT A4$(I, I1); TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
     PRINT USING "###"; LK(I1); : PRINT LK$(I1, 0); TAB(64); LK$(I1, 1)
8593 NEXT I1
     PRINT
     COLOR 14
     PRINT A$(0); TAB(18);
     IF Z(22) =  0 THEN PRINT "   0     0   --.-        -.-     0" ELSE PRINT USING "####  ####   ##.#      ###.#    ##"; Z(22); Z(23); Z(23) / Z(22); Z(23) / Y%; Z(18)
     PRINT "OPPONENT TOTALS"; TAB(18);
     IF Z1(22) = 0 THEN PRINT "   0     0   --.-        -.-     0" ELSE PRINT USING "####  ####   ##.#      ###.#    ##"; Z1(22); Z1(23); Z1(23) / Z1(22); Z1(23) / Y%; Z1(18)
     PRINT
     PRINT
     PRINT
     COLOR 12
     PRINT "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
     I = 0
     FOR I1 = 3 TO 5
     IF K2(I, I1, 14) = 0 THEN 8610
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT A5$(I, I1 - 3); TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
     PRINT USING "###"; PK(I1 - 3); : PRINT PK$(I1 - 3, 0); TAB(64); PK$(I1 - 3, 1)
8610 NEXT I1
     PRINT
     COLOR 14
     PRINT A$(0); TAB(18);
     IF Z(24) =  0 THEN PRINT "   0     0   --.-        -.-     0" ELSE PRINT USING "####  ####   ##.#      ###.#    ##"; Z(24); Z(25); Z(25) / Z(24); Z(25) / Y%; Z(19)
     PRINT "OPPONENT TOTALS"; TAB(18);
     IF Z1(24) =  0 THEN PRINT "   0     0   --.-        -.-     0" ELSE PRINT USING "####  ####   ##.#      ###.#    ##"; Z1(24); Z1(25); Z1(25) / Z1(24); Z1(25) / Y%; Z1(19)
     GOSUB 8670: GOSUB 8660
     CALL TITLE
     I = 0
     LOCATE 3, 1
     COLOR 12
     PRINT "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
     COLOR 15
     PRINT A6$(I, 0); TAB(18); USING "####  ####    ##.#    ##.#      ##     ##      ##"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); K(0, 7) / Y%; Z(31); Z(30); Z(28)
     PRINT "OPPONENT TOTALS"; TAB(18); USING "####  ####    ##.#    ##.#      ##     ##"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); K(1, 7) / Y%; Z1(31); Z1(30)
     PRINT
     PRINT
     PRINT
     COLOR 12
     PRINT "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
     COLOR 15
     PRINT A$(0); TAB(18); USING "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); Z1(24); Z1(25); Z1(25) / Z1(24); (K(0, 8) - Z1(25)) / K(0, 7)
     PRINT "OPPONENT TOTALS"; TAB(18); USING "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); Z(24); Z(25); Z(25) / Z(24); (K(1, 8) - Z(25)) / K(1, 7)
     PRINT
     PRINT
     PRINT
     COLOR 12
     PRINT "KICKING"; TAB(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
     COLOR 15
     IF K2(I, 4, 17) = 0 AND K2(I, 6, 17) = 0 THEN 8685
     PRINT A7$(I, 0);
     PRINT TAB(16);
     IF K2(I, 6, 17) <> 0 THEN PRINT USING "### ### ###.# "; K2(I, 5, 17); K2(I, 6, 17); K2(I, 5, 17) / K2(I, 6, 17) * 100;
     IF K2(I, 4, 17) = 0 THEN PRINT : GOTO 8685
     PRINT TAB(30);
     PRINT USING "### ### ###.# ## ##"; K2(I, 3, 17); K2(I, 4, 17); K2(I, 3, 17) / K2(I, 4, 17) * 100; LF(0); AM(0, 0);
     PRINT "-"; : PRINT USING "## ##"; AF(0, 0); AM(0, 1);
     PRINT "-"; : PRINT USING "## ##"; AF(0, 1); AM(0, 2);
     PRINT "-"; : PRINT USING "## ##"; AF(0, 2); AM(0, 3);
     PRINT "-"; : PRINT USING "## ##"; AF(0, 3); AM(0, 4);
     PRINT "-"; : PRINT USING "##"; AF(0, 4)

8685 IF K2(I, 8, 17) = 0 AND K2(I, 10, 17) = 0 THEN 8594
     PRINT A7$(I, 1); TAB(16);
     IF K2(I, 10, 17) <> 0 THEN PRINT USING "### ### ###.# "; K2(I, 9, 17); K2(I, 10, 17); K2(I, 9, 17) / K2(I, 10, 17) * 100;
     IF K2(I, 8, 17) = 0 THEN PRINT : GOTO 8594
     PRINT TAB(30);
     PRINT USING "### ### ###.# ## ##"; K2(I, 7, 17); K2(I, 8, 17); K2(I, 7, 17) / K2(I, 8, 17) * 100; LF(1); AM(1, 0);
     PRINT "-"; : PRINT USING "## ##"; AF(1, 0); AM(1, 1);
     PRINT "-"; : PRINT USING "## ##"; AF(1, 1); AM(1, 2);
     PRINT "-"; : PRINT USING "## ##"; AF(1, 2); AM(1, 3);
     PRINT "-"; : PRINT USING "## ##"; AF(1, 3); AM(1, 4);
     PRINT "-"; : PRINT USING "##"; AF(1, 4)
8594 PRINT
NOONE:
     GOSUB 8670: GOSUB 8660: CALL TITLE
     LOCATE 3, 1: COLOR 12
     PRINT "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
     I = 0
     FOR I1 = 0 TO 9
     IF GI(I, I1, 0) = 0 THEN 8628
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT DI$(I, I1); TAB(18);
     PRINT USING "####  ####   ##.#      ###.#    ##    "; GI(I, I1, 0); GI(I, I1, 1); GI(I, I1, 1) / GI(I, I1, 0); GI(I, I1, 1) / Y%; GI(I, I1, 2);
     PRINT USING "###"; LI(I1); : PRINT LI$(I1, 0); TAB(64); LI$(I1, 1)
8628 NEXT I1
     GOSUB 8670: GOSUB 8660: CALL TITLE

     LOCATE 4, 1: COLOR 12
     PRINT "DEFENSIVE SACKS    SACKS      YARDS LOST"
     FOR I1 = 0 TO 14
     IF GS(I, I1, 0) = 0 THEN 8627
     DM% = DM% + 1
     COLOR 7
     IF DM% / 2 = INT(DM% / 2) THEN COLOR 15
     PRINT DS$(I, I1); TAB(20); USING "###            ###"; GS(I, I1, 0); GS(I, I1, 1)
8627 NEXT
     GOSUB 4091


8624 CLS : PRINT "DISPLAY STATISTICS AGAIN (YN)";
8629 I$ = INKEY$: IF I$ = "" THEN 8629
     IF ucase$(I$) = "Y" THEN 8530
     GOTO 8800



8655 IF X = 1 THEN X = 0
     RETURN
8660 IF X = 0 THEN CLS
     RETURN
8670 IF X = 0 THEN GOSUB 4091
     RETURN
8699 IF D% = 1 THEN 8704
     FOR I1 = 0 TO 9: FOR I2 = 0 TO 3
     IF A1$(I, I1) = A3$(I, I2) THEN K2(I, I1, 0) = K2(I, I1, 0) + K2(I, I2 + 10, 0): K2(I, I1, 1) = K2(I, I1, 1) + K2(I, I2 + 10, 1): K2(I, I1, 2) = K2(I, I1, 2) + K2(I, I2 + 10, 2): K2(I, I2 + 10, 0) = 0: K2(I, I2 + 10, 1) = 0: K2(I, I2 + 10, 2)  _
= 0
     NEXT I2: NEXT I1
8704 D% = 1: RETURN
8800 CLS : PRINT "RESET STATS TO 0 ? (YN)"; :
8801 I$ = INKEY$: IF I$ = "" THEN 8801
     IF UCASE$(I$) <> "Y" THEN 14
     PRINT : PRINT "ARE YOU SURE ? (YN) ";
8804 I$ = INKEY$: IF I$ = "" THEN 8804
     IF UCASE$(I$) = "N" THEN X = FRE(0): GOTO 14
     IF UCASE$(I$) = "Y" THEN 8812
8812 KILL D$(1) + Z$
     KILL D$(1) + Z$ + "S"
     CLS
     GOTO 14


9700 IF UCASE$(LEFT$(D$(1), 1)) = "C" THEN 9705
     CLS : PRINT "MAKE SURE CORRECT STATS DISK IS IN DRIVE"
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
9701 I$ = INKEY$: IF I$ = "" THEN 9701
9705 RETURN
9800 P%(0) = 0: P%(1) = 0: L% = 0: W% = 0: X% = 0
     IF NOT EXIST%(D$(1) + Z$) THEN 5025
     OPEN D$(1) + Z$ + "S" FOR INPUT AS #2
     INPUT #2, X$
     Y% = VAL(X$)
     FOR I = 1 TO 120: INPUT #2, O%(I): NEXT I
     FOR I = 1 TO 120: INPUT #2, O$(I): NEXT I
     FOR I = 1 TO 120: INPUT #2, T%(I): NEXT I
     FOR I = 1 TO 120: INPUT #2, H$(I): NEXT I
     FOR I = 0 TO 1
     FOR I1 = 1 TO 120: INPUT #2, AP%(I, I1): NEXT: NEXT
     FOR I = 1 TO 120: INPUT #2, CRD(I): NEXT I
     FOR I = 1 TO 120: INPUT #2, SITE$(I): NEXT I
     CLOSE #2

     'STRENGTH OF SCHEDULE
     PW = 0: SS = 0
     FOR I = 1 TO Y%
     SS = AP%(1, I) + SS
     NEXT
     SS = SS / Y%

     'INDIVIDUAL GAME POWER RATINGS
     GL = 0: PW = 0
     FOR I = 1 TO Y%
     RAT = O%(I) - T%(I) + AP%(1, I)
     IF LEFT$(H$(I), 1) = "H" THEN RAT = RAT - 3
     IF LEFT$(H$(I), 1) = "A" THEN RAT = RAT + 3
     PW = PW + RAT
     NEXT
     IF Y% > 4 THEN 9805
     GL = 5 - Y%
     PW = PW + GL * AP%(0, 1)

9805 PW = PW / (Y% + GL)
     RETURN

9806 REM:*** ADD IN CLEAR SCREEN AND TITLE BAR ***
     CLS
     COLOR 12: PRINT "GAME-BY-GAME REPORT FOR "; A$(0)
     COLOR 14
     PRINT "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"
     RETURN


9910 REM: *** PRINT STATS ***
     OPEN "REPORT" FOR OUTPUT AS #2
     PRINT #2, "GAME-BY-GAME REPORT FOR "; A$(0)
     PRINT #2,
     PRINT #2, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"
     FOR I = 1 TO Y%
     PRINT #2, O$(I); TAB(19); USING "###"; O%(I);
     PRINT #2, "-"; : PRINT #2, USING "###   "; T%(I);
     IF O%(I) > T%(I) THEN W% = W% + 1: PRINT #2, "W";
     IF O%(I) < T%(I) THEN L% = L% + 1: PRINT #2, "L";
     IF O%(I) = T%(I) THEN X% = X% + 1: PRINT #2, "T";
     PRINT #2, TAB(31); : PRINT #2, USING "####,###   "; CRD(I); : PRINT #2, H$(I); "  "; LEFT$(SITE$(I), 24);
     PRINT #2, TAB(69);
     PRINT #2, "("; : PRINT #2, USING "##"; W%; : PRINT #2, "-"; : PRINT #2, USING "##"; L%; : PRINT #2, "-"; : PRINT #2, USING "##"; X%; : PRINT #2, ")"
     P%(0) = P%(0) + O%(I): P%(1) = P%(1) + T%(I)
     NEXT I
     PRINT #2,
     IF G9 = 0 THEN PRINT #2, USING "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; PW; SS
     P%(0) = 0: P%(1) = 0: W% = 0: L% = 0: X% = 0
     PRINT #2,
     PRINT #2,
     PRINT #2, "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
     PRINT #2, A$(0); TAB(30); USING " ###  ###  ###  ###   ###"; Z(32); Z(33); Z(34); Z(35); Z(32) + Z(33) + Z(34) + Z(35) + Z(36)
     PRINT #2, "OPPONENTS"; TAB(30); USING " ###  ###  ###  ###   ###"; Z1(32); Z1(33); Z1(34); Z1(35); Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)
     PRINT #2,
     PRINT #2, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
     PRINT #2, A$(0); TAB(30); USING " ##.#  ##.#  ##.#  ##.#   ##.#"; Z(32) / Y%; Z(33) / Y%; Z(34) / Y%; Z(35) / Y%; (Z(32) + Z(33) + Z(34) + Z(35) + Z(36)) / Y%
     PRINT #2, "OPPONENTS"; TAB(30); USING " ##.#  ##.#  ##.#  ##.#   ##.#"; Z1(32) / Y%; Z1(33) / Y%; Z1(34) / Y%; Z1(35) / Y%; (Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)) / Y%
     PRINT #2,
     HD = 0: AD = 0: ND = 0: TD = 0
     HT = 0: CT = 0: NT = 0: TT = 0
     HA = 0: AA = 0: NA = 0: TA = 0
     FOR I = 1 TO Y%
     IF LEFT$(H$(I), 1) = "H" THEN HD = HD + 1: HT = HT + CRD(I)
     IF LEFT$(H$(I), 1) = "A" THEN AD = AD + 1: CT = CT + CRD(I)
     IF LEFT$(H$(I), 1) = "X" THEN ND = ND + 1: NT = NT + CRD(I)
     TD = TD + 1: TT = TT + CRD(I)
     NEXT
     IF HD <> 0 THEN HA = HT / HD
     IF AD <> 0 THEN AA = CT / AD
     IF ND <> 0 THEN NA = NT / ND
     IF TD <> 0 THEN TA = TT / TD
     PRINT #2, "ATTENDANCE    DATES      TOTAL    AVERAGE"
     PRINT #2, USING "HOME            ##    #####,###   ###,###"; HD; HT; HA
     PRINT #2, USING "AWAY            ##    #####,###   ###,###"; AD; CT; AA
     PRINT #2, USING "NEUTRAL         ##    #####,###   ###,###"; ND; NT; NA
     PRINT #2, USING "TOTAL           ##    #####,###   ###,###"; TD; TT; TA
     PRINT #2, CHR$(12)
     PRINT #2, "TEAM STATISTICS CATEGORY"; TAB(41); A$(0); TAB(61); "OPPONENT TOTALS"
     PRINT #2, "FIRST DOWNS"; TAB(45); USING "####"; K(0, 0); : PRINT #2, TAB(65); USING "####"; K(1, 0)
     PRINT #2, "RUSHING ATTEMPTS"; TAB(45);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(0, 1);  ELSE PRINT #2, USING "####"; K(0, 1) - W6(0, 0);
     PRINT #2, TAB(65);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(1, 1) ELSE PRINT #2, USING "####"; K(1, 1) - W6(1, 0)
     PRINT #2, "NET RUSHING YARDAGE"; TAB(45);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(0, 2);  ELSE PRINT #2, USING "####"; K(0, 2) + W6(0, 1);
     PRINT #2, TAB(65);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(1, 2) ELSE PRINT #2, USING "####"; K(1, 2) + W6(1, 1)
     PRINT #2, "YARDS PER RUSH"; TAB(45);
     IF G9 = 0 AND K(0, 1) <> 0 THEN PRINT #2, USING "##.#"; K(0, 2) / K(0, 1);
     IF G9 <> 0 AND K(0, 1) - W6(0, 0) <> 0 THEN PRINT #2, USING "##.#"; (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0));
     PRINT #2, TAB(65);
     IF G9 = 0 AND K(1, 1) <> 0 THEN PRINT #2, USING "##.#"; K(1, 2) / K(1, 1)
     IF G9 <> 0 AND K(1, 1) - W6(1, 0) <> 0 THEN PRINT #2, USING "##.#"; (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0))
     IF G9 = 0 AND K(1, 1) = 0 OR G9 <> 0 AND K(1, 1) - W6(1, 0) = 0 THEN PRINT #2,
     PRINT #2, "RUSHING YARDS PER GAME"; TAB(44);
     IF G9 = 0 THEN PRINT #2, USING "###.#"; K(0, 2) / Y%;  ELSE PRINT #2, USING "###.#"; (K(0, 2) + W6(0, 1)) / Y%;
     PRINT #2, TAB(64);
     IF G9 = 0 THEN PRINT #2, USING "###.#"; K(1, 2) / Y% ELSE PRINT #2, USING "###.#"; (K(1, 2) + W6(1, 1)) / Y%
     PRINT #2,
     PRINT #2, "PASSES ATTEMPTED"; TAB(45); USING "####"; K(0, 3); : PRINT #2, TAB(65); USING "####"; K(1, 3)
     PRINT #2, "PASSES COMPLETED"; TAB(45); USING "####"; K(0, 4); : PRINT #2, TAB(65); USING "####"; K(1, 4)
     PRINT #2, "PASSES HAD INTERCEPTED"; TAB(45); USING "####"; K(0, 5); : PRINT #2, TAB(65); USING "####"; K(1, 5)
     PRINT #2, "PASS COMPLETION PERCENTAGE"; TAB(44);
     IF K(0, 3) <> 0 THEN PRINT #2, USING "###.#"; K(0, 4) / K(0, 3) * 100;
     IF K(1, 3) <> 0 THEN PRINT #2, TAB(64); USING "###.#"; K(1, 4) / K(1, 3) * 100
     IF K(1, 3) = 0 THEN PRINT #2,
     PRINT #2, "NET YARDS PASSING"; TAB(45); USING "####"; K(0, 6); : PRINT #2, TAB(65); USING "####"; K(1, 6)
     PRINT #2, "YARDS PER PASS ATTEMPT"; TAB(45);
     IF K(0, 3) <> 0 THEN PRINT #2, USING "##.#"; K(0, 6) / K(0, 3);
     IF K(1, 3) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; K(1, 6) / K(1, 3)
     IF K(1, 3) = 0 THEN PRINT #2,
     PRINT #2, "YARDS PER PASS COMPLETION"; TAB(45);
     IF K(0, 4) <> 0 THEN PRINT #2, USING "##.#"; K(0, 6) / K(0, 4);
     IF K(1, 4) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; K(1, 6) / K(1, 4)
     IF K(1, 4) = 0 THEN PRINT #2,
     PRINT #2, "PASSING YARDS PER GAME"; TAB(44); USING "###.#"; K(0, 6) / Y%; : PRINT #2, TAB(64); USING "###.#"; K(1, 6) / Y%
     PRINT #2,

     PRINT #2, "TOTAL PLAYS"; TAB(45);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(0, 1) + K(0, 3);  ELSE PRINT #2, USING "####"; K(0, 1) - W6(0, 0) + K(0, 3);
     PRINT #2, TAB(65);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(1, 1) + K(1, 3) ELSE PRINT #2, USING "####"; K(1, 1) - W6(1, 0) + K(1, 3)
     PRINT #2, "TOTAL PLAYS PER GAME"; TAB(44);
     IF G9 = 0 THEN PRINT #2, USING "###.#"; (K(0, 1) + K(0, 3)) / Y%;  ELSE PRINT #2, USING "###.#"; (K(0, 1) - W6(0, 0) + K(0, 3)) / Y%;
     PRINT #2, TAB(64);
     IF G9 = 0 THEN PRINT #2, USING "###.#"; (K(1, 1) + K(1, 3)) / Y% ELSE PRINT #2, USING "###.#"; (K(1, 1) - W6(1, 0) + K(1, 3)) / Y%
     PRINT #2, "TOTAL NET YARDS"; TAB(45);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(0, 2) + K(0, 6);  ELSE PRINT #2, USING "####"; K(0, 2) + W6(0, 1) + K(0, 6);
     PRINT #2, TAB(65);
     IF G9 = 0 THEN PRINT #2, USING "####"; K(1, 2) + K(1, 6) ELSE PRINT #2, USING "####"; K(1, 2) + W6(1, 1) + K(1, 6)
     PRINT #2, "YARDS GAINED PER PLAY"; TAB(45);
     IF G9 = 0 THEN PRINT #2, USING "##.#"; (K(0, 2) + K(0, 6)) / (K(0, 1) + K(0, 3));  ELSE PRINT #2, USING "##.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / (K(0, 1) - W6(0, 0) + K(0, 3));
     PRINT #2, TAB(64);
     IF G9 = 0 THEN PRINT #2, USING "##.#"; (K(1, 2) + K(1, 6)) / (K(1, 1) + K(1, 3)) ELSE PRINT #2, USING "##.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / (K(1, 1) - W6(1, 0) + K(1, 3))
     PRINT #2, "YARDS GAINED PER GAME"; TAB(44);
     IF G9 = 0 THEN PRINT #2, USING "###.#"; (K(0, 2) + K(0, 6)) / Y%;  ELSE PRINT #2, USING "###.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / Y%;
     PRINT #2, TAB(64);
     IF G9 = 0 THEN PRINT #2, USING "###.#"; (K(1, 2) + K(1, 6)) / Y% ELSE PRINT #2, USING "###.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / Y%
     PRINT #2,
     PRINT #2, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; TAB(41);
     PRINT #2, USING "###"; Z(22); : PRINT #2, "/"; : PRINT #2, USING "####"; Z(23);
     PRINT #2, TAB(61); USING "###"; Z1(22); : PRINT #2, "/"; : PRINT #2, USING "####"; Z1(23)
     PRINT #2, "AVERAGE YARDAGE PER KICKOFF RETURN"; TAB(45);
     IF Z(22) <> 0 THEN PRINT #2, USING "##.#"; Z(23) / Z(22);
     IF Z1(22) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; Z1(23) / Z1(22)
     IF Z1(22) = 0 THEN PRINT #2,
     PRINT #2, "KICKOFF RETURNS PER GAME"; TAB(45); : PRINT #2, USING "##.#"; Z(22) / Y%; : PRINT #2, TAB(65); USING "##.#"; Z1(22) / Y%
     PRINT #2, "PUNT RETURNS / PUNT RETURN YARDS"; TAB(41);
     PRINT #2, USING "###"; Z(24); : PRINT #2, "/"; : PRINT #2, USING "####"; Z(25);
     PRINT #2, TAB(61); USING "###"; Z1(24); : PRINT #2, "/"; : PRINT #2, USING "####"; Z1(25)
     PRINT #2, "AVERAGE YARDAGE PER PUNT RETURN"; TAB(45);
     IF Z(24) <> 0 THEN PRINT #2, USING "##.#"; Z(25) / Z(24);
     IF Z1(24) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; Z1(25) / Z1(24)
     IF Z1(24) = 0 THEN PRINT #2,
     PRINT #2, "PUNT RETURNS PER GAME"; TAB(45); : PRINT #2, USING "##.#"; Z(24) / Y%; : PRINT #2, TAB(65); USING "##.#"; Z1(24) / Y%
     PRINT #2, "INTERCEPTION RETURNS / RETURN YARDS"; TAB(41);
     PRINT #2, USING "###"; Z(26); : PRINT #2, "/"; : PRINT #2, USING "####"; Z(27);
     PRINT #2, TAB(61); USING "###"; Z1(26); : PRINT #2, "/"; : PRINT #2, USING "####"; Z1(27)
     PRINT #2, "AVERAGE YARDAGE PER RETURN"; TAB(45);
     IF Z(26) <> 0 THEN PRINT #2, USING "##.#"; Z(27) / Z(26);
     IF Z1(26) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; Z1(27) / Z1(26)
     IF Z1(26) = 0 THEN PRINT #2,
     PRINT #2, "INTERCEPTION RETURNS PER GAME"; TAB(45); : PRINT #2, USING "##.#"; Z(26) / Y%; : PRINT #2, TAB(65); USING "##.#"; Z1(26) / Y%
     PRINT #2,
     PRINT #2, "PUNTS / TOTAL PUNT YARDAGE"; TAB(41);
     PRINT #2, USING "###"; K(0, 7); : PRINT #2, "/"; : PRINT #2, USING "####"; K(0, 8);
     PRINT #2, TAB(61); USING "###"; K(1, 7); : PRINT #2, "/"; : PRINT #2, USING "####"; K(1, 8)
     PRINT #2, "AVERAGE YARDS PER PUNT"; TAB(45);
     IF K(0, 7) <> 0 THEN PRINT #2, USING "##.#"; K(0, 8) / K(0, 7);
     IF K(1, 7) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; K(1, 8) / K(1, 7)
     IF K(1, 7) = 0 THEN PRINT #2,
     PRINT #2, "AVERAGE NUMBER OF PUNTS PER GAME"; TAB(45); : PRINT #2, USING "##.#"; K(0, 7) / Y%; : PRINT #2, TAB(65); USING "##.#"; K(1, 7) / Y%
     PRINT #2,
     PRINT #2, "FUMBLES / FUMBLES LOST"; TAB(41);
     PRINT #2, USING "###"; K(0, 9); : PRINT #2, "/"; : PRINT #2, USING "####"; K(0, 10);
     PRINT #2, TAB(61); USING "###"; K(1, 9); : PRINT #2, "/"; : PRINT #2, USING "####"; K(1, 10)
     PRINT #2, "PENALTIES / YARDS PENALIZED"; TAB(41);
     PRINT #2, USING "###"; K(0, 11); : PRINT #2, "/"; : PRINT #2, USING "####"; K(0, 12);
     PRINT #2, TAB(61); USING "###"; K(1, 11); : PRINT #2, "/"; : PRINT #2, USING "####"; K(1, 12)
     PRINT #2, "AVERAGE YARDS PER PENALTY"; TAB(45);
     IF K(0, 11) <> 0 THEN PRINT #2, USING "##.#"; K(0, 12) / K(0, 11);
     IF K(1, 11) <> 0 THEN PRINT #2, TAB(65); USING "##.#"; K(1, 12) / K(1, 11)
     IF K(1, 11) = 0 THEN PRINT #2,
     PRINT #2, "PENALTIES PER GAME / YARDS PER GAME"; TAB(40);
     PRINT #2, USING "##.#"; K(0, 11) / Y%; : PRINT #2, "/"; : PRINT #2, USING "##.#"; K(0, 12) / Y%;
     PRINT #2, TAB(60); USING "##.#"; K(1, 11) / Y%; : PRINT #2, "/"; : PRINT #2, USING "##.#"; K(1, 12) / Y%
     PRINT #2, "SAFETIES"; TAB(45); : PRINT #2, USING "####"; Z(21); : PRINT #2, TAB(65); USING "####"; Z1(21)
     PRINT #2, "TIMES SACKED / YARDS LOST"; TAB(41);
     PRINT #2, USING "###"; W6(0, 0); : PRINT #2, "/"; : PRINT #2, USING "####"; W6(0, 1);
     PRINT #2, TAB(61); USING "###"; W6(1, 0); : PRINT #2, "/"; : PRINT #2, USING "####"; W6(1, 1)
     PRINT #2, CHR$(12)
     PRINT #2, "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"
     I = 0
     GOSUB 8699
     FOR I1 = 0 TO 13
     IF K2(I, I1, 0) = 0 THEN 5560
     IF I1 <= 9 THEN PRINT #2, A1$(I, I1);
     IF I1 > 9 THEN PRINT #2, A3$(I, I1 - 10);
     PRINT #2, TAB(18); USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 0); K2(I, I1, 1); K2(I, I1, 1) / K2(I, I1, 0); K2(I, I1, 1) / Y%; K2(I, I1, 2);
     IF I1 > 9 THEN PRINT #2, : GOTO 5560
     PRINT #2, USING "###"; LR(I1); : PRINT #2, LR$(I1, 0); TAB(64); LR$(I1, 1)
5560 NEXT I1
     PRINT #2, A$(0); TAB(18);
     IF G9 = 0 THEN PRINT #2, USING "####  ####   ##.#      ###.#    ##"; K(0, 1); K(0, 2); K(0, 2) / K(0, 1); K(0, 2) / Y%; Z(15)
     IF G9 <> 0 THEN PRINT #2, USING "####  ####   ##.#      ###.#    ##"; K(0, 1) - W6(0, 0); K(0, 2) + W6(0, 1); (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0)); (K(0, 2) + W6(0, 1)) / Y%; Z(15)
     PRINT #2, "OPPONENT TOTALS"; TAB(18);
     IF G9 = 0 THEN PRINT #2, USING "####  ####   ##.#      ###.#    ##"; K(1, 1); K(1, 2); K(1, 2) / K(1, 1); K(1, 2) / Y%; Z1(15)
     IF G9 <> 0 THEN PRINT #2, USING "####  ####   ##.#      ###.#    ##"; K(1, 1) - W6(1, 0); K(1, 2) + W6(1, 1); (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0)); (K(1, 2) + W6(1, 1)) / Y%; Z1(15)
     PRINT #2,
     PRINT #2, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"
     I = 0
     FOR I1 = 0 TO 5
     IF K2(I, I1, 6) = 0 THEN 5562
     PRINT #2, A2$(I, I1); TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 6); K2(I, I1, 7); K2(I, I1, 7) / K2(I, I1, 6); K2(I, I1, 7) / Y%; K2(I, I1, 8);
     PRINT #2, USING "###"; LC(I1 + 10); : PRINT #2, LC$(I1 + 10, 0); TAB(64); LC$(I1 + 10, 1)
5562 NEXT I1
     FOR I1 = 0 TO 9
     IF K2(I, I1, 3) = 0 THEN 5578
     PRINT #2, A1$(I, I1); TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 3); K2(I, I1, 4); K2(I, I1, 4) / K2(I, I1, 3); K2(I, I1, 4) / Y%; K2(I, I1, 5);
     PRINT #2, USING "###"; LC(I1); : PRINT #2, LC$(I1, 0); TAB(64); LC$(I1, 1)
5578 NEXT I1
     PRINT #2, A$(0); TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##"; K(0, 4); K(0, 6); K(0, 6) / K(0, 4); K(0, 6) / Y%; Z(17)
     PRINT #2, "OPPONENT TOTALS"; TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##"; K(1, 4); K(1, 6); K(1, 6) / K(1, 4); K(1, 6) / Y%; Z1(17)
     PRINT #2,
     PRINT #2, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"
     FOR I1 = 0 TO 3
     IF K2(I, I1, 9) = 0 THEN 5588
     PRINT #2, A3$(I, I1); TAB(18); USING "####  ### ###.# #### ##.#"; K2(I, I1, 9); K2(I, I1, 10); K2(I, I1, 10) / K2(I, I1, 9) * 100; K2(I, I1, 12); K2(I, I1, 12) / K2(I, I1, 9);
     IF K2(I, I1, 10) = 0 THEN PRINT #2, "   --";  ELSE PRINT #2, USING " ##.#"; K2(I, I1, 12) / K2(I, I1, 10);
     PRINT #2, USING " ###.# ### ### ###.#     "; K2(I, I1, 12) / Y%; K2(I, I1, 13); K2(I, I1, 11); K2(I, I1, 11) / K2(I, I1, 9) * 100;
     IF G9 = 0 THEN GOSUB 2500: PRINT #2, USING "###.#"; RT
     IF G9 = 1 THEN GOSUB 2000: PRINT #2, USING "###.#"; RT
5588 NEXT I1
     PRINT #2, A$(0); TAB(18);
     PRINT #2, USING "####  ### ###.# #### ##.#"; K(0, 3); K(0, 4); K(0, 4) / K(0, 3) * 100; K(0, 6); K(0, 6) / K(0, 3);
     PRINT #2, USING " ##.#"; K(0, 6) / K(0, 4);
     PRINT #2, USING " ###.# ### ### ###.#     "; K(0, 6) / Y%; Z(17); K(0, 5); K(0, 5) / K(0, 3) * 100
     PRINT #2, "OPPONENT TOTALS"; TAB(18);
     PRINT #2, USING "####  ### ###.# #### ##.#"; K(1, 3); K(1, 4); K(1, 4) / K(1, 3) * 100; K(1, 6); K(1, 6) / K(1, 3);
     PRINT #2, USING " ##.#"; K(1, 6) / K(1, 4);
     PRINT #2, USING " ###.# ### ### ###.#     "; K(1, 6) / Y%; Z1(17); K(1, 5); K(1, 5) / K(1, 3) * 100
     PRINT #2,
     PRINT #2,
     PRINT #2, "PASSING           LONG PASS    AGAINST"
     FOR I1 = 0 TO 3
     IF K2(I, I1, 9) = 0 THEN 5589
     PRINT #2, A3$(I, I1); TAB(22); USING "###"; LP(I1); : PRINT #2, LP$(I1, 0); TAB(32); LP$(I1, 1)
5589 NEXT
     PRINT #2, CHR$(12)
     PRINT #2, "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
     I = 0
     FOR I1 = 0 TO 2
     IF K2(I, I1, 14) = 0 THEN 5593
     PRINT #2, A4$(I, I1); TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
     PRINT #2, USING "###"; LK(I1); : PRINT #2, LK$(I1, 0); TAB(64); LK$(I1, 1)
5593 NEXT I1
     PRINT #2, A$(0); TAB(18);
     IF Z(22) =  0 THEN PRINT #2, "   0     0   --.-        -.-     0" ELSE PRINT #2, USING "####  ####   ##.#      ###.#    ##"; Z(22); Z(23); Z(23) / Z(22); Z(23) / Y%; Z(18)
     PRINT #2, "OPPONENT TOTALS"; TAB(18);
     IF Z1(22) = 0 THEN PRINT #2, "   0     0   --.-        -.-     0" ELSE PRINT #2, USING "####  ####   ##.#      ###.#    ##"; Z1(22); Z1(23); Z1(23) / Z1(22); Z1(23) / Y%; Z1(18)
     PRINT #2,
     PRINT #2,
     PRINT #2,
     PRINT #2, "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
     I = 0
     FOR I1 = 3 TO 5
     IF K2(I, I1, 14) = 0 THEN 5610
     PRINT #2, A5$(I, I1 - 3); TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
     PRINT #2, USING "###"; PK(I1 - 3); : PRINT #2, PK$(I1 - 3, 0); TAB(64); PK$(I1 - 3, 1)
5610 NEXT I1
     PRINT #2, A$(0); TAB(18);
     IF Z(24) =  0 THEN PRINT #2, "   0     0   --.-        -.-     0" ELSE PRINT #2, USING "####  ####   ##.#      ###.#    ##"; Z(24); Z(25); Z(25) / Z(24); Z(25) / Y%; Z(19)
     PRINT #2, "OPPONENT TOTALS"; TAB(18);
     IF Z1(24) =  0 THEN PRINT #2, "   0     0   --.-        -.-     0" ELSE PRINT #2, USING "####  ####   ##.#      ###.#    ##"; Z1(24); Z1(25); Z1(25) / Z1(24); Z1(25) / Y%; Z1(19)
     PRINT #2,
     I = 0
     PRINT #2, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
     PRINT #2, A6$(I, 0); TAB(18); USING "####  ####    ##.#    ##.#      ##     ##      ##"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); K(0, 7) / Y%; Z(31); Z(30); Z(28)
     PRINT #2, "OPPONENT TOTALS"; TAB(18); USING "####  ####    ##.#    ##.#      ##     ##"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); K(1, 7) / Y%; Z1(31); Z1(30)
     PRINT #2,
     PRINT #2,
     PRINT #2,
     PRINT #2, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
     PRINT #2, A$(0); TAB(18); USING "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); Z1(24); Z1(25); Z1(25) / Z1(24); (K(0, 8) - Z1(25)) / K(0, 7)
     PRINT #2, "OPPONENT TOTALS"; TAB(18); USING "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); Z(24); Z(25); Z(25) / Z(24); (K(1, 8) - Z(25)) / K(1, 7)
     PRINT #2,
     PRINT #2,
     PRINT #2,
     PRINT #2, "KICKING"; TAB(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
     IF K2(I, 4, 17) = 0 AND K2(I, 6, 17) = 0 THEN 5685
     PRINT #2, A7$(I, 0);
     PRINT #2, TAB(16);
     IF K2(I, 6, 17) <> 0 THEN PRINT #2, USING "### ### ###.# "; K2(I, 5, 17); K2(I, 6, 17); K2(I, 5, 17) / K2(I, 6, 17) * 100;
     IF K2(I, 4, 17) = 0 THEN PRINT #2, : GOTO 5685
     PRINT #2, TAB(30);
     PRINT #2, USING "### ### ###.# ## ##"; K2(I, 3, 17); K2(I, 4, 17); K2(I, 3, 17) / K2(I, 4, 17) * 100; LF(0); AM(0, 0);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(0, 0); AM(0, 1);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(0, 1); AM(0, 2);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(0, 2); AM(0, 3);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(0, 3); AM(0, 4);
     PRINT #2, "-"; : PRINT #2, USING "##"; AF(0, 4)

5685 IF K2(I, 8, 17) = 0 AND K2(I, 10, 17) = 0 THEN 5594
     PRINT #2, A7$(I, 1); TAB(16);
     IF K2(I, 10, 17) <> 0 THEN PRINT #2, USING "### ### ###.# "; K2(I, 9, 17); K2(I, 10, 17); K2(I, 9, 17) / K2(I, 10, 17) * 100;
     IF K2(I, 8, 17) = 0 THEN PRINT #2, : GOTO 5594
     PRINT #2, TAB(30);
     PRINT #2, USING "### ### ###.# ## ##"; K2(I, 7, 17); K2(I, 8, 17); K2(I, 7, 17) / K2(I, 8, 17) * 100; LF(1); AM(1, 0);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(1, 0); AM(1, 1);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(1, 1); AM(1, 2);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(1, 2); AM(1, 3);
     PRINT #2, "-"; : PRINT #2, USING "## ##"; AF(1, 3); AM(1, 4);
     PRINT #2, "-"; : PRINT #2, USING "##"; AF(1, 4)
5594 PRINT #2,
     PRINT #2,
     PRINT #2, "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
     I = 0
     FOR I1 = 0 TO 9
     IF GI(I, I1, 0) = 0 THEN 5628
     PRINT #2, DI$(I, I1); TAB(18);
     PRINT #2, USING "####  ####   ##.#      ###.#    ##    "; GI(I, I1, 0); GI(I, I1, 1); GI(I, I1, 1) / GI(I, I1, 0); GI(I, I1, 1) / Y%; GI(I, I1, 2);
     PRINT #2, USING "###"; LI(I1); : PRINT #2, LI$(I1, 0); TAB(64); LI$(I1, 1)
5628 NEXT I1
     PRINT #2,
     PRINT #2, "DEFENSIVE SACKS    SACKS      YARDS LOST"
     FOR I1 = 0 TO 14
     IF GS(I, I1, 0) = 0 THEN 5627
     PRINT #2, DS$(I, I1); TAB(20); USING "###            ###"; GS(I, I1, 0); GS(I, I1, 1)
5627 NEXT
     CLOSE 2
     OPEN "REPORT" FOR INPUT AS #2
     DO WHILE NOT EOF(2)
         LINE INPUT #2, X$
         LPRINT X$
     LOOP
     CLOSE 2
     LPRINT CHR$(12)
     GOTO 8624

3056 COLOR 15, 0: CLS
   JJ = 6
   SN = 1: LOCATE 6, 8: COLOR 15, 4
   PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
   PRINT " º"; STRING$(61, 32); "º "; : NEXT
   LOCATE 7 + JJ, 8
   PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
   LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
   LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
   LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
   PRINT
   LOCATE , 12: PRINT "ESC TO RETURN TO MAIN MENU"
3057 I$ = INKEY$: IF I$ = "" THEN 3057
   IF I$ = CHR$(27) THEN Z$ = "END": GOTO 90
   IF I$ = CHR$(13) THEN GOSUB 1102: GOTO 3060
   LOCATE 7, 27: PRINT I$
   LOCATE 7, 28: LINE INPUT NY$
   NY$ = I$ + NY$
   COLOR 15, 0
   GOSUB 4105
   IF NY$ = "" THEN GOSUB 1102: GOTO 3060
   YN$ = NY$
   FOR I = 1 TO 500: VV$(I) = "": NEXT
   GOSUB 1100
3060 IF BO% = 1 THEN BO% = 0: GOTO 3056
3058 LOCATE 2, 33: COLOR PA%(2), PA%(1): PRINT A$
     COLOR 15, 4: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     I$ = INKEY$: IF I$ = "" THEN 3058
   IF UCASE$(I$) = "Y" THEN 3056
   COLOR 15, 0
     RETURN
1100 NTMS = 0
     OPEN D$(0) + "FOOT." + YN$ FOR RANDOM AS #1 LEN = 934
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 934
     GET #1, I1
     FOR I2 = 1 TO 15
     IF ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1105
     NEXT I2
     NTMS = NTMS + 1
     VV$(NTMS) = Q$(0): V%(NTMS) = I1
1105 NEXT I1
    CLOSE 1
1102 SELECTION% = 1
     Count% = NTMS
     Start% = 1
     FileColr% = 79
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
     COLOR , 4: CLS : LOCATE , 33: COLOR 14
     PRINT "TEAM SELECTION": COLOR 15
     LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
     PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
     PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
     CALL ShowCursor
20   MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GOTO 20
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NTMS THEN Start% = NTMS
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
     T1 = V%(SELECTION%)
     OPEN D$(0) + "FOOT." + YN$ FOR RANDOM AS #1 LEN = 934
     FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
     GET #1, T1
     A$ = Q$(1)
     FOR I1 = 0 TO 4: PA%(I1) = CVI(Q$(I1 + 11)): NEXT I1
     CLOSE 1
9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
     RETURN
4105 IF UCASE$(LEFT$(D$(0), 1)) = "C" THEN RETURN
     CLS : PRINT "MAKE SURE CORRECT TEAMS DISK IS IN DRIVE "; D$(0)
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
     GOSUB 9701
     RETURN
6585 FOR XX = 4 TO 11: IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
     NEXT XX
     FOR XX = 12 TO 14: IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
     NEXT XX
     QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
     GOTO 6620
6615 NEXT XX
6620 RETURN
2000 REM:*** NFL QB RATING ***
     QB(I1, 8) = INT(K2(I, I1, 13) / K2(I, I1, 9) * 1000) / 10: QB(I1, 6) = K2(I, I1, 10) / K2(I, I1, 9) * 100
     QB(I1, 10) = INT(K2(I, I1, 12) / K2(I, I1, 9) * 100) / 100
     QB(I1, 9) = INT(K2(I, I1, 11) / K2(I, I1, 9) * 1000) / 10
     PP(I) = QB(I1, 6) - 30
     IF PP(I) < 0 THEN PP(I) = 0: GOTO 2009
     IF PP(I) > 77.5 THEN PP(I) = 2.375: GOTO 2009
     PP(I) = PP(I) * .05
2009 IF QB(I1, 8) > 11.9 THEN TT(I) = 2.375: GOTO 2011
     TT(I) = QB(I1, 8) * .2
2011 IF QB(I1, 9) = 0 THEN II(I) = 2.375: GOTO 2014
     IF QB(I1, 9) > 9.5 THEN II(I) = 0: GOTO 2014
     II(I) = 2.375 - (.25 * QB(I1, 9))
2014 QB(I1, 10) = QB(I1, 10) - 3
     IF QB(I1, 10) < 0 THEN YY(I) = 0: GOTO 2020
     IF QB(I1, 10) > 12.5 THEN YY(I) = 2.375: GOTO 2020
     D = INT(QB(I1, 10) * 100) / 2
     IF D = QB(I1, 10) / 2 THEN YY(I) = QB(I1, 10) / 2 * .3 + QB(I1, 10) / 2 * .2: GOTO 2020
     YY(I) = QB(I1, 10) / 2 * .3 + QB(I1, 10) / 2 * .2 + .003
2020 QB(I1, 7) = PP(I) + TT(I) + II(I) + YY(I)
     IF QB(I1, 7) >= 9.5 THEN QB(I1, 7) = 158.3: GOTO 2027
     QB(I1, 7) = QB(I1, 7) * 50 / 3
2027 QB(I1, 7) = INT(QB(I1, 7) * 10) / 10: RT = QB(I1, 7)
     RETURN
2075 REM:
     LOCATE 8, 8: COLOR 15, 1: PRINT " Ö"; STRING$(61, 196); "· ";
     FOR II = 1 TO J: LOCATE 8 + II, 8: PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 9 + J, 8: PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 10, 15: RETURN

2500 REM:*** COLLEGE QB RATING ***
     QB(I1, 7) = INT(K2(I, I1, 12) / K2(I, I1, 9)) * 8.399999 + K2(I, I1, 10) / K2(I, I1, 9) * 100 + INT(K2(I, I1, 13) / K2(I, I1, 9) * 1000) / 10 * 3.3 - INT(K2(I, I1, 11) / K2(I, I1, 9) * 1000) / 10 * 2
     QB(I1, 7) = INT(QB(I1, 7) * 10) / 10: RT = QB(I1, 7)
     RETURN

SUB TITLE ()
     COLOR , 2
     LOCATE 1, 1: PRINT TAB(80); : LOCATE , 26: COLOR 0
     PRINT "3 IN 1 FOOTBALL STATISTICS"
     LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
     PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1991"; TAB(70); LEFT$(TIME$, 5);
     LOCATE 4, 1: COLOR 2, 0
END SUB

