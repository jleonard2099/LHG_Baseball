DECLARE SUB TITLE ()
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST% (FILENAME$)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)

   DIM O%(100), O$(100), T%(100), P%(100), Q$(500), VV$(500)
   DIM Z2(14, 14), Z(15), Z1(15), VV%(500)
   DIM A$(14), D$(1), L%(4)
   DIM A(14, 24)
   DIM H$(100), CP$(25)
   DIM W0(14), W1(14), AP%(100, 1), CRD(100), SITE$(100)
   CP% = 1
   DATA NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
   DATA NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
   FOR I = 0 TO 25: READ CP$(I): NEXT
   OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): INPUT #1, D$(1): CLOSE 1
45 COLOR 15, 0: CLS
   C% = GetDrive%
   CALL TITLE
   GOSUB 3056
   GOSUB 6585
   Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
90 IF UCASE$(Z$) = "END" AND C% >= 67 THEN 91
   IF UCASE$(Z$) = "END" THEN
      CLS
      PRINT "MAKE SURE GAME DISK IS IN DRIVE"
      GOSUB 895
   END IF
91 IF UCASE$(Z$) = "END" THEN RUN "HELLO.EXE 4"
   REM:*** READ IN TEAM STATS ***
   IF NOT EXIST%(D$(1) + Z$) THEN 5025
   PX = 0
   P%(0) = 0: P%(1) = 0: L% = 0: W% = 0: X% = 0
    HW% = 0: VW% = 0: NW% = 0: HL% = 0: VL% = 0: NL% = 0
    OPEN D$(1) + Z$ + "S" FOR INPUT AS #2
    INPUT #2, Y%
    FOR I = 1 TO 100: INPUT #2, O%(I): NEXT I
    FOR I = 1 TO 100: INPUT #2, O$(I): NEXT I
    FOR I = 1 TO 100: INPUT #2, T%(I): NEXT I
    FOR I = 1 TO 100: INPUT #2, H$(I): NEXT I
    FOR I = 1 TO 100
    FOR I1 = 0 TO 1: INPUT #2, AP%(I, I1): NEXT: NEXT
    FOR I = 1 TO 100: INPUT #2, CRD(I): NEXT I
    FOR I = 1 TO 100: INPUT #2, SITE$(I): NEXT I
    CLOSE #2
    FOR I = 1 TO Y%
    IF O%(I) > T%(I) THEN W% = W% + 1
    IF O%(I) > T%(I) AND LEFT$(H$(I), 1) = "H" THEN HW% = HW% + 1
    IF O%(I) > T%(I) AND LEFT$(H$(I), 1) = "V" THEN VW% = VW% + 1
    IF O%(I) > T%(I) AND LEFT$(H$(I), 1) = "N" THEN NW% = NW% + 1
    IF O%(I) < T%(I) THEN L% = L% + 1
    IF O%(I) < T%(I) AND LEFT$(H$(I), 1) = "H" THEN HL% = HL% + 1
    IF O%(I) < T%(I) AND LEFT$(H$(I), 1) = "V" THEN VL% = VL% + 1
    IF O%(I) < T%(I) AND LEFT$(H$(I), 1) = "N" THEN NL% = NL% + 1
    IF O%(I) = T%(I) THEN X% = X% + 1
    P%(0) = P%(0) + O%(I): P%(1) = P%(1) + T%(I)
    NEXT
   OPEN D$(1) + Z$ FOR INPUT AS #1
   INPUT #1, B$
   FOR I = 0 TO 13: INPUT #1, A$(I)
   FOR J = 0 TO 13: INPUT #1, A(I, J): NEXT: INPUT #1, W0(I): INPUT #1, W1(I): NEXT
    FOR L = 0 TO 15: INPUT #1, Z(L): NEXT
    FOR L = 0 TO 15: INPUT #1, Z1(L): NEXT
    CLOSE #1
    FOR I = 0 TO 13
    IF A(I, 0) <> 0 THEN A(I, 14) = A(I, 10) / A(I, 0): A(I, 20) = (A(I, 2) * 2 + A(I, 4) + W0(I)) / A(I, 0): A(I, 21) = A(I, 5) / A(I, 0)
    A(I, 22) = A(I, 2) * 2 + A(I, 4) + W0(I)
    A(I, 23) = A(I, 5) - A(I, 12)
    IF A(I, 1) <> 0 THEN A(I, 15) = A(I, 2) / A(I, 1)
    A(I, 18) = W0(I): A(I, 19) = W1(I)
    IF A(I, 19) <> 0 THEN A(I, 16) = A(I, 18) / A(I, 19)
    IF A(I, 3) <> 0 THEN A(I, 17) = A(I, 4) / A(I, 3)
    NEXT
    GOSUB 815

125 I$ = INKEY$: IF I$ = "" THEN 125
    RETURN

142 CALL TITLE
    COLOR , 1
    LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
    COLOR 15, 1: PRINT "SCHEDULE ";
    COLOR 14: PRINT "[Pg UP]  ";
    COLOR 15: PRINT "MORE STATS ";
    COLOR 14: PRINT "[Pg DN]  ";
    COLOR 15: PRINT "NEW TEAM ";
    COLOR 14: PRINT "[ESC]  ";
    COLOR 15: PRINT "SORT ";
    COLOR 14: PRINT "<-,-> and [ENTER] ";
    LOCATE 25, 1
    COLOR 15: PRINT "PRINT "; : COLOR 14: PRINT "[P]  ";
    COLOR 15: PRINT "DELETE STATS "; : COLOR 14: PRINT "[D]                    ";
    COLOR 12, 0: LOCATE 2, 1: PRINT "SORT BY: "; CP$(CP%); TAB(40);
    LOCATE 3, 1: COLOR 2, 0
    COLOR 14: PRINT B$
    PRINT "NAME"; TAB(15);
    PRINT " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"
    FOR I = 0 TO 13
    IF A$(I) = "XXX" THEN 165
    COLOR 7
    IF I / 2 = INT(I / 2) THEN COLOR 15
    PRINT LEFT$(A$(I), 13); TAB(15);
    PRINT USING "## "; A(I, 0);
    IF A(I, 10) > 0 THEN LOCATE , 18: PRINT USING "####"; INT((A(I, 10) + 30) / 60); : LOCATE , 24: PRINT USING "##"; INT(((A(I, 10) / 60) / A(I, 0)) + .5);
    LOCATE , 28: PRINT USING "#### ####"; A(I, 2); A(I, 1); : LOCATE , 38
    IF A(I, 1) = 0 THEN PRINT ".---"; : GOTO 160
    IF A(I, 2) = A(I, 1) THEN PRINT "1.00"; : GOTO 160
    IF A(I, 2) = 0 THEN PRINT ".000"; : GOTO 160
    PRINT USING ".###"; A(I, 2) / A(I, 1);
160 LOCATE , 44: PRINT USING "#### ####"; W0(I); W1(I); : LOCATE , 54
    IF W1(I) = 0 THEN PRINT ".---"; : GOTO 161
    IF W1(I) = W0(I) THEN PRINT "1.00"; : GOTO 161
    IF W0(I) = 0 THEN PRINT ".000"; : GOTO 161
    PRINT USING ".###"; W0(I) / W1(I);
161 LOCATE , 60: PRINT USING "#### ####"; A(I, 4); A(I, 3); : LOCATE , 70
    IF A(I, 3) = 0 THEN PRINT ".---": GOTO 165
    IF A(I, 3) = A(I, 4) THEN PRINT "1.00": GOTO 165
    IF A(I, 4) = 0 THEN PRINT ".000": GOTO 165
    PRINT USING ".###"; A(I, 4) / A(I, 3)
165 NEXT
    COLOR 12: PRINT : PRINT "TOTALS"; : LOCATE , 15
    PRINT USING "##"; Y%;
    LOCATE , 28: PRINT USING "#### ####"; Z(2); Z(1);
    IF Z(1) <> 0 THEN LOCATE , 38: PRINT USING ".###"; Z(2) / Z(1);
    LOCATE , 44: PRINT USING "#### ####"; Z(13); Z(14);
    IF Z(14) <> 0 THEN LOCATE , 54: PRINT USING ".###"; Z(13) / Z(14);
    LOCATE , 60: PRINT USING "#### ####"; Z(4); Z(3);
    IF Z(3) <> 0 THEN LOCATE , 70: PRINT USING ".###"; Z(4) / Z(3)
    COLOR 12: LOCATE , 15: PRINT USING "##"; Y%;
    LOCATE , 28: PRINT USING "#### ####"; Z1(2); Z1(1);
    IF Z1(1) <> 0 THEN LOCATE , 38: PRINT USING ".###"; Z1(2) / Z1(1);
    LOCATE , 44: PRINT USING "#### ####"; Z1(13); Z1(14);
    IF Z1(14) <> 0 THEN LOCATE , 54: PRINT USING ".###"; Z1(13) / Z1(14);
    LOCATE , 60: PRINT USING "#### ####"; Z1(4); Z1(3);
    IF Z1(3) <> 0 THEN LOCATE , 70: PRINT USING ".###"; Z1(4) / Z1(3)
230 GOSUB 125
    IF I$ = CHR$(0) + "Q" THEN CP% = 14: GOTO 235
    IF I$ = CHR$(0) + "I" THEN CLS : GOTO 915
    IF UCASE$(I$) = "D" THEN GOSUB 485: GOTO 230
    IF I$ = CHR$(0) + "M" THEN GOSUB 650: GOTO 230: REM:RIGHT
    IF I$ = CHR$(0) + "K" THEN GOSUB 660: GOTO 230: REM:LEFT
    IF UCASE$(I$) = "P" THEN GOSUB 875: GOSUB 530: GOTO 230
    IF I$ = CHR$(13) THEN GOSUB 600: GOTO 142
    IF I$ = CHR$(27) THEN 45
    GOTO 230
650 CP% = CP% + 1: IF CP% > 12 THEN CP% = 1
    LOCATE 2, 1: PRINT "SORT BY: "; CP$(CP%); TAB(40);
    RETURN
660 CP% = CP% - 1: IF CP% < 1 THEN CP% = 12
    LOCATE 2, 1: PRINT "SORT BY: "; CP$(CP%); TAB(40);
    RETURN
235 CLS
    CALL TITLE
    COLOR , 1
    LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
    LOCATE 24, 1: COLOR 14, 1
    COLOR 15: PRINT "MORE STATS ";
    COLOR 14: PRINT "[Pg UP]  ";
    COLOR 15: PRINT "SELECT NEW TEAM ";
    COLOR 14: PRINT "[ESC]  ";
    COLOR 15: PRINT "SORT ";
    COLOR 14: PRINT "<-,-> and [ENTER]  ";
    COLOR 15: PRINT "PRINT ";
    COLOR 14: PRINT "[P]";
    LOCATE 25, 1
    COLOR 15: PRINT "DELETE STATS "; : COLOR 14: PRINT "[D]                     ";
    COLOR 12, 0: LOCATE 2, 1: PRINT "SORT BY: "; CP$(CP%); TAB(40);
    LOCATE 3, 1: COLOR 2, 0
    COLOR 14: PRINT B$; : LOCATE , 15
    PRINT "               OFF  DEF  TOT": PRINT "NAME"; TAB(15);
    PRINT " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"
    FOR I = 0 TO 13
    IF A$(I) = "XXX" THEN 266
    COLOR 7
    IF I / 2 = INT(I / 2) THEN COLOR 15
    PRINT LEFT$(A$(I), 13); : LOCATE , 15
    PRINT USING "####"; A(I, 2) * 2 + A(I, 4) + W0(I);
    IF A(I, 0) > 0 THEN LOCATE , 21: PRINT USING "##.#"; (A(I, 2) * 2 + A(I, 4) + W0(I)) / A(I, 0);
    LOCATE , 26: PRINT USING "## #### #### ####"; A(I, 13); A(I, 12); A(I, 5) - A(I, 12); A(I, 5);
    IF A(I, 0) > 0 THEN LOCATE , 45: PRINT USING "##.#"; A(I, 5) / A(I, 0);
    LOCATE , 51: PRINT USING "### ####  ###  ###  ###"; A(I, 6); A(I, 8); A(I, 9); A(I, 7); A(I, 11)
266 NEXT I
    PRINT "TEAM"; : LOCATE , 39: PRINT USING "####"; Z(15)
    COLOR 12: PRINT "TOTALS";
    LOCATE , 14: PRINT USING "##### ###.#"; Z(10); Z(10) / Y%;
    LOCATE , 29: PRINT USING "#### #### ####  ##.#"; Z(12); Z(5) - Z(12) - Z(15); Z(5); Z(5) / Y%;
    PRINT USING " #### #### #### #### ####"; Z(6); Z(8); Z(9); Z(7); Z(11)
    LOCATE , 14: PRINT USING "##### ###.#"; Z1(10); Z1(10) / Y%;
    LOCATE , 29: PRINT USING "#### #### ####  ##.#"; Z1(12); Z1(5) - Z1(12) - Z1(15); Z1(5); Z1(5) / Y%;
    PRINT USING " #### #### #### #### ####"; Z1(6); Z1(8); Z1(9); Z1(7); Z1(11): COLOR 3
370 I$ = INKEY$: IF I$ = "" THEN 370
    IF I$ = CHR$(0) + "I" THEN CLS : CP% = 1: GOTO 142
    IF I$ = CHR$(0) + "M" THEN GOSUB 655: GOTO 370: REM:RIGHT
    IF UCASE$(I$) = "D" THEN GOSUB 485: GOTO 370
    IF I$ = CHR$(0) + "K" THEN GOSUB 665: GOTO 370: REM:LEFT
    IF I$ = CHR$(13) THEN GOSUB 600: GOTO 235
    IF I$ = CHR$(27) THEN 45
    IF UCASE$(I$) = "P" THEN GOSUB 875: GOSUB 530: GOTO 370
    GOTO 370
655 CP% = CP% + 1: IF CP% > 25 THEN CP% = 14
    LOCATE 2, 1: PRINT "SORT BY: "; CP$(CP%); TAB(40);
    RETURN
665 CP% = CP% - 1: IF CP% < 14 THEN CP% = 25
    LOCATE 2, 1: PRINT "SORT BY: "; CP$(CP%); TAB(40);
    RETURN
    CLS : CALL TITLE: PRINT "RESET STATS TO 0 ? (YN)"; :
455 I$ = INKEY$: IF I$ = "" THEN 455
    IF UCASE$(I$) <> "Y" THEN 45
    PRINT : PRINT "ARE YOU SURE ? (YN) ";
470 I$ = INKEY$: IF I$ = "" THEN 470
    IF UCASE$(I$) = "N" THEN X = FRE(0): RUN
485 KILL D$(1) + Z$
    KILL D$(1) + Z$ + "S"
    RETURN
    CLS
    RUN
530 LPRINT "STATISTICS SUMMARY FOR "; B$
    LPRINT "NAME"; TAB(15);
    LPRINT " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"
    FOR I = 0 TO 13
    LPRINT LEFT$(A$(I), 13); TAB(15);
    LPRINT USING "## "; A(I, 0);
    IF A(I, 10) > 0 THEN LPRINT TAB(18); USING "####"; INT((A(I, 10) + 30) / 60); : LPRINT TAB(24); USING "##"; INT(((A(I, 10) / 60) / A(I, 0)) + .5);
    LPRINT TAB(28); USING "#### ####"; A(I, 2); A(I, 1);
    LPRINT TAB(38);
    IF A(I, 1) = 0 THEN LPRINT ".---"; : GOTO 531
    IF A(I, 2) = A(I, 1) THEN LPRINT "1.00"; : GOTO 531
    IF A(I, 2) = 0 THEN LPRINT ".000"; : GOTO 531
    LPRINT USING ".###"; A(I, 2) / A(I, 1);
531 LPRINT TAB(44); USING "#### ####"; W0(I); W1(I); : LPRINT TAB(54);
    IF W1(I) = 0 THEN LPRINT ".---"; : GOTO 532
    IF W1(I) = W0(I) THEN LPRINT "1.00"; : GOTO 532
    IF W0(I) = 0 THEN LPRINT ".000"; : GOTO 532
    LPRINT USING ".###"; W0(I) / W1(I);
532 LPRINT TAB(60); USING "#### ####"; A(I, 4); A(I, 3); : LPRINT TAB(70);
    IF A(I, 3) = 0 THEN LPRINT ".---": GOTO 533
    IF A(I, 3) = A(I, 4) THEN LPRINT "1.00": GOTO 533
    IF A(I, 4) = 0 THEN LPRINT ".000": GOTO 533
    LPRINT USING ".###"; A(I, 4) / A(I, 3)
533 NEXT
    LPRINT : LPRINT "TOTALS"; TAB(15);
    LPRINT USING "##"; Y%;
    LPRINT TAB(28); USING "#### ####"; Z(2); Z(1);
    IF Z(1) <> 0 THEN LPRINT TAB(38); USING ".###"; Z(2) / Z(1);
    LPRINT TAB(44); USING "#### ####"; Z(13); Z(14);
    IF Z(14) <> 0 THEN LPRINT TAB(54); USING ".###"; Z(13) / Z(14);
    LPRINT TAB(60); USING "#### ####"; Z(4); Z(3);
    IF Z(3) <> 0 THEN LPRINT TAB(70); USING ".###"; Z(4) / Z(3)
    LPRINT TAB(15); USING "##"; Y%;
    LPRINT TAB(28); USING "#### ####"; Z1(2); Z1(1);
    IF Z1(1) <> 0 THEN LPRINT TAB(38); USING ".###"; Z1(2) / Z1(1);
    LPRINT TAB(44); USING "#### ####"; Z1(13); Z1(14);
    IF Z1(14) <> 0 THEN LPRINT TAB(54); USING ".###"; Z1(13) / Z1(14);
    LPRINT TAB(60); USING "#### ####"; Z1(4); Z1(3);
    IF Z1(3) <> 0 THEN LPRINT TAB(70); USING ".###"; Z1(4) / Z1(3)
    LPRINT
    LPRINT TAB(15);
    LPRINT "               OFF  DEF  TOT": LPRINT TAB(15);
    LPRINT " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"
    FOR I = 0 TO 13
    LPRINT LEFT$(A$(I), 13); : LPRINT TAB(15);
    LPRINT USING "####"; A(I, 2) * 2 + A(I, 4) + W0(I);
    IF A(I, 0) > 0 THEN LPRINT TAB(21); USING "##.#"; (A(I, 2) * 2 + A(I, 4) + W0(I)) / A(I, 0);
    LPRINT TAB(26); USING "## #### #### ####"; A(I, 13); A(I, 12); A(I, 5) - A(I, 12); A(I, 5);
    IF A(I, 0) > 0 THEN LPRINT TAB(45); USING "##.#"; A(I, 5) / A(I, 0);
    LPRINT TAB(51); USING "### ####  ###  ###  ###"; A(I, 6); A(I, 8); A(I, 9); A(I, 7); A(I, 11)
    NEXT I
    LPRINT "TEAM"; : LPRINT TAB(39); USING "####"; Z(15)
    LPRINT : LPRINT "TOTALS";
    LPRINT TAB(14); USING "##### ###.#"; Z(10); Z(10) / Y%;
    LPRINT TAB(29); USING "#### #### ####  ##.#"; Z(12); Z(5) - Z(12) - Z(15); Z(5); Z(5) / Y%;
    LPRINT USING " #### #### #### #### ####"; Z(6); Z(8); Z(9); Z(7); Z(11)
    LPRINT TAB(14); USING "##### ###.#"; Z1(10); Z1(10) / Y%;
    LPRINT TAB(29); USING "#### #### ####  ##.#"; Z1(12); Z1(5) - Z1(12) - Z1(15); Z1(5); Z1(5) / Y%;
    LPRINT USING " #### #### #### #### ####"; Z1(6); Z1(8); Z1(9); Z1(7); Z1(11)
    LPRINT CHR$(12)
    RETURN
600 REM:SORT BASED ON CATEGORY
    IF CP% = 1 THEN SG% = 0
    IF CP% = 2 THEN SG% = 10
    IF CP% = 3 THEN SG% = 14
    IF CP% = 4 THEN SG% = 2
    IF CP% = 5 THEN SG% = 1
    IF CP% = 6 THEN SG% = 15
    IF CP% = 7 THEN SG% = 18
    IF CP% = 8 THEN SG% = 19
    IF CP% = 9 THEN SG% = 16
    IF CP% = 10 THEN SG% = 4
    IF CP% = 11 THEN SG% = 3
    IF CP% = 12 THEN SG% = 17
    IF CP% = 14 THEN SG% = 22
    IF CP% = 15 THEN SG% = 20
    IF CP% = 17 THEN SG% = 12
    IF CP% = 16 THEN SG% = 13
    IF CP% = 18 THEN SG% = 23
    IF CP% = 19 THEN SG% = 5
    IF CP% = 20 THEN SG% = 21
    IF CP% = 21 THEN SG% = 6
    IF CP% = 22 THEN SG% = 8
    IF CP% = 23 THEN SG% = 9
    IF CP% = 24 THEN SG% = 7
    IF CP% = 25 THEN SG% = 11
    FOR Z = 0 TO 12
    FOR Z1 = Z + 1 TO 13
    IF A(Z, SG%) >= A(Z1, SG%) THEN 605
    FOR I = 0 TO 24: SWAP A(Z, I), A(Z1, I): NEXT
    SWAP W0(Z), W0(Z1)
    SWAP W1(Z), W1(Z1)
    SWAP A$(Z), A$(Z1)
605 NEXT Z1: NEXT Z
    RETURN

815 CALL TITLE: COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 31: COLOR 14
    Y = LEN(B$): Y = INT((80 - Y) / 2): LOCATE , Y: PRINT B$
    LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1
    LOCATE 2, 1
    COLOR , 0
    FOR I = 1 TO Y%
    COLOR 7
    IF I / 2 = INT(I / 2) THEN COLOR 15
    IF I > 54 THEN 860
    IF I > 36 THEN 827
    IF I > 18 THEN 826
    PRINT USING "###"; O%(I);
    PRINT TAB(5); LEFT$(O$(I), 14);
    PRINT TAB(20); USING "###"; T%(I);
    PRINT TAB(24); H$(I)
    GOTO 830
826 LOCATE I - 17, 27: PRINT USING "###"; O%(I);
    PRINT TAB(31); LEFT$(O$(I), 14);
    PRINT TAB(46); USING "###"; T%(I);
    PRINT TAB(50); H$(I)
    GOTO 830
827 LOCATE I - 35, 53: PRINT USING "###"; O%(I);
    PRINT TAB(57); LEFT$(O$(I), 14);
    PRINT TAB(72); USING "###"; T%(I);
    PRINT TAB(76); H$(I)
830 NEXT I
    PRINT
860 LOCATE 21, 27
    COLOR 14: PRINT "OVERALL RECORD "; : COLOR 15: PRINT "("; W%; "-"; L%; ")"
    LOCATE , 27: COLOR 14: PRINT "HOME RECORD    "; : COLOR 15: PRINT "("; HW%; "-"; HL%; ")"
    LOCATE , 27: COLOR 14: PRINT "ROAD RECORD    "; : COLOR 15: PRINT "("; VW%; "-"; VL%; ")"
    LOCATE , 27: COLOR 14: PRINT "NEUTRAL RECORD "; : COLOR 15: PRINT "("; NW%; "-"; NL%; ")";
    COLOR 3
    LOCATE 25, 1
    COLOR 15, 1: PRINT "INDIVIDUAL STATS ";
    COLOR 14: PRINT "[Pg DN]  ";
    COLOR 15: PRINT "SELECT ANOTHER TEAM ";
    COLOR 14: PRINT "[ESC]  ";
    COLOR 15: PRINT "PRINT STATS ";
    COLOR 14: PRINT "[P]";
    LOCATE 3, 1
870 I$ = INKEY$: IF I$ = "" THEN 870
    IF I$ = CHR$(0) + "Q" THEN COLOR , 0: CLS : GOTO 915
    IF UCASE$(I$) = "D" THEN GOSUB 485: GOTO 870
    IF I$ = CHR$(27) THEN 45
    IF UCASE$(I$) = "P" THEN GOSUB 875: GOSUB 530: GOTO 870
    GOTO 870
915 COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 31: COLOR 14
    Y = LEN(B$): Y = INT((80 - Y) / 2): LOCATE , Y: PRINT B$
    LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1
    LOCATE 2, 1
    COLOR , 0
    FOR I = 1 TO Y%
    COLOR 7
    IF I / 2 = INT(I / 2) THEN COLOR 15
    IF I < 55 THEN 930
    IF I > 72 THEN 926
    PRINT USING "###"; O%(I);
    PRINT TAB(5); LEFT$(O$(I), 14);
    PRINT TAB(20); USING "###"; T%(I);
    PRINT TAB(24); H$(I)
    GOTO 930
926 LOCATE I - 71, 27: PRINT USING "###"; O%(I);
    PRINT TAB(31); LEFT$(O$(I), 14);
    PRINT TAB(46); USING "###"; T%(I);
    PRINT TAB(50); H$(I)
    GOTO 930
930 NEXT I
    PRINT
    LOCATE 21, 27
    COLOR 14: PRINT "OVERALL RECORD "; : COLOR 15: PRINT "("; W%; "-"; L%; ")"
    LOCATE , 27: COLOR 14: PRINT "HOME RECORD    "; : COLOR 15: PRINT "("; HW%; "-"; HL%; ")"
    LOCATE , 27: COLOR 14: PRINT "ROAD RECORD    "; : COLOR 15: PRINT "("; VW%; "-"; VL%; ")"
    LOCATE , 27: COLOR 14: PRINT "NEUTRAL RECORD "; : COLOR 15: PRINT "("; NW%; "-"; NL%; ")";
    COLOR 3
    LOCATE 25, 1
    COLOR 15, 1: PRINT "INDIVIDUAL STATS ";
    COLOR 14: PRINT "[Pg DN]  ";
    COLOR 15: PRINT "SELECT ANOTHER TEAM ";
    COLOR 14: PRINT "[ESC]  ";
    COLOR 15: PRINT "PRINT STATS ";
    COLOR 14: PRINT "[P]";
    LOCATE 3, 1
970 I$ = INKEY$: IF I$ = "" THEN 970
    IF I$ = CHR$(0) + "Q" THEN COLOR , 0: CLS : GOTO 142
    IF I$ = CHR$(0) + "I" THEN COLOR , 0: CLS : GOTO 815
    IF UCASE$(I$) = "D" THEN GOSUB 485: GOTO 970
    IF I$ = CHR$(27) THEN 45
    IF UCASE$(I$) = "P" THEN GOSUB 875: GOSUB 530: GOTO 970
    GOTO 970
875 LPRINT "SCHEDULE RESULTS FOR "; B$
    LPRINT
    FOR I = 1 TO 28
    LPRINT USING "###"; O%(I);
    LPRINT TAB(5); LEFT$(O$(I), 14);
    LPRINT TAB(20); USING "###"; T%(I);
    LPRINT TAB(24); H$(I); TAB(27);
    IF I + 28 > Y% THEN LPRINT : GOTO 880
    LPRINT USING "###"; O%(I + 28);
    LPRINT TAB(31); LEFT$(O$(I + 28), 14);
    LPRINT TAB(46); USING "###"; T%(I + 28);
    LPRINT TAB(50); H$(I + 28); TAB(53);
    IF I + 56 > Y% THEN LPRINT : GOTO 880
    LPRINT USING "###"; O%(I + 56);
    LPRINT TAB(57); LEFT$(O$(I + 56), 14);
    LPRINT TAB(72); USING "###"; T%(I + 56);
    LPRINT TAB(76); H$(I + 56)
880 NEXT I
    LPRINT
    LPRINT TAB(27); "OVERALL RECORD ("; W%; "-"; L%; ")"
    LPRINT TAB(27); "HOME RECORD    ("; HW%; "-"; HL%; ")"
    LPRINT TAB(27); "ROAD RECORD    ("; VW%; "-"; VL%; ")"
    LPRINT TAB(27); "NEUTRAL RECORD ("; NW%; "-"; NL%; ")"
    LPRINT CHR$(12)
    RETURN


3056 COLOR 15, 0
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
3058 LOCATE 2, 33: COLOR T%(0), T%(1): PRINT A$
     COLOR 15, 4: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     I$ = INKEY$: IF I$ = "" THEN 3058
   IF UCASE$(I$) = "Y" THEN 3056
   COLOR 15, 0
     RETURN
1100 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 655
     GET #1, I1
     VV$(I1) = Q$(0): VV%(I1) = I1
    NEXT
    CLOSE 1
    NTMS = Y / 655
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
     T1 = VV%(SELECTION%)
9000 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
GET #1, T1
   A$ = Q$(0)
9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
     B$ = A$
     SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
     FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
     FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
     CLOSE 1
     B$ = A$
     RETURN
6585 FOR XX = 4 TO 11: IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
     NEXT XX
     FOR XX = 12 TO 14: IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
     NEXT XX
     QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
     GOTO 6620
6615 NEXT XX
6620 RETURN

4105 IF UCASE$(LEFT$(D$(0), 1)) = "C" THEN RETURN
     CLS : PRINT "MAKE SURE CORRECT TEAMS DISK IS IN DRIVE "; D$(0)
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
     GOSUB 4501
     RETURN

4500 COLOR 15, 0: LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
4501 I$ = INKEY$: IF I$ = "" THEN 4501
     RETURN
9701 I$ = INKEY$: IF I$ = "" THEN 9701
     RETURN

5025 CLS : PRINT "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
     PRINT TAB(10); "CHECK THAT CORRECT DISK IS IN DRIVE"
     GOSUB 9701
     GOTO 45
895 COLOR 7: LOCATE 24, 1: PRINT "ANY KEY TO CONTINUE";
896 I$ = INKEY$: IF I$ = "" THEN 896
    RETURN

   SUB GETKEY ()
46 I$ = INKEY$: IF I$ = "" THEN 46
END SUB

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 23: COLOR 12
   PRINT "COURTSIDE COLLEGE BASKETBALL V4.0"
   LOCATE 3, 1: COLOR 2, 0
END SUB

