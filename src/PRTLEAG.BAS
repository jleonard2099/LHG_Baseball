DECLARE SUB GETKEY ()
DEFINT A-Z
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST (FILENAME$)
1 DIM T$(40), W(40), L(40), P$(298), S$(40), P(40), BA$(40), ER$(40), OFT(40, 20) AS SINGLE, DFT(40, 20) AS SINGLE
  DIM D$(2), J AS SINGLE
  DIM ATBATS AS SINGLE
  DIM INNINGS AS SINGLE
2 DIM LD$(50, 3), LD(50, 2), NM$(10, 6, 3), NM%(10, 6), Z2$(298), Z3$(298), Z(298) AS SINGLE, Z1(298) AS SINGLE
  DIM LF$(70), RT$(70)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
   OPEN "DEV.BAS" FOR INPUT AS #2
   FOR I = 0 TO 2
   INPUT #2, D$(I)
   NEXT
   CLOSE 2
   CLS
   IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 4000
   PRINT "MAKE SURE DISK WITH STATS AND LEADER FILES IS IN DRIVE "; D$(2)
   CALL GETKEY

4000 Spec$ = D$(2) + "*.LDR"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 4500
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
    PRINT "SELECT LEAGUE TO PRINT": COLOR 15
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
     IF ExitCode% = 27 THEN 2900
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 F$ = QPRTrim$(F$)
    WE$ = LEFT$(F$, LEN(F$) - 4)
    GOTO 7

4500 LOCATE 10, 8
     CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 2900

7 WE$ = LEFT$(WE$, 8)

  OPEN D$(2) + WE$ + ".TMS" FOR INPUT AS #2
  INPUT #2, LG$, D1$, D2$, D3$, D4$
  CLOSE 2
9 H$ = " 1/2"
  CLS
101 LOCATE 9, 1: PRINT "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"
111 OPEN D$(2) + WE$ + ".LDR" FOR INPUT AS #2
121 FOR X = 1 TO 40: INPUT #2, T$(X), W(X), L(X): NEXT
131 FOR X = 1 TO 298: INPUT #2, Z2$(X), Z3$(X), Z(X), Z1(X): NEXT
141 FOR X = 1 TO 40: INPUT #2, BA$(X): FOR M = 1 TO 14: INPUT #2, OFT(X, M): NEXT: NEXT
151 FOR X = 1 TO 40: INPUT #2, ER$(X): FOR M = 1 TO 14: INPUT #2, DFT(X, M): NEXT: NEXT
161 CLOSE #2
171 GOTO 421
181 L = 8: IF LEN(D1$) < L THEN D1$ = D1$ + " ": GOTO 181
191 L = 8: IF LEN(D2$) < L THEN D2$ = D2$ + " ": GOTO 191
201 L = 8: IF LEN(D3$) < L THEN D3$ = D3$ + " ": GOTO 201
211 L = 8: IF LEN(D4$) < L THEN D4$ = D4$ + " ": GOTO 211
221 RETURN
231 FOR P = 1 TO 5: P$(P) = "": IF NM$(CT, P, 1) = "*" THEN 261
241 P$(P) = NM$(CT, P, 1): IF NM$(CT, P, 2) <> "*" THEN P$(P) = P$(P) + ", " + NM$(CT, P, 2)
251 L = 22: GOSUB 271: J = NM%(CT, P): W = 3: GOSUB 391
261 NEXT P: RETURN
271 IF LEN(P$(P)) < L THEN P$(P) = P$(P) + " ": GOTO 271
281 RETURN
291 J = INT(100 * J + .5): A$ = STR$(J): LN = LEN(A$): A$ = RIGHT$(A$, LN - 1)
301 LN = LEN(A$): IF LEN(A$) < 3 THEN A$ = "0" + A$: GOTO 301
311 A$ = LEFT$(A$, LN - 2) + "." + RIGHT$(A$, 2)
321 A$ = "    " + A$: A$ = RIGHT$(A$, 5)
331 P$(P) = P$(P) + A$: RETURN
341 J = INT(1000 * J + .5): IF J = 1000 THEN P$(P) = P$(P) + "1.000 ": RETURN
351 A$ = STR$(J): L = LEN(A$): A$ = RIGHT$(A$, L - 1)
361 IF LEN(A$) < 3 THEN A$ = "0" + A$: GOTO 361
371 A$ = " ." + RIGHT$(A$, 3)
381 P$(P) = P$(P) + A$: RETURN
391 A$ = "     " + STR$(J): A$ = RIGHT$(A$, W): P$(P) = P$(P) + A$: RETURN
401 PRINT P$: RETURN
411 P$(P) = P$(P) + " " + STR$(J): RETURN
421 D1 = W(1) - L(1)
431 FOR P = 1 TO 10: IF T$(P) = "*" THEN P$(P) = "": GOTO 521
441 P$(P) = T$(P): L = 12: GOSUB 271: J = W(P): W = 3: GOSUB 391: L = 16: GOSUB 271
451 J = L(P): GOSUB 391: L = 21: GOSUB 271: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))
461 GOSUB 341: L = 28: GOSUB 271
471 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"
481 IF D = 1 THEN BK$ = "  "
491 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
501 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
511 P$(P) = P$(P) + BK$
521 NEXT P
531 D1 = W(11) - L(11)
541 FOR P = 11 TO 20: IF T$(P) = "*" THEN P$(P) = "": GOTO 631
551 P$(P) = T$(P): L = 12: GOSUB 271: J = W(P): W = 3: GOSUB 391: L = 16: GOSUB 271
561 J = L(P): GOSUB 391: L = 21: GOSUB 271: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))
571 GOSUB 341: L = 28: GOSUB 271
581 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"
591 IF D = 1 THEN BK$ = "  "
601 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
611 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
621 P$(P) = P$(P) + BK$
631 NEXT P
641 D1 = W(21) - L(21)
651 FOR P = 21 TO 30: IF T$(P) = "*" THEN P$(P) = "": GOTO 741
661 P$(P) = T$(P): L = 12: GOSUB 271: J = W(P): W = 3: GOSUB 391: L = 16: GOSUB 271
671 J = L(P): GOSUB 391: L = 21: GOSUB 271: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))
681 GOSUB 341: L = 28: GOSUB 271
691 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"
701 IF D = 1 THEN BK$ = "  "
711 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
721 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
731 P$(P) = P$(P) + BK$
741 NEXT P
751 D1 = W(31) - L(31)
761 FOR P = 31 TO 40: IF T$(P) = "*" THEN P$(P) = "": GOTO 851
771 P$(P) = T$(P): L = 12: GOSUB 271: J = W(P): W = 3: GOSUB 391: L = 16: GOSUB 271
781 J = L(P): GOSUB 391: L = 21: GOSUB 271: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))
791 GOSUB 341: L = 28: GOSUB 271
801 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"
811 IF D = 1 THEN BK$ = "  "
821 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
831 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
841 P$(P) = P$(P) + BK$
851 NEXT P
861 CLS : REM:****NOW TO BUILD THE STRINGS FOR PRINTING****
871 LF$(1) = "    " + LG$ + " LEAGUE STANDINGS AS OF " + DATE$
881 LF$(3) = "    " + D1$ + "      W   L   PCT   GB"
891 FOR P = 1 TO 10: IF P$(P) = "" THEN RT = RT + 1: GOTO 911
901 LF$(P + 3 - RT) = "    " + P$(P)
911 NEXT P
921 RT = 0
931 REM
941 LF$(15) = "    " + D3$ + "      W   L   PCT   GB": FOR P = 21 TO 30: IF P$(P) = "" THEN RT = RT + 1: GOTO 961
951 LF$(P - 5 - RT) = "    " + P$(P)
961 NEXT P
971 REM:RT=0
981 RT = 0
991 RT$(3) = "    " + D2$ + "      W   L   PCT   GB": FOR P = 11 TO 20: IF P$(P) = "" THEN RT = RT + 1: GOTO 1011
1001 RT$(P - 7 - RT) = "    " + P$(P)
1011 NEXT P
1021 RT = 0
1031 REM
1041 RT$(15) = "    " + D4$ + "      W   L   PCT   GB": FOR P = 31 TO 40: IF P$(P) = "" THEN RT = RT + 1: GOTO 1061
1051 RT$(P - 15 - RT) = "    " + P$(P)
1061 NEXT P
1071 FOR P = 2 TO 26
1081 IF LEN(LF$(P)) < 38 THEN LF$(P) = LF$(P) + " ": GOTO 1081
1091 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1091
1101 NEXT P
1111 FOR X = 1 TO 26: LPRINT LF$(X); RT$(X): NEXT
1121 AB = INT(AB): AB$ = STR$(AB): TD = IP - 3 * IG
1131 L = LEN(AB$): AB$ = RIGHT$(AB$, L - 1): IF TD > 0 THEN TD$ = " 1/3"
1141 IG$ = STR$(IP): L = LEN(IG$): IG$ = RIGHT$(IG$, L - 1): IP$ = IG$
1151 FOR P = 1 TO 20: P$(P) = "": IF Z2$(P) = "*" THEN 1191
1161 P$(P) = Z2$(P)
1171 IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1181 L = 27: GOSUB 271: J = Z(P): GOSUB 341: J = Z1(P): GOSUB 411
1191 NEXT P: CLS
1201 LF$(27) = "   BATTING LEADERS  (MIN 3.1 PA/G)"
1211 FOR P = 1 TO 19
     LF$(P + 27) = "   " + P$(P): NEXT: LF$(47) = " "

     FOR P = 239 TO 258: P$(P) = "": IF Z2$(P) = "*" THEN 1192
     P$(P) = Z2$(P)
     IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
     L = 27: GOSUB 271: J = Z(P): GOSUB 341: J = Z1(P): GOSUB 411
1192 NEXT P: CLS
     RT$(27) = "       BATTING TRAILERS  (MIN 3.1 PA/G)"
     FOR P = 239 TO 258
     RT$(P - 211) = "   " + P$(P): NEXT: RT$(47) = " "
1291 FOR P = 27 TO 47
1301 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1301
1311 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1311
1321 NEXT P
1331 FOR X = 27 TO 47: LPRINT LF$(X); RT$(X): NEXT

1351 FOR P = 21 TO 30: P$(P) = "": IF Z2$(P) = "*" THEN 1381
1361 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1371 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1381 NEXT P
1391 LF$(49) = "   HITS"
1401 FOR P = 21 TO 30: LF$(P + 29) = "   " + P$(P): NEXT P
1411 REM
1421 FOR P = 32 TO 41: P$(P) = "": IF Z2$(P) = "*" THEN 1451
1431 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1441 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1451 NEXT P
1461 RT$(49) = "       DOUBLES"
1471 FOR P = 32 TO 41: RT$(P + 18) = "       " + P$(P): NEXT P
1472 FOR P = 49 TO 59
1473 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1473
1474 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1474
1475 NEXT P
1476 FOR X = 49 TO 59: LPRINT LF$(X); RT$(X): NEXT
     LPRINT CHR$(12)
1481 FOR P = 43 TO 52: P$(P) = "": IF Z2$(P) = "*" THEN 1511
1491 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1501 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1511 NEXT P
1521 LF$(1) = "   TRIPLES"
1531 FOR P = 43 TO 52: LF$(P - 41) = "   " + P$(P): NEXT P: LF$(12) = " "
1541 FOR P = 54 TO 63: P$(P) = "": IF Z2$(P) = "*" THEN 1571
1551 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1561 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1571 NEXT P
1581 RT$(1) = "     HOME RUNS"
1591 FOR P = 54 TO 63: RT$(P - 52) = "     " + P$(P): NEXT P: RT$(12) = " "
1592 FOR P = 1 TO 12
1593 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1593
1594 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1594
1595 NEXT P
1596 FOR X = 1 TO 12: LPRINT LF$(X); RT$(X): NEXT
1611 FOR P = 65 TO 73: P$(P) = "": IF Z2$(P) = "*" THEN 1641
1621 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1631 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1641 NEXT P
1651 LF$(13) = "   RUNS"
1661 FOR P = 65 TO 73: LF$(P - 51) = "   " + P$(P): NEXT P: LF$(23) = " "
1671 FOR P = 76 TO 84: P$(P) = "": IF Z2$(P) = "*" THEN 1701
1681 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1691 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1701 NEXT P
1711 RT$(13) = "     RUNS BATTED IN"
1721 FOR P = 76 TO 84: RT$(P - 62) = "     " + P$(P): NEXT P: RT$(23) = " "
1722 FOR P = 13 TO 23
1723 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1723
1724 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1724
1725 NEXT P
1726 FOR X = 13 TO 23: LPRINT LF$(X); RT$(X): NEXT
1731 FOR P = 87 TO 96: P$(P) = "": IF Z2$(P) = "*" THEN 1761
1741 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1751 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1761 NEXT P
1771 LF$(24) = "   STOLEN BASES"
1781 FOR P = 87 TO 96: LF$(P - 62) = "   " + P$(P): NEXT P: LF$(35) = " "
1791 REM
1801 FOR P = 99 TO 108: P$(P) = "": IF Z2$(P) = "*" THEN 1831
1811 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1821 L = 28: GOSUB 271: J = Z(P): GOSUB 341
1831 NEXT P
1841 RT$(24) = "     SLUGGING PCT LEADERS"
1851 FOR P = 99 TO 108: RT$(P - 74) = "     " + P$(P): NEXT P: RT$(35) = " "
1852 FOR P = 24 TO 35
1853 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1853
1854 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1854
1855 NEXT P
1856 FOR X = 24 TO 35: LPRINT LF$(X); RT$(X): NEXT
1861 FOR P = 110 TO 119: P$(P) = "": IF Z2$(P) = "*" THEN 1891
1871 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1881 L = 28: GOSUB 271: J = Z(P): GOSUB 341
1891 NEXT P
1901 LF$(36) = "   ON BASE PCT LEADERS"
1911 FOR P = 110 TO 119: LF$(P - 73) = "   " + P$(P): NEXT P: LF$(47) = " "
1921 FOR P = 217 TO 226: P$(P) = "": IF Z2$(P) = "*" THEN 1951
1931 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1941 L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1951 NEXT P
1961 RT$(36) = "     BASE ON BALLS"
1971 FOR P = 217 TO 226: RT$(P - 180) = "    " + P$(P): NEXT P: RT$(47) = " "
1972 FOR P = 36 TO 47
1973 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1973
1974 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1974
1975 NEXT P
1976 FOR X = 36 TO 47: LPRINT LF$(X); RT$(X): NEXT
     FOR P = 279 TO 288: P$(P) = "": IF Z2$(P) = "*" THEN 1977
     P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
     L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1977 NEXT P
     LF$(48) = "   CURRENT HITTING STREAK"
     FOR P = 279 TO 288: LF$(P - 230) = "   " + P$(P): NEXT P
     FOR P = 289 TO 298: P$(P) = "": IF Z2$(P) = "*" THEN 1978
     P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
     L = 28: GOSUB 271: J = Z(P): W = 3: GOSUB 391
1978 NEXT P
     RT$(48) = "       LONGEST HITTING STREAK"
     FOR P = 289 TO 298: RT$(P - 240) = "       " + P$(P): NEXT P
     FOR P = 48 TO 58
1979 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 1979
1980 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 1980
     NEXT P
     FOR X = 48 TO 58: LPRINT LF$(X); RT$(X): NEXT
     LPRINT CHR$(12)



1221 FOR P = 121 TO 139: P$(P) = "": IF Z2$(P) = "*" THEN 1261
1231 P$(P) = Z2$(P)
1241 IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
1251 L = 24: GOSUB 271: J = Z(P): GOSUB 291: J = INT(Z1(P)): GOSUB 411
1261 NEXT P
1271 RT$(27) = "     ERA LEADERS (MIN 1 IP/G)"
1281 FOR P = 121 TO 139
     RT$(P - 93) = "    " + P$(P)
     NEXT

1991 FOR P = 140 TO 149
     P$(P) = ""
     IF Z(P) = Z(150) AND Z1(P) = Z1(150) THEN 2031
2001 P$(P) = P$(P) + Z2$(P)
     IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2011 L = 23: GOSUB 271: J = Z(P): W = 2: GOSUB 391: P$(P) = P$(P) + "-": J = Z1(P)
2021 GOSUB 391
2031 NEXT P

2041 LF$(48) = "   PITCHING RECORD"
2051 FOR P = 140 TO 149: LF$(P - 91) = "   " + P$(P)
2061 NEXT: LF$(59) = " "
2071 FOR P = 151 TO 160: P$(P) = "": IF Z2$(P) = "*" THEN 2101
2081 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2091 L = 24: GOSUB 271: J = Z(P): W = 3: GOSUB 391
2101 NEXT P
2111 RT$(48) = "     VICTORIES"
2121 FOR P = 151 TO 160: RT$(P - 102) = "     " + P$(P): NEXT P: RT$(59) = " "
2122 FOR P = 48 TO 59
2123 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 2123
2124 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 2124
2125 NEXT P
2126 FOR X = 48 TO 59: LPRINT LF$(X); RT$(X): NEXT: LPRINT CHR$(12)
2131 FOR P = 162 TO 171: P$(P) = "": IF Z2$(P) = "*" THEN 2161
2141 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2151 L = 24: GOSUB 271: J = INT(Z(P)): W = 3: GOSUB 391
2161 NEXT P
2171 LF$(1) = "   INNINGS PITCHED"
2181 FOR P = 162 TO 171: LF$(P - 160) = "   " + P$(P): NEXT P: LF$(12) = " "
2191 FOR P = 173 TO 182: P$(P) = "": IF Z2$(P) = "*" THEN 2221
2201 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2211 L = 24: GOSUB 271: J = Z(P): W = 3: GOSUB 391
2221 NEXT P
2231 RT$(1) = "     STRIKEOUTS"
2241 FOR P = 173 TO 182: RT$(P - 171) = "     " + P$(P): NEXT P: RT$(12) = " "
2242 FOR P = 1 TO 12
2243 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 2243
2244 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 2244
2245 NEXT P
2246 FOR X = 1 TO 12: LPRINT LF$(X); RT$(X): NEXT
2251 FOR P = 184 TO 193: P$(P) = "": IF Z2$(P) = "*" THEN 2281
2261 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2271 L = 24: GOSUB 271: J = Z(P): W = 3: GOSUB 391
2281 NEXT P
2291 LF$(13) = "   COMPLETE GAMES"
2301 FOR P = 184 TO 193: LF$(P - 170) = "   " + P$(P): NEXT P: LF$(24) = " "
2311 FOR P = 195 TO 204: P$(P) = "": IF Z2$(P) = "*" THEN 2341
2321 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2331 L = 24: GOSUB 271: J = Z(P): W = 3: GOSUB 391
2341 NEXT P
2351 RT$(13) = "     SHUTOUTS"
2361 FOR P = 195 TO 204: RT$(P - 181) = "     " + P$(P): NEXT P: RT$(24) = " "
2362 FOR P = 13 TO 24
2363 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 2363
2364 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 2364
2365 NEXT P
2366 FOR X = 13 TO 24: LPRINT LF$(X); RT$(X): NEXT
2371 FOR P = 206 TO 215: P$(P) = "": IF Z2$(P) = "*" THEN 2401
2381 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2391 L = 24: GOSUB 271: J = Z(P): W = 3: GOSUB 391
2401 NEXT P
2411 LF$(25) = "   SAVES"
2421 FOR P = 206 TO 215: LF$(P - 180) = "   " + P$(P): NEXT P: LF$(36) = " "
2431 RT$(25) = "     APPEARANCES"
2441 FOR P = 228 TO 237: P$(P) = "": IF Z2$(P) = "*" THEN 2471
2451 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
2461 L = 24: GOSUB 271: J = Z(P): W = 3: GOSUB 391
2471 NEXT P
2481 FOR P = 228 TO 237: RT$(P - 202) = "     " + P$(P): NEXT P: RT$(36) = " "
2482 FOR P = 25 TO 36
2483 IF LEN(LF$(P)) < 35 THEN LF$(P) = LF$(P) + " ": GOTO 2483
2484 IF LEN(RT$(P)) < 38 THEN RT$(P) = RT$(P) + " ": GOTO 2484
2485 NEXT P
2486 FOR X = 25 TO 36: LPRINT LF$(X); RT$(X): NEXT
     LPRINT CHR$(12)
2501 LPRINT "                                 CLUB BATTING   "
2511 LPRINT : LPRINT "CLUB         AVG   AB    R    H   2B   3B  HR  RBI  BB   SO   SB   CS  E"
2521 FOR X = 1 TO 40
2531 IF BA$(X) = "" OR LEFT$(BA$(X), 1) = "*" OR BA$(X) = "*" THEN 2581
2541 LPRINT LEFT$(BA$(X), 11); TAB(13);
2551 OFT(X, 1) = INT(OFT(X, 1) * 1000) / 1000: LPRINT USING ".###"; OFT(X, 1);
2561 LPRINT TAB(18); OFT(X, 3); TAB(24); OFT(X, 5); TAB(29); STR$(OFT(X, 4)); TAB(34); OFT(X, 7); TAB(39); OFT(X, 8); TAB(43); STR$(OFT(X, 9)); TAB(47); OFT(X, 6); TAB(52); OFT(X, 10);
2571 LPRINT TAB(57); OFT(X, 11); TAB(62); OFT(X, 12); TAB(67); OFT(X, 13); TAB(72); OFT(X, 14)
2581 NEXT
2591 LPRINT CHR$(12)
2601 LPRINT "                                 CLUB PITCHING  "
2611 LPRINT : LPRINT "CLUB         ERA  G    CG SHO  SV  IP    H    R   ER   BB   SO    "
2621 FOR X = 1 TO 40
2631 IF ER$(X) = "" OR ER$(X) = "*" THEN 2681
2641 LPRINT LEFT$(ER$(X), 11); TAB(13);
2651 DFT(X, 1) = INT(DFT(X, 1) * 100) / 100: LPRINT USING "##.##"; DFT(X, 1);
2661 LPRINT TAB(18); DFT(X, 11); TAB(23); DFT(X, 12); TAB(27); DFT(X, 13); TAB(31); DFT(X, 14); TAB(35); STR$(CINT(DFT(X, 2) / 3)); TAB(40); STR$(DFT(X, 3)); TAB(45); DFT(X, 4); TAB(50); DFT(X, 5);
2671 LPRINT TAB(55); DFT(X, 6); TAB(60); DFT(X, 7)
2681 NEXT: LPRINT CHR$(12)
2900 IF DRIVE >= 67 THEN 2905
     CLS
     PRINT "MAKE SURE PROGRAM DISK 3 IS IN DRIVE"
     CALL GETKEY
2905 RUN "HELLO.EXE"
2741 CP$ = "": L = LEN(LW$): FOR Z = 1 TO L: ALP = ASC(MID$(LW$, Z, 1))
2751 IF ALP > 64 AND ALP < 96 THEN CP$ = CP$ + CHR$(ALP + 32): GOTO 2771
2761 CP$ = CP$ + CHR$(ALP)
2771 NEXT Z: RETURN
2781 HZ = 1: VT = 24
2791 LOCATE VT, HZ: PRINT "                          "; :
2801 QK$ = INKEY$: IF QK$ = "" THEN 2801
2811 RETURN

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

