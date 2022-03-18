'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z

1 DIM T$(40), W(40), L(40), P$(298), S$(40), P(40), BA$(40), ER$(40), OFT(40, 20) AS SINGLE, DFT(40, 20) AS SINGLE
DIM D$(2), J AS SINGLE
DIM ATBATS AS SINGLE
DIM INNINGS AS SINGLE

2 DIM LD$(50, 3), LD(50, 2), NM$(10, 6, 3), NM%(10, 6), Z2$(298), Z3$(298), Z(298) AS SINGLE, Z1(298) AS SINGLE

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
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4500
REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	  F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE TO VIEW": COLOR 15
	SELECTION% = 1
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1
	COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS LEAGUE";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 2900
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
	
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 F$ = RTrim$(F$)
	WE$ = LEFT$(F$, LEN(F$) - 4)
	GOTO 7

4500 LOCATE 10, 8
	CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)

	GOTO 2900

7 WE$ = LEFT$(WE$, 8)
OPEN D$(2) + WE$ + ".TMS" FOR INPUT AS #2
INPUT #2, LG$, D1$, D2$, D3$, D4$
CLOSE 2
GOSUB 200

9 H$ = " 1/2"
CLS
	LOCATE 9, 1: PRINT "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

15 OPEN D$(2) + WE$ + ".LDR" FOR INPUT AS #2

20 FOR X = 1 TO 40: INPUT #2, T$(X), W(X), L(X): NEXT

30 FOR X = 1 TO 298: INPUT #2, Z2$(X), Z3$(X), Z(X), Z1(X): NEXT

40 FOR X = 1 TO 40: INPUT #2, BA$(X): FOR M = 1 TO 14: INPUT #2, OFT(X, M): NEXT: NEXT

41 FOR X = 1 TO 40: INPUT #2, ER$(X): FOR M = 1 TO 14: INPUT #2, DFT(X, M): NEXT: NEXT
	INPUT #2, APP

50 CLOSE #2

88 GOTO 1119

200 L = 8: IF LEN(D1$) < L THEN D1$ = D1$ + " ": GOTO 200

201 L = 8: IF LEN(D2$) < L THEN D2$ = D2$ + " ": GOTO 201

202 L = 8: IF LEN(D3$) < L THEN D3$ = D3$ + " ": GOTO 202

203 L = 8: IF LEN(D4$) < L THEN D4$ = D4$ + " ": GOTO 203

204 RETURN

410 FOR P = 1 TO 5: P$(P) = "": IF NM$(CT, P, 1) = "*" THEN 445

415 P$(P) = NM$(CT, P, 1): IF NM$(CT, P, 2) <> "*" THEN P$(P) = P$(P) + ", " + NM$(CT, P, 2)

425 L = 22: GOSUB 900: J = NM%(CT, P): W = 3: GOSUB 940

445 NEXT P: RETURN

900 IF LEN(P$(P)) < L THEN P$(P) = P$(P) + " ": GOTO 900

910 RETURN

920 J = INT(100 * J + .5): A$ = STR$(J): LN = LEN(A$): A$ = RIGHT$(A$, LN - 1)

922 LN = LEN(A$): IF LEN(A$) < 3 THEN A$ = "0" + A$: GOTO 922

923 A$ = LEFT$(A$, LN - 2) + "." + RIGHT$(A$, 2)

924 A$ = "    " + A$: A$ = RIGHT$(A$, 5)

925 P$(P) = P$(P) + A$: RETURN

930 J = INT(1000 * J + .5)
	IF J = 1000 THEN P$(P) = P$(P) + "1.000 ": RETURN
	 IF J > 1000 THEN
	  A$ = STR$(J - 1000)
	  L = LEN(A$)
	  A$ = RIGHT$(A$, L - 1)

965 IF LEN(A$) < 3 THEN A$ = "0" + A$: GOTO 965
A$ = "1." + A$
P$(P) = P$(P) + A$
RETURN
	END IF

931 A$ = STR$(J): L = LEN(A$): A$ = RIGHT$(A$, L - 1)

936 IF LEN(A$) < 3 THEN A$ = "0" + A$: GOTO 936

937 A$ = " ." + RIGHT$(A$, 3)

938 P$(P) = P$(P) + A$: RETURN

940 A$ = "     " + STR$(J): A$ = RIGHT$(A$, W): P$(P) = P$(P) + A$: RETURN

950 PRINT P$: RETURN

960 P$(P) = P$(P) + "     " + STR$(J): RETURN

1119 D1 = W(1) - L(1)

1120 FOR P = 1 TO 10: IF T$(P) = "*" THEN P$(P) = "": GOTO 1129

1121 P$(P) = T$(P): L = 12: GOSUB 900: J = W(P): W = 3: GOSUB 940: L = 16: GOSUB 900

1122 J = L(P): GOSUB 940: L = 21: GOSUB 900: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))

1123 GOSUB 930: L = 28: GOSUB 900

1124 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"

1125 IF D = 1 THEN BK$ = "  "

1126 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)

1127 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

1128 P$(P) = P$(P) + BK$

1129 NEXT P

1130 D1 = W(11) - L(11)

1131 FOR P = 11 TO 20: IF T$(P) = "*" THEN P$(P) = "": GOTO 1140

1132 P$(P) = T$(P): L = 12: GOSUB 900: J = W(P): W = 3: GOSUB 940: L = 16: GOSUB 900

1133 J = L(P): GOSUB 940: L = 21: GOSUB 900: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))

1134 GOSUB 930: L = 28: GOSUB 900

1135 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"

1136 IF D = 1 THEN BK$ = "  "

1137 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)

1138 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

1139 P$(P) = P$(P) + BK$

1140 NEXT P

1141 D1 = W(21) - L(21)

1142 FOR P = 21 TO 30: IF T$(P) = "*" THEN P$(P) = "": GOTO 1151

1143 P$(P) = T$(P): L = 12: GOSUB 900: J = W(P): W = 3: GOSUB 940: L = 16: GOSUB 900

1144 J = L(P): GOSUB 940: L = 21: GOSUB 900: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))

1145 GOSUB 930: L = 28: GOSUB 900

1146 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"

1147 IF D = 1 THEN BK$ = "  "

1148 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)

1149 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

1150 P$(P) = P$(P) + BK$

1151 NEXT P

1161 D1 = W(31) - L(31)

1162 FOR P = 31 TO 40: IF T$(P) = "*" THEN P$(P) = "": GOTO 1171

1163 P$(P) = T$(P): L = 12: GOSUB 900: J = W(P): W = 3: GOSUB 940: L = 16: GOSUB 900

1164 J = L(P): GOSUB 940: L = 21: GOSUB 900: J = 0: IF W(P) + L(P) > 0 THEN J = W(P) / (W(P) + L(P))

1165 GOSUB 930: L = 28: GOSUB 900

1166 D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"

1167 IF D = 1 THEN BK$ = "  "

1168 IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)

1169 IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

1170 P$(P) = P$(P) + BK$

1171 NEXT P

1200 CLS

1201 COLOR 12: PRINT "                        "; LG$; " LEAGUE STANDINGS": PRINT
	COLOR 14

1212 LOCATE 3, 1: PRINT "    " + D1$ + "      W   L   PCT   GB"
	COLOR 15

1220 FOR P = 1 TO 10: IF P$(P) = "" THEN RT = RT + 1: GOTO 1256

1255 LOCATE (P + 3 - RT), 1: PRINT "    "; P$(P)

1256 NEXT P

1260 PRINT

1270 RT = 0

1271 REM

1272 COLOR 14
	LOCATE 15, 1: PRINT "    " + D3$ + "      W   L   PCT   GB"
	COLOR 15
	FOR P = 21 TO 30: IF P$(P) = "" THEN RT = RT + 1: GOTO 1274

1273 LOCATE (P - 5 - RT), 1: PRINT "    "; P$(P)

1274 NEXT P

1275 'RT=0

1276 RT = 0

1277 COLOR 14: LOCATE 3, 41: PRINT "    " + D2$ + "      W   L   PCT   GB"
	COLOR 15
	FOR P = 11 TO 20: IF P$(P) = "" THEN RT = RT + 1: GOTO 1280

1278 LOCATE (P - 7 - RT), 41: PRINT "    "; P$(P)

1280 NEXT P

1281 PRINT : RT = 0

1282 REM

1283 COLOR 14: LOCATE 15, 41: PRINT "    " + D4$ + "      W   L   PCT   GB"
	COLOR 15
	FOR P = 31 TO 40: IF P$(P) = "" THEN RT = RT + 1: GOTO 1285

1284 LOCATE (P - 15 - RT), 41: PRINT "    "; P$(P)

1285 NEXT P: GOSUB 7900

1300 AB = INT(AB): AB$ = STR$(AB): TD = IP - 3 * IG

1305 L = LEN(AB$): AB$ = RIGHT$(AB$, L - 1): IF TD > 0 THEN TD$ = " 1/3"
	IG$ = STR$(IP): L = LEN(IG$): IG$ = RIGHT$(IG$, L - 1): IP$ = IG$

1375 FOR P = 1 TO 19
	P$(P) = ""
	IF Z2$(P) = "*" THEN 1390

1376 P$(P) = Z2$(P)

1380 IF Z3$(P) <> "*" THEN
		P$(P) = P$(P) + ", " + Z3$(P)
	END IF

1385 L = 26: GOSUB 900: J = Z(P): GOSUB 930: J = Z1(P): GOSUB 960

1390 NEXT P
	CLS

1391 COLOR 14
	PRINT "      BATTING LEADERS  (MIN 3.1 PA/G)": COLOR 15
	PRINT "WHO'S HOT                   AVG   AB    AND WHO'S NOT                AVG   AB"

1392 FOR P = 1 TO 19
	PRINT Z2$(P) + ", " + Z3$(P); TAB(28); USING ".###  ###"; Z(P); Z1(P);
	PRINT TAB(41); Z2$(P + 238) + ", " + Z3$(P + 238); TAB(69);
	PRINT USING ".###  ###"; Z(P + 238); Z1(P + 238)
	NEXT
	PRINT

1410 FOR P = 21 TO 30: P$(P) = "": IF Z2$(P) = "" THEN 1413

1411 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1412 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1413 NEXT P

1414 GOSUB 7900: CLS : COLOR 14: PRINT "      HITS": COLOR 15

1415 FOR P = 21 TO 30: PRINT "      "; P$(P): NEXT P: PRINT

1416 REM

1417 FOR P = 32 TO 41: P$(P) = "": IF Z2$(P) = "" THEN 1420

1418 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1419 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1420 NEXT P

1421 COLOR 14: PRINT "      DOUBLES": COLOR 15

1422 FOR P = 32 TO 41: PRINT "      "; P$(P): NEXT P

1423 FOR P = 43 TO 52: P$(P) = "": IF Z2$(P) = "" THEN 1426

1424 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1425 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1426 NEXT P

1427 COLOR 14: LOCATE 1, 40: PRINT "      TRIPLES": COLOR 15

1428 FOR P = 43 TO 52: LOCATE (P - 41), 40: PRINT "      "; P$(P): NEXT P: PRINT

1429 FOR P = 54 TO 63: P$(P) = "": IF Z2$(P) = "" THEN 1432

1430 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1431 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1432 NEXT P

1433 COLOR 14: LOCATE 13, 52: PRINT "   HOME RUNS": COLOR 15

1434 FOR P = 54 TO 63: LOCATE (P - 40), 40: PRINT "      "; P$(P): NEXT P

1435 GOSUB 7900

1440 FOR P = 65 TO 73: P$(P) = "": IF Z2$(P) = "" THEN 1443

1441 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1442 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1443 NEXT P

1444 CLS : COLOR 14: PRINT "      RUNS": COLOR 15

1445 FOR P = 65 TO 73: PRINT "      "; P$(P): NEXT P: PRINT

1446 FOR P = 76 TO 84: P$(P) = "": IF Z2$(P) = "" THEN 1449

1447 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1448 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1449 NEXT P

1450 PRINT : COLOR 14: PRINT "      RUNS BATTED IN": COLOR 15

1451 FOR P = 76 TO 84: PRINT "      "; P$(P): NEXT P

1452 FOR P = 87 TO 96: P$(P) = "": IF Z2$(P) = "" OR Z2$(P) = "" THEN 1455

1453 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1454 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1455 NEXT P

1456 COLOR 14: LOCATE 1, 40: PRINT "      STOLEN BASES": COLOR 15

1457 FOR P = 87 TO 96: LOCATE (P - 85), 40: PRINT "      "; P$(P): NEXT P

1458 REM

1459 FOR P = 98 TO 107: P$(P) = ""
	IF Z2$(P) = "" THEN 1463

1460 P$(P) = Z2$(P)
	IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1462 L = 26: GOSUB 900: J = Z(P): GOSUB 930

1463 NEXT P

1464 COLOR 14: LOCATE 13, 40: PRINT "      SLUGGING PCT LEADERS": COLOR 15

1465 FOR P = 99 TO 108: LOCATE (P - 85), 40: PRINT "      "; P$(P): NEXT P: GOSUB 7900

1466 FOR P = 110 TO 119
	P$(P) = ""
	IF Z2$(P) = "" THEN 1470

1467 P$(P) = Z2$(P)
	IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1469 L = 26: GOSUB 900: J = Z(P): GOSUB 930

1470 NEXT P: CLS

1471 COLOR 14: PRINT "ON BASE PCT LEADERS": COLOR 15

1472 FOR P = 110 TO 119
	PRINT P$(P)
	NEXT P
	PRINT

1473 FOR P = 217 TO 226: P$(P) = "": IF Z2$(P) = "" THEN 1476

1474 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

1475 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

1476 NEXT P

1477 COLOR 14: LOCATE 1, 41: PRINT "BASE ON BALLS": COLOR 15

1478 FOR P = 217 TO 226: LOCATE (P - 215), 41: PRINT P$(P): NEXT P
	LOCATE 13, 1
	COLOR 14: PRINT "CURRENT HITTING STREAK": COLOR 15
	FOR P = 279 TO 288
	PRINT Z2$(P)+", "+Z3$(P); TAB(28); USING "###";Z(P)
	NEXT P
	LOCATE 13, 41
	COLOR 14: PRINT "LONGEST HITTING STREAK": COLOR 15
	FOR P = 289 TO 298
	LOCATE, 41:PRINT Z2$(P)+", "+Z3$(P); TAB(68); USING "###";Z(P)
	NEXT P
	

1479 GOSUB 7900

2500 FOR P = 120 TO 138: P$(P) = "": IF Z2$(P) = "" THEN 2510

2501 P$(P) = Z2$(P)

2502 IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2505 L = 26: GOSUB 900: J = Z(P): GOSUB 920: J = INT(Z1(P)): GOSUB 960

2510 NEXT P

2511 CLS : COLOR 14
	PRINT "       ERA LEADERS (MIN 1 IP/G)"
	COLOR 15
	PRINT "WHO'S HOT                     ERA   IP  WHO'S NOT                      ERA  IP"

2515 FOR P = 121 TO 139
	PRINT Z2$(P) + ", " + Z3$(P); TAB(30); USING "#.##  ###"; Z(P); Z1(P);
	PRINT TAB(41);
	PRINT Z2$(P + 138) + ", " + Z3$(P + 138); TAB(70); USING "##.## ###"; Z(P + 138); Z1(P + 138)
	NEXT
	  GOSUB 7900

2590 FOR P = 140 TO 149: P$(P) = "": IF Z(P) = Z(150) AND Z1(P) = Z1(150) THEN 2602

2595 P$(P) = P$(P) + Z2$(P): IF Z3$(P) <> "" THEN P$(P) = P$(P) + ", " + Z3$(P)

2600 L = 26: GOSUB 900: J = Z(P): W = 2: GOSUB 940: P$(P) = P$(P) + "-": J = Z1(P)

2601 GOSUB 940

2602 NEXT P

2605 CLS : COLOR 14:
	PRINT "      PITCHING RECORD": COLOR 15

2610 FOR P = 140 TO 149: PRINT "      "; P$(P)

2615 NEXT: PRINT

2750 FOR P = 151 TO 160: P$(P) = "": IF Z2$(P) = "*" THEN 2753

2751 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2752 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

2753 NEXT P

2754 COLOR 14: PRINT "      VICTORIES": COLOR 15

2755 FOR P = 151 TO 160: PRINT "      "; P$(P): NEXT P

2756 FOR P = 162 TO 171: P$(P) = "": IF Z2$(P) = "" THEN 2759

2757 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2758 L = 26: GOSUB 900: J = INT(Z(P)): W = 3: GOSUB 940

2759 NEXT P

2760 LOCATE 1, 40: COLOR 14: PRINT "      INNINGS PITCHED": COLOR 15

2761 FOR P = 162 TO 171: LOCATE (P - 160), 40: PRINT "      "; P$(P): NEXT P

2762 FOR P = 173 TO 182: P$(P) = "": IF Z2$(P) = "" THEN 2765

2763 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2764 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

2765 NEXT P

2766 COLOR 14: LOCATE 13, 40: PRINT "      STRIKEOUTS     ": COLOR 15

2767 FOR P = 173 TO 182: LOCATE (P - 159), 40: PRINT "      "; P$(P): NEXT P: GOSUB 7900

2770 FOR P = 184 TO 193: P$(P) = "": IF Z2$(P) = "" THEN 2773

2771 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2772 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

2773 NEXT P

2774 CLS : COLOR 14: PRINT "      COMPLETE GAMES ": COLOR 15

2775 FOR P = 184 TO 193: PRINT "      "; P$(P): NEXT P: PRINT

2776 FOR P = 195 TO 204: P$(P) = "": IF Z2$(P) = "" OR Z2$(P) = "" THEN 2779

2777 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2778 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

2779 NEXT P

2780 COLOR 14: PRINT "      SHUTOUTS       ": COLOR 15

2781 FOR P = 195 TO 204: PRINT "      "; P$(P): NEXT P

2786 FOR P = 206 TO 215: P$(P) = "": IF Z2$(P) = "" THEN 2789

2787 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2788 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

2789 NEXT P

2790 LOCATE 1, 40: COLOR 14: PRINT "      SAVES          ": COLOR 15

2791 FOR P = 206 TO 215: LOCATE (P - 204), 40: PRINT "      "; P$(P): NEXT P

2792 COLOR 14: LOCATE 13, 40: PRINT "      APPEARANCES    ": COLOR 15

2793 FOR P = 228 TO 237: P$(P) = "": IF Z2$(P) = "" THEN 2796

2794 P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

2795 L = 26: GOSUB 900: J = Z(P): W = 3: GOSUB 940

2796 NEXT P

2797 FOR P = 228 TO 237: LOCATE (P - 214), 40: PRINT "      "; P$(P): NEXT P

2800 GOSUB 7900

2850 CLS : COLOR 12: PRINT "                                 CLUB BATTING   ": COLOR 14

2851 PRINT "CLUB         AVG   AB    R    H   2B   3B  HR   RBI   BB   SO  SB  CS   E": COLOR 15
	H = 0: AB = 0: R = 0: B2% = 0: B3 = 0: HR = 0: RBI = 0: BB = 0: SO = 0: SB = 0: CS = 0: E = 0

2852 FOR X = 1 TO 40

2853 IF BA$(X) = "" OR BA$(X) = "*" THEN 2860

2854 PRINT LEFT$(BA$(X), 11); TAB(13);

2855 PRINT USING ".### #### #### ####  ###  ### ###  #### #### #### ### ### ###"; OFT(X, 1); OFT(X, 3); OFT(X, 5); OFT(X, 4); OFT(X, 7); OFT(X, 8); OFT(X, 9); OFT(X, 6); OFT(X, 10); OFT(X, 11); OFT(X, 12); OFT(X, 13); OFT(X, 14)
	ATBATS = ATBATS + OFT(X, 3): H = H + OFT(X, 4): R = R + OFT(X, 5)
	B2 = B2 + OFT(X, 7): B3 = B3 + OFT(X, 8): HR = HR + OFT(X, 9)
	RBI = RBI + OFT(X, 6): BB = BB + OFT(X, 10): SO = SO + OFT(X, 11): SB = SB + OFT(X, 12): CS = CS + OFT(X, 13): E = E + OFT(X, 14)

2860 NEXT
	PRINT
	PRINT "LG TOTALS"; TAB(13); USING ".###      ####      ####  ###      ####     #####    ####"; H / ATBATS; R; B2; B3; RBI; SO; CS
	PRINT TAB(17); USING "#####     #####          ####       ####     ####    ####"; ATBATS; H; HR; BB; SB; E

2861 GOSUB 7900

2870 CLS : COLOR 12: PRINT "                                 CLUB PITCHING  ": COLOR 14

2871 PRINT "CLUB          ERA   G  CG  SHO  SV    IP     H     R    ER     BB     SO": COLOR 15

2872 R = 0: G = 0: IP = 0: FOR X = 1 TO 40

2873 IF ER$(X) = "" OR ER$(X) = "*" THEN 2880
	R = R + DFT(X, 5)
	G = G + DFT(X, 12)
	INNINGS = INNINGS + DFT(X, 2)

2874 PRINT LEFT$(ER$(X), 11); TAB(13);

2875 DFT(X, 1) = INT(DFT(X, 1) * 100) / 100
	PRINT USING "##.##"; DFT(X, 1);

2876 PRINT TAB(19); USING "### ### ###  ###  ####  ####  ####  ####"; DFT(X, 11); DFT(X, 12); DFT(X, 13); DFT(X, 14); CINT(DFT(X, 2) / 3); DFT(X, 3); DFT(X, 4); DFT(X, 5);

2877 PRINT USING "   ####   ####"; DFT(X, 6); DFT(X, 7)

2880 NEXT
	PRINT TAB(13); USING "##.##"; R / (INNINGS / 3) * 9
	'PRINT "COMPLETE GAMES: "; G
	'PRINT "APPEARANCES: "; APP

2881 GOSUB 7900

2900 RUN "HELLO.EXE"

5000 CP$ = "": L = LEN(LW$): FOR Z = 1 TO L: ALP = ASC(MID$(LW$, Z, 1))

5010 IF ALP > 64 AND ALP < 96 THEN CP$ = CP$ + CHR$(ALP + 32): GOTO 5030

5020 CP$ = CP$ + CHR$(ALP)

5030 NEXT Z: RETURN

7900 HZ = 1: VT = 24

8000 LOCATE VT, HZ: PRINT "                          "; :

9005 QK$ = GetKeyPress$

9010 RETURN

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
