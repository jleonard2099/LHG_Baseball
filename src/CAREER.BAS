'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Career Files"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim B$(100), P$(80), P1%(80), B1%(100), B0%(100, 23), P0%(80, 42)
Dim T(22), T1(23), BR$(100), PR$(80), PR1%(80), BR1%(100)
Dim BR0%(100, 17), PR0%(80, 16), TR(14), TR1(15)
Dim BLYR(15), BL$(15), BLL(15) As Single, PLYR(15), PL$(15), PLL(15) As Single
Dim D$(3), menuItems$(8), AVG As Single, SLG As Single, OBP As Single, EV As Single
ReDim PRS%(15, 80, 17), BRS%(15, 100, 18)

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, D$(I)
Next
Close #1

2 Cls: CH = 0: TMR = 4175: TMR1 = 6875: NB = -1: NP = -1: JX = 0: VV = 0

3 Call TITLE
menuItems$(1) = "CREATE AND MERGE INITIAL CAREER STAT STATS"
menuItems$(2) = "MERGE/UPDATE CAREER STATS                 "
menuItems$(3) = "VIEW/PRINT CAREER STATS                   "
menuItems$(4) = "EDIT CAREER STATS                         "
menuItems$(5) = "VIEW/PRINT TEAM CAREER LEADERS (SEASON)   "
menuItems$(6) = "UPDATE TEAM CAREER LEADERS (CAREER)       "
menuItems$(7) = "RETURN TO MAIN MENU                       "
SELECTION% = 1
Count% = 7
Start% = 1
SCANCODE% = 7
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 17

Call box0(5, 16, 13, 59, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I = SELECTION%
If I = 1 Then 1900
If I = 2 Then 2000
If I = 3 Then Run "CARVIEW.EXE"
If I = 4 Then Run "CAREDIT.EXE"
If I = 5 Then 3000
If I = 6 Then Run "CARLDR.EXE"
If I = 7 Then GoSub 993

17 CH = 1

18 '

21 If CH = 1 Then 1150

22 Color 12, 0: Cls: Locate 2, 5

30 Color 10

45 For J = 15 To 18: Locate J: Print "                                                                    "

46 Next

47 Color 11: Locate 15, 5: Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; Z$: Z$ = Left$(Z$, 8) + ".car"
For XX = 1 To Len(Z$)
    If Mid$(Z$, XX, 1) = " " Then Mid$(Z$, XX, 1) = "_"
Next XX

48 Locate , 5: Input "ENTER NAME OF STAT FILE TO ADD (EX. - WASHINGT.60)"; Z1$
If Len(Z1$) > 11 Then Z1$ = Left$(Z1$, 8) + Right$(Z1$, 3)

49 Locate , 5: Input "ENTER YEAR TO ADD (EX. - 1960)"; Y%

50 Locate , 5: Print "ANY CHANGE (YN)"
60 I$ = GetKeyPress$

70 If UCase$(I$) = "Y" Then 45

75 VV = 1: Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
76 I$ = GetKeyPress$

77 If UCase$(I$) = "Y" Then JX = 1

78 If UCase$(I$) <> "Y" Then JX = 0

100 'GoSub 1000

200 Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"
If Not _FileExists(D$(1) + Z1$) Then 900

220 Open D$(1) + Z1$ For Input As #1

222 Input #1, A$: Input #1, W2%: Input #1, L2%

223 For I = 0 To 22: Input #1, B$(I): Input #1, B1%(I): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I

224 For I = 0 To 21: Input #1, P$(I): Input #1, P1%(I): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I

225 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I1: Next I

226 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I1: Next I

227 Input #1, D0%: Input #1, D1%
For I = 0 To 21: Input #1, T(I): Next I
For I = 0 To 22: Input #1, T1(I): Next I

265 Close #1

400 For I = 0 To 99

    401 N = 1

    402 If B$(I) = "END" Or B$(I) = "XXX" Or B$(I) = "" Then 404 Else GoTo 403

    403 NB = NB + 1: GoTo 408

    404 B$(I) = ""

    408 For J = 0 To 15: BRS%(N, I, J) = B0%(I, J)

    409 Next J

    410 BRS%(N, I, 16) = 1: BRS%(N, I, 17) = Y%

411 Next I

420 For I = 0 To 79

    421 If P$(I) = "END" Or P$(I) = "XXX" Or P$(I) = "" Then 424 Else GoTo 423

    423 NP = NP + 1: GoTo 428

    424 P$(I) = ""

    428 For J = 0 To 14: PRS%(N, I, J) = P0%(I, J)

    429 Next J

    430 PRS%(N, I, 15) = 1: PRS%(N, I, 16) = Y%

431 Next I

700 If JX = 1 Then 704

701 If JX = 0 Then 705

704 GoSub 2080

705 'GoSub 1100

706 Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

707 Open D$(1) + Z$ For Output As #1

708 Write #1, NB, NP

710 For I = 0 To 99: Write #1, B$(I): Next

715 For I = 0 To 79: Write #1, P$(I): Next

720 For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Write #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I: Next J

725 For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Write #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I: Next J

740 Close #1

800 GoTo 2

900 Cls: Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"

901 Print: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

993 Run "HELLO.EXE"

1150 Color 12, 0: Cls: Locate 2, 5

1175 For J = 15 To 18: Locate J: Print "                                                                     "

1176 Next

1177 Color 11
Locate 15, 5
Input "ENTER NAME OF CAREER STAT FILE (EX. - MINNESOT)"; Z$
Z$ = Left$(Z$, 8) + ".car"
For XX = 1 To Len(Z$)
    If Mid$(Z$, XX, 1) = " " Then Mid$(Z$, XX, 1) = "_"
Next XX

1178 Locate , 5: Input "ENTER NAME OF FILE TO ADD (EX. - MINNESOT.61)"; Z1$
If Len(Z1$) > 11 Then Z1$ = Left$(Z1$, 8) + Right$(Z1$, 3)

1179 Locate , 5: Input "ENTER YEAR TO ADD (EX. - 1961)"; Y%

1180 Locate , 5: Print "ANY CHANGE (YN)"
1181 I$ = GetKeyPress$

1182 If UCase$(I$) = "Y" Then 1175

1185 VV = 0
Locate , 5
Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
1186 I$ = GetKeyPress$
JX = 0

1187 If UCase$(I$) = "Y" Then JX = 1

1190 'GoSub 1100

1191 Locate 20, 5
Color 15
Print "LOADING CAREER LEADER FILE . . . . PLEASE WAIT"

1200 If Not _FileExists(D$(1) + Z$) Then 1300

1201 Open D$(1) + Z$ For Input As #1

1208 Input #1, NB, NP

1210 For I = 0 To 99: Input #1, B$(I): Next

1215 For I = 0 To 79: Input #1, P$(I): Next

1220 For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR1 = TMR1 - 1: Color 9: Print Using "####"; TMR1: Next I: Next J

1225 For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR1 = TMR1 - 1: Color 9: Print Using "####"; TMR1: Next I: Next J

1265 Close #1

1300 'GoSub 1000

1305 Locate 20, 5
Color 15
Print "LOADING STATS FILE . . . . PLEASE WAIT"
If Not _FileExists(D$(1) + Z1$) Then 900

1320 Open D$(1) + Z1$ For Input As #1

1322 Input #1, A$: Input #1, W2%: Input #1, L2%

1323 For I = 0 To 22: Input #1, BR$(I): Input #1, B1%(I): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I

1324 For I = 0 To 21: Input #1, PR$(I): Input #1, P1%(I): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I

1325 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I1: Next I

1326 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Locate 21, 35: TMR = TMR - 1: Color 9: Print Using "####"; TMR: Next I1: Next I

1327 Input #1, D0%: Input #1, D1%
For I = 0 To 21: Input #1, T(I): Next I
For I = 0 To 22: Input #1, T1(I): Next I

1365 Close #1

1400 For I = 0 To 22

    1408 RR = 0: CHK = 0

    1409 For J = 0 To 99

        1410 If UCase$(BR$(I)) = UCase$(B$(J)) Then 1411 Else GoTo 1423

        1411 For W = 1 To 15: If BRS%(W, J, 17) = Y% Then 1412 Else GoTo 1413

            1412 CHK = CHK + 1

        1413 Next W

        1414 If CHK > 0 Then 1418 Else GoTo 1419

1418 N = BRS%(1, J, 16) + BRS%(2, J, 16) + BRS%(3, J, 16) + BRS%(4, J, 16) + BRS%(5, J, 16) + BRS%(6, J, 16) + BRS%(7, J, 16) + BRS%(8, J, 16) + BRS%(9, J, 16) + BRS%(10, J, 16) + BRS%(11, J, 16) + BRS%(12, J, 16) + BRS%(13, J, 16) + BRS%(14, J, 16) _
+ BRS%(15, J, 16): GOTO 1420

1419 N = BRS%(1, J, 16) + BRS%(2, J, 16) + BRS%(3, J, 16) + BRS%(4, J, 16) + BRS%(5, J, 16) + BRS%(6, J, 16) + BRS%(7, J, 16) + BRS%(8, J, 16) + BRS%(9, J, 16) + BRS%(10, J, 16) + BRS%(11, J, 16) + BRS%(12, J, 16) + BRS%(13, J, 16) + BRS%(14, J, 16) _
+ BRS%(15, J, 16) + 1

        1420 For K = 0 To 15: BRS%(N, J, K) = B0%(I, K): Next K

        1421 BRS%(N, J, 16) = 1: BRS%(N, J, 17) = Y%

        1422 GoTo 1430

        1423 RR = RR + 1

        1424 If RR = 100 Then GoSub 1450

    1425 Next J

1430 Next I

1431 GoTo 1500

1450 If BR$(I) = "END" Or BR$(I) = "XXX" Or BR$(I) = "" Then 1456 Else GoTo 1451

1451 NB = NB + 1

1452 B$(NB) = BR$(I)

1453 N = 1

1454 For K = 0 To 15: BRS%(N, NB, K) = B0%(I, K): Next

1455 BRS%(N, NB, 16) = 1: BRS%(N, NB, 17) = Y%

1456 Return

1500 For I = 0 To 21

    1508 RRP = 0: CHK = 0

    1509 For J = 0 To 79

        1510 If UCase$(PR$(I)) = UCase$(P$(J)) Then 1511 Else GoTo 1523

        1511 For W = 1 To 15: If PRS%(W, J, 16) = Y% Then 1512 Else GoTo 1513

            1512 CHK = CHK + 1

        1513 Next W

        1514 If CHK > 0 Then 1518 Else GoTo 1519

1518 N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) _
+ PRS%(15, J, 15): GOTO 1520

1519 N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) _
+ PRS%(15, J, 15) + 1

        1520 For K = 0 To 14: PRS%(N, J, K) = P0%(I, K): Next K

        1521 PRS%(N, J, 15) = 1: PRS%(N, J, 16) = Y%

        1522 GoTo 1530

        1523 RRP = RRP + 1

        1524 If RRP = 80 Then GoSub 1550

    1525 Next J

1530 Next I

1531 GoTo 1700

1550 If PR$(I) = "END" Or PR$(I) = "XXX" Or PR$(I) = "" Then 1556 Else GoTo 1551

1551 NP = NP + 1

1552 P$(NP) = PR$(I)

1553 N = 1

1554 For K = 0 To 14: PRS%(N, NP, K) = P0%(I, K): Next

1555 PRS%(N, NP, 15) = 1: PRS%(N, NP, 16) = Y%

1556 Return

1700 If JX = 1 Then 1704

1701 If JX = 0 Then 1705

1704 GoSub 2080

1705 'GoSub 1100

1706 Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

1707 Open D$(1) + Z$ For Output As #1

1708 Write #1, NB, NP

1710 For I = 0 To 99: Write #1, B$(I): Next

1715 For I = 0 To 79: Write #1, P$(I): Next

1720 For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Write #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR1 = TMR1 - 1: Color 9: Print Using "####"; TMR1: Next I: Next J

1725 For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Write #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR1 = TMR1 - 1: Color 9: Print Using "####"; TMR1: Next I: Next J

1740 Close #1

1800 GoTo 2

1900 Cls: Color 14: Locate 5, 27: Print " - - - C A U T I O N - - -"

1901 Color 15: Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"

1902 Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"

1903 Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"

1904 Locate , 18: Print "            USE OPTION #2."

1905 Print: Print: Print

1906 Color 12: Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
1907 I$ = GetKeyPress$

1908 If UCase$(I$) = "Y" Then GoTo 18

1909 If UCase$(I$) = "N" Then GoTo 2

1910 If UCase$(I$) <> "Y" Or UCase$(I$) <> "N" Then 1907

2000 Cls: Color 14: Locate 5, 27: Print "- - - C A U T I O N - - -"

2001 Color 15: Locate 8, 8: Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"

2002 Print "FILE.  IF YOU ARE CREATING A NEW CAREER FILE, USE OPTION #1.  IF YOU ARE ADDING"

2003 Print "AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER SLOTS (XXX, END,"

2006 Print "ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."

2007 Locate , 8: Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"

2009 Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"

2011 Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"

2012 Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"

2013 Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"

2014 Print "MERGING, OR YOU COULD CHANGE HIM TO 'B.WILLIAMS' IN THE CAREER FILE BY USING"

2015 Print "THE MODIFY REPLAY STATS OPTION FROM THE MAIN MENU."

2016 Print: Print

2017 Color 12: Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
2018 I$ = GetKeyPress$

2019 If UCase$(I$) = "Y" Then 17

2020 GoTo 2

2080 GoSub 5000
If Not _FileExists(D$(1) + Left$(Z$, JX) + ".CLD") Then 2100
Open D$(1) + Left$(Z$, JX) + ".CLD" For Input As #1
For I = 0 To 14: Input #1, BLYR(I), BL$(I), BLL(I): Next
For I = 0 To 14: Input #1, PLYR(I), PL$(I), PLL(I): Next
Close #1

2100 For I = 0 To 99

    2101 If B$(I) = "" Or B$(I) = "XXX" Or B$(I) = "END" Then 2300

    2102 For J = 1 To 15
        If BRS%(J, I, 17) = 0 Then 2250 Else GoTo 2103

        2103 If BRS%(J, I, 0) > BLL(0) Then 2104 Else GoTo 2113

        2104 BLL(0) = BRS%(J, I, 0): BL$(0) = B$(I): BLYR(0) = BRS%(J, I, 17)

        2113 If BRS%(J, I, 2) > BLL(1) Then 2114 Else GoTo 2123

        2114 BLL(1) = BRS%(J, I, 2): BL$(1) = B$(I): BLYR(1) = BRS%(J, I, 17)

        2123 If BRS%(J, I, 1) > BLL(2) Then 2124 Else GoTo 2133

        2124 BLL(2) = BRS%(J, I, 1): BL$(2) = B$(I): BLYR(2) = BRS%(J, I, 17)

        2133 If BRS%(J, I, 4) > BLL(3) Then 2134 Else GoTo 2143

        2134 BLL(3) = BRS%(J, I, 4): BL$(3) = B$(I): BLYR(3) = BRS%(J, I, 17)

        2143 If BRS%(J, I, 5) > BLL(4) Then 2144 Else GoTo 2153

        2144 BLL(4) = BRS%(J, I, 5): BL$(4) = B$(I): BLYR(4) = BRS%(J, I, 17)

        2153 If BRS%(J, I, 6) > BLL(5) Then 2154 Else GoTo 2163

        2154 BLL(5) = BRS%(J, I, 6): BL$(5) = B$(I): BLYR(5) = BRS%(J, I, 17)

        2163 If BRS%(J, I, 3) > BLL(6) Then 2164 Else GoTo 2173

        2164 BLL(6) = BRS%(J, I, 3): BL$(6) = B$(I): BLYR(6) = BRS%(J, I, 17)

        2173 If BRS%(J, I, 7) > BLL(7) Then 2174 Else GoTo 2183

        2174 BLL(7) = BRS%(J, I, 7): BL$(7) = B$(I): BLYR(7) = BRS%(J, I, 17)

        2183 If BRS%(J, I, 8) > BLL(8) Then 2184 Else GoTo 2193

        2184 BLL(8) = BRS%(J, I, 8): BL$(8) = B$(I): BLYR(8) = BRS%(J, I, 17)

        2193 If BRS%(J, I, 9) > BLL(9) Then 2194 Else GoTo 2203

        2194 BLL(9) = BRS%(J, I, 9): BL$(9) = B$(I): BLYR(9) = BRS%(J, I, 17)

        2203 If BRS%(J, I, 10) > BLL(10) Then 2204 Else GoTo 2213

        2204 BLL(10) = BRS%(J, I, 10): BL$(10) = B$(I): BLYR(10) = BRS%(J, I, 17)

        2213 If BRS%(J, I, 11) > BLL(11) Then 2214 Else GoTo 2223

        2214 BLL(11) = BRS%(J, I, 11): BL$(11) = B$(I): BLYR(11) = BRS%(J, I, 17)

        2223 If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then 2224 Else GoTo 2233

        2224 AVG = BRS%(J, I, 1) / BRS%(J, I, 0)

        2225 If AVG > BLL(12) Then 2226 Else GoTo 2233

        2226 BLL(12) = AVG: BL$(12) = B$(I): BLYR(12) = BRS%(J, I, 17)

        2233 If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then 2234 Else GoTo 2243

        2234 SLG = (BRS%(J, I, 1) + BRS%(J, I, 4) + BRS%(J, I, 5) * 2 + BRS%(J, I, 6) * 3) / BRS%(J, I, 0)

        2235 If SLG > BLL(13) Then 2236 Else GoTo 2243

        2236 BLL(13) = SLG: BL$(13) = B$(I): BLYR(13) = BRS%(J, I, 17)

        2243 If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then 2244 Else GoTo 2250

        2244 OBP = (BRS%(J, I, 1) + BRS%(J, I, 7)) / (BRS%(J, I, 7) + BRS%(J, I, 0))

        2245 If OBP > BLL(14) Then 2246 Else GoTo 2250

        2246 BLL(14) = OBP: BL$(14) = B$(I): BLYR(14) = BRS%(J, I, 17)

    2250 Next J

2300 Next I

2400 For I = 0 To 79

    2401 If P$(I) = "" Or P$(I) = "XXX" Or P$(I) = "END" Then 2600

    2402 For J = 1 To 15
        If PRS%(J, I, 16) = 0 Then 2550 Else GoTo 2403

        2403 If PRS%(J, I, 6) > PLL(0) Then 2404 Else GoTo 2413

        2404 PLL(0) = PRS%(J, I, 6): PL$(0) = P$(I): PLYR(0) = PRS%(J, I, 16)

        2413 If PRS%(J, I, 7) > PLL(1) Then 2414 Else GoTo 2423

        2414 PLL(1) = PRS%(J, I, 7): PL$(1) = P$(I): PLYR(1) = PRS%(J, I, 16)

        2423 If PRS%(J, I, 12) > PLL(2) Then 2424 Else GoTo 2433

        2424 PLL(2) = PRS%(J, I, 12): PL$(2) = P$(I): PLYR(2) = PRS%(J, I, 16)

        2433 If PRS%(J, I, 0) / 3 > PLL(3) Then 2434 Else GoTo 2443

        2434 PLL(3) = PRS%(J, I, 0) / 3: PL$(3) = P$(I): PLYR(3) = PRS%(J, I, 16)

        2443 If PRS%(J, I, 1) > PLL(4) Then 2444 Else GoTo 2453

        2444 PLL(4) = PRS%(J, I, 1): PL$(4) = P$(I): PLYR(4) = PRS%(J, I, 16)

        2453 If PRS%(J, I, 4) > PLL(5) Then 2454 Else GoTo 2463

        2454 PLL(5) = PRS%(J, I, 4): PL$(5) = P$(I): PLYR(5) = PRS%(J, I, 16)

        2463 If PRS%(J, I, 5) > PLL(6) Then 2464 Else GoTo 2473

        2464 PLL(6) = PRS%(J, I, 5): PL$(6) = P$(I): PLYR(6) = PRS%(J, I, 16)

        2473 If PRS%(J, I, 0) / 3 > 162 Then 2474 Else GoTo 2483

        2474 EV = (PRS%(J, I, 3) * 9) / (PRS%(J, I, 0) / 3)

        2475 If PLL(7) = 0 Then PLL(7) = 1000

        2476 If EV < PLL(7) Then 2477 Else GoTo 2483

        2477 PLL(7) = EV: PL$(7) = P$(I): PLYR(7) = PRS%(J, I, 16)

        2483 If PRS%(J, I, 8) > PLL(8) Then 2484 Else GoTo 2493

        2484 PLL(8) = PRS%(J, I, 8): PL$(8) = P$(I): PLYR(8) = PRS%(J, I, 16)

        2493 If PRS%(J, I, 10) > PLL(9) Then 2494 Else GoTo 2503

        2494 PLL(9) = PRS%(J, I, 10): PL$(9) = P$(I): PLYR(9) = PRS%(J, I, 16)

        2503 If PRS%(J, I, 2) > PLL(10) Then 2504 Else GoTo 2513

        2504 PLL(10) = PRS%(J, I, 2): PL$(10) = P$(I): PLYR(10) = PRS%(J, I, 16)

        2513 If PRS%(J, I, 3) > PLL(11) Then 2514 Else GoTo 2523

        2514 PLL(11) = PRS%(J, I, 3): PL$(11) = P$(I): PLYR(11) = PRS%(J, I, 16)

        2523 If PRS%(J, I, 11) > PLL(12) Then 2524 Else GoTo 2533

        2524 PLL(12) = PRS%(J, I, 11): PL$(12) = P$(I): PLYR(12) = PRS%(J, I, 16)

        2533 If PRS%(J, I, 14) > PLL(13) Then 2534 Else GoTo 2543

        2534 PLL(13) = PRS%(J, I, 14): PL$(13) = P$(I): PLYR(13) = PRS%(J, I, 16)

        2543 If PRS%(J, I, 13) > PLL(14) Then 2544 Else GoTo 2550

        2544 PLL(14) = PRS%(J, I, 13): PL$(14) = P$(I): PLYR(14) = PRS%(J, I, 16)

    2550 Next J

2600 Next I

2800 GoSub 5000
Open D$(1) + Left$(Z$, JX) + ".CLD" For Output As #1

2811 For I = 0 To 14: Write #1, BLYR(I), BL$(I), BLL(I): Next

2812 For I = 0 To 14: Write #1, PLYR(I), PL$(I), PLL(I): Next

2813 Close #1

2900 Return

3000 Cls
Spec$ = D$(2) + "*.CLD"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim conferenceLeaderFiles$(0 To Count%)
For X = 1 To Count%
    conferenceLeaderFiles$(X) = Space$(12)
Next
conferenceLeaderFiles$(0) = Spec$
Call ReadFile(conferenceLeaderFiles$())
'CALL SortStr(VARPTR(conferenceLeaderFiles$(1)), Count, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEADERS TO VIEW": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu conferenceLeaderFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 2
targetFile$ = conferenceLeaderFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 ZZ$ = RTrim$(targetFile$)

3100 Open D$(1) + ZZ$ For Input As #1
For I = 0 To 14: Input #1, BLYR(I), BL$(I), BLL(I): Next
For I = 0 To 14: Input #1, PLYR(I), PL$(I), PLL(I): Next
Close #1

3150 Locate 15, 10: Color 12: Print "V";: Color 7: Print "IEW OR ";: Color 12: Print "P";: Color 7: Print "RINT"
3151 I$ = GetKeyPress$

3153 If UCase$(I$) = "P" Then 3500

3200 Cls

3250 Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

3251 Print

3252 Color 3: Print "AT BATS";: Color 7: Print Tab(23); Using "###"; BLL(0);: Print Tab(37); BL$(0);: Print Tab(56); BLYR(0)

3253 Color 3: Print "RUNS SCORED";: Color 7: Print Tab(23); Using "###"; BLL(1);: Print Tab(37); BL$(1);: Print Tab(56); BLYR(1)

3254 Color 3: Print "HITS";: Color 7: Print Tab(23); Using "###"; BLL(2);: Print Tab(37); BL$(2);: Print Tab(56); BLYR(2)

3255 Color 3: Print "DOUBLES";: Color 7: Print Tab(23); Using "###"; BLL(3);: Print Tab(37); BL$(3);: Print Tab(56); BLYR(3)

3256 Color 3: Print "TRIPLES";: Color 7: Print Tab(23); Using "###"; BLL(4);: Print Tab(37); BL$(4);: Print Tab(56); BLYR(4)

3257 Color 3: Print "HOME RUNS";: Color 7: Print Tab(23); Using "###"; BLL(5);: Print Tab(37); BL$(5);: Print Tab(56); BLYR(5)

3258 Color 3: Print "RUNS BATTED IN";: Color 7: Print Tab(23); Using "###"; BLL(6);: Print Tab(37); BL$(6);: Print Tab(56); BLYR(6)

3259 Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; BLL(7);: Print Tab(37); BL$(7);: Print Tab(56); BLYR(7)

3260 Color 3: Print "STRIKEOUTS";: Color 7: Print Tab(23); Using "###"; BLL(8);: Print Tab(37); BL$(8);: Print Tab(56); BLYR(8)

3261 Color 3: Print "STEALS";: Color 7: Print Tab(23); Using "###"; BLL(9);: Print Tab(37); BL$(9);: Print Tab(56); BLYR(9)

3262 Color 3: Print "CAUGHT STEALING";: Color 7: Print Tab(23); Using "###"; BLL(10);: Print Tab(37); BL$(10);: Print Tab(56); BLYR(10)

3263 Color 3: Print "ERRORS";: Color 7: Print Tab(23); Using "###"; BLL(11);: Print Tab(37); BL$(11);: Print Tab(56); BLYR(11)

3264 Color 3: Print "BATTING AVERAGE";: Color 7: Print Tab(22); Using ".###"; BLL(12);: Print Tab(37); BL$(12);: Print Tab(56); BLYR(12)

3265 Color 3: Print "SLUGGING PCT.";: Color 7: Print Tab(22); Using ".###"; BLL(13);: Print Tab(37); BL$(13);: Print Tab(56); BLYR(13)

3266 Color 3: Print "ON BASE PCT.";: Color 7: Print Tab(22); Using ".###"; BLL(14);: Print Tab(37); BL$(14);: Print Tab(56); BLYR(14)

3270 Color 15: Locate 22: Print "HIT ANY KEY TO CONTINUE"
3271 I$ = GetKeyPress$

3300 Cls

3350 Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

3351 Print

3352 Color 3: Print "WINS";: Color 7: Print Tab(23); Using "###"; PLL(0);: Print Tab(37); PL$(0);: Print Tab(56); PLYR(0)

3353 Color 3: Print "LOSSES";: Color 7: Print Tab(23); Using "###"; PLL(1);: Print Tab(37); PL$(1);: Print Tab(56); PLYR(1)

3354 Color 3: Print "SAVES";: Color 7: Print Tab(23); Using "###"; PLL(2);: Print Tab(37); PL$(2);: Print Tab(56); PLYR(2)

3355 Color 3: Print "INNINGS PITCHED";: Color 7: Print Tab(23); Using "###"; PLL(3);:
If PLL(3) - (Int(PLL(3) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PLL(3) - (Int(PLL(3) / 3) * 3);:
Print Tab(37); PL$(3);: Print Tab(56); PLYR(3)

3356 Color 3: Print "HITS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL(4);: Print Tab(37); PL$(4);: Print Tab(56); PLYR(4)

3357 Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; PLL(5);: Print Tab(37); PL$(5);: Print Tab(56); PLYR(5)

3358 Color 3: Print "STRIKOUTS";: Color 7: Print Tab(23); Using "###"; PLL(6);: Print Tab(37); PL$(6);: Print Tab(56); PLYR(6)

3359 Color 3: Print "EARNED RUN AVERAGE";: Color 7: Print Tab(22); Using "#.##"; PLL(7);: Print Tab(37); PL$(7);: Print Tab(56); PLYR(7)

3360 Color 3: Print "GAMES";: Color 7: Print Tab(23); Using "###"; PLL(8);: Print Tab(37); PL$(8);: Print Tab(56); PLYR(8)

3361 Color 3: Print "COMPLETE GAMES";: Color 7: Print Tab(23); Using "###"; PLL(9);: Print Tab(37); PL$(9);: Print Tab(56); PLYR(9)

3362 Color 3: Print "RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL(10);: Print Tab(37); PL$(10);: Print Tab(56); PLYR(10)

3363 Color 3: Print "EARNED RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL(11);: Print Tab(37); PL$(11);: Print Tab(56); PLYR(11)

3364 Color 3: Print "SHUTOUTS";: Color 7: Print Tab(23); Using "###"; PLL(12);: Print Tab(37); PL$(12);: Print Tab(56); PLYR(12)

3365 Color 3: Print "HOME RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL(13);: Print Tab(37); PL$(13);: Print Tab(56); PLYR(13)

3366 Color 3: Print "DOUBLE PLAYS";: Color 7: Print Tab(23); Using "###"; PLL(14);: Print Tab(37); PL$(14);: Print Tab(56); PLYR(14)

3390 Color 15: Locate 22: Print "VIEW AGAIN (Y/N)"
3391 I$ = GetKeyPress$

3392 If UCase$(I$) = "Y" Then 3200

3393 GoTo 2

3500 LPrint "CATEGORY           RECORD           PLAYER              YEAR"

3551 LPrint

3552 LPrint "AT BATS";: LPrint Tab(23); Using "###"; BLL(0);: LPrint Tab(37); BL$(0);: LPrint Tab(56); BLYR(0)

3553 LPrint "RUNS SCORED";: LPrint Tab(23); Using "###"; BLL(1);: LPrint Tab(37); BL$(1);: LPrint Tab(56); BLYR(1)

3554 LPrint "HITS";: LPrint Tab(23); Using "###"; BLL(2);: LPrint Tab(37); BL$(2);: LPrint Tab(56); BLYR(2)

3555 LPrint "DOUBLES";: LPrint Tab(23); Using "###"; BLL(3);: LPrint Tab(37); BL$(3);: LPrint Tab(56); BLYR(3)

3556 LPrint "TRIPLES";: LPrint Tab(23); Using "###"; BLL(4);: LPrint Tab(37); BL$(4);: LPrint Tab(56); BLYR(4)

3557 LPrint "HOME RUNS";: LPrint Tab(23); Using "###"; BLL(5);: LPrint Tab(37); BL$(5);: LPrint Tab(56); BLYR(5)

3558 LPrint "RUNS BATTED IN";: LPrint Tab(23); Using "###"; BLL(6);: LPrint Tab(37); BL$(6);: LPrint Tab(56); BLYR(6)

3559 LPrint "WALKS";: LPrint Tab(23); Using "###"; BLL(7);: LPrint Tab(37); BL$(7);: LPrint Tab(56); BLYR(7)

3560 LPrint "STRIKEOUTS";: LPrint Tab(23); Using "###"; BLL(8);: LPrint Tab(37); BL$(8);: LPrint Tab(56); BLYR(8)

3561 LPrint "STEALS";: LPrint Tab(23); Using "###"; BLL(9);: LPrint Tab(37); BL$(9);: LPrint Tab(56); BLYR(9)

3562 LPrint "CAUGHT STEALING";: LPrint Tab(23); Using "###"; BLL(10);: LPrint Tab(37); BL$(10);: LPrint Tab(56); BLYR(10)

3563 LPrint "ERRORS";: LPrint Tab(23); Using "###"; BLL(11);: LPrint Tab(37); BL$(11);: LPrint Tab(56); BLYR(11)

3564 LPrint "BATTING AVERAGE";: LPrint Tab(22); Using ".###"; BLL(12);: LPrint Tab(37); BL$(12);: LPrint Tab(56); BLYR(12)

3565 LPrint "SLUGGING PCT.";: LPrint Tab(22); Using ".###"; BLL(13);: LPrint Tab(37); BL$(13);: LPrint Tab(56); BLYR(13)

3566 LPrint "ON BASE PCT.";: LPrint Tab(22); Using ".###"; BLL(14);: LPrint Tab(37); BL$(14);: LPrint Tab(56); BLYR(14)

3652 LPrint "WINS";: LPrint Tab(23); Using "###"; PLL(0);: LPrint Tab(37); PL$(0);: LPrint Tab(56); PLYR(0)

3653 LPrint "LOSSES";: LPrint Tab(23); Using "###"; PLL(1);: LPrint Tab(37); PL$(1);: LPrint Tab(56); PLYR(1)

3654 LPrint "SAVES";: LPrint Tab(23); Using "###"; PLL(2);: LPrint Tab(37); PL$(2);: LPrint Tab(56); PLYR(2)

3655 LPrint "INNINGS PITCHED";: LPrint Tab(23); Using "###"; PLL(3);:
If PLL(3) - (Int(PLL(3) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PLL(3) - (Int(PLL(3) / 3) * 3);:
LPrint Tab(37); PL$(3);: LPrint Tab(56); PLYR(3)

3656 LPrint "HITS ALLOWED";: LPrint Tab(23); Using "###"; PLL(4);: LPrint Tab(37); PL$(4);: LPrint Tab(56); PLYR(4)

3657 LPrint "WALKS";: LPrint Tab(23); Using "###"; PLL(5);: LPrint Tab(37); PL$(5);: LPrint Tab(56); PLYR(5)

3658 LPrint "STRIKOUTS";: LPrint Tab(23); Using "###"; PLL(6);: LPrint Tab(37); PL$(6);: LPrint Tab(56); PLYR(6)

3659 LPrint "EARNED RUN AVERAGE";: LPrint Tab(22); Using "#.##"; PLL(7);: LPrint Tab(37); PL$(7);: LPrint Tab(56); PLYR(7)

3660 LPrint "GAMES";: LPrint Tab(23); Using "###"; PLL(8);: LPrint Tab(37); PL$(8);: LPrint Tab(56); PLYR(8)

3661 LPrint "COMPLETE GAMES";: LPrint Tab(23); Using "###"; PLL(9);: LPrint Tab(37); PL$(9);: LPrint Tab(56); PLYR(9)

3662 LPrint "RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL(10);: LPrint Tab(37); PL$(10);: LPrint Tab(56); PLYR(10)

3663 LPrint "EARNED RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL(11);: LPrint Tab(37); PL$(11);: LPrint Tab(56); PLYR(11)

3664 LPrint "SHUTOUTS";: LPrint Tab(23); Using "###"; PLL(12);: LPrint Tab(37); PL$(12);: LPrint Tab(56); PLYR(12)

3665 LPrint "HOME RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL(13);: LPrint Tab(37); PL$(13);: LPrint Tab(56); PLYR(13)

3666 LPrint "DOUBLE PLAYS";: LPrint Tab(23); Using "###"; PLL(14);: LPrint Tab(37); PL$(14);: LPrint Tab(56); PLYR(14)

3670 LPrint Chr$(12)

3692 GoTo 2

5000 D = 0

5001 For J = 0 To 8

    5002 D = D + 1

    5003 X$ = Mid$(Z$, D, 1)

    5004 If X$ = "." Then JX = D - 1

5005 Next J

5006 Return

4500 Locate 10, 8
Call CSMsgBox("NO CAREER LEADER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 2


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 30: Color 12
    Print "FULL COUNT BASEBALL 6.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
