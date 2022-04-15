'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - League Standings"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim WW(40), LL(40), TML$(40), Z1$(40), EWW(40), RWW(40), HWW(40), ELL(40), RLL(40), HLL(40), LTW(40), LTL(40)
Dim H%(162), H$(162), V%(162), PL$(162), W$(162), L$(162), I%(162)
Dim TWS(40), TLS(40), CWS(40), CLT(40), LWS(40), LLS(40)
Dim D$(2), menuItems$(4)

4 For I = 1 To 40: TWS(I) = 0: TLS(I) = 0: LWS(I) = 0: LLS(I) = 0: CWS(I) = 0: CLT(I) = 0: Next
Open "DEV.BAS" For Input As #2: For I = 0 To 2: Input #2, D$(I): Next: Close 2

6 TMR = 40: PV = 0: GoTo 2300

9 For KK = 1 To 40: YY = YY + 1
    Locate 7, 20: Color 14: TMR = TMR - 1: Print Using "###"; TMR
    If Z1$(KK) = "*" Then 60 Else GoTo 10

    10 TML$(YY) = Z1$(KK)

    11 Locate 4: Color 15: Print "COMPUTING RECORDS . . . . . PLEASE WAIT"

    20 Open D$(1) + Z1$(KK) + "S" For Input As #1

    21 Input #1, XX%

    22 For I = 1 To 162: Input #1, H%(I): Next I

    23 For I = 1 To 162: Input #1, H$(I): Next I

    24 For I = 1 To 162: Input #1, V%(I): Next I

    25 For I = 1 To 162: Input #1, PL$(I): Next I

    26 For I = 1 To 162: Input #1, W$(I): Next I

    27 For I = 1 To 162: Input #1, L$(I): Next I

    28 For I = 1 To 162: Input #1, I%(I): Next I

    29 Close #1

    30 For I = 1 To XX%

        31 If H%(I) > V%(I) Then WW(YY) = WW(YY) + 1

        32 If H%(I) > V%(I) And I%(I) > 9 Then EWW(YY) = EWW(YY) + 1

        33 If H%(I) > V%(I) And PL$(I) = "A" Then RWW(YY) = RWW(YY) + 1

        34 If H%(I) > V%(I) And PL$(I) = "H" Then HWW(YY) = HWW(YY) + 1

        35 If V%(I) > H%(I) Then LL(YY) = LL(YY) + 1

        36 If V%(I) > H%(I) And I%(I) > 9 Then ELL(YY) = ELL(YY) + 1

        37 If V%(I) > H%(I) And PL$(I) = "A" Then RLL(YY) = RLL(YY) + 1

        38 If V%(I) > H%(I) And PL$(I) = "H" Then HLL(YY) = HLL(YY) + 1
        If H%(I) > V%(I) Then LWS(YY) = LWS(YY) + 1: LLS(YY) = 0
        If LWS(YY) > TWS(YY) Then TWS(YY) = LWS(YY)
        If H%(I) < V%(I) Then LLS(YY) = LLS(YY) + 1: LWS(YY) = 0
        If LLS(YY) > TLS(YY) Then TLS(YY) = LLS(YY)

        39 If H%(I) > V%(I) Then 40
        If H%(I) < V%(I) Then 42

        40 CWS(YY) = CWS(YY) + 1: CLT(YY) = 0: GoTo 44

        42 CLT(YY) = CLT(YY) + 1: CWS(YY) = 0

    44 Next I

    45 If XX% <= 10 Then LTL(YY) = LL(YY): LTW(YY) = WW(YY): GoTo 60

    46 M = XX% - 9

    47 For J = M To XX%

        48 If H%(J) > V%(J) Then LTW(YY) = LTW(YY) + 1

        49 If V%(J) > H%(J) Then LTL(YY) = LTL(YY) + 1

    50 Next J

60 Next KK

62 Open D$(2) + LG$ + ".STD" For Output As #1

63 Write #1, LGF$, D1$, D2$, D3$, D4$

67 For I = 1 To 40: Write #1, TML$(I), WW(I), LL(I), TWS(I), TLS(I), CWS(I), CLT(I), HWW(I), HLL(I), RWW(I), RLL(I), EWW(I), ELL(I), LTW(I), LTL(I): Next

69 Close #1

70 GoTo 2300

950 For X = AA To ZZ - 1: For Y = X + 1 To ZZ: If WW(X) - LL(X) > WW(Y) - LL(Y) Then 985

    955 If WW(X) - LL(X) = WW(Y) - LL(Y) And WW(X) <= WW(Y) And WW(X) > LL(X) Then 985

        960 If WW(X) - LL(X) = WW(Y) - LL(Y) And WW(X) >= WW(Y) And WW(X) < LL(X) Then 985

        965 Swap TML$(X), TML$(Y)
        Swap TWS(X), TWS(Y)
        Swap TLS(X), TLS(Y)
        Swap CWS(X), CWS(Y)
        Swap CLT(X), CLT(Y)
        Swap WW(X), WW(Y)
        Swap LL(X), LL(Y)
        Swap EWW(X), EWW(Y)
        Swap ELL(X), ELL(Y)
        Swap HLL(X), HLL(Y)
        Swap RLL(X), RLL(Y)
        Swap HWW(X), HWW(Y)
        Swap RWW(X), RWW(Y)
        Swap LTL(X), LTL(Y)
        Swap LTW(X), LTW(Y)

985 Next Y: Next X
Return

2000 For NT = 1 To 4
    AA = NT * 10 - 9: ZZ = NT * 10: GoSub 950

    2011 Cls: Locate 1, 35: Color 11: Print LGF$
    Locate 2: Color 14: Print Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
    Locate 3: Color 14
    If NT = 1 Then Print D1$;
    If NT = 2 Then Print D2$;
    If NT = 3 Then Print D3$;
    If NT = 4 Then Print D4$;
    Print Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"
    For I = AA To ZZ

        2012 If TML$(I) = "" Then 2014

        2013 Color 3: Print TML$(I);: Color 15: Print Tab(14); Using "###"; WW(I) + LL(I); Tab(18);
        Print Using "###"; WW(I); Tab(22);
        Print Using "###"; LL(I);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then Print Tab(26); "1.000";: GoTo 2015
        If WW(I) = 0 Then Print Tab(27); ".000";: GoTo 2015
        If WW(I) + LL(I) <> 0 Then Print Tab(27); Using ".###"; WW(I) / (WW(I) + LL(I));: GoTo 2015
        If WW(I) + LL(I) = 0 Then Print Tab(27); ".000";

        2015 If ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 = 0 Then Print Tab(32); " ---";
        If ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 <> 0 Then Print Tab(32); Using "##.#"; ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2;
        Print Tab(44); Using "##"; TWS(I); Tab(47); TLS(I); Tab(51); CWS(I); Tab(54); CLT(I);
        Print Tab(57); Using "##"; HWW(I);: Print Tab(60); Using "##"; HLL(I);
        Print Tab(63); Using "##"; RWW(I);: Print Tab(66); Using "##"; RLL(I);
        Print Tab(69); Using "##"; EWW(I);: Print Tab(72); Using "##"; ELL(I);
        Print Tab(75); Using "##"; LTW(I);: Print Tab(78); Using "##"; LTL(I)

    2014 Next
    Locate 4, 38: Print Using "###"; 163 - WW(NT * 10 - 9) - LL(NT * 10 - 8)
    Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
    2036 I$ = GetKeyPress$
Next NT

2300 Cls
Call TITLE
menuItems$(1) = "UPDATE LEAGUE STANDINGS      "
menuItems$(2) = "VIEW / PRINT LEAGUE STANDINGS"
menuItems$(3) = "RETURN TO MAIN MENU          "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 27
Call box0(5, 26, 9, 56, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 3
I = SELECTION%

2306 If I = 1 Then 2500

2307 If I = 2 Then 2600

2308 If I = 3 Then Run "NEWLDR.EXE"

2500 Cls

4000 Spec$ = D$(2) + "*.TMS"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 2300
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO UPDATE": Color 15
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
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 2300
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
2535 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310
targetFile$ = RTrim$(targetFile$)
LG$ = Left$(targetFile$, Len(targetFile$) - 4)

2530 Open D$(2) + LG$ + ".TMS" For Input As #2

2540 Input #2, LGF$: Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$

2550 For X = 1 To 40: Input #2, Z1$(X): Next

2560 Close 2

2565 GoTo 9

2600 Cls
Spec$ = D$(2) + "*.STD"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 2300
ReDim stdFiles$(0 To Count%)
For X = 1 To Count%
    stdFiles$(X) = Space$(12)
Next
stdFiles$(0) = Spec$
Call ReadFile(stdFiles$())
'CALL SortStr(VARPTR(stdFiles$(1)), Count%, 0)

1311 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO VIEW": Color 15
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
MAMenu stdFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 2300
targetFile$ = stdFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
2536 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310
targetFile$ = RTrim$(targetFile$)
LGS$ = Left$(targetFile$, Len(targetFile$) - 4)

2602 Open D$(2) + LGS$ + ".STD" For Input As #1

2603 Input #1, LGF$, D1$, D2$, D3$, D4$

2667 For I = 1 To 40: Input #1, TML$(I), WW(I), LL(I), TWS(I), TLS(I), CWS(I), CLT(I), HWW(I), HLL(I), RWW(I), RLL(I), EWW(I), ELL(I), LTW(I), LTL(I): Next

2669 Close #1

2670 Locate 10, 1
Color 12
Print "P";: Color 7: Print "RINT OR ";
Color 12: Print "V";: Color 7: Print "IEW STANDINGS"
2671 I$ = GetKeyPress$

2672 If UCase$(I$) = "P" GoTo 3000

2673 If UCase$(I$) = "V" GoTo 2000

3000 MJ = 0: LPrint Chr$(15)

'3001 WIDTH "LPT1:1", 130
LPrint Tab(35); LGF$
For NT = 1 To 4
    AA = NT * 10 - 9: ZZ = NT * 10: GoSub 950
    LPrint Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
    If NT = 1 Then LPrint D1$;
    If NT = 2 Then LPrint D2$;
    If NT = 3 Then LPrint D3$;
    If NT = 4 Then LPrint D4$;
    LPrint Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"
    For I = AA To ZZ

        3012 If TML$(I) = "" Then 3014

        3013 Color 3: LPrint TML$(I);: Color 15: LPrint Tab(14); Using "###"; WW(I) + LL(I); Tab(18);
        LPrint Using "###"; WW(I); Tab(22);
        LPrint Using "###"; LL(I);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then LPrint Tab(26); "1.000";: GoTo 3015
        If WW(I) = 0 Then LPrint Tab(27); ".000";: GoTo 3015
        If WW(I) + LL(I) <> 0 Then LPrint Tab(27); Using ".###"; WW(I) / (WW(I) + LL(I));: GoTo 3015
        If WW(I) + LL(I) = 0 Then LPrint Tab(27); ".000";

        3015 If ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 = 0 Then LPrint Tab(32); " ---";
        If ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2 <> 0 Then LPrint Tab(32); Using "##.#"; ((WW(NT * 10 - 9) - WW(I)) + (LL(I) - LL(NT * 10 - 9))) / 2;
        If I = NT * 10 - 9 Then LPrint Tab(38); Using "###"; 163 - WW(NT * 10 - 9) - LL(NT * 10 - 8);
        LPrint Tab(44); Using "##"; TWS(I); Tab(47); TLS(I); Tab(51); CWS(I); Tab(54); CLT(I);
        LPrint Tab(57); Using "##"; HWW(I);: LPrint Tab(60); Using "##"; HLL(I);
        LPrint Tab(63); Using "##"; RWW(I);: LPrint Tab(66); Using "##"; RLL(I);
        LPrint Tab(69); Using "##"; EWW(I);: LPrint Tab(72); Using "##"; ELL(I);
        LPrint Tab(75); Using "##"; LTW(I);: LPrint Tab(78); Using "##"; LTL(I)

    3014 Next
    LPrint: LPrint
Next NT
LPrint Chr$(12)

3096 LPrint Chr$(18)

'3097 WIDTH "LPT1:1", 80

3100 GOTO 2300


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
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

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
