'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Programmable Manager"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB DRAWBOX ()
DECLARE SUB PHBATTER ()
DECLARE SUB PROFILEPRINT ()
DECLARE SUB PHPITCHER ()
DECLARE SUB UPDATEFILE ()
DECLARE SUB PITSTRAT ()
DECLARE SUB PITCHAROUND ()
DECLARE SUB THROWHOME ()
DECLARE SUB BASERUN ()

5 Dim K9(1) As Long
Dim Y As Long
Dim FRAC As Single

15 Dim B$(23), B1$(4), BS$(22), BTR$(23), C$(10)
Dim N$(22), NP$(21), P$(21), PB$(23), PT$(1), PS$(21)
Dim X$(12), YN$(1)
Dim teamItems$(500), menuItems$(15)
Dim Shared D$(3), R$(999)
Dim Shared Q$(1843), YN$, B$


Dim AB%(9), B%(22, 44), B3%(9), B7%(9), B9%(9)
Dim GS%(21, 1), L%(13), M%(9), P%(21, 36)
Dim V%(500), V1%(1)
Dim Shared MG%(1000)

Dim CZ(23), BPR(23)
Dim Shared T

YN$(0) = "Y": YN$(1) = "N"
Data A,B,C,D,E,F,G,H,I,J,K,L,M
Data 681,663,645,627,609,591,573,555,537
For I = 0 To 12: Read X$(I): Next
For I = 1 To 9: Read AB%(I): Next
B$(23) = "PITCHER"
PT$(1) = "LHP": PT$(0) = "RHP"
Data LP,L,S,R,RP
Data dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph
For I = 0 To 4: Read B1$(I): Next
For I = 0 To 10: Read C$(I): Next I

Open "DEV.BAS" For Input As #1
For I = 0 To 3: Input #1, D$(I): Next I
Close #1

Cls
Print Tab(32);: Color 14, 4: Print "COMPUTER MANAGER": Color 15, 0
Print
Print
Print "     THIS PROGRAM ALLOWS YOU TO CUSTOMIZE THE COMPUTER MANAGER'S"
Print
Print "     STRATEGY AND PLAYER USAGE FOR EACH INDIVIDUAL TEAM"
30 I$ = GetKeyPress$

35 Cls

38 GoSub 1445
GoSub 4512
If MG%(0) <> 999 Then MG%(0) = 999: For I = 1 To 999: MG%(I) = -1: Next

45 Color , 1
Cls
Print
Print Tab(23);
Color 14, 4: Print "FULL COUNT PROGRAMMABLE MANAGER"
Print
Erase menuItems$
menuItems$(1) = "SELECT NEW TEAM TO EDIT                                  "
menuItems$(2) = "SELECT/CHANGE LINEUPS                                    "
menuItems$(3) = "SELECT/CHANGE ROTATIONS/RELIEVERS                        "
menuItems$(4) = "SELECT/CHANGE PINCH HITTERS                              "
menuItems$(5) = "SELECT/CHANGE DEFENSIVE REPLACEMENTS (8TH AND 9TH INNING)"
menuItems$(6) = "SELECT/CHANGE CLOSING STRATEGY                           "
menuItems$(7) = "SELECT/CHANGE HITTING STRATEGY                           "
menuItems$(8) = "SELECT/CHANGE DEFENSIVE STRATEGY                         "
menuItems$(9) = "SELECT/CHANGE PINCH RUNNING STRATEGY                     "
menuItems$(10) = "SELECT/CHANGE BASE RUNNING STRATEGY                      "
menuItems$(11) = "IMPORT MANAGERIAL PROFILE                                "
menuItems$(12) = "EXPORT MANAGERIAL PROFILE                                "
menuItems$(13) = "PRINT MANAGERIAL PROFILE                                 "
menuItems$(14) = "RETURN TO GAME MENU                                      "
SELECTION% = 1
Count% = 14
Start% = 1
SCANCODE% = 14
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 10
Call box0(4, 9, 19, 70, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 88
Select Case SELECTION%
    Case 1
        For I = 1 To 999: MG%(I) = -1: Next: GoTo 35
    Case 2
        GoTo 100
    Case 3
        GoTo 55
    Case 4
        GoTo 200
    Case 5
        GoTo 280
    Case 6
        Call PITSTRAT: GoTo 45
    Case 7
        GoTo 3000
    Case 8
        GoTo 3300
    Case 9
        GoTo 3501
    Case 10
        Call BASERUN: GoTo 45
    Case 11
        GoSub 3900: GoTo 45
    Case 12
        GoSub 3800: GoTo 45
    Case 13
        GoSub 4000: GoTo 45
    Case 14
        GoTo 88
End Select

55 Cls: Print Tab(22);: Color 14, 4: Print "PITCHING ROTATION/RELIEVER MANAGEMENT": Print: Print: Color , 0
Erase menuItems$
menuItems$(1) = "INPUT A NEW STARTING PITCHING ROTATION     "
menuItems$(2) = "EDIT AN EXISTING STARTING PITCHING ROTATION"
menuItems$(3) = "SELECT/EDIT MIDDLE RELIEVERS               "
menuItems$(4) = "SELECT/EDIT SET-UP MEN                     "
menuItems$(5) = "SELECT/EDIT CLOSERS                        "
menuItems$(6) = "RETURN TO MAIN MENU                        "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 19
Call box0(4, 18, 11, 62, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
Q = SELECTION% - 1
If SCANCODE% = 27 Or Q = 5 Then 45
If Q = 0 Or Q = 1 Then 4500
If Q = 2 Then 5000
If Q = 3 Then 5200
If Q = 4 Then 5300

88 Run "HELLO.EXE"

100 Cls: Print Tab(26);: Color 14, 4: Print "LINEUP STRATEGY OPTIONS": Print: Print: Color , 0
Erase menuItems$
menuItems$(1) = "SELECT/CHANGE/DISPLAY LINEUPS VS. RHP                  "
menuItems$(2) = "SELECT/CHANGE/DISPLAY LINEUPS VS. LHP                  "
menuItems$(3) = "ANALYZE PLAYER USAGE ACCORDING TO YOUR LINEUPS SELECTED"
menuItems$(4) = "RETURN TO TOP MENU                                     "
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 13
Call box0(4, 12, 9, 68, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
Q2% = SELECTION% - 1
If Q2% = 3 Or SCANCODE% = 27 Then 45
If Q2% = 1 Or Q2% = 0 Then 480
If Q2% = 2 Then 300

200 'SELECT PINCH HITTERS
If MG%(540) <> 999 Then MG%(540) = 999: For I = 1 To 10: MG%(I + 540) = -1: MG%(I + 550) = 0: Next
Cls
Locate 1, 1
Print "# ";: Color L%(11), L%(12): Print T$;
Color 14, 0: Locate , 16: Print "B   AB  HR  RBI   AVG"
I2 = 0
For I = 0 To 22
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then I2 = I2 + 1: GoTo 205
    Locate I + 2 - I2, 1
    Color 2, 0
    If I <= 9 Then Print Using "#"; I;: Color 15, 0
    If I > 9 Then Print X$(I - 10);: Color 15, 0
    For I1 = 1 To 10
        If I = MG%(I1 + 540) Then Color L%(11), L%(12)
    Next
    Locate , 3: Print B$(I);
    Color 15, 0
    Locate , 16: Print B1$(B%(I, 0) + 2);
    Locate , 17: Print Using "  ###  ##  ###"; B%(I, 4); B%(I, 9); B%(I, 10);
    If B%(I, 4) <> 0 Then Print Using "  .###"; B%(I, 6) / B%(I, 4);
    If B%(I, 4) = 0 Then Print "  .---";

205 Next I

207 Locate 1, 48: Print "  PINCH HITTERS        "
Locate , 48: Print "  ---- VS LHP  ----"
For I = 1 To 5
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 540) < 0 Then Print: GoTo 210
    Print B$(MG%(I + 540))

210 Next
Locate , 48: Print "  ---- VS RHP  ----"
For I = 6 To 10
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 540) < 0 Then Print: GoTo 212
    Print B$(MG%(I + 540))

212 Next

211 Locate 15, 48: Print "(11) ACCEPT CHOICES"
Locate 16, 48: Print "(12) DELETE A HITTER"

215 Locate 17, 48: Input "ENTER SELECTION (1-12)"; I
If I < 1 Or I > 12 Then 215
If I >= 1 And I <= 10 Then GoSub 220: GoTo 200
If I = 11 Then Call UPDATEFILE: GoTo 45
If I = 12 Then GoSub 250: GoTo 200

220 Locate 18, 48: Print Using "SELECT HITTER ##:"; I;
221 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 540) = I1
'LOCATE 18,48:PRINT "                     "
Return

250 Locate 16, 48: Input "SELECT HITTER TO DELETE: "; I
If I < 1 Or I > 10 Then 250
MG%(I + 540) = -1: MG%(I + 550) = 0
Return
GoTo 45

280 'DEFENSIVE REPLACEMENTS
Cls: Print Tab(26);: Color 14, 4: Print "DEFENSIVE REPLACEMENT 8TH AND 9TH INNING STRATEGY OPTIONS": Print: Print: Color , 0
If MG%(560) <> 999 Then MG%(560) = 999: For I = 1 To 4: MG%(I + 560) = -1: MG%(I + 564) = 0: MG%(I + 568) = 0: MG%(I + 572) = 0: MG%(I + 576) = 0: Next
Cls
Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Locate , 17: Print "1 FAVG 2 FAVG 3 FAVG 4 FAVG"
I1 = 0
For I = 0 To 22: Locate , 1
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then I1 = I1 + 1: GoTo 285
    Locate I + 2 - I1, 1
    Color 2, 0
    If I < 10 Then Print Using "#"; I;: Color 15, 0: GoSub 1980
    If I > 9 Then Print X$(I - 10);: Color 15, 0: GoSub 1980
    Locate , 3: Print B$(I);: Color 15, 0
    Locate , 17: If B%(I, 22) <> -1 Then Print Using "# ####"; B%(I, 22); B%(I, 26);
    Locate , 24: If B%(I, 23) <> -1 Then Print Using "# ####"; B%(I, 23); B%(I, 27);
    Locate , 31: If B%(I, 24) <> -1 Then Print Using "# ####"; B%(I, 24); B%(I, 28);
    Locate , 38: If B%(I, 25) <> -1 Then Print Using "# ####"; B%(I, 25); B%(I, 29);

285 Next
Locate 1, 46
Print "# PLAYER     P SUBSTITUTE  %  RA"
For I = 1 To 4
    Locate , 46
    Print Using "# "; I;
    If MG%(I + 560) < 0 Then Print: GoTo 290
    Print Left$(B$(MG%(I + 560)), 10); Tab(59);
    Print Using "# "; MG%(I + 576);
    Print Left$(B$(MG%(I + 564)), 10); Tab(72); Using "### #"; MG%(I + 568); MG%(I + 572)

290 Next

286 Locate 7, 46: Print "(5) ACCEPT CHOICES"
Locate 8, 46: Print "(6) DELETE A CHOICE"

287 Locate 9, 46: Input "ENTER SELECTION (1-6)"; I
If I < 1 Or I > 6 Then 287
If I >= 1 And I <= 4 Then GoSub 295: GoTo 280
If I = 5 Then Call UPDATEFILE: GoTo 45
If I = 6 Then GoSub 298: GoTo 280

295 Locate 10, 46: Print Using "SELECT HITTER #:"; I;

296 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 560) = I1
Locate 11, 46: Print "POSITION (2-9) ";

281 I$ = GetKeyPress$
I1 = Val(I$): If I1 < 2 Or I1 > 9 Then 281
MG%(I + 576) = I1
Locate 12, 46: Print "SELECT SUB #:";
282 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 564) = I1

283 Locate 13, 46: Input "ENTER % OF TIME TO USE "; MG%(I + 568)
If MG%(I + 568) < 0 Or MG%(I + 568) > 100 Then 283
Locate 14, 46: Print "NUMBER OF RUNS AHEAD"
Locate 15, 46: Input "0,1,2 OR MORE "; I1
If I1 < 0 Then I1 = 0
If I1 > 2 Then I1 = 2
MG%(I + 572) = I1
Return

298 Locate 10, 46: Input "SELECT CHOICE TO DELETE: "; I
If I < 1 Or I > 4 Then 298
MG%(I + 560) = -1: MG%(I + 564) = 0: MG%(I + 568) = 0: MG%(I + 572) = 0: MG%(I + 576) = 0
Return

300 'ANALYZE PLAYER USAGE
LH% = 0: RH% = 0
For I = 0 To 22: For I1 = 42 To 44
    B%(I, I1) = 0
Next: Next
For I = 10 To 19
    If MG%(120 + I * 21) <> 999 Then 305
    LH% = LH% + MG%(100 + I * 21)

305 Next
For I = 0 To 9
    If MG%(120 + I * 21) <> 999 Then 310
    RH% = RH% + MG%(100 + I * 21)

310 Next
For I = 0 To 9
    If MG%(120 + I * 21) <> 999 Then 320
    For I1 = 1 To 9
        If MG%(101 + I1 + I * 21) = 1 Then 315
        FRAC = MG%(100 + I * 21) / RH%
        B%(MG%(110 + I1 + I * 21), 42) = B%(MG%(110 + I1 + I * 21), 42) + (AB%(I1) * FRAC) * .7
        B%(MG%(110 + I1 + I * 21), 44) = B%(MG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC) * .7

    315 Next I1

320 Next I
For I = 10 To 19
    If MG%(120 + I * 21) <> 999 Then 325
    For I1 = 1 To 9
        If MG%(101 + I1 + I * 21) = 1 Then 330
        FRAC = MG%(100 + I * 21) / LH%
        B%(MG%(110 + I1 + I * 21), 43) = B%(MG%(110 + I1 + I * 21), 43) + (AB%(I1) * FRAC) * .3
        B%(MG%(110 + I1 + I * 21), 44) = B%(MG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC) * .3

    330 Next I1

325 Next I

350 Cls: Color 14
Print "# ";: Color L%(11), L%(12): Print T$;: Color 14, 0
Locate , 18: Print "B  POS  ACTUAL AB  PROJECTED: AB/LHP  AB/RHP  TOT/AB"
Locate 2
I1 = 0
For I = 0 To 22: Locate , 1
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then I1 = I1 + 1: GoTo 355
    Color 2, 0
    Locate I + 2 - I1, 1
    If I < 10 Then Print Using "#"; I;: Color 15, 0: GoSub 1980
    If I > 9 Then Print X$(I - 10);: Color 15, 0: GoSub 1980
    Locate , 3: Print B$(I);: Color 15, 0: Locate , 18: Print B1$(B%(I, 0) + 2);
    Locate , 21: If B%(I, 22) <> -1 Then Print Using "#"; B%(I, 22);
    Locate , 22: If B%(I, 23) <> -1 Then Print Using "#"; B%(I, 23);
    Locate , 23: If B%(I, 24) <> -1 Then Print Using "#"; B%(I, 24);
    Locate , 24: If B%(I, 25) <> -1 Then Print Using "#"; B%(I, 25);
    Locate , 27
    Print Using "   ###                ###     ###      ###"; B%(I, 4); B%(I, 43); B%(I, 42); B%(I, 44);

355 Next
GoSub 4095
GoTo 100

480 Cls
Locate 3, 1
Color L%(11), L%(12): Print T$;: Color 15, 0: Print " LINEUPS VS "; PT$(Q2%): Print
Erase menuItems$
menuItems$(1) = "INPUT A NEW LINEUP            "
menuItems$(2) = "EDIT AN EXISTING LINEUP       "
menuItems$(3) = "DELETE AN EXISTING LINEUP     "
menuItems$(4) = "RETURN TO LINEUP STRATEGY MENU"
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 13
Call box0(4, 12, 9, 43, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
Q3% = SELECTION% - 1
If Q3% = 3 Or SCANCODE% = 27 Then 100
If Q3% = 1 Or Q3% = 2 Then 495
For I = 1 To 9: B3%(I) = -1: B7%(I) = -1: Next
Locate 12, 1: Print "IS A DH TO BE USED IN THIS LINEUP (YN)"
492 I$ = GetKeyPress$
D% = 0
If UCase$(I$) = "Y" Then D% = 1

495 If Q3% = 0 Then 496
GoSub 2500
LF% = 0
For I = 0 To 9
    If MG%(120 + (I + Q2% * 10) * 21) = 999 Then LF% = LF% + 1
Next
If LF% = 0 Then Locate 23, 1: Color 12: Print "NO LINEUPS FOUND! HIT ANY KEY TO RETURN TO MENU": Color 15: GoSub 4095: GoTo 480

496 Locate 23, 1
Print "CHOOSE LINEUP NUMBER (0-9) OR <ESC> TO EXIT ";
497 I$ = GetKeyPress$
If I$ = Chr$(27) Then 480
LN% = Val(I$)
If LN% < 0 Or LN% > 9 Or Q3% = 1 And MG%(120 + (LN% + Q2% * 10) * 21) <> 999 Then 495
If Q2% = 1 Then LN% = LN% + 10
If Q3% = 2 Then MG%(120 + LN% * 21) = 0: MG%(100 + LN% * 21) = 0: GoTo 480

500 If Q3% <> 1 Then GoSub 1100: Locate 6, 64: Print "VS " + PT$(Q2%): Locate 7, 61: Print Using "LINEUP ##   POS"; LN% - Q2% * 10: GoSub 2310: GoSub 540

510 If Q3% = 1 Then GoSub 1830

520 If RP = 1 Then RP = 0: GoTo 480

525 'GOSUB 2390

530 W5 = 0: W6 = 1: V%(0) = 1: V1%(0) = 9: V%(1) = 1: V1%(1) = 9
GoTo 480

540 For I = 0 To 9: M%(I) = 0: Next I: GoSub 1370

550 For I = 1 To 9

    560 GoSub 1010: Print "                  ";: GoSub 1010
    561 I$ = GetKeyPress$

    570 GoSub 1290

    580 I1 = Val(I$): If UCase$(I$) = "X" Then 540

    590 For J = 1 To 9: If J = I Or B3%(J) = I1 And B7%(J) = 1 Then 610

        600 If B3%(J) = I1 And UCase$(I$) <> "P" Then 560

    610 Next J

    620 If D% = 1 And UCase$(I$) = "P" Then 560

    630 If B%(I1, 21) <> 0 And UCase$(I$) <> "P" Then 560

    640 If UCase$(I$) = "P" Then Print Using "# "; I;: GoTo 760

    650 B3%(I) = I1: Print Using "# "; I;: Print B$(B3%(I));

    660 Locate , 74
    680 I$ = GetKeyPress$

    690 If Val(I$) = 1 Then 680

    700 For J = 1 To 9: If J = I Then 720

        710 If Val(I$) = B7%(J) Then 680

    720 Next J

    730 If D% = 0 And Val(I$) = 0 Or UCase$(I$) = "X" Then 560

    740 Print C$(Val(I$));: B7%(I) = Val(I$)

    750 B9%(B7%(I)) = B3%(I): M%(Val(I$)) = 1

    751 Locate I1 + 2, 2: Print "*";: Color L%(11), L%(12): Print B$(I1): Color 15, 0: GoSub 1370: GoTo 830

    760 For J = 1 To 9: If J = I Then 780

        770 If B3%(J) = P1% And B7%(J) = 1 Then 560

    780 Next J

    790 B3%(I) = P1%: Print B$(23);: M%(Val(I$)) = 1

    800 Locate , 72

    820 Print "  p ";: B7%(I) = 1: B9%(1) = P1%: M%(1) = 1: GoSub 1370

    830 If C1 = 1 Then 850

840 Next I

850 C1 = 1: Locate 25, 1: Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
Color 15, 0
860 I$ = GetKeyPress$

861 If UCase$(I$) = "R" Then For J = 1 To 9: M%(J) = 0: B7%(J) = -1: B3%(J) = -1: Next: C1 = 0: For J = 0 To 22: B%(J, 21) = 0: Next: RP = 1: GoTo 1001
If UCase$(I$) = "L" Then GoSub 1440: GoTo 850
If UCase$(I$) = "P" Then GoSub 1430: GoTo 850

870 I = Val(I$): If I < 1 Or I > 9 Then 890

880 Locate B3%(I) + 2 - B4%, 2: Print " "; B$(B3%(I)): GoTo 560

890 Print: Input "WHAT PERCENT OF TIME IS THIS LINEUP TO BE USED (1-100) "; LU%
If LU% < 1 Then LU% = 1
If LU% > 100 Then LU% = 100
MG%(100 + LN% * 21) = LU%
MG%(101 + LN% * 21) = D%
For I = 1 To 9
    MG%(101 + I + LN% * 21) = B7%(I)
    MG%(110 + I + LN% * 21) = B3%(I)
Next
MG%(120 + LN% * 21) = 999
MG%(0) = 999
Call UPDATEFILE

900 GoSub 1200
C1 = 0
For I = 0 To 9: B3%(I) = -1: B7%(I) = -1: Next

910 Return

920 For I = 1 To 9: GoSub 1010
    Print Using "# "; I;

    940 If B7%(I) = 1 And I5 <> 66 Then Print B$(23);

    950 If B7%(I) <> 1 Then Print B$(B3%(I));

    960 Print Tab(74);

    980 Print C$(B7%(I));

990 Next I

1000 I5 = 0: Return

1001 For J = 0 To 22: If B%(J, 31) <> 0 Then B%(J, 21) = 99
Next
GoTo 910

1010 Locate I + 7, 59

1050 Return

1100 Cls: Color 14: Print "#";: Color L%(11), L%(12): Print T$;: Color 14, 0
Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"
Locate 2: For I = 0 To 22: Locate , 1

    1111 If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then B%(I, 21) = 98: GoTo 1191
    Color 2, 0
    If I < 10 Then Print Using "#"; I;: Color 15, 0: GoSub 1980
    If I > 9 Then Print X$(I - 10);: Color 15, 0: GoSub 1980
    Locate , 3: Print B$(I);: Color 15, 0: Locate , 15: Print B1$(B%(I, 0) + 2);

    1141 Locate , 18: If B%(I, 22) <> -1 Then Print Using "#"; B%(I, 22);

    1142 Locate , 19: If B%(I, 23) <> -1 Then Print Using "#"; B%(I, 23);

    1143 Locate , 20: If B%(I, 24) <> -1 Then Print Using "#"; B%(I, 24);

    1144 Locate , 21: If B%(I, 25) <> -1 Then Print Using "#"; B%(I, 25);

    1160 Locate , 23

    1161 Print Using "### ## ## ## ### ###"; B%(I, 4); B%(I, 7); B%(I, 8); B%(I, 9); B%(I, 11); B%(I, 12);

    1170 Locate , 44: Print Using "#"; B%(I, 16);: Print Using "#"; B%(I, 34);: Print Using "#"; B%(I, 33);

    1180 Locate , 47

    1181 Print Using "### ##"; B%(I, 13); B%(I, 18);

    1182 If B%(I, 6) = B%(I, 4) And B%(I, 6) <> 0 Then Print "1.000";: GoTo 1190

    1183 If B%(I, 4) = 0 Then Print " .---";: GoTo 1190

    1184 Print Using " .###"; B%(I, 6) / B%(I, 4);

    1190 If I <> 22 Then Print

1191 Next I: Return

1200 For I = 1 To 9

    1210 If B7%(I) = 1 Then 1230

    1220 'B%(B3%(I), 21) = 1

1230 M%(I) = 0: Next I

1240 M%(0) = 0: Return

1370 Locate 25, 1: For I1 = 0 To 9: If D% = 0 And I1 = 0 Then 1410

    1380 If D% = 1 And I1 = 1 Then 1410

    1390 If M%(I1) = 0 Then Print I1; " ";

    1400 If M%(I1) = 1 Then Print "  ";

1410 Next I1

1420 Return

1430 Locate 18, 59: Print "B.O. PLYR 1 (1-9)";
1431 I$ = GetKeyPress$
K1 = Val(I$): If K1 < 1 Or K1 > 9 Then 1431
If B7%(K1) = 1 Then 1431
Locate 19, 59: Print "B.O. PLYR 2 (1-9)";
1432 I$ = GetKeyPress$
K2 = Val(I$): If K2 < 1 Or K2 > 9 Then 1432
If B7%(K2) = 1 Then 1432
Swap B7%(K2), B7%(K1)
B9%(B7%(K1)) = B3%(K1)
B9%(B7%(K2)) = B3%(K2)
GoSub 920
Locate 18, 59: Print "                 "
Locate 19, 59: Print "                 "
Return

1440 Locate 18, 59: Print "B.O. PLYR 1 (1-9)";
1441 I$ = GetKeyPress$
K1 = Val(I$): If K1 < 1 Or K1 > 9 Then 1441
'IF B7%(K1)=1 THEN 1441
Locate 19, 59: Print "B.O. PLYR 2 (1-9)";
1442 I$ = GetKeyPress$
K2 = Val(I$): If K2 < 1 Or K2 > 9 Then 1442
'IF B7%(K2)=1 THEN 1442
Swap B7%(K2), B7%(K1)
Swap B3%(K1), B3%(K2)
B9%(B7%(K1)) = B3%(K1)
B9%(B7%(K2)) = B3%(K2)
GoSub 920
Locate 18, 59: Print "                 "
Locate 19, 59: Print "                 "
Return

1830 GoSub 1100: GoSub 2310
LU% = MG%(100 + LN% * 21)
D% = MG%(101 + LN% * 21)
For I = 1 To 9
    B3%(I) = MG%(110 + I + LN% * 21)
    B7%(I) = MG%(101 + I + LN% * 21)
Next

1910 For I = 1 To 9: B9%(B7%(I)) = B3%(I)

    1912 If B7%(I) = 1 Then B3%(I) = P1%: B9%(1) = P1%: GoTo 1925

    1920 B%(B3%(I), 21) = 0

1925 Next I

1930 GoSub 2310: GoSub 920: GoSub 1940: GoTo 850

1940 For I2 = 0 To 22: Locate I2 + 2, 2: For I3 = 1 To 9

    1941 If B3%(I3) = I2 And B7%(I3) <> 1 Then Print "*";: Color L%(11), L%(12): Print B$(I2): Color 15, 0

1950 Next I3: Next I2

1960 For I3 = 0 To 9: M%(I3) = 1: Next I3

1970 Return

1980 If B%(I, 21) >= 98 Then Color 0, 7

1990 Return

2000 If P%(I, 15) >= 98 Then Color 0, 7

2010 Return

2310 For I = 10 To 22: Locate I, 59: Print "                     ";: Next: Return

2500 Cls
Color 12: Print Tab(30); "LINEUPS FOUND VS. "; PT$(Q2%): Color 14
Print " #0    "; MG%(100 + (0 + Q2% * 10) * 21); "%";
Locate , 17: Print " #1    "; MG%(100 + (1 + Q2% * 10) * 21); "%";
Locate , 33: Print " #2    "; MG%(100 + (2 + Q2% * 10) * 21); "%";
Locate , 49: Print " #3    "; MG%(100 + (3 + Q2% * 10) * 21); "%";
Locate , 65: Print " #4    "; MG%(100 + (4 + Q2% * 10) * 21); "%"
Color 15
For I = 0 To 4
    For I1 = 1 To 9
        If MG%(120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then Locate 3, 16 * I + 1: Color 2: Print "NONE FOUND": Color 15: GoTo 2510
        If MG%(120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then Locate 2 + I1, 16 * I + 1: Print "         ": GoTo 2510
        Locate 2 + I1, 16 * I + 1
        If MG%(101 + I1 + (I + Q2% * 10) * 21) = 1 Then Print "PITCHER";: GoTo 2512
        Print Left$(B$(MG%(110 + I1 + (I + Q2% * 10) * 21)), 10);

        2512 Locate 2 + I1, 16 * I + 12: Print C$(MG%(101 + I1 + (I + Q2% * 10) * 21))

    2510 Next
Next
Print
Color 14: Print " #5    "; MG%(100 + (5 + Q2% * 10) * 21); "%";
Locate , 17: Print " #6    "; MG%(100 + (6 + Q2% * 10) * 21); "%";
Locate , 33: Print " #7    "; MG%(100 + (7 + Q2% * 10) * 21); "%";
Locate , 49: Print " #8    "; MG%(100 + (8 + Q2% * 10) * 21); "%";
Locate , 65: Print " #9    "; MG%(100 + (9 + Q2% * 10) * 21); "%"
Color 15
For I = 5 To 9
    For I1 = 1 To 9
        If MG%(120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then Locate 14, 16 * (I - 5) + 1: Color 2: Print "NONE FOUND": Color 15: GoTo 2505
        If MG%(120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then Locate 13 + I1, 16 * (I - 5) + 1: Print "         ": GoTo 2505
        Locate 13 + I1, 16 * (I - 5) + 1
        If MG%(101 + I1 + (I + Q2% * 10) * 21) = 1 Then Print "PITCHER";: GoTo 2507
        Print Left$(B$(MG%(110 + I1 + (I + Q2% * 10) * 21)), 10);

        2507 Locate 13 + I1, 16 * (I - 5) + 12: Print C$(MG%(101 + I1 + (I + Q2% * 10) * 21))

    2505 Next
Next
Return

2100 NT = 0
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1105
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1

1105 Next I1
Close 1

1102 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
T = V%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."

1195 GoTo 1485

1445 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
Erase teamItems$
For I = 1 To 500: V%(I) = 1: Next
GoSub 2100

182 If BO% = 1 Then BO% = 0: GoTo 1445

183 Locate 2, 33: Color L%(11), L%(12): Print T$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1445
Color 15, 0
Return

1290 If UCase$(I$) = "A" Then I$ = "10"

1300 If UCase$(I$) = "B" Then I$ = "11"

1310 If UCase$(I$) = "C" Then I$ = "12"

1320 If UCase$(I$) = "D" Then I$ = "13"

1330 If UCase$(I$) = "E" Then I$ = "14"

1340 If UCase$(I$) = "F" Then I$ = "15"

1350 If UCase$(I$) = "G" Then I$ = "16"

1351 If UCase$(I$) = "H" Then I$ = "17"

1352 If UCase$(I$) = "I" Then I$ = "18"

1353 If UCase$(I$) = "J" Then I$ = "19"

1354 If UCase$(I$) = "K" Then I$ = "20"

1355 If UCase$(I$) = "L" Then I$ = "21"

1356 If UCase$(I$) = "M" Then I$ = "22"

1360 Return

1485 If YN$ = "" Or Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then BO% = 1: Return
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
For I = 0 To 22
    Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
    Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
    Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
    Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
Next

For I = 0 To 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015), 2020 - 37 - I * 89 AS X$
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027), 2020 - 61 - I * 89 AS X$
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039), 2020 - 85 - I * 89 AS X$
    Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
Next

Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
Get #1, T
T$ = Q$(0)

1532 If Right$(T$, 1) = " " Then T$ = Left$(T$, Len(T$) - 1): GoTo 1532
For I = 1 To 13: L%(I) = CVI(Q$(I)): Next: K9(0) = CVS(Q$(14))
For I = 0 To 22: B$(I) = Q$(I * 43 + 15)

    1533 If Right$(B$(I), 1) = " " Then B$(I) = Left$(B$(I), Len(B$(I)) - 1): GoTo 1533
    For I1 = 0 To 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        If B%(I, 0) = -1 Then PB$(I) = "L"
        If B%(I, 0) = 0 Then PB$(I) = "S"
        If B%(I, 0) = 1 Then PB$(I) = "R"
        If B%(I, 0) = 2 Then PB$(I) = "RP"
        If B%(I, 0) = -2 Then PB$(I) = "LP"
Next: Next
For I = 0 To 21: P$(I) = Q$(I * 38 + 1004)

    1534 If Right$(P$(I), 1) = " " Then P$(I) = Left$(P$(I), Len(P$(I)) - 1): GoTo 1534
    For I1 = 0 To 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
Next: Next
S$ = Q$(1840): M$ = Q$(1841): NN$ = Q$(1842): CK = CVI(Q$(1843))
Close 1

1535 If Right$(M$, 1) = " " Then M$ = Left$(M$, Len(M$) - 1): GoTo 1535

1536 If Right$(S$, 1) = " " Then S$ = Left$(S$, Len(S$) - 1): GoTo 1536

1537 If Right$(NN$, 1) = " " Then NN$ = Left$(NN$, Len(NN$) - 1): GoTo 1537
Return

4000 ' PRINT MANAGER PROFILE
LPrint "MANAGERIAL PROFILE FOR "; T$
LPrint
Q2% = 0
LF% = 0
For I = 0 To 9
    If MG%(120 + (I + Q2% * 10) * 21) = 999 Then LF% = LF% + 1
Next
If LF% = 0 Then LPrint "NO LINEUPS VS RHP ": GoTo 4010
GoSub 2500
_PrintImage 0
'CALL PRTSC0 (1)

4010 Q2% = 1
LF% = 0
For I = 0 To 9
    If MG%(120 + (I + Q2% * 10) * 21) = 999 Then LF% = LF% + 1
Next
If LF% = 0 Then LPrint "NO LINEUPS VS LHP ": GoTo 4020
GoSub 2500
_PrintImage 0
'CALL PRTSC0 (1)

4020 LPrint
If MG%(99) <> 999 Then LPrint "NO STARTING PITCHING ROTATION DEFINED"
LPrint "SIZE OF ROTATION IS "; MG%(1); " MAN"
LPrint "    STARTING ROTATION     % STARTS"
For I = 1 To MG%(1)
    LPrint Using "(#) "; I;
    If MG%(I + 2) < 0 Then LPrint: GoTo 4021
    LPrint P$(MG%(I + 2)); Tab(30); MG%(I + 12)

4021 Next
LPrint
LPrint "     SPOT STARTERS         % USED"
For I = 21 To 36
    LPrint Using "(##) "; I - 20;
    If MG%(I) < 0 Then LPrint: GoTo 4022
    LPrint P$(MG%(I)); Tab(30); MG%(I + 20)

4022 Next
LPrint
LPrint "  MIDDLE RELIEVERS      % APPEAR"
For I = 1 To 10
    LPrint Using "(##) "; I;
    If MG%(I + 60) < 0 Then LPrint: GoTo 4023
    LPrint P$(MG%(I + 60)); Tab(35); MG%(I + 70)

4023 Next
LPrint
LPrint "  SET-UP RELIEVERS      % APPEAR"
LPrint "   -----  VS LEFT  -------      "
For I = 1 To 3
    LPrint Using "(##) "; I;
    If MG%(I + 80) < 0 Then LPrint: GoTo 4050
    LPrint P$(MG%(I + 80)); Tab(35); MG%(I + 90)

4050 Next
LPrint "   -----  VS RIGHT  -------      "
For I = 4 To 6
    LPrint Using "(##) "; I;
    If MG%(I + 80) < 0 Then LPrint: GoTo 4051
    LPrint P$(MG%(I + 80)); Tab(35); MG%(I + 90)

4051 Next
LPrint
LPrint "          CLOSERS       % APPEAR"
LPrint "   -----  VS LEFT  -------      "
For I = 1 To 3
    LPrint Using "(##) "; I;
    If MG%(I + 520) < 0 Then LPrint: GoTo 4024
    LPrint P$(MG%(I + 520)); Tab(35); MG%(I + 530)

4024 Next
LPrint "   -----  VS RIGHT  -------      "
For I = 4 To 6
    LPrint Using "(##) "; I;
    If MG%(I + 520) < 0 Then LPrint: GoTo 4025
    LPrint P$(MG%(I + 520)); Tab(35); MG%(I + 530)

4025 Next
LPrint
LPrint "  PINCH HITTERS        "
LPrint "  ---- VS LHP  ----"
For I = 1 To 5
    LPrint Using "(##) "; I;
    If MG%(I + 540) < 0 Then LPrint: GoTo 4026
    LPrint B$(MG%(I + 540))

4026 Next
LPrint "  ---- VS RHP  ----"
For I = 6 To 10
    LPrint Using "(##) "; I;
    If MG%(I + 540) < 0 Then LPrint: GoTo 4027
    LPrint B$(MG%(I + 540))

4027 Next
LPrint
LPrint "DEFENSIVE REPLACEMENTS"
LPrint "# PLAYER     P SUBSTITUTE  %  RA"
For I = 1 To 4
    LPrint Using "# "; I;
    If MG%(I + 560) < 0 Then LPrint: GoTo 4028
    LPrint Left$(B$(MG%(I + 560)), 10); Tab(19);
    LPrint Using "# "; MG%(I + 576);
    LPrint Left$(B$(MG%(I + 564)), 10); Tab(32); Using "### #"; MG%(I + 568); MG%(I + 572)

4028 Next
LPrint
If MG%(581) <> 999 Then LPrint "NO CLOSING STRATEGY HAS BEEN DEFINED": LPrint: GoTo 4029
LPrint Tab(31); "CLOSING STRATEGY"
LPrint
LPrint "STRATEGY                                        % OF TIME"
LPrint "(1) INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; MG%(588)
LPrint "(2) USE BOTH SETUP RELIEVER AND CLOSER "; YN$(MG%(589)): LPrint
LPrint "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
LPrint "(3) ";: LPrint Using "START INNING IF SAVE OPPORTUNITY               ###"; MG%(582)
LPrint "(4) ";: LPrint Using "START INNING IF AHEAD                          ###"; MG%(583)
LPrint "(5) ";: LPrint Using "START INNING IF TIED OR AHEAD                  ###"; MG%(584)
LPrint "(6) ";: LPrint Using "IF CURRENT PITCHER TIRED AND AHEAD             ###"; MG%(585)
LPrint "(7) ";: LPrint Using "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; MG%(586)
LPrint "(8) ";: LPrint Using "WHEN SAVE OPPORTUNITY OCCURS                   ###"; MG%(587)

4029 LPrint
If MG%(590) <> 999 Then LPrint "NO PINCH HITTING FOR THE PITCHER STRATEGY DEFINED": GoTo 4030
LPrint Tab(26); "PINCH HITTING FOR THE PITCHER"
LPrint
LPrint "PINCH HIT FOR THE PITCHER FROM 'A' INNING ON"
LPrint "IF DOWN BY 'B' OR MORE RUNS AND"
LPrint "'C' OR MORE RUNNERS ON BASE AND"
LPrint "'D' OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN 'E'"
LPrint "'F' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO        A      B      C      D     E     F"
For I = 0 To 2
    LPrint Using "   #           ##     ##      #      #    ###   ###"; I + 1; MG%(I * 6 + 591); MG%(I * 6 + 592); MG%(I * 6 + 593); MG%(I * 6 + 594); MG%(I * 6 + 595); MG%(I * 6 + 596)
Next

4030 LPrint
If MG%(609) <> 999 Then LPrint "NO BUNTING STRATEGY DEFINED FOR THE PITCHER": GoTo 4031
LPrint "SACRIFICE BUNTING FOR THE PITCHER": LPrint
LPrint "'A' OUTS OR LESS"
LPrint "IF 'B' OR LESS RUNS AHEAD AND"
LPrint "'C' OR LESS RUNS BEHIND"
LPrint "'D' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                       A      B     C      D"
LPrint "1 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(610); MG%(611); MG%(612); MG%(613)
LPrint "    RUNNER ON 1ST           "
LPrint "2 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(614); MG%(615); MG%(616); MG%(617)
LPrint "    RUNNER ON 1ST           "
LPrint "3 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(618); MG%(619); MG%(620); MG%(621)
LPrint "    RUNNER ON 2ND           "
LPrint "4 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(622); MG%(623); MG%(624); MG%(625)
LPrint "    RUNNER ON 2ND           "
LPrint "5 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(626); MG%(627); MG%(628); MG%(629)
LPrint "    RUNNERS ON 1ST AND 2ND  "
LPrint "6 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(630); MG%(631); MG%(632); MG%(633)
LPrint "    RUNNERS ON 1ST AND 2ND  "

4031 LPrint
If MG%(634) <> 999 Then LPrint "NO BUNTING STRATEGY DEFINED FOR POSITION PLAYERS": GoTo 4032
LPrint "SACRIFICE BUNTING FOR THE POSITION PLAYERS": LPrint
LPrint "IF 'A' OR LESS RUNS AHEAD AND"
LPrint "'B' OR LESS RUNS BEHIND"
LPrint "BATTING AVERAGE IS LESS THAN 'C' AND SLUGGING PCT IS LESS THAN 'D'"
LPrint "PITCHERS ERA LESS THAN 'E' , 'F' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                        A     B      C      D      E     F"
LPrint "1 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(635); MG%(636); MG%(637); MG%(638); MG%(639); MG%(640)
LPrint "    0 OUTS, RUNNER ON 1ST"
LPrint "2 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(641); MG%(642); MG%(643); MG%(644); MG%(645); MG%(646)
LPrint "    0 OUTS, RUNNER ON 2ND"
LPrint "3 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(647); MG%(648); MG%(649); MG%(650); MG%(651); MG%(652)
LPrint "    0 OUTS, RUNNERS 1ST AND 2ND"
LPrint "4 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(653); MG%(654); MG%(655); MG%(656); MG%(657); MG%(658)
LPrint "    0 OUTS, RUNNER ON 1ST"
LPrint "5 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(659); MG%(660); MG%(661); MG%(662); MG%(663); MG%(664)
LPrint "    0 OUTS, RUNNER ON 2ND"
LPrint "6 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(665); MG%(666); MG%(667); MG%(668); MG%(669); MG%(670)
LPrint "    0 OUTS, RUNNERS ON 1ST AND 2ND"

4032 LPrint
If MG%(671) <> 999 Then LPrint "NO HIT AND RUN STRATEGY DEFINED": GoTo 4033
LPrint "HIT AND RUN STRATEGY": LPrint
LPrint "IF 'A' OR LESS RUNS AHEAD AND"
LPrint "'B' OR LESS RUNS BEHIND"
LPrint "'C' % OF THE TIME"
LPrint "'D' AB PER STRIKEOUT"
LPrint " SLUGGING PCT LESS THAN OR EQUAL TO 'E'"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "    SCENARIO                          A     B      C   D    E"
LPrint "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##    ###  ##  ###"; MG%(672); MG%(673); MG%(674); MG%(866); MG%(872)
LPrint "2 - 1 OUT, RUNNER ON 1ST             "; Using "##    ##    ###  ##  ###"; MG%(675); MG%(676); MG%(677); MG%(867); MG%(873)
LPrint "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; Using "##    ##    ###  ##  ###"; MG%(678); MG%(679); MG%(680); MG%(868); MG%(874)
LPrint "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; Using "##    ##    ###  ##  ###"; MG%(681); MG%(682); MG%(683); MG%(869); MG%(875)
LPrint "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; Using "##    ##    ###  ##  ###"; MG%(684); MG%(685); MG%(686); MG%(870); MG%(876)
LPrint "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; Using "##    ##    ###  ##  ###"; MG%(687); MG%(688); MG%(689); MG%(871); MG%(877)

4033 LPrint
If MG%(847) <> 999 Then LPrint "NO CRITERIA DEFINED FOR PINCH HITTING FOR THE BATTER": GoTo 4034
LPrint "PINCH HITTING FOR THE BATTER"
LPrint
LPrint "PINCH HIT FOR THE BATTER FROM 'A' INNING ON"
LPrint "IF DOWN BY 'B' OR MORE RUNS AND"
LPrint "'C' RUNNERS ON BASE AND"
LPrint "SLUGGING PCT IS LESS THAN 'D'"
LPrint "AND BATTING AVERAGE IS LESS THAN 'E'"
LPrint "'F' % OF THE TIME"
LPrint
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO        A      B      C      D       E     F"
For I = 0 To 2
    LPrint Using "   #           ##     ##      #     ###    ###   ###"; I + 1; MG%(I * 6 + 848); MG%(I * 6 + 849); MG%(I * 6 + 850); MG%(I * 6 + 851); MG%(I * 6 + 852); MG%(I * 6 + 853)
Next

4034 LPrint
If MG%(690) <> 999 Then LPrint "NO STRATEGY DEFINED FOR INFIELD IN WITH 3RD BASE OCCUPIED": GoTo 4035
LPrint "INFIELD IN WITH 3RD BASE OCCUPIED": LPrint
LPrint "IF DOWN BY 'A' RUNS"
LPrint "AND NOT MORE THAN 'B' RUNS"
LPrint "FROM 'C' INNING ON"
LPrint "'D' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                              A     B   C     D"
LPrint "1 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(691); MG%(692); MG%(693); MG%(694)
LPrint "2 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(695); MG%(696); MG%(697); MG%(698)
LPrint "3 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(699); MG%(700); MG%(701); MG%(702)
LPrint "4 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(703); MG%(704); MG%(705); MG%(706)

4035 LPrint
Cls
If MG%(707) <> 999 Then LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": GoTo 4036
LPrint "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": LPrint
LPrint "IF BUNT RATING IS >= 'A'"
LPrint "'B' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "      A      B"
LPrint Using "(1)  ##    ###"; MG%(708); MG%(709)
LPrint Using "(2)  ##    ###"; MG%(710); MG%(711)
LPrint Using "(3)  ##    ###"; MG%(712); MG%(713)

4036 LPrint
If MG%(714) <> 999 Then LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SAC HIT SITUATION": GoTo 4037
LPrint "CORNERS IN VS HITTER IN SAC HIT SITUATION": LPrint
LPrint "IF AHEAD BY  'A' RUNS OR LESS"
LPrint "OR BEHIND BY NOT MORE THAN 'B' RUNS"
LPrint "BUNT RATING >= 'C'"
LPrint "'D' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                              A     B   C     D"
LPrint "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(715); MG%(716); MG%(717); MG%(718)
LPrint "2 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(719); MG%(720); MG%(721); MG%(722)
LPrint "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(723); MG%(724); MG%(725); MG%(726)
LPrint "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(727); MG%(728); MG%(729); MG%(730)

4037 LPrint
If MG%(731) <> 999 Then LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS PITCHER IN SAC HIT SITUATION": GoTo 4038
LPrint "CORNERS IN VS PITCHER IN SAC HIT SITUATION": LPrint
LPrint "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO 'A'"
LPrint "'B' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                                  A      B"
LPrint "1 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(732); MG%(733)
LPrint "2 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(734); MG%(735)
LPrint "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(736); MG%(737)
LPrint "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(738); MG%(739)

4038 LPrint
If MG%(740) <> 999 Then LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SQUEEZE SITUATION": GoTo 4039
LPrint "CORNERS IN VS HITTER IN SQUEEZE SITUATION": LPrint
LPrint "IF AHEAD BY  'A' RUNS OR LESS"
LPrint "OR BEHIND BY NOT MORE THAN 'B' RUNS"
LPrint "BUNT RATING >= 'C'"
LPrint "'D' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                              A     B   C     D"
LPrint "1 - 0 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(741); MG%(742); MG%(743); MG%(744)
LPrint "2 - 1 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(745); MG%(746); MG%(747); MG%(748)

4039 LPrint
If MG%(749) <> 999 Then LPrint "NO STRATEGY DEFINED FOR GUARDING THE LINES": GoTo 4040
LPrint "GUARD LINES STRATEGY": LPrint
LPrint "IF 'A' RUNS AHEAD"
LPrint "'B' INNING ON"
LPrint "'C' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "     A     B      C"
LPrint "1 - "; Using "##    ##    ###"; MG%(750); MG%(751); MG%(752)
LPrint "2 - "; Using "##    ##    ###"; MG%(753); MG%(754); MG%(755)

4040 LPrint
If MG%(756) <> 999 Then LPrint "NO STRATEGY DEFINED FOR PITCHING AROUND HITTERS": GoTo 4041
LPrint "PITCH AROUND STRATEGY": LPrint
LPrint "SLUGGING PCT >= 'A'"
LPrint "'B' INNING ON"
LPrint "'C' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                                               A     B      C"
LPrint "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(757); MG%(758); MG%(759)
LPrint "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(760); MG%(761); MG%(762)
LPrint "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(763); MG%(764); MG%(765)

4041 LPrint
If MG%(766) <> 999 Then LPrint "NO STRATEGY DEFINED FOR THROWING TO THE PLATE": GoTo 4042
LPrint "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": LPrint
LPrint "FROM 'A' INNING ON"
LPrint "'B' OR MORE RUNS AHEAD"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "      A      B"
LPrint Using "(1)  ##    ###"; MG%(767); MG%(768)
LPrint Using "(2)  ##    ###"; MG%(769); MG%(770)

4042 LPrint
LPrint "# PINCH RUNNER"
For I = 1 To 6
    LPrint Using "# "; I;
    If MG%(I + 771) < 0 Then LPrint: GoTo 4043
    LPrint B$(MG%(I + 771))

4043 Next
LPrint
LPrint "DEFENSIVE SUBS FOR PINCH RUNNERS"
LPrint "SCORE TIED OR DOWN BY ONE RUN"
If MG%(778) <> 999 Then LPrint "NO SUBS DEFINED": GoTo 4044
LPrint "# PLAYER     I  SUBSTITUTE  %"
For I = 0 To 5
    LPrint Using "# "; I + 1;
    If MG%(I * 4 + 779) < 0 Then LPrint: GoTo 4045
    LPrint Left$(B$(MG%(I * 4 + 779)), 10); Tab(19);
    LPrint Using "## "; MG%(I * 4 + 780);
    LPrint Left$(B$(MG%(I * 4 + 781)), 10); Tab(33); Using "###"; MG%(I * 4 + 782)

4045 Next

4044 LPrint
If MG%(803) <> 999 Then LPrint "NO STRATEGY DEFINED TO PINCH RUN FOR PITCHER": GoTo 4046
LPrint "PINCH RUN FOR PITCHER": LPrint
LPrint "RUN RATING <= 'A'"
LPrint "'B' INNING ON"
LPrint "'C' % OF THE TIME"
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                A     B      C"
LPrint "1 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(804); MG%(805); MG%(806)
LPrint "2 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(807); MG%(808); MG%(809)

4046 LPrint
If MG%(810) <> 999 Then LPrint "NO BASE RUNNING STRATEGY DEFINED": GoTo 4047
LPrint "BASE RUNNING STRATEGY": LPrint
LPrint Tab(32); "TABLE OF VALUES"
LPrint "SCENARIO                            %       %          %          %"
LPrint "                                  SAFE    NORMAL   AGGRESSIVE   TOTAL"
LPrint " 1 - INNING 1-3, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(811); MG%(812); MG%(813); MG%(811) + MG%(812) + MG%(813)
LPrint " 2 - INNING 1-3, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(814); MG%(815); MG%(816); MG%(816) + MG%(815) + MG%(814)
LPrint " 3 - INNING 1-3, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(817); MG%(818); MG%(819); MG%(819) + MG%(818) + MG%(817)
LPrint " 4 - INNING 4-6, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(820); MG%(821); MG%(822); MG%(820) + MG%(821) + MG%(822)
LPrint " 5 - INNING 4-6, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(823); MG%(824); MG%(825); MG%(823) + MG%(824) + MG%(825)
LPrint " 6 - INNING 4-6, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(826); MG%(827); MG%(828); MG%(826) + MG%(827) + MG%(828)
LPrint " 7 - INNING 7-9, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(829); MG%(830); MG%(831); MG%(829) + MG%(830) + MG%(831)
LPrint " 8 - INNING 7-9, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(832); MG%(833); MG%(834); MG%(832) + MG%(833) + MG%(834)
LPrint " 9 - INNING 7-9, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(835); MG%(836); MG%(837); MG%(835) + MG%(836) + MG%(837)
LPrint "10 - INNING 9- , WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(838); MG%(839); MG%(840); MG%(838) + MG%(839) + MG%(840)
LPrint "11 - INNING 9- , WHEN TIED        "; Using "###     ###        ###          ###"; MG%(841); MG%(842); MG%(843); MG%(841) + MG%(842) + MG%(843)
LPrint "12 - INNING 9- , WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(844); MG%(845); MG%(846); MG%(844) + MG%(845) + MG%(846)

4047 LPrint
Return
' BRANCH IF ENTERING A NEW ROTATION OR EDITING EXISTING

4500 '*** PITCHING ROTATIONS ***
If Q = 0 Then Cls: GoTo 4604
GoTo 4516

4512 Cls

4515 If Not _FileExists(D$(3) + "MGR." + YN$) Then 4505
Open D$(3) + "MGR." + YN$ For Random As #2 Len = 2000
For I = 0 To 999
    Field #2, I * 2 As Q$, 2 As R$(I), 2000 - I * 2 - 2 As Q$
Next
Get #2, T
For I = 0 To 999
    MG%(I) = CVI(R$(I))
Next
Close 2

4505 Return
' HERE AND BELOW FROM THE PITCHING STRATEGY

4516 If MG%(99) <> 999 Then 4600

4517 GoSub 4520
Locate 1, 40: Print "(0) SIZE OF ROTATION IS "; MG%(1)
Locate 3, 40: Print "    STARTING ROTATION     % STARTS"
For I = 1 To MG%(1)
    Locate , 40
    Print Using "(#) "; I;
    If MG%(I + 2) < 0 Then Print: GoTo 4530
    Print P$(MG%(I + 2)); Tab(70); MG%(I + 12)

4530 Next
Locate 10, 40: Print "(6) MOVE TO SPOT STARTERS"
4536 I$ = GetKeyPress$
I = Val(I$): If I < 0 Or I > 6 Then 4536
If I = 0 Then GoSub 4550: GoTo 4517
If I >= 1 And I <= MG%(1) Then GoSub 4560: GoTo 4517
If I = 6 Then GoSub 4580
GoTo 4536

4550 Locate 12, 40: Input "ENTER ROTATION SIZE (1-5) "; MG%(1)
If MG%(1) < 1 Or MG%(1) > 5 Then 4550
Locate 12, 40: Print "                                "
For I = MG%(1) + 1 To 5: MG%(I + 2) = -1: MG%(I + 12) = -1: Next
Return

4560 Locate 12, 40: Print "SELECT STARTER #"; I
I$ = GetKeyPress$
GoSub 1290: I1 = Val(I$)
MG%(I + 2) = I1
Locate 12, 40: Print "                     "

4565 Locate 12, 40: Input "ENTER % OF TIME TO START "; MG%(I + 12)
If MG%(I + 12) < 0 Or MG%(I + 12) > 100 Then 4565
Locate 12, 40: Print "                               "
Return

4580 'SPOT STARTERS
GoSub 4520
Locate 1, 40: Print "     SPOT STARTERS         % USED"
SS% = 0
For I = 21 To 36
    Locate , 40
    Print Using "(##) "; I - 20;
    If MG%(I) < 0 Then Print: GoTo 4585
    Print P$(MG%(I)); Tab(70); MG%(I + 20)
    SS% = SS% + MG%(I + 20)

4585 Next
Locate 19, 40: Print "TOTAL MUST EQUAL 100"; Tab(70); SS%
Locate 20, 40: Print "(A)DD SPOT STARTER (E)DIT SPOT %"
Locate 21, 40: Print "(D)ELETE STARTER (S)AVE PROFILE"
4586 I$ = GetKeyPress$
If UCase$(I$) = "S" And SS% = 100 Then MG%(99) = 999: Call UPDATEFILE: GoTo 55
If UCase$(I$) = "D" Then GoSub 4582: GoTo 4580
If UCase$(I$) = "E" Then GoSub 4583: GoTo 4580
If UCase$(I$) = "A" Then GoSub 4570: GoTo 4580
GoTo 4586

4582 Locate 20, 40: Print "                                "
Locate 21, 40: Print "                                "
Locate 20, 40: Input "ENTER NUMBER TO DELETE (1-16) "; I1
If I1 < 1 Or I1 > 16 Then 4582
MG%(I1 + 20) = -1: MG%(I1 + 40) = -1
Return

4583 Locate 20, 40: Print "                                "
Locate 21, 40: Print "                                "
Locate 20, 40: Input "ENTER NUMBER TO EDIT (1-16) "; I1
If I1 < 1 Or I1 > 16 Then 4583

4584 Locate 20, 40: Input "ENTER % OF TIME TO SPOT START "; I2
If I2 < 0 Or I2 > 100 Then 4584
MG%(I1 + 40) = I2
Return

4570 Locate 20, 40: Print "                                "
Locate 21, 40: Print "                                "
Locate 20, 40: Input "ENTER SLOT TO ADD TO (1-16) "; I1
If I1 < 1 Or I1 > 16 Then 4570
Locate 21, 40: Print "SELECT PITCHER FROM LIST"
4571 I$ = GetKeyPress$
GoSub 1290: I2 = Val(I$)
MG%(I1 + 20) = I2

4575 Locate 20, 40: Print "                                "
Locate 21, 40: Print "                                "
Locate 20, 40: Input "ENTER % OF TIME TO SPOT START "; I2
If I2 < 0 Or I2 > 100 Then 4575
MG%(I1 + 40) = I2
Return

4600 Cls: Print "ERROR::: ROTATION FILE NOT FOUND"
Print
Print "DO YOU WANT TO ENTER A ROTATION (YN)"
4602 I$ = GetKeyPress$

4603 If UCase$(I$) <> "Y" Then 55

4604 I = 1: GoSub 6000
U$ = "COMPUTER DETERMINE ROTATION (YN) "
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
Print
4605 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then CR% = 1
Input "SEASON LENGTH (154, 162 ,ETC.)"; SL%

4610 Cls
Print Tab(20); "SELECT SIZE OF YOUR PITCHING ROTATION": Print
Color 14: Print "(1) ";: Color 15: Print "MAN ROTATION    ";: Color 14: Print "(4) ";: Color 15: Print "MAN ROTATION"
Color 14: Print "(2) ";: Color 15: Print "MAN ROTATION    ";: Color 14: Print "(5) ";: Color 15: Print "MAN ROTATION"
Color 14: Print "(3) ";: Color 15: Print "MAN ROTATION": Print
Print "       YOU MAY CONSIDER THE FOLLOWING GUIDELINES:"
Print "MAX GAMES STARTED BY ANY PITCHER           ROTATION SIZE"
Print "          32                                    5"
Print "          41                                    4"
Print "          54                                    3"
Print "          75                                    2"
Print "         ALL                                    1"
Print
Print "ALL PITCHERS AND GAMES STARTED"
For I = 0 To 6
    Locate I + 17, 1
    If P$(I) = "XXX" Then 4607
    Print P$(I); Tab(18); P%(I, 5);

    4607 Locate , 25
    If P$(I + 7) = "XXX" Then 4608
    Print P$(I + 7); Tab(43); P%(I + 7, 5);

    4608 Locate , 50
    If P$(I + 14) = "XXX" Then 4609
    Print P$(I + 14); Tab(68); P%(I + 14, 5)

4609 Next
4611 I$ = GetKeyPress$

4612 I = Val(I$): If I < 1 Or I > 5 Then 4611

4615 MG%(1) = I: MG%(2) = 0: For I = 3 To 36: MG%(I) = -1: Next: For I = 41 To 56: MG%(I) = 0: Next
MG%(0) = 999
If CR% = 1 Then 4690

4620 I% = 99
GoSub 4520

4625 For I = 0 To MG%(1) - 1

    4626 Locate 15, 59: Print "STARTER "; I + 1
    4628 I$ = GetKeyPress$

    4629 GoSub 1290

    4632 I1 = Val(I$): If I1 > 21 Or MG%(3) = I1 Or MG%(4) = I1 Or MG%(5) = I1 Or MG%(6) = I1 Or MG%(7) = I1 Then 4628

    4635 Locate , 59: Print P$(I1)

    4636 Locate , 59: Print "ANY CHANGE (YN)";
    4637 I$ = GetKeyPress$

    4638 If UCase$(I$) = "Y" Then For I2 = 15 To 17: Locate I2, 59: Print "                 ": Next: GoTo 4626

    4640 MG%(I + 3) = I1: For I2 = 15 To 17: Locate I2, 59: Print "                  ": Next
Next
GoTo 4700

4690 'COMPUTER SET ROTATION
CR% = 0
For I = 0 To 21
    If P$(I) = "XXX" Then GS%(I, 0) = 0: GS%(I, 1) = -1: GoTo 4691
    GS%(I, 0) = P%(I, 5): GS%(I, 1) = I

4691 Next
For Z = 0 To 20: For Z1 = Z + 1 To 21: If GS%(Z, 0) >= GS%(Z1, 0) Then 4695
    Swap GS%(Z, 0), GS%(Z1, 0): Swap GS%(Z, 1), GS%(Z1, 1)

4695 Next Z1: Next Z
For I = 0 To MG%(1) - 1
    MG%(I + 3) = GS%(I, 1)
Next
I1 = 0
For I = MG%(1) To 21
    I1 = I1 + 1
    If GS%(I, 0) = 0 Then 4700
    MG%(I1 + 20) = GS%(I, 1)
Next

4700 I1 = I1 - 1: If MG%(1) = 1 Then MG%(13) = 100: GoTo 4517
If MG%(1) = 2 Then GS% = 75
If MG%(1) = 3 Then GS% = 54
If MG%(1) = 4 Then GS% = 41
If MG%(1) = 5 Then GS% = 32
GS% = GS% * SL% / 162
For I = 1 To MG%(1)
    MG%(I + 12) = Int(P%(MG%(I + 2), 5) * 100 / GS% + .5)
Next
SS% = 0
For I = 21 To 36
    If MG%(I) < 0 Or MG%(I) > 21 Then 4705
    SS% = SS% + P%(MG%(I), 5)

4705 Next
For I = 21 To 36
    If MG%(I) < 0 Or MG%(I) > 21 Then 4710
    MG%(I + 20) = Int(P%(MG%(I), 5) * 100 / SS% + .5)

4710 Next
GoTo 4517
4095 I$ = GetKeyPress$
Return

4520 Cls

4521 Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Color 14, 0: Locate , 16: Print "T   G GS   W- L   ERA"
For I = 0 To 21
    If P$(I) = "XXX" Or Left$(P$(I), 1) = " " Or P$(I) = "" Then P%(I, 15) = 98
    If P%(I, 15) = 1 Or P$(I) = "XXX" Then 4525
    Color 2, 0
    If I <= 9 Then Print Using "#"; I;: Color 15, 0
    If I > 9 Then Print X$(I - 10);: Color 15, 0
    For I1 = 1 To MG%(1)
        If I = MG%(I1 + 2) Then Color 0, 7
    Next
    Locate , 3: Print P$(I);
    Color 15, 0
    Locate , 16: Print B1$(P%(I, 0) + 2);
    Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
    Locate , 24: Print Using "  ##"; P%(I, 1);: Print "-";: Print Using "## ##.##"; P%(I, 2); P%(I, 10) / 100

4525 Next I
Return

5000 'MIDDLE RELIEVERS
If MG%(60) <> 999 Then MG%(60) = 999: For I = 1 To 10: MG%(I + 60) = -1: MG%(I + 70) = 0: Next
GoSub 5100
Cls
Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Color 14, 0: Locate , 16: Print "T   G GS   W- L %RA   ERA"
For I = 0 To 21
    If P$(I) = "XXX" Or Left$(P$(I), 1) = " " Or P$(I) = "" Then 5005
    Color 2, 0
    If I <= 9 Then Print Using "#"; I;: Color 15, 0
    If I > 9 Then Print X$(I - 10);: Color 15, 0
    For I1 = 1 To 10
        If I = MG%(I1 + 60) Then Color L%(11), L%(12)
    Next
    Locate , 3: Print P$(I);
    Color 15, 0
    Locate , 16: Print B1$(P%(I, 0) + 2);
    Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
    Locate , 24: Print Using "  ##"; P%(I, 1);: Print "-";: Print Using "## ### ##.##"; P%(I, 2); (P%(I, 4) - P%(I, 5) - P%(I, 3)) / RA% * 100; P%(I, 10) / 100

5005 Next I
Locate 18, 45: Print "%RA - PITCHER'S PERCENTAGE OF TEAM'S"
Locate , 45: Print "RELIEF APPEARANCES MINUS SAVES"

5007 Locate 1, 48: Print "  MIDDLE RELIEVERS      % APPEAR"
For I = 1 To 10
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 60) < 0 Then Print: GoTo 5010
    Print P$(MG%(I + 60)); Tab(75); MG%(I + 70)

5010 Next

5011 Locate 13, 48: Print "(11) ACCEPT CHOICES"
Locate 14, 48: Print "(12) DELETE A RELIEVER"

5015 Locate 15, 48: Input "ENTER SELECTION (1-12)"; I
If I < 1 Or I > 12 Then 5015
If I >= 1 And I <= 10 Then GoSub 5020: GoTo 5000
If I = 11 Then Call UPDATEFILE: GoTo 55
If I = 12 Then GoSub 5050: GoTo 5000

5020 Locate 16, 48: Print Using "SELECT RELIEVER ##:"; I;
5021 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 60) = I1
'LOCATE 16,48:PRINT "                     "

5025 Locate 17, 48: Input "ENTER % OF TIME TO APPEAR "; MG%(I + 70)
If MG%(I + 70) < 0 Or MG%(I + 70) > 100 Then 5025
Locate 17, 48: Print "                               "
Locate 16, 48: Print "                               "
Return

5050 Locate 16, 48: Input "SELECT RELIEVER TO DELETE: "; I
If I < 1 Or I > 10 Then 5050
MG%(I + 60) = -1: MG%(I + 70) = 0
Return

5100 RA% = 0
For I = 0 To 21
    If P$(I) = "XXX" Then 5105
    RA% = RA% + P%(I, 4) - P%(I, 5) - P%(I, 3)

5105 Next
Return

5200 'SET-UP RELIEVERS
If MG%(90) <> 999 Then MG%(90) = 999: For I = 1 To 6: MG%(I + 80) = -1: MG%(I + 90) = 0: Next
Cls
Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Color 14, 0: Locate , 16: Print "T   G GS   W- L SV   ERA"
For I = 0 To 21
    If P$(I) = "XXX" Or Left$(P$(I), 1) = " " Or P$(I) = "" Then 5205
    Color 2, 0
    If I <= 9 Then Print Using "#"; I;: Color 15, 0
    If I > 9 Then Print X$(I - 10);: Color 15, 0
    For I1 = 1 To 6
        If I = MG%(I1 + 80) Then Color L%(11), L%(12)
    Next
    Locate , 3: Print P$(I);
    Color 15, 0
    Locate , 16: Print B1$(P%(I, 0) + 2);
    Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
    Locate , 24: Print Using "  ##"; P%(I, 1);: Print "-";: Print Using "## ### ##.##"; P%(I, 2); P%(I, 3); P%(I, 10) / 100

5205 Next I

5207 Locate 1, 48: Print "  SET-UP RELIEVERS      % APPEAR"
Locate , 48: Print "   -----  VS LEFT  -------      "
For I = 1 To 3
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 80) < 0 Then Print: GoTo 5210
    Print P$(MG%(I + 80)); Tab(75); MG%(I + 90)

5210 Next
Locate , 48: Print "   -----  VS RIGHT  -------      "
For I = 4 To 6
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 80) < 0 Then Print: GoTo 5212
    Print P$(MG%(I + 80)); Tab(75); MG%(I + 90)

5212 Next

5211 Locate 13, 48: Print "(7) ACCEPT CHOICES"
Locate 14, 48: Print "(8) DELETE A RELIEVER"

5215 Locate 15, 48: Input "ENTER SELECTION (1-8)"; I
If I < 1 Or I > 8 Then 5215
If I >= 1 And I <= 6 Then GoSub 5220: GoTo 5200
If I = 7 Then Call UPDATEFILE: GoTo 55
If I = 8 Then GoSub 5250: GoTo 5200

5220 Locate 16, 48: Print Using "SELECT RELIEVER ##:"; I;
5221 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 80) = I1
'LOCATE 16,48:PRINT "                     "

5225 Locate 17, 48: Input "ENTER % OF TIME TO APPEAR "; MG%(I + 90)
If MG%(I + 90) < 0 Or MG%(I + 90) > 100 Then 5225
Locate 17, 48: Print "                               "
Locate 16, 48: Print "                               "
Return

5250 Locate 16, 48: Input "SELECT RELIEVER TO DELETE: "; I
If I < 1 Or I > 6 Then 5250
MG%(I + 80) = -1: MG%(I + 90) = 0
Return

5300 'CLOSERS
If MG%(520) <> 999 Then MG%(520) = 999: For I = 1 To 6: MG%(I + 520) = -1: MG%(I + 530) = 0: Next
Cls
Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Color 14, 0: Locate , 16: Print "T   G GS   W- L SV   ERA"
For I = 0 To 21
    If P$(I) = "XXX" Or Left$(P$(I), 1) = " " Or P$(I) = "" Then 5305
    Color 2, 0
    If I <= 9 Then Print Using "#"; I;: Color 15, 0
    If I > 9 Then Print X$(I - 10);: Color 15, 0
    For I1 = 1 To 6
        If I = MG%(I1 + 520) Then Color L%(11), L%(12)
    Next
    Locate , 3: Print P$(I);
    Color 15, 0
    Locate , 16: Print B1$(P%(I, 0) + 2);
    Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
    Locate , 24: Print Using "  ##"; P%(I, 1);: Print "-";: Print Using "## ### ##.##"; P%(I, 2); P%(I, 3); P%(I, 10) / 100

5305 Next I

5307 Locate 1, 48: Print "          CLOSERS       % APPEAR"
Locate , 48: Print "   -----  VS LEFT  -------      "
For I = 1 To 3
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 520) < 0 Then Print: GoTo 5310
    Print P$(MG%(I + 520)); Tab(75); MG%(I + 530)

5310 Next
Locate , 48: Print "   -----  VS RIGHT  -------      "
For I = 4 To 6
    Locate , 48
    Print Using "(##) "; I;
    If MG%(I + 520) < 0 Then Print: GoTo 5312
    Print P$(MG%(I + 520)); Tab(75); MG%(I + 530)

5312 Next

5311 Locate 13, 48: Print "(7) ACCEPT CHOICES"
Locate 14, 48: Print "(8) DELETE A RELIEVER"

5315 Locate 15, 48: Input "ENTER SELECTION (1-8)"; I
If I < 1 Or I > 8 Then 5315
If I >= 1 And I <= 6 Then GoSub 5320: GoTo 5300
If I = 7 Then Call UPDATEFILE: GoTo 55
If I = 8 Then GoSub 5350: GoTo 5300

5320 Locate 16, 48: Print Using "SELECT RELIEVER ##:"; I;
5321 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 520) = I1
'LOCATE 16,48:PRINT "                     "

5325 Locate 17, 48: Input "ENTER % OF TIME TO APPEAR "; MG%(I + 530)
If MG%(I + 530) < 0 Or MG%(I + 530) > 100 Then 5325
Locate 17, 48: Print "                               "
Locate 16, 48: Print "                               "
Return

5350 Locate 16, 48: Input "SELECT RELIEVER TO DELETE: "; I
If I < 1 Or I > 6 Then 5350
MG%(I + 520) = -1: MG%(I + 530) = 0
Return

3000 'HITTING STRATEGIES
Cls
Print Tab(28);: Color 14, 4: Print "HITTING STRATEGIES": Color 15, 0: Print
Erase menuItems$
menuItems$(1) = "SITUATIONS TO PH FOR PITCHER                 "
menuItems$(2) = "SITUATIONS FOR PITCHER TO SAC BUNT IN        "
menuItems$(3) = "SITUATONS FOR POSITION PLAYERS TO SAC BUNT IN"
menuItems$(4) = "HIT AND RUN SCENARIOS                        "
menuItems$(5) = "SITUATIONS TO PH FOR POSITION PLAYERS        "
menuItems$(6) = "RETURN TO MAIN MENU                          "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 13
Call box0(4, 12, 11, 58, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
I = SELECTION%
If I = 6 Or SCANCODE% = 27 Then 45
If I = 1 Then Call PHPITCHER: GoTo 3000
If I = 2 Then 3100
If I = 3 Then 3150
If I = 4 Then 3200
If I = 5 Then Call PHBATTER: GoTo 3000

3100 'SAC BUNT HIT BY PITCHER"
Cls
If MG%(609) <> 999 Then MG%(609) = 999: For I = 1 To 24: MG%(609 + I) = 0: Next
Locate 1, 24: Color 14, 2: Print "SACRIFICE BUNTING FOR THE PITCHER": Color 15, 0: Print
Color 15, 1: Print " A ";: Color 15, 0: Print " OUTS OR LESS"
Print "IF ";: Color 15, 1: Print " B ";: Color 15, 0: Print " OR LESS RUNS AHEAD AND"
Color 15, 1: Print " C ";: Color 15, 0: Print " OR LESS RUNS BEHIND"
Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                       A      B     C      D"
Print "1 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(610); MG%(611); MG%(612); MG%(613)
Print "    RUNNER ON 1ST           "
Print "2 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(614); MG%(615); MG%(616); MG%(617)
Print "    RUNNER ON 1ST           "
Print "3 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(618); MG%(619); MG%(620); MG%(621)
Print "    RUNNER ON 2ND           "
Print "4 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(622); MG%(623); MG%(624); MG%(625)
Print "    RUNNER ON 2ND           "
Print "5 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(626); MG%(627); MG%(628); MG%(629)
Print "    RUNNERS ON 1ST AND 2ND  "
Print "6 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(630); MG%(631); MG%(632); MG%(633)
Print "    RUNNERS ON 1ST AND 2ND  "
Print "7 - ACCEPT CHOICES"

3105 Locate 22, 1: Input "ENTER SELECTION (1-7) "; I
If I < 1 Or I > 7 Then 3105
If I = 7 Then Call UPDATEFILE: GoTo 3000
I = I - 1
Locate 22, 1: Input "OUTS OR LESS        "; MG%(I * 4 + 610)
Locate 22, 1: Input "RUNS AHEAD OR LESS  "; MG%(I * 4 + 611)
Locate 22, 1: Input "RUNS BEHIND OR LESS "; MG%(I * 4 + 612)
Locate 22, 1: Input "PERCENT OF TIME     "; MG%(I * 4 + 613)
GoTo 3100

3150 'SAC BUNT HIT BY POSITION PLAYERS"
Cls
If MG%(634) <> 999 Then MG%(634) = 999: For I = 1 To 36: MG%(634 + I) = 0: Next
Locate 1, 24: Color 14, 2: Print "SACRIFICE BUNTING FOR THE POSITION PLAYERS": Color 15, 0: Print
Print "IF ";: Color 15, 1: Print " A ";: Color 15, 0: Print " OR LESS RUNS AHEAD AND"
Color 15, 1: Print " B ";: Color 15, 0: Print " OR LESS RUNS BEHIND"
Print "BATTING AVERAGE IS LESS THAN ";: Color 15, 1: Print " C ";: Color 15, 0: Print " AND SLUGGING PCT IS LESS THAN ";: Color 15, 1: Print " D "
Color 15, 0: Print "PITCHERS ERA LESS THAN ";: Color 15, 1: Print " E ";: Color 15, 0: Print ", ";: Color 15, 1: Print " F ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                        A     B      C      D      E     F"
Print "1 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(635); MG%(636); MG%(637); MG%(638); MG%(639); MG%(640)
Print "    0 OUTS, RUNNER ON 1ST"
Print "2 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(641); MG%(642); MG%(643); MG%(644); MG%(645); MG%(646)
Print "    0 OUTS, RUNNER ON 2ND"
Print "3 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(647); MG%(648); MG%(649); MG%(650); MG%(651); MG%(652)
Print "    0 OUTS, RUNNERS 1ST AND 2ND"
Print "4 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(653); MG%(654); MG%(655); MG%(656); MG%(657); MG%(658)
Print "    0 OUTS, RUNNER ON 1ST"
Print "5 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(659); MG%(660); MG%(661); MG%(662); MG%(663); MG%(664)
Print "    0 OUTS, RUNNER ON 2ND"
Print "6 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(665); MG%(666); MG%(667); MG%(668); MG%(669); MG%(670)
Print "    0 OUTS, RUNNERS ON 1ST AND 2ND"
Print "7 - ACCEPT CHOICES"

3155 Locate 22, 1: Input "ENTER SELECTION (1-7) "; I
If I < 1 Or I > 7 Then 3155
If I = 7 Then Call UPDATEFILE: GoTo 3000
I = I - 1
Locate 22, 1: Input "RUNS AHEAD OR LESS        "; MG%(I * 6 + 635)
Locate 22, 1: Input "RUNS BEHIND OR LESS       "; MG%(I * 6 + 636)
Locate 22, 1: Input "BATTING AVERAGE LESS THAN "; MG%(I * 6 + 637)
Locate 22, 1: Input "SLUGGING PCT LESS THAT    "; MG%(I * 6 + 638)
Locate 22, 1: Input "PITCHER'S ERA LESS THAN   "; MG%(I * 6 + 639)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 6 + 640)
GoTo 3150

3200 'HIT AND RUN
Cls
If MG%(671) <> 999 Then MG%(671) = 999: For I = 1 To 18: MG%(671 + I) = 0: Next
Locate 1, 30: Color 14, 2: Print "HIT AND RUN STRATEGY": Color 15, 0: Print
Print "IF ";: Color 15, 1: Print " A ";: Color 15, 0: Print " OR LESS RUNS AHEAD AND"
Color 15, 1: Print " B ";: Color 15, 0: Print " OR LESS RUNS BEHIND"
Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
Color 15, 1: Print " D ";: Color 15, 0: Print " AB PER STRIKEOUT"
Print " SLUGGING PCT LESS THAN OR EQUAL TO ";: Color 15, 1: Print " E ": Color 15, 0
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "    SCENARIO                          A     B      C   D    E"
Print "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##    ###  ##  ###"; MG%(672); MG%(673); MG%(674); MG%(866); MG%(872)
Print "2 - 1 OUT, RUNNER ON 1ST             "; Using "##    ##    ###  ##  ###"; MG%(675); MG%(676); MG%(677); MG%(867); MG%(873)
Print "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; Using "##    ##    ###  ##  ###"; MG%(678); MG%(679); MG%(680); MG%(868); MG%(874)
Print "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; Using "##    ##    ###  ##  ###"; MG%(681); MG%(682); MG%(683); MG%(869); MG%(875)
Print "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; Using "##    ##    ###  ##  ###"; MG%(684); MG%(685); MG%(686); MG%(870); MG%(876)
Print "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; Using "##    ##    ###  ##  ###"; MG%(687); MG%(688); MG%(689); MG%(871); MG%(877)
Print "7 - ACCEPT CHOICES"

3205 Locate 22, 1: Input "ENTER SELECTION (1-7) "; I
If I < 1 Or I > 7 Then 3205
If I = 7 Then Call UPDATEFILE: GoTo 3000
I = I - 1
Locate 22, 1: Input "RUNS AHEAD OR LESS        "; MG%(I * 3 + 672)
Locate 22, 1: Input "RUNS BEHIND OR LESS       "; MG%(I * 3 + 673)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 674)
Locate 22, 1: Input "AB PER STRIKEOUT          "; MG%(I + 866)
Locate 22, 1: Input "SLUGGING PCT <=           "; MG%(I + 872)
GoTo 3200

3300 'DEFENSIVE STRATEGIES
Cls
Print Tab(31);: Color 14, 4: Print "DEFENSIVE STRATEGIES": Color 15, 0: Print
Erase menuItems$
menuItems$(1) = "INFIELD IN WITH 3RD BASE OCCUPIED            "
menuItems$(2) = "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT"
menuItems$(3) = "CORNERS IN VS HITTER IN SAC BUNT SITUATION   "
menuItems$(4) = "CORNERS IN VS PITCHER IN SAC BUNT SITUATION  "
menuItems$(5) = "CORNERS IN VS HITTER WHO MIGHT SQUEEZE BUNT  "
menuItems$(6) = "GUARD LINES IN LATE INNINGS                  "
menuItems$(7) = "PITCH AROUND THE HITTER                      "
menuItems$(8) = "NOT THROWING HOME WITH RUNNER TRYING TO SCORE"
menuItems$(9) = "RETURN TO MAIN MENU                          "
SELECTION% = 1
Count% = 9
Start% = 1
SCANCODE% = 9
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 18
Call box0(4, 17, 14, 63, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
I = SELECTION%
If I = 9 Or SCANCODE% = 27 Then 45
If I = 1 Then 3350
If I = 2 Then 3375
If I = 3 Then 3400
If I = 4 Then 3425
If I = 5 Then 3450
If I = 6 Then 3475
If I = 7 Then Call PITCHAROUND: GoTo 3300
If I = 8 Then Call THROWHOME: GoTo 3300

3350 'INFIELD IN
Cls
If MG%(690) <> 999 Then MG%(690) = 999: For I = 1 To 16: MG%(690 + I) = 0: Next
Locate 1, 30: Color 14, 2: Print "INFIELD IN WITH 3RD BASE OCCUPIED": Color 15, 0: Print
Print "IF DOWN BY ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS"
Print "AND NOT MORE THAN ";: Color 15, 1: Print " B ";: Color 15, 0: Print " RUNS"
Print "FROM ";: Color 15, 1: Print " C ";: Color 15, 0: Print "INNING ON"
Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                              A     B   C     D"
Print "1 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(691); MG%(692); MG%(693); MG%(694)
Print "2 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(695); MG%(696); MG%(697); MG%(698)
Print "3 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(699); MG%(700); MG%(701); MG%(702)
Print "4 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(703); MG%(704); MG%(705); MG%(706)
Print "5 - ACCEPT CHOICES"

3355 Locate 22, 1: Input "ENTER SELECTION (1-5) "; I
If I < 1 Or I > 5 Then 3355
If I = 5 Then Call UPDATEFILE: GoTo 3300
I = I - 1
Locate 22, 1: Input "AT LEAST DOWN BY          "; MG%(I * 4 + 691)
Locate 22, 1: Input "BUT NOT MORE THAN         "; MG%(I * 4 + 692)
Locate 22, 1: Input "THIS INNING ON            "; MG%(I * 4 + 693)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 4 + 694)
GoTo 3350

3375 'DEFENSIVE STRATEGIES"
Cls
If MG%(707) <> 999 Then MG%(707) = 999: For I = 1 To 6: MG%(707 + I) = 0: Next
Locate 1, 22: Color 14, 2: Print "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": Color 15, 0: Print
Print "IF BUNT RATING IS >= ";: Color 15, 1: Print " A ": Color 15, 0
Color 15, 1: Print " B ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "      A      B"
Print Using "(1)  ##    ###"; MG%(708); MG%(709)
Print Using "(2)  ##    ###"; MG%(710); MG%(711)
Print Using "(3)  ##    ###"; MG%(712); MG%(713)
Print "(4) - ACCEPT CHOICES"

3378 Locate 22, 1: Input "ENTER SELECTION (1-4) "; I
If I < 1 Or I > 4 Then 3378
If I = 4 Then Call UPDATEFILE: GoTo 3300
I = I - 1
Locate 22, 1: Input "BUNT RATING >=            "; MG%(I * 2 + 708)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 2 + 709)
GoTo 3375

3400 'HITTER SAC BUNT
Cls
If MG%(714) <> 999 Then MG%(714) = 999: For I = 1 To 16: MG%(714 + I) = 0: Next
Locate 1, 20: Color 14, 2: Print "CORNERS IN VS HITTER IN SAC HIT SITUATION": Color 15, 0: Print
Print "IF AHEAD BY  ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS OR LESS"
Print "OR BEHIND BY NOT MORE THAN ";: Color 15, 1: Print " B ";: Color 15, 0: Print " RUNS"
Print "BUNT RATING >= ";: Color 15, 1: Print " C ": Color 15, 0
Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                              A     B   C     D"
Print "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(715); MG%(716); MG%(717); MG%(718)
Print "2 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(719); MG%(720); MG%(721); MG%(722)
Print "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(723); MG%(724); MG%(725); MG%(726)
Print "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(727); MG%(728); MG%(729); MG%(730)
Print "5 - ACCEPT CHOICES"

3405 Locate 22, 1: Input "ENTER SELECTION (1-5) "; I
If I < 1 Or I > 5 Then 3405
If I = 5 Then Call UPDATEFILE: GoTo 3300
I = I - 1
Locate 22, 1: Input "AHEAD BY NOT MORE THAN    "; MG%(I * 4 + 715)
Locate 22, 1: Input "DOWN BY NOT MORE THAN     "; MG%(I * 4 + 716)
Locate 22, 1: Input "BUNT RATING >=            "; MG%(I * 4 + 717)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 4 + 718)
GoTo 3400

3425 'PITCHER SAC BUNT
Cls
If MG%(731) <> 999 Then MG%(731) = 999: For I = 1 To 8: MG%(731 + I) = 0: Next
Locate 1, 20: Color 14, 2: Print "CORNERS IN VS PITCHER IN SAC HIT SITUATION": Color 15, 0: Print
Print "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO ";: Color 15, 1: Print " A "
Color 15, 1: Print " B ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                                  A      B"
Print "1 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(732); MG%(733)
Print "2 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(734); MG%(735)
Print "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(736); MG%(737)
Print "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(738); MG%(739)
Print "5 - ACCEPT CHOICES"

3430 Locate 22, 1: Input "ENTER SELECTION (1-5) "; I
If I < 1 Or I > 5 Then 3430
If I = 5 Then Call UPDATEFILE: GoTo 3300
I = I - 1
Locate 22, 1: Input "BATTING AVG LESS THAN     "; MG%(I * 2 + 732)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 2 + 733)
GoTo 3425

3450 'HITTER SAC BUNT SQUEEZE
Cls
If MG%(740) <> 999 Then MG%(740) = 999: For I = 1 To 8: MG%(740 + I) = 0: Next
Locate 1, 20: Color 14, 2: Print "CORNERS IN VS HITTER IN SQUEEZE SITUATION": Color 15, 0: Print
Print "IF AHEAD BY  ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS OR LESS"
Print "OR BEHIND BY NOT MORE THAN ";: Color 15, 1: Print " B ";: Color 15, 0: Print " RUNS"
Print "BUNT RATING >= ";: Color 15, 1: Print " C ": Color 15, 0
Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                              A     B   C     D"
Print "1 - 0 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(741); MG%(742); MG%(743); MG%(744)
Print "2 - 1 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(745); MG%(746); MG%(747); MG%(748)
Print "3 - ACCEPT CHOICES"

3455 Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
If I < 1 Or I > 3 Then 3455
If I = 3 Then Call UPDATEFILE: GoTo 3300
I = I - 1
Locate 22, 1: Input "AHEAD BY NOT MORE THAN    "; MG%(I * 4 + 741)
Locate 22, 1: Input "DOWN BY NOT MORE THAN     "; MG%(I * 4 + 742)
Locate 22, 1: Input "BUNT RATING >=            "; MG%(I * 4 + 743)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 4 + 744)
GoTo 3450

3475 'GUARD LINES
Cls
If MG%(749) <> 999 Then MG%(749) = 999: For I = 1 To 6: MG%(749 + I) = 0: Next
Locate 1, 30: Color 14, 2: Print "GUARD LINES STRATEGY": Color 15, 0: Print
Print "IF ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS AHEAD"
Color 15, 1: Print " B ";: Color 15, 0: Print " INNING ON"
Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "     A     B      C"
Print "1 - "; Using "##    ##    ###"; MG%(750); MG%(751); MG%(752)
Print "2 - "; Using "##    ##    ###"; MG%(753); MG%(754); MG%(755)
Print "3 - ACCEPT CHOICES"

3476 Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
If I < 1 Or I > 3 Then 3476
If I = 3 Then Call UPDATEFILE: GoTo 3300
I = I - 1
Locate 22, 1: Input "RUNS AHEAD                "; MG%(I * 3 + 750)
Locate 22, 1: Input "INNING ON                 "; MG%(I * 3 + 751)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 752)
GoTo 3475

3501 'PINCH RUNNING STRATEGY
Cls
Print Tab(31);: Color 14, 2: Print "PINCH RUNNING STRATEGIES": Color 15, 0: Print
Erase menuItems$
menuItems$(1) = "SELECT PINCH RUNNERS                                  "
menuItems$(2) = "SELECT PLAYERS TO PINCH RUN FOR AND THEIR REPLACEMENTS"
menuItems$(3) = "SITUATIONS TO PINCH RUN FOR THE PITCHER               "
menuItems$(4) = "RETURN TO MAIN MENU                                   "
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 13
Call box0(4, 12, 9, 67, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
I = SELECTION%
If I = 4 Or SCANCODE% = 27 Then 45
If I = 1 Then 3500
If I = 2 Then 3600
If I = 3 Then 3680

3500 'PINCH RUNNERS
Cls: Print Tab(26);: Color 14, 4: Print "PINCH RUNNING STRATEGY OPTIONS": Print: Print: Color , 0
If MG%(771) <> 999 Then MG%(771) = 999: For I = 1 To 6: MG%(I + 771) = -1: Next
Cls
Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Locate , 16: Print "RUN RATING SB   CS"
I1 = 0
For I = 0 To 22: Locate , 1
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then I1 = I1 + 1: GoTo 3505
    Locate I + 2 - I1, 1
    Color 2, 0
    If I < 10 Then Print Using "#"; I;: Color 15, 0: GoSub 1980
    If I > 9 Then Print X$(I - 10);: Color 15, 0: GoSub 1980
    Locate , 3: Print B$(I);: Color 15, 0
    Locate , 17: Print Using "   #     ###  ###"; B%(I, 16); B%(I, 13); B%(I, 14);

3505 Next
Locate 1, 46
Print "# PLAYER"
For I = 1 To 6
    Locate , 46
    Print Using "# "; I;
    If MG%(I + 771) < 0 Then Print: GoTo 3510
    Print B$(MG%(I + 771))

3510 Next
Locate 9, 46: Print "(7) ACCEPT CHOICES"
Locate 10, 46: Print "(8) DELETE A CHOICE"

3515 Locate 11, 46: Input "ENTER SELECTION (1-8)"; I
If I < 1 Or I > 8 Then 3515
If I >= 1 And I <= 6 Then GoSub 3520: GoTo 3500
If I = 7 Then Call UPDATEFILE: GoTo 3501
If I = 8 Then GoSub 3525: GoTo 3500

3520 Locate 12, 46: Print Using "SELECT RUNNER #:"; I;
3517 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I + 771) = I1
Return

3525 Locate 12, 46: Input "SELECT CHOICE TO DELETE: "; I
If I < 1 Or I > 6 Then 3525
MG%(I + 771) = -1
Return

3600 'DEFENSIVE REPLACEMENTS
Cls
Locate 20, 46: Print "DEFENSIVE SUBS FOR PINCH RUNNERS"
Locate 21, 46: Print "SCORE TIED OR DOWN BY ONE RUN"
If MG%(778) <> 999 Then MG%(778) = 999: For I = 1 To 24: MG%(I + 778) = -1: Next
Locate 1, 1
Print "#";: Color L%(11), L%(12): Print T$;
Locate , 17: Print "1 FAVG 2 FAVG 3 FAVG 4 FAVG"
I1 = 0
For I = 0 To 22: Locate , 1
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then I1 = I1 + 1: GoTo 3605
    Locate I + 2 - I1, 1
    Color 2, 0
    If I < 10 Then Print Using "#"; I;: Color 15, 0: GoSub 1980
    If I > 9 Then Print X$(I - 10);: Color 15, 0: GoSub 1980
    Locate , 3: Print B$(I);: Color 15, 0
    Locate , 17: If B%(I, 22) <> -1 Then Print Using "# ####"; B%(I, 22); B%(I, 26);
    Locate , 24: If B%(I, 23) <> -1 Then Print Using "# ####"; B%(I, 23); B%(I, 27);
    Locate , 31: If B%(I, 24) <> -1 Then Print Using "# ####"; B%(I, 24); B%(I, 28);
    Locate , 38: If B%(I, 25) <> -1 Then Print Using "# ####"; B%(I, 25); B%(I, 29);

3605 Next
Locate 1, 46
Print "# PLAYER     I  SUBSTITUTE  %"
For I = 0 To 5
    Locate , 46
    Print Using "# "; I + 1;
    If MG%(I * 4 + 779) < 0 Then Print: GoTo 3610
    Print Left$(B$(MG%(I * 4 + 779)), 10); Tab(59);
    Print Using "## "; MG%(I * 4 + 780);
    Print Left$(B$(MG%(I * 4 + 781)), 10); Tab(73); Using "###"; MG%(I * 4 + 782)

3610 Next
Locate 9, 46: Print "(7) ACCEPT CHOICES"
Locate 10, 46: Print "(8) DELETE A CHOICE"

3611 Locate 11, 46: Input "ENTER SELECTION (1-8)"; I
If I < 1 Or I > 8 Then 3611
If I >= 1 And I <= 6 Then GoSub 3615: GoTo 3600
If I = 7 Then Call UPDATEFILE: GoTo 3501
If I = 8 Then GoSub 3625: GoTo 3600

3615 I = I - 1
Locate 12, 46: Print Using "SELECT HITTER #:"; I;
3616 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I * 4 + 779) = I1
Locate 13, 46: Input "INNING "; I1
MG%(I * 4 + 780) = I1
Locate 14, 46: Print "SELECT SUB #:";
3618 I$ = GetKeyPress$
GoSub 1290
I1 = Val(I$)
MG%(I * 4 + 781) = I1

3620 Locate 15, 46: Input "ENTER % OF TIME TO USE "; MG%(I * 4 + 782)
If MG%(I * 4 + 782) < 0 Or MG%(I * 4 + 782) > 100 Then 3620
Return

3625 Locate 12, 46: Input "SELECT CHOICE TO DELETE: "; I
If I < 1 Or I > 6 Then 298
MG%(I * 4 + 779) = -1: MG%(I * 4 + 780) = 0: MG%(I * 4 + 781) = 0: MG%(I * 4 + 782) = 0
Return

3680 'PINCH RUN PITCHER
Cls
If MG%(803) <> 999 Then MG%(803) = 999: For I = 1 To 6: MG%(803 + I) = 0: Next
Locate 1, 30: Color 14, 2: Print "PINCH RUN FOR PITCHER": Color 15, 0: Print
Print "RUN RATING <= ";: Color 15, 1: Print " A ": Color 15, 0
Color 15, 1: Print " B ";: Color 15, 0: Print " INNING ON"
Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
Print "SCENARIO                A     B      C"
Print "1 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(804); MG%(805); MG%(806)
Print "2 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(807); MG%(808); MG%(809)
Print "3 - ACCEPT CHOICES"

3685 Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
If I < 1 Or I > 3 Then 3685
If I = 3 Then Call UPDATEFILE: GoTo 3501
I = I - 1
Locate 22, 1: Input "RUN RATING <=             "; MG%(I * 3 + 804)
Locate 22, 1: Input "INNING ON                 "; MG%(I * 3 + 805)
Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 806)
GoTo 3680

6000 JJ = 1
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

3800 GoSub 1445
GoSub 4512
If MG%(0) = 999 Then 3820

' NO MANAGER FILE WAS FOUND, THUS UNABLE TO EXPORT ONE
Locate 10, 8
Call CSMsgBox("NO MANAGER PROFILE EXISTS FOR THIS TEAM PRESS ANY KEY TO RETURN TO MAIN MENU", 40, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 3895

3820 If PP% = 1 Then PP% = 0: GoTo 3850

Call DRAWBOX

Locate , 11: Input "ENTER 8 CHARACTER NAME FOR PROFILE"; PF$
For X = 1 To 8
    If Mid$(PF$, X, 1) = " " Or Mid$(PF$, X, 1) = "." Then Mid$(PF$, X, 1) = "_"
Next X

Open D$(0) + PF$ + "." + "MGR" For Output As #1
For I = 0 To 999
    Print #1, MG%(I)
Next
Close 1
GoTo 3895

3850 ' PRINT THE MANAGER PROFILE

3895 Return

3900 Spec$ = D$(0) + "*.MGR"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%

ReDim managerFiles$(0 To Count%)
For X = 1 To Count%
    managerFiles$(X) = Space$(12)
Next
managerFiles$(0) = Spec$
Call ReadFile(managerFiles$())
'CALL SortStr(VARPTR(managerFiles$(1)), Count, 0)

3905 Cls
Color , 1: Cls: Locate , 28: Color 14
Print "SELECT PROFILE TO IMPORT": Color 15
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
MAMenu managerFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 3925
targetFile$ = managerFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3905
L% = 0
Call DRAWBOX
Locate , 11: Print "IMPORT STRATEGY PERCENTAGES ONLY (YN)"

3909 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then L% = 1
Open D$(0) + targetFile$ For Input As #1
If L% = 0 Then For I = 0 To 999: Input #1, MG%(I): Next
If L% = 1 Then For I = 0 To 580: Input #1, L%: Next: For I = 581 To 769: Input #1, MG%(I): Next: For I = 770 To 802: Input #1, L%: Next: For I = 803 To 999: Input #1, MG%(I): Next
Close 1
Call UPDATEFILE

3925 Return


'------------------------------
'   BASERUN Subroutine
'------------------------------
' ...explanation...
Sub BASERUN ()

    3700 'BASE RUNNING STRATEGY
    Cls
    If MG%(810) <> 999 Then MG%(810) = 999: For I = 1 To 36: MG%(810 + I) = 0: Next
    Locate 1, 30: Color 14, 2: Print "BASE RUNNING STRATEGY": Color 15, 0: Print
    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
    Print "SCENARIO                            %       %          %          %"
    Print "                                  SAFE    NORMAL   AGGRESSIVE   TOTAL"
    Print " 1 - INNING 1-3, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(811); MG%(812); MG%(813); MG%(811) + MG%(812) + MG%(813)
    Print " 2 - INNING 1-3, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(814); MG%(815); MG%(816); MG%(816) + MG%(815) + MG%(814)
    Print " 3 - INNING 1-3, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(817); MG%(818); MG%(819); MG%(819) + MG%(818) + MG%(817)
    Print " 4 - INNING 4-6, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(820); MG%(821); MG%(822); MG%(820) + MG%(821) + MG%(822)
    Print " 5 - INNING 4-6, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(823); MG%(824); MG%(825); MG%(823) + MG%(824) + MG%(825)
    Print " 6 - INNING 4-6, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(826); MG%(827); MG%(828); MG%(826) + MG%(827) + MG%(828)
    Print " 7 - INNING 7-9, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(829); MG%(830); MG%(831); MG%(829) + MG%(830) + MG%(831)
    Print " 8 - INNING 7-9, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(832); MG%(833); MG%(834); MG%(832) + MG%(833) + MG%(834)
    Print " 9 - INNING 7-9, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(835); MG%(836); MG%(837); MG%(835) + MG%(836) + MG%(837)
    Print "10 - INNING 9- , WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(838); MG%(839); MG%(840); MG%(838) + MG%(839) + MG%(840)
    Print "11 - INNING 9- , WHEN TIED        "; Using "###     ###        ###          ###"; MG%(841); MG%(842); MG%(843); MG%(841) + MG%(842) + MG%(843)
    Print "12 - INNING 9- , WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(844); MG%(845); MG%(846); MG%(844) + MG%(845) + MG%(846)
    Print "13 - ACCEPT CHOICES"

    3705 Locate 22, 1: Input "ENTER SELECTION (1-13) "; I
    If I < 1 Or I > 13 Then 3705
    If I = 13 Then GoSub 3810: GoTo 3815
    I = I - 1
    Locate 22, 1: Input "SAFE                 "; MG%(I * 3 + 811)
    Locate 22, 1: Input "NORMAL               "; MG%(I * 3 + 812)
    Locate 22, 1: Input "AGGRESSIVE           "; MG%(I * 3 + 813)
    GoTo 3700

    3810 Open D$(3) + "MGR." + YN$ For Random As #2 Len = 2000
    For I = 0 To 999
        Field #2, I * 2 As Q$, 2 As R$(I), 2000 - I * 2 - 2 As Q$
    Next
    For I = 0 To 999: LSet R$(I) = MKI$(MG%(I)): Next
    Put #2, T: Close 2
    Return

3815 End Sub


'------------------------------
'   DRAWBOX Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOX ()
    JJ = 5
    Locate 6, 8: Color 15, 1
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
	Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
	Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
End Sub


'------------------------------
'   PHBATTER Subroutine
'------------------------------
' ...explanation...
Sub PHBATTER ()

    3059 'PINCH HIT FOR THE PITCHER
    If MG%(847) <> 999 Then MG%(847) = 999: For I = 848 To 865: MG%(I) = 0: Next
    Cls
    Locate 1, 26: Color 14, 4: Print "PINCH HITTING FOR THE BATTER": Color 15, 0
    Print
    Print "PINCH HIT FOR THE BATTER FROM ";: Color 15, 1: Print " A ";: Color 15, 0
    Print " INNING ON"
    Print "IF DOWN BY ";: Color 15, 1: Print " B ";: Color 15, 0: Print " OR MORE RUNS AND"
    Color 15, 1: Print " C ";: Color 15, 0: Print " RUNNERS ON BASE AND"
    Print "SLUGGING PCT IS LESS THAN ";: Color 15, 1: Print " D ";: Color 15, 0
    Print "AND BATTING AVERAGE IS LESS THAN ";: Color 15, 1: Print " E "
    Print " F ";: Color 15, 0: Print " % OF THE TIME"
    Print
    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
    Print "SCENARIO        A      B      C      D       E     F"
    For I = 0 To 2
        Print Using "   #           ##     ##      #     ###    ###   ###"; I + 1; MG%(I * 6 + 848); MG%(I * 6 + 849); MG%(I * 6 + 850); MG%(I * 6 + 851); MG%(I * 6 + 852); MG%(I * 6 + 853)
    Next
    Print
    Print "   4 - ACCEPT CHOICES"
    Print

    3057 Locate 17, 1: Input "ENTER SELECTION (1-4) "; I
    If I < 1 Or I > 4 Then 3057
    If I = 4 Then Call UPDATEFILE: GoTo 3058
    I = I - 1
    Input "INNING TO BEGIN PINCH HITTING FOR POSITION PLAYERS "; MG%(I * 6 + 848)
    Input "DOWN BY HOW MANY RUNS "; MG%(I * 6 + 849)
    Input "RUNNERS ON BASE "; MG%(I * 6 + 850)
    Input "SLUGGING PCT LESS THAN "; MG%(I * 6 + 851)
    Input "BATTING AVERAGE LESS THAN "; MG%(I * 6 + 852)
    Input "PERCENT OF TIME "; MG%(I * 6 + 853)
    GoTo 3059

3058 End Sub


'------------------------------
'   PHPITCHER Subroutine
'------------------------------
' ...explanation...
Sub PHPITCHER ()

    3050 'PINCH HIT FOR THE PITCHER
    If MG%(590) <> 999 Then MG%(590) = 999: For I = 591 To 608: MG%(I) = 0: Next
    Cls
    Locate 1, 26: Color 14, 4: Print "PINCH HITTING FOR THE PITCHER": Color 15, 0
    Print
    Print "PINCH HIT FOR THE PITCHER FROM ";: Color 15, 1: Print " A ";: Color 15, 0
    Print " INNING ON"
    Print "IF DOWN BY ";: Color 15, 1: Print " B ";: Color 15, 0: Print " OR MORE RUNS AND"
    Color 15, 1: Print " C ";: Color 15, 0: Print " OR MORE RUNNERS ON BASE AND"
    Color 15, 1: Print " D ";: Color 15, 0
    Print " OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN ";: Color 15, 1: Print " E "
    Print " F ";: Color 15, 0: Print " % OF THE TIME"
    Print
    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
    Print "SCENARIO        A      B      C      D     E     F"
    For I = 0 To 2
        Print Using "   #           ##     ##      #      #    ###   ###"; I + 1; MG%(I * 6 + 591); MG%(I * 6 + 592); MG%(I * 6 + 593); MG%(I * 6 + 594); MG%(I * 6 + 595); MG%(I * 6 + 596)
    Next
    Print
    Print "   4 - ACCEPT CHOICES"
    Print

    3055 Locate 17, 1: Input "ENTER SELECTION (1-4) "; I
    If I < 1 Or I > 4 Then 3055
    If I = 4 Then Call UPDATEFILE: GoTo 3056
    I = I - 1
    Input "ENTER INNING TO BEGIN PINCH HITTING FOR PITCHER "; MG%(I * 6 + 591)
    Input "DOWN BY HOW MANY RUNS "; MG%(I * 6 + 592)
    Input "RUNNERS ON BASE "; MG%(I * 6 + 593)
    Input "NUMBER OF OUTS OR LESS "; MG%(I * 6 + 594)
    Input "PITCHER HITS LESS THAN "; MG%(I * 6 + 595)
    Input "PERCENT OF TIME "; MG%(I * 6 + 596)
    GoTo 3050

3056 End Sub


'------------------------------
'   PITCHAROUND Subroutine
'------------------------------
' ...explanation...
Sub PITCHAROUND ()

    3480 'PITCH AROUND
    Cls
    If MG%(756) <> 999 Then MG%(756) = 999: For I = 1 To 9: MG%(756 + I) = 0: Next
    Locate 1, 30: Color 14, 2: Print "PITCH AROUND STRATEGY": Color 15, 0: Print
    Print "SLUGGING PCT >= ";: Color 15, 1: Print " A ": Color 15, 0
    Color 15, 1: Print " B ";: Color 15, 0: Print " INNING ON"
    Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
    Print "SCENARIO                                               A     B      C"
    Print "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(757); MG%(758); MG%(759)
    Print "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(760); MG%(761); MG%(762)
    Print "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(763); MG%(764); MG%(765)
    Print "4 - ACCEPT CHOICES"

    3485 Locate 22, 1: Input "ENTER SELECTION (1-4) "; I
    If I < 1 Or I > 4 Then 3485
    If I = 4 Then Call UPDATEFILE: GoTo 3486
    I = I - 1
    Locate 22, 1: Input "SLUGGING PCT >=           "; MG%(I * 3 + 757)
    Locate 22, 1: Input "INNING ON                 "; MG%(I * 3 + 758)
    Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 759)
    GoTo 3480

3486 End Sub


'------------------------------
'   PITSTRAT Subroutine
'------------------------------
' ...explanation...
Sub PITSTRAT ()

    400 'PITCHING STRATEGY
    Cls
    If MG%(581) <> 999 Then MG%(581) = 999: For I = 582 To 587: MG%(I) = 0: Next: MG%(588) = 8: MG%(589) = 0
    If MG%(588) < 8 Then MG%(588) = 8
    If MG%(588) > 9 Then MG%(588) = 9
    Print Tab(31);: Color 14, 4: Print "CLOSING STRATEGY": Color 15, 0
    Print
    Color 12: Print "STRATEGY                                        % OF TIME"
    Color 14: Print "(1) ";: Color 15: Print "INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; MG%(588)
    Color 14: Print "(2) ";: Color 15: Print "USE BOTH SETUP RELIEVER AND CLOSER "; YN$(MG%(589)): Print
    Color 14: Print "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
    Color 14: Print "(3) ";: Color 15: Print Using "START INNING IF SAVE OPPORTUNITY               ###"; MG%(582)
    Color 14: Print "(4) ";: Color 15: Print Using "START INNING IF AHEAD                          ###"; MG%(583)
    Color 14: Print "(5) ";: Color 15: Print Using "START INNING IF TIED OR AHEAD                  ###"; MG%(584)
    Color 14: Print "(6) ";: Color 15: Print Using "IF CURRENT PITCHER TIRED AND AHEAD             ###"; MG%(585)
    Color 14: Print "(7) ";: Color 15: Print Using "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; MG%(586)
    Color 14: Print "(8) ";: Color 15: Print Using "WHEN SAVE OPPORTUNITY OCCURS                   ###"; MG%(587)
    Color 14: Print "(9) ";: Color 15: Print "ACCEPT CHOICES"
    405 I$ = GetKeyPress$
    I = Val(I$): If I < 1 Or I > 9 Then 405
    If I = 9 Then Call UPDATEFILE: GoTo 415
    If I = 1 Then Locate 21, 1: Input "ENTER INNING "; MG%(588): GoTo 400
    If I = 2 Then MG%(589) = 1 - MG%(589): GoTo 400

    410 Locate 21, 1: Input "% OF TIME TO USE "; I1
    If I1 < 0 Or I1 > 100 Then 410
    MG%(I - 2 + 581) = I1: GoTo 400

415 End Sub


'------------------------------
'   PROFILEPRINT Subroutine
'------------------------------
' ...explanation...
Sub PROFILEPRINT ()
End Sub


'------------------------------
'   THROWHOME Subroutine
'------------------------------
' ...explanation...
Sub THROWHOME ()

    3490 'THROW HOME UNDER
    Cls
    If MG%(766) <> 999 Then MG%(766) = 999: For I = 1 To 4: MG%(766 + I) = 0: Next
    Locate 1, 18: Color 14, 2: Print "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": Color 15, 0: Print
    Print "FROM ";: Color 15, 1: Print " A ";: Color 15, 0: Print " INNING ON"
    Color 15, 1: Print " B ";: Color 15, 0: Print " OR MORE RUNS AHEAD"
    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
    Print "      A      B"
    Print Using "(1)  ##    ###"; MG%(767); MG%(768)
    Print Using "(2)  ##    ###"; MG%(769); MG%(770)
    Print "(3) - ACCEPT CHOICES"

    3495 Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
    If I < 1 Or I > 3 Then 3495
    If I = 3 Then Call UPDATEFILE: GoTo 3496
    I = I - 1
    Locate 22, 1: Input "INNING ON TO USE          "; MG%(I * 2 + 767)
    Locate 22, 1: Input "AHEAD BY                  "; MG%(I * 2 + 768)
    GoTo 3490

3496 End Sub


'------------------------------
'   UPDATEFILE Subroutine
'------------------------------
' ...explanation...
Sub UPDATEFILE ()
    MG%(0) = 999
    Open D$(3) + "MGR." + YN$ For Random As #2 Len = 2000
    For I = 0 To 999
        Field #2, I * 2 As Q$, 2 As R$(I), 2000 - I * 2 - 2 As Q$
    Next
    For I = 0 To 999: LSet R$(I) = MKI$(MG%(I)): Next
    Put #2, T: Close 2
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
