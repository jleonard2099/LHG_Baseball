'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Sort By Position"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim D$(4), Q$(1844), FS%(22, 4, 4), TM(40), A$(40), TYN$(40), BB$(23), PB$(22)
Dim B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21), B0%(22, 40)
Dim P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21)
Dim I2 As Single, I3 As Single, I4 As Single, menuItems$(4)
Dim K9(0) As Long
Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, D$(I)
Next
Close #1

6 Call TITLE
menuItems$(1) = "CREATE POSITION SEARCH FILE   "
menuItems$(2) = "SEARCH FOR PLAYERS BY POSITION"
menuItems$(3) = "RETURN TO MAIN MENU           "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 22
Call box0(5, 21, 9, 52, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I = SELECTION%

22 If I = 1 Then Run "POSCR.EXE"

23 If I = 2 Then 100

24 If I = 3 Then Run "HELLO.EXE"

100 Cls
Spec$ = D$(0) + "*.POS"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500

ReDim positionFiles$(0 To Count%)
For X = 1 To Count%
    positionFiles$(X) = Space$(12)
Next
positionFiles$(0) = Spec$
Call ReadFile(positionFiles$())
'CALL SortStr(VARPTR(positionFiles$(1)), Count, 0)

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
MAMenu positionFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 6
targetFile$ = positionFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 LGE$ = RTrim$(targetFile$)

110 Cls: SS = 0: Color 11: Locate 5, 30: Print "S E L E C T"

111 Locate 7, 28: Color 12: Print "0. ";: Color 7: Print "Catchers"

112 Locate , 28: Color 12: Print "1. ";: Color 7: Print "First Basemen"

113 Locate , 28: Color 12: Print "2. ";: Color 7: Print "Second Basemen"

114 Locate , 28: Color 12: Print "3. ";: Color 7: Print "Shortstops"

115 Locate , 28: Color 12: Print "4. ";: Color 7: Print "Third Basemen"

116 Locate , 28: Color 12: Print "5. ";: Color 7: Print "Left Fielders"

117 Locate , 28: Color 12: Print "6. ";: Color 7: Print "Center Fielders"

118 Locate , 28: Color 12: Print "7. ";: Color 7: Print "Right Fielders"

119 Locate , 28: Color 12: Print "8. ";: Color 7: Print "Pitchers"

120 Locate , 28: Color 12: Print "9. ";: Color 7: Print "Return To Main Menu"

130 Z$ = GetKeyPress$

131 Z = Val(Z$): If Z < 0 Or Z > 9 Then 130

140 If Z = 0 Then PS = 2: PS$ = "CATCHERS": GoTo 200

141 If Z = 1 Then PS = 3: PS$ = "FIRST BASEMEN": GoTo 200

142 If Z = 2 Then PS = 4: PS$ = "SECOND BASEMEN": GoTo 200

143 If Z = 3 Then PS = 6: PS$ = "SHORTSTOPS": GoTo 200

144 If Z = 4 Then PS = 5: PS$ = "THIRD BASEMEN": GoTo 200

145 If Z = 5 Then PS = 7: PS$ = "LEFT FIELDERS": GoTo 200

146 If Z = 6 Then PS = 8: PS$ = "CENTER FIELDERS": GoTo 200

147 If Z = 7 Then PS = 9: PS$ = "RIGHT FIELDERS": GoTo 200

148 If Z = 8 Then PS = 1: GoTo 650

149 If Z = 9 Then GoTo 6

200 Locate 18, 28: Color 12: Print "P";: Color 7: Print "RINT OR ";: Color 12: Print "V";: Color 7: Print "IEW"
201 I$ = GetKeyPress$

202 If UCase$(I$) = "P" Then 2305

203 If UCase$(I$) = "V" Then 300

204 If UCase$(I$) <> "P" Or UCase$(I$) <> "V" Then 201

300 Open D$(0) + LGE$ For Input As #1

301 For J = 1 To 40
    Input #1, TM(J), A$(J), TYN$(J)
Next

302 Close #1

306 Cls
Locate 1, 35: Color 15: Print PS$

307 Locate 3: Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"

350 For J = 1 To 40

    351 If TM(J) = 0 Then 410

    352 GoSub 5500

    370 For I = 0 To 22
        If B$(I) = "XXX" Or B$(I) = "" Then 409

        371 If B%(I, 22) = PS Then I2 = 33: GoTo 372
        If B%(I, 23) = PS Then I2 = 35: GoTo 372
        If B%(I, 24) = PS Then I2 = 37: GoTo 372
        If B%(I, 25) = PS Then I2 = 39: GoTo 372
        GoTo 409

        372 SS = SS + 1

        373 If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Then GoSub 500: GoSub 602

        374 If B%(I, 22) = PS Then 375 Else GoTo 376

        375 Color 12: Print Left$(B$(I), 10);: GoTo 377

        376 Color 2: Print Left$(B$(I), 10);

        377 Color 3: Print Tab(12); Left$(T$, 2); NN$;

        378 If B%(I, 0) = -1 Then BB$(I) = "L"

        379 If B%(I, 0) = 1 Then BB$(I) = "R"

        380 If B%(I, 0) = 0 Then BB$(I) = "S"
        If B%(I, 0) = 2 Then BB$(I) = "RP"
        If B%(I, 0) = -2 Then BB$(I) = "LP"

        381 Color 7: Print Tab(19); BB$(I); Using "#"; B%(I, I2);
        'PRINT B%(I,

        382 Print Tab(22); Using "###"; B%(I, 3);

        383 Print Tab(26); Using "###"; B%(I, 4);

        384 Print Tab(31); Using "###"; B%(I, 6);

        385 Print Tab(35); Using "##"; B%(I, 7);

        386 Print Tab(38); Using "##"; B%(I, 8);

        387 Print Tab(41); Using "##"; B%(I, 9);

        388 Print Tab(44); Using "###"; B%(I, 10);

        389 Print Tab(48); Using "###"; B%(I, 5);

        390 Print Tab(52); Using "###"; B%(I, 11);

        391 Print Tab(56); Using "###"; B%(I, 12);

        392 Print Tab(59); Using "###"; B%(I, 13);

        393 Print Tab(63); Using "##"; B%(I, 14);

394 I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)

        395 If B%(I, 4) = 0 Or I2 = 0 Then Print Tab(66); ".000";

        396 If I2 > 0 And I2 < 100 Then Print Tab(66); ".0";: Print Using "##"; I2;

        397 If I2 >= 100 Then Print Tab(66); ".";: Print Using "###"; I2;

        398 If B%(I, 4) = 0 Or I3 = 0 Then Print Tab(71); ".000";

        399 If I3 > 0 And I3 < 100 Then Print Tab(71); ".0";: Print Using "##"; I3;

        400 If I3 >= 100 Then Print Tab(71); ".";: Print Using "###"; I3;

        401 If B%(I, 4) = 0 Or I4 = 0 Then Print Tab(76); ".000";

        402 If I4 > 0 And I4 < 100 Then Print Tab(76); ".0";: Print Using "##"; I4;

        403 If I4 >= 100 Then Print Tab(76); ".";: Print Using "###"; I4

    409 Next I

410 Next J

415 Locate 23: Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
416 I$ = GetKeyPress$

420 GoTo 110

500 Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
501 I$ = GetKeyPress$

601 Return

602 Cls: Locate 1, 35: Color 15: Print PS$

603 Locate 3: Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"

604 Return

650 SS = 0: Cls: Color 11: Locate 5, 30: Print "S E L E C T"

651 Locate 7, 16: Color 12: Print "1. ";: Color 7: Print "Pitchers With Less Than 10 Games Started"

652 Locate , 16: Color 12: Print "2. ";: Color 7: Print "Pitchers With More Than 10 Games Started"

653 Locate , 16: Color 12: Print "3. ";: Color 7: Print "Right-Handed Pitchers"

654 Locate , 16: Color 12: Print "4. ";: Color 7: Print "Left-Handed Pitchers"
660 I$ = GetKeyPress$

661 I = Val(I$): If I < 1 Or I > 4 Then 660

662 If I = 1 Then PPS = 10: PC = 1: PPS$ = "PITCHERS WITH LESS THAN 10 GAMES STARTED": GoTo 680

663 If I = 2 Then PPS = 10: PC = 2: PPS$ = "PITCHERS WITH MORE THAN 10 GAMES STARTED": GoTo 680

664 If I = 3 Then THR = 1: PPS$ = "RIGHT-HANDED PITCHERS": GoTo 1680

665 If I = 4 Then THR = 2: PPS$ = "LEFT-HANDED PITCHERS": GoTo 1680

680 Locate 18, 28: Color 12: Print "P";: Color 7: Print "RINT OR ";: Color 12: Print "V";: Color 7: Print "IEW"
681 I$ = GetKeyPress$

682 If UCase$(I$) = "P" Then 3705

683 If UCase$(I$) = "V" Then 700

684 If UCase$(I$) <> "P" Or UCase$(I$) <> "V" Then 681

700 Open D$(0) + LGE$ For Input As #1

701 For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next

702 Close #1

706 Cls: Color 15: Locate 1, 20: Print PPS$

707 Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

750 For J = 1 To 40

    751 If TM(J) = 0 Then 810

    752 GoSub 5500

    767 For I = 0 To 21
        If P$(I) = "XXX" Or P$(I) = "" Then 800

        768 If PC = 1 Then 769 Else GoTo 770

        769 If P%(I, 5) < PPS Then 772 Else GoTo 800

        770 If P%(I, 5) > PPS Then 772 Else GoTo 800

        772 SS = SS + 1

        773 If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Or SS = 235 Or SS = 253 Or SS = 271 Or SS = 289 Or SS = 307 Or SS = 325 Or SS = 343 Or SS = 361 Or SS = 379 Or SS = 397 Or SS = 415 Then GoSub 850: GoSub 902

        775 Color 12: Print Left$(P$(I), 10);

        776 Color 3: Print Tab(12); Left$(T$, 2); NN$

        777 If P%(I, 0) = 1 Then PB$(I) = "R"

        778 If P%(I, 0) = -1 Then PB$(I) = "L"

        779 Color 7: Print Tab(22); PB$(I);

        780 Print Tab(25); Using "###"; P%(I, 4);

        781 Print Tab(30); Using "##"; P%(I, 1);

        782 Print Tab(34); Using "##"; P%(I, 2);

        783 Print Tab(39); Using "##"; P%(I, 3);

        784 Print Tab(43); Using "##"; P%(I, 5);

        785 Print Tab(47); Using "###"; P%(I, 6);

        786 Print Tab(52); Using "###"; P%(I, 7);

        787 Print Tab(56); Using "###"; P%(I, 8);

        788 Print Tab(61); Using "###"; P%(I, 9);

        789 Print Tab(65); Using "##.##"; P%(I, 10) / 100;

        790 Print Tab(73); Using "##"; P%(I, 13)

    800 Next I

810 Next J

815 Locate 23: Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
816 I$ = GetKeyPress$

820 GoTo 110

850 Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
851 I$ = GetKeyPress$

901 Return

902 Cls: Color 15: Locate 1, 20: Print PPS$

903 Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

904 Return

1680 Locate 18, 28: Color 12: Print "P";: Color 7: Print "RINT OR ";: Color 12: Print "V";: Color 7: Print "IEW"
1681 I$ = GetKeyPress$

1682 If UCase$(I$) = "P" Then 4705

1683 If UCase$(I$) = "V" Then 1700

1684 If UCase$(I$) <> "P" Or UCase$(I$) <> "V" Then 1681

1700 Open D$(0) + LGE$ For Input As #1

1701 For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next

1702 Close #1

1706 Cls: Color 15: Locate 1, 35: Print PPS$

1707 Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

1750 For J = 1 To 40

    1751 If TM(J) = 0 Then 1810

    1752 GoSub 5500

    1767 For I = 0 To 21
        If P$(I) = "XXX" Or P$(I) = "" Then 1800

        1768 If THR = 1 Then 1769 Else GoTo 1770

        1769 If P%(I, 0) = 1 Then 1772 Else GoTo 1800

        1770 If P%(I, 0) = -1 Then 1772 Else GoTo 1800

        1772 SS = SS + 1

        1773 If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Or SS = 235 Or SS = 253 Or SS = 271 Or SS = 289 Or SS = 307 Or SS = 325 Or SS = 343 Or SS = 361 Or SS = 379 Or SS = 397 Or SS = 415 Then GoSub 850: GoSub 902

        1775 Color 12: Print Left$(P$(I), 10);

        1776 Color 3: Print Tab(12); Left$(T$, 2); NN$;

        1777 If P%(I, 0) = 1 Then PB$(I) = "R"

        1778 If P%(I, 0) = -1 Then PB$(I) = "L"

        1779 Color 7: Print Tab(22); PB$(I);

        1780 Print Tab(25); Using "###"; P%(I, 4);

        1781 Print Tab(30); Using "##"; P%(I, 1);

        1782 Print Tab(34); Using "##"; P%(I, 2);

        1783 Print Tab(39); Using "##"; P%(I, 3);

        1784 Print Tab(43); Using "##"; P%(I, 5);

        1785 Print Tab(47); Using "###"; P%(I, 6);

        1786 Print Tab(52); Using "###"; P%(I, 7);

        1787 Print Tab(56); Using "###"; P%(I, 8);

        1788 Print Tab(61); Using "###"; P%(I, 9);

        1789 Print Tab(65); Using "##.##"; P%(I, 10) / 100;

        1790 Print Tab(73); Using "##"; P%(I, 13)

    1800 Next I

1810 Next J

1815 Locate 23: Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
1816 I$ = GetKeyPress$

1820 GoTo 110

1850 Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
1851 I$ = GetKeyPress$

1852 Return

1853 Color 15: Locate 1, 20: Print PPS$

1854 Cls: Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

1855 Return

2305 Open D$(0) + LGE$ For Input As #1

2306 For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next

2307 Close #1

2310 LPrint Chr$(15)

'2311 WIDTH "LPT1:1", 130

2313 LPrint Tab(55); PS$

2314 LPrint: LPrint "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"

2350 For J = 1 To 40

    2351 If TM(J) = 0 Then 2410

    2352 GoSub 5500

    2370 For I = 0 To 22
        If B$(I) = "XXX" Or B$(I) = "" Then 2409

        2371 If B%(I, 22) = PS Or B%(I, 23) = PS Or B%(I, 24) = PS Or B%(I, 25) = PS Then 2372 Else GoTo 2409

        2372 SS = SS + 1

        2373 If SS = 51 Or SS = 101 Or SS = 151 Or SS = 201 Then GoSub 2600

        2374 If B%(I, 22) = PS Then 2375 Else GoTo 2376

        2375 LPrint B$(I);: GoTo 2377

        2376 LPrint Tab(3); B$(I);

        2377 LPrint Tab(16); Left$(T$, 2); NN$;

        2378 If B%(I, 0) = -1 Then BB$(I) = "L"

        2379 If B%(I, 0) = 1 Then BB$(I) = "R"

        2380 If B%(I, 0) = 0 Then BB$(I) = "S"

        2381 LPrint Tab(34); BB$(I);

        2382 LPrint Tab(39); Using "###"; B%(I, 3);

        2383 LPrint Tab(45); Using "###"; B%(I, 4);

        2384 LPrint Tab(52); Using "###"; B%(I, 6);

        2385 LPrint Tab(58); Using "##"; B%(I, 7);

        2386 LPrint Tab(63); Using "##"; B%(I, 8);

        2387 LPrint Tab(68); Using "##"; B%(I, 9);

        2388 LPrint Tab(73); Using "###"; B%(I, 10);

        2389 LPrint Tab(80); Using "###"; B%(I, 5);

        2390 LPrint Tab(85); Using "###"; B%(I, 11);

        2391 LPrint Tab(91); Using "###"; B%(I, 12);

        2392 LPrint Tab(96); Using "###"; B%(I, 13);

        2393 LPrint Tab(102); Using "##"; B%(I, 14);

2394 I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)

        2395 If B%(I, 4) = 0 Or I2 = 0 Then LPrint Tab(107); ".000";

        2396 If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;

        2397 If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;

        2398 If B%(I, 4) = 0 Or I3 = 0 Then LPrint Tab(114); ".000";

        2399 If I3 > 0 And I3 < 100 Then LPrint Tab(114); ".0";: LPrint Using "##"; I3;

        2400 If I3 >= 100 Then LPrint Tab(114); ".";: LPrint Using "###"; I3;

        2401 If B%(I, 4) = 0 Or I4 = 0 Then LPrint Tab(121); ".000";

        2402 If I4 > 0 And I4 < 100 Then LPrint Tab(121); ".0";: LPrint Using "##"; I4;

        2403 If I4 >= 100 Then LPrint Tab(121); ".";: LPrint Using "###"; I4

    2409 Next I

2410 Next J

2411 LPrint Chr$(12)

2412 LPrint Chr$(18)

'2413 WIDTH "LPT1:1", 80

2415 GoTo 110

2600 LPrint Chr$(12)

2601 LPrint Tab(55); PS$

2602 LPrint: LPrint "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"

2603 Return

3705 Open D$(0) + LGE$ For Input As #1

3706 For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next

3707 Close #1

3711 LPrint Chr$(15)

'3712 WIDTH "LPT1:1", 130

3713 LPrint Tab(50); PPS$

3714 LPrint: LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

3750 For J = 1 To 40

    3751 If TM(J) = 0 Then 3810

    3752 GoSub 5500

    3767 For I = 0 To 21
        If P$(I) = "XXX" Or P$(I) = "" Then 3800

        3768 If PC = 1 Then 3769 Else GoTo 3770

        3769 If P%(I, 5) < PPS Then 3772 Else GoTo 3800

        3770 If P%(I, 5) > PPS Then 3772 Else GoTo 3800

        3772 SS = SS + 1

        3773 If SS = 51 Or SS = 101 Or SS = 151 Or SS = 201 Or SS = 251 Or SS = 301 Or SS = 351 Or SS = 401 Then GoSub 3900

        3775 LPrint P$(I);

        3776 LPrint Tab(19); Left$(T$, 2); NN$;

        3777 If P%(I, 0) = 1 Then PB$(I) = "R"

        3778 If P%(I, 0) = -1 Then PB$(I) = "L"

        3779 LPrint Tab(35); PB$(I);

        3780 LPrint Tab(39); Using "###"; P%(I, 4);

        3781 LPrint Tab(45); Using "##"; P%(I, 1);

        3782 LPrint Tab(50); Using "##"; P%(I, 2);

        3783 LPrint Tab(56); Using "##"; P%(I, 3);

        3784 LPrint Tab(62); Using "##"; P%(I, 5);

        3785 LPrint Tab(68); Using "###"; P%(I, 6);

        3786 LPrint Tab(73); Using "###"; P%(I, 7);

        3787 LPrint Tab(79); Using "###"; P%(I, 8);

        3788 LPrint Tab(85); Using "###"; P%(I, 9);

        3789 LPrint Tab(90); Using "##.##"; P%(I, 10) / 100;

        3790 LPrint Tab(99); Using "##"; P%(I, 13)

    3800 Next I

3810 Next J

3811 LPrint Chr$(12)

3812 LPrint Chr$(18)

'3813 WIDTH "LPT1:1", 80

3814 GoTo 110

3900 LPrint Chr$(12)

3901 LPrint Tab(50); PPS$

3902 LPrint: LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

3903 Return

4705 Open D$(0) + LGE$ For Input As #1

4706 For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next

4707 Close #1

4711 LPrint Chr$(15)

'4712 WIDTH "LPT1:1", 130

4713 LPrint Tab(50); PPS$

4714 LPrint: LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

4750 For J = 1 To 40

    4751 If TM(J) = 0 Then 4810

    4752 GoSub 5500

    4767 For I = 0 To 21
        If P$(I) = "XXX" Or P$(I) = "" Then 4800

        4768 If THR = 1 Then 4769 Else GoTo 4770

        4769 If P%(I, 0) = 1 Then 4772 Else GoTo 4800

        4770 If P%(I, 0) = -1 Then 4772 Else GoTo 4800

        4772 SS = SS + 1

        4773 If SS = 51 Or SS = 101 Or SS = 151 Or SS = 201 Or SS = 251 Or SS = 301 Or SS = 351 Or SS = 401 Then GoSub 4900

        4775 LPrint P$(I);

        4776 LPrint Tab(19); Left$(T$, 2); NN$;

        4777 If P%(I, 0) = 1 Then PB$(I) = "R"

        4778 If P%(I, 0) = -1 Then PB$(I) = "L"

        4779 LPrint Tab(35); PB$(I);

        4780 LPrint Tab(39); Using "###"; P%(I, 4);

        4781 LPrint Tab(45); Using "##"; P%(I, 1);

        4782 LPrint Tab(50); Using "##"; P%(I, 2);

        4783 LPrint Tab(56); Using "##"; P%(I, 3);

        4784 LPrint Tab(62); Using "##"; P%(I, 5);

        4785 LPrint Tab(68); Using "###"; P%(I, 6);

        4786 LPrint Tab(73); Using "###"; P%(I, 7);

        4787 LPrint Tab(79); Using "###"; P%(I, 8);

        4788 LPrint Tab(85); Using "###"; P%(I, 9);

        4789 LPrint Tab(90); Using "##.##"; P%(I, 10) / 100;

        4790 LPrint Tab(99); Using "##"; P%(I, 13)

    4800 Next I

4810 Next J

4811 LPrint Chr$(12)

4812 LPrint Chr$(18)

'4813 WIDTH "LPT1:1", 80

4814 GoTo 110

4900 LPrint Chr$(12)

4901 LPrint Tab(50); PPS$

4902 LPrint: LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

4903 Return

4500 Locate 10, 8
Call CSMsgBox("NO LEAGUE POSITION FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 6

5500 Open D$(0) + "FCTEAMS." + TYN$(J) For Random As #1 Len = 4342
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
Get #1, TM(J)
T$ = RTrim$(Q$(0))
For I = 1 To 13: L%(I) = CVI(Q$(I)): Next: K9(0) = CVS(Q$(14))
For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
    For I1 = 0 To 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
Next: Next

For I = 0 To 21: P$(I) = RTrim$(Q$(I * 38 + 1004))
    For I1 = 0 To 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
Next: Next

S$ = RTrim$(Q$(1840)): M$ = RTrim$(Q$(1841)): NN$ = RTrim$(Q$(1842)): CK = CVI(Q$(1843))
Close 1
Return


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
