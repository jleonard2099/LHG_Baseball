'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - View Career Files"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

5 Cls: TMR = 2700
Dim TT(100)

6 Dim B$(100), P$(80), P1%(80), B1%(100), B0%(100, 18), P0%(80, 17), T(14)
Dim T1(15), I2 As Single, I3 As Single, I4 As Single

7 Dim BR$(100), PR$(80), PR1%(80), BR1%(100), BR0%(100, 18), PR0%(80, 17)
Dim TR(14), TR1(15), D$(3)
ReDim BRS%(15, 100, 17), PRS%(15, 80, 16)

20 Open "DEV.BAS" For Input As #1
For I = 0 To 3: Input #1, D$(I)
Next

21 Close #1

30 Spec$ = D$(1) + "*.CAR"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim careerFiles$(0 To Count%)
For X = 1 To Count%
    careerFiles$(X) = Space$(12)
Next
careerFiles$(0) = Spec$
Call ReadFile(careerFiles$())
'CALL SortStr(VARPTR(careerFiles$(1)), Count, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT CAREER FILE TO VIEW": Color 15
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
MAMenu careerFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then Run "CAREER.EXE"
targetFile$ = careerFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 Z$ = RTrim$(targetFile$)

60 Color 15: Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

70 Open D$(1) + Z$ For Input As #2

71 Input #2, NB, NP

73 For I = 0 To 99: Input #2, B$(I): Next

74 For I = 0 To 79: Input #2, P$(I): Next

75 For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #2, BRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

76 For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #2, PRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

77 Close #2

80 Cls
Locate 18, 10: Color 12: Print "P";: Color 7: Print "RINT OR ";: Color 12: Print "V";: Color 7: Print "IEW"
81 I$ = GetKeyPress$

83 If UCase$(I$) = "P" Then 1100

84 GoTo 2000

100 Cls

101 TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0

102 TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0

104 Color 11: Locate 1: Print Z$

105 Color 12: Locate 2: Print "BATTER     YEAR   G   AB   R   H 2B 3B HR RBI  BB  SO  SB CS  E  AVG  SLG   OB"

106 For I = 0 To 99

    107 If B$(I) = "" Or B$(I) = "XXX" Then 239

    110 Locate 3: Color 15: Print Left$(B$(I), 10);

111 VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16) _
+ BRS%(15, I, 16)

    114 For V = 1 To VV

        115 Color 7: Print Tab(11); BRS%(V, I, 17);

        116 Print Tab(17); Using "###"; BRS%(V, I, 14);

        117 Print Tab(22); Using "###"; BRS%(V, I, 0);

        118 Print Tab(26); Using "###"; BRS%(V, I, 2);

        119 Print Tab(30); Using "###"; BRS%(V, I, 1);

        120 Print Tab(34); Using "##"; BRS%(V, I, 4);

        121 Print Tab(37); Using "##"; BRS%(V, I, 5);

        122 Print Tab(40); Using "##"; BRS%(V, I, 6);

        123 Print Tab(43); Using "###"; BRS%(V, I, 3);

        124 Print Tab(47); Using "###"; BRS%(V, I, 7);

        125 Print Tab(51); Using "###"; BRS%(V, I, 8);

        126 Print Tab(55); Using "###"; BRS%(V, I, 9);

        127 Print Tab(59); Using "##"; BRS%(V, I, 10);

        128 Print Tab(62); Using "##"; BRS%(V, I, 11);

210 I2 = 0: I3 = 0: I4 = 0: IF BRS%(V, I, 0) > 0 THEN I2 = INT((BRS%(V, I, 1) / BRS%(V, I, 0) + .0005) * 1000): I3 = INT(((BRS%(V, I, 1) + BRS%(V, I, 4) + 2 * BRS%(V, I, 5) + 3 * BRS%(V, I, 6)) / BRS%(V, I, 0) + .0005) * 1000): I4 = INT(((BRS%(V, I _
, 1) + BRS%(V, I, 7)) / (BRS%(V, I, 7) + BRS%(V, I, 0)) + .0005) * 1000)

        211 If BRS%(V, I, 0) = 0 Or I2 = 0 Then Locate , 65: Print ".000";

        212 If I2 > 0 And I2 < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2;

        213 If I2 >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2;

        215 If BRS%(V, I, 0) = 0 Or I3 = O Then Locate , 70: Print ".000";

        216 If I3 > 0 And I3 < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3;

        217 If I3 >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3;

        219 If BRS%(V, I, 0) = 0 Or I4 = 0 Then Locate , 75: Print ".000"

        220 If I4 > 0 And I4 < 100 Then Locate , 75: Print ".0";: Print Using "##"; I4

        221 If I4 >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4

    228 Next V

    229 GoSub 600

    230 TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0

    231 Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    233 I$ = GetKeyPress$

    234 If I$ = Chr$(0) Then 236

    235 If I$ = Chr$(27) Then 2000

    236 For T = 3 To 20: Locate T

        237 Print "                                                                              "

    238 Next

239 Next I

300 Cls

304 Color 11: Locate 1: Print Z$

305 Color 12: Locate 2: Print "PITCHER    YEAR  W  L  S   IP     H  BB  SO  ERA  G GS CG RUN  ER SHO HR DP"

306 For I = 0 To 79

    307 If P$(I) = "" Or P$(I) = "XXX" Then 364

    310 Locate 3: Color 15: Print Left$(P$(I), 10);

311 VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15) _
+ PRS%(15, I, 15)

    314 For V = 1 To VV

        315 Color 7: Print Tab(11); PRS%(V, I, 16);

        317 Print Tab(17); Using "##"; PRS%(V, I, 6);

        318 Print Tab(20); Using "##"; PRS%(V, I, 7);

        319 Print Tab(23); Using "##"; PRS%(V, I, 12);

        320 Locate , 27: Print Using "###"; Int(PRS%(V, I, 0) / 3);

        321 If PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3);

        328 Print Tab(33); Using "###"; PRS%(V, I, 1);

        329 Print Tab(37); Using "###"; PRS%(V, I, 4);

        330 Print Tab(41); Using "###"; PRS%(V, I, 5);

        331 I2 = 0: If PRS%(V, I, 0) > 0 Then I2 = Int((((PRS%(V, I, 3) * 9) / (PRS%(V, I, 0) / 3)) + .0005) * 100)

        332 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

        333 If PRS%(V, I, 0) = 0 Or I2 = 0 Then Locate , 44: Print " 0.00";

        334 If PRS%(V, I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate , 44: Print Using "##"; I3;: Print ".";

        335 If PRS%(V, I, 0) > 0 And I2 >= 1000 Then Locate , 44: Print Using "##"; I3;: Print ".";

        336 If PRS%(V, I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";

        337 If PRS%(V, I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;

        338 If PRS%(V, I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

        339 Print Tab(50); Using "##"; PRS%(V, I, 8);

        340 Print Tab(53); Using "##"; PRS%(V, I, 9);

        341 Print Tab(56); Using "##"; PRS%(V, I, 10);

        348 Print Tab(59); Using "###"; PRS%(V, I, 2);

        349 Print Tab(63); Using "###"; PRS%(V, I, 3);

        350 Print Tab(68); Using "##"; PRS%(V, I, 11);

        351 Print Tab(71); Using "##"; PRS%(V, I, 14);

        352 Print Tab(73); Using "###"; PRS%(V, I, 13)

    353 Next V

    354 GoSub 700

    355 TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0

    356 Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    358 I$ = GetKeyPress$

    360 If I$ = Chr$(27) Then 2000

    359 If I$ = Chr$(0) Then 361

    361 For T = 3 To 20: Locate T

        362 Print String$(76, " ")

    363 Next

364 Next I

370 Cls: Locate 5, 20: Color 14: Print "VIEW AGAIN (Y/N)"
371 I$ = GetKeyPress$

372 If UCase$(I$) = "Y" Then 100 Else GoTo 373

373 GoTo 2000

380 Run "CAREER.EXE"

600 VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16) _
+ BRS%(15, I, 16)

601 For V = 0 To VV

    602 TT1 = TT1 + BRS%(V, I, 14)

    603 TT3 = TT3 + BRS%(V, I, 2)

    604 TT5 = TT5 + BRS%(V, I, 4)

    605 TT7 = TT7 + BRS%(V, I, 6)

    606 TT9 = TT9 + BRS%(V, I, 7)

    607 TT11 = TT11 + BRS%(V, I, 9)

    608 TT13 = TT13 + BRS%(V, I, 11)

    609 TT2 = TT2 + BRS%(V, I, 0)

    610 TT4 = TT4 + BRS%(V, I, 1)

    611 TT6 = TT6 + BRS%(V, I, 5)

    612 TT8 = TT8 + BRS%(V, I, 3)

    613 TT10 = TT10 + BRS%(V, I, 8)

    614 TT12 = TT12 + BRS%(V, I, 10)

640 Next V

670 Print: Color 3

671 Print Tab(16); Using "####"; TT1;

672 Print Tab(25); Using "####"; TT3;

673 Print Tab(33); Using "###"; TT5;

674 Print Tab(38); Using "####"; TT7;

675 Print Tab(46); Using "####"; TT9;

676 Print Tab(54); Using "####"; TT11;

677 Print Tab(61); Using "###"; TT13;

678 I3 = 0: I3 = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

679 If TT2 = 0 Or I3 = O Then Locate , 70: Print ".000"

680 If I3 > 0 And I3 < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3

681 If I3 >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3

682 Print "TOTALS";

683 Print Tab(14); Using "##"; VV;

684 Print Tab(21); Using "####"; TT2;

685 Print Tab(29); Using "####"; TT4;

686 Print Tab(36); Using "###"; TT6;

687 Print Tab(42); Using "####"; TT8;

688 Print Tab(50); Using "####"; TT10;

689 Print Tab(58); Using "###"; TT12;

690 I2 = 0: If TT2 > 0 Then I2 = Int((TT4 / TT2 + .0005) * 1000)

691 If TT2 = 0 Or I2 = 0 Then Locate , 65: Print ".000";

692 If I2 > 0 And I2 < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2;

693 If I2 >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2;

694 I4 = 0: If TT9 + TT2 > 0 Then I4 = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

695 If TT2 = 0 Or I4 = 0 Then Locate , 75: Print ".000"

696 If I4 > 0 And I4 < 100 Then Locate , 75: Print ".0";: Print Using "##"; I4

697 If I4 >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4

698 Return

700 VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15) _
+ PRS%(15, I, 15)

701 For V = 0 To VV

    702 TT13 = TT13 + PRS%(V, I, 6)

    703 TT15 = TT15 + PRS%(V, I, 12)

    704 TT17 = TT17 + PRS%(V, I, 1)

    705 TT19 = TT19 + PRS%(V, I, 5)

    706 TT21 = TT21 + PRS%(V, I, 8)

    707 TT23 = TT23 + PRS%(V, I, 10)

    708 TT25 = TT25 + PRS%(V, I, 3)

    709 TT27 = TT27 + PRS%(V, I, 14)

    710 TT14 = TT14 + PRS%(V, I, 7)

    711 TT16 = TT16 + PRS%(V, I, 0)

    715 TT18 = TT18 + PRS%(V, I, 4)

    716 TT20 = TT20

    727 TT22 = TT22 + PRS%(V, I, 9)

    728 TT24 = TT24 + PRS%(V, I, 2)

    729 TT26 = TT26 + PRS%(V, I, 11)

    730 TT28 = TT28 + PRS%(V, I, 13)

740 Next V

770 Print: Color 3

771 Print Tab(16); Using "###"; TT13;

772 Print Tab(22); Using "###"; TT15;

773 Print Tab(32); Using "####"; TT17;

774 Print Tab(40); Using "####"; TT19;

775 Print Tab(49); Using "###"; TT21;

776 Print Tab(55); Using "###"; TT23;

777 Print Tab(62); Using "####"; TT25;

778 Print Tab(69); Using "####"; TT27

782 Print "TOTALS";

783 Print Tab(13); Using "###"; VV;

784 Print Tab(19); Using "###"; TT14;

785 Print Tab(27); Using "###"; Int(TT16 / 3);

786 If TT16 - (Int(TT16 / 3) * 3) > 0 Then Print ".";: Print Using "#"; TT16 - (Int(TT16 / 3) * 3);

787 Print Tab(37); Using "###"; TT18;

788 I2 = 0: If TT16 > 0 Then I2 = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

789 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

790 If TT16 = 0 Or I2 = 0 Then Locate , 44: Print " 0.00";

791 If TT16 > 0 And I2 > 0 And I2 <= 999 Then Locate , 44: Print Using "##"; I3;: Print ".";

792 If TT16 > 0 And I2 >= 1000 Then Locate , 44: Print Using "##"; I3;: Print ".";

793 If TT16 > 0 And I4 = 0 And I2 > 0 Then Print "00";

794 If TT16 > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;

795 If TT16 > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

796 Print Tab(51); Using "####"; TT22;

797 Print Tab(59); Using "###"; TT24;

798 Print Tab(66); Using "####"; TT26;

799 Print Tab(73); Using "###"; TT28

800 Return

1100 PTN = 0

1102 LPrint Chr$(15)

'1103 WIDTH "LPT1:1", 130

1104 LPrint Z$

1105 LPrint "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

1106 For I = 0 To 99

    1107 If B$(I) = "" Or B$(I) = "XXX" Then 1235

1110 VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16 _
) + BRS%(15, I, 16)

    1111 If 55 - PTN < VV Then GoSub 3000 Else GoTo 1112

    1112 LPrint Left$(B$(I), 10);

    1113 For V = 1 To VV

        1114 PTN = PTN + 1

        1115 LPrint Tab(11); BRS%(V, I, 17);

        1116 LPrint Tab(20); Using "###"; BRS%(V, I, 14);

        1117 LPrint Tab(28); Using "###"; BRS%(V, I, 0);

        1118 LPrint Tab(35); Using "###"; BRS%(V, I, 2);

        1119 LPrint Tab(42); Using "###"; BRS%(V, I, 1);

        1120 LPrint Tab(49); Using "##"; BRS%(V, I, 4);

        1121 LPrint Tab(55); Using "##"; BRS%(V, I, 5);

        1122 LPrint Tab(61); Using "##"; BRS%(V, I, 6);

        1123 LPrint Tab(67); Using "###"; BRS%(V, I, 3);

        1124 LPrint Tab(74); Using "###"; BRS%(V, I, 7);

        1125 LPrint Tab(81); Using "###"; BRS%(V, I, 8);

        1126 LPrint Tab(88); Using "###"; BRS%(V, I, 9);

        1127 LPrint Tab(95); Using "##"; BRS%(V, I, 10);

        1128 LPrint Tab(101); Using "##"; BRS%(V, I, 11);

1210 I2 = 0: I3 = 0: I4 = 0: IF BRS%(V, I, 0) > 0 THEN I2 = INT((BRS%(V, I, 1) / BRS%(V, I, 0) + .0005) * 1000): I3 = INT(((BRS%(V, I, 1) + BRS%(V, I, 4) + 2 * BRS%(V, I, 5) + 3 * BRS%(V, I, 6)) / BRS%(V, I, 0) + .0005) * 1000): I4 = INT(((BRS%(V, I _
, 1) + BRS%(V, I, 7)) / (BRS%(V, I, 7) + BRS%(V, I, 0)) + .0005) * 1000)

        1211 If BRS%(V, I, 0) = 0 Or I2 = 0 Then LPrint Tab(107); ".000";

        1212 If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;

        1213 If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;

        1215 If BRS%(V, I, 0) = 0 Or I3 = O Then LPrint Tab(115); ".000";

        1216 If I3 > 0 And I3 < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3;

        1217 If I3 >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3;

        1219 If BRS%(V, I, 0) = 0 Or I4 = 0 Then LPrint Tab(123); ".000"

        1220 If I4 > 0 And I4 < 100 Then LPrint Tab(123); ".0";: LPrint Using "##"; I4

        1221 If I4 >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4

    1228 Next V

    1229 GoSub 1600

    1230 TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0

    1231 PTN = PTN + 2: LPrint

1235 Next I

1250 LPrint Chr$(12)

1251 PTN = 0

1304 LPrint Z$

1305 LPrint "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

1306 For I = 0 To 79

    1307 If P$(I) = "" Or P$(I) = "XXX" Then 1362
    VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15 _
) + PRS%(15, I, 15)

    1311 If 55 - PTN < VV Then GoSub 3100 Else GoTo 1312

    1312 LPrint Left$(P$(I), 10);

    1313 For V = 1 To VV

        1314 PTN = PTN + 1

        1315 LPrint Tab(11); PRS%(V, I, 16);

        1317 LPrint Tab(20); Using "##"; PRS%(V, I, 6);

        1318 LPrint Tab(26); Using "##"; PRS%(V, I, 7);

        1319 LPrint Tab(32); Using "##"; PRS%(V, I, 12);

        1320 LPrint Tab(39);: LPrint Using "###"; Int(PRS%(V, I, 0) / 3);

        1321 If PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3);

        1328 LPrint Tab(48); Using "###"; PRS%(V, I, 1);

        1329 LPrint Tab(55); Using "###"; PRS%(V, I, 4);

        1330 LPrint Tab(62); Using "###"; PRS%(V, I, 5);

        1331 I2 = 0: If PRS%(V, I, 0) > 0 Then I2 = Int((((PRS%(V, I, 3) * 9) / (PRS%(V, I, 0) / 3)) + .0005) * 100)

        1332 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

        1333 If PRS%(V, I, 0) = 0 Or I2 = 0 Then LPrint Tab(68); " 0.00";

        1334 If PRS%(V, I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

        1335 If PRS%(V, I, 0) > 0 And I2 >= 1000 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

        1336 If PRS%(V, I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";

        1337 If PRS%(V, I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;

        1338 If PRS%(V, I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

        1339 LPrint Tab(77); Using "##"; PRS%(V, I, 8);

        1340 LPrint Tab(83); Using "##"; PRS%(V, I, 9);

        1341 LPrint Tab(89); Using "##"; PRS%(V, I, 10);

        1348 LPrint Tab(95); Using "###"; PRS%(V, I, 2);

        1349 LPrint Tab(102); Using "###"; PRS%(V, I, 3);

        1350 LPrint Tab(110); Using "##"; PRS%(V, I, 11);

        1351 LPrint Tab(116); Using "##"; PRS%(V, I, 14);

        1352 LPrint Tab(121); Using "###"; PRS%(V, I, 13)

    1353 Next V

    1354 GoSub 1700

    1355 TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0

    1357 PTN = PTN + 2: LPrint

1362 Next I

1370 LPrint Chr$(18)

'1371 WIDTH "LPT1:1", 80

1375 GoTo 380

1600 VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16 _
) + BRS%(15, I, 16)

1601 For V = 0 To VV

    1602 TT1 = TT1 + BRS%(V, I, 14)

    1603 TT3 = TT3 + BRS%(V, I, 2)

    1604 TT5 = TT5 + BRS%(V, I, 4)

    1605 TT7 = TT7 + BRS%(V, I, 6)

    1606 TT9 = TT9 + BRS%(V, I, 7)

    1607 TT11 = TT11 + BRS%(V, I, 9)

    1608 TT13 = TT13 + BRS%(V, I, 11)

    1609 TT2 = TT2 + BRS%(V, I, 0)

    1610 TT4 = TT4 + BRS%(V, I, 1)

    1611 TT6 = TT6 + BRS%(V, I, 5)

    1612 TT8 = TT8 + BRS%(V, I, 3)

    1613 TT10 = TT10 + BRS%(V, I, 8)

    1614 TT12 = TT12 + BRS%(V, I, 10)

1640 Next V

1670 LPrint "   Totals";

1671 LPrint Tab(14); Using "##"; VV;

1672 LPrint Tab(19); Using "####"; TT1;

1673 LPrint Tab(27); Using "####"; TT2;

1674 LPrint Tab(34); Using "####"; TT3;

1675 LPrint Tab(41); Using "####"; TT4;

1676 LPrint Tab(48); Using "###"; TT5;

1677 LPrint Tab(54); Using "###"; TT6;

1678 LPrint Tab(59); Using "####"; TT7;

1679 LPrint Tab(66); Using "####"; TT8;

1680 LPrint Tab(73); Using "####"; TT9;

1681 LPrint Tab(80); Using "####"; TT10;

1682 LPrint Tab(87); Using "####"; TT11;

1683 LPrint Tab(94); Using "###"; TT12;

1684 LPrint Tab(100); Using "###"; TT13;

1685 I2 = 0: If TT2 > 0 Then I2 = Int((TT4 / TT2 + .0005) * 1000)

1686 If TT2 = 0 Or I2 = 0 Then LPrint Tab(107); ".000";

1687 If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;

1688 If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;

1689 I3 = 0: I3 = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

1690 If TT2 = 0 Or I3 = O Then LPrint Tab(115); ".000";

1691 If I3 > 0 And I3 < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3;

1692 If I3 >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3;

1694 I4 = 0: If TT9 + TT2 > 0 Then I4 = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

1695 If TT2 = 0 Or I4 = 0 Then LPrint Tab(123); ".000"

1696 If I4 > 0 And I4 < 100 Then LPrint Tab(123); ".0";: LPrint Using "##"; I4

1697 If I4 >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4

1698 Return

1700 VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15 _
) + PRS%(15, I, 15)

1701 For V = 0 To VV

    1702 TT13 = TT13 + PRS%(V, I, 6)

    1703 TT15 = TT15 + PRS%(V, I, 12)

    1704 TT17 = TT17 + PRS%(V, I, 1)

    1705 TT19 = TT19 + PRS%(V, I, 5)

    1706 TT21 = TT21 + PRS%(V, I, 8)

    1707 TT23 = TT23 + PRS%(V, I, 10)

    1708 TT25 = TT25 + PRS%(V, I, 3)

    1709 TT27 = TT27 + PRS%(V, I, 14)

    1710 TT14 = TT14 + PRS%(V, I, 7)

    1711 TT16 = TT16 + PRS%(V, I, 0)

    1715 TT18 = TT18 + PRS%(V, I, 4)

    1716 TT20 = TT20

    1727 TT22 = TT22 + PRS%(V, I, 9)

    1728 TT24 = TT24 + PRS%(V, I, 2)

    1729 TT26 = TT26 + PRS%(V, I, 11)

    1730 TT28 = TT28 + PRS%(V, I, 13)

1740 Next V

1770 LPrint "   Totals";

1771 LPrint Tab(13); Using "###"; VV;

1772 LPrint Tab(19); Using "###"; TT13;

1773 LPrint Tab(25); Using "###"; TT14;

1774 LPrint Tab(31); Using "###"; TT15;

1775 LPrint Tab(39); Using "###"; Int(TT16 / 3);

1776 If TT16 - (Int(TT16 / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; TT16 - (Int(TT16 / 3) * 3);

1777 LPrint Tab(47); Using "####"; TT17;

1778 LPrint Tab(55); Using "###"; TT18;

1779 LPrint Tab(61); Using "####"; TT19;

1780 I2 = 0: If TT16 > 0 Then I2 = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

1781 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

1782 If TT16 = 0 Or I2 = 0 Then LPrint Tab(68); " 0.00";

1783 If TT16 > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

1784 If TT16 > 0 And I2 >= 1000 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

1785 If TT16 > 0 And I4 = 0 And I2 > 0 Then LPrint "00";

1786 If TT16 > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;

1787 If TT16 > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

1788 LPrint Tab(76); Using "###"; TT21;

1789 LPrint Tab(81); Using "####"; TT22;

1790 LPrint Tab(88); Using "###"; TT23;

1791 LPrint Tab(95); Using "###"; TT24;

1792 LPrint Tab(101); Using "####"; TT25;

1793 LPrint Tab(108); Using "####"; TT26;

1794 LPrint Tab(114); Using "####"; TT27;

1795 LPrint Tab(121); Using "###"; TT28

1800 Return

2000 CLS : COLOR 7: PRINT "VIEW "; : COLOR 12: PRINT "B"; : COLOR 7: PRINT "ATTERS, "; : COLOR 12: PRINT "P"; : COLOR 7: PRINT "ITCHERS, "; : COLOR 12: PRINT "R"; : COLOR 7: PRINT "OSTER IN SEQUENCE, OR "; : COLOR 12: PRINT "E"; : COLOR 7: PRINT  _
"XIT TO MAIN MENU"
2001 I$ = GetKeyPress$

2002 If UCase$(I$) = "B" Then 2010

2003 If UCase$(I$) = "P" Then 2210

2004 If UCase$(I$) = "R" Then 100

2005 If UCase$(I$) = "E" Then 380

2010 Cls: Color 12: Print Z$

2011 For I = 0 To 19
Color 11: Locate I + 2: Print Using "##"; I;: Print ".";: Color 7: Print Left$(B$(I), 12): Next
For I = 20 To 39
Color 11: Locate I - 18, 21: Print Using "##"; I;: Print ".";: Color 7: Print Left$(B$(I), 12): Next
For I = 40 To 59
Color 11: Locate I - 38, 41: Print Using "##"; I;: Print ".";: Color 7: Print Left$(B$(I), 12): Next
For I = 60 To 79
Color 11: Locate I - 58, 61: Print Using "##"; I;: Print ".";: Color 7: Print Left$(B$(I), 12): Next

2031 Print

2032 Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9

2033 Cls

2034 Color 11: Locate 1: Print Z$

2035 Color 12: Locate 2: Print "BATTER     YEAR   G   AB   R   H 2B 3B HR RBI  BB  SO  SB CS  E  AVG  SLG   OB"

2040 Locate 3: Color 15: Print Left$(B$(B9), 13);

2041 VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) +  _
BRS%(14, B9, 16) + BRS%(15, B9, 16)

2044 For V = 1 To VV

    2045 Color 7: Print Tab(11); BRS%(V, B9, 17);

    2046 Print Tab(17); Using "###"; BRS%(V, B9, 14);

    2047 Print Tab(22); Using "###"; BRS%(V, B9, 0);

    2048 Print Tab(26); Using "###"; BRS%(V, B9, 2);

    2049 Print Tab(30); Using "###"; BRS%(V, B9, 1);

    2050 Print Tab(34); Using "##"; BRS%(V, B9, 4);

    2051 Print Tab(37); Using "##"; BRS%(V, B9, 5);

    2052 Print Tab(40); Using "##"; BRS%(V, B9, 6);

    2053 Print Tab(43); Using "###"; BRS%(V, B9, 3);

    2054 Print Tab(47); Using "###"; BRS%(V, B9, 7);

    2055 Print Tab(51); Using "###"; BRS%(V, B9, 8);

    2056 Print Tab(55); Using "###"; BRS%(V, B9, 9);

    2057 Print Tab(59); Using "##"; BRS%(V, B9, 10);

    2058 Print Tab(62); Using "##"; BRS%(V, B9, 11);

    2060 I2 = 0: I3 = 0: I4 = 0
    IF BRS%(V, B9, 0) > 0 THEN I2 = INT((BRS%(V, B9, 1) / BRS%(V, B9, 0) + .0005) * 1000): I3 = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 4) + 2 * BRS%(V, B9, 5) + 3 * BRS%(V, B9, 6)) / BRS%(V, B9, 0) + .0005) * 1000): I4 = INT(((BRS%(V, B9, 1) + BRS%(V,  _
B9, 7)) / (BRS%(V, B9, 7) + BRS%(V, B9, 0)) + .0005) * 1000)

    2061 If BRS%(V, B9, 0) = 0 Or I2 = 0 Then Locate , 65: Print ".000";

    2062 If I2 > 0 And I2 < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2;

    2063 If I2 >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2;

    2065 If BRS%(V, B9, 0) = 0 Or I3 = O Then Locate , 70: Print ".000";

    2066 If I3 > 0 And I3 < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3;

    2067 If I3 >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3;

    2069 If BRS%(V, B9, 0) = 0 Or I4 = 0 Then Locate , 75: Print ".000"

    2070 If I4 > 0 And I4 < 100 Then Locate , 75: Print ".";: Print Using "###"; I4

    2071 If I4 >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4

2078 Next V

2079 VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) +  _
BRS%(14, B9, 16) + BRS%(15, B9, 16)

2080 TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0

2081 For V = 0 To VV

    2082 TT1 = TT1 + BRS%(V, B9, 14)

    2083 TT3 = TT3 + BRS%(V, B9, 2)

    2084 TT5 = TT5 + BRS%(V, B9, 4)

    2085 TT7 = TT7 + BRS%(V, B9, 6)

    2086 TT9 = TT9 + BRS%(V, B9, 7)

    2087 TT11 = TT11 + BRS%(V, B9, 9)

    2088 TT13 = TT13 + BRS%(V, B9, 11)

    2089 TT2 = TT2 + BRS%(V, B9, 0)

    2100 TT4 = TT4 + BRS%(V, B9, 1)

    2111 TT6 = TT6 + BRS%(V, B9, 5)

    2112 TT8 = TT8 + BRS%(V, B9, 3)

    2113 TT10 = TT10 + BRS%(V, B9, 8)

    2114 TT12 = TT12 + BRS%(V, B9, 10)

2140 Next V

2150 Print: Color 3

2151 Print Tab(16); Using "####"; TT1;

2152 Print Tab(25); Using "####"; TT3;

2153 Print Tab(33); Using "###"; TT5;

2154 Print Tab(38); Using "####"; TT7;

2155 Print Tab(46); Using "####"; TT9;

2156 Print Tab(54); Using "####"; TT11;

2157 Print Tab(61); Using "###"; TT13;

2158 I3 = 0: I3 = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

2159 If TT2 = 0 Or I3 = O Then Locate , 70: Print ".000"

2160 If I3 > 0 And I3 < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3

2161 If I3 >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3

2162 Print "TOTALS";

2163 Print Tab(13); Using "##"; VV;

2164 Print Tab(21); Using "####"; TT2;

2165 Print Tab(29); Using "####"; TT4;

2166 Print Tab(36); Using "###"; TT6;

2167 Print Tab(42); Using "####"; TT8;

2168 Print Tab(50); Using "####"; TT10;

2169 Print Tab(58); Using "###"; TT12;

2170 I2 = 0: If TT2 > 0 Then I2 = Int((TT4 / TT2 + .0005) * 1000)

2171 If TT2 = 0 Or I2 = 0 Then Locate , 65: Print ".000";

2172 If I2 > 0 And I2 < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2;

2173 If I2 >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2;

2174 I4 = 0: If TT9 + TT2 > 0 Then I4 = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

2175 If TT2 = 0 Or I4 = 0 Then Locate , 75: Print ".000"

2176 If I4 > 0 And I4 < 100 Then Locate , 75: Print ".";: Print Using "###"; I4

2177 If I4 >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4

2190 Print: Print: Color 15: Input "HIT ENTER TO CONTINUE"; DUMMY$

2191 GoTo 2000

2210 Cls: Color 12: Print Z$

2211 For I = 0 To 19
Color 11: Locate I + 2: Print Using "##"; I;: Print ".";: Color 7: Print Left$(P$(I), 12): Next
For I = 20 To 39
Color 11: Locate I - 18, 21: Print Using "##"; I;: Print ".";: Color 7: Print Left$(P$(I), 12): Next
For I = 40 To 59
Color 11: Locate I - 38, 41: Print Using "##"; I;: Print ".";: Color 7: Print Left$(P$(I), 12): Next
For I = 60 To 79
Color 11: Locate I - 58, 61: Print Using "##"; I;: Print ".";: Color 7: Print Left$(P$(I), 12): Next

2231 Print

2232 Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9

2233 Cls

2304 Color 11: Locate 1: Print Z$

2305 Color 12: Locate 2: Print "PITCHER    YEAR  W  L  S   IP     H  BB  SO  ERA  G GS CG RUN  ER SHO HR DP"

2310 Locate 3: Color 15: Print Left$(P$(B9), 13);

2311 VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) +  _
PRS%(14, B9, 15) + PRS%(15, B9, 15)

2314 For V = 1 To VV

    2315 Color 7: Print Tab(11); PRS%(V, B9, 16);

    2317 Print Tab(17); Using "##"; PRS%(V, B9, 6);

    2318 Print Tab(20); Using "##"; PRS%(V, B9, 7);

    2319 Print Tab(23); Using "##"; PRS%(V, B9, 12);

    2320 Locate , 27: Print Using "###"; Int(PRS%(V, B9, 0) / 3);

    2321 If PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3);

    2328 Print Tab(33); Using "###"; PRS%(V, B9, 1);

    2329 Print Tab(37); Using "###"; PRS%(V, B9, 4);

    2330 Print Tab(41); Using "###"; PRS%(V, B9, 5);

    2331 I2 = 0: If PRS%(V, B9, 0) > 0 Then I2 = Int((((PRS%(V, B9, 3) * 9) / (PRS%(V, B9, 0) / 3)) + .0005) * 100)

    2332 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

    2333 If PRS%(V, B9, 0) = 0 Or I2 = 0 Then Locate , 44: Print " 0.00";

    2334 If PRS%(V, B9, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate , 44: Print Using "##"; I3;: Print ".";

    2335 If PRS%(V, B9, 0) > 0 And I2 >= 1000 Then Locate , 44: Print Using "##"; I3;: Print ".";

    2336 If PRS%(V, B9, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";

    2337 If PRS%(V, B9, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;

    2338 If PRS%(V, B9, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

    2339 Print Tab(50); Using "##"; PRS%(V, B9, 8);

    2340 Print Tab(53); Using "##"; PRS%(V, B9, 9);

    2341 Print Tab(56); Using "##"; PRS%(V, B9, 10);

    2348 Print Tab(59); Using "###"; PRS%(V, B9, 2);

    2349 Print Tab(63); Using "###"; PRS%(V, B9, 3);

    2350 Print Tab(68); Using "##"; PRS%(V, B9, 11);

    2351 Print Tab(71); Using "##"; PRS%(V, B9, 14);

    2352 Print Tab(73); Using "###"; PRS%(V, B9, 13)

2353 Next V

2355 Print: Color 3

2700 VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) +  _
PRS%(14, B9, 15) + PRS%(15, B9, 15)

2701 TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0

2702 For V = 0 To VV

    2703 TT13 = TT13 + PRS%(V, B9, 6)

    2704 TT15 = TT15 + PRS%(V, B9, 12)

    2705 TT17 = TT17 + PRS%(V, B9, 1)

    2706 TT19 = TT19 + PRS%(V, B9, 5)

    2707 TT21 = TT21 + PRS%(V, B9, 8)

    2708 TT23 = TT23 + PRS%(V, B9, 10)

    2709 TT25 = TT25 + PRS%(V, B9, 3)

    2710 TT27 = TT27 + PRS%(V, B9, 14)

    2711 TT14 = TT14 + PRS%(V, B9, 7)

    2712 TT16 = TT16 + PRS%(V, B9, 0)

    2715 TT18 = TT18 + PRS%(V, B9, 4)

    2716 TT20 = TT20

    2727 TT22 = TT22 + PRS%(V, B9, 9)

    2728 TT24 = TT24 + PRS%(V, B9, 2)

    2729 TT26 = TT26 + PRS%(V, B9, 11)

    2730 TT28 = TT28 + PRS%(V, B9, 13)

2740 Next V

2770 Print: Color 3

2771 Print Tab(16); Using "###"; TT13;

2772 Print Tab(22); Using "###"; TT15;

2773 Print Tab(32); Using "####"; TT17;

2774 Print Tab(40); Using "####"; TT19;

2775 Print Tab(49); Using "###"; TT21;

2776 Print Tab(55); Using "###"; TT23;

2777 Print Tab(62); Using "####"; TT25;

2778 Print Tab(69); Using "####"; TT27

2782 Print "TOTALS";

2783 Print Tab(13); Using "###"; VV;

2784 Print Tab(19); Using "###"; TT14;

2785 Print Tab(27); Using "###"; Int(TT16 / 3);

2786 If TT16 - (Int(TT16 / 3) * 3) > 0 Then Print ".";: Print Using "#"; TT16 - (Int(TT16 / 3) * 3);

2787 Print Tab(37); Using "###"; TT18;

2788 I2 = 0: If TT16 > 0 Then I2 = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

2789 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

2790 If TT16 = 0 Or I2 = 0 Then Locate , 44: Print " 0.00";

2791 If TT16 > 0 And I2 > 0 And I2 <= 999 Then Locate , 44: Print Using "##"; I3;: Print ".";

2792 If TT16 > 0 And I2 >= 1000 Then Locate , 44: Print Using "##"; I3;: Print ".";

2793 If TT16 > 0 And I4 = 0 And I2 > 0 Then Print "00";

2794 If TT16 > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;

2795 If TT16 > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

2796 Print Tab(51); Using "####"; TT22;

2797 Print Tab(59); Using "###"; TT24;

2798 Print Tab(66); Using "####"; TT26;

2799 Print Tab(73); Using "###"; TT28

2800 Print: Print: Color 15: Input "HIT ENTER TO CONTINUE"; DUMMY$

2801 GoTo 2000

3000 LPrint Chr$(12)

3001 LPrint Z$

3002 LPrint "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

3003 PTN = 0

3004 Return

3100 LPrint Chr$(12)

3101 LPrint Z$

3102 LPrint "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

3103 PTN = 0

3104 Return

4500 Locate 10, 8
Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 380


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
