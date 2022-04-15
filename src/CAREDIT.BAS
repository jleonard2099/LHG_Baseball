'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Edit Career Files"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim B$(100), P$(80), P1%(80), B1%(100), B0%(100, 18), P0%(80, 17), T(14)
Dim T1(15), D$(3)
Dim BR$(100), PR$(80), PR1%(80), BR1%(100), BR0%(100, 18), PR0%(80, 17)
Dim TR(14), TR1(15), TT(100), I2 As Single, I3 As Single, I4 As Single
ReDim BRS%(15, 100, 17), PRS%(15, 80, 16)

Cls: TMR = 2700

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, D$(I)
Next
Close #1

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
Print "SELECT CAREER FILE TO EDIT": Color 15
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

If ExitCode% = 27 Then 5017
targetFile$ = careerFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3913 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 Z$ = RTrim$(targetFile$)

50 GoSub 7750

60 Color 15: Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

70 Open D$(1) + Z$ For Input As #2

71 Input #2, NB, NP

73 For I = 0 To 99: Input #2, B$(I): Next

74 For I = 0 To 79: Input #2, P$(I): Next

75 For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #2, BRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

76 For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #2, PRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

77 Close #2

2000 Cls
Color 7: Print "EDIT ";
Color 12: Print "B";
Color 7: Print "ATTERS, ";
Color 12: Print "P";
Color 7: Print "ITCHERS, ";
Color 12: Print "U";: Color 7
Print "PDATE FILE, OR ";: Color 12
Print "E";: Color 7: Print "XIT WITHOUT UPDATING"
2001 I$ = GetKeyPress$

2002 If UCase$(I$) = "B" Then 2010

2003 If UCase$(I$) = "P" Then 2210

2004 If UCase$(I$) = "E" Then 5017

2005 If UCase$(I$) = "U" Then 5000

2006 GoTo 2001

2010 Cls: Color 12: Print Z$

2011 For I = 0 To 19
Color 11: Locate I + 2, 1: Print Using "##"; I;: Print " ";: Color 7: Print Left$(B$(I), 12): Next
For I = 20 To 39
Color 11: Locate I - 18, 16: Print Using "##"; I;: Print " ";: Color 7: Print Left$(B$(I), 12): Next
For I = 40 To 59
Color 11: Locate I - 38, 32: Print Using "##"; I;: Print " ";: Color 7: Print Left$(B$(I), 12): Next
For I = 60 To 79
Color 11: Locate I - 58, 48: Print Using "##"; I;: Print " ";: Color 7: Print Left$(B$(I), 12): Next
For I = 80 To 99
Color 11: Locate I - 78, 64: Print Using "##"; I;: Print " ";: Color 7: Print Left$(B$(I), 12): Next

2031 Print

2032 Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

2100 If B9 = 100 Then 2000

2101 If B9 = 99 Then 2107

2102 GoSub 6000: Cls: Locate 5, 15: Print "CHANGING BATTER'S FILES . . . . . PLEASE WAIT"

2104 For I = B9 To 98: B$(I) = B$(I + 1): Next

2105 UPP = (99 - B9) * 15

2106 I = B9: For J = 1 To 15: For I = B9 To 98: For I1 = O To 17: BRS%(J, I, I1) = BRS%(J, I + 1, I1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2107 B$(99) = "": For J = 1 To 15: For I1 = 0 To 17: BRS%(J, 99, I1) = 0: Next: Next: GoTo 2191

2108 GoSub 6000: B$(99) = "": For J = 1 To 15: For I1 = 0 To 17: BRS%(J, 99, I1) = 0: Next: Next

2191 GoTo 2000

2210 Cls: Color 12: Print Z$
For I = 0 To 19
Color 11: Locate I + 2, 1: Print Using "##"; I;: Print " ";: Color 7: Print Left$(P$(I), 15): Next
For I = 20 To 39
Color 11: Locate I - 18, 21: Print Using "##"; I;: Print " ";: Color 7: Print Left$(P$(I), 15): Next
For I = 40 To 59
Color 11: Locate I - 38, 41: Print Using "##"; I;: Print " ";: Color 7: Print Left$(P$(I), 15): Next
For I = 60 To 79
Color 11: Locate I - 58, 61: Print Using "##"; I;: Print " ";: Color 7: Print Left$(P$(I), 15): Next

2231 Print

2232 Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

2300 If B9 = 100 Then 2000

2301 If B9 = 79 Then 2307

2302 GoSub 7000: Cls: Locate 5, 15: Print "CHANGING PITCHER'S FILES . . . . . PLEASE WAIT"

2304 For I = B9 To 78: P$(I) = P$(I + 1): Next

2305 UPP = (79 - B9) * 15

2306 I = B9: For J = 1 To 15: For I = B9 To 78: For I1 = O To 16: PRS%(J, I, I1) = PRS%(J, I + 1, I1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2307 P$(79) = "": For J = 1 To 15: For I1 = 0 To 16: PRS%(J, 79, I1) = 0: Next: Next: GoTo 2391

2308 GoSub 7000: P$(79) = "": For J = 1 To 15: For I1 = 0 To 16: PRS%(J, 79, I1) = 0: Next: Next
''
2391 GoTo 2000

5000 GoSub 7750

5001 Cls: Color 15: Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

5002 TMR = 2700

5010 Open D$(1) + Z$ For Output As #3

5011 Write #3, NB, NP

5012 For I = 0 To 99: Write #3, B$(I): Next

5013 For I = 0 To 79: Write #3, P$(I): Next

5014 For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Write #3, BRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

5015 For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Write #3, PRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

5016 Close #3

5017 Run "CAREER.EXE"

6000 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print B$(B9);: Color 15: Print " BEFORE "

6001 Locate , 20: Print "DELETING THEM? (Y/N)"

6002 Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"

6003 Locate , 20: Print "OF ";: Color 12: Print B$(B9);: Color 14: Print " HIT 'E'"
6004 I$ = GetKeyPress$

6005 If UCase$(I$) = "Y" Then 6100

6006 If UCase$(I$) = "N" Then 6300

6007 If UCase$(I$) = "E" Then 2000

6008 If UCase$(I$) <> "Y" Or UCase$(I$) <> "N" Or UCase$(I$) <> "E" Then 6004

6100 LPrint Chr$(15)

'6103 WIDTH "LPT1:1", 130

6104 LPrint Z$

6105 LPrint "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

6110 VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) +  _
BRS%(14, B9, 16) + BRS%(15, B9, 16)

6112 LPrint Left$(B$(B9), 10);

6113 For V = 1 To VV

    6115 LPrint Tab(11); BRS%(V, B9, 17);

    6116 LPrint Tab(20); Using "###"; BRS%(V, B9, 14);

    6117 LPrint Tab(28); Using "###"; BRS%(V, B9, 0);

    6118 LPrint Tab(35); Using "###"; BRS%(V, B9, 2);

    6119 LPrint Tab(42); Using "###"; BRS%(V, B9, 1);

    6120 LPrint Tab(49); Using "##"; BRS%(V, B9, 4);

    6121 LPrint Tab(55); Using "##"; BRS%(V, B9, 5);

    6122 LPrint Tab(61); Using "##"; BRS%(V, B9, 6);

    6123 LPrint Tab(67); Using "###"; BRS%(V, B9, 3);

    6124 LPrint Tab(74); Using "###"; BRS%(V, B9, 7);

    6125 LPrint Tab(81); Using "###"; BRS%(V, B9, 8);

    6126 LPrint Tab(88); Using "###"; BRS%(V, B9, 9);

    6127 LPrint Tab(95); Using "##"; BRS%(V, B9, 10);

    6128 LPrint Tab(101); Using "##"; BRS%(V, B9, 11);

    6210 I2 = 0: I3 = 0: I4 = 0
IF BRS%(V, B9, 0) > 0 THEN I2 = INT((BRS%(V, B9, 1) / BRS%(V, B9, 0) + .0005) * 1000): I3 = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 4) + 2 * BRS%(V, B9, 5) + 3 * BRS%(V, B9, 6)) / BRS%(V, B9, 0) + .0005) * 1000): I4 = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 7 _
)) / (BRS%(V, B9, 7) + BRS%(V, B9, 0)) + .0005) * 1000)

    6211 If BRS%(V, B9, 0) = 0 Or I2 = 0 Then LPrint Tab(107); ".000";

    6212 If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;

    6213 If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;

    6215 If BRS%(V, B9, 0) = 0 Or I3 = 0 Then LPrint Tab(115); ".000";

    6216 If I3 > 0 And I3 < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3;

    6217 If I3 >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3;

    6219 If BRS%(V, B9, 0) = 0 Or I4 = 0 Then LPrint Tab(123); ".000"

    6220 If I4 > 0 And I4 < 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4

    6221 If I4 >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4

6228 Next V

6231 For V = 0 To VV

    6232 TT1 = TT1 + BRS%(V, B9, 14)

    6233 TT3 = TT3 + BRS%(V, B9, 2)

    6234 TT5 = TT5 + BRS%(V, B9, 4)

    6235 TT7 = TT7 + BRS%(V, B9, 6)

    6236 TT9 = TT9 + BRS%(V, B9, 7)

    6237 TT11 = TT11 + BRS%(V, B9, 9)

    6238 TT13 = TT13 + BRS%(V, B9, 11)

    6239 TT2 = TT2 + BRS%(V, B9, 0)

    6240 TT4 = TT4 + BRS%(V, B9, 1)

    6241 TT6 = TT6 + BRS%(V, B9, 5)

    6242 TT8 = TT8 + BRS%(V, B9, 3)

    6243 TT10 = TT10 + BRS%(V, B9, 8)

    6244 TT12 = TT12 + BRS%(V, B9, 10)

6250 Next V

6260 LPrint "   Totals";

6261 LPrint Tab(14); Using "##"; VV;

6262 LPrint Tab(19); Using "####"; TT1;

6263 LPrint Tab(27); Using "####"; TT2;

6264 LPrint Tab(34); Using "####"; TT3;

6265 LPrint Tab(41); Using "####"; TT4;

6266 LPrint Tab(48); Using "###"; TT5;

6267 LPrint Tab(54); Using "###"; TT6;

6268 LPrint Tab(59); Using "####"; TT7;

6269 LPrint Tab(66); Using "####"; TT8;

6270 LPrint Tab(73); Using "####"; TT9;

6271 LPrint Tab(80); Using "####"; TT10;

6272 LPrint Tab(87); Using "####"; TT11;

6273 LPrint Tab(94); Using "###"; TT12;

6274 LPrint Tab(100); Using "###"; TT13;

6275 I2 = 0: If TT2 > 0 Then I2 = Int((TT4 / TT2 + .0005) * 1000)

6276 If TT2 = 0 Or I2 = 0 Then LPrint Tab(107); ".000";

6277 If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;

6278 If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;

6279 I3 = 0: I3 = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

6280 If TT2 = 0 Or I3 = 0 Then LPrint Tab(115); ".000";

6281 If I3 > 0 And I3 < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3;

6282 If I3 >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3;

6284 I4 = 0: If TT9 + TT2 > 0 Then I4 = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

6285 If TT2 = 0 Or I4 = 0 Then LPrint Tab(123); ".000"

6286 If I4 > 0 And I4 < 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4

6287 If I4 >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4

6290 TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0

6291 LPrint

6292 LPrint Chr$(18)

'6293 WIDTH "LPT1:1", 80

6300 Return

7000 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print P$(B9);: Color 15: Print " BEFORE "

7001 Locate , 20: Print "DELETING THEM? (Y/N)"

7002 Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"

7003 Locate , 20: Print "OF ";: Color 12: Print P$(B9);: Color 14: Print " HIT 'E'"
7004 I$ = GetKeyPress$

7005 If UCase$(I$) = "Y" Then 7300

7006 If UCase$(I$) = "N" Then 7600

7007 If UCase$(I$) = "E" Then 2000

7008 If UCase$(I$) <> "Y" Or UCase$(I$) <> "N" Or UCase$(I$) <> "E" Then 7004

7300 LPrint Chr$(15)

'7303 WIDTH "LPT1:1", 130

7304 LPrint Z$

7305 LPrint "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

7310 VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) +  _
PRS%(14, B9, 15) + PRS%(15, B9, 15)

7312 LPrint Left$(P$(B9), 10);

7313 For V = 1 To VV

    7315 LPrint Tab(11); PRS%(V, B9, 16);

    7317 LPrint Tab(20); Using "##"; PRS%(V, B9, 6);

    7318 LPrint Tab(26); Using "##"; PRS%(V, B9, 7);

    7319 LPrint Tab(32); Using "##"; PRS%(V, B9, 12);

    7320 LPrint Tab(39);: LPrint Using "###"; Int(PRS%(V, B9, 0) / 3);

    7321 If PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3);

    7328 LPrint Tab(48); Using "###"; PRS%(V, B9, 1);

    7329 LPrint Tab(55); Using "###"; PRS%(V, B9, 4);

    7330 LPrint Tab(62); Using "###"; PRS%(V, B9, 5);

    7331 I2 = 0: If PRS%(V, B9, 0) > 0 Then I2 = Int((((PRS%(V, B9, 3) * 9) / (PRS%(V, B9, 0) / 3)) + .0005) * 100)

    7332 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

    7333 If PRS%(V, B9, 0) = 0 Or I2 = 0 Then LPrint Tab(68); " 0.00";

    7334 If PRS%(V, B9, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

    7335 If PRS%(V, B9, 0) > 0 And I2 >= 1000 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

    7336 If PRS%(V, B9, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";

    7337 If PRS%(V, B9, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;

    7338 If PRS%(V, B9, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

    7339 LPrint Tab(77); Using "##"; PRS%(V, B9, 8);

    7340 LPrint Tab(83); Using "##"; PRS%(V, B9, 9);

    7341 LPrint Tab(89); Using "##"; PRS%(V, B9, 10);

    7348 LPrint Tab(95); Using "###"; PRS%(V, B9, 2);

    7349 LPrint Tab(102); Using "###"; PRS%(V, B9, 3);

    7350 LPrint Tab(110); Using "##"; PRS%(V, B9, 11);

    7351 LPrint Tab(116); Using "##"; PRS%(V, B9, 14);

    7352 LPrint Tab(121); Using "###"; PRS%(V, B9, 13)

7353 Next V

7355 TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0

7400 VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) +  _
PRS%(14, B9, 15) + PRS%(15, B9, 15)

7401 For V = 0 To VV

    7402 TT13 = TT13 + PRS%(V, B9, 6)

    7403 TT15 = TT15 + PRS%(V, B9, 12)

    7404 TT17 = TT17 + PRS%(V, B9, 1)

    7405 TT19 = TT19 + PRS%(V, B9, 5)

    7406 TT21 = TT21 + PRS%(V, B9, 8)

    7407 TT23 = TT23 + PRS%(V, B9, 10)

    7408 TT25 = TT25 + PRS%(V, B9, 3)

    7409 TT27 = TT27 + PRS%(V, B9, 14)

    7410 TT14 = TT14 + PRS%(V, B9, 7)

    7411 TT16 = TT16 + PRS%(V, B9, 0)

    7415 TT18 = TT18 + PRS%(V, B9, 4)

    7416 TT20 = TT20

    7427 TT22 = TT22 + PRS%(V, B9, 9)

    7428 TT24 = TT24 + PRS%(V, B9, 2)

    7429 TT26 = TT26 + PRS%(V, B9, 11)

    7430 TT28 = TT28 + PRS%(V, B9, 13)

7440 Next V

7470 LPrint "   Totals";

7471 LPrint Tab(13); Using "###"; VV;

7472 LPrint Tab(19); Using "###"; TT13;

7473 LPrint Tab(25); Using "###"; TT14;

7474 LPrint Tab(31); Using "###"; TT15;

7475 LPrint Tab(39); Using "###"; Int(TT16 / 3);

7476 If TT16 - (Int(TT16 / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; TT16 - (Int(TT16 / 3) * 3);

7477 LPrint Tab(47); Using "####"; TT17;

7478 LPrint Tab(55); Using "###"; TT18;

7479 LPrint Tab(61); Using "####"; TT19;

7480 I2 = 0: If TT16 > 0 Then I2 = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

7481 I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)

7482 If TT16 = 0 Or I2 = 0 Then LPrint Tab(68); " 0.00";

7483 If TT16 > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

7484 If TT16 > 0 And I2 >= 1000 Then LPrint Tab(68); Using "##"; I3;: LPrint ".";

7485 If TT16 > 0 And I4 = 0 And I2 > 0 Then LPrint "00";

7486 If TT16 > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;

7487 If TT16 > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

7488 LPrint Tab(76); Using "###"; TT21;

7489 LPrint Tab(81); Using "####"; TT22;

7490 LPrint Tab(88); Using "###"; TT23;

7491 LPrint Tab(95); Using "###"; TT24;

7492 LPrint Tab(101); Using "####"; TT25;

7493 LPrint Tab(108); Using "####"; TT26;

7494 LPrint Tab(114); Using "####"; TT27;

7495 LPrint Tab(121); Using "###"; TT28

7496 LPrint

7497 LPrint Chr$(18)

'7498 WIDTH "LPT1:1", 80

7600 Return

7750 Cls

7753 Return

4500 Locate 10, 8
Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 5017


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
