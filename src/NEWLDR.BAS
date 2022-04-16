'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Expanded Standings"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim B$(23), B%(23), P$(22), P%(22), B0%(23, 22), P0%(22, 42), T(22)
Dim T1(23), TT(40, 22) As Single, TT1(40, 23) As Single, TM$(40, 22), TT2$(40, 23)

2 Dim WW(40), LL(40), menuItems$(6)
Dim Z1$(40), TYP2(920), D$(2)

3 Dim TMB(920) As String * 15, TP$(32)
Dim TP1$(30), TP2$(40)
ReDim TMM(920) As String * 15
ReDim TYP1(920) As Single
ReDim GM1(920)
ReDim BL(920) As String * 15, PL(880) As String * 15
ReDim GM2(880)
ReDim BK%(920, 22)
ReDim PM%(880, 23)
ReDim TMP(880) As String * 15
ReDim TPP(920) As String * 15
ReDim TYP(920) As Single
Cls
Open "DEV.BAS" For Input As #2: For I = 0 To 2: Input #2, D$(I): Next: Close 2

4 K = 0: K1 = 0: PK = 0: PK1 = 0: JB = 0: JP = 0: TBN = 0: TPN = 0: YY = 0: TMR = 3680: TS = 0: PV = 0

6 Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,CURRENT HITTING STREAK,LONGEST HITTING STREAK,GAMES,GROUND INTO DP,PUTOUTS,ASSISTS,SACRIFICE HITS,SACRIFICE FLIES
Data FIELDING AVERAGE,BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,AT BATS PER HOME RUN,AT BATS PER STRIKEOUT,AT BATS PER WALK,AT BATS PER RBI,AT BATS PER RUNS SCORED,AT BATS PER DOUBLE PLAY,RUNNERS CAUGHT STEALING,GO TO MAIN MENU
For I = 0 To 31: Read TP$(I): Next

7 Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,WON,LOST,GAMES,GAMES STARTED,COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,LEADS HELD,RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP,SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT,RETURN TO MAIN MENU
For I = 0 To 29: Read TP1$(I): Next
Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO,FIELDING AVERAGE
Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,SACRIFICE HITS,SACRIFICE FLIES,RETURN TO MAIN MENU
For I = 0 To 39: Read TP2$(I): Next

8 GoTo 2300

9 Cls
For KK = 1 To 40: YY = YY + 1
    If Z1$(KK) = "*" Then 60
    If Not _FileExists(D$(1) + Z1$(KK)) Then 60

    10 Print "CONVERTING STAT FILE OF "; Z1$(KK)
    Open D$(1) + Z1$(KK) For Input As #1

    11 Input #1, A$: Input #1, W2%: Input #1, L2%

    12 For I = 0 To 22: Input #1, B$(I): Input #1, B%(I): Next I

    13 For I = 0 To 21: Input #1, P$(I): Input #1, P%(I): Next I

    14 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I

    15 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

    16 Input #1, D0%: Input #1, D1%
    For I = 0 To 21: Input #1, T(I): Next I
    For I = 0 To 22: Input #1, T1(I): Next I

    17 Close #1

    18 WW(YY) = W2%: LL(YY) = L2%

    19 TS = TS + 1
    For I = 0 To 21
        TT(TS, I) = T(I)
        TM$(TS, I) = A$
    Next

    20 TTS = TTS + 1
    For I = 0 To 22
        TT1(TTS, I) = T1(I)
        TT2$(TTS, I) = A$
    Next

    21 For I = 0 To 22

        22 If B$(I) = "XXX" Then 25 Else GoTo 23

        23 JB = JB + 1: BL$(JB) = B$(I): TBN = TBN + 1: TMB$(TBN) = A$: GM1(JB) = WW(YY) + LL(YY)

    25 Next I

    30 For I = 0 To 21

        31 If P$(I) = "XXX" Then 35 Else GoTo 33

        33 JP = JP + 1: PL$(JP) = P$(I): TPN = TPN + 1: TMP$(TPN) = A$: GM2(JP) = WW(YY) + LL(YY)

    35 Next I

    40 For I = 0 To 22

        41 If B$(I) = "XXX" Then 45 Else GoTo 42

        42 K = K + 1: K1 = 0: For I1 = 0 To 21: K1 = K1 + 1: BK%(K, K1) = B0%(I, I1)

        43 Next I1

    45 Next I

    50 For I = 0 To 21

        51 If P$(I) = "XXX" Then 55 Else GoTo 52

        52 PK = PK + 1: PK1 = 0: For I1 = 0 To 22: PK1 = PK1 + 1: PM%(PK, PK1) = P0%(I, I1)

        53 Next I1

    55 Next I

60 Next KK

61 Print "SAVING LEADER FILES . . . . PLEASE WAIT"

62 Open D$(2) + LG$ + ".BAT" For Output As #1
Write #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
For I = 1 To 920: Write #1, BL$(I), TMB$(I), GM1(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
For I = 1 To 920: For I1 = 1 To 22: Write #1, BK%(I, I1): Next I1: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
Close 1

64 Open D$(2) + LG$ + ".PIT" For Output As #1
Write #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
For I = 1 To 880: Write #1, PL$(I), TMP$(I), GM2(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
For I = 1 To 880: For I1 = 1 To 23: Write #1, PM%(I, I1): Next I1: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
Close 1

66 Open D$(2) + LG$ + ".TOT" For Output As #1
Write #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
For I = 1 To 40: Write #1, WW(I), LL(I): Next
For I = 1 To 40: For I1 = 0 To 21: Write #1, TT(I, I1), TM$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 1 To 40: For I1 = 0 To 22: Write #1, TT1(I, I1), TT2$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next
Close #1

70 Call TITLE
Erase menuItems$
menuItems$(1) = "VIEW BATTING LEADERS "
menuItems$(2) = "VIEW PITCHING LEADERS"
menuItems$(3) = "VIEW TEAM TOTALS     "
menuItems$(4) = "RETURN TO MAIN MENU  "
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 30
Call box0(5, 29, 10, 51, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I = SELECTION%

72 If I = 1 Then 80

73 If I = 2 Then 1080

75 If I = 3 Then 2880

76 If I = 4 Then 2300

80 GoSub 2602

81 Color , 0: Cls: Print: Print: Print: J = 19: ZZ = 1

87 '
Locate 2, 4: Color 15, 4: Print " " + Chr$(214); String$(66, 196); "· ";
For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

200 For I = 0 To 15: Color 14: Locate I + 3, 8: Print Using "##"; I;: Print " ";: Color 0: Print TP$(I);: Color 14: Print Tab(40); Using "##"; I + 16;: Print " ";: Color 0: Print TP$(I + 16): Next

202 Print: Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

303 If B9 > 31 Then 200

304 If B9 = 0 Then X = 1: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

305 If B9 = 1 Then X = 2: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

306 If B9 = 2 Then X = 3: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

307 If B9 = 3 Then X = 4: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

308 If B9 = 4 Then X = 5: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

309 If B9 = 5 Then X = 6: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

310 If B9 = 6 Then X = 7: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

311 If B9 = 7 Then X = 8: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

312 If B9 = 8 Then X = 9: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

313 If B9 = 9 Then X = 10: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

314 If B9 = 10 Then X = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

315 If B9 = 11 Then X = 12: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

316 If B9 = 12 Then X = 13: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

317 If B9 = 13 Then X = 14: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

318 If B9 = 14 Then X = 15: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

319 If B9 = 15 Then X = 16: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

320 If B9 = 16 Then X = 17: stpl = 0: PPP = 0: GoSub 5000: GoTo 400

321 If B9 = 17 Then X = 18: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 18 Then X = 19: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 19 Then X = 20: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 20 Then X = 17: X1 = 18: X2 = 12: stpl = 0: PPP = 0: GoSub 5000: GoTo 560

323 If B9 = 21 Then X = 1: X1 = 2: X2 = 13: X3 = 8: stpl = 0: PPP = 0: GoSub 5000: GoTo 450

324 If B9 = 22 Then X = 1: X1 = 2: X2 = 5: X3 = 6: X4 = 7: stpl = 0: PPP = 0: GoSub 5000: GoTo 480

325 If B9 = 23 Then X = 1: X1 = 2: X2 = 8: stpl = 0: PPP = 0: GoSub 5000: GoTo 500

326 If B9 = 24 Then X = 1: X1 = 7: stpl = 0: PPP = 0: GoSub 5000: GoTo 520

327 If B9 = 25 Then X = 1: X1 = 9: stpl = 0: PPP = 0: GoSub 5000: GoTo 520

328 If B9 = 26 Then X = 1: X1 = 8: stpl = 0: PPP = 0: GoSub 5000: GoTo 520

329 If B9 = 27 Then X = 1: X1 = 4: stpl = 0: PPP = 0: GoSub 5000: GoTo 520

330 If B9 = 28 Then X = 1: X1 = 3: stpl = 0: PPP = 0: GoSub 5000: GoTo 520

331 If B9 = 29 Then X = 1: X1 = 16: stpl = 0: PPP = 0: GoSub 5000: GoTo 520

332 If B9 = 30 Then X = 21: X1 = 22: stpl = 0: PPP = 0: GoSub 5000: GoTo 540

390 If B9 = 31 Then 70

400 Cls: PPP = 0
For I = 1 To JB

    403 If BK%(I, X) = 0 Then 409 Else GoTo 405

    405 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    407 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = BK%(I, X)

409 Next I

410 GoSub 5200

417 TVN = 1: QW = PPP: GoSub 8000

418 If PV = 1 Then 650

419 If PV = 2 Then 850

450 Cls: PPP = 0
For I = 1 To JB

    453 If BK%(I, X) + BK%(I, X3) < GM1(I) * 3.1 Then 459 Else GoTo 455

    455 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    456 If BK%(I, X1) = 0 Then 458 Else GoTo 457

    457 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = BK%(I, X1) / BK%(I, X): GoTo 459

    458 TPP$(PPP) = BL$(PPP): TMM$(PPP) = TMB$(PPP): TYP(PPP) = 0

459 Next I

461 GoSub 5200

472 TVN = 2: QW = PPP: GoSub 8000

473 If PV = 1 Then 650

474 If PV = 2 Then 850

480 Cls: PPP = 0
For I = 1 To JB

    483 If BK%(I, X) < GM1(I) * 3.1 Then 489 Else GoTo 485

    485 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    486 If BK%(I, X1) = 0 And BK%(I, X2) = 0 And BK%(I, X3) = 0 And BK%(I, X4) = 0 Then 488 Else GoTo 487

    487 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = (BK%(I, X1) + BK%(I, X2) + (BK%(I, X3) * 2) + (BK%(I, X4) * 3)) / BK%(I, X): GoTo 489

    488 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = 0

489 Next I

490 GoSub 5200

497 TVN = 2: QW = PPP: GoSub 8000

498 If PV = 1 Then 650

499 If PV = 2 Then 850

500 Cls: PPP = 0
For I = 1 To JB

    503 If BK%(I, X) < GM1(I) * 3.1 Then 509 Else GoTo 505

    505 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    506 If BK%(I, X1) = 0 And BK%(I, X2) = 0 Then 508 Else GoTo 507

    507 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = (BK%(I, X1) + BK%(I, X2)) / (BK%(I, X) + BK%(I, X2)): GoTo 509

    508 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = 0

509 Next I

510 GoSub 5200

517 TVN = 2: QW = PPP: GoSub 8000

518 If PV = 1 Then 650

519 If PV = 2 Then 850

520 Cls: PPP = 0
For I = 1 To JB

    523 If BK%(I, X) < GM1(I) * 3.1 Then 529 Else GoTo 525

    525 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    526 If BK%(I, X1) = 0 Then 528 Else GoTo 527

    527 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = BK%(I, X) / BK%(I, X1): GoTo 529

    528 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = 0

529 Next I

530 GoSub 5200

537 TVN = 3: QW = PPP: GoSub 8000

538 If PV = 1 Then 650

539 If PV = 2 Then 850

540 Cls: PPP = 0
For I = 1 To JB

    543 If BK%(I, X) + BK%(I, X1) = 0 Then 549 Else GoTo 545

    545 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    546 If BK%(I, X1) / (BK%(I, X) + BK%(I, X1)) = 0 Then 548 Else GoTo 547

    547 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = BK%(I, X1) / (BK%(I, X) + BK%(I, X1)): GoTo 549

    548 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = 0

549 Next I

550 GoSub 5200

557 TVN = 2: QW = PPP: GoSub 8000

558 If PV = 1 Then 650

559 If PV = 2 Then 850

560 Cls: PPP = 0
For I = 1 To JB

    563 If BK%(I, X) + BK%(I, X1) + BK%(I, X2) = 0 Then 569 Else GoTo 565

    565 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    567 TPP$(PPP) = BL$(I): TMM$(PPP) = TMB$(I): TYP(PPP) = (BK%(I, X) + BK%(I, X1)) / (BK%(I, X) + BK%(I, X1) + BK%(I, X2))

569 Next I

570 GoSub 5200

577 TVN = 2: QW = PPP: GoSub 8000

578 If PV = 1 Then 650

579 If PV = 2 Then 850

650 Color 14, 0: Cls: Print "     PLAYER                  TEAM                     "; TP$(B9)

701 For W = 1 To QW

    702 If W = 20 Or W = 40 Or W = 60 Or W = 80 Or W = 100 Or W = 120 Or W = 140 Or W = 160 Or W = 180 Or W = 200 Then GoSub 731: GoSub 743
    If W = 220 Or W = 240 Or W = 260 Or W = 280 Or W = 300 Or W = 320 Or W = 340 Or W = 360 Or W = 380 Or W = 400 Then GoSub 731: GoSub 743
    If W = 420 Or W = 440 Or W = 460 Or W = 480 Or W = 500 Or W = 520 Or W = 540 Or W = 560 Or W = 580 Or W = 600 Then GoSub 731: GoSub 743
    If W = 620 Or W = 640 Or W = 660 Or W = 680 Or W = 700 Or W = 720 Or W = 740 Or W = 760 Or W = 780 Or W = 800 Then GoSub 731: GoSub 743
    If W = 820 Or W = 840 Or W = 860 Or W = 880 Or W = 900 Or W = 920 Then GoSub 731: GoSub 743

    703 If TVN = 1 Then 709

    704 If TVN = 2 Then 708

    705 If TVN = 3 Then 707

    707 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "###.##"; TYP(W): GoTo 710

    708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using ".###"; TYP(W): GoTo 710

    709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "#####"; TYP(W)

710 Next W

711 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
712 I$ = GetKeyPress$

713 If I$ = Chr$(27) Then 81

714 Locate 22: Print "                                                                "

715 Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
716 I$ = GetKeyPress$

717 If UCase$(I$) = "Y" Then 650

719 GoTo 81

731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
732 I$ = GetKeyPress$

733 If I$ = Chr$(27) Then 81

734 Return

743 Cls: Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)

745 Return

850 LPrint Chr$(15)

'851 WIDTH "LPT1:1", 130

852 Color 14, 0: Cls: LPrint "     PLAYER                  TEAM                     "; TP$(B9)

901 For W = 1 To LG

    902 If W = 50 Or W = 100 Or W = 150 Or W = 200 Then GoSub 943
    If W = 250 Or W = 300 Or W = 350 Or W = 400 Then GoSub 943
    If W = 450 Or W = 500 Or W = 550 Or W = 600 Then GoSub 943
    If W = 650 Or W = 700 Or W = 750 Or W = 800 Then GoSub 943
    If W = 850 Or W = 900 Then GoSub 943

    903 If TVN = 1 Then 909

    904 If TVN = 2 Then 908

    905 If TVN = 3 Then 907

    907 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.##"; TYP(W): GoTo 910

    908 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using ".###"; TYP(W): GoTo 910

    909 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "#####"; TYP(W)

910 Next W

911 LPrint Chr$(18)

'912 WIDTH "LPT1:1", 80

913 GoTo 70

943 LPrint Chr$(12)

944 LPrint "     PLAYER                  TEAM                     "; TP$(B9)

945 Return

1080 GoSub 2604

1081 Color , 0: Cls: Print: Print: Print: Print: J = 20: ZZ = 2

1087 '
Locate 2, 4: Color 15, 2: Print " " + Chr$(214); String$(66, 196); "· ";
For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

1200 For I = 0 To 14: Color 14: Locate I + 3, 8: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I);: Color 14: Print Tab(40); Using "##"; I + 15;: Print " ";: Color 0: Print TP1$(I + 15): Next

1202 Print: Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

1303 If B9 > 29 Then 1200

1304 If B9 = 0 Then X = 1: stpl = 0: PPP = 0: GoSub 5000: GoTo 1400

1305 If B9 = 1 Then X = 2: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1306 If B9 = 2 Then X = 3: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1307 If B9 = 3 Then X = 4: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1308 If B9 = 4 Then X = 5: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1309 If B9 = 5 Then X = 6: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500
If B9 = 6 Then X = 7: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1311 If B9 = 7 Then X = 8: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1312 If B9 = 8 Then X = 9: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1313 If B9 = 9 Then X = 10: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1314 If B9 = 10 Then X = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500
If B9 = 11 Then X = 12: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1316 If B9 = 12 Then X = 13: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1317 If B9 = 13 Then X = 14: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1318 If B9 = 14 Then X = 15: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1319 If B9 = 15 Then X = 16: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1320 If B9 = 16 Then X = 18: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1321 If B9 = 17 Then X = 19: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500
If B9 = 18 Then X = 18: X1 = 19: stpl = 0: PPP = 0: GoSub 5000: GoTo 1620

1322 If B9 = 19 Then X = 20: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1323 If B9 = 20 Then X = 22: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500

1324 If B9 = 21 Then X = 1: X1 = 4: stpl = 0: PPP = 0: GoSub 5000: GoTo 1600

1325 If B9 = 22 Then X = 1: X1 = 5: stpl = 0: PPP = 0: GoSub 5000: GoTo 1600

1326 If B9 = 23 Then X = 1: X1 = 6: stpl = 0: PPP = 0: GoSub 5000: GoTo 1600

1327 If B9 = 24 Then X = 1: X1 = 2: stpl = 0: PPP = 0: GoSub 5000: GoTo 1600

1328 If B9 = 25 Then X = 1: X1 = 14: stpl = 0: PPP = 0: GoSub 5000: GoTo 1600
If B9 = 26 Then X = 16: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500
If B9 = 27 Then X = 17: stpl = 0: PPP = 0: GoSub 5000: GoTo 1500
If B9 = 28 Then X = 9: X1 = 21: stpl = 0: PPP = 0: GoSub 5000: GoTo 1520

1390 If B9 = 29 Then GoTo 70

1400 Cls: PPP = 0:

1401 For I = 1 To JP

    1402 If PM%(I, X) = 0 Then 1405 Else GoTo 1403

    1403 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    1404 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP2(PPP) = PM%(I, X)

1405 Next I

1406 For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
    For J = PPP To I Step -1

        1407 If JX = 1 Then 1409

        1408 If JX = 2 Then 1410

        1409 If TYP2(J - 1) < TYP2(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP2(J - 1), TYP2(J)
        GoTo 1411

        1410 If TYP2(J - 1) > TYP2(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP2(J - 1), TYP2(J)

    1411 Next J
Next I

1412 For I = 1 To PPP
    TYP(I) = (Int(TYP2(I) / 3)): TYP1(I) = TYP2(I) - (Int(TYP2(I) / 3) * 3)
Next

1413 TVN = 1: QW = PPP: GoSub 8000

1414 If PV = 1 Then 1650

1415 If PV = 2 Then 1850

1500 Cls: PPP = 0

1501 For I = 1 To JP

    1502 If PM%(I, X) = 0 Then 1506 Else GoTo 1504

    1504 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    1505 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP(PPP) = PM%(I, X)

1506 Next I

1507 GoSub 5400

1508 TVN = 2: QW = PPP: GoSub 8000

1510 If PV = 1 Then 1650

1511 If PV = 2 Then 1850

1520 Cls: PPP = 0
For I = 1 To JP

    1523 If PM%(I, X1) = 0 Then 1529 Else GoTo 1525

    1525 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    1527 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP(PPP) = PM%(I, X1) / PM%(I, X): GoTo 1529

1529 Next

1530 GoSub 5400

1537 TVN = 3: QW = PPP: GoSub 8000

1538 If PV = 1 Then 1650

1539 If PV = 2 Then 1850

1600 Cls: PPP = 0
For I = 1 To JP

    1603 If PM%(I, X) / 3 < GM2(I) Then 1609 Else GoTo 1605

    1605 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    1606 If PM%(I, X) = 0 Or PM%(I, X1) = 0 Then 1608 Else GoTo 1607

    1607 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP(PPP) = (PM%(I, X1) * 9) / (PM%(I, X) / 3): GoTo 1609

    1608 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP(PPP) = 0

1609 Next

1610 GoSub 5400

1617 TVN = 3: QW = PPP: GoSub 8000

1618 If PV = 1 Then 1650

1619 If PV = 2 Then 1850

1620 Cls: PPP = 0
For I = 1 To JP

    1623 If PM%(I, X) = 0 Then 1629 Else GoTo 1625

    1625 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

    1626 If PM%(I, X1) = 0 Then 1628 Else GoTo 1627

    1627 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP(PPP) = PM%(I, X1) / PM%(I, X): GoTo 1629

    1628 TPP$(PPP) = PL$(I): TMM$(PPP) = TMP$(I): TYP(PPP) = 0

1629 Next

1630 GoSub 5400

1637 TVN = 4: QW = PPP: GoSub 8000

1638 If PV = 1 Then 1650

1639 If PV = 2 Then 1850

1650 Color 14, 0: Cls: Print "     PLAYER                  TEAM                     "; TP1$(B9)

1701 For W = 1 To QW

    1702 If W = 20 Or W = 40 Or W = 60 Or W = 80 Or W = 100 Or W = 120 Or W = 140 Or W = 160 Or W = 180 Or W = 200 Then GoSub 1731: GoSub 1743
    If W = 220 Or W = 240 Or W = 260 Or W = 280 Or W = 300 Or W = 320 Or W = 340 Or W = 360 Or W = 380 Or W = 400 Then GoSub 1731: GoSub 1743
    If W = 420 Or W = 440 Or W = 460 Or W = 480 Or W = 500 Or W = 520 Or W = 540 Or W = 560 Or W = 580 Or W = 600 Then GoSub 1731: GoSub 1743
    If W = 620 Or W = 640 Or W = 660 Or W = 680 Or W = 700 Or W = 720 Or W = 740 Or W = 760 Or W = 780 Or W = 800 Then GoSub 1731: GoSub 1743
    If W = 820 Or W = 840 Or W = 860 Or W = 880 Then GoSub 1731: GoSub 1743

    1703 If TVN = 1 Then 1709

    1704 If TVN = 2 Then 1708

    1705 If TVN = 3 Then 1707
    If TVN = 4 Then 1706

    1706 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using ".##"; TYP(W);: Print "%": GoTo 1710

    1707 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "###.##"; TYP(W): GoTo 1710

    1708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "#####"; TYP(W): GoTo 1710

    1709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "###"; TYP(W);: Print ".";: Print Using "#"; TYP1(W)

1710 Next W

1711 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
1712 I$ = GetKeyPress$

1713 If I$ = Chr$(27) Then 1081

1714 Locate 22: Print "                                                                "

1715 Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
1716 I$ = GetKeyPress$

1717 If UCase$(I$) = "Y" Then 1650

1718 If UCase$(I$) = "N" Then 1719

1719 GoTo 1081

1731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
1732 I$ = GetKeyPress$

1733 If I$ = Chr$(27) Then 1081

1734 Return

1743 Cls: Color 14: Print "     PLAYER                  TEAM                     "; TP1$(B9)

1745 Return

1850 LPrint Chr$(15)

'1851 WIDTH "LPT1:1", 130

1852 LPrint "     PLAYER                  TEAM                     "; TP1$(B9)

1901 For W = 1 To LG

    1902 If W = 50 Or W = 100 Or W = 150 Or W = 200 Then GoSub 1743
    If W = 250 Or W = 300 Or W = 350 Or W = 400 Then GoSub 1743
    If W = 450 Or W = 500 Or W = 550 Or W = 600 Then GoSub 1743
    If W = 650 Or W = 700 Or W = 750 Or W = 800 Then GoSub 1743
    If W = 850 Or W = 900 Then GoSub 1743

    1903 If TVN = 1 Then 1909

    1904 If TVN = 2 Then 1908

    1905 If TVN = 3 Then 1907

    1907 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.##"; TYP(W): GoTo 1910

    1908 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "#####"; TYP(W): GoTo 1910

    1909 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###"; TYP(W);: LPrint ".";: LPrint Using "#"; TYP1(W)

1910 Next W

1911 LPrint Chr$(18)

'1912 WIDTH "LPT1:1", 80

1913 GoTo 70

1943 LPrint Chr$(12)

1944 LPrint "     PLAYER                  TEAM                     "; TP1$(B9)

1945 Return

2300 Cls
Call TITLE
Erase menuItems$
menuItems$(1) = "UPDATE EXPANDED LEAGUE LEADER FILES  "
menuItems$(2) = "VIEW EXPANDED LEAGUE LEADER FILES    "
menuItems$(3) = "UPDATE/VIEW EXPANDED LEAGUE STANDINGS"
menuItems$(4) = "UPDATE/VIEW SEASON RECORDS           "
menuItems$(5) = "RETURN TO LEAGUE LEADER MAIN MENU    "
SELECTION% = 1
Count% = 5
Start% = 1
SCANCODE% = 5
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 23
Call box0(5, 22, 11, 60, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I = SELECTION%

2306 If I = 1 Then 2500
2307 If I = 2 Then 2600

If I = 3 Then Run "NEWLDRST.EXE"
If I = 4 Then Run "NEWLDRRC.EXE"

2355 Run "HELLO.EXE"

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
LGF$ = LG$

2530 Open D$(2) + LG$ + ".TMS" For Input As #2

2540 Input #2, LGF$: Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$

2550 For X = 1 To 40: Input #2, Z1$(X): Next

2560 Close 2

2565 GoTo 9

2600 Cls

2610 Spec$ = D$(2) + "*.TMS"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 2355
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

1315 Cls
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
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 2300
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1315
targetFile$ = RTrim$(targetFile$)
LG$ = Left$(targetFile$, Len(targetFile$) - 4)
LGF$ = LG$
GoTo 70

2602 For I = 1 To 880: TMP$(I) = "": PL$(I) = "": Next
For I = 1 To 40: For I1 = 0 To 22: TM$(I, I1) = "": TT2$(I, I1) = "": Next: Next
Open D$(2) + LGF$ + ".BAT" For Input As #1
Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
For I = 1 To 920: Input #1, BL$(I), TMB$(I), GM1(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
For I = 1 To 920: For I1 = 1 To 22: Input #1, BK%(I, I1): Next I1: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
Close 1: Return

2604 For I = 1 To 920: TMB$(I) = "": BL$(I) = "": Next
For I = 1 To 40: For I1 = 0 To 22: TM$(I, I1) = "": TT2$(I, I1) = "": Next: Next
Open D$(2) + LGF$ + ".PIT" For Input As #1
Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
For I = 1 To 880: Input #1, PL$(I), TMP$(I), GM2(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
For I = 1 To 880: For I1 = 1 To 23: Input #1, PM%(I, I1): Next I1: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
Close 1: Return

2606 For I = 1 To 920: TMB$(I) = "": BL$(I) = "": Next
For I = 1 To 880: TMP$(I) = "": PL$(I) = "": Next
Open D$(2) + LGF$ + ".TOT" For Input As #1
Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
For I = 1 To 40: Input #1, WW(I), LL(I): Next
For I = 1 To 40: For I1 = 0 To 21: Input #1, TT(I, I1), TM$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 1 To 40: For I1 = 0 To 22: Input #1, TT1(I, I1), TT2$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next
Close #1: Return

2880 GoSub 2606

2881 Color , 0: Cls: Print: J = 21: ZZ = 3

2887 '
Locate 2, 4: Color 15, 6: Print " " + Chr$(214); String$(66, 196); "· ";
For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

3000 For I = 0 To 19: Locate I + 3, 8: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP2$(I): Next

3001 For I = 20 To 39: Locate I - 17, 40: Color 11: Print Tab(40); Using "##"; I;: Print " ";: Color 0: Print TP2$(I): Next

3050 Color 15, 0: Locate 1, 23: Input "SELECT NUMBER TO VIEW"; B9

3051 If B9 > 39 Then 3050

3052 If B9 = 0 Then X = 0: XX = 0: stpl = 0: GoSub 5000: GoTo 3500

3053 If B9 = 1 Then X = 1: XX = 1: stpl = 0: GoSub 5000: GoTo 3500

3054 If B9 = 2 Then X = 2: XX = 2: stpl = 0: GoSub 5000: GoTo 3500

3055 If B9 = 3 Then X = 3: XX = 3: stpl = 0: GoSub 5000: GoTo 3500

3056 If B9 = 4 Then X = 4: XX = 4: stpl = 0: GoSub 5000: GoTo 3500

3057 If B9 = 5 Then X = 5: XX = 5: stpl = 0: GoSub 5000: GoTo 3500

3058 If B9 = 6 Then X = 6: XX = 6: stpl = 0: GoSub 5000: GoTo 3500

3060 If B9 = 7 Then X = 7: XX = 7: stpl = 0: GoSub 5000: GoTo 3500

3061 If B9 = 8 Then X = 8: XX = 8: stpl = 0: GoSub 5000: GoTo 3500

3062 If B9 = 9 Then X = 9: XX = 9: stpl = 0: GoSub 5000: GoTo 3500

3063 If B9 = 10 Then X = 10: XX = 10: stpl = 0: GoSub 5000: GoTo 3500

3064 If B9 = 11 Then X = 11: XX = 11: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 12 Then X = 18: XX = 12: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 13 Then X = 19: XX = 13: stpl = 0: GoSub 5000: GoTo 3500

3065 If B9 = 14 Then X = 0: X1 = 1: XX = 14: stpl = 0: GoSub 5000: GoTo 3510

3066 If B9 = 15 Then X = 0: X1 = 1: X2 = 4: X3 = 5: X4 = 6: XX = 15: stpl = 0: GoSub 5000: GoTo 3520

3067 If B9 = 16 Then X = 0: X1 = 1: X2 = 7: XX = 16: stpl = 0: GoSub 5000: GoTo 3530
If B9 = 17 Then X = 12: XX = 17: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 18 Then X = 15: XX = 18: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 19 Then X = 16: X1 = 17: X2 = 11: XX = 19: stpl = 0: GoSub 5000: GoTo 3540

3100 If B9 = 20 Then X = 0: XX = 20: stpl = 0: GoSub 5000: GoTo 3600

3101 If B9 = 21 Then X = 1: XX = 21: stpl = 0: GoSub 5000: GoTo 3612

3102 If B9 = 22 Then X = 2: XX = 22: stpl = 0: GoSub 5000: GoTo 3612

3103 If B9 = 23 Then X = 3: XX = 23: stpl = 0: GoSub 5000: GoTo 3612

3104 If B9 = 24 Then X = 4: XX = 24: stpl = 0: GoSub 5000: GoTo 3612

3105 If B9 = 25 Then X = 5: XX = 25: stpl = 0: GoSub 5000: GoTo 3612

3106 If B9 = 26 Then X = 10: XX = 26: stpl = 0: GoSub 5000: GoTo 3612

3107 If B9 = 27 Then X = 11: XX = 27: stpl = 0: GoSub 5000: GoTo 3612

3108 If B9 = 28 Then X = 12: XX = 28: stpl = 0: GoSub 5000: GoTo 3612

3109 If B9 = 29 Then X = 13: XX = 29: stpl = 0: GoSub 5000: GoTo 3612

3110 If B9 = 30 Then X = 14: XX = 30: stpl = 0: GoSub 5000: GoTo 3612

3111 If B9 = 31 Then X = 17: XX = 31: stpl = 0: GoSub 5000: GoTo 3612

3112 If B9 = 32 Then X = 18: XX = 32: stpl = 0: GoSub 5000: GoTo 3612
If B9 = 33 Then X = 17: X1 = 18: XX = 33: stpl = 0: GoSub 5000: GoTo 3630
If B9 = 34 Then X = 19: XX = 34: stpl = 0: GoSub 5000: GoTo 3612
If B9 = 35 Then X = 21: XX = 35: stpl = 0: GoSub 5000: GoTo 3612

3113 If B9 = 36 Then X = 0: X1 = 3: XX = 36: stpl = 0: GoSub 5000: GoTo 3620
If B9 = 37 Then X = 15: XX = 37: stpl = 0: GoSub 5000: GoTo 3612
If B9 = 38 Then X = 16: XX = 38: stpl = 0: GoSub 5000: GoTo 3612
If B9 = 39 Then 70

3500 Cls: For I = 1 To TS
TMM$(I) = TM$(I, X): TYP(I) = TT(I, X): Next

3502 GoSub 5500

3503 TVN = 1: QW = TS

3504 If PV = 1 Then 3650

3505 If PV = 2 Then 3850

3510 Cls: For I = 1 To TS
TMM$(I) = TM$(I, X): TYP(I) = TT(I, X1) / TT(I, X): Next

3512 GoSub 5500

3514 TVN = 2: QW = TS

3515 If PV = 1 Then 3650

3516 If PV = 2 Then 3850

3520 Cls: For I = 1 To TS
TMM$(I) = TM$(I, X): TYP(I) = (TT(I, X1) + TT(I, X2) + (TT(I, X3) * 2) + (TT(I, X4) * 3)) / TT(I, X): Next

3522 GoSub 5500

3524 TVN = 2: QW = TS

3525 If PV = 1 Then 3650

3526 If PV = 2 Then 3850

3530 Cls: For I = 1 To QW
TMM$(I) = TM$(I, X): TYP(I) = (TT(I, X1) + TT(I, X2)) / (TT(I, X) + TT(I, X2)): Next

3532 GoSub 5500

3534 TVN = 2: QW = TS

3535 If PV = 1 Then 3650

3536 If PV = 2 Then 3850

3540 Cls: For I = 1 To QW
TMM$(I) = TM$(I, X): TYP(I) = (TT(I, X) + TT(I, X1)) / (TT(I, X) + TT(I, X1) + TT(I, X2)): Next

3542 GoSub 5500

3544 TVN = 2: QW = TS

3545 If PV = 1 Then 3650

3546 If PV = 2 Then 3850

3600 Cls: For I = 1 To TTS
TMM$(I) = TT2$(I, X): TYP(I) = TT1(I, X): Next

3601 For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
    For J = TTS To I Step -1

        3603 If JX = 1 Then 3605

        3604 If JX = 2 Then 3606

        3605 If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 3607

        3606 If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    3607 Next J
Next I

3608 For I = 1 To TTS
    TYP(I) = (Int(TYP(I) / 3)): TYP1(I) = TYP(I) - (Int(TYP(I) / 3) * 3)
Next

3609 TVN = 1: QW = TTS

3610 If PV = 1 Then 4650

3611 If PV = 2 Then 4850

3612 Cls: For I = 1 To TTS
TMM$(I) = TT2$(I, X): TYP(I) = TT1(I, X): Next

3613 GoSub 5600

3614 TVN = 2: QW = TTS

3615 If PV = 1 Then 4650

3616 If PV = 2 Then 4850

3620 Cls: For I = 1 To TTS
TMM$(I) = TT2$(I, X): TYP(I) = (TT1(I, X1) * 9) / (TT1(I, X) / 3): Next

3621 GoSub 5600

3624 TVN = 3: QW = TTS

3625 If PV = 1 Then 4650

3626 If PV = 2 Then 4850

3630 Cls: For I = 1 To TTS
TMM$(I) = TT2$(I, X): TYP(I) = (TT1(I, X1) / TT1(I, X)) * 100: Next

3631 GoSub 5600

3634 TVN = 3: QW = TTS

3635 If PV = 1 Then 4650

3636 If PV = 2 Then 4850

3650 Color 14, 0: Cls:: Print "     TEAM                     "; TP2$(XX)

3701 For W = 1 To QW

    3702 If W = 20 Or W = 40 Then GoSub 3731: GoSub 3743

    3703 If TVN = 1 Then 3709

    3704 If TVN = 2 Then 3708

    3708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP(W): GoTo 3710

    3709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W)

3710 Next W

3711 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
3712 I$ = GetKeyPress$

3713 If I$ = Chr$(27) Then 2881

3714 Locate 22: Print "                                                                "

3715 Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
3716 I$ = GetKeyPress$

3717 If UCase$(I$) = "Y" Then 3650

3718 If UCase$(I$) = "N" Then 3719

3719 GoTo 2881

3731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
3732 I$ = GetKeyPress$

3733 If I$ = Chr$(27) Then 2881

3734 Return

3743 Cls: Color 14: Print "     TEAM                     "; TP2$(XX)

3745 Return

3850 LPrint Chr$(15)

'3851 WIDTH "LPT1:1", 130

3860 LPrint "     TEAM                     "; TP$(XX)

3901 For W = 1 To QW

    3903 If TVN = 1 Then 3909

    3904 If TVN = 2 Then 3908

    3908 Color 12: LPrint Using "###"; W;: LPrint " ";: Color 3: LPrint TMM$(W);: Color 15: LPrint Tab(33); Using ".###"; TYP(W): GoTo 3910

    3909 Color 12: LPrint Using "###"; W;: LPrint " ";: Color 3: LPrint TMM$(W);: Color 15: LPrint Tab(32); Using "#####"; TYP(W)

3910 Next W

3911 LPrint Chr$(12)
LPrint Chr$(18)

'3913 WIDTH "LPT1:1", 80

3914 GoTo 70

4650 Color 14, 0: Cls: Print "     TEAM                     "; TP2$(XX)

4701 For W = 1 To QW

    4702 If W = 20 Or W = 40 Then GoSub 4731: GoSub 4743

    4703 If TVN = 1 Then 4709

    4704 If TVN = 2 Then 4708

    4705 If TVN = 3 Then 4707

    4707 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "##.##"; TYP(W): GoTo 4710

    4708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W): GoTo 4710

    4709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W);: Print ".";: Print Using "#"; TYP1(W)

4710 Next W

4711 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
4712 I$ = GetKeyPress$

4713 If I$ = Chr$(27) Then 2881

4714 Locate 22: Print "                                                                "

4715 Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
4716 I$ = GetKeyPress$

4717 If UCase$(I$) = "Y" Then 4650

4718 If UCase$(I$) = "N" Then 4719

4719 GoTo 2881

4731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
4732 I$ = GetKeyPress$

4733 If I$ = Chr$(27) Then 2881

4734 Return

4743 Cls: Color 14: Print "     TEAM                     "; TP2$(XX)

4745 Return

4850 LPrint Chr$(15)

'4851 WIDTH "LPT1:1", 130

4859 LPrint "     TEAM                     "; TP1$(XX)

4901 For W = 1 To QW

    4903 If TVN = 1 Then 4909

    4904 If TVN = 2 Then 4908

    4905 If TVN = 3 Then 4907

    4907 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(32); Using "##.##"; TYP(W): GoTo 4910

    4908 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(32); Using "#####"; TYP(W): GoTo 4910

    4909 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(32); Using "#####"; TYP(W);: LPrint ".";: LPrint Using "#"; TYP1(W)

4910 Next W

4920 LPrint Chr$(12)

4921 LPrint Chr$(18)

'4922 WIDTH "LPT1:1", 80

4945 GoTo 70

5000 Print: Print: Print: J = 6: GoSub 5007: Color 3: Locate , 30: Print "SELECT OPTION:"
Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"
5002 I$ = GetKeyPress$

5003 If UCase$(I$) = "H" Then GoSub 7000: GoTo 5015

5004 If UCase$(I$) = "L" Then GoSub 7000: GoTo 5016

5005 If UCase$(I$) = "R" Then GoTo 5017
If UCase$(I$) <> "H" And UCase$(I$) <> "L" And UCase$(I$) <> "R" Then 5002

5007 '
Locate 8, 8: Color 15, 1: Print " " + Chr$(214); String$(57, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";: Next
Locate 9 + J, 8: Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";: Locate 10, 15: Return

5015 JX = 1: GoTo 5030

5016 JX = 2: GoTo 5030

5017 If ZZ = 1 GoTo 81
If ZZ = 2 GoTo 1081
If ZZ = 3 GoTo 2881

5030 Return

5200 For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
    For J = PPP To I Step -1

        5262 If JX = 1 Then 5264

        5263 If JX = 2 Then 5265

        5264 If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5266

        5265 If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5266 Next J
Next I

5267 Return

5400 For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
    For J = PPP To I Step -1

        5412 If JX = 1 Then 5414

        5413 If JX = 2 Then 5415

        5414 If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5416

        5415 If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5416 Next J
Next I

5417 Return

5500 For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
    For J = TS To I Step -1

        5503 If JX = 1 Then 5505

        5504 If JX = 2 Then 5506

        5505 If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5507

        5506 If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5507 Next J
Next I

5508 Return

5600 For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
    For J = TTS To I Step -1

        5613 If JX = 1 Then 5615

        5614 If JX = 2 Then 5616

        5615 If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5617

        5616 If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5617 Next J
Next I

5618 Return

7000 Print: Print: Print: Print: Print: J = 4: GoSub 7007: Color 12: Locate , 30: Print "SELECT OPTION:"
Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"
7002 I$ = GetKeyPress$

7003 If UCase$(I$) = "P" Then PV = 2: GoTo 7015

7004 If UCase$(I$) = "V" Then PV = 1: GoTo 7015
If UCase$(I$) = "R" Then GoTo 5017
If UCase$(I$) <> "P" And UCase$(I$) <> "V" And UCase$(I$) <> "R" Then 7002

7007 '
Locate 9, 15: Color 15, 0: Print " " + Chr$(214); String$(40, 196); "· ";
For II = 1 To J: Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";: Next
Locate 10 + J, 15: Print " " + Chr$(211); String$(40, 196); "½ ": Locate 10, 10

7015 Return

8000 Color , 0: Cls: If PV = 1 Then 8015
Print: Print: Print: Print: Print: J = 2: GoSub 8007
Locate , 25: Color 1: Print "MAXIMUM = "; QW
Locate , 25: Input "ENTER LENGTH OF LIST"; LG

8002 If LG > QW Then 8000

8003 GoTo 8015

8007 '
Locate 10, 20: Color 4, 15: Print " " + Chr$(214); String$(30, 196); "· ";
For II = 1 To J: Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";: Next
Locate 11 + J, 20: Print " " + Chr$(211); String$(30, 196); "½ ": Locate 11

8015 Return


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
