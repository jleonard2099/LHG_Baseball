'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - View Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim B$(22), P$(21), B0%(22, 22), P0%(21, 41), T(22), T1(22)
Dim H1$(40), P%(21), B%(22), H%(162), H$(162), VS%(162), PL$(162), W$(162), L$(162)
Dim D$(2), I%(162), L%(13), Q$(15), VteamNames$(200), teamIndex%(200), Y As Long, menuItems$(5)
Dim SS%(22, 6)

Open "DEV.BAS" For Input As #1
For I = 0 To 2
    Input #1, D$(I)
Next I
Close #1
50 Cls
Call TITLE
menuItems$(1) = "VIEW/PRINT COMPILED STATS FOR ONE TEAM "
menuItems$(2) = "PRINT REPORTS FOR ALL TEAMS IN A LEAGUE"
menuItems$(3) = "RETURN TO MAIN MENU                    "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 22

Call box0(5, 21, 9, 61, 2, 15, 1)
_MouseShow "Default"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 100
I = SELECTION% - 1
If I = 0 Then 86
If I = 1 Then 2000
If I = 2 Then 100

86 Cls
GoSub 3056
If BO% = 1 Then BO% = 0: GoTo 50
B$ = T$
For XX = 4 To 11
    If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
QQ = 8
For XX = 11 To 4 Step -1
    If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620
6615 Next XX
6620 Rem:Z$ = LEFT$(Z$, 11)
Rem:Z$ = LEFT$(RIGHT$(B$, LEN(B$) - 3), 8) + "." + LEFT$(B$, 2)
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)


93 If Not _FileExists(D$(1) + Z$) Then GoSub 4550: GoTo 50
Open D$(1) + Z$ For Input As #1
Input #1, A$: Input #1, W2%: Input #1, L2%
For I = 0 To 22: Input #1, B$(I): Input #1, B%(I): Next I
For I = 0 To 21: Input #1, P$(I): Input #1, P%(I): Next I
For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I
For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I
Input #1, D0%: Input #1, D1%
For I = 0 To 21: Input #1, T(I): Next I
For I = 0 To 22: Input #1, T1(I): Next I
Close #1

W = 0: L = 0: RW = 0: RL = 0: ORW = 0: ORL = 0: HW = 0: HL = 0: EW = 0: EL = 0: LTL = 0: LTW = 0

If Not _FileExists(D$(1) + Z$ + "R") Then SS% = 0: GoTo 95
SS% = 1
Open D$(1) + Z$ + "R" For Input As #1
For I = 0 To 22
    For I1 = 0 To 6
Input #1, SS%(I, I1): Next I1: Next I
Close #1

95 Open D$(1) + Z$ + "S" For Input As #1
Input #1, GAMES%
For I = 1 To 162: Input #1, H%(I): Next I
For I = 1 To 162: Input #1, H$(I): Next I
For I = 1 To 162: Input #1, VS%(I): Next I
For I = 1 To 162: Input #1, PL$(I): Next I
For I = 1 To 162: Input #1, W$(I): Next I
For I = 1 To 162: Input #1, L$(I): Next I
For I = 1 To 162: Input #1, I%(I): Next I
Close #1

For I = 1 To GAMES%
    If Abs(H%(I) - VS%(I)) = 1 And H%(I) > VS%(I) Then ORW = ORW + 1
    If Abs(H%(I) - VS%(I)) = 1 And H%(I) < VS%(I) Then ORL = ORL + 1
    If H%(I) > VS%(I) Then W = W + 1
    If H%(I) > VS%(I) And I%(I) > 9 Then EW = EW + 1
    If H%(I) > VS%(I) And PL$(I) = "A" Then RW = RW + 1
    If H%(I) > VS%(I) And PL$(I) = "H" Then HW = HW + 1
    If VS%(I) > H%(I) Then L = L + 1
    If VS%(I) > H%(I) And I%(I) > 9 Then EL = EL + 1
    If VS%(I) > H%(I) And PL$(I) = "A" Then RL = RL + 1
    If VS%(I) > H%(I) And PL$(I) = "H" Then HL = HL + 1
Next I

If GAMES% <= 10 Then LTL = L: LTW = W: GoTo 138
M = GAMES% - 9
For J = M To GAMES%
    If H%(J) > VS%(J) Then LTW = LTW + 1
    If VS%(J) > H%(J) Then LTL = LTL + 1
Next J
138 GoTo 8530



100 '
110 Run "HELLO.EXE"



200 Cls: Color 12, 0: Color L%(11), L%(12): Print A$;
Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
Color 15, 1: Print "BATTERS      G  CHS LHS GIDP NEEDS PROJ AB  PO   A  E  FAVG  SBA CS %CS SH SF": Color 15
203 For I = 0 To 22
    If B$(I) = "XXX" Or B$(I) = "" Then 205
    Locate I + 3, 1
    204 Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
    GoSub 300
205 Next I
GoSub 8670
GoTo 8550



230 Cls: Color L%(11), L%(12)
Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
Color 15, 1: Print "PITCHERS     G  CHS LHS GIDP   PO   A  E  FAVG SH SF": Color 15
For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "" Then 235
    Locate I + 3, 1
    Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
    GoSub 400
235 Next I
Locate 1, 1
GoSub 8670
GoTo 8562


300 Print Left$(B$(I), 10);
Locate , 12: Print Using "###"; B0%(I, 14);
Locate , 18: Print Using "##"; B0%(I, 12);
Locate , 22: Print Using "##"; B0%(I, 13);
Locate , 27: Print Using "##"; B0%(I, 15);: GoSub 9450
Print Using " #### ### ## "; B0%(I, 16); B0%(I, 17); B0%(I, 11);
If B0%(I, 16) + B0%(I, 17) + B0%(I, 11) = 0 Then 311
If B0%(I, 11) = 0 Then Print "1.000";: GoTo 311
Print Using " .###"; (B0%(I, 16) + B0%(I, 17)) / (B0%(I, 16) + B0%(I, 17) + B0%(I, 11));
311 If B0%(I, 21) + B0%(I, 20) = 0 Then 313
Print Using "  ### ## ###"; B0%(I, 20) + B0%(I, 21); B0%(I, 21); B0%(I, 21) / (B0%(I, 20) + B0%(I, 21)) * 100;
313 Locate , 73: Print Using "## ##"; B0%(I, 18); B0%(I, 19);
Return
400 Print Left$(P$(I), 10);
Locate , 12: Print Using "###"; P0%(I, 8);
Locate , 18: Print Using "##"; P0%(I, 35);
Locate , 22: Print Using "##"; P0%(I, 36);
Locate , 27: Print Using "##"; P0%(I, 37);: Locate , 29
Print Using " #### ### ## "; P0%(I, 38); P0%(I, 39); P0%(I, 34);
If P0%(I, 38) + P0%(I, 39) + P0%(I, 34) = 0 Then 432
If P0%(I, 34) = 0 Then Print "1.000";: GoTo 432
Print Using " .###"; (P0%(I, 38) + P0%(I, 39)) / (P0%(I, 38) + P0%(I, 39) + P0%(I, 34));
432 Locate , 48: Print Using "## ##"; P0%(I, 40); P0%(I, 41);
Return
2000 Cls
Spec$ = D$(2) + "*.TMS"
Count = FCount%(Spec$)
If Count = 0 Then GoSub 4575: GoTo 50
ReDim leagueFiles$(0 To Count)
For X = 1 To Count
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO SCHEDULE": Color 15
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
Print "-SELECTS LEAGUE";

_MouseShow "Default"
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 50
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$: Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$=GetKeyPress$
If UCase$(I$) = "Y" Then 1310
3911 targetFile$ = RTrim$(targetFile$)
LH$ = Left$(targetFile$, Len(targetFile$) - 4)
Open D$(2) + LH$ + ".TMS" For Input As #2
Input #2, LG$, D1$, D2$, D3$, D4$
For XW = 1 To 40
    2002 Input #2, H1$(XW)
Next
Close 2
2010 For XW = 1 To 40
    2015 If Not _FileExists(D$(1) + H1$(XW)) Then 2030
    Open D$(1) + H1$(XW) For Input As #1
    Input #1, A$: Input #1, W2%: Input #1, L2%
    For I = 0 To 22: Input #1, B$(I): Input #1, B%(I): Next I
    For I = 0 To 21: Input #1, P$(I): Input #1, P%(I): Next I
    For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I
    For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I
    Input #1, D0%: Input #1, D1%
    For I = 0 To 21: Input #1, T(I): Next I
    For I = 0 To 22: Input #1, T1(I): Next I
    Close #1
    BP = 1
    X = 3
    GoSub 7000
2030 Next
GoTo 50
3056 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$=GetKeyPress$
If I$ = Chr$(27) Then Z$ = "END": Color 15, 0: GoTo 50
If I$ = Chr$(13) Then GoSub 1102: GoTo 182
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
For I = 1 To 200: VteamNames$(I) = "": Next
1441 GoSub 1101
182 If BO% = 1 Then BO% = 0: Return
183 Locate 2, 33
Color 15, 0
Color L%(11), L%(12)
Print T$
Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";: I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3056
Color 15, 0
Return

1101 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then BO% = 1: GoSub 4500: Return
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1103
    Next I2
    NT = NT + 1
    VteamNames$(NT) = Q$(0): teamIndex%(NT) = I1
1103 Next I1
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
_MouseShow "Default"
MAMenu VteamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide
If ExitCode% = 27 Then BO% = 1: Return
T = teamIndex%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then GoSub 4500: BO% = 1: Return
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
Get #1, T

T$ = RTrim$(Q$(0))

For I = 1 To 13
    L%(I) = CVI(Q$(I))
Next
Close 1
Return

4091 Rem
4093 I$=GetKeyPress$
4094 Return


5100 JJ = 1
Locate 6, 8
Color L%(11), L%(12)

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 7
Return

7000 Rem:*** NEW PRINT ROUTINE 6-24-90 ***
7001 GoSub 9910
LPrint Chr$(15) ': WIDTH "LPT1:", 137
7003 LPrint A$; "    WON: "; W2%; " LOST: "; L2%
7004 LPrint "BATTERS      G  AB   R   H 2B 3B HR RBI  BB   K  SB CS CHS LHS GIDP  BAVG   SLG   OBP NEED PROJ.AB %RAB   PO   A  E  FAVG SH SF"
7005 For I = 0 To 22
    If B$(I) = "XXX" Or B$(I) = "" Then 7111
    7015 LPrint Left$(B$(I), 10); Tab(12);: LPrint Using "###"; B0%(I, 14);
    7020 LPrint Tab(16); Using "### ### ### ##"; B0%(I, 0); B0%(I, 2); B0%(I, 1); B0%(I, 4);
    7040 LPrint Tab(31); Using "## ## ### ###"; B0%(I, 5); B0%(I, 6); B0%(I, 3); B0%(I, 7);
    LPrint Tab(45); Using "### ### ##"; B0%(I, 8); B0%(I, 9); B0%(I, 10);
    7080 LPrint Tab(56);: LPrint Using "##"; B0%(I, 12);
    LPrint Tab(60);: LPrint Using "##"; B0%(I, 13);
    LPrint Tab(64);: LPrint Using "##"; B0%(I, 15);
    7095 LPrint Tab(69);: GoSub 7800
    7100 LPrint Tab(75);: GoSub 7820
    7105 LPrint Tab(81);: GoSub 7840
    7110 LPrint Tab(87);: GoSub 7900
    7115 LPrint Tab(95);: GoSub 7910
    7120 LPrint Tab(100);: GoSub 7940
    7121 LPrint Tab(105);: LPrint Using "####"; B0%(I, 16);
    7122 LPrint Tab(110);: LPrint Using "###"; B0%(I, 17);
    7123 LPrint Tab(114);: LPrint Using "##"; B0%(I, 11);: LPrint Tab(117);
    7124 If B0%(I, 11) + B0%(I, 17) + B0%(I, 16) = 0 Then LPrint " .---";: GoTo 7129
    7125 If B0%(I, 11) = 0 Then LPrint "1.000";: GoTo 7129
    7126 LPrint Using " .###"; (B0%(I, 16) + B0%(I, 17)) / (B0%(I, 16) + B0%(I, 17) + B0%(I, 11));
    7129 LPrint Tab(123); Using "## ##"; B0%(I, 18); B0%(I, 19)
7111 Next
7065 For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "" Then 7086
    7066 LPrint Left$(P$(I), 10); Tab(12);: LPrint Using "###"; P0%(I, 8);
    7067 LPrint Tab(16); Using "### ### ### ##"; P0%(I, 23); P0%(I, 25); P0%(I, 24); P0%(I, 27);
    7068 LPrint Tab(31); Using "## ## ### ###"; P0%(I, 28); P0%(I, 29); P0%(I, 26); P0%(I, 30);
    7069 LPrint Tab(45); Using "### ### ##"; P0%(I, 31); P0%(I, 32); P0%(I, 33);
    7070 LPrint Tab(56);: LPrint Using "##"; P0%(I, 35);
    7071 LPrint Tab(60);: LPrint Using "##"; P0%(I, 36);
    7072 LPrint Tab(64);: LPrint Using "##"; P0%(I, 37);
    7073 LPrint Tab(69);: GoSub 7600
    7074 LPrint Tab(75);: GoSub 7620
    7075 LPrint Tab(81);: GoSub 7640
    7079 LPrint Tab(105);: LPrint Using "####"; P0%(I, 38);
    LPrint Tab(110);: LPrint Using "###"; P0%(I, 39);
    7081 LPrint Tab(114);: LPrint Using "##"; P0%(I, 34);: LPrint Tab(117);
    7082 If P0%(I, 34) + P0%(I, 38) + P0%(I, 39) = 0 Then LPrint " .---";: GoTo 7085
    7083 If P0%(I, 34) = 0 Then LPrint "1.000";: GoTo 7085
    7084 LPrint Using " .###"; (P0%(I, 38) + P0%(I, 39)) / (P0%(I, 38) + P0%(I, 39) + P0%(I, 34));
    7085 LPrint Tab(123); Using "## ##"; P0%(I, 40); P0%(I, 41)
7086 Next
7130 LPrint " ": LPrint "TOTALS";: LPrint Tab(12);: LPrint Using "###"; GAMES%;
7131 LPrint Tab(19);: LPrint Using "####"; T(2);
7132 LPrint Tab(27);: LPrint Using "###"; T(4);
7133 LPrint Tab(33);: LPrint Using "###"; T(6);
7134 LPrint Tab(40);: LPrint Using "####"; T(7);
7135 LPrint Tab(49);: LPrint Using "###"; T(9);
7137 LPrint Tab(69);: LPrint Using " .###"; (T(1) / T(0));
7138 LPrint Tab(75);: LPrint Using " .###"; ((T(1) + T(4) + T(5) * 2 + T(6) * 3) / T(0));
7139 LPrint Tab(81);: LPrint Using " .###"; ((T(1) + T(7)) / (T(7) + T(0)));
7140 LPrint Tab(105);: LPrint Using "####"; T(16);
7141 LPrint Tab(113);: LPrint Using "###"; T(11);
LPrint Tab(122); Using "###"; T(18)
7150 LPrint Tab(15);: LPrint Using "####"; T(0);
7151 LPrint Tab(23);: LPrint Using "####"; T(1);
7152 LPrint Tab(30);: LPrint Using "###"; T(5);
7153 LPrint Tab(36);: LPrint Using "####"; T(3);
7154 LPrint Tab(44);: LPrint Using "####"; T(8);
7155 LPrint Tab(52);: LPrint Using "####"; T(10);
LPrint Tab(63); Using "###"; T(15);
7156 LPrint Tab(109);: LPrint Using "####"; T(17);
7157 LPrint Tab(117);: If T(16) + T(11) + T(17) = 0 Then LPrint " .---";: GoTo 7200
7158 If T(11) = 0 Then LPrint "1.000";: GoTo 7200
7159 LPrint Using " .###"; (T(16) + T(17)) / (T(16) + T(11) + T(17));
7200 LPrint Tab(125); Using "###"; T(19)
LPrint: LPrint
TT$ = Str$(T(2) / (W2% + L2%) + .05): UU$ = Str$(T1(2) / (W2% + L2%) + .05): LPrint "RUNS PER GAME ";: LPrint Left$(TT$, 4);: LPrint " OPPONENTS";: LPrint Left$(UU$, 4)
LPrint
LPrint
LPrint Tab(20); "RECORD         ("; Str$(W); " -"; Str$(L); " )"
LPrint Tab(20); "HOME RECORD    ("; Str$(HW); " -"; Str$(HL); " )"
LPrint Tab(20); "ROAD RECORD    ("; Str$(RW); " -"; Str$(RL); " )"
LPrint Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
LPrint Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
LPrint Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
LPrint Chr$(12)
LPrint A$; "    WON: "; W2%; " LOST: "; L2%
7301 LPrint "PITCHERS    W  L SV  IP     H  BB  SO  ERA    G GS CG RUN  ER ShO HR DP %RIP SH SF  RI RIS BS   SUP QS"
7305 For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "" Then 7352
    LPrint Left$(P$(I), 10);
    7307 LPrint Tab(12);: LPrint Using "##"; P0%(I, 6);
    7310 LPrint Tab(15);: LPrint Using "##"; P0%(I, 7);
    7311 LPrint Tab(18);: LPrint Using "##"; P0%(I, 12);
    7312 LPrint Tab(21);: LPrint Using "###"; Int(P0%(I, 0) / 3);: If P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3);
    7313 LPrint Tab(27);: LPrint Using "###"; P0%(I, 1);
    7314 LPrint Tab(31);: LPrint Using "###"; P0%(I, 4);
    7315 LPrint Tab(35);: LPrint Using "###"; P0%(I, 5);
    7320 I2 = 0: If P0%(I, 0) > 0 Then LPrint Tab(39);: LPrint Using "##.##"; (P0%(I, 3) * 9 / (P0%(I, 0) / 3));
    7321 If P0%(I, 0) = 0 Then LPrint Tab(39); " -.--";
    7325 LPrint Tab(45);: LPrint Using "###"; P0%(I, 8);
    7327 LPrint Tab(49);: LPrint Using "##"; P0%(I, 9);
    7328 LPrint Tab(52);: LPrint Using "##"; P0%(I, 10);
    7329 LPrint Tab(55);: LPrint Using "###"; P0%(I, 2);
    7330 LPrint Tab(59);: LPrint Using "###"; P0%(I, 3);
    7331 LPrint Tab(63);: LPrint Using "###"; P0%(I, 11);
    7332 LPrint Tab(67);: LPrint Using "##"; P0%(I, 14);
    7333 LPrint Tab(70);: LPrint Using "##"; P0%(I, 13);
    7335 LPrint Tab(74);
    7350 If P%(I) > 0 Then LPrint Using "###"; Int((((P0%(I, 0) / 3) * 100) / P%(I)) + .5);
    7351 If P%(I) = 0 Then LPrint "  ";
    LPrint Tab(78); Using "## ## ### ### ## "; P0%(I, 15); P0%(I, 16); P0%(I, 17); P0%(I, 18); P0%(I, 19);
    If P0%(I, 9) > 0 Then LPrint Using "##.## "; P0%(I, 20) / P0%(I, 9);
    LPrint Tab(101); Using "##"; P0%(I, 21)
7352 Next
7360 LPrint
7365 LPrint "TOTALS";
7366 LPrint Tab(18);: LPrint Using "##"; T1(12);
7367 LPrint Tab(26);: LPrint Using "####"; T1(1);
7368 LPrint Tab(34);: LPrint Using "####"; T1(5);
7369 LPrint Tab(39);: LPrint Using "##.##"; T1(3) / (T1(0) / 3) * 9;
7370 LPrint Tab(52);: LPrint Using "##"; T1(10);
7371 LPrint Tab(58);: LPrint Using "####"; T1(3);
7372 LPrint Tab(63);: LPrint Using "###"; T1(11);
7373 LPrint Tab(69);: LPrint Using "###"; T1(13);
LPrint Tab(77); Using "###"; T1(15);
LPrint Tab(83); Using "####"; T1(17);
LPrint Tab(91); Using "###"; T1(19);
LPrint Tab(100); Using "###"; T1(21)
7376 LPrint Tab(21);: LPrint Using "####"; T1(0) / 3;
7377 LPrint Tab(30);: LPrint Using "####"; T1(4);
7378 LPrint Tab(54);: LPrint Using "####"; T1(2);
7379 LPrint Tab(66);: LPrint Using "###"; T1(14);
LPrint Tab(80); Using "###"; T1(16);
LPrint Tab(87); Using "####"; T1(18)
LPrint: LPrint: LPrint
LPrint "CATCHERS    SBA   CS   CS%"
For I = 0 To 22
    If B$(I) = "XXX" Or B$(I) = "" Or B0%(I, 21) + B0%(I, 20) = 0 Then 7384
    LPrint Left$(B$(I), 10); Tab(12); Using "###   ##   ###"; B0%(I, 20) + B0%(I, 21); B0%(I, 21); B0%(I, 21) / (B0%(I, 20) + B0%(I, 21)) * 100
7384 Next
If SS% = 0 Then 7500
Rem:DISPLAY SPLIT STATS
LPrint A$;
LPrint Tab(15); "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
For I = 0 To 22
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then 7386
    LPrint B$(I);
    LPrint Tab(20); Using "### ## ## ## ### ### "; SS%(I, 0); SS%(I, 2); SS%(I, 3); SS%(I, 4); SS%(I, 5); SS%(I, 6);
    If SS%(I, 0) = 0 Or SS%(I, 1) = 0 Then LPrint " .000";: GoTo 7382
    If SS%(I, 0) = SS%(I, 1) Then LPrint "1.000";: GoTo 7382
    LPrint Using " .###"; SS%(I, 1) / SS%(I, 0);

    7382 LPrint Tab(53); Using "### ## ## ## ### ### "; B0%(I, 0) - SS%(I, 0); B0%(I, 4) - SS%(I, 2); B0%(I, 5) - SS%(I, 3); B0%(I, 6) - SS%(I, 4); B0%(I, 7) - SS%(I, 5); B0%(I, 8) - SS%(I, 6);
    If B0%(I, 0) - SS%(I, 0) = 0 Or B0%(I, 1) - SS%(I, 1) = 0 Then LPrint " .000": GoTo 7385
    If B0%(I, 0) - SS%(I, 0) = B0%(I, 1) - SS%(I, 1) Then LPrint "1.000": GoTo 7385
    LPrint Using " .###"; (B0%(I, 1) - SS%(I, 1)) / (B0%(I, 0) - SS%(I, 0))
    7385 Rem
7386 Next I
7500 LPrint Chr$(18): LPrint Chr$(12)
7505 Return
7600 If P0%(I, 23) = 0 Then LPrint "-.---";: GoTo 7615
7605 If P0%(I, 23) = P0%(I, 24) Then LPrint "1.000";: GoTo 7615
7606 If P0%(I, 24) = 0 Then LPrint " .000";: GoTo 7615
7610 LPrint Using " .###"; (P0%(I, 24) / P0%(I, 23));
7615 Return
7620 If P0%(I, 23) = 0 Then LPrint "-.---";: GoTo 7635
7625 J = P0%(I, 24) + P0%(I, 27) + P0%(I, 28) * 2 + P0%(I, 29) * 3
If J = P0%(I, 23) Then LPrint "1.000";: GoTo 7635
If J / P0%(I, 23) > 1 Then LPrint Using "#.###"; (J / P0%(I, 23));: GoTo 7635
7626 If P0%(I, 24) = 0 Then LPrint " .000";: GoTo 7635
7630 LPrint Using " .###"; (J / P0%(I, 23));
7635 Return
7640 If P0%(I, 23) = 0 Then LPrint "-.---";: GoTo 7655
7645 J = P0%(I, 24) + P0%(I, 30)
If J = P0%(I, 23) Then LPrint "1.000";: GoTo 7655
If J / (P0%(I, 30) + P0%(I, 23)) > 1 Then LPrint Using "#.###"; (J / (P0%(I, 30) + P0%(I, 23)));: GoTo 7635
7646 If J = 0 Then LPrint " .000";: GoTo 7655
7650 LPrint Using " .###"; (J / (P0%(I, 30) + P0%(I, 23)));
7655 Return
7800 If B0%(I, 0) = 0 Then LPrint "-.---";: GoTo 7815
7805 If B0%(I, 0) = B0%(I, 1) Then LPrint "1.000";: GoTo 7815
7806 If B0%(I, 1) = 0 Then LPrint " .000";: GoTo 7815
7810 LPrint Using " .###"; (B0%(I, 1) / B0%(I, 0));
7815 Return
7820 If B0%(I, 0) = 0 Then LPrint "-.---";: GoTo 7835
7825 J = B0%(I, 1) + B0%(I, 4) + B0%(I, 5) * 2 + B0%(I, 6) * 3
If J = B0%(I, 0) Then LPrint "1.000";: GoTo 7835
If J / B0%(I, 0) > 1 Then LPrint Using "#.###"; (J / B0%(I, 0));: GoTo 7835
7826 If B0%(I, 1) = 0 Then LPrint " .000";: GoTo 7835
7830 LPrint Using " .###"; (J / B0%(I, 0));
7835 Return
7840 If B0%(I, 0) = 0 Then LPrint "-.---";: GoTo 7855
7845 J = B0%(I, 1) + B0%(I, 7)
If J = B0%(I, 0) Then LPrint "1.000";: GoTo 7855
If J / (B0%(I, 7) + B0%(I, 0)) > 1 Then LPrint Using "#.###"; (J / (B0%(I, 7) + B0%(I, 0)));: GoTo 7835
7846 If J = 0 Then LPrint " .000";: GoTo 7855
7850 LPrint Using " .###"; (J / (B0%(I, 7) + B0%(I, 0)));
7855 Return
7900 RQ$ = Right$(Z$, 2): RT = 60: RQ = Val(RQ$)
7901 If RQ > RT Then QT = 162: GoTo 7905
7903 QT = 154
7905 If B%(I) > 0 And (Int((((B%(I) + 0) / QT) * (T1(9) + 1)) + .5) - (B0%(I, 0) + 0)) > 0 Then LPrint Using "###"; Int((((B%(I) + 0) / QT) * (T1(9) + 1)) + .5) - (B0%(I, 0) + 0);: Return
7907 If B%(I) > 0 Then LPrint Using "###"; Int(((B%(I) / QT) * (T1(9) + 1)) + .5) - (B0%(I, 0) + 0);
7908 Return
7910 If B%(I) > 0 Then H5% = Int((((B%(I) + 0) / QT) * (T1(9) + 1)) + .5): LPrint Using "###"; H5%;: T(13) = T(13) + H5%
7915 Return
7940 If B%(I) > 0 Then LPrint Using "###"; ((B0%(I, 0) / B%(I)) * 100);
7942 Return
8530 GoSub 8650
If X = 2 Or X = 3 Then GoSub 7000
8531 GoSub 9810
8532 Cls: Color L%(11), L%(12): Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
Color 15, 1
Print "BATTERS    AB   R   H RBI  2B 3B  HR    BA   SLG   OBP  SO  BB  SB  CS %RAB"
Color 15, 0
For I = 0 To 22
    If B$(I) = "XXX" Or B$(I) = "" Then 8533
    Locate I + 3, 1
    Color 7: If I / 2 = Int(I / 2) Then Color 15
    Print Left$(B$(I), 10);
    GoSub 8540
8533 Next I
Locate 1, 1
GoSub 8670
GoTo 200

8540 Locate , 11: Print Using "###"; B0%(I, 0);
Locate , 15: Print Using "###"; B0%(I, 2);
Locate , 19: Print Using "###"; B0%(I, 1);
Locate , 23: Print Using "###"; B0%(I, 3);
Locate , 28: Print Using "##"; B0%(I, 4);
Locate , 31: Print Using "##"; B0%(I, 5);
Locate , 35: Print Using "##"; B0%(I, 6);
GoSub 9000
Locate , 56: Print Using "###"; B0%(I, 8);
Locate , 60: Print Using "###"; B0%(I, 7);
Locate , 64: Print Using "###"; B0%(I, 9);
Locate , 69: Print Using "##"; B0%(I, 10);: GoSub 9400
If T(0) = 0 Then T(0) = 1
Return
8550 Cls: Color L%(11), L%(12): Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
Color 15, 1
Print "PITCHERS   AB   R   H RBI  2B 3B  HR    BA   SLG   OBP  SO  BB  SB  CS"
For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "" Then 8555
    Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
    Locate I + 3, 1: Print Left$(P$(I), 10);
    GoSub 8560
8555 Next I
Locate 1, 1
GoSub 8670
GoTo 230
8560 Locate , 11: Print Using "###"; P0%(I, 23);
Locate , 15: Print Using "###"; P0%(I, 25);
Locate , 19: Print Using "###"; P0%(I, 24);
Locate , 23: Print Using "###"; P0%(I, 26);
Locate , 28: Print Using "##"; P0%(I, 27);
Locate , 31: Print Using "##"; P0%(I, 28);
Locate , 35: Print Using "##"; P0%(I, 29);
GoSub 9005
Locate , 56: Print Using "###"; P0%(I, 31);
Locate , 60: Print Using "###"; P0%(I, 30);
Locate , 64: Print Using "###"; P0%(I, 32);
Locate , 69: Print Using "##"; P0%(I, 33);
If T(0) = 0 Then T(0) = 1
Return
8562 Cls
Color L%(11), L%(12): Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%;: Color 14, 0: Print "  TEAM BATTING TOTALS"
Color 15, 1: Print "  AB    R    H  RBI  2B  3B  HR    BA   SLG   OBP   SO   BB  SB  CS"
Color 14, 0
Print Using "#### #### #### #### ### ### ###"; T(0); T(2); T(1); T(3); T(4); T(5); T(6);
Locate , 34: Print ".";: Print Using "###"; Int((T(1) / T(0) + .0005) * 1000);
Locate , 40: Print ".";
Print Using "###"; Int((((T(1) + T(4) + 2 * T(5) + 3 * T(6)) / T(0)) + .0005) * 1000);
Locate , 46: Print ".";
Print Using "###"; Int(((T(1) + T(7)) / (T(7) + T(0)) + .0005) * 1000);
Locate , 51: Print Using "#### #### ### ###"; T(8); T(7); T(9); T(10)
Locate 8, 1: Color 15, 1: Print "GIDP   PO    A   E  FAVG  SH  SF  LOB": Color 15
Color 14, 0
Print Using "#### #### #### ###"; T(15); T(16); T(17); T(11);
Locate , 20: If T(11) = 0 Then Print "1.000";
If T(11) <> 0 Then Print Using " .###"; (T(16) + T(17)) / (T(16) + T(17) + T(11));
Print Using " ### ### ####"; T(18); T(19); T(12)
GoSub 8670
If SS% = 0 Then 8570
Rem:DISPLAY SPLIT STATS
Cls
Color L%(11), L%(12): Print A$;
Locate 1, 15
Color 14, 0
Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
Color 15, 0
Locate 2
For I = 0 To 22
    If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then 2806
    Print B$(I);
    Locate , 20: Print Using "### ## ## ## ### ### "; SS%(I, 0); SS%(I, 2); SS%(I, 3); SS%(I, 4); SS%(I, 5); SS%(I, 6);
    If SS%(I, 0) = 0 Or SS%(I, 1) = 0 Then Print " .000";: GoTo 2802
    If SS%(I, 0) = SS%(I, 1) Then Print "1.000";: GoTo 2802
    Print Using " .###"; SS%(I, 1) / SS%(I, 0);

    2802 Locate , 53: Print Using "### ## ## ## ### ### "; B0%(I, 0) - SS%(I, 0); B0%(I, 4) - SS%(I, 2); B0%(I, 5) - SS%(I, 3); B0%(I, 6) - SS%(I, 4); B0%(I, 7) - SS%(I, 5); B0%(I, 8) - SS%(I, 6);
    If B0%(I, 0) - SS%(I, 0) = 0 Or B0%(I, 1) - SS%(I, 1) = 0 Then Print " .000";: GoTo 2805
    If B0%(I, 0) - SS%(I, 0) = B0%(I, 1) - SS%(I, 1) Then Print "1.000";: GoTo 2805
    Print Using " .###"; (B0%(I, 1) - SS%(I, 1)) / (B0%(I, 0) - SS%(I, 0));
    2805 If I <> 22 Then Print
2806 Next I
GoSub 8670
8570 Cls: Color L%(11), L%(12): Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
Color 15, 4: Print "PITCHERS    W  L SV    IP    H  BB   SO   ERA  G GS CG RUN  ER ShO HRA  DP %RIP": Color 15
For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "" Then 8575
    Locate I + 3, 1
    Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
    Print Left$(P$(I), 10);
    Locate , 12: Print Using "##"; P0%(I, 6);
    Locate , 15: Print Using "##"; P0%(I, 7);
    Locate , 18: Print Using "##"; P0%(I, 12);
    Locate , 22: Print Using "###"; Int(P0%(I, 0) / 3);
    Print ".";: Print Using "#"; P0%(I, 0) - Int(P0%(I, 0) / 3) * 3;
    Locate , 27: Print Using "####"; P0%(I, 1);
    Locate , 32: Print Using "###"; P0%(I, 4);
    Locate , 36: Print Using "####"; P0%(I, 5);
    Locate , 41
    If P0%(I, 0) = 0 Then Print " -.--";: GoTo 8571
    If P0%(I, 3) = 0 Then Print " 0.00";: GoTo 8571
    Print Using "##.##"; (P0%(I, 3) * 9) / (P0%(I, 0) / 3);
    8571 Locate , 47: Print Using "## ## ## ### ### ## ### ###"; P0%(I, 8); P0%(I, 9); P0%(I, 10); P0%(I, 2); P0%(I, 3); P0%(I, 11); P0%(I, 14); P0%(I, 13);
    If P%(I) > 0 Then Locate , 76: Print Using "###"; Int((((P0%(I, 0) / 3) * 100) / P%(I)) + .5);
    If P%(I) = 0 Then Print "  ";
8575 Next I
GoSub 8670
8585 Cls: Color L%(11), L%(12): Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
Color 15, 4: Print "PITCHERS   BS HL  RI  RS   SUP GS QS  SH SF": Color 15
For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "" Then 8587
    Locate I + 3, 1
    Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
    Print Left$(P$(I), 10);: Locate , 12
    Print Using "## ## ### ### "; P0%(I, 19); P0%(I, 22); P0%(I, 17); P0%(I, 18);
    If P0%(I, 9) > 0 Then Print Using "##.## ## ##"; P0%(I, 20) / P0%(I, 9); P0%(I, 9); P0%(I, 21);: GoTo 8586
    Print " -.-- -- --";
    8586 Print Using "  ## ##"; P0%(I, 15); P0%(I, 16);
8587 Next I
Locate 1, 1
GoSub 8670
8588 Cls: Color L%(11), L%(12): Print A$;: Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%;: Color 14, 0: Print "  TEAM PITCHING TOTALS"
Color 15, 4: Print "  W   L  SV     IP    H   BB   SO   ERA   G  GS  CG  RUN   ER ShO HRA  DP": Color 14, 0
Print Using "### ### ### "; W2%; L2%; T1(12);
Print Using "####"; Int(T1(0) / 3);
Print ".";: Print Using "#"; T1(0) - Int(T1(0) / 3) * 3;
Print Using " #### #### #### ##.## ### ### ### "; T1(1); T1(4); T1(5); T1(3) * 9 / (T1(0) / 3); T1(9); T1(9); T1(10);
Print Using "#### #### ### ### ###"; T1(2); T1(3); T1(11); T1(14); T1(13)
Locate 8, 1
Color 15, 4: Print " BS   RI   RS   SUP  QS  SH  SF": Color 14, 0
Print Using "### #### #### ##.## ### ### ###"; T1(19); T1(17); T1(18); T(2) / T1(9); T1(21); T1(15); T1(16)
GoSub 8670
8625 Print: GoSub 8655: Color 15, 0
Cls
GoSub 5100
U$ = "DISPLAY STATS AGAIN (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
8629 I$=GetKeyPress$
8630 If UCase$(I$) = "Y" Then 8530
8631 GoTo 8800
8650 Cls: Print: Print
Color 14: Print "(0) ";: Color 15: Print "VIEW SCHEDULE AND STATS": Print
Color 14: Print "(1) ";: Color 15: Print "VIEW STATS ONLY": Print
Color 14: Print "(2) ";: Color 15: Print "PRINT SCHEDULE AND STATS": Print
Color 14: Print "(3) ";: Color 15: Print "PRINT STATS ONLY"
8651 I$=GetKeyPress$
8652 X = Val(I$)
8653 GoTo 8680
8655 If X = 2 Then X = 0
8656 Return
8660 If X = 0 Or X = 1 Then Cls
8662 Return
8670 If X = 0 Or X = 1 Then GoSub 4091
8671 Return
8680 If X = 1 Then X = 1
8681 If X = 2 Then X = 2
8682 Return
8800 Cls
GoSub 5100
U$ = "RESET COMPILED STATS TO 0 (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
8801 I$=GetKeyPress$
8802 If UCase$(I$) <> "Y" Then 86
8803 GoSub 5100
U$ = "ARE YOU SURE (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
8804 I$=GetKeyPress$
8806 If UCase$(I$) = "N" Then GoTo 86
8809 If UCase$(I$) = "Y" Then 8812
8812 Kill D$(1) + Z$
Kill D$(1) + Z$ + "S"
If _FileExists(D$(1) + Z$ + "R") Then Kill D$(1) + Z$ + "R"
Cls: GoTo 50
9000 I2 = 0: I3 = 0: I4 = 0
Locate , 38
If B0%(I, 0) = 0 Then Print " .--- ";: GoTo 9001
If B0%(I, 0) = B0%(I, 1) Then Print "1.000 ";: GoTo 9001
If B0%(I, 1) = 0 Then Print " .000 ";: GoTo 9001
Print Using " .### "; B0%(I, 1) / B0%(I, 0);
9001 I3 = B0%(I, 1) + B0%(I, 4) + 2 * B0%(I, 5) + 3 * B0%(I, 6)
If B0%(I, 0) = 0 Then Print " .--- ";: GoTo 9002
If I3 = 0 Then Print " .000 ";: GoTo 9002
If B0%(I, 0) = I3 Then Print "1.000 ";: GoTo 9002
If I3 / B0%(I, 0) >= 1 Then Print Using "#.### "; I3 / B0%(I, 0);: GoTo 9002
Print Using " .### "; I3 / B0%(I, 0);
9002 I4 = B0%(I, 1) + B0%(I, 7): I3 = B0%(I, 7) + B0%(I, 0)
If I3 = 0 Then Print " .---";: GoTo 9004
If I4 = 0 Then Print " .000";: GoTo 9004
If I4 = I3 Then Print "1.000";: GoTo 9004
Print Using " .###"; I4 / I3;
9004 Return
9005 Locate , 38
If P0%(I, 23) = 0 Then Print " .--- ";: GoTo 9006
If P0%(I, 23) = P0%(I, 24) Then Print "1.000 ";: GoTo 9006
If P0%(I, 24) = 0 Then Print " .000 ";: GoTo 9006
Print Using " .### "; P0%(I, 24) / P0%(I, 23);
9006 I3 = P0%(I, 24) + P0%(I, 27) + 2 * P0%(I, 28) + 3 * P0%(I, 29)
If P0%(I, 23) = 0 Then Print " .--- ";: GoTo 9007
If I3 = 0 Then Print " .000 ";: GoTo 9007
If P0%(I, 23) = I3 Then Print "1.000 ";: GoTo 9007
If I3 / P0%(I, 23) >= 1 Then Print Using "#.### "; I3 / P0%(I, 23);: GoTo 9007
Print Using " .### "; I3 / P0%(I, 23);
9007 I4 = P0%(I, 24) + P0%(I, 30): I3 = P0%(I, 30) + P0%(I, 23)
If I3 = 0 Then Print " .---";: GoTo 9008
If I4 = 0 Then Print " .000";: GoTo 9008
If I4 = I3 Then Print "1.000";: GoTo 9008
Print Using " .###"; I4 / I3;
9008 Return
9400 If B%(I) > 0 Then Locate , 73: MM = B0%(I, 0) / B%(I) * 100: MM = Int(MM): Print Str$(MM);
9401 If B%(I) = 0 Then Locate , 73: Print "--";
9402 Return
9450 RQ$ = Right$(Z$, 2): RT = 60: RQ = Val(RQ$)
If RQ > RT Then QT = 162: GoTo 9500
QT = 154
9500 If B%(I) > 0 And (Int((((B%(I) + 0) / QT) * (T1(9) + 1)) + .5) - (B0%(I, 0) + 0)) > 0 Then Locate , 31: Print Using "####"; Int((((B%(I) + 0) / QT) * (T1(9) + 1)) + .5) - (B0%(I, 0) + 0);: GoTo 9505
If B%(I) = 0 Then Print "  ";
If B%(I) > 0 Then Locate , 31: Print Using "####"; Int(((B%(I) / QT) * (T1(9) + 1)) + .5) - (B0%(I, 0) + 0);
9505 If B%(I) > 0 Then Locate , 37: H5% = Int((((B%(I) + 0) / QT) * (T1(9) + 1)) + .5): Print Using "####"; H5%;: T(13) = T(13) + H5%
If B%(I) = 0 Then Locate , 39: Print "0";
Locate , 42: Return
9810 If X = 1 Then Cls: GoTo 9850
Cls: Color 12: Print "        SCHEDULE RESULTS FOR ";: Color L%(11), L%(12): Print A$: Color 15, 0
9811 Color 14: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
9820 Color 15
For I = 1 To GAMES%
    If I = 22 Or I = 43 Or I = 64 Or I = 85 Or I = 106 Or I = 127 Or I = 148 Then GoSub 8670: GoSub 8660: GoSub 9860
    9830 Print Str$(H%(I)); Tab(6); Left$(H$(I), 15); Tab(22); Str$(VS%(I)); Tab(27); "("; PL$(I); ")"; Tab(33); W$(I); Tab(50); L$(I); Tab(66); I%(I)
Next
9851 GoSub 8670: GoSub 8660
9850 Print Tab(20); "RECORD         ("; Str$(W); " -"; Str$(L); " )"
9852 Print Tab(20); "HOME RECORD    ("; Str$(HW); " -"; Str$(HL); " )"
9853 Print Tab(20); "ROAD RECORD    ("; Str$(RW); " -"; Str$(RL); " )"
9854 Print Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
9855 Print Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
Print Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
9856 GoSub 8670: GoSub 8660: Return
9860 Cls: Color 14: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS": Color 15: Return
9910 If X = 3 Then Return Else W = 0: ORW = 0: ORL = 0: L = 0: RW = 0: RL = 0: HW = 0: HL = 0: EW = 0: EL = 0: LTL = 0: LTW = 0: Cls: LPrint "        SCHEDULE RESULTS FOR "; A$: LPrint
9911 LPrint Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
9920 For I = 1 To GAMES%
    9930 LPrint Str$(H%(I)); Tab(6); Left$(H$(I), 15); Tab(22); Str$(VS%(I)); Tab(27); "("; PL$(I); ")"; Tab(33); W$(I); Tab(50); L$(I); Tab(66); I%(I)
Next
9950 LPrint: LPrint
9951 LPrint Tab(20); "RECORD         ("; Str$(W); " -"; Str$(L); " )"
9952 LPrint Tab(20); "HOME RECORD    ("; Str$(HW); " -"; Str$(HL); " )"
9953 LPrint Tab(20); "ROAD RECORD    ("; Str$(RW); " -"; Str$(RL); " )"
9954 LPrint Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
9955 LPrint Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
LPrint Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
9956 LPrint Chr$(12): Return
4500 Locate 10, 8
Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
3805 I$=GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return
4550 Locate 10, 8
Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
3806 I$=GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return
4575 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND ...  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
3807 I$=GetKeyPress$
Call CSMsgBox("", 0, Pass)
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
