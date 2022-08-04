'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Compare Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim K9(1) As Long, Q$(1844), FS%(22, 4, 4), D$(4)
Dim B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21)
Dim B0%(22, 40), P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22)
Dim teamNames$(200), teamIndex%(500), NP$(21), menuItems$(3)
Dim P1%(22), B1%(23), TB%(15), TP(33) As Long
Dim Y As Long
Dim I2 As Single, I3 As Single, I4 As Single, I5 As Single, ERX As Single

Cls

Open "DEV.BAS" For Input As #2
For I = 0 To 3: Input #2, D$(I)
Next
Close #2

10 Call TITLE
menuItems$(1) = "VIEW/PRINT STATISTICAL COMPARISONS"
menuItems$(2) = "RETURN TO UTILITIES MENU          "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 24
    
Call box0(5, 23, 8, 58, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I1 = SELECTION%
If I1 = 2 Then 550

40 Color 15, 0: Cls
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
Erase teamNames$

1441 GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 10

183 Locate 2, 33: Color 15, 0: Color L%(11), L%(12)
Print T$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 40
GoTo 119

1100 NT = 0
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
    teamNames$(NT) = Q$(0): V%(NT) = I1

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

_MouseShow "DEFAULT"
MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then BO% = 1: Return
T = teamIndex%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then GoSub 4500: BO% = 1: Return
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
For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
    For I1 = 0 To 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
Next: Next

For I = 0 To 21: P$(I) = RTrim$(Q$(I * 38 + 1004))
    For I1 = 0 To 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
Next: Next

S$ = RTrim$(Q$(1840)): M$ = RTrim$(Q$(1841)): NN$ = RTrim$(Q$(1842))
CK = CVI(Q$(1843))
Close 1
Return

119 B$ = T$
B$ = RTrim$(B$)
For XX = 4 To 11
    If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14
    If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8
For XX = 11 To 4 Step -1
    If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 'Z$ = LEFT$(Z$, 11)
'Z$ = LEFT$(RIGHT$(B$, LEN(B$) - 3), 8) + "." + LEFT$(B$, 2)
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
GoSub 9000

120 If Right$(Z$, 1) = "_" Then Z$ = Left$(Z$, Len(Z$) - 1): GoTo 120
If Not _FileExists(D$(1) + Z$) Then GoSub 4550: GoTo 10
Open D$(1) + Z$ For Input As #1
Input #1, A$: Input #1, W2%: Input #1, L2%
For I = 0 To 22: Input #1, B$(I): Input #1, B1%(I): Next I
For I = 0 To 21: Input #1, P$(I): Input #1, P1%(I): Next I
For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I
For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I
Input #1, D0%: Input #1, D1%
For I = 0 To 21: Input #1, T(I): Next I
For I = 0 To 22: Input #1, T1(I): Next I
Close #1

170 Color 15, 0: Cls
Locate 12, 5: Color 12: Print "V";: Color 7: Print "IEW OR ";: Color 12: Print "P";: Color 7: Print "RINT"
171 I$ = GetKeyPress$
If UCase$(I$) = "P" Then 600

190 GoSub 9200

200 Color 0, 0: Cls
For I = 0 To 22: If Left$(B$(I), 3) = "XXX" Then 201 Else GoTo 202

    201 B$(I) = ""

202 Next
For I = 0 To 21: If Left$(P$(I), 3) = "XXX" Then 205 Else GoTo 206

    205 P$(I) = ""

206 Next
Color 9: Locate 1, 22: Print "STAT COMPARISONS FOR ";: Color 15: Print T$
For I = 0 To 16
Color 12: Locate I + 4, 10: Print Using "##"; I + 1;: Print ". ";: Color 7: Print Left$(B$(I), 14): Next
For I = 17 To 22
Color 12: Locate I - 13, 30: Print Using "##"; I + 1;: Print ". ";: Color 7: Print Left$(B$(I), 14): Next
For I = 0 To 10
Color 12: Locate I + 10, 30: Print Using "##"; I + 24;: Print ". ";: Color 3: Print Left$(P$(I), 14): Next
For I = 11 To 21
Color 12: Locate I - 7, 50: Print Using "##"; I + 24;: Print ". ";: Color 3: Print Left$(P$(I), 14): Next
Color 12: Locate , 50: Print "46. ";: Color 14: Print "View Players in Sequence"
Color 12: Locate , 50: Print "47. ";: Color 14: Print "View Another Team"
Color 12: Locate , 50: Print "48. ";: Color 14: Print "Exit to DOS"
Color 12: Locate , 50: Print "49. ";: Color 14: Print "Return to UTILITIES menu"

229 Color 15: Locate 23, 30: Input "ENTER # TO VIEW "; B9
If B9 < 1 Or B9 > 49 Then GoTo 229
If B9 >= 1 And B9 <= 23 Then I = B9 - 1: Cls: GoSub 5000: GoTo 265
If B9 >= 24 And B9 <= 45 Then I = B9 - 24: Cls: GoSub 6000: GoTo 400
If B9 = 46 Then BI = 1: Cls: GoTo 260
If B9 = 47 Then GoTo 40
If B9 = 48 Then Cls: End
If B9 = 49 Then Run "HELLO.EXE"

260 GoSub 5000
I = I + 1
For I = 0 To 22
    Locate 2, 30: Color 15, 0: Print String$(40, 177)
    If B$(I) = "XXX" Or B$(I) = "END" Or B$(I) = "" Then 391

    265 If B%(I, 22) = 2 Then PST$ = "Catcher"
    If B%(I, 22) = 3 Then PST$ = "First Base"
    If B%(I, 22) = 4 Then PST$ = "Second Base"
    If B%(I, 22) = 5 Then PST$ = "Third Base"
    If B%(I, 22) = 6 Then PST$ = "Shortstop"
    If B%(I, 22) = 7 Then PST$ = "Left Field"
    If B%(I, 22) = 8 Then PST$ = "Center Field"
    If B%(I, 22) = 9 Then PST$ = "Right Field"
    Color 11: Locate 2, 2: Print T$
    Color 15, 0: Locate 2, 30: Print String$(14, 177): Color 10: Locate 2, 30: Print Left$(B$(I), 14);: Print " - ";: Print PST$
    Color 12: Locate 5, 2: Print "REGULAR "
    Locate 6, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)
    Locate 4, 13: Color 0, 11: Print "  G  ";:
    Print Tab(19); "AB ";:
    Print Tab(24); "R  ";:
    Print Tab(28); "H  ";:
    Print Tab(31); "2B ";:
    Print Tab(35); "3B ";:
    Print Tab(39); "HR ";:
    Print Tab(43); "RBI ";:
    Print Tab(48); "BB ";:
    Print Tab(52); "SO ";:
    Print Tab(56); "SB ";:
    Print Tab(59); "CS ";:
    Print Tab(63); "AVG ";:
    Print Tab(68); "SLG ";:
    Print Tab(73); "OBP";:
    Color 0, 11: Locate 9, 13: Print "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
    Locate 10, 13: Print "  RUNS";:
    Print Tab(21); " HITS";:
    Print Tab(30); " 2B ";:
    Print Tab(37); " 3B ";:
    Print Tab(44); " HR ";:
    Print Tab(50); " RBI ";:
    Print Tab(58); " BB ";:
    Print Tab(65); " SO ";:
    Print Tab(70); " %-SB ";: Color 7, 0: Print String$(2, 177)
    Color 0, 11: Locate 15, 13: Print "FLD.PCT."
    Color 1, 11: Locate 5, 13: Print Using "###"; B%(I, 3);
    If B%(I, 4) = 0 Or B%(I, 5) = 0 Then Locate 11, 13: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 5) > 0 Then Locate 11, 13: Print Using "###.##"; B%(I, 4) / B%(I, 5)
    Locate 5, 16: Print Using "#####"; B%(I, 4)
    If B%(I, 4) = 0 Or B%(I, 6) = 0 Then Locate 11, 19: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 6) > 0 Then Locate 11, 19: Print Using "####.##"; B%(I, 4) / B%(I, 6)
    Locate 5, 21: Print Using "####"; B%(I, 5)
    If B%(I, 4) = 0 Or B%(I, 7) = 0 Then Locate 11, 26: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 7) > 0 Then Locate 11, 26: Print Using "####.##"; B%(I, 4) / B%(I, 7)
    Locate 5, 25: Print Using "####"; B%(I, 6)
    If B%(I, 4) = 0 Or B%(I, 8) = 0 Then Locate 11, 33: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 8) > 0 Then Locate 11, 33: Print Using "####.##"; B%(I, 4) / B%(I, 8)
    Locate 5, 29: Print Using "####"; B%(I, 7)
    If B%(I, 4) = 0 Or B%(I, 9) = 0 Then Locate 11, 40: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 9) > 0 Then Locate 11, 40: Print Using "####.##"; B%(I, 4) / B%(I, 9)
    Locate 5, 33: Print Using "####"; B%(I, 8)
    If B%(I, 4) = 0 Or B%(I, 10) = 0 Then Locate 11, 47: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 10) > 0 Then Locate 11, 47: Print Using "####.##"; B%(I, 4) / B%(I, 10)
    Locate 5, 37: Print Using "####"; B%(I, 9)
    If B%(I, 4) = 0 Or B%(I, 11) = 0 Then Locate 11, 54: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 11) > 0 Then Locate 11, 54: Print Using "####.##"; B%(I, 4) / B%(I, 11)
    Locate 5, 41: Print Using "#####"; B%(I, 10)
    If B%(I, 4) = 0 Or B%(I, 12) = 0 Then Locate 11, 61: Print " ---.--"
    If B%(I, 4) > 0 And B%(I, 12) > 0 Then Locate 11, 61: Print Using "####.##"; B%(I, 4) / B%(I, 12)
    Locate 5, 46: Print Using "####"; B%(I, 11)
    Locate 5, 50: Print Using "####"; B%(I, 12)
    If B%(I, 13) + B%(I, 14) = 0 Or B%(I, 13) = 0 Then Locate 11, 68: Print "    00% ";: Color 7, 0: Print String$(2, 177)
    Color 1, 11: If B%(I, 13) > 0 And B%(I, 14) = 0 Then Locate 11, 68: Print "   100% ";: Color 7, 0: Print String$(2, 177)
    Color 1, 11: If B%(I, 13) > 0 And B%(I, 14) > 0 Then Locate 11, 68: Print "   ";: Print Using "###"; (B%(I, 13) / (B%(I, 13) + B%(I, 14))) * 100;: Print "% ";: Color 7, 0: Print String$(2, 177)
    Color 1, 11: Locate 5, 54: Print Using "####"; B%(I, 13)
    Locate 5, 58: Print Using "###"; B%(I, 14);: Print " "
    I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)
    If B%(I, 4) = 0 Or I2 = 0 Then Locate 5, 62: Print ".000";: Print " "
    If I2 > 0 And I2 < 100 Then Locate 5, 62: Print ".0";: Print Using "##"; I2;: Print " "
    If I2 >= 100 Then Locate 5, 62: Print ".";: Print Using "###"; I2;: Print " "
    If B%(I, 4) = 0 Or I3 = 0 Then Locate 5, 67: Print ".000";: Print " "
    If I3 > 0 And I3 < 100 Then Locate 5, 67: Print ".0";: Print Using "##"; I3;: Print " "
    If I3 >= 100 Then Locate 5, 67: Print ".";: Print Using "###"; I3;: Print " "
    If B%(I, 4) = 0 Or I4 = 0 Then Locate 5, 72: Print ".000";: Color 7, 0: Print String$(2, 177)
    Color 1, 11: If I4 > 0 And I4 < 100 Then Locate 5, 72: Print ".0";: Print Using "##"; I4;: Color 7, 0: Print String$(2, 177)
    Color 1, 11: If I4 >= 100 Then Locate 5, 72: Print ".";: Print Using "###"; I4;: Color 7, 0: Print String$(2, 177)
    Color 7, 0: Locate 7, 15: Print String$(63, 177)
    Color 1, 11: Locate 16, 13: Print "  .";: Print Using "###"; B%(I, 26);: Print "  ";: Color 7, 0: Print String$(2, 177)
    Color 12: Locate 11, 2: Print "REGULAR "
    Locate 12, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 13, 4: Print String$(8, 177)
    Color 12: Locate 16, 2: Print "REGULAR "
    Locate 17, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 18, 4: Print String$(8, 177)
    Color 14, 11: Locate 6, 13: Print Using "###"; B0%(I, 14);
    If B0%(I, 0) = 0 Or B0%(I, 2) = 0 Then Locate 12, 13: Print "---.--"
    If B0%(I, 0) > 0 And B0%(I, 2) > 0 Then Locate 12, 13: Print Using "###.##"; B0%(I, 0) / B0%(I, 2)
    Locate 6, 16: Print Using "#####"; B0%(I, 0)
    If B0%(I, 0) = 0 Or B0%(I, 1) = 0 Then Locate 12, 19: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 1) > 0 Then Locate 12, 19: Print Using "####.##"; B0%(I, 0) / B0%(I, 1)
    Locate 6, 21: Print Using "####"; B0%(I, 2)
    If B0%(I, 0) = 0 Or B0%(I, 4) = 0 Then Locate 12, 26: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 4) > 0 Then Locate 12, 26: Print Using "####.##"; B0%(I, 0) / B0%(I, 4)
    Locate 6, 25: Print Using "####"; B0%(I, 1)
    If B0%(I, 0) = 0 Or B0%(I, 5) = 0 Then Locate 12, 33: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 5) > 0 Then Locate 12, 33: Print Using "####.##"; B0%(I, 0) / B0%(I, 5)
    Locate 6, 29: Print Using "####"; B0%(I, 4)
    If B0%(I, 0) = 0 Or B0%(I, 6) = 0 Then Locate 12, 40: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 6) > 0 Then Locate 12, 40: Print Using "####.##"; B0%(I, 0) / B0%(I, 6)
    Locate 6, 33: Print Using "####"; B0%(I, 5)
    If B0%(I, 0) = 0 Or B0%(I, 3) = 0 Then Locate 12, 47: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 3) > 0 Then Locate 12, 47: Print Using "####.##"; B0%(I, 0) / B0%(I, 3)
    Locate 6, 37: Print Using "####"; B0%(I, 6)
    If B0%(I, 0) = 0 Or B0%(I, 7) = 0 Then Locate 12, 54: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 7) > 0 Then Locate 12, 54: Print Using "####.##"; B0%(I, 0) / B0%(I, 7)
    Locate 6, 41: Print Using "#####"; B0%(I, 3)
    If B0%(I, 0) = 0 Or B0%(I, 8) = 0 Then Locate 12, 61: Print " ---.--"
    If B0%(I, 0) > 0 And B0%(I, 8) > 0 Then Locate 12, 61: Print Using "####.##"; B0%(I, 0) / B0%(I, 8)
    Locate 6, 46: Print Using "####"; B0%(I, 7)
    Locate 6, 50: Print Using "####"; B0%(I, 8)
    If B0%(I, 9) + B0%(I, 10) = 0 Or B0%(I, 9) = 0 Then Locate 12, 68: Print "    00% ";: Color 7, 0: Print String$(2, 177)
    Color 14, 11: If B0%(I, 9) > 0 And B0%(I, 10) = 0 Then Locate 12, 68: Print "   100% ";: Color 7, 0: Print String$(2, 177)
    Color 14, 11: If B0%(I, 9) > 0 And B0%(I, 10) > 0 Then Locate 12, 68: Print "   ";: Print Using "###"; (B0%(I, 9) / (B0%(I, 10) + B0%(I, 9))) * 100;: Print "% ";: Color 7, 0: Print String$(2, 177)
    Color 14, 11: Locate 6, 54: Print Using "####"; B0%(I, 9)
    Locate 6, 58: Print Using "###"; B0%(I, 10);: Print " "
    I2 = 0: I3 = 0: I4 = 0: IF B0%(I, 0) > 0 THEN I2 = INT((B0%(I, 1) / B0%(I, 0) + .0005) * 1000): I3 = INT(((B0%(I, 1) + B0%(I, 4) + 2 * B0%(I, 5) + 3 * B0%(I, 6)) / B0%(I, 0) + .0005) * 1000): I4 = INT(((B0%(I, 1) + B0%(I, 7)) / (B0%(I, 7) + B0%( _
I, 0)) + .0005) * 1000)
    If B0%(I, 0) = 0 Or I2 = 0 Then Locate 6, 62: Print ".000";: Print " "
    If I2 > 0 And I2 < 100 Then Locate 6, 62: Print ".0";: Print Using "##"; I2;: Print " "
    If I2 >= 100 Then Locate 6, 62: Print ".";: Print Using "###"; I2;: Print " "
    If B0%(I, 0) = 0 Or I3 = 0 Then Locate 6, 67: Print ".000";: Print " "
    If I3 > 0 And I3 < 100 Then Locate 6, 67: Print ".0";: Print Using "##"; I3;: Print " "
    If I3 >= 100 Then Locate 6, 67: Print ".";: Print Using "###"; I3;: Print " "
    If B0%(I, 0) = 0 Or I4 = 0 Then Locate 6, 72: Print ".000";: Color 7, 0: Print String$(2, 177)
    Color 14, 11: If I4 > 0 And I4 < 100 Then Locate 6, 72: Print ".0";: Print Using "##"; I4;: Color 7, 0: Print String$(2, 177)
    Color 14, 11: If I4 >= 100 Then Locate 6, 72: Print ".";: Print Using "###"; I4;: Color 7, 0: Print String$(2, 177)
    Color 7, 0: Locate 13, 15: Print String$(63, 177)
    If B0%(I, 16) = 0 And B0%(I, 17) = 0 Then I5 = 0 Else I5 = ((B0%(I, 16) + B0%(I, 17)) / (B0%(I, 16) + B0%(I, 17) + B0%(I, 11))) * 1000
    Color 14, 11: If I5 = 0 Then Locate 17, 13: Print "  .000  ";: Color 7, 0: Print String$(2, 177)
    Color 14, 11: If I5 > 0 And I5 < 1000 Then Locate 17, 13: Print "  .";: Print Using "###"; I5;: Print "  ";: Color 7, 0: Print String$(2, 177)
    Color 14, 11: If I5 = 1000 Then Locate 17, 13: Print Using "##"; I5 / 1000;: Print ".000  ";: Color 7, 0: Print String$(2, 177)
    Color 7, 0: Locate 18, 15: Print String$(8, 177)
    Color 15, 0: Locate 23, 4: Print "HIT ANY KEY TO CONTINUE"
    If BI = 1 Then 380 Else GoTo 381

    380 Locate 23, 28: Print "OR ESC TO RETURN TO MENU"
    381 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 391
    If I$ = Chr$(27) Then 388
    If BI = 0 Then 387
    If BI = 1 Then 391
    If I < 17 Then 391 Else GoTo 395

    387 Cls: GoTo 200

    388 BI = 0: GoTo 200
    If BI = 1 Then 391 Else GoTo 395

391 Next
If BI = 1 Then GoTo 395

395 Cls: GoSub 6000
For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "END" Or P$(I) = "" Then 508

    400 Color 11: Locate 2, 2: Print T$
    Color 15, 0: Locate 2, 30: Print String$(14, 177): Color 13: Locate 2, 30: Print Left$(P$(I), 14)
    Color 10: Locate 5, 2: Print "REGULAR "
    Locate 6, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)
    Color 0, 12: Locate 4, 14: Print "  G ";:
    Print Tab(20); " W ";:
    Print Tab(25); " L ";:
    Print Tab(30); " SV ";:
    Print Tab(37); " IP ";:
    Print Tab(43); " H  ";:
    Print Tab(48); " BB ";:
    Print Tab(53); " SO ";:
    Print Tab(58); " ERA ";:
    Print Tab(63); " GS";:
    Print Tab(66); " CG"
    Color 10, 0: Locate 10, 2: Print "REGULAR "
    Locate 11, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)
    Color 0, 12: Locate 9, 14: Print " H/9IP ";:
    Print Tab(21); " SO/9IP ";:
    Print Tab(29); " BB/9IP ";:
    Print Tab(37); " HR ALL";:
    Color 14, 12: Locate 5, 14: Print Using "###"; P%(I, 4);:
    Locate , 17: Print Using "#####"; P%(I, 1);:
    Locate , 22: Print Using "#####"; P%(I, 2);:
    Locate , 27: Print Using "######"; P%(I, 3);:
    Locate , 33: Print Using "#####.#"; P%(I, 6);:
    Locate , 40: Print Using "#####"; P%(I, 7);:
    Locate , 45: Print Using "######"; P%(I, 8);:
    Locate , 51: Print Using "#####"; P%(I, 9);:
    Locate , 56: Print Using "###.##"; (P%(I, 10)) / 100;:
    Locate , 62: Print Using "####"; P%(I, 5);:
    Locate , 66: Print Using "###"; P%(I, 16);: Color 7, 0: Print String$(2, 177)
    Color 14, 12: If P%(I, 6) = 0 Then Locate 10, 14: Print "  0.00";
    If P%(I, 6) > 0 Then Locate 10, 14: Print Using "###.##"; (P%(I, 7) * 9) / P%(I, 6)
    If P%(I, 6) = 0 Then Locate 10, 20: Print "    0.00";
    If P%(I, 6) > 0 Then Locate 10, 20: Print Using "#####.##"; (P%(I, 9) * 9) / P%(I, 6)
    If P%(I, 6) = 0 Then Locate 10, 28: Print "    0.00";
    If P%(I, 6) > 0 Then Locate 10, 28: Print Using "#####.##"; (P%(I, 8) * 9) / P%(I, 6)
    Locate 10, 36: Print Using "########"; P%(I, 13);: Color 7, 0: Print String$(2, 177)
    Color 15, 12: Locate 6, 14: Print Using "###"; P0%(I, 8);:
    Locate , 17: Print Using "#####"; P0%(I, 6);:
    Locate , 22: Print Using "#####"; P0%(I, 7);:
    Locate , 27: Print Using "######"; P0%(I, 12);:
    Locate , 33: Print Using "#####"; Int(P0%(I, 0) / 3);
    If P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3) = 0 Then Print ".0";
    If P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3);
    Locate , 40: Print Using "#####"; P0%(I, 1);:
    Locate , 45: Print Using "######"; P0%(I, 4);:
    Locate , 51: Print Using "#####"; P0%(I, 5);:
    I2 = 0
    If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 3) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then Locate 6, 56: Print "  0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 6, 56: Print Using "###"; I3;: Print ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then Locate 6, 56: Print Using "###"; I3;: Print ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;
    Locate 6, 62: Print Using "####"; P0%(I, 9);
    Locate , 66: Print Using "###"; P0%(I, 10);: Color 7, 0: Print String$(2, 177)
    Locate 7, 16: Print String$(55, 177)
    Color 15, 12: I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 1) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 14: Print "  0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 14: Print Using "###"; I3;: Print ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 14: Print Using "###"; I3;: Print ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;
    I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 5) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 20: Print "    0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 20: Print Using "#####"; I3;: Print ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 20: Print Using "#####"; I3;: Print ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;
    I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 4) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 28: Print "    0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 28: Print Using "#####"; I3;: Print ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 28: Print Using "#####"; I3;: Print ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;
    Locate 11, 36: Print Using "########"; P0%(I, 14);: Color 7, 0: Print String$(2, 177)
    Locate 12, 16: Print String$(30, 177)
    Color 15, 0: Locate 23, 4: Print "HIT ANY KEY TO CONTINUE"
    If BI = 1 Then 503 Else GoTo 504

    503 Locate 23, 28: Print "OR ESC TO RETURN TO MENU"
    504 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 508
    If I$ = Chr$(27) Then 510
    If BI = 1 Then 508 Else GoTo 510

508 Next
If BI = 1 Then GoTo 510

510 BI = 0: GoTo 200

550 Run "HELLO.EXE"

600 LPrint Chr$(15)
'    WIDTH "LPT1:1", 140
LPrint "STAT COMPARISONS FOR "; T$
LPrint: LPrint:
LPrint "BATTER";
LPrint Tab(21); "  G";
LPrint Tab(26); " AB";
LPrint Tab(31); "  R";
LPrint Tab(36); "  H";
LPrint Tab(41); " 2B";
LPrint Tab(45); " 3B";
LPrint Tab(49); " HR";
LPrint Tab(54); "RBI";
LPrint Tab(59); " BB";
LPrint Tab(64); " SO";
LPrint Tab(69); " SB";
LPrint Tab(74); " CS";
LPrint Tab(79); "AVG";
LPrint Tab(85); "SLG";
LPrint Tab(91); "OBP";
LPrint Tab(97); "FLD.PCT"
For I = 0 To 22
    If B$(I) = "XXX" Or B$(I) = "END" Or B$(I) = "" Then 735
    LPrint Left$(B$(I), 14);
    LPrint Tab(16); "Reg.";
    LPrint Tab(21); Using "###"; B%(I, 3);
    LPrint Tab(26); Using "###"; B%(I, 4);
    LPrint Tab(31); Using "###"; B%(I, 5);
    LPrint Tab(36); Using "###"; B%(I, 6);
    LPrint Tab(42); Using "##"; B%(I, 7);
    LPrint Tab(46); Using "##"; B%(I, 8);
    LPrint Tab(50); Using "##"; B%(I, 9);
    LPrint Tab(54); Using "###"; B%(I, 10);
    LPrint Tab(59); Using "###"; B%(I, 11);
    LPrint Tab(64); Using "###"; B%(I, 12);
    LPrint Tab(69); Using "###"; B%(I, 13);
    LPrint Tab(74); Using "###"; B%(I, 14);
    I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)
    If B%(I, 4) = 0 Or I2 = 0 Then LPrint Tab(79); ".000";
    If I2 > 0 And I2 < 100 Then LPrint Tab(79); ".0";: LPrint Using "##"; I2;
    If I2 >= 100 Then LPrint Tab(79); ".";: LPrint Using "###"; I2;
    If B%(I, 4) = 0 Or I3 = 0 Then LPrint Tab(84); ".000";
    If I3 > 0 And I3 < 100 Then LPrint Tab(84); ".0";: LPrint Using "##"; I3;
    If I3 >= 100 Then LPrint Tab(84); ".";: LPrint Using "###"; I3;
    If B%(I, 4) = 0 Or I4 = 0 Then LPrint Tab(90); ".000";
    If I4 > 0 And I4 < 100 Then LPrint Tab(90); ".0";: LPrint Using "##"; I4;
    If I4 >= 100 Then LPrint Tab(90); ".";: LPrint Using "###"; I4;
    LPrint Tab(99); ".";: LPrint Using "###"; B%(I, 26)
    LPrint Tab(16); "Lge.";
    LPrint Tab(21); Using "###"; B0%(I, 14);
    LPrint Tab(26); Using "###"; B0%(I, 0);
    LPrint Tab(31); Using "###"; B0%(I, 2);
    LPrint Tab(36); Using "###"; B0%(I, 1);
    LPrint Tab(42); Using "##"; B0%(I, 4);
    LPrint Tab(46); Using "##"; B0%(I, 5);
    LPrint Tab(50); Using "##"; B0%(I, 6);
    LPrint Tab(54); Using "###"; B0%(I, 3);
    LPrint Tab(59); Using "###"; B0%(I, 7);
    LPrint Tab(64); Using "###"; B0%(I, 8);
    LPrint Tab(69); Using "###"; B0%(I, 9);
    LPrint Tab(74); Using "###"; B0%(I, 10);
    I2 = 0: I3 = 0: I4 = 0: IF B0%(I, 0) > 0 THEN I2 = INT((B0%(I, 1) / B0%(I, 0) + .0005) * 1000): I3 = INT(((B0%(I, 1) + B0%(I, 4) + 2 * B0%(I, 5) + 3 * B0%(I, 6)) / B0%(I, 0) + .0005) * 1000): I4 = INT(((B0%(I, 1) + B0%(I, 7)) / (B0%(I, 7) + B0%( _
I, 0)) + .0005) * 1000)
    If B0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(79); ".000";
    If I2 > 0 And I2 < 100 Then LPrint Tab(79); ".0";: LPrint Using "##"; I2;
    If I2 >= 100 Then LPrint Tab(79); ".";: LPrint Using "###"; I2;
    If B0%(I, 0) = 0 Or I3 = 0 Then LPrint Tab(84); ".000";
    If I3 > 0 And I3 < 100 Then LPrint Tab(84); ".0";: LPrint Using "##"; I3;
    If I3 >= 100 Then LPrint Tab(84); ".";: LPrint Using "###"; I3;
    If B0%(I, 0) = 0 Or I4 = 0 Then LPrint Tab(90); ".000";
    If I4 > 0 And I4 < 100 Then LPrint Tab(90); ".0";: LPrint Using "##"; I4;
    If I4 >= 100 Then LPrint Tab(90); ".";: LPrint Using "###"; I4;
    If B0%(I, 16) = 0 And B0%(I, 17) = 0 Then I5 = 0 Else I5 = ((B0%(I, 16) + B0%(I, 17)) / (B0%(I, 16) + B0%(I, 17) + B0%(I, 11))) * 1000
    If I5 = 0 Then LPrint Tab(99); ".000  "
    If I5 > 0 And I5 < 1000 Then LPrint Tab(99); ".";: LPrint Using "###"; I5
    If I5 = 1000 Then LPrint Tab(98); Using "#"; I5 / 1000;: LPrint ".000  "
    LPrint

735 Next I
GoSub 7000
LPrint Chr$(12)
LPrint "STAT COMPARISONS FOR "; T$
LPrint: LPrint:
LPrint "PITCHER";
LPrint Tab(25); "  G";
LPrint Tab(30); "  W";
LPrint Tab(35); "  L";
LPrint Tab(40); " SV";
LPrint Tab(47); " IP";
LPrint Tab(52); "  H";
LPrint Tab(56); " BB";
LPrint Tab(61); " SO";
LPrint Tab(67); "ERA";
LPrint Tab(72); " GS";
LPrint Tab(78); " CG";
LPrint Tab(85); "H/9IP";
LPrint Tab(92); "SO/9IP";
LPrint Tab(99); "BB/9IP";
LPrint Tab(107); "HR ALL."
For I = 0 To 21
    If P$(I) = "XXX" Or P$(I) = "END" Or P$(I) = "" Then 983
    LPrint Left$(P$(I), 14);
    LPrint Tab(18); "REG.";
    LPrint Tab(25); Using "###"; P%(I, 4);
    LPrint Tab(31); Using "##"; P%(I, 1);
    LPrint Tab(36); Using "##"; P%(I, 2);
    LPrint Tab(41); Using "##"; P%(I, 3);
    LPrint Tab(45); Using "###.#"; P%(I, 6);
    LPrint Tab(52); Using "###"; P%(I, 7);
    LPrint Tab(56); Using "###"; P%(I, 8);
    LPrint Tab(61); Using "###"; P%(I, 9);
    LPrint Tab(66); Using "##.##"; (P%(I, 10)) / 100;
    LPrint Tab(73); Using "##"; P%(I, 5);
    LPrint Tab(79); Using "##"; P%(I, 16);
    If P%(I, 6) = 0 Then LPrint Tab(85); " 0.00";
    If P%(I, 6) > 0 Then LPrint Tab(85); Using "##.##"; (P%(I, 7) * 9) / P%(I, 6);
    If P%(I, 6) = 0 Then LPrint Tab(93); " 0.00";
    If P%(I, 6) > 0 Then LPrint Tab(93); Using "##.##"; (P%(I, 9) * 9) / P%(I, 6);
    If P%(I, 6) = 0 Then LPrint Tab(100); " 0.00";
    If P%(I, 6) > 0 Then LPrint Tab(100); Using "##.##"; (P%(I, 8) * 9) / P%(I, 6);
    LPrint Tab(109); Using "##"; P%(I, 13)
    LPrint Tab(18); "LGE.";
    LPrint Tab(25); Using "###"; P0%(I, 8);
    LPrint Tab(31); Using "##"; P0%(I, 6);
    LPrint Tab(36); Using "##"; P0%(I, 7);
    LPrint Tab(41); Using "##"; P0%(I, 12);
    LPrint Tab(45); Using "###"; Int(P0%(I, 0) / 3);
    If P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; P0%(I, 0) - (Int(P0%(I, 0) / 3) * 3);
    LPrint Tab(52); Using "###"; P0%(I, 1);
    LPrint Tab(56); Using "###"; P0%(I, 4);
    LPrint Tab(61); Using "###"; P0%(I, 5);
    I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 3) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(66); " 0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(66); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(66); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;
    LPrint Tab(73); Using "##"; P0%(I, 9);
    LPrint Tab(79); Using "##"; P0%(I, 10);
    I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 1) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(85); " 0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(85); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(85); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;
    I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 5) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(93); " 0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(93); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(93); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;
    I2 = 0: If P0%(I, 0) > 0 Then I2 = Int((((P0%(I, 4) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100): I4 = I2 - (I3 * 100)
    If P0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(100); " 0.00";
    If P0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(100); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(100); Using "##"; I3;: LPrint ".";
    If P0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
    If P0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
    If P0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;
    LPrint Tab(109); Using "##"; P0%(I, 14)
    LPrint

983 Next
LPrint Chr$(18)
'    WIDTH "LPT1:1", 80

1000 GoTo 10

5000 For dc = 1 To 20
    Color 15, 0: Locate dc, 1
    Print String$(79, 177)
Next
Return

6000 For dc = 1 To 14
    Color 15, 0: Locate dc, 1
    Print String$(78, 177)
Next
Return

7000 LPrint Chr$(12)
LPrint "STAT COMPARISONS FOR "; T$
LPrint: LPrint:
LPrint Tab(20); " [ - - - - - - - - - - AT BATS PER - - - - - - - - - - - ]"
LPrint "BATTER";
LPrint Tab(26); "R";
LPrint Tab(33); "H";
LPrint Tab(40); "2B";
LPrint Tab(47); "3B";
LPrint Tab(54); "HR";
LPrint Tab(61); "RBI";
LPrint Tab(68); "BB";
LPrint Tab(75); "SO";
LPrint Tab(79); "SUCCESS % - SB"
For I = 0 To 16
    If B$(I) = "XXX" Or B$(I) = "END" Or B$(I) = "" Then 7725
    LPrint Left$(B$(I), 14);
    LPrint Tab(16); "Reg.";
    If B%(I, 4) = 0 Or B%(I, 5) = 0 Then LPrint Tab(21); "---.--";
    If B%(I, 4) > 0 And B%(I, 5) > 0 Then LPrint Tab(21); Using "###.##"; B%(I, 4) / B%(I, 5);
    If B%(I, 4) = 0 Or B%(I, 6) = 0 Then LPrint Tab(28); "---.--";
    If B%(I, 4) > 0 And B%(I, 6) > 0 Then LPrint Tab(28); Using "###.##"; B%(I, 4) / B%(I, 6);
    If B%(I, 4) = 0 Or B%(I, 7) = 0 Then LPrint Tab(36); "---.--";
    If B%(I, 4) > 0 And B%(I, 7) > 0 Then LPrint Tab(36); Using "###.##"; B%(I, 4) / B%(I, 7);
    If B%(I, 4) = 0 Or B%(I, 8) = 0 Then LPrint Tab(43); "---.--";
    If B%(I, 4) > 0 And B%(I, 8) > 0 Then LPrint Tab(43); Using "###.##"; B%(I, 4) / B%(I, 8);
    If B%(I, 4) = 0 Or B%(I, 9) = 0 Then LPrint Tab(50); "---.--";
    If B%(I, 4) > 0 And B%(I, 9) > 0 Then LPrint Tab(50); Using "###.##"; B%(I, 4) / B%(I, 9);
    If B%(I, 4) = 0 Or B%(I, 10) = 0 Then LPrint Tab(58); "---.--";
    If B%(I, 4) > 0 And B%(I, 10) > 0 Then LPrint Tab(58); Using "###.##"; B%(I, 4) / B%(I, 10);
    If B%(I, 4) = 0 Or B%(I, 11) = 0 Then LPrint Tab(64); "---.--";
    If B%(I, 4) > 0 And B%(I, 11) > 0 Then LPrint Tab(64); Using "###.##"; B%(I, 4) / B%(I, 11);
    If B%(I, 4) = 0 Or B%(I, 12) = 0 Then LPrint Tab(71); "---.--";
    If B%(I, 4) > 0 And B%(I, 12) > 0 Then LPrint Tab(71); Using "###.##"; B%(I, 4) / B%(I, 12);
    If B%(I, 13) + B%(I, 14) = 0 Or B%(I, 13) = 0 Then LPrint Tab(83); " 00%";
    If B%(I, 13) > 0 And B%(I, 14) = 0 Then LPrint Tab(83); "100%";
    If B%(I, 13) > 0 And B%(I, 14) > 0 Then LPrint Tab(83); Using "###"; (B%(I, 13) / (B%(I, 13) + B%(I, 14))) * 100;: LPrint Tab(86); "%"
    LPrint Tab(16); "Leg.";
    If B0%(I, 0) = 0 Or B0%(I, 2) = 0 Then LPrint Tab(21); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 2) > 0 Then LPrint Tab(21); Using "###.##"; B0%(I, 0) / B0%(I, 2);
    If B0%(I, 0) = 0 Or B0%(I, 1) = 0 Then LPrint Tab(28); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 1) > 0 Then LPrint Tab(28); Using "###.##"; B0%(I, 0) / B0%(I, 1);
    If B0%(I, 0) = 0 Or B0%(I, 4) = 0 Then LPrint Tab(36); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 4) > 0 Then LPrint Tab(36); Using "###.##"; B0%(I, 0) / B0%(I, 4);
    If B0%(I, 0) = 0 Or B0%(I, 5) = 0 Then LPrint Tab(43); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 5) > 0 Then LPrint Tab(43); Using "###.##"; B0%(I, 0) / B0%(I, 5);
    If B0%(I, 0) = 0 Or B0%(I, 6) = 0 Then LPrint Tab(50); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 6) > 0 Then LPrint Tab(50); Using "###.##"; B0%(I, 0) / B0%(I, 6);
    If B0%(I, 0) = 0 Or B0%(I, 3) = 0 Then LPrint Tab(58); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 3) > 0 Then LPrint Tab(58); Using "###.##"; B0%(I, 0) / B0%(I, 3);
    If B0%(I, 0) = 0 Or B0%(I, 7) = 0 Then LPrint Tab(64); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 7) > 0 Then LPrint Tab(64); Using "###.##"; B0%(I, 0) / B0%(I, 7);
    If B0%(I, 0) = 0 Or B0%(I, 8) = 0 Then LPrint Tab(71); "---.--";
    If B0%(I, 0) > 0 And B0%(I, 8) > 0 Then LPrint Tab(71); Using "###.##"; B0%(I, 0) / B0%(I, 8);
    If B0%(I, 9) + B0%(I, 10) = 0 Or B0%(I, 9) = 0 Then LPrint Tab(83); " 00%"
    If B0%(I, 9) > 0 And B0%(I, 10) = 0 Then LPrint Tab(83); "100%"
    If B0%(I, 9) > 0 And B0%(I, 10) > 0 Then LPrint Tab(83); Using "###"; (B0%(I, 9) / (B0%(I, 10) + B0%(I, 9))) * 100;: LPrint Tab(86); "%"

7725 Next
Return

8000 Locate 13, 20: Color 14: Print "ERROR::: INVALID TEAM # OR STAT FILE NAME!!"
Locate 15, 20: Line Input "HIT ENTER TO RE-ENTER"; DUMMY$
For J = 5 To 15
    Locate J
    Print "                                                                         "
Next
Return

9000 For K = 4 To 18: For I = 0 To 22
    TB%(K - 3) = TB%(K - 3) + B%(I, K)
Next: Next
For K = 1 To 33
    For I = 0 To 21
        TP(K) = TP(K) + P%(I, K)
Next: Next
ERX = 0: For I = 0 To 21
    ERX = ERX + ((P%(I, 6) / 9) * (P%(I, 10) / 100))
Next
For K = 4 To 14: For I = 0 To 21
    If P%(I, 11) = 999 Then TB%(K - 3) = TB%(K - 3) + P%(I, K + 16)
Next: Next
Return

9200 Cls: GoSub 9700: Locate 1, 35: Color L%(11), L%(12): Print T$
Color 10, 0: Locate 3, 2: Print "BATTING "
Color 12: Locate 4, 2: Print "REGULAR ";: Color 7, 0: Print String$(2, 177)
Locate 5, 2: Color 12: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
Locate 6, 4: Print String$(8, 177)
Locate 3, 13: Color 0, 11: Print "  AB";:
Print Tab(18); "  R";:
Print Tab(23); "  H";:
Print Tab(27); " 2B";:
Print Tab(31); " 3B";:
Print Tab(35); " HR";:
Print Tab(39); " RBI";:
Print Tab(43); " BB";:
Print Tab(49); " SO";:
Print Tab(53); " SB";:
Print Tab(57); " CS";:
Print Tab(61); " AVG";:
Print Tab(66); " SLG";:
Print Tab(72); "OBP";:
Color 10, 0: Locate 9, 2: Print "BATTING "
Color 12, 0: Locate 10, 2: Print "REGULAR ";: Color 7, 0: Print String$(2, 177)
Locate 11, 2: Color 12: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
Locate 12, 4: Print String$(8, 177)
Color 0, 11: Locate 8, 13: Print "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
Locate 9, 13: Print "  RUNS";:
Print Tab(21); " HITS";:
Print Tab(30); " 2B ";:
Print Tab(37); " 3B ";:
Print Tab(44); " HR ";:
Print Tab(50); " RBI ";:
Print Tab(58); " BB ";:
Print Tab(65); " SO ";:
Print Tab(70); " %-SB ";: Color 7, 0: Print String$(2, 177)
Color 1, 11: Locate 4, 13: Print Using "####"; TB%(1);
Locate , 17: Print Using "####"; TB%(2);
Locate , 21: Print Using "#####"; TB%(3);
Locate , 26: Print Using "####"; TB%(4);
Locate , 30: Print Using "####"; TB%(5);
Locate , 34: Print Using "####"; TB%(6);
Locate , 38: Print Using "#####"; TB%(7);
Locate , 43: Print Using "####"; TB%(8);
Locate , 47: Print Using "#####"; TB%(9);
Locate , 52: Print Using "####"; TB%(10);
Locate , 56: Print Using "####"; TB%(11);: Print " ";
Locate , 61: Print Using ".###"; TB%(3) / TB%(1);: Print " ";
Locate , 66: Print Using ".###"; (TB%(3) + TB%(4) + TB%(5) * 2 + TB%(6) * 3) / TB%(1);: Print " ";
Locate , 71: Print Using ".###"; (TB%(3) + TB%(8)) / (TB%(1) + TB%(8));: Color 7, 0: Print String$(2, 177)
Color 14, 11: Locate 5, 13: Print Using "####"; T(0);
Locate , 17: Print Using "####"; T(2);
Locate , 21: Print Using "#####"; T(1);
Locate , 26: Print Using "####"; T(4);
Locate , 30: Print Using "####"; T(5);
Locate , 34: Print Using "####"; T(6);
Locate , 38: Print Using "#####"; T(3);
Locate , 43: Print Using "####"; T(7);
Locate , 47: Print Using "#####"; T(8);
Locate , 52: Print Using "####"; T(9);
Locate , 56: Print Using "####"; T(10);: Print " ";
Locate , 61: Print Using ".###"; T(1) / T(0);: Print " ";
Locate , 66: Print Using ".###"; (T(1) + T(4) + T(5) * 2 + T(6) * 3) / T(0);: Print " ";
Locate , 71: Print Using ".###"; (T(1) + T(7)) / (T(0) + T(7));: Color 7, 0: Print String$(2, 177)
Locate , 15: Print String$(62, 177)
Color 1, 11
If TB%(2) = 0 Then Locate 10, 13: Print " ---.--";
If TB%(2) > 0 Then Locate 10, 13: Print Using "###.##"; TB%(1) / TB%(2);
If TB%(3) = 0 Then Locate , 18: Print " ---.--";
If TB%(3) > 0 Then Locate , 18: Print Using "#####.##"; TB%(1) / TB%(3);
If TB%(4) = 0 Then Locate , 26: Print " ---.--";
If TB%(4) > 0 Then Locate , 26: Print Using "####.##"; TB%(1) / TB%(4);
If TB%(5) = 0 Then Locate , 33: Print " ---.--";
If TB%(5) > 0 Then Locate , 33: Print Using "####.##"; TB%(1) / TB%(5);
If TB%(6) = 0 Then Locate , 40: Print " ---.--";
If TB%(6) > 0 Then Locate , 40: Print Using "####.##"; TB%(1) / TB%(6);
If TB%(7) = 0 Then Locate , 47: Print " ---.--";
If TB%(7) > 0 Then Locate , 47: Print Using "####.##"; TB%(1) / TB%(7);
If TB%(8) = 0 Then Locate , 54: Print " ---.--";
If TB%(8) > 0 Then Locate , 54: Print Using "####.##"; TB%(1) / TB%(8);
If TB%(9) = 0 Then Locate , 61: Print " ---.--";
If TB%(9) > 0 Then Locate , 61: Print Using "####.##"; TB%(1) / TB%(9);
If TB%(10) = 0 Then Locate , 68: Print " ---.--";
If TB%(10) > 0 Then Locate , 68: Print Using "####.##"; TB%(10) / (TB%(10) + TB%(11));: Print " ";: Color 7, 0: Print String$(2, 177)
Color 14, 11
If T(2) = 0 Then Locate 11, 13: Print " ---.--";
If T(2) > 0 Then Locate 11, 13: Print Using "###.##"; T(0) / T(2);
If T(1) = 0 Then Locate , 18: Print " ---.--";
If T(1) > 0 Then Locate , 18: Print Using "#####.##"; T(0) / T(1);
If T(4) = 0 Then Locate , 26: Print " ---.--";
If T(4) > 0 Then Locate , 26: Print Using "####.##"; T(0) / T(4);
If T(5) = 0 Then Locate , 33: Print " ---.--";
If T(5) > 0 Then Locate , 33: Print Using "####.##"; T(0) / T(5);
If T(6) = 0 Then Locate , 40: Print " ---.--";
If T(6) > 0 Then Locate , 40: Print Using "####.##"; T(0) / T(6);
If T(3) = 0 Then Locate , 47: Print " ---.--";
If T(3) > 0 Then Locate , 47: Print Using "####.##"; T(0) / T(3);
If T(7) = 0 Then Locate , 54: Print " ---.--";
If T(7) > 0 Then Locate , 54: Print Using "####.##"; T(0) / T(7);
If T(8) = 0 Then Locate , 61: Print " ---.--";
If T(8) > 0 Then Locate , 61: Print Using "####.##"; T(0) / T(8);
If T(9) = 0 Then Locate , 68: Print " ---.--";
If T(9) > 0 Then Locate , 68: Print Using "####.##"; T(9) / (T(9) + T(10));: Print " ";: Color 7, 0: Print String$(2, 177)
Locate , 15: Print String$(63, 177)
Color 10, 0: Locate 14, 2: Print "PITCHING"
Color 12, 0: Locate 15, 2: Print "REGULAR ";: Color 7, 0: Print String$(2, 177)
Locate 16, 2: Color 12: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
Locate 17, 4: Print String$(8, 177)
Color 0, 12: Locate 14, 14: Print "  SV";
Print Tab(20); " IP";
Print Tab(25); "  H";
Print Tab(30); "  BB";
Print Tab(37); "  SO";
Print Tab(43); " HRA";
Print Tab(48); "  CG";
Print Tab(53); "  ERA";
Color 10, 0: Locate 19, 2: Print "PITCHING"
Color 12, 0: Locate 20, 2: Print "REGULAR ";: Color 7, 0: Print String$(2, 177)
Locate 21, 2: Color 12: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
Locate 22, 4: Print String$(8, 177)
Color 0, 12: Locate 19, 14: Print " H/9IP ";
Print Tab(21); " SO/9IP ";
Print Tab(29); " BB/9IP";
Color 15, 12: Locate 15, 14: Print Using "####"; TP(3);
Locate , 18: Print Using "#####"; TP(6);
Locate , 23: Print Using "#####"; TP(7);
Locate , 28: Print Using "######"; TP(8);
Locate , 34: Print Using "#######"; TP(9);
Locate , 41: Print Using "######"; TP(13);
Locate , 47: Print Using "#####"; TP(16);
Locate , 52: Print Using "###.##"; (ERX * 9) / TP(6);: Color 7, 0: Print String$(2, 177)
Color 15, 12: Locate 20, 14: Print Using "###.##"; TP(7) * 9 / TP(6);
Locate , 20: Print Using "#####.##"; TP(9) * 9 / TP(6);
Locate , 28: Print Using "#####.##"; TP(8) * 9 / TP(6);: Color 7, 0: Print String$(2, 177)
Color 14, 12: Locate 16, 14: Print Using "####"; T1(12);
Locate , 18: Print Using "#####"; T1(0) / 3;
Locate , 23: Print Using "#####"; T1(1);
Locate , 28: Print Using "######"; T1(4);
Locate , 34: Print Using "#######"; T1(5);
Locate , 41: Print Using "######"; T1(14);
Locate , 47: Print Using "#####"; T1(10);
Locate , 52: Print Using "###.##"; (T1(3) * 9) / (T1(0) / 3);: Color 7, 0: Print String$(2, 177)
Locate , 16: Color 7, 0: Print String$(44, 177)
Color 14, 12: Locate 21, 14: Print Using "###.##"; T1(1) * 9 / (T1(0) / 3);
Locate , 20: Print Using "#####.##"; T1(5) * 9 / (T1(0) / 3);
Locate , 28: Print Using "#####.##"; T1(4) * 9 / (T1(0) / 3);: Color 7, 0: Print String$(2, 177)
Locate , 16: Color 7, 0: Print String$(22, 177)
Color 15, 0: Locate 22, 48: Print "HIT ANY KEY TO CONTINUE"
For I = 1 To 15: TB%(I) = 0: Next
For I = 1 To 33: TP(I) = 0: Next
9500 I$ = GetKeyPress$
Return

9700 For dc = 1 To 23
    Color 15, 0: Locate dc, 1
    Print String$(79, 177)
Next
Return

4500 Locate 10, 8
Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4550 Locate 10, 8
Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
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
