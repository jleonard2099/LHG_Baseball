'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Trade Players"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim T(1), A$(1), L%(1, 14), B$(2, 23), B%(2, 23, 79), P$(2, 22), P%(2, 22, 88)
Dim D$(2), Q$(2056), FS%(22, 4, 4), SS%(1, 22, 6)
Dim A1$(1), W2%(1), L2%(1), B1$(2, 23), B3%(2, 23), P1$(2, 22), P3%(2, 22), B0%(2, 22, 22), P0%(2, 22, 42)
Dim D0%(1), D1%(1), TEAM%(1, 22), P4%(1, 14), T1%(2, 23)
Dim K9(1) As Long, S(1), YR$(2), SA$(2)
Dim TS%(1, 11), SA%(1, 24)
Dim Y As Long
Dim X$(4), C$(9), menuItems$(3), teamItems$(200), V%(200), M$(2), S$(2), NN$(2)
X$(0) = "LP": X$(1) = "L": X$(2) = "S": X$(3) = "R": X$(4) = "RP"
C$(0) = "DH": C$(1) = "P": C$(2) = "C": C$(3) = "1B": C$(4) = "2B": C$(5) = "3B": C$(6) = "SS": C$(7) = "LF": C$(8) = "CF": C$(9) = "RF"

Open "DEV.BAS" For Input As #1
For R = 0 To 2
    Input #1, D$(R)
Next R
Close #1

50 Cls
Call TITLE
menuItems$(1) = "CHOOSE TEAMS FOR TRADING PLAYERS"
menuItems$(2) = "RETURN TO MAIN MENU             "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 24

Call box0(5, 23, 8, 56, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I1 = SELECTION%
If I1 = 1 Then 75
Cls

70 Run "HELLO.EXE"

75 J = 0
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID FOR TEAM 1: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 36: Input NY$
If NY$ = "" And YR$(0) <> "" Then GoSub 1102: GoTo 77
YR$(0) = NY$
Erase teamItems$

76 GoSub 1100

77 If BO% = 1 Then BO% = 0: GoTo 50

78 Locate 2, 33: Color 15, 0: Color L%(J, 11), L%(J, 12)
Print A$(J): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 75
Color 15, 0
GoTo 140

1100 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YR$(J)) Then BO% = 1: GoSub 4500: Return

Open D$(0) + "FCTEAMS." + YR$(J) For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1103
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1
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
MAMenu teamItems$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then BO% = 1: Return
T(J) = V%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."
Open D$(0) + "FCTEAMS." + YR$(J) For Random As #1 Len = 4342
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
Get #1, T(J)
A$(J) = RTrim$(Q$(0))
For I1 = 1 To 13: L%(J, I1) = CVI(Q$(I1)): Next
K9(J) = CVS(Q$(14))
For I1 = 0 To 22: B$(J, I1) = RTrim$(Q$(I1 * 43 + 15))
    For I2 = 0 To 41: B%(J, I1, I2) = CVI(Q$(I1 * 43 + I2 + 16))
Next: Next
For I1 = 0 To 21: P$(J, I1) = RTrim$(Q$(I1 * 38 + 1004))
    For I2 = 0 To 36: P%(J, I1, I2) = CVI(Q$(I1 * 38 + I2 + 1005))
Next: Next
S$(J) = RTrim$(Q$(1840))
M$(J) = RTrim$(Q$(1841))
NN$(J) = RTrim$(Q$(1842))
CK = CVI(Q$(1843))
Close 1
If Not _FileExists(D$(0) + "LR." + YR$(J)) Then 1110
Open D$(0) + "LR." + YR$(J) For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
Get #1, T(J)
For X = 0 To 11
    TS%(J, X) = CVI(Q$(X + 1))
Next
For X = 0 To 22
    For X1 = 1 To 14
        B%(J, X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(J, X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(J, X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
Next: Next
For X = 1 To 24
    SA%(J, X) = CVI(Q$(X + 950))
Next
For X = 0 To 22
    For X1 = 1 To 24
        B%(J, X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        P%(J, X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
Next: Next
Close 1
If TS%(J, 0) = 999 Then 1110
For X = 0 To 22
    For X1 = 42 To 79
        B%(J, X, X1) = 0
Next: Next
For X = 0 To 21
    For X1 = 37 To 88
        P%(J, X, X1) = 0
Next: Next
For X = 1 To 24
    SA%(J, X) = 0
Next
For X = 0 To 11
    TS%(J, X) = 0
Next

1110 Return

140 Cls
Print "DOES ";: Color L%(J, 11), L%(J, 12)
Print A$(0);: Color 15, 0
Print " HAVE COMPILED STATS (YN)": S(0) = 0
145 I$ = GetKeyPress$
If UCase$(I$) = "N" Then S(0) = 1: YR$(1) = YR$(0): GoTo 205
Cls

155 GoSub 160
If NF = 1 Then NF = 0: S(0) = 1
YR$(1) = YR$(0)
GoTo 205

160 GoSub 1005
F$ = Mid$(SA$(J), 4, QQ)
R$ = F$ + "." + Left$(SA$(J), 2)
If Not _FileExists(D$(1) + R$) Then GoSub 4550: NF = 1: GoTo 204
Open D$(1) + R$ For Input As #1
Input #1, A1$(J): Input #1, W2%(J): Input #1, L2%(J)
For I1 = 0 To 22: Input #1, B1$(J, I1): Input #1, B3%(J, I1): Next I1
For I1 = 0 To 21: Input #1, P1$(J, I1): Input #1, P3%(J, I1): Next I1
For I1 = 0 To 22: For I2 = 0 To 21: Input #1, B0%(J, I1, I2): Next I2: Next I1
For I1 = 0 To 21: For I2 = 0 To 41: Input #1, P0%(J, I1, I2): Next I2: Next I1
Input #1, D0%(J): Input #1, D1%(J)
For I = 0 To 21: Input #1, TEAM%(J, I): Next I
For I = 0 To 22: Input #1, T1%(J, I): Next I
Close #1
If Not _FileExists(D$(1) + R$ + "R") Then 204
Open D$(1) + R$ + "R" For Input As #1
For I1 = 0 To 22
    For I2 = 0 To 6
        Input #1, SS%(J, I1, I2)
Next I2: Next I1
Close #1

204 Return

205 J = 1
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID FOR TEAM 2: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 36: Input NY$
If NY$ = "" And YR$(1) <> "" Then GoSub 1102: GoTo 277
YR$(1) = NY$
Erase teamItems$

276 GoSub 1100

277 If BO% = 1 Then BO% = 0: GoTo 50

278 Locate 2, 33: Color 15, 0: Color L%(J, 11), L%(J, 12)
Print A$(J): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)"
I$ = GetKeyPress
If UCase$(I$) = "Y" Then 205
Color 15, 0

235 Cls
Print "DOES ";: Color L%(J, 11), L%(J, 12)
Print A$(1);: Color 15, 0
Print " HAVE COMPILED STATS (YN)": S(1) = 0
240 I$ = GetKeyPress$
If UCase$(I$) = "N" Then S(1) = 1: GoTo 260
Cls

255 GoSub 160
If NF = 1 Then NF = 0: S(1) = 1

260 Cls: Color 12: Print Tab(35); "TRADE MENU": Print
Color 14: Print "(1) ";: Color 15: Print "TRADE BATTERS": Print
Color 14: Print "(2) ";: Color 15: Print "TRADE PITCHERS": Print
Color 14: Print "(3) ";: Color 15: Print "END TRADING AND SAVE TRADES": Print
Color 14: Print "(4) ";: Color 15: Print "END TRADING WITHOUT SAVING TRADES"
265 I$ = GetKeyPress$
If I$ = "4" Then 50
If I$ = "3" Then 555
If I$ = "1" Then GoSub 290: GoTo 325
If I$ = "2" Then GoSub 290: GoTo 440
GoTo 260

290 If S(0) = 1 Or S(1) = 1 Then 320
Cls: Print: Print
Color 14: Print "(T) ";: Color 15: Print "TRADE PLAYERS' COMPILED STATS": Print
Color 14: Print "(Z) ";: Color 15: Print "ZERO PLAYERS' COMPILED STATS"
300 I$ = GetKeyPress$
If UCase$(I$) <> "T" And UCase$(I$) <> "Z" Then 300
If UCase$(I$) = "T" Then W1 = 1
If UCase$(I$) = "Z" Then W1 = 0

320 Return

325 Cls: GoSub 6000: Color 15, 0: Locate 1: Print A$(0);: Locate , 40: Print A$(1)
For I = 0 To 16
    Color 15, 0: Print Using "##"; I;: Print " ";
    Color L%(0, 11), L%(0, 12): Print B$(0, I);: Color 15, 0: Locate , 40
    Print Using "##"; I;: Print " ";: Color L%(1, 11), L%(1, 12): Print B$(1, I)
Next I
For I = 17 To 22
    Color 15, 0: Locate I - 15, 20: Print Using "##"; I;: Print " ";
    Color L%(0, 11), L%(0, 12): Print B$(0, I);: Color 15, 0: Locate , 60
    Print Using "##"; I;: Print " ";: Color L%(1, 11), L%(1, 12): Print B$(1, I)
Next I

335 Color 15, 0: Locate 20, 1: GoSub 1065: GoSub 1065: GoSub 1065
Locate 20, 1: Print A$(0); " BATTER: ";

345 Input I$
I = Val(I$): If I < 0 Or I > 22 Then 345
Print B$(0, I)
Locate 21, 1: Print A$(1); " BATTER: ";

365 Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 22 Then 365
Print B$(1, I1)
Locate 22, 1: Print "ANY CHANGE (YN) ";
385 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 335
GoSub 665
If TS%(0, 0) = 999 And TS%(1, 0) = 999 Then 386
If TS%(0, 0) <> 999 And TS%(1, 0) <> 999 Then 386
If TS%(0, 0) = 999 And TS%(1, 0) <> 999 Then For I2 = 0 To 11: TS%(1, I2) = TS%(0, I2): Next: For I2 = 1 To 24: SA%(1, I2) = SA%(0, I2): Next: GoTo 386
If TS%(1, 0) = 999 And TS%(0, 0) <> 999 Then For I2 = 0 To 11: TS%(0, I2) = TS%(1, I2): Next: For I2 = 1 To 24: SA%(0, I2) = SA%(1, I2): Next

386 Swap B$(0, I), B$(1, I1)
For I2 = 0 To 79
    Swap B%(0, I, I2), B%(1, I1, I2)
Next I2
For I2 = 0 To 6
    Swap SS%(0, I, I2), SS%(1, I1, I2)
Next I2
B1$(0, I) = B$(0, I): B1$(1, I1) = B$(1, I1)
Swap B3%(0, I), B3%(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 430
For I2 = 0 To 21: Swap B0%(0, I, I2), B0%(1, I1, I2): Next I2
GoSub 685: GoTo 260

430 For I2 = 0 To 21: B0%(0, I, I2) = 0: B0%(1, I1, I2) = 0: Next I2
GoSub 685: GoTo 260

440 Cls: GoSub 7000: Color 15, 0: Locate 1: Print A$(0);: Locate , 40: Print A$(1)
For I = 0 To 11
    Color 15, 0: Print Using "##"; I;: Print " ";
    Color L%(0, 11), L%(0, 12): Print P$(0, I);: Color 15, 0
    Locate , 40: Print Using "##"; I;: Print " ";: Color L%(1, 11), L%(1, 12)
    Print P$(1, I)
Next I
For I = 12 To 21
    Color 15, 0: Locate I - 10, 20: Print Using "##"; I;: Print "  ";
    Color L%(0, 11), L%(0, 12): Print P$(0, I);: Locate I - 10, 40: Color 15, 0
    Locate , 60: Print Using "##"; I;: Print "  ";: Color L%(1, 11), L%(1, 12)
    Print P$(1, I)
Next I

450 Color 15, 0: Locate 20, 1: GoSub 1065: GoSub 1065: GoSub 1065
Locate 20, 1: Print A$(0); " PITCHER: ";

460 Input I$
I = Val(I$): If I < 0 Or I > 21 Then 460
Print P$(0, I)
Locate 21, 1: Print A$(1); " PITCHER: ";

480 Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 21 Then 480
Print P$(1, I1)
Locate 22, 1: Print "ANY CHANGE (YN) ";

500 I$ = GetKeyPress
If UCase$(I$) = "Y" Then 450
GoSub 705
If TS%(0, 0) = 999 And TS%(1, 0) = 999 Then 505
If TS%(0, 0) <> 999 And TS%(1, 0) <> 999 Then 505
If TS%(0, 0) = 999 And TS%(1, 0) <> 999 Then For I2 = 0 To 11: TS%(1, I2) = TS%(0, I2): Next: For I2 = 1 To 24: SA%(1, I2) = SA%(0, I2): Next: GoTo 505
If TS%(1, 0) = 999 And TS%(0, 0) <> 999 Then For I2 = 0 To 11: TS%(0, I2) = TS%(1, I2): Next: For I2 = 1 To 24: SA%(0, I2) = SA%(1, I2): Next

505 Swap P$(0, I), P$(1, I1)
For I2 = 0 To 88: Swap P%(0, I, I2), P%(1, I1, I2): Next I2
P1$(0, I) = P$(0, I): P1$(1, I1) = P$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 545
Swap P3%(0, I), P3%(1, I1)
For I2 = 0 To 41: Swap P0%(0, I, I2), P0%(1, I1, I2): Next I2
GoSub 725: GoTo 260

545 For I2 = 0 To 41: P0%(0, I, I2) = 0: P0%(1, I1, I2) = 0: Next I2
GoSub 725: GoTo 260

555 For I = 0 To 1
    Cls: Color 15, 0
    If UCase$(Left$(D$(0), 1)) <> "A" And UCase$(Left$(D$(0), 1)) <> "B" Then 556

    556 Open D$(0) + "FCTEAMS." + YR$(I) For Random As #1 Len = 4342
    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
    For J = 0 To 22
        Field #1, 45 + J * 99 As X$, 15 As Q$(J * 43 + 15), 2 As Q$(J * 43 + 16), 2 As Q$(J * 43 + 17), 2 As Q$(J * 43 + 18), 2 As Q$(J * 43 + 19), 2 As Q$(J * 43 + 20), 2 As Q$(J * 43 + 21), 2 As Q$(J * 43 + 22), 2 As Q$(J * 43 + 23), 2 As Q$(J * 43 + 24), 2 As Q$(J * 43 + 25), 2 As Q$(J * 43 + 26), 4297 - 37 - J * 99 As X$
        Field #1, 82 + J * 99 As X$, 2 As Q$(J * 43 + 27), 2 As Q$(J * 43 + 28), 2 As Q$(J * 43 + 29), 2 As Q$(J * 43 + 30), 2 As Q$(J * 43 + 31), 2 As Q$(J * 43 + 32), 2 As Q$(J * 43 + 33), 2 As Q$(J * 43 + 34), 2 As Q$(J * 43 + 35), 2 As Q$(J * 43 + 36), 2 As Q$(J * 43 + 37), 2 As Q$(J * 43 + 38), 4297 - 61 - J * 99 As X$
        Field #1, 106 + J * 99 As X$, 2 As Q$(J * 43 + 39), 2 As Q$(J * 43 + 40), 2 As Q$(J * 43 + 41), 2 As Q$(J * 43 + 42), 2 As Q$(J * 43 + 43), 2 As Q$(J * 43 + 44), 2 As Q$(J * 43 + 45), 2 As Q$(J * 43 + 46), 2 As Q$(J * 43 + 47), 2 As Q$(J * 43 + 48), 2 As Q$(J * 43 + 49), 2 As Q$(J * 43 + 50), 4297 - 85 - J * 99 As X$
        Field #1, 130 + J * 99 As X$, 2 As Q$(J * 43 + 51), 2 As Q$(J * 43 + 52), 2 As Q$(J * 43 + 53), 2 As Q$(J * 43 + 54), 2 As Q$(J * 43 + 55), 2 As Q$(J * 43 + 56), 2 As Q$(J * 43 + 57), 4297 - 99 - J * 99 As X$
    Next

    For J = 0 To 21
        Field #1, 2322 + J * 89 As X$, 15 As Q$(J * 38 + 1004), 2 As Q$(J * 38 + 1005), 2 As Q$(J * 38 + 1006), 2 As Q$(J * 38 + 1007), 2 As Q$(J * 38 + 1008), 2 As Q$(J * 38 + 1009), 2 As Q$(J * 38 + 1010), 2 As Q$(J * 38 + 1011), 2 As Q$(J * 38 + 1012), 2 As Q$(J * 38 + 1013), 2 As Q$(J * 38 + 1014), 2 As Q$(J * 38 + 1015), 2020 - 37 - J * 89 As X$
        Field #1, 2359 + J * 89 As X$, 2 As Q$(J * 38 + 1016), 2 As Q$(J * 38 + 1017), 2 As Q$(J * 38 + 1018), 2 As Q$(J * 38 + 1019), 2 As Q$(J * 38 + 1020), 2 As Q$(J * 38 + 1021), 2 As Q$(J * 38 + 1022), 2 As Q$(J * 38 + 1023), 2 As Q$(J * 38 + 1024), 2 As Q$(J * 38 + 1025), 2 As Q$(J * 38 + 1026), 2 As Q$(J * 38 + 1027), 2020 - 61 - J * 89 As X$
        Field #1, 2383 + J * 89 As X$, 2 As Q$(J * 38 + 1028), 2 As Q$(J * 38 + 1029), 2 As Q$(J * 38 + 1030), 2 As Q$(J * 38 + 1031), 2 As Q$(J * 38 + 1032), 2 As Q$(J * 38 + 1033), 2 As Q$(J * 38 + 1034), 2 As Q$(J * 38 + 1035), 2 As Q$(J * 38 + 1036), 2 As Q$(J * 38 + 1037), 2 As Q$(J * 38 + 1038), 2 As Q$(J * 38 + 1039), 2020 - 85 - J * 89 As X$
        Field #1, 2407 + J * 89 As X$, 2 As Q$(J * 38 + 1040), 2 As Q$(J * 38 + 1041), 2020 - 89 - J * 89 As X$
    Next

    Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
    LSet Q$(0) = A$(I): For J = 1 To 13: LSet Q$(J) = MKI$(L%(I, J)): Next: LSet Q$(14) = MKS$(K9(I))
    For J = 0 To 22: LSet Q$(J * 43 + 15) = B$(I, J)
        For I1 = 0 To 41: LSet Q$(J * 43 + I1 + 16) = MKI$(B%(I, J, I1))
    Next: Next
    For J = 0 To 21: LSet Q$(J * 38 + 1004) = P$(I, J)
        For I1 = 0 To 36: LSet Q$(J * 38 + I1 + 1005) = MKI$(P%(I, J, I1))
    Next: Next
    LSet Q$(1840) = S$(I): LSet Q$(1841) = M$(I): LSet Q$(1842) = NN$(I): LSet Q$(1843) = MKI$(999)
    Put #1, T(I): Close 1
    SCK = 0
    For X = 1 To 24
        SCK = SCK + SA%(I, X)
    Next
    If SCK <= 0 Then 1435
    TS%(I, 0) = 999
    For X = 1 To 11
        TS%(I, X) = -1
    Next
    Open D$(0) + "LR." + YR$(I) For Random As #1 Len = 4108
    For X = 0 To 11
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
    Next
    For X = 0 To 22
        For X1 = 1 To 14
            Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
        Next
    Next
    For X = 0 To 21
        For X1 = 1 To 14
            Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
        Next
    Next
    For X = 0 To 21
        For X1 = 1 To 14
            Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
        Next
    Next
    For X = 1 To 24
        Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
    Next
    For X = 0 To 22
        For X1 = 1 To 24
            Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
        Next
    Next
    For X = 0 To 21
        For X1 = 1 To 24
            Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
        Next
    Next
    For X = 0 To 11: LSet Q$(X + 1) = MKI$(TS%(I, X)): Next
    For X = 0 To 22
        For X1 = 1 To 14
            LSet Q$(12 + X1 + X * 14) = MKI$(B%(I, X, X1 + 41))
    Next: Next
    For X = 0 To 21
        For X1 = 1 To 14
            LSet Q$(334 + X1 + X * 14) = MKI$(P%(I, X, X1 + 36))
    Next: Next
    For X = 0 To 21
        For X1 = 1 To 14
            LSet Q$(642 + X1 + X * 14) = MKI$(P%(I, X, X1 + 50))
    Next: Next
    For X = 1 To 24
        LSet Q$(X + 950) = MKI$(SA%(I, X))
    Next
    For X = 0 To 22
        For X1 = 1 To 24
            LSet Q$(975 + X1 + X * 24) = MKI$(B%(I, X, X1 + 55))
    Next: Next
    For X = 0 To 21
        For X1 = 1 To 24
            LSet Q$(1528 + X1 + X * 24) = MKI$(P%(I, X, X1 + 64))
    Next: Next
    Put #1, T(I)
    Close 1

1435 Next I
For I = 0 To 1
    If S(I) = 1 Then 655
    Cls

    557 GoSub 1035
    F$ = Mid$(SA$(I), 4, QQ): R$ = F$ + "." + Left$(SA$(I), 2)
    Open D$(1) + R$ For Output As #1
    Print #1, A1$(I): Print #1, W2%(I): Print #1, L2%(I)
    For I1 = 0 To 22: Print #1, B1$(I, I1): Print #1, B3%(I, I1): Next I1
    For I1 = 0 To 21: Print #1, P1$(I, I1): Print #1, P3%(I, I1): Next I1
    For I1 = 0 To 22: For I2 = 0 To 21: Print #1, B0%(I, I1, I2): Next I2: Next I1
    For I1 = 0 To 21: For I2 = 0 To 41: Print #1, P0%(I, I1, I2): Next I2: Next I1
    Print #1, D0%(I): Print #1, D1%(I)
    For I1 = 0 To 21: Print #1, TEAM%(I, I1): Next I1
    For I1 = 0 To 22: Print #1, T1%(I, I1): Next I1
    Close #1
    Open D$(1) + R$ + "R" For Output As #1
    For I1 = 0 To 22: For I2 = 0 To 6
        Print #1, SS%(I, I1, I2)
    Next I2: Next I1
    Close #1

655 Next I
GoTo 50

665 GoSub 745
Locate 24, 1: Print "HIT ANY KEY TO TRADE";
675 I$ = GetKeyPress$
Return

685 GoSub 745
Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
695 I$ = GetKeyPress$
Return

705 GoSub 905
Locate 24, 1: Print "HIT ANY KEY TO TRADE";
715 I$ = GetKeyPress$
Return

725 GoSub 905
Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
735 I$ = GetKeyPress$
Return

745 Cls: Color L%(0, 11), L%(0, 12): Print A$(0);: Locate , 41: Color L%(1, 11), L%(1, 12): Print A$(1): Color 15, 0
Print B$(0, I); " "; X$(B%(0, I, 0) + 2); " "; C$(B%(0, I, 22));
If B%(0, I, 23) >= 0 Then Print ","; C$(B%(0, I, 23));
If B%(0, I, 24) >= 0 Then Print ","; C$(B%(0, I, 24));
If B%(0, I, 25) >= 0 Then Print ","; C$(B%(0, I, 25));
Locate , 41:
Print B$(1, I1); " "; X$(B%(1, I1, 0) + 2); " "; C$(B%(1, I1, 22));
If B%(1, I1, 23) >= 0 Then Print ","; C$(B%(1, I1, 23));
If B%(1, I1, 24) >= 0 Then Print ","; C$(B%(1, I1, 24));
If B%(1, I1, 25) >= 0 Then Print ","; C$(B%(1, I1, 25));
Locate , 1: Print: Print "REAL";: Locate , 15: Print "COMP";: Locate , 41: Print "REAL";: Locate , 55: Print "COMP": Locate , 1
Locate , 1: Print B%(0, I, 3);: Locate , 10: Print "G";: Locate , 15: Print B0%(0, I, 14);: Locate , 41: Print B%(1, I1, 3);: Locate , 50: Print "G";: Locate , 55: Print B0%(1, I1, 14)
Locate , 1: Print B%(0, I, 4);: Locate , 10: Print "AB";: Locate , 15: Print B0%(0, I, 0);: Locate , 41: Print B%(1, I1, 4);: Locate , 50: Print "AB";: Locate , 55: Print B0%(1, I1, 0)
Locate , 1: Print B%(0, I, 5);: Locate , 10: Print "R";: Locate , 15: Print B0%(0, I, 2);: Locate , 41: Print B%(1, I1, 5);: Locate , 50: Print "R";: Locate , 55: Print B0%(1, I1, 2)
Locate , 1: Print B%(0, I, 6);: Locate , 10: Print "H";: Locate , 15: Print B0%(0, I, 1);: Locate , 41: Print B%(1, I1, 6);: Locate , 50: Print "H";: Locate , 55: Print B0%(1, I1, 1)
Locate , 1: Print B%(0, I, 10);: Locate , 9: Print "RBI";: Locate , 15: Print B0%(0, I, 3);: Locate , 41: Print B%(1, I1, 10);: Locate , 49: Print "RBI";: Locate , 55: Print B0%(1, I1, 3)
Locate , 1: Print B%(0, I, 7);: Locate , 10: Print "DB";: Locate , 15: Print B0%(0, I, 4);: Locate , 41: Print B%(1, I1, 7);: Locate , 50: Print "DB";: Locate , 55: Print B0%(1, I1, 4)
Locate , 1: Print B%(0, I, 8);: Locate , 10: Print "TR";: Locate , 15: Print B0%(0, I, 5);: Locate , 41: Print B%(1, I1, 8);: Locate , 50: Print "TR";: Locate , 55: Print B0%(1, I1, 5)
Locate , 1: Print B%(0, I, 9);: Locate , 10: Print "HR";: Locate , 15: Print B0%(0, I, 6);: Locate , 41: Print B%(1, I1, 9);: Locate , 50: Print "HR";: Locate , 55: Print B0%(1, I1, 6)
Locate , 1: Print B%(0, I, 11);: Locate , 10: Print "BB";: Locate , 15: Print B0%(0, I, 7);: Locate , 41: Print B%(1, I1, 11);: Locate , 50: Print "BB";: Locate , 55: Print B0%(1, I1, 7)
Locate , 1: Print B%(0, I, 12);: Locate , 10: Print "SO";: Locate , 15: Print B0%(0, I, 8);: Locate , 41: Print B%(1, I1, 12);: Locate , 50: Print "S0";: Locate , 55: Print B0%(1, I1, 8)
Locate , 1: If B%(0, I, 4) = 0 Then Print "-.---";: GoTo 845
If B%(0, I, 6) = B%(0, I, 4) Then Print "1.000";: GoTo 845
Print Using " .###"; (B%(0, I, 6) / B%(0, I, 4));

845 Locate , 9: Print "BAVG";: Locate , 15: If B0%(0, I, 0) = 0 Then Print "-.---";: GoTo 860
If B0%(0, I, 0) = B0%(0, I, 1) Then Print "1.000";: GoTo 860
Print Using " .###"; (B0%(0, I, 1) / B0%(0, I, 0));

860 Locate , 41: If B%(1, I1, 4) = 0 Then Print "-.---";: GoTo 875
If B%(1, I1, 6) = B%(1, I1, 4) Then Print "1.000";: GoTo 875
Print Using " .###"; (B%(1, I1, 6) / B%(1, I1, 4));

875 Locate , 49: Print "BAVG";: Locate , 55: If B0%(1, I1, 0) = 0 Then Print "-.---": GoTo 890
If B0%(1, I1, 0) = B0%(1, I1, 1) Then Print "1.000": GoTo 890
Print Using " .###"; (B0%(1, I1, 1) / B0%(1, I1, 0))

890 Locate , 1: Print B%(0, I, 13);: Locate , 10: Print "SB";: Locate , 15: Print B0%(0, I, 9);: Locate , 41: Print B%(1, I1, 13);: Locate , 50: Print "SB";: Locate , 55: Print B0%(1, I1, 9)
Locate , 1: Print B%(0, I, 14);: Locate , 10: Print "CS";: Locate , 15: Print B0%(0, I, 10);: Locate , 41: Print B%(1, I1, 14);: Locate , 50: Print "CS";: Locate , 55: Print B0%(1, I1, 10)
Return

905 Cls: Color L%(0, 11), L%(0, 12): Print A$(0);: Locate , 41
Color L%(1, 11), L%(1, 12): Print A$(1): Color 15, 0
Locate , 1: Print P$(0, I); " "; X$(P%(0, I, 0) + 2);: Locate , 41: Print P$(1, I1); " "; X$(P%(1, I1, 0) + 2)
Locate , 1: Print: Print "REAL";: Locate , 15: Print "COMP";: Locate , 41: Print "REAL";: Locate , 55: Print "COMP"
    LOCATE , 1: PRINT P%(0, I, 1); "-"; P%(0, I, 2); : LOCATE , 9: PRINT "W-L"; : LOCATE , 15: PRINT P0%(0, I, 6); "-"; P0%(0, I, 7); : LOCATE , 41: PRINT P%(1, I1, 1); "-"; P%(1, I1, 2); : LOCATE , 49: PRINT "W-L"; : LOCATE , 55: PRINT P0%(1, I1, 6 _
); "-"; P0%(1, I1, 7)
Locate , 1: Print P%(0, I, 4);: Locate , 10: Print "G";: Locate , 15: Print P0%(0, I, 8);: Locate , 41: Print P%(1, I1, 4);: Locate , 50: Print "G";: Locate , 55: Print P0%(1, I1, 8)
Locate , 1: Print P%(0, I, 5);: Locate , 10: Print "GS";: Locate , 15: Print P0%(0, I, 9);: Locate , 41: Print P%(1, I1, 5);: Locate , 50: Print "GS";: Locate , 55: Print P0%(1, I1, 9)
Locate , 1: Print P%(0, I, 3);: Locate , 10: Print "SV";: Locate , 15: Print P0%(0, I, 12);: Locate , 41: Print P%(1, I1, 3);: Locate , 50: Print "SV";: Locate , 55: Print P0%(1, I1, 12)
Locate , 1: Print P%(0, I, 6);: Locate , 10: Print "IP";: Locate , 15: Print Int((P0%(0, I, 0) / 3) + .5);: Locate , 41: Print P%(1, I1, 6);: Locate , 50: Print "IP";: Locate , 55: Print Int((P0%(1, I1, 0) / 3) + .5)
Locate , 1: Print P%(0, I, 7);: Locate , 10: Print "H";: Locate , 15: Print P0%(0, I, 1);: Locate , 41: Print P%(1, I1, 7);: Locate , 50: Print "H";: Locate , 55: Print P0%(1, I1, 1)
Locate , 1: Print P%(0, I, 8);: Locate , 10: Print "BB";: Locate , 15: Print P0%(0, I, 4);: Locate , 41: Print P%(1, I1, 8);: Locate , 50: Print "BB";: Locate , 55: Print P0%(1, I1, 4)
Locate , 1: Print P%(0, I, 9);: Locate , 10: Print "SO";: Locate , 15: Print P0%(0, I, 5);: Locate , 41: Print P%(1, I1, 9);: Locate , 50: Print "SO";: Locate , 55: Print P0%(1, I1, 5)
Locate , 1: Print P%(0, I, 13);: Locate , 10: Print "HR";: Locate , 15: Print P0%(0, I, 14);: Locate , 41: Print P%(1, I1, 13);: Locate , 50: Print "HR";: Locate , 55: Print P0%(1, I1, 14)
Locate , 1: Print Using "##.##"; P%(0, I, 10) / 100;: Locate , 9: Print "ERA";: Locate , 15: If P0%(0, I, 0) = 0 Then Print " -.--";: GoTo 965
Print Using "##.##"; P0%(0, I, 3) * 27 / P0%(0, I, 0);

965 Locate , 41
Print Using "##.##"; P%(1, I1, 10) / 100;: Locate , 49: Print "ERA";: Locate , 55: If P0%(1, I1, 0) = 0 Then Print " -.--": GoTo 985
Print Using "##.##"; P0%(1, I1, 3) * 27 / P0%(1, I1, 0)
If P0%(0, I, 0) > 0 Then G1% = Int((((P0%(0, I, 3) * 9) / (P0%(0, I, 0) / 3)) + .0005) * 100)
E1% = Int(G1% / 100): F1% = G1% - (E1% * 100)

985 Return

1005 SA$(J) = A$(J)
For XX = 4 To 11
    If Mid$(SA$(J), XX, 1) = " " Or Mid$(SA$(J), XX, 1) = "." Then Mid$(SA$(J), XX, 1) = "_"
Next
QQ = 8
For XX = 11 To 4 Step -1
    If Mid$(SA$(J), XX, 1) = "_" Then QQ = XX - 4: GoTo 1025
    GoTo 1030

1025 Next XX

1030 Return

1035 For XX = 4 To 11
    If Mid$(SA$(I), XX, 1) = " " Or Mid$(SA$(I), XX, 1) = "." Then Mid$(SA$(I), XX, 1) = "_"
Next
QQ = 8
For XX = 11 To 4 Step -1
    If Mid$(SA$(I), XX, 1) = "_" Then QQ = XX - 4: GoTo 1055
    GoTo 1060

1055 Next XX

1060 Return

1065 Print "                                        ";: Return

6000 For I = 0 To 16: Locate I + 2, 4: Color , L%(0, 12): Print "               ": Next
For I = 0 To 5: Locate I + 2, 24: Color , L%(0, 12): Print "               ": Next
For I = 0 To 16: Locate I + 2, 44: Color , L%(1, 12): Print "               ": Next
For I = 0 To 5: Locate I + 2, 64: Color , L%(1, 12): Print "               ": Next
Return

7000 For I = 0 To 11: Locate I + 2, 4: Color , L%(0, 12): Print "               ": Next
For I = 0 To 10: Locate I + 2, 24: Color , L%(0, 12): Print "               ": Next
For I = 0 To 11: Locate I + 2, 44: Color , L%(1, 12): Print "               ": Next
For I = 0 To 10: Locate I + 2, 64: Color , L%(1, 12): Print "               ": Next
Return
    
Locate 10, 8
Call CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4550 Locate 10, 8
Call CSMsgBox("NO COMPILED STATS FOR TEAM WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4500 Locate 10, 8
Call CSMsgBox("NO TEAMS MATCHING THIS ID WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
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
