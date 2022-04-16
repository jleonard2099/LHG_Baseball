'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Team Input"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

DECLARE FUNCTION PadRight$ (X$, Length)

DECLARE SUB BATTEREDIT ()
DECLARE SUB PITCHEREDIT ()
DECLARE SUB PITCHINIT (i, Colr)
DECLARE SUB PITCHINIT2 (i, Colr)
DECLARE SUB EDITTEAM (YN$, T$, NN$, S$, M$, T)
DECLARE SUB TITLE ()

Dim D$(3), menuItems$(10)
Dim K9(1) As Long
Dim Q$(2100), FS%(22, 4, 4), BP$(3)
Dim B%(22, 79), P%(21, 88), B$(22), P$(21), L%(13)
Dim BS$(22), PS$(21)
Dim B0%(22, 40), P0%(21, 41), BS%(22), PS%(32), T(22), T1(22)
Dim teamItems$(200), V%(200), SS%(22, 6)
Dim TB%(15), TP%(33), INJ%(22), DYS%(21)
Dim SA%(24), TS%(11), BRS$(79)
Dim SLG2 As Single, OB As Single, R1 As Single, R2 As Single
Dim BUNT As Single, B1 As Single, RR As Single
Dim I2 As Single, ERX As Single
Dim Y As Long

Dim Shared B%, P%, B$, P$, L%, BRS$, K9 As Long, SA%, TS%
Dim Shared YN$, T$, NN$, S$, M$, T, N$, INJ%, DYS%


For I = 0 To 3
    Read BP$(I)
Next

Data "OUTDOOR","INDOOR ","GRASS","TURF"

Open "DEV.BAS" For Input As #1
For I = 0 To 2
    Input #1, D$(I)
Next I
Close #1

55 Call TITLE
menuItems$(1) = "INPUT A NEW TEAM           "
menuItems$(2) = "EDIT AN EXISTING TEAM      "
menuItems$(3) = "PRINT ACTUAL TEAM STATS    "
menuItems$(4) = "ENTER/EDIT UMPIRES         "
menuItems$(5) = "SWAP SPOTS ON A TEAM ROSTER"
menuItems$(6) = "RETURN TO MAIN MENU        "
menuItems$(7) = "CALCULATE BUNT RATINGS     "
menuItems$(8) = "CALCULATE RUN RATINGS      "
menuItems$(9) = "VIEW TEAM ROSTERS/RATINGS  "
SELECTION% = 1
Count% = 9
Start% = 1
SCANCODE% = 9
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 25

Call box0(5, 24, 15, 52, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 6
Q = SELECTION% - 1
If Q = 0 Then 105
If Q = 2 Or Q = 10 Then 77
If Q = 1 Then 77
If Q = 3 Then Run "UMPMGR.EXE"
If Q = 4 Then 77
If Q = 5 Then 88
If Q = 6 Then 77

77 If Q = 1 Or Q = 2 Or Q = 4 Or Q = 8 Or Q = 6 Or Q = 7 Then 1445

88 Run "HELLO.EXE"

105 If Q <> 0 Then 106
T$ = ""
T = 0
YN$ = ""
For X = 1 To 13
    L%(X) = 0
Next
K9(0) = 0
For X = 0 To 22
    B$(X) = ""
    For X1 = 0 To 41
        B%(X, X1) = 0
Next: Next
For X = 0 To 21
    P$(X) = ""
    For X1 = 0 To 36
        P%(X, X1) = 0
Next: Next
S$ = ""
M$ = ""
NN$ = ""
CK = 0
For X = 0 To 22
    For X1 = 42 To 79
        B%(X, X1) = 0
Next: Next
For X = 0 To 21
    For X1 = 37 To 88
        P%(X, X1) = 0
Next: Next
For X = 1 To 24
    SA%(X) = 0
Next
For X = 0 To 11
    TS%(X) = 0
Next

106 Call EDITTEAM(YN$, T$, NN$, S$, M$, T)
Call BATTEREDIT
Call PITCHEREDIT

1385 Locate 10, 8
Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14)
For I = 0 To 22
    Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26)
    Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38)
    Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50)
    Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57)
Next

For I = 0 To 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015)
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027)
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039)
    Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
Next

Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
LSet Q$(0) = T$: For I = 1 To 13: LSet Q$(I) = MKI$(L%(I)): Next: LSet Q$(14) = MKS$(K9(0))

For I = 0 To 22: LSet Q$(I * 43 + 15) = B$(I)
    For I1 = 0 To 41: LSet Q$(I * 43 + I1 + 16) = MKI$(B%(I, I1))
Next: Next
For I = 0 To 21: LSet Q$(I * 38 + 1004) = P$(I)
    For I1 = 0 To 36: LSet Q$(I * 38 + I1 + 1005) = MKI$(P%(I, I1))
Next: Next
LSet Q$(1840) = S$: LSet Q$(1841) = M$: LSet Q$(1842) = NN$: LSet Q$(1843) = MKI$(999)
Put #1, T: Close 1

1386 SCK = 0
For I = 1 To 24
    SCK = SCK + SA%(I)
Next
If SCK <= 0 Then 1435
TS%(0) = 999
For I = 1 To 11
    TS%(I) = -1
Next

Open D$(0) + "LR." + YN$ For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q$(X + 1)
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14)
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14)
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14)
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X)
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24)
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24)
    Next
Next
For X = 0 To 11: LSet Q$(X + 1) = MKI$(TS%(X)): Next
For X = 0 To 22
    For X1 = 1 To 14
        LSet Q$(12 + X1 + X * 14) = MKI$(B%(X, X1 + 41))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        LSet Q$(334 + X1 + X * 14) = MKI$(P%(X, X1 + 36))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        LSet Q$(642 + X1 + X * 14) = MKI$(P%(X, X1 + 50))
Next: Next
For X = 1 To 24
    LSet Q$(X + 950) = MKI$(SA%(X))
Next
For X = 0 To 22
    For X1 = 1 To 24
        LSet Q$(975 + X1 + X * 24) = MKI$(B%(X, X1 + 55))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        LSet Q$(1528 + X1 + X * 24) = MKI$(P%(X, X1 + 64))
Next: Next
Put #1, T
Close 1

1435 Open D$(1) + F$ + "D" For Output As #1
For I1 = 0 To 22
    Print #1, INJ%(I1)
Next
For I1 = 0 To 21
    Print #1, DYS%(I1)
Next

1388 Close 1
    
Call CSMsgBox("", 0, Pass)
GoTo 55

1100 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then GoSub 4500: BO% = 1: Return

Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
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
T = V%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14)
For I = 0 To 22
    Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26)
    Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38)
    Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50)
    Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57)
Next

For I = 0 To 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015)
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027)
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039)
    Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
Next

Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
Get #1, T
T$ = RTrim$(Q$(0))
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
If Not _FileExists(D$(0) + "LR." + YN$) Then 1200
Open D$(0) + "LR." + YN$ For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q$(X + 1)
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14)
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14)
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14)
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X)
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24)
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24)
    Next
Next
Get #1, T
For X = 0 To 11
    TS%(X) = CVI(Q$(X + 1))
Next
For X = 0 To 22
    For X1 = 1 To 14
        B%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
Next: Next
For X = 1 To 24
    SA%(X) = CVI(Q$(X + 950))
Next
For X = 0 To 22
    For X1 = 1 To 24
        B%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        P%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
Next: Next
Close 1
If TS%(0) = 999 Then 1200
For X = 0 To 22
    For X1 = 42 To 79
        B%(X, X1) = 0
Next: Next
For X = 0 To 21
    For X1 = 37 To 88
        P%(X, X1) = 0
Next: Next
For X = 1 To 24
    SA%(X) = 0
Next
For X = 0 To 11
    TS%(X) = 0
Next

1200 N$ = T$
For xx = 4 To 11
    If Mid$(N$, xx, 1) = " " Or Mid$(N$, xx, 1) = "." Then Mid$(N$, xx, 1) = "_"
Next xx
QQ = 8
For xx = 11 To 4 Step -1
    If Mid$(N$, xx, 1) = "_" Then QQ = xx - 4: GoTo 118
    GoTo 119
118 Next xx
119 '
F$ = Mid$(N$, 4, QQ)
F$ = F$ + "." + Left$(N$, 2)
If Not _FileExists(D$(1) + F$ + "D") Then 318
Open D$(1) + F$ + "D" For Input As #1
For I1 = 0 To 22
    Input #1, INJ%(I1)
Next
For I1 = 0 To 21
    Input #1, DYS%(I1)
Next

318 Close 1
Return
    
Return

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

1441 GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 1445

183 Locate 2, 33: Color 15, 0: Color L%(11), L%(12): Print T$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1445
Color 15, 0
If Q = 2 Then 1675

1540 If Q = 4 Then 3000
If Q = 7 Then 1971
If Q = 6 Then 1920
If Q = 8 Then 80
GoTo 105

1675 LPrint Chr$(15) ': WIDTH "LPT1:", 137
LPrint "TEAM # "; T

1685 LPrint "TEAM NAME "; T$; Tab(30); "MANAGER: "; M$; Tab(50); "STADIUM: "; S$

1695 LPrint: LPrint "LBA LSO LBB LHR DPG DBL TR    HR   FOUL PARK INFO"

1705 LPrint Using "### ### ### ### ### ### ### ###### ###  "; L%(1); L%(2); L%(3); L%(4); L%(5); L%(6); L%(7); K9(0); L%(8);: LPrint BP$(L%(9)); " "; BP$(L%(10) + 2)

1725 LPrint

1735 LPrint "BATTER      B    G  AB   R   H DB TR HR RBI  BB  SO  SB CS RN B GA P1 FAV1 A R P2 FAV2 A R P3 FAV3 A R P4 FAV4 A R EBA BAVG"

1755 For I = 0 To 22: If B$(I) = "" Or B$(I) = "XXX" Then 1855

    1756 LPrint Left$(B$(I), 12); Tab(13);: If B%(I, 0) = 0 Then LPrint "S  ";

    1765 If B%(I, 0) = -1 Then LPrint "L  ";

    1775 If B%(I, 0) = 1 Then LPrint "R  ";

    1785 If B%(I, 0) = -2 Then LPrint "LP ";

    1795 If B%(I, 0) = 2 Then LPrint "RP ";

    1796 LPrint Using "### ### ### ### ## ## ## ### ### ### ### ##  "; B%(I, 3); B%(I, 4); B%(I, 5); B%(I, 6); B%(I, 7); B%(I, 8); B%(I, 9); B%(I, 10); B%(I, 11); B%(I, 12); B%(I, 13); B%(I, 14);

    1797 LPrint Using "# "; B%(I, 16);: LPrint Using "# "; B%(I, 30);

    1798 LPrint Using "## ## #### # # "; B%(I, 18); B%(I, 22); B%(I, 26); B%(I, 33); B%(I, 34);
    If B%(I, 23) < 0 Then 1845
    LPrint Using "## #### # # "; B%(I, 23); B%(I, 27); B%(I, 35); B%(I, 36);
    If B%(I, 24) < 0 Then 1845
    LPrint Using "## #### # # "; B%(I, 24); B%(I, 28); B%(I, 37); B%(I, 38);
    If B%(I, 25) < 0 Then 1845
    LPrint Using "## #### # # "; B%(I, 25); B%(I, 29); B%(I, 39); B%(I, 40);

    1845 LPrint Tab(116); Using "### "; B%(I, 20);

    1846 If B%(I, 4) = 0 Then LPrint " 000": GoTo 1855

    1847 Z% = B%(I, 6) / B%(I, 4) * 1000: LPrint Using " ###"; B%(I, 6) / B%(I, 4) * 1000

1855 Next I
For I = 0 To 21: If P$(I) = "" Or P$(I) = "XXX" Then 1862

    1856 LPrint Left$(P$(I), 12); Tab(13);: If P%(I, 0) = 1 Then LPrint "R  ";

    1857 If P%(I, 0) = -1 Then LPrint "L  ";

    1858 LPrint Using "### ### ### ### ## ## ## ### ### ### ### ## "; P%(I, 19); P%(I, 20); P%(I, 21); P%(I, 22); P%(I, 23); P%(I, 24); P%(I, 25); P%(I, 26); P%(I, 27); P%(I, 28); P%(I, 29); P%(I, 30);

    1859 LPrint Using " # # 52  1 ####                                         ### "; P%(I, 31); P%(I, 32); P%(I, 17); P%(I, 14);

    1860 If P%(I, 20) = 0 Then LPrint " 000": GoTo 1862

    1861 Z% = P%(I, 22) / P%(I, 20) * 1000: LPrint Using " ###"; P%(I, 22) / P%(I, 20) * 1000

1862 Next I: LPrint

1865 LPrint "PITCHER     T  W  L  SV G  GS CG IP  H   BB  SO  ERA  GA HRA HOLD EBA"

1885 For I = 0 To 21: If P$(I) = "" Or P$(I) = "XXX" Then 1915

    1895 LPrint Mid$(P$(I), 1, 12); Tab(13);: If P%(I, 0) = 1 Then LPrint "R  ";

    1905 If P%(I, 0) = -1 Then LPrint "L  ";

    1910 LPrint Using "## ## ## ## ## ## ### ### ### ### #.## ## ##   #   ###"; P%(I, 1); P%(I, 2); P%(I, 3); P%(I, 4); P%(I, 5); P%(I, 16); P%(I, 6); P%(I, 7); P%(I, 8); P%(I, 9); P%(I, 10) / 100; P%(I, 12); P%(I, 13); P%(I, 18); P%(I, 14)

1915 Next I: LPrint Chr$(18): LPrint Chr$(12): GoTo 55

1920 '*** BUNT RATING CALCS ***
Cls
For I = 0 To 22
    If B$(I) = "" Or B$(I) = "XXX" Then 1950
    PA = B%(I, 11) + B%(I, 4)
    If PA = 0 Then PA = 1
    OB = (B%(I, 6) * 100 + PA / 2) / PA
    SLG = (B%(I, 6) + B%(I, 7) + B%(I, 8) * 2 + B%(I, 9) * 3) * 100
    SLG2 = (SLG + PA / 2) / PA
    R1 = (B%(I, 12) * 100 + PA / 2) / PA
    R1 = 11 - R1 ^ 2 / 25
    R2 = 11 - (SLG2 - OB) / 25
    BUNT = (R1 + R1 + R2) / 3
    B1 = B%(I, 15) * 1000 / PA
    If B1 <= 0 Then BR = 1: GoTo 1921
    If B1 <= 1 Then BR = 2: GoTo 1921
    If B1 <= 3 Then BR = 3: GoTo 1921
    If B1 <= 6 Then BR = 4: GoTo 1921
    If B1 <= 10 Then BR = 5: GoTo 1921
    If B1 <= 15 Then BR = 6: GoTo 1921
    If B1 <= 20 Then BR = 7: GoTo 1921
    If B1 <= 25 Then BR = 8: GoTo 1921
    If B1 <= 30 Then BR = 9: GoTo 1921
    If B1 <= 35 Then BR = 10: GoTo 1921
    BR = 10

    1921 BUNT = (BUNT + BR) / 2
    If BUNT < 1 Then BUNT = 1
    If BUNT > 9 Then BUNT = 9
    B%(I, 30) = Int(BUNT + .5)
    Print B$(I); Tab(20); B%(I, 30)

1950 Next

1955 Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
I$ = GetKeyPress$
Cls
For I = 0 To 21
    If P$(I) = "" Or P$(I) = "XXX" Then 1970
    PA = P%(I, 20) + P%(I, 27)
    If PA = 0 Then PA = 1
    OB = (P%(I, 22) * 100 + PA / 2) / PA
    SLG = (P%(I, 22) + P%(I, 23) + P%(I, 24) * 2 + P%(I, 25) * 3) * 100
    SLG2 = (SLG + PA / 2) / PA
    R1 = (P%(I, 28) * 100 + PA / 2) / PA
    R1 = 11 - R1 ^ 2 / 25
    If R1 < 1 Then R1 = 1
    If R1 > 10 Then R1 = 10
    R2 = 11 - (SLG2 - OB) / 25
    If R2 < 1 Then R2 = 1
    If R2 > 10 Then R2 = 10
    BUNT = (R1 + R1 + R2) / 3
    B1 = P%(I, 36) * 1000 / PA
    If B1 <= 0 Then BR = 1: GoTo 1961
    If B1 <= 1 Then BR = 2: GoTo 1961
    If B1 <= 3 Then BR = 3: GoTo 1961
    If B1 <= 6 Then BR = 4: GoTo 1961
    If B1 <= 10 Then BR = 5: GoTo 1961
    If B1 <= 15 Then BR = 6: GoTo 1961
    If B1 <= 20 Then BR = 7: GoTo 1961
    If B1 <= 25 Then BR = 8: GoTo 1961
    If B1 <= 30 Then BR = 9: GoTo 1961
    If B1 <= 35 Then BR = 10: GoTo 1961
    BR = 10

    1961 BUNT = (BUNT + BR) / 2
    If BUNT < 1 Then BUNT = 1
    If BUNT > 9 Then BUNT = 9
    P%(I, 32) = Int(BUNT + .5)
    Print P$(I); Tab(20); P%(I, 32)

1970 Next
Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
I$ = GetKeyPress$
GoTo 1385

1971 'RUN RATING
Cls
For I = 0 To 22
    If B$(I) = "" Or B$(I) = "XXX" Then 1975
    SPD = 200 * B%(I, 8) + B%(I, 13) * 100
    If B%(I, 6) + B%(I, 11) - B%(I, 7) - B%(I, 8) - B%(I, 9) = 0 Then RR = Int(Rnd(1) * 15) + 1: GoTo 1972
    RR = Int(SPD / (B%(I, 6) + B%(I, 11) - B%(I, 7) - B%(I, 8) - B%(I, 9)) + .5)
    RR = RR * 1.4

    1972 If RR <= 3 Then B%(I, 16) = 1
    If RR > 3 And RR <= 5 Then B%(I, 16) = 2
    If RR > 5 And RR <= 10 Then B%(I, 16) = 3
    If RR > 10 And RR <= 15 Then B%(I, 16) = 4
    If RR > 15 And RR <= 20 Then B%(I, 16) = 5
    If RR > 20 And RR <= 25 Then B%(I, 16) = 6
    If RR > 25 And RR <= 30 Then B%(I, 16) = 7
    If RR > 30 And RR <= 40 Then B%(I, 16) = 8
    If RR > 40 Then B%(I, 16) = 9
    Print B$(I); Tab(20); B%(I, 16)

1975 Next
Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
I$ = GetKeyPress$
For I = 0 To 21
    Cls
    If P$(I) = "" Or P$(I) = "XXX" Then 1985
    RR = Int(Rnd(1) * 20) + 1
    RR = RR * 1.4

    1982 If RR <= 4 Then P%(I, 31) = 1
    If RR > 4 And RR <= 8 Then P%(I, 31) = 2
    If RR > 8 And RR <= 12 Then P%(I, 31) = 3
    If RR > 12 And RR <= 16 Then P%(I, 31) = 4
    If RR > 16 Then P%(I, 31) = 5
    Print P$(I); Tab(20); P%(I, 31)

1985 Next
Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
I$ = GetKeyPress$
GoTo 1385

3000 TS = 0
Cls
Print "DOES "; T$; " HAVE COMPILED STATS (YN)";
3001 I$ = GetKeyPress$

3005 If UCase$(I$) = "Y" Then TS = 1

3010 If TS = 1 Then GoSub 4160

3055 Cls
Print "TEAM ROSTER FOR "; T$

3060 For I = 0 To 22
    Locate I + 3, 1
    Print Using "## "; I + 1;: Print B$(I);
Next

3061 For I = 0 To 21
    Locate I + 3, 25
    Print Using "## "; I + 1;
    Print P$(I);
Next

3065 Locate 10, 50: Print "1 - SWITCH BATTERS"

3066 Locate 11, 50: Print "2 - SWITCH PITCHERS"

3067 Locate 12, 50: Print "3 - SAVE CHANGES"

3068 Locate 13, 50: Print "4 - RETURN TO MENU (NO CHANGES)"
3070 I$ = GetKeyPress$

3071 I = Val(I$): If I < 1 Or I > 4 Then 3070

3075 If I = 4 Then 55

3080 If I = 3 Then GoSub 4260: GoTo 1385

3090 If I = 1 Then GoSub 3100: GoTo 3055

3095 GoSub 3200: GoTo 3055

3100 GoSub 3300

3105 Locate 16, 50: Input "BATTER #1 "; I1

3106 If I1 < 1 Or I1 > 23 Then 3105

3107 Locate 16, 60: Print B$(I1 - 1)

3108 Locate 17, 50: Input "BATTER #2 "; I2

3109 If I2 < 1 Or I2 > 23 Then 3108

3110 Locate 17, 60: Print B$(I2 - 1)

3115 Locate 18, 50: Print "ANY CHANGE (YN)";
3116 I$ = GetKeyPress$

3117 If UCase$(I$) = "Y" Then 3100

3120 Swap B$(I1 - 1), B$(I2 - 1): Swap BS$(I1 - 1), BS$(I2 - 1)

3121 For I = 0 To 79: Swap B%(I1 - 1, I), B%(I2 - 1, I): Next

3122 For I = 0 To 21: Swap B0%(I1 - 1, I), B0%(I2 - 1, I): Next
For I = 0 To 6: Swap SS%(I1 - 1, I), SS%(I2 - 1, I): Next

3124 Swap BS%(I1 - 1), BS%(I2 - 1)

3125 Return

3200 GoSub 3300

3205 Locate 16, 50: Input "PITCHER #1 "; I1

3206 If I1 < 1 Or I1 > 22 Then 3205

3207 Locate 16, 60: Print P$(I1 - 1)

3208 Locate 17, 50: Input "PITCHER #2 "; I2

3209 If I2 < 1 Or I2 > 22 Then 3208

3210 Locate 17, 60: Print P$(I2 - 1)

3215 Locate 18, 50: Print "ANY CHANGE (YN)";
3216 I$ = GetKeyPress$

3217 If UCase$(I$) = "Y" Then 3200

3220 Swap P$(I1 - 1), P$(I2 - 1): Swap PS$(I1 - 1), PS$(I2 - 1)

3221 For I = 0 To 88: Swap P%(I1 - 1, I), P%(I2 - 1, I): Next

3222 For I = 0 To 41: Swap P0%(I1 - 1, I), P0%(I2 - 1, I): Next

3224 Swap PS%(I1 - 1), PS%(I2 - 1)

3225 Return

3300 For I = 16 To 19: Locate I, 50: Print "                           ": Next: Return

4005 For xx = 4 To 11: If Mid$(ST$, xx, 1) = " " Or Mid$(ST$, xx, 1) = "." Then Mid$(ST$, xx, 1) = "_"

4010 Next

4015 QQ = 8: For xx = 11 To 4 Step -1: If Mid$(ST$, xx, 1) = "_" Then QQ = xx - 4: GoTo 4025

    4020 GoTo 4030

4025 Next xx

4030 Return

4065 Print "                                        ";: Return

4160 ST$ = T$
Cls

4162 GoSub 4005

4165 F$ = Mid$(ST$, 4, QQ)
R$ = F$ + "." + Left$(ST$, 2)

4170 If Not _FileExists(D$(1) + R$) Then GoSub 4550: TS = 0: GoTo 4210
Open D$(1) + R$ For Input As #1

4171 Input #1, A$: Input #1, W2%: Input #1, L2%

4175 For I = 0 To 22: Input #1, BS$(I): Input #1, BS%(I): Next I

4180 For I = 0 To 21: Input #1, PS$(I): Input #1, PS%(I): Next I

4185 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I

4190 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

4195 Input #1, D0%: Input #1, D1%
For I = 0 To 21: Input #1, T(I): Next I
For I = 0 To 22: Input #1, T1(I): Next I
Close #1
If Not _FileExists(D$(1) + R$ + "R") Then SS = 0: GoTo 4210
SS = 1
Open D$(1) + R$ + "R" For Input As #1
For I = 0 To 22
    For I1 = 0 To 6
        Input #1, SS%(I, I1)
Next I1: Next I
Close #1

4210 Return

4260 If TS = 0 Then 4297

4265 Cls

4270 Open D$(1) + R$ For Output As #1

4271 Print #1, A$: Print #1, W2%: Print #1, L2%

4275 For I = 0 To 22: Print #1, BS$(I): Print #1, BS%(I): Next I

4280 For I = 0 To 21: Print #1, PS$(I): Print #1, PS%(I): Next I

4285 For I = 0 To 22: For I1 = 0 To 21: Print #1, B0%(I, I1): Next I1: Next I

4290 For I = 0 To 21: For I1 = 0 To 41: Print #1, P0%(I, I1): Next I1: Next I

4295 Print #1, D0%: Print #1, D1%
For I = 0 To 21: Print #1, T(I): Next I
For I = 0 To 22: Print #1, T1(I): Next I

4296 Close 1
If SS = 0 Then 4297
Open D$(1) + R$ + "R" For Output As #1
For I = 0 To 22
    For I1 = 0 To 6
        Print #1, SS%(I, I1)
Next I1: Next I
Close #1

4297 Return

80 GoSub 5000: GoTo 500

81 Cls: Color 12: Print "TEAM OPTIONS": Print
Color 14: Print "(1) ";: Color 15: Print "VIEW HITTERS": Print
Color 14: Print "(2) ";: Color 15: Print "VIEW PITCHERS": Print
Color 14: Print "(3) ";: Color 15: Print "VIEW PITCHERS HITTING/FLDG STATS": Print
Color 14: Print "(4) ";: Color 15: Print "RETURN TO MAIN MENU"
82 I$ = GetKeyPress$
I = Val(I$): If I < 1 Or I > 4 Then 82
If I = 1 Then 83
If I = 2 Then 130
If I = 3 Then 700
If I = 4 Then 55

83 Cls: Color 14: Print "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"

96 For I = 0 To 22
    If I = 20 Then GoSub 5050 Else GoTo 97

    97 Color 3
    If B$(I) = "XXX" Then 120
    Print Left$(B$(I), 10);
    Color 7
    If B%(I, 0) = -1 Then Print Tab(12); "L";
    If B%(I, 0) = 0 Then Print Tab(12); "S";
    If B%(I, 0) = 1 Then Print Tab(12); "R";
    If B%(I, 0) = -2 Then Print Tab(12); "LP";
    If B%(I, 0) = 2 Then Print Tab(12); "RP";
    Print Tab(14); Using "#"; B%(I, 22);: If B%(I, 23) <> -1 Then Print Using "#"; B%(I, 23);: If B%(I, 24) <> -1 Then Print Using "#"; B%(I, 24);: If B%(I, 25) <> -1 Then Print Using "#"; B%(I, 25);
    Print Tab(20); Using "###"; B%(I, 3);
    Print Tab(24); Using "###"; B%(I, 4);
    Print Tab(28); Using "###"; B%(I, 5);
    Print Tab(32); Using "###"; B%(I, 6);
    Print Tab(36); Using "##"; B%(I, 7);
    Print Tab(39); Using "##"; B%(I, 8);
    Print Tab(42); Using "##"; B%(I, 9);
    Print Tab(45); Using "###"; B%(I, 10);
    Print Tab(49); Using "###"; B%(I, 11);
    Print Tab(53); Using "###"; B%(I, 12);
    Print Tab(57); Using "###"; B%(I, 13);
    Print Tab(61); Using "##"; B%(I, 14);
    Print Tab(64); Using "##"; B%(I, 18);
    I2 = 0: If B%(I, 4) > 0 Then I2 = Int((B%(I, 6) / B%(I, 4) + .0005) * 1000)

    117 If B%(I, 4) = 0 Or I2 = 0 Then Print Tab(68); ".000"
    If I2 > 0 And I2 < 100 Then Print Tab(68); ".0";: Print Tab(70); Using "##"; I2
    If I2 > 100 Then Print Tab(68); ".";: Print Tab(69); Using "###"; I2

120 Next: GoSub 4000

121 Color 11: Locate 20: Print "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
122 I$ = GetKeyPress$

123 If I$ = Chr$(9) Then 600

124 If I$ <> Chr$(9) Then 81

130 Cls: Color 12: Print T$
Color 14: Print "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"
app = 0

133 For I = 0 To 21
    If I = 18 Then GoSub 5025 Else GoTo 135

    135 Color 3
    If P$(I) = "XXX" Then 156
    app = app + P%(I, 4)
    Print Left$(P$(I), 10);

    136 Color 7
    If P%(I, 0) = -1 Then Print Tab(14); "L";
    If P%(I, 0) = 1 Then Print Tab(14); "R";

    137 Print Tab(16); Using "##"; P%(I, 1);

    138 Print Tab(19); Using "##"; P%(I, 2);

    139 Print Tab(22); Using "##"; P%(I, 3);

    140 Print Tab(25); Using "##"; P%(I, 4);

    141 Print Tab(28); Using "##"; P%(I, 5);

    142 Print Tab(31); Using "###"; P%(I, 6);

    143 Print Tab(35); Using "###"; P%(I, 7);

    144 Print Tab(39); Using "###"; P%(I, 8);
    Print Tab(43); Using "###"; P%(I, 9);

    146 Print Tab(46); Using "##.##"; P%(I, 10) / 100;

    148 Print Tab(53); Using "##"; P%(I, 12);

    149 Print Tab(56); Using "##"; P%(I, 13);

    150 Print Tab(59); Using "##"; P%(I, 16);

    151 Print Tab(62); Using "##"; P%(I, 33);

    152 Print Tab(66); Using "##"; P%(I, 18)

156 Next: GoSub 5020

157 Color 11: Locate 24: Print "HIT ANY KEY TO CONTINUE";
158 I$ = GetKeyPress$

159 GoTo 81

500 Cls

501 Color 12: Locate 5, 10: Print T$

502 Color 3: Locate 7, 10: Print "LEAGUE BATTING AVG.";: Locate 7, 38: Color 7: Print Using "#####"; L%(1)

503 Color 3: Locate , 10: Print "LEAGUE STRIKE OUT AVG.";: Locate , 38: Color 7: Print Using "#####"; L%(2)

504 Color 3: Locate , 10: Print "LEAGUE BASE ON BALLS AVG.";: Locate , 38: Color 7: Print Using "#####"; L%(3)
Color 3: Locate , 10: Print "LEAGUE HOME RUN AVG.";: Locate , 38: Color 7: Print Using "#####"; L%(4)

506 Color 3: Locate , 10: Print "DOUBLE PLAYS AVG.";: Locate , 38: Color 7: Print Using "#####"; L%(5)

507 Color 3: Locate , 10: Print "PARK DOUBLE ADJ.";: Locate , 38: Color 7: Print Using "#####"; L%(6)

508 Color 3: Locate , 10: Print "PARK TRIPLE ADJ.";: Locate , 38: Color 7: Print Using "#####"; L%(7)

509 Color 3: Locate , 10: Print "PARK HOME RUN ADJ.";: Locate , 37: Color 7: Print Using "######"; K9(0)

510 Color 3: Locate , 10: Print "PARK FOUL GROUND ADJ.";: Locate , 37: Color 7: Print Using "######"; L%(8)

520 Color 15: Locate 23, 10: Print "HIT ANY KEY TO CONTINUE"
521 I$ = GetKeyPress$

522 GoTo 81

600 Cls: Color 14: Print "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

602 For I = 0 To 22
    If I = 20 Then GoSub 5010 Else GoTo 603

    603 Color 3: Print Left$(B$(I), 10);
    Color 7
    Print Tab(13); Using "####"; B%(I, 26);
    Print Tab(19); Using "#"; B%(I, 33);

    606 Print Tab(23); Using "#"; B%(I, 34);

    607 Print Tab(26); Using "####"; B%(I, 27);

    608 Print Tab(32); Using "#"; B%(I, 35);

    609 Print Tab(36); Using "#"; B%(I, 36);

    610 Print Tab(39); Using "####"; B%(I, 28);

    611 Print Tab(45); Using "#"; B%(I, 37);

    612 Print Tab(49); Using "#"; B%(I, 38);

    613 Print Tab(52); Using "####"; B%(I, 29);

    614 Print Tab(58); Using "#"; B%(I, 39);
    Print Tab(62); Using "#"; B%(I, 40);

    616 Print Tab(66); Using "#"; B%(I, 16);

    617 Print Tab(71); Using "#"; B%(I, 30)

618 Next

621 Color 11: Locate 20: Print "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
622 I$ = GetKeyPress$

623 If I$ = Chr$(9) Then 83

624 If I$ <> Chr$(9) Then 81

700 Cls: Color 14: Print "PITCHER        G  AB   R   H DB TR HR RBI  BB  SO  SB CS BAVG  R B FAVG"

701 For I = 0 To 21

    702 Color 3: Print Left$(P$(I), 10);
    Color 7: Print Tab(14); Using "###"; P%(I, 19);
    Print Tab(18); Using "###"; P%(I, 20);
    Print Tab(22); Using "###"; P%(I, 21);
    Print Tab(26); Using "###"; P%(I, 22);

    703 Print Tab(30); Using "##"; P%(I, 23);

    704 Print Tab(33); Using "##"; P%(I, 24);
    Print Tab(36); Using "##"; P%(I, 25);
    Print Tab(39); Using "###"; P%(I, 26);

    707 Print Tab(43); Using "###"; P%(I, 27);

    708 Print Tab(47); Using "###"; P%(I, 28);

    709 Print Tab(52); Using "##"; P%(I, 29);

    710 Print Tab(55); Using "##"; P%(I, 30);

    714 If P%(I, 11) <> 999 Then Print Tab(59); Using ".###"; P%(I, 11) / 1000;: GoTo 715
    I2 = 0: If P%(I, 20) > 0 Then I2 = Int((P%(I, 22) / P%(I, 20) + .0005) * 1000)
    If P%(I, 20) = 0 Or I2 = 0 Then Print Tab(59); ".000";
    If I2 > 0 And I2 < 100 Then Print Tab(59); ".0";: Print Tab(61); Using "##"; I2;
    If I2 > 100 Then Print Tab(59); ".";: Print Tab(60); Using "###"; I2;

    715 Print Tab(64); Using "#"; P%(I, 31);

    716 Print Tab(66); Using "#"; P%(I, 32);

    717 Print Tab(68); Using "####"; P%(I, 17)

720 Next

721 Color 11: Locate 24: Print "HIT ANY KEY TO CONTINUE";
722 I$ = GetKeyPress$

723 GoTo 81

5000 For K = 0 To 15
    TB%(K) = 0
Next
For K = 0 To 33
    TP%(K) = 0
Next
For K = 4 To 18: For I = 0 To 22
    If B$(I) = "XXX" Then 3002
        TB%(K - 3) = TB%(K - 3) + B%(I, K)

3002 Next: Next
For K = 4 To 18: For I = 0 To 21
    If P$(I) = "XXX" Or P%(I, 11) <> 999 Then 2005
        TB%(K - 3) = TB%(K - 3) + P%(I, K + 16)

2005 Next: Next

3003 For K = 1 To 33: For I = 0 To 21
    If P$(I) = "XXX" Then 3006

        3004 TP%(K) = TP%(K) + P%(I, K)

3006 Next: Next
ERX = 0: For I = 0 To 21
    If P$(I) = "XXX" Then 3012

    3011 ERX = ERX + ((P%(I, 6) / 9) * (P%(I, 10) / 100))

3012 Next

3025 Return

4000 Print: Color 14: Print "TOTALS";: Locate , 23: Print Using "####    ####   ###    ###    ####    ###     .###"; TB%(1); TB%(3); TB%(5); TB%(7); TB%(9); TB%(11); TB%(3) / TB%(1)

4001 Locate , 27: Print Using "####   ####   ###     ###     ###"; TB%(2); TB%(4); TB%(6); TB%(8); TB%(10)
Return

5020 Print: Color 14: Print "TOTALS";: Locate , 14: Print Using "####  ####      ####    ####     #.##    ###   ###"; TP%(1); TP%(3); TP%(6); TP%(8); (ERX * 9) / TP%(6); TP%(13); TP%(33)

4021 Locate , 17: Print Using "####      ###    ####    ####            ###"; TP%(2); TP%(5); TP%(7); TP%(9); TP%(16)
Print "TOTAL APPEARANCES: "; app

4022 Return

5050 Locate 23: Print "HIT ANY KEY TO CONTINUE"
5001 I$ = GetKeyPress$

5002 Cls: Color 12: Print T$
Color 14: Print "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"

5003 Return

5010 Locate 23: Print "HIT ANY KEY TO CONTINUE"
5011 I$ = GetKeyPress$

5012 Cls: Color 12: Print T$
Color 14: Print "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

5013 Return

5025 Locate 23: Print "HIT ANY KEY TO CONTINUE"
5021 I$ = GetKeyPress$
Cls: Color 12: Print T$
Color 14: Print "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"

5023 Return

6000 For I = 1 To 15: TB%(I) = 0: Next

6001 For I = 1 To 33: TP%(I) = 0: Next

6002 Return

4500 Locate 10, 8
Call CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO SELECT ANOTHER ID", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4550 Locate 10, 8
Call CSMsgBox("NO COMPILED STATS WERE FOUND... PRESS ANY KEY TO RETURN TO CONTINUE", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'------------------------------
'     BATTEREDIT Subroutine
'------------------------------
' ...explanation...
Sub BATTEREDIT ()
    200 Color 15, 0
    Cls
    Print "EDIT/ENTER BATTERS"
    X% = 2
    For i = 0 To 10
        Locate i + 5, 5
        Print Using "##"; i + 1;: Print " "; b$(i); Tab(40);
        Print Using "##"; i + 13;: Print " "; b$(i + 12)
    Next
    Locate , 5: Print Using "##"; 12;: Print " "; b$(11)
    Print
    ':FOR i = 0 TO 22
    ':FOR i1 = 22 TO 25
    ':IF (b%(i, i1) > 0 AND b%(i, i1 + 4) = 0) OR b%(i, 4) = 0 THEN PRINT b$(i)
    ':NEXT
    ':NEXT

    855 Locate 22, 1
    Print "# TO ENTER/EDIT (1-23) (0 TO CONTINUE) ";: Input i
    If i < 0 Or i > 23 Then 855
    i = i - 1
    If i < 0 Then 300
    ': INITIALIZE BATTER SCREEN

    Color 15, 1: Cls: Locate 1, 30: Print "EDIT AND INPUT BATTERS"
    Locate 3, 1: Print "NAME:"
    Locate 3, 24: Print "BATS:"
    Locate 3, 36: Print "ACTIVE:"
    Locate 3, 50: Print "INJ LENGTH:"
    Locate 5, 14: Print " G  AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
    Locate 6, 1: Print "SEASON:"
    Locate 7, 1: Print "VS LHP:"
    Locate 8, 1: Print "VS RHP:"
    Locate 10, 33: Print "FIELDING RATINGS"
    Locate 11, 31: Print "POS  FAVG  ARM RANGE"
    Locate 12, 19: Print "POSITION 1"
    Locate 13, 19: Print "POSITION 2"
    Locate 14, 19: Print "POSITION 3"
    Locate 15, 19: Print "POSITION 4"
    Locate 17, 12: Print "RUN (1-9):"
    Locate 17, 27: Print "BUNT (1-9):"
    Locate 17, 44: Print "G/A:"
    Locate 17, 54: Print "LEAGUE BAVG:"
    Locate 20, 29: Print "SPLIT STATS LEAGUE DATA"
    Locate 21, 26: Print "BAVG   K%  BB%  HR%  2B%  3B%"
    Locate 22, 13: Print "LHB vs LHP"
    Locate 23, 13: Print "LHB vs RHP"
    Locate 24, 13: Print "RHB vs LHP";
    Locate 25, 13: Print "RHB vs RHP";

    ': INITIALIZE BATTER FIELDS
    'Colr = 112
    FG = 0
    BG = 7
    Color 0, 7
    Locate 3, 7: QPrint PadRight$(b$(i), 15), FG, BG, -1
    If b%(i, 0) = 2 Then BATS$ = "RP"
    If b%(i, 0) = -2 Then BATS$ = "LP"
    If b%(i, 0) = 1 Then BATS$ = "R"
    If b%(i, 0) = -1 Then BATS$ = "L"
    If b%(i, 0) = 0 Then BATS$ = "S"
    Locate 3, 30: QPrint PadRight$(BATS$, 2), FG, BG, -1
    ACT$ = "Y"
    If b%(i, 31) = 1 Then ACT$ = "N"
    Locate 3, 44: Print ACT$
    Locate 3, 62: Print Using "###"; INJ%(i)
    Locate 6, 12: Print Using "###"; b%(i, 3)
    Locate 6, 17: Print Using "###"; b%(i, 4)
    Locate 6, 22: Print Using "###"; b%(i, 5)
    Locate 6, 27: Print Using "###"; b%(i, 6)
    Locate 6, 32: Print Using "##"; b%(i, 7)
    Locate 6, 36: Print Using "##"; b%(i, 8)
    Locate 6, 40: Print Using "##"; b%(i, 9)
    Locate 6, 44: Print Using "###"; b%(i, 10)
    Locate 6, 49: Print Using "###"; b%(i, 11)
    Locate 6, 54: Print Using "###"; b%(i, 12)
    Locate 6, 59: Print Using "###"; b%(i, 13)
    Locate 6, 64: Print Using "##"; b%(i, 14)
    Locate 6, 68: Print Using "##"; b%(i, 15)

    Locate 7, 17: Print Using "###"; b%(i, 42)
    Locate 7, 27: Print Using "###"; b%(i, 43)
    Locate 7, 32: Print Using "##"; b%(i, 44)
    Locate 7, 36: Print Using "##"; b%(i, 45)
    Locate 7, 40: Print Using "##"; b%(i, 46)
    Locate 7, 49: Print Using "###"; b%(i, 47)
    Locate 7, 54: Print Using "###"; b%(i, 48)

    Locate 8, 17: Print Using "###"; b%(i, 49)
    Locate 8, 27: Print Using "###"; b%(i, 50)
    Locate 8, 32: Print Using "##"; b%(i, 51)
    Locate 8, 36: Print Using "##"; b%(i, 52)
    Locate 8, 40: Print Using "##"; b%(i, 53)
    Locate 8, 49: Print Using "###"; b%(i, 54)
    Locate 8, 54: Print Using "###"; b%(i, 55)

    Locate 12, 31: Print Using "##"; b%(i, 22)
    Locate 12, 36: Print Using "####"; b%(i, 26)
    Locate 12, 43: Print Using "#"; b%(i, 33)
    Locate 12, 47: Print Using "#"; b%(i, 34)

    Locate 13, 31: Print Using "##"; b%(i, 23)
    Locate 13, 36: Print Using "####"; b%(i, 27)
    Locate 13, 43: Print Using "#"; b%(i, 35)
    Locate 13, 47: Print Using "#"; b%(i, 36)

    Locate 14, 31: Print Using "##"; b%(i, 24)
    Locate 14, 36: Print Using "####"; b%(i, 28)
    Locate 14, 43: Print Using "#"; b%(i, 37)
    Locate 14, 47: Print Using "#"; b%(i, 38)

    Locate 15, 31: Print Using "##"; b%(i, 25)
    Locate 15, 36: Print Using "####"; b%(i, 29)
    Locate 15, 43: Print Using "#"; b%(i, 39)
    Locate 15, 47: Print Using "#"; b%(i, 40)

    Locate 17, 23: Print Using "#"; b%(i, 16)
    Locate 17, 39: Print Using "#"; b%(i, 30)
    Locate 17, 49: Print Using "##"; b%(i, 18)
    Locate 17, 67: Print Using "###"; b%(i, 20)

    Locate 22, 27: Print Using "###"; b%(i, 56)
    Locate 22, 32: Print Using "###"; b%(i, 57)
    Locate 22, 37: Print Using "###"; b%(i, 58)
    Locate 22, 42: Print Using "###"; b%(i, 59)
    Locate 22, 47: Print Using "###"; b%(i, 60)
    Locate 22, 52: Print Using "###"; b%(i, 61)
    Locate 23, 27: Print Using "###"; b%(i, 62)
    Locate 23, 32: Print Using "###"; b%(i, 63)
    Locate 23, 37: Print Using "###"; b%(i, 64)
    Locate 23, 42: Print Using "###"; b%(i, 65)
    Locate 23, 47: Print Using "###"; b%(i, 66)
    Locate 23, 52: Print Using "###"; b%(i, 67)
    Locate 24, 27: Print Using "###"; b%(i, 68);
    Locate 24, 32: Print Using "###"; b%(i, 69);
    Locate 24, 37: Print Using "###"; b%(i, 70);
    Locate 24, 42: Print Using "###"; b%(i, 71);
    Locate 24, 47: Print Using "###"; b%(i, 72);
    Locate 24, 52: Print Using "###"; b%(i, 73);
    Locate 25, 27: Print Using "###"; b%(i, 74);
    Locate 25, 32: Print Using "###"; b%(i, 75);
    Locate 25, 37: Print Using "###"; b%(i, 76);
    Locate 25, 42: Print Using "###"; b%(i, 77);
    Locate 25, 47: Print Using "###"; b%(i, 78);
    Locate 25, 52: Print Using "###"; b%(i, 79);


    BName:
    Locate 3, 7
    Max = 15
    Call TextIn(b$(i), Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo BName:
    If XCode = 2 GoTo bDone

    BATS:
    Locate 3, 30
    Max = 2
    Call TextIn(BATS$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo BName:
    If XCode = 2 GoTo bDone

    BActive:
    Locate 3, 44
    Max = 1
    Call TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo BATS:
    If XCode = 2 GoTo bDone

    BINJ:
    Locate 3, 62
    Max = 3
    Call NumIn(INJ%(i), Max, XCode, FG, BG)
    Locate 3, 62: Print Using "###"; INJ%(i)
    If XCode = 1 GoTo BActive:
    If XCode = 2 GoTo bDone

    BGAMES:
    Locate 6, 12
    Max = 3
    Call NumIn(b%(i, 3), Max, XCode, FG, BG)
    Locate 6, 12: Print Using "###"; b%(i, 3)
    If XCode = 1 GoTo BINJ:
    If XCode = 2 GoTo bDone

    BAB:
    Locate 6, 17
    Max = 3
    Call NumIn(b%(i, 4), Max, XCode, FG, BG)
    Locate 6, 17: Print Using "###"; b%(i, 4)
    If XCode = 1 GoTo BGAMES:
    If XCode = 2 GoTo bDone

    BR:
    Locate 6, 22
    Max = 3
    Call NumIn(b%(i, 5), Max, XCode, FG, BG)
    Locate 6, 22: Print Using "###"; b%(i, 5)
    If XCode = 1 GoTo BAB:
    If XCode = 2 GoTo bDone

    BH:
    Locate 6, 27
    Max = 3
    Call NumIn(b%(i, 6), Max, XCode, FG, BG)
    Locate 6, 27: Print Using "###"; b%(i, 6)
    If XCode = 1 GoTo BR:
    If XCode = 2 GoTo bDone

    B2B:
    Locate 6, 32
    Max = 2
    Call NumIn(b%(i, 7), Max, XCode, FG, BG)
    Locate 6, 32: Print Using "##"; b%(i, 7)
    If XCode = 1 GoTo BH:
    If XCode = 2 GoTo bDone

    B3B:
    Locate 6, 36
    Max = 2
    Call NumIn(b%(i, 8), Max, XCode, FG, BG)
    Locate 6, 36: Print Using "##"; b%(i, 8)
    If XCode = 1 GoTo B2B:
    If XCode = 2 GoTo bDone

    bHR:
    Locate 6, 40
    Max = 2
    Call NumIn(b%(i, 9), Max, XCode, FG, BG)
    Locate 6, 40: Print Using "##"; b%(i, 9)
    If XCode = 1 GoTo B3B:
    If XCode = 2 GoTo bDone

    BRBI:
    Locate 6, 44
    Max = 3
    Call NumIn(b%(i, 10), Max, XCode, FG, BG)
    Locate 6, 44: Print Using "###"; b%(i, 10)
    If XCode = 1 GoTo bHR:
    If XCode = 2 GoTo bDone

    bBB:
    Locate 6, 49
    Max = 3
    Call NumIn(b%(i, 11), Max, XCode, FG, BG)
    Locate 6, 49: Print Using "###"; b%(i, 11)
    If XCode = 1 GoTo BRBI:
    If XCode = 2 GoTo bDone

    bKO:
    Locate 6, 54
    Max = 3
    Call NumIn(b%(i, 12), Max, XCode, FG, BG)
    Locate 6, 54: Print Using "###"; b%(i, 12)
    If XCode = 1 GoTo bBB:
    If XCode = 2 GoTo bDone

    bSB:
    Locate 6, 59
    Max = 3
    Call NumIn(b%(i, 13), Max, XCode, FG, BG)
    Locate 6, 59: Print Using "###"; b%(i, 13)
    If XCode = 1 GoTo bKO:
    If XCode = 2 GoTo bDone

    bCS:
    Locate 6, 64
    Max = 2
    Call NumIn(b%(i, 14), Max, XCode, FG, BG)
    Locate 6, 64: Print Using "##"; b%(i, 14)
    If XCode = 1 GoTo bSB:
    If XCode = 2 GoTo bDone

    BSH:
    Locate 6, 68
    Max = 2
    Call NumIn(b%(i, 15), Max, XCode, FG, BG)
    Locate 6, 68: Print Using "##"; b%(i, 15)
    If XCode = 1 GoTo bCS:
    If XCode = 2 GoTo bDone

    BLAB:
    Locate 7, 17
    Max = 3
    Call NumIn(b%(i, 42), Max, XCode, FG, BG)
    Locate 7, 17: Print Using "###"; b%(i, 42)
    If XCode = 1 GoTo BSH:
    If XCode = 2 GoTo bDone

    BLH:
    Locate 7, 27
    Max = 3
    Call NumIn(b%(i, 43), Max, XCode, FG, BG)
    Locate 7, 27: Print Using "###"; b%(i, 43)
    If XCode = 1 GoTo BLAB:
    If XCode = 2 GoTo bDone

    BL2B:
    Locate 7, 32
    Max = 2
    Call NumIn(b%(i, 44), Max, XCode, FG, BG)
    Locate 7, 32: Print Using "##"; b%(i, 44)
    If XCode = 1 GoTo BLH:
    If XCode = 2 GoTo bDone

    BL3B:
    Locate 7, 36
    Max = 2
    Call NumIn(b%(i, 45), Max, XCode, FG, BG)
    Locate 7, 36: Print Using "##"; b%(i, 45)
    If XCode = 1 GoTo BL2B:
    If XCode = 2 GoTo bDone

    BLHR:
    Locate 7, 40
    Max = 2
    Call NumIn(b%(i, 46), Max, XCode, FG, BG)
    Locate 7, 40: Print Using "##"; b%(i, 46)
    If XCode = 1 GoTo BL3B:
    If XCode = 2 GoTo bDone

    bLBB:
    Locate 7, 49
    Max = 3
    Call NumIn(b%(i, 47), Max, XCode, FG, BG)
    Locate 7, 49: Print Using "###"; b%(i, 47)
    If XCode = 1 GoTo BLHR:
    If XCode = 2 GoTo bDone

    BLKO:
    Locate 7, 54
    Max = 3
    Call NumIn(b%(i, 48), Max, XCode, FG, BG)
    Locate 7, 54: Print Using "###"; b%(i, 48)
    If XCode = 1 GoTo bLBB:
    If XCode = 2 GoTo bDone

    BRAB:
    Locate 8, 17
    Max = 3
    Call NumIn(b%(i, 49), Max, XCode, FG, BG)
    Locate 8, 17: Print Using "###"; b%(i, 49)
    If XCode = 1 GoTo BLKO:
    If XCode = 2 GoTo bDone

    BRH:
    Locate 8, 27
    Max = 3
    Call NumIn(b%(i, 50), Max, XCode, FG, BG)
    Locate 8, 27: Print Using "###"; b%(i, 50)
    If XCode = 1 GoTo BRAB:
    If XCode = 2 GoTo bDone

    BR2B:
    Locate 8, 32
    Max = 2
    Call NumIn(b%(i, 51), Max, XCode, FG, BG)
    Locate 8, 32: Print Using "##"; b%(i, 51)
    If XCode = 1 GoTo BRH:
    If XCode = 2 GoTo bDone

    BR3B:
    Locate 8, 36
    Max = 2
    Call NumIn(b%(i, 52), Max, XCode, FG, BG)
    Locate 8, 36: Print Using "##"; b%(i, 52)
    If XCode = 1 GoTo BR2B:
    If XCode = 2 GoTo bDone

    BRHR:
    Locate 8, 40
    Max = 2
    Call NumIn(b%(i, 53), Max, XCode, FG, BG)
    Locate 8, 40: Print Using "##"; b%(i, 53)
    If XCode = 1 GoTo BR3B:
    If XCode = 2 GoTo bDone

    bRBB:
    Locate 8, 49
    Max = 3
    Call NumIn(b%(i, 54), Max, XCode, FG, BG)
    Locate 8, 49: Print Using "###"; b%(i, 54)
    If XCode = 1 GoTo BRHR:
    If XCode = 2 GoTo bDone

    BRKO:
    Locate 8, 54
    Max = 3
    Call NumIn(b%(i, 55), Max, XCode, FG, BG)
    Locate 8, 54: Print Using "###"; b%(i, 55)
    If XCode = 1 GoTo bRBB:
    If XCode = 2 GoTo bDone

    BP1:
    Locate 12, 31
    Max = 2
    Call NumIn(b%(i, 22), Max, XCode, FG, BG)
    Locate 12, 31: Print Using "##"; b%(i, 22)
    If XCode = 1 GoTo BRKO:
    If XCode = 2 GoTo bDone

    ': STOP HERE

    BF1:
    Locate 12, 36
    Max = 4
    Call NumIn(b%(i, 26), Max, XCode, FG, BG)
    Locate 12, 36: Print Using "####"; b%(i, 26)
    If XCode = 1 GoTo BP1:
    If XCode = 2 GoTo bDone

    bA1:
    Locate 12, 43
    Max = 1
    Call NumIn(b%(i, 33), Max, XCode, FG, BG)
    Locate 12, 43: Print Using "#"; b%(i, 33)
    If XCode = 1 GoTo BF1:
    If XCode = 2 GoTo bDone

    BR1:
    Locate 12, 47
    Max = 1
    Call NumIn(b%(i, 34), Max, XCode, FG, BG)
    Locate 12, 47: Print Using "#"; b%(i, 34)
    If XCode = 1 GoTo bA1:
    If XCode = 2 GoTo bDone

    BP2:
    Locate 13, 31
    Max = 2
    Call NumIn(b%(i, 23), Max, XCode, FG, BG)
    Locate 13, 31: Print Using "##"; b%(i, 23)
    If XCode = 1 GoTo BR1:
    If XCode = 2 GoTo bDone

    BF2:
    Locate 13, 36
    Max = 4
    Call NumIn(b%(i, 27), Max, XCode, FG, BG)
    Locate 13, 36: Print Using "####"; b%(i, 27)
    If XCode = 1 GoTo BP2:
    If XCode = 2 GoTo bDone

    bA2:
    Locate 13, 43
    Max = 1
    Call NumIn(b%(i, 35), Max, XCode, FG, BG)
    Locate 13, 43: Print Using "#"; b%(i, 35)
    If XCode = 1 GoTo BF2:
    If XCode = 2 GoTo bDone

    BR2:
    Locate 13, 47
    Max = 1
    Call NumIn(b%(i, 36), Max, XCode, FG, BG)
    Locate 13, 47: Print Using "#"; b%(i, 36)
    If XCode = 1 GoTo bA2:
    If XCode = 2 GoTo bDone

    BP3:
    Locate 14, 31
    Max = 2
    Call NumIn(b%(i, 24), Max, XCode, FG, BG)
    Locate 14, 31: Print Using "##"; b%(i, 24)
    If XCode = 1 GoTo BR2:
    If XCode = 2 GoTo bDone

    BF3:
    Locate 14, 36
    Max = 4
    Call NumIn(b%(i, 28), Max, XCode, FG, BG)
    Locate 14, 36: Print Using "####"; b%(i, 28)
    If XCode = 1 GoTo BP3:
    If XCode = 2 GoTo bDone

    bA3:
    Locate 14, 43
    Max = 1
    Call NumIn(b%(i, 37), Max, XCode, FG, BG)
    Locate 14, 43: Print Using "#"; b%(i, 37)
    If XCode = 1 GoTo BF3:
    If XCode = 2 GoTo bDone

    BR3:
    Locate 14, 47
    Max = 1
    Call NumIn(b%(i, 38), Max, XCode, FG, BG)
    Locate 14, 47: Print Using "#"; b%(i, 38)
    If XCode = 1 GoTo bA3:
    If XCode = 2 GoTo bDone

    BP4:
    Locate 15, 31
    Max = 2
    Call NumIn(b%(i, 25), Max, XCode, FG, BG)
    Locate 15, 31: Print Using "##"; b%(i, 25)
    If XCode = 1 GoTo BR3:
    If XCode = 2 GoTo bDone

    BF4:
    Locate 15, 36
    Max = 4
    Call NumIn(b%(i, 29), Max, XCode, FG, BG)
    Locate 15, 36: Print Using "####"; b%(i, 29)
    If XCode = 1 GoTo BP4:
    If XCode = 2 GoTo bDone

    bA4:
    Locate 15, 43
    Max = 1
    Call NumIn(b%(i, 39), Max, XCode, FG, BG)
    Locate 15, 43: Print Using "#"; b%(i, 39)
    If XCode = 1 GoTo BF4:
    If XCode = 2 GoTo bDone

    bR4:
    Locate 15, 47
    Max = 1
    Call NumIn(b%(i, 40), Max, XCode, FG, BG)
    Locate 15, 47: Print Using "#"; b%(i, 40)
    If XCode = 1 GoTo bA4:
    If XCode = 2 GoTo bDone

    bRUN:
    Locate 17, 23
    Max = 1
    Call NumIn(b%(i, 16), Max, XCode, FG, BG)
    Locate 17, 23: Print Using "#"; b%(i, 16)
    If XCode = 1 GoTo bR4:
    If XCode = 2 GoTo bDone

    bBUNT:
    Locate 17, 39
    Max = 1
    Call NumIn(b%(i, 30), Max, XCode, FG, BG)
    Locate 17, 39: Print Using "#"; b%(i, 30)
    If XCode = 1 GoTo bRUN:
    If XCode = 2 GoTo bDone

    bGA:
    Locate 17, 49
    Max = 2
    Call NumIn(b%(i, 18), Max, XCode, FG, BG)
    Locate 17, 49: Print Using "##"; b%(i, 18)
    If XCode = 1 GoTo bBUNT:
    If XCode = 2 GoTo bDone

    bLA:
    Locate 17, 67
    Max = 3
    Call NumIn(b%(i, 20), Max, XCode, FG, BG)
    Locate 17, 67: Print Using "###"; b%(i, 20)
    If XCode = 1 GoTo bGA:
    If XCode = 2 GoTo bDone

    bLLBA:
    Locate 22, 27
    Max = 3
    Call NumIn(b%(i, 56), Max, XCode, FG, BG)
    Locate 22, 27: Print Using "###"; b%(i, 56)
    If XCode = 1 GoTo bLA:
    If XCode = 2 GoTo bDone

    bLL2B:
    Locate 22, 32
    Max = 3
    Call NumIn(b%(i, 57), Max, XCode, FG, BG)
    Locate 22, 32: Print Using "###"; b%(i, 57)
    If XCode = 1 GoTo bLLBA:
    If XCode = 2 GoTo bDone

    bLL3B:
    Locate 22, 37
    Max = 3
    Call NumIn(b%(i, 58), Max, XCode, FG, BG)
    Locate 22, 37: Print Using "###"; b%(i, 58)
    If XCode = 1 GoTo bLL2B:
    If XCode = 2 GoTo bDone

    bLLHR:
    Locate 22, 42
    Max = 3
    Call NumIn(b%(i, 59), Max, XCode, FG, BG)
    Locate 22, 42: Print Using "###"; b%(i, 59)
    If XCode = 1 GoTo bLL3B:
    If XCode = 2 GoTo bDone

    bLLBB:
    Locate 22, 47
    Max = 3
    Call NumIn(b%(i, 60), Max, XCode, FG, BG)
    Locate 22, 47: Print Using "###"; b%(i, 60)
    If XCode = 1 GoTo bLLHR:
    If XCode = 2 GoTo bDone

    bLLKO:
    Locate 22, 52
    Max = 3
    Call NumIn(b%(i, 61), Max, XCode, FG, BG)
    Locate 22, 52: Print Using "###"; b%(i, 61)
    If XCode = 1 GoTo bLLBB:
    If XCode = 2 GoTo bDone

    bLRBA:
    Locate 23, 27
    Max = 3
    Call NumIn(b%(i, 62), Max, XCode, FG, BG)
    Locate 23, 27: Print Using "###"; b%(i, 62)
    If XCode = 1 GoTo bLLKO:
    If XCode = 2 GoTo bDone

    bLR2B:
    Locate 23, 32
    Max = 3
    Call NumIn(b%(i, 63), Max, XCode, FG, BG)
    Locate 23, 32: Print Using "###"; b%(i, 63)
    If XCode = 1 GoTo bLRBA:
    If XCode = 2 GoTo bDone

    bLR3B:
    Locate 23, 37
    Max = 3
    Call NumIn(b%(i, 64), Max, XCode, FG, BG)
    Locate 23, 37: Print Using "###"; b%(i, 64)
    If XCode = 1 GoTo bLR2B:
    If XCode = 2 GoTo bDone

    bLRHR:
    Locate 23, 42
    Max = 3
    Call NumIn(b%(i, 65), Max, XCode, FG, BG)
    Locate 23, 42: Print Using "###"; b%(i, 65)
    If XCode = 1 GoTo bLR3B:
    If XCode = 2 GoTo bDone

    bLRBB:
    Locate 23, 47
    Max = 3
    Call NumIn(b%(i, 66), Max, XCode, FG, BG)
    Locate 23, 47: Print Using "###"; b%(i, 66)
    If XCode = 1 GoTo bLRHR:
    If XCode = 2 GoTo bDone

    bLRKO:
    Locate 23, 52
    Max = 3
    Call NumIn(b%(i, 67), Max, XCode, FG, BG)
    Locate 23, 52: Print Using "###"; b%(i, 67)
    If XCode = 1 GoTo bLRBB:
    If XCode = 2 GoTo bDone

    bRLBA:
    Locate 24, 27
    Max = 3
    Call NumIn(b%(i, 68), Max, XCode, FG, BG)
    Locate 24, 27: Print Using "###"; b%(i, 68);
    If XCode = 1 GoTo bLRKO:
    If XCode = 2 GoTo bDone

    bRL2B:
    Locate 24, 32
    Max = 3
    Call NumIn(b%(i, 69), Max, XCode, FG, BG)
    Locate 24, 32: Print Using "###"; b%(i, 69);
    If XCode = 1 GoTo bRLBA:
    If XCode = 2 GoTo bDone

    bRL3B:
    Locate 24, 37
    Max = 3
    Call NumIn(b%(i, 70), Max, XCode, FG, BG)
    Locate 24, 37: Print Using "###"; b%(i, 70);
    If XCode = 1 GoTo bRL2B:
    If XCode = 2 GoTo bDone

    bRLHR:
    Locate 24, 42
    Max = 3
    Call NumIn(b%(i, 71), Max, XCode, FG, BG)
    Locate 24, 42: Print Using "###"; b%(i, 71);
    If XCode = 1 GoTo bRL3B:
    If XCode = 2 GoTo bDone

    bRLBB:
    Locate 24, 47
    Max = 3
    Call NumIn(b%(i, 72), Max, XCode, FG, BG)
    Locate 24, 47: Print Using "###"; b%(i, 72);
    If XCode = 1 GoTo bRLHR:
    If XCode = 2 GoTo bDone

    bRLKO:
    Locate 24, 52
    Max = 3
    Call NumIn(b%(i, 73), Max, XCode, FG, BG)
    Locate 24, 52: Print Using "###"; b%(i, 73);
    If XCode = 1 GoTo bRLBB:
    If XCode = 2 GoTo bDone

    bRRBA:
    Locate 25, 27
    Max = 3
    Call NumIn(b%(i, 74), Max, XCode, FG, BG)
    Locate 25, 27: Print Using "###"; b%(i, 74);
    If XCode = 1 GoTo bRLKO:
    If XCode = 2 GoTo bDone

    bRR2B:
    Locate 25, 32
    Max = 3
    Call NumIn(b%(i, 75), Max, XCode, FG, BG)
    Locate 25, 32: Print Using "###"; b%(i, 75);
    If XCode = 1 GoTo bRRBA:
    If XCode = 2 GoTo bDone

    bRR3B:
    Locate 25, 37
    Max = 3
    Call NumIn(b%(i, 76), Max, XCode, FG, BG)
    Locate 25, 37: Print Using "###"; b%(i, 76);
    If XCode = 1 GoTo bRR2B:
    If XCode = 2 GoTo bDone

    bRRHR:
    Locate 25, 42
    Max = 3
    Call NumIn(b%(i, 77), Max, XCode, FG, BG)
    Locate 25, 42: Print Using "###"; b%(i, 77);
    If XCode = 1 GoTo bRR3B:
    If XCode = 2 GoTo bDone

    bRRBB:
    Locate 25, 47
    Max = 3
    Call NumIn(b%(i, 78), Max, XCode, FG, BG)
    Locate 25, 47: Print Using "###"; b%(i, 78);
    If XCode = 1 GoTo bRRHR:
    If XCode = 2 GoTo bDone

    bRRKO:
    Locate 25, 52
    Max = 3
    Call NumIn(b%(i, 79), Max, XCode, FG, BG)
    Locate 25, 52: Print Using "###"; b%(i, 79);
    If XCode = 1 GoTo bRRBB:

    bDone:
    Color 15, 1
    Locate 25, 1
    Print "Is this okay? (Yes/No) ";

    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    If XCode = 1 GoTo bRRKO
    If X$ = "N" GoTo BName
    If X$ <> "Y" GoTo bDone 'insist on either Y or N or up-arrow

    b%(i, 0) = 0
    BATS$ = _Trim$(BATS$)
    If UCase$(BATS$) = "L" Then b%(i, 0) = -1
    If UCase$(BATS$) = "R" Then b%(i, 0) = 1
    If UCase$(BATS$) = "LP" Then b%(i, 0) = -2
    If UCase$(BATS$) = "RP" Then b%(i, 0) = 2
    ACT$ = _Trim$(ACT$)
    b%(i, 31) = 0
    If UCase$(ACT$) = "N" Then b%(i, 31) = 1
    GoTo 200
    300 ':
End Sub


'------------------------------
'     EDITTEAM Subroutine
'------------------------------
' ...explanation...
Sub EDITTEAM (YN$, T$, NN$, S$, M$, T)
    Color 15, 1: Cls: Locate 1, 36: Print "TEAM DATA"
    Locate 3, 1: Print "TEAM NUMBER:"
    Locate 5, 1: Print "LEAGUE BATTING AVERAGE:"
    Locate 6, 1: Print "LEAGUE STRIKEOUT %    :"
    Locate 7, 1: Print "LEAGUE WALK PERCENTAGE:"
    Locate 8, 1: Print "LEAGUE HOME RUN %     :"
    Locate 10, 1: Print "DBL PLAYS/GAME X 100  :"
    Locate 14, 1: Print "PARK NAME:"
    Locate 15, 1: Print "PARK ID  :"
    Locate 16, 1: Print "DOUBLES RATING  :"
    Locate 17, 1: Print "TRIPLES RATING  :"
    Locate 18, 1: Print "HOME RUN RATING :"
    Locate 19, 1: Print "FOUL BALL RATING:"
    Locate 21, 1: Print "OUTDOOR(0)/INDOOR(1):"
    Locate 22, 1: Print "GRASS(0)/TURF(1)    :"
    Locate 24, 1: Print "MANAGER :";
    Locate 18, 29: Print "TEXT COLOR      :"
    Locate 19, 29: Print "BACKGROUND COLOR:"
    Locate 3, 25: Print "DISK ID:"
    Locate 3, 44: Print "TEAM NAME:"
    Locate 4, 44: Print "ABBREVIATION:"
    Locate 6, 51: Print "SPLIT STATS LEAGUE DATA"
    Locate 7, 50: Print "BAVG   K%  BB%  HR%  2B%  3B%"
    Locate 8, 38: Print "LHB vs LHP"
    Locate 9, 38: Print "LHB vs RHP"
    Locate 10, 38: Print "RHB vs LHP"
    Locate 11, 38: Print "RHB vs RHP"

    ': INITIALIZE THE FIELDS
    Color 0, 7
    COLR = 112
    Locate 3, 14: Print Using "###"; T
    Locate 3, 33: Print YN$
    Locate 3, 55: Print T$
    Locate 4, 58: Print NN$
    Locate 5, 25: Print Using "###"; L%(1)
    Locate 6, 25: Print Using "###"; L%(2)
    Locate 7, 25: Print Using "###"; L%(3)
    Locate 8, 25: Print Using "###"; L%(4)
    Locate 10, 25: Print Using "###"; L%(5)
    Locate 14, 12: Print S$
    Locate 15, 12: Print Using "###"; L%(13)
    Locate 16, 19: Print Using "###"; L%(6)
    Locate 17, 19: Print Using "###"; L%(7)
    Locate 18, 19: Print Using "######"; K9(0)
    Locate 19, 19: Print Using "###"; L%(8)
    Locate 21, 25: Print Using "#"; L%(9)
    Locate 22, 25: Print Using "#"; L%(10)
    Locate 24, 11: Print M$;
    Locate 18, 46: Print Using "##"; L%(11)
    Locate 19, 46: Print Using "##"; L%(12)
    Locate 8, 50: Print Using "###"; SA%(1)
    Locate 8, 55: Print Using "###"; SA%(2)
    Locate 8, 60: Print Using "###"; SA%(3)
    Locate 8, 65: Print Using "###"; SA%(4)
    Locate 8, 70: Print Using "###"; SA%(5)
    Locate 8, 75: Print Using "###"; SA%(6)
    Locate 9, 50: Print Using "###"; SA%(7)
    Locate 9, 55: Print Using "###"; SA%(8)
    Locate 9, 60: Print Using "###"; SA%(9)
    Locate 9, 65: Print Using "###"; SA%(10)
    Locate 9, 70: Print Using "###"; SA%(11)
    Locate 9, 75: Print Using "###"; SA%(12)
    Locate 10, 50: Print Using "###"; SA%(13)
    Locate 10, 55: Print Using "###"; SA%(14)
    Locate 10, 60: Print Using "###"; SA%(15)
    Locate 10, 65: Print Using "###"; SA%(16)
    Locate 10, 70: Print Using "###"; SA%(17)
    Locate 10, 75: Print Using "###"; SA%(18)
    Locate 11, 50: Print Using "###"; SA%(19)
    Locate 11, 55: Print Using "###"; SA%(20)
    Locate 11, 60: Print Using "###"; SA%(21)
    Locate 11, 65: Print Using "###"; SA%(22)
    Locate 11, 70: Print Using "###"; SA%(23)
    Locate 11, 75: Print Using "###"; SA%(24)
    Locate 25, 1: Print "ESC-WILL END EDITING";
    HR$ = _Trim$(Str$(K9(0)))
    TeamNum:
    Locate 3, 14
    Max = 3
    Call NumIn(T, Max, XCode, FG, BG)
    Locate 3, 14: Print Using "###"; T
    If XCode = 1 GoTo TeamNum:
    If XCode = 2 GoTo Done

    DiskID:
    Locate 3, 33
    Max = 3
    Call TextIn(YN$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo TeamNum:
    If XCode = 2 GoTo Done

    TeamName:
    Locate 3, 55
    Max = 15
    Call TextIn(T$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo DiskID:
    If XCode = 2 GoTo Done

    Abbrev:
    Locate 4, 58
    Max = 5
    Call TextIn(NN$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo TeamName:
    If XCode = 2 GoTo Done

    LgBA:
    Locate 5, 25
    Max = 3
    Call NumIn(L%(1), Max, XCode, FG, BG)
    Locate 5, 25: Print Using "###"; L%(1)
    If XCode = 1 GoTo Abbrev:
    If XCode = 2 GoTo Done

    LgKO:
    Locate 6, 25
    Max = 3
    Call NumIn(L%(2), Max, XCode, FG, BG)
    Locate 6, 25: Print Using "###"; L%(2)
    If XCode = 1 GoTo LgBA:
    If XCode = 2 GoTo Done

    LgBB:
    Locate 7, 25
    Max = 3
    Call NumIn(L%(3), Max, XCode, FG, BG)
    Locate 7, 25: Print Using "###"; L%(3)
    If XCode = 1 GoTo LgKO:
    If XCode = 2 GoTo Done

    LgHR:
    Locate 8, 25
    Max = 3
    Call NumIn(L%(4), Max, XCode, FG, BG)
    Locate 8, 25: Print Using "###"; L%(4)
    If XCode = 1 GoTo LgBB:
    If XCode = 2 GoTo Done

    LgDP:
    Locate 10, 25
    Max = 3
    Call NumIn(L%(5), Max, XCode, FG, BG)
    Locate 10, 25: Print Using "###"; L%(5)
    If XCode = 1 GoTo LgHR:
    If XCode = 2 GoTo Done

    ParkName:
    Locate 14, 12
    Max = 40
    Call TextIn(S$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo LgDP:
    If XCode = 2 GoTo Done

    ParkID:
    Locate 15, 12
    Max = 3
    Call NumIn(L%(13), Max, XCode, FG, BG)
    Locate 15, 12: Print Using "###"; L%(13)
    If XCode = 1 GoTo ParkName:
    If XCode = 2 GoTo Done

    DoubleRating:
    Locate 16, 19
    Max = 3
    Call NumIn(L%(6), Max, XCode, FG, BG)
    Locate 16, 19: Print Using "###"; L%(6)
    If XCode = 1 GoTo ParkID:
    If XCode = 2 GoTo Done

    TripleRating:
    Locate 17, 19
    Max = 3
    Call NumIn(L%(7), Max, XCode, FG, BG)
    Locate 17, 19: Print Using "###"; L%(7)
    If XCode = 1 GoTo DoubleRating:
    If XCode = 2 GoTo Done

    Homer:
    Locate 18, 19
    Max = 6
    Call TextIn(HR$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo TripleRating:
    If XCode = 2 GoTo Done

    Foul:
    Locate 19, 19
    Max = 3
    Call NumIn(L%(8), Max, XCode, FG, BG)
    Locate 19, 19: Print Using "###"; L%(8)
    If XCode = 1 GoTo Homer:
    If XCode = 2 GoTo Done

    Outdoor:
    Locate 21, 25
    Max = 1
    Call NumIn(L%(9), Max, XCode, FG, BG)
    Locate 21, 25: Print Using "#"; L%(9)
    If XCode = 1 GoTo Foul:
    If XCode = 2 GoTo Done

    Grass:
    Locate 22, 25
    Max = 1
    Call NumIn(L%(10), Max, XCode, FG, BG)
    Locate 22, 25: Print Using "#"; L%(10)
    If XCode = 1 GoTo Outdoor:
    If XCode = 2 GoTo Done

    Manager:
    Locate 24, 11
    Max = 15
    Call TextIn(M$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo Grass:
    If XCode = 2 GoTo Done

    Text:
    Locate 18, 46
    Max = 2
    Call NumIn(L%(11), Max, XCode, FG, BG)
    Locate 18, 46: Print Using "##"; L%(11)
    If XCode = 1 GoTo Manager:
    If XCode = 2 GoTo Done

    Bgrnd:
    Locate 19, 46
    Max = 2
    Call NumIn(L%(12), Max, XCode, FG, BG)
    Locate 19, 46: Print Using "##"; L%(12)
    If XCode = 1 GoTo Text:
    If XCode = 2 GoTo Done

    Locate 20, 29: Color L%(11), L%(12): Print "SAMPLE TEXT": Color 0, 7

    LLBA:
    Locate 8, 50
    Max = 3
    Call NumIn(SA%(1), Max, XCode, FG, BG)
    Locate 8, 50: Print Using "###"; SA%(1)
    If XCode = 1 GoTo Bgrnd:
    If XCode = 2 GoTo Done

    LL2B:
    Locate 8, 55
    Max = 3
    Call NumIn(SA%(2), Max, XCode, FG, BG)
    Locate 8, 55: Print Using "###"; SA%(2)
    If XCode = 1 GoTo LLBA:
    If XCode = 2 GoTo Done

    LL3B:
    Locate 8, 60
    Max = 3
    Call NumIn(SA%(3), Max, XCode, FG, BG)
    Locate 8, 60: Print Using "###"; SA%(3)
    If XCode = 1 GoTo LL2B:
    If XCode = 2 GoTo Done

    LLHR:
    Locate 8, 65
    Max = 3
    Call NumIn(SA%(4), Max, XCode, FG, BG)
    Locate 8, 65: Print Using "###"; SA%(4)
    If XCode = 1 GoTo LL3B:
    If XCode = 2 GoTo Done

    LLBB:
    Locate 8, 70
    Max = 3
    Call NumIn(SA%(5), Max, XCode, FG, BG)
    Locate 8, 70: Print Using "###"; SA%(5)
    If XCode = 1 GoTo LLHR:
    If XCode = 2 GoTo Done

    LLKO:
    Locate 8, 75
    Max = 3
    Call NumIn(SA%(6), Max, XCode, FG, BG)
    Locate 8, 75: Print Using "###"; SA%(6)
    If XCode = 1 GoTo LLBB:
    If XCode = 2 GoTo Done

    LRBA:
    Locate 9, 50
    Max = 3
    Call NumIn(SA%(7), Max, XCode, FG, BG)
    Locate 9, 50: Print Using "###"; SA%(7)
    If XCode = 1 GoTo LLKO:
    If XCode = 2 GoTo Done

    LR2B:
    Locate 9, 55
    Max = 3
    Call NumIn(SA%(8), Max, XCode, FG, BG)
    Locate 9, 55: Print Using "###"; SA%(8)
    If XCode = 1 GoTo LRBA:
    If XCode = 2 GoTo Done

    LR3B:
    Locate 9, 60
    Max = 3
    Call NumIn(SA%(9), Max, XCode, FG, BG)
    Locate 9, 60: Print Using "###"; SA%(9)
    If XCode = 1 GoTo LR2B:
    If XCode = 2 GoTo Done

    LRHR:
    Locate 9, 65
    Max = 3
    Call NumIn(SA%(10), Max, XCode, FG, BG)
    Locate 9, 65: Print Using "###"; SA%(10)
    If XCode = 1 GoTo LR3B:
    If XCode = 2 GoTo Done

    LRBB:
    Locate 9, 70
    Max = 3
    Call NumIn(SA%(11), Max, XCode, FG, BG)
    Locate 9, 70: Print Using "###"; SA%(11)
    If XCode = 1 GoTo LRHR:
    If XCode = 2 GoTo Done

    LRKO:
    Locate 9, 75
    Max = 3
    Call NumIn(SA%(12), Max, XCode, FG, BG)
    Locate 9, 75: Print Using "###"; SA%(12)
    If XCode = 1 GoTo LRBB:
    If XCode = 2 GoTo Done

    RLBA:
    Locate 10, 50
    Max = 3
    Call NumIn(SA%(13), Max, XCode, FG, BG)
    Locate 10, 50: Print Using "###"; SA%(13)
    If XCode = 1 GoTo LRKO:
    If XCode = 2 GoTo Done

    RL2B:
    Locate 10, 55
    Max = 3
    Call NumIn(SA%(14), Max, XCode, FG, BG)
    Locate 10, 55: Print Using "###"; SA%(14)
    If XCode = 1 GoTo RLBA:
    If XCode = 2 GoTo Done

    RL3B:
    Locate 10, 60
    Max = 3
    Call NumIn(SA%(15), Max, XCode, FG, BG)
    Locate 10, 60: Print Using "###"; SA%(15)
    If XCode = 1 GoTo RL2B:
    If XCode = 2 GoTo Done

    RLHR:
    Locate 10, 65
    Max = 3
    Call NumIn(SA%(16), Max, XCode, FG, BG)
    Locate 10, 65: Print Using "###"; SA%(16)
    If XCode = 1 GoTo RL3B:
    If XCode = 2 GoTo Done

    RLBB:
    Locate 10, 70
    Max = 3
    Call NumIn(SA%(17), Max, XCode, FG, BG)
    Locate 10, 70: Print Using "###"; SA%(17)
    If XCode = 1 GoTo RLHR:
    If XCode = 2 GoTo Done

    RLKO:
    Locate 10, 75
    Max = 3
    Call NumIn(SA%(18), Max, XCode, FG, BG)
    Locate 10, 75: Print Using "###"; SA%(18)
    If XCode = 1 GoTo RLBB:
    If XCode = 2 GoTo Done

    RRBA:
    Locate 11, 50
    Max = 3
    Call NumIn(SA%(19), Max, XCode, FG, BG)
    Locate 11, 50: Print Using "###"; SA%(19)
    If XCode = 1 GoTo RLKO:
    If XCode = 2 GoTo Done

    RR2B:
    Locate 11, 55
    Max = 3
    Call NumIn(SA%(20), Max, XCode, FG, BG)
    Locate 11, 55: Print Using "###"; SA%(20)
    If XCode = 1 GoTo RRBA:
    If XCode = 2 GoTo Done

    RR3B:
    Locate 11, 60
    Max = 3
    Call NumIn(SA%(21), Max, XCode, FG, BG)
    Locate 11, 60: Print Using "###"; SA%(21)
    If XCode = 1 GoTo RR2B:
    If XCode = 2 GoTo Done

    RRHR:
    Locate 11, 65
    Max = 3
    Call NumIn(SA%(22), Max, XCode, FG, BG)
    Locate 11, 65: Print Using "###"; SA%(22)
    If XCode = 1 GoTo RR3B:
    If XCode = 2 GoTo Done

    RRBB:
    Locate 11, 70
    Max = 3
    Call NumIn(SA%(23), Max, XCode, FG, BG)
    Locate 11, 70: Print Using "###"; SA%(23)
    If XCode = 1 GoTo RRHR:
    If XCode = 2 GoTo Done

    RRKO:
    Locate 11, 75
    Max = 3
    Call NumIn(SA%(24), Max, XCode, FG, BG)
    Locate 11, 75: Print Using "###"; SA%(24)
    If XCode = 1 GoTo RRBB:

    Done:

    Locate 25, 1
    Print "Is this okay? (Y)es/(N)o/(A)bort ";

    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ':ClearEOL 30
    If XCode = 1 GoTo RRKO
    If X$ = "N" GoTo TeamNum
    If X$ = "A" Then Run "HELLO.EXE"
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow
    K9(0) = Val(HR$)
End Sub


'------------------------------
'     PITCHEREDIT Subroutine
'------------------------------
' ...explanation...
Sub PITCHEREDIT ()
    1150 Color 15, 0:
    Cls
    Print "EDIT/ENTER PITCHERS"
    X% = 2
    For i = 0 To 10
        Locate i + 5, 5: Print Using "##"; i + 1;: Print " "; p$(i); Tab(40);
        Print Using "##"; i + 12;: Print " "; p$(i + 11)
    Next
    1175 Locate 22, 1
    Print "# TO EDIT/ENTER (1-22) (0 TO CONTINUE) ";
    Input i
    If i < 0 Or i > 22 Then 1175
    i = i - 1
    1195 If i < 0 Then 1180

    ': INITIALIZE PITCHER SCREEN

    Color 15, 1: Cls
    Locate 1, 1: Print "NAME:"
    Locate 1, 24: Print "THROWS:"
    Locate 1, 36: Print "ACTIVE:"
    Locate 1, 48: Print "INJ LENGTH:"
    Locate 1, 66: Print "HOLD (1-9):"
    Locate 3, 34: Print "PITCHING STATS"
    Locate 4, 11: Print "W   L  SV    G  GS  CG   IP    H   BB    K  WP   ERA  HRA"
    Locate 5, 1: Print "SEASON:"
    Locate 6, 27: Print "G/A:"
    Locate 6, 37: Print "LEAGUE BAVG:"
    Locate 8, 31: Print "SPLIT STATS AGAINST"
    Locate 9, 26: Print "AB    H  2B  3B  HR   BB    K"
    Locate 10, 1: Print "VS LHB:"
    Locate 11, 1: Print "VS RHB:"
    Locate 13, 30: Print "HITTING STATISTICS"
    Locate 14, 9: Print "RUN (1-9):"
    Locate 14, 24: Print "BUNT (1-9):"
    Locate 14, 39: Print "BATTING AVG:"
    Locate 14, 58: Print "FLDG AVG:"
    Locate 15, 14: Print "G   AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
    Locate 16, 1: Print "SEASON:"
    Locate 17, 1: Print "VS LHP:"
    Locate 18, 1: Print "VS RHP:"

    Locate 20, 29: Print "SPLIT STATS LEAGUE DATA"
    Locate 21, 26: Print "BAVG   K%  BB%  HR%  2B%  3B%"
    Locate 22, 13: Print "LHB vs LHP"
    Locate 23, 13: Print "LHB vs RHP"
    Locate 24, 13: Print "RHB vs LHP";
    Locate 25, 13: Print "RHB vs RHP";

    ': INITIALIZE PITCHER FIELDS
    FG = 0
    BG = 7
    Locate 1, 7: QPrint PadRight$(p$(i), 15), FG, BG, -1
    THROW$ = "R"
    If p%(i, 0) = -1 Then THROW$ = "L"
    Locate 1, 32: QPrint PadRight$(THROW$, 1), FG, BG, -1
    ACT$ = "Y"
    If p%(i, 34) = 1 Then ACT$ = "N"
    Locate 1, 44: QPrint PadRight$(ACT$, 1), FG, BG, -1
    Call PITCHINIT(i, Colr)
    Call PITCHINIT2(i, Colr)


    PName:
    Locate 1, 7
    CapsOn = 0: NumOnly = 0: Max = 15
    Call TextIn(p$(i), Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo PName:
    If XCode = 2 GoTo PDone

    THROW:
    Locate 1, 32
    Max = 1
    Call TextIn(THROW$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo PName:
    If XCode = 2 GoTo PDone

    PActive:
    Locate 1, 44
    Max = 1
    Call TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
    If XCode = 1 GoTo THROW:
    If XCode = 2 GoTo PDone

    PINJ:
    Locate 1, 60
    Max = 3
    Call NumIn(p%(i, 35), Max, XCode, FG, BG)
    Locate 1, 60: Print Using "###"; p%(i, 35)
    If XCode = 1 GoTo PActive:
    If XCode = 2 GoTo PDone

    PHOLD:
    Locate 1, 78
    Max = 1
    Call NumIn(p%(i, 18), Max, XCode, FG, BG)
    Locate 1, 78: Print Using "#"; p%(i, 18)
    If XCode = 1 GoTo PINJ:
    If XCode = 2 GoTo PDone


    PWINS:
    Locate 5, 10
    Max = 2
    Call NumIn(p%(i, 1), Max, XCode, FG, BG)
    Locate 5, 10: Print Using "##"; p%(i, 1)
    If XCode = 1 GoTo PHOLD:
    If XCode = 2 GoTo PDone

    PLOSSES:
    Locate 5, 14
    Max = 2
    Call NumIn(p%(i, 2), Max, XCode, FG, BG)
    Locate 5, 14: Print Using "##"; p%(i, 2)
    If XCode = 1 GoTo PWINS:
    If XCode = 2 GoTo PDone

    PSAVES:
    Locate 5, 18
    Max = 2
    Call NumIn(p%(i, 3), Max, XCode, FG, BG)
    Locate 5, 18: Print Using "##"; p%(i, 3)
    If XCode = 1 GoTo PLOSSES:
    If XCode = 2 GoTo PDone

    PGAMES:
    Locate 5, 22
    Max = 3
    Call NumIn(p%(i, 4), Max, XCode, FG, BG)
    Locate 5, 22: Print Using "###"; p%(i, 4)
    If XCode = 1 GoTo PSAVES:
    If XCode = 2 GoTo PDone

    PSTARTS:
    Locate 5, 27
    Max = 2
    Call NumIn(p%(i, 5), Max, XCode, FG, BG)
    Locate 5, 27: Print Using "##"; p%(i, 5)
    If XCode = 1 GoTo PGAMES:
    If XCode = 2 GoTo PDone

    PCG:
    Locate 5, 31
    Max = 2
    Call NumIn(p%(i, 16), Max, XCode, FG, BG)
    Locate 5, 31: Print Using "##"; p%(i, 16)
    If XCode = 1 GoTo PSTARTS:
    If XCode = 2 GoTo PDone

    PIP:
    Locate 5, 35
    Max = 3
    Call NumIn(p%(i, 6), Max, XCode, FG, BG)
    Locate 5, 35: Print Using "###"; p%(i, 6)
    If XCode = 1 GoTo PCG:
    If XCode = 2 GoTo PDone

    PH:
    Locate 5, 40
    Max = 3
    Call NumIn(p%(i, 7), Max, XCode, FG, BG)
    Locate 5, 40: Print Using "###"; p%(i, 7)
    If XCode = 1 GoTo PIP:
    If XCode = 2 GoTo PDone

    PBB:
    Locate 5, 45
    Max = 3
    Call NumIn(p%(i, 8), Max, XCode, FG, BG)
    Locate 5, 45: Print Using "###"; p%(i, 8)
    If XCode = 1 GoTo PH:
    If XCode = 2 GoTo PDone

    PKO:
    Locate 5, 50
    Max = 3
    Call NumIn(p%(i, 9), Max, XCode, FG, BG)
    Locate 5, 50: Print Using "###"; p%(i, 9)
    If XCode = 1 GoTo PBB:
    If XCode = 2 GoTo PDone

    PWP:
    Locate 5, 55
    Max = 2
    Call NumIn(p%(i, 33), Max, XCode, FG, BG)
    Locate 5, 55: Print Using "##"; p%(i, 33)
    If XCode = 1 GoTo PKO:
    If XCode = 2 GoTo PDone

    PERA:
    Locate 5, 59
    Max = 4
    Call NumIn(p%(i, 10), Max, XCode, FG, BG)
    Locate 5, 59: Print Using "####"; p%(i, 10)
    If XCode = 1 GoTo PWP:
    If XCode = 2 GoTo PDone

    PHRA:
    Locate 5, 66
    Max = 2
    Call NumIn(p%(i, 13), Max, XCode, FG, BG)
    Locate 5, 66: Print Using "##"; p%(i, 13)
    If XCode = 1 GoTo PERA:
    If XCode = 2 GoTo PDone

    PGA:
    Locate 6, 32
    Max = 2
    Call NumIn(p%(i, 12), Max, XCode, FG, BG)
    Locate 6, 32: Print Using "##"; p%(i, 12)
    If XCode = 1 GoTo PHRA:
    If XCode = 2 GoTo PDone

    PLBA:
    Locate 6, 50
    Max = 3
    Call NumIn(p%(i, 14), Max, XCode, FG, BG)
    Locate 6, 50: Print Using "###"; p%(i, 14)
    If XCode = 1 GoTo PHRA:
    If XCode = 2 GoTo PDone

    PLAB:
    Locate 10, 25
    Max = 3
    Call NumIn(p%(i, 51), Max, XCode, FG, BG)
    Locate 10, 25: Print Using "###"; p%(i, 51)
    If XCode = 1 GoTo PLBA:
    If XCode = 2 GoTo PDone

    PLH:
    Locate 10, 30
    Max = 3
    Call NumIn(p%(i, 52), Max, XCode, FG, BG)
    Locate 10, 30: Print Using "###"; p%(i, 52)
    If XCode = 1 GoTo PLAB:
    If XCode = 2 GoTo PDone

    PL2B:
    Locate 10, 35
    Max = 2
    Call NumIn(p%(i, 53), Max, XCode, FG, BG)
    Locate 10, 35: Print Using "##"; p%(i, 53)
    If XCode = 1 GoTo PLH:
    If XCode = 2 GoTo PDone

    PL3B:
    Locate 10, 39
    Max = 2
    Call NumIn(p%(i, 54), Max, XCode, FG, BG)
    Locate 10, 39: Print Using "##"; p%(i, 54)
    If XCode = 1 GoTo PL2B:
    If XCode = 2 GoTo PDone

    PLHR:
    Locate 10, 43
    Max = 2
    Call NumIn(p%(i, 55), Max, XCode, FG, BG)
    Locate 10, 43: Print Using "##"; p%(i, 55)
    If XCode = 1 GoTo PL3B:
    If XCode = 2 GoTo PDone

    PLBB:
    Locate 10, 47
    Max = 3
    Call NumIn(p%(i, 56), Max, XCode, FG, BG)
    Locate 10, 47: Print Using "###"; p%(i, 56)
    If XCode = 1 GoTo PLHR:
    If XCode = 2 GoTo PDone

    PLKO:
    Locate 10, 52
    Max = 3
    Call NumIn(p%(i, 57), Max, XCode, FG, BG)
    Locate 10, 52: Print Using "###"; p%(i, 57)
    If XCode = 1 GoTo PLBB:
    If XCode = 2 GoTo PDone

    PRAB:
    Locate 11, 25
    Max = 3
    Call NumIn(p%(i, 58), Max, XCode, FG, BG)
    Locate 11, 25: Print Using "###"; p%(i, 58)
    If XCode = 1 GoTo PLKO:
    If XCode = 2 GoTo PDone

    PRH:
    Locate 11, 30
    Max = 3
    Call NumIn(p%(i, 59), Max, XCode, FG, BG)
    Locate 11, 30: Print Using "###"; p%(i, 59)
    If XCode = 1 GoTo PRAB:
    If XCode = 2 GoTo PDone

    PR2B:
    Locate 11, 35
    Max = 2
    Call NumIn(p%(i, 60), Max, XCode, FG, BG)
    Locate 11, 35: Print Using "##"; p%(i, 60)
    If XCode = 1 GoTo PRH:
    If XCode = 2 GoTo PDone

    PR3B:
    Locate 11, 39
    Max = 2
    Call NumIn(p%(i, 61), Max, XCode, FG, BG)
    Locate 11, 39: Print Using "##"; p%(i, 61)
    If XCode = 1 GoTo PR2B:
    If XCode = 2 GoTo PDone

    PRHR:
    Locate 11, 43
    Max = 2
    Call NumIn(p%(i, 62), Max, XCode, FG, BG)
    Locate 11, 43: Print Using "##"; p%(i, 62)
    If XCode = 1 GoTo PR3B:
    If XCode = 2 GoTo PDone

    PRBB:
    Locate 11, 47
    Max = 3
    Call NumIn(p%(i, 63), Max, XCode, FG, BG)
    Locate 11, 47: Print Using "###"; p%(i, 63)
    If XCode = 1 GoTo PRHR:
    If XCode = 2 GoTo PDone

    PRKO:
    Locate 11, 52
    Max = 3
    Call NumIn(p%(i, 64), Max, XCode, FG, BG)
    Locate 11, 52: Print Using "###"; p%(i, 64)
    If XCode = 1 GoTo PRBB:
    If XCode = 2 GoTo PDone

    PRUN:
    Locate 14, 20
    Max = 1
    Call NumIn(p%(i, 31), Max, XCode, FG, BG)
    Locate 14, 20: Print Using "#"; p%(i, 31)
    If XCode = 1 GoTo PRKO:
    If XCode = 2 GoTo PDone

    PBUNT:
    Locate 14, 36
    Max = 1
    Call NumIn(p%(i, 32), Max, XCode, FG, BG)
    Locate 14, 36: Print Using "#"; p%(i, 32)
    If XCode = 1 GoTo PRKO:
    If XCode = 2 GoTo PDone

    PBAVG:
    Locate 14, 52
    Max = 3
    Call NumIn(p%(i, 11), Max, XCode, FG, BG)
    Locate 14, 52: Print Using "###"; p%(i, 11)
    If XCode = 1 GoTo PBUNT:
    If XCode = 2 GoTo PDone

    PFAVG:
    Locate 14, 68
    Max = 4
    Call NumIn(p%(i, 17), Max, XCode, FG, BG)
    Locate 14, 68: Print Using "####"; p%(i, 17)
    If XCode = 1 GoTo PBAVG:
    If XCode = 2 GoTo PDone

    PBGAMES:
    Locate 16, 12
    Max = 3
    Call NumIn(p%(i, 19), Max, XCode, FG, BG)
    Locate 16, 12: Print Using "###"; p%(i, 19)
    If XCode = 1 GoTo PFAVG:
    If XCode = 2 GoTo PDone

    PAB:
    Locate 16, 17
    Max = 3
    Call NumIn(p%(i, 20), Max, XCode, FG, BG)
    Locate 16, 17: Print Using "###"; p%(i, 20)
    If XCode = 1 GoTo PBGAMES:
    If XCode = 2 GoTo PDone

    PR:
    Locate 16, 22
    Max = 3
    Call NumIn(p%(i, 21), Max, XCode, FG, BG)
    Locate 16, 22: Print Using "###"; p%(i, 21)
    If XCode = 1 GoTo PAB:
    If XCode = 2 GoTo PDone

    PBH:
    Locate 16, 27
    Max = 3
    Call NumIn(p%(i, 22), Max, XCode, FG, BG)
    Locate 16, 27: Print Using "###"; p%(i, 22)
    If XCode = 1 GoTo PR:
    If XCode = 2 GoTo PDone

    P2B:
    Locate 16, 32
    Max = 2
    Call NumIn(p%(i, 23), Max, XCode, FG, BG)
    Locate 16, 32: Print Using "##"; p%(i, 23)
    If XCode = 1 GoTo PBH:
    If XCode = 2 GoTo PDone

    P3B:
    Locate 16, 36
    Max = 2
    Call NumIn(p%(i, 24), Max, XCode, FG, BG)
    Locate 16, 36: Print Using "##"; p%(i, 24)
    If XCode = 1 GoTo P2B:
    If XCode = 2 GoTo PDone

    PHR:
    Locate 16, 40
    Max = 2
    Call NumIn(p%(i, 25), Max, XCode, FG, BG)
    Locate 16, 40: Print Using "##"; p%(i, 25)
    If XCode = 1 GoTo P3B:
    If XCode = 2 GoTo PDone

    PRBI:
    Locate 16, 44
    Max = 3
    Call NumIn(p%(i, 26), Max, XCode, FG, BG)
    Locate 16, 44: Print Using "###"; p%(i, 26)
    If XCode = 1 GoTo PHR:
    If XCode = 2 GoTo PDone

    PBBB:
    Locate 16, 49
    Max = 3
    Call NumIn(p%(i, 27), Max, XCode, FG, BG)
    Locate 16, 49: Print Using "###"; p%(i, 27)
    If XCode = 1 GoTo PRBI:
    If XCode = 2 GoTo PDone

    PBKO:
    Locate 16, 54
    Max = 3
    Call NumIn(p%(i, 28), Max, XCode, FG, BG)
    Locate 16, 54: Print Using "###"; p%(i, 28)
    If XCode = 1 GoTo PBBB:
    If XCode = 2 GoTo PDone

    PSB:
    Locate 16, 59
    Max = 3
    Call NumIn(p%(i, 29), Max, XCode, FG, BG)
    Locate 16, 59: Print Using "###"; p%(i, 29)
    If XCode = 1 GoTo PBKO:
    If XCode = 2 GoTo PDone

    PCS:
    Locate 16, 64
    Max = 2
    Call NumIn(p%(i, 30), Max, XCode, FG, BG)
    Locate 16, 64: Print Using "##"; p%(i, 30)
    If XCode = 1 GoTo PSB:
    If XCode = 2 GoTo PDone

    PSH:
    Locate 16, 68
    Max = 2
    Call NumIn(p%(i, 36), Max, XCode, FG, BG)
    Locate 16, 68: Print Using "##"; p%(i, 36)
    If XCode = 1 GoTo PCS:
    If XCode = 2 GoTo PDone

    PPLAB:
    Locate 17, 17
    Max = 3
    Call NumIn(p%(i, 37), Max, XCode, FG, BG)
    Locate 17, 17: Print Using "###"; p%(i, 37)
    If XCode = 1 GoTo PSH:
    If XCode = 2 GoTo PDone

    PPLH:
    Locate 17, 27
    Max = 3
    Call NumIn(p%(i, 38), Max, XCode, FG, BG)
    Locate 17, 27: Print Using "###"; p%(i, 38)
    If XCode = 1 GoTo PPLAB:
    If XCode = 2 GoTo PDone

    PPL2B:
    Locate 17, 32
    Max = 2
    Call NumIn(p%(i, 39), Max, XCode, FG, BG)
    Locate 17, 32: Print Using "##"; p%(i, 39)
    If XCode = 1 GoTo PPLH:
    If XCode = 2 GoTo PDone

    PPL3B:
    Locate 17, 36
    Max = 2
    Call NumIn(p%(i, 40), Max, XCode, FG, BG)
    Locate 17, 36: Print Using "##"; p%(i, 40)
    If XCode = 1 GoTo PPL2B:
    If XCode = 2 GoTo PDone

    PPLHR:
    Locate 17, 40
    Max = 2
    Call NumIn(p%(i, 41), Max, XCode, FG, BG)
    Locate 17, 40: Print Using "##"; p%(i, 41)
    If XCode = 1 GoTo PPL3B:
    If XCode = 2 GoTo PDone

    PPLBB:
    Locate 17, 49
    Max = 3
    Call NumIn(p%(i, 42), Max, XCode, FG, BG)
    Locate 17, 49: Print Using "###"; p%(i, 42)
    If XCode = 1 GoTo PPLHR:
    If XCode = 2 GoTo PDone

    PPLKO:
    Locate 17, 54
    Max = 3
    Call NumIn(p%(i, 43), Max, XCode, FG, BG)
    Locate 17, 54: Print Using "###"; p%(i, 43)
    If XCode = 1 GoTo PPLBB:
    If XCode = 2 GoTo PDone

    PPRAB:
    Locate 18, 17
    Max = 3
    Call NumIn(p%(i, 44), Max, XCode, FG, BG)
    Locate 18, 17: Print Using "###"; p%(i, 44)
    If XCode = 1 GoTo PPLKO:
    If XCode = 2 GoTo PDone

    PPRH:
    Locate 18, 27
    Max = 3
    Call NumIn(p%(i, 45), Max, XCode, FG, BG)
    Locate 18, 27: Print Using "###"; p%(i, 45)
    If XCode = 1 GoTo PPRAB:
    If XCode = 2 GoTo PDone

    PPR2B:
    Locate 18, 32
    Max = 2
    Call NumIn(p%(i, 46), Max, XCode, FG, BG)
    Locate 18, 32: Print Using "##"; p%(i, 46)
    If XCode = 1 GoTo PPRH:
    If XCode = 2 GoTo PDone

    PPR3B:
    Locate 18, 36
    Max = 2
    Call NumIn(p%(i, 47), Max, XCode, FG, BG)
    Locate 18, 36: Print Using "##"; p%(i, 47)
    If XCode = 1 GoTo PPR2B:
    If XCode = 2 GoTo PDone

    PPRHR:
    Locate 18, 40
    Max = 2
    Call NumIn(p%(i, 48), Max, XCode, FG, BG)
    Locate 18, 40: Print Using "##"; p%(i, 48)
    If XCode = 1 GoTo PPR3B:
    If XCode = 2 GoTo PDone

    PPRBB:
    Locate 18, 49
    Max = 3
    Call NumIn(p%(i, 49), Max, XCode, FG, BG)
    Locate 18, 49: Print Using "###"; p%(i, 49)
    If XCode = 1 GoTo PPRHR:
    If XCode = 2 GoTo PDone

    PPRKO:
    Locate 18, 54
    Max = 3
    Call NumIn(p%(i, 50), Max, XCode, FG, BG)
    Locate 18, 54: Print Using "###"; p%(i, 50)
    If XCode = 1 GoTo PPRBB:
    If XCode = 2 GoTo PDone

    PLLBA:
    Locate 22, 27
    Max = 3
    Call NumIn(p%(i, 65), Max, XCode, FG, BG)
    Locate 22, 27: Print Using "###"; p%(i, 65)
    If XCode = 1 GoTo PPRKO:
    If XCode = 2 GoTo PDone

    PLL2B:
    Locate 22, 32
    Max = 3
    Call NumIn(p%(i, 66), Max, XCode, FG, BG)
    Locate 22, 32: Print Using "###"; p%(i, 66)
    If XCode = 1 GoTo PLLBA:
    If XCode = 2 GoTo PDone

    PLL3B:
    Locate 22, 37
    Max = 3
    Call NumIn(p%(i, 67), Max, XCode, FG, BG)
    Locate 22, 37: Print Using "###"; p%(i, 67)
    If XCode = 1 GoTo PLL2B:
    If XCode = 2 GoTo PDone

    PLLHR:
    Locate 22, 42
    Max = 3
    Call NumIn(p%(i, 68), Max, XCode, FG, BG)
    Locate 22, 42: Print Using "###"; p%(i, 68)
    If XCode = 1 GoTo PLL3B:
    If XCode = 2 GoTo PDone

    PLLBB:
    Locate 22, 47
    Max = 3
    Call NumIn(p%(i, 69), Max, XCode, FG, BG)
    Locate 22, 47: Print Using "###"; p%(i, 69)
    If XCode = 1 GoTo PLLHR:
    If XCode = 2 GoTo PDone

    PLLKO:
    Locate 22, 52
    Max = 3
    Call NumIn(p%(i, 70), Max, XCode, FG, BG)
    Locate 22, 52: Print Using "###"; p%(i, 70)
    If XCode = 1 GoTo PLLBB:
    If XCode = 2 GoTo PDone

    PLRBA:
    Locate 23, 27
    Max = 3
    Call NumIn(p%(i, 71), Max, XCode, FG, BG)
    Locate 23, 27: Print Using "###"; p%(i, 71)
    If XCode = 1 GoTo PLLKO:
    If XCode = 2 GoTo PDone

    PLR2B:
    Locate 23, 32
    Max = 3
    Call NumIn(p%(i, 72), Max, XCode, FG, BG)
    Locate 23, 32: Print Using "###"; p%(i, 72)
    If XCode = 1 GoTo PLRBA:
    If XCode = 2 GoTo PDone

    PLR3B:
    Locate 23, 37
    Max = 3
    Call NumIn(p%(i, 73), Max, XCode, FG, BG)
    Locate 23, 37: Print Using "###"; p%(i, 73)
    If XCode = 1 GoTo PLR2B:
    If XCode = 2 GoTo PDone

    PLRHR:
    Locate 23, 42
    Max = 3
    Call NumIn(p%(i, 74), Max, XCode, FG, BG)
    Locate 23, 42: Print Using "###"; p%(i, 74)
    If XCode = 1 GoTo PLR3B:
    If XCode = 2 GoTo PDone

    PLRBB:
    Locate 23, 47
    Max = 3
    Call NumIn(p%(i, 75), Max, XCode, FG, BG)
    Locate 23, 47: Print Using "###"; p%(i, 75)
    If XCode = 1 GoTo PLRHR:
    If XCode = 2 GoTo PDone

    PLRKO:
    Locate 23, 52
    Max = 3
    Call NumIn(p%(i, 76), Max, XCode, FG, BG)
    Locate 23, 52: Print Using "###"; p%(i, 76)
    If XCode = 1 GoTo PLRBB:
    If XCode = 2 GoTo PDone

    PRLBA:
    Locate 24, 27
    Max = 3
    Call NumIn(p%(i, 77), Max, XCode, FG, BG)
    Locate 24, 27: Print Using "###"; p%(i, 77);
    If XCode = 1 GoTo PLRKO:
    If XCode = 2 GoTo PDone

    PRL2B:
    Locate 24, 32
    Max = 3
    Call NumIn(p%(i, 78), Max, XCode, FG, BG)
    Locate 24, 32: Print Using "###"; p%(i, 78);
    If XCode = 1 GoTo PRLBA:
    If XCode = 2 GoTo PDone

    PRL3B:
    Locate 24, 37
    Max = 3
    Call NumIn(p%(i, 79), Max, XCode, FG, BG)
    Locate 24, 37: Print Using "###"; p%(i, 79);
    If XCode = 1 GoTo PRL2B:
    If XCode = 2 GoTo PDone

    PRLHR:
    Locate 24, 42
    Max = 3
    Call NumIn(p%(i, 80), Max, XCode, FG, BG)
    Locate 24, 42: Print Using "###"; p%(i, 80);
    If XCode = 1 GoTo PRL3B:
    If XCode = 2 GoTo PDone

    PRLBB:
    Locate 24, 47
    Max = 3
    Call NumIn(p%(i, 81), Max, XCode, FG, BG)
    Locate 24, 47: Print Using "###"; p%(i, 81);
    If XCode = 1 GoTo PRLHR:
    If XCode = 2 GoTo PDone

    PRLKO:
    Locate 24, 52
    Max = 3
    Call NumIn(p%(i, 82), Max, XCode, FG, BG)
    Locate 24, 52: Print Using "###"; p%(i, 82);
    If XCode = 1 GoTo PRLBB:
    If XCode = 2 GoTo PDone

    PRRBA:
    Locate 25, 27
    Max = 3
    Call NumIn(p%(i, 83), Max, XCode, FG, BG)
    Locate 25, 27: Print Using "###"; p%(i, 83);
    If XCode = 1 GoTo PRLKO:
    If XCode = 2 GoTo PDone

    PRR2B:
    Locate 25, 32
    Max = 3
    Call NumIn(p%(i, 84), Max, XCode, FG, BG)
    Locate 25, 32: Print Using "###"; p%(i, 84);
    If XCode = 1 GoTo PRRBA:
    If XCode = 2 GoTo PDone

    PRR3B:
    Locate 25, 37
    Max = 3
    Call NumIn(p%(i, 85), Max, XCode, FG, BG)
    Locate 25, 37: Print Using "###"; p%(i, 85);
    If XCode = 1 GoTo PRR2B:
    If XCode = 2 GoTo PDone

    PRRHR:
    Locate 25, 42
    Max = 3
    Call NumIn(p%(i, 86), Max, XCode, FG, BG)
    Locate 25, 42: Print Using "###"; p%(i, 86);
    If XCode = 1 GoTo PRR3B:
    If XCode = 2 GoTo PDone

    PRRBB:
    Locate 25, 47
    Max = 3
    Call NumIn(p%(i, 87), Max, XCode, FG, BG)
    Locate 25, 47: Print Using "###"; p%(i, 87);
    If XCode = 1 GoTo PRRHR:
    If XCode = 2 GoTo PDone

    PRRKO:
    Locate 25, 52
    Max = 3
    Call NumIn(p%(i, 88), Max, XCode, FG, BG)
    Locate 25, 52: Print Using "###"; p%(i, 88);
    If XCode = 1 GoTo PRRBB:

    PDone:

    Locate 25, 1
    Print "Is this okay? (Yes/No) ";

    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ':ClearEOL 30
    If XCode = 1 GoTo PRRKO
    If X$ = "N" GoTo PName
    If X$ <> "Y" GoTo PDone 'insist on either Y or N or up-arrow

    p%(i, 0) = 1

    THROW$ = _Trim$(THROW$)
    If UCase$(THROW$) = "L" Then p%(i, 0) = -1

    ACT$ = _Trim$(ACT$)
    p%(i, 34) = 0
    If UCase$(ACT$) = "N" Then p%(i, 34) = 1

    GoTo 1150
1180 End Sub


'------------------------------
'      PITCHINIT Subroutine
'------------------------------
' ...explanation...
Sub PITCHINIT (i, Colr)
    Color 0, 7
    Locate 1, 60: Print Using "###"; p%(i, 35)
    Locate 1, 78: Print Using "#"; p%(i, 18)

    Locate 5, 10: Print Using "##"; p%(i, 1)
    Locate 5, 14: Print Using "##"; p%(i, 2)
    Locate 5, 18: Print Using "##"; p%(i, 3)
    Locate 5, 22: Print Using "###"; p%(i, 4)
    Locate 5, 27: Print Using "##"; p%(i, 5)
    Locate 5, 31: Print Using "##"; p%(i, 16)
    Locate 5, 35: Print Using "###"; p%(i, 6)
    Locate 5, 40: Print Using "###"; p%(i, 7)
    Locate 5, 45: Print Using "###"; p%(i, 8)
    Locate 5, 50: Print Using "###"; p%(i, 9)
    Locate 5, 55: Print Using "##"; p%(i, 33)
    Locate 5, 59: Print Using "####"; p%(i, 10)
    Locate 5, 66: Print Using "##"; p%(i, 13)

    Locate 6, 32: Print Using "##"; p%(i, 12)
    Locate 6, 50: Print Using "###"; p%(i, 14)

    Locate 10, 25: Print Using "###"; p%(i, 51)
    Locate 10, 30: Print Using "###"; p%(i, 52)
    Locate 10, 35: Print Using "##"; p%(i, 53)
    Locate 10, 39: Print Using "##"; p%(i, 54)
    Locate 10, 43: Print Using "##"; p%(i, 55)
    Locate 10, 47: Print Using "###"; p%(i, 56)
    Locate 10, 52: Print Using "###"; p%(i, 57)

    Locate 11, 25: Print Using "###"; p%(i, 58)
    Locate 11, 30: Print Using "###"; p%(i, 59)
    Locate 11, 35: Print Using "##"; p%(i, 60)
    Locate 11, 39: Print Using "##"; p%(i, 61)
    Locate 11, 43: Print Using "##"; p%(i, 62)
    Locate 11, 47: Print Using "###"; p%(i, 63)
    Locate 11, 52: Print Using "###"; p%(i, 64)

    Locate 14, 20: Print Using "#"; p%(i, 31)
    Locate 14, 36: Print Using "#"; p%(i, 32)
    Locate 14, 52: Print Using "###"; p%(i, 11)
    Locate 14, 68: Print Using "####"; p%(i, 17)

    Locate 16, 12: Print Using "###"; p%(i, 19)
    Locate 16, 17: Print Using "###"; p%(i, 20)
    Locate 16, 22: Print Using "###"; p%(i, 21)
    Locate 16, 27: Print Using "###"; p%(i, 22)
    Locate 16, 32: Print Using "##"; p%(i, 23)
    Locate 16, 36: Print Using "##"; p%(i, 24)
    Locate 16, 40: Print Using "##"; p%(i, 25)
    Locate 16, 44: Print Using "###"; p%(i, 26)
    Locate 16, 49: Print Using "###"; p%(i, 27)
    Locate 16, 54: Print Using "###"; p%(i, 28)
    Locate 16, 59: Print Using "###"; p%(i, 29)
    Locate 16, 64: Print Using "##"; p%(i, 30)
    Locate 16, 68: Print Using "##"; p%(i, 36)

    Locate 17, 17: Print Using "###"; p%(i, 37)
    Locate 17, 27: Print Using "###"; p%(i, 38)
    Locate 17, 32: Print Using "##"; p%(i, 39)
    Locate 17, 36: Print Using "##"; p%(i, 40)
    Locate 17, 40: Print Using "##"; p%(i, 41)
    Locate 17, 49: Print Using "###"; p%(i, 42)
    Locate 17, 54: Print Using "###"; p%(i, 43)

    Locate 18, 17: Print Using "###"; p%(i, 44)
    Locate 18, 27: Print Using "###"; p%(i, 45)
    Locate 18, 32: Print Using "##"; p%(i, 46)
    Locate 18, 36: Print Using "##"; p%(i, 47)
    Locate 18, 40: Print Using "##"; p%(i, 48)
    Locate 18, 49: Print Using "###"; p%(i, 49)
    Locate 18, 54: Print Using "###"; p%(i, 50)
End Sub


'------------------------------
'     PITCHINIT2 Subroutine
'------------------------------
' ...explanation...
Sub PITCHINIT2 (i, Colr)
    Color 0, 7
    Locate 22, 27: Print Using "###"; p%(i, 65)
    Locate 22, 32: Print Using "###"; p%(i, 66)
    Locate 22, 37: Print Using "###"; p%(i, 67)
    Locate 22, 42: Print Using "###"; p%(i, 68)
    Locate 22, 47: Print Using "###"; p%(i, 69)
    Locate 22, 52: Print Using "###"; p%(i, 70)
    Locate 23, 27: Print Using "###"; p%(i, 71)
    Locate 23, 32: Print Using "###"; p%(i, 72)
    Locate 23, 37: Print Using "###"; p%(i, 73)
    Locate 23, 42: Print Using "###"; p%(i, 74)
    Locate 23, 47: Print Using "###"; p%(i, 75)
    Locate 23, 52: Print Using "###"; p%(i, 76)
    Locate 24, 27: Print Using "###"; p%(i, 77);
    Locate 24, 32: Print Using "###"; p%(i, 78);
    Locate 24, 37: Print Using "###"; p%(i, 79);
    Locate 24, 42: Print Using "###"; p%(i, 80);
    Locate 24, 47: Print Using "###"; p%(i, 81);
    Locate 24, 52: Print Using "###"; p%(i, 82);
    Locate 25, 27: Print Using "###"; p%(i, 83);
    Locate 25, 32: Print Using "###"; p%(i, 84);
    Locate 25, 37: Print Using "###"; p%(i, 85);
    Locate 25, 42: Print Using "###"; p%(i, 86);
    Locate 25, 47: Print Using "###"; p%(i, 87);
    Locate 25, 52: Print Using "###"; p%(i, 88);
End Sub


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

