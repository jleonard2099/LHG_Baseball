'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Create League"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE FUNCTION PadRight$ (X$, Length)

Dim Z1$(40), Z2$(40), Z1%(40), D$(2)
Dim Q$(15), teamItems$(200), V%(200), T%(34), L%(13)
Dim Y As Long

'determine if the monitor is mono or color
FG = 0
BG = 7
'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte

Open "DEV.BAS" For Input As #2
For I = 0 To 2
    Input #2, D$(I)
Next
Close 2
Cls

50 For X = 1 To 40
    Z1$(X) = "XXX": Z2$(X) = "XXX": Z1%(X) = -1
Next

60 Cls
Color 12: Print Tab(31); "LEAGUE EDITOR MENU": Print

70 Color 14: Print "(1)";: Color 15: Print " EDIT AN EXISTING LEAGUE": Print

80 Color 14: Print "(2)";: Color 15: Print " CREATE A NEW LEAGUE": Print

90 Color 14: Print "(3)";: Color 15: Print " RETURN TO STANDINGS/LEADERS MENU"
100 I$ = GetKeyPress$

110 I1 = Val(I$)
If I1 < 1 Or I1 > 3 Then 100
If I1 = 1 Then 4000

120 If I1 = 3 Or I$ = Chr$(27) Then 1195

130 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
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

Locate 7, 32
Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES."
Locate , 12: Print "SAMPLE NAMES ARE NL1990 AL56 1956AL NATIONAL"
Locate 7, 37: Input LG$
If LG$ = "" Then 130
LG$ = Left$(LG$, 8)
Locate 13, 12: Print "ANY CHANGE (YN) "

22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 130

25 Cls
Color 15, 1
Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
Locate , 21: QPrint PadRight$(D1$, 8), FG, BG, -1
Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
Locate , 21: QPrint PadRight$(D2$, 8), FG, BG, -1
Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
Locate , 21: QPrint PadRight$(D3$, 8), FG, BG, -1
Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
Locate , 21: QPrint PadRight$(D4$, 8), FG, BG, -1

GetDiv1:
Locate 5, 21 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
Call TextIn(D1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data

GetDiv2:
Locate 7, 21 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
Call TextIn(D2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data

GetDiv3:
Locate 9, 21 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
Call TextIn(D3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data

GetDiv4:
Locate 11, 21 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
Call TextIn(D4$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

Done:
Locate 23, 1
Print "Is this okay? (Yes/No) ";
X$ = ""
Call YesNoB(X$, XCode, FG, BG)
Locate , 1
ClearEOL 14, 1
If XCode = 1 GoTo GetDiv4
If X$ = "N" GoTo GetDiv1
If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

220 Color 14, 0
Cls
Print
Print "  "; LG$; " LEAGUE ": Print: Color 12
Locate , 4: Print D1$; Tab(24); D2$; Tab(44); D3$; Tab(64); D4$

230 For I = 1 To 10
    Color 2: Print Using "## "; I;: Color 15: Print Z1$(I);
    Locate , 21: Color 2: Print Using "## "; I + 10;: Color 15: Print Z1$(I + 10);
    Locate , 41: Color 2: Print Using "## "; I + 20;: Color 15: Print Z1$(I + 20);
    Locate , 61: Color 2: Print Using "## "; I + 30;: Color 15: Print Z1$(I + 30)
Next I

240 Locate 19, 1
Color 14: Print "(1) ";: Color 15: Print "ADD A TEAM"
Color 14: Print "(2) ";: Color 15: Print "REMOVE A TEAM"
Color 14: Print "(3) ";: Color 15: Print "END AND SAVE SETTINGS"
250 I$ = GetKeyPress$

260 If I$ = "3" Then 950

270 Locate 20, 30: Print "SLOT # (1-40) ";

280 N = 1: Input B

290 If B < 1 Or B > 40 Then 270

300 If I$ = "2" Then Z1$(B) = "XXX": GoTo 220

310 GoSub 1440

390 Z1$(B) = T1$: Z2$(B) = YN$: Z1%(B) = D

400 GoTo 220

950 Cls

970 Open D$(2) + LG$ + ".LIB" For Output As #2
Print #2, LG$: Print #2, D1$: Print #2, D2$: Print #2, D3$: Print #2, D4$
For X = 1 To 40
    Print #2, Z1$(X): Print #2, Z1%(X): Print #2, Z2$(X)
Next
Close 2
For X = 1 To 40

    980 If Z1$(X) = "XXX" Then Z1$(X) = Chr$(42): GoTo 1040

    990 For D = 4 To 11
        If Mid$(Z1$(X), D, 1) = Chr$(32) Or Mid$(Z1$(X), D, 1) = "." Then Mid$(Z1$(X), D, 1) = "_"

    1000 Next D
    QQ = 8
    For D = 11 To 4 Step -1
        If Mid$(Z1$(X), D, 1) = "_" Then QQ = D - 4: GoTo 1020

        1010 GoTo 1030

    1020 Next D

    1030 L = Len(Z1$(X))
    YR$ = Left$(Z1$(X), 2)
    TM$ = Mid$(Z1$(X), 4, QQ)
    Z1$(X) = TM$ + "." + YR$

1040 Next X

1050 Q$ = LG$ + ".TMS"

1060 Open D$(2) + Q$ For Output As #2

1070 Write #2, LG$: Write #2, D1$: Write #2, D2$: Write #2, D3$: Write #2, D4$

1080 For X = 1 To 40
    Write #2, Z1$(X)
Next

1090 Close 2

1195 Cls
Run "HELLO.EXE"
End

1100 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then 2060
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
D = V%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."

1300 Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
Get #1, D
T1$ = Q$(0)
T1$ = RTrim$(T1$)
For I1 = 1 To 13: L%(I1) = CVI(Q$(I1)): Next
Close 1
Return

1440 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
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

Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
Erase teamItems$

182 If BO% = 1 Then BO% = 0: GoTo 1440

183 Locate 2, 33: Color 15, 0: Color L%(11), L%(12): Print T1$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1440
Return

2060 Cls: Print "ERROR:: UNABLE TO READ TEAM FILE": Print

2720 BO% = 1: Return

4000 Spec$ = D$(2) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
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
Print "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 60
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
    
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
LG$ = Left$(targetFile$, Len(targetFile$) - 4)
Open D$(2) + LG$ + ".LIB" For Input As #2
Input #2, LG$: Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$
For X = 1 To 40
    Input #2, Z1$(X): Input #2, Z1%(X): Input #2, Z2$(X)
Next
Close 2
Color 15, 1
GoTo 25

4500 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 50


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
