'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
''$INCLUDE: 'QEditype.BI'

_Title "Full Count Baseball - Scheduler"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
Dim U6 As Single

' STILL HAVE TO TAKE OLD FORMAT AND WRITE TO NEW FORMAT....HAVE REMOVED
' NEED FOR TEAM ABBREV....FIND WAY TO PUT MO AND DATE IN INTEGER VARIABLES
' POSSIBLY STORE EACH PART OF DATE IN 3 FIELDS ON INTEGER 6 VS 8 BYTES
' TIME SAME WAY, CONVERT TO MILITARY EQUIVAKLENT FOR STORAGE PURPOSES

DECLARE SUB TITLE ()
'DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)

Dim teamItems$(200), V%(200), TS$(1), MO$(3), U5$(4), TE$(2), CM$(1), PU$(48)
Dim Q$(377), AP%(1), T(34), FT$(1), FR$(1), T$(0), Z1$(40), Z1%(40), Z2$(40)
Dim H$(20), V$(20), HS%(1300), VS%(1300), L%(13), D$(2), WE$(1), E%(13)
Dim Y As Long, menuItems$(9)
Dim Y1 As Single
'DIM Ed AS EditInfo              'create the editor's data structure

Dim Shared teamItems$, V%, TS$, DY, Q$
Dim Shared YN$, NG%, HS%, VS%

'commenting out here as it is mostly irrelevant
'CALL InitMouse(There)

F2$ = Chr$(0) + Chr$(60) 'extended code for the F2 function key
F3$ = Chr$(0) + Chr$(61) 'extended code for the F3 function key

Data HH,C1,C2,CC: For X = 0 To 3: Read MO$(X): Next
Data 15,0,15,1,19,4,4,6,3,1,2,3,14

For X = 1 To 13: Read E%(X): Next

10 ReDim DY(1300)
ReDim NG%(1300, 18)
ReDim YN$(1300, 1)

Open "DEV.BAS" For Input As #1: For I = 0 To 2: Input #1, D$(I): Next: Close 1

16 Color 7, 0: Cls
NG%(0, 8) = 0
NG%(0, 3) = 1
NG%(0, 7) = 0
NG%(0, 2) = 1
NG%(0, 14) = 4
DT$ = "04-04-93"
TM$ = "07:30P"
TS$(0) = "N": TS$(1) = "Y"
U5$(0) = "HUMAN OPPONENT             "
U5$(1) = "COMPUTER IS VISITING TEAM  "
U5$(2) = "COMPUTER IS HOME TEAM      "
U5$(3) = "COMPUTER VS COMPUTER       "
PU$(0) = "1980 - PRESENT"
PU$(1) = "1973 - 1979   "
PU$(2) = "1955 - 1972   "
PU$(3) = "1946 - 1954   "
PU$(4) = "1933 - 1945   "
PU$(5) = "1919 - 1932   "
PU$(6) = "1909 - 1918   "
PU$(7) = "1894 - 1908   "
PU$(8) = "1876 - 1893   "

11 Call TITLE
Erase menuItems$
Locate 16: Print: Color 2
Print "GENERATING A STANDARD SEASON SCHEDULE IS TO BE USED IN CONJUNCTION"
Print "WITH THE STANDINGS AND LEAGUE LEADERS OPTIONS. YOU MUST ALREADY HAVE"
Print "CREATED AN APPROPRIATE LEAGUE FOR THIS OPTION TO PROPERLY FUNCTION."
menuItems$(1) = "ENTER A NEW SCHEDULE                             "
menuItems$(2) = "RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE          "
menuItems$(3) = "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE "
menuItems$(4) = "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE "
menuItems$(5) = "MAKE GLOBAL CHANGES TO A SCHEDULE                "
menuItems$(6) = "GENERATE A STANDARD SEASON SCHEDULE              "
menuItems$(7) = "BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS  "
menuItems$(8) = "EXIT AUTO SCHEDULER TO FULL COUNT MENU           "
SELECTION% = 1
Count% = 8
Start% = 1
SCANCODE% = 8
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 16
Call box0(5, 15, 14, 65, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 8
D3% = SELECTION% - 1
On SELECTION% GOTO 20, 600, 600, 600, 600, 1300, 4500
15 I$ = GetKeyPress$
Run "HELLO.EXE"

20 AP%(0) = 0: NS% = 0: TS% = 0

21 GoSub 4100

25 Cls
Locate 23, 1: Color , 1: Print Tab(80);: Locate 23, 1
Color 13: Print "MA";: Color 14: Print "-MODE OF PLAY  ";
Color 13: Print "WEA";: Color 14: Print "-USE WEATHER";
Color 11: Print " G";: Color 14: Print "-GOTO GAME";
Color 11: Print " B";: Color 14: Print "-BOXSCORE OPTIONS"
Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
Color 11: Print "C";: Color 14: Print "-SAVE AND CONTINUE  ";
Color 11: Print "A";: Color 14: Print "-ADD A GAME  ";
Color 11: Print "D";: Color 14: Print "-DELETE A GAME  ";
Color 11: Print "E";: Color 14: Print "-EDIT A GAME  ";
Color 11: Print "PGUP  PGDN";
Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
Color 11: Print "S";: Color 14: Print "-SORT GAMES  ";
Color 11: Print "P";: Color 14: Print "-PLAY GAMES   ";
Color 11: Print "X";: Color 14: Print "-SAVE AND EXIT  ";
Color 11: Print "Q";: Color 14: Print "-QUIT  ";
Color 11: Print "R";: Color 14: Print "-RESET SCORES";
Locate 1, 1: Print Tab(80);: Locate 1, 1: Print F$
Locate 1, 33: Color 14: Print "AUTO SCHEDULER"
Color , 0

50 Locate 2, 1
Color 14
Print "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  DH  WEA": Color 15
For X = 1 To 20
    Locate X + 2, 1
    If X + NS% * 20 > AP%(0) Then 100
    T1 = NG%(X + NS% * 20, 0): YN$ = YN$(X + NS% * 20, 0)
    GoSub 9000
    V$(X) = T$(0)
    T1 = NG%(X + NS% * 20, 1): YN$ = YN$(X + NS% * 20, 1)
    GoSub 9000
    H$(X) = T$(0)
    Print Using "####  "; X + NS% * 20;
    Print Using "##/##/##"; NG%(X + NS% * 20, 17); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 18);
    Locate , 17
    I$ = "A"
    HR% = NG%(X + NS% * 20, 15) / 100
    MIN% = NG%(X + NS% * 20, 15) - HR% * 100
    If HR% = 12 Then I$ = "P"
    If HR% > 12 Then HR% = HR% - 12: I$ = "P"
    TM$ = Right$(Str$(HR%), 2) + ":"
    If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
    If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)
    TM$ = TM$ + I$
    Print TM$;
    Locate , 25: Print V$(X); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; VS%(X + NS% * 20);
    Locate , 46: Print H$(X); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; HS%(X + NS% * 20);
    Locate , 67: Print MO$(NG%(X + NS% * 20, 8));
    Locate , 72: If NG%(X + NS% * 20, 3) = 1 Then Print "Y";: Else Print "N";
    Locate , 75: Print TS$(1 - NG%(X + NS% * 20, 5))
Next
100 I$ = GetKeyPress$
If UCase$(I$) = "A" Then GoSub 179: GoTo 25
If UCase$(I$) = "E" Then GoSub 140: GoTo 25
If UCase$(I$) = "D" Then GoSub 200: GoTo 25
If UCase$(I$) = "G" Then GoSub 120: GoTo 25
If UCase$(I$) = "B" Then 5000
If UCase$(I$) = "S" Then GoSub 8500: GoTo 25
If UCase$(I$) = "C" Then GoSub 350: GoTo 25
If UCase$(I$) = "X" Then GoSub 350: GoTo 10
If I$ = Chr$(0) + "Q" Then GoSub 130: GoTo 25
If I$ = Chr$(0) + "I" Then GoSub 135: GoTo 25
If UCase$(I$) = "Q" Then 10
If UCase$(I$) = "P" Then GoSub 350: GoTo 300
If UCase$(I$) = "R" Then GoSub 400: GoTo 25
If I$ = Chr$(27) Then 10
GoTo 100

120 'GOTO A SPECIFIC PAGE
Locate 23, 1: Print "ENTER GAME # TO GO TO        "
Locate 23, 1: Input "ENTER GAME # TO GO TO "; NG%
If NG% < 1 Or NG% > AP%(0) Then 120
NS% = Int((NG% - 1) / 20)
Return

130 ' *** PAGE DOWN ***
If NS% >= BS% Then NS% = BS%: GoTo 133
NS% = NS% + 1

133 Return

135 ' *** PAGE UP ***
If NS% = 0 Then 137
NS% = NS% - 1

137 Return

140 ' *** EDIT A GAME ***
Locate 23, 1: Print "ENTER GAME # TO EDIT        "
Locate 23, 1: Input "ENTER GAME # TO EDIT "; NG%
If NG% < 1 Or NG% > AP%(0) Then 140
V$ = V$(NG% - Int(NG% / 20) * 20)
H$ = H$(NG% - Int(NG% / 20) * 20)
If NG% - Int(NG% / 20) * 20 = 0 Then V$ = V$(20): H$ = H$(20)
I$ = "A"
HR% = Int(NG%(NG%, 15) / 100)
MIN% = NG%(NG%, 15) - HR% * 100
If HR% = 12 Then I$ = "P"
If HR% > 12 Then HR% = HR% - 12: I$ = "P"
TM$ = Right$(Str$(HR%), 2) + ":"
If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)
TM$ = TM$ + I$
DT$ = Right$(Str$(NG%(NG%, 17)), 2) + "/"
DY% = Val(Right$(Str$(NG%(NG%, 16)), 2))
If DY% < 10 Then DY$ = "0" + Right$(Str$(NG%(NG%, 16)), 1)
If DY% >= 10 Then DY$ = Right$(Str$(NG%(NG%, 16)), 2)
DT$ = DT$ + DY$ + "/" + Right$(Str$(NG%(NG%, 18)), 2)
GoTo 149

179 ' *** ADD A GAME TO THE SCHEDULE ***
V$ = "": H$ = ""
AP%(0) = AP%(0) + 1
NG% = AP%(0)
BS% = Int((NG% - 1) / 20): If NS% < BS% Then NS% = BS%
For X = 0 To 18
    If X = 13 Then 139
    NG%(NG%, X) = NG%(NG% - 1, X)

139 Next
For X = 0 To 1: YN$(NG%, X) = YN$(NG% - 1, X): Next

149 Cls: Color 2: Print Tab(29); "SELECT GAME OPTIONS": Print

151 Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(NG%(NG%, 8))

152 Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: "; V$
Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: "; H$

154 Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "DESIGNATED HITTER: "; TS$(NG%(NG%, 3))

157 Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(NG%, 4) / 10

160 Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "GAME DATE (XX-XX-XX) "; DT$; "   "

161 Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print "GAME TIME (XX:XXP) "; TM$; "   "

158 Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "USE WEATHER EFFECTS: "; TS$(1 - NG%(NG%, 5))
Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print "USE INJURIES "; TS$(1 - NG%(NG%, 6))
Locate 11, 2: Color 14: Print "(A) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; TS$(1 - NG%(NG%, 7))
Locate 12, 2: Color 14: Print "(B) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; TS$(1 - NG%(NG%, 9))
Locate 13, 2: Color 14: Print "(C) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; TS$(1 - NG%(NG%, 10))
Locate 14, 2: Color 14: Print "(D) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; TS$(1 - NG%(NG%, 11))
Locate 15, 2: Color 14: Print "(E) ";: Color 15: Print "PITCHER ERA : "; PU$(NG%(NG%, 14))

176 Locate 16, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"
175 I$ = GetKeyPress$
If UCase$(I$) = "A" Then I$ = "10"
If UCase$(I$) = "B" Then I$ = "11"
If UCase$(I$) = "C" Then I$ = "12"
If UCase$(I$) = "D" Then I$ = "13"
If UCase$(I$) = "E" Then I$ = "14"
I3% = Val(I$): If I3% < 0 Or I3% > 14 Then 175
On I3% GOTO 180, 181, 181, 184, 187, 190, 191, 192, 251, 252, 253, 254, 255, 193
If V$ <> "" And H$ <> "" And TM$ <> "" And DT$ <> "" Then 250
GoTo 175

250 I$ = Right$(TM$, 1): TM$ = Left$(TM$, Len(TM$) - 1)
MIN% = Val(Right$(TM$, 2))
TM$ = Left$(TM$, Len(TM$) - 3)
HR% = Val(TM$)
If UCase$(I$) = "P" And HR% < 12 Then HR% = HR% + 12
NG%(NG%, 15) = HR% * 100 + MIN%
NG%(NG%, 17) = Val(Left$(DT$, 2))
NG%(NG%, 18) = Val(Mid$((DT$), 7, 2))
NG%(NG%, 16) = Val(Mid$((DT$), 4, 2))
Return

195 NG%(NG%, 11) = 1 - NG%(NG%, 11): GoTo 149

251 NG%(NG%, 6) = 1 - NG%(NG%, 6): GoTo 149

252 NG%(NG%, 7) = 1 - NG%(NG%, 7): GoTo 149

253 NG%(NG%, 9) = 1 - NG%(NG%, 9): GoTo 149

254 NG%(NG%, 10) = 1 - NG%(NG%, 10): GoTo 149

255 NG%(NG%, 11) = 1 - NG%(NG%, 11): GoTo 149

193 NG%(NG%, 14) = NG%(NG%, 14) + 1
If NG%(NG%, 14) > 8 Then NG%(NG%, 14) = 0
GoTo 149

194 NG%(NG%, 10) = 1 - NG%(NG%, 10): GoTo 149

180 NG%(NG%, 8) = NG%(NG%, 8) + 1: If NG%(NG%, 8) > 3 Then NG%(NG%, 8) = 0
GoTo 151

181 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
'Print box top
Print " " + Chr$(201); String$(61, 196); Chr$(187) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(200); String$(61, 196); Chr$(188) + " ";

Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
Erase teamItems$
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 181

183 Locate 2, 33: Color L%(11), L%(12): Print T$(0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 181
Color 15, 0
NG%(NG%, I3% - 2) = T1: YN$(NG%, I3% - 2) = YN$
If I3% = 2 Then V$ = T$(0): V$(NG% - Int(NG% / 20) * 20) = T$(0)
If I3% = 3 Then H$ = T$(0): H$(NG% - Int(NG% / 20) * 20) = T$(0)
GoTo 149

184 NG%(NG%, 3) = 1 - NG%(NG%, 3): GoTo 154

187 Locate 6, 32: Input U6: NG%(NG%, 4) = U6 * 10: GoTo 157

190 Locate 7, 27: Input DT$: GoTo 160

191 Locate 8, 25: Input TM$: GoTo 161

192 NG%(NG%, 5) = 1 - NG%(NG%, 5): GoTo 158

200 ' *** DELETE A GAME FROM THE SCHEDULE ***
Locate 24, 1: Input "ENTER GAME # TO DELETE "; NG%
If NG% < 1 Or NG% > AP%(0) Then 200
For X = NG% To AP%(0) - 1
    For X1 = 0 To 18: NG%(X, X1) = NG%(X + 1, X1): Next
    VS%(X) = VS%(X + 1)
    HS%(X) = HS%(X + 1)
    For X1 = 0 To 1: YN$(X, X1) = YN$(X + 1, X1): Next
Next
AP%(0) = AP%(0) - 1: BS% = Int((AP%(0) - 1) / 20)
If NS% > BS% Then NS% = BS%
Return

300 ' *** PLAY GAMES ***
Color , 0: Cls
JJ = 8
Locate 6, 8: Color 15, 4
'Print box top
Print " " + Chr$(201); String$(61, 196); Chr$(187) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(200); String$(61, 196); Chr$(188) + " ";

Locate 7, 32
Locate , 12: Print "TOTAL GAMES ="; AP%(0)
Locate , 12: Print "SELECT GAME # TO START AND END WITH": Print
Locate , 12: Input "ENTER START GAME #: "; SG%: Print
Locate , 12: Input "ENTER END GAME #:"; EG%: Print
Locate , 12: Print "ANY CHANGE (YN)"
301 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 300
If SG% <= 0 Then SG% = 1
If EG% > AP%(0) Then EG% = AP%(0)
Open "GAME82" For Output As #2
Print #2, F$
Print #2, SG%
Print #2, EG%
Print #2, 0
Close 2
GoSub 1200
Open "TICKER" For Output As #2
For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
Print #2, -1: Next: Next: Next
For I = 1 To 32: For I1 = 0 To 1: Print #2, "XXX"
Next: Next
For I = SG% To SG% + 31
    Print #2, NG%(I, 12)
Next
Close 2

335 Run "LOADER.EXE"

350 ' *** SAVE SCHEDULE FILE ***
If _FileExists(D$(0) + F$) Then Kill D$(0) + F$

365 Open D$(0) + F$ For Random As #1 Len = 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
    For X = 0 To 1: LSet Q$(20 + X) = YN$(I, X): Next
    LSet Q$(22) = MKI$(VS%(I)): LSet Q$(23) = MKI$(HS%(I))
    Put #1, I
Next
Close 1
Return

400 ' *** RESET GAMES ***
For I = 1 To AP%(0)
    NG%(I, 13) = 0
Next
Return

600 ' *** LOAD IN A SAVED SCHEDULE ***
Spec$ = D$(0) + "*.SCD"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4050
ReDim scheduleFiles$(0 To Count%)
For X = 1 To Count%
    scheduleFiles$(X) = Space$(12)
Next
scheduleFiles$(0) = Spec$
Call ReadFile(scheduleFiles$())
'CALL SortStr(VARPTR(scheduleFiles$(1)), Count, 0)
Cls
Color , 1: Cls: Locate , 28: Color 14
Print "SELECT SCHEDULE TO EDIT": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15
Print "-SELECTS SCHEDULE";

_MouseShow "DEFAULT"
620 MAMenu scheduleFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 10
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 620
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 620
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 620
F$ = scheduleFiles$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
604 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 600
Open D$(0) + F$ For Random As #1 Len = 48
Y = LOF(1)
AP%(0) = Y / 48
BS% = Int((AP%(0) - 1) / 20)
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    Get #1, I
    For X = 0 To 18: NG%(I, X) = CVI(Q$(X + 1)): Next
    If NG%(I, 14) > 8 Then NG%(I, 14) = 0
    For X = 0 To 1: YN$(I, X) = Q$(20 + X): Next
    VS%(I) = CVI(Q$(22)): HS%(I) = CVI(Q$(23))
    If NG%(I, 6) > 1 Then NG%(I, 6) = 1
    If NG%(I, 7) > 1 Then NG%(I, 7) = 1
    If NG%(I, 9) > 1 Then NG%(I, 9) = 1
    If NG%(I, 10) > 1 Then NG%(I, 10) = 1
    If NG%(I, 11) > 1 Then NG%(I, 11) = 1
Next
Close 1
If D3% = 2 Or D3% = 3 Or D3% = 4 Then 700
GoTo 25

700 'EXTRACT A SCHEDULE FROM ANOTHER
If D3% = 3 Then 725
If D3% = 4 Then GoSub 900: GoSub 350: GoTo 25
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
'Print box top
Print " " + Chr$(201); String$(61, 196); Chr$(187) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(200); String$(61, 196); Chr$(188) + " ";

Locate 7, 32
Locate , 12: Print "ENTER NAME FOR SUB-SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE PHI73, STL68, MONTREAL, ETC"
Locate 7, 37: Input SF$
If SF$ = "" Then 700
Locate 13, 12: Print "ANY CHANGE (YN) "
705 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 700
Color 15, 0
Cls

725 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
'Print box top
Print " " + Chr$(201); String$(61, 196); Chr$(187) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(200); String$(61, 196); Chr$(188) + " ";

Locate 7, 32
Locate , 12: Print "SELECTING TEAM TO CHANGE CONTROL FOR"
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 8, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 730
YN$ = NY$
Erase teamItems$
GoSub 1100

730 If BO% = 1 Then BO% = 0: GoTo 725

732 Locate 2, 33: Color L%(11), L%(12): Print T$(0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 725
Color 15, 0
If D3% = 3 Then GoSub 775
'HAVE T1 AND YN$
For I = 1 To AP%(0)

    735 If Right$(YN$(I, 0), 1) = " " Then YN$(I, 0) = Left$(YN$(I, 0), Len(YN$(I, 0)) - 1): GoTo 735

    736 If Right$(YN$(I, 1), 1) = " " Then YN$(I, 1) = Left$(YN$(I, 1), Len(YN$(I, 1)) - 1): GoTo 736
    If T1 = NG%(I, 0) And YN$ = YN$(I, 0) Or T1 = NG%(I, 1) And YN$ = YN$(I, 1) Then GoSub 780: GoSub 750
Next
If D3% = 3 Then GoSub 350
GoTo 10

750 If D3% = 3 Then 755
Locate 1, 1: Print NG% + 1; " MATCHES"
Open D$(0) + SF$ + ".SCD" For Random As #1 Len = 48
Y = LOF(1)
GoTo 753

752 Y = 0

753 NG% = Y / 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
For X = 0 To 1: LSet Q$(20 + X) = YN$(I, X): Next
LSet Q$(22) = MKI$(VS%(I)): LSet Q$(23) = MKI$(HS%(I))
Put #1, NG% + 1
Close 1

755 Return

775 Cls
Color L%(11), L%(12): Print T$(0);: Color 15, 0: Print " IS TO BE:"
Print
Print "(0) MANUALLY CONTROLLED": Print
Print "(1) COMPUTER CONTROLLED"
776 I$ = GetKeyPress$
I1 = Val(I$)
If I1 < 0 Or I1 > 1 Then 776
Input "ENTER DISPLAY VARIABLE FOR THESE GAMES "; Y1
Y1 = Y1 * 10
Return

780 ' CHANGE SETTINGS FOR A TEAM
If T1 = NG%(I, 0) And YN$ = YN$(I, 0) Then NG%(I, 8) = 2
If T1 = NG%(I, 1) And YN$ = YN$(I, 1) Then NG%(I, 8) = 1
If (T1 = NG%(I, 0) And YN$ = YN$(I, 0) Or T1 = NG%(I, 1) And YN$ = YN$(I, 1)) And I1 = 1 And D3% = 3 Then NG%(I, 8) = 3
NG%(I, 4) = Y1
Return

900 ' GLOBAL SETTINGS FOR A SCHEDULE
Color 15, 0: Cls: Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"
'901 LOCATE 2, 2: COLOR 14: PRINT "(0) "; : COLOR 15: PRINT U5$(U5)

901 Locate 3, 2: Color 14: Print "(1) ";: Color 15: Print "DESIGNATED HITTER: "; TS$(DH%)
Locate 4, 2: Color 14: Print "(2) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; Y1 / 10
Locate 5, 2: Color 14: Print "(3) ";: Color 15: Print "USE WEATHER EFFECTS: "; TS$(1 - WE%)
Locate 6, 2: Color 14: Print "(4) ";: Color 15: Print "USE INJURIES "; TS$(1 - IJ%)
Locate 7, 2: Color 14: Print "(5) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; TS$(1 - LR%)
Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; TS$(1 - DR%)
Locate 9, 2: Color 14: Print "(7) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; TS$(1 - MP%)
Locate 10, 2: Color 14: Print "(8) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; TS$(1 - VG%)
Locate 11, 2: Color 14: Print "(9) ";: Color 15: Print "PITCHER ERA: "; PU$(PE%)
Locate 12, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT GLOBAL SETTINGS"
905 I$ = GetKeyPress$
I = Val(I$)
If I < 0 Or I > 9 Then 905
If I = 1 Then DH% = 1 - DH%: GoTo 901
If I = 3 Then WE% = 1 - WE%: GoTo 901
If I = 4 Then IJ% = 1 - IJ%: GoTo 901
If I = 5 Then LR% = 1 - LR%: GoTo 901
If I = 6 Then DR% = 1 - DR%: GoTo 901
If I = 7 Then MP% = 1 - MP%: GoTo 901
If I = 8 Then VG% = 1 - VG%: GoTo 901
If I = 9 Then PE% = PE% + 1
If I = 9 And PE% > 8 Then PE% = 0: GoTo 901
If I = 9 Then 901
If I = 2 Then Input "DISPLAY VARIABLE "; Y1: Y1 = Y1 * 10: GoTo 901
'    IF I = 0 THEN U5 = U5 + 1
'    IF I = 0 AND U5 > 3 THEN U5 = 0
'    IF I = 0 THEN 901
For I = 1 To AP%(0)
    '    NG%(I, 8) = U5
    NG%(I, 3) = DH%
    NG%(I, 4) = Y1
    NG%(I, 5) = WE%
    NG%(I, 6) = IJ%
    NG%(I, 7) = LR%
    NG%(I, 9) = DR%
    NG%(I, 10) = MP%
    NG%(I, 11) = VG%
    NG%(I, 14) = PE%
Next
Return

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
26 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 26
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 26
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 26
T1 = V%(SELECTION%)
GoTo 9000

1200 For I = 1 To AP%(0)
    HR% = Int(NG%(I, 15) / 100)
    MIN% = NG%(I, 15) - HR% * 100
    TM% = HR% * 60 + MIN%
    NG%(I, 12) = Int(TM% / 30 - 18)
Next
Return

2060 Cls: Print "ERROR:: UNABLE TO READ TEAM FILE": Print

2720 BO% = 1: Return

8500 ' *** SORT SCHEDULE ***
For I = 1 To AP%(0)
    HR% = Int(NG%(I, 15) / 100)
    MIN% = NG%(I, 15) - HR% * 100
    TM% = HR% * 60 + MIN%
    NG%(I, 12) = Int(TM% / 30 - 18)
    D = (NG%(I, 18) - 1): D = D * 365
    M = (NG%(I, 17) - 1): M = M * 31
    Y = NG%(I, 16)
    DY(I) = D + M + Y
Next
For Z = 1 To AP%(0) - 1: For Z1 = Z + 1 To AP%(0)
    If NG%(Z, 12) <= NG%(Z1, 12) Then 8505
        GoSub 8525

8505 Next Z1: Next Z
For Z = 1 To AP%(0) - 1: For Z1 = Z + 1 To AP%(0)
    If DY(Z) <= DY(Z1) Then 8510
        GoSub 8525

8510 Next Z1: Next Z
Return

8525 For I = 0 To 18: Swap NG%(Z, I), NG%(Z1, I): Next
For I = 0 To 1: Swap YN$(Z, I), YN$(Z1, I): Next
Swap DY(Z), DY(Z1)
Swap VS%(Z), VS%(Z1)
Swap HS%(Z), HS%(Z1)
Return

9000 Open D$(0) + "FCTEAMS." + YN$ For Random As #4 Len = 4342
Field #4, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

9005 Get #4, T1
T$(0) = Q$(0)

9010 If Right$(T$(0), 1) = " " Then T$(0) = Left$(T$(0), Len(T$(0)) - 1): GoTo 9010
For I1 = 1 To 13: L%(I1) = CVI(Q$(I1)): Next
Close 4
Return

1300 Cls: Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
Print "AUTO SCHEDULER"
Locate 25, 1: Print Tab(80);: Locate , 1: Print "(C) SHANNON LYNN 1990 V3.0";
Locate 5, 1: Color , 0
Color 15: Print "(0) ";: Color 14: Print "8 TEAM, 154 GAME SCHEDULE": Print
Color 15: Print "(1) ";: Color 14: Print "10 TEAM, 162 GAME SCHEDULE": Print
Color 15: Print "(2) ";: Color 14: Print "12 TEAM, 2 DIVISION, 162 GAME SCHEDULE": Print
Color 15: Print "(3) ";: Color 14: Print "14 TEAM, 2 DIVISION, 162 GAME SCHEDULE": Print
Color 15: Print "(4) ";: Color 14: Print "RETURN TO FULL COUNT MAIN MENU"
1305 I$ = GetKeyPress$
D3% = Val(I$)
If I$ = Chr$(27) Then D3% = 4
If D3% < 0 Or D3% > 4 Then 1305
If D3% = 4 Then 10

Spec$ = D$(2) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4000
ReDim libFiles$(0 To Count%)
For X = 1 To Count%
    libFiles$(X) = Space$(12)
Next
libFiles$(0) = Spec$
Call ReadFile(libFiles$())
'CALL SortStr(VARPTR(libFiles$(1)), Count, 0)

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

_MouseShow "DEFAULT"
1320 MAMenu libFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 1300
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 1320
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 1320
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 1320
F$ = libFiles$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310
Open D$(2) + F$ For Input As #2
Input #2, LG$
Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$
For X = 1 To 40
    Input #2, Z1$(X): Input #2, Z1%(X): Input #2, Z2$(X)
Next
Close 2
If D3% = 0 Then F$ = "154TEMP.SCX"
If D3% = 1 Then F$ = "162TENTM.SCX"
If D3% = 2 Then F$ = "16212TM.SCX"
If D3% = 3 Then F$ = "16214TM.SCX"
Open F$ For Random As #1 Len = 48
Y = LOF(1)
AP%(0) = Y / 48
BS% = Int((AP%(0) - 1) / 20)
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    Get #1, I
    For X = 0 To 18: NG%(I, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 1: YN$(I, X) = Q$(20 + X): Next
    VS%(I) = CVI(Q$(22)): HS%(I) = CVI(Q$(23))
    If NG%(I, 14) > 8 Then NG%(I, 14) = 0
    If NG%(I, 6) > 1 Then NG%(I, 6) = 1
    If NG%(I, 7) > 1 Then NG%(I, 7) = 1
    If NG%(I, 9) > 1 Then NG%(I, 9) = 1
    If NG%(I, 10) > 1 Then NG%(I, 10) = 1
    If NG%(I, 11) > 1 Then NG%(I, 11) = 1
Next
Close 1
GoSub 900
GoSub 4100
For I = 1 To 20
    Print Z1%(I), Z2$(I)
Next
For I = 1 To AP%(0)
    Print "GAME"; I
    Print NG%(I, 0), YN$(I, 0)
    Print NG%(I, 1), YN$(I, 1)
    If D3% = 0 Or D3% = 1 Then X = NG%(I, 0): NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): X = NG%(I, 1): NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X)
    If D3% = 2 And NG%(I, 0) <= 6 Then X = NG%(I, 0): Print X: NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): GoTo 3915
    If D3% = 2 And NG%(I, 0) > 6 Then X = NG%(I, 0): Print X: NG%(I, 0) = Z1%(X + 4): YN$(I, 0) = Z2$(X + 4)

    3915 If D3% = 2 And NG%(I, 1) <= 6 Then X = NG%(I, 1): Print X: NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X): GoTo 3916
    If D3% = 2 And NG%(I, 1) > 6 Then X = NG%(I, 1): Print X: NG%(I, 1) = Z1%(X + 4): YN$(I, 1) = Z2$(X + 4)

    3916 If D3% = 3 And NG%(I, 0) <= 7 Then X = NG%(I, 0): NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): GoTo 3917
    If D3% = 3 And NG%(I, 0) > 7 Then X = NG%(I, 0): NG%(I, 0) = Z1%(X + 3): YN$(I, 0) = Z2$(X + 3)

    3917 If D3% = 3 And NG%(I, 1) <= 7 Then X = NG%(I, 1): NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X): GoTo 3918
    If D3% = 3 And NG%(I, 1) > 7 Then X = NG%(I, 1): NG%(I, 1) = Z1%(X + 3): YN$(I, 1) = Z2$(X + 3)

    3918 Print "GAME"; I
    Print NG%(I, 0), YN$(I, 0)
    Print NG%(I, 1), YN$(I, 1)
Next
GoSub 350
GoTo 25

4100 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
'Print box top
Print " " + Chr$(201); String$(61, 196); Chr$(187) + " ";

'Print box sides
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(200); String$(61, 196); Chr$(188) + " ";

Locate 7, 32
Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE MONTREAL, STL68, NY76, CIN90, ETC"
Locate 7, 37: Input F$
If F$ = "" Then 21
Locate 13, 12: Print "ANY CHANGE (YN) "
22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 4100
Color 15, 0
F$ = F$ + ".SCD"
Return

'EDIT THE PARAMETERS TO SAVE A SPECIAL EVENTS BOXSCORE

5070 If Not _FileExists("EVENTS.BOX") Then 5075
Open "EVENTS.BOX" For Input As #1
For X = 1 To 13
    Input #1, E%(X)
Next
Close 1

5075 Cls: Color 14
Print "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES": X% = 2: Color 15
Print " 1. IF INNINGS ARE OVER "; E%(1)
Print " 2. A TEAM'S RUNS LESS THAN "; E%(2)
Print " 3. A TEAM'S RUNS MORE THAN "; E%(3)
Print " 4. A TEAM'S HITS LESS THAN "; E%(4)
Print " 5. A TEAM'S HITS MORE THAN "; E%(5)
Print " 6. BATTER'S HITS MORE THAN "; E%(6)
Print " 7. BATTER'S RUNS MORE THAN "; E%(7)
Print " 8. BATTER'S RBI'S MORE THAN "; E%(8)
Print " 9. BATTER'S 2B'S MORE THAN "; E%(9)
Print "10. BATTER'S 3B'S MORE THAN "; E%(10)
Print "11. BATTER'S HR'S MORE THAN "; E%(11)
Print "12. BATTER'S SB'S MORE THAN "; E%(12)
Print "13. PITCHER'S K'S MORE THAN "; E%(13)
GoTo 197

147 Locate 2: Input " 1. IF INNINGS ARE OVER "; E%(1): If C1 = 1 Then 197

162 Locate 3: Input " 2. A TEAM'S RUNS LESS THAN "; E%(2): If C1 = 1 Then 197

163 Locate 4: Input " 3. A TEAM'S RUNS MORE THAN "; E%(3): If C1 = 1 Then 197

148 Locate 5: Input " 4. A TEAM'S HITS LESS THAN "; E%(4): If C1 = 1 Then 197

178 Locate 6: Input " 5. A TEAM'S HITS MORE THAN "; E%(5): If C1 = 1 Then 197

150 Locate 7: Input " 6. BATTER'S HITS MORE THAN "; E%(6): If C1 = 1 Then 197

111 Locate 8: Input " 7. BATTER'S RUNS MORE THAN "; E%(7): If C1 = 1 Then 197

112 Locate 9: Input " 8. BATTER'S RBI'S MORE THAN "; E%(8): If C1 = 1 Then 197

153 Locate 10: Input " 9. BATTER'S 2B'S MORE THAN "; E%(9): If C1 = 1 Then 197

114 Locate 11: Input "10. BATTER'S 3B'S MORE THAN "; E%(10): If C1 = 1 Then 197

155 Locate 12: Input "11. BATTER'S HR'S MORE THAN "; E%(11): If C1 = 1 Then 197

156 Locate 13: Input "12. BATTER'S SB'S MORE THAN "; E%(12): If C1 = 1 Then 197

117 Locate 14: Input "13. PITCHER'S K'S MORE THAN "; E%(13): If C1 = 1 Then 197

197 C1 = 1
Locate 23, 1: Print "SELECT # TO CHANGE 1-13 (0 TO CONTINUE) ";
Input B9
On B9 GOTO 147, 162, 163, 148, 178, 150, 111, 112, 153, 114, 155, 156, 117
Open "EVENTS.BOX" For Output As #1
For X = 1 To 13
    Print #1, E%(X)
Next
Close 1
GoTo 4500

4000 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10

4010 Locate 10, 8
Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 4500

4020 Locate 10, 8
Call CSMsgBox("NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 4500

4050 Locate 10, 8
Call CSMsgBox("NO SCHEDULE FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10

4060 Locate 10, 8
Call CSMsgBox("BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU", 23, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4500 'BOXSCORE MAINTENANCE
Call TITLE
Erase menuItems$
menuItems$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
menuItems$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
menuItems$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
menuItems$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
menuItems$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
menuItems$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 16
Call box0(5, 15, 12, 65, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 6
On SELECTION% GOTO 5070, 5060, 5010, 5010, 5010
GoTo 10

5000 ReDim array$(500) 'holds the main editor string array
Locate 23, 1: Print "ENTER GAME # TO GO TO        "
Locate 23, 1: Input "ENTER GAME # TO GO TO "; NG%
If NG% < 1 Or NG% > AP%(0) Then 120
NS% = Int((NG% - 1) / 20)
BN$ = D$(1) + "box" + Mid$(Str$(NG%), 2, Len(Str$(NG%)))
GoSub 5050
GoTo 25

5010 If SELECTION% = 3 Then Spec$ = D$(1) + "BOX*."
If SELECTION% = 4 Then Spec$ = D$(1) + "*.SPC"
If SELECTION% = 5 Then Spec$ = D$(1) + "*.SPC"
GoSub 5020
If SELECTION% = 5 Then Spec$ = D$(1) + "BOX*.": GoSub 5020
GoTo 4500

5020 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 And SELECTION% = 4 Then 4010
If Count% = 0 And SELECTION% = 3 Then 4020
If Count% = 0 And SELECTION% = 5 Then Return
ReDim boxFiles$(0 To Count%)
For X = 1 To Count%
    boxFiles$(X) = Space$(12)
Next

boxFiles$(0) = Spec$
Call ReadFile(boxFiles$())
For X = 1 To Count%
    Kill (D$(1) + boxFiles$(X))
    Locate 12, 34: Print "DELETING "; boxFiles$(X)
Next
Return

5060 Spec$ = D$(1) + "*.SPC"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4010
ReDim boxFiles$(0 To Count%)
For X = 1 To Count%
    boxFiles$(X) = Space$(12)
Next
boxFiles$(0) = Spec$
Call ReadFile(boxFiles$())
'CALL SortStr(VARPTR(boxFiles$(1)), Count, 0)

5065 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT BOXSCORE TO VIEW": Color 15
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
Print "-SELECTS BOXSCORE";

_MouseShow "DEFAULT"
4520 MAMenu boxFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 4500
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 4520
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 4520
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 4520
F$ = boxFiles$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3911 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 5065
BN$ = D$(1) + F$
ReDim array$(500) 'holds the main editor string array
GoSub 5050
GoTo 5060

'EDIT A BOXSCORE LOADED FROM THE FILE NAME BN$

5050 If Not _FileExists(BN$) Then GoSub 4060: Return
Open BN$ For Input As #1
CURLINE = 1
While Not EOF(1)
    Line Input #1, array$(CURLINE)
    CURLINE = CURLINE + 1
Wend
Close #1

Cls '  show how QEdit saves the screen
Color 15, 0 'print instructions on bottom of screen
Locate 25, 1
Print " ESC - Quit    F1 - Help    F2 - Print Boxscore   F3 - Save Boxscore"; Tab(79);
'----- fill in editing parameters

Select Case Monitor% 'determine the monitor type
    Case 3, 5, 7, 9, 10, 12 'CGA, EGA, VGA monitors
        Ed.AColor = 23 'white on blue
    Case Else 'all other monitors
        Ed.AColor = 7 'white on black
End Select
    
Ed.Wrap = 0 'word wrap at column 65 (use 0 for to disable word wrap)
Ed.Frame = 0 'frame code (1 = show ruler line & scroll bars, 0 = don't)
Ed.Rows = 24 'number of text lines to display on the screen (3 - 25)
Ed.Wide = 80 'width of the active screen (10 - 80)
Ed.HTab = 4 'set tab stops to 4 spaces
    
Locate 1, 1 'locate the upper left corner of the edit window frame
Action = 1 'Action 1 to save underlying screen and
'  display text, operate in polled mode

'----- main loop for editing
Do
    '----- call QEdit in polled mode
    'CALL QEdit(array$(), Ky$, Action, Ed)
    '----- F2 function key pressed? - change margin

    If Ky$ = F2$ Then
        For I = 1 To CURLINE - 1
            LPrint array$(I)
        Next
    End If

    If Ky$ = F3$ Then

        Open BN$ For Output As #1
        For I = 1 To CURLINE - 1
            Print #1, array$(I)
        Next
        Close 1

        Call CSMsgBox("REVISED BOXSCORE HAS BEEN SAVED...PRESS ANY KEY TO CONTINUE", 31, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    End If

Loop Until Ky$ = Chr$(27) 'keep calling QEdit until Esc is pressed

'----- call QEdit again with an Action flag of 5 to restore the screen
'CALL QEdit(array$(), Ky$, 5, Ed)
Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
    Print "AUTO SCHEDULER"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1995";
    Locate 5, 1: Color , 0
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
