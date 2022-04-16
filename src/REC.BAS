'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Head-to-Head Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

5 Cls

6 Dim H(162), O$(162), V(162), L$(162), WP$(162), LP$(162), I(162), Z1$(40)
Dim Z2$(40), Z1%(40)
Dim HW(50), AW(50), HL(50), AL(50), R1(200), R2(200), R3(200), R4(200)

7 Dim B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21), B0%(22, 40)
Dim P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21), Q$(15)
Dim teamItems$(200), VV%(200), D$(3), menuItems$(5), K9(0) As Long
Dim DV$(4), TR4(4), TR3(4), TR2(4), TR1(4), THW(4), THL(4), TAW(4), TAL(4)
Dim Y As Long

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, D$(I)
Next
Close #1

600 Call TITLE
menuItems$(1) = "ANALYZE A TEAM'S RECORD                "
menuItems$(2) = "ANALYZE ANOTHER TEAM IN THE SAME LEAGUE"
menuItems$(3) = "SELECT A NEW TEAM AND LEAGUE           "
menuItems$(4) = "RETURN TO MAIN MENU                    "
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 22
Call box0(5, 21, 10, 61, 2, 15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
I = SELECTION%

612 If I = 1 Then 39

613 If I = 2 Then 620

614 If I = 3 Then 621

615 If I = 4 Then 700

39 Color 15, 0: Cls
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

182 If BO% = 1 Then BO% = 0: GoTo 600

183 Locate 2, 33: Color 15, 0: Color L%(11), L%(12)
Print T$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 39
GoTo 40

1100 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then BO% = 1: GoSub 4500: Return
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1103
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): VV%(NT) = I1

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
T = VV%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then BO% = 1: GoSub 4500: Return
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
Get #1, T
T$ = RTrim$(Q$(0))
For I = 1 To 13: L%(I) = CVI(Q$(I)): Next
Close 1
Return

40 ER = 0

119 B$ = T$
B$ = RTrim$(B$)
For XX = 4 To 11
    If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
QQ = 8
For XX = 11 To 4 Step -1
    If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
Cls

111 If Not _FileExists(D$(1) + Z$ + "S") Then GoSub 4550: GoTo 600
Open D$(1) + Z$ + "S" For Input As #1

115 Input #1, Y

120 For J = 1 To 162: Input #1, H(J): Next

122 For J = 1 To 162: Input #1, O$(J): Next

123 For J = 1 To 162: Input #1, V(J): Next

124 For J = 1 To 162: Input #1, L$(J): Next

125 For J = 1 To 162: Input #1, WP$(J): Next

126 For J = 1 To 162: Input #1, LP$(J): Next

127 For J = 1 To 162: Input #1, I(J): Next

130 Close #1

42 If SL = 1 Then 60

45 Spec$ = D$(2) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then GoSub 4575: GoTo 600
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
Locate , 33
Color 14
Print "SELECT LEAGUE": Color 15
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

If ExitCode% = 27 Then 600
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
LG$ = Left$(targetFile$, Len(targetFile$) - 4)

60 Color 15, 0
Cls
Color 12: Locate 12, 5
Print "V";: Color 7: Print "IEW OR ";: Color 12: Print "P";: Color 7: Print "RINT"

61 M$ = GetKeyPress$
VPP = 0

62 If UCase$(M$) = "P" Then 80

65 VPP = 1

80 Q$ = LG$ + ".LIB"
Cls

82 Open D$(2) + Q$ For Input As #3
Input #3, LG$: Input #3, D1$: Input #3, D2$: Input #3, D3$: Input #3, D4$
For X = 1 To 40
    Input #3, Z1$(X): Input #3, Z1%(X): Input #3, Z2$(X)
Next

85 Close #3
Locate 18, 18: Print "COMPUTING W/L RECORDS . . . PLEASE WAIT"

145 For Q = 1 To 40: AW(Q) = 0: HW(Q) = 0: AL(Q) = 0: HL(Q) = 0: R1(Q) = 0: R2(Q) = 0: R3(Q) = 0: R4(Q) = 0: Next

146 For K = 1 To 4: THW(K) = 0: THL(K) = 0: TAW(K) = 0: TAL(K) = 0: TR1(K) = 0: TR2(K) = 0: TR3(K) = 0: TR4(K) = 0: Next

148 For K = 1 To 40

    149 If Z1$(K) = "XXX" Then 181

    150 For I = 1 To Y

        152 If H(I) > V(I) And L$(I) = "A" And O$(I) = Z1$(K) Then AW(K) = AW(K) + 1

        153 If H(I) > V(I) And L$(I) = "H" And O$(I) = Z1$(K) Then HW(K) = HW(K) + 1

        154 If H(I) < V(I) And L$(I) = "A" And O$(I) = Z1$(K) Then AL(K) = AL(K) + 1

        155 If H(I) < V(I) And L$(I) = "H" And O$(I) = Z1$(K) Then HL(K) = HL(K) + 1

        156 If O$(I) = Z1$(K) And L$(I) = "H" Then R1(K) = R1(K) + H(I)

        157 If O$(I) = Z1$(K) And L$(I) = "H" Then R2(K) = R2(K) + V(I)

        158 If O$(I) = Z1$(K) And L$(I) = "A" Then R3(K) = R3(K) + H(I)

        159 If O$(I) = Z1$(K) And L$(I) = "A" Then R4(K) = R4(K) + V(I)

    180 Next I

181 Next K
RDD = 0
If VPP = 1 Then 190 Else GoTo 1105

190 DV$(1) = D1$

191 DV$(2) = D2$

192 DV$(3) = D3$

193 DV$(4) = D4$

200 Cls: Color 12

201 Locate 2, 35: Print Z$

202 Color 3: Locate 4: Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)

203 LOCATE 5: PRINT TAB(16); "W"; TAB(19); "L"; TAB(23); "PCT."; TAB(28); "RF"; TAB(32); "RA"; TAB(35); CHR$(179); TAB(38); "W"; TAB(41); "L"; TAB(45); "PCT."; TAB(50); "RF"; TAB(54); "RA"; TAB(58); CHR$(179); TAB(61); "W"; TAB(64); "L"; TAB(68);  _
"PCT."; TAB(73); "RF"; TAB(77); "RA"

205 RDD = RDD + 1

206 If RDD = 1 Then 210

207 If RDD = 2 Then 211

208 If RDD = 3 Then 212

209 If RDD = 4 Then 213

210 DD = 1: EE = 10: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 400 Else GoTo 215

211 DD = 11: EE = 20: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 400 Else GoTo 215

212 DD = 21: EE = 30: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 400 Else GoTo 215

213 DD = 31: EE = 40: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 400 Else GoTo 215

215 Color 11: Print DV$(RDD);: Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

218 For K = DD To EE

    219 If Z1$(K) = "XXX" Or Z1$(K) = T$ Then 250

    220 Color 15: Locate , 1: Print Left$(Z1$(K), 13);

    221 Color 7: Locate , 15: Print Using "##"; HW(K);

    222 Locate , 18: Print Using "##"; HL(K);

    223 If HW(K) = HW(K) + HL(K) And HW(K) <> 0 Then Locate , 21: Print "1.000";

    224 If HW(K) = 0 Then Locate , 22: Print ".000";

    225 If HW(K) > 0 And HW(K) < HW(K) + HL(K) Then Locate , 22: Print Using ".###"; (HW(K) / (HW(K) + HL(K)));

    226 Locate , 27: Print Using "###"; R1(K);

    227 Locate , 31: Print Using "###"; R2(K);
    Color 3: Print Tab(35); Chr$(179);

    228 Color 7: Locate , 37: Print Using "##"; AW(K);

    229 Locate , 40: Print Using "##"; AL(K);

    230 If AW(K) = AW(K) + AL(K) And AW(K) <> 0 Then Locate , 43: Print "1.000";

    231 If AW(K) = 0 Then Locate , 44: Print ".000";

    232 If AW(K) > 0 And AW(K) < AW(K) + AL(K) Then Locate , 44: Print Using ".###"; (AW(K) / (AW(K) + AL(K)));

    233 Locate , 49: Print Using "###"; R3(K);

    234 Locate , 53: Print Using "###"; R4(K);
    Color 3: Print Tab(58); Chr$(179);

    235 Color 7: Locate , 60: Print Using "##"; HW(K) + AW(K);

    236 Locate , 63: Print Using "##"; HL(K) + AL(K);

    237 If (HW(K) + AW(K)) = (HW(K) + AW(K) + HL(K) + AL(K)) And (HW(K) + AW(K)) <> 0 Then Locate , 66: Print "1.000";

    238 If (HW(K) + AW(K)) = 0 Then Locate , 67: Print ".000";

    239 If (HW(K) + AW(K)) > 0 And (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) Then Locate , 67: Print Using ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));

    240 Locate , 72: Print Using "###"; R1(K) + R3(K);

    241 Locate , 76: Print Using "###"; R2(K) + R4(K)
    Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179);

    242 THW(RDD) = THW(RDD) + HW(K)

    243 THL(RDD) = THL(RDD) + HL(K)

    244 TAW(RDD) = TAW(RDD) + AW(K)

    245 TAL(RDD) = TAL(RDD) + AL(K)

    246 TR1(RDD) = TR1(RDD) + R1(K)

    247 TR2(RDD) = TR2(RDD) + R2(K)

    248 TR3(RDD) = TR3(RDD) + R3(K)

    249 TR4(RDD) = TR4(RDD) + R4(K)

250 Next K

260 Print

261 Color 11: Locate , 5: Print "TOTALS";

262 Locate , 15: Print Using "##"; THW(RDD);

263 Locate , 18: Print Using "##"; THL(RDD);

264 If THW(RDD) = THW(RDD) + THL(RDD) And THW(RDD) <> 0 Then Locate , 21: Print "1.000";

265 If THW(RDD) = 0 Then Locate , 22: Print ".000";

266 If THW(RDD) > 0 And THW(RDD) < THW(RDD) + THL(RDD) Then Locate , 22: Print Using ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));

267 Locate , 27: Print Using "###"; TR1(RDD);

268 Locate , 31: Print Using "###"; TR2(RDD);
Color 3: Print Tab(35); Chr$(179);

269 Color 11: Locate , 37: Print Using "##"; TAW(RDD);

270 Locate , 40: Print Using "##"; TAL(RDD);

271 If TAW(RDD) = TAW(RDD) + TAL(RDD) And TAW(RDD) <> 0 Then Locate , 43: Print "1.000";

272 If TAW(RDD) = 0 Then Locate , 44: Print ".000";

273 If TAW(RDD) > 0 And TAW(RDD) < TAW(RDD) + TAL(RDD) Then Locate , 44: Print Using ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));

274 Locate , 49: Print Using "###"; TR3(RDD);

275 Locate , 53: Print Using "###"; TR4(RDD);
Color 3: Print Tab(58); Chr$(179);

280 Color 11: Locate , 59: Print Using "###"; THW(RDD) + TAW(RDD);

281 Locate , 62: Print Using "###"; THL(RDD) + TAL(RDD);

282 If THW(RDD) + TAW(RDD) = THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD) And THW(RDD) + TAW(RDD) <> 0 Then Locate , 66: Print "1.000";

283 If THW(RDD) + TAW(RDD) = 0 Then Locate , 67: Print ".000";

284 If THW(RDD) + TAW(RDD) > 0 And THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) Then Locate , 67: Print Using ".###"; (THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD));

285 Locate , 72: Print Using "###"; TR1(RDD) + TR3(RDD);

286 Locate , 76: Print Using "###"; TR2(RDD) + TR4(RDD)

390 Locate 22, 5: Color 14: Input "HIT ENTER TO CONTINUE"; DUMMY$

395 If RDD = 4 Then 400 Else GoTo 200

400 Cls: Color 12: Locate 2, 35: Print Z$

401 Color 3: Locate 4: Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)

402 LOCATE 5: PRINT TAB(16); "W"; TAB(19); "L"; TAB(23); "PCT."; TAB(28); "RF"; TAB(32); "RA"; TAB(35); CHR$(179); TAB(38); "W"; TAB(41); "L"; TAB(45); "PCT."; TAB(50); "RF"; TAB(54); "RA"; TAB(58); CHR$(179); TAB(61); "W"; TAB(64); "L"; TAB(68);  _
"PCT."; TAB(73); "RF"; TAB(77); "RA"

403 Color 11: Print "DIVISIONS";

404 Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

450 For W = 1 To 4

    451 If DV$(W) = "XXX" Or DV$(W) = "" Then 490 Else GoTo 461

    461 Color 15: Locate , 1: Print DV$(W);

    462 Color 7: Locate , 15: Print Using "##"; THW(W);

    463 Locate , 18: Print Using "##"; THL(W);

    464 If THW(W) = THW(W) + THL(W) And THW(W) <> 0 Then Locate , 21: Print "1.000"

    465 If THW(W) = 0 Then Locate , 22: Print ".000";

    466 If THW(W) > 0 And THW(W) < THW(W) + THL(W) Then Locate , 22: Print Using ".###"; (THW(W) / (THW(W) + THL(W)));

    467 Locate , 27: Print Using "###"; TR1(W);

    468 Locate , 31: Print Using "###"; TR2(W);
    Color 3: Print Tab(35); Chr$(179);

    469 Color 7: Locate , 37: Print Using "##"; TAW(W);

    470 Locate , 40: Print Using "##"; TAL(W);

    471 If TAW(W) = TAW(W) + TAL(W) And TAW(W) <> 0 Then Locate , 43: Print "1.000";

    472 If TAW(W) = 0 Then Locate , 44: Print ".000";

    473 If TAW(W) > 0 And TAW(W) < TAW(W) + TAL(W) Then Locate , 44: Print Using ".###"; (TAW(W) / (TAW(W) + TAL(W)));

    474 Locate , 49: Print Using "###"; TR3(W);

    475 Locate , 53: Print Using "###"; TR4(W);
    Color 3: Print Tab(58); Chr$(179);

    480 Color 7: Locate , 59: Print Using "###"; THW(W) + TAW(W);

    481 Locate , 62: Print Using "###"; THL(W) + TAL(W);

    482 If THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) And THW(W) + TAW(W) <> 0 Then Locate , 66: Print "1.000";

    483 If (THW(W) + TAW(W)) = 0 Then Locate , 67: Print ".000";

    484 If (THW(W) + TAW(W)) > 0 And THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) Then Locate , 67: Print Using ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));

    485 Locate , 72: Print Using "###"; TR1(W) + TR3(W);

    486 Locate , 76: Print Using "###"; TR2(W) + TR4(W)
    Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179);

490 Next W

491 THWW = 0: THLL = 0: TAWW = 0: TALL = 0

492 For I = 1 To 4

    493 THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)

494 Next

495 TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0

496 For I = 1 To 4

    497 TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)

498 Next

500 Print

501 Color 11: Locate , 3: Print "TOTALS";

502 Locate , 14: Print Using "###"; THWW;

503 Locate , 17: Print Using "###"; THLL;

504 If THWW = THWW + THLL And THWW <> 0 Then Locate , 21: Print "1.000";

505 If THWW = 0 Then Locate , 22: Print ".000";

506 If THWW > 0 And THWW < THWW + THLL Then Locate , 22: Print Using ".###"; THWW / (THWW + THLL);

507 Locate , 27: Print Using "###"; TR11;

508 Locate , 31: Print Using "###"; TR22;
Color 3: Print Tab(35); Chr$(179);

509 Color 11: Locate , 36: Print Using "###"; TAWW;

510 Locate , 39: Print Using "###"; TALL;

511 If TAWW = TAWW + TALL And TAWW <> 0 Then Locate , 43: Print "1.000";

512 If TAWW = 0 Then Locate , 44: Print ".000";

513 If TAWW > 0 And TAWW < TAWW + TALL Then Locate , 44: Print Using ".###"; TAWW / (TAWW + TALL);

514 Locate , 49: Print Using "###"; TR33;

515 Locate , 53: Print Using "###"; TR44;
Color 3: Print Tab(58); Chr$(179);

516 Color 11: Locate , 59: Print Using "###"; THWW + TAWW;

517 Locate , 62: Print Using "###"; THLL + TALL;

518 If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Locate , 66: Print "1.000";

519 If THWW + TAWW = 0 Then Locate , 67: Print ".000";

520 If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then Locate , 67: Print Using ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);

521 Locate , 72: Print Using "###"; TR11 + TR33;

522 Locate , 76: Print Using "###"; TR22 + TR44

523 Locate 22, 5: Color 14: Input "HIT ENTER TO CONTINUE"; DUMMY$

524 Locate 23: Color 15: Print "VIEW TEAM AGAIN (Y/N)"
525 I$ = GetKeyPress$

526 If UCase$(I$) = "Y" Then 528

527 GoTo 600

528 RDD = 0: GoTo 630

620 RDD = 0: SL = 1: VPP = 0: GoTo 39

621 RDD = 0: SL = 0: VPP = 0: GoTo 39

630 For K = 1 To 4: THW(K) = 0: THL(K) = 0: TAW(K) = 0: TAL(K) = 0: TR1(K) = 0: TR2(K) = 0: TR3(K) = 0: TR4(K) = 0: Next

632 GoTo 200

700 Color 15

703 Run "HELLO.EXE"

1105 LPrint Chr$(15)

'1101 WIDTH "LPT1:1", 130

1110 DV$(1) = D1$

1111 DV$(2) = D2$

1112 DV$(3) = D3$

1113 DV$(4) = D4$

1200 LPrint Tab(5); Z$

1201 LPrint Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOTALS -------"

1202 LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"

1204 RDD = RDD + 1

1205 If RDD = 1 Then 1210

2206 If RDD = 2 Then 1211

1207 If RDD = 3 Then 1212

1208 If RDD = 4 Then 1213

1210 DD = 1: EE = 10
If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 1401 Else GoTo 1215

1211 DD = 11: EE = 20
If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 1401 Else GoTo 1215

1212 DD = 21: EE = 30
If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 1401 Else GoTo 1215

1213 DD = 31: EE = 40
If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 1401 Else GoTo 1215

1215 LPrint Tab(2); DV$(RDD)

1218 For K = DD To EE

    1219 If Z1$(K) = "XXX" Or Z1$(K) = T$ Then 1250

    1220 LPrint Z1$(K);

    1221 LPrint Tab(21); Using "##"; HW(K);

    1222 LPrint Tab(25); Using "##"; HL(K);

    1223 If HW(K) = HW(K) + HL(K) And HW(K) <> 0 Then LPrint Tab(28); "1.000";

    1224 If HW(K) = 0 Then LPrint Tab(29); ".000";

    1225 If HW(K) > 0 And HW(K) < HW(K) + HL(K) Then LPrint Tab(29); Using ".###"; (HW(K) / (HW(K) + HL(K)));

    1226 LPrint Tab(35); Using "###"; R1(K);

    1227 LPrint Tab(39); Using "###"; R2(K);

    1228 LPrint Tab(53); Using "##"; AW(K);

    1229 LPrint Tab(57); Using "##"; AL(K);

    1230 If AW(K) = AW(K) + AL(K) And AW(K) <> 0 Then LPrint Tab(60); "1.000";

    1231 If AW(K) = 0 Then LPrint Tab(61); ".000";

    1232 If AW(K) > 0 And AW(K) < AW(K) + AL(K) Then LPrint Tab(61); Using ".###"; (AW(K) / (AW(K) + AL(K)));

    1233 LPrint Tab(67); Using "###"; R3(K);

    1234 LPrint Tab(71); Using "###"; R4(K);

    1235 LPrint Tab(85); Using "##"; HW(K) + AW(K);

    1236 LPrint Tab(90); Using "##"; HL(K) + AL(K);

    1237 If HW(K) + AW(K) = HW(K) + AW(K) + HL(K) + AL(K) And HW(K) + AW(K) <> 0 Then LPrint Tab(95); "1.000";

    1238 If (HW(K) + AW(K)) = 0 Then LPrint Tab(96); ".000";

    1239 If (HW(K) + AW(K)) > 0 And (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) Then LPrint Tab(96); Using ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
    LPrint Tab(102); Using "###"; R1(K) + R3(K);

    1241 LPrint Tab(106); Using "###"; R2(K) + R4(K)

    1242 THW(RDD) = THW(RDD) + HW(K)

    1243 THL(RDD) = THL(RDD) + HL(K)

    1244 TAW(RDD) = TAW(RDD) + AW(K)

    1245 TAL(RDD) = TAL(RDD) + AL(K)

    1246 TR1(RDD) = TR1(RDD) + R1(K)

    1247 TR2(RDD) = TR2(RDD) + R2(K)

    1248 TR3(RDD) = TR3(RDD) + R3(K)

    1249 TR4(RDD) = TR4(RDD) + R4(K)

1250 Next K

1261 LPrint Tab(5); "TOTALS";

1262 LPrint Tab(21); Using "##"; THW(RDD);

1263 LPrint Tab(25); Using "##"; THL(RDD);

1264 If THW(RDD) = THW(RDD) + THL(RDD) And THW(RDD) <> 0 Then LPrint Tab(28); "1.000";

1265 If THW(RDD) = 0 Then LPrint Tab(29); ".000";

1266 If THW(RDD) > 0 And THW(RDD) < THW(RDD) + THL(RDD) Then LPrint Tab(29); Using ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));

1267 LPrint Tab(35); Using "###"; TR1(RDD);

1268 LPrint Tab(39); Using "###"; TR2(RDD);

1269 LPrint Tab(53); Using "##"; TAW(RDD);

1270 LPrint Tab(57); Using "##"; TAL(RDD);

1271 If TAW(RDD) = TAW(RDD) + TAL(RDD) And TAW(RDD) <> 0 Then LPrint Tab(60); "1.000";

1272 If TAW(RDD) = 0 Then LPrint Tab(61); ".000";

1273 If TAW(RDD) > 0 And TAW(RDD) < TAW(RDD) + TAL(RDD) Then LPrint Tab(61); Using ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));

1274 LPrint Tab(67); Using "###"; TR3(RDD);

1275 LPrint Tab(71); Using "###"; TR4(RDD);

1280 LPrint Tab(84); Using "###"; THW(RDD) + TAW(RDD);

1281 LPrint Tab(89); Using "###"; THL(RDD) + TAL(RDD);

1282 If THW(RDD) + TAW(RDD) = THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) And THW(RDD) + TAW(RDD) <> 0 Then LPrint Tab(95); "1.000";

1283 If THW(RDD) + TAW(RDD) = 0 Then LPrint Tab(96); ".000";

1284 If THW(RDD) + TAW(RDD) > 0 And THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) Then LPrint Tab(96); Using ".###"; ((THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD)));

1285 LPrint Tab(102); Using "###"; TR1(RDD) + TR3(RDD);

1286 LPrint Tab(106); Using "###"; TR2(RDD) + TR4(RDD)

1287 LPrint

1395 If RDD = 4 Then 1401 Else GoTo 1204

1401 LPrint: LPrint: LPrint: LPrint: LPrint Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOTALS -------"

1402 LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"

1403 Print

1450 For W = 1 To 4

    1451 If DV$(W) = "XXX" Or DV$(W) = "" Then 1490 Else GoTo 1461

    1461 LPrint Tab(2); DV$(W);

    1462 LPrint Tab(21); Using "##"; THW(W);

    1463 LPrint Tab(25); Using "##"; THL(W);

    1464 If THW(W) = THW(W) + THL(W) And THW(W) <> 0 Then LPrint Tab(28); "1.000";

    1465 If THW(W) = 0 Then LPrint Tab(29); ".000";

    1466 If THW(W) > 0 And THW(W) < THW(W) + THL(W) Then LPrint Tab(29); Using ".###"; (THW(W) / (THW(W) + THL(W)));

    1467 LPrint Tab(35); Using "###"; TR1(W);

    1468 LPrint Tab(39); Using "###"; TR2(W);

    1469 LPrint Tab(53); Using "##"; TAW(W);

    1470 LPrint Tab(57); Using "##"; TAL(W);

    1471 If TAW(W) = TAW(W) + TAL(W) And TAW(W) <> 0 Then LPrint Tab(60); "1.000";

    1472 If TAW(W) = 0 Then LPrint Tab(61); ".000";

    1473 If TAW(W) > 0 And TAW(W) < TAW(W) + TAL(W) Then LPrint Tab(61); Using ".###"; (TAW(W) / (TAW(W) + TAL(W)));

    1474 LPrint Tab(67); Using "###"; TR3(W);

    1475 LPrint Tab(71); Using "###"; TR4(W);

    1480 LPrint Tab(84); Using "###"; THW(W) + TAW(W);

    1481 LPrint Tab(89); Using "###"; THL(W) + TAL(W);

    1482 If THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) And THW(W) + TAW(W) <> 0 Then LPrint Tab(95); "1.000";

    1483 If (THW(W) + TAW(W)) = 0 Then LPrint Tab(96); ".000";

    1484 If (THW(W) + TAW(W)) > 0 And THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) Then LPrint Tab(96); Using ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));

    1485 LPrint Tab(102); Using "###"; TR1(W) + TR3(W);

    1486 LPrint Tab(106); Using "###"; TR2(W) + TR4(W)

1490 Next W

1491 THWW = 0: THLL = 0: TAWW = 0: TALL = 0

1492 For I = 1 To 4

    1493 THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)

1494 Next

1495 TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0

1496 For I = 1 To 4

    1497 TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)

1498 Next

1500 LPrint Tab(5); "TOTALS";

1502 LPrint Tab(20); Using "###"; THWW;

1503 LPrint Tab(24); Using "###"; THLL;

1504 If THWW = THWW + THLL And THWW <> 0 Then LPrint Tab(28); "1.000";

1505 If THWW = 0 Then LPrint Tab(29); ".000";

1506 If THWW > 0 And THWW < THWW + THLL Then LPrint Tab(29); Using ".###"; THWW / (THWW + THLL);

1507 LPrint Tab(35); Using "###"; TR11;

1508 LPrint Tab(39); Using "###"; TR22;

1509 LPrint Tab(52); Using "###"; TAWW;

1510 LPrint Tab(56); Using "###"; TALL;

1511 If TAWW = TAWW + TALL And TAWW <> 0 Then LPrint Tab(60); "1.000";

1512 If TAWW = 0 Then LPrint Tab(61); ".000";

1513 If TAWW > 0 And TAWW < TAWW + TALL Then LPrint Tab(61); Using ".###"; TAWW / (TAWW + TALL);

1514 LPrint Tab(67); Using "###"; TR33;

1515 LPrint Tab(71); Using "###"; TR44;

1516 LPrint Tab(84); Using "###"; THWW + TAWW;

1517 LPrint Tab(89); Using "###"; THLL + TALL;

1518 If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then LPrint Tab(95); "1.000";

1519 If THWW + TAWW = 0 Then LPrint Tab(96); ".000";

1520 If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then LPrint Tab(96); Using ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);

1521 LPrint Tab(102); Using "###"; TR11 + TR33;

1522 LPrint Tab(106); Using "###"; TR22 + TR44

1523 LPrint Chr$(18)

'1524 WIDTH "LPT1:1", 80

1525 LPrint Chr$(12)

1552 GoTo 600

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

4575 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
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
