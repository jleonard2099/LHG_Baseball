'$INCLUDE: 'COMMON.GW'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

'The following SOURCE files combine
'to make the full game
'LOADER + SETUP + RAREPLAY + ROUTINES

_Title "Full Count Baseball - PLAY BALL!"
$ExeIcon:'./lhg_fcbase.ico'
_Icon	
_AllowFullScreen

DefInt A-Z
DECLARE SUB ADDPIT (D,I5,I6)
DECLARE SUB ADDSTTS (p, D)
DECLARE SUB ADJRUNNERS (S2%, p)
DECLARE SUB ADV1BASE (i)
DECLARE SUB ADV2BASES (i)
DECLARE SUB ADV3BASES (I3, p, D)
DECLARE SUB ADVANCEMENT (P,D,S2%,F%,I3)
DECLARE SUB BATDISP (PG, P9)
DECLARE SUB BATOFF (p, I5, I6)
DECLARE SUB BATTER1 (i)
DECLARE SUB BATTER2 (i)
DECLARE SUB BATTER3 (I3, p, D)
DECLARE SUB BLANKLINE ()
DECLARE SUB CHARGERUN (D,i)
DECLARE SUB CHECKSAVE (D, p, S4%)
DECLARE SUB CLEARBASE (i)
DECLARE SUB COLCHK (P9, I)
DECLARE SUB CONVNAME (QQ,I)
DECLARE SUB CREDITASSIST (W%,D)
DECLARE SUB CREDITPUTOUT (I3,FB2P%,AST%,D)
DECLARE SUB CREDITREGPO (W%,D)
DECLARE SUB DELAY ()
DECLARE SUB DELIVERY (D)
DECLARE SUB DISPBAVG (D)
DECLARE SUB DISPRUN (I2, p, X)
DECLARE SUB DPPBP ()
DECLARE SUB DPRELAY ()
DECLARE SUB DRAWBOX ()
DECLARE SUB EJECTINC (EJ%,X$)
DECLARE SUB EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6)
DECLARE SUB FLDAVG (D, W%, I3)
DECLARE SUB FLDDISP (BC%, p, U%, D)
DECLARE SUB FLDERR (F%, W%, D, I3, SB%, S2%, p)
DECLARE SUB GETAKEY ()
DECLARE SUB GETFIELDER (W%, p, I1, D)
DECLARE SUB GETFIELDNAME (W%, D)
DECLARE SUB GETOUTFIELDER (W%, p, I1, D)
DECLARE SUB GETSTATS ()
DECLARE SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)
DECLARE SUB GROUNDERS (W%, D)
DECLARE SUB HITTINGLINES (l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
DECLARE SUB HOLDSTATUS (H0%)
DECLARE SUB INCBATDF (D, I5, I6)
DECLARE SUB INCBATOFF (P,I5,I6)
DECLARE SUB INCBATOFF (P,I5,I6)
DECLARE SUB INCREMENTHIT (p, D, MO)
DECLARE SUB INFO ()
DECLARE SUB INITVARI ()
DECLARE SUB INJPIT (P,D)
DECLARE SUB INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6)
DECLARE SUB INNDISP (I2, i)
DECLARE SUB LDRIVE ()
DECLARE SUB LETTER (I$)
DECLARE SUB LINSWAP (P9)
DECLARE SUB LONGFLY (D,W%)
DECLARE SUB LRADJ (A!, A1!)
DECLARE SUB MAINSCREEN (D, p, BC%, U%, MO)
DECLARE SUB NEWLINES ()
DECLARE SUB NEWLINES (P9)
DECLARE SUB NORMALIZE (I7!, I2!, I6!, K8!)
DECLARE SUB OUTFIELDERR ()
DECLARE SUB PITCHERNAME (W%, D)
DECLARE SUB PITPLACEMENT (p, X$)
DECLARE SUB PITSTAM (P9)
DECLARE SUB PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO)
DECLARE SUB POPUP ()
DECLARE SUB POPUPPIT (WK, M)
DECLARE SUB POPUPSTATS ()
DECLARE SUB POSITION (I%)
DECLARE SUB POSSWAP (P9)
DECLARE SUB POSTGAME ()
DECLARE SUB RANDNUM (rn)
DECLARE SUB REPLACEMENTS (I7%, I8%, i9%)
DECLARE SUB RecordOut (i, I3)
DECLARE SUB RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
DECLARE SUB SCOREBOARD ()
DECLARE SUB SECOND2FIRST (D)
DECLARE SUB SHOWBATTERS (P9)
DECLARE SUB SHOWFIELD (P9%)
DECLARE SUB SHOWSPLIT (P9%)
DECLARE SUB SINGLEPBP (S2%, D, W%)
DECLARE SUB SOMESTATS (V, p, F%, D)
DECLARE SUB STRIKEOUT (U%, D)
DECLARE SUB TAGADVANCE (S2%, I8, D)
DECLARE SUB THROW2FIRST (D)

'DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)

Type PBP
    YY As String * 30
End Type

Dim BP(1), BP(9), F(9), T1(1)

Dim B2%(1, 22), BB%(9), CZ%(1), DYS%(1, 21), GS%(21, 1), H0%(1)
Dim INJ%(1, 22), M%(9), MF%(1), NB%(1, 4), P3%(1, 21), P4%(22), PS%(23, 1)
Dim SU%(1,1), SP%(1), T%(22), U%(50), UM%(1, 9, 4), UV%(200), VA%(26)

Dim DH$(1), DL$(1), DN$(1), LGT$(2), MN$(10)
Dim PARK$(83), PT$(1), PU$(8), U6$(2), UMP$(50)
Dim V$(200), VI$(1), WD$(9), WE$(1)

Dim E As Single, JJ As Single, Q1 As Single, QQ As Single
Dim R As Single, SE As Single, Y As Long

Dim Shared D,P, U6, U9, RE, RD

Dim Shared MON(5,14), VV(1), VV1(1)

Dim Shared AP%, CGERA%, DH%, DR%, EJ%, IJ%, IJL%, INNING%
Dim Shared PC%, SC%, TA%, TP%, W%, WS%

Dim Shared A5%(4), AP%(2), B%(1, 22, 80), B0%(3, 2), B1%(3, 1), B3%(1, 9), B4%(1, 7, 9)
Dim Shared B5%(1, 22, 21), B7%(1, 9), B8%(1, 22, 21), B9%(1, 10), BT%(1, 9, 9)
Dim Shared CF%(9, 2), CSS%(1,22,6), D0%(1), D3%(1), DP%(1), GK%(1)
Dim Shared L%(1, 13), L0%(1), LB%(9, 1), MG%(1,999), NG%(18), O%(1)
Dim Shared P%(1, 21, 89), P1%(1), P2%(1), P5%(1, 21, 41), P6%(1), P8%(1, 21, 41), PR%(24)
Dim Shared S%(1, 26), S1%(1), S6%(1, 2), S8%(1, 1), SA%(1, 24), SO%(5, 2)
Dim Shared T1%(), T3%(22), TP%(11, 2), TS%(1, 11), X0%(1, 2, 23)

Dim Shared S$, WI$

Dim Shared A$(1), B$(1, 22), B1$(4), C$(11), D$(3), EJ$(20), F$(10), H$(4), H0$(1)
Dim Shared IJ$(20), M$(1), N$(1), NN$(1), P$(1, 21), PC$(4), Q$(23), Q3$(100)
Dim Shared S$(1), SC$(4), TM$(4), U$(4), X$(12), YN$(1)

Dim Shared displayPause AS SINGLE, K9(1) AS SINGLE, WF AS SINGLE
Dim Shared B1!(9)
Dim Shared BUCK AS PBP

ReDim Q2$(2056)

'commenting out here as it is mostly irrelevant
'CALL InitMouse(There)

Randomize Timer

Cls

BK$ = "                         "
VI$(0) = "VISITING TEAM"
VI$(1) = "HOME TEAM"
TM$(1) = "HOT": TM$(2) = "WARM": TM$(3) = "COOL": TM$(4) = "COLD"
PT$(0) = "RIGHT"
PT$(1) = "LEFT "
WD$(2) = "LEFT TO RIGHT AT"
WD$(3) = "RIGHT TO LEFT AT"
WD$(4) = "BLOWING IN FROM LEFT AT"
WD$(5) = "BLOWING IN FROM CENTER AT"
WD$(6) = "BLOWING IN FROM RIGHT AT"
WD$(7) = "BLOWING OUT TO LEFT AT"
WD$(8) = "BLOWING OUT TO CENTER AT"
WD$(9) = "BLOWING OUT TO RIGHT AT"
SC$(1) = "CLEAR": SC$(2) = "PARTLY CLOUDY": SC$(3) = "OVERCAST": SC$(4) = "DOME"
PC$(1) = "THUNDERSTORMS": PC$(2) = "RAIN": PC$(3) = "SPRINKLES": PC$(4) = "NONE"

51 SP%(0) = -1: SP%(1) = -1

56 U6$(0) = "HUMAN OPPONENT      "
U6$(1) = "COMPUTER OPPONENT   "
U6$(2) = "COMPUTER VS COMPUTER"
PU$(0) = "1980 - PRESENT"
PU$(1) = "1973 - 1979   "
PU$(2) = "1955 - 1972   "
PU$(3) = "1946 - 1954   "
PU$(4) = "1933 - 1945   "
PU$(5) = "1919 - 1932   "
PU$(6) = "1909 - 1918   "
PU$(7) = "1894 - 1908   "
PU$(8) = "1876 - 1893   "

57 LGT$(0) = "AL GAME         "
LGT$(1) = "NL GAME         "
LGT$(2) = "INTERLEAGUE GAME"

58 DH$(0) = "NO DH "
DH$(1) = "USE DH"
UMP$(0) = "AL"
UMP$(1) = "NL"
UMP$(2) = "NL"
WE$(0) = "YES"
WE$(1) = "NO "

59 DL$(0) = "LEAGUE GAME    "
DL$(1) = "EXHIBITION GAME"
If Not _FileExists("DEFERA") Then Open "DEFERA" For Output As #1: For I = 1 To 14: Print #1, 0: Next: Close 1
Open "DEFERA" For Input As #1: Input #1, U6: Input #1, LGT: Input #1, DH%
Input #1, DL: Input #1, MO%: Input #1, DN%: Input #1, WE%
Input #1, IJ%: Input #1, LR%: Input #1, DR%: Input #1, MP%
Input #1, G4: Input #1, VG%: Input #1, CGERA%
Close 1

60 Open "DEV.BAS" For Input As #1

70 For I = 0 To 3: Input #1, D$(I): Next I

80 Close #1

85 MN$(4) = "APRIL"
MN$(5) = "MAY"
MN$(6) = "JUNE"
MN$(7) = "JULY"
MN$(8) = "AUGUST"
MN$(9) = "SEPTEMBER"
MN$(10) = "OCTOBER"

86 DN$(0) = "AFTERNOON"
DN$(1) = "NIGHT"
B1$(0) = "LP"
B1$(1) = "L"
B1$(2) = "S"
B1$(3) = "R"
B1$(4) = "RP"

90 Data A,B,C,D,E,F,G,H,I,J,K,L,M,dh,p,"c ",1b,2b,3b,ss,lf,cf,rf,ph,pr

100 Cls: For RA = 1 To 60: Randomize Timer: Next: P = 1

110 For I = 0 To 12: Read X$(I): Next I

120 For I = 0 To 11: Read C$(I): Next I

140 H$(1) = "1b": H$(2) = "2b": H$(3) = "3b": H$(4) = "home"

144 For I = 0 To 1: For I1 = 0 To 4: NB%(I, I1) = -1: Next: Next
For I = 0 To 1: P1%(I) = -1: P6%(I) = -1
Next
SP%(0) = -1: SP%(1) = -1
For I = 0 To 1: For J = 0 To 9: M%(J) = 0: B7%(I, J) = -1: B3%(I, J) = -1: Next: Next
Call INITVARI
If Not _FileExists("GAME82") Then 6
Open "GAME82" For Input As #2
Input #2, FF$
For I = 0 To 2: Input #2, AP%(I): Next
Close 2
Open D$(0) + FF$ For Random As #1 Len = 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q2$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q2$(22), 2 As Q2$(23), 3 As Q2$(20), 3 As Q2$(21)
Get #1, AP%(0) + AP%(2)
For X = 0 To 18: NG%(X) = CVI(Q2$(X + 1))
Next
For X = 0 To 1: YN$(X) = Q2$(20 + X)
Next
VS% = CVI(Q2$(22)): HS% = CVI(Q2$(23))
Close 1
AP% = 1

6 If AP% <> 1 Then 170
U6 = NG%(8)
DH% = NG%(3)
G4 = NG%(4) / 10
MO% = NG%(17)
WE% = NG%(5)
VG% = NG%(11)
IJ% = NG%(6)
LR% = NG%(7)
DR% = NG%(9)
MP% = NG%(10)
CGERA% = NG%(14)
DN% = 1
If NG%(15) < 1600 Then DN% = 0
If U6 = 0 Then 160
If U6 = 3 Then U6 = 2: GoTo 160
If U6 = 1 Then U9 = 0: GoTo 160
U9 = 1: U6 = 1

160 GoTo 260

170 Call INFO

171 Color , 0: Locate 3, 1
Color 14: Print "(1) ";: Color 15: Print U6$(U6)

172 Color 14: Print "(2) ";: Color 15: Print LGT$(LGT)

173 Color 14: Print "(3) ";: Color 15: Print DH$(DH%)

174 Color 14: Print "(4) ";: Color 15: Print DL$(DL)

175 Color 14: Print "(5) ";: Color 15: Print MN$(MO%)

177 Color 14: Print "(6) ";: Color 15: Print DN$(DN%)

179 Color 14: Print "(7) ";: Color 15: Print "PROCEED TO TEAM SELECTION"

180 Color 14: Print "(8) ";: Color 15: Print "USE CLIMATE/WIND/TEMP DATA "; WE$(WE%)
Color 14: Print "(9) ";: Color 15: Print "USE INJURIES "; WE$(IJ%)

181 Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS AS DEFAULT"
Color 14: Print "(A) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; WE$(LR%)
Color 14: Print "(B) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; WE$(DR%)
Color 14: Print "(C) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; WE$(MP%)
Color 14: Print "(D) ";: Color 15: Print Using "DISPLAY PAUSE: #.# "; G4
Color 14: Print "(E) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; WE$(VG%)
Color 14: Print "(F) ";: Color 15: Print "PITCHER USAGE ERA: "; PU$(CGERA%)

183 Color 7, 0: I$ = GetKeyPress$
If I$ = Chr$(27) Then Run "HELLO.EXE"
Call LETTER(I$)

185 I = Val(I$): If I < 0 Or I > 16 Then 183

190 On I + 1 GOTO 205, 191, 193, 195, 197, 199, 201, 260, 204, 206, 207, 208, 209, 210, 211, 212

191 U6 = U6 + 1: If U6 > 2 Then U6 = 0
GoTo 171

211 VG% = 1 - VG%: GoTo 171

206 IJ% = 1 - IJ%: GoTo 171

207 LR% = 1 - LR%: GoTo 171

208 DR% = 1 - DR%: GoTo 171

193 LGT = LGT + 1: If LGT > 2 Then LGT = 0
GoTo 171

195 DH% = 1 - DH%: GoTo 171

197 DL = 1 - DL: GoTo 171

209 MP% = 1 - MP%: GoTo 171

199 MO% = MO% + 1: If MO% > 10 Then MO% = 4
Locate 7, 4: Print BK$: GoTo 171

201 DN% = 1 - DN%
Locate 8, 4
Print BK$
GoTo 171

212 CGERA% = CGERA% + 1
If CGERA% > 8 Then CGERA% = 0
GoTo 171

204 WE% = 1 - WE%: GoTo 171

210 Locate 16, 22: Input G4: GoTo 171

205 Open "DEFERA" For Output As #1: Print #1, U6: Print #1, LGT: Print #1, DH%
Print #1, DL: Print #1, MO%: Print #1, DN%: Print #1, WE%
Print #1, IJ%: Print #1, LR%: Print #1, DR%: Print #1, MP%
Print #1, G4: Print #1, VG%: Print #1, CGERA%
Close 1
GoTo 171

260 YV = 1: YM = 0

261 If Not _FileExists(D$(0) + "UMPIRES." + UMP$(LGT)) Then 265
YU = 0
Open D$(0) + "UMPIRES." + UMP$(LGT) For Input As #1
For I1 = 1 To 50
    Input #1, UMP$(I1)
    Input #1, U%(I1)
Next
Close #1
For I1 = 1 To 50
    If UMP$(I1) <> "" Then YU = YU + 1
Next
If YU < 4 Then 265

262 YU = Int(Rnd(1) * 50) + 1: If UMP$(YU) = "" Or YU = YM Then 262

263 U$(YV) = UMP$(YU): YM = YU: YV = YV + 1: If YV < 5 Then 262

264 GoTo 270

265 U$(1) = "RHODES": U$(2) = "WINBERRY": U$(3) = "TIPPETT": U$(4) = "MILLER"

270 For I = 0 To 1
    If AP% = 1 Then T1(I) = NG%(I): YN$ = YN$(I): GoSub 300: GoTo 2860

    280 Call INFO

    2800 Color 15, 0

    Cls
    JJ = 10
    Locate 6, 8
    Color 15, 4

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    JJ = 9
    For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8

    'Print box bottom
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";


    Locate 7, 32

    Locate , 12
    Print VI$(I)

    Locate , 12: Print "ENTER DISK ID: ": Print
    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    Print
    Locate , 12: Print "MAKE SURE NUM LOCK KEY IS OFF TO USE ARROW KEYS"
    Locate , 12: Print "AND PG UP/PG DN KEYS"
    Print
    Locate , 12: Print "(JUST HIT ENTER TO USE PREVIOUS ID, IF SAME)"
    Locate 8, 27: Input NY$
    Color 15, 0
    If NY$ = "" Then GoSub 8102: GoTo 2856
    YN$ = NY$
    For I1 = 1 To 200: V$(I1) = "": Next

    2857 GoSub 8100

    2856 If BO% = 1 Then BO% = 0: GoTo 2800

    2858 Locate 2, 33: Color L%(I, 11), L%(I, 12): Print A$(I): Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
    I$ = GetKeyPress$

    2859 If UCase$(I$) = "Y" Then 2800

    2860 YN$(I) = YN$
    Color 15, 0
    A$(I) = RTrim$(A$(I))

430 Next I

431 N = 1

440 If U6 = 1 And AP% <> 1 Then GoSub 2430

450 For P9 = 0 To 1
    GoSub 4500
    GoSub 2040
    GoSub 1590

    460 GoSub 1060
Next P9

470 For P9 = 0 To 1

    480 Cls: Call INFO
    Locate 3, 1
    Color L%(P9, 11), L%(P9, 12): Print A$(P9): Color 7, 0: Print
    Color 14: Print "(0) ";: Color 15: Print "INPUT A LINEUP MANUALLY": Print
    Color 14: Print "(1) ";: Color 15: Print "SELECT A LINEUP FROM MANAGER PROFILE": Print
    Color 14: Print "(2) ";: Color 15: Print "COMPUTER DETERMINE LINEUP RANDOMLY": Print
    Color 14: Print "(3) ";: Color 15: Print "COMPUTER DETERMINE LINEUP FROM MANAGER PROFILE"
    If AP% = 1 And NLF% = 1 Then NLF% = 0: I$ = "2": GoTo 491
    If AP% = 1 And MP% = 0 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "1": GoTo 491
    If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "2": GoTo 491
    490 I$ = GetKeyPress$
    If I$ = Chr$(27) Then 170

    491 I1 = Val(I$): If I1 < 0 Or I1 > 3 Then 490
    If I$ = "2" Then GoSub 5000: If LE = 0 Then 520

    496 If LE = 1 Then LE = 0: GoTo 480

    500 If I$ <> "1" And I$ <> "3" Then GoSub 2160: GoSub 2310: GoSub 540

    510 If I$ = "1" Or I$ = "3" Then GoSub 1830

    520 Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
    Locate , 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
    If RP = 1 Then RP = 0: GoTo 480

525 Next P9: GoSub 2390

530 W5 = 0: W6 = 1: VV(0) = 1: VV1(0) = 9: VV(1) = 1: VV1(1) = 9: GoTo 2320

540 For I = 0 To 9: M%(I) = 0: Next I: GoSub 1370

550 For I = 1 To 9
    Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER": Locate , 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

    560 Call POSITION(I): Print "                  ";: Call POSITION(I)
    561 I$ = GetKeyPress$

    570 Call LETTER(I$)
    If I$ = Chr$(27) Then
        For J = 1 To 9
            M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1
        Next
        C1 = 0
        For J = 0 To 22
            B%(P9, J, 21) = 0
        Next
        RP = 1
        For J = 0 To 22
            If B%(P9, J, 31) <> 0 Then B%(P9, J, 21) = 99
        Next
        GoTo 480
    End If

    580 I1 = Val(I$): If UCase$(I$) = "X" Then 540

    590 For J = 1 To 9: If J = I Or B3%(P9, J) = I1 And B7%(P9, J) = 1 Then 610

        600 If B3%(P9, J) = I1 And UCase$(I$) <> "P" Then 560

    610 Next J

    620 If DH% = 1 And UCase$(I$) = "P" Then 560

    630 If B%(P9, I1, 21) <> 0 And UCase$(I$) <> "P" Then 560

    640 If UCase$(I$) = "P" Then 760

    650 B3%(P9, I) = I1: Print Using "# "; I;: Print B$(P9, B3%(P9, I));

    660 Locate , 74
    680 I$ = GetKeyPress$

    690 If Val(I$) = 1 Then 680

    700 For J = 1 To 9: If J = I Then 720

        710 If Val(I$) = B7%(P9, J) Then 680

    720 Next J

    730 If DH% = 0 And Val(I$) = 0 Or UCase$(I$) = "X" Then 560

    740 Print C$(Val(I$));: B7%(P9, I) = Val(I$)

    750 B9%(P9, B7%(P9, I)) = B3%(P9, I): M%(Val(I$)) = 1

    751 Locate I1 + 2, 2: Print "*";: Color L%(P9, 11), L%(P9, 12): Print B$(P9, I1): Color 15, 0: GoSub 1370: GoTo 830

    760 For J = 1 To 9: If J = I Then 780

        770 If B3%(P9, J) = P1%(P9) And B7%(P9, J) = 1 Then 560

    780 Next J

    790 B3%(P9, I) = P1%(P9): Print P$(P9, P1%(P9));: M%(Val(I$)) = 1

    800 Locate , 72

    820 Print "P ";: B7%(P9, I) = 1: B9%(P9, 1) = P1%(P9): M%(1) = 1: GoSub 1370

    830 If C1 = 1 Then 850

840 Next I

850 C1 = 1: Locate 25, 1: Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
Color 15, 0
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "0": GoTo 861
860 I$ = GetKeyPress$

861 If UCase$(I$) = "R" Or I$ = Chr$(27) Then For J = 1 To 9: M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1: Next: C1 = 0: For J = 0 To 22: B%(P9, J, 21) = 0: Next: RP = 1: GoTo 1001
If UCase$(I$) = "L" Then Call LINSWAP(P9): GoTo 850
If UCase$(I$) = "P" Then Call POSSWAP(P9): GoTo 850

870 I = Val(I$): If I < 1 Or I > 9 Then 890

880 Locate B3%(P9, I) + 2 - B4%, 2: Print " "; B$(P9, B3%(P9, I)): GoTo 560

890 If I% = 0 Then GoSub 1710

900 GoSub 1200: C1 = 0

910 Return

1001 For J = 0 To 22: If B%(P9, J, 31) <> 0 Then B%(P9, J, 21) = 99
Next
GoTo 910

1060 S8%(P9, 0) = Int(((P%(P9, P1%(P9), 8) + P%(P9, P1%(P9), 7)) / P%(P9, P1%(P9), 4)) + .5)

1070 S8%(P9, 1) = 3
'S8%(P9,1) = 4

1080 If P6%(P9) > 1 Then S8%(P9, 1) = 0

1090 GoSub 1250
Return

1200 For I = 1 To 9

    1210 If B7%(P9, I) = 1 Then 1230

    1220 B%(P9, B3%(P9, I), 21) = 1

1230 M%(I) = 0: Next I

1240 M%(0) = 0: Return

1250 If I% <= 5 And S6%(P9, 0) - S6%(1 - P9, 0) > 0 Then P2%(P9) = P1%(P9)

1260 Return

1370 Locate 25, 1: For I1 = 0 To 9: If DH% = 0 And I1 = 0 Then 1410

    1380 If DH% = 1 And I1 = 1 Then 1410

    1390 If M%(I1) = 0 Then Print I1; " ";

    1400 If M%(I1) = 1 Then Print "  ";

1410 Next I1

1420 Return

1520 Cls: Print "#";: Color L%(P9, 11), L%(P9, 12): Print A$(P9);
Color 14, 0: Locate , 16: Print "T  IP   H  G  GS DR  BB  SO   W- L   ERA"

1530 For I = 0 To 21

    1531 If P$(P9, I) = "XXX" Or Left$(P$(P9, I), 1) = " " Or P$(P9, I) = "" Then P%(P9, I, 15) = 98

    1532 If P%(P9, I, 15) = 1 Or P$(P9, I) = "XXX" Then 1580
    Color 2, 0

    1540 If I <= 9 Then Print Using "#"; I;: Color 15, 0: GoSub 2000

    1550 If I > 9 Then Print X$(I - 10);: Color 15, 0: GoSub 2000

    1560 Locate , 3: Print P$(P9, I);
    Color 15, 0

    1561 Locate , 16: Print B1$(P%(P9, I, 0) + 2);
    Locate , 17: Print Using " ### ### ### ## ## ### ###"; P%(P9, I, 6); P%(P9, I, 7); P%(P9, I, 4); P%(P9, I, 5); P%(P9, I, 35); P%(P9, I, 8); P%(P9, I, 9);
    Locate , 43: Print Using "  ##"; P%(P9, I, 1);: Print "-";: Print Using "## ##.##"; P%(P9, I, 2); P%(P9, I, 10) / 100

1580 Next I
If I% > 0 Then 1681

1590 Locate 19, 58: Print "SELECT STARTER"

1595 If SP%(P9) <> -1 Then I1 = SP%(P9): GoTo 1640
1600 I$ = GetKeyPress$

1610 Call LETTER(I$)

1630 I1 = Val(I$): If I1 > 21 Or P%(P9, I1, 15) > 0 Then 1590

1640 Locate , 58: Print P$(P9, I1); " "; B1$(P%(P9, I1, 0) + 2)

1650 Print: Locate , 58: Print "ANY CHANGE (YN)"
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then 1680
1660 I$ = GetKeyPress$

1670 If UCase$(I$) = "Y" Then Locate 20, 58: SP%(P9) = -1: Print "                ": GoTo 1590

1680 P1%(P9) = I1: P6%(P9) = 1: P%(P9, I1, 15) = P6%(P9): GoSub 1060

1681 Return

1710 Locate 25, 1: Print "                                                                          ";

1711 Locate 25, 1: Print "SAVE THIS LINE UP (YN) ";
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "N": GoTo 1730
1720 I$ = GetKeyPress$

1730 If UCase$(I$) <> "Y" Then 1820

1790 Cls
If P%(1 - P9, P1%(1 - P9), 0) = -1 Then Q2% = 1
If P%(1 - P9, P1%(1 - P9), 0) = 0 Then Q2% = 0
For I = 0 To 9
    Print I; " ";
    If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 Then Print "AVAILABLE"
    If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then Print "LINEUP ALREADY SAVED "; MG%(P9, 100 + (I + Q2% * 10) * 21); " % USAGE"
Next
Input "LINEUP # TO SAVE "; LN%
Input "% OF TIME TO USE "; LU%
If MG%(P9, 120 + (LN% + Q2% * 10) * 21) <> 999 Then MG%(P9, 120 + (LN% + Q2% * 10) * 21) = 999
If Q2% = 1 Then LN% = LN% + 10
MG%(P9, 101 + LN% * 21) = DH%
MG%(P9, 100 + LN% * 21) = LU%
For I = 1 To 9
    MG%(P9, 110 + I + LN% * 21) = B3%(P9, I)
    MG%(P9, 101 + I + LN% * 21) = B7%(P9, I)
Next
GoSub 4522

1820 Return

2500 Cls
If P%(1 - P9, P1%(1 - P9), 0) = -1 Then Q2% = 1
If P%(1 - P9, P1%(1 - P9), 0) = 1 Then Q2% = 0
Print Tab(30); "LINEUPS FOUND VS. "; PT$(Q2%)
Print " #0    "; MG%(P9, 100 + (0 + Q2% * 10) * 21); "%";
Locate , 17: Print " #1    "; MG%(P9, 100 + (1 + Q2% * 10) * 21); "%";
Locate , 33: Print " #2    "; MG%(P9, 100 + (2 + Q2% * 10) * 21); "%";
Locate , 49: Print " #3    "; MG%(P9, 100 + (3 + Q2% * 10) * 21); "%";
Locate , 65: Print " #4    "; MG%(P9, 100 + (4 + Q2% * 10) * 21); "%"
For I = 0 To 4
    For I1 = 1 To 9
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then Locate 3, 16 * I + 1: Print "NONE FOUND": GoTo 2510
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then Locate 2 + I1, 16 * I + 1: Print "         ": GoTo 2510
        Locate 2 + I1, 16 * I + 1
        If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then Print "PITCHER";: GoTo 2512
        Print Left$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);

        2512 Locate 2 + I1, 16 * I + 12: Print C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))

    2510 Next
Next
Print
Print " #5    "; MG%(P9, 100 + (5 + Q2% * 10) * 21); "%";
Locate , 17: Print " #6    "; MG%(P9, 100 + (6 + Q2% * 10) * 21); "%";
Locate , 33: Print " #7    "; MG%(P9, 100 + (7 + Q2% * 10) * 21); "%";
Locate , 49: Print " #8    "; MG%(P9, 100 + (8 + Q2% * 10) * 21); "%";
Locate , 65: Print " #9    "; MG%(P9, 100 + (9 + Q2% * 10) * 21); "%"
For I = 5 To 9
    For I1 = 1 To 9
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then Locate 14, 16 * (I - 5) + 1: Print "NONE FOUND": GoTo 2505
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then Locate 13 + I1, 16 * (I - 5) + 1: Print "         ": GoTo 2505
        Locate 13 + I1, 16 * (I - 5) + 1
        If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then Print "PITCHER";: GoTo 2507
        Print Left$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);

        2507 Locate 13 + I1, 16 * (I - 5) + 12: Print C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))

    2505 Next
Next
Return

2530 RN = Int(Rnd(1) * PCT%) + 1
PCT% = 0
For I = 0 To 9
    If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 Then 2535
    PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
    If RN <= PCT% Then LN% = I: GoTo 2536

2535 Next

2536 Return

1830 GoSub 2500
LF% = 0: PCT% = 0
For I = 0 To 9
    If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then LF% = LF% + 1: PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
Next
If LF% = 0 Then 3110
If AP% = 1 And MP% = 0 And (U6 = 2 Or U6 = 1 And U9 = P9) Or I$ = "3" Then GoSub 2530: GoTo 2516

2515 Locate 23, 1: Input "ENTER LINEUP NUMBER (0-9) "; LN%

2516 If LN% < 0 Or LN% > 9 Then 2515
If MG%(P9, 120 + (LN% + Q2% * 10) * 21) <> 999 Then 480
If Q2% = 1 Then LN% = LN% + 10
I1 = MG%(P9, 101 + LN% * 21)
If DH% <> I1 Then 480
For I = 1 To 9
    B3%(P9, I) = MG%(P9, 110 + I + LN% * 21)
    B7%(P9, I) = MG%(P9, 101 + I + LN% * 21)
Next
LU% = 1

1910 For I = 1 To 9: B9%(P9, B7%(P9, I)) = B3%(P9, I)
    If B7%(P9, I) = 1 Then B3%(P9, I) = P1%(P9): B9%(P9, 1) = P1%(P9): GoTo 2525
    B%(P9, B3%(P9, I), 21) = 0

2525 Next I

1930 If LU% = 1 Then LU% = 0: GoSub 2160
GoSub 2310: Call NEWLINES(P9): GoSub 1940: GoTo 850

1940 For I2 = 0 To 22: Locate I2 + 2, 2: For I3 = 1 To 9

    1941 If B3%(P9, I3) = I2 And B7%(P9, I3) <> 1 Then Print "*";: Color L%(P9, 11), L%(P9, 12): Print B$(P9, I2): Color 15, 0

1950 Next I3: Next I2

1960 For I3 = 0 To 9: M%(I3) = 1: Next I3

1970 Return

2000 If P%(P9, I, 15) >= 98 Then Color 0, 7

2010 Return

2040 I% = 99: GoSub 1520
Locate 15, 58: Print "DISABLE PITCHER (0-L)"
Locate , 58: Print "Y-GRANT DAY OFF"
Locate , 58: Print "Z-CONTINUE"
Locate , 58: Print "X-ACTIVATE ALL PITCHERS"
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "Z": GoTo 2060
2050 I$ = GetKeyPress$
If I$ = Chr$(27) Then 170

2060 Call LETTER(I$)

2080 If UCase$(I$) = "Z" Then 2120
If UCase$(I$) = "Y" Then GoSub 2125: GoTo 2040

2090 If UCase$(I$) = "X" Then GoSub 2130: GoTo 2050

2100 I1 = Val(I$): If I1 > 21 Then 2050

2110 If P%(P9, I1, 15) = 0 Then P%(P9, I1, 15) = 99: Locate I1 + 2, 3: Color 0, 7: Print P$(P9, I1): Color 15, 0: GoTo 2112
If P%(P9, I1, 15) <> 0 Then P%(P9, I1, 15) = 0: Locate I1 + 2, 3: Color 15, 0: Print P$(P9, I1): Color 15, 0

2112 GoTo 2050

2120 I% = 0: Return

2125 'GRANT DAY OFF
If DR% = 1 Then 2126
For I1 = 0 To 21
    If P%(P9, I1, 35) > 0 Then P%(P9, I1, 35) = P%(P9, I1, 35) - 1
Next

2126 Return

2130 For I3 = 0 To 21: If P%(P9, I3, 15) = 99 And P%(P9, I3, 34) = 0 Then P%(P9, I3, 15) = 0: Locate I3 + 2, 3: Print P$(P9, I3)

2140 Next I3

2150 Return

2160 I% = 99: Call SHOWBATTERS(P9)
Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
Locate 6, 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

2161 Locate 10, 59: Print "DISABLE BATTER (0-M)": Locate , 59: Print "Z-CONTINUE"

2162 Locate , 59: Print "X-REACTIVATE ALL"

2163 Locate , 59: Print "<TAB>-BAT/FIELD/SPLIT"
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "Z": GoTo 2175
2170 I$ = GetKeyPress$
If I$ = Chr$(27) Then
    For J = 1 To 9
        M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1
    Next
    C1 = 0
    For J = 0 To 22
        B%(P9, J, 21) = 0
    Next
    RP = 1
    For J = 0 To 22
        If B%(P9, J, 31) <> 0 Then B%(P9, J, 21) = 99
    Next
    GoTo 480
End If

2175 If Asc(I$) = 9 Then TG = TG + 1
If TG > 2 Or TG = 2 And TS%(P9, 0) <> 999 Then TG = 0
If Asc(I$) = 9 And TG = 1 Then Call SHOWFIELD(P9): GoTo 2170

2176 If Asc(I$) = 9 And TG = 0 Then Call SHOWBATTERS(P9): GoTo 2161
If Asc(I$) = 9 And TG = 2 Then Call SHOWSPLIT(P9): GoTo 2170

2180 Call LETTER(I$)

2190 If UCase$(I$) = "Z" Then TG = 0: Call SHOWBATTERS(P9): GoTo 2230

2200 If UCase$(I$) = "X" Then GoSub 2240: GoTo 2170

2210 I1 = Val(I$): If I1 > 22 Then 2170

2220 B%(P9, I1, 21) = 99: Locate I1 + 2, 3: Color 0, 7: Print B$(P9, I1);: Color 15, 0: GoTo 2170

2230 Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
Locate 6, 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
I% = 0: Return

2240 For I3 = 0 To 22: If B%(P9, I3, 21) = 99 And B%(P9, I3, 31) = 0 Then B%(P9, I3, 21) = 0: Locate I3 + 2, 3: Print B$(P9, I3)

2250 Next I3: Return

2260 Locate 24, 1
I$ = GetKeyPress$

2310 For I = 10 To 22
    Locate I, 59: Print "                     ";
Next
Return

2320 Cls

2330 GoSub 2470

2340 If AP% = 1 Or DRIVE >= 67 Then 2345
    
Locate 6: Print "LOADING GAME..."

2345 I1 = Val(Left$(A$(0), 2)): I2 = Val(Left$(A$(1), 2))
If Val(Left$(A$(0), 3)) = 211 Then I1 = 201
If Val(Left$(A$(0), 3)) = 200 Then I1 = 200
If Val(Left$(A$(1), 3)) = 200 Then I2 = 200
If Val(Left$(A$(1), 3)) = 211 Then I2 = 201
If CGERA% > 2 Then S8%(1, 1) = 4: S8%(0, 1) = 4
If CGERA% > 3 Then S8%(1, 1) = 5: S8%(0, 1) = 5
If CGERA% = 7 Then S8%(1, 1) = 7: S8%(0, 1) = 7
If CGERA% >= 8 Then S8%(1, 1) = 99: S8%(0, 1) = 99
If (MON = 6 Or MON = 7) And VG% = 0 Then Chain "sourceg.exe"
Chain "SOURCE.EXE"

2390 For I = 0 To 1: For I1 = 1 To 9
    X0%(I, 0, I1) = B3%(I, I1)
    X0%(I, 1, I1) = B7%(I, I1)
    X0%(I, 2, I1) = I1 * 10
    Next I1
    D0%(I) = 9
Next I
Return

2430 Call INFO: Color 14: Print "WHICH TEAM DOES COMPUTER MANAGE ?": Print

2431 Color 15: Print "(0) ";: Color L%(0, 11), L%(0, 12): Print A$(0): Color 7, 0: Print
Color 15: Print "(1) ";: Color L%(1, 11), L%(1, 12): Print A$(1): Color 7, 0
2440 I$ = GetKeyPress$

2450 U9 = Val(I$): If U9 <> 1 Then U9 = 0

2460 Return

2470 RD = 0
For I = 0 To 1
    For I1 = 1 To 9
        If B7%(I, I1) < 2 Or B7%(I, I1) > 9 Then 2475
        If B7%(I, I1) = B%(I, B3%(I, I1), 22) Then B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 33): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 34): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 26): GoTo 2475
        If B7%(I, I1) = B%(I, B3%(I, I1), 23) Then B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 35): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 36): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 27): GoTo 2475
        If B7%(I, I1) = B%(I, B3%(I, I1), 24) Then B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 37): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 38): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 28): GoTo 2475
        If B7%(I, I1) = B%(I, B3%(I, I1), 25) Then B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 39): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 40): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 29): GoTo 2475
        If B7%(I, I1) = 2 Then B%(I, B3%(I, I1), 15) = 1: B%(I, B3%(I, I1), 17) = 0: B%(I, B3%(I, I1), 19) = 910 + Int(Rnd(1) * 10) + 1
        If B7%(I, I1) = 3 Then B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 960 + Int(Rnd(1) * 10) + 1
        If B7%(I, I1) = 4 Then B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 912 + Int(Rnd(1) * 10) + 1
        If B7%(I, I1) = 5 Or B7%(I, I1) = 6 Then B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 902 + Int(Rnd(1) * 10) + 1
        If B7%(I, I1) >= 7 Then B%(I, B3%(I, I1), 15) = 2: B%(I, B3%(I, I1), 17) = 2: B%(I, B3%(I, I1), 19) = 972 + Int(Rnd(1) * 10) + 1

2475 Next: Next

2476 GoSub 3500
If BO% = 1 Then BO% = 0: GoTo 2476
Call GETWEATHER(WD%, WS%, MO%, WF!, SC%, TP%, RD): Cls
Call DRAWBOX
Locate , 11: Print "CONDITIONS AT "; S$(1)
If L%(1, 9) = 1 Then Locate , 11: Print "TEMPERATURE: 70": Locate , 11: Print "WEATHER: DOME": Locate , 11: Print "WIND: NONE": GoTo 2479
Locate , 11: Print "TEMPERATURE: "; TP%
Locate , 11: Print "SKY CONDITIONS: "; SC$(SC%)
Locate , 11: Print "WIND: "; WD$(WD%);
Print Using " ## MPH"; WS%
If VG% = 1 Then
    WD$(2) = "L->R"
    WD$(3) = "R->L"
    WD$(4) = "<-LF"
    WD$(5) = "<-CF"
    WD$(6) = "<-RF"
    WD$(7) = "->LF"
    WD$(8) = "->CF"
    WD$(9) = "->RF"
End If
WI$ = WD$(WD%) + Str$(WS%) + " MPH"
Locate , 11: Print "PRECIPITATION: "; PC$(PC%)

2479 S$ = PARK$
Return

3110 Cls: Print "ERROR:: NO LINEUPS  FOUND..."
If AP% = 1 Then NLF% = 1: GoTo 480

3111 Call GETAKEY: GoTo 480

3500 'SELECT A DIFFERENT STADIUM
PARK$(1) = "ANAHEIM STADIUM             "
PARK$(2) = "THE ASTRODOME               "
PARK$(3) = "OAKLAND-ALAMEADA COLISEUM   "
PARK$(4) = "SKYDOME                     "
PARK$(5) = "FULTON COUNTY STADIUM       "
PARK$(6) = "MILWAUKEE COUNTY STADIUM    "
PARK$(7) = "BUSCH MEMORIAL STADIUM      "
PARK$(8) = "WRIGLEY FIELD               "
PARK$(9) = "DODGER STADIUM              "
PARK$(10) = "OLYMPIC STADIUM             "
PARK$(11) = "CANDLESTICK PARK            "
PARK$(13) = "SEATTLE KINGDOME            "
PARK$(12) = "CLEVELAND MUNICIPAL STADIUM "
PARK$(14) = "SHEA STADIUM                "
PARK$(15) = "BALTIMORE MEMORIAL STADIUM  "
PARK$(16) = "JACK MURPHY STADIUM         "
PARK$(17) = "VETERAN'S STADIUM           "
PARK$(18) = "THREE RIVERS STADIUM        "
PARK$(19) = "ARLINGTON STADIUM           "
PARK$(20) = "RIVERFRONT STADIUM          "
PARK$(21) = "FENWAY PARK                 "
PARK$(22) = "ROYALS/KAUFMANN STADIUM     "
PARK$(23) = "TIGER/BRIGGS/NAVIN STADIUM  "
PARK$(24) = "H.H.H. METRODOME            "
PARK$(25) = "OLD COMISKEY PARK           "
PARK$(26) = "OLD YANKEE STADIUM          "
PARK$(27) = "LOS ANGELES COLISEUM        "
PARK$(28) = "COLT STADIUM                "
PARK$(29) = "CONNIE MACK/SHIBE STADIUM   "
PARK$(30) = "EXHIBITION FIELD (TOR)      "
PARK$(31) = "PILOT FIELD                 "
PARK$(32) = "COUNTY STADIUM (MIL)        "
PARK$(33) = "SPORTSMAN'S PARK            "
PARK$(34) = "EBBETTS FIELD               "
PARK$(35) = "JARRY PARK                  "
PARK$(36) = "POLO GROUNDS                "
PARK$(37) = "CROSLEY FIELD               "
PARK$(38) = "RFK STADIUM                 "
PARK$(39) = "METROPOLITAN STADIUM        "
PARK$(40) = "FORBES FIELD                "
PARK$(41) = "GRIFFITH STADIUM            "
PARK$(42) = "SAN DIEGO STADIUM           "
PARK$(43) = "KAN CITY MUNICIPAL STADIUM  "
PARK$(44) = "SICKS STADIUM               "
PARK$(45) = "LOS ANGELES WRIGLEY FIELD   "
PARK$(46) = "SEALS STADIUM               "
PARK$(47) = "*AMERICAN LEAGUE            "
PARK$(48) = "*AMERICAN LEAGUE II         "
PARK$(49) = "HILLTOP PARK                "
PARK$(50) = "POLO GROUNDS (1913-1922)    "
PARK$(51) = "BRAVES FIELD (1915-1952)    "
PARK$(52) = "BRAVES FIELD (1915-1916)    "
PARK$(53) = "SOUTHSIDE PARK              "
PARK$(54) = "BAKER BOWL                  "
PARK$(55) = "CAMDEN YARDS                "
PARK$(56) = "COLUMBIA PARK               "
PARK$(57) = "EXPOSITION PARK             "
PARK$(58) = "LEAGUE PARK/PALACE OF FANS  "
PARK$(59) = "WEST SIDE PARK              "
PARK$(60) = "SOUTH END GROUNDS           "
PARK$(61) = "HUNTINGTON AVENUE GROUNDS   "
PARK$(62) = "LEAGUE PARK                 "
PARK$(63) = "BENNETT PARK                "
PARK$(64) = "WASHINGTON PARK             "
PARK$(65) = "MILE HIGH STADIUM           "
PARK$(66) = "JOE ROBBIE STADIUM          "
PARK$(67) = "JACOBS FIELD                "
PARK$(68) = "BALLPARK AT ARLINGTON       "
PARK$(69) = "COORS STADIUM               "
PARK$(70) = "NEW YANKEE STADIUM          "
PARK$(71) = "NEW COMISKEY PARK           "
PARK$(72) = "BUSCH STADIUM - GRASS       "
PARK$(73) = "TURNER FIELD                "
PARK$(74) = "BANK ONE BALLPARK           "
PARK$(75) = "TROPICANA FIELD             "
PARK$(76) = "SAFECO FIELD (SEA)          "
PARK$(77) = "ENRON PARK (HOU)            "
PARK$(78) = "PACIFIC BELL PARK (SF)      "
PARK$(79) = "COMERICA PARK (DET)         "
PARK$(80) = "                            "
PARK$(81) = "PNC PARK                    "
PARK$(82) = "MILLER PARK-MILWAUKEE       "
PARK$(83) = "GENERIC                     "
Call DRAWBOX
PARK$ = Right$(Str$(L%(1, 13)), Len(Str$(L%(1, 13))) - 1) + "."
If DN% = 0 Then PARK$ = PARK$ + "D" Else PARK$ = PARK$ + "N"
Locate , 11: Print "CURRENT PARK IS:"
Locate , 11: Print PARK$(L%(1, 13))
Locate , 11
If _FileExists(PARK$) Then Print "GRAPHIC EXISTS" Else Print "GRAPHIC NOT AVAILABLE": PARK$ = "99.N"
Locate , 11: Print "SELECT A DIFFERENT STADIUM (YN)"
Color 15, 0
If AP% = 1 Then Return
3505 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then Return
Cls
Locate 23, 10: Print "* - STADIUM GRAPHIC FILE NOT AVAILABLE"
Start% = 1

3510 SELECTION% = 1
Count% = 15
SCANCODE% = 83
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 10
Call box0(4, 9, 20, 38, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert PARK$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then BO% = 1: Return
PARK$ = Right$(Str$(SELECTION%), Len(Str$(SELECTION%)) - 1) + "."
If DN% = 0 Then PARK$ = PARK$ + "D" Else PARK$ = PARK$ + "N"
Color 15, 0
If _FileExists(PARK$) Then L%(1, 13) = SELECTION%: S$(1) = PARK$(SELECTION%): Return
Locate 10, 8
Print "STADIUM GRAPHIC NOT AVAILABLE"
Locate , 8: Print "USE SELECTED PARK RATINGS WITH"
Locate , 8: Print "GENERIC DISPLAY (YN)"
3515 I$ = GetKeyPress$
If I$ <> "Y" Then 3510
L%(1, 13) = SELECTION%: S$(1) = PARK$(SELECTION%)
If DN% = 0 Then PARK$ = "99.D" Else PARK$ = "99.N"
Return

4500 '*** PITCHING ROTATIONS ***
I% = 99: GoSub 1520
Locate 15, 57: Print "0-MANUALLY PICK STARTER"
Locate 16, 57: Print "1-USE PRE-SET ROTATION"
Locate 17, 57: Print "2-ENTER A NEW ROTATION"
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I = 1: GoTo 4508
4505 I$ = GetKeyPress$
If I$ = Chr$(27) Then 170
I = Val(I$): If I < 0 Or I > 2 Then 4505

4508 If I = 0 Then 4535

4510 If I = 1 Then 4512
If I = 2 Then Cls: GoTo 4604

4512 Cls

4515 If MF%(P9) = -1 Then 4600
If MG%(P9, 99) <> 999 Then 4600

4516 'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION
GoTo 4517
For I = 0 To MG%(P9, 1) - 1
    P%(P9, MG%(P9, 3 + I), 15) = 99
Next

4517 RN = Int(Rnd(1) * 100) + 1
P%(P9, MG%(P9, MG%(P9, 2) + 3), 15) = 0
If RN <= MG%(P9, MG%(P9, 2) + 10 + 3) Or MG%(P9, MG%(P9, 2) + 10 + 3) = 0 Then SP%(P9) = MG%(P9, MG%(P9, 2) + 3): GoTo 4520
RN = Int(Rnd(1) * 100) + 1
SP% = 0
For I = 21 To 36
    If MG%(P9, I) = -1 Or MG%(P9, I) < 0 Or MG%(P9, I) > 21 Then 4518
    SP% = SP% + MG%(P9, I + 20)
    If RN <= SP% And MG%(P9, I) <> -1 Then P%(P9, MG%(P9, I), 15) = 0: SP%(P9) = MG%(P9, I): GoTo 4520

4518 Next
GoTo 4517

4520 MG%(P9, 2) = MG%(P9, 2) + 1: If MG%(P9, 2) = MG%(P9, 1) Then MG%(P9, 2) = 0

4522 MG%(P9, 99) = 999
MG%(P9, 0) = 999
MF%(P9) = 0
Open D$(3) + "MGR." + YN$(P9) For Random As #2 Len = 2000
For I = 0 To 999
    Field #2, I * 2 As Q$, 2 As Q2$(I), 2000 - I * 2 - 2 As Q$
Next
For I = 0 To 999: LSet Q2$(I) = MKI$(MG%(P9, I)): Next
Put #2, T1(P9): Close 2

4535 Return

4600 If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then CR% = 1: GoTo 4610
Cls: Print "ERROR::: ROTATION NOT FOUND": Print
Print "ENTER A ROTATION (YN)"
4602 I$ = GetKeyPress$

4603 If UCase$(I$) <> "Y" Then 4535

4604 Print "COMPUTER SET ROTATION (YN) ";
4605 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then CR% = 1

4610 Cls: Call INFO
Locate 2, 1
For I = 1 To 5
    Color 14: Print "(";: Print Using "#"; I;: Print ") ";: Color 15: Print "MAN ROTATION"
Next
Print "YOU MAY CONSIDER THE FOLLOWING GUIDELINES:"
Print "MAX GAMES STARTED BY ANY PITCHER           ROTATION SIZE"
Print "          32                                    5"
Print "          41                                    4"
Print "          54                                    3"
Print "          75                                    2"
Print "         ALL                                    1"
Print
For I = 0 To 6
    Locate I + 15, 1
    If P$(P9, I) = "XXX" Then 4607
    Print P$(P9, I); Tab(18); P%(P9, I, 5);

    4607 Locate , 25
    If P$(P9, I + 7) = "XXX" Then 4608
    Print P$(P9, I + 7); Tab(43); P%(P9, I + 7, 5);

    4608 Locate , 50
    If P$(P9, I + 14) = "XXX" Then 4609
    Print P$(P9, I + 14); Tab(68); P%(P9, I + 14, 5)

4609 Next
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then GoSub 4685: GoTo 4613
4611 I$ = GetKeyPress$

4612 I = Val(I$)

4613 If I < 1 Or I > 5 Then 4611

4615 MG%(P9, 1) = I: MG%(P9, 2) = 0
For I = 3 To 36
    MG%(P9, I) = -1
Next
For I = 41 To 56
    MG%(P9, I) = 0
Next
MG%(P9, 0) = 999

4620 I% = 99: GoSub 1520
If CR% = 1 Then 4690

4625 For I = 0 To MG%(P9, 1) - 1

    4626 Locate 15, 59: Print "STARTER "; I + 1
    4628 I$ = GetKeyPress$

    4629 Call LETTER(I$)

    4632 I1 = Val(I$): If I1 > 21 Or MG%(P9, 3) = I1 Or MG%(P9, 4) = I1 Or MG%(P9, 5) = I1 Or MG%(P9, 6) = I1 Or MG%(P9, 7) = I1 Then 4628

    4635 Locate , 59: Print P$(P9, I1)

    4636 Locate , 59: Print "ANY CHANGE (YN)";
    4637 I$ = GetKeyPress$

    4638 If UCase$(I$) = "Y" Then For I2 = 15 To 17: Locate I2, 59: Print "                 ": Next: GoTo 4626

    4640 MG%(P9, I + 3) = I1: For I2 = 15 To 17: Locate I2, 59: Print "                  ": Next
Next
GoTo 4700

4680 Cls

4683 Return

4685 GS = 0
For I = 0 To 21
    If P%(P9, I, 5) > GS Then GS = P%(P9, I, 5)
Next
I = 1
If GS <= 75 Then I = 2
If GS <= 54 Then I = 3
If GS <= 41 Then I = 4
If GS <= 32 Then I = 5
Return

4690 'COMPUTER SET ROTATION
CR% = 0
For I = 0 To 21
    If P$(P9, I) = "XXX" Then GS%(I, 0) = 0: GS%(I, 1) = -1: GoTo 4691
    GS%(I, 0) = P%(P9, I, 5): GS%(I, 1) = I

4691 Next
For Z = 0 To 20: For Z1 = Z + 1 To 21: If GS%(Z, 0) >= GS%(Z1, 0) Then 4695
    Swap GS%(Z, 0), GS%(Z1, 0): Swap GS%(Z, 1), GS%(Z1, 1)

4695 Next Z1: Next Z
For I = 0 To MG%(P9, 1) - 1
    MG%(P9, I + 3) = GS%(I, 1)
Next
I1 = 0
For I = MG%(P9, 1) To 21
    I1 = I1 + 1
    If GS%(I, 0) = 0 Then 4700
    MG%(P9, I1 + 20) = GS%(I, 1)
Next

4700 I1 = I1 - 1: If MG%(P9, 1) = 1 Then MG%(P9, 13) = 100: GoTo 4516
If MG%(P9, 1) = 2 Then GS% = 75
If MG%(P9, 1) = 3 Then GS% = 54
If MG%(P9, 1) = 4 Then GS% = 41
If MG%(P9, 1) = 5 Then GS% = 32
For I = 1 To MG%(P9, 1)
    MG%(P9, I + 12) = Int(P%(P9, MG%(P9, I + 2), 5) * 100 / GS% + .5)
Next
SS% = 0
For I = 21 To 36
    If MG%(P9, I) < 0 Or MG%(P9, I) > 21 Then 4705
    SS% = SS% + P%(P9, MG%(P9, I), 5)

4705 Next
For I = 21 To 36
    If MG%(P9, I) < 0 Or MG%(P9, I) > 21 Then 4710
    MG%(P9, I + 20) = Int(P%(P9, MG%(P9, I), 5) * 100 / SS% + .5)

4710 Next
GoTo 4516

5000 '*** COMPUTER DETERMINE LINEUPS ***

5001 For I = 0 To 22: If B$(P9, I) = "XXX" Then B%(P9, I, 21) = 98

5002 Next

5003 GoSub 2160: GoSub 2310

5105 For I = 1 To 9: F(I) = 0: BP(I) = 0: BB%(I) = -1: Next

5115 M = 0: For I = 9 To 1 Step -1: J = I: For I2 = 0 To 22: PS%(I2, 0) = 0: PS%(I2, 1) = -1: Next: I2 = 0
    If I = 1 Then 5120
    For I1 = 0 To 22: If B%(P9, I1, 21) > 0 Or B$(P9, I1) = "XXX" Then 5117
        If B%(P9, I1, 22) = I And B%(P9, I1, 23) = -1 Then PS%(I2, 0) = B%(P9, I1, 4): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 22) = I And B%(P9, I1, 24) = -1 Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .9 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 22) = I And B%(P9, I1, 25) = -1 Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .8 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 22) = I Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .7 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 23) = I And B%(P9, I1, 24) = -1 Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 23) = I And B%(P9, I1, 25) = -1 Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 23) = I Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 24) = I And B%(P9, I1, 25) = -1 Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 24) = I Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117
        If B%(P9, I1, 25) = I Then PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GoTo 5117

    5117 Next I1

    5118 I2 = I2 - 1
    AB% = 0
    For I1 = 0 To I2
        AB% = AB% + PS%(I1, 0)
    Next
    RN = Int(Rnd(1) * AB%) + 1
    For I1 = 0 To I2
        If RN <= PS%(I1, 0) Then F(J) = PS%(I1, 1): B%(P9, PS%(I1, 1), 21) = 1: GoTo 5119
        PS%(I1 + 1, 0) = PS%(I1 + 1, 0) + PS%(I1, 0)
    Next I1
    Locate 25, 1: Print "CANNOT MAKE A LINEUP!!...";: LE = 1
    For J = 1 To 9: M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1: Next: C1 = 0
    For J = 0 To 22
        If B%(P9, J, 21) <> 99 Then B%(P9, J, 21) = 0
    Next
    If AP% = 1 Then 5163
    5162 I$ = GetKeyPress$

    5163 Return

5119 Next I: GoTo 5230

5120 If DH% = 0 Then BP(9) = 1: GoTo 5230
J = J - 1
I2 = 0: For I1 = 0 To 22: If B%(P9, I1, 21) > 0 Or B$(P9, I1) = "XXX" Then 5122
    If B%(P9, I1, 22) = 0 Then PS%(I2, 0) = B%(P9, I1, 4): PS%(I2, 1) = I1: GoTo 5121
    If B%(P9, I1, 23) = 0 Then PS%(I2, 0) = B%(P9, I1, 4) * .5: PS%(I2, 1) = I1: GoTo 5121
    If B%(P9, I1, 24) = 0 Then PS%(I2, 0) = B%(P9, I1, 4) * .3: PS%(I2, 1) = I1: GoTo 5121
    If B%(P9, I1, 25) = 0 Then PS%(I2, 0) = B%(P9, I1, 4) * .2: PS%(I2, 1) = I1: GoTo 5121
    PS%(I2, 0) = Int(B%(P9, I1, 4) * .05 + .5): PS%(I2, 1) = I1

    5121 I2 = I2 + 1

5122 Next I1
GoTo 5118

5230 ' SORT HITTERS

5240 C = 2
B = 1: GoSub 5300
B = 2: GoSub 5300

5235 C = 1
B = 3: GoSub 5300
B = 4: GoSub 5300
C = 1
B = 5: GoSub 5300
B = 6: GoSub 5300

5245 B = 7: GoSub 5300
B = 8: GoSub 5300
If DH% = 1 Then B = 9: GoSub 5300

5250 For I = 1 To 9
    B3%(P9, I) = BB%(I)
    B7%(P9, I) = BP(I)
Next
GoTo 1910

5300 E = -100: U = -1: T = -1
For L = 0 To 9

    5305 If DH% = 0 And L = 0 Then L = L + 2

    5310 If DH% = 1 And L = 1 Then L = L + 1

    5315 J = F(L): X = 1
    For Z = 1 To 9
        If BB%(Z) = J Then X = 0: Z = 9

    5320 Next Z
    If X = 0 Then 5350

    5325 If C = 1 Then 5345

    5330 JJ = J: GoSub 5355: T1% = 100 * B%(P9, J, 6) / B%(P9, J, 4) - 27
    If T1% < 0 Then T1% = 3 * T1%

    5335 JJ = 3 * R + T1%: If JJ > E Then E = JJ: T = J: U = L

    5340 GoTo 5350

    5345 SE = (B%(P9, J, 6) + B%(P9, J, 7) + B%(P9, J, 8) * 2 + B%(P9, J, 9) * 3) / B%(P9, J, 4)
    If SE >= E Then E = SE: T = J: U = L

5350 Next L
BB%(B) = T
BP(B) = U
Return

5355 'IF A PITCHER THEN 268

5360 Q1 = (B%(P9, JJ, 6) - B%(P9, JJ, 8) - B%(P9, JJ, 9) + B%(P9, JJ, 11)) * .7
If Q1 = 0 Then Q1 = 1

5365 Q1 = B%(P9, JJ, 13) / Q1: If Q1 < .02 Then 5375

5370 QQ = Q1 + .18: R = Int(QQ * (18 + 2 * Abs((QQ < .34)))): GoTo 5380

5375 R = 2 + Int(Q1 * 100): If B%(P9, JJ, 1) = 2 Then R = R - 1

5380 If (B%(P9, JJ, 1) = 6 Or B%(P9, JJ, 1) = 8) And R < 7 Then R = R + 1

5385 If R > 9 Then R = 9

5390 If R < 0 Then R = 1

5395 Return

8100 NT = 0
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q2$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Left$(Q2$(0), 1) = " " Or Asc(Mid$(Q2$(0), I2, 1)) < 32 Or Asc(Mid$(Q2$(0), I2, 1)) > 123 Or UCase$(Left$(Q2$(0), 3)) = "NOT" Or UCase$(Left$(Q2$(0), 3)) = "UN" Then 8105
    Next I2
    NT = NT + 1
    V$(NT) = Q2$(0): UV%(NT) = I1

8105 Next I1
Close 1

8102 SELECTION% = 1
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
MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
T1(I) = UV%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."

300 Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342

Field #1, 15 As Q2$(0), 2 As Q2$(1), 2 As Q2$(2), 2 As Q2$(3), 2 As Q2$(4), 2 As Q2$(5), 2 As Q2$(6), 2 As Q2$(7), 2 As Q2$(8), 2 As Q2$(9), 2 As Q2$(10), 2 As Q2$(11), 2 As Q2$(12), 2 As Q2$(13), 4 As Q2$(14), 4297 As X$

For A = 0 To 22

    FIELD #1, 45 + A * 99 AS X$, 15 AS Q2$(A * 43 + 15), 2 AS Q2$(A * 43 + 16), 2 AS Q2$(A * 43 + 17), 2 AS Q2$(A * 43 + 18), 2 AS Q2$(A * 43 + 19), 2 AS Q2$(A * 43 + 20), 2 AS Q2$(A * 43 + 21), 2 AS Q2$(A * 43 + 22), 2 AS Q2$(A * 43 + 23), 2 AS Q2$(A * _
    43 + 24), 2 AS Q2$(A * 43 + 25), 2 AS Q2$(A * 43 + 26), 4297 - 37 - A * 99 AS X$

    FIELD #1, 82 + A * 99 AS X$, 2 AS Q2$(A * 43 + 27), 2 AS Q2$(A * 43 + 28), 2 AS Q2$(A * 43 + 29), 2 AS Q2$(A * 43 + 30), 2 AS Q2$(A * 43 + 31), 2 AS Q2$(A * 43 + 32), 2 AS Q2$(A * 43 + 33), 2 AS Q2$(A * 43 + 34), 2 AS Q2$(A * 43 + 35), 2 AS Q2$(A *  _
    43 + 36), 2 AS Q2$(A * 43 + 37), 2 AS Q2$(A * 43 + 38), 4297 - 61 - A * 99 AS X$

    FIELD #1, 106 + A * 99 AS X$, 2 AS Q2$(A * 43 + 39), 2 AS Q2$(A * 43 + 40), 2 AS Q2$(A * 43 + 41), 2 AS Q2$(A * 43 + 42), 2 AS Q2$(A * 43 + 43), 2 AS Q2$(A * 43 + 44), 2 AS Q2$(A * 43 + 45), 2 AS Q2$(A * 43 + 46), 2 AS Q2$(A * 43 + 47), 2 AS Q2$(A * _
    43 + 48), 2 AS Q2$(A * 43 + 49), 2 AS Q2$(A * 43 + 50), 4297 - 85 - A * 99 AS X$

    Field #1, 130 + A * 99 As X$, 2 As Q2$(A * 43 + 51), 2 As Q2$(A * 43 + 52), 2 As Q2$(A * 43 + 53), 2 As Q2$(A * 43 + 54), 2 As Q2$(A * 43 + 55), 2 As Q2$(A * 43 + 56), 2 As Q2$(A * 43 + 57), 4297 - 99 - A * 99 As X$

Next

For A = 0 To 21
    FIELD #1, 2322 + A * 89 AS X$, 15 AS Q2$(A * 38 + 1004), 2 AS Q2$(A * 38 + 1005), 2 AS Q2$(A * 38 + 1006), 2 AS Q2$(A * 38 + 1007), 2 AS Q2$(A * 38 + 1008), 2 AS Q2$(A * 38 + 1009), 2 AS Q2$(A * 38 + 1010), 2 AS Q2$(A * 38 + 1011), 2 AS Q2$(A * 38 + _
    1012), 2 AS Q2$(A * 38 + 1013), 2 AS Q2$(A * 38 + 1014), 2 AS Q2$(A * 38 + 1015), 2020 - 37 - A * 89 AS X$

    FIELD #1, 2359 + A * 89 AS X$, 2 AS Q2$(A * 38 + 1016), 2 AS Q2$(A * 38 + 1017), 2 AS Q2$(A * 38 + 1018), 2 AS Q2$(A * 38 + 1019), 2 AS Q2$(A * 38 + 1020), 2 AS Q2$(A * 38 + 1021), 2 AS Q2$(A * 38 + 1022), 2 AS Q2$(A * 38 + 1023), 2 AS Q2$(A * 38 +  _
    1024), 2 AS Q2$(A * 38 + 1025), 2 AS Q2$(A * 38 + 1026), 2 AS Q2$(A * 38 + 1027), 2020 - 61 - A * 89 AS X$

    FIELD #1, 2383 + A * 89 AS X$, 2 AS Q2$(A * 38 + 1028), 2 AS Q2$(A * 38 + 1029), 2 AS Q2$(A * 38 + 1030), 2 AS Q2$(A * 38 + 1031), 2 AS Q2$(A * 38 + 1032), 2 AS Q2$(A * 38 + 1033), 2 AS Q2$(A * 38 + 1034), 2 AS Q2$(A * 38 + 1035), 2 AS Q2$(A * 38 +  _
    1036), 2 AS Q2$(A * 38 + 1037), 2 AS Q2$(A * 38 + 1038), 2 AS Q2$(A * 38 + 1039), 2020 - 85 - A * 89 AS X$

    Field #1, 2407 + A * 89 As X$, 2 As Q2$(A * 38 + 1040), 2 As Q2$(A * 38 + 1041), 2020 - 89 - A * 89 As X$
Next

Field #1, 4280 As X$, 40 As Q2$(1840), 15 As Q2$(1841), 5 As Q2$(1842), 2 As Q2$(1843)

Get #1, T1(I)
A$(I) = Q2$(0)
A$(I) = RTrim$(A$(I))

For I1 = 1 To 13
    L%(I, I1) = CVI(Q2$(I1))
Next

If (L%(I, 11) = 7 Or L%(I, 11) = 15) And L%(I, 12) = 0 Then L%(I, 11) = 0: L%(I, 12) = 7
K9(I) = CVS(Q2$(14))

For I1 = 0 To 22: B$(I, I1) = Q2$(I1 * 43 + 15)
    B$(I, I1) = RTrim$(B$(I, I1))
    For I2 = 0 To 41
        B%(I, I1, I2) = CVI(Q2$(I1 * 43 + I2 + 16))
    Next
    If B%(I, I1, 31) <> 0 Then B%(I, I1, 21) = 99
Next

For I1 = 0 To 21: P$(I, I1) = Q2$(I1 * 38 + 1004)
    P$(I, I1) = RTrim$(P$(I, I1))
    For I2 = 0 To 36: P%(I, I1, I2) = CVI(Q2$(I1 * 38 + I2 + 1005))
    Next
    If P%(I, I1, 34) <> 0 Then P%(I, I1, 15) = 99
    If P%(I, I1, 0) = 0 Then P%(I, I1, 0) = 1
Next

S$(I) = Q2$(1840): M$(I) = Q2$(1841): NN$(I) = Q2$(1842): CK = CVI(Q2$(1843))
Close 1

For I1 = 0 To 22
    'check for 1934 season
    '-- what does this do??? is it Babe Ruth related???
    If Val(Left$(A$(I), 2)) < 54 And Mid$(A$(I), 3, 1) = " " Then B%(I, I1, 30) = B%(I, I1, 30) * .75

    For I2 = 22 To 25
        If B%(I, I1, I2) <> -1 And B%(I, I1, I2) <> 0 And B%(I, I1, I2 + 4) = 0 Then B%(I, I1, I2 + 4) = 900
    Next
Next

M$(I) = RTrim$(M$(I))
S$(I) = RTrim$(S$(I))
NN$(I) = RTrim$(NN$(I))

For X = 0 To 22
    For X1 = 1 To 14
        B%(I, X, X1 + 41) = 0
    Next
Next

For X = 0 To 21
    For X1 = 1 To 28
        P%(I, X, X1 + 36) = 0
Next: Next
If LR% = 1 Then TS%(I, 0) = -1: GoTo 316
If Not _FileExists(D$(0) + "LR." + YN$) Then TS%(I, 0) = -1: GoTo 316
Open D$(0) + "LR." + YN$ For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q2$(X + 1), 4108 - 2 - 2 * X As X$
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q2$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q2$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q2$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q2$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q2$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q2$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
Get #1, T1(I)
For X = 0 To 11
    TS%(I, X) = CVI(Q2$(X + 1))
Next
For X = 0 To 22
    For X1 = 1 To 14
        B%(I, X, X1 + 41) = CVI(Q2$(12 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(I, X, X1 + 36) = CVI(Q2$(334 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(I, X, X1 + 50) = CVI(Q2$(642 + X1 + X * 14))
Next: Next
For X = 1 To 24: SA%(I, X) = CVI(Q2$(X + 950)): Next
For X = 0 To 22
    For X1 = 1 To 24
        B%(I, X, X1 + 55) = CVI(Q2$(975 + X1 + X * 24))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        P%(I, X, X1 + 64) = CVI(Q2$(1528 + X1 + X * 24))
Next: Next
Close 1
If TS%(I, 0) <> 999 Then TS%(I, 0) = -1

316 For X = 0 To 22
    For X1 = 0 To 14
        B%(I, X, 80) = B%(I, X, 80) + B%(I, X, X1 + 41)
Next: Next
For X = 0 To 21
    For X1 = 0 To 14
        P%(I, X, 89) = P%(I, X, 89) + P%(I, X, X1 + 50)
Next: Next
If Not _FileExists(D$(3) + "MGR." + YN$) Then MF%(I) = -1: GoTo 317
Open D$(3) + "MGR." + YN$ For Random As #2 Len = 2000
For X = 0 To 999
    Field #2, X * 2 As Q2$, 2 As Q2$(X), 2000 - X * 2 - 2 As Q2$
Next
Get #2, T1(I)
For X = 0 To 999: MG%(I, X) = CVI(Q2$(X)): Next
Close 2

317 If MG%(I, 0) <> 999 Or MF%(I) = -1 Then MF%(I) = -1: For X = 0 To 999: MG%(I, X) = -1: Next
N$(I) = A$(I)
For xx = 4 To 11
    If Mid$(N$(I), xx, 1) = " " Or Mid$(N$(I), xx, 1) = "." Then Mid$(N$(I), xx, 1) = "_"
Next xx
QQ = 8
For xx = 11 To 4 Step -1
    If Mid$(N$(I), xx, 1) = "_" Then QQ = xx - 4: GoTo 118
    GoTo 119

118 Next xx

119 '
F$ = Mid$(N$(I), 4, QQ)
F$ = F$ + "." + Left$(N$(I), 2)
If Not _FileExists(D$(1) + F$ + "D") Then 318
Open D$(1) + F$ + "D" For Input As #1
For I1 = 0 To 22
    Input #1, INJ%(I, I1)
Next
For I1 = 0 To 21
    Input #1, DYS%(I, I1)
Next
Close 1
If IJ% = 1 Then 318
For I1 = 0 To 22
    If INJ%(I, I1) > 0 Then B%(I, I1, 21) = 99
Next

318 If DR% = 1 Then 319
For I1 = 0 To 21
    If DYS%(I, I1) = 0 Or DYS%(I, I1) >= 90 Then 320
    P%(I, I1, 35) = DYS%(I, I1)

320 Next

319 Return



'------------------------------
'   COLCHK Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB COLCHK (P9, I)
    If B%(P9, I, 21) >= 98 Then Color 0, 7
End Sub



'------------------------------
'   DRAWBOX Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB DRAWBOX ()
    JJ = 5
    Locate 6, 8
    Color L%(1, 11), L%(1, 12)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
    Locate 7
End Sub



'------------------------------
'   GETAKEY Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB GETAKEY ()
    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    4001 I$ = GetKeyPress$
End Sub



'------------------------------
'   GETWEATHER Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)

    If L%(1, 13) > 82 Then
        ID = Int(Rnd(1) * 79) + 1
    Else
        ID = L%(1, 13)
    End If

    'LAYOUT OF THE PARKS FILE IS
    'NAME, CAPACITY, LF,LCF,CF,RCF,RF,GRASS,DOME,WINFAC
    '6 MONTHS OF DATA, TEMP, DELTA,RL,LR,INL,INC,INR,OUTL,OUTC,OUTR,WS09,WS1014,WS1519,WS20,PRECIP

    Open "PARKS.DBF" For Random As #1 Len = 240
    Field #1, 40 As Q3$(0), 4 As Q3$(1), 2 As Q3$(2), 2 As Q3$(3), 2 As Q3$(4), 2 As Q3$(5), 2 As Q3$(6), 2 As Q3$(7), 2 As Q3$(8), 2 As Q3$(9)
    For I = 0 To 5
        For I1 = 0 To 14
            Field #1, 60 + I * 30 + I1 * 2 As X$, 2 As Q3$(10 + I1 + I * 15)
    Next: Next

    Get #1, ID
    ATTEN = CVL(Q3$(1))
    LF = CVI(Q3$(2)): LCF = CVI(Q3$(3))
    CF = CVI(Q3$(4)): RCF = CVI(Q3$(5)): RF = CVI(Q3$(6))
    TG = CVI(Q3$(7)): DOME = CVI(Q3$(8)): WINFAC = CVI(Q3$(9))
    For I = 0 To 5: For I1 = 0 To 14
        MON(I, I1) = CVI(Q3$(10 + I1 + I * 15))
    Next: Next
    Close 1

    If MO% < 4 Then MO% = 4
    If MO% > 9 Then MO% = 9

    SM% = MO% - 4

    If Int(Rnd(1) * 100) + 1 <= 50 Then
        TP% = MON(SM%, 0) + Int(Rnd(1) * MON(SM%, 1) + 1)
    Else
        TP% = MON(SM%, 0) - Int(Rnd(1) * MON(SM%, 1) + 1)
    End If

    SUM = 0
    For I = 2 To 9
        SUM = SUM + MON(SM%, I)
    Next

    RN = Int(Rnd(1) * SUM) + 1

    SUM = 0
    For I = 2 To 9
        SUM = SUM + MON(SM%, I)
        If RN <= SUM Then 6105
    Next

    6105 WD% = I

    SUM = 0
    For I = 10 To 13
        SUM = SUM + MON(SM%, I)
    Next

    RN = Int(Rnd(1) * SUM) + 1

    SUM = 0
    For I = 10 To 13
        SUM = SUM + MON(SM%, I)
        If RN <= SUM Then 6106
    Next

    6106 If I = 10 Then WS% = Int(Rnd(1) * 10)
    If I = 11 Then WS% = Int(Rnd(1) * 5) + 10
    If I = 12 Then WS% = Int(Rnd(1) * 5) + 15
    If I = 13 Then WS% = Int(Rnd(1) * 10) + 20

    WF! = 1

    If WD% > 3 And WD% < 7 And WS% >= 15 Then WF! = 1 - WINFAC / 100
    If WD% > 6 And WD% < 10 And WS% >= 15 Then WF! = 1 + WINFAC / 100

    PC% = 4
    SC% = Int(Rnd(1) * 2) + 1

    If Int(Rnd(1) * 100) + 1 <= MON(SM%, 14) Then
        PC% = Int(Rnd(1) * 3) + 1
        SC% = 3
        RN = Int(Rnd(1) * 100) + 1
        If RN <= 5 Then RD = 1: RE = Int(Rnd(1) * 3) + 6
    End If

    If L%(1, 13) = 2 Or L%(1, 13) = 4 Or L%(1, 13) = 10 Or L%(1, 13) = 13 Or L%(1, 13) = 24 Then PC% = 4: SC% = 4

6200 End Sub



'------------------------------
'   INFO Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB INFO ()
    Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
    Print "GAME OPTIONS MENU"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER 1986-2000"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color , 0
End Sub



'------------------------------
'   INITVARI Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB INITVARI ()
    For P9 = 0 To 1: For I3 = 0 To 21: If P%(P9, I3, 15) <> 99 Then P%(P9, I3, 15) = 0
    Next: Next
    For I = 0 To 1: For I1 = 0 To 23: X0%(I, 0, I1) = -1: X0%(I, 2, I1) = 100: Next I1: Next I
    For I = 0 To 1: For I1 = 0 To 22
        If B%(I, I1, 21) <> 99 Then B%(I, I1, 21) = 0
    Next: Next
End Sub



'------------------------------
'   LETTER Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB LETTER (I$)

    1290 If UCase$(I$) = "A" Then I$ = "10"

    1300 If UCase$(I$) = "B" Then I$ = "11"

    1310 If UCase$(I$) = "C" Then I$ = "12"

    1320 If UCase$(I$) = "D" Then I$ = "13"

    1330 If UCase$(I$) = "E" Then I$ = "14"

    1340 If UCase$(I$) = "F" Then I$ = "15"

    1350 If UCase$(I$) = "G" Then I$ = "16"

    1351 If UCase$(I$) = "H" Then I$ = "17"

    1352 If UCase$(I$) = "I" Then I$ = "18"

    1353 If UCase$(I$) = "J" Then I$ = "19"

    1354 If UCase$(I$) = "K" Then I$ = "20"

    1355 If UCase$(I$) = "L" Then I$ = "21"

    1356 If UCase$(I$) = "M" Then I$ = "22"
End Sub



'------------------------------
'   LINSWAP Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB LINSWAP (P9)

    1440 Locate 18, 59: Print "B.O. PLYR 1 (1-9)";
    1441 I$ = GetKeyPress$
    K1 = Val(I$): If K1 < 1 Or K1 > 9 Then 1441
    If B7%(P9, K1) = 1 Then 1441
    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";
    1442 I$ = GetKeyPress$
    K2 = Val(I$): If K2 < 1 Or K2 > 9 Then 1442
    If B7%(P9, K2) = 1 Then 1442
    Swap B7%(P9, K2), B7%(P9, K1)
    Swap B3%(P9, K1), B3%(P9, K2)
    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)
    Call NEWLINES(P9)
    Locate 18, 59: Print "                 "
    Locate 19, 59: Print "                 "
End Sub



'------------------------------
'   NEWLINES Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB NEWLINES (P9)

    920 For I = 1 To 9
        Call POSITION(I)
        Print Using "# "; I;

        940 If B7%(P9, I) = 1 And I5 <> 66 Then Print P$(P9, B3%(P9, I));

        950 If B7%(P9, I) <> 1 Then Print B$(P9, B3%(P9, I));

        960 Print Tab(74);

        980 Print C$(B7%(P9, I));

    990 Next I

    1000 I5 = 0
End Sub



'------------------------------
'   POSITION Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB POSITION (I)
    Locate I + 7, 59
End Sub



'------------------------------
'   POSSWAP Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB POSSWAP (P9)
    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";
    1431 I$ = GetKeyPress$
    K1 = Val(I$): If K1 < 1 Or K1 > 9 Then 1431
    If B7%(P9, K1) = 1 Then 1431
    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";
    1432 I$ = GetKeyPress$
    K2 = Val(I$): If K2 < 1 Or K2 > 9 Then 1432
    If B7%(P9, K2) = 1 Then 1432
    Swap B7%(P9, K2), B7%(P9, K1)
    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)
    Call NEWLINES(P9)
    Locate 18, 59: Print "                 "
    Locate 19, 59: Print "                 "
End Sub



'------------------------------
'   SHOWBATTERS Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB SHOWBATTERS (P9)
    Cls: Color 14: Print "#";: Color L%(P9, 11), L%(P9, 12): Print A$(P9);: Color 14, 0

    1101 Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"

    1110 Locate 2
    For I = 0 To 22: Locate , 1

        1111 If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then B%(P9, I, 21) = 98: GoTo 1191
        Color 2, 0

        1120 If I < 10 Then Print Using "#"; I;

        1130 If I > 9 Then Print X$(I - 10);
        Color 15, 0
        Call COLCHK(P9, I)

        1140 Locate , 3: Print B$(P9, I);
        Color 15, 0
        Locate , 15: Print B1$(B%(P9, I, 0) + 2);

        1141 Locate , 18: If B%(P9, I, 22) <> -1 Then Print Using "#"; B%(P9, I, 22);

        1142 Locate , 19: If B%(P9, I, 23) <> -1 Then Print Using "#"; B%(P9, I, 23);

        1143 Locate , 20: If B%(P9, I, 24) <> -1 Then Print Using "#"; B%(P9, I, 24);

        1144 Locate , 21: If B%(P9, I, 25) <> -1 Then Print Using "#"; B%(P9, I, 25);

        1160 Locate , 23

        1161 Print Using "### ## ## ## ### ###"; B%(P9, I, 4); B%(P9, I, 7); B%(P9, I, 8); B%(P9, I, 9); B%(P9, I, 11); B%(P9, I, 12);

        1170 Locate , 44: Print Using "#"; B%(P9, I, 16);: Print Using "#"; B%(P9, I, 34);: Print Using "#"; B%(P9, I, 33);

        1180 Locate , 47

        1181 Print Using "### ##"; B%(P9, I, 13); B%(P9, I, 18);

        1182 If B%(P9, I, 6) = B%(P9, I, 4) And B%(P9, I, 6) <> 0 Then Print "1.000";: GoTo 1190

        1183 If B%(P9, I, 4) = 0 Then Print " .---";: GoTo 1190

        1184 Print Using " .###"; B%(P9, I, 6) / B%(P9, I, 4);

        1190 If I <> 22 Then Print

    1191 Next I
End Sub



'------------------------------
'   SHOWFIELD Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB SHOWFIELD (P9)
    'DISPLAY FIELDING AVERAGES
    Locate 1, 15
    Color 14, 0
    Print "P1 FAVG A R P2 FAVG A R P3 FAVG A R P4 FAVG A R                 "
    Color 15, 0
    Locate 2
    For I = 0 To 22
        Locate , 15: Print "                                                                ";
        If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then 2807
        Locate , 16: If B%(P9, I, 22) <> -1 Then Print Using "# #### # #"; B%(P9, I, 22); B%(P9, I, 26); B%(P9, I, 33); B%(P9, I, 34);
        Locate , 28: If B%(P9, I, 23) <> -1 Then Print Using "# #### # #"; B%(P9, I, 23); B%(P9, I, 27); B%(P9, I, 35); B%(P9, I, 36);
        Locate , 40: If B%(P9, I, 24) <> -1 Then Print Using "# #### # #"; B%(P9, I, 24); B%(P9, I, 28); B%(P9, I, 37); B%(P9, I, 38);
        Locate , 52: If B%(P9, I, 25) <> -1 Then Print Using "# #### # #"; B%(P9, I, 25); B%(P9, I, 29); B%(P9, I, 39); B%(P9, I, 40);
        If I <> 22 Then Print

    2807 Next I
End Sub



'------------------------------
'   SHOWSPLIT Subroutine
'------------------------------
' ...explanation...
' ...explanation...
SUB SHOWSPLIT (P9)
    'DISPLAY SPLIT STATS
    Locate 1, 15
    Color 14, 0
    Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
    Color 15, 0
    Locate 2
    For I = 0 To 22
        Locate , 15: Print "                                                                ";
        If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then 2806
        Locate , 20: Print Using "### ## ## ## ### ### "; B%(P9, I, 42); B%(P9, I, 44); B%(P9, I, 45); B%(P9, I, 46); B%(P9, I, 47); B%(P9, I, 48);
        If B%(P9, I, 42) = 0 Or B%(P9, I, 43) = 0 Then Print " .000";: GoTo 2802
        If B%(P9, I, 42) = B%(P9, I, 43) Then Print "1.000";: GoTo 2802
        Print Using " .###"; B%(P9, I, 43) / B%(P9, I, 42);

        2802 Locate , 53: Print Using "### ## ## ## ### ### "; B%(P9, I, 49); B%(P9, I, 51); B%(P9, I, 52); B%(P9, I, 53); B%(P9, I, 54); B%(P9, I, 55);
        If B%(P9, I, 49) = 0 Or B%(P9, I, 50) = 0 Then Print " .000";: GoTo 2805
        If B%(P9, I, 49) = B%(P9, I, 50) Then Print "1.000";: GoTo 2805
        Print Using " .###"; B%(P9, I, 50) / B%(P9, I, 49);

        2805 If I <> 22 Then Print

    2806 Next I
    
End Sub


'------------------------------
'   RAREPLAYS Subroutine
'------------------------------
' ...explanation...
SUB RAREPLAYS (H6%, H7%)
IF H7% = 20 THEN H7% = 0: GOTO 2008

2000 CALL NEWLINES: H6% = INT(RND(1) * 100) + 1
	IF H6% = 1 THEN PRINT b$(D, B9%(D, 2)); " is arguing ball and strike calls": CALL DELAY
	IF H6% = 1 THEN PRINT U$(1); " has heard enough": CALL DELAY
	IF H6% = 1 THEN PRINT b$(D, B9%(D, 2)); " has been ejected!!": X$ = b$(D, B9%(D, 2)): CALL EJECTINC (EJ%,X$): CALL DELAY: po% = 2: CALL EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6): GOTO 2850
	IF H6% <= 3 THEN PRINT p$(D, P1%(D)); " steps off the mound"
	IF H6% <= 3 THEN PRINT "after that last call": CALL DELAY: PRINT "He is shouting at "; U$(1): CALL DELAY
	IF H6% <= 3 THEN PRINT U$(1); " does not appreciate what he hears...": CALL DELAY: PRINT p$(D, P1%(D)); " has been ejected!!"
	IF H6% <= 3 THEN X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL INJPIT (P,D): GOTO 2850
	IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN PRINT Q$(0); " is unhappy with that last call": CALL DELAY: PRINT "He is venting his opinion to "; U$(1): CALL DELAY: PRINT "Thats it!! "; U$(1); " has ejected "; Q$(0); "!!"
	IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN X$ = Q$(0): CALL EJECTINC (EJ%,X$):CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO)
	IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN 2850
	IF H6% = 4 THEN PRINT Q$(0); " did not like that": PRINT "last call at all!!": CALL DELAY: GOTO 2850
	IF H6% > 5 THEN 2005
	PRINT Q$(0); " hits a slow roller to first"
	CALL DELAY: PRINT b$(D, B9%(D, 3)); " fields it cleanly"
	CALL DELAY: PRINT "He flips it to "; p$(D, P1%(D)); " covering just in time!"
	CALL DELAY: PRINT "Wait... "; U$(2); " rules safe!!": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 1: NEXT i: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD
	CALL NEWLINES: PRINT m$(D); " charges from the dugout": CALL DELAY
	PRINT m$(D); ", "; p$(D, P1%(D)); ","
	PRINT b$(D, B9%(D, 2)); ", and "; b$(D, B9%(D, 3))
	PRINT "are going at it with "; U$(2)
	CALL DELAY: CALL DELAY: CALL DELAY
	PRINT U$(3); " and "; U$(1); " come in to help out": CALL DELAY
	PRINT "This argument is out of control now!!": CALL DELAY
	PRINT "What is this!!": CALL DELAY
	PRINT "All of them have been ejected!!!"
	X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$)
	X$ = b$(D, B9%(D, 2)): CALL EJECTINC (EJ%,X$)
	X$ = b$(D, B9%(D, 3)): CALL EJECTINC (EJ%,X$)
	X$ = m$(D): CALL EJECTINC (EJ%,X$)
	CALL DELAY: po% = 2: CALL EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6): po% = 3: CALL EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6): CALL INJPIT (P,D): GOTO 2850

2005 IF H6% > 7 THEN 2007
	PRINT Q$(0); " sends a long one"
	PRINT "down the left field line": CALL DELAY
	PRINT "It may go...": CALL DELAY: PRINT "HOME RUN!!!": CALL DELAY
	PRINT "Wait!!! "; : CALL DELAY: PRINT U$(4); " is calling it foul!!": CALL DELAY: CALL NEWLINES
	PRINT Q$(0); " is arguing vehemently"
	PRINT "with "; U$(1); " and "; U$(4): CALL DELAY
	PRINT m$(p); " charges from the dugout": CALL DELAY
	PRINT "He and "; Q$(0); " are really going"
	PRINT "at it with the umpires!!": CALL DELAY: CALL DELAY
	PRINT Q$(0); " has just been tossed!!": X$ = Q$(0): CALL EJECTINC (EJ%,X$): CALL DELAY
	CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850

2007 IF H6% = 8 THEN PRINT U$(1); " has just ejected "; p$(D, P1%(D))
	IF H6% = 8 THEN PRINT "for wetting the ball!!": CALL EJECTINC (EJ%,X$): EJ$(EJ%) = p$(D, P1%(D)): CALL DELAY: CALL DELAY: CALL INJPIT (P,D): GOTO 2850
	IF H6% = 9 THEN PRINT "Here's the 3-2 pitch...": CALL DELAY: PRINT "Time called...": PRINT U$(1); " is calling it ball 4": CALL DELAY: PRINT "He says "; p$(D, P1%(D)); " went to his mouth": CALL DELAY: GOTO 2850
	IF H6% > 15 THEN 2006
	PRINT Q$(0); " hits the deck on a high"
	PRINT "and tight pitch from "; p$(D, P1%(D)): CALL DELAY: CALL DELAY
	PRINT Q$(0); " charges the mound!": CALL DELAY
	PRINT "Both dugouts have emptied...": CALL DELAY
	PRINT "It's a bench-clearing brawl!": CALL DELAY
	CALL NEWLINES: PRINT "It will take a few minutes to clean this up": CALL DELAY: i = INT(RND(1) * 3) + 1
	IF i = 1 THEN PRINT p$(D, P1%(D)); " has been ejected": X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL INJPIT (P,D): GOTO 2850
	IF i = 2 THEN PRINT Q$(0); " has been ejected": X$ = Q$(0): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850
	PRINT Q$(0); " and "; p$(D, P1%(D)); " have been ejected": X$ = Q$(0): CALL EJECTINC (EJ%,X$): X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL INJPIT (P,D): CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850

2006 IF H6% > 21 THEN 2010

2008 H6%=21:CALL NEWLINES: IF RD = 0 AND (PC% = 0 OR PC% = 4) THEN PRINT Q$(0); " fouls the pitch out of play": CALL DELAY: GOTO 2850
	tm = INT(RND(1) * 59) + 2: COLOR 0, 7: PRINT "The game has been halted by rain": FOR i = 1 TO 3: CALL DELAY: NEXT
	PRINT "Rain delay lasted"; tm; " minutes": CALL DELAY: CALL DELAY
	RD% = 1
	IF tm > 28 THEN PRINT "Pitchers have lost effectiveness during delay": CALL DELAY: PRINT "Their arms have stiffened up": CALL DELAY: CALL DELAY: FOR i = 0 TO 1: s8%(i, 0) = s8%(i, 0) - 2: NEXT
	GOTO 2850

2010 IF H6% > 24 THEN 2015
	IF s4% = 0 AND L%(1, 13) = 14 THEN PRINT Q$(0); " steps out of the box as a jet passes over": CALL DELAY: GOTO 2850
	IF s4% = 0 THEN PRINT U$(1); " calls time to get some new baseballs": CALL DELAY: GOTO 2850
	PRINT b$(D, B9%(D, 2)); " rifles the ball to ";
	IF s4% = 1 THEN PRINT "1st...": CALL DELAY: i = 2: GOTO 2012
	IF s4% = 2 OR s4% = 4 THEN PRINT "2nd base...": CALL DELAY: i = 3: GOTO 2012
	PRINT "3rd base...": CALL DELAY: i = 4

2012 PRINT Q$(i - 1); " is picked off base!"
	a5%(i - 1) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: W% = 2
	CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
	I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
	IF O%(0) >= 3 THEN B1!(p)=B1!(p)-1:H6%=5:GOTO 2850

2015 IF H6% > 29 THEN 2020
	PRINT Q$(0); " pops it up near the 3rd base stands": CALL DELAY
	PRINT "A fan interferes...": CALL DELAY
	PRINT b$(D, B9%(D, 6)); " makes a great catch!!": W% = 6: CALL CREDITREGPO (W%,D)
	O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6)
	I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6)
	CALL DELAY: GOTO 2850

2020 IF H6% <= 31 THEN PRINT Q$(0); " pops it up foul near the stands": CALL DELAY
	IF H6% <= 30 THEN PRINT b$(D, B9%(D, 2)); " runs into the stands!!": po% = 2: GOTO 2099
	IF H6% <= 31 THEN PRINT b$(D, B9%(D, 3)); " gives chase and runs into the stands!!": po% = 3: GOTO 2099
	IF H6% = 32 THEN PRINT Q$(0); " pops it up foul down the rf line": CALL DELAY: PRINT b$(D, B9%(D, 4)); " crashes into the wall going for it": po% = 4: GOTO 2099
	IF H6% = 33 THEN PRINT Q$(0); " pops it up foul by"
	IF H6% = 33 THEN PRINT "the 3rd base dugout": CALL DELAY
	IF H6% = 33 THEN PRINT b$(D, B9%(D, 5)); " falls in trying to make the catch!!": CALL DELAY: po% = 5: GOTO 2099
	IF H6% = 34 THEN PRINT Q$(0); " pops it up foul"
	IF H6% = 34 THEN PRINT "down the lf line": CALL DELAY
	IF H6% = 34 THEN PRINT b$(D, B9%(D, 6)); " tumbles over the railing"
	IF H6% = 34 THEN PRINT "trying to make the catch!": CALL DELAY: CALL DELAY: po% = 6: GOTO 2099
	IF H6% > 37 THEN 2023
	PRINT Q$(0); " lines one to deep left field": CALL DELAY: PRINT b$(D, B9%(D, 7)); " races back...": CALL DELAY: PRINT "He slams into the wall and is down!!": CALL DELAY
	op% = 7: PRINT b$(D, B9%(D, 8)); " comes over and gets the ball in": CALL DELAY

2021 I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 2: NEXT i: a5%(3) = 1: a5%(1) = 3: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES: po% = op%: GOTO 2098

2023 IF H6% > 40 THEN 2025
	PRINT Q$(0); " drives a sinking liner"
	PRINT "into deep center": CALL DELAY
	PRINT b$(D, B9%(D, 8)); " gets a good jump on it": CALL DELAY
	PRINT "He crashes into the wall and"
	PRINT "the ball bounces away!!": CALL DELAY
	PRINT "The runners are circling the bases": CALL DELAY
	PRINT b$(D, B9%(D, 9)); " retrieves the ball": CALL DELAY
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): I6 = 14: CALL ADDPIT (D,I5,I6): W% = 8
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 6: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 29: CALL INCPITOFF (P,I5,I6)
	a5%(0) = 4: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): PRINT "It's an inside the park HOME RUN!!!": CALL DELAY: CALL DELAY: CALL SCOREBOARD: CALL NEWLINES: po% = 8: GOTO 2098

2025 IF H6% > 43 THEN 2027
	PRINT Q$(0); " lifts a deep drive into right": CALL DELAY: PRINT b$(D, B9%(D, 9)); " races back...": CALL DELAY: PRINT "He slams into the wall and is down!!": CALL DELAY
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = 9: PRINT b$(D, B9%(D, 8)); " comes over to get the ball back in": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 5: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 28: CALL INCPITOFF (P,I5,I6)
	a5%(0) = 3: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): PRINT "Triple!!": CALL DELAY: CALL DELAY: CALL SCOREBOARD: CALL NEWLINES: po% = 9: GOTO 2098

2027 IF H6% > 44 THEN 2030
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = INT(RND(1) * 3) + 7: PRINT Q$(0); " hits a sinking liner into "; c$(W%): CALL DELAY
	PRINT b$(D, B9%(D, W%)); " dives for the catch...": CALL DELAY
	PRINT "the ball rolls free!": CALL DELAY
	PRINT b$(D, B9%(D, W% - 1)); " gets the loose ball in": CALL DELAY
	op% = W%: GOTO 2021

2030 IF H6% = 45 THEN PRINT Q$(0); " is hit by a pitch and injured": CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850
	IF H6% <= 49 THEN PRINT b$(D, B9%(D, 2)); " is injured by a foul tip!": CALL DELAY: po% = 2: GOTO 2099
	IF H6% > 58 THEN 2037
	i = INT(RND(1) * 3) + 7
	IF i = 9 THEN hi1 = 3: hi2 = 9: hi3 = 8
	IF i = 8 THEN hi1 = 4: hi2 = 8: hi3 = 7
	IF i = 7 THEN hi1 = 6: hi2 = 7: hi3 = 8
	PRINT Q$(0); " hits a weak fly into short "; c$(i): CALL DELAY
	PRINT b$(D, B9%(D, hi1)); " goes out": CALL DELAY
	PRINT b$(D, B9%(D, hi2)); " is charging in": CALL DELAY
	PRINT "They collide and the ball rolls free!!": CALL DELAY
	PRINT b$(D, B9%(D, hi3)); " gets it back in"
	CALL DELAY
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 2: NEXT i: a5%(3) = 1: a5%(1) = 3: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD
	CALL NEWLINES: po% = hi1: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): po% = hi2: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5: GOTO 2850

2037 IF H6% <= 61 THEN PRINT Q$(0); " fouls the pitch off his foot": CALL DELAY: PRINT "He is injured": CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): H6% = 1: GOTO 2850
	IF H6% > 64 THEN 2040
	PRINT Q$(0); " pops it up in foul along the 3b line": CALL DELAY: PRINT "It drifts into foul territory": CALL DELAY
	PRINT b$(D, B9%(D, 5)); " and "; p$(D, P1%(D)); " chase after it and collide!!": CALL DELAY
	PRINT b$(D, B9%(D, 5)); " is down and injured": CALL DELAY: po% = 5: GOTO 2099

2040 IF H6% > 70 THEN 2045
	PRINT Q$(0); : I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = 8: CALL LONGFLY (D,W%)
	PRINT b$(D, B9%(D, 7)); " collides with "; b$(D, B9%(D, 8)); "!!": CALL DELAY
	PRINT "both fielders are down... ": CALL DELAY
	PRINT b$(D, B9%(D, 6)); " gets the ball back in": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
	a5%(0) = 2: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1
	CALL ADVANCEMENT (P,D,S2%,F%,I3): PRINT "Double!!": CALL SCOREBOARD
	CALL NEWLINES: po% = 7: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6)
	po% = 8: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5
	GOTO 2850

2045 IF H6% > 85 THEN 2055
	PRINT Q$(0); " lines it off ";
	i = INT(RND(1) * 5) + 1: ON i GOTO 2046, 2047, 2047, 2047, 2047

2046 PRINT "the leg of "; p$(D, P1%(D)): CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES
	PRINT "He reaches on a single": CALL DELAY: PRINT p$(D, P1%(D)); " is down and in pain!": CALL DELAY
	CALL INJPIT (P,D): H6% = 5: GOTO 2850

2047 po% = i + 1: PRINT "of "; b$(D, B9%(D, po%)); " for a single": CALL DELAY: CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES
	PRINT b$(D, B9%(D, po%)); " is down and injured": CALL DELAY
	CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5: GOTO 2850

2055 IF H6% <= 89 THEN PRINT p$(D, P1%(D)); " is motioning for "; m$(D); " to come out": CALL DELAY: PRINT "It appears he has a blister": CALL DELAY: PRINT "He will have to leave the game": CALL DELAY: CALL INJPIT (P,D): H6% = 1: GOTO 2850
	IF H6% = 90 THEN PRINT Q$(0); " takes a mighty swing!": CALL DELAY: PRINT "It looks like he injured himself": CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): H6% = 1: GOTO 2850
	IF H6% = 91 THEN PRINT "Time is called": CALL DELAY: PRINT "Morganna strikes again!!": CALL DELAY: H6% = 1: GOTO 2850
	PRINT U$(1); " calls time to dust off the plate": CALL DELAY: H6% = 1: GOTO 2850

2098 CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5: GOTO 2850

2099 CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 1: GOTO 2850

2850 '
END SUB


'------------------------------
'      ADDPIT Subroutine
'------------------------------
' ...explanation...
SUB ADDPIT (D, I5, I6)
	P8%(D, I5, I6) = P8%(D, I5, I6) + 1
	P5%(D, I5, I6) = P5%(D, I5, I6) + 1
END SUB


'------------------------------
'      ADDSTTS Subroutine
'------------------------------
' ...explanation...
SUB ADDSTTS (p, D)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P, I5, I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P, I5, I6)
	I5 = P1%(D): I6 = 0: CALL ADDPIT(D, I5, I6)
END SUB


'------------------------------
'    ADJRUNNERS Subroutine
'------------------------------
' ...explanation...
SUB ADJRUNNERS (S2%, p)
	FOR i = 1 TO 3
	IF S2% = 13 AND A5%(i) = 11 AND B1%(i, 0) = 0 THEN A5%(0) = 11
	NEXT i
	IF B1%(3, 0) = 0 OR A5%(3) = 0 THEN 6805
	i = 3
	IF A5%(3) = 10 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(3) >= 1 AND A5%(3) <= 4 THEN CALL CHARGERUN(D,i)
	IF A5%(3) = 11 THEN I3 = 4: CALL CLEARBASE(i)

6805 IF B1%(2, 0) = 0 OR A5%(2) = 0 THEN 6812
	i = 2
	IF A5%(2) = 10 THEN I3 = 2: CALL CLEARBASE(i)
	IF A5%(2) = 11 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(2) = 12 AND B1%(3, 0) = 0 THEN I3 = 4: CALL CLEARBASE(i)
	IF A5%(2) = 1 AND B1%(3, 0) = 0 THEN CALL BATTER1(i)
	IF A5%(2) >= 2 AND A5%(2) <= 4 AND B1%(3, 0) = 0 THEN CALL CHARGERUN(D,i)

6812 IF B1%(1, 0) = 0 OR A5%(1) = 0 THEN 6821
	i = 1: IF A5%(1) = 8 AND B1%(2, 0) = 0 THEN CALL BATTER1(i)
	IF A5%(1) = 10 THEN I3 = 1: CALL CLEARBASE(i)
	IF A5%(1) = 11 THEN I3 = 2: CALL CLEARBASE(i)
	IF A5%(1) = 12 AND B1%(3, 0) = 0 AND B1%(2, 0) = 0 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(1) = 13 AND B1%(3, 0) = 0 AND B1%(2, 0) = 0 THEN I3 = 4: CALL CLEARBASE(i)
	IF A5%(1) = 1 AND B1%(2, 0) = 0 THEN CALL BATTER1(i)
	IF A5%(1) = 2 AND B1%(2, 0) = 0 AND B1%(3, 0) = 0 THEN CALL BATTER2(i)
	IF A5%(1) >= 3 AND A5%(1) <= 4 AND B1%(2, 0) = 0 AND B1%(3, 0) = 0 THEN CALL CHARGERUN(D,i)

6821 i = 0: B1%(0, 0) = B1!(p): B1%(0, 1) = P1%(D)
	IF A5%(0) = 0 THEN 6835
	IF A5%(0) > 0 AND A5%(0) < 4 THEN I3 = A5%(0): CALL BATTER3(I3, p, D)
	IF A5%(0) = 4 THEN I3 = A5%(0): CALL CHARGERUN(D,i)
	IF A5%(0) = 11 THEN I3 = 1: CALL CLEARBASE(i)
	IF A5%(0) = 12 THEN I3 = 2: CALL CLEARBASE(i)
	IF A5%(0) = 13 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(0) = 14 THEN I3 = 4: CALL CLEARBASE(i)

6835 '
END SUB


'------------------------------
'   ADV1BASE Subroutine
'------------------------------
' ...explanation...
SUB ADV1BASE (i)
	FOR I4 = 0 TO 2
	B0%(i + 1, I4) = B0%(i, I4): B0%(i + 1, 2) = 0: B0%(i, I4) = 0
	NEXT I4
	Q$(i + 1) = Q$(i): Q$(i) = ""
END SUB


'------------------------------
'   ADV2BASES Subroutine
'------------------------------
' ...explanation...
SUB ADV2BASES (i)
	FOR I4 = 0 TO 2
	B0%(i + 2, I4) = B0%(i, I4): B0%(i + 1, 2) = 0: B0%(i, I4) = 0
	NEXT I4
	Q$(i + 2) = Q$(i): Q$(i) = ""
END SUB


'------------------------------
'   ADV3BASES Subroutine
'------------------------------
SUB ADV3BASES (I3, p, D)
	B0%(I3, 0) = B1!(p): B0%(I3, 1) = P1%(D): Q$(I3) = Q$(0): B0%(I3, 2) = 0
	B0%(0, 0) = 0
END SUB


'------------------------------
'   ADVANCEMENT Subroutine
'------------------------------
' ...explanation...
SUB ADVANCEMENT (p, D, S2%, F%,I3)
	IF B0%(3, 0) = 0 OR A5%(3) = 0 THEN 3805
	i = 3
	IF A5%(3) = 10 THEN I3 = 3: CALL RecordOut(i, I3)
	IF A5%(3) >= 1 AND A5%(3) <= 4 THEN CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
	IF A5%(3) = 11 THEN I3 = 4: CALL RecordOut (i, I3)

3805 IF B0%(2, 0) = 0 OR A5%(2) = 0 THEN 3812
	i = 2
	IF A5%(2) = 10 THEN I3 = 2: CALL RecordOut (i, I3)
	IF A5%(2) = 11 THEN I3 = 3: CALL RecordOut (i, I3)
	IF A5%(2) = 12 AND B0%(3, 0) = 0 THEN I3 = 4: CALL RecordOut (i, I3)
	IF A5%(2) = 1 AND B0%(3, 0) = 0 THEN CALL ADV1BASE (i)
	IF A5%(2) >= 2 AND A5%(2) <= 4 AND B0%(3, 0) = 0 THEN CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

3812 IF B0%(1, 0) = 0 OR A5%(1) = 0 THEN 3821
	i = 1: IF A5%(1) = 8 AND B0%(2, 0) = 0 THEN CALL ADV1BASE (i)
	IF A5%(1) = 10 THEN I3 = 1: CALL RecordOut (i, I3)
	IF A5%(1) = 11 THEN I3 = 2: CALL RecordOut (i, I3)
	IF A5%(1) = 12 AND B0%(3, 0) = 0 AND B0%(2, 0) = 0 THEN I3 = 3: CALL RecordOut (i, I3)
	IF A5%(1) = 13 AND B0%(2, 0) = 0 AND B0%(3, 0) = 0 THEN I3 = 4: CALL RecordOut (i, I3)
	IF A5%(1) = 1 AND B0%(2, 0) = 0 THEN CALL ADV1BASE (i)
	IF A5%(1) = 2 AND B0%(2, 0) = 0 AND B0%(3, 0) = 0 THEN CALL ADV2BASES (i)
	IF A5%(1) >= 3 AND A5%(1) <= 4 AND B0%(2, 0) = 0 AND B0%(3, 0) = 0 THEN CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

3821 i = 0: B0%(0, 0) = B1!(p): B0%(0, 1) = P1%(D)
	IF A5%(0) = 0 THEN 3835
	IF A5%(0) > 0 AND A5%(0) < 4 THEN I3 = A5%(0): CALL ADV3BASES (I3, p, D)
	IF A5%(0) = 4 THEN I3 = A5%(0): CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
	IF A5%(0) = 11 THEN I3 = 1: CALL RecordOut (i, I3)
	IF A5%(0) = 12 THEN I3 = 2: CALL RecordOut (i, I3)
	IF A5%(0) = 13 THEN I3 = 3: CALL RecordOut (i, I3)
	IF A5%(0) = 14 THEN I3 = 4: CALL RecordOut (i, I3)

3835 IF F% = 0 AND O%(1) < 3 THEN CALL ADJRUNNERS (S2%, p)
	FOR i = 0 TO 3: A5%(i) = 0: NEXT i
END SUB


'------------------------------
'   BATDISP Subroutine
'------------------------------
' ...explanation...
SUB BATDISP (PG, P9)
	LOCATE 1, 1
	COLOR 14: PRINT "#"; : COLOR l%(P9, 11), l%(P9, 12): PRINT A$(P9); : COLOR 14, 0
	LOCATE , 16: PRINT "B   POS    G  AB 2B 3B HR RBI  BB  SO SRAB  SB CS G%  BA"
	FOR i = 2 TO 13: LOCATE i, 1: PRINT STRING$(78, 32): NEXT: LOCATE 2, 1
	IF PG = 0 THEN AA = 0: BB = 12
	IF PG = 1 THEN AA = 13: BB = 22
	FOR i = AA TO BB: LOCATE , 1
	IF b%(P9, i, 21) <> 0 THEN 150
	COLOR 2, 0
	IF i < 10 THEN PRINT USING "#"; i;
	IF i > 9 THEN PRINT X$(i - 10);
	COLOR 15, 0
	PRINT TAB(3); b$(P9, i); TAB(17); B1$(b%(P9, i, 0) + 2);
	FOR xx = 20 TO 23
	PRINT TAB(xx); : IF b%(P9, i, xx + 2) <> -1 THEN PRINT USING "#"; b%(P9, i, xx + 2);
	NEXT
	PRINT TAB(24);
	PRINT USING " ### ### ## ## ## ### ### ### #"; b%(P9, i, 3); b%(P9, i, 4); b%(P9, i, 7); b%(P9, i, 8); b%(P9, i, 9); b%(P9, i, 10); b%(P9, i, 11); b%(P9, i, 12); b%(P9, i, 16);
	PRINT USING "#"; b%(P9, i, 17); : PRINT USING "#"; b%(P9, i, 15); : PRINT USING "# "; b%(P9, i, 30);
	PRINT USING "### ## ## "; b%(P9, i, 13); b%(P9, i, 14); b%(P9, i, 18); : PRINT TAB(69); ".";
	IF b%(P9, i, 4) <> 0 THEN PRINT USING "###"; INT(((b%(P9, i, 6) / b%(P9, i, 4)) + .0005) * 1000): GOTO 150
	PRINT

150 NEXT i
END SUB


'------------------------------
'   BATOFF Subroutine
'------------------------------
' ...explanation...
SUB BATOFF (p, I5, I6)
	B8%(p, I5, I6) = B8%(p, I5, I6) - 1: B5%(p, I5, I6) = B5%(p, I5, I6) - 1
	IF I6 > 8 OR I6 = 2 OR I6 = 3 THEN 100
	IF P%(1 - P, P1%(1 - P), 0) = 1 THEN 100
	IF I6 > = 4 THEN CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) - 1:GOTO 100
	CSS%(P, I5, I6) = CSS%(P, I5, I6) - 1

100 END SUB


'------------------------------
'   BATTER1 Subroutine
'------------------------------
' ...explanation...
SUB BATTER1 (i)
	FOR I4 = 0 TO 1
	B1%(i + 1, I4) = B1%(i, I4): B1%(i, I4) = 0
	NEXT I4
END SUB


'------------------------------
'   BATTER2 Subroutine
'------------------------------
' ...explanation...
SUB BATTER2 (i)
	FOR I4 = 0 TO 1
	B1%(i + 2, I4) = B1%(i, I4): B1%(i, I4) = 0
	NEXT I4
END SUB


'------------------------------
'   BATTER3 Subroutine
'------------------------------
' ...explanation...
SUB BATTER3 (I3, p, D)
	B1%(I3, 0) = B1!(p): B1%(I3, 1) = P1%(D)
END SUB


'------------------------------
'   BLANKLINE Subroutine
'------------------------------
' ...explanation...
SUB BLANKLINE ()
	PRINT "                                            ";
END SUB


'------------------------------
'   CAUGHTFLY Subroutine
'------------------------------
' ...explanation...
SUB CAUGHTFLY (i, S2%, D)
	CALL RANDNUM(rn)
	IF rn <= 96 THEN i = 1
	IF rn = 97 THEN i = 2
	IF rn = 98 THEN i = 3
	IF rn = 99 THEN i = 4
	IF rn = 100 THEN i = 6
	ON i GOTO 4352, 4354, 4356, 4358, 4361, 4362

4352 IF S2% = 17 THEN 4353
	Y% = INT(RND(1) * 60) + 1230
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Q$(4); " "; Y$
	CALL DELAY

4353 IF S2% = 17 AND l%(1, 13) = 37 THEN PRINT "he climbs the terrace": CALL DELAY
	Y% = INT(RND(1) * 20) + 1290
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$
	GOTO 4359

4354 PRINT Q$(4); " makes a sliding catch": CALL DELAY: PRINT "What a play!!!": GOTO 4359

4356 PRINT Q$(4); " makes a leaping grab": CALL DELAY: PRINT "What a catch!!!": GOTO 4359

4362 PRINT Q$(4); " makes an incredible catch!!": GOTO 4359

4358 PRINT Q$(4); " makes a diving catch!!!"

4359 CALL DELAY

4361 '
END SUB


'------------------------------
'   CHANGELINEUP Subroutine
'------------------------------
' ...explanation...
SUB CHANGELINEUP (P9, p)
	LOCATE 14, 1: COLOR 2: PRINT " # LINEUP       PS  FA  OB"
	COLOR 15
	FOR i = 1 TO 9: LOCATE , 1
	IF B1!(P9) = i THEN PRINT "*";
	LOCATE , 2: PRINT USING "#"; i; : LOCATE , 4
	IF B7%(P9, i) = 1 THEN PRINT p$(P9, P1%(P9));
	IF B7%(P9, i) <> 1 THEN PRINT b$(P9, B3%(P9, i));
	IF B7%(P9, i) <> 0 AND B7%(P9, i) < 10 THEN LOCATE , 20: PRINT b%(P9, B3%(P9, i), 19);
	LOCATE , 17: PRINT C$(B7%(P9, i));
	LOCATE , 25
	FOR I1 = 1 TO 3
	IF B0%(I1, 0) = i AND P9 = p THEN COLOR 3: PRINT I1; : COLOR 15
	NEXT I1
	PRINT " "
	NEXT i
END SUB


'------------------------------
'   CHARGERUN Subroutine
'------------------------------
' ...explanation...
SUB CHARGERUN (D,i)
	IF O%(1) >= 3 THEN CALL CLEARBASE (i): GOTO 6841
	I5 = B1%(i, 1): I6 = 3: CALL ADDPIT (D, I5, I6): CALL CLEARBASE (i)

6841 '
END SUB


'------------------------------
'   CHECKSAVE Subroutine
'------------------------------
' ...explanation...
SUB CHECKSAVE (D, p, S4%)
	I3 = S6%(D, 0) - S6%(p, 0): S1%(D) = -1: IF I3 < 1 THEN 1807
	IF S4% = 0 AND I3 <= 2 THEN S1%(D) = P1%(D)
	IF S4% > 0 AND S4% < 4 AND I3 <= 3 THEN S1%(D) = P1%(D)
	IF S4% > 3 AND S4% < 7 AND I3 <= 4 THEN S1%(D) = P1%(D)
	IF S4% = 7 AND I3 <= 5 THEN S1%(D) = P1%(D)
	IF S4% = 0 AND O%(0) = 0 AND I3 <= 3 THEN S1%(D) = P1%(D)
	IF INNING% > 5 AND I3 > 0 AND INNING% < 8 THEN S1%(D) = P1%(D)

1807 RI = 0: IF S4% = 0 THEN 1820
	IF S4% = 1 OR S4% = 2 OR S4% = 3 THEN RI = 1
	IF S4% = 4 OR S4% = 5 OR S4% = 6 THEN RI = 2
	IF S4% = 7 THEN RI = 3
	P5%(D, P1%(D), 17) = P5%(D, P1%(D), 17) + RI: P8%(D, P1%(D), 17) = P8%(D, P1%(D), 17) + RI

1820 '
END SUB


'------------------------------
'   CLEARBASE Subroutine
'------------------------------
' ...explanation...
SUB CLEARBASE (i)
	B1%(i, 0) = 0: B1%(i, 1) = 0
END SUB


'------------------------------
'   CREDITASSIST Subroutine
'------------------------------
' ...explanation...
SUB CREDITASSIST (W%, D)
	'*** CREDIT ASSISTS ***
	IF W% = 1 THEN I5 = P1%(D): I6 = 39: CALL ADDPIT(D, I5, I6): GOTO 3408
	I5 = B9%(D, W%): I6 = 17: CALL INCBATDF(D, I5, I6)

3408 '
END SUB


'------------------------------
'   CREDITPUTOUT Subroutine
'------------------------------
' ...explanation...
SUB CREDITPUTOUT (I3, FB2P%, AST%, D)
	'*** CREDIT INFIELD PUTOUTS ***
	if i3>4 then i3=1
	po% = I3 + 2: ' 1=1st 2=2nd 3=3rd 4=home
	IF po% = 6 THEN po% = 2: GOTO 3417
	IF po% = 3 AND FB2P% = 1 THEN FB2P% = 0: po% = 1: GOTO 3415
	IF po% = 3 THEN 3417
	IF po% = 4 AND AST% = 4 THEN po% = 6: GOTO 3417
	IF po% = 4 AND AST% = 6 THEN po% = 4: GOTO 3417
	GOTO 3417

3415 IF po% = 1 THEN I5 = P1%(D): I6 = 38: CALL ADDPIT(D, I5, I6): GOTO 3418

3417 AST% = 0
	I5 = B9%(D, po%): I6 = 16: CALL INCBATDF(D, I5, I6)

3418 '
END SUB


'------------------------------
'   CREDITREGPO Subroutine
'------------------------------
' ...explanation...
SUB CREDITREGPO (W%, D)
	'*** CREDIT REGULAR PUTOUT  ***
	IF W% = 1 THEN
	  I5 = P1%(D): I6 = 38
	  CALL ADDPIT(D, I5, I6)
	ELSE
	  I5 = B9%(D, W%): I6 = 16
	  CALL INCBATDF(D, I5, I6)
	END IF
END SUB


'------------------------------
'   DELAY Subroutine
'------------------------------
' ...explanation...
SUB DELAY ()
	'Is the IF statement necessary?
	'IF displayPause <> THEN
		_Delay displayPause
	'END IF
END SUB


'------------------------------
'   DELIVERY Subroutine
'------------------------------
' ...explanation...
SUB DELIVERY (D)
	'*** setup the delivery ***
	CALL RANDNUM(RN)
	IF RN<=20 THEN
		Y% = INT(RND(1) * 20) + 50
		CALL GETRPBP(Y%, Y$, D)
		CALL STRIPRPBP(Y$, Y%, D)
		PRINT Y$
		CALL DELAY
	END IF
	  Y% = INT(RND(1) * 46) + 1
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$
	CALL DELAY
END SUB


'------------------------------
'   DISPFLDG Subroutine
'------------------------------
' ...explanation...
SUB DISPFLDG (PG, P9, TG)
	'DIS FLDG
	LOCATE 1, 16: PRINT "P1 FAVG A R   P2 FAVG A R   P3 FAVG A R   P4 FAVG A R      "
	FOR i = 2 TO 13: LOCATE i, 1: PRINT STRING$(78, 32): NEXT
	LOCATE 2
	IF PG = 0 THEN AA = 0: BB = 12
	IF PG = 1 THEN AA = 13: BB = 22
	FOR i = AA TO BB: IF b%(P9, i, 21) <> 0 THEN 5802
	IF i < 10 THEN PRINT USING "#"; i;
	IF i > 9 THEN PRINT X$(i - 10);
	COLOR 15, 0
	PRINT TAB(3); b$(P9, i);
	LOCATE , 17: IF b%(P9, i, 22) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 22); b%(P9, i, 26); b%(P9, i, 33); b%(P9, i, 34);
	LOCATE , 31: IF b%(P9, i, 23) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 23); b%(P9, i, 27); b%(P9, i, 35); b%(P9, i, 36);
	LOCATE , 45: IF b%(P9, i, 24) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 24); b%(P9, i, 28); b%(P9, i, 37); b%(P9, i, 38);
	LOCATE , 59: IF b%(P9, i, 25) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 25); b%(P9, i, 29); b%(P9, i, 39); b%(P9, i, 40);
	PRINT

5802 NEXT i: TG = TG + 1
END SUB


'------------------------------
'   DISPRUN Subroutine
'------------------------------
' ...explanation...
SUB DISPRUN (I2, p, X)
	J = B0%(I2, 0): IF J = 0 THEN 363
	COLOR l%(p, 11), l%(p, 12)
	IF B7%(p, J) = 1 AND p%(p, P1%(p), 11) <> 999 THEN PRINT LEFT$(p$(p, P1%(p)), 10): COLOR 15, 2: LOCATE , X: PRINT "1/  0": GOTO 363
	IF B7%(p, J) = 1 THEN PRINT LEFT$(p$(p, P1%(p)), 10): LOCATE , X: COLOR 15, 2: PRINT USING "#"; p%(p, P1%(p), 31); : PRINT "/"; : PRINT USING "###"; p%(p, P1%(p), 29): GOTO 363
	PRINT LEFT$(b$(p, B3%(p, J)), 10): LOCATE , X: COLOR 15, 2: PRINT USING "#"; b%(p, B3%(p, J), 16); : PRINT "/"; : PRINT USING "###"; b%(p, B3%(p, J), 13)

363 COLOR 15, 0
END SUB


'------------------------------
'   DPPBP Subroutine
'------------------------------
' ...explanation...
SUB DPPBP ()
	CALL RANDNUM(rn)
	IF rn <= 20 THEN PRINT "may be a double play ball"
	IF rn > 20 AND rn <= 40 THEN PRINT "they might turn two"
	IF rn > 40 AND rn <= 60 THEN PRINT "they could get two"
	IF rn > 60 AND rn <= 80 THEN PRINT "could be two"
	IF rn > 80 THEN PRINT "could be a double play"
	CALL DELAY
END SUB


'------------------------------
'   DPPIVOT Subroutine
'------------------------------
' ...explanation...
SUB DPPIVOT (W%, D)
	'*** double play pivot ***
	IF W% = 4 THEN ww% = 6
	IF W% = 6 THEN ww% = 4
	IF W% = 3 THEN ww% = 1
	Y% = INT(RND(1) * 20) + 1910
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$; " "; b$(D, B9%(D, ww%))
	CALL DELAY
END SUB


'------------------------------
'   DPRELAY Subroutine
'------------------------------
' ...explanation...
SUB DPRELAY ()
	CALL RANDNUM(rn)
	IF rn <= 10 AND A5%(0)<>11 THEN PRINT "Give me the Hoover!": GOTO 3716
	IF A5%(0) <> 11 THEN 3716
	CALL RANDNUM(rn)
	IF rn <= 8 THEN PRINT "over to 1st..."
	IF rn > 8 AND rn <= 16 THEN PRINT "throw to 1st..."
	IF rn > 16 AND rn <= 24 THEN PRINT "on to first..."
	IF rn > 24 AND rn <= 30 THEN PRINT Q$(4); " pivots and throws"
	IF rn > 30 AND rn <= 36 THEN PRINT Q$(4); " gets the throw off"
	IF rn > 36 AND rn <= 44 THEN PRINT Q$(4); " is hit as he throws"
	IF rn > 44 AND rn <= 50 THEN PRINT Q$(4); " leaps and throws"
	IF rn > 50 AND rn <= 56 THEN PRINT Q$(4); " jumps over the sliding "; Q$(1)
	IF rn > 56 AND rn <= 64 THEN PRINT "the relay to first..."
	IF rn > 64 AND rn <= 72 THEN PRINT "quick turn by "; Q$(4)
	IF rn > 72 AND rn <= 80 THEN PRINT "relay to 1st..."
	IF rn > 80 AND rn <= 88 THEN PRINT "to 1st..."
	IF rn > 88 AND A5%(0) = 11 THEN PRINT "over to 1st..."

3716 CALL DELAY
END SUB


'------------------------------
'   DPSAFE Subroutine
'------------------------------
' ...explanation...
SUB DPSAFE ()
	CALL RANDNUM(rn)
	IF rn <= 8 THEN PRINT "over to 1st..."
	IF rn > 8 AND rn <= 16 THEN PRINT "throw to 1st..."
	IF rn > 16 AND rn <= 24 THEN PRINT "on to first..."
	IF rn > 24 AND rn <= 30 THEN PRINT "pivot to first"
	IF rn > 30 AND rn <= 36 THEN PRINT "the throw to 1st"
	IF rn > 36 AND rn <= 44 THEN PRINT "relay to first"
	IF rn > 44 AND rn <= 50 THEN PRINT "on to first..."
	IF rn > 50 AND rn <= 56 THEN PRINT "on to first"
	IF rn > 56 AND rn <= 64 THEN PRINT "the relay to first..."
	IF rn > 64 AND rn <= 72 THEN PRINT "quick turn to first"
	IF rn > 72 AND rn <= 80 THEN PRINT "relay to 1st..."
	IF rn > 80 AND rn <= 88 THEN PRINT "to 1st..."
	IF rn > 88 AND A5%(0) = 11 THEN PRINT "over to 1st..."
	CALL DELAY
	Y% = INT(RND(1) * 20) + 1890
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$
	CALL DELAY
END SUB


'------------------------------
'   DRAWBOX Subroutine
'------------------------------
' ...explanation...
SUB DRAWBOX (J)
	LOCATE 8, 8: COLOR l%(1, 11), l%(1, 12)
	PRINT " Ö"; STRING$(61, 196); "· ";
	FOR II = 1 TO J
	LOCATE 8 + II, 8
	PRINT " º"; STRING$(61, 32); "º ";
	NEXT
	LOCATE 9 + J, 8: PRINT " Ó"; STRING$(61, 196); "½ ";
END SUB


'------------------------------
'   EJECTINC Subroutine
'------------------------------
' ...explanation...
SUB EJECTINC (EJ%, X$)
	EJ% = EJ% + 1: EJ$(EJ%) = X$
END SUB


'------------------------------
'   EJECTIONINJURY Subroutine
'------------------------------
' ...explanation...
SUB EJECTIONINJURY (u6, p, u9, D, po%, P9, W5, W6)
	'*** Defensive Player Ejected/Injured ***
	IF u6 = 2 OR u6 = 1 AND p <> u9 THEN 2704
	PRINT "You must replace "; b$(D, B9%(D, po%)): CALL DELAY: GOTO 2749

2704 FOR I2 = 1 TO 9: IF B7%(D, I2) = po% THEN 2705
	NEXT

2705 FOR i = 0 TO 22: IF b%(D, i, 21) > 0 OR b$(D, i) = "XXX" THEN 2708
	IF B7%(D, I2) <> b%(D, i, 22) AND B7%(D, I2) <> b%(D, i, 23) AND B7%(D, I2) <> b%(D, i, 24) AND B7%(D, I2) <> b%(D, i, 25) THEN 2708

2707 PRINT b$(D, B9%(D, po%)); " will be replaced by "; b$(D, i): CALL DELAY
	D0%(D) = D0%(D) + 1
	X0%(D, 0, D0%(D)) = i
	X0%(D, 1, D0%(D)) = B7%(D, I2)
	X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)
	b%(D, i, 21) = 1
	B3%(D, I2) = i
	I7% = D: I8% = I2: i9% = i
	CALL REPLACEMENTS(I7%, I8%, i9%)
	B9%(D, B7%(D, I2)) = i
	P9 = D: W5 = P9: W6 = P9: VV(P9) = po%: VV1(P9) = po%
	CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	 CALL FLDDISP(BC%, p, U%, D): CALL MAINSCREEN(D, p, BC%, U%, MO): LOCATE 15, 1
	 VV(P9)=1:VV1(P9)=9
	GOTO 2749

2708 NEXT i
	FOR i = 0 TO 22: IF b%(D, i, 21) > 0 OR b$(D, i) = "XXX" THEN 2710
	GOTO 2707

2710 NEXT i

2749 '
END SUB


'------------------------------
'   FLDAVG Subroutine
'------------------------------
' ...explanation...
SUB FLDAVG (D, W%, I3)
	IF b%(D, B9%(D, W%), 19) >= 999 THEN I3 = 999: GOTO 4454
	IF W% >= 7 THEN I3 = b%(D, B9%(D, W%), 19): GOTO 4454
	IF W% = 3 OR W% = 2 THEN I3 = (b%(D, B9%(D, W%), 19)) * .96: GOTO 4454
	'IF W%<>1 AND W%<>4 AND W%<>6 THEN I3=(B%(D,B9%(D,W%),19))*.985

4454 I4 = b%(D, B9%(D, W%), 22)
	I5 = b%(D, B9%(D, W%), 23)
	I6 = b%(D, B9%(D, W%), 24)
	I7 = b%(D, B9%(D, W%), 25)
	IF W% = I4 OR W% = I5 OR W% = I6 OR W% = I7 THEN 4462
	IF W% = 4 OR W% > 6 THEN I3 = 982
	IF W% = 2 THEN I3 = 987
	IF W% = 3 THEN I3 = 980
	IF W% = 6 OR W% = 5 THEN I3 = 964
	IF W% >= 7 THEN I3 = I3 - 10: GOTO 4462
	I3 = I3 - 20
	IF W% = 2 OR W% >= 4 AND W% <= 6 THEN I3 = I3 - 80: GOTO 4464

4462 IF W% = 4 OR W% = 6 THEN I3 = (b%(D, B9%(D, W%), 19)) * .995
	IF W% = 5 THEN I3 = (b%(D, B9%(D, W%), 19)) * .985

4464 REM
END SUB


'------------------------------
'   FLDDISP Subroutine
'------------------------------
' ...explanation...
SUB FLDDISP (BC%, p, U%, D)
	COLOR , 0: CLS
	FOR X = 5 TO 25
	LOCATE X, 46
	COLOR , 2
	PRINT STRING$(34, 32);
	NEXT
	COLOR , 0
	LOCATE 1, 1
	X% = 1: Y% = 9
	IF INNING% > 9 THEN X% = 10: Y% = 18
	IF INNING% > 18 THEN X% = 19: Y% = 27
	PRINT TAB(10);
	FOR X1% = 0 TO 2
	PRINT USING "## ## ##  "; X% + 3 * X1%; X% + 3 * X1% + 1; X% + 3 * X1% + 2;
	NEXT
	PRINT " R  H  E "; : COLOR 1
	PRINT CHR$(179); : COLOR 15: PRINT " BALL:  "; : COLOR 14: PRINT BC%;
	COLOR 1: PRINT CHR$(179); : COLOR 15
	PRINT "WIND: "; : COLOR 14: PRINT WI$
	FOR i = 0 TO 1: LOCATE i + 2: PRINT LEFT$(A$(i), 3); NN$(i);
	FOR I2 = X% TO Y%
	IF p = 0 AND i = 0 AND I2 > INNING% - 1 AND s%(i, INNING%) = 0 OR p = 0 AND i = 0 AND I2 > INNING% AND s%(i, INNING%) > 0 OR p = 1 AND I2 > INNING% AND i = 0 THEN 310
	IF p = 0 AND i = 1 AND I2 > INNING% - 1 OR p = 1 AND i = 1 AND s%(i, INNING%) = 0 AND I2 > INNING% - 1 OR p = 1 AND i = 1 AND s%(i, INNING%) > 0 AND I2 > INNING% THEN 310
	CALL INNDISP(I2, i)
	NEXT I2

310 LOCATE , 40: PRINT USING "## ## ## "; S6%(i, 0); S6%(i, 1); S6%(i, 2);
	COLOR 1: PRINT CHR$(179); : COLOR 15
	IF i = 0 THEN PRINT " STRIKE:"; : COLOR 14: PRINT U%; : COLOR 1: PRINT CHR$(179); : COLOR 15: PRINT "SKY:  "; : COLOR 14: PRINT SC$(SC%)
	IF i = 1 THEN PRINT " OUT:   "; : COLOR 14: PRINT O%(0); : COLOR 1: PRINT CHR$(179); : COLOR 15: PRINT "TEMP:"; : COLOR 14: PRINT TP%
	NEXT i
	COLOR 1: PRINT STRING$(20, 196); CHR$(194); STRING$(23, 196); CHR$(194); STRING$(3, 196); CHR$(193); STRING$(11, 196); CHR$(193); STRING$(19, 196)
	FOR X = 5 TO 12: LOCATE X, 21: PRINT CHR$(179); : NEXT
	FOR X = 5 TO 25: LOCATE X, 45: PRINT CHR$(179); : NEXT
	COLOR 15, 2
	LOCATE 5, 58: COLOR l%(D, 11), l%(D, 12): PRINT LEFT$(b$(D, B9%(D, 8)), 10): COLOR 15, 2
	LOCATE 6, 59: PRINT USING "A:# R:#"; b%(D, B9%(D, 8), 15); b%(D, B9%(D, 8), 17)
	LOCATE 7, 59: PRINT USING "FA:####"; b%(D, B9%(D, 8), 19)
	LOCATE 6, 46: COLOR l%(D, 11), l%(D, 12): PRINT LEFT$(b$(D, B9%(D, 7)), 10);
	LOCATE , 70: PRINT LEFT$(b$(D, B9%(D, 9)), 10): COLOR 15, 2
	LOCATE 7, 47: PRINT USING "A:# R:#"; b%(D, B9%(D, 7), 15); b%(D, B9%(D, 7), 17);
	LOCATE , 71: PRINT USING "A:# R:#"; b%(D, B9%(D, 9), 15); b%(D, B9%(D, 9), 17)
	LOCATE 8, 47: PRINT USING "FA:####"; b%(D, B9%(D, 7), 19);
	LOCATE , 71: PRINT USING "FA:####"; b%(D, B9%(D, 9), 19)
	LOCATE 10, 49: COLOR l%(D, 11), l%(D, 12): PRINT LEFT$(b$(D, B9%(D, 6)), 10);
	LOCATE , 68: PRINT LEFT$(b$(D, B9%(D, 4)), 10): COLOR 15, 2
	LOCATE 11, 50: PRINT USING "R:#"; b%(D, B9%(D, 6), 17);
	LOCATE , 69: PRINT USING "R:#"; b%(D, B9%(D, 4), 17)
	LOCATE 12, 50: PRINT USING "FA:####"; b%(D, B9%(D, 6), 19); : LOCATE , 62
	PRINT CHR$(219);
	LOCATE , 69: PRINT USING "FA:####"; b%(D, B9%(D, 4), 19)
	LOCATE 14, 46: COLOR l%(D, 11), l%(D, 12): PRINT LEFT$(b$(D, B9%(D, 5)), 10);
	LOCATE , 70: PRINT LEFT$(b$(D, B9%(D, 3)), 10): COLOR 15, 2
	LOCATE 15, 47: PRINT USING "R:#"; b%(D, B9%(D, 5), 17); : LOCATE 17, 49
	PRINT CHR$(219);
	LOCATE , 78: PRINT CHR$(219); : LOCATE 15, 71: PRINT USING "R:#"; b%(D, B9%(D, 3), 17)
	LOCATE 16, 47: PRINT USING "FA:####"; b%(D, B9%(D, 5), 19);
	LOCATE , 71: PRINT USING "FA:####"; b%(D, B9%(D, 3), 19)
	LOCATE 17, 55: COLOR l%(D, 11), l%(D, 12): PRINT "["; B1$(p%(D, P1%(D), 0) + 2); "] "; LEFT$(p$(D, P1%(D)), 10): COLOR 15, 2
	LOCATE 18, 59: PRINT USING "H:#"; p%(D, P1%(D), 18)
	LOCATE 19, 59: PRINT USING "FA:####"; p%(D, P1%(D), 17)
	LOCATE 21, 62: PRINT CHR$(219)
	LOCATE 23, 58: COLOR l%(D, 11), l%(D, 12): PRINT LEFT$(b$(D, B9%(D, 2)), 10): COLOR 15, 2
	LOCATE 24, 59: PRINT USING "   A:#"; b%(D, B9%(D, 2), 15);
	LOCATE 25, 59: PRINT USING "FA:####"; b%(D, B9%(D, 2), 19);
	COLOR 7, 0: FOR X = 5 TO 25: LOCATE X, 1: PRINT STRING$(44, 32); : NEXT
	COLOR 1: LOCATE 4, 1: PRINT STRING$(20, 196); CHR$(194); STRING$(23, 196); CHR$(194)
	LOCATE 14, 1: PRINT STRING$(20, 196); CHR$(193); STRING$(23, 196); CHR$(180)
END SUB


'------------------------------
'   FLDERR Subroutine
'------------------------------
' ...explanation...
SUB FLDERR (F%, W%, D, I3, SB%, S2%, p)
	F% = 0
	IF W% = 1 THEN I3 = p%(D, P1%(D), 17) + 30
	IF W% = 1 AND SB% = 1 THEN I3 = 995
	IF W% <> 1 THEN CALL FLDAVG (D, W%, I3)
	IF I3 >= 999 THEN I6 = 1: GOTO 4408
	IF S2% >= 12 AND S2% <= 15 THEN I6 = 1009 - I3
	IF S2% >= 7 AND S2% <= 9 THEN I6 = 1006 - I3
	IF S2% = 16 OR S2% >= 22 THEN I6 = 1000 - I3
	IF S2% >= 17 AND S2% <= 21 THEN I6 = (1000 - I3) * .05

4408 IF W% = 7 THEN I6 = I6 - 4
	IF W% = 8 THEN I6 = I6 - 10
	IF W% = 9 THEN I6 = I6 - 15
	IF W% = 5 THEN I6 = I6 - 15
	H6% = INT(RND(1) * 1000) + 1: IF H6% > I6 THEN 4421
	F% = 1: i = INT(RND(1) * 2) + 1: I1 = 2
	IF W% = 1 THEN I5 = P1%(D): I6 = 34: CALL ADDPIT(D, I5, I6)
	IF W% <> 1 THEN I5 = B9%(D, W%): I6 = 11: CALL INCBATDF(D, I5, I6)
	IF S2% <> 12 THEN 4410
	IF S2% = 12 THEN PRINT Q$(0); " beats out an infield single": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF(P, I5, I6): I6 = 1: CALL INCBATOFF(P, I5, I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT(D, I5, I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF(P, I5, I6): I6 = 24: CALL INCPITOFF(P, I5, I6)

4410 IF S2% >= 17 AND S2% <= 21 THEN I1 = 3
	IF S2% >= 17 AND S2% <= 20 THEN CALL OUTFIELDERR
	IF S2% = 8 OR S2% = 9 OR S2% >= 22 OR S2% = 16 THEN I1 = 1
	IF S2% >= 12 AND S2% <= 14 AND i = 1 OR S2% = 15 THEN I1 = 1
	CALL RANDNUM(rn)
	IF I1 = 1 AND W% = 1 AND S2% = 16 THEN PRINT "*** WILD PICK OFF THROW BY"; : GOTO 4415
	IF I1 = 1 AND rn <= 50 THEN PRINT "*** THROWING ERROR ON"; : GOTO 4415
	IF I1 = 1 AND rn > 50 AND rn <= 55 THEN PRINT "*** BALL KICKED BY"; : GOTO 4415
	IF I1 = 1 THEN PRINT "*** "; Q$(4); " THROWS IT AWAY ***": GOTO 4403
	IF I1 = 2 AND rn <= 30 THEN PRINT "*** FIELDING ERROR ON"; : GOTO 4415
	IF I1 = 2 AND rn > 30 AND rn <= 50 THEN PRINT "*** OFF THE GLOVE OF"; : GOTO 4415
	IF I1 = 2 AND rn > 50 AND rn <= 75 THEN PRINT "*** BALL IS BOOTED BY"; : GOTO 4415
	IF I1 = 2 AND rn > 75 AND rn <= 95 THEN PRINT "*** BALL IS BOBBLED BY"; : GOTO 4415
	IF I1 = 2 AND rn > 95 THEN PRINT "*** BALL IS THROUGH THE LEGS OF"; : GOTO 4415
	IF I1 = 3 AND rn <= 33 THEN PRINT "*** BALL DROPPED BY"; : GOTO 4415
	IF I1 = 3 AND rn > 33 AND rn <= 66 THEN PRINT "*** OFF THE GLOVE OF"; : GOTO 4415
	IF I1 = 3 THEN PRINT "*** BALL IS MISPLAYED BY";

4415 PRINT " "; Q$(4); " ***"

4403 IF S2% = 16 OR S2% = 22 THEN I8 = INT(RND(1) * 10) + 1: IF I8 < 5 THEN A5%(1) = 2: A5%(2) = 2: A5%(3) = 1: A5%(0) = 2: CALL DELAY: GOTO 4418
	i = INT(RND(1) * 100) + 1
	A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: A5%(0) = 1: CALL DELAY
	IF S2% > 17 AND S2% <= 21 THEN 4418
	IF i <= 17 AND S2% <> 8 OR S2% = 12 THEN A5%(1) = 2: A5%(2) = 2: A5%(0) = 2

4418 IF S2% >= 17 AND S2% <= 20 AND O%(0) = 2 OR S2% >= 23 OR S2% = 16 AND W% = 2 THEN A5%(2) = 2: A5%(1) = 2: A5%(0) = 2: A5%(3) = 1
	IF S2% >= 17 AND S2% <= 19 AND i <= 33 AND O%(0) < 2 AND B0%(1, 0) = 0 THEN A5%(0) = 2
	IF S2% = 16 OR S2% = 22 THEN A5%(0) = 0
	IF I1 > 0 AND S2% <> 7 AND S2% <> 8 AND S2% <> 9 AND S2% <> 16 AND S2% <> 22 THEN O%(1) = O%(1) + 1
	IF S2% = 16 OR S2% = 7 OR S2% = 8 OR S2% = 9 OR S2% = 22 THEN A5%(0) = 0: IF I8 > 4 AND W% <> 2 THEN A5%(1) = 1: A5%(2) = 1: A5%(3) = 1
	S6%(D, 2) = S6%(D, 2) + 1
	CALL ADVANCEMENT(p, D, S2%, F%,I3)

4421 '
END SUB


'------------------------------
'   GETFIELDER Subroutine
'------------------------------
' ...explanation...
SUB GETFIELDER (W%, p, I1, D)
	W% = 0
	i = INT(RND(1) * 100) + 1
	IF B7%(p, B1!(p)) <> 1 AND b%(p, B3%(p, B1!(p)), 0) < 0 THEN I1 = -1
	IF B7%(p, B1!(p)) <> 1 AND b%(p, B3%(p, B1!(p)), 0) > 0 THEN I1 = 1
	IF B7%(p, B1!(p)) <> 1 AND b%(p, B3%(p, B1!(p)), 0) = 0 THEN I1 = -1 * p%(D, P1%(D), 0)
	IF B7%(p, B1!(p)) = 1 THEN I1 = p%(p, P1%(p), 0)
END SUB


'------------------------------
'   GETFIELDNAME Subroutine
'------------------------------
' ...explanation...
SUB GETFIELDNAME (W%, D)
	IF W% <> 1 THEN Q$(4) = b$(D, B9%(D, W%))
	IF W% = 1 THEN Q$(4) = p$(D, P1%(D))
END SUB


'------------------------------
'   GETINFIELDER Subroutine
'------------------------------
' ...explanation...
SUB GETINFIELDER (W%, p, I1, D, S2%, P2)
	CALL GETFIELDER(W%, p, I1, D)
	i = INT(RND(1) * 100) + 1
	IF I1 = 1 THEN W% = 3
	IF I1 = 1 AND i <= 92 THEN W% = 4
	IF I1 = 1 AND i <= 72 THEN W% = 6
	IF I1 = 1 AND i <= 32 THEN W% = 5
	IF I1 = -1 THEN W% = 5
	IF I1 = -1 AND i <= 88 THEN W% = 6
	IF I1 = -1 AND i <= 68 THEN W% = 4
	IF I1 = -1 AND i <= 28 THEN W% = 3
	IF i <= 12 AND S2% <> 20 THEN W% = 1
	IF i <= 4 OR i <= 12 AND S2% = 20 THEN W% = 2
	IF S2% = 21 AND W% = 2 THEN W% = 1
	IF P2 = 4 AND W% = 6 THEN W% = 1
	IF P2 = 4 AND W% = 4 OR S2% = 20 AND W% = 1 THEN W% = 2
	CALL GETFIELDNAME(W%, D)
END SUB


'------------------------------
'   GETOUTFIELDER Subroutine
'------------------------------
' ...explanation...
SUB GETOUTFIELDER (W%, p, I1, D)
	CALL GETFIELDER(W%, p, I1, D)
	i = INT(RND(1) * 100) + 1
	IF I1 = 1 THEN W% = 7
	IF I1 = 1 AND i <= 50 THEN W% = 9
	IF I1 = -1 THEN W% = 9
	IF I1 = -1 AND i <= 50 THEN W% = 7
	IF i <= 30 THEN W% = 8
	Q$(4) = b$(D, B9%(D, W%))
END SUB


'------------------------------
'   GETRPBP Subroutine
'------------------------------
' ...explanation...
SUB GETRPBP (Y%, Y$, D)
OPEN "revised.pbp" FOR RANDOM AS #1 LEN = 44
	FIELD #1, 44 AS YY$
	GET #1, Y%
	y$ = yy$
CLOSE 1

3996 'PRINT Y$; " BEF", Y%, YY$
	IF RIGHT$(Y$, 1) = " " THEN Y$ = LEFT$(Y$, LEN(Y$) - 1): GOTO 3996
	if TGTBASE% = 3 and left$(y$, 2) = "Q1" then y$ = "Q2 " + right$(y$,len(y$)-3)
	if TGTBASE% = 3 and right$(y$, 2) = "Q1" then y$ = left$(y$,len(y$)-2) + "Q2"
	IF LEFT$(Y$, 2) = "BB" THEN Y$ = Q$(0) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "Q1" THEN Y$ = Q$(1) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "Q2" THEN Y$ = Q$(2) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "PP" THEN Y$ = p$(D, P1%(D)) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "UU" THEN Y$ = U$(1) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "XX" THEN Y$ = b$(D, B9%(D, 5))+" and "+b$(D, B9%(D, 6))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "TT" THEN Y$ = b$(D, B9%(D, 5))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "FB" THEN Y$ = b$(D, B9%(D, 3))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "YY" THEN Y$ = b$(D, B9%(D, 4))+" and "+b$(D, B9%(D, 3))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "ZZ" THEN Y$ = b$(D, B9%(D, 4))+" and "+b$(D, B9%(D, 6))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "Q4" THEN Y$ = Q$(4) + RIGHT$(Y$, LEN(Y$) - 2)
	IF RIGHT$(Y$, 3) = " PP" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + p$(D, P1%(D))
	IF RIGHT$(Y$, 3) = " BB" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(0)
	IF RIGHT$(Y$, 2) = "Q1" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(1)
	IF RIGHT$(Y$, 2) = "Q2" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(2)
	IF RIGHT$(Y$, 2) = "UU" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + U$(1)
	IF RIGHT$(Y$, 2) = "Q4" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(4)
	IF RIGHT$(Y$, 3) = " TT" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 5))
	IF RIGHT$(Y$, 2) = "FB" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 3))
	IF RIGHT$(Y$, 3) = " XX" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 5))+" and "+b$(D, B9%(D, 6))
	IF RIGHT$(Y$, 2) = "YY" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 3))+" and "+b$(D, B9%(D, 4))
	IF RIGHT$(Y$, 3) = " ZZ" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 6))+" and "+b$(D, B9%(D, 4))
	'PRINT Y$; " AFT"
END SUB


'------------------------------
'   GROUNDERPBP Subroutine
'------------------------------
' ...explanation...
SUB GROUNDERPBP (W%, D)
	IF W% = 2 THEN 3760
	CALL RANDNUM(rn)
	Y% = INT(RND(1) * 140) + 1930
	IF rn <= 35 THEN 3752
	IF w% = 1 THEN Y% = INT(RND(1) * 160) + 2070
	if w% = 3 then Y% = INT(RND(1) * 150) + 2230
	if w% = 4 then Y% = INT(RND(1) * 160) + 2380
	if w% = 6 then Y% = INT(RND(1) * 160) + 2540
	if w% = 5 then Y% = INT(RND(1) * 150) + 2700

3752 CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$; " ";

3751 GOTO 3765

3760 '*** in front of plate ***
	Y% = INT(RND(1) * 25) + 2870
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$
	CALL DELAY

3765 '
END SUB


'------------------------------
'   GROUNDERS Subroutine
'------------------------------
' ...explanation...
SUB GROUNDERS (W%, D)
	'*** grounders to infielders ***
	X$ = Q$(4)
	IF W% = 2 THEN 3953
	Y% = INT(RND(1) * 120) + 1420
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	PRINT X$; " "; Y$
	CALL DELAY
	GOTO 3954

3953 ' field grounders in front of the plate
	Y% = INT(RND(1) * 13) + 2850
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	IF LEFT$(Y$, 2) = "QQ" THEN Y$ = X$ + RIGHT$(Y$, LEN(Y$) - 2)
	PRINT Y$
	CALL DELAY

3954 '
END SUB


'------------------------------
'   HITS2GAP Subroutine
'------------------------------
' ...explanation...
SUB HITS2GAP (W%, p, I1, D)
	CALL GETFIELDER(W%, p, I1, D): i = INT(RND(1) * 100) + 1
	IF I1 = 1 THEN W% = 7
	IF I1 = 1 AND i <= 15 THEN W% = 9
	IF I1 = -1 THEN W% = 9
	IF I1 = -1 AND i <= 15 THEN W% = 7
	IF i <= 5 THEN W% = 8
	Q$(4) = b$(D, B9%(D, W%))
END SUB


'------------------------------
'   HOLDSTATUS Subroutine
'------------------------------
' ...explanation...
SUB HOLDSTATUS (H0%)
	LOCATE 17, 77: COLOR 15, 2: PRINT H0$(H0%): COLOR , 0
END SUB


'------------------------------
'   INCBATDF Subroutine
'------------------------------
' ...explanation...
SUB INCBATDF (D, I5, I6)
	B8%(D, I5, I6) = B8%(D, I5, I6) + 1
	B5%(D, I5, I6) = B5%(D, I5, I6) + 1
END SUB


'------------------------------
'   INCBATOFF Subroutine
'------------------------------
' ...explanation...
SUB INCBATOFF (p, I5, I6)
	B8%(p, I5, I6) = B8%(p, I5, I6) + 1
	B5%(p, I5, I6) = B5%(p, I5, I6) + 1
	IF I6 > 8 OR I6 = 2 OR I6 = 3 THEN 105
	IF P%(1 - P, P1%(1 - P), 0) = 1 THEN 105
	IF I6 > = 4 THEN CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) + 1:GOTO 105
	CSS%(P, I5, I6) = CSS%(P, I5, I6) + 1

105 END SUB


'------------------------------
'   INCPITOFF Subroutine
'------------------------------
' ...explanation...
SUB INCPITOFF (p, I5, I6)
	P8%(p, I5, I6) = P8%(p, I5, I6) + 1
	P5%(p, I5, I6) = P5%(p, I5, I6) + 1
END SUB


'------------------------------
'   INCREMENTHIT Subroutine
'------------------------------
' ...explanation...
SUB INCREMENTHIT (p, D, MO)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 15: CALL INCBATOFF (P, I5, I6): GOTO 3646
	I5 = P1%(p): I6 = 37: CALL INCPITOFF(P, I5, I6)

3646 I5 = P1%(D): I6 = 13: CALL ADDPIT(D, I5, I6)
	IF MO > 0 THEN MO = MO - 1
END SUB


'------------------------------
'   INJPIT Subroutine
'------------------------------
' ...explanation...
SUB INJPIT (p, D)
	'*** Defensive Pitcher Ejected/Injured ***
	IF u6 = 2 OR u6 = 1 AND p <> u9 THEN 2755
	PRINT "You must replace "; p$(D, P1%(D)): CALL DELAY: GOTO 2799

2755 CALL RELIEVERS

2799 '
END SUB


'------------------------------
'   INJURYLENGTH Subroutine
'------------------------------
' ...explanation...
SUB INJURYLENGTH (IJL%, D, po%, u6, p, u9, P9, W5, W6)
	IJL% = IJL% + 1
	IJ$(IJL%) = b$(D, B9%(D, po%))
	CALL DELAY: PRINT b$(D, B9%(D, po%)); " is injured for"
	'*** Determine Injury Length ***
	il = 0
	IF b%(D, B9%(D, po%), 3) = 162 THEN PRINT " remainder of game": GOTO 2115
	GOSUB 2116
	IF IL=0 THEN 2115
	IF b%(D, B9%(D, po%), 3) = 161 THEN PRINT " this game and next game": il = 1: GOTO 2115
	il = INT(RND(1) * (162 - b%(D, B9%(D, po%), 3))) + 1
	PRINT il; " games"

2115 b%(D, B9%(D, po%), 32) = b%(D, B9%(D, po%), 32) + il
	CALL DELAY: CALL DELAY
	CALL EJECTIONINJURY(u6, p, u9, D, po%, P9, W5, W6)
	GOTO 2120

2116 ct = 0
	FOR I = 0 TO 22
	FOR I1 = 22 TO 25
	IF B%(D, I, I1) = PO% AND B%(D, I, 32) = 0 AND I <> B9%(D, po%) THEN ct = ct + 1
	if ct > 3 then il= 1:goto 2117
	NEXT
	NEXT
	PRINT " remainder of game":IL=0

2117 RETURN

2120
END SUB


'------------------------------
'   INNDISP Subroutine
'------------------------------
' ...explanation...
SUB INNDISP (I2, i)
	IF I2 < 4 THEN LOCATE , 3 * I2 + 7
	IF I2 > 3 AND I2 < 7 THEN LOCATE , 3 * I2 + 8
	IF I2 > 6 AND I2 < 10 THEN LOCATE , 3 * I2 + 9
	IF I2 > 9 AND I2 < 13 THEN LOCATE , 3 * (I2 - 9) + 7
	IF I2 > 12 AND I2 < 16 THEN LOCATE , 3 * (I2 - 9) + 8
	IF I2 > 15 AND I2 < 19 THEN LOCATE , 3 * (I2 - 9) + 9
	IF I2 > 18 AND I2 < 22 THEN LOCATE , 3 * (I2 - 18) + 7
	IF I2 > 21 AND I2 < 25 THEN LOCATE , 3 * (I2 - 18) + 8
	IF I2 > 24 AND I2 < 28 THEN LOCATE , 3 * (I2 - 18) + 9
	PRINT USING "##"; s%(i, I2);
END SUB


'------------------------------
'   LDRIVE Subroutine
'------------------------------
' ...explanation...
SUB LDRIVE ()
	CALL RANDNUM(rn)
	IF rn <= 20 THEN X$ = "rips"
	IF rn > 20 AND rn <= 40 THEN X$ = "hammers"
	IF rn > 40 AND rn <= 60 THEN X$ = "lashes"
	IF rn > 60 AND rn <= 80 THEN X$ = "lines"
	IF rn > 80 THEN X$ = "drives"
	PRINT " "; X$; " ";
END SUB


'------------------------------
'   LET2NUM Subroutine
'------------------------------
' ...explanation...
SUB LET2NUM (i$)
	IF UCASE$(i$) = "A" THEN i$ = "10"
	IF UCASE$(i$) = "B" THEN i$ = "11"
	IF UCASE$(i$) = "C" THEN i$ = "12"
	IF UCASE$(i$) = "D" THEN i$ = "13"
	IF UCASE$(i$) = "E" THEN i$ = "14"
	IF UCASE$(i$) = "F" THEN i$ = "15"
	IF UCASE$(i$) = "G" THEN i$ = "16"
	IF UCASE$(i$) = "H" THEN i$ = "17"
	IF UCASE$(i$) = "I" THEN i$ = "18"
	IF UCASE$(i$) = "J" THEN i$ = "19"
	IF UCASE$(i$) = "K" THEN i$ = "20"
	IF UCASE$(i$) = "L" THEN i$ = "21"
	IF UCASE$(i$) = "M" THEN i$ = "22"
END SUB


'------------------------------
'   LONGFLY Subroutine
'------------------------------
' ...explanation...
SUB LONGFLY (D, W%)
	Y% = INT(RND(1) * 60) + 1310
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$; " "; C$(W%): CALL DELAY
	IF WF! < 1 AND RND(1) > WF! THEN PRINT "The wind is bringing it back...": CALL DELAY: GOTO 8055
	IF WF! > 1 AND RND(1) <= 1 - WF! THEN PRINT "It's carrying well in the breeze...": CALL DELAY
	Y% = INT(RND(1) * 50) + 1370
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$
	CALL DELAY
	CALL RANDNUM(rn)
	IF (l%(1, 13) = 21 OR l%(1, 13) = 27) AND W% = 7 THEN 8059
	IF rn <= 5 THEN PRINT Q$(4); " leaps"
	IF rn >  5 and rn <= 10 THEN PRINT Q$(4); " against the wall"
	IF rn > 10 AND rn <= 20 THEN PRINT Q$(4); " jumps at the wall"
	IF rn > 20 AND rn <= 30 THEN PRINT Q$(4); " climbs the wall"

8059 CALL DELAY

8055 REM
END SUB


'------------------------------
'   MAINSCREEN Subroutine
'------------------------------
' ...explanation...
SUB MAINSCREEN (D, p, BC%, U%, MO)
	FOR I5 = 5 TO 13
	LOCATE I5, 1
	CALL BLANKLINE
	NEXT I5
	LOCATE 5, 1
	LOCATE 21, 48: COLOR 15, 2: PRINT TAB(62); CHR$(219); TAB(79); : COLOR 7, 0
	LOCATE 10, 1: COLOR l%(D, 11), l%(D, 12)
	PRINT p$(D, P1%(D)); " "; B1$(p%(D, P1%(D), 0) + 2): COLOR 15, 0
	CALL DISPBAVG(D)
	COLOR 1: FOR X = 5 TO 13: LOCATE X, 21: PRINT CHR$(179); : NEXT
	LOCATE 11, 1: COLOR 2, 0: PRINT "W-L ERA "; : COLOR 15, 0
	PRINT USING "##"; p%(D, P1%(D), 1); : PRINT "-"; : PRINT USING "##"; p%(D, P1%(D), 2);
	PRINT " "; : PRINT USING "##.##"; p%(D, P1%(D), 10) / 100
	COLOR 2, 0
	PRINT "  IP  H  BB  K GAME": COLOR 15, 0
	IF P8%(D, P1%(D), 0) = 0 THEN PRINT " 0.0"; : GOTO 338
	PRINT USING "##"; INT(P8%(D, P1%(D), 0) / 3);
	PRINT "."; : PRINT USING "#"; P8%(D, P1%(D), 0) - (INT(P8%(D, P1%(D), 0) / 3) * 3);

338 PRINT USING " ##  ## ##"; P8%(D, P1%(D), 1); P8%(D, P1%(D), 4); P8%(D, P1%(D), 5)
	LOCATE 5, 1
	J = B3%(p, B1!(p)): COLOR l%(p, 11), l%(p, 12)
	IF B7%(p, B1!(p)) = 1 THEN X$ = p$(p, P1%(p)): PRINT X$; " "; B1$(p%(p, P1%(p), 0) + 2); : CALL PITPLACEMENT(p, X$): GOTO 335
	X$ = b$(p, J): PRINT X$; " "; B1$(b%(p, J, 0) + 2);
	IF b%(p, J, 0) < 0 OR b%(p, J, 0) = 0 AND p%(D, P1%(D), 0) > 0 THEN LOCATE 21, 64
	X = LEN(X$): IF b%(p, J, 0) > 0 OR b%(p, J, 0) = 0 AND p%(D, P1%(D), 0) < 0 THEN LOCATE 21, 51 + 10 - X
	PRINT X$

335 LOCATE 6, 1: COLOR 2, 0: PRINT " AVG HR  BB   K": COLOR 15, 0
	IF B7%(p, B1!(p)) = 1 THEN 336
	IF b%(p, J, 4) = 0 THEN PRINT ".000  0   0   0"; : GOTO 337
	PRINT USING ".### ## ### ###"; b%(p, J, 6) / b%(p, J, 4); b%(p, J, 9); b%(p, J, 11); b%(p, J, 12): GOTO 337

336 IF p%(p, P1%(p), 11) <> 999 THEN PRINT USING ".###"; p%(p, P1%(p), 11) / 1000: GOTO 337
	IF p%(p, P1%(p), 20) = 0 THEN PRINT ".000 0"; : GOTO 337
	PRINT USING ".### ## ### ###"; p%(p, P1%(p), 22) / p%(p, P1%(p), 20); p%(p, P1%(p), 25); p%(p, P1%(p), 27); p%(p, P1%(p), 28)

337 LOCATE 8, 2: IF B7%(p, B1!(p)) = 1 THEN 332
	PRINT B8%(p, B3%(p, B1!(p)), 1); "-"; B8%(p, B3%(p, B1!(p)), 0); : GOTO 333

332 PRINT P8%(p, P1%(p), 24); "-"; P8%(p, P1%(p), 23);

333 PRINT " TODAY"
	COLOR 14, 0: LOCATE 3, 58: PRINT O%(0): LOCATE 2, 58: PRINT U%: LOCATE 1, 58: PRINT BC%
	COLOR 15, 2: LOCATE 22, 46: PRINT "INFIELD:"
	LOCATE 23, 46
	IF INFPOS% = 0 THEN PRINT "NORMAL    "
	IF INFPOS% = 1 THEN PRINT "CORNERS IN"
	IF INFPOS% = 2 THEN PRINT "INFIELD IN"
	CALL HOLDSTATUS(H0%)
	BK$ = "        "
	LOCATE 13, 58: COLOR 15, 2: PRINT TAB(68); : LOCATE 14, 58: PRINT BK$: LOCATE 13, 58
	I2 = 2: X = 58: CALL DISPRUN(I2, p, X)
	IF O%(0) <> 3 THEN MO = 0
	FOR WK = 1 TO 3
	IF B0%(WK, 0) > 0 THEN MO = MO + 1
	NEXT
	LOCATE 18, 47: COLOR 15, 2: PRINT TAB(57); : LOCATE 19, 47: PRINT BK$: LOCATE 18, 47
	I2 = 3: X = 47: CALL DISPRUN(I2, p, X)
	LOCATE 18, 69: COLOR 15, 2: PRINT TAB(79); : LOCATE 19, 70: PRINT BK$: LOCATE 18, 69
	I2 = 1: X = 70: CALL DISPRUN(I2, p, X)
	COLOR 15, 0
END SUB


'------------------------------
'   NEWLINES Subroutine
'------------------------------
' ...explanation...
SUB NEWLINES ()
	FOR I5 = 15 TO 24: LOCATE , 1: LOCATE I5: CALL BLANKLINE: NEXT I5: LOCATE 15: LOCATE , 1
END SUB


'------------------------------
'   NEWPITCHER Subroutine
'------------------------------
' ...explanation...
SUB NEWPITCHER
	V0 = i
	IF S1%(D) <> -1 AND P6%(D) > 1 THEN I5 = P1%(D): I6 = 22: CALL ADDPIT(D, I5, I6)
	P1%(D) = V0: P6%(D) = P6%(D) + 1: p%(D, V0, 15) = P6%(D): P9 = D: CALL PITSTAM(P9)
	FOR V0 = 1 TO 9
	IF B7%(D, V0) = 1 OR B7%(D, V0) = 10 THEN B3%(D, V0) = P1%(D): B7%(D, V0) = 1: B9%(D, 1) = P1%(D): P9 = D: W5 = P9: W6 = P9: VV(P9) = V0: VV1(P9) = V0: CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	NEXT V0
	VV(P9)=1:VV1(P9)=9
	B9%(D, 1) = P1%(D): CALL CHECKSAVE(D, p, S4%)
	W5 = 1 - P9: W6 = 1 - P9: VV(1 - P9) = 1: VV1(1 - P9) = 9
	CALL NEWLINES: LOCATE 15, 1
	CALL RANDNUM(rn)
	IF rn <= 50 THEN PRINT "Hold the phone...": CALL DELAY
	PRINT M$(D); " is making a pitching change"
	CALL DELAY: CALL DELAY
	CALL HITTINGLINES (l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	CALL FLDDISP (BC%, p, U%, D): CALL MAINSCREEN (D, p, BC%, U%, MO): PPH = 0

8895 pz=1
END SUB


'------------------------------
'   NOJUMP Subroutine
'------------------------------
' ...explanation...
SUB NOJUMP (D)
	Y% = INT(RND(1) * 20) + 1050
	CALL GETRPBP(Y%, Y$, D)
	  CALL STRIPRPBP (Y$,Y%,D)
	PRINT Y$:CALL DELAY:CALL DELAY
END SUB


'------------------------------
'   OFFPIT Subroutine
'------------------------------
' ...explanation...
SUB OFFPIT (P9, I5, I6)
	P8%(P9, I5, I6) = P8%(P9, I5, I6) + 1: P5%(P9, I5, I6) = P5%(P9, I5, I6) + 1
END SUB


'------------------------------
'   OUTATFIRST Subroutine
'------------------------------
' ...explanation...
SUB OUTATFIRST (W%, D, fbu%, FB2P%)
	CALL PITCHERNAME(W%, D)
	CALL GROUNDERS(W%, D)
	CALL RANDNUM(rn)
	IF W% = 3 AND rn <= 94 THEN PRINT "he takes it to the bag unassisted": fbu% = 1: GOTO 3745
	IF W% = 3 THEN W% = 1: CALL PITCHERNAME(W%, D): PRINT "He flips it to "; Q$(4); " covering": FB2P% = 1: W% = 3
	IF W% = 4 THEN CALL SECOND2FIRST(D)
	IF W% = 5 OR W% = 6 THEN CALL THROW2FIRST(D)

3745 '
END SUB


'------------------------------
'   OUTFIELDERR Subroutine
'------------------------------
' ...explanation...
SUB OUTFIELDERR ()
	'*** outfielders in trouble ***
	CALL RANDNUM(rn)
	IF rn <= 20 THEN PRINT Q$(4); " can't find it"
	IF rn > 20 AND rn <= 40 THEN PRINT Q$(4); " misjudges it"
	IF rn > 40 AND rn <= 60 THEN PRINT Q$(4); " turns the wrong way"
	IF rn > 60 AND rn <= 80 THEN PRINT Q$(4); " loses it"
	IF rn > 80 THEN PRINT Q$(4); " doesn't see it"
	CALL DELAY
END SUB


'------------------------------
'   OUTFLDLOCATION Subroutine
'------------------------------
' ...explanation...
SUB OUTFLDLOCATION (W%)
	CALL RANDNUM(rn)
	IF W% = 7 OR W% = 8 AND rn > 50 THEN PRINT "lcf"
	IF W% = 9 OR W% = 8 AND rn <= 50 THEN PRINT "rcf"
END SUB


'------------------------------
'   OUTFLDNAME Subroutine
'------------------------------
' ...explanation...
SUB OUTFLDNAME (W%, D)
	W% = 9: i = INT(RND(1) * 100) + 1
	IF i <= 25 THEN W% = 8
	IF i <= 5 THEN W% = 7
	Q$(4) = b$(D, B9%(D, W%))
END SUB


'------------------------------
'   PICKEDOFF Subroutine
'------------------------------
' ...explanation...
SUB PICKEDOFF (D, i)
	'*** picked off ***
	CALL NEWLINES
	CALL DELIVERY(D): CALL RANDNUM(rn)
	IF rn <= 33 THEN PRINT "NO, "; p$(D, P1%(D)); " throws to "; H$(i - 1)
	IF rn > 33 AND rn <= 66 THEN PRINT "quick move to "; H$(i - 1)
	IF rn > 66 THEN PRINT "pickoff throw to "; H$(i - 1)
	CALL DELAY: CALL RANDNUM(rn)
	IF rn <= 50 THEN PRINT Q$(i - 1); " is picked off base!"
	IF rn > 50 THEN PRINT Q$(i - 1); " is caught napping"
	CALL DELAY: I8 = INT(RND(1) * 100) + 1
	IF I8 <= 25 THEN PRINT "he can't believe it"
	IF I8 > 25 AND I8 <= 50 THEN PRINT "what a teriffic move by "; p$(D, P1%(D))
	CALL DELAY
END SUB


'------------------------------
'   PINCHRUNNER Subroutine
'------------------------------
' ...explanation...
SUB PINCHRUNNER (MS%, A1, S4%, I2, p, PR%, BC%, U%, D, MO)
	' *** CHECK FOR PINCH RUNNER ***
	i0% = 0
	IF A1 > 1 OR A1 < -2 OR INNING% < 8 THEN 8999
	I2=1
	IF S4% = 1 OR S4% = 5 THEN I2 = 1
	IF S4% = 2 OR S4% = 4 OR S4% = 6 THEN I2 = 2
	' J REPRESENTS THE SPOT IN THE BATTING ORDER
	' j1 represents the base
	J = B0%(I2, 0):J1 = I2
	IF J = 0 THEN 8999
	IF MG%(P,771) < > 999 THEN 8000
	FOR I1=772 TO 777
	IF MG%(P,I1) < > -1 AND B%(P,MG%(P,I1),21)=0 THEN I1=MG%(P,I1):GOTO 8001
	MG%(P,I1)=-1
	NEXT
	GOTO 8999

8001 IF B7%(p, J) = 1 THEN 8010
	FOR I = 0 TO 5
	IF MG%(P,I*4+779) = B3%(p, J) THEN 8003
	NEXT
	GOTO 8999

8003 'FOUND A MATCH FOR THE RUNNER ON BASE
	CALL RANDNUM (RN)
	IF INNING%>=MG%(P,I*4+780) AND B%(P,MG%(P,I*4+781),21)=0 AND RN<=MG%(P,I*4+782) THEN B%(P,MG%(P,I*4+781),21)=40+J:GOTO 8993
	GOTO 8999

8010 'PINCH RUNNING FOR THE PITCHER
	IF MG%(P,803)<>999 THEN 8999
	CALL RANDNUM (RN)
	IF P%(P,P1%(P),31)<=MG%(P,804) AND INNING%>=MG%(P,805) AND RN<=MG%(P,806) THEN 8993
	IF P%(P,P1%(P),31)<=MG%(P,807) AND INNING%>=MG%(P,808) AND RN<=MG%(P,809) THEN 8993
	GOTO 8999

8000 IF B7%(p, J) = 1 AND P%(P,P1%(P),31)>4 THEN 8999
	IF B7%(P,J)<>1 AND b%(p, B3%(p, J), 16) > 3 THEN 8999
	I1=-1
	FOR i = 0 TO 22
	IF b$(p, i) = "XXX" OR b%(p, i, 21) > 0 THEN 8992
	FOR I2 = 22 TO 25
	IF B%(P,I,I2) < > -1 AND B%(P,I,I2) = B7%(P,J) AND B7%(P,J) < > 1 THEN I1 = I
	NEXT
	IF b%(p, i, 16) > 5 THEN PR%(i0%) = i: i0% = i0% + 1

8992 NEXT
	IF i0% = 0 OR I1 = -1 AND B7%(P,J) < > 1 THEN 8999
	IF B7%(P,J) < > 1 THEN B%(P,I1,21) = 40 + J
	rn = INT(RND(1) * i0%)
	I1 = PR%(rn)

8993 I2 = J
	q$(j1) = b$(p,i1):b%(p, I1, 21) = 1: B3%(p, I2) = I1: B9%(p, B7%(p, I2)) = I1
	IF B7%(p, I2) = 1 OR B7%(p, I2) = 10 THEN B9%(p, 1) = 99: B9%(p, 0) = I2: B7%(p, I2) = 10
	D0%(p) = D0%(p) + 1: X0%(p, 0, D0%(p)) = B3%(p, I2)
	X0%(p, 1, D0%(p)) = 11:' WAS EQUAL TO B7%(p, I2)
	X0%(p, 2, D0%(p)) = (I2 * 10) + (D0%(p) - 9)
	W5 = p: W6 = p: VV(p) = I2: VV1(p) = I2
	FOR xx = 22 TO 25
	IF B7%(p, I2) = b%(p, I1, xx) THEN b%(p, I1, 15) = b%(p, I1, 33 + (xx - 22) * 2): b%(p, I1, 17) = b%(p, I1, 34 + (xx - 22) * 2): b%(p, I1, 19) = b%(p, I1, xx + 4): GOTO 8995
	NEXT

8995 CALL NEWLINES: LOCATE 15, 1: PRINT M$(p); " is sending in a pinch runner": PR% = 1
	  CALL DELAY: CALL DELAY
	CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	CALL FLDDISP(BC%, p, U%, D): CALL MAINSCREEN(D, p, BC%, U%, MO): MS% = 1
	VV(P)=1:VV1(P)=9

8999 '
END SUB


'------------------------------
'   PITCHERNAME Subroutine
'------------------------------
' ...explanation...
SUB PITCHERNAME (W%, D)
	IF W% = 1 THEN Q$(4) = p$(D, P1%(D))
	IF W% <> 1 THEN Q$(4) = b$(D, B9%(D, W%))
END SUB


'------------------------------
'   PITCHOUT Subroutine
'------------------------------
' ...explanation...
SUB PITCHOUT (S4%, TGTBASE%, Q0)
	IF S4% = 5 OR TGTBASE% <> 2 THEN 4883: REM*** PITCH OUT ***
	PJ = INT(RND(1) * 100) + 1
	IF PJ<=8 THEN PRINT "...PITCH OUT!!!": CALL DELAY: Q0 = Q0 - 200

4883 '
END SUB


'------------------------------
'   PITDISP Subroutine
'------------------------------
' ...explanation...
SUB PITDISP (P9)
	CLS
	COLOR 14: PRINT "#"; : COLOR l%(P9, 11), l%(P9, 12): PRINT A$(P9); : COLOR 14, 0
	LOCATE , 18: PRINT "T  IP   H   G GS CG SV HR  BB  SO  W- L   ERA  H G%"
	AA = 0: BB = 21
	FOR i = AA TO BB: IF p%(P9, i, 15) > 0 THEN 206
	COLOR 2, 0
	IF i <= 9 THEN PRINT USING "#"; i;
	IF i > 9 THEN PRINT X$(i - 10);
	COLOR 15, 0
	LOCATE , 3: PRINT p$(P9, i); : LOCATE , 18: PRINT B1$(p%(P9, i, 0) + 2);
	PRINT USING " ### ### ### ## ## ## ## ### ### ##"; p%(P9, i, 6); p%(P9, i, 7); p%(P9, i, 4); p%(P9, i, 5); p%(P9, i, 16); p%(P9, i, 3); p%(P9, i, 13); p%(P9, i, 8); p%(P9, i, 9); p%(P9, i, 1); : PRINT "-";
	PRINT USING "##"; p%(P9, i, 2);
	LOCATE , 59: PRINT USING "##.## # ##"; p%(P9, i, 10) / 100; p%(P9, i, 18); p%(P9, i, 12)

206 NEXT i
END SUB


'------------------------------
'   PITOFF Subroutine
'------------------------------
' ...explanation...
SUB PITOFF (p, I5, I6)
	P8%(p, I5, I6) = P8%(p, I5, I6) - 1: P5%(p, I5, I6) = P5%(p, I5, I6) - 1
END SUB


'------------------------------
'   PITPLACEMENT Subroutine
'------------------------------
' ...explanation...
SUB PITPLACEMENT (p, X$)
	IF p%(p, P1%(p), 0) < 0 THEN LOCATE 21, 64
	IF p%(p, P1%(p), 0) > 0 THEN LOCATE 21, 51
	PRINT X$
END SUB


'------------------------------
'   PITSTAM Subroutine
'------------------------------
' ...explanation...
SUB PITSTAM (P9)
	IF p%(P9, P1%(P9), 4) = 0 THEN S8%(P9, 0) = 3
	IF p%(P9, P1%(P9), 4)<>0 THEN S8%(P9, 0) = INT(((p%(P9, P1%(P9), 8) + p%(P9, P1%(P9), 7)) / p%(P9, P1%(P9), 4)) + .5)
	IF S8%(P9, 0) < 3 THEN S8%(P9, 0) = 3
	S8%(P9, 1) = 3
	IF P6%(P9) > 1 THEN S8%(P9, 1) = 0
	 IF CGERA%>0 THEN S8%(P9,0)=S8%(P9,0)+1
	IF CGERA%>0 AND P6%(P9)>1 THEN S8%(P9, 1) = 1
	'1955 - IF RUNS=1 AND HIT+BB INCREMENT BY ONE
	 IF INNING% <= 5 AND S6%(P9, 0) - S6%(1 - P9, 0) > 0 THEN P2%(P9) = P1%(P9)
END SUB


'------------------------------
'   PITSTRAT Subroutine
'------------------------------
' ...explanation...
SUB PITSTRAT
	  A1 = S6%(D, 0) - S6%(p, 0)
	pz=0
	' if there is no middle relievers in the profile and less than the 7th
	' then go back and find one the old fashioned way
	IF MG%(D,60)<>999 AND INNING%<=7 THEN 8195
	  'if no closers set up or no closing strategy defined, skip out
	IF INNING%>=8 AND MG%(D,520)<>999 AND MG%(D,90)<>999 OR MG%(D,581)<>999 THEN 8195
	  'if still needing middle relief
	IF INNING%<=7 THEN 8115

' we have defined a closing/setup possibility
' identify the stratgey
	I2=0
	FOR I = 582 TO 587
	I2 = I2 + MG%(D,I)
	NEXT
	I3 = INT(RND(1) * I2) + 1
	I2 = 0
	FOR I=582 TO 587
	I2=I2+MG%(D,I)
	IF I3<=I2 THEN SN%=I:GOTO 8111
	NEXT
	GOTO 8195

'strategy identified, now

8111 SF%=0
	IF A1 < 1 THEN 8195
	IF A1 > 0 AND A1 <= 2 THEN SF%=1
	IF S4% = 0 AND A1 <= 2 THEN SF%=1
	IF S4% > 0 AND S4% < 4 AND A1 <= 3 THEN SF%=1
	IF S4% > 3 AND S4% < 7 AND A1 <= 4 THEN SF%=1
	IF S4% = 7 AND A1 <= 5 THEN SF%=1
	IF S4% = 0 AND O%(0) = 0 AND A1 <= 3 THEN SF%=1
	  IF B1!(P)<=7 THEN FOR I=0 TO 2:I3=I3+B%(P,B3%(P, B1!(P)+I),0):NEXT
	IF B1!(P)=8 THEN FOR I=0 TO 1:I3=I3+B%(P,B3%(P, B1!(P)+I),0):NEXT:I3=I3+B%(P,B3%(P, 1),0)
	IF B1!(P)=9 THEN FOR I=1 TO 2:I3=I3+B%(P,B3%(P, I),0):NEXT:I3=I3+B%(P,B3%(P, 9),0)

8112 IF I3=0 THEN I3=INT(RND(1)*3)-1
	IF I3=0 THEN 8112
	  IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND SF%=1 AND (SN%=582 OR SN%=587) THEN 8114
	IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND SF%=1 AND (SN%=582 OR SN%=587) THEN 8113
	IF INNING%>=9 AND SF%=1 AND (SN%=582 OR SN%=587) THEN 8113
	  IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>0 AND SN%=583 THEN 8114
	IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>0 AND SN%=583 THEN 8113
	IF INNING%>=9 AND A1>0 AND SN%=583 THEN 8113
	  IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>=0 AND SN%=584 THEN 8114
	IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>=0 AND SN%=584 THEN 8113
	IF INNING%>=9 AND A1>=0 AND SN%=584 THEN 8113
	  IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>0 AND SN%=585 THEN 8114
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>0 AND SN%=585 THEN 8113
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%>=9 AND A1>0 AND SN%=585 THEN 8113
	  IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>=0 AND SN%=586 THEN 8114
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>=0 AND SN%=586 THEN 8113
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%>=9 AND A1>=0 AND SN%=586 THEN 8113
	  GOTO 8195

8114
	I2=0
	IF I3<0 THEN I1=81
	IF I3>0 THEN I1=84
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	NEXT
	I3 = INT (RND(1) * I2) + 1 : I2 = 0
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	IF I3<=I2 THEN V0=MG%(D,I):MG%(D,I)=-1:I=V0:CALL NEWPITCHER:SU%(D,0)=-1:GOTO 8195
	NEXT
	'NO SETUP MAN FOUND
	'GOTO 8195

8113
	I2=0
	IF I3<0 THEN I1=521
	IF I3>0 THEN I1=524
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	NEXT
	I3=INT(RND(1)*I2)+1
	I2=0
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	IF I3<=I2 THEN V0=MG%(D,I):MG%(D,I)=-1:I=V0:CALL NEWPITCHER:SU%(D,1)=-1:GOTO 8195
	NEXT
	'NO CLOSER FOUND
	'GOTO 8195
	SU%(D,0)=-1:SU%(D,1)=-1

8115 I2=0
	FOR I=61 TO 70
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	NEXT
	I3=INT(RND(1)*I2)+1
	I2=0
	FOR I=61 TO 70
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	IF I3<=I2 THEN V0=MG%(D,I):MG%(D,I)=-1:I=V0:CALL NEWPITCHER: GOTO 8195
	NEXT
	'NO MIDDLE RELIEVER FOUND
	GOTO 8195

8195 '
END SUB


'------------------------------
'   PITSWAP Subroutine
'------------------------------
' ...explanation...
SUB PITSWAP (i)
	IF i = 4 THEN V = 3: CALL SOMESTATS(V, p, F%, D): V = 2: CALL SOMESTATS(V, p, F%, D): V = 1: CALL SOMESTATS(V, p, F%, D)
	IF i = 3 THEN V = 2: CALL SOMESTATS(V, p, F%, D): V = 1: CALL SOMESTATS(V, p, F%, D)
	IF i = 2 THEN V = 2: CALL SOMESTATS(V, p, F%, D): V = 1: CALL SOMESTATS(V, p, F%, D)
END SUB


'------------------------------
'   PLAYERNAME Subroutine
'------------------------------
' ...explanation...
SUB PLAYERNAME (p)
	IF B7%(p, B1!(p)) = 1 THEN Q$(0) = p$(p, P1%(p)): GOTO 8002
	Q$(0) = b$(p, B3%(p, B1!(p)))

8002 FOR i = 1 TO 3
	IF B0%(i, 0) > 0 AND B7%(p, B0%(i, 0)) <> 1 THEN Q$(i) = b$(p, B3%(p, B0%(i, 0)))
	IF B0%(i, 0) > 0 AND B7%(p, B0%(i, 0)) = 1 THEN Q$(i) = p$(p, P1%(p))
	NEXT i
END SUB


'------------------------------
'   PLYRHRT Subroutine
'------------------------------
' ...explanation...
SUB PLYRHRT (p, P9, W5, W6, BC%, U%, D, MO)
	'*** Offensive Player Ejected/Injured ***
	IF u6 = 2 OR u6 = 1 AND p = u9 THEN 2805
	PRINT "You must replace "; Q$(0); " with a pinch-hitter": CALL DELAY: GOTO 2849

2805 FOR i = 0 TO 22: IF b%(p, i, 21) > 0 OR b$(p, i) = "XXX" THEN 2808
	IF B7%(p, B1!(p)) <> b%(p, i, 22) AND B7%(p, B1!(p)) <> b%(p, i, 23) AND B7%(p, B1!(p)) <> b%(p, i, 24) AND B7%(p, B1!(p)) <> b%(p, i, 25) THEN 2808

2807 PRINT Q$(0); " will be replaced by "; b$(p, i): CALL DELAY
	D0%(p) = D0%(p) + 1: X0%(p, 0, D0%(p)) = i: X0%(p, 1, D0%(p)) = B7%(p, B1!(p)): X0%(p, 2, D0%(p)) = (B1!(p) * 10) + (D0%(p) - 9)
	b%(p, i, 21) = 1: B3%(p, B1!(p)) = i: B9%(p, B7%(p, B1!(p))) = i: IF B7%(p, B1!(p)) = 1 OR B7%(p, B1!(p)) = 10 THEN B9%(p, 1) = 99: B9%(p, 0) = B1!(p): B7%(p, B1!(p)) = 10
	P9 = p: W5 = P9: W6 = P9: VV(P9) = B1!(p): VV1(P9) = B1!(p)
	CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	VV(P9)=1:VV1(P9)=9
	CALL FLDDISP(BC%, p, U%, D): CALL MAINSCREEN(D, p, BC%, U%, MO): LOCATE 15, 1
	Q$(0) = b$(p, B3%(p, B1!(p))): IF B7%(p, B1!(p)) = 1 THEN Q$(0) = p$(p, P1%(D))
	GOTO 2849

2808 NEXT i
	FOR i = 0 TO 22: IF b%(p, i, 21) > 0 OR b$(p, i) = "XXX" THEN 2810
	GOTO 2807

2810 NEXT i

2849 '
END SUB


'------------------------------
'   POPUP Subroutine
'------------------------------
' ...explanation...
SUB POPUP ()
	' *** POPUP LINEUPS FOR TEAMS ***
	J = 11
	LOCATE 8, 7: COLOR l%(1, 11), l%(1, 12): PRINT " Ö"; STRING$(62, 196); "· ";
	FOR II = 1 TO J: LOCATE 8 + II, 7: PRINT " º"; STRING$(62, 32); "º "; : NEXT
	LOCATE 9 + J, 7: PRINT " Ó"; STRING$(62, 196); "½ ";
	LOCATE 9, 14: PRINT A$(0); TAB(40); A$(1)
	FOR J = 0 TO 1
	FOR X = 1 TO 9: LOCATE X + 9, 9 + J * 26
	IF B1!(J) = X THEN PRINT "->";
	LOCATE , 11 + J * 26
	IF B7%(J, X) <> 1 THEN 841
	PRINT B1$(p%(J, P1%(J), 0) + 2); TAB(14 + J * 26); LEFT$(p$(J, P1%(J)), 10); " p"; TAB(28 + J * 26);
	IF p%(J, P1%(J), 11) <> 999 THEN PRINT USING " .###"; p%(J, P1%(J), 11) / 1000: GOTO 842
	IF p%(J, P1%(J), 20) = 0 THEN PRINT " .000": GOTO 842
	IF p%(J, P1%(J), 20) = p%(J, P1%(J), 22) THEN PRINT "1.000": GOTO 842
	PRINT USING " .###"; p%(J, P1%(J), 22) / p%(J, P1%(J), 20)
	GOTO 842

841 PRINT B1$(b%(J, B3%(J, X), 0) + 2); TAB(14 + J * 26); LEFT$(b$(J, B3%(J, X)), 10); " "; C$(B7%(J, X)); TAB(28 + J * 26);
	IF b%(J, B3%(J, X), 4) = 0 THEN PRINT " .000": GOTO 842
	IF b%(J, B3%(J, X), 6) = b%(J, B3%(J, X), 4) THEN PRINT "1.000": GOTO 842
	PRINT USING " .###"; b%(J, B3%(J, X), 6) / b%(J, B3%(J, X), 4)

842 NEXT: NEXT: COLOR 15, 0

	LOCATE 24, 1: PRINT "Hit any key to continue";
843 I$ = GetKeyPress$

END SUB


'------------------------------
'   POPUPBAT Subroutine
'------------------------------
' ...explanation...
SUB POPUPBAT (WK, M)
	IF TS%(WK,0)=999 THEN LOCATE 4,56:PRINT "VS LHP  .000";
	IF TS%(WK,0)=999 AND B%(WK,M,42)<>0 THEN LOCATE 4,64:PRINT USING ".###";B%(WK,M,43)/B%(WK,M,42)
	IF TS%(WK,0)=999 THEN LOCATE 5,56:PRINT "VS RHP  .000";
	IF TS%(WK,0)=999 AND B%(WK,M,49)<>0 THEN LOCATE 5,64:PRINT USING ".###";B%(WK,M,50)/B%(WK,M,49)
	LOCATE 8, 6: PRINT "SEASON"; : LOCATE , 15
	IF b%(WK, M, 4) = 0 THEN PRINT ".---  .---":GOTO 734
	PRINT USING ".###  .### ### ### ### ### ## ## ## ### ### ### "; b%(WK, M, 6) / b%(WK, M, 4); (b%(WK, M, 6) + b%(WK, M, 11)) / (b%(WK, M, 4) + b%(WK, M, 11)); b%(WK, M, 3); b%(WK, M, 4); b%(WK, M, 5); b%(WK, M, 6); b%(WK, M, 7); b%(WK, M, 8); b%( _
WK, M, 9); b%(WK, M, 10);
	PRINT USING "### ### ### ##"; b%(WK, M, 11); b%(WK, M, 12); b%(WK, M, 13); b%(WK, M, 14)

734 LOCATE , 6: PRINT "LEAGUE";
	IF B5%(WK, M, 0) = 0 OR B5%(WK, M, 1) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 735
	IF B5%(WK, M, 0) = B5%(WK, M, 1) THEN LOCATE , 14: PRINT "1.000"; : GOTO 735
	LOCATE , 15: PRINT USING ".###"; B5%(WK, M, 1) / B5%(WK, M, 0);

735 IF B5%(WK, M, 0) + B5%(WK, M, 7) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 736
	IF B5%(WK, M, 1) + B5%(WK, M, 7) = B5%(WK, M, 0) + B5%(WK, M, 7) THEN LOCATE , 20: PRINT "1.000"; : GOTO 736
	LOCATE , 21: PRINT USING ".###"; (B5%(WK, M, 1) + B5%(WK, M, 7)) / (B5%(WK, M, 0) + B5%(WK, M, 7));

736 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B5%(WK, M, 14); B5%(WK, M, 0); B5%(WK, M, 2); B5%(WK, M, 1); B5%(WK, M, 4); B5%(WK, M, 5); B5%(WK, M, 6); B5%(WK, M, 3); B5%(WK, M, 7); B5%(WK, M, 8); B5%(WK, M, 9);  _
B5%(WK, M, 10); B5%(WK, M, 12); B5%(WK, M, 13)
	LOCATE , 6: PRINT "GAME";
	IF B8%(WK, M, 0) = 0 OR B8%(WK, M, 1) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 737
	IF B8%(WK, M, 0) = B8%(WK, M, 1) THEN LOCATE , 14: PRINT "1.000"; : GOTO 737
	LOCATE , 15: PRINT USING ".###"; B8%(WK, M, 1) / B8%(WK, M, 0);

737 IF B8%(WK, M, 0) + B8%(WK, M, 7) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 738
	IF B8%(WK, M, 1) + B8%(WK, M, 7) = B8%(WK, M, 0) + B8%(WK, M, 7) THEN LOCATE , 20: PRINT "1.000"; : GOTO 738
	LOCATE , 21: PRINT USING ".###"; (B8%(WK, M, 1) + B8%(WK, M, 7)) / (B8%(WK, M, 0) + B8%(WK, M, 7));

738 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B8%(WK, M, 14); B8%(WK, M, 0); B8%(WK, M, 2); B8%(WK, M, 1); B8%(WK, M, 4); B8%(WK, M, 5); B8%(WK, M, 6); B8%(WK, M, 3); B8%(WK, M, 7); B8%(WK, M, 8); B8%(WK, M, 9);  _
B8%(WK, M, 10); B8%(WK, M, 12); B8%(WK, M, 13)
END SUB


'------------------------------
'   POPUPPIT Subroutine
'------------------------------
' ...explanation...
SUB POPUPPIT (WK, M)
	LOCATE 14, 3: : COLOR l%(WK, 11), l%(WK, 12): PRINT " "; CHR$(201); STRING$(71, 205); CHR$(187); " "
	FOR X = 1 TO 7: LOCATE , 3: PRINT " "; CHR$(186); STRING$(71, 32); CHR$(186); " ": NEXT
	LOCATE , 3: PRINT " "; CHR$(200); STRING$(71, 205); CHR$(188); " "
	IF TS%(WK,0)=999 THEN LOCATE 15,56:PRINT "VS LHB  .000";
	IF TS%(WK,0)=999 AND P%(WK,M,51)<>0 THEN LOCATE 15,64:PRINT USING ".###";P%(WK,M,52)/P%(WK,M,51)
	IF TS%(WK,0)=999 THEN LOCATE 16,56:PRINT "VS RHB  .000";
	IF TS%(WK,0)=999 AND P%(WK,M,58)<>0 THEN LOCATE 16,64:PRINT USING ".###";P%(WK,M,59)/P%(WK,M,58)
	LOCATE 15, 6: PRINT "Stats for "; p$(WK, M)
	LOCATE , 6: PRINT "THROWS: "; B1$(p%(WK, M, 0) + 2); "    "; : PRINT USING "G/A%: ##     HOLD: #"; p%(WK, M, 12); p%(WK, M, 18)
	LOCATE , 3: PRINT " "; CHR$(204); STRING$(71, 205); CHR$(185); " "
	LOCATE , 6: PRINT "          W  L SV   ERA  G GS CG   IP    H  BB  SO   R  ER HR"
	LOCATE , 6: PRINT "SEASON   "; : PRINT USING "## ## ## ##.## ## ## ## ###   ### "; p%(WK, M, 1); p%(WK, M, 2); p%(WK, M, 3); p%(WK, M, 10) / 100; p%(WK, M, 4); p%(WK, M, 5); p%(WK, M, 16); p%(WK, M, 6); p%(WK, M, 7);
	PRINT USING "### ###         ##"; p%(WK, M, 8); p%(WK, M, 9); p%(WK, M, 13)
	LOCATE , 6: PRINT "LEAGUE   "; : PRINT USING "## ## ## "; P5%(WK, M, 6); P5%(WK, M, 7); P5%(WK, M, 12);
	IF P5%(WK, M, 3) = 0 OR P5%(WK, M, 0) = 0 THEN PRINT " 0.00"; : GOTO 805
	PRINT USING "##.##"; P5%(WK, M, 3) / (P5%(WK, M, 0) / 3) * 9;

805 PRINT USING " ## ## ## "; P5%(WK, M, 8); P5%(WK, M, 9); P5%(WK, M, 10);
	IF P5%(WK, M, 0) = 0 THEN PRINT "  0.0"; : GOTO 806
	PRINT USING "###"; INT(P5%(WK, M, 0) / 3);
	PRINT "."; : PRINT USING "#"; P5%(WK, M, 0) - (INT(P5%(WK, M, 0) / 3) * 3);

806 PRINT USING " ### ### ### ### ### ##"; P5%(WK, M, 1); P5%(WK, M, 4); P5%(WK, M, 5); P5%(WK, M, 2); P5%(WK, M, 3); P5%(WK, M, 14)
	LOCATE , 6: PRINT "GAME     "; : PRINT USING "## ## ## "; P8%(WK, M, 6); P8%(WK, M, 7); P8%(WK, M, 12);
	IF P8%(WK, M, 3) = 0 OR P8%(WK, M, 0) = 0 THEN PRINT " 0.00"; : GOTO 807
	PRINT USING "##.##"; P8%(WK, M, 3) / (P8%(WK, M, 0) / 3) * 9;

807 PRINT USING " ## ## ## "; P8%(WK, M, 8); P8%(WK, M, 9); P8%(WK, M, 10);
	IF P8%(WK, M, 0) = 0 THEN PRINT "  0.0"; : GOTO 808
	PRINT USING "###"; INT(P8%(WK, M, 0) / 3);
	PRINT "."; : PRINT USING "#"; P8%(WK, M, 0) - (INT(P8%(WK, M, 0) / 3) * 3);

808 PRINT USING " ### ### ### ### ### ##"; P8%(WK, M, 1); P8%(WK, M, 4); P8%(WK, M, 5); P8%(WK, M, 2); P8%(WK, M, 3); P8%(WK, M, 14)
END SUB


'------------------------------
'   POPUPSTATS Subroutine
'------------------------------
' ...explanation...
SUB POPUPSTATS ()
	J = B3%(p, B1!(p))
	LOCATE 3, 3: COLOR l%(p, 11), l%(p, 12): PRINT " "; CHR$(201); STRING$(71, 205); CHR$(187); " "
	FOR X = 1 TO 7: LOCATE , 3: PRINT " "; CHR$(186); STRING$(71, 32); CHR$(186); " ": NEXT
	LOCATE , 3: PRINT " "; CHR$(200); STRING$(71, 205); CHR$(188); " "
	LOCATE 4, 6: PRINT "Stats for ";
	IF B7%(p, B1!(p)) = 1 AND p%(p, P1%(p), 11) <> 999 THEN PRINT p$(p, P1%(p)): GOTO 510
	IF B7%(p, B1!(p)) = 1 THEN PRINT p$(p, P1%(p)): LOCATE , 6: PRINT "BATS: "; B1$(p%(p, P1%(p), 0) + 2); "    "; : PRINT USING "G/A%: ##       BUNT: #       RUN: #"; 52; p%(p, P1%(p), 32); p%(p, P1%(p), 31): GOTO 510
	PRINT b$(p, J): LOCATE , 6: PRINT "BATS: "; B1$(b%(p, J, 0) + 2); "    "; : PRINT USING "G/A%: ##       BUNT: #       RUN: #"; b%(p, J, 18); b%(p, J, 30); b%(p, J, 16)

510 IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 THEN LOCATE 4,56:PRINT "VS LHP  .000";
	IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 AND P%(P,P1%(P),37)<>0 THEN LOCATE 4,64:PRINT USING ".###";P%(P,P1%(P),38)/P%(P,P1%(P),37)
	IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 THEN LOCATE 5,56:PRINT "VS RHP  .000";
	IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 AND P%(P,P1%(P),44)<>0 THEN LOCATE 5,64:PRINT USING ".###";P%(P,P1%(P),45)/P%(P,P1%(P),44)
	LOCATE 6, 3: PRINT " "; CHR$(204); STRING$(71, 205); CHR$(185); " "
	LOCATE , 6: PRINT "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"
	IF B7%(p, B1!(p)) = 1 AND p%(p, P1%(p), 11) <> 999 THEN LOCATE , 6: PRINT "SEASON"; : LOCATE , 15: PRINT USING ".###"; p%(p, P1%(p), 11) / 1000: GOTO 520
	IF B7%(p, B1!(p)) = 1 THEN 517
	WK = p
	M = J
	CALL POPUPBAT(WK, M)
	GOTO 621

517 LOCATE , 6: PRINT "SEASON"; : LOCATE , 15
	IF p%(p, P1%(p), 20) = 0 THEN PRINT ".---  .---":GOTO 520
	PRINT USING ".###  .### ### ### ### ### ## "; p%(p, P1%(p), 22) / p%(p, P1%(p), 20); (p%(p, P1%(p), 22) + p%(p, P1%(p), 27)) / (p%(p, P1%(p), 27) + p%(p, P1%(p), 20)); p%(p, P1%(p), 19); p%(p, P1%(p), 20); p%(p, P1%(p), 21); p%(p, P1%(p), 22);  _
p%(p, P1%(p), 23);
	PRINT USING "## ## ### ### ### ### ##"; p%(p, P1%(p), 24); p%(p, P1%(p), 25); p%(p, P1%(p), 26); p%(p, P1%(p), 27); p%(p, P1%(p), 28); p%(p, P1%(p), 29); p%(p, P1%(p), 30)

520 LOCATE , 6: PRINT "LEAGUE"; : J = P1%(p)
	IF P5%(p, J, 23) = 0 OR P5%(p, J, 24) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 521
	IF P5%(p, J, 23) = P5%(p, J, 24) THEN LOCATE , 14: PRINT "1.000"; : GOTO 521
	LOCATE , 15: PRINT USING ".###"; P5%(p, J, 24) / P5%(p, J, 23);

521 IF P5%(p, J, 23) + P5%(p, J, 30) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 522
	IF P5%(p, J, 24) + P5%(p, J, 30) = P5%(p, J, 23) + P5%(p, J, 30) THEN LOCATE , 20: PRINT "1.000"; : GOTO 522
	LOCATE , 21: PRINT USING ".###"; (P5%(p, J, 24) + P5%(p, J, 30)) / (P5%(p, J, 23) + P5%(p, J, 30));

522 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P5%(p, J, 8); P5%(p, J, 23); P5%(p, J, 25); P5%(p, J, 24); P5%(p, J, 27); P5%(p, J, 28); P5%(p, J, 29); P5%(p, J, 26); P5%(p, J, 30); P5%(p, J, 31); P5%(p, J, 32);  _
P5%(p, J, 33); P5%(p, J, 35); P5%(p, J, 36)
	LOCATE , 6: PRINT "GAME";
	IF P8%(p, J, 23) = 0 OR P8%(p, J, 24) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 523
	IF P8%(p, J, 23) = P8%(p, J, 24) THEN LOCATE , 14: PRINT "1.000"; : GOTO 523
	LOCATE , 15: PRINT USING ".###"; P8%(p, J, 24) / P8%(p, J, 23);

523 IF P8%(p, J, 23) + P8%(p, J, 30) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 524
	IF P8%(p, J, 24) + P8%(p, J, 30) = P8%(p, J, 23) + P8%(p, J, 30) THEN LOCATE , 20: PRINT "1.000"; : GOTO 524
	LOCATE , 21: PRINT USING ".###"; (P8%(p, J, 24) + P8%(p, J, 30)) / (P8%(p, J, 23) + P8%(p, J, 30));

524 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P8%(p, J, 8); P8%(p, J, 23); P8%(p, J, 25); P8%(p, J, 24); P8%(p, J, 27); P8%(p, J, 28); P8%(p, J, 29); P8%(p, J, 26); P8%(p, J, 30); P8%(p, J, 31); P8%(p, J, 32);  _
P8%(p, J, 33); P8%(p, J, 35); P8%(p, J, 36)

621 WK = D
	M = P1%(D)
	CALL POPUPPIT(WK, M)

	LOCATE 24, 1: PRINT "Hit any key to continue";
650 I$ = GetKeyPress$
	COLOR 7, 0
END SUB


'------------------------------
'   PUTOUT Subroutine
'------------------------------
' ...explanation...
SUB PUTOUT (i, D, p)
	IF B7%(p, B0%(i - 1, 0)) = 1 THEN I5 = P1%(p): I6 = 33: CALL INCPITOFF(P, I5, I6)
	IF B7%(p, B0%(i - 1, 0)) <> 1 THEN I5 = B3%(p, B0%(i - 1, 0)): I6 = 10: CALL INCBATOFF(P, I5, I6)
END SUB


'------------------------------
'   RANDNUM Subroutine
'------------------------------
' ...explanation...
SUB RANDNUM (rn)
	rn = INT(RND(1) * 100) + 1
END SUB


'------------------------------
'   RecordOut Subroutine
'------------------------------
' ...explanation...
SUB RecordOut (i, I3)
	PRINT Q$(i); " out @ "; H$(I3)
	B0%(i, 0) = 0: B0%(i, 1) = 0: B0%(i, 2) = 0
	Q$(i) = ""
	CALL DELAY
END SUB


'------------------------------
'   RecordRun Subroutine
'------------------------------
' ...explanation...
SUB RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
	IF sah% = 0 THEN PRINT Q$(i); " scores"
	sah% = 0
	I5 = B0%(i, 1): I6 = 2: CALL ADDPIT (D, I5, I6)
	IF P1%(D) <> B0%(i, 1) THEN I5 = P1%(D): I6 = 18: CALL ADDPIT (D, I5, I6)
	IF P6%(p) = 1 THEN P8%(p, P1%(p), 20) = P8%(p, P1%(p), 20) + 1
	IF B7%(p, B0%(i, 0)) = 1 THEN I5 = P1%(p): I6 = 25: CALL INCPITOFF (P, I5, I6)
	IF B7%(p, B0%(i, 0)) <> 1 THEN I5 = B3%(p, B0%(i, 0)): I6 = 2: CALL INCBATOFF (P, I5, I6)
	IF F% = 1 AND O%(0) = 2 THEN PQ = 8: GOTO 4220
	IF F% = 1 AND W% <= 3 AND S2% <> 23 OR F% = 1 AND W% = 5 AND S2% <> 23 THEN PQ = 8: GOTO 4220
	IF F% = 1 AND W% = 4 AND INFPOS% = 2 THEN PQ = 8: GOTO 4220
	IF F% = 1 AND W% = 6 AND INFPOS% = 2 THEN PQ = 8: GOTO 4220
	IF S2% >= 4 AND S2% <= 6 THEN PQ = 8: GOTO 4220

4220 '
	IF PQ = 8 THEN 1825
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 26: CALL INCPITOFF(P, I5, I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 3: CALL INCBATOFF (P, I5, I6)
	IF F% = 1 THEN PQ = 8

1825 s%(p, INNING%) = s%(p, INNING%) + 1: S6%(p, 0) = S6%(p, 0) + 1
	IF S6%(p, 0) - S6%(D, 0) = 1 THEN P2%(D) = B0%(i, 1): P2%(p) = P1%(p): S1%(1) = -1: S1%(0) = -1: GOTO 1155
	IF S6%(p, 0) - S6%(D, 0) = 0 AND S1%(D) <> -1 THEN I5 = P1%(D): I6 = 19: CALL ADDPIT(D, I5, I6): S1%(D) = -1

1155 B0%(i, 0) = 0: B0%(i, 1) = 0: B0%(i, 2) = 0: Q$(i) = "": CALL DELAY
	IF s%(p, INNING%) = 4 AND INT(RND(1)*100)+1<=25 THEN PRINT "They're tearin' up the peapatch!": CALL DELAY
	IF INNING% >= 9 AND p = 1 AND S6%(p, 0) - S6%(D, 0) = 1 AND F% = 0 AND O%(1) < 3 AND S2% <> 10 THEN CALL CHARGERUN (D,i)
	IF INNING% >= 9 AND p = 1 AND S6%(p, 0) - S6%(D, 0) = 1 AND S2% <> 10 THEN A5%(0) = 0: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
END SUB


'------------------------------
'   OPTIONS Subroutine
'------------------------------
' ...explanation...
SUB OPTIONS ()
DIM u6$(3)
	U6$(0) = "HUMAN OPPONENT      "
	U6$(1) = "COMPUTER VISIT TEAM "
	U6$(2) = "COMPUTER HOME TEAM  "
	U6$(3) = "COMPUTER VS COMPUTER"

IF U6 = 2 THEN U6 = 3:GOTO 170
IF U6 = 1 AND U9 = 1 THEN U6 = 2:GOTO 170
IF U6 = 1 AND U9 = 0 THEN U6 = 1

170 CLS

171 COLOR , 0: LOCATE 3, 1
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT U6$(U6)
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT USING "DISPLAY INTERVAL: #.# "; displayPause
	COLOR 14: PRINT "<ENTER> ";:COLOR 15:PRINT "RETURN TO GAME PLAY"

183 COLOR 7, 0
	LOCATE 24, 1: PRINT "Hit any key to continue";
	I$=GetKeyPress$
	I = VAL(I$)
	IF I < 0 OR I > 2 THEN 183
	if i = 0 then 184
	IF I = 1 THEN U6 = U6 + 1
	IF U6 > 3 THEN U6 = 0
	IF I = 1 THEN 171
	IF I = 2 THEN LOCATE 4, 22: INPUT displayPause: GOTO 171

184 IF U6 = 0 THEN 185
IF U6 = 3 THEN U6 = 2:GOTO 185
IF U6 = 2 THEN U6 = 1:u9 = 1:GOTO 185
U6 = 1:u9 = 0

185 '
END SUB


'------------------------------
'   CONVNAME Subroutine
'------------------------------
' ...explanation...
SUB CONVNAME (QQ,I)
	N$(I) = A$(I)
	FOR xx = 4 TO 11
	IF MID$(N$(I), xx, 1) = " " OR MID$(N$(I), xx, 1) = "." THEN MID$(N$(I), xx, 1) = "_"
	NEXT xx
	QQ = 8
	FOR xx = 11 TO 4 STEP -1
	IF MID$(N$(I), xx, 1) = "_" THEN QQ = xx - 4: GOTO 118
	GOTO 119

118 NEXT xx

119 '
END SUB


'------------------------------
'   DISPBAVG Subroutine
'------------------------------
' ...explanation...
SUB DISPBAVG (D)
	IF INNING% > 5 AND S8%(D, 0) < P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) < P8%(D, P1%(D), 2) THEN LOCATE 10, 17: COLOR 30, 0: PRINT "-T-": COLOR 15, 0
	LOCATE 5, 1
	FOR X = 1 TO 9: COLOR 3, 0: LOCATE X + 4, 22
	IF B1!(p) = X THEN COLOR 11, 0
	LOCATE , 22: IF B7%(p, X) <> 1 THEN 358
	PRINT B1$(p%(p, P1%(p), 0) + 2); TAB(25); LEFT$(p$(p, P1%(p)), 10); " p"; TAB(39);
	IF p%(p, P1%(p), 11) <> 999 THEN PRINT USING " .###"; p%(p, P1%(p), 11) / 1000: GOTO 359
	IF p%(p, P1%(p), 20) = 0 THEN PRINT " .000": GOTO 359
	IF p%(p, P1%(p), 20) = p%(p, P1%(p), 22) THEN PRINT "1.000": GOTO 359
	PRINT USING " .###"; p%(p, P1%(p), 22) / p%(p, P1%(p), 20)
	GOTO 359

358 PRINT B1$(b%(p, B3%(p, X), 0) + 2); TAB(25); LEFT$(b$(p, B3%(p, X)), 10); " "; C$(B7%(p, X)); TAB(39);
	IF b%(p, B3%(p, X), 4) = 0 THEN PRINT " .000": GOTO 359
	IF b%(p, B3%(p, X), 6) = b%(p, B3%(p, X), 4) THEN PRINT "1.000": GOTO 359
	PRINT USING " .###"; b%(p, B3%(p, X), 6) / b%(p, B3%(p, X), 4)

359 NEXT: COLOR 15, 0
END SUB


'------------------------------
'   GETSTATS Subroutine
'------------------------------
' ...explanation...
SUB GETSTATS ()
	I = 0

100 COLOR 15, 0: CLS : CALL CONVNAME (QQ,I): F$ = MID$(N$(I), 4, QQ): R$ = F$ + "." + LEFT$(N$(I), 2)
	
102 FILENAME$ = D$(1) + R$
	IF NOT _FILEEXISTS(FILENAME$) THEN 110
OPEN D$(1) + R$ FOR INPUT AS #1
	INPUT #1, A$: INPUT #1, W2%: INPUT #1, L2%
	FOR I1 = 0 TO 22: INPUT #1, K$: INPUT #1, K%: NEXT I1
	FOR I1 = 0 TO 21: INPUT #1, K$: INPUT #1, K%: NEXT I1
	FOR I1 = 0 TO 22: FOR I2 = 0 TO 21: INPUT #1, B5%(I, I1, I2): NEXT: NEXT
	FOR I1 = 0 TO 21: FOR I2 = 0 TO 41: INPUT #1, P5%(I, I1, I2): NEXT: NEXT

110 CLOSE #1
	IF I = 0 THEN I = 1: GOTO 100
END SUB


'------------------------------
'   HITTINGLINES Subroutine
'------------------------------
' ...explanation...
SUB HITTINGLINES (l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	RANDOMIZE TIMER
	CLS : PRINT "Please Wait..."
	PRINT
	PRINT "Calculating Pitcher/Batter Park Effects..."
	FOR I = 0 TO 1
	I1 = 1 - I

'CALCULATE BASE ON BALLS PERCENTAGES
	 FOR I3 = VV(I) TO VV1(I)
	I6! = l%(I1, 3) / 1000
	IF p%(I1, P1%(I1), 6) * 3 * .94 + p%(I1, P1%(I1), 7) + p%(I1, P1%(I1), 8)=0 THEN I2!=0:GOTO 221
	I2! = p%(I1, P1%(I1), 8) / (p%(I1, P1%(I1), 6) * 3 * .94 + p%(I1, P1%(I1), 7) + p%(I1, P1%(I1), 8))
	'I2! = p%(I1, P1%(I1), 8) / (((p%(I1, P1%(I1), 6) * 3) * .94) + p%(I1, P1%(I1), 7) + p%(I1, P1%(I1), 8))

221 GOSUB 500
	 IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = SA%(I1,TY%*6+ 3) / 1000:I2! = P%(I1, P1%(I1),56+PY%*7)*.9/ (P%(I1, P1%(I1),51+PY%*7) + P%(I1, P1%(I1),56+PY%*7)*.9)
	 IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN I7! = .037: GOTO 223
	IF B7%(I, I3) = 1 AND (p%(I, P1%(I), 27) + p%(I, P1%(I), 20) = 0) THEN I7! = 0: GOTO 223
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 AND P%(I, P1%(I),37+BY%*7) + P%(I, P1%(I),42+BY%*7) = 0 THEN I7! = 0: GOTO 223
	IF B7%(I, I3) = 1 AND TS%(I,0)<>999 THEN I7! = p%(I, P1%(I), 27) / (p%(I, P1%(I), 27) + p%(I, P1%(I), 20)): GOTO 223
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 THEN I7! = P%(I, P1%(I),42+BY%*7)*.9 / (P%(I, P1%(I),42+BY%*7)*.9 + P%(I, P1%(I),37+BY%*7)): GOTO 223
	 IF b%(I, B3%(I, I3), 4) + b%(I, B3%(I, I3), 11) = 0 THEN I7! = 0: GOTO 223
	IF TS%(I,0)=999 AND b%(I, B3%(I, I3), 47+BY%*7) + b%(I, B3%(I, I3), 42+BY%*7) = 0 THEN I7! = 0: GOTO 223
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I7! = b%(I, B3%(I, I3), 47+BY%*7)*.9 / (b%(I, B3%(I, I3), 42+BY%*7) + b%(I, B3%(I, I3), 47+BY%*7)*.9):GOTO 223
	I7! = b%(I, B3%(I, I3), 11) / (b%(I, B3%(I, I3), 4) + b%(I, B3%(I, I3), 11))

223 CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 5, I3) = K8!
	NEXT I3

' CALCULATE STRIKEOUT PERCENTAGES
	FOR I3 = VV(I) TO VV1(I)
	IF p%(I1, P1%(I1), 6) = 0 THEN I2!=0:GOTO 222
	I2! = p%(I1, P1%(I1), 9) / ((p%(I1, P1%(I1), 6) * 3) * .94)

222 I6! = l%(I1, 2) / 1000
	GOSUB 500
	IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 AND P%(I1, P1%(I1),51+PY%*7) - P%(I1, P1%(I1),52+PY%*7) = 0 THEN I2!=0:GOTO 224
	IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 THEN I2! = P%(I1, P1%(I1),57+PY%*7) / (P%(I1, P1%(I1),51+PY%*7) - P%(I1, P1%(I1),52+PY%*7))

224 IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = SA%(I1,TY%*6+ 2) / 1000
	 IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN I7! = .42: GOTO 282
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 20) - p%(I, P1%(I), 22) = 0 THEN I7! = 0: GOTO 282
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 AND P%(I, P1%(I),37+BY%*7) - P%(I, P1%(I),38+BY%*7)=0 THEN I7! = 0: GOTO 282
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 THEN I7! = P%(I, P1%(I),43+BY%*7) / (P%(I, P1%(I),37+BY%*7) - P%(I, P1%(I),38+BY%*7)): GOTO 282
	IF B7%(I, I3) = 1 THEN I7! = p%(I, P1%(I), 28) / (p%(I, P1%(I), 20) - p%(I, P1%(I), 22)): GOTO 282
	 IF b%(I, B3%(I, I3), 4) = 0 OR b%(I, B3%(I, I3), 4) - b%(I, B3%(I, I3), 6) = 0 THEN I7! = 0: GOTO 282
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 AND b%(I, B3%(I, I3), 42+BY%*7) - b%(I, B3%(I, I3), 43+BY%*7) = 0 THEN I7! = 0: GOTO 282
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I7! = b%(I, B3%(I, I3), 48+BY%*7) / (b%(I, B3%(I, I3), 42+BY%*7) - b%(I, B3%(I, I3), 43+BY%*7)):GOTO 282
	I7! = b%(I, B3%(I, I3), 12) / (b%(I, B3%(I, I3), 4) - b%(I, B3%(I, I3), 6))

282 'IF I7!>=1 THEN I7%=.8
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 4, I3) = K8!
	NEXT I3

' CALCULATE THE BATTING AVERAGE
	 FOR I3 = VV(I) TO VV1(I)
	I2! = l%(I1, 1) / 1000
	I6! = p%(I1, P1%(I1), 14) / 1000
	IF p%(I1, P1%(I1), 6) + p%(I1, P1%(I1), 7)=0 THEN I7!=0:GOTO 283
	I7! = p%(I1, P1%(I1), 7) / (((p%(I1, P1%(I1), 6) * 3) * .94) + p%(I1, P1%(I1), 7))

283 CALL NORMALIZE (I7!, I2!, I6!, K8!)
	i9! = K8! / 1000
	GOSUB 500
	 IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = P%(I1, P1%(I1), TY%*6 + 65) / 1000
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 AND P%(I1, P1%(I1),51+PY%*7)=0 THEN I7! = 0:GOTO 225
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I7! = P%(I1, P1%(I1),52+PY%*7) / P%(I1, P1%(I1),51+PY%*7)

225 IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I2! = SA%(I1,TY%*6+ 1) / 1000
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN CALL NORMALIZE (I7!, I2!, I6!, K8!)
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN i9! = K8! / 1000
	 IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN I7! = p%(I, P1%(I), 11) / 1000: I6! = p%(I, P1%(I), 14) / 1000: GOTO 228
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 20) = 0 THEN I7! = 0:GOTO 227
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 AND P%(I, P1%(I),37+BY%*7) = 0 THEN I7! = 0:GOTO 227
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN I7! = P%(I, P1%(I),38+BY%*7) / P%(I, P1%(I),37+BY%*7):GOTO 227
	IF B7%(I, I3) = 1 THEN I7! = p%(I, P1%(I), 22) / p%(I, P1%(I), 20)

227 IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN I6! = P%(I, P1%(I), TY%*6 + 65) / 1000:GOTO 228
	IF B7%(I, I3) = 1 THEN I6! = p%(I, P1%(I), 14) / 1000: GOTO 228
	 IF b%(I, B3%(I, I3), 4) = 0 THEN I7! = 0: GOTO 230
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80)<>0 AND b%(I, B3%(I, I3), 42+BY%*7) = 0 THEN I7! = 0:GOTO 230
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I7! = b%(I, B3%(I, I3), 43+BY%*7) / b%(I, B3%(I, I3), 42+BY%*7):GOTO 230
	I7! = b%(I, B3%(I, I3), 6) / b%(I, B3%(I, I3), 4)

230 I6! = b%(I, B3%(I, I3), 20) / 1000
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I6! = b%(I, B3%(I, I3), 56+TY%*6) / 1000

228 I2! = l%(I, 1) / 1000
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I2! = SA%(I,TY%*6+ 1) / 1000
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	I7! = K8! / 1000
	I6! = l%(I, 1) / 1000
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I6! = SA%(I,TY%*6+ 1) / 1000
	I2! = i9!
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 0, I3) = K8!
'------------------------------
'    PRINT B$(I, B3%(I, I3)),B4%(I, 0, I3)
	NEXT I3
'222 I$=GetKeyPress$
' CALCULATE THE EXTRA BASE HITS RATIOS
	 FOR I3 = VV(I) TO VV1(I)
	GOSUB 500
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN B4%(I, 1, I3) = (130 * (i9! * 1000)) / l%(I1, 1): B4%(I, 2, I3) = (19 * (i9! * 1000)) / l%(I1, 1): B4%(I, 3, I3) = 37: GOTO 235
	E2! = (i9! * 1000) / l%(I1, 1)
	IF TS%(I1,0) =999 THEN E2! = (I9! * 1000) / SA%(I1,TY%*6+ 1)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 22) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	IF B7%(I, I3) <> 1 AND b%(I, B3%(I, I3), 6) =0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 AND P%(I, P1%(I),38+BY%*7) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	 IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN B4%(I, 1, I3) = INT((P%(I, P1%(I),39+BY%*7) / P%(I, P1%(I),38+BY%*7)) * 1000) * E2!:' doubles times  * .95
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN B4%(I, 2, I3) = INT((P%(I, P1%(I),40+BY%*7) / P%(I, P1%(I),38+BY%*7)) * 1000) * E2!
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN B4%(I, 3, I3) = INT((P%(I, P1%(I),41+BY%*7) / P%(I, P1%(I),38+BY%*7)) * 1000): GOTO 235
	 IF B7%(I, I3) = 1 THEN B4%(I, 1, I3) = INT((p%(I, P1%(I), 23) / p%(I, P1%(I), 22)) * 1000) * E2!:' doubles times  * .95
	IF B7%(I, I3) = 1 THEN B4%(I, 2, I3) = INT((p%(I, P1%(I), 24) / p%(I, P1%(I), 22)) * 1000) * E2!
	IF B7%(I, I3) = 1 THEN B4%(I, 3, I3) = INT((p%(I, P1%(I), 25) / p%(I, P1%(I), 22)) * 1000): GOTO 235
	 IF b%(I, B3%(I, I3), 6) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 AND b%(I, B3%(I, I3), 43+BY%*7) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	 IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN B4%(I, 1, I3) = INT((b%(I, B3%(I, I3), 44+BY%*7) / b%(I, B3%(I, I3), 43+BY%*7)) * 1000) * E2!:'see note on pitchers * .95
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN B4%(I, 2, I3) = INT((b%(I, B3%(I, I3), 45+BY%*7) / b%(I, B3%(I, I3), 43+BY%*7)) * 1000) * E2!
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN B4%(I, 3, I3) = INT((b%(I, B3%(I, I3), 46+BY%*7) / b%(I, B3%(I, I3), 43+BY%*7)) * 1000):GOTO 235
	 B4%(I, 1, I3) = INT((b%(I, B3%(I, I3), 7) / b%(I, B3%(I, I3), 6)) * 1000) * E2!:'see note on pitchers * .95
	B4%(I, 2, I3) = INT((b%(I, B3%(I, I3), 8) / b%(I, B3%(I, I3), 6)) * 1000) * E2!
	B4%(I, 3, I3) = INT((b%(I, B3%(I, I3), 9) / b%(I, B3%(I, I3), 6)) * 1000)

235 I6! = l%(I1, 4) / 1000
	I7! = B4%(I, 3, I3) / 1000
	I2! = 0
	IF p%(I1, P1%(I1), 7)<>0 THEN I2! = p%(I1, P1%(I1), 13) / p%(I1, P1%(I1), 7)
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = SA%(I1,TY%*6+ 4) / 1000
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 AND P%(I1, P1%(I1),52+PY%*7)<>0 THEN I2! = P%(I1, P1%(I1),55+PY%*7) / P%(I1, P1%(I1),52+PY%*7)
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 3, I3) = K8!
	NEXT I3
' ADJUST LEFTY/RIGHTY THE OLD WAY
	 I4 = p%(I1, P1%(I1), 0)
	FOR I3 = VV(I) TO VV1(I)
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 THEN 236
	I2 = b%(I, B3%(I, I3), 0)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND p%(I1, P1%(I1), 0) = 1 THEN A = 1.04: A1 = .88: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND p%(I1, P1%(I1), 0) = -1 THEN A = .93: A1 = 1.16: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND p%(I1, P1%(I1), 0) = -1 THEN A = 1.02: A1 = .98: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND p%(I1, P1%(I1), 0) = 1 THEN A = .97: A1 = 1.1: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -2 AND I4 = 1 THEN A = 1.04: A1 = .96: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -2 AND I4 = -1 THEN A = .78: A1 = 1.22: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 2 AND I4 = 1 THEN A = .78: A1 = 1.22: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 2 AND I4 = -1 THEN A = 1.04: A1 = .96: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -1 AND I4 = -1 THEN A = .93: A1 = 1.16: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -1 AND I4 = 1 THEN A = 1.04: A1 = .88: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 1 AND I4 = 1 THEN A = .97: A1 = 1.1: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 1 AND I4 = -1 THEN A = 1.02: A1 = .98: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 0 AND I4 = -1 THEN A = 1.01: A1 = .86: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 0 AND I4 = 1 THEN A = .99: A1 = .93: CALL LRADJ (A!, A1!)

236 B4%(I, 6, I3) = B4%(I, 3, I3): B4%(I, 7, I3) = B4%(I, 3, I3)
	NEXT I3

' ADJUST AVERAGES FOR THE HOME TEAM
	 FOR I3 = VV(I) TO VV1(I)
	IF I = 1 THEN B4%(I, 0, I3) = B4%(I, 0, I3) + 5
	IF I <> 1 THEN B4%(I, 0, I3) = B4%(I, 0, I3) - 5
	NEXT I3

' CALCULATE THE HOME PARK FACTORS
	 IF N = 2 THEN 273
	FOR I2 = VV(I) TO VV1(I)
	IF I = 1 THEN B4%(I, 1, I2) = INT(B4%(I, 1, I2) * (l%(I, 6) / 100) + .5): B4%(I, 2, I2) = INT(B4%(I, 2, I2) * (l%(I, 7) / 100) + .5)
	IF I <> 1 THEN B4%(I, 1, I2) = INT(B4%(I, 1, I2) * (l%(I1, 6) / 100) * ((200 - l%(I, 6)) / 100) + .5): B4%(I, 2, I2) = INT(B4%(I, 2, I2) * (l%(I1, 7) / 100) * ((200 - l%(I, 7)) / 100) + .5)
	IF K9(I) > 1000 THEN I3! = 100: I6! = INT(K9(I) / 1000) * WF!: I7! = (K9(I) - INT((K9(I) / 1000)) * 1000) * WF!
	IF K9(I1) > 1000 THEN I4! = 100: K8! = INT(K9(I1) / 1000) * WF!: i9! = (K9(I1) - INT((K9(I1) / 1000)) * 1000) * WF!
	IF K9(I) < 1000 THEN I3! = K9(I) * WF!: I6! = I3!: I7! = I3!
	IF K9(I1) < 1000 THEN I4! = K9(I1) * WF!: K8! = I4!: i9! = I4!
	IF I = 1 THEN B4%(I, 3, I2) = INT((B4%(I, 3, I2) * (I3! / 100)) + .5): B4%(I, 6, I2) = INT((B4%(I, 6, I2) * (I6! / 100)) + .5): B4%(I, 7, I2) = INT((B4%(I, 7, I2) * (I7! / 100)) + .5)
	IF I<>1 THEN B4%(I, 3, I2)=INT((B4%(I, 3, I2)*(I4! / 100) * ((200 - I3!) / 100)) + .5): B4%(I, 6, I2) = INT((B4%(I, 6, I2) * (K8! / 100) * ((200 - I6!) / 100)) + .5): B4%(I, 7, I2) = INT((B4%(I, 7, I2) * (i9! / 100) * ((200-I7!)/100))+ .5)
	NEXT I2
	NEXT I
	GOTO 273

500 II = b%(I, B3%(I, I3), 0)
	I4 = p%(I1, P1%(I1), 0)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND I4 = 1 THEN TY%=1
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND I4 = -1 THEN TY%=0
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND I4 = -1 THEN TY%=2
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND I4 = 1 THEN TY%=3
	IF B7%(I, I3) <>1 AND II<0 AND I4=1 THEN TY%=1
	IF B7%(I, I3) <>1 AND II<0 AND I4=-1 THEN TY%=0
	IF B7%(I, I3) <>1 AND II>0 AND I4=-1 THEN TY%=2
	IF B7%(I, I3) <>1 AND II>0 AND I4=1 THEN TY%=3
	IF B7%(I, I3) <>1 AND II=0 AND I4=1 THEN TY%=1
	IF B7%(I, I3) <>1 AND II=0 AND I4=-1 THEN TY%=2
	IF TY%=0 OR TY%=1 THEN PY%=0
	IF TY%=2 OR TY%=3 THEN PY%=1
	IF TY%=0 OR TY%=2 THEN BY%=0
	IF TY%=1 OR TY%=3 THEN BY%=1
	RETURN

273 '
END SUB


'------------------------------
'   LRADJ Subroutine
'------------------------------
' ...explanation...
SUB LRADJ (A!, A1!)
	FOR I6 = 0 TO 3
	B4%(I, I6, I3) = B4%(I, I6, I3) * A!
	NEXT I6
	B4%(I, 5, I3) = B4%(I, 5, I3) * A!: ' NEWLY ADDED
	B4%(I, 4, I3) = B4%(I, 4, I3) * A1!
END SUB


'------------------------------
'   NORMALIZE Subroutine
'------------------------------
' ...explanation...
SUB NORMALIZE (I7!, I2!, I6!, K8!)
	 I0! = (I7! * I2! * (1 - I6!))
	IF (I0! + ((1 - I7!) * (1 - I2!) * I6!))=0 THEN K8!=0:GOTO 15
	K8! = I0! / (I0! + ((1 - I7!) * (1 - I2!) * I6!))

15  K8! = INT((K8! * 1000) + .0005)

END SUB


'------------------------------
'   SETUP Subroutine
'------------------------------
' ...explanation...
SUB SETUP ()
		FOR I = 3 TO 6: READ F$(I): NEXT
	H0$(0) = " ": H0$(1) = "H"
	TM$(1) = "HOT": TM$(2) = "WARM": TM$(3) = "COOL": TM$(4) = "COLD"
	SC$(1) = "CLEAR": SC$(2) = "PARTLY CLOUDY": SC$(3) = "OVERCAST": SC$(4) = "DOME"
	PC$(1) = "THUNDERSTORMS": PC$(2) = "SHOWERS": PC$(3) = "DRIZZLE": PC$(4) = "NONE"

10   CALL GETSTATS
	CALL DISPBAVG(D)
	W5 = 0: W6 = 1
	FOR I = 0 TO 1: FOR I1 = 1 TO 9: FOR I2 = 1 TO 9: READ BT%(I, I1, I2): NEXT: NEXT: NEXT
	FOR I = 2 TO 9: FOR I1 = 0 TO 2: READ CF%(I, I1): NEXT: NEXT
	FOR I = 2 TO 9: FOR I1 = 0 TO 1: READ LB%(I, I1): NEXT: NEXT
	FOR I = 0 TO 5: FOR I1 = 0 TO 2: READ SO%(I, I1): NEXT: NEXT
	FOR I = 11 TO 0 STEP -1: FOR I1 = 0 TO 2: READ TP%(I, I1): NEXT: NEXT
END SUB


'------------------------------
'   BASEONBALLS Subroutine
'------------------------------
' ...explanation...
SUB BASEONBALLS (S2%,D2,D,P)
	S2% = 2
	CALL NEWLINES: bv = INT(RND(1) * 100) + 1: CALL RANDNUM (RN)
	IF D2 = 2 AND bv <= 50 THEN PRINT Q$(0); " is walked intentionally..."
	IF D2 = 2 AND bv > 50 THEN PRINT Q$(0); " is given a purpose pass..."
	IF D2 = 2 THEN S8%(D, 0) = S8%(D, 0) + 1: CALL DELAY: GOTO 3241
	IF RN <= 30 THEN PRINT "Full count...": CALL DELAY
	CALL DELIVERY (D)
	  Y% = INT(RND(1) * 80) + 780
	CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
	PRINT Y$
	CALL DELAY
		Y% = INT(RND(1) * 30) + 860
	CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
	PRINT Y$
	call delay

3241 call randnum(rn)
	if s4% = 7 and  rn<=30 then PRINT "that forces a run in":call delay
	rn=int(rnd(1)*4)+1
	if s4%>3 and s4%<7 and rn=1 then print "and the bases are loaded!":call delay
	if s4%>3 and s4%<7 and rn=2 then print "and the sacks are jammed!":call delay
	if s4%>3 and s4%<7 and rn=3 then print "and the bases are full!":call delay
	if s4%>3 and s4%<7 and rn=4 then print "and that loads them up!":call delay
	  I5 = P1%(D): I6 = 4: CALL ADDPIT (D,I5,I6)
	IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 30: CALL INCPITOFF (P,I5,I6): GOTO 3243
	I5 = B3%(P, B1!(P)): I6 = 7: CALL INCBATOFF (P,I5,I6)

3243 '
END SUB


'------------------------------
'   HITBYPITCH Subroutine
'------------------------------
' ...explanation...
SUB HITBYPITCH (S2%,D)
	S2% = 1
	CALL NEWLINES
	CALL DELIVERY (D)
	y% = INT(RND(1) * 20) + 720
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
	PRINT Y$
	CALL DELAY
	y% = INT(RND(1) * 11) + 740
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
	PRINT Y$
	CALL DELAY
	y% = INT(RND(1) * 20) + 760
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
	PRINT Y$
	CALL DELAY
	call randnum(rn)
	if rn<=50 and s4% = 7 then print "tough way to get an RBI":call delay
END SUB


'------------------------------
'   WILDPITCH Subroutine
'------------------------------
' ...explanation...
SUB WILDPITCH (S2%,D)
	S2% = 6
	CALL RANDNUM (RN)
	IF RN <= 16 THEN PRINT "and it gets away from "; B$(D, B9%(D, 2))
	IF RN > 16 AND RN <= 32 THEN PRINT "pitch is WAY high, and "; B$(D, B9%(D, 2)); " can't get it"
	IF RN > 32 AND RN <= 48 THEN PRINT "It's in the dirt": CALL DELAY: PRINT "The runners move up 90 ft."
	IF RN > 48 AND RN <= 64 THEN PRINT "It bounces in front of the plate": CALL DELAY: PRINT "it's past the catcher": CALL DELAY: PRINT "the runners advance"
	IF RN > 64 AND RN <= 80 THEN PRINT "it's over "; B$(D, B9%(D, 2)); "'s head"
	IF RN > 67 THEN PRINT "wild pitch by "; P$(D, P1%(D)): CALL DELAY: GOTO 3235
	CALL RANDNUM (RN)
	CALL DELAY
	IF RN <= 33 THEN PRINT "It's ruled a wild pitch"
	IF RN > 33 AND RN <= 66 THEN PRINT "score it a wild pitch"
	IF RN > 66 THEN PRINT "credit "; P$(D, P1%(D)); " with a wild pitch"
	CALL DELAY

3235 '
END SUB


'------------------------------
'   RELIEVERS Subroutine
'------------------------------
' ...explanation...
SUB RELIEVERS
	CALL PITSTRAT
	if pz=1 then pz=0:goto 8885
	I2 = 0
	A1 = S6%(D, 0) - S6%(p, 0)
	FOR I = 0 TO 21
	A2 = p%(D, i, 4) - p%(D, i, 5) - p%(d, i, 3)*1.3
	IF p%(D, i, 15) > 0 OR P$(D, i) = "XXX" OR A2=0 THEN 8874
	IF INNING% <= 7 AND A2 > 0 THEN I2 = I2 + A2
	IF INNING% > 7 AND A1 < 0 AND A2 > 0 THEN I2 = I2 + A2
	IF INNING% > 7 AND A1 >= 0 THEN I2 = I2 + p%(D, i, 3)

8874 NEXT i
	I3 = INT(RND(1) * I2) + 1
	I2 = 0
	FOR i = 0 TO 21
	A2 = p%(D, i, 4) - p%(D, i, 5) - p%(d, i, 3)*1.3
	IF p%(D, i, 15) > 0 OR p$(D, i) = "XXX" OR A2 = 0 THEN 8883
	IF INNING% <= 7 AND A2 > 0 AND I3 <= I2 + A2 THEN CALL NEWPITCHER: GOTO 8885
	IF INNING% <= 7 AND A2 > 0 THEN I2 = I2 + A2: GOTO 8883
	  IF INNING% > 7 AND A1 < 0 AND A2 > 0 AND I3 <= I2 + A2 THEN CALL NEWPITCHER: GOTO 8885
	IF INNING% > 7 AND A1 < 0 AND A2 > 0 THEN I2 = I2 + A2: GOTO 8883
	IF INNING% > 7 AND A1 >= 0 AND I3 > I2 AND I3 <= I2 + p%(D, i, 3) THEN CALL NEWPITCHER: GOTO 8885
	IF INNING% > 7 AND A1 >= 0 THEN I2 = I2 + p%(D, i, 3)

8883 NEXT i
	FOR i = 0 TO 21
	IF p%(D, i, 15) = 0 AND p$(D, i) <> "XXX" THEN CALL NEWPITCHER: GOTO 8885
	NEXT i

8885 '
END SUB


'------------------------------
'   REPLACEMENTS Subroutine
'------------------------------
' ...explanation...
SUB REPLACEMENTS (I7%, I8%, i9%)
	' REPLACEMENT AVERAGE CALCS, I7%,I8%,I9%
	FOR xx = 22 TO 25
	IF B7%(I7%, I8%) = b%(I7%, i9%, xx) THEN b%(I7%, i9%, 15) = b%(I7%, i9%, 33 + (xx - 22) * 2): b%(I7%, i9%, 17) = b%(I7%, i9%, 34 + (xx - 22) * 2): b%(I7%, i9%, 19) = b%(I7%, i9%, xx + 4): GOTO 7110
	NEXT
	J1 = B7%(I7%, I8%)
	IF J1 = 2 THEN J2 = 1: J3 = 0: J4 = 910 + INT(RND(1) * 10) + 1
	IF J1 = 3 THEN J2 = 0: J3 = 3: J4 = 960 + INT(RND(1) * 10) + 1
	IF J1 = 4 THEN J2 = 0: J3 = 3: J4 = 912 + INT(RND(1) * 10) + 1
	IF J1 = 5 OR J1 = 6 THEN J2 = 0: J3 = 3: J4 = 902 + INT(RND(1) * 10) + 1
	IF J1 >= 7 THEN J2 = 2: J3 = 2: J4 = 972 + INT(RND(1) * 10) + 1
	b%(I7%, i9%, 15) = J2
	b%(I7%, i9%, 17) = J3
	b%(I7%, i9%, 19) = J4

7110 '
END SUB


'------------------------------
'   SCOREBOARD Subroutine
'------------------------------
' ...explanation...
SUB SCOREBOARD ()
	'***INCREMENT THE HIT TOTAL ON THE SCOREBOARD***
	COLOR 14, 0: S6%(p, 1) = S6%(p, 1) + 1
	LOCATE p + 2, 43: PRINT USING "##"; S6%(p, 1);
	COLOR 15, 0
END SUB


'------------------------------
'   SECOND2FIRST Subroutine
'------------------------------
' ...explanation...
SUB SECOND2FIRST (D)
	'*** throw to 1st from 2nd ***
	Y% = INT(RND(1) * 30) + 1540
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$; " to first"
	CALL DELAY
END SUB


'------------------------------
'   SINGLEPBP Subroutine
'------------------------------
' ...explanation...
SUB SINGLEPBP (S2%, D, W%)
	'*** play by play for a single s2%=7, s2%=12 ***
	if s2% = -12 then s2% = 12:goto 8014
	CALL DELIVERY(D)
	CALL RANDNUM(rn)
	IF S2% = 12 THEN PRINT Q$(0); " beats out an infield single": GOTO 8013
	IF rn <= 50 THEN Y% = INT(RND(1) * 50) + 1610: CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D): PRINT Y$; " "; C$(W%): GOTO 8013
	IF W% = 7 THEN Y% = INT(RND(1) * 70) + 1660
	IF W% = 9 THEN Y% = INT(RND(1) * 70) + 1730
	IF W% = 8 THEN Y% = INT(RND(1) * 90) + 1800
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$

8013 CALL DELAY:CALL DELAY

8014 END SUB


'------------------------------
'   SINGLEROUTINE Subroutine
'------------------------------
' ...explanation...
SUB SINGLEROUTINE (W%, p, I1, D, S2%, P2)
	CALL GETOUTFIELDER (W%, p, I1, D)
	CALL NEWLINES
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF(P, I5, I6): I6 = 1: CALL INCBATOFF(P, I5, I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT(D, I5, I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF(P, I5, I6): I6 = 24: CALL INCPITOFF(P, I5, I6)
	if h7% > sn then CALL SINGLEPBP(S2%, D, W%)
	FOR I = 0 TO 3
	A5%(I) = 1
	NEXT I
	IF S2% = 12 OR B0%(2, 0) = 0 THEN 3075
	H6% = INT(RND(1) * 100) + 1
	IF B7%(p, B0%(2, 0)) <> 1 THEN BR% = b%(p, B3%(p, B0%(2, 0)), 16)
	IF B7%(p, B0%(2, 0)) = 1 THEN BR% = p%(p, P1%(p), 31)
	IF O%(0) = 0 AND W% = 7 THEN I3= 16 + BR% * 3
	IF O%(0) = 1 AND W% = 7 THEN I3= 36 + BR% * 3
	IF O%(0) = 2 AND W% = 7 THEN I3= 79 + BR% * 3
	IF O%(0) = 0 AND W% = 8 THEN I3= 49 + BR% * 3
	IF O%(0) = 1 AND W% = 8 THEN I3= 61 + BR% * 3
	IF O%(0) = 2 AND W% = 8 THEN I3= 82 + BR% * 3
	IF O%(0) = 0 AND W% = 9 THEN I3= 27 + BR% * 3
	IF O%(0) = 1 AND W% = 9 THEN I3= 44 + BR% * 3
	IF O%(0) = 2 AND W% = 9 THEN I3= 79 + BR% * 3
	IF H6% <= I3 OR P2 = 2 AND S2% <> 12 THEN A5%(2) = 2

3075 IF B0%(1, 0) = 0 OR S2% = 12 OR B0%(2, 0) > 0 AND A5%(2) = 1 THEN 3092
	H6% = INT(RND(1) * 100) + 1
	BR% = b%(p, B3%(p, B0%(1, 0)), 16)
	IF B7%(p, B0%(1, 0)) = 1 THEN BR% = p%(p, P1%(p), 31)
	I3 = 0
	IF O%(0) = 0 AND W% = 7 THEN I3=  6 + BR% * 3
	IF O%(0) = 1 AND W% = 7 THEN I3= 10 + BR% * 3
	IF O%(0) = 2 AND W% = 7 THEN I3= 11 + BR% * 3
	IF O%(0) = 0 AND W% = 8 THEN I3= 25 + BR% * 3
	IF O%(0) = 1 AND W% = 8 THEN I3= 26 + BR% * 3
	IF O%(0) = 2 AND W% = 8 THEN I3= 45 + BR% * 3
	IF O%(0) = 0 AND W% = 9 THEN I3= 38 + BR% * 3
	IF O%(0) = 1 AND W% = 9 THEN I3= 40 + BR% * 3
	IF O%(0) = 2 AND W% = 9 THEN I3= 62 + BR% * 3
	IF H6% <= I3 OR P2 = 2 AND S2% <> 12 THEN A5%(1) = 2

3092 CALL ADVANCEMENT(p, D, S2%, F%,I3)
END SUB


'------------------------------
'   SOMESTATS Subroutine
'------------------------------
' ...explanation...
SUB SOMESTATS (V, p, F%, D)
	IF B0%(V, 0) > 0 AND B7%(p, B0%(V, 0)) = 1 AND F% = 0 THEN I5 = P1%(p): I6 = 32: CALL INCPITOFF(P, I5, I6)
	IF B0%(V, 0) > 0 AND B7%(p, B0%(V, 0)) <> 1 AND F% = 0 THEN I5 = B3%(p, B0%(V, 0)): I6 = 9: CALL INCBATOFF(P, I5, I6)
END SUB


'------------------------------
'   SQUEEZECHOICE Subroutine
'------------------------------
' ...explanation...
SUB SQUEEZECHOICE (SH)
	'*** SQUEEZE ***
	IF u6 = 2 OR u6 = 1 AND p = u9 THEN SH = 2: GOTO 4242
	LOCATE 15, 1: PRINT "1 - SQUEEZE PLAY": PRINT "2 - BUNT FOR SACRIFICE/HIT"
4241 i$ = GetKeyPress$
	SH = VAL(i$): IF SH < 1 OR SH > 2 THEN 4241

4242 '
END SUB


'------------------------------
'   STEAL3RD Subroutine
'------------------------------
' ...explanation...
SUB STEAL3RD (TGTBASE%, AA, Q0, H0%, D)
	IF TGTBASE% = 3 AND AA >= 24 THEN Q0 = 830: ' *** SUCCESS OF 3B STEAL ***
	IF TGTBASE% = 3 AND AA < 24 THEN Q0 = 700
	IF TGTBASE% = 4 THEN Q0 = Q0 * .5
	' *** H0%=0, runner not held ***
	IF H0% = 0 THEN Q0 = Q0 + 50: GOTO 4874
	IF H0% = 1 THEN Q0 = Q0 - ((p%(D, P1%(D), 18) - 5) * 25):' - 50

4874 REM
END SUB


'------------------------------
'   STEALCHANCE Subroutine
'------------------------------
' ...explanation...
SUB STEALCHANCE (AA, I2, TGTBASE%,CHANCE%)
DIM DD AS SINGLE
DIM TT AS SINGLE
DIM RR AS SINGLE
DIM SS AS SINGLE
DIM GG AS SINGLE
DIM I1 AS SINGLE
	AA = b%(p, I2, 13)
	BB = b%(p, I2, 13) + b%(p, I2, 14)
	CC = b%(p, I2, 7): ' *** CHANCE TO STEAL 2B & 3B ***
	IF AA <= 43 THEN DD = .16: GOTO 4853
	IF AA <= 60 THEN DD = .21: GOTO 4853
	DD = .3

4853 TT = CC + BB - (BB * DD): ' *** TOTAL CHANCES TO STEAL 3RD ***
	RR = (TT * .19) * 1.4: ' *** "      "      "   "     "  1 OUT ***
	SS = (TT * .56) * 1.4: ' ***                            0 OUT ***
	GG = RR + SS:    ' ***                            (0,1 OUT) ***
	IF TGTBASE% = 3 THEN 4862
	FF = b%(p, I2, 6) + b%(p, I2, 11) - b%(p, I2, 9)
	IF FF=0 THEN I1=0:GOTO 4868
	IF AA >= 12 THEN I1 = INT(((BB - BB * DD) / FF) * 1500)
	IF AA < 12 THEN I1 = INT((BB / FF) * 1500)
	IF I1 < 0 THEN I1 = 0
	GOTO 4868

4862 IF BB < 10 AND O%(0) = 0 THEN I1 = 0
	IF BB < 10 AND O%(0) = 1 THEN I1 = 10
	IF O%(0) = 2 OR TT=0 THEN I1 = 0
	IF BB >= 10 AND O%(0) = 1 THEN I1 = BB * DD / TT * 1000
	IF BB >= 10 AND O%(0) = 0 THEN I1 = BB * DD / TT * 1000
	IF I1 < 0 THEN I1 = 0

4868 CHANCE%=I1
	'
END SUB


'------------------------------
'   STRIKEOUT Subroutine
'------------------------------
' ...explanation...
SUB STRIKEOUT (U%, D)
	'*** strikeout pbp ***
	IF U% = 3 THEN BV = 20: GOTO 3770
	BV = INT(RND(1) * 100): CALL RANDNUM(rn)
	IF rn <= 20 THEN PRINT "FULL COUNT...": CALL DELAY
	CALL DELIVERY(D)
	IF BV = 1 THEN PRINT "Strike Three!!": PRINT Q$(0); " turns to "; U$(1): CALL DELAY: PRINT "he says he tees them up higher than that": PRINT "at the country club!!": CALL DELAY: GOTO 3774
	IF BV = 2 THEN PRINT "Strike three...": CALL DELAY: PRINT Q$(0); " stands there like a house": PRINT "by the side of the road": CALL DELAY: CALL DELAY: GOTO 3774
	IF BV > 2 AND BV <= 5 THEN PRINT Q$(0); " looks at a called strike three": CALL DELAY: PRINT "and turns to glare at "; U$(1): CALL DELAY: PRINT M$(p); " is barking from the dugout!!": GOTO 3774
	  CALL RANDNUM(RN)
	IF RN<=25 THEN
		Y% = INT(RND(1) * 40) + 890
	ELSE
		Y% = INT(RND(1) * 80) + 930
	END IF
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$: CALL DELAY

3770 Y% = INT(RND(1) * 40) + 1010
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$

3774 CALL DELAY
	CALL RANDNUM(rn)
	IF rn <= 20 THEN PRINT "Number"; P8%(D, P1%(D), 5) + 1; "for "; p$(D, P1%(D))
	IF rn > 20 AND rn <= 40 THEN PRINT "That's"; P8%(D, P1%(D), 5) + 1; "for "; p$(D, P1%(D))
	IF RN = 41 THEN PRINT Q$(0)+" is not a happy man"
	IF RN = 42 THEN PRINT Q$(0)+" has had better AB's"
	CALL DELAY
END SUB


'------------------------------
'   STRIKEOUTCS Subroutine
'------------------------------
' ...explanation...
SUB STRIKEOUTCS (SO)
	IF SO = 7 THEN B1!(p) = B1!(p) + 1
END SUB


'------------------------------
'   STRIPRPBP Subroutine
'------------------------------
' ...explanation...
SUB STRIPRPBP (Y$, Y%, D)

3991 FOR BH% = 1 TO LEN(Y$)
	IF ASC(MID$(Y$, BH%, 1)) > 122 OR ASC(MID$(Y$, BH%, 1)) < 32 THEN CALL GETRPBP(Y%, Y$, D): GOTO 3991
	NEXT
END SUB


'------------------------------
'   TAGADVANCE Subroutine
'------------------------------
' ...explanation...
SUB TAGADVANCE (S2%, I8, D)
	IF S2% = 7 AND I8 = 2 AND B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 27: CALL INCPITOFF(P, I5, I6): GOTO 3394
	IF S2% = 7 AND I8 = 2 AND B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 4: CALL INCBATOFF(P, I5, I6): GOTO 3394
	IF S2% = 8 AND I8 = 3 AND B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 28: CALL INCPITOFF(P, I5, I6): P8%(p, P1%(p), 27) = P8%(p, P1%(p), 27) - 1: P5%(p, P1%(p), 27) = P5%(p, P1%(p), 27) - 1: GOTO 3394
	IF S2% = 8 AND I8 = 3 AND B7%(p, B1!(p)) <> 1 THEN I3 = 4: I4 = 5: I5 = B3%(p, B1!(p))
	IF S2% = 9 AND I8 = 4 THEN I3 = 5: I4 = 6: I5 = B3%(p, B1!(p))
	IF S2% = 8 AND I8 = 3 OR S2% = 9 AND I8 = 4 THEN 3393
	GOTO 3394

3393 IF P%(D,P1%(D),0) = 1 THEN 3392
	CSS%(P, I5, I3 - 2) = CSS%(P, I5, I3 - 2) - 1
	CSS%(P, I5, I4 - 2) = CSS%(P, I5, I4 - 2) + 1

3392 B8%(p, I5, I3) = B8%(p, I5, I3) - 1
	B5%(p, I5, I3) = B5%(p, I5, I3) - 1
	B8%(p, I5, I4) = B8%(p, I5, I4) + 1
	B5%(p, I5, I4) = B5%(p, I5, I4) + 1

3394 '
END SUB


'------------------------------
'   THROW2FIRST Subroutine
'------------------------------
' ...explanation...
SUB THROW2FIRST (D)
	'*** throw to 1st from 3rd/ss ***
	Y% = INT(RND(1) * 40) + 1570
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	PRINT Y$; " to first"
	CALL DELAY
END SUB


'------------------------------
'   TIRING Subroutine
'------------------------------
' ...explanation...
SUB TIRING (P9)
	'tired on runs and hits
	IF S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) AND S8%(P9, 1) <= P8%(P9, P1%(P9), 2) THEN 9756
	IF S8%(P9, 1) > 0 AND (S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) OR S8%(P9, 1) <= P8%(P9, P1%(P9), 2)) THEN 9755
	IF S8%(P9, 1) > 0 THEN 9754
	IF P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) < 3 AND P8%(P9, P1%(P9), 2) = 0 THEN 9754
	IF P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 2 AND P8%(P9, P1%(P9), 0) >= 5 THEN 9755

9754 PRINT p$(P9, P1%(P9)); " seems to be OK!!"; : GOTO 9757

9755 PRINT p$(P9, P1%(P9)); " is tiring!!"; : GOTO 9757

9756 CALL RANDNUM(rn)
	IF rn <= 50 AND S8%(P9, 1) > 0 AND INNING% < 6 OR S8%(P9, 1) = 0 AND P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 4 THEN PRINT p$(P9, P1%(P9)); " just doesn't have it today"; : GOTO 9757
	IF rn > 50 AND S8%(P9, 1) > 0 AND INNING% < 6 OR S8%(P9, 1) = 0 AND P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 4 THEN PRINT p$(P9, P1%(P9)); " ain't got nuthin' on his fastball folks!"; : GOTO 9757
	PRINT p$(P9, P1%(P9)); " is tired!!";

9757 '
END SUB


'------------------------------
'   TRIPLEPLAY Subroutine
'------------------------------
' ...explanation...
SUB TRIPLEPLAY (D)
	CALL NEWLINES: Q$(4) = b$(D, B9%(D, 5))
	PRINT "grounder to "; Q$(4); " @ 3B": CALL DELAY
	PRINT Q$(2); " OUT @ 3B": CALL DELAY
	PRINT Q$(1); " OUT @ 2B": CALL DELAY
	PRINT Q$(0); " OUT @ 1B": CALL DELAY:
	PRINT "--- TRIPLE PLAY !!! ---": CALL DELAY: CALL DELAY
	O%(0) = 3
	I5 = P1%(D): I6 = 0: FOR i = 1 TO 3: CALL ADDPIT(D, I5, I6): NEXT i
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF(P, I5, I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF(P, I5, I6)
END SUB


'------------------------------
'   TRYFOREXTRABASE Subroutine
'------------------------------
' ...explanation...
SUB TRYFOREXTRABASE (LN%, S2%, P2, Z2%, D, W%, sah%, SB%,I8,I9)
	IF LN% = 3308 THEN LN% = 0: GOTO 3308
	I8 = 0: H6% = INT(RND(1) * 100) + 1
	IF S2% = 8 AND B0%(2, 0) > 0 AND H6% < 6 AND B0%(3, 0) = 0 AND b%(p, B3%(p, B0%(2, 0)), 8) > 0 THEN I8 = 3
	IF S2% = 9 AND B0%(3, 0) > 0 AND H6% <= 2 AND b%(p, B3%(p, B0%(3, 0)), 9) > 0 THEN I8 = 4
	IF S2% = 7 AND B0%(3, 0) > 0 AND P2 = 2 AND H6% >= 6 AND H6% <= b%(p, B3%(p, B0%(3, 0)), 16) THEN I8 = 4
	IF S2% = 7 AND B0%(3, 0) > 0 AND Z2% = B0%(3, 0) AND P2 <> 2 AND H6% <= b%(p, B3%(p, B0%(3, 0)), 16) * 12 THEN I8 = 4
	E2% = 10
	IF B0%(2,0)>0 THEN E2%=E2% + b%(p, B3%(p, B0%(2, 0)), 16)
	IF S2% = 7 AND B0%(2, 0) > 0 AND P2 <> 2 AND H6% <= E2% AND B0%(3, 0) = 0 AND I8 = 0 THEN I8 = 3
	IF S2% = 7 AND B0%(1, 0) > 0 AND B0%(2, 0) = 0 AND I8 = 0 AND H6% <= b%(p, B3%(p, B0%(1, 0)), 16) AND b%(p, B3%(p, B0%(1, 0)), 7) > 0 THEN I8 = 2
	IF I8 = 0 THEN 3385

3308 IF u6 = 2 OR u6 = 1 AND p = u9 THEN 3310
	PRINT Q$(I8 - 1);
	IF S2% < 10 THEN PRINT " try for "; H$(I8); " (YN) "
	IF S2% > 10 THEN PRINT " tag up (YN) "
	PRINT "against "; Q$(4)

3310 ';" Arm:";B%(D,B9%(D,W%),15)
	i$ = "N": A1 = S6%(p, 0) - S6%(D, 0)
	IF (u6 = 2 OR u6 = 1 AND p = u9) AND A1 > -2 AND A1 < 2 AND S2% <> 19 THEN i$ = "Y": GOTO 3312
	IF (u6 = 2 OR u6 = 1 AND p = u9) AND A1 < 0 AND (INNING% >= 7 OR S2% = 19) THEN i$ = "N": GOTO 3312
	IF S2% = 8 AND O%(0) = 2 AND I8 = 3 AND (u6 = 2 OR u6 = 1 AND p = u9) THEN i$ = "N"
	IF u6 = 2 OR u6 = 1 AND p = u9 THEN 3312
3311 i$ = GetKeyPress$

3312 IF UCASE$(i$) <> "Y" THEN i9 = 1: GOTO 3385
	PRINT Q$(I8 - 1); " is "; : IF S2% < 10 THEN PRINT "going for "; H$(I8)
	IF S2% > 10 THEN PRINT "tagging"
	CALL DELAY
	IF S2% = 7 AND I8 = 2 OR S2% > 10 AND S4% = 3 THEN 3317
	IF u6 = 2 OR u6 = 1 AND D = u9 THEN 3322
	PRINT Q$(4); " throw to "; H$(I8); " (YN) "

3322 CALL RANDNUM (RN)
	IF (u6 = 2 OR u6 = 1 AND D = u9) AND MG%(D,766)=999 AND I8=4 AND INNING%>=MG%(D,767) AND MG%(D,767)<>0 AND MG%(D,768)<=S6%(D, 0) - S6%(P, 0) THEN I$="N":GOTO 3316
	IF (u6 = 2 OR u6 = 1 AND D = u9) AND MG%(D,766)=999 AND I8=4 AND INNING%>=MG%(D,769) AND MG%(D,769)<>0 AND MG%(D,770)<=S6%(D, 0) - S6%(P, 0) THEN I$="N":GOTO 3316
	IF u6 = 2 OR u6 = 1 AND D = u9 THEN i$ = "Y": GOTO 3316
3315 i$ = GetKeyPress$

3316 i9 = 0: IF UCASE$(i$) <> "Y" THEN i9 = 1: PRINT "No throw is made": CALL DELAY: GOTO 3380

3317 CALL NEWLINES: PRINT "Here comes the throw by "; Q$(4)
	FOR i = 1 TO 3: PRINT "."; : CALL DELAY: NEXT i: PRINT " "
	CALL FLDERR (F%, W%, D, I3, SB%, S2%, p): IF F% = 1 THEN 3385
	I4 = INT(RND(1) * 1000) + 1: A = b%(p, B3%(p, B0%(I8 - 1, 0)), 16)
	IF B7%(p, B0%((I8 - 1), 0)) = 1 THEN A = p%(p, P1%(p), 31)
	A = A - 5: A = A * 50
	IF S2% = 17 THEN b = ((b%(D, B9%(D, W%), 15) - 5) * 25): I3 = 1000 + A - b:' WAS 800
	IF S2% = 18 THEN b = ((b%(D, B9%(D, W%), 15) - 5) * 25): I3 = 800 + A - b:' WAS 650
	IF S2% = 19 OR S2% < 10 THEN b = ((b%(D, B9%(D, W%), 15) - 5) * 25): I3 = 500 + A - b
	IF I4 <= I3 THEN 3380
	A5%(I8 - 1) = 11: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5 = P1%(D): I6 = 0: CALL ADDPIT(D, I5, I6)
	CALL RANDNUM (rn)
	IF rn <= 4 AND I8 = 4 THEN PRINT Q$(I8 - 1); " collides with "; b$(D, B9%(D, 2)): CALL DELAY: PRINT b$(D, B9%(D, 2)); " hangs on!!": GOTO 3377
	IF rn <= 50 AND I8 <> 4 THEN PRINT Q$(I8 - 1); " doesn't make it!": GOTO 3377
	IF rn <= 80 THEN PRINT "throw is perfect!": GOTO 3377
	IF rn <= 90 THEN PRINT "This will be close": GOTO 3377
	PRINT "Close play.....": CALL DELAY
	IF I8 < 4 THEN PRINT U$(I8 + 1);
	IF I8 = 4 THEN PRINT U$(1);
	PRINT " makes the call..."

3377 CALL DELAY: CALL CREDITASSIST (W%, D): CALL ADVANCEMENT (p, D, S2%, F%,I3): CALL CREDITPUTOUT(2, FB2P%, AST%, D): W% = 2
	IF O%(0) = 3 OR S2% > 10 THEN 3385

3371 I5 = INT(RND(1) * 100) + 1
	A1 = S6%(p, 0) - S6%(D, 0)
	IF A1 < -2 OR A1 < 0 AND INNING% > 8 THEN 3376
	  '*** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
	IF I8 - 1 > 2 AND I5 <= 15 AND B0%(2, 0) > 0 THEN
		CALL CREDITASSIST (W%, D)
		PRINT "runner is trying for 3rd...": CALL DELAY
		A5%(2) = 11
		O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
		W% = 5: CALL CREDITREGPO(W%, D)
		I5 = P1%(D): I6 = 0
		CALL ADDPIT(D , I5, I6)
	END IF
	  '*** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***
	I5 = INT(RND(1) * 100) + 1
	IF I8 - 1 > 1 AND I5 <= 15 AND B0%(1, 0) > 0 AND A5%(2) <> 11 THEN
		CALL CREDITASSIST(W%, D)
		W% = 6: CALL CREDITREGPO(W%, D)
		PRINT "runner is trying for 2nd...":CALL DELAY
		A5%(1) = 11
		O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
		I5 = P1%(D): I6 = 0
		CALL ADDPIT(D, I5, I6)
	END IF
	I5 = INT(RND(1) * 100) + 1
	IF B0%(2,0)>0 AND I8 - 1 > 2 AND A5%(2) <> 11 AND I5 > (80 - b%(p, B3%(p, B0%(2, 0)), 16) * 2) AND i9 = 0 THEN A5%(2) = 1: PRINT "runner is trying for 3rd...": CALL DELAY: PRINT "He's safe!": CALL DELAY
	I5 = INT(RND(1) * 10) + 1
	IF B0%(1,0)>0 AND A5%(1)<>11 AND (S4%=4 OR S4%=7 AND I8-1>1 AND I5>(10 - b%(p, B3%(p, B0%(1, 0)), 16)) AND i9 = 0) THEN A5%(1) = 1: PRINT "runner is trying for 2nd...": CALL DELAY: PRINT "He's in there!": CALL DELAY: GOTO 3376
	I5 = INT(RND(1) * 100) + 1
	IF B0%(1,0)>0 AND I8-1>1 AND I5>(80-b%(p,B3%(p,B0%(1, 0)),16)*2) AND i9=0 AND A5%(1)<>11 THEN PRINT "runner safe at 2nd on the throw": CALL DELAY: A5%(1) = 1

3376 CALL ADVANCEMENT(p, D, S2%, F%,I3): i9 = 0: GOTO 3385

3380 A5%(I8 - 1) = 1: IF i9 = 1 THEN 3379
	CALL RANDNUM (rn)
	IF rn <= 4 AND I8 = 4 THEN PRINT Q$(I8 - 1); " collides with "; b$(D, B9%(D, 2)): CALL DELAY: PRINT "He knocks the ball loose!!": GOTO 3379
	IF rn <= 50 AND I8 <> 4 THEN PRINT Q$(I8 - 1); " beats the throw!": GOTO 3379
	IF rn <= 60 THEN PRINT Q$(I8 - 1); " is ahead of the throw": GOTO 3379
	IF rn <= 80 THEN PRINT "throw is off line": GOTO 3379
	IF rn <= 90 THEN PRINT Q$(I8 - 1); " slides under the tag": GOTO 3379
	PRINT "Close play...": CALL DELAY
	IF I8 <> 4 THEN PRINT U$(I8 + 1);
	IF I8 = 4 THEN PRINT U$(1);
	PRINT " makes the call..."

3379 CALL DELAY: PRINT Q$(I8 - 1); " is safe @ "; H$(I8): CALL DELAY
	IF I8 = 4 THEN sah% = 1
	CALL TAGADVANCE(S2%, I8, D): IF S2% < 10 THEN 3371
	IF S2% > 10 AND B7%(p, B1!(p)) = 1 THEN P8%(p, P1%(p), 23) = P8%(p, P1%(p), 23) - 1: P5%(p, P1%(p), 23) = P5%(p, P1%(p), 23) - 1: I5 = P1%(p): I6 = 41: CALL INCPITOFF(P, I5, I6): I5 = P1%(D): I6 = 16: CALL ADDPIT(D, I5, I6): GOTO 3385
	IF S2% > 10 AND B7%(p, B1!(p)) <> 1 THEN 3381
	GOTO 3385

3381 B8%(p, B3%(p, B1!(p)), 0) = B8%(p, B3%(p, B1!(p)), 0) - 1
	B5%(p, B3%(p, B1!(p)), 0) = B5%(p, B3%(p, B1!(p)), 0) - 1
	IF P%(D,P1%(D),0) = 1 THEN 3382
	CSS%(P, B3%(p, B1!(p)), 0) = CSS%(P, B3%(p, B1!(p)), 0) - 1

3382 I5 = B3%(p, B1!(p)): I6 = 19: CALL INCBATOFF(P, I5, I6): I5 = P1%(D)
	I6 = 16: CALL ADDPIT(D, I5, I6): GOTO 3371

3385 '
END SUB


'------------------------------
'   VIEWTEAM Subroutine
'------------------------------
' ...explanation...
SUB VIEWTEAM ()

700 CLS
	COLOR 12: PRINT TAB(20); "SELECT TEAM TO VIEW DETAILED STATS FOR:": PRINT : PRINT
	COLOR 14, 0: PRINT "(0) "; : COLOR l%(0, 11), l%(0, 12): PRINT A$(0): PRINT
	COLOR 14, 0: PRINT "(1) "; : COLOR l%(1, 11), l%(1, 12): PRINT A$(1): PRINT
	COLOR 14, 0: PRINT "(2) "; : COLOR 15: PRINT "RETURN TO PLAY"
701 i$ = GetKeyPress$
	WK = VAL(i$): IF WK < 0 OR WK > 2 THEN 701
	IF WK = 2 THEN 850
	CLS
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "VIEW PITCHERS": PRINT
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "VIEW HITTERS": PRINT
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "RETURN TO PLAY"
706 i$ = GetKeyPress$
	Q = VAL(i$): IF Q < 1 OR Q > 3 THEN 706
	IF Q = 3 THEN 850
	IF Q = 1 THEN 800

725 CLS
	COLOR 12: PRINT "VIEW BATTERS (AVAILABLE PLAYERS HIGHLIGHTED)"
	FOR i = 0 TO 10: COLOR 3, 0
	IF b%(WK, i, 21) = 0 THEN COLOR 11
	LOCATE i + 5, 5: PRINT USING "##"; i; : PRINT " "; b$(WK, i); TAB(40);
	COLOR 3
	IF b%(WK, i + 12, 21) = 0 THEN COLOR 11
	PRINT USING "##"; i + 12; : PRINT " "; b$(WK, i + 12): COLOR 3: NEXT
	IF b%(WK, 11, 21) = 0 THEN COLOR 11
	LOCATE , 5: PRINT USING "##"; 11; : PRINT " "; b$(WK, 11)
	COLOR 3

730 COLOR 15: LOCATE 20, 1: PRINT "# TO VIEW (0-22) (23 TO CONTINUE) "; : INPUT M
	IF M < 0 OR M > 23 THEN 730
	IF M = 23 THEN 700
	LOCATE 3, 3: COLOR l%(WK, 11), l%(WK, 12): PRINT " "; CHR$(201); STRING$(71, 205); CHR$(187); " "
	FOR X = 1 TO 7: LOCATE , 3: PRINT " "; CHR$(186); STRING$(71, 32); CHR$(186); " ": NEXT
	LOCATE , 3: PRINT " "; CHR$(200); STRING$(71, 205); CHR$(188); " "
	LOCATE 4, 6: PRINT "Stats for ";
	PRINT b$(WK, M): LOCATE , 6: PRINT "BATS: "; B1$(b%(WK, M, 0) + 2); "    "; : PRINT USING "G/A%: ##       BUNT: #       RUN: #"; b%(WK, M, 18); b%(WK, M, 30); b%(WK, M, 16)
	LOCATE , 3: PRINT " "; CHR$(204); STRING$(71, 205); CHR$(185); " "
	LOCATE , 6: PRINT "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"
	CALL POPUPBAT(WK, M)
790 i$ = GetKeyPress$
	COLOR 7, 0: GOTO 725

800 CLS
	COLOR 12: PRINT "VIEW PITCHERS (AVAILABLE PLAYERS HIGHLIGHTED)"
	FOR i = 0 TO 10: COLOR 3, 0
	IF p%(WK, i, 15) = 0 THEN COLOR 11
	LOCATE i + 5, 5: PRINT USING "##"; i; : PRINT " "; p$(WK, i); TAB(40);
	COLOR 3
	IF p%(WK, i + 11, 15) = 0 THEN COLOR 11
	PRINT USING "##"; i + 11; : PRINT " "; p$(WK, i + 11): COLOR 3: NEXT

801 COLOR 15: LOCATE 20, 1: PRINT "# TO VIEW (0-21) (22 TO CONTINUE) "; : INPUT M
	IF M < 0 OR M > 22 THEN 801
	IF M = 22 THEN 700
	CALL POPUPPIT(WK, M)
830 i$ = GetKeyPress$
	COLOR 7, 0: GOTO 800

850 '
END SUB


'------------------------------
'   WALLDOUBLE Subroutine
'------------------------------
' ...explanation...
SUB WALLDOUBLE (HW%, W%)
	'*** doubles off of a wall ***
	BV = INT(RND(1) * 100) + 1: rn = INT(RND(1) * 100) + 1

3781 IF BV <= 40 AND HW% = 0 THEN PRINT "It's in the gap and to the wall": GOTO 3785
	IF BV <= 65 AND rn <= 25 AND (l%(1, 13) = 8 OR l%(1, 13) = 40) THEN PRINT "It's off the ivy!": GOTO 3785
	IF BV <= 65 AND rn <= 50 AND l%(1, 13) = 21 AND W% = 7 THEN PRINT "It hits off the Green Monster!": GOTO 3785
	IF BV <= 65 AND l%(1, 13) = 27 AND W% = 7 THEN PRINT "It hits off the screen!": GOTO 3785
	IF BV <= 65 THEN PRINT "It's off the wall": GOTO 3785
	IF BV <= 85 AND HW% = 0 THEN 3788
	IF BV <= 90 AND HW% = 0 THEN PRINT "It's to the wall": CALL DELAY: PRINT Q$(0); " slides into second": PRINT "with a stand-up double!!": GOTO 3785
	IF rn <= 25 AND l%(1, 13) = 21 THEN PRINT "It's off the Green Monster!": GOTO 3785
	IF l%(1, 13) = 27 AND W% = 7 THEN PRINT "It's off the screen...": GOTO 3785
	PRINT "It's off the top of the wall...": GOTO 3785

3788 IF W% = 8 THEN BV = 39: GOTO 3781
	PRINT "It's past "; Q$(4): CALL DELAY: PRINT "It goes into the corner...": GOTO 3785

3785 '
END SUB


'------------------------------
'   WHICHRUNNER Subroutine
'------------------------------
' ...explanation...
SUB WHICHRUNNER (i, S4%, D5)
	IF i = 4 THEN A5%(3) = 1
	IF S4% <> 4 THEN 4297
	CALL NEWLINES: LOCATE 15, 1: PRINT "WHICH RUNNER DO YOU WANT TO GO AFTER"
	PRINT "1 - "; b$(p, B3%(p, B0%(2, 0)))
	IF B7%(p, B0%(1, 0)) <> 1 THEN PRINT "2 - "; b$(p, B3%(p, B0%(1, 0)))
	IF B7%(p, B0%(1, 0)) = 1 THEN PRINT "2 - "; p$(p, P1%(p))
	IF u6 = 2 OR u6 = 1 AND p <> u9 THEN D5 = INT(RND(1) * 2) + 1: i$ = STR$(D5): GOTO 4296
4295 i$ = GetKeyPress$

4296 D5 = VAL(i$): CALL NEWLINES: LOCATE 15, 1: IF D5 = 2 THEN i = 2: D5 = 3: I2 = B3%(p, B0%(1, 0)): IF B7%(p, B0%(1, 0)) = 1 THEN Q$(i - 1) = p$(p, P1%(p))
	D5 = 0

4297 '
END SUB


'------------------------------
'   WHOSON1ST Subroutine
'------------------------------
' ...explanation...
SUB WHOSON1ST (S4%)
	S4% = 0: I1 = B0%(1, 0): I2 = B0%(2, 0): I3 = B0%(3, 0)
	IF I1 > 0 AND I2 = 0 AND I3 = 0 THEN S4% = 1
	IF I1 = 0 AND I2 > 0 AND I3 = 0 THEN S4% = 2: REM*** RUNNER ON 2ND ***
	IF I1 = 0 AND I2 = 0 AND I3 > 0 THEN S4% = 3: REM*** RUNNER ON 3RD ***
	IF I1 > 0 AND I2 > 0 AND I3 = 0 THEN S4% = 4: REM*** 1ST AND 2ND ***
	IF I1 > 0 AND I2 = 0 AND I3 > 0 THEN S4% = 5: REM*** 1ST AND 3RD ***
	IF I1 = 0 AND I2 > 0 AND I3 > 0 THEN S4% = 6: REM*** 2ND AND 3RD ***
	IF I1 > 0 AND I2 > 0 AND I3 > 0 THEN S4% = 7: REM*** BASES LOADED ***
END SUB


'------------------------------
'   FOULBALL Subroutine
'------------------------------
' ...explanation...
SUB FOULBALL ()
	call randnum(rn)
	if rn<=50 then
		Y% = INT(RND(1) * 20) + 3130
		CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
		PRINT Y$
		CALL DELAY
		PRINT "foul ball!!"
		call delay
	else
		Y% = INT(RND(1) * 10) + 3150
		CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
		PRINT Y$
		CALL DELAY
		Y% = INT(RND(1) * 10) + 3160
		CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
		PRINT Y$
		CALL DELAY
	end if
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
