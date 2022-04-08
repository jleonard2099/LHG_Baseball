'--INCLUDE: 'COMMON.GW'
'--INCLUDE: 'GETVIDEO.BAS'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

'The following files are combined
'to make the full EXE / game
'LOADER
'SOURCE + SETUP + RAREPLAY + ROUTINES

_Title "Full Count Baseball - PLAY BALL!"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB COLCHK (P9, I)
DECLARE SUB DRAWBOX ()
DECLARE SUB GETAKEY ()
DECLARE SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)
DECLARE SUB INFO ()
DECLARE SUB INITVARI ()
DECLARE SUB LETTER (I$)
DECLARE SUB LINSWAP (P9)
DECLARE SUB NEWLINES (P9)
DECLARE SUB POSITION (I%)
DECLARE SUB POSSWAP (P9)
DECLARE SUB SHOWBATTERS (P9)
DECLARE SUB SHOWFIELD (P9%)
DECLARE SUB SHOWSPLIT (P9%)
'DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)

Type PBP
    YY As String * 30
End Type

Dim K9(1) As Single
Dim A$(1), L%(1, 13), T1(1), B%(1, 22, 80), P%(1, 21, 89), B$(1, 22), P$(1, 21), H$(4)
Dim PT$(1), PARK$(83), TS%(1, 11), SA%(1, 24)
Dim Shared Q3$(100), MON(5, 14)
Dim Shared ATTEN As Long
Dim INJ%(1, 22), DYS%(1, 21), PU$(8), WD$(9)
Dim P6%(1), P1%(1), S8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
Dim S%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), C$(11), F$(10)
Dim VV(1), VV1(1), D3%(1), P2%(1), A5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
Dim X0%(1, 2, 23), D0%(1), TP%(11, 2), L0%(1), T3%(22), AP%(2)
Dim P5%(1, 21, 41), B5%(1, 22, 21), P8%(1, 21, 41), B8%(1, 22, 21)
Dim T%(22), P4%(22), B2%(1, 22), P3%(1, 21)
Dim NG%(18), YN$(1), Q$(23), H0$(1), TM$(4), SC$(4), PC$(4), N$(1), D$(3), M$(1)
Dim BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2), UM%(1, 9, 4), MF%(1)
Dim PR%(24), GK%(1)
Dim CSS%(1, 22, 6)
Dim MG%(1, 999), GS%(21, 1), PS%(23, 1)
Dim U%(50)
Dim V$(200), S$(1), SP%(1), U6$(2), LGT$(2), DH$(1), UMP$(50), WE$(1), MN$(10), NB%(1, 4)
Dim UV%(200), VI$(1), DL$(1), DN$(1), F(9), M%(9), BB%(9), BP(9)

' DIM Ed AS EditInfo              'create the editor's data structure

Dim Shared TS%(), SA%(), P8%(), B8%(), P%(), P$(), B$(), P5%(), B5%(), X0%(), B%(), AP%(), A$()
Dim Shared C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
Dim Shared S%(), P1%(), S1%(), D$(), S6%(), L%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
Dim Shared B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
Dim Shared VV(), VV1(), B4%(), K9() As Single, H0$(), H$(), D0%(), M$(), LB%()
Dim Shared F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
Dim Shared S$, P, U6, DH%, U9, G4 As Single, RE, RD
Dim Shared TA%, SC%, WF As Single, PC%, TP%, WS%, WI$, DRIVE, AP%, INNING%, BUCK As PBP
Dim Shared MG%(), CSS%(), D, IJ%, DR%, CGERA%
ReDim Q2$(2056)

Dim E As Single
Dim SE As Single
Dim Q1 As Single
Dim QQ As Single
Dim R As Single
Dim Y As Long
Dim JJ As Single

DATA first,second,third,short
DATA 6,12,37,55,66,74,86,96,100
DATA 5,10,32,55,67,75,86,96,100
DATA 4,8,27,54,68,76,86,96,100
DATA 3,7,25,54,69,76,86,96,100
DATA 3,6,22,54,69,77,86,96,100
DATA 2,5,20,53,69,78,86,96,100
DATA 2,4,17,53,70,78,86,96,100
DATA 1,3,12,52,71,79,86,96,100
DATA 1,2,7,51,72,80,86,96,100
DATA 6,14,42,55,63,74,86,96,100
DATA 5,12,37,55,64,75,86,96,100
DATA 4,10,32,55,65,76,86,96,100
DATA 3,8,30,55,66,77,86,96,100
DATA 3,7,27,55,66,77,86,96,100
DATA 3,6,24,55,66,77,86,96,100
DATA 2,5,22,55,67,78,86,96,100
DATA 1,4,17,55,68,79,86,96,100
DATA 1,3,12,55,69,80,86,96,100
DATA 0,0,100
DATA 0,15,100
DATA 0,30,100
DATA 0,45,100
DATA 0,60,100
DATA 1,75,100
DATA 3,90,100
DATA 4,97,100
DATA 100,0,90,100,75,100,60,100,45,100,30,100,15,100,8,100
DATA 100,0,0,90,100,0,70,95,100,50,80,100,30,65,100,10,50,100
DATA 0,90,100,5,87,100,10,85,100,15,80,100,20,77,100,25,75,100
DATA 30,73,100,35,70,100,40,67,100,45,65,100,50,63,100,60,65,100

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

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
