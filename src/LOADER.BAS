$Debug
'$INCLUDE: 'ROUTINES.BI'
'$INCLUDE: 'ROUTINES_G.BI'
'$INCLUDE: 'SOURCE.BI'
'$INCLUDE: 'SOURCE_G.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - PLAY BALL!"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

DefInt A-Z

DECLARE SUB SHOWFIELD (P9%)
DECLARE SUB SHOWSPLIT (P9%)
DECLARE SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)
DECLARE SUB INFO ()
DECLARE SUB INITVARI ()
DECLARE SUB DRAWBOXA ()
DECLARE SUB NEWLINES (P9)
DECLARE SUB POSITION (I%)
DECLARE SUB POSSWAP (P9)
DECLARE SUB LINSWAP (P9)
DECLARE SUB SHOWBATTERS (P9)
DECLARE SUB LETTER (I$)
DECLARE SUB COLCHK (P9, I)
DECLARE SUB POSTGAME ()

Type PlayCall
    YY As String * 30
End Type

'Declare all variables for the modules
Dim Shared B1!(9)
Dim Shared displayPause As Single, K9(1) As Single, WF As Single
Dim Shared ATTEN As Long

Dim Shared AP%, CGERA%, DH%, DR%, EJ%, IJ%, IJL%, INNING%
Dim Shared PB%, PC%, SC%, TA%, TP%, W%, WS%
Dim Shared F$, PARK$, S$, WI$
Dim Shared D, P, RE, RD, U6, U9
Dim Shared usingGfx

'DIM SHARED BP(1)
Dim BP(9), F(9), T1(1)
Dim Shared MON(5, 14), VV(1), VV1(1)

Dim BAT$(23), BM$(10), BMT$(10), DH$(1), DL$(1), DN$(1)
Dim LGT$(2), MN$(10), PIT$(22), PM$(6), PMT$(6), PT$(1), PU$(8)
Dim U6$(2), UMP$(50), teamItems$(200), VI$(1), WE$(1)
Dim Shared PARK$(99), Q3$(100), WD$(10)
'--> should WD$ have been increased??? Probably not, just DIM'd as 0 to 9

Dim Shared A$(1), B$(1, 22), B1$(4), C$(11), D$(3)
'G$ = PBP lines
Dim Shared EJ$(20), F$(10), G$(10), H$(4), H0$(1), IJ$(20)
Dim Shared M$(1), N$(1), NN$(1), P$(1, 21), PC$(4)
Dim Shared Q$(23), S$(1), SC$(4), TM$(4), U$(4), X$(12), YN$(1)

Dim BAT%(23), BB%(9), DYS%(1, 21), GS%(21, 1)
Dim MF%(1), NB%(1, 4), PS%(23, 1), PIT%(22), SP%(1)
Dim U%(50), UM%(1, 9, 4), UV%(200)
Dim Shared A5%(4), AP%(2), B%(1, 0 To 22, 80), B0%(3, 2)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(1, 9), B4%(1, 7, 9)
Dim Shared B5%(0 To 1, 0 To 22, 0 To 21), B7%(1, 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(1, 9, 9)
Dim Shared CF%(9, 2), CSS%(1, 22, 6), D0%(1), D3%(1), DP%(1), GK%(1), H0%(1)
Dim Shared INJ%(0 To 1, 0 To 22)
Dim Shared L%(1, 13), L0%(1), LB%(9, 1), M%(9), MG%(1, 999), NG%(18), O%(1)
Dim Shared P%(0 To 1, 0 To 21, 0 To 89), P1%(1), P2%(1), P3%(1, 21), P4%(22), P5%(1, 21, 41), P6%(1), P8%(1, 21, 41), PR%(24)
Dim Shared S%(1, 26), S1%(1), S6%(1, 2), S8%(1, 1), SA%(1, 24), SO%(5, 2), SU%(1, 1)
Dim Shared T%(22), T3%(22), TP%(11, 2), TS%(1, 11), VA%(26), X0%(1, 2, 23)
Dim Shared normalFinish%

ReDim Q2$(2056)
Dim E As Single
Dim SE As Single
Dim Q1 As Single
Dim QQ As Single
Dim R As Single
Dim Y As Long
Dim JJ As Single

Dim Shared FontColAdj%

normalFinish% = 0

'Graphics co-ordinates originally were for a
'routine that operated as if the screen were
'still just 80 column width; so 80x8 = 640
FontColAdj% = 8

MON = Monitor%
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
Input #1, displayPause: Input #1, VG%: Input #1, CGERA%
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
MO% = 4

86 DN$(0) = "AFTERNOON"
DN$(1) = "NIGHT"
B1$(0) = "LP"
B1$(1) = "L"
B1$(2) = "S"
B1$(3) = "R"
B1$(4) = "RP"

90 Data A,B,C,D,E,F,G,H,I,J,K,L,M,dh,p,"c ",1b,2b,3b,ss,lf,cf,rf,ph,pr

100 Cls
'For RA = 1 To 60
Randomize Timer
'Next

P = 1

110 For I = 0 To 12: Read X$(I): Next I

120 For I = 0 To 11: Read C$(I): Next I

140 H$(1) = "1b": H$(2) = "2b": H$(3) = "3b": H$(4) = "home"

144 For I = 0 To 1: For I1 = 0 To 4: NB%(I, I1) = -1: Next: Next
For I = 0 To 1: P1%(I) = -1: P6%(I) = -1
Next
SP%(0) = -1: SP%(1) = -1


For I = 0 To 1
    For J = 1 To 9
        M%(J) = 0
        B7%(I, J) = -1
        B3%(I, J) = -1
    Next
Next
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
displayPause = NG%(4) / 10
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
Color 14: Print "(D) ";: Color 15: Print Using "DISPLAY INTERVAL: #.# "; displayPause
Color 14: Print "(E) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; WE$(VG%)
Color 14: Print "(F) ";: Color 15: Print "PITCHER USAGE ERA: "; PU$(CGERA%)

183 Color 7, 0
I$ = GetKeyPress$
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

210 Locate 16, 22: Input displayPause: GoTo 171

205 Open "DEFERA" For Output As #1: Print #1, U6: Print #1, LGT: Print #1, DH%
Print #1, DL: Print #1, MO%: Print #1, DN%: Print #1, WE%
Print #1, IJ%: Print #1, LR%: Print #1, DR%: Print #1, MP%
Print #1, displayPause: Print #1, VG%: Print #1, CGERA%
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

    2800 Color 15, 0: Cls
    JJ = 10
    Locate 6, 8: Color 15, 4
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
    Locate 7 + JJ, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
    Locate , 12: Print VI$(I)
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
    For I1 = 1 To 200: teamItems$(I1) = "": Next

    2857 GoSub 8100

    2856 If BO% = 1 Then BO% = 0: GoTo 2800

    2858 Locate 2, 33: Color L%(I, 11), L%(I, 12): Print A$(I): Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";: I$ = GetKeyPress$

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

    560 Call POSITION(I): Print Space$(18);: Call POSITION(I)

    561 I$ = GetKeyPress$

    570 Call LETTER(I$)
    If I$ = Chr$(27) Then
        For J = 1 To 9
            M%(J) = 0
            B7%(P9, J) = -1
            B3%(P9, J) = -1
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

861 If UCase$(I$) = "R" Or I$ = Chr$(27) Then
    For J = 1 To 9: M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1: Next: C1 = 0: For J = 0 To 22: B%(P9, J, 21) = 0: Next: RP = 1: GoTo 1001
End If

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
':S8%(P9,1) = 4

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

1770 GoTo 1790

1780 I$ = GetKeyPress$

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

2125 ':GRANT DAY OFF
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

2260 Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then 2280

2270 I$ = GetKeyPress$

2280 Return

2310 For I = 10 To 22
    Locate I, 59: Print "                     ";
Next
Return

2320 Cls

2330 GoSub 2470

2345 I1 = Val(Left$(A$(0), 2)): I2 = Val(Left$(A$(1), 2))
If Val(Left$(A$(0), 3)) = 211 Then I1 = 201
If Val(Left$(A$(0), 3)) = 200 Then I1 = 200
If Val(Left$(A$(1), 3)) = 200 Then I2 = 200
If Val(Left$(A$(1), 3)) = 211 Then I2 = 201


If CGERA% > 2 Then S8%(1, 1) = 4: S8%(0, 1) = 4
If CGERA% > 3 Then S8%(1, 1) = 5: S8%(0, 1) = 5
If CGERA% = 7 Then S8%(1, 1) = 7: S8%(0, 1) = 7
If CGERA% >= 8 Then S8%(1, 1) = 99: S8%(0, 1) = 99


'STACK 0
'Data to read in to variables
Data first,second,third,short
Data 6,12,37,55,66,74,86,96,100
Data 5,10,32,55,67,75,86,96,100
Data 4,8,27,54,68,76,86,96,100
Data 3,7,25,54,69,76,86,96,100
Data 3,6,22,54,69,77,86,96,100
Data 2,5,20,53,69,78,86,96,100
Data 2,4,17,53,70,78,86,96,100
Data 1,3,12,52,71,79,86,96,100
Data 1,2,7,51,72,80,86,96,100
Data 6,14,42,55,63,74,86,96,100
Data 5,12,37,55,64,75,86,96,100
Data 4,10,32,55,65,76,86,96,100
Data 3,8,30,55,66,77,86,96,100
Data 3,7,27,55,66,77,86,96,100
Data 3,6,24,55,66,77,86,96,100
Data 2,5,22,55,67,78,86,96,100
Data 1,4,17,55,68,79,86,96,100
Data 1,3,12,55,69,80,86,96,100
Data 0,0,100
Data 0,15,100
Data 0,30,100
Data 0,45,100
Data 0,60,100
Data 1,75,100
Data 3,90,100
Data 4,97,100
Data 100,0,90,100,75,100,60,100,45,100,30,100,15,100,8,100
Data 100,0,0,90,100,0,70,95,100,50,80,100,30,65,100,10,50,100
Data 0,90,100,5,87,100,10,85,100,15,80,100,20,77,100,25,75,100
Data 30,73,100,35,70,100,40,67,100,45,65,100,50,63,100,60,65,100


'Read in data and assign variables
For I = 3 To 6
    Read F$(I)
Next

H0$(0) = " ": H0$(1) = "H"
TM$(1) = "HOT": TM$(2) = "WARM": TM$(3) = "COOL": TM$(4) = "COLD"
SC$(1) = "CLEAR": SC$(2) = "PARTLY CLOUDY": SC$(3) = "OVERCAST": SC$(4) = "DOME"
PC$(1) = "THUNDERSTORMS": PC$(2) = "SHOWERS": PC$(3) = "DRIZZLE": PC$(4) = "NONE"

10 Call GETSTATS
Call DISPBAVG(D)
W5 = 0: W6 = 1
For I = 0 To 1: For I1 = 1 To 9: For I2 = 1 To 9: Read BT%(I, I1, I2): Next: Next: Next
For I = 2 To 9: For I1 = 0 To 2: Read CF%(I, I1): Next: Next
For I = 2 To 9: For I1 = 0 To 1: Read LB%(I, I1): Next: Next
For I = 0 To 5: For I1 = 0 To 2: Read SO%(I, I1): Next: Next
For I = 11 To 0 Step -1: For I1 = 0 To 2: Read TP%(I, I1): Next: Next

If (MON = 6 Or MON = 7 Or MON = 12) And VG% = 0 Then
    usingGfx = 1
    Call SOURCEG
Else
    usingGfx = 0
    Call SOURCE
End If

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
Call DRAWBOXA
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


3110 Cls: Print "ERROR:: NO LINEUPS  FOUND...BE SURE CORRECT LINEUP DISK IS IN DRIVE"
If AP% = 1 Then NLF% = 1: GoTo 480

3111 Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE"
I$ = GetKeyPress$
GoTo 480

3500 ':SELECT A DIFFERENT STADIUM
PARK$(1) = "ANAHEIM STADIUM             "
PARK$(2) = "THE ASTRODOME               "
PARK$(3) = "OAKLAND-ALAMEADA COLISEUM   "
PARK$(4) = "SKYDOME                     "
PARK$(5) = "ATLANTA-FULTON COUNTY STADIU"
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
PARK$(83) = "UNKNOWN                     "
PARK$(99) = "GENERIC                     "

Call DRAWBOXA
PARK$ = Right$(Str$(L%(1, 13)), Len(Str$(L%(1, 13))) - 1)
If DN% = 0 Then PARK$ = PARK$ + "D.PNG" Else PARK$ = PARK$ + "N.PNG"
Locate , 11: Print "CURRENT PARK IS:"
Locate , 11: Print PARK$(L%(1, 13))
Locate , 11

'99 is the Generic Park
If _FileExists(".\parks\" + PARK$) Then Print "GRAPHIC EXISTS" Else Print "GRAPHIC NOT AVAILABLE": PARK$ = "99N.PNG"
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
SCANCODE% = 99
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 10

Call box0(4, 9, 20, 38, 2, 15, 1)
_MouseShow "Default"
'MMenuVert PARK$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
MAMenu PARK$(), SELECTION%, Start%, Count%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, 15, 1, 1, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then BO% = 1: Return
PARK$ = Right$(Str$(SELECTION%), Len(Str$(SELECTION%)) - 1)
If DN% = 0 Then PARK$ = PARK$ + "D.PNG" Else PARK$ = PARK$ + "N.PNG"
Color 15, 0
If _FileExists(".\parks\" + PARK$) Then L%(1, 13) = SELECTION%: S$(1) = PARK$(SELECTION%): Return
Locate 10, 8
Print "STADIUM GRAPHIC NOT AVAILABLE"
Locate , 8: Print "USE SELECTED PARK RATINGS WITH"
Locate , 8: Print "GENERIC DISPLAY (YN)"

3515 I$ = GetKeyPress$
If I$ <> "Y" Then 3510
L%(1, 13) = SELECTION%: S$(1) = PARK$(SELECTION%)
If DN% = 0 Then PARK$ = "99D.PNG" Else PARK$ = "99N.PNG"
Return


4500 ':*** PITCHING ROTATIONS ***
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



4516 ':AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

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

    4638 If UCase$(I$) = "Y" Then For I2 = 15 To 17: Locate I2, 59: Print Space$(17): Next: GoTo 4626

    4640 MG%(P9, I + 3) = I1: For I2 = 15 To 17: Locate I2, 59: Print Space$(18): Next
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

4690 ':COMPUTER SET ROTATION
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

5000 ':*** COMPUTER DETERMINE LINEUPS ***

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

5230 ': SORT HITTERS

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

5355 ':IF A PITCHER THEN 268

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
    teamItems$(NT) = Q2$(0): UV%(NT) = I1

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

_MouseShow "Default"
MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
T1(I) = UV%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."

300 Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q2$(0), 2 As Q2$(1), 2 As Q2$(2), 2 As Q2$(3), 2 As Q2$(4), 2 As Q2$(5), 2 As Q2$(6), 2 As Q2$(7), 2 As Q2$(8), 2 As Q2$(9), 2 As Q2$(10), 2 As Q2$(11), 2 As Q2$(12), 2 As Q2$(13), 4 As Q2$(14), 4297 As X$
For A = 0 To 22
    Field #1, 45 + A * 99 As X$, 15 As Q2$(A * 43 + 15), 2 As Q2$(A * 43 + 16), 2 As Q2$(A * 43 + 17), 2 As Q2$(A * 43 + 18), 2 As Q2$(A * 43 + 19), 2 As Q2$(A * 43 + 20), 2 As Q2$(A * 43 + 21), 2 As Q2$(A * 43 + 22), 2 As Q2$(A * 43 + 23), 2 As Q2$(A * 43 + 24), 2 As Q2$(A * 43 + 25), 2 As Q2$(A * 43 + 26), 4297 - 37 - A * 99 As X$
    Field #1, 82 + A * 99 As X$, 2 As Q2$(A * 43 + 27), 2 As Q2$(A * 43 + 28), 2 As Q2$(A * 43 + 29), 2 As Q2$(A * 43 + 30), 2 As Q2$(A * 43 + 31), 2 As Q2$(A * 43 + 32), 2 As Q2$(A * 43 + 33), 2 As Q2$(A * 43 + 34), 2 As Q2$(A * 43 + 35), 2 As Q2$(A * 43 + 36), 2 As Q2$(A * 43 + 37), 2 As Q2$(A * 43 + 38), 4297 - 61 - A * 99 As X$
    Field #1, 106 + A * 99 As X$, 2 As Q2$(A * 43 + 39), 2 As Q2$(A * 43 + 40), 2 As Q2$(A * 43 + 41), 2 As Q2$(A * 43 + 42), 2 As Q2$(A * 43 + 43), 2 As Q2$(A * 43 + 44), 2 As Q2$(A * 43 + 45), 2 As Q2$(A * 43 + 46), 2 As Q2$(A * 43 + 47), 2 As Q2$(A * 43 + 48), 2 As Q2$(A * 43 + 49), 2 As Q2$(A * 43 + 50), 4297 - 85 - A * 99 As X$
    Field #1, 130 + A * 99 As X$, 2 As Q2$(A * 43 + 51), 2 As Q2$(A * 43 + 52), 2 As Q2$(A * 43 + 53), 2 As Q2$(A * 43 + 54), 2 As Q2$(A * 43 + 55), 2 As Q2$(A * 43 + 56), 2 As Q2$(A * 43 + 57), 4297 - 99 - A * 99 As X$
Next

For A = 0 To 21
    Field #1, 2322 + A * 89 As X$, 15 As Q2$(A * 38 + 1004), 2 As Q2$(A * 38 + 1005), 2 As Q2$(A * 38 + 1006), 2 As Q2$(A * 38 + 1007), 2 As Q2$(A * 38 + 1008), 2 As Q2$(A * 38 + 1009), 2 As Q2$(A * 38 + 1010), 2 As Q2$(A * 38 + 1011), 2 As Q2$(A * 38 + 1012), 2 As Q2$(A * 38 + 1013), 2 As Q2$(A * 38 + 1014), 2 As Q2$(A * 38 + 1015), 2020 - 37 - A * 89 As X$
    Field #1, 2359 + A * 89 As X$, 2 As Q2$(A * 38 + 1016), 2 As Q2$(A * 38 + 1017), 2 As Q2$(A * 38 + 1018), 2 As Q2$(A * 38 + 1019), 2 As Q2$(A * 38 + 1020), 2 As Q2$(A * 38 + 1021), 2 As Q2$(A * 38 + 1022), 2 As Q2$(A * 38 + 1023), 2 As Q2$(A * 38 + 1024), 2 As Q2$(A * 38 + 1025), 2 As Q2$(A * 38 + 1026), 2 As Q2$(A * 38 + 1027), 2020 - 61 - A * 89 As X$
    Field #1, 2383 + A * 89 As X$, 2 As Q2$(A * 38 + 1028), 2 As Q2$(A * 38 + 1029), 2 As Q2$(A * 38 + 1030), 2 As Q2$(A * 38 + 1031), 2 As Q2$(A * 38 + 1032), 2 As Q2$(A * 38 + 1033), 2 As Q2$(A * 38 + 1034), 2 As Q2$(A * 38 + 1035), 2 As Q2$(A * 38 + 1036), 2 As Q2$(A * 38 + 1037), 2 As Q2$(A * 38 + 1038), 2 As Q2$(A * 38 + 1039), 2020 - 85 - A * 89 As X$
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

    ':check for 1934 season

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
Next: Next
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

119 ':

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
Sub COLCHK (P9, I)
    If B%(P9, I, 21) >= 98 Then Color 0, 7
End Sub


'------------------------------
'   DRAWBOXA Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOXA ()
    JJ = 5
    Locate 6, 8: Color L%(1, 11), L%(1, 12)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
End Sub


'------------------------------
'   GETWEATHER Subroutine
'------------------------------
' ...explanation...
Sub GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)

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
Sub INFO ()
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
Sub INITVARI ()
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
Sub LETTER (I$)

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
Sub LINSWAP (P9)

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
Sub NEWLINES (P9)

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
Sub POSITION (I)
    Locate I + 7, 59
End Sub


'------------------------------
'   POSSWAP Subroutine
'------------------------------
' ...explanation...
Sub POSSWAP (P9)
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
Sub SHOWBATTERS (P9)
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
Sub SHOWFIELD (P9)
    ':DISPLAY FIELDING AVERAGES
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
Sub SHOWSPLIT (P9)
    ':DISPLAY SPLIT STATS
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




Sub POSTGAME ()
    Dim DB$(20), E$(20), HR$(20), HT$(0 To 162), L$(0 To 162)
    Dim PL$(0 To 162), SA$(0 To 1), SB$(20), TR$(20), W$(0 To 162)

    Dim B6%(22, 21), D1%(1), DYS%(0 To 1, 0 To 21), E%(13), H%(0 To 162)
    Dim ING%(0 To 162), P0%(0 To 21, 0 To 41), SS%(0 To 22, 0 To 6), SX%(0 To 1), V%(0 To 162)

    Dim SPECIAL

    If Not _FileExists("EVENTS.BOX") Then 102
    Open "EVENTS.BOX" For Input As #1
    For X = 1 To 13
        Input #1, E%(X)
    Next
    Close 1

    102 If INNING% > E%(1) Then GoSub 7100: GoTo 7105
    If S6%(1, 0) < E%(2) Then GoSub 7100: GoTo 7105
    If S6%(0, 0) < E%(2) Then GoSub 7100: GoTo 7105
    If S6%(1, 0) > E%(3) Then GoSub 7100: GoTo 7105
    If S6%(0, 0) > E%(3) Then GoSub 7100: GoTo 7105
    If S6%(1, 1) > E%(5) Then GoSub 7100: GoTo 7105
    If S6%(0, 1) > E%(5) Then GoSub 7100: GoTo 7105
    If S6%(1, 1) < E%(4) Then GoSub 7100: GoTo 7105
    If S6%(0, 1) < E%(4) Then GoSub 7100: GoTo 7105
    For I = 0 To 1
        For I1 = 0 To 22
            If B8%(I, I1, 1) > E%(6) Then GoSub 7100: GoTo 7105
            If B8%(I, I1, 2) > E%(7) Then GoSub 7100: GoTo 7105
            If B8%(I, I1, 3) > E%(8) Then GoSub 7100: GoTo 7105
            If B8%(I, I1, 4) > E%(9) Then GoSub 7100: GoTo 7105
            If B8%(I, I1, 5) > E%(10) Then GoSub 7100: GoTo 7105
            If B8%(I, I1, 6) > E%(11) Then GoSub 7100: GoTo 7105
            If B8%(I, I1, 9) > E%(12) Then GoSub 7100: GoTo 7105
        Next
    Next
    For I = 0 To 1
        For I1 = 0 To 21
            If P8%(I, I1, 5) > E%(13) Then GoSub 7100: GoTo 7105
        Next
    Next
    GoTo 7105

    7100 Spec$ = D$(1) + "*.SPC"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%
    Count% = Count% + 1
    SPECIAL = 1
    Return

    7105 BK$ = "## # # ## # #  # # #  #  # # ## ##"
    For I = 0 To 1: For I1 = 0 To 21: If P8%(I, I1, 1) < 0 Then P8%(I, I1, 1) = 0
    Next I1: Next I
    For I = 0 To 1: For I1 = 0 To 21
        If P%(I, I1, 15) <> 1 Then 7250
            If P8%(I, I1, 0) >= 18 And P8%(I, I1, 3) <= 3 Then P8%(I, I1, 21) = 1

        7250 Next I1
    Next I

    For I = 0 To 1: For I1 = 0 To 22
        If B8%(I, I1, 11) = 0 Then 7257
            If B8%(I, I1, 11) = 1 Then E$(E) = B$(I, I1): GoTo 7256
            E$(E) = B$(I, I1) + Str$(B8%(I, I1, 11))

            7256 E$(E) = E$(E) + " (" + Right$(Str$(B5%(I, I1, 11)), (Len(Str$(B5%(I, I1, 11))) - 1)) + ")": E = E + 1

            7257 If B8%(I, I1, 4) = 0 Then 7261: '        ***CHECKING DOUBLES***
            If B8%(I, I1, 4) = 1 Then DB$(DB) = B$(I, I1): GoTo 7260
            DB$(DB) = B$(I, I1) + Str$(B8%(I, I1, 4))

            7260 DB$(DB) = DB$(DB) + " (" + Right$(Str$(B5%(I, I1, 4)), (Len(Str$(B5%(I, I1, 4))) - 1)) + ")": DB = DB + 1

            7261 If B8%(I, I1, 5) = 0 Then 7265: '        ***CHECKING TRIPLES***
            If B8%(I, I1, 5) = 1 Then TR$(TR) = B$(I, I1): GoTo 7264
            TR$(TR) = B$(I, I1) + Str$(B8%(I, I1, 5))

            7264 TR$(TR) = TR$(TR) + " (" + Right$(Str$(B5%(I, I1, 5)), (Len(Str$(B5%(I, I1, 5))) - 1)) + ")": TR = TR + 1

            7265 If B8%(I, I1, 6) = 0 Then 7269: '        ***CHECKING HOME RUNS***
            If B8%(I, I1, 6) = 1 Then HR$(HR) = B$(I, I1): GoTo 7268
            HR$(HR) = B$(I, I1) + Str$(B8%(I, I1, 6))

            7268 HR$(HR) = HR$(HR) + " (" + Right$(Str$(B5%(I, I1, 6)), (Len(Str$(B5%(I, I1, 6))) - 1)) + ")": HR = HR + 1

            7269 If B8%(I, I1, 9) = 0 Then 7272: '        ***CHECKING STOLEN BASES ***
            If B8%(I, I1, 9) = 1 Then SB$(SB) = B$(I, I1) + " (" + Right$(Str$(B5%(I, I1, 9)), (Len(Str$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1: GoTo 7272
            SB$(SB) = B$(I, I1) + Str$(B8%(I, I1, 9)) + " (" + Right$(Str$(B5%(I, I1, 9)), (Len(Str$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1

            7272 Next I1: Next I: For I = 0 To 1: For I1 = 0 To 21
            If P8%(I, I1, 34) = 0 Then 7277
            If P8%(I, I1, 34) = 1 Then E$(E) = P$(I, I1): GoTo 7276
            E$(E) = P$(I, I1) + Str$(P8%(I, I1, 34))

            7276 E$(E) = E$(E) + " (" + Right$(Str$(P5%(I, I1, 34)), (Len(Str$(P5%(I, I1, 34))) - 1)) + ")": E = E + 1

            7277 If P8%(I, I1, 27) = 0 Then 7281: '        ***CHECKING DOUBLES***
            If P8%(I, I1, 27) = 1 Then DB$(DB) = P$(I, I1): GoTo 7280
            DB$(DB) = P$(I, I1) + Str$(P8%(I, I1, 27))

            7280 DB$(DB) = DB$(DB) + " (" + Right$(Str$(P5%(I, I1, 27)), (Len(Str$(P5%(I, I1, 27))) - 1)) + ")": DB = DB + 1

            7281 If P8%(I, I1, 28) = 0 Then 7285: '        ***CHECKING TRIPLES***
            If P8%(I, I1, 28) = 1 Then TR$(TR) = P$(I, I1): GoTo 7284
            TR$(TR) = P$(I, I1) + Str$(P8%(I, I1, 28))

            7284 TR$(TR) = TR$(TR) + " (" + Right$(Str$(P5%(I, I1, 28)), (Len(Str$(P5%(I, I1, 28))) - 1)) + ")": TR = TR + 1

            7285 If P8%(I, I1, 29) = 0 Then 7289: '        ***CHECKING HOME RUNS***
            If P8%(I, I1, 29) = 1 Then HR$(HR) = P$(I, I1): GoTo 7288
            HR$(HR) = P$(I, I1) + Str$(P8%(I, I1, 29))

            7288 HR$(HR) = HR$(HR) + " (" + Right$(Str$(P5%(I, I1, 29)), (Len(Str$(P5%(I, I1, 29))) - 1)) + ")": HR = HR + 1

            7289 If P8%(I, I1, 32) = 0 Then 7292: '        ***CHECKING STOLEN BASES ***
            If P8%(I, I1, 32) = 1 Then SB$(SB) = P$(I, I1) + " (" + Right$(Str$(P5%(I, I1, 32)), (Len(Str$(P5%(I, I1, 32))) - 1)) + ")": SB = SB + 1: GoTo 7292
            SB$(SB) = P$(I, I1) + Str$(P8%(I, I1, 32)) + " (" + Right$(Str$(P5%(I, I1, 32)), (Len(Str$(P5%(I, I1, 32))) - 1)) + ")": SB = SB + 1

    7292 Next I1: Next I

    7106 GT$ = "2:": ATT = Int(Rnd(1) * 100): If ATT < 5 Or ATT > 60 Then 7106
    ATT = ATT * 1000

    7107 MIN = Int(Rnd(1) * 100): If MIN > 59 Then 7107
    If MIN < 10 Then MIN$ = "0" + Right$(Str$(MIN), 1): GT$ = GT$ + MIN$ Else GT$ = GT$ + Right$(Str$(MIN), 2)
    For I = 0 To 1: For I1 = 0 To 21: If P%(I, I1, 15) >= 98 Then P%(I, I1, 15) = 0
    Next I1: Next I
    For I = 0 To 1: For I1 = 0 To 22: If B%(I, I1, 21) >= 98 Then B%(I, I1, 21) = 0
        If B%(I, I1, 21) <> 0 Then B8%(I, I1, 14) = 1
    Next I1: Next I
    Cls: Print "PLEASE WAIT..."
    For I = 0 To 1
        M = 23
        L = 22

        7363 I6 = 0: I7 = 0

        7364 H1 = 0: H2 = 0: H3 = 0
        If X0%(I, 2, I6) <= X0%(I, 2, I6 + 1) Then 7370
        H1 = X0%(I, 2, I6): H2 = X0%(I, 1, I6): H3 = X0%(I, 0, I6)
        X0%(I, 0, I6) = X0%(I, 0, I6 + 1): X0%(I, 1, I6) = X0%(I, 1, I6 + 1): X0%(I, 2, I6) = X0%(I, 2, I6 + 1)
        X0%(I, 2, I6 + 1) = H1: X0%(I, 1, I6 + 1) = H2: X0%(I, 0, I6 + 1) = H3
        I7 = I6

        7370 I6 = I6 + 1
        If I6 > L Then 7373
        GoTo 7364

        7373 If I7 = 0 Then 7118
        L = I7 - 1: GoTo 7363

    7118 Next I
    GoTo 7293

    7119 If X = 1 Then NM$ = "TEMPBOX": GoTo 7121
    If AP% <> 1 Then 7120
    Open "GAME82" For Input As #1
    Input #1, FF$
    For I = 0 To 2
        Input #1, AP%(I)
    Next
    Close 1
    ' NM$ = "BOXSCORE." + MID$(STR$(AP%(0) + AP%(2)), 2, LEN(STR$(AP%(0) + AP%(2))))
    NM$ = "BOX" + Mid$(Str$(AP%(0) + AP%(2)), 2, Len(Str$(AP%(0) + AP%(2))))
    GoTo 7121

    7120 Cls: Print "ENTER 8-CHARACTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";: Input NM$: NM$ = NM$ + ".SPC"

    7121 Open D$(1) + NM$ For Output As #2
    If SPECIAL = 1 Then GoSub 7188
    GoSub 7183
    For I = 0 To 1
        Print #2, A$(I); Tab(18); "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
        For I3 = 0 To 22
            If X0%(I, 0, I3) = -1 Or X0%(I, 1, I3) = 1 Then 7127
            I1 = X0%(I, 0, I3)

            7124 Print #2, B$(I, I1); ", "; C$(X0%(I, 1, I3)); Tab(18);
            Print #2, Using BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);
            Print #2, "   ";
            If B5%(I, I1, 0) = 0 Or B5%(I, I1, 1) = 0 Then Print #2, " .000   ";: GoTo 7122
            If B5%(I, I1, 0) = B5%(I, I1, 1) Then Print #2, "1.000   ";: GoTo 7122
            Print #2, Using " .###   "; B5%(I, I1, 1) / B5%(I, I1, 0);

            7122 Print #2, Using "##  ###"; B5%(I, I1, 6); B5%(I, I1, 3)
            For I2 = 0 To 17
                T3%(I2) = T3%(I2) + B8%(I, I1, I2)
            Next I2

        7127 Next I3
        For I1 = 0 To 21: For I2 = 0 To 13: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23): Next
        For I2 = 14 To 17: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22): Next: Next
        For I1 = 1 To P6%(I)
            For I2 = 0 To 21
                If P%(I, I2, 15) <> I1 Then 7134
                Print #2, P$(I, I2); ", p";
                Print #2, Tab(18);
                Print #2, Using BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
                Print #2, "   ";
                If P5%(I, I2, 24) = 0 Or P5%(I, I2, 23) = 0 Then Print #2, " .000   ";: GoTo 7128
                If P5%(I, I2, 24) = P5%(I, I2, 23) Then Print #2, "1.000   ";: GoTo 7128
                Print #2, Using " .###   "; P5%(I, I2, 24) / P5%(I, I2, 23);

                7128 Print #2, Using "##  ###"; P5%(I, I2, 29); P5%(I, I2, 26)

        7134 Next: Next
        Print #2, "TOTALS"; Tab(17); T3%(0); Tab(22); T3%(1); Tab(27); T3%(4);
        Print #2, Tab(32); T3%(6); Tab(36); T3%(7); Tab(42);
        Print #2, T3%(10); Tab(46); T3%(16); Tab(50); T3%(17)
        Print #2, Tab(20); T3%(2); Tab(24); T3%(3); Tab(29);
        Print #2, T3%(5); Tab(34); T3%(8); Tab(39); T3%(9);
        Print #2, Tab(44); T3%(11)
        Print #2,: For I1 = 0 To 22: T3%(I1) = 0: Next I1
    Next I
    For I = 0 To 1: Print #2,: Print #2, A$(I); Tab(23); "       IP   H  R  ER BB SO    ERA   W  L  S  SO"
        For I1 = 1 To P6%(I)
            For I2 = 0 To 21
                If P%(I, I2, 15) <> I1 Then 7154
                I4 = 1 - I: Print #2, P$(I, I2); " ";
                If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then 7179

                7148 GoSub DSKBOX
                If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then 7180

                7149 If P8%(I, I2, 19) > 0 Then Print #2, "(BS,";: Print #2, Using "##"; P5%(I, I2, 19);: Print #2, ")";
                If P8%(I, I2, 22) > 0 Then Print #2, "(H,";: Print #2, Using "##"; P5%(I, I2, 22);: Print #2, ")";
                Print #2, Tab(29);: Print #2, Using "##"; Int(P8%(I, I2, 0) / 3);
                If P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3) > 0 Then Print #2, ".";: Print #2, Using "#"; P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3);
                Print #2, Tab(34);
                Print #2, Using "## ##  ## ## ##"; P8%(I, I2, 1); P8%(I, I2, 2); P8%(I, I2, 3); P8%(I, I2, 4); P8%(I, I2, 5);
                Print #2, "   ";
                If P5%(I, I2, 3) > 0 And P5%(I, I2, 0) = 0 Then Print #2, "99.99 ";: GoTo 7150
                If P5%(I, I2, 0) = 0 Then Print #2, "--.-- ";: GoTo 7150
                If P5%(I, I2, 3) = 0 Then Print #2, " 0.00 ";: GoTo 7150
                Print #2, Using "##.## "; P5%(I, I2, 3) / (P5%(I, I2, 0) / 3) * 9;

                7150 If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, Using "## "; P5%(I, I2, 6) + 1; Else Print #2, Using "## "; P5%(I, I2, 6);
                If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then Print #2, Using "## "; P5%(I, I2, 7) + 1; Else Print #2, Using "## "; P5%(I, I2, 7);
                If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, Using "## "; P5%(I, I2, 12) + 1; Else Print #2, Using "## "; P5%(I, I2, 12);
                Print #2, Using "###"; P5%(I, I2, 5)
                For I4 = 0 To 5
                    T3%(I4) = T3%(I4) + P8%(I, I2, I4)
                Next I4

            7154 Next I2
        Next I1
        Print #2, "TOTALS"; Tab(28);
        Print #2, Using "##"; Int(T3%(0) / 3);
        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then Print #2, ".";: Print #2, Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);
        Print #2, Tab(34);
        Print #2, Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)
        LYN% = CInt(T3%(0) / 3)
        For I1 = 0 To 5: T3%(I1) = 0: Next I1
    Next I
    Print #2,: Print #2, " E-";
    If E = 0 Then Print #2, "NONE": GoTo 7163
    For X1 = 0 To (E - 1): Print #2, E$(X1); ",";: Next: Print #2,

    7163 Print #2, "2B-";
    If DB = 0 Then Print #2, "NONE": GoTo 7166
    For X1 = 0 To (DB - 1): Print #2, DB$(X1); ",";: Next: Print #2,

    7166 Print #2, "3B-";
    If TR = 0 Then Print #2, "NONE": GoTo 7169
    For X1 = 0 To (TR - 1): Print #2, TR$(X1); ",";: Next: Print #2,

    7169 Print #2, "HR-";
    If HR = 0 Then Print #2, "NONE": GoTo 7172
    For X1 = 0 To (HR - 1): Print #2, HR$(X1); ",";: Next: Print #2,

    7172 Print #2, "SB-";
    If SB = 0 Then Print #2, "NONE": GoTo 7175
    For X1 = 0 To (SB - 1): Print #2, SB$(X1); ",";: Next: Print #2,

    7175 Print #2, "LOB-"; A$(0); Str$(L0%(0)); ","; A$(1); Str$(L0%(1))
    Print #2, "DP-"; A$(0); Str$(DP%(0)); ","; A$(1); Str$(DP%(1))
    Print #2, "UMPIRES: HP-"; U$(1); "; 1B-"; U$(2); "; 2B-"; U$(3); "; 3B-"; U$(4): Print #2, "T: "; GT$; " A: "; ATT
    Print #2,
    Print #2, "INJURIES-";
    If IJL% = 0 Then Print #2, "none": GoTo peject
    For X1 = 1 To IJL%: Print #2, IJ$(X1); ",";: Next: Print #2,
    peject:
    Print #2, "EJECTIONS-";
    If EJ% = 0 Then Print #2, "none": GoTo pweath
    For X1 = 1 To EJ%: Print #2, EJ$(X1); ",";: Next: Print #2,
    pweath:
    Print #2, "TEMPERATURE:"; TP%
    Print #2, "WIND: "; WI$
    Print #2, "SKY: "; SC$(SC%)
    Print #2, "PRECIPITATION: "; PC$(PC%)
    Print #2, Chr$(12): Close #2
    If SPECIAL = 1 Then FCopy D$(1) + NM$, D$(1) + "BOX" + Mid$(Str$(Count%), 2, Len(Str$(Count%))) + ".SPC", Space$(4096), ErrCode 'copy the file
    If X = 1 Then 7580
    GoTo 7328

    7179 Print #2, "(W,"; Right$(Str$(P5%(I, I2, 6) + 1), (Len(Str$(P5%(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7)), (Len(Str$(P5%(I, I2, 7))) - 1)); ")";: W$ = P$(I, I2): GoTo 7148

    7180 Print #2, "(L,"; Right$(Str$(P5%(I, I2, 6) + 0), (Len(Str$(P5%(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7) + 1), (Len(Str$(P5%(I, I2, 7) + 1)) - 1)); ")";: L$ = P$(I, I2): GoTo 7149
    Locate , 33: Print "-";: Locate , 34: Print Using "##"; S6%(I, 0);
    Locate , 37: Print Using "##"; S6%(I, 1);: Locate , 40: Print Using "#"; S6%(I, 2): Return

    7183 If INNING% < 9 Then GoSub 7185
    If INNING% = 10 And P = 0 Or INNING% = 9 Then GoSub 7187
    If INNING% = 10 And P = 1 Or INNING% > 10 And INNING% < 19 Or INNING% = 19 And P = 0 Then GoSub 7194
    If INNING% = 19 And P = 1 Or INNING% > 19 And INNING% <= 28 Then GoSub 7202
    Return

    7185 For I = 0 To 1
        Print #2, A$(I);
        For I2 = 1 To INNING%
            GoSub 7212
            If I = 0 Then Print #2, Using "##"; S%(I, I2);
            If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) = 0 Then Print #2, " X";
            If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = INNING% Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) > 0 Then Print #2, Using "##"; S%(I, I2);
            If I = 1 And I2 < INNING% Then Print #2, Using "##"; S%(I, I2);
        Next I2
        GoSub 7222
    Next I
    Return

    7187 For I = 0 To 1
        Print #2, A$(I);
        For I2 = 1 To 9
            GoSub 7212
            If I = 0 Then Print #2, Using "##"; S%(I, I2);
            If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) = 0 Then Print #2, " X";
            If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = 9 Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) > 0 Then Print #2, Using "##"; S%(I, I2);
            If I = 1 And I2 < 9 Then Print #2, Using "##"; S%(I, I2);
        Next I2
        GoSub 7222
    Next I
    Return

    7188 If INNING% > E%(1) Then Print #2, Using "MORE THAN ## INNINGS"; E%(1)
    If S6%(1, 0) < E%(2) Then Print #2, A$(1);: Print #2, Using " SCORED LESS THAN ## RUN(S)"; E%(2)
    If S6%(0, 0) < E%(2) Then Print #2, A$(0);: Print #2, Using " SCORED LESS THAN ## RUN(S)"; E%(2)
    If S6%(1, 0) > E%(3) Then Print #2, A$(1);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; E%(3)
    If S6%(0, 0) > E%(3) Then Print #2, A$(0);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; E%(3)
    If S6%(1, 1) > E%(5) Then Print #2, A$(1);: Print #2, Using " HAD MORE THAN ## HIT(S)"; E%(5)
    If S6%(0, 1) > E%(5) Then Print #2, A$(0);: Print #2, Using " HAD MORE THAN ## HIT(S)"; E%(5)
    If S6%(1, 1) < E%(4) Then Print #2, A$(1);: Print #2, Using " HAD LESS THAN ## HIT(S)"; E%(4)
    If S6%(0, 1) < E%(4) Then Print #2, A$(0);: Print #2, Using " HAD LESS THAN ## HIT(S)S"; E%(4)
    For I = 0 To 1
        For I1 = 0 To 22
            If B8%(I, I1, 1) > E%(6) Then Print #2, B$(I, I1);: Print #2, Using " HAD MORE THAN ## HIT(S)"; E%(6)
            If B8%(I, I1, 2) > E%(7) Then Print #2, B$(I, I1);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; E%(7)
            If B8%(I, I1, 3) > E%(8) Then Print #2, B$(I, I1);: Print #2, Using " HAD MORE THAN ## RBI(S)"; E%(8)
            If B8%(I, I1, 4) > E%(9) Then Print #2, B$(I, I1);: Print #2, Using " HIT MORE THAN ## DOUBLES(S)"; E%(9)
            If B8%(I, I1, 5) > E%(10) Then Print #2, B$(I, I1);: Print #2, Using " HIT MORE THAN ## TRIPLE(S)"; E%(10)
            If B8%(I, I1, 6) > E%(11) Then Print #2, B$(I, I1);: Print #2, Using " HOMERED MORE THAN ## TIME(S)"; E%(11)
            If B8%(I, I1, 9) > E%(12) Then Print #2, B$(I, I1);: Print #2, Using " STOLE MORE THAN ## BASE(S)"; E%(12)
        Next
    Next
    For I = 0 To 1
        For I1 = 0 To 21
            If P8%(I, I1, 5) > E%(13) Then Print #2, P$(I, I1);: Print #2, Using " STRUCK OUT MORE THAN ## BATTER(S)"; E%(13)
        Next
    Next
    Return

    7194 For I = 0 To 1
        Print #2, A$(I);
        For I2 = 1 To 9
            GoSub 7212
            Print #2, Using "##"; S%(I, I2);
        Next I2
        Print #2,
    Next I
    Print #2,
    If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1
    For I = 0 To 1
        For I2 = 10 To INNING%
            GoSub 7212: Print #2, Using "##"; S%(I, I2);
        Next I2
        GoSub 7222
    Next I
    Return

    7202 For I = 0 To 1
        Print #2, A$(I);
        For I2 = 1 To 9
            GoSub 7212
            Print #2, Using "##"; S%(I, I2);
    Next I2: Print #2,: Next I
    Print #2,
    If INNING% = 28 Or INNING% < 28 And P = 0 Then INNING% = INNING% - 1
    For I = 0 To 1
        For I2 = 10 To 18
            GoSub 7212
            Print #2, Using "##"; S%(I, I2);
    Next I2: Print #2,: Next I
    For I = 0 To 1
        For I2 = 19 To INNING%
            GoSub 7212
            Print #2, Using "##"; S%(I, I2);
        Next I2
        GoSub 7222
    Next I
    Print #2,: Return

    7212 If I2 < 4 Then Print #2, Tab(((2 * I2) + 18));
    If I2 > 3 And I2 < 7 Then Print #2, Tab(((2 * I2) + 19));
    If I2 > 6 And I2 < 10 Then Print #2, Tab(((2 * I2) + 20));
    If I2 > 9 And I2 < 13 Then Print #2, Tab(((2 * (I2 - 9)) + 18));
    If I2 > 12 And I2 < 16 Then Print #2, Tab(((2 * (I2 - 9)) + 19));
    If I2 > 15 And I2 < 19 Then Print #2, Tab(((2 * (I2 - 9)) + 20));
    If I2 > 18 And I2 < 22 Then Print #2, Tab(((2 * (I2 - 18)) + 18));
    If I2 > 21 And I2 < 25 Then Print #2, Tab(((2 * (I2 - 18)) + 19));
    If I2 > 24 And I2 < 28 Then Print #2, Tab(((2 * (I2 - 18)) + 20));
    Return

    7222 Print #2, Tab(40); "-";
    Print #2, Tab(41);
    Print #2, Using "##"; S6%(I, 0);
    Print #2, Tab(44);: Print #2, Using "##"; S6%(I, 1);
    Print #2, Tab(47);: Print #2, Using "#"; S6%(I, 2)
    Return

    7227 If AP% = 1 And U6 = 2 Then Return
    Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
    7229 I$ = GetKeyPress$
    Return

    7293 I3 = 0: If S6%(1, 0) - S6%(0, 0) > 0 Then I3 = 1
    If P2%(I3) <> P1%(I3) And S1%(I3) = -1 And P8%(I3, P1%(I3), 0) >= 9 And P8%(I3, P1%(I3), 0) <= 12 And P6%(I3) > 0 And P8%(I3, P1%(I3), 3) < 3 Then S1%(I3) = P1%(I3)
    If P2%(I3) = S1%(I3) Then S1%(I3) = -1
    Cls: Print Tab(32);: Color 12: Print "BOXSCORE OPTIONS": Print
    Color 14: Print "(0) ";: Color 15: Print "SEND BOXSCORE TO THE SCREEN": Print
    Color 14: Print "(1) ";: Color 15: Print "SEND BOXSCORE TO THE PRINTER": Print
    Color 14: Print "(2) ";: Color 15: Print "SEND BOXSCORE TO A TEXT FILE"
    If AP% = 1 And U6 = 2 Then X = 2: GoTo 7339
    7337 I$ = GetKeyPress$
    X = Val(I$)

    7339 If X = 1 Or X = 2 Then 7119
    X = 0
    GoSub 7491: GoSub 7344: GoSub 7342: GoSub 7342
    For I = 0 To 1
        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0
        Locate , 18: Print "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
        Color 15, 0
        For I3 = 0 To 22
            If X0%(I, 0, I3) = -1 Or X0%(I, 1, I3) = 1 Then 7302
            I1 = X0%(I, 0, I3)

            7298 Print B$(I, I1); ", "; C$(X0%(I, 1, I3));
            Locate , 18
            Print Using BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);
            Print "   ";
            If B5%(I, I1, 0) = 0 Or B5%(I, I1, 1) = 0 Then Print " .000   ";: GoTo 735
            If B5%(I, I1, 0) = B5%(I, I1, 1) Then Print "1.000   ";: GoTo 735
            Print Using " .###   "; B5%(I, I1, 1) / B5%(I, I1, 0);

            735 Print Using "##  ###"; B5%(I, I1, 6); B5%(I, I1, 3)
            For I2 = 0 To 17
                T3%(I2) = T3%(I2) + B8%(I, I1, I2)
            Next I2

        7302 Next I3
        For I1 = 0 To 21: For I2 = 0 To 13: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23): Next
        For I2 = 14 To 17: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22): Next: Next
        For I1 = 1 To P6%(I): For I2 = 0 To 21
            If P%(I, I2, 15) <> I1 Then 7305
                GoTo 7310

        7305 Next: Next: Color 14, 0: Print "TOTALS";: Color 15, 0:
        Locate , 17: Print T3%(0);: Locate , 22: Print T3%(1);: Locate , 27: Print T3%(4);
        Locate , 32: Print T3%(6);: Locate , 36: Print T3%(7);: Locate , 42: Print T3%(10);: Locate , 46: Print T3%(16);: Locate , 50: Print T3%(17)
        Locate , 20: Print T3%(2);: Locate , 24: Print T3%(3);: Locate , 29: Print T3%(5);: Locate , 34: Print T3%(8);: Locate , 39: Print T3%(9);: Locate , 44: Print T3%(11)
        Print: GoSub 7344: GoSub 7342: For I1 = 0 To 22: T3%(I1) = 0: Next I1
    Next I: GoTo 7313

    7310 Print P$(I, I2); ", p";: Locate , 18
    Print Using BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
    Print "   ";
    If P5%(I, I2, 24) = 0 Or P5%(I, I2, 23) = 0 Then Print " .000   ";: GoTo 736
    If P5%(I, I2, 24) = P5%(I, I2, 23) Then Print "1.000   ";: GoTo 736
    Print Using " .###   "; P5%(I, I2, 24) / P5%(I, I2, 23);

    736 Print Using "##  ###"; P5%(I, I2, 29); P5%(I, I2, 26)
    GoTo 7305

    7313 For I = 0 To 1
        GoSub 7342
        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0: Locate , 23
        Print "       IP   H  R  ER BB SO    ERA   W  L  S  SO": Color 15, 0
        For I1 = 1 To P6%(I)
            For I2 = 0 To 21
                If P%(I, I2, 15) <> I1 Then 7321
                I4 = 1 - I: Locate , 1
                Print P$(I, I2); " ";
                If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then 7346

                7316 GoSub 7348: If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then 7347

                7317 If P8%(I, I2, 19) > 0 Then Print "(BS,";: Print Using "##"; P5%(I, I2, 19);: Print ")";
                If P8%(I, I2, 22) > 0 Then Print "(H,";: Print Using "##"; P5%(I, I2, 22);: Print ")";
                Locate , 29: Print Using "##"; Int(P8%(I, I2, 0) / 3);
                If P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3);
                Locate , 34
                Print Using "## ##  ## ## ##"; P8%(I, I2, 1); P8%(I, I2, 2); P8%(I, I2, 3); P8%(I, I2, 4); P8%(I, I2, 5);
                Print "   ";
                If P5%(I, I2, 3) > 0 And P5%(I, I2, 0) = 0 Then Print "99.99 ";: GoTo 737
                If P5%(I, I2, 0) = 0 Then Print "--.-- ";: GoTo 737
                If P5%(I, I2, 3) = 0 Then Print " 0.00 ";: GoTo 737
                Print Using "##.## "; P5%(I, I2, 3) / (P5%(I, I2, 0) / 3) * 9;

                737 If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print Using "## "; P5%(I, I2, 6) + 1; Else Print Using "## "; P5%(I, I2, 6);
                If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then Print Using "## "; P5%(I, I2, 7) + 1; Else Print Using "## "; P5%(I, I2, 7);
                If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print Using "## "; P5%(I, I2, 12) + 1; Else Print Using "## "; P5%(I, I2, 12);
                Print Using "###"; P5%(I, I2, 5)
                For I4 = 0 To 5
                    T3%(I4) = T3%(I4) + P8%(I, I2, I4)
                Next I4

            7321 Next I2
        Next I1
        Color 14, 0: Print "TOTALS";: Color 15, 0: Locate , 29: If (T3%(0) / 3) < 10 Then Print Int(T3%(0) / 3); Else Locate (P6%(I) + 2), 28: Print Int(T3%(0) / 3);
        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);
        Locate , 34
        Print Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)
        Print
        GoSub 7344: GoSub 7342
        LYN% = CInt(T3%(0) / 3): For I1 = 0 To 5: T3%(I1) = 0: Next I1
    Next I
    GoSub 7340: Cls: GoSub 7375: GoSub 7227

    7328 Color 15, 0: Cls: J = 1: Call DRAWBOX(J): Locate 9, 28: Print "DISPLAY STATS AGAIN (YN)";: Color 15, 0: If AP% = 1 And U6 = 2 Then 7331
    7329 I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 7293

    7331 GoSub 7408
    Color 15, 0
    Cls: J = 2: Call DRAWBOX(J)

    7334 Run "HELLO.EXE"

    7340 If X = 1 Then X = 0
    Return

    7342 If X = 0 Then Cls
    Return

    7344 If X = 0 Then GoSub 7227
    Return

    7346 Print "(W,"; Right$(Str$(P5%(I, I2, 6) + 1), (Len(Str$(P5%(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7)), (Len(Str$(P5%(I, I2, 7))) - 1)); ")";: W$ = P$(I, I2): GoTo 7316

    7347 Print "(L,"; Right$(Str$(P5%(I, I2, 6) + 0), (Len(Str$(P5%(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7) + 1), (Len(Str$(P5%(I, I2, 7) + 1)) - 1)); ")";: L$ = P$(I, I2): GoTo 7317

    7348 If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print "(S,"; Right$(Str$(P5%(I, I2, 12) + 1), (Len(Str$(P5%(I, I2, 12) + 1)) - 1)); ")";
    Return
    DSKBOX:
    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, "(S,"; Right$(Str$(P5%(I, I2, 12) + 1), (Len(Str$(P5%(I, I2, 12) + 1)) - 1)); ")";
    Return

    7375 Color 14, 0: Print " E-";
    Color 15, 0
    If E = 0 Then Print "none": GoTo 7378
    For X = 0 To (E - 1): Print E$(X); ",";: Next: Print

    7378 Color 14, 0: Print "2B-";: Color 15, 0
    If DB = 0 Then Print "none": GoTo 7381
    For X = 0 To (DB - 1): Print DB$(X); ",";: Next: Print

    7381 Color 14, 0: Print "3B-";: Color 15, 0
    If TR = 0 Then Print "none": GoTo 7384
    For X = 0 To (TR - 1): Print TR$(X); ",";: Next: Print

    7384 Color 14, 0: Print "HR-";: Color 15, 0
    If HR = 0 Then Print "none": GoTo 7387
    For X = 0 To (HR - 1): Print HR$(X); ",";: Next: Print

    7387 Color 14, 0: Print "SB-";: Color 15, 0
    If SB = 0 Then Print "none": GoTo 7390
    For X = 0 To (SB - 1): Print SB$(X); ",";: Next: Print

    7390 Color 14, 0: Print "LOB-";: Color 15, 0: Print A$(0); Str$(L0%(0)); ","; A$(1); Str$(L0%(1))
    Print "DP-";: Color 15, 0: Print A$(0); Str$(DP%(0)); ","; A$(1); Str$(DP%(1))
    Color 14, 0: Print "INJURIES-";: Color 15, 0
    If IJL% = 0 Then Print "none": GoTo eject
    For X = 1 To IJL%: Print IJ$(X); ",";: Next: Print
    eject:
    Color 14, 0: Print "EJECTIONS-";: Color 15, 0
    If EJ% = 0 Then Print "none": GoTo 7392
    For X = 1 To EJ%: Print EJ$(X); ",";: Next: Print

    7392 Color 14, 0: Print "UMPIRES: ";: Color 15, 0: Print "HP-"; U$(1); "; 1B-"; U$(2); "; 2B-"; U$(3); "; 3B-"; U$(4): Print "T: "; GT$; " A: "; ATT
    Print
    Color 14, 0: Print "TEMPERATURE:";: Color 15, 0: Print TP%
    Color 14, 0: Print "WIND: ";: Color 15, 0: Print WI$
    Color 14, 0: Print "SKY: ";: Color 15, 0: Print SC$(SC%)
    Color 14, 0: Print "PRECIPITATION: ";: Color 15, 0: Print PC$(PC%)
    Return

    7408 Cls
    Color 15, 0: Cls: J = 1: Call DRAWBOX(J): Locate 9, 28: Print "SAVE STATS FOR EITHER TEAM (YN)";: Color 15, 0: If AP% = 1 Then BG% = 0: BN% = 1: GoTo 7411
    7410 I$ = GetKeyPress$
    If UCase$(I$) <> "Y" Then X1 = 3: Return
    Cls: Color 12: Print Tab(33); "COMPILE STATS": Print
    Color 14: Print "(0) ";: Color 15: Print "BOTH TEAMS": Print
    Color 14: Print "(1) ";: Color L%(0, 11), L%(0, 12): Print A$(0); " ONLY": Color , 0: Print
    Color 14: Print "(2) ";: Color L%(1, 11), L%(1, 12): Print A$(1); " ONLY": Color 15, 0: Print
    7409 I$ = GetKeyPress$
    X1 = Val(I$)
    If X1 < 0 Or X1 > 2 Then 7409

    7411 'SX%(0) = -1: SX%(1) = -1
    For PX = 0 To 1
        For I = 0 To 21
            If P%(PX, I, 15) = 1 Then SX%(PX) = I: GoTo 7395
        Next I

    7395 Next PX: GoTo 7600

    7491 Cls
    If INNING% < 9 Then GoSub 7493
    If INNING% = 10 And P = 0 Or INNING% <= 9 Then GoSub 7495
    If INNING% = 10 And P = 1 Or INNING% > 10 And INNING% < 19 Or INNING% = 19 And P = 0 Then GoSub 7504
    If INNING% = 19 And P = 1 Or INNING% > 19 And INNING% <= 28 Then GoSub 7512
    Return

    7493 For I = 0 To 1
        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0
        For I2 = 1 To INNING%
            GoSub 7522
            If I = 0 Then Print Using "##"; S%(I, I2);
            If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) = 0 Then Print , " X";
            If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = INNING% Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) > 0 Then Print Using "##"; S%(I, I2);
            If I = 1 And I2 < INNING% Then Print Using "##"; S%(I, I2);
        Next I2
        GoSub 7532
    Next I
    Return

    7495 For I = 0 To 1
        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0
        For I2 = 1 To 9
            GoSub 7522
            If I = 0 Then Print Using "##"; S%(I, I2);
            If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) = 0 Then Print " X";
            If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = 9 Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) > 0 Then Print Using "##"; S%(I, I2);
            If I = 1 And I2 < 9 Then Print Using "##"; S%(I, I2);
        Next I2
        GoSub 7532
    Next I
    Return

    7504 For I = 0 To 1
        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0
        For I2 = 1 To 9
            GoSub 7522
            Print Using "##"; S%(I, I2);
        Next I2
        Print
    Next I
    Print
    If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1
    For I = 0 To 1
        For I2 = 10 To INNING%
            GoSub 7522
            Print Using "##"; S%(I, I2);
        Next I2
        GoSub 7532
    Next I
    Return

    7512 For I = 0 To 1
        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0
        For I2 = 1 To 9
            GoSub 7522
            Print Using "##"; S%(I, I2);
        Next I2
        Print
    Next I
    Print
    If INNING% = 28 Or INNING% < 28 And P = 0 Then INNING% = INNING% - 1
    For I = 0 To 1
        For I2 = 10 To 18
            GoSub 7522
            Print Using "##"; S%(I, I2);
        Next I2
        Print
    Next I
    Print
    For I = 0 To 1
        For I2 = 19 To INNING%: GoSub 7522: Print Using "##"; S%(I, I2);: Next I2
        GoSub 7532
    Next I
    Return

    7522 If I2 < 4 Then Locate , ((2 * I2) + 18)
    If I2 > 3 And I2 < 7 Then Locate , ((2 * I2) + 19)
    If I2 > 6 And I2 < 10 Then Locate , ((2 * I2) + 20)
    If I2 > 9 And I2 < 13 Then Locate , ((2 * (I2 - 9)) + 18)
    If I2 > 12 And I2 < 16 Then Locate , ((2 * (I2 - 9)) + 19)
    If I2 > 15 And I2 < 19 Then Locate , ((2 * (I2 - 9)) + 20)
    If I2 > 18 And I2 < 22 Then Locate , ((2 * (I2 - 18)) + 18)
    If I2 > 21 And I2 < 25 Then Locate , ((2 * (I2 - 18)) + 19)
    If I2 > 24 And I2 < 28 Then Locate , ((2 * (I2 - 18)) + 20)
    Return

    7532 Locate , 40: Print "-";: Locate , 41: Print Using "##"; S6%(I, 0);
    Locate , 44: Print Using "##"; S6%(I, 1);: Locate , 47: Print Using "#"; S6%(I, 2)
    Return

    7580 Open D$(1) + NM$ For Input As #2

    7581 Line Input #2, X$
    If Left$(X$, 1) = Chr$(12) Then LPrint X$: Close #2: GoTo 7328
    LPrint X$: GoTo 7581

    7600 FILENAME$ = "GAME82"
    If Not _FileExists(FILENAME$) Then 7630
    Open "GAME82" For Input As #2
    Input #2, FF$
    For I = 0 To 2: Input #2, AP%(I): Next: Close 2
    Open D$(0) + FF$ For Random As #1 Len = 48
    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
    Next
    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
    NG%(13) = 1
    For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(X)): Next
    For X = 0 To 1: LSet Q$(20 + X) = YN$(X): Next
    LSet Q$(22) = MKI$(S6%(0, 0)): LSet Q$(23) = MKI$(S6%(1, 0))
    Put #1, AP%(0) + AP%(2)
    Close 1
    AP%(2) = AP%(2) + 1
    AP% = 1: If AP%(1) < AP%(0) + AP%(2) Then Kill "GAME82": NM = 1: GoTo 7630
    Open "GAME82" For Output As #2
    Print #2, FF$
    For I = 0 To 2: Print #2, AP%(I): Next: Close 2

    7630 Cls
    Q = 0
    W2% = 0: L2% = 0

    7662 C% = 0: GoSub 7834: If C% = 1 Then 7770
    SA$(Q) = A$(Q): GoSub 7862
    F$ = Mid$(SA$(Q), 4, QQ): R$ = F$ + "." + Left$(SA$(Q), 2)
    FILENAME$ = D$(1) + R$ + "D"
    If Not _FileExists(FILENAME$) Then 7663
    Open D$(1) + R$ + "D" For Input As #1
    For I1 = 0 To 22
        Input #1, INJ%(Q, I1)
    Next
    For I1 = 0 To 21
        Input #1, DYS%(Q, I1)
    Next
    Close 1

    7663 If IJ% = 1 And DR% = 1 Then 7666
    If IJ% = 1 Then 7664
    For I1 = 0 To 22
        If INJ%(Q, I1) > 0 Then INJ%(Q, I1) = INJ%(Q, I1) - 1
        If B%(Q, I1, 32) > 0 Then INJ%(Q, I1) = B%(Q, I1, 32)
    Next

    7664 If DR% = 1 Then 7665
    For I1 = 0 To 21
        If DYS%(Q, I1) > 0 And DYS%(Q, I1) < 90 Then DYS%(Q, I1) = DYS%(Q, I1) - 1
        If P8%(Q, I1, 0) = 0 And DYS%(Q, I1) > 0 And DYS%(Q, I1) < 90 Then 7667
        If P8%(Q, I1, 0) = 0 And (DYS%(Q, I1) = 0 Or DYS%(Q, I1) = 1 Or DYS%(Q, I1) = 91 Or DYS%(Q, I1) = 92) Then DYS%(Q, I1) = 0: GoTo 7667
        If P8%(Q, I1, 0) > 0 And P8%(Q, I1, 0) <= 3 And DYS%(Q, I1) = 0 Then DYS%(Q, I1) = 91: GoTo 7667
        If P8%(Q, I1, 0) > 0 And P8%(Q, I1, 0) <= 3 And DYS%(Q, I1) = 91 Then DYS%(Q, I1) = 911: GoTo 7667
        If P8%(Q, I1, 0) > 0 And P8%(Q, I1, 0) <= 3 And DYS%(Q, I1) = 911 Then DYS%(Q, I1) = 1: GoTo 7667
        If P8%(Q, I1, 0) > 0 And P8%(Q, I1, 0) <= 3 And DYS%(Q, I1) = 92 Then DYS%(Q, I1) = 1: GoTo 7667
        If P8%(Q, I1, 0) > 0 And P8%(Q, I1, 0) <= 3 And DYS%(Q, I1) > 0 Then DYS%(Q, I1) = DYS%(Q, I1) + 1: GoTo 7667
        If P8%(Q, I1, 0) > 3 And P8%(Q, I1, 0) <= 6 And DYS%(Q, I1) = 0 Then DYS%(Q, I1) = 92: GoTo 7667
        If P8%(Q, I1, 0) > 3 And P8%(Q, I1, 0) <= 6 And DYS%(Q, I1) = 91 Then DYS%(Q, I1) = 1: GoTo 7667
        If P8%(Q, I1, 0) > 3 And P8%(Q, I1, 0) <= 6 And DYS%(Q, I1) = 92 Then DYS%(Q, I1) = 1: GoTo 7667
        If P8%(Q, I1, 0) > 3 And P8%(Q, I1, 0) <= 6 And DYS%(Q, I1) = 911 Then DYS%(Q, I1) = 1: GoTo 7667
        If P8%(Q, I1, 0) > 3 And P8%(Q, I1, 0) <= 6 And DYS%(Q, I1) > 0 Then DYS%(Q, I1) = DYS%(Q, I1) + 1: GoTo 7667
        If P8%(Q, I1, 0) > 6 Then DYS%(Q, I1) = Int(P8%(Q, I1, 0) / 3) + 1 - 2
        If MG%(Q, 1) <= 0 Then DYS%(Q, I1) = 3: GoTo 7667
        If DYS%(Q, I1) > MG%(Q, 1) - 1 Then DYS%(Q, I1) = MG%(Q, 1) - 1
        If DYS%(Q, I1) < 0 Then DYS%(Q, I1) = 0

    7667 Next

    7665 Open D$(1) + R$ + "D" For Output As #1
    For I1 = 0 To 22
        Print #1, INJ%(Q, I1)
    Next
    For I1 = 0 To 21
        Print #1, DYS%(Q, I1)
    Next
    Close 1

    7666 FILENAME$ = D$(1) + R$
    If Not _FileExists(FILENAME$) Then 7690
    Open D$(1) + R$ For Input As #1
    Input #1, A$(Q): Input #1, W2%: Input #1, L2%
    For I = 0 To 22: Input #1, B$(Q, I): Input #1, B2%(Q, I): Next I
    For I = 0 To 21: Input #1, P$(Q, I): Input #1, P3%(Q, I): Next I
    For I = 0 To 22: For I1 = 0 To 21: Input #1, B6%(I, I1): Next I1: Next I
    For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I
    Input #1, D0%(Q): Input #1, D1%(Q): For I = 0 To 21: Input #1, T%(I): Next I: For I = 0 To 22: Input #1, P4%(I): Next I
    Close #1

    7690 For I = 0 To 22
        B2%(Q, I) = B%(Q, I, 4)
        For I1 = 0 To 11
            B6%(I, I1) = B6%(I, I1) + B8%(Q, I, I1): T%(I1) = T%(I1) + B8%(Q, I, I1)
        Next I1
        If B8%(Q, I, 0) > 0 And B8%(Q, I, 1) = 0 Then B6%(I, 12) = 0: GoTo 7698
        If B8%(Q, I, 0) > 0 And B8%(Q, I, 1) > 0 Then B6%(I, 12) = B6%(I, 12) + 1

        7698 If B8%(Q, I, 14) > 0 Then B6%(I, 14) = B6%(I, 14) + 1
        For I1 = 15 To 21
            B6%(I, I1) = B6%(I, I1) + B8%(Q, I, I1): T%(I1) = T%(I1) + B8%(Q, I, I1)
        Next I1
        If B6%(I, 12) >= B6%(I, 13) Then B6%(I, 13) = B6%(I, 12)
    Next I

    For I = 0 To 21
        For I1 = 23 To 34
            P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): T%(I1 - 23) = T%(I1 - 23) + P8%(Q, I, I1)
        Next I1
        If P8%(Q, I, 23) > 0 And P8%(Q, I, 24) = 0 Then P0%(I, 35) = 0: GoTo 7705
        If P8%(Q, I, 23) > 0 And P8%(Q, I, 24) > 0 Then P0%(I, 35) = P0%(I, 35) + 1

        7705 For I1 = 37 To 41
            P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): T%(I1 - 22) = T%(I1 - 22) + P8%(Q, I, I1)
        Next I1
        If P0%(I, 35) >= P0%(I, 36) Then P0%(I, 36) = P0%(I, 35)
    Next I

    T%(12) = T%(12) + L0%(Q)
    INNING% = 0

    For I = 0 To 21
        INNING% = INNING% + P8%(Q, I, 0)
        P3%(Q, I) = P%(Q, I, 6)
        For I1 = 0 To 5
            P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): P4%(I1) = P4%(I1) + P8%(Q, I, I1)
        Next I1
        For I1 = 13 To 22: P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1): P4%(I1) = P4%(I1) + P8%(Q, I, I1)
        Next I1
    Next I

    If S6%(Q, 0) - S6%(1 - Q, 0) > 0 Then P0%(P2%(Q), 6) = P0%(P2%(Q), 6) + 1
    If S6%(Q, 0) - S6%(1 - Q, 0) < 0 Then P0%(P2%(Q), 7) = P0%(P2%(Q), 7) + 1
    If S6%(Q, 0) - S6%(1 - Q, 0) > 0 And S1%(Q) >= 0 Then P0%(S1%(Q), 12) = P0%(S1%(Q), 12) + 1: P4%(12) = P4%(12) + 1

    For I = 0 To 21
        If P%(Q, I, 15) <> 0 Then P0%(I, 8) = P0%(I, 8) + 1: P4%(8) = P4%(8) + 1
        If INNING% = P8%(Q, I, 0) Then P0%(I, 10) = P0%(I, 10) + 1: P4%(10) = P4%(10) + 1
        If INNING% = P8%(Q, I, 0) And S6%(1 - Q, 0) = 0 Then P0%(I, 11) = P0%(I, 11) + 1: P4%(11) = P4%(11) + 1
    Next I

    P0%(SX%(Q), 9) = P0%(SX%(Q), 9) + 1: P4%(9) = P4%(9) + 1
    D0%(Q) = D0%(Q) + D3%(Q): D1%(Q) = D1%(Q) + D3%(1 - Q)
    If S6%(Q, 0) - S6%(1 - Q, 0) > 0 Then W2% = W2% + 1
    If S6%(Q, 0) - S6%(1 - Q, 0) < 0 Then L2% = L2% + 1
    Color 15, 0
    Print ">>>>>": Print: Print: Print "THE STATS ARE BEING SAVED UNDER": Print "THE FOLLOWING NAME ::: ";
    Color L%(Q, 11), L%(Q, 12): Print R$: Color 15, 0
    Open D$(1) + R$ For Output As #1
    Print #1, A$(Q): Print #1, W2%: Print #1, L2%

    1 For I = 0 To 22: Print #1, B$(Q, I): Print #1, B2%(Q, I): Next I

    2 For I = 0 To 21: Print #1, P$(Q, I): Print #1, P3%(Q, I): Next I

    3 For I = 0 To 22: For I1 = 0 To 21: Print #1, B6%(I, I1): Next I1: Next I

    4 For I = 0 To 21: For I1 = 0 To 41: Print #1, P0%(I, I1): Next I1: Next I

    5 Print #1, D0%(Q): Print #1, D1%(Q)

    6 For I = 0 To 21: Print #1, T%(I): Next I

    7 For I = 0 To 22: Print #1, P4%(I): Next I

    8 Close #1: GoTo 7766

    9 Print "<<<<<"

    7766 GoSub 7780
    GoSub 7850: GoSub 7856

    7770 Q = Q + 1: If Q < 2 Then 7662
    If AP% = 1 And NM <> 1 Then Run "LOADER.EXE"

    7777 Run "HELLO.EXE"

    7780 FILENAME$ = D$(1) + R$ + "R"
    If Not _FileExists(FILENAME$) Then 7782
    Open D$(1) + R$ + "R" For Input As #1
    For I = 0 To 22
        For I1 = 0 To 6
            Input #1, SS%(I, I1)
        Next
    Next
    Close 1

    7782 For I = 0 To 22
        For I1 = 0 To 6
            SS%(I, I1) = SS%(I, I1) + CSS%(Q, I, I1)
        Next
    Next
    Open D$(1) + R$ + "R" For Output As #1
    For I = 0 To 22
        For I1 = 0 To 6
            Print #1, SS%(I, I1)
        Next
    Next
    Close 1
    Y% = 0
    FILENAME$ = D$(1) + R$ + "S"
    If Not _FileExists(FILENAME$) Then 7800
    Open D$(1) + R$ + "S" For Input As #1
    Input #1, Y%

    10 For I = 1 To 162: Input #1, H%(I): Next I

    11 For I = 1 To 162: Input #1, HT$(I): Next I

    12 For I = 1 To 162: Input #1, V%(I): Next I

    13 For I = 1 To 162: Input #1, PL$(I): Next I

    14 For I = 1 To 162: Input #1, W$(I): Next I

    15 For I = 1 To 162: Input #1, L$(I): Next I

    16 For I = 1 To 162: Input #1, ING%(I): Next I
    Close #1: GoTo 7806

    7800 For I = 1 To 162
        H%(I) = 999: HT$(I) = "XXXXXXX"
        V%(I) = 999: PL$(I) = "X"
        W$(I) = "LYNN": L$(I) = "SUMLER"
        ING%(I) = 999
    Next I

    7806 Y% = Y% + 1

    7807 H%(Y%) = S6%(Q, 0)

    7808 HT$(Y%) = A$(1 - Q)

    7809 V%(Y%) = S6%(1 - Q, 0)

    7810 W$(Y%) = W$: L$(Y%) = L$

    7811 ING%(Y%) = LYN%

    7812 If Q = 0 Then PL$(Y%) = "A": GoTo 7814

    7813 PL$(Y%) = "H"

    7814 Rem
    Open D$(1) + R$ + "S" For Output As #1: Print #1, Y%
    For I = 1 To 162: Print #1, H%(I): Next I
    For I = 1 To 162: Print #1, HT$(I): Next I
    For I = 1 To 162: Print #1, V%(I): Next I
    For I = 1 To 162: Print #1, PL$(I): Next I
    For I = 1 To 162: Print #1, W$(I): Next I
    For I = 1 To 162: Print #1, L$(I): Next I
    For I = 1 To 162: Print #1, ING%(I): Next I
    Close #1
    Y% = 0
    Return

    7834 If AP% = 1 Then C% = 0: GoTo 7848
    If X1 = 0 Or X1 = 1 And Q = 0 Or X1 = 2 And Q = 1 Then 7840
    If X1 = 2 And Q = 0 Or X1 = 1 And Q = 1 Then C% = 1: GoTo 7848

    7840 Cls

    7848 Return

    7850 For I = 0 To 22: For I1 = 0 To 21: B6%(I, I1) = 0: Next: Next
    For I = 0 To 22: For I1 = 0 To 6: SS%(I, I1) = 0: Next: Next
    For I = 0 To 21: For I1 = 0 To 41: P0%(I, I1) = 0: Next: Next
    W2% = 0: L2% = 0: For I = 0 To 21: T%(I) = 0: Next I: For I = 0 To 22: P4%(I) = 0: Next I: Return

    7856 For I = 0 To 1: For I1 = 0 To 22: B2%(I, I1) = 0: Next: Next
    For I = 0 To 1: For I1 = 0 To 21: P3%(I, I1) = 0: Next: Next
    Return

    7862 For XX = 4 To 11
        If Mid$(SA$(Q), XX, 1) = " " Or Mid$(SA$(Q), XX, 1) = "." Then Mid$(SA$(Q), XX, 1) = "_"
    Next XX
    QQ = 8: For XX = 11 To 4 Step -1: If Mid$(SA$(Q), XX, 1) = "_" Then QQ = XX - 4: GoTo 7870
        GoTo 7872

    7870 Next XX

    7872 Return

End Sub


'$INCLUDE: 'ROUTINES.BM'
'$INCLUDE: 'ROUTINES_G.BM'
'$INCLUDE: 'SOURCE.BM'
'$INCLUDE: 'SOURCE_G.BM'

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'

