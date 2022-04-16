$Debug
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'ROUTINES.BI'

_Title "Full Count Baseball - PLAY BALL!"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

DefInt A-Z

'Define all routines for this module
DECLARE SUB ACTBATTERS (B%(),P9)
DECLARE SUB ACTPITCHERS (P%(),P9)
DECLARE SUB BDAYOFF (DR%,INJ%(),P9)
DECLARE SUB COLCHK (P9, I)
DECLARE SUB DAYOFF (DR%,P%(),P9)
DECLARE SUB DRAWBOX6 ()
DECLARE SUB GETAKEY ()
DECLARE SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)
DECLARE SUB INFO ()
DECLARE SUB INITVARI ()
DECLARE SUB LETTER (I$)
DECLARE SUB LINSWAP (P9)
DECLARE SUB NEWLINES (P9)
DECLARE SUB POSSWAP (P9)
DECLARE SUB SELECTBATTER (P9, BAT$(), BAT%(), SELECTION%)
DECLARE SUB SELECTLINEUP (P9, BAT$(), BAT%(), SELECTION%)
DECLARE SUB SELECTPITCHER (P$(),P9,P%(),PIT$(),PIT%(),NP,SELECTION%)
DECLARE SUB SELECTPOS (P9, BAT$(), BAT%(), SELECTION%)
DECLARE SUB SHOWBATTERS (P9)
DECLARE SUB SHOWFIELD (P9%)
DECLARE SUB SHOWSPLIT (P9%)
DECLARE SUB PBP (F$)
DECLARE SUB SOURCEG ()
DECLARE SUB WEATHER_G ()
DECLARE SUB boxie0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, FGColor%, BGColor%)

Type PlayCall
    YY As String * 30
End Type

Dim Shared park_graphic As Long, screen_handle As Long

'Declare all variables for the modules
Dim Shared B1!(9)
Dim Shared displayPause As Single, K9(1) As Single, WF As Single
Dim Shared ATTEN As Long

Dim Shared AP%, CGERA%, DH%, DR%, EJ%, IJ%, IJL%, INNING%
Dim Shared PC%, SC%, TA%, TP%, W%, WS%
Dim Shared S$, WI$
Dim Shared D, P, RE, RD, U6, U9

'DIM SHARED BP(1)
Dim BP(9), F(9), MON(5, 14), T1(1)
Dim Shared VV(1), VV1(1)

Dim BAT$(23), BM$(10), BMT$(10), DH$(1), DL$(1), DN$(1)
Dim LGT$(2), MN$(10), PARK$(72), PIT$(22), PM$(6), PMT$(6), PT$(1), PU$(8)
Dim Q3$(100), U6$(2), UMP$(50), teamItems$(200), VI$(1), WD$(9), WE$(1)

Dim Shared A$(1), B$(1, 22), B1$(4), C$(11), D$(3)
Dim Shared EJ$(20), F$(10), H$(4), H0$(1), IJ$(20)
Dim Shared M$(1), N$(1), NN$(1), P$(1, 21), PC$(4)
Dim Shared Q$(23), S$(1), SC$(4), TM$(4), U$(4), X$(12), YN$(1)

Dim BAT%(23), BB%(9), DYS%(1, 21), GS%(21, 1), INJ%(1, 22)
Dim MF%(1), NB%(1, 4), PS%(23, 1), PIT%(22), SP%(1)
Dim U%(50), UM%(1, 9, 4), UV%(200)
Dim Shared A5%(4), AP%(2), B%(1, 22, 80), B0%(3, 2)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(1, 9), B4%(1, 7, 9)
Dim Shared B5%(1, 22, 21), B7%(1, 9), B8%(1, 22, 21), B9%(1, 10), BT%(1, 9, 9)
Dim Shared CF%(9, 2), CSS%(1, 22, 6), D0%(1), D3%(1), DP%(1), GK%(1), H0%(1)
Dim Shared L%(1, 13), L0%(1), LB%(9, 1), M%(9), MG%(1, 999), NG%(18), O%(1)
Dim Shared P%(1, 21, 89), P1%(1), P2%(1), P3%(1, 21), P4%(22), P5%(1, 21, 41), P6%(1), P8%(1, 21, 41), PR%(24)
Dim Shared S%(1, 26), S1%(1), S6%(1, 2), S8%(1, 1), SA%(1, 24), SO%(5, 2), SU%(1, 1)
Dim Shared T%(22), T3%(22), TP%(11, 2), TS%(1, 11), VA%(26), X0%(1, 2, 23)

ReDim Q2$(2056)
Dim E As Single
Dim SE As Single
Dim Q1 As Single
Dim QQ As Single
Dim R As Single
Dim Y As Long
Dim JJ As Single

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
SP%(0) = -1: SP%(1) = -1
U6$(0) = "HUMAN OPPONENT      "
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
LGT$(0) = "AL GAME         "
LGT$(1) = "NL GAME         "
LGT$(2) = "INTERLEAGUE GAME"
DH$(0) = "NO DH "
DH$(1) = "USE DH"
UMP$(0) = "AL"
UMP$(1) = "NL"
UMP$(2) = "NL"
WE$(0) = "YES"
WE$(1) = "NO "
DL$(0) = "LEAGUE GAME    "
DL$(1) = "EXHIBITION GAME"

If Not _FileExists("DEFERA") Then Open "DEFERA" For Output As #1: For I = 1 To 14: Print #1, 0: Next: Close 1

Open "DEFERA" For Input As #1: Input #1, U6: Input #1, LGT: Input #1, DH%
Input #1, DL: Input #1, MO%: Input #1, DN%: Input #1, WE%
Input #1, IJ%: Input #1, LR%: Input #1, DR%: Input #1, MP%
Input #1, G4: Input #1, vg%: Input #1, CGERA%
Close 1

Open "DEV.BAS" For Input As #1
For I = 0 To 3: Input #1, D$(I): Next I
Close #1

MN$(4) = "APRIL"
MN$(5) = "MAY"
MN$(6) = "JUNE"
MN$(7) = "JULY"
MN$(8) = "AUGUST"
MN$(9) = "SEPTEMBER"
MN$(10) = "OCTOBER"
DN$(0) = "AFTERNOON"
DN$(1) = "NIGHT"
B1$(0) = "LP"
B1$(1) = "L"
B1$(2) = "S"
B1$(3) = "R"
B1$(4) = "RP"
    
Data A,B,C,D,E,F,G,H,I,J,K,L,M,dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph,pr

'Is an extra CLS really needed?
Cls
    
'This is overkill; we've already seeded randomizer with TIMER earlier
'FOR RA = 1 TO 60
'   RANDOMIZE TIMER
'NEXT
    
P = 1
    
For I = 0 To 12
    Read X$(I)
Next I
    
For I = 0 To 11
    Read C$(I)
Next I

140 H$(1) = "1b": H$(2) = "2b": H$(3) = "3b": H$(4) = "home"
For I = 0 To 1: For I1 = 0 To 4: NB%(I, I1) = -1: Next: Next
For I = 0 To 1: P1%(I) = -1: P6%(I) = -1
Next
SP%(0) = -1: SP%(1) = -1
For I = 0 To 1
    For J = 0 To 9
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
G4 = NG%(4) / 10
MO% = NG%(17)
WE% = NG%(5)
vg% = NG%(11)
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
Color 14: Print "(2) ";: Color 15: Print LGT$(LGT)
Color 14: Print "(3) ";: Color 15: Print DH$(DH%)
Color 14: Print "(4) ";: Color 15: Print DL$(DL)
Color 14: Print "(5) ";: Color 15: Print MN$(MO%)
Color 14: Print "(6) ";: Color 15: Print DN$(DN%)
Color 14: Print "(7) ";: Color 15: Print "PROCEED TO TEAM SELECTION"
Color 14: Print "(8) ";: Color 15: Print "USE CLIMATE/WIND/TEMP DATA "; WE$(WE%)
Color 14: Print "(9) ";: Color 15: Print "USE INJURIES "; WE$(IJ%)
Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS AS DEFAULT"
Color 14: Print "(A) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; WE$(LR%)
Color 14: Print "(B) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; WE$(DR%)
Color 14: Print "(C) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; WE$(MP%)
Color 14: Print "(D) ";: Color 15: Print Using "DISPLAY INTERVAL: #.# "; G4
Color 14: Print "(E) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; WE$(vg%)
Color 14: Print "(F) ";: Color 15: Print "PITCHER USAGE ERA: "; PU$(CGERA%)

183 Color 7, 0
I$ = GetKeyPress$
If I$ = Chr$(27) Then Run "HELLO.EXE"
Call LETTER(I$)
I = Val(I$): If I < 0 Or I > 16 Then 183
On I + 1 GOTO 205, 191, 193, 195, 197, 199, 201, 260, 204, 206, 207, 208, 209, 210, 211, 212

191 U6 = U6 + 1: If U6 > 2 Then U6 = 0
GoTo 171

211 vg% = 1 - vg%: GoTo 171

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
Print #1, G4: Print #1, vg%: Print #1, CGERA%
Close 1
GoTo 171

260 YV = 1: YM = 0
If Not _FileExists(D$(0) + "UMPIRES." + UMP$(LGT)) Then 265
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
U$(YV) = UMP$(YU): YM = YU: YV = YV + 1: If YV < 5 Then 262
GoTo 270

265 U$(1) = "RHODES": U$(2) = "WINBERRY": U$(3) = "TIPPETT": U$(4) = "MILLER"

270 For I = 0 To 1
    If AP% = 1 Then T1(I) = NG%(I): YN$ = YN$(I): GoSub 300: GoTo 2860
    Call INFO

    2800 Color 15, 0: Cls
    JJ = 10

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
    GoSub 8100

    2856 If BO% = 1 Then BO% = 0: GoTo 2800

    2858 Locate 2, 33
    Color L%(I, 11), L%(I, 12)
    Print A$(I)
    Color 15, 4
    Locate , 33
    Print "ANY CHANGE (YN)";
    I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 2800

    2860 YN$(I) = YN$
    Color 15, 0
    A$(I) = RTrim$(A$(I))
Next I

N = 1

If U6 = 1 And AP% <> 1 Then GoSub 2430
'***   SELECT PITCHERS FOR EACH TEAM   ***

2861 For P9 = 0 To 1
    EC% = 0
    GoSub 4500
    If EC% = 1 And P9 = 0 Then 170
    If EC% = 1 And P9 = 1 Then 2861
    S8%(P9, 0) = Int(((P%(P9, P1%(P9), 8) + P%(P9, P1%(P9), 7)) / P%(P9, P1%(P9), 4)) + .5)
    S8%(P9, 1) = 3
Next P9

'***   SELECT LINEUPS FOR EACH TEAM   ***
For P9 = 0 To 1
    For I = 0 To 9
        M%(I) = 0
    Next
    If DH% = 0 Then
        B3%(P9, 9) = P1%(P9)
        M%(1) = 1
        B7%(P9, 9) = 1
        B9%(P9, 1) = P1%(P9)
    End If
    Call SHOWBATTERS(P9)
    BM$(1) = "Select"
    BM$(2) = "Comp"
    BM$(3) = "Load/Save"
    BM$(4) = "Dis"
    BM$(5) = "Rest"
    BM$(6) = "Act"
    BM$(7) = "Trade"
    BM$(8) = "Pos"
    BM$(9) = "More"
    BM$(10) = "Next"
    BMT$(1) = "Select a player to insert into the lineup"
    BMT$(2) = "Allow the computer to determine the lineup"
    BMT$(3) = "Load/save a lineup from/to the managerial profile"
    BMT$(4) = "Toggle a player's status between active and disabled"
    BMT$(5) = "Grant a day's rest to all players to recover from injuries"
    BMT$(6) = "Activate all disabled players"
    BMT$(7) = "Trade lineup positions between two players"
    BMT$(8) = "Swap defensive positions between two players"
    BMT$(9) = "View more stats (fielding, situational, etc.)"
    BMT$(10) = "Proceed to next menu"

    2862 Call boxie0(6, 39, 17, 51, 2, 15, 1)
    '2862 Call boxie0(6, 58, 17, 80, 2, 15, 1)

    Call NEWLINES(P9)

    GoSub 1590
    GoSub 1370

    Locate 24, 1
    Color 15, 0

    Call Lts2Menu(BM$(), BMT$(), SELECTION%, 15, 0)

    If SELECTION% = 0 Then
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
        If DH% = 0 Then
            B3%(P9, 9) = P1%(P9)
            M%(1) = 1
            B7%(P9, 9) = 1
            B9%(P9, 1) = P1%(P9)
        End If
        GoTo 2862
    End If

    If SELECTION% = 1 Then
        Call SELECTBATTER(P9, BAT$(), BAT%(), I)
        If I = -1 Then 2862
        If B%(P9, I, 21) > 0 Then

            Locate 9
            Message$ = "This player has either been disabled or currently in the lineup. Press any key to continue"
            Wdth = 54
            Call CSMsgBox(Message$, Wdth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

            GoTo 2862
        End If
        'HAVE A FIELDER, NOW PASS CONTROL TO THE LINEUP BLOCK
        Call SELECTLINEUP(P9, BAT$(), BAT%(), I1)
        If I1 = -1 Then 2862
        'CHECK TO SEE IF I PICKED A SPOT WITH THE PITCHER IN IT
        If B7%(P9, I1) = 1 Then

            Locate 9
            Message$ = "This lineup slot is occupied by the pitcher...try again. Press any key to continue"
            Wdth = 54
            Call CSMsgBox(Message$, Wdth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

            GoTo 2862
        End If
        'CHECK TO SEE IF SOMEONE IS ALREADY OCCUYING THE SPOT?
        'IF SO, CLEAR THE EXISTING PLAYER'S FIELDS
        If B7%(P9, I1) <> -1 Then
            M%(B7%(P9, I1)) = 0
            B%(P9, B3%(P9, I1), 21) = 0
            B3%(P9, I1) = -1
            B7%(P9, I1) = -1
        End If
        'HAVE A CLEAN LINEUP SLOT, NOW PASS CONTROL TO THE POSITION BLOCK
        Call SELECTPOS(P9, BAT$(), BAT%(), I2)
        If I2 = -1 Then 2862
        'CHECK TO SEE IF SOMEONE ELSE IS PLAYING THAT POSITION ALREADY
        'IF SO, USE THIS PLAYER, SEND WARNING AND REMOVE OTHER PLAYER
        'FROM THE LINEUP, CLEARING THEIR FIELDS IN THE PROCESS
        Locate 1, 1: Print "SLOT "; I, B$(P9, I), C$(I2)
        GoTo 2862
    End If
    If SELECTION% = 2 Then
        GoSub 5000
        GoTo 2862
    End If
    If SELECTION% = 3 Then
    End If
    If SELECTION% = 4 Then
        Call SELECTBATTER(P9, BAT$(), BAT%(), I1)
        If I1 = -1 Then 2862
        If B%(P9, I1, 21) = 99 Then
            B%(P9, I1, 21) = 0
            GoTo 2862
        End If
        If B%(P9, I1, 21) = 0 Then
            B%(P9, I1, 21) = 99
            GoTo 2862
        End If
        GoTo 2862
    End If
    If SELECTION% = 5 Then
        Call BDAYOFF(DR%, INJ%(), P9)

        Locate 9
        Message$ = "All players have been granted a day's rest. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 2862
    End If
    If SELECTION% = 6 Then
        Call ACTBATTERS(B%(), P9)

        Locate 9
        Message$ = "All batters are now activated for duty. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 2862
    End If
    If SELECTION% = 7 Then
        Call LINSWAP(P9)
        GoTo 2862
    End If
    If SELECTION% = 8 Then
        Call POSSWAP(P9)
        GoTo 2862
    End If
    If SELECTION% = 9 Then
        TG = TG + 1
        If TG > 2 Then TG = 0
        If TG = 1 And TS%(P9, 0) <> 999 Then TG = 2
        If TG = 2 Then Call SHOWFIELD(P9)
        If TG = 0 Then Call SHOWBATTERS(P9)
        If TG = 1 Then Call SHOWSPLIT(P9)
        GoTo 2862
    End If
    If SELECTION% = 10 Then
        TG = 0
    End If
Next
End
'***   SELECT LINEUPS FOR EACH TEAM   ***
For P9 = 0 To 1

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
    If LE = 1 Then LE = 0: GoTo 480
    If I$ <> "1" And I$ <> "3" Then GoSub 2160: GoSub 2310: GoSub 540
    If I$ = "1" Or I$ = "3" Then GoSub 1830

    520 Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
    Locate , 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
    If RP = 1 Then RP = 0: GoTo 480
Next P9: GoSub 2390
VV(0) = 1: VV1(0) = 9: VV(1) = 1: VV1(1) = 9: GoTo 2320

540 For I = 0 To 9: M%(I) = 0: Next I: GoSub 1370
For I = 1 To 9
    Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER": Locate , 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

    560 Locate I + 6, 59
    Print "                     ";
    Locate I + 6, 59
    M%(B7%(P9, I)) = 0: B7%(P9, I) = -1: B3%(P9, I) = -1
    GoSub 1370
    561 I$ = GetKeyPress$
    Call LETTER(I$)
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
    I1 = Val(I$)
    If UCase$(I$) = "X" Then I$ = "R": GoTo 861
    For J = 1 To 9
        If J = I Or B3%(P9, J) = I1 And B7%(P9, J) = 1 Then 610
        If B3%(P9, J) = I1 And UCase$(I$) <> "P" Then 560

    610 Next J
    If DH% = 1 And UCase$(I$) = "P" Then 560
    If B%(P9, I1, 21) <> 0 And UCase$(I$) <> "P" Then 560
    If UCase$(I$) = "P" Then 760
    B3%(P9, I) = I1
    Locate I + 6, 59
    Print Using "# "; I;
    Print B$(P9, B3%(P9, I));
    Locate , 78
    680 I$ = GetKeyPress$
    If Val(I$) = 1 Then 680
    For J = 1 To 9
        If J = I Then 720
        If Val(I$) = B7%(P9, J) Then 680

    720 Next J
    If DH% = 0 And Val(I$) = 0 Or UCase$(I$) = "X" Then 560
    Print C$(Val(I$));: B7%(P9, I) = Val(I$)
    B9%(P9, B7%(P9, I)) = B3%(P9, I): M%(Val(I$)) = 1
    Color 2, 0
    Locate I1 + 2, 1
    'mark the player as being in the lineup
    Print "L";
    'COLOR L%(P9, 11), L%(P9, 12)
    'PRINT B$(P9, I1)
    Color 15, 0
    GoSub 1370
    GoTo 830

    760 For J = 1 To 9: If J = I Then 780
        If B3%(P9, J) = P1%(P9) And B7%(P9, J) = 1 Then 560

    780 Next J
    B3%(P9, I) = P1%(P9)
    Print Using "# "; I;: Print P$(P9, P1%(P9));
    M%(Val(I$)) = 1
    Locate , 78
    Print "p";
    B7%(P9, I) = 1
    B9%(P9, 1) = P1%(P9)
    M%(1) = 1
    GoSub 1370

    830 'IF C1 = 1 THEN 2862
    GoTo 2862
Next I

850 C1 = 1: Locate 25, 1: Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
Color 15, 0
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then I$ = "0": GoTo 861
860 I$ = GetKeyPress$

861 If UCase$(I$) = "R" Or I$ = Chr$(27) Then
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
    GoTo 910
End If
If UCase$(I$) = "P" Then Call POSSWAP(P9): GoTo 2862
I = Val(I$)
If I < 1 Or I > 9 Then 890
Locate B3%(P9, I) + 2, 2
Print " "; B$(P9, B3%(P9, I))
GoTo 560

890 If I% = 0 Then GoSub 1710
For I = 1 To 9
    If B7%(P9, I) = 1 Then 1230
    B%(P9, B3%(P9, I), 21) = 1

    1230 M%(I) = 0
Next I
M%(0) = 0
C1 = 0

910 Return

1370 Call boxie0(19, 58, 22, 80, 2, 15, 1)

Locate 20, 59
For I1 = 0 To 4
    If DH% = 0 And I1 = 0 Then
        Color 7, 0: Print "dh ";
        Color 15
        GoTo 1410
    End If
    If DH% = 1 And I1 = 1 Then
        Color 7, 0
        Print "p  ";
        Color 15
        GoTo 1410
    End If
    If M%(I1) = 0 Then Print C$(I1); " ";
    If M%(I1) = 1 Then Color 7, 0: Print C$(I1); " ";: Color 15

1410 Next I1
Locate 21, 59
For I1 = 5 To 9
    If M%(I1) = 0 Then Print C$(I1); " ";
    If M%(I1) = 1 Then Color 7, 0: Print C$(I1); " ";: Color 15
Next I1
Return

1520 Cls

1521 Locate 1, 1
Color L%(P9, 11), L%(P9, 12): Print A$(P9);
Color 14, 0: Locate , 27: Print "T    G GS DR    W- L   ERA"
For I = 0 To 21
    If P$(P9, I) = "XXX" Or Left$(P$(P9, I), 1) = " " Or P$(P9, I) = "" Then
        P%(P9, I, 15) = 98
    End If
    If P$(P9, I) = "XXX" Or Left$(P$(P9, I), 1) = " " Or P$(P9, I) = "" Then 1580
    Color 2, 0
    If P%(P9, I, 15) = 0 Then
        Print "A";
    ElseIf P%(P9, I, 15) = 1 Then
        Print "L";
    Else
        Print "X";
    End If
    Color 15, 0
    Locate , 3: Print P$(P9, I);
    Locate , 27: Print B1$(P%(P9, I, 0) + 2);
    Locate , 29: Print Using " ### ## ## "; P%(P9, I, 4); P%(P9, I, 5); P%(P9, I, 35);
    Locate , 40: Print Using "  ##"; P%(P9, I, 1);: Print "-";: Print Using "## ##.##"; P%(P9, I, 2); P%(P9, I, 10) / 100

1580 Next I
Return

1590 ' PRINT THE BATTER NAMES AND AVAILABILITY STATUS
Locate 1, 1
Color L%(P9, 11), L%(P9, 12): Print A$(P9)
Color 14, 0
For I = 0 To 22
    Locate , 1
    If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then
        B%(P9, I, 21) = 98
    End If
    If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then 1591
    Color 2, 0
    If B%(P9, I, 21) = 0 Then
        Print "A";
    ElseIf B%(P9, I, 21) = 1 Then
        Print "L";
    ElseIf INJ%(P9, I) <> 0 Then
        Print "I";
    Else
        Print "X";
    End If
    Color 15, 0
    Locate , 3: Print B$(P9, I);
    If I <> 22 Then Print

1591 Next I
Return

1710 Locate 25, 1: Print "                                                                          ";
Locate 25, 1: Print "SAVE THIS LINE UP (YN) ";
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
    
1830 Cls
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
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
            Locate 3, 16 * I + 1
            Print "NONE FOUND"
            GoTo 2510
        End If
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
            Locate 2 + I1, 16 * I + 1
            Print "         "
            GoTo 2510
        End If
        Locate 2 + I1, 16 * I + 1
        If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
            Print "PITCHER";
            GoTo 2512
        End If
        Print Left$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);

        2512 Locate 2 + I1, 16 * I + 12
        Print C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))

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
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
            Locate 14, 16 * (I - 5) + 1
            Print "NONE FOUND"
            GoTo 2505
        End If
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
            Locate 13 + I1, 16 * (I - 5) + 1
            Print "         "
            GoTo 2505
        End If
        Locate 13 + I1, 16 * (I - 5) + 1
        If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
            Print "PITCHER";
            GoTo 2507
        End If
        Print Left$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);

        2507 Locate 13 + I1, 16 * (I - 5) + 12: Print C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))

    2505 Next
Next
LF% = 0: PCT% = 0
For I = 0 To 9
    If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
        LF% = LF% + 1
        PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
    End If
Next
If LF% = 0 Then 3110
If AP% = 1 And MP% = 0 And (U6 = 2 Or U6 = 1 And U9 = P9) Or I$ = "3" Then
    RN = Int(Rnd(1) * PCT%) + 1
    PCT% = 0
    For I = 0 To 9
        If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 Then 2535
        PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
        If RN <= PCT% Then LN% = I: GoTo 2516

    2535 Next
    GoTo 2516
End If

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

1910 For I = 1 To 9
    B9%(P9, B7%(P9, I)) = B3%(P9, I)
    If B7%(P9, I) = 1 Then
        B3%(P9, I) = P1%(P9)
        B9%(P9, 1) = P1%(P9)
        GoTo 2525
    End If
    B%(P9, B3%(P9, I), 21) = 1

2525 Next I
If LU% = 1 Then
    LU% = 0
    GoSub 2160
End If
GoSub 2310
Call NEWLINES(P9)
For I2 = 0 To 22
    Locate I2 + 2, 1
    For I3 = 1 To 9
        If B3%(P9, I3) = I2 And B7%(P9, I3) <> 1 Then
            Color 2, 0
            Print "L";
            Color 15, 0
        End If
    Next I3
Next I2
For I3 = 0 To 9
    M%(I3) = 1
Next I3
' original code had  GOTO 850
'this return works for both the computer generated lineup and
'for the select from managerial profile
Return
    

2160 I% = 99: Call SHOWBATTERS(P9)
Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
Locate 6, 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

2161 Locate 10, 59: Print "DISABLE BATTER (0-M)": Locate , 59: Print "Z-CONTINUE"
Locate , 59: Print "X-REACTIVATE ALL"
Locate , 59: Print "<TAB>-BAT/FIELD/SPLIT"
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
If Asc(I$) = 9 And TG = 0 Then Call SHOWBATTERS(P9): GoTo 2161
If Asc(I$) = 9 And TG = 2 Then Call SHOWSPLIT(P9): GoTo 2170
Call LETTER(I$)
If UCase$(I$) = "Z" Then TG = 0: Call SHOWBATTERS(P9): GoTo 2230
If UCase$(I$) = "X" Then GoSub 2240: GoTo 2170
I1 = Val(I$): If I1 > 22 Then 2170
B%(P9, I1, 21) = 99: Locate I1 + 2, 3: Color 0, 7: Print B$(P9, I1);: Color 15, 0: GoTo 2170

2230 Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
Locate 6, 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
I% = 0: Return

2240 For I3 = 0 To 22: If B%(P9, I3, 21) = 99 And B%(P9, I3, 31) = 0 Then B%(P9, I3, 21) = 0: Locate I3 + 2, 3: Print B$(P9, I3)
Next I3: Return

2310 For I = 7 To 16
    Locate I, 59: Print "                     ";
Next
Return

2320 Cls

2330 GoSub 2470

2345 I1 = Val(Left$(A$(0), 2)): I2 = Val(Left$(A$(1), 2))
If CGERA% > 2 Then S8%(1, 1) = 4: S8%(0, 1) = 4
If CGERA% > 3 Then S8%(1, 1) = 5: S8%(0, 1) = 5
If CGERA% = 7 Then S8%(1, 1) = 7: S8%(0, 1) = 7
If CGERA% >= 8 Then S8%(1, 1) = 99: S8%(0, 1) = 99

If (MON = 6 Or MON = 7) And vg% = 0 Then
    Call SOURCEG
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
Color 15: Print "(0) ";: Color L%(0, 11), L%(0, 12): Print A$(0): Color 7, 0: Print
Color 15: Print "(1) ";: Color L%(1, 11), L%(1, 12): Print A$(1): Color 7, 0
2440 I$ = GetKeyPress$
U9 = Val(I$): If U9 <> 1 Then U9 = 0
Return

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
Call DRAWBOX6
Locate , 11: Print "CONDITIONS AT "; S$(1)
If L%(1, 9) = 1 Then Locate , 11: Print "TEMPERATURE: 70": Locate , 11: Print "WEATHER: DOME": Locate , 11: Print "WIND: NONE": GoTo 2479
Locate , 11: Print "TEMPERATURE: "; TP%
Locate , 11: Print "SKY CONDITIONS: "; SC$(SC%)
Locate , 11: Print "WIND: "; WD$(WD%);
Print Using " ## MPH"; WS%
If vg% = 1 Then
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
If AP% = 1 Then NLF% = 1
GoTo 480

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
PARK$(72) = "GENERIC                     "
Call DRAWBOX6
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
SCANCODE% = 72
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 5
Column% = 10

Call boxie0(4, 9, 20, 38, 2, 15, 1)

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
GoSub 1520
If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then Return
PM$(1) = "Select"
PM$(2) = "Computer"
PM$(3) = "Disable"
PM$(4) = "Rest"
PM$(5) = "Activate"
PM$(6) = "Proceed"
PMT$(1) = "Select the starting pitcher for this game"
PMT$(2) = "Allow the computer to choose the starting pitcher"
PMT$(3) = "Toggle a player's status between active and disabled"
PMT$(4) = "Grant a day's rest to all pitchers"
PMT$(5) = "Activate all disabled pitchers"
PMT$(6) = "Proceed to next menu"

4501 GoSub 1521
Locate 7, 57
Color 15, 0
Print "STARTING PITCHER IS:"
If SP%(P9) <> -1 Then
    Locate , 57
    Print P$(P9, P1%(P9)); Tab(79);
Else
    Locate , 57
    Print "NONE SELECTED"; Tab(79);
End If
Locate 24, 1

Call Lts2Menu(PM$(), PMT$(), SELECTION%, 15, 0)

If SELECTION% = 0 Then
    EC% = 1
    Return
End If

If SELECTION% = 1 Then
    Call SELECTPITCHER(P$(), P9, P%(), PIT$(), PIT%(), NP, I1)
    If I1 = -1 Then 4501
    If P%(P9, I1, 15) > 0 Then

        Locate 9
        Message$ = "This pitcher has either been disabled or is the current starter. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4501
    End If
    P1%(P9) = I1
    P6%(P9) = 1
    For I1 = 0 To 21
        If P%(P9, I1, 15) = 1 Then
            P%(P9, I1, 15) = 0
        End If
    Next
    P%(P9, P1%(P9), 15) = 1
    SP%(P9) = P1%(P9)
    GoTo 4501
End If
If SELECTION% = 2 Then
    GoSub 1520
    GoTo 4501
End If
If SELECTION% = 3 Then
    Call SELECTPITCHER(P$(), P9, P%(), PIT$(), PIT%(), NP, I1)
    If I1 = -1 Then 4501
    If P%(P9, I1, 15) = 0 Then
        P%(P9, I1, 15) = 99
        GoTo 4501
    End If
    If P%(P9, I1, 15) = 99 Then
        P%(P9, I1, 15) = 0
        GoTo 4501
    End If
    GoTo 4501
End If
If SELECTION% = 4 Then

    Call DAYOFF(DR%, P%(), P9)

    Locate 9
    Message$ = "All pitchers have been granted a day's rest. Press any key to continue"
    Wdth = 54
    Call CSMsgBox(Message$, Wdth, Pass)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

    GoTo 4501
End If
If SELECTION% = 5 Then

    Call ACTPITCHERS(P%(), P9)

    Locate 9
    Message$ = "All pitchers are now activated for duty. Press any key to continue"
    Wdth = 54
    Call CSMsgBox(Message$, Wdth, Pass)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

    GoTo 4501
End If
If SELECTION% = 6 And SP%(P9) = -1 Then

    Locate 9
    Message$ = "A starting pitcher must be selected before proceeding. Press any key to continue"
    Wdth = 54
    Call CSMsgBox(Message$, Wdth, Pass)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

    GoTo 4501
Else
    Return
End If

4515 If MF%(P9) = -1 Then 4600
If MG%(P9, 99) <> 999 Then 4600

4516 'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

4517 RN = Int(Rnd(1) * 100) + 1
P%(P9, MG%(P9, MG%(P9, 2) + 3), 15) = 0
If RN <= MG%(P9, MG%(P9, 2) + 10 + 3) Or MG%(P9, MG%(P9, 2) + 10 + 3) = 0 Then
    P1%(P9) = MG%(P9, MG%(P9, 2) + 3)
    P6%(P9) = 1
    For I1 = 0 To 21
        If P%(P9, I1, 15) = 1 Then
            P%(P9, I1, 15) = 0
        End If
    Next
    P%(P9, P1%(P9), 15) = 1
    SP%(P9) = P1%(P9)
    GoTo 4520
End If
RN = Int(Rnd(1) * 100) + 1
SP% = 0
For I = 21 To 36
    If MG%(P9, I) = -1 Or MG%(P9, I) < 0 Or MG%(P9, I) > 21 Then 4518
    SP% = SP% + MG%(P9, I + 20)
    If RN <= SP% And MG%(P9, I) <> -1 Then
        P1%(P9) = MG%(P9, I)
        P6%(P9) = 1
        For I1 = 0 To 21
            If P%(P9, I1, 15) = 1 Then
                P%(P9, I1, 15) = 0
            End If
        Next
        P%(P9, P1%(P9), 15) = 1
        SP%(P9) = P1%(P9)
        GoTo 4520
    End If

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

4600 CR% = 1
GS = 0
For I = 0 To 21
    If P%(P9, I, 5) > GS Then GS = P%(P9, I, 5)
Next
I = 1
If GS <= 75 Then I = 2
If GS <= 54 Then I = 3
If GS <= 41 Then I = 4
If GS <= 32 Then I = 5
MG%(P9, 1) = I: MG%(P9, 2) = 0
For I = 3 To 36
    MG%(P9, I) = -1
Next
For I = 41 To 56
    MG%(P9, I) = 0
Next
MG%(P9, 0) = 999
'COMPUTER SET ROTATION
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
For I = 0 To 22
    If B$(P9, I) = "XXX" Then B%(P9, I, 21) = 98
    If B%(P9, I, 21) = 1 Then B%(P9, I, 21) = 0
Next
For I = 1 To 9
    F(I) = 0
    BP(I) = 0
    BB%(I) = -1
Next
M = 0
For I = 9 To 1 Step -1
    J = I
    For I2 = 0 To 22
        PS%(I2, 0) = 0
        PS%(I2, 1) = -1
    Next
    I2 = 0
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
        If RN <= PS%(I1, 0) Then
            F(J) = PS%(I1, 1)
            B%(P9, PS%(I1, 1), 21) = 1
            GoTo 5119
        End If
        PS%(I1 + 1, 0) = PS%(I1 + 1, 0) + PS%(I1, 0)
    Next I1

    Locate 9
    Message$ = "Computer is unable to generate a lineup for this team. Press any key to continue"
    Wdth = 54
    Call CSMsgBox(Message$, Wdth, Pass)

    Locate 25, 1: Print "CANNOT MAKE A LINEUP!!...";: LE = 1

    For J = 1 To 9
        M%(J) = 0
        B7%(P9, J) = -1
        B3%(P9, J) = -1
    Next

    C1 = 0

    For J = 0 To 22
        If B%(P9, J, 21) <> 99 Then B%(P9, J, 21) = 0
    Next

    If AP% = 1 Then 5163
    
    I$ = GetKeyPress$
    5163 Call CSMsgBox("", 0, Pass)
    Return

5119 Next I
GoTo 5230

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
C = 2
B = 1: GoSub 5300
B = 2: GoSub 5300
C = 1
B = 3: GoSub 5300
B = 4: GoSub 5300
C = 1
B = 5: GoSub 5300
B = 6: GoSub 5300
B = 7: GoSub 5300
B = 8: GoSub 5300
If DH% = 1 Then B = 9: GoSub 5300
For I = 1 To 9
    B3%(P9, I) = BB%(I)
    B7%(P9, I) = BP(I)
Next
GoTo 1910

5300 E = -100: U = -1: T = -1
For L = 0 To 9
    If DH% = 0 And L = 0 Then L = L + 2
    If DH% = 1 And L = 1 Then L = L + 1
    J = F(L): X = 1
    For Z = 1 To 9
        If BB%(Z) = J Then X = 0: Z = 9
    Next Z
    If X = 0 Then 5350
    If C = 1 Then 5345
    JJ = J: GoSub 5355: T1% = 100 * B%(P9, J, 6) / B%(P9, J, 4) - 27
    If T1% < 0 Then T1% = 3 * T1%
    JJ = 3 * R + T1%: If JJ > E Then E = JJ: T = J: U = L
    GoTo 5350

    5345 SE = (B%(P9, J, 6) + B%(P9, J, 7) + B%(P9, J, 8) * 2 + B%(P9, J, 9) * 3) / B%(P9, J, 4)
    If SE >= E Then E = SE: T = J: U = L

5350 Next L
BB%(B) = T
BP(B) = U
Return

5355 'IF A PITCHER THEN 268
Q1 = (B%(P9, JJ, 6) - B%(P9, JJ, 8) - B%(P9, JJ, 9) + B%(P9, JJ, 11)) * .7
If Q1 = 0 Then Q1 = 1
Q1 = B%(P9, JJ, 13) / Q1: If Q1 < .02 Then 5375
QQ = Q1 + .18: R = Int(QQ * (18 + 2 * Abs((QQ < .34)))): GoTo 5380

5375 R = 2 + Int(Q1 * 100): If B%(P9, JJ, 1) = 2 Then R = R - 1

5380 If (B%(P9, JJ, 1) = 6 Or B%(P9, JJ, 1) = 8) And R < 7 Then R = R + 1
If R > 9 Then R = 9
If R < 0 Then R = 1
Return

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

_MouseShow "DEFAULT"
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

    'check for 1934 season

    If Val(Left$(A$(I), 2)) < 54 Then B%(I, I1, 30) = B%(I, I1, 30) * .75
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
Sub COLCHK (P9, I)
    If B%(P9, I, 21) >= 98 Then Color 0, 7
End Sub


'------------------------------
'   DRAWBOX6 Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOX6 ()
    JJ = 5
    Locate 6, 8
    Color L%(1, 11), L%(1, 12)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ
        Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
End Sub


'------------------------------
'   GETAKEY Subroutine
'------------------------------
' ...explanation...
Sub GETAKEY ()
    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    4001 I$ = GetKeyPress$
End Sub


'------------------------------
'   GETWEATHER Subroutine
'------------------------------
' ...explanation...
Sub GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)

    If L%(1, 13) > 71 Then
        ID = Int(Rnd(1) * 71) + 1
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
End Sub


'------------------------------
'   INFO Subroutine
'------------------------------
' ...explanation...
Sub INFO ()
    Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
    Print "GAME OPTIONS MENU"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
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
    If UCase$(I$) = "A" Then I$ = "10"
    If UCase$(I$) = "B" Then I$ = "11"
    If UCase$(I$) = "C" Then I$ = "12"
    If UCase$(I$) = "D" Then I$ = "13"
    If UCase$(I$) = "E" Then I$ = "14"
    If UCase$(I$) = "F" Then I$ = "15"
    If UCase$(I$) = "G" Then I$ = "16"
    If UCase$(I$) = "H" Then I$ = "17"
    If UCase$(I$) = "I" Then I$ = "18"
    If UCase$(I$) = "J" Then I$ = "19"
    If UCase$(I$) = "K" Then I$ = "20"
    If UCase$(I$) = "L" Then I$ = "21"
    If UCase$(I$) = "M" Then I$ = "22"
End Sub


'------------------------------
'   LINSWAP Subroutine
'------------------------------
' ...explanation...
Sub LINSWAP (P9)
    Locate 23, 59: Print "B.O. PLYR 1 (1-9)";
    1441 I$ = GetKeyPress$
    K1 = Val(I$)
    If K1 < 1 Or K1 > 9 Then 1441
    If B7%(P9, K1) = 1 Then 1441
    Locate 24, 59: Print "B.O. PLYR 2 (1-9)";
    1442 I$ = GetKeyPress$
    K2 = Val(I$)
    If K2 < 1 Or K2 > 9 Then 1442
    If B7%(P9, K2) = 1 Then 1442
    Swap B7%(P9, K2), B7%(P9, K1)
    Swap B3%(P9, K1), B3%(P9, K2)
    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)
    Call NEWLINES(P9)
    Locate 23, 59: Print "                 ";
    Locate 24, 59: Print "                 ";
End Sub


'------------------------------
'   NEWLINES Subroutine
'------------------------------
' ...explanation...
Sub NEWLINES (P9)
    For I = 1 To 9
        Locate I + 6, 59
        If B7%(P9, I) <> -1 Then
            Print Using "# "; I;
            If B7%(P9, I) = 1 Then Print P$(P9, P1%(P9));
            If B7%(P9, I) <> 1 Then Print B$(P9, B3%(P9, I));
            Print Tab(78);
            Print C$(B7%(P9, I));
        Else
            Print Using "# "; I;
            Print Tab(78);
            Print "  ";
        End If
    Next I
    Locate 16, 61: Print P$(P9, P1%(P9)); Tab(78); "p"
End Sub


'------------------------------
'   POSSWAP Subroutine
'------------------------------
' ...explanation...
Sub POSSWAP (P9)
    Locate 23, 59: Print "B.O. PLYR 1 (1-9)";
    1431 I$ = GetKeyPress$
    K1 = Val(I$): If K1 < 1 Or K1 > 9 Then 1431
    If B7%(P9, K1) = 1 Then 1431
    Locate 24, 59: Print "B.O. PLYR 2 (1-9)";
    1432 I$ = GetKeyPress$
    K2 = Val(I$): If K2 < 1 Or K2 > 9 Then 1432
    If B7%(P9, K2) = 1 Then 1432
    Swap B7%(P9, K2), B7%(P9, K1)
    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)
    Call NEWLINES(P9)
    Locate 23, 59: Print "                 ";
    Locate 24, 59: Print "                 ";
End Sub


'------------------------------
'   SHOWBATTERS Subroutine
'------------------------------
' ...explanation...
Sub SHOWBATTERS (P9)
    Cls
    Color L%(P9, 11), L%(P9, 12): Print A$(P9);: Color 14, 0
    Locate , 24: Print "B  POS   AB 2B 3B HR RBI  SB   AVG"
    Locate 2
    For I = 0 To 22: Locate , 1
        If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then B%(P9, I, 21) = 98: GoTo 1191
        Color 2, 0
        If I < 10 Then Print Using "#"; I;
        If I > 9 Then Print X$(I - 10);
        Color 15, 0
        Call COLCHK(P9, I)
        Locate , 3: Print B$(P9, I);
        Color 15, 0
        Locate , 24: Print B1$(B%(P9, I, 0) + 2);
        Locate , 27: If B%(P9, I, 22) <> -1 Then Print Using "#"; B%(P9, I, 22);
        Locate , 28: If B%(P9, I, 23) <> -1 Then Print Using "#"; B%(P9, I, 23);
        Locate , 29: If B%(P9, I, 24) <> -1 Then Print Using "#"; B%(P9, I, 24);
        Locate , 30: If B%(P9, I, 25) <> -1 Then Print Using "#"; B%(P9, I, 25);
        Locate , 32
        Print Using "### ## ## ## ### ###"; B%(P9, I, 4); B%(P9, I, 7); B%(P9, I, 8); B%(P9, I, 9); B%(P9, I, 10); B%(P9, I, 13);
        Locate , 53
        If B%(P9, I, 6) = B%(P9, I, 4) And B%(P9, I, 6) <> 0 Then Print "1.000";: GoTo 1190
        If B%(P9, I, 4) = 0 Then Print " .---";: GoTo 1190
        Print Using " .###"; B%(P9, I, 6) / B%(P9, I, 4);

        1190 If I <> 22 Then Print

    1191 Next I
    Color 15, 0
    Locate 1, 59
    Print "OPPOSING PITCHER:"
    Locate 2, 59
    Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
    Locate , 59
    Print Using "##"; P%(1 - P9, P1%(1 - P9), 1);: Print "-";: Print Using "## ##.##"; P%(1 - P9, P1%(1 - P9), 2); P%(1 - P9, P1%(1 - P9), 10) / 100
    Locate , 59
    If P%(1 - P9, P1%(1 - P9), 51) + P%(1 - P9, P1%(1 - P9), 58) <> 0 And TS%(1 - P9, 0) <> -1 Then
        Print Using "L:  .###"; P%(1 - P9, P1%(1 - P9), 52) / (P%(1 - P9, P1%(1 - P9), 51) + .001);
        Print Using "  R:  .###"; P%(1 - P9, P1%(1 - P9), 59) / (P%(1 - P9, P1%(1 - P9), 58) + .001)
    ElseIf P%(1 - P9, P1%(1 - P9), 6) * 3 * 0.94 + P%(1 - P9, P1%(1 - P9), 7) = 0 Then
        Print "OPP AVG:  .---"
    ElseIf P%(1 - P9, P1%(1 - P9), 6) * 3 * 0.94 + P%(1 - P9, P1%(1 - P9), 7) = P%(1 - P9, P1%(1 - P9), 7) Then
        Print "OPP AVG: 1.000"
    Else
        Print Using "OPP AVG:  .###"; P%(1 - P9%, P1%(1 - P9), 7) / (P%(1 - P9, P1%(1 - P9), 6) * 3 * 0.94 + P%(1 - P9, P1%(1 - P9), 7))
    End If
End Sub


'------------------------------
'   SHOWFIELD Subroutine
'------------------------------
' ...explanation...
Sub SHOWFIELD (P9)
    'DISPLAY FIELDING AVERAGES
    Locate 1, 24
    Color 14, 0
    Print "P1 FAVG  P2 FAVG  P3 FAVG  P4 FAVG"
    Color 15, 0
    Locate 2
    For I = 0 To 22
        Locate , 24
        Print "                                  ";
        If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then 2807
        Locate , 24: If B%(P9, I, 22) <> -1 Then Print Using " # ####"; B%(P9, I, 22); B%(P9, I, 26);
        Locate , 33: If B%(P9, I, 23) <> -1 Then Print Using " # ####"; B%(P9, I, 23); B%(P9, I, 27);
        Locate , 42: If B%(P9, I, 24) <> -1 Then Print Using " # ####"; B%(P9, I, 24); B%(P9, I, 28);
        Locate , 51: If B%(P9, I, 25) <> -1 Then Print Using " # ####"; B%(P9, I, 25); B%(P9, I, 29);
        If I <> 22 Then Print

    2807 Next I
End Sub


'------------------------------
'   SHOWSPLIT Subroutine
'------------------------------
' ...explanation...
Sub SHOWSPLIT (P9)
    'DISPLAY SPLIT STATS
    Locate 1, 24
    Color 14, 0
    Print "VS LHP  HR   AVG VS RHP  HR   AVG "
    Color 15, 0
    Locate 2
    For I = 0 To 22
        Locate , 24
        Print "                                  ";
        If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then 2806
        Locate , 27: Print Using "###  ## "; B%(P9, I, 42); B%(P9, I, 46);
        If B%(P9, I, 42) = 0 Or B%(P9, I, 43) = 0 Then Print " .000";: GoTo 2802
        If B%(P9, I, 42) = B%(P9, I, 43) Then Print "1.000";: GoTo 2802
        Print Using " .###"; B%(P9, I, 43) / B%(P9, I, 42);

        2802 Locate , 44: Print Using "###  ## "; B%(P9, I, 49); B%(P9, I, 53);
        If B%(P9, I, 49) = 0 Or B%(P9, I, 50) = 0 Then Print " .000";: GoTo 2805
        If B%(P9, I, 49) = B%(P9, I, 50) Then Print "1.000";: GoTo 2805
        Print Using " .###"; B%(P9, I, 50) / B%(P9, I, 49);

        2805 If I <> 22 Then Print

    2806 Next I
    
End Sub


'------------------------------
'   DAYOFF Subroutine
'------------------------------
' ...explanation...
Sub DAYOFF (DR%, P%(), P9)
    If DR% <> 1 Then
        For I1 = 0 To 21
            If P%(P9, I1, 35) > 0 Then P%(P9, I1, 35) = P%(P9, I1, 35) - 1
        Next
    End If
End Sub


'------------------------------
'   BDAYOFF Subroutine
'------------------------------
' ...explanation...
Sub BDAYOFF (DR%, INJ%(), P9)
    If DR% <> 1 Then
        For I1 = 0 To 22
            If INJ%(P9, I1) > 0 Then INJ%(P9, I1) = INJ%(P9, I1) - 1
        Next
    End If
End Sub


'------------------------------
'   ACTPITCHERS Subroutine
'------------------------------
' ...explanation...
Sub ACTPITCHERS (P%(), P9)
    For I3 = 0 To 21
        If P%(P9, I3, 15) = 99 And P%(P9, I3, 34) = 0 Then
            P%(P9, I3, 15) = 0
        End If
    Next I3
End Sub


'------------------------------
'   ACTBATTERS Subroutine
'------------------------------
' ...explanation...
Sub ACTBATTERS (B%(), P9)
    For I3 = 0 To 22
        If B%(P9, I3, 21) = 99 And B%(P9, I3, 31) = 0 Then B%(P9, I3, 21) = 0
    Next I3
End Sub


'------------------------------
'   SELECTLINEUP Subroutine
'------------------------------
' ...explanation...
Sub SELECTLINEUP (P9, BAT$(), BAT%(), SELECTION%)
    For I = 1 To 9
        If B7%(P9, I) <> -1 Then
            If B7%(P9, I) = 1 Then BAT$(I) = P$(P9, B3%(P9, I))
            If B7%(P9, I) <> 1 Then BAT$(I) = B$(P9, B3%(P9, I))
        Else
            BAT$(I) = "          "
        End If
    Next I
    Start% = 1

    4510 SELECTION% = 1
    Count% = 9
    SCANCODE% = 9
    FileColr% = 15
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 7
    Column% = 61
    
    _MouseShow "DEFAULT"
    MMenuVert BAT$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
    _MouseHide

    Color , 0
    If SCANCODE% = 27 Then
        SELECTION% = -1
        GoTo 4511
    End If
    If SCANCODE% > 48 And SCANCODE% < 58 Then
        SELECTION% = SCANCODE% - 48
        Call NEWLINES(P9)
        Locate SELECTION% + 6, 61
        Color 0, 7
        Print "          "
        Color 15, 0
        GoTo 4511
    End If
    If SCANCODE% <> 13 And SCANCODE% <> 1003 Then 4510

4511 End Sub


'------------------------------
'   SELECTPOS Subroutine
'------------------------------
' ...explanation...
Sub SELECTPOS (P9, BAT$(), BAT%(), SELECTION%)
    BAT$(1) = C$(0)
    BAT$(2) = C$(5)
    BAT$(3) = C$(1)
    BAT$(4) = C$(6)
    BAT$(5) = C$(2)
    BAT$(6) = C$(7)
    BAT$(7) = C$(3)
    BAT$(8) = C$(8)
    BAT$(9) = C$(4)
    BAT$(10) = C$(9)
    Start% = 1

    4514 SELECTION% = 1
    Count% = 10
    SCANCODE% = 10
    FileColr% = 15
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Rows% = 2
    Columns% = 5
    Row% = 20
    Column% = 59

    _MouseShow "DEFAULT"
    MAMenu BAT$(), SELECTION%, Start%, Count%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    Color , 0
    If SCANCODE% = 27 Then SELECTION% = -1: GoTo 4513
    If SCANCODE% = 48 And DH% = 0 Then
        Locate 9
        Message$ = "You selected not to use the DH. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4514
    End If
    If SCANCODE% = 49 Then
        Locate 9
        Message$ = P$(P9, P1%(P9)) + " must face at least one batter. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4514
    End If
    If SCANCODE% > 49 And SCANCODE% < 58 Or SCANCODE% = 48 Then
        SELECTION% = SCANCODE% - 48
        GoTo 4513
    End If
    If SCANCODE% <> 13 And SCANCODE% <> 1003 Then 4514
    If SELECTION% = 1 And DH% = 0 Then
        Locate 9
        Message$ = "You selected not to use the DH. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4514
    End If
    If SELECTION% = 3 Then
        Locate 9
        Message$ = P$(P9, P1%(P9)) + " must face at least one batter. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4514
    End If
    
    If SELECTION% = 2 Then SELECTION% = 5: GoTo 4513
    If SELECTION% = 4 Then SELECTION% = 6: GoTo 4513
    If SELECTION% = 5 Then SELECTION% = 2: GoTo 4513
    If SELECTION% = 6 Then SELECTION% = 7: GoTo 4513
    If SELECTION% = 7 Then SELECTION% = 3: GoTo 4513
    If SELECTION% = 8 Then SELECTION% = 8: GoTo 4513
    If SELECTION% = 9 Then SELECTION% = 4: GoTo 4513
    If SELECTION% = 10 Then SELECTION% = 9: GoTo 4513

4513 End Sub


'------------------------------
'   SELECTBATTER Subroutine
'------------------------------
' ...explanation...
Sub SELECTBATTER (P9, BAT$(), BAT%(), SELECTION%)
    NP = 0
    For I = 0 To 22
        If Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "XXX" Then 4507
        NP = NP + 1
        BAT$(NP) = B$(P9, I)
        BAT%(NP) = I

    4507 Next
    Start% = 1

    4508 SELECTION% = 1
    Count% = NP
    SCANCODE% = NP
    FileColr% = 15
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 2
    Column% = 3

    _MouseShow "DEFAULT"
    MMenuVert BAT$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
    _MouseHide

    Color , 0
    If SCANCODE% = 27 Then SELECTION% = -1: GoTo 4509
    If SCANCODE% <> 13 And SCANCODE% <> 1003 Then 4508
    SELECTION% = BAT%(SELECTION%)

4509 End Sub


'------------------------------
'   SELECTPITCHER Subroutine
'------------------------------
' ...explanation...
Sub SELECTPITCHER (P$(), P9, P%(), PIT$(), PIT%(), NP, SELECTION%)
    NP = 0
    For I = 0 To 21
        If Left$(P$(P9, I), 1) = " " Or P$(P9, I) = "XXX" Then 4504
        NP = NP + 1
        PIT$(NP) = P$(P9, I)
        PIT%(NP) = I

    4504 Next
    Start% = 1

    4503 SELECTION% = 1
    Count% = NP
    SCANCODE% = NP
    FileColr% = 15
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 2
    Column% = 3

    _MouseShow "DEFAULT"
    MMenuVert PIT$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
    _MouseHide

    Color , 0
    If SCANCODE% = 27 Then SELECTION% = -1: GoTo 4506
    If SCANCODE% <> 13 And SCANCODE% <> 1003 Then 4503
    SELECTION% = PIT%(SELECTION%)

4506 End Sub

'------------------------------
'   SOURCEG Subroutine
'------------------------------
' ...explanation...
Sub SOURCEG ()
    'Declare all variables for the modules
    Dim F1$(10)
    Dim CZ%(1), PH%(22, 1)

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

    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    Call LOADSCREEN
    GoTo 5000

    2000 ' BRANCH FOR RARE PLAYS
    Call RAREPLAYS(H6%, H7%) ', IP%)
    'This line below is not in SOURCE.BAS
    If IP% = 1 Then IP% = 0: Call RELIEVERS
    If H6% <= 4 Or H6% > 5 And H6% < 9 Or H6% >= 10 And H6% <= 24 Then GoSub 2002: GoTo 5052
    If H6% = 5 Or H6% >= 25 And H6% <= 29 Then GoSub 2002: GoTo 3900
    If H6% = 9 Then GoSub 2002: GoTo 3240
    If H6% = 45 Then GoSub 2002: GoTo 3201
    GoSub 2002: GoTo 5052
    2002 Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    Call FLDDISP(BC%, P, U%, D)
    Call MAINSCREEN(D, P, BC%, U%, MO)
    Return

    3050 Call RANDNUM(RN)
    If (((PC% = 1 Or PC% = 2 Or PC% = 3) And RN = 2) Or (RD = 1 And RN <= 5)) And INNING% >= 2 And RD% = 0 Then H7% = 20: H6% = 21: GoTo 2000
    H6% = Int(Rnd(1) * 1000) + 1
    If H6% <= 3 And IJ% = 0 Then 2000
    SN = 0: H6% = Int(Rnd(1) * 1000) + 1
    If O%(0) = 0 And S4% = 4 And INFPOS% < 2 And H6% = 768 Or O%(0) = 0 And S4% = 4 And INFPOS% < 2 And H6% = 123 Or O%(0) = 0 And S4% = 7 And INFPOS% < 2 And H6% = 245 Or O%(0) = 0 And S4% = 7 And INFPOS% < 2 And H6% = 381 Then Call TRIPLEPLAY(D): GoTo 3900
    H7% = Int(Rnd(1) * 7500) + 1
    Call BLANKLINES
    If H6% <= 4 Then 3200
    If H6% <= 6 Then 3210
    If H6% <= 11 Or H6% <= 200 And B%(D, B9%(D, 2), 22) <> 2 And B%(D, B9%(D, 2), 23) <> 2 And B%(D, B9%(D, 2), 24) <> 2 And B%(D, B9%(D, 2), 25) <> 2 Then 3220
    If H6% <= 19 Then 3230
    If H7% = 27 Then 3244
    If H7% = 28 Then 3246
    If H6% <= B4%(P, 5, B1!(P)) + 25 Then S2% = 2: GoTo 3240
    If P2 = 4 Then 4100
    3056 H6% = Int(Rnd(1) * 1000) + 1: H7% = Int(Rnd(1) * 1000) + 1
    I4 = 0: If P2 = 2 Then I4 = -5
    If INFPOS% = 2 Then SN = 70: I4 = I4 + 15
    If U% = 2 Then I4 = I4 - 15
    If INFPOS% = 1 Then SN = 20
    If INNING% > 5 And S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) Then I4 = I4 + 40
    If P = 0 Then I4 = I4 - 2
    If P = 1 Then I4 = I4 + 2
    If H7% <= SN Then Call PBP("It gets through the drawn-in infield!"): Call DELAY: GoTo 3063
    If H6% > B4%(P, 0, B1!(P)) + I4 Then 3600
    H6% = Int(Rnd(1) * 1000) + 1
    If H6% <= B4%(P, 1, B1!(P)) And P2 <> 2 Or H6% <= B4%(P, 1, B1!(P)) * .5 And P2 = 2 Then S2% = 8: GoTo 3500
    If H6% <= B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P)) And P2 <> 2 Or H6% <= B4%(P, 1, B1!(P)) * .5 + B4%(P, 2, B1!(P)) * .5 And P2 = 2 Then S2% = 9: GoTo 3520

    Call HITS2GAP(W%, P, I1, D)
    I3 = B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P))
    E2% = I3 + B4%(P, 3, B1!(P)): If W% = 8 And H6% <= E2% And P2 <> 2 Or W% = 8 And H6% <= E2% * .5 And P2 = 2 Then S2% = 10: GoTo 3530
    E2% = I3 + B4%(P, 6, B1!(P)): If W% = 7 And H6% <= E2% And P2 <> 2 Or W% = 7 And H6% <= E2% * .5 And P2 = 2 Then S2% = 10: GoTo 3530
    E2% = I3 + B4%(P, 7, B1!(P)): If W% = 9 And H6% <= E2% And P2 <> 2 Or W% = 9 And H6% <= E2% * .5 And P2 = 2 Then S2% = 10: GoTo 3530
    3063 H6% = Int(Rnd(1) * 100) + 1: S2% = 7: If B7%(P, B1!(P)) = 1 Or H7% <= SN Then 3065
    If H6% <= (B%(P, B3%(P, B1!(1)), 16) * 2.5) Then S2% = 12: Call GETINFIELDER(W%, P, I1, D, S2%, P2): Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900


    3065 '*** single ***
    Call SINGLEROUTINE(W%, P, I1, D, S2%, P2)
    If S2% = 12 Then Call SCOREBOARD: GoTo 5050
    F% = 0: PQ = 0: Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9)
    Call SCOREBOARD
    GoTo 3900


     
    3200 '*** hit by pitch ***
    Call HITBYPITCH(S2%, D)
    3201 A5%(0) = 1: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
    If B0%(1, 0) > 0 Then A5%(1) = 1
    If B0%(1, 0) > 0 And B0%(2, 0) > 0 Then A5%(2) = 1
    If B0%(1, 0) > 0 And B0%(2, 0) > 0 And B0%(3, 0) > 0 Then A5%(3) = 1: If W% = 2 And S2% > 2 Then A5%(3) = 11
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    GoTo 5050


    3210 '*** balk ***
    If S4% = 0 Then 3100
    S2% = 4: Call BLANKLINES
    Call RANDNUM(RN)
    UN = Int(Rnd(1) * 4) + 1
    If RN <= 33 Then F$ = "a balk has been called"
    If RN > 33 And RN <= 66 Then Call PBP("and a BALK is called"): F$ = P$(D, P1%(D)) + " dropped the ball"
    If RN > 66 Then F$ = U$(UN) + " calls a balk on " + P$(D, P1%(D))
    Call PBP(F$): GoTo 3223

    3100 '*** fouled out of play ***
    Call FOULBALL
    GoTo 5052


    3220 '*** passed ball ***
    If S4% = 0 And SO <> 1 Then 3100
    S2% = 5: Call BLANKLINES
    Call DELIVERY(D)
    Call RANDNUM(RN)
    If RN <= 50 Then Call PBP("pitch gets by " + B$(D, B9%(D, 2)))
    If RN > 50 Then Call PBP("off " + B$(D, B9%(D, 2)) + "'s glove"): Call PBP("it gets away from him")
    Call PBP("It's ruled a passed ball")
    If B0%(1, 0) = 0 And I = 9 And SO = 1 Or O%(0) > 1 And I = 9 And SO = 1 Then O%(1) = O%(1) + 1
    3222 I8 = Int(Rnd(1) * 10) + 1
    If S4% = 1 And P2 = 2 And I8 < B%(P, B3%(P, B0%(1, 0)), 16) Or S4% = 5 And P2 = 2 And I8 < B%(P, B3%(P, B0%(1, 0)), 16) Then A5%(1) = 2: A5%(3) = 1: A5%(0) = 0: A5%(2) = 2: Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 5051
    3223 If B0%(1, 0) = 0 And I = 9 And SO = 1 Or O%(0) > 1 And I = 9 And SO = 1 Then A5%(0) = 1: A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 3900
    A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: A5%(0) = 0: Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 5051
    3230 '*** wild pitch ***
    If S4% = 0 And SO <> 1 Then 3100
    Call WILDPITCH(S2%, D)
    3236 GoTo 3222

    3240 '*** base on balls ***
    Call BASEONBALLS(S2%, D2, D, P)
    GoTo 3201


    3244 S2% = 2: Call BLANKLINES: Call PBP(B$(D, B9%(D, 2)) + " is called for interference!!")
    Call PBP(Q$(0) + " trots to first base")
    GoTo 3201


    3246 Call BLANKLINES: Call PBP(Q$(0) + " is out for obstruction!")
    W% = 2: Call CREDITREGPO(W%, D): O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): GoTo 3248
    I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6)
    3248 I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    GoTo 3900

    3500 '*** double s2%=8 ***
    Call GETOUTFIELDER(W%, P, I1, D)
    I5 = P1%(D): I6 = 1: Call ADDPIT(D, I5, I6): Call BLANKLINES
    Call DELIVERY(D)
    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P)): I6 = 0
        Call INCBATOFF(P, I5, I6)
        I6 = 1
        Call INCBATOFF(P, I5, I6)
        I6 = 4
        Call INCBATOFF(P, I5, I6)
    Else
        I5 = P1%(P): I6 = 23
        Call INCPITOFF(P, I5, I6)
        I6 = 24
        Call INCPITOFF(P, I5, I6)
        I6 = 27
        Call INCPITOFF(P, I5, I6)
    End If
    Call RANDNUM(RN)
    If RN <= 40 And W% = 7 Then
        Y% = Int(Rnd(1) * 40) + 2900
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        Y% = Int(Rnd(1) * 30) + 3020
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        GoTo 3502
    End If

    If RN <= 40 And W% = 9 Then
        Y% = Int(Rnd(1) * 40) + 2940
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        Y% = Int(Rnd(1) * 30) + 3020
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        GoTo 3502
    End If

    If RN <= 40 And W% = 8 Then
        Y% = Int(Rnd(1) * 40) + 2980
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        Y% = Int(Rnd(1) * 30) + 3020
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        GoTo 3502
    End If


    Call RANDNUM(RN)
    If RN <= 25 Then
        Call LONGFLY(D, W%)
        HW% = 1
        Call WALLDOUBLE(HW%, W%)
        GoTo 3502
    End If
    If RN > 25 And RN <= 50 Then
        F$ = Q$(0): Call LDRIVE
        F$ = F$ + "one to the gap in "
        Call OUTFLDLOCATION(W%)
        Call PBP(F$)
        GoTo 3502
    End If
    If RN > 50 And RN <= 60 Then
        F$ = Q$(0) + " hits a sinking liner into "
        Call OUTFLDLOCATION(W%)
        Call PBP(F$): Call PBP("It goes to the wall")
        GoTo 3502
    End If
    If RN > 60 And RN <= 75 Then
        F$ = Q$(0) + " lines one to the alley in "
        Call OUTFLDLOCATION(W%)
        Call PBP(F$)
        GoTo 3502
    End If
    If RN > 75 And RN <= 85 And (W% = 7 Or W% = 9) Then
        F$ = Q$(0)
        Call LDRIVE
        F$ = F$ + "one down the " + C$(W%) + " line": Call PBP(F$)
        Call PBP("It rolls into the corner")
        GoTo 3502
    End If
    If RN > 75 And RN <= 85 And W% = 8 Then
        F$ = Q$(0): Call LDRIVE
        F$ = F$ + "one between the fielders": Call PBP(F$)
        Call PBP(Q$(4) + " cuts it off...")
        GoTo 3502
    End If
    If RN > 85 And RN <= 92 Then
        F$ = Q$(0): Call LDRIVE
        F$ = F$ + "a blue darter into " + C$(W%)
        Call PBP(F$)
        GoTo 3502
    End If
    If RN > 92 Then
        F$ = Q$(0) + " lines one to "
        Call OUTFLDLOCATION(W%)
        Call PBP(F$)
        Call PBP("It's down and to the wall!")
    End If
    3502 HW% = 0
    If Int(Rnd(1) * 100) + 1 <= 80 Then F$ = "Double!!" Else F$ = "2-Bagger!!"
    Call PBP(F$)

    For I = 0 To 3: A5%(I) = 2: Next I: A5%(3) = 1
    If B0%(1, 0) = 0 Then 3511
    H6% = Int(Rnd(1) * 100) + 1
    BR% = B%(P, B3%(P, B0%(1, 0)), 16)
    If B7%(P, B0%(1, 0)) = 1 Then BR% = P%(P, P1%(P), 31)
    I3 = 0
    If O%(0) = 0 And W% = 7 Then I3 = 18 + BR% * 3
    If O%(0) = 1 And W% = 7 Then I3 = 16 + BR% * 3
    If O%(0) = 2 And W% = 7 Then I3 = 34 + BR% * 3
    If O%(0) = 0 And W% = 8 Then I3 = 37 + BR% * 3
    If O%(0) = 1 And W% = 8 Then I3 = 44 + BR% * 3
    If O%(0) = 2 And W% = 8 Then I3 = 65 + BR% * 3
    If O%(0) = 0 And W% = 9 Then I3 = 12 + BR% * 3
    If O%(0) = 1 And W% = 9 Then I3 = 18 + BR% * 3
    If O%(0) = 2 And W% = 9 Then I3 = 40 + BR% * 3
    If H6% <= I3 Or P2 = 2 Then A5%(1) = 3
    3511 Call ADVANCEMENT(P, D, S2%, F%, I3)
    F% = 0
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 0 Then
        Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9)
    End If
    Call SCOREBOARD
    GoTo 3900

    3520 '*** TRIPLE S2%=9 ***
    Call OUTFLDNAME(W%, D)
    Call DELIVERY(D)
    Call BLANKLINES
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 5: Call INCBATOFF(P, I5, I6): GoTo 3522
    I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 28: Call INCPITOFF(P, I5, I6)
    3522 I5 = P1%(D): I6 = 1: Call ADDPIT(D, I5, I6)
    Call RANDNUM(RN)
    If RN <= 30 And W% = 7 Then
        Y% = Int(Rnd(1) * 40) + 2900
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        Y% = Int(Rnd(1) * 40) + 3050
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        GoTo 3524
    End If

    If RN <= 30 And W% = 9 Then
        Y% = Int(Rnd(1) * 40) + 2940
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        Y% = Int(Rnd(1) * 40) + 3050
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        GoTo 3524
    End If

    If RN <= 30 And W% = 8 Then
        Y% = Int(Rnd(1) * 40) + 2980
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        Y% = Int(Rnd(1) * 40) + 3050
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)
        GoTo 3524
    End If


    Call RANDNUM(RN)

    If RN <= 10 And W% <> 8 Then 3570
    Call LONGFLY(D, W%)
    ad$ = "wall": If W% = 7 And L%(1, 13) = 27 Then ad$ = "screen"
    bv = Int(Rnd(1) * 100) + 1: Call RANDNUM(RN)
    If RN <= 20 Then F$ = "It one-hops the " + ad$ + "..."
    If RN <= 40 And RN > 20 Then F$ = "It's off the top of the " + ad$ + "..."
    If RN <= 60 And RN > 40 Then Call PBP("It bounces fair"): F$ = "down into the corner..."
    If RN <= 80 And RN > 60 Then Call PBP("It's in the gap"): F$ = "It rolls to the " + ad$
    If RN <= 99 And RN > 80 Then F$ = "It hits at the base of the " + ad$ + "..."
    If RN > 99 Then F$ = "The outfielders cross"
    Call PBP(F$)
    3524 Call RANDNUM(RN)
    If RN <= 90 Then F$ = "Triple!!"
    If RN > 90 Then F$ = "3-Bagger!"
    Call PBP(F$)
    3525 A5%(3) = 1: A5%(0) = 3: A5%(1) = 3: A5%(2) = 2
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    F% = 0
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9)
    Call SCOREBOARD: GoTo 3900

    3530 '*** HOME RUN S2%=10 ***
    Call BLANKLINES
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 6: Call INCBATOFF(P, I5, I6)
    I5 = P1%(D): I6 = 1: Call ADDPIT(D, I5, I6)
    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 29: Call INCPITOFF(P, I5, I6)
    Call RANDNUM(RN)
    If RN <= 80 Then Call LONGFLY(D, W%): GoTo 3535
    Call RANDNUM(RN)
    If RN <= 20 And (W% = 7 Or W% = 9) Then
        F$ = Q$(0) + " hits it down the " + C$(W%) + " line"
        Call PBP(F$)
        Call PBP("It's long enough if it's fair...")
        GoTo 3536
    End If
    Call PBP(Q$(0) + " swings...")
    If RN > 20 And RN <= 40 Then Call PBP("NO DOUBT ABOUT THAT ONE!!"): F$ = "IT'S WAY OUT OF HERE!!"
    If RN > 40 And RN <= 60 Then F$ = Q$(4) + " CAN ONLY LOOK UP!!"
    If RN > 60 And RN <= 80 Then F$ = P$(D, P1%(D)) + " WATCHES IT SAIL OUT!!"
    If RN > 80 And RN <= 90 Then F$ = "GONE!!!"
    If RN > 90 Then F$ = "SAINTS AND HAYRAKES, LOOK AT 'ER SAIL!!"
    Call PBP(F$)
    3536 Call PBP("HOME RUN!!"): Call DELAY: GoTo 3544
    3535 Call RANDNUM(RN)
    If RN <= 5 And L%(1, 13) = 8 And W% = 7 Then Call PBP("THAT'S OUT OF THE PARK"): Call PBP("ON WAVELAND AVENUE!!!"): F$ = "HOME RUN...HOLY COW!!!": GoTo 3543
    If RN <= 15 And L%(1, 13) = 8 And W% = 7 Then Call PBP("IT'S GONE"): F$ = "LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!": GoTo 3543
    If RN <= 25 Then 3580
    3545 Call RANDNUM(RN)
    If RN <= 7 Then F$ = "GOODBYE!! IT'S A HOME RUN!!"
    If RN > 7 And RN <= 14 Then F$ = "ADIOS!! IT'S A HOME RUN !!!"
    If RN > 14 And RN <= 21 Then F$ = "SAY GOOD-BYE TO THAT ONE!!!"
    If RN > 21 And RN <= 28 Then F$ = "TOUCH 'EM ALL!!"
    If RN > 28 And RN <= 35 Then F$ = "SEE YOU LATER!!"
    If RN > 35 And RN <= 42 Then F$ = "GOOD-BYE MR. SPALDING!! HOME RUN!!!"
    If RN > 42 And RN <= 49 Then F$ = "HOME RUN!! MY OH MY!!"
    If RN > 49 And RN <= 56 Then F$ = "IT'S OVER THE WALL...OH DOCTOR!!!"
    If RN > 56 And RN <= 63 Then F$ = "LONG, LONG, AND GONE!!"
    If RN > 63 And RN <= 70 Then F$ = "HEY! HEY!...IT'S A HOME RUN !!!"
    If RN > 70 And RN <= 77 Then F$ = "IT'S LONG, IT'S LONG, IT'S GONE!!!"
    If RN > 77 And RN <= 84 Then F$ = "HOLY COW!! IT'S A HOME RUN!!!"
    If RN > 84 And RN <= 89 Then F$ = "OPEN THE WINDOW AUNT MINNIE...HERE SHE COMES"
    If RN > 89 And RN <= 95 Then Call PBP("WAAYYY BACK..."): F$ = "YOU CAN PUT IT ON THE BOARD, YYESSS!!"
    If RN > 95 Then Call PBP("IT'S WAY OUT OF HERE!! HOME RUN!!!"): F$ = "HOW 'BOUT THAT!!!"
    3543 Call PBP(F$): Call DELAY
    3544 A5%(3) = 1: A5%(2) = 2: A5%(1) = 3: A5%(0) = 4
    I5 = P1%(D): I6 = 14: Call ADDPIT(D, I5, I6)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call SCOREBOARD
    GoTo 3900

    3570 Call RANDNUM(RN): X$ = "1st"
    If W% = 7 Then X$ = "3rd"
    If RN <= 50 Then Call PBP(Q$(0) + " hits a hot smash")
    If RN > 50 Then Call PBP(Q$(0) + " lines one")
    Call PBP("Fair past " + X$ + "!!"): Call PBP("It rolls into the " + C$(W%) + " corner")
    Call PBP("It's a triple!!"): GoTo 3525
    3580 If L%(1, 13) = 21 And W% = 7 Then Call PBP("IT CLEARS THE GREEN MONSTER!!"): F$ = "HOME RUN!!!": GoTo 3543
    If L%(1, 13) = 27 And W% = 7 Then Call PBP("IT CLEARS THE SCREEN!!"): F$ = "HOME RUN!!!": GoTo 3543
    If L%(1, 13) = 33 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): F$ = "THE BUSCH EAGLE FLAPS ITS WINGS": GoTo 3543
    If L%(1, 13) = 25 And P = 1 Then Call PBP("IT'S OUT OF HERE FOR A HOMER!!!"): F$ = "THERE GOES THE EXPLODING SCOREBOARD!": GoTo 3543
    If L%(1, 13) = 7 And W% = 9 Then Call PBP("IT CLEARS THE WAGON GATE!!"): F$ = "THAT'S A HOME RUN!!": GoTo 3543
    If L%(1, 13) = 7 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): F$ = "THERE GO THE CANNONS!!": GoTo 3543
    If L%(1, 13) = 8 Or L%(1, 13) = 40 Then F$ = "IT CLEARS THE IVY FOR A HOME RUN!!": GoTo 3543
    If L%(1, 13) = 23 And (W% = 7 Or W% = 9) Then Call PBP("IT'S LONG, IT'S LONG, IT'S GONE!!"): F$ = "THAT ONE'S IN THE UPPER DECK": GoTo 3543
    GoTo 3545


    3600 '*** strikeout ***
    H6% = Int(Rnd(1) * 1000) + 1
    If H6% > B4%(P, 4, B1!(P)) Then 3610


    3601 Call BLANKLINES
    Call STRIKEOUT(U%, D)
    3602 W% = 2: Call CREDITREGPO(W%, D)
    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    SO = 1
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 8: Call INCBATOFF(P, I5, I6): If P2 = 2 Then U% = 3
    I5 = P1%(D): I6 = 5: Call ADDPIT(D, I5, I6): I6 = 0: Call ADDPIT(D, I5, I6)
    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 31: Call INCPITOFF(P, I5, I6): If P2 = 2 Then U% = 3
    If P2 = 2 And O%(0) < 3 Then SO = 7: GoTo 4800

    '*** WILD PITCH OR PASSED BALL ON A STRIKEOUT
    I = Int(Rnd(1) * 600) + 1
    If B0%(1, 0) = 0 And I = 9 And SO = 1 Or O%(0) = 3 And I = 9 Then
        O%(0) = O%(0) - 1: O%(1) = O%(1) - 1
        I5 = P1%(D): P8%(D, I5, 0) = P8%(D, I5, 0) - 1
        P5%(D, I5, 0) = P5%(D, I5, 0) - 1
        B8%(D, B9%(D, 2), 16) = B8%(D, B9%(D, 2), 16) - 1
        B5%(D, B9%(D, 2), 16) = B5%(D, B9%(D, 2), 16) - 1
        I1 = Int(Rnd(1) * 100) + 1: If I1 < 77 Then 3230 Else GoTo 3220
    End If
    GoTo 3900


    3610 '*** ground ball for out ***
    H6% = Int(Rnd(1) * 100) + 1
    E2% = B%(P, B3%(P, B1!(P)), 18) + P%(D, P1%(D), 12) - 52
    E3% = 23 + P%(D, P1%(D), 12)
    E4% = 0: If H6% > E2% And B7%(P, B1!(P)) <> 1 Then E4% = 1
    If H6% > E3% And B7%(P, B1!(P)) = 1 Then E4% = 1
    If E4% = 1 Then 4000
    S2% = 13
    Call GETINFIELDER(W%, P, I1, D, S2%, P2): Call BLANKLINES: S2% = 13
    Call DELIVERY(D)
    Call GROUNDERPBP(W%, D)
    If W% = 2 Then 3612
    If W% = 1 Then F$ = F$ + "the pitcher": Call PBP(F$): GoTo 3612
    F$ = F$ + F$(W%): Call PBP(F$)
    3612 If (S4% = 1 Or S4% = 4 Or ((S4% = 7 Or S4% = 5) And INFPOS% < 2)) And O%(0) <= 1 And RN <= 50 Then
        Call DPPBP
        DPC% = 1
    End If
    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P)): I6 = 0
        Call INCBATOFF(P, I5, I6)
    Else
        I5 = P1%(P): I6 = 23
        Call INCPITOFF(P, I5, I6)
    End If
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 3900
    '*** 2 outs, bases empty, runner 2nd, runner 3rd, or 2nd&3rd ***
    If O%(0) = 2 Or S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% = 6 Then 3650
    H6% = Int(Rnd(1) * 1000) + 1: E2% = ((L%(D, 5) - 80) * 4)
    E3% = ((5 - B%(P, B3%(P, B1!(P)), 16)) * 9)
    If W% = 2 And H6% > 250 + E2% + E3% Then 3650
    If H6% > 490 + E2% + E3% Then 3650
    If P2 = 2 Then 3650
    S2% = 13
    If S4% = 4 And W% = 5 And Int(Rnd(1) * 3) + 1 = 1 Then 3633
    If S4% = 4 And W% = 2 Then Call CREDITASSIST(W%, D): GoTo 3633
    If S4% = 7 And W% = 1 Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He throws home"): GoTo 3631
    If S4% = 5 And S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 7 And O%(0) = 0 And INFPOS% > 0 And W% = 5 Or S4% = 5 And S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 7 And O%(0) = 0 And INFPOS% > 0 And W% = 3 Then 3650
    If S4% = 7 And INFPOS% > 0 And (W% = 5 Or W% = 3) Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He fires it home"): GoTo 3631
    If S4% = 7 And INFPOS% = 2 And W% = 4 Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He throws home for the force"): GoTo 3631
    If S4% = 7 And INFPOS% = 2 And W% = 6 Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He goes home with it"): GoTo 3631
    If W% <= 4 Then
        Call CREDITASSIST(W%, D)
        W% = 6
        Call PBP("He throws to " + B$(D, B9%(D, 6)))
    Else
        Call CREDITASSIST(W%, D)
        W% = 4
        Call PBP("He throws to " + B$(D, B9%(D, 4)))
    End If

    A5%(1) = 11: A5%(2) = 0: A5%(3) = 0: A5%(0) = 0

    If S4% = 7 And (W% = 2 Or W% = 1) Then
        A5%(3) = 11: A5%(2) = 1: A5%(1) = 1: A5%(0) = 1
    End If
    GoTo 3634

    3631 A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 3634
    3633 A5%(3) = 0: A5%(2) = 11: A5%(1) = 0: A5%(0) = 0
    3634 I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6): O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    Q$(4) = B$(D, B9%(D, W%))


    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITREGPO(W%, D)


    S2% = 15
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)


    If F% = 1 Then 3900
    Call CREDITASSIST(W%, D)
    If W% = 5 And O%(0) < 2 And Int(Rnd(1) * 10) + 1 <= B%(P, B3%(P, B0%(1, 0)), 16) Then 3643
    A5%(0) = 11: A5%(1) = 1: A5%(2) = 0: A5%(3) = 0
    If O%(0) < 2 Then A5%(2) = 1: A5%(3) = 1
    GoTo 3644
    3643 A5%(0) = 1: A5%(1) = 11: A5%(2) = 0: A5%(3) = 0
    If O%(0) < 2 Then A5%(2) = 1: A5%(3) = 1
    3644 O%(0) = O%(0) + 1: O%(1) = 2: I5 = P1%(D): I6 = 0
    Call DPRELAY
    Call ADDPIT(D, I5, I6)

    Call ADVANCEMENT(P, D, S2%, F%, I3)


    Call CREDITPUTOUT(I3, FB2P%, AST%, D)


    Call RANDNUM(RN)
    DPC% = 0
    If RN <= 80 Then F$ = " --- DOUBLE PLAY ---"
    If RN > 80 Then F$ = " --- TWIN KILLING ---"
    Call PBP(F$)
    DP%(D) = DP%(D) + 1: D3%(D) = D3%(D) + 1
    TB = 0: Call INCREMENTHIT(P, D, MO): GoTo 3900
    3650 If O%(0) < 2 Then 3690
    I = Int(Rnd(1) * 10) + 1: S2% = 13
    If W% = 5 And I <= 5 And (S4% = 4 Or S4% = 7) Then Call PITCHERNAME(W%, D): Call PBP(Q$(4) + " gloves it and heads for 3rd"): GoTo 3670
    If W% = 5 Then Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): Call THROW2FIRST(D): GoTo 3672
    If W% = 6 And I <= 8 And (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): Call DPPIVOT(W%, D): AST% = 6: GoTo 3671
    If W% = 6 Then Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): Call THROW2FIRST(D): GoTo 3672
    If W% = 4 And I <= 5 And (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): Call DPPIVOT(W%, D): AST% = 4: GoTo 3671
    Call RANDNUM(RN)
    If W% = 3 And RN <= 94 Then Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): Call PBP("He takes it to the bag unassisted"): fbu% = 1: GoTo 3672
    If W% = 3 Then Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): W% = 1: Call PITCHERNAME(W%, D): Call PBP("He flips it to " + Q$(4) + " covering"): FB2P% = 1: W% = 3: GoTo 3672
    Call PITCHERNAME(W%, D): Call GROUNDERS(W%, D): Call SECOND2FIRST(D): GoTo 3672
    3670 A5%(0) = 0: A5%(1) = 0: A5%(2) = 11: A5%(3) = 0: GoTo 3684
    3671 A5%(0) = 0: A5%(1) = 11: A5%(2) = 0: A5%(3) = 0: GoTo 3684
    3672 A5%(0) = 11: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0

    3684 If fbu% = 0 Then Call CREDITASSIST(W%, D)
    fbu% = 0
    If DPC% = 1 And A5%(0) = 1 Then DPC% = 2
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    If DPC% = 2 Then
        DPC% = 0
        Call DPSAFE
    End If
    If I3 > 4 Then I3 = 1
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)


    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    GoTo 3900

    3690 I = Int(Rnd(1) * 100) + 1
    If I <= 90 And P2 <> 2 Or S4% = 0 Then 3720
    If I <= 10 And P2 = 2 Then 3720
    S2% = 13: A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: A5%(0) = 11
    I = Int(Rnd(1) * 100) + 1
    If S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7 Then 3713
    3693 I = Int(Rnd(1) * 100) + 1
    A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
    If W% = 5 And I <= 25 And INFPOS% < 2 Then A5%(2) = 1
    If W% = 5 And I <= 83 And INFPOS% = 0 Then A5%(3) = 1: Rem WAS 50
    If W% = 5 And I <= 10 And INFPOS% > 0 Then A5%(3) = 1
    If W% = 5 And I < 5 And INFPOS% < 2 And S4% = 2 And S6%(P, 0) - S6%(D, 0) > -2 Or W% = 5 And I < 5 And INFPOS% < 2 And S4% = 6 And A5%(3) = 1 And S6%(P, 0) - S6%(D, 0) > -2 Then A5%(2) = 11: A5%(0) = 1: Call PBP("NO, he's going to third")
    If W% = 6 And I <= 20 And INFPOS% < 2 Then A5%(2) = 1
    If W% = 6 And I <= 92 And INFPOS% < 2 Then A5%(3) = 1
    If W% = 6 And I <= 5 And INFPOS% = 2 Then A5%(3) = 1
    If W% = 6 And I < 6 And INFPOS% < 2 And S4% = 2 And S6%(P, 0) - S6%(D, 0) > -2 Or W% = 6 And I < 6 And INFPOS% < 2 And S4% = 6 And A5%(3) = 1 And S6%(P, 0) - S6%(D, 0) > -2 Then A5%(2) = 11: A5%(0) = 1: Call PBP("NO, he's going to third")
    If W% = 4 And I <= 90 And INFPOS% < 2 Then A5%(2) = 1
    If W% = 4 And I <= 90 And INFPOS% < 2 Then A5%(3) = 1
    If W% = 4 And I <= 65 And INFPOS% = 2 Then A5%(2) = 1
    If W% = 4 And I <= 5 And INFPOS% = 2 Then A5%(3) = 1
    If W% = 3 And I <= 95 Then A5%(2) = 1
    If W% = 3 And I <= 88 And INFPOS% = 0 Then A5%(3) = 1: ' WAS 60
    If W% = 3 And I <= 15 And INFPOS% > 0 Then A5%(3) = 1
    If W% = 1 And I <= 30 Then A5%(2) = 1
    If W% = 1 And I <= 50 And INFPOS% = 0 Then A5%(3) = 1
    If W% = 1 And I <= 10 And INFPOS% > 0 Then A5%(3) = 1
    If W% = 2 And I <= 45 Then A5%(2) = 1
    3713 GoTo 3684
    3720 S2% = 14
    If S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% = 6 Then A5%(0) = 11: Call OUTATFIRST(W%, D, fbu%, FB2P%): GoTo 3693
    I = Int(Rnd(1) * 100) + 1
    A5%(0) = 1: A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: If INFPOS% = 0 And W% = 2 Then A5%(3) = 0
    If S4% = 5 Or S4% = 7 Or S4% = 4 Then 3725
    A5%(1) = 11: GoTo 3743
    3725 If S4% = 4 Or S4% = 7 Then 3732
    If S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 6 And O%(0) = 0 And INFPOS% > 0 And W% = 5 Or S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 6 And O%(0) = 0 And INFPOS% > 0 And W% = 3 Then A5%(3) = 11: GoTo 3743
    If INFPOS% = 0 And W% <> 2 Then A5%(1) = 11: GoTo 3743
    If INFPOS% > 0 And W% = 5 Or INFPOS% > 0 And W% = 3 Then A5%(3) = 0: A5%(1) = 11: GoTo 3743
    If INFPOS% > 0 And (W% = 1 Or W% = 3) Then A5%(3) = 0: A5%(1) = 1: A5%(0) = 11: GoTo 3743
    If INFPOS% = 2 And W% = 4 Or INFPOS% = 2 And W% = 6 And I <= 90 Then A5%(3) = 0: A5%(1) = 11: GoTo 3743
    A5%(1) = 11: GoTo 3743
    3732 If W% = 6 And I < 6 And INFPOS% < 2 And INNING% < 9 Or W% = 5 And I < 6 And INFPOS% < 2 And INNING% < 9 Then A5%(2) = 11: GoTo 3684
    If S4% = 7 Then 3740
    I = Int(Rnd(1) * 10) + 1: A5%(0) = 1: A5%(1) = 11: A5%(2) = 1: A5%(3) = 0
    If W% = 5 And I <= 5 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
    If W% = 1 And I <= 3 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
    If W% = 2 And B%(P, B3%(P, B0%(2, 0)), 16) < 7 And B%(P, B3%(P, B0%(2, 0)), 16) > 0 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0: GoTo 3743
    If W% = 2 And B%(P, B3%(P, B0%(2, 0)), 16) > 6 Then A5%(1) = 1: A5%(2) = 1: A5%(0) = 11
    GoTo 3743
    3740 I = Int(Rnd(1) * 100) + 1: A5%(1) = 11: A5%(2) = 1: A5%(3) = 1: A5%(0) = 1
    If INFPOS% > 0 And W% = 5 Or INFPOS% > 0 And W% = 3 Or INFPOS% = 2 And W% = 4 Or INFPOS% = 2 And W% = 6 Or W% = 2 Or W% = 1 Then A5%(3) = 11: A5%(0) = 1: A5%(1) = 1: A5%(2) = 1: GoTo 3743
    If W% = 5 And I <= 5 Or W% = 1 Or W% = 2 Then A5%(3) = 11: A5%(2) = 1: A5%(1) = 1: A5%(0) = 1
    3743 GoTo 3684


    3900 _PutImage (20, 48)-Step(44, 128), screen_handle, park_graphic
    'CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
    'CALL ClearVE
    I$ = InKey$
    If UCase$(I$) = "Q" Then Screen 0: End

    _PutImage (17, 128)-Step(46, 80), park_graphic, screen_handle
    'CALL GMove2VE (17,128,46,80,VA%(17),0)
    DPC% = 0: H0% = 0: INFPOS% = 0: F% = 0: PQ = 0: H0%(0) = 0
    If O%(0) = 3 Then 5000
    GoTo 5050
     
    3955 _PutImage (20, 48)-Step(44, 128), screen_handle, park_graphic
    'CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
    'CALL ClearVE
    Return

    4000 'balls hit to the air
    I = Int(Rnd(1) * 100) + 1
    A5%(0) = 0: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
    If I <= 73 Then 4020: Rem flyout
    If I <= 89 Then 4010: Rem popup
    S2% = 21
    Call GETINFIELDER(W%, P, I1, D, S2%, P2)
    Call BLANKLINES
    Call DELIVERY(D)

    Y% = Int(Rnd(1) * 42) + 70
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)

    Y% = Int(Rnd(1) * 34) + 120
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6)
    End If

    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 3900

    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    Call CREDITREGPO(W%, D)
    I5 = P1%(D): I6 = 0
    Call ADDPIT(D, I5, I6)
    If O%(0) = 3 Then 3900

    I = Int(Rnd(1) * 100) + 1
    If (S4% = 1 Or S4% = 5) And I >= 80 Or (S4% = 1 Or S4% = 5) And P2 = 2 Then 4012
    If (S4% = 4 Or S4% = 2) And I >= 80 Or S4% = 4 And P2 = 2 Then 4014
    4009 Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 3900

    4010 ' infield popup
    Call GETINFIELDER(W%, P, I1, D, S2%, P2): S2% = 20: Call BLANKLINES
    If W% = 1 Then W% = 2: Q$(4) = B$(D, B9%(D, W%))
    If (W% = 2 Or W% = 5 Or W% = 3) And Int(Rnd(1) * 100) + 1 <= 30 Then fo% = 1
    If fo% = 0 And (S4% = 4 Or S4% = 7) And O%(0) < 2 Then Call PBP("INFIELD FLY RULE CALLED")
    If fo% = 1 And W% = 2 Then Y% = Int(Rnd(1) * 24) + 180
    If fo% = 1 And (W% = 3 Or W% = 4) Then Y% = Int(Rnd(1) * 31) + 210
    If fo% = 1 And (W% = 5 Or W% = 6) Then Y% = Int(Rnd(1) * 31) + 250
    If fo% <> 1 And W% = 2 Then Y% = Int(Rnd(1) * 50) + 400
    If fo% <> 1 And W% = 3 Then Y% = Int(Rnd(1) * 60) + 450
    If fo% <> 1 And W% = 5 Then Y% = Int(Rnd(1) * 60) + 510
    If fo% <> 1 And W% = 6 Then Y% = Int(Rnd(1) * 60) + 570
    If fo% <> 1 And W% = 4 Then Y% = Int(Rnd(1) * 60) + 630
    Call GETRPBP(Y%, Y$, D): Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)

    If fo% = 1 And W% = 2 Then Y% = Int(Rnd(1) * 12) + 321
    If fo% = 1 And (W% = 3 Or W% = 4) Then Y% = Int(Rnd(1) * 25) + 340
    If fo% = 1 And (W% = 5 Or W% = 6) Then Y% = Int(Rnd(1) * 25) + 370
    If fo% <> 1 Then Y% = Int(Rnd(1) * 25) + 690
    Call GETRPBP(Y%, Y$, D): Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)


    4011 If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P): I6 = 23
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P)): I6 = 0
        Call INCBATOFF(P, I5, I6)
    End If

    If S4% = 4 Then 4017
    If Int(Rnd(1) * 100) + 1 <= 25 And (L%(1, 13) = 11 Or L%(1, 13) = 8) Then Call PBP("fights the wind...")
    If Int(Rnd(1) * 100) + 1 <= 15 And (L%(1, 13) = 2 Or L%(1, 13) = 4 Or L%(1, 13) = 13 Or L%(1, 13) = 24) Then Call PBP("fights the lights...")
    If fo% <> 1 Then Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900

    4017 fo% = 0
    Call PBP(Q$(4) + " makes the catch")
    Call CREDITREGPO(W%, D)
    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    I5 = P1%(D): I6 = 0
    Call ADDPIT(D, I5, I6)
    GoTo 3900

    4012 'runner doubled off of 1st
    A5%(1) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    Call ADDPIT(D, I5, I6)
    D3%(D) = D3%(D) + 1: DP%(D) = DP%(D) + 1
    If W% <> 3 Then
        Y% = Int(Rnd(1) * 10) + 160
    Else
        Y% = Int(Rnd(1) * 10) + 170
    End If
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)
    I3 = 1
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    Call PBP(" --- DOUBLE PLAY ---")
    If W% <> 3 Then Call CREDITASSIST(W%, D)
    GoTo 4009

    4014 'runner doubled off of 2nd
    A5%(2) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    Call ADDPIT(D, I5, I6)
    D3%(D) = D3%(D) + 1: DP%(D) = DP%(D) + 1
    I3 = 2
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    Call PBP(" --- DOUBLE PLAY ---")
    Call CREDITASSIST(W%, D)
    GoTo 4009




    4020 '*** FLY  OUTS ***
    Call GETOUTFIELDER(W%, P, I1, D)
    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P)): I6 = 0
        Call INCBATOFF(P, I5, I6)
    Else
        I5 = P1%(P): I6 = 23
        Call INCPITOFF(P, I5, I6)
    End If
    I = Int(Rnd(1) * 100) + 1
    If I <= 17 Then 4050
    If I <= 33 Then 4060
    '*** regular fly out ***
    S2% = 18: Call BLANKLINES
    X$ = C$(W%)
    If Int(Rnd(1) * 100) <= 25 And W% = 7 Then X$ = "lcf"
    If Int(Rnd(1) * 100) <= 25 And W% = 9 Then X$ = "rcf"
    Call DELIVERY(D)
    Call RANDNUM(RN)
    Y% = Int(Rnd(1) * 90) + 1070
    If (W% = 7 Or W% = 9) And RN <= 10 Then X$ = "": Y% = Int(Rnd(1) * 10) + 1160
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$ + " " + X$)
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 3900
    4025 Call CREDITREGPO(W%, D)
    Call CAUGHTFLY(I, S2%, D)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    If O%(0) = 3 Then 3900
    I = Int(Rnd(1) * 100) + 1
    If S2% = 19 And S4% = 1 And I <= 33 And P2 = 2 Or S2% = 19 And S4% = 5 And I <= 33 Then
        A5%(1) = 10
        O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
        Call ADDPIT(D, I5, I6)
        D3%(D) = D3%(D) + 1: DP%(D) = DP%(D) + 1
        Call CREDITASSIST(W%, D)
        GoTo 4009
    End If
    If B0%(3, 0) > 0 Then I8 = 4: LN% = 3308: Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9)
    If I9 = 1 Then I9 = 0: GoTo 4074
    GoTo 4066

    4050 '*** shallow fly ball ***
    S2% = 19
    Call BLANKLINES
    X$ = C$(W%)
    If Int(Rnd(1) * 100) <= 25 And W% = 7 Then X$ = "lcf"
    If Int(Rnd(1) * 100) <= 25 And W% = 9 Then X$ = "rcf"
    Call DELIVERY(D)
    Call RANDNUM(RN)
    Y% = Int(Rnd(1) * 60) + 1170
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$ + " " + X$)
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 3900
    GoTo 4025

    4060 '*** deep fly out ***
    S2% = 17
    Call BLANKLINES
    Call LONGFLY(D, W%)
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 3900
    Call CREDITREGPO(W%, D)
    Call CAUGHTFLY(I, S2%, D)
    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    I5 = P1%(D): I6 = 0
    Call ADDPIT(D, I5, I6)
    If O%(0) = 3 Then 3900
    If B0%(3, 0) = 0 Then 4066
    A5%(3) = 1: I5 = P1%(D): I6 = 16: Call ADDPIT(D, I5, I6)
    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call PITOFF(P, I5, I6): I5 = P1%(P): I6 = 41: Call INCPITOFF(P, I5, I6): GoTo 4066
    I5 = B3%(P, B1!(P)): I6 = 0: Call BATOFF(P, I5, I6): I5 = B3%(P, B1!(P)): I6 = 19: Call INCBATOFF(P, I5, I6)

    4066 If B0%(2, 0) = 0 Then 4072
    I4 = (B%(P, B3%(P, B0%(2, 0)), 16) - 5) * 25: ZM = (B%(D, B9%(D, W%), 15) - 5) * 25
    I4 = I4 - ZM
    I = Int(Rnd(1) * 1000) + 1
    If W% = 9 And S2% = 17 And I <= 800 + I4 And P2 <> 2 Or W% = 9 And S2% = 18 And I <= 400 + I4 And P2 <> 2 Or W% = 8 And S2% = 17 And I <= 700 + I4 And P2 <> 2 Or W% = 8 And S2% = 18 And I <= 350 + I4 And P2 <> 2 Or W% = 7 And S2% = 17 And I <= 250 + I4 And P2 <> 2 Then A5%(2) = 1
    If P2 = 2 And B%(P, B3%(P, B0%(2, 0)), 16) > 6 And W% = 9 And S2% = 17 And I <= 400 + I4 Or P2 = 2 And B%(P, B3%(P, B0%(2, 0)), 16) > 6 And W% = 8 And S2% = 17 And I <= 400 + I4 Then A5%(2) = 1
    If A5%(2) <> 1 And S2% = 18 And W% = 9 And P2 <> 2 And S4% < 6 And I > 800 Then I8 = 3: LN% = 3308: Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9): GoTo 4072
    If A5%(2) <> 1 And S2% = 18 And W% = 8 And P2 <> 2 And S4% < 6 And I > 840 Then I8 = 3: LN% = 3308: Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9): GoTo 4072
    If A5%(2) <> 1 And W% = 7 And P2 <> 2 And S4% < 6 And I > 880 Then I8 = 3: LN% = 3308: Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9)
    4072 If B0%(1, 0) = 0 Then 4074
    I = Int(Rnd(1) * 1000) + 1: A5%(1) = 0
    I3 = (B%(P, B3%(P, B0%(1, 0)), 16) - 5) * 25
    I4 = (B%(D, B9%(D, W%), 15) - 5) * 25: J5 = I3 - I4
    If S2% = 17 And P2 <> 2 And S4% = 1 And I <= 100 + J5 Then I8 = 2: LN% = 3308: Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8, I9)
    4074 Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 3900
    4095 I$=GetKeyPress$
    Return
    4100 '*** BUNT ROUTINE ***
    Call BLANKLINES: For I = 0 To 3: A5%(I) = 0: Next: SH = 2: S2% = 23
    If (S4% = 3 Or S4% > 4) And O%(0) < 2 Then Call SQUEEZECHOICE(SH)
    Call RANDNUM(RN)
    If B7%(P, B1!(P)) = 1 Then BR = P%(P, P1%(P), 32)
    If B7%(P, B1!(P)) <> 1 Then BR = B%(P, B3%(P, B1!(P)), 30)
    If INFPOS% > 0 Then BR = BR - 2
    If BR <= 1 Then BR = 1
    For I = 1 To 9: If RN <= BT%(L%(1, 10), BR, I) Then 4107
    Next
    4107 WW% = Int(Rnd(1) * 10)
    If WW% <= 2 Then W% = 5
    If WW% > 2 And WW% <= 4 Then W% = 1
    If WW% > 4 And WW% <= 7 Then W% = 3
    If WW% > 7 Then W% = 2
    Call GETFIELDNAME(W%, D)
    Call RANDNUM(RN)
    If RN <= 52 Then 4175
    On I GOTO 4110, 4120, 4130, 4140, 4150, 4160, 4170, 4180, 4175
    4110 'PU
    Call PBP(Q$(0) + " bunts it in the air...")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    Call PBP(Q$(4) + " makes the catch")
    Call ADDSTTS(P, D)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: Call CREDITREGPO(W%, D)
    If O%(0) = 3 Or SH = 2 Then 3900
    4111 If W% <> 5 Then Call CREDITASSIST(W%, D): W% = 5: Call CREDITREGPO(W%, D): Q$(5) = Q$(4): Call GETFIELDNAME(W%, D): GoTo 4115
    Call CREDITASSIST(W%, D): W% = 6: Call CREDITREGPO(W%, D): Q$(5) = Q$(4): Call GETFIELDNAME(W%, D)
    4115 Call PBP(Q$(5) + " throws to " + Q$(4) + " at 3rd")
    A5%(3) = 10: Call ADVANCEMENT(P, D, S2%, F%, I3)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    If O%(0) < 3 And SH = 1 Then 5051
    If O%(0) >= 3 And SH = 1 Then B1!(P) = B1!(P) - 1
    GoTo 3900
    4120 'DP
    If S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7 Then 4125
    Call PBP(Q$(0) + " lays down the bunt...")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    Call PBP(Q$(4) + " makes a nice fielding play")
    Call CREDITASSIST(W%, D): A5%(0) = 11
    If SH = 1 And (S4% = 3 Or S4% = 6) Then A5%(3) = 11: For I = 0 To 2: A5%(I) = 1: Next
    4122 Call ADDSTTS(P, D)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: Call ADVANCEMENT(P, D, S2%, F%, I3): Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    GoTo 3900
    4125 Call PBP(Q$(0) + " lays down a good bunt")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    Call PBP(Q$(4) + " fields it cleanly")
    If SH = 1 And (S4% = 5 Or S4% = 7) Then 4127
    If S4% = 7 And O%(0) < 2 Then 4128
    Call PBP("He throws to 2nd"): Call CREDITASSIST(W%, D): A5%(1) = 11
    Call ADDSTTS(P, D)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: Call ADVANCEMENT(P, D, S2%, F%, I3): Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    If O%(0) >= 3 Then 3900
    A5%(0) = 11: Call CREDITASSIST(W%, D): A5%(2) = 1
    Call PBP("over to first..."): Call ADVANCEMENT(P, D, S2%, F%, I3): Call PBP("DOUBLE PLAY!!")
    Call CREDITPUTOUT(I3, FB2P%, AST%, D): O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    GoTo 3900
    4127 If W% = 2 Then Call PBP("and catches " + Q$(3) + " heading home!"): Call CREDITREGPO(W%, D)
    If W% <> 2 Then Call PBP("and throws home to nail " + Q$(3) + "!"): Call CREDITASSIST(W%, D): W% = 2: Call CREDITREGPO(W%, D)
    A5%(3) = 11: For I = 0 To 2: A5%(0) = 1: Next: GoTo 4122
    4128 A5%(3) = 11
    If W% = 2 Then Call PBP("He steps on home for one..."): Call CREDITREGPO(W%, D): Call CREDITASSIST(W%, D)
    If W% <> 2 Then Call PBP("He throws home for the force"): Call CREDITASSIST(W%, D): W% = 2: Call CREDITREGPO(W%, D): Call CREDITASSIST(W%, D)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call PBP("Back to first..."): A5%(0) = 11: Call ADVANCEMENT(P, D, S2%, F%, I3): Call CREDITPUTOUT(I3, FB2P%, AST%, D): Call PBP("DOUBLE PLAY!!")
    O%(0) = O%(0) + 2: O%(1) = O%(1) + 2
    Call ADDSTTS(P, D)
    I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    If O%(0) >= 3 Then 3900
    A5%(2) = 1: A5%(1) = 1: Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 3900
    4130 'LDRNRCTDN
    If S4% = 7 Or S4% = 6 Or S4% = 5 Or S4% = 3 Then A5%(3) = 11: For I = 0 To 2: A5%(I) = 1: Next
    If S4% = 2 Or S4% = 4 Then A5%(2) = 11: A5%(0) = 1: A5%(1) = 1
    If S4% = 1 Then A5%(1) = 11: A5%(0) = 1
    If S4% = 0 Then A5%(0) = 11
    If PLB = 0 Then Call PBP(Q$(0) + " bunts it hard!")
    If PLB = 0 Then Call PBP(Q$(4) + " makes an easy play")
    PLB = 0: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    Call ADDSTTS(P, D)
    Call CREDITASSIST(W%, D): Call ADVANCEMENT(P, D, S2%, F%, I3): Call CREDITPUTOUT(I3, FB2P%, AST%, D): GoTo 3900
    4140 'SHGOOD
    Call PBP(Q$(0) + " lays down a good bunt...")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    Call PBP(Q$(4) + " makes a nice play")
    If S4% = 0 Then
        S2% = -12
        Call PBP(Q$(0) + " beats out an infield hit!")
        GoTo 3065
    End If
    4141 I5 = P1%(D): I6 = 15: Call ADDPIT(D, I5, I6)
    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 40: Call INCPITOFF(P, I5, I6)
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 18: Call INCBATOFF(P, I5, I6)
    I = Int(Rnd(1) * 100) + 1
    If S4% = 5 And SH = 2 And I > 98 Or I > 97 Then
        S2% = -12
        Call PBP(Q$(0) + " beats it out!")
        GoTo 3065
    End If
    A5%(0) = 11
    Call PBP("The only play is to first")
    I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
    Call CREDITASSIST(W%, D): Call ADVANCEMENT(P, D, S2%, F%, I3): Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    If O%(0) >= 3 Then 3900
    For I = 1 To 3: A5%(I) = 1: Next: Call ADVANCEMENT(P, D, S2%, F%, I3)
    GoTo 3900
    4150 'C@3
    If SH = 1 Then 4160
    Call RANDNUM(RN)
    If B7%(P, B1!(P)) = 1 Then RR = 2
    If B7%(P, B1!(P)) <> 1 Then RR = B%(P, B3%(P, B1!(P)), 16)
    If INFPOS% > 0 Then RR = RR - 2
    If RR <= 2 Then RR = 2
    For I = 0 To 2
        If RN <= CF%(RR, I) Then 4155
    Next
    4155 On I + 1 GOTO 4156, 4157, 4158
    4156 If S4% > 0 Then 4157
    Call PBP(Q$(0) + " drops it right down the line")
    Call PBP(Q$(4) + " lets it roll...")
    Call PBP(Q$(0) + " is trying for 2nd!!")
    Call PBP(Q$(4) + " fields and throws too late!!")
    Call PBP("You won't see that play very often")
    A5%(0) = 2: I5 = P1%(D): I6 = 1: Call ADDPIT(D, I5, I6)
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6): I6 = 0: Call INCBATOFF(P, I5, I6)
    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)
    Call ADVANCEMENT(P, D, S2%, F%, I3): Call SCOREBOARD: GoTo 3900
    4157 S2% = 12
    Call PBP(Q$(0) + " lays down a great bunt...")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    Call PBP(Q$(4) + " fields... but too late!")
    S2% = -12
    GoTo 3065
    4158 GoTo 4140
    4160 'PLYLDBS
    If B7%(P, B1!(P)) = 1 Then RR = 2
    If B7%(P, B1!(P)) <> 1 Then RR = B%(P, B3%(P, B1!(P)), 16)
    If INFPOS% > 0 Then RR = RR - 2
    If RR <= 2 Then RR = 2
    If S4% = 0 Then 4150
    If S4% = 1 Then I3 = 1: J3$ = "2nd"
    If S4% = 2 Or S4% = 4 Then I3 = 2: J3$ = "3rd"
    If S4% = 3 Or S4% > 4 Then I3 = 3: J3$ = "home"
    Call PBP(Q$(0) + " gets the bunt down...")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    Call PBP(Q$(4) + " fields it quickly...")
    Call PBP("Looks like a close play at " + J3$ + "!!")
    Call PBP("1- Throw to " + J3$ + " 2- Throw to 1st")
    If U6 = 2 Or U6 = 1 And P <> U9 Then I$ = "1": GoTo 4162
    4161 I$=GetKeyPress$
    I5 = Val(I$): If I5 < 1 Or I5 > 2 Then 4161
    4162 If I5 = 2 Then 4141
    Call RANDNUM(RN)
    For I = 0 To 1
        If RN <= LB%(RR, I) Then 4163
    Next
    4163 If I = 0 Then PLB = 1: GoTo 4130
    Call PBP(Q$(4) + " throws to " + J3$ + "... SAFE!!")
    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6)
    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6)
    For I = 0 To 3: A5%(I) = 1: Next: Call ADVANCEMENT(P, D, S2%, F%, I3): GoTo 3900
    4170 'FLKSO
    Call RANDNUM(RN)
    If P%(D, P1%(D), 6) = 0 Then BR = 0: GoTo 4173
    BR = P%(D, P1%(D), 9) / P%(D, P1%(D), 6)
    4173 If BR <= 4 Then PR = 5
    If BR > 4 And BR <= 5.4 Then PR = 4
    If BR > 5.4 And BR <= 6.9 Then PR = 3
    If BR > 6.9 And BR <= 8.4 Then PR = 2
    If BR > 8.4 And BR <= 11.9 Then PR = 1
    If BR > 11.9 Then PR = 0
    For I = 0 To 2
        If RN <= SO%(PR, I) Then 4171
    Next
    4171 On I + 1 GOTO 4172, 4175, 4140
    4172 GoTo 3601
    4175 U% = U% + 1
    'IF U% = 3 THEN 3601
    _PrintString (29, 64), Str$(U%), park_graphic
    '--GPRINT0VE 29,64,STR$(U%),15

    Call BLANKLINES
    Call RANDNUM(RN)
    F$ = Q$(0) + " "
    If RN <= 13 Or SH = 1 And RN <= 42 Then F$ = F$ + "misses the pitch"
    If RN > 13 And RN <= 42 And SH <> 1 Then F$ = F$ + "takes a called strike"
    If RN > 42 Then F$ = F$ + "bunts it foul"
    Call PBP(F$)
    If U% = 3 Then 3601
    If SH = 1 And RN <= 42 Then W% = 2: Call GETFIELDNAME(W%, D): GoTo 4111
    F$ = "He has" + Str$(U%) + " strike"
    If U% > 1 Then F$ = "He has" + Str$(U%) + " strikes": Call PBP(F$)
    If S4% = 4 Or S4% = 2 Then INFPOS% = 0

    _PutImage (20, 48)-Step(44, 128), screen_handle, park_graphic
    'CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
    'CALL ClearVE

    GoTo 5080
    4180 'TFPLY
    Call PBP(Q$(0) + " lays down the bunt...")
    Call PBP(Q$(4) + " has a tough play")
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P): If F% = 1 Then 3900
    If W% = 1 Then RR = 3
    If W% <> 1 Then RR = B%(D, B9%(D, W%), 17)
    If INFPOS% > 0 And S4% <> 0 Then RR = RR + 1 + L%(1, 10)
    If INFPOS% > 0 And S4% = 0 Then RR = RR + L%(1, 10)
    If INFPOS% = 0 And S4% = 0 Then RR = RR - 1
    If RR > 11 Then RR = 11
    If RR < 0 Then RR = 0
    Call RANDNUM(RN)
    For I = 0 To 2
        If RN <= TP%(RR, I) Then 4182
    Next
    4182 On I + 1 GOTO 4183, 4184, 4185
    4183 S2% = 12
    Call PBP("but too late...base hit!")
    GoTo 3065
    4184 PLB = 1: GoTo 4130
    4185 GoTo 4141

    4795 If P2 <> 2 Then
        Call BLANKLINES
        SB% = 1
        Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
        SB% = 0
        If F% = 1 Then
            B1!(P) = B1!(P) - 1
            Call WHOSON1ST(S4%)
            GoTo 3900
        End If
    End If
    GoTo 4804

    4800 SH% = 0: S2% = 16: W% = 1: Q$(4) = P$(D, P1%(D))
    If S4% = 0 Then 5051
    If S4% = 1 Or S4% = 5 Then TGTBASE% = 2: GoTo 4795
    If S4% = 2 Or S4% = 4 Then TGTBASE% = 3: GoTo 4795
    If S4% = 3 Or S4% >= 6 Then TGTBASE% = 4
    4804 I2 = B3%(P, B0%(TGTBASE% - 1, 0))
    For I3 = 1 To 2
        A5%(I3) = 1
        A5%(3) = 0
    Next I3
    Call STEALCHANCE(AA, I2, TGTBASE%, CHANCE%)
    If B7%(P, B0%(TGTBASE% - 1, 0)) = 1 Then CHANCE% = 0
    If TGTBASE% = 4 Then CHANCE% = CHANCE% * .05: Rem *** REDUCTION FOR STEALING HOME ***
    4809 If B%(P, I2, 14) > 0 Then Q0 = Int((B%(P, I2, 13) / (B%(P, I2, 13) + B%(P, I2, 14))) * 1000): GoTo 4813
    If B%(P, I2, 13) > 31 Then Q0 = 700 + ((B%(P, I2, 13) - 30) * 2): GoTo 4813
    If B%(P, I2, 13) > 10 Then Q0 = 600 + ((B%(P, I2, 13) - 10) * 10): GoTo 4813
    If B%(P, I2, 13) > 0 Then Q0 = 400 + ((B%(P, I2, 13) * 20))
    Q0 = 100
    4813 If B7%(P, B0%(TGTBASE% - 1, 0)) = 1 Then Q0 = 100
    Call STEAL3RD(TGTBASE%, AA, Q0, H0%, D): Rem*** SUCCESS OF 3B STEAL ***
    If P%(D, P1%(D), 0) = 1 And TGTBASE% = 2 Then CHANCE% = CHANCE% + 25: Q0 = Q0 + 10
    If P%(D, P1%(D), 0) = -1 And TGTBASE% = 2 Then CHANCE% = CHANCE% - 50: Q0 = Q0 - 30
    If P2 = 2 Then 4838
    B0%(TGTBASE% - 1, 2) = B0%(TGTBASE% - 1, 2) + 1
    I4 = Int(Rnd(1) * 100) + 1
    If I4 > B0%(TGTBASE% - 1, 2) Then 4825

    S2% = 22: W% = 1: Q$(4) = P$(D, P1%(D))
    4820 Call BLANKLINES
    Call PBP(Q$(TGTBASE% - 1) + " caught off base in a rundown!")
    A5%(1) = 0: A5%(2) = 0: A5%(3) = 0: A5%(TGTBASE% - 1) = 11
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: Call PUTOUT(TGTBASE%, D, P)
    Call ADVANCEMENT(P, D, S2%, F%, I3): Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    If O%(0) >= 3 Then B1!(P) = B1!(P) - 1: GoTo 3900
    INFPOS% = 0
    GoTo 5051

    4825 'RUNNER HAS A CHANCE TO STEAL
    B0%(TGTBASE% - 1, 2) = B0%(TGTBASE% - 1, 2) + 17
    I9 = Int(Rnd(1) * 1000) + 1

    If I9 > CHANCE% * 1.388 And D5 <> 3 Then
        Call BLANKLINES
        Call NOJUMP(D)

        _PutImage (17, 128)-Step(46, 80), screen_handle, park_graphic
        'CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
        'CALL ClearVE

        GoTo 5060
    End If

    4826 Call BLANKLINES
    TGBASE% = B3%(P, B0%(TGTBASE% - 1, 0))
    Y% = Int(Rnd(1) * 20) + 3090
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)
    Y% = Int(Rnd(1) * 13) + 3110
    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$)
    Call PITCHOUT(S4%, TGTBASE%, Q0)
    If S4% <> 5 Then 4888: Rem*** DOUBLE STEAL ***1st&3rd
    If SH% = 1 Then I9 = Int(Rnd(1) * 1000) + 1: GoTo 4828
    If U6 = 2 Or U6 = 1 And P <> U9 Then I$ = "Y": GoTo 4887
    Call PBP("Throw to 2B (YN) ")
    4886 I$=GetKeyPress$
    4887 If UCase$(I$) <> "Y" Then PJ = 7: GoTo 4829
    4888 W% = 2: Q$(4) = B$(D, B9%(D, 2))
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then I5 = TGBASE%: I6 = 9: Call INCBATOFF(P, I5, I6): Call PITSWAP(TGTBASE%): B1!(P) = B1!(P) - 1: GoTo 3900
    4827 Call WHICHRUNNER(TGTBASE%, S4%, D5)
    I9 = Int(Rnd(1) * 1000) + 1
    Q0 = Q0 - ((B%(D, B9%(D, 2), 15) - 5) * 25)
    4828 RN = Int(Rnd(1) * 11) + 1
    If TGTBASE% = 4 Then 4837
    If RN = 1 Then F$ = B$(D, B9%(D, 2)) + " rifles it to " + H$(TGTBASE%)
    If RN = 2 Then F$ = B$(D, B9%(D, 2)) + " fires it to " + H$(TGTBASE%)
    If RN = 3 Then F$ = B$(D, B9%(D, 2)) + " unloads it"
    If RN = 4 Then F$ = B$(D, B9%(D, 2)) + " is up and firing"
    If RN = 5 Then F$ = B$(D, B9%(D, 2)) + " comes up firing"
    If RN = 6 Then F$ = B$(D, B9%(D, 2)) + " lets it fly"
    If RN = 7 Then F$ = B$(D, B9%(D, 2)) + " throws down"
    If RN = 8 Then F$ = B$(D, B9%(D, 2)) + " gets it off"
    If RN = 9 Then F$ = B$(D, B9%(D, 2)) + " fires a missile"
    If RN = 10 Then F$ = B$(D, B9%(D, 2)) + " throws quickly"
    If RN = 11 Then F$ = B$(D, B9%(D, 2)) + " throws"
    Call PBP(F$)
    4837 If B0%(TGTBASE% - 1, 2) = 18 And I9 > Q0 Or B0%(TGTBASE% - 1, 2) > 18 And I9 > Q0 - 200 Then 4830
    4829 RN = Int(Rnd(1) * 100) + 1
    If RN <= 20 Then F$ = "He beats the throw!"
    If RN > 20 And RN <= 40 Then F$ = "He's in there!"
    If RN > 40 And RN <= 50 Then F$ = "Throw is off-line..."
    If RN > 50 And RN <= 60 Then F$ = "Throw is high..."
    If RN > 60 And RN <= 80 Then F$ = "He slides under the tag!"
    If RN > 80 And TGTBASE% <> 4 Then F$ = U$(TGTBASE% + 1) + " with the call"
    If RN > 80 And TGTBASE% = 4 Then F$ = U$(1) + " with the call"
    Call PBP(F$): Call PBP("safe @ " + H$(TGTBASE%) + "!")
    B0%(TGTBASE% - 1, 2) = 0
    INFPOS% = 0
    Call PITSWAP(TGTBASE%): Call ADVANCEMENT(P, D, S2%, F%, I3)
    I5 = B9%(D, 2): I6 = 20: Call INCBATDF(D, I5, I6)
    If SH% <> 1 Then 4892
    SH% = 0: GoTo 5051
    4830 RN = Int(Rnd(1) * 100) + 1
    If RN <= 20 Then F$ = B$(D, B9%(D, 2)) + " nails " + Q$(TGTBASE% - 1)
    If RN > 20 And RN <= 40 Then F$ = Q$(TGTBASE% - 1) + " is pegged at " + H$(TGTBASE%)
    If RN > 40 And RN <= 60 Then F$ = "Great throw by " + B$(D, B9%(D, 2)) + "!!"
    If RN > 60 And RN <= 80 Then F$ = Q$(TGTBASE% - 1) + " slides into the tag"
    If RN > 80 And TGTBASE% <> 4 Then F$ = U$(TGTBASE% + 1) + " with the call"
    If RN > 80 And TGTBASE% = 4 Then F$ = U$(1) + " with the call"
    Call PBP(F$)
    A5%(TGTBASE% - 1) = 11: INFPOS% = 0
    I5 = B3%(P, B0%(TGTBASE% - 1, 0)): I6 = 10: Call INCBATOFF(P, I5, I6)
    I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: W% = 2
    Call CREDITASSIST(W%, D): Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    I5 = B9%(D, 2): I6 = 21: Call INCBATDF(D, I5, I6)
    If O%(0) >= 3 Then B1!(P) = B1!(P) - 1: Call STRIKEOUTCS(SO): GoTo 3900
    4831 If O%(0) >= 3 Or P2 = 2 And U% = 3 Then 3900
    If SH% = 1 Then SH% = 0: GoTo 5051
    If AA < 20 Or S4% <> 5 Or B7%(P, B0%(3, 0)) = 1 Then 5051
    Call BLANKLINES
    If U6 = 2 Or U6 = 1 And P = U9 Then I$ = "N": GoTo 4835
    Call PBP("TRY TO STEAL HOME (YN)")
    4834 I$=GetKeyPress$
    4835 If UCase$(I$) <> "Y" Then 5051
    SH% = 1: GoTo 4890
    4838 CHANCE% = 100: Q0 = Q0 - 200: W% = 2: Q$(4) = B$(D, B9%(D, 2))
    TGBASE% = B3%(P, B0%(TGTBASE% - 1, 0))
    B0%(TGTBASE% - 1, 2) = 18
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then I5 = TGBASE%: I6 = 9: Call INCBATOFF(P, I5, I6): B1!(P) = B1!(P) - 1: Call STRIKEOUTCS(SO): GoTo 3900
    GoTo 4827




    4890 TGTBASE% = 4: A5%(1) = 0: A5%(2) = 0: A5%(3) = 1
    I2 = B3%(P, B0%(TGTBASE% - 1, 0)): CHANCE% = 100
    Q0 = 700
    GoTo 4826: Rem *** STEAL HOME ***
    4892 If S4% <> 5 Or S4% = 5 And PJ = 7 Then 4831: Rem*** PICK OFF IF NOT STEALING HOME ***
    PJ = 0: TGTBASE% = 4: SS% = Int(Rnd(1) * 20) + 1: If SS% = 6 Then 4820
    GoTo 4831


    4900 Screen 0
    P9 = D: B1!(P) = B1!(P) + 1: U% = 0: If B1!(P) = 10 Then B1!(P) = 1
    Cls: Color 15: Print "DOES "; B$(D, B3%(D, B9%(D, 0))); " STAY IN THE GAME (YN)"
    4902 I$=GetKeyPress$
    If UCase$(I$) = "N" Then I5 = 77: GoTo 5504
    I5 = 66
    Locate 5, 1
    For I = 1 To 9: Color 3, 0
        If BP(P9) = I Then Color 9
        Locate I + 4, 5: Print Using "# "; I;
        If B7%(P9, I) = 1 And I5 = 66 Then Print B$(P9, B3%(P9, I));
        If B7%(P9, I) = 1 And I5 <> 66 Then Print P$(P9, B3%(P9, I));
        If B7%(P9, I) <> 1 Then Print B$(P9, B3%(P9, I));
        Print " "; C$(B7%(P9, I));
        Color 3
    Next I
    I5 = 0
    Color 15: Locate 3, 1: Print "WHAT LINEUP SLOT WILL THE PITCHER BAT IN (1-9)"
    4905 I$=GetKeyPress$
    I2 = Val(I$)
    If I2 < 1 Or I2 > 9 Then 4905
    If B7%(D, I2) = 10 Then B7%(D, I2) = 1
    INFPOS% = 0
    B7%(D, B9%(D, 0)) = B7%(D, I2): B9%(D, B7%(D, B9%(D, 0))) = B3%(D, B9%(D, 0)): B7%(D, I2) = 1
    I7% = D: I8% = B9%(D, 0): I9% = B3%(D, B9%(D, 0))
    Call REPLACEMENTS(I7%, I8%, I9%)
    I5 = 66: GoTo 5504
    4950 I1 = IJ0%
    I2 = B%(D, IJ0%, 21) - 40
    4952 B%(D, I1, 21) = 1
    B3%(D, I2) = I1
    B9%(D, B7%(D, I2)) = I1

    D0%(D) = D0%(D) + 1: X0%(D, 0, D0%(D)) = B3%(D, I2)
    X0%(D, 1, D0%(D)) = B7%(D, I2): X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)
    W5 = D: W6 = D: VV(D) = I2: VV1(D) = I2
    For xx = 22 To 25
        If B7%(D, I2) = B%(D, I1, xx) Then B%(D, I1, 15) = B%(D, I1, 33 + (xx - 22) * 2): B%(D, I1, 17) = B%(D, I1, 34 + (xx - 22) * 2): B%(D, I1, 19) = B%(D, I1, xx + 4): GoTo 4955
    Next
    4955 Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    Call FLDDISP(BC%, P, U%, D): Call MAINSCREEN(D, P, BC%, U%, MO)
    'CALL BLANKLINES
    'LOCATE 15, 1:COLOR 15,0
    'PRINT M$(D); " is sending in a defensive sub"
    'CALL DELAY: CALL DELAY
    VV(D) = 1: VV1(D) = 9
    Return
    4956 For I2 = 1 To 9
        If B7%(D, I2) = MG%(D, 576 + IJ0%) Then 4957
    Next
    4957 Return
    4998 S8%(I, 0) = S8%(I, 0) + 6: S8%(I, 1) = 6
    Return
    5000 F% = 0: PQ = 0
    For I = 0 To 1: O%(I) = 0
    Next I
    L0%(P) = L0%(P) + MO
    MO = 0
    H0% = 0
    For I = 1 To 3
        For I1 = 0 To 1
            B0%(I, I1) = 0: B1%(I, I1) = 0
        Next I1
        B0%(I, 2) = 0
    Next I
    S4% = 0
    If P = 1 Then INNING% = INNING% + 1: CZ%(1) = 0: CZ%(0) = 0
    P = 1 - P: D = 1 - P
    If INNING% = 7 And L%(1, 13) = 8 And P = 1 And Val(Left$(A$(1), 2)) > 75 Then Call BLANKLINES: Call PBP("ALL RIGHT..."): Call PBP("LEMME HEAR YA..."): Call PBP("A ONE, A TWO...")

    GoTo 5004

    If CGERA% > 0 And INNING% = 8 And P = 0 And S6%(1, 0) > S6%(0, 0) And P%(1, P1%(1), 1) > 0 And P6%(1) = 1 And Int(P%(1, P1%(1), 5) / P%(1, P1%(1), 1) * 100 + .5) <= 75 And Int(Rnd(1) * 100) + 1 <= Int(P%(1, P1%(1), 16) / P%(1, P1%(1), 1) * 100 + .5) Then I = 1: GoSub 4998
    If CGERA% > 0 And INNING% = 8 And P = 0 And S6%(0, 0) > S6%(1, 0) And P%(0, P1%(0), 1) > 0 And P6%(0) = 1 And Int(P%(0, P1%(0), 5) / P%(0, P1%(0), 1) * 100 + .5) <= 75 And Int(Rnd(1) * 100) + 1 <= Int(P%(0, P1%(0), 16) / P%(0, P1%(0), 1) * 100 + .5) Then I = 0: GoSub 4998

    If P%(1, P1%(1), 1) = 0 Or P6%(1) <> 1 Then 5002
    Call RANDNUM(RN)
    If CGERA% > 0 And INNING% = 8 And P = 0 And S6%(1, 0) >= S6%(0, 0) And Int(P%(1, P1%(1), 5) / P%(1, P1%(1), 1) * 100 + .5) > 75 And Int(P%(1, P1%(1), 5) / P%(1, P1%(1), 1) * 100 + .5) <= 100 And RN <= Int(P%(1, P1%(1), 16) / P%(1, P1%(1), 1) * 100 + .5) Then I = 1: GoSub 4998
    5002 If P%(0, P1%(0), 1) = 0 Or P6%(0) <> 1 Then 5003
    Call RANDNUM(RN)
    If CGERA% > 0 And INNING% = 8 And P = 0 And S6%(0, 0) >= S6%(1, 0) And Int(P%(0, P1%(0), 5) / P%(0, P1%(0), 1) * 100 + .5) > 75 And Int(P%(0, P1%(0), 5) / P%(0, P1%(0), 1) * 100 + .5) <= 100 And RN <= Int(P%(0, P1%(0), 16) / P%(0, P1%(0), 1) * 100 + .5) Then I = 0: GoSub 4998

    5003 If CGERA% > 0 And INNING% = 8 And P = 0 And (S6%(1, 0) >= S6%(0, 0) Or S6%(0, 0) - S6%(1, 0) <= 2) And P%(1, P1%(1), 1) > 0 And P6%(1) = 1 And Int(P%(1, P1%(1), 16) / P%(1, P1%(1), 1) * 100 + .5) > 100 Then I = 1: GoSub 4998
    If CGERA% > 0 And INNING% = 8 And P = 0 And (S6%(0, 0) >= S6%(1, 0) Or S6%(1, 0) - S6%(0, 0) <= 2) And P%(0, P1%(0), 1) > 0 And P6%(0) = 1 And Int(P%(0, P1%(0), 16) / P%(0, P1%(0), 1) * 100 + .5) > 100 Then I = 0: GoSub 4998


    5004 If INNING% > 9 And D = 1 And S6%(1, 0) <> S6%(0, 0) Or INNING% = 28 Or INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) > 0 Then 8500
    If RD = 1 And INNING% >= RE And RE >= 6 And (S6%(1, 0) > S6%(0, 0) And P = 1 Or S6%(0, 0) > S6%(1, 0) And P = 0) Then Screen 0: Print "THIS GAME HAS BEEN CALLED ON ACCOUNT OF RAIN!!!": Print "ALL RECORDS ARE OFFICIAL!!": I$ = GetKeyPress$: GoTo 8500
    If RD = 1 And P = 0 And INNING% = 4 Then Call PBP("THE RAIN IS FALLING A LITTLE MORE STEADILY"): Call PBP("THIS GAME MAY NOT GO 9!"): I$ = GetKeyPress$
    Call FLDDISP(BC%, P, U%, D)
    For IJ0% = 0 To 22
        If B%(D, IJ0%, 21) > 39 And B%(D, IJ0%, 21) <= 49 Then GoSub 4950
    Next
    A1 = S6%(D, 0) - S6%(P, 0)
    If MG%(D, 560) <> 999 Or INNING% < 8 Or U6 = 0 Or U6 = 1 And P = U9 Then 5025
    For IJ0% = 1 To 4
        If MG%(D, 560 + IJ0%) = -1 Or B%(D, MG%(D, 564 + IJ0%), 21) <> 0 Then 5010
        Call RANDNUM(RN)
        If B9%(D, MG%(D, 576 + IJ0%)) = MG%(D, 560 + IJ0%) And RN <= MG%(D, 568 + IJ0%) And A1 >= MG%(D, 572 + IJ0%) Then I1 = MG%(D, 564 + IJ0%): GoSub 4956: GoSub 4952
    5010 Next
    5025 If B9%(D, 1) = 99 And U6 = 0 Or B9%(D, 1) = 99 And U6 = 1 And U9 = 1 - D Then 4900
    5050 SH = 0: F% = 0: PQ = 0: SO = 0
    B1!(P) = B1!(P) + 1: INFPOS% = 0: U% = 0
    If B1!(P) = 10 Then B1!(P) = 1
    5051 Call WHOSON1ST(S4%)
    Call PLAYERNAME(P)
    Call MAINSCREEN(D, P, BC%, U%, MO)

    P2 = 0
    D2 = 0
    If INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) > 0 Then 8500
    5052 _PutImage (20, 48)-Step(44, 128), screen_handle, park_graphic
    'CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
    'CALL ClearVE
    I2 = INNING%: I = P
    If S%(P, INNING%) > 0 Then Call INNDISP(I2, I)

    'Need to strip it to 2 digits
    FORMATTED$ = Str$(Int(S6%(I, 0) * 100) / 100)

    'FORMATTED$ = Print Using "##"; STR$(S6%(I,0))
    _PrintString (1 - I + 29, 53), FORMATTED$, park_graphic
    '--GPRINT0VE I+29,53,FORMATTED$,15
     
    'Need to strip it to 2 digits
    FORMATTED$ = Str$(Int(S6%(D, 2) * 100) / 100)

    'FORMATTED$ = Print Using "##"; STR$(S6%(D,2))
    _PrintString (1 - I + 29, 59), FORMATTED$, park_graphic
    '--GPRINT0VE 1-I+29,59,FORMATTED$,15

    Z2% = B0%(2, 0): F% = 0: PQ = 0: GoTo 5080
    5054 D2 = Val(I$): If D2 < 1 Or D2 > 9 Then 5052
    5055 If D2 = 4 Then INFPOS% = 1: F$ = "INF:CORNERS IN"
    If D2 = 3 Then INFPOS% = 2: F$ = "INF:INFIELD IN"
    If D2 = 5 Then INFPOS% = 0: F$ = "INF:NORMAL    "
    If D2 = 3 Or D2 = 4 Or D2 = 5 Then
        _PutImage (51, 416)-Step(14, 16), screen_handle, park_graphic
        'CALL GMove2VE (52,416,14,16,VA%(1),-1)

        _PrintString (27, 52), F$, park_graphic
        '--GPRINT1VE 27,52,F$,0
        GoTo 5052
    End If
    5058 If D2 = 2 Then Call BLANKLINES: GoTo 3240
    If D2 = 6 Then P9 = D: W5 = -1: W6 = -1: GoTo 5500
    5060 GoTo 5085
    5061 I$=GetKeyPress$

    _PutImage (1, 304)-Step(14, 176), screen_handle, park_graphic
    'CALL GMove2VE (1,304,14,176,VA%(16),-1)
    If UCase$(I$) = "Q" Then Screen 0: End
    If UCase$(I$) = "D" Then Call VIEWTEAM: GoTo 5580
    If UCase$(I$) = "W" Then Call WEATHER_G: GoTo 5580
    If UCase$(I$) = "S" Then Call POPUPSTATS: GoTo 5580
    If UCase$(I$) = "L" Then Call POPUP: GoTo 5580
    If UCase$(I$) = "O" Then Call OPTIONS: GoTo 5580
    P2 = Val(I$)
    If P2 < 1 Or P2 > 5 Then 5060
    5064 If P2 = 5 Then P9 = P: W5 = -1: W6 = -1: GoTo 5500
    If P2 = 3 Then 4800
    If P2 = 2 And O%(0) = 2 Or P2 = 2 And S4% <> 1 And S4% <> 4 And S4% <> 5 Then 5051
    5067 If P2 <> 2 Then 5100
    If S4% = 4 Then I = 3 Else I = 2
    I3 = Int(Rnd(1) * 100) + 1
    If I3 >= 6 Then 5068
    Call PICKEDOFF(D, I): PO = 1: A5%(I - 1) = 10: O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1: W% = 1: Call CREDITASSIST(W%, D): Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D): I5 = P1%(D): I6 = 0: Call ADDPIT(D, I5, I6)
    If O%(0) >= 3 Then B1!(P) = B1!(P) - 1: PO = 0: GoTo 3900
    5068 If PO = 1 Then PO = 0: GoTo 5051
    Call BLANKLINES: I3 = Int(Rnd(1) * 1000) + 1: I9 = Int(Rnd(1) * 1000) + 1
    If I3 > 350 Then Call PBP("Runner(s) going")
    If I3 <= 350 Then Call PBP("Count not favorable...hit away"): P2 = 1
    Call DELAY
    If I3 <= 350 Then 3050
    If I9 <= (B4%(P, 4, B1!(P)) * 2 * .33) Then U% = U% + 1: Call PBP(Q$(0) + " swings and misses"): Call DELAY: If U% = 3 Then GoTo 3601 Else GoTo 4800
    If I9 <= B4%(P, 4, B1!(P)) * 2 Then Call PBP(Q$(0) + " fouls off the pitch...hit away"): Call DELAY: P2 = 1
    GoTo 3050
    5080 If U6 = 2 Or U6 = 1 And P <> U9 Then 8700
    If S4% = 1 Or S4% = 5 Then H0% = 1: Call HOLDSTATUS(H0%)
    5081 F$ = Left$(A$(D), 3) + NN$(D)
    Call SPA(F$, 8)
    _PrintString (20, 1), F$, park_graphic
    '--GPRINT1VE 20,1,F$,0
    F1$(1) = "1-PITCH       "
    F1$(2) = "2-INT WALK    "
    F1$(3) = "3-INF IN      "
    F1$(4) = "4-CORNERS IN  "
    F1$(5) = "5-INF NORMAL  "
    F1$(6) = "6-PERSONNEL   "
    F1$(7) = "8-PITCH AROUND"
    F1$(8) = "G-GUARD LINES "
    F1$(9) = "H-HOLD RUNNER "
    F1$(10) = "S/D/B/P STATS "
    For I = 21 To 30
        _PrintString (I, 1), F1$(I - 20), park_graphic
        '--GPRINT1VE I,1,F1$(I-20),0
    Next

    GoTo 5090
    5085 If U6 = 2 Or U6 = 1 And P = U9 Then 8900
    F$ = Left$(A$(P), 3) + NN$(P)
    Call SPA(F$, 8)
    _PrintString (20, 1), F$, park_graphic
    '--GPRINT1VE 20,1,F$,0
    F1$(1) = "1-HIT AWAY    "
    F1$(2) = "2-HIT AND RUN "
    F1$(3) = "3-STEAL       "
    F1$(4) = "4-BUNT        "
    F1$(5) = "5-PERSONNEL   "
    F1$(6) = "O-GAME OPTIONS"
    F1$(7) = "D/S/P/B STATS "
    F1$(8) = "W-WEATHER COND"
    F1$(9) = "L-SHOW LINEUPS"
    F1$(10) = "Q-QUIT GAME   "
    For I = 21 To 30
        _PrintString (I, 1), F1$(I - 20), park_graphic
        '--GPRINT1VE I,1,F1$(I-20),0
    Next
    GoTo 5061
    5090 I$=GetKeyPress$

    _PutImage (17, 128)-Step(46, 80), screen_handle, park_graphic
    'CALL GMove2VE (1,304,14,176,VA%(16),-1)
    If UCase$(I$) = "S" Then Call POPUPSTATS: GoTo 5580
    If UCase$(I$) = "L" Then Call POPUP: GoTo 5580
    If UCase$(I$) = "W" Then Call WEATHER_G: GoTo 5580
    If UCase$(I$) = "G" Then
        INFPOS% = 3
        F$ = "INF:GUARD LINE"
        _PutImage (52, 416)-Step(14, 16), screen_handle, park_graphic
        'CALL GMove2VE (52,416,14,16,VA%(1),-1)
        _PrintString (27, 52), F$, park_graphic
        '--GPRINT1VE 27,52,F$,0: GOTO 5052
    End If
    If UCase$(I$) = "D" Then Call VIEWTEAM: GoTo 5580
    If UCase$(I$) = "O" Then Call OPTIONS: GoTo 5580
    If UCase$(I$) = "Q" Then Screen 0: End
    If UCase$(I$) = "H" Then H0% = 1 - H0%: Call HOLDSTATUS(H0%): GoTo 5081
    If Val(I$) = 7 Then P2 = 1: D2 = 1: GoTo 3050

    GoTo 5054
    5100 If D2 <> 8 Then 3050
    Call BLANKLINES
    Call RANDNUM(RN): If P2 = 4 Or RN <= 50 And P2 = 1 Then 3050
    GoTo 3240

    5500 Screen 0
    TG = 0: PG = 0: RG = 0
    Cls
    Color 12: Print Tab(33); "PERSONNEL MENU": Print
    Color 14: Print "(0) ";: Color 15: Print "RESUME PLAY": Print
    Color 14: Print "(1) ";: Color 15: Print "RELIEF PITCHER": Print
    Color 14: Print "(2) ";: Color 15: Print "CHANGE BATTER/FIELDER"
    5501 I$=GetKeyPress$
    I = Val(I$): I5 = 0: If I > 2 Then 5501
    On I + 1 GOTO 5580, 5504, 5540
    5504 If P2 = 5 Then Call TIRING(P9): GoTo 5501
    5505 Call PITDISP(P9)
    Locate 23, 1: Print "SELECT RELIEVER (Z = NO CHANGE) ";
    If B9%(P9, 1) <> 99 Then Call TIRING(P9)
    Print
    5507 I$=GetKeyPress$
    If UCase$(I$) = "Z" Then 5500
    Call LET2NUM(I$)
    I1 = Val(I$): If I1 > 21 Then 5507
    If P%(P9, I1, 15) > 0 Then 5507
    Locate 23, 1: Print String$(78, 32): Locate 23, 1: Print P$(P9, I1); " "; B1$(P%(P9, I1, 0) + 2); " ";
    Print "ARE YOU SURE (YN)";
    5515 I$=GetKeyPress$
    If UCase$(I$) = "N" Then 5505
    PPH = 0: PR% = 0
    If S1%(P9) <> -1 And P6%(P9) > 1 Then I5 = P1%(P9): I6 = 22: Call OFFPIT(P9, I5, I6)
    P1%(P9) = I1: P6%(P9) = P6%(P9) + 1: P%(P9, I1, 15) = P6%(P9): Call PITSTAM(P9): Call CHECKSAVE(D, P, S4%)
    5518 P9 = 1 - P9: W5 = P9: W6 = W5: VV(P9) = 1: VV1(P9) = 9
    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    P9 = 1 - P9: If DH% = 1 Or I5 = 66 Or I5 = 77 Then 5538
    Cls: Locate , 14: Color 14: Print "DO YOU WISH TO MAKE A FLIP FLOP OR DOUBLE SWITCH (YN)";
    Color 15
    5520 I$=GetKeyPress$
    If UCase$(I$) = "N" Then 5538
    Cls: Call BATDISP(PG, P9)
    5523 Call CHANGELINEUP(P9, P): Locate 12, 30: Print "LINEUP SLOT FOR PITCHER TO BAT IN (1-9)"
    5524 I$=GetKeyPress$
    If Asc(I$) = 32 And PG = 0 Then PG = 1: Cls: Call BATDISP(PG, P9): GoTo 5523
    If Asc(I$) = 32 And PG = 1 Then PG = 0: Cls: Call BATDISP(PG, P9): GoTo 5523
    If Asc(I$) = 9 And TG = 0 Then Call DISPFLDG(PG, P9, TG): GoTo 5523
    If Asc(I$) = 9 And TG = 1 Then TG = 0: Call BATDISP(PG, P9): GoTo 5523
    V2 = Val(I$): If V2 < 1 Or V2 > 9 Then 5524
    5526 Locate 13, 30: Print "WHO WILL REPLACE "; B$(D, B3%(D, V2)); " @ "; C$(B7%(D, V2));
    5527 I$=GetKeyPress$
    If Asc(I$) = 32 And PG = 0 Then PG = 1: Call BATDISP(PG, P9): GoTo 5526
    If Asc(I$) = 32 And PG = 1 Then PG = 0: Call BATDISP(PG, P9): GoTo 5526
    If Asc(I$) = 9 And TG = 0 Then Call DISPFLDG(PG, P9, TG): GoTo 5526
    If Asc(I$) = 9 And TG = 1 Then TG = 0: Call BATDISP(PG, P9): GoTo 5526
    Call LET2NUM(I$): V3 = Val(I$): If V3 > 22 Then 5527
    Locate 16, 29: Print B$(D, V3)
    Locate 17, 29: Print "ARE YOU SURE (YN)";
    5531 I$=GetKeyPress$
    If UCase$(I$) = "N" Then 5518
    D0%(D) = D0%(D) + 1: X0%(D, 0, D0%(D)) = V3: X0%(D, 1, D0%(D)) = B7%(D, V2)
    For V0 = 1 To 9
        If B7%(D, V0) = 1 Then X0%(D, 2, D0%(D)) = (V0 * 10) + (D0%(D) - 9)
    Next V0
    For V0 = 1 To 9
        If B7%(D, V0) = 1 Then B7%(D, V0) = B7%(D, V2): B3%(D, V0) = V3: B9%(D, B7%(D, V0)) = V3: P9 = D: W5 = D: W6 = D: VV(D) = V0: VV1(D) = V0: Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
        VV(D) = 1: VV1(D) = 9
    Next V0
    B%(D, V3, 21) = 1
    I7% = D: I8% = V2: I9% = V3
    Call REPLACEMENTS(I7%, I8%, I9%)
    I5 = 0
    For V0 = 1 To 9
        If V0 = V2 Then B7%(D, V2) = 1: B3%(D, V2) = P1%(D): B9%(D, 1) = P1%(D): VV(D) = V0: VV1(D) = V0: Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    Next V0
    5537 VV(D) = 1: VV1(D) = 9
    GoTo 5500
    5538 I5 = 0
    For V0 = 1 To 9
        If B7%(D, V0) = 1 Or B7%(D, V0) = 10 Then
            B3%(D, V0) = P1%(D)
            B9%(D, 1) = P1%(D)
            B7%(D, V0) = 1
            P9 = D: W5 = D: W6 = D
            VV(D) = V0: VV1(D) = V0
            Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
            GoTo 5537
        End If
    Next V0
    5540 Cls: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P)
    5546 Locate 14, 29: Color 12: Print "SELECT:"
    Locate , 30: Color 14: Print " (1) ";: Color 15: Print "SELECT A PINCH-HITTER"
    Locate , 30: Color 14: Print " (2) ";: Color 15: Print "SWAP POSITIONS BETWEEN PLAYERS"
    Locate , 30: Color 14: Print " (3) ";: Color 15: Print "SUBSTITUTE PLAYER IN LINEUP"
    Locate , 30: Color 14: Print " (Z) ";: Color 15: Print "RESUME PLAY"
    Locate , 30: Color 14: Print "<SPACE> ";: Color 15: Print "TOGGLE ROSTER";: Color 14: Print " <TAB> ";: Color 15: Print "TOGGLE FIELDING/BATTING"
    5547 I$=GetKeyPress$
    If Asc(I$) = 32 And PG = 0 Then PG = 1: Call BATDISP(PG, P9): GoTo 5547
    If Asc(I$) = 32 And PG = 1 Then PG = 0: Call BATDISP(PG, P9): GoTo 5547
    GoTo 5590


    5550 Locate 20, 30: Print "LINEUP SLOT TO REPLACE (1-9) (Z)=NO CHANGE"
    5552 I$=GetKeyPress$
    If Asc(I$) = 32 And PG = 0 Then PG = 1: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P): GoTo 5552
    If Asc(I$) = 32 And PG = 1 Then PG = 0: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P): GoTo 5552
    If Asc(I$) = 9 And TG = 0 Then Call DISPFLDG(PG, P9, TG): Call CHANGELINEUP(P9, P): GoTo 5552
    If Asc(I$) = 9 And TG = 1 Then TG = 0: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P): GoTo 5552
    If UCase$(I$) = "Z" Then 5540
    I2 = Val(I$): If I2 < 1 Or I2 > 9 Or P9 = D And B7%(P9, I2) = 1 Then 5552
    Locate , 30
    If B7%(P9, I2) = 1 Then Print P$(P9, P1%(P9));
    If B7%(P9, I2) <> 1 Then Print B$(P9, B3%(P9, I2));
    5555 Locate 22, 30: Print "# OF SUB"
    5556 I$=GetKeyPress$
    If Asc(I$) = 32 And PG = 0 Then PG = 1: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P): GoTo 5556
    If Asc(I$) = 32 And PG = 1 Then PG = 0: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P): GoTo 5556
    If Asc(I$) = 9 And TG = 0 Then Call DISPFLDG(PG, P9, TG): Call CHANGELINEUP(P9, P): GoTo 5556
    If Asc(I$) = 9 And TG = 1 Then TG = 0: Call BATDISP(PG, P9): Call CHANGELINEUP(P9, P): GoTo 5556
    Call LET2NUM(I$): I1 = Val(I$): If I1 > 22 Then 5555
    If B%(P9, I1, 21) > 0 Then 5555
    Locate 23, 30: Print B$(P9, I1); "... ARE YOU SURE (YN)"
    5560 I$=GetKeyPress$
    If UCase$(I$) = "N" Then 5540
    B%(P9, I1, 21) = 1: B3%(P9, I2) = I1: B9%(P9, B7%(P9, I2)) = I1
    If B7%(P9, I2) = 1 Or B7%(P9, I2) = 10 Then B9%(P9, 1) = 99: B9%(P9, 0) = I2: B7%(P9, I2) = 10
    D0%(P9) = D0%(P9) + 1: X0%(P9, 0, D0%(P9)) = B3%(P9, I2)
    X0%(P9, 1, D0%(P9)) = B7%(P9, I2): X0%(P9, 2, D0%(P9)) = (I2 * 10) + (D0%(P9) - 9)
    W5 = P9: W6 = P9: VV(P9) = I2: VV1(P9) = I2
    I7% = P9: I8% = I2: I9% = I1
    Call REPLACEMENTS(I7%, I8%, I9%)
    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    VV(P9) = 1: VV1(P9) = 9
    GoTo 5540
    5567 Locate 20, 29: Print "B.O. #1 (1-9)"
    5568 I$=GetKeyPress$
    I1 = Val(I$): If I1 < 1 Or I1 > 9 Then 5568
    If B7%(P9, I1) = 1 Then 5568
    Locate 21, 29: Print "B.O. #2 (1-9)"
    5572 I$=GetKeyPress$
    I2 = Val(I$): If I2 < 1 Or I2 > 9 Then 5572
    If B7%(P9, I2) = 1 Then 5572
    I3 = B7%(P9, I2): B7%(P9, I2) = B7%(P9, I1): B7%(P9, I1) = I3
    B9%(P9, B7%(P9, I1)) = B3%(P9, I1)
    B9%(P9, B7%(P9, I2)) = B3%(P9, I2)
    I7% = P9: I8% = I2: I9% = B3%(P9, I2)
    Call REPLACEMENTS(I7%, I8%, I9%)
    I7% = P9: I8% = I1: I9% = B3%(P9, I1)
    Call REPLACEMENTS(I7%, I8%, I9%)
    GoTo 5540
    5580 For I = 1 To 9
        If B7%(P9, I) = 1 Then 154
        B%(P9, B3%(P9, I), 21) = 1
    154 Next I
    Call LOADSCREEN
    Call FLDDISP(BC%, P, U%, D)
    GoTo 5051
    5590 If Asc(I$) = 9 And TG = 0 Then Call DISPFLDG(PG, P9, TG): GoTo 5547
    If Asc(I$) = 9 And TG = 1 Then TG = 0: Call BATDISP(PG, P9): GoTo 5547
    If UCase$(I$) = "Z" Then 5500
    I1 = Val(I$): If I1 = 2 Then 5567
    If I1 = 3 Then 5550
    If D2 = 6 Then 5550
    I2 = B1!(P): GoTo 5555
    For I5 = 20 To 23: Locate I5, 28
        Print "                                                    "
    Next I5
    Return
    8500 If S6%(0, 0) > S6%(1, 0) Then Call PBP(A$(0) + " defeats " + A$(1) + "!") Else Call PBP(A$(1) + " defeats " + A$(0) + "!")
    If AP% = 1 And U6 = 2 Then 8607
    G4 = 1
    Call PBP("PRESS ANY KEY TO CONTINUE")
    8606 I$=GetKeyPress$
    8607 Screen 0: Chain "POSTGAME.exe"
    End
    8700 H0% = 0: Call HOLDSTATUS(H0%)
    A1 = S6%(D, 0) - S6%(P, 0)
    If A1 > 1 Or S4% = 3 Or S4% >= 6 Then 8800
    If S4% <> 1 And S4% <> 5 Then I2 = 2: GoTo 8800
    I2 = 1
    J = B0%(I2, 0): If J = 0 Then 8800
    If B7%(P, J) = 1 Then 8800
    If B7%(P, J) <> 1 And B%(P, B3%(P, J), 13) >= 10 Then H0% = 1: Call HOLDSTATUS(H0%)
    8800 If SU%(D, 0) <> -1 And MG%(D, 589) = 0 And INNING% = MG%(D, 588) Then Call PITSTRAT
    If SU%(D, 1) <> -1 And MG%(D, 589) = 0 And INNING% > MG%(D, 588) Then Call PITSTRAT
    If SU%(D, 1) <> -1 And MG%(D, 589) <> 0 And INNING% >= MG%(D, 588) Then Call PITSTRAT


    If PZ = 1 Then PZ = 0: GoTo 5055
    A1 = S6%(D, 0) - S6%(P, 0)
    Call RANDNUM(RN)
    If INNING% < 9 Or CZ%(D) = -1 Or MG%(D, 581) = 999 Then 8725


    CZ%(D) = -1
    If A1 > 3 Or A1 < 0 Then 8725
    If P6%(D) <> 1 Then 8730
    If S6%(P, 0) = 0 And P%(D, P1%(D), 5) > 0 And RN <= Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 5) * 100 + .5) + 10 Then 8725
    If P%(D, P1%(D), 5) > 0 And RN <= Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 5) * 100 + .5) Then 8725
    If CGERA% > 0 And INNING% > 7 And S6%(D, 0) > S6%(P, 0) And P%(D, P1%(D), 1) > 0 And P6%(D) = 1 And Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5) <= 100 And Int(Rnd(1) * 100) + 1 <= Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5) Then 8725
    If CGERA% > 0 And INNING% > 7 And (S6%(D, 0) >= S6%(P, 0) Or S6%(P, 0) - S6%(D, 0) <= 2) And P%(D, P1%(D), 1) > 0 And P6%(D) = 1 And Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5) >= 100 Then 8725
    If CGERA% >= 8 Then 8725
    GK%(D) = 1
    GoTo 8725
    8730 Call RANDNUM(RN)
    If RN <= 80 And P%(D, P1%(D), 3) < 15 Then GK%(D) = 1
    8725 If GK%(D) = 1 Then GK%(D) = 0: Call RELIEVERS: GoSub 3955: GoTo 5055
    If B7%(P, B1!(P)) = 1 Then BR = 0
    If B7%(P, B1!(P)) <> 1 Then BR = B%(P, B3%(P, B1!(P)), 30)
    If P%(P, P1%(P), 11) <> 999 Then I7! = P%(I, P1%(I), 11): GoTo 8726
    If P%(P, P1%(P), 20) = 0 Then I7! = 0: GoTo 8726
    I7! = Int((P%(P, P1%(P), 22) / P%(P, P1%(P), 20) * 1000 + .5))
    8726 D2 = 1: If INFPOS% > 0 Then 8820

    If MG%(D, 690) <> 999 Then 8801
    ' PROFILE INFIELD IN WITH 3RD OCCUPIED
    If S4% < 3 Or S4% = 4 Then 8801
    Call RANDNUM(RN)
    If O%(0) = 0 And A1 >= MG%(D, 691) * -1 And A1 <= MG%(D, 692) * -1 And INNING% >= MG%(D, 693) And RN <= MG%(D, 694) Then INFPOS% = 2: D2 = 3: GoTo 5055
    If O%(0) = 0 And A1 >= MG%(D, 695) * -1 And A1 <= MG%(D, 696) * -1 And INNING% >= MG%(D, 697) And RN <= MG%(D, 698) Then INFPOS% = 2: D2 = 3: GoTo 5055
    If O%(0) = 1 And (S4% = 5 Or S4% = 7) And A1 >= MG%(D, 699) * -1 And A1 <= MG%(D, 700) * -1 And INNING% >= MG%(D, 701) And RN <= MG%(D, 702) Then INFPOS% = 2: D2 = 3: GoTo 5055
    If O%(0) = 1 And (S4% = 5 Or S4% = 7) And A1 >= MG%(D, 703) * -1 And A1 <= MG%(D, 704) * -1 And INNING% >= MG%(D, 705) And RN <= MG%(D, 706) Then INFPOS% = 2: D2 = 3: GoTo 5055
    8801 If MG%(D, 707) <> 999 Then 8802
    If S4% <> 0 Then 8802
    Call RANDNUM(RN)
    ' PROFILE CORNERS IN AGAINST A BUNT HIT
    If BR >= MG%(D, 708) And RN <= MG%(D, 709) Or BR >= MG%(D, 710) And RN <= MG%(D, 711) Or BR >= MG%(D, 712) And RN <= MG%(D, 713) Then 8811
    8802 If MG%(D, 714) <> 999 Or B7%(P, B1!(P)) = 1 Then 8803
    If O%(0) <> 0 Then 8803
    If S4% <> 1 And S4% <> 4 Then 8803
    ' PROFILE CORNERS IN VS HITTER IN SAC BUNT SITUATION
    Call RANDNUM(RN)
    If A1 <= MG%(D, 715) And A1 >= MG%(D, 716) * -1 And S4% = 1 And BR >= MG%(D, 717) And RN <= MG%(D, 718) Then 8811
    If A1 <= MG%(D, 719) And A1 >= MG%(D, 720) * -1 And S4% = 1 And BR >= MG%(D, 721) And RN <= MG%(D, 722) Then 8811
    If A1 <= MG%(D, 723) And A1 >= MG%(D, 724) * -1 And S4% = 4 And BR >= MG%(D, 725) And RN <= MG%(D, 726) Then 8811
    If A1 <= MG%(D, 727) And A1 >= MG%(D, 728) * -1 And S4% = 4 And BR >= MG%(D, 729) And RN <= MG%(D, 730) Then 8811
    8803 If MG%(D, 731) <> 999 Or B7%(P, B1!(P)) <> 1 Or O%(0) = 2 Then 8804
    If S4% <> 1 And S4% <> 2 And S4% <> 4 Then 8804
    ' PROFILE CORNERS IN VS A PITCHER IN SAC BUNT SITUATION
    Call RANDNUM(RN)
    If I7! <= MG%(D, 732) And RN <= MG%(D, 733) And S4% = 1 And O%(0) <= 1 Then 8811
    If I7! <= MG%(D, 734) And RN <= MG%(D, 735) And S4% = 1 And O%(0) <= 1 Then 8811
    If I7! <= MG%(D, 736) And RN <= MG%(D, 737) And (S4% = 2 Or S4% = 4) And O%(0) = 0 Then 8811
    If I7! <= MG%(D, 738) And RN <= MG%(D, 739) And (S4% = 2 Or S4% = 4) And O%(0) = 0 Then 8811
    8804 If MG%(D, 740) <> 999 Then 8805
    If O%(0) = 2 Or S4% <> 3 And S4% < 5 Then 8805
    ' PROFILE CORNERS IN FOR A SQUEEZE
    Call RANDNUM(RN)
    If O%(0) = 0 And A1 <= MG%(D, 741) And A1 >= MG%(D, 742) And BR >= MG%(D, 743) And RN <= MG%(D, 744) Then 8811
    If O%(0) = 1 And A1 <= MG%(D, 745) And A1 >= MG%(D, 746) And BR >= MG%(D, 747) And RN <= MG%(D, 748) Then 8811
    8805 If MG%(D, 756) <> 999 Then 8806
    If S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7 Then 8806
    MOB% = 0
    For I = 1 To 3
        If B0%(I, 0) > 0 Then MOB% = MOB% + 1
    Next
    If A1 + MOB% + 1 >= 0 Then 8806: Rem BATTER NOT TYING OR WINNING RUN
    Call RANDNUM(RN)
    If B%(P, B3%(P, B1!(P)), 4) = 0 Then I7! = 0: I6! = 0: GoTo 8806
    I6! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
    If I6! <= MG%(D, 757) And INNING% >= MG%(D, 758) And RN <= MG%(D, 759) Then D2 = 8: GoTo 8850
    If I6! <= MG%(D, 760) And INNING% >= MG%(D, 761) And RN <= MG%(D, 762) Then D2 = 8: GoTo 8850
    If I6! <= MG%(D, 763) And INNING% >= MG%(D, 764) And RN <= MG%(D, 765) Then D2 = 8: GoTo 8850
    8806 A1 = S6%(D, 0) - S6%(P, 0)
    If A1 >= -1 And A1 <= 1 And INNING% >= 8 And (S4% = 3 Or S4% >= 5) And O%(0) < 2 Then 8809
    If A1 > 0 Or O%(0) > 0 Or S4% <= 2 Or S4% = 4 Or S4% = 5 And O%(0) = 1 Or INNING% <= 5 And A1 < -2 Or INNING% > 5 And A1 < -4 Then 8810
    8809 INFPOS% = 2: D2 = 3: GoTo 5055
    8810 If S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% >= 5 Or O%(0) = 2 Or B1!(P) >= 3 And B1!(P) <= 6 Or INNING% <= 6 And A1 >= 3 Or INNING% <= 6 And A1 <= -3 Then 8820
    If INNING% > 7 And A1 >= 1 Or INNING% > 7 And A1 < -1 Then 8820
    8811 INFPOS% = 1: D2 = 4: GoTo 5055
    8820 If A1 <= 0 Or A1 = 0 And S4% <= 1 Or A1 = 0 And S4% = 4 Or S4% = 7 Or B7%(P, B1!(P)) = 1 Then 8850
    If INNING% >= 9 And P = 1 And O%(0) < 2 And A1 = 0 And S4% >= 2 And S4% <= 3 Or INNING% >= 9 And P = 1 And O%(0) = 2 And A1 = 0 And S4% >= 5 And S4% <= 6 Then 8835
    I = B4%(P, 0, B1!(P)): I1 = B4%(P, 3, B1!(P)): If B1!(P) = 9 Then I2 = B4%(P, 0, 0): I3 = B4%(P, 3, 0)
    If B1!(P) < 9 Then I2 = B4%(P, 0, B1!(P) + 1): I3 = B4%(P, 3, B1!(P) + 1)
    If S4% <> 2 And S4% <> 6 Then 8850
    If I - I2 >= 50 And I1 - I3 >= 50 And A1 < 1 And INNING% > 8 Or I >= 280 And I1 >= 140 And INNING% > 8 And I1 > I3 And A1 < 1 Then 8835
    'IF I-I2>=50 AND I1-I3>=50 OR I>=280 AND I1>=140 AND I1>I3 THEN 8835
    GoTo 8850
    8835 D2 = 2: GoTo 5058
    8850 If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) Then Call RELIEVERS: GoSub 3955: GoTo 5055
    If B9%(D, 1) = 99 Then Call RELIEVERS
    GoSub 3955
    GoTo 5055
    8900 JM = B3%(P, B1!(P))
    If PPH = 1 Then 8905
    A1 = S6%(P, 0) - S6%(D, 0)
    If B7%(P, B1!(P)) <> 1 Then 8906
    For I = 0 To 21
        If P%(P, I, 15) = 0 Then 8903
    Next
    GoTo 8941
    8903 If MG%(P, 590) = 999 Then 8985
    If INNING% > 4 And INNING% < 7 And A1 < -2 And S6%(P, 0) > 0 Then 8930
    If INNING% > 4 And INNING% < 7 And A1 < -3 Then 8930
    If INNING% > 6 And A1 < 0 Or A1 = 0 And INNING% > 6 And INNING% < 9 And S4% > 1 Then 8930
    If A1 = 0 And INNING% >= 9 And S4% > 0 Or A1 = 0 And INNING% >= 9 And O%(0) = 0 Then 8930
    If P%(P, P1%(P), 6) / P%(P, P1%(P), 4) <= 2 And P8%(P, P1%(P), 0) >= 6 Then 8930
    8905 GoTo 8941
    8906 If MG%(P, 847) = 999 Then 8991
    If B%(P, JM, 4) = 0 Then 8909
    If B%(P, JM, 6) / B%(P, JM, 4) * 1000 > 233 Or B%(P, JM, 9) >= 15 Then 8908
    If INNING% > 6 And A1 < -2 Or A1 <= 0 And INNING% >= 8 Or A1 = 0 And INNING% >= 9 Then 8909
    8908 GoTo 8941
    8909 'GET A PINCH HITTER FOR A POSITION PLAYER
    If MG%(P, 540) = 999 Then 8993
    8935 For I = 0 To 22: If B%(P, I, 21) > 0 Or B$(P, I) = "XXX" Or B%(P, I, 4) = 0 Then 8917
        If B7%(P, B1!(P)) <> B%(P, I, 22) And B7%(P, B1!(P)) <> B%(P, I, 23) And B7%(P, B1!(P)) <> B%(P, I, 24) And B7%(P, B1!(P)) <> B%(P, I, 25) Then 8917
        If P%(D, P1%(D), 0) = 1 And B%(P, I, 0) > 0 And B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 35 Then 8916
        If P%(D, P1%(D), 0) = -1 And B%(P, I, 0) < 0 And B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 45 Then 8916
        If B%(P, I, 0) = 0 And B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 30 Then 8916
        GoTo 8917
        8916 PPH = 1: D0%(P) = D0%(P) + 1: X0%(P, 0, D0%(P)) = I: X0%(P, 1, D0%(P)) = B7%(P, B1!(P)): X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9): GoTo 8919
    8917 Next I
    GoTo 8925
    8919 B%(P, I, 21) = 1: B3%(P, B1!(P)) = I: B9%(P, B7%(P, B1!(P))) = I
    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10
    I7% = P: I8% = B1!(P): I9% = B3%(P, B1!(P))
    Call REPLACEMENTS(I7%, I8%, I9%)
    GoTo 8938
    8925 GoTo 8941
    8930 'GET A PINCH HITTER FOR THE PITCHER
    If MG%(P, 540) = 999 Then 8990
    8933 J% = 0: GP% = 0

    If CGERA% > 1 And INNING% < 4 Then 8941
    If CGERA% > 2 And INNING% < 7 Then 8941
    If CGERA% > 3 And INNING% < 9 Then 8941
    If CGERA% >= 8 Then 8941
    If CGERA% > 0 And INNING% > 7 And S6%(P, 0) > S6%(D, 0) And P%(P, P1%(P), 1) > 0 And P6%(P) = 1 And Int(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5) <= 100 And Int(Rnd(1) * 100) + 1 <= Int(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5) Then 8941
    If CGERA% > 0 And INNING% > 7 And (S6%(P, 0) >= S6%(D, 0) Or S6%(D, 0) - S6%(P, 0) <= 2) And P%(P, P1%(P), 1) > 0 And P6%(P) = 1 And Int(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5) >= 100 Then 8941

    For I = 0 To 22
        If B$(P, I) = "XXX" Or B%(P, I, 21) > 0 Or B%(P, I, 4) = 0 Then 8932
        If B%(P, I, 0) > 0 And P%(D, P1%(D), 0) = 1 Or B%(P, I, 0) < 0 And P%(D, P1%(D), 0) = -1 Then PH%(J%, 0) = Int(B%(P, I, 3) / 4 + .5): GoTo 8931
        PH%(J%, 0) = B%(P, I, 3)
        8931 PH%(J%, 1) = I: GP% = GP% + PH%(J%, 0): J% = J% + 1
    8932 Next
    If J% = 0 Then 8941
    RN = Int(Rnd(1) * GP%) + 1
    For I = 0 To J% - 1
        If RN <= PH%(I, 0) Then PH% = PH%(I, 1): GoTo 8934
        PH%(I + 1, 0) = PH%(I, 0) + PH%(I + 1, 0)
    Next: GoTo 8941
    8934 I = PH%: PPH = 1: D0%(P) = D0%(P) + 1
    X0%(P, 0, D0%(P)) = I
    X0%(P, 1, D0%(P)) = 10
    X0%(P, 2, D0%(P)) = 81 + D0%(P)
    B%(P, I, 21) = 1: B3%(P, B1!(P)) = I
    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10
    8938 P9 = P: W5 = P9: W6 = P9: VV(P9) = B1!(P): VV1(P9) = B1!(P)
    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
    Call FLDDISP(BC%, P, U%, D)
    Call MAINSCREEN(D, P, BC%, U%, MO)
    VV(P9) = 1: VV1(P9) = 9
    Q$(0) = B$(P, B3%(P, B1!(P)))
    If B7%(P, B1!(P)) = 1 Then Q$(0) = P$(P, P1%(D))
    Call BLANKLINES: Call PBP(M$(P) + " is sending up a pinch hitter"): PPH = 1: GoTo 5052

    8941 If PR% <> 1 Then Call PINCHRUNNER(MS%, A1, S4%, I2, P, PR%, BC%, U%, D, MO)
    If MS% = 1 Then MS% = 0: GoTo 5052


    8971 If B7%(P, B1!(P)) = 1 Then BR = 0
    If B7%(P, B1!(P)) <> 1 Then BR = B%(P, B3%(P, B1!(P)), 30)
    Call RANDNUM(RN)
    If S4% = 0 And (INFPOS% > 0 And RN <= 5 Or INFPOS% = 0 And RN <= 5) And BR > 8 Then P2 = 4: GoTo 4100
    ' MANAGER PROFILE TO HIT AND RUN
    Call RANDNUM(RN)
    If S4% = 0 Or MG%(P, 671) <> 999 Or B7%(P, B1!(P)) = 1 Then 8945
    If B%(P, B3%(P, B1!(P)), 4) = 0 Then I7! = 0 Else I7! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
    If B%(P, B3%(P, B1!(P)), 12) = 0 Then I6! = 0 Else I6! = Int(B%(P, B3%(P, B1!(P)), 4) / B%(P, B3%(P, B1!(P)), 12) + .5)
    8949 If S4% = 1 And O%(0) = 0 And A1 <= MG%(P, 672) And A1 >= MG%(P, 673) * -1 And RN <= MG%(P, 674) And I7! <= MG%(P, 872) And I6! >= MG%(P, 866) Then P2 = 2: GoTo 5067
    If S4% = 1 And O%(0) = 1 And A1 <= MG%(P, 675) And A1 >= MG%(P, 676) * -1 And RN <= MG%(P, 677) And I7! <= MG%(P, 873) And I6! >= MG%(P, 867) Then P2 = 2: GoTo 5067
    If S4% = 5 And O%(0) = 0 And A1 <= MG%(P, 678) And A1 >= MG%(P, 679) * -1 And RN <= MG%(P, 680) And I7! <= MG%(P, 874) And I6! >= MG%(P, 868) Then P2 = 2: GoTo 5067
    If S4% = 5 And O%(0) = 1 And A1 <= MG%(P, 681) And A1 >= MG%(P, 682) * -1 And RN <= MG%(P, 683) And I7! <= MG%(P, 875) And I6! >= MG%(P, 869) Then P2 = 2: GoTo 5067
    If S4% = 4 And O%(0) = 0 And A1 <= MG%(P, 684) And A1 >= MG%(P, 685) * -1 And RN <= MG%(P, 686) And I7! <= MG%(P, 876) And I6! >= MG%(P, 870) Then P2 = 2: GoTo 5067
    If S4% = 4 And O%(0) = 1 And A1 <= MG%(P, 687) And A1 >= MG%(P, 688) * -1 And RN <= MG%(P, 689) And I7! <= MG%(P, 877) And I6! >= MG%(P, 871) Then P2 = 2: GoTo 5067
    8945 If MG%(P, 609) <> 999 Or B7%(P, B1!(P)) <> 1 Then 8946
    If S4% <> 1 And S4% <> 2 And S4% <> 4 Then 8946
    Call RANDNUM(RN)
    If S4% = 1 And INNING% <= 7 And MG%(P, 610) >= O%(0) And A1 <= MG%(P, 611) And A1 >= MG%(P, 612) * -1 And RN <= MG%(P, 613) Then 8970
    If S4% = 1 And INNING% > 7 And MG%(P, 614) >= O%(0) And A1 <= MG%(P, 615) And A1 >= MG%(P, 616) * -1 And RN <= MG%(P, 617) Then 8970
    If S4% = 2 And INNING% <= 7 And MG%(P, 618) >= O%(0) And A1 <= MG%(P, 619) And A1 >= MG%(P, 620) * -1 And RN <= MG%(P, 621) Then 8970
    If S4% = 2 And INNING% > 7 And MG%(P, 622) >= O%(0) And A1 <= MG%(P, 623) And A1 >= MG%(P, 624) * -1 And RN <= MG%(P, 625) Then 8970
    If S4% = 4 And INNING% <= 7 And MG%(P, 626) >= O%(0) And A1 <= MG%(P, 627) And A1 >= MG%(P, 628) * -1 And RN <= MG%(P, 629) Then 8970
    If S4% = 4 And INNING% > 7 And MG%(P, 630) >= O%(0) And A1 <= MG%(P, 631) And A1 >= MG%(P, 632) * -1 And RN <= MG%(P, 633) Then 8970
    8946 If MG%(P, 634) <> 999 Or B7%(P, B1!(P)) = 1 Or O%(0) <> 0 Then 8947
    If S4% <> 1 And S4% <> 2 And S4% <> 4 Then 8947
    If B%(P, B3%(P, B1!(P)), 4) = 0 Then I7! = 0: I6! = 0: GoTo 8948
    I7! = Int(B%(P, B3%(P, B1!(P)), 6) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
    I6! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
    8948 Call RANDNUM(RN)
    If S4% = 1 And INNING% <= 7 And A1 <= MG%(P, 635) And A1 >= MG%(P, 636) * -1 And I7! <= MG%(P, 637) And I6! <= MG%(P, 638) And P%(D, P1%(D), 10) <= MG%(P, 639) And RN <= MG%(P, 640) Then 8970
    If S4% = 2 And INNING% <= 7 And A1 <= MG%(P, 641) And A1 >= MG%(P, 642) * -1 And I7! <= MG%(P, 643) And I6! <= MG%(P, 644) And P%(D, P1%(D), 10) <= MG%(P, 645) And RN <= MG%(P, 646) Then 8970
    If S4% = 4 And INNING% <= 7 And A1 <= MG%(P, 647) And A1 >= MG%(P, 648) * -1 And I7! <= MG%(P, 649) And I6! <= MG%(P, 650) And P%(D, P1%(D), 10) <= MG%(P, 651) And RN <= MG%(P, 652) Then 8970
    If S4% = 1 And INNING% > 7 And A1 <= MG%(P, 653) And A1 >= MG%(P, 654) * -1 And I7! <= MG%(P, 655) And I6! <= MG%(P, 656) And P%(D, P1%(D), 10) <= MG%(P, 657) And RN <= MG%(P, 658) Then 8970
    If S4% = 2 And INNING% > 7 And A1 <= MG%(P, 659) And A1 >= MG%(P, 660) * -1 And I7! <= MG%(P, 661) And I6! <= MG%(P, 662) And P%(D, P1%(D), 10) <= MG%(P, 663) And RN <= MG%(P, 664) Then 8970
    If S4% = 4 And INNING% > 7 And A1 <= MG%(P, 665) And A1 >= MG%(P, 666) * -1 And I7! <= MG%(P, 667) And I6! <= MG%(P, 668) And P%(D, P1%(D), 10) <= MG%(P, 669) And RN <= MG%(P, 670) Then 8970

    8947 P2 = 1
    If S4% = 1 Or S4% = 5 Then 8944
    If S4% = 2 Or S4% = 4 Then 8950
    8943 GoTo 8960
    8944 If A1 <= -1 And B%(P, B3%(P, B0%(1, 0)), 13) < 10 Or B0%(1, 2) > 1 Then 8943: 'CHANGED FROM <10
    If A1 >= -1 And S4% = 5 Then 8943
    If A1 < -5 Or B7%(P, B0%(1, 0)) = 1 And S4% = 1 Or B7%(P, B0%(2, 0)) = 1 And S4% = 2 Then 8943
    If A1 < 0 And INNING% >= 7 Then 8943
    P2 = 3: GoTo 4800
    8950 If B%(P, B3%(P, B0%(2, 0)), 13) < 40 Or B7%(P, B1!(P)) = 1 Or O%(0) = 2 Then 8943
    If B0%(2, 2) > 1 Then 8943
    P2 = 3: GoTo 4800
    8960 If S4% = 0 Or O%(0) > 0 Or S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 5 Or U% = 2 Or P = 0 And INNING% > 7 Then 8968
    If B7%(P, B1!(P)) = 1 And S4% = 1 And O%(0) < 2 Or B7%(P, B1!(P)) = 1 And S4% = 2 And O%(0) = 0 Or B7%(P, B1!(P)) = 1 And S4% = 4 And O%(0) = 0 Then 8969
    If B1!(P) > 2 And B1!(P) < 6 Or B4%(P, 0, B1!(P)) > 290 And B7%(P, B1!(P)) <> 1 Or B7%(P, B1!(P)) <> 1 And B4%(P, 3, B1!(P)) > 70 Then 8968
    If INNING% > 7 And A1 = -1 And O%(0) = 0 And S4% = 1 Then 8969
    8968 PPH = 0: PR% = 0: GoTo 8980
    8969 If O%(0) >= 1 And INNING% >= 7 Then 8968
    8970 P2 = 4: GoTo 4100
    8980 If O%(0) = 2 Or B4%(P, 4, B1!(P)) > 100 Or S4% = 0 Then 5064
    If S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 2 Or A1 < -3 Then 5064
    If Int(Rnd(1) * 100) + 1 > 20 Then 5064
    P2 = 2
    GoTo 5067
    8985 'PH FOR PITCHER WITH PROFILE
    Call RANDNUM(RN)
    MOB% = 0
    For I = 1 To 3
        If B0%(I, 0) > 0 Then MOB% = MOB% + 1
    Next
    If P%(P, P1%(P), 11) <> 999 Then I7! = P%(I, P1%(I), 11): GoTo 8986
    If P%(P, P1%(P), 20) = 0 Then I7! = 0: GoTo 8986
    I7! = Int((P%(P, P1%(P), 22) / P%(P, P1%(P), 20) * 1000 + .5))
    8986 For I = 0 To 2
        If INNING% >= MG%(P, 591 + I * 6) And A1 <= MG%(P, 592 + I * 6) * -1 And O%(0) <= MG%(P, 594 + I * 6) And I7! <= MG%(P, 595 + I * 6) And RN <= MG%(P, 596 + I * 6) And MOB% >= MG%(P, 593 + I * 6) Then 8930
    Next
    GoTo 8905
    8990 'SELECT PH FOR PITCHER
    I = P%(D, P1%(D), 0)
    I1 = 6
    If I < 0 Then I1 = 1
    For I = I1 To I1 + 4
        If MG%(P, I1 + 540) <> -1 And B%(P, MG%(P, I1 + 540), 21) = 0 Then PH% = MG%(P, I1 + 540): MG%(P, I1 + 540) = -1: GoTo 8934
    Next
    GoTo 8933
    8991 'PH FOR POS PLAYER CASES
    Call RANDNUM(RN)
    MOB% = 0
    For I = 1 To 3
        If B0%(I, 0) > 0 Then MOB% = MOB% + 1
    Next
    If B%(P, B3%(P, B1!(P)), 4) = 0 Then I7! = 0: I6! = 0: GoTo 8992
    I7! = Int(B%(P, B3%(P, B1!(P)), 6) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
    I6! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
    8992 For I = 0 To 2
        If INNING% >= MG%(P, 848 + I * 6) And A1 <= MG%(P, 849 + I * 6) * -1 And I6! <= MG%(P, 851 + I * 6) And I7! <= MG%(P, 852 + I * 6) And RN <= MG%(P, 853 + I * 6) And MOB% >= MG%(P, 850 + I * 6) Then 8909
    Next
    GoTo 8905

    8993 'SELECT PH FOR HITTER
    For I = 0 To 22
        If B%(P, I, 21) > 0 Or B$(P, I) = "XXX" Then 8994
        If B7%(P, B1!(P)) <> B%(P, I, 22) And B7%(P, B1!(P)) <> B%(P, I, 23) And B7%(P, B1!(P)) <> B%(P, I, 24) And B7%(P, B1!(P)) <> B%(P, I, 25) Then 8994
        GoTo 8995
    8994 Next
    GoTo 8935
    8995 I = P%(D, P1%(D), 0)
    I1 = 6
    If I < 0 Then I1 = 1
    For I = I1 To I1 + 4
        If MG%(P, I1 + 540) <> -1 And B%(P, MG%(P, I1 + 540), 21) = 0 Then I = MG%(P, I1 + 540): MG%(P, I1 + 540) = -1: GoTo 8916
    Next
    GoTo 8935
End Sub


'------------------------------
'   LOADSCREEN Subroutine
'------------------------------
' ...explanation...
Sub LOADSCREEN
    screen_handle = _NewImage(640, 480, 32)
    Screen screen_handle '32 bit Screen 12 dimensions
    park_graphic = _LoadImage("S$", 32)

    VA% = &HAA00 'PLAY BY PLAY BOX WINDOW
    MEMNEED% = 44 * 128 * 4 + 4
    ReDim VB%(MEMNEED% \ 2)

    _PutImage (20, 48)-Step(44, 128), park_graphic, screen_handle
    'CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),0)
    'CALL ClearVE

    _PutImage (53, 400)-Step(12, 16), park_graphic, screen_handle
    'VA%(0)=VA%+12800\16 + 1       'SAVE THE HOLD RUNNERS SECTION
    'va%(0)=va%
    'call GMove2VE (53,400,12,16,VA%(0),0)

    _PutImage (52, 416)-Step(14, 16), park_graphic, screen_handle
    'VA%(1)=VA%(0)+768\16 + 1      'INFIELD POSITIONING
    'CALL GMove2VE (52,416,14,16,VA%(1),0)

    _PutImage (38, 224)-Step(10, 32), park_graphic, screen_handle
    'VA%(2)=VA%(1)+896\16 + 1     'GET CENTER FIELDER
    'CALL GMove2VE (38,224,10,32,VA%(2),0)

    _PutImage (7, 256)-Step(10, 32), park_graphic, screen_handle
    'VA%(3)=VA%(2)+1280\16 + 1     'GET LEFT FIELDER
    'CALL GMove2VE (7,256,10,32,VA%(3),0)

    _PutImage (65, 256)-Step(10, 32), park_graphic, screen_handle
    'VA%(4)=VA%(3)+1280\16 + 1     'GET RIGHT FIELDER
    'CALL GMove2VE (65,256,10,32,VA%(4),0)

    _PutImage (30, 272)-Step(10, 32), park_graphic, screen_handle
    'VA%(5)=VA%(4)+1280\16 + 1     'GET SHORTSTOP
    'CALL GMove2VE (30,272,10,32,VA%(5),0)

    _PutImage (45, 272)-Step(10, 32), park_graphic, screen_handle
    'VA%(6)=VA%(5)+1280\16 + 1     'GET SECOND BASE
    'CALL GMove2VE (45,272,10,32,VA%(6),0)

    _PutImage (15, 304)-Step(10, 32), park_graphic, screen_handle
    'VA%(7)=VA%(6)+1280\16 + 1     'GET THIRD BASE
    'CALL GMove2VE (15,304,10,32,VA%(7),0)

    _PutImage (61, 304)-Step(10, 32), park_graphic, screen_handle
    'VA%(8)=VA%(7)+1280\16 + 1     'GET FIRST BASE
    'CALL GMove2VE (61,304,10,32,VA%(8),0)

    _PutImage (35, 336)-Step(13, 32), park_graphic, screen_handle
    'VA%(9)=VA%(8)+1280\16 + 1     'GET PITCHER
    'CALL GMove2VE (35,336,13,32,VA%(9),0)

    _PutImage (35, 416)-Step(15, 16), park_graphic, screen_handle
    'VA%(10)=VA%(9)+1664\16 + 1     'GET CATCHER
    'CALL GMove2VE (35,416,15,16,VA%(10),0)

    _PutImage (37, 304)-Step(10, 32), park_graphic, screen_handle
    'VA%(11)=VA%(10)+960\16 + 1     'RUNNER ON 2ND
    'CALL GMove2VE (37,304,10,32,VA%(11),0)

    _PutImage (15, 336)-Step(10, 32), park_graphic, screen_handle
    'VA%(12)=VA%(11)+1280\16 + 1     'RUNNER ON 3RD
    'CALL GMove2VE (15,336,10,32,VA%(12),0)

    _PutImage (56, 336)-Step(10, 32), park_graphic, screen_handle
    'VA%(13)=VA%(12)+1280\16 + 1     'RUNNER ON 1ST
    'CALL GMove2VE (56,336,10,32,VA%(13),0)

    VA%(16) = VA%(13) + 1280 \ 16 + 1 'LEFT HANDED BATTER
    MEMNEED% = 22 * 16 * 4 + 4
    ReDim VC%(MEMNEED% \ 2)

    _PutImage (30, 400)-Step(22, 16), park_graphic, screen_handle
    'CALL GMove4VE (30,400,22,16,VARSEG(VC%(0)),0)
    'CALL ClearVE

    _PutImage (1, 304)-Step(14, 176), park_graphic, screen_handle
    'CALL GMove2VE (1,304,14,176,VA%(16),0)
End Sub


'------------------------------
'   PBP Subroutine
'------------------------------
' ...explanation...
Sub PBP (F$)
    If displayPause <> 0 Then
        Call SPA(F$, 44)
        F$ = Left$(F$, 44)
        PB% = PB% + 1
        If PB% < 9 Then G$(PB%) = F$
        If PB% > 8 Then For I9 = 1 To 7: G$(I9) = G$(I9 + 1): Next: G$(8) = F$
        For I9 = 1 To 8

            _PrintString (I9 + 3, 20), G$(I9), park_graphic
            '--GPRINT0VE I9+3,20,G$(I9),15

        Next
        Call DELAY
    End If
End Sub


'------------------------------
'   WEATHER Subroutine
'------------------------------
' ...explanation...
Sub WEATHER_G
    Screen 0
    Call DRAWBOX(5)
    Locate 9, 11
    If L%(1, 9) = 1 Then Locate , 11: Print "TEMPERATURE: 70": Locate , 11: Print "WEATHER: DOME": Locate , 11: Print "WIND: NONE": GoTo 8719
    Locate , 11: Print "TEMPERATURE: "; TP%
    Locate , 11: Print "SKY CONDITIONS: "; SC$(SC%)
    WS% = Int(Rnd(1) * 15) + 7
    Locate , 11: Print "WIND: "; WI$
    Locate , 11: Print "PRECIPITATION: "; PC$(PC%)
    8719 I$=GetKeyPress$
End Sub


'-----------------------------------
' Lts2Menu Subroutine
'-----------------------------------
Sub Lts2Menu (Item$(), Prompt$(), Choice%, FGColor%, BGColor%)
    'Item$() and Prompt$() have one dimension
    'Choice% is the value returned, or zero if Escape is pressed
    'Colr% is the packed color to use for the display
    '
    'NOTE: The foreground and background portions of Colr% are reversed for
    '      the highlighted item.  Also, a new color is derived from Colr%
    '      for the prompt message - change this to suit by searching for
    '      the string "prompt color".
    '
    'NOTE: A comment near the end shows how to modify this menu to exit
    '      when a letter key is pressed, without also needing to press Enter.
    '      If you do this, each item should start with a unique first letter.
    '      Search for the string "having" to find where.

    Locate , , 0 'turn off the cursor

    'SplitColor Colr%, FGColor%, BGColor% 'split color into FG and BG
    ReDim ScrBuf%(0 To 159)
    TRow% = CsrLin

    'PCopy 0, 1
    oldmode& = _CopyImage(0) 'the 0 value designates the current destination SCREEN

    Call ClearEOL(7, 0) 'clear any old prompt information

    '-- determine how many choices there are
    NumItems% = UBound(Item$) 'get the number of items passed

    While Item$(NumItems%) = "" 'seek the last non-blank item
        NumItems% = NumItems% - 1
    Wend

    ReDim Break%(NumItems% + 1) 'remembers which items start on a new screen
    Break%(NumItems% + 1) = -2 'mark one past the end as a flag
    Col% = 1 'tracks the accumulated line width and columns
    Page% = 1 'tracks which item begins a new screen page

    '-- Build a table of starting columns and page breaks for each item.
    '   For each item that begins a new page, use a negative version of the
    '   page number as a flag.  we don't need the starting column for those
    '   anyway, because the first item on each page always is in column 1.
    '   For all other items, the equivalent Break%() element will hold the
    '   column in which this item is located.
    For X% = 1 To NumItems%
        Break%(X%) = Col% 'assume we're not starting a new screen page
        If X% = 1 Then Break%(X%) = -1
        Col% = Col% + Len(Item$(X%)) + 2 'add two for a small border
        If Col% > 80 Then
            Page% = Page% + 1 'we advanced to the next page
            Break%(X%) = -Page% 'this item starts a page, remember which one
            Col% = Len(Item$(X%)) + 3
        End If
    Next

    '-- print a page-full of items
    Choice% = 1 'start with item 1 the first time

    QPPrintLt2:
    X% = Choice% 'assume we'll start at the current item

    While Break%(X%) > 0 'seek the first item on this page
        X% = X% - 1
    Wend

    Do 'print until the negative Break%() flag
        C_FG% = FGColor% 'assume the item being printed is NOT current
        C_BG% = BGColor%

        If X% = Choice% Then 'we were wrong, reverse the FG and BG colors
            'C% = OneColor%(BG%, FG% And 7)
            C_FG% = BGColor%
            C_BG% = FGColor%

            Locate CsrLin + 1, 1 'go down a line and to the first column
            Call ClearEOL(7, 0) 'clear any old prompt information

            'you may adjust the prompt color by changing the line below
            Locate CsrLin - 2 'return to the line above

            ' ((NOT Colr) AND 7) OR 2
            QPrint Space$(1) + Prompt$(X%) + Space$(1), 2, 0, -1
            Locate CsrLin + 1 'return to the line above
        End If

        If Break%(X%) < 0 Then 'if it's the first item on this page
            Locate , 1 'locate to the first column
            If ClearFlag% Then 'if we just crossed a page boundary
                Call ClearEOL(FGColor%, BGColor%) '  clear any old items that may still show
                ClearFlag% = 0
            End If
        Else 'otherwise
            Locate , Break%(X%) '  locate to the correct column
        End If

        QPrint Space$(1) + Item$(X%) + Space$(1), C_FG%, C_BG%, -1
        X% = X% + 1 'advance to the next item
    Loop Until Break%(X%) < -1

    Do
        X$ = GetKeyPress$
    Loop Until Len(X$)

    If Len(X$) = 2 Then 'extended key
        Select Case Asc(Right$(X$, 1))
            Case 71 'Home Key
                Choice% = 1 'set to the first item in the list
                ClearFlag% = 1 'flag to clear old items
            Case 75 'Left Arrow
                If Choice% > 1 Then
                    Choice% = Choice% - 1
                    If Break%(Choice%) < 0 Then ClearFlag% = 1
                Else
                    Choice% = NumItems%
                    ClearFlag% = 1
                End If
            Case 77 'Right Arrow
                If Choice% < NumItems% Then
                    Choice% = Choice% + 1
                    If Break%(Choice%) < 0 Then ClearFlag% = 1
                Else
                    Choice% = 1
                    ClearFlag% = 1
                End If

            Case 79 'End Key
                Choice% = NumItems%
                ClearFlag% = 1
            Case Else
        End Select
    Else
        Select Case X$
            Case Chr$(13) 'Enter Key
                GoTo QPExitLt2 'exit with the current choice

            Case Chr$(27) 'Escape Key
                Choice% = 0 'set the choice to zero as an Escape flag
                GoTo QPExitLt2

            Case Else 'they must be typing a choice's first letter
                '--- haha, bold of you to assume that!!!

                X% = Choice% 'scan through each item looking for a match

                Do 'starting at the item after the current one
                    If X% = NumItems% Then X% = 0 'wrap past the last one
                    X% = X% + 1 'consider the next item
                    If Asc(UCase$(Item$(X%))) = Asc(UCase$(X$)) Then
                        Choice% = X% 'we found one, save where
                        ClearFlag% = 1 'set flag to clear old stuff
                        'EXIT DO                      '  and stop searching
                        GoTo QPExitLt2
                        'change the EXIT DO above to GOTO QPExitLt2 to exit without
                        '  having to press Enter
                    End If

                    If X% = Choice% Then Exit Do 'we already went around once
                Loop

        End Select

    End If

    GoTo QPPrintLt2

    QPExitLt2:


    Screen oldmode&
    'PCopy 1, 0
    Erase Break%, ScrBuf%

End Sub


Sub boxie0 (TopRow%, LeftColumn%, BottomRow%, RightColumn%, Char%, FGColor%, BGColor%)

    '1 = single line all around
    '2 = double line all around
    '3 = double line horizontally, single line vertically
    '4 = single line horizontally, double line vertically
    'If Char% is assigned to any other value, that ASCII character will be used for the entire box.

    Select Case Char%
        Case 1:
            ulCorner$ = Chr$(218)
            horizontal$ = Chr$(196)
            urCorner$ = Chr$(191)
            vertical$ = Chr$(179)
            llCorner$ = Chr$(192)
            lrCorner$ = Chr$(217)
        Case 2:
            ulCorner$ = Chr$(201)
            horizontal$ = Chr$(205)
            urCorner$ = Chr$(187)
            vertical$ = Chr$(186)
            llCorner$ = Chr$(200)
            lrCorner$ = Chr$(188)
        Case 3:
            ulCorner$ = Chr$(213)
            horizontal$ = Chr$(205)
            urCorner$ = Chr$(184)
            vertical$ = Chr$(179)
            llCorner$ = Chr$(212)
            lrCorner$ = Chr$(190)
        Case 4:
            ulCorner$ = Chr$(214)
            horizontal$ = Chr$(196)
            urCorner$ = Chr$(183)
            vertical$ = Chr$(186)
            llCorner$ = Chr$(211)
            lrCorner$ = Chr$(189)
        Case Else:
            ulCorner$ = Chr$(Char%)
            horizontal$ = Chr$(Char%)
            urCorner$ = Chr$(Char%)
            vertical$ = Chr$(Char%)
            llCorner$ = Chr$(Char%)
            lrCorner$ = Chr$(Char%)
    End Select

    Color FGColor%, BGColor%

    ' Prints the top of the box
    Locate TopRow%, LeftColumn%
    Print ulCorner$ + String$(RightColumn% - LeftColumn%, Asc(horizontal$)) + urCorner$

    ' Print the sides of the box
    For currentRow = (TopRow% + 1) To (BottomRow% - 1) Step 1
        Locate currentRow, LeftColumn%
        Print vertical$ + Space$(RightColumn% - LeftColumn%) + vertical$
    Next

    'Print the bottom line of the box
    Print llCorner$ + String$(RightColumn% - LeftColumn%, Asc(horizontal$)) + lrCorner$

End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'ROUTINES.BM'

