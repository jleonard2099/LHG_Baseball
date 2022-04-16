$Debug
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'ROUTINES.BI'
'$INCLUDE: 'SOURCE.BI'
'$INCLUDE: 'SOURCEG.BI'

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

Type PlayCall
    YY As String * 30
End Type

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

'MON = Monitor%
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
Input #1, displayPause: Input #1, vg%: Input #1, CGERA%
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
displayPause = NG%(4) / 10
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
Color 14: Print "(D) ";: Color 15: Print Using "DISPLAY INTERVAL: #.# "; displayPause
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

210 Locate 16, 22: Input displayPause: GoTo 171

205 Open "DEFERA" For Output As #1: Print #1, U6: Print #1, LGT: Print #1, DH%
Print #1, DL: Print #1, MO%: Print #1, DN%: Print #1, WE%
Print #1, IJ%: Print #1, LR%: Print #1, DR%: Print #1, MP%
Print #1, displayPause: Print #1, vg%: Print #1, CGERA%
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
    
    2862 CALL box0(6, 58, 17, 80, 2,  15, 1)
    
    Call NEWLINES(P9)
    
    GoSub 1590
    GoSub 1370
    Locate 24, 1
    
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
            Call CSMsgBox(Message$, Wdth, Cnf)
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
            Call CSMsgBox(Message$, Wdth, Cnf)
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
        Call CSMsgBox(Message$, Wdth, Cnf)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 2862
    End If
    If SELECTION% = 6 Then
        Call ACTBATTERS(B%(), P9)

        Locate 9
        Message$ = "All batters are now activated for duty. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Cnf)
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

1370 CALL box0(19, 58, 22, 80, 2,  15, 1)
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
Else
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

CALL box0(4, 9, 20, 38, 2,  15, 1)
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
        Call CSMsgBox(Message$, Wdth, Cnf)
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
    Call CSMsgBox(Message$, Wdth, Cnf)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

    GoTo 4501
End If
If SELECTION% = 5 Then

    Call ACTPITCHERS(P%(), P9)

    Locate 9
    Message$ = "All pitchers are now activated for duty. Press any key to continue"
    Wdth = 54
    Call CSMsgBox(Message$, Wdth, Cnf)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

    GoTo 4501
End If
If SELECTION% = 6 And SP%(P9) = -1 Then

    Locate 9
    Message$ = "A starting pitcher must be selected before proceeding. Press any key to continue"
    Wdth = 54
    Call CSMsgBox(Message$, Wdth, Cnf)
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
    Call CSMsgBox(Message$, Wdth, Cnf)

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
        Call CSMsgBox(Message$, Wdth, Cnf)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4514
    End If
    If SCANCODE% = 49 Then
        Locate 9
        Message$ = P$(P9, P1%(P9)) + " must face at least one batter. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Cnf)
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
        Call CSMsgBox(Message$, Wdth, Cnf)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        GoTo 4514
    End If
    If SELECTION% = 3 Then
        Locate 9
        Message$ = P$(P9, P1%(P9)) + " must face at least one batter. Press any key to continue"
        Wdth = 54
        Call CSMsgBox(Message$, Wdth, Cnf)
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


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'ROUTINES.BM'
'$INCLUDE: 'SOURCE.BM'
'$INCLUDE: 'SOURCEG.BM'

