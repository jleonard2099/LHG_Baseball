$Debug

'$INCLUDE: 'Game_Routines.BI'
'$INCLUDE: 'Game_Routines_Gfx.BI'
'$INCLUDE: 'SOURCE.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'

On Error GoTo Errhandler

'Open "pbplog" For Append As #7

_Title "Full Count Baseball - PLAY BALL!"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------


Data A,B,C,D,E,F,G,H,I,J,K,L,M,dh," p"," c",1b,2b,3b,ss,lf,cf,rf,ph,pr
For I = 0 To 12: Read X$(I): Next I
For I = 0 To 11: Read C$(I): Next I

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

For I = 0 To 1
    For I1 = 1 To 9
        For I2 = 1 To 9
            Read BT%(I, I1, I2)
        Next
    Next
Next

For I = 2 To 9
    For I1 = 0 To 2
        Read CF%(I, I1)
    Next
Next

For I = 2 To 9
    For I1 = 0 To 1
        Read LB%(I, I1)
    Next
Next

For I = 0 To 5
    For I1 = 0 To 2
        Read SO%(I, I1)
    Next
Next

For I = 11 To 0 Step -1
    For I1 = 0 To 2
        Read TP%(I, I1)
    Next
Next

'----------------------------------------
'    INITIALIZE ALL VARIABLES
'----------------------------------------

normalFinish% = 0

'Graphics co-ordinates originally were for a
'routine that operated as if the screen were
'still just 80 column width; so 80x8 = 640
FontColAdj% = 8

MON = Monitor%
'CALL InitMouse(There)

BK$ = Space$(25)

VI$(0) = "VISITING TEAM"
VI$(1) = "HOME TEAM"

TM$(1) = "HOT"
TM$(2) = "WARM"
TM$(3) = "COOL"
TM$(4) = "COLD"

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

SC$(1) = "CLEAR"
SC$(2) = "PARTLY CLOUDY"
SC$(3) = "OVERCAST"
SC$(4) = "DOME"

PC$(1) = "THUNDERSTORMS"
PC$(2) = "SHOWERS"
PC$(3) = "DRIZZLE"
PC$(4) = "NONE"

SP%(0) = -1
SP%(1) = -1

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

month$(4) = "APRIL"
month$(5) = "MAY"
month$(6) = "JUNE"
month$(7) = "JULY"
month$(8) = "AUGUST"
month$(9) = "SEPTEMBER"
month$(10) = "OCTOBER"
MO% = 4

dayNight$(0) = "AFTERNOON"
dayNight$(1) = "NIGHT"

B1$(0) = "LP"
B1$(1) = "L"
B1$(2) = "S"
B1$(3) = "R"
B1$(4) = "RP"

H$(1) = "1b"
H$(2) = "2b"
H$(3) = "3b"
H$(4) = "home"

H0$(0) = " "
H0$(1) = "H"

P = 1

For I = 0 To 1: For I1 = 0 To 4: NB%(I, I1) = -1: Next: Next
For I = 0 To 1: P1%(I) = -1: P6%(I) = -1: Next

SP%(0) = -1
SP%(1) = -1

For I = 0 To 1
    For J = 1 To 9
        M%(J) = 0
        B7%(I, J) = -1
        B3%(I, J) = -1
    Next
Next

Call INITVARI

'----------------------------------------
'       DETERMINE GAME OPTIONS
'----------------------------------------

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 3
        Print #1, _CWD$ + "\"
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, diskPaths$(I)
Next
Close 1


If Not _FileExists("DEFERA") Then
    Open "DEFERA" For Output As #1
    For I = 1 To 4:
        Print #1, 0
    Next

    Print #1, 4

    For I = 6 To 14:
        Print #1, 0
    Next

    Close 1
End If

Open "DEFERA" For Input As #1
Input #1, U6
Input #1, LGT
Input #1, DH%
Input #1, DL
Input #1, MO%
Input #1, DN%
Input #1, WE%
Input #1, IJ%
Input #1, LR%
Input #1, DR%
Input #1, MP%
Input #1, pbpDelay!
Input #1, VG%
Input #1, CGERA%
Close 1


'Load game in-progress, if there is one
If _FileExists("GAME82") Then

    Open "GAME82" For Input As #2
    Input #2, scheduleFile$
    For I = 0 To 2: Input #2, AP%(I): Next
    Close 2

    Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = 48

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q2$(X + 1), 48 - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q2$(22), 2 As Q2$(23), 3 As Q2$(20), 3 As Q2$(21)

    Get #1, AP%(0) + AP%(2)

    For X = 0 To 18
        NG%(X) = CVI(Q2$(X + 1))
    Next

    For X = 0 To 1
        YN$(X) = _Trim$(Q2$(20 + X))
    Next

    VS% = CVI(Q2$(22))
    HS% = CVI(Q2$(23))

    Close 1

    AP% = 1
End If

If AP% = 1 Then
    'Setup variables for auto-play
    U6 = NG%(8)
    DH% = NG%(3)
    pbpDelay! = NG%(4) / 10
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

    Select Case U6
        Case 0: '
        Case 1: U9 = 0
        Case 3: U6 = 2
        Case Else:
            U9 = 1
            U6 = 1
    End Select

    'At this point we can skip the
    'option selection below
Else

    Randomize Timer

    170 '
    Call INFO

    Do
        Color 15, 0
        Cls
        'Color , 0
        
        Locate 3, 1
        Color 14: Print "(1) ";: Color 15: Print U6$(U6)
        Color 14: Print "(2) ";: Color 15: Print LGT$(LGT)
        Color 14: Print "(3) ";: Color 15: Print DH$(DH%)
        Color 14: Print "(4) ";: Color 15: Print DL$(DL)
        Color 14: Print "(5) ";: Color 15: Print month$(MO%)
        Color 14: Print "(6) ";: Color 15: Print dayNight$(DN%)
        Color 14: Print "(7) ";: Color 15: Print "PROCEED TO TEAM SELECTION"
        Color 14: Print "(8) ";: Color 15: Print "USE CLIMATE/WIND/TEMP DATA "; WE$(WE%)
        Color 14: Print "(9) ";: Color 15: Print "USE INJURIES "; WE$(IJ%)
        Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS AS DEFAULT"
        Color 14: Print "(A) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; WE$(LR%)
        Color 14: Print "(B) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; WE$(DR%)
        Color 14: Print "(C) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; WE$(MP%)
        Color 14: Print "(D) ";: Color 15: Print Using "DISPLAY INTERVAL: #.# "; pbpDelay!
        Color 14: Print "(E) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; WE$(VG%)
        Color 14: Print "(F) ";: Color 15: Print "PITCHER USAGE ERA: "; PU$(CGERA%)

        Do
            Color 7, 0

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then Call LetterToNumber(I$)

            I = Val(I$)

        Loop Until I >= 0 And I <= 16 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

            Select Case I + 1
                Case 1:
                    Open "DEFERA" For Output As #1
                    Print #1, U6
                    Print #1, LGT
                    Print #1, DH%
                    Print #1, DL
                    Print #1, MO%
                    Print #1, DN%
                    Print #1, WE%
                    Print #1, IJ%
                    Print #1, LR%
                    Print #1, DR%
                    Print #1, MP%
                    Print #1, pbpDelay!
                    Print #1, VG%
                    Print #1, CGERA%
                    Close 1

                Case 2:
                    U6 = U6 + 1
                    If U6 > 2 Then U6 = 0

                Case 3:
                    LGT = LGT + 1
                    If LGT > 2 Then LGT = 0

                Case 4:
                    DH% = 1 - DH%

                Case 5:
                    DL = 1 - DL

                Case 6:
                    MO% = MO% + 1
                    If MO% > 10 Then MO% = 4
                    Locate 7, 4: Print BK$

                Case 7:
                    DN% = 1 - DN%
                    Locate 8, 4
                    Print BK$

                Case 8:
                    '

                Case 9:
                    WE% = 1 - WE%

                Case 10:
                    IJ% = 1 - IJ%

                Case 11:
                    LR% = 1 - LR%

                Case 12:
                    DR% = 1 - DR%

                Case 13:
                    MP% = 1 - MP%

                Case 14:
                    Locate 16, 22: Input pbpDelay!

                Case 15:
                    VG% = 1 - VG%

                Case 16:
                    CGERA% = CGERA% + 1
                    If CGERA% > 8 Then CGERA% = 0

            End Select

        Else

            End
            System

        End If

    Loop Until I$ = "7"

End If

YV = 1
YM = 0

'-------------------------
' Determine umpires
'-------------------------
If _FileExists(diskPaths$(0) + "UMPIRES." + UMP$(LGT)) Then

    YU = 0

    Open diskPaths$(0) + "UMPIRES." + UMP$(LGT) For Input As #1
    For I1 = 1 To 50
        Input #1, UMP$(I1)
        Input #1, U%(I1)
    Next

    Close #1

    For I1 = 1 To 50
        If UMP$(I1) <> "" Then YU = YU + 1
    Next

    If YU < 4 Then
        'If there aren't 4 umpires, assign generic names
        U$(1) = "RHODES": U$(2) = "WINBERRY": U$(3) = "TIPPETT": U$(4) = "MILLER"
    Else
        'Randomly assign umpires
        Do
            While (UMP$(YU) = "" Or YU = YM)
                YU = Int(Rnd(1) * 50) + 1
            Wend

            U$(YV) = UMP$(YU): YM = YU: YV = YV + 1
        Loop Until YV >= 5

    End If
Else
    'Assign generic umpire names if we don't have any
    U$(1) = "RHODES": U$(2) = "WINBERRY": U$(3) = "TIPPETT": U$(4) = "MILLER"
End If

'-------------------------
' Team Selection
'-------------------------
For I = 0 To 1

    If AP% = 1 Then
        T1%(I) = NG%(I)
        YN$ = YN$(I)
        Call LoadTeam_LOADER(T1%(I), YN$, I)
    Else

        Call INFO

        Do
            Color 15, 0
            Cls
            JJ = 10
            Locate 6, 8: Color 15, 4
            Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

            For II = 1 To JJ:
                Locate 6 + II, 8
                Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
            Next
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

            If NY$ = "" And YN$ <> "" Then
                'NY$ = YN$
                Call SelectTeam_LOADER(NTMS, YN$, I) 'GoSub 8102
            Else
                YN$ = NY$
                Erase teamNames$

                NTMS = 0

                Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
                Y = LOF(1)

                Field #1, 15 As Q2$(0)

                For I1 = 1 To Y / 4342

                    Get #1, I1

                    For I2 = 1 To 15
                        If Left$(Q2$(0), 1) = " " Or Asc(Mid$(Q2$(0), I2, 1)) < 32 Or Asc(Mid$(Q2$(0), I2, 1)) > 123 Or UCase$(Left$(Q2$(0), 3)) = "NOT" Or UCase$(Left$(Q2$(0), 3)) = "UN" Then skipCount% = 1: Exit For
                    Next I2

                    If skipCount% <> 1 Then
                        NTMS = NTMS + 1
                        teamNames$(NTMS) = Q2$(0)
                        UV%(NTMS) = I1
                    End If

                Next I1

                Close 1

                Call SelectTeam_LOADER(NTMS, YN$, I)
            End If

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color L%(I, 11), L%(I, 12): Print A$(I)

                Color 15, 4

                Do
                    Locate , 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "N"

        YN$(I) = YN$
        A$(I) = RTrim$(A$(I))

        Color 15, 0

    End If 'Done checking for autoplay

Next I

'-------------------------
' Team Mgmt + Lineups
'-------------------------

N = 1

'Determine which team PC will play
If U6 = 1 And AP% <> 1 Then
    'If not doing autoplay...
    Call INFO

    Color 14: Print "WHICH TEAM DOES COMPUTER MANAGE ?": Print

    Color 15: Print "(0) ";: Color L%(0, 11), L%(0, 12): Print A$(0): Color 7, 0
    Print
    Color 15: Print "(1) ";: Color L%(1, 11), L%(1, 12): Print A$(1): Color 7, 0

    Do
        I$ = GetKeyPress$
        U9 = Val(I$)
    Loop Until U9 = 0 Or U9 = 1
End If

'Determine pitchers
For P9 = 0 To 1

    cancelPitchers% = 0

    '4500 / 2040
    Call SelectPitchers(P9, cancelPitchers%, computerRotations%)

    '1590
    Call PitchingStarter(P9)
    If Inotloop% <= 5 And S6%(P9, 0) - S6%(1 - P9, 0) > 0 Then P2%(P9) = P1%(P9)

    '1060
    S8%(P9, 0) = Int(((P%(P9, P1%(P9), 8) + P%(P9, P1%(P9), 7)) / P%(P9, P1%(P9), 4)) + .5)
    S8%(P9, 1) = 3

    If P6%(P9) > 1 Then S8%(P9, 1) = 0

    'I'm not sure why we go back only
    'if ESC was pressed during pitcher select
    If cancelPitchers% = 1 Then GoTo 170

Next P9

'Determine lineup for each team
For P9 = 0 To 1

    Do
        480 '
        Cls
        Call INFO
        Locate 3, 1
        Color L%(P9, 11), L%(P9, 12): Print A$(P9): Color 7, 0: Print
        Color 14: Print "(0) ";: Color 15: Print "INPUT A LINEUP MANUALLY": Print
        Color 14: Print "(1) ";: Color 15: Print "SELECT A LINEUP FROM MANAGER PROFILE": Print
        Color 14: Print "(2) ";: Color 15: Print "COMPUTER DETERMINE LINEUP RANDOMLY": Print
        Color 14: Print "(3) ";: Color 15: Print "COMPUTER DETERMINE LINEUP FROM MANAGER PROFILE"
        reselect = 0

        If AP% = 1 Then

            'NLF% = no lineup found?
            If NLF% = 1 Then
                NLF% = 0
                I$ = "2"
            Else
                If (U6 = 2 Or U6 = 1 And U9 = P9) Then
                    If MP% = 0 Then
                        I$ = "1"
                    Else
                        I$ = "2"
                    End If
                End If
            End If
        Else

            Do
                I$ = GetKeyPress$
                I1 = Val(I$)
            Loop Until I1 >= 0 And I1 <= 3 And I$ <> Chr$(27)

        End If

        Select Case I$
            Case "0":
                'Lineup - no profile

                Call SelectBatters(batterFlag%, P9)

                If batterFlag% = 1 Then
                    For I = 10 To 22
                        Locate I, 59: Print Space$(21);
                    Next

                    Call StartingLineup(P9)
                    Call ChangeLineup_PreGame(P9, reselect)

                    If noLineups = 1 Then
                        noLineups = 0
                    End If
                End If

            Case "1", "3":
                'Lineup using profile

                '2500
                Call SearchForLineup(P9)

                '1830
                Call LineupFromProfile(lineupFound%, P9, skipLineChange%)

                'Lineup found?
                If lineupFound% = 1 Or skipLineChange% = 0 Then
                    Call ChangeLineup_PreGame(P9, reselect)
                    If noLineups = 1 Then
                        noLineups = 0
                    End If
                Else
                    noLineups = 1
                End If

            Case "2"
                'GoSub 5000
                Call ComputerLineups(batterFlag%, P9, noLineups, reselect)

                If noLineups = 1 Then
                    noLineups = 0
                    skipLineChange% = 1
                End If

        End Select

        If I$ <> Chr$(27) Then
            'We didn't ESC so we have to move forward

            If skipLineChange% <> 1 Then
                Color 15, 0
                Locate 5, 59: Print "OPPOSING PITCHER"
                Locate , 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
                If RP = 1 Then RP = 0
            End If

        Else
            'ESC means we go back to earlier menus
            noLineups = 1
            GoTo 170

        End If

    Loop Until noLineups = 0 And reselect = 0

Next P9



'-------------------------
' Ready to Play
'-------------------------

For I = 0 To 1
    For I1 = 1 To 9
        X0%(I, 0, I1) = B3%(I, I1)
        X0%(I, 1, I1) = B7%(I, I1)
        X0%(I, 2, I1) = I1 * 10
    Next I1
    D0%(I) = 9
Next I



W5 = 0
W6 = 1
VV(0) = 1
VV1(0) = 9
VV(1) = 1
VV1(1) = 9

Cls

RD = 0

For I = 0 To 1
    For I1 = 1 To 9
        If B7%(I, I1) >= 2 And B7%(I, I1) <= 9 Then
            If B7%(I, I1) = B%(I, B3%(I, I1), 22) Then
                B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 33): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 34): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 26)
            Else
                If B7%(I, I1) = B%(I, B3%(I, I1), 23) Then
                    B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 35): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 36): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 27)
                Else
                    If B7%(I, I1) = B%(I, B3%(I, I1), 24) Then
                        B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 37): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 38): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 28)
                    Else
                        If B7%(I, I1) = B%(I, B3%(I, I1), 25) Then
                            B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 39): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 40): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 29)
                        Else

                            If B7%(I, I1) = 2 Then B%(I, B3%(I, I1), 15) = 1: B%(I, B3%(I, I1), 17) = 0: B%(I, B3%(I, I1), 19) = 910 + Int(Rnd(1) * 10) + 1
                            If B7%(I, I1) = 3 Then B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 960 + Int(Rnd(1) * 10) + 1
                            If B7%(I, I1) = 4 Then B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 912 + Int(Rnd(1) * 10) + 1
                            If B7%(I, I1) = 5 Or B7%(I, I1) = 6 Then B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 902 + Int(Rnd(1) * 10) + 1
                            If B7%(I, I1) >= 7 Then B%(I, B3%(I, I1), 15) = 2: B%(I, B3%(I, I1), 17) = 2: B%(I, B3%(I, I1), 19) = 972 + Int(Rnd(1) * 10) + 1
                        End If
                    End If
                End If
            End If
        End If
    Next I1
Next I

Call SelectStadium

Call GETWEATHER(WD%, WS%, MO%, WF!, SC%, TP%, RD)

Cls

Call DRAWBOXA

Locate , 11: Print "CONDITIONS AT "; S$(1)
If L%(1, 9) = 1 Then
    Locate , 11: Print "TEMPERATURE: 70"
    Locate , 11: Print "WEATHER: DOME"
    Locate , 11: Print "WIND: NONE"
Else
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

End If

S$ = PARK$

I1 = Val(Left$(A$(0), 2))
I2 = Val(Left$(A$(1), 2))

If Val(Left$(A$(0), 3)) = 211 Then I1 = 201
If Val(Left$(A$(0), 3)) = 200 Then I1 = 200
If Val(Left$(A$(1), 3)) = 200 Then I2 = 200
If Val(Left$(A$(1), 3)) = 211 Then I2 = 201

If CGERA% > 2 Then S8%(1, 1) = 4: S8%(0, 1) = 4
If CGERA% > 3 Then S8%(1, 1) = 5: S8%(0, 1) = 5
If CGERA% = 7 Then S8%(1, 1) = 7: S8%(0, 1) = 7
If CGERA% >= 8 Then S8%(1, 1) = 99: S8%(0, 1) = 99

Call GETSTATS
Call DISPBAVG(D)

W5 = 0
W6 = 1

If (MON = 6 Or MON = 7 Or MON = 12) And VG% = 0 Then
    usingGfx = 1
Else
    usingGfx = 0
End If

Call SOURCE

End
System

'================================================================================

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error occurred! " '; Date$; " "; Time$
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
Print #9, A$(0) + " vs " + A$(1)
Print #9, "D0%(P)      "; D0%(P)
Print #9, "B3%(0, X)   "; Using "## ## ## ## ## ## ## ## ## ##"; B3%(0, 0); B3%(0, 1); B3%(0, 2); B3%(0, 3); B3%(0, 4); B3%(0, 5); B3%(0, 6); B3%(0, 7); B3%(0, 8); B3%(0, 9)
Print #9, "B3%(1, X)   "; Using "## ## ## ## ## ## ## ## ## ##"; B3%(1, 0); B3%(1, 1); B3%(1, 2); B3%(1, 3); B3%(1, 4); B3%(1, 5); B3%(1, 6); B3%(1, 7); B3%(1, 8); B3%(1, 9)
Print #9,
Close #9
Resume Next ' moves program to code following the error.

'================================================================================

'------------------------------
'   StartingLineup Subroutine
'------------------------------
' ...explanation...
Sub StartingLineup (P9)

    540 '

    'track assigned positions
    '0 = unassigned
    For I = 0 To 9:
        M%(I) = 0
    Next I

    '1370
    Locate 25, 1

    For I1 = 0 To 9
        If DH% <> 0 Or I1 <> 0 Then
            If DH% <> 1 Or I1 <> 1 Then
                Locate 24, (I1 * 3) + 1
                If M%(I1) = 0 Then Print C$(I1); "  ";
                If M%(I1) = 1 Then Print "   ";

                Locate 25, (I1 * 3) + 1
                If M%(I1) = 0 Then Print I1; "  ";
                If M%(I1) = 1 Then Print "   ";
            End If
        End If
    Next I1

    '550
    For I = 1 To 9
        Color 15, 0
        Locate 5, 59: Print "OPPOSING PITCHER"
        Locate , 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
        Call LineupPositions(P9, I, skipLU%)
        If C1 = 1 Then Exit For
    Next I

    If skipLU% = 1 Then GoTo 540

End Sub

'------------------------------
'   LineupPositions Subroutine
'------------------------------
' ...explanation...
Sub LineupPositions (teamIdx, posIdx, skipLU%)

    'Do

    560 '
    Call POSITION(posIdx)
    Print Space$(18);
    Call POSITION(posIdx)

    'Get keypress for batter #
    I$ = GetKeyPress$

    If I$ = Chr$(27) Then

        For J = 1 To 9
            M%(J) = 0
            B7%(teamIdx, J) = -1
            B3%(teamIdx, J) = -1
        Next

        C1 = 0

        For J = 0 To 22
            B%(teamIdx, J, 21) = 0
        Next

        RP = 1

        For J = 0 To 22
            If B%(teamIdx, J, 31) <> 0 Then B%(teamIdx, J, 21) = 99
        Next

    Else

        Call LetterToNumber(I$)
        I1 = Val(I$)

        If UCase$(I$) <> "X" Then

            '590
            For J = 1 To 9
                If Not (J = posIdx Or (B3%(teamIdx, J) = I1 And B7%(teamIdx, J) = 1)) Then
                    If B3%(teamIdx, J) = I1 And UCase$(I$) <> "P" Then GoTo 560 'skip1% = 1
                End If
            Next J

            '620
            If Not (DH% = 1 And UCase$(I$) = "P") Or Not (B%(teamIdx, I1, 21) <> 0 And UCase$(I$) <> "P") Then

                '640
                If UCase$(I$) = "P" Then

                    '760
                    For J = 1 To 9
                        If J <> posIdx Then
                            If B3%(teamIdx, J) = P1%(teamIdx) And B7%(teamIdx, J) = 1 Then skip2% = 1
                        End If
                    Next J

                    If skip2% <> 1 Then
                        '790
                        B3%(teamIdx, posIdx) = P1%(teamIdx)
                        Print P$(teamIdx, P1%(teamIdx));
                        M%(I1) = 1

                        Locate , 75

                        Print "P ";
                        B7%(teamIdx, posIdx) = 1
                        B9%(teamIdx, 1) = P1%(teamIdx)
                        M%(1) = 1

                        '1370
                        Locate 25, 1
                        For I0 = 0 To 9
                            If DH% <> 0 Or I0 <> 0 Then
                                If DH% <> 1 Or I0 <> 1 Then

                                    Locate 24, (I0 * 3) + 1
                                    If M%(I0) = 0 Then Print C$(I0); "  ";
                                    If M%(I0) = 1 Then Print "   ";

                                    Locate 25, (I0 * 3) + 1
                                    If M%(I0) = 0 Then Print I0; "  ";
                                    If M%(I0) = 1 Then Print "   ";

                                End If
                            End If
                        Next I0

                        '-- this is taken care of outside the routine
                        'If C1 = 1 Then Call ChangeLineup_Pregame

                    End If

                Else

                    '650
                    B3%(teamIdx, posIdx) = I1
                    Print Using "# "; posIdx;
                    Print B$(teamIdx, B3%(teamIdx, posIdx));

                    Locate , 74

                    680 '
                    'Input fielding position
                    'If DH% = 0 Then firstSlot = 0 Else firstSlot = 1
                    firstSlot = 1 'pitcher

                    Do
                        J$ = GetKeyPress$
                        J1 = Val(J$)
                    Loop Until J1 <> firstSlot

                    For J = 1 To 9
                        If J <> posIdx Then
                            If J1 = B7%(teamIdx, J) Then 680
                        End If
                    Next J

                    If Not (DH% = 0 And J1 = 0 Or UCase$(J$) = "X") Then

                        'Print position selected
                        Print C$(J1);
                        B7%(teamIdx, posIdx) = J1

                        B9%(teamIdx, B7%(teamIdx, posIdx)) = B3%(teamIdx, posIdx)
                        M%(J1) = 1

                        'Highlight the player we selected
                        Locate I1 + 2, 2: Print "*";

                        Color L%(teamIdx, 11), L%(teamIdx, 12)
                        Print B$(teamIdx, I1)
                        Color 15, 0

                        Locate 25, 1
                        For I1 = 0 To 9
                            If DH% <> 0 Or I1 <> 0 Then
                                If DH% <> 1 Or I1 <> 1 Then
                                    Locate 24, (I1 * 3) + 1
                                    If M%(I1) = 0 Then Print C$(I1); "  ";
                                    If M%(I1) = 1 Then Print "   ";

                                    Locate 25, (I1 * 3) + 1
                                    If M%(I1) = 0 Then Print I1; "  ";
                                    If M%(I1) = 1 Then Print "   ";
                                End If
                            End If
                        Next I1

                    End If

                End If 'Done checking for "P"

            Else

                GoTo 560 'skipLU% = 1

            End If 'Done checking skip

        End If 'Done checking for "X"

    End If 'Done checking for ESC

End Sub

'------------------------------
'    ChangeLineup_PreGame Subroutine
'------------------------------
' ...explanation...
Sub ChangeLineup_PreGame (P9, reselect)

    850 '
    Do

        C1 = 1
        Locate 25, 1
        Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
        Color 15, 0

        If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then
            I$ = "0"
        Else
            I$ = GetKeyPress$
        End If

        I = Val(I$)

        Select Case I

            Case 0:
                '0 can be us entering "0"
                'or it is also what gets assigned
                'if you use VAL against an alpha char
                Select Case UCase$(I$)
                    Case "0"
                        'Here we still could have selected R
                        If Inotloop% = 0 Then Call SaveLineup(P9)

                        '1200
                        For I = 1 To 9
                            If B7%(P9, I) <> 1 Then
                                B%(P9, B3%(P9, I), 21) = 1
                            End If

                            M%(I) = 0
                        Next I

                        M%(0) = 0

                        C1 = 0

                    Case "R", Chr$(27):
                        For J = 1 To 9
                            M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1
                        Next

                        C1 = 0

                        For J = 0 To 22: B%(P9, J, 21) = 0: Next

                        RP = 1

                        '1001
                        For J = 0 To 22
                            If B%(P9, J, 31) <> 0 Then B%(P9, J, 21) = 99
                        Next

                        reselect = 1

                    Case "L":
                        Call LINSWAP(P9)

                    Case "P":
                        Call POSSWAP(P9)

                End Select

            Case 1 To 9:
                Locate B3%(P9, I) + 2 - B4%, 2
                Print " "; B$(P9, B3%(P9, I))

                'GoTo 560
                Call LineupPositions(P9, I, skipLU%)

        End Select

    Loop Until UCase$(I$) = "0" Or UCase$(I$) = "R" Or I$ = Chr$(27)

End Sub

'------------------------------
'     SaveLineup Subroutine
'------------------------------
' ...explanation...
Sub SaveLineup (idx%)

    Shared MF%(), T1%()
    Shared Q2$()

    If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = idx%) Then
        I$ = "N"
    Else
        Do
            Locate 25, 1: Print Space$(74);
            Locate 25, 1: Print "SAVE THIS LINE UP (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Then

        Cls

        If P%(1 - idx%, P1%(1 - idx%), 0) = -1 Then Q2% = 1
        If P%(1 - idx%, P1%(1 - idx%), 0) = 0 Then Q2% = 0

        For I = 0 To 9
            Print I; " ";
            If MG%(idx%, 120 + (I + Q2% * 10) * 21) <> 999 Then Print "AVAILABLE"
            If MG%(idx%, 120 + (I + Q2% * 10) * 21) = 999 Then Print "LINEUP ALREADY SAVED "; MG%(idx%, 100 + (I + Q2% * 10) * 21); " % USAGE"
        Next

        Input "LINEUP # TO SAVE "; LN%
        Input "% OF TIME TO USE "; LU%

        If MG%(idx%, 120 + (LN% + Q2% * 10) * 21) <> 999 Then MG%(idx%, 120 + (LN% + Q2% * 10) * 21) = 999
        If Q2% = 1 Then LN% = LN% + 10

        MG%(idx%, 101 + LN% * 21) = DH%
        MG%(idx%, 100 + LN% * 21) = LU%

        For I = 1 To 9
            MG%(idx%, 110 + I + LN% * 21) = B3%(idx%, I)
            MG%(idx%, 101 + I + LN% * 21) = B7%(idx%, I)
        Next

        'idx% = P9
        MG%(idx%, 99) = 999
        MG%(idx%, 0) = 999
        MF%(idx%) = 0

        Open diskPaths$(3) + "MGR." + YN$(idx%) For Random As #2 Len = 2000

        For I = 0 To 999
            Field #2, I * 2 As Q$, 2 As Q2$(I), 2000 - I * 2 - 2 As Q$
        Next

        For I = 0 To 999: LSet Q2$(I) = MKI$(MG%(idx%, I)): Next

        Put #2, T1%(idx%)

        Close 2

    End If

End Sub

'------------------------------
'  SearchForLineup Subroutine
'------------------------------
' ...explanation...
Sub SearchForLineup (P9)

    Shared PT$()

    '2500
    Cls
    If P%(1 - P9, P1%(1 - P9), 0) = -1 Then Q2% = 1
    If P%(1 - P9, P1%(1 - P9), 0) = 1 Then Q2% = 0
    'P%(0-1,0-1,0); all indexes are 1's

    Print Tab(30); "LINEUPS FOUND VS. "; PT$(Q2%)
    Print " #0    "; MG%(P9, 100 + (0 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #1    "; MG%(P9, 100 + (1 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #2    "; MG%(P9, 100 + (2 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #3    "; MG%(P9, 100 + (3 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #4    "; MG%(P9, 100 + (4 + Q2% * 10) * 21); "%"

    For I = 0 To 4
        For I1 = 1 To 9

            If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 3, 16 * I + 1: Print "NONE FOUND"
            Else

                If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 2 + I1, 16 * I + 1: Print "         "
                Else
                    Locate 2 + I1, 16 * I + 1

                    If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 2 + I1, 16 * I + 12: Print C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If
        Next I1
    Next I

    Print
    Print " #5    "; MG%(P9, 100 + (5 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #6    "; MG%(P9, 100 + (6 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #7    "; MG%(P9, 100 + (7 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #8    "; MG%(P9, 100 + (8 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #9    "; MG%(P9, 100 + (9 + Q2% * 10) * 21); "%"

    For I = 5 To 9

        For I1 = 1 To 9

            If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 14, 16 * (I - 5) + 1: Print "NONE FOUND"
            Else
                If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 13 + I1, 16 * (I - 5) + 1: Print "         "
                Else
                    Locate 13 + I1, 16 * (I - 5) + 1

                    If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 13 + I1, 16 * (I - 5) + 12: Print C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If

        Next

    Next

End Sub

'------------------------------
' LineupFromProfile Subroutine
'------------------------------
' ...explanation...
Sub LineupFromProfile (lineupFound%, P9, skipLineChange%)
    '1830

    lineupFound% = 0
    PCT% = 0

    For I = 0 To 9
        If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
            lineupFound% = lineupFound% + 1
            PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
        End If
    Next

    If lineupFound% = 0 Then
        Cls
        Print "ERROR:: NO LINEUPS FOUND...BE SURE CORRECT LINEUP ID WAS CHOSEN"

        If AP% = 1 Then
            NLF% = 1
        Else
            'Locate 24, 1
            Locate 22, 1
            Print "PRESS ANY KEY TO CONTINUE"
            I$ = GetKeyPress$
        End If
        skipLineChange% = 1
        'Goto 480
    Else

        If AP% = 1 And MP% = 0 And (U6 = 2 Or U6 = 1 And U9 = P9) Or I$ = "3" Then
            RN = Int(Rnd(1) * PCT%) + 1
            PCT% = 0
            For I = 0 To 9
                If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
                    PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
                    If RN <= PCT% Then LN% = I: Exit For
                End If
            Next

        Else
            Do
                Locate 23, 1: Input "ENTER LINEUP NUMBER (0-9) "; LN%
            Loop Until LN% >= 0 And LN% <= 9

        End If

        If MG%(P9, 120 + (LN% + Q2% * 10) * 21) = 999 Then

            If Q2% = 1 Then LN% = LN% + 10

            I1 = MG%(P9, 101 + LN% * 21)

            If DH% = I1 Then

                For I = 1 To 9
                    B3%(P9, I) = MG%(P9, 110 + I + LN% * 21)
                    B7%(P9, I) = MG%(P9, 101 + I + LN% * 21)
                Next
                LU% = 1
                Call DHLineups(P9, skipLineChange%, LU%)

            Else
                'Technically we found a lineup
                'But we will skip prompting for
                'any lineup changes
                skipLineChange% = 1
            End If 'Done checking DH%
        Else
            'skip lineup changes
            skipLineChange% = 1
        End If 'Done checking for 999

    End If 'Done searching for lineup

End Sub

'------------------------------
'   DHLineups Subroutine
'------------------------------
' ...explanation...
Sub DHLineups (P9, reselect, LU%)

    1910 '
    For I = 1 To 9:

        B9%(P9, B7%(P9, I)) = B3%(P9, I)

        If B7%(P9, I) = 1 Then
            B3%(P9, I) = P1%(P9)
            B9%(P9, 1) = P1%(P9)
        Else
            B%(P9, B3%(P9, I), 21) = 0
        End If

    Next I

    If LU% = 1 Then LU% = 0: Call SelectBatters(batterFlag%, P9)

    For I = 10 To 22
        Locate I, 59: Print Space$(21);
    Next

    'Cls

    Call NEWLINES(P9)

    For I2 = 0 To 22:
        Locate I2 + 2, 2
        For I3 = 1 To 9
            If B3%(P9, I3) = I2 And B7%(P9, I3) <> 1 Then
                Print "*";: Color L%(P9, 11), L%(P9, 12)
                Print B$(P9, I2): Color 15, 0
            End If
        Next I3
    Next I2

    For I3 = 0 To 9: M%(I3) = 1: Next I3

    Call ChangeLineup_PreGame(P9, reselect)

    'End If

End Sub

'------------------------------
' SelectBatters Subroutine
'------------------------------
' ...explanation...
Sub SelectBatters (batterFlag%, P9)

    Inotloop% = 99
    Call SHOWBATTERS(P9)
    Color 15, 0
    Locate 5, 59: Print "OPPOSING PITCHER"
    Locate 6, 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

    'TG = 0

    Do
        'Select batters first
        Locate 10, 59: Print "DISABLE BATTER (0-M)"
        Locate , 59: Print "Z-CONTINUE"
        Locate , 59: Print "X-REACTIVATE ALL"
        Locate , 59: Print "<TAB>-BAT/FIELD/SPLIT"

        If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then
            'Autoplay = auto-select
            I$ = "Z"
            batterFlag% = 1
        Else
            I$ = GetKeyPress$

            'Handle ESC
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

                batterFlag% = 0

                Exit Sub

            End If

        End If

        ' Check if user pressed Tab
        If Asc(I$) = 9 Then TG = TG + 1

        If TG > 2 Or TG = 2 And TS%(P9, 0) <> 999 Then TG = 0

        If Asc(I$) = 9 Then
            '   Update display after TAB
            Select Case TG
                Case 0:
                    Call SHOWBATTERS(P9)
                Case 1:
                    Call SHOWFIELD(P9)
                Case 2:
                    Call ShowSplit(P9)
            End Select

        Else
            'Not TAB / ESC
            'Deteremine index of batter chosen
            Call LetterToNumber(I$)

            'Assign field position
            Select Case UCase$(I$)
                Case "Z": TG = 0: Call SHOWBATTERS(P9)

                Case "X":
                    For I3 = 0 To 22
                        If B%(P9, I3, 21) = 99 And B%(P9, I3, 31) = 0 Then
                            B%(P9, I3, 21) = 0: Locate I3 + 2, 3: Print B$(P9, I3)
                        End If
                    Next I3

                Case Else:
                    I1 = Val(I$)
                    If I1 >= 0 And I1 <= 22 Then
                        B%(P9, I1, 21) = 99
                        Locate I1 + 2, 3: Color 0, 7: Print B$(P9, I1);
                        Color 15, 0
                    End If
            End Select

        End If 'Done checking for TAB

        'If it wasn't an ESC, let's print the batter
        If I$ <> Chr$(27) Then
            Color 15, 0: Locate 5, 59: Print "OPPOSING PITCHER"
            Locate 6, 59: Print P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
            Inotloop% = 0

            '--- this is getting assigned always
            '--- so what does this represent then???
            '--- I created it!!!
            batterFlag% = 1
        End If 'Done checking for ESC

    Loop Until UCase$(I$) = "Z"

End Sub

'------------------------------
'   SelectStadium Subroutine
'------------------------------
' ...explanation...
Sub SelectStadium ()

    Do
        'SELECT A DIFFERENT STADIUM
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

        'Determine park from the team data
        PARK$ = Right$(Str$(L%(1, 13)), Len(Str$(L%(1, 13))) - 1)

        If DN% = 0 Then
            '0 = daytime
            PARK$ = PARK$ + "D.PNG"
        Else
            '1 = nighttime
            PARK$ = PARK$ + "N.PNG"
        End If

        'Display detected park
        Locate , 11: Print "CURRENT PARK IS:"
        Locate , 11: Print PARK$(L%(1, 13))

        Locate , 11
        '99 is the Generic Park
        If _FileExists(".\parks\" + PARK$) Then
            Print "GRAPHIC EXISTS"
        Else
            Print "GRAPHIC NOT AVAILABLE"
            PARK$ = "99N.PNG"
        End If

        If AP% <> 1 Then
            'No autoplay
            Do
                Locate 10, 11: Print "SELECT A DIFFERENT STADIUM (YN)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

            If UCase$(I$) = "Y" Then

                Cls
                Locate 23, 10: Print "* - STADIUM GRAPHIC FILE NOT AVAILABLE"
                Start% = 1

                Do
                    Selection% = 1
                    Count% = 15
                    ExitCode% = 99
                    FileFGColr% = 15: FileBGColr% = 1
                    HiLiteFGColr% = 0: HiLiteBGColr% = 7
                    Spacing% = 1
                    Row% = 5
                    Column% = 10

                    Call box0(4, 9, 20, 38, 2, 15, 1)
                    _MouseShow "Default"
                    'MMenuVert PARK$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
                    MAMenu PARK$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, 15, 1, 1, Row%, Column%
                    _MouseHide

                    Color , 0

                    If ExitCode% <> 27 Then

                        PARK$ = Right$(Str$(Selection%), Len(Str$(Selection%)) - 1)
                        If DN% = 0 Then PARK$ = PARK$ + "D.PNG" Else PARK$ = PARK$ + "N.PNG"
                        Color 15, 0

                        If _FileExists(".\parks\" + PARK$) Then
                            foundPark% = 1
                            L%(1, 13) = Selection%
                            S$(1) = PARK$(Selection%)
                        Else
                            Do
                                Locate 10, 8
                                Print "STADIUM GRAPHIC NOT AVAILABLE"
                                Locate , 8: Print "USE SELECTED PARK RATINGS WITH"
                                Locate , 8: Print "GENERIC DISPLAY (YN)"
                                J$ = GetKeyPress$
                            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                            If I$ = "Y" Then
                                L%(1, 13) = Selection%: S$(1) = PARK$(Selection%)
                                If DN% = 0 Then PARK$ = "99D.PNG" Else PARK$ = "99N.PNG"
                            End If

                        End If

                    Else
                        BO% = 1
                    End If

                Loop Until UCase$(J$) = "Y" Or ExitCode% = 27 Or foundPark% = 1

            End If

        End If 'Done checking auto play

    Loop Until B0% = 0 Or UCase$(I$) = "N" Or ExitCode% = 27

End Sub

'------------------------------
' PitchingRotations Subroutine
'------------------------------
' ...explanation...
Sub PitchingRotations (computerRotations%, P9)

    Dim gamesStarted%(21, 1)

    '4610
    Cls
    Call INFO

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
        If P$(P9, I) <> "XXX" Then
            Print P$(P9, I); Tab(18); P%(P9, I, 5);
        End If

        Locate , 25
        If P$(P9, I + 7) <> "XXX" Then
            Print P$(P9, I + 7); Tab(43); P%(P9, I + 7, 5);
        End If

        Locate , 50
        If P$(P9, I + 14) <> "XXX" Then
            Print P$(P9, I + 14); Tab(68); P%(P9, I + 14, 5)
        End If

    Next

    If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = P9) Then
        '   4685
        gamesStarted = 0

        For I = 0 To 21
            If P%(P9, I, 5) > gamesStarted Then gamesStarted = P%(P9, I, 5)
        Next

        'Auto-determine # of men in rotation
        'based on # of games started?
        I = 1
        Select Case gamesStarted
            Case 1 To 32: I = 5
            Case 33 To 41: I = 4
            Case 42 To 54: I = 3
            Case 55 To 75: I = 2
        End Select

    Else
        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 5
    End If

    '4615
    MG%(P9, 1) = I
    MG%(P9, 2) = 0

    For I = 3 To 36
        MG%(P9, I) = -1
    Next

    For I = 41 To 56
        MG%(P9, I) = 0
    Next

    MG%(P9, 0) = 999

    Inotloop% = 99

    Call PrintPitcherInfo(P9)

    If computerRotations% = 0 Then

        For I = 0 To MG%(P9, 1) - 1

            Do

                'We need to skip this in certain circumstances
                'Right now, it is being loaded for auto-play
                Locate 15, 59: Print "STARTER "; I + 1

                Do
                    I$ = GetKeyPress$
                    Call LetterToNumber(I$)

                    I1 = Val(I$)

                Loop Until I1 <= 21 And MG%(P9, 3) <> I1 And MG%(P9, 4) <> I1 And MG%(P9, 5) <> I1 And MG%(P9, 6) <> I1 And MG%(P9, 7) <> I1

                Locate , 59: Print P$(P9, I1)

                Do
                    Locate , 59: Print "ANY CHANGE (YN)";
                    J$ = GetKeyPress$
                Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                If UCase$(J$) = "Y" Then
                    For I2 = 15 To 17: Locate I2, 59: Print Space$(17): Next
                End If

            Loop Until UCase$(J$) = "N"

            MG%(P9, I + 3) = I1

            For I2 = 15 To 17: Locate I2, 59: Print Space$(18): Next

        Next

    Else

        ' COMPUTER SET ROTATION
        'Make sure our next team isn't auto-set
        'without a choice
        computerRotations% = 0

        For I = 0 To 21
            If P$(P9, I) = "XXX" Then
                gamesStarted%(I, 0) = 0
                gamesStarted%(I, 1) = -1
            Else
                gamesStarted%(I, 0) = P%(P9, I, 5): gamesStarted%(I, 1) = I
            End If
        Next

        For Z = 0 To 20:
            For Z1 = Z + 1 To 21:
                If gamesStarted%(Z, 0) < gamesStarted%(Z1, 0) Then
                    Swap gamesStarted%(Z, 0), gamesStarted%(Z1, 0)
                    Swap gamesStarted%(Z, 1), gamesStarted%(Z1, 1)
                End If
            Next Z1
        Next Z

        For I = 0 To MG%(P9, 1) - 1
            MG%(P9, I + 3) = gamesStarted%(I, 1)
        Next

        I1 = 0

        For I = MG%(P9, 1) To 21
            I1 = I1 + 1

            If gamesStarted%(I, 0) <> 0 Then
                MG%(P9, I1 + 20) = gamesStarted%(I, 1)
            Else
                Exit For
            End If
        Next

    End If

    I1 = I1 - 1

    If MG%(P9, 1) = 1 Then
        MG%(P9, 13) = 100
    Else
        If MG%(P9, 1) = 2 Then gamesStarted% = 75
        If MG%(P9, 1) = 3 Then gamesStarted% = 54
        If MG%(P9, 1) = 4 Then gamesStarted% = 41
        If MG%(P9, 1) = 5 Then gamesStarted% = 32

        For I = 1 To MG%(P9, 1)
            MG%(P9, I + 12) = Int(P%(P9, MG%(P9, I + 2), 5) * 100 / gamesStarted% + .5)
        Next

        SS% = 0

        For I = 21 To 36
            If MG%(P9, I) >= 0 And MG%(P9, I) <= 21 Then
                SS% = SS% + P%(P9, MG%(P9, I), 5)
            End If
        Next

        For I = 21 To 36
            If MG%(P9, I) >= 0 And MG%(P9, I) <= 21 Then
                MG%(P9, I + 20) = Int(P%(P9, MG%(P9, I), 5) * 100 / SS% + .5)
            End If
        Next

    End If

End Sub

'------------------------------
'  ComputerLineups Subroutine
'------------------------------
' ...explanation...
Sub ComputerLineups (batterFlag%, P9, noLineups, reselect)

    Shared BP(), F()
    Shared BB%()
    Dim PS%(23, 1)

    5000 ':*** COMPUTER DETERMINE LINEUPS ***

    For I = 0 To 22
        If B$(P9, I) = "XXX" Then B%(P9, I, 21) = 98
    Next

    Call SelectBatters(batterFlag%, P9)

    If batterFlag% <> 0 Then

        For I = 10 To 22
            Locate I, 59: Print Space$(21);
        Next

        For I = 1 To 9:
            F(I) = 0
            BP(I) = 0
            BB%(I) = -1
        Next

        M = 0

        For I = 9 To 1 Step -1

            J = I

            For I2 = 0 To 22: PS%(I2, 0) = 0: PS%(I2, 1) = -1: Next

            I2 = 0

            If I = 1 Then
                '5120
                If DH% = 0 Then
                    BP(9) = 1
                    Exit For
                Else
                    J = J - 1
                    I2 = 0
                    For I1 = 0 To 22
                        If B%(P9, I1, 21) <= 0 And B$(P9, I1) <> "XXX" Then
                            If B%(P9, I1, 22) = 0 Then
                                PS%(I2, 0) = B%(P9, I1, 4): PS%(I2, 1) = I1
                            Else
                                If B%(P9, I1, 23) = 0 Then
                                    PS%(I2, 0) = B%(P9, I1, 4) * .5: PS%(I2, 1) = I1
                                Else
                                    If B%(P9, I1, 24) = 0 Then
                                        PS%(I2, 0) = B%(P9, I1, 4) * .3: PS%(I2, 1) = I1
                                    Else
                                        If B%(P9, I1, 25) = 0 Then
                                            PS%(I2, 0) = B%(P9, I1, 4) * .2: PS%(I2, 1) = I1
                                        Else
                                            PS%(I2, 0) = Int(B%(P9, I1, 4) * .05 + .5): PS%(I2, 1) = I1
                                        End If
                                    End If
                                End If
                            End If

                            I2 = I2 + 1

                        End If

                    Next I1

                End If
            Else
                'I <> 1
                For I1 = 0 To 22:
                    If B%(P9, I1, 21) <= 0 And B$(P9, I1) <> "XXX" Then
                        If B%(P9, I1, 22) = I And B%(P9, I1, 23) = -1 Then
                            PS%(I2, 0) = B%(P9, I1, 4): PS%(I2, 1) = I1: I2 = I2 + 1
                        Else
                            If B%(P9, I1, 22) = I And B%(P9, I1, 24) = -1 Then
                                PS%(I2, 0) = Int(B%(P9, I1, 4) * .9 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                            Else
                                If B%(P9, I1, 22) = I And B%(P9, I1, 25) = -1 Then
                                    PS%(I2, 0) = Int(B%(P9, I1, 4) * .8 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                Else
                                    If B%(P9, I1, 22) = I Then
                                        PS%(I2, 0) = Int(B%(P9, I1, 4) * .7 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                    Else
                                        If B%(P9, I1, 23) = I And B%(P9, I1, 24) = -1 Then
                                            PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                        Else
                                            If B%(P9, I1, 23) = I And B%(P9, I1, 25) = -1 Then
                                                PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                            Else
                                                If B%(P9, I1, 23) = I Then
                                                    PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                                Else
                                                    If B%(P9, I1, 24) = I And B%(P9, I1, 25) = -1 Then
                                                        PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                                    Else
                                                        If B%(P9, I1, 24) = I Then
                                                            PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                                        Else
                                                            If B%(P9, I1, 25) = I Then
                                                                PS%(I2, 0) = Int(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1
                                                            End If
                                                        End If
                                                    End If
                                                End If
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    End If

                Next I1

            End If 'Done checking value of I

            I2 = I2 - 1
            AB% = 0

            For I1 = 0 To I2
                AB% = AB% + PS%(I1, 0)
            Next

            'I2 = 4
            'AB% = 630
            RN = Int(Rnd(1) * AB%) + 1

            For I1 = 0 To I2
                If RN <= PS%(I1, 0) Then
                    F(J) = PS%(I1, 1)
                    B%(P9, PS%(I1, 1), 21) = 1
                    lineupDone% = 1
                Else
                    PS%(I1 + 1, 0) = PS%(I1 + 1, 0) + PS%(I1, 0)
                End If
            Next I1

            If lineupDone% <> 1 Then
                Locate 25, 1: Print "CANNOT MAKE A LINEUP!!...";
                noLineups = 1
                'reselect = 1

                For J = 1 To 9: M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1: Next

                C1 = 0

                For J = 0 To 22
                    If B%(P9, J, 21) <> 99 Then B%(P9, J, 21) = 0
                Next

                If AP% <> 1 Then
                    I$ = GetKeyPress$
                    Exit For
                End If

            End If 'done checking lineup

        Next I

        Call SortHitters(P9)
        Call DHLineups(P9, reselect, LU%)

    End If 'done checking batterFlag

End Sub

'------------------------------
'   SortHitters Subroutine
'------------------------------
' ...explanation...
Sub SortHitters (P9)

    Shared BP()
    Shared BB%()

    C = 2

    B = 1
    Call SomeCalculations(B, C, P9)
    B = 2
    Call SomeCalculations(B, C, P9)

    C = 1

    B = 3
    Call SomeCalculations(B, C, P9)
    B = 4
    Call SomeCalculations(B, C, P9)

    C = 1

    B = 5
    Call SomeCalculations(B, C, P9)
    B = 6
    Call SomeCalculations(B, C, P9)

    B = 7
    Call SomeCalculations(B, C, P9)
    B = 8
    Call SomeCalculations(B, C, P9)

    If DH% = 1 Then B = 9: Call SomeCalculations(B, C, P9)

    For I = 1 To 9
        B3%(P9, I) = BB%(I)
        B7%(P9, I) = BP(I)
    Next

End Sub

'------------------------------
' SomeCalculations Subroutine
'------------------------------
' I literally have no clue
' what this routine does.
' It was used so often but
' does not have any context
' hinting to it's purpose.
Sub SomeCalculations (B, C, P9)

    Shared BP(), F()
    Shared E!
    Shared BB%()

    Dim JJ!, R!, SE!, Q1!, QQ!

    E! = -100
    U = -1
    T = -1

    For L = 0 To 9

        If DH% = 0 And L = 0 Then L = L + 2
        If DH% = 1 And L = 1 Then L = L + 1
        J = F(L)
        X = 1

        For Z = 1 To 9
            If BB%(Z) = J Then X = 0: Z = 9
        Next Z

        If X <> 0 Then

            If C <> 1 Then
                SE! = (B%(P9, J, 6) + B%(P9, J, 7) + B%(P9, J, 8) * 2 + B%(P9, J, 9) * 3) / B%(P9, J, 4)
                If SE! >= E! Then E! = SE!: T = J: U = L
            Else
                JJ = J

                'IF A PITCHER THEN 268
                Q1! = (B%(P9, JJ, 6) - B%(P9, JJ, 8) - B%(P9, JJ, 9) + B%(P9, JJ, 11)) * .7

                If Q1! = 0 Then Q1! = 1

                Q1! = B%(P9, JJ, 13) / Q1!

                If Q1! >= .02 Then
                    QQ! = Q1! + .18
                    R! = Int(QQ! * (18 + 2 * Abs((QQ! < .34))))
                Else
                    R! = 2 + Int(Q1! * 100)
                    If B%(P9, JJ, 1) = 2 Then R = R - 1
                End If

                If (B%(P9, JJ, 1) = 6 Or B%(P9, JJ, 1) = 8) And R! < 7 Then R! = R! + 1

                If R! > 9 Then R! = 9
                If R! < 0 Then R! = 1

                T1% = 100 * B%(P9, J, 6) / B%(P9, J, 4) - 27
                If T1% < 0 Then T1% = 3 * T1%

                JJ! = 3 * R! + T1%
                If JJ! > E! Then E! = JJ!: T = J: U = L

            End If 'Done checking C

        End If 'Done checking X

    Next L

    BB%(B) = T
    BP(B) = U

End Sub

'------------------------------
' SelectTeam_LOADER Subroutine
'------------------------------
' ...explanation...
Sub SelectTeam_LOADER (NTMS, YN$, idx%)

    Shared T1%(), UV%()

    Selection% = 1
    Count% = NTMS
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
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then
        T1%(idx%) = UV%(Selection%)
        Locate 2, 33: Color 31: Print "LOADING..."
        Call LoadTeam_LOADER(T1%(idx%), YN$, idx%)
    Else
        BO% = 1
    End If

End Sub

'------------------------------
'   LoadTeam_LOADER Subroutine
'------------------------------
' ...explanation...
Sub LoadTeam_LOADER (teamIdx%, YN$, idx%)

    Dim DYS%(1, 21)
    Shared MF%()
    Shared Q2$()

    Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342

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
    Get #1, teamIdx%

    A$(idx%) = Q2$(0)
    A$(idx%) = RTrim$(A$(idx%))

    For I1 = 1 To 13
        L%(idx%, I1) = CVI(Q2$(I1))
    Next

    If (L%(idx%, 11) = 7 Or L%(idx%, 11) = 15) And L%(idx%, 12) = 0 Then L%(idx%, 11) = 0: L%(idx%, 12) = 7
    K9!(idx%) = CVS(Q2$(14))

    For I1 = 0 To 22:
        B$(idx%, I1) = Q2$(I1 * 43 + 15)
        B$(idx%, I1) = RTrim$(B$(idx%, I1))

        For I2 = 0 To 41
            B%(idx%, I1, I2) = CVI(Q2$(I1 * 43 + I2 + 16))
        Next

        If B%(idx%, I1, 31) <> 0 Then B%(idx%, I1, 21) = 99
    Next

    For I1 = 0 To 21: P$(idx%, I1) = Q2$(I1 * 38 + 1004)
        P$(idx%, I1) = RTrim$(P$(idx%, I1))

        For I2 = 0 To 36
            P%(idx%, I1, I2) = CVI(Q2$(I1 * 38 + I2 + 1005))
        Next

        If P%(idx%, I1, 34) <> 0 Then P%(idx%, I1, 15) = 99
        If P%(idx%, I1, 0) = 0 Then P%(idx%, I1, 0) = 1
    Next

    S$(idx%) = Q2$(1840)
    M$(idx%) = Q2$(1841)
    NN$(idx%) = Q2$(1842)
    CK = CVI(Q2$(1843))

    Close 1

    For I1 = 0 To 22
        ':check for 1934 season
        If Val(Left$(A$(idx%), 2)) < 54 And Mid$(A$(idx%), 3, 1) = " " Then B%(idx%, I1, 30) = B%(idx%, I1, 30) * .75

        For I2 = 22 To 25
            If B%(idx%, I1, I2) <> -1 And B%(idx%, I1, I2) <> 0 And B%(idx%, I1, I2 + 4) = 0 Then B%(idx%, I1, I2 + 4) = 900
        Next
    Next

    M$(idx%) = RTrim$(M$(idx%))
    S$(idx%) = RTrim$(S$(idx%))
    NN$(idx%) = RTrim$(NN$(idx%))

    For X = 0 To 22
        For X1 = 1 To 14
            B%(idx%, X, X1 + 41) = 0
        Next
    Next

    For X = 0 To 21
        For X1 = 1 To 28
            P%(idx%, X, X1 + 36) = 0
        Next
    Next

    If LR% = 1 Then
        TS%(idx%, 0) = -1
    Else

        If _FileExists(diskPaths$(0) + "LR." + YN$) Then

            Open diskPaths$(0) + "LR." + YN$ For Random As #1 Len = 4108

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

            Get #1, teamIdx%

            For X = 0 To 11
                TS%(idx%, X) = CVI(Q2$(X + 1))
            Next

            For X = 0 To 22
                For X1 = 1 To 14
                    B%(idx%, X, X1 + 41) = CVI(Q2$(12 + X1 + X * 14))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    P%(idx%, X, X1 + 36) = CVI(Q2$(334 + X1 + X * 14))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    P%(idx%, X, X1 + 50) = CVI(Q2$(642 + X1 + X * 14))
                Next
            Next

            For X = 1 To 24: SA%(idx%, X) = CVI(Q2$(X + 950)): Next
            For X = 0 To 22
                For X1 = 1 To 24
                    B%(idx%, X, X1 + 55) = CVI(Q2$(975 + X1 + X * 24))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    P%(idx%, X, X1 + 64) = CVI(Q2$(1528 + X1 + X * 24))
                Next
            Next

            Close 1

            If TS%(idx%, 0) <> 999 Then TS%(idx%, 0) = -1
        Else
            TS%(idx%, 0) = -1
        End If

    End If

    For X = 0 To 22
        For X1 = 0 To 14
            B%(idx%, X, 80) = B%(idx%, X, 80) + B%(idx%, X, X1 + 41)
        Next
    Next

    For X = 0 To 21
        For X1 = 0 To 14
            P%(idx%, X, 89) = P%(idx%, X, 89) + P%(idx%, X, X1 + 50)
        Next
    Next

    'Read in manager data
    If _FileExists(diskPaths$(3) + "MGR." + YN$) Then

        Open diskPaths$(3) + "MGR." + YN$ For Random As #2 Len = 2000

        For X = 0 To 999
            Field #2, X * 2 As Q2$, 2 As Q2$(X), 2000 - X * 2 - 2 As Q2$
        Next

        Get #2, teamIdx%

        For X = 0 To 999: MG%(idx%, X) = CVI(Q2$(X)): Next

        Close 2
    Else
        MF%(idx%) = -1
    End If

    If MG%(idx%, 0) <> 999 Or MF%(idx%) = -1 Then
        MF%(idx%) = -1
        For X = 0 To 999: MG%(idx%, X) = -1: Next
    End If

    N$(idx%) = A$(idx%)

    For xx = 4 To 11
        If Mid$(N$(idx%), xx, 1) = " " Or Mid$(N$(idx%), xx, 1) = "." Then Mid$(N$(idx%), xx, 1) = "_"
    Next xx
    QQ = 8
    For xx = 11 To 4 Step -1
        If Mid$(N$(idx%), xx, 1) = "_" Then
            QQ = xx - 4:
        Else
            Exit For
        End If
    Next xx

    statFile$ = Mid$(N$(idx%), 4, QQ)
    statFile$ = statFile$ + "." + Left$(N$(idx%), 2)

    'Read in supplemental data
    If _FileExists(diskPaths$(1) + statFile$ + "D") Then

        Open diskPaths$(1) + statFile$ + "D" For Input As #1

        For I1 = 0 To 22
            Input #1, INJ%(idx%, I1)
        Next

        For I1 = 0 To 21
            Input #1, DYS%(idx%, I1)
        Next

        Close 1

        If IJ% <> 1 Then
            For I1 = 0 To 22
                If INJ%(idx%, I1) > 0 Then B%(idx%, I1, 21) = 99
            Next
        End If

    End If

    If DR% <> 1 Then
        For I1 = 0 To 21
            If DYS%(idx%, I1) <> 0 And DYS%(idx%, I1) < 90 Then
                P%(idx%, I1, 35) = DYS%(idx%, I1)
            End If
        Next
    End If

End Sub

'------------------------------
'   SelectPitchers Subroutine
'------------------------------
' ...explanation...
Sub SelectPitchers (idx%, cancelPitchers%, computerRotations%)

    Shared MF%(), SP%(), T1%()
    Shared Q2$()

    4500 '
    ' *** PITCHING ROTATIONS ***
    Inotloop% = 99
    skipDisablePitchers% = 0

    'GoSub 1520
    Call PrintPitcherInfo(idx%)

    Locate 15, 57: Print "0-MANUALLY PICK STARTER"
    Locate 16, 57: Print "1-USE PRE-SET ROTATION"
    Locate 17, 57: Print "2-ENTER A NEW ROTATION"

    If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = idx%) Then
        'For automated games, use preset rotation
        'This will end up being computer-generated
        I = 1
    Else
        Do
            I$ = GetKeyPress$
            If I$ <> Chr$(27) Then
                I = Val(I$)
            End If
        Loop Until I >= 0 And I <= 2 Or I$ = Chr$(27)
    End If

    If I$ <> Chr$(27) Then

        Select Case I
            Case 0:
                '4535 = Return
                'manual pick starter means we are
                'not going to disable anyone, but
                'just simply pick 1 pitcher
                skipDisablePitchers = 1

            Case 1:
                '4512
                'Pre-Set Rotation
                Cls

                If MF%(idx%) = -1 Or MG%(idx%, 99) <> 999 Then

                    '4600
                    If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = idx%) Then
                        computerRotations% = 1
                    Else
                        'This should instead go to automatic disable of rotations???
                        Cls
                        Print "ERROR::: ROTATION NOT FOUND"
                        Print
                        Print "ENTER A ROTATION (YN)"

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
                    End If

                    'Don't we want to skipDisablePitchers
                    'when we are doing computerRotations
                    'as well ????
                    If computerRotations% = 0 Then

                        If UCase$(I$) = "N" Then
                            skipDisablePitchers% = 1
                        Else
                            Print "COMPUTER SET ROTATION (YN) ";

                            Do
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                            If UCase$(I$) = "Y" Then computerRotations% = 1

                        End If

                    End If

                    Call PitchingRotations(computerRotations%, idx%)

                Else
                    skipDisablePitchers% = 0
                End If

            Case 2:
                '4604
                Cls
                Print "COMPUTER SET ROTATION (YN) ";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then computerRotations% = 1

                '4610
                Call PitchingRotations(computerRotations%, idx%)

        End Select

        If skipDisablePitchers% <> 1 Then
            '4516
            'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

            'For I = 0 To MG%(idx%, 1) - 1
            '    P%(idx%, MG%(idx%, 3 + I), 15) = 99
            'Next

            '4517
            Do
                RN = Int(Rnd(1) * 100) + 1

                'Currently having issues here with auto-run
                'But only from GAME82 / schedule file
                'I suspect MG% isn't getting set right
                P%(idx%, MG%(idx%, MG%(idx%, 2) + 3), 15) = 0

                If RN <= MG%(idx%, MG%(idx%, 2) + 10 + 3) Or MG%(idx%, MG%(idx%, 2) + 10 + 3) = 0 Then
                    SP%(idx%) = MG%(idx%, MG%(idx%, 2) + 3)
                    doneAutoDisable% = 1
                Else

                    RN = Int(Rnd(1) * 100) + 1
                    SP% = 0

                    For I = 21 To 36
                        If MG%(idx%, I) <> -1 And MG%(idx%, I) >= 0 And MG%(idx%, I) <= 21 Then
                            SP% = SP% + MG%(idx%, I + 20)
                            If RN <= SP% And MG%(idx%, I) <> -1 Then
                                P%(idx%, MG%(idx%, I), 15) = 0
                                SP%(idx%) = MG%(idx%, I)
                                doneAutoDisable% = 1
                                Exit For
                            End If
                        End If
                    Next

                End If

            Loop Until doneAutoDisable% = 1

            MG%(idx%, 2) = MG%(idx%, 2) + 1

            If MG%(idx%, 2) = MG%(idx%, 1) Then MG%(idx%, 2) = 0

            MG%(idx%, 99) = 999
            MG%(idx%, 0) = 999
            MF%(idx%) = 0

            Open diskPaths$(3) + "MGR." + YN$(idx%) For Random As #2 Len = 2000

            For I = 0 To 999
                Field #2, I * 2 As Q$, 2 As Q2$(I), 2000 - I * 2 - 2 As Q$
            Next

            For I = 0 To 999: LSet Q2$(I) = MKI$(MG%(idx%, I)): Next

            Put #2, T1%(idx%)

            Close 2

        End If

        2040 '
        Do

            Inotloop% = 99

            Call PrintPitcherInfo(idx%)

            Locate 15, 58: Print "DISABLE PITCHER (0-L)"
            Locate , 58: Print "Y-GRANT DAY OFF"
            Locate , 58: Print "Z-CONTINUE"
            Locate , 58: Print "X-ACTIVATE ALL PITCHERS"

            If AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = idx%) Then
                I$ = "Z"
            End If

            While UCase$(I$) <> "Z" And I$ <> Chr$(27)

                I$ = GetKeyPress$

                If I$ <> Chr$(27) And UCase$(I$) <> "Z" Then

                    Call LetterToNumber(I$)

                    Select Case UCase$(I$)
                        Case "Y":
                            ':GRANT DAY OFF
                            If DR% <> 1 Then
                                For I1 = 0 To 21
                                    If P%(idx%, I1, 35) > 0 Then P%(idx%, I1, 35) = P%(idx%, I1, 35) - 1
                                Next
                            End If

                        Case "X":
                            'Activate all pitchers
                            For I3 = 0 To 21
                                If P%(idx%, I3, 15) = 99 And P%(idx%, I3, 34) = 0 Then P%(idx%, I3, 15) = 0: Locate I3 + 2, 3: Print P$(idx%, I3)
                            Next I3

                        Case Else:
                            I1 = Val(I$)
                            If I1 <= 21 Then
                                'If a pitcher was selected,
                                'disable them
                                If P%(idx%, I1, 15) = 0 Then
                                    P%(idx%, I1, 15) = 99: Locate I1 + 2, 3: Color 0, 7: Print P$(idx%, I1): Color 15, 0
                                Else
                                    If P%(idx%, I1, 15) <> 0 Then P%(idx%, I1, 15) = 0: Locate I1 + 2, 3: Color 15, 0: Print P$(idx%, I1): Color 15, 0
                                End If
                            End If

                    End Select

                End If

            Wend

        Loop Until UCase$(I$) = "Z" Or I$ = Chr$(27)

        'cancelPitchers% = 0
    Else
        cancelPitchers% = 1
    End If 'Done checking for ESC from rotation choice

    If I$ <> Chr$(27) Then
        'We pressed Z
        'Reset the value
        Inotloop% = 0

    Else
        'We cancelled
        cancelPitchers% = 1
    End If 'Done checking for ESC

End Sub

'------------------------------
' PrintPitcherInfo Subroutine
'------------------------------
' ...explanation...
Sub PrintPitcherInfo (idx%)

    '1520
    Cls
    Print "#";: Color L%(idx%, 11), L%(idx%, 12): Print A$(idx%);
    Color 14, 0: Locate , 16
    Print "T  IP   H  G  GS DR  BB  SO   W- L   ERA"

    For I = 0 To 21

        If P$(idx%, I) = "XXX" Or Left$(P$(idx%, I), 1) = " " Or P$(idx%, I) = "" Then P%(idx%, I, 15) = 98

        If P%(idx%, I, 15) <> 1 And P$(idx%, I) <> "XXX" Then
            Color 2, 0

            If I <= 9 Then
                Print Using "#"; I;: Color 15, 0
            Else
                'I > 9
                Print X$(I - 10);: Color 15, 0
            End If

            If P%(idx%, I, 15) >= 98 Then Color 0, 7

            Locate , 3: Print P$(idx%, I);
            Color 15, 0

            Locate , 16: Print B1$(P%(idx%, I, 0) + 2);
            Locate , 17: Print Using " ### ### ### ## ## ### ###"; P%(idx%, I, 6); P%(idx%, I, 7); P%(idx%, I, 4); P%(idx%, I, 5); P%(idx%, I, 35); P%(idx%, I, 8); P%(idx%, I, 9);
            Locate , 43: Print Using "  ##"; P%(idx%, I, 1);: Print "-";: Print Using "## ##.##"; P%(idx%, I, 2); P%(idx%, I, 10) / 100
        End If

    Next I

    If Inotloop% <= 0 Then

        Call PitchingStarter(idx%)
        'If Inotloop% <= 5 And S6%(P9, 0) - S6%(1 - P9, 0) > 0 Then P2%(P9) = P1%(P9)

    End If

End Sub





'INCLUDE: 'Game_Routines.BM'


'------------------------------
'   PitchingStarter routine
'------------------------------
' ...explanation...
Sub PitchingStarter (idx%)

    Shared SP%()

    Do

        Locate 19, 58: Print "SELECT STARTER"

        If SP%(idx%) <> -1 Then
            I1 = SP%(idx%)
        Else

            Do
                I$ = GetKeyPress$
                Call LetterToNumber(I$)
                I1 = Val(I$)
            Loop Until I1 <= 21 And P%(idx%, I1, 15) <= 0

        End If

        Locate , 58
        Print P$(idx%, I1); " "; B1$(P%(idx%, I1, 0) + 2)

        Print

        If Not (AP% = 1 And (U6 = 2 Or U6 = 1 And U9 = idx%)) Then

            Do
                Locate , 58: Print "ANY CHANGE (YN)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                Locate 20, 58: SP%(idx%) = -1: Print Space$(16)
            End If

        Else

            I$ = "N"

        End If

    Loop Until UCase$(I$) = "N"

    P1%(idx%) = I1
    P6%(idx%) = 1
    P%(idx%, I1, 15) = P6%(idx%)

    S8%(idx%, 0) = Int(((P%(idx%, P1%(idx%), 8) + P%(idx%, P1%(idx%), 7)) / P%(idx%, P1%(idx%), 4)) + .5)
    S8%(idx%, 1) = 3

    If P6%(idx%) > 1 Then S8%(idx%, 1) = 0

End Sub

'------------------------------
'      ADDPIT Subroutine
'------------------------------
' ...explanation...
Sub ADDPIT (D, I5, I6)

    P8%(D, I5, I6) = P8%(D, I5, I6) + 1

    P5%(D, I5, I6) = P5%(D, I5, I6) + 1

End Sub

'------------------------------
'      ADDSTTS Subroutine
'------------------------------
' ...explanation...
Sub ADDSTTS (P, D)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 0
    Call ADDPIT(D, I5, I6)

End Sub

'------------------------------
'    ADJRUNNERS Subroutine
'------------------------------
' ...explanation...
Sub ADJRUNNERS (S2%, P)

    For idx = 1 To 3
        If S2% = 13 And A5%(idx) = 11 And B1%(idx, 0) = 0 Then A5%(0) = 11
    Next idx

    If Not (B1%(3, 0) = 0 Or A5%(3) = 0) Then
        idx = 3
        If A5%(3) = 10 Then I3 = 3: Call CLEARBASE(idx)
        If A5%(3) >= 1 And A5%(3) <= 4 Then Call CHARGERUN(D, idx)
        If A5%(3) = 11 Then I3 = 4: Call CLEARBASE(idx)
    End If

    If Not (B1%(2, 0) = 0 Or A5%(2) = 0) Then
        idx = 2
        If A5%(2) = 10 Then I3 = 2: Call CLEARBASE(idx)
        If A5%(2) = 11 Then I3 = 3: Call CLEARBASE(idx)
        If A5%(2) = 12 And B1%(3, 0) = 0 Then I3 = 4: Call CLEARBASE(idx)
        If A5%(2) = 1 And B1%(3, 0) = 0 Then Call BATTER1(idx)
        If A5%(2) >= 2 And A5%(2) <= 4 And B1%(3, 0) = 0 Then Call CHARGERUN(D, idx)
    End If

    If Not (B1%(1, 0) = 0 Or A5%(1) = 0) Then
        idx = 1
        If A5%(1) = 8 And B1%(2, 0) = 0 Then Call BATTER1(idx)
        If A5%(1) = 10 Then I3 = 1: Call CLEARBASE(idx)
        If A5%(1) = 11 Then I3 = 2: Call CLEARBASE(idx)
        If A5%(1) = 12 And B1%(3, 0) = 0 And B1%(2, 0) = 0 Then I3 = 3: Call CLEARBASE(idx)
        If A5%(1) = 13 And B1%(3, 0) = 0 And B1%(2, 0) = 0 Then I3 = 4: Call CLEARBASE(idx)
        If A5%(1) = 1 And B1%(2, 0) = 0 Then Call BATTER1(idx)
        If A5%(1) = 2 And B1%(2, 0) = 0 And B1%(3, 0) = 0 Then Call BATTER2(idx)
        If A5%(1) >= 3 And A5%(1) <= 4 And B1%(2, 0) = 0 And B1%(3, 0) = 0 Then Call CHARGERUN(D, idx)
    End If

    idx = 0
    B1%(0, 0) = B1!(P)
    B1%(0, 1) = P1%(D)

    If A5%(0) <> 0 Then
        If A5%(0) > 0 And A5%(0) < 4 Then I3 = A5%(0): Call BATTER3(I3, P, D)
        If A5%(0) = 4 Then I3 = A5%(0): Call CHARGERUN(D, idx)
        If A5%(0) = 11 Then I3 = 1: Call CLEARBASE(idx)
        If A5%(0) = 12 Then I3 = 2: Call CLEARBASE(idx)
        If A5%(0) = 13 Then I3 = 3: Call CLEARBASE(idx)
        If A5%(0) = 14 Then I3 = 4: Call CLEARBASE(idx)
    End If

End Sub


'------------------------------
'   ADV1BASE Subroutine
'------------------------------
' ...explanation...
Sub ADV1BASE (i)

    For I4 = 0 To 2
        B0%(i + 1, I4) = B0%(i, I4)
        B0%(i + 1, 2) = 0
        B0%(i, I4) = 0
    Next I4

    Q$(i + 1) = Q$(i)
    Q$(i) = ""

End Sub

'------------------------------
'   ADV2BASES Subroutine
'------------------------------
' ...explanation...
Sub ADV2BASES (i)
    For I4 = 0 To 2
        B0%(i + 2, I4) = B0%(i, I4)
        B0%(i + 1, 2) = 0
        B0%(i, I4) = 0
    Next I4

    Q$(i + 2) = Q$(i)
    Q$(i) = ""

End Sub

'------------------------------
'   ADV3BASES Subroutine
'------------------------------
' ...explanation...
Sub ADV3BASES (I3, P, D)

    B0%(I3, 0) = B1!(P)
    B0%(I3, 1) = P1%(D)

    Q$(I3) = Q$(0)

    B0%(I3, 2) = 0
    B0%(0, 0) = 0

End Sub

'------------------------------
'   ADVANCEMENT Subroutine
'------------------------------
' ...explanation...
Sub ADVANCEMENT (P, D, S2%, F%, I3)

    If B0%(3, 0) <> 0 And A5%(3) <> 0 Then
        i = 3
        If A5%(3) >= 1 And A5%(3) <= 4 Then Call RecordRun(sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

        'nothing happens for values 5-9? or are they not possible?

        If A5%(3) = 10 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(3) = 11 Then I3 = 4: Call RecordOut(i, I3)
    End If

    If B0%(2, 0) <> 0 And A5%(2) <> 0 Then
        i = 2
        If A5%(2) = 1 And B0%(3, 0) = 0 Then Call ADV1BASE(i)
        If A5%(2) >= 2 And A5%(2) <= 4 And B0%(3, 0) = 0 Then Call RecordRun(sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

        'nothing happens for values 5-9? or are they not possible?

        If A5%(2) = 10 Then I3 = 2: Call RecordOut(i, I3)
        If A5%(2) = 11 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(2) = 12 And B0%(3, 0) = 0 Then I3 = 4: Call RecordOut(i, I3)
    End If

    If B0%(1, 0) <> 0 And A5%(1) <> 0 Then
        i = 1
        If A5%(1) = 1 And B0%(2, 0) = 0 Then Call ADV1BASE(i)
        If A5%(1) = 2 And B0%(2, 0) = 0 And B0%(3, 0) = 0 Then Call ADV2BASES(i)
        If A5%(1) >= 3 And A5%(1) <= 4 And B0%(2, 0) = 0 And B0%(3, 0) = 0 Then Call RecordRun(sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

        'nothing happens for values 5, 6, 7, 9? or are they not possible?

        If A5%(1) = 8 And B0%(2, 0) = 0 Then Call ADV1BASE(i)
        If A5%(1) = 10 Then I3 = 1: Call RecordOut(i, I3)
        If A5%(1) = 11 Then I3 = 2: Call RecordOut(i, I3)
        If A5%(1) = 12 And B0%(3, 0) = 0 And B0%(2, 0) = 0 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(1) = 13 And B0%(2, 0) = 0 And B0%(3, 0) = 0 Then I3 = 4: Call RecordOut(i, I3)
    End If

    i = 0
    B0%(0, 0) = B1!(P)
    B0%(0, 1) = P1%(D)

    If A5%(0) <> 0 Then
        'values 1 to 3
        If A5%(0) > 0 And A5%(0) < 4 Then I3 = A5%(0): Call ADV3BASES(I3, P, D)
        If A5%(0) = 4 Then I3 = A5%(0): Call RecordRun(sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

        'nothing happens for values 5-10 ? or are they not possible?

        If A5%(0) = 11 Then I3 = 1: Call RecordOut(i, I3)
        If A5%(0) = 12 Then I3 = 2: Call RecordOut(i, I3)
        If A5%(0) = 13 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(0) = 14 Then I3 = 4: Call RecordOut(i, I3)
    End If

    If F% = 0 And O%(1) < 3 Then Call ADJRUNNERS(S2%, P)

    For i = 0 To 3:
        A5%(i) = 0
    Next i

End Sub

'------------------------------
'   DisplayBatter_Lineup Subroutine
'------------------------------
' ...explanation...
Sub DisplayBatter_Lineup (PG, P9)

    Locate 1, 1

    Color 14: Print "#";
    Color L%(P9, 11), L%(P9, 12): Print A$(P9);

    Color 14, 0

    Locate , 16: Print "B   POS    G  AB 2B 3B HR RBI  BB  SO SRAB  SB CS G%  BA"

    For i = 2 To 13: Locate i, 1: Print String$(78, 32): Next

    Locate 2, 1

    If PG = 0 Then AA = 0: BB = 12
    If PG = 1 Then AA = 13: BB = 22

    For i = AA To BB:
        Locate , 1

        If B%(P9, i, 21) = 0 Then

            Color 2, 0

            If i < 10 Then Print Using "#"; i;
            If i > 9 Then Print X$(i - 10);

            Color 15, 0

            Print Tab(3); B$(P9, i); Tab(17); B1$(B%(P9, i, 0) + 2);

            For xx = 20 To 23
                Print Tab(xx);: If B%(P9, i, xx + 2) <> -1 Then Print Using "#"; B%(P9, i, xx + 2);
            Next

            Print Tab(24);
            Print Using " ### ### ## ## ## ### ### ### #"; B%(P9, i, 3); B%(P9, i, 4); B%(P9, i, 7); B%(P9, i, 8); B%(P9, i, 9); B%(P9, i, 10); B%(P9, i, 11); B%(P9, i, 12); B%(P9, i, 16);
            Print Using "#"; B%(P9, i, 17);: Print Using "#"; B%(P9, i, 15);: Print Using "# "; B%(P9, i, 30);
            Print Using "### ## ## "; B%(P9, i, 13); B%(P9, i, 14); B%(P9, i, 18);
            Print Tab(69); ".";

            'BA
            If B%(P9, i, 4) <> 0 Then
                bAvg! = B%(P9, i, 6) / B%(P9, i, 4)
                Print Using "###"; Int(((bAvg!) + .0005) * 1000);
            Else
                Print
            End If

        End If

    Next i

End Sub

'------------------------------
'   BATOFF Subroutine
'------------------------------
' ...explanation...
Sub BATOFF (P, I5, I6)

    B8%(P, I5, I6) = B8%(P, I5, I6) - 1
    B5%(P, I5, I6) = B5%(P, I5, I6) - 1

    If Not (I6 > 8 Or I6 = 2 Or I6 = 3) Then

        If P%(1 - P, P1%(1 - P), 0) <> 1 Then

            If I6 >= 4 Then
                CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) - 1
            Else
                CSS%(P, I5, I6) = CSS%(P, I5, I6) - 1
            End If

        End If

    End If

End Sub

'------------------------------
'   BATTER1 Subroutine
'------------------------------
' ...explanation...
Sub BATTER1 (i)

    For I4 = 0 To 1
        B1%(i + 1, I4) = B1%(i, I4)
        B1%(i, I4) = 0
    Next I4

End Sub

'------------------------------
'   BATTER2 Subroutine
'------------------------------
' ...explanation...
Sub BATTER2 (i)

    For I4 = 0 To 1
        B1%(i + 2, I4) = B1%(i, I4)
        B1%(i, I4) = 0
    Next I4

End Sub

'------------------------------
'   BATTER3 Subroutine
'------------------------------
' ...explanation...
Sub BATTER3 (I3, P, D)

    B1%(I3, 0) = B1!(P)
    B1%(I3, 1) = P1%(D)

End Sub

'------------------------------
'   CAUGHTFLY Subroutine
'------------------------------
' ...explanation...
Sub CAUGHTFLY (i, S2%, D)

    Call d100(RN)

    Select Case RN

        Case 1 To 96
            i = 1

            If S2% <> 17 Then
                pbpIdx% = Int(Rnd(1) * 60) + 1230
                Call GETRPBP(pbpIdx%, Y$, D)
                Call STRIPRPBP(Y$, pbpIdx%, D)
                Call PBP(Q$(4) + " " + Y$)
            Else
                If L%(1, 13) = 37 Then Call PBP("he climbs the terrace")
                pbpIdx% = Int(Rnd(1) * 20) + 1290
                Call GETRPBP(pbpIdx%, Y$, D)
                Call STRIPRPBP(Y$, pbpIdx%, D)
                Call PBP(Y$)
            End If

        Case 97
            i = 2
            Call PBP(Q$(4) + " makes a sliding catch")
            Call PBP("What a play!!!")

        Case 98
            i = 3
            Call PBP(Q$(4) + " makes a leaping grab")
            Call PBP("What a catch!!!")

        Case 99
            i = 4
            Call PBP(Q$(4) + " makes a diving catch!!!")

        Case 100
            i = 6
            Call PBP(Q$(4) + " makes an incredible catch!!")

    End Select

End Sub

'------------------------------
'   CHANGELINEUP Subroutine
'------------------------------
' ...explanation...
Sub CHANGELINEUP (P9, P)

    Locate 14, 1
    Color 2: Print " # LINEUP       PS  FA  OB"
    Color 15

    For i = 1 To 9:

        Locate , 1
        If B1!(P9) = i Then Print "*";

        Locate , 2: Print Using "#"; i;
        Locate , 4

        If B7%(P9, i) = 1 Then Print P$(P9, P1%(P9));
        If B7%(P9, i) <> 1 Then Print B$(P9, B3%(P9, i));
        If B7%(P9, i) <> 0 And B7%(P9, i) < 10 Then Locate , 20: Print B%(P9, B3%(P9, i), 19);

        Locate , 17: Print C$(B7%(P9, i));
        Locate , 25

        For I1 = 1 To 3
            If B0%(I1, 0) = i And P9 = P Then Color 3: Print I1;: Color 15
        Next I1

        Print " "

    Next i

End Sub

'------------------------------
'   CHARGERUN Subroutine
'------------------------------
' ...explanation...
Sub CHARGERUN (D, i)

    If O%(1) >= 3 Then
        Call CLEARBASE(i)
    Else
        I5 = B1%(i, 1)
        I6 = 3
        Call ADDPIT(D, I5, I6)
        Call CLEARBASE(i)
    End If

End Sub

'------------------------------
'   CHECKSAVE Subroutine
'------------------------------
' ...explanation...
Sub CHECKSAVE (D, P, S4%)

    I3 = S6%(D, 0) - S6%(P, 0)
    S1%(D) = -1

    If I3 >= 1 Then
        If S4% = 0 And I3 <= 2 Then S1%(D) = P1%(D)
        If S4% > 0 And S4% < 4 And I3 <= 3 Then S1%(D) = P1%(D)
        If S4% > 3 And S4% < 7 And I3 <= 4 Then S1%(D) = P1%(D)
        If S4% = 7 And I3 <= 5 Then S1%(D) = P1%(D)
        If S4% = 0 And O%(0) = 0 And I3 <= 3 Then S1%(D) = P1%(D)
        If INNING% > 5 And I3 > 0 And INNING% < 8 Then S1%(D) = P1%(D)
    End If

    RI = 0

    If S4% <> 0 Then
        If S4% = 1 Or S4% = 2 Or S4% = 3 Then RI = 1
        If S4% = 4 Or S4% = 5 Or S4% = 6 Then RI = 2
        If S4% = 7 Then RI = 3
        P5%(D, P1%(D), 17) = P5%(D, P1%(D), 17) + RI
        P8%(D, P1%(D), 17) = P8%(D, P1%(D), 17) + RI
    End If

End Sub

'------------------------------
'   CLEARBASE Subroutine
'------------------------------
' ...explanation...
Sub CLEARBASE (i)

    B1%(i, 0) = 0
    B1%(i, 1) = 0

End Sub

'------------------------------
'   CREDITASSIST Subroutine
'------------------------------
' ...explanation...
Sub CREDITASSIST (W%, D)

    If W% = 1 Then
        I5 = P1%(D)
        I6 = 39
        Call ADDPIT(D, I5, I6)
    Else
        I5 = B9%(D, W%)
        I6 = 17
        Call INCBATDF(D, I5, I6)
    End If

End Sub

'------------------------------
'   CREDITPUTOUT Subroutine
'------------------------------
' ...explanation...
Sub CREDITPUTOUT (I3, FB2P%, AST%, D)

    ' *** CREDIT INFIELD PUTOUTS ***

    If I3 > 4 Then I3 = 1

    po% = I3 + 2: ' 1=1st 2=2nd 3=3rd 4=home

    If po% = 6 Then
        po% = 2
    Else
        If po% = 3 And FB2P% = 1 Then
            FB2P% = 0
            po% = 1
            If po% = 1 Then I5 = P1%(D): I6 = 38: Call ADDPIT(D, I5, I6): GoTo 3418
        Else
            If po% = 4 And AST% = 4 Then
                po% = 6
            Else
                If po% = 4 And AST% = 6 Then
                    po% = 4
                End If
            End If
        End If

    End If

    3417 '
    AST% = 0
    I5 = B9%(D, po%)
    I6 = 16
    Call INCBATDF(D, I5, I6)

    3418 '
End Sub

'------------------------------
'   CREDITREGPO Subroutine
'------------------------------
' ...explanation...
Sub CREDITREGPO (W%, D)

    ' *** CREDIT REGULAR PUTOUT  ***

    If W% = 1 Then
        I5 = P1%(D)
        I6 = 38
        Call ADDPIT(D, I5, I6)
    Else
        I5 = B9%(D, W%)
        I6 = 16
        Call INCBATDF(D, I5, I6)
    End If

End Sub

'------------------------------
'   DELAY Subroutine
'------------------------------
' ...explanation...
Sub DELAY ()

    _Delay pbpDelay!

End Sub

'------------------------------
'   DELIVERY Subroutine
'------------------------------
' ...explanation...
Sub DELIVERY (D)

    ' *** setup the delivery ***
    Call d100(RN)

    If RN <= 20 Then
        pbpIdx% = Int(Rnd(1) * 20) + 50
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)
    End If

    pbpIdx% = Int(Rnd(1) * 46) + 1

    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)

End Sub

'------------------------------
'   DisplayFielding_Lineup Subroutine
'------------------------------
' ...explanation...
Sub DisplayFielding_Lineup (PG, P9, TG)

    Locate 1, 16
    Color 14: Print "P1 FAVG A R   P2 FAVG A R   P3 FAVG A R   P4 FAVG A R      "

    For i = 2 To 13: Locate i, 1: Print String$(78, 32): Next

    Locate 2

    If PG = 0 Then AA = 0: BB = 12
    If PG = 1 Then AA = 13: BB = 22

    For i = AA To BB:

        If B%(P9, i, 21) = 0 Then
            Color 2
            If i < 10 Then Print Using "#"; i;
            If i > 9 Then Print X$(i - 10);

            Color 15, 0
            Print Tab(3); B$(P9, i);

            Locate , 17: If B%(P9, i, 22) <> -1 Then Print Using "# #### # #"; B%(P9, i, 22); B%(P9, i, 26); B%(P9, i, 33); B%(P9, i, 34);
            Locate , 31: If B%(P9, i, 23) <> -1 Then Print Using "# #### # #"; B%(P9, i, 23); B%(P9, i, 27); B%(P9, i, 35); B%(P9, i, 36);
            Locate , 45: If B%(P9, i, 24) <> -1 Then Print Using "# #### # #"; B%(P9, i, 24); B%(P9, i, 28); B%(P9, i, 37); B%(P9, i, 38);
            Locate , 59: If B%(P9, i, 25) <> -1 Then Print Using "# #### # #"; B%(P9, i, 25); B%(P9, i, 29); B%(P9, i, 39); B%(P9, i, 40);

            Print

        End If

    Next i

    TG = TG + 1

End Sub

'------------------------------
'   DisplayRun Subroutine
'------------------------------
' ...explanation...
Sub DisplayRun (I2, P, X)

    J = B0%(I2, 0)

    If J <> 0 Then

        Color L%(P, 11), L%(P, 12)

        If B7%(P, J) = 1 And P%(P, P1%(P), 11) <> 999 Then

            Print Left$(P$(P, P1%(P)), 10)
            Color 15, 2
            Locate , X: Print "1/  0"

        Else
            If B7%(P, J) = 1 Then
                Print Left$(P$(P, P1%(P)), 10)
                Locate , X
                Color 15, 2: Print Using "#"; P%(P, P1%(P), 31);
                Print "/";: Print Using "###"; P%(P, P1%(P), 29)
            Else
                Print Left$(B$(P, B3%(P, J)), 10)
                Locate , X
                Color 15, 2: Print Using "#"; B%(P, B3%(P, J), 16);
                Print "/";: Print Using "###"; B%(P, B3%(P, J), 13)
            End If

        End If

    End If

    363 '
    Color 15, 0

End Sub

'------------------------------
'   DPPBP Subroutine
'------------------------------
' ...explanation...
Sub DPPBP ()

    Call d100(RN)

    Select Case RN
        Case 1 To 19: F$ = "may be a double play ball"
        Case 20 To 40: F$ = "they might turn two"
        Case 41 To 60: F$ = "they could get two"
        Case 61 To 80: F$ = "could be two"
        Case 81 To 100: F$ = "could be a double play"
    End Select

    Call PBP(F$)

End Sub

'------------------------------
'   DPPIVOT Subroutine
'------------------------------
' ...explanation...
Sub DPPIVOT (W%, D)

    ' *** double play pivot ***
    If W% = 4 Then ww% = 6
    If W% = 6 Then ww% = 4
    If W% = 3 Then ww% = 1

    pbpIdx% = Int(Rnd(1) * 20) + 1910

    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$ + " " + B$(D, B9%(D, ww%)))

End Sub

'------------------------------
'   DPRELAY Subroutine
'------------------------------
' ...explanation...
Sub DPRELAY ()

    If A5%(0) = 11 Then
        Call d100(RN)

        If RN <= 10 Then
            Call PBP("Give me the Hoover!")
        Else

            Call d100(RN)

            Select Case RN
                Case 1 To 8: F$ = "over to 1st..."
                Case 9 To 16: F$ = "throw to 1st..."
                Case 17 To 24: F$ = "on to first..."
                Case 25 To 30: F$ = Q$(4) + " pivots and throws"
                Case 31 To 36: F$ = Q$(4) + " gets the throw off"
                Case 37 To 44: F$ = Q$(4) + " is hit as he throws"
                Case 45 To 50: F$ = Q$(4) + " leaps and throws"
                Case 51 To 56: F$ = Q$(4) + " jumps over the sliding " + Q$(1)
                Case 57 To 64: F$ = "the relay to first..."
                Case 65 To 72: F$ = "quick turn by " + Q$(4)
                Case 73 To 80: F$ = "relay to 1st..."
                Case 81 To 88: F$ = "to 1st..."
                Case 89 To 100: If A5%(0) = 11 Then F$ = "over to 1st..."
            End Select

            Call PBP(F$)

        End If

    Else

        Call DELAY

    End If

End Sub

'------------------------------
'   DPSAFE routine
'------------------------------
' ...explanation...
Sub DPSAFE ()

    Call d100(RN)

    F$ = ""

    Select Case RN
        Case 1 To 8: F$ = "over to 1st..."
        Case 9 To 16: F$ = "throw to 1st..."
        Case 17 To 24: F$ = "on to first..."
        Case 25 To 30: F$ = "pivot to first"
        Case 31 To 36: F$ = "the throw to first"
        Case 37 To 44: F$ = "relay to first"
        Case 45 To 50: F$ = "quick turn to 1st"
        Case 51 To 56: F$ = "on to first..."
        Case 57 To 64: F$ = "the relay to first..."
        Case 65 To 72: F$ = "quick turn"
        Case 73 To 80: F$ = "relay to 1st..."
        Case 81 To 88: F$ = "to 1st..."
        Case 89 To 100: If A5%(0) = 11 Then F$ = "over to 1st..."
    End Select

    Call PBP(F$)

    pbpIdx% = Int(Rnd(1) * 20) + 1890

    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)

End Sub

'------------------------------
'   DRAWBOX Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOX (J)

    Locate 8, 8

    Color L%(1, 11), L%(1, 12)

    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    For II = 1 To J
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 9 + J, 8

    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

End Sub

'------------------------------
'   EJECTINC Subroutine
'------------------------------
' ...explanation...
Sub EJECTINC (EJ%, X$)

    EJ% = EJ% + 1

    EJ$(EJ%) = X$

End Sub

'------------------------------
'   EJECTIONINJURY Subroutine
'------------------------------
' ...explanation...
Sub EJECTIONINJURY (U6, P, U9, D, po%, P9, W5, W6)
    ' *** Defensive Player Ejected/Injured ***

    If Not (U6 = 2 Or U6 = 1 And P <> U9) Then
        F$ = "You must replace " + B$(D, B9%(D, po%)): Call PBP(F$)
    Else

        For I2 = 1 To 9
            If B7%(D, I2) = po% Then Exit For
        Next

        For idx = 0 To 22

            If B%(D, idx, 21) <= 0 And B$(D, idx) <> "XXX" Then

                If B7%(D, I2) = B%(D, idx, 22) Or B7%(D, I2) = B%(D, idx, 23) Or B7%(D, I2) = B%(D, idx, 24) Or B7%(D, I2) = B%(D, idx, 25) Then

                    2707 '
                    F$ = B$(D, B9%(D, po%)) + " will be replaced by " + B$(D, idx)
                    Call PBP(F$)

                    D0%(D) = D0%(D) + 1
                    X0%(D, 0, D0%(D)) = idx
                    X0%(D, 1, D0%(D)) = B7%(D, I2)
                    X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)
                    B%(D, idx, 21) = 1
                    B3%(D, I2) = idx
                    I7% = D
                    I8% = I2
                    i9% = idx

                    Call REPLACEMENTS(I7%, I8%, i9%)

                    B9%(D, B7%(D, I2)) = idx
                    P9 = D
                    W5 = P9
                    W6 = P9
                    VV(P9) = 1
                    VV1(P9) = 9

                    Exit Sub

                End If

            End If

        Next idx

    End If
    'For i = 0 To 22

    '   If B%(D, i, 21) <= 0 And B$(D, i) <> "XXX" Then
    '       GoTo 2707
    '   End If

    'Next i

End Sub

'------------------------------
'   FLDAVG Subroutine
'------------------------------
' ...explanation...
Sub FLDAVG (D, W%, I3)

    If B%(D, B9%(D, W%), 19) >= 999 Then

        I3 = 999

    Else

        If W% >= 7 Then
            I3 = B%(D, B9%(D, W%), 19)
        Else
            If W% = 3 Or W% = 2 Then I3 = (B%(D, B9%(D, W%), 19)) * .96
        End If

    End If

    I4 = B%(D, B9%(D, W%), 22)
    I5 = B%(D, B9%(D, W%), 23)
    I6 = B%(D, B9%(D, W%), 24)
    I7 = B%(D, B9%(D, W%), 25)

    If W% <> I4 And W% <> I5 And W% <> I6 And W% <> I7 Then
        If W% = 4 Or W% > 6 Then I3 = 982
        If W% = 2 Then I3 = 987
        If W% = 3 Then I3 = 980
        If W% = 6 Or W% = 5 Then I3 = 964

        If W% >= 7 Then
            I3 = I3 - 10
        Else
            I3 = I3 - 20
        End If

        If W% = 2 Or W% >= 4 And W% <= 6 Then I3 = I3 - 80: Exit Sub

    End If

    4462 '
    If W% = 4 Or W% = 6 Then I3 = (B%(D, B9%(D, W%), 19)) * .995
    If W% = 5 Then I3 = (B%(D, B9%(D, W%), 19)) * .985

End Sub

'------------------------------
'   DisplayBallField Subroutine
'------------------------------
' ...explanation...
Sub DisplayBallField (BC%, P, U%, D)

    Color , 0
    Cls

    For X = 5 To 25
        Locate X, 46
        Color , 2
        Print String$(34, 32);
    Next

    Color , 0
    Locate 1, 1

    inningStart% = 1: inningEnd% = 9

    If INNING% > 9 Then inningStart% = 10: inningEnd% = 18
    If INNING% > 18 Then inningStart% = 19: inningEnd% = 27
    Print Tab(10);

    'Innings / 1st row of stats
    For X1% = 0 To 2
        Print Using "## ## ##  "; inningStart% + 3 * X1%; inningStart% + 3 * X1% + 1; inningStart% + 3 * X1% + 2;
    Next

    Print " R  H  E ";
    Color 1: Print Chr$(179);
    Color 15: Print " BALL:  ";
    Color 14: Print BC%;
    Color 1: Print Chr$(179);
    Color 15: Print "WIND: ";
    Color 14: Print WI$

    'Team Score / Rows 2-3 of display
    For team = 0 To 1:

        Locate team + 2: Print Left$(A$(team), 3); NN$(team);

        For I2 = inningStart% To inningEnd%
            If P = 0 And team = 0 And I2 > INNING% - 1 And S%(team, INNING%) = 0 Or P = 0 And team = 0 And I2 > INNING% And S%(team, INNING%) > 0 Or P = 1 And I2 > INNING% And team = 0 Then Exit For
            If P = 0 And team = 1 And I2 > INNING% - 1 Or P = 1 And team = 1 And S%(team, INNING%) = 0 And I2 > INNING% - 1 Or P = 1 And team = 1 And S%(team, INNING%) > 0 And I2 > INNING% Then Exit For
            Call INNDISP(I2, team)
        Next I2

        Locate , 40: Print Using "## ## ## "; S6%(team, 0); S6%(team, 1); S6%(team, 2);
        Color 1: Print Chr$(179);
        Color 15

        If team = 0 Then Print " STRIKE:";: Color 14: Print U%;: Color 1: Print Chr$(179);: Color 15: Print "SKY:  ";: Color 14: Print SC$(SC%)
        If team = 1 Then Print " OUT:   ";: Color 14: Print O%(0);: Color 1: Print Chr$(179);: Color 15: Print "TEMP:";: Color 14: Print TP%

    Next team

    Color 1

    'Graphics / Lines
    Print String$(20, 196); Chr$(194); String$(23, 196); Chr$(194); String$(3, 196); Chr$(193); String$(11, 196); Chr$(193); String$(19, 196)
    For X = 5 To 12: Locate X, 21: Print Chr$(179);: Next
    For X = 5 To 25: Locate X, 45: Print Chr$(179);: Next

    'CF Name
    Color L%(D, 11), L%(D, 12)
    Locate 5, 58: Print Left$(B$(D, B9%(D, 8)), 10)

    'CF Stats
    Color 15, 2
    Locate 6, 59: Print Using "A:# R:#"; B%(D, B9%(D, 8), 15); B%(D, B9%(D, 8), 17)
    If B%(D, B9%(D, 8), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 8), 19)), "###", 1)
    End If
    Locate 7, 59: Print "FA:" + fldPct$

    'RF / LF Names
    Color L%(D, 11), L%(D, 12)
    Locate 6, 46: Print Left$(B$(D, B9%(D, 7)), 10);
    Locate , 70: Print Left$(B$(D, B9%(D, 9)), 10)

    'RF / LF Stats
    Color 15, 2
    Locate 7, 47: Print Using "A:# R:#"; B%(D, B9%(D, 7), 15); B%(D, B9%(D, 7), 17);
    Locate , 71: Print Using "A:# R:#"; B%(D, B9%(D, 9), 15); B%(D, B9%(D, 9), 17)

    If B%(D, B9%(D, 7), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 7), 19)), "###", 1)
    End If
    Locate 8, 47: Print "FA:" + fldPct$

    If B%(D, B9%(D, 9), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 9), 19)), "###", 1)
    End If
    Locate 8, 71: Print "FA:" + fldPct$

    'SS / 2B Names
    Color L%(D, 11), L%(D, 12)
    Locate 10, 49: Print Left$(B$(D, B9%(D, 6)), 10);
    Locate , 68: Print Left$(B$(D, B9%(D, 4)), 10)

    'SS / 2B Stats
    Color 15, 2
    Locate 11, 50: Print Using "R:#"; B%(D, B9%(D, 6), 17);
    Locate 11, 69: Print Using "R:#"; B%(D, B9%(D, 4), 17)

    If B%(D, B9%(D, 6), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 6), 19)), "###", 1)
    End If
    Locate 12, 50: Print "FA:" + fldPct$

    If B%(D, B9%(D, 4), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 4), 19)), "###", 1)
    End If
    Locate 12, 69: Print "FA:" + fldPct$

    '2nd Base "Graphic"
    Locate 12, 62: Print Chr$(219);

    '1B / 3B Names
    Color L%(D, 11), L%(D, 12)
    Locate 14, 46: Print Left$(B$(D, B9%(D, 5)), 10);
    Locate , 70: Print Left$(B$(D, B9%(D, 3)), 10)

    '1B / 3B Stats
    Color 15, 2
    Locate 15, 47: Print Using "R:#"; B%(D, B9%(D, 5), 17);
    Locate 15, 71: Print Using "R:#"; B%(D, B9%(D, 3), 17)

    If B%(D, B9%(D, 5), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 5), 19)), "###", 1)
    End If
    Locate 16, 47: Print "FA:" + fldPct$

    If B%(D, B9%(D, 3), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 3), 19)), "###", 1)
    End If
    Locate 16, 71: Print "FA:" + fldPct$

    '1st / 3rd Base "Graphic"
    Locate 17, 49: Print Chr$(219);
    Locate 17, 78: Print Chr$(219);

    'Pitcher Name
    Locate 17, 55: Print "["; B1$(P%(D, P1%(D), 0) + 2); "] "; Left$(P$(D, P1%(D)), 10)

    'Pitcher Stats
    Color 15, 2
    Locate 18, 59: Print Using "H:#"; P%(D, P1%(D), 18)
    If P%(D, P1%(D), 17) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(P%(D, P1%(D), 17)), "###", 1)
    End If
    Locate 19, 59: Print "FA:" + fldPct$

    'Home Plate "Graphic
    Locate 21, 62: Print Chr$(219)

    'Catcher Name
    Color L%(D, 11), L%(D, 12):
    Locate 23, 58: Print Left$(B$(D, B9%(D, 2)), 10)

    'Catcher Stats
    Color 15, 2
    Locate 24, 59: Print Using "   A:#"; B%(D, B9%(D, 2), 15);

    If B%(D, B9%(D, 2), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 2), 19)), "###", 1)
    End If
    Locate 25, 59: Print "FA:" + fldPct$;

    'Graphics / Lines
    Color 7, 0
    For X = 5 To 25: Locate X, 1: Print String$(44, 32);: Next

    Color 1
    Locate 4, 1: Print String$(20, 196); Chr$(194); String$(23, 196); Chr$(194)
    Locate 14, 1: Print String$(20, 196); Chr$(193); String$(23, 196); Chr$(180)

End Sub

'------------------------------
'   FLDERR Subroutine
'------------------------------
' ...explanation...
Sub FLDERR (F%, W%, D, I3, SB%, S2%, P)

    F% = 0

    If W% = 1 Then I3 = P%(D, P1%(D), 17) + 30
    If W% = 1 And SB% = 1 Then I3 = 995
    If W% <> 1 Then Call FLDAVG(D, W%, I3)

    If I3 >= 999 Then
        I6 = 1
    Else

        If S2% >= 12 And S2% <= 15 Then I6 = 1009 - I3
        If S2% >= 7 And S2% <= 9 Then I6 = 1006 - I3
        If S2% = 16 Or S2% >= 22 Then I6 = 1000 - I3
        If S2% >= 17 And S2% <= 21 Then I6 = (1000 - I3) * .05

    End If

    4408 '
    If W% = 7 Then I6 = I6 - 4
    If W% = 8 Then I6 = I6 - 10
    If W% = 9 Then I6 = I6 - 15
    If W% = 5 Then I6 = I6 - 15

    H6% = Int(Rnd(1) * 1000) + 1

    If H6% <= I6 Then

        F% = 1
        rand = Int(Rnd(1) * 2) + 1
        I1 = 2

        If W% = 1 Then I5 = P1%(D): I6 = 34: Call ADDPIT(D, I5, I6)
        If W% <> 1 Then I5 = B9%(D, W%): I6 = 11: Call INCBATDF(D, I5, I6)

        If S2% = 12 Then

            Call PBP(Q$(0) + " beats out an infield single")
            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
                I6 = 1
                Call INCBATOFF(P, I5, I6)
            End If

            I5 = P1%(D)
            I6 = 1
            Call ADDPIT(D, I5, I6)

            If B7%(P, B1!(P)) = 1 Then
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
                I6 = 24
                Call INCPITOFF(P, I5, I6)
            End If
        End If

        If S2% >= 17 And S2% <= 21 Then I1 = 3
        If S2% >= 17 And S2% <= 20 Then Call OUTFIELDERR
        If S2% = 8 Or S2% = 9 Or S2% >= 22 Or S2% = 16 Then I1 = 1
        If S2% >= 12 And S2% <= 14 And rand = 1 Or S2% = 15 Then I1 = 1

        Call d100(RN)

        If I1 = 1 And W% = 1 And S2% = 16 Then F$ = "*** WILD PICK OFF THROW BY" + " " + Q$(4) + " ***"
        If I1 = 1 And RN <= 50 Then F$ = "*** THROWING ERROR ON" + " " + Q$(4) + " ***"
        If I1 = 1 And RN > 50 And RN <= 55 Then F$ = "*** BALL KICKED BY" + " " + Q$(4) + " ***"
        If I1 = 1 Then F$ = "*** " + Q$(4) + " THROWS IT AWAY ***"

        If I1 = 2 And RN <= 30 Then F$ = "*** FIELDING ERROR ON" + " " + Q$(4) + " ***"
        If I1 = 2 And RN > 30 And RN <= 50 Then F$ = "*** OFF THE GLOVE OF" + " " + Q$(4) + " ***"
        If I1 = 2 And RN > 50 And RN <= 75 Then F$ = "*** BALL IS BOOTED BY" + " " + Q$(4) + " ***"
        If I1 = 2 And RN > 75 And RN <= 95 Then F$ = "*** BALL IS BOBBLED BY" + " " + Q$(4) + " ***"
        If I1 = 2 And RN > 95 Then F$ = "*** BALL IS THROUGH THE LEGS OF" + " " + Q$(4) + " ***"

        If I1 = 3 And RN <= 33 Then F$ = "*** BALL DROPPED BY" + " " + Q$(4) + " ***"
        If I1 = 3 And RN > 33 And RN <= 66 Then F$ = "*** OFF THE GLOVE OF" + " " + Q$(4) + " ***"
        If I1 = 3 Then F$ = "*** BALL IS MISPLAYED BY" + " " + Q$(4) + " ***"

        Call PBP(F$)

        If S2% = 16 Or S2% = 22 Then

            I8! = Int(Rnd(1) * 10) + 1

            If I8! < 5 Then
                A5%(0) = 2
                A5%(1) = 2
                A5%(2) = 2
                A5%(3) = 1
                Call DELAY
            End If

        Else

            Call d100(RN)
            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1

            If Not (S2% > 17 Or S2% <= 20) Then

                If RN <= 17 And S2% <> 8 Or S2% = 12 Then
                    A5%(0) = 2
                    A5%(1) = 2
                    A5%(2) = 2
                End If
            End If

        End If

        4418 '

        If S2% >= 17 And S2% <= 20 And O%(0) = 2 Or S2% >= 23 Or S2% = 16 And W% = 2 Then
            A5%(3) = 1
            A5%(2) = 2
            A5%(1) = 2
            A5%(0) = 2
        End If

        If S2% >= 17 And S2% <= 19 And RN <= 33 And O%(0) < 2 And B0%(1, 0) = 0 Then A5%(0) = 2

        If S2% = 16 Or S2% = 22 Then A5%(0) = 0

        If I1 > 0 And S2% <> 7 And S2% <> 8 And S2% <> 9 And S2% <> 16 And S2% <> 22 Then O%(1) = O%(1) + 1

        If S2% = 16 Or S2% = 7 Or S2% = 8 Or S2% = 9 Or S2% = 22 Then

            A5%(0) = 0

            If I8! > 4 And W% <> 2 Then
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
            End If

        End If

        S6%(D, 2) = S6%(D, 2) + 1

        Call ADVANCEMENT(P, D, S2%, F%, I3)

    End If

    4421 '

End Sub

'------------------------------
'   GETFIELDER Subroutine
'------------------------------
' ...explanation...
Sub GETFIELDER (W%, P, I1, D)

    W% = 0

    If B7%(P, B1!(P)) <> 1 Then

        If B%(P, B3%(P, B1!(P)), 0) < 0 Then I1 = -1

        If B%(P, B3%(P, B1!(P)), 0) > 0 Then I1 = 1

        If B%(P, B3%(P, B1!(P)), 0) = 0 Then I1 = -1 * P%(D, P1%(D), 0)

    Else

        I1 = P%(P, P1%(P), 0)

    End If

End Sub

'------------------------------
'   GETFIELDNAME Subroutine
'------------------------------
' ...explanation...
Sub GETFIELDNAME (W%, D)

    If W% <> 1 Then

        Q$(4) = B$(D, B9%(D, W%))

    Else

        Q$(4) = P$(D, P1%(D))

    End If

End Sub

'------------------------------
'   GETINFIELDER Subroutine
'------------------------------
' ...explanation...
Sub GETINFIELDER (W%, P, I1, D, S2%, P2)

    Call GETFIELDER(W%, P, I1, D)

    Call d100(RN)

    If I1 = 1 Then W% = 3
    If I1 = 1 And RN <= 92 Then W% = 4
    If I1 = 1 And RN <= 72 Then W% = 6
    If I1 = 1 And RN <= 32 Then W% = 5
    If I1 = -1 Then W% = 5
    If I1 = -1 And RN <= 88 Then W% = 6
    If I1 = -1 And RN <= 68 Then W% = 4
    If I1 = -1 And RN <= 28 Then W% = 3
    If RN <= 12 And S2% <> 20 Then W% = 1
    If RN <= 4 Or RN <= 12 And S2% = 20 Then W% = 2
    If S2% = 21 And W% = 2 Then W% = 1
    If P2 = 4 And W% = 6 Then W% = 1
    If P2 = 4 And W% = 4 Or S2% = 20 And W% = 1 Then W% = 2

    Call GETFIELDNAME(W%, D)

End Sub

'------------------------------
'   GETOUTFIELDER Subroutine
'------------------------------
' ...explanation...
Sub GETOUTFIELDER (W%, P, I1, D)

    Call GETFIELDER(W%, P, I1, D)

    Call d100(RN)

    If I1 = 1 Then W% = 7
    If I1 = 1 And RN <= 50 Then W% = 9
    If I1 = -1 Then W% = 9
    If I1 = -1 And RN <= 50 Then W% = 7
    If RN <= 30 Then W% = 8

    Q$(4) = B$(D, B9%(D, W%))

End Sub

'------------------------------
'   GROUNDERPBP Subroutine
'------------------------------
' ...explanation...
Sub GROUNDERPBP (W%, D)

    If W% <> 2 Then

        Call d100(RN)

        pbpIdx% = Int(Rnd(1) * 140) + 1930

        If RN > 35 Then
            If W% = 1 Then pbpIdx% = Int(Rnd(1) * 160) + 2070
            If W% = 3 Then pbpIdx% = Int(Rnd(1) * 150) + 2230
            If W% = 4 Then pbpIdx% = Int(Rnd(1) * 160) + 2380
            If W% = 6 Then pbpIdx% = Int(Rnd(1) * 160) + 2540
            If W% = 5 Then pbpIdx% = Int(Rnd(1) * 150) + 2700
        End If

        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        F$ = Y$ + " "

    Else

        3760 '*** in front of plate ***
        pbpIdx% = Int(Rnd(1) * 25) + 2870

        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)

    End If

End Sub

'------------------------------
'   GROUNDERS Subroutine
'------------------------------
' ...explanation...
Sub GROUNDERS (W%, D)

    ' *** grounders to infielders ***
    X$ = Q$(4)

    If W% <> 2 Then
        pbpIdx% = Int(Rnd(1) * 120) + 1420
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(X$ + " " + Y$)
    Else

        ' field grounders in front of the plate
        pbpIdx% = Int(Rnd(1) * 13) + 2850
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        If Left$(Y$, 2) = "QQ" Then Y$ = X$ + Right$(Y$, Len(Y$) - 2)
        Call PBP(Y$)
    End If

End Sub

'------------------------------
'   HITS2GAP Subroutine
'------------------------------
' ...explanation...
Sub HITS2GAP (W%, P, I1, D)

    Call GETFIELDER(W%, P, I1, D)

    Call d100(RN)

    If I1 = 1 Then
        'should be 6 to 15 ?
        If RN <= 15 Then
            W% = 9
        Else
            W% = 7
        End If
    End If

    If I1 = -1 Then
        'should be 6 to 15 ?
        If RN <= 15 Then
            W% = 7
        Else
            W% = 9
        End If
    End If

    If RN <= 5 Then W% = 8

    Q$(4) = B$(D, B9%(D, W%))

End Sub

'------------------------------
'   HOLDSTATUS Subroutine
'------------------------------
' ...explanation...
Sub HOLDSTATUS (H0%)

    Locate 17, 77

    Color 15, 2

    Print H0$(H0%)

    Color , 0

End Sub

'------------------------------
'   INCBATDF Subroutine
'------------------------------
' ...explanation...
Sub INCBATDF (D, I5, I6)

    B8%(D, I5, I6) = B8%(D, I5, I6) + 1

    B5%(D, I5, I6) = B5%(D, I5, I6) + 1

End Sub

'------------------------------
'   INCBATOFF Subroutine
'------------------------------
' ...explanation...
Sub INCBATOFF (P, I5, I6)

    B8%(P, I5, I6) = B8%(P, I5, I6) + 1

    B5%(P, I5, I6) = B5%(P, I5, I6) + 1

    If I6 >= 4 And I6 <= 8 And I6 <> 1 And P%(1 - P, P1%(1 - P), 0) <> 1 Then

        If I6 >= 4 Then
            CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) + 1
        Else
            CSS%(P, I5, I6) = CSS%(P, I5, I6) + 1
        End If

    End If

End Sub

'------------------------------
'   INCPITOFF Subroutine
'------------------------------
' ...explanation...
Sub INCPITOFF (P, I5, I6)

    P8%(P, I5, I6) = P8%(P, I5, I6) + 1

    P5%(P, I5, I6) = P5%(P, I5, I6) + 1

End Sub

'------------------------------
'   INCREMENTHIT Subroutine
'------------------------------
' ...explanation...
Sub INCREMENTHIT (P, D, MO)

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P))
        I6 = 15
        Call INCBATOFF(P, I5, I6)
    Else
        I5 = P1%(P)
        I6 = 37
        Call INCPITOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 13
    Call ADDPIT(D, I5, I6)

    If MO > 0 Then MO = MO - 1

End Sub

'------------------------------
'   INJPIT Subroutine
'------------------------------
' ...explanation...
Sub INJPIT (P, D, IP%)

    ' *** Defensive Pitcher Ejected/Injured ***
    If U6 = 2 Or U6 = 1 And P <> U9 Then
        IP% = 1
        Call RELIEVERS
    Else
        Call PBP("You must replace " + P$(D, P1%(D)))
    End If

End Sub

'------------------------------
'   INJURYLENGTH Subroutine
'------------------------------
' ...explanation...
Sub INJURYLENGTH (IJL%, D, po%, U6, P, U9, P9, W5, W6)

    doneLength = 0

    IJL% = IJL% + 1

    IJ$(IJL%) = B$(D, B9%(D, po%))

    F$ = B$(D, B9%(D, po%)) + " is injured for": Call PBP(F$)

    ' *** Determine Injury Length ***
    il = 0

    If B%(D, B9%(D, po%), 3) = 162 Then

        F$ = " remainder of game"

    Else

        ct = 0

        For I = 0 To 22

            For I1 = 22 To 25
                If B%(D, I, I1) = po% And B%(D, I, 32) = 0 And I <> B9%(D, po%) Then ct = ct + 1
                If ct > 3 Then il = 1: doneLength = 1: Exit For
            Next

        Next

        If doneLength = 0 Then
            F$ = " remainder of game"
            il = 0
        End If

        2117 '
        If il <> 0 Then

            If B%(D, B9%(D, po%), 3) = 161 Then
                F$ = " this game and next game"
                il = 1
            Else
                il = Int(Rnd(1) * (162 - B%(D, B9%(D, po%), 3))) + 1
                F$ = Str$(il) + " games"
            End If

        End If

    End If

    Call PBP(F$)
    B%(D, B9%(D, po%), 32) = B%(D, B9%(D, po%), 32) + il

    Call DELAY
    Call DELAY
    Call EJECTIONINJURY(U6, P, U9, D, po%, P9, W5, W6)

End Sub

'------------------------------
'   INNDISP Subroutine
'------------------------------
' ...explanation...
Sub INNDISP (currInning, teamLine)

    If currInning < 4 Then Locate , 3 * currInning + 7
    If currInning > 3 And currInning < 7 Then Locate , 3 * currInning + 8
    If currInning > 6 And currInning < 10 Then Locate , 3 * currInning + 9
    If currInning > 9 And currInning < 13 Then Locate , 3 * (currInning - 9) + 7
    If currInning > 12 And currInning < 16 Then Locate , 3 * (currInning - 9) + 8
    If currInning > 15 And currInning < 19 Then Locate , 3 * (currInning - 9) + 9
    If currInning > 18 And currInning < 22 Then Locate , 3 * (currInning - 18) + 7
    If currInning > 21 And currInning < 25 Then Locate , 3 * (currInning - 18) + 8
    If currInning > 24 And currInning < 28 Then Locate , 3 * (currInning - 18) + 9

    Print Using "##"; S%(teamLine, currInning);

End Sub

'------------------------------
'   LDRIVE Subroutine
'------------------------------
' ...explanation...
Sub LDRIVE ()

    Call d100(RN)

    Select Case RN
        Case 1 To 20: X$ = "rips"
        Case 21 To 40: X$ = "hammers"
        Case 41 To 60: X$ = "lashes"
        Case 61 To 80: X$ = "lines"
        Case 81 To 100: X$ = "drives"
    End Select

    Call PBP(" " + X$ + " ")

End Sub

'------------------------------
'   LONGFLY Subroutine
'------------------------------
' ...explanation...
Sub LONGFLY (D, W%)

    pbpIdx% = Int(Rnd(1) * 60) + 1310
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$ + " " + C$(W%))

    If WF < 1 And Rnd(1) > WF Then
        Call PBP("The wind is bringing it back..."):
    Else
        If WF > 1 And Rnd(1) <= 1 - WF Then Call PBP("It's carrying well in the breeze...")

        pbpIdx% = Int(Rnd(1) * 50) + 1370
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)

        Call d100(RN)

        If Not ((L%(1, 13) = 21 Or L%(1, 13) = 27) And W% = 7) Then

            Select Case RN
                Case 1 To 5: Call PBP(Q$(4) + " leaps")
                Case 6 To 10: Call PBP(Q$(4) + " against the wall")
                Case 11 To 20: Call PBP(Q$(4) + " jumps at the wall")
                Case 21 To 30: Call PBP(Q$(4) + " climbs the wall")
            End Select

        End If

    End If

End Sub

'------------------------------
'   MAINSCREEN Subroutine
'------------------------------
' ...explanation...
Sub MAINSCREEN (D, P, BC%, U%, MO)

    For I5 = 5 To 13
        Locate I5, 1
        Print Space$(44);
    Next I5

    Locate 5, 1
    Locate 21, 48

    Color 15, 2: Print Tab(62); Chr$(219); Tab(79);

    '--- is this necessary?
    'Color 7, 0

    Locate 10, 1

    Color L%(D, 11), L%(D, 12)
    Print P$(D, P1%(D)); " "; B1$(P%(D, P1%(D), 0) + 2)

    Color 15, 0

    Call DISPBAVG(D)

    Color 1

    For X = 5 To 13: Locate X, 21: Print Chr$(179);: Next

    Locate 11, 1: Color 2, 0: Print "W-L ERA ";

    Color 15, 0

    Print Using "##"; P%(D, P1%(D), 1);: Print "-";: Print Using "##"; P%(D, P1%(D), 2);
    Print " ";: Print Using "##.##"; P%(D, P1%(D), 10) / 100

    Color 2, 0

    Print "  IP  H  BB  K GAME"

    Color 15, 0

    If P8%(D, P1%(D), 0) = 0 Then
        Print " 0.0";
    Else
        Print Using "##"; Int(P8%(D, P1%(D), 0) / 3);
        Print ".";: Print Using "#"; P8%(D, P1%(D), 0) - (Int(P8%(D, P1%(D), 0) / 3) * 3);
    End If

    Print Using " ##  ## ##"; P8%(D, P1%(D), 1); P8%(D, P1%(D), 4); P8%(D, P1%(D), 5)
    Locate 5, 1
    J = B3%(P, B1!(P))
    Color L%(P, 11), L%(P, 12)

    If B7%(P, B1!(P)) = 1 Then
        X$ = P$(P, P1%(P))
        Print X$; " "; B1$(P%(P, P1%(P), 0) + 2);
        Call PITPLACEMENT(P, X$)
    Else
        X$ = B$(P, J)
        Print X$; " "; B1$(B%(P, J, 0) + 2);
        If B%(P, J, 0) < 0 Or B%(P, J, 0) = 0 And P%(D, P1%(D), 0) > 0 Then Locate 21, 64
        X = Len(X$)
        If B%(P, J, 0) > 0 Or B%(P, J, 0) = 0 And P%(D, P1%(D), 0) < 0 Then Locate 21, 51 + 10 - X
        Print X$
    End If

    Locate 6, 1
    Color 2, 0: Print " AVG HR  BB   K"
    Color 15, 0

    If B7%(P, B1!(P)) <> 1 Then
        If B%(P, J, 4) = 0 Then
            Print ".000  0   0   0";
        Else
            bAvg! = B%(P, J, 6) / B%(P, J, 4) * 1000
            Print "."; Using "###"; bAvg!
            Print Using " ## ### ###"; B%(P, J, 9); B%(P, J, 11); B%(P, J, 12)
        End If
    Else

        If P%(P, P1%(P), 11) <> 999 Then
            pAvg! = P%(P, P1%(P), 11)
            If pAvg! = 0 Then
                Print ".000 0"
            Else
                Print "."; Using "###"; pAvg! * 1000
            End If
        Else
            If P%(P, P1%(P), 20) = 0 Then
                Print ".000 0";
            Else
                pAvg! = P%(P, P1%(P), 22) / P%(P, P1%(P), 20) * 1000
                Print "."; Using "###"; pAvg!
                Print Using " ## ### ###"; P%(P, P1%(P), 25); P%(P, P1%(P), 27); P%(P, P1%(P), 28)
            End If
        End If
    End If

    Locate 8, 2

    If B7%(P, B1!(P)) <> 1 Then
        Print B8%(P, B3%(P, B1!(P)), 1); "-"; B8%(P, B3%(P, B1!(P)), 0);
    Else
        Print P8%(P, P1%(P), 24); "-"; P8%(P, P1%(P), 23);
    End If

    Print " TODAY"
    Color 14, 0
    'Outs
    Locate 3, 58: Print O%(0)
    'Strikes
    Locate 2, 58: Print U%
    'Balls
    Locate 1, 58: Print BC%
    Color 15, 2
    Locate 22, 46: Print "INFIELD:"

    Locate 23, 46
    Select Case INFPOS%
        Case 0: Print "NORMAL    "
        Case 1: Print "CORNERS IN"
        Case 2: Print "INFIELD IN"
    End Select

    Call HOLDSTATUS(H0%)

    Locate 13, 58
    Color 15, 2: Print Tab(68);
    Locate 14, 58: Print Space$(8)

    Locate 13, 58

    I2 = 2
    X = 58

    Call DisplayRun(I2, P, X)

    If O%(0) <> 3 Then MO = 0

    For WK = 1 To 3
        If B0%(WK, 0) > 0 Then MO = MO + 1
    Next

    Locate 18, 47
    Color 15, 2: Print Tab(57);

    Locate 19, 47: Print Space$(8)

    Locate 18, 47

    I2 = 3
    X = 47

    Call DisplayRun(I2, P, X)

    Locate 18, 69
    Color 15, 2: Print Tab(79);
    Locate 19, 70: Print Space$(8)

    Locate 18, 69

    I2 = 1
    X = 70

    Call DisplayRun(I2, P, X)

    Color 15, 0

End Sub

'------------------------------
'   WINDEX Subroutine
'------------------------------
' ...explanation...
Sub WINDEX ()

    If pbpDelay! > 0 Then


        PB% = 0

        If usingGfx = 1 Then

            Color 4294967295
            _PrintMode _FillBackground , imageScreen&

            For I5 = 4 To 11
                PrintOnGfx 20, I5, Space$(44), imageScreen&
            Next

        Else

            For I5 = 15 To 24
                Locate , 1
                Locate I5
                Print Space$(44);
            Next I5
            Locate 15, 1

        End If

        Erase G$

    End If

End Sub

'------------------------------
'   NEWPITCHER Subroutine
'------------------------------
' ...explanation...
Sub NEWPITCHER

    V0 = i

    If S1%(D) <> -1 And P6%(D) > 1 Then I5 = P1%(D): I6 = 22: Call ADDPIT(D, I5, I6)

    P1%(D) = V0
    P6%(D) = P6%(D) + 1
    P%(D, V0, 15) = P6%(D)
    P9 = D

    Call PITSTAM(P9)

    For V0 = 1 To 9
        If B7%(D, V0) = 1 Or B7%(D, V0) = 10 Then
            B3%(D, V0) = P1%(D)
            B7%(D, V0) = 1
            B9%(D, 1) = P1%(D)
            P9 = D
            W5 = P9
            W6 = P9
            VV(P9) = V0
            VV1(P9) = V0
            Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
        End If
    Next V0

    VV(P9) = 1
    VV1(P9) = 9
    B9%(D, 1) = P1%(D)

    Call CHECKSAVE(D, P, S4%)

    W5 = 1 - P9
    W6 = 1 - P9
    VV(1 - P9) = 1
    VV1(1 - P9) = 9

    Call WINDEX

    Locate 15, 1

    Call d100(RN)

    If RN <= 50 Then Call PBP("Hold the phone...")

    Call PBP(M$(D) + " is making a pitching change")
    Call DELAY
    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

    If usingGfx = 1 Then
        Call DisplayBallField_Gfx(BC%, P, U%, D)
        Call MAINSCREEN_G(D, P, BC%, U%, MO)
        PPH = 0
    Else
        Call DisplayBallField(BC%, P, U%, D)
        Call MAINSCREEN(D, P, BC%, U%, MO)
        PPH = 0
    End If

    pz = 1

End Sub

'------------------------------
'   NOJUMP Subroutine
'------------------------------
' ...explanation...
Sub NOJUMP (D)

    pbpIdx% = Int(Rnd(1) * 20) + 1050
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)
    Call DELAY

End Sub

'------------------------------
'   OFFPIT Subroutine
'------------------------------
' ...explanation...
Sub OFFPIT (P9, I5, I6)

    P8%(P9, I5, I6) = P8%(P9, I5, I6) + 1
    P5%(P9, I5, I6) = P5%(P9, I5, I6) + 1

End Sub

'------------------------------
'   OUTATFIRST Subroutine
'------------------------------
' ...explanation...
Sub OUTATFIRST (W%, D, fbu%, FB2P%)

    Call PITCHERNAME(W%, D)
    Call GROUNDERS(W%, D)
    Call d100(RN)

    If W% = 3 And RN <= 94 Then
        Call PBP("he takes it to the bag unassisted")
        fbu% = 1
    Else
        If W% = 3 Then
            W% = 1
            Call PITCHERNAME(W%, D)
            Call PBP("He flips it to " + Q$(4) + " covering")
            FB2P% = 1
            W% = 3
        End If

        If W% = 4 Then Call SECOND2FIRST(D)

        If W% = 5 Or W% = 6 Then Call THROW2FIRST(D)

    End If

End Sub

'------------------------------
'   OUTFIELDERR Subroutine
'------------------------------
' ...explanation...
Sub OUTFIELDERR ()

    ' *** outfielders in trouble ***

    Call d100(RN)

    Select Case RN
        Case 1 To 20: F$ = Q$(4) + " can't find it"
        Case 21 To 40: F$ = Q$(4) + " misjudges it"
        Case 41 To 60: F$ = Q$(4) + " turns the wrong way"
        Case 61 To 80: F$ = Q$(4) + " loses it"
        Case 81 To 100: F$ = Q$(4) + " doesn't see it"
    End Select

    Call PBP(F$)

End Sub

'------------------------------
'   OUTFLDLOCATION Subroutine
'------------------------------
' ...explanation...
Function OUTFLDLOCATION$ (W%)

    Call d100(RN)

    If W% = 7 Or W% = 8 And RN > 50 Then OUTFLDLOCATION$ = "lcf"
    If W% = 9 Or W% = 8 And RN <= 50 Then OUTFLDLOCATION$ = "rcf"

End Function

'------------------------------
'   OUTFLDNAME Subroutine
'------------------------------
' ...explanation...
Sub OUTFLDNAME (W%, D)

    W% = 9

    Call d100(RN)

    Select Case RN
        Case 1 To 5: W% = 7
        Case 6 To 25: W% = 8
    End Select

    Q$(4) = B$(D, B9%(D, W%))

End Sub

'------------------------------
'   PICKEDOFF Subroutine
'------------------------------
' ...explanation...
Sub PICKEDOFF (D, i)

    ' *** picked off ***
    Call WINDEX
    Call DELIVERY(D)

    Call d100(RN)
    Select Case RN
        Case 1 To 33: F$ = "NO, " + P$(D, P1%(D)) + " throws to " + H$(i - 1)
        Case 34 To 66: F$ = "quick move to " + H$(i - 1)
        Case 67 To 100: F$ = "pickoff throw to " + H$(i - 1)
    End Select

    Call PBP(F$)

    Call d100(RN)

    If RN <= 50 Then
        F$ = Q$(i - 1) + " is picked off base!"
    Else
        F$ = Q$(i - 1) + " is caught napping"
    End If

    Call PBP(F$)

    Call d100(I8!)

    Select Case I8!
        Case 1 To 25: F$ = "he can't believe it"
        Case 26 To 50: F$ = "what a teriffic move by " + P$(D, P1%(D))
    End Select

    Call PBP(F$)

End Sub

'------------------------------
'   PINCHRUNNER Subroutine
'------------------------------
' ...explanation...
Sub PINCHRUNNER (MS%, A1, S4%, I2, P, PR%, BC%, U%, D, MO)

    ' *** CHECK FOR PINCH RUNNER ***
    i0% = 0

    If A1 <= 1 And A1 >= -2 And INNING% >= 8 Then

        I2 = 1

        If S4% = 1 Or S4% = 5 Then I2 = 1
        If S4% = 2 Or S4% = 4 Or S4% = 6 Then I2 = 2

        ' J REPRESENTS THE SPOT IN THE BATTING ORDER
        ' j1 represents the base
        J = B0%(I2, 0)
        J1 = I2

        If J <> 0 Then

            If MG%(P, 771) = 999 Then

                For I1 = 772 To 777
                    If MG%(P, I1) <> -1 Then
                        If B%(P, MG%(P, I1), 21) = 0 Then
                            I1 = MG%(P, I1)
                            If B7%(P, J) = 1 Then
                                8010 'PINCH RUNNING FOR THE PITCHER
                                If MG%(P, 803) = 999 Then
                                    Call d100(RN)
                                    If P%(P, P1%(P), 31) <= MG%(P, 804) And INNING% >= MG%(P, 805) And RN <= MG%(P, 806) Then 8993
                                    If P%(P, P1%(P), 31) <= MG%(P, 807) And INNING% >= MG%(P, 808) And RN <= MG%(P, 809) Then 8993
                                    GoTo 8999
                                End If
                            End If

                            For I = 0 To 5
                                If MG%(P, I * 4 + 779) = B3%(P, J) Then
                                    8003 'FOUND A MATCH FOR THE RUNNER ON BASE
                                    Call d100(RN)
                                    If INNING% >= MG%(P, I * 4 + 780) And B%(P, MG%(P, I * 4 + 781), 21) = 0 And RN <= MG%(P, I * 4 + 782) Then B%(P, MG%(P, I * 4 + 781), 21) = 40 + J: GoTo 8993
                                    GoTo 8999
                                End If
                            Next

                        End If
                        MG%(P, I1) = -1
                    End If

                Next

                GoTo 8999

            End If

            8000 '
            If B7%(P, J) = 1 And P%(P, P1%(P), 31) > 4 Then 8999
            If B7%(P, J) <> 1 And B%(P, B3%(P, J), 16) > 3 Then 8999

            I1 = -1

            For I = 0 To 22
                If B$(P, I) <> "XXX" And B%(P, I, 21) <= 0 Then
                    For I2 = 22 To 25
                        If B%(P, I, I2) <> -1 And B%(P, I, I2) = B7%(P, J) And B7%(P, J) <> 1 Then I1 = I
                    Next
                    If B%(P, I, 16) > 5 Then PR%(i0%) = I: i0% = i0% + 1
                End If
            Next

            If i0% = 0 Or I1 = -1 And B7%(P, J) <> 1 Then 8999
            If B7%(P, J) <> 1 Then B%(P, I1, 21) = 40 + J

            RN = Int(Rnd(1) * i0%)
            I1 = PR%(RN)

            8993 '
            I2 = J
            Q$(J1) = B$(P, I1)
            B%(P, I1, 21) = 1
            B3%(P, I2) = I1
            B9%(P, B7%(P, I2)) = I1

            If B7%(P, I2) = 1 Or B7%(P, I2) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = I2: B7%(P, I2) = 10

            D0%(P) = D0%(P) + 1
            X0%(P, 0, D0%(P)) = B3%(P, I2)

            X0%(P, 1, D0%(P)) = 11
            X0%(P, 2, D0%(P)) = (I2 * 10) + (D0%(P) - 9)

            W5 = P
            W6 = P
            VV(P) = I2
            VV1(P) = I2

            For xx = 22 To 25
                If B7%(P, I2) = B%(P, I1, xx) Then B%(P, I1, 15) = B%(P, I1, 33 + (xx - 22) * 2): B%(P, I1, 17) = B%(P, I1, 34 + (xx - 22) * 2): B%(P, I1, 19) = B%(P, I1, xx + 4): Exit For
            Next

            8995 '
            Call WINDEX
            Locate 15, 1
            Call PBP(M$(P) + " is sending in a pinch runner")
            PR% = 1

            Call DELAY
            Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

            If usingGfx = 1 Then
                Call DisplayBallField_Gfx(BC%, P, U%, D)
                Call MAINSCREEN_G(D, P, BC%, U%, MO)
                MS% = 1
            Else
                Call DisplayBallField(BC%, P, U%, D)
                Call MAINSCREEN(D, P, BC%, U%, MO)
                MS% = 1
            End If

            VV(P) = 1
            VV1(P) = 9

        End If

    End If

    8999 '

End Sub

'------------------------------
'   PITCHERNAME Subroutine
'------------------------------
' ...explanation...
Sub PITCHERNAME (W%, D)

    If W% = 1 Then
        Q$(4) = P$(D, P1%(D))
    Else
        Q$(4) = B$(D, B9%(D, W%))
    End If

End Sub

'------------------------------
'   PITCHOUT Subroutine
'------------------------------
' ...explanation...
Sub PITCHOUT (S4%, TGTBASE%, Q0)

    If S4% <> 5 And TGTBASE% = 2 Then
        Call d100(PJ)
        If PJ <= 8 Then Call PBP("...PITCH OUT!!!"): Q0 = Q0 - 200
    End If

End Sub

'------------------------------
'   PITDISP Subroutine
'------------------------------
' ...explanation...
Sub PITDISP (P9)

    Cls

    Color 14, 0
    Print "#";
    Color L%(P9, 11), L%(P9, 12): Print A$(P9);
    Color 14, 0
    Print " T  IP   H   G GS CG SV HR  BB  SO  W- L   ERA  H G%"

    AA = 0
    BB = 21

    For i = AA To BB:

        If P%(P9, i, 15) <= 0 Then
            Color 2, 0

            If i <= 9 Then
                Print Using "#"; i;
            Else
                Print X$(i - 10);
            End If

            Color 15, 0

            Locate , 3: Print P$(P9, i);
            Locate , 18: Print B1$(P%(P9, i, 0) + 2);

            Print Using " ### ### ### ## ## ## ## ### ### ##"; P%(P9, i, 6); P%(P9, i, 7); P%(P9, i, 4); P%(P9, i, 5); P%(P9, i, 16); P%(P9, i, 3); P%(P9, i, 13); P%(P9, i, 8); P%(P9, i, 9); P%(P9, i, 1);
            Print "-";
            Print Using "##"; P%(P9, i, 2);

            Locate , 59: Print Using "##.## # ##"; P%(P9, i, 10) / 100; P%(P9, i, 18); P%(P9, i, 12)

        End If

    Next i

End Sub

'------------------------------
'   PITOFF Subroutine
'------------------------------
' ...explanation...
Sub PITOFF (P, I5, I6)

    P8%(P, I5, I6) = P8%(P, I5, I6) - 1
    P5%(P, I5, I6) = P5%(P, I5, I6) - 1

End Sub

'------------------------------
'   PITPLACEMENT Subroutine
'------------------------------
' ...explanation...
Sub PITPLACEMENT (P, X$)

    If usingGfx = 1 Then
        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color 4294967295

    End If

    If P%(P, P1%(P), 0) < 0 Then
        If usingGfx = 1 Then
            PrintOnGfx 42, 25, X$, imageScreen&
        Else
            Locate 21, 64
            Print X$
        End If
    End If

    If P%(P, P1%(P), 0) > 0 Then
        If usingGfx = 1 Then
            PrintOnGfx 30, 25, X$, imageScreen&
        Else
            Locate 21, 51
            Print X$
        End If
    End If

    If usingGfx = 1 Then
        'Printing white on black
        Color 4294967295
        _PrintMode _FillBackground , imageScreen&
    End If

End Sub

'------------------------------
'   PITSTAM Subroutine
'------------------------------
' ...explanation...
Sub PITSTAM (P9)

    If P%(P9, P1%(P9), 4) = 0 Then
        S8%(P9, 0) = 3
    Else
        S8%(P9, 0) = Int(((P%(P9, P1%(P9), 8) + P%(P9, P1%(P9), 7)) / P%(P9, P1%(P9), 4)) + .5)
    End If

    If S8%(P9, 0) < 3 Then S8%(P9, 0) = 3
    S8%(P9, 1) = 3

    If P6%(P9) > 1 Then S8%(P9, 1) = 0

    If CGERA% > 0 Then
        If P6%(P9) > 1 Then
            S8%(P9, 1) = 1
        Else
            S8%(P9, 0) = S8%(P9, 0) + 1
        End If
    End If

    If INNING% <= 5 And S6%(P9, 0) - S6%(1 - P9, 0) > 0 Then P2%(P9) = P1%(P9)

End Sub

'------------------------------
'   PITSTRAT Subroutine
'------------------------------
' ...explanation...
Sub PITSTRAT ()

    A1 = S6%(D, 0) - S6%(P, 0)

    pz = 0

    ' if there are no middle relievers in the profile,
    ' and it's less than the 7th inning, then go back
    ' and find one the old fashioned way (which is?)

    If MG%(D, 60) = 999 Or INNING% > 7 Then

        'if no closers set up or no closing strategy defined, skip out
        If Not (INNING% >= 8 And MG%(D, 520) <> 999 And MG%(D, 90) <> 999 Or MG%(D, 581) <> 999) Then

            'if still needing middle relief
            If INNING% <= 7 Then
                GoTo 8115
            End If

            ' we have defined a closing/setup possibility
            ' identify the stratgey
            I2 = 0
            For I = 582 To 587
                I2 = I2 + MG%(D, I)
            Next
            I3 = Int(Rnd(1) * I2) + 1
            I2 = 0
            For I = 582 To 587
                I2 = I2 + MG%(D, I)
                If I3 <= I2 Then
                    SN% = I

                    8111 SF% = 0

                    If A1 < 1 Then 8195

                    If A1 > 0 And A1 <= 2 Then SF% = 1
                    If S4% = 0 And A1 <= 2 Then SF% = 1
                    If S4% > 0 And S4% < 4 And A1 <= 3 Then SF% = 1
                    If S4% > 3 And S4% < 7 And A1 <= 4 Then SF% = 1
                    If S4% = 7 And A1 <= 5 Then SF% = 1
                    If S4% = 0 And O%(0) = 0 And A1 <= 3 Then SF% = 1
                    If B1!(P) <= 7 Then For I = 0 To 2: I3 = I3 + B%(P, B3%(P, B1!(P) + I), 0): Next
                    If B1!(P) = 8 Then For I = 0 To 1: I3 = I3 + B%(P, B3%(P, B1!(P) + I), 0): Next: I3 = I3 + B%(P, B3%(P, 1), 0)
                    If B1!(P) = 9 Then For I = 1 To 2: I3 = I3 + B%(P, B3%(P, I), 0): Next: I3 = I3 + B%(P, B3%(P, 9), 0)

                    8112 If I3 = 0 Then I3 = Int(Rnd(1) * 3) - 1
                    If I3 = 0 Then 8112

                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8114
                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8113
                    If INNING% >= 9 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8113

                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 > 0 And SN% = 583 Then 8114
                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 > 0 And SN% = 583 Then 8113
                    If INNING% >= 9 And A1 > 0 And SN% = 583 Then 8113

                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 >= 0 And SN% = 584 Then 8114
                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 >= 0 And SN% = 584 Then 8113
                    If INNING% >= 9 And A1 >= 0 And SN% = 584 Then 8113

                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 > 0 And SN% = 585 Then 8114
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 > 0 And SN% = 585 Then 8113
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% >= 9 And A1 > 0 And SN% = 585 Then 8113

                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 >= 0 And SN% = 586 Then 8114
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 >= 0 And SN% = 586 Then 8113
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% >= 9 And A1 >= 0 And SN% = 586 Then 8113

                    GoTo 8195

                End If
            Next

        End If

    End If

    GoTo 8195

    'strategy identified, now

    8114
    I2 = 0
    If I3 < 0 Then I1 = 81
    If I3 > 0 Then I1 = 84
    For I = I1 To I1 + 2
        If MG%(D, I) >= 0 And P%(D, MG%(D, I), 15) = 0 Then I2 = I2 + MG%(D, I + 10)
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For I = I1 To I1 + 2
        If MG%(D, I) >= 0 And P%(D, MG%(D, I), 15) = 0 Then I2 = I2 + MG%(D, I + 10)
        If I3 <= I2 Then V0 = MG%(D, I): MG%(D, I) = -1: I = V0: Call NEWPITCHER: SU%(D, 0) = -1: GoTo 8195
    Next
    'NO SETUP MAN FOUND
    'GOTO 8195

    8113
    I2 = 0
    If I3 < 0 Then I1 = 521
    If I3 > 0 Then I1 = 524

    For I = I1 To I1 + 2
        If MG%(D, I) >= 0 And P%(D, MG%(D, I), 15) = 0 Then I2 = I2 + MG%(D, I + 10)
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For I = I1 To I1 + 2
        If MG%(D, I) >= 0 And P%(D, MG%(D, I), 15) = 0 Then I2 = I2 + MG%(D, I + 10)
        If I3 <= I2 Then V0 = MG%(D, I): MG%(D, I) = -1: I = V0: Call NEWPITCHER: SU%(D, 1) = -1: GoTo 8195
    Next

    'NO CLOSER FOUND
    'GOTO 8195
    SU%(D, 0) = -1: SU%(D, 1) = -1

    8115 '
    I2 = 0

    For I = 61 To 70
        If MG%(D, I) >= 0 And P%(D, MG%(D, I), 15) = 0 Then I2 = I2 + MG%(D, I + 10)
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For I = 61 To 70
        If MG%(D, I) >= 0 And P%(D, MG%(D, I), 15) = 0 Then I2 = I2 + MG%(D, I + 10)
        If I3 <= I2 Then V0 = MG%(D, I): MG%(D, I) = -1: I = V0: Call NEWPITCHER: GoTo 8195
    Next

    'NO MIDDLE RELIEVER FOUND
    'GoTo 8195

    8195 '
End Sub

'------------------------------
'   PITSWAP Subroutine
'------------------------------
' ...explanation...
Sub PITSWAP (i)

    If i = 4 Then V = 3: Call SOMESTATS(V, P, F%, D): V = 2: Call SOMESTATS(V, P, F%, D): V = 1: Call SOMESTATS(V, P, F%, D)

    If i = 3 Then V = 2: Call SOMESTATS(V, P, F%, D): V = 1: Call SOMESTATS(V, P, F%, D)

    If i = 2 Then V = 2: Call SOMESTATS(V, P, F%, D): V = 1: Call SOMESTATS(V, P, F%, D)

End Sub

'------------------------------
'   PLAYERNAME Subroutine
'------------------------------
' ...explanation...
Sub PLAYERNAME (P)

    If B7%(P, B1!(P)) = 1 Then
        Q$(0) = P$(P, P1%(P))
    Else
        Q$(0) = B$(P, B3%(P, B1!(P)))
    End If

    For i = 1 To 3

        If B0%(i, 0) > 0 Then
            If B7%(P, B0%(i, 0)) <> 1 Then
                Q$(i) = B$(P, B3%(P, B0%(i, 0)))
            Else
                Q$(i) = P$(P, P1%(P))
            End If
        End If

    Next i

End Sub

'------------------------------
'   PLYRHRT Subroutine
'------------------------------
' ...explanation...
Sub PLYRHRT (P, P9, W5, W6, BC%, U%, D, MO)

    ' *** Offensive Player Ejected/Injured ***
    If Not (U6 = 2 Or U6 = 1 And P = U9) Then

        Call PBP("You must replace " + Q$(0) + " with a pinch-hitter")

    Else

        For i = 0 To 22

            If B%(P, i, 21) <= 0 And B$(P, i) <> "XXX" Then

                If B7%(P, B1!(P)) = B%(P, i, 22) Or B7%(P, B1!(P)) = B%(P, i, 23) Or B7%(P, B1!(P)) = B%(P, i, 24) Or B7%(P, B1!(P)) = B%(P, i, 25) Then

                    Call PinchHitter(P, P9, W5, W6, i)

                    If usingGfx = 1 Then
                        Call DisplayBallField_Gfx(BC%, P, U%, D)
                        Call MAINSCREEN_G(D, P, BC%, U%, MO)
                    Else
                        Call DisplayBallField(BC%, P, U%, D)
                        Call MAINSCREEN(D, P, BC%, U%, MO): MS% = 1
                    End If

                    Locate 15, 1

                    Q$(0) = B$(P, B3%(P, B1!(P))): If B7%(P, B1!(P)) = 1 Then Q$(0) = P$(P, P1%(D))

                    Exit Sub 'GoTo 2849

                End If

            End If

        Next i

        For i = 0 To 22
            If B%(P, i, 21) <= 0 And B$(P, i) <> "XXX" Then
                Call PinchHitter(P, P9, W5, W6, i)

                If usingGfx = 1 Then
                    Call DisplayBallField_Gfx(BC%, P, U%, D)
                    Call MAINSCREEN_G(D, P, BC%, U%, MO)
                Else
                    Call DisplayBallField(BC%, P, U%, D)
                    Call MAINSCREEN(D, P, BC%, U%, MO): MS% = 1
                End If

                Locate 15, 1

                Q$(0) = B$(P, B3%(P, B1!(P))): If B7%(P, B1!(P)) = 1 Then Q$(0) = P$(P, P1%(D))

                Exit Sub 'GoTo 2849

            End If
        Next i

    End If
    2849 '

End Sub

'------------------------------
'   PinchHitter Subroutine
'------------------------------
Sub PinchHitter (P, P9, W5, W6, idx)

    2807 '
    Call PBP(Q$(0) + " will be replaced by " + B$(P, idx))

    D0%(P) = D0%(P) + 1
    X0%(P, 0, D0%(P)) = idx
    X0%(P, 1, D0%(P)) = B7%(P, B1!(P))
    X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9)
    B%(P, idx, 21) = 1
    B3%(P, B1!(P)) = idx
    B9%(P, B7%(P, B1!(P))) = idx

    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then
        B9%(P, 1) = 99
        B9%(P, 0) = B1!(P)
        B7%(P, B1!(P)) = 10
    End If

    P9 = P
    W5 = P9
    W6 = P9
    VV(P9) = B1!(P)
    VV1(P9) = B1!(P)

    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

    VV(P9) = 1
    VV1(P9) = 9

End Sub

'------------------------------
'   PopUp_Lineup Subroutine
'------------------------------
' ...explanation...
Sub PopUp_Lineup ()

    If usingGfx = 1 Then Screen 0

    ' *** POPUP LINEUPS FOR TEAMS ***
    J = 11

    Locate 8, 7
    Color L%(1, 11), L%(1, 12)
    Print " " + Chr$(214); String$(62, 196); Chr$(183) + " ";

    For II = 1 To J:
        Locate 8 + II, 7
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 9 + J, 7
    Print " " + Chr$(211); String$(62, 196); Chr$(189) + " ";
    '-- the above is obviously just our drawbox routine

    Locate 9, 14: Print A$(0); Tab(40); A$(1)

    For J = 0 To 1

        For X = 1 To 9:

            Locate X + 9, 9 + J * 26
            If B1!(J) = X Then Print "->";

            Locate , 11 + J * 26
            If B7%(J, X) = 1 Then

                Print B1$(P%(J, P1%(J), 0) + 2); Tab(14 + J * 26); Left$(P$(J, P1%(J)), 10); " P"; Tab(28 + J * 26);
                If P%(J, P1%(J), 11) <> 999 Then
                    Print " ."; Using "###"; P%(J, P1%(J), 11) '/ 1000
                Else
                    If P%(J, P1%(J), 20) = 0 Then
                        Print " .000"
                    Else
                        If P%(J, P1%(J), 20) = P%(J, P1%(J), 22) Then
                            Print "1.000"
                        Else
                            Print " ."; Using "###"; P%(J, P1%(J), 22) / P%(J, P1%(J), 20)
                        End If
                    End If
                End If

            Else
                Print B1$(B%(J, B3%(J, X), 0) + 2); Tab(14 + J * 26); Left$(B$(J, B3%(J, X)), 10); " "; C$(B7%(J, X)); Tab(28 + J * 26);

                If B%(J, B3%(J, X), 4) = 0 Then
                    Print " .000"
                Else
                    If B%(J, B3%(J, X), 6) = B%(J, B3%(J, X), 4) Then
                        Print "1.000"
                    Else
                        Print Using " .###"; B%(J, B3%(J, X), 6) / B%(J, B3%(J, X), 4)
                    End If
                End If
            End If

        Next

    Next

    Color 15, 0
    Locate 24, 1: Print "Hit any key to continue";
    I$ = GetKeyPress$

End Sub

'------------------------------
'   PopUp_Batter Subroutine
'------------------------------
' ...explanation...
Sub PopUp_Batter (WK, M)

    If TS%(WK, 0) = 999 Then
        Locate 4, 56: Print "VS LHP  .000";
        If B%(WK, M, 42) <> 0 Then statPCT! = B%(WK, M, 43) / B%(WK, M, 42): Locate 4, 64: Print Using ".###"; statPCT!

        Locate 5, 56: Print "VS RHP  .000";
        If B%(WK, M, 49) <> 0 Then statPCT! = B%(WK, M, 50) / B%(WK, M, 49): Locate 5, 64: Print Using ".###"; statPCT!
    End If

    Locate 8, 6: Print "SEASON";
    Locate , 15

    If B%(WK, M, 4) = 0 Then
        Print ".---  .---"
    Else
        statPCT! = B%(WK, M, 6) / B%(WK, M, 4)
        statPCT2! = (B%(WK, M, 6) + B%(WK, M, 11)) / (B%(WK, M, 4) + B%(WK, M, 11))
        Print Using ".###  .### ### ### ### ### ## ## ## ### ### ### "; statPCT!; statPCT2!; B%(WK, M, 3); B%(WK, M, 4); B%(WK, M, 5); B%(WK, M, 6); B%(WK, M, 7); B%(WK, M, 8); B%(WK, M, 9); B%(WK, M, 10);
        Print Using "### ### ### ##"; B%(WK, M, 11); B%(WK, M, 12); B%(WK, M, 13); B%(WK, M, 14)
    End If

    Locate , 6: Print "LEAGUE";

    If B5%(WK, M, 0) = 0 Or B5%(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If B5%(WK, M, 0) = B5%(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = B5%(WK, M, 1) / B5%(WK, M, 0)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If B5%(WK, M, 0) + B5%(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If B5%(WK, M, 1) + B5%(WK, M, 7) = B5%(WK, M, 0) + B5%(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            statPCT! = (B5%(WK, M, 1) + B5%(WK, M, 7)) / (B5%(WK, M, 0) + B5%(WK, M, 7))
            Locate , 21: Print Using ".###"; statPCT!;
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B5%(WK, M, 14); B5%(WK, M, 0); B5%(WK, M, 2); B5%(WK, M, 1); B5%(WK, M, 4); B5%(WK, M, 5); B5%(WK, M, 6); B5%(WK, M, 3); B5%(WK, M, 7); B5%(WK, M, 8); B5%(WK, M, 9); B5%(WK, M, 10); B5%(WK, M, 12); B5%(WK, M, 13)

    Locate , 6: Print "GAME";

    If B8%(WK, M, 0) = 0 Or B8%(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If B8%(WK, M, 0) = B8%(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = B8%(WK, M, 1) / B8%(WK, M, 0)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If B8%(WK, M, 0) + B8%(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If B8%(WK, M, 1) + B8%(WK, M, 7) = B8%(WK, M, 0) + B8%(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            Locate , 21: Print Using ".###"; (B8%(WK, M, 1) + B8%(WK, M, 7)) / (B8%(WK, M, 0) + B8%(WK, M, 7));
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B8%(WK, M, 14); B8%(WK, M, 0); B8%(WK, M, 2); B8%(WK, M, 1); B8%(WK, M, 4); B8%(WK, M, 5); B8%(WK, M, 6); B8%(WK, M, 3); B8%(WK, M, 7); B8%(WK, M, 8); B8%(WK, M, 9); B8%(WK, M, 10); B8%(WK, M, 12); B8%(WK, M, 13)

End Sub

'------------------------------
'   PopUp_Pitcher Subroutine
'------------------------------
' ...explanation...
Sub PopUp_Pitcher (WK, M)

    Locate 14, 3
    Color L%(WK, 11), L%(WK, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

    For X = 1 To 7: Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " ": Next

    Locate , 3: Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

    If TS%(WK, 0) = 999 Then
        Locate 15, 56: Print "VS LHB  .000";
        If P%(WK, M, 51) <> 0 Then statPCT! = P%(WK, M, 52) / P%(WK, M, 51): Locate 15, 64: Print Using ".###"; statPCT!
        Locate 16, 56: Print "VS RHB  .000";
        If P%(WK, M, 58) <> 0 Then statPCT! = P%(WK, M, 59) / P%(WK, M, 58): Locate 16, 64: Print Using ".###"; statPCT!
    End If

    Locate 15, 6: Print "Stats for "; P$(WK, M)
    Locate , 6: Print "THROWS: "; B1$(P%(WK, M, 0) + 2); "    ";: Print Using "G/A%: ##     HOLD: #"; P%(WK, M, 12); P%(WK, M, 18)
    Locate , 3: Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
    Locate , 6: Print "          W  L SV   ERA  G GS CG   IP    H  BB  SO   R  ER HR"

    Locate , 6: Print "SEASON   ";: Print Using "## ## ## ##.## ## ## ## ###   ### "; P%(WK, M, 1); P%(WK, M, 2); P%(WK, M, 3); P%(WK, M, 10) / 100; P%(WK, M, 4); P%(WK, M, 5); P%(WK, M, 16); P%(WK, M, 6); P%(WK, M, 7);
    Print Using "### ###         ##"; P%(WK, M, 8); P%(WK, M, 9); P%(WK, M, 13)

    Locate , 6: Print "LEAGUE   ";: Print Using "## ## ## "; P5%(WK, M, 6); P5%(WK, M, 7); P5%(WK, M, 12);

    If P5%(WK, M, 3) = 0 Or P5%(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; P5%(WK, M, 3) / (P5%(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; P5%(WK, M, 8); P5%(WK, M, 9); P5%(WK, M, 10);

    If P5%(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(P5%(WK, M, 0) / 3);
        Print ".";: Print Using "#"; P5%(WK, M, 0) - (Int(P5%(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; P5%(WK, M, 1); P5%(WK, M, 4); P5%(WK, M, 5); P5%(WK, M, 2); P5%(WK, M, 3); P5%(WK, M, 14)
    Locate , 6: Print "GAME     ";: Print Using "## ## ## "; P8%(WK, M, 6); P8%(WK, M, 7); P8%(WK, M, 12);
    If P8%(WK, M, 3) = 0 Or P8%(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; P8%(WK, M, 3) / (P8%(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; P8%(WK, M, 8); P8%(WK, M, 9); P8%(WK, M, 10);

    If P8%(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(P8%(WK, M, 0) / 3);
        Print ".";: Print Using "#"; P8%(WK, M, 0) - (Int(P8%(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; P8%(WK, M, 1); P8%(WK, M, 4); P8%(WK, M, 5); P8%(WK, M, 2); P8%(WK, M, 3); P8%(WK, M, 14)

End Sub

'------------------------------
'   POPUPSTATS Subroutine
'------------------------------
' ...explanation...
Sub POPUPSTATS ()

    If usingGfx = 1 Then Screen 0

    J = B3%(P, B1!(P))

    'Stats for Active Batter
    Locate 3, 3: Color L%(P, 11), L%(P, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

    For X = 1 To 7:
        Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " "
    Next

    Locate , 3: Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

    Locate 4, 6: Print "Stats for ";

    If B7%(P, B1!(P)) = 1 Then
        If P%(P, P1%(P), 11) <> 999 Then
            Print P$(P, P1%(P))
        Else
            Print P$(P, P1%(P)): Locate , 6: Print "BATS: "; B1$(P%(P, P1%(P), 0) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; 52; P%(P, P1%(P), 32); P%(P, P1%(P), 31):
        End If
    Else
        Print B$(P, J)
        Locate , 6: Print "BATS: "; B1$(B%(P, J, 0) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; B%(P, J, 18); B%(P, J, 30); B%(P, J, 16)
    End If

    510 '
    If B7%(P, B1!(P)) = 1 And TS%(P, 0) = 999 Then
        Locate 4, 56: Print "VS LHP  .000";
        If P%(P, P1%(P), 37) <> 0 Then statPCT! = P%(P, P1%(P), 38) / P%(P, P1%(P), 37): Locate 4, 64: Print Using ".###"; statPCT!

        Locate 5, 56: Print "VS RHP  .000";
        If P%(P, P1%(P), 44) <> 0 Then statPCT! = P%(P, P1%(P), 45) / P%(P, P1%(P), 44): Locate 5, 64: Print Using ".###"; statPCT!
    End If

    Locate 6, 3: Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
    Locate , 6: Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

    If B7%(P, B1!(P)) = 1 And P%(P, P1%(P), 11) <> 999 Then

        statPCT! = P%(P, P1%(P), 11) '/ 1000
        Locate , 6: Print "SEASON";: Locate , 15: Print Using ".###"; statPCT!

    Else

        If B7%(P, B1!(P)) <> 1 Then
            WK = P
            M = J
            Call PopUp_Batter(WK, M)
            GoTo 621
        Else

            Locate , 6: Print "SEASON";: Locate , 15

            If P%(P, P1%(P), 20) = 0 Then
                Print ".---  .---"
            Else
                PRINT USING ".###  .### ### ### ### ### ## "; P%(P, P1%(P), 22) / P%(P, P1%(P), 20); (P%(P, P1%(P), 22) + P%(P, P1%(P), 27)) / (P%(P, P1%(P), 27) + P%(P, P1%(P), 20)); P%(P, P1%(P), 19); P%(P, P1%(P), 20); P%(P, P1%(P), 21); P%(P, P1%(P), 22);  _
            P%(P, P1%(P), 23);
                Print Using "## ## ### ### ### ### ##"; P%(P, P1%(P), 24); P%(P, P1%(P), 25); P%(P, P1%(P), 26); P%(P, P1%(P), 27); P%(P, P1%(P), 28); P%(P, P1%(P), 29); P%(P, P1%(P), 30)
            End If
        End If
    End If

    Locate , 6: Print "LEAGUE";: J = P1%(P)
    If P5%(P, J, 23) = 0 Or P5%(P, J, 24) = 0 Then
        Locate , 15: Print ".000";
    Else
        If P5%(P, J, 23) = P5%(P, J, 24) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = P5%(P, J, 24) / P5%(P, J, 23)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If P5%(P, J, 23) + P5%(P, J, 30) = 0 Then
        Locate , 21: Print ".000";
    Else
        If P5%(P, J, 24) + P5%(P, J, 30) = P5%(P, J, 23) + P5%(P, J, 30) Then
            Locate , 20: Print "1.000";
        Else
            statPCT! = (P5%(P, J, 24) + P5%(P, J, 30)) / (P5%(P, J, 23) + P5%(P, J, 30))
            Locate , 21: Print Using ".###"; statPCT!;
        End If
    End If

    Locate , 26: Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P5%(P, J, 8); P5%(P, J, 23); P5%(P, J, 25); P5%(P, J, 24); P5%(P, J, 27); P5%(P, J, 28); P5%(P, J, 29); P5%(P, J, 26); P5%(P, J, 30); P5%(P, J, 31); P5%(P, J, 32); P5%(P, J, 33); P5%(P, J, 35); P5%(P, J, 36)
    Locate , 6: Print "GAME";

    If P8%(P, J, 23) = 0 Or P8%(P, J, 24) = 0 Then
        Locate , 15: Print ".000";
    Else
        If P8%(P, J, 23) = P8%(P, J, 24) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = P8%(P, J, 24) / P8%(P, J, 23)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If P8%(P, J, 23) + P8%(P, J, 30) = 0 Then
        Locate , 21: Print ".000";
    Else
        If P8%(P, J, 24) + P8%(P, J, 30) = P8%(P, J, 23) + P8%(P, J, 30) Then
            Locate , 20: Print "1.000";
        Else
            statPCT! = (P8%(P, J, 24) + P8%(P, J, 30)) / (P8%(P, J, 23) + P8%(P, J, 30))
            Locate , 21: Print Using ".###"; statPCT!;
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P8%(P, J, 8); P8%(P, J, 23); P8%(P, J, 25); P8%(P, J, 24); P8%(P, J, 27); P8%(P, J, 28); P8%(P, J, 29); P8%(P, J, 26); P8%(P, J, 30); P8%(P, J, 31); P8%(P, J, 32); P8%(P, J, 33); P8%(P, J, 35); P8%(P, J, 36)

    621 ''
    WK = D
    M = P1%(D)
    Call PopUp_Pitcher(WK, M)

    Locate 24, 1: Print "Hit any key to continue";
    I$ = GetKeyPress$
    Color 7, 0

End Sub

'------------------------------
'   PUTOUT Subroutine
'------------------------------
' ...explanation...
Sub PUTOUT (i, D, P)

    If B7%(P, B0%(i - 1, 0)) = 1 Then
        I5 = P1%(P)
        I6 = 33
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B0%(i - 1, 0))
        I6 = 10
        Call INCBATOFF(P, I5, I6)
    End If

End Sub

'------------------------------
'   d100 Subroutine
'------------------------------
' ...explanation...
Sub d100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub

'------------------------------
'   RecordOut Subroutine
'------------------------------
' ...explanation...
Sub RecordOut (i, I3)

    Call PBP(Q$(i) + " out @ " + H$(I3))

    B0%(i, 0) = 0
    B0%(i, 1) = 0: B0%(i, 2) = 0
    Q$(i) = ""

End Sub

'------------------------------
'   RecordRun Subroutine
'------------------------------
' ...explanation...
Sub RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

    If sah% = 0 Then Call PBP(Q$(i) + " scores")
    sah% = 0

    I5 = B0%(i, 1)
    I6 = 2
    Call ADDPIT(D, I5, I6)

    If P1%(D) <> B0%(i, 1) Then I5 = P1%(D): I6 = 18: Call ADDPIT(D, I5, I6)

    If P6%(P) = 1 Then P8%(P, P1%(P), 20) = P8%(P, P1%(P), 20) + 1

    If B7%(P, B0%(i, 0)) = 1 Then
        I5 = P1%(P)
        I6 = 25
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B0%(i, 0))
        I6 = 2
        Call INCBATOFF(P, I5, I6)
    End If

    If F% = 1 Then
        If O%(0) = 2 Then
            PQ = 8
        Else
            Select Case W%
                Case Is <= 3:
                    If S2% <> 23 Or F% = 1 And W% = 5 And S2% <> 23 Then PQ = 8
                Case 4, 6
                    If INFPOS% = 2 Then PQ = 8
                Case Else
                    If S2% >= 4 And S2% <= 6 Then PQ = 8
            End Select

        End If
    End If

    4220 '
    If PQ <> 8 Then
        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 26
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, B1!(P))
            I6 = 3
            Call INCBATOFF(P, I5, I6)
        End If

        If F% = 1 Then PQ = 8

    End If

    S%(P, INNING%) = S%(P, INNING%) + 1
    S6%(P, 0) = S6%(P, 0) + 1

    If S6%(P, 0) - S6%(D, 0) = 1 Then
        P2%(D) = B0%(i, 1)
        P2%(P) = P1%(P)
        S1%(1) = -1
        S1%(0) = -1
    Else
        If S6%(P, 0) - S6%(D, 0) = 0 And S1%(D) <> -1 Then
            I5 = P1%(D)
            I6 = 19
            Call ADDPIT(D, I5, I6)
            S1%(D) = -1
        End If
    End If

    B0%(i, 0) = 0
    B0%(i, 1) = 0
    B0%(i, 2) = 0
    Q$(i) = ""

    If S%(P, INNING%) = 4 And Int(Rnd(1) * 100) + 1 <= 25 Then Call PBP("They're tearin' up the peapatch!")
    If INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) = 1 And F% = 0 And O%(1) < 3 And S2% <> 10 Then Call CHARGERUN(D, i)
    If INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) = 1 And S2% <> 10 Then A5%(0) = 0: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
End Sub

'------------------------------
'   OPTIONS Subroutine
'------------------------------
' ...explanation...
Sub OPTIONS ()

    Dim U6$(3)

    If usingGfx = 1 Then Cls: Screen 0

    U6$(0) = "HUMAN OPPONENT      "
    U6$(1) = "COMPUTER VISIT TEAM "
    U6$(2) = "COMPUTER HOME TEAM  "
    U6$(3) = "COMPUTER VS COMPUTER"

    If U6 = 2 Then
        U6 = 3
    Else
        If U6 = 1 Then
            If U9 = 1 Then
                U6 = 2
            Else
                If U9 = 0 Then
                    U6 = 1
                End If
            End If
        End If
    End If

    170 Cls

    Do
        171 '
        Color , 0: Locate 3, 1
        Color 14: Print "(1) ";
        Color 15: Print U6$(U6)
        Color 14: Print "(2) ";
        Color 15: Print Using "DISPLAY INTERVAL: #.# "; pbpDelay!
        Color 14: Print "<ENTER> ";
        Color 15: Print "RETURN TO GAME PLAY"

        Do
            183 Color 7, 0
            Locate 24, 1: Print "Hit any key to continue";
            I$ = GetKeyPress$
            I = Val(I$)
            'The 0 is likely there to catch ENTER
            'But honestly will catch any alpha-key
            'Chr$(13) should specifically catch ENTER
        Loop Until I >= 0 And I <= 2

        If I <> 0 Then
            If I = 1 Then U6 = U6 + 1
            If U6 > 3 Then U6 = 0
            If I = 2 Then Locate 4, 22: Input pbpDelay!
        End If

    Loop Until I = 0

    184 If U6 <> 0 Then
        If U6 = 3 Then
            U6 = 2
        Else
            If U6 = 2 Then
                U6 = 1
                U9 = 1
            Else
                U6 = 1
                U9 = 0
            End If
        End If
    End If

    If usingGfx = 1 Then Screen imageScreen&

    185 '
End Sub

'------------------------------
'   CONVNAME Subroutine
'------------------------------
' ...explanation...
Sub CONVNAME (QQ, I)

    N$(I) = A$(I)

    For xx = 4 To 11
        If Mid$(N$(I), xx, 1) = " " Or Mid$(N$(I), xx, 1) = "." Then Mid$(N$(I), xx, 1) = "_"
    Next xx

    QQ = 8

    For xx = 11 To 4 Step -1
        If Mid$(N$(I), xx, 1) = "_" Then
            QQ = xx - 4
        Else
            Exit For
        End If

    Next xx

End Sub

'------------------------------
'   DISPBAVG Subroutine
'------------------------------
' ...explanation...
Sub DISPBAVG (D)

    If INNING% > 5 And S8%(D, 0) < P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) < P8%(D, P1%(D), 2) Then
        Locate 10, 17
        Color 30, 0: Print "-T-"
        Color 15, 0
    End If

    Locate 5, 1

    For X = 1 To 9

        Color 3, 0
        Locate X + 4, 22

        If B1!(P) = X Then Color 11, 0

        Locate , 22

        If B7%(P, X) = 1 Then

            Print B1$(P%(P, P1%(P), 0) + 2); Tab(25); Left$(P$(P, P1%(P)), 10); " P"; Tab(39);

            If P%(P, P1%(P), 11) <> 999 Then
                If P%(P, P1%(P), 11) = 0 Then
                    Print " .000"
                Else
                    Print " ."; Using "###"; P%(P, P1%(P), 11)
                End If

            Else

                If P%(P, P1%(P), 20) = 0 Then
                    Print " .000"
                Else
                    If P%(P, P1%(P), 20) = P%(P, P1%(P), 22) Then
                        Print "1.000"
                    Else
                        pAvg! = (P%(P, P1%(P), 22) / P%(P, P1%(P), 20)) * 1000
                        Print " ."; Using "###"; pAvg!
                    End If

                End If

            End If

        Else

            Print B1$(B%(P, B3%(P, X), 0) + 2); Tab(25); Left$(B$(P, B3%(P, X)), 10); " "; C$(B7%(P, X)); Tab(39);

            If B%(P, B3%(P, X), 4) = 0 Then
                Print " .000"
            Else
                If B%(P, B3%(P, X), 6) = B%(P, B3%(P, X), 4) Then
                    Print "1.000"
                Else
                    bAvg! = (B%(P, B3%(P, X), 6) / B%(P, B3%(P, X), 4)) * 1000
                    Print " ."; Using "###"; bAvg!
                End If
            End If

        End If

    Next

    Color 15, 0

End Sub

'------------------------------
'   GETSTATS Subroutine
'------------------------------
' ...explanation...
Sub GETSTATS ()

    For I = 0 To 1

        Color 15, 0
        Cls

        Call CONVNAME(QQ, I)

        statName$ = Mid$(N$(I), 4, QQ)
        statFile$ = statName$ + "." + Left$(N$(I), 2)

        FILENAME$ = diskPaths$(1) + statFile$

        If _FileExists(FILENAME$) Then
            Open diskPaths$(1) + statFile$ For Input As #1
            Input #1, A$: Input #1, W2%: Input #1, L2%
            For I1 = 0 To 22: Input #1, K$: Input #1, K%: Next I1
            For I1 = 0 To 21: Input #1, K$: Input #1, K%: Next I1
            For I1 = 0 To 22: For I2 = 0 To 21: Input #1, B5%(I, I1, I2): Next: Next
            For I1 = 0 To 21: For I2 = 0 To 41: Input #1, P5%(I, I1, I2): Next: Next
            Close #1
        End If

    Next I
    'If I = 0 Then I = 1: GoTo 100

End Sub

'------------------------------
'   HITTINGLINES Subroutine
'------------------------------
' ...explanation...
Sub HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

    Randomize Timer

    'CLS
    'PRINT "Please Wait..."
    'PRINT
    'PRINT "Calculating Pitcher/Batter Park Effects..."

    For I = 0 To 1

        I1 = 1 - I

        'CALCULATE BASE ON BALLS PERCENTAGES
        For I3 = VV(I) To VV1(I)

            I6! = L%(I1, 3) / 1000

            If P%(I1, P1%(I1), 6) * 3 * .94 + P%(I1, P1%(I1), 7) + P%(I1, P1%(I1), 8) = 0 Then
                I2! = 0
            Else
                I2! = P%(I1, P1%(I1), 8) / (P%(I1, P1%(I1), 6) * 3 * .94 + P%(I1, P1%(I1), 7) + P%(I1, P1%(I1), 8))
                'I2! = P%(I1, P1%(I1), 8) / (((P%(I1, P1%(I1), 6) * 3) * .94) + P%(I1, P1%(I1), 7) + P%(I1, P1%(I1), 8))
            End If

            II = B%(I, B3%(I, I3), 0)
            I4 = P%(I1, P1%(I1), 0)

            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2
            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then I6! = SA%(I1, TY% * 6 + 3) / 1000: I2! = P%(I1, P1%(I1), 56 + PY% * 7) * .9 / (P%(I1, P1%(I1), 51 + PY% * 7) + P%(I1, P1%(I1), 56 + PY% * 7) * .9)

            If B7%(I, I3) = 1 Then
                If P%(I, P1%(I), 11) <> 999 Then
                    I7! = .037
                Else
                    If (P%(I, P1%(I), 27) + P%(I, P1%(I), 20) = 0) Then
                        I7! = 0
                    Else
                        If TS%(I, 0) = 999 Then
                            If P%(I, P1%(I), 37 + BY% * 7) + P%(I, P1%(I), 42 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = P%(I, P1%(I), 42 + BY% * 7) * .9 / (P%(I, P1%(I), 42 + BY% * 7) * .9 + P%(I, P1%(I), 37 + BY% * 7))
                            End If
                        Else
                            I7! = P%(I, P1%(I), 27) / (P%(I, P1%(I), 27) + P%(I, P1%(I), 20))
                        End If
                    End If
                End If
            Else
                If B%(I, B3%(I, I3), 4) + B%(I, B3%(I, I3), 11) = 0 Then
                    I7! = 0
                Else
                    If TS%(I, 0) = 999 Then
                        If B%(I, B3%(I, I3), 47 + BY% * 7) + B%(I, B3%(I, I3), 42 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            If B%(I, B3%(I, I3), 80) <> 0 Then
                                I7! = B%(I, B3%(I, I3), 47 + BY% * 7) * .9 / (B%(I, B3%(I, I3), 42 + BY% * 7) + B%(I, B3%(I, I3), 47 + BY% * 7) * .9)
                            End If
                        End If
                    Else
                        I7! = B%(I, B3%(I, I3), 11) / (B%(I, B3%(I, I3), 4) + B%(I, B3%(I, I3), 11))
                    End If
                End If
            End If

            223 '
            Call NORMALIZE(I7!, I2!, I6!, K8!)
            B4%(I, 5, I3) = K8!

        Next I3

        ' CALCULATE STRIKEOUT PERCENTAGES
        For I3 = VV(I) To VV1(I)

            If P%(I1, P1%(I1), 6) = 0 Then
                I2! = 0
            Else
                I2! = P%(I1, P1%(I1), 9) / ((P%(I1, P1%(I1), 6) * 3) * .94)
            End If

            I6! = L%(I1, 2) / 1000

            II = B%(I, B3%(I, I3), 0)
            I4 = P%(I1, P1%(I1), 0)
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2
            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then
                If P%(I1, P1%(I1), 51 + PY% * 7) - P%(I1, P1%(I1), 52 + PY% * 7) = 0 Then
                    I2! = 0
                Else
                    I2! = P%(I1, P1%(I1), 57 + PY% * 7) / (P%(I1, P1%(I1), 51 + PY% * 7) - P%(I1, P1%(I1), 52 + PY% * 7))
                    I6! = SA%(I1, TY% * 6 + 2) / 1000
                End If
            End If

            224 '
            If B7%(I, I3) = 1 Then
                If P%(I, P1%(I), 11) <> 999 Then
                    I7! = .42
                Else
                    If P%(I, P1%(I), 20) - P%(I, P1%(I), 22) = 0 Then
                        I7! = 0
                    Else
                        If TS%(I, 0) = 999 Then
                            If P%(I, P1%(I), 37 + BY% * 7) - P%(I, P1%(I), 38 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = P%(I, P1%(I), 43 + BY% * 7) / (P%(I, P1%(I), 37 + BY% * 7) - P%(I, P1%(I), 38 + BY% * 7))
                            End If
                        Else
                            If B7%(I, I3) = 1 Then
                                I7! = P%(I, P1%(I), 28) / (P%(I, P1%(I), 20) - P%(I, P1%(I), 22))
                            End If
                        End If
                    End If
                End If
            Else
                If B%(I, B3%(I, I3), 4) = 0 Or B%(I, B3%(I, I3), 4) - B%(I, B3%(I, I3), 6) = 0 Then
                    I7! = 0
                Else
                    If TS%(I, 0) = 999 Then
                        If B%(I, B3%(I, I3), 80) <> 0 Then
                            If B%(I, B3%(I, I3), 42 + BY% * 7) - B%(I, B3%(I, I3), 43 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = B%(I, B3%(I, I3), 48 + BY% * 7) / (B%(I, B3%(I, I3), 42 + BY% * 7) - B%(I, B3%(I, I3), 43 + BY% * 7))
                            End If
                        End If
                    Else
                        I7! = B%(I, B3%(I, I3), 12) / (B%(I, B3%(I, I3), 4) - B%(I, B3%(I, I3), 6))
                    End If
                End If
            End If

            'IF I7!>=1 THEN I7%=.8
            Call NORMALIZE(I7!, I2!, I6!, K8!)
            B4%(I, 4, I3) = K8!

        Next I3

        ' CALCULATE THE BATTING AVERAGE
        For I3 = VV(I) To VV1(I)

            I2! = L%(I1, 1) / 1000
            I6! = P%(I1, P1%(I1), 14) / 1000

            If P%(I1, P1%(I1), 6) + P%(I1, P1%(I1), 7) = 0 Then
                I7! = 0
            Else
                I7! = P%(I1, P1%(I1), 7) / (((P%(I1, P1%(I1), 6) * 3) * .94) + P%(I1, P1%(I1), 7))
            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            i9! = K8! / 1000
            II = B%(I, B3%(I, I3), 0)
            I4 = P%(I1, P1%(I1), 0)
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2
            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then
                I6! = P%(I1, P1%(I1), TY% * 6 + 65) / 1000

                If P%(I1, P1%(I1), 51 + PY% * 7) = 0 Then
                    I7! = 0
                Else
                    I7! = P%(I1, P1%(I1), 52 + PY% * 7) / P%(I1, P1%(I1), 51 + PY% * 7)
                End If

            End If

            225 '
            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then I2! = SA%(I1, TY% * 6 + 1) / 1000
            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then Call NORMALIZE(I7!, I2!, I6!, K8!)
            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then i9! = K8! / 1000

            If B7%(I, I3) = 1 And P%(I, P1%(I), 11) <> 999 Then

                I7! = P%(I, P1%(I), 11) / 1000: I6! = P%(I, P1%(I), 14) / 1000

            Else

                If B7%(I, I3) = 1 And P%(I, P1%(I), 20) = 0 Then
                    I7! = 0
                Else

                    If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then
                        If P%(I, P1%(I), 37 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            I7! = P%(I, P1%(I), 38 + BY% * 7) / P%(I, P1%(I), 37 + BY% * 7)
                        End If
                    Else
                        If B7%(I, I3) = 1 Then I7! = P%(I, P1%(I), 22) / P%(I, P1%(I), 20)
                    End If

                End If

                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then

                    I6! = P%(I, P1%(I), TY% * 6 + 65) / 1000
                Else

                    If B7%(I, I3) = 1 Then
                        I6! = P%(I, P1%(I), 14) / 1000
                    Else
                        If B%(I, B3%(I, I3), 4) = 0 Then
                            I7! = 0
                        Else

                            If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then
                                If B%(I, B3%(I, I3), 42 + BY% * 7) = 0 Then
                                    I7! = 0
                                Else
                                    I7! = B%(I, B3%(I, I3), 43 + BY% * 7) / B%(I, B3%(I, I3), 42 + BY% * 7)
                                End If
                            Else
                                I7! = B%(I, B3%(I, I3), 6) / B%(I, B3%(I, I3), 4)
                            End If

                        End If

                        I6! = B%(I, B3%(I, I3), 20) / 1000
                        If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then I6! = B%(I, B3%(I, I3), 56 + TY% * 6) / 1000

                    End If

                End If

            End If

            228 '
            I2! = L%(I, 1) / 1000
            If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then I2! = SA%(I, TY% * 6 + 1) / 1000
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            I7! = K8! / 1000
            I6! = L%(I, 1) / 1000

            If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then I6! = SA%(I, TY% * 6 + 1) / 1000

            I2! = i9!
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            B4%(I, 0, I3) = K8!

        Next I3

        ' CALCULATE THE EXTRA BASE HITS RATIOS
        For I3 = VV(I) To VV1(I)

            II = B%(I, B3%(I, I3), 0)
            I4 = P%(I1, P1%(I1), 0)

            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2

            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If B7%(I, I3) = 1 And P%(I, P1%(I), 11) <> 999 Then
                B4%(I, 1, I3) = (130 * (i9! * 1000)) / L%(I1, 1)
                B4%(I, 2, I3) = (19 * (i9! * 1000)) / L%(I1, 1)
                B4%(I, 3, I3) = 37
            Else

                E2! = (i9! * 1000) / L%(I1, 1)

                If TS%(I1, 0) = 999 Then E2! = (i9! * 1000) / SA%(I1, TY% * 6 + 1)

                If B7%(I, I3) = 1 And P%(I, P1%(I), 22) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If B7%(I, I3) <> 1 And B%(I, B3%(I, I3), 6) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 And P%(I, P1%(I), 38 + BY% * 7) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then B4%(I, 1, I3) = Int((P%(I, P1%(I), 39 + BY% * 7) / P%(I, P1%(I), 38 + BY% * 7)) * 1000) * E2!: ' doubles times  * .95
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then B4%(I, 2, I3) = Int((P%(I, P1%(I), 40 + BY% * 7) / P%(I, P1%(I), 38 + BY% * 7)) * 1000) * E2!
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then B4%(I, 3, I3) = Int((P%(I, P1%(I), 41 + BY% * 7) / P%(I, P1%(I), 38 + BY% * 7)) * 1000): GoTo 235
                If B7%(I, I3) = 1 Then B4%(I, 1, I3) = Int((P%(I, P1%(I), 23) / P%(I, P1%(I), 22)) * 1000) * E2!: ' doubles times  * .95
                If B7%(I, I3) = 1 Then B4%(I, 2, I3) = Int((P%(I, P1%(I), 24) / P%(I, P1%(I), 22)) * 1000) * E2!
                If B7%(I, I3) = 1 Then B4%(I, 3, I3) = Int((P%(I, P1%(I), 25) / P%(I, P1%(I), 22)) * 1000): GoTo 235

                If B%(I, B3%(I, I3), 6) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235

                If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 And B%(I, B3%(I, I3), 43 + BY% * 7) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then B4%(I, 1, I3) = Int((B%(I, B3%(I, I3), 44 + BY% * 7) / B%(I, B3%(I, I3), 43 + BY% * 7)) * 1000) * E2!: 'see note on pitchers * .95
                If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then B4%(I, 2, I3) = Int((B%(I, B3%(I, I3), 45 + BY% * 7) / B%(I, B3%(I, I3), 43 + BY% * 7)) * 1000) * E2!
                If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) <> 0 Then B4%(I, 3, I3) = Int((B%(I, B3%(I, I3), 46 + BY% * 7) / B%(I, B3%(I, I3), 43 + BY% * 7)) * 1000): GoTo 235

                B4%(I, 1, I3) = Int((B%(I, B3%(I, I3), 7) / B%(I, B3%(I, I3), 6)) * 1000) * E2!: 'see note on pitchers * .95
                B4%(I, 2, I3) = Int((B%(I, B3%(I, I3), 8) / B%(I, B3%(I, I3), 6)) * 1000) * E2!
                B4%(I, 3, I3) = Int((B%(I, B3%(I, I3), 9) / B%(I, B3%(I, I3), 6)) * 1000)

            End If

            235 '
            I6! = L%(I1, 4) / 1000
            I7! = B4%(I, 3, I3) / 1000
            I2! = 0

            If P%(I1, P1%(I1), 7) <> 0 Then I2! = P%(I1, P1%(I1), 13) / P%(I1, P1%(I1), 7)
            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 Then I6! = SA%(I1, TY% * 6 + 4) / 1000
            If TS%(I1, 0) = 999 And P%(I1, P1%(I1), 89) <> 0 And P%(I1, P1%(I1), 52 + PY% * 7) <> 0 Then I2! = P%(I1, P1%(I1), 55 + PY% * 7) / P%(I1, P1%(I1), 52 + PY% * 7)
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            B4%(I, 3, I3) = K8!

        Next I3

        ' ADJUST LEFTY/RIGHTY THE OLD WAY
        I4 = P%(I1, P1%(I1), 0)

        For I3 = VV(I) To VV1(I)

            If TS%(I, 0) = 999 And B%(I, B3%(I, I3), 80) = 0 Then

                I2 = B%(I, B3%(I, I3), 0)

                If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And P%(I1, P1%(I1), 0) = 1 Then A = 1.04: A1 = .88: Call LRADJ(A!, A1!)
                If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = -1 And P%(I1, P1%(I1), 0) = -1 Then A = .93: A1 = 1.16: Call LRADJ(A!, A1!)
                If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And P%(I1, P1%(I1), 0) = -1 Then A = 1.02: A1 = .98: Call LRADJ(A!, A1!)
                If B7%(I, I3) = 1 And P%(I, P1%(I), 0) = 1 And P%(I1, P1%(I1), 0) = 1 Then A = .97: A1 = 1.1: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -2 And I4 = 1 Then A = 1.04: A1 = .96: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -2 And I4 = -1 Then A = .78: A1 = 1.22: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 2 And I4 = 1 Then A = .78: A1 = 1.22: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 2 And I4 = -1 Then A = 1.04: A1 = .96: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -1 And I4 = -1 Then A = .93: A1 = 1.16: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -1 And I4 = 1 Then A = 1.04: A1 = .88: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 1 And I4 = 1 Then A = .97: A1 = 1.1: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 1 And I4 = -1 Then A = 1.02: A1 = .98: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 0 And I4 = -1 Then A = 1.01: A1 = .86: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 0 And I4 = 1 Then A = .99: A1 = .93: Call LRADJ(A!, A1!)

            End If

            B4%(I, 6, I3) = B4%(I, 3, I3)
            B4%(I, 7, I3) = B4%(I, 3, I3)

        Next I3

        ' ADJUST AVERAGES FOR THE HOME TEAM
        For I3 = VV(I) To VV1(I)
            If I = 1 Then B4%(I, 0, I3) = B4%(I, 0, I3) + 5
            If I <> 1 Then B4%(I, 0, I3) = B4%(I, 0, I3) - 5
        Next I3

        ' CALCULATE THE HOME PARK FACTORS
        If N <> 2 Then

            For I2 = VV(I) To VV1(I)
                If I = 1 Then
                    B4%(I, 1, I2) = Int(B4%(I, 1, I2) * (L%(I, 6) / 100) + .5)
                    B4%(I, 2, I2) = Int(B4%(I, 2, I2) * (L%(I, 7) / 100) + .5)
                Else
                    'I <> 1
                    B4%(I, 1, I2) = Int(B4%(I, 1, I2) * (L%(I1, 6) / 100) * ((200 - L%(I, 6)) / 100) + .5)
                    B4%(I, 2, I2) = Int(B4%(I, 2, I2) * (L%(I1, 7) / 100) * ((200 - L%(I, 7)) / 100) + .5)
                End If

                If K9!(I) > 1000 Then I3! = 100: I6! = Int(K9!(I) / 1000) * WF!: I7! = (K9!(I) - Int((K9!(I) / 1000)) * 1000) * WF!
                If K9!(I1) > 1000 Then I4! = 100: K8! = Int(K9!(I1) / 1000) * WF!: i9! = (K9!(I1) - Int((K9!(I1) / 1000)) * 1000) * WF!

                If K9!(I) < 1000 Then I3! = K9!(I) * WF!: I6! = I3!: I7! = I3!
                If K9!(I1) < 1000 Then I4! = K9!(I1) * WF!: K8! = I4!: i9! = I4!

                If I = 1 Then
                    B4%(I, 3, I2) = Int((B4%(I, 3, I2) * (I3! / 100)) + .5)
                    B4%(I, 6, I2) = Int((B4%(I, 6, I2) * (I6! / 100)) + .5)
                    B4%(I, 7, I2) = Int((B4%(I, 7, I2) * (I7! / 100)) + .5)
                Else
                    'I <> 1
                    B4%(I, 3, I2) = Int((B4%(I, 3, I2) * (I4! / 100) * ((200 - I3!) / 100)) + .5)
                    B4%(I, 6, I2) = Int((B4%(I, 6, I2) * (K8! / 100) * ((200 - I6!) / 100)) + .5)
                    B4%(I, 7, I2) = Int((B4%(I, 7, I2) * (i9! / 100) * ((200 - I7!) / 100)) + .5)
                End If

            Next I2

        Else
            Exit For
        End If

    Next I

End Sub

'------------------------------
'   LRADJ Subroutine
'------------------------------
' ...explanation...
Sub LRADJ (A!, A1!)

    For I6 = 0 To 3
        B4%(I, I6, I3) = B4%(I, I6, I3) * A!
    Next I6

    B4%(I, 5, I3) = B4%(I, 5, I3) * A! ' NEWLY ADDED
    B4%(I, 4, I3) = B4%(I, 4, I3) * A1!

End Sub

'------------------------------
'   NORMALIZE Subroutine
'------------------------------
' ...explanation...
Sub NORMALIZE (I7!, I2!, I6!, K8!)

    I0! = (I7! * I2! * (1 - I6!))

    If (I0! + ((1 - I7!) * (1 - I2!) * I6!)) = 0 Then
        K8! = 0
    Else
        K8! = I0! / (I0! + ((1 - I7!) * (1 - I2!) * I6!))
    End If

    K8! = Int((K8! * 1000) + .0005)

End Sub

'------------------------------
'   BASEONBALLS Subroutine
'------------------------------
' ...explanation...
Sub BASEONBALLS (S2%, D2, D, P)

    S2% = 2

    Call WINDEX

    Call d100(BV)
    Call d100(RN)

    If D2 = 2 Then

        If BV <= 50 Then
            Call PBP(Q$(0) + " is walked intentionally...")
        Else
            Call PBP(Q$(0) + " is given a purpose pass...")
        End If

        S8%(D, 0) = S8%(D, 0) + 1

    Else

        If RN <= 30 Then Call PBP("Full count...")

        Call DELIVERY(D)

        pbpIdx% = Int(Rnd(1) * 80) + 780
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)

        pbpIdx% = Int(Rnd(1) * 30) + 860
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)

    End If

    Call d100(RN)

    '-- Where does s4 get a value???
    '-- There is an S4 variable, but
    '--     it's passed as a parameter to
    '--     any routines that need it
    '-- From context of original source,
    '--     it can have a value of 0 to 7

    '-- is s4 the # of "BALLS"?
    '-- theny why look for it to get as high as 7?

    '-- is it a SUM of STRIKES + BALLS?
    '-- it would never get to 7
    If s4% = 7 And RN <= 30 Then Call PBP("that forces a run in")

    RN = Int(Rnd(1) * 4) + 1

    '-- here, if s4 is between 4 and 6, bases are loaded
    '-- in context, this is obviously AFTER the walk
    If s4% > 3 And s4% < 7 Then
        Select Case RN
            Case 1: Call PBP("and the bases are loaded!")
            Case 2: Call PBP("and the sacks are jammed!")
            Case 3: Call PBP("and the bases are full!")
            Case 4: Call PBP("and that loads them up!")
        End Select
    End If

    I5 = P1%(D)
    I6 = 4
    Call ADDPIT(D, I5, I6)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 30
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 7
        Call INCBATOFF(P, I5, I6)
    End If

End Sub

'------------------------------
'   HITBYPITCH Subroutine
'------------------------------
' ...explanation...
Sub HITBYPITCH (S2%, D)
    S2% = 1
    Call WINDEX
    Call DELIVERY(D)

    pbpIdx% = Int(Rnd(1) * 20) + 720
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)

    pbpIdx% = Int(Rnd(1) * 11) + 740
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)

    pbpIdx% = Int(Rnd(1) * 20) + 760
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)

    Call d100(RN)
    If RN <= 50 And s4% = 7 Then Call PBP("tough way to get an RBI")

End Sub

'------------------------------
'   WILDPITCH Subroutine
'------------------------------
' ...explanation...
Sub WILDPITCH (S2%, D)

    S2% = 6
    ruleAsWild = 0

    Call d100(RN)

    Select Case RN
        Case 1 To 16:
            Call PBP("and it gets away from " + B$(D, B9%(D, 2)))
            ruleAsWild = 1
        Case 17 To 32:
            Call PBP("pitch is WAY high, and " + B$(D, B9%(D, 2)) + " can't get it")
            ruleAsWild = 1
        Case 33 To 48:
            Call PBP("It's in the dirt")
            Call PBP("The runners move up 90 ft.")
            ruleAsWild = 1
        Case 49 To 64:
            Call PBP("It bounces in front of the plate")
            Call PBP("it's past the catcher")
            Call PBP("the runners advance")
            ruleAsWild = 1
        Case 65 To 80:
            Call PBP("it's over " + B$(D, B9%(D, 2)) + "'s head")

            '-- This was originally written as:
            '   If RN > 67 Then
            '-- This would clearly work by adding
            '--     the "wild pitch" text to the above.
            '-- But then it's clear that some numbers
            '--     were meant to not append that text.
            '-- For now,I've chosen to remove the overlap
            '-- My working theory is the Case 67+ below
            '--     for crediting the pitch may have been
            '--     duplicated but not fixed
        Case 81 To 100:
            Call PBP("wild pitch by " + P$(D, P1%(D)))
    End Select

    If ruleAsWild = 1 Then
        Call d100(RN)
        Select Case RN
            Case 1 To 33: F$ = "It's ruled a wild pitch"
            Case 34 To 66: F$ = "score it a wild pitch"
            Case 67 To 100: F$ = "credit " + P$(D, P1%(D)) + " with a wild pitch"
        End Select

        Call PBP(F$)
    End If

End Sub


'------------------------------
'   RELIEVERS Subroutine
'------------------------------
' ...explanation...
Sub RELIEVERS ()

    Call PITSTRAT

    If pz = 1 Then
        pz = 0
    Else
        I2 = 0
        A1 = S6%(D, 0) - S6%(P, 0)

        For I = 0 To 21

            A2 = P%(D, I, 4) - P%(D, I, 5) - P%(D, I, 3) * 1.3

            If Not (P%(D, I, 15) > 0 Or P$(D, I) = "XXX" Or A2 = 0) Then
                If INNING% <= 7 And A2 > 0 Then I2 = I2 + A2
                If INNING% > 7 And A1 < 0 And A2 > 0 Then I2 = I2 + A2
                If INNING% > 7 And A1 >= 0 Then I2 = I2 + P%(D, I, 3)
            End If

        Next I

        I3 = Int(Rnd(1) * I2) + 1
        I2 = 0

        For I = 0 To 21

            A2 = P%(D, I, 4) - P%(D, I, 5) - P%(D, I, 3) * 1.3

            If Not (P%(D, I, 15) > 0 Or P$(D, I) = "XXX" Or A2 = 0) Then

                If INNING% <= 7 Then

                    If A2 > 0 Then
                        If I3 <= I2 + A2 Then
                            Call NEWPITCHER
                            Exit Sub
                        Else
                            I2 = I2 + A2
                        End If
                    End If

                Else

                    'INNING% > 7
                    If A1 < 0 And A2 > 0 Then
                        If I3 <= I2 + A2 Then
                            Call NEWPITCHER
                            Exit Sub
                        Else
                            I2 = I2 + A2
                        End If
                    Else
                        If A1 >= 0 And I3 > I2 Then
                            If I3 <= I2 + P%(D, I, 3) Then
                                Call NEWPITCHER
                                Exit Sub
                            Else
                                I2 = I2 + P%(D, I, 3)
                            End If
                        End If
                    End If

                End If

            End If

        Next I

        For I = 0 To 21
            If P%(D, I, 15) = 0 And P$(D, I) <> "XXX" Then Call NEWPITCHER: Exit For
        Next I

    End If

End Sub

'------------------------------
'   REPLACEMENTS Subroutine
'------------------------------
' ...explanation...
Sub REPLACEMENTS (I7%, I8%, i9%)
    ' REPLACEMENT AVERAGE CALCS, I7%,I8%,I9%
    For xx = 22 To 25
        If B7%(I7%, I8%) = B%(I7%, i9%, xx) Then
            B%(I7%, i9%, 15) = B%(I7%, i9%, 33 + (xx - 22) * 2)
            B%(I7%, i9%, 17) = B%(I7%, i9%, 34 + (xx - 22) * 2)
            B%(I7%, i9%, 19) = B%(I7%, i9%, xx + 4)
            Exit Sub
        End If
    Next

    J1 = B7%(I7%, I8%)
    Select Case J1
        Case 2
            J2 = 1
            J3 = 0
            J4 = 910 + Int(Rnd(1) * 10) + 1
        Case 3
            J2 = 0
            J3 = 3
            J4 = 960 + Int(Rnd(1) * 10) + 1
        Case 4
            J2 = 0
            J3 = 3
            J4 = 912 + Int(Rnd(1) * 10) + 1
        Case 5 To 6
            J2 = 0
            J3 = 3
            J4 = 902 + Int(Rnd(1) * 10) + 1
        Case Is >= 7:
            J2 = 2
            J3 = 2
            J4 = 972 + Int(Rnd(1) * 10) + 1
    End Select

    B%(I7%, i9%, 15) = J2
    B%(I7%, i9%, 17) = J3
    B%(I7%, i9%, 19) = J4

End Sub

'------------------------------
'   SCOREBOARD Subroutine
'------------------------------
' ...explanation...
Sub SCOREBOARD ()

    ' ***INCREMENT THE HIT TOTAL ON THE SCOREBOARD***
    S6%(P, 1) = S6%(P, 1) + 1

    If usingGfx = 0 Then
        Color 14, 0
        Locate P + 2, 43
        Print Using "##"; S6%(P, 1);
        Color 15, 0
    Else
        FORMATTED$ = FUsing$(Str$(S6%(P, 1)), "##", 1)
        PrintOnGfx 56, P + 28, FORMATTED$, imageScreen&
    End If

End Sub

'------------------------------
'   SECOND2FIRST Subroutine
'------------------------------
' ...explanation...
Sub SECOND2FIRST (D)

    ' *** throw to 1st from 2nd ***
    pbpIdx% = Int(Rnd(1) * 30) + 1540
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$ + " to first")

End Sub

'------------------------------
'   SINGLEPBP Subroutine
'------------------------------
' ...explanation...
Sub SINGLEPBP (S2%, D, W%)
    ' *** play by play for a single s2%=7, s2%=12 ***

    If S2% = -12 Then
        S2% = 12
    Else
        Call DELIVERY(D)
        Call d100(RN)

        If S2% = 12 Then
            Call PBP(Q$(0) + " beats out an infield single")
        Else

            If RN <= 50 Then
                pbpIdx% = Int(Rnd(1) * 50) + 1610
                Call GETRPBP(pbpIdx%, Y$, D)
                Call STRIPRPBP(Y$, pbpIdx%, D)
                Call PBP(Y$ + " " + C$(W%))
            Else
                If W% = 7 Then pbpIdx% = Int(Rnd(1) * 70) + 1660
                If W% = 9 Then pbpIdx% = Int(Rnd(1) * 70) + 1730
                If W% = 8 Then pbpIdx% = Int(Rnd(1) * 90) + 1800
                Call GETRPBP(pbpIdx%, Y$, D)
                Call STRIPRPBP(Y$, pbpIdx%, D)
                Call PBP(Y$)
            End If

        End If 'Done checking for S2%

        Call DELAY

    End If 'Done checking for S2%

End Sub

'------------------------------
'   SINGLEROUTINE Subroutine
'------------------------------
' ...explanation...
Sub SINGLEROUTINE (W%, P, I1, D, S2%, P2)

    Call GETOUTFIELDER(W%, P, I1, D)
    Call WINDEX

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
        I6 = 1
        Call INCBATOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 1
    Call ADDPIT(D, I5, I6)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
        I6 = 24
        Call INCPITOFF(P, I5, I6)
    End If

    '-- Original code is as follows:
    'If H7% > sn Then Call SINGLEPBP(S2%, D, W%)
    '-- But there is no clue what sn is supposed to be
    Call SINGLEPBP(S2%, D, W%)

    For I = 0 To 3
        A5%(I) = 1
    Next I

    If S2% <> 12 And B0%(2, 0) <> 0 Then

        Call d100(H6%)

        If B7%(P, B0%(2, 0)) <> 1 Then
            BR% = B%(P, B3%(P, B0%(2, 0)), 16)
        Else
            BR% = P%(P, P1%(P), 31)
        End If

        If O%(0) = 0 Then
            If W% = 7 Then I3 = 16 + BR% * 3
            If W% = 8 Then I3 = 49 + BR% * 3
            If W% = 9 Then I3 = 27 + BR% * 3
        End If

        If O%(0) = 1 Then
            If W% = 7 Then I3 = 36 + BR% * 3
            If W% = 8 Then I3 = 61 + BR% * 3
            If W% = 9 Then I3 = 44 + BR% * 3
        End If

        If O%(0) = 2 Then
            If W% = 7 Then I3 = 79 + BR% * 3
            If W% = 8 Then I3 = 82 + BR% * 3
            If W% = 9 Then I3 = 79 + BR% * 3
        End If

        If H6% <= I3 Or P2 = 2 And S2% <> 12 Then A5%(2) = 2

    End If

    If Not (B0%(1, 0) = 0 Or S2% = 12 Or B0%(2, 0) > 0 And A5%(2) = 1) Then

        Call d100(H6%)

        BR% = B%(P, B3%(P, B0%(1, 0)), 16)

        If B7%(P, B0%(1, 0)) = 1 Then BR% = P%(P, P1%(P), 31)

        I3 = 0

        If O%(0) = 0 Then
            If W% = 7 Then I3 = 6 + BR% * 3
            If W% = 8 Then I3 = 25 + BR% * 3
            If W% = 9 Then I3 = 38 + BR% * 3
        End If

        If O%(0) = 1 Then
            If W% = 7 Then I3 = 10 + BR% * 3
            If W% = 8 Then I3 = 26 + BR% * 3
            If W% = 9 Then I3 = 40 + BR% * 3
        End If

        If O%(0) = 2 Then
            If W% = 7 Then I3 = 11 + BR% * 3
            If W% = 8 Then I3 = 45 + BR% * 3
            If W% = 9 Then I3 = 62 + BR% * 3
        End If

        If H6% <= I3 Or P2 = 2 And S2% <> 12 Then A5%(1) = 2

    End If

    Call ADVANCEMENT(P, D, S2%, F%, I3)

End Sub

'------------------------------
'   SOMESTATS Subroutine
'------------------------------
' ...explanation...
Sub SOMESTATS (V, P, F%, D)

    If B0%(V, 0) > 0 And F% = 0 Then
        If B7%(P, B0%(V, 0)) = 1 Then
            I5 = P1%(P)
            I6 = 32
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, B0%(V, 0))
            I6 = 9
            Call INCBATOFF(P, I5, I6)
        End If
    End If

End Sub

'------------------------------
'   SQUEEZECHOICE Subroutine
'------------------------------
' ...explanation...
Sub SQUEEZECHOICE (SH)

    If U6 = 2 Or U6 = 1 And P = U9 Then
        '-- Autoplay essentially
        '-- always does a Bunt rather
        '-- than ever attempt a squeeze

        '-- Consider updating to run
        '-- a squeeze play a small %
        '-- of time, and only in the
        '-- most common conditions:
        '   Team has < 2 outs
        '   Runner is on 3rd

        '-- This could be done even more
        '-- often for teams before the
        '-- 1970's DH rule, since bunting
        '-- is not as frequent now
        SH = 2
    Else
        Locate 15, 1
        Print "1 - SQUEEZE PLAY"
        Print "2 - BUNT FOR SACRIFICE/HIT"
        Do
            i$ = GetKeyPress$
            SH = Val(i$)
        Loop Until SH >= 1 And SH <= 2
    End If

End Sub

'------------------------------
'   STEAL3RD Subroutine
'------------------------------
' ...explanation...
Sub STEAL3RD (TGTBASE%, AA, Q0, H0%, D)

    If TGTBASE% = 3 Then
        ' *** SUCCESS OF 3B STEAL ***
        If AA >= 24 Then
            Q0 = 830
        Else
            Q0 = 700
        End If
    End If

    If TGTBASE% = 4 Then Q0 = Q0 * .5

    ' *** H0%=0, runner not held ***
    If H0% = 0 Then
        Q0 = Q0 + 50
    Else
        ' *** H0% = 1, runner is held ***
        Q0 = Q0 - ((P%(D, P1%(D), 18) - 5) * 25): ' - 50
    End If

End Sub

'------------------------------
'   STEALCHANCE Subroutine
'------------------------------
' ...explanation...
Sub STEALCHANCE (AA, I2, TGTBASE%, CHANCE%)

    Dim DD As Single
    Dim TT As Single
    Dim RR As Single
    Dim SS As Single
    Dim GG As Single
    Dim I1 As Single

    AA = B%(P, I2, 13)
    BB = B%(P, I2, 13) + B%(P, I2, 14)
    CC = B%(P, I2, 7): ' *** CHANCE TO STEAL 2B & 3B ***

    Select Case AA
        Case Is <= 43: DD = .16
        Case 44 To 60: DD = .21
        Case Else: DD = .3
    End Select

    TT = CC + BB - (BB * DD): ' *** TOTAL CHANCES TO STEAL 3RD
    RR = (TT * .19) * 1.4: ' *** "      "      "   "     "  1 OUT
    SS = (TT * .56) * 1.4: ' ***                            0 OUT
    GG = RR + SS: ' ***                           (0,1 OUT)

    If TGTBASE% <> 3 Then

        FF = B%(P, I2, 6) + B%(P, I2, 11) - B%(P, I2, 9)

        If FF = 0 Then
            I1 = 0
        Else
            If AA >= 12 Then I1 = Int(((BB - BB * DD) / FF) * 1500)
            If AA < 12 Then I1 = Int((BB / FF) * 1500)
            If I1 < 0 Then I1 = 0
        End If

    Else
        If BB < 10 And O%(0) = 0 Then I1 = 0
        If BB < 10 And O%(0) = 1 Then I1 = 10
        If O%(0) = 2 Or TT = 0 Then I1 = 0
        If BB >= 10 And O%(0) = 1 Then I1 = BB * DD / TT * 1000
        If BB >= 10 And O%(0) = 0 Then I1 = BB * DD / TT * 1000
        If I1 < 0 Then I1 = 0
    End If

    CHANCE% = I1

End Sub

'------------------------------
'   STRIKEOUT Subroutine
'------------------------------
' ...explanation...
Sub STRIKEOUT (U%, D)

    ' *** strikeout pbp ***
    If U% = 3 Then
        BV = 20
    Else
        BV = Int(Rnd(1) * 100)
        Call d100(RN)
        If RN <= 20 Then Call PBP("FULL COUNT...")
        Call DELIVERY(D)

        If BV = 1 Then
            Call PBP("  Three!!")
            Call PBP(Q$(0) + " turns to " + U$(1))
            Call PBP("he says he tees them up higher than that")
            Call PBP("at the country club!!")
        End If

        If BV = 2 Then
            Call PBP("Strike three...")
            Call PBP(Q$(0) + " stands there like a house")
            Call PBP("by the side of the road")
            Call DELAY
        End If

        If BV > 2 And BV <= 5 Then
            Call PBP(Q$(0) + " looks at a called strike three")
            Call PBP("and turns to glare at " + U$(1))
            Call PBP(M$(P) + " is barking from the dugout!!")
        End If

        Call d100(RN)
        If RN <= 25 Then
            pbpIdx% = Int(Rnd(1) * 40) + 890
        Else
            pbpIdx% = Int(Rnd(1) * 80) + 930
        End If

        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)

    End If

    pbpIdx% = Int(Rnd(1) * 40) + 1010
    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$)

    Call d100(RN)
    Select Case RN
        Case 1 To 20: F$ = "Number" + Str$(P8%(D, P1%(D), 5) + 1) + " for " + P$(D, P1%(D))
        Case 21 To 40: F$ = "That's" + Str$(P8%(D, P1%(D), 5) + 1) + " for " + P$(D, P1%(D))
        Case 41: F$ = Q$(0) + " is not a happy man"
        Case 42: F$ = Q$(0) + " has had better AB's"
    End Select

    Call PBP(F$)

End Sub

'------------------------------
'   STRIKEOUTCS Subroutine
'------------------------------
' ...explanation...
Sub STRIKEOUTCS (SO)

    If SO = 7 Then B1!(P) = B1!(P) + 1

End Sub

'------------------------------
'   TAGADVANCE Subroutine
'------------------------------
' ...explanation...
Sub TAGADVANCE (S2%, I8!, D)

    If S2% = 7 And I8! = 2 Then
        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 27
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, B1!(P))
            I6 = 4
            Call INCBATOFF(P, I5, I6)
        End If
    End If

    If S2% = 8 And I8! = 3 Then
        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 28
            Call INCPITOFF(P, I5, I6)

            P8%(P, P1%(P), 27) = P8%(P, P1%(P), 27) - 1
            P5%(P, P1%(P), 27) = P5%(P, P1%(P), 27) - 1
            Exit Sub
        Else
            I3 = 4: I4 = 5: I5 = B3%(P, B1!(P))
        End If
    End If

    If S2% = 9 And I8! = 4 Then
        I3 = 5
        I4 = 6
        I5 = B3%(P, B1!(P))
    End If

    If S2% = 8 And I8! = 3 Or S2% = 9 And I8! = 4 Then
        If P%(D, P1%(D), 0) <> 1 Then
            CSS%(P, I5, I3 - 2) = CSS%(P, I5, I3 - 2) - 1
            CSS%(P, I5, I4 - 2) = CSS%(P, I5, I4 - 2) + 1
        Else
            B8%(P, I5, I3) = B8%(P, I5, I3) - 1
            B5%(P, I5, I3) = B5%(P, I5, I3) - 1
            B8%(P, I5, I4) = B8%(P, I5, I4) + 1
            B5%(P, I5, I4) = B5%(P, I5, I4) + 1
        End If
    End If

End Sub

'------------------------------
'   THROW2FIRST Subroutine
'------------------------------
' ...explanation...
Sub THROW2FIRST (D)

    ' *** throw to 1st from 3rd/ss ***

    pbpIdx% = Int(Rnd(1) * 40) + 1570

    Call GETRPBP(pbpIdx%, Y$, D)
    Call STRIPRPBP(Y$, pbpIdx%, D)
    Call PBP(Y$ + " to first")

End Sub


'------------------------------
'   TIRING Subroutine
'------------------------------
' ...explanation...
Sub TIRING (P9)
    'tired on runs and hits

    If S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) And S8%(P9, 1) <= P8%(P9, P1%(P9), 2) Then

        Call d100(RN)

        If S8%(P9, 1) > 0 And INNING% < 6 Or S8%(P9, 1) = 0 And P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 4 Then
            If RN <= 50 Then
                Print P$(P9, P1%(P9)); " just doesn't have it today";
            Else
                Print P$(P9, P1%(P9)); " ain't got nuthin' on his fastball folks!";
            End If
        Else
            Print P$(P9, P1%(P9)); " is tired!!";
        End If

    Else

        If S8%(P9, 1) > 0 And (S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) Or S8%(P9, 1) <= P8%(P9, P1%(P9), 2)) Or (P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 2 And P8%(P9, P1%(P9), 0) >= 5) Then
            Print P$(P9, P1%(P9)); " is tiring!!";
        Else

            If S8%(P9, 1) > 0 Or (P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) < 3 And P8%(P9, P1%(P9), 2) = 0) Then
                Print P$(P9, P1%(P9)); " seems to be OK!!";
            End If

        End If

    End If

End Sub

'------------------------------
'   TRIPLEPLAY Subroutine
'------------------------------
' ...explanation...
Sub TRIPLEPLAY (D)

    Call WINDEX

    Q$(4) = B$(D, B9%(D, 5))

    Call PBP("grounder to " + Q$(4) + " @ 3B")
    Call PBP(Q$(2) + " OUT @ 3B")
    Call PBP(Q$(1) + " OUT @ 2B")
    Call PBP(Q$(0) + " OUT @ 1B")
    Call PBP("--- TRIPLE PLAY !!! ---"): Call DELAY

    O%(0) = 3
    I5 = P1%(D)
    I6 = 0

    For i = 1 To 3: Call ADDPIT(D, I5, I6): Next i

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
    End If

End Sub

'------------------------------
'   TRYFOREXTRABASE Subroutine
'------------------------------
' ...explanation...
Sub TRYFOREXTRABASE (LN%, S2%, P2, Z2%, D, W%, sah%, SB%, I8!, I9)

    If LN% = 3308 Then

        LN% = 0

    Else

        I8! = 0

        Call d100(H6%)

        ' B3%(P, ...) is -1
        If S2% = 8 And B0%(2, 0) > 0 And H6% < 6 And B0%(3, 0) = 0 And B%(P, B3%(P, B0%(2, 0)), 8) > 0 Then I8! = 3
        If S2% = 9 And B0%(3, 0) > 0 And H6% <= 2 And B%(P, B3%(P, B0%(3, 0)), 9) > 0 Then I8! = 4
        If S2% = 7 And B0%(3, 0) > 0 And P2 = 2 And H6% >= 6 And H6% <= B%(P, B3%(P, B0%(3, 0)), 16) Then I8! = 4
        If S2% = 7 And B0%(3, 0) > 0 And Z2% = B0%(3, 0) And P2 <> 2 And H6% <= B%(P, B3%(P, B0%(3, 0)), 16) * 12 Then I8! = 4

        E2% = 10

        If B0%(2, 0) > 0 Then E2% = E2% + B%(P, B3%(P, B0%(2, 0)), 16)
        If S2% = 7 And B0%(2, 0) > 0 And P2 <> 2 And H6% <= E2% And B0%(3, 0) = 0 And I8! = 0 Then I8! = 3
        If S2% = 7 And B0%(1, 0) > 0 And B0%(2, 0) = 0 And I8! = 0 And H6% <= B%(P, B3%(P, B0%(1, 0)), 16) And B%(P, B3%(P, B0%(1, 0)), 7) > 0 Then I8! = 2

        If I8! = 0 Then Exit Sub

    End If

    '3308
    If Not (U6 = 2 Or U6 = 1 And P = U9) Then
        If S2% < 10 Then Call PBP(Q$(I8! - 1) + " try for " + H$(I8!) + " (YN) ")
        If S2% > 10 Then Call PBP(Q$(I8! - 1) + " tag up (YN) ")
        Call PBP("against " + Q$(4))
    End If

    3310 '
    i$ = "N"
    A1 = S6%(P, 0) - S6%(D, 0)

    If (U6 = 2 Or U6 = 1 And P = U9) Then
        If A1 > -2 And A1 < 2 And S2% <> 19 Then
            i$ = "Y"
        Else
            If A1 < 0 And (INNING% >= 7 Or S2% = 19) Then
                i$ = "N"
            End If
        End If
    Else
        If S2% = 8 And O%(0) = 2 And I8! = 3 And (U6 = 2 Or U6 = 1 And P = U9) Then
            i$ = "N"
        End If
    End If

    skipThrow% = 0
    If Not (U6 = 2 Or U6 = 1 And P = U9) Then
        i$ = GetKeyPress$
    End If

    If UCase$(i$) <> "Y" Then

        I9 = 1

    Else

        F$ = Q$(I8! - 1) + " is "

        If S2% < 10 Then Call PBP(F$ + "going for " + H$(I8!))
        If S2% > 10 Then Call PBP(F$ + "tagging")

        If Not (S2% = 7 And I8! = 2 Or S2% > 10 And S4% = 3) Then

            If Not (U6 = 2 Or U6 = 1 And D = U9) Then
                Call PBP(Q$(4) + " throw to " + H$(I8!) + " (YN) ")
            End If

            Call d100(RN)

            If (U6 = 2 Or U6 = 1 And D = U9) And MG%(D, 766) = 999 And I8! = 4 Then
                If INNING% >= MG%(D, 767) And MG%(D, 767) <> 0 And MG%(D, 768) <= S6%(D, 0) - S6%(P, 0) Then
                    i$ = "N"
                Else
                    If INNING% >= MG%(D, 769) And MG%(D, 769) <> 0 And MG%(D, 770) <= S6%(D, 0) - S6%(P, 0) Then
                        i$ = "N"
                    End If
                End If
            Else
                If U6 = 2 Or U6 = 1 And D = U9 Then
                    i$ = "Y"
                Else
                    i$ = GetKeyPress$
                End If
            End If

            I9 = 0

            If UCase$(i$) <> "Y" Then
                I9 = 1
                Call PBP("No throw is made")
                skipThrow% = 1
            End If

        End If

        If skipThrow% = 0 Then
            Call WINDEX
            Call PBP("Here comes the throw by " + Q$(4))
            'For i = 1 To 3: Call PBP("."): Next i: Call PBP(" ")

            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)

            If F% <> 1 Then

                I4 = Int(Rnd(1) * 1000) + 1
                A = B%(P, B3%(P, B0%(I8! - 1, 0)), 16)

                If B7%(P, B0%((I8! - 1), 0)) = 1 Then A = P%(P, P1%(P), 31)
                A = A - 5
                A = A * 50

                If S2% = 17 Then b = ((B%(D, B9%(D, W%), 15) - 5) * 25): I3 = 1000 + A - b: ' WAS 800
                If S2% = 18 Then b = ((B%(D, B9%(D, W%), 15) - 5) * 25): I3 = 800 + A - b: ' WAS 650
                If S2% = 19 Or S2% < 10 Then b = ((B%(D, B9%(D, W%), 15) - 5) * 25): I3 = 500 + A - b

                If I4 > I3 Then

                    A5%(I8! - 1) = 11
                    O%(0) = O%(0) + 1
                    O%(1) = O%(1) + 1
                    I5 = P1%(D)
                    I6 = 0
                    Call ADDPIT(D, I5, I6)
                    Call d100(RN)

                    Select Case RN
                        Case 1 To 4
                            If I8! = 4 Then
                                Call PBP(Q$(I8! - 1) + " collides with " + B$(D, B9%(D, 2))): Call PBP(B$(D, B9%(D, 2)) + " hangs on!!")
                            End If

                        Case 5 To 50
                            If I8! <> 4 Then Call PBP(Q$(I8! - 1) + " doesn't make it!")
                        Case 51 To 80
                            Call PBP("throw is perfect!")
                        Case 81 To 90
                            Call PBP("This will be close")
                        Case 91 To 100
                            Call PBP("Close play.....")
                            If I8! < 4 Then
                                Call PBP(U$(I8! + 1) + " makes the call...")
                            Else
                                Call PBP(U$(1) + " makes the call...")
                            End If
                    End Select

                    Call CREDITASSIST(W%, D)
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call CREDITPUTOUT(2, FB2P%, AST%, D)
                    W% = 2

                    If O%(0) = 3 Or S2% > 10 Then Exit Sub

                    3371 '
                    Call d100(I5)

                    A1 = S6%(P, 0) - S6%(D, 0)

                    If Not (A1 < -2 Or A1 < 0 And INNING% > 8) Then

                        ' *** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
                        If I8! - 1 > 2 And I5 <= 15 And B0%(2, 0) > 0 Then
                            Call CREDITASSIST(W%, D)
                            Call PBP("runner is trying for 3rd...")
                            A5%(2) = 11
                            O%(0) = O%(0) + 1
                            O%(1) = O%(1) + 1
                            W% = 5
                            Call CREDITREGPO(W%, D)
                            I5 = P1%(D)
                            I6 = 0
                            Call ADDPIT(D, I5, I6)
                        End If

                        ' *** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***
                        Call d100(I5)

                        If I8! - 1 > 1 And I5 <= 15 And B0%(1, 0) > 0 And A5%(2) <> 11 Then
                            Call CREDITASSIST(W%, D)
                            W% = 6
                            Call CREDITREGPO(W%, D)
                            Call PBP("runner is trying for 2nd...")
                            A5%(1) = 11
                            O%(0) = O%(0) + 1
                            O%(1) = O%(1) + 1
                            I5 = P1%(D)
                            I6 = 0
                            Call ADDPIT(D, I5, I6)
                        End If

                        Call d100(I5)

                        If B0%(2, 0) > 0 And I8! - 1 > 2 And A5%(2) <> 11 And I5 > (80 - B%(P, B3%(P, B0%(2, 0)), 16) * 2) And I9 = 0 Then
                            A5%(2) = 1
                            Call PBP("runner is trying for 3rd...")
                            Call PBP("He's safe!")
                        End If

                        I5 = Int(Rnd(1) * 10) + 1

                        If B0%(1, 0) > 0 And A5%(1) <> 11 And (S4% = 4 Or S4% = 7 And I8! - 1 > 1 And I5 > (10 - B%(P, B3%(P, B0%(1, 0)), 16)) And I9 = 0) Then
                            A5%(1) = 1
                            Call PBP("runner is trying for 2nd...")
                            Call PBP("He's in there!")
                        Else

                            Call d100(I5)

                            If B0%(1, 0) > 0 And I8! - 1 > 1 And I5 > (80 - B%(P, B3%(P, B0%(1, 0)), 16) * 2) And I9 = 0 And A5%(1) <> 11 Then
                                A5%(1) = 1
                                Call PBP("runner safe at 2nd on the throw")
                            End If

                        End If

                    End If

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    I9 = 0

                    Exit Sub

                End If

            End If

            3380 '
            A5%(I8! - 1) = 1

            If I9 <> 1 Then

                Call d100(RN)

                Select Case RN
                    Case 1 To 4:
                        If I8! = 4 Then
                            Call PBP(Q$(I8! - 1) + " collides with " + B$(D, B9%(D, 2)))
                            Call PBP("He knocks the ball loose!!")
                        End If

                    Case 5 To 50:
                        If I8! <> 4 Then Call PBP(Q$(I8! - 1) + " beats the throw!")

                    Case 51 To 60:
                        Call PBP(Q$(I8! - 1) + " is ahead of the throw")

                    Case 61 To 80:
                        Call PBP("throw is off line")

                    Case 81 To 90:
                        Call PBP(Q$(I8! - 1) + " slides under the tag")

                    Case 91 To 100:
                        Call PBP("Close play...")

                        If I8! <> 4 Then
                            Call PBP(U$(I8! + 1) + " makes the call...")
                        Else
                            Call PBP(U$(1) + " makes the call...")
                        End If

                End Select

            End If

            3379 '
            Call PBP(Q$(I8! - 1) + " is safe @ " + H$(I8!))

            'sah% = safeAtHome ?
            If I8! = 4 Then sah% = 1

            Call TAGADVANCE(S2%, I8!, D)

            If S2% < 10 Then 3371

            If S2% > 10 Then

                If B7%(P, B1!(P)) = 1 Then

                    P8%(P, P1%(P), 23) = P8%(P, P1%(P), 23) - 1
                    P5%(P, P1%(P), 23) = P5%(P, P1%(P), 23) - 1
                    I5 = P1%(P)
                    I6 = 41
                    Call INCPITOFF(P, I5, I6)
                    I5 = P1%(D)
                    I6 = 16
                    Call ADDPIT(D, I5, I6)

                Else

                    B8%(P, B3%(P, B1!(P)), 0) = B8%(P, B3%(P, B1!(P)), 0) - 1
                    B5%(P, B3%(P, B1!(P)), 0) = B5%(P, B3%(P, B1!(P)), 0) - 1

                    If P%(D, P1%(D), 0) <> 1 Then
                        CSS%(P, B3%(P, B1!(P)), 0) = CSS%(P, B3%(P, B1!(P)), 0) - 1
                    End If

                    I5 = B3%(P, B1!(P))
                    I6 = 19
                    Call INCBATOFF(P, I5, I6)

                    I5 = P1%(D)
                    I6 = 16
                    Call ADDPIT(D, I5, I6)

                    GoTo 3371

                End If

            End If

            'Loop Until S2% >= 10 And B7%(P, B1!(P)) = 1

        End If 'Done checking for fielding error

    End If 'Done checking if we're going to try for base

End Sub

'------------------------------
'   VIEWTEAM Subroutine
'------------------------------
' ...explanation...
Sub VIEWTEAM ()

    If usingGfx = 1 Then Screen 0

    Do
        Cls
        Color 12: Print Tab(20); "SELECT TEAM TO VIEW DETAILED STATS FOR:"
        Print
        Print
        Color 14, 0: Print "(0) ";: Color L%(0, 11), L%(0, 12): Print A$(0)
        Print
        Color 14, 0: Print "(1) ";: Color L%(1, 11), L%(1, 12): Print A$(1)
        Print
        Color 14, 0: Print "(2) ";: Color 15: Print "RETURN TO PLAY"

        Do
            i$ = GetKeyPress$
            WK = Val(i$)
        Loop Until WK >= 0 And WK <= 2

        If WK < 2 Then

            Cls
            Color 14: Print "(1) ";: Color 15: Print "VIEW PITCHERS"
            Print
            Color 14: Print "(2) ";: Color 15: Print "VIEW HITTERS"
            Print
            Color 14: Print "(3) ";: Color 15: Print "RETURN TO PLAY"

            Do
                i$ = GetKeyPress$
                Q = Val(i$)
            Loop Until Q >= 1 And Q <= 3

            Select Case Q
                Case 1

                    Do

                        Cls
                        Color 12: Print "VIEW BATTERS (AVAILABLE PLAYERS HIGHLIGHTED)"

                        For i = 0 To 10:
                            Color 3, 0
                            If B%(WK, i, 21) = 0 Then Color 11
                            Locate i + 5, 5: Print Using "##"; i;: Print " "; B$(WK, i); Tab(40);
                            Color 3
                            If B%(WK, i + 12, 21) = 0 Then Color 11
                            Print Using "##"; i + 12;: Print " "; B$(WK, i + 12)
                            Color 3
                        Next

                        If B%(WK, 11, 21) = 0 Then Color 11

                        Locate , 5: Print Using "##"; 11;: Print " "; B$(WK, 11)

                        Color 3

                        Do
                            Color 15
                            Locate 20, 1: Print "# TO VIEW (0-22) (23 TO CONTINUE) ";: Input M
                        Loop Until M >= 0 And M <= 23

                        If M <> 23 Then
                            Locate 3, 3
                            Color L%(WK, 11), L%(WK, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

                            For X = 1 To 7: Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " ": Next

                            Locate , 3
                            Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

                            Locate 4, 6: Print "Stats for "; B$(WK, M)
                            Locate , 6: Print "BATS: "; B1$(B%(WK, M, 0) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; B%(WK, M, 18); B%(WK, M, 30); B%(WK, M, 16)
                            Locate , 3: Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
                            Locate , 6: Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"
                            Call PopUp_Batter(WK, M)
                            i$ = GetKeyPress$
                            Color 7, 0
                        End If

                    Loop Until M = 23

                Case 2
                    Do
                        Cls

                        Color 12: Print "VIEW PITCHERS (AVAILABLE PLAYERS HIGHLIGHTED)"

                        For i = 0 To 10:

                            Color 3, 0

                            If P%(WK, i, 15) = 0 Then Color 11

                            Locate i + 5, 5: Print Using "##"; i;: Print " "; P$(WK, i); Tab(40);

                            Color 3

                            If P%(WK, i + 11, 15) = 0 Then Color 11

                            Print Using "##"; i + 11;: Print " "; P$(WK, i + 11)

                            Color 3

                        Next

                        Do
                            Color 15
                            Locate 20, 1: Print "# TO VIEW (0-21) (22 TO CONTINUE) ";: Input M
                        Loop Until M >= 0 And M <= 22

                        If M <> 22 Then
                            Call PopUp_Pitcher(WK, M)
                            i$ = GetKeyPress$
                            Color 7, 0
                        End If

                    Loop Until M = 22

            End Select

        End If

    Loop Until WK = 2 Or Q = 3 Or M = 23

    850 '

End Sub

'------------------------------
'   WALLDOUBLE Subroutine
'------------------------------
' ...explanation...
Sub WALLDOUBLE (HW%, W%)

    ' *** doubles off of a wall ***
    Call d100(BV)
    Call d100(RN)

    3781 '
    Select Case BV
        Case 1 To 40:
            If HW% = 0 Then F$ = "It's in the gap and to the wall"
        Case 41 To 65
            Select Case RN
                Case 1 To 25
                    If (L%(1, 13) = 8 Or L%(1, 13) = 40) Then F$ = "It's off the ivy!"
                Case 26 To 50
                    If L%(1, 13) = 21 And W% = 7 Then F$ = "It hits off the Green Monster!"
            End Select

            If L%(1, 13) = 27 And W% = 7 Then
                F$ = "It hits off the screen!"
            Else
                F$ = "It's off the wall"
            End If
        Case 66 To 85
            If HW% = 0 Then
                If W% = 8 Then
                    BV = 39
                    GoTo 3781
                Else
                    Call PBP("It's past " + Q$(4)): F$ = "It goes into the corner..."
                End If
            End If

        Case 86 To 90
            If HW% = 0 Then
                Call PBP("It's to the wall")
                Call PBP(Q$(0) + " slides into second")
                F$ = "It's a stand-up double!!"
            End If
        Case Else
            If RN <= 25 And L%(1, 13) = 21 Then
                F$ = "It's off the Green Monster!"
            Else
                If L%(1, 13) = 27 And W% = 7 Then
                    F$ = "It's off the screen..."
                Else
                    F$ = "It's off the top of the wall..."
                End If
            End If

    End Select

    Call PBP(F$)

End Sub

'------------------------------
'   WHICHRUNNER Subroutine
'------------------------------
' ...explanation...
Sub WHICHRUNNER (i, S4%, D5)

    If i = 4 Then A5%(3) = 1

    If S4% = 4 Then

        Call PBP("WHICH RUNNER DO YOU WANT TO GO AFTER")

        Call PBP("1 - " + B$(P, B3%(P, B0%(2, 0))))

        If B7%(P, B0%(1, 0)) <> 1 Then
            Call PBP("2 - " + B$(P, B3%(P, B0%(1, 0))))
        Else
            Call PBP("2 - " + P$(P, P1%(P)))
        End If

        If U6 = 2 Or U6 = 1 And P <> U9 Then
            D5 = Int(Rnd(1) * 2) + 1
            i$ = Str$(D5)
        Else
            i$ = GetKeyPress$
        End If

        D5 = Val(i$)

        If D5 = 2 Then
            i = 2
            D5 = 3
            I2 = B3%(P, B0%(1, 0))
            If B7%(P, B0%(1, 0)) = 1 Then Q$(i - 1) = P$(P, P1%(P))
        End If

        D5 = 0

    End If

End Sub

'------------------------------
'   WHOSON1ST Subroutine
'------------------------------
' ...explanation...
Sub WHOSON1ST (S4%)

    S4% = 0
    I1 = B0%(1, 0)
    I2 = B0%(2, 0)
    I3 = B0%(3, 0)

    If I1 > 0 Then
        If I2 = 0 Then
            If I3 = 0 Then
                S4% = 1
            Else
                ' *** 1ST AND 3RD ***
                S4% = 5
            End If
        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** 1ST AND 2ND ***
                S4% = 4
            Else
                'I3 > 0
                ' *** BASES LOADED ***
                S4% = 7
            End If
        End If
    Else
        'I1 = 0
        If I2 = 0 Then
            If I3 > 0 Then
                ' *** RUNNER ON 3RD ***
                S4% = 3
            End If

        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** RUNNER ON 2ND ***
                S4% = 2
            Else
                'I3 > 0
                ' *** 2ND AND 3RD ***
                S4% = 6
            End If
        End If
    End If

End Sub

'------------------------------
'   FOULBALL Subroutine
'------------------------------
' ...explanation...
Sub FOULBALL ()

    Call d100(RN)

    If RN <= 50 Then
        pbpIdx% = Int(Rnd(1) * 20) + 3130
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)
        Call PBP("foul ball!!")
    Else
        pbpIdx% = Int(Rnd(1) * 10) + 3150
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)

        pbpIdx% = Int(Rnd(1) * 10) + 3160
        Call GETRPBP(pbpIdx%, Y$, D)
        Call STRIPRPBP(Y$, pbpIdx%, D)
        Call PBP(Y$)
    End If
End Sub

'------------------------------
'   RAREPLAYS Subroutine
'------------------------------
' ...explanation...

' H6% gets randomly assigned first.
' Then at the end of most of these
' branches, it gets assigned a
' static value.

' Where is it used in the game?

Sub RAREPLAYS (H6%, H7%, IP%)

    If H7% = 20 Then
        H7% = 0
    Else

        Call WINDEX
        Call d100(H6%)

        Select Case H6%

            Case 1

                Call PBP(B$(D, B9%(D, 2)) + " is arguing ball and strike calls")
                Call PBP(U$(1) + " has heard enough")
                Call PBP(B$(D, B9%(D, 2)) + " has been ejected!!")

                X$ = B$(D, B9%(D, 2))

                Call EJECTINC(EJ%, X$)
                Call DELAY

                po% = 2

                Call EJECTIONINJURY(U6, P, U9, D, po%, P9, W5, W6)

            Case 2 To 3

                Call PBP(P$(D, P1%(D)) + " steps off the mound")
                Call PBP("after that last call")
                Call PBP("He is shouting at " + U$(1))
                Call PBP(U$(1) + " does not appreciate what he hears...")
                Call PBP(P$(D, P1%(D)) + " has been ejected!!")

                X$ = P$(D, P1%(D))

                Call EJECTINC(EJ%, X$)
                Call DELAY
                Call INJPIT(P, D, IP%)

            Case 4
                If B7%(P, B1!(P)) <> 1 Then
                    Call PBP(Q$(0) + " is unhappy with that last call"): Call PBP("He is venting his opinion to " + U$(1)): Print "Thats it!! "; U$(1); " has ejected "; Q$(0); "!!"
                    X$ = Q$(0): Call EJECTINC(EJ%, X$): Call DELAY: Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)
                Else
                    Call PBP(Q$(0) + " did not like that"): Call PBP("last call at all!!")
                End If

            Case 5

                Call PBP(Q$(0) + " hits a slow roller to first")
                Call PBP(B$(D, B9%(D, 3)) + " fields it cleanly")
                Call PBP("He flips it to " + P$(D, P1%(D)) + " covering just in time!")
                Call PBP("Wait... " + U$(2) + " rules safe!!")

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6)

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6)

                For i = 0 To 3: A5%(i) = 1: Next i

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                Call PBP(M$(D) + " charges from the dugout")
                Call PBP(M$(D) + ", " + P$(D, P1%(D)) + ",")
                Call PBP(B$(D, B9%(D, 2)) + ", and " + B$(D, B9%(D, 3)))
                Call PBP("are going at it with " + U$(2))
                Call DELAY
                Call DELAY

                Call PBP(U$(3) + " and " + U$(1) + " come in to help out")
                Call PBP("This argument is out of control now!!")
                Call PBP("What is this?!")
                Call PBP("All of them have been ejected!!!")

                X$ = P$(D, P1%(D))

                Call EJECTINC(EJ%, X$)

                X$ = B$(D, B9%(D, 2))

                Call EJECTINC(EJ%, X$)

                X$ = B$(D, B9%(D, 3))

                Call EJECTINC(EJ%, X$)

                X$ = M$(D)

                Call EJECTINC(EJ%, X$)
                Call DELAY

                po% = 2

                Call EJECTIONINJURY(U6, P, U9, D, po%, P9, W5, W6)

                po% = 3

                Call EJECTIONINJURY(U6, P, U9, D, po%, P9, W5, W6)
                Call INJPIT(P, D, IP%)

            Case 6 To 7

                Call PBP(Q$(0) + " sends a long one")
                Call PBP("down the left field line")
                Call PBP("It may go...")
                Call PBP("HOME RUN!!!")
                Call PBP("Wait!!! ")
                Call PBP(U$(4) + " is calling it foul!!")

                Call WINDEX

                Call PBP(Q$(0) + " is arguing vehemently")
                Call PBP("with " + U$(1) + " and " + U$(4))
                Call PBP(M$(P) + " charges from the dugout")
                Call PBP("He and " + Q$(0) + " are really going")
                Call PBP("at it with the umpires!!")
                Call DELAY
                Call PBP(" has just been tossed!!"): X$ = Q$(0): Call EJECTINC(EJ%, X$)
                Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)

            Case 8

                Call PBP(U$(1) + " has just ejected " + P$(D, P1%(D)))
                Call PBP("for wetting the ball!!")
                Call EJECTINC(EJ%, X$)

                EJ$(EJ%) = P$(D, P1%(D))

                Call DELAY
                Call INJPIT(P, D, IP%)

            Case 9

                Call PBP("Here's the 3-2 pitch...")
                Call PBP("Time called...")
                Call PBP(U$(1) + " is calling it ball 4")
                Call PBP("He says " + P$(D, P1%(D)) + " went to his mouth")

            Case 10 To 15

                Call PBP(Q$(0) + " hits the deck on a high")
                Call PBP("and tight pitch from " + P$(D, P1%(D)))
                Call PBP(Q$(0) + " charges the mound!")
                Call PBP("Both dugouts have emptied...")
                Call PBP("It's a bench-clearing brawl!")
                Call WINDEX
                Call PBP("It will take a few minutes to clean this up")

                i = Int(Rnd(1) * 3) + 1

                Select Case i

                    Case 1:
                        Call PBP(P$(D, P1%(D)) + " has been ejected")
                        X$ = P$(D, P1%(D))
                        Call EJECTINC(EJ%, X$)
                        Call INJPIT(P, D, IP%)

                    Case 2:
                        Call PBP(Q$(0) + " has been ejected")
                        X$ = Q$(0)
                        Call EJECTINC(EJ%, X$)
                        Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)

                    Case 3:
                        Call PBP(Q$(0) + " and " + P$(D, P1%(D)) + " have been ejected")
                        X$ = Q$(0)
                        Call EJECTINC(EJ%, X$)
                        X$ = P$(D, P1%(D))
                        Call EJECTINC(EJ%, X$)
                        Call INJPIT(P, D, IP%)
                        Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)

                End Select

            Case 16 To 21

                H6% = 21

                Call WINDEX

                If RD = 0 And (PC% = 0 Or PC% = 4) Then
                    Call PBP(Q$(0) + " fouls the pitch out of play")
                Else
                    tm = Int(Rnd(1) * 59) + 2

                    Color 0, 7
                    Call PBP("The game has been halted by rain")

                    For i = 1 To 2: Call DELAY: Next

                    Call PBP("Rain delay lasted" + Str$(tm) + " minutes")
                    Call DELAY

                    RD% = 1

                    If tm > 28 Then
                        Call PBP("Pitchers have lost effectiveness during delay")
                        Call PBP("Their arms have stiffened up")
                        Call DELAY
                        For i = 0 To 1: S8%(i, 0) = S8%(i, 0) - 2: Next
                    End If

                End If

            Case 22 To 24

                Select Case s4%
                    Case 0

                        If L%(1, 13) = 14 Then
                            Call PBP(Q$(0) + " steps out of the box as a jet passes over")
                        Else
                            Call PBP(U$(1) + " calls time to get some new baseballs")
                        End If

                    Case 1
                        Call PBP(B$(D, B9%(D, 2)) + " rifles the ball to ")
                        Call PBP("1st...")
                        i = 2

                    Case 2, 4
                        Call PBP(B$(D, B9%(D, 2)) + " rifles the ball to ")
                        Call PBP("2nd base...")
                        i = 3

                    Case 3
                        Call PBP(B$(D, B9%(D, 2)) + " rifles the ball to ")
                        Call PBP("3rd base...")
                        i = 4

                End Select

                Call PBP(Q$(i - 1) + " is picked off base!")

                A5%(i - 1) = 10
                O%(0) = O%(0) + 1
                O%(1) = O%(1) + 1
                W% = 2

                Call CREDITASSIST(W%, D)
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                I5 = P1%(D)
                I6 = 0

                Call ADDPIT(D, I5, I6)

                If O%(0) >= 3 Then B1!(P) = B1!(P) - 1: H6% = 5

                'If I understand the logic right, most cases
                'of values 22 - 24 will still move ahead
                'and run the code below, although it makes
                'little sense to me that right after an out
                ' GoTo 2020

            Case 25 To 29

                2020 '
                Call PBP(Q$(0) + " pops it up near the 3rd base stands")
                Call PBP("A fan interferes...")
                Call PBP(B$(D, B9%(D, 6)) + " makes a great catch!!")

                W% = 6
                Call CREDITREGPO(W%, D)

                O%(0) = O%(0) + 1
                O%(1) = O%(1) + 1
                Call DELAY

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6)

                I5 = P1%(D)
                I6 = 0
                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) = 1 Then
                    I5 = P1%(P)
                    I6 = 23
                    Call INCPITOFF(P, I5, I6)
                End If

                Call DELAY

            Case 30

                Call PBP(Q$(0) + " pops it up foul near the stands")
                Call PBP(B$(D, B9%(D, 2)) + " runs into the stands!!")

                po% = 2

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 30 To 31

                Call PBP(Q$(0) + " pops it up foul near the stands")
                Call PBP(B$(D, B9%(D, 3)) + " gives chase")
                Call PBP("he runs into the stands!!")

                po% = 3

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 32

                Call PBP(Q$(0) + " pops it up foul down the rf line")
                Call PBP(B$(D, B9%(D, 4)) + " crashes into the wall")

                po% = 4

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 33

                Call PBP(Q$(0) + " pops it up foul by")
                Call PBP("the 3rd base dugout")
                Call PBP(B$(D, B9%(D, 5)) + " falls in trying to make the catch!!")

                po% = 5

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 34

                Call PBP(Q$(0) + " pops it up foul")
                Call PBP("down the lf line")
                Call PBP(B$(D, B9%(D, 6)) + " tumbles over the railing")
                Call PBP("trying to make the catch!")
                Call DELAY

                po% = 6

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 35 To 37

                Call PBP(Q$(0) + " lines one to deep left field")
                Call PBP(B$(D, B9%(D, 7)) + " races back...")
                Call PBP("He slams into the wall and is down!!")

                op% = 7

                Call PBP(B$(D, B9%(D, 8)) + " comes over and gets the ball in")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                For i = 0 To 3: A5%(i) = 2: Next i

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = op%

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 5

            Case 38 To 40

                Call PBP(Q$(0) + " drives a sinking liner")
                Call PBP("into deep center")
                Call PBP(B$(D, B9%(D, 8)) + " gets a good jump on it")
                Call PBP("He crashes into the wall and")
                Call PBP("the ball bounces away!!")
                Call PBP("The runners are circling the bases")
                Call PBP(B$(D, B9%(D, 9)) + " retrieves the ball")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                I6 = 14

                Call ADDPIT(D, I5, I6)

                W% = 8

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 6: Call INCBATOFF(P, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 29: Call INCPITOFF(P, I5, I6)

                A5%(0) = 4
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call PBP("It's an inside the park HOME RUN!!!")
                Call DELAY
                Call SCOREBOARD
                Call WINDEX

                po% = 8

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 5

            Case 41 To 43

                Call PBP(Q$(0) + " lifts a deep drive into right")
                Call PBP(B$(D, B9%(D, 9)) + " races back...")
                Call PBP("He slams into the wall and is down!!")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                W% = 9

                Call PBP(B$(D, B9%(D, 8)) + " comes over to get the ball back in")

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 5: Call INCBATOFF(P, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 28: Call INCPITOFF(P, I5, I6)

                A5%(0) = 3
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call PBP("Triple!!")
                Call DELAY
                Call SCOREBOARD
                Call WINDEX

                po% = 9

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 5

            Case 44

                I5 = P1%(D)
                I6 = 1
                Call ADDPIT(D, I5, I6)

                W% = Int(Rnd(1) * 3) + 7

                Call PBP(Q$(0) + " hits a sinking liner into " + C$(W%))
                Call PBP(B$(D, B9%(D, W%)) + " dives for the catch...")
                Call PBP("the ball rolls free!")
                Call PBP(B$(D, B9%(D, W% - 1)) + " gets the loose ball in")

                op% = W%

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                For i = 0 To 3: A5%(i) = 2: Next i

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = op%

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 5

            Case 45

                Call PBP(Q$(0) + " is hit by a pitch and injured")
                Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)

            Case 46 To 49

                Call PBP(B$(D, B9%(D, 2)) + " is injured by a foul tip!")

                po% = 2

                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 50 To 58

                i = Int(Rnd(1) * 3) + 7

                If i = 9 Then hi1 = 3: hi2 = 9: hi3 = 8
                If i = 8 Then hi1 = 4: hi2 = 8: hi3 = 7
                If i = 7 Then hi1 = 6: hi2 = 7: hi3 = 8

                Call PBP(Q$(0) + " hits a weak fly into short " + C$(i))
                Call PBP(B$(D, B9%(D, hi1)) + " goes out")
                Call PBP(B$(D, B9%(D, hi2)) + " is charging in")
                Call PBP("They collide and the ball rolls free!!")
                Call PBP(B$(D, B9%(D, hi3)) + " gets it back in")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                For i = 0 To 3: A5%(i) = 2: Next i

                A5%(3) = 1
                A5%(1) = 3
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = hi1
                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                po% = hi2
                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 5

            Case 59 To 61
                Call PBP(Q$(0) + " fouls the pitch off his foot")
                Call PBP("He is injured")
                Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)

                H6% = 1

                '62 to 64
            Case 62 To 64

                Call PBP(Q$(0) + " pops it up in foul along the 3b line"): Call PBP("It drifts into foul territory")
                Call PBP(B$(D, B9%(D, 5)) + " and " + P$(D, P1%(D)) + " chase after it and collide!!")
                Call PBP(B$(D, B9%(D, 5)) + " is down and injured")

                po% = 5
                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                H6% = 1

            Case 65 To 70

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                W% = 8

                Call LONGFLY(D, W%)
                Call PBP(B$(D, B9%(D, 7)) + " collides with " + B$(D, B9%(D, 8)) + "!!")
                Call PBP("both fielders are down... ")
                Call PBP(B$(D, B9%(D, 6)) + " gets the ball back in")

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                A5%(0) = 2
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call PBP("Double!!")
                Call SCOREBOARD
                Call WINDEX

                po% = 7
                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6)

                po% = 8
                Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6): H6% = 5

            Case 71 To 85

                F$ = Q$(0) + " lines it off "

                i = Int(Rnd(1) * 5) + 1

                If i = 1 Then
                    F$ = F$ + "the leg of " + P$(D, P1%(D))
                    Call PBP(F$)

                    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6)

                    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6)

                    I5 = P1%(D)
                    I6 = 1

                    Call ADDPIT(D, I5, I6)

                    For i = 0 To 3: A5%(i) = 1: Next

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call SCOREBOARD
                    Call WINDEX
                    Call PBP("He reaches on a single")
                    Call PBP(P$(D, P1%(D)) + " is down and in pain!")
                    Call INJPIT(P, D, IP%)
                    H6% = 5
                Else
                    'i = 2 to 5
                    po% = i + 1
                    F$ = F$ + "of " + B$(D, B9%(D, po%)) + " for a single"

                    Call PBP(F$)
                    Call DELAY

                    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6)
                    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6)

                    I5 = P1%(D)
                    I6 = 1

                    Call ADDPIT(D, I5, I6)

                    For i = 0 To 3: A5%(i) = 1: Next

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call SCOREBOARD
                    Call WINDEX
                    Call PBP(B$(D, B9%(D, po%)) + " is down and injured")
                    Call INJURYLENGTH(IJL%, D, po%, U6, P, U9, P9, W5, W6): H6% = 5

                End If

            Case 86 To 89

                Call PBP(P$(D, P1%(D)) + " is motioning for " + M$(D) + " to come out")
                Call PBP("It appears he has a blister")
                Call PBP("He will have to leave the game")
                Call INJPIT(P, D, IP%)

                H6% = 1

            Case 90

                Call PBP(Q$(0) + " takes a mighty swing!")
                Call PBP("It looks like he injured himself")
                Call PLYRHRT(P, P9, W5, W6, BC%, U%, D, MO)

                H6% = 1

            Case 91

                Call PBP("Time is called")
                Call PBP("Morganna strikes again!!")

                H6% = 1

            Case 92 To 100

                Call PBP(U$(1) + " calls time to dust off the plate")

                H6% = 1

        End Select

    End If

End Sub


'------------------------------
'   WEATHER Subroutine
'------------------------------
' ...explanation...
Sub WEATHER ()

    If usingGfx = 1 Then Cls: Screen 0

    Call DRAWBOX(5)

    Locate 9, 11

    If L%(1, 9) = 1 Then
        Locate , 11: Print "TEMPERATURE: 70": Locate , 11: Print "WEATHER: DOME": Locate , 11: Print "WIND: NONE"
    Else
        Locate , 11: Print "TEMPERATURE: "; TP%
        Locate , 11: Print "SKY CONDITIONS: "; SC$(SC%)
        WS% = Int(Rnd(1) * 15) + 7
        Locate , 11: Print "WIND: "; WI$
        Locate , 11: Print "PRECIPITATION: "; PC$(PC%)
    End If

    I$ = GetKeyPress$

    If usingGfx = 1 Then
        Screen imageScreen&
    Else
        Color 15, 0
    End If

End Sub


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

    For II = 1 To JJ:
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 7 + JJ, 8

    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Locate 7

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
        Next
    Next

    Get #1, ID

    attendance& = CVL(Q3$(1))
    LF = CVI(Q3$(2))
    LCF = CVI(Q3$(3))
    CF = CVI(Q3$(4))
    RCF = CVI(Q3$(5))
    RF = CVI(Q3$(6))
    TG = CVI(Q3$(7))
    DOME = CVI(Q3$(8))
    WINFAC = CVI(Q3$(9))

    For I = 0 To 5
        For I1 = 0 To 14
            MON(I, I1) = CVI(Q3$(10 + I1 + I * 15))
        Next
    Next
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
        If RN <= SUM Then Exit For
    Next

    WD% = I

    SUM = 0
    For I = 10 To 13
        SUM = SUM + MON(SM%, I)
    Next

    RN = Int(Rnd(1) * SUM) + 1

    SUM = 0

    For I = 10 To 13
        SUM = SUM + MON(SM%, I)
        If RN <= SUM Then Exit For
    Next

    If I = 10 Then WS% = Int(Rnd(1) * 10)
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

        Call d100(RN)
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
    Print "(C) LANCE HAFFNER 1986-2000"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color , 0
End Sub

'----------------------------------------
'           INITVARI Subroutine
'----------------------------------------
' Initializes variables for the game
' It is not clear why more variables
' aren't processed here, but in future
' versions this type of thing will be
' stadard for all variables
Sub INITVARI ()

    For P9 = 0 To 1
        For I3 = 0 To 21
            If P%(P9, I3, 15) <> 99 Then P%(P9, I3, 15) = 0
        Next
    Next

    For I = 0 To 1
        For I1 = 0 To 23
            X0%(I, 0, I1) = -1
            X0%(I, 2, I1) = 100
        Next I1
    Next I

    For I = 0 To 1
        For I1 = 0 To 22
            If B%(I, I1, 21) <> 99 Then B%(I, I1, 21) = 0
        Next
    Next

End Sub

'------------------------------
'   LetterToNumber Subroutine
'------------------------------
' ...explanation...
Sub LetterToNumber (I$)

    Select Case UCase$(I$)
        Case "A": I$ = "10"
        Case "B": I$ = "11"
        Case "C": I$ = "12"
        Case "D": I$ = "13"
        Case "E": I$ = "14"
        Case "F": I$ = "15"
        Case "G": I$ = "16"
        Case "H": I$ = "17"
        Case "I": I$ = "18"
        Case "J": I$ = "19"
        Case "K": I$ = "20"
        Case "L": I$ = "21"
        Case "M": I$ = "22"
    End Select

End Sub

'------------------------------
'   LINSWAP Subroutine
'------------------------------
' ...explanation...
Sub LINSWAP (P9)

    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

    Do
        I$ = GetKeyPress$
        K1 = Val(I$)
    Loop Until K1 >= 1 And K1 <= 9 Or B7%(P9, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or B7%(P9, K2) <> 1

    Swap B7%(P9, K2), B7%(P9, K1)
    Swap B3%(P9, K1), B3%(P9, K2)

    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)

    Call NEWLINES(P9)

    Locate 18, 59: Print Space$(17)
    Locate 19, 59: Print Space$(17)

End Sub

'------------------------------
'   NEWLINES Subroutine
'------------------------------
' ...explanation...
Sub NEWLINES (P9)

    For I = 1 To 9

        Call POSITION(I)

        Print Using "# "; I;

        If B7%(P9, I) = 1 And I5 <> 66 Then Print P$(P9, B3%(P9, I));

        If B7%(P9, I) <> 1 Then Print B$(P9, B3%(P9, I));

        Print Tab(74);

        Print C$(B7%(P9, I));

    Next I

    I5 = 0

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

    Do
        I$ = GetKeyPress$
        K1 = Val(I$)
    Loop Until K1 >= 1 And K1 <= 9 Or B7%(P9, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or B7%(P9, K2) <> 1

    Swap B7%(P9, K2), B7%(P9, K1)

    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)

    Call NEWLINES(P9)

    Locate 18, 59: Print Space$(17)
    Locate 19, 59: Print Space$(17)

End Sub

'------------------------------
'   SHOWBATTERS Subroutine
'------------------------------
' ...explanation...
Sub SHOWBATTERS (P9)

    Cls

    Color 14
    Print "#";: Color L%(P9, 11), L%(P9, 12): Print A$(P9);

    Color 14, 0

    Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"

    Locate 2

    For I = 0 To 22: Locate , 1

        If B$(P9, I) = "XXX" Or Left$(B$(P9, I), 1) = " " Or B$(P9, I) = "" Then
            B%(P9, I, 21) = 98
        Else
            Color 2, 0

            If I < 10 Then Print Using "#"; I;
            If I > 9 Then Print X$(I - 10);

            Color 15, 0
            Call COLCHK(P9, I)

            'Name / Position
            Locate , 3: Print B$(P9, I);
            Color 15, 0
            Locate , 15: Print B1$(B%(P9, I, 0) + 2);

            Locate , 18: If B%(P9, I, 22) <> -1 Then Print Using "#"; B%(P9, I, 22);
            Locate , 19: If B%(P9, I, 23) <> -1 Then Print Using "#"; B%(P9, I, 23);
            Locate , 20: If B%(P9, I, 24) <> -1 Then Print Using "#"; B%(P9, I, 24);
            Locate , 21: If B%(P9, I, 25) <> -1 Then Print Using "#"; B%(P9, I, 25);

            Locate , 23

            Print Using "### ## ## ## ### ###"; B%(P9, I, 4); B%(P9, I, 7); B%(P9, I, 8); B%(P9, I, 9); B%(P9, I, 11); B%(P9, I, 12);

            Locate , 44: Print Using "#"; B%(P9, I, 16);: Print Using "#"; B%(P9, I, 34);: Print Using "#"; B%(P9, I, 33);

            Locate , 47
            '           SB   G%
            Print Using "### ##"; B%(P9, I, 13); B%(P9, I, 18);

            'BA
            If B%(P9, I, 6) = B%(P9, I, 4) And B%(P9, I, 6) <> 0 Then
                Print "1.000";
            Else
                If B%(P9, I, 4) = 0 Then
                    Print " .---";
                Else
                    bAvg! = B%(P9, I, 6) / B%(P9, I, 4)
                    If bAvg! = 0 Then
                        Print " .000";
                    Else
                        Print " ."; Using "###"; bAvg! * 1000;
                    End If
                End If
            End If

            If I <> 22 Then Print

        End If

    Next I

End Sub

'------------------------------
'     SHOWFIELD Subroutine
'------------------------------
' ...explanation...
Sub SHOWFIELD (P9)

    'DISPLAY FIELDING AVERAGES
    Locate 1, 15
    Color 14, 0
    Print "P1 FAVG A R P2 FAVG A R P3 FAVG A R P4 FAVG A R                 "

    Color 15, 0
    Locate 2

    For I = 0 To 22

        Locate , 15: Print Space$(64);

        If B$(P9, I) <> "XXX" And Left$(B$(P9, I), 1) <> " " And B$(P9, I) <> "" Then
            Locate , 16: If B%(P9, I, 22) <> -1 Then Print Using "# #### # #"; B%(P9, I, 22); B%(P9, I, 26); B%(P9, I, 33); B%(P9, I, 34);
            Locate , 28: If B%(P9, I, 23) <> -1 Then Print Using "# #### # #"; B%(P9, I, 23); B%(P9, I, 27); B%(P9, I, 35); B%(P9, I, 36);
            Locate , 40: If B%(P9, I, 24) <> -1 Then Print Using "# #### # #"; B%(P9, I, 24); B%(P9, I, 28); B%(P9, I, 37); B%(P9, I, 38);
            Locate , 52: If B%(P9, I, 25) <> -1 Then Print Using "# #### # #"; B%(P9, I, 25); B%(P9, I, 29); B%(P9, I, 39); B%(P9, I, 40);
            If I <> 22 Then Print
        End If
    Next I

End Sub

'------------------------------
'   ShowSplit Subroutine
'------------------------------
' ...explanation...
Sub ShowSplit (P9)

    'DISPLAY SPLIT STATS
    Locate 1, 15
    Color 14, 0

    Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
    Color 15, 0

    Locate 2

    For I = 0 To 22

        Locate , 15: Print "                                                                ";

        If B$(P9, I) <> "XXX" And Left$(B$(P9, I), 1) <> " " And B$(P9, I) <> "" Then

            Locate , 20: Print Using "### ## ## ## ### ### "; B%(P9, I, 42); B%(P9, I, 44); B%(P9, I, 45); B%(P9, I, 46); B%(P9, I, 47); B%(P9, I, 48);

            If B%(P9, I, 42) = 0 Or B%(P9, I, 43) = 0 Then
                Print " .000";
            Else
                If B%(P9, I, 42) = B%(P9, I, 43) Then
                    Print "1.000";
                Else
                    Print Using " .###"; B%(P9, I, 43) / B%(P9, I, 42);
                End If
            End If

            Locate , 53: Print Using "### ## ## ## ### ### "; B%(P9, I, 49); B%(P9, I, 51); B%(P9, I, 52); B%(P9, I, 53); B%(P9, I, 54); B%(P9, I, 55);

            'AVG
            If B%(P9, I, 49) = 0 Or B%(P9, I, 50) = 0 Then
                Print " .000";
            Else
                If B%(P9, I, 49) = B%(P9, I, 50) Then
                    Print "1.000";
                Else
                    Print " ."; Using "###"; B%(P9, I, 50) / B%(P9, I, 49);
                End If
            End If

            If I <> 22 Then Print

        End If

    Next I

End Sub

'------------------------------
'   POSTGAME Subroutine
'------------------------------
' ...explanation...
Sub POSTGAME ()

    Dim HT$(0 To 162), L$(0 To 162), PL$(0 To 162)
    Dim SA$(0 To 1), W$(0 To 162)
    Dim B6%(22, 21), D1%(1), DYS%(0 To 1, 0 To 21), E%(13), H%(0 To 162)
    Dim ING%(0 To 162), P0%(0 To 21, 0 To 41), SS%(0 To 22, 0 To 6), SX%(0 To 1), V%(0 To 162)

    '--------------------
    ' Determine settings
    ' for records
    '--------------------
    If _FileExists("EVENTS.BOX") Then
        Open "EVENTS.BOX" For Input As #1
        For X = 1 To 13
            Input #1, E%(X)
        Next
        Close 1
    End If

    '--------------------
    ' Create special file
    '--------------------
    If INNING% > E%(1) Or S6%(1, 0) < E%(2) Or S6%(0, 0) < E%(2) Or S6%(1, 0) > E%(3) Or S6%(0, 0) > E%(3) Or S6%(1, 1) > E%(5) Or S6%(0, 1) > E%(5) Or S6%(1, 1) < E%(4) Or S6%(0, 1) < E%(4) Then

        Spec$ = diskPaths$(1) + "*.SPC"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%
        Count% = Count% + 1
        SPECIAL = 1

    Else

        skipAhead% = 0

        For I = 0 To 1

            For I1 = 0 To 22

                If B8%(I, I1, 1) > E%(6) Or B8%(I, I1, 2) > E%(7) Or B8%(I, I1, 3) > E%(8) Or B8%(I, I1, 4) > E%(9) Or B8%(I, I1, 5) > E%(10) Or B8%(I, I1, 6) > E%(11) Or B8%(I, I1, 9) > E%(12) Then
                    Spec$ = diskPaths$(1) + "*.SPC"
                    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
                    Count% = DirCount%
                    Count% = Count% + 1
                    SPECIAL = 1
                    skipAhead% = 1
                    Exit For
                End If

                If skipAhead% = 1 Then Exit For

            Next

        Next

        skipAhead% = 0
        For I = 0 To 1

            For I1 = 0 To 21

                If P8%(I, I1, 5) > E%(13) Then
                    Spec$ = diskPaths$(1) + "*.SPC"
                    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
                    Count% = DirCount%
                    Count% = Count% + 1
                    SPECIAL = 1
                    skipAhead% = 1
                    Exit For
                End If

            Next

            If skipAhead% = 1 Then Exit For

        Next

    End If

    Call CheckRecords

    BK$ = "## # # ## # #  # # #  #  # # ## ##"

    '--------------------
    ' Boxscore Options
    '--------------------
    Do
        I3 = 0

        If S6%(1, 0) - S6%(0, 0) > 0 Then I3 = 1
        If P2%(I3) <> P1%(I3) And S1%(I3) = -1 And P8%(I3, P1%(I3), 0) >= 9 And P8%(I3, P1%(I3), 0) <= 12 And P6%(I3) > 0 And P8%(I3, P1%(I3), 3) < 3 Then S1%(I3) = P1%(I3)
        If P2%(I3) = S1%(I3) Then S1%(I3) = -1

        Cls
        Print Tab(32);
        Color 12: Print "BOXSCORE OPTIONS"
        Print
        Color 14: Print "(0) ";
        Color 15: Print "SEND BOXSCORE TO THE SCREEN"
        Print
        Color 14: Print "(1) ";
        Color 15: Print "SEND BOXSCORE TO THE PRINTER"
        Print
        Color 14: Print "(2) ";
        Color 15: Print "SEND BOXSCORE TO A TEXT FILE"

        If AP% = 1 And U6 = 2 Then
            X = 2
        Else
            Do
                I$ = GetKeyPress$
                X = Val(I$)
            Loop Until X >= 0 And X <= 2

        End If

        Select Case X
            Case 0:
                Call BoxToScreen(BK$)

            Case 1, 2:
                If X = 1 Then
                    NM$ = "TEMPBOX"
                Else
                    If AP% <> 1 Then
                        Cls
                        Print "ENTER 8-CHARACTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";: Input NM$
                        NM$ = NM$ + ".SPC"
                    Else
                        Open "GAME82" For Input As #1
                        Input #1, scheduleFile$
                        For I = 0 To 2
                            Input #1, AP%(I)
                        Next
                        Close 1
                        ' NM$ = "BOXSCORE." + MID$(STR$(AP%(0) + AP%(2)), 2, LEN(STR$(AP%(0) + AP%(2))))
                        NM$ = "BOX" + Mid$(Str$(AP%(0) + AP%(2)), 2, Len(Str$(AP%(0) + AP%(2))))
                    End If
                End If

                Call BoxToFile(NM$, BK$, E%(), SPECIAL)

        End Select

        Color 15, 0
        Cls

        J = 1
        Call DRAWBOX(J)

        If Not (AP% = 1 And U6 = 2) Then

            Locate 9, 28
            Print "DISPLAY STATS AGAIN (YN)";
            Color 15, 0

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (AP% = 1 And U6 = 2)

    Cls
    Color 15, 0
    Cls

    J = 1
    Call DRAWBOX(J)

    Locate 9, 28: Print "SAVE STATS FOR EITHER TEAM (YN)";
    Color 15, 0

    If AP% = 1 Then
        BG% = 0
        BN% = 1
        I$ = "Y"
    Else
        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If UCase$(I$) = "N" Then
            X1 = 3
        Else
            Cls
            Color 12: Print Tab(33); "COMPILE STATS"
            Print
            Color 14: Print "(0) ";: Color 15: Print "BOTH TEAMS"
            Print
            Color 14: Print "(1) ";: Color L%(0, 11), L%(0, 12): Print A$(0); " ONLY": Color , 0
            Print
            Color 14: Print "(2) ";: Color L%(1, 11), L%(1, 12): Print A$(1); " ONLY": Color 15, 0
            Print

            Do
                J$ = GetKeyPress$
                X1 = Val(J$)
            Loop Until X1 >= 0 And X1 <= 2
        End If

    End If

    If UCase$(I$) = "Y" Then

        For PX = 0 To 1
            For I = 0 To 21
                If P%(PX, I, 15) = 1 Then SX%(PX) = I: Exit For
            Next I
        Next PX

        FILENAME$ = "GAME82"

        If _FileExists(FILENAME$) Then

            Open "GAME82" For Input As #2
            Input #2, scheduleFile$
            For I = 0 To 2: Input #2, AP%(I): Next
            Close 2

            Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = 48

            For X = 0 To 18
                Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
            Next

            Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
            NG%(13) = 1

            For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(X)): Next
            For X = 0 To 1: LSet Q$(20 + X) = YN$(X): Next

            LSet Q$(22) = MKI$(S6%(0, 0))
            LSet Q$(23) = MKI$(S6%(1, 0))
            Put #1, AP%(0) + AP%(2)

            Close 1

            AP%(2) = AP%(2) + 1
            AP% = 1

            If AP%(1) < AP%(0) + AP%(2) Then
                Kill "GAME82"
                NM = 1
            Else
                Open "GAME82" For Output As #2
                Print #2, scheduleFile$
                For I = 0 To 2: Print #2, AP%(I): Next
                Close 2
            End If

        End If

        Cls
        Q = 0
        W2% = 0
        L2% = 0

        Do
            C% = 0

            If AP% = 1 Then
                C% = 0
            Else
                If X1 = 0 Or X1 = 1 And Q = 0 Or X1 = 2 And Q = 1 Then
                    Cls
                Else
                    If X1 = 2 And Q = 0 Or X1 = 1 And Q = 1 Then C% = 1
                End If
            End If

            If C% <> 1 Then

                SA$(Q) = A$(Q)

                For XX = 4 To 11
                    If Mid$(SA$(Q), XX, 1) = " " Or Mid$(SA$(Q), XX, 1) = "." Then Mid$(SA$(Q), XX, 1) = "_"
                Next XX

                QQ = 8

                For XX = 11 To 4 Step -1:
                    If Mid$(SA$(Q), XX, 1) = "_" Then
                        QQ = XX - 4
                    Else
                        Exit For
                    End If
                Next XX

                statName$ = Mid$(SA$(Q), 4, QQ)
                statFile$ = statName$ + "." + Left$(SA$(Q), 2)
                FILENAME$ = diskPaths$(1) + statFile$ + "D"

                If _FileExists(FILENAME$) Then

                    Open diskPaths$(1) + statFile$ + "D" For Input As #1
                    For I1 = 0 To 22
                        Input #1, INJ%(Q, I1)
                    Next
                    For I1 = 0 To 21
                        Input #1, DYS%(Q, I1)
                    Next
                    Close 1

                End If

                If IJ% <> 1 Or DR% <> 1 Then

                    If IJ% <> 1 Then
                        For I1 = 0 To 22
                            If INJ%(Q, I1) > 0 Then INJ%(Q, I1) = INJ%(Q, I1) - 1
                            If B%(Q, I1, 32) > 0 Then INJ%(Q, I1) = B%(Q, I1, 32)
                        Next
                    End If

                    If DR% <> 1 Then

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

                    End If

                    Open diskPaths$(1) + statFile$ + "D" For Output As #1
                    For I1 = 0 To 22
                        Print #1, INJ%(Q, I1)
                    Next
                    For I1 = 0 To 21
                        Print #1, DYS%(Q, I1)
                    Next
                    Close 1

                End If

                FILENAME$ = diskPaths$(1) + statFile$

                If _FileExists(FILENAME$) Then
                    Open diskPaths$(1) + statFile$ For Input As #1
                    Input #1, A$(Q)
                    Input #1, W2%
                    Input #1, L2%
                    For I = 0 To 22: Input #1, B$(Q, I): Input #1, B2%(Q, I): Next I
                    For I = 0 To 21: Input #1, P$(Q, I): Input #1, P3%(Q, I): Next I
                    For I = 0 To 22: For I1 = 0 To 21: Input #1, B6%(I, I1): Next I1: Next I
                    For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I
                    Input #1, D0%(Q)
                    Input #1, D1%(Q)
                    For I = 0 To 21: Input #1, T%(I): Next I
                    For I = 0 To 22: Input #1, P4%(I): Next I
                    Close #1
                End If

                For I = 0 To 22

                    B2%(Q, I) = B%(Q, I, 4)

                    For I1 = 0 To 11
                        B6%(I, I1) = B6%(I, I1) + B8%(Q, I, I1): T%(I1) = T%(I1) + B8%(Q, I, I1)
                    Next I1

                    If B8%(Q, I, 0) > 0 Then
                        If B8%(Q, I, 1) = 0 Then
                            B6%(I, 12) = 0
                        Else
                            B6%(I, 12) = B6%(I, 12) + 1
                        End If
                    End If

                    If B8%(Q, I, 14) > 0 Then B6%(I, 14) = B6%(I, 14) + 1

                    For I1 = 15 To 21
                        B6%(I, I1) = B6%(I, I1) + B8%(Q, I, I1)
                        T%(I1) = T%(I1) + B8%(Q, I, I1)
                    Next I1

                    If B6%(I, 12) >= B6%(I, 13) Then B6%(I, 13) = B6%(I, 12)

                Next I

                For I = 0 To 21

                    For I1 = 23 To 34
                        P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1)
                        T%(I1 - 23) = T%(I1 - 23) + P8%(Q, I, I1)
                    Next I1

                    If P8%(Q, I, 23) > 0 Then
                        If P8%(Q, I, 24) = 0 Then
                            P0%(I, 35) = 0
                        Else
                            P0%(I, 35) = P0%(I, 35) + 1
                        End If
                    End If

                    For I1 = 37 To 41
                        P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1)
                        T%(I1 - 22) = T%(I1 - 22) + P8%(Q, I, I1)
                    Next I1

                    If P0%(I, 35) >= P0%(I, 36) Then P0%(I, 36) = P0%(I, 35)

                Next I

                T%(12) = T%(12) + L0%(Q)
                INNING% = 0

                For I = 0 To 21
                    INNING% = INNING% + P8%(Q, I, 0)
                    P3%(Q, I) = P%(Q, I, 6)

                    For I1 = 0 To 5
                        P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1)
                        P4%(I1) = P4%(I1) + P8%(Q, I, I1)
                    Next I1

                    For I1 = 13 To 22:
                        P0%(I, I1) = P0%(I, I1) + P8%(Q, I, I1)
                        P4%(I1) = P4%(I1) + P8%(Q, I, I1)
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

                P0%(SX%(Q), 9) = P0%(SX%(Q), 9) + 1
                P4%(9) = P4%(9) + 1

                D0%(Q) = D0%(Q) + D3%(Q)
                D1%(Q) = D1%(Q) + D3%(1 - Q)

                If S6%(Q, 0) - S6%(1 - Q, 0) > 0 Then W2% = W2% + 1
                If S6%(Q, 0) - S6%(1 - Q, 0) < 0 Then L2% = L2% + 1

                Color 15, 0

                Print ">>>>>"
                Print "THE STATS ARE BEING SAVED UNDER": Print "THE FOLLOWING NAME ::: ";
                Color L%(Q, 11), L%(Q, 12): Print statFile$
                Color 15, 0

                Open diskPaths$(1) + statFile$ For Output As #1
                Print #1, A$(Q)
                Print #1, W2%
                Print #1, L2%
                For I = 0 To 22: Print #1, B$(Q, I): Print #1, B2%(Q, I): Next I
                For I = 0 To 21: Print #1, P$(Q, I): Print #1, P3%(Q, I): Next I
                For I = 0 To 22: For I1 = 0 To 21: Print #1, B6%(I, I1): Next I1: Next I
                For I = 0 To 21: For I1 = 0 To 41: Print #1, P0%(I, I1): Next I1: Next I
                Print #1, D0%(Q)
                Print #1, D1%(Q)
                For I = 0 To 21: Print #1, T%(I): Next I
                For I = 0 To 22: Print #1, P4%(I): Next I
                Close #1

                Print "<<<<<"
                Print
                Print

                FILENAME$ = diskPaths$(1) + statFile$ + "R"

                If _FileExists(FILENAME$) Then
                    Open diskPaths$(1) + statFile$ + "R" For Input As #1
                    For I = 0 To 22
                        For I1 = 0 To 6
                            Input #1, SS%(I, I1)
                        Next
                    Next
                    Close 1
                End If

                For I = 0 To 22
                    For I1 = 0 To 6
                        SS%(I, I1) = SS%(I, I1) + CSS%(Q, I, I1)
                    Next
                Next

                Open diskPaths$(1) + statFile$ + "R" For Output As #1
                For I = 0 To 22
                    For I1 = 0 To 6
                        Print #1, SS%(I, I1)
                    Next
                Next
                Close 1

                Y% = 0
                FILENAME$ = diskPaths$(1) + statFile$ + "S"

                If _FileExists(FILENAME$) Then

                    Open diskPaths$(1) + statFile$ + "S" For Input As #1
                    Input #1, Y%
                    For I = 1 To 162: Input #1, H%(I): Next I
                    For I = 1 To 162: Input #1, HT$(I): Next I
                    For I = 1 To 162: Input #1, V%(I): Next I
                    For I = 1 To 162: Input #1, PL$(I): Next I
                    For I = 1 To 162: Input #1, W$(I): Next I
                    For I = 1 To 162: Input #1, L$(I): Next I
                    For I = 1 To 162: Input #1, ING%(I): Next I
                    Close #1

                Else
                    'Default stat values
                    For I = 1 To 162
                        H%(I) = 999
                        HT$(I) = "XXXXXXX"
                        V%(I) = 999
                        PL$(I) = "X"
                        W$(I) = "LYNN"
                        L$(I) = "SUMLER"
                        ING%(I) = 999
                    Next I

                End If

                Y% = Y% + 1
                H%(Y%) = S6%(Q, 0)
                HT$(Y%) = A$(1 - Q)
                V%(Y%) = S6%(1 - Q, 0)
                W$(Y%) = W$
                L$(Y%) = L$
                ING%(Y%) = LYN%

                If Q = 0 Then
                    PL$(Y%) = "A"
                Else
                    PL$(Y%) = "H"
                End If

                Open diskPaths$(1) + statFile$ + "S" For Output As #1

                Print #1, Y%

                For I = 1 To 162: Print #1, H%(I): Next I
                For I = 1 To 162: Print #1, HT$(I): Next I
                For I = 1 To 162: Print #1, V%(I): Next I
                For I = 1 To 162: Print #1, PL$(I): Next I
                For I = 1 To 162: Print #1, W$(I): Next I
                For I = 1 To 162: Print #1, L$(I): Next I
                For I = 1 To 162: Print #1, ING%(I): Next I

                Close #1

                Y% = 0

                For I = 0 To 22:
                    For I1 = 0 To 21:
                        B6%(I, I1) = 0
                    Next
                Next

                For I = 0 To 22:
                    For I1 = 0 To 6:
                        SS%(I, I1) = 0
                    Next
                Next

                For I = 0 To 21:
                    For I1 = 0 To 41:
                        P0%(I, I1) = 0:
                    Next
                Next

                W2% = 0
                L2% = 0

                For I = 0 To 21: T%(I) = 0: Next I
                For I = 0 To 22: P4%(I) = 0: Next I

                For I = 0 To 1:
                    For I1 = 0 To 22:
                        B2%(I, I1) = 0
                    Next
                Next

                For I = 0 To 1:
                    For I1 = 0 To 21:
                        P3%(I, I1) = 0:
                    Next
                Next

            End If

            Q = Q + 1

        Loop Until Q = 2

    End If

    If AP% = 1 And NM <> 1 Then Run "LOADER.EXE"

    Color 15, 0

    Cls

End Sub

'------------------------------
'   LocateInning routine
'------------------------------
' ...explanation...
Sub LocateInning (I2)

    If I2 < 4 Then Locate , ((2 * I2) + 18)
    If I2 > 3 And I2 < 7 Then Locate , ((2 * I2) + 19)
    If I2 > 6 And I2 < 10 Then Locate , ((2 * I2) + 20)
    If I2 > 9 And I2 < 13 Then Locate , ((2 * (I2 - 9)) + 18)
    If I2 > 12 And I2 < 16 Then Locate , ((2 * (I2 - 9)) + 19)
    If I2 > 15 And I2 < 19 Then Locate , ((2 * (I2 - 9)) + 20)
    If I2 > 18 And I2 < 22 Then Locate , ((2 * (I2 - 18)) + 18)
    If I2 > 21 And I2 < 25 Then Locate , ((2 * (I2 - 18)) + 19)
    If I2 > 24 And I2 < 28 Then Locate , ((2 * (I2 - 18)) + 20)

End Sub

'------------------------------
'       BoxToScreen routine
'------------------------------
' ...explanation...
Sub BoxToScreen (BK$)

    Shared DB, HR, TR, SB
    'Shared DB$(), E$(), HR$(), SB$(), TR$()

    X = 0

    Cls
    If INNING% < 9 Then
        For I = 0 To 1

            Color L%(I, 11), L%(I, 12)
            Print A$(I);
            Color 14, 0

            For I2 = 1 To INNING%
                Call LocateInning(I2)
                If I = 0 Then Print Using "##"; S%(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) = 0 Then Print , " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = INNING% Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) > 0 Then Print Using "##"; S%(I, I2);
                If I = 1 And I2 < INNING% Then Print Using "##"; S%(I, I2);

            Next I2

            Locate , 40: Print "-";
            Locate , 41: Print Using "##"; S6%(I, 0);
            Locate , 44: Print Using "##"; S6%(I, 1);
            Locate , 47: Print Using "#"; S6%(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 0 Or INNING% <= 9 Then

        For I = 0 To 1

            Color L%(I, 11), L%(I, 12)
            Print A$(I);
            Color 14, 0

            For I2 = 1 To 9
                Call LocateInning(I2)
                If I = 0 Then Print Using "##"; S%(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) = 0 Then Print " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = 9 Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) > 0 Then Print Using "##"; S%(I, I2);
                If I = 1 And I2 < 9 Then Print Using "##"; S%(I, I2);
            Next I2

            Locate , 40: Print "-";
            Locate , 41: Print Using "##"; S6%(I, 0);
            Locate , 44: Print Using "##"; S6%(I, 1);
            Locate , 47: Print Using "#"; S6%(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 1 Or INNING% > 10 And INNING% < 19 Or INNING% = 19 And P = 0 Then

        For I = 0 To 1

            Color L%(I, 11), L%(I, 12)
            Print A$(I);
            Color 14, 0

            For I2 = 1 To 9
                Call LocateInning(I2)
                Print Using "##"; S%(I, I2);
            Next I2

            Print
        Next I

        Print

        If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1

        For I = 0 To 1

            For I2 = 10 To INNING%
                Call LocateInning(I2)
                Print Using "##"; S%(I, I2);
            Next I2

            Locate , 40: Print "-";
            Locate , 41: Print Using "##"; S6%(I, 0);
            Locate , 44: Print Using "##"; S6%(I, 1);
            Locate , 47: Print Using "#"; S6%(I, 2)

        Next I

    End If

    If INNING% = 19 And P = 1 Or INNING% > 19 And INNING% <= 28 Then

        For I = 0 To 1
            Color L%(I, 11), L%(I, 12)
            Print A$(I);
            Color 14, 0
            For I2 = 1 To 9
                Call LocateInning(I2)
                Print Using "##"; S%(I, I2);
            Next I2
            Print
        Next I

        Print

        If INNING% = 28 Or INNING% < 28 And P = 0 Then INNING% = INNING% - 1

        For I = 0 To 1
            For I2 = 10 To 18
                Call LocateInning(I2)
                Print Using "##"; S%(I, I2);
            Next I2
            Print
        Next I

        Print

        For I = 0 To 1
            For I2 = 19 To INNING%
                Call LocateInning(I2)
                Print Using "##"; S%(I, I2);
            Next I2
            Locate , 40: Print "-";
            Locate , 41: Print Using "##"; S6%(I, 0);
            Locate , 44: Print Using "##"; S6%(I, 1);
            Locate , 47: Print Using "#"; S6%(I, 2)
        Next I

    End If

    If X = 0 Then
        If AP% <> 1 Or U6 <> 2 Then
            Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If
    End If

    If X = 0 Then Cls

    For I = 0 To 1

        Color L%(I, 11), L%(I, 12)
        Print A$(I);
        Color 14, 0
        Locate , 18: Print "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
        Color 15, 0

        For I3 = 0 To 22

            If X0%(I, 0, I3) <> -1 And X0%(I, 1, I3) <> 1 Then

                I1 = X0%(I, 0, I3)

                Print B$(I, I1); ", "; C$(X0%(I, 1, I3));
                Locate , 18
                '                AB             R              H              BI             2              3              HR             K              W              SB             CS              E               PO              A
                Print Using BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);
                Print "   ";

                'BA
                If B5%(I, I1, 0) = 0 Or B5%(I, I1, 1) = 0 Then
                    Print " .000   ";
                Else
                    If B5%(I, I1, 0) = B5%(I, I1, 1) Then
                        Print "1.000   ";
                    Else
                        Print " ."; Using "###   "; (B5%(I, I1, 1) / B5%(I, I1, 0) * 1000);
                    End If
                End If

                '           HR   RBI
                Print Using "##  ###"; B5%(I, I1, 6); B5%(I, I1, 3)

                For I2 = 0 To 17
                    T3%(I2) = T3%(I2) + B8%(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21:
            For I2 = 0 To 13:
                T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23)
            Next

            For I2 = 14 To 17
                T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22)
            Next
        Next

        For I1 = 1 To P6%(I)

            For I2 = 0 To 21

                If P%(I, I2, 15) = I1 Then

                    Print P$(I, I2); ", P";
                    Locate , 18
                    '                AB              R               H               BI              2               3               HR              K               W               SB              CS              E               PO              A
                    Print Using BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
                    Print "   ";

                    'BA
                    If P5%(I, I2, 24) = 0 Or P5%(I, I2, 23) = 0 Then
                        Print " .000   "
                    Else
                        If P5%(I, I2, 24) = P5%(I, I2, 23) Then
                            Print "1.000   ";
                        Else
                            Print Using "#.###   "; P5%(I, I2, 24) / P5%(I, I2, 23);
                        End If

                        '           HR   RBI
                        Print Using "##  ###"; P5%(I, I2, 29); P5%(I, I2, 26)

                    End If
                End If

            Next

        Next

        Color 14, 0: Print "TOTALS";
        Color 15, 0
        Locate , 17: Print T3%(0);
        Locate , 22: Print T3%(1);
        Locate , 27: Print T3%(4);
        Locate , 32: Print T3%(6);
        Locate , 36: Print T3%(7);
        Locate , 42: Print T3%(10);
        Locate , 46: Print T3%(16);
        Locate , 50: Print T3%(17)
        Locate , 20: Print T3%(2);
        Locate , 24: Print T3%(3);
        Locate , 29: Print T3%(5);
        Locate , 34: Print T3%(8);
        Locate , 39: Print T3%(9);
        Locate , 44: Print T3%(11)

        Print

        If X = 0 Then
            If AP% <> 1 Or U6 <> 2 Then
                Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
                I$ = GetKeyPress$
            End If
        End If

        If X = 0 Then Cls

        For I1 = 0 To 22: T3%(I1) = 0: Next I1

    Next I

    'GoTo 7313

    For I = 0 To 1

        If X = 0 Then Cls

        Color L%(I, 11), L%(I, 12)
        Print A$(I);

        Color 14, 0: Locate , 23
        Print "       IP   H  R  ER BB SO    ERA   W  L  S  SO": Color 15, 0

        For I1 = 1 To P6%(I)

            For I2 = 0 To 21

                If P%(I, I2, 15) = I1 Then
                    I4 = 1 - I
                    Locate , 1
                    Print P$(I, I2); " ";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then
                        Print "(W,"; Right$(Str$(P5%(I, I2, 6) + 1), (Len(Str$(P5%(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7)), (Len(Str$(P5%(I, I2, 7))) - 1)); ")";
                        W$ = P$(I, I2)
                    End If

                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print "(S,"; Right$(Str$(P5%(I, I2, 12) + 1), (Len(Str$(P5%(I, I2, 12) + 1)) - 1)); ")";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then
                        Print "(L,"; Right$(Str$(P5%(I, I2, 6) + 0), (Len(Str$(P5%(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7) + 1), (Len(Str$(P5%(I, I2, 7) + 1)) - 1)); ")";
                        L$ = P$(I, I2)
                    End If

                    If P8%(I, I2, 19) > 0 Then Print "(BS,";: Print Using "##"; P5%(I, I2, 19);: Print ")";
                    If P8%(I, I2, 22) > 0 Then Print "(H,";: Print Using "##"; P5%(I, I2, 22);: Print ")";

                    Locate , 30: Print Using "##"; Int(P8%(I, I2, 0) / 3);

                    If P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3);

                    Locate , 34

                    Print Using "## ##  ## ## ##"; P8%(I, I2, 1); P8%(I, I2, 2); P8%(I, I2, 3); P8%(I, I2, 4); P8%(I, I2, 5);

                    Print "   ";

                    If P8%(I, I2, 3) > 0 And P8%(I, I2, 0) = 0 Then
                        Print "99.99 ";
                    Else
                        If P8%(I, I2, 0) = 0 Then
                            Print "--.-- ";
                        Else
                            If P8%(I, I2, 3) = 0 Then
                                Print " 0.00 ";
                            Else
                                Print Using "##.## "; P8%(I, I2, 3) / (P8%(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print Using "## "; P5%(I, I2, 6) + 1; Else Print Using "## "; P5%(I, I2, 6);
                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then Print Using "## "; P5%(I, I2, 7) + 1; Else Print Using "## "; P5%(I, I2, 7);
                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print Using "## "; P5%(I, I2, 12) + 1; Else Print Using "## "; P5%(I, I2, 12);

                    Print Using "###"; P5%(I, I2, 5)

                    For I4 = 0 To 5
                        T3%(I4) = T3%(I4) + P8%(I, I2, I4)
                    Next I4
                End If

            Next I2

        Next I1

        Color 14, 0
        Print "TOTALS";
        Color 15, 0

        Locate , 30

        If (T3%(0) / 3) >= 10 Then
            Locate (P6%(I) + 2), 29
        End If

        Print Int(T3%(0) / 3);

        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then
            Print ".";: Print Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);
        End If

        Locate , 34

        Print Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)
        Print

        If X = 0 Then
            If AP% <> 1 Or U6 <> 2 Then
                Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
                I$ = GetKeyPress$
            End If
        End If

        If X = 0 Then Cls

        LYN% = CInt(T3%(0) / 3)

        For I1 = 0 To 5: T3%(I1) = 0: Next I1

    Next I

    If X = 1 Then X = 0

    Cls

    Color 14, 0: Print " E-";
    Color 15, 0

    If E = 0 Then
        Print "none"
    Else
        For X = 0 To (E - 1): Print E$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "2B-";
    Color 15, 0

    If DB = 0 Then
        Print "none"
    Else
        For X = 0 To (DB - 1): Print DB$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "3B-";
    Color 15, 0

    If TR = 0 Then
        Print "none"
    Else
        For X = 0 To (TR - 1): Print TR$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "HR-";
    Color 15, 0

    If HR = 0 Then
        Print "none"
    Else
        For X = 0 To (HR - 1): Print HR$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "SB-";
    Color 15, 0
    If SB = 0 Then
        Print "none"
    Else
        For X = 0 To (SB - 1): Print SB$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "LOB-";
    Color 15, 0: Print A$(0); Str$(L0%(0)); ","; A$(1); Str$(L0%(1))
    Print "DP-";
    Color 15, 0: Print A$(0); Str$(DP%(0)); ","; A$(1); Str$(DP%(1))
    Color 14, 0: Print "INJURIES-";
    Color 15, 0

    If IJL% = 0 Then
        Print "none"
    Else
        For X = 1 To IJL%: Print IJ$(X); ",";: Next
        Print
    End If

    eject:
    Color 14, 0: Print "EJECTIONS-";
    Color 15, 0

    If EJ% = 0 Then
        Print "none"
    Else
        For X = 1 To EJ%: Print EJ$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "UMPIRES: ";
    Color 15, 0: Print "HP-"; U$(1); "; 1B-"; U$(2); "; 2B-"; U$(3); "; 3B-"; U$(4)
    Print "T: "; gameTime$; " A: "; attendance&
    Print
    Color 14, 0: Print "TEMPERATURE:";
    Color 15, 0: Print TP%
    Color 14, 0: Print "WIND: ";
    Color 15, 0: Print WI$
    Color 14, 0: Print "SKY: ";
    Color 15, 0: Print SC$(SC%)
    Color 14, 0: Print "PRECIPITATION: ";
    Color 15, 0: Print PC$(PC%)

    If AP% <> 1 Or U6 <> 2 Then
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
    End If

End Sub

'------------------------------
'       BoxToFile routine
'------------------------------
' ...explanation...
Sub BoxToFile (NM$, BK$, E%(), SPECIAL)

    Shared DB, HR, TR, SB

    Open diskPaths$(1) + NM$ For Output As #2

    If SPECIAL = 1 Then

        If INNING% > E%(1) Then Print #2, Using "MORE THAN ## INNINGS"; E%(1)

        If S6%(1, 0) < E%(2) Then Print #2, A$(1);: Print #2, Using " SCORED LESS THAN ## RUN(S)"; E%(2)
        If S6%(0, 0) < E%(2) Then Print #2, A$(0);: Print #2, Using " SCORED LESS THAN ## RUN(S)"; E%(2)
        If S6%(1, 0) > E%(3) Then Print #2, A$(1);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; E%(3)
        If S6%(0, 0) > E%(3) Then Print #2, A$(0);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; E%(3)
        If S6%(1, 1) > E%(5) Then Print #2, A$(1);: Print #2, Using " HAD MORE THAN ## HIT%(S)"; E%(5)
        If S6%(0, 1) > E%(5) Then Print #2, A$(0);: Print #2, Using " HAD MORE THAN ## HIT%(S)"; E%(5)
        If S6%(1, 1) < E%(4) Then Print #2, A$(1);: Print #2, Using " HAD LESS THAN ## HIT%(S)"; E%(4)
        If S6%(0, 1) < E%(4) Then Print #2, A$(0);: Print #2, Using " HAD LESS THAN ## HIT%(S)S"; E%(4)

        For I = 0 To 1

            For I1 = 0 To 22

                If B8%(I, I1, 1) > E%(6) Then Print #2, B$(I, I1);: Print #2, Using " HAD MORE THAN ## HIT%(S)"; E%(6)
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

    End If

    If INNING% < 9 Then

        For I = 0 To 1

            Print #2, A$(I);

            For I2 = 1 To INNING%
                Call InningPrint(I2)

                If I = 0 Then Print #2, Using "##"; S%(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) = 0 Then Print #2, " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = INNING% Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And S%(1, INNING%) > 0 Then Print #2, Using "##"; S%(I, I2);
                If I = 1 And I2 < INNING% Then Print #2, Using "##"; S%(I, I2);

            Next I2

            Print #2, Tab(40); "-";
            Print #2, Tab(41);
            Print #2, Using "##"; S6%(I, 0);
            Print #2, Tab(44);
            Print #2, Using "##"; S6%(I, 1);
            Print #2, Tab(47);
            Print #2, Using "#"; S6%(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 0 Or INNING% = 9 Then

        For I = 0 To 1

            Print #2, A$(I);

            For I2 = 1 To 9
                Call InningPrint(I2)
                If I = 0 Then Print #2, Using "##"; S%(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) = 0 Then Print #2, " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = 9 Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And S%(1, 9) > 0 Then Print #2, Using "##"; S%(I, I2);
                If I = 1 And I2 < 9 Then Print #2, Using "##"; S%(I, I2);

            Next I2

            Print #2, Tab(40); "-";
            Print #2, Tab(41);
            Print #2, Using "##"; S6%(I, 0);
            Print #2, Tab(44);
            Print #2, Using "##"; S6%(I, 1);
            Print #2, Tab(47);
            Print #2, Using "#"; S6%(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 1 Or INNING% > 10 And INNING% < 19 Or INNING% = 19 And P = 0 Then

        For I = 0 To 1

            Print #2, A$(I);

            For I2 = 1 To 9
                Call InningPrint(I2)
                Print #2, Using "##"; S%(I, I2);

            Next I2

            Print #2,
        Next I

        Print #2,

        If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1

        For I = 0 To 1

            For I2 = 10 To INNING%
                Call InningPrint(I2)
                Print #2, Using "##"; S%(I, I2);
            Next I2

            Print #2, Tab(40); "-";
            Print #2, Tab(41);
            Print #2, Using "##"; S6%(I, 0);
            Print #2, Tab(44);
            Print #2, Using "##"; S6%(I, 1);
            Print #2, Tab(47);
            Print #2, Using "#"; S6%(I, 2)
        Next I

    End If

    If INNING% = 19 And P = 1 Or INNING% > 19 And INNING% <= 28 Then

        For I = 0 To 1

            Print #2, A$(I);

            For I2 = 1 To 9
                Call InningPrint(I2)
                Print #2, Using "##"; S%(I, I2);
            Next I2
            Print #2,
        Next I

        Print #2,

        If INNING% = 28 Or INNING% < 28 And P = 0 Then INNING% = INNING% - 1

        For I = 0 To 1
            For I2 = 10 To 18
                Call InningPrint(I2)
                Print #2, Using "##"; S%(I, I2);
            Next I2
            Print #2,
        Next I

        For I = 0 To 1

            For I2 = 19 To INNING%
                Call InningPrint(I2)
                Print #2, Using "##"; S%(I, I2);
            Next I2

            Print #2, Tab(40); "-";
            Print #2, Tab(41);
            Print #2, Using "##"; S6%(I, 0);
            Print #2, Tab(44);
            Print #2, Using "##"; S6%(I, 1);
            Print #2, Tab(47);
            Print #2, Using "#"; S6%(I, 2)

        Next I
        Print #2,

    End If

    For I = 0 To 1

        Print #2, A$(I); Tab(18); "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"

        For I3 = 0 To 22

            If X0%(I, 0, I3) <> -1 And X0%(I, 1, I3) <> 1 Then

                I1 = X0%(I, 0, I3)

                Print #2, B$(I, I1); ", "; C$(X0%(I, 1, I3)); Tab(18);
                '                    AB             R              H              BI             2              3              HR             K              W              SB             CS              E               PO              A
                Print #2, Using BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);

                Print #2, "   ";

                'BA
                If B5%(I, I1, 0) = 0 Or B5%(I, I1, 1) = 0 Then
                    Print #2, " .000   ";
                Else
                    If B5%(I, I1, 0) = B5%(I, I1, 1) Then
                        Print #2, "1.000   ";
                    Else
                        Print #2, " ."; Using "###   "; (B5%(I, I1, 1) / B5%(I, I1, 0) * 1000);
                    End If
                End If

                'HR / RBI
                Print #2, Using "##  ###"; B5%(I, I1, 6); B5%(I, I1, 3)

                For I2 = 0 To 17
                    T3%(I2) = T3%(I2) + B8%(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21: For I2 = 0 To 13: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23): Next

        For I2 = 14 To 17: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22): Next: Next

        For I1 = 1 To P6%(I)
            For I2 = 0 To 21

                If P%(I, I2, 15) = I1 Then

                    Print #2, P$(I, I2); ", P";
                    Print #2, Tab(18);
                    Print #2, Using BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
                    Print #2, "   ";

                    If P5%(I, I2, 24) = 0 Or P5%(I, I2, 23) = 0 Then
                        Print #2, " .000   ";
                    Else
                        If P5%(I, I2, 24) = P5%(I, I2, 23) Then
                            Print #2, "1.000   ";
                        Else
                            Print #2, Using " .###   "; P5%(I, I2, 24) / P5%(I, I2, 23);
                        End If
                    End If

                    Print #2, Using "##  ###"; P5%(I, I2, 29); P5%(I, I2, 26)
                End If
            Next
        Next

        Print #2, "TOTALS"; Tab(17); T3%(0); Tab(22); T3%(1); Tab(27); T3%(4);
        Print #2, Tab(32); T3%(6); Tab(36); T3%(7); Tab(42);
        Print #2, T3%(10); Tab(46); T3%(16); Tab(50); T3%(17)
        Print #2, Tab(20); T3%(2); Tab(24); T3%(3); Tab(29);
        Print #2, T3%(5); Tab(34); T3%(8); Tab(39); T3%(9);
        Print #2, Tab(44); T3%(11)
        Print #2,: For I1 = 0 To 22: T3%(I1) = 0: Next I1

    Next I

    For I = 0 To 1

        Print #2,: Print #2, A$(I); Tab(23); "       IP   H  R  ER BB SO    ERA   W  L  S  SO"

        For I1 = 1 To P6%(I)

            For I2 = 0 To 21

                If P%(I, I2, 15) = I1 Then

                    I4 = 1 - I: Print #2, P$(I, I2); " ";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then
                        Print #2, "(W,"; Right$(Str$(P5%(I, I2, 6) + 1), (Len(Str$(P5%(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7)), (Len(Str$(P5%(I, I2, 7))) - 1)); ")";
                        W$ = P$(I, I2)
                    End If

                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, "(S,"; Right$(Str$(P5%(I, I2, 12) + 1), (Len(Str$(P5%(I, I2, 12) + 1)) - 1)); ")";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then
                        Print #2, "(L,"; Right$(Str$(P5%(I, I2, 6) + 0), (Len(Str$(P5%(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7) + 1), (Len(Str$(P5%(I, I2, 7) + 1)) - 1)); ")";
                        L$ = P$(I, I2)
                    End If

                    If P8%(I, I2, 19) > 0 Then Print #2, "(BS,";: Print #2, Using "##"; P5%(I, I2, 19);: Print #2, ")";
                    If P8%(I, I2, 22) > 0 Then Print #2, "(H,";: Print #2, Using "##"; P5%(I, I2, 22);: Print #2, ")";

                    Print #2, Tab(29);: Print #2, Using "##"; Int(P8%(I, I2, 0) / 3);

                    If P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3) > 0 Then Print #2, ".";: Print #2, Using "#"; P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3);

                    Print #2, Tab(34);
                    Print #2, Using "## ##  ## ## ##"; P8%(I, I2, 1); P8%(I, I2, 2); P8%(I, I2, 3); P8%(I, I2, 4); P8%(I, I2, 5);
                    Print #2, "   ";

                    If P5%(I, I2, 3) > 0 And P5%(I, I2, 0) = 0 Then
                        Print #2, "99.99 ";
                    Else
                        If P5%(I, I2, 0) = 0 Then
                            Print #2, "--.-- ";
                        Else
                            If P5%(I, I2, 3) = 0 Then
                                Print #2, " 0.00 ";
                            Else
                                Print #2, Using "##.## "; P5%(I, I2, 3) / (P5%(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, Using "## "; P5%(I, I2, 6) + 1; Else Print #2, Using "## "; P5%(I, I2, 6);
                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then Print #2, Using "## "; P5%(I, I2, 7) + 1; Else Print #2, Using "## "; P5%(I, I2, 7);
                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, Using "## "; P5%(I, I2, 12) + 1; Else Print #2, Using "## "; P5%(I, I2, 12);

                    Print #2, Using "###"; P5%(I, I2, 5)

                    For I4 = 0 To 5
                        T3%(I4) = T3%(I4) + P8%(I, I2, I4)
                    Next I4
                End If

            Next I2

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

    If E = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (E - 1): Print #2, E$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "2B-";

    If DB = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (DB - 1): Print #2, DB$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "3B-";
    If TR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (TR - 1): Print #2, TR$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "HR-";

    If HR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (HR - 1): Print #2, HR$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "SB-";

    If SB = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (SB - 1): Print #2, SB$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "LOB-"; A$(0); Str$(L0%(0)); ","; A$(1); Str$(L0%(1))
    Print #2, "DP-"; A$(0); Str$(DP%(0)); ","; A$(1); Str$(DP%(1))
    Print #2, "UMPIRES: HP-"; U$(1); "; 1B-"; U$(2); "; 2B-"; U$(3); "; 3B-"; U$(4)
    Print #2, "T: "; gameTime$; " A: "; attendance&
    Print #2,
    Print #2, "INJURIES-";

    If IJL% = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To IJL%: Print #2, IJ$(X1); ",";: Next
        Print #2,
    End If

    peject:
    Print #2, "EJECTIONS-";
    If EJ% = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To EJ%: Print #2, EJ$(X1); ",";: Next
        Print #2,
    End If

    pweath:
    Print #2, "TEMPERATURE:"; TP%
    Print #2, "WIND: "; WI$
    Print #2, "SKY: "; SC$(SC%)
    Print #2, "PRECIPITATION: "; PC$(PC%)
    Print #2, Chr$(12)
    Close #2

    If SPECIAL = 1 Then FCopy diskPaths$(1) + NM$, diskPaths$(1) + "BOX" + Mid$(Str$(Count%), 2, Len(Str$(Count%))) + ".SPC", Space$(4096), ErrCode 'copy the file

    If X = 1 Then

        Open diskPaths$(1) + NM$ For Input As #2

        Do
            Line Input #2, X$
            LPrint X$
        Loop While Left$(X$, 1) <> Chr$(12)

    End If

End Sub

'------------------------------
'      CheckRecords routine
'------------------------------
' ...explanation...
Sub CheckRecords ()

    Shared DB, HR, TR, SB
    'Shared DB$(), E$(), HR$(), SB$(), TR$()

    For I = 0 To 1:
        For I1 = 0 To 21:
            If P8%(I, I1, 1) < 0 Then P8%(I, I1, 1) = 0
        Next I1
    Next I

    For I = 0 To 1:

        For I1 = 0 To 21
            If P%(I, I1, 15) = 1 Then
                If P8%(I, I1, 0) >= 18 And P8%(I, I1, 3) <= 3 Then P8%(I, I1, 21) = 1
            End If
        Next I1

    Next I

    For I = 0 To 1:
        For I1 = 0 To 22

            If B8%(I, I1, 11) <> 0 Then
                'Where does value of E come from?
                'Or from LOADER, it's a single not INT
                If B8%(I, I1, 11) = 1 Then
                    E$(E) = B$(I, I1)
                Else
                    E$(E) = B$(I, I1) + Str$(B8%(I, I1, 11))
                End If

                E$(E) = E$(E) + " (" + Right$(Str$(B5%(I, I1, 11)), (Len(Str$(B5%(I, I1, 11))) - 1)) + ")"
                E = E + 1
            End If

            '        ***CHECKING DOUBLES***
            If B8%(I, I1, 4) <> 0 Then
                If B8%(I, I1, 4) = 1 Then
                    DB$(DB) = B$(I, I1)
                Else
                    DB$(DB) = B$(I, I1) + Str$(B8%(I, I1, 4))
                End If

                DB$(DB) = DB$(DB) + " (" + Right$(Str$(B5%(I, I1, 4)), (Len(Str$(B5%(I, I1, 4))) - 1)) + ")"
                DB = DB + 1

            End If

            '        ***CHECKING TRIPLES***
            If B8%(I, I1, 5) <> 0 Then
                If B8%(I, I1, 5) = 1 Then
                    TR$(TR) = B$(I, I1)
                Else
                    TR$(TR) = B$(I, I1) + Str$(B8%(I, I1, 5))
                End If

                TR$(TR) = TR$(TR) + " (" + Right$(Str$(B5%(I, I1, 5)), (Len(Str$(B5%(I, I1, 5))) - 1)) + ")"
                TR = TR + 1

            End If

            '        ***CHECKING HOME RUNS***
            If B8%(I, I1, 6) <> 0 Then
                If B8%(I, I1, 6) = 1 Then
                    HR$(HR) = B$(I, I1)
                Else
                    HR$(HR) = B$(I, I1) + Str$(B8%(I, I1, 6))
                End If

                HR$(HR) = HR$(HR) + " (" + Right$(Str$(B5%(I, I1, 6)), (Len(Str$(B5%(I, I1, 6))) - 1)) + ")"
                HR = HR + 1

            End If

            '        ***CHECKING STOLEN BASES ***
            If B8%(I, I1, 9) <> 0 Then
                If B8%(I, I1, 9) = 1 Then
                    SB$(SB) = B$(I, I1) + " (" + Right$(Str$(B5%(I, I1, 9)), (Len(Str$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1
                Else
                    SB$(SB) = B$(I, I1) + Str$(B8%(I, I1, 9)) + " (" + Right$(Str$(B5%(I, I1, 9)), (Len(Str$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1
                End If
            End If

        Next I1

    Next I

    For I = 0 To 1

        For I1 = 0 To 21

            If P8%(I, I1, 34) <> 0 Then

                If P8%(I, I1, 34) = 1 Then
                    E$(E) = P$(I, I1)
                Else
                    E$(E) = P$(I, I1) + Str$(P8%(I, I1, 34))
                End If

                E$(E) = E$(E) + " (" + Right$(Str$(P5%(I, I1, 34)), (Len(Str$(P5%(I, I1, 34))) - 1)) + ")"
                E = E + 1

            End If

            '        ***CHECKING DOUBLES***
            If P8%(I, I1, 27) <> 0 Then

                If P8%(I, I1, 27) = 1 Then
                    DB$(DB) = P$(I, I1)
                Else
                    DB$(DB) = P$(I, I1) + Str$(P8%(I, I1, 27))
                End If

                DB$(DB) = DB$(DB) + " (" + Right$(Str$(P5%(I, I1, 27)), (Len(Str$(P5%(I, I1, 27))) - 1)) + ")"
                DB = DB + 1

            End If

            '        ***CHECKING TRIPLES***
            If P8%(I, I1, 28) <> 0 Then

                If P8%(I, I1, 28) = 1 Then
                    TR$(TR) = P$(I, I1)
                Else
                    TR$(TR) = P$(I, I1) + Str$(P8%(I, I1, 28))
                End If

                'Error #9 = index ???
                TR$(TR) = TR$(TR) + " (" + Right$(Str$(P5%(I, I1, 28)), (Len(Str$(P5%(I, I1, 28))) - 1)) + ")"
                TR = TR + 1

            End If

            '        ***CHECKING HOME RUNS***
            If P8%(I, I1, 29) <> 0 Then

                If P8%(I, I1, 29) = 1 Then
                    HR$(HR) = P$(I, I1)
                Else
                    HR$(HR) = P$(I, I1) + Str$(P8%(I, I1, 29))
                End If

                HR$(HR) = HR$(HR) + " (" + Right$(Str$(P5%(I, I1, 29)), (Len(Str$(P5%(I, I1, 29))) - 1)) + ")"
                HR = HR + 1

            End If

            '        ***CHECKING STOLEN BASES ***
            If P8%(I, I1, 32) <> 0 Then
                If P8%(I, I1, 32) = 1 Then
                    SB$(SB) = P$(I, I1) + " (" + Right$(Str$(P5%(I, I1, 32)), (Len(Str$(P5%(I, I1, 32))) - 1)) + ")"
                    SB = SB + 1
                Else
                    SB$(SB) = P$(I, I1) + Str$(P8%(I, I1, 32)) + " (" + Right$(Str$(P5%(I, I1, 32)), (Len(Str$(P5%(I, I1, 32))) - 1)) + ")"
                    SB = SB + 1
                End If
            End If

        Next I1
    Next I

    gameTime$ = "2:"

    'Random # from 5 to 60
    attendance& = Int(Rnd(1) * 55) + 5
    attendance& = attendance& * 1000

    MIN = Int(Rnd(1) * 60)

    If MIN < 10 Then
        MIN$ = "0" + Right$(Str$(MIN), 1): gameTime$ = gameTime$ + MIN$
    Else
        gameTime$ = gameTime$ + Right$(Str$(MIN), 2)
    End If

    For I = 0 To 1:
        For I1 = 0 To 21:
            If P%(I, I1, 15) >= 98 Then P%(I, I1, 15) = 0
        Next I1
    Next I

    For I = 0 To 1:
        For I1 = 0 To 22:
            If B%(I, I1, 21) >= 98 Then B%(I, I1, 21) = 0
            If B%(I, I1, 21) <> 0 Then B8%(I, I1, 14) = 1
        Next I1
    Next I

    Cls
    Print "PLEASE WAIT..."

    For I = 0 To 1

        M = 23
        L = 22

        Do
            7363 '
            I6 = 0
            I7 = 0

            Do
                7364 '
                H1 = 0
                H2 = 0
                H3 = 0

                If X0%(I, 2, I6) > X0%(I, 2, I6 + 1) Then

                    H1 = X0%(I, 2, I6)
                    H2 = X0%(I, 1, I6)
                    H3 = X0%(I, 0, I6)
                    X0%(I, 0, I6) = X0%(I, 0, I6 + 1)
                    X0%(I, 1, I6) = X0%(I, 1, I6 + 1)
                    X0%(I, 2, I6) = X0%(I, 2, I6 + 1)
                    X0%(I, 2, I6 + 1) = H1
                    X0%(I, 1, I6 + 1) = H2
                    X0%(I, 0, I6 + 1) = H3
                    I7 = I6

                End If

                I6 = I6 + 1

            Loop While I6 <= L

            If I7 <> 0 Then
                L = I7 - 1
            End If

        Loop While I7 <> 0

    Next I

End Sub

'------------------------------
'      InningPrint routine
'------------------------------
' ...explanation...
Sub InningPrint (I2)

    If I2 < 4 Then Print #2, Tab(((2 * I2) + 18));
    If I2 > 3 And I2 < 7 Then Print #2, Tab(((2 * I2) + 19));
    If I2 > 6 And I2 < 10 Then Print #2, Tab(((2 * I2) + 20));
    If I2 > 9 And I2 < 13 Then Print #2, Tab(((2 * (I2 - 9)) + 18));
    If I2 > 12 And I2 < 16 Then Print #2, Tab(((2 * (I2 - 9)) + 19));
    If I2 > 15 And I2 < 19 Then Print #2, Tab(((2 * (I2 - 9)) + 20));
    If I2 > 18 And I2 < 22 Then Print #2, Tab(((2 * (I2 - 18)) + 18));
    If I2 > 21 And I2 < 25 Then Print #2, Tab(((2 * (I2 - 18)) + 19));
    If I2 > 24 And I2 < 28 Then Print #2, Tab(((2 * (I2 - 18)) + 20));

End Sub

'------------------------------
'   GETRPBP Subroutine
'------------------------------
' ...explanation...
Sub GETRPBP (pbpIdx%, Y$, D)
    Open "REVISED.PBP" For Random As #1 Len = 44
    Field #1, 44 As YY$
    Get #1, pbpIdx%
    Y$ = _Trim$(YY$)
    Close 1

    'Y$ = _Trim$(YYY$(pbpIdx%))

    If TGTBASE% = 3 And Left$(Y$, 2) = "Q1" Then Y$ = "Q2 " + Right$(Y$, Len(Y$) - 3)
    If TGTBASE% = 3 And Right$(Y$, 2) = "Q1" Then Y$ = Left$(Y$, Len(Y$) - 2) + "Q2"
    If Left$(Y$, 2) = "BB" Then Y$ = Q$(0) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "Q1" Then Y$ = Q$(1) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "Q2" Then Y$ = Q$(2) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "PP" Then Y$ = P$(D, P1%(D)) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "UU" Then Y$ = U$(1) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "XX" Then Y$ = B$(D, B9%(D, 5)) + " and " + B$(D, B9%(D, 6)) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "TT" Then Y$ = B$(D, B9%(D, 5)) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "FB" Then Y$ = B$(D, B9%(D, 3)) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "YY" Then Y$ = B$(D, B9%(D, 4)) + " and " + B$(D, B9%(D, 3)) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "ZZ" Then Y$ = B$(D, B9%(D, 4)) + " and " + B$(D, B9%(D, 6)) + Right$(Y$, Len(Y$) - 2)
    If Left$(Y$, 2) = "Q4" Then Y$ = Q$(4) + Right$(Y$, Len(Y$) - 2)
    If Right$(Y$, 3) = " PP" Then Y$ = Left$(Y$, Len(Y$) - 2) + P$(D, P1%(D))
    If Right$(Y$, 3) = " BB" Then Y$ = Left$(Y$, Len(Y$) - 2) + Q$(0)
    If Right$(Y$, 2) = "Q1" Then Y$ = Left$(Y$, Len(Y$) - 2) + Q$(1)
    If Right$(Y$, 2) = "Q2" Then Y$ = Left$(Y$, Len(Y$) - 2) + Q$(2)
    If Right$(Y$, 2) = "UU" Then Y$ = Left$(Y$, Len(Y$) - 2) + U$(1)
    If Right$(Y$, 2) = "Q4" Then Y$ = Left$(Y$, Len(Y$) - 2) + Q$(4)
    If Right$(Y$, 3) = " TT" Then Y$ = Left$(Y$, Len(Y$) - 2) + B$(D, B9%(D, 5))
    If Right$(Y$, 2) = "FB" Then Y$ = Left$(Y$, Len(Y$) - 2) + B$(D, B9%(D, 3))
    If Right$(Y$, 3) = " XX" Then Y$ = Left$(Y$, Len(Y$) - 2) + B$(D, B9%(D, 5)) + " and " + B$(D, B9%(D, 6))
    If Right$(Y$, 2) = "YY" Then Y$ = Left$(Y$, Len(Y$) - 2) + B$(D, B9%(D, 3)) + " and " + B$(D, B9%(D, 4))
    If Right$(Y$, 3) = " ZZ" Then Y$ = Left$(Y$, Len(Y$) - 2) + B$(D, B9%(D, 6)) + " and " + B$(D, B9%(D, 4))

End Sub

'------------------------------
'   STRIPRPBP Subroutine
'------------------------------
' ...explanation...
Sub STRIPRPBP (Y$, pbpIdx%, D)

    3991 For stringPosition% = 1 To Len(Y$)

        test = Asc(Mid$(Y$, stringPosition%, 1))

        'Or current PBP string has non-printable characters (corrupt data)
        While test > 122 Or test < 32
            'Get a new PBP string to use
            Call GETRPBP(pbpIdx%, Y$, D)
            ' this only works as long as the PBP file structure is the same
            ' pbpIdx% basically is the index of the PBP item to grab and
            ' this is a fixed index depending on the PBP type
        Wend
    Next
End Sub

'------------------------------
'    LDRNRCTDN Subroutine
'------------------------------
' ...explanation...
Sub LDRNRCTDN ()

    If S4% = 7 Or S4% = 6 Or S4% = 5 Or S4% = 3 Then
        A5%(3) = 11
        For I = 0 To 2: A5%(I) = 1: Next
    End If

    If S4% = 2 Or S4% = 4 Then
        A5%(0) = 1
        A5%(1) = 1
        A5%(2) = 11
    End If

    If S4% = 1 Then
        A5%(1) = 11
        A5%(0) = 1
    End If

    If S4% = 0 Then A5%(0) = 11

    If PLB = 0 Then Call PBP(Q$(0) + " bunts it hard!")
    If PLB = 0 Then Call PBP(Q$(4) + " makes an easy play")

    PLB = 0
    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1

    Call ADDSTTS(P, D)
    Call CREDITASSIST(W%, D)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

End Sub

'------------------------------
'   PBP Subroutine
'------------------------------
' ...explanation...

' Has been updated from original
' source to determine Or we're
' printing on GFX or text screen
Sub PBP (F$)

    If pbpDelay! > 0 Then

        F$ = PadRight(F$, 44)
        'Should this ever be more than 44 characters?

        F$ = Left$(F$, 44)
        PB% = PB% + 1

        ' Put PBP text in a slot for line PB%
        If PB% < 9 Then G$(PB%) = F$

        ' Or we're past line 8, move all slots down
        ' to make room
        If PB% > 8 Then

            For I9 = 1 To 7
                G$(I9) = G$(I9 + 1)
            Next

            G$(8) = F$

        End If

        'Print all 8 lines from scratch
        For I9 = 1 To 8

            If usingGfx = 1 Then
                Color 4294967295
                _PrintMode _FillBackground , imageScreen&
                text$ = G$(I9)
                PrintOnGfx 20, I9 + 3, text$, imageScreen&
            Else
                Locate 14 + I9, 1
                Print G$(I9)
            End If

        Next

        Call DELAY

    End If

End Sub

'------------------------------
'   EndOfGame Subroutine
'------------------------------
' ...explanation...
Sub EndOfGame ()
    Cls

    If (usingGfx = 1) Then
        Screen 0
        _FreeImage imageScreen&
    End If

    If normalFinish% = 1 Then Call POSTGAME

End Sub



'INCLUDE: 'Game_Routines_Gfx.BM'

'------------------------------
'   MAINSCREEN_G routine
'------------------------------
' ...explanation...

Sub MAINSCREEN_G (D, p, BC%, U%, MO)

    _PutImage (29 * FontColAdj%, 400)-Step(24 * FontColAdj%, 16), backupScreen&, imageScreen&, (29 * FontColAdj%, 400)-Step(24 * FontColAdj%, 16)

    'Printing white on black
    Color 4294967295
    _PrintMode _FillBackground , imageScreen&

    '-----

    'Pitcher Name
    pitcher$ = Left$(P$(D, P1%(D)), 14)
    PrintOnGfx 65, 26, " " + pitcher$, imageScreen&

    '-- Why is this not Pitch Count / Strikes ???

    'Wins
    FORMATTED$ = FUsing$(Str$(P%(D, P1%(D), 1)), "##", 1)
    pStats$ = FORMATTED$ + "-"
    'Losses
    FORMATTED$ = FUsing$(Str$(P%(D, P1%(D), 2)), "##", 1)
    '                                              ERA
    pStats$ = pStats$ + FORMATTED$ + " " + FUsing$(Str$(P%(D, P1%(D), 10) / 100), "##.##", 1)

    'Pitcher Stats
    PrintOnGfx 66, 27, pStats$, imageScreen&

    '-----

    'IP / H / BB / K headers
    PrintOnGfx 66, 28, "IP   H BB  K", imageScreen&

    If P8%(D, P1%(D), 0) <> 0 Then
        pStats$ = FUsing$(Str$(Int(P8%(D, P1%(D), 0) / 3)), "##.#", 1)
    Else
        pStats$ = " 0.0"
    End If

    '                                      H                                            BB                                                      K
    pStats$ = pStats$ + " " + FUsing$(Str$(P8%(D, P1%(D), 1)), "##", 1) + " " + FUsing$(Str$(P8%(D, P1%(D), 4)), "###", 1) + " " + FUsing$(Str$(P8%(D, P1%(D), 5)), "###", 1)
    PrintOnGfx 65, 29, pStats$, imageScreen&

    J = B3%(p, B1!(p))

    If B7%(p, B1!(p)) = 1 Then
        X$ = P$(p, P1%(p))
        'We're going to use this in just a bit
        'We'll be printing it with more space
        player$ = X$
        X$ = PadRight$(X$, 11)
        Call PITPLACEMENT(p, X$)
    Else
        X$ = B$(p, J)
        'We're going to use this in just a bit
        'We'll be printing it with more space
        player$ = X$
        X$ = PadRight$(X$, 11)

        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color 4294967295

        bpj = B%(p, J, 0)
        pdd = P%(D, P1%(D), 0)

        If B%(p, J, 0) < 0 Or B%(p, J, 0) = 0 And P%(D, P1%(D), 0) > 0 Then
            'Batter Name - Right of Plate
            PrintOnGfx 41, 25, Left$(X$, 11), imageScreen&
        End If

        If B%(p, J, 0) > 0 Or B%(p, J, 0) = 0 And P%(D, P1%(D), 0) < 0 Then
            'Batter Name - Left of Plate
            PrintOnGfx 30, 25, Left$(X$, 11), imageScreen&
        End If

        'Printing white on black
        _PrintMode _FillBackground , imageScreen&

    End If

    player$ = PadRight$(player$, 14)

    'Batter Name - Full
    PrintOnGfx 65, 22, " " + player$, imageScreen&

    'Batter Stats - Header
    PrintOnGfx 65, 23, " AVG HR  BB  K ", imageScreen&

    If B7%(p, B1!(p)) <> 1 Then
        'Batter Stats - Hitter
        If B%(p, J, 4) = 0 Then
            PrintOnGfx 65, 24, ".000  0  0   0", imageScreen&
        Else
            'AVG
            bAvg! = B%(p, J, 6) / B%(p, J, 4)
            If bAvg! = 0 Then
                PrintOnGfx 65, 24, ".000  0  0   0", imageScreen&
            Else
                bb = B%(p, J, 11)
                pStats$ = "." + FUsing$(Str$(bAvg! * 1000), "###", 1)
                '                                 HR                                         BB                                            K
                pStats$ = pStats$ + " " + FUsing$(Str$(B%(p, J, 9)), "##", 1) + " " + FUsing$(Str$(B%(p, J, 11)), "###", 1) + " " + FUsing$(Str$(B%(p, J, 12)), "###", 1)
                PrintOnGfx 65, 24, pStats$, imageScreen&
            End If

        End If

    Else

        'Batter Stats - Pitcher
        If P%(p, P1%(p), 11) <> 999 Then
            If pAvg! = 0 Then
                pStats$ = ".000"
            Else
                pAvg! = P%(p, P1%(p), 11)
                pStats$ = "." + FUsing$(Str$(pAvg! * 1000), "###", 1)
            End If
            PrintOnGfx 65, 24, pStats$ + "           ", imageScreen&
        Else
            If P%(p, P1%(p), 20) = 0 Then
                PrintOnGfx 65, 24, ".000  0   0   0", imageScreen&
            Else
                'AVG
                pAvg! = P%(p, P1%(p), 22) / P%(p, P1%(p), 20)

                If pAvg! = 0 Then
                    PrintOnGfx 65, 24, ".000  0   0   0", imageScreen&
                Else
                    pStats$ = FUsing$(Str$(pAvg! * 1000), ".###", 1)
                    '                                 HR                                                BB                                         K
                    pStats$ = pStats$ + " " + FUsing$(Str$(P%(p, P1%(p), 25)), "##", 1) + " " + FUsing$(Str$(P%(p, P1%(p), 27)), "###", 1) + " " + FUsing$(Str$(P%(p, P1%(p), 28)), "###", 1)
                    PrintOnGfx 65, 24, pStats$, imageScreen&

                End If

            End If
        End If

    End If

    If B7%(p, B1!(p)) <> 1 Then
        today$ = FUsing$(Str$(B8%(p, B3%(p, B1!(p)), 1)), "##", 1) + "-" + FUsing$(Str$(B8%(p, B3%(p, B1!(p)), 0)), "##", 1)
    Else
        today$ = FUsing$(Str$(P8%(p, P1%(p), 24)), "##", 1) + "-" + FUsing$(Str$(P8%(p, P1%(p), 23)), "##", 1)
    End If

    today$ = today$ + " TODAY"

    ' # - # TODAY
    PrintOnGfx 66, 25, today$, imageScreen&

    'Balls
    PrintOnGfx 63, 27, Str$(BC%), imageScreen&

    'Strikes
    PrintOnGfx 63, 28, Str$(U%), imageScreen&

    'Outs
    PrintOnGfx 63, 29, Str$(O%(0)), imageScreen&

    'Infield
    _PutImage (52 * FontColAdj%, 416)-Step(13 * FontColAdj%, 16), backupScreen&, imageScreen&, (52 * FontColAdj%, 416)-Step(13 * FontColAdj%, 16)

    infStatus$ = "INF:"
    Select Case INFPOS%
        Case 0: infStatus$ = infStatus$ + "NORMAL    "
        Case 1: infStatus$ = infStatus$ + "CORNERS IN"
        Case 2: infStatus$ = infStatus$ + "INFIELD IN"
    End Select

    'Printing black on background
    _PrintMode _KeepBackground , imageScreen&
    Color 4278190080
    PrintOnGfx 52, 26, infStatus$, imageScreen&

    'Printing white on black
    Color 4294967295
    _PrintMode _FillBackground , imageScreen&
    Call HOLDSTATUS_G(H0%)

    'RUNNER AT SECOND
    I2 = 2
    X = 37
    Call DisplayRun_G(I2, p, X)

    If O%(0) <> 3 Then MO = 0

    For WK = 1 To 3
        If B0%(WK, 0) > 0 Then MO = MO + 1
    Next

    'RUNNER AT THIRD
    I2 = 3
    X = 15
    Call DisplayRun_G(I2, p, X)

    'RUNNER AT FIRST
    I2 = 1
    X = 56
    Call DisplayRun_G(I2, p, X)

End Sub

'------------------------------
'   DisplayRun_G Subroutine
'------------------------------
' ...explanation...
Sub DisplayRun_G (I2, p, X)

    'I2 = Base #

    Select Case I2
        '1st Base Runner
        Case 1:
            '_PutImage (54 * FontColAdj%, 336)-Step(11 * FontColAdj%, 32), backupScreen&, imageScreen&, (54 * FontColAdj%, 336)-Step(11 * FontColAdj%, 32)
            _PutImage (54 * FontColAdj%, 336)-Step(14 * FontColAdj%, 16), backupScreen&, imageScreen&, (54 * FontColAdj%, 336)-Step(14 * FontColAdj%, 16)
            _PutImage (54 * FontColAdj%, 352)-Step(11 * FontColAdj%, 16), backupScreen&, imageScreen&, (54 * FontColAdj%, 352)-Step(11 * FontColAdj%, 16)

            '2nd Base Runner
        Case 2: _PutImage (35 * FontColAdj%, 304)-Step(14 * FontColAdj%, 32), backupScreen&, imageScreen&, (35 * FontColAdj%, 304)-Step(14 * FontColAdj%, 32)

            '3rd Base Runner
        Case 3: _PutImage (15 * FontColAdj%, 336)-Step(14 * FontColAdj%, 32), backupScreen&, imageScreen&, (15 * FontColAdj%, 336)-Step(14 * FontColAdj%, 32)

    End Select

    J = B0%(I2, 0)

    If J <> 0 Then

        Y = 22
        If I2 = 2 Then Y = 20

        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color 4294967295

        If B7%(p, J) = 1 Then
            If P%(p, P1%(p), 11) <> 999 Then
                runner$ = P$(p, P1%(p))
                runner$ = PadRight(runner$, 11)
                PrintOnGfx X, Y - 1, runner$, imageScreen&
                PrintOnGfx X, Y, "1/  0", imageScreen&
            Else
                runner$ = P$(p, P1%(p))
                runner$ = PadRight(runner$, 11)
                PrintOnGfx X, Y - 1, runner$, imageScreen&
                runStats$ = FUsing$(Str$(P%(p, P1%(p), 31)), "#", 1) + "/" + FUsing$(Str$(P%(p, P1%(p), 29)), "###", 1)
                PrintOnGfx X, Y, runStats$, imageScreen&
            End If
        Else
            runner$ = B$(p, B3%(p, J))
            runner$ = PadRight(runner$, 11)
            PrintOnGfx X, Y - 1, runner$, imageScreen&
            runStats$ = FUsing$(Str$(B%(p, B3%(p, J), 16)), "#", 1) + "/" + FUsing$(Str$(B%(p, B3%(p, J), 13)), "###", 1)
            PrintOnGfx X, Y, runStats$, imageScreen&

            'Printing white on black
            _PrintMode _FillBackground , imageScreen&
        End If

    End If

End Sub

'------------------------------
'   DisplayBallField_Gfx Subroutine
'------------------------------
' ...explanation...
Sub DisplayBallField_Gfx (BC%, p, U%, D)

    ReDim FORMATTED$(0 To 2)

    inningStart% = 1
    inningEnd% = 9

    If INNING% > 9 Then X% = 10: inningStart% = 18
    If INNING% > 18 Then X% = 19: inningEnd% = 27

    'Print Batter / Pitcher box
    '----------------------------------------
    For I = 23 To 30
        PrintOnGfx 65, I - 1, Space$(16), imageScreen&
    Next

    'Print Score Box
    '----------------------------------------
    For I = 28 To 30
        PrintOnGfx 15, I - 1, Space$(51), imageScreen&
    Next

    For X1% = 0 To 2
        '3 innings at a time
        FORMATTED$(X1%) = FUsing$(Str$(inningStart% + 3 * X1%), "##", 1) + " " + FUsing$(Str$(inningStart% + 3 * X1% + 1), "##", 1) + " " + FUsing$(Str$(inningStart% + 3 * X1% + 2), "##", 1) + "  "
    Next

    'Print innings, headers for runs / hits / errors, along with balls
    FORMATTED$ = FORMATTED$(0) + FORMATTED$(1) + FORMATTED$(2) + " R  H  E B" + Str$(BC%)
    PrintOnGfx 23, 27, FORMATTED$, imageScreen&

    For I = 0 To 1

        'Print inning score
        PrintOnGfx 15, I + 28, Left$(A$(I), 3) + NN$(I), imageScreen&

        For I2 = inningStart% To inningEnd%
            If p = 0 And I = 0 And I2 > INNING% - 1 And S%(I, INNING%) = 0 Or p = 0 And I = 0 And I2 > INNING% And S%(I, INNING%) > 0 Or p = 1 And I2 > INNING% And I = 0 Then Exit For
            If p = 0 And I = 1 And I2 > INNING% - 1 Or p = 1 And I = 1 And S%(I, INNING%) = 0 And I2 > INNING% - 1 Or p = 1 And I = 1 And S%(I, INNING%) > 0 And I2 > INNING% Then Exit For
            Call INNDISP_G(I2, I)
        Next I2

        R% = S6%(I, 0)
        H% = S6%(I, 1)
        E% = S6%(I, 2)

        '                         R                                         H                                                      E
        FORMATTED$ = FUsing$(Str$(S6%(I, 0)), "##", 1) + " " + FUsing$(Str$(S6%(I, 1)), "##", 1) + " " + FUsing$(Str$(S6%(I, 2)), "##", 1) + " "

        If I = 0 Then
            'Strikes
            FORMATTED$ = FORMATTED$ + "S" + Str$(U%)
        Else
            'Outs
            FORMATTED$ = FORMATTED$ + "O" + Str$(O%(0))
        End If

        PrintOnGfx 53, I + 28, FORMATTED$, imageScreen&

    Next I

    'Center Field
    _PutImage (36 * FontColAdj%, 224)-Step(13 * FontColAdj%, 32), backupScreen&, imageScreen&, (36 * FontColAdj%, 224)-Step(13 * FontColAdj%, 32)

    'Left Field
    _PutImage (7 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32), backupScreen&, imageScreen&, (7 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32)

    'Right Field
    _PutImage (64 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32), backupScreen&, imageScreen&, (64 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32)

    'Short Stop
    _PutImage (28 * FontColAdj%, 272)-Step(12 * FontColAdj%, 32), backupScreen&, imageScreen&, (28 * FontColAdj%, 272)-Step(12 * FontColAdj%, 32)

    '2nd Base
    _PutImage (46 * FontColAdj%, 272)-Step(13 * FontColAdj%, 32), backupScreen&, imageScreen&, (46 * FontColAdj%, 272)-Step(13 * FontColAdj%, 32)

    '3rd Base
    _PutImage (13 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32), backupScreen&, imageScreen&, (13 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32)

    '1st Base
    _PutImage (61 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32), backupScreen&, imageScreen&, (61 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32)

    'Pitcher
    _PutImage (35 * FontColAdj%, 336)-Step(13 * FontColAdj%, 32), backupScreen&, imageScreen&, (35 * FontColAdj%, 336)-Step(13 * FontColAdj%, 32)

    'Catcher
    _PutImage (35 * FontColAdj%, 416)-Step(13 * FontColAdj%, 32), backupScreen&, imageScreen&, (35 * FontColAdj%, 416)-Step(13 * FontColAdj%, 32)

    player$ = Left$(B$(D, B9%(D, 8)), 11)
    player$ = PadRight(player$, 11)

    'Print all positions around the diamond
    '----------------------------------------
    'Printing black on background
    _PrintMode _KeepBackground , imageScreen&
    Color 4278190080

    'Center Field Name
    PrintOnGfx 36, 14, player$, imageScreen&

    'Center Field Stats
    If B%(D, B9%(D, 8), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 8), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 8), 19)), "###", 1)
        End If
    End If

    player$ = "A" + FUsing$(Str$(B%(D, B9%(D, 8), 15)), "#", 1) + " R" + FUsing$(Str$(B%(D, B9%(D, 8), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 36, 15, player$, imageScreen&

    'Left Field Name
    player$ = Left$(B$(D, B9%(D, 7)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 7, 16, player$, imageScreen&

    'Left Field Stats
    If B%(D, B9%(D, 7), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 7), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 7), 19)), "###", 1)
        End If
    End If

    player$ = "A" + FUsing$(Str$(B%(D, B9%(D, 7), 15)), "#", 1) + " R" + FUsing$(Str$(B%(D, B9%(D, 7), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 7, 17, player$, imageScreen&

    'Right Field Name
    player$ = Left$(B$(D, B9%(D, 9)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 64, 16, player$, imageScreen&

    'Right Field Stats
    If B%(D, B9%(D, 9), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 9), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 9), 19)), "###", 1)
        End If
    End If

    player$ = "A" + FUsing$(Str$(B%(D, B9%(D, 9), 15)), "#", 1) + " R" + FUsing$(Str$(B%(D, B9%(D, 9), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 64, 17, player$, imageScreen&

    'Short Stop Name
    player$ = Left$(B$(D, B9%(D, 6)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 28, 17, player$, imageScreen&

    'Short Stop Stats
    If B%(D, B9%(D, 6), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 6), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 6), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(B%(D, B9%(D, 6), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 28, 18, player$, imageScreen&

    '2nd Base Name
    player$ = Left$(B$(D, B9%(D, 4)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 46, 17, player$, imageScreen&

    '2nd Base Stats
    If B%(D, B9%(D, 4), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 4), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 4), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(B%(D, B9%(D, 4), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 46, 18, player$, imageScreen&

    '3rd Base Name
    player$ = Left$(B$(D, B9%(D, 5)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 13, 19, player$, imageScreen&

    '3rd Base Stats
    If B%(D, B9%(D, 5), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 5), 19) = 0 Then
            fldPct$ = " .000"
        Else

            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 5), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(B%(D, B9%(D, 5), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 13, 20, player$, imageScreen&

    '1st Base Name
    player$ = Left$(B$(D, B9%(D, 3)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 61, 19, player$, imageScreen&

    '1st Base Stats
    If B%(D, B9%(D, 1), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If B%(D, B9%(D, 1), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(B%(D, B9%(D, 1), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(B%(D, B9%(D, 3), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 61, 20, player$, imageScreen&

    'Pitcher Name
    player$ = "[" + B1$(P%(D, P1%(D), 0) + 2) + "] " + Left$(P$(D, P1%(D)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 35, 21, player$, imageScreen&

    'Pitcher Stats
    If P%(D, P1%(D), 17) = 1000 Then
        fldPct$ = "1.000"
    Else
        If P%(D, P1%(D), 17) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(P%(D, P1%(D), 17)), "###", 1)
        End If
    End If

    player$ = "H" + FUsing$(Str$(P%(D, P1%(D), 18)), "#", 1) + " F" + fldPct$
    'was column 39 previously
    PrintOnGfx 35, 22, player$, imageScreen&

    'Catcher Name
    player$ = Left$(B$(D, B9%(D, 2)), 10)
    player$ = PadRight(player$, 10)
    player$ = player$ + "A" + FUsing$(Str$(B%(D, B9%(D, 2), 15)), "#", 1)
    PrintOnGfx 35, 26, player$, imageScreen&

    'Printing white on black
    Color 4294967295
    _PrintMode _FillBackground , imageScreen&

End Sub

'------------------------------
'   HOLDSTATUS_G Subroutine
'------------------------------
' ...explanation...
Sub HOLDSTATUS_G (H0%)

    _PutImage (53 * FontColAdj%, 400)-Step(12 * FontColAdj%, 16), backupScreen&, imageScreen&, (53 * FontColAdj%, 400)-Step(12 * FontColAdj%, 16)

    rhStatus$ = "RUNNERS:"
    'Can this only be 0 | 1???
    If H0% = 0 Then rhStatus$ = rhStatus$ + "    "
    If H0% = 1 Then rhStatus$ = rhStatus$ + "HELD"

    'Printing black on background
    _PrintMode _KeepBackground , imageScreen&
    Color 4278190080
    PrintOnGfx 53, 25, rhStatus$, imageScreen&

    'Printing white on black
    Color 4294967295
    _PrintMode _FillBackground , imageScreen&

End Sub

'------------------------------
'   INNDISP_G Subroutine
'------------------------------
' ...explanation...
Sub INNDISP_G (currInning, teamLine)

    If currInning < 4 Then L = 3 * currInning + 7
    If currInning > 3 And currInning < 7 Then L = 3 * currInning + 8
    If currInning > 6 And currInning < 10 Then L = 3 * currInning + 9
    If currInning > 9 And currInning < 13 Then L = 3 * (currInning - 9) + 7
    If currInning > 12 And currInning < 16 Then L = 3 * (currInning - 9) + 8
    If currInning > 15 And currInning < 19 Then L = 3 * (currInning - 9) + 9
    If currInning > 18 And currInning < 22 Then L = 3 * (currInning - 18) + 7
    If currInning > 21 And currInning < 25 Then L = 3 * (currInning - 18) + 8
    If currInning > 24 And currInning < 28 Then L = 3 * (currInning - 18) + 9

    FORMATTED$ = FUsing$(Str$(S%(teamLine, currInning)), "##", 1)
    PrintOnGfx L + 13, teamLine + 28, FORMATTED$, imageScreen&

End Sub

'------------------------------
'   LOADSCREEN Subroutine
'------------------------------
' ...explanation...
Sub LOADSCREEN ()

    parkImage& = _LoadImage(".\parks\" + PARK$, 32)
    imageWidth% = _Width(parkImage&)
    imageHeight% = _Height(parkImage&)
    imageScreen& = _NewImage(imageWidth%, imageHeight%, 32)
    backupScreen& = _NewImage(imageWidth%, imageHeight%, 32)

    Screen imageScreen&
    Cls
    _PutImage (0, 0), parkImage&, 0

    'Backup several areas of image where player text will go

    'Play-by-Play box
    _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 144), imageScreen&, backupScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 144)

    'Runners Hold section
    _PutImage (53 * FontColAdj%, 400)-Step(12 * FontColAdj%, 16), imageScreen&, backupScreen&, (53 * FontColAdj%, 400)-Step(12 * FontColAdj%, 16)

    'Infield
    _PutImage (52 * FontColAdj%, 416)-Step(14 * FontColAdj%, 16), imageScreen&, backupScreen&, (52 * FontColAdj%, 416)-Step(14 * FontColAdj%, 16)

    'Center Field
    _PutImage (36 * FontColAdj%, 224)-Step(13 * FontColAdj%, 32), imageScreen&, backupScreen&, (36 * FontColAdj%, 224)-Step(13 * FontColAdj%, 32)

    'Left Field
    _PutImage (7 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32), imageScreen&, backupScreen&, (7 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32)

    'Right Field
    _PutImage (64 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32), imageScreen&, backupScreen&, (64 * FontColAdj%, 256)-Step(16 * FontColAdj%, 32)

    'Short Stop
    _PutImage (28 * FontColAdj%, 272)-Step(12 * FontColAdj%, 32), imageScreen&, backupScreen&, (28 * FontColAdj%, 272)-Step(12 * FontColAdj%, 32)

    '2nd Base
    _PutImage (46 * FontColAdj%, 272)-Step(13 * FontColAdj%, 32), imageScreen&, backupScreen&, (46 * FontColAdj%, 272)-Step(13 * FontColAdj%, 32)

    '3rd Base
    _PutImage (13 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32), imageScreen&, backupScreen&, (13 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32)

    '1st Base
    _PutImage (61 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32), imageScreen&, backupScreen&, (61 * FontColAdj%, 304)-Step(12 * FontColAdj%, 32)

    'Pitcher
    _PutImage (35 * FontColAdj%, 336)-Step(13 * FontColAdj%, 32), imageScreen&, backupScreen&, (35 * FontColAdj%, 336)-Step(13 * FontColAdj%, 32)

    'Catcher
    _PutImage (35 * FontColAdj%, 416)-Step(13 * FontColAdj%, 16), imageScreen&, backupScreen&, (35 * FontColAdj%, 416)-Step(13 * FontColAdj%, 16)

    '2nd Base Runner
    _PutImage (37 * FontColAdj%, 304)-Step(14 * FontColAdj%, 32), imageScreen&, backupScreen&, (37 * FontColAdj%, 304)-Step(14 * FontColAdj%, 32)

    '3rd Base Runner
    _PutImage (15 * FontColAdj%, 336)-Step(14 * FontColAdj%, 32), imageScreen&, backupScreen&, (15 * FontColAdj%, 336)-Step(14 * FontColAdj%, 32)

    '1st Base Runner Name
    _PutImage (54 * FontColAdj%, 336)-Step(14 * FontColAdj%, 16), imageScreen&, backupScreen&, (54 * FontColAdj%, 336)-Step(14 * FontColAdj%, 16)
    '1st Base Runner Stats
    _PutImage (54 * FontColAdj%, 352)-Step(11 * FontColAdj%, 16), imageScreen&, backupScreen&, (54 * FontColAdj%, 352)-Step(11 * FontColAdj%, 16)

    'Batter - Right
    _PutImage (30 * FontColAdj%, 400)-Step(23 * FontColAdj%, 16), imageScreen&, backupScreen&, (30 * FontColAdj%, 400)-Step(23 * FontColAdj%, 16)

    'Commands for Human Play
    _PutImage (1 * FontColAdj%, 304)-Step(14 * FontColAdj%, 176), imageScreen&, backupScreen&, (1 * FontColAdj%, 304)-Step(14 * FontColAdj%, 176)

End Sub


'------------------------------
'   PrintOnGfx Subroutine
'------------------------------
' ...explanation...
Sub PrintOnGfx (Col%, Row%, Text$, tgtScrn&)

    PrintCol% = (Col% * _PrintWidth("W"))
    PrintRow% = (Row% * _FontHeight)
    _PrintString (PrintCol%, PrintRow%), Text$, tgtScrn&

End Sub



'INCLUDE: 'SOURCE.BM'


'------------------------------
'       SOURCE Subroutine
'------------------------------
' ...explanation...
Sub SOURCE ()

    'Declare all variables for the modules
    Dim F1$(10)
    Dim CZ%(0 To 1), PH%(0 To 22, 0 To 1)

    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

    If usingGfx = 1 Then Call LOADSCREEN

    15000 '
    'PQ = 0

    F% = 0

    For I = 0 To 1
        O%(I) = 0
    Next I

    L0%(P) = L0%(P) + MO
    MO = 0
    H0% = 0

    For I = 1 To 3
        For I1 = 0 To 1
            B0%(I, I1) = 0
            B1%(I, I1) = 0
        Next I1
        B0%(I, 2) = 0
    Next I

    S4% = 0

    If P = 1 Then
        INNING% = INNING% + 1
        CZ%(0) = 0
        CZ%(1) = 0
    End If

    'Switch posession
    P = 1 - P
    D = 1 - P

    If INNING% = 7 And L%(1, 13) = 8 And P = 1 And Val(Left$(A$(1), 2)) > 75 Then
        Call WINDEX
        Call PBP("ALL RIGHT..."): Call PBP("LEMME HEAR YA..."): Call PBP("A ONE, A TWO...")
    End If

    'Check status of scores / innings
    '----------------------------------------
    If INNING% > 9 And D = 1 And S6%(1, 0) <> S6%(0, 0) Or INNING% = 28 Or INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) > 0 Then
        'After 9th inning, score not tied, etc..
        'End of game!
        GoTo 18500
    End If

    If RD = 1 And INNING% >= RE And RE >= 6 And (S6%(1, 0) > S6%(0, 0) And P = 1 Or S6%(0, 0) > S6%(1, 0) And P = 0) Then
        Cls
        If usingGfx = 0 Then Screen 0
        Print "THIS GAME HAS BEEN CALLED ON ACCOUNT OF RAIN!!!"
        Print "ALL RECORDS ARE OFFICIAL!!"
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

        'End of game!
        GoTo 18500
    End If

    If RD = 1 And P = 0 And INNING% = 4 Then
        Call PBP("THE RAIN IS FALLING A LITTLE MORE STEADILY")
        Call PBP("THIS GAME MAY NOT GO 9!")
        I$ = GetKeyPress$
    End If

    'Game is not over - show the field
    If usingGfx = 1 Then
        Call DisplayBallField_Gfx(BC%, P, U%, D)
    Else
        Call DisplayBallField(BC%, P, U%, D)
    End If

    For IJ0% = 0 To 22

        If B%(D, IJ0%, 21) > 39 And B%(D, IJ0%, 21) <= 49 Then

            I1 = IJ0%
            I2 = B%(D, IJ0%, 21) - 40

            B%(D, I1, 21) = 1
            B3%(D, I2) = I1
            B9%(D, B7%(D, I2)) = I1

            D0%(D) = D0%(D) + 1
            X0%(D, 0, D0%(D)) = B3%(D, I2)
            X0%(D, 1, D0%(D)) = B7%(D, I2)
            X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)

            W5 = D
            W6 = D
            VV(D) = I2
            VV1(D) = I2

            For xx = 22 To 25
                If B7%(D, I2) = B%(D, I1, xx) Then
                    B%(D, I1, 15) = B%(D, I1, 33 + (xx - 22) * 2)
                    B%(D, I1, 17) = B%(D, I1, 34 + (xx - 22) * 2)
                    B%(D, I1, 19) = B%(D, I1, xx + 4)
                    Exit For
                End If
            Next

            '14955  ---> this is it's own routine used often
            Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

            If usingGfx = 1 Then
                Call DisplayBallField_Gfx(BC%, P, U%, D)
                Call MAINSCREEN_G(D, P, BC%, U%, MO)
            Else
                Call DisplayBallField(BC%, P, U%, D)
                Call MAINSCREEN(D, P, BC%, U%, MO)
            End If

            VV(D) = 1
            VV1(D) = 9

        End If

    Next

    A1 = S6%(D, 0) - S6%(P, 0)

    If Not (MG%(D, 560) <> 999 Or INNING% < 8 Or U6 = 0 Or U6 = 1 And P = U9) Then

        For IJ0% = 1 To 4

            If MG%(D, 560 + IJ0%) <> -1 And B%(D, MG%(D, 564 + IJ0%), 21) = 0 Then

                Call d100(RN)

                If B9%(D, MG%(D, 576 + IJ0%)) = MG%(D, 560 + IJ0%) And RN <= MG%(D, 568 + IJ0%) And A1 >= MG%(D, 572 + IJ0%) Then

                    I1 = MG%(D, 564 + IJ0%)

                    For I2 = 1 To 9
                        If B7%(D, I2) = MG%(D, 576 + IJ0%) Then Exit For
                    Next

                    B%(D, I1, 21) = 1
                    B3%(D, I2) = I1
                    B9%(D, B7%(D, I2)) = I1

                    D0%(D) = D0%(D) + 1
                    X0%(D, 0, D0%(D)) = B3%(D, I2)
                    X0%(D, 1, D0%(D)) = B7%(D, I2)
                    X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)

                    W5 = D
                    W6 = D
                    VV(D) = I2
                    VV1(D) = I2

                    For xx = 22 To 25
                        If B7%(D, I2) = B%(D, I1, xx) Then
                            B%(D, I1, 15) = B%(D, I1, 33 + (xx - 22) * 2)
                            B%(D, I1, 17) = B%(D, I1, 34 + (xx - 22) * 2)
                            B%(D, I1, 19) = B%(D, I1, xx + 4)
                            Exit For
                        End If
                    Next

                    Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

                    If usingGfx = 1 Then
                        Call DisplayBallField_Gfx(BC%, P, U%, D)
                        Call MAINSCREEN_G(D, P, BC%, U%, MO)
                    Else
                        Call DisplayBallField(BC%, P, U%, D)
                        Call MAINSCREEN(D, P, BC%, U%, MO)
                    End If

                    VV(D) = 1
                    VV1(D) = 9

                End If

            End If

        Next

    End If

    15025 '
    If B9%(D, 1) = 99 And U6 = 0 Or B9%(D, 1) = 99 And U6 = 1 And U9 = 1 - D Then
        '----- move stuff here -----
        GoTo 14900
    End If

    15050 '
    'PQ = 0
    SH = 0
    F% = 0
    SO = 0
    B1!(P) = B1!(P) + 1
    INFPOS% = 0
    U% = 0

    If B1!(P) = 10 Then B1!(P) = 1

    '--- Start of a new sub
    15051 '
    Call WHOSON1ST(S4%)
    Call PLAYERNAME(P)

    If usingGfx = 1 Then
        Call MAINSCREEN_G(D, P, BC%, U%, MO)
    Else
        Call MAINSCREEN(D, P, BC%, U%, MO)
    End If

    P2 = 0
    D2 = 0

    ' Game over - there's a winner
    If INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) > 0 Then 18500

    15052 '
    Do
        If usingGfx = 1 Then
            _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128)
        Else
            Color 14, 0
        End If

        I2 = INNING%

        I0 = P

        If S%(P, INNING%) > 0 Then
            If usingGfx = 1 Then
                Call INNDISP_G(I2, I0)
            Else
                Call INNDISP(I2, I0)
            End If
        End If

        If usingGfx = 1 Then

            FORMATTED$ = FUsing$(Str$(S6%(I0, 0)), "##", 1)
            PrintOnGfx 53, I0 + 28, FORMATTED$, imageScreen&

            FORMATTED$ = FUsing$(Str$(S6%(D, 2)), "##", 1)
            PrintOnGfx 59, 1 - I0 + 28, FORMATTED$, imageScreen&

        Else

            Locate , 40
            Print Using "##"; S6%(I0, 0);
            Locate D + 2, 46
            Print Using "##"; S6%(D, 2);
            Color 15, 0

        End If

        Z2% = B0%(2, 0)

        'PQ = 0
        F% = 0

        15080 '
        If U6 = 2 Or U6 = 1 And P <> U9 Then 18700

        If S4% = 1 Or S4% = 5 Then
            H0% = 1
            If usingGfx = 1 Then
                Call HOLDSTATUS_G(H0%)
            Else
                Call HOLDSTATUS(H0%)
            End If
        End If

        '----------------------------------------
        ' DEFENSE OPTIONS MENU
        '----------------------------------------
        15081 '
        F$ = PadRight$(Left$(A$(D), 3) + NN$(D), 8)

        'Call WINDEX

        If usingGfx = 1 Then

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color 4278190080

            PrintOnGfx 1, 19, F$, imageScreen&

            F1$(1) = "1-PITCH       "
            F1$(2) = "2-INT WALK    "
            F1$(3) = "3-INF IN      "
            F1$(4) = "4-CORNERS IN  "
            F1$(5) = "5-INF NORMAL  "
            F1$(6) = "6-PERSONNEL   "
            F1$(7) = "8-PITCH AROUND"
            F1$(8) = "G-GUARD LINES "
            F1$(9) = "H-HOLD RUNNER "

            '-- B/P do not do anything
            '-- but could be programmed to
            F1$(10) = "S/D/B/P STATS "

            For I = 21 To 30
                PrintOnGfx 1, I - 1, F1$(I - 20), imageScreen&
            Next

        Else

            Color 14, 0: Print F$

            Print " 1-PITCH          D-DETAILED STATS      "
            Print " 2-INTENTIONAL BB S-BATTER/PITCHER STATS"
            Print " 3-INFIELD IN     H-HOLD RUNNER"
            Print " 4-CORNERS IN     Q-QUIT GAME"
            Print " 5-INFIELD NORMAL W-WEATHER CONDITIONS"
            Print " 6-PERSONNEL      G-GUARD LINES"
            Print " 7-PITCH/HIT AWAY O-GAME OPTIONS"
            Print " 8-PITCH AROUND"
            Color 15, 0

        End If

        15090 '
        I$ = GetKeyPress$

        'Restore image where commands went
        If usingGfx = 1 Then _PutImage (1 * FontColAdj%, 304)-Step(14 * FontColAdj%, 176), backupScreen&, imageScreen&, (1 * FontColAdj%, 304)-Step(14 * FontColAdj%, 176)

        If UCase$(I$) = "D" Then Call VIEWTEAM: GoTo 15580

        If UCase$(I$) = "G" Then

            INFPOS% = 3

            If usingGfx = 1 Then
                F$ = "INF:GUARD LINE"
                _PutImage (52 * FontColAdj%, 416)-Step(14 * FontColAdj%, 16), backupScreen&, imageScreen&, (52 * FontColAdj%, 416)-Step(14 * FontColAdj%, 16)

                'Printing black on background
                _PrintMode _KeepBackground , imageScreen&
                Color 4278190080

                PrintOnGfx 52, 26, F$, imageScreen&

                'Printing white on black
                Color 4294967295
                _PrintMode _FillBackground , imageScreen&
            Else
                Color 15, 2
                Locate 23, 46: Print "GUARD LINE"
            End If

            GoTo 15052

        End If

        If UCase$(I$) = "L" Then Call PopUp_Lineup: GoTo 15580

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 15580
        End If

        If UCase$(I$) = "Q" Then
            If usingGfx = 0 Then Cls
            Call EndOfGame
            End
        End If

        If UCase$(I$) = "S" Then Call POPUPSTATS: GoTo 15580

        If UCase$(I$) = "B" Then
            If usingGfx = 1 Then Screen 0
            Call PopUp_Batter(D, P1%(D)): GoTo 15580
        End If

        If UCase$(I$) = "P" Then
            If usingGfx = 1 Then Screen 0
            Call PopUp_Pitcher(P, P1%(P)): GoTo 15580
        End If

        If UCase$(I$) = "W" Then
            Call WEATHER
            GoTo 15580
        End If

        If UCase$(I$) = "H" Then
            H0% = 1 - H0%
            If usingGfx = 1 Then
                Call HOLDSTATUS_G(H0%)
            Else
                Call HOLDSTATUS(H0%)
            End If
            GoTo 15081
        End If

        If Val(I$) = 7 Then P2 = 1: D2 = 1: GoTo 13050

        15054 '
        D2 = Val(I$)

    Loop Until D2 >= 1 And D2 <= 9

    15055 '
    Select Case D2
        Case 1
            '

        Case 2
            Call WINDEX
            Call BASEONBALLS(S2%, D2, D, P)
            GoTo 13201

        Case 3
            INFPOS% = 2
            infStatus$ = "INF:INFIELD IN"

        Case 4
            INFPOS% = 1
            infStatus$ = "INF:CORNERS IN"

        Case 5
            INFPOS% = 0
            infStatus$ = "INF:NORMAL    "

        Case 6
            P9 = D
            W5 = -1
            W6 = -1
            GoTo 15500

    End Select

    'We've assigned the text,
    'now we have to print it on the screen
    If D2 >= 3 And D2 <= 5 Then

        If usingGfx = 1 Then
            'Restore image where options go
            _PutImage (52 * FontColAdj%, 416)-Step(14 * FontColAdj%, 16), backupScreen&, imageScreen&, (52 * FontColAdj%, 416)-Step(14 * FontColAdj%, 16)

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color 4278190080

            'Print infield status
            PrintOnGfx 52, 26, infStatus$, imageScreen&

            'Printing white on black
            Color 4294967295
            _PrintMode _FillBackground , imageScreen&
        Else
            Color 15, 2
            Locate 23, 46: Print infStatus$
        End If

        GoTo 15052

    End If

    '----------------------------------------
    ' OFFENSE OPTIONS MENU
    '----------------------------------------
    15085 '
    Do

        If (U6 = 2 Or U6 = 1 And P = U9) Then

            18900 '
            JM = B3%(P, B1!(P))

            If PPH <> 1 Then

                A1 = S6%(P, 0) - S6%(D, 0)

                If B7%(P, B1!(P)) <> 1 Then

                    If MG%(P, 847) = 999 Then

                        18991 'PH FOR POS PLAYER CASES
                        Call d100(RN)
                        MOB% = 0

                        For I = 1 To 3
                            If B0%(I, 0) > 0 Then MOB% = MOB% + 1
                        Next

                        If B%(P, B3%(P, B1!(P)), 4) = 0 Then
                            I7! = 0
                            I6! = 0
                        Else
                            I7! = Int(B%(P, B3%(P, B1!(P)), 6) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                            I6! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                        End If

                        18992 '
                        For I = 0 To 2
                            If INNING% >= MG%(P, 848 + I * 6) And A1 <= MG%(P, 849 + I * 6) * -1 And I6! <= MG%(P, 851 + I * 6) And I7! <= MG%(P, 852 + I * 6) And RN <= MG%(P, 853 + I * 6) And MOB% >= MG%(P, 850 + I * 6) Then 18909
                        Next

                    Else

                        If B%(P, JM, 4) = 0 Then 18909

                        If Not (B%(P, JM, 6) / B%(P, JM, 4) * 1000 > 233 Or B%(P, JM, 9) >= 15) Then
                            If INNING% > 6 And A1 < -2 Or A1 <= 0 And INNING% >= 8 Or A1 = 0 And INNING% >= 9 Then 18909
                        End If

                    End If

                End If

            Else

                For I = 0 To 21

                    If P%(P, I, 15) = 0 Then

                        If MG%(P, 590) = 999 Then

                            18985 'PH FOR PITCHER WITH PROFILE
                            Call d100(RN)

                            MOB% = 0

                            For I = 1 To 3
                                If B0%(I, 0) > 0 Then MOB% = MOB% + 1
                            Next

                            If P%(P, P1%(P), 11) <> 999 Then
                                I7! = P%(I0, P1%(I0), 11)
                            Else
                                If P%(P, P1%(P), 20) = 0 Then
                                    I7! = 0
                                Else
                                    I7! = Int((P%(P, P1%(P), 22) / P%(P, P1%(P), 20) * 1000 + .5))
                                End If
                            End If

                            18986 '
                            For I = 0 To 2
                                If INNING% >= MG%(P, 591 + I * 6) And A1 <= MG%(P, 592 + I * 6) * -1 And O%(0) <= MG%(P, 594 + I * 6) And I7! <= MG%(P, 595 + I * 6) And RN <= MG%(P, 596 + I * 6) And MOB% >= MG%(P, 593 + I * 6) Then 18930
                            Next

                        Else

                            If INNING% > 4 And INNING% < 7 And A1 < -2 And S6%(P, 0) > 0 Then 18930
                            If INNING% > 4 And INNING% < 7 And A1 < -3 Then 18930
                            If INNING% > 6 And A1 < 0 Or A1 = 0 And INNING% > 6 And INNING% < 9 And S4% > 1 Then 18930
                            If A1 = 0 And INNING% >= 9 And S4% > 0 Or A1 = 0 And INNING% >= 9 And O%(0) = 0 Then 18930
                            If P%(P, P1%(P), 6) / P%(P, P1%(P), 4) <= 2 And P8%(P, P1%(P), 0) >= 6 Then 18930

                            Exit For

                        End If

                    End If

                Next

            End If

            GoTo 18941

        Else

            Call WINDEX

            _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 144), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 144)

            F$ = PadRight$(Left$(A$(P), 3) + NN$(P), 8)

            If usingGfx = 1 Then
                'Printing black on background
                _PrintMode _KeepBackground , imageScreen&
                Color 4278190080

                PrintOnGfx 1, 19, F$, imageScreen&

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
                    PrintOnGfx 1, I - 1, F1$(I - 20), imageScreen&
                Next

                'Printing white on black
                Color 4294967295
                _PrintMode _FillBackground , imageScreen&

            Else
                Color 14, 0
                Print A$(P); " SELECT:"
                Print " 1- HIT AWAY      D-DETAILED STATS"
                Print " 2- HIT AND RUN   S-BATTER/PITCHER STATS"
                Print " 3- STEAL         L-DISPLAY LINEUPS"
                Print " 4- BUNT          Q-QUIT GAME"
                Print " 5- PERSONNEL     W-WEATHER CONDITIONS"
                Color 15, 0

            End If

        End If

        15061 '
        I$ = GetKeyPress$

        _PutImage (1 * FontColAdj%, 304)-Step(14 * FontColAdj%, 176), backupScreen&, imageScreen&, (1 * FontColAdj%, 304)-Step(14 * FontColAdj%, 176)

        If UCase$(I$) = "D" Then Call VIEWTEAM: GoTo 15580

        If UCase$(I$) = "L" Then Call PopUp_Lineup: GoTo 15580

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 15580
        End If

        If UCase$(I$) = "Q" Then
            If usingGfx = 0 Then Cls
            Call EndOfGame
            End
        End If

        If UCase$(I$) = "S" Then Call POPUPSTATS: GoTo 15580

        If UCase$(I$) = "W" Then
            Call WEATHER
            GoTo 15580
        End If

        P2 = Val(I$)

    Loop Until P2 >= 1 And P2 <= 5


    15064 '
    Select Case P2
        Case 1, 4

            If D2 <> 8 Then 13050

            Call WINDEX

            Call d100(RN)
            If P2 = 4 Or (P2 = 1 And RN <= 50) Then 13050

            Call BASEONBALLS(S2%, D2, D, P)

            GoTo 13201

        Case 2
            If O%(0) = 2 Or (S4% <> 1 And S4% <> 4 And S4% <> 5) Then 15051

        Case 3
            GoTo 14800

        Case 5
            P9 = P
            W5 = -1
            W6 = -1
            GoTo 15500

    End Select

    15067 '
    '-- this is related to the PICKEDOFF routine and it's immediate aftermath
    If S4% = 4 Then I = 3 Else I = 2

    Call d100(I3)

    If I3 < 6 Then

        Call PICKEDOFF(D, I)

        PO = 1
        A5%(I - 1) = 10
        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1
        W% = 1

        Call CREDITASSIST(W%, D)
        Call ADVANCEMENT(P, D, S2%, F%, I3)
        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        I5 = P1%(D)
        I6 = 0
        Call ADDPIT(D, I5, I6)

        If O%(0) >= 3 Then
            B1!(P) = B1!(P) - 1
            PO = 0
            GoTo 13900
        End If

    End If


    15068 '
    If PO = 1 Then PO = 0: GoTo 15051

    Call WINDEX

    I3 = Int(Rnd(1) * 1000) + 1
    I9 = Int(Rnd(1) * 1000) + 1

    If I3 > 350 Then
        Call PBP("Runner(s) going")
    Else
        Call PBP("Count not favorable...hit away")
        P2 = 1
    End If

    Call DELAY

    If I3 <= 350 Then 13050

    If I9 <= (B4%(P, 4, B1!(P)) * 2 * .33) Then
        U% = U% + 1
        Call PBP(Q$(0) + " swings and misses")
        Call DELAY
        If U% = 3 Then
            GoTo 13601
        Else
            GoTo 14800
        End If
    End If

    If I9 <= B4%(P, 4, B1!(P)) * 2 Then
        Call PBP(Q$(0) + " fouls off the pitch...hit away")
        Call DELAY
        P2 = 1
    End If

    GoTo 13050


    '--- end of a new sub


    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not yet been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================


    '----------------------------------------


    12000 ' BRANCH FOR RARE PLAYS
    Call RAREPLAYS(H6%, H7%, IP%)

    If IP% = 1 Then IP% = 0: Call RELIEVERS

    '-- is it me, or do the code lines
    '   "Call HITTING LINES...
    '   If usingGfx = 1...
    '-- get executed no matter what value H6% is?!?!
    '-- in original code this was a short sub
    '-- but in evaluating the logic I'm not sure
    '-- why it can't be run outside the If/Then

    '1 to 4
    '6 to 8
    '10 to 24
    If H6% <= 4 Or H6% > 5 And H6% < 9 Or H6% >= 10 And H6% <= 24 Then
        Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

        If usingGfx = 1 Then
            Call DisplayBallField_Gfx(BC%, P, U%, D)
            Call MAINSCREEN_G(D, P, BC%, U%, MO)
        Else
            Call DisplayBallField(BC%, P, U%, D)
            Call MAINSCREEN(D, P, BC%, U%, MO)
        End If

        GoTo 15052
    End If

    '5
    '25 to 29
    If H6% = 5 Or H6% >= 25 And H6% <= 29 Then
        Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

        If usingGfx = 1 Then
            Call DisplayBallField_Gfx(BC%, P, U%, D)
            Call MAINSCREEN_G(D, P, BC%, U%, MO)
        Else
            Call DisplayBallField(BC%, P, U%, D)
            Call MAINSCREEN(D, P, BC%, U%, MO)
        End If

        GoTo 13900
    End If

    '9
    If H6% = 9 Then
        Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

        If usingGfx = 1 Then
            Call DisplayBallField_Gfx(BC%, P, U%, D)
            Call MAINSCREEN_G(D, P, BC%, U%, MO)
        Else
            Call DisplayBallField(BC%, P, U%, D)
            Call MAINSCREEN(D, P, BC%, U%, MO)
        End If

        '13240 / *** base on balls ***
        Call BASEONBALLS(S2%, D2, D, P)
        GoTo 13201

    End If

    '45
    If H6% = 45 Then
        Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

        If usingGfx = 1 Then
            Call DisplayBallField_Gfx(BC%, P, U%, D)
            Call MAINSCREEN_G(D, P, BC%, U%, MO)
        Else
            Call DisplayBallField(BC%, P, U%, D)
            Call MAINSCREEN(D, P, BC%, U%, MO)
        End If

        GoTo 13201
    End If

    '30 to 44
    '46+ ???
    Call HITTINGLINES '(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

    If usingGfx = 1 Then
        Call DisplayBallField_Gfx(BC%, P, U%, D)
        Call MAINSCREEN_G(D, P, BC%, U%, MO)
    Else
        Call DisplayBallField(BC%, P, U%, D)
        Call MAINSCREEN(D, P, BC%, U%, MO)
    End If

    GoTo 15052


    '----------------------------------------


    13050 '
    Call d100(RN)

    If (((PC% = 1 Or PC% = 2 Or PC% = 3) And RN = 2) Or (RD = 1 And RN <= 5)) And INNING% >= 2 And RD% = 0 Then
        H7% = 20
        H6% = 21
        GoTo 12000
    Else
        H6% = Int(Rnd(1) * 1000) + 1
    End If

    If H6% <= 3 And IJ% = 0 Then 12000

    SN = 0
    H6% = Int(Rnd(1) * 1000) + 1

    If O%(0) = 0 And S4% = 4 And INFPOS% < 2 And H6% = 768 Or O%(0) = 0 And S4% = 4 And INFPOS% < 2 And H6% = 123 Or O%(0) = 0 And S4% = 7 And INFPOS% < 2 And H6% = 245 Or O%(0) = 0 And S4% = 7 And INFPOS% < 2 And H6% = 381 Then
        Call TRIPLEPLAY(D)
        GoTo 13900
    End If

    H7% = Int(Rnd(1) * 7500) + 1

    Call WINDEX

    If H6% <= 4 Then
        13200 ' *** hit by pitch ***
        Call HITBYPITCH(S2%, D)

        13201 '
        A5%(0) = 1
        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 0

        If B0%(1, 0) > 0 Then
            If B0%(2, 0) > 0 Then
                If B0%(3, 0) > 0 Then
                    A5%(3) = 1
                    If W% = 2 And S2% > 2 Then A5%(3) = 11
                Else
                    A5%(2) = 1
                End If
            Else
                A5%(1) = 1
            End If
        End If

        Call ADVANCEMENT(P, D, S2%, F%, I3)
        GoTo 15050

    End If

    If H6% <= 6 Then
        13210 ' *** balk ***
        If S4% = 0 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo 15052
        Else
            S2% = 4

            Call WINDEX

            Call d100(RN)
            UN = Int(Rnd(1) * 4) + 1
            If RN <= 33 Then F$ = "a balk has been called"
            If RN > 33 And RN <= 66 Then Call PBP("and a BALK is called"): F$ = P$(D, P1%(D)) + " dropped the ball"
            If RN > 66 Then F$ = U$(UN) + " calls a balk on " + P$(D, P1%(D))
            Call PBP(F$)

            '13223
            If B0%(1, 0) = 0 And RN = 9 And SO = 1 Or O%(0) > 1 And RN = 9 And SO = 1 Then
                A5%(0) = 1
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                GoTo 13900
            Else
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
                A5%(0) = 0
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                GoTo 15051
            End If

        End If

    End If

    '7 to 11 OR 7 to 200 And ...
    If H6% <= 11 Or H6% <= 200 And B%(D, B9%(D, 2), 22) <> 2 And B%(D, B9%(D, 2), 23) <> 2 And B%(D, B9%(D, 2), 24) <> 2 And B%(D, B9%(D, 2), 25) <> 2 Then

        13220 ' *** passed ball ***
        If S4% = 0 And SO <> 1 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo 15052
        End If

        S2% = 5
        Call WINDEX
        Call DELIVERY(D)

        Call d100(RN2)
        If RN2 <= 50 Then
            Call PBP("pitch gets by " + B$(D, B9%(D, 2)))
        Else
            'RN > 50
            Call PBP("off " + B$(D, B9%(D, 2)) + "'s glove")
            Call PBP("it gets away from him")
        End If

        Call PBP("It's ruled a passed ball")
        
        If B0%(1, 0) = 0 And RN = 9 And SO = 1 Or O%(0) > 1 And RN = 9 And SO = 1 Then O%(1) = O%(1) + 1

        13222 '
        I8! = Int(Rnd(1) * 10) + 1

        If S4% = 1 And P2 = 2 And I8! < B%(P, B3%(P, B0%(1, 0)), 16) Or S4% = 5 And P2 = 2 And I8! < B%(P, B3%(P, B0%(1, 0)), 16) Then
            A5%(1) = 2
            A5%(3) = 1
            A5%(0) = 0
            A5%(2) = 2
            Call ADVANCEMENT(P, D, S2%, F%, I3)
            GoTo 15051
        End If

        13223 '
        If B0%(1, 0) = 0 And RN = 9 And SO = 1 Or O%(0) > 1 And RN = 9 And SO = 1 Then
            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1
            Call ADVANCEMENT(P, D, S2%, F%, I3)
            GoTo 13900
        Else
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1
            A5%(0) = 0
            Call ADVANCEMENT(P, D, S2%, F%, I3)
            GoTo 15051
        End If

    End If

    If H6% <= 19 Then

        '13230 / *** wild pitch ***
        If S4% = 0 And SO <> 1 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo 15052
        End If

        Call WILDPITCH(S2%, D)

        GoTo 13222
    End If

    If H7% = 27 Then
        13244 '
        S2% = 2
        Call WINDEX
        Call PBP(B$(D, B9%(D, 2)) + " is called for interference!!")
        Call PBP(Q$(0) + " trots to first base")
        GoTo 13201
    End If

    If H7% = 28 Then

        Call WINDEX
        Call PBP(Q$(0) + " is out for obstruction!")
        W% = 2
        Call CREDITREGPO(W%, D)
        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1

        If B7%(P, B1!(P)) <> 1 Then
            I5 = B3%(P, B1!(P))
            I6 = 0
            Call INCBATOFF(P, I5, I6)
        Else
            I5 = P1%(P)
            I6 = 23
            Call INCPITOFF(P, I5, I6)
        End If

        I5 = P1%(D)
        I6 = 0

        Call ADDPIT(D, I5, I6)

        GoTo 13900
    End If

    If H6% <= B4%(P, 5, B1!(P)) + 25 Then
        S2% = 2
        '13240 / *** base on balls ***
        Call BASEONBALLS(S2%, D2, D, P)
        GoTo 13201
    End If

    If P2 = 4 Then 14100

    H6% = Int(Rnd(1) * 1000) + 1
    H7% = Int(Rnd(1) * 1000) + 1
    I4 = 0

    If P2 = 2 Then I4 = -5
    If INFPOS% = 2 Then SN = 70: I4 = I4 + 15
    If U% = 2 Then I4 = I4 - 15
    If INFPOS% = 1 Then SN = 20
    If INNING% > 5 And S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) Then I4 = I4 + 40

    If P = 0 Then I4 = I4 - 2
    If P = 1 Then I4 = I4 + 2

    Call WINDEX

    If H7% <= SN Then
        Call PBP("It gets through the drawn-in infield!")
        Call DELAY
        GoTo 13063
    End If

    If H6% > B4%(P, 0, B1!(P)) + I4 Then

        ' *** strikeout ***
        H6% = Int(Rnd(1) * 1000) + 1

        If H6% > B4%(P, 4, B1!(P)) Then

            ' *** ground ball for out ***
            Call d100(H6%)

            E2% = B%(P, B3%(P, B1!(P)), 18) + P%(D, P1%(D), 12) - 52
            E3% = 23 + P%(D, P1%(D), 12)
            E4% = 0

            If H6% > E2% And B7%(P, B1!(P)) <> 1 Then E4% = 1
            If H6% > E3% And B7%(P, B1!(P)) = 1 Then E4% = 1
            If E4% = 1 Then 14000

            S2% = 13
            Call GETINFIELDER(W%, P, I1, D, S2%, P2)
            Call WINDEX

            S2% = 13
            Call DELIVERY(D)
            Call GROUNDERPBP(W%, D)

            If W% <> 2 Then
                If W% = 1 Then F$ = F$ + "the pitcher": Call PBP(F$)
            End If

            F$ = F$ + F$(W%)
            Call PBP(F$)

            13612 '
            Call d100(RN)

            If (S4% = 1 Or S4% = 4 Or ((S4% = 7 Or S4% = 5) And INFPOS% < 2)) And O%(0) <= 1 And RN <= 50 Then
                DPC% = 1
                Call DPPBP
            End If

            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
            Else
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
            End If

            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            If F% = 1 Then 13900

            ' *** 2 outs, bases empty, runner 2nd, runner 3rd, or 2nd&3rd ***
            If O%(0) = 2 Or S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% = 6 Then 13650

            H6% = Int(Rnd(1) * 1000) + 1
            E2% = ((L%(D, 5) - 80) * 4)
            E3% = ((5 - B%(P, B3%(P, B1!(P)), 16)) * 9)

            If (W% = 2 And H6% > 250 + E2% + E3%) Or (H6% > 490 + E2% + E3%) Or P2 = 2 Then 13650

            S2% = 13

            If S4% = 4 Then

                If W% = 5 And Int(Rnd(1) * 3) + 1 = 1 Then
                    A5%(3) = 0
                    A5%(2) = 11
                    A5%(1) = 0
                    A5%(0) = 0
                    GoTo 13634
                End If

                If W% = 2 Then
                    Call CREDITASSIST(W%, D)
                    A5%(3) = 0
                    A5%(2) = 11
                    A5%(1) = 0
                    A5%(0) = 0
                    GoTo 13634
                End If

            End If

            If S4% = 7 And W% = 1 Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He throws home"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634
            If S4% = 5 And S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 7 And O%(0) = 0 And INFPOS% > 0 And W% = 5 Or S4% = 5 And S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 7 And O%(0) = 0 And INFPOS% > 0 And W% = 3 Then 13650
            If S4% = 7 And INFPOS% > 0 And (W% = 5 Or W% = 3) Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He fires it home"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634
            If S4% = 7 And INFPOS% = 2 And W% = 4 Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He throws home for the force"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634
            If S4% = 7 And INFPOS% = 2 And W% = 6 Then Call CREDITASSIST(W%, D): TB = 9: W% = 2: Call PBP("He goes home with it"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634

            If W% <= 4 Then
                Call CREDITASSIST(W%, D)
                W% = 6
                Call PBP("He throws to " + B$(D, B9%(D, 6)))
            Else
                Call CREDITASSIST(W%, D)
                W% = 4
                Call PBP("He throws to " + B$(D, B9%(D, 4)))
            End If

            A5%(1) = 11
            A5%(2) = 0
            A5%(3) = 0
            A5%(0) = 0

            If S4% = 7 And (W% = 2 Or W% = 1) Then
                A5%(3) = 11
                A5%(2) = 1
                A5%(1) = 1
                A5%(0) = 1
            End If

            GoTo 13634

        End If

        'Strikeout!
        13601 '
        Call WINDEX
        Call STRIKEOUT(U%, D)

        13602 '
        W% = 2
        Call CREDITREGPO(W%, D)
        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1
        SO = 1

        If B7%(P, B1!(P)) <> 1 Then
            I5 = B3%(P, B1!(P))
            I6 = 0
            Call INCBATOFF(P, I5, I6)
            I6 = 8
            Call INCBATOFF(P, I5, I6)
            If P2 = 2 Then U% = 3
        End If

        I5 = P1%(D)
        I6 = 5
        Call ADDPIT(D, I5, I6)

        I6 = 0
        Call ADDPIT(D, I5, I6)

        If B7%(P, B1!(P)) = 1 Then

            I5 = P1%(P)
            I6 = 23
            Call INCPITOFF(P, I5, I6)

            I6 = 31
            Call INCPITOFF(P, I5, I6)
            If P2 = 2 Then U% = 3
        End If

        If P2 = 2 And O%(0) < 3 Then SO = 7: GoTo 14800

        ' *** WILD PITCH OR PASSED BALL ON A STRIKEOUT
        RN = Int(Rnd(1) * 600) + 1
        If B0%(1, 0) = 0 And RN = 9 And SO = 1 Or O%(0) = 3 And RN = 9 Then

            O%(0) = O%(0) - 1
            O%(1) = O%(1) - 1
            I5 = P1%(D)
            P8%(D, I5, 0) = P8%(D, I5, 0) - 1
            P5%(D, I5, 0) = P5%(D, I5, 0) - 1
            B8%(D, B9%(D, 2), 16) = B8%(D, B9%(D, 2), 16) - 1
            B5%(D, B9%(D, 2), 16) = B5%(D, B9%(D, 2), 16) - 1

            Call d100(I1)
            If I1 < 77 Then
                '13230 / *** wild pitch ***
                If S4% = 0 And SO <> 1 Then
                    '13100 / *** fouled out of play ***
                    Call FOULBALL
                    GoTo 15052
                Else
                    Call WILDPITCH(S2%, D)
                    GoTo 13222
                End If
            Else
                GoTo 13220
            End If

        End If

        '-- this should be an Else statement above?
        GoTo 13900

    Else

        H6% = Int(Rnd(1) * 1000) + 1

        If H6% <= B4%(P, 1, B1!(P)) And P2 <> 2 Or H6% <= B4%(P, 1, B1!(P)) * .5 And P2 = 2 Then

            S2% = 8

            '3500
            ' *** double s2%=8 ***
            Call GETOUTFIELDER(W%, P, I1, D)

            I5 = P1%(D)
            I6 = 1

            Call ADDPIT(D, I5, I6)
            Call WINDEX
            Call DELIVERY(D)

            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
                I6 = 1
                Call INCBATOFF(P, I5, I6)
                I6 = 4
                Call INCBATOFF(P, I5, I6)
            Else
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
                I6 = 24
                Call INCPITOFF(P, I5, I6)
                I6 = 27
                Call INCPITOFF(P, I5, I6)
            End If

            If RN <= 40 Then

                If W% = 7 Then
                    Y% = Int(Rnd(1) * 40) + 2900
                    Call GETRPBP(Y%, Y$, D)
                    Call STRIPRPBP(Y$, Y%, D)
                    Call PBP(Y$)

                End If

                If W% = 9 Then
                    Y% = Int(Rnd(1) * 40) + 2940
                    Call GETRPBP(Y%, Y$, D)
                    Call STRIPRPBP(Y$, Y%, D)
                    Call PBP(Y$)
                End If

                If W% = 8 Then
                    Y% = Int(Rnd(1) * 40) + 2980
                    Call GETRPBP(Y%, Y$, D)
                    Call STRIPRPBP(Y$, Y%, D)
                    Call PBP(Y$)
                End If

                Y% = Int(Rnd(1) * 30) + 3020
                Call GETRPBP(Y%, Y$, D)
                Call STRIPRPBP(Y$, Y%, D)
                Call PBP(Y$)

            Else

                Call d100(RN)

                Select Case RN

                    Case Is <= 25
                        Call LONGFLY(D, W%)
                        HW% = 1
                        Call WALLDOUBLE(HW%, W%)

                    Case 26 To 50
                        Call LDRIVE
                        Call PBP(Q$(0) + "one to the gap in " + OUTFLDLOCATION$(W%))

                    Case 51 To 60
                        Call PBP(Q$(0) + " hits a sinking liner into " + OUTFLDLOCATION$(W%))
                        Call PBP("It goes to the wall")

                    Case 61 To 75
                        Call PBP(Q$(0) + " lines one to the alley in " + OUTFLDLOCATION$(W%))

                    Case 76 To 85
                        If (W% = 7 Or W% = 9) Then
                            Call LDRIVE
                            Call PBP(Q$(0) + "one down the " + C$(W%) + " line")
                            Call PBP("It rolls into the corner")
                        End If

                        If W% = 8 Then
                            Call LDRIVE
                            Call PBP(Q$(0) + "one between the fielders")
                            Call PBP(Q$(4) + " cuts it off...")
                        End If

                    Case 86 To 92
                        Call LDRIVE
                        Call PBP(Q$(0) + "a blue darter into " + C$(W%))

                    Case 93 To 100
                        Call PBP(Q$(0) + " lines one to " + OUTFLDLOCATION$(W%))
                        Call PBP("It's down and to the wall!")

                End Select

            End If

            HW% = 0

            If Int(Rnd(1) * 100) + 1 <= 80 Then
                F$ = "Double!!"
            Else
                F$ = "2-Bagger!!"
            End If

            Call PBP(F$)

            For I = 0 To 3: A5%(I) = 2: Next I

            A5%(3) = 1

            If B0%(1, 0) <> 0 Then

                Call d100(H6%)
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

            End If

            Call ADVANCEMENT(P, D, S2%, F%, I3)

            F% = 0
            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)

            Call SCOREBOARD

            GoTo 13900

        End If


        If H6% <= B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P)) And P2 <> 2 Or H6% <= B4%(P, 1, B1!(P)) * .5 + B4%(P, 2, B1!(P)) * .5 And P2 = 2 Then

            S2% = 9

            '3520
            ' *** TRIPLE S2%=9 ***
            Call OUTFLDNAME(W%, D)
            Call DELIVERY(D)
            Call WINDEX

            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
                I6 = 1
                Call INCBATOFF(P, I5, I6)
                I6 = 5
                Call INCBATOFF(P, I5, I6)
            Else
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
                I6 = 24
                Call INCPITOFF(P, I5, I6)
                I6 = 28
                Call INCPITOFF(P, I5, I6)
            End If

            I5 = P1%(D)
            I6 = 1

            Call ADDPIT(D, I5, I6)

            Call d100(RN)

            If RN <= 30 Then

                If W% = 7 Then
                    Y% = Int(Rnd(1) * 40) + 2900
                    Call GETRPBP(Y%, Y$, D)
                    Call STRIPRPBP(Y$, Y%, D)
                    Call PBP(Y$)
                End If

                If W% = 8 Then
                    Y% = Int(Rnd(1) * 40) + 2980
                    Call GETRPBP(Y%, Y$, D)
                    Call STRIPRPBP(Y$, Y%, D)
                    Call PBP(Y$)
                End If

                If W% = 9 Then
                    Y% = Int(Rnd(1) * 40) + 2940
                    Call GETRPBP(Y%, Y$, D)
                    Call STRIPRPBP(Y$, Y%, D)
                    Call PBP(Y$)
                End If

                Y% = Int(Rnd(1) * 40) + 3050
                Call GETRPBP(Y%, Y$, D)
                Call STRIPRPBP(Y$, Y%, D)
                Call PBP(Y$)

                Call d100(RN)
                If RN <= 90 Then
                    F$ = "Triple!!"
                Else
                    F$ = "3-Bagger!"
                End If
                Call PBP(F$)

            Else

                Call d100(RN)

                If RN <= 10 And W% <> 8 Then

                    Call d100(RN)
                    X$ = "1st"
                    If W% = 7 Then X$ = "3rd"

                    If RN <= 50 Then
                        Call PBP(Q$(0) + " hits a hot smash")
                    Else
                        Call PBP(Q$(0) + " lines one")
                    End If

                    Call PBP("Fair past " + X$ + "!!")
                    Call PBP("It rolls into the " + C$(W%) + " corner")
                    Call PBP("It's a triple!!")

                Else

                    Call LONGFLY(D, W%)
                    ad$ = "wall"
                    If W% = 7 And L%(1, 13) = 27 Then ad$ = "screen"

                    Call d100(BV)
                    Call d100(RN)
                    Select Case RN
                        Case 1 To 20
                            F$ = "It one-hops the " + ad$ + "..."
                        Case 21 To 40
                            F$ = "It's off the top of the " + ad$ + "..."
                        Case 41 To 60
                            Call PBP("It bounces fair"): F$ = "down into the corner..."
                        Case 61 To 80
                            Call PBP("It's in the gap"): F$ = "It rolls to the " + ad$
                        Case 81 To 99
                            F$ = "It hits at the base of the " + ad$ + "..."
                        Case 100
                            F$ = "The outfielders cross"
                    End Select

                    Call PBP(F$)

                    Call d100(RN)
                    If RN <= 90 Then
                        F$ = "Triple!!"
                    Else
                        F$ = "3-Bagger!"
                    End If
                    Call PBP(F$)

                End If

            End If

            A5%(0) = 3
            A5%(1) = 3
            A5%(2) = 2
            A5%(3) = 1

            Call ADVANCEMENT(P, D, S2%, F%, I3)

            F% = 0
            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)

            Call SCOREBOARD
            GoTo 13900

        End If

        '-- this could be nested in an Else clause below

        Call HITS2GAP(W%, P, I1, D)

        I3 = B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P))

        E2% = I3 + B4%(P, 3, B1!(P))
        If W% = 8 And H6% <= E2% And P2 <> 2 Or W% = 8 And H6% <= E2% * .5 And P2 = 2 Then
            S2% = 10
            GoTo 13530
        End If

        E2% = I3 + B4%(P, 6, B1!(P))
        If W% = 7 And H6% <= E2% And P2 <> 2 Or W% = 7 And H6% <= E2% * .5 And P2 = 2 Then
            S2% = 10
            GoTo 13530
        End If

        E2% = I3 + B4%(P, 7, B1!(P))
        If W% = 9 And H6% <= E2% And P2 <> 2 Or W% = 9 And H6% <= E2% * .5 And P2 = 2 Then
            S2% = 10
            GoTo 13530
        End If

        13063 '
        Call d100(H6%)
        S2% = 7

        If Not (B7%(P, B1!(P)) = 1 Or H7% <= SN) Then

            If H6% <= (B%(P, B3%(P, B1!(1)), 16) * 2.5) Then
                S2% = 12
                Call GETINFIELDER(W%, P, I1, D, S2%, P2)
                Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                If F% = 1 Then 13900
            End If

        End If

        13065 '/ *** single ***
        Call SINGLEROUTINE(W%, P, I1, D, S2%, P2)

        If S2% = 12 Then

            Call SCOREBOARD
            GoTo 15050

        Else

            'PQ = 0

            F% = 0
            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)

            Call SCOREBOARD

            GoTo 13900

        End If

    End If



    '----------------------------------------


    13530 ' *** HOME RUN S2%=10 ***

    Call WINDEX

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
        I6 = 1
        Call INCBATOFF(P, I5, I6)
        I6 = 6
        Call INCBATOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 1

    Call ADDPIT(D, I5, I6)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
        I6 = 24
        Call INCPITOFF(P, I5, I6)
        I6 = 29
        Call INCPITOFF(P, I5, I6)
    End If

    Call d100(RN)

    If RN <= 80 Then
        Call LONGFLY(D, W%)

        13535 '
        Call d100(RN)

        If RN <= 5 And L%(1, 13) = 8 And W% = 7 Then Call PBP("THAT'S OUT OF THE PARK"): Call PBP("ON WAVELAND AVENUE!!!"): F$ = "HOME RUN...HOLY COW!!!": GoTo 13543
        If RN <= 15 And L%(1, 13) = 8 And W% = 7 Then Call PBP("IT'S GONE"): F$ = "LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!": GoTo 13543
        If RN <= 25 Then

            '13580
            If L%(1, 13) = 21 And W% = 7 Then Call PBP("IT CLEARS THE GREEN MONSTER!!"): F$ = "HOME RUN!!!": GoTo 13543
            If L%(1, 13) = 27 And W% = 7 Then Call PBP("IT CLEARS THE SCREEN!!"): F$ = "HOME RUN!!!": GoTo 13543
            If L%(1, 13) = 33 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): F$ = "THE BUSCH EAGLE FLAPS ITS WINGS": GoTo 13543
            If L%(1, 13) = 25 And P = 1 Then Call PBP("IT'S OUT OF HERE FOR A HOMER!!!"): F$ = "THERE GOES THE EXPLODING SCOREBOARD!": GoTo 13543
            If L%(1, 13) = 7 And W% = 9 Then Call PBP("IT CLEARS THE WAGON GATE!!"): F$ = "THAT'S A HOME RUN!!": GoTo 13543
            If L%(1, 13) = 7 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): F$ = "THERE GO THE CANNONS!!": GoTo 13543
            If L%(1, 13) = 8 Or L%(1, 13) = 40 Then F$ = "IT CLEARS THE IVY FOR A HOME RUN!!": GoTo 13543
            If L%(1, 13) = 23 And (W% = 7 Or W% = 9) Then Call PBP("IT'S LONG, IT'S LONG, IT'S GONE!!"): F$ = "THAT ONE'S IN THE UPPER DECK": GoTo 13543
            'GOTO 13545

            13545 '
            Call d100(RN)
            Select Case RN
                Case 1 To 7
                    F$ = "GOODBYE!! IT'S A HOME RUN!!"

                Case 8 To 14
                    F$ = "ADIOS!! IT'S A HOME RUN !!!"

                Case 15 To 21
                    F$ = "SAY GOOD-BYE TO THAT ONE!!!"

                Case 22 To 28
                    F$ = "TOUCH 'EM ALL!!"

                Case 29 To 35
                    F$ = "SEE YOU LATER!!"

                Case 36 To 42
                    F$ = "GOOD-BYE MR. SPALDING!! HOME RUN!!!"

                Case 43 To 49
                    F$ = "HOME RUN!! MY OH MY!!"

                Case 50 To 56
                    F$ = "IT'S OVER THE WALL...OH DOCTOR!!!"

                Case 57 To 63
                    F$ = "LONG, LONG, AND GONE!!"

                Case 64 To 70
                    F$ = "HEY! HEY!...IT'S A HOME RUN !!!"

                Case 71 To 77
                    F$ = "IT'S LONG, IT'S LONG, IT'S GONE!!!"

                Case 78 To 84
                    F$ = "HOLY COW!! IT'S A HOME RUN!!!"

                Case 85 To 89
                    F$ = "OPEN THE WINDOW AUNT MINNIE...HERE SHE COMES"

                Case 90 To 95
                    Call PBP("WAAYYY BACK..."): F$ = "YOU CAN PUT IT ON THE BOARD, YYESSS!!"

                Case 96 To 100
                    Call PBP("IT'S WAY OUT OF HERE!! HOME RUN!!!"): F$ = "HOW 'BOUT THAT!!!"

            End Select

        End If

    Else

        Call d100(RN)

        If RN <= 20 And (W% = 7 Or W% = 9) Then

            F$ = Q$(0) + " hits it down the " + C$(W%) + " line"
            Call PBP(F$)
            Call PBP("It's long enough if it's fair...")

        Else

            Call PBP(Q$(0) + " swings...")

            Select Case RN
                Case 21 To 40
                    Call PBP("NO DOUBT ABOUT THAT ONE!!")
                    F$ = "IT'S WAY OUT OF HERE!!"
                Case 41 To 60
                    F$ = Q$(4) + " CAN ONLY LOOK UP!!"
                Case 61 To 80
                    F$ = P$(D, P1%(D)) + " WATCHES IT SAIL OUT!!"
                Case 81 To 90
                    F$ = "GONE!!!"
                Case 91 To 100
                    F$ = "SAINTS AND HAYRAKES, LOOK AT 'ER SAIL!!"
            End Select

            Call PBP(F$)

        End If

        '3536 / modified
        F$ = "HOME RUN!!"

    End If

    13543 '
    Call PBP(F$)
    Call DELAY

    '13544
    A5%(3) = 1
    A5%(2) = 2
    A5%(1) = 3
    A5%(0) = 4
    I5 = P1%(D)
    I6 = 14

    Call ADDPIT(D, I5, I6)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call SCOREBOARD

    GoTo 13900


    '----------------------------------------


    13634 '
    I5 = P1%(D)
    I6 = 0
    
    Call ADDPIT(D, I5, I6)
    
    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    Q$(4) = B$(D, B9%(D, W%))

    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITREGPO(W%, D)

    S2% = 15
    
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 13900

    Call CREDITASSIST(W%, D)

    If W% = 5 And O%(0) < 2 And Int(Rnd(1) * 10) + 1 <= B%(P, B3%(P, B0%(1, 0)), 16) Then
        A5%(0) = 1
        A5%(1) = 11
        A5%(2) = 0
        A5%(3) = 0
        If O%(0) < 2 Then A5%(2) = 1: A5%(3) = 1
    Else
        A5%(0) = 11
        A5%(1) = 1
        A5%(2) = 0
        A5%(3) = 0
        If O%(0) < 2 Then A5%(2) = 1: A5%(3) = 1
    End If

    O%(0) = O%(0) + 1
    O%(1) = 2
    I5 = P1%(D)
    I6 = 0

    Call DPRELAY
    Call ADDPIT(D, I5, I6)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    Call d100(RN)

    DPC% = 0
    If RN <= 80 Then
        F$ = " --- DOUBLE PLAY ---"
    Else
        'RN > 80
        F$ = " --- TWIN KILLING ---"
    End If
    Call PBP(F$)
    
    DP%(D) = DP%(D) + 1
    D3%(D) = D3%(D) + 1
    TB = 0
    Call INCREMENTHIT(P, D, MO)

    GoTo 13900


    '----------------------------------------


    13650 '
    If O%(0) >= 2 Then

        RN = Int(Rnd(1) * 10) + 1
        S2% = 13

        If W% = 5 Then

            If RN <= 5 And (S4% = 4 Or S4% = 7) Then

                Call PITCHERNAME(W%, D)
                Call PBP(Q$(4) + " gloves it and heads for 3rd")
                A5%(0) = 0
                A5%(1) = 0
                A5%(2) = 11
                A5%(3) = 0
                GoTo 13684

            Else

                Call PITCHERNAME(W%, D)
                Call GROUNDERS(W%, D)
                Call THROW2FIRST(D)
                A5%(0) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo 13684

            End If

        End If

        If W% = 6 Then

            If RN <= 8 And (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then
                Call PITCHERNAME(W%, D)
                Call GROUNDERS(W%, D)
                Call DPPIVOT(W%, D)
                AST% = 6
                A5%(0) = 0
                A5%(1) = 11
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo 13684

            Else
                Call PITCHERNAME(W%, D)
                Call GROUNDERS(W%, D)
                Call THROW2FIRST(D)
                A5%(0) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo 13684

            End If

        End If

        If W% = 4 And RN <= 5 And (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then

            Call PITCHERNAME(W%, D)
            Call GROUNDERS(W%, D)
            Call DPPIVOT(W%, D)
            AST% = 4
            A5%(0) = 0
            A5%(1) = 11
            A5%(2) = 0
            A5%(3) = 0

        Else

            Call d100(RN)

            If W% = 3 Then

                If RN <= 94 Then

                    Call PITCHERNAME(W%, D)
                    Call GROUNDERS(W%, D)
                    Call PBP("He takes it to the bag unassisted")
                    fbu% = 1

                Else

                    Call PITCHERNAME(W%, D)
                    Call GROUNDERS(W%, D)
                    W% = 1
                    Call PITCHERNAME(W%, D)
                    Call PBP("He flips it to " + Q$(4) + " covering")
                    FB2P% = 1
                    W% = 3

                End If

            Else

                Call PITCHERNAME(W%, D)
                Call GROUNDERS(W%, D)
                Call SECOND2FIRST(D)

            End If

            Call PITCHERNAME(W%, D)
            Call GROUNDERS(W%, D)
            Call SECOND2FIRST(D)

            13672 '
            A5%(0) = 11
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0

        End If

        13684 'Call DELAY
        If fbu% = 0 Then Call CREDITASSIST(W%, D)
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
        I5 = P1%(D)
        I6 = 0
        Call ADDPIT(D, I5, I6)

        GoTo 13900

    Else

        13690 '
        Call d100(RN)
        If (RN <= 90 And P2 <> 2 Or S4% = 0) Or (RN <= 10 And P2 = 2) Then

            S2% = 14

            If S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% = 6 Then
                A5%(0) = 11
                Call OUTATFIRST(W%, D, fbu%, FB2P%)
                GoTo 13693
            End If

            Call d100(RN)

            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1

            If INFPOS% = 0 And W% = 2 Then A5%(3) = 0
            If S4% = 5 Or S4% = 7 Or S4% = 4 Then 13725

            A5%(1) = 11

            GoTo 13684

        End If

        S2% = 13
        A5%(1) = 1
        A5%(2) = 1
        A5%(3) = 1
        A5%(0) = 11
                
        If Not (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then

            13693 '
            Call d100(RN)
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0

            If W% = 5 And RN <= 25 And INFPOS% < 2 Then A5%(2) = 1
            If W% = 5 And RN <= 83 And INFPOS% = 0 Then A5%(3) = 1: ' WAS 50
            If W% = 5 And RN <= 10 And INFPOS% > 0 Then A5%(3) = 1

            If W% = 5 And RN < 5 And INFPOS% < 2 And S4% = 2 And S6%(P, 0) - S6%(D, 0) > -2 Or W% = 5 And RN < 5 And INFPOS% < 2 And S4% = 6 And A5%(3) = 1 And S6%(P, 0) - S6%(D, 0) > -2 Then
                A5%(2) = 11
                A5%(0) = 1
                Call PBP("NO, he's going to third")
            End If

            If W% = 6 And RN <= 20 And INFPOS% < 2 Then A5%(2) = 1
            If W% = 6 And RN <= 92 And INFPOS% < 2 Then A5%(3) = 1
            If W% = 6 And RN <= 5 And INFPOS% = 2 Then A5%(3) = 1
            If W% = 6 And RN < 6 And INFPOS% < 2 And S4% = 2 And S6%(P, 0) - S6%(D, 0) > -2 Or W% = 6 And RN < 6 And INFPOS% < 2 And S4% = 6 And A5%(3) = 1 And S6%(P, 0) - S6%(D, 0) > -2 Then A5%(2) = 11: A5%(0) = 1: Call PBP("NO, he's going to third")
            If W% = 4 And RN <= 90 And INFPOS% < 2 Then A5%(2) = 1
            If W% = 4 And RN <= 90 And INFPOS% < 2 Then A5%(3) = 1
            If W% = 4 And RN <= 65 And INFPOS% = 2 Then A5%(2) = 1
            If W% = 4 And RN <= 5 And INFPOS% = 2 Then A5%(3) = 1
            If W% = 3 And RN <= 95 Then A5%(2) = 1
            If W% = 3 And RN <= 88 And INFPOS% = 0 Then A5%(3) = 1: ' WAS 60
            If W% = 3 And RN <= 15 And INFPOS% > 0 Then A5%(3) = 1
            If W% = 1 And RN <= 30 Then A5%(2) = 1
            If W% = 1 And RN <= 50 And INFPOS% = 0 Then A5%(3) = 1
            If W% = 1 And RN <= 10 And INFPOS% > 0 Then A5%(3) = 1
            If W% = 2 And RN <= 45 Then A5%(2) = 1

        End If

        GoTo 13684

    End If

    13725 '
    If S4% <> 4 And S4% <> 7 Then
        If S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 6 And O%(0) = 0 And INFPOS% > 0 And W% = 5 Or S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 6 And O%(0) = 0 And INFPOS% > 0 And W% = 3 Then

            A5%(3) = 11

        Else

            If INFPOS% = 0 And W% <> 2 Then A5%(1) = 11: GoTo 13684
            If INFPOS% > 0 And W% = 5 Or INFPOS% > 0 And W% = 3 Then A5%(3) = 0: A5%(1) = 11: GoTo 13684
            If INFPOS% > 0 And (W% = 1 Or W% = 3) Then A5%(3) = 0: A5%(1) = 1: A5%(0) = 11: GoTo 13684
            If INFPOS% = 2 And W% = 4 Or INFPOS% = 2 And W% = 6 And RN <= 90 Then
                A5%(3) = 0
                A5%(1) = 11
            Else
                A5%(1) = 11
            End If

        End If

    Else

        13732 '
        If W% = 6 And RN < 6 And INFPOS% < 2 And INNING% < 9 Or W% = 5 And RN < 6 And INFPOS% < 2 And INNING% < 9 Then
            A5%(2) = 11
        Else
            If S4% = 7 Then

                13740 '
                Call d100(RN)
                A5%(1) = 11
                A5%(2) = 1
                A5%(3) = 1
                A5%(0) = 1

                If INFPOS% > 0 And W% = 5 Or INFPOS% > 0 And W% = 3 Or INFPOS% = 2 And W% = 4 Or INFPOS% = 2 And W% = 6 Or W% = 2 Or W% = 1 Then
                    A5%(3) = 11
                    A5%(0) = 1
                    A5%(1) = 1
                    A5%(2) = 1
                Else
                    If W% = 5 And RN <= 5 Or W% = 1 Or W% = 2 Then
                        A5%(3) = 11
                        A5%(2) = 1
                        A5%(1) = 1
                        A5%(0) = 1
                    End If
                End If

            Else

                RN = Int(Rnd(1) * 10) + 1
                
                A5%(0) = 1
                A5%(1) = 11
                A5%(2) = 1
                A5%(3) = 0

                If W% = 5 And RN <= 5 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
                If W% = 1 And RN <= 3 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0

                If W% = 2 And B%(P, B3%(P, B0%(2, 0)), 16) < 7 And B%(P, B3%(P, B0%(2, 0)), 16) > 0 Then
                    A5%(1) = 1
                    A5%(2) = 11
                    A5%(3) = 0
                Else
                    If W% = 2 And B%(P, B3%(P, B0%(2, 0)), 16) > 6 Then
                        A5%(1) = 1
                        A5%(2) = 1
                        A5%(0) = 11
                    End If
                End If

            End If

        End If

    End If

    GoTo 13684


    '-----------------------------------------------------
    '--- THIS IS CRITICAL FOR PBP MOVING AUTOMATICALLY ---
    '-----------------------------------------------------
    ' Play-by-Play box
    13900 '
    If usingGfx = 1 Then _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 144), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 144)

    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    I$ = InKey$

    If UCase$(I$) = "Q" Then Call EndOfGame: End

    If UCase$(I$) = "O" Then
        Call OPTIONS
    End If

    '----------------------------------------------------

    'There was some question as to whether this is needed
    'based on comparing to the original code, but the working
    'version of my code had this in it still
    If usingGfx = 1 Then _PutImage (17 * FontColAdj%, 128)-Step(46 * FontColAdj%, 80), backupScreen&, imageScreen&, (17 * FontColAdj%, 128)-Step(46 * FontColAdj%, 80)

    'PQ = 0

    DPC% = 0
    H0% = 0
    INFPOS% = 0
    F% = 0
    H0%(0) = 0

    If O%(0) = 3 Then
        GoTo 15000
    Else
        GoTo 15050
    End If

    '----------

    14000 'balls hit to the air
    Call d100(RN)
    A5%(0) = 0
    A5%(1) = 0
    A5%(2) = 0
    A5%(3) = 0

    'Case 1 to 73
    If RN <= 73 Then
        '-- this is a longer area
        GoTo 14020: ' flyout
    End If

    'Case 74 to 89
    If RN <= 89 Then
        ' infield popup
        Call GETINFIELDER(W%, P, I1, D, S2%, P2)
        S2% = 20
        Call WINDEX

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

        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)

        If fo% = 1 And W% = 2 Then Y% = Int(Rnd(1) * 12) + 321
        If fo% = 1 And (W% = 3 Or W% = 4) Then Y% = Int(Rnd(1) * 25) + 340
        If fo% = 1 And (W% = 5 Or W% = 6) Then Y% = Int(Rnd(1) * 25) + 370
        If fo% <> 1 Then Y% = Int(Rnd(1) * 25) + 690

        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$)

        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 23
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, B1!(P))
            I6 = 0
            Call INCBATOFF(P, I5, I6)
        End If

        If S4% <> 4 Then

            If Int(Rnd(1) * 100) + 1 <= 25 And (L%(1, 13) = 11 Or L%(1, 13) = 8) Then Call PBP("fights the wind...")
            If Int(Rnd(1) * 100) + 1 <= 15 And (L%(1, 13) = 2 Or L%(1, 13) = 4 Or L%(1, 13) = 13 Or L%(1, 13) = 24) Then Call PBP("fights the lights...")

            If fo% <> 1 Then
                Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                If F% = 1 Then 13900
            End If

        End If

        fo% = 0
        
        Call PBP(Q$(4) + " makes the catch")
        Call CREDITREGPO(W%, D)

        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1
        I5 = P1%(D)
        I6 = 0
        
        Call ADDPIT(D, I5, I6)

        GoTo 13900

    End If

    'Case 90 to 100
    S2% = 21

    Call GETINFIELDER(W%, P, I1, D, S2%, P2)
    Call WINDEX
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
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
    End If

    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 13900

    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1

    Call CREDITREGPO(W%, D)
    I5 = P1%(D)
    I6 = 0

    Call ADDPIT(D, I5, I6)
    If O%(0) = 3 Then 13900

    Call d100(RN)

    If (S4% = 1 Or S4% = 5) And RN >= 80 Or (S4% = 1 Or S4% = 5) And P2 = 2 Then

        14012 'runner doubled off of 1st
        A5%(1) = 10
        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1

        Call ADDPIT(D, I5, I6)

        D3%(D) = D3%(D) + 1
        DP%(D) = DP%(D) + 1

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


    Else

        If (S4% = 4 Or S4% = 2) And RN >= 80 Or S4% = 4 And P2 = 2 Then

            'runner doubled off of 2nd
            A5%(2) = 10
            O%(0) = O%(0) + 1
            O%(1) = O%(1) + 1

            Call ADDPIT(D, I5, I6)

            D3%(D) = D3%(D) + 1
            DP%(D) = DP%(D) + 1
            I3 = 2

            Call CREDITPUTOUT(I3, FB2P%, AST%, D)
            Call PBP(" --- DOUBLE PLAY ---")
            Call CREDITASSIST(W%, D)
        
        End If

    End If

    14009 '
    Call ADVANCEMENT(P, D, S2%, F%, I3)

    GoTo 13900


    '--------------------


    14020 ' *** FLY  OUTS ***
    Call GETOUTFIELDER(W%, P, I1, D)

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P)): I6 = 0
        Call INCBATOFF(P, I5, I6)
    Else
        I5 = P1%(P): I6 = 23
        Call INCPITOFF(P, I5, I6)
    End If
    
    Call d100(RN)

    'Case 1 to 17
    If RN <= 17 Then
        14050 ' *** shallow fly ball ***
        S2% = 19
        Call WINDEX

        X$ = C$(W%)
        If Int(Rnd(1) * 100) <= 25 And W% = 7 Then X$ = "lcf"
        If Int(Rnd(1) * 100) <= 25 And W% = 9 Then X$ = "rcf"

        Call DELIVERY(D)

        'What is this used for?
        Call d100(RN)

        Y% = Int(Rnd(1) * 60) + 1170
        Call GETRPBP(Y%, Y$, D)
        Call STRIPRPBP(Y$, Y%, D)
        Call PBP(Y$ + " " + X$)

        Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
        If F% = 1 Then 13900
        GoTo 14025

    End If

    'Case 18 to 33
    If RN <= 33 Then
        ' *** deep fly out ***
        S2% = 17

        Call WINDEX
        Call LONGFLY(D, W%)

        Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
        If F% = 1 Then 13900

        Call CREDITREGPO(W%, D)
        Call CAUGHTFLY(RN, S2%, D)

        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1
        I5 = P1%(D)
        I6 = 0

        Call ADDPIT(D, I5, I6)

        If O%(0) = 3 Then 13900

        If B0%(3, 0) <> 0 Then

            A5%(3) = 1
            I5 = P1%(D)
            I6 = 16

            Call ADDPIT(D, I5, I6)

            If B7%(P, B1!(P)) = 1 Then
                I5 = P1%(P)
                I6 = 23
                Call PITOFF(P, I5, I6)
                I5 = P1%(P)
                I6 = 41
                Call INCPITOFF(P, I5, I6)
                GoTo 14066
            End If
            
            I5 = B3%(P, B1!(P))
            I6 = 0

            Call BATOFF(P, I5, I6)
            I5 = B3%(P, B1!(P))
            I6 = 19
            Call INCBATOFF(P, I5, I6)

        End If

        'ASSUMPTION: Somewhere in here is where it gets locked up
        14066 '
        If B0%(2, 0) <> 0 Then

            I4 = (B%(P, B3%(P, B0%(2, 0)), 16) - 5) * 25
            ZM = (B%(D, B9%(D, W%), 15) - 5) * 25
            I4 = I4 - ZM

            RN = Int(Rnd(1) * 1000) + 1

            If W% = 9 And S2% = 17 And RN <= 800 + I4 And P2 <> 2 Or W% = 9 And S2% = 18 And RN <= 400 + I4 And P2 <> 2 Or W% = 8 And S2% = 17 And RN <= 700 + I4 And P2 <> 2 Or W% = 8 And S2% = 18 And RN <= 350 + I4 And P2 <> 2 Or W% = 7 And S2% = 17 And RN <= 250 + I4 And P2 <> 2 Then A5%(2) = 1
            If P2 = 2 And B%(P, B3%(P, B0%(2, 0)), 16) > 6 And W% = 9 And S2% = 17 And RN <= 400 + I4 Or P2 = 2 And B%(P, B3%(P, B0%(2, 0)), 16) > 6 And W% = 8 And S2% = 17 And RN <= 400 + I4 Then A5%(2) = 1

            If A5%(2) <> 1 And S2% = 18 And W% = 9 And P2 <> 2 And S4% < 6 And RN > 800 Then
                I8! = 3
                LN% = 3308
                If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)


            Else

                If A5%(2) <> 1 And S2% = 18 And W% = 8 And P2 <> 2 And S4% < 6 And RN > 840 Then
                    I8! = 3
                    LN% = 3308
                    If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)

                Else

                    If A5%(2) <> 1 And W% = 7 And P2 <> 2 And S4% < 6 And RN > 880 Then
                        I8! = 3
                        LN% = 3308
                        If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)
                    End If

                End If

            End If

        End If

        14072 '
        If B0%(1, 0) <> 0 Then
            RN = Int(Rnd(1) * 1000) + 1
            A5%(1) = 0
            I3 = (B%(P, B3%(P, B0%(1, 0)), 16) - 5) * 25
            I4 = (B%(D, B9%(D, W%), 15) - 5) * 25
            J5 = I3 - I4

            If S2% = 17 And P2 <> 2 And S4% = 1 And RN <= 100 + J5 Then
                I8! = 2
                LN% = 3308
                If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)
            End If
            I8! = 2
            LN% = 3308
        End If

        '--- Double-check this sometime
        Call ADVANCEMENT(P, D, S2%, F%, I3)

        GoTo 13900

    End If


    '--------------------


    'Case 34 to 100
    ' *** regular fly out ***
    S2% = 18
    Call WINDEX
    X$ = C$(W%)
    If Int(Rnd(1) * 100) <= 25 And W% = 7 Then X$ = "lcf"
    If Int(Rnd(1) * 100) <= 25 And W% = 9 Then X$ = "rcf"

    Call DELIVERY(D)
    Call d100(RN)

    Y% = Int(Rnd(1) * 90) + 1070
    If (W% = 7 Or W% = 9) And RN <= 10 Then X$ = "": Y% = Int(Rnd(1) * 10) + 1160

    Call GETRPBP(Y%, Y$, D)
    Call STRIPRPBP(Y$, Y%, D)
    Call PBP(Y$ + " " + X$)
    
    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
    If F% = 1 Then 13900

    '--- where does this end?
    '--- can we turn into subroutine?
    14025 '
    Call CREDITREGPO(W%, D)
    Call CAUGHTFLY(RN, S2%, D)

    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    I5 = P1%(D)
    I6 = 0

    Call ADDPIT(D, I5, I6)

    If O%(0) = 3 Then 13900

    Call d100(RN)

    If S2% = 19 And S4% = 1 And RN <= 33 And P2 = 2 Or S2% = 19 And S4% = 5 And RN <= 33 Then
        A5%(1) = 10
        O%(0) = O%(0) + 1
        O%(1) = O%(1) + 1
        Call ADDPIT(D, I5, I6)
        D3%(D) = D3%(D) + 1
        DP%(D) = DP%(D) + 1
        Call CREDITASSIST(W%, D)
        GoTo 14009
    End If

    If B0%(3, 0) > 0 Then
        I8! = 4
        LN% = 3308
        If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, W%, SAH%, SB%, I8!, I9)

    End If

    If I9 = 1 Then
        I9 = 0
        Call ADVANCEMENT(P, D, S2%, F%, I3)
        GoTo 13900
    Else
        GoTo 14066
    End If


    '--------------------


    14100 ' *** BUNT ROUTINE ***
    Call WINDEX

    For I = 0 To 3: A5%(I) = 0: Next

    SH = 2
    S2% = 23

    If (S4% = 3 Or S4% > 4) And O%(0) < 2 Then Call SQUEEZECHOICE(SH)

    Call d100(RN)

    If B7%(P, B1!(P)) = 1 Then BR = P%(P, P1%(P), 32)
    If B7%(P, B1!(P)) <> 1 Then BR = B%(P, B3%(P, B1!(P)), 30)
    If INFPOS% > 0 Then BR = BR - 2
    If BR <= 1 Then BR = 1

    For I = 1 To 9:
        If RN <= BT%(L%(1, 10), BR, I) Then Exit For
    Next

    14107 '
    WW% = Int(Rnd(1) * 10)
    If WW% <= 2 Then W% = 5
    If WW% > 2 And WW% <= 4 Then W% = 1
    If WW% > 4 And WW% <= 7 Then W% = 3
    If WW% > 7 Then W% = 2
    Call GETFIELDNAME(W%, D)

    Call d100(RN)

    If RN <= 52 Then 14175

    If I = 5 And SH = 1 Then I = 6

    'This I and the ones in the case statement
    'all have to do with the For-Next loop
    Select Case I

        Case 1
            'PU
            Call PBP(Q$(0) + " bunts it in the air...")
            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            Call PBP(Q$(4) + " makes the catch")
            Call ADDSTTS(P, D)

            O%(0) = O%(0) + 1
            O%(1) = O%(1) + 1
            Call CREDITREGPO(W%, D)

            If O%(0) <> 3 And SH <> 2 Then
                '--- start of a new sub ???
                '--- where does 4111 end ???
                14111 '
                If W% <> 5 Then
                    Call CREDITASSIST(W%, D)
                    W% = 5
                    Call CREDITREGPO(W%, D)
                    Q$(5) = Q$(4)
                    Call GETFIELDNAME(W%, D)
                Else
                    Call CREDITASSIST(W%, D)
                    W% = 6
                    Call CREDITREGPO(W%, D)
                    Q$(5) = Q$(4)
                    Call GETFIELDNAME(W%, D)
                End If

                14115 '
                Call PBP(Q$(5) + " throws to " + Q$(4) + " at 3rd")
                A5%(3) = 10
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                O%(0) = O%(0) + 1
                O%(1) = O%(1) + 1
                I5 = P1%(D)
                I6 = 0

                Call ADDPIT(D, I5, I6)

                If SH = 1 Then
                    If O%(0) < 3 Then
                        GoTo 15051
                    Else
                        'O%(0) >= 3
                        B1!(P) = B1!(P) - 1
                    End If
                End If
            End If

            GoTo 13900

        Case 2
            'DP
            If S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7 Then

                Call PBP(Q$(0) + " lays down a good bunt")

                Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                If F% = 1 Then 13900

                Call PBP(Q$(4) + " fields it cleanly")

                If SH = 1 And (S4% = 5 Or S4% = 7) Then

                    If W% = 2 Then
                        Call PBP("and catches " + Q$(3) + " heading home!")
                        Call CREDITREGPO(W%, D)
                    Else
                        'W% <> 2
                        Call PBP("and throws home to nail " + Q$(3) + "!")
                        Call CREDITASSIST(W%, D)
                        W% = 2
                        Call CREDITREGPO(W%, D)
                    End If

                    A5%(3) = 11

                    For I = 0 To 2: A5%(0) = 1: Next

                    '14122
                    Call ADDSTTS(P, D)
                    O%(0) = O%(0) + 1
                    O%(1) = O%(1) + 1
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                    GoTo 13900

                End If

                If S4% = 7 And O%(0) < 2 Then

                    A5%(3) = 11

                    If W% = 2 Then
                        Call PBP("He steps on home for one...")
                        Call CREDITREGPO(W%, D)
                        Call CREDITASSIST(W%, D)
                    Else
                        'W% <> 2
                        Call PBP("He throws home for the force")
                        Call CREDITASSIST(W%, D)
                        W% = 2
                        Call CREDITREGPO(W%, D)
                        Call CREDITASSIST(W%, D)
                    End If

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call PBP("Back to first...")

                    A5%(0) = 11

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                    Call PBP("DOUBLE PLAY!!")

                    O%(0) = O%(0) + 2
                    O%(1) = O%(1) + 2

                    Call ADDSTTS(P, D)

                    I5 = P1%(D)
                    I6 = 0

                    Call ADDPIT(D, I5, I6)

                    If O%(0) < 3 Then
                        A5%(2) = 1
                        A5%(1) = 1
                        Call ADVANCEMENT(P, D, S2%, F%, I3)
                    End If

                    GoTo 13900

                End If

                Call PBP("He throws to 2nd")
                Call CREDITASSIST(W%, D)

                A5%(1) = 11

                Call ADDSTTS(P, D)

                O%(0) = O%(0) + 1
                O%(1) = O%(1) + 1
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                If O%(0) < 3 Then
                    A5%(0) = 11
                    Call CREDITASSIST(W%, D)

                    A5%(2) = 1

                    Call PBP("over to first...")
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call PBP("DOUBLE PLAY!!")
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    O%(0) = O%(0) + 1
                    O%(1) = O%(1) + 1
                    I5 = P1%(D)
                    I6 = 0
                    Call ADDPIT(D, I5, I6)
                End If

                GoTo 13900

            Else
                Call PBP(Q$(0) + " lays down the bunt...")
                Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                Call PBP(Q$(4) + " makes a nice fielding play")
                Call CREDITASSIST(W%, D): A5%(0) = 11
                If SH = 1 And (S4% = 3 Or S4% = 6) Then A5%(3) = 11: For I = 0 To 2: A5%(I) = 1: Next

                '14122
                Call ADDSTTS(P, D)
                O%(0) = O%(0) + 1
                O%(1) = O%(1) + 1
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                GoTo 13900
            End If

        Case 3
            Call LDRNRCTDN
            GoTo 13900

        Case 4
            14140 'SHGOOD
            Call PBP(Q$(0) + " lays down a good bunt...")
            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            Call PBP(Q$(4) + " makes a nice play")

            If S4% = 0 Then
                S2% = -12
                Call PBP(Q$(0) + " beats out an infield hit!")
                GoTo 13065

            Else
                '-- turn this into a unique sub?
                '-- or direct other code to this loop
                '-- with a value of 4 and a 'skip' variable
                14141 '
                I5 = P1%(D)
                I6 = 15

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) = 1 Then
                    I5 = P1%(P): I6 = 40: Call INCPITOFF(P, I5, I6)
                Else
                    I5 = B3%(P, B1!(P)): I6 = 18: Call INCBATOFF(P, I5, I6)
                End If

                Call d100(RN)

                If S4% = 5 And SH = 2 And RN > 98 Or RN > 97 Then
                    S2% = -12
                    Call PBP(Q$(0) + " beats it out!")
                    GoTo 13065
                End If

                A5%(0) = 11

                Call PBP("The only play is to first")

                I5 = P1%(D)
                I6 = 0

                Call ADDPIT(D, I5, I6)

                O%(0) = O%(0) + 1
                O%(1) = O%(1) + 1

                Call CREDITASSIST(W%, D)
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                If O%(0) < 3 Then
                    For I = 1 To 3: A5%(I) = 1: Next
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                End If

            End If

            GoTo 13900

        Case 5
            14150 'C@3
            Call d100(RN)
            If B7%(P, B1!(P)) = 1 Then RR = 2
            If B7%(P, B1!(P)) <> 1 Then RR = B%(P, B3%(P, B1!(P)), 16)
            If INFPOS% > 0 Then RR = RR - 2
            If RR <= 2 Then RR = 2

            For I = 0 To 2
                If RN <= CF%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1
                Case 1

                    If S4% > 0 Then 14157
                    Call PBP(Q$(0) + " drops it right down the line")
                    Call PBP(Q$(4) + " lets it roll...")
                    Call PBP(Q$(0) + " is trying for 2nd!!")
                    Call PBP(Q$(4) + " fields and throws too late!!")
                    Call PBP("You won't see that play very often")

                    A5%(0) = 2
                    I5 = P1%(D)
                    I6 = 1
                    Call ADDPIT(D, I5, I6)

                    If B7%(P, B1!(P)) <> 1 Then
                        I5 = B3%(P, B1!(P))
                        I6 = 1
                        Call INCBATOFF(P, I5, I6)
                        I6 = 4
                        Call INCBATOFF(P, I5, I6)
                        I6 = 0
                        Call INCBATOFF(P, I5, I6)
                    Else
                        I5 = P1%(P)
                        I6 = 23
                        Call INCPITOFF(P, I5, I6)
                        I6 = 24
                        Call INCPITOFF(P, I5, I6)
                        I6 = 27
                        Call INCPITOFF(P, I5, I6)
                    End If

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call SCOREBOARD
                    GoTo 13900

                Case 2
                    14157 '
                    S2% = 12
                    Call PBP(Q$(0) + " lays down a great bunt...")

                    Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                    If F% = 1 Then 13900

                    Call PBP(Q$(4) + " fields... but too late!")
                    S2% = -12
                    GoTo 13065

                Case 3
                    GoTo 14140

            End Select

        Case 6
            'PLYLDBS
            If B7%(P, B1!(P)) = 1 Then RR = 2
            If B7%(P, B1!(P)) <> 1 Then RR = B%(P, B3%(P, B1!(P)), 16)
            If INFPOS% > 0 Then RR = RR - 2
            If RR <= 2 Then RR = 2

            If S4% = 0 Then 14150
            If S4% = 1 Then I3 = 1: J3$ = "2nd"
            If S4% = 2 Or S4% = 4 Then I3 = 2: J3$ = "3rd"
            If S4% = 3 Or S4% > 4 Then I3 = 3: J3$ = "home"

            Call PBP(Q$(0) + " gets the bunt down...")

            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            If F% = 1 Then 13900

            Call PBP(Q$(4) + " fields it quickly...")
            Call PBP("Looks like a close play at " + J3$ + "!!")
            Call PBP("1- Throw to " + J3$ + " 2- Throw to 1st")

            If U6 = 2 Or U6 = 1 And P <> U9 Then
                I$ = "1"
            Else
                Do
                    I$ = GetKeyPress$
                    I5 = Val(I$)
                Loop Until I5 >= 1 And I5 <= 2
            End If

            If I5 = 2 Then 14141

            Call d100(RN)
            For I = 0 To 1
                If RN <= LB%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            If I = 0 Then
                PLB = 1
                Call LDRNRCTDN
            Else
                Call PBP(Q$(4) + " throws to " + J3$ + "... SAFE!!")
                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6)
                For I = 0 To 3: A5%(I) = 1: Next
                Call ADVANCEMENT(P, D, S2%, F%, I3)
            End If

            GoTo 13900

        Case 7
            'FLKSO
            Call d100(RN)

            If P%(D, P1%(D), 6) = 0 Then
                BR = 0
            Else
                BR = P%(D, P1%(D), 9) / P%(D, P1%(D), 6)
            End If

            14173 '
            If BR <= 4 Then PR = 5
            If BR > 4 And BR <= 5.4 Then PR = 4
            If BR > 5.4 And BR <= 6.9 Then PR = 3
            If BR > 6.9 And BR <= 8.4 Then PR = 2
            If BR > 8.4 And BR <= 11.9 Then PR = 1
            If BR > 11.9 Then PR = 0

            For I = 0 To 2
                If RN <= SO%(PR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1

                Case 1
                    GoTo 13601

                Case 2
                    U% = U% + 1

                    If usingGfx = 1 Then
                        PrintOnGfx 64, 28, Str$(U%), imageScreen&
                    Else
                        Locate 2, 58: Print U%
                    End If

                    Call WINDEX

                    Call d100(RN)
                    F$ = Q$(0) + " "
                    If RN <= 13 Or SH = 1 And RN <= 42 Then F$ = F$ + "misses the pitch"
                    If RN > 13 And RN <= 42 And SH <> 1 Then F$ = F$ + "takes a called strike"
                    If RN > 42 Then F$ = F$ + "bunts it foul"
                    Call PBP(F$)

                    If U% = 3 Then 13601

                    If SH = 1 And RN <= 42 Then W% = 2: Call GETFIELDNAME(W%, D): GoTo 14111

                    If U% = 1 Then
                        Call PBP("He has" + Str$(U%) + " strike")
                    Else
                        'U% > 1
                        Call PBP("He has" + Str$(U%) + "strikes")
                    End If
                    Call PBP(F$)

                    If S4% = 4 Or S4% = 2 Then INFPOS% = 0

                    If usingGfx = 1 Then _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128)

                    GoTo 15080

                Case 3
                    GoTo 14140

            End Select

        Case 8
            14180 'TFPLY
            Call PBP(Q$(0) + " lays down the bunt...")
            Call PBP(Q$(4) + " has a tough play")

            Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
            If F% = 1 Then 13900

            If W% = 1 Then RR = 3
            If W% <> 1 Then RR = B%(D, B9%(D, W%), 17)
            If INFPOS% > 0 And S4% <> 0 Then RR = RR + 1 + L%(1, 10)
            If INFPOS% > 0 And S4% = 0 Then RR = RR + L%(1, 10)
            If INFPOS% = 0 And S4% = 0 Then RR = RR - 1
            If RR > 11 Then RR = 11
            If RR < 0 Then RR = 0

            Call d100(RN)

            For I = 0 To 2
                If RN <= TP%(RR, I) Then Exit For
            Next

            14182 '
            'This I has to do with the above For-Next
            Select Case I + 1
                Case 1:
                    S2% = 12
                    Call PBP("but too late...base hit!")
                    GoTo 13065

                Case 2:
                    PLB = 1
                    Call LDRNRCTDN
                    GoTo 13900

                Case 3:
                    GoTo 14141

            End Select

        Case 9
            14175 '
            U% = U% + 1
            If usingGfx = 1 Then
                PrintOnGfx 64, 28, Str$(U%), imageScreen&
            Else
                Locate 2, 58: Print U%
            End If

            Call WINDEX
            Call d100(RN)

            F$ = Q$(0) + " "
            If RN <= 13 Or SH = 1 And RN <= 42 Then F$ = F$ + "misses the pitch"
            If RN > 13 And RN <= 42 And SH <> 1 Then F$ = F$ + "takes a called strike"
            If RN > 42 Then F$ = F$ + "bunts it foul"
            Call PBP(F$)

            If U% = 3 Then
                GoTo 13601
            End If

            If SH = 1 And RN <= 42 Then W% = 2: Call GETFIELDNAME(W%, D): GoTo 14111

            If U% = 1 Then
                Call PBP("He has" + Str$(U%) + " strike")
            Else
                'U% > 1
                Call PBP("He has" + Str$(U%) + "strikes")
            End If

            If S4% = 4 Or S4% = 2 Then INFPOS% = 0

            If usingGfx = 1 Then _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128)

            GoTo 15080

    End Select

    '-- at this point, each selection in the Case statement
    '-- should have directed the code somewhere else
    '-- in essence, this is kind of the end of this code


    '--------------------


    14800 '
    SH% = 0
    S2% = 16
    W% = 1
    Q$(4) = P$(D, P1%(D))

    Select Case S4%
        Case 0
            GoTo 15051
            'set a "skip" variable here

        Case 1, 5
            TGTBASE% = 2
            If P2 <> 2 Then
                Call WINDEX
                SB% = 1
                Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                SB% = 0
                If F% = 1 Then
                    B1!(P) = B1!(P) - 1
                    Call WHOSON1ST(S4%)
                    GoTo 13900
                End If
            End If

        Case 2, 4
            TGTBASE% = 3
            If P2 <> 2 Then
                Call WINDEX
                SB% = 1
                Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
                SB% = 0
                If F% = 1 Then
                    B1!(P) = B1!(P) - 1
                    Call WHOSON1ST(S4%)
                    GoTo 13900
                End If
            End If

        Case 3, Is >= 6
            TGTBASE% = 4

    End Select

    I2 = B3%(P, B0%(TGTBASE% - 1, 0))
    For I3 = 1 To 2
        A5%(I3) = 1
        A5%(3) = 0
    Next I3

    Call STEALCHANCE(AA, I2, TGTBASE%, CHANCE%)
    If B7%(P, B0%(TGTBASE% - 1, 0)) = 1 Then CHANCE% = 0

    'Reduce chance for stealing home
    If TGTBASE% = 4 Then CHANCE% = CHANCE% * .05

    If B%(P, I2, 14) > 0 Then Q0 = Int((B%(P, I2, 13) / (B%(P, I2, 13) + B%(P, I2, 14))) * 1000): GoTo 14813
    If B%(P, I2, 13) > 31 Then Q0 = 700 + ((B%(P, I2, 13) - 30) * 2): GoTo 14813
    If B%(P, I2, 13) > 10 Then Q0 = 600 + ((B%(P, I2, 13) - 10) * 10): GoTo 14813
    If B%(P, I2, 13) > 0 Then Q0 = 400 + ((B%(P, I2, 13) * 20))
    Q0 = 100

    14813 '
    If B7%(P, B0%(TGTBASE% - 1, 0)) = 1 Then Q0 = 100

    Call STEAL3RD(TGTBASE%, AA, Q0, H0%, D) ' *** SUCCESS OF 3B STEAL ***

    If P%(D, P1%(D), 0) = 1 And TGTBASE% = 2 Then CHANCE% = CHANCE% + 25: Q0 = Q0 + 10
    If P%(D, P1%(D), 0) = -1 And TGTBASE% = 2 Then CHANCE% = CHANCE% - 50: Q0 = Q0 - 30
    If P2 = 2 Then

        CHANCE% = 100
        Q0 = Q0 - 200
        W% = 2
        Q$(4) = B$(D, B9%(D, 2))
        TGBASE% = B3%(P, B0%(TGTBASE% - 1, 0))
        B0%(TGTBASE% - 1, 2) = 18

        Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
        If F% = 1 Then
            I5 = TGBASE%
            I6 = 9
            Call INCBATOFF(P, I5, I6)
            B1!(P) = B1!(P) - 1
            Call STRIKEOUTCS(SO)
            GoTo 13900
        Else
            GoTo 14827
        End If

    End If

    B0%(TGTBASE% - 1, 2) = B0%(TGTBASE% - 1, 2) + 1

    Call d100(I4)
    If I4 > B0%(TGTBASE% - 1, 2) Then 14825

    S2% = 22
    W% = 1
    Q$(4) = P$(D, P1%(D))

    14820 '
    Call WINDEX
    Call PBP(Q$(TGTBASE% - 1) + " caught off base in a rundown!")

    A5%(1) = 0
    A5%(2) = 0
    A5%(3) = 0
    A5%(TGTBASE% - 1) = 11

    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    Call PUTOUT(TGTBASE%, D, P)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

    I5 = P1%(D)
    I6 = 0
    Call ADDPIT(D, I5, I6)

    If O%(0) >= 3 Then B1!(P) = B1!(P) - 1: GoTo 13900

    INFPOS% = 0

    GoTo 15051


    '----------------------------------------
    '-- this is the start a new large area of code
    '-- perhaps it could become a sub?
    '----------------------------------------

    14825 'RUNNER HAS A CHANCE TO STEAL
    B0%(TGTBASE% - 1, 2) = B0%(TGTBASE% - 1, 2) + 17
    I9 = Int(Rnd(1) * 1000) + 1

    If I9 > CHANCE% * 1.388 And D5 <> 3 Then
        Call WINDEX
        Call NOJUMP(D)

        If usingGfx = 1 Then _PutImage (17 * FontColAdj%, 128)-Step(46 * FontColAdj%, 80), backupScreen&, imageScreen&, (17 * FontColAdj%, 128)-Step(46 * FontColAdj%, 80)

        GoTo 15085
    End If

    14826 '
    Call WINDEX
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

    If S4% <> 5 Then 14888 ' *** DOUBLE STEAL ***1st&3rd

    If SH% = 1 Then
        I9 = Int(Rnd(1) * 1000) + 1
    Else
        If U6 = 2 Or U6 = 1 And P <> U9 Then
            I$ = "Y"
        Else
            Call PBP("Throw to 2B (YN) ")
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then PJ = 7: GoTo 14829

        14888 '
        W% = 2
        Q$(4) = B$(D, B9%(D, 2))

        Call FLDERR(F%, W%, D, I3, SB%, S2%, P)
        If F% = 1 Then
            I5 = TGBASE%
            I6 = 9
            Call INCBATOFF(P, I5, I6)
            Call PITSWAP(TGTBASE%)
            B1!(P) = B1!(P) - 1
            GoTo 13900
        End If

        14827 '
        Call WHICHRUNNER(TGTBASE%, S4%, D5)
        I9 = Int(Rnd(1) * 1000) + 1
        Q0 = Q0 - ((B%(D, B9%(D, 2), 15) - 5) * 25)
    End If

    14828 '
    RN = Int(Rnd(1) * 11) + 1
    If TGTBASE% <> 4 Then
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
    End If

    14837 '
    If Not (B0%(TGTBASE% - 1, 2) = 18 And I9 > Q0 Or B0%(TGTBASE% - 1, 2) > 18 And I9 > Q0 - 200) Then

        14829
        Call d100(RN)
        If RN <= 20 Then F$ = "He beats the throw!"
        If RN > 20 And RN <= 40 Then F$ = "He's in there!"
        If RN > 40 And RN <= 50 Then F$ = "Throw is off-line..."
        If RN > 50 And RN <= 60 Then F$ = "Throw is high..."
        If RN > 60 And RN <= 80 Then F$ = "He slides under the tag!"
        If RN > 80 And TGTBASE% <> 4 Then F$ = U$(TGTBASE% + 1) + " with the call"
        If RN > 80 And TGTBASE% = 4 Then F$ = U$(1) + " with the call"
        Call PBP(F$)
        Call PBP("safe @ " + H$(TGTBASE%) + "!")

        B0%(TGTBASE% - 1, 2) = 0
        INFPOS% = 0
        Call PITSWAP(TGTBASE%)
        Call ADVANCEMENT(P, D, S2%, F%, I3)

        I5 = B9%(D, 2)
        I6 = 20
        Call INCBATDF(D, I5, I6)

        If SH% <> 1 Then
            14892 '
            If Not (S4% <> 5 Or S4% = 5 And PJ = 7) Then
                PJ = 0
                TGTBASE% = 4
                SS% = Int(Rnd(1) * 20) + 1

                If SS% = 6 Then
                    GoTo 14820
                Else
                    GoTo 14831
                End If
            Else
                GoTo 14831
            End If
        Else
            SH% = 0
            GoTo 15051
        End If

    End If

    14830 '
    Call d100(RN)

    Select Case RN
        Case 1 To 20
            F$ = B$(D, B9%(D, 2)) + " nails " + Q$(TGTBASE% - 1)

        Case 21 To 40
            F$ = Q$(TGTBASE% - 1) + " is pegged at " + H$(TGTBASE%)

        Case 41 To 60
            F$ = "Great throw by " + B$(D, B9%(D, 2)) + "!!"

        Case 61 To 80
            F$ = Q$(TGTBASE% - 1) + " slides into the tag"

        Case 81 To 100
            If TGTBASE% <> 4 Then
                F$ = U$(TGTBASE% + 1) + " with the call"
            Else
                'TGTBASE% = 4
                F$ = U$(1) + " with the call"
            End If
    End Select

    Call PBP(F$)

    A5%(TGTBASE% - 1) = 11
    INFPOS% = 0
    I5 = B3%(P, B0%(TGTBASE% - 1, 0))
    I6 = 10

    Call INCBATOFF(P, I5, I6)

    I5 = P1%(D)
    I6 = 0

    Call ADDPIT(D, I5, I6)

    O%(0) = O%(0) + 1
    O%(1) = O%(1) + 1
    W% = 2

    Call CREDITASSIST(W%, D)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

    I5 = B9%(D, 2)
    I6 = 21

    Call INCBATDF(D, I5, I6)

    If O%(0) >= 3 Then B1!(P) = B1!(P) - 1: Call STRIKEOUTCS(SO): GoTo 13900

    14831 '
    If O%(0) >= 3 Or P2 = 2 And U% = 3 Then 13900
    If SH% = 1 Then SH% = 0: GoTo 15051
    If AA < 20 Or S4% <> 5 Or B7%(P, B0%(3, 0)) = 1 Then 15051
    Call WINDEX

    If U6 = 2 Or U6 = 1 And P = U9 Then
        I$ = "N"
    Else
        Call PBP("TRY TO STEAL HOME (YN)")
        I$ = GetKeyPress$
    End If

    14835 '
    If UCase$(I$) = "Y" Then

        GoTo 15051

    Else
        SH% = 1
        TGTBASE% = 4
        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 1
        I2 = B3%(P, B0%(TGTBASE% - 1, 0))
        CHANCE% = 100
        Q0 = 700

        GoTo 14826 ' *** STEAL HOME ***

    End If


    '----------------------------------------
    '-- this is a major area of code, but
    '-- it's also short. perhaps it could
    '-- be made into a subroutine?
    '----------------------------------------
    
    14900 '
    Cls
    If usingGfx = 0 Then Screen 0
    P9 = D
    B1!(P) = B1!(P) + 1
    U% = 0
    If B1!(P) = 10 Then B1!(P) = 1

    Color 15: Print "DOES "; B$(D, B3%(D, B9%(D, 0))); " STAY IN THE GAME (YN)"
    I$ = GetKeyPress$

    If UCase$(I$) = "N" Then

        I5 = 77

    Else

        I5 = 66

        Locate 5, 1

        For I = 1 To 9:
            Color 3, 0
            If BP(P9) = I Then Color 9
            Locate I + 4, 5: Print Using "# "; I;
            If B7%(P9, I) = 1 And I5 = 66 Then Print B$(P9, B3%(P9, I));
            If B7%(P9, I) = 1 And I5 <> 66 Then Print P$(P9, B3%(P9, I));
            If B7%(P9, I) <> 1 Then Print B$(P9, B3%(P9, I));
            Print " "; C$(B7%(P9, I));
            Color 3
        Next I

        I5 = 0

        Do
            Color 15
            Locate 3, 1: Print "WHAT LINEUP SLOT WILL THE PITCHER BAT IN (1-9)"
            I$ = GetKeyPress$
            I2 = Val(I$)
        Loop Until I2 >= 1 And I2 <= 9

        If B7%(D, I2) = 10 Then B7%(D, I2) = 1

        INFPOS% = 0
        B7%(D, B9%(D, 0)) = B7%(D, I2)
        B9%(D, B7%(D, B9%(D, 0))) = B3%(D, B9%(D, 0))
        B7%(D, I2) = 1
        I7% = D
        I8% = B9%(D, 0)
        I9% = B3%(D, B9%(D, 0))

        Call REPLACEMENTS(I7%, I8%, I9%)

        I5 = 66

    End If

    GoTo 15504


    '-----------------------------------
    '-----      PERSONNEL MENU     -----
    '-----------------------------------
    '-- make this into a subroutine if possible!!!

    15500 '
    Cls
    If usingGfx = 1 Then Screen 0
    TG = 0
    PG = 0
    RG = 0
    Color 12: Print Tab(33); "PERSONNEL MENU": Print
    Color 14: Print "(0) ";: Color 15: Print "RESUME PLAY": Print
    Color 14: Print "(1) ";: Color 15: Print "RELIEF PITCHER": Print
    Color 14: Print "(2) ";: Color 15: Print "CHANGE BATTER/FIELDER"

    15501 '
    Do
        I$ = GetKeyPress$
        I = Val(I$)
        I5 = 0
    Loop Until I >= 0 And I <= 2

    'This I has to do with the above key loop
    Select Case I + 1

        Case 1
            15580 '
            For I = 1 To 9
                If B7%(P9, I) <> 1 Then
                    B%(P9, B3%(P9, I), 21) = 1
                End If
            Next I

            If usingGfx = 1 Then
                Call LOADSCREEN
                Call DisplayBallField_Gfx(BC%, P, U%, D)
            Else
                Call DisplayBallField(BC%, P, U%, D)
            End If

            GoTo 15051

        Case 2
            15504 '
            If P2 = 5 Then Call TIRING(P9): GoTo 15501

            15505 '
            Do

                Call PITDISP(P9)
                Locate 24, 1: Print "SELECT RELIEVER (Z = NO CHANGE)   ";
                If B9%(P9, 1) <> 99 Then Call TIRING(P9)
                Print

                15507 '
                Do

                    I$ = GetKeyPress$
                    If UCase$(I$) = "Z" Then 15500
                    Call LetterToNumber(I$)
                    I1 = Val(I$)

                Loop Until I1 <= 21 And P%(P9, I1, 15) <= 0

                Locate 23, 1: Print String$(78, 32)
                Locate 23, 1: Print "YOU CHOSE "; P$(P9, I1); " "; B1$(P%(P9, I1, 0) + 2); " - ";

                Print "ARE YOU SURE (YN)";
                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "Y"

            PPH = 0
            PR% = 0

            If S1%(P9) <> -1 And P6%(P9) > 1 Then I5 = P1%(P9): I6 = 22: Call OFFPIT(P9, I5, I6)

            P1%(P9) = I1
            P6%(P9) = P6%(P9) + 1
            P%(P9, I1, 15) = P6%(P9)
            Call PITSTAM(P9)
            Call CHECKSAVE(D, P, S4%)

            15518 '
            P9 = 1 - P9
            W5 = P9
            W6 = W5
            VV(P9) = 1
            VV1(P9) = 9
            Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

            P9 = 1 - P9

            If Not (DH% = 1 Or I5 = 66 Or I5 = 77) Then

                'No Designated Hitter

                Do

                    Cls
                    Locate , 14: Color 14: Print "DO YOU WISH TO MAKE A FLIP FLOP OR DOUBLE SWITCH (YN)";
                    Color 15

                    I$ = GetKeyPress$

                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then

                    Cls
                    Call DisplayBatter_Lineup(PG, P9)

                    15523 '
                    Call CHANGELINEUP(P9, P)

                    15524 '
                    Do
                        Locate 12, 30: Print "LINEUP SLOT FOR PITCHER TO BAT IN (1-9)"

                        I$ = GetKeyPress$
                        If Asc(I$) = 32 And PG = 0 Then PG = 1: Cls: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
                        If Asc(I$) = 32 And PG = 1 Then PG = 0: Cls: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
                        If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
                        If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
                        V2 = Val(I$)
                    Loop Until V2 >= 1 And V2 <= 9


                    Do
                        Locate 13, 30: Print "WHO WILL REPLACE "; B$(D, B3%(D, V2)); " @ "; C$(B7%(D, V2));

                        I$ = GetKeyPress$
                        If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9)
                        If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9)
                        If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG)
                        If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9)
                        Call LetterToNumber(I$)
                        V3 = Val(I$)

                    Loop Until V3 <= 22

                    Locate 16, 29: Print B$(D, V3)
                    Locate 17, 29: Print "ARE YOU SURE (YN)";

                    I$ = GetKeyPress$

                    If UCase$(I$) = "Y" Then

                        D0%(D) = D0%(D) + 1
                        X0%(D, 0, D0%(D)) = V3
                        X0%(D, 1, D0%(D)) = B7%(D, V2)

                        For V0 = 1 To 9
                            If B7%(D, V0) = 1 Then X0%(D, 2, D0%(D)) = (V0 * 10) + (D0%(D) - 9)
                        Next V0

                        For V0 = 1 To 9
                            If B7%(D, V0) = 1 Then B7%(D, V0) = B7%(D, V2): B3%(D, V0) = V3: B9%(D, B7%(D, V0)) = V3: P9 = D: W5 = D: W6 = D: VV(D) = V0: VV1(D) = V0: Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
                            VV(D) = 1
                            VV1(D) = 9
                        Next V0

                        B%(D, V3, 21) = 1
                        I7% = D
                        I8% = V2
                        I9% = V3

                        Call REPLACEMENTS(I7%, I8%, I9%)

                        I5 = 0

                        For V0 = 1 To 9
                            If V0 = V2 Then B7%(D, V2) = 1: B3%(D, V2) = P1%(D): B9%(D, 1) = P1%(D): VV(D) = V0: VV1(D) = V0: Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
                        Next V0

                        15537 '
                        VV(D) = 1
                        VV1(D) = 9
                        GoTo 15500
                    Else
                        GoTo 15518
                    End If

                End If

            End If

            I5 = 0

            For V0 = 1 To 9

                If B7%(D, V0) = 1 Or B7%(D, V0) = 10 Then

                    B3%(D, V0) = P1%(D)
                    B9%(D, 1) = P1%(D)
                    B7%(D, V0) = 1
                    P9 = D
                    W5 = D
                    W6 = D
                    VV(D) = V0
                    VV1(D) = V0

                    Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

                    VV(D) = 1
                    VV1(D) = 9
                    GoTo 15500

                End If

            Next V0

            GoTo 15540

        Case 3

            '--- START OF A SHORT SUB
            '-- ChangeBatterFielder

            15540 '
            Cls
            Call DisplayBatter_Lineup(PG, P9)
            Call CHANGELINEUP(P9, P)

            15546 '
            Locate 14, 29: Color 12: Print "SELECT:"
            Locate , 30: Color 14: Print " (1) ";: Color 15: Print "SELECT A PINCH-HITTER"
            Locate , 30: Color 14: Print " (2) ";: Color 15: Print "SWAP POSITIONS BETWEEN PLAYERS"
            Locate , 30: Color 14: Print " (3) ";: Color 15: Print "SUBSTITUTE PLAYER IN LINEUP"
            Locate , 30: Color 14: Print " (Z) ";: Color 15: Print "RESUME PLAY"
            Locate , 30: Color 14: Print "<SPACE> ";: Color 15: Print "TOGGLE ROSTER";
            Color 14: Print " <TAB> ";: Color 15: Print "TOGGLE FIELDING/BATTING"

            15547 '
            I$ = GetKeyPress$
            If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): GoTo 15547
            If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): GoTo 15547
            'GoTo 15590

            15590 '
            If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): GoTo 15547
            If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): GoTo 15547
            If UCase$(I$) = "Z" Then 15500
            I1 = Val(I$)
            If I1 = 2 Then 15567
            If I1 = 3 Then 15550
            If D2 = 6 Then 15550
            I2 = B1!(P): GoTo 15555

            For I5 = 20 To 23: Locate I5, 28
                Print Space$(52)
            Next I5

            Return

            '--- END OF A SHORT SUB

    End Select

    15550 '
    Do
        Locate 20, 30: Print "LINEUP SLOT TO REPLACE (1-9) (Z)=NO CHANGE"

        I$ = GetKeyPress$
        If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
        If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
        If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
        If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
        If UCase$(I$) <> "Z" Then I2 = Val(I$)

    Loop Until I2 >= 1 And I2 <= 9 And Not (P9 = D And B7%(P9, I2) = 1) Or UCase$(I$) = "Z"


    If UCase$(I$) <> "Z" Then

        Locate , 30
        If B7%(P9, I2) = 1 Then Print P$(P9, P1%(P9));
        If B7%(P9, I2) <> 1 Then Print B$(P9, B3%(P9, I2));

        15555 '
        Do
            Locate 22, 30: Print "# OF SUB"

            15556 '
            I$ = GetKeyPress$
            If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            Call LetterToNumber(I$)
            I1 = Val(I$)

        Loop Until I1 <= 22 Or B%(P9, I1, 21) <= 0

        Do
            Locate 23, 30
            Print B$(P9, I1); "... ARE YOU SURE (YN)"

            15560 '
            I$ = GetKeyPress$

        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"


        If UCase$(I$) <> "N" Then
            
            B%(P9, I1, 21) = 1
            B3%(P9, I2) = I1
            B9%(P9, B7%(P9, I2)) = I1

            If B7%(P9, I2) = 1 Or B7%(P9, I2) = 10 Then B9%(P9, 1) = 99: B9%(P9, 0) = I2: B7%(P9, I2) = 10

            D0%(P9) = D0%(P9) + 1
            X0%(P9, 0, D0%(P9)) = B3%(P9, I2)
            X0%(P9, 1, D0%(P9)) = B7%(P9, I2)
            X0%(P9, 2, D0%(P9)) = (I2 * 10) + (D0%(P9) - 9)
            W5 = P9
            W6 = P9
            VV(P9) = I2
            VV1(P9) = I2
            I7% = P9
            I8% = I2
            I9% = I1

            Call REPLACEMENTS(I7%, I8%, I9%)
            Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

            VV(P9) = 1
            VV1(P9) = 9

        End If

    End If

    GoTo 15540


    '--------------------


    15567 '
    Do
        Locate 20, 29: Print "B.O. #1 (1-9)"
        I$ = GetKeyPress$
        I1 = Val(I$)
    Loop Until I1 >= 1 And I1 <= 9 Or B7%(P9, I1) <> 1

    Do
        Locate 21, 29: Print "B.O. #2 (1-9)"
        I$ = GetKeyPress$
        I2 = Val(I$)
    Loop Until I1 >= 1 And I1 <= 9 Or B7%(P9, I2) <> 1

    I3 = B7%(P9, I2)
    B7%(P9, I2) = B7%(P9, I1)
    B7%(P9, I1) = I3
    B9%(P9, B7%(P9, I1)) = B3%(P9, I1)
    B9%(P9, B7%(P9, I2)) = B3%(P9, I2)

    I7% = P9
    I8% = I2: I9% = B3%(P9, I2)
    Call REPLACEMENTS(I7%, I8%, I9%)

    I7% = P9
    I8% = I1
    I9% = B3%(P9, I1)
    Call REPLACEMENTS(I7%, I8%, I9%)

    GoTo 15540


    '----------------------------------------
    ' Print final score and run post game
    ' routines (scoreboard, stats, etc.)
    '----------------------------------------

    18500 '
    'Locate 22, 1
    'Print winning team and score
    If S6%(0, 0) > S6%(1, 0) Then
        Call PBP(A$(0) + " defeats " + A$(1) + Str$(S6%(0, 0)) + " TO" + Str$(S6%(1, 0)) + "!")
    Else
        Call PBP(A$(1) + " defeats " + A$(0) + Str$(S6%(1, 0)) + " TO" + Str$(S6%(0, 0)) + "!")
    End If

    normalFinish% = 1

    If AP% <> 1 Or U6 <> 2 Then
        If pbpDelay! = 0 Then pbpDelay! = 0.1
        'Delay of 0 will not present the text
        'But we want to be sure the user sees it
        Call PBP("PRESS ANY KEY TO CONTINUE")
        I$ = GetKeyPress$
    End If

    Close #7

    Call EndOfGame

    End
    System

    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not yet been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    18700 '
    H0% = 0
    If usingGfx = 1 Then
        Call HOLDSTATUS_G(H0%)
    Else
        Call HOLDSTATUS(H0%)
    End If

    A1 = S6%(D, 0) - S6%(P, 0)

    If Not (A1 > 1 Or S4% = 3 Or S4% >= 6) Then
    
        If S4% <> 1 And S4% <> 5 Then
            I2 = 2
        Else

            I2 = 1
            J = B0%(I2, 0)

            If J <> 0 Then

                If B7%(P, J) <> 1 Then

                    If B7%(P, J) <> 1 And B%(P, B3%(P, J), 13) >= 10 Then
                        H0% = 1
                        If usingGfx = 1 Then
                            Call HOLDSTATUS_G(H0%)
                        Else
                            Call HOLDSTATUS(H0%)
                        End If
                    End If

                End If

            End If

        End If

    End If

    18800 '
    If SU%(D, 0) <> -1 And MG%(D, 589) = 0 And INNING% = MG%(D, 588) Then Call PITSTRAT
    If SU%(D, 1) <> -1 And MG%(D, 589) = 0 And INNING% > MG%(D, 588) Then Call PITSTRAT
    If SU%(D, 1) <> -1 And MG%(D, 589) <> 0 And INNING% >= MG%(D, 588) Then Call PITSTRAT

    If PZ = 1 Then PZ = 0: GoTo 15055
    
    A1 = S6%(D, 0) - S6%(P, 0)
    Call d100(RN)
    
    If Not (INNING% < 9 Or CZ%(D) = -1 Or MG%(D, 581) = 999) Then

        CZ%(D) = -1

        If A1 <= 3 And A1 >= 0 Then

            If P6%(D) <> 1 Then
                Call d100(RN)
                If RN <= 80 And P%(D, P1%(D), 3) < 15 Then GK%(D) = 1

            Else

                If S6%(P, 0) = 0 And P%(D, P1%(D), 5) > 0 And RN <= Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 5) * 100 + .5) + 10 Then 18725
                If P%(D, P1%(D), 5) > 0 And RN <= Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 5) * 100 + .5) Then 18725
                If CGERA% > 0 And INNING% > 7 And S6%(D, 0) > S6%(P, 0) And P%(D, P1%(D), 1) > 0 And P6%(D) = 1 And Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5) <= 100 And Int(Rnd(1) * 100) + 1 <= Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5) Then 18725
                If CGERA% > 0 And INNING% > 7 And (S6%(D, 0) >= S6%(P, 0) Or S6%(P, 0) - S6%(D, 0) <= 2) And P%(D, P1%(D), 1) > 0 And P6%(D) = 1 And Int(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5) >= 100 Then 18725
                If CGERA% >= 8 Then 18725

                GK%(D) = 1

            End If

        End If

    End If
 
    18725 '
    If GK%(D) = 1 Then
        GK%(D) = 0
        Call RELIEVERS
        If usingGfx = 1 Then _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128)
        GoTo 15055
    End If

    If B7%(P, B1!(P)) = 1 Then BR = 0
    If B7%(P, B1!(P)) <> 1 Then BR = B%(P, B3%(P, B1!(P)), 30)

    If P%(P, P1%(P), 11) <> 999 Then

        If I0 > 1 Then
            ' Variable I0 here should only have a value of 0/1
            Open "errlog" For Append As #9
            Print #9, "An error is about to happen"
            Print #9, "I0 -      "; I0
            Print #9, ""
            Close #9
        End If

        I7! = P%(I0, P1%(I0), 11)
    Else
        If P%(P, P1%(P), 20) = 0 Then
            I7! = 0
        Else
            I7! = Int((P%(P, P1%(P), 22) / P%(P, P1%(P), 20) * 1000 + .5))
        End If
    End If


    18726 '
    D2 = 1

    If INFPOS% <= 0 Then

        If MG%(D, 690) = 999 Then

            If Not (S4% < 3 Or S4% = 4) Then
                
                ' PROFILE INFIELD IN WITH 3RD OCCUPIED
                Call d100(RN)
                If O%(0) = 0 And A1 >= MG%(D, 691) * -1 And A1 <= MG%(D, 692) * -1 And INNING% >= MG%(D, 693) And RN <= MG%(D, 694) Then INFPOS% = 2: D2 = 3: GoTo 15055
                If O%(0) = 0 And A1 >= MG%(D, 695) * -1 And A1 <= MG%(D, 696) * -1 And INNING% >= MG%(D, 697) And RN <= MG%(D, 698) Then INFPOS% = 2: D2 = 3: GoTo 15055
                If O%(0) = 1 And (S4% = 5 Or S4% = 7) And A1 >= MG%(D, 699) * -1 And A1 <= MG%(D, 700) * -1 And INNING% >= MG%(D, 701) And RN <= MG%(D, 702) Then INFPOS% = 2: D2 = 3: GoTo 15055
                If O%(0) = 1 And (S4% = 5 Or S4% = 7) And A1 >= MG%(D, 703) * -1 And A1 <= MG%(D, 704) * -1 And INNING% >= MG%(D, 705) And RN <= MG%(D, 706) Then INFPOS% = 2: D2 = 3: GoTo 15055

            End If

        End If

        18801 '
        If MG%(D, 707) = 999 Then

            If S4% = 0 Then

                ' PROFILE CORNERS IN AGAINST A BUNT HIT
                Call d100(RN)
                If BR >= MG%(D, 708) And RN <= MG%(D, 709) Or BR >= MG%(D, 710) And RN <= MG%(D, 711) Or BR >= MG%(D, 712) And RN <= MG%(D, 713) Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

            End If

        End If

        18802 '
        If MG%(D, 714) = 999 And B7%(P, B1!(P)) <> 1 Then

            If O%(0) = 0 Then

                If Not (S4% <> 1 And S4% <> 4) Then

                    ' PROFILE CORNERS IN VS HITTER IN SAC BUNT SITUATION
                    Call d100(RN)
                    If A1 <= MG%(D, 715) And A1 >= MG%(D, 716) * -1 And S4% = 1 And BR >= MG%(D, 717) And RN <= MG%(D, 718) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    If A1 <= MG%(D, 719) And A1 >= MG%(D, 720) * -1 And S4% = 1 And BR >= MG%(D, 721) And RN <= MG%(D, 722) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    If A1 <= MG%(D, 723) And A1 >= MG%(D, 724) * -1 And S4% = 4 And BR >= MG%(D, 725) And RN <= MG%(D, 726) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    If A1 <= MG%(D, 727) And A1 >= MG%(D, 728) * -1 And S4% = 4 And BR >= MG%(D, 729) And RN <= MG%(D, 730) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    
                End If

            End If

        End If

        18803 '
        If Not (MG%(D, 731) <> 999 Or B7%(P, B1!(P)) <> 1 Or O%(0) = 2) Then

            If Not (S4% <> 1 And S4% <> 2 And S4% <> 4) Then

                ' PROFILE CORNERS IN VS A PITCHER IN SAC BUNT SITUATION
                Call d100(RN)

                If I7! <= MG%(D, 732) And RN <= MG%(D, 733) And S4% = 1 And O%(0) <= 1 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If
                
                If I7! <= MG%(D, 734) And RN <= MG%(D, 735) And S4% = 1 And O%(0) <= 1 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If
                
                If I7! <= MG%(D, 736) And RN <= MG%(D, 737) And (S4% = 2 Or S4% = 4) And O%(0) = 0 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If
                
                If I7! <= MG%(D, 738) And RN <= MG%(D, 739) And (S4% = 2 Or S4% = 4) And O%(0) = 0 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

            End If

        End If

        18804 '
        If MG%(D, 740) = 999 Then

            If Not (O%(0) = 2 Or S4% <> 3 And S4% < 5) Then

                ' PROFILE CORNERS IN FOR A SQUEEZE
                Call d100(RN)
                If O%(0) = 0 And A1 <= MG%(D, 741) And A1 >= MG%(D, 742) And BR >= MG%(D, 743) And RN <= MG%(D, 744) Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If
                If O%(0) = 1 And A1 <= MG%(D, 745) And A1 >= MG%(D, 746) And BR >= MG%(D, 747) And RN <= MG%(D, 748) Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If
                

            End If

        End If

        18805 '
        If MG%(D, 756) = 999 Then
            
            If Not (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then

                MOB% = 0

                For I = 1 To 3
                    If B0%(I, 0) > 0 Then MOB% = MOB% + 1
                Next

                If A1 + MOB% + 1 < 0 Then
                    ' BATTER NOT TYING OR WINNING RUN
                    Call d100(RN)

                    If B%(P, B3%(P, B1!(P)), 4) = 0 Then
                        I7! = 0
                        I6! = 0
                    Else
                        I6! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                        If I6! <= MG%(D, 757) And INNING% >= MG%(D, 758) And RN <= MG%(D, 759) Then D2 = 8: GoTo 18850
                        If I6! <= MG%(D, 760) And INNING% >= MG%(D, 761) And RN <= MG%(D, 762) Then D2 = 8: GoTo 18850
                        If I6! <= MG%(D, 763) And INNING% >= MG%(D, 764) And RN <= MG%(D, 765) Then D2 = 8: GoTo 18850
                    End If

                End If
                
            End If
            
        End If

        18806 '
        A1 = S6%(D, 0) - S6%(P, 0)

        If A1 >= -1 And A1 <= 1 And INNING% >= 8 And (S4% = 3 Or S4% >= 5) And O%(0) < 2 Then
            INFPOS% = 2
            D2 = 3

        Else

            If A1 > 0 Or O%(0) > 0 Or S4% <= 2 Or S4% = 4 Or S4% = 5 And O%(0) = 1 Or INNING% <= 5 And A1 < -2 Or INNING% > 5 And A1 < -4 Then

                If Not (S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% >= 5 Or O%(0) = 2 Or B1!(P) >= 3 And B1!(P) <= 6 Or INNING% <= 6 And A1 >= 3 Or INNING% <= 6 And A1 <= -3) Then
                    If Not (INNING% > 7 And A1 >= 1 Or INNING% > 7 And A1 < -1) Then
                        INFPOS% = 1
                        D2 = 4
                    End If
                End If
            Else
                INFPOS% = 2
                D2 = 3
            End If
        
        End If

        GoTo 15055

    End If

    18820 '
    If Not (A1 <= 0 Or A1 = 0 And S4% <= 1 Or A1 = 0 And S4% = 4 Or S4% = 7 Or B7%(P, B1!(P)) = 1) Then
    
        If INNING% >= 9 And P = 1 And O%(0) < 2 And A1 = 0 And S4% >= 2 And S4% <= 3 Or INNING% >= 9 And P = 1 And O%(0) = 2 And A1 = 0 And S4% >= 5 And S4% <= 6 Then
            '18835
            D2 = 2
            GoTo 15055
        End If

        Ib4 = B4%(P, 0, B1!(P))
        I1 = B4%(P, 3, B1!(P))
        If B1!(P) = 9 Then I2 = B4%(P, 0, 0): I3 = B4%(P, 3, 0)
        If B1!(P) < 9 Then I2 = B4%(P, 0, B1!(P) + 1): I3 = B4%(P, 3, B1!(P) + 1)

        If Not (S4% <> 2 And S4% <> 6) Then

            If Ib4 - I2 >= 50 And I1 - I3 >= 50 And A1 < 1 And INNING% > 8 Or Ib4 >= 280 And I1 >= 140 And INNING% > 8 And I1 > I3 And A1 < 1 Then
                D2 = 2
                GoTo 15055
            End If

        End If

    End If

    18850 '
    If (S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2)) Or B9%(D, 1) = 99 Then
        Call RELIEVERS
        If usingGfx = 1 Then _PutImage (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128), backupScreen&, imageScreen&, (20 * FontColAdj%, 48)-Step(44 * FontColAdj%, 128)
    End If

    GoTo 15055


    '--------------------


    18909 'GET A PINCH HITTER FOR A POSITION PLAYER
    If MG%(P, 540) = 999 Then

        18993 'SELECT PH FOR HITTER
        For I = 0 To 22

            If B%(P, I, 21) <= 0 And B$(P, I) <> "XXX" Then

                If B7%(P, B1!(P)) = B%(P, I, 22) Or B7%(P, B1!(P)) = B%(P, I, 23) Or B7%(P, B1!(P)) = B%(P, I, 24) Or B7%(P, B1!(P)) = B%(P, I, 25) Then

                    I1 = 6
                    If P%(D, P1%(D), 0) < 0 Then I1 = 1

                    For I = I1 To I1 + 4

                        If MG%(P, I1 + 540) <> -1 And B%(P, MG%(P, I1 + 540), 21) = 0 Then
                            mg% = MG%(P, I1 + 540)
                            MG%(P, I1 + 540) = -1

                            PPH = 1
                            D0%(P) = D0%(P) + 1
                            X0%(P, 0, D0%(P)) = mg%
                            X0%(P, 1, D0%(P)) = B7%(P, B1!(P))
                            X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9)
                            B%(P, I, 21) = 1
                            B3%(P, B1!(P)) = mg%
                            B9%(P, B7%(P, B1!(P))) = mg%

                            If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then
                                B9%(P, 1) = 99
                                B9%(P, 0) = B1!(P)
                                B7%(P, B1!(P)) = 10
                            End If

                            I7% = P
                            I8% = B1!(P)
                            I9% = B3%(P, B1!(P))
                            Call REPLACEMENTS(I7%, I8%, I9%)
                            GoTo 18938

                        End If

                    Next

                End If

            End If

        Next

    End If


    18935 '
    For I = 0 To 22:

        If Not (B%(P, I, 21) > 0 Or B$(P, I) = "XXX" Or B%(P, I, 4) = 0) Then

            If Not (B7%(P, B1!(P)) <> B%(P, I, 22) And B7%(P, B1!(P)) <> B%(P, I, 23) And B7%(P, B1!(P)) <> B%(P, I, 24) And B7%(P, B1!(P)) <> B%(P, I, 25)) Then

                '-- The next 3 IF statements execute identical code
                If P%(D, P1%(D), 0) = 1 And B%(P, I, 0) > 0 And B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 35 Then
                    PPH = 1
                    D0%(P) = D0%(P) + 1
                    X0%(P, 0, D0%(P)) = I
                    X0%(P, 1, D0%(P)) = B7%(P, B1!(P))
                    X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9)
                    B%(P, I, 21) = 1
                    B3%(P, B1!(P)) = I
                    B9%(P, B7%(P, B1!(P))) = I

                    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                    I7% = P
                    I8% = B1!(P)
                    I9% = B3%(P, B1!(P))
                    Call REPLACEMENTS(I7%, I8%, I9%)
                    GoTo 18938
                End If

                If P%(D, P1%(D), 0) = -1 And B%(P, I, 0) < 0 And B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 45 Then
                    PPH = 1
                    D0%(P) = D0%(P) + 1
                    X0%(P, 0, D0%(P)) = I
                    X0%(P, 1, D0%(P)) = B7%(P, B1!(P))
                    X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9)
                    B%(P, I, 21) = 1
                    B3%(P, B1!(P)) = I
                    B9%(P, B7%(P, B1!(P))) = I

                    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                    I7% = P
                    I8% = B1!(P)
                    I9% = B3%(P, B1!(P))
                    Call REPLACEMENTS(I7%, I8%, I9%)
                    GoTo 18938
                End If

                If B%(P, I, 0) = 0 And B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 30 Then
                    PPH = 1
                    D0%(P) = D0%(P) + 1
                    X0%(P, 0, D0%(P)) = I
                    X0%(P, 1, D0%(P)) = B7%(P, B1!(P))
                    X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9)
                    B%(P, I, 21) = 1
                    B3%(P, B1!(P)) = I
                    B9%(P, B7%(P, B1!(P))) = I

                    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                    I7% = P
                    I8% = B1!(P)
                    I9% = B3%(P, B1!(P))
                    Call REPLACEMENTS(I7%, I8%, I9%)
                    GoTo 18938
                End If

                '-- Done with identical If/Then code

            End If

        End If

    Next I

    GoTo 18941


    '--------------------


    '-- it's not clear what variable this "I" should be
    18930 'GET A PINCH HITTER FOR THE PITCHER
    If MG%(P, 540) = 999 Then

        'SELECT PH FOR PITCHER
        If P%(D, P1%(D), 0) < 0 Then
            I1 = 1
        Else
            I1 = 6
        End If

        For I = I1 To I1 + 4

            If MG%(P, I1 + 540) <> -1 And B%(P, MG%(P, I1 + 540), 21) = 0 Then

                PH% = MG%(P, I1 + 540)
                MG%(P, I1 + 540) = -1

                18934 '
                PPH = 1
                D0%(P) = D0%(P) + 1
                X0%(P, 0, D0%(P)) = PH%
                X0%(P, 1, D0%(P)) = 10
                X0%(P, 2, D0%(P)) = 81 + D0%(P)
                B%(P, I, 21) = 1
                B3%(P, B1!(P)) = PH%

                If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                18938 '
                P9 = P
                W5 = P9
                W6 = P9
                VV(P9) = B1!(P)
                VV1(P9) = B1!(P)
                Call HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)

                If usingGfx = 1 Then
                    Call DisplayBallField_Gfx(BC%, P, U%, D)
                    Call MAINSCREEN_G(D, P, BC%, U%, MO)
                Else
                    Call DisplayBallField(BC%, P, U%, D)
                    Call MAINSCREEN(D, P, BC%, U%, MO)
                End If

                VV(P9) = 1
                VV1(P9) = 9

                Q$(0) = B$(P, B3%(P, B1!(P)))

                If B7%(P, B1!(P)) = 1 Then Q$(0) = P$(P, P1%(D))
                Call WINDEX
                Call PBP(M$(P) + " is sending up a pinch hitter")
                PPH = 1

                GoTo 15052

            End If

        Next

    End If

    18933 '
    J% = 0
    GP% = 0

    If Not (CGERA% > 1 And INNING% < 4 Or (CGERA% > 2 And INNING% < 7) Or (CGERA% > 3 And INNING% < 9) Or (CGERA% >= 8)) Then

        If CGERA% > 0 And INNING% > 7 And S6%(P, 0) > S6%(D, 0) And P%(P, P1%(P), 1) > 0 And P6%(P) = 1 And Int(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5) <= 100 And Int(Rnd(1) * 100) + 1 <= Int(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5) Then 18941
        If CGERA% > 0 And INNING% > 7 And (S6%(P, 0) >= S6%(D, 0) Or S6%(D, 0) - S6%(P, 0) <= 2) And P%(P, P1%(P), 1) > 0 And P6%(P) = 1 And Int(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5) >= 100 Then 18941

        For Idx = 0 To 22

            If Not (B$(P, Idx) = "XXX" Or B%(P, Idx, 21) > 0 Or B%(P, Idx, 4) = 0) Then

                If B%(P, Idx, 0) > 0 And P%(D, P1%(D), 0) = 1 Or B%(P, Idx, 0) < 0 And P%(D, P1%(D), 0) = -1 Then
                    PH%(J%, 0) = Int(B%(P, Idx, 3) / 4 + .5)
                Else
                    PH%(J%, 0) = B%(P, Idx, 3)
                End If

                PH%(J%, 1) = Idx
                GP% = GP% + PH%(J%, 0)
                J% = J% + 1

            End If

        Next

        If J% <> 0 Then

            RN = Int(Rnd(1) * GP%) + 1

            For I = 0 To J% - 1
                If RN <= PH%(I, 0) Then
                    PH% = PH%(I, 1)
                    GoTo 18934
                Else
                    PH%(I + 1, 0) = PH%(I, 0) + PH%(I + 1, 0)
                End If
            Next

        End If

    End If
    'GoTo 18941

    18941 '
    If PR% <> 1 Then Call PINCHRUNNER(MS%, A1, S4%, I2, P, PR%, BC%, U%, D, MO)
    If MS% = 1 Then MS% = 0: GoTo 15052

    18971 '
    If B7%(P, B1!(P)) = 1 Then BR = 0
    If B7%(P, B1!(P)) <> 1 Then BR = B%(P, B3%(P, B1!(P)), 30)

    Call d100(RN)
    If S4% = 0 And (INFPOS% > 0 And RN <= 5 Or INFPOS% = 0 And RN <= 5) And BR > 8 Then P2 = 4: GoTo 14100

    ' MANAGER PROFILE TO HIT AND RUN
    Call d100(RN)
    If Not (S4% = 0 Or MG%(P, 671) <> 999 Or B7%(P, B1!(P)) = 1) Then
        If B%(P, B3%(P, B1!(P)), 4) = 0 Then I7! = 0 Else I7! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
        If B%(P, B3%(P, B1!(P)), 12) = 0 Then I6! = 0 Else I6! = Int(B%(P, B3%(P, B1!(P)), 4) / B%(P, B3%(P, B1!(P)), 12) + .5)

        18949 '
        If S4% = 1 And O%(0) = 0 And A1 <= MG%(P, 672) And A1 >= MG%(P, 673) * -1 And RN <= MG%(P, 674) And I7! <= MG%(P, 872) And I6! >= MG%(P, 866) Then P2 = 2: GoTo 15067
        If S4% = 1 And O%(0) = 1 And A1 <= MG%(P, 675) And A1 >= MG%(P, 676) * -1 And RN <= MG%(P, 677) And I7! <= MG%(P, 873) And I6! >= MG%(P, 867) Then P2 = 2: GoTo 15067
        If S4% = 5 And O%(0) = 0 And A1 <= MG%(P, 678) And A1 >= MG%(P, 679) * -1 And RN <= MG%(P, 680) And I7! <= MG%(P, 874) And I6! >= MG%(P, 868) Then P2 = 2: GoTo 15067
        If S4% = 5 And O%(0) = 1 And A1 <= MG%(P, 681) And A1 >= MG%(P, 682) * -1 And RN <= MG%(P, 683) And I7! <= MG%(P, 875) And I6! >= MG%(P, 869) Then P2 = 2: GoTo 15067
        If S4% = 4 And O%(0) = 0 And A1 <= MG%(P, 684) And A1 >= MG%(P, 685) * -1 And RN <= MG%(P, 686) And I7! <= MG%(P, 876) And I6! >= MG%(P, 870) Then P2 = 2: GoTo 15067
        If S4% = 4 And O%(0) = 1 And A1 <= MG%(P, 687) And A1 >= MG%(P, 688) * -1 And RN <= MG%(P, 689) And I7! <= MG%(P, 877) And I6! >= MG%(P, 871) Then P2 = 2: GoTo 15067
    End If

    18945 '
    If Not (MG%(P, 609) <> 999 Or B7%(P, B1!(P)) <> 1) Then

        If Not (S4% <> 1 And S4% <> 2 And S4% <> 4) Then

            Call d100(RN)
            If S4% = 1 And INNING% <= 7 And MG%(P, 610) >= O%(0) And A1 <= MG%(P, 611) And A1 >= MG%(P, 612) * -1 And RN <= MG%(P, 613) Then 18970
            If S4% = 1 And INNING% > 7 And MG%(P, 614) >= O%(0) And A1 <= MG%(P, 615) And A1 >= MG%(P, 616) * -1 And RN <= MG%(P, 617) Then 18970
            If S4% = 2 And INNING% <= 7 And MG%(P, 618) >= O%(0) And A1 <= MG%(P, 619) And A1 >= MG%(P, 620) * -1 And RN <= MG%(P, 621) Then 18970
            If S4% = 2 And INNING% > 7 And MG%(P, 622) >= O%(0) And A1 <= MG%(P, 623) And A1 >= MG%(P, 624) * -1 And RN <= MG%(P, 625) Then 18970
            If S4% = 4 And INNING% <= 7 And MG%(P, 626) >= O%(0) And A1 <= MG%(P, 627) And A1 >= MG%(P, 628) * -1 And RN <= MG%(P, 629) Then 18970
            If S4% = 4 And INNING% > 7 And MG%(P, 630) >= O%(0) And A1 <= MG%(P, 631) And A1 >= MG%(P, 632) * -1 And RN <= MG%(P, 633) Then 18970

        End If

    End If

    If MG%(P, 634) = 999 And B7%(P, B1!(P)) <> 1 And O%(0) = 0 Or (S4% <> 1 And S4% <> 2 And S4% <> 4) Then

        If B%(P, B3%(P, B1!(P)), 4) = 0 Then
            I7! = 0
            I6! = 0
        Else
            I7! = Int(B%(P, B3%(P, B1!(P)), 6) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
            I6! = Int((B%(P, B3%(P, B1!(P)), 6) + B%(P, B3%(P, B1!(P)), 7) + B%(P, B3%(P, B1!(P)), 8) * 2 + B%(P, B3%(P, B1!(P)), 9) * 3) / B%(P, B3%(P, B1!(P)), 4) * 1000 + .5)
        End If

        Call d100(RN)
        If S4% = 1 And INNING% <= 7 And A1 <= MG%(P, 635) And A1 >= MG%(P, 636) * -1 And I7! <= MG%(P, 637) And I6! <= MG%(P, 638) And P%(D, P1%(D), 10) <= MG%(P, 639) And RN <= MG%(P, 640) Then 18970
        If S4% = 2 And INNING% <= 7 And A1 <= MG%(P, 641) And A1 >= MG%(P, 642) * -1 And I7! <= MG%(P, 643) And I6! <= MG%(P, 644) And P%(D, P1%(D), 10) <= MG%(P, 645) And RN <= MG%(P, 646) Then 18970
        If S4% = 4 And INNING% <= 7 And A1 <= MG%(P, 647) And A1 >= MG%(P, 648) * -1 And I7! <= MG%(P, 649) And I6! <= MG%(P, 650) And P%(D, P1%(D), 10) <= MG%(P, 651) And RN <= MG%(P, 652) Then 18970
        If S4% = 1 And INNING% > 7 And A1 <= MG%(P, 653) And A1 >= MG%(P, 654) * -1 And I7! <= MG%(P, 655) And I6! <= MG%(P, 656) And P%(D, P1%(D), 10) <= MG%(P, 657) And RN <= MG%(P, 658) Then 18970
        If S4% = 2 And INNING% > 7 And A1 <= MG%(P, 659) And A1 >= MG%(P, 660) * -1 And I7! <= MG%(P, 661) And I6! <= MG%(P, 662) And P%(D, P1%(D), 10) <= MG%(P, 663) And RN <= MG%(P, 664) Then 18970
        If S4% = 4 And INNING% > 7 And A1 <= MG%(P, 665) And A1 >= MG%(P, 666) * -1 And I7! <= MG%(P, 667) And I6! <= MG%(P, 668) And P%(D, P1%(D), 10) <= MG%(P, 669) And RN <= MG%(P, 670) Then 18970

    End If

    P2 = 1
    If S4% = 1 Or S4% = 5 Then

        If A1 <= -1 And B%(P, B3%(P, B0%(1, 0)), 13) < 10 Or B0%(1, 2) > 1 Then 18943: 'CHANGED FROM <10
        If A1 >= -1 And S4% = 5 Then 18943
        If A1 < -5 Or B7%(P, B0%(1, 0)) = 1 And S4% = 1 Or B7%(P, B0%(2, 0)) = 1 And S4% = 2 Then 18943
        If A1 < 0 And INNING% >= 7 Then 18943
        P2 = 3
        GoTo 14800
    End If

    If S4% = 2 Or S4% = 4 Then
        If Not (B%(P, B3%(P, B0%(2, 0)), 13) < 40 Or B7%(P, B1!(P)) = 1 Or O%(0) = 2) Then
            If B0%(2, 2) <= 1 Then
                P2 = 3
                GoTo 14800
            End If
        End If
    End If

    18943 '
    If Not (S4% = 0 Or O%(0) > 0 Or S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 5 Or U% = 2 Or P = 0 And INNING% > 7) Then

        If B7%(P, B1!(P)) = 1 And S4% = 1 And O%(0) < 2 Or B7%(P, B1!(P)) = 1 And S4% = 2 And O%(0) = 0 Or B7%(P, B1!(P)) = 1 And S4% = 4 And O%(0) = 0 Then 18969
        If B1!(P) > 2 And B1!(P) < 6 Or B4%(P, 0, B1!(P)) > 290 And B7%(P, B1!(P)) <> 1 Or B7%(P, B1!(P)) <> 1 And B4%(P, 3, B1!(P)) > 70 Then 18968
        If INNING% > 7 And A1 = -1 And O%(0) = 0 And S4% = 1 Then 18969

    End If

    18968 '
    PPH = 0
    PR% = 0

    'If O%(0) = 2 Or B4%(P, 4, B1!(P)) > 100 Or S4% = 0 Then 15064
    'If S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 2 Or A1 < -3 Then 15064
    'If Int(Rnd(1) * 100) + 1 > 20 Then 15064
    If O%(0) = 2 Or B4%(P, 4, B1!(P)) > 100 Or S4% = 0 Or S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 2 Or A1 < -3 Or Int(Rnd(1) * 100) + 1 > 20 Then
        GoTo 15064
    Else
        P2 = 2
        GoTo 15067
    End If

    '----------

    18969 '
    If O%(0) >= 1 And INNING% >= 7 Then
        GoTo 18968
    Else
        18970 '
        P2 = 4
        GoTo 14100
    End If


End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'



Sub pbpLog (lineNbr%)

    Print #7, "Executing code from "; lineNbr%
    Print #7, "S6%(0, 1)"; S6%(0, 1)
    Print #7, "S6%(1, 1)"; S6%(1, 1)

End Sub

