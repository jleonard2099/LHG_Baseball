Sub LOADER

    Dim dayNight$(0 To 1), DH$(1), DL$(1)
    Dim LGT$(2), month$(1 To 12)
    Dim PU$(8), U6$(2), UMP$(50)
    Dim VI$(1), WE$(1)

    Dim fileLength&

    Shared PT$(), Q2$()
    Shared SP%()

    'Open "pbplog" For Output As #7

    _Title "Full Count Baseball - PLAY BALL!"
    ''$ExeIcon:'./lhg_fcbase.ico'
    '_Icon
    '_AllowFullScreen

    Color 7, 0
    Cls

    Randomize Timer

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

    'temp$(1) = "HOT"
    'temp$(2) = "WARM"
    'temp$(3) = "COOL"
    'temp$(4) = "COLD"

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

    'For I = 0 To 1: For I1 = 0 To 4: NB%(I, I1) = -1: Next: Next
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

                'Close #7
                'End
                'System
                Exit Sub

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
    For Idx = 0 To 1

        If AP% = 1 Then
            T1%(Idx) = NG%(Idx)
            YN$ = YN$(Idx)
            Call LoadTeam_LOADER(T1%(Idx), YN$, Idx)
        Else

            Call INFO

            Do
                skipCount% = 0

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
                Locate , 12: Print VI$(Idx)
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
                    Call SelectTeam_LOADER(NTMS, YN$, Idx) 'GoSub 8102
                Else
                    YN$ = NY$
                    Erase teamNames$

                    NTMS = 0

                    Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
                    fileLength& = LOF(1)

                    Field #1, 15 As Q2$(0)

                    For I1 = 1 To fileLength& / 4342

                        Get #1, I1

                        For I2 = 1 To 15
                            If Left$(Q2$(0), 1) = " " Or Asc(Mid$(Q2$(0), I2, 1)) < 32 Or Asc(Mid$(Q2$(0), I2, 1)) > 123 Or UCase$(Left$(Q2$(0), 3)) = "NOT" Or UCase$(Left$(Q2$(0), 3)) = "UN" Then skipCount% = 1: Exit For
                        Next I2

                        If skipCount% <> 1 Then
                            NTMS = NTMS + 1
                            teamNames$(NTMS) = Q2$(0)
                            teamIndex%(NTMS) = I1
                        End If

                    Next I1

                    Close 1

                    Call SelectTeam_LOADER(NTMS, YN$, Idx)

                End If

                If BO% = 1 Then
                    BO% = 0
                Else
                    Locate 2, 33
                    Color L%(Idx, 11), L%(Idx, 12): Print A$(Idx)

                    Color 15, 4

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                End If

            Loop Until UCase$(I$) = "N"

            YN$(Idx) = YN$
            A$(Idx) = RTrim$(A$(Idx))

            Color 15, 0

        End If 'Done checking for autoplay

    Next Idx

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

    'Close #7
    'End
    'System

End Sub

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

    Shared T1%()

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
        T1%(idx%) = teamIndex%(Selection%)
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


Sub pbpLog (lineNbr%)

    Print #7, "Executing code from: "; lineNbr%
    Print #7, Y$
    Print #7,

End Sub

