' STILL HAVE TO TAKE OLD FORMAT AND WRITE TO NEW FORMAT....HAVE REMOVED
' NEED FOR TEAM ABBREV....FIND WAY TO PUT MO AND DATE IN INTEGER VARIABLES
' POSSIBLY STORE EACH PART OF DATE IN 3 FIELDS ON INTEGER 6 VS 8 BYTES
' TIME SAME WAY, CONVERT TO MILITARY EQUIVAKLENT FOR STORAGE PURPOSES

Sub SCHEDULE ()

    DefInt A-Z

    Dim scheduleFile$

    ReDim menuChoices$(1 To 8)

    Shared NS%
    Shared scheduleAP%(), NG%()

    _Title "Full Count Baseball - Scheduler"

    NG%(0, 8) = 0
    NG%(0, 3) = 1
    NG%(0, 7) = 0
    NG%(0, 2) = 1
    NG%(0, 14) = 4

    Do

        Color 7, 0
        Cls

        Call TITLE_SCHEDULER
        Locate 16: Print: Color 2
        Print "GENERATING A STANDARD SEASON SCHEDULE IS TO BE USED IN CONJUNCTION"
        Print "WITH THE STANDINGS AND LEAGUE LEADERS OPTIONS. YOU MUST ALREADY HAVE"
        Print "CREATED AN APPROPRIATE LEAGUE FOR THIS OPTION TO PROPERLY FUNCTION."
        menuChoices$(1) = "ENTER A NEW SCHEDULE                             "
        menuChoices$(2) = "RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE          "
        menuChoices$(3) = "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE "
        menuChoices$(4) = "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE "
        menuChoices$(5) = "MAKE GLOBAL CHANGES TO A SCHEDULE                "
        menuChoices$(6) = "GENERATE A STANDARD SEASON SCHEDULE              "
        menuChoices$(7) = "BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS  "
        menuChoices$(8) = "EXIT AUTO SCHEDULER TO FULL COUNT MENU           "
        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 6
        Column% = 16
        Call box0(5, 15, 14, 65, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            action% = Selection% - 1
            'action% = 0 | Selection% = 1: ENTER A NEW SCHEDULE
            'action% = 1 | Selection% = 2: RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE
            'action% = 2 | Selection% = 3: EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE
            'action% = 3 | Selection% = 4: CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE
            'action% = 4 | Selection% = 5: MAKE GLOBAL CHANGES TO A SCHEDULE
            'action% = 5 | Selection% = 6: GENERATE A STANDARD SEASON SCHEDULE
            'action% = 6 | Selection% = 7: BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS

            Select Case action%
                Case 0:
                    scheduleAP%(0) = 0
                    NS% = 0

                    Do
                        Color 15, 0
                        Cls

                        JJ = 7

                        Locate 6, 8
                        Color 15, 1

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
                        Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
                        Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
                        Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
                        Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
                        Locate , 12: Print "SAMPLE NAMES ARE ARKANSAS, LSU-68, IU76, ETC."
                        Locate 7, 37: Input scheduleFile$

                        If scheduleFile$ <> "" Then

                            Do
                                Locate 13, 12: Print "ANY CHANGE (YN) "
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        End If

                    Loop Until UCase$(I$) = "N" And scheduleFile$ <> ""

                    Color 15, 0

                    Call NewSchedule(scheduleFile$)

                Case 1 To 4:
                    'Have the user pick a schedule
                    Call LoadSchedule(scheduleFile$)

                    If scheduleFile$ <> "" Then
                        Cls

                        'Extract / Change / Global
                        If action% >= 2 And action% <= 4 Then
                            Call ExtractSchedule(action%, scheduleFile$)
                            If action% >= 3 Then Call SaveScheduleFile(scheduleFile$)
                        Else
                            'Retrieve / Edit / Play Schedule
                            Color 15, 0
                            Call NewSchedule(scheduleFile$)
                        End If

                    End If 'Done checking for valid file

                Case 5:
                    'Generate schedule
                    Call CreateSeasonSchedule
                Case 6:
                    Call BoxscoreMaintenance
                Case 7: Cls
            End Select
        End If

    Loop Until Selection% = 8 Or ExitCode% = 27

End Sub

'----------------------------------------
'      CreateSeasonSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub CreateSeasonSchedule ()

    Shared BS%

    Shared scheduleAP%(), homeScore%(), visitingScore%()
    Shared NG%(), scheduleZ1%()
    Shared scheduleYN$(), Z1$(), Z2$()

    Call TITLE_SCHEDULER

    Color 15: Print "(0) ";
    Color 14: Print "8 TEAM, 154 GAME SCHEDULE"
    Print
    Color 15: Print "(1) ";
    Color 14: Print "10 TEAM, 162 GAME SCHEDULE"
    Print
    Color 15: Print "(2) ";
    Color 14: Print "12 TEAM, 2 DIVISION, 162 GAME SCHEDULE"
    Print
    Color 15: Print "(3) ";
    Color 14: Print "14 TEAM, 2 DIVISION, 162 GAME SCHEDULE"
    Print
    Color 15: Print "(4) ";
    Color 14: Print "RETURN TO FULL COUNT MAIN MENU"
    Do
        I$ = GetKeyPress$
        choice% = Val(I$)
        If I$ = Chr$(27) Then choice% = 4
    Loop Until choice% >= 0 And choice% <= 4

    If choice% <> 4 Then

        Do

            Spec$ = diskPaths$(2) + "*.LIB"
            null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
            Count% = DirCount%
            If Count% = 0 Then
                Locate 10, 8
                Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
            Else

                ReDim leagueFiles$(0 To Count%)

                For X = 1 To Count%
                    leagueFiles$(X) = Space$(12)
                Next

                leagueFiles$(0) = Spec$

                Call ReadFile(leagueFiles$())

                Do
                    Cls
                    Color , 1
                    Cls
                    Locate , 28
                    Color 14
                    Print "SELECT LEAGUE TO SCHEDULE": Color 15
                    Selection% = 1
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
                    MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                    _MouseHide

                    If ExitCode% <> 27 Then
                        leagueFile$ = leagueFiles$(Selection%)
                        Color 15, 1
                        Locate 2, 33: Print leagueFile$

                        Do
                            Locate , 33: Print "ANY CHANGE? (YN)";
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        Color 15, 0

                    End If

                Loop Until UCase$(I$) = "N" Or ExitCode% = 27

                If ExitCode% <> 27 Then

                    Open diskPaths$(2) + leagueFile$ For Input As #2
                    Input #2, LG$
                    Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$
                    For X = 1 To 40
                        Input #2, Z1$(X): Input #2, scheduleZ1%(X): Input #2, Z2$(X)
                    Next
                    Close 2

                    Select Case choice%
                        Case 0: scxFile$ = "154TEMP.SCX"
                        Case 1: scxFile$ = "162TENTM.SCX"
                        Case 2: scxFile$ = "16212TM.SCX"
                        Case 3: scxFile$ = "16214TM.SCX"
                    End Select

                    '-- We should make sure this exists first!!!
                    '-- If not, warn user and exit
                    Open scxFile$ For Random As #1 Len = 48
                    Y = LOF(1)
                    scheduleAP%(0) = Y / 48
                    BS% = Int((scheduleAP%(0) - 1) / 20)

                    For X = 0 To 18
                        Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
                    Next

                    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

                    For I = 1 To scheduleAP%(0)
                        Get #1, I
                        For X = 0 To 18: NG%(I, X) = CVI(Q$(X + 1)): Next
                        For X = 0 To 1: scheduleYN$(I, X) = RTrim$(Q$(20 + X)): Next

                        visitingScore%(I) = CVI(Q$(22))
                        homeScore%(I) = CVI(Q$(23))

                        If NG%(I, 14) > 8 Then NG%(I, 14) = 0
                        If NG%(I, 6) > 1 Then NG%(I, 6) = 1
                        If NG%(I, 7) > 1 Then NG%(I, 7) = 1
                        If NG%(I, 9) > 1 Then NG%(I, 9) = 1
                        If NG%(I, 10) > 1 Then NG%(I, 10) = 1
                        If NG%(I, 11) > 1 Then NG%(I, 11) = 1
                    Next

                    Close 1

                    Call ChangeGlobalSettings

                    Do
                        Color 15, 0
                        Cls

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
                        Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
                        Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
                        Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
                        Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
                        Locate , 12: Print "SAMPLE NAMES ARE MONTREAL, STL68, NY76, CIN90, ETC"
                        Locate 7, 37: Input scxFile$

                        If scxFile$ <> "" Then
                            scxFile$ = scxFile$ + ".SCD"

                            Do
                                Locate 13, 12
                                Print "ANY CHANGE (YN) "
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        End If

                    Loop Until UCase$(I$) = "N" And scxFile$ <> ""

                    Color 15, 0

                    For I = 1 To 20
                        Print scheduleZ1%(I), Z2$(I)
                    Next

                    For I = 1 To scheduleAP%(0)
                        Print "GAME"; I
                        Print NG%(I, 0), scheduleYN$(I, 0)
                        Print NG%(I, 1), scheduleYN$(I, 1)

                        Select Case choice%
                            Case 0 To 1:
                                X = NG%(I, 0)
                                NG%(I, 0) = scheduleZ1%(X)
                                scheduleYN$(I, 0) = Z2$(X)
                                X = NG%(I, 1)
                                NG%(I, 1) = scheduleZ1%(X)
                                scheduleYN$(I, 1) = Z2$(X)
                            Case 2:
                                If NG%(I, 0) <= 6 Then
                                    X = NG%(I, 0)
                                    Print X
                                    NG%(I, 0) = scheduleZ1%(X)
                                    scheduleYN$(I, 0) = Z2$(X)
                                Else
                                    X = NG%(I, 0)
                                    Print X
                                    NG%(I, 0) = scheduleZ1%(X + 4)
                                    scheduleYN$(I, 0) = Z2$(X + 4)
                                End If

                                If NG%(I, 1) <= 6 Then
                                    X = NG%(I, 1)
                                    Print X
                                    NG%(I, 1) = scheduleZ1%(X)
                                    scheduleYN$(I, 1) = Z2$(X)
                                Else
                                    X = NG%(I, 1)
                                    Print X
                                    NG%(I, 1) = scheduleZ1%(X + 4)
                                    scheduleYN$(I, 1) = Z2$(X + 4)
                                End If

                            Case 3:
                                If NG%(I, 0) <= 7 Then
                                    X = NG%(I, 0)
                                    NG%(I, 0) = scheduleZ1%(X)
                                    scheduleYN$(I, 0) = Z2$(X)
                                Else
                                    X = NG%(I, 0)
                                    NG%(I, 0) = scheduleZ1%(X + 3)
                                    scheduleYN$(I, 0) = Z2$(X + 3)
                                End If

                                If NG%(I, 1) <= 7 Then
                                    X = NG%(I, 1)
                                    NG%(I, 1) = scheduleZ1%(X)
                                    scheduleYN$(I, 1) = Z2$(X)
                                Else
                                    X = NG%(I, 1)
                                    NG%(I, 1) = scheduleZ1%(X + 3)
                                    scheduleYN$(I, 1) = Z2$(X + 3)
                                End If

                        End Select

                        Print "GAME"; I
                        Print NG%(I, 0), scheduleYN$(I, 0)
                        Print NG%(I, 1), scheduleYN$(I, 1)
                    Next 'done looping through games

                    Call SaveScheduleFile(scxFile$)

                End If 'Done checking for ESC

            End If 'Done checking for count = 0

            '...Until we press ESC
        Loop Until ExitCode% = 27 Or Count% = 0

    End If 'Done checking for exit

End Sub

'----------------------------------------
'       NewSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub NewSchedule (scheduleFile$)

    Dim TS$(0 To 1)
    TS$(0) = "N"
    TS$(1) = "Y"

    Shared NG%()
    Shared BS%, NS%
    Shared N$

    Shared MO$()

    Shared scheduleAP%(), homeScore%(), visitingScore%()
    Shared scheduleH$(), scheduleV$()
    Shared scheduleYN$(), TS$()

    'scheduleFile$ = scheduleFile$ + ".SCD"

    Do
        Cls
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
        Locate 1, 1: Print Tab(80);
        Locate 1, 1: Print scheduleFile$
        Locate 1, 33: Color 14: Print "AUTO SCHEDULER"

        Color 15, 0

        Locate 2, 1
        Color 14
        Print "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  DH  WEA": Color 15

        For X = 1 To 20

            Locate X + 2, 1

            If X + NS% * 20 <= scheduleAP%(0) Then

                T1 = NG%(X + NS% * 20, 0)
                YN$ = scheduleYN$(X + NS% * 20, 0)
                Call CheckTeam_Schedule(T1, YN$)
                N$ = RTrim$(Q$(0))
                scheduleV$(X) = N$ 'T$(0)

                T1 = NG%(X + NS% * 20, 1)
                YN$ = scheduleYN$(X + NS% * 20, 1)
                Call CheckTeam_Schedule(T1, YN$)
                N$ = RTrim$(Q$(0))
                scheduleH$(X) = N$ 'T$(0)

                'Game Numnber
                Print Using "####  "; X + NS% * 20;

                'Date
                Print Using "##/##/##"; NG%(X + NS% * 20, 17); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 18);

                'Time
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

                'Visitor
                Locate , 25: Print scheduleV$(X); " ";
                If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; visitingScore%(X + NS% * 20);

                'Home
                Locate , 46: Print scheduleH$(X); " ";
                If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; homeScore%(X + NS% * 20);

                'Mode of Play
                Locate , 67: Print MO$(NG%(X + NS% * 20, 8));

                'DH
                Locate , 72: If NG%(X + NS% * 20, 3) = 1 Then Print "Y";: Else Print "N";

                'Weather
                Locate , 75: Print TS$(1 - NG%(X + NS% * 20, 5))
            End If
        Next

        I$ = GetKeyPress$
        Select Case UCase$(I$)
            Case "A":
                ' *** ADD A GAME TO THE SCHEDULE ***
                V$ = "": H$ = ""
                scheduleAP%(0) = scheduleAP%(0) + 1
                targetGame% = scheduleAP%(0)
                BS% = Int((targetGame% - 1) / 20)
                If NS% < BS% Then NS% = BS%

                For X = 0 To 18
                    If X <> 13 Then
                        NG%(targetGame%, X) = NG%(targetGame% - 1, X)
                    End If
                Next

                For X = 0 To 1: scheduleYN$(targetGame%, X) = scheduleYN$(targetGame% - 1, X): Next

                DT$ = "04-04-93"
                TM$ = "07:30P"
                Call NewGame(V$, H$, targetGame%)

            Case "E"
                Call EditGame

            Case "B":
                'ReDim array$(500) 'holds the main editor string array
                'Do
                '   Locate 23, 1: Print "ENTER GAME # TO GO TO        "
                '   Locate 23, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                'Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)
                'NS% = Int((targetGame% - 1) / 20)
                'BN$ = diskPaths$(1) + "box" + Mid$(Str$(targetGame%, 2, Len(Str$(targetGame%))
                'Call EditBoxscore (BN$)

            Case "D":
                ' *** DELETE A GAME FROM THE SCHEDULE ***
                Do
                    Locate 24, 1: Input "ENTER GAME # TO DELETE "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

                For X = targetGame% To scheduleAP%(0) - 1
                    For X1 = 0 To 18
                        NG%(X, X1) = NG%(X + 1, X1)
                    Next
                    visitingScore%(X) = visitingScore%(X + 1)
                    homeScore%(X) = homeScore%(X + 1)
                    For X1 = 0 To 1
                        scheduleYN$(X, X1) = scheduleYN$(X + 1, X1)
                    Next
                Next

                scheduleAP%(0) = scheduleAP%(0) - 1
                BS% = Int((scheduleAP%(0) - 1) / 20)
                If NS% > BS% Then NS% = BS%

            Case "G":
                'GOTO A SPECIFIC PAGE
                Do
                    Locate 23, 1: Print "ENTER GAME # TO GO TO        "
                    Locate 23, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)
                NS% = Int((targetGame% - 1) / 20)

            Case "S":
                Call SortSchedule

            Case "C", "X":
                Call SaveScheduleFile(scheduleFile$)

            Case Chr$(0) + "Q":
                ' *** PAGE DOWN ***
                If NS% >= BS% Then
                    NS% = BS%
                Else
                    NS% = NS% + 1
                End If

            Case Chr$(0) + "I":
                ' *** PAGE UP ***
                If NS% <> 0 Then
                    NS% = NS% - 1
                End If

            Case "Q", Chr$(27): '

            Case "P":
                Call SaveScheduleFile(scheduleFile$)

                ' *** PLAY GAMES ***
                Color , 0
                Cls

                JJ = 8

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

                Do
                    Locate 7, 32
                    Locate , 12: Print "TOTAL GAMES ="; scheduleAP%(0)
                    Locate , 12: Print "SELECT GAME # TO START AND END WITH": Print
                    Locate , 12: Input "ENTER START GAME #: "; SG%: Print
                    Locate , 12: Input "ENTER END GAME #:"; EG%: Print

                    Do
                        Locate , 12: Print "ANY CHANGE (YN)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Loop Until UCase$(I$) = "N"

                If SG% <= 0 Then SG% = 1
                If EG% > scheduleAP%(0) Then EG% = scheduleAP%(0)

                Open "GAME82" For Output As #2
                Print #2, scheduleFile$ ' + ".SCD"
                Print #2, SG% 'Start Game
                Print #2, EG% 'End Game
                Print #2, 0
                Close 2

                For I = 1 To scheduleAP%(0)
                    HR% = Int(NG%(I, 15) / 100)
                    MIN% = NG%(I, 15) - HR% * 100
                    TM% = HR% * 60 + MIN%
                    NG%(I, 12) = Int(TM% / 30 - 18)
                Next

                Open "TICKER" For Output As #2
                For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
                Print #2, -1: Next: Next: Next
                For I = 1 To 32: For I1 = 0 To 1: Print #2, "XXX"
                Next: Next
                For I = SG% To SG% + 31
                    Print #2, NG%(I, 12)
                Next
                Close 2

                Run "LOADER.EXE"

            Case "R":
                ' *** RESET GAMES ***
                For I = 1 To scheduleAP%(0)
                    NG%(I, 13) = 0
                Next
        End Select

    Loop Until UCase$(I$) = "Q" Or UCase$(I$) = "X"

End Sub

'----------------------------------------
'       EditGame SubRoutine
'----------------------------------------
'...explanation...
Sub EditGame ()

    Shared scheduleAP%()
    Shared scheduleH$(), scheduleV$()
    Shared NG%()

    ' *** EDIT A GAME ***
    Do
        Locate 23, 1: Print "ENTER GAME # TO EDIT        "
        Locate 23, 1: Input "ENTER GAME # TO EDIT "; targetGame%
    Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

    V$ = scheduleV$(targetGame% - Int(targetGame% / 20) * 20)
    H$ = scheduleH$(targetGame% - Int(targetGame% / 20) * 20)
    If targetGame% - Int(targetGame% / 20) * 20 = 0 Then V$ = scheduleV$(20): H$ = scheduleH$(20)

    I$ = "A"
    HR% = Int(NG%(targetGame%, 15) / 100)
    MIN% = NG%(targetGame%, 15) - HR% * 100

    If HR% = 12 Then I$ = "P"
    If HR% > 12 Then HR% = HR% - 12: I$ = "P"
    TM$ = Right$(Str$(HR%), 2) + ":"

    If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
    If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)

    TM$ = TM$ + I$
    DT$ = Right$(Str$(NG%(targetGame%, 17)), 2) + "/"
    DY% = Val(Right$(Str$(NG%(targetGame%, 16)), 2))

    If DY% < 10 Then DY$ = "0" + Right$(Str$(NG%(targetGame%, 16)), 1)
    If DY% >= 10 Then DY$ = Right$(Str$(NG%(targetGame%, 16)), 2)

    DT$ = DT$ + DY$ + "/" + Right$(Str$(NG%(targetGame%, 18)), 2)

    Call NewGame(V$, H$, targetGame%)

End Sub

'----------------------------------------
'           NewGame SubRoutine
'----------------------------------------
'...explanation...
Sub NewGame (visitor$, home$, gameIndex%)

    Dim TS$(1), U5$(4), PU$(48)
    Dim displayPause!

    Shared N$
    Shared scheduleH$(), scheduleV$()
    Shared scheduleYN$()

    Shared NG%(), scheduleL%()

    TS$(0) = "N"
    TS$(1) = "Y"
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

    Do
        Cls
        Color 2: Print Tab(29); "SELECT GAME OPTIONS": Print

        151 Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(NG%(gameIndex%, 8))
        152 Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: "; visitor$
        Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: "; home$
        154 Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "DESIGNATED HITTER: "; TS$(NG%(gameIndex%, 3))
        'NG%(X + NS% * 20, 3)
        157 Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(gameIndex%, 4) / 10
        160 Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "GAME DATE (XX-XX-XX) "; DT$; "   "
        161 Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print "GAME TIME (XX:XXP) "; TM$; "   "
        158 Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "USE WEATHER EFFECTS: "; TS$(1 - NG%(gameIndex%, 5))
        Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print "USE INJURIES "; TS$(1 - NG%(gameIndex%, 6))
        Locate 11, 2: Color 14: Print "(A) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; TS$(1 - NG%(gameIndex%, 7))
        Locate 12, 2: Color 14: Print "(B) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; TS$(1 - NG%(gameIndex%, 9))
        Locate 13, 2: Color 14: Print "(C) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; TS$(1 - NG%(gameIndex%, 10))
        Locate 14, 2: Color 14: Print "(D) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; TS$(1 - NG%(gameIndex%, 11))
        Locate 15, 2: Color 14: Print "(E) ";: Color 15: Print "PITCHER ERA : "; PU$(NG%(gameIndex%, 14))

        176 Locate 16, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) = "A" Then I$ = "10"
            If UCase$(I$) = "B" Then I$ = "11"
            If UCase$(I$) = "C" Then I$ = "12"
            If UCase$(I$) = "D" Then I$ = "13"
            If UCase$(I$) = "E" Then I$ = "14"
            I3% = Val(I$)

            Select Case I3%
                Case 1:
                    NG%(gameIndex%, 8) = NG%(gameIndex%, 8) + 1: If NG%(gameIndex%, 8) > 3 Then NG%(gameIndex%, 8) = 0
                    GoTo 151
                Case 2, 3:
                    Do
                        Color 15, 0: Cls
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

                        If NY$ = "" And YN$ <> "" Then
                            NY$ = YN$
                            Call GetTeam_Schedule(numberTeams, YN$, T1)
                        Else
                            YN$ = NY$
                            Erase teamNames$
                            numberTeams = 0
                            Call SelectTeam_Schedule(numberTeams, YN$)
                            Call GetTeam_Schedule(numberTeams, YN$, T1)
                        End If

                        Call CheckTeam_Schedule(T1, YN$)
                        'N$ = RTrim$(Q$(0))
                        N$ = teamNames$(T1)

                        If BO% = 1 Then
                            BO% = 0
                        Else
                            Locate 2, 33
                            Color scheduleL%(11), scheduleL%(12): Print N$

                            Color 15, 4

                            Do
                                Locate , 33: Print "ANY CHANGE (YN)";
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                            If UCase$(I$) = "N" Then

                                Color 15, 0
                                NG%(gameIndex%, I3% - 2) = T1
                                scheduleYN$(gameIndex%, I3% - 2) = YN$

                                If I3% = 2 Then
                                    visitor$ = N$
                                    scheduleV$(gameIndex% - Int(gameIndex% / 20) * 20) = N$
                                End If

                                If I3% = 3 Then
                                    home$ = N$
                                    scheduleH$(gameIndex% - Int(gameIndex% / 20) * 20) = N$
                                End If

                            End If

                        End If
                        'GoTo 149

                    Loop Until UCase$(I$) = "N" 'BO% = 0

                Case 4:
                    NG%(gameIndex%, 3) = 1 - NG%(gameIndex%, 3)
                    GoTo 154

                Case 5:
                    Locate 6, 32: Input displayPause!
                    NG%(gameIndex%, 4) = displayPause! * 10
                    GoTo 157

                Case 6:
                    Locate 7, 27: Input DT$
                    GoTo 160

                Case 7:
                    Locate 8, 25: Input TM$
                    GoTo 161

                Case 8:
                    NG%(gameIndex%, 5) = 1 - NG%(gameIndex%, 5)
                    GoTo 158

                Case 9:
                    NG%(gameIndex%, 6) = 1 - NG%(gameIndex%, 6)

                Case 10:
                    NG%(gameIndex%, 7) = 1 - NG%(gameIndex%, 7)

                Case 11:
                    NG%(gameIndex%, 9) = 1 - NG%(gameIndex%, 9)

                Case 12:
                    NG%(gameIndex%, 10) = 1 - NG%(gameIndex%, 10)

                Case 13:
                    NG%(gameIndex%, 11) = 1 - NG%(gameIndex%, 11)

                Case 14:
                    NG%(gameIndex%, 14) = NG%(gameIndex%, 14) + 1
                    If NG%(gameIndex%, 14) > 8 Then NG%(gameIndex%, 14) = 0

            End Select

        Loop Until (I3% >= 0 And I% <= 14)

        If visitor$ <> "" And home$ <> "" And TM$ <> "" And DT$ <> "" Then
            I$ = Right$(TM$, 1)
            TM$ = Left$(TM$, Len(TM$) - 1)
            MIN% = Val(Right$(TM$, 2))
            TM$ = Left$(TM$, Len(TM$) - 3)
            HR% = Val(TM$)
            If UCase$(I$) = "P" And HR% < 12 Then HR% = HR% + 12
            NG%(gameIndex%, 15) = HR% * 100 + MIN%
            NG%(gameIndex%, 17) = Val(Left$(DT$, 2))
            NG%(gameIndex%, 18) = Val(Mid$((DT$), 7, 2))
            NG%(gameIndex%, 16) = Val(Mid$((DT$), 4, 2))
        End If

    Loop Until I3% = 0

End Sub

'----------------------------------------
'       ExtractSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub ExtractSchedule (scheduleAction%, scheduleFile$)
    'scheduleAction% = 2: "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE
    'scheduleAction% = 3: "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE
    'scheduleAction% = 4: "MAKE GLOBAL CHANGES TO A SCHEDULE
    Dim displayPause!

    Shared N$
    Shared NG%(), homeScore%(), visitingScore%()
    Shared scheduleAP%(), scheduleL%()
    Shared scheduleT%(), scheduleZ1%()
    Shared scheduleYN$()

    If scheduleAction% = 4 Then
        ' GLOBAL SETTINGS FOR A SCHEDULE
        Call ChangeGlobalSettings
        Call SaveScheduleFile(scheduleFile$)
        Call NewSchedule(scheduleFile$)
    Else
        'Action 2 | 3 = prompt for new file name
        If scheduleAction% = 2 Then

            Color 15, 0
            Cls

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

            Do
                Locate 7, 32
                Locate , 12: Print "ENTER NAME FOR SUB-SCHEDULE: ": Print
                Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
                Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
                Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
                Locate , 12: Print "SAMPLE NAMES ARE INDIANA, BOS68, CHI-76, ETC      "
                Locate 7, 37: Input subSchedFile$

                If subSchedFile$ <> "" Then

                    Do
                        Locate 13, 12
                        Print "ANY CHANGE (YN) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                End If

            Loop Until (UCase$(I$) = "N" And subSchedFile$ <> "") Or I$ = Chr$(27)

            If I$ <> Chr$(27) Then

                Color 15, 0
                Cls

            End If

        End If

        If I$ <> Chr$(27) Then

            'Select team to target
            Do

                725 Color 15, 0: Cls
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
                Locate , 12: Print "SELECTING TEAM TO CHANGE CONTROL FOR"
                Locate , 12: Print "ENTER DISK ID: ": Print
                Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
                Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
                Locate , 12: Print
                Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
                Locate 8, 27: Input NY$
                If NY$ = "" And YN$ <> "" Then
                    Call GetTeam_Schedule(numberTeams, YN$, T1)
                Else
                    YN$ = NY$
                    Erase teamNames$
                    numberTeams = 0
                    Call SelectTeam_Schedule(numberTeams, YN$)
                    Call GetTeam_Schedule(numberTeams, YN$, T1)
                End If

                N$ = RTrim$(Q$(0))

                If BO% = 1 Then
                    BO% = 0
                Else
                    Locate 2, 33
                    Color scheduleL%(11), scheduleL%(12): Print N$

                    Color 15, 4

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
                        J$ = GetKeyPress$
                    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

                If UCase$(J$) = "N" Then

                    Color 15, 0

                    'Change mode of play
                    If scheduleAction% = 3 Then
                        Cls
                        Color scheduleL%(11), scheduleL%(12)
                        Print N$;: Color 15, 0: Print " IS TO BE:"
                        Print
                        Print "(0) MANUALLY CONTROLLED": Print
                        Print "(1) COMPUTER CONTROLLED"

                        Do
                            C$ = GetKeyPress$
                            I1 = Val(C$)
                        Loop Until I1 = 0 Or I1 = 1

                        Input "ENTER DISPLAY VARIABLE FOR THESE GAMES "; displayPause!
                        'displayPause! = displayPause! * 10
                    End If

                    'HAVE T1 AND YN$
                    For I = 1 To scheduleAP%(0)

                        scheduleYN$(I, 0) = RTrim$(scheduleYN$(I, 0))
                        scheduleYN$(I, 1) = RTrim$(scheduleYN$(I, 1))

                        If T1 = NG%(I, 0) And YN$ = scheduleYN$(I, 0) Or T1 = NG%(I, 1) And YN$ = scheduleYN$(I, 1) Then

                            ' CHANGE SETTINGS FOR A TEAM
                            If T1 = NG%(I, 0) And YN$ = scheduleYN$(I, 0) Then NG%(I, 8) = 2
                            If T1 = NG%(I, 1) And YN$ = scheduleYN$(I, 1) Then NG%(I, 8) = 1
                            If (T1 = NG%(I, 0) And YN$ = scheduleYN$(I, 0) Or T1 = NG%(I, 1) And YN$ = scheduleYN$(I, 1)) And I1 = 1 And scheduleAction% = 3 Then NG%(I, 8) = 3
                            NG%(I, 4) = displayPause! * 10

                            'Extract only
                            If Not (scheduleAction% = 3) Then
                                'Write matching games
                                Locate 1, 1: Print numberGames% + 1; " MATCHES"

                                Open diskPaths$(0) + subSchedFile$ + ".SCD" For Random As #1 Len = 48
                                Y = LOF(1)

                                numberGames% = Y / 48

                                For X = 0 To 18
                                    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
                                Next

                                Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

                                For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next

                                For X = 0 To 1: LSet Q$(20 + X) = scheduleYN$(I, X): Next

                                LSet Q$(22) = MKI$(visitingScore%(I)): LSet Q$(23) = MKI$(homeScore%(I))

                                Put #1, numberGames% + 1

                                Close 1

                            End If 'Done checking schedule action

                        End If 'Done checking schedule YN$

                    Next

                End If 'Done checking for changes

            Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

        End If 'Done checking for ESC

    End If 'Action 2 / 3

    '752 Y = 0

End Sub

'----------------------------------------
'       EditParameters SubRoutine
'----------------------------------------
'...explanation...
Sub EditParameters ()

    Dim E%(13)

    'EDIT THE PARAMETERS TO SAVE A SPECIAL EVENTS BOXSCORE
    If _FileExists("EVENTS.BOX") Then
        Open "EVENTS.BOX" For Input As #1
        For X = 1 To 13
            Input #1, E%(X)
        Next
        Close 1
    End If

    Cls
    Color 14
    Print "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES"

    X% = 2: Color 15

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

    197 C1 = 1

    Do

        Locate 23, 1: Print "SELECT # TO CHANGE 1-13 (0 TO CONTINUE) ";
        Input B9

        Select Case B9

            Case 1: Locate 2: Input " 1. IF INNINGS ARE OVER "; E%(1) ': If C1 = 1 Then 197
            Case 2: Locate 3: Input " 2. A TEAM'S RUNS LESS THAN "; E%(2) ': If C1 = 1 Then 197
            Case 3: Locate 4: Input " 3. A TEAM'S RUNS MORE THAN "; E%(3) ': If C1 = 1 Then 197
            Case 4: Locate 5: Input " 4. A TEAM'S HITS LESS THAN "; E%(4) ': If C1 = 1 Then 197
            Case 5: Locate 6: Input " 5. A TEAM'S HITS MORE THAN "; E%(5) ': If C1 = 1 Then 197
            Case 6: Locate 7: Input " 6. BATTER'S HITS MORE THAN "; E%(6) ': If C1 = 1 Then 197
            Case 7: Locate 8: Input " 7. BATTER'S RUNS MORE THAN "; E%(7) ': If C1 = 1 Then 197
            Case 8: Locate 9: Input " 8. BATTER'S RBI'S MORE THAN "; E%(8) ': If C1 = 1 Then 197
            Case 9: Locate 10: Input " 9. BATTER'S 2B'S MORE THAN "; E%(9) ': If C1 = 1 Then 197
            Case 10: Locate 11: Input "10. BATTER'S 3B'S MORE THAN "; E%(10) ': If C1 = 1 Then 197
            Case 11: Locate 12: Input "11. BATTER'S HR'S MORE THAN "; E%(11) ': If C1 = 1 Then 197
            Case 12: Locate 13: Input "12. BATTER'S SB'S MORE THAN "; E%(12) ': If C1 = 1 Then 197
            Case 13: Locate 14: Input "13. PITCHER'S K'S MORE THAN "; E%(13) ': If C1 = 1 Then 197

        End Select

    Loop Until B9 = 0 'ENTER also counts as 0

    Open "EVENTS.BOX" For Output As #1
    For X = 1 To 13
        Print #1, E%(X)
    Next
    Close 1

    'Call BoxscoreMaintenance

End Sub

'----------------------------------------
'    BoxscoreMaintenance SubRoutine
'----------------------------------------
'...explanation...
Sub BoxscoreMaintenance ()

    ReDim menuChoices$(1 To 6)

    'Erase menuChoices$
    menuChoices$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
    menuChoices$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
    menuChoices$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
    menuChoices$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
    menuChoices$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
    menuChoices$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "
    Selection% = 1
    Count% = 6
    Start% = 1
    ExitCode% = 6
    FileFGColr% = 15: FileBGColr% = 1
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 6
    Column% = 16

    Do

        Call TITLE_SCHEDULER

        Call box0(5, 15, 12, 65, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case Selection%
                Case 1:
                    Call EditParameters
                Case 2:
                    Spec$ = diskPaths$(1) + "*.SPC"
                    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
                    Count% = DirCount%
                    If Count% = 0 Then
                        Locate 10, 8
                        Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                    Else
                        ReDim boxFiles$(0 To Count%)
                        For X = 1 To Count%
                            boxFiles$(X) = Space$(12)
                        Next
                        boxFiles$(0) = Spec$
                        Call ReadFile(boxFiles$())

                        Do
                            Cls
                            Color , 1
                            Cls
                            Locate , 28
                            Color 14
                            Print "SELECT BOXSCORE TO VIEW": Color 15
                            Selection% = 1
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
                            MAMenu boxFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                            _MouseHide

                            If ExitCode% <> 27 Then

                                Color 15, 1
                                targetFile$ = boxFiles$(Selection%)
                                Locate 2, 33: Print targetFile$

                                Do
                                    Locate , 33: Print "ANY CHANGE (YN)";
                                    I$ = GetKeyPress$
                                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                                Color 15, 0

                                If UCase$(I$) = "N" Then
                                    boxName$ = diskPaths$(1) + targetFile$
                                    'ReDim array$(500) 'holds the main editor string array
                                End If

                            End If

                        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

                    End If 'Done checking for file count

                Case 3
                    Spec$ = diskPaths$(1) + "BOX*."
                    Call DeleteSchedules(Selection%, Spec$)
                    '-- we should give the user a confirmation message
                Case 4
                    Spec$ = diskPaths$(1) + "*.SPC"
                    Call DeleteSchedules(Selection%, Spec$)
                    '-- we should give the user a confirmation message
                Case 5:
                    Spec$ = diskPaths$(1) + "*.SPC"
                    Call DeleteSchedules(Selection%, Spec$)
                    Spec$ = diskPaths$(1) + "BOX*."
                    Call DeleteSchedules(Selection%, Spec$)
            End Select

        End If 'Done checking for ESC

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub

'----------------------------------------
'       SaveScheduleFile SubRoutine
'----------------------------------------
'...explanation...
Sub SaveScheduleFile (saveFile$)

    Shared scheduleAP%(), homeScore%(), visitingScore%()
    Shared NG%()
    Shared scheduleYN$()

    'If _FileExists(diskPaths$(0) + saveFile$ + ".SCD") Then Kill diskPaths$(0) + saveFile$ + ".SCD"
    If _FileExists(diskPaths$(0) + saveFile$) Then Kill diskPaths$(0) + saveFile$

    'Open diskPaths$(0) + saveFile$ + ".SCD" For Random As #1 Len = 48
    Open diskPaths$(0) + saveFile$ For Random As #1 Len = 48

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

    For I = 1 To scheduleAP%(0)
        For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
        For X = 0 To 1: LSet Q$(20 + X) = scheduleYN$(I, X): Next
        LSet Q$(22) = MKI$(visitingScore%(I)): LSet Q$(23) = MKI$(homeScore%(I))
        Put #1, I
    Next

    Close 1

End Sub

'----------------------------------------
'       SortSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub SortSchedule ()

    Dim DY(MAX_GAMES) As Long
    Dim D As Long, M As Long, Y As Long

    Shared scheduleAP%(), homeScore%(), visitingScore%()
    Shared NG%()
    Shared scheduleYN$()

    ' *** SORT SCHEDULE ***
    For I = 1 To scheduleAP%(0)
        HR% = Int(NG%(I, 15) / 100)
        MIN% = NG%(I, 15) - HR% * 100
        TM% = HR% * 60 + MIN%
        NG%(I, 12) = Int(TM% / 30 - 18)
        D = (NG%(I, 18) - 1): D = D * 365
        M = (NG%(I, 17) - 1): M = M * 31
        Y = NG%(I, 16)
        DY(I) = D + M + Y
    Next

    For Z = 1 To scheduleAP%(0) - 1
        For Z1 = Z + 1 To scheduleAP%(0)
            If NG%(Z, 12) > NG%(Z1, 12) Then
                For I = 0 To 18: Swap NG%(Z, I), NG%(Z1, I): Next
                For I = 0 To 1: Swap scheduleYN$(Z, I), scheduleYN$(Z1, I): Next
                Swap DY(Z), DY(Z1)
                Swap visitingScore%(Z), visitingScore%(Z1)
                Swap homeScore%(Z), homeScore%(Z1)
            End If
        Next Z1
    Next Z

    For Z = 1 To scheduleAP%(0) - 1
        For Z1 = Z + 1 To scheduleAP%(0)
            If DY(Z) > DY(Z1) Then
                For I = 0 To 18: Swap NG%(Z, I), NG%(Z1, I): Next
                For I = 0 To 1: Swap scheduleYN$(Z, I), scheduleYN$(Z1, I): Next
                Swap DY(Z), DY(Z1)
                Swap visitingScore%(Z), visitingScore%(Z1)
                Swap homeScore%(Z), homeScore%(Z1)
            End If
        Next Z1
    Next Z

End Sub

'----------------------------------------
'    ChangeGlobalSettings SubRoutine
'----------------------------------------
'...explanation...
Sub ChangeGlobalSettings ()

    'For schedule options
    Dim TS$(1), U5$(4), PU$(48)
    Dim displayPause!

    Shared scheduleAP%(), NG%()

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

    ' GLOBAL SETTINGS FOR A SCHEDULE
    Do
        Color 15, 0: Cls: Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"

        Locate 3, 2: Color 14: Print "(1) ";: Color 15: Print "DESIGNATED HITTER: "; TS$(DH%)
        Locate 4, 2: Color 14: Print "(2) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; displayPause! ' / 10
        Locate 5, 2: Color 14: Print "(3) ";: Color 15: Print "USE WEATHER EFFECTS: "; TS$(1 - WE%)
        Locate 6, 2: Color 14: Print "(4) ";: Color 15: Print "USE INJURIES "; TS$(1 - IJ%)
        Locate 7, 2: Color 14: Print "(5) ";: Color 15: Print "USE LEFT/RIGHT SPLIT STATS "; TS$(1 - LR%)
        Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print "USE DAYS REST DATA FOR PITCHERS "; TS$(1 - DR%)
        Locate 9, 2: Color 14: Print "(7) ";: Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE "; TS$(1 - MP%)
        Locate 10, 2: Color 14: Print "(8) ";: Color 15: Print "USE VGA SCREENS, IF ABLE: "; TS$(1 - VG%)
        Locate 11, 2: Color 14: Print "(9) ";: Color 15: Print "PITCHER ERA: "; PU$(PE%)
        Locate 12, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT GLOBAL SETTINGS"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) = "A" Then I$ = "10"
            I = Val(I$)
        Loop Until I >= 0 And I <= 10

        Select Case I
            Case 1: DH% = 1 - DH%
            Case 2: Input "DISPLAY VARIABLE "; displayPause! ': displayPause! = displayPause! * 10
            Case 3: WE% = 1 - WE%
            Case 4: IJ% = 1 - IJ%
            Case 5: LR% = 1 - LR%
            Case 6: DR% = 1 - DR%
            Case 7: MP% = 1 - MP%
            Case 8: VG% = 1 - VG%
            Case 9:
                PE% = PE% + 1
                If PE% > 8 Then PE% = 0
            Case 9: '

        End Select

    Loop Until I = 0

    For I = 1 To scheduleAP%(0)
        '    NG%(I, 8) = U5
        NG%(I, 3) = DH%
        NG%(I, 4) = displayPause! * 10
        NG%(I, 5) = WE%
        NG%(I, 6) = IJ%
        NG%(I, 7) = LR%
        NG%(I, 9) = DR%
        NG%(I, 10) = MP%
        NG%(I, 11) = VG%
        NG%(I, 14) = PE%
    Next

End Sub

'----------------------------------------
'       LoadSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub LoadSchedule (targetFile$)

    Shared BS%, NS%

    Shared scheduleAP%(), homeScore%(), visitingScore%()
    Shared NG%()
    Shared scheduleYN$()

    ' *** LOAD IN A SAVED SCHEDULE ***
    Spec$ = diskPaths$(0) + "*.SCD"

    Do
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO SCHEDULE FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim scheduleFiles$(0 To Count%)
            For X = 1 To Count%
                scheduleFiles$(X) = Space$(12)
            Next

            scheduleFiles$(0) = Spec$

            Call ReadFile(scheduleFiles$())

            Call SelectScheduleFile(targetFile$, scheduleFiles$(), Count%)

        End If

        If targetFile$ <> "" Then

            'Open diskPaths$(0) + targetFile$ + ".SCD" For Random As #1 Len = 48
            Open diskPaths$(0) + targetFile$ For Random As #1 Len = 48
            Y = LOF(1)

            scheduleAP%(0) = Y / 48

            BS% = Int((scheduleAP%(0) - 1) / 20)

            For X = 0 To 18
                Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
            Next

            Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

            For I = 1 To scheduleAP%(0)
                Get #1, I

                For X = 0 To 18
                    NG%(I, X) = CVI(Q$(X + 1))
                Next

                If NG%(I, 14) > 8 Then NG%(I, 14) = 0

                For X = 0 To 1
                    scheduleYN$(I, X) = RTrim$(Q$(20 + X))
                Next

                visitingScore%(I) = CVI(Q$(22))
                homeScore%(I) = CVI(Q$(23))

                If NG%(I, 6) > 1 Then NG%(I, 6) = 1
                If NG%(I, 7) > 1 Then NG%(I, 7) = 1
                If NG%(I, 9) > 1 Then NG%(I, 9) = 1
                If NG%(I, 10) > 1 Then NG%(I, 10) = 1
                If NG%(I, 11) > 1 Then NG%(I, 11) = 1
            Next

            Close 1
        Else
            'Force a loop exit as someone tried to ESC
            Count% = 0
        End If 'Done checking for ESC

    Loop Until Count% = 0 Or targetFile$ <> ""

End Sub

'----------------------------------------
'     SelectTeam_Schedule SubRoutine
'----------------------------------------
'...explanation...
Sub SelectTeam_Schedule (numberTeams, yearNumber$)

    numberTeams = 0

    If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
        Cls
        Print "ERROR:: UNABLE TO READ TEAM FILE"
        Print
        BO% = 1
    Else

        Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

        Y = LOF(1)

        Field #1, 15 As Q$(0)

        For I1 = 1 To Y / 4342

            Get #1, I1

            For I2 = 1 To 15
                'Skip team entries with junk names / terminate conditions
                If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
            Next I2

            If nullTeam <> 1 Then
                numberTeams = numberTeams + 1
                teamNames$(numberTeams) = Q$(0)
                teamIndex%(numberTeams) = I1
            End If
        Next I1

        Close 1

    End If

End Sub

'----------------------------------------
'       GetTeam_Schedule SubRoutine
'----------------------------------------
'...explanation...
Sub GetTeam_Schedule (numberTeams, yearNumber$, teamIdx)

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then
        teamIdx = teamIndex%(Selection%)
    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'       CheckTeam_Schedule SubRoutine
'----------------------------------------
'...explanation...
Sub CheckTeam_Schedule (teamIdx, yearNumber$)

    Shared scheduleL%()

    Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #4 Len = 4342

    Field #4, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

    Get #4, teamIdx

    For I1 = 1 To 13: scheduleL%(I1) = CVI(Q$(I1)): Next

    Close 4

End Sub

'----------------------------------------
'       DeleteSchedules SubRoutine
'----------------------------------------
'...explanation...
Sub DeleteSchedules (fileChoice%, fileSpec$)

    null$ = READDIR$(fileSpec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%

    If Count% = 0 Then

        Select Case fileChoice%
            Case 3:
                Locate 10, 8
                Call CSMsgBox("NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
            Case 4:
                Locate 10, 8
                Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
            Case 5:
                ' Exit Sub
        End Select

    Else

        ReDim boxFiles$(0 To Count%)

        For X = 1 To Count%
            boxFiles$(X) = Space$(12)
        Next

        boxFiles$(0) = fileSpec$

        Call ReadFile(boxFiles$())

        For X = 1 To Count%
            Kill (diskPaths$(1) + boxFiles$(X))
            Locate 12, 34: Print "DELETING "; boxFiles$(X)
        Next

    End If

End Sub

'----------------------------------------
'       EditBoxscore SubRoutine
'----------------------------------------
'...explanation...
Sub EditBoxscore (boxName$)

    F2$ = Chr$(0) + Chr$(60) 'extended code for the F2 function key
    F3$ = Chr$(0) + Chr$(61) 'extended code for the F3 function key

    If Not _FileExists(boxName$) Then
        Locate 10, 8
        Call CSMsgBox("BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU", 23, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else

        Open boxName$ For Input As #1
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
            If Ky$ = F2$ Then: For I = 1 To CURLINE - 1: LPrint array$(I): Next
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

    End If

End Sub


'----------------------------------------
'     SelectScheduleFile SubRoutine
'----------------------------------------
'...explanation...
Sub SelectScheduleFile (targetFile$, scheduleFiles$(), Count%)

    Do
        Cls
        Color , 1
        Cls
        Locate , 28
        Color 14
        Print "SELECT SCHEDULE TO EDIT": Color 15
        Selection% = 1
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
        Print "ESC";: Color 15
        Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
        Print "ENTER";: Color 15
        Print "-SELECTS SCHEDULE";

        _MouseShow "DEFAULT"
        MAMenu scheduleFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Color 15, 1
            targetFile$ = scheduleFiles$(Selection%)
            Locate 2, 33: Print targetFile$

            Do
                Locate , 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

        End If

    Loop Until ExitCode% = 27 Or UCase$(I$) = "N"

End Sub
