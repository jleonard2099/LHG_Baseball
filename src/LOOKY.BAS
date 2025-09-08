'----------------------------------------
'            LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "Full Count Baseball - View Stats"

    ReDim menuChoices$(1 To 3)

    menuExit = 0

    Do

        Color , 0
        Cls

        Call TITLE

        menuChoices$(1) = "REVIEW COMPILED STATS FOR ONE TEAM     "
        menuChoices$(2) = "PRINT REPORTS FOR ALL TEAMS IN A LEAGUE"
        menuChoices$(3) = "RETURN TO MAIN MENU                    "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "Default"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%
                Case 1: Call StatsOneTeam
                Case 2: Call ReportsAllTeams
                Case 3: Cls
            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       StatsOneTeam Subroutine
'----------------------------------------
'...explanation...
Sub StatsOneTeam ()

    Shared teamRatings()

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

    If teamNbr <> 0 Then

        Color 15, 0

        statFile$ = nameStatFile$(team$)

        If Not _FileExists(diskPaths$(1) + statFile$) Then

            MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call LoadTeamStats(statFile$, nbrGames)
            Call ViewAndPrintOptions(statFile$, team$, nbrGames, menuExit)

        End If

    End If

End Sub

'----------------------------------------
'        LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, nbrGames)

    Shared statS0(), teamScore(), oppScore(), nbrInnings(), locIndicator$()

    Shared extraInnWins, extraInnLosses
    Shared homeWins, homeLosses
    Shared lastTenLosses, lastTenWins, oneRunWins, oneRunLosses
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared teamLosses, teamWins

    totalWins = 0
    totalLosses = 0
    roadWins = 0
    roadLosses = 0
    oneRunWins = 0
    oneRunLosses = 0
    homeWins = 0
    homeLosses = 0
    extraInnWins = 0
    extraInnLosses = 0
    lastTenLosses = 0
    lastTenWins = 0

    Call ReadTeamStats(team$, statFile$)

    If Not _FileExists(diskPaths$(1) + statFile$ + "R") Then

        SS = 0

    Else

        SS = 1

        Open diskPaths$(1) + statFile$ + "R" For Input As #1

        For I = 0 To 22
            For I1 = 0 To 6
                Input #1, statS0(I, I1)
            Next I1

        Next I

        Close #1

    End If

    Call ReadScheduleStats(statFile$, nbrGames)

    For I = 1 To nbrGames

        If Abs(teamScore(I) - oppScore(I)) = 1 And teamScore(I) > oppScore(I) Then
            oneRunWins = oneRunWins + 1
        End If
        If Abs(teamScore(I) - oppScore(I)) = 1 And teamScore(I) < oppScore(I) Then
            oneRunLosses = oneRunLosses + 1
        End If

        If teamScore(I) > oppScore(I) Then
            totalWins = totalWins + 1
        End If
        If teamScore(I) > oppScore(I) And nbrInnings(I) > 9 Then
            extraInnWins = extraInnWins + 1
        End If
        If teamScore(I) > oppScore(I) And locIndicator$(I) = "A" Then
            roadWins = roadWins + 1
        End If
        If teamScore(I) > oppScore(I) And locIndicator$(I) = "H" Then
            homeWins = homeWins + 1
        End If

        If oppScore(I) > teamScore(I) Then
            totalLosses = totalLosses + 1
        End If
        If oppScore(I) > teamScore(I) And nbrInnings(I) > 9 Then
            extraInnLosses = extraInnLosses + 1
        End If
        If oppScore(I) > teamScore(I) And locIndicator$(I) = "A" Then
            roadLosses = roadLosses + 1
        End If
        If oppScore(I) > teamScore(I) And locIndicator$(I) = "H" Then
            homeLosses = homeLosses + 1
        End If

    Next I

    If nbrGames <= 10 Then

        lastTenLosses = totalLosses
        lastTenWins = totalWins

    Else

        M = nbrGames - 9

        For J = M To nbrGames
            If teamScore(J) > oppScore(J) Then
                lastTenWins = lastTenWins + 1
            End If
            If oppScore(J) > teamScore(J) Then
                lastTenLosses = lastTenLosses + 1
            End If
        Next J

    End If

End Sub

'----------------------------------------
'     ViewAndPrintOptions Subroutine
'----------------------------------------
'This subroutine gives the user available
'options for what parts of the stat summary
'to print / view
Sub ViewAndPrintOptions (statFile$, teamName$, nbrGames, menuExit)

    Shared teamScore(), nbrInnings(), teamRatings(), oppScore()

    Shared oppName$(), losePitcher$(), locIndicator$(), winPitcher$()

    ReDim menuChoices$(1 To 4)

    Do

        Color , 0
        Cls

        Call TITLE

        menuChoices$(1) = "VIEW SCHEDULE AND STATS "
        menuChoices$(2) = "VIEW STATS ONLY         "
        menuChoices$(3) = "PRINT SCHEDULE AND STATS"
        menuChoices$(4) = "PRINT STATS ONLY        "

        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "Default"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1

                    Color 12, 0
                    Cls

                    header$ = "SCHEDULE RESULTS FOR " + teamName$
                    Xcoord = Len(header$)
                    Xcoord = Int((80 - Xcoord) / 2)
                    Locate 2, Xcoord
                    Print "SCHEDULE RESULTS FOR ";
                    Color teamRatings(11), teamRatings(12)
                    Print teamName$

                    Locate 4
                    Color 14, 0: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
                    Color 15

                    For I = 1 To nbrGames

                        Print Str$(teamScore(I)); Tab(6); Left$(oppName$(I), 15); Tab(22); Str$(oppScore(I)); Tab(27); "("; locIndicator$(I); ")"; Tab(33); winPitcher$(I); Tab(50); losePitcher$(I); Tab(66); nbrInnings(I)

                        If (I Mod (LAST_ROW - 7) = 0) Then

                            Locate LAST_ROW - 1, 27: Print "PRESS ANY KEY TO CONTINUE";
                            I$ = GetKeyPress$

                            Color 12
                            Cls

                            Locate 2, Xcoord
                            Print "SCHEDULE RESULTS FOR ";
                            Color teamRatings(11), teamRatings(12)
                            Print teamName$

                            Locate 4
                            Color 14, 0: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
                            Color 15

                        End If

                    Next

                    Locate LAST_ROW - 1, 27: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$

                Case 2:
                    '

                Case 3, 4
                    userKey$ = GetPrintOptions$
                    If userKey$ <> Chr$(27) Then

                        Select Case userKey$

                            Case "F"
                                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                If userFile$ <> "" Then
                                    Call PrintScheduleStatSummary(0, userFile$, statFile$, teamName$, nbrGames, SELECTION%, splitStatChoice)
                                End If

                            Case "P"
                                Call PrintScheduleStatSummary(1, "REPORT", statFile$, teamName$, nbrGames, SELECTION%, splitStatChoice)

                        End Select

                    End If

            End Select

            If SELECTION% <> 3 And SELECTION% <> 4 Then

                Color 15, 0
                Cls

                Call ViewScheduleStatSummary(statFile$, teamName$, splitStatChoice)

                Color 15, 0

                Cls

                JJ = 1
                Locate 6, 8

                Call MenuBox(6, 8, 61, 1, teamRatings(11), teamRatings(12))

                Do
                    Locate 7
                    U$ = "DISPLAY STATS AGAIN? (Y/N)"
                    Locate , (80 - Len(U$)) / 2
                    Print U$
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

            End If

        Else

            menuExit% = 1

        End If

    Loop Until SELECTION% = 5 Or ExitCode% = 27

    'Call ResetStatPrompt (statFile$, teamRatings())

    Cls

End Sub


'----------------------------------------
'   ViewScheduleStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewScheduleStatSummary (statFile$, teamName$, splitStatChoice)

    Shared extraInnWins, extraInnLosses
    Shared homeWins, homeLosses
    Shared lastTenLosses, lastTenWins, oneRunWins, oneRunLosses
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared teamLosses, teamWins

    Shared teamRatings()

    Shared batterNames$(), pitcherNames$(), statS0()
    Shared statT0!(), statT1!()

    Shared statB(), statB0()
    Shared teamScore(), nbrInnings(), teamRatings(), oppScore()
    Shared statP(), statP0()

    Shared oppName$(), losePitcher$(), locIndicator$(), winPitcher$()

    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate 1, Xcoord

    Color teamRatings(11), teamRatings(12)
    Print teamName$
    Print

    Color 15, 0
    Locate , 26
    Color 14: Print "OVERALL RECORD ";
    Color 15: Print "("; totalWins; "-"; totalLosses; ")"
    Locate , 26
    Color 14: Print "HOME RECORD    ";
    Color 15: Print "("; homeWins; "-"; homeLosses; ")"
    Locate , 26
    Color 14: Print "ROAD RECORD    ";
    Color 15: Print "("; roadWins; "-"; roadLosses; ")"
    Locate , 26
    Color 14: Print "LAST 10 GAMES  ";
    Color 15: Print "("; lastTenWins; " -"; lastTenLosses; ")"
    Locate , 26
    Color 14: Print "EXTRA INNINGS  ";
    Color 15: Print "("; extraInnWins; " -"; extraInnLosses; ")"
    Locate , 26
    Color 14: Print "ONE RUN GAMES  ";
    Color 15: Print "("; oneRunWins; " -"; oneRunLosses; ")"

    I$ = GetKeyPress$
    Cls

    'Call TITLE_STATS

    'Locate 2, 1
    'Print Tab(80); " ";

    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses
    Locate 5
    Color 15, 1
    Print "BATTERS      G   AB  R   H  2B 3B HR RBI  SB  BB  SO   BA    OBP   SLG CS %RAB "

    Color 15, 0

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7

            If (I Mod 2) = 0 Then Color 15

            'Name
            Print Left$(batterNames$(I), 10);

            'Games
            Locate , 13: Print Using "###"; statB0(I, 14);
            'At Bats
            Locate , 17: Print Using "###"; statB0(I, 0);
            'Runs
            Locate , 21: Print Using "###"; statB0(I, 2);
            'Hits
            Locate , 25: Print Using "###"; statB0(I, 1);
            '2B
            Locate , 29: Print Using "##"; statB0(I, 4);
            '3B
            Locate , 32: Print Using "##"; statB0(I, 5);
            'HR
            Locate , 35: Print Using "##"; statB0(I, 6);
            'RBI
            Locate , 38: Print Using "###"; statB0(I, 3);
            'SB
            Locate , 42: Print Using "###"; statB0(I, 9);
            'BB
            Locate , 46: Print Using "###"; statB0(I, 7);
            'SO
            Locate , 50: Print Using "###"; statB0(I, 8);

            I2 = 0
            I3 = 0
            I4 = 0

            'BA
            Locate , 54

            If statB0(I, 0) = 0 Then
                Print " .--- ";
            Else
                If statB0(I, 0) = statB0(I, 1) Then
                    Print "1.000 ";
                Else
                    If statB0(I, 1) = 0 Then
                        Print " .000 ";
                    Else
                        statCalc! = statB0(I, 1) / statB0(I, 0)
                        Print Using " .### "; statCalc!;
                    End If
                End If
            End If

            I3 = statB0(I, 1) + statB0(I, 4) + 2 * statB0(I, 5) + 3 * statB0(I, 6)

            'SLG
            Locate , 60

            If statB0(I, 0) = 0 Then
                Print " .--- ";
            Else
                If I3 = 0 Then
                    Print " .000 ";
                Else
                    If statB0(I, 0) = I3 Then
                        Print "1.000 ";
                    Else
                        statCalc! = (I3 / statB0(I, 0))
                        If statCalc! >= 1 Then
                            Print Using "#.### "; statCalc!;
                        Else
                            Print Using " .### "; statCalc!;
                        End If
                    End If
                End If
            End If

            I3 = statB0(I, 0) + statB0(I, 7)
            I4 = statB0(I, 1) + statB0(I, 7)

            'OBP
            Locate , 66
            If I3 = 0 Then
                Print " .---";
            Else
                If I4 = 0 Then
                    Print " .000";
                Else
                    If I4 = I3 Then
                        Print "1.000";
                    Else
                        statCalc! = (I4 / I3)
                        Print Using " .###"; statCalc!;
                    End If
                End If
            End If

            'CS
            Locate , 72: Print Using "##"; statB0(I, 10);

            'Runs / At-Bat
            If statB(I) > 0 Then
                Locate , 76
                MM = statB0(I, 0) / statB(I) * 100
                MM = Int(MM)
                Print Str$(MM);
            Else
                If statB(I) = 0 Then Locate , 73: Print "--";
            End If

            If statT0!(0) = 0 Then statT0!(0) = 1

        End If 'Done checking for XXX / terminal chars

    Next I

    Locate 2, 1
    I$ = GetKeyPress$

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses
    Locate 5

    Color 15, 1
    Print "BATTERS      G  CHS LHS GIDP NEEDS PROJ AB  PO   A  E  FAVG SBA CS %CS  SH SF"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            'Name
            Print Left$(batterNames$(I), 10);

            'Games
            Locate , 12: Print Using "###"; statB0(I, 14);
            'Chances
            Locate , 18: Print Using "##"; statB0(I, 12);
            'Left-Hand
            Locate , 22: Print Using "##"; statB0(I, 13);
            'Ground-into-DP
            Locate , 27: Print Using "##"; statB0(I, 15);

            RQ = Val(Right$(statFile$, 2))
            RT = 60
            If RQ > RT Then
                '# games afer 1960
                QT = 162
            Else
                '# games before 1960
                QT = 154
            End If

            'NEEDS
            statCalc = (CInt((((statB(I) + 0) / QT) * (statT1!(9) + 1))) - (statB0(I, 0) + 0))

            If (statB(I) > 0) And statCalc > 0 Then
                Locate , 31: Print Using "####"; statCalc;
            Else
                If statB(I) = 0 Then
                    Print "  ";
                End If
                If statB(I) > 0 Then
                    Locate , 31: Print Using "####"; statCalc;
                End If
            End If

            'PROJ AB
            If statB(I) > 0 Then
                Locate , 37
                statCalc = CInt((((statB(I) + 0) / QT) * (statT1!(9) + 1)))
                Print Using "####"; statCalc;
                statT0!(13) = statT0!(13) + statCalc
            End If

            If statB(I) = 0 Then
                Locate , 39: Print "0";
            End If

            Locate , 42

            'PutOuts, Assists, Errors
            Print Using " #### ### ##"; statB0(I, 16); statB0(I, 17); statB0(I, 11);

            'Fielding AVG
            If statB0(I, 16) + statB0(I, 17) + statB0(I, 11) <> 0 Then
                If statB0(I, 11) <> 0 Then
                    'Putouts + Assists / (Putouts + Assists + Errors)
                    statCalc! = (statB0(I, 16) + statB0(I, 17)) / (statB0(I, 16) + statB0(I, 17) + statB0(I, 11))
                    Print Using "  .###"; statCalc!;
                Else
                    'No errors
                    Print " 1.000";
                End If
            End If

            statCalc = statB0(I, 21) / (statB0(I, 20) + statB0(I, 21)) * 100

            'Stolen Bases Allowed, Caught Steal, Caught Steal %
            If statB0(I, 21) + statB0(I, 20) <> 0 Then
                Print Using " ### ## ###"; statB0(I, 20) + statB0(I, 21); statB0(I, 21); statCalc;
            End If

            'Sac. Hits, Sac. Flies
            Locate , 73: Print Using "## ##"; statB0(I, 18); statB0(I, 19);

        End If

    Next I

    I$ = GetKeyPress$

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses
    Locate 5

    Color 15, 1
    Print "PITCHERS     G   AB   R   H 2B 3B HR RBI  SB  BB  SO   BA    OBP   SLG CS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            'Name
            Locate I + 6, 1: Print Left$(pitcherNames$(I), 10);
            'Games
            Locate , 13: Print Using "###"; statP0(I, 8);
            'At-bats
            Locate , 17: Print Using "###"; statP0(I, 23);
            'Runs
            Locate , 21: Print Using "###"; statP0(I, 25);
            'Hits
            Locate , 25: Print Using "###"; statP0(I, 24);
            '2B
            Locate , 29: Print Using "##"; statP0(I, 27);
            '3B
            Locate , 32: Print Using "##"; statP0(I, 28);
            'HR
            Locate , 35: Print Using "##"; statP0(I, 29);
            'RBI
            Locate , 38: Print Using "###"; statP0(I, 26);
            'SB
            Locate , 42: Print Using "###"; statP0(I, 32);
            'BB
            Locate , 46: Print Using "###"; statP0(I, 30);
            'SO
            Locate , 50: Print Using "###"; statP0(I, 31);

            'Bat AVG
            Locate , 54

            If statP0(I, 23) = 0 Then
                Print " .--- ";
            Else
                If statP0(I, 23) = statP0(I, 24) Then
                    Print "1.000 ";
                Else
                    If statP0(I, 24) = 0 Then
                        Print " .000 ";
                    Else
                        statCalc = statP0(I, 24) / statP0(I, 23)
                        Print Using " .### "; statCalc;
                    End If
                End If
            End If

            'SLG
            Locate , 60

            I3 = statP0(I, 24) + statP0(I, 27) + 2 * statP0(I, 28) + 3 * statP0(I, 29)

            If statP0(I, 23) = 0 Then
                Print " .--- ";
            Else
                If I3 = 0 Then
                    Print " .000 ";
                Else
                    If statP0(I, 23) = I3 Then
                        Print "1.000 ";
                    Else
                        statCalc = I3 / statP0(I, 23)
                        If statCalc >= 1 Then
                            Print Using "#.### "; statCalc;
                        Else
                            Print Using " .### "; statCalc;
                        End If
                    End If
                End If
            End If

            'OBP
            Locate , 66

            I3 = statP0(I, 30) + statP0(I, 23)
            I4 = statP0(I, 24) + statP0(I, 30)

            If I3 = 0 Then
                Print " .---";
            Else
                If I4 = 0 Then
                    Print " .000";
                Else
                    If I4 = I3 Then
                        Print "1.000";
                    Else
                        statCalc = I4 / I3
                        Print Using " .###"; statCalc;
                    End If
                End If
            End If

            'CS
            Locate , 72: Print Using "##"; statP0(I, 33);

            If statT0!(0) = 0 Then
                statT0!(0) = 1
            End If

        End If

    Next I

    I$ = GetKeyPress$

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses
    Locate 5

    Color 15, 1
    Print "PITCHERS     G   CHS LHS GIDP  PO   A  E   FAVG  SH SF"

    Color 15

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            'Name
            Print Left$(pitcherNames$(I), 10);
            'Games
            Locate , 13: Print Using "###"; statP0(I, 8);
            'Chances
            Locate , 19: Print Using "##"; statP0(I, 35);
            'Left-Hand
            Locate , 23: Print Using "##"; statP0(I, 36);
            'Ground-into-DP
            Locate , 27: Print Using "##"; statP0(I, 37);
            'PutOuts, Assists, Errors
            Locate , 29
            Print Using " #### ### ## "; statP0(I, 38); statP0(I, 39); statP0(I, 34);

            'Fielding AVG
            If statP0(I, 38) + statP0(I, 39) + statP0(I, 34) <> 0 Then
                If statP0(I, 34) = 0 Then
                    Print " 1.000";
                Else
                    statCalc! = (statP0(I, 38) + statP0(I, 39)) / (statP0(I, 38) + statP0(I, 39) + statP0(I, 34))
                    Print Using "  .###"; statCalc!;
                End If
            End If

            'SH, SF
            Locate , 50: Print Using "## ##"; statP0(I, 40); statP0(I, 41);

        End If
    Next I

    I$ = GetKeyPress$

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses

    Locate 5
    Color 15, 0
    Print "  TEAM BATTING TOTALS"
    Color 15, 1
    Print
    Print "  AB   R    H   2B  3B  HR  RBI  SB  BB   SO   CS  BA   OBP  SLG"

    Color 14, 0

    'At Bats, Runs, Hits, Doubles, Triples, HR, RBI
    Print Using "#### #### #### ### ### ### ####"; statT0!(0); statT0!(2); statT0!(1); statT0!(4); statT0!(5); statT0!(6); statT0!(3);

    'Stolen Base, Walks, Strikeouts, Caught Stealing
    Locate , 33: Print Using "### #### #### ###"; statT0!(9); statT0!(7); statT0!(8); statT0!(10);

    'Bat Average
    statCalc = Int((statT0!(1) / statT0!(0) + .0005) * 1000)
    Locate , 51: Print ".";
    Print Using "###"; statCalc;

    'On-Base %
    statCalc = Int(((statT0!(1) + statT0!(7)) / (statT0!(7) + statT0!(0)) + .0005) * 1000)
    Locate , 56: Print ".";
    Print Using "###"; statCalc;

    'Slug %
    statCalc = Int((((statT0!(1) + statT0!(4) + 2 * statT0!(5) + 3 * statT0!(6)) / statT0!(0)) + .0005) * 1000)
    Locate , 61: Print ".";
    Print Using "###"; statCalc;

    Color 15, 1
    Locate 10, 1

    'Ground-into-DP, Putouts, Assists, Errors, Fldg. Avg, Sac. Hits, Sac. Flies, Left on Base
    Print "GIDP  PO    A    E  FAVG  SH  SF  LOB"

    Color 14, 0

    'Ground-into-DP, PutOuts, Assists, Errors
    Print Using "#### ####  #### ###"; statT0!(15); statT0!(16); statT0!(17); statT0!(11);
    Locate , 20: If statT0!(11) = 0 Then Print "1.000";

    'Fielding AVG
    If statT0!(11) <> 0 Then
        statCalc! = (statT0!(16) + statT0!(17)) / (statT0!(16) + statT0!(17) + statT0!(11))
        Print Using " .###"; statCalc!;
    End If

    'SH, SF, LOB
    Print Using " ### ### ####"; statT0!(18); statT0!(19); statT0!(12)

    I$ = GetKeyPress$

    If splitStatChoice <> 0 Then

        ' DISPLAY SPLIT STATS
        ' -- this probably won't look right
        Cls

        Color teamRatings(11), teamRatings(12)
        Print teamName$;

        Locate 1, 15
        Color 14, 0
        '...Description...
        Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"

        Color 15, 0
        Locate 2

        For I = 0 To 22

            If batterNames$(I) <> "XXX" And Left$(batterNames$(I), 1) <> " " And batterNames$(I) <> "" Then

                Print batterNames$(I);

                'At Bats, Doubles, Triples, HR, Walks, Strikeout
                Locate , 20: Print Using "### ## ## ## ### ### "; statS0(I, 0); statS0(I, 2); statS0(I, 3); statS0(I, 4); statS0(I, 5); statS0(I, 6);

                'Bat Avg
                If statS0(I, 0) = 0 And statS0(I, 1) = 0 Then
                    Print " .000";
                Else
                    If statS0(I, 0) = statS0(I, 1) Then
                        Print "1.000";
                    Else
                        Print Using "#.###"; statS0(I, 1) / statS0(I, 0);
                    End If
                End If

                'At Bats, Doubles, Triples, HR, Walks, Strikeout
                Locate , 53: Print Using "### ## ## ## ### ### "; statB0(I, 0) - statS0(I, 0); statB0(I, 4) - statS0(I, 2); statB0(I, 5) - statS0(I, 3); statB0(I, 6) - statS0(I, 4); statB0(I, 7) - statS0(I, 5); statB0(I, 8) - statS0(I, 6);

                'Bat Avg
                If statB0(I, 0) - statS0(I, 0) = 0 Or statB0(I, 1) - statS0(I, 1) = 0 Then
                    Print " .000";
                Else
                    If statB0(I, 0) - statS0(I, 0) = statB0(I, 1) - statS0(I, 1) Then
                        Print "1.000";
                    Else
                        Print Using "#.###"; (statB0(I, 1) - statS0(I, 1)) / (statB0(I, 0) - statS0(I, 0));
                    End If
                End If
                If I <> 22 Then Print

            End If
        Next I

        I$ = GetKeyPress$

    End If

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses
    Locate 5

    Color 15, 4
    Print "PITCHERS    W  L   ERA  G GS CG SHO SV   IP     H   R  ER HRA  BB   SO  DP %IP"

    Color 15

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            Print Left$(pitcherNames$(I), 10);

            'Wins
            Locate , 12: Print Using "##"; statP0(I, 6);
            'Losses
            Locate , 15: Print Using "##"; statP0(I, 7);
            'ERA
            Locate , 18
            If statP0(I, 0) = 0 Then
                Print " -.--";
            Else
                If statP0(I, 3) = 0 Then
                    Print " 0.00";
                Else
                    test1! = (statP0(I, 3) * 9)
                    test2! = (statP0(I, 0) / 3)
                    statCalc! = (statP0(I, 3) * 9) / (statP0(I, 0) / 3)
                    Print Using "##.##"; statCalc!;
                End If
            End If

            'Games, Games Started, Complete Game
            Locate , 24: Print Using "## ## ##"; statP0(I, 8); statP0(I, 9); statP0(I, 10);
            'Shutout
            Locate , 34: Print Using "##"; statP0(I, 11);
            'Saves
            Locate , 37: Print Using "##"; statP0(I, 12);
            'Inn Pitched
            Locate , 40: Print Using "###"; Int(statP0(I, 0) / 3);
            Print ".";: Print Using "#"; statP0(I, 0) - Int(statP0(I, 0) / 3) * 3;
            'Hits
            Locate , 46: Print Using "####"; statP0(I, 1);
            'Runs, Earned Run, HR Allowed
            Locate , 51: Print Using "### ### ###"; statP0(I, 2); statP0(I, 3); statP0(I, 14);
            'Walks
            Locate , 63: Print Using "###"; statP0(I, 4);
            'Strikeout
            Locate , 67: Print Using "####"; statP0(I, 5);
            'Double Play
            Locate , 72: Print Using "###"; statP0(I, 13);

            '%IP
            If statP(I) > 0 Then
                statCalc = CInt((((statP0(I, 0) / 3) * 100) / statP(I)))
                Locate , 76: Print Using "###"; statCalc;
            End If
            If statP(I) = 0 Then Print "  ";

        End If

    Next I

    I$ = GetKeyPress$

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses
    Locate 5

    Color 15, 4
    Print "PITCHERS   BS HL  RI  RS   SUP  GS  QS  SH SF"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            Print Left$(pitcherNames$(I), 10);

            'Blown Save, Hold, Runs Inherited, Runs Scored
            Locate , 12
            Print Using "## ## ### ### "; statP0(I, 19); statP0(I, 22); statP0(I, 17); statP0(I, 18);

            If statP0(I, 9) > 0 Then
                'Run Support, Games Started, Quality Start
                statCalc = statP0(I, 20) / statP0(I, 9)
                Print Using "##.##  ##  ##"; statCalc; statP0(I, 9); statP0(I, 21);
            Else
                Print " -.--  --  --";
            End If

            'Sacifice Bunt, Sacrifice Fly
            Print Using "  ## ##"; statP0(I, 15); statP0(I, 16);

        End If

    Next I

    I$ = GetKeyPress$

    Cls

    Locate 2, Xcoord
    Color teamRatings(11), teamRatings(12)
    Print teamName$

    Locate 3, 31
    Color 12, 0
    Print "WON: "; teamWins; " LOST: "; teamLosses

    Locate 5
    Color 15, 0
    Print "  TEAM PITCHING TOTALS"
    Color 15, 4
    Print
    Print "  W   L  SV   IP      H   BB   SO   ERA  G   GS  CG  RUN   ER SHO HRA  DP"

    Color 14, 0

    'W, L, SV
    Print Using "### ### ### "; teamWins; teamLosses; statT1!(12);

    'IP
    Print Using "####"; Int(statT1!(0) / 3);
    Print ".";: Print Using "#"; statT1!(0) - Int(statT1!(0) / 3) * 3;

    'Hits, Walks, Strikeouts, ERA, Games, Games Started, Completed Games
    statCalc! = statT1!(3) * 9 / (statT1!(0) / 3)
    Print Using " #### #### #### ##.## ### ### ### "; statT1!(1); statT1!(4); statT1!(5); statCalc!; statT1!(9); statT1!(9); statT1!(10);

    'Runs, Earned Runs, Shutouts, HR Allowed, DP
    Print Using "#### #### ### ### ###"; statT1!(2); statT1!(3); statT1!(11); statT1!(14); statT1!(13)

    Color 15, 4
    Locate 10, 1

    'Blown Save, RI, Runs Saved, SUP, Quality Start, Sacrifice Bunt, Sacrifice Fly
    Print " BS   RI   RS  SUP    QS  SH  SF"

    Color 14, 0

    statCalc = statT0!(2) / statT1!(9)
    'BS, RI, RS, SUP, QS, SH, SF
    Print Using "### #### #### ##.##  ### ### ###"; statT1!(19); statT1!(17); statT1!(18); statCalc; statT1!(21); statT1!(15); statT1!(16)

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       ReportsAllTeams Subroutine
'----------------------------------------
'This subroutine generates a stat report
'for each team in the chosen league.
Sub ReportsAllTeams ()

    Shared memberName$()

    Shared teamLosses, teamWins

    leagueFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.TMS", "League Stats files [.TMS]")

    If leagueFile$ <> "" Then

        Call ReadLeagueStats(leagueFile$)

        For XW = 1 To TEAMS_PER_LEAGUE

            If _FileExists(diskPaths$(1) + memberName$(XW)) Then

                Call ReadTeamStats(teamName$, memberName$(XW))

                userKey$ = GetPrintOptions$

                If userKey$ <> Chr$(27) Then

                    Select Case userKey$

                        Case "F"
                            userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                            If userFile$ <> "" Then
                                Call NewPrintStats(0, userFile$, memberName$(XW), teamName$, nbrGames, 3, splitStatChoice)
                            End If

                        Case "P"
                            Call NewPrintStats(1, "REPORT", memberName$(XW), teamName$, nbrGames, 3, splitStatChoice)

                    End Select

                End If

            End If

        Next

    End If 'Done checking for ESC


End Sub


'----------------------------------------
'   PrintScheduleStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule results and stats to the
'default printer
Sub PrintScheduleStatSummary (printDest, destFile$, statFile$, teamName$, nbrGames, printChoice, splitStatChoice)

    Shared teamLosses, teamWins

    Shared extraInnWins, extraInnLosses
    Shared homeWins, homeLosses
    Shared lastTenLosses, lastTenWins, oneRunWins, oneRunLosses
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared batterNames$(), pitcherNames$()
    Shared statT0!(), statT1!()
    Shared statB(), statB0()
    Shared statP(), statP0()

    Shared oppName$(), losePitcher$()
    Shared locIndicator$(), winPitcher$()
    Shared teamScore(), nbrInnings()
    Shared statS0(), oppScore()

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    header$ = "SCHEDULE RESULTS FOR " + teamName$
    Xcoord = Len(header$)
    Xcoord = Int((80 - Xcoord) / 2)
    Print
    Print #1, Tab(Xcoord); "SCHEDULE RESULTS FOR "; teamName$

    Print #1,
    Print #1, Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"

    For I = 1 To nbrGames

        Print #1, Str$(teamScore(I)); Tab(6); Left$(oppName$(I), 15); Tab(22); Str$(oppScore(I)); Tab(27); "("; locIndicator$(I); ")"; Tab(33); winPitcher$(I); Tab(50); losePitcher$(I); Tab(66); nbrInnings(I)

        If (I Mod (LAST_ROW - 7) = 0) Then

            Print
            Print #1, Chr$(12)

            Print #1, Tab(Xcoord); "SCHEDULE RESULTS FOR "; teamName$
            Print #1, Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"

        End If

    Next

    Print #1, Tab(26);
    Print #1, "OVERALL RECORD ";
    Print #1, "("; totalWins; "-"; totalLosses; ")"
    Print #1, Tab(26);
    Print #1, "HOME RECORD    ";
    Print #1, "("; homeWins; "-"; homeLosses; ")"
    Print #1, Tab(26);
    Print #1, "ROAD RECORD    ";
    Print #1, "("; roadWins; "-"; roadLosses; ")"
    Print #1, Tab(26);
    Print #1, "LAST 10 GAMES  ";
    Print #1, "("; lastTenWins; " -"; lastTenLosses; ")"
    Print #1, Tab(26);
    Print #1, "EXTRA INNINGS  ";
    Print #1, "("; extraInnWins; " -"; extraInnLosses; ")"
    Print #1, Tab(26);
    Print #1, "ONE RUN GAMES  ";
    Print #1, "("; oneRunWins; " -"; oneRunLosses; ")"

    Print
    Print #1, Chr$(12)

    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "BATTERS      G   AB  R   H  2B 3B HR RBI  SB  BB  SO   BA    OBP   SLG CS %RAB "

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            'Name
            Print #1, Left$(batterNames$(I), 10);

            'Games
            Print #1, Tab(13);: Print #1, Using "###"; statB0(I, 14);
            'At Bats
            Print #1, Tab(17);: Print #1, Using "###"; statB0(I, 0);
            'Runs
            Print #1, Tab(21);: Print #1, Using "###"; statB0(I, 2);
            'Hits
            Print #1, Tab(25);: Print #1, Using "###"; statB0(I, 1);
            '2B
            Print #1, Tab(29);: Print #1, Using "##"; statB0(I, 4);
            '3B
            Print #1, Tab(32);: Print #1, Using "##"; statB0(I, 5);
            'HR
            Print #1, Tab(35);: Print #1, Using "##"; statB0(I, 6);
            'RBI
            Print #1, Tab(38);: Print #1, Using "###"; statB0(I, 3);
            'SB
            Print #1, Tab(42);: Print #1, Using "###"; statB0(I, 9);
            'BB
            Print #1, Tab(46);: Print #1, Using "###"; statB0(I, 7);
            'SO
            Print #1, Tab(50);: Print #1, Using "###"; statB0(I, 8);

            I2 = 0
            I3 = 0
            I4 = 0

            'BA
            Print #1, Tab(54);

            If statB0(I, 0) = 0 Then
                Print #1, " .--- ";
            Else
                If statB0(I, 0) = statB0(I, 1) Then
                    Print #1, "1.000 ";
                Else
                    If statB0(I, 1) = 0 Then
                        Print #1, " .000 ";
                    Else
                        statCalc! = statB0(I, 1) / statB0(I, 0)
                        Print #1, Using " .### "; statCalc!;
                    End If
                End If
            End If

            I3 = statB0(I, 1) + statB0(I, 4) + 2 * statB0(I, 5) + 3 * statB0(I, 6)

            'SLG
            Print #1, Tab(60);

            If statB0(I, 0) = 0 Then
                Print #1, " .--- ";
            Else
                If I3 = 0 Then
                    Print #1, " .000 ";
                Else
                    If statB0(I, 0) = I3 Then
                        Print #1, "1.000 ";
                    Else
                        statCalc! = (I3 / statB0(I, 0))
                        If statCalc! >= 1 Then
                            Print #1, Using "#.### "; statCalc!;
                        Else
                            Print #1, Using " .### "; statCalc!;
                        End If
                    End If
                End If
            End If

            I3 = statB0(I, 0) + statB0(I, 7)
            I4 = statB0(I, 1) + statB0(I, 7)

            'OBP
            Print #1, Tab(66);
            If I3 = 0 Then
                Print #1, " .---";
            Else
                If I4 = 0 Then
                    Print #1, " .000";
                Else
                    If I4 = I3 Then
                        Print #1, "1.000";
                    Else
                        statCalc! = (I4 / I3)
                        Print #1, Using " .###"; statCalc!;
                    End If
                End If
            End If

            'CS
            Print #1, Tab(72);: Print #1, Using "##"; statB0(I, 10);

            'Runs / At-Bat
            If statB(I) > 0 Then
                Print #1, Tab(76);
                MM = statB0(I, 0) / statB(I) * 100
                MM = Int(MM)
                Print #1, Str$(MM);
            Else
                If statB(I) = 0 Then Print #1, Tab(73);: Print #1, "--";
            End If

            If statT0!(0) = 0 Then statT0!(0) = 1

        End If 'Done checking for XXX / terminal chars

    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "BATTERS      G  CHS LHS GIDP NEEDS PROJ AB  PO   A  E  FAVG SBA CS %CS  SH SF"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            'Name
            Print #1, Left$(batterNames$(I), 10);

            'Games
            Print #1, Tab(12);: Print #1, Using "###"; statB0(I, 14);
            'Chances
            Print #1, Tab(18);: Print #1, Using "##"; statB0(I, 12);
            'Left-Hand
            Print #1, Tab(22);: Print #1, Using "##"; statB0(I, 13);
            'Ground-into-DP
            Print #1, Tab(27);: Print #1, Using "##"; statB0(I, 15);

            RQ = Val(Right$(statFile$, 2))
            RT = 60
            If RQ > RT Then
                '# games afer 1960
                QT = 162
            Else
                '# games before 1960
                QT = 154
            End If

            'NEEDS
            statCalc = (CInt((((statB(I) + 0) / QT) * (statT1!(9) + 1))) - (statB0(I, 0) + 0))

            If (statB(I) > 0) And statCalc > 0 Then
                Print #1, Tab(31);: Print #1, Using "####"; statCalc;
            Else
                If statB(I) = 0 Then
                    Print #1, "  ";
                End If
                If statB(I) > 0 Then
                    Print #1, Tab(31);: Print #1, Using "####"; statCalc;
                End If
            End If

            'PROJ AB
            If statB(I) > 0 Then
                Print #1, Tab(37);
                statCalc = CInt((((statB(I) + 0) / QT) * (statT1!(9) + 1)))
                Print #1, Using "####"; statCalc;
                statT0!(13) = statT0!(13) + statCalc
            End If

            If statB(I) = 0 Then
                Print #1, Tab(39);: Print #1, "0";
            End If

            Print #1, Tab(42);

            'PutOuts, Assists, Errors
            Print #1, Using " #### ### ##"; statB0(I, 16); statB0(I, 17); statB0(I, 11);

            'Fielding AVG
            If statB0(I, 16) + statB0(I, 17) + statB0(I, 11) <> 0 Then
                If statB0(I, 11) <> 0 Then
                    'Putouts + Assists / (Putouts + Assists + Errors)
                    statCalc! = (statB0(I, 16) + statB0(I, 17)) / (statB0(I, 16) + statB0(I, 17) + statB0(I, 11))
                    Print #1, Using "  .###"; statCalc!;
                Else
                    'No errors
                    Print #1, " 1.000";
                End If
            End If

            statCalc = statB0(I, 21) / (statB0(I, 20) + statB0(I, 21)) * 100

            'Stolen Bases Allowed, Caught Steal, Caught Steal %
            If statB0(I, 21) + statB0(I, 20) <> 0 Then
                Print #1, Using " ### ## ###"; statB0(I, 20) + statB0(I, 21); statB0(I, 21); statCalc;
            End If

            'Sac. Hits, Sac. Flies
            Print #1, Tab(73);: Print #1, Using "## ##"; statB0(I, 18); statB0(I, 19);

        End If

    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "PITCHERS     G   AB   R   H 2B 3B HR RBI  SB  BB  SO   BA    OBP   SLG CS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            'Name
            Locate I + 6, 1: Print #1, Left$(pitcherNames$(I), 10);
            'Games
            Print #1, Tab(13);: Print #1, Using "###"; statP0(I, 8);
            'At-bats
            Print #1, Tab(17);: Print #1, Using "###"; statP0(I, 23);
            'Runs
            Print #1, Tab(21);: Print #1, Using "###"; statP0(I, 25);
            'Hits
            Print #1, Tab(25);: Print #1, Using "###"; statP0(I, 24);
            '2B
            Print #1, Tab(29);: Print #1, Using "##"; statP0(I, 27);
            '3B
            Print #1, Tab(32);: Print #1, Using "##"; statP0(I, 28);
            'HR
            Print #1, Tab(35);: Print #1, Using "##"; statP0(I, 29);
            'RBI
            Print #1, Tab(38);: Print #1, Using "###"; statP0(I, 26);
            'SB
            Print #1, Tab(42);: Print #1, Using "###"; statP0(I, 32);
            'BB
            Print #1, Tab(46);: Print #1, Using "###"; statP0(I, 30);
            'SO
            Print #1, Tab(50);: Print #1, Using "###"; statP0(I, 31);

            'Bat AVG
            Print #1, Tab(54);

            If statP0(I, 23) = 0 Then
                Print #1, " .--- ";
            Else
                If statP0(I, 23) = statP0(I, 24) Then
                    Print #1, "1.000 ";
                Else
                    If statP0(I, 24) = 0 Then
                        Print #1, " .000 ";
                    Else
                        statCalc = statP0(I, 24) / statP0(I, 23)
                        Print #1, Using " .### "; statCalc;
                    End If
                End If
            End If

            'SLG
            Print #1, Tab(60);

            I3 = statP0(I, 24) + statP0(I, 27) + 2 * statP0(I, 28) + 3 * statP0(I, 29)

            If statP0(I, 23) = 0 Then
                Print #1, " .--- ";
            Else
                If I3 = 0 Then
                    Print #1, " .000 ";
                Else
                    If statP0(I, 23) = I3 Then
                        Print #1, "1.000 ";
                    Else
                        statCalc = I3 / statP0(I, 23)
                        If statCalc >= 1 Then
                            Print #1, Using "#.### "; statCalc;
                        Else
                            Print #1, Using " .### "; statCalc;
                        End If
                    End If
                End If
            End If

            'OBP
            Print #1, Tab(66);

            I3 = statP0(I, 30) + statP0(I, 23)
            I4 = statP0(I, 24) + statP0(I, 30)

            If I3 = 0 Then
                Print #1, " .---";
            Else
                If I4 = 0 Then
                    Print #1, " .000";
                Else
                    If I4 = I3 Then
                        Print #1, "1.000";
                    Else
                        statCalc = I4 / I3
                        Print #1, Using " .###"; statCalc;
                    End If
                End If
            End If

            'CS
            Print #1, Tab(72);: Print #1, Using "##"; statP0(I, 33);

            If statT0!(0) = 0 Then
                statT0!(0) = 1
            End If

        End If

    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "PITCHERS     G   CHS LHS GIDP  PO   A  E   FAVG  SH SF"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            'Name
            Print #1, Left$(pitcherNames$(I), 10);
            'Games
            Print #1, Tab(13);: Print #1, Using "###"; statP0(I, 8);
            'Chances
            Print #1, Tab(19);: Print #1, Using "##"; statP0(I, 35);
            'Left-Hand
            Print #1, Tab(23);: Print #1, Using "##"; statP0(I, 36);
            'Ground-into-DP
            Print #1, Tab(27);: Print #1, Using "##"; statP0(I, 37);
            'PutOuts, Assists, Errors
            Print #1, Tab(29);
            Print #1, Using " #### ### ## "; statP0(I, 38); statP0(I, 39); statP0(I, 34);

            'Fielding AVG
            If statP0(I, 38) + statP0(I, 39) + statP0(I, 34) <> 0 Then
                If statP0(I, 34) = 0 Then
                    Print #1, " 1.000";
                Else
                    statCalc! = (statP0(I, 38) + statP0(I, 39)) / (statP0(I, 38) + statP0(I, 39) + statP0(I, 34))
                    Print #1, Using "  .###"; statCalc!;
                End If
            End If

            'SH, SF
            Print #1, Tab(50);: Print #1, Using "## ##"; statP0(I, 40); statP0(I, 41);

        End If
    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1, "  TEAM BATTING TOTALS"
    Print #1,
    Print #1, "  AB   R    H   2B  3B  HR  RBI  SB  BB   SO   CS  BA   OBP  SLG"

    'At Bats, Runs, Hits, Doubles, Triples, HR, RBI
    Print #1, Using "#### #### #### ### ### ### ####"; statT0!(0); statT0!(2); statT0!(1); statT0!(4); statT0!(5); statT0!(6); statT0!(3);

    'Stolen Base, Walks, Strikeouts, Caught Stealing
    Print #1, Tab(33);: Print #1, Using "### #### #### ###"; statT0!(9); statT0!(7); statT0!(8); statT0!(10);

    'Bat Average
    statCalc = Int((statT0!(1) / statT0!(0) + .0005) * 1000)
    Print #1, Tab(51);: Print #1, ".";
    Print #1, Using "###"; statCalc;

    'On-Base %
    statCalc = Int(((statT0!(1) + statT0!(7)) / (statT0!(7) + statT0!(0)) + .0005) * 1000)
    Print #1, Tab(56);: Print #1, ".";
    Print #1, Using "###"; statCalc;

    'Slug %
    statCalc = Int((((statT0!(1) + statT0!(4) + 2 * statT0!(5) + 3 * statT0!(6)) / statT0!(0)) + .0005) * 1000)
    Print #1, Tab(61);: Print #1, ".";
    Print #1, Using "###"; statCalc;

    Print #1,

    'Ground-into-DP, Putouts, Assists, Errors, Fldg. Avg, Sac. Hits, Sac. Flies, Left on Base
    Print #1, "GIDP  PO    A    E  FAVG  SH  SF  LOB"

    'Ground-into-DP, PutOuts, Assists, Errors
    Print #1, Using "#### ####  #### ###"; statT0!(15); statT0!(16); statT0!(17); statT0!(11);
    Print #1, Tab(20);: If statT0!(11) = 0 Then Print #1, "1.000";

    'Fielding AVG
    If statT0!(11) <> 0 Then
        statCalc! = (statT0!(16) + statT0!(17)) / (statT0!(16) + statT0!(17) + statT0!(11))
        Print #1, Using " .###"; statCalc!;
    End If

    'SH, SF, LOB
    Print #1, Using " ### ### ####"; statT0!(18); statT0!(19); statT0!(12)

    If splitStatChoice <> 0 Then

        ' DISPLAY SPLIT STATS
        ' -- this probably won't look right
        Print
        Print #1, Chr$(12)

        Print #1,
        Print #1, Tab(Xcoord); teamName$
        Print #1,
        Print #1,
        Print #1, "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"

        For I = 0 To 22

            If batterNames$(I) <> "XXX" And Left$(batterNames$(I), 1) <> " " And batterNames$(I) <> "" Then

                Print #1, batterNames$(I);

                'At Bats, Doubles, Triples, HR, Walks, Strikeout
                Print #1, Tab(20);: Print #1, Using "### ## ## ## ### ### "; statS0(I, 0); statS0(I, 2); statS0(I, 3); statS0(I, 4); statS0(I, 5); statS0(I, 6);

                'Bat Avg
                If statS0(I, 0) = 0 And statS0(I, 1) = 0 Then
                    Print #1, " .000";
                Else
                    If statS0(I, 0) = statS0(I, 1) Then
                        Print #1, "1.000";
                    Else
                        Print #1, Using "#.###"; statS0(I, 1) / statS0(I, 0);
                    End If
                End If

                'At Bats, Doubles, Triples, HR, Walks, Strikeout
                Print #1, Tab(53);: Print #1, Using "### ## ## ## ### ### "; statB0(I, 0) - statS0(I, 0); statB0(I, 4) - statS0(I, 2); statB0(I, 5) - statS0(I, 3); statB0(I, 6) - statS0(I, 4); statB0(I, 7) - statS0(I, 5); statB0(I, 8) - statS0(I, 6);

                'Bat Avg
                If statB0(I, 0) - statS0(I, 0) = 0 Or statB0(I, 1) - statS0(I, 1) = 0 Then
                    Print #1, " .000";
                Else
                    If statB0(I, 0) - statS0(I, 0) = statB0(I, 1) - statS0(I, 1) Then
                        Print #1, "1.000";
                    Else
                        Print #1, Using "#.###"; (statB0(I, 1) - statS0(I, 1)) / (statB0(I, 0) - statS0(I, 0));
                    End If
                End If
                If I <> 22 Then Print #1,

            End If
        Next I

    End If

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "PITCHERS    W  L   ERA  G GS CG SHO SV   IP     H   R  ER HRA  BB   SO  DP %IP"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Print #1, Left$(pitcherNames$(I), 10);

            'Wins
            Print #1, Tab(12);: Print #1, Using "##"; statP0(I, 6);
            'Losses
            Print #1, Tab(15);: Print #1, Using "##"; statP0(I, 7);
            'ERA
            Print #1, Tab(18);
            If statP0(I, 0) = 0 Then
                Print #1, " -.--";
            Else
                If statP0(I, 3) = 0 Then
                    Print #1, " 0.00";
                Else
                    statCalc! = (statP0(I, 3) * 9) / (statP0(I, 0) / 3)
                    Print #1, Using "##.##"; statCalc!;
                End If
            End If

            'Games, Games Started, Complete Game
            Print #1, Tab(24);: Print #1, Using "## ## ##"; statP0(I, 8); statP0(I, 9); statP0(I, 10);
            'Shutout
            Print #1, Tab(34);: Print #1, Using "##"; statP0(I, 11);
            'Saves
            Print #1, Tab(37);: Print #1, Using "##"; statP0(I, 12);
            'Inn Pitched
            Print #1, Tab(40);: Print #1, Using "###"; Int(statP0(I, 0) / 3);
            Print #1, ".";: Print #1, Using "#"; statP0(I, 0) - Int(statP0(I, 0) / 3) * 3;
            'Hits
            Print #1, Tab(46);: Print #1, Using "####"; statP0(I, 1);
            'Runs, Earned Run, HR Allowed
            Print #1, Tab(51);: Print #1, Using "### ### ###"; statP0(I, 2); statP0(I, 3); statP0(I, 14);
            'Walks
            Print #1, Tab(63);: Print #1, Using "###"; statP0(I, 4);
            'Strikeout
            Print #1, Tab(67);: Print #1, Using "####"; statP0(I, 5);
            'Double Play
            Print #1, Tab(72);: Print #1, Using "###"; statP0(I, 13);

            '%IP
            If statP(I) > 0 Then
                statCalc = CInt((((statP0(I, 0) / 3) * 100) / statP(I)))
                Print #1, Tab(76);: Print #1, Using "###"; statCalc;
            End If
            If statP(I) = 0 Then Print #1, "  ";

        End If

    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "PITCHERS   BS HL  RI  RS   SUP  GS  QS  SH SF"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Print #1, Left$(pitcherNames$(I), 10);: Print #1, Tab(12);

            'Blown Save, Hold, Runs Inherited, Run Support
            Print #1, Using "## ## ### ### "; statP0(I, 19); statP0(I, 22); statP0(I, 17); statP0(I, 18);

            If statP0(I, 9) > 0 Then
                'Support, Games Started, Quality Start
                statCalc = statP0(I, 20) / statP0(I, 9)
                Print #1, Using "##.##  ##  ##"; statCalc; statP0(I, 9); statP0(I, 21);
            Else
                Print #1, " -.--  --  --";
            End If

            'Sacifice Bunt, Sacrifice Fly
            Print #1, Using "  ## ##"; statP0(I, 15); statP0(I, 16);

        End If

    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "  TEAM PITCHING TOTALS"
    Print #1,
    Print #1, "  W   L  SV   IP      H   BB   SO   ERA  G   GS  CG  RUN   ER SHO HRA  DP"

    'W, L, SV
    Print #1, Using "### ### ### "; teamWins; teamLosses; statT1!(12);

    'IP
    Print #1, Using "####"; Int(statT1!(0) / 3);
    Print #1, ".";: Print #1, Using "#"; statT1!(0) - Int(statT1!(0) / 3) * 3;

    'Hits, Walks, Strikeouts, ERA, Games, Games Started, Completed Games
    statCalc = statT1!(3) * 9 / (statT1!(0) / 3)
    Print #1, Using " #### #### #### ##.## ### ### ### "; statT1!(1); statT1!(4); statT1!(5); statCalc; statT1!(9); statT1!(9); statT1!(10);

    'Runs, Earned Runs, Shutouts, HR Allowed, DP
    Print #1, Using "#### #### ### ### ###"; statT1!(2); statT1!(3); statT1!(11); statT1!(14); statT1!(13)

    'Blown Save, RI, Runs Saved, SUP, Quality Start, Sacrifice Bunt, Sacrifice Fly
    Print #1, " BS   RI   RS  SUP    QS  SH  SF"

    statCalc = statT0!(2) / statT1!(9)
    'BS, RI, RS, SUP, QS, SH, SF
    Print #1, Using "### #### #### ##.##  ### ### ###"; statT1!(19); statT1!(17); statT1!(18); statCalc; statT1!(21); statT1!(15); statT1!(16)

    Print
    Print #1, Chr$(12)

    Close #1

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'       NewPrintStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule results and stats to the
'default printer
Sub NewPrintStats (printDest, destFile$, statFile$, teamName$, nbrGames, printChoice, splitStatChoice)

    Shared teamLosses, teamWins

    Shared extraInnWins, extraInnLosses
    Shared homeWins, homeLosses
    Shared lastTenLosses, lastTenWins, oneRunWins, oneRunLosses
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared batterNames$(), pitcherNames$()
    Shared statT0!(), statT1!()
    Shared statB(), statB0()
    Shared statP(), statP0()

    Shared oppName$(), losePitcher$()
    Shared locIndicator$(), winPitcher$()
    Shared teamScore(), nbrInnings()
    Shared statS0(), oppScore()

    RQ = Val(Right$(statFile$, 2))
    RT = 60

    If RQ > RT Then
        '# games afer 1960
        QT = 162
    Else
        '# games before 1960
        QT = 154
    End If

    ' *** NEW PRINT ROUTINE 6-24-90 ***
    If printChoice <> 3 Then

        totalWins = 0
        totalLosses = 0
        oneRunWins = 0
        oneRunLosses = 0
        roadWins = 0
        roadLosses = 0
        homeWins = 0
        homeLosses = 0
        extraInnWins = 0
        extraInnLosses = 0
        lastTenLosses = 0
        lastTenWins = 0

        Cls
        LPrint "        SCHEDULE RESULTS FOR "; teamName$
        LPrint

    Else

        LPrint Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"

        For I = 1 To nbrGames
            LPrint Str$(teamScore(I)); Tab(6); Left$(oppName$(I), 15); Tab(22); Str$(oppScore(I)); Tab(27); "("; locIndicator$(I); ")"; Tab(33); winPitcher$(I); Tab(50); losePitcher$(I); Tab(66); nbrInnings(I)
        Next

        LPrint
        LPrint
        LPrint Tab(20); "OVERALL RECORD ("; Str$(totalWins); " -"; Str$(totalLosses); " )"
        LPrint Tab(20); "HOME RECORD    ("; Str$(homeWins); " -"; Str$(homeLosses); " )"
        LPrint Tab(20); "ROAD RECORD    ("; Str$(roadWins); " -"; Str$(roadLosses); " )"
        LPrint Tab(20); "LAST 10 GAMES  ("; Str$(lastTenWins); " -"; Str$(lastTenLosses); " )"
        LPrint Tab(20); "EXTRA INNINGS  ("; Str$(extraInnWins); " -"; Str$(extraInnLosses); " )"
        LPrint Tab(20); "ONE RUN GAMES  ("; Str$(oneRunWins); " -"; Str$(oneRunLosses); " )"
        LPrint Chr$(12)

    End If

    LPrint teamName$; "    WON: "; teamWins; " LOST: "; teamLosses
    LPrint "BATTERS      G  AB   R   H 2B 3B HR RBI  BB   K  SB CS CHS LHS GIDP  BAVG   SLG   OBP NEED PROJ.AB %RAB   PO   A  E  FAVG SH SF"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            LPrint Left$(batterNames$(I), 10); Tab(12);
            'Games
            LPrint Using "###"; statB0(I, 14);
            'At Bats, Runs, Hits, 2B
            LPrint Tab(16); Using "### ### ### ##"; statB0(I, 0); statB0(I, 2); statB0(I, 1); statB0(I, 4);
            '3B, HR, RBI, BB
            LPrint Tab(31); Using "## ## ### ###"; statB0(I, 5); statB0(I, 6); statB0(I, 3); statB0(I, 7);
            'L, SB, CS
            LPrint Tab(45); Using "### ### ##"; statB0(I, 8); statB0(I, 9); statB0(I, 10);
            'CHS
            LPrint Tab(56);: LPrint Using "##"; statB0(I, 12);
            'LHS
            LPrint Tab(60);: LPrint Using "##"; statB0(I, 13);
            'GIDP
            LPrint Tab(64);: LPrint Using "##"; statB0(I, 15);

            'BAVG
            LPrint Tab(69);

            If statB0(I, 0) = 0 Then
                LPrint "-.---";
            Else
                If statB0(I, 0) = statB0(I, 1) Then
                    LPrint "1.000";
                Else
                    If statB0(I, 1) = 0 Then
                        LPrint " .000";
                    Else
                        LPrint Using " .###"; (statB0(I, 1) / statB0(I, 0));
                    End If
                End If
            End If

            'SLG
            LPrint Tab(75);

            If statB0(I, 0) = 0 Then
                LPrint "-.---";
            Else
                J = statB0(I, 1) + statB0(I, 4) + statB0(I, 5) * 2 + statB0(I, 6) * 3
                If J = statB0(I, 0) Then
                    LPrint "1.000";
                Else
                    If J / statB0(I, 0) > 1 Then
                        LPrint Using "#.###"; (J / statB0(I, 0));
                    Else
                        If statB0(I, 1) = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; (J / statB0(I, 0));
                        End If
                    End If
                End If
            End If

            'OBP
            LPrint Tab(81);

            If statB0(I, 0) = 0 Then
                LPrint "-.---";
            Else
                J = statB0(I, 1) + statB0(I, 7)
                If J = statB0(I, 0) Then
                    LPrint "1.000";
                Else
                    If J / (statB0(I, 7) + statB0(I, 0)) > 1 Then
                        LPrint Using "#.###"; (J / (statB0(I, 7) + statB0(I, 0)));
                    Else
                        If J = 0 Then
                            LPrint " .000";
                        Else
                            statCalc = (J / (statB0(I, 7) + statB0(I, 0)))
                            LPrint Using " .###"; statCalc;
                        End If
                    End If
                End If
            End If

            'Need
            LPrint Tab(87);

            statCalc = (CInt((((statB(I) + 0) / QT) * (statT1!(9) + 1))) - (statB0(I, 0) + 0))
            If statB(I) > 0 And statCalc > 0 Then
                LPrint Using "###"; statCalc;
            Else
                If statB(I) > 0 Then
                    statCalc = CInt(((statB(I) / QT) * (statT1!(9) + 1))) - (statB0(I, 0) + 0)
                    LPrint Using "###"; statCalc;
                End If
            End If

            'Proj AB
            LPrint Tab(95);
            If statB(I) > 0 Then
                statCalc = CInt((((statB(I) + 0) / QT) * (statT1!(9) + 1)))
                LPrint Using "###"; statCalc;
                statT0!(13) = statT0!(13) + statCalc
            End If

            '% RAB
            LPrint Tab(100);
            If statB(I) > 0 Then
                statCalc = ((statB0(I, 0) / statB(I)) * 100)
                LPrint Using "###"; statCalc;
            End If

            'Putouts
            LPrint Tab(105);: LPrint Using "####"; statB0(I, 16);
            'Assists
            LPrint Tab(110);: LPrint Using "###"; statB0(I, 17);
            'Errors
            LPrint Tab(114);: LPrint Using "##"; statB0(I, 11);

            'Fielding AVG
            LPrint Tab(117);

            If statB0(I, 11) + statB0(I, 17) + statB0(I, 16) = 0 Then
                LPrint " .---";
            Else
                If statB0(I, 11) = 0 Then
                    LPrint "1.000";
                Else
                    statCalc = (statB0(I, 16) + statB0(I, 17)) / (statB0(I, 16) + statB0(I, 17) + statB0(I, 11))
                    LPrint Using " .###"; statCalc;
                End If
            End If

            'Sac. Hits, Sac. Flies
            LPrint Tab(123); Using "## ##"; statB0(I, 18); statB0(I, 19)

        End If

    Next

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            LPrint Left$(pitcherNames$(I), 10); Tab(12);
            'Games
            LPrint Using "###"; statP0(I, 8);
            'At Bats, Runs, Hits, 2B
            LPrint Tab(16); Using "### ### ### ##"; statP0(I, 23); statP0(I, 25); statP0(I, 24); statP0(I, 27);
            '3B, HR, RBI, BB
            LPrint Tab(31); Using "## ## ### ###"; statP0(I, 28); statP0(I, 29); statP0(I, 26); statP0(I, 30);
            'L, SB, CS
            LPrint Tab(45); Using "### ### ##"; statP0(I, 31); statP0(I, 32); statP0(I, 33);
            'CHS
            LPrint Tab(56);: LPrint Using "##"; statP0(I, 35);
            'LHS
            LPrint Tab(60);: LPrint Using "##"; statP0(I, 36);
            'GIDP
            LPrint Tab(64);: LPrint Using "##"; statP0(I, 37);

            'BAVG
            LPrint Tab(69);
            If statP0(I, 23) = 0 Then
                LPrint "-.---";
            Else
                If statP0(I, 23) = statP0(I, 24) Then
                    LPrint "1.000";
                Else
                    If statP0(I, 24) = 0 Then
                        LPrint " .000";
                    Else
                        statCalc = (statP0(I, 24) / statP0(I, 23))
                        LPrint Using " .###"; statCalc;
                    End If
                End If
            End If

            'SLG
            LPrint Tab(75);
            If statP0(I, 23) = 0 Then
                LPrint "-.---";
            Else
                J = statP0(I, 24) + statP0(I, 27) + statP0(I, 28) * 2 + statP0(I, 29) * 3
                If J = statP0(I, 23) Then
                    LPrint "1.000";
                Else
                    statCalc = J / statP0(I, 23)
                    If statCalc > 1 Then
                        LPrint Using "#.###"; statCalc;
                    Else
                        If statP0(I, 24) = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; statCalc;
                        End If
                    End If
                End If
            End If

            'OBP
            LPrint Tab(81);
            If statP0(I, 23) = 0 Then
                LPrint "-.---";
            Else
                J = statP0(I, 24) + statP0(I, 30)
                If J = statP0(I, 23) Then
                    LPrint "1.000";
                Else
                    statCalc = (J / (statP0(I, 30) + statP0(I, 23)))
                    If statCalc > 1 Then
                        LPrint Using "#.###"; statCalc;
                    Else
                        If J = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; statCalc;
                        End If
                    End If
                End If
            End If

            'Put Outs
            LPrint Tab(105);: LPrint Using "####"; statP0(I, 38);
            'Assists
            LPrint Tab(110);: LPrint Using "###"; statP0(I, 39);
            'Errors
            LPrint Tab(114);: LPrint Using "##"; statP0(I, 34);

            'Fielding AVG
            LPrint Tab(117);

            If statP0(I, 34) + statP0(I, 38) + statP0(I, 39) = 0 Then
                LPrint " .---";
            Else
                If statP0(I, 34) = 0 Then
                    LPrint "1.000";
                Else
                    statCalc = (statP0(I, 38) + statP0(I, 39)) / (statP0(I, 38) + statP0(I, 39) + statP0(I, 34))
                    LPrint Using " .###"; statCalc;
                End If
            End If

            LPrint Tab(123); Using "## ##"; statP0(I, 40); statP0(I, 41)

        End If

    Next

    LPrint " "
    LPrint "TOTALS";
    LPrint Tab(12);: LPrint Using "###"; nbrGames;
    LPrint Tab(19);: LPrint Using "####"; statT0!(2);
    LPrint Tab(27);: LPrint Using "###"; statT0!(4);
    LPrint Tab(33);: LPrint Using "###"; statT0!(6);
    LPrint Tab(40);: LPrint Using "####"; statT0!(7);
    LPrint Tab(49);: LPrint Using "###"; statT0!(9);
    statCalc! = (statT0!(1) / statT0!(0))
    LPrint Tab(69);: LPrint Using " .###"; statCalc!;
    statCalc! = ((statT0!(1) + statT0!(4) + statT0!(5) * 2 + statT0!(6) * 3) / statT0!(0))
    LPrint Tab(75);: LPrint Using " .###"; statCalc!;
    statCalc! = ((statT0!(1) + statT0!(7)) / (statT0!(7) + statT0!(0)))
    LPrint Tab(81);: LPrint Using " .###"; statCalc!;
    LPrint Tab(105);: LPrint Using "####"; statT0!(16);
    LPrint Tab(113);: LPrint Using "###"; statT0!(11);
    LPrint Tab(122); Using "###"; statT0!(18)
    LPrint Tab(15);: LPrint Using "####"; statT0!(0);
    LPrint Tab(23);: LPrint Using "####"; statT0!(1);
    LPrint Tab(30);: LPrint Using "###"; statT0!(5);
    LPrint Tab(36);: LPrint Using "####"; statT0!(3);
    LPrint Tab(44);: LPrint Using "####"; statT0!(8);
    LPrint Tab(52);: LPrint Using "####"; statT0!(10);
    LPrint Tab(63); Using "###"; statT0!(15);
    LPrint Tab(109);: LPrint Using "####"; statT0!(17);
    LPrint Tab(117);

    If statT0!(16) + statT0!(11) + statT0!(17) = 0 Then
        LPrint " .---";
    Else
        If statT0!(11) = 0 Then
            LPrint "1.000";
        Else
            statCalc! = (statT0!(16) + statT0!(17)) / (statT0!(16) + statT0!(11) + statT0!(17))
            LPrint Using " .###"; statCalc!;
        End If
    End If

    LPrint Tab(125); Using "###"; statT0!(19)
    LPrint
    LPrint

    TT$ = Str$(statT0!(2) / (teamWins + teamLosses) + .05)
    UU$ = Str$(statT1!(2) / (teamWins + teamLosses) + .05)
    LPrint "RUNS PER GAME ";: LPrint Left$(TT$, 4);
    LPrint " OPPONENTS";: LPrint Left$(UU$, 4)
    LPrint
    LPrint
    LPrint Tab(20); "OVERALL RECORD ("; Str$(totalWins); " -"; Str$(totalLosses); " )"
    LPrint Tab(20); "HOME RECORD    ("; Str$(homeWins); " -"; Str$(homeLosses); " )"
    LPrint Tab(20); "ROAD RECORD    ("; Str$(roadWins); " -"; Str$(roadLosses); " )"
    LPrint Tab(20); "LAST 10 GAMES  ("; Str$(lastTenWins); " -"; Str$(lastTenLosses); " )"
    LPrint Tab(20); "EXTRA INNINGS  ("; Str$(extraInnWins); " -"; Str$(extraInnLosses); " )"
    LPrint Tab(20); "ONE RUN GAMES  ("; Str$(oneRunWins); " -"; Str$(oneRunLosses); " )"
    LPrint Chr$(12)

    LPrint teamName$; "    WON: "; teamWins; " LOST: "; teamLosses
    LPrint "PITCHERS    W  L SV  IP     H  BB  SO  ERA    G GS CG RUN  ER SHO HR DP %RIP SH SF  RI RIS BS   SUP QS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            LPrint Left$(pitcherNames$(I), 10);
            LPrint Tab(12);: LPrint Using "##"; statP0(I, 6);
            LPrint Tab(15);: LPrint Using "##"; statP0(I, 7);
            LPrint Tab(18);: LPrint Using "##"; statP0(I, 12);
            statCalc = Int(statP0(I, 0) / 3)
            LPrint Tab(21);: LPrint Using "###"; statCalc;

            If statP0(I, 0) - (Int(statP0(I, 0) / 3) * 3) > 0 Then
                LPrint ".";
                statCalc = (Int(statP0(I, 0) / 3) * 3)
                LPrint Using "#"; statP0(I, 0) - statCalc;
            End If

            LPrint Tab(27);: LPrint Using "###"; statP0(I, 1);
            LPrint Tab(31);: LPrint Using "###"; statP0(I, 4);
            LPrint Tab(35);: LPrint Using "###"; statP0(I, 5);
            I2 = 0

            If statP0(I, 0) > 0 Then
                LPrint Tab(39);
                statCalc! = (statP0(I, 3) * 9 / (statP0(I, 0) / 3))
                LPrint Using "##.##"; statCalc!;
            Else
                '= 0
                LPrint Tab(39); " -.--";
            End If

            LPrint Tab(45);: LPrint Using "###"; statP0(I, 8);
            LPrint Tab(49);: LPrint Using "##"; statP0(I, 9);
            LPrint Tab(52);: LPrint Using "##"; statP0(I, 10);
            LPrint Tab(55);: LPrint Using "###"; statP0(I, 2);
            LPrint Tab(59);: LPrint Using "###"; statP0(I, 3);
            LPrint Tab(63);: LPrint Using "###"; statP0(I, 11);
            LPrint Tab(67);: LPrint Using "##"; statP0(I, 14);
            LPrint Tab(70);: LPrint Using "##"; statP0(I, 13);
            LPrint Tab(74);

            If statP(I) > 0 Then
                statCalc = CInt((((statP0(I, 0) / 3) * 100) / statP(I)))
                LPrint Using "###"; statCalc;
            Else
                '= 0
                LPrint "  ";
            End If

            LPrint Tab(78); Using "## ## ### ### ## "; statP0(I, 15); statP0(I, 16); statP0(I, 17); statP0(I, 18); statP0(I, 19);

            If statP0(I, 9) > 0 Then LPrint Using "##.## "; statP0(I, 20) / statP0(I, 9);

            LPrint Tab(101); Using "##"; statP0(I, 21)

        End If

    Next

    LPrint
    LPrint "TOTALS";
    LPrint Tab(18);: LPrint Using "##"; statT1!(12);
    LPrint Tab(26);: LPrint Using "####"; statT1!(1);
    LPrint Tab(34);: LPrint Using "####"; statT1!(5);
    statCalc! = statT1!(3) / (statT1!(0) / 3) * 9
    LPrint Tab(39);: LPrint Using "##.##"; statCalc!;
    LPrint Tab(52);: LPrint Using "##"; statT1!(10);
    LPrint Tab(58);: LPrint Using "####"; statT1!(3);
    LPrint Tab(63);: LPrint Using "###"; statT1!(11);
    LPrint Tab(69);: LPrint Using "###"; statT1!(13);
    LPrint Tab(77); Using "###"; statT1!(15);
    LPrint Tab(83); Using "####"; statT1!(17);
    LPrint Tab(91); Using "###"; statT1!(19);
    LPrint Tab(100); Using "###"; statT1!(21)
    LPrint Tab(21);: LPrint Using "####"; statT1!(0) / 3;
    LPrint Tab(30);: LPrint Using "####"; statT1!(4);
    LPrint Tab(54);: LPrint Using "####"; statT1!(2);
    LPrint Tab(66);: LPrint Using "###"; statT1!(14);
    LPrint Tab(80); Using "###"; statT1!(16);
    LPrint Tab(87); Using "####"; statT1!(18)
    LPrint
    LPrint
    LPrint
    LPrint "CATCHERS    SBA   CS   CS%"

    For I = 0 To 22
        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" And statB0(I, 21) + statB0(I, 20) <> 0 Then
            statCalc! = statB0(I, 21) / (statB0(I, 20) + statB0(I, 21)) * 100
            LPrint Left$(batterNames$(I), 10); Tab(12); Using "###   ##   ###"; statB0(I, 20) + statB0(I, 21); statB0(I, 21); statCalc!
        End If
    Next

    If splitStatChoice <> 0 Then
        'DISPLAY SPLIT STATS
        LPrint teamName$;
        LPrint Tab(15); "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"

        For I = 0 To 22

            If batterNames$(I) <> "XXX" And Left$(batterNames$(I), 1) <> " " And batterNames$(I) <> "" Then

                LPrint batterNames$(I);
                LPrint Tab(20); Using "### ## ## ## ### ### "; statS0(I, 0); statS0(I, 2); statS0(I, 3); statS0(I, 4); statS0(I, 5); statS0(I, 6);

                If statS0(I, 0) = 0 Or statS0(I, 1) = 0 Then
                    LPrint " .000";
                Else
                    If statS0(I, 0) = statS0(I, 1) Then
                        LPrint "1.000";
                    Else
                        statCalc! = statS0(I, 1) / statS0(I, 0)
                        LPrint Using " .###"; statCalc!;
                    End If
                End If

                LPrint Tab(53); Using "### ## ## ## ### ### "; statB0(I, 0) - statS0(I, 0); statB0(I, 4) - statS0(I, 2); statB0(I, 5) - statS0(I, 3); statB0(I, 6) - statS0(I, 4); statB0(I, 7) - statS0(I, 5); statB0(I, 8) - statS0(I, 6);

                If statB0(I, 0) - statS0(I, 0) = 0 Or statB0(I, 1) - statS0(I, 1) = 0 Then
                    LPrint " .000"
                Else
                    If statB0(I, 0) - statS0(I, 0) = statB0(I, 1) - statS0(I, 1) Then
                        LPrint "1.000"
                    Else
                        statCalc! = (statB0(I, 1) - statS0(I, 1)) / (statB0(I, 0) - statS0(I, 0))
                        LPrint Using " .###"; statCalc!
                    End If
                End If

            End If

        Next I

    End If

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with
'a choice to reset stats. If they chose,
'and confirm, it will then proceed to
'delete the files.
Sub ResetStatPrompt (statFile$, teamRatings())

    Cls

    Call MenuBox(6, 8, 61, 1, teamRatings(11), teamRatings(12))

    Locate 7

    Do
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        Call MenuBox(6, 8, 61, 1, teamRatings(11), teamRatings(12))

        Locate 7

        Do
            U$ = "ARE YOU SURE? (Y/N)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then
            Kill diskPaths$(1) + statFile$
            Kill diskPaths$(1) + statFile$ + "S"
            If _FileExists(diskPaths$(1) + statFile$ + "R") Then Kill diskPaths$(1) + statFile$ + "R"
        End If

        'Loop Until UCase$(I$) = "N"

    End If 'Done checking if reset stats

End Sub
