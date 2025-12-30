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

                Case 1:
                    Call StatsOneTeam

                Case 2:
                    Call ReportsAllTeams

                Case 3: 'Cls

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       StatsOneTeam Subroutine
'----------------------------------------
'Handles loading stats for a team and
'presents user options for reports
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

            Call LoadTeamStats(statFile$, team$, nbrGames)
            Call ViewAndPrintOptions(statFile$, team$, nbrGames, menuExit)

        End If

    End If

End Sub

'----------------------------------------
'        LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, nbrGames)

    Shared statSplit(), teamScore(), oppScore(), nbrInnings(), locIndicator$()

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

    Call ReadTeamStats(teamName$, statFile$)

    If Not _FileExists(diskPaths$(1) + statFile$ + "R") Then

        SS = 0

    Else

        SS = 1

        of% = FreeFile

        Open diskPaths$(1) + statFile$ + "R" For Input As #of%

        For I = 0 To 22
            For I1 = 0 To 6
                Input #of%, statSplit(I, I1)
            Next I1

        Next I

        Close #of%

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

    'Call ResetStatsPrompt (statFile$, teamRatings())

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

    Shared batterNames$(), pitcherNames$(), statSplit()
    Shared statTeamBat!(), statTeamPit!()

    Shared calcProperties()
    Shared teamScore(), nbrInnings(), oppScore()
    Shared statAtBats(), statBatter()
    Shared statInnPitch(), statPitcher()
    Shared teamRatings()

    Shared oppName$(), losePitcher$(), locIndicator$(), winPitcher$()

    Shared batCategoryBatterAbbr$(), batCategoryPitcherAbbr$(), pitCategoryAbbr$()

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
    Print "BATTERS      G   AB  R   H  2B 3B HR RBI  SB  BB  SO   BA    SLG   OBP CS %RAB "

    Color 15, 0

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7

            If (I Mod 2) = 0 Then Color 15

            'Name
            Print Left$(batterNames$(I), 10);

            'Games
            Locate , 13: Print Using "###"; statBatter(I, 14);
            'At Bats
            Locate , 17: Print Using "###"; statBatter(I, 0);
            'Runs
            Locate , 21: Print Using "###"; statBatter(I, 2);
            'Hits
            Locate , 25: Print Using "###"; statBatter(I, 1);
            '2B
            Locate , 29: Print Using "##"; statBatter(I, 4);
            '3B
            Locate , 32: Print Using "##"; statBatter(I, 5);
            'HR
            Locate , 35: Print Using "##"; statBatter(I, 6);
            'RBI
            Locate , 38: Print Using "###"; statBatter(I, 3);
            'SB
            Locate , 42: Print Using "###"; statBatter(I, 9);
            'BB
            Locate , 46: Print Using "###"; statBatter(I, 7);
            'SO
            Locate , 50: Print Using "###"; statBatter(I, 8);

            'Bat Avg
            Locate , 54

            I3 = 0
            I4 = 0

            If statBatter(I, 0) = 0 Then
                Print " .--- ";
            Else
                If statBatter(I, 0) = statBatter(I, 1) Then
                    Print "1.000 ";
                Else
                    If statBatter(I, 1) = 0 Then
                        Print " .000 ";
                    Else
                        statCalc! = statBatter(I, 1) / statBatter(I, 0)
                        Print Using " .### "; statCalc!;
                    End If
                End If
            End If

            'SLG
            Locate , 60

            I3 = statBatter(I, 1) + statBatter(I, 4) + 2 * statBatter(I, 5) + 3 * statBatter(I, 6)

            If statBatter(I, 0) = 0 Then
                Print " .--- ";
            Else
                If I3 = 0 Then
                    Print " .000 ";
                Else
                    If statBatter(I, 0) = I3 Then
                        Print "1.000 ";
                    Else
                        statCalc! = (I3 / statBatter(I, 0))
                        If statCalc! >= 1 Then
                            Print Using "#.### "; statCalc!;
                        Else
                            Print Using " .### "; statCalc!;
                        End If
                    End If
                End If
            End If

            'At Bats + Walks
            I3 = statBatter(I, 0) + statBatter(I, 7)
            'Hits + Walks
            I4 = statBatter(I, 1) + statBatter(I, 7)

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
            Locate , 72: Print Using "##"; statBatter(I, 10);

            'Runs / At-Bat
            If statAtBats(I) > 0 Then
                Locate , 76
                MM = statBatter(I, 0) / statAtBats(I) * 100
                MM = Int(MM)
                Print Str$(MM);
            Else
                If statAtBats(I) = 0 Then Locate , 73: Print "--";
            End If

            If statTeamBat!(0) = 0 Then
                statTeamBat!(0) = 1
            End If

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
            Locate , 12: Print Using "###"; statBatter(I, 14);
            'Curr Hit Streak
            Locate , 18: Print Using "##"; statBatter(I, 12);
            'Long Hit Streak
            Locate , 22: Print Using "##"; statBatter(I, 13);
            'Ground-into-DP
            Locate , 27: Print Using "##"; statBatter(I, 15);

            RQ = Val(Right$(statFile$, 2))
            RT = 60
            If RQ > RT Then
                '# games afer 1960
                QT = 162
            Else
                '# games before 1960
                QT = 154
            End If

            'NEEDS (Expected At-Bats)
            statCalc = (CInt((((statAtBats(I) + 0) / QT) * (statTeamPit!(9) + 1))) - (statBatter(I, 0) + 0))

            If (statAtBats(I) > 0) And statCalc > 0 Then
                Locate , 31: Print Using "####"; statCalc;
            Else
                If statAtBats(I) = 0 Then
                    Print "  ";
                End If
                If statAtBats(I) > 0 Then
                    Locate , 31: Print Using "####"; statCalc;
                End If
            End If

            'Projected At-Bats
            If statAtBats(I) > 0 Then
                Locate , 37
                statCalc = CInt((((statAtBats(I) + 0) / QT) * (statTeamPit!(9) + 1)))
                Print Using "####"; statCalc;
                statTeamBat!(13) = statTeamBat!(13) + statCalc
            End If

            If statAtBats(I) = 0 Then
                Locate , 39: Print "0";
            End If

            Locate , 42

            'PutOuts, Assists, Errors
            Print Using " #### ### ##"; statBatter(I, 16); statBatter(I, 17); statBatter(I, 11);

            'Fielding AVG
            '>> Denominator = Putouts + Assists + Errors
            If statBatter(I, 16) + statBatter(I, 17) + statBatter(I, 11) <> 0 Then
                If statBatter(I, 11) <> 0 Then
                    statCalc! = (statBatter(I, 16) + statBatter(I, 17)) / (statBatter(I, 16) + statBatter(I, 17) + statBatter(I, 11))
                    Print Using "  .###"; statCalc!;
                Else
                    'No errors
                    Print " 1.000";
                End If
            Else
                'No fielding stats
                Print " .---";
            End If

            'Stolen Bases, Caught Steal, Caught Steal %
            statCalc = statBatter(I, 21) / (statBatter(I, 20) + statBatter(I, 21)) * 100

            If statBatter(I, 21) + statBatter(I, 20) <> 0 Then
                Print Using " ### ## ###"; statBatter(I, 20) + statBatter(I, 21); statBatter(I, 21); statCalc;
            End If

            'Sac. Hits, Sac. Flies
            Locate , 73: Print Using "## ##"; statBatter(I, 18); statBatter(I, 19);

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
    Print "PITCHERS     G   AB   R   H 2B 3B HR RBI  SB  BB  SO   BA    SLG   OBP CS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            'Name
            Locate I + 6, 1: Print Left$(pitcherNames$(I), 10);
            'Games
            Locate , 13: Print Using "###"; statPitcher(I, 8);
            'At-bats
            Locate , 17: Print Using "###"; statPitcher(I, 23);
            'Runs
            Locate , 21: Print Using "###"; statPitcher(I, 25);
            'Hits
            Locate , 25: Print Using "###"; statPitcher(I, 24);
            '2B
            Locate , 29: Print Using "##"; statPitcher(I, 27);
            '3B
            Locate , 32: Print Using "##"; statPitcher(I, 28);
            'HR
            Locate , 35: Print Using "##"; statPitcher(I, 29);
            'RBI
            Locate , 38: Print Using "###"; statPitcher(I, 26);
            'SB
            Locate , 42: Print Using "###"; statPitcher(I, 32);
            'BB
            Locate , 46: Print Using "###"; statPitcher(I, 30);
            'SO
            Locate , 50: Print Using "###"; statPitcher(I, 31);

            'Bat AVG
            Locate , 54

            If statPitcher(I, 23) = 0 Then
                Print " .--- ";
            Else
                If statPitcher(I, 23) = statPitcher(I, 24) Then
                    Print "1.000 ";
                Else
                    If statPitcher(I, 24) = 0 Then
                        Print " .000 ";
                    Else
                        statCalc! = statPitcher(I, 24) / statPitcher(I, 23)
                        Print Using " .### "; statCalc!;
                    End If
                End If
            End If

            'SLG
            Locate , 60

            I3 = statPitcher(I, 24) + statPitcher(I, 27) + 2 * statPitcher(I, 28) + 3 * statPitcher(I, 29)

            If statPitcher(I, 23) = 0 Then
                Print " .--- ";
            Else
                If I3 = 0 Then
                    Print " .000 ";
                Else
                    If statPitcher(I, 23) = I3 Then
                        Print "1.000 ";
                    Else
                        statCalc! = I3 / statPitcher(I, 23)
                        If statCalc! >= 1 Then
                            Print Using "#.### "; statCalc!;
                        Else
                            Print Using " .### "; statCalc!;
                        End If
                    End If
                End If
            End If

            'OBP
            Locate , 66

            'At Bats + Walks
            I3 = statPitcher(I, 30) + statPitcher(I, 23)
            'Hits + Walks
            I4 = statPitcher(I, 30) + statPitcher(I, 24)

            If I3 = 0 Then
                Print " .---";
            Else
                If I4 = 0 Then
                    Print " .000";
                Else
                    If I4 = I3 Then
                        Print "1.000";
                    Else
                        statCalc! = I4 / I3
                        Print Using " .###"; statCalc!;
                    End If
                End If
            End If

            'CS
            Locate , 72: Print Using "##"; statPitcher(I, 33);

            If statTeamBat!(0) = 0 Then
                statTeamBat!(0) = 1
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
            Locate , 13: Print Using "###"; statPitcher(I, 8);
            'Chances
            Locate , 19: Print Using "##"; statPitcher(I, 35);
            'Left-Hand
            Locate , 23: Print Using "##"; statPitcher(I, 36);
            'Ground-into-DP
            Locate , 27: Print Using "##"; statPitcher(I, 37);
            'PutOuts, Assists, Errors
            Locate , 29
            Print Using " #### ### ## "; statPitcher(I, 38); statPitcher(I, 39); statPitcher(I, 34);

            'Fielding AVG
            '>> Denominator = Putouts + Assists + Errors
            If statPitcher(I, 38) + statPitcher(I, 39) + statPitcher(I, 34) <> 0 Then
                If statPitcher(I, 34) <> 0 Then
                    statCalc! = (statPitcher(I, 38) + statPitcher(I, 39)) / (statPitcher(I, 38) + statPitcher(I, 39) + statPitcher(I, 34))
                    Print Using "  .###"; statCalc!;
                Else
                    'No errors
                    Print " 1.000";
                End If
            Else
                'No fielding stats
                Print " .---";
            End If

            'SH, SF
            Locate , 50: Print Using "## ##"; statPitcher(I, 40); statPitcher(I, 41);

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
    Print Using "#### #### #### ### ### ### ####"; statTeamBat!(0); statTeamBat!(2); statTeamBat!(1); statTeamBat!(4); statTeamBat!(5); statTeamBat!(6); statTeamBat!(3);

    'Stolen Base, Walks, Strikeouts, Caught Stealing
    Locate , 33: Print Using "### #### #### ###"; statTeamBat!(9); statTeamBat!(7); statTeamBat!(8); statTeamBat!(10);

    'Bat Average
    statCalc = Int((statTeamBat!(1) / statTeamBat!(0) + .0005) * 1000)
    Locate , 51: Print ".";
    Print Using "###"; statCalc;

    'On-Base %
    statCalc = Int(((statTeamBat!(1) + statTeamBat!(7)) / (statTeamBat!(7) + statTeamBat!(0)) + .0005) * 1000)
    Locate , 56: Print ".";
    Print Using "###"; statCalc;

    'Slug %
    statCalc = Int((((statTeamBat!(1) + statTeamBat!(4) + 2 * statTeamBat!(5) + 3 * statTeamBat!(6)) / statTeamBat!(0)) + .0005) * 1000)
    Locate , 61: Print ".";
    Print Using "###"; statCalc;

    Color 15, 1
    Locate 10, 1

    'Ground-into-DP, Putouts, Assists, Errors, Fldg. Avg, Sac. Hits, Sac. Flies, Left on Base
    Print "GIDP  PO    A    E  FAVG  SH  SF  LOB"

    Color 14, 0

    'Ground-into-DP, PutOuts, Assists, Errors
    Print Using "#### ####  #### ###"; statTeamBat!(15); statTeamBat!(16); statTeamBat!(17); statTeamBat!(11);
    Locate , 20: If statTeamBat!(11) = 0 Then Print "1.000";

    'Fielding AVG
    If statTeamBat!(11) <> 0 Then
        statCalc! = (statTeamBat!(16) + statTeamBat!(17)) / (statTeamBat!(16) + statTeamBat!(17) + statTeamBat!(11))
        Print Using " .###"; statCalc!;
    End If

    'SH, SF, LOB
    Print Using " ### ### ####"; statTeamBat!(18); statTeamBat!(19); statTeamBat!(12)

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
                Locate , 20: Print Using "### ## ## ## ### ### "; statSplit(I, 0); statSplit(I, 2); statSplit(I, 3); statSplit(I, 4); statSplit(I, 5); statSplit(I, 6);

                'Bat Avg
                If statSplit(I, 0) = 0 And statSplit(I, 1) = 0 Then
                    Print " .000";
                Else
                    If statSplit(I, 0) = statSplit(I, 1) Then
                        Print "1.000";
                    Else
                        Print Using "#.###"; statSplit(I, 1) / statSplit(I, 0);
                    End If
                End If

                'At Bats, Doubles, Triples, HR, Walks, Strikeout
                Locate , 53: Print Using "### ## ## ## ### ### "; statBatter(I, 0) - statSplit(I, 0); statBatter(I, 4) - statSplit(I, 2); statBatter(I, 5) - statSplit(I, 3); statBatter(I, 6) - statSplit(I, 4); statBatter(I, 7) - statSplit(I, 5); statBatter(I, 8) - statSplit(I, 6);

                'Bat Avg
                If statBatter(I, 0) - statSplit(I, 0) = 0 Or statBatter(I, 1) - statSplit(I, 1) = 0 Then
                    Print " .000";
                Else
                    If statBatter(I, 0) - statSplit(I, 0) = statBatter(I, 1) - statSplit(I, 1) Then
                        Print "1.000";
                    Else
                        Print Using "#.###"; (statBatter(I, 1) - statSplit(I, 1)) / (statBatter(I, 0) - statSplit(I, 0));
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
            Locate , 12: Print Using "##"; statPitcher(I, 6);
            'Losses
            Locate , 15: Print Using "##"; statPitcher(I, 7);
            'ERA
            Locate , 18
            If statPitcher(I, 0) = 0 Then
                Print " -.--";
            Else
                If statPitcher(I, 3) = 0 Then
                    Print " 0.00";
                Else
                    test1! = (statPitcher(I, 3) * 9)
                    test2! = (statPitcher(I, 0) / 3)
                    statCalc! = (statPitcher(I, 3) * 9) / (statPitcher(I, 0) / 3)
                    Print Using "##.##"; statCalc!;
                End If
            End If

            'Games, Games Started, Complete Game
            Locate , 24: Print Using "## ## ##"; statPitcher(I, 8); statPitcher(I, 9); statPitcher(I, 10);
            'Shutout
            Locate , 34: Print Using "##"; statPitcher(I, 11);
            'Saves
            Locate , 37: Print Using "##"; statPitcher(I, 12);
            'Inn Pitched
            Locate , 40: Print Using "###"; Int(statPitcher(I, 0) / 3);
            Print ".";: Print Using "#"; statPitcher(I, 0) - Int(statPitcher(I, 0) / 3) * 3;
            'Hits
            Locate , 46: Print Using "####"; statPitcher(I, 1);
            'Runs, Earned Run, HR Allowed
            Locate , 51: Print Using "### ### ###"; statPitcher(I, 2); statPitcher(I, 3); statPitcher(I, 14);
            'Walks
            Locate , 63: Print Using "###"; statPitcher(I, 4);
            'Strikeout
            Locate , 67: Print Using "####"; statPitcher(I, 5);
            'Double Play
            Locate , 72: Print Using "###"; statPitcher(I, 13);

            '%IP
            If statInnPitch(I) > 0 Then
                statCalc = CInt((((statPitcher(I, 0) / 3) * 100) / statInnPitch(I)))
                Locate , 76: Print Using "###"; statCalc;
            End If
            If statInnPitch(I) = 0 Then Print "  ";

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
    Print "PITCHERS   BS HL  IR IR-A  RPS  GS  QS  SH SF"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 6, 1
            Color 7, 0

            If (I Mod 2) = 0 Then Color 15

            Print Left$(pitcherNames$(I), 10);

            'Blown Save, Hold, Inherited Runners, IR Allowed
            Locate , 12
            Print Using "## ## ### ### "; statPitcher(I, 19); statPitcher(I, 22); statPitcher(I, 17); statPitcher(I, 18);

            If statPitcher(I, 9) > 0 Then
                'Runs / Start, Games Started, Quality Start
                statCalc = statPitcher(I, 20) / statPitcher(I, 9)
                Print Using "##.##  ##  ##"; statCalc; statPitcher(I, 9); statPitcher(I, 21);
            Else
                Print " -.--  --  --";
            End If

            'Sacifice Bunt, Sacrifice Fly
            Print Using "  ## ##"; statPitcher(I, 15); statPitcher(I, 16);

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
    Print Using "### ### ### "; teamWins; teamLosses; statTeamPit!(12);

    'IP
    Print Using "####"; Int(statTeamPit!(0) / 3);
    Print ".";: Print Using "#"; statTeamPit!(0) - Int(statTeamPit!(0) / 3) * 3;

    'Hits, Walks, Strikeouts, ERA, Games, Games Started, Completed Games
    statCalc! = statTeamPit!(3) * 9 / (statTeamPit!(0) / 3)
    Print Using " #### #### #### ##.## ### ### ### "; statTeamPit!(1); statTeamPit!(4); statTeamPit!(5); statCalc!; statTeamPit!(9); statTeamPit!(9); statTeamPit!(10);

    'Runs, Earned Runs, Shutouts, HR Allowed, DP
    Print Using "#### #### ### ### ###"; statTeamPit!(2); statTeamPit!(3); statTeamPit!(11); statTeamPit!(14); statTeamPit!(13)

    Color 15, 4
    Locate 10, 1

    'Blown Save, Inherited Runners, IR Allowed, Runs / Start, Quality Start, Sacrifice Bunt, Sacrifice Fly
    Print " BS   IR IR-A  RPS    QS  SH  SF"

    Color 14, 0

    statCalc = statTeamBat!(2) / statTeamPit!(9)
    'BS, IR, IR-A, Runs / Start, QS, SH, SF
    Print Using "### #### #### ##.##  ### ### ###"; statTeamPit!(19); statTeamPit!(17); statTeamPit!(18); statCalc; statTeamPit!(21); statTeamPit!(15); statTeamPit!(16)

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       ReportsAllTeams Subroutine
'----------------------------------------
'This subroutine generates a stat report
'for each team in the chosen league.
Sub ReportsAllTeams ()

    Shared memberIdx()
    Shared memberName$(), memberYear$()
    Dim memberTeam$(TEAMS_PER_LEAGUE)

    Shared teamLosses, teamWins

    leagueFile$ = _OpenFileDialog$("Choose League Index file", diskPaths$(2), "*.TMI", "League Index files [.TMI]")

    If leagueFile$ <> "" Then

        Call ReadLeagueIndex(leagueFile$)

        userKey$ = GetPrintOptions$

        If userKey$ <> Chr$(27) Then

            If userKey$ = "F" Then
                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
            End If

            If (userKey$ = "P") Or (userKey$ = "F" And userFile$ <> "") Then

                For Z = 1 To TEAMS_PER_LEAGUE
                    memberTeam$(Z) = memberName$(Z)
                Next Z

                'Replace "team names" in memberName$() with stat file names
                Call ReadLeagueMembers(diskPaths$(2) + GetFileBaseName$(leagueFile$) + ".TMS")

                For XW = 1 To TEAMS_PER_LEAGUE

                    If _FileExists(diskPaths$(1) + memberName$(XW)) Then

                        Call LoadTeamStats(memberName$(XW), memberTeam$(XW), nbrGames)

                        Select Case userKey$

                            Case "F"
                                Call NewPrintStats(0, userFile$, memberName$(XW), memberTeam$(XW), nbrGames, 3, splitStatChoice)

                            Case "P"
                                Call NewPrintStats(1, "REPORT", memberName$(XW), memberTeam$(XW), nbrGames, 3, splitStatChoice)

                        End Select

                    End If

                Next

            End If

        End If

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
    Shared statTeamBat!(), statTeamPit!()
    Shared statAtBats(), statBatter()
    Shared statInnPitch(), statPitcher()

    Shared oppName$(), losePitcher$()
    Shared locIndicator$(), winPitcher$()
    Shared teamScore(), nbrInnings()
    Shared statSplit(), oppScore()

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    header$ = "SCHEDULE RESULTS FOR " + teamName$
    Xcoord = Len(header$)
    Xcoord = Int((80 - Xcoord) / 2)
    Print #1,
    Print #1, Tab(Xcoord); header$

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
            Print #1, Tab(13);: Print #1, Using "###"; statBatter(I, 14);
            'At Bats
            Print #1, Tab(17);: Print #1, Using "###"; statBatter(I, 0);
            'Runs
            Print #1, Tab(21);: Print #1, Using "###"; statBatter(I, 2);
            'Hits
            Print #1, Tab(25);: Print #1, Using "###"; statBatter(I, 1);
            '2B
            Print #1, Tab(29);: Print #1, Using "##"; statBatter(I, 4);
            '3B
            Print #1, Tab(32);: Print #1, Using "##"; statBatter(I, 5);
            'HR
            Print #1, Tab(35);: Print #1, Using "##"; statBatter(I, 6);
            'RBI
            Print #1, Tab(38);: Print #1, Using "###"; statBatter(I, 3);
            'SB
            Print #1, Tab(42);: Print #1, Using "###"; statBatter(I, 9);
            'BB
            Print #1, Tab(46);: Print #1, Using "###"; statBatter(I, 7);
            'SO
            Print #1, Tab(50);: Print #1, Using "###"; statBatter(I, 8);

            'Bat Avg
            Print #1, Tab(54);

            I3 = 0
            I4 = 0

            If statBatter(I, 0) = 0 Then
                Print #1, " .--- ";
            Else
                If statBatter(I, 0) = statBatter(I, 1) Then
                    Print #1, "1.000 ";
                Else
                    If statBatter(I, 1) = 0 Then
                        Print #1, " .000 ";
                    Else
                        statCalc! = statBatter(I, 1) / statBatter(I, 0)
                        Print #1, Using " .### "; statCalc!;
                    End If
                End If
            End If

            'SLG
            Print #1, Tab(60);

            I3 = statBatter(I, 1) + statBatter(I, 4) + 2 * statBatter(I, 5) + 3 * statBatter(I, 6)

            If statBatter(I, 0) = 0 Then
                Print #1, " .--- ";
            Else
                If I3 = 0 Then
                    Print #1, " .000 ";
                Else
                    If statBatter(I, 0) = I3 Then
                        Print #1, "1.000 ";
                    Else
                        statCalc! = (I3 / statBatter(I, 0))
                        If statCalc! >= 1 Then
                            Print #1, Using "#.### "; statCalc!;
                        Else
                            Print #1, Using " .### "; statCalc!;
                        End If
                    End If
                End If
            End If

            'OBP
            Print #1, Tab(66);

            'At Bats + Walks
            I3 = statBatter(I, 0) + statBatter(I, 7)
            'Hits + Walks
            I4 = statBatter(I, 1) + statBatter(I, 7)

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
            Print #1, Tab(72);: Print #1, Using "##"; statBatter(I, 10);

            'Runs / At-Bat
            If statAtBats(I) > 0 Then
                Print #1, Tab(76);
                MM = statBatter(I, 0) / statAtBats(I) * 100
                MM = Int(MM)
                Print #1, Str$(MM);
            Else
                If statAtBats(I) = 0 Then Print #1, Tab(73);: Print #1, "--";
            End If

            If statTeamBat!(0) = 0 Then statTeamBat!(0) = 1

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
            Print #1, Tab(12);: Print #1, Using "###"; statBatter(I, 14);
            'Chances
            Print #1, Tab(18);: Print #1, Using "##"; statBatter(I, 12);
            'Left-Hand
            Print #1, Tab(22);: Print #1, Using "##"; statBatter(I, 13);
            'Ground-into-DP
            Print #1, Tab(27);: Print #1, Using "##"; statBatter(I, 15);

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
            statCalc = (CInt((((statAtBats(I) + 0) / QT) * (statTeamPit!(9) + 1))) - (statBatter(I, 0) + 0))

            If (statAtBats(I) > 0) And statCalc > 0 Then
                Print #1, Tab(31);: Print #1, Using "####"; statCalc;
            Else
                If statAtBats(I) = 0 Then
                    Print #1, "  ";
                End If
                If statAtBats(I) > 0 Then
                    Print #1, Tab(31);: Print #1, Using "####"; statCalc;
                End If
            End If

            'PROJ AB
            If statAtBats(I) > 0 Then
                Print #1, Tab(37);
                statCalc = CInt((((statAtBats(I) + 0) / QT) * (statTeamPit!(9) + 1)))
                Print #1, Using "####"; statCalc;
                statTeamBat!(13) = statTeamBat!(13) + statCalc
            End If

            If statAtBats(I) = 0 Then
                Print #1, Tab(39);: Print #1, "0";
            End If

            Print #1, Tab(42);

            'PutOuts, Assists, Errors
            Print #1, Using " #### ### ##"; statBatter(I, 16); statBatter(I, 17); statBatter(I, 11);

            'Fielding AVG
            '>> Denominator = Putouts + Assists + Errors
            If statBatter(I, 16) + statBatter(I, 17) + statBatter(I, 11) <> 0 Then
                If statBatter(I, 11) <> 0 Then
                    statCalc! = (statBatter(I, 16) + statBatter(I, 17)) / (statBatter(I, 16) + statBatter(I, 17) + statBatter(I, 11))
                    Print #1, Using "  .###"; statCalc!;
                Else
                    'No errors
                    Print #1, " 1.000";
                End If
            Else
                'No fielding stats
                Print #1, " .---";
            End If

            statCalc = statBatter(I, 21) / (statBatter(I, 20) + statBatter(I, 21)) * 100

            'Stolen Bases Allowed, Caught Steal, Caught Steal %
            If statBatter(I, 21) + statBatter(I, 20) <> 0 Then
                Print #1, Using " ### ## ###"; statBatter(I, 20) + statBatter(I, 21); statBatter(I, 21); statCalc;
            End If

            'Sac. Hits, Sac. Flies
            Print #1, Tab(73);: Print #1, Using "## ##"; statBatter(I, 18); statBatter(I, 19);

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
            Print #1, Tab(13);: Print #1, Using "###"; statPitcher(I, 8);
            'At-bats
            Print #1, Tab(17);: Print #1, Using "###"; statPitcher(I, 23);
            'Runs
            Print #1, Tab(21);: Print #1, Using "###"; statPitcher(I, 25);
            'Hits
            Print #1, Tab(25);: Print #1, Using "###"; statPitcher(I, 24);
            '2B
            Print #1, Tab(29);: Print #1, Using "##"; statPitcher(I, 27);
            '3B
            Print #1, Tab(32);: Print #1, Using "##"; statPitcher(I, 28);
            'HR
            Print #1, Tab(35);: Print #1, Using "##"; statPitcher(I, 29);
            'RBI
            Print #1, Tab(38);: Print #1, Using "###"; statPitcher(I, 26);
            'SB
            Print #1, Tab(42);: Print #1, Using "###"; statPitcher(I, 32);
            'BB
            Print #1, Tab(46);: Print #1, Using "###"; statPitcher(I, 30);
            'SO
            Print #1, Tab(50);: Print #1, Using "###"; statPitcher(I, 31);

            'Bat AVG
            Print #1, Tab(54);

            If statPitcher(I, 23) = 0 Then
                Print #1, " .--- ";
            Else
                If statPitcher(I, 23) = statPitcher(I, 24) Then
                    Print #1, "1.000 ";
                Else
                    If statPitcher(I, 24) = 0 Then
                        Print #1, " .000 ";
                    Else
                        statCalc = statPitcher(I, 24) / statPitcher(I, 23)
                        Print #1, Using " .### "; statCalc;
                    End If
                End If
            End If

            'SLG
            Print #1, Tab(60);

            I3 = statPitcher(I, 24) + statPitcher(I, 27) + 2 * statPitcher(I, 28) + 3 * statPitcher(I, 29)

            If statPitcher(I, 23) = 0 Then
                Print #1, " .--- ";
            Else
                If I3 = 0 Then
                    Print #1, " .000 ";
                Else
                    If statPitcher(I, 23) = I3 Then
                        Print #1, "1.000 ";
                    Else
                        statCalc = I3 / statPitcher(I, 23)
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

            I3 = statPitcher(I, 30) + statPitcher(I, 23)
            I4 = statPitcher(I, 24) + statPitcher(I, 30)

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
            Print #1, Tab(72);: Print #1, Using "##"; statPitcher(I, 33);

            If statTeamBat!(0) = 0 Then
                statTeamBat!(0) = 1
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
            Print #1, Tab(13);: Print #1, Using "###"; statPitcher(I, 8);
            'Chances
            Print #1, Tab(19);: Print #1, Using "##"; statPitcher(I, 35);
            'Left-Hand
            Print #1, Tab(23);: Print #1, Using "##"; statPitcher(I, 36);
            'Ground-into-DP
            Print #1, Tab(27);: Print #1, Using "##"; statPitcher(I, 37);
            'PutOuts, Assists, Errors
            Print #1, Tab(29);
            Print #1, Using " #### ### ## "; statPitcher(I, 38); statPitcher(I, 39); statPitcher(I, 34);

            'Fielding AVG
            '>> Denominator = Putouts + Assists + Errors
            If statPitcher(I, 38) + statPitcher(I, 39) + statPitcher(I, 34) <> 0 Then
                If statPitcher(I, 34) <> 0 Then
                    statCalc! = (statPitcher(I, 38) + statPitcher(I, 39)) / (statPitcher(I, 38) + statPitcher(I, 39) + statPitcher(I, 34))
                    Print #1, Using "  .###"; statCalc!;
                Else
                    'No errors
                    Print #1, " 1.000";
                End If
            Else
                'No fielding stats
                Print #1, " .---";
            End If

            'SH, SF
            Print #1, Tab(50);: Print #1, Using "## ##"; statPitcher(I, 40); statPitcher(I, 41);

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
    Print #1, Using "#### #### #### ### ### ### ####"; statTeamBat!(0); statTeamBat!(2); statTeamBat!(1); statTeamBat!(4); statTeamBat!(5); statTeamBat!(6); statTeamBat!(3);

    'Stolen Base, Walks, Strikeouts, Caught Stealing
    Print #1, Tab(33);: Print #1, Using "### #### #### ###"; statTeamBat!(9); statTeamBat!(7); statTeamBat!(8); statTeamBat!(10);

    'Bat Average
    statCalc = Int((statTeamBat!(1) / statTeamBat!(0) + .0005) * 1000)
    Print #1, Tab(51);: Print #1, ".";
    Print #1, Using "###"; statCalc;

    'On-Base %
    statCalc = Int(((statTeamBat!(1) + statTeamBat!(7)) / (statTeamBat!(7) + statTeamBat!(0)) + .0005) * 1000)
    Print #1, Tab(56);: Print #1, ".";
    Print #1, Using "###"; statCalc;

    'Slug %
    statCalc = Int((((statTeamBat!(1) + statTeamBat!(4) + 2 * statTeamBat!(5) + 3 * statTeamBat!(6)) / statTeamBat!(0)) + .0005) * 1000)
    Print #1, Tab(61);: Print #1, ".";
    Print #1, Using "###"; statCalc;

    Print #1,

    'Ground-into-DP, Putouts, Assists, Errors, Fldg. Avg, Sac. Hits, Sac. Flies, Left on Base
    Print #1, "GIDP  PO    A    E  FAVG  SH  SF  LOB"

    'Ground-into-DP, PutOuts, Assists, Errors
    Print #1, Using "#### ####  #### ###"; statTeamBat!(15); statTeamBat!(16); statTeamBat!(17); statTeamBat!(11);
    Print #1, Tab(20);: If statTeamBat!(11) = 0 Then Print #1, "1.000";

    'Fielding AVG
    If statTeamBat!(11) <> 0 Then
        statCalc! = (statTeamBat!(16) + statTeamBat!(17)) / (statTeamBat!(16) + statTeamBat!(17) + statTeamBat!(11))
        Print #1, Using " .###"; statCalc!;
    End If

    'SH, SF, LOB
    Print #1, Using " ### ### ####"; statTeamBat!(18); statTeamBat!(19); statTeamBat!(12)

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
                Print #1, Tab(20);: Print #1, Using "### ## ## ## ### ### "; statSplit(I, 0); statSplit(I, 2); statSplit(I, 3); statSplit(I, 4); statSplit(I, 5); statSplit(I, 6);

                'Bat Avg
                If statSplit(I, 0) = 0 And statSplit(I, 1) = 0 Then
                    Print #1, " .000";
                Else
                    If statSplit(I, 0) = statSplit(I, 1) Then
                        Print #1, "1.000";
                    Else
                        Print #1, Using "#.###"; statSplit(I, 1) / statSplit(I, 0);
                    End If
                End If

                'At Bats, Doubles, Triples, HR, Walks, Strikeout
                Print #1, Tab(53);: Print #1, Using "### ## ## ## ### ### "; statBatter(I, 0) - statSplit(I, 0); statBatter(I, 4) - statSplit(I, 2); statBatter(I, 5) - statSplit(I, 3); statBatter(I, 6) - statSplit(I, 4); statBatter(I, 7) - statSplit(I, 5); statBatter(I, 8) - statSplit(I, 6);

                'Bat Avg
                If statBatter(I, 0) - statSplit(I, 0) = 0 Or statBatter(I, 1) - statSplit(I, 1) = 0 Then
                    Print #1, " .000";
                Else
                    If statBatter(I, 0) - statSplit(I, 0) = statBatter(I, 1) - statSplit(I, 1) Then
                        Print #1, "1.000";
                    Else
                        Print #1, Using "#.###"; (statBatter(I, 1) - statSplit(I, 1)) / (statBatter(I, 0) - statSplit(I, 0));
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
            Print #1, Tab(12);: Print #1, Using "##"; statPitcher(I, 6);
            'Losses
            Print #1, Tab(15);: Print #1, Using "##"; statPitcher(I, 7);
            'ERA
            Print #1, Tab(18);
            If statPitcher(I, 0) = 0 Then
                Print #1, " -.--";
            Else
                If statPitcher(I, 3) = 0 Then
                    Print #1, " 0.00";
                Else
                    statCalc! = (statPitcher(I, 3) * 9) / (statPitcher(I, 0) / 3)
                    Print #1, Using "##.##"; statCalc!;
                End If
            End If

            'Games, Games Started, Complete Game
            Print #1, Tab(24);: Print #1, Using "## ## ##"; statPitcher(I, 8); statPitcher(I, 9); statPitcher(I, 10);
            'Shutout
            Print #1, Tab(34);: Print #1, Using "##"; statPitcher(I, 11);
            'Saves
            Print #1, Tab(37);: Print #1, Using "##"; statPitcher(I, 12);
            'Inn Pitched
            Print #1, Tab(40);: Print #1, Using "###"; Int(statPitcher(I, 0) / 3);
            Print #1, ".";: Print #1, Using "#"; statPitcher(I, 0) - Int(statPitcher(I, 0) / 3) * 3;
            'Hits
            Print #1, Tab(46);: Print #1, Using "####"; statPitcher(I, 1);
            'Runs, Earned Run, HR Allowed
            Print #1, Tab(51);: Print #1, Using "### ### ###"; statPitcher(I, 2); statPitcher(I, 3); statPitcher(I, 14);
            'Walks
            Print #1, Tab(63);: Print #1, Using "###"; statPitcher(I, 4);
            'Strikeout
            Print #1, Tab(67);: Print #1, Using "####"; statPitcher(I, 5);
            'Double Play
            Print #1, Tab(72);: Print #1, Using "###"; statPitcher(I, 13);

            '%IP
            If statInnPitch(I) > 0 Then
                statCalc = CInt((((statPitcher(I, 0) / 3) * 100) / statInnPitch(I)))
                Print #1, Tab(76);: Print #1, Using "###"; statCalc;
            End If
            If statInnPitch(I) = 0 Then Print #1, "  ";

        End If

    Next I

    Print
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(Xcoord); teamName$
    Print #1,
    Print #1,
    Print #1, "PITCHERS   BS HL  RI  RS   RPS  GS  QS  SH SF"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Print #1, Left$(pitcherNames$(I), 10);: Print #1, Tab(12);

            'Blown Save, Hold, Runs Inherited, Run Support
            Print #1, Using "## ## ### ### "; statPitcher(I, 19); statPitcher(I, 22); statPitcher(I, 17); statPitcher(I, 18);

            If statPitcher(I, 9) > 0 Then
                'Support, Games Started, Quality Start
                statCalc = statPitcher(I, 20) / statPitcher(I, 9)
                Print #1, Using "##.##  ##  ##"; statCalc; statPitcher(I, 9); statPitcher(I, 21);
            Else
                Print #1, " -.--  --  --";
            End If

            'Sacifice Bunt, Sacrifice Fly
            Print #1, Using "  ## ##"; statPitcher(I, 15); statPitcher(I, 16);

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
    Print #1, Using "### ### ### "; teamWins; teamLosses; statTeamPit!(12);

    'IP
    Print #1, Using "####"; Int(statTeamPit!(0) / 3);
    Print #1, ".";: Print #1, Using "#"; statTeamPit!(0) - Int(statTeamPit!(0) / 3) * 3;

    'Hits, Walks, Strikeouts, ERA, Games, Games Started, Completed Games
    statCalc = statTeamPit!(3) * 9 / (statTeamPit!(0) / 3)
    Print #1, Using " #### #### #### ##.## ### ### ### "; statTeamPit!(1); statTeamPit!(4); statTeamPit!(5); statCalc; statTeamPit!(9); statTeamPit!(9); statTeamPit!(10);

    'Runs, Earned Runs, Shutouts, HR Allowed, DP
    Print #1, Using "#### #### ### ### ###"; statTeamPit!(2); statTeamPit!(3); statTeamPit!(11); statTeamPit!(14); statTeamPit!(13)

    'Blown Save, RI, Runs Saved, SUP, Quality Start, Sacrifice Bunt, Sacrifice Fly
    Print #1, " BS   RI   RS  RPS    QS  SH  SF"

    statCalc = statTeamBat!(2) / statTeamPit!(9)
    'BS, RI, RS, SUP, QS, SH, SF
    Print #1, Using "### #### #### ##.##  ### ### ###"; statTeamPit!(19); statTeamPit!(17); statTeamPit!(18); statCalc; statTeamPit!(21); statTeamPit!(15); statTeamPit!(16)

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
    Shared statTeamBat!(), statTeamPit!()
    Shared statAtBats(), statBatter()
    Shared statInnPitch(), statPitcher()

    Shared oppName$(), losePitcher$()
    Shared locIndicator$(), winPitcher$()
    Shared teamScore(), nbrInnings()
    Shared statSplit(), oppScore()

    of% = FreeFile

    Open destFile$ For Append As #of%

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

        header$ = "SCHEDULE RESULTS FOR " + teamName$
        Xcoord = Len(header$)
        Xcoord = Int((80 - Xcoord) / 2)
        Print #of%,
        Print #of%, Tab(Xcoord); header$

    Else

        header$ = "SCHEDULE RESULTS FOR " + teamName$
        Xcoord = Len(header$)
        Xcoord = Int((80 - Xcoord) / 2)
        Print #of%,
        Print #of%, Tab(Xcoord); header$

        Print #of%,
        Print #of%, Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"

        For I = 1 To nbrGames
            Print #of%, Str$(teamScore(I)); Tab(6); Left$(oppName$(I), 15); Tab(22); Str$(oppScore(I)); Tab(27); "("; locIndicator$(I); ")"; Tab(33); winPitcher$(I); Tab(50); losePitcher$(I); Tab(66); nbrInnings(I)
        Next

        Print #of%,
        Print #of%,
        Print #of%, Tab(20); "OVERALL RECORD ("; Str$(totalWins); " -"; Str$(totalLosses); " )"
        Print #of%, Tab(20); "HOME RECORD    ("; Str$(homeWins); " -"; Str$(homeLosses); " )"
        Print #of%, Tab(20); "ROAD RECORD    ("; Str$(roadWins); " -"; Str$(roadLosses); " )"
        Print #of%, Tab(20); "LAST 10 GAMES  ("; Str$(lastTenWins); " -"; Str$(lastTenLosses); " )"
        Print #of%, Tab(20); "EXTRA INNINGS  ("; Str$(extraInnWins); " -"; Str$(extraInnLosses); " )"
        Print #of%, Tab(20); "ONE RUN GAMES  ("; Str$(oneRunWins); " -"; Str$(oneRunLosses); " )"
        Print #of%, Chr$(12)

    End If

    Print #of%, "BATTERS      G  AB   R   H 2B 3B HR RBI  BB   K  SB CS CHS LHS GIDP  BAVG   SLG   OBP NEED PROJ.AB %RAB   PO   A  E  FAVG SH SF"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            'Name
            Print #of%, Left$(batterNames$(I), 10); Tab(12);
            'Games
            Print #of%, Using "###"; statBatter(I, 14);
            'At Bats, Runs, Hits, 2B
            Print #of%, Tab(16); Using "### ### ### ##"; statBatter(I, 0); statBatter(I, 2); statBatter(I, 1); statBatter(I, 4);
            '3B, HR, RBI, BB
            Print #of%, Tab(31); Using "## ## ### ###"; statBatter(I, 5); statBatter(I, 6); statBatter(I, 3); statBatter(I, 7);
            'L, SB, CS
            Print #of%, Tab(45); Using "### ### ##"; statBatter(I, 8); statBatter(I, 9); statBatter(I, 10);
            'CHS
            Print #of%, Tab(56);: Print #of%, Using "##"; statBatter(I, 12);
            'LHS
            Print #of%, Tab(60);: Print #of%, Using "##"; statBatter(I, 13);
            'GIDP
            Print #of%, Tab(64);: Print #of%, Using "##"; statBatter(I, 15);

            'Bat AVG
            Print #of%, Tab(69);

            If statBatter(I, 0) = 0 Then
                Print #of%, " .---";
            Else
                If statBatter(I, 0) = statBatter(I, 1) Then
                    Print #of%, "1.000";
                Else
                    If statBatter(I, 1) = 0 Then
                        Print #of%, " .000";
                    Else
                        statCalc! = statBatter(I, 1) / statBatter(I, 0)
                        Print #of%, Using " .###"; statCalc!;
                    End If
                End If
            End If

            'SLG
            Print #of%, Tab(75);

            J = statBatter(I, 1) + statBatter(I, 4) + statBatter(I, 5) * 2 + statBatter(I, 6) * 3

            If statBatter(I, 0) = 0 Then
                Print #of%, " .---";
            Else
               If J = 0 Then
                    Print #1, " .000 ";
                Else
                    If statBatter(I, 1) = J Then
                        Print #of%, "1.000";
                    Else
                        statCalc! = (J / statBatter(I, 0))
                        If statCalc! >= 1 Then
                            Print #of%, Using "#.###"; statCalc!;
                        Else
                            Print #of%, Using " .###"; statCalc!;
                        End If
                    End If
                End If
            End If

            'OBP
            Print #of%, Tab(81);

            K = statBatter(I, 0) + statBatter(I, 7)
            J = statBatter(I, 1) + statBatter(I, 7)

            If statBatter(I, 0) = 0 Then
                Print #of%, " .---";
            Else
                If J = statBatter(I, 0) Then
                    Print #of%, " .000";
                Else
                    If J = K Then
                        Print #of%, "1.000";
                    Else
                        statCalc! = (J / K)
                        Print #of%, Using " .###"; statCalc!;
                    End If
                End If
            End If

            'Need
            Print #of%, Tab(87);

            RQ = Val(Right$(statFile$, 2))
            RT = 60

            If RQ > RT Then
                '# games afer 1960
                QT = 162
            Else
                '# games before 1960
                QT = 154
            End If

            statCalc = (CInt((((statAtBats(I) + 0) / QT) * (statTeamPit!(9) + 1))) - (statBatter(I, 0) + 0))

            If statAtBats(I) > 0 And statCalc > 0 Then
                Print #of%, Using "###"; statCalc;
            Else
                If statAtBats(I) > 0 Then
                    statCalc = CInt(((statAtBats(I) / QT) * (statTeamPit!(9) + 1))) - (statBatter(I, 0) + 0)
                    Print #of%, Using "###"; statCalc;
                End If
            End If

            'Proj AB
            Print #of%, Tab(95);

            If statAtBats(I) > 0 Then
                statCalc = CInt((((statAtBats(I) + 0) / QT) * (statTeamPit!(9) + 1)))
                Print #of%, Using "###"; statCalc;
                statTeamBat!(13) = statTeamBat!(13) + statCalc
            End If

            '% RAB
            Print #of%, Tab(100);
            If statAtBats(I) > 0 Then
                statCalc = ((statBatter(I, 0) / statAtBats(I)) * 100)
                Print #of%, Using "###"; statCalc;
            End If

            'Putouts
            Print #of%, Tab(105);: Print #of%, Using "####"; statBatter(I, 16);
            'Assists
            Print #of%, Tab(110);: Print #of%, Using "###"; statBatter(I, 17);
            'Errors
            Print #of%, Tab(114);: Print #of%, Using "##"; statBatter(I, 11);

            'Fielding AVG
            Print #of%, Tab(117);

             '>> Denominator = Putouts + Assists + Errors
           If statBatter(I, 11) + statBatter(I, 17) + statBatter(I, 16) <> 0 Then
                If statBatter(I, 11) <> 0 Then
                    statCalc = (statBatter(I, 16) + statBatter(I, 17)) / (statBatter(I, 16) + statBatter(I, 17) + statBatter(I, 11))
                    Print #of%, Using " .###"; statCalc;
                Else
                    'No errors
                    Print #of%, "1.000";
                End If
            Else
                'No fielding stats
                Print #of%, " .---";
            End If

            'Sac. Hits, Sac. Flies
            Print #of%, Tab(123); Using "## ##"; statBatter(I, 18); statBatter(I, 19)

        End If

    Next I

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Print #of%, Left$(pitcherNames$(I), 10); Tab(12);
            'Games
            Print #of%, Using "###"; statPitcher(I, 8);
            'At Bats, Runs, Hits, 2B
            Print #of%, Tab(16); Using "### ### ### ##"; statPitcher(I, 23); statPitcher(I, 25); statPitcher(I, 24); statPitcher(I, 27);
            '3B, HR, RBI, BB
            Print #of%, Tab(31); Using "## ## ### ###"; statPitcher(I, 28); statPitcher(I, 29); statPitcher(I, 26); statPitcher(I, 30);
            'L, SB, CS
            Print #of%, Tab(45); Using "### ### ##"; statPitcher(I, 31); statPitcher(I, 32); statPitcher(I, 33);
            'Chances
            Print #of%, Tab(56);: Print #of%, Using "##"; statPitcher(I, 35);
            'Left-Hand
            Print #of%, Tab(60);: Print #of%, Using "##"; statPitcher(I, 36);
            'Ground-into-DP
            Print #of%, Tab(64);: Print #of%, Using "##"; statPitcher(I, 37);

            'Bat AVG
            Print #of%, Tab(69);

            If statPitcher(I, 23) = 0 Then
                Print #of%, " .---";
            Else
                If statPitcher(I, 23) = statPitcher(I, 24) Then
                    Print #of%, "1.000";
                Else
                    If statPitcher(I, 24) = 0 Then
                        Print #of%, " .000";
                    Else
                        statCalc! = (statPitcher(I, 24) / statPitcher(I, 23))
                        Print #of%, Using " .###"; statCalc!;
                    End If
                End If
            End If

            'SLG
            Print #of%, Tab(75);

            J = statPitcher(I, 24) + statPitcher(I, 27) + statPitcher(I, 28) * 2 + statPitcher(I, 29) * 3

            If statPitcher(I, 23) = 0 Then
                Print #of%, " .---";
            Else
               If J = 0 Then
                    Print #1, " .000 ";
                Else                
                    If statPitcher(I, 23) = J Then
                        Print #of%, "1.000";
                    Else
                        statCalc! = J / statPitcher(I, 23)
                        If statCalc! >= 1 Then
                            Print #of%, Using "#.###"; statCalc!;
                        Else
                            If statPitcher(I, 24) = 0 Then
                                Print #of%, " .000";
                            Else
                                Print #of%, Using " .###"; statCalc;
                            End If
                        End If
                    End If
                End If
            End If

            'OBP
            Print #of%, Tab(81);

            J = statPitcher(I, 24) + statPitcher(I, 30)
            K = statPitcher(I, 30) + statPitcher(I, 23)

            If statPitcher(I, 23) = 0 Then
                Print #of%, " .---";
            Else
                If statPitcher(I, 23) = J Then
                    Print #of%, "1.000";
                Else
                    If J = K Then
                        Print #of%, "1.000";
                    Else
                        statCalc! = (J / K)
                        Print #of%, Using " .###"; statCalc!;
                    End If
                End If
            End If

            'Put Outs
            Print #of%, Tab(105);: Print #of%, Using "####"; statPitcher(I, 38);
            'Assists
            Print #of%, Tab(110);: Print #of%, Using "###"; statPitcher(I, 39);
            'Errors
            Print #of%, Tab(114);: Print #of%, Using "##"; statPitcher(I, 34);

            'Fielding AVG
            Print #of%, Tab(117);

            '>> Denominator = Putouts + Assists + Errors
            If statPitcher(I, 34) + statPitcher(I, 38) + statPitcher(I, 39) <> 0 Then
                If statPitcher(I, 34) <> 0 Then
                    statCalc! = (statPitcher(I, 38) + statPitcher(I, 39)) / (statPitcher(I, 38) + statPitcher(I, 39) + statPitcher(I, 34))
                    Print #of%, Using " .###"; statCalc!;
                Else
                    'No errors
                    Print #of%, "1.000";
                End If
            Else
                'No fielding stats
                Print #of%, " .---";
            End If

            Print #of%, Tab(123); Using "## ##"; statPitcher(I, 40); statPitcher(I, 41)

        End If

    Next I

    Print #of%, " "
    Print #of%, "TOTALS";
    Print #of%, Tab(12);: Print #of%, Using "###"; nbrGames;
    Print #of%, Tab(19);: Print #of%, Using "####"; statTeamBat!(2);
    Print #of%, Tab(27);: Print #of%, Using "###"; statTeamBat!(4);
    Print #of%, Tab(33);: Print #of%, Using "###"; statTeamBat!(6);
    Print #of%, Tab(40);: Print #of%, Using "####"; statTeamBat!(7);
    Print #of%, Tab(49);: Print #of%, Using "###"; statTeamBat!(9);
    statCalc! = (statTeamBat!(1) / statTeamBat!(0))
    Print #of%, Tab(69);: Print #of%, Using " .###"; statCalc!;
    statCalc! = ((statTeamBat!(1) + statTeamBat!(4) + statTeamBat!(5) * 2 + statTeamBat!(6) * 3) / statTeamBat!(0))
    Print #of%, Tab(75);: Print #of%, Using " .###"; statCalc!;
    statCalc! = ((statTeamBat!(1) + statTeamBat!(7)) / (statTeamBat!(7) + statTeamBat!(0)))
    Print #of%, Tab(81);: Print #of%, Using " .###"; statCalc!;
    Print #of%, Tab(105);: Print #of%, Using "####"; statTeamBat!(16);
    Print #of%, Tab(113);: Print #of%, Using "###"; statTeamBat!(11);
    Print #of%, Tab(122); Using "###"; statTeamBat!(18)
    Print #of%, Tab(15);: Print #of%, Using "####"; statTeamBat!(0);
    Print #of%, Tab(23);: Print #of%, Using "####"; statTeamBat!(1);
    Print #of%, Tab(30);: Print #of%, Using "###"; statTeamBat!(5);
    Print #of%, Tab(36);: Print #of%, Using "####"; statTeamBat!(3);
    Print #of%, Tab(44);: Print #of%, Using "####"; statTeamBat!(8);
    Print #of%, Tab(52);: Print #of%, Using "####"; statTeamBat!(10);
    Print #of%, Tab(63); Using "###"; statTeamBat!(15);
    Print #of%, Tab(109);: Print #of%, Using "####"; statTeamBat!(17);
    Print #of%, Tab(117);

    If statTeamBat!(16) + statTeamBat!(11) + statTeamBat!(17) = 0 Then
        Print #of%, " .---";
    Else
        If statTeamBat!(11) = 0 Then
            Print #of%, "1.000";
        Else
            statCalc! = (statTeamBat!(16) + statTeamBat!(17)) / (statTeamBat!(16) + statTeamBat!(11) + statTeamBat!(17))
            Print #of%, Using " .###"; statCalc!;
        End If
    End If

    Print #of%, Tab(125); Using "###"; statTeamBat!(19)
    Print #of%,
    Print #of%,

    TT$ = Str$(statTeamBat!(2) / (teamWins + teamLosses) + .05)
    UU$ = Str$(statTeamPit!(2) / (teamWins + teamLosses) + .05)
    Print #of%, "RUNS PER GAME ";: Print #of%, Left$(TT$, 4);
    Print #of%, " OPPONENTS";: Print #of%, Left$(UU$, 4)
    Print #of%,
    Print #of%,
    Print #of%, Tab(20); "OVERALL RECORD ("; Str$(totalWins); " -"; Str$(totalLosses); " )"
    Print #of%, Tab(20); "HOME RECORD    ("; Str$(homeWins); " -"; Str$(homeLosses); " )"
    Print #of%, Tab(20); "ROAD RECORD    ("; Str$(roadWins); " -"; Str$(roadLosses); " )"
    Print #of%, Tab(20); "LAST 10 GAMES  ("; Str$(lastTenWins); " -"; Str$(lastTenLosses); " )"
    Print #of%, Tab(20); "EXTRA INNINGS  ("; Str$(extraInnWins); " -"; Str$(extraInnLosses); " )"
    Print #of%, Tab(20); "ONE RUN GAMES  ("; Str$(oneRunWins); " -"; Str$(oneRunLosses); " )"
    Print #of%, Chr$(12)

    Print #of%, "PITCHERS    W  L SV  IP     H  BB  SO  ERA    G GS CG RUN  ER SHO HR DP %RIP SH SF  IR IRA BS   RPS QS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            'Name
            Print #of%, Left$(pitcherNames$(I), 10);
            'Wins
            Print #of%, Tab(12);: Print #of%, Using "##"; statPitcher(I, 6);
            'Losses
            Print #of%, Tab(15);: Print #of%, Using "##"; statPitcher(I, 7);
            'Saves
            Print #of%, Tab(18);: Print #of%, Using "##"; statPitcher(I, 12);
            'IP
            statCalc = Int(statPitcher(I, 0) / 3)
            Print #of%, Tab(21);: Print #of%, Using "###"; statCalc;

            If statPitcher(I, 0) - (Int(statPitcher(I, 0) / 3) * 3) > 0 Then
                Print #of%, ".";
                statCalc = (Int(statPitcher(I, 0) / 3) * 3)
                Print #of%, Using "#"; statPitcher(I, 0) - statCalc;
            End If

            'Hits
            Print #of%, Tab(27);: Print #of%, Using "###"; statPitcher(I, 1);
            'Walks
            Print #of%, Tab(31);: Print #of%, Using "###"; statPitcher(I, 4);
            'Strikeouts
            Print #of%, Tab(35);: Print #of%, Using "###"; statPitcher(I, 5);

            'ERA
            If statPitcher(I, 0) > 0 Then
                Print #of%, Tab(39);
                statCalc! = (statPitcher(I, 3) * 9 / (statPitcher(I, 0) / 3))
                Print #of%, Using "##.##"; statCalc!;
            Else
                '= 0
                Print #of%, Tab(39); " -.--";
            End If
            'G
            Print #of%, Tab(45);: Print #of%, Using "###"; statPitcher(I, 8);
            'GS
            Print #of%, Tab(49);: Print #of%, Using "##"; statPitcher(I, 9);
            'CG
            Print #of%, Tab(52);: Print #of%, Using "##"; statPitcher(I, 10);
            'Run
            Print #of%, Tab(55);: Print #of%, Using "###"; statPitcher(I, 2);
            'ER
            Print #of%, Tab(59);: Print #of%, Using "###"; statPitcher(I, 3);
            'SHO
            Print #of%, Tab(63);: Print #of%, Using "###"; statPitcher(I, 11);
            'HR
            Print #of%, Tab(67);: Print #of%, Using "##"; statPitcher(I, 14);
            'DP
            Print #of%, Tab(70);: Print #of%, Using "##"; statPitcher(I, 13);
            Print #of%, Tab(74);

            '%RIP
            If statInnPitch(I) > 0 Then
                statCalc = CInt((((statPitcher(I, 0) / 3) * 100) / statInnPitch(I)))
                Print #of%, Using "###"; statCalc;
            Else
                Print #of%, "  ";
            End If

            'Sac Hits, Sac Flies, IR, IRA, Blown Saves
            Print #of%, Tab(78); Using "## ## ### ### ## "; statPitcher(I, 15); statPitcher(I, 16); statPitcher(I, 17); statPitcher(I, 18); statPitcher(I, 19);

            'RPS
            If statPitcher(I, 9) > 0 Then
                Print #of%, Using "##.## "; statPitcher(I, 20) / statPitcher(I, 9);
            End If

            'QS
            Print #of%, Tab(101); Using "##"; statPitcher(I, 21)

        End If

    Next

    Print #of%,
    Print #of%, "TOTALS";
    Print #of%, Tab(18);: Print #of%, Using "##"; statTeamPit!(12);
    Print #of%, Tab(26);: Print #of%, Using "####"; statTeamPit!(1);
    Print #of%, Tab(34);: Print #of%, Using "####"; statTeamPit!(5);
    statCalc! = statTeamPit!(3) / (statTeamPit!(0) / 3) * 9
    Print #of%, Tab(39);: Print #of%, Using "##.##"; statCalc!;
    Print #of%, Tab(52);: Print #of%, Using "##"; statTeamPit!(10);
    Print #of%, Tab(58);: Print #of%, Using "####"; statTeamPit!(3);
    Print #of%, Tab(63);: Print #of%, Using "###"; statTeamPit!(11);
    Print #of%, Tab(69);: Print #of%, Using "###"; statTeamPit!(13);
    Print #of%, Tab(77); Using "###"; statTeamPit!(15);
    Print #of%, Tab(83); Using "####"; statTeamPit!(17);
    Print #of%, Tab(91); Using "###"; statTeamPit!(19);
    Print #of%, Tab(100); Using "###"; statTeamPit!(21)
    Print #of%, Tab(21);: Print #of%, Using "####"; statTeamPit!(0) / 3;
    Print #of%, Tab(30);: Print #of%, Using "####"; statTeamPit!(4);
    Print #of%, Tab(54);: Print #of%, Using "####"; statTeamPit!(2);
    Print #of%, Tab(66);: Print #of%, Using "###"; statTeamPit!(14);
    Print #of%, Tab(80); Using "###"; statTeamPit!(16);
    Print #of%, Tab(87); Using "####"; statTeamPit!(18)
    Print #of%,


    Print #of%,
    Print #of%,
    Print #of%, "CATCHERS    SBA   CS   CS%"

    For I = 0 To 22
        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" And statBatter(I, 21) + statBatter(I, 20) <> 0 Then
            statCalc! = statBatter(I, 21) / (statBatter(I, 20) + statBatter(I, 21)) * 100
            Print #of%, Left$(batterNames$(I), 10); Tab(12); Using "###   ##   ###"; statBatter(I, 20) + statBatter(I, 21); statBatter(I, 21); statCalc!
        End If
    Next

    If splitStatChoice <> 0 Then
        'DISPLAY SPLIT STATS
        Print #of%, teamName$;
        Print #of%, Tab(15); "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"

        For I = 0 To 22

            If batterNames$(I) <> "XXX" And Left$(batterNames$(I), 1) <> " " And batterNames$(I) <> "" Then

                Print #of%, batterNames$(I);
                Print #of%, Tab(20); Using "### ## ## ## ### ### "; statSplit(I, 0); statSplit(I, 2); statSplit(I, 3); statSplit(I, 4); statSplit(I, 5); statSplit(I, 6);

                If statSplit(I, 0) = 0 Or statSplit(I, 1) = 0 Then
                    Print #of%, " .000";
                Else
                    If statSplit(I, 0) = statSplit(I, 1) Then
                        Print #of%, "1.000";
                    Else
                        statCalc! = statSplit(I, 1) / statSplit(I, 0)
                        Print #of%, Using " .###"; statCalc!;
                    End If
                End If

                Print #of%, Tab(53); Using "### ## ## ## ### ### "; statBatter(I, 0) - statSplit(I, 0); statBatter(I, 4) - statSplit(I, 2); statBatter(I, 5) - statSplit(I, 3); statBatter(I, 6) - statSplit(I, 4); statBatter(I, 7) - statSplit(I, 5); statBatter(I, 8) - statSplit(I, 6);

                If statBatter(I, 0) - statSplit(I, 0) = 0 Or statBatter(I, 1) - statSplit(I, 1) = 0 Then
                    Print #of%, " .000"
                Else
                    If statBatter(I, 0) - statSplit(I, 0) = statBatter(I, 1) - statSplit(I, 1) Then
                        Print #of%, "1.000"
                    Else
                        statCalc! = (statBatter(I, 1) - statSplit(I, 1)) / (statBatter(I, 0) - statSplit(I, 0))
                        Print #of%, Using " .###"; statCalc!
                    End If
                End If

            End If

        Next I

    End If

    Print #of%,
    Print #of%,

    Print #of%, Chr$(12)

    Close #of%


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
'       ResetStatsPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with
'a choice to reset stats. If they chose,
'and confirm, it will then proceed to
'delete the files.
Sub ResetStatsPrompt (statFile$, teamRatings())

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


'----------------------------------------
'       SortBatterStats Subroutine
'----------------------------------------
'This subroutine sorts the associate stats
'based on the chosen category
'playerType
' 0 = BATTER (non-pitcher)
' 1 = BATTER (pitcher)
' 2 = PITCHER
Sub SortBatterStats (playerType, categoryPick)

    Shared batterNames$(), pitcherNames$()
    Shared calcProperties()

    Shared statAtBats(), statBatter()
    Shared statInnPitch(), statPitcher()

    Shared statTeamBat!(), statTeamPit!()

    'SORT BASED ON CATEGORY
    'Map category choices from menu
    'to index #'s in arrays

    ' ** NEED TO TAKE OUT "CALCULATED" CATEGORIES
    ' then re-index everything from there as well
    ' role of statTeamPit!()
    Select Case playerType

        Case 0

            'Batting / Fielding - Batters
            Select Case categoryPick

                Case 1: statGroup = 14
                Case 2: statGroup = 0
                Case 3: statGroup = 2
                Case 4: statGroup = 1
                Case 5: statGroup = 4
                Case 6: statGroup = 5
                Case 7: statGroup = 6
                Case 8: statGroup = 3
                Case 9: statGroup = 9
                Case 10: statGroup = 7
                Case 11: statGroup = 8
                Case 12: statGroup = -1 'Bat AVG
                Case 13: statGroup = -1 'SLG
                Case 14: statGroup = -1 'OBP
                Case 15: statGroup = 10
                Case 16: statGroup = -1 'Runs / At Bat
                Case 17: statGroup = 12
                Case 18: statGroup = 13
                Case 19: statGroup = 14
                Case 20: statGroup = -1 'Needs
                Case 21: statGroup = -1 'Proj AB
                Case 22: statGroup = 16
                Case 23: statGroup = 17
                Case 24: statGroup = 11
                Case 25: statGroup = -1 'Fielding AVG
                Case 26: statGroup = -1 'SBA
                Case 27: statGroup = 21
                Case 28: statGroup = -1 'CS %
                Case 29: statGroup = 18
                Case 30: statGroup = 19

            End Select

            maxPlayers = 22

        Case 1

            'Batting / Fielding - Pitchers
            Select Case categoryPick

                Case 1: statGroup = 8
                Case 2: statGroup = 23
                Case 3: statGroup = 25
                Case 4: statGroup = 24
                Case 5: statGroup = 27
                Case 6: statGroup = 28
                Case 7: statGroup = 29
                Case 8: statGroup = 26
                Case 9: statGroup = 32
                Case 10: statGroup = 30
                Case 11: statGroup = 31
                Case 12: statGroup = -1 'Bat AVG
                Case 13: statGroup = -1 'SLG
                Case 14: statGroup = -1 'OBP
                Case 15: statGroup = 33
                Case 16: statGroup = 35
                Case 17: statGroup = 36
                Case 18: statGroup = 37
                Case 19: statGroup = 38
                Case 20: statGroup = 39
                Case 21: statGroup = 34
                Case 22: statGroup = -1 'Fielding AVG
                Case 23: statGroup = 40
                Case 24: statGroup = 41

            End Select

            maxPlayers = 21

        Case 2

            'Pitcher Categories
            Select Case categoryPick

                Case 1: statGroup = 6
                Case 2: statGroup = 7
                Case 3: statGroup = -1 'ERA
                Case 4: statGroup = 8
                Case 5: statGroup = 9
                Case 6: statGroup = 10
                Case 7: statGroup = 11
                Case 8: statGroup = 12
                Case 9: statGroup = 0
                Case 10: statGroup = 1
                Case 11: statGroup = 2
                Case 12: statGroup = 3
                Case 13: statGroup = 14
                Case 14: statGroup = 4
                Case 15: statGroup = 5
                Case 16: statGroup = 13
                Case 17: statGroup = -1 '%IP
                Case 18: statGroup = 19
                Case 19: statGroup = 22
                Case 20: statGroup = 17
                Case 21: statGroup = 18
                Case 22: statGroup = -1 'RS
                Case 23: statGroup = 9
                Case 24: statGroup = 21
                Case 25: statGroup = 15
                Case 26: statGroup = 16

            End Select

            maxPlayers = 21

    End Select


    Select Case categoryPick

        Case 0
            'Batter / Pitcher Name
            For Z = 0 To maxPlayers - 1

                For Z1 = Z + 1 To maxPlayers

                    If playerType = 0 Then
                        sourceName$ = batterNames$(Z)
                        targetName$ = batterNames$(Z1)
                    Else
                        sourceName$ = pitcherNames$(Z)
                        targetName$ = pitcherNames$(Z1)
                    End If

                    If _StriCmp(sourceName$, targetName$) = 1 Then

                        If playerType = 0 Then
                            Swap batterNames$(Z), batterNames$(Z1)
                            Swap statAtBats(Z), statAtBats(Z1)
                            For I = 0 To 30:
                                Swap statBatter(Z, I), statBatter(Z1, I)
                            Next
                        Else
                            Swap pitcherNames$(Z), pitcherNames$(Z1)
                            Swap statInnPitch(Z), statInnPitch(Z1)
                            For I = 0 To 41:
                                Swap statPitcher(Z, I), statPitcher(Z1, I)
                            Next

                        End If


                    End If

                Next Z1

            Next Z

        Case Else

            'Batter / Pitcher Stats
            For Z = 0 To maxPlayers - 1

                For Z1 = Z + 1 To maxPlayers

                    If playerType = 0 Then
                        sourceStat = statBatter(Z, statGroup)
                        targetStat = statBatter(Z1, statGroup)
                    Else
                        sourceStat = statPitcher(Z, statGroup)
                        targetStat = statPitcher(Z1, statGroup)
                    End If

                    If sourceStat < targetStat Then

                        Select Case playerType

                            Case 0
                                Swap batterNames$(Z), batterNames$(Z1)
                                Swap statAtBats(Z), statAtBats(Z1)

                                'Sort ALL batter values
                                For I = 0 To 30:
                                    Swap statBatter(Z, I), statBatter(Z1, I)
                                Next

                            Case Else
                                Swap pitcherNames$(Z), pitcherNames$(Z1)
                                Swap statInnPitch(Z), statInnPitch(Z1)

                                'Sort ALL pitcher values
                                For I = 0 To 41:
                                    Swap statPitcher(Z, I), statPitcher(Z1, I)
                                Next

                        End Select

                    End If

                Next Z1

            Next Z

    End Select

End Sub
