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

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS FOR ONE TEAM"
        menuChoices$(2) = "PRINT REPORTS FOR ALL TEAMS IN A LEAGUE "
        menuChoices$(3) = "RETURN TO MAIN MENU                     "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "Default"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%
                Case 1: Call StatsOneTeam
                Case 2: Call ReportsAllTeams
                Case 3: Cls
            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       StatsOneTeam Subroutine
'----------------------------------------
'...explanation...
Sub StatsOneTeam ()

    Shared teamRatings%()

    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

    If teamNbr <> 0 Then

        Color 15, 0

        statFile$ = nameStatFile$(team$)

        If Not _FileExists(diskPaths$(1) + statFile$) And teamNbr > 0 Then

            MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
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

    Shared EW, EL
    Shared homeWins, homeLosses
    Shared LTL, LTW, ORW, ORL
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared batterNames$(), pitcherNames$()
    Shared statT0!(), statT1!()
    Shared statB%(), statB0%()
    Shared statP%(), statP0%()
    Shared statSS%()

    Shared oppName$(), losePitcher$(), locIndicator$(), winPitcher$()
    Shared teamScore(), nbrInnings(), oppScore()

    Shared D0%, D1%, L2%, W2%

    totalWins = 0
    totalLosses = 0
    roadWins = 0
    roadLosses = 0
    ORW = 0
    ORL = 0
    homeWins = 0
    homeLosses = 0
    EW = 0
    EL = 0
    LTL = 0
    LTW = 0

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, teamName$
    Input #1, W2%
    Input #1, L2%

    For I = 0 To 22: 
        Input #1, batterNames$(I)
        Input #1, statB%(I)
    Next I
    
    For I = 0 To 21: 
        Input #1, pitcherNames$(I)
        Input #1, statP%(I)
    Next I
    
    For I = 0 To 22: 
        For I1 = 0 To 21: 
            Input #1, statB0%(I, I1)
        Next I1
    Next I
    
    For I = 0 To 21: 
        For I1 = 0 To 41: 
            Input #1, statP0%(I, I1)
        Next I1
    Next I

    Input #1, D0%
    Input #1, D1%

    For I = 0 To 21: 
        Input #1, statT0!(I)
    Next I

    For I = 0 To 22: 
        Input #1, statT1!(I)
    Next I

    Close #1

    If Not _FileExists(diskPaths$(1) + statFile$ + "R") Then

        SS% = 0

    Else

        SS% = 1

        Open diskPaths$(1) + statFile$ + "R" For Input As #1

        For I = 0 To 22
            For I1 = 0 To 6
                Input #1, statSS%(I, I1)
            Next I1

        Next I

        Close #1

    End If

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, nbrGames

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, teamScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, oppName$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, oppScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, locIndicator$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, winPitcher$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, losePitcher$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Input #1, nbrInnings(I)
    Next I

    Close #1

    For I = 1 To nbrGames
        If Abs(teamScore(I) - oppScore(I)) = 1 And teamScore(I) > oppScore(I) Then ORW = ORW + 1
        If Abs(teamScore(I) - oppScore(I)) = 1 And teamScore(I) < oppScore(I) Then ORL = ORL + 1
        If teamScore(I) > oppScore(I) Then totalWins = totalWins + 1
        If teamScore(I) > oppScore(I) And nbrInnings(I) > 9 Then EW = EW + 1
        If teamScore(I) > oppScore(I) And locIndicator$(I) = "A" Then roadWins = roadWins + 1
        If teamScore(I) > oppScore(I) And locIndicator$(I) = "H" Then homeWins = homeWins + 1
        If oppScore(I) > teamScore(I) Then totalLosses = totalLosses + 1
        If oppScore(I) > teamScore(I) And nbrInnings(I) > 9 Then EL = EL + 1
        If oppScore(I) > teamScore(I) And locIndicator$(I) = "A" Then roadLosses = roadLosses + 1
        If oppScore(I) > teamScore(I) And locIndicator$(I) = "H" Then homeLosses = homeLosses + 1
    Next I

    If nbrGames <= 10 Then
        LTL = totalLosses
        LTW = totalWins
    Else
        M = nbrGames - 9
        For J = M To nbrGames
            If teamScore(J) > oppScore(J) Then LTW = LTW + 1
            If oppScore(J) > teamScore(J) Then LTL = LTL + 1
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

    Shared teamScore(), nbrInnings(), teamRatings%(), oppScore()

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

        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "Default"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1
                    Color , 0
                    Cls

                    Color 12
                    header$ = "SCHEDULE RESULTS FOR " + teamName$
                    Xcoord = Len(header$)
                    Xcoord = Int((80 - Xcoord) / 2)
                    Locate 1, Xcoord

                    Print "SCHEDULE RESULTS FOR ";
                    Color teamRatings%(11), teamRatings%(12)
                    Print teamName$

                    Color 15, 0
                    Color 14: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
                    Color 15

                    For I = 1 To nbrGames

                        'winPitcher$() = Winning Pitcher
                        'losePitcher$() = Losing Pitcher
                        'nbrInnings() = Innings
                        Print Str$(teamScore(I)); Tab(6); Left$(oppName$(I), 15); Tab(22); Str$(oppScore(I)); Tab(27); "("; locIndicator$(I); ")"; Tab(33); winPitcher$(I); Tab(50); losePitcher$(I); Tab(66); nbrInnings(I)

                        If (I Mod 20 = 0) Then

                            Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";
                            I$ = GetKeyPress$

                            Cls

                            Locate 1, Xcoord
                            Color 12
                            Print "SCHEDULE RESULTS FOR ";
                            Color teamRatings%(11), teamRatings%(12)
                            Print teamName$

                            Color 15, 0
                            Color 14: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
                            Color 15

                        End If

                    Next

                    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    I$ = GetKeyPress$

                Case 2:
                    '

                Case 3, 4
                    Call NewPrintStats(statFile$, teamName$, nbrGames, Selection%, splitScreenChoice%)

            End Select

            If Selection% <> 3 And Selection% <> 4 Then

                Color 15, 0
                Cls

                Call ViewStatSummary(statFile$, teamName$, splitScreenChoice%)

                Color 15, 0

                Cls

                JJ = 1
                Locate 6, 8

                Call MenuBox (6, 8, 61, 1, teamRatings%(11), teamRatings%(12))

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

    Loop Until Selection% = 5 Or ExitCode% = 27

    'Call ResetStatPrompt (statFile$, teamRatings%())

    Cls

End Sub


'----------------------------------------
'       ViewStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatSummary (statFile$, teamName$, splitScreenChoice%)

    Shared EW, EL
    Shared homeWins, homeLosses
    Shared LTL, LTW, ORW, ORL
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared L2%, W2%

    Shared teamRatings%()

    Shared batterNames$(), pitcherNames$(), statSS%()
    Shared statT0!(), statT1!()

    Shared statB%(), statB0%()
    Shared teamScore(), nbrInnings(), teamRatings%(), oppScore()
    Shared statP%(), statP0%()

    Shared oppName$(), losePitcher$(), locIndicator$(), winPitcher$()

    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate 1, Xcoord

    Color teamRatings%(11), teamRatings%(12)
    Print teamName$
    Print

    Color 15, 0
    Locate , 20
    Color 14: Print "OVERALL RECORD ";
    Color 15: Print "("; totalWins; "-"; totalLosses; ")"
    Locate , 20
    Color 14: Print "HOME RECORD    ";
    Color 15: Print "("; homeWins; "-"; homeLosses; ")"
    Locate , 20
    Color 14: Print "ROAD RECORD    ";
    Color 15: Print "("; roadWins; "-"; roadLosses; ")"
    Locate , 20
    Color 14: Print "LAST 10 GAMES  ";
    Color 15: Print "("; LTW; " -"; LTL; ")"
    Locate , 20
    Color 14: Print "EXTRA INNINGS  ";
    Color 15: Print "("; EW; " -"; EL; ")"
    Locate , 20
    Color 14: Print "ONE RUN GAMES  ";
    Color 15: Print "("; ORW; " -"; ORL; ")"

    I$ = GetKeyPress$
    Cls

    'Call TITLE_STATS

    Locate 1, 1
    Print Tab(80);

    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord

    'Color 14
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 15, 1
    Print "BATTERS    AB   R   H RBI  2B 3B  HR    BA   SLG   OBP  SO  BB  SB  CS %RAB    "
    Color 15, 0

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            Locate I + 3, 1
            Color 7
            If I / 2 = Int(I / 2) Then Color 15

            Print Left$(batterNames$(I), 10);

            Locate , 11: Print Using "###"; statB0%(I, 0);
            Locate , 15: Print Using "###"; statB0%(I, 2);
            Locate , 19: Print Using "###"; statB0%(I, 1);
            Locate , 23: Print Using "###"; statB0%(I, 3);
            Locate , 28: Print Using "##"; statB0%(I, 4);
            Locate , 31: Print Using "##"; statB0%(I, 5);
            Locate , 35: Print Using "##"; statB0%(I, 6);

            I2 = 0
            I3 = 0
            I4 = 0

            Locate , 38

            If statB0%(I, 0) = 0 Then
                Print " .--- ";
            Else
                If statB0%(I, 0) = statB0%(I, 1) Then
                    Print "1.000 ";
                Else
                    If statB0%(I, 1) = 0 Then
                        Print " .000 ";
                    Else
                        Print Using " .### "; statB0%(I, 1) / statB0%(I, 0);
                    End If
                End If
            End If

            I3 = statB0%(I, 1) + statB0%(I, 4) + 2 * statB0%(I, 5) + 3 * statB0%(I, 6)

            If statB0%(I, 0) = 0 Then
                Print " .--- ";
            Else
                If I3 = 0 Then
                    Print " .000 ";
                Else
                    If statB0%(I, 0) = I3 Then
                        Print "1.000 ";
                    Else
                        If I3 / statB0%(I, 0) >= 1 Then
                            Print Using "#.### "; I3 / statB0%(I, 0);
                        Else
                            Print Using " .### "; I3 / statB0%(I, 0);
                        End If
                    End If
                End If
            End If

            I4 = statB0%(I, 1) + statB0%(I, 7)
            I3 = statB0%(I, 7) + statB0%(I, 0)

            If I3 = 0 Then
                Print " .---";
            Else
                If I4 = 0 Then
                    Print " .000";
                Else
                    If I4 = I3 Then
                        Print "1.000";
                    Else
                        Print Using " .###"; I4 / I3;
                    End If
                End If
            End If

            Locate , 56: Print Using "###"; statB0%(I, 8);
            Locate , 60: Print Using "###"; statB0%(I, 7);
            Locate , 64: Print Using "###"; statB0%(I, 9);
            Locate , 69: Print Using "##"; statB0%(I, 10);

            If statB%(I) > 0 Then
                Locate , 73
                MM = statB0%(I, 0) / statB%(I) * 100
                MM = Int(MM)
                Print Str$(MM);
            Else
                If statB%(I) = 0 Then Locate , 73: Print "--";
            End If

            If statT0!(0) = 0 Then statT0!(0) = 1

        End If 'Done checking for XXX / terminal chars

    Next I

    Locate 1, 1
    I$ = GetKeyPress$

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 15, 1
    Print "BATTERS      G  CHS LHS GIDP NEEDS PROJ AB  PO   A  E  FAVG  SBA CS %CS SH SF"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

            Locate I + 3, 1

            Color 7, 0

            If I / 2 = Int(I / 2) Then Color 15

            Print Left$(batterNames$(I), 10);
            Locate , 12: Print Using "###"; statB0%(I, 14);
            Locate , 18: Print Using "##"; statB0%(I, 12);
            Locate , 22: Print Using "##"; statB0%(I, 13);
            Locate , 27: Print Using "##"; statB0%(I, 15);

            RQ$ = Right$(statFile$, 2)
            RT = 60
            RQ = Val(RQ$)
            If RQ > RT Then
                QT = 162
            Else
                QT = 154
            End If

            statCalc% = (Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5) - (statB0%(I, 0) + 0))

            If (statB%(I) > 0) And statCalc% > 0 Then
                Locate , 31: Print Using "####"; statCalc%;
            Else
                If statB%(I) = 0 Then Print "  ";
                If statB%(I) > 0 Then Locate , 31: Print Using "####"; statCalc%;
            End If

            If statB%(I) > 0 Then Locate , 37: H5% = Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5): Print Using "####"; H5%;: statT0!(13) = statT0!(13) + H5%
            If statB%(I) = 0 Then Locate , 39: Print "0";
            Locate , 42

            Print Using " #### ### ## "; statB0%(I, 16); statB0%(I, 17); statB0%(I, 11);

            If statB0%(I, 16) + statB0%(I, 17) + statB0%(I, 11) <> 0 Then
                If statB0%(I, 11) <> 0 Then
                    Print Using " .###"; (statB0%(I, 16) + statB0%(I, 17)) / (statB0%(I, 16) + statB0%(I, 17) + statB0%(I, 11));
                Else
                    Print "1.000";
                End If
            End If

            If statB0%(I, 21) + statB0%(I, 20) <> 0 Then
                Print Using "  ### ## ###"; statB0%(I, 20) + statB0%(I, 21); statB0%(I, 21); statB0%(I, 21) / (statB0%(I, 20) + statB0%(I, 21)) * 100;
            End If
            Locate , 73: Print Using "## ##"; statB0%(I, 18); statB0%(I, 19);

        End If

    Next I

    I$ = GetKeyPress$

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 15, 1
    Print "PITCHERS   AB   R   H RBI  2B 3B  HR    BA   SLG   OBP  SO  BB  SB  CS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Color 7, 0

            If I / 2 = Int(I / 2) Then Color 15

            Locate I + 3, 1: Print Left$(pitcherNames$(I), 10);
            Locate , 11: Print Using "###"; statP0%(I, 23);
            Locate , 15: Print Using "###"; statP0%(I, 25);
            Locate , 19: Print Using "###"; statP0%(I, 24);
            Locate , 23: Print Using "###"; statP0%(I, 26);
            Locate , 28: Print Using "##"; statP0%(I, 27);
            Locate , 31: Print Using "##"; statP0%(I, 28);
            Locate , 35: Print Using "##"; statP0%(I, 29);

            Locate , 38

            If statP0%(I, 23) = 0 Then
                Print " .--- ";
            Else
                If statP0%(I, 23) = statP0%(I, 24) Then
                    Print "1.000 ";
                Else
                    If statP0%(I, 24) = 0 Then
                        Print " .000 ";
                    Else
                        Print Using " .### "; statP0%(I, 24) / statP0%(I, 23);
                    End If
                End If
            End If

            I3 = statP0%(I, 24) + statP0%(I, 27) + 2 * statP0%(I, 28) + 3 * statP0%(I, 29)

            If statP0%(I, 23) = 0 Then
                Print " .--- ";
            Else
                If I3 = 0 Then
                    Print " .000 ";
                Else
                    If statP0%(I, 23) = I3 Then
                        Print "1.000 ";
                    Else
                        If I3 / statP0%(I, 23) >= 1 Then
                            Print Using "#.### "; I3 / statP0%(I, 23);
                        Else
                            Print Using " .### "; I3 / statP0%(I, 23);
                        End If
                    End If
                End If
            End If

            I3 = statP0%(I, 30) + statP0%(I, 23)
            I4 = statP0%(I, 24) + statP0%(I, 30)
            If I3 = 0 Then
                Print " .---";
            Else
                If I4 = 0 Then
                    Print " .000";
                Else
                    If I4 = I3 Then
                        Print "1.000";
                    Else
                        Print Using " .###"; I4 / I3;
                    End If
                End If
            End If

            Locate , 56: Print Using "###"; statP0%(I, 31);
            Locate , 60: Print Using "###"; statP0%(I, 30);
            Locate , 64: Print Using "###"; statP0%(I, 32);
            Locate , 69: Print Using "##"; statP0%(I, 33);
            If statT0!(0) = 0 Then statT0!(0) = 1

        End If
    Next I

    Locate 1, 1
    I$ = GetKeyPress$

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 15, 1
    Print "PITCHERS     G  CHS LHS GIDP   PO   A  E  FAVG SH SF"
    Color 15

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

            Locate I + 3, 1

            Color 7, 0

            If I / 2 = Int(I / 2) Then Color 15

            Print Left$(pitcherNames$(I), 10);
            Locate , 12: Print Using "###"; statP0%(I, 8);
            Locate , 18: Print Using "##"; statP0%(I, 35);
            Locate , 22: Print Using "##"; statP0%(I, 36);
            Locate , 27: Print Using "##"; statP0%(I, 37);: Locate , 29
            Print Using " #### ### ## "; statP0%(I, 38); statP0%(I, 39); statP0%(I, 34);

            If statP0%(I, 38) + statP0%(I, 39) + statP0%(I, 34) <> 0 Then
                If statP0%(I, 34) = 0 Then
                    Print "1.000";
                Else
                    Print Using " .###"; (statP0%(I, 38) + statP0%(I, 39)) / (statP0%(I, 38) + statP0%(I, 39) + statP0%(I, 34));
                End If
            End If

            Locate , 48: Print Using "## ##"; statP0%(I, 40); statP0%(I, 41);

        End If
    Next I

    Locate 1, 1
    I$ = GetKeyPress$

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 14, 0
    Print
    Print "  TEAM BATTING TOTALS"
    Color 15, 1
    Print "  AB    R    H  RBI  2B  3B  HR    BA   SLG   OBP   SO   BB  SB  CS"
    Color 14, 0

    Print Using "#### #### #### #### ### ### ###"; statT0!(0); statT0!(2); statT0!(1); statT0!(3); statT0!(4); statT0!(5); statT0!(6);
    Locate , 34: Print ".";: Print Using "###"; Int((statT0!(1) / statT0!(0) + .0005) * 1000);
    Locate , 40: Print ".";
    Print Using "###"; Int((((statT0!(1) + statT0!(4) + 2 * statT0!(5) + 3 * statT0!(6)) / statT0!(0)) + .0005) * 1000);
    Locate , 46: Print ".";
    Print Using "###"; Int(((statT0!(1) + statT0!(7)) / (statT0!(7) + statT0!(0)) + .0005) * 1000);
    Locate , 51: Print Using "#### #### ### ###"; statT0!(8); statT0!(7); statT0!(9); statT0!(10)
    Locate 8, 1: Color 15, 1: Print "GIDP   PO    A   E  FAVG  SH  SF  LOB": Color 15
    Color 14, 0
    Print Using "#### #### #### ###"; statT0!(15); statT0!(16); statT0!(17); statT0!(11);
    Locate , 20: If statT0!(11) = 0 Then Print "1.000";
    If statT0!(11) <> 0 Then Print Using " .###"; (statT0!(16) + statT0!(17)) / (statT0!(16) + statT0!(17) + statT0!(11));
    Print Using " ### ### ####"; statT0!(18); statT0!(19); statT0!(12)

    I$ = GetKeyPress$

    If splitScreenChoice% <> 0 Then
        ' DISPLAY SPLIT STATS
        Cls
        Color teamRatings%(11), teamRatings%(12): Print teamName$;
        Locate 1, 15
        Color 14, 0
        Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
        Color 15, 0
        Locate 2

        For I = 0 To 22

            If batterNames$(I) <> "XXX" And Left$(batterNames$(I), 1) <> " " And batterNames$(I) <> "" Then

                Print batterNames$(I);
                '                        AB   2B 3B HR BB  K
                Locate , 20: Print Using "### ## ## ## ### ### "; statSS%(I, 0); statSS%(I, 2); statSS%(I, 3); statSS%(I, 4); statSS%(I, 5); statSS%(I, 6);

                If statSS%(I, 0) = 0 And statSS%(I, 1) = 0 Then
                    Print " .000";
                Else
                    If statSS%(I, 0) = statSS%(I, 1) Then
                        Print "1.000";
                    Else
                        Print Using "#.###"; statSS%(I, 1) / statSS%(I, 0);
                    End If
                End If

                '                        AB   2B 3B HR BB  K
                Locate , 53: Print Using "### ## ## ## ### ### "; statB0%(I, 0) - statSS%(I, 0); statB0%(I, 4) - statSS%(I, 2); statB0%(I, 5) - statSS%(I, 3); statB0%(I, 6) - statSS%(I, 4); statB0%(I, 7) - statSS%(I, 5); statB0%(I, 8) - statSS%(I, 6);
                If statB0%(I, 0) - statSS%(I, 0) = 0 Or statB0%(I, 1) - statSS%(I, 1) = 0 Then
                    Print " .000";
                Else
                    If statB0%(I, 0) - statSS%(I, 0) = statB0%(I, 1) - statSS%(I, 1) Then
                        Print "1.000";
                    Else
                        Print Using "#.###"; (statB0%(I, 1) - statSS%(I, 1)) / (statB0%(I, 0) - statSS%(I, 0));
                    End If
                End If
                If I <> 22 Then Print

            End If
        Next I

        I$ = GetKeyPress$

    End If

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 15, 4
    Print "PITCHERS    W  L SV    IP    H  BB   SO   ERA  G GS CG RUN  ER ShO HRA  DP %RIP"

    Color 15

    For I = 0 To 21
        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then
            Locate I + 3, 1
            Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
            Print Left$(pitcherNames$(I), 10);
            Locate , 12: Print Using "##"; statP0%(I, 6);
            Locate , 15: Print Using "##"; statP0%(I, 7);
            Locate , 18: Print Using "##"; statP0%(I, 12);
            Locate , 22: Print Using "###"; Int(statP0%(I, 0) / 3);
            Print ".";: Print Using "#"; statP0%(I, 0) - Int(statP0%(I, 0) / 3) * 3;
            Locate , 27: Print Using "####"; statP0%(I, 1);
            Locate , 32: Print Using "###"; statP0%(I, 4);
            Locate , 36: Print Using "####"; statP0%(I, 5);
            Locate , 41

            If statP0%(I, 0) = 0 Then
                Print " -.--";
            Else
                If statP0%(I, 3) = 0 Then
                    Print " 0.00";
                Else
                    Print Using "##.##"; (statP0%(I, 3) * 9) / (statP0%(I, 0) / 3);
                End If
            End If

            Locate , 47: Print Using "## ## ## ### ### ## ### ###"; statP0%(I, 8); statP0%(I, 9); statP0%(I, 10); statP0%(I, 2); statP0%(I, 3); statP0%(I, 11); statP0%(I, 14); statP0%(I, 13);

            If statP%(I) > 0 Then Locate , 76: Print Using "###"; Int((((statP0%(I, 0) / 3) * 100) / statP%(I)) + .5);
            If statP%(I) = 0 Then Print "  ";

        End If

    Next I

    I$ = GetKeyPress$

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 15, 4
    Print "PITCHERS   BS HL  RI  RS   SUP GS QS  SH SF"

    For I = 0 To 21
        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then
            Locate I + 3, 1
            Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
            Print Left$(pitcherNames$(I), 10);: Locate , 12
            Print Using "## ## ### ### "; statP0%(I, 19); statP0%(I, 22); statP0%(I, 17); statP0%(I, 18);
            If statP0%(I, 9) > 0 Then
                Print Using "##.## ## ##"; statP0%(I, 20) / statP0%(I, 9); statP0%(I, 9); statP0%(I, 21);
            Else
                Print " -.-- -- --";
            End If
            Print Using "  ## ##"; statP0%(I, 15); statP0%(I, 16);
        End If
    Next I

    Locate 1, 1

    I$ = GetKeyPress$

    Cls

    Locate , Xcoord
    Color teamRatings%(11), teamRatings%(12)
    Print teamName$;

    Color 12, 0
    Print "    WON: "; W2%; "  LOST: "; L2%
    Color 14, 0
    Print
    Print "  TEAM PITCHING TOTALS"
    Color 15, 4
    Print "  W   L  SV     IP    H   BB   SO   ERA   G  GS  CG  RUN   ER ShO HRA  DP"
    Color 14, 0
    Print Using "### ### ### "; W2%; L2%; statT1!(12);
    Print Using "####"; Int(statT1!(0) / 3);
    Print ".";: Print Using "#"; statT1!(0) - Int(statT1!(0) / 3) * 3;
    Print Using " #### #### #### ##.## ### ### ### "; statT1!(1); statT1!(4); statT1!(5); statT1!(3) * 9 / (statT1!(0) / 3); statT1!(9); statT1!(9); statT1!(10);
    Print Using "#### #### ### ### ###"; statT1!(2); statT1!(3); statT1!(11); statT1!(14); statT1!(13)

    Locate 8, 1
    Color 15, 4: Print " BS   RI   RS   SUP  QS  SH  SF": Color 14, 0
    Print Using "### #### #### ##.## ### ### ###"; statT1!(19); statT1!(17); statT1!(18); statT0!(2) / statT1!(9); statT1!(21); statT1!(15); statT1!(16)

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       ReportsAllTeams Subroutine
'----------------------------------------
'This subroutine generates a stat report
'for each team in the chosen league.
Sub ReportsAllTeams ()

    Dim statFiles$(40)
    Shared D0%, D1%, L2%, W2%

    Shared batterNames$(), pitcherNames$()
    Shared statT0!(), statT1!()
    Shared statB%(), statB0%()
    Shared statP%(), statP0%()

    leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Open diskPaths$(2) + leagStatFile$ + ".TMS" For Input As #2

        Input #2, league$, D1$, D2$, D3$, D4$

        For XW = 1 To 40
            Input #2, statFiles$(XW)
        Next

        Close 2

        For XW = 1 To 40

            If _FileExists(diskPaths$(1) + statFiles$(XW)) Then

                Open diskPaths$(1) + statFiles$(XW) For Input As #1

                Input #1, teamName$
                Input #1, W2%
                Input #1, L2%

                For I = 0 To 22: 
                    Input #1, batterNames$(I)
                    Input #1, statB%(I)
                Next I

                For I = 0 To 21: 
                    Input #1, pitcherNames$(I)
                    Input #1, statP%(I)
                Next I

                For I = 0 To 22: 
                    For I1 = 0 To 21: 
                        Input #1, statB0%(I, I1)
                    Next I1
                Next I

                For I = 0 To 21: 
                    For I1 = 0 To 41: 
                        Input #1, statP0%(I, I1)
                    Next I1
                Next I

                Input #1, D0%
                Input #1, D1%

                For I = 0 To 21: 
                    Input #1, statT0!(I)
                Next I
                
                For I = 0 To 22: 
                    Input #1, statT1!(I)
                Next I

                Close #1

                BP = 1 'this doesn't appear to be used
                X = 3

                Call NewPrintStats(statFile$, teamName$, nbrGames, X, splitScreenChoice%)

            End If

        Next

    End If 'Done checking for ESC


End Sub


'----------------------------------------
'       NewPrintStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'team's schedule results and stats to the 
'default printer
Sub NewPrintStats (statFile$, teamName$, nbrGames, printChoice, splitScreenChoice%)

    Shared L2%, W2%

    Shared EW, EL
    Shared homeWins, homeLosses
    Shared LTL, LTW, ORW, ORL
    Shared roadWins, roadLosses
    Shared totalWins, totalLosses

    Shared batterNames$(), pitcherNames$()
    Shared statT0!(), statT1!()
    Shared statB%(), statB0%()
    Shared statP%(), statP0%()

    Shared oppName$(), losePitcher$()
    Shared locIndicator$(), winPitcher$()
    Shared teamScore(), nbrInnings()
    Shared statSS%(), oppScore()

    ' *** NEW PRINT ROUTINE 6-24-90 ***
    If printChoice <> 3 Then
        totalWins = 0
        totalLosses = 0
        ORW = 0
        ORL = 0
        roadWins = 0
        roadLosses = 0
        homeWins = 0
        homeLosses = 0
        EW = 0
        EL = 0
        LTL = 0
        LTW = 0

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
        LPrint Tab(20); "RECORD         ("; Str$(totalWins); " -"; Str$(totalLosses); " )"
        LPrint Tab(20); "HOME RECORD    ("; Str$(homeWins); " -"; Str$(homeLosses); " )"
        LPrint Tab(20); "ROAD RECORD    ("; Str$(roadWins); " -"; Str$(roadLosses); " )"
        LPrint Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
        LPrint Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
        LPrint Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
        LPrint Chr$(12)

    End If

    LPrint teamName$; "    WON: "; W2%; " LOST: "; L2%
    LPrint "BATTERS      G  AB   R   H 2B 3B HR RBI  BB   K  SB CS CHS LHS GIDP  BAVG   SLG   OBP NEED PROJ.AB %RAB   PO   A  E  FAVG SH SF"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then
            LPrint Left$(batterNames$(I), 10); Tab(12);: LPrint Using "###"; statB0%(I, 14);
            LPrint Tab(16); Using "### ### ### ##"; statB0%(I, 0); statB0%(I, 2); statB0%(I, 1); statB0%(I, 4);
            LPrint Tab(31); Using "## ## ### ###"; statB0%(I, 5); statB0%(I, 6); statB0%(I, 3); statB0%(I, 7);
            LPrint Tab(45); Using "### ### ##"; statB0%(I, 8); statB0%(I, 9); statB0%(I, 10);
            LPrint Tab(56);: LPrint Using "##"; statB0%(I, 12);
            LPrint Tab(60);: LPrint Using "##"; statB0%(I, 13);
            LPrint Tab(64);: LPrint Using "##"; statB0%(I, 15);
            LPrint Tab(69);

            If statB0%(I, 0) = 0 Then
                LPrint "-.---";
            Else
                If statB0%(I, 0) = statB0%(I, 1) Then
                    LPrint "1.000";
                Else
                    If statB0%(I, 1) = 0 Then
                        LPrint " .000";
                    Else
                        LPrint Using " .###"; (statB0%(I, 1) / statB0%(I, 0));
                    End If
                End If
            End If

            LPrint Tab(75);

            If statB0%(I, 0) = 0 Then
                LPrint "-.---";
            Else
                J = statB0%(I, 1) + statB0%(I, 4) + statB0%(I, 5) * 2 + statB0%(I, 6) * 3
                If J = statB0%(I, 0) Then
                    LPrint "1.000";
                Else
                    If J / statB0%(I, 0) > 1 Then
                        LPrint Using "#.###"; (J / statB0%(I, 0));
                    Else
                        If statB0%(I, 1) = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; (J / statB0%(I, 0));
                        End If
                    End If
                End If
            End If

            LPrint Tab(81);

            If statB0%(I, 0) = 0 Then
                LPrint "-.---";
            Else
                J = statB0%(I, 1) + statB0%(I, 7)
                If J = statB0%(I, 0) Then
                    LPrint "1.000";
                Else
                    If J / (statB0%(I, 7) + statB0%(I, 0)) > 1 Then
                        LPrint Using "#.###"; (J / (statB0%(I, 7) + statB0%(I, 0)));
                    Else
                        If J = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; (J / (statB0%(I, 7) + statB0%(I, 0)));
                        End If
                    End If
                End If
            End If

            LPrint Tab(87);

            RQ$ = Right$(statFile$, 2)
            RT = 60
            RQ = Val(RQ$)

            If RQ > RT Then
                QT = 162
            Else
                QT = 154
            End If

            If statB%(I) > 0 And (Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5) - (statB0%(I, 0) + 0)) > 0 Then
                LPrint Using "###"; Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5) - (statB0%(I, 0) + 0);
            Else
                If statB%(I) > 0 Then
                    LPrint Using "###"; Int(((statB%(I) / QT) * (statT1!(9) + 1)) + .5) - (statB0%(I, 0) + 0);
                End If
            End If

            LPrint Tab(95);
            If statB%(I) > 0 Then H5% = Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5): LPrint Using "###"; H5%;: statT0!(13) = statT0!(13) + H5%

            LPrint Tab(100);
            If statB%(I) > 0 Then LPrint Using "###"; ((statB0%(I, 0) / statB%(I)) * 100);

            LPrint Tab(105);: LPrint Using "####"; statB0%(I, 16);
            LPrint Tab(110);: LPrint Using "###"; statB0%(I, 17);
            LPrint Tab(114);: LPrint Using "##"; statB0%(I, 11);: LPrint Tab(117);

            If statB0%(I, 11) + statB0%(I, 17) + statB0%(I, 16) = 0 Then
                LPrint " .---";
            Else
                If statB0%(I, 11) = 0 Then
                    LPrint "1.000";
                Else
                    LPrint Using " .###"; (statB0%(I, 16) + statB0%(I, 17)) / (statB0%(I, 16) + statB0%(I, 17) + statB0%(I, 11));
                End If
            End If

            LPrint Tab(123); Using "## ##"; statB0%(I, 18); statB0%(I, 19)

        End If

    Next

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then
            LPrint Left$(pitcherNames$(I), 10); Tab(12);
            LPrint Using "###"; statP0%(I, 8);
            LPrint Tab(16); Using "### ### ### ##"; statP0%(I, 23); statP0%(I, 25); statP0%(I, 24); statP0%(I, 27);
            LPrint Tab(31); Using "## ## ### ###"; statP0%(I, 28); statP0%(I, 29); statP0%(I, 26); statP0%(I, 30);
            LPrint Tab(45); Using "### ### ##"; statP0%(I, 31); statP0%(I, 32); statP0%(I, 33);
            LPrint Tab(56);: LPrint Using "##"; statP0%(I, 35);
            LPrint Tab(60);: LPrint Using "##"; statP0%(I, 36);
            LPrint Tab(64);: LPrint Using "##"; statP0%(I, 37);

            LPrint Tab(69);
            If statP0%(I, 23) = 0 Then
                LPrint "-.---";
            Else
                If statP0%(I, 23) = statP0%(I, 24) Then
                    LPrint "1.000";
                Else
                    If statP0%(I, 24) = 0 Then
                        LPrint " .000";
                    Else
                        LPrint Using " .###"; (statP0%(I, 24) / statP0%(I, 23));
                    End If
                End If
            End If

            LPrint Tab(75);
            If statP0%(I, 23) = 0 Then
                LPrint "-.---";
            Else
                J = statP0%(I, 24) + statP0%(I, 27) + statP0%(I, 28) * 2 + statP0%(I, 29) * 3
                If J = statP0%(I, 23) Then
                    LPrint "1.000";
                Else
                    If J / statP0%(I, 23) > 1 Then
                        LPrint Using "#.###"; (J / statP0%(I, 23));
                    Else
                        If statP0%(I, 24) = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; (J / statP0%(I, 23));
                        End If
                    End If
                End If
            End If

            LPrint Tab(81);
            If statP0%(I, 23) = 0 Then
                LPrint "-.---";
            Else
                J = statP0%(I, 24) + statP0%(I, 30)
                If J = statP0%(I, 23) Then
                    LPrint "1.000";
                Else
                    If J / (statP0%(I, 30) + statP0%(I, 23)) > 1 Then
                        LPrint Using "#.###"; (J / (statP0%(I, 30) + statP0%(I, 23)));
                    Else
                        If J = 0 Then
                            LPrint " .000";
                        Else
                            LPrint Using " .###"; (J / (statP0%(I, 30) + statP0%(I, 23)));
                        End If
                    End If
                End If
            End If

            LPrint Tab(105);: LPrint Using "####"; statP0%(I, 38);
            LPrint Tab(110);: LPrint Using "###"; statP0%(I, 39);
            LPrint Tab(114);: LPrint Using "##"; statP0%(I, 34);: LPrint Tab(117);

            If statP0%(I, 34) + statP0%(I, 38) + statP0%(I, 39) = 0 Then
                LPrint " .---";
            Else
                If statP0%(I, 34) = 0 Then
                    LPrint "1.000";
                Else
                    LPrint Using " .###"; (statP0%(I, 38) + statP0%(I, 39)) / (statP0%(I, 38) + statP0%(I, 39) + statP0%(I, 34));
                End If
            End If

            LPrint Tab(123); Using "## ##"; statP0%(I, 40); statP0%(I, 41)

        End If

    Next

    LPrint " ": LPrint "TOTALS";
    LPrint Tab(12);: LPrint Using "###"; nbrGames;
    LPrint Tab(19);: LPrint Using "####"; statT0!(2);
    LPrint Tab(27);: LPrint Using "###"; statT0!(4);
    LPrint Tab(33);: LPrint Using "###"; statT0!(6);
    LPrint Tab(40);: LPrint Using "####"; statT0!(7);
    LPrint Tab(49);: LPrint Using "###"; statT0!(9);
    LPrint Tab(69);: LPrint Using " .###"; (statT0!(1) / statT0!(0));
    LPrint Tab(75);: LPrint Using " .###"; ((statT0!(1) + statT0!(4) + statT0!(5) * 2 + statT0!(6) * 3) / statT0!(0));
    LPrint Tab(81);: LPrint Using " .###"; ((statT0!(1) + statT0!(7)) / (statT0!(7) + statT0!(0)));
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
            LPrint Using " .###"; (statT0!(16) + statT0!(17)) / (statT0!(16) + statT0!(11) + statT0!(17));
        End If
    End If

    LPrint Tab(125); Using "###"; statT0!(19)
    LPrint
    LPrint
    TT$ = Str$(statT0!(2) / (W2% + L2%) + .05)
    UU$ = Str$(statT1!(2) / (W2% + L2%) + .05)
    LPrint "RUNS PER GAME ";: LPrint Left$(TT$, 4);
    LPrint " OPPONENTS";: LPrint Left$(UU$, 4)
    LPrint
    LPrint
    LPrint Tab(20); "RECORD         ("; Str$(totalWins); " -"; Str$(totalLosses); " )"
    LPrint Tab(20); "HOME RECORD    ("; Str$(homeWins); " -"; Str$(homeLosses); " )"
    LPrint Tab(20); "ROAD RECORD    ("; Str$(roadWins); " -"; Str$(roadLosses); " )"
    LPrint Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
    LPrint Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
    LPrint Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
    LPrint Chr$(12)
    LPrint teamName$; "    WON: "; W2%; " LOST: "; L2%
    LPrint "PITCHERS    W  L SV  IP     H  BB  SO  ERA    G GS CG RUN  ER ShO HR DP %RIP SH SF  RI RIS BS   SUP QS"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then
            LPrint Left$(pitcherNames$(I), 10);
            LPrint Tab(12);: LPrint Using "##"; statP0%(I, 6);
            LPrint Tab(15);: LPrint Using "##"; statP0%(I, 7);
            LPrint Tab(18);: LPrint Using "##"; statP0%(I, 12);
            LPrint Tab(21);: LPrint Using "###"; Int(statP0%(I, 0) / 3);

            If statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3);

            LPrint Tab(27);: LPrint Using "###"; statP0%(I, 1);
            LPrint Tab(31);: LPrint Using "###"; statP0%(I, 4);
            LPrint Tab(35);: LPrint Using "###"; statP0%(I, 5);
            I2 = 0

            If statP0%(I, 0) > 0 Then
                LPrint Tab(39);
                LPrint Using "##.##"; (statP0%(I, 3) * 9 / (statP0%(I, 0) / 3));
            Else
                '= 0
                LPrint Tab(39); " -.--";
            End If

            LPrint Tab(45);: LPrint Using "###"; statP0%(I, 8);
            LPrint Tab(49);: LPrint Using "##"; statP0%(I, 9);
            LPrint Tab(52);: LPrint Using "##"; statP0%(I, 10);
            LPrint Tab(55);: LPrint Using "###"; statP0%(I, 2);
            LPrint Tab(59);: LPrint Using "###"; statP0%(I, 3);
            LPrint Tab(63);: LPrint Using "###"; statP0%(I, 11);
            LPrint Tab(67);: LPrint Using "##"; statP0%(I, 14);
            LPrint Tab(70);: LPrint Using "##"; statP0%(I, 13);
            LPrint Tab(74);

            If statP%(I) > 0 Then
                LPrint Using "###"; Int((((statP0%(I, 0) / 3) * 100) / statP%(I)) + .5);
            Else
                '= 0
                LPrint "  ";
            End If

            LPrint Tab(78); Using "## ## ### ### ## "; statP0%(I, 15); statP0%(I, 16); statP0%(I, 17); statP0%(I, 18); statP0%(I, 19);

            If statP0%(I, 9) > 0 Then LPrint Using "##.## "; statP0%(I, 20) / statP0%(I, 9);

            LPrint Tab(101); Using "##"; statP0%(I, 21)

        End If

    Next

    LPrint
    LPrint "TOTALS";
    LPrint Tab(18);: LPrint Using "##"; statT1!(12);
    LPrint Tab(26);: LPrint Using "####"; statT1!(1);
    LPrint Tab(34);: LPrint Using "####"; statT1!(5);
    LPrint Tab(39);: LPrint Using "##.##"; statT1!(3) / (statT1!(0) / 3) * 9;
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
        If batterNames$(I) <> "XXX" And batterNames$(I) <> "" And statB0%(I, 21) + statB0%(I, 20) <> 0 Then
            LPrint Left$(batterNames$(I), 10); Tab(12); Using "###   ##   ###"; statB0%(I, 20) + statB0%(I, 21); statB0%(I, 21); statB0%(I, 21) / (statB0%(I, 20) + statB0%(I, 21)) * 100
        End If
    Next

    If splitScreenChoice% <> 0 Then
        'DISPLAY SPLIT STATS
        LPrint teamName$;
        LPrint Tab(15); "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"

        For I = 0 To 22

            If batterNames$(I) <> "XXX" And Left$(batterNames$(I), 1) <> " " And batterNames$(I) <> "" Then
                LPrint batterNames$(I);
                LPrint Tab(20); Using "### ## ## ## ### ### "; statSS%(I, 0); statSS%(I, 2); statSS%(I, 3); statSS%(I, 4); statSS%(I, 5); statSS%(I, 6);

                If statSS%(I, 0) = 0 Or statSS%(I, 1) = 0 Then
                    LPrint " .000";
                Else
                    If statSS%(I, 0) = statSS%(I, 1) Then
                        LPrint "1.000";
                    Else
                        LPrint Using " .###"; statSS%(I, 1) / statSS%(I, 0);
                    End If
                End If

                LPrint Tab(53); Using "### ## ## ## ### ### "; statB0%(I, 0) - statSS%(I, 0); statB0%(I, 4) - statSS%(I, 2); statB0%(I, 5) - statSS%(I, 3); statB0%(I, 6) - statSS%(I, 4); statB0%(I, 7) - statSS%(I, 5); statB0%(I, 8) - statSS%(I, 6);

                If statB0%(I, 0) - statSS%(I, 0) = 0 Or statB0%(I, 1) - statSS%(I, 1) = 0 Then
                    LPrint " .000"
                Else
                    If statB0%(I, 0) - statSS%(I, 0) = statB0%(I, 1) - statSS%(I, 1) Then
                        LPrint "1.000"
                    Else
                        LPrint Using " .###"; (statB0%(I, 1) - statSS%(I, 1)) / (statB0%(I, 0) - statSS%(I, 0))
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
Sub ResetStatPrompt (statFile$, teamRatings%())

    Cls

    Call MenuBox (6, 8, 61, 1, teamRatings%(11), teamRatings%(12))

    Locate 7

    Do
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        Call MenuBox (6, 8, 61, 1, teamRatings%(11), teamRatings%(12))

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
