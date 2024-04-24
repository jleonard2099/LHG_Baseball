'----------------------------------------
'            HD2HD Subroutine
'----------------------------------------
'This subroutine presents the user a menu
'of basic options for viewing Head-to-Head
'team comparisons.
Sub HD2HD ()

    _Title "Full Count Baseball - Head-to-Head Stats"

    ReDim menuChoices$(1 To 4)

    Do

        Color 15, 0
        Cls

        Call TITLE_UTILS

        menuChoices$(1) = "ANALYZE A TEAM'S RECORD                "
        menuChoices$(2) = "ANALYZE ANOTHER TEAM IN THE SAME LEAGUE"
        menuChoices$(3) = "SELECT A NEW TEAM AND LEAGUE           "
        menuChoices$(4) = "RETURN TO MAIN MENU                    "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%
                ' We need to pass league$ as a variable
                ' So that we can look at other teams
                ' in the same league (option 2)
                Case 1:
                    'Call Hd2HdPro_League(0)

                Case 2:
                    'Call Hd2HdPro_League(1)

                Case 3:
                    'Call Hd2HdPro_League(0)

            End Select

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'     Hd2HdPro_League Subroutine
'----------------------------------------
'This subroutine reads in stats from the
'selected team and then presents the user
'a menu of options for viewing Head-to-Head
'team comparisons.
Sub Hd2HdPro_League ()
    'Sub Hd2HdPro_League (sameLeague)

    Shared teamScore(), nbrInnings(), oppScore(), statZ1%()
    Shared awayLosses(), awayWins()
    Shared homeLosses(), homeWins()
    Shared homeScoreTeam(), homeScoreOpp(), awayScoreTeam(), awayScoreOpp()
    Shared totHomeScoreTeam(), totHomeScoreOpp(), totAwayScoreTeam(), totAwayScoreOpp()
    Shared totHomeWin(), totHomeLoss(), totAwayWin(), totAwayLoss()

    Shared DV$(), locIndicator$(), losePitcher$(), oppName$()
    Shared winPitcher$(), Z1$(), Z2$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    newTeam = 1

    Do

        If newTeam = 1 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    K$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

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

                    'Check if same league
                    If sameLeague <> 1 Then
                        leagueFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)
                    End If

                End If

            Else
                'If a team wasn't chosen, treat it like ESC
                ExitCode% = 27

            End If 'Done checking for valid team

        End If 'Done checking for new team

        Color 7, 0
        Cls

        If teamNbr > 0 And ExitCode% <> 27 And ExitCode% <> 0 Then

            Call MenuBox(8, 25, 24, 5, 15, 1)

            Do

                Locate 10
                Color 7, 1

                Locate , 32: Print "SEND TO ";
                Color 15: Print "F";
                Color 7: Print "ILE"
                Locate , 32: Print "SEND TO ";
                Color 15: Print "P";
                Color 7: Print "RINTER"
                Locate , 32: Print "SEND TO ";
                Color 15: Print "S";
                Color 7: Print "CREEN"

                J$ = GetKeyPress$

                Color 14, 0

            Loop Until UCase$(J$) = "F" Or UCase$(J$) = "P" Or UCase$(J$) = "S" Or J$ = Chr$(27)

            If J$ <> Chr$(27) Then

                If UCase$(J$) = "F" Then
                    printChoice = 0
                End If
                If UCase$(J$) = "P" Then
                    printChoice = 1
                End If
                If UCase$(J$) = "S" Then
                    VPP = 1
                Else
                    VPP = 0
                End If

                'Haven't checked that file exists
                Open diskPaths$(2) + leagueFile$ + ".TMI" For Input As #3

                Input #3, LG$
                Input #3, div1Name$
                Input #3, div2Name$
                Input #3, div3Name$
                Input #3, div4Name$

                For X = 1 To TEAMS_PER_LEAGUE
                    Input #3, Z1$(X)
                    Input #3, statZ1%(X)
                    Input #3, Z2$(X)
                Next

                Close #3

                Cls
                Color 15
                Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                'Initialization for 1st league
                Erase awayLosses, awayWins, homeLosses, homeWins
                Erase homeScoreTeam, homeScoreOpp, awayScoreTeam, awayScoreOpp
                Erase totHomeLoss, totHomeWin, totAwayLoss, totAwayWin
                Erase totHomeScoreTeam, totHomeScoreOpp, totAwayScoreTeam, totAwayScoreOpp

                'Calculations for 1st league
                For K = 1 To TEAMS_PER_LEAGUE

                    If Z1$(K) <> "XXX" Then

                        For I = 1 To nbrGames

                            If locIndicator$(I) = "A" And oppName$(I) = Z1$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    awayWins(K) = awayWins(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    awayLosses(K) = awayLosses(K) + 1
                                End If

                                awayScoreTeam(K) = awayScoreTeam(K) + teamScore(I)
                                awayScoreOpp(K) = awayScoreOpp(K) + oppScore(I)

                            End If

                            If locIndicator$(I) = "H" And oppName$(I) = Z1$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    homeWins(K) = homeWins(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    homeLosses(K) = homeLosses(K) + 1
                                End If

                                homeScoreTeam(K) = homeScoreTeam(K) + teamScore(I)
                                homeScoreOpp(K) = homeScoreOpp(K) + oppScore(I)

                            End If

                        Next I

                    End If

                Next K

                'Generate output
                If VPP = 0 Then
                    Call Hd2HdPro_Report(team$, printChoice)
                Else
                    Call Hd2HdPro_Screen(team$)
                End If

                Cls

                Call TITLE_UTILS

                'Call MenuBox(8, 23, 39, 5, 15, 1)
                'Call MenuBox(8, 15, 44, 6, 15, 1)
                Call MenuBox(8, 17, 40, 6, 15, 1)

                Do

                    Locate 10, 15

                    Color 3: Locate , 24: Print "SELECT OPTION:"
                    Color 7: Locate , 24: Print "VIEW ";
                    Color 15: Print "S";
                    Color 7: Print "AME TEAM AGAIN"
                    Color 7: Locate , 24: Print "VIEW ";
                    Color 15: Print "A";
                    Color 7: Print "NOTHER TEAM (SAME LEAGUE)"
                    Color 15: Locate , 24: Print "R";
                    Color 7: Print "ETURN TO OPTIONS"

                    K$ = GetKeyPress$

                    Select Case UCase$(K$)

                        Case "A"
                            newTeam = 1
                            sameLeague = 1
                            RDD = 0

                        Case "R"
                            newTeam = 0
                            sameLeague = 0
                            RDD = 0

                        Case "S"
                            newTeam = 0
                            sameLeague = 1
                            RDD = 0

                            'Re-initialize for the next loop
                            Erase totAwayLoss, totAwayWin, totHomeLoss, totHomeWin
                            Erase totHomeScoreTeam, totHomeScoreOpp, totAwayScoreTeam, totAwayScoreOpp

                    End Select

                Loop Until UCase$(K$) = "A" Or UCase$(K$) = "R" Or UCase$(K$) = "S" Or K$ = Chr$(27) Or J$ = Chr$(27)

            End If 'Done checking for stats file

        End If

    Loop Until UCase$(K$) = "R" Or K$ = Chr$(27) Or J$ = Chr$(27) Or ExitCode% = 27

End Sub


'----------------------------------------
'       Hd2HdPro_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the default printer
Sub Hd2HdPro_Report (teamName$, printDest)

    Shared awayLosses(), awayWins(), homeWins(), homeLosses()
    Shared homeScoreTeam(), homeScoreOpp(), awayScoreTeam(), awayScoreOpp(), totAwayWin(), totHomeWin()

    Shared DV$(), Z1$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    If printDest = 0 Then
        destFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
    Else
        destFile$ = "Comparison"
    End If

    DV$(1) = div1Name$
    DV$(2) = div2Name$
    DV$(3) = div3Name$
    DV$(4) = div4Name$

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    RDD = 1

    Do

        Print #1, Tab(5); teamName$
        Print #1, Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOtotAwayLossesS -------"
        'Print #1, Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOtotAwayLossesS "; String$(6, 196)
        Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"
        'Print #1, Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

        DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
        EE = RDD * TEAMS_PER_DIV

        'If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

        Print #1, Tab(2); DV$(RDD)

        For K = DD To EE

            If Z1$(K) <> "XXX" And Z1$(K) <> T$ Then

                Print #1, Z1$(K);
                Print #1, Tab(21); Using "##"; homeWins(K);
                Print #1, Tab(25); Using "##"; homeLosses(K);

                If homeWins(K) = homeWins(K) + homeLosses(K) And homeWins(K) <> 0 Then Print #1, Tab(28); "1.000";
                If homeWins(K) = 0 Then Print #1, Tab(29); ".000";
                If homeWins(K) > 0 And homeWins(K) < homeWins(K) + homeLosses(K) Then
                    statPCT! = (homeWins(K) / (homeWins(K) + homeLosses(K)))
                    Print #1, Tab(29); Using ".###"; statPCT!;
                End If

                Print #1, Tab(35); Using "###"; homeScoreTeam(K);
                Print #1, Tab(39); Using "###"; homeScoreOpp(K);
                Print #1, Tab(53); Using "##"; homeWins(K);
                Print #1, Tab(57); Using "##"; awayLosses(K);

                If homeWins(K) = homeWins(K) + awayLosses(K) And homeWins(K) <> 0 Then Print #1, Tab(60); "1.000";
                If homeWins(K) = 0 Then Print #1, Tab(61); ".000";
                If homeWins(K) > 0 And homeWins(K) < homeWins(K) + awayLosses(K) Then
                    statPCT! = (homeWins(K) / (homeWins(K) + awayLosses(K)))
                    Print #1, Tab(61); Using ".###"; statPCT!;
                End If

                Print #1, Tab(67); Using "###"; awayScoreTeam(K);
                Print #1, Tab(71); Using "###"; awayScoreOpp(K);
                Print #1, Tab(85); Using "##"; homeWins(K) + homeWins(K);
                Print #1, Tab(90); Using "##"; homeLosses(K) + awayLosses(K);

                If homeWins(K) + homeWins(K) = homeWins(K) + homeWins(K) + homeLosses(K) + awayLosses(K) And homeWins(K) + homeWins(K) <> 0 Then Print #1, Tab(95); "1.000";
                If (homeWins(K) + homeWins(K)) = 0 Then Print #1, Tab(96); ".000";
                If (homeWins(K) + homeWins(K)) > 0 And (homeWins(K) + homeWins(K)) < (homeWins(K) + homeWins(K) + homeLosses(K) + awayLosses(K)) Then
                    statPCT! = ((homeWins(K) + homeWins(K)) / (homeWins(K) + homeWins(K) + homeLosses(K) + awayLosses(K)))
                    Print #1, Tab(96); Using ".###"; statPCT!;
                End If

                Print #1, Tab(102); Using "###"; homeScoreTeam(K) + awayScoreTeam(K);
                Print #1, Tab(106); Using "###"; homeScoreOpp(K) + awayScoreOpp(K)

                totHomeWin(RDD) = totHomeWin(RDD) + homeWins(K)
                totHomeLoss(RDD) = totHomeLoss(RDD) + homeLosses(K)
                totAwayWin(RDD) = totAwayWin(RDD) + awayWins(K)
                totAwayLoss(RDD) = totAwayLoss(RDD) + awayLosses(K)
                totHomeScoreTeam(RDD) = totHomeScoreTeam(RDD) + homeScoreTeam(K)
                totHomeScoreOpp(RDD) = totHomeScoreOpp(RDD) + homeScoreOpp(K)
                totAwayScoreTeam(RDD) = totAwayScoreTeam(RDD) + awayScoreTeam(K)
                totAwayScoreOpp(RDD) = totAwayScoreOpp(RDD) + awayScoreOpp(K)

            End If

        Next K

        Print #1, Tab(5); "TOtotAwayLossesS";
        Print #1, Tab(21); Using "##"; totHomeWin(RDD);
        Print #1, Tab(25); Using "##"; totHomeLoss(RDD);

        If totHomeWin(RDD) = totHomeWin(RDD) + totHomeLoss(RDD) And totHomeWin(RDD) <> 0 Then Print #1, Tab(28); "1.000";
        If totHomeWin(RDD) = 0 Then Print #1, Tab(29); ".000";
        If totHomeWin(RDD) > 0 And totHomeWin(RDD) < totHomeWin(RDD) + totHomeLoss(RDD) Then
            statPCT! = (totHomeWin(RDD) / (totHomeWin(RDD) + totHomeLoss(RDD)))
            Print #1, Tab(29); Using ".###"; statPCT!;
        End If

        Print #1, Tab(35); Using "###"; totHomeScoreTeam(RDD);
        Print #1, Tab(39); Using "###"; totHomeScoreOpp(RDD);
        Print #1, Tab(53); Using "##"; totAwayWin(RDD);
        Print #1, Tab(57); Using "##"; totAwayLoss(RDD);

        If totAwayWin(RDD) = totAwayWin(RDD) + totAwayLoss(RDD) And totAwayWin(RDD) <> 0 Then Print #1, Tab(60); "1.000";
        If totAwayWin(RDD) = 0 Then Print #1, Tab(61); ".000";
        If totAwayWin(RDD) > 0 And totAwayWin(RDD) < totAwayWin(RDD) + totAwayLoss(RDD) Then
            statPCT! = (totAwayWin(RDD) / (totAwayWin(RDD) + totAwayLoss(RDD)))
            Print #1, Tab(61); Using ".###"; statPCT!;
        End If

        Print #1, Tab(67); Using "###"; totAwayScoreTeam(RDD);
        Print #1, Tab(71); Using "###"; totAwayScoreOpp(RDD);
        Print #1, Tab(84); Using "###"; totHomeWin(RDD) + totAwayWin(RDD);
        Print #1, Tab(89); Using "###"; totHomeLoss(RDD) + totAwayLoss(RDD);

        If totHomeWin(RDD) + totAwayWin(RDD) = totHomeWin(RDD) + totAwayWin(RDD) + totHomeLoss(RDD) + totAwayLoss(RDD) And totHomeWin(RDD) + totAwayWin(RDD) <> 0 Then Print #1, Tab(95); "1.000";
        If totHomeWin(RDD) + totAwayWin(RDD) = 0 Then Print #1, Tab(96); ".000";
        If totHomeWin(RDD) + totAwayWin(RDD) > 0 And totHomeWin(RDD) + totAwayWin(RDD) < totHomeWin(RDD) + totAwayWin(RDD) + totHomeLoss(RDD) + totAwayLoss(RDD) Then
            statPCT! = ((totHomeWin(RDD) + totAwayWin(RDD)) / (totHomeWin(RDD) + totAwayWin(RDD) + totHomeLoss(RDD) + totAwayLoss(RDD)))
            Print #1, Tab(96); Using ".###"; statPCT!;
        End If

        Print #1, Tab(102); Using "###"; totHomeScoreTeam(RDD) + totAwayScoreTeam(RDD);
        Print #1, Tab(106); Using "###"; totHomeScoreOpp(RDD) + totAwayScoreOpp(RDD)
        Print #1,

        RDD = RDD + 1

    Loop Until RDD > DIVS_PER_LEAGUE Or DV$(RDD) = "XXX" Or DV$(RDD) = ""

    Print #1,
    Print #1,
    Print #1,
    Print #1,
    Print #1, Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOtotAwayLossesS -------"
    Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"

    Print

    For W = 1 To DIVS_PER_LEAGUE

        If DV$(W) <> "XXX" Then

            Print #1, Tab(2); DV$(W);
            Print #1, Tab(21); Using "##"; totHomeWin(W);
            Print #1, Tab(25); Using "##"; totHomeLoss(W);

            If totHomeWin(W) = totHomeWin(W) + totHomeLoss(W) And totHomeWin(W) <> 0 Then Print #1, Tab(28); "1.000";
            If totHomeWin(W) = 0 Then Print #1, Tab(29); ".000";
            If totHomeWin(W) > 0 And totHomeWin(W) < totHomeWin(W) + totHomeLoss(W) Then
                statPCT! = (totHomeWin(W) / (totHomeWin(W) + totHomeLoss(W)))
                Print #1, Tab(29); Using ".###"; statPCT!;
            End If

            Print #1, Tab(35); Using "###"; totHomeScoreTeam(W);
            Print #1, Tab(39); Using "###"; totHomeScoreOpp(W);
            Print #1, Tab(53); Using "##"; totHomeWin(W);
            Print #1, Tab(57); Using "##"; totAwayLoss(W);

            If totHomeWin(W) = totHomeWin(W) + totAwayLoss(W) And totHomeWin(W) <> 0 Then Print #1, Tab(60); "1.000";
            If totHomeWin(W) = 0 Then Print #1, Tab(61); ".000";
            If totHomeWin(W) > 0 And totHomeWin(W) < totHomeWin(W) + totAwayLoss(W) Then
                statPCT! = (totHomeWin(W) / (totHomeWin(W) + totAwayLoss(W)))
                Print #1, Tab(61); Using ".###"; statPCT!;
            End If

            Print #1, Tab(67); Using "###"; totAwayScoreTeam(W);
            Print #1, Tab(71); Using "###"; totAwayScoreOpp(W);
            Print #1, Tab(84); Using "###"; totHomeWin(W) + totHomeWin(W);
            Print #1, Tab(89); Using "###"; totHomeLoss(W) + totAwayLoss(W);

            If totHomeWin(W) + totHomeWin(W) = totHomeWin(W) + totHomeWin(W) + totHomeLoss(W) + totAwayLoss(W) And totHomeWin(W) + totHomeWin(W) <> 0 Then Print #1, Tab(95); "1.000";
            If (totHomeWin(W) + totHomeWin(W)) = 0 Then Print #1, Tab(96); ".000";
            If (totHomeWin(W) + totHomeWin(W)) > 0 And totHomeWin(W) + totHomeWin(W) < totHomeWin(W) + totHomeWin(W) + totHomeLoss(W) + totAwayLoss(W) Then
                statPCT! = ((totHomeWin(W) + totHomeWin(W)) / (totHomeWin(W) + totHomeWin(W) + totHomeLoss(W) + totAwayLoss(W)))
                Print #1, Tab(96); Using ".###"; statPCT!;
            End If

            Print #1, Tab(102); Using "###"; totHomeScoreTeam(W) + totAwayScoreTeam(W);
            Print #1, Tab(106); Using "###"; totHomeScoreOpp(W) + totAwayScoreOpp(W)

        End If

    Next W

    THWW = 0
    totHomeLossesL = 0
    TAWW = 0
    totAwayLossesL = 0

    For I = 1 To 4
        THWW = THWW + totHomeWin(I)
        totHomeLossesL = totHomeLossesL + totHomeLoss(I)
        TAWW = TAWW + totAwayWin(I)
        totAwayLossesL = totAwayLossesL + totAwayLoss(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_LEAGUE
        TR11 = TR11 + totHomeScoreTeam(I)
        TR22 = TR22 + totHomeScoreOpp(I)
        TR33 = TR33 + totAwayScoreTeam(I)
        TR44 = TR44 + totAwayScoreOpp(I)
    Next

    Print #1, Tab(5); "TOtotAwayLossesS";
    Print #1, Tab(20); Using "###"; THWW;
    Print #1, Tab(24); Using "###"; totHomeLossesL;

    If THWW = THWW + totHomeLossesL And THWW <> 0 Then Print #1, Tab(28); "1.000";
    If THWW = 0 Then Print #1, Tab(29); ".000";
    If THWW > 0 And THWW < THWW + totHomeLossesL Then
        statPCT! = THWW / (THWW + totHomeLossesL)
        Print #1, Tab(29); Using ".###"; statPCT!;
    End If

    Print #1, Tab(35); Using "###"; TR11;
    Print #1, Tab(39); Using "###"; TR22;
    Print #1, Tab(52); Using "###"; TAWW;
    Print #1, Tab(56); Using "###"; totAwayLossesL;

    If TAWW = TAWW + totAwayLossesL And TAWW <> 0 Then Print #1, Tab(60); "1.000";
    If TAWW = 0 Then Print #1, Tab(61); ".000";
    If TAWW > 0 And TAWW < TAWW + totAwayLossesL Then
        statPCT! = TAWW / (TAWW + totAwayLossesL)
        Print #1, Tab(61); Using ".###"; statPCT!;
    End If

    Print #1, Tab(67); Using "###"; TR33;
    Print #1, Tab(71); Using "###"; TR44;
    Print #1, Tab(84); Using "###"; THWW + TAWW;
    Print #1, Tab(89); Using "###"; totHomeLossesL + totAwayLossesL;

    If THWW + TAWW = THWW + TAWW + totHomeLossesL + totAwayLossesL And THWW + TAWW <> 0 Then Print #1, Tab(95); "1.000";
    If THWW + TAWW = 0 Then Print #1, Tab(96); ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + totHomeLossesL + totAwayLossesL Then
        statPCT! = (THWW + TAWW) / (THWW + TAWW + totHomeLossesL + totAwayLossesL)
        Print #1, Tab(96); Using ".###"; statPCT!;
    End If

    Print #1, Tab(102); Using "###"; TR11 + TR33;
    Print #1, Tab(106); Using "###"; TR22 + TR44

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
'       Hd2HdPro_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the end user display
Sub Hd2HdPro_Screen (teamName$)

    Shared homeWins(), awayWins(), homeLosses(), awayLosses()
    Shared homeScoreTeam(), homeScoreOpp(), awayScoreTeam(), awayScoreOpp()
    Shared teamRatings%()

    Shared DV$(), Z1$()

    Shared totHomeScoreTeam(), totHomeScoreOpp(), totAwayScoreTeam(), totAwayScoreOpp(), totHomeWin(), totHomeLoss(), totAwayWin(), totAwayLoss()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    DV$(1) = div1Name$
    DV$(2) = div2Name$
    DV$(3) = div3Name$
    DV$(4) = div4Name$

    RDD = 1

    Do

        Cls

        Locate 2, 35
        Color teamRatings%(11), teamRatings%(12): Print teamName$
        Color 3, 0

        Locate 4
        Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOtotAwayLossesS "; String$(6, 196)
        Locate 5
        Print Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

        DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
        EE = RDD * TEAMS_PER_DIV

        Color 11
        Print Left$(DV$(RDD), 15);

        Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

        For K = DD To EE

            If Z1$(K) <> "XXX" And Z1$(K) <> teamName$ And Z1$(K) <> "" Then

                Color 15
                Locate , 1: Print Left$(Z1$(K), 13);

                Color 7

                Locate , 15: Print Using "##"; homeWins(K);
                Locate , 18: Print Using "##"; homeLosses(K);

                If homeWins(K) = homeWins(K) + homeLosses(K) And homeWins(K) <> 0 Then Locate , 21: Print "1.000";
                If homeWins(K) = 0 Then Locate , 22: Print ".000";
                If homeWins(K) > 0 And homeWins(K) < homeWins(K) + homeLosses(K) Then
                    statPCT! = (homeWins(K) / (homeWins(K) + homeLosses(K)))
                    Locate , 22: Print Using ".###"; statPCT!;
                End If

                Locate , 27: Print Using "###"; homeScoreTeam(K);
                Locate , 31: Print Using "###"; homeScoreOpp(K);

                Color 3
                Print Tab(35); Chr$(179);

                Color 7

                Locate , 37: Print Using "##"; homeWins(K);
                Locate , 40: Print Using "##"; awayLosses(K);

                If homeWins(K) = homeWins(K) + awayLosses(K) And homeWins(K) <> 0 Then Locate , 43: Print "1.000";
                If homeWins(K) = 0 Then Locate , 44: Print ".000";
                If homeWins(K) > 0 And homeWins(K) < homeWins(K) + awayLosses(K) Then
                    statPCT! = (homeWins(K) / (homeWins(K) + awayLosses(K)))
                    Locate , 44: Print Using ".###"; statPCT!;
                End If

                Locate , 49: Print Using "###"; awayScoreTeam(K);
                Locate , 53: Print Using "###"; awayScoreOpp(K);

                Color 3
                Print Tab(58); Chr$(179);

                Color 7

                Locate , 60: Print Using "##"; homeWins(K) + homeWins(K);
                Locate , 63: Print Using "##"; homeLosses(K) + awayLosses(K);

                If (homeWins(K) + homeWins(K)) = (homeWins(K) + homeWins(K) + homeLosses(K) + awayLosses(K)) And (homeWins(K) + homeWins(K)) <> 0 Then Locate , 66: Print "1.000";
                If (homeWins(K) + homeWins(K)) = 0 Then Locate , 67: Print ".000";
                If (homeWins(K) + homeWins(K)) > 0 And (homeWins(K) + homeWins(K)) < (homeWins(K) + homeWins(K) + homeLosses(K) + awayLosses(K)) Then
                    statPCT! = ((homeWins(K) + homeWins(K)) / (homeWins(K) + homeWins(K) + homeLosses(K) + awayLosses(K)))
                    Locate , 67: Print Using ".###"; statPCT!;
                End If

                Locate , 72: Print Using "###"; homeScoreTeam(K) + awayScoreTeam(K);
                Locate , 76: Print Using "###"; homeScoreOpp(K) + awayScoreOpp(K)

                Color 3
                Print Tab(35); Chr$(179); Tab(58); Chr$(179);

                totHomeWin(RDD) = totHomeWin(RDD) + homeWins(K)
                totHomeLoss(RDD) = totHomeLoss(RDD) + homeLosses(K)
                totAwayWin(RDD) = totAwayWin(RDD) + awayWins(K)
                totAwayLoss(RDD) = totAwayLoss(RDD) + awayLosses(K)
                totHomeScoreTeam(RDD) = totHomeScoreTeam(RDD) + homeScoreTeam(K)
                totHomeScoreOpp(RDD) = totHomeScoreOpp(RDD) + homeScoreOpp(K)
                totAwayScoreTeam(RDD) = totAwayScoreTeam(RDD) + awayScoreTeam(K)
                totAwayScoreOpp(RDD) = totAwayScoreOpp(RDD) + awayScoreOpp(K)

            End If

        Next K

        Print

        Color 11

        Locate , 5: Print "TOtotAwayLossesS";
        Locate , 15: Print Using "##"; totHomeWin(RDD);
        Locate , 18: Print Using "##"; totHomeLoss(RDD);

        If totHomeWin(RDD) = totHomeWin(RDD) + totHomeLoss(RDD) And totHomeWin(RDD) <> 0 Then Locate , 21: Print "1.000";
        If totHomeWin(RDD) = 0 Then Locate , 22: Print ".000";
        If totHomeWin(RDD) > 0 And totHomeWin(RDD) < totHomeWin(RDD) + totHomeLoss(RDD) Then
            statPCT! = (totHomeWin(RDD) / (totHomeWin(RDD) + totHomeLoss(RDD)))
            Locate , 22: Print Using ".###"; statPCT!;
        End If

        Locate , 27: Print Using "###"; totHomeScoreTeam(RDD);
        Locate , 31: Print Using "###"; totHomeScoreOpp(RDD);

        Color 3
        Print Tab(35); Chr$(179);

        Color 11
        Locate , 37: Print Using "##"; totAwayWin(RDD);
        Locate , 40: Print Using "##"; totAwayLoss(RDD);

        If totAwayWin(RDD) = totAwayWin(RDD) + totAwayLoss(RDD) And totAwayWin(RDD) <> 0 Then Locate , 43: Print "1.000";
        If totAwayWin(RDD) = 0 Then Locate , 44: Print ".000";
        If totAwayWin(RDD) > 0 And totAwayWin(RDD) < totAwayWin(RDD) + totAwayLoss(RDD) Then
            statPCT! = (totAwayWin(RDD) / (totAwayWin(RDD) + totAwayLoss(RDD)))
            Locate , 44: Print Using ".###"; statPCT!;
        End If

        Locate , 49: Print Using "###"; totAwayScoreTeam(RDD);
        Locate , 53: Print Using "###"; totAwayScoreOpp(RDD);

        Color 3
        Print Tab(58); Chr$(179);

        Color 11

        Locate , 59: Print Using "###"; totHomeWin(RDD) + totAwayWin(RDD);
        Locate , 62: Print Using "###"; totHomeLoss(RDD) + totAwayLoss(RDD);

        If totHomeWin(RDD) + totAwayWin(RDD) = totHomeWin(RDD) + totHomeLoss(RDD) + totAwayWin(RDD) + totAwayLoss(RDD) And totHomeWin(RDD) + totAwayWin(RDD) <> 0 Then Locate , 66: Print "1.000";
        If totHomeWin(RDD) + totAwayWin(RDD) = 0 Then Locate , 67: Print ".000";
        If totHomeWin(RDD) + totAwayWin(RDD) > 0 And totHomeWin(RDD) + totAwayWin(RDD) < totHomeWin(RDD) + totAwayWin(RDD) + totHomeLoss(RDD) + totAwayLoss(RDD) Then
            statPCT! = (totHomeWin(RDD) + totAwayWin(RDD)) / (totHomeWin(RDD) + totAwayWin(RDD) + totHomeLoss(RDD) + totAwayLoss(RDD))
            Locate , 67: Print Using ".###"; statPCT!;
        End If

        Locate , 72: Print Using "###"; totHomeScoreTeam(RDD) + totAwayScoreTeam(RDD);
        Locate , 76: Print Using "###"; totHomeScoreOpp(RDD) + totAwayScoreOpp(RDD)

        Color 14
        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

        I$ = GetKeyPress$

        RDD = RDD + 1

    Loop Until RDD > DIVS_PER_LEAGUE Or DV$(RDD) = "XXX" Or DV$(RDD) = ""

    Cls

    Locate 2, 35
    Color teamRatings%(11), teamRatings%(12): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOtotAwayLossesS "; String$(6, 196)
    Locate 5
    Print Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

    Color 11: Print "DIVISIONS";

    Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

    For W = 1 To DIVS_PER_LEAGUE

        If DV$(W) <> "XXX" Then

            Color 15
            'We have to limit this to 14 because we start stats on column 15
            Locate , 1: Print Left$(DV$(W), 14);

            Color 7

            Locate , 15: Print Using "##"; totHomeWin(W);
            Locate , 18: Print Using "##"; totHomeLoss(W);

            If totHomeWin(W) = totHomeWin(W) + totHomeLoss(W) And totHomeWin(W) <> 0 Then Locate , 21: Print "1.000";
            If totHomeWin(W) = 0 Then Locate , 22: Print ".000";
            If totHomeWin(W) > 0 And totHomeWin(W) < totHomeWin(W) + totHomeLoss(W) Then
                statPCT! = (totHomeWin(W) / (totHomeWin(W) + totHomeLoss(W)))
                Locate , 22: Print Using ".###"; statPCT!;
            End If

            Locate , 27: Print Using "###"; totHomeScoreTeam(W);
            Locate , 31: Print Using "###"; totHomeScoreOpp(W);

            Color 3
            Print Tab(35); Chr$(179);

            Color 7

            Locate , 37: Print Using "##"; totAwayWin(W);
            Locate , 40: Print Using "##"; totAwayLoss(W);

            If totAwayWin(W) = totAwayWin(W) + totAwayLoss(W) And totAwayWin(W) <> 0 Then Locate , 43: Print "1.000";
            If totAwayWin(W) = 0 Then Locate , 44: Print ".000";
            If totAwayWin(W) > 0 And totAwayWin(W) < totAwayWin(W) + totAwayLoss(W) Then
                statPCT! = (totAwayWin(W) / (totAwayWin(W) + totAwayLoss(W)))
                Locate , 44: Print Using ".###"; statPCT!;
            End If

            Locate , 49: Print Using "###"; totAwayScoreTeam(W);
            Locate , 53: Print Using "###"; totAwayScoreOpp(W);

            Color 3: Print Tab(58); Chr$(179);

            Color 7
            Locate , 59: Print Using "###"; totHomeWin(W) + totAwayWin(W);
            Locate , 62: Print Using "###"; totHomeLoss(W) + totAwayLoss(W);

            If totHomeWin(W) + totAwayWin(W) = totHomeWin(W) + totAwayWin(W) + totHomeLoss(W) + totAwayLoss(W) And totHomeWin(W) + totAwayWin(W) <> 0 Then Locate , 66: Print "1.000";
            If (totHomeWin(W) + totAwayWin(W)) = 0 Then Locate , 67: Print ".000";
            If (totHomeWin(W) + totAwayWin(W)) > 0 And totHomeWin(W) + totAwayWin(W) < totHomeWin(W) + totAwayWin(W) + totHomeLoss(W) + totAwayLoss(W) Then
                statPCT! = ((totHomeWin(W) + totAwayWin(W)) / (totHomeWin(W) + totAwayWin(W) + totHomeLoss(W) + totAwayLoss(W)))
                Locate , 67: Print Using ".###"; statPCT!;
            End If

            Locate , 72: Print Using "###"; totHomeScoreTeam(W) + totAwayScoreTeam(W);
            Locate , 76: Print Using "###"; totHomeScoreOpp(W) + totAwayScoreOpp(W)

            Color 3
            Print Tab(35); Chr$(179); Tab(58); Chr$(179);

        End If

    Next W

    THWW = 0
    totHomeLossesL = 0
    TAWW = 0
    totAwayLossesL = 0

    For I = 1 To DIVS_PER_LEAGUE
        THWW = THWW + totHomeWin(I)
        totHomeLossesL = totHomeLossesL + totHomeLoss(I)
        TAWW = TAWW + totHomeWin(I)
        totAwayLossesL = totAwayLossesL + totAwayLoss(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_LEAGUE
        TR11 = TR11 + totHomeScoreTeam(I)
        TR22 = TR22 + totHomeScoreOpp(I)
        TR33 = TR33 + totAwayScoreTeam(I)
        TR44 = TR44 + totAwayScoreOpp(I)
    Next

    Print

    Color 11: Locate , 3: Print "TOtotAwayLossesS";
    Locate , 14: Print Using "###"; THWW;
    Locate , 17: Print Using "###"; totHomeLossesL;

    If THWW = THWW + totHomeLossesL And THWW <> 0 Then Locate , 21: Print "1.000";
    If THWW = 0 Then Locate , 22: Print ".000";
    If THWW > 0 And THWW < THWW + totHomeLossesL Then
        statPCT! = THWW / (THWW + totHomeLossesL)
        Locate , 22: Print Using ".###"; statPCT!;
    End If

    Locate , 27: Print Using "###"; TR11;
    Locate , 31: Print Using "###"; TR22;
    Color 3: Print Tab(35); Chr$(179);
    Color 11: Locate , 36: Print Using "###"; TAWW;
    Locate , 39: Print Using "###"; totAwayLossesL;

    If TAWW = TAWW + totAwayLossesL And TAWW <> 0 Then Locate , 43: Print "1.000";
    If TAWW = 0 Then Locate , 44: Print ".000";
    If TAWW > 0 And TAWW < TAWW + totAwayLossesL Then
        statPCT! = TAWW / (TAWW + totAwayLossesL)
        Locate , 44: Print Using ".###"; statPCT!;
    End If

    Locate , 49: Print Using "###"; TR33;
    Locate , 53: Print Using "###"; TR44;
    Color 3: Print Tab(58); Chr$(179);
    Color 11: Locate , 59: Print Using "###"; THWW + TAWW;
    Locate , 62: Print Using "###"; totHomeLossesL + totAwayLossesL;

    If THWW + TAWW = THWW + TAWW + totHomeLossesL + totAwayLossesL And THWW + TAWW <> 0 Then Locate , 66: Print "1.000";
    If THWW + TAWW = 0 Then Locate , 67: Print ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + totHomeLossesL + totAwayLossesL Then
        statPCT! = (THWW + TAWW) / (THWW + TAWW + totHomeLossesL + totAwayLossesL)
        Locate , 67: Print Using ".###"; statPCT!;
    End If

    Locate , 72: Print Using "###"; TR11 + TR33;
    Locate , 76: Print Using "###"; TR22 + TR44

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

End Sub


