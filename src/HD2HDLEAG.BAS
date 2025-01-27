'----------------------------------------
'            HD2HD Subroutine
'----------------------------------------
'This subroutine reads in stats from the
'selected team and then presents the user
'a menu of options for viewing Head-to-Head
'team comparisons.
Sub HD2HDLEAG ()

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
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%
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

    Loop Until SELECTION% = 4 Or ExitCode% = 27

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

    Shared memberIdx(), teamScore(), oppScore(), nbrInnings()

    Shared awayWins!(), awayLosses!() 
    Shared homeWins!(), homeLosses!()
    Shared awayScoreTeam!(), awayScoreOpp!()
    Shared homeScoreTeam!(), homeScoreOpp!()
    shared totAwayWins!(), totAwayLosses!()
    Shared totHomeWins!(), totHomeLosses!()
    Shared totAwayScoreTeam!(), totAwayScoreOpp!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!()

    Shared divisionNames$()
    Shared locIndicator$(), oppName$()
    Shared memberName$(), memberYear$()
    Shared losePitcher$(), winPitcher$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    newTeam = 1

    Do

        If newTeam = 1 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    K$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Call ReadScheduleStats (statFile$, nbrGames)

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

            userKey$ = GetOutputOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"                   
                        printChoice = 0
                        VPP = 0
                    Case "P"
                        printChoice = 1
                        VPP = 0
                    Case "S"
                        VPP = 1

                End Select

                'Haven't checked that file exists
                Call ReadLeagueIndex (leagueFile$)

                Cls
                Color 15
                Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                'Initialization for 1st league
                Erase awayWins!, awayLosses!
                Erase homeWins!, homeLosses!
                Erase awayScoreTeam!, awayScoreOpp!
                Erase homeScoreTeam!, homeScoreOpp!
                Erase totAwayWins!, totAwayLosses!
                Erase totHomeWins!, totHomeLosses!
                Erase totAwayScoreTeam!, totAwayScoreOpp!
                Erase totHomeScoreTeam!, totHomeScoreOpp!

                'Calculations for 1st league
                For K = 1 To TEAMS_PER_LEAGUE

                    If memberName$(K) <> "XXX" Then

                        For I = 1 To nbrGames

                            If Left$(locIndicator$(I), 1) = "V" And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    awayWins!(K) = awayWins!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    awayLosses!(K) = awayLosses!(K) + 1
                                End If

                                awayScoreTeam!(K) = awayScoreTeam!(K) + teamScore(I)
                                awayScoreOpp!(K) = awayScoreOpp!(K) + oppScore(I)

                            End If

                            If locIndicator$(I) = "H" And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    homeWins!(K) = homeWins!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    homeLosses!(K) = homeLosses!(K) + 1
                                End If

                                homeScoreTeam!(K) = homeScoreTeam!(K) + teamScore(I)
                                homeScoreOpp!(K) = homeScoreOpp!(K) + oppScore(I)

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
                            Erase totAwayLosses!, totAwayWins!
                            Erase totHomeLosses!, totHomeWins!
                            Erase totAwayScoreTeam!, totAwayScoreOpp!
                            Erase totHomeScoreTeam!, totHomeScoreOpp!

                    End Select

                Loop Until UCase$(K$) = "A" Or UCase$(K$) = "R" Or UCase$(K$) = "S" Or K$ = Chr$(27)

            End If 'Done checking for stats file

        End If

    Loop Until UCase$(K$) = "R" Or K$ = Chr$(27) Or userKey$ = Chr$(27) Or ExitCode% = 27

End Sub


'----------------------------------------
'       Hd2HdPro_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the default printer
Sub Hd2HdPro_Report (teamName$, printDest)

    Shared awayWins!(), awayLosses!()
    Shared homeWins!(), homeLosses!()
    Shared awayScoreTeam!(), awayScoreOpp!()
    Shared homeScoreTeam!(), homeScoreOpp!()
    Shared totAwayLosses!(), totAwayWins!()
    Shared totHomeLosses!(), totHomeWins!()
    Shared totAwayScoreTeam!(), totAwayScoreOpp!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!()

    Shared divisionNames$(), memberName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    If printDest = 0 Then
        destFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
    Else
        destFile$ = "Comparison"
    End If

    Color 15, 0
    Cls

    divisionNames$(1) = div1Name$
    divisionNames$(2) = div2Name$
    divisionNames$(3) = div3Name$
    divisionNames$(4) = div4Name$

    Open destFile$ For Output As #1

    div = 0

    Do

        If divisionNames$(RDD) <> "XXX" And divisionNames$(RDD) <> "*" And divisionNames$(RDD) <> "" Then
        
            RDD = div + 1

            Print #1, Tab(47); teamName$
            Print #1,
            Print #1, Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOTALS -------"
            Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; 
            Print #1, Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; 
            Print #1, Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"

            DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
            EE = RDD * TEAMS_PER_DIV

            Print #1, Tab(2); divisionNames$(RDD)

            For K = DD To EE

                If memberName$(K) <> "XXX" And memberName$(K) <> "*" And memberName$(K) <> teamName$ Then

                    Print #1, memberName$(K);
                    Print #1, Tab(21); Using "##"; homeWins!(K);
                    Print #1, Tab(25); Using "##"; homeLosses!(K);

                    If homeWins!(K) = homeWins!(K) + homeLosses!(K) And homeWins!(K) <> 0 Then 
                        Print #1, Tab(28); "1.000";
                    End If
                    If homeWins!(K) = 0 Then 
                        Print #1, Tab(29); ".000";
                    End If
                    If homeWins!(K) > 0 And homeWins!(K) < homeWins!(K) + homeLosses!(K) Then
                        statPCT! = (homeWins!(K) / (homeWins!(K) + homeLosses!(K)))
                        Print #1, Tab(29); Using ".###"; statPCT!;
                    End If

                    Print #1, Tab(35); Using "###"; homeScoreTeam!(K);
                    Print #1, Tab(39); Using "###"; homeScoreOpp!(K);

                    Print #1, Tab(53); Using "##"; awayWins!(K);
                    Print #1, Tab(57); Using "##"; awayLosses!(K);

                    If awayWins!(K) = awayWins!(K) + awayLosses!(K) And awayWins!(K) <> 0 Then
                        Print #1, Tab(60); "1.000";
                    End If
                    If awayWins!(K) = 0 Then
                        Print #1, Tab(61); ".000";
                    End If
                    If awayWins!(K) > 0 And awayWins!(K) < awayWins!(K) + awayLosses!(K) Then
                        statPCT! = (awayWins!(K) / (awayWins!(K) + awayLosses!(K)))
                        Print #1, Tab(61); Using ".###"; statPCT!;
                    End If

                    Print #1, Tab(67); Using "###"; awayScoreTeam!(K);
                    Print #1, Tab(71); Using "###"; awayScoreOpp!(K);

                    Print #1, Tab(85); Using "##"; homeWins!(K) + awayWins!(K);
                    Print #1, Tab(90); Using "##"; homeLosses!(K) + awayLosses!(K);

                    If homeWins!(K) + awayWins!(K) = homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses!(K) And homeWins!(K) + awayWins!(K) <> 0 Then 
                        Print #1, Tab(95); "1.000";
                    End If
                    If (homeWins!(K) + awayWins!(K)) = 0 Then 
                        Print #1, Tab(96); ".000";
                    End If
                    If (homeWins!(K) + awayWins!(K)) > 0 And (homeWins!(K) + awayWins!(K)) < (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses!(K)) Then
                        statPCT! = ((homeWins!(K) + awayWins!(K)) / (awayWins!(K) + homeWins!(K) + homeLosses!(K) + awayLosses!(K)))
                        Print #1, Tab(96); Using ".###"; statPCT!;
                    End If

                    Print #1, Tab(102); Using "###"; homeScoreTeam!(K) + awayScoreTeam!(K);
                    Print #1, Tab(106); Using "###"; homeScoreOpp!(K) + awayScoreOpp!(K)

                    totHomeWins!(RDD) = totHomeWins!(RDD) + homeWins!(K)
                    totHomeLosses!(RDD) = totHomeLosses!(RDD) + homeLosses!(K)
                    totAwayWins!(RDD) = totAwayWins!(RDD) + awayWins!(K)
                    totAwayLosses!(RDD) = totAwayLosses!(RDD) + awayLosses!(K)
                    totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                    totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                    totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                    totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

                End If

            Next K

            ' ** DIVISION TOTALS
            Print #1, Tab(5); "TOTALS";
            Print #1, Tab(21); Using "##"; totHomeWins!(RDD);
            Print #1, Tab(25); Using "##"; totHomeLosses!(RDD);

            If totHomeWins!(RDD) = totHomeWins!(RDD) + totHomeLosses!(RDD) And totHomeWins!(RDD) <> 0 Then 
                Print #1, Tab(28); "1.000";
            End If
            If totHomeWins!(RDD) = 0 Then 
                Print #1, Tab(29); ".000";
            End If
            If totHomeWins!(RDD) > 0 And totHomeWins!(RDD) < totHomeWins!(RDD) + totHomeLosses!(RDD) Then
                statPCT! = (totHomeWins!(RDD) / (totHomeWins!(RDD) + totHomeLosses!(RDD)))
                Print #1, Tab(29); Using ".###"; statPCT!;
            End If

            Print #1, Tab(35); Using "###"; totHomeScoreTeam!(RDD);
            Print #1, Tab(39); Using "###"; totHomeScoreOpp!(RDD);

            Print #1, Tab(53); Using "##"; totAwayWins!(RDD);
            Print #1, Tab(57); Using "##"; totAwayLosses!(RDD);

            If totAwayWins!(RDD) = totAwayWins!(RDD) + totAwayLosses!(RDD) And totAwayWins!(RDD) <> 0 Then 
                Print #1, Tab(60); "1.000";
            End If
            If totAwayWins!(RDD) = 0 Then 
                Print #1, Tab(61); ".000";
            End If
            If totAwayWins!(RDD) > 0 And totAwayWins!(RDD) < totAwayWins!(RDD) + totAwayLosses!(RDD) Then
                statPCT! = (totAwayWins!(RDD) / (totAwayWins!(RDD) + totAwayLosses!(RDD)))
                Print #1, Tab(61); Using ".###"; statPCT!;
            End If

            Print #1, Tab(67); Using "###"; totAwayScoreTeam!(RDD);
            Print #1, Tab(71); Using "###"; totAwayScoreOpp!(RDD);

            Print #1, Tab(84); Using "###"; totHomeWins!(RDD) + totAwayWins!(RDD);
            Print #1, Tab(89); Using "###"; totHomeLosses!(RDD) + totAwayLosses!(RDD);

            If totHomeWins!(RDD) + totAwayWins!(RDD) = totHomeWins!(RDD) + totAwayWins!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD) And totHomeWins!(RDD) + totAwayWins!(RDD) <> 0 Then 
                Print #1, Tab(95); "1.000";
            End If
            If totHomeWins!(RDD) + totAwayWins!(RDD) = 0 Then 
                Print #1, Tab(96); ".000";
            End If
            If totHomeWins!(RDD) + totAwayWins!(RDD) > 0 And totHomeWins!(RDD) + totAwayWins!(RDD) < totHomeWins!(RDD) + totAwayWins!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD) Then
                statPCT! = ((totHomeWins!(RDD) + totAwayWins!(RDD)) / (totHomeWins!(RDD) + totAwayWins!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD)))
                Print #1, Tab(96); Using ".###"; statPCT!;
            End If

            Print #1, Tab(102); Using "###"; totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD);
            Print #1, Tab(106); Using "###"; totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)
            Print #1,

        End If

        div = div + 1

    Loop Until div > DIVS_PER_LEAGUE Or divisionNames$(div) = "XXX" Or divisionNames$(div) = "*" Or divisionNames$(div) = ""

    ' ** LEAGUE TOTALS **
    Print #1,
    Print #1,
    Print #1,
    Print #1,
    Print #1, Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOTALS -------"
    Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; 
    Print #1, Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; 
    Print #1, Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"
    Print #1

    For W = 1 To DIVS_PER_LEAGUE

        If divisionNames$(W) <> "XXX" And divisionNames$(W) <> "*" And divisionNames$(W) <> "" Then

            Print #1, Tab(2); divisionNames$(W);
            Print #1, Tab(21); Using "##"; totHomeWins!(W);
            Print #1, Tab(25); Using "##"; totHomeLosses!(W);

            If totHomeWins!(W) = totHomeWins!(W) + totHomeLosses!(W) And totHomeWins!(W) <> 0 Then 
                Print #1, Tab(28); "1.000";
            End If
            If totHomeWins!(W) = 0 Then 
                Print #1, Tab(29); ".000";
            End If
            If totHomeWins!(W) > 0 And totHomeWins!(W) < totHomeWins!(W) + totHomeLosses!(W) Then
                statPCT! = (totHomeWins!(W) / (totHomeWins!(W) + totHomeLosses!(W)))
                Print #1, Tab(29); Using ".###"; statPCT!;
            End If

            Print #1, Tab(35); Using "###"; totHomeScoreTeam!(W);
            Print #1, Tab(39); Using "###"; totHomeScoreOpp!(W);

            Print #1, Tab(53); Using "##"; totAwayWins!(W);
            Print #1, Tab(57); Using "##"; totAwayLosses!(W);

            If totAwayWins!(W) = totAwayWins!(W) + totAwayLosses!(W) And totAwayWins!(W) <> 0 Then 
                Print #1, Tab(60); "1.000";
            End If
            If totAwayWins!(W) = 0 Then 
                Print #1, Tab(61); ".000";
            End If
            If totAwayWins!(W) > 0 And totAwayWins!(W) < totAwayWins!(W) + totAwayLosses!(W) Then 
                statPCT! = (totAwayWins!(W) / (totAwayWins!(W) + totAwayLosses!(W)))
                Print #1, Tab(61); Using ".###"; statPCT!;
            End If

            Print #1, Tab(67); Using "###"; totAwayScoreTeam!(W);
            Print #1, Tab(71); Using "###"; totAwayScoreOpp!(W);

            Print #1, Tab(84); Using "###"; totHomeWins!(W) + totHomeWins!(W);
            Print #1, Tab(89); Using "###"; totHomeLosses!(W) + totAwayLosses!(W);

            If totHomeWins!(W) + totAwayWins!(W) = totHomeWins!(W) + totAwayWins!(W) + totHomeLosses!(W) + totAwayLosses!(W) And totHomeWins!(W) + totAwayWins!(W) <> 0 Then 
                Print #1, Tab(95); "1.000";
            End If
            If (totHomeWins!(W) + totAwayWins!(W)) = 0 Then 
                Print #1, Tab(96); ".000";
            End If
            If (totHomeWins!(W) + totAwayWins!(W)) > 0 And totHomeWins!(W) + totAwayWins!(W) < totHomeWins!(W) + totAwayWins!(W) + totHomeLosses!(W) + totAwayLosses!(W) Then 
                statPCT! = ((totHomeWins!(W) + totAwayWins!(W)) / (totHomeWins!(W) + totAwayWins!(W) + totHomeLosses!(W) + totAwayLosses!(W)))
                Print #1, Tab(96); Using ".###"; statPCT!;
            End If

            Print #1, Tab(102); Using "###"; totHomeScoreTeam!(W) + totAwayScoreTeam!(W);
            Print #1, Tab(106); Using "###"; totHomeScoreOpp!(W) + totAwayScoreOpp!(W)

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To DIVS_PER_LEAGUE
        THWW = THWW + totHomeWins!(I)
        THLL = THLL + totHomeLosses!(I)
        TAWW = TAWW + totAwayWins!(I)
        TALL = TALL + totAwayLosses!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_LEAGUE
        TR11 = TR11 + totHomeScoreTeam!(I)
        TR22 = TR22 + totHomeScoreOpp!(I)
        TR33 = TR33 + totAwayScoreTeam!(I)
        TR44 = TR44 + totAwayScoreOpp!(I)
    Next

    Print #1, Tab(5); "TOTALS";
    Print #1, Tab(20); Using "###"; THWW;
    Print #1, Tab(24); Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then 
        Print #1, Tab(28); "1.000";
    End If
    If THWW = 0 Then 
        Print #1, Tab(29); ".000";
    End If
    If THWW > 0 And THWW < THWW + THLL Then
        statPCT! = THWW / (THWW + THLL)
        Print #1, Tab(29); Using ".###"; statPCT!;
    End If

    Print #1, Tab(35); Using "###"; TR11;
    Print #1, Tab(39); Using "###"; TR22;
    Print #1, Tab(52); Using "###"; TAWW;
    Print #1, Tab(56); Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then 
        Print #1, Tab(60); "1.000";
    End If
    If TAWW = 0 Then 
        Print #1, Tab(61); ".000";
    End If
    If TAWW > 0 And TAWW < TAWW + TALL Then
        statPCT! = TAWW / (TAWW + TALL)
        Print #1, Tab(61); Using ".###"; statPCT!;
    End If

    Print #1, Tab(67); Using "###"; TR33;
    Print #1, Tab(71); Using "###"; TR44;
    Print #1, Tab(84); Using "###"; THWW + TAWW;
    Print #1, Tab(89); Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then 
        Print #1, Tab(95); "1.000";
    End If
    If THWW + TAWW = 0 Then 
        Print #1, Tab(96); ".000";
    End If
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then
        statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL)
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

    Shared teamRatings()

    Shared awayWins!(), awayLosses!()
    Shared homeWins!(), homeLosses!()
    Shared awayScoreTeam!(), awayScoreOpp!()
    Shared homeScoreTeam!(), homeScoreOpp!()
    Shared totAwayWins!(), totAwayLosses!()
    Shared totHomeWins!(), totHomeLosses!()
    Shared totAwayScoreTeam!(), totAwayScoreOpp!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!()

    Shared divisionNames$(), memberName$()
    Shared div1Name$, div2Name$, div3Name$, div4Name$

    divisionNames$(1) = div1Name$
    divisionNames$(2) = div2Name$
    divisionNames$(3) = div3Name$
    divisionNames$(4) = div4Name$

    div = 0

    Do

        If divisionNames$(RDD) <> "XXX" And divisionNames$(RDD) <> "*" And divisionNames$(RDD) <> "" Then

            RDD = div + 1

            Cls

            Locate 2, 35
            Color teamRatings(11), teamRatings(12): Print teamName$
            Color 3, 0

            'Table header
            Locate 4
            Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)
            Locate 5
            Print Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); 
            Print Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); 
            Print Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

            DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
            EE = RDD * TEAMS_PER_DIV

            Color 11
            Print Left$(divisionNames$(RDD), 15);

            Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

            For K = DD To EE

                If memberName$(K) <> "XXX" And memberName$(K) <> "*" And memberName$(K) <> teamName$ Then

                    Color 15
                    Locate , 1: Print Left$(memberName$(K), 13);

                    Color 7
                    Locate , 15: Print Using "##"; homeWins!(K);
                    Locate , 18: Print Using "##"; homeLosses!(K);

                    If homeWins!(K) = homeWins!(K) + homeLosses!(K) And homeWins!(K) <> 0 Then 
                        Locate , 21: Print "1.000";
                    End If
                    If homeWins!(K) = 0 Then 
                        Locate , 22: Print ".000";
                    End If
                    If homeWins!(K) > 0 And homeWins!(K) < homeWins!(K) + homeLosses!(K) Then
                        statPCT! = homeWins!(K) / (homeWins!(K) + homeLosses!(K))
                        Locate , 22: Print Using ".###"; statPCT!;
                    End If

                    Locate , 27: Print Using "###"; homeScoreTeam!(K);
                    Locate , 31: Print Using "###"; homeScoreOpp!(K);

                    Color 3
                    Print Tab(35); Chr$(179);

                    Color 7
                    Locate , 37: Print Using "##"; awayWins!(K);
                    Locate , 40: Print Using "##"; awayLosses!(K);

                    If awayWins!(K) = awayWins!(K) + awayLosses!(K) And awayWins!(K) <> 0 Then
                        Locate , 43: Print "1.000";
                    End If
                    If awayWins!(K) = 0 Then 
                        Locate , 44: Print ".000";
                    End If
                    If awayWins!(K) > 0 And awayWins!(K) < awayWins!(K) + awayLosses!(K) Then
                        statPCT! = (awayWins!(K) / (awayWins!(K) + awayLosses!(K)))
                        Locate , 44: Print Using ".###"; statPCT!;
                    End If

                    Locate , 49: Print Using "###"; awayScoreTeam!(K);
                    Locate , 53: Print Using "###"; awayScoreOpp!(K);

                    Color 3
                    Print Tab(58); Chr$(179);

                    Color 7
                    Locate , 60: Print Using "##"; homeWins!(K) + awayWins!(K);
                    Locate , 63: Print Using "##"; homeLosses!(K) + awayLosses!(K);

                    If (homeWins!(K) + awayWins!(K)) = (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses!(K)) And (homeWins!(K) + awayWins!(K)) <> 0 Then 
                        Locate , 66: Print "1.000";
                    End If
                    If (homeWins!(K) + awayWins!(K)) = 0 Then 
                        Locate , 67: Print ".000";
                    End If
                    If (homeWins!(K) + awayWins!(K)) > 0 And (homeWins!(K) + awayWins!(K)) < (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses!(K)) Then
                        statPCT! = (homeWins!(K) + awayWins!(K)) / (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses!(K))
                        Locate , 67: Print Using ".###"; statPCT!;
                    End If

                    Locate , 72: Print Using "###"; homeScoreTeam!(K) + awayScoreTeam!(K);
                    Locate , 76: Print Using "###"; homeScoreOpp!(K) + awayScoreOpp!(K)

                    Color 3
                    Print Tab(35); Chr$(179); Tab(58); Chr$(179);

                    totHomeWins!(RDD) = totHomeWins!(RDD) + homeWins!(K)
                    totHomeLosses!(RDD) = totHomeLosses!(RDD) + homeLosses!(K)
                    totAwayWins!(RDD) = totAwayWins!(RDD) + awayWins!(K)
                    totAwayLosses!(RDD) = totAwayLosses!(RDD) + awayLosses!(K)
                    totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                    totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                    totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                    totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

                End If

            Next K

            ' ** DIVISION TOTALS
            Print

            Color 11

            Locate , 5: Print "TOTALS";

            Locate , 15: Print Using "##"; totHomeWins!(RDD);
            Locate , 18: Print Using "##"; totHomeLosses!(RDD);

            If totHomeWins!(RDD) = totHomeWins!(RDD) + totHomeLosses!(RDD) And totHomeWins!(RDD) <> 0 Then 
                Locate , 21: Print "1.000";
            End If
            If totHomeWins!(RDD) = 0 Then 
                Locate , 22: Print ".000";
            End If
            If totHomeWins!(RDD) > 0 And totHomeWins!(RDD) < totHomeWins!(RDD) + totHomeLosses!(RDD) Then
                statPCT! = (totHomeWins!(RDD) / (totHomeWins!(RDD) + totHomeLosses!(RDD)))
                Locate , 22: Print Using ".###"; statPCT!;
            End If

            Locate , 27: Print Using "###"; totHomeScoreTeam!(RDD);
            Locate , 31: Print Using "###"; totHomeScoreOpp!(RDD);

            Color 3
            Print Tab(35); Chr$(179);

            Color 11
            Locate , 37: Print Using "##"; totAwayWins!(RDD);
            Locate , 40: Print Using "##"; totAwayLosses!(RDD);

            If totAwayWins!(RDD) = totAwayWins!(RDD) + totAwayLosses!(RDD) And totAwayWins!(RDD) <> 0 Then 
                Locate , 43: Print "1.000";
            End If
            If totAwayWins!(RDD) = 0 Then 
                Locate , 44: Print ".000";
            End If
            If totAwayWins!(RDD) > 0 And totAwayWins!(RDD) < totAwayWins!(RDD) + totAwayLosses!(RDD) Then
                statPCT! = (totAwayWins!(RDD) / (totAwayWins!(RDD) + totAwayLosses!(RDD)))
                Locate , 44: Print Using ".###"; statPCT!;
            End If

            Locate , 49: Print Using "###"; totAwayScoreTeam!(RDD);
            Locate , 53: Print Using "###"; totAwayScoreOpp!(RDD);

            Color 3
            Print Tab(58); Chr$(179);

            Color 11

            Locate , 59: Print Using "###"; totHomeWins!(RDD) + totAwayWins!(RDD);
            Locate , 62: Print Using "###"; totHomeLosses!(RDD) + totAwayLosses!(RDD);

            If totHomeWins!(RDD) + totAwayWins!(RDD) = totHomeWins!(RDD) + totHomeLosses!(RDD) + totAwayWins!(RDD) + totAwayLosses!(RDD) And totHomeWins!(RDD) + totAwayWins!(RDD) <> 0 Then 
                Locate , 66: Print "1.000";
            End If
            If totHomeWins!(RDD) + totAwayWins!(RDD) = 0 Then 
                Locate , 67: Print ".000";
            End If
            If totHomeWins!(RDD) + totAwayWins!(RDD) > 0 And totHomeWins!(RDD) + totAwayWins!(RDD) < totHomeWins!(RDD) + totAwayWins!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD) Then
                statPCT! = (totHomeWins!(RDD) + totAwayWins!(RDD)) / (totHomeWins!(RDD) + totAwayWins!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD))
                Locate , 67: Print Using ".###"; statPCT!;
            End If

            Locate , 72: Print Using "###"; totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD);
            Locate , 76: Print Using "###"; totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)

            Color 14
            Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

        End If

        div = div + 1

    Loop Until div > DIVS_PER_LEAGUE Or divisionNames$(div) = "XXX" Or divisionNames$(div) = "*" Or divisionNames$(div) = ""

    ' ** LEAGUE TOTALS **
    Cls

    Locate 2, 35
    Color teamRatings(11), teamRatings(12): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)
    Locate 5
    Print Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); 
    Print Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); 
    Print Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

    Color 11: Print "DIVISIONS";

    Color 3
    Print Tab(35); Chr$(179); Tab(58); Chr$(179)

    For W = 1 To DIVS_PER_LEAGUE

        If divisionNames$(W) <> "XXX" And divisionNames$(W) <> "*" And divisionNames$(W) <> "" Then

            Color 15

            'We have to limit this to 14 because we start stats on column 15
            Locate , 1: Print Left$(divisionNames$(W), 14);

            Color 7
            Locate , 15: Print Using "##"; totHomeWins!(W);
            Locate , 18: Print Using "##"; totHomeLosses!(W);

            If totHomeWins!(W) = totHomeWins!(W) + totHomeLosses!(W) And totHomeWins!(W) <> 0 Then 
                Locate , 21: Print "1.000";
            End If
            If totHomeWins!(W) = 0 Then 
                Locate , 22: Print ".000";
            End If
            If totHomeWins!(W) > 0 And totHomeWins!(W) < totHomeWins!(W) + totHomeLosses!(W) Then
                statPCT! = (totHomeWins!(W) / (totHomeWins!(W) + totHomeLosses!(W)))
                Locate , 22: Print Using ".###"; statPCT!;
            End If

            Locate , 27: Print Using "###"; totHomeScoreTeam!(W);
            Locate , 31: Print Using "###"; totHomeScoreOpp!(W);

            Color 3
            Print Tab(35); Chr$(179);

            Color 7
            Locate , 37: Print Using "##"; totAwayWins!(W);
            Locate , 40: Print Using "##"; totAwayLosses!(W);

            If totAwayWins!(W) = totAwayWins!(W) + totAwayLosses!(W) And totAwayWins!(W) <> 0 Then 
                Locate , 43: Print "1.000";
            End If
            If totAwayWins!(W) = 0 Then 
                Locate , 44: Print ".000";
            End If
            If totAwayWins!(W) > 0 And totAwayWins!(W) < totAwayWins!(W) + totAwayLosses!(W) Then
                statPCT! = (totAwayWins!(W) / (totAwayWins!(W) + totAwayLosses!(W)))
                Locate , 44: Print Using ".###"; statPCT!;
            End If

            Locate , 49: Print Using "###"; totAwayScoreTeam!(W);
            Locate , 53: Print Using "###"; totAwayScoreOpp!(W);

            Color 3
            Print Tab(58); Chr$(179);

            Color 7
            Locate , 59: Print Using "###"; totHomeWins!(W) + totAwayWins!(W);
            Locate , 62: Print Using "###"; totHomeLosses!(W) + totAwayLosses!(W);

            If totHomeWins!(W) + totAwayWins!(W) = totHomeWins!(W) + totAwayWins!(W) + totHomeLosses!(W) + totAwayLosses!(W) And totHomeWins!(W) + totAwayWins!(W) <> 0 Then 
                Locate , 66: Print "1.000";
            End If
            If (totHomeWins!(W) + totAwayWins!(W)) = 0 Then 
                Locate , 67: Print ".000";
            End If
            If (totHomeWins!(W) + totAwayWins!(W)) > 0 And totHomeWins!(W) + totAwayWins!(W) < totHomeWins!(W) + totAwayWins!(W) + totHomeLosses!(W) + totAwayLosses!(W) Then
                statPCT! = ((totHomeWins!(W) + totAwayWins!(W)) / (totHomeWins!(W) + totAwayWins!(W) + totHomeLosses!(W) + totAwayLosses!(W)))
                Locate , 67: Print Using ".###"; statPCT!;
            End If

            Locate , 72: Print Using "###"; totHomeScoreTeam!(W) + totAwayScoreTeam!(W);
            Locate , 76: Print Using "###"; totHomeScoreOpp!(W) + totAwayScoreOpp!(W)

            Color 3
            Print Tab(35); Chr$(179); Tab(58); Chr$(179);

        End If

    Next W

    'Calculate TOTALS for League
    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To DIVS_PER_LEAGUE
        THWW = THWW + totHomeWins!(I)
        THLL = THLL + totHomeLosses!(I)
        TAWW = TAWW + totHomeWins!(I)
        TALL = TALL + totAwayLosses!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_LEAGUE
        TR11 = TR11 + totHomeScoreTeam!(I)
        TR22 = TR22 + totHomeScoreOpp!(I)
        TR33 = TR33 + totAwayScoreTeam!(I)
        TR44 = TR44 + totAwayScoreOpp!(I)
    Next

    'Division Totals
    Color 11
    Print "TOTALS";

    Locate , 14: Print Using "###"; THWW;
    Locate , 17: Print Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then 
        Locate , 21: Print "1.000";
    End If
    If THWW = 0 Then 
        Locate , 22: Print ".000";
    End If
    If THWW > 0 And THWW < THWW + THLL Then
        statPCT! = THWW / (THWW + THLL)
        Locate , 22: Print Using ".###"; statPCT!;
    End If

    Locate , 27: Print Using "###"; TR11;
    Locate , 31: Print Using "###"; TR22;

    Color 3
    Print Tab(35); Chr$(179);
    
    Color 11
    Locate , 36: Print Using "###"; TAWW;
    Locate , 39: Print Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then 
        Locate , 43: Print "1.000";
    End If
    If TAWW = 0 Then 
        Locate , 44: Print ".000";
    End If
    If TAWW > 0 And TAWW < TAWW + TALL Then
        statPCT! = TAWW / (TAWW + TALL)
        Locate , 44: Print Using ".###"; statPCT!;
    End If

    Locate , 49: Print Using "###"; TR33;
    Locate , 53: Print Using "###"; TR44;

    Color 3
    Print Tab(58); Chr$(179);

    Color 11
    Locate , 59: Print Using "###"; THWW + TAWW;
    Locate , 62: Print Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then 
        Locate , 66: Print "1.000";
    End If
    If THWW + TAWW = 0 Then 
        Locate , 67: Print ".000";
    End If
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then
        statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL)
        Locate , 67: Print Using ".###"; statPCT!;
    End If

    Locate , 72: Print Using "###"; TR11 + TR33;
    Locate , 76: Print Using "###"; TR22 + TR44

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

End Sub
