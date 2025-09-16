'----------------------------------------
'            CNGTM Subroutine
'----------------------------------------
'This subroutine presents a menu of all
'the options related to team rosters
Sub CNGTM (userChoice)

    _Title "Full Count Baseball - Change Team"

    ReDim menuChoices$(1 To 4)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER  "
        menuChoices$(3) = "VIEW A TEAM ROSTER   "
        menuChoices$(4) = "EXIT TO MAIN MENU    "
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

        If (userChoice <> 0) Then
            SELECTION% = userChoice
        Else

            Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            Color , 0
        End If

        If ExitCode% <> 27 And SELECTION% <> 4 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                Select Case SELECTION%

                    Case 1:
                        Call InputEditTeam(2, YN$, team$, teamNbr)
                        statFile$ = nameStatFile$(team$)
                        Call SaveTeamFile(0, YN$, team$, statFile$, teamNbr)

                    Case 2:
                        Call CalcPlayerTotals
                        userKey$ = GetPrintOptions$

                        If userKey$ <> Chr$(27) Then

                            Select Case userKey$

                                Case "F"
                                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                    If userFile$ <> "" Then
                                        Call PrintTeamStats(0, userFile$, team$)
                                    End If

                                Case "P"
                                    Call PrintTeamStats(1, "REPORT", team$)

                            End Select

                        End If

                    Case 3:
                        Call CalcPlayerTotals
                        Call ViewRoster(team$)

                End Select

            End If

        End If

        'Reset "user choice" after first call
        'We're at the menu now, no purpose in forcing shortcuts
        userChoice = 0

    Loop Until SELECTION% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'       PrintTeamStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'previously loaded team stat data and sends
'it to the default printer
Sub PrintTeamStats (printDest, destFile$, targetTeam$)

    Shared batterNames$(), parkType$(), pitcherNames$()

    Shared totBatRat(), totPitchRat()
    Shared batterRatings(), pitcherRatings(), teamRatings()

    Shared CK, parkHR

    Shared earnedRuns!

    Shared Manager$, teamAbbrev$, Stadium$

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1,
    Print #1, Tab(10); targetTeam$
    Print #1, Tab(10); "MANAGER: "; Manager$
    Print #1, Tab(10); "STADIUM: "; Stadium$
    Print #1, 
    Print #1, Tab(10); "TEXT COLOR: "; teamRatings(11)
    Print #1, Tab(10); "BACKGROUND COLOR: "; teamRatings(12)

    Print #1,
    Print #1, Tab(10); "LEAGUE BATTING AVG.";
    Print #1, Tab(38); Using "#####"; teamRatings(1)

    Print #1, Tab(10); "LEAGUE STRIKE OUT AVG.";
    Print #1, Tab(38); Using "#####"; teamRatings(2)
    Print #1, Tab(10); "LEAGUE BASE ON BALLS AVG.";
    Print #1, Tab(38); Using "#####"; teamRatings(3)
    Print #1, Tab(10); "LEAGUE HOME RUN AVG.";
    Print #1, Tab(38); Using "#####"; teamRatings(4)
    Print #1, Tab(10); "DOUBLE PLAYS AVG.";
    Print #1, Tab(38); Using "#####"; teamRatings(5)
    Print #1, Tab(10); "PARK DOUBLE ADJ.";
    Print #1, Tab(38); Using "#####"; teamRatings(6)
    Print #1, Tab(10); "PARK TRIPLE ADJ.";
    Print #1, Tab(38); Using "#####"; teamRatings(7)
    Print #1, Tab(10); "PARK HOME RUN ADJ.";
    Print #1, Tab(37); Using "######"; parkHR
    Print #1, Tab(10); "PARK FOUL GROUND ADJ.";
    Print #1, Tab(37); Using "######"; teamRatings(8)
    Print #1, Tab(10); "PARK INFORMATION";
    Print #1, Tab(40); parkType$(teamRatings(9)); " "; parkType$(teamRatings(10) + 2)

    Print #1,
    Print #1,
    Print #1, "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  3"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" Then

            Print #1, Left$(batterNames$(I), 10);

            Select Case batterRatings(I, 0)

                Case -2: Print #1, Tab(12); "LP";
                Case -1: Print #1, Tab(12); "L";
                Case 0: Print #1, Tab(12); "S";
                Case 1: Print #1, Tab(12); "R";
                Case 2: Print #1, Tab(12); "RP";

            End Select

            Print #1, Tab(14); Using "#"; batterRatings(I, 22);

            If batterRatings(I, 23) <> -1 Then
                Print #1, Using "#"; batterRatings(I, 23);
                If batterRatings(I, 24) <> -1 Then
                    Print #1, Using "#"; batterRatings(I, 24);
                    If batterRatings(I, 25) <> -1 Then
                        Print #1, Using "#"; batterRatings(I, 25);
                    End If
                End If
            End If

            Print #1, Tab(20); Using "###"; batterRatings(I, 3);
            Print #1, Tab(24); Using "###"; batterRatings(I, 4);
            Print #1, Tab(28); Using "###"; batterRatings(I, 5);
            Print #1, Tab(32); Using "###"; batterRatings(I, 6);
            Print #1, Tab(36); Using "##"; batterRatings(I, 7);
            Print #1, Tab(39); Using "##"; batterRatings(I, 8);
            Print #1, Tab(42); Using "##"; batterRatings(I, 9);
            Print #1, Tab(45); Using "###"; batterRatings(I, 10);
            Print #1, Tab(49); Using "###"; batterRatings(I, 11);
            Print #1, Tab(53); Using "###"; batterRatings(I, 12);
            Print #1, Tab(57); Using "###"; batterRatings(I, 13);
            Print #1, Tab(61); Using "##"; batterRatings(I, 14);
            Print #1, Tab(64); Using "##"; batterRatings(I, 18);

            batAvg! = 0

            If batterRatings(I, 4) > 0 Then
                batAvg! = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
            End If
            If batterRatings(I, 4) = 0 Or batAvg! = 0 Then
                Print #1, Tab(68); ".000"
            End If
            If batAvg! > 0 And batAvg! < 100 Then
                Print #1, Tab(68); ".0";: Print #1, Tab(70); Using "##"; batAvg!
            End If
            If batAvg! > 100 Then
                Print #1, Tab(68); ".";: Print #1, Tab(69); Using "###"; batAvg!
            End If

        End If

    Next

    If totBatRat(1) > 0 Then
        bavg! = totBatRat(3) / totBatRat(1)
    Else
        bavg! = 0
    End If

    Print #1, "TOTALS";
    Print #1, Tab(23); Using "####    ####   ###    ###    ####    ###     .###"; totBatRat(1); totBatRat(3); totBatRat(5); totBatRat(7); totBatRat(9); totBatRat(11); bavg!
    Print #1, Tab(27); Using "####   ####   ###     ###     ###"; totBatRat(2); totBatRat(4); totBatRat(6); totBatRat(8); totBatRat(10)

    Print #1,
    Print #1,
    Print #1, "             POSITION 1    POSITION 2  POSITION 3   POSITION 4"
    Print #1, "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  RUN BUNT"

    For I = 0 To 22

        Print #1, Left$(batterNames$(I), 10);

        'Position 1 Ratings
        Print #1, Tab(13); Using "####"; batterRatings(I, 26);
        Print #1, Tab(19); Using "#"; batterRatings(I, 33);
        Print #1, Tab(23); Using "#"; batterRatings(I, 34);

        'Position 2 Ratings
        Print #1, Tab(26); Using "####"; batterRatings(I, 27);
        Print #1, Tab(32); Using "#"; batterRatings(I, 35);
        Print #1, Tab(36); Using "#"; batterRatings(I, 36);

        'Position 3 Ratings
        Print #1, Tab(39); Using "####"; batterRatings(I, 28);
        Print #1, Tab(45); Using "#"; batterRatings(I, 37);
        Print #1, Tab(49); Using "#"; batterRatings(I, 38);

        'Position 4 Ratings
        Print #1, Tab(52); Using "####"; batterRatings(I, 29);
        Print #1, Tab(58); Using "#"; batterRatings(I, 39);
        Print #1, Tab(62); Using "#"; batterRatings(I, 40);

        'Run
        Print #1, Tab(67); Using "#"; batterRatings(I, 16);

        'Bunt
        Print #1, Tab(72); Using "#"; batterRatings(I, 30)

    Next I

    Print #1,
    Print #1,
    Print #1, "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" Then

            numberAppearances = numberAppearances + pitcherRatings(I, 4)
            Print #1, Left$(pitcherNames$(I), 10);

            If pitcherRatings(I, 0) = -1 Then
                Print #1, Tab(14); "L";
            End If
            If pitcherRatings(I, 0) = 1 Then
                Print #1, Tab(14); "R";
            End If

            'Wins
            Print #1, Tab(16); Using "##"; pitcherRatings(I, 1);
            'Losses
            Print #1, Tab(19); Using "##"; pitcherRatings(I, 2);
            'Saves
            Print #1, Tab(22); Using "##"; pitcherRatings(I, 3);
            'Games
            Print #1, Tab(25); Using "##"; pitcherRatings(I, 4);
            'Games Started
            Print #1, Tab(28); Using "##"; pitcherRatings(I, 5);
            'Inn Pitched
            Print #1, Tab(31); Using "###"; pitcherRatings(I, 6);
            'Hits
            Print #1, Tab(35); Using "###"; pitcherRatings(I, 7);
            'Walks
            Print #1, Tab(39); Using "###"; pitcherRatings(I, 8);
            'Strikeouts
            Print #1, Tab(43); Using "###"; pitcherRatings(I, 9);
            'ERA
            Print #1, Tab(46); Using "##.##"; pitcherRatings(I, 10) / 100;
            'GA
            Print #1, Tab(53); Using "##"; pitcherRatings(I, 12);
            'HR
            Print #1, Tab(56); Using "##"; pitcherRatings(I, 13);
            'CG
            Print #1, Tab(59); Using "##"; pitcherRatings(I, 16);
            'WP
            Print #1, Tab(62); Using "##"; pitcherRatings(I, 33);
            'HLDR
            Print #1, Tab(66); Using "##"; pitcherRatings(I, 18)

        End If

    Next

    If totPitchRat(6) > 0 Then
        rate! = (earnedRuns! * 9) / totPitchRat(6)
    Else
        rate! = 0
    End If

    Print #1, "TOTALS";
    Print #1, Tab(14); Using "####  ####      ####    ####     #.##    ###   ###"; totPitchRat(1); totPitchRat(3); totPitchRat(6); totPitchRat(8); rate!; totPitchRat(13); totPitchRat(33)
    Print #1, Tab(17); Using "####      ###    ####    ####            ###"; totPitchRat(2); totPitchRat(5); totPitchRat(7); totPitchRat(9); totPitchRat(16)
    Print #1,
    Print #1, "TOTAL APPEARANCES: "; numberAppearances

    Print #1,
    Print #1,
    Print #1, "PITCHER        G  AB   R   H DB TR HR RBI  BB  SO  SB CS BAVG  R B FAVG"

    For I = 0 To 21

        Print #1, Left$(pitcherNames$(I), 10);

        Print #1, Tab(14); Using "###"; pitcherRatings(I, 19);
        Print #1, Tab(18); Using "###"; pitcherRatings(I, 20);
        Print #1, Tab(22); Using "###"; pitcherRatings(I, 21);
        Print #1, Tab(26); Using "###"; pitcherRatings(I, 22);
        Print #1, Tab(30); Using "##"; pitcherRatings(I, 23);
        Print #1, Tab(33); Using "##"; pitcherRatings(I, 24);
        Print #1, Tab(36); Using "##"; pitcherRatings(I, 25);
        Print #1, Tab(39); Using "###"; pitcherRatings(I, 26);
        Print #1, Tab(43); Using "###"; pitcherRatings(I, 27);
        Print #1, Tab(47); Using "###"; pitcherRatings(I, 28);
        Print #1, Tab(52); Using "##"; pitcherRatings(I, 29);
        Print #1, Tab(55); Using "##"; pitcherRatings(I, 30);

        If pitcherRatings(I, 11) <> 999 Then
            statInnPitchCT! = pitcherRatings(I, 11) / 1000
            Print #1, Tab(58); Using "#.###"; statInnPitchCT!;
        Else
            batAvg! = 0

            '-- This is initialized as 1000
            '-- So when not given a value it calculates too large
            If pitcherRatings(I, 20) > 0 Then
                batAvg! = Int((pitcherRatings(I, 22) / pitcherRatings(I, 20) + .0005) * 1000)
            End If
            If pitcherRatings(I, 20) = 0 Or batAvg! = 0 Then
                Print #1, Tab(58); ".000";
            End If

            If batAvg! > 0 And batAvg! < 100 Then
                Print #1, Tab(58); ".0";: Print #1, Tab(61); Using "##"; batAvg!;
            End If
            If batAvg! > 100 And batAvg! < 1000 Then
                Print #1, Tab(58); ".";: Print #1, Tab(60); Using "###"; batAvg!;
            End If
            If batAvg! >= 1000 Then
                Print #1, Tab(58); " ---";
            End If

        End If

        Print #1, Tab(64); Using "#"; pitcherRatings(I, 31);
        Print #1, Tab(66); Using "#"; pitcherRatings(I, 32);
        Print #1, Tab(68); Using "####"; pitcherRatings(I, 17)

    Next

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
'         ViewRoster subroutine
'----------------------------------------
'This subroutine formats and displays the
'general stats for a team, then provides
'a menu for choosing options to display
'various aspects of the roster
Sub ViewRoster (targetTeam$)

    Shared earnedRuns!

    Shared batterRatings(), pitcherRatings()
    Shared teamRatings(), totBatRat(), totPitchRat()

    Shared batterNames$(), parkType$(), pitcherNames$()

    Shared CK, parkHR

    Shared Manager$, teamAbbrev$, Stadium$

    Erase totBatRat, totPitchRat

    For K = 4 To 18:
        For I = 0 To 22
            If batterNames$(I) <> "XXX" Then
                totBatRat(K - 3) = totBatRat(K - 3) + batterRatings(I, K)
            End If
        Next
    Next

    For K = 4 To 18:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" And pitcherRatings(I, 11) = 999 Then
                totBatRat(K - 3) = totBatRat(K - 3) + pitcherRatings(I, K + 16)
            End If
        Next
    Next

    For K = 1 To 33:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" Then
                totPitchRat(K) = totPitchRat(K) + pitcherRatings(I, K)
            End If
        Next
    Next

    earnedRuns! = 0

    For I = 0 To 21
        If pitcherNames$(I) <> "XXX" Then
            earnedRuns! = earnedRuns! + ((pitcherRatings(I, 6) / 9) * (pitcherRatings(I, 10) / 100))
        End If
    Next

    Color 15, 0
    Cls

    Locate 3, 10:
    Color teamRatings(11), teamRatings(12): Print targetTeam$
    Color 15
    Locate , 10
    Color 14
    Print "MANAGER: ";
    Color 15: Print Manager$
    Locate , 10
    Color 14
    Print "STADIUM: ";
    Color 15: Print Stadium$
    Locate 7, 10
    Color 14: Print "TEXT COLOR      : "; 
    Color 15: Print teamRatings(11)
    Locate, 10
    Color 14: Print "BACKGROUND COLOR: ";
    Color 15: Print teamRatings(12)

    Color 3
    Locate 10, 10: Print "LEAGUE BATTING AVG.";
    Color 15
    Locate 10, 38: Print Using "#####"; teamRatings(1)
    Color 3
    Locate , 10: Print "LEAGUE STRIKE OUT AVG.";
    Color 15
    Locate , 38: Print Using "#####"; teamRatings(2)
    Color 3
    Locate , 10: Print "LEAGUE BASE ON BALLS AVG.";
    Color 15
    Locate , 38: Print Using "#####"; teamRatings(3)
    Color 3
    Locate , 10: Print "LEAGUE HOME RUN AVG.";
    Color 15
    Locate , 38: Print Using "#####"; teamRatings(4)
    Color 3
    Locate , 10: Print "DOUBLE PLAYS AVG.";
    Color 15
    Locate , 38: Print Using "#####"; teamRatings(5)
    Color 3
    Locate , 10: Print "PARK DOUBLE ADJ.";
    Color 15
    Locate , 38: Print Using "#####"; teamRatings(6)
    Color 3
    Locate , 10: Print "PARK TRIPLE ADJ.";
    Color 15
    Locate , 38: Print Using "#####"; teamRatings(7)
    Color 3
    Locate , 10: Print "PARK HOME RUN ADJ.";
    Color 15
    Locate , 37: Print Using "######"; parkHR
    Color 3
    Locate , 10: Print "PARK FOUL GROUND ADJ.";
    Color 15
    Locate , 37: Print Using "######"; teamRatings(8)
    Color 3
    Locate , 10: Print "PARK INFORMATION";
    Color 15
    Locate , 40: Print parkType$(teamRatings(9)); " "; parkType$(teamRatings(10) + 2)

    Color 14
    Locate LAST_ROW - 2, 10: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

    Do

        Cls

        Color 15
        Locate 2, 1
        Color teamRatings(11), teamRatings(12): Print targetTeam$

        Locate 4, 1
        Color 11
        Print "ROSTER OPTIONS:";

        Locate 6, 1
        Color 14: Print "(1) ";
        Color 15: Print "VIEW HITTERS"

        Locate 8, 1
        Color 14: Print "(2) ";
        Color 15: Print "VIEW PITCHERS"

        Locate 10, 1
        Color 14: Print "(3) ";
        Color 15: Print "VIEW PITCHERS HITTING/FLDG STATS"

        Locate 12, 1
        Color 14: Print "(4) ";
        Color 15: Print "RETURN TO MAIN MENU"

        Do
            Locate 14, 1
            Color 15
            Input "SELECT AN OPTION"; I
            Color 15
        Loop Until I >= 1 And I <= 4

        Select Case I

            Case 1:
                Call ViewHitters(targetTeam$)

            Case 2:
                Call ViewPitchers(targetTeam$)

            Case 3:
                Call ViewPitcherHittingFielding

        End Select

    Loop Until I = 4

End Sub



'----------------------------------------
'       CalcPlayerTotals Subroutine
'----------------------------------------
'This subroutine totals up all of the
'individual batter and pitcher Ratings
'so they can be used for output of
'team roster
Sub CalcPlayerTotals ()

    Shared batterRatings(), pitcherRatings()
    Shared totBatRat(), totPitchRat()

    Shared batterNames$(), pitcherNames$()

    Shared earnedRuns!

    Erase totBatRat, totPitchRat

    For K = 4 To 18:
        For I = 0 To 22
            If batterNames$(I) <> "XXX" Then
                totBatRat(K - 3) = totBatRat(K - 3) + batterRatings(I, K)
            End If
        Next
    Next

    For K = 4 To 18:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" And pitcherRatings(I, 11) = 999 Then
                totBatRat(K - 3) = totBatRat(K - 3) + pitcherRatings(I, K + 16)
            End If
        Next
    Next

    For K = 1 To 33:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" Then
                totPitchRat(K) = totPitchRat(K) + pitcherRatings(I, K)
            End If
        Next
    Next

    earnedRuns! = 0

    For I = 0 To 21
        If pitcherNames$(I) <> "XXX" Then
            earnedRuns! = earnedRuns! + ((pitcherRatings(I, 6) / 9) * (pitcherRatings(I, 10) / 100))
        End If
    Next

End Sub


