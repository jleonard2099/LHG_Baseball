'----------------------------------------
'           COMPARE Subroutine
'----------------------------------------
'This subroutine loads in existing stats
'for a team and provides different options
'to the end user for formatting stats
Sub COMPARE ()

    _Title "Full Count Baseball - Stats Comparison"

    Shared earnedRuns!

    Shared statAtBats(), statInnPitch()
    Shared statBatter(), statPitcher()
    Shared batterRatings(), teamRatings(), pitcherRatings(), totBatRat()
    Shared statTeamBat!(), statTeamPit!()

    Shared totPitRat&()

    Shared batterNames$(), pitcherNames$()

    Do

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            haveStats = 1

            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                haveStats = 0

                MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                For K = 4 To 18:
                    For I = 0 To 22
                        totBatRat(K - 3) = totBatRat(K - 3) + batterRatings(I, K)
                    Next
                Next

                For K = 1 To 33
                    For I = 0 To 21
                        totPitRat&(K) = totPitRat&(K) + pitcherRatings(I, K)
                    Next
                Next

                earnedRuns! = 0

                For I = 0 To 21
                    earnedRuns! = earnedRuns! + ((pitcherRatings(I, 6) / 9) * (pitcherRatings(I, 10) / 100))
                Next

                For K = 4 To 14:
                    For I = 0 To 21
                        If pitcherRatings(I, 11) = 999 Then
                            totBatRat(K - 3) = totBatRat(K - 3) + pitcherRatings(I, K + 16)
                        End If
                    Next
                Next

                Call ReadTeamStats(team$, statFile$)

                userKey$ = GetOutputOptions$

                If userKey$ <> Chr$(27) Then

                    backToMenu = 0

                    Select Case userKey$

                        Case "F":
                            userFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
                            If userFile$ <> "" Then
                                Call Comparisons_Report(0, userFile$, team$)
                            End If

                        Case "P":
                            Call Comparisons_Report(1, "COMPARISON", team$)

                        Case "S":
                            Call Comparisons_Screen(team$, backToMenu)

                    End Select

                End If

            End If 'Done checking for valid stat file

        End If 'Done checking for valid team

    Loop Until backToMenu = 0 Or userKey$ = Chr$(27) Or teamNbr = 0 Or haveStats = 0

End Sub


'----------------------------------------
'     Comparisons_Report Subroutine
'----------------------------------------
'This subroutine formats team stats for
'output to the default printer
Sub Comparisons_Report (printDest, destFile$, teamName$)

    Shared batterNames$(), pitcherNames$()
    Shared batterRatings(), pitcherRatings()
    Shared statBatter(), statPitcher()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, "STAT COMPARISONS FOR "; teamName$
    Print #1,
    Print #1,
    Print #1, "BATTER";
    Print #1, Tab(21); "  G";
    Print #1, Tab(26); " AB";
    Print #1, Tab(31); "  R";
    Print #1, Tab(36); "  H";
    Print #1, Tab(41); " 2B";
    Print #1, Tab(45); " 3B";
    Print #1, Tab(49); " HR";
    Print #1, Tab(54); "RBI";
    Print #1, Tab(59); " BB";
    Print #1, Tab(64); " SO";
    Print #1, Tab(69); " SB";
    Print #1, Tab(74); " CS";
    Print #1, Tab(79); "AVG";
    Print #1, Tab(85); "SLG";
    Print #1, Tab(91); "OBP";
    Print #1, Tab(97); "FLD.PCT"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "END" And batterNames$(I) <> "" Then

            Print #1, Left$(batterNames$(I), 14);
            Print #1, Tab(16); "REG.";
            Print #1, Tab(21); Using "###"; batterRatings(I, 3);
            Print #1, Tab(26); Using "###"; batterRatings(I, 4);
            Print #1, Tab(31); Using "###"; batterRatings(I, 5);
            Print #1, Tab(36); Using "###"; batterRatings(I, 6);
            Print #1, Tab(42); Using "##"; batterRatings(I, 7);
            Print #1, Tab(46); Using "##"; batterRatings(I, 8);
            Print #1, Tab(50); Using "##"; batterRatings(I, 9);
            Print #1, Tab(54); Using "###"; batterRatings(I, 10);
            Print #1, Tab(59); Using "###"; batterRatings(I, 11);
            Print #1, Tab(64); Using "#.###"; batterRatings(I, 12);
            Print #1, Tab(69); Using "#.###"; batterRatings(I, 13);
            Print #1, Tab(74); Using "#.###"; batterRatings(I, 14);

            I2 = 0
            I3 = 0
            I4 = 0

            If batterRatings(I, 4) > 0 Then
                I2 = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                I3 = Int(((batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)) / batterRatings(I, 4) + .0005) * 1000)
                I4 = Int(((batterRatings(I, 6) + batterRatings(I, 11)) / (batterRatings(I, 11) + batterRatings(I, 4)) + .0005) * 1000)
            End If

            If batterRatings(I, 4) = 0 Or I2 = 0 Then
                Print #1, Tab(79); ".000";
            End If

            If I2 > 0 And I2 < 100 Then
                Print #1, Tab(79); ".0";
                Print #1, Using "##"; I2;
            End If

            If I2 >= 100 Then
                Print #1, Tab(79); Using ".###"; I2;
            End If

            If batterRatings(I, 4) = 0 Or I3 = 0 Then
                Print #1, Tab(84); ".000";
            End If

            If I3 > 0 And I3 < 100 Then
                Print #1, Tab(84); ".0";
                Print #1, Using "##"; I3;
            End If

            If I3 >= 100 Then
                Print #1, Tab(84); Using ".###"; I3;
            End If

            If batterRatings(I, 4) = 0 Or I4 = 0 Then
                Print #1, Tab(90); ".000";
            End If

            If I4 > 0 And I4 < 100 Then
                Print #1, Tab(90); ".0";
                Print #1, Using "##"; I4;
            End If

            If I4 >= 100 Then
                Print #1, Tab(90); ".";
                Print #1, Using "###"; I4;
            End If

            Print #1, Tab(99); Using ".###"; batterRatings(I, 26)
            Print #1, Tab(16); "LGE.";
            Print #1, Tab(21); Using "###"; statBatter(I, 14);
            Print #1, Tab(26); Using "###"; statBatter(I, 0);
            Print #1, Tab(31); Using "###"; statBatter(I, 2);
            Print #1, Tab(36); Using "###"; statBatter(I, 1);
            Print #1, Tab(42); Using "##"; statBatter(I, 4);
            Print #1, Tab(46); Using "##"; statBatter(I, 5);
            Print #1, Tab(50); Using "##"; statBatter(I, 6);
            Print #1, Tab(54); Using "###"; statBatter(I, 3);
            Print #1, Tab(59); Using "###"; statBatter(I, 7);
            Print #1, Tab(64); Using "###"; statBatter(I, 8);
            Print #1, Tab(69); Using "###"; statBatter(I, 9);
            Print #1, Tab(74); Using "###"; statBatter(I, 10);

            I2 = 0
            I3 = 0
            I4 = 0

            If statBatter(I, 0) > 0 Then
                I2 = Int((statBatter(I, 1) / statBatter(I, 0) + .0005) * 1000)
                I3 = Int(((statBatter(I, 1) + statBatter(I, 4) + 2 * statBatter(I, 5) + 3 * statBatter(I, 6)) / statBatter(I, 0) + .0005) * 1000)
                I4 = Int(((statBatter(I, 1) + statBatter(I, 7)) / (statBatter(I, 7) + statBatter(I, 0)) + .0005) * 1000)
            End If

            If statBatter(I, 0) = 0 Or I2 = 0 Then
                Print #1, Tab(79); ".000";
            End If

            If I2 > 0 And I2 < 100 Then
                Print #1, Tab(79); ".0";
                Print #1, Using "##"; I2;
            End If
            If I2 >= 100 Then
                Print #1, Tab(79); Using ".###"; I2;
            End If

            If statBatter(I, 0) = 0 Or I3 = 0 Then
                Print #1, Tab(84); ".000";
            End If

            If I3 > 0 And I3 < 100 Then
                Print #1, Tab(84); ".0";
                Print #1, Using "##"; I3;
            End If

            If I3 >= 100 Then
                Print #1, Tab(84); Using ".###"; I3;
            End If

            If statBatter(I, 0) = 0 Or I4 = 0 Then
                Print #1, Tab(90); ".000";
            End If

            If I4 > 0 And I4 < 100 Then
                Print #1, Tab(90); ".0";: Print #1, Using "##"; I4;
            End If

            If I4 >= 100 Then
                Print #1, Tab(90); Using ".###"; I4;
            End If

            If statBatter(I, 16) = 0 And statBatter(I, 17) = 0 Then
                I5 = 0
            Else
                I5 = ((statBatter(I, 16) + statBatter(I, 17)) / (statBatter(I, 16) + statBatter(I, 17) + statBatter(I, 11))) * 1000
            End If

            If I5 = 0 Then
                Print #1, Tab(99); ".000  "
            End If
            If I5 > 0 And I5 < 1000 Then
                Print #1, Tab(99); Using ".###"; I5
            End If
            If I5 = 1000 Then
                Print #1, Tab(98); Using "#"; I5 / 1000;
                Print #1, ".000  "
            End If

            Print #1,

        End If

    Next I

    Call TeamComparison_Report(teamName$)

    Print #1, Chr$(12)
    Print #1, "STAT COMPARISONS FOR "; teamName$
    Print #1,
    Print #1,
    Print #1, "PITCHER";
    Print #1, Tab(25); "  G";
    Print #1, Tab(30); "  W";
    Print #1, Tab(35); "  L";
    Print #1, Tab(40); " SV";
    Print #1, Tab(47); " IP";
    Print #1, Tab(52); "  H";
    Print #1, Tab(56); " BB";
    Print #1, Tab(61); " SO";
    Print #1, Tab(67); "ERA";
    Print #1, Tab(72); " GS";
    Print #1, Tab(78); " CG";
    Print #1, Tab(85); "H/9IP";
    Print #1, Tab(92); "SO/9IP";
    Print #1, Tab(99); "BB/9IP";
    Print #1, Tab(107); "HR ALL."

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "END" And pitcherNames$(I) <> "" Then

            Print #1, Left$(pitcherNames$(I), 14);
            Print #1, Tab(18); "REG.";
            Print #1, Tab(25); Using "###"; pitcherRatings(I, 4);
            Print #1, Tab(31); Using "##"; pitcherRatings(I, 1);
            Print #1, Tab(36); Using "##"; pitcherRatings(I, 2);
            Print #1, Tab(41); Using "##"; pitcherRatings(I, 3);
            Print #1, Tab(45); Using "###.#"; pitcherRatings(I, 6);
            Print #1, Tab(52); Using "###"; pitcherRatings(I, 7);
            Print #1, Tab(56); Using "###"; pitcherRatings(I, 8);
            Print #1, Tab(61); Using "###"; pitcherRatings(I, 9);
            Print #1, Tab(66); Using "##.##"; (pitcherRatings(I, 10)) / 100;
            Print #1, Tab(73); Using "##"; pitcherRatings(I, 5);
            Print #1, Tab(79); Using "##"; pitcherRatings(I, 16);

            If pitcherRatings(I, 6) = 0 Then
                Print #1, Tab(85); " 0.00";
            End If
            If pitcherRatings(I, 6) > 0 Then
                Print #1, Tab(85); Using "##.##"; (pitcherRatings(I, 7) * 9) / pitcherRatings(I, 6);
            End If
            If pitcherRatings(I, 6) = 0 Then
                Print #1, Tab(93); " 0.00";
            End If
            If pitcherRatings(I, 6) > 0 Then
                Print #1, Tab(93); Using "##.##"; (pitcherRatings(I, 9) * 9) / pitcherRatings(I, 6);
            End If
            If pitcherRatings(I, 6) = 0 Then
                Print #1, Tab(100); " 0.00";
            End If
            If pitcherRatings(I, 6) > 0 Then
                Print #1, Tab(100); Using "##.##"; (pitcherRatings(I, 8) * 9) / pitcherRatings(I, 6);
            End If

            Print #1, Tab(109); Using "##"; pitcherRatings(I, 13)
            Print #1, Tab(18); "LGE.";
            Print #1, Tab(25); Using "###"; statPitcher(I, 8);
            Print #1, Tab(31); Using "##"; statPitcher(I, 6);
            Print #1, Tab(36); Using "##"; statPitcher(I, 7);
            Print #1, Tab(41); Using "##"; statPitcher(I, 12);
            Print #1, Tab(45); Using "###"; Int(statPitcher(I, 0) / 3);

            If statPitcher(I, 0) - (Int(statPitcher(I, 0) / 3) * 3) > 0 Then
                Print #1, ".";
                Print #1, Using "#"; statPitcher(I, 0) - (Int(statPitcher(I, 0) / 3) * 3);
            End If

            Print #1, Tab(52); Using "###"; statPitcher(I, 1);
            Print #1, Tab(56); Using "###"; statPitcher(I, 4);
            Print #1, Tab(61); Using "###"; statPitcher(I, 5);

            If statPitcher(I, 0) > 0 Then
                I2 = Int((((statPitcher(I, 3) * 9) / (statPitcher(I, 0) / 3)) + .0005) * 100)
            Else
                I2 = 0
            End If

            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statPitcher(I, 0) = 0 Or I2 = 0 Then
                Print #1, Tab(66); " 0.00";
            End If
            If statPitcher(I, 0) > 0 And I2 > 0 And I2 <= 999 Then
                Print #1, Tab(66); Using "##"; I3;
                Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I2 >= 1000 Then
                Print #1, Tab(66); Using "##"; I3;
                Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I4 = 0 And I2 > 0 Then
                Print #1, "00";
            End If
            If statPitcher(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
                Print #1, "0";
                Print #1, Using "#"; I4;
            End If
            If statPitcher(I, 0) > 0 And I4 >= 10 And I2 > 0 Then
                Print #1, Using "##"; I4;
            End If

            Print #1, Tab(73); Using "##"; statPitcher(I, 9);
            Print #1, Tab(79); Using "##"; statPitcher(I, 10);

            If statPitcher(I, 0) > 0 Then
                I2 = Int((((statPitcher(I, 1) * 9) / (statPitcher(I, 0) / 3)) + .0005) * 100)
            Else
                I2 = 0
            End If

            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statPitcher(I, 0) = 0 Or I2 = 0 Then
                Print #1, Tab(85); " 0.00";
            End If
            If statPitcher(I, 0) > 0 And I2 > 0 And I2 <= 999 Then
                Print #1, Tab(85); Using "##"; I3;
                Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I2 >= 1000 Then
                Print #1, Tab(85); Using "##"; I3;
                Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I4 = 0 And I2 > 0 Then
                Print #1, "00";
            End If
            If statPitcher(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
                Print #1, "0";
                Print #1, Using "#"; I4;
            End If
            If statPitcher(I, 0) > 0 And I4 >= 10 And I2 > 0 Then
                Print #1, Using "##"; I4;
            End If

            I2 = 0
            If statPitcher(I, 0) > 0 Then I2 = Int((((statPitcher(I, 5) * 9) / (statPitcher(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statPitcher(I, 0) = 0 Or I2 = 0 Then
                Print #1, Tab(93); " 0.00";
            End If
            If statPitcher(I, 0) > 0 And I2 > 0 And I2 <= 999 Then
                Print #1, Tab(93); Using "##"; I3;
                Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I2 >= 1000 Then
                Print #1, Tab(93); Using "##"; I3;
                Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I4 = 0 And I2 > 0 Then
                Print #1, "00";
            End If
            If statPitcher(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
                Print #1, "0";
                Print #1, Using "#"; I4;
            End If
            If statPitcher(I, 0) > 0 And I4 >= 10 And I2 > 0 Then
                Print #1, Using "##"; I4;
            End If

            If statPitcher(I, 0) > 0 Then
                I2 = Int((((statPitcher(I, 4) * 9) / (statPitcher(I, 0) / 3)) + .0005) * 100)
            Else
                I2 = 0
            End If

            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statPitcher(I, 0) = 0 Or I2 = 0 Then
                Print #1, Tab(100); " 0.00";
            End If
            If statPitcher(I, 0) > 0 And I2 > 0 And I2 <= 999 Then
                Print #1, Tab(100); Using "##"; I3;: Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I2 >= 1000 Then
                Print #1, Tab(100); Using "##"; I3;: Print #1, ".";
            End If
            If statPitcher(I, 0) > 0 And I4 = 0 And I2 > 0 Then
                Print #1, "00";
            End If
            If statPitcher(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
                Print #1, "0";: Print #1, Using "#"; I4;
            End If
            If statPitcher(I, 0) > 0 And I4 >= 10 And I2 > 0 Then
                Print #1, Using "##"; I4;
            End If

            Print #1, Tab(109); Using "##"; statPitcher(I, 14)
            Print #1,

        End If

    Next

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
'     Comparisons_Screen Subroutine
'----------------------------------------
'This subroutine provides a brief menu of
'player comparisons that can be done, and
'then formats stats for those options for
'use with the user display
Sub Comparisons_Screen (teamName$, backToMenu)

    Shared batterNames$(), pitcherNames$()

    Call TeamComparison_Screen(teamName$)

    Do

        For I = 0 To 22
            If Left$(batterNames$(I), 3) = "XXX" Then
                batterNames$(I) = ""
            End If
        Next

        For I = 0 To 21
            If Left$(pitcherNames$(I), 3) = "XXX" Then
                pitcherNames$(I) = ""
            End If
        Next

        Color 0, 0
        Cls

        Color 9
        Locate 2, 22: Print "STAT COMPARISONS FOR ";
        Color 15: Print teamName$

        For I = 0 To 16
            Color 12
            Locate I + 4, 10: Print Using "##"; I + 1;
            Print ". ";
            Color 7: Print Left$(batterNames$(I), 14)
        Next

        For I = 17 To 22
            Color 12
            Locate I - 13, 30: Print Using "##"; I + 1;
            Print ". ";
            Color 7: Print Left$(batterNames$(I), 14)
        Next

        For I = 0 To 10
            Color 12
            Locate I + 10, 30: Print Using "##"; I + 24;
            Print ". ";
            Color 3: Print Left$(pitcherNames$(I), 14)
        Next

        For I = 11 To 21
            Color 12
            Locate I - 7, 50: Print Using "##"; I + 24;
            Print ". ";
            Color 3: Print Left$(pitcherNames$(I), 14)
        Next

        Color 12
        Locate , 50: Print "46. ";
        Color 14: Print "View Players in Sequence"
        Color 12
        Locate , 50: Print "47. ";
        Color 14: Print "View Another Team"
        Color 12
        Locate , 50: Print "48. ";
        Color 14: Print "Exit to DOS"
        Color 12
        Locate , 50: Print "49. ";
        Color 14: Print "Return to Main Menu"

        Do

            Color 15
            Locate 23, 30
            Input "ENTER # TO VIEW "; B9

            startI = 0

            Select Case B9

                Case 1 To 23

                    viewPitch = 0
                    viewMode = 0
                    startI = B9 - 1

                    Cls

                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next

                Case 24 To 45

                    viewPitch = 1
                    viewMode = 0
                    startI = B9 - 24

                    Cls

                    For dc = 1 To 14
                        Color 15, 0: Locate dc, 1
                        Print String$(78, 177)
                    Next

                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next

                Case 46

                    viewPitch = 1
                    viewMode = 1
                    startI = 0

                    Cls

                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next


                Case 47:
                    backToMenu = 1

                Case 48:
                    Cls
                    End
                    System

                Case 49:
                    Exit Sub

            End Select

        Loop Until B9 >= 1 And B9 <= 49

        If B9 <> 49 And backToMenu <> 1 Then

            If viewPitch = 0 Or viewMode = 1 Then

                For I = startI To 22

                    Locate 2, 30
                    Color 15, 0: Print String$(40, 177)

                    If batterNames$(I) <> "XXX" And batterNames$(I) <> "END" And batterNames$(I) <> "" Then

                        Call BatterComparisons_Screen(I, teamName$)

                        Color 15, 0
                        Locate LAST_ROW - 2, 4: Print "HIT ANY KEY TO CONTINUE"

                        If viewMode = 1 Then
                            Locate LAST_ROW - 2, 28: Print "OR ESC TO RETURN TO MENU"
                        End If

                        I$ = GetKeyPress$

                        'Hit ESC or only viewing one player
                        If I$ = Chr$(27) Or viewMode = 0 Then
                            'skip viewing pitchers
                            viewPitch = 0
                            Exit For
                        End If

                    End If

                Next

            End If

            If viewPitch = 1 Or viewMode = 1 Then

                Cls

                For dc = 1 To 14
                    Color 15, 0: Locate dc, 1
                    Print String$(78, 177)
                Next

                If viewMode = 1 Then startI = 0

                For I = startI To 21

                    If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "END" And pitcherNames$(I) <> "" Then

                        Call PitcherComparisons_Screen(I, teamName$)

                        Color 15, 0
                        Locate LAST_ROW - 2, 4: Print "HIT ANY KEY TO CONTINUE"

                        If viewMode = 1 Then
                            Locate LAST_ROW - 2, 28: Print "OR ESC TO RETURN TO MENU"
                        End If

                        I$ = GetKeyPress$

                        'Hit ESC or only viewing one player
                        If I$ = Chr$(27) Or viewMode = 0 Then
                            Exit For
                        End If

                    End If 'Done checking for invalid chars

                Next

            End If

        End If 'Done checking for return to menu

    Loop Until B9 = 49 Or backToMenu = 1

End Sub


'----------------------------------------
'   BatterComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the output
'to screen for a batter, to compare their
'stats to actual values
Sub BatterComparisons_Screen (playerIdx, teamName$)

    Shared batterNames$()
    Shared batterRatings(), statBatter()

    Select Case batterRatings(playerIdx, 22)
        Case 2: PST$ = "Catcher"
        Case 3: PST$ = "First Base"
        Case 4: PST$ = "Second Base"
        Case 5: PST$ = "Third Base"
        Case 6: PST$ = "Shortstop"
        Case 7: PST$ = "Left Field"
        Case 8: PST$ = "Center Field"
        Case 9: PST$ = "Right Field"
    End Select

    Color 11
    Locate 2, 2: Print teamName$
    Color 15, 0
    Locate 2, 30: Print String$(14, 177)
    Color 10
    Locate 2, 30: Print Left$(batterNames$(playerIdx), 14);: Print " - ";: Print PST$

    Color 12
    Locate 5, 2: Print "ACTUAL "
    Locate 6, 2: Print "REPLAY ";

    Color 7, 0
    Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)

    Color 0, 11
    Locate 4, 13: Print "  G  ";:
    Print Tab(19); "AB ";:
    Print Tab(24); "R  ";:
    Print Tab(28); "H  ";:
    Print Tab(31); "2B ";:
    Print Tab(35); "3B ";:
    Print Tab(39); "HR ";:
    Print Tab(43); "RBI ";:
    Print Tab(48); "BB ";:
    Print Tab(52); "SO ";:
    Print Tab(56); "SB ";:
    Print Tab(59); "CS ";:
    Print Tab(63); "AVG ";:
    Print Tab(68); "SLG ";:
    Print Tab(73); "OBP";:

    Color 0, 11
    Locate 9, 13
    Print "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
    Locate 10, 13
    Print "  RUNS";:

    Print Tab(21); " HITS";:
    Print Tab(30); " 2B ";:
    Print Tab(37); " 3B ";:
    Print Tab(44); " HR ";:
    Print Tab(50); " RBI ";:
    Print Tab(58); " BB ";:
    Print Tab(65); " SO ";:
    Print Tab(70); " %-SB ";

    Color 7, 0: Print String$(2, 177)

    Color 0, 11
    Locate 15, 13: Print "FLD.PCT."

    Color 1, 11
    Locate 5, 13: Print Using "###"; batterRatings(playerIdx, 3);

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 5) = 0 Then
        Locate 11, 13: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 5) > 0 Then
        Locate 11, 13: Print Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 5)
    End If

    Locate 5, 16: Print Using "#####"; batterRatings(playerIdx, 4)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 6) = 0 Then
        Locate 11, 19: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 6) > 0 Then
        Locate 11, 19: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 6)
    End If

    Locate 5, 21: Print Using "####"; batterRatings(playerIdx, 5)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 7) = 0 Then
        Locate 11, 26: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 7) > 0 Then
        Locate 11, 26: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 7)
    End If

    Locate 5, 25: Print Using "####"; batterRatings(playerIdx, 6)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 8) = 0 Then
        Locate 11, 33: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 8) > 0 Then
        Locate 11, 33: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 8)
    End If

    Locate 5, 29: Print Using "####"; batterRatings(playerIdx, 7)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 9) = 0 Then
        Locate 11, 40: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 9) > 0 Then
        Locate 11, 40: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 9)
    End If

    Locate 5, 33: Print Using "####"; batterRatings(playerIdx, 8)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 10) = 0 Then
        Locate 11, 47: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 10) > 0 Then
        Locate 11, 47: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 10)
    End If

    Locate 5, 37: Print Using "####"; batterRatings(playerIdx, 9)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 11) = 0 Then
        Locate 11, 54: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 11) > 0 Then
        Locate 11, 54: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 11)
    End If

    Locate 5, 41: Print Using "#####"; batterRatings(playerIdx, 10)

    If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 12) = 0 Then
        Locate 11, 61: Print " ---.--"
    End If
    If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 12) > 0 Then
        Locate 11, 61: Print Using "####.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 12)
    End If

    Locate 5, 46: Print Using "####"; batterRatings(playerIdx, 11)
    Locate 5, 50: Print Using "####"; batterRatings(playerIdx, 12)

    If batterRatings(playerIdx, 13) + batterRatings(playerIdx, 14) = 0 Or batterRatings(playerIdx, 13) = 0 Then
        Locate 11, 68
        Print "    00% ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 1, 11
    If batterRatings(playerIdx, 13) > 0 And batterRatings(playerIdx, 14) = 0 Then
        Locate 11, 68
        Print "   100% ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 1, 11
    If batterRatings(playerIdx, 13) > 0 And batterRatings(playerIdx, 14) > 0 Then
        Locate 11, 68: Print "   ";: Print Using "###"; (batterRatings(playerIdx, 13) / (batterRatings(playerIdx, 13) + batterRatings(playerIdx, 14))) * 100;: Print "% ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 1, 11
    Locate 5, 54: Print Using "####"; batterRatings(playerIdx, 13)
    Locate 5, 58: Print Using "###"; batterRatings(playerIdx, 14);: Print " "

    I2 = 0
    I3 = 0
    I4 = 0

    If batterRatings(playerIdx, 4) > 0 Then
        I2 = Int((batterRatings(playerIdx, 6) / batterRatings(playerIdx, 4) + .0005) * 1000)
        I3 = Int(((batterRatings(playerIdx, 6) + batterRatings(playerIdx, 7) + 2 * batterRatings(playerIdx, 8) + 3 * batterRatings(playerIdx, 9)) / batterRatings(playerIdx, 4) + .0005) * 1000)
        I4 = Int(((batterRatings(playerIdx, 6) + batterRatings(playerIdx, 11)) / (batterRatings(playerIdx, 11) + batterRatings(playerIdx, 4)) + .0005) * 1000)
    End If
    If batterRatings(playerIdx, 4) = 0 Or I2 = 0 Then
        Locate 5, 62
        Print ".000";: Print " "
    End If

    If I2 > 0 And I2 < 100 Then
        Locate 5, 62
        Print ".0";: Print Using "##"; I2;: Print " "
    End If
    If I2 >= 100 Then
        Locate 5, 62
        Print ".";: Print Using "###"; I2;: Print " "
    End If

    If batterRatings(playerIdx, 4) = 0 Or I3 = 0 Then
        Locate 5, 67
        Print ".000";: Print " "
    End If

    If I3 > 0 And I3 < 100 Then
        Locate 5, 67
        Print ".0";: Print Using "##"; I3;: Print " "
    End If
    If I3 >= 100 Then
        Locate 5, 67
        Print ".";: Print Using "###"; I3;: Print " "
    End If

    If batterRatings(playerIdx, 4) = 0 Or I4 = 0 Then
        Locate 5, 72
        Print ".000";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 1, 11
    If I4 > 0 And I4 < 100 Then
        Locate 5, 72
        Print ".0"; Using "##"; I4;
        Color 7, 0: Print String$(2, 177)
    End If

    Color 1, 11
    If I4 >= 100 Then
        Locate 5, 72
        Print "."; Using "###"; I4;
        Color 7, 0: Print String$(2, 177)
    End If

    Locate 7, 15
    Color 7, 0: Print String$(63, 177)

    Color 1, 11
    Locate 16, 13
    Print "  ."; Using "###"; batterRatings(playerIdx, 26);
    Print "  ";

    Color 7, 0: Print String$(2, 177)

    Color 12
    Locate 11, 2: Print "ACTUAL  "
    Locate 12, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 13, 4: Print String$(8, 177)

    Color 12
    Locate 16, 2: Print "ACTUAL  "
    Locate 17, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 18, 4: Print String$(8, 177)

    Color 4, 11
    Locate 6, 13: Print Using "###"; statBatter(playerIdx, 14);

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 2) = 0 Then
        Locate 12, 13: Print "---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 2) > 0 Then
        Locate 12, 13: Print Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 2)
    End If

    Locate 6, 16: Print Using "#####"; statBatter(playerIdx, 0)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 1) = 0 Then
        Locate 12, 19: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 1) > 0 Then
        Locate 12, 19: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 1)
    End If

    Locate 6, 21: Print Using "####"; statBatter(playerIdx, 2)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 4) = 0 Then
        Locate 12, 26: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 4) > 0 Then
        Locate 12, 26: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 4)
    End If

    Locate 6, 25: Print Using "####"; statBatter(playerIdx, 1)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 5) = 0 Then
        Locate 12, 33: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 5) > 0 Then
        Locate 12, 33: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 5)
    End If

    Locate 6, 29: Print Using "####"; statBatter(playerIdx, 4)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 6) = 0 Then
        Locate 12, 40: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 6) > 0 Then
        Locate 12, 40: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 6)
    End If

    Locate 6, 33: Print Using "####"; statBatter(playerIdx, 5)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 3) = 0 Then
        Locate 12, 47: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 3) > 0 Then
        Locate 12, 47: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 3)
    End If

    Locate 6, 37: Print Using "####"; statBatter(playerIdx, 6)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 7) = 0 Then
        Locate 12, 54: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 7) > 0 Then
        Locate 12, 54: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 7)
    End If

    Locate 6, 41: Print Using "#####"; statBatter(playerIdx, 3)

    If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 8) = 0 Then
        Locate 12, 61: Print " ---.--"
    End If
    If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 8) > 0 Then
        Locate 12, 61: Print Using "####.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 8)
    End If

    Locate 6, 46: Print Using "####"; statBatter(playerIdx, 7)
    Locate 6, 50: Print Using "####"; statBatter(playerIdx, 8)

    If statBatter(playerIdx, 9) + statBatter(playerIdx, 10) = 0 Or statBatter(playerIdx, 9) = 0 Then
        Locate 12, 68: Print "    00% ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    If statBatter(playerIdx, 9) > 0 And statBatter(playerIdx, 10) = 0 Then
        Locate 12, 68: Print "   100% ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    If statBatter(playerIdx, 9) > 0 And statBatter(playerIdx, 10) > 0 Then
        Locate 12, 68: Print "   ";: Print Using "###"; (statBatter(playerIdx, 9) / (statBatter(playerIdx, 10) + statBatter(playerIdx, 9))) * 100;: Print "% ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    Locate 6, 54
    Print Using "####"; statBatter(playerIdx, 9)
    Locate 6, 58
    Print Using "###"; statBatter(playerIdx, 10);: Print " "

    I2 = 0
    I3 = 0
    I4 = 0

    If statBatter(playerIdx, 0) > 0 Then
        I2 = Int((statBatter(playerIdx, 1) / statBatter(playerIdx, 0) + .0005) * 1000)
        I3 = Int(((statBatter(playerIdx, 1) + statBatter(playerIdx, 4) + 2 * statBatter(playerIdx, 5) + 3 * statBatter(playerIdx, 6)) / statBatter(playerIdx, 0) + .0005) * 1000)
        I4 = Int(((statBatter(playerIdx, 1) + statBatter(playerIdx, 7)) / (statBatter(playerIdx, 7) + statBatter(playerIdx, 0)) + .0005) * 1000)
    End If

    If statBatter(playerIdx, 0) = 0 Or I2 = 0 Then
        Locate 6, 62: Print ".000";: Print " "
    End If

    If I2 > 0 And I2 < 100 Then
        Locate 6, 62: Print ".0";: Print Using "##"; I2;: Print " "
    End If
    If I2 >= 100 Then
        Locate 6, 62: Print ".";: Print Using "###"; I2;: Print " "
    End If

    If statBatter(playerIdx, 0) = 0 Or I3 = 0 Then
        Locate 6, 67: Print ".000";: Print " "
    End If

    If I3 > 0 And I3 < 100 Then
        Locate 6, 67: Print ".0";: Print Using "##"; I3;: Print " "
    End If
    If I3 >= 100 Then
        Locate 6, 67: Print ".";: Print Using "###"; I3;: Print " "
    End If

    If statBatter(playerIdx, 0) = 0 Or I4 = 0 Then
        Locate 6, 72: Print ".000";: Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    If I4 > 0 And I4 < 100 Then
        Locate 6, 72
        Print ".0";: Print Using "##"; I4;
        Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    If I4 >= 100 Then
        Locate 6, 72
        Print ".";: Print Using "###"; I4;
        Color 7, 0: Print String$(2, 177)
    End If

    Color 7, 0
    Locate 13, 15: Print String$(63, 177)

    If statBatter(playerIdx, 16) = 0 And statBatter(playerIdx, 17) = 0 Then
        I5 = 0
    Else
        I5 = ((statBatter(playerIdx, 16) + statBatter(playerIdx, 17)) / (statBatter(playerIdx, 16) + statBatter(playerIdx, 17) + statBatter(playerIdx, 11))) * 1000
    End If

    Color 4, 11
    If I5 = 0 Then
        Locate 17, 13
        Print "  .000  ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    If I5 > 0 And I5 < 1000 Then
        Locate 17, 13: Print "  .";
        Print Using "###"; I5;
        Print "  ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 4, 11
    If I5 = 1000 Then
        Locate 17, 13
        Print Using "##"; I5 / 1000;: Print ".000  ";
        Color 7, 0: Print String$(2, 177)
    End If

    Color 7, 0
    Locate 18, 15: Print String$(8, 177)

End Sub


'----------------------------------------
'  PitcherComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the output
'to screen for a pitcher, to compare their
'stats to actual values
Sub PitcherComparisons_Screen (playerIdx, teamName$)

    Shared pitcherNames$()
    Shared statPitcher(), pitcherRatings()

    Color 11
    Locate 2, 2: Print teamName$

    Color 15, 0

    Locate 2, 30: Print String$(14, 177)
    Color 13
    Locate 2, 30: Print Left$(pitcherNames$(playerIdx), 14)

    Color 10

    Locate 5, 2: Print "ACTUAL  "
    Locate 6, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)

    Color 0, 12
    Locate 4, 14

    Print "  G ";:
    Print Tab(20); " W ";:
    Print Tab(25); " L ";:
    Print Tab(30); " SV ";:
    Print Tab(37); " IP ";:
    Print Tab(43); " H  ";:
    Print Tab(48); " BB ";:
    Print Tab(53); " SO ";:
    Print Tab(58); " ERA ";:
    Print Tab(63); " GS";:
    Print Tab(66); " CG"

    Color 10, 0
    Locate 10, 2: Print "ACTUAL  "
    Locate 11, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    Color 0, 12
    Locate 9, 14

    Print " H/9IP ";:
    Print Tab(21); " SO/9IP ";:
    Print Tab(29); " BB/9IP ";:
    Print Tab(37); " HR ALL";:

    Color 14, 12

    Locate 5, 14: Print Using "###"; pitcherRatings(playerIdx, 4);:
    Locate , 17: Print Using "#####"; pitcherRatings(playerIdx, 1);:
    Locate , 22: Print Using "#####"; pitcherRatings(playerIdx, 2);:
    Locate , 27: Print Using "######"; pitcherRatings(playerIdx, 3);:
    Locate , 33: Print Using "#####.#"; pitcherRatings(playerIdx, 6);:
    Locate , 40: Print Using "#####"; pitcherRatings(playerIdx, 7);:
    Locate , 45: Print Using "######"; pitcherRatings(playerIdx, 8);:
    Locate , 51: Print Using "#####"; pitcherRatings(playerIdx, 9);:
    Locate , 56: Print Using "###.##"; (pitcherRatings(playerIdx, 10)) / 100;:
    Locate , 62: Print Using "####"; pitcherRatings(playerIdx, 5);:
    Locate , 66: Print Using "###"; pitcherRatings(playerIdx, 16);

    Color 7, 0: Print String$(2, 177)

    Color 14, 12

    If pitcherRatings(playerIdx, 6) = 0 Then
        Locate 10, 14: Print "  0.00";
    End If
    If pitcherRatings(playerIdx, 6) > 0 Then
        Locate 10, 14: Print Using "###.##"; (pitcherRatings(playerIdx, 7) * 9) / pitcherRatings(playerIdx, 6)
    End If

    If pitcherRatings(playerIdx, 6) = 0 Then
        Locate 10, 20: Print "    0.00";
    End If
    If pitcherRatings(playerIdx, 6) > 0 Then
        Locate 10, 20: Print Using "#####.##"; (pitcherRatings(playerIdx, 9) * 9) / pitcherRatings(playerIdx, 6)
    End If

    If pitcherRatings(playerIdx, 6) = 0 Then
        Locate 10, 28: Print "    0.00";
    End If
    If pitcherRatings(playerIdx, 6) > 0 Then
        Locate 10, 28: Print Using "#####.##"; (pitcherRatings(playerIdx, 8) * 9) / pitcherRatings(playerIdx, 6)
    End If

    Locate 10, 36
    Print Using "########"; pitcherRatings(playerIdx, 13);: Color 7, 0: Print String$(2, 177)

    Color 15, 12
    Locate 6, 14: Print Using "###"; statPitcher(playerIdx, 8);:
    Locate , 17: Print Using "#####"; statPitcher(playerIdx, 6);:
    Locate , 22: Print Using "#####"; statPitcher(playerIdx, 7);:
    Locate , 27: Print Using "######"; statPitcher(playerIdx, 12);:
    Locate , 33: Print Using "#####"; Int(statPitcher(playerIdx, 0) / 3);

    If statPitcher(playerIdx, 0) - (Int(statPitcher(playerIdx, 0) / 3) * 3) = 0 Then
        Print ".0";
    End If
    If statPitcher(playerIdx, 0) - (Int(statPitcher(playerIdx, 0) / 3) * 3) > 0 Then
        Print ".";: Print Using "#"; statPitcher(playerIdx, 0) - (Int(statPitcher(playerIdx, 0) / 3) * 3);
    End If

    Locate , 40: Print Using "#####"; statPitcher(playerIdx, 1);:
    Locate , 45: Print Using "######"; statPitcher(playerIdx, 4);:
    Locate , 51: Print Using "#####"; statPitcher(playerIdx, 5);:

    If statPitcher(playerIdx, 0) > 0 Then
        I2 = Int((((statPitcher(playerIdx, 3) * 9) / (statPitcher(playerIdx, 0) / 3)) + .0005) * 100)
    Else
        I2 = 0
    End If

    I3 = Int(I2 / 100)
    I4 = I2 - (I3 * 100)

    If statPitcher(playerIdx, 0) = 0 Or I2 = 0 Then
        Locate 6, 56: Print "  0.00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 > 0 And I2 <= 999 Then
        Locate 6, 56: Print Using "###"; I3;: Print ".";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 >= 1000 Then
        Locate 6, 56: Print Using "###"; I3;: Print ".";
    End If

    If statPitcher(playerIdx, 0) > 0 And I4 = 0 And I2 > 0 Then
        Print "00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
        Print "0";: Print Using "#"; I4;
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 >= 10 And I2 > 0 Then
        Print Using "##"; I4;
    End If

    Locate 6, 62
    Print Using "####"; statPitcher(playerIdx, 9);
    Locate , 66
    Print Using "###"; statPitcher(playerIdx, 10);: Color 7, 0: Print String$(2, 177)

    Locate 7, 16: Print String$(55, 177)

    Color 15, 12

    I2 = 0
    If statPitcher(playerIdx, 0) > 0 Then I2 = Int((((statPitcher(playerIdx, 1) * 9) / (statPitcher(playerIdx, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100)
    I4 = I2 - (I3 * 100)

    If statPitcher(playerIdx, 0) = 0 Or I2 = 0 Then
        Locate 11, 14: Print "  0.00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 > 0 And I2 <= 999 Then
        Locate 11, 14: Print Using "###"; I3;: Print ".";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 >= 1000 Then
        Locate 11, 14: Print Using "###"; I3;: Print ".";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 = 0 And I2 > 0 Then
        Print "00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
        Print "0";: Print Using "#"; I4;
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 >= 10 And I2 > 0 Then
        Print Using "##"; I4;
    End If

    I2 = 0
    If statPitcher(playerIdx, 0) > 0 Then I2 = Int((((statPitcher(playerIdx, 5) * 9) / (statPitcher(playerIdx, 0) / 3)) + .0005) * 100)
    I3 = Int(I2 / 100)
    I4 = I2 - (I3 * 100)

    If statPitcher(playerIdx, 0) = 0 Or I2 = 0 Then
        Locate 11, 20: Print "    0.00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 > 0 And I2 <= 999 Then
        Locate 11, 20: Print Using "#####"; I3;: Print ".";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 >= 1000 Then
        Locate 11, 20: Print Using "#####"; I3;: Print ".";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 = 0 And I2 > 0 Then
        Print "00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
        Print "0";: Print Using "#"; I4;
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 >= 10 And I2 > 0 Then
        Print Using "##"; I4;
    End If

    If statPitcher(playerIdx, 0) > 0 Then
        I2 = Int((((statPitcher(playerIdx, 4) * 9) / (statPitcher(playerIdx, 0) / 3)) + .0005) * 100)
    Else
        I2 = 0
    End If
    I3 = Int(I2 / 100)
    I4 = I2 - (I3 * 100)

    If statPitcher(playerIdx, 0) = 0 Or I2 = 0 Then
        Locate 11, 28: Print "    0.00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I2 > 0 And I2 <= 999 Then
        Locate 11, 28: Print Using "#####"; I3;: Print ".";
    End If

    If statPitcher(playerIdx, 0) > 0 And I2 >= 1000 Then
        Locate 11, 28: Print Using "#####"; I3;: Print ".";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 = 0 And I2 > 0 Then
        Print "00";
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then
        Print "0";: Print Using "#"; I4;
    End If
    If statPitcher(playerIdx, 0) > 0 And I4 >= 10 And I2 > 0 Then
        Print Using "##"; I4;
    End If

    Locate 11, 36
    Print Using "########"; statPitcher(playerIdx, 14);

    Color 7, 0: Print String$(2, 177)
    Locate 12, 16: Print String$(30, 177)

End Sub


'----------------------------------------
'   TeamComparison_Report Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the default printer
Sub TeamComparison_Report (teamName$)

    Shared batterNames$()
    Shared batterRatings(), statBatter()

    Print #1, Chr$(12)
    Print #1, "STAT COMPARISONS FOR "; teamName$
    Print #1,
    Print #1,
    Print #1, Tab(20); " [ - - - - - - - - - - AT BATS PER - - - - - - - - - - - ]"
    Print #1, "BATTER";
    Print #1, Tab(26); "R";
    Print #1, Tab(33); "H";
    Print #1, Tab(40); "2B";
    Print #1, Tab(47); "3B";
    Print #1, Tab(54); "HR";
    Print #1, Tab(61); "RBI";
    Print #1, Tab(68); "BB";
    Print #1, Tab(75); "SO";
    Print #1, Tab(79); "SUCCESS % - SB"

    For playerIdx = 0 To 16

        If batterNames$(playerIdx) <> "XXX" And batterNames$(playerIdx) = "END" And batterNames$(playerIdx) <> "" Then

            Print #1, Left$(batterNames$(playerIdx), 14);
            Print #1, Tab(16); "REG.";

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 5) = 0 Then
                Print #1, Tab(21); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 5) > 0 Then
                Print #1, Tab(21); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 5);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 6) = 0 Then
                Print #1, Tab(28); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 6) > 0 Then
                Print #1, Tab(28); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 6);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 7) = 0 Then
                Print #1, Tab(36); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 7) > 0 Then
                Print #1, Tab(36); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 7);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 8) = 0 Then
                Print #1, Tab(43); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 8) > 0 Then
                Print #1, Tab(43); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 8);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 9) = 0 Then
                Print #1, Tab(50); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 9) > 0 Then
                Print #1, Tab(50); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 9);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 10) = 0 Then
                Print #1, Tab(58); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 10) > 0 Then
                Print #1, Tab(58); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 10);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 11) = 0 Then
                Print #1, Tab(64); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 11) > 0 Then
                Print #1, Tab(64); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 11);
            End If

            If batterRatings(playerIdx, 4) = 0 Or batterRatings(playerIdx, 12) = 0 Then
                Print #1, Tab(71); "---.--";
            End If
            If batterRatings(playerIdx, 4) > 0 And batterRatings(playerIdx, 12) > 0 Then
                Print #1, Tab(71); Using "###.##"; batterRatings(playerIdx, 4) / batterRatings(playerIdx, 12);
            End If

            If batterRatings(playerIdx, 13) + batterRatings(playerIdx, 14) = 0 Or batterRatings(playerIdx, 13) = 0 Then
                Print #1, Tab(83); " 00%";
            End If
            If batterRatings(playerIdx, 13) > 0 And batterRatings(playerIdx, 14) = 0 Then
                Print #1, Tab(83); "100%";
            End If
            If batterRatings(playerIdx, 13) > 0 And batterRatings(playerIdx, 14) > 0 Then
                Print #1, Tab(83); Using "###"; (batterRatings(playerIdx, 13) / (batterRatings(playerIdx, 13) + batterRatings(playerIdx, 14))) * 100;
                Print #1, Tab(86); "%"
            End If

            Print #1, Tab(16); "LGE.";

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 2) = 0 Then
                Print #1, Tab(21); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 2) > 0 Then
                Print #1, Tab(21); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 2);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 1) = 0 Then
                Print #1, Tab(28); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 1) > 0 Then
                Print #1, Tab(28); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 1);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 4) = 0 Then
                Print #1, Tab(36); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 4) > 0 Then
                Print #1, Tab(36); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 4);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 5) = 0 Then
                Print #1, Tab(43); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 5) > 0 Then
                Print #1, Tab(43); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 5);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 6) = 0 Then
                Print #1, Tab(50); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 6) > 0 Then
                Print #1, Tab(50); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 6);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 3) = 0 Then
                Print #1, Tab(58); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 3) > 0 Then
                Print #1, Tab(58); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 3);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 7) = 0 Then
                Print #1, Tab(64); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 7) > 0 Then
                Print #1, Tab(64); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 7);
            End If

            If statBatter(playerIdx, 0) = 0 Or statBatter(playerIdx, 8) = 0 Then
                Print #1, Tab(71); "---.--";
            End If
            If statBatter(playerIdx, 0) > 0 And statBatter(playerIdx, 8) > 0 Then
                Print #1, Tab(71); Using "###.##"; statBatter(playerIdx, 0) / statBatter(playerIdx, 8);
            End If

            If statBatter(playerIdx, 9) + statBatter(playerIdx, 10) = 0 Or statBatter(playerIdx, 9) = 0 Then
                Print #1, Tab(83); " 00%"
            End If

            If statBatter(playerIdx, 9) > 0 And statBatter(playerIdx, 10) = 0 Then
                Print #1, Tab(83); "100%"
            End If
            If statBatter(playerIdx, 9) > 0 And statBatter(playerIdx, 10) > 0 Then
                Print #1, Tab(83); Using "###"; (statBatter(playerIdx, 9) / (statBatter(playerIdx, 10) + statBatter(playerIdx, 9))) * 100;
                Print #1, Tab(86); "%"
            End If

        End If

    Next

End Sub


'----------------------------------------
'   TeamComparison_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub TeamComparison_Screen (teamName$)

    Shared teamRatings(), totBatRat()
    Shared statTeamBat!(), statTeamPit!()
    Shared totPitRat&()

    Shared earnedRuns!

    Cls

    For dc = 1 To 23
        Color 15, 0: Locate dc, 1
        Print String$(79, 177)
    Next

    Locate 1, 35
    Color teamRatings(11), teamRatings(12): Print teamName$

    Color 10, 0
    Locate 3, 2: Print "BATTING "

    Color 12
    Locate 4, 2: Print "ACTUAL  ";

    Color 7, 0: Print String$(2, 177)

    Color 12
    Locate 5, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 6, 4: Print String$(8, 177)

    Color 0, 11
    Locate 3, 13: Print "  AB";
    Print Tab(18); "  R";
    Print Tab(23); "  H";
    Print Tab(27); " 2B";
    Print Tab(31); " 3B";
    Print Tab(35); " HR";
    Print Tab(39); " RBI";
    Print Tab(43); " BB";
    Print Tab(49); " SO";
    Print Tab(53); " SB";
    Print Tab(57); " CS";
    Print Tab(61); " AVG";
    Print Tab(66); " SLG";
    Print Tab(72); "OBP";

    Color 10, 0
    Locate 9, 2: Print "BATTING "

    Color 12, 0
    Locate 10, 2: Print "ACTUAL  ";

    Color 7, 0: Print String$(2, 177)

    Color 12
    Locate 11, 2
    Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    Color 0, 11
    Locate 8, 13: Print "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
    Locate 9, 13: Print "  RUNS";:
    Print Tab(21); " HITS";:
    Print Tab(30); " 2B ";:
    Print Tab(37); " 3B ";:
    Print Tab(44); " HR ";:
    Print Tab(50); " RBI ";:
    Print Tab(58); " BB ";:
    Print Tab(65); " SO ";:
    Print Tab(70); " %-SB ";

    Color 7, 0: Print String$(2, 177)
    Color 1, 11

    Locate 4, 13: Print Using "####"; totBatRat(1);
    Locate , 17: Print Using "####"; totBatRat(2);
    Locate , 21: Print Using "#####"; totBatRat(3);
    Locate , 26: Print Using "####"; totBatRat(4);
    Locate , 30: Print Using "####"; totBatRat(5);
    Locate , 34: Print Using "####"; totBatRat(6);
    Locate , 38: Print Using "#####"; totBatRat(7);
    Locate , 43: Print Using "####"; totBatRat(8);
    Locate , 47: Print Using "#####"; totBatRat(9);
    Locate , 52: Print Using "####"; totBatRat(10);
    Locate , 56: Print Using "####"; totBatRat(11);: Print " ";

    statInnPitchCT! = totBatRat(3) / totBatRat(1)
    Locate , 61: Print Using ".###"; statInnPitchCT!;: Print " ";

    statInnPitchCT! = (totBatRat(3) + totBatRat(4) + totBatRat(5) * 2 + totBatRat(6) * 3) / totBatRat(1)
    Locate , 66: Print Using ".###"; statInnPitchCT!;: Print " ";

    statInnPitchCT! = (totBatRat(3) + totBatRat(8)) / (totBatRat(1) + totBatRat(8))
    Locate , 71: Print Using ".###"; statInnPitchCT!;

    Color 7, 0
    Print String$(2, 177)

    Color 4, 11
    Locate 5, 13: Print Using "####"; statTeamBat!(0);
    Locate , 17: Print Using "####"; statTeamBat!(2);
    Locate , 21: Print Using "#####"; statTeamBat!(1);
    Locate , 26: Print Using "####"; statTeamBat!(4);
    Locate , 30: Print Using "####"; statTeamBat!(5);
    Locate , 34: Print Using "####"; statTeamBat!(6);
    Locate , 38: Print Using "#####"; statTeamBat!(3);
    Locate , 43: Print Using "####"; statTeamBat!(7);
    Locate , 47: Print Using "#####"; statTeamBat!(8);
    Locate , 52: Print Using "####"; statTeamBat!(9);
    Locate , 56: Print Using "####"; statTeamBat!(10);
    Print " ";

    statInnPitchCT! = statTeamBat!(1) / statTeamBat!(0)
    Locate , 61: Print Using ".###"; statInnPitchCT!;: Print " ";

    statInnPitchCT! = (statTeamBat!(1) + statTeamBat!(4) + statTeamBat!(5) * 2 + statTeamBat!(6) * 3) / statTeamBat!(0)
    Locate , 66: Print Using ".###"; statInnPitchCT!;: Print " ";

    statInnPitchCT! = (statTeamBat!(1) + statTeamBat!(7)) / (statTeamBat!(0) + statTeamBat!(7))
    Locate , 71: Print Using ".###"; statInnPitchCT!;

    Color 7, 0
    Print String$(2, 177)

    Locate , 15: Print String$(62, 177)

    Color 1, 11

    Locate 10, 13
    If totBatRat(2) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(2) > 0 Then
        Print Using "###.##"; totBatRat(1) / totBatRat(2);
    End If

    Locate , 18
    If totBatRat(3) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(3) > 0 Then
        Print Using "#####.##"; totBatRat(1) / totBatRat(3);
    End If

    Locate , 26
    If totBatRat(4) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(4) > 0 Then
        Print Using "####.##"; totBatRat(1) / totBatRat(4);
    End If

    Locate , 33
    If totBatRat(5) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(5) > 0 Then
        Print Using "####.##"; totBatRat(1) / totBatRat(5);
    End If

    Locate , 40
    If totBatRat(6) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(6) > 0 Then
        Print Using "####.##"; totBatRat(1) / totBatRat(6);
    End If

    Locate , 47
    If totBatRat(7) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(7) > 0 Then
        Print Using "####.##"; totBatRat(1) / totBatRat(7);
    End If

    Locate , 54
    If totBatRat(8) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(8) > 0 Then
        Print Using "####.##"; totBatRat(1) / totBatRat(8);
    End If

    Locate , 61
    If totBatRat(9) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(9) > 0 Then
        Print Using "####.##"; totBatRat(1) / totBatRat(9);
    End If

    Locate , 68
    If totBatRat(10) = 0 Then
        Print " ---.--";
    End If
    If totBatRat(10) > 0 Then
        Print Using "####.##"; totBatRat(10) / (totBatRat(10) + totBatRat(11));
    End If

    Print " ";

    Color 7, 0
    Print String$(2, 177)

    Color 4, 11

    Locate 11, 13
    If statTeamBat!(2) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(2) > 0 Then
        Print Using "###.##"; statTeamBat!(0) / statTeamBat!(2);
    End If

    Locate , 18
    If statTeamBat!(1) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(1) > 0 Then
        Print Using "#####.##"; statTeamBat!(0) / statTeamBat!(1);
    End If

    Locate , 26
    If statTeamBat!(4) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(4) > 0 Then
        Print Using "####.##"; statTeamBat!(0) / statTeamBat!(4);
    End If

    Locate , 33
    If statTeamBat!(5) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(5) > 0 Then
        Print Using "####.##"; statTeamBat!(0) / statTeamBat!(5);
    End If

    Locate , 40
    If statTeamBat!(6) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(6) > 0 Then
        Print Using "####.##"; statTeamBat!(0) / statTeamBat!(6);
    End If

    Locate , 47
    If statTeamBat!(3) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(3) > 0 Then
        Print Using "####.##"; statTeamBat!(0) / statTeamBat!(3);
    End If

    Locate , 54
    If statTeamBat!(7) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(7) > 0 Then
        Print Using "####.##"; statTeamBat!(0) / statTeamBat!(7);
    End If

    Locate , 61
    If statTeamBat!(8) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(8) > 0 Then
        Print Using "####.##"; statTeamBat!(0) / statTeamBat!(8);
    End If

    Locate , 68
    If statTeamBat!(9) = 0 Then
        Print " ---.--";
    End If
    If statTeamBat!(9) > 0 Then
        Print Using "####.## "; statTeamBat!(9) / (statTeamBat!(9) + statTeamBat!(10));
    End If

    Color 7, 0

    Print String$(2, 177)
    Locate , 15: Print String$(63, 177)

    Color 10, 0
    Locate 14, 2: Print "PITCHING"

    Color 12, 0
    Locate 15, 2: Print "ACTUAL  ";

    Color 7, 0
    Print String$(2, 177)

    Color 12
    Locate 16, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 17, 4: Print String$(8, 177)

    Color 0, 12

    Locate 14, 14: Print "  SV";
    Print Tab(20); " IP";
    Print Tab(25); "  H";
    Print Tab(30); "  BB";
    Print Tab(37); "  SO";
    Print Tab(43); " HRA";
    Print Tab(48); "  CG";
    Print Tab(53); "  ERA";

    Color 10, 0
    Locate 19, 2: Print "PITCHING"

    Color 12, 0
    Locate 20, 2: Print "ACTUAL  ";

    Color 7, 0
    Print String$(2, 177)

    Color 12
    Locate 21, 2: Print "REPLAY  ";

    Color 7, 0
    Print String$(2, 177)

    Locate 22, 4: Print String$(8, 177)

    Color 0, 12

    Locate 19, 14: Print " H/9IP ";
    Print Tab(21); " SO/9IP ";
    Print Tab(29); " BB/9IP";

    Color 15, 12
    Locate 15, 14: Print Using "####"; totPitRat&(3);
    Locate , 18: Print Using "#####"; totPitRat&(6);
    Locate , 23: Print Using "#####"; totPitRat&(7);
    Locate , 28: Print Using "######"; totPitRat&(8);
    Locate , 34: Print Using "#######"; totPitRat&(9);
    Locate , 41: Print Using "######"; totPitRat&(13);
    Locate , 47: Print Using "#####"; totPitRat&(16);
    Locate , 52: Print Using "###.##"; (earnedRuns! * 9) / totPitRat&(6);

    Color 7, 0
    Print String$(2, 177)

    Color 15, 12
    Locate 20, 14: Print Using "###.##"; totPitRat&(7) * 9 / totPitRat&(6);
    Locate , 20: Print Using "#####.##"; totPitRat&(9) * 9 / totPitRat&(6);
    Locate , 28: Print Using "#####.##"; totPitRat&(8) * 9 / totPitRat&(6);

    Color 7, 0
    Print String$(2, 177)

    Color 14, 12
    Locate 16, 14: Print Using "####"; statTeamPit!(12);
    Locate , 18: Print Using "#####"; statTeamPit!(0) / 3;
    Locate , 23: Print Using "#####"; statTeamPit!(1);
    Locate , 28: Print Using "######"; statTeamPit!(4);
    Locate , 34: Print Using "#######"; statTeamPit!(5);
    Locate , 41: Print Using "######"; statTeamPit!(14);
    Locate , 47: Print Using "#####"; statTeamPit!(10);
    Locate , 52: Print Using "###.##"; (statTeamPit!(3) * 9) / (statTeamPit!(0) / 3);

    Color 7, 0: Print String$(2, 177)

    Locate , 16

    Color 7, 0: Print String$(44, 177)

    Color 14, 12
    Locate 21, 14: Print Using "###.##"; statTeamPit!(1) * 9 / (statTeamPit!(0) / 3);
    Locate , 20: Print Using "#####.##"; statTeamPit!(5) * 9 / (statTeamPit!(0) / 3);
    Locate , 28: Print Using "#####.##"; statTeamPit!(4) * 9 / (statTeamPit!(0) / 3);

    Color 7, 0
    Print String$(2, 177)

    Locate , 16

    Color 7, 0
    Print String$(22, 177)

    Color 15, 0
    Locate LAST_ROW - 2, 48: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

    Erase totBatRat, totPitRat&

End Sub
