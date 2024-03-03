'----------------------------------------
'           COMPARE Subroutine
'----------------------------------------
'This subroutine loads in existing stats
'for a team and provides different options
'to the end user for formatting stats
Sub COMPARE ()

    _Title "Full Count Baseball - Stats Comparison"

    Shared ERX!

    Shared statB0%(), statP0%()
    Shared batterRatings(), teamRatings%(), pitcherRatings(), TB%()
    Shared statT0%(), statT1%()

    Shared TP&()

    Shared batterNames$(), pitcherNames$()

    Dim compareB1%(23), compareP1%(22)

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        For K = 4 To 18:
            For I = 0 To 22
                TB%(K - 3) = TB%(K - 3) + batterRatings(I, K)
            Next
        Next

        For K = 1 To 33
            For I = 0 To 21
                TP&(K) = TP&(K) + pitcherRatings(I, K)
            Next
        Next

        ERX! = 0

        For I = 0 To 21
            ERX! = ERX! + ((pitcherRatings(I, 6) / 9) * (pitcherRatings(I, 10) / 100))
        Next

        For K = 4 To 14:
            For I = 0 To 21
                If pitcherRatings(I, 11) = 999 Then TB%(K - 3) = TB%(K - 3) + pitcherRatings(I, K + 16)
            Next
        Next

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, team$
        Input #1, W2%
        Input #1, L2%

        For I = 0 To 22:
            Input #1, batterNames$(I)
            Input #1, compareB1%(I)
        Next I

        For I = 0 To 21:
            Input #1, pitcherNames$(I)
            Input #1, compareP1%(I)
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
            Input #1, statT0%(I)
        Next I
        
        For I = 0 To 22: 
            Input #1, statT1%(I)
        Next I

        Close #1

        Color 15, 0
        Cls

        Call box0 (8, 8, 13, 70, 2, 15, 1)

        Do
            Locate 10
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";
            Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";
            Color 7: Print "RINTER"
            J$ = GetKeyPress$

        Loop Until UCase$(J$) = "P" Or UCase$(J$) = "S" Or J$ = Chr$(27)

        backToMenu = 0

        If J$ <> Chr$(27) Then

            Select Case UCase$(J$)

                Case "P":
                    Call Comparisons_Printer(team$)

                Case "S":
                    Call Comparisons_Screen(team$, backToMenu)

            End Select

        End If

    End If 'Done checking file count

End Sub


'----------------------------------------
'     Comparisons_Printer Subroutine
'----------------------------------------
'This subroutine formats team stats for
'output to the default printer
Sub Comparisons_Printer (teamName$)

    Shared batterNames$(), pitcherNames$()
    Shared batterRatings(), pitcherRatings()
    Shared statB0%(), statP0%()

    LPrint "STAT COMPARISONS FOR "; teamName$
    LPrint
    LPrint
    LPrint "BATTER";
    LPrint Tab(21); "  G";
    LPrint Tab(26); " AB";
    LPrint Tab(31); "  R";
    LPrint Tab(36); "  H";
    LPrint Tab(41); " 2B";
    LPrint Tab(45); " 3B";
    LPrint Tab(49); " HR";
    LPrint Tab(54); "RBI";
    LPrint Tab(59); " BB";
    LPrint Tab(64); " SO";
    LPrint Tab(69); " SB";
    LPrint Tab(74); " CS";
    LPrint Tab(79); "AVG";
    LPrint Tab(85); "SLG";
    LPrint Tab(91); "OBP";
    LPrint Tab(97); "FLD.PCT"

    For I = 0 To 22

        If batterNames$(I) <> "XXX" And batterNames$(I) <> "END" And batterNames$(I) <> "" Then

            LPrint Left$(batterNames$(I), 14);
            LPrint Tab(16); "REG.";
            LPrint Tab(21); Using "###"; batterRatings(I, 3);
            LPrint Tab(26); Using "###"; batterRatings(I, 4);
            LPrint Tab(31); Using "###"; batterRatings(I, 5);
            LPrint Tab(36); Using "###"; batterRatings(I, 6);
            LPrint Tab(42); Using "##"; batterRatings(I, 7);
            LPrint Tab(46); Using "##"; batterRatings(I, 8);
            LPrint Tab(50); Using "##"; batterRatings(I, 9);
            LPrint Tab(54); Using "###"; batterRatings(I, 10);
            LPrint Tab(59); Using "###"; batterRatings(I, 11);
            LPrint Tab(64); Using "#.###"; batterRatings(I, 12);
            LPrint Tab(69); Using "#.###"; batterRatings(I, 13);
            LPrint Tab(74); Using "#.###"; batterRatings(I, 14);

            I2 = 0
            I3 = 0
            I4 = 0

            If batterRatings(I, 4) > 0 Then 
                I2 = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                I3 = Int(((batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)) / batterRatings(I, 4) + .0005) * 1000)
                I4 = Int(((batterRatings(I, 6) + batterRatings(I, 11)) / (batterRatings(I, 11) + batterRatings(I, 4)) + .0005) * 1000)
            End If
            If batterRatings(I, 4) = 0 Or I2 = 0 Then LPrint Tab(79); ".000";

            If I2 > 0 And I2 < 100 Then LPrint Tab(79); ".0";: LPrint Using "##"; I2;
            If I2 >= 100 Then LPrint Tab(79); ".";: LPrint Using "###"; I2;

            If batterRatings(I, 4) = 0 Or I3 = 0 Then LPrint Tab(84); ".000";

            If I3 > 0 And I3 < 100 Then LPrint Tab(84); ".0";: LPrint Using "##"; I3;
            If I3 >= 100 Then LPrint Tab(84); ".";: LPrint Using "###"; I3;

            If batterRatings(I, 4) = 0 Or I4 = 0 Then LPrint Tab(90); ".000";

            If I4 > 0 And I4 < 100 Then LPrint Tab(90); ".0";: LPrint Using "##"; I4;
            If I4 >= 100 Then LPrint Tab(90); ".";: LPrint Using "###"; I4;

            LPrint Tab(99); ".";: LPrint Using "###"; batterRatings(I, 26)
            LPrint Tab(16); "LGE.";
            LPrint Tab(21); Using "###"; statB0%(I, 14);
            LPrint Tab(26); Using "###"; statB0%(I, 0);
            LPrint Tab(31); Using "###"; statB0%(I, 2);
            LPrint Tab(36); Using "###"; statB0%(I, 1);
            LPrint Tab(42); Using "##"; statB0%(I, 4);
            LPrint Tab(46); Using "##"; statB0%(I, 5);
            LPrint Tab(50); Using "##"; statB0%(I, 6);
            LPrint Tab(54); Using "###"; statB0%(I, 3);
            LPrint Tab(59); Using "###"; statB0%(I, 7);
            LPrint Tab(64); Using "###"; statB0%(I, 8);
            LPrint Tab(69); Using "###"; statB0%(I, 9);
            LPrint Tab(74); Using "###"; statB0%(I, 10);

            I2 = 0
            I3 = 0
            I4 = 0

            If statB0%(I, 0) > 0 Then 
                I2 = Int((statB0%(I, 1) / statB0%(I, 0) + .0005) * 1000)
                I3 = Int(((statB0%(I, 1) + statB0%(I, 4) + 2 * statB0%(I, 5) + 3 * statB0%(I, 6)) / statB0%(I, 0) + .0005) * 1000)
                I4 = Int(((statB0%(I, 1) + statB0%(I, 7)) / (statB0%(I, 7) + statB0%(I, 0)) + .0005) * 1000)
            End If

            If statB0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(79); ".000";

            If I2 > 0 And I2 < 100 Then LPrint Tab(79); ".0";: LPrint Using "##"; I2;
            If I2 >= 100 Then LPrint Tab(79); ".";: LPrint Using "###"; I2;

            If statB0%(I, 0) = 0 Or I3 = 0 Then LPrint Tab(84); ".000";

            If I3 > 0 And I3 < 100 Then LPrint Tab(84); ".0";: LPrint Using "##"; I3;
            If I3 >= 100 Then LPrint Tab(84); ".";: LPrint Using "###"; I3;

            If statB0%(I, 0) = 0 Or I4 = 0 Then LPrint Tab(90); ".000";

            If I4 > 0 And I4 < 100 Then LPrint Tab(90); ".0";: LPrint Using "##"; I4;
            If I4 >= 100 Then LPrint Tab(90); ".";: LPrint Using "###"; I4;

            If statB0%(I, 16) = 0 And statB0%(I, 17) = 0 Then I5 = 0 Else I5 = ((statB0%(I, 16) + statB0%(I, 17)) / (statB0%(I, 16) + statB0%(I, 17) + statB0%(I, 11))) * 1000

            If I5 = 0 Then LPrint Tab(99); ".000  "
            If I5 > 0 And I5 < 1000 Then LPrint Tab(99); ".";: LPrint Using "###"; I5
            If I5 = 1000 Then LPrint Tab(98); Using "#"; I5 / 1000;: LPrint ".000  "

            LPrint

        End If

    Next I

    Call TeamComparison_Printer(teamName$)

    LPrint Chr$(12)
    LPrint "STAT COMPARISONS FOR "; teamName$
    LPrint
    LPrint
    LPrint "PITCHER";
    LPrint Tab(25); "  G";
    LPrint Tab(30); "  W";
    LPrint Tab(35); "  L";
    LPrint Tab(40); " SV";
    LPrint Tab(47); " IP";
    LPrint Tab(52); "  H";
    LPrint Tab(56); " BB";
    LPrint Tab(61); " SO";
    LPrint Tab(67); "ERA";
    LPrint Tab(72); " GS";
    LPrint Tab(78); " CG";
    LPrint Tab(85); "H/9IP";
    LPrint Tab(92); "SO/9IP";
    LPrint Tab(99); "BB/9IP";
    LPrint Tab(107); "HR ALL."

    For I = 0 To 21

        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "END" And pitcherNames$(I) <> "" Then

            LPrint Left$(pitcherNames$(I), 14);
            LPrint Tab(18); "REG.";
            LPrint Tab(25); Using "###"; pitcherRatings(I, 4);
            LPrint Tab(31); Using "##"; pitcherRatings(I, 1);
            LPrint Tab(36); Using "##"; pitcherRatings(I, 2);
            LPrint Tab(41); Using "##"; pitcherRatings(I, 3);
            LPrint Tab(45); Using "###.#"; pitcherRatings(I, 6);
            LPrint Tab(52); Using "###"; pitcherRatings(I, 7);
            LPrint Tab(56); Using "###"; pitcherRatings(I, 8);
            LPrint Tab(61); Using "###"; pitcherRatings(I, 9);
            LPrint Tab(66); Using "##.##"; (pitcherRatings(I, 10)) / 100;
            LPrint Tab(73); Using "##"; pitcherRatings(I, 5);
            LPrint Tab(79); Using "##"; pitcherRatings(I, 16);

            If pitcherRatings(I, 6) = 0 Then LPrint Tab(85); " 0.00";
            If pitcherRatings(I, 6) > 0 Then LPrint Tab(85); Using "##.##"; (pitcherRatings(I, 7) * 9) / pitcherRatings(I, 6);
            If pitcherRatings(I, 6) = 0 Then LPrint Tab(93); " 0.00";
            If pitcherRatings(I, 6) > 0 Then LPrint Tab(93); Using "##.##"; (pitcherRatings(I, 9) * 9) / pitcherRatings(I, 6);
            If pitcherRatings(I, 6) = 0 Then LPrint Tab(100); " 0.00";
            If pitcherRatings(I, 6) > 0 Then LPrint Tab(100); Using "##.##"; (pitcherRatings(I, 8) * 9) / pitcherRatings(I, 6);

            LPrint Tab(109); Using "##"; pitcherRatings(I, 13)
            LPrint Tab(18); "LGE.";
            LPrint Tab(25); Using "###"; statP0%(I, 8);
            LPrint Tab(31); Using "##"; statP0%(I, 6);
            LPrint Tab(36); Using "##"; statP0%(I, 7);
            LPrint Tab(41); Using "##"; statP0%(I, 12);
            LPrint Tab(45); Using "###"; Int(statP0%(I, 0) / 3);

            If statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3);

            LPrint Tab(52); Using "###"; statP0%(I, 1);
            LPrint Tab(56); Using "###"; statP0%(I, 4);
            LPrint Tab(61); Using "###"; statP0%(I, 5);

            I2 = 0
            If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 3) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(66); " 0.00";
            If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(66); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(66); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            LPrint Tab(73); Using "##"; statP0%(I, 9);
            LPrint Tab(79); Using "##"; statP0%(I, 10);

            I2 = 0
            If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 1) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(85); " 0.00";
            If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(85); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(85); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            I2 = 0
            If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 5) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(93); " 0.00";
            If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(93); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(93); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            I2 = 0
            If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 4) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If statP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(100); " 0.00";
            If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(100); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(100); Using "##"; I3;: LPrint ".";
            If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            LPrint Tab(109); Using "##"; statP0%(I, 14)
            LPrint

        End If

    Next

    LPrint Chr$(12)

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
    Shared batterRatings(), statB0%()
    Shared statP0%(), pitcherRatings()

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
        Locate 1, 22: Print "STAT COMPARISONS FOR ";
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
            Locate 23, 30: Input "ENTER # TO VIEW "; B9

            startI = 0

            Select Case B9

                Case 1 To 23

                    startI = B9 - 1

                    Cls

                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next

                Case 24 To 45

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

                    BI = 1

                    Cls

                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next

                    startI = startI + 1

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

            For I = startI To 22

                Locate 2, 30
                Color 15, 0: Print String$(40, 177)

                If batterNames$(I) <> "XXX" And batterNames$(I) <> "END" And batterNames$(I) <> "" Then

                    Select Case batterRatings(I, 22)
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
                    Locate 2, 30: Print Left$(batterNames$(I), 14);: Print " - ";: Print PST$

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
                    Locate 5, 13: Print Using "###"; batterRatings(I, 3);

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 5) = 0 Then Locate 11, 13: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 5) > 0 Then Locate 11, 13: Print Using "###.##"; batterRatings(I, 4) / batterRatings(I, 5)

                    Locate 5, 16: Print Using "#####"; batterRatings(I, 4)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 6) = 0 Then Locate 11, 19: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 6) > 0 Then Locate 11, 19: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 6)

                    Locate 5, 21: Print Using "####"; batterRatings(I, 5)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 7) = 0 Then Locate 11, 26: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 7) > 0 Then Locate 11, 26: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 7)

                    Locate 5, 25: Print Using "####"; batterRatings(I, 6)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 8) = 0 Then Locate 11, 33: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 8) > 0 Then Locate 11, 33: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 8)

                    Locate 5, 29: Print Using "####"; batterRatings(I, 7)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 9) = 0 Then Locate 11, 40: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 9) > 0 Then Locate 11, 40: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 9)

                    Locate 5, 33: Print Using "####"; batterRatings(I, 8)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 10) = 0 Then Locate 11, 47: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 10) > 0 Then Locate 11, 47: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 10)

                    Locate 5, 37: Print Using "####"; batterRatings(I, 9)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 11) = 0 Then Locate 11, 54: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 11) > 0 Then Locate 11, 54: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 11)

                    Locate 5, 41: Print Using "#####"; batterRatings(I, 10)

                    If batterRatings(I, 4) = 0 Or batterRatings(I, 12) = 0 Then Locate 11, 61: Print " ---.--"
                    If batterRatings(I, 4) > 0 And batterRatings(I, 12) > 0 Then Locate 11, 61: Print Using "####.##"; batterRatings(I, 4) / batterRatings(I, 12)

                    Locate 5, 46: Print Using "####"; batterRatings(I, 11)
                    Locate 5, 50: Print Using "####"; batterRatings(I, 12)

                    If batterRatings(I, 13) + batterRatings(I, 14) = 0 Or batterRatings(I, 13) = 0 Then 
                        Locate 11, 68
                        Print "    00% ";
                        Color 7, 0: Print String$(2, 177)
                    End If

                    Color 1, 11
                    If batterRatings(I, 13) > 0 And batterRatings(I, 14) = 0 Then 
                        Locate 11, 68
                        Print "   100% ";
                        Color 7, 0: Print String$(2, 177)
                    End If
                    
                    Color 1, 11
                    If batterRatings(I, 13) > 0 And batterRatings(I, 14) > 0 Then 
                        Locate 11, 68: Print "   ";: Print Using "###"; (batterRatings(I, 13) / (batterRatings(I, 13) + batterRatings(I, 14))) * 100;: Print "% ";
                        Color 7, 0: Print String$(2, 177)
                    End If
                    
                    Color 1, 11
                    Locate 5, 54: Print Using "####"; batterRatings(I, 13)
                    Locate 5, 58: Print Using "###"; batterRatings(I, 14);: Print " "

                    I2 = 0
                    I3 = 0
                    I4 = 0

                    If batterRatings(I, 4) > 0 Then 
                        I2 = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                        I3 = Int(((batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)) / batterRatings(I, 4) + .0005) * 1000)
                        I4 = Int(((batterRatings(I, 6) + batterRatings(I, 11)) / (batterRatings(I, 11) + batterRatings(I, 4)) + .0005) * 1000)
                    End If
                    If batterRatings(I, 4) = 0 Or I2 = 0 Then 
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

                    If batterRatings(I, 4) = 0 Or I3 = 0 Then 
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

                    If batterRatings(I, 4) = 0 Or I4 = 0 Then 
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
                    Print "  ."; Using "###"; batterRatings(I, 26);
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

                    Color 14, 11
                    Locate 6, 13: Print Using "###"; statB0%(I, 14);

                    If statB0%(I, 0) = 0 Or statB0%(I, 2) = 0 Then Locate 12, 13: Print "---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 2) > 0 Then Locate 12, 13: Print Using "###.##"; statB0%(I, 0) / statB0%(I, 2)

                    Locate 6, 16: Print Using "#####"; statB0%(I, 0)

                    If statB0%(I, 0) = 0 Or statB0%(I, 1) = 0 Then Locate 12, 19: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 1) > 0 Then Locate 12, 19: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 1)

                    Locate 6, 21: Print Using "####"; statB0%(I, 2)

                    If statB0%(I, 0) = 0 Or statB0%(I, 4) = 0 Then Locate 12, 26: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 4) > 0 Then Locate 12, 26: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 4)

                    Locate 6, 25: Print Using "####"; statB0%(I, 1)

                    If statB0%(I, 0) = 0 Or statB0%(I, 5) = 0 Then Locate 12, 33: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 5) > 0 Then Locate 12, 33: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 5)

                    Locate 6, 29: Print Using "####"; statB0%(I, 4)

                    If statB0%(I, 0) = 0 Or statB0%(I, 6) = 0 Then Locate 12, 40: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 6) > 0 Then Locate 12, 40: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 6)

                    Locate 6, 33: Print Using "####"; statB0%(I, 5)

                    If statB0%(I, 0) = 0 Or statB0%(I, 3) = 0 Then Locate 12, 47: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 3) > 0 Then Locate 12, 47: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 3)

                    Locate 6, 37: Print Using "####"; statB0%(I, 6)

                    If statB0%(I, 0) = 0 Or statB0%(I, 7) = 0 Then Locate 12, 54: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 7) > 0 Then Locate 12, 54: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 7)

                    Locate 6, 41: Print Using "#####"; statB0%(I, 3)

                    If statB0%(I, 0) = 0 Or statB0%(I, 8) = 0 Then Locate 12, 61: Print " ---.--"
                    If statB0%(I, 0) > 0 And statB0%(I, 8) > 0 Then Locate 12, 61: Print Using "####.##"; statB0%(I, 0) / statB0%(I, 8)

                    Locate 6, 46: Print Using "####"; statB0%(I, 7)
                    Locate 6, 50: Print Using "####"; statB0%(I, 8)

                    If statB0%(I, 9) + statB0%(I, 10) = 0 Or statB0%(I, 9) = 0 Then 
                        Locate 12, 68: Print "    00% ";
                        Color 7, 0: Print String$(2, 177)
                    End If

                    Color 14, 11
                    If statB0%(I, 9) > 0 And statB0%(I, 10) = 0 Then 
                        Locate 12, 68: Print "   100% ";
                        Color 7, 0: Print String$(2, 177)
                    End If
                    
                    Color 14, 11
                    If statB0%(I, 9) > 0 And statB0%(I, 10) > 0 Then 
                        Locate 12, 68: Print "   ";: Print Using "###"; (statB0%(I, 9) / (statB0%(I, 10) + statB0%(I, 9))) * 100;: Print "% ";
                        Color 7, 0: Print String$(2, 177)
                    End If
                    
                    Color 14, 11
                    Locate 6, 54
                    Print Using "####"; statB0%(I, 9)
                    Locate 6, 58
                    Print Using "###"; statB0%(I, 10);: Print " "

                    I2 = 0
                    I3 = 0
                    I4 = 0

                    If statB0%(I, 0) > 0 Then 
                        I2 = Int((statB0%(I, 1) / statB0%(I, 0) + .0005) * 1000)
                        I3 = Int(((statB0%(I, 1) + statB0%(I, 4) + 2 * statB0%(I, 5) + 3 * statB0%(I, 6)) / statB0%(I, 0) + .0005) * 1000)
                        I4 = Int(((statB0%(I, 1) + statB0%(I, 7)) / (statB0%(I, 7) + statB0%(I, 0)) + .0005) * 1000)
                    End If

                    If statB0%(I, 0) = 0 Or I2 = 0 Then Locate 6, 62: Print ".000";: Print " "

                    If I2 > 0 And I2 < 100 Then Locate 6, 62: Print ".0";: Print Using "##"; I2;: Print " "
                    If I2 >= 100 Then Locate 6, 62: Print ".";: Print Using "###"; I2;: Print " "

                    If statB0%(I, 0) = 0 Or I3 = 0 Then Locate 6, 67: Print ".000";: Print " "

                    If I3 > 0 And I3 < 100 Then Locate 6, 67: Print ".0";: Print Using "##"; I3;: Print " "
                    If I3 >= 100 Then Locate 6, 67: Print ".";: Print Using "###"; I3;: Print " "

                    If statB0%(I, 0) = 0 Or I4 = 0 Then Locate 6, 72: Print ".000";: Color 7, 0: Print String$(2, 177)

                    Color 14, 11
                    If I4 > 0 And I4 < 100 Then 
                        Locate 6, 72
                        Print ".0";: Print Using "##"; I4;
                        Color 7, 0: Print String$(2, 177)
                    End If

                    Color 14, 11
                    If I4 >= 100 Then 
                        Locate 6, 72
                        Print ".";: Print Using "###"; I4;
                        Color 7, 0: Print String$(2, 177)
                    End If

                    Color 7, 0
                    Locate 13, 15: Print String$(63, 177)

                    If statB0%(I, 16) = 0 And statB0%(I, 17) = 0 Then 
                        I5 = 0 
                    Else 
                        I5 = ((statB0%(I, 16) + statB0%(I, 17)) / (statB0%(I, 16) + statB0%(I, 17) + statB0%(I, 11))) * 1000
                    End If

                    Color 14, 11
                    If I5 = 0 Then 
                        Locate 17, 13
                        Print "  .000  ";
                        Color 7, 0: Print String$(2, 177)
                    End If

                    Color 14, 11
                    If I5 > 0 And I5 < 1000 Then 
                        Locate 17, 13: Print "  .";
                        Print Using "###"; I5;
                        Print "  ";
                        Color 7, 0: Print String$(2, 177)
                    End If

                    Color 14, 11
                    If I5 = 1000 Then 
                        Locate 17, 13
                        Print Using "##"; I5 / 1000;: Print ".000  ";
                        Color 7, 0: Print String$(2, 177)
                    End If
                    
                    Color 7, 0
                    Locate 18, 15: Print String$(8, 177)

                    Color 15, 0
                    Locate 23, 4: Print "HIT ANY KEY TO CONTINUE"

                    If BI = 1 Then
                        Locate 23, 28: Print "OR ESC TO RETURN TO MENU"
                    End If

                    I$ = GetKeyPress$

                    'This below is admittedly horrible
                    'It's basically several conditions for kicking us out of the loop
                    'However we've simply reached a point where we're presenting
                    'an option to continue paging through screens
                    'BI = 1 helps us know we're at the last screen
                    'But BI = 0 will also exit the loop; it just won't do anything outside
                    If I$ <> Chr$(27) Then

                        If BI = 0 Then Cls: Exit For 'Existing conditionals will skip later work so it loops around

                        If BI = 1 Then Exit For 'More will be done later

                        If I < 17 Then
                            Exit For
                        Else
                            BI = 1
                            Exit For
                        End If
                    Else
                        BI = 0
                        Exit For 'Get back to loop beginning
                    End If

                    If BI <> 1 Then
                        BI = 1
                        Exit For
                    End If

                End If

            Next

            If BI = 1 Then

                Cls

                For dc = 1 To 14
                    Color 15, 0: Locate dc, 1
                    Print String$(78, 177)
                Next

                For I = 0 To 21
                    If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "END" And pitcherNames$(I) <> "" Then

                        Color 11
                        Locate 2, 2: Print teamName$
                        
                        Color 15, 0
                        
                        Locate 2, 30: Print String$(14, 177)
                        Color 13
                        Locate 2, 30: Print Left$(pitcherNames$(I), 14)
                        
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

                        Locate 5, 14: Print Using "###"; pitcherRatings(I, 4);:
                        Locate , 17: Print Using "#####"; pitcherRatings(I, 1);:
                        Locate , 22: Print Using "#####"; pitcherRatings(I, 2);:
                        Locate , 27: Print Using "######"; pitcherRatings(I, 3);:
                        Locate , 33: Print Using "#####.#"; pitcherRatings(I, 6);:
                        Locate , 40: Print Using "#####"; pitcherRatings(I, 7);:
                        Locate , 45: Print Using "######"; pitcherRatings(I, 8);:
                        Locate , 51: Print Using "#####"; pitcherRatings(I, 9);:
                        Locate , 56: Print Using "###.##"; (pitcherRatings(I, 10)) / 100;:
                        Locate , 62: Print Using "####"; pitcherRatings(I, 5);:
                        Locate , 66: Print Using "###"; pitcherRatings(I, 16);: Color 7, 0: Print String$(2, 177)

                        Color 14, 12
                        
                        If pitcherRatings(I, 6) = 0 Then Locate 10, 14: Print "  0.00";
                        If pitcherRatings(I, 6) > 0 Then Locate 10, 14: Print Using "###.##"; (pitcherRatings(I, 7) * 9) / pitcherRatings(I, 6)
                        If pitcherRatings(I, 6) = 0 Then Locate 10, 20: Print "    0.00";
                        If pitcherRatings(I, 6) > 0 Then Locate 10, 20: Print Using "#####.##"; (pitcherRatings(I, 9) * 9) / pitcherRatings(I, 6)
                        If pitcherRatings(I, 6) = 0 Then Locate 10, 28: Print "    0.00";
                        If pitcherRatings(I, 6) > 0 Then Locate 10, 28: Print Using "#####.##"; (pitcherRatings(I, 8) * 9) / pitcherRatings(I, 6)

                        Locate 10, 36
                        Print Using "########"; pitcherRatings(I, 13);: Color 7, 0: Print String$(2, 177)

                        Color 15, 12
                        Locate 6, 14: Print Using "###"; statP0%(I, 8);:
                        Locate , 17: Print Using "#####"; statP0%(I, 6);:
                        Locate , 22: Print Using "#####"; statP0%(I, 7);:
                        Locate , 27: Print Using "######"; statP0%(I, 12);:
                        Locate , 33: Print Using "#####"; Int(statP0%(I, 0) / 3);

                        If statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3) = 0 Then Print ".0";
                        If statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; statP0%(I, 0) - (Int(statP0%(I, 0) / 3) * 3);

                        Locate , 40: Print Using "#####"; statP0%(I, 1);:
                        Locate , 45: Print Using "######"; statP0%(I, 4);:
                        Locate , 51: Print Using "#####"; statP0%(I, 5);:

                        I2 = 0
                        If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 3) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
                        I3 = Int(I2 / 100)
                        I4 = I2 - (I3 * 100)

                        If statP0%(I, 0) = 0 Or I2 = 0 Then Locate 6, 56: Print "  0.00";
                        If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 6, 56: Print Using "###"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I2 >= 1000 Then Locate 6, 56: Print Using "###"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                        If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                        If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                        Locate 6, 62
                        Print Using "####"; statP0%(I, 9);
                        Locate , 66
                        Print Using "###"; statP0%(I, 10);: Color 7, 0: Print String$(2, 177)
                        
                        Locate 7, 16: Print String$(55, 177)
                        
                        Color 15, 12

                        I2 = 0
                        If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 1) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
                        I3 = Int(I2 / 100)
                        I4 = I2 - (I3 * 100)

                        If statP0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 14: Print "  0.00";
                        If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 14: Print Using "###"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 14: Print Using "###"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                        If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                        If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                        I2 = 0
                        If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 5) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
                        I3 = Int(I2 / 100)
                        I4 = I2 - (I3 * 100)

                        If statP0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 20: Print "    0.00";
                        If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 20: Print Using "#####"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 20: Print Using "#####"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                        If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                        If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                        I2 = 0
                        If statP0%(I, 0) > 0 Then I2 = Int((((statP0%(I, 4) * 9) / (statP0%(I, 0) / 3)) + .0005) * 100)
                        I3 = Int(I2 / 100)
                        I4 = I2 - (I3 * 100)

                        If statP0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 28: Print "    0.00";
                        If statP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 28: Print Using "#####"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 28: Print Using "#####"; I3;: Print ".";
                        If statP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                        If statP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                        If statP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                        Locate 11, 36
                        Print Using "########"; statP0%(I, 14);
                        
                        Color 7, 0: Print String$(2, 177)
                        Locate 12, 16: Print String$(30, 177)

                        Color 15, 0
                        Locate 23, 4: Print "HIT ANY KEY TO CONTINUE"

                        If BI = 1 Then
                            Locate 23, 28: Print "OR ESC TO RETURN TO MENU"
                        End If

                        I$ = GetKeyPress$

                        If I$ <> Chr$(0) Then
                            If I$ = Chr$(27) Then Exit For
                            If BI <> 1 Then Exit For
                        End If

                    End If 'Done checking for invalid chars

                Next

            End If

            If BI = 1 Then
                BI = 0
            End If

        End If 'Done checking for return to menu

    Loop Until B9 = 49 Or backToMenu = 1

End Sub


'----------------------------------------
'   TeamComparison_Printer Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the default printer
Sub TeamComparison_Printer (teamName$)

    Shared batterNames$()
    Shared batterRatings(), statB0%()

    LPrint Chr$(12)
    LPrint "STAT COMPARISONS FOR "; teamName$
    LPrint
    LPrint
    LPrint Tab(20); " [ - - - - - - - - - - AT BATS PER - - - - - - - - - - - ]"
    LPrint "BATTER";
    LPrint Tab(26); "R";
    LPrint Tab(33); "H";
    LPrint Tab(40); "2B";
    LPrint Tab(47); "3B";
    LPrint Tab(54); "HR";
    LPrint Tab(61); "RBI";
    LPrint Tab(68); "BB";
    LPrint Tab(75); "SO";
    LPrint Tab(79); "SUCCESS % - SB"

    For I = 0 To 16

        If batterNames$(I) <> "XXX" And batterNames$(I) = "END" And batterNames$(I) <> "" Then

            LPrint Left$(batterNames$(I), 14);
            LPrint Tab(16); "REG.";

            If batterRatings(I, 4) = 0 Or batterRatings(I, 5) = 0 Then LPrint Tab(21); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 5) > 0 Then LPrint Tab(21); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 5);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 6) = 0 Then LPrint Tab(28); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 6) > 0 Then LPrint Tab(28); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 6);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 7) = 0 Then LPrint Tab(36); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 7) > 0 Then LPrint Tab(36); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 7);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 8) = 0 Then LPrint Tab(43); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 8) > 0 Then LPrint Tab(43); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 8);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 9) = 0 Then LPrint Tab(50); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 9) > 0 Then LPrint Tab(50); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 9);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 10) = 0 Then LPrint Tab(58); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 10) > 0 Then LPrint Tab(58); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 10);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 11) = 0 Then LPrint Tab(64); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 11) > 0 Then LPrint Tab(64); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 11);
            If batterRatings(I, 4) = 0 Or batterRatings(I, 12) = 0 Then LPrint Tab(71); "---.--";
            If batterRatings(I, 4) > 0 And batterRatings(I, 12) > 0 Then LPrint Tab(71); Using "###.##"; batterRatings(I, 4) / batterRatings(I, 12);
            If batterRatings(I, 13) + batterRatings(I, 14) = 0 Or batterRatings(I, 13) = 0 Then LPrint Tab(83); " 00%";
            If batterRatings(I, 13) > 0 And batterRatings(I, 14) = 0 Then LPrint Tab(83); "100%";
            If batterRatings(I, 13) > 0 And batterRatings(I, 14) > 0 Then LPrint Tab(83); Using "###"; (batterRatings(I, 13) / (batterRatings(I, 13) + batterRatings(I, 14))) * 100;: LPrint Tab(86); "%"

            LPrint Tab(16); "LGE.";

            If statB0%(I, 0) = 0 Or statB0%(I, 2) = 0 Then LPrint Tab(21); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 2) > 0 Then LPrint Tab(21); Using "###.##"; statB0%(I, 0) / statB0%(I, 2);
            If statB0%(I, 0) = 0 Or statB0%(I, 1) = 0 Then LPrint Tab(28); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 1) > 0 Then LPrint Tab(28); Using "###.##"; statB0%(I, 0) / statB0%(I, 1);
            If statB0%(I, 0) = 0 Or statB0%(I, 4) = 0 Then LPrint Tab(36); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 4) > 0 Then LPrint Tab(36); Using "###.##"; statB0%(I, 0) / statB0%(I, 4);
            If statB0%(I, 0) = 0 Or statB0%(I, 5) = 0 Then LPrint Tab(43); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 5) > 0 Then LPrint Tab(43); Using "###.##"; statB0%(I, 0) / statB0%(I, 5);
            If statB0%(I, 0) = 0 Or statB0%(I, 6) = 0 Then LPrint Tab(50); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 6) > 0 Then LPrint Tab(50); Using "###.##"; statB0%(I, 0) / statB0%(I, 6);
            If statB0%(I, 0) = 0 Or statB0%(I, 3) = 0 Then LPrint Tab(58); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 3) > 0 Then LPrint Tab(58); Using "###.##"; statB0%(I, 0) / statB0%(I, 3);
            If statB0%(I, 0) = 0 Or statB0%(I, 7) = 0 Then LPrint Tab(64); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 7) > 0 Then LPrint Tab(64); Using "###.##"; statB0%(I, 0) / statB0%(I, 7);
            If statB0%(I, 0) = 0 Or statB0%(I, 8) = 0 Then LPrint Tab(71); "---.--";
            If statB0%(I, 0) > 0 And statB0%(I, 8) > 0 Then LPrint Tab(71); Using "###.##"; statB0%(I, 0) / statB0%(I, 8);
            If statB0%(I, 9) + statB0%(I, 10) = 0 Or statB0%(I, 9) = 0 Then LPrint Tab(83); " 00%"
            If statB0%(I, 9) > 0 And statB0%(I, 10) = 0 Then LPrint Tab(83); "100%"
            If statB0%(I, 9) > 0 And statB0%(I, 10) > 0 Then LPrint Tab(83); Using "###"; (statB0%(I, 9) / (statB0%(I, 10) + statB0%(I, 9))) * 100;: LPrint Tab(86); "%"

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

    Shared teamRatings%(), TB%()
    Shared statT0%(), statT1%()
    Shared TP&()

    Shared ERX!

    Cls

    For dc = 1 To 23
        Color 15, 0: Locate dc, 1
        Print String$(79, 177)
    Next

    Locate 1, 35
    Color teamRatings%(11), teamRatings%(12): Print teamName$

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

    Locate 4, 13: Print Using "####"; TB%(1);
    Locate , 17: Print Using "####"; TB%(2);
    Locate , 21: Print Using "#####"; TB%(3);
    Locate , 26: Print Using "####"; TB%(4);
    Locate , 30: Print Using "####"; TB%(5);
    Locate , 34: Print Using "####"; TB%(6);
    Locate , 38: Print Using "#####"; TB%(7);
    Locate , 43: Print Using "####"; TB%(8);
    Locate , 47: Print Using "#####"; TB%(9);
    Locate , 52: Print Using "####"; TB%(10);
    Locate , 56: Print Using "####"; TB%(11);: Print " ";

    statPCT! = TB%(3) / TB%(1)
    Locate , 61: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (TB%(3) + TB%(4) + TB%(5) * 2 + TB%(6) * 3) / TB%(1)
    Locate , 66: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (TB%(3) + TB%(8)) / (TB%(1) + TB%(8))
    Locate , 71: Print Using ".###"; statPCT!;

    Color 7, 0
    Print String$(2, 177)

    Color 14, 11
    Locate 5, 13: Print Using "####"; statT0%(0);
    Locate , 17: Print Using "####"; statT0%(2);
    Locate , 21: Print Using "#####"; statT0%(1);
    Locate , 26: Print Using "####"; statT0%(4);
    Locate , 30: Print Using "####"; statT0%(5);
    Locate , 34: Print Using "####"; statT0%(6);
    Locate , 38: Print Using "#####"; statT0%(3);
    Locate , 43: Print Using "####"; statT0%(7);
    Locate , 47: Print Using "#####"; statT0%(8);
    Locate , 52: Print Using "####"; statT0%(9);
    Locate , 56: Print Using "####"; statT0%(10);
    Print " ";

    statPCT! = statT0%(1) / statT0%(0)
    Locate , 61: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (statT0%(1) + statT0%(4) + statT0%(5) * 2 + statT0%(6) * 3) / statT0%(0)
    Locate , 66: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (statT0%(1) + statT0%(7)) / (statT0%(0) + statT0%(7))
    Locate , 71: Print Using ".###"; statPCT!;

    Color 7, 0
    Print String$(2, 177)

    Locate , 15: Print String$(62, 177)

    Color 1, 11
    If TB%(2) = 0 Then Locate 10, 13: Print " ---.--";
    If TB%(2) > 0 Then Locate 10, 13: Print Using "###.##"; TB%(1) / TB%(2);
    If TB%(3) = 0 Then Locate , 18: Print " ---.--";
    If TB%(3) > 0 Then Locate , 18: Print Using "#####.##"; TB%(1) / TB%(3);
    If TB%(4) = 0 Then Locate , 26: Print " ---.--";
    If TB%(4) > 0 Then Locate , 26: Print Using "####.##"; TB%(1) / TB%(4);
    If TB%(5) = 0 Then Locate , 33: Print " ---.--";
    If TB%(5) > 0 Then Locate , 33: Print Using "####.##"; TB%(1) / TB%(5);
    If TB%(6) = 0 Then Locate , 40: Print " ---.--";
    If TB%(6) > 0 Then Locate , 40: Print Using "####.##"; TB%(1) / TB%(6);
    If TB%(7) = 0 Then Locate , 47: Print " ---.--";
    If TB%(7) > 0 Then Locate , 47: Print Using "####.##"; TB%(1) / TB%(7);
    If TB%(8) = 0 Then Locate , 54: Print " ---.--";
    If TB%(8) > 0 Then Locate , 54: Print Using "####.##"; TB%(1) / TB%(8);
    If TB%(9) = 0 Then Locate , 61: Print " ---.--";
    If TB%(9) > 0 Then Locate , 61: Print Using "####.##"; TB%(1) / TB%(9);
    If TB%(10) = 0 Then Locate , 68: Print " ---.--";
    If TB%(10) > 0 Then Locate , 68: Print Using "####.##"; TB%(10) / (TB%(10) + TB%(11));
    Print " ";

    Color 7, 0
    Print String$(2, 177)

    Color 14, 11

    If statT0%(2) = 0 Then Locate 11, 13: Print " ---.--";
    If statT0%(2) > 0 Then Locate 11, 13: Print Using "###.##"; statT0%(0) / statT0%(2);
    If statT0%(1) = 0 Then Locate , 18: Print " ---.--";
    If statT0%(1) > 0 Then Locate , 18: Print Using "#####.##"; statT0%(0) / statT0%(1);
    If statT0%(4) = 0 Then Locate , 26: Print " ---.--";
    If statT0%(4) > 0 Then Locate , 26: Print Using "####.##"; statT0%(0) / statT0%(4);
    If statT0%(5) = 0 Then Locate , 33: Print " ---.--";
    If statT0%(5) > 0 Then Locate , 33: Print Using "####.##"; statT0%(0) / statT0%(5);
    If statT0%(6) = 0 Then Locate , 40: Print " ---.--";
    If statT0%(6) > 0 Then Locate , 40: Print Using "####.##"; statT0%(0) / statT0%(6);
    If statT0%(3) = 0 Then Locate , 47: Print " ---.--";
    If statT0%(3) > 0 Then Locate , 47: Print Using "####.##"; statT0%(0) / statT0%(3);
    If statT0%(7) = 0 Then Locate , 54: Print " ---.--";
    If statT0%(7) > 0 Then Locate , 54: Print Using "####.##"; statT0%(0) / statT0%(7);
    If statT0%(8) = 0 Then Locate , 61: Print " ---.--";
    If statT0%(8) > 0 Then Locate , 61: Print Using "####.##"; statT0%(0) / statT0%(8);
    If statT0%(9) = 0 Then Locate , 68: Print " ---.--";
    If statT0%(9) > 0 Then Locate , 68: Print Using "####.##"; statT0%(9) / (statT0%(9) + statT0%(10));
    Print " ";

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
    Locate 15, 14: Print Using "####"; TP&(3);
    Locate , 18: Print Using "#####"; TP&(6);
    Locate , 23: Print Using "#####"; TP&(7);
    Locate , 28: Print Using "######"; TP&(8);
    Locate , 34: Print Using "#######"; TP&(9);
    Locate , 41: Print Using "######"; TP&(13);
    Locate , 47: Print Using "#####"; TP&(16);
    Locate , 52: Print Using "###.##"; (ERX! * 9) / TP&(6);

    Color 7, 0
    Print String$(2, 177)

    Color 15, 12
    Locate 20, 14: Print Using "###.##"; TP&(7) * 9 / TP&(6);
    Locate , 20: Print Using "#####.##"; TP&(9) * 9 / TP&(6);
    Locate , 28: Print Using "#####.##"; TP&(8) * 9 / TP&(6);

    Color 7, 0
    Print String$(2, 177)

    Color 14, 12
    Locate 16, 14: Print Using "####"; statT1%(12);
    Locate , 18: Print Using "#####"; statT1%(0) / 3;
    Locate , 23: Print Using "#####"; statT1%(1);
    Locate , 28: Print Using "######"; statT1%(4);
    Locate , 34: Print Using "#######"; statT1%(5);
    Locate , 41: Print Using "######"; statT1%(14);
    Locate , 47: Print Using "#####"; statT1%(10);
    Locate , 52: Print Using "###.##"; (statT1%(3) * 9) / (statT1%(0) / 3);

    Color 7, 0: Print String$(2, 177)

    Locate , 16

    Color 7, 0: Print String$(44, 177)

    Color 14, 12
    Locate 21, 14: Print Using "###.##"; statT1%(1) * 9 / (statT1%(0) / 3);
    Locate , 20: Print Using "#####.##"; statT1%(5) * 9 / (statT1%(0) / 3);
    Locate , 28: Print Using "#####.##"; statT1%(4) * 9 / (statT1%(0) / 3);

    Color 7, 0
    Print String$(2, 177)

    Locate , 16

    Color 7, 0
    Print String$(22, 177)

    Color 15, 0
    Locate 22, 48: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

    Erase TB%, TP&

End Sub
