Sub LOOKY ()

    _Title "Full Count Baseball - View Stats"

    DefInt A-Z

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        Color 15, 0

        Call TITLE

        menuChoices$(1) = "VIEW/PRINT COMPILED STATS FOR ONE TEAM "
        menuChoices$(2) = "PRINT REPORTS FOR ALL TEAMS IN A LEAGUE"
        menuChoices$(3) = "RETURN TO MAIN MENU                    "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 7
        Column% = 22

        Call box0(6, 21, 10, 61, 2, 15, 1)
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

    Shared ratings%()

    Cls

    Do
        Color 15, 0
        Cls

        JJ = 6
        SN = 1: Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        I$ = GetKeyPress$

        If I$ = Chr$(27) Then
            Z$ = "END"
        Else
            If I$ = Chr$(13) Then
                Call SelectTeam_LOOKY(numberTeams, YN$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" And YN$ <> "" Then
                    Call SelectTeam_LOOKY(numberTeams, YN$)
                Else
                    YN$ = NY$
                    Erase teamNames$

                    numberTeams = 0

                    If Not _FileExists(diskPaths$(0) + "FCTEAMS." + YN$) Then
                        BO% = 1
                        Locate 10, 8
                        Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                    Else
                        Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
                        Y = LOF(1)
                        Field #1, 15 As Q$(0)
                        For I1 = 1 To Y / 4342
                            Get #1, I1
                            For I2 = 1 To 15
                                If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                            Next I2
                            If nullTeam <> 1 Then
                                numberTeams = numberTeams + 1
                                teamNames$(numberTeams) = Q$(0)
                                teamIndex%(numberTeams) = I1
                            End If
                        Next I1

                        Close 1

                        Call SelectTeam_LOOKY(numberTeams, YN$)

                    End If

                End If

            End If

            If BO% = 1 Then
                BO% = 0
            Else
                'This could go in thte SelectTeam routine,
                'if we use A$ as a parameter (ie, teamName)
                A$ = RTrim$(Q$(0))
                statB$ = A$

                Locate 2, 33: Color 15, 0

                Color ratings%(11), ratings%(12): Print A$

                Color 15, 4

                Do
                    Locate 3, 33
                    Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

                If UCase$(I$) = "N" Then

                    For XX = 4 To 11:
                        If Mid$(statB$, XX, 1) = " " Or Mid$(statB$, XX, 1) = "." Then Mid$(statB$, XX, 1) = "_"
                    Next XX

                    For XX = 12 To 14:
                        If Mid$(statB$, XX, 2) = "ST" Then Mid$(statB$, 9, 1) = "_": Mid$(statB$, 10, 2) = "ST"
                    Next XX

                    QQ = 8

                    For XX = 11 To 4 Step -1
                        If Mid$(statB$, XX, 1) = "_" Then
                            QQ = XX - 4
                        Else
                            Exit For
                        End If
                    Next XX

                    Z$ = Mid$(statB$, 4, QQ) + "." + Left$(statB$, 2)

                End If 'Done checking if "N"

                If UCase$(Z$) <> "END" Then
                    Call ReadStats(Z$)
                End If

            End If 'Done checking BO%

        End If 'Done checking for ESC

    Loop Until UCase$(Z$) = "END" Or UCase$(I$) = "N"

End Sub

'----------------------------------------
'       ReadStats Subroutine
'----------------------------------------
'...explanation...
Sub ReadStats (statFile$)

    Shared EW, EL, HW, HL
    Shared LTL, LTW, ORW, ORL
    Shared RW, RL, Wins, L

    Shared statB$(), statP$()
    Shared statT!(), statT1!()
    Shared statB%(), statB0%()
    Shared statP%(), statP0%()
    Shared SS%()

    Shared statH$(), L$(), PL$(), W$()
    Shared H%(), I%(), V%()

    Shared D0%, D1%, L2%, W2%

    If Not _FileExists(diskPaths$(1) + statFile$) Then
        Locate 10, 8
        Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else

        Wins = 0: L = 0
        RW = 0: RL = 0
        ORW = 0: ORL = 0
        HW = 0: HL = 0
        EW = 0: EL = 0
        LTL = 0: LTW = 0

        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, teamName$
        Input #1, W2%
        Input #1, L2%
        For I = 0 To 22: Input #1, statB$(I): Input #1, statB%(I): Next I
        For I = 0 To 21: Input #1, statP$(I): Input #1, statP%(I): Next I
        For I = 0 To 22: For I1 = 0 To 21: Input #1, statB0%(I, I1): Next I1: Next I
        For I = 0 To 21: For I1 = 0 To 41: Input #1, statP0%(I, I1): Next I1: Next I
        Input #1, D0%
        Input #1, D1%
        For I = 0 To 21: Input #1, statT!(I): Next I
        For I = 0 To 22: Input #1, statT1!(I): Next I
        Close #1

        If _FileExists(diskPaths$(1) + statFile$ + "R") Then
            SS% = 1
            Open diskPaths$(1) + statFile$ + "R" For Input As #1
            For I = 0 To 22
                For I1 = 0 To 6
                    Input #1, SS%(I, I1)
                Next I1
            Next I
            Close #1
        Else
            SS% = 0
        End If

        test$ = diskPaths$(1) + statFile$ + "S"
        Open diskPaths$(1) + statFile$ + "S" For Input As #1
        Input #1, totalGames%
        For I = 1 To NUM_STATRECORDS: Input #1, H%(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, statH$(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, V%(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, PL$(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, W$(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, L$(I): Next I
        For I = 1 To NUM_STATRECORDS: Input #1, I%(I): Next I
        Close #1

        For I = 1 To totalGames%
            If Abs(H%(I) - V%(I)) = 1 And H%(I) > V%(I) Then ORW = ORW + 1
            If Abs(H%(I) - V%(I)) = 1 And H%(I) < V%(I) Then ORL = ORL + 1
            If H%(I) > V%(I) Then Wins = Wins + 1
            If H%(I) > V%(I) And I%(I) > 9 Then EW = EW + 1
            If H%(I) > V%(I) And PL$(I) = "A" Then RW = RW + 1
            If H%(I) > V%(I) And PL$(I) = "H" Then HW = HW + 1
            If V%(I) > H%(I) Then L = L + 1
            If V%(I) > H%(I) And I%(I) > 9 Then EL = EL + 1
            If V%(I) > H%(I) And PL$(I) = "A" Then RL = RL + 1
            If V%(I) > H%(I) And PL$(I) = "H" Then HL = HL + 1
        Next I

        If totalGames% <= 10 Then
            LTL = L
            LTW = Wins
        Else
            M = totalGames% - 9
            For J = M To totalGames%
                If H%(J) > V%(J) Then LTW = LTW + 1
                If V%(J) > H%(J) Then LTL = LTL + 1
            Next J

        End If

        Call ViewAndPrintOptions(statFile$, SS%, teamName$, totalGames%)

    End If 'Done checking file count

End Sub

'----------------------------------------
'     ViewAndPrintOptions Subroutine
'----------------------------------------
'...explanation...
Sub ViewAndPrintOptions (statFile$, splitScreenChoice%, teamName$, totalGames%)

    Shared EW, EL, HW, HL
    Shared LTL, LTW, ORW, ORL
    Shared RW, RL, Wins, L

    Shared L2%, W2%

    Shared statB$(), statP$(), SS%()
    Shared statT!(), statT1!()

    Shared statB%(), statB0%()
    Shared H%(), I%(), ratings%(), V%()
    Shared statP%(), statP0%()

    Shared statH$(), L$(), PL$(), W$()

    Do
        Cls
        Print
        Print
        '-- Setup a box menu here; centered (similar to "STATS TO SCREEN / STATS TO PRINTER"
        Color 14: Print "(0) ";: Color 15: Print "VIEW SCHEDULE AND STATS"
        Print
        Color 14: Print "(1) ";: Color 15: Print "VIEW STATS ONLY"
        Print
        Color 14: Print "(2) ";: Color 15: Print "PRINT SCHEDULE AND STATS"
        Print
        Color 14: Print "(3) ";: Color 15: Print "PRINT STATS ONLY"

        Do
            prompt$ = GetKeyPress$
            X = Val(prompt$) + 1
        Loop Until X >= 1 And X <= 4 Or prompt$ = Chr$(27)

        If prompt$ <> Chr$(27) Then

            If X = 3 Or X = 4 Then
                Call NewPrintStats(statFile$, X, splitScreenChoice%, teamName$, totalGames%)
            End If

            '9810
            If X = 2 Then
                Cls
            Else
                'X = 1
                Cls
                Color 12: Print "        SCHEDULE RESULTS FOR ";
                Color ratings%(11), ratings%(12): Print teamName$

                Color 15, 0
                Color 14: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
                Color 15

                For I = 1 To totalGames%

					If (I Mod 21 = 1) Then

                        If X = 0 Or X = 1 Then
                            I$ = GetKeyPress$
                            Cls
                        End If

                        Cls
                        Color 14: Print Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"
                        Color 15

                    End If

                    'W$() = Winning Pitcher
                    'L$() = Losing Pitcher
                    'I%() = Innings
                    Print Str$(H%(I)); Tab(6); Left$(statH$(I), 15); Tab(22); Str$(V%(I)); Tab(27); "("; PL$(I); ")"; Tab(33); W$(I); Tab(50); L$(I); Tab(66); I%(I)

                Next

                If X = 1 Or X = 2 Then
                    I$ = GetKeyPress$
                    Cls
                End If

            End If

            Print Tab(20); "RECORD         ("; Str$(Wins); " -"; Str$(L); " )"
            Print Tab(20); "HOME RECORD    ("; Str$(HW); " -"; Str$(HL); " )"
            Print Tab(20); "ROAD RECORD    ("; Str$(RW); " -"; Str$(RL); " )"
            Print Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
            Print Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
            Print Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"

            If X = 1 Or X = 2 Then
                I$ = GetKeyPress$
                Cls
            End If

            Cls
            Color ratings%(11), ratings%(12): Print teamName$;

            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
            Color 15, 1
            Print "BATTERS    AB   R   H RBI  2B 3B  HR    BA   SLG   OBP  SO  BB  SB  CS %RAB"
            Color 15, 0

            For I = 0 To 22
                If statB$(I) <> "XXX" And statB$(I) <> "" Then
                    Locate I + 3, 1
                    Color 7
                    If I / 2 = Int(I / 2) Then Color 15

                    Print Left$(statB$(I), 10);

                    Locate , 11: Print Using "###"; statB0%(I, 0);
                    Locate , 15: Print Using "###"; statB0%(I, 2);
                    Locate , 19: Print Using "###"; statB0%(I, 1);
                    Locate , 23: Print Using "###"; statB0%(I, 3);
                    Locate , 28: Print Using "##"; statB0%(I, 4);
                    Locate , 31: Print Using "##"; statB0%(I, 5);
                    Locate , 35: Print Using "##"; statB0%(I, 6);

                    I2 = 0: I3 = 0: I4 = 0
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

                    I4 = statB0%(I, 1) + statB0%(I, 7): I3 = statB0%(I, 7) + statB0%(I, 0)
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

                    If statT!(0) = 0 Then statT!(0) = 1

                End If 'Done checking for XXX / terminal chars

            Next I

            Locate 1, 1
            If X = 1 Or X = 2 Then I$ = GetKeyPress$

            '200
            Cls
            Color 12, 0: Color ratings%(11), ratings%(12): Print teamName$;

            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
            Color 15, 1
            Print "BATTERS      G  CHS LHS GIDP NEEDS PROJ AB  PO   A  E  FAVG  SBA CS %CS SH SF": Color 15

            For I = 0 To 22
                If statB$(I) <> "XXX" And statB$(I) <> "" Then

                    Locate I + 3, 1

                    Color 7, 0

                    If I / 2 = Int(I / 2) Then Color 15

                    Print Left$(statB$(I), 10);
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

                    If statB%(I) > 0 Then Locate , 37: H5% = Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5): Print Using "####"; H5%;: statT!(13) = statT!(13) + H5%
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

            If X = 1 Or X = 2 Then I$ = GetKeyPress$

            Cls
            Color ratings%(11), ratings%(12): Print teamName$;

            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
            Color 15, 1
            Print "PITCHERS   AB   R   H RBI  2B 3B  HR    BA   SLG   OBP  SO  BB  SB  CS"

            For I = 0 To 21

                If statP$(I) <> "XXX" And statP$(I) <> "" Then

                    Color 7, 0

                    If I / 2 = Int(I / 2) Then Color 15

                    Locate I + 3, 1: Print Left$(statP$(I), 10);
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
                    If statT!(0) = 0 Then statT!(0) = 1

                End If
            Next I

            Locate 1, 1
            If X = 1 Or X = 2 Then I$ = GetKeyPress$

            Cls
            Color ratings%(11), ratings%(12): Print teamName$;
            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
            Color 15, 1: Print "PITCHERS     G  CHS LHS GIDP   PO   A  E  FAVG SH SF": Color 15

            For I = 0 To 21

                If statP$(I) <> "XXX" And statP$(I) <> "" Then

                    Locate I + 3, 1

                    Color 7, 0

                    If I / 2 = Int(I / 2) Then Color 15

                    Print Left$(statP$(I), 10);
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
            If X = 1 Or X = 2 Then I$ = GetKeyPress$

            Cls
            Color ratings%(11), ratings%(12): Print teamName$;

            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%;
            Color 14, 0: Print "  TEAM BATTING TOTALS"
            Color 15, 1: Print "  AB    R    H  RBI  2B  3B  HR    BA   SLG   OBP   SO   BB  SB  CS"
            Color 14, 0

            Print Using "#### #### #### #### ### ### ###"; statT!(0); statT!(2); statT!(1); statT!(3); statT!(4); statT!(5); statT!(6);
            Locate , 34: Print ".";: Print Using "###"; Int((statT!(1) / statT!(0) + .0005) * 1000);
            Locate , 40: Print ".";
            Print Using "###"; Int((((statT!(1) + statT!(4) + 2 * statT!(5) + 3 * statT!(6)) / statT!(0)) + .0005) * 1000);
            Locate , 46: Print ".";
            Print Using "###"; Int(((statT!(1) + statT!(7)) / (statT!(7) + statT!(0)) + .0005) * 1000);
            Locate , 51: Print Using "#### #### ### ###"; statT!(8); statT!(7); statT!(9); statT!(10)
            Locate 8, 1: Color 15, 1: Print "GIDP   PO    A   E  FAVG  SH  SF  LOB": Color 15
            Color 14, 0
            Print Using "#### #### #### ###"; statT!(15); statT!(16); statT!(17); statT!(11);
            Locate , 20: If statT!(11) = 0 Then Print "1.000";
            If statT!(11) <> 0 Then Print Using " .###"; (statT!(16) + statT!(17)) / (statT!(16) + statT!(17) + statT!(11));
            Print Using " ### ### ####"; statT!(18); statT!(19); statT!(12)
            If X = 0 Or X = 1 Then I$ = GetKeyPress$

            If splitScreenChoice% <> 0 Then
                ' DISPLAY SPLIT STATS
                Cls
                Color ratings%(11), ratings%(12): Print teamName$;
                Locate 1, 15
                Color 14, 0
                Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
                Color 15, 0
                Locate 2

                For I = 0 To 22

                    If statB$(I) <> "XXX" And Left$(statB$(I), 1) <> " " And statB$(I) <> "" Then

                        Print statB$(I);
                        '                        AB   2B 3B HR BB  K
                        Locate , 20: Print Using "### ## ## ## ### ### "; SS%(I, 0); SS%(I, 2); SS%(I, 3); SS%(I, 4); SS%(I, 5); SS%(I, 6);

                        If SS%(I, 0) = 0 And SS%(I, 1) = 0 Then
                            Print " .000";
                        Else
                            If SS%(I, 0) = SS%(I, 1) Then
                                Print "1.000";
                            Else
                                Print Using "#.###"; SS%(I, 1) / SS%(I, 0);
                            End If
                        End If

                        '                        AB   2B 3B HR BB  K
                        Locate , 53: Print Using "### ## ## ## ### ### "; statB0%(I, 0) - SS%(I, 0); statB0%(I, 4) - SS%(I, 2); statB0%(I, 5) - SS%(I, 3); statB0%(I, 6) - SS%(I, 4); statB0%(I, 7) - SS%(I, 5); statB0%(I, 8) - SS%(I, 6);
                        If statB0%(I, 0) - SS%(I, 0) = 0 Or statB0%(I, 1) - SS%(I, 1) = 0 Then
                            Print " .000";
                        Else
                            If statB0%(I, 0) - SS%(I, 0) = statB0%(I, 1) - SS%(I, 1) Then
                                Print "1.000";
                            Else
                                Print Using "#.###"; (statB0%(I, 1) - SS%(I, 1)) / (statB0%(I, 0) - SS%(I, 0));
                            End If
                        End If
                        If I <> 22 Then Print

                    End If
                Next I

                If X = 1 Or X = 2 Then I$ = GetKeyPress$

            End If

            Cls
            Color ratings%(11), ratings%(12): Print teamName$;
            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
            Color 15, 4: Print "PITCHERS    W  L SV    IP    H  BB   SO   ERA  G GS CG RUN  ER ShO HRA  DP %RIP"

            Color 15

            For I = 0 To 21
                If statP$(I) <> "XXX" And statP$(I) <> "" Then
                    Locate I + 3, 1
                    Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
                    Print Left$(statP$(I), 10);
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

            If X = 0 Or X = 1 Then I$ = GetKeyPress$

            Cls
            Color ratings%(11), ratings%(12): Print teamName$;
            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%
            Color 15, 4: Print "PITCHERS   BS HL  RI  RS   SUP GS QS  SH SF"

            Color 15

            For I = 0 To 21
                If statP$(I) <> "XXX" And statP$(I) <> "" Then
                    Locate I + 3, 1
                    Color 7, 0: If I / 2 = Int(I / 2) Then Color 15
                    Print Left$(statP$(I), 10);: Locate , 12
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

            If X = 1 Or X = 2 Then I$ = GetKeyPress$

            Cls

            Color ratings%(11), ratings%(12): Print teamName$;

            Color 12, 0: Print "    WON: "; W2%; "  LOST: "; L2%;
            Color 14, 0: Print "  TEAM PITCHING TOTALS"
            Color 15, 4: Print "  W   L  SV     IP    H   BB   SO   ERA   G  GS  CG  RUN   ER ShO HRA  DP": Color 14, 0
            Print Using "### ### ### "; W2%; L2%; statT1!(12);
            Print Using "####"; Int(statT1!(0) / 3);
            Print ".";: Print Using "#"; statT1!(0) - Int(statT1!(0) / 3) * 3;
            Print Using " #### #### #### ##.## ### ### ### "; statT1!(1); statT1!(4); statT1!(5); statT1!(3) * 9 / (statT1!(0) / 3); statT1!(9); statT1!(9); statT1!(10);
            Print Using "#### #### ### ### ###"; statT1!(2); statT1!(3); statT1!(11); statT1!(14); statT1!(13)

            Locate 8, 1
            Color 15, 4: Print " BS   RI   RS   SUP  QS  SH  SF": Color 14, 0
            Print Using "### #### #### ##.## ### ### ###"; statT1!(19); statT1!(17); statT1!(18); statT!(2) / statT1!(9); statT1!(21); statT1!(15); statT1!(16)

            If X = 1 Or X = 2 Then I$ = GetKeyPress$

            Print

            If X = 2 Then X = 0

            Color 15, 0

            Cls
            JJ = 1
            Locate 6, 8
            Color ratings%(11), ratings%(12)

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

            Locate 7
            U$ = "DISPLAY STATS AGAIN (YN)"
            Do
                Locate , (80 - Len(U$)) / 2
                Print U$
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

        End If

    Loop Until UCase$(I$) = "N" Or prompt$ = Chr$(27)

    'If prompt$ <> Chr$(27) Then

	'Call ResetStatPrompt (statFile$, ratings%())

    'End If 'Done checking for ESC

    Cls

End Sub

'----------------------------------------
'       ReadStats Subroutine
'----------------------------------------
'...explanation...
Sub ReportsAllTeams ()

    Dim H1$(40)
    Shared D0%, D1%, L2%, W2%

    Shared statB$(), statP$()
    Shared statT!(), statT1!()
    Shared statB%(), statB0%()
    Shared statP%(), statP0%()

    Do
        'Display list of files...

        Cls

        Spec$ = diskPaths$(2) + "*.TMS"
        Count = FCount%(Spec$)

        If Count = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND ...  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim leagueFiles$(0 To Count)
            For X = 1 To Count
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
                Print "SELECT LEAGUE TO PRINT": Color 15
                Selection% = 1
                Start% = 1
                FileFGColor% = 15: FileBGColor% = 1
                HiLiteFGColor% = 0: HiLiteBGColor% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS LEAGUE";

                _MouseShow "Default"
                MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = leagueFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                LH$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(2) + LH$ + ".TMS" For Input As #2
                Input #2, LG$, D1$, D2$, D3$, D4$
                For XW = 1 To 40
                    Input #2, H1$(XW)
                Next
                Close 2

                For XW = 1 To 40

                    If _FileExists(diskPaths$(1) + H1$(XW)) Then
                        Open diskPaths$(1) + H1$(XW) For Input As #1
                        Input #1, teamName$
                        Input #1, W2%
                        Input #1, L2%
                        For I = 0 To 22: Input #1, statB$(I): Input #1, statB%(I): Next I
                        For I = 0 To 21: Input #1, statP$(I): Input #1, statP%(I): Next I
                        For I = 0 To 22: For I1 = 0 To 21: Input #1, statB0%(I, I1): Next I1: Next I
                        For I = 0 To 21: For I1 = 0 To 41: Input #1, statP0%(I, I1): Next I1: Next I
                        Input #1, D0%: Input #1, D1%
                        For I = 0 To 21: Input #1, statT!(I): Next I
                        For I = 0 To 22: Input #1, statT1!(I): Next I
                        Close #1

                        BP = 1 'this doesn't appear to be used
                        X = 3
                        Call NewPrintStats(statFile$, X, splitScreenChoice%, teamName$, totalGames%)
                    End If

                Next

            End If 'Done checking for ESC

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or I$ = "N"

End Sub

'----------------------------------------
'       SelectTeam_LOOKY Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_LOOKY (numberTeams, yearNumber$)

    Shared ratings%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColor% = 15: FileBGColor% = 4
    HiLiteFGColor% = 0: HiLiteBGColor% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4
    Cls
    Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "Default"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        T1 = teamIndex%(Selection%)
        Locate 2, 33: Color 31: Print "LOADING..."

        If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
            BO% = 1
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

            Get #1, T1

            For I = 1 To 13
                ratings%(I) = CVI(Q$(I))
            Next
			
            Close 1

        End If

    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'       NewPrintStats Subroutine
'----------------------------------------
'...explanation...
Sub NewPrintStats (statFile$, printChoice, splitScreenChoice%, teamName$, totalGames%)

    Shared L2%, W2%

    Shared EW, EL, HW, HL
    Shared LTL, LTW, ORW, ORL
    Shared RW, RL, Wins, L

    Shared statB$(), statP$()
    Shared statT!(), statT1!()
    Shared statB%(), statB0%()
    Shared statP%(), statP0%()

    Shared statH$(), L$()
    Shared PL$(), W$()
    Shared H%(), I%()
    Shared SS%(), V%()

    ' *** NEW PRINT ROUTINE 6-24-90 ***
    If printChoice <> 3 Then
        Wins = 0: L = 0
        ORW = 0: ORL = 0
        RW = 0: RL = 0
        HW = 0: HL = 0
        EW = 0: EL = 0
        LTL = 0: LTW = 0
        Cls
        LPrint "        SCHEDULE RESULTS FOR "; teamName$
        LPrint
    Else
        LPrint Tab(6); "OPPONENT"; Tab(33); "WINNING PITCHER"; Tab(50); "LOSING PITCHER"; Tab(66); "INNINGS"

        For I = 1 To totalGames%
            LPrint Str$(H%(I)); Tab(6); Left$(statH$(I), 15); Tab(22); Str$(V%(I)); Tab(27); "("; PL$(I); ")"; Tab(33); W$(I); Tab(50); L$(I); Tab(66); I%(I)
        Next

        LPrint
        LPrint
        LPrint Tab(20); "RECORD         ("; Str$(Wins); " -"; Str$(L); " )"
        LPrint Tab(20); "HOME RECORD    ("; Str$(HW); " -"; Str$(HL); " )"
        LPrint Tab(20); "ROAD RECORD    ("; Str$(RW); " -"; Str$(RL); " )"
        LPrint Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
        LPrint Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
        LPrint Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
        LPrint Chr$(12)

    End If

    LPrint teamName$; "    WON: "; W2%; " LOST: "; L2%
    LPrint "BATTERS      G  AB   R   H 2B 3B HR RBI  BB   K  SB CS CHS LHS GIDP  BAVG   SLG   OBP NEED PROJ.AB %RAB   PO   A  E  FAVG SH SF"

    For I = 0 To 22

        If statB$(I) <> "XXX" And statB$(I) <> "" Then
            LPrint Left$(statB$(I), 10); Tab(12);: LPrint Using "###"; statB0%(I, 14);
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
            If statB%(I) > 0 Then H5% = Int((((statB%(I) + 0) / QT) * (statT1!(9) + 1)) + .5): LPrint Using "###"; H5%;: statT!(13) = statT!(13) + H5%

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

        If statP$(I) <> "XXX" And statP$(I) <> "" Then
            LPrint Left$(statP$(I), 10); Tab(12);: LPrint Using "###"; statP0%(I, 8);
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

    LPrint " ": LPrint "TOTALS";: LPrint Tab(12);: LPrint Using "###"; totalGames%;
    LPrint Tab(19);: LPrint Using "####"; statT!(2);
    LPrint Tab(27);: LPrint Using "###"; statT!(4);
    LPrint Tab(33);: LPrint Using "###"; statT!(6);
    LPrint Tab(40);: LPrint Using "####"; statT!(7);
    LPrint Tab(49);: LPrint Using "###"; statT!(9);
    LPrint Tab(69);: LPrint Using " .###"; (statT!(1) / statT!(0));
    LPrint Tab(75);: LPrint Using " .###"; ((statT!(1) + statT!(4) + statT!(5) * 2 + statT!(6) * 3) / statT!(0));
    LPrint Tab(81);: LPrint Using " .###"; ((statT!(1) + statT!(7)) / (statT!(7) + statT!(0)));
    LPrint Tab(105);: LPrint Using "####"; statT!(16);
    LPrint Tab(113);: LPrint Using "###"; statT!(11);
    LPrint Tab(122); Using "###"; statT!(18)
    LPrint Tab(15);: LPrint Using "####"; statT!(0);
    LPrint Tab(23);: LPrint Using "####"; statT!(1);
    LPrint Tab(30);: LPrint Using "###"; statT!(5);
    LPrint Tab(36);: LPrint Using "####"; statT!(3);
    LPrint Tab(44);: LPrint Using "####"; statT!(8);
    LPrint Tab(52);: LPrint Using "####"; statT!(10);
    LPrint Tab(63); Using "###"; statT!(15);
    LPrint Tab(109);: LPrint Using "####"; statT!(17);
    LPrint Tab(117);

    If statT!(16) + statT!(11) + statT!(17) = 0 Then
        LPrint " .---";
    Else
        If statT!(11) = 0 Then
            LPrint "1.000";
        Else
            LPrint Using " .###"; (statT!(16) + statT!(17)) / (statT!(16) + statT!(11) + statT!(17));
        End If
    End If

    LPrint Tab(125); Using "###"; statT!(19)
    LPrint
    LPrint
    TT$ = Str$(statT!(2) / (W2% + L2%) + .05)
    UU$ = Str$(statT1!(2) / (W2% + L2%) + .05)
    LPrint "RUNS PER GAME ";: LPrint Left$(TT$, 4);
    LPrint " OPPONENTS";: LPrint Left$(UU$, 4)
    LPrint
    LPrint
    LPrint Tab(20); "RECORD         ("; Str$(Wins); " -"; Str$(L); " )"
    LPrint Tab(20); "HOME RECORD    ("; Str$(HW); " -"; Str$(HL); " )"
    LPrint Tab(20); "ROAD RECORD    ("; Str$(RW); " -"; Str$(RL); " )"
    LPrint Tab(20); "LAST 10 GAMES  ("; Str$(LTW); " -"; Str$(LTL); " )"
    LPrint Tab(20); "EXTRA INNINGS  ("; Str$(EW); " -"; Str$(EL); " )"
    LPrint Tab(20); "ONE RUN GAMES  ("; Str$(ORW); " -"; Str$(ORL); " )"
    LPrint Chr$(12)
    LPrint teamName$; "    WON: "; W2%; " LOST: "; L2%
    LPrint "PITCHERS    W  L SV  IP     H  BB  SO  ERA    G GS CG RUN  ER ShO HR DP %RIP SH SF  RI RIS BS   SUP QS"

    For I = 0 To 21

        If statP$(I) <> "XXX" And statP$(I) <> "" Then
            LPrint Left$(statP$(I), 10);
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
        If statB$(I) <> "XXX" And statB$(I) <> "" And statB0%(I, 21) + statB0%(I, 20) <> 0 Then
            LPrint Left$(statB$(I), 10); Tab(12); Using "###   ##   ###"; statB0%(I, 20) + statB0%(I, 21); statB0%(I, 21); statB0%(I, 21) / (statB0%(I, 20) + statB0%(I, 21)) * 100
        End If
    Next

    If splitScreenChoice% <> 0 Then
        'DISPLAY SPLIT STATS
        LPrint teamName$;
        LPrint Tab(15); "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"

        For I = 0 To 22

            If statB$(I) <> "XXX" And Left$(statB$(I), 1) <> " " And statB$(I) <> "" Then
                LPrint statB$(I);
                LPrint Tab(20); Using "### ## ## ## ### ### "; SS%(I, 0); SS%(I, 2); SS%(I, 3); SS%(I, 4); SS%(I, 5); SS%(I, 6);

                If SS%(I, 0) = 0 Or SS%(I, 1) = 0 Then
                    LPrint " .000";
                Else
                    If SS%(I, 0) = SS%(I, 1) Then
                        LPrint "1.000";
                    Else
                        LPrint Using " .###"; SS%(I, 1) / SS%(I, 0);
                    End If
                End If

                LPrint Tab(53); Using "### ## ## ## ### ### "; statB0%(I, 0) - SS%(I, 0); statB0%(I, 4) - SS%(I, 2); statB0%(I, 5) - SS%(I, 3); statB0%(I, 6) - SS%(I, 4); statB0%(I, 7) - SS%(I, 5); statB0%(I, 8) - SS%(I, 6);

                If statB0%(I, 0) - SS%(I, 0) = 0 Or statB0%(I, 1) - SS%(I, 1) = 0 Then
                    LPrint " .000"
                Else
                    If statB0%(I, 0) - SS%(I, 0) = statB0%(I, 1) - SS%(I, 1) Then
                        LPrint "1.000"
                    Else
                        LPrint Using " .###"; (statB0%(I, 1) - SS%(I, 1)) / (statB0%(I, 0) - SS%(I, 0))
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
' Prompts user with choice to reset stats
' If they chose so, and confirm, then
' it will proceed to delete the files.
Sub ResetStatPrompt (statFile$, ratings%())
	
	Cls

	JJ = 1
	Locate 6, 8
	'Color ratings%(11), ratings%(12)

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

	Locate 7

	Do
		U$ = "RESET COMPILED STATS TO 0 (YN)"
		Locate , (80 - Len(U$)) / 2
		Print U$
		I$ = GetKeyPress$
	Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

	Color 15, 0

	If UCase$(I$) = "Y" Then

		'Do
		JJ = 1
		Locate 6, 8
		'Color ratings%(11), ratings%(12)

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

		Locate 7

		Do
			U$ = "ARE YOU SURE (YN)"
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