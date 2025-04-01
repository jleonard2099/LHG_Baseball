'----------------------------------------
'            CARLDR Subroutine
'----------------------------------------
'This routine provides a menu for the user
'that allows them to Update and View the
'Career leaders for a specific team
Sub CARLDR ()

    _Title "Full Count Baseball - Career Leaders"

    Do

        Color 15, 0
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "UPDATE CAREER LEADERS      "
        menuChoices$(2) = "VIEW / PRINT CAREER LEADERS"
        menuChoices$(3) = "RETURN TO UTILITIES MENU   "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call UpdateCareerLeaders

                Case 2:
                    Call CareerLeaderOptions

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'     UpdateCareerLeaders Subroutine
'----------------------------------------
'This routine reads in the current stats,
'calculates the latest values (based on
'the criteria given) and then creates /
'overwrites the TCL file with the newest
'calculated values.
Sub UpdateCareerLeaders ()

    Dim batCarVals(100, 18), pitCarVals(80, 17)

    Shared carLdrBatName$(), carLdrPitName$()
    Shared carLdrBatVal!(), carLdrPitVal!()

    Shared careerBatters$(), careerPitchers$()
    Shared carRecBatStats(), carRecPitStats()

    TMR1 = (100 * 14) + (80 * 17)

    Erase carLdrBatVal!, carLdrPitVal!

    '-- Initialize to 1000 because later we assign
    '-- values based on comparison. (ie, looking
    '-- for lowest values. Nothing will ever be
    '-- lower than 0 if we don't have something here.
    For I1 = 0 To 20
        carLdrPitVal!(15, I1) = 1000
    Next

    careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Cls

        Do

            Locate 3
            Input "ENTER NUMBER OF AT BATS TO QUALIFY"; N1
            Locate 4
            Input "ENTER NUMBER OF INNINGS PITCHED TO QUALIFY"; N2

            Do
                Locate 5: Print "ANY CHANGE? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Loop Until UCase$(I$) = "N"

        Call LoadCareerFile(careerFile$)

        Color 14

        Locate 10: Print "COMPUTING LEADER FILE . . . . . . PLEASE WAIT"

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS

            '# Player
            For I = 0 To 99

                If careerBatters$(I) <> "" And careerBatters$(I) <> "XXX" And careerBatters$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 14

                        batCarVals(I, J) = batCarVals(I, J) + carRecBatStats(N, I, J)

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS

            '# Player
            For I = 0 To 79

                If careerPitchers$(I) <> "" And careerPitchers$(I) <> "XXX" And careerPitchers$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 16

                        pitCarVals(I, J) = pitCarVals(I, J) + carRecPitStats(N, I, J)

                    Next J

                End If

            Next I

        Next N

        ' ##############################
        ' #   Update and sort leaders
        ' ##############################
        'Batters

        'Categories # 0 to 14
        For Z = 0 To 14

            For K = 0 To 99

                If batCarVals(K, Z) > carLdrBatVal!(Z, 19) Then

                    carLdrBatVal!(Z, 19) = batCarVals(K, Z)
                    carLdrBatName$(Z, 19) = careerBatters$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrBatVal!(Z, J - 1) < carLdrBatVal!(Z, J) Then
                                Swap carLdrBatVal!(Z, J - 1), carLdrBatVal!(Z, J)
                                Swap carLdrBatName$(Z, J - 1), carLdrBatName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        'Category # 15 - Batting Average
        For K = 0 To 99

            If batCarVals(K, 0) < N1 Or batCarVals(K, 0) = 0 Then

                If batCarVals(K, 1) / batCarVals(K, 0) > carLdrBatVal!(15, 19) Then

                    carLdrBatVal!(15, 19) = batCarVals(K, 1) / batCarVals(K, 0)
                    carLdrBatName$(15, 19) = careerBatters$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrBatVal!(15, J - 1) < carLdrBatVal!(15, J) Then
                                Swap carLdrBatVal!(15, J - 1), carLdrBatVal!(15, J)
                                Swap carLdrBatName$(15, J - 1), carLdrBatName$(15, J)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        'Category # 16 - Slugging %
        For K = 0 To 99

            If batCarVals(K, 0) < N1 Or batCarVals(K, 0) = 0 Then

                If (batCarVals(K, 1) + batCarVals(K, 4) + batCarVals(K, 5) * 2 + batCarVals(K, 6) * 3) / batCarVals(K, 0) > carLdrBatVal!(16, 19) Then

                    carLdrBatVal!(16, 19) = (batCarVals(K, 1) + batCarVals(K, 4) + batCarVals(K, 5) * 2 + batCarVals(K, 6) * 3) / batCarVals(K, 0)
                    carLdrBatName$(16, 19) = careerBatters$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrBatVal!(16, J - 1) < carLdrBatVal!(16, J) Then
                                Swap carLdrBatVal!(16, J - 1), carLdrBatVal!(16, J)
                                Swap carLdrBatName$(16, J - 1), carLdrBatName$(16, J)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        ' Category # 17 - On Base %
        For K = 0 To 99

            If Not (batCarVals(K, 0) < N1 Or batCarVals(K, 0) + batCarVals(K, 7) = 0) Then

                If (batCarVals(K, 1) + batCarVals(K, 7)) / (batCarVals(K, 0) + batCarVals(K, 7)) > carLdrBatVal!(17, 19) Then

                    carLdrBatVal!(17, 19) = (batCarVals(K, 1) + batCarVals(K, 7)) / (batCarVals(K, 0) + batCarVals(K, 7))
                    carLdrBatName$(17, 19) = careerBatters$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrBatVal!(17, J - 1) < carLdrBatVal!(17, J) Then
                                Swap carLdrBatVal!(17, J - 1), carLdrBatVal!(17, J)
                                Swap carLdrBatName$(17, J - 1), carLdrBatName$(17, J)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        ' ##############################
        ' #   Update and sort leaders
        ' ##############################
        'Pitchers

        'Categories # 0 to 14
        For Z = 0 To 14

            For K = 0 To 79

                If pitCarVals(K, Z) > carLdrPitVal!(Z, 19) Then

                    carLdrPitVal!(Z, 19) = pitCarVals(K, Z)
                    carLdrPitName$(Z, 19) = careerPitchers$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrPitVal!(Z, J - 1) < carLdrPitVal!(Z, J) Then
                                Swap carLdrPitVal!(Z, J - 1), carLdrPitVal!(Z, J)
                                Swap carLdrPitName$(Z, J - 1), carLdrPitName$(Z, J)
                            End If
                        Next J
                    Next I
                End If

            Next K

        Next Z

        'Category #15 - ERA
        For K = 0 To 79

            If Not (pitCarVals(K, 0) < N2 * 3 Or pitCarVals(K, 0) = 0) Then

                If ((pitCarVals(K, 3) * 9) / pitCarVals(K, 0)) * 3 < carLdrPitVal!(15, 19) Then

                    carLdrPitVal!(15, 19) = ((pitCarVals(K, 3) * 9) / pitCarVals(K, 0)) * 3
                    carLdrPitName$(15, 19) = careerPitchers$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrPitVal!(15, J) < carLdrPitVal!(15, J - 1) Then
                                Swap carLdrPitVal!(15, J), carLdrPitVal!(15, J - 1)
                                Swap carLdrPitName$(15, J), carLdrPitName$(15, J - 1)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        'Save leaders
        Open diskPaths$(2) + careerFile$ + ".TCL" For Output As #1

        For I = 0 To 17:
            For I1 = 0 To 19:
                'Batting leaders
                Write #1, carLdrBatVal!(I, I1), carLdrBatName$(I, I1)
            Next
        Next

        For I = 0 To 15:
            For I1 = 0 To 19:
                'Pitching leaders
                Write #1, carLdrPitVal!(I, I1), carLdrPitName$(I, I1)
            Next
        Next

        Close #1

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'     CareerLeaderOptions Subroutine
'----------------------------------------
'This routine reads in previously compiled
'career stats (.TCL file) and then provides
'the user options to print or view the stats.
Sub CareerLeaderOptions ()

    Shared carLdrBatVal!(), carLdrPitVal!()
    Shared carLdrBatName$(), carLdrPitName$()

    Cls

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.TCL", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + leaderFile$ + ".TCL" For Input As #1

        For I = 0 To 17:
            For I1 = 0 To 19:
                Input #1, carLdrBatVal!(I, I1), carLdrBatName$(I, I1)
            Next
        Next

        For I = 0 To 15:
            For I1 = 0 To 19:
                Input #1, carLdrPitVal!(I, I1), carLdrPitName$(I, I1)
            Next
        Next

        Close #1

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call PrintCareerLeaders(0, userFile$)
                    End If

                Case "P"
                    Call PrintCareerLeaders(1, "REPORT")

                Case "S"
                    Call ViewCareerLeaders

            End Select

        End If

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'       PrintCareerLeaders Subroutine
'----------------------------------------
'This routine sends formatted career stats
'(loaded from the .TCL file) to the user's
'default printer
Sub PrintCareerLeaders (printDest, destFile$)

    Shared carLdrBatName$(), carLdrPitName$()
    Shared carLdrBatVal!(), carLdrPitVal!()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    ' ### BATTERS ###

    Print #1, Tab(5); "AT BATS"; Tab(29); "HITS"; Tab(54); "RUNS SCORED"; Tab(79); "RBI'S"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(0, I), 13);: Print #1, Tab(18); Using "####"; carLdrBatVal!(0, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(1, I), 13);: Print #1, Tab(43); Using "####"; carLdrBatVal!(1, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(2, I), 13);: Print #1, Tab(68); Using "####"; carLdrBatVal!(2, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(3, I), 13);: Print #1, Tab(93); Using "####"; carLdrBatVal!(3, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "DOUBLES"; Tab(29); "TRIPLES"; Tab(54); "HOME RUNS"; Tab(79); "WALKS"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(4, I), 13);: Print #1, Tab(18); Using "####"; carLdrBatVal!(4, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(5, I), 13);: Print #1, Tab(43); Using "####"; carLdrBatVal!(5, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(6, I), 13);: Print #1, Tab(68); Using "####"; carLdrBatVal!(6, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(7, I), 13);: Print #1, Tab(93); Using "####"; carLdrBatVal!(7, I)
    Next

    Print #1, Chr$(12)

    Print #1,
    Print #1,

    Print #1, Tab(5); "STRIKE OUTS"; Tab(29); "STOLEN BASES"; Tab(54); "CAUGHT STEALING"; Tab(79); "ERRORS"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(10, I), 13);: Print #1, Tab(18); Using "####"; carLdrBatVal!(10, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(11, I), 13);: Print #1, Tab(43); Using "####"; carLdrBatVal!(11, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(12, I), 13);: Print #1, Tab(68); Using "####"; carLdrBatVal!(12, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(13, I), 13);: Print #1, Tab(93); Using "####"; carLdrBatVal!(13, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "ON-BASE PCT."; Tab(29); "GAMES PLAYED"; Tab(54); "BATTING AVERAGE"; Tab(79); "SLUGGING PCT."

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(17, I), 13);: Print #1, Tab(18); Using "#.###"; carLdrBatVal!(17, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(14, I), 13);: Print #1, Tab(43); Using "####"; carLdrBatVal!(14, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(15, I), 13);: Print #1, Tab(68); Using "#.###"; carLdrBatVal!(15, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrBatName$(16, I), 13);: Print #1, Tab(93); Using "#.###"; carLdrBatVal!(16, I)
    Next

    Print #1, Chr$(12)

    Print #1,
    Print #1,

    ' ### PITCHERS ###

    Print #1, Tab(5); "INNINGS PITCHED"; Tab(29); "HITS ALLOWED"; Tab(54); "RUNS ALLOWED"; Tab(79); "EARNED RUNS ALLOWED"

    For I = 0 To 19

        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(0, I), 13);: Print #1, Tab(18); Using "###"; Int(carLdrPitVal!(0, I) / 3);
        If carLdrPitVal!(0, I) - (Int(carLdrPitVal!(0, I) / 3) * 3) > 0 Then
            Print #1, ".";: Print #1, Using "#"; carLdrPitVal!(0, I) - (Int(carLdrPitVal!(0, I) / 3) * 3);
        End If

        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(1, I), 13);: Print #1, Tab(43); Using "####"; carLdrPitVal!(1, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(2, I), 13);: Print #1, Tab(68); Using "####"; carLdrPitVal!(2, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(3, I), 13);: Print #1, Tab(93); Using "####"; carLdrPitVal!(3, I)

    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "WALKS"; Tab(29); "STRIKEOUTS"; Tab(54); "WINS"; Tab(79); "LOSSES"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(4, I), 13);: Print #1, Tab(18); Using "####"; carLdrPitVal!(4, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(5, I), 13);: Print #1, Tab(43); Using "####"; carLdrPitVal!(5, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(6, I), 13);: Print #1, Tab(68); Using "####"; carLdrPitVal!(6, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(7, I), 13);: Print #1, Tab(93); Using "####"; carLdrPitVal!(7, I)
    Next

    Print #1, Chr$(12)

    Print #1,
    Print #1,

    Print #1, Tab(5); "GAMES"; Tab(29); "GAMES STARTED"; Tab(54); "COMPLETE GAMES"; Tab(79); "SHUTOUTS"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(8, I), 13);: Print #1, Tab(18); Using "####"; carLdrPitVal!(8, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(9, I), 13);: Print #1, Tab(43); Using "####"; carLdrPitVal!(9, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(10, I), 13);: Print #1, Tab(68); Using "####"; carLdrPitVal!(10, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(11, I), 13);: Print #1, Tab(93); Using "####"; carLdrPitVal!(11, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "SAVES"; Tab(29); "DOUBLE PLAYS THROWN"; Tab(54); "HOME RUNS ALLOWED"; Tab(79); "EARNED RUN AVERAGE"

    For I = 0 To 19

        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(12, I), 13);: Print #1, Tab(18); Using "####"; carLdrPitVal!(12, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(13, I), 13);: Print #1, Tab(43); Using "####"; carLdrPitVal!(13, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(14, I), 13);: Print #1, Tab(68); Using "####"; carLdrPitVal!(14, I);

        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, Left$(carLdrPitName$(15, I), 13);
        If carLdrPitVal!(15, I) = 1000 Then
            Print #1, Tab(93); Using "###.##"; 0.0
        Else
            Print #1, Tab(93); Using "###.##"; carLdrPitVal!(15, I)
        End If

    Next

    Print #1, Chr$(12)

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
'      ViewCareerLeaders Subroutine
'----------------------------------------
'This routine displays formatted career
'stats (loaded from the .TCL file) to the
'primary screen
Sub ViewCareerLeaders ()

    Shared carLdrBatName$(), carLdrPitName$()
    Shared carLdrBatVal!(), carLdrPitVal!()

    Cls
    Color 12

    ' ### BATTERS ###

    Locate 2, 1
    Print Tab(5); "AT BATS"; Tab(44); "HITS"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(0, I);
        Print Tab(20); Using "####"; carLdrBatVal!(0, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(1, I);
        Print Tab(60); Using "####"; carLdrBatVal!(1, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "RUNS SCORE"; Tab(44); "RBI'S"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(2, I);
        Print Tab(20); Using "####"; carLdrBatVal!(2, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(3, I);
        Print Tab(60); Using "####"; carLdrBatVal!(3, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "DOUBLES"; Tab(44); "TRIPLES"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(4, I);
        Print Tab(20); Using "####"; carLdrBatVal!(4, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(5, I);
        Print Tab(60); Using "####"; carLdrBatVal!(5, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "HOME RUNS"; Tab(44); "WALKS"

    For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(6, I);
        Print Tab(20); Using "####"; carLdrBatVal!(6, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(7, I);
        Print Tab(60); Using "####"; carLdrBatVal!(7, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "STRIKE OUTS"; Tab(44); "STOLEN BASES"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(8, I);
        Print Tab(20); Using "####"; carLdrBatVal!(8, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(9, I);
        Print Tab(60); Using "####"; carLdrBatVal!(9, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "CAUGHT STEALING"; Tab(44); "ERRORS"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(10, I);
        Print Tab(20); Using "####"; carLdrBatVal!(10, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(11, I);
        Print Tab(60); Using "####"; carLdrBatVal!(11, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "GAMES PLAYED"; Tab(44); "BATTING AVERAGE"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(14, I);
        Print Tab(20); Using "####"; carLdrBatVal!(14, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(15, I);
        Print Tab(60); Using "#.###"; carLdrBatVal!(15, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "SLUGGING PCT."; Tab(44); "ON-BASE PCT."
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(16, I);
        Print Tab(20); Using "#.###"; carLdrBatVal!(16, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrBatName$(17, I);
        Print Tab(60); Using "#.###"; carLdrBatVal!(17, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    ' ### PITCHERS ###

    Locate 2, 1
    Print Tab(5); "INNINGS PITCHED"; Tab(44); "HITS ALLOWED"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(0, I);
        Print Tab(18); Using "###"; Int(carLdrPitVal!(0, I) / 3);
        If carLdrPitVal!(O, I) - (Int(carLdrPitVal!(O, I) / 3) * 3) > 0 Then
            Print ".";
            Print Using "#"; carLdrPitVal!(O, I) - (Int(carLdrPitVal!(O, I) / 3) * 3);:
        End If

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(1, I);
        Print Tab(60); Using "####"; carLdrPitVal!(1, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "RUNS ALLOWED"; Tab(44); "EARNED RUNS ALLOWED"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(2, I);
        Print Tab(20); Using "####"; carLdrPitVal!(2, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(3, I);
        Print Tab(60); Using "####"; carLdrPitVal!(3, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "WALKS"; Tab(44); "STRIKEOUTS"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(4, I);
        Print Tab(20); Using "####"; carLdrPitVal!(4, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(5, I);
        Print Tab(60); Using "####"; carLdrPitVal!(5, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "WINS"; Tab(44); "LOSSES"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(6, I);
        Print Tab(20); Using "####"; carLdrPitVal!(6, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(7, I);
        Print Tab(60); Using "####"; carLdrPitVal!(7, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "GAMES"; Tab(44); "GAMES STARTED"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(8, I);
        Print Tab(20); Using "####"; carLdrPitVal!(8, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(9, I);
        Print Tab(60); Using "####"; carLdrPitVal!(9, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "COMPLETE GAMES"; Tab(44); "SHUTOUTS"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(10, I);
        Print Tab(20); Using "####"; carLdrPitVal!(10, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(11, I);
        Print Tab(60); Using "####"; carLdrPitVal!(11, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "SAVES"; Tab(44); "DOUBLE PLAYS THROWN"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(12, I);
        Print Tab(20); Using "####"; carLdrPitVal!(12, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(13, I);
        Print Tab(60); Using "####"; carLdrPitVal!(13, I)
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Locate 2, 1
    Print Tab(5); "HOME RUNS ALLOWED"; Tab(44); "EARNED RUN AVERAGE"
    
	For I = 0 To 19
        Color 3
        Locate I + 3: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(14, I);
        Print Tab(20); Using "####"; carLdrPitVal!(14, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPitName$(15, I);
        If carLdrPitVal!(15, I) = 1000 Then
            Print Tab(58); Using "###.##"; 0.0
        Else
            Print Tab(58); Using "###.##"; carLdrPitVal!(15, I)
        End If
    Next

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub

