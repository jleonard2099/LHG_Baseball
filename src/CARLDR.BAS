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
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call UpdateLeaders

                Case 2:
                    Call CareerLeaderOptions

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       UpdateLeaders Subroutine
'----------------------------------------
'This routine reads in the current stats, 
'calculates the latest values (based on
'the criteria given) and then creates /
'overwrites the TCL file with the newest
'calculated values.
Sub UpdateLeaders ()

    Dim BC(100, 18), PC(80, 17)

    Shared BL0$(), PL0$()
    Shared careerB$(), careerP$()

    Shared BRS%(), PRS%()

    Shared BL1!(), PL1!()

    TMR1 = 2700

    Erase BL1!, PL1!

    '-- Initialize to 1000 because later we assign
    '-- values based on comparison. (ie, looking 
    '-- for lowest values. Nothing will ever be
    '-- lower than 0 if we don't have something here.
    For I1 = 0 To 20
        PL1!(15, I1) = 1000
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

        Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #1

        Input #1, NB, NP

        For I = 0 To 99:
            Input #1, careerB$(I)
        Next

        For I = 0 To 79:
            Input #1, careerP$(I)
        Next

        For J = 1 To 15:

            For I = 0 To 99:

                For I1 = O To 17:
                    Input #1, BRS%(J, I, I1)
                Next I1

                Color 9
                Locate 7, 38
                TMR1 = TMR1 - 1: Print Using "####"; TMR1

            Next I

        Next J

        For J = 1 To 15:

            For I = 0 To 79:

                For I1 = 0 To 16:
                    Input #1, PRS%(J, I, I1)
                Next I1

                Color 9
                Locate 7, 38
                TMR1 = TMR1 - 1: Print Using "####"; TMR1

            Next I

        Next J

        Close #1

        Color 14

        Locate 10: Print "COMPUTING LEADER FILE . . . . . . PLEASE WAIT"

        For I = 0 To 99
            For I1 = 0 To 14
                For J = 1 To 15
                    BC(I, I1) = BC(I, I1) + BRS%(J, I, I1)
                Next
            Next
        Next

        For I = 0 To 79
            For I1 = 0 To 16
                For J = 1 To 15
                    PC(I, I1) = PC(I, I1) + PRS%(J, I, I1)
                Next
            Next
        Next

        For K = 0 To 99

            For X = 0 To 14

                If BC(K, X) > BL1!(X, 19) Then

                    BL1!(X, 19) = BC(K, X)
                    BL0$(X, 19) = careerB$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If BL1!(X, J - 1) < BL1!(X, J) Then Swap BL1!(X, J - 1), BL1!(X, J): Swap BL0$(X, J - 1), BL0$(X, J)
                        Next J
                    Next I

                End If

            Next X

        Next K

        For K = 0 To 99

            If BC(K, 0) < N1 Or BC(K, 0) = 0 Then

                If BC(K, 1) / BC(K, 0) > BL1!(15, 19) Then

                    BL1!(15, 19) = BC(K, 1) / BC(K, 0)
                    BL0$(15, 19) = careerB$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If BL1!(15, J - 1) < BL1!(15, J) Then
                                Swap BL1!(15, J - 1), BL1!(15, J)
                                Swap BL0$(15, J - 1), BL0$(15, J)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        For K = 0 To 99

            If BC(K, 0) < N1 Or BC(K, 0) = 0 Then

                If (BC(K, 1) + BC(K, 4) + BC(K, 5) * 2 + BC(K, 6) * 3) / BC(K, 0) > BL1!(16, 19) Then

                    BL1!(16, 19) = (BC(K, 1) + BC(K, 4) + BC(K, 5) * 2 + BC(K, 6) * 3) / BC(K, 0)
                    BL0$(16, 19) = careerB$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If BL1!(16, J - 1) < BL1!(16, J) Then
                                Swap BL1!(16, J - 1), BL1!(16, J)
                                Swap BL0$(16, J - 1), BL0$(16, J)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        For K = 0 To 99

            If Not (BC(K, 0) < N1 Or BC(K, 0) + BC(K, 7) = 0) Then

                If (BC(K, 1) + BC(K, 7)) / (BC(K, 0) + BC(K, 7)) > BL1!(17, 19) Then

                    BL1!(17, 19) = (BC(K, 1) + BC(K, 7)) / (BC(K, 0) + BC(K, 7))
                    BL0$(17, 19) = careerB$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If BL1!(17, J - 1) < BL1!(17, J) Then
                                Swap BL1!(17, J - 1), BL1!(17, J)
                                Swap BL0$(17, J - 1), BL0$(17, J)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        For K = 0 To 79

            For X = 0 To 14

                If PC(K, X) > PL1!(X, 19) Then

                    PL1!(X, 19) = PC(K, X)
                    PL0$(X, 19) = careerP$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If PL1!(X, J - 1) < PL1!(X, J) Then
                                Swap PL1!(X, J - 1), PL1!(X, J)
                                Swap PL0$(X, J - 1), PL0$(X, J)
                            End If
                        Next J
                    Next I
                End If

            Next X

        Next K

        For K = 0 To 79

            If Not (PC(K, 0) < N2 * 3 Or PC(K, 0) = 0) Then

                If ((PC(K, 3) * 9) / PC(K, 0)) * 3 < PL1!(15, 19) Then

                    PL1!(15, 19) = ((PC(K, 3) * 9) / PC(K, 0)) * 3
                    PL0$(15, 19) = careerP$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If PL1!(15, J) < PL1!(15, J - 1) Then
                                Swap PL1!(15, J), PL1!(15, J - 1)
                                Swap PL0$(15, J), PL0$(15, J - 1)
                            End If
                        Next J
                    Next I

                End If

            End If

        Next K

        Open diskPaths$(1) + careerFile$ + ".TCL" For Output As #1

        For I = 0 To 17:
            For I1 = 0 To 19:
                Write #1, BL1!(I, I1), BL0$(I, I1)
            Next
        Next

        For I = 0 To 15:
            For I1 = 0 To 19:
                Write #1, PL1!(I, I1), PL0$(I, I1)
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

    Shared BL1!(), PL1!()
    Shared BL0$(), PL0$()

    Cls

    leaderFile$ = ChooseFile$(diskPaths$(1) + "*.TCL", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(1) + leaderFile$ + ".TCL" For Input As #1

        For I = 0 To 17:
            For I1 = 0 To 19:
                Input #1, BL1!(I, I1), BL0$(I, I1)
            Next
        Next

        For I = 0 To 15:
            For I1 = 0 To 19:
                Input #1, PL1!(I, I1), PL0$(I, I1)
            Next
        Next

        Close #1

        Color 15, 0
        Cls

        Call MenuBox(8, 25, 24, 5, 15, 1)

        Do

            Locate 10, 32
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

            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P" Or UCase$(I$) = "S"

        Select Case UCase$(I$)

            Case "F"
                userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                If userFile$ <> "" Then
                    Call PrintCareerLeaders(0, userFile$)
                End If

            Case "P"
                Call PrintCareerLeaders(1, "REPORT")
                
            Case "S"
                Call ViewCareerLeaders

        End Select

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'       PrintCareerLeaders Subroutine
'----------------------------------------
'This routine sends formatted career stats 
'(loaded from the .TCL file) to the user's 
'default printer
Sub PrintCareerLeaders (printDest, destFile$)

    Shared BL0$(), PL0$()
    Shared BL1!(), PL1!()

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    Print #1, Tab(5); "AT BATS"; Tab(29); "HITS"; Tab(54); "RUNS SCORED"; Tab(79); "RBI'S"; Tab(104); "DOUBLES"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(0, I);: Print #1, Tab(18); Using "####"; BL1!(0, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(1, I);: Print #1, Tab(43); Using "####"; BL1!(1, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(2, I);: Print #1, Tab(68); Using "####"; BL1!(2, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(3, I);: Print #1, Tab(93); Using "####"; BL1!(3, I);
        Print #1, Tab(100); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(4, I);: Print #1, Tab(118); Using "####"; BL1!(4, I)
    Next

    Print #1,
    Print #1,
    Print #1, Tab(5); "TRIPLES"; Tab(29); "HOME RUNS"; Tab(54); "WALKS"; Tab(79); "STRIKE OUTS"; Tab(104); "STOLEN BASES"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(5, I);: Print #1, Tab(18); Using "####"; BL1!(5, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(6, I);: Print #1, Tab(43); Using "####"; BL1!(6, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(7, I);: Print #1, Tab(68); Using "####"; BL1!(7, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(8, I);: Print #1, Tab(93); Using "####"; BL1!(8, I);
        Print #1, Tab(100); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(9, I);: Print #1, Tab(118); Using "####"; BL1!(9, I)
    Next

    Print #1, Chr$(12)

    Print #1,
    Print #1,
    Print #1, Tab(5); "CAUGHT STEALING"; Tab(29); "ERRORS"; Tab(54); "GAMES PLAYED"; Tab(79); "BATTING AVERAGE"; Tab(104); "SLUGGING PCT."

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(10, I);: Print #1, Tab(18); Using "####"; BL1!(10, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(11, I);: Print #1, Tab(43); Using "####"; BL1!(11, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(14, I);: Print #1, Tab(68); Using "####"; BL1!(14, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(15, I);: Print #1, Tab(93); Using "#.###"; BL1!(15, I);
        Print #1, Tab(100); Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(16, I);: Print #1, Tab(118); Using "#.###"; BL1!(16, I)
    Next

    Print #1,
    Print #1,
    Print #1, Tab(5); "ON-BASE PCT."; Tab(29); "INNINGS PITCHED"; Tab(54); "HITS ALLOWED"; Tab(79); "RUNS ALLOWED"; Tab(104); "EARNED RUNS ALLOWED"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, BL0$(17, I);: Print #1, Tab(18); Using "#.###"; BL1!(17, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(0, I);: Print #1, Tab(43); Using "###"; Int(PL1!(0, I) / 3);: If PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3) > 0 Then Print #1, ".";: Print #1, Using "#"; PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(1, I);: Print #1, Tab(68); Using "####"; PL1!(1, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(2, I);: Print #1, Tab(93); Using "####"; PL1!(2, I);
        Print #1, Tab(100); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(3, I);: Print #1, Tab(118); Using "####"; PL1!(3, I)
    Next

    Print #1, Chr$(12)

    Print #1,
    Print #1,
    Print #1, Tab(5); "WALKS"; Tab(29); "STRIKEOUTS"; Tab(54); "WINS"; Tab(79); "LOSSES"; Tab(104); "GAMES"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(4, I);: Print #1, Tab(18); Using "####"; PL1!(4, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(5, I);: Print #1, Tab(43); Using "####"; PL1!(5, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(6, I);: Print #1, Tab(68); Using "####"; PL1!(6, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(7, I);: Print #1, Tab(93); Using "####"; PL1!(7, I);
        Print #1, Tab(100); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(8, I);: Print #1, Tab(118); Using "####"; PL1!(8, I)
    Next

    Print #1,
    Print #1,
    Print #1, Tab(5); "GAMES STARTED"; Tab(29); "COMPLETE GAMES"; Tab(54); "SHUTOUTS"; Tab(79); "SAVES"; Tab(104); "DOUBLE PLAYS THROWN"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(9, I);: Print #1, Tab(18); Using "####"; PL1!(9, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(10, I);: Print #1, Tab(43); Using "####"; PL1!(10, I);
        Print #1, Tab(50); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(11, I);: Print #1, Tab(68); Using "####"; PL1!(11, I);
        Print #1, Tab(75); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(12, I);: Print #1, Tab(93); Using "####"; PL1!(12, I);
        Print #1, Tab(100); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(13, I);: Print #1, Tab(118); Using "####"; PL1!(13, I)
    Next

    Print #1, Chr$(12)

    Print #1,
    Print #1,
    Print #1, Tab(5); "HOME RUNS ALLOWED"; Tab(29); "EARNED RUN AVERAGE"

    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(14, I);: Print #1, Tab(20); Using "####"; PL1!(14, I);
        Print #1, Tab(25); Using "##"; I + 1;: Print #1, ". ";: Print #1, PL0$(15, I);
        If PL1!(15, I) = 1000 Then
            Print #1, Tab(43); Using "###.##"; 0.0
        Else
            Print #1, Tab(43); Using "###.##"; PL1!(15, I)
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

    Shared BL0$(), PL0$()
    Shared BL1!(), PL1!()

    Cls
    Color 12

    Print Tab(5); "AT BATS                                HITS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(0, I);
        Print Tab(20); Using "####"; BL1!(0, I);:

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(1, I);
        Print Tab(60); Using "####"; BL1!(1, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "RUNS SCORED                            RBI'S"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(2, I);
        Print Tab(20); Using "####"; BL1!(2, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(3, I);
        Print Tab(60); Using "####"; BL1!(3, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "DOUBLES                                TRIPLES"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(4, I);
        Print Tab(20); Using "####"; BL1!(4, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(5, I);
        Print Tab(60); Using "####"; BL1!(5, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "HOME RUNS                              WALKS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(6, I);
        Print Tab(20); Using "####"; BL1!(6, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(7, I);
        Print Tab(60); Using "####"; BL1!(7, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "STRIKE OUTS                            STOLEN BASES"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(8, I);
        Print Tab(20); Using "####"; BL1!(8, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(9, I);
        Print Tab(60); Using "####"; BL1!(9, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "CAUGHT STEALING                        ERRORS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(10, I);
        Print Tab(20); Using "####"; BL1!(10, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(11, I);
        Print Tab(60); Using "####"; BL1!(11, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "GAMES PLAYED                           BATTING AVERAGE"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(14, I);
        Print Tab(20); Using "####"; BL1!(14, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(15, I);
        Print Tab(60); Using "#.###"; BL1!(15, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "SLUGGING PCT.                          ON-BASE PCT."
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(16, I);
        Print Tab(20); Using "#.###"; BL1!(16, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print BL0$(17, I);
        Print Tab(60); Using "#.###"; BL1!(17, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "INNINGS PITCHED                        HITS ALLOWED"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(0, I);
        Print Tab(18); Using "###"; Int(PL1!(0, I) / 3);
        If PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(1, I);
        Print Tab(60); Using "####"; PL1!(1, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "RUNS ALLOWED                           EARNED RUNS ALLOWED"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(2, I);
        Print Tab(20); Using "####"; PL1!(2, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(3, I);
        Print Tab(60); Using "####"; PL1!(3, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "WALKS                                  STRIKEOUTS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(4, I);
        Print Tab(20); Using "####"; PL1!(4, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(5, I);
        Print Tab(60); Using "####"; PL1!(5, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "WINS                                   LOSSES"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(6, I);
        Print Tab(20); Using "####"; PL1!(6, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(7, I);
        Print Tab(60); Using "####"; PL1!(7, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "GAMES                                  GAMES STARTED"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(8, I);
        Print Tab(20); Using "####"; PL1!(8, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(9, I);
        Print Tab(60); Using "####"; PL1!(9, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "COMPLETE GAMES                         SHUTOUTS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(10, I);
        Print Tab(20); Using "####"; PL1!(10, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(11, I);
        Print Tab(60); Using "####"; PL1!(11, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "SAVES                                  DOUBLE PLAYS THROWN"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(12, I);
        Print Tab(20); Using "####"; PL1!(12, I);:
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(13, I);
        Print Tab(60); Using "####"; PL1!(13, I)
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "HOME RUNS ALLOWED                     EARNED RUN AVERAGE"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(14, I);
        Print Tab(20); Using "####"; PL1!(14, I);
        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print PL0$(15, I);
        If PL1!(15, I) = 1000 Then
            Print Tab(58); Using "###.##"; 0.0
        Else
            Print Tab(58); Using "###.##"; PL1!(15, I)
        End If
    Next

    Locate 23, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub


