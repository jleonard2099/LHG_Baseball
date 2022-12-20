Sub CARLDR ()

    _Title "Full Count Baseball - Career Leaders"

    DefInt A-Z

    TMR1 = 2700

    Do
        Cls

        Color 12: Print "CAREER LEADER MENU"
        Color 14: Print "(1)  ";: Color 15: Print "UPDATE LEADERS": Print
        Color 14: Print "(2)  ";: Color 15: Print "VIEW/PRINT CAREER LEADERS": Print
        Color 14: Print "(3)  ";: Color 15: Print "RETURN TO UTILITIES MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 3

        Select Case I
            Case 1: Call UpdateLeaders
            Case 2: Call ViewPrintLeaders
            Case 3: Cls
        End Select

    Loop Until I = 3

End Sub

'----------------------------------------
'       UpdateLeaders Subroutine
'----------------------------------------
'...explanation...
Sub UpdateLeaders ()

    Dim BC(100, 18), PC(80, 17)

    Shared BL0$(), PL0$()
    Shared careerB$(), careerP$()

    Shared BRS%(), PRS%()

    Shared BL1!(), PL1!()

    For I = 0 To 17: For I1 = 0 To 19: BL1!(I, I1) = 0: Next: Next

    For I = 0 To 14: For I1 = 0 To 19: PL1!(I, I1) = 0: Next: Next

    For I1 = 0 To 20: PL1!(15, I1) = 1000: Next

    Do
    'Display list of files...

        Spec$ = diskPaths$(1) + "*.CAR"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then 
            Locate 10, 8
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim careerFiles$(0 To Count%)
            For X = 1 To Count%
                careerFiles$(X) = Space$(12)
            Next
            careerFiles$(0) = Spec$
            Call ReadFile(careerFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

					Color 15, 1
                    targetFile$ = careerFiles$(Selection%)
                    Locate 2, 33: Print targetFile$
					
					Do
						Locate , 33: Print "ANY CHANGE (YN)";
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

					Color 15, 0

                End If

            Loop Until UCase$(I$) = "N"

            If ExitCode% <> 27 Then

                leaderFile$ = RTrim$(targetFile$)

                Cls

                Do

                    Input "ENTER NUMBER OF AT BATS TO QUALIFY"; N1
                    Input "ENTER NUMBER OF INNINGS PITCHED TO QUALIFY"; N2

					Do
						Locate 7: Print "ANY CHANGE (Y/N)"
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Loop Until UCase$(I$) = "N"

                Open diskPaths$(1) + leaderFile$ For Input As #1
                Input #1, NB, NP
                For I = 0 To 99: Input #1, careerB$(I): Next
                For I = 0 To 79: Input #1, careerP$(I): Next
                For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #1, BRS%(J, I, I1): Next I1: Locate 7, 35: TMR1 = TMR1 - 1: Color 9: Print Using "####"; TMR1: Next I: Next J
                For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #1, PRS%(J, I, I1): Next I1: Locate 7, 35: TMR1 = TMR1 - 1: Color 9: Print Using "####"; TMR1: Next I: Next J
                Close #1

                Locate 10: Color 14: Print "COMPUTING LEADER FILE . . . . . . PLEASE WAIT"

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

                            BL1!(X, 19) = BC(K, X): BL0$(X, 19) = careerB$(K)

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

                            BL1!(15, 19) = BC(K, 1) / BC(K, 0): BL0$(15, 19) = careerB$(K)
                            For I = 1 To 20
                                For J = 20 To I Step -1
                                    If BL1!(15, J - 1) < BL1!(15, J) Then Swap BL1!(15, J - 1), BL1!(15, J): Swap BL0$(15, J - 1), BL0$(15, J)
                                Next J
                            Next I

                        End If

                    End If

                Next K

                For K = 0 To 99
                    If BC(K, 0) < N1 Or BC(K, 0) = 0 Then

                        If (BC(K, 1) + BC(K, 4) + BC(K, 5) * 2 + BC(K, 6) * 3) / BC(K, 0) > BL1!(16, 19) Then

                            BL1!(16, 19) = (BC(K, 1) + BC(K, 4) + BC(K, 5) * 2 + BC(K, 6) * 3) / BC(K, 0): BL0$(16, 19) = careerB$(K)

                            For I = 1 To 20
                                For J = 20 To I Step -1
                                    If BL1!(16, J - 1) < BL1!(16, J) Then Swap BL1!(16, J - 1), BL1!(16, J): Swap BL0$(16, J - 1), BL0$(16, J)
                                Next J
                            Next I

                        End If

                    End If

                Next K

                For K = 0 To 99

                    If Not (BC(K, 0) < N1 Or BC(K, 0) + BC(K, 7) = 0) Then

                        If (BC(K, 1) + BC(K, 7)) / (BC(K, 0) + BC(K, 7)) > BL1!(17, 19) Then

                            BL1!(17, 19) = (BC(K, 1) + BC(K, 7)) / (BC(K, 0) + BC(K, 7)): BL0$(17, 19) = careerB$(K)
                            For I = 1 To 20
                                For J = 20 To I Step -1
                                    If BL1!(17, J - 1) < BL1!(17, J) Then Swap BL1!(17, J - 1), BL1!(17, J): Swap BL0$(17, J - 1), BL0$(17, J)
                                Next J
                            Next I

                        End If

                    End If

                Next K

                For K = 0 To 79
                    For X = 0 To 14

                        If PC(K, X) > PL1!(X, 19) Then

                            532 PL1!(X, 19) = PC(K, X): PL0$(X, 19) = careerP$(K)
                            For I = 1 To 20
                                For J = 20 To I Step -1
                                    If PL1!(X, J - 1) < PL1!(X, J) Then Swap PL1!(X, J - 1), PL1!(X, J): Swap PL0$(X, J - 1), PL0$(X, J)
                                Next J
                            Next I
                        End If

                    Next X

                Next K

                For K = 0 To 79
                    If Not(PC(K, 0) < N2 * 3 Or PC(K, 0) = 0) Then

                        If ((PC(K, 3) * 9) / PC(K, 0)) * 3 < PL1!(15, 19) Then

                            PL1!(15, 19) = ((PC(K, 3) * 9) / PC(K, 0)) * 3: PL0$(15, 19) = careerP$(K)
                            For I = 1 To 20
                                For J = 20 To I Step -1
                                    If PL1!(15, J) < PL1!(15, J - 1) Then Swap PL1!(15, J), PL1!(15, J - 1): Swap PL0$(15, J), PL0$(15, J - 1)
                                Next J
                            Next I

                        End If

                    End If

                Next K

                D = 0
                For J = 0 To 8
                    D = D + 1
                    X$ = Mid$(leaderFile$, D, 1)
                    If X$ = "." Then JX = D - 1
                Next J

                Open diskPaths$(1) + Left$(leaderFile$, JX) + ".TCL" For Output As #1
                For I = 0 To 17: For I1 = 0 To 19: Write #1, BL1!(I, I1), BL0$(I, I1): Next: Next
                For I = 0 To 15: For I1 = 0 To 19: Write #1, PL1!(I, I1), PL0$(I, I1): Next: Next
                Close #1

            End If 'Done checking for ESC

        End If 'Done checking file count

    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'       ViewPrintLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewPrintLeaders ()

    Shared BL1!(), PL1!()
    Shared BL0$(), PL0$()

    Cls

    Do
    'Display list of files...
        Spec$ = diskPaths$(1) + "*.TCL"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then 
            Locate 10, 8
            Call CSMsgBox("NO CAREER LEADER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim careerLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                careerLeaderFiles$(X) = Space$(12)
            Next
            careerLeaderFiles$(0) = Spec$
            Call ReadFile(careerLeaderFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerLeaderFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

					Color 15, 1
                    targetFile$ = careerLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

					Do
						Locate , 33: Print "ANY CHANGE (YN)";
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

					Color 15, 0

                End If

            Loop Until UCase$(I$) = "N"

			If ExitCode% <> 27 Then

				Cls

				Z1$ = RTrim$(targetFile$)

				Open diskPaths$(1) + Z1$ For Input As #1
				For I = 0 To 17: For I1 = 0 To 19: Input #1, BL1!(I, I1), BL0$(I, I1): Next: Next
				For I = 0 To 15: For I1 = 0 To 19: Input #1, PL1!(I, I1), PL0$(I, I1): Next: Next
				Close #1

				' -- Choose --
				Color 15, 0
				Cls
				Locate 8: Color 14: Print "P";: Color 15: Print "RINT OR ";: Color 14: Print "V";: Color 15: Print "IEW"

				Do
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

				If UCase$(I$) = "P" Then 
					Call PrintLeaders_Screen
				Else
					Call ViewLeaders_Screen
				End If

			End If 'Done checking for ESC

		End If 'Done checking file count

    '...Until we press ESC
    Loop Until ExitCode% = 27 or Count% = 0

End Sub

'----------------------------------------
'       PrintLeaders_Screen Subroutine
'----------------------------------------
'...explanation...
Sub PrintLeaders_Screen ()

    Shared BL0$(), PL0$()
    Shared BL1!(), PL1!()

    LPrint Chr$(15)

    'WIDTH "LPT1:1",130
    LPrint Tab(5); "AT BATS"; Tab(29); "HITS"; Tab(54); "RUNS SCORED"; Tab(79); "RBI'S"; Tab(104); "DOUBLES"
    For I = 0 To 19
        LPrint Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(0, I);: LPrint Tab(18); Using "####"; BL1!(0, I);: LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(1, I);: LPrint Tab(43); Using "####"; BL1!(1, I);
        LPrint Tab(50); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(2, I);: LPrint Tab(68); Using "####"; BL1!(2, I);: LPrint Tab(75); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(3, I);: LPrint Tab(93); Using "####"; BL1!(3, I);
    LPrint Tab(100); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(4, I);: LPrint Tab(118); Using "####"; BL1!(4, I): Next
    LPrint
    LPrint
    LPrint Tab(5); "TRIPLES"; Tab(29); "HOME RUNS"; Tab(54); "WALKS"; Tab(79); "STRIKE OUTS"; Tab(104); "STOLEN BASES"
    For I = 0 To 19
        LPrint Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(5, I);: LPrint Tab(18); Using "####"; BL1!(5, I);: LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(6, I);: LPrint Tab(43); Using "####"; BL1!(6, I);
        LPrint Tab(50); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(7, I);: LPrint Tab(68); Using "####"; BL1!(7, I);: LPrint Tab(75); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(8, I);: LPrint Tab(93); Using "####"; BL1!(8, I);
    LPrint Tab(100); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(9, I);: LPrint Tab(118); Using "####"; BL1!(9, I): Next

    LPrint Chr$(12)

    LPrint
    LPrint
    LPrint Tab(5); "CAUGHT STEALING"; Tab(29); "ERRORS"; Tab(54); "GAMES PLAYED"; Tab(79); "BATTING AVERAGE"; Tab(104); "SLUGGING PCT."
    For I = 0 To 19
        LPrint Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(10, I);: LPrint Tab(18); Using "####"; BL1!(10, I);: LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(11, I);: LPrint Tab(43); Using "####"; BL1!(11, I);
        LPrint Tab(50); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(14, I);: LPrint Tab(68); Using "####"; BL1!(14, I);: LPrint Tab(75); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(15, I);: LPrint Tab(93); Using ".###"; BL1!(15, I);
    LPrint Tab(100); Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(16, I);: LPrint Tab(118); Using ".###"; BL1!(16, I): Next
    LPrint
    LPrint
    LPrint Tab(5); "ON-BASE PCT."; Tab(29); "INNINGS PITCHED"; Tab(54); "HITS ALLOWED"; Tab(79); "RUNS ALLOWED"; Tab(104); "EARNED RUNS ALLOWED"
    For I = 0 To 19
        LPrint Using "##"; I + 1;: LPrint ". ";: LPrint BL0$(17, I);: LPrint Tab(18); Using ".###"; BL1!(17, I);
        LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(0, I);: LPrint Tab(43); Using "###"; Int(PL1!(0, I) / 3);: If PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3);
        LPrint Tab(50); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(1, I);: LPrint Tab(68); Using "####"; PL1!(1, I);: LPrint Tab(75); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(2, I);: LPrint Tab(93); Using "####"; PL1!(2, I);
    LPrint Tab(100); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(3, I);: LPrint Tab(118); Using "####"; PL1!(3, I): Next

    LPrint Chr$(12)

    LPrint
    LPrint
    LPrint Tab(5); "WALKS"; Tab(29); "STRIKEOUTS"; Tab(54); "WINS"; Tab(79); "LOSSES"; Tab(104); "GAMES"
    For I = 0 To 19
        LPrint Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(4, I);: LPrint Tab(18); Using "####"; PL1!(4, I);: LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(5, I);: LPrint Tab(43); Using "####"; PL1!(5, I);
        LPrint Tab(50); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(6, I);: LPrint Tab(68); Using "####"; PL1!(6, I);: LPrint Tab(75); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(7, I);: LPrint Tab(93); Using "####"; PL1!(7, I);
    LPrint Tab(100); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(8, I);: LPrint Tab(118); Using "####"; PL1!(8, I): Next
    LPrint
    LPrint
    LPrint Tab(5); "GAMES STARTED"; Tab(29); "COMPLETE GAMES"; Tab(54); "SHUTOUTS"; Tab(79); "SAVES"; Tab(104); "DOUBLE PLAYS THROWN"
    For I = 0 To 19
        LPrint Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(9, I);: LPrint Tab(18); Using "####"; PL1!(9, I);: LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(10, I);: LPrint Tab(43); Using "####"; PL1!(10, I);
        LPrint Tab(50); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(11, I);: LPrint Tab(68); Using "####"; PL1!(11, I);: LPrint Tab(75); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(12, I);: LPrint Tab(93); Using "####"; PL1!(12, I);
    LPrint Tab(100); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(13, I);: LPrint Tab(118); Using "####"; PL1!(13, I): Next

    LPrint Chr$(12)

    LPrint
    LPrint
    LPrint Tab(5); "HOME RUNS ALLOWED"; Tab(29); "EARNED RUN AVERAGE"
    For I = 0 To 19
    LPrint Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(14, I);: LPrint Tab(20); Using "####"; PL1!(14, I);: LPrint Tab(25); Using "##"; I + 1;: LPrint ". ";: LPrint PL0$(15, I);: LPrint Tab(43); Using "###.##"; PL1!(15, I): Next

    LPrint Chr$(12)

End Sub

'----------------------------------------
'     ViewLeaders_Screen Subroutine
'----------------------------------------
'...explanation...
Sub ViewLeaders_Screen ()

    Shared BL0$(), PL0$()
    Shared BL1!(), PL1!()

    Cls
    Color 12: Print Tab(5); "AT BATS                                HITS"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(0, I);: Color 14: Print Tab(20); Using "####"; BL1!(0, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(1, I);: Color 14: Print Tab(60); Using "####"; BL1!(1, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "RUNS SCORED                            RBI'S"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(2, I);: Color 14: Print Tab(20); Using "####"; BL1!(2, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(3, I);: Color 14: Print Tab(60); Using "####"; BL1!(3, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "DOUBLES                                TRIPLES"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(4, I);: Color 14: Print Tab(20); Using "####"; BL1!(4, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(5, I);: Color 14: Print Tab(60); Using "####"; BL1!(5, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "HOME RUNS                              WALKS"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(6, I);: Color 14: Print Tab(20); Using "####"; BL1!(6, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(7, I);: Color 14: Print Tab(60); Using "####"; BL1!(7, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "STRIKE OUTS                            STOLEN BASES"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(8, I);: Color 14: Print Tab(20); Using "####"; BL1!(8, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(9, I);: Color 14: Print Tab(60); Using "####"; BL1!(9, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "CAUGHT STEALING                        ERRORS"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(10, I);: Color 14: Print Tab(20); Using "####"; BL1!(10, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(11, I);: Color 14: Print Tab(60); Using "####"; BL1!(11, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "GAMES PLAYED                           BATTING AVERAGE"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(14, I);: Color 14: Print Tab(20); Using "####"; BL1!(14, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(15, I);: Color 14: Print Tab(60); Using ".###"; BL1!(15, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "SLUGGING PCT.                          ON-BASE PCT."
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(16, I);: Color 14: Print Tab(20); Using ".###"; BL1!(16, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print BL0$(17, I);: Color 14: Print Tab(60); Using ".###"; BL1!(17, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "INNINGS PITCHED                        HITS ALLOWED"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(0, I);: Color 14: Print Tab(18); Using "###"; Int(PL1!(0, I) / 3);: If PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PL1!(O, I) - (Int(PL1!(O, I) / 3) * 3);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(1, I);: Color 14: Print Tab(60); Using "####"; PL1!(1, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "RUNS ALLOWED                           EARNED RUNS ALLOWED"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(2, I);: Color 14: Print Tab(20); Using "####"; PL1!(2, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(3, I);: Color 14: Print Tab(60); Using "####"; PL1!(3, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "WALKS                                  STRIKEOUTS"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(4, I);: Color 14: Print Tab(20); Using "####"; PL1!(4, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(5, I);: Color 14: Print Tab(60); Using "####"; PL1!(5, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "WINS                                   LOSSES"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(6, I);: Color 14: Print Tab(20); Using "####"; PL1!(6, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(7, I);: Color 14: Print Tab(60); Using "####"; PL1!(7, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "GAMES                                  GAMES STARTED"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(8, I);: Color 14: Print Tab(20); Using "####"; PL1!(8, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(9, I);: Color 14: Print Tab(60); Using "####"; PL1!(9, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "COMPLETE GAMES                         SHUTOUTS"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(10, I);: Color 14: Print Tab(20); Using "####"; PL1!(10, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(11, I);: Color 14: Print Tab(60); Using "####"; PL1!(11, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "SAVES                                  DOUBLE PLAYS THROWN"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(12, I);: Color 14: Print Tab(20); Using "####"; PL1!(12, I);:
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(13, I);: Color 14: Print Tab(60); Using "####"; PL1!(13, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12: Print Tab(5); "HOME RUNS ALLOWED                     EARNED RUN AVERAGE"
    For I = 0 To 19
        Color 3: Locate I + 2: Print Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(14, I);: Color 14: Print Tab(20); Using "####"; PL1!(14, I);
    Color 3: Print Tab(40); Using "##"; I + 1;: Print ". ";: Color 15: Print PL0$(15, I);: Color 14: Print Tab(58); Using "###.##"; PL1!(15, I): Next

    Locate 23, 22: Color 15: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub
