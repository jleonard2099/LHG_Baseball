Sub CAREER ()

    ReDim menuChoices$(1 To 7)

    _Title "Full Count Baseball - Career Files"

    Do

        Color 15, 0

        Cls

        'Need to replace with custom CAREER/UTILS title
        'to be consistent with all other games
        Call TITLE

        menuChoices$(1) = "CREATE AND MERGE INITIAL CAREER STATS  "
        menuChoices$(2) = "MERGE/UPDATE CAREER STATS              "
        menuChoices$(3) = "VIEW/PRINT CAREER STATS                "
        menuChoices$(4) = "EDIT CAREER STATS                      "
        menuChoices$(5) = "VIEW/PRINT TEAM CAREER LEADERS (SEASON)"
        menuChoices$(6) = "UPDATE TEAM CAREER LEADERS (CAREER)    "
        menuChoices$(7) = "RETURN TO MAIN MENU                    "
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 21

        Call box0(6, 20, 14, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Cls
                    Color 14

                    Locate 5, 27: Print " - - - C A U T I O N - - -"

                    Do
                        Color 15: Locate 8, 18
                        Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
                        Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
                        Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
                        Locate , 18: Print "            USE OPTION #2."
                        Print
                        Print
                        Print
                        Color 12
                        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
                        I$ = GetKeyPress$

                        If UCase$(I$) = "Y" Then
                            Call Merge_FirstYear
                        End If 'Done confirming user continue

                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                Case 2:
                    Cls
                    Color 14

                    Locate 5, 27
                    Print "- - - C A U T I O N - - -"

                    Do

                        Color 15: Locate 8, 8
                        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
                        Print "FILE.  IF YOU ARE CREATING A NEW CAREER FILE, USE OPTION #1.  IF YOU ARE ADDING"
                        Print "AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER SLOTS (XXX, END,"
                        Print "ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
                        Locate , 8: Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
                        Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
                        Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
                        Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
                        Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
                        Print "MERGING, OR YOU COULD CHANGE HIM TO 'B.WILLIAMS' IN THE CAREER FILE BY USING"
                        Print "THE MODIFY REPLAY STATS OPTION FROM THE MAIN MENU."

                        Print
                        Print

                        Color 12
                        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
                        I$ = GetKeyPress$

                        If UCase$(I$) = "Y" Then
                            Call Merge_AddYear
                        End If

                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                Case 3: Call CARVIEW
                Case 4: Call CAREDIT
                Case 5: Call ViewTeamLeaders
                Case 6: Call CARLDR
                Case 7: Cls
            End Select
        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

End Sub

'----------------------------------------
'      ViewTeamLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewTeamLeaders ()

    Shared BLL!(), PLL!()
    Shared BLYR%(), PLYR%()

    Shared BL$(), careerPL$()

    'Display list of files...
    Do

        Cls
        Spec$ = diskPaths$(2) + "*.CLD"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CAREER LEADER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            'GoTo 2
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

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                leaderFile$ = RTrim$(targetFile$)

                Open diskPaths$(2) + leaderFile$ For Input As #1
                For I = 0 To 14: Input #1, BLYR%(I), BL$(I), BLL!(I): Next
                For I = 0 To 14: Input #1, PLYR%(I), careerPL$(I), PLL!(I): Next
                Close #1

                Color 15, 0
                Cls

                Do
                    Locate 15, 10
                    Color 12: Print "V";: Color 7: Print "iew or ";
                    Color 12: Print "P";: Color 7: Print "rint"
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

                Select Case UCase$(I$)

                    Case "P":
                        LPrint "CATEGORY           RECORD           PLAYER              YEAR"
                        LPrint
                        LPrint "AT BATS";: LPrint Tab(23); Using "###"; BLL!(0);: LPrint Tab(37); BL$(0);: LPrint Tab(56); BLYR%(0)
                        LPrint "RUNS SCORED";: LPrint Tab(23); Using "###"; BLL!(1);: LPrint Tab(37); BL$(1);: LPrint Tab(56); BLYR%(1)
                        LPrint "HITS";: LPrint Tab(23); Using "###"; BLL!(2);: LPrint Tab(37); BL$(2);: LPrint Tab(56); BLYR%(2)
                        LPrint "DOUBLES";: LPrint Tab(23); Using "###"; BLL!(3);: LPrint Tab(37); BL$(3);: LPrint Tab(56); BLYR%(3)
                        LPrint "TRIPLES";: LPrint Tab(23); Using "###"; BLL!(4);: LPrint Tab(37); BL$(4);: LPrint Tab(56); BLYR%(4)
                        LPrint "HOME RUNS";: LPrint Tab(23); Using "###"; BLL!(5);: LPrint Tab(37); BL$(5);: LPrint Tab(56); BLYR%(5)
                        LPrint "RUNS BATTED IN";: LPrint Tab(23); Using "###"; BLL!(6);: LPrint Tab(37); BL$(6);: LPrint Tab(56); BLYR%(6)
                        LPrint "WALKS";: LPrint Tab(23); Using "###"; BLL!(7);: LPrint Tab(37); BL$(7);: LPrint Tab(56); BLYR%(7)
                        LPrint "STRIKEOUTS";: LPrint Tab(23); Using "###"; BLL!(8);: LPrint Tab(37); BL$(8);: LPrint Tab(56); BLYR%(8)
                        LPrint "STEALS";: LPrint Tab(23); Using "###"; BLL!(9);: LPrint Tab(37); BL$(9);: LPrint Tab(56); BLYR%(9)
                        LPrint "CAUGHT STEALING";: LPrint Tab(23); Using "###"; BLL!(10);: LPrint Tab(37); BL$(10);: LPrint Tab(56); BLYR%(10)
                        LPrint "ERRORS";: LPrint Tab(23); Using "###"; BLL!(11);: LPrint Tab(37); BL$(11);: LPrint Tab(56); BLYR%(11)

                        LPrint "BATTING AVERAGE";

                        If BLL!(12) > 0 Then
                            LPrint Tab(22); Using ".###"; BLL!(12);: LPrint Tab(37); BL$(12);: LPrint Tab(56); BLYR%(12)
                        Else
                            LPrint Tab(22); " ---"
                        End If

                        LPrint "SLUGGING PCT.";
                        If BLL!(13) > 0 Then
                            LPrint Tab(22); Using "#.###"; BLL!(13);: LPrint Tab(37); BL$(13);: LPrint Tab(56); BLYR%(13)
                        Else
                            LPrint Tab(22); " ---"
                        End If

                        LPrint "ON BASE PCT.";
                        If BLL!(14) > 0 Then
                            LPrint Tab(22); Using "#.###"; BLL!(14);: LPrint Tab(37); BL$(14);: LPrint Tab(56); BLYR%(14)
                        Else
                            LPrint Tab(22); " ---"
                        End If

                        LPrint "WINS";: LPrint Tab(23); Using "###"; PLL!(0);: LPrint Tab(37); careerPL$(0);: LPrint Tab(56); PLYR%(0)
                        LPrint "LOSSES";: LPrint Tab(23); Using "###"; PLL!(1);: LPrint Tab(37); careerPL$(1);: LPrint Tab(56); PLYR%(1)
                        LPrint "SAVES";: LPrint Tab(23); Using "###"; PLL!(2);: LPrint Tab(37); careerPL$(2);: LPrint Tab(56); PLYR%(2)
                        LPrint "INNINGS PITCHED";: LPrint Tab(23); Using "###"; PLL!(3);:

                        If PLL!(3) - (Int(PLL!(3) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PLL!(3) - (Int(PLL!(3) / 3) * 3);:

                        LPrint Tab(37); careerPL$(3);: LPrint Tab(56); PLYR%(3)
                        LPrint "HITS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(4);: LPrint Tab(37); careerPL$(4);: LPrint Tab(56); PLYR%(4)
                        LPrint "WALKS";: LPrint Tab(23); Using "###"; PLL!(5);: LPrint Tab(37); careerPL$(5);: LPrint Tab(56); PLYR%(5)
                        LPrint "STRIKOUTS";: LPrint Tab(23); Using "###"; PLL!(6);: LPrint Tab(37); careerPL$(6);: LPrint Tab(56); PLYR%(6)

                        LPrint "EARNED RUN AVERAGE";:
                        If PLL!(7) > 0 Then
                            LPrint Tab(22); Using "##.##"; PLL!(7);: LPrint Tab(37); careerPL$(7);: LPrint Tab(56); PLYR%(7)
                        Else
                            LPrint Tab(22); " ---"
                        End If

                        LPrint "GAMES";: LPrint Tab(23); Using "###"; PLL!(8);: LPrint Tab(37); careerPL$(8);: LPrint Tab(56); PLYR%(8)
                        LPrint "COMPLETE GAMES";: LPrint Tab(23); Using "###"; PLL!(9);: LPrint Tab(37); careerPL$(9);: LPrint Tab(56); PLYR%(9)
                        LPrint "RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(10);: LPrint Tab(37); careerPL$(10);: LPrint Tab(56); PLYR%(10)
                        LPrint "EARNED RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(11);: LPrint Tab(37); careerPL$(11);: LPrint Tab(56); PLYR%(11)
                        LPrint "SHUTOUTS";: LPrint Tab(23); Using "###"; PLL!(12);: LPrint Tab(37); careerPL$(12);: LPrint Tab(56); PLYR%(12)
                        LPrint "HOME RUNS ALLOWED";: LPrint Tab(23); Using "###"; PLL!(13);: LPrint Tab(37); careerPL$(13);: LPrint Tab(56); PLYR%(13)
                        LPrint "DOUBLE PLAYS";: LPrint Tab(23); Using "###"; PLL!(14);: LPrint Tab(37); careerPL$(14);: LPrint Tab(56); PLYR%(14)
                        LPrint Chr$(12)

                    Case "V":
                        Do
                            Cls

                            Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

                            Print

                            Color 3: Print "AT BATS";: Color 7: Print Tab(23); Using "###"; BLL!(0);: Print Tab(37); BL$(0);: Print Tab(56); BLYR%(0)
                            Color 3: Print "RUNS SCORED";: Color 7: Print Tab(23); Using "###"; BLL!(1);: Print Tab(37); BL$(1);: Print Tab(56); BLYR%(1)
                            Color 3: Print "HITS";: Color 7: Print Tab(23); Using "###"; BLL!(2);: Print Tab(37); BL$(2);: Print Tab(56); BLYR%(2)
                            Color 3: Print "DOUBLES";: Color 7: Print Tab(23); Using "###"; BLL!(3);: Print Tab(37); BL$(3);: Print Tab(56); BLYR%(3)
                            Color 3: Print "TRIPLES";: Color 7: Print Tab(23); Using "###"; BLL!(4);: Print Tab(37); BL$(4);: Print Tab(56); BLYR%(4)
                            Color 3: Print "HOME RUNS";: Color 7: Print Tab(23); Using "###"; BLL!(5);: Print Tab(37); BL$(5);: Print Tab(56); BLYR%(5)
                            Color 3: Print "RUNS BATTED IN";: Color 7: Print Tab(23); Using "###"; BLL!(6);: Print Tab(37); BL$(6);: Print Tab(56); BLYR%(6)
                            Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; BLL!(7);: Print Tab(37); BL$(7);: Print Tab(56); BLYR%(7)
                            Color 3: Print "STRIKEOUTS";: Color 7: Print Tab(23); Using "###"; BLL!(8);: Print Tab(37); BL$(8);: Print Tab(56); BLYR%(8)
                            Color 3: Print "STEALS";: Color 7: Print Tab(23); Using "###"; BLL!(9);: Print Tab(37); BL$(9);: Print Tab(56); BLYR%(9)
                            Color 3: Print "CAUGHT STEALING";: Color 7: Print Tab(23); Using "###"; BLL!(10);: Print Tab(37); BL$(10);: Print Tab(56); BLYR%(10)
                            Color 3: Print "ERRORS";: Color 7: Print Tab(23); Using "###"; BLL!(11);: Print Tab(37); BL$(11);: Print Tab(56); BLYR%(11)

                            Color 3: Print "BATTING AVERAGE";: Color 7

                            If BLL!(12) > 0 Then
                                Print Tab(22); Using ".###"; BLL!(12);: Print Tab(37); BL$(12);: Print Tab(56); BLYR%(12)
                            Else
                                Print Tab(22); " ---"
                            End If

                            Color 3: Print "SLUGGING PCT.";: Color 7:
                            If BLL!(13) > 0 Then
                                Print Tab(22); Using "#.###"; BLL!(13);: Print Tab(37); BL$(13);: Print Tab(56); BLYR%(13)
                            Else
                                Print Tab(22); " ---"
                            End If

                            Color 3: Print "ON BASE PCT.";: Color 7:
                            If BLL!(13) > 0 Then
                                Print Tab(22); Using "#.###"; BLL!(14);: Print Tab(37); BL$(14);: Print Tab(56); BLYR%(14)
                            Else
                                Print Tab(22); " ---"
                            End If

                            Color 15: Locate 22: Print "HIT ANY KEY TO CONTINUE"
                            I$ = GetKeyPress$

                            Cls

                            Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

                            Print

                            Color 3: Print "WINS";: Color 7: Print Tab(23); Using "###"; PLL!(0);: Print Tab(37); careerPL$(0);: Print Tab(56); PLYR%(0)
                            Color 3: Print "LOSSES";: Color 7: Print Tab(23); Using "###"; PLL!(1);: Print Tab(37); careerPL$(1);: Print Tab(56); PLYR%(1)
                            Color 3: Print "SAVES";: Color 7: Print Tab(23); Using "###"; PLL!(2);: Print Tab(37); careerPL$(2);: Print Tab(56); PLYR%(2)
                            Color 3: Print "INNINGS PITCHED";: Color 7: Print Tab(23); Using "###"; PLL!(3);:

                            If PLL!(3) - (Int(PLL!(3) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PLL!(3) - (Int(PLL!(3) / 3) * 3);:

                            Print Tab(37); careerPL$(3);: Print Tab(56); PLYR%(3)
                            Color 3: Print "HITS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(4);: Print Tab(37); careerPL$(4);: Print Tab(56); PLYR%(4)
                            Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; PLL!(5);: Print Tab(37); careerPL$(5);: Print Tab(56); PLYR%(5)
                            Color 3: Print "STRIKOUTS";: Color 7: Print Tab(23); Using "###"; PLL!(6);: Print Tab(37); careerPL$(6);: Print Tab(56); PLYR%(6)

                            Color 3: Print "EARNED RUN AVERAGE";: Color 7
                            If PLL!(7) > 0 Then
                                Print Tab(22); Using "##.##"; PLL!(7);: Print Tab(37); careerPL$(7);: Print Tab(56); PLYR%(7)
                            Else
                                Print Tab(22); " ---"
                            End If

                            Color 3: Print "GAMES";: Color 7: Print Tab(23); Using "###"; PLL!(8);: Print Tab(37); careerPL$(8);: Print Tab(56); PLYR%(8)
                            Color 3: Print "COMPLETE GAMES";: Color 7: Print Tab(23); Using "###"; PLL!(9);: Print Tab(37); careerPL$(9);: Print Tab(56); PLYR%(9)
                            Color 3: Print "RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(10);: Print Tab(37); careerPL$(10);: Print Tab(56); PLYR%(10)
                            Color 3: Print "EARNED RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(11);: Print Tab(37); careerPL$(11);: Print Tab(56); PLYR%(11)
                            Color 3: Print "SHUTOUTS";: Color 7: Print Tab(23); Using "###"; PLL!(12);: Print Tab(37); careerPL$(12);: Print Tab(56); PLYR%(12)
                            Color 3: Print "HOME RUNS ALLOWED";: Color 7: Print Tab(23); Using "###"; PLL!(13);: Print Tab(37); careerPL$(13);: Print Tab(56); PLYR%(13)
                            Color 3: Print "DOUBLE PLAYS";: Color 7: Print Tab(23); Using "###"; PLL!(14);: Print Tab(37); careerPL$(14);: Print Tab(56); PLYR%(14)

                            Color 15

                            Do
                                Locate 22: Print "VIEW AGAIN (Y/N)"
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "N"

                        Loop Until UCase$(I$) = "N"
                        '
                End Select

            End If 'Done checking ESC

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'      Merge_FirstYear Subroutine
'----------------------------------------
'...explanation...
Sub Merge_FirstYear ()

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared P0%(), careerP1%(), PRS%()

    'Dim T%(0 to 21), T1%(0 to 22)
    Shared T%(), T1%()

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared P0%(), careerP1%(), PRS%()

    Dim TMR%

    TMR% = 4175
    NB = -1
    NP = -1

    Do
        Color 12, 0
        Cls
        Locate 2, 5

        Color 10

        For J = 15 To 18:
            Locate J
            Print Space$(68)
        Next

        Color 11
        Locate 15, 5: Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; careerFile$

        careerFile$ = Left$(careerFile$, 8) + ".CAR"
        For XX = 1 To Len(careerFile)
            If Mid$(careerFile$, XX, 1) = " " Then Mid$(careerFile$, XX, 1) = "_"
        Next XX

        Locate , 5: Input "ENTER NAME OF STAT FILE TO ADD (EX. - WASHINGT.60)"; statFile$
        If Len(statFile$) > 11 Then
            statFile$ = Left$(statFile$, 8) + Right$(statFile$, 3)
        End If

        Locate , 5: Input "ENTER YEAR TO ADD (EX. - 1960)"; statYear

        Do
            Locate , 5: Print "ANY CHANGE (YN)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Where is VV used??
    VV = 1

    Do
        Locate , 5
        Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
        I$ = GetKeyPress$
        finalUpdate$ = UCase$(I$)
    Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

    If finalUpdate$ = "Y" Then finalupdate% = 1 Else finalupdate% = 0

    Locate 20, 5
    Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

    If Not _FileExists(diskPaths$(1) + statFile$) Then
        Cls
        Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
        Print
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$
    Else

        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, A$
        Input #1, W2%
        Input #1, L2%
        For I = 0 To 22: Input #1, careerB$(I): Input #1, careerB1%(I): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I
        For I = 0 To 21: Input #1, careerP$(I): Input #1, careerP1%(I): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I
        For I = 0 To 22: For I1 = 0 To 21: Input #1, careerB0%(I, I1): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I1: Next I
        For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I1: Next I
        Input #1, D0%
        Input #1, D1%
        For I = 0 To 21: Input #1, T%(I): Next I
        For I = 0 To 22: Input #1, T1%(I): Next I
        Close #1

        For I = 0 To 99

            N = 1

            If careerB$(I) = "END" Or careerB$(I) = "XXX" Or careerB$(I) = "" Then
                careerB$(I) = ""
            Else
                NB = NB + 1
            End If

            For J = 0 To 15:
                BRS%(N, I, J) = careerB0%(I, J)
            Next J

            BRS%(N, I, 16) = 1
            BRS%(N, I, 17) = statYear

        Next I

        For I = 0 To 79

            If careerP$(I) = "END" Or careerP$(I) = "XXX" Or careerP$(I) = "" Then
                careerP$(I) = ""
            Else
                NP = NP + 1
            End If

            For J = 0 To 14
                PRS%(N, I, J) = P0%(I, J)
            Next J

            PRS%(N, I, 15) = 1
            PRS%(N, I, 16) = statYear

        Next I

        If finalupdate% = 1 Then
            Call FinalMerge(careerFile$)
        End If

        Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ For Output As #1
        Write #1, NB, NP
        For I = 0 To 99: Write #1, careerB$(I): Next
        For I = 0 To 79: Write #1, careerP$(I): Next
        For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Write #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I: Next J
        For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Write #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I: Next J
        Close #1

    End If 'Done checking for file

End Sub

'----------------------------------------
'      Merge_AddYear Subroutine
'----------------------------------------
'...explanation...
Sub Merge_AddYear ()

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared P0%(), careerP1%(), PRS%()

    'Dim T%(0 to 21), T1%(0 to 22)
    Shared T%(), T1%()

    Dim BR$(100), PR$(80)

    Dim TMR%, TMR1%

    TMR% = 4175
    TMR1% = 6875

    Do
        Color 12, 0
        Cls
        Locate 2, 5

        For J = 15 To 18:
            Locate J: Print Space$(69)
        Next

        Color 11
        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE (EX. - MINNESOT)"; careerFile$

        careerFile$ = Left$(careerFile$, 8) + ".CAR"
        For XX = 1 To Len(careerFile$)
            If Mid$(careerFile$, XX, 1) = " " Then Mid$(careerFile$, XX, 1) = "_"
        Next XX

        Locate , 5: Input "ENTER NAME OF FILE TO ADD (EX. - MINNESOT.61)"; statFile$
        If Len(statFile$) > 11 Then
            statFile$ = Left$(statFile$, 8) + Right$(statFile$, 3)
        End If

        Locate , 5: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear

        Do
            Locate , 5: Print "ANY CHANGE (YN)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Where is VV used??
    VV = 0

    Do
        Locate , 5
        Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
        finalUpdate$ = UCase$(I$)
    Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

    If finalUpdate$ = "Y" Then finalupdate% = 1 Else finalupdate% = 0

    Color 15, 0
    Cls
    Locate 20, 5: Print "LOADING CAREER LEADER FILE . . . . PLEASE WAIT"

    If _FileExists(diskPaths$(1) + careerFile$) Then
        Open diskPaths$(1) + careerFile$ For Input As #1
        Input #1, NB, NP
        For I = 0 To 99: Input #1, careerB$(I): Next
        For I = 0 To 79: Input #1, careerP$(I): Next
        For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR1% = TMR1% - 1: Color 9: Print Using "####"; TMR1%: Next I: Next J
        For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR1% = TMR1% - 1: Color 9: Print Using "####"; TMR1%: Next I: Next J
        Close #1
    End If

    'Color 15, 0
    Locate 20, 5
    Print "LOADING STATS FILE . . . . PLEASE WAIT"
    If Not _FileExists(diskPaths$(1) + statFile$) Then
        Cls
        Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
        Print
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$
    Else
        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, A$
        Input #1, W2%
        Input #1, L2%
        For I = 0 To 22: Input #1, BR$(I): Input #1, careerB1%(I): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I
        For I = 0 To 21: Input #1, PR$(I): Input #1, careerP1%(I): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I
        For I = 0 To 22: For I1 = 0 To 21: Input #1, careerB0%(I, I1): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I1: Next I
        For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Locate 21, 35: TMR% = TMR% - 1: Color 9: Print Using "####"; TMR%: Next I1: Next I
        Input #1, D0%
        Input #1, D1%
        For I = 0 To 21: Input #1, T%(I): Next I
        For I = 0 To 22: Input #1, T1%(I): Next I
        Close #1

        For I = 0 To 22

            RR = 0
            CHK = 0

            For J = 0 To 99

                If UCase$(BR$(I)) = UCase$(careerB$(J)) Then

                    For W = 1 To 15
                        If BRS%(W, J, 17) = statYear Then
                            CHK = CHK + 1
                        End If
                    Next W

                    If CHK > 0 Then
                        N = BRS%(1, J, 16) + BRS%(2, J, 16) + BRS%(3, J, 16) + BRS%(4, J, 16) + BRS%(5, J, 16) + BRS%(6, J, 16) + BRS%(7, J, 16) + BRS%(8, J, 16) + BRS%(9, J, 16) + BRS%(10, J, 16) + BRS%(11, J, 16) + BRS%(12, J, 16) + BRS%(13, J, 16) + BRS%(14, J, 16) + BRS%(15, J, 16)
                    Else
                        N = BRS%(1, J, 16) + BRS%(2, J, 16) + BRS%(3, J, 16) + BRS%(4, J, 16) + BRS%(5, J, 16) + BRS%(6, J, 16) + BRS%(7, J, 16) + BRS%(8, J, 16) + BRS%(9, J, 16) + BRS%(10, J, 16) + BRS%(11, J, 16) + BRS%(12, J, 16) + BRS%(13, J, 16) + BRS%(14, J, 16) + BRS%(15, J, 16) + 1
                    End If

                    For K = 0 To 15: BRS%(N, J, K) = careerB0%(I, K): Next K

                    BRS%(N, J, 16) = 1
                    BRS%(N, J, 17) = statYear
                Else
                    GoTo 1423
                End If

            Next J

        Next I

        1423 '
        RR = RR + 1

        If RR = 100 Then
            If BR$(I) <> "END" And BR$(I) <> "XXX" And BR$(I) <> "" Then
                NB = NB + 1
                careerB$(NB) = BR$(I)
                N = 1
                For K = 0 To 15: BRS%(N, NB, K) = careerB0%(I, K): Next
                BRS%(N, NB, 16) = 1: BRS%(N, NB, 17) = statYear
            End If

        End If

        For I = 0 To 21

            RRP = 0
            CHK = 0

            For J = 0 To 79

                If UCase$(PR$(I)) = UCase$(careerP$(J)) Then

                    For W = 1 To 15
                        If PRS%(W, J, 16) = statYear Then
                            CHK = CHK + 1
                        End If
                    Next W

                    If CHK > 0 Then
                        1518 N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) + PRS%(15, J, 15)
                    Else
                        1519 N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) + PRS%(15, J, 15) + 1
                    End If

                    For K = 0 To 14: PRS%(N, J, K) = P0%(I, K): Next K

                    PRS%(N, J, 15) = 1
                    PRS%(N, J, 16) = statYear

                    1522 Exit For 'GoTo 1530
                Else
                    RRP = RRP + 1
                    If RRP = 80 Then

                        If PR$(I) <> "END" And PR$(I) <> "XXX" And PR$(I) <> "" Then
                            NP = NP + 1
                            careerP$(NP) = PR$(I)
                            N = 1
                            For K = 0 To 14: PRS%(N, NP, K) = P0%(I, K): Next
                            PRS%(N, NP, 15) = 1: PRS%(N, NP, 16) = statYear
                        End If

                    End If

                End If

            Next J

        Next I

        If finalupdate% = 1 Then
            Call FinalMerge(careerFile$)
        End If

        Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ For Output As #1
        Write #1, NB, NP
        For I = 0 To 99: Write #1, careerB$(I): Next
        For I = 0 To 79: Write #1, careerP$(I): Next
        For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Write #1, BRS%(J, I, I1): Next I1: Locate 21, 35: TMR1% = TMR1% - 1: Color 9: Print Using "####"; TMR1%: Next I: Next J
        For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Write #1, PRS%(J, I, I1): Next I1: Locate 21, 35: TMR1% = TMR1% - 1: Color 9: Print Using "####"; TMR1%: Next I: Next J
        Close #1

    End If 'Done checking if file exists

End Sub

'----------------------------------------
'           FinalMerge Subroutine
'----------------------------------------
'...explanation...
Sub FinalMerge (leaderFile$)

    Dim AVG!, SLG!, OBP!, EV!

    Shared BLL!(), PLL!()

    Shared BLYR%(), PLYR%()

    Shared careerB$(), careerP$()
    Shared BL$(), careerPL$()

    Shared BRS%(), PRS%()

    D = 0

    For J = 0 To 8
        D = D + 1
        X$ = Mid$(leaderFile$, D, 1)
        If X$ = "." Then finalupdate% = D - 1
    Next J

    If _FileExists(diskPaths$(2) + Left$(leaderFile$, finalupdate%) + ".CLD") Then
        Open diskPaths$(2) + Left$(leaderFile$, finalupdate%) + ".CLD" For Input As #1
        For I = 0 To 14: Input #1, BLYR%(I), BL$(I), BLL!(I): Next
        For I = 0 To 14: Input #1, PLYR%(I), careerPL$(I), PLL!(I): Next
        Close #1
    End If

    'Batters
    For I = 0 To 99

        If careerB$(I) <> "" And careerB$(I) <> "XXX" And careerB$(I) <> "END" Then

            For J = 1 To 15

                If BRS%(J, I, 17) <> 0 Then

                    If BRS%(J, I, 0) > BLL!(0) Then
                        BLL!(0) = BRS%(J, I, 0): BL$(0) = careerB$(I): BLYR%(0) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 2) > BLL!(1) Then
                        BLL!(1) = BRS%(J, I, 2): BL$(1) = careerB$(I): BLYR%(1) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 1) > BLL!(2) Then
                        BLL!(2) = BRS%(J, I, 1): BL$(2) = careerB$(I): BLYR%(2) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 4) > BLL!(3) Then
                        BLL!(3) = BRS%(J, I, 4): BL$(3) = careerB$(I): BLYR%(3) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 5) > BLL!(4) Then
                        BLL!(4) = BRS%(J, I, 5): BL$(4) = careerB$(I): BLYR%(4) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 6) > BLL!(5) Then
                        BLL!(5) = BRS%(J, I, 6): BL$(5) = careerB$(I): BLYR%(5) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 3) > BLL!(6) Then
                        BLL!(6) = BRS%(J, I, 3): BL$(6) = careerB$(I): BLYR%(6) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 7) > BLL!(7) Then
                        BLL!(7) = BRS%(J, I, 7): BL$(7) = careerB$(I): BLYR%(7) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 8) > BLL!(8) Then
                        BLL!(8) = BRS%(J, I, 8): BL$(8) = careerB$(I): BLYR%(8) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 9) > BLL!(9) Then
                        BLL!(9) = BRS%(J, I, 9): BL$(9) = careerB$(I): BLYR%(9) = BRS%(J, I, 17)
                    Else
                        If BRS%(J, I, 10) > BLL!(10) Then
                            BLL!(10) = BRS%(J, I, 10): BL$(10) = careerB$(I): BLYR%(10) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 11) > BLL!(11) Then
                        BLL!(11) = BRS%(J, I, 11): BL$(11) = careerB$(I): BLYR%(11) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        AVG! = BRS%(J, I, 1) / BRS%(J, I, 0)

                        If AVG! > BLL!(12) Then
                            BLL!(12) = AVG!: BL$(12) = careerB$(I): BLYR%(12) = BRS%(J, I, 17)
                        End If

                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        SLG! = (BRS%(J, I, 1) + BRS%(J, I, 4) + BRS%(J, I, 5) * 2 + BRS%(J, I, 6) * 3) / BRS%(J, I, 0)
                        If SLG! > BLL!(13) Then
                            BLL!(13) = SLG!: BL$(13) = careerB$(I): BLYR%(13) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then

                        OBP! = (BRS%(J, I, 1) + BRS%(J, I, 7)) / (BRS%(J, I, 7) + BRS%(J, I, 0))

                        test! = BLL!(14)
                        If OBP! > BLL!(14) Then
                            BLL!(14) = OBP!: BL$(14) = careerB$(I): BLYR%(14) = BRS%(J, I, 17)
                        End If

                    End If

                End If 'Done checking for non-zero value

            Next J

        End If 'Done checking for terminating characters

    Next I

    'Pitchers
    For I = 0 To 79

        If careerP$(I) <> "" And careerP$(I) <> "XXX" And careerP$(I) <> "END" Then

            For J = 1 To 15

                If PRS%(J, I, 16) <> 0 Then

                    If PRS%(J, I, 6) > PLL!(0) Then
                        PLL!(0) = PRS%(J, I, 6): careerPL$(0) = careerP$(I): PLYR%(0) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 7) > PLL!(1) Then
                        PLL!(1) = PRS%(J, I, 7): careerPL$(1) = careerP$(I): PLYR%(1) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 12) > PLL!(2) Then
                        PLL!(2) = PRS%(J, I, 12): careerPL$(2) = careerP$(I): PLYR%(2) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > PLL!(3) Then
                        PLL!(3) = PRS%(J, I, 0) / 3: careerPL$(3) = careerP$(I): PLYR%(3) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 1) > PLL!(4) Then
                        PLL!(4) = PRS%(J, I, 1): careerPL$(4) = careerP$(I): PLYR%(4) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 4) > PLL!(5) Then
                        PLL!(5) = PRS%(J, I, 4): careerPL$(5) = careerP$(I): PLYR%(5) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 5) > PLL!(6) Then
                        PLL!(6) = PRS%(J, I, 5): careerPL$(6) = careerP$(I): PLYR%(6) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > 162 Then
                        EV! = (PRS%(J, I, 3) * 9) / (PRS%(J, I, 0) / 3)
                        If PLL!(7) = 0 Then PLL!(7) = 1000
                        If EV! < PLL!(7) Then
                            PLL!(7) = EV!: careerPL$(7) = careerP$(I): PLYR%(7) = PRS%(J, I, 16)
                        End If
                    End If

                    If PRS%(J, I, 8) > PLL!(8) Then
                        PLL!(8) = PRS%(J, I, 8): careerPL$(8) = careerP$(I): PLYR%(8) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 10) > PLL!(9) Then
                        PLL!(9) = PRS%(J, I, 10): careerPL$(9) = careerP$(I): PLYR%(9) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 2) > PLL!(10) Then
                        PLL!(10) = PRS%(J, I, 2): careerPL$(10) = careerP$(I): PLYR%(10) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 3) > PLL!(11) Then
                        PLL!(11) = PRS%(J, I, 3): careerPL$(11) = careerP$(I): PLYR%(11) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 11) > PLL!(12) Then
                        PLL!(12) = PRS%(J, I, 11): careerPL$(12) = careerP$(I): PLYR%(12) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 14) > PLL!(13) Then
                        PLL!(13) = PRS%(J, I, 14): careerPL$(13) = careerP$(I): PLYR%(13) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 13) > PLL!(14) Then
                        PLL!(14) = PRS%(J, I, 13): careerPL$(14) = careerP$(I): PLYR%(14) = PRS%(J, I, 16)
                    End If

                End If 'Done checking value of 0

            Next J

        End If 'Done checking for bogus values

    Next I

    D = 0

    For J = 0 To 8
        D = D + 1
        X$ = Mid$(leaderFile$, D, 1)
        If X$ = "." Then finalupdate% = D - 1
    Next J

    Open diskPaths$(2) + Left$(leaderFile$, finalupdate%) + ".CLD" For Output As #1
    For I = 0 To 14: Write #1, BLYR%(I), BL$(I), BLL!(I): Next
    For I = 0 To 14: Write #1, PLYR%(I), careerPL$(I), PLL!(I): Next
    Close #1

End Sub
