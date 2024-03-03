'----------------------------------------
'          CAREER Subroutine
'----------------------------------------
'This routine gives the user a menu from 
'which they can choose several options
'for managing career files
Sub CAREER ()

    _Title "Full Count Baseball - Career Stats"

    ReDim menuChoices$(1 To 7)

    Do

        Color 15, 0
        Cls

        Call TITLE_CAREER

        menuChoices$(1) = "CREATE AND MERGE INITIAL STAT FILE"
        menuChoices$(2) = "MERGE / UPDATE CAREER STATS       "
        menuChoices$(3) = "VIEW / PRINT CAREER STATS         "
        menuChoices$(4) = "EDIT CAREER STATS                 "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "UPDATE / VIEW TEAM CAREER LEADERS "
        menuChoices$(7) = "RETURN TO MAIN MENU               "
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call CreateCareerFile

                Case 2:
                    Call MergeCareerFiles

                Case 3:
                    Call CARVIEW

                Case 4:
                    Call CAREDIT

                Case 5:
                    Call ViewPrintCareerRecords

                Case 6:
                    Call CARLDR

            End Select

        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'       CreateCareerFile Subroutine
'----------------------------------------
'This routine assists the user with the
'creation of a team's first career file.
'This naturally means it will guide them 
'through also adding the first year's
'stats to the file.
Sub CreateCareerFile ()

    Do

        Cls
        Color 14

        Locate 5, 27: Print " - - - C A U T I O N - - -"

        Color 15
        Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
        Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
        Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
        Locate , 18: Print "            USE OPTION #2."
        Print
        Print
        Print

        Color 12
        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Call MergeFirstYear

    End If 'Done confirming user continue

End Sub


'----------------------------------------
'        MergeFirstYear Subroutine
'----------------------------------------
'This routine actually prompts the user for 
'parameters to be used for merging the first
'year's worth of stats to the career file
Sub MergeFirstYear ()

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared careerP0%(), careerP1%(), PRS%()

    Shared statT0%(), statT1%()

    Dim TMR

    NB = -1
    NP = -1

    Do

        Cls

        For J = 15 To 18:
            Locate J
            Print Space$(68)
        Next

        Color 11
        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE "; careerFile$

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            statFile$ = nameStatFile$(team$)

            Do

                Color 15, 0
                Cls

                Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
                Locate 9, 25: Print "ANY CHANGE? (Y/N)"

                J$ = GetKeyPress$

            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N" Or J$ = Chr$(27)

            If UCase$(J$) = "N" Then

                VV = 1

                Do
                    Locate , 5
                    Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
                    K$ = GetKeyPress$
                Loop Until UCase$(K$) = "Y" Or UCase$(K$) = "N" Or K$ = Chr$(27)

                If UCase$(K$) = "Y" Then doPostLeaders = 1 Else doPostLeaders = 0

                If Not _FileExists(diskPaths$(1) + statFile$) Then

                    MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Locate 20, 5
                    Color 15
                    Print "MERGING AND UPDATING CAREER STATS . . . . PLEASE WAIT"

                    TMR = 4175

                    Open diskPaths$(1) + statFile$ For Input As #1

                    Input #1, team$
                    Input #1, W2%
                    Input #1, L2%

                    For I = 0 To 22:
                        Input #1, careerB$(I)
                        Input #1, careerB1%(I)
                        Color 9
                        Locate 21, 38
                        TMR = TMR - 1
                        Print Using "####"; TMR
                    Next I

                    For I = 0 To 21:
                        Input #1, careerP$(I)
                        Input #1, careerP1%(I)
                        Color 9
                        Locate 21, 38
                        TMR = TMR - 1
                        Print Using "####"; TMR
                    Next I

                    For I = 0 To 22:

                        For I1 = 0 To 21:
                            Input #1, careerB0%(I, I1)
                            Color 9
                            Locate 21, 38
                            TMR = TMR - 1
                            Print Using "####"; TMR
                        Next I1

                    Next I

                    For I = 0 To 21:

                        For I1 = 0 To 41:
                            Input #1, careerP0%(I, I1)
                            Color 9
                            Locate 21, 38
                            TMR = TMR - 1
                            Print Using "####"; TMR
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
                            PRS%(N, I, J) = careerP0%(I, J)
                        Next J

                        PRS%(N, I, 15) = 1
                        PRS%(N, I, 16) = statYear

                    Next I

                    If doPostLeaders = 1 Then Call UpdateCareerLeaders(careerFile$)

                    Locate 20, 5
                    Color 15: Print "MERGING AND UPDATING CAREER STATS . . . . PLEASE WAIT"

                    Open diskPaths$(1) + careerFile$ + ".CAR" For Output As #1

                    Write #1, NB, NP

                    For I = 0 To 99:
                        Write #1, careerB$(I)
                    Next

                    For I = 0 To 79:
                        Write #1, careerP$(I)
                    Next

                    For J = 1 To 15:

                        For I = 0 To 99:

                            For I1 = 0 To 17:
                                Write #1, BRS%(J, I, I1)
                            Next I1

                            Color 9
                            Locate 21, 38
                            TMR = TMR - 1
                            Print Using "####"; TMR

                        Next I

                    Next J

                    For J = 1 To 15:

                        For I = 0 To 79:

                            For I1 = 0 To 16:
                                Write #1, PRS%(J, I, I1)
                            Next I1

                            Color 9
                            Locate 21, 38
                            TMR = TMR - 1
                            Print Using "####"; TMR

                        Next I

                    Next J

                    Close #1

                End If 'Done checking for stat file

            End If 'Done checking for changes

        End If 'Done checking if team was chosen

    Loop Until UCase$(J$) = "N"

End Sub


'----------------------------------------
'    ViewPrintCareerRecords Subroutine
'----------------------------------------
'This routine loads in leader records from
'the .CLD file (which were compiled from  
'the career file). It then gives the user
'an option to View or Print results, AND
'formats the results for them as directed.
Sub ViewPrintCareerRecords ()

    Shared BLL!(), PLL!()
    Shared BLYR%(), PLYR%()

    Shared careerBL$(), careerPL$()

    recordFile$ = ChooseFile$(diskPaths$(2) + "*.CLD", "CAREER RECORD", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + recordFile$ + ".CLD" For Input As #1

        For I = 0 To 14: 
            Input #1, BLYR%(I), careerBL$(I), BLL!(I)
        Next

        For I = 0 To 14: 
            Input #1, PLYR%(I), careerPL$(I), PLL!(I)
        Next

        Close #1

        Do
            Color 15, 0
            Cls

            Do
                Locate 15, 10
                Color 12: Print "V";: Color 7: Print "iew or ";
                Color 12: Print "P";: Color 7: Print "rint"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V" Or I$ = Chr$(27)

            Select Case UCase$(I$)

                Case "P":

                    Color 0, 15

                    LPrint "CATEGORY           RECORD           PLAYER              YEAR"
                    LPrint
                    LPrint "AT BATS";: LPrint Tab(23); Using "###"; BLL!(0);: LPrint Tab(37); careerBL$(0);: LPrint Tab(56); BLYR%(0)
                    LPrint "RUNS SCORED";: LPrint Tab(23); Using "###"; BLL!(1);: LPrint Tab(37); careerBL$(1);: LPrint Tab(56); BLYR%(1)
                    LPrint "HITS";: LPrint Tab(23); Using "###"; BLL!(2);: LPrint Tab(37); careerBL$(2);: LPrint Tab(56); BLYR%(2)
                    LPrint "DOUBLES";: LPrint Tab(23); Using "###"; BLL!(3);: LPrint Tab(37); careerBL$(3);: LPrint Tab(56); BLYR%(3)
                    LPrint "TRIPLES";: LPrint Tab(23); Using "###"; BLL!(4);: LPrint Tab(37); careerBL$(4);: LPrint Tab(56); BLYR%(4)
                    LPrint "HOME RUNS";: LPrint Tab(23); Using "###"; BLL!(5);: LPrint Tab(37); careerBL$(5);: LPrint Tab(56); BLYR%(5)
                    LPrint "RUNS BATTED IN";: LPrint Tab(23); Using "###"; BLL!(6);: LPrint Tab(37); careerBL$(6);: LPrint Tab(56); BLYR%(6)
                    LPrint "WALKS";: LPrint Tab(23); Using "###"; BLL!(7);: LPrint Tab(37); careerBL$(7);: LPrint Tab(56); BLYR%(7)
                    LPrint "STRIKEOUTS";: LPrint Tab(23); Using "###"; BLL!(8);: LPrint Tab(37); careerBL$(8);: LPrint Tab(56); BLYR%(8)
                    LPrint "STEALS";: LPrint Tab(23); Using "###"; BLL!(9);: LPrint Tab(37); careerBL$(9);: LPrint Tab(56); BLYR%(9)
                    LPrint "CAUGHT STEALING";: LPrint Tab(23); Using "###"; BLL!(10);: LPrint Tab(37); careerBL$(10);: LPrint Tab(56); BLYR%(10)
                    LPrint "ERRORS";: LPrint Tab(23); Using "###"; BLL!(11);: LPrint Tab(37); careerBL$(11);: LPrint Tab(56); BLYR%(11)

                    LPrint "BATTING AVERAGE";

                    If BLL!(12) > 0 Then
                        LPrint Tab(22); Using ".###"; BLL!(12);: LPrint Tab(37); careerBL$(12);: LPrint Tab(56); BLYR%(12)
                    Else
                        LPrint Tab(22); " ---"
                    End If

                    LPrint "SLUGGING PCT.";
                    If BLL!(13) > 0 Then
                        LPrint Tab(22); Using "#.###"; BLL!(13);: LPrint Tab(37); careerBL$(13);: LPrint Tab(56); BLYR%(13)
                    Else
                        LPrint Tab(22); " ---"
                    End If

                    LPrint "ON BASE PCT.";
                    If BLL!(14) > 0 Then
                        LPrint Tab(22); Using "#.###"; BLL!(14);: LPrint Tab(37); careerBL$(14);: LPrint Tab(56); BLYR%(14)
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
                        Color 15, 0
                        Cls

                        Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"
                        Print

                        Color 3: Print "AT BATS";: Color 7: Print Tab(23); Using "###"; BLL!(0);: Print Tab(37); careerBL$(0);: Print Tab(56); BLYR%(0)
                        Color 3: Print "RUNS SCORED";: Color 7: Print Tab(23); Using "###"; BLL!(1);: Print Tab(37); careerBL$(1);: Print Tab(56); BLYR%(1)
                        Color 3: Print "HITS";: Color 7: Print Tab(23); Using "###"; BLL!(2);: Print Tab(37); careerBL$(2);: Print Tab(56); BLYR%(2)
                        Color 3: Print "DOUBLES";: Color 7: Print Tab(23); Using "###"; BLL!(3);: Print Tab(37); careerBL$(3);: Print Tab(56); BLYR%(3)
                        Color 3: Print "TRIPLES";: Color 7: Print Tab(23); Using "###"; BLL!(4);: Print Tab(37); careerBL$(4);: Print Tab(56); BLYR%(4)
                        Color 3: Print "HOME RUNS";: Color 7: Print Tab(23); Using "###"; BLL!(5);: Print Tab(37); careerBL$(5);: Print Tab(56); BLYR%(5)
                        Color 3: Print "RUNS BATTED IN";: Color 7: Print Tab(23); Using "###"; BLL!(6);: Print Tab(37); careerBL$(6);: Print Tab(56); BLYR%(6)
                        Color 3: Print "WALKS";: Color 7: Print Tab(23); Using "###"; BLL!(7);: Print Tab(37); careerBL$(7);: Print Tab(56); BLYR%(7)
                        Color 3: Print "STRIKEOUTS";: Color 7: Print Tab(23); Using "###"; BLL!(8);: Print Tab(37); careerBL$(8);: Print Tab(56); BLYR%(8)
                        Color 3: Print "STEALS";: Color 7: Print Tab(23); Using "###"; BLL!(9);: Print Tab(37); careerBL$(9);: Print Tab(56); BLYR%(9)
                        Color 3: Print "CAUGHT STEALING";: Color 7: Print Tab(23); Using "###"; BLL!(10);: Print Tab(37); careerBL$(10);: Print Tab(56); BLYR%(10)
                        Color 3: Print "ERRORS";: Color 7: Print Tab(23); Using "###"; BLL!(11);: Print Tab(37); careerBL$(11);: Print Tab(56); BLYR%(11)
                        Color 3: Print "BATTING AVERAGE";: Color 7

                        If BLL!(12) > 0 Then
                            Print Tab(22); Using ".###"; BLL!(12);: Print Tab(37); careerBL$(12);: Print Tab(56); BLYR%(12)
                        Else
                            Print Tab(22); " ---"
                        End If

                        Color 3: Print "SLUGGING PCT.";: Color 7:
                        If BLL!(13) > 0 Then
                            Print Tab(22); Using "#.###"; BLL!(13);: Print Tab(37); careerBL$(13);: Print Tab(56); BLYR%(13)
                        Else
                            Print Tab(22); " ---"
                        End If

                        Color 3: Print "ON BASE PCT.";: Color 7:
                        If BLL!(13) > 0 Then
                            Print Tab(22); Using "#.###"; BLL!(14);: Print Tab(37); careerBL$(14);: Print Tab(56); BLYR%(14)
                        Else
                            Print Tab(22); " ---"
                        End If

                        Color 15
                        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
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
                            Locate 22: Print "VIEW AGAIN? (Y/N)"
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

                    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

            End Select

        Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P" Or I$ = Chr$(27)

    End If 'Done checking ESC

End Sub


'----------------------------------------
'       MergeCareerFiles Subroutine
'----------------------------------------
'This routine assists thet user with the
'addition of career stats from any extra 
'season years that have been compiled.
Sub MergeCareerFiles ()

    _Title "Full Count Baseball - Update Career Files"

    Do
        Color 14, 0
        Cls

        Locate 5, 27
        Print "- - - C A U T I O N - - -"

        Color 15
        Locate 8, 1

        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
        Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
        Print
        Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
        Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
        Print
        Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
        Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
        Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
        Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
        Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
        Print "MERGING, OR YOU COULD CHANGE HIM TO 'B.WILLIAMS' IN THE CAREER FILE"
        Print
        Print

        Color 12
        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not (_FileExists(diskPaths$(1) + statFile$)) Then

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                Call MergeAddYear(careerFile$, statFile$)

            End If

        End If

    End If


End Sub


'----------------------------------------
'         MergeAddYear Subroutine
'----------------------------------------
'This routine actually prompts the user
'for parameters for the team / year to
'be updated. It then loads the existing 
'career stats, adds the team's stats to 
'them, then overwrites the career file 
'with the new values. 
Sub MergeAddYear (selectedFile$, statFile$)

    Shared careerB$(), careerP$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared careerP0%(), careerP1%(), PRS%()

    'Dim statT0%(0 to 21), statT1%(0 to 22)
    Shared statT0%(), statT1%()

    Dim BR$(100), PR$(80)

    Dim TMR1%, TMR2

    TMR1% = 4175
    TMR2 = 6875

    Color 15, 0
    Cls

    VV = 0

    Do
        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
        Locate 9, 25: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        Do
            Locate , 5
            Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then doPostLeaders = 1 Else doPostLeaders = 0

        Locate 20, 5
        Print "LOADING CAREER STATS . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Input As #1

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

                Locate 21, 35: Color 9
                TMR2 = TMR2 - 1
                Print Using "####"; TMR2

            Next I

        Next J

        For J = 1 To 15:

            For I = 0 To 79:

                For I1 = 0 To 16:
                    Input #1, PRS%(J, I, I1)
                Next I1

                Locate 21, 35: Color 9
                TMR2 = TMR2 - 1
                Print Using "####"; TMR2

            Next I

        Next J

        Close #1

        Locate 20, 5
        Print "LOADING STATS FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, team$
        Input #1, W2%
        Input #1, L2%

        For I = 0 To 22:
            Input #1, BR$(I)
            Input #1, careerB1%(I)
            Color 9: Locate 21, 35
            TMR1% = TMR1% - 1
            Print Using "####"; TMR1%
        Next I

        For I = 0 To 21:
            Input #1, PR$(I)
            Input #1, careerP1%(I)
            Color 9: Locate 21, 35
            TMR1% = TMR1% - 1
            Print Using "####"; TMR1%
        Next I

        For I = 0 To 22:
            For I1 = 0 To 21:
                Input #1, careerB0%(I, I1)
                Locate 21, 35
                Color 9
                TMR1% = TMR1% - 1: Print Using "####"; TMR1%
            Next I1
        Next I

        For I = 0 To 21:
            For I1 = 0 To 41:
                Input #1, careerP0%(I, I1)
                Color 9: Locate 21, 35
                TMR1% = TMR1% - 1
                Print Using "####"; TMR1%
            Next I1
        Next I

        Input #1, D0%, D1%

        For I = 0 To 21:
            Input #1, statT0%(I)
        Next I

        For I = 0 To 22:
            Input #1, statT1%(I)
        Next I

        Close #1

        For I = 0 To 22

            RR = 0
            CHK = 0
            doneLoop% = 0

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
                    doneLoop% = 1
                End If

                If doneLoop% = 1 Then Exit For

            Next J

            If doneLoop% = 1 Then Exit For

        Next I

        RR = RR + 1

        If RR = 100 Then
            If BR$(I) <> "END" And BR$(I) <> "XXX" And BR$(I) <> "" Then
                NB = NB + 1
                careerB$(NB) = BR$(I)
                N = 1

                For K = 0 To 15:
                    BRS%(N, NB, K) = careerB0%(I, K)
                Next

                BRS%(N, NB, 16) = 1
                BRS%(N, NB, 17) = statYear

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
                        N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) + PRS%(15, J, 15)
                    Else
                        N = PRS%(1, J, 15) + PRS%(2, J, 15) + PRS%(3, J, 15) + PRS%(4, J, 15) + PRS%(5, J, 15) + PRS%(6, J, 15) + PRS%(7, J, 15) + PRS%(8, J, 15) + PRS%(9, J, 15) + PRS%(10, J, 15) + PRS%(11, J, 15) + PRS%(12, J, 15) + PRS%(13, J, 15) + PRS%(14, J, 15) + PRS%(15, J, 15) + 1
                    End If

                    For K = 0 To 14:
                        PRS%(N, J, K) = careerP0%(I, K)
                    Next K

                    PRS%(N, J, 15) = 1
                    PRS%(N, J, 16) = statYear

                    Exit For 'GoTo 1530

                Else

                    RRP = RRP + 1

                    If RRP = 80 Then

                        If PR$(I) <> "END" And PR$(I) <> "XXX" And PR$(I) <> "" Then
                            NP = NP + 1
                            careerP$(NP) = PR$(I)
                            N = 1

                            For K = 0 To 14:
                                PRS%(N, NP, K) = careerP0%(I, K)
                            Next

                            PRS%(N, NP, 15) = 1
                            PRS%(N, NP, 16) = statYear
                        End If

                    End If

                End If

            Next J

        Next I

        If doPostLeaders = 1 Then Call UpdateCareerLeaders(selectedFile$)

        Locate 20, 5
        Print "MERGING CAREER STATS . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Output As #1

        Write #1, NB, NP

        For I = 0 To 99: Write #1, careerB$(I): Next

        For I = 0 To 79: Write #1, careerP$(I): Next

        For J = 1 To 15:

            For I = 0 To 99:

                For I1 = O To 17:
                    Write #1, BRS%(J, I, I1)
                Next I1

                Color 9: Locate 21, 35
                TMR2 = TMR2 - 1
                Print Using "####"; TMR2

            Next I

        Next J

        For J = 1 To 15:

            For I = 0 To 79:

                For I1 = 0 To 16:
                    Write #1, PRS%(J, I, I1)
                Next I1

                Color 9: Locate 21, 35
                TMR2 = TMR2 - 1
                Print Using "####"; TMR2

            Next I

        Next J

        Close #1

    End If ' Done checking for year to add

End Sub

'----------------------------------------
'     UpdateCareerLeaders Subroutine
'----------------------------------------
'This routine reads in any existing career 
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerLeaders (recordFile$)

    Dim AVG!, SLG!, OBP!, EV!

    Shared BLL!(), PLL!()

    Shared BLYR%(), PLYR%()

    Shared careerB$(), careerP$()
    Shared careerBL$(), careerPL$()

    Shared BRS%(), PRS%()

    If _FileExists(diskPaths$(2) + recordFile$ + ".CLD") Then

        Open diskPaths$(2) + recordFile$ + ".CLD" For Input As #1

        For I = 0 To 14:
            Input #1, BLYR%(I), careerBL$(I), BLL!(I)
        Next

        For I = 0 To 14:
            Input #1, PLYR%(I), careerPL$(I), PLL!(I)
        Next

        Close #1

    End If

    'Batters
    For I = 0 To 99

        If careerB$(I) <> "" And careerB$(I) <> "XXX" And careerB$(I) <> "END" Then

            For J = 1 To 15

                If BRS%(J, I, 17) <> 0 Then

                    If BRS%(J, I, 0) > BLL!(0) Then
                        BLL!(0) = BRS%(J, I, 0)
                        careerBL$(0) = careerB$(I)
                        BLYR%(0) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 2) > BLL!(1) Then
                        BLL!(1) = BRS%(J, I, 2)
                        careerBL$(1) = careerB$(I)
                        BLYR%(1) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 1) > BLL!(2) Then
                        BLL!(2) = BRS%(J, I, 1)
                        careerBL$(2) = careerB$(I)
                        BLYR%(2) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 4) > BLL!(3) Then
                        BLL!(3) = BRS%(J, I, 4)
                        careerBL$(3) = careerB$(I)
                        BLYR%(3) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 5) > BLL!(4) Then
                        BLL!(4) = BRS%(J, I, 5)
                        careerBL$(4) = careerB$(I)
                        BLYR%(4) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 6) > BLL!(5) Then
                        BLL!(5) = BRS%(J, I, 6)
                        careerBL$(5) = careerB$(I)
                        BLYR%(5) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 3) > BLL!(6) Then
                        BLL!(6) = BRS%(J, I, 3): careerBL$(6) = careerB$(I): BLYR%(6) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 7) > BLL!(7) Then
                        BLL!(7) = BRS%(J, I, 7)
                        careerBL$(7) = careerB$(I)
                        BLYR%(7) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 8) > BLL!(8) Then
                        BLL!(8) = BRS%(J, I, 8)
                        careerBL$(8) = careerB$(I)
                        BLYR%(8) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 9) > BLL!(9) Then
                        BLL!(9) = BRS%(J, I, 9)
                        careerBL$(9) = careerB$(I)
                        BLYR%(9) = BRS%(J, I, 17)
                    Else
                        If BRS%(J, I, 10) > BLL!(10) Then
                            BLL!(10) = BRS%(J, I, 10)
                            careerBL$(10) = careerB$(I)
                            BLYR%(10) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 11) > BLL!(11) Then
                        BLL!(11) = BRS%(J, I, 11)
                        careerBL$(11) = careerB$(I)
                        BLYR%(11) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        AVG! = BRS%(J, I, 1) / BRS%(J, I, 0)

                        If AVG! > BLL!(12) Then
                            BLL!(12) = AVG!
                            careerBL$(12) = careerB$(I)
                            BLYR%(12) = BRS%(J, I, 17)
                        End If

                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        SLG! = (BRS%(J, I, 1) + BRS%(J, I, 4) + BRS%(J, I, 5) * 2 + BRS%(J, I, 6) * 3) / BRS%(J, I, 0)
                        If SLG! > BLL!(13) Then
                            BLL!(13) = SLG!
                            careerBL$(13) = careerB$(I)
                            BLYR%(13) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then

                        OBP! = (BRS%(J, I, 1) + BRS%(J, I, 7)) / (BRS%(J, I, 7) + BRS%(J, I, 0))

                        test! = BLL!(14)
                        If OBP! > BLL!(14) Then
                            BLL!(14) = OBP!
                            careerBL$(14) = careerB$(I)
                            BLYR%(14) = BRS%(J, I, 17)
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
                        PLL!(0) = PRS%(J, I, 6)
                        careerPL$(0) = careerP$(I)
                        PLYR%(0) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 7) > PLL!(1) Then
                        PLL!(1) = PRS%(J, I, 7)
                        careerPL$(1) = careerP$(I)
                        PLYR%(1) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 12) > PLL!(2) Then
                        PLL!(2) = PRS%(J, I, 12)
                        careerPL$(2) = careerP$(I)
                        PLYR%(2) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > PLL!(3) Then
                        PLL!(3) = PRS%(J, I, 0) / 3
                        careerPL$(3) = careerP$(I)
                        PLYR%(3) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 1) > PLL!(4) Then
                        PLL!(4) = PRS%(J, I, 1)
                        careerPL$(4) = careerP$(I)
                        PLYR%(4) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 4) > PLL!(5) Then
                        PLL!(5) = PRS%(J, I, 4)
                        careerPL$(5) = careerP$(I)
                        PLYR%(5) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 5) > PLL!(6) Then
                        PLL!(6) = PRS%(J, I, 5)
                        careerPL$(6) = careerP$(I)
                        PLYR%(6) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > 162 Then
                        EV! = (PRS%(J, I, 3) * 9) / (PRS%(J, I, 0) / 3)
                        If PLL!(7) = 0 Then PLL!(7) = 1000
                        If EV! < PLL!(7) Then
                            PLL!(7) = EV!
                            careerPL$(7) = careerP$(I)
                            PLYR%(7) = PRS%(J, I, 16)
                        End If
                    End If

                    If PRS%(J, I, 8) > PLL!(8) Then
                        PLL!(8) = PRS%(J, I, 8)
                        careerPL$(8) = careerP$(I)
                        PLYR%(8) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 10) > PLL!(9) Then
                        PLL!(9) = PRS%(J, I, 10)
                        careerPL$(9) = careerP$(I)
                        PLYR%(9) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 2) > PLL!(10) Then
                        PLL!(10) = PRS%(J, I, 2)
                        careerPL$(10) = careerP$(I)
                        PLYR%(10) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 3) > PLL!(11) Then
                        PLL!(11) = PRS%(J, I, 3)
                        careerPL$(11) = careerP$(I)
                        PLYR%(11) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 11) > PLL!(12) Then
                        PLL!(12) = PRS%(J, I, 11)
                        careerPL$(12) = careerP$(I)
                        PLYR%(12) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 14) > PLL!(13) Then
                        PLL!(13) = PRS%(J, I, 14)
                        careerPL$(13) = careerP$(I)
                        PLYR%(13) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 13) > PLL!(14) Then
                        PLL!(14) = PRS%(J, I, 13)
                        careerPL$(14) = careerP$(I)
                        PLYR%(14) = PRS%(J, I, 16)
                    End If

                End If 'Done checking value of 0

            Next J

        End If 'Done checking for bogus values

    Next I

    Open diskPaths$(2) + recordFile$ + ".CLD" For Output As #1

    For I = 0 To 14:
        Write #1, BLYR%(I), careerBL$(I), BLL!(I)
    Next

    For I = 0 To 14:
        Write #1, PLYR%(I), careerPL$(I), PLL!(I)
    Next

    Close #1

End Sub

