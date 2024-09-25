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

        menuChoices$(1) = "CREATE INITIAL CAREER STATS FILE  "
        menuChoices$(2) = "ADD CAREER YEAR TO STATS          "
        menuChoices$(3) = "REMOVE PLAYERS FROM CAREER STATS  "
        menuChoices$(4) = "VIEW / PRINT CAREER STATS         "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "UPDATE / VIEW TEAM CAREER LEADERS "
        menuChoices$(7) = "RETURN TO MAIN MENU               "
        SELECTION% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call CreateCareerFile

                Case 2:
                    Call MergeCareerFiles

                Case 3:
                    Call CAREDIT

                Case 4:
                    Call CARVIEW

                Case 5:
                    Call ReviewCareerRecords

                Case 6:
                    Call CARLDR

            End Select

        End If

    Loop Until SELECTION% = 7 Or ExitCode% = 27

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

    End If 'Done checking if user is ready

End Sub


'----------------------------------------
'        MergeFirstYear Subroutine
'----------------------------------------
'This routine actually prompts the user for 
'parameters to be used for merging the first
'year's worth of stats to the career file
Sub MergeFirstYear ()

    Shared NB, NP

    Shared careerBatters$(), careerPitchers$()

    Shared careerB0%(), careerB1%(), BRS%()
    Shared careerP0%(), careerP1%(), PRS%()

    Shared statT0%(), statT1%()

    Dim TMR

    NB = -1
    NP = -1

    Do

        Color 11, 0
        Cls

        For J = 15 To 18:
            Locate J
            Print Space$(68)
        Next

        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE "; careerFile$

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            statFile$ = nameStatFile$(team$)

            Do

                Color 15, 0
                Cls

                Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
                Locate 9, 22: Print "ANY CHANGE? (Y/N)"

                J$ = GetKeyPress$

            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N" Or J$ = Chr$(27)

            If UCase$(J$) = "N" Then

                Do
                    Locate , 5
                    Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
                    K$ = GetKeyPress$
                Loop Until UCase$(K$) = "Y" Or UCase$(K$) = "N" Or K$ = Chr$(27)

                If UCase$(K$) = "Y" Then 
                    doPostLeaders = 1 
                Else 
                    doPostLeaders = 0
                End If

                If Not _FileExists(diskPaths$(1) + statFile$) Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
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

                    TMR = 3 + (23*2) + (22*2) + (23*22) + (22*42)

                    Open diskPaths$(1) + statFile$ For Input As #1

                    Input #1, team$
                    Input #1, teamWins
                    Input #1, teamLosses

                    For I = 0 To 22:
                        Input #1, careerBatters$(I)
                        Input #1, careerB1%(I)
                        Color 9
                        Locate 21, 38
                        TMR = TMR - 1
                        Print Using "####"; TMR
                    Next I

                    For I = 0 To 21:
                        Input #1, careerPitchers$(I)
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

                    Input #1, D0, D1

                    For I = 0 To 21:
                        Input #1, statT0%(I)
                    Next I

                    For I = 0 To 22:
                        Input #1, statT1%(I)
                    Next I

                    Close #1

                    For I = 0 To 99

                        N = 1

                        If careerBatters$(I) = "END" Or careerBatters$(I) = "XXX" Or careerBatters$(I) = "" Then
                            careerBatters$(I) = ""
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

                        If careerPitchers$(I) = "END" Or careerPitchers$(I) = "XXX" Or careerPitchers$(I) = "" Then
                            careerPitchers$(I) = ""
                        Else
                            NP = NP + 1
                        End If

                        For J = 0 To 14
                            PRS%(N, I, J) = careerP0%(I, J)
                        Next J

                        PRS%(N, I, 15) = 1
                        PRS%(N, I, 16) = statYear

                    Next I

                    If doPostLeaders = 1 Then Call UpdateCareerRecords(careerFile$)

                    Call SaveCareerFile (careerFile$)

                End If 'Done checking for stat file

            End If 'Done checking for changes

        End If 'Done checking if team was chosen

    Loop Until UCase$(J$) = "N"

End Sub


'----------------------------------------
'    ReviewCareerRecords Subroutine
'----------------------------------------
'This routine loads in leader records from
'the .CLD file (which were compiled from  
'the career file). It then gives the user
'an option to View or Print results, AND
'formats the results for them as directed.
Sub ReviewCareerRecords ()

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

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Career","*.txt","Text File")
                    If userFile$ <> "" Then
                        Call CareerRecords_Report(0, userFile$)
                    End If

                Case "P"
                    Call CareerRecords_Report(1, "CAREER")

                Case "S":

                    Do
                        Cls
                        Color 15, 0

                        Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"
                        Print

                        Color 3: Print "AT BATS";
                        Color 7: Print Tab(23); Using "###"; BLL!(0);: Print Tab(37); careerBL$(0);: Print Tab(56); BLYR%(0)
                        Color 3: Print "RUNS SCORED";
                        Color 7: Print Tab(23); Using "###"; BLL!(1);: Print Tab(37); careerBL$(1);: Print Tab(56); BLYR%(1)
                        Color 3: Print "HITS";
                        Color 7: Print Tab(23); Using "###"; BLL!(2);: Print Tab(37); careerBL$(2);: Print Tab(56); BLYR%(2)
                        Color 3: Print "DOUBLES";
                        Color 7: Print Tab(23); Using "###"; BLL!(3);: Print Tab(37); careerBL$(3);: Print Tab(56); BLYR%(3)
                        Color 3: Print "TRIPLES";
                        Color 7: Print Tab(23); Using "###"; BLL!(4);: Print Tab(37); careerBL$(4);: Print Tab(56); BLYR%(4)
                        Color 3: Print "HOME RUNS";
                        Color 7: Print Tab(23); Using "###"; BLL!(5);: Print Tab(37); careerBL$(5);: Print Tab(56); BLYR%(5)
                        Color 3: Print "RUNS BATTED IN";
                        Color 7: Print Tab(23); Using "###"; BLL!(6);: Print Tab(37); careerBL$(6);: Print Tab(56); BLYR%(6)
                        Color 3: Print "WALKS";
                        Color 7: Print Tab(23); Using "###"; BLL!(7);: Print Tab(37); careerBL$(7);: Print Tab(56); BLYR%(7)
                        Color 3: Print "STRIKEOUTS";
                        Color 7: Print Tab(23); Using "###"; BLL!(8);: Print Tab(37); careerBL$(8);: Print Tab(56); BLYR%(8)
                        Color 3: Print "STEALS";
                        Color 7: Print Tab(23); Using "###"; BLL!(9);: Print Tab(37); careerBL$(9);: Print Tab(56); BLYR%(9)
                        Color 3: Print "CAUGHT STEALING";
                        Color 7: Print Tab(23); Using "###"; BLL!(10);: Print Tab(37); careerBL$(10);: Print Tab(56); BLYR%(10)
                        Color 3: Print "ERRORS";
                        Color 7: Print Tab(23); Using "###"; BLL!(11);: Print Tab(37); careerBL$(11);: Print Tab(56); BLYR%(11)
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

                        Color 3: Print "WINS";
                        Color 7: Print Tab(23); Using "###"; PLL!(0);: Print Tab(37); careerPL$(0);: Print Tab(56); PLYR%(0)
                        Color 3: Print "LOSSES";
                        Color 7: Print Tab(23); Using "###"; PLL!(1);: Print Tab(37); careerPL$(1);: Print Tab(56); PLYR%(1)
                        Color 3: Print "SAVES";
                        Color 7: Print Tab(23); Using "###"; PLL!(2);: Print Tab(37); careerPL$(2);: Print Tab(56); PLYR%(2)
                        Color 3: Print "INNINGS PITCHED";
                        Color 7: Print Tab(23); Using "###"; PLL!(3);:

                        If PLL!(3) - (Int(PLL!(3) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PLL!(3) - (Int(PLL!(3) / 3) * 3);:

                        Print Tab(37); careerPL$(3);: Print Tab(56); PLYR%(3)
                        Color 3: Print "HITS ALLOWED";
                        Color 7: Print Tab(23); Using "###"; PLL!(4);: Print Tab(37); careerPL$(4);: Print Tab(56); PLYR%(4)
                        Color 3: Print "WALKS";
                        Color 7: Print Tab(23); Using "###"; PLL!(5);: Print Tab(37); careerPL$(5);: Print Tab(56); PLYR%(5)
                        Color 3: Print "STRIKOUTS";
                        Color 7: Print Tab(23); Using "###"; PLL!(6);: Print Tab(37); careerPL$(6);: Print Tab(56); PLYR%(6)

                        Color 3: Print "EARNED RUN AVERAGE";: Color 7
                        If PLL!(7) > 0 Then
                            Print Tab(22); Using "##.##"; PLL!(7);: Print Tab(37); careerPL$(7);: Print Tab(56); PLYR%(7)
                        Else
                            Print Tab(22); " ---"
                        End If

                        Color 3: Print "GAMES";
                        Color 7: Print Tab(23); Using "###"; PLL!(8);: Print Tab(37); careerPL$(8);: Print Tab(56); PLYR%(8)
                        Color 3: Print "COMPLETE GAMES";
                        Color 7: Print Tab(23); Using "###"; PLL!(9);: Print Tab(37); careerPL$(9);: Print Tab(56); PLYR%(9)
                        Color 3: Print "RUNS ALLOWED";
                        Color 7: Print Tab(23); Using "###"; PLL!(10);: Print Tab(37); careerPL$(10);: Print Tab(56); PLYR%(10)
                        Color 3: Print "EARNED RUNS ALLOWED";
                        Color 7: Print Tab(23); Using "###"; PLL!(11);: Print Tab(37); careerPL$(11);: Print Tab(56); PLYR%(11)
                        Color 3: Print "SHUTOUTS";
                        Color 7: Print Tab(23); Using "###"; PLL!(12);: Print Tab(37); careerPL$(12);: Print Tab(56); PLYR%(12)
                        Color 3: Print "HOME RUNS ALLOWED";
                        Color 7: Print Tab(23); Using "###"; PLL!(13);: Print Tab(37); careerPL$(13);: Print Tab(56); PLYR%(13)
                        Color 3: Print "DOUBLE PLAYS";
                        Color 7: Print Tab(23); Using "###"; PLL!(14);: Print Tab(37); careerPL$(14);: Print Tab(56); PLYR%(14)

                        Color 15

                        Do
                            Locate 22: Print "VIEW AGAIN? (Y/N)"
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "N" Or Ucase$(I$) = "Y" Or I$ = Chr$(27)

                    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

            End Select
            
        End If

    End If 'Done checking ESC

End Sub


'----------------------------------------
'    CareerRecords_Report Subroutine
'----------------------------------------
'This routine formats career records for
'output to either a file or printer, as
'specified by the user. All operations
'involve creating a file first, that can
'then be looped through if sending to 
'printer
Sub CareerRecords_Report (printDest, destFile$)

    Shared BLL!(), PLL!()
    Shared BLYR%(), PLYR%()

    Shared careerBL$(), careerPL$()

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    Print #1, "CATEGORY           RECORD           PLAYER              YEAR"
    Print #1,
    Print #1, "AT BATS";
    Print #1, Tab(23); Using "###"; BLL!(0);
    Print #1, Tab(37); careerBL$(0);
    Print #1, Tab(56); BLYR%(0)
    Print #1, "RUNS SCORED";
    Print #1, Tab(23); Using "###"; BLL!(1);
    Print #1, Tab(37); careerBL$(1);
    Print #1, Tab(56); BLYR%(1)
    Print #1, "HITS";
    Print #1, Tab(23); Using "###"; BLL!(2);
    Print #1, Tab(37); careerBL$(2);
    Print #1, Tab(56); BLYR%(2)
    Print #1, "DOUBLES";
    Print #1, Tab(23); Using "###"; BLL!(3);
    Print #1, Tab(37); careerBL$(3);
    Print #1, Tab(56); BLYR%(3)
    Print #1, "TRIPLES";
    Print #1, Tab(23); Using "###"; BLL!(4);
    Print #1, Tab(37); careerBL$(4);
    Print #1, Tab(56); BLYR%(4)
    Print #1, "HOME RUNS";
    Print #1, Tab(23); Using "###"; BLL!(5);
    Print #1, Tab(37); careerBL$(5);
    Print #1, Tab(56); BLYR%(5)
    Print #1, "RUNS BATTED IN";
    Print #1, Tab(23); Using "###"; BLL!(6);
    Print #1, Tab(37); careerBL$(6);
    Print #1, Tab(56); BLYR%(6)
    Print #1, "WALKS";
    Print #1, Tab(23); Using "###"; BLL!(7);
    Print #1, Tab(37); careerBL$(7);
    Print #1, Tab(56); BLYR%(7)
    Print #1, "STRIKEOUTS";
    Print #1, Tab(23); Using "###"; BLL!(8);
    Print #1, Tab(37); careerBL$(8);
    Print #1, Tab(56); BLYR%(8)
    Print #1, "STEALS";
    Print #1, Tab(23); Using "###"; BLL!(9);
    Print #1, Tab(37); careerBL$(9);
    Print #1, Tab(56); BLYR%(9)
    Print #1, "CAUGHT STEALING";
    Print #1, Tab(23); Using "###"; BLL!(10);
    Print #1, Tab(37); careerBL$(10);
    Print #1, Tab(56); BLYR%(10)
    Print #1, "ERRORS";
    Print #1, Tab(23); Using "###"; BLL!(11);
    Print #1, Tab(37); careerBL$(11);
    Print #1, Tab(56); BLYR%(11)

    Print #1, "BATTING AVERAGE";

    If BLL!(12) > 0 Then
        Print #1, Tab(22); Using ".###"; BLL!(12);
        Print #1, Tab(37); careerBL$(12);
        Print #1, Tab(56); BLYR%(12)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "SLUGGING PCT.";
    If BLL!(13) > 0 Then
        Print #1, Tab(22); Using "#.###"; BLL!(13);
        Print #1, Tab(37); careerBL$(13);
        Print #1, Tab(56); BLYR%(13)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "ON BASE PCT.";
    If BLL!(14) > 0 Then
        Print #1, Tab(22); Using "#.###"; BLL!(14);
        Print #1, Tab(37); careerBL$(14);
        Print #1, Tab(56); BLYR%(14)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "WINS";
    Print #1, Tab(23); Using "###"; PLL!(0);
    Print #1, Tab(37); careerPL$(0);
    Print #1, Tab(56); PLYR%(0)
    Print #1, "LOSSES";
    Print #1, Tab(23); Using "###"; PLL!(1);
    Print #1, Tab(37); careerPL$(1);
    Print #1, Tab(56); PLYR%(1)
    Print #1, "SAVES";
    Print #1, Tab(23); Using "###"; PLL!(2);
    Print #1, Tab(37); careerPL$(2);
    Print #1, Tab(56); PLYR%(2)

    Print #1, "INNINGS PITCHED";
    Print #1, Tab(23); Using "###"; PLL!(3);:

    If PLL!(3) - (Int(PLL!(3) / 3) * 3) > 0 Then 
        Print #1, ".";
        Print #1, Using "#"; PLL!(3) - (Int(PLL!(3) / 3) * 3);:
    End If

    Print #1, Tab(37); careerPL$(3);
    Print #1, Tab(56); PLYR%(3)

    Print #1, "HITS ALLOWED";
    Print #1, Tab(23); Using "###"; PLL!(4);
    Print #1, Tab(37); careerPL$(4);
    Print #1, Tab(56); PLYR%(4)
    Print #1, "WALKS";
    Print #1, Tab(23); Using "###"; PLL!(5);
    Print #1, Tab(37); careerPL$(5);
    Print #1, Tab(56); PLYR%(5)
    Print #1, "STRIKOUTS";
    Print #1, Tab(23); Using "###"; PLL!(6);
    Print #1, Tab(37); careerPL$(6);
    Print #1, Tab(56); PLYR%(6)

    Print #1, "EARNED RUN AVERAGE";:
    If PLL!(7) > 0 Then
        Print #1, Tab(22); Using "##.##"; PLL!(7);
        Print #1, Tab(37); careerPL$(7);
        Print #1, Tab(56); PLYR%(7)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "GAMES";
    Print #1, Tab(23); Using "###"; PLL!(8);
    Print #1, Tab(37); careerPL$(8);
    Print #1, Tab(56); PLYR%(8)
    Print #1, "COMPLETE GAMES";
    Print #1, Tab(23); Using "###"; PLL!(9);
    Print #1, Tab(37); careerPL$(9);
    Print #1, Tab(56); PLYR%(9)
    Print #1, "RUNS ALLOWED";
    Print #1, Tab(23); Using "###"; PLL!(10);
    Print #1, Tab(37); careerPL$(10);
    Print #1, Tab(56); PLYR%(10)
    Print #1, "EARNED RUNS ALLOWED";
    Print #1, Tab(23); Using "###"; PLL!(11);
    Print #1, Tab(37); careerPL$(11);
    Print #1, Tab(56); PLYR%(11)
    Print #1, "SHUTOUTS";
    Print #1, Tab(23); Using "###"; PLL!(12);
    Print #1, Tab(37); careerPL$(12);
    Print #1, Tab(56); PLYR%(12)
    Print #1, "HOME RUNS ALLOWED";
    Print #1, Tab(23); Using "###"; PLL!(13);
    Print #1, Tab(37); careerPL$(13);
    Print #1, Tab(56); PLYR%(13)
    Print #1, "DOUBLE PLAYS";
    Print #1, Tab(23); Using "###"; PLL!(14);
    Print #1, Tab(37); careerPL$(14);
    Print #1, Tab(56); PLYR%(14)

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

        Color 12: Locate , 25
        Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
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

    Color 15, 0
    Cls

    Do
        Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
        Locate 9, 22: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        Do
            Locate , 5
            Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then 
            doPostLeaders = 1 
        Else 
            doPostLeaders = 0
        End If

        Call AddCareerYear(statYear, doPostLeaders, selectedFile$, statFile$)

    End If ' Done checking for year to add

End Sub


'----------------------------------------
'       AddCareerYear Subroutine
'----------------------------------------
'This routine loads the existing career
'stats, adds the team's stats to them,
'then overwrites the career file with the
'new values. 
Sub AddCareerYear (statYear, finalUpdate, careerFile$, statFile$)

    Shared NB, NP

    Shared careerBatters$(), careerPitchers$()
    Dim BR$(100), PR$(80)

    Shared careerB0%(), careerB1%(), BRS%()
    Shared careerP0%(), careerP1%(), PRS%()

    Shared statT0%(), statT1%()

    NB = -1

    Call LoadCareerFile (careerFile$)

    Locate 20, 5
    Print "LOADING STATS FILE . . . . PLEASE WAIT"

    TMR1% = 3 + (23*2) + (22*2) + (23*22) + (22*42)

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, team$
    Input #1, teamWins
    Input #1, teamLosses

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

    Input #1, D0, D1

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

            If UCase$(BR$(I)) = UCase$(careerBatters$(J)) Then

                For W = 1 To MAX_CAREER_YEARS
                    If BRS%(W, J, 17) = statYear Then
                        CHK = CHK + 1
                    End If
                Next W

                For Z = 1 to MAX_CAREER_YEARS
                    N = N + BRS%(Z, J, 16)
                Next Z 

                If CHK = 0 Then
                    N = N + 1
                End If

                For K = 0 To 15: 
                    BRS%(N, J, K) = careerB0%(I, K)
                Next K

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
            careerBatters$(NB) = BR$(I)
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

            If UCase$(PR$(I)) = UCase$(careerPitchers$(J)) Then

                For W = 1 To MAX_CAREER_YEARS
                    If PRS%(W, J, 16) = statYear Then
                        CHK = CHK + 1
                    End If
                Next W

                For Z = 1 to MAX_CAREER_YEARS
                    N = N + PRS%(Z, J, 15)
                Next Z 

                If CHK = 0 Then
                    N = N + 1
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
                        careerPitchers$(NP) = PR$(I)
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

    If finalUpdate = 1 Then Call UpdateCareerRecords(careerFile$)

    Call SaveCareerFile (careerFile$)

End Sub


'----------------------------------------
'      LoadCareerFile Subroutine
'----------------------------------------
'This routine loads the existing career
'file data into arrays.
Sub LoadCareerFile (loadFile$)

    Shared NB, NP
    
    Shared careerBatters$(), careerPitchers$()

    Shared BRS%(), PRS%()

    Color 15, 0
    Cls

    Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

    TMR = 100 + 80 + (MAX_CAREER_YEARS*100) + (MAX_CAREER_YEARS*80)

    Open diskPaths$(1) + loadFile$ + ".CAR" For Input As #2

    Input #2, NB, NP

    For I = 0 To 99: 
        Input #2, careerBatters$(I)
    Next

    For I = 0 To 79: 
        Input #2, careerPitchers$(I)
    Next

    For J = 1 To MAX_CAREER_YEARS: 

        For I = 0 To 99: 

            For I1 = 0 To 17: 
                Input #2, BRS%(J, I, I1)
            Next I1

            Color 3
            Locate 21, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

    Next J

    For J = 1 To MAX_CAREER_YEARS: 

        For I = 0 To 79: 

            For I1 = 0 To 16: 
                Input #2, PRS%(J, I, I1)
            Next I1

            Color 3
            Locate 21, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

    Next J

    Close #2

End Sub


'----------------------------------------
'      SaveCareerFile Subroutine
'----------------------------------------
'This routine updates the existing career
'file with the modified stats values.
Sub SaveCareerFile (saveFile$)

    Shared NB, NP

    Shared careerBatters$(), careerPitchers$()

    Shared BRS%(), PRS%()

    Cls
    Color 15

    Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

    TMR = 100 + 80 + (MAX_CAREER_YEARS*100) + (MAX_CAREER_YEARS*80)

    Open diskPaths$(1) + saveFile$ + ".CAR" For Output As #3

    Write #3, NB, NP

    For I = 0 To 99: 
        Write #3, careerBatters$(I)
    Next

    For I = 0 To 79: 
        Write #3, careerPitchers$(I)
    Next

    For J = 1 To MAX_CAREER_YEARS: 

        For I = 0 To 99: 

            For I1 = 0 To 17: 
                Write #3, BRS%(J, I, I1)
            Next I1

            Color 3
            Locate 21, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

    Next J

    For J = 1 To MAX_CAREER_YEARS:

        For I = 0 To 79: 

            For I1 = 0 To 16: 
                Write #3, PRS%(J, I, I1)
            Next I1

            Color 3
            Locate 21, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

    Next J

    Close #3

End Sub


'----------------------------------------
'     UpdateCareerRecords Subroutine
'----------------------------------------
'This routine reads in any existing career 
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerRecords (recordFile$)

    Dim AVG!, SLG!, OBP!, EV!

    Shared BLL!(), PLL!()

    Shared BLYR%(), PLYR%()

    Shared careerBatters$(), careerPitchers$()
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

        If careerBatters$(I) <> "" And careerBatters$(I) <> "XXX" And careerBatters$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If BRS%(J, I, 17) <> 0 Then

                    If BRS%(J, I, 0) > BLL!(0) Then
                        BLL!(0) = BRS%(J, I, 0)
                        careerBL$(0) = careerBatters$(I)
                        BLYR%(0) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 2) > BLL!(1) Then
                        BLL!(1) = BRS%(J, I, 2)
                        careerBL$(1) = careerBatters$(I)
                        BLYR%(1) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 1) > BLL!(2) Then
                        BLL!(2) = BRS%(J, I, 1)
                        careerBL$(2) = careerBatters$(I)
                        BLYR%(2) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 4) > BLL!(3) Then
                        BLL!(3) = BRS%(J, I, 4)
                        careerBL$(3) = careerBatters$(I)
                        BLYR%(3) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 5) > BLL!(4) Then
                        BLL!(4) = BRS%(J, I, 5)
                        careerBL$(4) = careerBatters$(I)
                        BLYR%(4) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 6) > BLL!(5) Then
                        BLL!(5) = BRS%(J, I, 6)
                        careerBL$(5) = careerBatters$(I)
                        BLYR%(5) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 3) > BLL!(6) Then
                        BLL!(6) = BRS%(J, I, 3)
                        careerBL$(6) = careerBatters$(I)
                        BLYR%(6) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 7) > BLL!(7) Then
                        BLL!(7) = BRS%(J, I, 7)
                        careerBL$(7) = careerBatters$(I)
                        BLYR%(7) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 8) > BLL!(8) Then
                        BLL!(8) = BRS%(J, I, 8)
                        careerBL$(8) = careerBatters$(I)
                        BLYR%(8) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 9) > BLL!(9) Then
                        BLL!(9) = BRS%(J, I, 9)
                        careerBL$(9) = careerBatters$(I)
                        BLYR%(9) = BRS%(J, I, 17)
                    Else
                        If BRS%(J, I, 10) > BLL!(10) Then
                            BLL!(10) = BRS%(J, I, 10)
                            careerBL$(10) = careerBatters$(I)
                            BLYR%(10) = BRS%(J, I, 17)
                        End If
                    End If

                    If BRS%(J, I, 11) > BLL!(11) Then
                        BLL!(11) = BRS%(J, I, 11)
                        careerBL$(11) = careerBatters$(I)
                        BLYR%(11) = BRS%(J, I, 17)
                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then

                        AVG! = BRS%(J, I, 1) / BRS%(J, I, 0)

                        If AVG! > BLL!(12) Then
                            BLL!(12) = AVG!
                            careerBL$(12) = careerBatters$(I)
                            BLYR%(12) = BRS%(J, I, 17)
                        End If

                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then
                        
                        SLG! = (BRS%(J, I, 1) + BRS%(J, I, 4) + BRS%(J, I, 5) * 2 + BRS%(J, I, 6) * 3) / BRS%(J, I, 0)

                        If SLG! > BLL!(13) Then
                            BLL!(13) = SLG!
                            careerBL$(13) = careerBatters$(I)
                            BLYR%(13) = BRS%(J, I, 17)
                        End If

                    End If

                    If BRS%(J, I, 0) + BRS%(J, I, 7) > 501 Then

                        OBP! = (BRS%(J, I, 1) + BRS%(J, I, 7)) / (BRS%(J, I, 7) + BRS%(J, I, 0))

                        If OBP! > BLL!(14) Then
                            BLL!(14) = OBP!
                            careerBL$(14) = careerBatters$(I)
                            BLYR%(14) = BRS%(J, I, 17)
                        End If

                    End If

                End If 'Done checking for non-zero value

            Next J

        End If 'Done checking for terminating characters

    Next I

    'Pitchers
    For I = 0 To 79

        If careerPitchers$(I) <> "" And careerPitchers$(I) <> "XXX" And careerPitchers$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If PRS%(J, I, 16) <> 0 Then

                    If PRS%(J, I, 6) > PLL!(0) Then
                        PLL!(0) = PRS%(J, I, 6)
                        careerPL$(0) = careerPitchers$(I)
                        PLYR%(0) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 7) > PLL!(1) Then
                        PLL!(1) = PRS%(J, I, 7)
                        careerPL$(1) = careerPitchers$(I)
                        PLYR%(1) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 12) > PLL!(2) Then
                        PLL!(2) = PRS%(J, I, 12)
                        careerPL$(2) = careerPitchers$(I)
                        PLYR%(2) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > PLL!(3) Then
                        PLL!(3) = PRS%(J, I, 0) / 3
                        careerPL$(3) = careerPitchers$(I)
                        PLYR%(3) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 1) > PLL!(4) Then
                        PLL!(4) = PRS%(J, I, 1)
                        careerPL$(4) = careerPitchers$(I)
                        PLYR%(4) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 4) > PLL!(5) Then
                        PLL!(5) = PRS%(J, I, 4)
                        careerPL$(5) = careerPitchers$(I)
                        PLYR%(5) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 5) > PLL!(6) Then
                        PLL!(6) = PRS%(J, I, 5)
                        careerPL$(6) = careerPitchers$(I)
                        PLYR%(6) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 0) / 3 > 162 Then
                        EV! = (PRS%(J, I, 3) * 9) / (PRS%(J, I, 0) / 3)
                        If PLL!(7) = 0 Then PLL!(7) = 1000
                        If EV! < PLL!(7) Then
                            PLL!(7) = EV!
                            careerPL$(7) = careerPitchers$(I)
                            PLYR%(7) = PRS%(J, I, 16)
                        End If
                    End If

                    If PRS%(J, I, 8) > PLL!(8) Then
                        PLL!(8) = PRS%(J, I, 8)
                        careerPL$(8) = careerPitchers$(I)
                        PLYR%(8) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 10) > PLL!(9) Then
                        PLL!(9) = PRS%(J, I, 10)
                        careerPL$(9) = careerPitchers$(I)
                        PLYR%(9) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 2) > PLL!(10) Then
                        PLL!(10) = PRS%(J, I, 2)
                        careerPL$(10) = careerPitchers$(I)
                        PLYR%(10) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 3) > PLL!(11) Then
                        PLL!(11) = PRS%(J, I, 3)
                        careerPL$(11) = careerPitchers$(I)
                        PLYR%(11) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 11) > PLL!(12) Then
                        PLL!(12) = PRS%(J, I, 11)
                        careerPL$(12) = careerPitchers$(I)
                        PLYR%(12) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 14) > PLL!(13) Then
                        PLL!(13) = PRS%(J, I, 14)
                        careerPL$(13) = careerPitchers$(I)
                        PLYR%(13) = PRS%(J, I, 16)
                    End If

                    If PRS%(J, I, 13) > PLL!(14) Then
                        PLL!(14) = PRS%(J, I, 13)
                        careerPL$(14) = careerPitchers$(I)
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

