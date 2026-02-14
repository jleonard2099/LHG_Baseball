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

        menuChoices$(1) = "CREATE INITIAL CAREER STATS FILE"
        menuChoices$(2) = "ADD CAREER YEAR TO STATS        "
        menuChoices$(3) = "REMOVE PLAYERS FROM CAREER STATS"
        menuChoices$(4) = "REVIEW CAREER STATS             "
        menuChoices$(5) = "REVIEW CAREER RECORDS           "
        menuChoices$(6) = "MANAGE TEAM CAREER LEADERS      "
        menuChoices$(7) = "RETURN TO MAIN MENU             "
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

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

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
'      CreateCareerFile Subroutine
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
        Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING AND"
        Locate , 18: Print "MERGING THE INITIAL CAREER FILE."
        Locate , 18: Print "IF YOU ARE ADDING AN EXTRA YEAR TO AN EXISTING"
        Locate , 18: Print "CAREER FILE, USE THE 'ADD CAREER YEAR' OPTION"
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

    Shared careerB0(), careerB1(), carRecBatStats()
    Shared careerP0(), careerP1(), carRecPitStats()

    Shared statTeamBat!(), statTeamPit!()

    NB = -1
    NP = -1

    Do

        Color 11, 0
        Cls

        'careerFile$ = _InputBox$("Career File", "Enter name for Career stats file")
        careerFile$ = _SaveFileDialog$("Save Career File", diskPaths$(1), "*.CAR", "Career files [.CAR]")

        If careerFile$ <> "" Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                statFile$ = nameStatFile$(team$)

                Color 15, 0
                Cls

                statYear = Val(YN$)
                Locate 8, 53: Print targetYear

                Do

                    Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
                    Locate 9, 22: Print "ANY CHANGE? (Y/N)"

                    J$ = GetKeyPress$

                Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N" Or J$ = Chr$(27)

                If UCase$(J$) = "N" Then

                    Do
                        Locate 11, 18
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
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)

                    Else

                        Locate 20, 13
                        Color 15
                        Print "MERGING AND UPDATING CAREER STATS . . . . PLEASE WAIT"

                        Open diskPaths$(1) + statFile$ For Input As #1

                        Input #1, team$
                        Input #1, teamWins
                        Input #1, teamLosses

                        For I = 0 To 22:
                            Input #1, careerBatters$(I)
                            Input #1, careerB1(I)
                        Next I

                        For I = 0 To 21:
                            Input #1, careerPitchers$(I)
                            Input #1, careerP1(I)
                        Next I

                        For I = 0 To 22:

                            For I1 = 0 To 21:
                                Input #1, careerB0(I, I1)
                            Next I1

                        Next I

                        For I = 0 To 21:

                            For I1 = 0 To 41:
                                Input #1, careerP0(I, I1)
                            Next I1

                        Next I

                        Input #1, D0, D1

                        For I = 0 To 21:
                            Input #1, statTeamBat!(I)
                        Next I

                        For I = 0 To 22:
                            Input #1, statTeamPit!(I)
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
                                carRecBatStats(N, I, J) = careerB0(I, J)
                            Next J

                            carRecBatStats(N, I, 16) = 1
                            carRecBatStats(N, I, 17) = statYear

                        Next I

                        For I = 0 To 79

                            If careerPitchers$(I) = "END" Or careerPitchers$(I) = "XXX" Or careerPitchers$(I) = "" Then
                                careerPitchers$(I) = ""
                            Else
                                NP = NP + 1
                            End If

                            For J = 0 To 14
                                carRecPitStats(N, I, J) = careerP0(I, J)
                            Next J

                            carRecPitStats(N, I, 15) = 1
                            carRecPitStats(N, I, 16) = statYear

                        Next I

                        If doPostLeaders = 1 Then
                            Call UpdateCareerRecords(diskPaths$(2) + GetFileBaseName$(careerFile$) + ".CLD")
                        End If

                        Call SaveCareerFile(careerFile$)

                    End If 'Done checking for stat file

                End If 'Done checking for changes

            End If 'Done checking if team was chosen

        End If

    Loop Until UCase$(I$) = "N" Or UCase$(J$) = "N"

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

    recordFile$ = _OpenFileDialog$("Choose Career Record file", diskPaths$(2), "*.CLD", "Career Record files [.CLD]")

    If recordFile$ <> "" Then

        Color 15, 0
        Cls

        Call LoadCareerLeaders(recordFile$)

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Career Records Report", _CWD$ + "\Career Records Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CareerRecords_Report(0, userFile$)
                    End If

                Case "P"
                    Call CareerRecords_Report(1, "CAREER")

                Case "S":
                    Call CareerRecords_Screen(GetFileBaseName$(recordFile$))

            End Select

        End If

    End If 'Done checking for ESC

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

    Shared carRecBatLdrVal!(), carRecPitLdrVal!()
    Shared carRecBatYear(), carRecPitYear()

    Shared carRecBatName$(), carRecPitName$()

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    Print #1, Tab((80 - Len(careerName$)) / 2)
    Print #1, careerName$
    Print #1,

    Print #1, "CATEGORY           RECORD           PLAYER              YEAR"
    Print #1,
    Print #1, "AT BATS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(0);
    Print #1, Tab(37); carRecBatName$(0);
    Print #1, Tab(56); carRecBatYear(0)
    Print #1, "RUNS SCORED";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(1);
    Print #1, Tab(37); carRecBatName$(1);
    Print #1, Tab(56); carRecBatYear(1)
    Print #1, "HITS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(2);
    Print #1, Tab(37); carRecBatName$(2);
    Print #1, Tab(56); carRecBatYear(2)
    Print #1, "DOUBLES";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(3);
    Print #1, Tab(37); carRecBatName$(3);
    Print #1, Tab(56); carRecBatYear(3)
    Print #1, "TRIPLES";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(4);
    Print #1, Tab(37); carRecBatName$(4);
    Print #1, Tab(56); carRecBatYear(4)
    Print #1, "HOME RUNS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(5);
    Print #1, Tab(37); carRecBatName$(5);
    Print #1, Tab(56); carRecBatYear(5)
    Print #1, "RUNS BATTED IN";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(6);
    Print #1, Tab(37); carRecBatName$(6);
    Print #1, Tab(56); carRecBatYear(6)
    Print #1, "WALKS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(7);
    Print #1, Tab(37); carRecBatName$(7);
    Print #1, Tab(56); carRecBatYear(7)
    Print #1, "STRIKEOUTS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(8);
    Print #1, Tab(37); carRecBatName$(8);
    Print #1, Tab(56); carRecBatYear(8)
    Print #1, "STEALS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(9);
    Print #1, Tab(37); carRecBatName$(9);
    Print #1, Tab(56); carRecBatYear(9)
    Print #1, "CAUGHT STEALING";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(10);
    Print #1, Tab(37); carRecBatName$(10);
    Print #1, Tab(56); carRecBatYear(10)
    Print #1, "ERRORS";
    Print #1, Tab(23); Using "###"; carRecBatLdrVal!(11);
    Print #1, Tab(37); carRecBatName$(11);
    Print #1, Tab(56); carRecBatYear(11)

    Print #1, "BATTING AVERAGE";

    If carRecBatLdrVal!(12) > 0 Then
        Print #1, Tab(22); Using ".###"; carRecBatLdrVal!(12);
        Print #1, Tab(37); carRecBatName$(12);
        Print #1, Tab(56); carRecBatYear(12)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "SLUGGING PCT.";
    If carRecBatLdrVal!(13) > 0 Then
        Print #1, Tab(22); Using "#.###"; carRecBatLdrVal!(13);
        Print #1, Tab(37); carRecBatName$(13);
        Print #1, Tab(56); carRecBatYear(13)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "ON BASE PCT.";
    If carRecBatLdrVal!(14) > 0 Then
        Print #1, Tab(22); Using "#.###"; carRecBatLdrVal!(14);
        Print #1, Tab(37); carRecBatName$(14);
        Print #1, Tab(56); carRecBatYear(14)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "WINS";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(0);
    Print #1, Tab(37); carRecPitName$(0);
    Print #1, Tab(56); carRecPitYear(0)
    Print #1, "LOSSES";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(1);
    Print #1, Tab(37); carRecPitName$(1);
    Print #1, Tab(56); carRecPitYear(1)
    Print #1, "SAVES";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(2);
    Print #1, Tab(37); carRecPitName$(2);
    Print #1, Tab(56); carRecPitYear(2)

    Print #1, "INNINGS PITCHED";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(3);:

    If carRecPitLdrVal!(3) - (Int(carRecPitLdrVal!(3) / 3) * 3) > 0 Then
        Print #1, ".";
        Print #1, Using "#"; carRecPitLdrVal!(3) - (Int(carRecPitLdrVal!(3) / 3) * 3);:
    End If

    Print #1, Tab(37); carRecPitName$(3);
    Print #1, Tab(56); carRecPitYear(3)

    Print #1, "HITS ALLOWED";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(4);
    Print #1, Tab(37); carRecPitName$(4);
    Print #1, Tab(56); carRecPitYear(4)
    Print #1, "WALKS";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(5);
    Print #1, Tab(37); carRecPitName$(5);
    Print #1, Tab(56); carRecPitYear(5)
    Print #1, "STRIKOUTS";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(6);
    Print #1, Tab(37); carRecPitName$(6);
    Print #1, Tab(56); carRecPitYear(6)

    Print #1, "EARNED RUN AVERAGE";:
    If carRecPitLdrVal!(7) > 0 Then
        Print #1, Tab(22); Using "##.##"; carRecPitLdrVal!(7);
        Print #1, Tab(37); carRecPitName$(7);
        Print #1, Tab(56); carRecPitYear(7)
    Else
        Print #1, Tab(22); " ---"
    End If

    Print #1, "GAMES";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(8);
    Print #1, Tab(37); carRecPitName$(8);
    Print #1, Tab(56); carRecPitYear(8)
    Print #1, "COMPLETE GAMES";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(9);
    Print #1, Tab(37); carRecPitName$(9);
    Print #1, Tab(56); carRecPitYear(9)
    Print #1, "RUNS ALLOWED";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(10);
    Print #1, Tab(37); carRecPitName$(10);
    Print #1, Tab(56); carRecPitYear(10)
    Print #1, "EARNED RUNS ALLOWED";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(11);
    Print #1, Tab(37); carRecPitName$(11);
    Print #1, Tab(56); carRecPitYear(11)
    Print #1, "SHUTOUTS";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(12);
    Print #1, Tab(37); carRecPitName$(12);
    Print #1, Tab(56); carRecPitYear(12)
    Print #1, "HOME RUNS ALLOWED";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(13);
    Print #1, Tab(37); carRecPitName$(13);
    Print #1, Tab(56); carRecPitYear(13)
    Print #1, "DOUBLE PLAYS";
    Print #1, Tab(23); Using "###"; carRecPitLdrVal!(14);
    Print #1, Tab(37); carRecPitName$(14);
    Print #1, Tab(56); carRecPitYear(14)

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
'    CareerRecords_Screen Subroutine
'----------------------------------------
'This routine formats career records for
'output to the screen
Sub CareerRecords_Screen (careerName$)

    Shared carRecBatLdrVal!(), carRecPitLdrVal!()
    Shared carRecBatYear(), carRecPitYear()

    Shared carRecBatName$(), carRecPitName$()

    Do

        Cls
        Color 15, 0

        Locate 1, (80 - Len(careerName$)) / 2
        Print careerName$

        Locate 3, 1
        Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

        Locate 4, 1
        Color 3: Print "AT BATS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(0);
        Print Tab(37); carRecBatName$(0);
		Print Tab(56); carRecBatYear(0)
        Color 3: Print "RUNS SCORED";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(1);
        Print Tab(37); carRecBatName$(1);
		Print Tab(56); carRecBatYear(1)
        Color 3: Print "HITS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(2);
        Print Tab(37); carRecBatName$(2);
		Print Tab(56); carRecBatYear(2)
        Color 3: Print "DOUBLES";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(3);
        Print Tab(37); carRecBatName$(3);
		Print Tab(56); carRecBatYear(3)
        Color 3: Print "TRIPLES";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(4);
        Print Tab(37); carRecBatName$(4);
		Print Tab(56); carRecBatYear(4)
        Color 3: Print "HOME RUNS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(5);
        Print Tab(37); carRecBatName$(5);
		Print Tab(56); carRecBatYear(5)
        Color 3: Print "RUNS BATTED IN";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(6);
        Print Tab(37); carRecBatName$(6);
		Print Tab(56); carRecBatYear(6)
        Color 3: Print "WALKS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(7);
        Print Tab(37); carRecBatName$(7);
		Print Tab(56); carRecBatYear(7)
        Color 3: Print "STRIKEOUTS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(8);
        Print Tab(37); carRecBatName$(8);
		Print Tab(56); carRecBatYear(8)
        Color 3: Print "STEALS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(9);
        Print Tab(37); carRecBatName$(9);
		Print Tab(56); carRecBatYear(9)
        Color 3: Print "CAUGHT STEALING";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(10);
        Print Tab(37); carRecBatName$(10);
		Print Tab(56); carRecBatYear(10)
        Color 3: Print "ERRORS";
        Color 15: Print Tab(23); Using "###"; carRecBatLdrVal!(11);
        Print Tab(37); carRecBatName$(11);
		Print Tab(56); carRecBatYear(11)
        Color 3: Print "BATTING AVERAGE";: Color 15

        If carRecBatLdrVal!(12) > 0 Then
            Print Tab(22); Using ".###"; carRecBatLdrVal!(12);
            Print Tab(37); carRecBatName$(12);
            Print Tab(56); carRecBatYear(12)
        Else
            Print Tab(22); " ---"
        End If

        Color 3: Print "SLUGGING PCT.";: Color 15:
        If carRecBatLdrVal!(13) > 0 Then
            Print Tab(22); Using "#.###"; carRecBatLdrVal!(13);
            Print Tab(37); carRecBatName$(13);
            Print Tab(56); carRecBatYear(13)
        Else
            Print Tab(22); " ---"
        End If

        Color 3: Print "ON BASE PCT.";: Color 15:
        If carRecBatLdrVal!(13) > 0 Then
            Print Tab(22); Using "#.###"; carRecBatLdrVal!(14);
            Print Tab(37); carRecBatName$(14);
			Print Tab(56); carRecBatYear(14)
        Else
            Print Tab(22); " ---"
        End If

        Color 3: Print "WINS";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(0);
        Print Tab(37); carRecPitName$(0);
        Print Tab(56); carRecPitYear(0)
        Color 3: Print "LOSSES";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(1);
        Print Tab(37); carRecPitName$(1);
		Print Tab(56); carRecPitYear(1)
        Color 3: Print "SAVES";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(2);
        Print Tab(37); carRecPitName$(2);
		Print Tab(56); carRecPitYear(2)
        Color 3: Print "INNINGS PITCHED";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(3);:

        If carRecPitLdrVal!(3) - (Int(carRecPitLdrVal!(3) / 3) * 3) > 0 Then 
            Print ".";
            Print Using "#"; carRecPitLdrVal!(3) - (Int(carRecPitLdrVal!(3) / 3) * 3);
        End If

        Print Tab(37); carRecPitName$(3);
        Print Tab(56); carRecPitYear(3)
        Color 3: Print "HITS ALLOWED";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(4);
        Print Tab(37); carRecPitName$(4);
        Print Tab(56); carRecPitYear(4)
        Color 3: Print "WALKS";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(5);
        Print Tab(37); carRecPitName$(5);
        Print Tab(56); carRecPitYear(5)
        Color 3: Print "STRIKOUTS";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(6);
        Print Tab(37); carRecPitName$(6);
        Print Tab(56); carRecPitYear(6)

        Color 3: Print "EARNED RUN AVERAGE";: Color 15
        If carRecPitLdrVal!(7) > 0 Then
            Print Tab(22); Using "##.##"; carRecPitLdrVal!(7);
            Print Tab(37); carRecPitName$(7);
            Print Tab(56); carRecPitYear(7)
        Else
            Print Tab(22); " ---"
        End If

        Color 14
        Locate LAST_ROW - 1, 28: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

        Cls

        Locate 1, (80 - Len(careerName$)) / 2
        Print careerName$

        Locate 3, 1
        Color 12: Print "CATEGORY           RECORD           PLAYER              YEAR"

        Locate 4, 1
        Color 3: Print "GAMES";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(8);
        Print Tab(37); carRecPitName$(8);
        Print Tab(56); carRecPitYear(8)
        Color 3: Print "COMPLETE GAMES";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(9);
        Print Tab(37); carRecPitName$(9);
		Print Tab(56); carRecPitYear(9)
        Color 3: Print "RUNS ALLOWED";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(10);
        Print Tab(37); carRecPitName$(10);
		Print Tab(56); carRecPitYear(10)
        Color 3: Print "EARNED RUNS ALLOWED";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(11);
        Print Tab(37); carRecPitName$(11);
		Print Tab(56); carRecPitYear(11)
        Color 3: Print "SHUTOUTS";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(12);
        Print Tab(37); carRecPitName$(12);
		Print Tab(56); carRecPitYear(12)
        Color 3: Print "HOME RUNS ALLOWED";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(13);
        Print Tab(37); carRecPitName$(13);
		Print Tab(56); carRecPitYear(13)
        Color 3: Print "DOUBLE PLAYS";
        Color 15: Print Tab(23); Using "###"; carRecPitLdrVal!(14);
        Print Tab(37); carRecPitName$(14);
		Print Tab(56); carRecPitYear(14)

        Do
            Color 14
            Locate LAST_ROW - 1, 31: Print "VIEW AGAIN? (Y/N)";
            I$ = GetKeyPress$
            Color 15
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'      MergeCareerFiles Subroutine
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

        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING FILE"
        Print "IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
        Print
        Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE ANY UNUSED PLAYER SLOTS"
        Print "(XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
        Print
        Print "ALSO, ENSURE THAT PLAYER NAMES IN THE FILE YOU ARE ADDING MATCH, EXACTLY,"
        Print "THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR PREVIOUS YEARS."
        Print "EXAMPLE - IF BILLY WILLIAMS IS LISTED AS 'WILLIAMS' IN THE CAREER FILE,"
        Print "AND THE SEASON STATS HAD HIM AS 'B.WILLIAMS', HE SHOULD BE CHANGED TO"
        Print "'WILLIAMS' IN THE SEASON FILE BEFORE MERGING"
        Print
        Print

        Color 12
        Locate , 25
        Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        careerFile$ = _OpenFileDialog$("Choose Career file", diskPaths$(1), "*.CAR", "Career Career files [.CAR]")

        If careerFile$ <> "" Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                Call MergeAddYear(careerFile$, statFile$, YN$)

            End If

        End If 'Done checking for ESC

    End If 'Done confirming user choice

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
Sub MergeAddYear (selectedFile$, statFile$, yearNumber$)

    Color 15, 0
    Cls

    targetYear = Val(yearNumber$)
    Locate 8, 53: Print targetYear

    Do
        Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; targetYear
        Locate 9, 22: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        Do
            Locate 11, 18
            Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then
            doPostLeaders = 1
        Else
            doPostLeaders = 0
        End If

        Call AddCareerYear(targetYear, doPostLeaders, selectedFile$, statFile$)

    End If ' Done checking for year to add

End Sub


'----------------------------------------
'       AddCareerYear Subroutine
'----------------------------------------
'This routine loads the existing career
'stats, adds the team's stats to them,
'then overwrites the career file with the
'new values.
Sub AddCareerYear (targetYear, doPostLeaders, careerFile$, statFile$)

    Shared NB, NP

    Shared careerBatters$(), careerPitchers$()
    Dim BR$(100), PR$(80)

    Shared careerB0(), careerB1(), carRecBatStats()
    Shared careerP0(), careerP1(), carRecPitStats()

    Shared statTeamBat!(), statTeamPit!()

    Dim TMR1

    NB = -1

    Call LoadCareerFile(careerFile$)

    Locate 20, 21
    Print "LOADING STATS FILE . . . . PLEASE WAIT"

    TMR1 = 3 + (23 * 2) + (22 * 2) + (23 * 22) + (22 * 42)

    'Call ReadTeamStats(temp$, statFile$)

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, team$
    Input #1, teamWins
    Input #1, teamLosses

    For I = 0 To 22:
        Input #1, BR$(I)
        Input #1, careerB1(I)

        Color 9
        Locate 21, 35
        TMR1 = TMR1 - 1
        Print Using "####"; TMR1
    Next I

    For I = 0 To 21:
        Input #1, PR$(I)
        Input #1, careerP1(I)

        Color 9
        Locate 21, 35
        TMR1 = TMR1 - 1
        Print Using "####"; TMR1
    Next I

    For I = 0 To 22:
        For I1 = 0 To 21:
            Input #1, careerB0(I, I1)

            Color 9
            Locate 21, 35
            TMR1 = TMR1 - 1: Print Using "####"; TMR1
        Next I1
    Next I

    For I = 0 To 21:
        For I1 = 0 To 41:
            Input #1, careerP0(I, I1)

            Color 9
            Locate 21, 35
            TMR1 = TMR1 - 1
            Print Using "####"; TMR1
        Next I1
    Next I

    Input #1, D0, D1

    For I = 0 To 21:
        Input #1, statTeamBat!(I)
    Next I

    For I = 0 To 22:
        Input #1, statTeamPit!(I)
    Next I

    Close #1

    'Batters
    For I = 0 To 22

        RR = 0
        CHK = 0
        matchFound = 0

        For J = 0 To 99

            If UCase$(BR$(I)) = UCase$(careerBatters$(J)) Then

                CHK = 0

                'Find next stat year to save
                For W = 1 To MAX_CAREER_YEARS
                    If carRecBatStats(W, J, 17) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                If CHK > 0 Then

                    N = 0

                    'Look at number of years from previous
                    For Z = 1 To MAX_CAREER_YEARS
                        N = N + carRecBatStats(Z, J, 16)
                    Next Z

                Else

                    N = 0
                    For Z = 1 To MAX_CAREER_YEARS
                        N = N + carRecBatStats(Z, J, 16)
                    Next Z
                    N = N + 1

                End If

                For K = 0 To 15:
                    carRecBatStats(N, J, K) = careerB0(I, K)
                Next K

                carRecBatStats(N, J, 16) = 1
                carRecBatStats(N, J, 17) = targetYear

                matchFound = 1
                Exit For

            Else

                RR = RR + 1

                If RR = 100 Then

                    If BR$(I) <> "END" And BR$(I) <> "XXX" And BR$(I) <> "" Then

                        NB = NB + 1
                        careerBatters$(NB) = BR$(I)
                        N = 1

                        For K = 0 To 15:
                            carRecBatStats(N, NB, K) = careerB0(I, K)
                        Next

                        carRecBatStats(N, NB, 16) = 1
                        carRecBatStats(N, NB, 17) = targetYear

                    End If

                End If

            End If

        Next J

    Next I

    'Pitchers
    For I = 0 To 21

        RRP = 0
        CHK = 0

        For J = 0 To 79

            If UCase$(PR$(I)) = UCase$(careerPitchers$(J)) Then

                CHK = 0

                For W = 1 To MAX_CAREER_YEARS
                    If carRecPitStats(W, J, 16) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                If CHK > 0 Then
                    N = 0

                    For Z = 1 To MAX_CAREER_YEARS
                        N = N + carRecPitStats(Z, J, 15)
                    Next Z
                Else
                    N = 0

                    For Z = 1 To MAX_CAREER_YEARS
                        N = N + carRecPitStats(Z, J, 15)
                    Next Z

                    N = N + 1
                End If

                For K = 0 To 14:
                    carRecPitStats(N, J, K) = careerP0(I, K)
                Next K

                carRecPitStats(N, J, 15) = 1
                carRecPitStats(N, J, 16) = targetYear

                matchFound = 1
                Exit For

            Else

                RRP = RRP + 1

                If RRP = 80 Then

                    If PR$(I) <> "END" And PR$(I) <> "XXX" And PR$(I) <> "" Then
                        NP = NP + 1
                        careerPitchers$(NP) = PR$(I)
                        N = 1

                        For K = 0 To 14:
                            carRecPitStats(N, NP, K) = careerP0(I, K)
                        Next

                        carRecPitStats(N, NP, 15) = 1
                        carRecPitStats(N, NP, 16) = targetYear
                    End If

                End If

            End If

        Next J

    Next I

    Locate 20, 21
    Print Space$(38)

    If doPostLeaders = 1 Then Call UpdateCareerRecords(careerFile$)

    Call SaveCareerFile(careerFile$)

End Sub


'----------------------------------------
'      LoadCareerFile Subroutine
'----------------------------------------
'This routine loads the existing career
'file data into arrays.
Sub LoadCareerFile (loadFile$)

    Shared NB, NP

    Shared careerBatters$(), careerPitchers$()

    Shared carRecBatStats(), carRecPitStats()

    Dim TMR As Long

    Color 15, 0
    Cls

    Locate 12, 21
    Print "LOADING CAREER FILE . . . . PLEASE WAIT"

    TMR = 100 + 80 + (MAX_CAREER_YEARS * 100 * 18) + (MAX_CAREER_YEARS * 80 * 17)

    Open loadFile$ For Input As #1

    Input #1, NB, NP

    For I = 0 To 99:
        Input #1, careerBatters$(I)

        Color 5
        Locate 21, 38
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 79:
        Input #1, careerPitchers$(I)

        Color 5
        Locate 21, 38
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For J = 1 To MAX_CAREER_YEARS:

        For I = 0 To 99:

            For I1 = 0 To 17:
                Input #1, carRecBatStats(J, I, I1)

                Color 5
                Locate 21, 38
                TMR = TMR - 1: Print Using "####"; TMR
            Next I1

        Next I

    Next J

    For J = 1 To MAX_CAREER_YEARS:

        For I = 0 To 79:

            For I1 = 0 To 16:
                Input #1, carRecPitStats(J, I, I1)

                Color 5
                Locate 21, 38
                TMR = TMR - 1: Print Using "####"; TMR
            Next I1

        Next I

    Next J

    Close #1

    Locate 12, 21
    Print Space$(39)

End Sub


'----------------------------------------
'      SaveCareerFile Subroutine
'----------------------------------------
'This routine updates the existing career
'file with the modified stats values.
Sub SaveCareerFile (saveFile$)

    Shared NB, NP

    Shared careerBatters$(), careerPitchers$()

    Shared carRecBatStats(), carRecPitStats()

    Dim TMR As Long

    Cls
    Color 15

    Locate 12, 18
    Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

    TMR = 100 + 80 + (MAX_CAREER_YEARS * 100 * 18) + (MAX_CAREER_YEARS * 80 * 17)

    Open saveFile$ For Output As #3

    Write #3, NB, NP

    For I = 0 To 99:
        Write #3, careerBatters$(I)
    Next

    For I = 0 To 79:
        Write #3, careerPitchers$(I)
    Next

    '# Years
    For J = 1 To MAX_CAREER_YEARS:

        For I = 0 To 99:

            For I1 = 0 To 17:
                Write #3, carRecBatStats(J, I, I1)
            Next I1

            Color 3
            Locate 21, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

    Next J

    For J = 1 To MAX_CAREER_YEARS:

        For I = 0 To 79:

            For I1 = 0 To 16:
                Write #3, carRecPitStats(J, I, I1)
            Next I1

            Color 3
            Locate 21, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

    Next J

    Close #3

    Locate 12, 18
    Print Space$(44)

End Sub


'----------------------------------------
'      LoadCareerLeaders Subroutine
'----------------------------------------
'This routine loads the existing career
'leaders file data into arrays.
Sub LoadCareerLeaders (leaderFile$)

    Shared carRecBatYear(), carRecPitYear()
    Shared carRecBatName$(), carRecPitName$()
    Shared carRecBatLdrVal!(), carRecPitLdrVal!()

    Open leaderFile$ For Input As #1

    For I = 0 To 14:
        Input #1, carRecBatYear(I), carRecBatName$(I), carRecBatLdrVal!(I)
    Next

    For I = 0 To 14:
        Input #1, carRecPitYear(I), carRecPitName$(I), carRecPitLdrVal!(I)
    Next

    Close #1

End Sub


'----------------------------------------
'     UpdateCareerRecords Subroutine
'----------------------------------------
'This routine reads in any existing career
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerRecords (careerRecordFile$)

    Dim AVG!, SLG!, OBP!, EV!

    Shared carRecBatLdrVal!(), carRecPitLdrVal!()
    Shared carRecBatYear(), carRecPitYear()

    Shared careerBatters$(), careerPitchers$()
    Shared carRecBatName$(), carRecPitName$()

    Shared carRecBatStats(), carRecPitStats()

    If _FileExists(careerRecordFile$) Then
        Call LoadCareerLeaders(careerRecordFile$)
    End If

    'Batters
    For I = 0 To 99

        If careerBatters$(I) <> "" And careerBatters$(I) <> "XXX" And careerBatters$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If carRecBatStats(J, I, 17) <> 0 Then

                    If carRecBatStats(J, I, 0) > carRecBatLdrVal!(0) Then
                        carRecBatLdrVal!(0) = carRecBatStats(J, I, 0)
                        carRecBatName$(0) = careerBatters$(I)
                        carRecBatYear(0) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 2) > carRecBatLdrVal!(1) Then
                        carRecBatLdrVal!(1) = carRecBatStats(J, I, 2)
                        carRecBatName$(1) = careerBatters$(I)
                        carRecBatYear(1) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 1) > carRecBatLdrVal!(2) Then
                        carRecBatLdrVal!(2) = carRecBatStats(J, I, 1)
                        carRecBatName$(2) = careerBatters$(I)
                        carRecBatYear(2) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 4) > carRecBatLdrVal!(3) Then
                        carRecBatLdrVal!(3) = carRecBatStats(J, I, 4)
                        carRecBatName$(3) = careerBatters$(I)
                        carRecBatYear(3) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 5) > carRecBatLdrVal!(4) Then
                        carRecBatLdrVal!(4) = carRecBatStats(J, I, 5)
                        carRecBatName$(4) = careerBatters$(I)
                        carRecBatYear(4) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 6) > carRecBatLdrVal!(5) Then
                        carRecBatLdrVal!(5) = carRecBatStats(J, I, 6)
                        carRecBatName$(5) = careerBatters$(I)
                        carRecBatYear(5) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 3) > carRecBatLdrVal!(6) Then
                        carRecBatLdrVal!(6) = carRecBatStats(J, I, 3)
                        carRecBatName$(6) = careerBatters$(I)
                        carRecBatYear(6) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 7) > carRecBatLdrVal!(7) Then
                        carRecBatLdrVal!(7) = carRecBatStats(J, I, 7)
                        carRecBatName$(7) = careerBatters$(I)
                        carRecBatYear(7) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 8) > carRecBatLdrVal!(8) Then
                        carRecBatLdrVal!(8) = carRecBatStats(J, I, 8)
                        carRecBatName$(8) = careerBatters$(I)
                        carRecBatYear(8) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 9) > carRecBatLdrVal!(9) Then
                        carRecBatLdrVal!(9) = carRecBatStats(J, I, 9)
                        carRecBatName$(9) = careerBatters$(I)
                        carRecBatYear(9) = carRecBatStats(J, I, 17)
                    Else
                        If carRecBatStats(J, I, 10) > carRecBatLdrVal!(10) Then
                            carRecBatLdrVal!(10) = carRecBatStats(J, I, 10)
                            carRecBatName$(10) = careerBatters$(I)
                            carRecBatYear(10) = carRecBatStats(J, I, 17)
                        End If
                    End If

                    If carRecBatStats(J, I, 11) > carRecBatLdrVal!(11) Then
                        carRecBatLdrVal!(11) = carRecBatStats(J, I, 11)
                        carRecBatName$(11) = careerBatters$(I)
                        carRecBatYear(11) = carRecBatStats(J, I, 17)
                    End If

                    If carRecBatStats(J, I, 0) + carRecBatStats(J, I, 7) > 501 Then

                        AVG! = carRecBatStats(J, I, 1) / carRecBatStats(J, I, 0)

                        If AVG! > carRecBatLdrVal!(12) Then
                            carRecBatLdrVal!(12) = AVG!
                            carRecBatName$(12) = careerBatters$(I)
                            carRecBatYear(12) = carRecBatStats(J, I, 17)
                        End If

                    End If

                    If carRecBatStats(J, I, 0) + carRecBatStats(J, I, 7) > 501 Then

                        SLG! = (carRecBatStats(J, I, 1) + carRecBatStats(J, I, 4) + carRecBatStats(J, I, 5) * 2 + carRecBatStats(J, I, 6) * 3) / carRecBatStats(J, I, 0)

                        If SLG! > carRecBatLdrVal!(13) Then
                            carRecBatLdrVal!(13) = SLG!
                            carRecBatName$(13) = careerBatters$(I)
                            carRecBatYear(13) = carRecBatStats(J, I, 17)
                        End If

                    End If

                    If carRecBatStats(J, I, 0) + carRecBatStats(J, I, 7) > 501 Then

                        OBP! = (carRecBatStats(J, I, 1) + carRecBatStats(J, I, 7)) / (carRecBatStats(J, I, 7) + carRecBatStats(J, I, 0))

                        If OBP! > carRecBatLdrVal!(14) Then
                            carRecBatLdrVal!(14) = OBP!
                            carRecBatName$(14) = careerBatters$(I)
                            carRecBatYear(14) = carRecBatStats(J, I, 17)
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

                If carRecPitStats(J, I, 16) <> 0 Then

                    If carRecPitStats(J, I, 6) > carRecPitLdrVal!(0) Then
                        carRecPitLdrVal!(0) = carRecPitStats(J, I, 6)
                        carRecPitName$(0) = careerPitchers$(I)
                        carRecPitYear(0) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 7) > carRecPitLdrVal!(1) Then
                        carRecPitLdrVal!(1) = carRecPitStats(J, I, 7)
                        carRecPitName$(1) = careerPitchers$(I)
                        carRecPitYear(1) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 12) > carRecPitLdrVal!(2) Then
                        carRecPitLdrVal!(2) = carRecPitStats(J, I, 12)
                        carRecPitName$(2) = careerPitchers$(I)
                        carRecPitYear(2) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 0) / 3 > carRecPitLdrVal!(3) Then
                        carRecPitLdrVal!(3) = carRecPitStats(J, I, 0) / 3
                        carRecPitName$(3) = careerPitchers$(I)
                        carRecPitYear(3) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 1) > carRecPitLdrVal!(4) Then
                        carRecPitLdrVal!(4) = carRecPitStats(J, I, 1)
                        carRecPitName$(4) = careerPitchers$(I)
                        carRecPitYear(4) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 4) > carRecPitLdrVal!(5) Then
                        carRecPitLdrVal!(5) = carRecPitStats(J, I, 4)
                        carRecPitName$(5) = careerPitchers$(I)
                        carRecPitYear(5) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 5) > carRecPitLdrVal!(6) Then
                        carRecPitLdrVal!(6) = carRecPitStats(J, I, 5)
                        carRecPitName$(6) = careerPitchers$(I)
                        carRecPitYear(6) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 0) / 3 > 162 Then
                        EV! = (carRecPitStats(J, I, 3) * 9) / (carRecPitStats(J, I, 0) / 3)
                        If carRecPitLdrVal!(7) = 0 Then carRecPitLdrVal!(7) = 1000
                        If EV! < carRecPitLdrVal!(7) Then
                            carRecPitLdrVal!(7) = EV!
                            carRecPitName$(7) = careerPitchers$(I)
                            carRecPitYear(7) = carRecPitStats(J, I, 16)
                        End If
                    End If

                    If carRecPitStats(J, I, 8) > carRecPitLdrVal!(8) Then
                        carRecPitLdrVal!(8) = carRecPitStats(J, I, 8)
                        carRecPitName$(8) = careerPitchers$(I)
                        carRecPitYear(8) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 10) > carRecPitLdrVal!(9) Then
                        carRecPitLdrVal!(9) = carRecPitStats(J, I, 10)
                        carRecPitName$(9) = careerPitchers$(I)
                        carRecPitYear(9) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 2) > carRecPitLdrVal!(10) Then
                        carRecPitLdrVal!(10) = carRecPitStats(J, I, 2)
                        carRecPitName$(10) = careerPitchers$(I)
                        carRecPitYear(10) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 3) > carRecPitLdrVal!(11) Then
                        carRecPitLdrVal!(11) = carRecPitStats(J, I, 3)
                        carRecPitName$(11) = careerPitchers$(I)
                        carRecPitYear(11) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 11) > carRecPitLdrVal!(12) Then
                        carRecPitLdrVal!(12) = carRecPitStats(J, I, 11)
                        carRecPitName$(12) = careerPitchers$(I)
                        carRecPitYear(12) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 14) > carRecPitLdrVal!(13) Then
                        carRecPitLdrVal!(13) = carRecPitStats(J, I, 14)
                        carRecPitName$(13) = careerPitchers$(I)
                        carRecPitYear(13) = carRecPitStats(J, I, 16)
                    End If

                    If carRecPitStats(J, I, 13) > carRecPitLdrVal!(14) Then
                        carRecPitLdrVal!(14) = carRecPitStats(J, I, 13)
                        carRecPitName$(14) = careerPitchers$(I)
                        carRecPitYear(14) = carRecPitStats(J, I, 16)
                    End If

                End If 'Done checking value of 0

            Next J

        End If 'Done checking for bogus values

    Next I

    Open careerRecordFile$ For Output As #1

    For I = 0 To 14:
        Write #1, carRecBatYear(I), carRecBatName$(I), carRecBatLdrVal!(I)
    Next

    For I = 0 To 14:
        Write #1, carRecPitYear(I), carRecPitName$(I), carRecPitLdrVal!(I)
    Next

    Close #1

End Sub
