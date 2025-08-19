'----------------------------------------
'           UMPMGR Subroutine
'----------------------------------------
'This subroutine gives the user options for
'working with umpire files.
Sub UMPMGR ()

    _Title "Full Count Baseball - Umpire Manager"

    Do

        Cls
        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "ENTER A NEW UMPIRE FILE  "
        menuChoices$(2) = "EDIT EXISTING UMPIRE FILE"
        menuChoices$(3) = "RETURN TO MAIN MENU      "
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

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call EnterUmpireFile

                Case 2:
                    Call EditUmpireFile

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       EnterUmpireFile Subroutine
'----------------------------------------
'This subroutine assists the user with
'creating a specific umpire file.
Sub EnterUmpireFile ()

    Shared leagWalksPerG!, leagRunsPerG!, leagSOsPerG!

    Shared umpireName$(), umpireRating()
    Shared umpireLeague$
    Shared umpireYear

    'Initialize values
    For I = 1 To 100
        umpireName$(I) = "XXX"
        For I1 = 0 To 2
            umpireRating(I, I1) = -1
        Next I1
    Next I

    Do

        Do

            Cls

            Locate 2
            Print "IS THIS UMPIRING CREW:"
            Color 14
            Locate 4
            Print "(1)";
            Color 15
            Print " - AL"
            Color 14
            Print "(2)";
            Color 15
            Print " - NL"
            I$ = GetKeyPress$
            I = Val(I$)

        Loop Until I >= 1 And I <= 2 Or I$ = Chr$(27)

        If I$ <> Chr$(27) Then

            If I = 1 Then
                LG$ = "AL"
            Else
                LG$ = "NL"
            End If

        End If

        Locate 7: Print "LEAGUE IS "; LG$

        '--- Determine year

        Locate 10
        Input "ENTER LEAGUE YEAR"; umpireYear
        Locate 12
        Print "ANY CHANGES (Y/N)?";

        Do
            yearChange$ = GetKeyPress$
        Loop Until UCase$(yearChange$) = "N" Or UCase$(yearChange$) = "Y"

    Loop Until UCase$(yearChange$) = "N"


    ' *** League Averages Entry ***
    Cls

    Color 12
    Locate 2, 27: Print "LEAGUE AVERAGES (ALL TEAMS)"

    Color 15

    Locate 4: Input "1. WALKS "; leagWalks
    Locate 4, 10: Print " "

    Locate 5: Input "2. STRIKES "; leagSO
    Locate 5, 12: Print " "

    Locate 6: Input "3. RUNS "; leagRuns
    Locate 6, 9: Print " "

    Locate 7: Input "4. GAMES "; leagRuns
    Locate 7, 9: Print " "

    Do

        Do
            Locate 9, 1
            Color 11
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 4

        Color 15

        Select Case B9

            Case 1:
                Locate 4, 10: Print Space$(10)
                Locate 4, 1: Input "1. WALKS "; leagWalks
                Locate 4, 10: Print " "

            Case 2:
                Locate 5, 12: Print Space$(10)
                Locate 5, 1: Input "2. STRIKES "; leagSO
                Locate 5, 12: Print " "

            Case 3:
                Locate 6, 9: Print Space$(10)
                Locate 6, 1: Input "3. RUNS "; leagRuns
                Locate 6, 9: Print " "

            Case 4:
                Locate 7, 9: Print Space$(10)
                Locate 7, 1: Input "4. GAMES "; leagGames
                Locate 7, 9: Print " "

        End Select

        Locate 9, 27
        Print Space$(10)

    Loop Until B9 = 0

    leagWalksPerG! = CInt((leagWalks / leagGames) * 100) / 100
    leagSOsPerG! = CInt((leagSO / leagGames) * 100) / 100
    leagRunsPerG! = CInt((leagRuns / leagGames) * 100) / 100

    ' *** Umpire Values Entry ***
    ' --> This will never show you the summary of umpires you are entering
    ' --> We should fix that
    For X = 1 To MAX_UMPIRES

        Cls

        Locate 2
        Color 14
        Print "UMPIRE #"; X

        Color 15
        Locate 4
        Input "ENTER UMPIRE NAME (END TO QUIT) "; umpireName$(X)

        If UCase$(umpireName$(X)) = "END" Then

            Erase umpireName$
            Erase umpireRating
            Exit For

        Else

            Locate 6, 1: Input "1. WALKS"; umpWalks
            Locate 6, 10: Print " "

            Locate 7, 1: Input "2. STRIKES"; umpSO
            Locate 7, 12: Print " "

            Locate 8, 1: Input "3. RUNS"; umpRuns
            Locate 8, 9: Print " "

            Locate 9, 1: Input "4. GAMES UMPIRED"; gamesUmp
            Locate 9, 9: Print " "

        End If

        Do

            Do
                Locate 10, 1
                Color 11
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Color 15
            Loop Until B9 >= 0 And B9 <= 4

            Select Case B9

                Case 1:
                    Locate 6, 10: Print Space$(10)
                    Locate 6, 1: Input "1. WALKS "; umpWalks
                    Locate 6, 10: Print " "

                Case 2:
                    Locate 7, 12: Print Space$(10)
                    Locate 7, 1: Input "2. STRIKES "; umpSO
                    Locate 7, 12: Print " "

                Case 3:
                    Locate 8, 9: Print Space$(10)
                    Locate 8, 1: Input "3. RUNS "; umpRuns
                    Locate 8, 9: Print " "

                Case 4:
                    Locate 8, 18: Print Space$(10)
                    Locate 8, 1: Input "4. GAMES UMIPRED "; umpGames
                    Locate 8, 18: Print " "

            End Select

            Locate 10, 27
            Print Space$(10)

        Loop Until B9 = 0

        umpireRating(X, 0) = (umpWalksPerG! / leagWalksPerG!) * 100
        umpireRating(X, 1) = (umpSOsPerG! / leagSOsPerG!) * 100
        umpireRating(X, 2) = (umpRunsPerG! / leagRunsPerG!) * 100

    Next

    finalFile$ = diskPaths$(0) + "_" + LG$ + "." + Str$(umpireYear)

    Call SaveUmpireFile(finalFile$)

    Erase umpireName$
    Erase umpireRating

End Sub

'----------------------------------------
'       EditUmpireFile Subroutine
'----------------------------------------
'This subroutine assists the user with
'editing specific umpires in the file.
Sub EditUmpireFile ()

    Shared leagWalksPerG!, leagRunsPerG!, leagSOsPerG!

    Shared umpireName$(), umpireRating()
    Shared umpireLeague$
    Shared umpireYear

    umpireFile$ = _OpenFileDialog$("Choose Umpire file", diskPaths$(0), "UMPIRES*.*", "Umpire files [UMPIRES*.*]")

    If umpireFile$ <> "" Then

        Call ReadUmpireFile(umpireFile$)

        Cls

        leagWalksPerG! = CInt(leagWalksPerG! * 10) / 10
        leagSOsPerG! = CInt(leagSOsPerG! * 10) / 10
        leagRunsPerG! = CInt(leagRunsPerG! * 10) / 10

        Color 12
        Locate 2, 27: Print "EDIT LEAGUE TOTALS (ALL TEAMS)"

        Color 14
        Locate 4, 33: Print "CREW LEAGUE: ";
        Color 15
        Print umpireLeague$
        Color 14
        Locate 5, 33: Print "CREW YEAR: ";
        Color 15
        Print umpireYear

        Locate 7: Print "1. WALKS / GAME "; leagWalksPerG!
        Locate 8: Print "2. STRIKES / GAME "; leagSOsPerG!
        Locate 9: Print "3. RUNS / GAME "; leagRunsPerG!

        Do

            Do
                Locate 11, 1
                Color 11
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 3

            Color 15

            Select Case B9

                Case 1:
                    Locate 7, 17: Print Space$(10)
                    Locate 7, 1: Input "1. WALKS / GAME"; leagWalksPerG!
                    Locate 7, 17: Print " "

                Case 2:
                    Locate 8, 19: Print Space$(10)
                    Locate 8, 1: Input "2. STRIKES / GAME"; leagSOsPerG!
                    Locate 8, 19: Print " "

                Case 3:
                    Locate 9, 16: Print Space$(10)
                    Locate 9, 1: Input "3. RUNS / GAME"; leagRunsPerG!
                    Locate 9, 16: Print " "

            End Select

            Locate 11, 27
            Print Space$(10)

        Loop Until B9 = 0


        ' ** Umpire Values **
        umpsPerPage = 40

        For page = 1 To 3

            Do

                Cls

                Color 12
                Locate 2, 35: Print "UMPIRE LIST"

                Color 15

                For I = 1 To 10

                    umpStartIdx = (umpsPerPage * (page - 1))

                    Locate I + 3, 1
                    'Color 14
                    Print Using "###. "; I + umpStartIdx;
                    'Color 15
                    Print umpireName$(I + umpStartIdx);

                    Locate , 21
                    'Color 14
                    Print Using "###. "; I + umpStartIdx + 10;
                    'Color 15
                    Print umpireName$(I + umpStartIdx + 10);

                    If page < 3 Then

                        Locate , 41
                        'Color 14
                        Print Using "###. "; I + umpStartIdx + 20;
                        'Color 15
                        Print umpireName$(I + umpStartIdx + 20);

                        Locate , 61
                        'Color 14
                        Print Using "###. "; I + umpStartIdx + 30;
                        'Color 15
                        Print umpireName$(I + umpStartIdx + 30);

                    End If

                Next

                Do

                    Locate 18, 5

                    Color 11
                    If page = maxPages Then
                        Input "SELECT UMPIRE TO EDIT (0 TO QUIT) "; umpIdx
                    Else
                        Input "SELECT UMPIRE TO EDIT (0 FOR MORE) "; umpIdx
                    End If

                Loop Until umpIdx >= 0 And umpIdx <= MAX_UMPIRES

                Color 15

                If umpIdx <> 0 Then

                    'Reverse engineer umpire's original ratings
                    umpWalksPerG! = (umpireRating(umpIdx, 0) * leagRunsPerG!) / 100
                    umpSOsPerG! = (umpRating(umpIdx, 1) * leagSOsPerG!) / 100
                    umpRunsPerG! = (umpireRating(umpIdx, 2) * leagWalksPerG!) / 100

                    'Round to 2 decimal places
                    umpWalksPerG! = CInt(umpWalksPerG! * 10) / 10
                    umpSOsPerG! = CInt(umpSOsPerG! * 10) / 10
                    umpRunsPerG! = CInt(umpRunsPerG! * 10) / 10

                    Cls

                    Color 14
                    Locate 2, 30: Print "UMPIRE - ";
                    Color 15
                    Print umpIdx
                    Locate , 1

                    Color 12
                    Locate 4, 18: Print "VALUES ARE FOR SPECIFIED SEASON + LEAGUE ONLY"

                    Color 15
                    Locate 6: Print "1. NAME "; umpireName$(umpIdx)
                    Locate 7: Print "2. WALKS / GAME"; umpWalksPerG!
                    Locate 8: Print "3. STRIKES / GAME "; umpSOsPerG!
                    Locate 9: Print "4. RUNS / GAME "; umpRunsPerG!

                    C1 = 1

                    Do

                        Do
                            Color 11
                            Locate 11: Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Loop Until B9 >= 0 And B9 <= 4

                        Color 15

                        Select Case B9

                            Case 1:
                                Locate 6, 9: Print Space$(25)
                                Locate 6, 1: Input "1. NAME"; umpireName$(umpIdx)
                                Locate 6, 9: Print " "

                            Case 2:
                                Locate 7, 17: Print Space$(10)
                                Locate 7, 1: Input "2. WALKS / GAME"; umpWalksPerG!
                                Locate 7, 17: Print " "

                            Case 3:
                                Locate 8, 19: Print Space$(10)
                                Locate 8, 1: Input "3. STRIKES / GAME"; umpSOsPerG!
                                Locate 8, 19: Print " "

                            Case 4:
                                Locate 9, 16: Print Space$(10)
                                Locate 9, 1: Input "4. RUNS / GAME"; umpRunsPerG!
                                Locate 9, 16: Print " "

                        End Select

                    Loop Until B9 = 0

                    umpireRating(umpIdx, 0) = (umpWalksPerG! / leagWalksPerG!) * 100
                    umpireRating(umpIdx, 1) = (umpSOsPerG! / leagSOsPerG!) * 100
                    umpireRating(umpIdx, 2) = (umpRunsPerG! / leagRunsPerG!) * 100

                End If

            Loop Until umpIdx = 0

        Next page

    End If

End Sub


'----------------------------------------
'       SaveUmpireFile Subroutine
'----------------------------------------
'This subroutine loads all of the umpires
'from a previously created file.
Sub ReadUmpireFile (umpireFile$)

    Shared leagWalksPerG!, leagRunsPerG!, leagSOsPerG!

    Shared umpireName$(), umpireRating()
    Shared umpireLeague$
    Shared umpireYear

    Open umpireFile$ For Input As #1

    Input #1, umpireLeague$
    Input #1, umpireYear
    Input #1, leagWalksPerG!
    Input #1, leagRunsPerG!
    Input #1, leagSOsPerG!

    For X = 1 To MAX_UMPIRES:

        Input #1, umpireName$(X)

        For X1 = 0 To 2
            Input #1, umpireRating(X, X1)
        Next X1

    Next

    Close #1

End Sub


'----------------------------------------
'       SaveUmpireFile Subroutine
'----------------------------------------
'This subroutine saves all of the umpires
'that have been created/loaded to a file.
Sub SaveUmpireFile (umpireFile$)

    Shared leagWalksPerG!, leagRunsPerG!, leagSOsPerG!

    Shared umpireName$(), umpireRating()
    Shared umpireLeague$
    Shared umpireYear

    Open umpireFile$ For Output As #1

    Print #1, umpireLeague$
    Print #1, umpireYear
    Print #1, leagWalksPerG!
    Print #1, leagRunsPerG!
    Print #1, leagSOsPerG!

    For X = 1 To MAX_UMPIRES:

        Print #1, umpireName$(X)

        For X1 = 0 To 2
            Print #1, umpireRating(X, X1)
        Next X1

    Next

    Close #1

End Sub
