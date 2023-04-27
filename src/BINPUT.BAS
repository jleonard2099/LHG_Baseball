Sub BINPUT ()

    _Title "Full Count Baseball - Team Input"

    DefInt A-Z

    Shared ratings%(), statP0%(), SS%()

    Shared statB$(), BP$(), statP$()

    Shared K9&()

    Shared T$ 'only if we can't keep up with team name separately

    ReDim menuChoices$(1 To 9)

    Do
        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "INPUT A NEW TEAM           "
        menuChoices$(2) = "EDIT AN EXISTING TEAM      "
        menuChoices$(3) = "PRINT ACTUAL TEAM STATS    "
        menuChoices$(4) = "ENTER/EDIT UMPIRES         "
        menuChoices$(5) = "SWAP SPOTS ON A TEAM ROSTER"
        menuChoices$(6) = "RETURN TO MAIN MENU        "
        menuChoices$(7) = "CALCULATE BUNT RATINGS     "
        menuChoices$(8) = "CALCULATE RUN RATINGS      "
        menuChoices$(9) = "VIEW TEAM ROSTERS/RATINGS  "
        Selection% = 1
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 25

        Call box0(6, 24, 16, 52, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1:
                    Call InputTeam(Selection%, YN$, T$, T)

                    'Read from stat file
                    N$ = RTrim$(Q$(0))

                    For xx = 4 To 11
                        If Mid$(N$, xx, 1) = " " Or Mid$(N$, xx, 1) = "." Then Mid$(N$, xx, 1) = "_"
                    Next xx

                    QQ = 8

                    For xx = 11 To 4 Step -1
                        If Mid$(N$, xx, 1) = "_" Then
                            QQ = xx - 4
                        Else
                            Exit For
                        End If
                    Next xx

                    F$ = Mid$(N$, 4, QQ)

                    Call SaveTeamFile(YN$, T$, F$, T)

                Case 2:
                    Call GetTeam_BINPUT(YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call InputTeam(Selection%, YN$, T$, T)

                Case 3:
                    Call GetTeam_BINPUT(YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call PrintTeamStats(T$, T)

                Case 4:
                    Call UMPMGR

                Case 5:
                    Call GetTeam_BINPUT(YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call SwapSpots(YN$, T$, T)

                Case 6:
                    Cls

                Case 7:
                    Call GetTeam_BINPUT(YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call CalculateBuntRating

                    'Read from stat file
                    N$ = RTrim$(Q$(0))

                    For xx = 4 To 11
                        If Mid$(N$, xx, 1) = " " Or Mid$(N$, xx, 1) = "." Then Mid$(N$, xx, 1) = "_"
                    Next xx

                    QQ = 8

                    For xx = 11 To 4 Step -1
                        If Mid$(N$, xx, 1) = "_" Then
                            QQ = xx - 4
                        Else
                            Exit For
                        End If
                    Next xx

                    F$ = Mid$(N$, 4, QQ)

                    Call SaveTeamFile(YN$, T$, F$, T)
                    'GoTo 1920

                Case 8:
                    Call GetTeam_BINPUT(YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call CalculateRunRating

                    'Read from stat file
                    N$ = RTrim$(Q$(0))

                    For xx = 4 To 11
                        If Mid$(N$, xx, 1) = " " Or Mid$(N$, xx, 1) = "." Then Mid$(N$, xx, 1) = "_"
                    Next xx

                    QQ = 8

                    For xx = 11 To 4 Step -1
                        If Mid$(N$, xx, 1) = "_" Then
                            QQ = xx - 4
                        Else
                            Exit For
                        End If
                    Next xx

                    F$ = Mid$(N$, 4, QQ)

                    Call SaveTeamFile(YN$, T$, F$, T)

                Case 9:
                    Call GetTeam_BINPUT(YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call ViewRostersRatings(T$)

            End Select

        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

    Cls
    '88 Exit Sub

End Sub

'----------------------------------------
'       GetTeam_BINPUT subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_BINPUT (yearNumber$, numberTeams, teamIdx)

    Shared ratings%()

    Do

        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_BINPUT(yearNumber$, numberTeams, teamIdx)
        Else
            yearNumber$ = NY$
            Erase teamNames$
            numberTeams = 0

            If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
                Locate 10, 8
                Call CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO SELECT ANOTHER ID", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
                BO% = 1
            Else
                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

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

                Call SelectTeam_BINPUT(yearNumber$, numberTeams, teamIdx)

            End If 'Done checking for file

        End If 'Done checking input

        If BO% = 1 Then
            BO% = 0
        Else
            T$ = RTrim$(Q$(0))
            Locate 2, 33
            Color ratings%(11), ratings%(12): Print T$

            Color 15, 4

            Do
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'     SelectTeam_BINPUT subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_BINPUT (yearNumber$, numberTeams, teamIdx)

    Shared draftB%(), draftP%()
    Shared DYS%(), inputINJ%(), ratings%()
    Shared inputSA%(), inputTS%()

    Shared statB$(), statP$()

    Shared K9&()

    ' Shared M$, NN$, S$
    Shared CK

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIdx = teamIndex%(Selection%)

        Locate 2, 33: Color 31: Print "LOADING..."

        Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14)

        For I = 0 To 22
            Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26)
            Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38)
            Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50)
            Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57)
        Next

        For I = 0 To 21
            Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015)
            Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027)
            Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039)
            Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
        Next

        Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

        Get #1, teamIdx

        For I = 1 To 13: ratings%(I) = CVI(Q$(I)): Next

        K9&(0) = CVS(Q$(14))

        For I = 0 To 22: statB$(I) = RTrim$(Q$(I * 43 + 15))
            For I1 = 0 To 41
                draftB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
            Next
        Next

        For I = 0 To 21
            statP$(I) = RTrim$(Q$(I * 38 + 1004))
            For I1 = 0 To 36
                draftP%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
            Next
        Next

        CK = CVI(Q$(1843))
        Close 1

        If _FileExists(diskPaths$(0) + "LR." + yearNumber$) Then

            Open diskPaths$(0) + "LR." + yearNumber$ For Random As #1 Len = 4108

            For X = 0 To 11
                Field #1, X * 2 As X$, 2 As Q$(X + 1)
            Next

            For X = 0 To 22
                For X1 = 1 To 14
                    Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14)
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14)
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14)
                Next
            Next

            For X = 1 To 24
                Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X)
            Next

            For X = 0 To 22
                For X1 = 1 To 24
                    Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24)
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24)
                Next
            Next

            Get #1, T

            For X = 0 To 11
                inputTS%(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 22
                For X1 = 1 To 14
                    draftB%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    draftP%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    draftP%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
                Next
            Next

            For X = 1 To 24
                inputSA%(X) = CVI(Q$(X + 950))
            Next

            For X = 0 To 22
                For X1 = 1 To 24
                    draftB%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    draftP%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
                Next
            Next

            Close 1

            If inputTS%(0) <> 999 Then
                For X = 0 To 22
                    For X1 = 42 To 79
                        draftB%(X, X1) = 0
                    Next
                Next

                For X = 0 To 21
                    For X1 = 37 To 88
                        draftP%(X, X1) = 0
                    Next
                Next

                For X = 1 To 24
                    inputSA%(X) = 0
                Next

                For X = 0 To 11
                    inputTS%(X) = 0
                Next

            End If

        End If

        'Read from stat file
        N$ = RTrim$(Q$(0))

        For xx = 4 To 11
            If Mid$(N$, xx, 1) = " " Or Mid$(N$, xx, 1) = "." Then Mid$(N$, xx, 1) = "_"
        Next xx

        QQ = 8

        For xx = 11 To 4 Step -1
            If Mid$(N$, xx, 1) = "_" Then
                QQ = xx - 4
            Else
                Exit For
            End If
        Next xx

        targetFile$ = Mid$(N$, 4, QQ)
        targetFile$ = targetFile$ + "." + Left$(N$, 2)

        If _FileExists(diskPaths$(1) + targetFile$ + "D") Then

            Open diskPaths$(1) + targetFile$ + "D" For Input As #1
            For I1 = 0 To 22
                Input #1, inputINJ%(I1)
            Next
            For I1 = 0 To 21
                Input #1, DYS%(I1)
            Next
        End If

        Close 1

    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'     ViewRostersRatings subroutine
'----------------------------------------
'...explanation...
Sub ViewRostersRatings (targetTeam$)

    Shared ERX!

    Shared K9&()
    Shared statB$(), statP$()
    Shared draftB%(), draftP%()
    Shared ratings%(), TB%(), inputTP%()

    For K = 0 To 15
        TB%(K) = 0
    Next

    For K = 0 To 33
        inputTP%(K) = 0
    Next

    For K = 4 To 18:
        For I = 0 To 22
            If statB$(I) <> "XXX" Then
                TB%(K - 3) = TB%(K - 3) + draftB%(I, K)
            End If
        Next
    Next

    For K = 4 To 18:
        For I = 0 To 21
            If statP$(I) <> "XXX" And draftP%(I, 11) = 999 Then
                TB%(K - 3) = TB%(K - 3) + draftP%(I, K + 16)
            End If
        Next
    Next

    For K = 1 To 33:
        For I = 0 To 21
            If statP$(I) <> "XXX" Then
                inputTP%(K) = inputTP%(K) + draftP%(I, K)
            End If
        Next
    Next

    ERX! = 0

    For I = 0 To 21
        If statP$(I) <> "XXX" Then
            ERX! = ERX! + ((draftP%(I, 6) / 9) * (draftP%(I, 10) / 100))
        End If
    Next

    Cls

    Color 12: Locate 5, 10: Print targetTeam$
    Color 3: Locate 7, 10: Print "LEAGUE BATTING AVG.";
    Locate 7, 38: Color 7: Print Using "#####"; ratings%(1)
    Color 3: Locate , 10: Print "LEAGUE STRIKE OUT AVG.";
    Locate , 38: Color 7: Print Using "#####"; ratings%(2)
    Color 3: Locate , 10: Print "LEAGUE BASE ON BALLS AVG.";
    Locate , 38: Color 7: Print Using "#####"; ratings%(3)
    Color 3: Locate , 10: Print "LEAGUE HOME RUN AVG.";
    Locate , 38: Color 7: Print Using "#####"; ratings%(4)
    Color 3: Locate , 10: Print "DOUBLE PLAYS AVG.";
    Locate , 38: Color 7: Print Using "#####"; ratings%(5)
    Color 3: Locate , 10: Print "PARK DOUBLE ADJ.";
    Locate , 38: Color 7: Print Using "#####"; ratings%(6)
    Color 3: Locate , 10: Print "PARK TRIPLE ADJ.";
    Locate , 38: Color 7: Print Using "#####"; ratings%(7)
    Color 3: Locate , 10: Print "PARK HOME RUN ADJ.";
    Locate , 37: Color 7: Print Using "######"; K9&(0)
    Color 3: Locate , 10: Print "PARK FOUL GROUND ADJ.";
    Locate , 37: Color 7: Print Using "######"; ratings%(8)
    Color 15: Locate 23, 10: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Do

        Cls
        Color 12: Print "TEAM OPTIONS"
        Print
        Color 14: Print "(1) ";: Color 15: Print "VIEW HITTERS"
        Print
        Color 14: Print "(2) ";: Color 15: Print "VIEW PITCHERS"
        Print
        Color 14: Print "(3) ";: Color 15: Print "VIEW PITCHERS HITTING/FLDG STATS"
        Print
        Color 14: Print "(4) ";: Color 15: Print "RETURN TO MAIN MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 4

        Select Case I
            Case 1: Call ViewHitters
            Case 2: Call ViewPitchers
            Case 3: Call ViewHittingFielding
            Case 4: Cls '
        End Select

        ' --wrap it up here

    Loop Until I = 4

End Sub

'----------------------------------------
'           InputTeam subroutine
'----------------------------------------
'...explanation...
'inputAction% = 1: New Team
'inputAction% = 2: Edit Team
Sub InputTeam (inputAction%, yearNumber$, targetTeam$, teamIdx)

    Shared draftB%(), draftP%()
    Shared ratings%(), inputSA%(), inputTS%()

    Shared statB$(), statP$()

    Shared K9&()

    ' New team only
    If inputAction% = 1 Then

        targetTeam$ = ""
        teamIdx = 0
        yearNumber$ = ""

        For X = 1 To 13
            ratings%(X) = 0
        Next

        K9&(0) = 0

        For X = 0 To 22
            statB$(X) = ""
            For X1 = 0 To 41
                draftB%(X, X1) = 0
            Next
        Next

        For X = 0 To 21
            statP$(X) = ""
            For X1 = 0 To 36
                draftP%(X, X1) = 0
            Next
        Next

        S$ = ""
        M$ = ""
        NN$ = ""
        CK = 0

        For X = 0 To 22
            For X1 = 42 To 79
                draftB%(X, X1) = 0
            Next
        Next

        For X = 0 To 21
            For X1 = 37 To 88
                draftP%(X, X1) = 0
            Next
        Next

        For X = 1 To 24
            inputSA%(X) = 0
        Next

        For X = 0 To 11
            inputTS%(X) = 0
        Next
    Else
        S$ = RTrim$(Q$(1840))
        M$ = RTrim$(Q$(1841))
        NN$ = RTrim$(Q$(1842))
    End If

    Call EDITTEAM(yearNumber$, targetTeam$, NN$, S$, M$, teamIdx)
    Call BATTEREDIT
    Call PITCHEREDIT

    Locate 10, 8

End Sub

'----------------------------------------
'        SaveTeamFile subroutine
'----------------------------------------
Sub SaveTeamFile (yearNumber$, targetTeam$, targetFile$, teamIdx)

    ' Shared M$, NN$, S$

    Shared K9&()

    Shared DYS%(), inputINJ%()
    Shared draftB%(), draftP%()
    Shared ratings%(), inputSA%(), inputTS%()

    Shared statB$(), statP$()

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))

    Cls

    Locate 11,

    Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

    Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14)

    For I = 0 To 22
        Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26)
        Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38)
        Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50)
        Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57)
    Next

    For I = 0 To 21
        Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015)
        Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027)
        Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039)
        Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
    Next

    Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

    LSet Q$(0) = targetTeam$

    For I = 1 To 13:
        LSet Q$(I) = MKI$(ratings%(I))
    Next

    LSet Q$(14) = MKS$(K9&(0))

    For I = 0 To 22:
        LSet Q$(I * 43 + 15) = statB$(I)
        For I1 = 0 To 41:
            LSet Q$(I * 43 + I1 + 16) = MKI$(draftB%(I, I1))
        Next
    Next

    For I = 0 To 21: LSet Q$(I * 38 + 1004) = statP$(I)
        For I1 = 0 To 36: LSet Q$(I * 38 + I1 + 1005) = MKI$(draftP%(I, I1))
        Next
    Next

    LSet Q$(1840) = S$
    LSet Q$(1841) = M$
    LSet Q$(1842) = NN$
    LSet Q$(1843) = MKI$(999)

    Put #1, teamIdx

    Close 1

    SCK = 0

    For I = 1 To 24
        SCK = SCK + inputSA%(I)
    Next

    If SCK > 0 Then
        inputTS%(0) = 999
        For I = 1 To 11
            inputTS%(I) = -1
        Next

        Open diskPaths$(0) + "LR." + yearNumber$ For Random As #1 Len = 4108

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1)
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14)
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14)
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14)
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X)
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24)
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24)
            Next
        Next

        For X = 0 To 11
            LSet Q$(X + 1) = MKI$(inputTS%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                LSet Q$(12 + X1 + X * 14) = MKI$(draftB%(X, X1 + 41))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(334 + X1 + X * 14) = MKI$(draftP%(X, X1 + 36))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(642 + X1 + X * 14) = MKI$(draftP%(X, X1 + 50))
            Next
        Next

        For X = 1 To 24
            LSet Q$(X + 950) = MKI$(inputSA%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                LSet Q$(975 + X1 + X * 24) = MKI$(draftB%(X, X1 + 55))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                LSet Q$(1528 + X1 + X * 24) = MKI$(draftP%(X, X1 + 64))
            Next
        Next

        Put #1, teamIdx
        Close 1

    End If

    Open diskPaths$(1) + targetFile$ + "D" For Output As #1

    For I1 = 0 To 22
        Print #1, inputINJ%(I1)
    Next

    For I1 = 0 To 21
        Print #1, DYS%(I1)
    Next

    Close 1

    Call CSMsgBox("", 0, Pass)

End Sub

'----------------------------------------
'       SwapSpots subroutine
'----------------------------------------
'...explanation...
Sub SwapSpots (yearNumber$, targetTeam$, teamIdx)

    Shared statB$(), BS$(), statP$(), PS$()
    Shared statB0%(), statP0%(), SS%()
    Shared statT!(), statT1!()

    Shared D0%, D1%

    Dim SS, TS

    TS = 0

    Do
        Cls
        Print "DOES "; targetTeam$; " HAVE COMPILED STATS (YN)";
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "Y" Then
        TS = 1
        Call LoadTeamStats(targetTeam$, TS, SS, teamStatFile$)
    End If

    Do
        Cls
        Print "TEAM ROSTER FOR "; targetTeam$

        For I = 0 To 22
            Locate I + 3, 1
            Print Using "## "; I + 1;
            Print statB$(I);
        Next

        For I = 0 To 21
            Locate I + 3, 25
            Print Using "## "; I + 1;
            Print statP$(I);
        Next

        Do
            Locate 10, 50: Print "1 - SWITCH BATTERS"
            Locate 11, 50: Print "2 - SWITCH PITCHERS"
            Locate 12, 50: Print "3 - SAVE CHANGES"
            Locate 13, 50: Print "4 - RETURN TO MENU (NO CHANGES)"
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 4

        Select Case I
            Case 1:
                Call SwitchBatters
            Case 2:
                Call SwitchPitchers
            Case 3:
                If TS <> 0 Then
                    Cls

                    Open diskPaths$(1) + R$ For Output As #1
                    Print #1, A$: Print #1, W2%: Print #1, L2%
                    For I = 0 To 22: Print #1, BS$(I): Print #1, BS%(I): Next I
                    For I = 0 To 21: Print #1, PS$(I): Print #1, PS%(I): Next I
                    For I = 0 To 22: For I1 = 0 To 21: Print #1, statB0%(I, I1): Next I1: Next I
                    For I = 0 To 21: For I1 = 0 To 41: Print #1, statP0%(I, I1): Next I1: Next I
                    Print #1, D0%
                    Print #1, D1%
                    For I = 0 To 21: Print #1, statT!(I): Next I
                    For I = 0 To 22: Print #1, statT1!(I): Next I
                    Close 1

                    If SS <> 0 Then
                        Open diskPaths$(1) + R$ + "R" For Output As #1
                        For I = 0 To 22
                            For I1 = 0 To 6
                                Print #1, SS%(I, I1)
                            Next I1
                        Next I
                        Close #1
                    End If
                End If

                Call SaveTeamFile(yearNumber$, targetTeam$, teamStatFile$, teamIdx)

            Case 4: '

        End Select

    Loop Until I >= 3

End Sub

'----------------------------------------
'       LoadTeamStats subroutine
'----------------------------------------
'...explanation...
Sub LoadTeamStats (targetTeam$, teamHasStats, SS, targetFile$)

    Shared BS%(), PS%(), SS%()
    Shared statB0%(), statP0%()

    Shared BS$(), PS$()

    Shared statT!(), statT1!()

    statFile$ = targetTeam$

    Cls

    For xx = 4 To 11
        If Mid$(statFile$, xx, 1) = " " Or Mid$(statFile$, xx, 1) = "." Then Mid$(statFile$, xx, 1) = "_"
    Next

    QQ = 8

    For xx = 11 To 4 Step -1
        If Mid$(statFile$, xx, 1) = "_" Then
            QQ = xx - 4
        Else
            Exit For
        End If
    Next xx

    fileName$ = Mid$(statFile$, 4, QQ)
    targetFile$ = fileName$ + "." + Left$(statFile$, 2)

    If Not _FileExists(diskPaths$(1) + targetFile$) Then
        Locate 10, 8
        Call CSMsgBox("NO COMPILED STATS WERE FOUND... PRESS ANY KEY TO RETURN TO CONTINUE", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
        teamHasStats = 0
    Else

        Open diskPaths$(1) + targetFile$ For Input As #1

        Input #1, A$: Input #1, W2%: Input #1, L2%

        For I = 0 To 22: Input #1, BS$(I): Input #1, BS%(I): Next I
        For I = 0 To 21: Input #1, PS$(I): Input #1, PS%(I): Next I
        For I = 0 To 22: For I1 = 0 To 21: Input #1, statB0%(I, I1): Next I1: Next I
        For I = 0 To 21: For I1 = 0 To 41: Input #1, statP0%(I, I1): Next I1: Next I

        Input #1, D0%: Input #1, D1%

        For I = 0 To 21: Input #1, statT!(I): Next I
        For I = 0 To 22: Input #1, statT1!(I): Next I

        Close #1

        If Not _FileExists(diskPaths$(1) + targetFile$ + "R") Then
            SS = 0
        Else
            SS = 1

            Open diskPaths$(1) + targetFile$ + "R" For Input As #1
            For I = 0 To 22
                For I1 = 0 To 6
                    Input #1, SS%(I, I1)
                Next I1
            Next I
            Close #1

        End If 'done checking for files

    End If 'Done checking for stats files

End Sub

'----------------------------------------
'       SwitchBatters subroutine
'----------------------------------------
'...explanation...
Sub SwitchBatters ()

    Shared statB$(), BS$()
    Shared BS%(), draftB%(), statB0%(), SS%()

    Do
        For I = 16 To 19: Locate I, 50: Print Space$(27): Next

        Do
            Locate 16, 50
            Input "BATTER #1 "; I1
        Loop Until I1 >= 1 And I1 <= 23

        Locate 16, 60: Print statB$(I1 - 1)

        Do
            Locate 17, 50
            Input "BATTER #2 "; I2
        Loop Until I2 >= 1 And I2 <= 23

        Locate 17, 60: Print statB$(I2 - 1)

        Do
            Locate 18, 50: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Swap statB$(I1 - 1), statB$(I2 - 1)
    Swap BS$(I1 - 1), BS$(I2 - 1)

    For I = 0 To 79: Swap draftB%(I1 - 1, I), draftB%(I2 - 1, I): Next
    For I = 0 To 21: Swap statB0%(I1 - 1, I), statB0%(I2 - 1, I): Next
    For I = 0 To 6: Swap SS%(I1 - 1, I), SS%(I2 - 1, I): Next

    Swap BS%(I1 - 1), BS%(I2 - 1)

End Sub

'----------------------------------------
'       SwitchBatters subroutine
'----------------------------------------
'...explanation...
Sub SwitchPitchers ()

    Shared draftP%(), statP0%(), PS%()
    Shared statP$(), PS$()

    Do
        For I = 16 To 19: Locate I, 50: Print Space$(27): Next

        Do
            Locate 16, 50: Input "PITCHER #1 "; I1
        Loop Until I1 >= 1 And I1 <= 22

        Locate 16, 60: Print statP$(I1 - 1)

        Do
            Locate 17, 50: Input "PITCHER #2 "; I2
        Loop Until I1 >= 1 And I1 <= 22

        Locate 17, 60: Print statP$(I2 - 1)

        Do
            Locate 18, 50: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Swap statP$(I1 - 1), statP$(I2 - 1): Swap PS$(I1 - 1), PS$(I2 - 1)

    For I = 0 To 88: Swap draftP%(I1 - 1, I), draftP%(I2 - 1, I): Next

    For I = 0 To 41: Swap statP0%(I1 - 1, I), statP0%(I2 - 1, I): Next

    Swap PS%(I1 - 1), PS%(I2 - 1)

End Sub

'----------------------------------------
'       PrintTeamStats subroutine
'----------------------------------------
'...explanation...
Sub PrintTeamStats (targetTeam$, teamIdx)

    Shared statB$(), BP$(), statP$()

    Shared draftB%(), draftP%(), ratings%()

    Shared K9&()

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))

    LPrint "TEAM # "; teamIdx

    LPrint "TEAM NAME "; targetTeam$; Tab(30); "MANAGER: "; M$; Tab(50); "STADIUM: "; S$

    LPrint: LPrint "LBA LSO LBB LHR DPG DBL TR    HR   FOUL PARK INFO"

    LPrint Using "### ### ### ### ### ### ### ###### ###  "; ratings%(1); ratings%(2); ratings%(3); ratings%(4); ratings%(5); ratings%(6); ratings%(7); K9&(0); ratings%(8);: LPrint BP$(ratings%(9)); " "; BP$(ratings%(10) + 2)

    LPrint

    LPrint "BATTER      B    G  AB   R   H DB TR HR RBI  BB  SO  SB CS RN B GA P1 FAV1 A R P2 FAV2 A R P3 FAV3 A R P4 FAV4 A R EBA BAVG"

    For I = 0 To 22

        If statB$(I) <> "" And statB$(I) <> "XXX" Then

            LPrint Left$(statB$(I), 12); Tab(13);

            If draftB%(I, 0) = 0 Then LPrint "S  ";
            If draftB%(I, 0) = -1 Then LPrint "L  ";
            If draftB%(I, 0) = 1 Then LPrint "R  ";
            If draftB%(I, 0) = -2 Then LPrint "LP ";
            If draftB%(I, 0) = 2 Then LPrint "RP ";

            LPrint Using "### ### ### ### ## ## ## ### ### ### ### ##  "; draftB%(I, 3); draftB%(I, 4); draftB%(I, 5); draftB%(I, 6); draftB%(I, 7); draftB%(I, 8); draftB%(I, 9); draftB%(I, 10); draftB%(I, 11); draftB%(I, 12); draftB%(I, 13); draftB%(I, 14);

            LPrint Using "# "; draftB%(I, 16);: LPrint Using "# "; draftB%(I, 30);

            LPrint Using "## ## #### # # "; draftB%(I, 18); draftB%(I, 22); draftB%(I, 26); draftB%(I, 33); draftB%(I, 34);

            If draftB%(I, 23) >= 0 Then
                LPrint Using "## #### # # "; draftB%(I, 23); draftB%(I, 27); draftB%(I, 35); draftB%(I, 36);
            End If

            If draftB%(I, 24) >= 0 Then
                LPrint Using "## #### # # "; draftB%(I, 24); draftB%(I, 28); draftB%(I, 37); draftB%(I, 38);
            End If

            If draftB%(I, 25) >= 0 Then
                LPrint Using "## #### # # "; draftB%(I, 25); draftB%(I, 29); draftB%(I, 39); draftB%(I, 40);
            End If

            LPrint Tab(116); Using "### "; draftB%(I, 20);

            If draftB%(I, 4) <> 0 Then
                Z% = draftB%(I, 6) / draftB%(I, 4) * 1000
                LPrint Using " ###"; draftB%(I, 6) / draftB%(I, 4) * 1000
            Else
                LPrint " 000"
            End If

        End If

    Next I

    For I = 0 To 21
        If statP$(I) <> "" And statP$(I) <> "XXX" Then

            LPrint Left$(statP$(I), 12); Tab(13);

            If draftP%(I, 0) = 1 Then LPrint "R  ";
            If draftP%(I, 0) = -1 Then LPrint "L  ";

            LPrint Using "### ### ### ### ## ## ## ### ### ### ### ## "; draftP%(I, 19); draftP%(I, 20); draftP%(I, 21); draftP%(I, 22); draftP%(I, 23); draftP%(I, 24); draftP%(I, 25); draftP%(I, 26); draftP%(I, 27); draftP%(I, 28); draftP%(I, 29); draftP%(I, 30);

            LPrint Using " # # 52  1 ####                                         ### "; draftP%(I, 31); draftP%(I, 32); draftP%(I, 17); draftP%(I, 14);

            If draftP%(I, 20) <> 0 Then
                Z% = draftP%(I, 22) / draftP%(I, 20) * 1000: LPrint Using " ###"; draftP%(I, 22) / draftP%(I, 20) * 1000
            Else
                LPrint " 000"
            End If

        End If

    Next I

    LPrint

    LPrint "PITCHER     T  W  L  SV G  GS CG IP  H   BB  SO  ERA  GA HRA HOLD EBA"

    For I = 0 To 21
        If statP$(I) <> "" And statP$(I) <> "XXX" Then

            LPrint Mid$(statP$(I), 1, 12); Tab(13);

            If draftP%(I, 0) = 1 Then LPrint "R  ";
            If draftP%(I, 0) = -1 Then LPrint "L  ";

            LPrint Using "## ## ## ## ## ## ### ### ### ### #.## ## ##   #   ###"; draftP%(I, 1); draftP%(I, 2); draftP%(I, 3); draftP%(I, 4); draftP%(I, 5); draftP%(I, 16); draftP%(I, 6); draftP%(I, 7); draftP%(I, 8); draftP%(I, 9); draftP%(I, 10) / 100; draftP%(I, 12); draftP%(I, 13); draftP%(I, 18); draftP%(I, 14)
        End If

    Next I

    LPrint Chr$(12)

End Sub

'----------------------------------------
'     CalculateRunRating subroutine
'----------------------------------------
'...explanation...
Sub CalculateRunRating ()

    Shared statB$(), statP$()
    Shared draftB%(), draftP%()

    Cls
    For I = 0 To 22
        If statB$(I) <> "" And statB$(I) <> "XXX" Then
            SPD = 200 * draftB%(I, 8) + draftB%(I, 13) * 100
            If draftB%(I, 6) + draftB%(I, 11) - draftB%(I, 7) - draftB%(I, 8) - draftB%(I, 9) = 0 Then
                RR = Int(Rnd(1) * 15) + 1
            Else
                RR = Int(SPD / (draftB%(I, 6) + draftB%(I, 11) - draftB%(I, 7) - draftB%(I, 8) - draftB%(I, 9)) + .5)
                RR = RR * 1.4
            End If

            If RR <= 3 Then draftB%(I, 16) = 1
            If RR > 3 And RR <= 5 Then draftB%(I, 16) = 2
            If RR > 5 And RR <= 10 Then draftB%(I, 16) = 3
            If RR > 10 And RR <= 15 Then draftB%(I, 16) = 4
            If RR > 15 And RR <= 20 Then draftB%(I, 16) = 5
            If RR > 20 And RR <= 25 Then draftB%(I, 16) = 6
            If RR > 25 And RR <= 30 Then draftB%(I, 16) = 7
            If RR > 30 And RR <= 40 Then draftB%(I, 16) = 8
            If RR > 40 Then draftB%(I, 16) = 9
            Print statB$(I); Tab(20); draftB%(I, 16)
        End If
    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    For I = 0 To 21
        Cls
        If statP$(I) <> "" And statP$(I) <> "XXX" Then
            RR = Int(Rnd(1) * 20) + 1
            RR = RR * 1.4

            If RR <= 4 Then draftP%(I, 31) = 1
            If RR > 4 And RR <= 8 Then draftP%(I, 31) = 2
            If RR > 8 And RR <= 12 Then draftP%(I, 31) = 3
            If RR > 12 And RR <= 16 Then draftP%(I, 31) = 4
            If RR > 16 Then draftP%(I, 31) = 5
            Print statP$(I); Tab(20); draftP%(I, 31)
        End If
    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'     CalculateBuntRating subroutine
'----------------------------------------
'...explanation...
Sub CalculateBuntRating ()

    Dim B1!, BUNT!
    Dim OB!
    Dim R1!, R2!, RR!
    Dim SLG!, SLG2!

    Shared draftB%(), draftP%()
    Shared statB$(), statP$()

    ' *** BUNT RATING CALCS ***
    Cls
    For I = 0 To 22
        If statB$(I) <> "" And statB$(I) <> "XXX" Then
            PA = draftB%(I, 11) + draftB%(I, 4)
            If PA = 0 Then PA = 1
            OB! = (draftB%(I, 6) * 100 + PA / 2) / PA
            SLG! = (draftB%(I, 6) + draftB%(I, 7) + draftB%(I, 8) * 2 + draftB%(I, 9) * 3) * 100
            SLG2! = (SLG! + PA / 2) / PA
            R1! = (draftB%(I, 12) * 100 + PA / 2) / PA
            R1! = 11 - R1! ^ 2 / 25
            R2! = 11 - (SLG2! - OB!) / 25
            BUNT! = (R1! + R1! + R2!) / 3

            B1! = draftB%(I, 15) * 1000 / PA

            Select Case B1!
                Case Is <= 0: BR = 1
                Case 1: BR = 2
                Case 2 To 3: BR = 3
                Case 4 To 6: BR = 4
                Case 7 To 10: BR = 5
                Case 11 To 15: BR = 6
                Case 16 To 20: BR = 7
                Case 21 To 25: BR = 8
                Case 26 To 30: BR = 9
                Case 31 To 35: BR = 10
                Case Is >= 36: BR = 10
            End Select

            1921 '
            BUNT! = (BUNT! + BR) / 2
            If BUNT! < 1 Then BUNT! = 1
            If BUNT! > 9 Then BUNT! = 9
            draftB%(I, 30) = Int(BUNT! + .5)
            Print statB$(I); Tab(20); draftB%(I, 30)
        End If

    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    Cls

    For I = 0 To 21
        If statP$(I) <> "" And statP$(I) <> "XXX" Then
            PA = draftP%(I, 20) + draftP%(I, 27)
            If PA = 0 Then PA = 1
            OB! = (draftP%(I, 22) * 100 + PA / 2) / PA
            SLG! = (draftP%(I, 22) + draftP%(I, 23) + draftP%(I, 24) * 2 + draftP%(I, 25) * 3) * 100
            SLG2! = (SLG! + PA / 2) / PA
            R1! = (draftP%(I, 28) * 100 + PA / 2) / PA
            R1! = 11 - R1! ^ 2 / 25
            If R1! < 1 Then R1! = 1
            If R1! > 10 Then R1! = 10
            R2! = 11 - (SLG2! - OB!) / 25
            If R2! < 1 Then R2! = 1
            If R2! > 10 Then R2! = 10
            BUNT! = (R1! + R1! + R2!) / 3
            B1! = draftP%(I, 36) * 1000 / PA

            Select Case B1!
                Case Is <= 0: BR = 1
                Case 1: BR = 2
                Case 2 To 3: BR = 3
                Case 4 To 6: BR = 4
                Case 7 To 10: BR = 5
                Case 11 To 15: BR = 6
                Case 16 To 20: BR = 7
                Case 21 To 25: BR = 8
                Case 26 To 30: BR = 9
                Case 31 To 35: BR = 10
                Case Is >= 36: BR = 10
            End Select

            1961 '
            BUNT! = (BUNT! + BR) / 2
            If BUNT! < 1 Then BUNT! = 1
            If BUNT! > 9 Then BUNT! = 9
            draftP%(I, 32) = Int(BUNT! + .5)
            Print statP$(I); Tab(20); draftP%(I, 32)
        End If
    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       BATTEREDIT subroutine
'----------------------------------------
'...explanation...
Sub BATTEREDIT ()

    Shared statB$(), draftB%()

    Shared inputINJ%()

    Do
        Color 15, 0
        Cls
        Print "EDIT/ENTER BATTERS"
        X% = 2
        For i = 0 To 10
            Locate i + 5, 5
            Print Using "##"; i + 1;: Print " "; statB$(i); Tab(40);
            Print Using "##"; i + 13;: Print " "; statB$(i + 12)
        Next
        Locate , 5: Print Using "##"; 12;: Print " "; statB$(11)
        Print
        'FOR i = 0 TO 22
        'FOR i1 = 22 TO 25
        'IF (draftB%(i, i1) > 0 AND draftB%(i, i1 + 4) = 0) OR draftB%(i, 4) = 0 THEN PRINT statB$(i)
        'NEXT
        'NEXT

        Do
            Locate 22, 1
            Print "# TO ENTER/EDIT (1-23) (0 TO CONTINUE) ";: Input i
        Loop Until i >= 0 And i <= 23

        If i <> 0 Then

            i = i - 1
            ' INITIALIZE BATTER SCREEN

            Color 15, 1: Cls
            Locate 1, 30: Print "EDIT AND INPUT BATTERS"
            Locate 3, 1: Print "NAME:"
            Locate 3, 24: Print "BATS:"
            Locate 3, 36: Print "ACTIVE:"
            Locate 3, 50: Print "INJ LENGTH:"
            Locate 5, 14: Print " G  AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
            Locate 6, 1: Print "SEASON:"
            Locate 7, 1: Print "VS LHP:"
            Locate 8, 1: Print "VS RHP:"
            Locate 10, 33: Print "FIELDING RATINGS"
            Locate 11, 31: Print "POS  FAVG  ARM RANGE"
            Locate 12, 19: Print "POSITION 1"
            Locate 13, 19: Print "POSITION 2"
            Locate 14, 19: Print "POSITION 3"
            Locate 15, 19: Print "POSITION 4"
            Locate 17, 12: Print "RUN (1-9):"
            Locate 17, 27: Print "BUNT (1-9):"
            Locate 17, 44: Print "G/A:"
            Locate 17, 54: Print "LEAGUE BAVG:"
            Locate 20, 29: Print "SPLIT STATS LEAGUE DATA"
            Locate 21, 26: Print "BAVG   K%  BB%  HR%  2B%  3B%"
            Locate 22, 13: Print "LHB vs LHP"
            Locate 23, 13: Print "LHB vs RHP"
            Locate 24, 13: Print "RHB vs LHP";
            Locate 25, 13: Print "RHB vs RHP";

            ' INITIALIZE BATTER FIELDS
            'Colr = 112
            FG = 0
            BG = 7
            Color FG, BG

            Locate 3, 7: QPrint PadRight$(statB$(i), 15), FG, BG, -1
            If draftB%(i, 0) = 2 Then BATS$ = "RP"
            If draftB%(i, 0) = -2 Then BATS$ = "LP"
            If draftB%(i, 0) = 1 Then BATS$ = "R"
            If draftB%(i, 0) = -1 Then BATS$ = "L"
            If draftB%(i, 0) = 0 Then BATS$ = "S"

            Locate 3, 30: QPrint PadRight$(BATS$, 2), FG, BG, -1
            ACT$ = "Y"
            If draftB%(i, 31) = 1 Then ACT$ = "N"
            Locate 3, 44: Print ACT$

            Locate 3, 62: Print Using "###"; inputINJ%(i)
            Locate 6, 12: Print Using "###"; draftB%(i, 3)
            Locate 6, 17: Print Using "###"; draftB%(i, 4)
            Locate 6, 22: Print Using "###"; draftB%(i, 5)
            Locate 6, 27: Print Using "###"; draftB%(i, 6)
            Locate 6, 32: Print Using "##"; draftB%(i, 7)
            Locate 6, 36: Print Using "##"; draftB%(i, 8)
            Locate 6, 40: Print Using "##"; draftB%(i, 9)
            Locate 6, 44: Print Using "###"; draftB%(i, 10)
            Locate 6, 49: Print Using "###"; draftB%(i, 11)
            Locate 6, 54: Print Using "###"; draftB%(i, 12)
            Locate 6, 59: Print Using "###"; draftB%(i, 13)
            Locate 6, 64: Print Using "##"; draftB%(i, 14)
            Locate 6, 68: Print Using "##"; draftB%(i, 15)
            Locate 7, 17: Print Using "###"; draftB%(i, 42)
            Locate 7, 27: Print Using "###"; draftB%(i, 43)
            Locate 7, 32: Print Using "##"; draftB%(i, 44)
            Locate 7, 36: Print Using "##"; draftB%(i, 45)
            Locate 7, 40: Print Using "##"; draftB%(i, 46)
            Locate 7, 49: Print Using "###"; draftB%(i, 47)
            Locate 7, 54: Print Using "###"; draftB%(i, 48)
            Locate 8, 17: Print Using "###"; draftB%(i, 49)
            Locate 8, 27: Print Using "###"; draftB%(i, 50)
            Locate 8, 32: Print Using "##"; draftB%(i, 51)
            Locate 8, 36: Print Using "##"; draftB%(i, 52)
            Locate 8, 40: Print Using "##"; draftB%(i, 53)
            Locate 8, 49: Print Using "###"; draftB%(i, 54)
            Locate 8, 54: Print Using "###"; draftB%(i, 55)
            Locate 12, 31: Print Using "##"; draftB%(i, 22)
            Locate 12, 36: Print Using "####"; draftB%(i, 26)
            Locate 12, 43: Print Using "#"; draftB%(i, 33)
            Locate 12, 47: Print Using "#"; draftB%(i, 34)
            Locate 13, 31: Print Using "##"; draftB%(i, 23)
            Locate 13, 36: Print Using "####"; draftB%(i, 27)
            Locate 13, 43: Print Using "#"; draftB%(i, 35)
            Locate 13, 47: Print Using "#"; draftB%(i, 36)
            Locate 14, 31: Print Using "##"; draftB%(i, 24)
            Locate 14, 36: Print Using "####"; draftB%(i, 28)
            Locate 14, 43: Print Using "#"; draftB%(i, 37)
            Locate 14, 47: Print Using "#"; draftB%(i, 38)
            Locate 15, 31: Print Using "##"; draftB%(i, 25)
            Locate 15, 36: Print Using "####"; draftB%(i, 29)
            Locate 15, 43: Print Using "#"; draftB%(i, 39)
            Locate 15, 47: Print Using "#"; draftB%(i, 40)
            Locate 17, 23: Print Using "#"; draftB%(i, 16)
            Locate 17, 39: Print Using "#"; draftB%(i, 30)
            Locate 17, 49: Print Using "##"; draftB%(i, 18)
            Locate 17, 67: Print Using "###"; draftB%(i, 20)
            Locate 22, 27: Print Using "###"; draftB%(i, 56)
            Locate 22, 32: Print Using "###"; draftB%(i, 57)
            Locate 22, 37: Print Using "###"; draftB%(i, 58)
            Locate 22, 42: Print Using "###"; draftB%(i, 59)
            Locate 22, 47: Print Using "###"; draftB%(i, 60)
            Locate 22, 52: Print Using "###"; draftB%(i, 61)
            Locate 23, 27: Print Using "###"; draftB%(i, 62)
            Locate 23, 32: Print Using "###"; draftB%(i, 63)
            Locate 23, 37: Print Using "###"; draftB%(i, 64)
            Locate 23, 42: Print Using "###"; draftB%(i, 65)
            Locate 23, 47: Print Using "###"; draftB%(i, 66)
            Locate 23, 52: Print Using "###"; draftB%(i, 67)
            Locate 24, 27: Print Using "###"; draftB%(i, 68);
            Locate 24, 32: Print Using "###"; draftB%(i, 69);
            Locate 24, 37: Print Using "###"; draftB%(i, 70);
            Locate 24, 42: Print Using "###"; draftB%(i, 71);
            Locate 24, 47: Print Using "###"; draftB%(i, 72);
            Locate 24, 52: Print Using "###"; draftB%(i, 73);
            Locate 25, 27: Print Using "###"; draftB%(i, 74);
            Locate 25, 32: Print Using "###"; draftB%(i, 75);
            Locate 25, 37: Print Using "###"; draftB%(i, 76);
            Locate 25, 42: Print Using "###"; draftB%(i, 77);
            Locate 25, 47: Print Using "###"; draftB%(i, 78);
            Locate 25, 52: Print Using "###"; draftB%(i, 79);

            BName:
            Locate 3, 7
            Max = 15
            Call TextIn(statB$(i), Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo BName:
            If XCode = 2 GoTo bDone

            BATS:
            Locate 3, 30
            Max = 2
            Call TextIn(BATS$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo BName:
            If XCode = 2 GoTo bDone

            BActive:
            Locate 3, 44
            Max = 1
            Call TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo BATS:
            If XCode = 2 GoTo bDone

            BINJ:
            Locate 3, 62
            Max = 3
            inj# = inputINJ%(i)
            Call NumIn(inputINJ%(i), Max, XCode, FG, BG)
            inputINJ%(i) = Int(inj#)
            Locate 3, 62: Print Using "###"; inputINJ%(i)
            If XCode = 1 GoTo BActive:
            If XCode = 2 GoTo bDone

            BGAMES:
            Locate 6, 12
            Max = 3
            draftB# = draftB%(i, 3)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 3) = Int(draftB#)
            Locate 6, 12: Print Using "###"; draftB%(i, 3)
            If XCode = 1 GoTo BINJ:
            If XCode = 2 GoTo bDone

            BAB:
            Locate 6, 17
            Max = 3
            draftB# = draftB%(i, 4)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 4) = Int(draftB#)
            Locate 6, 17: Print Using "###"; draftB%(i, 4)
            If XCode = 1 GoTo BGAMES:
            If XCode = 2 GoTo bDone

            BR:
            Locate 6, 22
            Max = 3
            draftB# = draftB%(i, 5)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 5) = Int(draftB#)
            Locate 6, 22: Print Using "###"; draftB%(i, 5)
            If XCode = 1 GoTo BAB:
            If XCode = 2 GoTo bDone

            BH:
            Locate 6, 27
            Max = 3
            draftB# = draftB%(i, 6)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 6) = Int(draftB#)
            Locate 6, 27: Print Using "###"; draftB%(i, 6)
            If XCode = 1 GoTo BR:
            If XCode = 2 GoTo bDone

            B2B:
            Locate 6, 32
            Max = 2
            draftB# = draftB%(i, 7)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 7) = Int(draftB#)
            Locate 6, 32: Print Using "##"; draftB%(i, 7)
            If XCode = 1 GoTo BH:
            If XCode = 2 GoTo bDone

            B3B:
            Locate 6, 36
            Max = 2
            draftB# = draftB%(i, 8)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 8) = Int(draftB#)
            Locate 6, 36: Print Using "##"; draftB%(i, 8)
            If XCode = 1 GoTo B2B:
            If XCode = 2 GoTo bDone

            bHR:
            Locate 6, 40
            Max = 2
            draftB# = draftB%(i, 9)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 9) = Int(draftB#)
            Locate 6, 40: Print Using "##"; draftB%(i, 9)
            If XCode = 1 GoTo B3B:
            If XCode = 2 GoTo bDone

            BRBI:
            Locate 6, 44
            Max = 3
            draftB# = draftB%(i, 10)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 10) = Int(draftB#)
            Locate 6, 44: Print Using "###"; draftB%(i, 10)
            If XCode = 1 GoTo bHR:
            If XCode = 2 GoTo bDone

            bBB:
            Locate 6, 49
            Max = 3
            draftB# = draftB%(i, 11)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 11) = Int(draftB#)
            Locate 6, 49: Print Using "###"; draftB%(i, 11)
            If XCode = 1 GoTo BRBI:
            If XCode = 2 GoTo bDone

            bKO:
            Locate 6, 54
            Max = 3
            draftB# = draftB%(i, 12)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 12) = Int(draftB#)
            Locate 6, 54: Print Using "###"; draftB%(i, 12)
            If XCode = 1 GoTo bBB:
            If XCode = 2 GoTo bDone

            bSB:
            Locate 6, 59
            Max = 3
            draftB# = draftB%(i, 13)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 13) = Int(draftB#)
            Locate 6, 59: Print Using "###"; draftB%(i, 13)
            If XCode = 1 GoTo bKO:
            If XCode = 2 GoTo bDone

            bCS:
            Locate 6, 64
            Max = 2
            draftB# = draftB%(i, 14)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 14) = Int(draftB#)
            Locate 6, 64: Print Using "##"; draftB%(i, 14)
            If XCode = 1 GoTo bSB:
            If XCode = 2 GoTo bDone

            BSH:
            Locate 6, 68
            Max = 2
            draftB# = draftB%(i, 15)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 15) = Int(draftB#)
            Locate 6, 68: Print Using "##"; draftB%(i, 15)
            If XCode = 1 GoTo bCS:
            If XCode = 2 GoTo bDone

            BLAB:
            Locate 7, 17
            Max = 3
            draftB# = draftB%(i, 42)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 42) = Int(draftB#)
            Locate 7, 17: Print Using "###"; draftB%(i, 42)
            If XCode = 1 GoTo BSH:
            If XCode = 2 GoTo bDone

            BLH:
            Locate 7, 27
            Max = 3
            draftB# = draftB%(i, 43)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 43) = Int(draftB#)
            Locate 7, 27: Print Using "###"; draftB%(i, 43)
            If XCode = 1 GoTo BLAB:
            If XCode = 2 GoTo bDone

            BL2B:
            Locate 7, 32
            Max = 2
            draftB# = draftB%(i, 44)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 44) = Int(draftB#)
            Locate 7, 32: Print Using "##"; draftB%(i, 44)
            If XCode = 1 GoTo BLH:
            If XCode = 2 GoTo bDone

            BL3B:
            Locate 7, 36
            Max = 2
            draftB# = draftB%(i, 45)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 45) = Int(draftB#)
            Locate 7, 36: Print Using "##"; draftB%(i, 45)
            If XCode = 1 GoTo BL2B:
            If XCode = 2 GoTo bDone

            BLHR:
            Locate 7, 40
            Max = 2
            draftB# = draftB%(i, 46)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 46) = Int(draftB#)
            Locate 7, 40: Print Using "##"; draftB%(i, 46)
            If XCode = 1 GoTo BL3B:
            If XCode = 2 GoTo bDone

            bLBB:
            Locate 7, 49
            Max = 3
            draftB# = draftB%(i, 47)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 47) = Int(draftB#)
            Locate 7, 49: Print Using "###"; draftB%(i, 47)
            If XCode = 1 GoTo BLHR:
            If XCode = 2 GoTo bDone

            BLKO:
            Locate 7, 54
            Max = 3
            draftB# = draftB%(i, 48)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 48) = Int(draftB#)
            Locate 7, 54: Print Using "###"; draftB%(i, 48)
            If XCode = 1 GoTo bLBB:
            If XCode = 2 GoTo bDone

            BRAB:
            Locate 8, 17
            Max = 3
            draftB# = draftB%(i, 49)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 49) = Int(draftB#)
            Locate 8, 17: Print Using "###"; draftB%(i, 49)
            If XCode = 1 GoTo BLKO:
            If XCode = 2 GoTo bDone

            BRH:
            Locate 8, 27
            Max = 3
            draftB# = draftB%(i, 50)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 50) = Int(draftB#)
            Locate 8, 27: Print Using "###"; draftB%(i, 50)
            If XCode = 1 GoTo BRAB:
            If XCode = 2 GoTo bDone

            BR2B:
            Locate 8, 32
            Max = 2
            draftB# = draftB%(i, 51)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 51) = Int(draftB#)
            Locate 8, 32: Print Using "##"; draftB%(i, 51)
            If XCode = 1 GoTo BRH:
            If XCode = 2 GoTo bDone

            BR3B:
            Locate 8, 36
            Max = 2
            draftB# = draftB%(i, 52)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 52) = Int(draftB#)
            Locate 8, 36: Print Using "##"; draftB%(i, 52)
            If XCode = 1 GoTo BR2B:
            If XCode = 2 GoTo bDone

            BRHR:
            Locate 8, 40
            Max = 2
            draftB# = draftB%(i, 53)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 53) = Int(draftB#)
            Locate 8, 40: Print Using "##"; draftB%(i, 53)
            If XCode = 1 GoTo BR3B:
            If XCode = 2 GoTo bDone

            bRBB:
            Locate 8, 49
            Max = 3
            draftB# = draftB%(i, 54)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 54) = Int(draftB#)
            Locate 8, 49: Print Using "###"; draftB%(i, 54)
            If XCode = 1 GoTo BRHR:
            If XCode = 2 GoTo bDone

            BRKO:
            Locate 8, 54
            Max = 3
            draftB# = draftB%(i, 55)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 55) = Int(draftB#)
            Locate 8, 54: Print Using "###"; draftB%(i, 55)
            If XCode = 1 GoTo bRBB:
            If XCode = 2 GoTo bDone

            BP1:
            Locate 12, 31
            Max = 2
            draftB# = draftB%(i, 22)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 22) = Int(draftB#)
            Locate 12, 31: Print Using "##"; draftB%(i, 22)
            If XCode = 1 GoTo BRKO:
            If XCode = 2 GoTo bDone

            ' STOP HERE

            BF1:
            Locate 12, 36
            Max = 4
            draftB# = draftB%(i, 26)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 26) = Int(draftB#)
            Locate 12, 36: Print Using "####"; draftB%(i, 26)
            If XCode = 1 GoTo BP1:
            If XCode = 2 GoTo bDone

            bA1:
            Locate 12, 43
            Max = 1
            draftB# = draftB%(i, 33)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 33) = Int(draftB#)
            Locate 12, 43: Print Using "#"; draftB%(i, 33)
            If XCode = 1 GoTo BF1:
            If XCode = 2 GoTo bDone

            BR1:
            Locate 12, 47
            Max = 1
            draftB# = draftB%(i, 34)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 34) = Int(draftB#)
            Locate 12, 47: Print Using "#"; draftB%(i, 34)
            If XCode = 1 GoTo bA1:
            If XCode = 2 GoTo bDone

            BP2:
            Locate 13, 31
            Max = 2
            draftB# = draftB%(i, 23)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 23) = Int(draftB#)
            Locate 13, 31: Print Using "##"; draftB%(i, 23)
            If XCode = 1 GoTo BR1:
            If XCode = 2 GoTo bDone

            BF2:
            Locate 13, 36
            Max = 4
            draftB# = draftB%(i, 27)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 27) = Int(draftB#)
            Locate 13, 36: Print Using "####"; draftB%(i, 27)
            If XCode = 1 GoTo BP2:
            If XCode = 2 GoTo bDone

            bA2:
            Locate 13, 43
            Max = 1
            draftB# = draftB%(i, 35)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 35) = Int(draftB#)
            Locate 13, 43: Print Using "#"; draftB%(i, 35)
            If XCode = 1 GoTo BF2:
            If XCode = 2 GoTo bDone

            BR2:
            Locate 13, 47
            Max = 1
            draftB# = draftB%(i, 36)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 36) = Int(draftB#)
            Locate 13, 47: Print Using "#"; draftB%(i, 36)
            If XCode = 1 GoTo bA2:
            If XCode = 2 GoTo bDone

            BP3:
            Locate 14, 31
            Max = 2
            draftB# = draftB%(i, 24)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 24) = Int(draftB#)
            Locate 14, 31: Print Using "##"; draftB%(i, 24)
            If XCode = 1 GoTo BR2:
            If XCode = 2 GoTo bDone

            BF3:
            Locate 14, 36
            Max = 4
            draftB# = draftB%(i, 28)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 28) = Int(draftB#)
            Locate 14, 36: Print Using "####"; draftB%(i, 28)
            If XCode = 1 GoTo BP3:
            If XCode = 2 GoTo bDone

            bA3:
            Locate 14, 43
            Max = 1
            draftB# = draftB%(i, 37)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 37) = Int(draftB#)
            Locate 14, 43: Print Using "#"; draftB%(i, 37)
            If XCode = 1 GoTo BF3:
            If XCode = 2 GoTo bDone

            BR3:
            Locate 14, 47
            Max = 1
            draftB# = draftB%(i, 38)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 38) = Int(draftB#)
            Locate 14, 47: Print Using "#"; draftB%(i, 38)
            If XCode = 1 GoTo bA3:
            If XCode = 2 GoTo bDone

            BP4:
            Locate 15, 31
            Max = 2
            draftB# = draftB%(i, 25)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 25) = Int(draftB#)
            Locate 15, 31: Print Using "##"; draftB%(i, 25)
            If XCode = 1 GoTo BR3:
            If XCode = 2 GoTo bDone

            BF4:
            Locate 15, 36
            Max = 4
            draftB# = draftB%(i, 29)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 29) = Int(draftB#)
            Locate 15, 36: Print Using "####"; draftB%(i, 29)
            If XCode = 1 GoTo BP4:
            If XCode = 2 GoTo bDone

            bA4:
            Locate 15, 43
            Max = 1
            draftB# = draftB%(i, 39)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 39) = Int(draftB#)
            Locate 15, 43: Print Using "#"; draftB%(i, 39)
            If XCode = 1 GoTo BF4:
            If XCode = 2 GoTo bDone

            bR4:
            Locate 15, 47
            Max = 1
            draftB# = draftB%(i, 40)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 40) = Int(draftB#)
            Locate 15, 47: Print Using "#"; draftB%(i, 40)
            If XCode = 1 GoTo bA4:
            If XCode = 2 GoTo bDone

            bRUN:
            Locate 17, 23
            Max = 1
            draftB# = draftB%(i, 16)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 16) = Int(draftB#)
            Locate 17, 23: Print Using "#"; draftB%(i, 16)
            If XCode = 1 GoTo bR4:
            If XCode = 2 GoTo bDone

            bBUNT:
            Locate 17, 39
            Max = 1
            draftB# = draftB%(i, 30)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 30) = Int(draftB#)
            Locate 17, 39: Print Using "#"; draftB%(i, 30)
            If XCode = 1 GoTo bRUN:
            If XCode = 2 GoTo bDone

            bGA:
            Locate 17, 49
            Max = 2
            draftB# = draftB%(i, 18)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 18) = Int(draftB#)
            Locate 17, 49: Print Using "##"; draftB%(i, 18)
            If XCode = 1 GoTo bBUNT:
            If XCode = 2 GoTo bDone

            bLA:
            Locate 17, 67
            Max = 3
            draftB# = draftB%(i, 29)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 20) = Int(draftB#)
            Locate 17, 67: Print Using "###"; draftB%(i, 20)
            If XCode = 1 GoTo bGA:
            If XCode = 2 GoTo bDone

            bLLBA:
            Locate 22, 27
            Max = 3
            draftB# = draftB%(i, 56)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 56) = Int(draftB#)
            Locate 22, 27: Print Using "###"; draftB%(i, 56)
            If XCode = 1 GoTo bLA:
            If XCode = 2 GoTo bDone

            bLL2B:
            Locate 22, 32
            Max = 3
            draftB# = draftB%(i, 57)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 57) = Int(draftB#)
            Locate 22, 32: Print Using "###"; draftB%(i, 57)
            If XCode = 1 GoTo bLLBA:
            If XCode = 2 GoTo bDone

            bLL3B:
            Locate 22, 37
            Max = 3
            draftB# = draftB%(i, 58)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 58) = Int(draftB#)
            Locate 22, 37: Print Using "###"; draftB%(i, 58)
            If XCode = 1 GoTo bLL2B:
            If XCode = 2 GoTo bDone

            bLLHR:
            Locate 22, 42
            Max = 3
            draftB# = draftB%(i, 59)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 59) = Int(draftB#)
            Locate 22, 42: Print Using "###"; draftB%(i, 59)
            If XCode = 1 GoTo bLL3B:
            If XCode = 2 GoTo bDone

            bLLBB:
            Locate 22, 47
            Max = 3
            draftB# = draftB%(i, 60)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 60) = Int(draftB#)
            Locate 22, 47: Print Using "###"; draftB%(i, 60)
            If XCode = 1 GoTo bLLHR:
            If XCode = 2 GoTo bDone

            bLLKO:
            Locate 22, 52
            Max = 3
            draftB# = draftB%(i, 61)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 61) = Int(draftB#)
            Locate 22, 52: Print Using "###"; draftB%(i, 61)
            If XCode = 1 GoTo bLLBB:
            If XCode = 2 GoTo bDone

            bLRBA:
            Locate 23, 27
            Max = 3
            draftB# = draftB%(i, 62)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 62) = Int(draftB#)
            Locate 23, 27: Print Using "###"; draftB%(i, 62)
            If XCode = 1 GoTo bLLKO:
            If XCode = 2 GoTo bDone

            bLR2B:
            Locate 23, 32
            Max = 3
            draftB# = draftB%(i, 63)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 63) = Int(draftB#)
            Locate 23, 32: Print Using "###"; draftB%(i, 63)
            If XCode = 1 GoTo bLRBA:
            If XCode = 2 GoTo bDone

            bLR3B:
            Locate 23, 37
            Max = 3
            draftB# = draftB%(i, 64)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 64) = Int(draftB#)
            Locate 23, 37: Print Using "###"; draftB%(i, 64)
            If XCode = 1 GoTo bLR2B:
            If XCode = 2 GoTo bDone

            bLRHR:
            Locate 23, 42
            Max = 3
            draftB# = draftB%(i, 65)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 65) = Int(draftB#)
            Locate 23, 42: Print Using "###"; draftB%(i, 65)
            If XCode = 1 GoTo bLR3B:
            If XCode = 2 GoTo bDone

            bLRBB:
            Locate 23, 47
            Max = 3
            draftB# = draftB%(i, 66)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 66) = Int(draftB#)
            Locate 23, 47: Print Using "###"; draftB%(i, 66)
            If XCode = 1 GoTo bLRHR:
            If XCode = 2 GoTo bDone

            bLRKO:
            Locate 23, 52
            Max = 3
            draftB# = draftB%(i, 67)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 67) = Int(draftB#)
            Locate 23, 52: Print Using "###"; draftB%(i, 67)
            If XCode = 1 GoTo bLRBB:
            If XCode = 2 GoTo bDone

            bRLBA:
            Locate 24, 27
            Max = 3
            draftB# = draftB%(i, 68)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 68) = Int(draftB#)
            Locate 24, 27: Print Using "###"; draftB%(i, 68);
            If XCode = 1 GoTo bLRKO:
            If XCode = 2 GoTo bDone

            bRL2B:
            Locate 24, 32
            Max = 3
            draftB# = draftB%(i, 69)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 69) = Int(draftB#)
            Locate 24, 32: Print Using "###"; draftB%(i, 69);
            If XCode = 1 GoTo bRLBA:
            If XCode = 2 GoTo bDone

            bRL3B:
            Locate 24, 37
            Max = 3
            draftB# = draftB%(i, 70)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 70) = Int(draftB#)
            Locate 24, 37: Print Using "###"; draftB%(i, 70);
            If XCode = 1 GoTo bRL2B:
            If XCode = 2 GoTo bDone

            bRLHR:
            Locate 24, 42
            Max = 3
            draftB# = draftB%(i, 71)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 71) = Int(draftB#)
            Locate 24, 42: Print Using "###"; draftB%(i, 71);
            If XCode = 1 GoTo bRL3B:
            If XCode = 2 GoTo bDone

            bRLBB:
            Locate 24, 47
            Max = 3
            draftB# = draftB%(i, 72)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 72) = Int(draftB#)
            Locate 24, 47: Print Using "###"; draftB%(i, 72);
            If XCode = 1 GoTo bRLHR:
            If XCode = 2 GoTo bDone

            bRLKO:
            Locate 24, 52
            Max = 3
            draftB# = draftB%(i, 73)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 73) = Int(draftB#)
            Locate 24, 52: Print Using "###"; draftB%(i, 73);
            If XCode = 1 GoTo bRLBB:
            If XCode = 2 GoTo bDone

            bRRBA:
            Locate 25, 27
            Max = 3
            draftB# = draftB%(i, 74)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 74) = Int(draftB#)
            Locate 25, 27: Print Using "###"; draftB%(i, 74);
            If XCode = 1 GoTo bRLKO:
            If XCode = 2 GoTo bDone

            bRR2B:
            Locate 25, 32
            Max = 3
            draftB# = draftB%(i, 75)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 75) = Int(draftB#)
            Locate 25, 32: Print Using "###"; draftB%(i, 75);
            If XCode = 1 GoTo bRRBA:
            If XCode = 2 GoTo bDone

            bRR3B:
            Locate 25, 37
            Max = 3
            draftB# = draftB%(i, 76)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 76) = Int(draftB#)
            Locate 25, 37: Print Using "###"; draftB%(i, 76);
            If XCode = 1 GoTo bRR2B:
            If XCode = 2 GoTo bDone

            bRRHR:
            Locate 25, 42
            Max = 3
            draftB# = draftB%(i, 77)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 77) = Int(draftB#)
            Locate 25, 42: Print Using "###"; draftB%(i, 77);
            If XCode = 1 GoTo bRR3B:
            If XCode = 2 GoTo bDone

            bRRBB:
            Locate 25, 47
            Max = 3
            draftB# = draftB%(i, 78)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 78) = Int(draftB#)
            Locate 25, 47: Print Using "###"; draftB%(i, 78);
            If XCode = 1 GoTo bRRHR:
            If XCode = 2 GoTo bDone

            bRRKO:
            Locate 25, 52
            Max = 3
            draftB# = draftB%(i, 79)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            draftB%(i, 79) = Int(draftB#)
            Locate 25, 52: Print Using "###"; draftB%(i, 79);
            If XCode = 1 GoTo bRRBB:

            bDone:
            Color 15, 1
            Locate 25, 1
            Print "Is this okay? (Yes/No) ";
            X$ = ""
            Call YesNoB(X$, XCode, FG, BG)
            Locate , 1
            If XCode = 1 GoTo bRRKO
            If X$ = "N" GoTo BName
            If X$ <> "Y" GoTo bDone 'insist on either Y or N or up-arrow

            draftB%(i, 0) = 0
            BATS$ = _Trim$(BATS$)
            If UCase$(BATS$) = "L" Then draftB%(i, 0) = -1
            If UCase$(BATS$) = "R" Then draftB%(i, 0) = 1
            If UCase$(BATS$) = "LP" Then draftB%(i, 0) = -2
            If UCase$(BATS$) = "RP" Then draftB%(i, 0) = 2

            ACT$ = _Trim$(ACT$)
            draftB%(i, 31) = 0
            If UCase$(ACT$) = "N" Then draftB%(i, 31) = 1

        End If

    Loop Until i = 0

End Sub

'----------------------------------------
'       EDITTEAM subroutine
'----------------------------------------
'...explanation...
Sub EDITTEAM (YN$, T$, NN$, S$, M$, teamIdx)

    Shared K9&()
    Shared ratings%(), inputSA%()

    Do

        Color 15, 1
        Cls

        Locate 1, 36: Print "TEAM DATA"
        Locate 3, 1: Print "TEAM NUMBER:"
        Locate 5, 1: Print "LEAGUE BATTING AVERAGE:"
        Locate 6, 1: Print "LEAGUE STRIKEOUT %    :"
        Locate 7, 1: Print "LEAGUE WALK PERCENTAGE:"
        Locate 8, 1: Print "LEAGUE HOME RUN %     :"
        Locate 10, 1: Print "DBL PLAYS/GAME X 100  :"
        Locate 14, 1: Print "PARK NAME:"
        Locate 15, 1: Print "PARK ID  :"
        Locate 16, 1: Print "DOUBLES RATING  :"
        Locate 17, 1: Print "TRIPLES RATING  :"
        Locate 18, 1: Print "HOME RUN RATING :"
        Locate 19, 1: Print "FOUL BALL RATING:"
        Locate 21, 1: Print "OUTDOOR(0)/INDOOR(1):"
        Locate 22, 1: Print "GRASS(0)/TURF(1)    :"
        Locate 24, 1: Print "MANAGER :";
        Locate 18, 29: Print "TEXT COLOR      :"
        Locate 19, 29: Print "BACKGROUND COLOR:"
        Locate 3, 25: Print "DISK ID:"
        Locate 3, 44: Print "TEAM NAME:"
        Locate 4, 44: Print "ABBREVIATION:"
        Locate 6, 51: Print "SPLIT STATS LEAGUE DATA"
        Locate 7, 50: Print "BAVG   K%  BB%  HR%  2B%  3B%"
        Locate 8, 38: Print "LHB vs LHP"
        Locate 9, 38: Print "LHB vs RHP"
        Locate 10, 38: Print "RHB vs LHP"
        Locate 11, 38: Print "RHB vs RHP"

        ' INITIALIZE THE FIELDS
        FG = 0
        BG = 7
        Color FG, BG
        'COLR = 112

        Locate 3, 14: Print Using "###"; teamIdx
        Locate 3, 33: Print YN$
        Locate 3, 55: Print T$
        Locate 4, 58: Print NN$

        Locate 5, 25: Print Using "###"; ratings%(1)
        Locate 6, 25: Print Using "###"; ratings%(2)
        Locate 7, 25: Print Using "###"; ratings%(3)
        Locate 8, 25: Print Using "###"; ratings%(4)
        Locate 10, 25: Print Using "###"; ratings%(5)
        Locate 14, 12: Print S$
        Locate 15, 12: Print Using "###"; ratings%(13)
        Locate 16, 19: Print Using "###"; ratings%(6)
        Locate 17, 19: Print Using "###"; ratings%(7)
        Locate 18, 19: Print Using "######"; K9&(0)
        Locate 19, 19: Print Using "###"; ratings%(8)
        Locate 21, 25: Print Using "#"; ratings%(9)
        Locate 22, 25: Print Using "#"; ratings%(10)
        Locate 24, 11: Print M$;
        Locate 18, 46: Print Using "##"; ratings%(11)
        Locate 19, 46: Print Using "##"; ratings%(12)
        Locate 8, 50: Print Using "###"; inputSA%(1)
        Locate 8, 55: Print Using "###"; inputSA%(2)
        Locate 8, 60: Print Using "###"; inputSA%(3)
        Locate 8, 65: Print Using "###"; inputSA%(4)
        Locate 8, 70: Print Using "###"; inputSA%(5)
        Locate 8, 75: Print Using "###"; inputSA%(6)
        Locate 9, 50: Print Using "###"; inputSA%(7)
        Locate 9, 55: Print Using "###"; inputSA%(8)
        Locate 9, 60: Print Using "###"; inputSA%(9)
        Locate 9, 65: Print Using "###"; inputSA%(10)
        Locate 9, 70: Print Using "###"; inputSA%(11)
        Locate 9, 75: Print Using "###"; inputSA%(12)
        Locate 10, 50: Print Using "###"; inputSA%(13)
        Locate 10, 55: Print Using "###"; inputSA%(14)
        Locate 10, 60: Print Using "###"; inputSA%(15)
        Locate 10, 65: Print Using "###"; inputSA%(16)
        Locate 10, 70: Print Using "###"; inputSA%(17)
        Locate 10, 75: Print Using "###"; inputSA%(18)
        Locate 11, 50: Print Using "###"; inputSA%(19)
        Locate 11, 55: Print Using "###"; inputSA%(20)
        Locate 11, 60: Print Using "###"; inputSA%(21)
        Locate 11, 65: Print Using "###"; inputSA%(22)
        Locate 11, 70: Print Using "###"; inputSA%(23)
        Locate 11, 75: Print Using "###"; inputSA%(24)
        Locate 25, 1: Print "ESC-WILL END EDITING";

        HR$ = _Trim$(Str$(K9&(0)))

        TeamNum:
        Locate 3, 14
        Max = 3
        T# = teamIdx
        Call NumIn(T#, Max, XCode, FG, BG)
        teamIdx = Int(T#)
        Locate 3, 14: Print Using "###"; teamIdx
        If XCode = 1 GoTo TeamNum:
        If XCode = 2 GoTo Done

        DiskID:
        Locate 3, 33
        Max = 3
        Call TextIn(YN$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo TeamNum:
        If XCode = 2 GoTo Done

        TeamName:
        Locate 3, 55
        Max = 15
        Call TextIn(T$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo DiskID:
        If XCode = 2 GoTo Done

        Abbrev:
        Locate 4, 58
        Max = 5
        Call TextIn(NN$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo TeamName:
        If XCode = 2 GoTo Done

        LgBA:
        Locate 5, 25
        Max = 3
        L# = ratings%(1)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(1) = Int(L#)
        Locate 5, 25: Print Using "###"; ratings%(1)
        If XCode = 1 GoTo Abbrev:
        If XCode = 2 GoTo Done

        LgKO:
        Locate 6, 25
        Max = 3
        L# = ratings%(2)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(2) = Int(L#)
        Locate 6, 25: Print Using "###"; ratings%(2)
        If XCode = 1 GoTo LgBA:
        If XCode = 2 GoTo Done

        LgBB:
        Locate 7, 25
        Max = 3
        L# = ratings%(3)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(3) = Int(L#)
        Locate 7, 25: Print Using "###"; ratings%(3)
        If XCode = 1 GoTo LgKO:
        If XCode = 2 GoTo Done

        LgHR:
        Locate 8, 25
        Max = 3
        L# = ratings%(4)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(4) = Int(L#)
        Locate 8, 25: Print Using "###"; ratings%(4)
        If XCode = 1 GoTo LgBB:
        If XCode = 2 GoTo Done

        LgDP:
        Locate 10, 25
        Max = 3
        L# = ratings%(5)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(5) = Int(L#)
        Locate 10, 25: Print Using "###"; ratings%(5)
        If XCode = 1 GoTo LgHR:
        If XCode = 2 GoTo Done

        ParkName:
        Locate 14, 12
        Max = 40
        Call TextIn(S$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo LgDP:
        If XCode = 2 GoTo Done

        ParkID:
        Locate 15, 12
        Max = 3
        L# = ratings%(13)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(13) = Int(L#)
        Locate 15, 12: Print Using "###"; ratings%(13)
        If XCode = 1 GoTo ParkName:
        If XCode = 2 GoTo Done

        DoubleRating:
        Locate 16, 19
        Max = 3
        L# = ratings%(6)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(6) = Int(L#)
        Locate 16, 19: Print Using "###"; ratings%(6)
        If XCode = 1 GoTo ParkID:
        If XCode = 2 GoTo Done

        TripleRating:
        Locate 17, 19
        Max = 3
        L# = ratings%(7)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(7) = Int(L#)
        Locate 17, 19: Print Using "###"; ratings%(7)
        If XCode = 1 GoTo DoubleRating:
        If XCode = 2 GoTo Done

        Homer:
        Locate 18, 19
        Max = 6
        Call TextIn(HR$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo TripleRating:
        If XCode = 2 GoTo Done

        Foul:
        Locate 19, 19
        Max = 3
        L# = ratings%(8)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(8) = Int(L#)
        Locate 19, 19: Print Using "###"; ratings%(8)
        If XCode = 1 GoTo Homer:
        If XCode = 2 GoTo Done

        Outdoor:
        Locate 21, 25
        Max = 1
        L# = ratings%(9)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(9) = Int(L#)
        Locate 21, 25: Print Using "#"; ratings%(9)
        If XCode = 1 GoTo Foul:
        If XCode = 2 GoTo Done

        Grass:
        Locate 22, 25
        Max = 1
        L# = ratings%(10)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(10) = Int(L#)
        Locate 22, 25: Print Using "#"; ratings%(10)
        If XCode = 1 GoTo Outdoor:
        If XCode = 2 GoTo Done

        Manager:
        Locate 24, 11
        Max = 15
        Call TextIn(M$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo Grass:
        If XCode = 2 GoTo Done

        Text:
        Locate 18, 46
        Max = 2
        L# = ratings%(11)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(11) = Int(L#)
        Locate 18, 46: Print Using "##"; ratings%(11)
        If XCode = 1 GoTo Manager:
        If XCode = 2 GoTo Done

        Bgrnd:
        Locate 19, 46
        Max = 2
        L# = ratings%(12)
        Call NumIn(L#, Max, XCode, FG, BG)
        ratings%(12) = Int(L#)
        Locate 19, 46: Print Using "##"; ratings%(12)
        If XCode = 1 GoTo Text:
        If XCode = 2 GoTo Done

        Locate 20, 29: Color ratings%(11), ratings%(12): Print "SAMPLE TEXT": Color 0, 7

        LLBA:
        Locate 8, 50
        Max = 3
        SA# = inputSA%(1)
        Call NumIn(inputSA%(1), Max, XCode, FG, BG)
        inputSA%(1) = Int(SA#)
        Locate 8, 50: Print Using "###"; inputSA%(1)
        If XCode = 1 GoTo Bgrnd:
        If XCode = 2 GoTo Done

        LL2B:
        Locate 8, 55
        Max = 3
        SA# = inputSA%(2)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(2) = Int(SA#)
        Locate 8, 55: Print Using "###"; inputSA%(2)
        If XCode = 1 GoTo LLBA:
        If XCode = 2 GoTo Done

        LL3B:
        Locate 8, 60
        Max = 3
        SA# = inputSA%(3)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(3) = Int(SA#)
        Locate 8, 60: Print Using "###"; inputSA%(3)
        If XCode = 1 GoTo LL2B:
        If XCode = 2 GoTo Done

        LLHR:
        Locate 8, 65
        Max = 3
        SA# = inputSA%(4)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(4) = Int(SA#)
        Locate 8, 65: Print Using "###"; inputSA%(4)
        If XCode = 1 GoTo LL3B:
        If XCode = 2 GoTo Done

        LLBB:
        Locate 8, 70
        Max = 3
        SA# = inputSA%(5)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(5) = Int(SA#)
        Locate 8, 70: Print Using "###"; inputSA%(5)
        If XCode = 1 GoTo LLHR:
        If XCode = 2 GoTo Done

        LLKO:
        Locate 8, 75
        Max = 3
        SA# = inputSA%(6)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(6) = Int(SA#)
        Locate 8, 75: Print Using "###"; inputSA%(6)
        If XCode = 1 GoTo LLBB:
        If XCode = 2 GoTo Done

        LRBA:
        Locate 9, 50
        Max = 3
        SA# = inputSA%(7)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(7) = Int(SA#)
        Locate 9, 50: Print Using "###"; inputSA%(7)
        If XCode = 1 GoTo LLKO:
        If XCode = 2 GoTo Done

        LR2B:
        Locate 9, 55
        Max = 3
        SA# = inputSA%(8)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(8) = Int(SA#)
        Locate 9, 55: Print Using "###"; inputSA%(8)
        If XCode = 1 GoTo LRBA:
        If XCode = 2 GoTo Done

        LR3B:
        Locate 9, 60
        Max = 3
        SA# = inputSA%(9)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(9) = Int(SA#)
        Locate 9, 60: Print Using "###"; inputSA%(9)
        If XCode = 1 GoTo LR2B:
        If XCode = 2 GoTo Done

        LRHR:
        Locate 9, 65
        Max = 3
        SA# = inputSA%(10)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(10) = Int(SA#)
        Locate 9, 65: Print Using "###"; inputSA%(10)
        If XCode = 1 GoTo LR3B:
        If XCode = 2 GoTo Done

        LRBB:
        Locate 9, 70
        Max = 3
        SA# = inputSA%(11)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(11) = Int(SA#)
        Locate 9, 70: Print Using "###"; inputSA%(11)
        If XCode = 1 GoTo LRHR:
        If XCode = 2 GoTo Done

        LRKO:
        Locate 9, 75
        Max = 3
        SA# = inputSA%(12)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(12) = Int(SA#)
        Locate 9, 75: Print Using "###"; inputSA%(12)
        If XCode = 1 GoTo LRBB:
        If XCode = 2 GoTo Done

        RLBA:
        Locate 10, 50
        Max = 3
        SA# = inputSA%(13)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(13) = Int(SA#)
        Locate 10, 50: Print Using "###"; inputSA%(13)
        If XCode = 1 GoTo LRKO:
        If XCode = 2 GoTo Done

        RL2B:
        Locate 10, 55
        Max = 3
        SA# = inputSA%(14)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(14) = Int(SA#)
        Locate 10, 55: Print Using "###"; inputSA%(14)
        If XCode = 1 GoTo RLBA:
        If XCode = 2 GoTo Done

        RL3B:
        Locate 10, 60
        Max = 3
        SA# = inputSA%(15)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(15) = Int(SA#)
        Locate 10, 60: Print Using "###"; inputSA%(15)
        If XCode = 1 GoTo RL2B:
        If XCode = 2 GoTo Done

        RLHR:
        Locate 10, 65
        Max = 3
        SA# = inputSA%(16)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(16) = Int(SA#)
        Locate 10, 65: Print Using "###"; inputSA%(16)
        If XCode = 1 GoTo RL3B:
        If XCode = 2 GoTo Done

        RLBB:
        Locate 10, 70
        Max = 3
        SA# = inputSA%(17)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(17) = Int(SA#)
        Locate 10, 70: Print Using "###"; inputSA%(17)
        If XCode = 1 GoTo RLHR:
        If XCode = 2 GoTo Done

        RLKO:
        Locate 10, 75
        Max = 3
        SA# = inputSA%(18)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(18) = Int(SA#)
        Locate 10, 75: Print Using "###"; inputSA%(18)
        If XCode = 1 GoTo RLBB:
        If XCode = 2 GoTo Done

        RRBA:
        Locate 11, 50
        Max = 3
        SA# = inputSA%(19)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(19) = Int(SA#)
        Locate 11, 50: Print Using "###"; inputSA%(19)
        If XCode = 1 GoTo RLKO:
        If XCode = 2 GoTo Done

        RR2B:
        Locate 11, 55
        Max = 3
        SA# = inputSA%(20)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(20) = Int(SA#)
        Locate 11, 55: Print Using "###"; inputSA%(20)
        If XCode = 1 GoTo RRBA:
        If XCode = 2 GoTo Done

        RR3B:
        Locate 11, 60
        Max = 3
        SA# = inputSA%(21)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(21) = Int(SA#)
        Locate 11, 60: Print Using "###"; inputSA%(21)
        If XCode = 1 GoTo RR2B:
        If XCode = 2 GoTo Done

        RRHR:
        Locate 11, 65
        Max = 3
        SA# = inputSA%(22)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(22) = Int(SA#)
        Locate 11, 65: Print Using "###"; inputSA%(22)
        If XCode = 1 GoTo RR3B:
        If XCode = 2 GoTo Done

        RRBB:
        Locate 11, 70
        Max = 3
        SA# = inputSA%(23)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(23) = Int(SA#)
        Locate 11, 70: Print Using "###"; inputSA%(23)
        If XCode = 1 GoTo RRHR:
        If XCode = 2 GoTo Done

        RRKO:
        Locate 11, 75
        Max = 3
        SA# = inputSA%(24)
        Call NumIn(SA#, Max, XCode, FG, BG)
        inputSA%(24) = Int(SA#)
        Locate 11, 75: Print Using "###"; inputSA%(24)
        If XCode = 1 GoTo RRBB:

        Done:
        'Do
        Locate 25, 1
        'Print "Is this okay? (Y)es/(N)o/(A)bort ";
        Print "Is this okay? (Y)es/(N)o";
        X$ = ""
        Call YesNoB(X$, XCode, FG, BG)
        Locate , 1
        ClearEOL 14, 1
        If XCode = 1 GoTo RRKO

        If X$ = "N" Then GoTo TeamNum
        If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

        validData = 1

        If teamIdx = 0 Then validData = 0
        If T$ = "" Then validData = 0

        If validData = 0 Then
            MsgText$ = "TEAM INDEX OR TEAM NAME NOT COMPLETED. PRESS ANY KEY TO RETURN TO DATA ENTRY"
            BoxWidth = 26
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        End If

    Loop Until validData = 1

    K9&(0) = Val(HR$)

End Sub

'----------------------------------------
'       PITCHEREDIT subroutine
'----------------------------------------
'...explanation...
Sub PITCHEREDIT ()

    Shared statP$()
    Shared draftP%()

    Do
        Color 15, 0:
        Cls
        Print "EDIT/ENTER PITCHERS"
        X% = 2
        For i = 0 To 10
            Locate i + 5, 5: Print Using "##"; i + 1;
            Print " "; statP$(i); Tab(40);
            Print Using "##"; i + 12;
            Print " "; statP$(i + 11)
        Next

        Do
            Locate 22, 1
            Print "# TO EDIT/ENTER (1-22) (0 TO CONTINUE) ";
            Input i
        Loop Until i >= 0 And i <= 22

        If i > 0 Then

            'adjust value for 0 index array
            i = i - 1

            ' INITIALIZE PITCHER SCREEN
            Color 15, 1: Cls
            Locate 1, 1: Print "NAME:"
            Locate 1, 24: Print "THROWS:"
            Locate 1, 36: Print "ACTIVE:"
            Locate 1, 48: Print "INJ LENGTH:"
            Locate 1, 66: Print "HOLD (1-9):"
            Locate 3, 34: Print "PITCHING STATS"
            Locate 4, 11: Print "W   L  SV    G  GS  CG   IP    H   BB    K  WP   ERA  HRA"
            Locate 5, 1: Print "SEASON:"
            Locate 6, 27: Print "G/A:"
            Locate 6, 37: Print "LEAGUE BAVG:"
            Locate 8, 31: Print "SPLIT STATS AGAINST"
            Locate 9, 26: Print "AB    H  2B  3B  HR   BB    K"
            Locate 10, 1: Print "VS LHB:"
            Locate 11, 1: Print "VS RHB:"
            Locate 13, 30: Print "HITTING STATISTICS"
            Locate 14, 9: Print "RUN (1-9):"
            Locate 14, 24: Print "BUNT (1-9):"
            Locate 14, 39: Print "BATTING AVG:"
            Locate 14, 58: Print "FLDG AVG:"
            Locate 15, 14: Print "G   AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
            Locate 16, 1: Print "SEASON:"
            Locate 17, 1: Print "VS LHP:"
            Locate 18, 1: Print "VS RHP:"
            Locate 20, 29: Print "SPLIT STATS LEAGUE DATA"
            Locate 21, 26: Print "BAVG   K%  BB%  HR%  2B%  3B%"
            Locate 22, 13: Print "LHB vs LHP"
            Locate 23, 13: Print "LHB vs RHP"
            Locate 24, 13: Print "RHB vs LHP";
            Locate 25, 13: Print "RHB vs RHP";

            ' INITIALIZE PITCHER FIELDS
            'FG, BG = 112
            FG = 0
            BG = 7

            Locate 1, 7: QPrint PadRight$(statP$(i), 15), FG, BG, -1

            THROW$ = "R"
            If draftP%(i, 0) = -1 Then THROW$ = "L"
            Locate 1, 32: QPrint PadRight$(THROW$, 1), FG, BG, -1

            ACT$ = "Y"
            If draftP%(i, 34) = 1 Then ACT$ = "N"
            Locate 1, 44: QPrint PadRight$(ACT$, 1), FG, BG, -1

            Call PITCHINIT(i, FG, BG)

            PName:
            Locate 1, 7
            CapsOn = 0: NumOnly = 0: Max = 15
            Call TextIn(statP$(i), Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo PName:
            If XCode = 2 GoTo PDone

            THROW:
            Locate 1, 32
            Max = 1
            Call TextIn(THROW$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo PName:
            If XCode = 2 GoTo PDone

            PActive:
            Locate 1, 44
            Max = 1
            Call TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo THROW:
            If XCode = 2 GoTo PDone

            PINJ:
            Locate 1, 60
            Max = 3
            draftP# = draftP%(i, 35)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 35) = Int(draftP#)
            Locate 1, 60: Print Using "###"; draftP%(i, 35)
            If XCode = 1 GoTo PActive:
            If XCode = 2 GoTo PDone

            PHOLD:
            Locate 1, 78
            Max = 1
            draftP# = draftP%(i, 18)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 18) = Int(draftP#)
            Locate 1, 78: Print Using "#"; draftP%(i, 18)
            If XCode = 1 GoTo PINJ:
            If XCode = 2 GoTo PDone

            PWINS:
            Locate 5, 10
            Max = 2
            draftP# = draftP%(i, 1)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 1) = Int(draftP#)
            Locate 5, 10: Print Using "##"; draftP%(i, 1)
            If XCode = 1 GoTo PHOLD:
            If XCode = 2 GoTo PDone

            PLOSSES:
            Locate 5, 14
            Max = 2
            draftP# = draftP%(i, 2)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 2) = Int(draftP#)
            Locate 5, 14: Print Using "##"; draftP%(i, 2)
            If XCode = 1 GoTo PWINS:
            If XCode = 2 GoTo PDone

            PSAVES:
            Locate 5, 18
            Max = 2
            draftP# = draftP%(i, 3)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 3) = Int(draftP#)
            Locate 5, 18: Print Using "##"; draftP%(i, 3)
            If XCode = 1 GoTo PLOSSES:
            If XCode = 2 GoTo PDone

            PGAMES:
            Locate 5, 22
            Max = 3
            draftP# = draftP%(i, 4)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 4) = Int(draftP#)
            Locate 5, 22: Print Using "###"; draftP%(i, 4)
            If XCode = 1 GoTo PSAVES:
            If XCode = 2 GoTo PDone

            PSTARTS:
            Locate 5, 27
            Max = 2
            draftP# = draftP%(i, 5)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 5) = Int(draftP#)
            Locate 5, 27: Print Using "##"; draftP%(i, 5)
            If XCode = 1 GoTo PGAMES:
            If XCode = 2 GoTo PDone

            PCG:
            Locate 5, 31
            Max = 2
            draftP# = draftP%(i, 16)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 16) = Int(draftP#)
            Locate 5, 31: Print Using "##"; draftP%(i, 16)
            If XCode = 1 GoTo PSTARTS:
            If XCode = 2 GoTo PDone

            PIP:
            Locate 5, 35
            Max = 3
            draftP# = draftP%(i, 6)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 6) = Int(draftP#)
            Locate 5, 35: Print Using "###"; draftP%(i, 6)
            If XCode = 1 GoTo PCG:
            If XCode = 2 GoTo PDone

            PH:
            Locate 5, 40
            Max = 3
            draftP# = draftP%(i, 7)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 7) = Int(draftP#)
            Locate 5, 40: Print Using "###"; draftP%(i, 7)
            If XCode = 1 GoTo PIP:
            If XCode = 2 GoTo PDone

            PBB:
            Locate 5, 45
            Max = 3
            draftP# = draftP%(i, 8)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 8) = Int(draftP#)
            Locate 5, 45: Print Using "###"; draftP%(i, 8)
            If XCode = 1 GoTo PH:
            If XCode = 2 GoTo PDone

            PKO:
            Locate 5, 50
            Max = 3
            draftP# = draftP%(i, 9)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 9) = Int(draftP#)
            Locate 5, 50: Print Using "###"; draftP%(i, 9)
            If XCode = 1 GoTo PBB:
            If XCode = 2 GoTo PDone

            PWP:
            Locate 5, 55
            Max = 2
            draftP# = draftP%(i, 33)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 33) = Int(draftP#)
            Locate 5, 55: Print Using "##"; draftP%(i, 33)
            If XCode = 1 GoTo PKO:
            If XCode = 2 GoTo PDone

            PERA:
            Locate 5, 59
            Max = 4
            draftP# = draftP%(i, 10)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 10) = Int(draftP#)
            Locate 5, 59: Print Using "####"; draftP%(i, 10)
            If XCode = 1 GoTo PWP:
            If XCode = 2 GoTo PDone

            PHRA:
            Locate 5, 66
            Max = 2
            draftP# = draftP%(i, 13)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 13) = Int(draftP#)
            Locate 5, 66: Print Using "##"; draftP%(i, 13)
            If XCode = 1 GoTo PERA:
            If XCode = 2 GoTo PDone

            PGA:
            Locate 6, 32
            Max = 2
            draftP# = draftP%(i, 12)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 12) = Int(draftP#)
            Locate 6, 32: Print Using "##"; draftP%(i, 12)
            If XCode = 1 GoTo PHRA:
            If XCode = 2 GoTo PDone

            PLBA:
            Locate 6, 50
            Max = 3
            draftP# = draftP%(i, 14)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 14) = Int(draftP#)
            Locate 6, 50: Print Using "###"; draftP%(i, 14)
            If XCode = 1 GoTo PHRA:
            If XCode = 2 GoTo PDone

            PLAB:
            Locate 10, 25
            Max = 3
            draftP# = draftP%(i, 51)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 51) = Int(draftP#)
            Locate 10, 25: Print Using "###"; draftP%(i, 51)
            If XCode = 1 GoTo PLBA:
            If XCode = 2 GoTo PDone

            PLH:
            Locate 10, 30
            Max = 3
            draftP# = draftP%(i, 52)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 52) = Int(draftP#)
            Locate 10, 30: Print Using "###"; draftP%(i, 52)
            If XCode = 1 GoTo PLAB:
            If XCode = 2 GoTo PDone

            PL2B:
            Locate 10, 35
            Max = 2
            draftP# = draftP%(i, 53)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 53) = Int(draftP#)
            Locate 10, 35: Print Using "##"; draftP%(i, 53)
            If XCode = 1 GoTo PLH:
            If XCode = 2 GoTo PDone

            PL3B:
            Locate 10, 39
            Max = 2
            draftP# = draftP%(i, 54)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 54) = Int(draftP#)
            Locate 10, 39: Print Using "##"; draftP%(i, 54)
            If XCode = 1 GoTo PL2B:
            If XCode = 2 GoTo PDone

            PLHR:
            Locate 10, 43
            Max = 2
            draftP# = draftP%(i, 55)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 55) = Int(draftP#)
            Locate 10, 43: Print Using "##"; draftP%(i, 55)
            If XCode = 1 GoTo PL3B:
            If XCode = 2 GoTo PDone

            PLBB:
            Locate 10, 47
            Max = 3
            draftP# = draftP%(i, 56)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 56) = Int(draftP#)
            Locate 10, 47: Print Using "###"; draftP%(i, 56)
            If XCode = 1 GoTo PLHR:
            If XCode = 2 GoTo PDone

            PLKO:
            Locate 10, 52
            Max = 3
            draftP# = draftP%(i, 57)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 57) = Int(draftP#)
            Locate 10, 52: Print Using "###"; draftP%(i, 57)
            If XCode = 1 GoTo PLBB:
            If XCode = 2 GoTo PDone

            PRAB:
            Locate 11, 25
            Max = 3
            draftP# = draftP%(i, 58)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 58) = Int(draftP#)
            Locate 11, 25: Print Using "###"; draftP%(i, 58)
            If XCode = 1 GoTo PLKO:
            If XCode = 2 GoTo PDone

            PRH:
            Locate 11, 30
            Max = 3
            draftP# = draftP%(i, 59)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 59) = Int(draftP#)
            Locate 11, 30: Print Using "###"; draftP%(i, 59)
            If XCode = 1 GoTo PRAB:
            If XCode = 2 GoTo PDone

            PR2B:
            Locate 11, 35
            Max = 2
            draftP# = draftP%(i, 60)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 60) = Int(draftP#)
            Locate 11, 35: Print Using "##"; draftP%(i, 60)
            If XCode = 1 GoTo PRH:
            If XCode = 2 GoTo PDone

            PR3B:
            Locate 11, 39
            Max = 2
            draftP# = draftP%(i, 61)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 61) = Int(draftP#)
            Locate 11, 39: Print Using "##"; draftP%(i, 61)
            If XCode = 1 GoTo PR2B:
            If XCode = 2 GoTo PDone

            PRHR:
            Locate 11, 43
            Max = 2
            draftP# = draftP%(i, 62)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 62) = Int(draftP#)
            Locate 11, 43: Print Using "##"; draftP%(i, 62)
            If XCode = 1 GoTo PR3B:
            If XCode = 2 GoTo PDone

            PRBB:
            Locate 11, 47
            Max = 3
            draftP# = draftP%(i, 63)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 63) = Int(draftP#)
            Locate 11, 47: Print Using "###"; draftP%(i, 63)
            If XCode = 1 GoTo PRHR:
            If XCode = 2 GoTo PDone

            PRKO:
            Locate 11, 52
            Max = 3
            draftP# = draftP%(i, 64)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 64) = Int(draftP#)
            Locate 11, 52: Print Using "###"; draftP%(i, 64)
            If XCode = 1 GoTo PRBB:
            If XCode = 2 GoTo PDone

            PRUN:
            Locate 14, 20
            Max = 1
            draftP# = draftP%(i, 31)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 31) = Int(draftP#)
            Locate 14, 20: Print Using "#"; draftP%(i, 31)
            If XCode = 1 GoTo PRKO:
            If XCode = 2 GoTo PDone

            PBUNT:
            Locate 14, 36
            Max = 1
            draftP# = draftP%(i, 32)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 32) = Int(draftP#)
            Locate 14, 36: Print Using "#"; draftP%(i, 32)
            If XCode = 1 GoTo PRKO:
            If XCode = 2 GoTo PDone

            PBAVG:
            Locate 14, 52
            Max = 3
            draftP# = draftP%(i, 11)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 11) = Int(draftP#)
            Locate 14, 52: Print Using "###"; draftP%(i, 11)
            If XCode = 1 GoTo PBUNT:
            If XCode = 2 GoTo PDone

            PFAVG:
            Locate 14, 68
            Max = 4
            draftP# = draftP%(i, 17)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 17) = Int(draftP#)
            Locate 14, 68: Print Using "####"; draftP%(i, 17)
            If XCode = 1 GoTo PBAVG:
            If XCode = 2 GoTo PDone

            PBGAMES:
            Locate 16, 12
            Max = 3
            draftP# = draftP%(i, 19)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 19) = Int(draftP#)
            Locate 16, 12: Print Using "###"; draftP%(i, 19)
            If XCode = 1 GoTo PFAVG:
            If XCode = 2 GoTo PDone

            PAB:
            Locate 16, 17
            Max = 3
            draftP# = draftP%(i, 20)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 20) = Int(draftP#)
            Locate 16, 17: Print Using "###"; draftP%(i, 20)
            If XCode = 1 GoTo PBGAMES:
            If XCode = 2 GoTo PDone

            PR:
            Locate 16, 22
            Max = 3
            draftP# = draftP%(i, 21)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 21) = Int(draftP#)
            Locate 16, 22: Print Using "###"; draftP%(i, 21)
            If XCode = 1 GoTo PAB:
            If XCode = 2 GoTo PDone

            PBH:
            Locate 16, 27
            Max = 3
            draftP# = draftP%(i, 22)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 22) = Int(draftP#)
            Locate 16, 27: Print Using "###"; draftP%(i, 22)
            If XCode = 1 GoTo PR:
            If XCode = 2 GoTo PDone

            P2B:
            Locate 16, 32
            Max = 2
            draftP# = draftP%(i, 23)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 23) = Int(draftP#)
            Locate 16, 32: Print Using "##"; draftP%(i, 23)
            If XCode = 1 GoTo PBH:
            If XCode = 2 GoTo PDone

            P3B:
            Locate 16, 36
            Max = 2
            draftP# = draftP%(i, 24)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 24) = Int(draftP#)
            Locate 16, 36: Print Using "##"; draftP%(i, 24)
            If XCode = 1 GoTo P2B:
            If XCode = 2 GoTo PDone

            PHR:
            Locate 16, 40
            Max = 2
            draftP# = draftP%(i, 25)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 25) = Int(draftP#)
            Locate 16, 40: Print Using "##"; draftP%(i, 25)
            If XCode = 1 GoTo P3B:
            If XCode = 2 GoTo PDone

            PRBI:
            Locate 16, 44
            Max = 3
            draftP# = draftP%(i, 26)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 26) = Int(draftP#)
            Locate 16, 44: Print Using "###"; draftP%(i, 26)
            If XCode = 1 GoTo PHR:
            If XCode = 2 GoTo PDone

            PBBB:
            Locate 16, 49
            Max = 3
            draftP# = draftP%(i, 27)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 27) = Int(draftP#)
            Locate 16, 49: Print Using "###"; draftP%(i, 27)
            If XCode = 1 GoTo PRBI:
            If XCode = 2 GoTo PDone

            PBKO:
            Locate 16, 54
            Max = 3
            draftP# = draftP%(i, 28)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 28) = Int(draftP#)
            Locate 16, 54: Print Using "###"; draftP%(i, 28)
            If XCode = 1 GoTo PBBB:
            If XCode = 2 GoTo PDone

            PSB:
            Locate 16, 59
            Max = 3
            draftP# = draftP%(i, 29)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 29) = Int(draftP#)
            Locate 16, 59: Print Using "###"; draftP%(i, 29)
            If XCode = 1 GoTo PBKO:
            If XCode = 2 GoTo PDone

            PCS:
            Locate 16, 64
            Max = 2
            draftP# = draftP%(i, 30)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 30) = Int(draftP#)
            Locate 16, 64: Print Using "##"; draftP%(i, 30)
            If XCode = 1 GoTo PSB:
            If XCode = 2 GoTo PDone

            PSH:
            Locate 16, 68
            Max = 2
            draftP# = draftP%(i, 36)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 36) = Int(draftP#)
            Locate 16, 68: Print Using "##"; draftP%(i, 36)
            If XCode = 1 GoTo PCS:
            If XCode = 2 GoTo PDone

            PPLAB:
            Locate 17, 17
            Max = 3
            draftP# = draftP%(i, 37)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 37) = Int(draftP#)
            Locate 17, 17: Print Using "###"; draftP%(i, 37)
            If XCode = 1 GoTo PSH:
            If XCode = 2 GoTo PDone

            PPLH:
            Locate 17, 27
            Max = 3
            draftP# = draftP%(i, 38)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 38) = Int(draftP#)
            Locate 17, 27: Print Using "###"; draftP%(i, 38)
            If XCode = 1 GoTo PPLAB:
            If XCode = 2 GoTo PDone

            PPL2B:
            Locate 17, 32
            Max = 2
            draftP# = draftP%(i, 39)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 39) = Int(draftP#)
            Locate 17, 32: Print Using "##"; draftP%(i, 39)
            If XCode = 1 GoTo PPLH:
            If XCode = 2 GoTo PDone

            PPL3B:
            Locate 17, 36
            Max = 2
            draftP# = draftP%(i, 40)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 40) = Int(draftP#)
            Locate 17, 36: Print Using "##"; draftP%(i, 40)
            If XCode = 1 GoTo PPL2B:
            If XCode = 2 GoTo PDone

            PPLHR:
            Locate 17, 40
            Max = 2
            draftP# = draftP%(i, 41)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 41) = Int(draftP#)
            Locate 17, 40: Print Using "##"; draftP%(i, 41)
            If XCode = 1 GoTo PPL3B:
            If XCode = 2 GoTo PDone

            PPLBB:
            Locate 17, 49
            Max = 3
            draftP# = draftP%(i, 42)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 42) = Int(draftP#)
            Locate 17, 49: Print Using "###"; draftP%(i, 42)
            If XCode = 1 GoTo PPLHR:
            If XCode = 2 GoTo PDone

            PPLKO:
            Locate 17, 54
            Max = 3
            draftP# = draftP%(i, 43)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 43) = Int(draftP#)
            Locate 17, 54: Print Using "###"; draftP%(i, 43)
            If XCode = 1 GoTo PPLBB:
            If XCode = 2 GoTo PDone

            PPRAB:
            Locate 18, 17
            Max = 3
            draftP# = draftP%(i, 44)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 44) = Int(draftP#)
            Locate 18, 17: Print Using "###"; draftP%(i, 44)
            If XCode = 1 GoTo PPLKO:
            If XCode = 2 GoTo PDone

            PPRH:
            Locate 18, 27
            Max = 3
            draftP# = draftP%(i, 45)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 45) = Int(draftP#)
            Locate 18, 27: Print Using "###"; draftP%(i, 45)
            If XCode = 1 GoTo PPRAB:
            If XCode = 2 GoTo PDone

            PPR2B:
            Locate 18, 32
            Max = 2
            draftP# = draftP%(i, 46)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 46) = Int(draftP#)
            Locate 18, 32: Print Using "##"; draftP%(i, 46)
            If XCode = 1 GoTo PPRH:
            If XCode = 2 GoTo PDone

            PPR3B:
            Locate 18, 36
            Max = 2
            draftP# = draftP%(i, 47)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 47) = Int(draftP#)
            Locate 18, 36: Print Using "##"; draftP%(i, 47)
            If XCode = 1 GoTo PPR2B:
            If XCode = 2 GoTo PDone

            PPRHR:
            Locate 18, 40
            Max = 2
            draftP# = draftP%(i, 48)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 48) = Int(draftP#)
            Locate 18, 40: Print Using "##"; draftP%(i, 48)
            If XCode = 1 GoTo PPR3B:
            If XCode = 2 GoTo PDone

            PPRBB:
            Locate 18, 49
            Max = 3
            draftP# = draftP%(i, 49)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 49) = Int(draftP#)
            Locate 18, 49: Print Using "###"; draftP%(i, 49)
            If XCode = 1 GoTo PPRHR:
            If XCode = 2 GoTo PDone

            PPRKO:
            Locate 18, 54
            Max = 3
            draftP# = draftP%(i, 50)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 50) = Int(draftP#)
            Locate 18, 54: Print Using "###"; draftP%(i, 50)
            If XCode = 1 GoTo PPRBB:
            If XCode = 2 GoTo PDone

            PLLBA:
            Locate 22, 27
            Max = 3
            draftP# = draftP%(i, 65)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 65) = Int(draftP#)
            Locate 22, 27: Print Using "###"; draftP%(i, 65)
            If XCode = 1 GoTo PPRKO:
            If XCode = 2 GoTo PDone

            PLL2B:
            Locate 22, 32
            Max = 3
            draftP# = draftP%(i, 66)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 66) = Int(draftP#)
            Locate 22, 32: Print Using "###"; draftP%(i, 66)
            If XCode = 1 GoTo PLLBA:
            If XCode = 2 GoTo PDone

            PLL3B:
            Locate 22, 37
            Max = 3
            draftP# = draftP%(i, 67)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 67) = Int(draftP#)
            Locate 22, 37: Print Using "###"; draftP%(i, 67)
            If XCode = 1 GoTo PLL2B:
            If XCode = 2 GoTo PDone

            PLLHR:
            Locate 22, 42
            Max = 3
            draftP# = draftP%(i, 68)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 68) = Int(draftP#)
            Locate 22, 42: Print Using "###"; draftP%(i, 68)
            If XCode = 1 GoTo PLL3B:
            If XCode = 2 GoTo PDone

            PLLBB:
            Locate 22, 47
            Max = 3
            draftP# = draftP%(i, 69)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 69) = Int(draftP#)
            Locate 22, 47: Print Using "###"; draftP%(i, 69)
            If XCode = 1 GoTo PLLHR:
            If XCode = 2 GoTo PDone

            PLLKO:
            Locate 22, 52
            Max = 3
            draftP# = draftP%(i, 70)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 70) = Int(draftP#)
            Locate 22, 52: Print Using "###"; draftP%(i, 70)
            If XCode = 1 GoTo PLLBB:
            If XCode = 2 GoTo PDone

            PLRBA:
            Locate 23, 27
            Max = 3
            draftP# = draftP%(i, 71)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 71) = Int(draftP#)
            Locate 23, 27: Print Using "###"; draftP%(i, 71)
            If XCode = 1 GoTo PLLKO:
            If XCode = 2 GoTo PDone

            PLR2B:
            Locate 23, 32
            Max = 3
            draftP# = draftP%(i, 72)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 72) = Int(draftP#)
            Locate 23, 32: Print Using "###"; draftP%(i, 72)
            If XCode = 1 GoTo PLRBA:
            If XCode = 2 GoTo PDone

            PLR3B:
            Locate 23, 37
            Max = 3
            draftP# = draftP%(i, 73)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 73) = Int(draftP#)
            Locate 23, 37: Print Using "###"; draftP%(i, 73)
            If XCode = 1 GoTo PLR2B:
            If XCode = 2 GoTo PDone

            PLRHR:
            Locate 23, 42
            Max = 3
            draftP# = draftP%(i, 74)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 74) = Int(draftP#)
            Locate 23, 42: Print Using "###"; draftP%(i, 74)
            If XCode = 1 GoTo PLR3B:
            If XCode = 2 GoTo PDone

            PLRBB:
            Locate 23, 47
            Max = 3
            draftP# = draftP%(i, 75)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 75) = Int(draftP#)
            Locate 23, 47: Print Using "###"; draftP%(i, 75)
            If XCode = 1 GoTo PLRHR:
            If XCode = 2 GoTo PDone

            PLRKO:
            Locate 23, 52
            Max = 3
            draftP# = draftP%(i, 76)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 76) = Int(draftP#)
            Locate 23, 52: Print Using "###"; draftP%(i, 76)
            If XCode = 1 GoTo PLRBB:
            If XCode = 2 GoTo PDone

            PRLBA:
            Locate 24, 27
            Max = 3
            draftP# = draftP%(i, 77)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 77) = Int(draftP#)
            Locate 24, 27: Print Using "###"; draftP%(i, 77);
            If XCode = 1 GoTo PLRKO:
            If XCode = 2 GoTo PDone

            PRL2B:
            Locate 24, 32
            Max = 3
            draftP# = draftP%(i, 78)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 78) = Int(draftP#)
            Locate 24, 32: Print Using "###"; draftP%(i, 78);
            If XCode = 1 GoTo PRLBA:
            If XCode = 2 GoTo PDone

            PRL3B:
            Locate 24, 37
            Max = 3
            draftP# = draftP%(i, 79)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 79) = Int(draftP#)
            Locate 24, 37: Print Using "###"; draftP%(i, 79);
            If XCode = 1 GoTo PRL2B:
            If XCode = 2 GoTo PDone

            PRLHR:
            Locate 24, 42
            Max = 3
            draftP# = draftP%(i, 80)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 80) = Int(draftP#)
            Locate 24, 42: Print Using "###"; draftP%(i, 80);
            If XCode = 1 GoTo PRL3B:
            If XCode = 2 GoTo PDone

            PRLBB:
            Locate 24, 47
            Max = 3
            draftP# = draftP%(i, 81)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 81) = Int(draftP#)
            Locate 24, 47: Print Using "###"; draftP%(i, 81);
            If XCode = 1 GoTo PRLHR:
            If XCode = 2 GoTo PDone

            PRLKO:
            Locate 24, 52
            Max = 3
            draftP# = draftP%(i, 82)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 82) = Int(draftP#)
            Locate 24, 52: Print Using "###"; draftP%(i, 82);
            If XCode = 1 GoTo PRLBB:
            If XCode = 2 GoTo PDone

            PRRBA:
            Locate 25, 27
            Max = 3
            draftP# = draftP%(i, 83)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 83) = Int(draftP#)
            Locate 25, 27: Print Using "###"; draftP%(i, 83);
            If XCode = 1 GoTo PRLKO:
            If XCode = 2 GoTo PDone

            PRR2B:
            Locate 25, 32
            Max = 3
            draftP# = draftP%(i, 84)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 84) = Int(draftP#)
            Locate 25, 32: Print Using "###"; draftP%(i, 84);
            If XCode = 1 GoTo PRRBA:
            If XCode = 2 GoTo PDone

            PRR3B:
            Locate 25, 37
            Max = 3
            draftP# = draftP%(i, 85)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 85) = Int(draftP#)
            Locate 25, 37: Print Using "###"; draftP%(i, 85);
            If XCode = 1 GoTo PRR2B:
            If XCode = 2 GoTo PDone

            PRRHR:
            Locate 25, 42
            Max = 3
            draftP# = draftP%(i, 86)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 86) = Int(draftP#)
            Locate 25, 42: Print Using "###"; draftP%(i, 86);
            If XCode = 1 GoTo PRR3B:
            If XCode = 2 GoTo PDone

            PRRBB:
            Locate 25, 47
            Max = 3
            draftP# = draftP%(i, 87)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 87) = Int(draftP#)
            Locate 25, 47: Print Using "###"; draftP%(i, 87);
            If XCode = 1 GoTo PRRHR:
            If XCode = 2 GoTo PDone

            PRRKO:
            Locate 25, 52
            Max = 3
            draftP# = draftP%(i, 88)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            draftP%(i, 88) = Int(draftP#)
            Locate 25, 52: Print Using "###"; draftP%(i, 88);
            If XCode = 1 GoTo PRRBB:

            PDone:
            Locate 25, 1
            Print "Is this okay? (Yes/No) ";
            X$ = ""
            Call YesNoB(X$, XCode, FG, BG)
            Locate , 1
            'ClearEOL 14, 1
            If XCode = 1 GoTo PRRKO
            If X$ = "N" GoTo PName
            If X$ <> "Y" GoTo PDone 'insist on either Y or N or up-arrow

            draftP%(i, 0) = 1
            If UCase$(_Trim$(THROW$)) = "L" Then draftP%(i, 0) = -1

            draftP%(i, 34) = 0
            If UCase$(_Trim$(ACT$)) = "N" Then draftP%(i, 34) = 1

        End If

    Loop Until i = 0

End Sub

'----------------------------------------
'       PITCHINIT subroutine
'----------------------------------------
'...explanation...
Sub PITCHINIT (i, FG, BG)

    Shared draftP%()

    Color FG, BG

    Locate 1, 60: Print Using "###"; draftP%(i, 35)
    Locate 1, 78: Print Using "#"; draftP%(i, 18)
    Locate 5, 10: Print Using "##"; draftP%(i, 1)
    Locate 5, 14: Print Using "##"; draftP%(i, 2)
    Locate 5, 18: Print Using "##"; draftP%(i, 3)
    Locate 5, 22: Print Using "###"; draftP%(i, 4)
    Locate 5, 27: Print Using "##"; draftP%(i, 5)
    Locate 5, 31: Print Using "##"; draftP%(i, 16)
    Locate 5, 35: Print Using "###"; draftP%(i, 6)
    Locate 5, 40: Print Using "###"; draftP%(i, 7)
    Locate 5, 45: Print Using "###"; draftP%(i, 8)
    Locate 5, 50: Print Using "###"; draftP%(i, 9)
    Locate 5, 55: Print Using "##"; draftP%(i, 33)
    Locate 5, 59: Print Using "####"; draftP%(i, 10)
    Locate 5, 66: Print Using "##"; draftP%(i, 13)
    Locate 6, 32: Print Using "##"; draftP%(i, 12)
    Locate 6, 50: Print Using "###"; draftP%(i, 14)
    Locate 10, 25: Print Using "###"; draftP%(i, 51)
    Locate 10, 30: Print Using "###"; draftP%(i, 52)
    Locate 10, 35: Print Using "##"; draftP%(i, 53)
    Locate 10, 39: Print Using "##"; draftP%(i, 54)
    Locate 10, 43: Print Using "##"; draftP%(i, 55)
    Locate 10, 47: Print Using "###"; draftP%(i, 56)
    Locate 10, 52: Print Using "###"; draftP%(i, 57)
    Locate 11, 25: Print Using "###"; draftP%(i, 58)
    Locate 11, 30: Print Using "###"; draftP%(i, 59)
    Locate 11, 35: Print Using "##"; draftP%(i, 60)
    Locate 11, 39: Print Using "##"; draftP%(i, 61)
    Locate 11, 43: Print Using "##"; draftP%(i, 62)
    Locate 11, 47: Print Using "###"; draftP%(i, 63)
    Locate 11, 52: Print Using "###"; draftP%(i, 64)
    Locate 14, 20: Print Using "#"; draftP%(i, 31)
    Locate 14, 36: Print Using "#"; draftP%(i, 32)
    Locate 14, 52: Print Using "###"; draftP%(i, 11)
    Locate 14, 68: Print Using "####"; draftP%(i, 17)
    Locate 16, 12: Print Using "###"; draftP%(i, 19)
    Locate 16, 17: Print Using "###"; draftP%(i, 20)
    Locate 16, 22: Print Using "###"; draftP%(i, 21)
    Locate 16, 27: Print Using "###"; draftP%(i, 22)
    Locate 16, 32: Print Using "##"; draftP%(i, 23)
    Locate 16, 36: Print Using "##"; draftP%(i, 24)
    Locate 16, 40: Print Using "##"; draftP%(i, 25)
    Locate 16, 44: Print Using "###"; draftP%(i, 26)
    Locate 16, 49: Print Using "###"; draftP%(i, 27)
    Locate 16, 54: Print Using "###"; draftP%(i, 28)
    Locate 16, 59: Print Using "###"; draftP%(i, 29)
    Locate 16, 64: Print Using "##"; draftP%(i, 30)
    Locate 16, 68: Print Using "##"; draftP%(i, 36)
    Locate 17, 17: Print Using "###"; draftP%(i, 37)
    Locate 17, 27: Print Using "###"; draftP%(i, 38)
    Locate 17, 32: Print Using "##"; draftP%(i, 39)
    Locate 17, 36: Print Using "##"; draftP%(i, 40)
    Locate 17, 40: Print Using "##"; draftP%(i, 41)
    Locate 17, 49: Print Using "###"; draftP%(i, 42)
    Locate 17, 54: Print Using "###"; draftP%(i, 43)
    Locate 18, 17: Print Using "###"; draftP%(i, 44)
    Locate 18, 27: Print Using "###"; draftP%(i, 45)
    Locate 18, 32: Print Using "##"; draftP%(i, 46)
    Locate 18, 36: Print Using "##"; draftP%(i, 47)
    Locate 18, 40: Print Using "##"; draftP%(i, 48)
    Locate 18, 49: Print Using "###"; draftP%(i, 49)
    Locate 18, 54: Print Using "###"; draftP%(i, 50)
    Locate 22, 27: Print Using "###"; draftP%(i, 65)
    Locate 22, 32: Print Using "###"; draftP%(i, 66)
    Locate 22, 37: Print Using "###"; draftP%(i, 67)
    Locate 22, 42: Print Using "###"; draftP%(i, 68)
    Locate 22, 47: Print Using "###"; draftP%(i, 69)
    Locate 22, 52: Print Using "###"; draftP%(i, 70)
    Locate 23, 27: Print Using "###"; draftP%(i, 71)
    Locate 23, 32: Print Using "###"; draftP%(i, 72)
    Locate 23, 37: Print Using "###"; draftP%(i, 73)
    Locate 23, 42: Print Using "###"; draftP%(i, 74)
    Locate 23, 47: Print Using "###"; draftP%(i, 75)
    Locate 23, 52: Print Using "###"; draftP%(i, 76)
    Locate 24, 27: Print Using "###"; draftP%(i, 77);
    Locate 24, 32: Print Using "###"; draftP%(i, 78);
    Locate 24, 37: Print Using "###"; draftP%(i, 79);
    Locate 24, 42: Print Using "###"; draftP%(i, 80);
    Locate 24, 47: Print Using "###"; draftP%(i, 81);
    Locate 24, 52: Print Using "###"; draftP%(i, 82);
    Locate 25, 27: Print Using "###"; draftP%(i, 83);
    Locate 25, 32: Print Using "###"; draftP%(i, 84);
    Locate 25, 37: Print Using "###"; draftP%(i, 85);
    Locate 25, 42: Print Using "###"; draftP%(i, 86);
    Locate 25, 47: Print Using "###"; draftP%(i, 87);
    Locate 25, 52: Print Using "###"; draftP%(i, 88);

End Sub

'----------------------------------------
'       VIEWHITTERS subroutine
'----------------------------------------
'...explanation...
Sub ViewHitters ()

    Shared statB$()
    Shared draftB%(), TB%()
    Shared statI2!

    Do
        Cls
        Color 14: Print "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"

        For I = 0 To 22
            If I = 20 Then
                Locate 23
                Print "HIT ANY KEY TO CONTINUE"
                I$ = GetKeyPress$

                Cls
                Color 12: Print T$
                Color 14: Print "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"
            End If

            Color 3
            If statB$(I) <> "XXX" Then
                Print Left$(statB$(I), 10);
                Color 7

                If draftB%(I, 0) = -1 Then Print Tab(12); "L";
                If draftB%(I, 0) = 0 Then Print Tab(12); "S";
                If draftB%(I, 0) = 1 Then Print Tab(12); "R";
                If draftB%(I, 0) = -2 Then Print Tab(12); "LP";
                If draftB%(I, 0) = 2 Then Print Tab(12); "RP";

                Print Tab(14); Using "#"; draftB%(I, 22);: If draftB%(I, 23) <> -1 Then Print Using "#"; draftB%(I, 23);: If draftB%(I, 24) <> -1 Then Print Using "#"; draftB%(I, 24);: If draftB%(I, 25) <> -1 Then Print Using "#"; draftB%(I, 25);
                Print Tab(20); Using "###"; draftB%(I, 3);
                Print Tab(24); Using "###"; draftB%(I, 4);
                Print Tab(28); Using "###"; draftB%(I, 5);
                Print Tab(32); Using "###"; draftB%(I, 6);
                Print Tab(36); Using "##"; draftB%(I, 7);
                Print Tab(39); Using "##"; draftB%(I, 8);
                Print Tab(42); Using "##"; draftB%(I, 9);
                Print Tab(45); Using "###"; draftB%(I, 10);
                Print Tab(49); Using "###"; draftB%(I, 11);
                Print Tab(53); Using "###"; draftB%(I, 12);
                Print Tab(57); Using "###"; draftB%(I, 13);
                Print Tab(61); Using "##"; draftB%(I, 14);
                Print Tab(64); Using "##"; draftB%(I, 18);

                statI2! = 0

                If draftB%(I, 4) > 0 Then statI2! = Int((draftB%(I, 6) / draftB%(I, 4) + .0005) * 1000)
                If draftB%(I, 4) = 0 Or statI2! = 0 Then Print Tab(68); ".000"
                If statI2! > 0 And statI2! < 100 Then Print Tab(68); ".0";: Print Tab(70); Using "##"; statI2!
                If statI2! > 100 Then Print Tab(68); ".";: Print Tab(69); Using "###"; statI2!

            End If

        Next

        Print
        Color 14: Print "TOTALS";
        Locate , 23: Print Using "####    ####   ###    ###    ####    ###     .###"; TB%(1); TB%(3); TB%(5); TB%(7); TB%(9); TB%(11); TB%(3) / TB%(1)
        Locate , 27: Print Using "####   ####   ###     ###     ###"; TB%(2); TB%(4); TB%(6); TB%(8); TB%(10)

        Color 11
        Locate 20: Print "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        If I$ = Chr$(9) Then
            Call FieldingStats
            Color 11
            Locate 20: Print "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
            I$ = GetKeyPress$
            'any key goes to options selection (EXIT SUB)
            'TAB goes back to previous screen
        End If

    Loop Until I$ <> Chr$(9)

End Sub

'----------------------------------------
'       ViewPitchers subroutine
'----------------------------------------
'...explanation...
Sub ViewPitchers ()

    Shared statP$()
    Shared draftP%(), inputTP%()

    Cls
    Color 12: Print T$
    Color 14: Print "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"
    app = 0

    For I = 0 To 21
        If I = 18 Then
            Locate 23: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$
            Cls
            Color 12: Print T$
            Color 14: Print "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"
        End If

        Color 3

        If statP$(I) <> "XXX" Then

            app = app + draftP%(I, 4)
            Print Left$(statP$(I), 10);

            Color 7

            If draftP%(I, 0) = -1 Then Print Tab(14); "L";
            If draftP%(I, 0) = 1 Then Print Tab(14); "R";

            Print Tab(16); Using "##"; draftP%(I, 1);
            Print Tab(19); Using "##"; draftP%(I, 2);
            Print Tab(22); Using "##"; draftP%(I, 3);
            Print Tab(25); Using "##"; draftP%(I, 4);
            Print Tab(28); Using "##"; draftP%(I, 5);
            Print Tab(31); Using "###"; draftP%(I, 6);
            Print Tab(35); Using "###"; draftP%(I, 7);
            Print Tab(39); Using "###"; draftP%(I, 8);
            Print Tab(43); Using "###"; draftP%(I, 9);
            Print Tab(46); Using "##.##"; draftP%(I, 10) / 100;
            Print Tab(53); Using "##"; draftP%(I, 12);
            Print Tab(56); Using "##"; draftP%(I, 13);
            Print Tab(59); Using "##"; draftP%(I, 16);
            Print Tab(62); Using "##"; draftP%(I, 33);
            Print Tab(66); Using "##"; draftP%(I, 18)

        End If

    Next

    Print
    Color 14: Print "TOTALS";
    Locate , 14: Print Using "####  ####      ####    ####     #.##    ###   ###"; inputTP%(1); inputTP%(3); inputTP%(6); inputTP%(8); (ERX! * 9) / inputTP%(6); inputTP%(13); inputTP%(33)
    Locate , 17: Print Using "####      ###    ####    ####            ###"; inputTP%(2); inputTP%(5); inputTP%(7); inputTP%(9); inputTP%(16)
    Print "TOTAL APPEARANCES: "; app

    Color 11
    Locate 24: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'     ViewHittingFielding subroutine
'----------------------------------------
'...explanation...
Sub ViewHittingFielding ()

    Shared draftP%()
    Shared statP$()

    Cls
    Color 14: Print "PITCHER        G  AB   R   H DB TR HR RBI  BB  SO  SB CS BAVG  R B FAVG"

    For I = 0 To 21

        Color 3: Print Left$(statP$(I), 10);
        Color 7: Print Tab(14); Using "###"; draftP%(I, 19);
        Print Tab(18); Using "###"; draftP%(I, 20);
        Print Tab(22); Using "###"; draftP%(I, 21);
        Print Tab(26); Using "###"; draftP%(I, 22);
        Print Tab(30); Using "##"; draftP%(I, 23);
        Print Tab(33); Using "##"; draftP%(I, 24);
        Print Tab(36); Using "##"; draftP%(I, 25);
        Print Tab(39); Using "###"; draftP%(I, 26);
        Print Tab(43); Using "###"; draftP%(I, 27);
        Print Tab(47); Using "###"; draftP%(I, 28);
        Print Tab(52); Using "##"; draftP%(I, 29);
        Print Tab(55); Using "##"; draftP%(I, 30);

        If draftP%(I, 11) <> 999 Then
            statPCT! = draftP%(I, 11) / 1000
            Print Tab(59); Using "#.###"; statPCT!;
        Else
            statI2! = 0
            '-- This is initialized as 1000
            '-- So when not given a value it calculates too large
            If draftP%(I, 20) > 0 Then statI2! = Int((draftP%(I, 22) / draftP%(I, 20) + .0005) * 1000)

            If draftP%(I, 20) = 0 Or statI2! = 0 Then Print Tab(59); ".000";
            If statI2! > 0 And statI2! < 100 Then Print Tab(59); ".0";: Print Tab(61); Using "##"; statI2!;
            If statI2! > 100 And statI2! < 1000 Then Print Tab(59); ".";: Print Tab(60); Using "###"; statI2!;
            'If statI2! >= 1000 Then Print Tab(59); Using "#.###"; statI2!
            If statI2! >= 1000 Then Print Tab(59); " ---";

        End If

        Print Tab(64); Using "#"; draftP%(I, 31);
        Print Tab(66); Using "#"; draftP%(I, 32);
        Print Tab(68); Using "####"; draftP%(I, 17)

    Next

    Color 11: Locate 24: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       FieldingStats Subroutine
'----------------------------------------
'...explanation...
Sub FieldingStats ()

    Shared statB$()
    Shared draftB%()

    Cls
    Color 14: Print "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

    For I = 0 To 22
        If I = 20 Then
            Locate 23: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

            Cls
            Color 12: Print T$
            Color 14: Print "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"
        End If

        Color 3: Print Left$(statB$(I), 10);
        Color 7
        Print Tab(13); Using "####"; draftB%(I, 26);
        Print Tab(19); Using "#"; draftB%(I, 33);
        Print Tab(23); Using "#"; draftB%(I, 34);
        Print Tab(26); Using "####"; draftB%(I, 27);
        Print Tab(32); Using "#"; draftB%(I, 35);
        Print Tab(36); Using "#"; draftB%(I, 36);
        Print Tab(39); Using "####"; draftB%(I, 28);
        Print Tab(45); Using "#"; draftB%(I, 37);
        Print Tab(49); Using "#"; draftB%(I, 38);
        Print Tab(52); Using "####"; draftB%(I, 29);
        Print Tab(58); Using "#"; draftB%(I, 39);
        Print Tab(62); Using "#"; draftB%(I, 40);
        Print Tab(66); Using "#"; draftB%(I, 16);
        Print Tab(71); Using "#"; draftB%(I, 30)
    Next

End Sub
