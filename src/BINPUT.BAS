Sub BINPUT ()

    _Title "Full Count Baseball - Team Input"

    DefInt A-Z

    Shared L%(), statP0%(), SS%()

    Shared B$(), BP$(), P$()

    Shared K9&()

    Shared T$ 'only if we can't keep up with team name separately

    'Dim FS%(22, 4, 4)
    'Dim BRS$(79)

    ReDim menuChoices$(1 to 9)

    Do
        Cls

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
        Row% = 6
        Column% = 25

        Call box0(5, 24, 15, 52, 2, 15, 1)
            
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: 
                    Call InputTeam (Selection%, YN$, T$, T)

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

                    Call SaveTeamFile (YN$, T$, F$)

                Case 2:
                    Call GetTeam_BINPUT (YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call InputTeam (Selection%, YN$, T$, T)
                    
                Case 3:
                    Call GetTeam_BINPUT (YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call PrintTeamStats (T$, T)
                    
                Case 4: 
                    Call UMPMGR
                
                Case 5: 
                    Call GetTeam_BINPUT (YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call SwapSpots (YN$, T$)

                Case 6: 
                    Cls

                Case 7: 
                    Call GetTeam_BINPUT (YN$, NTMS, T)
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
                    
                    Call SaveTeamFile (YN$, T$, F$)
                    'GoTo 1920
                
                Case 8: 
                    Call GetTeam_BINPUT (YN$, NTMS, T)
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

                    Call SaveTeamFile (YN$, T$, F$)

                Case 9: 
                    Call GetTeam_BINPUT (YN$, NTMS, T)
                    Color 15, 0
                    T$ = RTrim$(Q$(0))
                    Call ViewRostersRatings(T$)
                
            End Select

        End If

    Loop Until Selection% = 6 OR ExitCode% = 27

    Cls
    '88 Exit Sub

End Sub


'----------------------------------------
'       GetTeam_BINPUT subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_BINPUT (yearNumber$, numberTeams, teamIdx)

    Shared L%()
    
    Do

        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" Then 
            Call SelectTeam_BINPUT (yearNumber$, numberTeams, teamIdx)
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
                1103 Next I1
                Close 1

                Call SelectTeam_BINPUT (yearNumber$, numberTeams, teamIdx)
                
            End If 'Done checking for file

        End If 'Done checking input

        If BO% = 1 Then 
            BO% = 0
        Else
            T$ = RTrim$(Q$(0))
            Locate 2, 33: Color 15, 0
            Color L%(11), L%(12): Print T$
            Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If
    
    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'     SelectTeam_BINPUT subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_BINPUT (yearNumber$, numberTeams, teamIdx)

    Shared draftB%(), draftP%(), SA%(), TS%()
    Shared DYS%(), INJ%()

    Shared B$(), P$()

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
            FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015)
            FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2 AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027)
            FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2 AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039)
            Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
        Next

        Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

        Get #1, teamidx

        For I = 1 To 13: L%(I) = CVI(Q$(I)): Next: K9&(0) = CVS(Q$(14))

        For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
            For I1 = 0 To 41
                draftB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
            Next
        Next

        For I = 0 To 21
            P$(I) = RTrim$(Q$(I * 38 + 1004))
            For I1 = 0 To 36
                draftP%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
            Next
        Next
        
        S$ = RTrim$(Q$(1840))
        M$ = RTrim$(Q$(1841))
        NN$ = RTrim$(Q$(1842))
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
                TS%(X) = CVI(Q$(X + 1))
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
                SA%(X) = CVI(Q$(X + 950))
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

            If TS%(0) <> 999 Then
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
                    SA%(X) = 0
                Next

                For X = 0 To 11
                    TS%(X) = 0
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
                Input #1, INJ%(I1)
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
    Shared B$(), P$()
    Shared draftB%(), draftP%()
    Shared L%(), TB%(), TP%()

    For K = 0 To 15
        TB%(K) = 0
    Next

    For K = 0 To 33
        TP%(K) = 0
    Next

    For K = 4 To 18:
        For I = 0 To 22
            If B$(I) <> "XXX" Then
                TB%(K - 3) = TB%(K - 3) + draftB%(I, K)
            End If
        Next
    Next

    For K = 4 To 18: 
        For I = 0 To 21
            If P$(I) <> "XXX" And draftP%(I, 11) = 999 Then
                TB%(K - 3) = TB%(K - 3) + draftP%(I, K + 16)
            End If
        Next
    Next

    For K = 1 To 33: 
        For I = 0 To 21
            If P$(I) <> "XXX" Then
                TP%(K) = TP%(K) + draftP%(I, K)
            End If
        Next
    Next

    ERX! = 0

    For I = 0 To 21
        If P$(I) <> "XXX" Then
            ERX! = ERX! + ((draftP%(I, 6) / 9) * (draftP%(I, 10) / 100))
        End If
    Next

    Cls

    Color 12: Locate 5, 10: Print targetTeam$
    Color 3: Locate 7, 10: Print "LEAGUE BATTING AVG.";
    Locate 7, 38: Color 7: Print Using "#####"; L%(1)
    Color 3: Locate , 10: Print "LEAGUE STRIKE OUT AVG.";
    Locate , 38: Color 7: Print Using "#####"; L%(2)
    Color 3: Locate , 10: Print "LEAGUE BASE ON BALLS AVG.";
    Locate , 38: Color 7: Print Using "#####"; L%(3)
    Color 3: Locate , 10: Print "LEAGUE HOME RUN AVG.";
    Locate , 38: Color 7: Print Using "#####"; L%(4)
    Color 3: Locate , 10: Print "DOUBLE PLAYS AVG.";
    Locate , 38: Color 7: Print Using "#####"; L%(5)
    Color 3: Locate , 10: Print "PARK DOUBLE ADJ.";
    Locate , 38: Color 7: Print Using "#####"; L%(6)
    Color 3: Locate , 10: Print "PARK TRIPLE ADJ.";
    Locate , 38: Color 7: Print Using "#####"; L%(7)
    Color 3: Locate , 10: Print "PARK HOME RUN ADJ.";
    Locate , 37: Color 7: Print Using "######"; K9&(0)
    Color 3: Locate , 10: Print "PARK FOUL GROUND ADJ.";
    Locate , 37: Color 7: Print Using "######"; L%(8)
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
    Shared L%(), SA%(), TS%()
    
    Shared B$(), P$()

    ' New team only
    If inputAction% = 1 Then
        targetTeam$ = ""
        teamIdx = 0
        yearNumber$ = ""

        For X = 1 To 13
            L%(X) = 0
        Next

        K9&(0) = 0

        For X = 0 To 22
            B$(X) = ""
            For X1 = 0 To 41
                draftB%(X, X1) = 0
            Next
        Next
        
        For X = 0 To 21
            P$(X) = ""
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
            SA%(X) = 0
        Next

        For X = 0 To 11
            TS%(X) = 0
        Next

    End If

    Call EDITTEAM(yearNumber$, targetTeam$, NN$, S$, M$, teamIdx)
    Call BATTEREDIT
    Call PITCHEREDIT

    Locate 10, 8

End Sub


'----------------------------------------
'        SaveTeamFile subroutine
'----------------------------------------
Sub SaveTeamFile (yearNumber$, targetTeam$, targetFile$)

    ' Shared M$, NN$, S$

    Shared K9&()

    Shared DYS%(), INJ%()
    Shared draftB%(), draftP%()
    Shared L%(), SA%(), TS%()

    Shared B$(), P$()

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))

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
        FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015)
        FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2 AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027)
        FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2 AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039)
        Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
    Next

    Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
    LSet Q$(0) = targetTeam$
    
    For I = 1 To 13: 
        LSet Q$(I) = MKI$(L%(I))
    Next
    
    LSet Q$(14) = MKS$(K9&(0))

    For I = 0 To 22: 
        LSet Q$(I * 43 + 15) = B$(I)
        For I1 = 0 To 41: 
            LSet Q$(I * 43 + I1 + 16) = MKI$(draftB%(I, I1))
        Next
    Next
    
    For I = 0 To 21: LSet Q$(I * 38 + 1004) = P$(I)
        For I1 = 0 To 36: LSet Q$(I * 38 + I1 + 1005) = MKI$(draftP%(I, I1))
        Next
    Next

    LSet Q$(1840) = S$
    LSet Q$(1841) = M$
    LSet Q$(1842) = NN$
    LSet Q$(1843) = MKI$(999)

    Put #1, T: Close 1

    SCK = 0

    For I = 1 To 24
        SCK = SCK + SA%(I)
    Next
    
    If SCK > 0 Then
        TS%(0) = 999
        For I = 1 To 11
            TS%(I) = -1
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
            LSet Q$(X + 1) = MKI$(TS%(X))
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
            LSet Q$(X + 950) = MKI$(SA%(X))
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

        Put #1, T
        Close 1

    End If
    
    Open diskPaths$(1) + targetFile$ + "D" For Output As #1
    
    For I1 = 0 To 22
        Print #1, INJ%(I1)
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
Sub SwapSpots (yearNumber$, targetTeam$)

    Shared B$(), BS$(), P$(), PS$()
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
        Call LoadTeamStats (targetTeam$, TS, SS, teamStatFile$)
    End If

    Do
        Cls
        Print "TEAM ROSTER FOR "; targetTeam$

        For I = 0 To 22
            Locate I + 3, 1
            Print Using "## "; I + 1;
            Print B$(I);
        Next

        For I = 0 To 21
            Locate I + 3, 25
            Print Using "## "; I + 1;
            Print P$(I);
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
                
                Call SaveTeamFile (yearNumber$, targetTeam$, teamStatFile$)

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

    Shared B$(), BS$()
    Shared BS%(), draftB%(), statB0%(), SS%()

    Do
        For I = 16 To 19: Locate I, 50: Print Space$(27): Next

        Do
            Locate 16, 50
            Input "BATTER #1 "; I1
        Loop Until I1 >= 1 And I1 <= 23

        Locate 16, 60: Print B$(I1 - 1)

        Do
            Locate 17, 50
            Input "BATTER #2 "; I2
        Loop Until I2 >= 1 And I2 <= 23

        Locate 17, 60: Print B$(I2 - 1)

        Locate 18, 50: Print "ANY CHANGE (YN)";
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N"

    Swap B$(I1 - 1), B$(I2 - 1)
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
Sub SwitchPitchers

    Shared draftP%(), statP0%(), PS%()
    Shared P$(), PS$()

    Do
        For I = 16 To 19: Locate I, 50: Print Space$(27): Next

        Do
            Locate 16, 50: Input "PITCHER #1 "; I1
        Loop Until I1 >= 1 And I1 <= 22

        Locate 16, 60: Print P$(I1 - 1)

        Do
            Locate 17, 50: Input "PITCHER #2 "; I2
        Loop Until I1 >= 1 And I1 <= 22

        Locate 17, 60: Print P$(I2 - 1)

        Locate 18, 50: Print "ANY CHANGE (YN)";
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N"

    Swap P$(I1 - 1), P$(I2 - 1): Swap PS$(I1 - 1), PS$(I2 - 1)

    For I = 0 To 88: Swap draftP%(I1 - 1, I), draftP%(I2 - 1, I): Next

    For I = 0 To 41: Swap statP0%(I1 - 1, I), statP0%(I2 - 1, I): Next

    Swap PS%(I1 - 1), PS%(I2 - 1)

End Sub


'----------------------------------------
'       PrintTeamStats subroutine
'----------------------------------------
'...explanation...
Sub PrintTeamStats (targetTeam$, teamIdx)

    Shared BP$(), P$()

    Shared draftB%(), draftP%(), L%()

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))
    'M$, S$, T$

    LPrint Chr$(15) ': WIDTH "LPT1:", 137
    
    LPrint "TEAM # "; teamIdx

    LPrint "TEAM NAME "; targetTeam$; Tab(30); "MANAGER: "; M$; Tab(50); "STADIUM: "; S$

    LPrint: LPrint "LBA LSO LBB LHR DPG DBL TR    HR   FOUL PARK INFO"

    LPrint Using "### ### ### ### ### ### ### ###### ###  "; L%(1); L%(2); L%(3); L%(4); L%(5); L%(6); L%(7); K9&(0); L%(8);: LPrint BP$(L%(9)); " "; BP$(L%(10) + 2)

    LPrint

    LPrint "BATTER      B    G  AB   R   H DB TR HR RBI  BB  SO  SB CS RN B GA P1 FAV1 A R P2 FAV2 A R P3 FAV3 A R P4 FAV4 A R EBA BAVG"

    For I = 0 To 22
        
        If B$(I) <> "" And B$(I) <> "XXX" Then

            LPrint Left$(B$(I), 12); Tab(13);
            
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
        If P$(I) <> "" And P$(I) <> "XXX" Then

            LPrint Left$(P$(I), 12); Tab(13);
            
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
        If P$(I) <> "" And P$(I) <> "XXX" Then

            LPrint Mid$(P$(I), 1, 12); Tab(13);
            
            If draftP%(I, 0) = 1 Then LPrint "R  ";
            If draftP%(I, 0) = -1 Then LPrint "L  ";

            LPrint Using "## ## ## ## ## ## ### ### ### ### #.## ## ##   #   ###"; draftP%(I, 1); draftP%(I, 2); draftP%(I, 3); draftP%(I, 4); draftP%(I, 5); draftP%(I, 16); draftP%(I, 6); draftP%(I, 7); draftP%(I, 8); draftP%(I, 9); draftP%(I, 10) / 100; draftP%(I, 12); draftP%(I, 13); draftP%(I, 18); draftP%(I, 14)
        End If

    Next I
    
    LPrint Chr$(18): LPrint Chr$(12)

End Sub


'----------------------------------------
'     CalculateRunRating subroutine
'----------------------------------------
'...explanation...
Sub CalculateRunRating ()

    Shared B$(), P$()
    Shared draftB%(), draftP%()

    Cls
    For I = 0 To 22
        If B$(I) <> "" And B$(I) <> "XXX" Then
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
            Print B$(I); Tab(20); draftB%(I, 16)
        End If
    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    For I = 0 To 21
        Cls
        If P$(I) <> "" And P$(I) <> "XXX" Then 
            RR = Int(Rnd(1) * 20) + 1
            RR = RR * 1.4

            If RR <= 4 Then draftP%(I, 31) = 1
            If RR > 4 And RR <= 8 Then draftP%(I, 31) = 2
            If RR > 8 And RR <= 12 Then draftP%(I, 31) = 3
            If RR > 12 And RR <= 16 Then draftP%(I, 31) = 4
            If RR > 16 Then draftP%(I, 31) = 5
            Print P$(I); Tab(20); draftP%(I, 31)
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

    Dim B1 As Single, BUNT As Single
    Dim OB As Single
    Dim R1 As Single, R2 As Single, RR As Single
    Dim SLG As Single, SLG2 As Single

    Shared draftB%(), draftP%()
    Shared B$(), P$()

    ' *** BUNT RATING CALCS ***
    Cls
    For I = 0 To 22
        If B$(I) <> "" And B$(I) <> "XXX" Then
            PA = draftB%(I, 11) + draftB%(I, 4)
            If PA = 0 Then PA = 1
            OB = (draftB%(I, 6) * 100 + PA / 2) / PA
            SLG = (draftB%(I, 6) + draftB%(I, 7) + draftB%(I, 8) * 2 + draftB%(I, 9) * 3) * 100
            SLG2 = (SLG + PA / 2) / PA
            R1 = (draftB%(I, 12) * 100 + PA / 2) / PA
            R1 = 11 - R1 ^ 2 / 25
            R2 = 11 - (SLG2 - OB) / 25
            BUNT = (R1 + R1 + R2) / 3

            B1 = draftB%(I, 15) * 1000 / PA
            
            Select Case B1
                Case Is <= 0: BR = 1
                Case 1: BR = 2
                Case 2 to 3: BR = 3
                Case 4 to 6: BR = 4
                Case 7 to 10: BR = 5
                Case 11 to 15: BR = 6
                Case 16 to 20: BR = 7
                Case 21 to 25: BR = 8
                Case 26 to 30: BR = 9
                Case 31 to 35: BR = 10
                Case Is >= 36: BR = 10
            End Select
            
            1921 '
            BUNT = (BUNT + BR) / 2
            If BUNT < 1 Then BUNT = 1
            If BUNT > 9 Then BUNT = 9
            draftB%(I, 30) = Int(BUNT + .5)
            Print B$(I); Tab(20); draftB%(I, 30)
        End If
        
    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$
    
    Cls
    
    For I = 0 To 21
        If P$(I) <> "" And P$(I) <> "XXX" Then
            PA = draftP%(I, 20) + draftP%(I, 27)
            If PA = 0 Then PA = 1
            OB = (draftP%(I, 22) * 100 + PA / 2) / PA
            SLG = (draftP%(I, 22) + draftP%(I, 23) + draftP%(I, 24) * 2 + draftP%(I, 25) * 3) * 100
            SLG2 = (SLG + PA / 2) / PA
            R1 = (draftP%(I, 28) * 100 + PA / 2) / PA
            R1 = 11 - R1 ^ 2 / 25
            If R1 < 1 Then R1 = 1
            If R1 > 10 Then R1 = 10
            R2 = 11 - (SLG2 - OB) / 25
            If R2 < 1 Then R2 = 1
            If R2 > 10 Then R2 = 10
            BUNT = (R1 + R1 + R2) / 3
            B1 = draftP%(I, 36) * 1000 / PA

            Select Case B1
                Case Is <= 0: BR = 1
                Case 1: BR = 2
                Case 2 to 3: BR = 3
                Case 4 to 6: BR = 4
                Case 7 to 10: BR = 5
                Case 11 to 15: BR = 6
                Case 16 to 20: BR = 7
                Case 21 to 25: BR = 8
                Case 26 to 30: BR = 9
                Case 31 to 35: BR = 10
                Case Is >= 36: BR = 10
            End Select
            
            1961 '
            BUNT = (BUNT + BR) / 2
            If BUNT < 1 Then BUNT = 1
            If BUNT > 9 Then BUNT = 9
            draftP%(I, 32) = Int(BUNT + .5)
            Print P$(I); Tab(20); draftP%(I, 32)
        End If
    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       BATTEREDIT subroutine
'----------------------------------------
'...explanation...
SUB BATTEREDIT ()

    Shared B$(), B%()

    Do
        COLOR 15, 0
        CLS
        PRINT "EDIT/ENTER BATTERS"
        X% = 2
        FOR i = 0 TO 10
        LOCATE i + 5, 5
        PRINT USING "##"; i + 1; : PRINT " "; B$(i); TAB(40);
        PRINT USING "##"; i + 13; : PRINT " "; B$(i + 12)
        NEXT
        LOCATE , 5: PRINT USING "##"; 12; : PRINT " "; B$(11)
        PRINT
        'FOR i = 0 TO 22
        'FOR i1 = 22 TO 25
        'IF (B%(i, i1) > 0 AND B%(i, i1 + 4) = 0) OR B%(i, 4) = 0 THEN PRINT B$(i)
        'NEXT
        'NEXT

        Do
            LOCATE 22, 1
            PRINT "# TO ENTER/EDIT (1-23) (0 TO CONTINUE) "; : INPUT i
        Loop Until i >= 1 And i <= 23

        IF i <> 0 THEN

        ' INITIALIZE BATTER SCREEN

            COLOR 15, 1: CLS
            LOCATE 1, 30: PRINT "EDIT AND INPUT BATTERS"
            LOCATE 3, 1: PRINT "NAME:"
            LOCATE 3, 24: PRINT "BATS:"
            LOCATE 3, 36: PRINT "ACTIVE:"
            LOCATE 3, 50: PRINT "INJ LENGTH:"
            LOCATE 5, 14: PRINT " G  AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
            LOCATE 6, 1: PRINT "SEASON:"
            LOCATE 7, 1: PRINT "VS LHP:"
            LOCATE 8, 1: PRINT "VS RHP:"
            LOCATE 10, 33: PRINT "FIELDING RATINGS"
            LOCATE 11, 31: PRINT "POS  FAVG  ARM RANGE"
            LOCATE 12, 19: PRINT "POSITION 1"
            LOCATE 13, 19: PRINT "POSITION 2"
            LOCATE 14, 19: PRINT "POSITION 3"
            LOCATE 15, 19: PRINT "POSITION 4"
            LOCATE 17, 12: PRINT "RUN (1-9):"
            LOCATE 17, 27: PRINT "BUNT (1-9):"
            LOCATE 17, 44: PRINT "G/A:"
            LOCATE 17, 54: PRINT "LEAGUE BAVG:"
            LOCATE 20, 29: PRINT "SPLIT STATS LEAGUE DATA"
            LOCATE 21, 26: PRINT "BAVG   K%  BB%  HR%  2B%  3B%"
            LOCATE 22, 13: PRINT "LHB vs LHP"
            LOCATE 23, 13: PRINT "LHB vs RHP"
            LOCATE 24, 13: PRINT "RHB vs LHP";
            LOCATE 25, 13: PRINT "RHB vs RHP";

        ' INITIALIZE BATTER FIELDS
            'Colr = 112
            FG = 0
            BG = 7
            COLOR FG,BG

            LOCATE 3, 7: QPrint PadRight$(B$(i), 15), FG, BG, -1
            IF B%(i, 0) = 2 THEN BATS$ = "RP"
            IF B%(i, 0) = -2 THEN BATS$ = "LP"
            IF B%(i, 0) = 1 THEN BATS$ = "R"
            IF B%(i, 0) = -1 THEN BATS$ = "L"
            IF B%(i, 0) = 0 THEN BATS$ = "S"

            LOCATE 3, 30: QPrint PadRight$(BATS$, 2), FG, BG, -1
            ACT$ = "Y"
            IF B%(i, 31) = 1 THEN ACT$ = "N"
            LOCATE 3, 44: PRINT ACT$

            LOCATE 3, 62: PRINT USING "###";INJ%(i)
            LOCATE 6, 12: PRINT USING "###";B%(i, 3)
            LOCATE 6, 17: PRINT USING "###";B%(i, 4)
            LOCATE 6, 22: PRINT USING "###";B%(i, 5)
            LOCATE 6, 27: PRINT USING "###";B%(i, 6)
            LOCATE 6, 32: PRINT USING "##"; B%(i, 7)
            LOCATE 6, 36: PRINT USING "##"; B%(i, 8)
            LOCATE 6, 40: PRINT USING "##"; B%(i, 9)
            LOCATE 6, 44: PRINT USING "###";B%(i, 10)
            LOCATE 6, 49: PRINT USING "###";B%(i, 11)
            LOCATE 6, 54: PRINT USING "###";B%(i, 12)
            LOCATE 6, 59: PRINT USING "###";B%(i, 13)
            LOCATE 6, 64: PRINT USING "##"; B%(i, 14)
            LOCATE 6, 68: PRINT USING "##"; B%(i, 15)
            LOCATE 7, 17: PRINT USING "###";B%(i, 42)
            LOCATE 7, 27: PRINT USING "###";B%(i, 43)
            LOCATE 7, 32: PRINT USING "##";B%(i, 44)
            LOCATE 7, 36: PRINT USING "##";B%(i, 45)
            LOCATE 7, 40: PRINT USING "##";B%(i, 46)
            LOCATE 7, 49: PRINT USING "###";B%(i, 47)
            LOCATE 7, 54: PRINT USING "###";B%(i, 48)
            LOCATE 8, 17: PRINT USING "###";B%(i, 49)
            LOCATE 8, 27: PRINT USING "###";B%(i, 50)
            LOCATE 8, 32: PRINT USING "##";B%(i, 51)
            LOCATE 8, 36: PRINT USING "##";B%(i, 52)
            LOCATE 8, 40: PRINT USING "##";B%(i, 53)
            LOCATE 8, 49: PRINT USING "###";B%(i, 54)
            LOCATE 8, 54: PRINT USING "###";B%(i, 55)
            LOCATE 12, 31: PRINT USING "##";B%(i, 22)
            LOCATE 12, 36: PRINT USING "####";B%(i, 26)
            LOCATE 12, 43: PRINT USING "#";B%(i, 33)
            LOCATE 12, 47: PRINT USING "#";B%(i, 34)
            LOCATE 13, 31: PRINT USING "##";B%(i, 23)
            LOCATE 13, 36: PRINT USING "####";B%(i, 27)
            LOCATE 13, 43: PRINT USING "#";B%(i, 35)
            LOCATE 13, 47: PRINT USING "#";B%(i, 36)
            LOCATE 14, 31: PRINT USING "##";B%(i, 24)
            LOCATE 14, 36: PRINT USING "####";B%(i, 28)
            LOCATE 14, 43: PRINT USING "#";B%(i, 37)
            LOCATE 14, 47: PRINT USING "#";B%(i, 38)
            LOCATE 15, 31: PRINT USING "##";B%(i, 25)
            LOCATE 15, 36: PRINT USING "####";B%(i, 29)
            LOCATE 15, 43: PRINT USING "#";B%(i, 39)
            LOCATE 15, 47: PRINT USING "#";B%(i, 40)
            LOCATE 17, 23: PRINT USING "#";B%(i, 16)
            LOCATE 17, 39: PRINT USING "#";B%(i, 30)
            LOCATE 17, 49: PRINT USING "##";B%(i, 18)
            LOCATE 17, 67: PRINT USING "###";B%(i, 20)
            LOCATE 22, 27: PRINT USING "###";B%(i, 56)
            LOCATE 22, 32: PRINT USING "###";B%(i, 57)
            LOCATE 22, 37: PRINT USING "###";B%(i, 58)
            LOCATE 22, 42: PRINT USING "###";B%(i, 59)
            LOCATE 22, 47: PRINT USING "###";B%(i, 60)
            LOCATE 22, 52: PRINT USING "###";B%(i, 61)
            LOCATE 23, 27: PRINT USING "###";B%(i, 62)
            LOCATE 23, 32: PRINT USING "###";B%(i, 63)
            LOCATE 23, 37: PRINT USING "###";B%(i, 64)
            LOCATE 23, 42: PRINT USING "###";B%(i, 65)
            LOCATE 23, 47: PRINT USING "###";B%(i, 66)
            LOCATE 23, 52: PRINT USING "###";B%(i, 67)
            LOCATE 24, 27: PRINT USING "###";B%(i, 68);
            LOCATE 24, 32: PRINT USING "###";B%(i, 69);
            LOCATE 24, 37: PRINT USING "###";B%(i, 70);
            LOCATE 24, 42: PRINT USING "###";B%(i, 71);
            LOCATE 24, 47: PRINT USING "###";B%(i, 72);
            LOCATE 24, 52: PRINT USING "###";B%(i, 73);
            LOCATE 25, 27: PRINT USING "###";B%(i, 74);
            LOCATE 25, 32: PRINT USING "###";B%(i, 75);
            LOCATE 25, 37: PRINT USING "###";B%(i, 76);
            LOCATE 25, 42: PRINT USING "###";B%(i, 77);
            LOCATE 25, 47: PRINT USING "###";B%(i, 78);
            LOCATE 25, 52: PRINT USING "###";B%(i, 79);

        BName:
            LOCATE 3, 7
            Max = 15
            CALL TextIn(B$(i), Max, NumOnly, CapsOn, XCode, FG, BG)
            IF XCode = 1 GOTO BName:
            IF XCode = 2 GOTO bDone

        BATS:
            LOCATE 3, 30
            Max = 2
            CALL TextIn(BATS$, Max, NumOnly, CapsOn, XCode, FG, BG)
            IF XCode = 1 GOTO BName:
            IF XCode = 2 GOTO bDone

        BActive:
            LOCATE 3, 44
            Max = 1
            CALL TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
            IF XCode = 1 GOTO BATS:
            IF XCode = 2 GOTO bDone

        BINJ:
            LOCATE 3, 62
            Max = 3
            CALL NumIn(INJ%(i), Max, XCode, FG, BG)
            locate 3,62:print using "###";INJ%(i)
            IF XCode = 1 GOTO BActive:
            IF XCode = 2 GOTO bDone

        BGAMES:
            LOCATE 6, 12
            Max = 3
            CALL NumIn(B%(i, 3), Max, XCode, FG, BG)
            LOCATE 6, 12:PRINT USING "###";B%(I,3)
            IF XCode = 1 GOTO BINJ:
            IF XCode = 2 GOTO bDone

        BAB:
            LOCATE 6, 17
            Max = 3
            CALL NumIn(B%(i, 4), Max, XCode, FG, BG)
            LOCATE 6, 17:PRINT USING "###";B%(I,4)
            IF XCode = 1 GOTO BGAMES:
            IF XCode = 2 GOTO bDone

        BR:
            LOCATE 6, 22
            Max = 3
            CALL NumIn(B%(i, 5), Max, XCode, FG, BG)
            LOCATE 6, 22:PRINT USING "###";B%(I,5)
            IF XCode = 1 GOTO BAB:
            IF XCode = 2 GOTO bDone

        BH:
            LOCATE 6, 27
            Max = 3
            CALL NumIn(B%(i, 6), Max, XCode, FG, BG)
            LOCATE 6, 27:PRINT USING "###";B%(I,6)
            IF XCode = 1 GOTO BR:
            IF XCode = 2 GOTO bDone

        B2B:
            LOCATE 6, 32
            Max = 2
            CALL NumIn(B%(i, 7), Max, XCode, FG, BG)
            LOCATE 6, 32:PRINT USING "##";B%(I,7)
            IF XCode = 1 GOTO BH:
            IF XCode = 2 GOTO bDone

        B3B:
            LOCATE 6, 36
            Max = 2
            CALL NumIn(B%(i, 8), Max, XCode, FG, BG)
            LOCATE 6, 36:PRINT USING "##";B%(I,8)
            IF XCode = 1 GOTO B2B:
            IF XCode = 2 GOTO bDone

        bHR:
            LOCATE 6, 40
            Max = 2
            CALL NumIn(B%(i, 9), Max, XCode, FG, BG)
            LOCATE 6, 40:PRINT USING "##";B%(I,9)
            IF XCode = 1 GOTO B3B:
            IF XCode = 2 GOTO bDone

        BRBI:
            LOCATE 6, 44
            Max = 3
            CALL NumIn(B%(i, 10), Max, XCode, FG, BG)
            LOCATE 6, 44:PRINT USING "###";B%(I,10)
            IF XCode = 1 GOTO bHR:
            IF XCode = 2 GOTO bDone

        bBB:
            LOCATE 6, 49
            Max = 3
            CALL NumIn(B%(i, 11), Max, XCode, FG, BG)
            LOCATE 6, 49:PRINT USING "###";B%(I,11)
            IF XCode = 1 GOTO BRBI:
            IF XCode = 2 GOTO bDone

        bKO:
            LOCATE 6, 54
            Max = 3
            CALL NumIn(B%(i, 12), Max, XCode, FG, BG)
            LOCATE 6, 54:PRINT USING "###";B%(I,12)
            IF XCode = 1 GOTO bBB:
            IF XCode = 2 GOTO bDone

        bSB:
            LOCATE 6, 59
            Max = 3
            CALL NumIn(B%(i, 13), Max, XCode, FG, BG)
            LOCATE 6, 59:PRINT USING "###";B%(I,13)
            IF XCode = 1 GOTO bKO:
            IF XCode = 2 GOTO bDone

        bCS:
            LOCATE 6, 64
            Max = 2
            CALL NumIn(B%(i, 14), Max, XCode, FG, BG)
            LOCATE 6, 64:PRINT USING "##";B%(I,14)
            IF XCode = 1 GOTO bSB:
            IF XCode = 2 GOTO bDone

        BSH:
            LOCATE 6, 68
            Max = 2
            CALL NumIn(B%(i, 15), Max, XCode, FG, BG)
            LOCATE 6, 68:PRINT USING "##";B%(I,15)
            IF XCode = 1 GOTO bCS:
            IF XCode = 2 GOTO bDone

        BLAB:
            LOCATE 7, 17
            Max = 3
            CALL NumIn(B%(i, 42), Max, XCode, FG, BG)
            LOCATE 7, 17:PRINT USING "###";B%(I,42)
            IF XCode = 1 GOTO BSH:
            IF XCode = 2 GOTO bDone

        BLH:
            LOCATE 7, 27
            Max = 3
            CALL NumIn(B%(i, 43), Max, XCode, FG, BG)
            LOCATE 7, 27:PRINT USING "###";B%(I,43)
            IF XCode = 1 GOTO BLAB:
            IF XCode = 2 GOTO bDone

        BL2B:
            LOCATE 7, 32
            Max = 2
            CALL NumIn(B%(i, 44), Max, XCode, FG, BG)
            LOCATE 7, 32:PRINT USING "##";B%(I,44)
            IF XCode = 1 GOTO BLH:
            IF XCode = 2 GOTO bDone

        BL3B:
            LOCATE 7, 36
            Max = 2
            CALL NumIn(B%(i, 45), Max, XCode, FG, BG)
            LOCATE 7, 36:PRINT USING "##";B%(I,45)
            IF XCode = 1 GOTO BL2B:
            IF XCode = 2 GOTO bDone

        BLHR:
            LOCATE 7, 40
            Max = 2
            CALL NumIn(B%(i, 46), Max, XCode, FG, BG)
            LOCATE 7, 40:PRINT USING "##";B%(I,46)
            IF XCode = 1 GOTO BL3B:
            IF XCode = 2 GOTO bDone

        bLBB:
            LOCATE 7, 49
            Max = 3
            CALL NumIn(B%(i, 47), Max, XCode, FG, BG)
            LOCATE 7, 49:PRINT USING "###";B%(I,47)
            IF XCode = 1 GOTO BLHR:
            IF XCode = 2 GOTO bDone

        BLKO:
            LOCATE 7, 54
            Max = 3
            CALL NumIn(B%(i, 48), Max, XCode, FG, BG)
            LOCATE 7, 54:PRINT USING "###";B%(I,48)
            IF XCode = 1 GOTO bLBB:
            IF XCode = 2 GOTO bDone

        BRAB:
            LOCATE 8, 17
            Max = 3
            CALL NumIn(B%(i, 49), Max, XCode, FG, BG)
            LOCATE 8, 17:PRINT USING "###";B%(I,49)
            IF XCode = 1 GOTO BLKO:
            IF XCode = 2 GOTO bDone

        BRH:
            LOCATE 8, 27
            Max = 3
            CALL NumIn(B%(i, 50), Max, XCode, FG, BG)
            LOCATE 8, 27:PRINT USING "###";B%(I,50)
            IF XCode = 1 GOTO BRAB:
            IF XCode = 2 GOTO bDone

        BR2B:
            LOCATE 8, 32
            Max = 2
            CALL NumIn(B%(i, 51), Max, XCode, FG, BG)
            LOCATE 8, 32:PRINT USING "##";B%(I,51)
            IF XCode = 1 GOTO BRH:
            IF XCode = 2 GOTO bDone

        BR3B:
            LOCATE 8, 36
            Max = 2
            CALL NumIn(B%(i, 52), Max, XCode, FG, BG)
            LOCATE 8, 36:PRINT USING "##";B%(I,52)
            IF XCode = 1 GOTO BR2B:
            IF XCode = 2 GOTO bDone

        BRHR:
            LOCATE 8, 40
            Max = 2
            CALL NumIn(B%(i, 53), Max, XCode, FG, BG)
            LOCATE 8, 40:PRINT USING "##";B%(I,53)
            IF XCode = 1 GOTO BR3B:
            IF XCode = 2 GOTO bDone

        bRBB:
            LOCATE 8, 49
            Max = 3
            CALL NumIn(B%(i, 54), Max, XCode, FG, BG)
            LOCATE 8, 49:PRINT USING "###";B%(I,54)
            IF XCode = 1 GOTO BRHR:
            IF XCode = 2 GOTO bDone

        BRKO:
            LOCATE 8, 54
            Max = 3
            CALL NumIn(B%(i, 55), Max, XCode, FG, BG)
            LOCATE 8, 54:PRINT USING "###";B%(I,55)
            IF XCode = 1 GOTO bRBB:
            IF XCode = 2 GOTO bDone

        BP1:
            LOCATE 12, 31
            Max = 2
            CALL NumIn(B%(i, 22), Max, XCode, FG, BG)
            LOCATE 12, 31:PRINT USING "##";B%(I,22)
            IF XCode = 1 GOTO BRKO:
            IF XCode = 2 GOTO bDone

        ' STOP HERE

        BF1:
            LOCATE 12, 36
            Max = 4
            CALL NumIn(B%(i, 26), Max, XCode, FG, BG)
            locate 12,36:print using "####";B%(i,26)
            IF XCode = 1 GOTO BP1:
            IF XCode = 2 GOTO bDone

        bA1:
            LOCATE 12, 43
            Max = 1
            CALL NumIn(B%(i, 33), Max, XCode, FG, BG)
            locate 12,43:print using "#";B%(i, 33)
            IF XCode = 1 GOTO BF1:
            IF XCode = 2 GOTO bDone

        BR1:
            LOCATE 12, 47
            Max = 1
            CALL NumIn(B%(i, 34), Max, XCode, FG, BG)
            locate 12,47:print using "#";B%(i,34)
            IF XCode = 1 GOTO bA1:
            IF XCode = 2 GOTO bDone

        BP2:
            LOCATE 13, 31
            Max = 2
            CALL NumIn(B%(i, 23), Max, XCode, FG, BG)
            locate 13,31:print using "##";B%(i,23)
            IF XCode = 1 GOTO BR1:
            IF XCode = 2 GOTO bDone

        BF2:
            LOCATE 13, 36
            Max = 4
            CALL NumIn(B%(i, 27), Max, XCode, FG, BG)
            locate 13,36:print using "####";B%(i,27)
            IF XCode = 1 GOTO BP2:
            IF XCode = 2 GOTO bDone

        bA2:
            LOCATE 13, 43
            Max = 1
            CALL NumIn(B%(i, 35), Max, XCode, FG, BG)
            locate 13,43:print using "#";B%(i,35)
            IF XCode = 1 GOTO BF2:
            IF XCode = 2 GOTO bDone

        BR2:
            LOCATE 13, 47
            Max = 1
            CALL NumIn(B%(i, 36), Max, XCode, FG, BG)
            locate 13,47:print using "#";B%(i,36)
            IF XCode = 1 GOTO bA2:
            IF XCode = 2 GOTO bDone

        BP3:
            LOCATE 14, 31
            Max = 2
            CALL NumIn(B%(i, 24), Max, XCode, FG, BG)
            locate 14,31:print using "##";B%(i,24)
            IF XCode = 1 GOTO BR2:
            IF XCode = 2 GOTO bDone

        BF3:
            LOCATE 14, 36
            Max = 4
            CALL NumIn(B%(i, 28), Max, XCode, FG, BG)
            locate 14,36:print using "####";B%(i,28)
            IF XCode = 1 GOTO BP3:
            IF XCode = 2 GOTO bDone

        bA3:
            LOCATE 14, 43
            Max = 1
            CALL NumIn(B%(i, 37), Max, XCode, FG, BG)
            locate 14,43:print using "#";B%(i,37)
            IF XCode = 1 GOTO BF3:
            IF XCode = 2 GOTO bDone

        BR3:
            LOCATE 14, 47
            Max = 1
            CALL NumIn(B%(i, 38), Max, XCode, FG, BG)
            locate 14,47:print using "#";B%(i,38)
            IF XCode = 1 GOTO bA3:
            IF XCode = 2 GOTO bDone

        BP4:
            LOCATE 15, 31
            Max = 2
            CALL NumIn(B%(i, 25), Max, XCode, FG, BG)
            locate 15,31:print using "##";B%(i,25)
            IF XCode = 1 GOTO BR3:
            IF XCode = 2 GOTO bDone

        BF4:
            LOCATE 15, 36
            Max = 4
            CALL NumIn(B%(i, 29), Max, XCode, FG, BG)
            locate 15,36:print using "####";B%(i,29)
            IF XCode = 1 GOTO BP4:
            IF XCode = 2 GOTO bDone

        bA4:
            LOCATE 15, 43
            Max = 1
            CALL NumIn(B%(i, 39), Max, XCode, FG, BG)
            locate 15,43:print using "#";B%(i,39)
            IF XCode = 1 GOTO BF4:
            IF XCode = 2 GOTO bDone

        bR4:
            LOCATE 15, 47
            Max = 1
            CALL NumIn(B%(i, 40), Max, XCode, FG, BG)
            locate 15,47:print using "#";B%(i,40)
            IF XCode = 1 GOTO bA4:
            IF XCode = 2 GOTO bDone

        bRUN:
            LOCATE 17, 23
            Max = 1
            CALL NumIn(B%(i, 16), Max, XCode, FG, BG)
            locate 17,23:print using "#";B%(i,16)
            IF XCode = 1 GOTO bR4:
            IF XCode = 2 GOTO bDone

        bBUNT:
            LOCATE 17, 39
            Max = 1
            CALL NumIn(B%(i, 30), Max, XCode, FG, BG)
            locate 17,39:print using "#";B%(i,30)
            IF XCode = 1 GOTO bRUN:
            IF XCode = 2 GOTO bDone

        bGA:
            LOCATE 17, 49
            Max = 2
            CALL NumIn(B%(i, 18), Max, XCode, FG, BG)
            locate 17,49:print using "##";B%(i,18)
            IF XCode = 1 GOTO bBUNT:
            IF XCode = 2 GOTO bDone

        bLA:
            LOCATE 17, 67
            Max = 3
            CALL NumIn(B%(i, 20), Max, XCode, FG, BG)
            locate 17,67:print using "###";B%(i,20)
            IF XCode = 1 GOTO bGA:
            IF XCode = 2 GOTO bDone

        bLLBA:
            LOCATE 22, 27
            Max = 3
            CALL NumIn(B%(i, 56), Max, XCode, FG, BG)
            locate 22,27:print using "###";B%(i,56)
            IF XCode = 1 GOTO bLA:
            IF XCode = 2 GOTO bDone

        bLL2B:
            LOCATE 22, 32
            Max = 3
            CALL NumIn(B%(i, 57), Max, XCode, FG, BG)
            locate 22,32:print using "###";B%(i,57)
            IF XCode = 1 GOTO bLLBA:
            IF XCode = 2 GOTO bDone

        bLL3B:
            LOCATE 22, 37
            Max = 3
            CALL NumIn(B%(i, 58), Max, XCode, FG, BG)
            locate 22,37:print using "###";B%(i,58)
            IF XCode = 1 GOTO bLL2B:
            IF XCode = 2 GOTO bDone

        bLLHR:
            LOCATE 22, 42
            Max = 3
            CALL NumIn(B%(i, 59), Max, XCode, FG, BG)
            locate 22,42:print using "###";B%(i,59)
            IF XCode = 1 GOTO bLL3B:
            IF XCode = 2 GOTO bDone

        bLLBB:
            LOCATE 22, 47
            Max = 3
            CALL NumIn(B%(i, 60), Max, XCode, FG, BG)
            locate 22,47:print using "###";B%(i,60)
            IF XCode = 1 GOTO bLLHR:
            IF XCode = 2 GOTO bDone

        bLLKO:
            LOCATE 22, 52
            Max = 3
            CALL NumIn(B%(i, 61), Max, XCode, FG, BG)
            locate 22,52:print using "###";B%(i,61)
            IF XCode = 1 GOTO bLLBB:
            IF XCode = 2 GOTO bDone

        bLRBA:
            LOCATE 23, 27
            Max = 3
            CALL NumIn(B%(i, 62), Max, XCode, FG, BG)
            locate 23,27:print using "###";B%(i,62)
            IF XCode = 1 GOTO bLLKO:
            IF XCode = 2 GOTO bDone

        bLR2B:
            LOCATE 23, 32
            Max = 3
            CALL NumIn(B%(i, 63), Max, XCode, FG, BG)
            locate 23,32:print using "###";B%(i,63)
            IF XCode = 1 GOTO bLRBA:
            IF XCode = 2 GOTO bDone

        bLR3B:
            LOCATE 23, 37
            Max = 3
            CALL NumIn(B%(i, 64), Max, XCode, FG, BG)
            locate 23,37:print using "###";B%(i,64)
            IF XCode = 1 GOTO bLR2B:
            IF XCode = 2 GOTO bDone

        bLRHR:
            LOCATE 23, 42
            Max = 3
            CALL NumIn(B%(i, 65), Max, XCode, FG, BG)
            locate 23,42:print using "###";B%(i,65)
            IF XCode = 1 GOTO bLR3B:
            IF XCode = 2 GOTO bDone

        bLRBB:
            LOCATE 23, 47
            Max = 3
            CALL NumIn(B%(i, 66), Max, XCode, FG, BG)
            locate 23,47:print using "###";B%(i,66)
            IF XCode = 1 GOTO bLRHR:
            IF XCode = 2 GOTO bDone

        bLRKO:
            LOCATE 23, 52
            Max = 3
            CALL NumIn(B%(i, 67), Max, XCode, FG, BG)
            locate 23,52:print using "###";B%(i,67)
            IF XCode = 1 GOTO bLRBB:
            IF XCode = 2 GOTO bDone

        bRLBA:
            LOCATE 24, 27
            Max = 3
            CALL NumIn(B%(i, 68), Max, XCode, FG, BG)
            locate 24,27:print using "###";B%(i,68);
            IF XCode = 1 GOTO bLRKO:
            IF XCode = 2 GOTO bDone

        bRL2B:
            LOCATE 24, 32
            Max = 3
            CALL NumIn(B%(i, 69), Max, XCode, FG, BG)
            locate 24,32:print using "###";B%(i,69);
            IF XCode = 1 GOTO bRLBA:
            IF XCode = 2 GOTO bDone

        bRL3B:
            LOCATE 24, 37
            Max = 3
            CALL NumIn(B%(i, 70), Max, XCode, FG, BG)
            locate 24,37:print using "###";B%(i,70);
            IF XCode = 1 GOTO bRL2B:
            IF XCode = 2 GOTO bDone

        bRLHR:
            LOCATE 24, 42
            Max = 3
            CALL NumIn(B%(i, 71), Max, XCode, FG, BG)
            locate 24,42:print using "###";B%(i,71);
            IF XCode = 1 GOTO bRL3B:
            IF XCode = 2 GOTO bDone

        bRLBB:
            LOCATE 24, 47
            Max = 3
            CALL NumIn(B%(i, 72), Max, XCode, FG, BG)
            locate 24,47:print using "###";B%(i,72);
            IF XCode = 1 GOTO bRLHR:
            IF XCode = 2 GOTO bDone

        bRLKO:
            LOCATE 24, 52
            Max = 3
            CALL NumIn(B%(i, 73), Max, XCode, FG, BG)
            locate 24,52:print using "###";B%(i,73);
            IF XCode = 1 GOTO bRLBB:
            IF XCode = 2 GOTO bDone

        bRRBA:
            LOCATE 25, 27
            Max = 3
            CALL NumIn(B%(i, 74), Max, XCode, FG, BG)
            locate 25,27:print using "###";B%(i,74);
            IF XCode = 1 GOTO bRLKO:
            IF XCode = 2 GOTO bDone

        bRR2B:
            LOCATE 25, 32
            Max = 3
            CALL NumIn(B%(i, 75), Max, XCode, FG, BG)
            locate 25,32:print using "###";B%(i,75);
            IF XCode = 1 GOTO bRRBA:
            IF XCode = 2 GOTO bDone

        bRR3B:
            LOCATE 25, 37
            Max = 3
            CALL NumIn(B%(i, 76), Max, XCode, FG, BG)
            locate 25,37:print using "###";B%(i,76);
            IF XCode = 1 GOTO bRR2B:
            IF XCode = 2 GOTO bDone

        bRRHR:
            LOCATE 25, 42
            Max = 3
            CALL NumIn(B%(i, 77), Max, XCode, FG, BG)
            locate 25,42:print using "###";B%(i,77);
            IF XCode = 1 GOTO bRR3B:
            IF XCode = 2 GOTO bDone

        bRRBB:
            LOCATE 25, 47
            Max = 3
            CALL NumIn(B%(i, 78), Max, XCode, FG, BG)
            locate 25,47:print using "###";B%(i,78);
            IF XCode = 1 GOTO bRRHR:
            IF XCode = 2 GOTO bDone

        bRRKO:
            LOCATE 25, 52
            Max = 3
            CALL NumIn(B%(i, 79), Max, XCode, FG, BG)
            locate 25,52:print using "###";B%(i,79);
            IF XCode = 1 GOTO bRRBB:

        bDone:
            COLOR 15,1
            LOCATE 25, 1
            PRINT "Is this okay? (Yes/No) ";
            X$ = ""
            CALL YesNoB(X$, XCode, FG, BG)
            LOCATE , 1
            IF XCode = 1 GOTO bRRKO
            IF X$ = "N" GOTO BName
            IF X$ <> "Y" GOTO bDone               'insist on either Y or N or up-arrow

            B%(i, 0) = 0
            BATS$ = _TRIM$(BATS$)
            IF UCASE$(BATS$) = "L" THEN B%(i, 0) = -1
            IF UCASE$(BATS$) = "R" THEN B%(i, 0) = 1
            IF UCASE$(BATS$) = "LP" THEN B%(i, 0) = -2
            IF UCASE$(BATS$) = "RP" THEN B%(i, 0) = 2

            ACT$ = _TRIM$(ACT$)
            B%(i, 31) = 0
            IF UCASE$(ACT$) = "N" THEN B%(i, 31) = 1
            
        End If

    Loop Until i = 0


END SUB


'----------------------------------------
'       EDITTEAM subroutine
'----------------------------------------
'...explanation...
SUB EDITTEAM (YN$, T$, NN$, S$, M$, T)

    Shared K9&()
    Shared L%(), SA%()

    COLOR 15, 1: CLS
    LOCATE 1, 36: PRINT "TEAM DATA"
    LOCATE 3, 1: PRINT "TEAM NUMBER:"
    LOCATE 5, 1: PRINT "LEAGUE BATTING AVERAGE:"
    LOCATE 6, 1: PRINT "LEAGUE STRIKEOUT %    :"
    LOCATE 7, 1: PRINT "LEAGUE WALK PERCENTAGE:"
    LOCATE 8, 1: PRINT "LEAGUE HOME RUN %     :"
    LOCATE 10, 1: PRINT "DBL PLAYS/GAME X 100  :"
    LOCATE 14, 1: PRINT "PARK NAME:"
    LOCATE 15, 1: PRINT "PARK ID  :"
    LOCATE 16, 1: PRINT "DOUBLES RATING  :"
    LOCATE 17, 1: PRINT "TRIPLES RATING  :"
    LOCATE 18, 1: PRINT "HOME RUN RATING :"
    LOCATE 19, 1: PRINT "FOUL BALL RATING:"
    LOCATE 21, 1: PRINT "OUTDOOR(0)/INDOOR(1):"
    LOCATE 22, 1: PRINT "GRASS(0)/TURF(1)    :"
    LOCATE 24, 1: PRINT "MANAGER :";
    LOCATE 18, 29: PRINT "TEXT COLOR      :"
    LOCATE 19, 29: PRINT "BACKGROUND COLOR:"
    LOCATE 3, 25: PRINT "DISK ID:"
    LOCATE 3, 44: PRINT "TEAM NAME:"
    LOCATE 4, 44: PRINT "ABBREVIATION:"
    LOCATE 6, 51: PRINT "SPLIT STATS LEAGUE DATA"
    LOCATE 7, 50: PRINT "BAVG   K%  BB%  HR%  2B%  3B%"
    LOCATE 8, 38: PRINT "LHB vs LHP"
    LOCATE 9, 38: PRINT "LHB vs RHP"
    LOCATE 10, 38: PRINT "RHB vs LHP"
    LOCATE 11, 38: PRINT "RHB vs RHP"

' INITIALIZE THE FIELDS
    Color 0,7
    'COLR = 112
    LOCATE 3, 14: PRINT USING "###";T
    LOCATE 3, 33: PRINT YN$
    LOCATE 3, 55: PRINT T$
    LOCATE 4, 58: PRINT NN$
    LOCATE 5, 25: PRINT USING "###";L%(1)
    LOCATE 6, 25: PRINT USING "###";L%(2)
    LOCATE 7, 25: PRINT USING "###";L%(3)
    LOCATE 8, 25: PRINT USING "###";L%(4)
    LOCATE 10, 25:PRINT USING "###";L%(5)
    LOCATE 14, 12: PRINT S$
    LOCATE 15, 12: PRINT USING "###";L%(13)
    LOCATE 16, 19: PRINT USING "###";L%(6)
    LOCATE 17, 19: PRINT USING "###";L%(7)
    LOCATE 18, 19: PRINT USING "######";K9&(0)
    LOCATE 19, 19: PRINT USING "###";L%(8)
    LOCATE 21, 25: PRINT USING "#";L%(9)
    LOCATE 22, 25: PRINT USING "#";L%(10)
    LOCATE 24, 11: PRINT M$;
    LOCATE 18, 46: PRINT USING "##";L%(11)
    LOCATE 19, 46: PRINT USING "##";L%(12)
    LOCATE 8, 50: PRINT USING "###";SA%(1)
    LOCATE 8, 55: PRINT USING "###";SA%(2)
    LOCATE 8, 60: PRINT USING "###";SA%(3)
    LOCATE 8, 65: PRINT USING "###";SA%(4)
    LOCATE 8, 70: PRINT USING "###";SA%(5)
    LOCATE 8, 75: PRINT USING "###";SA%(6)
    LOCATE 9, 50: PRINT USING "###";SA%(7)
    LOCATE 9, 55: PRINT USING "###";SA%(8)
    LOCATE 9, 60: PRINT USING "###";SA%(9)
    LOCATE 9, 65: PRINT USING "###";SA%(10)
    LOCATE 9, 70: PRINT USING "###";SA%(11)
    LOCATE 9, 75: PRINT USING "###";SA%(12)
    LOCATE 10, 50:PRINT USING "###";SA%(13)
    LOCATE 10, 55:PRINT USING "###";SA%(14)
    LOCATE 10, 60:PRINT USING "###";SA%(15)
    LOCATE 10, 65:PRINT USING "###";SA%(16)
    LOCATE 10, 70:PRINT USING "###";SA%(17)
    LOCATE 10, 75:PRINT USING "###";SA%(18)
    LOCATE 11, 50:PRINT USING "###";SA%(19)
    LOCATE 11, 55:PRINT USING "###";SA%(20)
    LOCATE 11, 60:PRINT USING "###";SA%(21)
    LOCATE 11, 65:PRINT USING "###";SA%(22)
    LOCATE 11, 70:PRINT USING "###";SA%(23)
    LOCATE 11, 75:PRINT USING "###";SA%(24)
    LOCATE 25, 1: PRINT "ESC-WILL END EDITING";
    
    HR$ = _TRIM$(STR$(K9&(0)))
    
TeamNum:
    LOCATE 3, 14
    Max = 3
    CALL NumIn(T, Max, XCode, FG, BG)
    LOCATE 3, 14:PRINT USING "###";T
    IF XCode = 1 GOTO TeamNum:
    IF XCode = 2 GOTO Done

DiskID:
    LOCATE 3, 33
    Max = 3
    CALL TextIn(YN$, Max, NumOnly, CapsOn, XCode, FG, BG)
    IF XCode = 1 GOTO TeamNum:
    IF XCode = 2 GOTO Done

TeamName:
    LOCATE 3, 55
    Max = 15
    CALL TextIn(T$, Max, NumOnly, CapsOn, XCode, FG, BG)
    IF XCode = 1 GOTO DiskID:
    IF XCode = 2 GOTO Done

Abbrev:
    LOCATE 4, 58
    Max = 5
    CALL TextIn(NN$, Max, NumOnly, CapsOn, XCode, FG, BG)
    IF XCode = 1 GOTO TeamName:
    IF XCode = 2 GOTO Done

LgBA:
    LOCATE 5, 25
    Max = 3
    CALL NumIn(L%(1), Max, XCode, FG, BG)
    LOCATE 5, 25:PRINT USING "###";L%(1)
    IF XCode = 1 GOTO Abbrev:
    IF XCode = 2 GOTO Done

LgKO:
    LOCATE 6, 25
    Max = 3
    CALL NumIn(L%(2), Max, XCode, FG, BG)
    LOCATE 6, 25:PRINT USING "###";L%(2)
    IF XCode = 1 GOTO LgBA:
    IF XCode = 2 GOTO Done

LgBB:
    LOCATE 7, 25
    Max = 3
    CALL NumIn(L%(3), Max, XCode, FG, BG)
    LOCATE 7, 25:PRINT USING "###";L%(3)
    IF XCode = 1 GOTO LgKO:
    IF XCode = 2 GOTO Done

LgHR:
    LOCATE 8, 25
    Max = 3
    CALL NumIn(L%(4), Max, XCode, FG, BG)
    LOCATE 8, 25:PRINT USING "###";L%(4)
    IF XCode = 1 GOTO LgBB:
    IF XCode = 2 GOTO Done

LgDP:
    LOCATE 10, 25
    Max = 3
    CALL NumIn(L%(5), Max, XCode, FG, BG)
    LOCATE 10, 25:PRINT USING "###";L%(5)
    IF XCode = 1 GOTO LgHR:
    IF XCode = 2 GOTO Done

ParkName:
    LOCATE 14, 12
    Max = 40
    CALL TextIn(S$, Max, NumOnly, CapsOn, XCode, FG, BG)
    IF XCode = 1 GOTO LgDP:
    IF XCode = 2 GOTO Done

ParkID:
    LOCATE 15, 12
    Max = 3
    CALL NumIn(L%(13), Max, XCode, FG, BG)
    LOCATE 15, 12:PRINT USING "###";L%(13)
    IF XCode = 1 GOTO ParkName:
    IF XCode = 2 GOTO Done

DoubleRating:
    LOCATE 16, 19
    Max = 3
    CALL NumIn(L%(6), Max, XCode, FG, BG)
    LOCATE 16, 19:PRINT USING "###";L%(6)
    IF XCode = 1 GOTO ParkID:
    IF XCode = 2 GOTO Done

TripleRating:
    LOCATE 17, 19
    Max = 3
    CALL NumIn(L%(7), Max, XCode, FG, BG)
    LOCATE 17, 19:PRINT USING "###";L%(7)
    IF XCode = 1 GOTO DoubleRating:
    IF XCode = 2 GOTO Done

Homer:
    LOCATE 18, 19
    Max = 6
    CALL TextIn(HR$, Max, NumOnly, CapsOn, XCode, FG, BG)
    IF XCode = 1 GOTO TripleRating:
    IF XCode = 2 GOTO Done

Foul:
    LOCATE 19, 19
    Max = 3
    CALL NumIn(L%(8), Max, XCode, FG, BG)
    LOCATE 19, 19:PRINT USING "###";L%(8)
    IF XCode = 1 GOTO Homer:
    IF XCode = 2 GOTO Done

Outdoor:
    LOCATE 21, 25
    Max = 1
    CALL NumIn(L%(9), Max, XCode, FG, BG)
    LOCATE 21, 25:PRINT USING "#";L%(9)
    IF XCode = 1 GOTO Foul:
    IF XCode = 2 GOTO Done

Grass:
    LOCATE 22, 25
    Max = 1
    CALL NumIn(L%(10), Max, XCode, FG, BG)
    LOCATE 22, 25:PRINT USING "#";L%(10)
    IF XCode = 1 GOTO Outdoor:
    IF XCode = 2 GOTO Done

Manager:
    LOCATE 24, 11
    Max = 15
    CALL TextIn(M$, Max, NumOnly, CapsOn, XCode, FG, BG)
    IF XCode = 1 GOTO Grass:
    IF XCode = 2 GOTO Done

Text:
    LOCATE 18, 46
    Max = 2
    CALL NumIn(L%(11), Max, XCode, FG, BG)
    LOCATE 18, 46:PRINT USING "##";L%(11)
    IF XCode = 1 GOTO Manager:
    IF XCode = 2 GOTO Done

Bgrnd:
    LOCATE 19, 46
    Max = 2
    CALL NumIn(L%(12), Max, XCode, FG, BG)
    LOCATE 19, 46:PRINT USING "##";L%(12)
    IF XCode = 1 GOTO Text:
    IF XCode = 2 GOTO Done

LOCATE 20, 29: COLOR L%(11), L%(12): PRINT "SAMPLE TEXT": COLOR 0,7

LLBA:
    LOCATE 8, 50
    Max = 3
    CALL NumIn(SA%(1), Max, XCode, FG, BG)
    LOCATE 8, 50:PRINT USING "###";SA%(1)
    IF XCode = 1 GOTO Bgrnd:
    IF XCode = 2 GOTO Done

LL2B:
    LOCATE 8, 55
    Max = 3
    CALL NumIn(SA%(2), Max, XCode, FG, BG)
    LOCATE 8, 55:PRINT USING "###";SA%(2)
    IF XCode = 1 GOTO LLBA:
    IF XCode = 2 GOTO Done

LL3B:
    LOCATE 8, 60
    Max = 3
    CALL NumIn(SA%(3), Max, XCode, FG, BG)
    LOCATE 8, 60:PRINT USING "###";SA%(3)
    IF XCode = 1 GOTO LL2B:
    IF XCode = 2 GOTO Done

LLHR:
    LOCATE 8, 65
    Max = 3
    CALL NumIn(SA%(4), Max, XCode, FG, BG)
    LOCATE 8, 65:PRINT USING "###";SA%(4)
    IF XCode = 1 GOTO LL3B:
    IF XCode = 2 GOTO Done

LLBB:
    LOCATE 8, 70
    Max = 3
    CALL NumIn(SA%(5), Max, XCode, FG, BG)
    LOCATE 8, 70:PRINT USING "###";SA%(5)
    IF XCode = 1 GOTO LLHR:
    IF XCode = 2 GOTO Done

LLKO:
    LOCATE 8, 75
    Max = 3
    CALL NumIn(SA%(6), Max, XCode, FG, BG)
    LOCATE 8, 75:PRINT USING "###";SA%(6)
    IF XCode = 1 GOTO LLBB:
    IF XCode = 2 GOTO Done

LRBA:
    LOCATE 9, 50
    Max = 3
    CALL NumIn(SA%(7), Max, XCode, FG, BG)
    LOCATE 9, 50:PRINT USING "###";SA%(7)
    IF XCode = 1 GOTO LLKO:
    IF XCode = 2 GOTO Done

LR2B:
    LOCATE 9, 55
    Max = 3
    CALL NumIn(SA%(8), Max, XCode, FG, BG)
    LOCATE 9, 55:PRINT USING "###";SA%(8)
    IF XCode = 1 GOTO LRBA:
    IF XCode = 2 GOTO Done

LR3B:
    LOCATE 9, 60
    Max = 3
    CALL NumIn(SA%(9), Max, XCode, FG, BG)
    LOCATE 9, 60:PRINT USING "###";SA%(9)
    IF XCode = 1 GOTO LR2B:
    IF XCode = 2 GOTO Done

LRHR:
    LOCATE 9, 65
    Max = 3
    CALL NumIn(SA%(10), Max, XCode, FG, BG)
    LOCATE 9, 65:PRINT USING "###";SA%(10)
    IF XCode = 1 GOTO LR3B:
    IF XCode = 2 GOTO Done

LRBB:
    LOCATE 9, 70
    Max = 3
    CALL NumIn(SA%(11), Max, XCode, FG, BG)
    LOCATE 9, 70:PRINT USING "###";SA%(11)
    IF XCode = 1 GOTO LRHR:
    IF XCode = 2 GOTO Done

LRKO:
    LOCATE 9, 75
    Max = 3
    CALL NumIn(SA%(12), Max, XCode, FG, BG)
    LOCATE 9, 75:PRINT USING "###";SA%(12)
    IF XCode = 1 GOTO LRBB:
    IF XCode = 2 GOTO Done

RLBA:
    LOCATE 10, 50
    Max = 3
    CALL NumIn(SA%(13), Max, XCode, FG, BG)
    LOCATE 10, 50:PRINT USING "###";SA%(13)
    IF XCode = 1 GOTO LRKO:
    IF XCode = 2 GOTO Done

RL2B:
    LOCATE 10, 55
    Max = 3
    CALL NumIn(SA%(14), Max, XCode, FG, BG)
    LOCATE 10, 55:PRINT USING "###";SA%(14)
    IF XCode = 1 GOTO RLBA:
    IF XCode = 2 GOTO Done

RL3B:
    LOCATE 10, 60
    Max = 3
    CALL NumIn(SA%(15), Max, XCode, FG, BG)
    LOCATE 10, 60:PRINT USING "###";SA%(15)
    IF XCode = 1 GOTO RL2B:
    IF XCode = 2 GOTO Done

RLHR:
    LOCATE 10, 65
    Max = 3
    CALL NumIn(SA%(16), Max, XCode, FG, BG)
    LOCATE 10, 65:PRINT USING "###";SA%(16)
    IF XCode = 1 GOTO RL3B:
    IF XCode = 2 GOTO Done

RLBB:
    LOCATE 10, 70
    Max = 3
    CALL NumIn(SA%(17), Max, XCode, FG, BG)
    LOCATE 10, 70:PRINT USING "###";SA%(17)
    IF XCode = 1 GOTO RLHR:
    IF XCode = 2 GOTO Done

RLKO:
    LOCATE 10, 75
    Max = 3
    CALL NumIn(SA%(18), Max, XCode, FG, BG)
    LOCATE 10, 75:PRINT USING "###";SA%(18)
    IF XCode = 1 GOTO RLBB:
    IF XCode = 2 GOTO Done

RRBA:
    LOCATE 11, 50
    Max = 3
    CALL NumIn(SA%(19), Max, XCode, FG, BG)
    LOCATE 11, 50:PRINT USING "###";SA%(19)
    IF XCode = 1 GOTO RLKO:
    IF XCode = 2 GOTO Done

RR2B:
    LOCATE 11, 55
    Max = 3
    CALL NumIn(SA%(20), Max, XCode, FG, BG)
    LOCATE 11, 55:PRINT USING "###";SA%(20)
    IF XCode = 1 GOTO RRBA:
    IF XCode = 2 GOTO Done

RR3B:
    LOCATE 11, 60
    Max = 3
    CALL NumIn(SA%(21), Max, XCode, FG, BG)
    LOCATE 11, 60:PRINT USING "###";SA%(21)
    IF XCode = 1 GOTO RR2B:
    IF XCode = 2 GOTO Done

RRHR:
    LOCATE 11, 65
    Max = 3
    CALL NumIn(SA%(22), Max, XCode, FG, BG)
    LOCATE 11, 65:PRINT USING "###";SA%(22)
    IF XCode = 1 GOTO RR3B:
    IF XCode = 2 GOTO Done

RRBB:
    LOCATE 11, 70
    Max = 3
    CALL NumIn(SA%(23), Max, XCode, FG, BG)
    LOCATE 11, 70:PRINT USING "###";SA%(23)
    IF XCode = 1 GOTO RRHR:
    IF XCode = 2 GOTO Done

RRKO:
    LOCATE 11, 75
    Max = 3
    CALL NumIn(SA%(24), Max, XCode, FG, BG)
    LOCATE 11, 75:PRINT USING "###";SA%(24)
    IF XCode = 1 GOTO RRBB:

Done:
    Do
        LOCATE 25, 1
        PRINT "Is this okay? (Y)es/(N)o/(A)bort ";
        X$ = ""
        CALL YesNoB(X$, XCode, FG, BG)
        LOCATE , 1
        'ClearEOL 14, 1
        IF XCode = 1 GOTO RRKO

        Select Case X$
            Case "A": Exit Sub
            Case "N": GoTo TeamNum
            Case Else: GOTO Done
        End Select

    Loop Until X$ = "N"     'insist on either Y or N or up-arrow

    K9&(0) = VAL(HR$)

END SUB



'----------------------------------------
'       PITCHEREDIT subroutine
'----------------------------------------
'...explanation...
SUB PITCHEREDIT ()

    Shared P$()
    Shared draftP%()

    Do
        COLOR 15, 0:
        CLS
        PRINT "EDIT/ENTER PITCHERS"
        X% = 2
        FOR i = 0 TO 10
            LOCATE i + 5, 5: PRINT USING "##"; i + 1;
            PRINT " "; P$(i); TAB(40);
            PRINT USING "##"; i + 12;
            PRINT " "; P$(i + 11)
        NEXT


        Do
            LOCATE 22, 1
            PRINT "# TO EDIT/ENTER (1-22) (0 TO CONTINUE) ";
            INPUT i
        Loop Until i >= 0 And i <= 22
        i = i - 1

        'user chose a number
        IF i >= 0 THEN

            ' INITIALIZE PITCHER SCREEN
            COLOR 15, 1: CLS
            LOCATE 1, 1: PRINT "NAME:"
            LOCATE 1, 24: PRINT "THROWS:"
            LOCATE 1, 36: PRINT "ACTIVE:"
            LOCATE 1, 48: PRINT "INJ LENGTH:"
            LOCATE 1, 66: PRINT "HOLD (1-9):"
            LOCATE 3, 34: PRINT "PITCHING STATS"
            LOCATE 4, 11: PRINT "W   L  SV    G  GS  CG   IP    H   BB    K  WP   ERA  HRA"
            LOCATE 5, 1: PRINT "SEASON:"
            LOCATE 6, 27: PRINT "G/A:"
            LOCATE 6, 37: PRINT "LEAGUE BAVG:"
            LOCATE 8, 31: PRINT "SPLIT STATS AGAINST"
            LOCATE 9, 26: PRINT "AB    H  2B  3B  HR   BB    K"
            LOCATE 10, 1: PRINT "VS LHB:"
            LOCATE 11, 1: PRINT "VS RHB:"
            LOCATE 13, 30: PRINT "HITTING STATISTICS"
            LOCATE 14, 9: PRINT "RUN (1-9):"
            LOCATE 14, 24: PRINT "BUNT (1-9):"
            LOCATE 14, 39: PRINT "BATTING AVG:"
            LOCATE 14, 58: PRINT "FLDG AVG:"
            LOCATE 15, 14: PRINT "G   AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
            LOCATE 16, 1: PRINT "SEASON:"
            LOCATE 17, 1: PRINT "VS LHP:"
            LOCATE 18, 1: PRINT "VS RHP:"
            LOCATE 20, 29: PRINT "SPLIT STATS LEAGUE DATA"
            LOCATE 21, 26: PRINT "BAVG   K%  BB%  HR%  2B%  3B%"
            LOCATE 22, 13: PRINT "LHB vs LHP"
            LOCATE 23, 13: PRINT "LHB vs RHP"
            LOCATE 24, 13: PRINT "RHB vs LHP";
            LOCATE 25, 13: PRINT "RHB vs RHP";

            ' INITIALIZE PITCHER FIELDS
            'FG, BG = 112
            FG = 0
            BG = 7

            LOCATE 1, 7: QPrint PadRight$(P$(i), 15), FG, BG, -1

            THROW$ = "R"
            IF draftP%(i, 0) = -1 THEN THROW$ = "L"
            LOCATE 1, 32: QPrint PadRight$(THROW$, 1), FG, BG, -1

            ACT$ = "Y"
            IF draftP%(i, 34) = 1 THEN ACT$ = "N"
            LOCATE 1, 44: QPrint PadRight$(ACT$, 1), FG, BG, -1
            
            CALL PITCHINIT(i, FG, BG)

            PName:
                LOCATE 1, 7
                CapsOn = 0: NumOnly = 0: Max = 15
                CALL TextIn(P$(i), Max, NumOnly, CapsOn, XCode, FG, BG)
                IF XCode = 1 GOTO PName:
                IF XCode = 2 GOTO PDone

            THROW:
                LOCATE 1, 32
                Max = 1
                CALL TextIn(THROW$, Max, NumOnly, CapsOn, XCode, FG, BG)
                IF XCode = 1 GOTO PName:
                IF XCode = 2 GOTO PDone

            PActive:
                LOCATE 1, 44
                Max = 1
                CALL TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
                IF XCode = 1 GOTO THROW:
                IF XCode = 2 GOTO PDone

            PINJ:
                LOCATE 1, 60
                Max = 3
                CALL NumIn(draftP%(i, 35), Max, XCode, FG, BG)
                LOCATE 1,60:PRINT USING "###";draftP%(I,35)
                IF XCode = 1 GOTO PActive:
                IF XCode = 2 GOTO PDone

            PHOLD:
                LOCATE 1, 78
                Max = 1
                CALL NumIn(draftP%(i, 18), Max, XCode, FG, BG)
                LOCATE 1,78:PRINT USING "#";draftP%(I,18)
                IF XCode = 1 GOTO PINJ:
                IF XCode = 2 GOTO PDone

            PWINS:
                LOCATE 5, 10
                Max = 2
                CALL NumIn(draftP%(i, 1), Max, XCode, FG, BG)
                LOCATE 5, 10:PRINT USING "##";draftP%(I,1)
                IF XCode = 1 GOTO PHOLD:
                IF XCode = 2 GOTO PDone

            PLOSSES:
                LOCATE 5, 14
                Max = 2
                CALL NumIn(draftP%(i, 2), Max, XCode, FG, BG)
                LOCATE 5, 14:PRINT USING "##";draftP%(I,2)
                IF XCode = 1 GOTO PWINS:
                IF XCode = 2 GOTO PDone

            PSAVES:
                LOCATE 5, 18
                Max = 2
                CALL NumIn(draftP%(i, 3), Max, XCode, FG, BG)
                LOCATE 5, 18:PRINT USING "##";draftP%(I,3)
                IF XCode = 1 GOTO PLOSSES:
                IF XCode = 2 GOTO PDone

            PGAMES:
                LOCATE 5, 22
                Max = 3
                CALL NumIn(draftP%(i, 4), Max, XCode, FG, BG)
                LOCATE 5, 22:PRINT USING "###";draftP%(I,4)
                IF XCode = 1 GOTO PSAVES:
                IF XCode = 2 GOTO PDone

            PSTARTS:
                LOCATE 5, 27
                Max = 2
                CALL NumIn(draftP%(i, 5), Max, XCode, FG, BG)
                LOCATE 5, 27:PRINT USING "##";draftP%(I,5)
                IF XCode = 1 GOTO PGAMES:
                IF XCode = 2 GOTO PDone

            PCG:
                LOCATE 5, 31
                Max = 2
                CALL NumIn(draftP%(i, 16), Max, XCode, FG, BG)
                LOCATE 5, 31:PRINT USING "##";draftP%(I,16)
                IF XCode = 1 GOTO PSTARTS:
                IF XCode = 2 GOTO PDone

            PIP:
                LOCATE 5, 35
                Max = 3
                CALL NumIn(draftP%(i, 6), Max, XCode, FG, BG)
                LOCATE 5, 35:PRINT USING "###";draftP%(I,6)
                IF XCode = 1 GOTO PCG:
                IF XCode = 2 GOTO PDone

            PH:
                LOCATE 5, 40
                Max = 3
                CALL NumIn(draftP%(i, 7), Max, XCode, FG, BG)
                LOCATE 5, 40:PRINT USING "###";draftP%(I,7)
                IF XCode = 1 GOTO PIP:
                IF XCode = 2 GOTO PDone

            PBB:
                LOCATE 5, 45
                Max = 3
                CALL NumIn(draftP%(i, 8), Max, XCode, FG, BG)
                LOCATE 5, 45:PRINT USING "###";draftP%(I,8)
                IF XCode = 1 GOTO PH:
                IF XCode = 2 GOTO PDone

            PKO:
                LOCATE 5, 50
                Max = 3
                CALL NumIn(draftP%(i, 9), Max, XCode, FG, BG)
                LOCATE 5, 50:PRINT USING "###";draftP%(I,9)
                IF XCode = 1 GOTO PBB:
                IF XCode = 2 GOTO PDone

            PWP:
                LOCATE 5, 55
                Max = 2
                CALL NumIn(draftP%(i, 33), Max, XCode, FG, BG)
                LOCATE 5, 55:PRINT USING "##";draftP%(I,33)
                IF XCode = 1 GOTO PKO:
                IF XCode = 2 GOTO PDone

            PERA:
                LOCATE 5, 59
                Max = 4
                CALL NumIn(draftP%(i, 10), Max, XCode, FG, BG)
                LOCATE 5, 59:PRINT USING "####";draftP%(I,10)
                IF XCode = 1 GOTO PWP:
                IF XCode = 2 GOTO PDone

            PHRA:
                LOCATE 5, 66
                Max = 2
                CALL NumIn(draftP%(i, 13), Max, XCode, FG, BG)
                LOCATE 5, 66:PRINT USING "##";draftP%(I,13)
                IF XCode = 1 GOTO PERA:
                IF XCode = 2 GOTO PDone

            PGA:
                LOCATE 6, 32
                Max = 2
                CALL NumIn(draftP%(i, 12), Max, XCode, FG, BG)
                LOCATE 6, 32:PRINT USING "##";draftP%(I,12)
                IF XCode = 1 GOTO PHRA:
                IF XCode = 2 GOTO PDone

            PLBA:
                LOCATE 6, 50
                Max = 3
                CALL NumIn(draftP%(i, 14), Max, XCode, FG, BG)
                LOCATE 6, 50:PRINT USING "###";draftP%(I,14)
                IF XCode = 1 GOTO PHRA:
                IF XCode = 2 GOTO PDone

            PLAB:
                LOCATE 10, 25
                Max = 3
                CALL NumIn(draftP%(i, 51), Max, XCode, FG, BG)
                LOCATE 10, 25:PRINT USING "###";draftP%(I,51)
                IF XCode = 1 GOTO PLBA:
                IF XCode = 2 GOTO PDone

            PLH:
                LOCATE 10, 30
                Max = 3
                CALL NumIn(draftP%(i, 52), Max, XCode, FG, BG)
                LOCATE 10, 30:PRINT USING "###";draftP%(I,52)
                IF XCode = 1 GOTO PLAB:
                IF XCode = 2 GOTO PDone

            PL2B:
                LOCATE 10, 35
                Max = 2
                CALL NumIn(draftP%(i, 53), Max, XCode, FG, BG)
                LOCATE 10, 35:PRINT USING "##";draftP%(I,53)
                IF XCode = 1 GOTO PLH:
                IF XCode = 2 GOTO PDone

            PL3B:
                LOCATE 10, 39
                Max = 2
                CALL NumIn(draftP%(i, 54), Max, XCode, FG, BG)
                LOCATE 10, 39:PRINT USING "##";draftP%(I,54)
                IF XCode = 1 GOTO PL2B:
                IF XCode = 2 GOTO PDone

            PLHR:
                LOCATE 10, 43
                Max = 2
                CALL NumIn(draftP%(i, 55), Max, XCode, FG, BG)
                LOCATE 10, 43:PRINT USING "##";draftP%(I,55)
                IF XCode = 1 GOTO PL3B:
                IF XCode = 2 GOTO PDone

            PLBB:
                LOCATE 10, 47
                Max = 3
                CALL NumIn(draftP%(i, 56), Max, XCode, FG, BG)
                LOCATE 10, 47:PRINT USING "###";draftP%(I,56)
                IF XCode = 1 GOTO PLHR:
                IF XCode = 2 GOTO PDone

            PLKO:
                LOCATE 10, 52
                Max = 3
                CALL NumIn(draftP%(i, 57), Max, XCode, FG, BG)
                LOCATE 10, 52:PRINT USING "###";draftP%(I,57)
                IF XCode = 1 GOTO PLBB:
                IF XCode = 2 GOTO PDone

            PRAB:
                LOCATE 11, 25
                Max = 3
                CALL NumIn(draftP%(i, 58), Max, XCode, FG, BG)
                LOCATE 11, 25:PRINT USING "###";draftP%(I,58)
                IF XCode = 1 GOTO PLKO:
                IF XCode = 2 GOTO PDone

            PRH:
                LOCATE 11, 30
                Max = 3
                CALL NumIn(draftP%(i, 59), Max, XCode, FG, BG)
                LOCATE 11, 30:PRINT USING "###";draftP%(I,59)
                IF XCode = 1 GOTO PRAB:
                IF XCode = 2 GOTO PDone

            PR2B:
                LOCATE 11, 35
                Max = 2
                CALL NumIn(draftP%(i, 60), Max, XCode, FG, BG)
                LOCATE 11, 35:PRINT USING "##";draftP%(I,60)
                IF XCode = 1 GOTO PRH:
                IF XCode = 2 GOTO PDone

            PR3B:
                LOCATE 11, 39
                Max = 2
                CALL NumIn(draftP%(i, 61), Max, XCode, FG, BG)
                LOCATE 11, 39:PRINT USING "##";draftP%(I,61)
                IF XCode = 1 GOTO PR2B:
                IF XCode = 2 GOTO PDone

            PRHR:
                LOCATE 11, 43
                Max = 2
                CALL NumIn(draftP%(i, 62), Max, XCode, FG, BG)
                LOCATE 11, 43:PRINT USING "##";draftP%(I,62)
                IF XCode = 1 GOTO PR3B:
                IF XCode = 2 GOTO PDone

            PRBB:
                LOCATE 11, 47
                Max = 3
                CALL NumIn(draftP%(i, 63), Max, XCode, FG, BG)
                LOCATE 11, 47:PRINT USING "###";draftP%(I,63)
                IF XCode = 1 GOTO PRHR:
                IF XCode = 2 GOTO PDone

            PRKO:
                LOCATE 11, 52
                Max = 3
                CALL NumIn(draftP%(i, 64), Max, XCode, FG, BG)
                LOCATE 11, 52:PRINT USING "###";draftP%(I,64)
                IF XCode = 1 GOTO PRBB:
                IF XCode = 2 GOTO PDone

            PRUN:
                LOCATE 14, 20
                Max = 1
                CALL NumIn(draftP%(i, 31), Max, XCode, FG, BG)
                LOCATE 14, 20:PRINT USING "#";draftP%(I,31)
                IF XCode = 1 GOTO PRKO:
                IF XCode = 2 GOTO PDone

            PBUNT:
                LOCATE 14, 36
                Max = 1
                CALL NumIn(draftP%(i, 32), Max, XCode, FG, BG)
                LOCATE 14, 36:PRINT USING "#";draftP%(I,32)
                IF XCode = 1 GOTO PRKO:
                IF XCode = 2 GOTO PDone

            PBAVG:
                LOCATE 14, 52
                Max = 3
                CALL NumIn(draftP%(i, 11), Max, XCode, FG, BG)
                LOCATE 14, 52:PRINT USING "###";draftP%(I,11)
                IF XCode = 1 GOTO PBUNT:
                IF XCode = 2 GOTO PDone

            PFAVG:
                LOCATE 14, 68
                Max = 4
                CALL NumIn(draftP%(i, 17), Max, XCode, FG, BG)
                LOCATE 14, 68:PRINT USING "####";draftP%(I,17)
                IF XCode = 1 GOTO PBAVG:
                IF XCode = 2 GOTO PDone

            PBGAMES:
                LOCATE 16, 12
                Max = 3
                CALL NumIn(draftP%(i, 19), Max, XCode, FG, BG)
                LOCATE 16, 12:PRINT USING "###";draftP%(I,19)
                IF XCode = 1 GOTO PFAVG:
                IF XCode = 2 GOTO PDone

            PAB:
                LOCATE 16, 17
                Max = 3
                CALL NumIn(draftP%(i, 20), Max, XCode, FG, BG)
                LOCATE 16, 17:PRINT USING "###";draftP%(I,20)
                IF XCode = 1 GOTO PBGAMES:
                IF XCode = 2 GOTO PDone

            PR:
                LOCATE 16, 22
                Max = 3
                CALL NumIn(draftP%(i, 21), Max, XCode, FG, BG)
                LOCATE 16, 22:PRINT USING "###";draftP%(I,21)
                IF XCode = 1 GOTO PAB:
                IF XCode = 2 GOTO PDone

            PBH:
                LOCATE 16, 27
                Max = 3
                CALL NumIn(draftP%(i, 22), Max, XCode, FG, BG)
                LOCATE 16, 27:PRINT USING "###";draftP%(I,22)
                IF XCode = 1 GOTO PR:
                IF XCode = 2 GOTO PDone

            P2B:
                LOCATE 16, 32
                Max = 2
                CALL NumIn(draftP%(i, 23), Max, XCode, FG, BG)
                LOCATE 16, 32:PRINT USING "##";draftP%(I,23)
                IF XCode = 1 GOTO PBH:
                IF XCode = 2 GOTO PDone

            P3B:
                LOCATE 16, 36
                Max = 2
                CALL NumIn(draftP%(i, 24), Max, XCode, FG, BG)
                LOCATE 16, 36:PRINT USING "##";draftP%(I,24)
                IF XCode = 1 GOTO P2B:
                IF XCode = 2 GOTO PDone

            PHR:
                LOCATE 16, 40
                Max = 2
                CALL NumIn(draftP%(i, 25), Max, XCode, FG, BG)
                LOCATE 16, 40:PRINT USING "##";draftP%(I,25)
                IF XCode = 1 GOTO P3B:
                IF XCode = 2 GOTO PDone

            PRBI:
                LOCATE 16, 44
                Max = 3
                CALL NumIn(p%(i, 26), Max, XCode, FG, BG)
                LOCATE 16, 44:PRINT USING "###";draftP%(I,26)
                IF XCode = 1 GOTO PHR:
                IF XCode = 2 GOTO PDone

            PBBB:
                LOCATE 16, 49
                Max = 3
                CALL NumIn(draftP%(i, 27), Max, XCode, FG, BG)
                LOCATE 16, 49:PRINT USING "###";draftP%(I,27)
                IF XCode = 1 GOTO PRBI:
                IF XCode = 2 GOTO PDone

            PBKO:
                LOCATE 16, 54
                Max = 3
                CALL NumIn(draftP%(i, 28), Max, XCode, FG, BG)
                LOCATE 16, 54:PRINT USING "###";draftP%(I,28)
                IF XCode = 1 GOTO PBBB:
                IF XCode = 2 GOTO PDone

            PSB:
                LOCATE 16, 59
                Max = 3
                CALL NumIn(draftP%(i, 29), Max, XCode, FG, BG)
                LOCATE 16, 59:PRINT USING "###";draftP%(I,29)
                IF XCode = 1 GOTO PBKO:
                IF XCode = 2 GOTO PDone

            PCS:
                LOCATE 16, 64
                Max = 2
                CALL NumIn(draftP%(i, 30), Max, XCode, FG, BG)
                LOCATE 16, 64:PRINT USING "##";draftP%(I,30)
                IF XCode = 1 GOTO PSB:
                IF XCode = 2 GOTO PDone

            PSH:
                LOCATE 16, 68
                Max = 2
                CALL NumIn(draftP%(i, 36), Max, XCode, FG, BG)
                LOCATE 16, 68:PRINT USING "##";draftP%(I,36)
                IF XCode = 1 GOTO PCS:
                IF XCode = 2 GOTO PDone

            PPLAB:
                LOCATE 17, 17
                Max = 3
                CALL NumIn(draftP%(i, 37), Max, XCode, FG, BG)
                LOCATE 17, 17:PRINT USING "###";draftP%(I,37)
                IF XCode = 1 GOTO PSH:
                IF XCode = 2 GOTO PDone

            PPLH:
                LOCATE 17, 27
                Max = 3
                CALL NumIn(draftP%(i, 38), Max, XCode, FG, BG)
                LOCATE 17, 27:PRINT USING "###";draftP%(I,38)
                IF XCode = 1 GOTO PPLAB:
                IF XCode = 2 GOTO PDone

            PPL2B:
                LOCATE 17, 32
                Max = 2
                CALL NumIn(draftP%(i, 39), Max, XCode, FG, BG)
                LOCATE 17, 32:PRINT USING "##";draftP%(I,39)
                IF XCode = 1 GOTO PPLH:
                IF XCode = 2 GOTO PDone

            PPL3B:
                LOCATE 17, 36
                Max = 2
                CALL NumIn(draftP%(i, 40), Max, XCode, FG, BG)
                LOCATE 17, 36:PRINT USING "##";draftP%(I,40)
                IF XCode = 1 GOTO PPL2B:
                IF XCode = 2 GOTO PDone

            PPLHR:
                LOCATE 17, 40
                Max = 2
                CALL NumIn(draftP%(i, 41), Max, XCode, FG, BG)
                LOCATE 17, 40:PRINT USING "##";draftP%(I,41)
                IF XCode = 1 GOTO PPL3B:
                IF XCode = 2 GOTO PDone

            PPLBB:
                LOCATE 17, 49
                Max = 3
                CALL NumIn(draftP%(i, 42), Max, XCode, FG, BG)
                LOCATE 17, 49:PRINT USING "###";draftP%(I,42)
                IF XCode = 1 GOTO PPLHR:
                IF XCode = 2 GOTO PDone

            PPLKO:
                LOCATE 17, 54
                Max = 3
                CALL NumIn(draftP%(i, 43), Max, XCode, FG, BG)
                LOCATE 17, 54:PRINT USING "###";draftP%(I,43)
                IF XCode = 1 GOTO PPLBB:
                IF XCode = 2 GOTO PDone

            PPRAB:
                LOCATE 18, 17
                Max = 3
                CALL NumIn(draftP%(i, 44), Max, XCode, FG, BG)
                LOCATE 18, 17:PRINT USING "###";draftP%(I,44)
                IF XCode = 1 GOTO PPLKO:
                IF XCode = 2 GOTO PDone

            PPRH:
                LOCATE 18, 27
                Max = 3
                CALL NumIn(draftP%(i, 45), Max, XCode, FG, BG)
                LOCATE 18, 27:PRINT USING "###";draftP%(I,45)
                IF XCode = 1 GOTO PPRAB:
                IF XCode = 2 GOTO PDone

            PPR2B:
                LOCATE 18, 32
                Max = 2
                CALL NumIn(draftP%(i, 46), Max, XCode, FG, BG)
                LOCATE 18, 32:PRINT USING "##";draftP%(I,46)
                IF XCode = 1 GOTO PPRH:
                IF XCode = 2 GOTO PDone

            PPR3B:
                LOCATE 18, 36
                Max = 2
                CALL NumIn(draftP%(i, 47), Max, XCode, FG, BG)
                LOCATE 18, 36:PRINT USING "##";draftP%(I,47)
                IF XCode = 1 GOTO PPR2B:
                IF XCode = 2 GOTO PDone

            PPRHR:
                LOCATE 18, 40
                Max = 2
                CALL NumIn(draftP%(i, 48), Max, XCode, FG, BG)
                LOCATE 18, 40:PRINT USING "##";draftP%(I,48)
                IF XCode = 1 GOTO PPR3B:
                IF XCode = 2 GOTO PDone

            PPRBB:
                LOCATE 18, 49
                Max = 3
                CALL NumIn(draftP%(i, 49), Max, XCode, FG, BG)
                LOCATE 18, 49:PRINT USING "###";draftP%(I,49)
                IF XCode = 1 GOTO PPRHR:
                IF XCode = 2 GOTO PDone

            PPRKO:
                LOCATE 18, 54
                Max = 3
                CALL NumIn(draftP%(i, 50), Max, XCode, FG, BG)
                LOCATE 18, 54:PRINT USING "###";draftP%(I,50)
                IF XCode = 1 GOTO PPRBB:
                IF XCode = 2 GOTO PDone

            PLLBA:
                LOCATE 22, 27
                Max = 3
                CALL NumIn(draftP%(i, 65), Max, XCode, FG, BG)
                LOCATE 22, 27:PRINT USING "###";draftP%(I,65)
                IF XCode = 1 GOTO PPRKO:
                IF XCode = 2 GOTO PDone

            PLL2B:
                LOCATE 22, 32
                Max = 3
                CALL NumIn(draftP%(i, 66), Max, XCode, FG, BG)
                LOCATE 22, 32:PRINT USING "###";draftP%(I,66)
                IF XCode = 1 GOTO PLLBA:
                IF XCode = 2 GOTO PDone

            PLL3B:
                LOCATE 22, 37
                Max = 3
                CALL NumIn(draftP%(i, 67), Max, XCode, FG, BG)
                LOCATE 22, 37:PRINT USING "###";draftP%(I,67)
                IF XCode = 1 GOTO PLL2B:
                IF XCode = 2 GOTO PDone

            PLLHR:
                LOCATE 22, 42
                Max = 3
                CALL NumIn(draftP%(i, 68), Max, XCode, FG, BG)
                LOCATE 22, 42:PRINT USING "###";draftP%(I,68)
                IF XCode = 1 GOTO PLL3B:
                IF XCode = 2 GOTO PDone

            PLLBB:
                LOCATE 22, 47
                Max = 3
                CALL NumIn(draftP%(i, 69), Max, XCode, FG, BG)
                LOCATE 22, 47:PRINT USING "###";draftP%(I,69)
                IF XCode = 1 GOTO PLLHR:
                IF XCode = 2 GOTO PDone

            PLLKO:
                LOCATE 22, 52
                Max = 3
                CALL NumIn(draftP%(i, 70), Max, XCode, FG, BG)
                LOCATE 22, 52:PRINT USING "###";draftP%(I,70)
                IF XCode = 1 GOTO PLLBB:
                IF XCode = 2 GOTO PDone

            PLRBA:
                LOCATE 23, 27
                Max = 3
                CALL NumIn(draftP%(i, 71), Max, XCode, FG, BG)
                LOCATE 23, 27:PRINT USING "###";draftP%(I,71)
                IF XCode = 1 GOTO PLLKO:
                IF XCode = 2 GOTO PDone

            PLR2B:
                LOCATE 23, 32
                Max = 3
                CALL NumIn(draftP%(i, 72), Max, XCode, FG, BG)
                LOCATE 23, 32:PRINT USING "###";draftP%(I,72)
                IF XCode = 1 GOTO PLRBA:
                IF XCode = 2 GOTO PDone

            PLR3B:
                LOCATE 23, 37
                Max = 3
                CALL NumIn(draftP%(i, 73), Max, XCode, FG, BG)
                LOCATE 23, 37:PRINT USING "###";draftP%(I,73)
                IF XCode = 1 GOTO PLR2B:
                IF XCode = 2 GOTO PDone

            PLRHR:
                LOCATE 23, 42
                Max = 3
                CALL NumIn(draftP%(i, 74), Max, XCode, FG, BG)
                LOCATE 23, 42:PRINT USING "###";draftP%(I,74)
                IF XCode = 1 GOTO PLR3B:
                IF XCode = 2 GOTO PDone

            PLRBB:
                LOCATE 23, 47
                Max = 3
                CALL NumIn(draftP%(i, 75), Max, XCode, FG, BG)
                LOCATE 23, 47:PRINT USING "###";draftP%(I,75)
                IF XCode = 1 GOTO PLRHR:
                IF XCode = 2 GOTO PDone

            PLRKO:
                LOCATE 23, 52
                Max = 3
                CALL NumIn(draftP%(i, 76), Max, XCode, FG, BG)
                LOCATE 23, 52:PRINT USING "###";draftP%(I,76)
                IF XCode = 1 GOTO PLRBB:
                IF XCode = 2 GOTO PDone

            PRLBA:
                LOCATE 24, 27
                Max = 3
                CALL NumIn(draftP%(i, 77), Max, XCode, FG, BG)
                LOCATE 24, 27:PRINT USING "###";draftP%(I,77);
                IF XCode = 1 GOTO PLRKO:
                IF XCode = 2 GOTO PDone

            PRL2B:
                LOCATE 24, 32
                Max = 3
                CALL NumIn(draftP%(i, 78), Max, XCode, FG, BG)
                LOCATE 24, 32:PRINT USING "###";draftP%(I,78);
                IF XCode = 1 GOTO PRLBA:
                IF XCode = 2 GOTO PDone

            PRL3B:
                LOCATE 24, 37
                Max = 3
                CALL NumIn(draftP%(i, 79), Max, XCode, FG, BG)
                LOCATE 24, 37:PRINT USING "###";draftP%(I,79);
                IF XCode = 1 GOTO PRL2B:
                IF XCode = 2 GOTO PDone

            PRLHR:
                LOCATE 24, 42
                Max = 3
                CALL NumIn(draftP%(i, 80), Max, XCode, FG, BG)
                LOCATE 24, 42:PRINT USING "###";draftP%(I,80);
                IF XCode = 1 GOTO PRL3B:
                IF XCode = 2 GOTO PDone

            PRLBB:
                LOCATE 24, 47
                Max = 3
                CALL NumIn(draftP%(i, 81), Max, XCode, FG, BG)
                LOCATE 24, 47:PRINT USING "###";draftP%(I,81);
                IF XCode = 1 GOTO PRLHR:
                IF XCode = 2 GOTO PDone

            PRLKO:
                LOCATE 24, 52
                Max = 3
                CALL NumIn(draftP%(i, 82), Max, XCode, FG, BG)
                LOCATE 24, 52:PRINT USING "###";draftP%(I,82);
                IF XCode = 1 GOTO PRLBB:
                IF XCode = 2 GOTO PDone

            PRRBA:
                LOCATE 25, 27
                Max = 3
                CALL NumIn(draftP%(i, 83), Max, XCode, FG, BG)
                LOCATE 25, 27:PRINT USING "###";draftP%(I,83);
                IF XCode = 1 GOTO PRLKO:
                IF XCode = 2 GOTO PDone

            PRR2B:
                LOCATE 25, 32
                Max = 3
                CALL NumIn(draftP%(i, 84), Max, XCode, FG, BG)
                LOCATE 25, 32:PRINT USING "###";draftP%(I,84);
                IF XCode = 1 GOTO PRRBA:
                IF XCode = 2 GOTO PDone

            PRR3B:
                LOCATE 25, 37
                Max = 3
                CALL NumIn(draftP%(i, 85), Max, XCode, FG, BG)
                LOCATE 25, 37:PRINT USING "###";draftP%(I,85);
                IF XCode = 1 GOTO PRR2B:
                IF XCode = 2 GOTO PDone

            PRRHR:
                LOCATE 25, 42
                Max = 3
                CALL NumIn(draftP%(i, 86), Max, XCode, FG, BG)
                LOCATE 25, 42:PRINT USING "###";draftP%(I,86);
                IF XCode = 1 GOTO PRR3B:
                IF XCode = 2 GOTO PDone

            PRRBB:
                LOCATE 25, 47
                Max = 3
                CALL NumIn(draftP%(i, 87), Max, XCode, FG, BG)
                LOCATE 25, 47:PRINT USING "###";draftP%(I,87);
                IF XCode = 1 GOTO PRRHR:
                IF XCode = 2 GOTO PDone

            PRRKO:
                LOCATE 25, 52
                Max = 3
                CALL NumIn(draftP%(i, 88), Max, XCode, FG, BG)
                LOCATE 25, 52:PRINT USING "###";draftP%(I,88);
                IF XCode = 1 GOTO PRRBB:

            PDone:
                LOCATE 25, 1
                PRINT "Is this okay? (Yes/No) ";
                X$ = ""
                CALL YesNoB(X$, XCode, FG, BG)
                LOCATE , 1
                'ClearEOL 14, 1
                IF XCode = 1 GOTO PRRKO
                IF X$ = "N" GOTO PName
                IF X$ <> "Y" GOTO PDone               'insist on either Y or N or up-arrow
                draftP%(i, 0) = 1
                THROW$ = _TRIM$(THROW$)

                IF UCASE$(THROW$) = "L" THEN draftP%(i, 0) = -1
                ACT$ = _TRIM$(ACT$)
                draftP%(i, 34) = 0
                IF UCASE$(ACT$) = "N" THEN draftP%(i, 34) = 1

        End If

    Loop Until i = 0

END SUB



'----------------------------------------
'       PITCHINIT subroutine
'----------------------------------------
'...explanation...
SUB PITCHINIT (i, FG, BG)

    Shared draftP%()

    COLOR FG,BG

    LOCATE 1, 60: PRINT USING "###";draftP%(i, 35)
    LOCATE 1, 78: PRINT USING "#";draftP%(i, 18)
    LOCATE 5, 10: PRINT USING "##";draftP%(i, 1)
    LOCATE 5, 14: PRINT USING "##";draftP%(i, 2)
    LOCATE 5, 18: PRINT USING "##";draftP%(i, 3)
    LOCATE 5, 22: PRINT USING "###";draftP%(i, 4)
    LOCATE 5, 27: PRINT USING "##";draftP%(i, 5)
    LOCATE 5, 31: PRINT USING "##";draftP%(i, 16)
    LOCATE 5, 35: PRINT USING "###";draftP%(i, 6)
    LOCATE 5, 40: PRINT USING "###";draftP%(i, 7)
    LOCATE 5, 45: PRINT USING "###";draftP%(i, 8)
    LOCATE 5, 50: PRINT USING "###";draftP%(i, 9)
    LOCATE 5, 55: PRINT USING "##";draftP%(i, 33)
    LOCATE 5, 59: PRINT USING "####";draftP%(i, 10)
    LOCATE 5, 66: PRINT USING "##";draftP%(i, 13)
    LOCATE 6, 32: PRINT USING "##";draftP%(i, 12)
    LOCATE 6, 50: PRINT USING "###";draftP%(i, 14)
    LOCATE 10, 25: PRINT USING "###";draftP%(i, 51)
    LOCATE 10, 30: PRINT USING "###";draftP%(i, 52)
    LOCATE 10, 35: PRINT USING "##";draftP%(i, 53)
    LOCATE 10, 39: PRINT USING "##";draftP%(i, 54)
    LOCATE 10, 43: PRINT USING "##";draftP%(i, 55)
    LOCATE 10, 47: PRINT USING "###";draftP%(i, 56)
    LOCATE 10, 52: PRINT USING "###";draftP%(i, 57)
    LOCATE 11, 25: PRINT USING "###";draftP%(i, 58)
    LOCATE 11, 30: PRINT USING "###";draftP%(i, 59)
    LOCATE 11, 35: PRINT USING "##";draftP%(i, 60)
    LOCATE 11, 39: PRINT USING "##";draftP%(i, 61)
    LOCATE 11, 43: PRINT USING "##";draftP%(i, 62)
    LOCATE 11, 47: PRINT USING "###";draftP%(i, 63)
    LOCATE 11, 52: PRINT USING "###";draftP%(i, 64)
    LOCATE 14, 20: PRINT USING "#";draftP%(i, 31)
    LOCATE 14, 36: PRINT USING "#";draftP%(i, 32)
    LOCATE 14, 52: PRINT USING "###";draftP%(i, 11)
    LOCATE 14, 68: PRINT USING "####";draftP%(i, 17)
    LOCATE 16, 12: PRINT USING "###";draftP%(i, 19)
    LOCATE 16, 17: PRINT USING "###";draftP%(i, 20)
    LOCATE 16, 22: PRINT USING "###";draftP%(i, 21)
    LOCATE 16, 27: PRINT USING "###";draftP%(i, 22)
    LOCATE 16, 32: PRINT USING "##";draftP%(i, 23)
    LOCATE 16, 36: PRINT USING "##";draftP%(i, 24)
    LOCATE 16, 40: PRINT USING "##";draftP%(i, 25)
    LOCATE 16, 44: PRINT USING "###";draftP%(i, 26)
    LOCATE 16, 49: PRINT USING "###";draftP%(i, 27)
    LOCATE 16, 54: PRINT USING "###";draftP%(i, 28)
    LOCATE 16, 59: PRINT USING "###";draftP%(i, 29)
    LOCATE 16, 64: PRINT USING "##";draftP%(i, 30)
    LOCATE 16, 68: PRINT USING "##";draftP%(i, 36)
    LOCATE 17, 17: PRINT USING "###";draftP%(i, 37)
    LOCATE 17, 27: PRINT USING "###";draftP%(i, 38)
    LOCATE 17, 32: PRINT USING "##";draftP%(i, 39)
    LOCATE 17, 36: PRINT USING "##";draftP%(i, 40)
    LOCATE 17, 40: PRINT USING "##";draftP%(i, 41)
    LOCATE 17, 49: PRINT USING "###";draftP%(i, 42)
    LOCATE 17, 54: PRINT USING "###";draftP%(i, 43)
    LOCATE 18, 17: PRINT USING "###";draftP%(i, 44)
    LOCATE 18, 27: PRINT USING "###";draftP%(i, 45)
    LOCATE 18, 32: PRINT USING "##";draftP%(i, 46)
    LOCATE 18, 36: PRINT USING "##";draftP%(i, 47)
    LOCATE 18, 40: PRINT USING "##";draftP%(i, 48)
    LOCATE 18, 49: PRINT USING "###";draftP%(i, 49)
    LOCATE 18, 54: PRINT USING "###";draftP%(i, 50)
    LOCATE 22, 27: PRINT USING "###";draftP%(i, 65)
    LOCATE 22, 32: PRINT USING "###";draftP%(i, 66)
    LOCATE 22, 37: PRINT USING "###";draftP%(i, 67)
    LOCATE 22, 42: PRINT USING "###";draftP%(i, 68)
    LOCATE 22, 47: PRINT USING "###";draftP%(i, 69)
    LOCATE 22, 52: PRINT USING "###";draftP%(i, 70)
    LOCATE 23, 27: PRINT USING "###";draftP%(i, 71)
    LOCATE 23, 32: PRINT USING "###";draftP%(i, 72)
    LOCATE 23, 37: PRINT USING "###";draftP%(i, 73)
    LOCATE 23, 42: PRINT USING "###";draftP%(i, 74)
    LOCATE 23, 47: PRINT USING "###";draftP%(i, 75)
    LOCATE 23, 52: PRINT USING "###";draftP%(i, 76)
    LOCATE 24, 27: PRINT USING "###";draftP%(i, 77);
    LOCATE 24, 32: PRINT USING "###";draftP%(i, 78);
    LOCATE 24, 37: PRINT USING "###";draftP%(i, 79);
    LOCATE 24, 42: PRINT USING "###";draftP%(i, 80);
    LOCATE 24, 47: PRINT USING "###";draftP%(i, 81);
    LOCATE 24, 52: PRINT USING "###";draftP%(i, 82);
    LOCATE 25, 27: PRINT USING "###";draftP%(i, 83);
    LOCATE 25, 32: PRINT USING "###";draftP%(i, 84);
    LOCATE 25, 37: PRINT USING "###";draftP%(i, 85);
    LOCATE 25, 42: PRINT USING "###";draftP%(i, 86);
    LOCATE 25, 47: PRINT USING "###";draftP%(i, 87);
    LOCATE 25, 52: PRINT USING "###";draftP%(i, 88);

END SUB


'----------------------------------------
'       VIEWHITTERS subroutine
'----------------------------------------
'...explanation...
Sub ViewHitters

    Shared B$()
    Shared draftB%(), TB%()

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
            If B$(I) <> "XXX" Then
                Print Left$(B$(I), 10);
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
Sub ViewPitchers

    Shared P$()
    Shared draftP%(), TP%()

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

        If P$(I) <> "XXX" Then
            
            app = app + draftP%(I, 4)
            Print Left$(P$(I), 10);

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
    Locate , 14: Print Using "####  ####      ####    ####     #.##    ###   ###"; TP%(1); TP%(3); TP%(6); TP%(8); (ERX! * 9) / TP%(6); TP%(13); TP%(33)
    Locate , 17: Print Using "####      ###    ####    ####            ###"; TP%(2); TP%(5); TP%(7); TP%(9); TP%(16)
    Print "TOTAL APPEARANCES: "; app

    Color 11
    Locate 24: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'     ViewHittingFielding subroutine
'----------------------------------------
'...explanation...
Sub ViewHittingFielding

    Shared draftP%()

    Cls
    Color 14: Print "PITCHER        G  AB   R   H DB TR HR RBI  BB  SO  SB CS BAVG  R B FAVG"

    For I = 0 To 21

        Color 3: Print Left$(P$(I), 10);
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
            statPCT! = draftP%(I, 11) / 1000: Print Tab(59); Using ".###"; statPCT!;
        Else
            statI2! = 0
            If draftP%(I, 20) > 0 Then statI2! = Int((draftP%(I, 22) / draftP%(I, 20) + .0005) * 1000)
            If draftP%(I, 20) = 0 Or statI2! = 0 Then Print Tab(59); ".000";
            If statI2! > 0 And statI2! < 100 Then Print Tab(59); ".0";: Print Tab(61); Using "##"; statI2!;
            If statI2! > 100 Then Print Tab(59); ".";: Print Tab(60); Using "###"; statI2!;
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

    Shared B$()
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

        Color 3: Print Left$(B$(I), 10);
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
