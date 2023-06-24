Sub TRADE ()

    _Title "Full Count Baseball - Trade Players"

    Dim yearNumbers$(0 To 1)

    ReDim menuChoices$(1 To 2)

    Do

        Call TITLE

        menuChoices$(1) = "TRADE PLAYERS                 "
        menuChoices$(2) = "RETURN TO FULL COUNT MAIN MENU"
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 24

        Call box0(7, 23, 10, 56, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    tradeIdx = 0
                    Call GetTeam_TRADE(yearNumbers$(tradeIdx), teamIdx)
                    Call LoadStatsForTrade(yearNumbers$(tradeIdx), tradeIdx)
                    Call StartTrade(yearNumbers$(), tradeIdx, NF)
                    tradeIdx = 1
                    Call GetTeam_TRADE(yearNumbers$(tradeIdx), tradeIdx)
                    Call ContinueTrade(yearNumbers$(), tradeIdx, NF)

                Case 2:
                    Cls

            End Select

        End If

    Loop Until Selection% = 2 Or ExitCode% = 27


End Sub

'----------------------------------------
'       GetTeam_TRADE Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_TRADE (yearNumber$, tradeIdx)

    Shared tradeA$()
    Shared tradeL%()

    Do

        Color 15, 0
        Cls

        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12
        If tradeIdx = 0 Then
            Print "ENTER DISK ID FOR TEAM 1: "
        Else
            Print "ENTER DISK ID FOR TEAM 2: "
        End If
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 36: Input NY$

        If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_TRADE(yearNumber$, numberTeams, tradeIdx)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
                BO% = 1
                Locate 10, 8
                Call CSMsgBox("NO TEAMS MATCHING THIS ID WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
            Else

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 To fileLength& / 4342
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

                Call SelectTeam_TRADE(yearNumber$, numberTeams, tradeIdx)

            End If 'Done checking for file

        End If 'Done checking for emtpy value

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color tradeL%(tradeIdx, 11), tradeL%(tradeIdx, 12): Print tradeA$(tradeIdx)

            Color 15, 4

            Do
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'     SelectTeam_TRADE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_TRADE (yearNumber$, numberTeams, tradeIdx)

    Shared tradeA$(), tradeB$(), tradeP$()
    Shared tradeL%(), tradeB%(), tradeP%()
    Shared teamIndexes()
    Shared Managers$(), Stadiums$(), teamAbbrevs$()

    Shared K9&()

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

    Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIndexes(tradeIdx) = teamIndex%(Selection%)
        teamYear$ = RTrim$(yearNumber$)

        Locate 2, 33: Color 31
        Print "LOADING..."

        Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

        For I = 0 To 22
            Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
            Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
            Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
            Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
        Next

        For I = 0 To 21
            Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015), 2020 - 37 - I * 89 As X$
            Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027), 2020 - 61 - I * 89 As X$
            Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039), 2020 - 85 - I * 89 As X$
            Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
        Next

        Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

        Get #1, teamIndexes(tradeIdx)

        tradeA$(tradeIdx) = RTrim$(Q$(0))

        For I1 = 1 To 13: tradeL%(tradeIdx, I1) = CVI(Q$(I1)): Next

        K9&(tradeIdx) = CVS(Q$(14))

        For I1 = 0 To 22:
            tradeB$(tradeIdx, I1) = RTrim$(Q$(I1 * 43 + 15))
            For I2 = 0 To 41
                tradeB%(tradeIdx, I1, I2) = CVI(Q$(I1 * 43 + I2 + 16))
            Next
        Next

        For I1 = 0 To 21:
            tradeB$(tradeIdx, I1) = RTrim$(Q$(I1 * 38 + 1004))
            For I2 = 0 To 36
                tradeP%(tradeIdx, I1, I2) = CVI(Q$(I1 * 38 + I2 + 1005))
            Next
        Next

        Stadiums$(tradeIdx) = RTrim$(Q$(1840))
        Managers$(tradeIdx) = RTrim$(Q$(1841))
        teamAbbrevs$(tradeIdx) = RTrim$(Q$(1842))
        CK = CVI(Q$(1843))

        Close 1

    Else

        BO% = 1

    End If

End Sub

'----------------------------------------
'     LoadStatsForTrade Subroutine
'----------------------------------------
'...explanation...
Sub LoadStatsForTrade (teamYear$, tradeIdx)

    Shared tradeB%(), tradeP%()
    Shared tradeSA%(), tradeTS%()

    If _FileExists(diskPaths$(0) + "LR." + teamYear$) Then

        teamYear$ = RTrim$(teamYear$)

        Open diskPaths$(0) + "LR." + teamYear$ For Random As #1 Len = 4108

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        Get #1, teamIndexes(tradeIdx)

        For X = 0 To 11
            tradeTS%(tradeIdx, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                tradeB%(tradeIdx, X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                tradeP%(tradeIdx, X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                tradeP%(tradeIdx, X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
            Next
        Next

        For X = 1 To 24
            tradeSA%(tradeIdx, X) = CVI(Q$(X + 950))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                tradeB%(tradeIdx, X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                tradeP%(tradeIdx, X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
            Next
        Next

        Close 1

        If tradeTS%(tradeIdx, 0) <> 999 Then

            For X = 0 To 22
                For X1 = 42 To 79
                    tradeB%(tradeIdx, X, X1) = 0
                Next
            Next

            For X = 0 To 21
                For X1 = 37 To 88
                    tradeP%(tradeIdx, X, X1) = 0
                Next
            Next

            For X = 1 To 24
                tradeSA%(tradeIdx, X) = 0
            Next

            For X = 0 To 11
                tradeTS%(tradeIdx, X) = 0
            Next

        End If

    End If

    Color 15, 0

End Sub


'----------------------------------------
'          StartTrade Subroutine
'----------------------------------------
'...explanation...
Sub StartTrade (yearNumbers$(), sourceIdx, noStats)

    Shared tradeA$(), A1$(), tradeSA$()
    Shared tradeB1$(), tradeP1$()

    Shared tradeD0%(), tradeD1%(), tradeL%(), L2%()
    Shared TEAM%(), tradeT1%(), tradeP0%(), tradeP3%()
    Shared tradeB0%(), tradeB3%(), tradeSS%(), W2%()

    Shared tradeS()

    tradeS(0) = 0

    Cls
    Print "DOES ";
    Color tradeL%(sourceIdx, 11), tradeL%(sourceIdx, 12): Print tradeA$(0);
    Color 15, 0
    Print " HAVE SAVED STATS (YN)"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

    If UCase$(I$) = "N" Then
        noStats = 1
        tradeS(0) = 1
        yearNumbers$(1) = yearNumbers$(0)
    Else
        Cls

        tradeSA$(sourceIdx) = tradeA$(sourceIdx)

        For XX = 4 To 11
            If Mid$(tradeSA$(sourceIdx), XX, 1) = " " Or Mid$(tradeSA$(sourceIdx), XX, 1) = "." Then Mid$(tradeSA$(sourceIdx), XX, 1) = "_"
        Next

        QQ = 8

        For XX = 11 To 4 Step -1
            If Mid$(tradeSA$(sourceIdx), XX, 1) = "_" Then
                QQ = XX - 4
            Else
                Exit For
            End If
        Next XX

        F$ = Mid$(tradeSA$(sourceIdx), 4, QQ)
        R$ = F$ + "." + Left$(tradeSA$(sourceIdx), 2)

        If _FileExists(diskPaths$(1) + R$) Then

            Open diskPaths$(1) + R$ For Input As #1
            Input #1, A1$(sourceIdx)
            Input #1, W2%(sourceIdx)
            Input #1, L2%(sourceIdx)

            For I1 = 0 To 22:
                Input #1, tradeB1$(sourceIdx, I1)
                Input #1, tradeB3%(sourceIdx, I1)
            Next I1

            For I1 = 0 To 21:
                Input #1, tradeP1$(sourceIdx, I1)
                Input #1, tradeP3%(sourceIdx, I1)
            Next I1

            For I1 = 0 To 22:
                For I2 = 0 To 21:
                    Input #1, tradeB0%(sourceIdx, I1, I2)
                Next I2
            Next I1

            For I1 = 0 To 21:
                For I2 = 0 To 41:
                    Input #1, tradeP0%(sourceIdx, I1, I2)
                Next I2
            Next I1

            Input #1, tradeD0%(sourceIdx)
            Input #1, tradeD1%(sourceIdx)

            For I = 0 To 21: Input #1, TEAM%(sourceIdx, I): Next I
            For I = 0 To 22: Input #1, tradeT1%(sourceIdx, I): Next I

            Close #1

            If noStats = 1 Then
                noStats = 0
                tradeS(0) = 1
            End If

            yearNumbers$(1) = yearNumbers$(0)

            If _FileExists(diskPaths$(1) + R$ + "R") Then

                Open diskPaths$(1) + R$ + "R" For Input As #1

                For I1 = 0 To 22
                    For I2 = 0 To 6
                        Input #1, tradeSS%(sourceIdx, I1, I2)
                    Next I2
                Next I1

                Close #1

            End If

        Else
            noStats = 1
            Locate 10, 8
            Call CSMsgBox("NO COMPILED STATS FOR TEAM WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        End If

    End If

End Sub


'----------------------------------------
'       ContinueTrade Subroutine
'----------------------------------------
'...explanation...
Sub ContinueTrade (yearNumbers$(), destIdx, noStats)

    Shared tradeA$(), A1$(), tradeSA$()
    Shared tradeB1$(), tradeP1$()

    Shared tradeD0%(), tradeD1%(), tradeL%(), L2%()
    Shared TEAM%(), tradeT1%(), tradeP0%(), tradeP3%()
    Shared tradeB0%(), tradeB3%(), tradeSS%(), W2%()

    Shared tradeS()

    Color 15, 0

    tradeS(1) = 0

    Do
        Cls

        Print "DOES ";
        Color tradeL%(destIdx, 11), tradeL%(destIdx, 12): Print tradeA$(1);
        Color 15, 0
        Print " HAVE SAVED STATS (YN)"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then
        tradeS(1) = 1
    Else

        Cls

        tradeSA$(destIdx) = tradeA$(destIdx)

        For XX = 4 To 11
            If Mid$(tradeSA$(destIdx), XX, 1) = " " Or Mid$(tradeSA$(destIdx), XX, 1) = "." Then Mid$(tradeSA$(destIdx), XX, 1) = "_"
        Next

        QQ = 8

        For XX = 11 To 4 Step -1
            If Mid$(tradeSA$(destIdx), XX, 1) = "_" Then
                QQ = XX - 4:
            Else
                Exit For
            End If
        Next XX

        If noStats = 1 Then
            noStats = 0
            tradeS(1) = 1
        End If

    End If

    Do
        Cls
        Color 12: Print Tab(35); "TRADE MENU"
        Print
        Color 15: Print "(1) ";
        Color 3: Print "TRADE BATTERS"
        Print
        Color 15: Print "(2) ";
        Color 3: Print "TRADE PITCHERS"
        Print
        Color 15: Print "(3) ";
        Color 3: Print "END AND SAVE TRADES"
        Print
        Color 15: Print "(4) ";
        Color 3: Print "END WITHOUT SAVING TRADES"

        Do
            I$ = GetKeyPress$
        Loop Until Val(I$) >= 0 And Val(I$) <= 4

        Select Case I$
            Case "1":
                If tradeS(0) <> 1 And tradeS(1) <> 1 Then
                    Cls
                    Print
                    Print
                    Color 14: Print "(T) ";: Color 15: Print "TRADE PLAYERS' COMPILED STATS"
                    Print
                    Color 14: Print "(Z) ";: Color 15: Print "ZERO PLAYERS' COMPILED STATS"
                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "T" Or UCase$(I$) = "Z"

                    If UCase$(I$) = "T" Then
                        W1 = 1
                    Else
                        W1 = 0
                    End If
                End If

                Call TradeBatters

            Case "2":
                If tradeS(0) <> 1 And tradeS(1) <> 1 Then
                    Cls
                    Print
                    Print
                    Color 14: Print "(T) ";
                    Color 15: Print "TRADE PLAYERS' COMPILED STATS"
                    Print
                    Color 14: Print "(Z) ";
                    Color 15: Print "ZERO PLAYERS' COMPILED STATS"
                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "T" Or UCase$(I$) = "Z"

                    If UCase$(I$) = "T" Then
                        W1 = 1
                    Else
                        W1 = 0
                    End If
                End If

                Call TradePitchers

            Case "3":
                Call SaveTrades(yearNumbers$())

            Case "4":
                Cls '

        End Select

    Loop Until I$ = "3" Or I$ = "4"

End Sub

'----------------------------------------
'       TradeBatters Subroutine
'----------------------------------------
'...explanation...
Sub TradeBatters ()

    Shared tradeA$(), tradeB1$(), tradeB$()

    Shared tradeB%(), tradeB0%(), tradeB3%(), tradeL%()
    Shared tradeSA%(), tradeSS%(), tradeTS%()

    Shared tradeS()

    Cls

    For I = 0 To 16:
        Locate I + 2, 4
        Color , tradeL%(0, 12): Print Space$(15)
    Next

    For I = 0 To 5:
        Locate I + 2, 24
        Color , tradeL%(0, 12): Print Space$(15)
    Next

    For I = 0 To 16:
        Locate I + 2, 44
        Color , tradeL%(1, 12): Print Space$(15)
    Next

    For I = 0 To 5:
        Locate I + 2, 64
        Color , tradeL%(1, 12): Print Space$(15)
    Next

    Color 15, 0
    Locate 1: Print tradeA$(0);
    Locate , 40: Print tradeA$(1)

    For I = 0 To 16
        Color 15, 0
        Print Using "##"; I;: Print " ";
        Color tradeL%(0, 11), tradeL%(0, 12): Print tradeB$(0, I);
        Color 15, 0
        Locate , 40
        Print Using "##"; I;: Print " ";
        Color tradeL%(1, 11), tradeL%(1, 12): Print tradeB$(1, I)
    Next I

    For I = 17 To 22
        Color 15, 0
        Locate I - 15, 20: Print Using "##"; I;: Print " ";
        Color tradeL%(0, 11), tradeL%(0, 12): Print tradeB$(0, I);
        Color 15, 0
        Locate , 60
        Print Using "##"; I;: Print " ";
        Color tradeL%(1, 11), tradeL%(1, 12): Print tradeB$(1, I)
    Next I

    Do
        Color 15, 0: Locate 20, 1

        Print Space$(40);
        Print Space$(40);
        Print Space$(40);

        Locate 20, 1: Print tradeA$(0); " BATTER: ";

        Do
            Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 22

        Print tradeB$(0, I1)
        Locate 21, 1: Print tradeA$(1); " BATTER: ";

        Do
            Input I$
            I2 = Val(I$)
        Loop Until I2 >= 0 And I2 <= 22

        Print tradeB$(1, I2)

        Do
            Locate 22, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TradeSummary_Batters(I1, I2)

    Locate 24, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    If Not (tradeTS%(0, 0) = 999 And tradeTS%(1, 0) = 999) And Not (tradeTS%(0, 0) <> 999 And tradeTS%(1, 0) <> 999) Then

        If tradeTS%(0, 0) = 999 And tradeTS%(1, 0) <> 999 Then

            For I3 = 0 To 11:
                tradeTS%(1, I3) = tradeTS%(0, I3)
            Next

            For I3 = 1 To 24:
                tradeSA%(1, I3) = tradeSA%(0, I3)
            Next

        Else

            If tradeTS%(1, 0) = 999 And tradeTS%(0, 0) <> 999 Then

                For I3 = 0 To 11:
                    tradeTS%(0, I3) = tradeTS%(1, I3)
                Next

                For I3 = 1 To 24:
                    tradeSA%(0, I3) = tradeSA%(1, I3)
                Next

            End If

        End If

    End If

    Swap tradeB$(0, I1), tradeB$(1, I2)

    For I3 = 0 To 79
        Swap tradeB%(0, I1, I3), tradeB%(1, I2, I3)
    Next I3

    For I3 = 0 To 6
        Swap tradeSS%(0, I1, I3), tradeSS%(1, I2, I3)
    Next I3

    tradeB1$(0, I1) = tradeB$(0, I1)
    tradeB1$(1, I2) = tradeB$(1, I2)
    Swap tradeB3%(0, I1), tradeB3%(1, I2)

    If tradeS(0) = 1 Or tradeS(1) = 1 Or W1 = 0 Then

        For I3 = 0 To 21:
            Swap tradeB0%(0, I1, I3), tradeB0%(1, I2, I3)
        Next I3

    Else

        For I3 = 0 To 21:
            tradeB0%(0, I1, I3) = 0
            tradeB0%(1, I2, I3) = 0
        Next I3

    End If

    Call TradeSummary_Batters(I1, I2)

    Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       TradePitchers Subroutine
'----------------------------------------
'...explanation...
Sub TradePitchers ()

    Shared tradeA$(), tradeB$(), tradeP1$()
    Shared tradeL%(), tradeP0%(), tradeP%(), tradeP3%()
    Shared tradeSA%(), tradeTS%()
    Shared tradeS()

    Cls

    For I = 0 To 11:
        Locate I + 2, 4:
        Color , tradeL%(0, 12): Print Space$(15)
    Next

    For I = 0 To 10:
        Locate I + 2, 24:
        Color , tradeL%(0, 12): Print Space$(15)
    Next

    For I = 0 To 11:
        Locate I + 2, 44:
        Color , tradeL%(1, 12): Print Space$(15)
    Next

    For I = 0 To 10:
        Locate I + 2, 64:
        Color , tradeL%(1, 12): Print Space$(15)
    Next

    Color 15, 0: Locate 1: Print tradeA$(0);: Locate , 40: Print tradeA$(1)

    For I = 0 To 11
        Color 15, 0
        Print Using "##"; I;: Print " ";
        Color tradeL%(0, 11), tradeL%(0, 12): Print tradeB$(0, I);
        Locate , 40
        Color 15, 0
        Print Using "##"; I;: Print " ";
        Color tradeL%(1, 11), tradeL%(1, 12): Print tradeB$(1, I)
    Next I

    For I = 12 To 21
        Color 15, 0
        Locate I - 10, 20: Print Using "##"; I;: Print "  ";
        Color tradeL%(0, 11), tradeL%(0, 12): Print tradeB$(0, I);
        Locate I - 10, 40
        Locate , 60
        Color 15, 0
        Print Using "##"; I;: Print "  ";
        Color tradeL%(1, 11), tradeL%(1, 12): Print tradeB$(1, I)
    Next I

    Do
        Color 15, 0
        Locate 20, 1
        Print Space$(40);
        Print Space$(40);
        Print Space$(40);

        Locate 20, 1: Print tradeA$(0); " PITCHER: ";

        Do
            Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 21

        Print tradeB$(0, I1)
        Locate 21, 1: Print tradeA$(1); " PITCHER: ";

        Do
            Input I$
            I2 = Val(I$)
        Loop Until I2 >= 0 And I2 <= 21

        Print tradeB$(1, I2)

        Do
            Locate 22, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

    Loop Until UCase$(I$) = "N"

    Call TradeSummary_Pitchers(I1, I2)

    Locate 24, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    If tradeTS%(0, 0) = 999 And tradeTS%(1, 0) = 999 Then 505

    If tradeTS%(0, 0) <> 999 And tradeTS%(1, 0) <> 999 Then 505

    If tradeTS%(0, 0) = 999 And tradeTS%(1, 0) <> 999 Then

        For I3 = 0 To 11:
            tradeTS%(1, I3) = tradeTS%(0, I3)
        Next

        For I3 = 1 To 24:
            tradeSA%(1, I3) = tradeSA%(0, I3)
        Next

        GoTo 505

    End If

    If tradeTS%(1, 0) = 999 And tradeTS%(0, 0) <> 999 Then

        For I3 = 0 To 11:
            tradeTS%(0, I3) = tradeTS%(1, I3)
        Next

        For I3 = 1 To 24:
            tradeSA%(0, I3) = tradeSA%(1, I3)
        Next

    End If

    505 '
    Swap tradeB$(0, I1), tradeB$(1, I2)

    For I3 = 0 To 88:
        Swap tradeP%(0, I1, I3), tradeP%(1, I2, I3)
    Next I3

    tradeP1$(0, I1) = tradeB$(0, I1)
    tradeP1$(1, I2) = tradeB$(1, I2)

    If tradeS(0) = 1 Or tradeS(1) = 1 Or W1 = 0 Then
        For I3 = 0 To 41:
            tradeP0%(0, I1, I3) = 0
            tradeP0%(1, I2, I3) = 0
        Next I3
    Else
        Swap tradeP3%(0, I1), tradeP3%(1, I2)
        For I3 = 0 To 41:
            Swap tradeP0%(0, I1, I3), tradeP0%(1, I2, I3)
        Next I3
    End If

    Call TradeSummary_Pitchers(I1, I2)

    Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       SaveTrades Subroutine
'----------------------------------------
'...explanation...
Sub SaveTrades (yearNumbers$())

    Shared tradeA$(), A1$(), tradeSA$()
    Shared tradeB$(), tradeP$()
    Shared tradeB1$(), tradeP1$()

    Shared tradeD0%(), tradeD1%(), tradeL%(), L2%()
    Shared tradeS(), TEAM%()
    Shared tradeB%(), tradeB0%(), tradeB3%()
    Shared tradeP%(), tradeP0%(), tradeP3%()
    Shared tradeSA%(), tradeSS%(), tradeT1%(), tradeTS%()
    Shared teamIndexes(), W2%()

    Shared Managers$(), Stadiums$(), teamAbbrevs$()

    Shared K9&()

    For tradeIdx = 0 To 1

        Cls

        Color 15, 0

        teamYear$ = RTrim$(yearNumbers$(tradeIdx))

        Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

        For J = 0 To 22
            Field #1, 45 + J * 99 As X$, 15 As Q$(J * 43 + 15), 2 As Q$(J * 43 + 16), 2 As Q$(J * 43 + 17), 2 As Q$(J * 43 + 18), 2 As Q$(J * 43 + 19), 2 As Q$(J * 43 + 20), 2 As Q$(J * 43 + 21), 2 As Q$(J * 43 + 22), 2 As Q$(J * 43 + 23), 2 As Q$(J * 43 + 24), 2 As Q$(J * 43 + 25), 2 As Q$(J * 43 + 26), 4297 - 37 - J * 99 As X$
            Field #1, 82 + J * 99 As X$, 2 As Q$(J * 43 + 27), 2 As Q$(J * 43 + 28), 2 As Q$(J * 43 + 29), 2 As Q$(J * 43 + 30), 2 As Q$(J * 43 + 31), 2 As Q$(J * 43 + 32), 2 As Q$(J * 43 + 33), 2 As Q$(J * 43 + 34), 2 As Q$(J * 43 + 35), 2 As Q$(J * 43 + 36), 2 As Q$(J * 43 + 37), 2 As Q$(J * 43 + 38), 4297 - 61 - J * 99 As X$
            Field #1, 106 + J * 99 As X$, 2 As Q$(J * 43 + 39), 2 As Q$(J * 43 + 40), 2 As Q$(J * 43 + 41), 2 As Q$(J * 43 + 42), 2 As Q$(J * 43 + 43), 2 As Q$(J * 43 + 44), 2 As Q$(J * 43 + 45), 2 As Q$(J * 43 + 46), 2 As Q$(J * 43 + 47), 2 As Q$(J * 43 + 48), 2 As Q$(J * 43 + 49), 2 As Q$(J * 43 + 50), 4297 - 85 - J * 99 As X$
            Field #1, 130 + J * 99 As X$, 2 As Q$(J * 43 + 51), 2 As Q$(J * 43 + 52), 2 As Q$(J * 43 + 53), 2 As Q$(J * 43 + 54), 2 As Q$(J * 43 + 55), 2 As Q$(J * 43 + 56), 2 As Q$(J * 43 + 57), 4297 - 99 - J * 99 As X$
        Next

        For J = 0 To 21
            Field #1, 2322 + J * 89 As X$, 15 As Q$(J * 38 + 1004), 2 As Q$(J * 38 + 1005), 2 As Q$(J * 38 + 1006), 2 As Q$(J * 38 + 1007), 2 As Q$(J * 38 + 1008), 2 As Q$(J * 38 + 1009), 2 As Q$(J * 38 + 1010), 2 As Q$(J * 38 + 1011), 2 As Q$(J * 38 + 1012), 2 As Q$(J * 38 + 1013), 2 As Q$(J * 38 + 1014), 2 As Q$(J * 38 + 1015), 2020 - 37 - J * 89 As X$
            Field #1, 2359 + J * 89 As X$, 2 As Q$(J * 38 + 1016), 2 As Q$(J * 38 + 1017), 2 As Q$(J * 38 + 1018), 2 As Q$(J * 38 + 1019), 2 As Q$(J * 38 + 1020), 2 As Q$(J * 38 + 1021), 2 As Q$(J * 38 + 1022), 2 As Q$(J * 38 + 1023), 2 As Q$(J * 38 + 1024), 2 As Q$(J * 38 + 1025), 2 As Q$(J * 38 + 1026), 2 As Q$(J * 38 + 1027), 2020 - 61 - J * 89 As X$
            Field #1, 2383 + J * 89 As X$, 2 As Q$(J * 38 + 1028), 2 As Q$(J * 38 + 1029), 2 As Q$(J * 38 + 1030), 2 As Q$(J * 38 + 1031), 2 As Q$(J * 38 + 1032), 2 As Q$(J * 38 + 1033), 2 As Q$(J * 38 + 1034), 2 As Q$(J * 38 + 1035), 2 As Q$(J * 38 + 1036), 2 As Q$(J * 38 + 1037), 2 As Q$(J * 38 + 1038), 2 As Q$(J * 38 + 1039), 2020 - 85 - J * 89 As X$
            Field #1, 2407 + J * 89 As X$, 2 As Q$(J * 38 + 1040), 2 As Q$(J * 38 + 1041), 2020 - 89 - J * 89 As X$
        Next

        Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

        LSet Q$(0) = tradeA$(tradeIdx)

        For J = 1 To 13:
            LSet Q$(J) = MKI$(tradeL%(tradeIdx, J))
        Next

        LSet Q$(14) = MKS$(K9&(tradeIdx))

        For J = 0 To 22
            batter$ = tradeB$(tradeIdx, J)
            LSet Q$(J * 43 + 15) = tradeB$(tradeIdx, J)
            For I1 = 0 To 41:
                LSet Q$(J * 43 + I1 + 16) = MKI$(tradeB%(tradeIdx, J, I1))
            Next
        Next

        For J = 0 To 21
            LSet Q$(J * 38 + 1004) = tradeB$(tradeIdx, J)
            For I1 = 0 To 36:
                LSet Q$(J * 38 + I1 + 1005) = MKI$(tradeP%(tradeIdx, J, I1))
            Next
        Next

        LSet Q$(1840) = Stadiums$(tradeIdx)
        LSet Q$(1841) = Managers$(tradeIdx)
        LSet Q$(1842) = teamAbbrevs$(tradeIdx)
        LSet Q$(1843) = MKI$(999)

        saveIdx = teamIndexes(tradeIdx)
        Put #1, teamIndexes(tradeIdx)

        Close 1

        SCK = 0

        For X = 1 To 24
            SCK = SCK + tradeSA%(tradeIdx, X)
        Next

        If SCK > 0 Then

            tradeTS%(tradeIdx, 0) = 999

            For X = 1 To 11
                tradeTS%(tradeIdx, X) = -1
            Next

            Open diskPaths$(0) + "LR." + yearNumbers$(tradeIdx) For Random As #1 Len = 4108

            For X = 0 To 11
                Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
            Next

            For X = 0 To 22
                For X1 = 1 To 14
                    Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
                Next
            Next

            For X = 1 To 24
                Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
            Next

            For X = 0 To 22
                For X1 = 1 To 24
                    Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
                Next
            Next

            For X = 0 To 11: LSet Q$(X + 1) = MKI$(tradeTS%(tradeIdx, X)): Next

            For X = 0 To 22
                For X1 = 1 To 14
                    LSet Q$(12 + X1 + X * 14) = MKI$(tradeB%(tradeIdx, X, X1 + 41))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    LSet Q$(334 + X1 + X * 14) = MKI$(tradeP%(tradeIdx, X, X1 + 36))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    LSet Q$(642 + X1 + X * 14) = MKI$(tradeP%(tradeIdx, X, X1 + 50))
                Next
            Next

            For X = 1 To 24
                LSet Q$(X + 950) = MKI$(tradeSA%(tradeIdx, X))
            Next

            For X = 0 To 22
                For X1 = 1 To 24
                    LSet Q$(975 + X1 + X * 24) = MKI$(tradeB%(tradeIdx, X, X1 + 55))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    LSet Q$(1528 + X1 + X * 24) = MKI$(tradeP%(tradeIdx, X, X1 + 64))
                Next
            Next

            Put #1, teamIndexes(tradeIdx)

            Close 1

        End If

    Next tradeIdx

    For I = 0 To 1

        If tradeS(I) <> 1 Then

            Cls

            For XX = 4 To 11
                If Mid$(tradeSA$(I), XX, 1) = " " Or Mid$(tradeSA$(I), XX, 1) = "." Then Mid$(tradeSA$(I), XX, 1) = "_"
            Next

            QQ = 8

            For XX = 11 To 4 Step -1
                If Mid$(tradeSA$(I), XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            F$ = Mid$(tradeSA$(I), 4, QQ)
            R$ = F$ + "." + Left$(tradeSA$(I), 2)

            Open diskPaths$(1) + R$ For Output As #1

            Print #1, A1$(I)
            Print #1, W2%(I)
            Print #1, L2%(I)

            For I1 = 0 To 22:
                Print #1, tradeB1$(I, I1)
                Print #1, tradeB3%(I, I1)
            Next I1

            For I1 = 0 To 21:
                Print #1, tradeP1$(I, I1)
                Print #1, tradeP3%(I, I1)
            Next I1

            For I1 = 0 To 22:
                For I2 = 0 To 21:
                    Print #1, tradeB0%(I, I1, I2)
                Next I2
            Next I1

            For I1 = 0 To 21:
                For I2 = 0 To 41:
                    Print #1, tradeP0%(I, I1, I2)
                Next I2
            Next I1

            Print #1, tradeD0%(I)
            Print #1, tradeD1%(I)

            For I1 = 0 To 21: Print #1, TEAM%(I, I1): Next I1

            For I1 = 0 To 22: Print #1, tradeT1%(I, I1): Next I1

            Close #1

            Open diskPaths$(1) + R$ + "R" For Output As #1

            For I1 = 0 To 22:
                For I2 = 0 To 6
                    Print #1, tradeSS%(I, I1, I2)
                Next I2
            Next I1

            Close #1

        End If
    Next I

End Sub

'----------------------------------------
'    TradeSummary_Batters Subroutine
'----------------------------------------
'...explanation...
Sub TradeSummary_Batters (p1idx, p2idx)

    Shared tradeA$(), tradeB$()
    Shared tradeL%(), tradeB%(), tradeB0%()

    Dim progC$(9), plateX$(4)

    plateX$(0) = "LP"
    plateX$(1) = "L"
    plateX$(2) = "S"
    plateX$(3) = "R"
    plateX$(4) = "RP"

    progC$(0) = "DH"
    progC$(1) = "P"
    progC$(2) = "C"
    progC$(3) = "1B"
    progC$(4) = "2B"
    progC$(5) = "3B"
    progC$(6) = "SS"
    progC$(7) = "LF"
    progC$(8) = "CF"
    progC$(9) = "RF"

    Cls
    Color tradeL%(0, 11), tradeL%(0, 12): Print tradeA$(0);
    Locate , 41
    Color tradeL%(1, 11), tradeL%(1, 12): Print tradeA$(1)

    Color 15, 0
    Print tradeB$(0, p1idx); " "; plateX$(tradeB%(0, p1idx, 0) + 2); " "; progC$(tradeB%(0, p1idx, 22));
    If tradeB%(0, p1idx, 23) >= 0 Then Print ","; progC$(tradeB%(0, p1idx, 23));
    If tradeB%(0, p1idx, 24) >= 0 Then Print ","; progC$(tradeB%(0, p1idx, 24));
    If tradeB%(0, p1idx, 25) >= 0 Then Print ","; progC$(tradeB%(0, p1idx, 25));

    Locate , 41:

    Print tradeB$(1, p2idx); " "; plateX$(tradeB%(1, p2idx, 0) + 2); " "; progC$(tradeB%(1, p2idx, 22));
    If tradeB%(1, p2idx, 23) >= 0 Then Print ","; progC$(tradeB%(1, p2idx, 23));
    If tradeB%(1, p2idx, 24) >= 0 Then Print ","; progC$(tradeB%(1, p2idx, 24));
    If tradeB%(1, p2idx, 25) >= 0 Then Print ","; progC$(tradeB%(1, p2idx, 25));

    Locate , 1: Print
    Print "REAL";
    Locate , 15
    Print "COMP";
    Locate , 41
    Print "REAL";
    Locate , 55
    Print "COMP"
    Locate , 1

    Locate , 1
    Print tradeB%(0, p1idx, 3);
    Locate , 10
    Print "G";
    Locate , 15
    Print tradeB0%(0, p1idx, 14);
    Locate , 41
    Print tradeB%(1, p2idx, 3);
    Locate , 50
    Print "G";
    Locate , 55
    Print tradeB0%(1, p2idx, 14)

    Locate , 1
    Print tradeB%(0, p1idx, 4);
    Locate , 10
    Print "AB";
    Locate , 15
    Print tradeB0%(0, p1idx, 0);
    Locate , 41
    Print tradeB%(1, p2idx, 4);
    Locate , 50
    Print "AB";
    Locate , 55
    Print tradeB0%(1, p2idx, 0)

    Locate , 1
    Print tradeB%(0, p1idx, 5);
    Locate , 10
    Print "R";
    Locate , 15
    Print tradeB0%(0, p1idx, 2);
    Locate , 41
    Print tradeB%(1, p2idx, 5);
    Locate , 50
    Print "R";
    Locate , 55
    Print tradeB0%(1, p2idx, 2)

    Locate , 1
    Print tradeB%(0, p1idx, 6);
    Locate , 10
    Print "H";
    Locate , 15
    Print tradeB0%(0, p1idx, 1);
    Locate , 41
    Print tradeB%(1, p2idx, 6);
    Locate , 50
    Print "H";
    Locate , 55
    Print tradeB0%(1, p2idx, 1)


    Locate , 1
    Print tradeB%(0, p1idx, 10);
    Locate , 9
    Print "RBI";
    Locate , 15
    Print tradeB0%(0, p1idx, 3);
    Locate , 41
    Print tradeB%(1, p2idx, 10);
    Locate , 49
    Print "RBI";
    Locate , 55
    Print tradeB0%(1, p2idx, 3)

    Locate , 1
    Print tradeB%(0, p1idx, 7);
    Locate , 10
    Print "DB";
    Locate , 15
    Print tradeB0%(0, p1idx, 4);
    Locate , 41
    Print tradeB%(1, p2idx, 7);
    Locate , 50
    Print "DB";: Locate , 55
    Print tradeB0%(1, p2idx, 4)

    Locate , 1
    Print tradeB%(0, p1idx, 8);
    Locate , 10
    Print "TR";
    Locate , 15
    Print tradeB0%(0, p1idx, 5);
    Locate , 41
    Print tradeB%(1, p2idx, 8);
    Locate , 50
    Print "TR";
    Locate , 55
    Print tradeB0%(1, p2idx, 5)

    Locate , 1
    Print tradeB%(0, p1idx, 9);
    Locate , 10
    Print "HR";
    Locate , 15
    Print tradeB0%(0, p1idx, 6);
    Locate , 41
    Print tradeB%(1, p2idx, 9);
    Locate , 50
    Print "HR";
    Locate , 55
    Print tradeB0%(1, p2idx, 6)

    Locate , 1
    Print tradeB%(0, p1idx, 11);
    Locate , 10
    Print "BB";
    Locate , 15
    Print tradeB0%(0, p1idx, 7);
    Locate , 41
    Print tradeB%(1, p2idx, 11);
    Locate , 50
    Print "BB";
    Locate , 55
    Print tradeB0%(1, p2idx, 7)

    Locate , 1
    Print tradeB%(0, p1idx, 12);
    Locate , 10
    Print "SO";
    Locate , 15
    Print tradeB0%(0, p1idx, 8);
    Locate , 41
    Print tradeB%(1, p2idx, 12);
    Locate , 50
    Print "S0";
    Locate , 55
    Print tradeB0%(1, p2idx, 8)

    Locate , 1
    If tradeB%(0, p1idx, 4) = 0 Then
        Print "-.---";
    Else
        If tradeB%(0, p1idx, 6) = tradeB%(0, p1idx, 4) Then
            Print "1.000";
        Else
            Print Using " .###"; (tradeB%(0, p1idx, 6) / tradeB%(0, p1idx, 4));
        End If
    End If

    Locate , 9
    Print "BAVG";: Locate , 15
    If tradeB0%(0, p1idx, 0) = 0 Then
        Print "-.---";
    Else
        If tradeB0%(0, p1idx, 0) = tradeB0%(0, p1idx, 1) Then
            Print "1.000";
        Else
            Print Using " .###"; (tradeB0%(0, p1idx, 1) / tradeB0%(0, p1idx, 0));
        End If
    End If

    Locate , 41
    If tradeB%(1, p2idx, 4) = 0 Then
        Print "-.---";
    Else
        If tradeB%(1, p2idx, 6) = tradeB%(1, p2idx, 4) Then
            Print "1.000";
        Else
            Print Using " .###"; (tradeB%(1, p2idx, 6) / tradeB%(1, p2idx, 4));
        End If
    End If

    Locate , 49
    Print "BAVG";: Locate , 55
    If tradeB0%(1, p2idx, 0) = 0 Then
        Print "-.---"
    Else
        If tradeB0%(1, p2idx, 0) = tradeB0%(1, p2idx, 1) Then
            Print "1.000"
        Else
            Print Using " .###"; (tradeB0%(1, p2idx, 1) / tradeB0%(1, p2idx, 0))
        End If
    End If

    Locate , 1
    Print tradeB%(0, p1idx, 13);
    Locate , 10
    Print "SB";
    Locate , 15
    Print tradeB0%(0, p1idx, 9);
    Locate , 41
    Print tradeB%(1, p2idx, 13);
    Locate , 50
    Print "SB";
    Locate , 55
    Print tradeB0%(1, p2idx, 9)

    Locate , 1
    Print tradeB%(0, p1idx, 14);
    Locate , 10
    Print "CS";
    Locate , 15
    Print tradeB0%(0, p1idx, 10);
    Locate , 41
    Print tradeB%(1, p2idx, 14);
    Locate , 50
    Print "CS";
    Locate , 55
    Print tradeB0%(1, p2idx, 10)

End Sub

'----------------------------------------
'     PrintTradeSummary Subroutine
'----------------------------------------
'...explanation...
Sub TradeSummary_Pitchers (p1idx, p2idx)

    Shared tradeA$(), tradeB$()
    Shared tradeL%(), tradeP%(), tradeP0%()
    Dim plateX$(4)

    plateX$(0) = "LP"
    plateX$(1) = "L"
    plateX$(2) = "S"
    plateX$(3) = "R"
    plateX$(4) = "RP"

    '905
    Cls
    Color tradeL%(0, 11), tradeL%(0, 12): Print tradeA$(0);
    Locate , 41
    Color tradeL%(1, 11), tradeL%(1, 12): Print tradeA$(1)
    Color 15, 0

    Locate , 1: Print tradeB$(0, p1idx); " "; plateX$(tradeP%(0, p1idx, 0) + 2);
    Locate , 41: Print tradeB$(1, p2idx); " "; plateX$(tradeP%(1, p2idx, 0) + 2)
    Locate , 1: Print: Print "REAL";
    Locate , 15: Print "COMP";
    Locate , 41: Print "REAL";
    Locate , 55: Print "COMP"
    Locate , 1: Print tradeP%(0, p1idx, 1); "-"; tradeP%(0, p1idx, 2);
    Locate , 9: Print "W-L";
    Locate , 15: Print tradeP0%(0, p1idx, 6); "-"; tradeP0%(0, p1idx, 7);
    Locate , 41: Print tradeP%(1, p2idx, 1); "-"; tradeP%(1, p2idx, 2);
    Locate , 49: Print "W-L";
    Locate , 55: Print tradeP0%(1, p2idx, 6); "-"; tradeP0%(1, p2idx, 7)
    Locate , 1: Print tradeP%(0, p1idx, 4);: Locate , 10: Print "G";
    Locate , 15: Print tradeP0%(0, p1idx, 8);
    Locate , 41: Print tradeP%(1, p2idx, 4);
    Locate , 50: Print "G";
    Locate , 55: Print tradeP0%(1, p2idx, 8)
    Locate , 1: Print tradeP%(0, p1idx, 5);
    Locate , 10: Print "GS";
    Locate , 15: Print tradeP0%(0, p1idx, 9);
    Locate , 41: Print tradeP%(1, p2idx, 5);
    Locate , 50: Print "GS";
    Locate , 55: Print tradeP0%(1, p2idx, 9)
    Locate , 1: Print tradeP%(0, p1idx, 3);
    Locate , 10: Print "SV";
    Locate , 15: Print tradeP0%(0, p1idx, 12);
    Locate , 41: Print tradeP%(1, p2idx, 3);
    Locate , 50: Print "SV";
    Locate , 55: Print tradeP0%(1, p2idx, 12)
    Locate , 1: Print tradeP%(0, p1idx, 6);
    Locate , 10: Print "IP";
    Locate , 15: Print Int((tradeP0%(0, p1idx, 0) / 3) + .5);
    Locate , 41: Print tradeP%(1, p2idx, 6);
    Locate , 50: Print "IP";
    Locate , 55: Print Int((tradeP0%(1, p2idx, 0) / 3) + .5)
    Locate , 1: Print tradeP%(0, p1idx, 7);
    Locate , 10: Print "H";
    Locate , 15: Print tradeP0%(0, p1idx, 1);
    Locate , 41: Print tradeP%(1, p2idx, 7);
    Locate , 50: Print "H";
    Locate , 55: Print tradeP0%(1, p2idx, 1)
    Locate , 1: Print tradeP%(0, p1idx, 8);
    Locate , 10: Print "BB";
    Locate , 15: Print tradeP0%(0, p1idx, 4);
    Locate , 41: Print tradeP%(1, p2idx, 8);
    Locate , 50: Print "BB";
    Locate , 55: Print tradeP0%(1, p2idx, 4)
    Locate , 1: Print tradeP%(0, p1idx, 9);
    Locate , 10: Print "SO";
    Locate , 15: Print tradeP0%(0, p1idx, 5);
    Locate , 41: Print tradeP%(1, p2idx, 9);
    Locate , 50: Print "SO";
    Locate , 55: Print tradeP0%(1, p2idx, 5)
    Locate , 1: Print tradeP%(0, p1idx, 13);
    Locate , 10: Print "HR";
    Locate , 15: Print tradeP0%(0, p1idx, 14);
    Locate , 41: Print tradeP%(1, p2idx, 13);
    Locate , 50: Print "HR";
    Locate , 55: Print tradeP0%(1, p2idx, 14)
    Locate , 1: Print Using "##.##"; tradeP%(0, p1idx, 10) / 100;
    Locate , 9: Print "ERA";

    Locate , 15:

    If tradeP0%(0, p1idx, 0) = 0 Then
        Print " -.--";
    Else
        Print Using "##.##"; tradeP0%(0, p1idx, 3) * 27 / tradeP0%(0, p1idx, 0);
    End If

    Locate , 41

    Print Using "##.##"; tradeP%(1, p2idx, 10) / 100;
    Locate , 49: Print "ERA";

    Locate , 55: If tradeP0%(1, p2idx, 0) = 0 Then
        Print " -.--"
    Else
        Print Using "##.##"; tradeP0%(1, p2idx, 3) * 27 / tradeP0%(1, p2idx, 0)

        If tradeP0%(0, p1idx, 0) > 0 Then G1% = Int((((tradeP0%(0, p1idx, 3) * 9) / (tradeP0%(0, p1idx, 0) / 3)) + .0005) * 100)

        E1% = Int(G1% / 100)
        F1% = G1% - (E1% * 100)
    End If

End Sub

