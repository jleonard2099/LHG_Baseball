Sub MERGE ()

    _Title "Full Count Baseball - Merge Stats"

    Do
        Cls
        Color 12: Print "STATISTICS MERGE/ALIGN MENU"
        Print

        Color 14: Print "(1) ";: Color 15: Print "MERGE STAT FILES"
        Print
        Color 14: Print "(2) ";: Color 15: Print "ALIGN STAT FILES"
        Print
        Color 14: Print "(3) ";: Color 15: Print "RETURN TO UTILITIES MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 Or I <= 3 Or I$ = Chr$(27)

        If I$ <> Chr$(27) Then

            Select Case I
                Case 1: Call MergeIt
                Case 2: Call ALIGN
                Case 3: Cls
            End Select

        End If

    Loop Until I = 3 Or I$ = Chr$(27)

End Sub

'----------------------------------------
'       MergeIt Subroutine
'----------------------------------------
'...explanation...
Sub MergeIt ()
    Shared statB$(), statP$()

    Dim BR$(22), DR$(2)
    Dim statH$(162), H1$(40), H1R$(40), HR$(162)
    Dim L$(162), LR$(162)
    Dim PL$(162), PLR$(162), PR$(21)
    Dim W$(162), WR$(162)

    Dim mergeB%(22), careerB0%(22, 22), B0R%(22, 22), BR%(22)
    Dim H%(162), HR%(162), I%(162), IR%(162)
    Dim mergeP%(21), P0%(21, 41), P0R%(21, 41), PR%(21)
    Dim S0%(22, 6), SR%(22, 6)
    Dim V%(162), VR%(162)

    Dim TR(22), TR1(22)

    'Dim statT%(0 to 21), statT1%(0 to 22)
    Shared statT%(), statT1%()

    Do

        Color 12, 0
        Cls

        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"
        Color 15
        Locate 4, 5: Print "EXAMPLE - A:"
        Locate , 5: Print "          C:\HAFFNER"
        Locate , 5: Print "          C:\HAFFNER\1960"
        Locate , 5: Print "          D:\BASEBALL"

        For J = 10 To 13:
            Locate J
            Print Space$(60)
        Next

        Do
            Color 10
            Locate 10, 5: Line Input "ROAD STATS:     "; PR$
            Locate , 5
            Locate , 5: Line Input "CURRENT STATS:  "; PC$

            Color 14

            Locate 13, 5: Print "ANY CHANGE (Y/N)"
            I$ = GetKeyPress$

        Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

        If UCase$(I$) <> "N" And I$ <> Chr$(27) Then

            Call GetDiskID_MERGE(A$)

            statB$ = A$

            For XX = 4 To 11:
                If Mid$(statB$, XX, 1) = " " Or Mid$(statB$, XX, 1) = "." Then Mid$(statB$, XX, 1) = "_"
            Next XX

            For XX = 12 To 14:
                If Mid$(statB$, XX, 2) = "ST" Then Mid$(statB$, 9, 1) = "_": Mid$(statB$, 10, 2) = "ST"
            Next XX

            QQ = 8
            For XX = 11 To 4 Step -1:
                If Mid$(statB$, XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            statFile$ = Mid$(statB$, 4, QQ) + "." + Left$(statB$, 2)

            Cls

            Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
            Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
            Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$

            Do
                Locate 14, 15: Print "ANY CHANGE (YN)"
                J$ = GetKeyPress$
            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

            If UCase$(J$) = "N" Then

                If Not _FileExists(PR$ + "\" + statFile$) Then

                    Cls
                    Locate 2, 10
                    Color 7
                    Print "NON-EXISTENT PATH OR INCORRECT FILE NAME"
                    Print

                    Do
                        Locate , 10
                        Print "TRY AGAIN (YN) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        Print
                        Print "HIT ENTER TO RE-ENTER INFORMATION"
                        I$ = GetKeyPress$
                    End If

                Else

                    Open PR$ + "\" + statFile$ For Input As #1
                    Input #1, A$: Input #1, WR2%: Input #1, LR2%

                    For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I
                    For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I
                    For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I
                    For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

                    Input #1, D0R%: Input #1, D1R%
                    For I = 0 To 21: Input #1, TR(I): Next I
                    For I = 0 To 22: Input #1, TR1(I): Next I

                    Close #1

                    If _FileExists(PR$ + "\" + statFile$ + "R") Then
                        Open PR$ + "\" + statFile$ + "R" For Input As #1
                        For I = 0 To 22
                            For I1 = 0 To 6
                                Input #1, SR%(I, I1)
                            Next
                        Next
                        Close 1
                    End If

                    Open PR$ + "\" + statFile$ + "S" For Input As #1
                    Input #1, YR%
                    For I = 1 To 162: Input #1, HR%(I): Next I
                    For I = 1 To 162: Input #1, HR$(I): Next I
                    For I = 1 To 162: Input #1, VR%(I): Next I
                    For I = 1 To 162: Input #1, PLR$(I): Next I
                    For I = 1 To 162: Input #1, WR$(I): Next I
                    For I = 1 To 162: Input #1, LR$(I): Next I
                    For I = 1 To 162: Input #1, IR%(I): Next I
                    Close #1

                    If Not _FileExists(PC$ + "\" + statFile$) Then
                        Cls
                        Locate 2, 10
                        Color 7
                        Print "NON-EXISTENT PATH OR INCORRECT FILE NAME"
                        Print

                        Do
                            Locate , 10
                            Print "TRY AGAIN (YN) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "Y" Then
                            Print
                            Print "HIT ENTER TO RE-ENTER INFORMATION"
                            I$ = GetKeyPress$
                        End If

                    Else

                        Open PC$ + "\" + statFile$ For Input As #1

                        Input #1, A$: Input #1, W2%: Input #1, L2%

                        For I = 0 To 22: Input #1, statB$(I): Input #1, mergeB%(I): Next I
                        For I = 0 To 21: Input #1, statP$(I): Input #1, mergeP%(I): Next I
                        For I = 0 To 22: For I1 = 0 To 21: Input #1, careerB0%(I, I1): Next I1: Next I
                        For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

                        Input #1, D0%: Input #1, D1%
                        For I = 0 To 21: Input #1, statT%(I): Next I
                        For I = 0 To 22: Input #1, statT1%(I): Next I

                        Close #1

                        If _FileExists(PC$ + "\" + statFile$ + "R") Then
                            Open PC$ + "\" + statFile$ + "R" For Input As #1
                            For I = 0 To 22
                                For I1 = 0 To 6
                                    Input #1, S0%(I, I1)
                            Next: Next
                            Close 1
                        End If

                        Open PC$ + "\" + statFile$ + "S" For Input As #1
                        Input #1, Y%
                        For I = 1 To 162: Input #1, H%(I): Next I
                        For I = 1 To 162: Input #1, statH$(I): Next I
                        For I = 1 To 162: Input #1, V%(I): Next I
                        For I = 1 To 162: Input #1, PL$(I): Next I
                        For I = 1 To 162: Input #1, W$(I): Next I
                        For I = 1 To 162: Input #1, L$(I): Next I
                        For I = 1 To 162: Input #1, I%(I): Next I
                        Close #1

                        SEQ = 0

                        For K = 0 To 22
                            If UCase$(BR$(K)) <> UCase$(statB$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 21
                            If UCase$(PR$(K)) <> UCase$(statP$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            For K = 1 To YR%
                                Y% = Y% + 1
                                H%(Y%) = HR%(K)
                                statH$(Y%) = HR$(K)
                                V%(Y%) = VR%(K)
                                PL$(Y%) = PLR$(K)
                                W$(Y%) = WR$(K)
                                L$(Y%) = LR$(K)
                                I%(Y%) = IR%(K)
                            Next

                            W2% = W2% + WR2%: L2% = L2% + LR2%

                            Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

                            For I = 0 To 22
                                For I1 = 0 To 6
                                    S0%(I, I1) = S0%(I, I1) + SR%(I, I1)
                                Next

                                For I1 = 0 To 11: careerB0%(I, I1) = careerB0%(I, I1) + B0R%(I, I1): Next I1
                                For I1 = 14 To 21: careerB0%(I, I1) = careerB0%(I, I1) + B0R%(I, I1): Next I1
                            Next I

                            For I = 0 To 21
                                For I1 = 0 To 34: P0%(I, I1) = P0%(I, I1) + P0R%(I, I1): Next I1
                                For I1 = 37 To 41: P0%(I, I1) = P0%(I, I1) + P0R%(I, I1): Next I1
                            Next I

                            D0% = D0% + DR0%: D1% = D1% + DR1%

                            For I = 0 To 21: statT%(I) = statT%(I) + TR(I): Next
                            For I = 0 To 22: statT1%(I) = statT1%(I) + TR1(I): Next

                            Open PC$ + "\" + statFile$ For Output As #1

                            Write #1, A$: Write #1, W2%: Write #1, L2%

                            For I = 0 To 22: Write #1, statB$(I): Write #1, mergeB%(I): Next I
                            For I = 0 To 21: Write #1, statP$(I): Write #1, mergeP%(I): Next I
                            For I = 0 To 22: For I1 = 0 To 21: Write #1, careerB0%(I, I1): Next I1: Next I
                            For I = 0 To 21: For I1 = 0 To 41: Write #1, P0%(I, I1): Next I1: Next I

                            Write #1, D0%: Write #1, D1%

                            For I = 0 To 21: Write #1, statT%(I): Next I
                            For I = 0 To 22: Write #1, statT1%(I): Next I

                            Close #1

                            Open PC$ + "\" + statFile$ + "R" For Output As #1
                            For I = 0 To 22
                                For I1 = 0 To 6
                                    Write #1, S0%(I, I1)
                                Next
                            Next
                            Close 1

                            Open PC$ + "\" + statFile$ + "S" For Output As #1
                            Write #1, Y%
                            For I = 1 To 162: Write #1, H%(I): Next I
                            For I = 1 To 162: Write #1, statH$(I): Next I
                            For I = 1 To 162: Write #1, V%(I): Next I
                            For I = 1 To 162: Write #1, PL$(I): Next I
                            For I = 1 To 162: Write #1, W$(I): Next I
                            For I = 1 To 162: Write #1, L$(I): Next I
                            For I = 1 To 162: Write #1, I%(I): Next I
                            Close #1

                            Y = 0

                            Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

                        Else

                            Cls
                            Color 14
                            Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$
                            Color 15
                            Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                            Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                            Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                            Locate 11, 24
                            Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                            Locate , 31: Print "ENTER STATS MANUALLY."
                            Locate 15, 29
                            Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                        End If 'Done checking for SEQ = 0

                    End If 'Done checking for stat file

                End If 'Done checking for road file

            End If 'Done cheking for changes

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"  Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       GetDiskID_MERGE  Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskID_MERGE (A$)

    Shared ratings%()

    Do

        Color 15, 0
        Cls
        JJ = 6
        SN = 1

        Locate 6, 8: Color 15, 4

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ = Chr$(13) Then
                Call SelectTeam_MERGE(numberTeams, YN$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0
                If NY$ = "" Then
                    Call SelectTeam_MERGE(numberTeams, NY$)
                Else
                    YN$ = NY$
                    Erase teamNames$

                    numberTeams = 0

                    Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
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

                    Call SelectTeam_MERGE(numberTeams, YN$)
                End If
            End If

            A$ = RTrim$(Q$(0))

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color ratings%(11), ratings%(12): Print A$

                Color 15, 4

                Do
                    Locate , 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

            End If

        Else
            statFile$ = "END"
        End If

    Loop Until UCase$(I$) = "N" Or statFile$ = "END"

End Sub

'----------------------------------------
'       SelectTeam_MERGE  Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_MERGE (numberTeams, yearNumber$)

    Shared ratings%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColor% = 15: FileBGColor% = 4
    HiLiteFGColor% = 0: HiLiteBGColor% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4
    Cls
    Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "Default"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        T1 = teamIndex%(Selection%)
        Locate 2, 33: Color 31: Print "LOADING..."

        If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
            BO% = 1
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

            Get #1, T1

            For I = 1 To 13
                ratings%(I) = CVI(Q$(I))
            Next

            Close 1

        End If

    Else
        BO% = 1
    End If

End Sub
