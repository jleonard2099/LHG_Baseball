Sub ALIGN ()

    _Title "Full Count Baseball - Align Stats"

    Dim statB$(23), BN$(23), BR$(23)
    Dim statP$(22), PN$(22), PR$(22)

    Dim careerB0%(23, 22), B0N%(23, 22), B0R%(23, 22)
    Dim draftB%(23), BN%(23), BR%(23)
    Dim P0%(22, 42), P0N%(22, 42), P0R%(22, 42)
    Dim draftP%(22), PN%(22), PR%(22)
    Dim S0%(22, 6), S0N%(22, 6), S0R%(22, 6)

    Dim TR(22), TR1(23)

    Shared statT%(), statT1%()

    Do

        Color 12, 0
        Cls
        
        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO ALIGN"
        Color 15
        Locate 4, 5: Print "EXAMPLE - A:"
        Locate , 5: Print "          C:\HAFFNER"
        Locate , 5: Print "          C:\HAFFNER\1960"
        Locate , 5: Print "          D:\BASEBALL"

        For J = 10 To 13
            Locate J
            Print Space$(62)
        Next

        Color 10

        Do

            Locate 10, 5: Line Input "ROAD STATS:     "; PR$
            Locate , 5: Line Input "CURRENT STATS:  "; PC$

            Color 14

            Locate 13, 5: Print "ANY CHANGE (Y/N)"
            I$ = GetKeyPress$

        Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

        If UCase$(I$) <> "N" And I$ <> Chr$(27) Then

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)

            Color 11

            Call GetDiskID_ALIGN(A$)

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

            Locate 18, 5: Color 12: Print "SEARCHING FOR FILES . . ."

            If _FileExists(PR$ + "\" + statFile$) Then

                Cls
                Color 15

                Open PR$ + "\" + statFile$ For Input As #1

                Input #1, AR$
                Input #1, W2R%
                Input #1, L2R%

                For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I

                For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I

                For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I

                For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

                Input #1, DR0%
                Input #1, DR1%
                For I = 0 To 21: Input #1, TR(I): Next I
                For I = 0 To 22: Input #1, TR1(I): Next I

                Close #1

                If _FileExists(PR$ + "\" + statFile$ + "R") Then

                    Open PR$ + "\" + statFile$ + "R" For Input As #1
                    For I = 0 To 22
                        For I1 = 0 To 6
                            Input #1, S0R%(I, I1)
                        Next
                    Next
                    Close #1

                End If

                If _FileExists(PC$ + "\" + statFile$) Then

                    Open PC$ + "\" + statFile$ For Input As #1

                    Input #1, A$: Input #1, W2%: Input #1, L2%

                    For I = 0 To 22: Input #1, statB$(I): Input #1, draftB%(I): Next I

                    For I = 0 To 21: Input #1, statP$(I): Input #1, draftP%(I): Next I

                    For I = 0 To 22: For I1 = 0 To 21: Input #1, careerB0%(I, I1): Next I1: Next I

                    For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

                    Input #1, D0%: Input #1, D1%
                    For I = 0 To 21: Input #1, statT%(I): Next I
                    For I = 0 To 22: Input #1, statT1%(I): Next I

                    Close #1

                    If Not _FileExists(PC$ + "\" + statFile$ + "R") Then
                        Cls
                        Locate 2, 10: Color 7
                        Print "NON-EXISTANT PATH OR INCORRECT FILE NAME"
                        Print
                        Locate , 10
                        Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    Else

                        Open PC$ + "\" + statFile$ + "R" For Input As #1
                        For I = 0 To 22
                            For I1 = 0 To 6
                                Input #1, S0%(I, I1)
                            Next
                        Next
                        Close #1

                    End If

                    'Do

                    Cls
                    Locate 1, 34: Color 12: Print "STAT ROSTERS"
                    Locate , 33: Color 15: Print "CURRENT  ";
                    Color 7: Print " ROAD"

                    For I = 0 To 22: 
                        Color 12: Locate I + 1, 1: Print Using "##"; I;: Print " ";: Color 15: Print Left$(statB$(I), 10);: Color 12: Print Tab(17); Using "##"; I;: Print " ";: Color 7: Print Left$(BR$(I), 10);
                    Next

                    For I = 0 To 21: 
                        Color 12: Locate I + 1, 48: Print Using "##"; I;: Print " ";: Color 15: Print Left$(statP$(I), 10);: Color 12: Print Tab(62); Using "##"; I;: Print " ";: Color 7: Print Left$(PR$(I), 10);
                    Next

                    Color 11
                    Locate 7, 33: Print "HIT ANY KEY TO"
                    Locate 8, 37: Print "ALIGN"
                    I$ = GetKeyPress$

                    Color 11
                    Locate 7, 33: Print "ALIGNING FILES"
                    Locate 8, 37: Print Space$(7)

                    N = 0

                    Do

                        For J = 0 To 22

                            If UCase$(statB$(N)) = UCase$(BR$(J)) Then
                                BN$(N) = BR$(J): BN%(N) = BR%(J)

                                For K = 0 To 21
                                    B0N%(N, K) = B0R%(J, K)
                                Next K

                                For K = 0 To 6
                                    S0N%(N, K) = S0R%(J, K)
                                Next K

                            End If

                        Next J

                        N = N + 1

                    Loop Until N = 23

                    N = 0

                    Do
                        For J = 0 To 21

                            If UCase$(statP$(N)) = UCase$(PR$(J)) Then

                                PN$(N) = PR$(J)
                                PN%(N) = PR%(J)

                                For K = 0 To 41
                                    P0N%(N, K) = P0R%(J, K)
                                Next K

                            End If

                        Next J

                        N = N + 1

                    Loop Until N = 22

                    KKK = 1

                    SEQ = 0

                    'Check batters
                    For J = 0 To 22
                        If UCase$(BN$(J)) <> UCase$(statB$(J)) Then
                            SEQ = SEQ + 1
                        End If
                    Next J

                    'Check pitchers
                    For J = 0 To 21
                        If UCase$(PN$(J)) <> UCase$(statP$(J)) Then
                            SEQ = SEQ + 1
                        End If
                    Next J

                    If SEQ = 0 Then

                        Cls
                        Locate 1, 34
                        Color 12: Print "STAT ROSTERS"
                        Locate , 33
                        Color 15: Print "CURRENT  ";
                        Color 7: Print " ROAD"

                        For I = 0 To 22: Color 12: Locate I + 1, 1: Print Using "##"; I;: Print " ";: Color 15: Print Left$(statB$(I), 10);: Color 12: Print Tab(17); Using "##"; I;: Print " ";: Color 7: Print Left$(BN$(I), 10);: Next

                        For I = 0 To 21: Color 12: Locate I + 1, 48: Print Using "##"; I;: Print " ";: Color 15: Print Left$(statP$(I), 10);: Color 12: Print Tab(62); Using "##"; I;: Print " ";: Color 7: Print Left$(PN$(I), 10);: Next

                        Locate 7, 33
                        Color 11: Print "HIT ANY KEY TO"
                        Locate 8, 36: Print "CONTINUE"
                        I$ = GetKeyPress$

                        Open PR$ + "\" + statFile$ For Output As #1

                        Write #1, AR$
                        Write #1, W2R%
                        Write #1, L2R%

                        For I = 0 To 22: Write #1, BN$(I): Write #1, BN%(I): Next I
                        For I = 0 To 21: Write #1, PN$(I): Write #1, PN%(I): Next I
                        For I = 0 To 22: For I1 = 0 To 21: Write #1, B0N%(I, I1): Next I1: Next I
                        For I = 0 To 21: For I1 = 0 To 41: Write #1, P0N%(I, I1): Next I1: Next I

                        Write #1, DR0%
                        Write #1, DR1%
                        For I = 0 To 21: Write #1, TR(I): Next I
                        For I = 0 To 22: Write #1, TR1(I): Next I

                        Close #1

                        Open PR$ + "\" + statFile$ + "R" For Output As #1
                        For I = 0 To 22
                            For I1 = 0 To 6
                                Print #1, S0N%(I, I1)
                            Next
                        Next
                        Close 1

                        Cls

                        Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"
                        Print
                        Locate 12, 20
                        Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

                        Cls

                    Else

                        Cls
                        Locate 5
                        Color 14: Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
                        Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
                        Locate 23, 30
                        Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

                    End If 'Done checking SEQ

                End If 'Done checking for current stats

            End If 'Done checking for road stats

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"  Or I$ = Chr$(27)

End Sub



'----------------------------------------
'       GetDiskID_ALIGN  Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskID_ALIGN (A$)

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
                Call SelectTeam_ALIGN(numberTeams, YN$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0
                If NY$ = "" Then
                    Call SelectTeam_ALIGN(numberTeams, NY$)
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

                    Call SelectTeam_ALIGN(numberTeams, YN$)
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
                    Locate 3, 33: Print "ANY CHANGE (YN)";
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
'       SelectTeam_ALIGN  Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_ALIGN (numberTeams, yearNumber$)

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
