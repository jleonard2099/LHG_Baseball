'----------------------------------------
'             ALIGN Subroutine
'----------------------------------------
'This routine presents a menu / interface
'that assists the user with aligning 
'player names in separate stat files. 
'(ie, Road Vs. Local) This should be done 
'before merging stats, since player name 
'entry may not be consistent.
Sub ALIGN ()

    _Title "Full Count Baseball - Align Stats"

    Shared BR$(), PR$()
    Shared batterNames$(), pitcherNames$()

    Shared B0R%(), BR%()
    Shared P0R%(), PR%()

    Shared draftTR(), draftTR1()

    Shared statB0%(), statB%()
    Shared S0R%(), S0%()
    Shared statP%(), statP0%()
    Shared statT0%(), statT1%()

    Do

        Call GetMergePaths (pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)
            Color 11

            Call ChooseTeam (YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            Color 12, 0
            Cls
            Locate 18, 5: Color 12: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1

                Input #1, AR$
                Input #1, W2R%
                Input #1, L2R%

                For I = 0 To 22:
                    Input #1, BR$(I)
                    Input #1, BR%(I)
                Next I

                For I = 0 To 21:
                    Input #1, PR$(I)
                    Input #1, PR%(I)
                Next I

                For I = 0 To 22:
                    For I1 = 0 To 21:
                        Input #1, B0R%(I, I1)
                    Next I1
                Next I

                For I = 0 To 21:
                    For I1 = 0 To 41:
                        Input #1, P0R%(I, I1)
                    Next I1
                Next I

                Input #1, DR0%
                Input #1, DR1%

                For I = 0 To 21: 
                    Input #1, draftTR(I)
                Next I
                
                For I = 0 To 22: 
                    Input #1, draftTR1(I)
                Next I

                Close #1

                If _FileExists(pathRoadStats$ + statFile$ + "R") Then

                    Open pathRoadStats$ + statFile$ + "R" For Input As #1

                    For I = 0 To 22
                        For I1 = 0 To 6
                            Input #1, S0R%(I, I1)
                        Next
                    Next

                    Close #1

                End If

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Open pathCurrentStats$ + statFile$ For Input As #1

                    Input #1, A$
                    Input #1, W2%
                    Input #1, L2%

                    For I = 0 To 22:
                        Input #1, batterNames$(I)
                        Input #1, statB%(I)
                    Next I

                    For I = 0 To 21:
                        Input #1, pitcherNames$(I)
                        Input #1, statP%(I)
                    Next I

                    For I = 0 To 22:
                        For I1 = 0 To 21:
                            Input #1, statB0%(I, I1)
                        Next I1
                    Next I

                    For I = 0 To 21:
                        For I1 = 0 To 41:
                            Input #1, statP0%(I, I1)
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

                    If _FileExists(pathCurrentStats$ + statFile$ + "R") Then

                        Open pathCurrentStats$ + statFile$ + "R" For Input As #1

                        For I = 0 To 22
                            For I1 = 0 To 6
                                Input #1, S0%(I, I1)
                            Next
                        Next

                        Close #1

                        Call AlignStats(pathRoadStats$, statFile$)
                        DUMMY$ = "OK"

                    Else

                        Cls

                        Color 7
                        Locate 2, 10
                        Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                        Print
                        Locate , 10
                        Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    End If

                End If 'Done checking if current file exists

            Else

                Cls

                Color 7
                Locate 2, 10
                Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                Print
                Locate , 10
                Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

            End If 'Done checking if road file exists

        End If 'Done checking for change / ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         AlignStats Subroutine
'----------------------------------------
'This routine presents the user with a 
'summary of the statistics in each file.
'It then performs the alignment before 
'presenting the user a chance to review 
'the results.
Sub AlignStats (pathRoadStats$, statFile$)

    Shared batterNames$(), pitcherNames$()

    Shared BN$(), PN$()
    Shared B0N%(), BN%()
    Shared P0N%(), PN%()
    Shared S0N%()

    Shared BR$(), PR$()
    Shared PR%()
    Shared B0R%(), BR%()
    Shared P0R%(), S0R%()

    Cls
    Locate 1, 34
    Color 12: Print "STAT ROSTERS"

    Locate , 33
    Color 15: Print "CURRENT  ";
    Color 7: Print " ROAD"

    For I = 0 To 22:
        Color 12
        Locate I + 1, 1: Print Using "##"; I;
        Print " ";
        Color 15: Print Left$(batterNames$(I), 10);
        Color 12: Print Tab(17); Using "##"; I;
        Print " ";
        Color 7: Print Left$(BR$(I), 10);
    Next

    For I = 0 To 21:
        Color 12
        Locate I + 1, 48: Print Using "##"; I;
        Print " ";
        Color 15: Print Left$(pitcherNames$(I), 10);
        Color 12: Print Tab(62); Using "##"; I;
        Print " ";
        Color 7: Print Left$(PR$(I), 10);
    Next

    Color 11
    Locate 24, 31: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 24, 30: Print "ALIGNING FILES . . ."

    N = 0

    Do

        For J = 0 To 22

            If UCase$(batterNames$(N)) = UCase$(BR$(J)) Then

                BN$(N) = BR$(J)
                BN%(N) = BR%(J)

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

            If UCase$(pitcherNames$(N)) = UCase$(PR$(J)) Then

                PN$(N) = PR$(J)
                PN%(N) = PR%(J)

                For K = 0 To 41
                    P0N%(N, K) = P0R%(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 22

    SEQ = 0

    'Check batters
    For J = 0 To 22
        If UCase$(BN$(J)) <> UCase$(batterNames$(J)) Then
            SEQ = SEQ + 1
        End If
    Next J

    'Check pitchers
    For J = 0 To 21
        If UCase$(PN$(J)) <> UCase$(pitcherNames$(J)) Then
            SEQ = SEQ + 1
        End If
    Next J

    If SEQ = 0 Then
        Call ReviewAndSaveAlignment(pathRoadStats$, statFile$)
    Else

        Color 14
        Locate 20
        Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
        Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
        Color 13
        Locate 23, 30
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    End If 'Done checking SEQ

End Sub


'----------------------------------------
'    ReviewAndSaveAlignment Subroutine
'----------------------------------------
'This routine displays the results of the 
'player name alignment, then gives the end
'user a chance to actually save the results 
Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)

    Shared batterNames$(), pitcherNames$()

    Shared draftTR(), draftTR1()

    Shared BN$(), PN$()
    Shared B0N%(), BN%()
    Shared P0N%(), PN%()
    Shared S0N%()

    Cls
    Locate 1, 34
    Color 12: Print "STAT ROSTERS"
    Locate , 33
    Color 15: Print "CURRENT  ";
    Color 7: Print " ROAD"

    For I = 0 To 22:
        Color 12: Locate I + 1, 1: Print Using "##"; I;
        Print " ";: Color 15: Print Left$(batterNames$(I), 10);
        Color 12: Print Tab(17); Using "##"; I;: Print " ";
        Color 7: Print Left$(BN$(I), 10);
    Next

    For I = 0 To 21:
        Color 12
        Locate I + 1, 48: Print Using "##"; I;
        Print " ";
        Color 15: Print Left$(pitcherNames$(I), 10);
        Color 12: Print Tab(62); Using "##"; I;
        Print " ";
        Color 7: Print Left$(PN$(I), 10);
    Next

    Color 11
    Locate 24, 31: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 24, 30: Print "ALIGNING FILES . . ."

    Open pathRoadStats$ + statFile$ For Output As #1

    Write #1, AR$
    Write #1, W2R%
    Write #1, L2R%

    For I = 0 To 22: Write #1, BN$(I): Write #1, BN%(I): Next I

    For I = 0 To 21: Write #1, PN$(I): Write #1, PN%(I): Next I

    For I = 0 To 22: For I1 = 0 To 21: Write #1, B0N%(I, I1): Next I1: Next I

    For I = 0 To 21: For I1 = 0 To 41: Write #1, P0N%(I, I1): Next I1: Next I

    Write #1, DR0%
    Write #1, DR1%

    For I = 0 To 21: Write #1, draftTR(I): Next I

    For I = 0 To 22: Write #1, draftTR1(I): Next I

    Close #1

    Open pathRoadStats$ + statFile$ + "R" For Output As #1

    For I = 0 To 22
        For I1 = 0 To 6
            Print #1, S0N%(I, I1)
        Next
    Next

    Close 1

    Cls

    Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"
    Color 12
    Locate 12, 20
    Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub
