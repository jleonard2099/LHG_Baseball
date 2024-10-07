'----------------------------------------
'            ALIGN Subroutine
'----------------------------------------
'This routine presents a menu / interface
'that assists the user with aligning 
'player names in separate stat files. 
'(ie, Road Vs. Local) This should be done 
'before merging stats, since player name 
'entry may not be consistent.
Sub ALIGN ()

    _Title "Full Count Baseball - Align Stats"

    Shared statsD0, statsD1
    Shared statsD0_Road, statsD1_Road

    Shared teamLosses_Road, teamWins_Road
    Shared teamName_Align$

    Shared pitcherNames$(), pitNames_Road$()
    Shared batterNames$(), batNames_Road$()
    
    Shared alignTR(), alignTR1()

    Shared alignB0_Road(), alignB_Road()
    Shared alignP0_Road(), alignP_Road()
    Shared alignS0_Road()

    Shared alignS() 
    Shared statB0(), statB()
    Shared statP(), statP0()
    Shared statT0!(), statT1!()

    Do

        Call GetMergePaths (pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)
            Color 11

            teamNbr = 0
            Call ChooseTeam (YN$, NTMS, teamNbr, team$)

            statFile$ = nameStatFile$(team$)

            Color 12, 0
            Cls

            Locate 18, 5: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1

                Input #1, teamNameRoad$
                Input #1, teamWins_Road
                Input #1, teamLosses_Road

                For I = 0 To 22:
                    Input #1, batNames_Road$(I)
                    Input #1, alignB_Road(I)
                Next I

                For I = 0 To 21:
                    Input #1, pitNames_Road$(I)
                    Input #1, alignP_Road(I)
                Next I

                For I = 0 To 22:
                    For I1 = 0 To 21:
                        Input #1, alignB0_Road(I, I1)
                    Next I1
                Next I

                For I = 0 To 21:
                    For I1 = 0 To 41:
                        Input #1, alignP0_Road(I, I1)
                    Next I1
                Next I

                Input #1, statsD0_Road
                Input #1, statsD1_Road

                For I = 0 To 21: 
                    Input #1, alignTR(I)
                Next I
                
                For I = 0 To 22: 
                    Input #1, alignTR1(I)
                Next I

                Close #1

                If _FileExists(pathRoadStats$ + statFile$ + "R") Then

                    Open pathRoadStats$ + statFile$ + "R" For Input As #1

                    For I = 0 To 22
                        For I1 = 0 To 6
                            Input #1, alignS0_Road(I, I1)
                        Next
                    Next

                    Close #1

                End If

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Open pathCurrentStats$ + statFile$ For Input As #1

                    Input #1, teamNameCurrent$
                    Input #1, teamWins
                    Input #1, teamLosses

                    For I = 0 To 22:
                        Input #1, batterNames$(I)
                        Input #1, statB(I)
                    Next I

                    For I = 0 To 21:
                        Input #1, pitcherNames$(I)
                        Input #1, statP(I)
                    Next I

                    For I = 0 To 22:
                        For I1 = 0 To 21:
                            Input #1, statB0(I, I1)
                        Next I1
                    Next I

                    For I = 0 To 21:
                        For I1 = 0 To 41:
                            Input #1, statP0(I, I1)
                        Next I1
                    Next I

                    Input #1, statsD0
                    Input #1, statsD1

                    For I = 0 To 21: 
                        Input #1, statT0!(I)
                    Next I

                    For I = 0 To 22: 
                        Input #1, statT1!(I)
                    Next I

                    Close #1

                    If _FileExists(pathCurrentStats$ + statFile$ + "R") Then

                        Open pathCurrentStats$ + statFile$ + "R" For Input As #1

                        For I = 0 To 22
                            For I1 = 0 To 6
                                Input #1, alignS(I, I1)
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

    Shared batNames_Align$(), pitName_Align$()
    Shared alignB0(), alignB()
    Shared alignP0(), alignP()
    Shared alignS0()

    Shared batNames_Road$(), pitNames_Road$()
    Shared alignP_Road()
    Shared alignB0_Road(), alignB_Road()
    Shared alignP0_Road(), alignS0_Road()

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
        Color 7: Print Left$(batNames_Road$(I), 10);
    Next

    For I = 0 To 21:
        Color 12
        Locate I + 1, 48: Print Using "##"; I;
        Print " ";
        Color 15: Print Left$(pitcherNames$(I), 10);
        Color 12: Print Tab(62); Using "##"; I;
        Print " ";
        Color 7: Print Left$(pitNames_Road$(I), 10);
    Next

    Color 11
    Locate 24, 31: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 24, 30: Print "ALIGNING FILES . . ."

    N = 0

    Do

        For J = 0 To 22

            If UCase$(batterNames$(N)) = UCase$(batNames_Road$(J)) Then

                batNames_Align$(N) = batNames_Road$(J)
                alignB(N) = alignB_Road(J)

                For K = 0 To 21
                    alignB0(N, K) = alignB0_Road(J, K)
                Next K

                For K = 0 To 6
                    alignS0(N, K) = alignS0_Road(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 23

    N = 0

    Do

        For J = 0 To 21

            If UCase$(pitcherNames$(N)) = UCase$(pitNames_Road$(J)) Then

                pitName_Align$(N) = pitNames_Road$(J)
                alignP(N) = alignP_Road(J)

                For K = 0 To 41
                    alignP0(N, K) = alignP0_Road(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 22

    SEQ = 0

    'Check batters
    For J = 0 To 22
        If UCase$(batNames_Align$(J)) <> UCase$(batterNames$(J)) Then
            SEQ = SEQ + 1
        End If
    Next J

    'Check pitchers
    For J = 0 To 21
        If UCase$(pitName_Align$(J)) <> UCase$(pitcherNames$(J)) Then
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

    Shared statsD0_Road, statsD1_Road
    Shared teamLosses_Road, teamWins_Road

    Shared teamNameRoad$

    Shared batterNames$(), pitcherNames$()
    Shared batNames_Align$(), pitName_Align$()

    Shared alignTR(), alignTR1()

    Shared alignB0(), alignB()
    Shared alignP0(), alignP()
    Shared alignS0()

    Cls

    Locate 1, 32
    Color 12: Print "STAT ROSTERS"

    Color 15
    Locate , 26: Print "CURRENT  ";
    Color 7: Print "             ROAD"

    For I = 0 To 22:
        Color 12: Locate I + 1, 1: Print Using "##"; I;
        Print " ";: Color 15: Print Left$(batterNames$(I), 10);
        Color 12: Print Tab(17); Using "##"; I;: Print " ";
        Color 7: Print Left$(batNames_Align$(I), 10);
    Next

    For I = 0 To 21:
        Color 12
        Locate I + 1, 48: Print Using "##"; I;
        Print " ";
        Color 15: Print Left$(pitcherNames$(I), 10);
        Color 12: Print Tab(62); Using "##"; I;
        Print " ";
        Color 7: Print Left$(pitName_Align$(I), 10);
    Next

    Color 11
    Locate 24, 31: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 24, 30: Print "ALIGNING FILES . . ."

    Open pathRoadStats$ + statFile$ For Output As #1

    Write #1, teamNameRoad$
    Write #1, teamWins_Road
    Write #1, teamLosses_Road

    For I = 0 To 22: 
        Write #1, batNames_Align$(I)
        Write #1, alignB(I)
    Next I

    For I = 0 To 21: 
        Write #1, pitName_Align$(I)
        Write #1, alignP(I)
    Next I

    For I = 0 To 22: 
        For I1 = 0 To 21: 
            Write #1, alignB0(I, I1)
        Next I1
    Next I

    For I = 0 To 21: 
        For I1 = 0 To 41: 
            Write #1, alignP0(I, I1)
        Next I1
    Next I

    Write #1, statsD0_Road
    Write #1, statsD1_Road

    For I = 0 To 21: 
        Write #1, alignTR(I)
    Next I

    For I = 0 To 22: 
        Write #1, alignTR1(I)
    Next I

    Close #1

    Open pathRoadStats$ + statFile$ + "R" For Output As #1

    For I = 0 To 22
        For I1 = 0 To 6
            Print #1, alignS0(I, I1)
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
