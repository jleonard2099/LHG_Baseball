'----------------------------------------
'            MERGE Subroutine
'----------------------------------------
'This subroutine gives the user options
'for aligning / merging stat files
Sub MERGE ()

    _Title "Full Count Baseball - Merge Stats"

    ReDim menuChoices$(1 To 3)

    Do

        Color , 0
        Cls

        Call TITLE_MERGE

        menuChoices$(1) = "MERGE STATISTICAL FILES"
        menuChoices$(2) = "ALIGN STATISTICAL FILES"
        menuChoices$(3) = "EXIT TO MAIN MENU      "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: 
                    Call MergeStats

                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'          MergeStats Subroutine
'----------------------------------------
'This subroutine determines the parameters
'for the road and existing stat files that
'can be merged. It then reads in all stat
'files, checks for player sequence, and then
'calls a supporting routine to actually
'perform the merge
Sub MergeStats ()

    Dim BR$(22)

	Shared nbrGamesCurr, nbrGamesRoad

    Shared BR%(), PR%()

    Shared batterNames$(), pitcherNames$()
    Shared oppName$(), HR$()
    Shared losePitcher$(), LR$()
    Shared locIndicator$(), PLR$(), PR$()
    Shared winPitcher$(), WR$()

    Shared statB%(), statB0%(), B0R%()
    Shared teamScore(), HR%(), nbrInnings(), IR%()
    Shared statP%(), statP0%(), P0R%()
    Shared S0%(), SR%()
    Shared statT0%(), statT1%()
    Shared oppScore(), VR%()

    Shared draftTR(), draftTR1()

    Do

        Call GetMergePaths (pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

			Color , 0
            Cls

            Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; pathRoadStats$
            Locate , 15: Print "MASTER STATS ARE ON DRIVE "; pathCurrentStats$
            Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; team$

            Do
                Locate 14, 15: Print "ANY CHANGE? (Y/N)"
                J$ = GetKeyPress$
            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

            If UCase$(J$) = "N" Then

                If _FileExists(pathRoadStats$ + statFile$) Then

                    Open pathRoadStats$ + statFile$ For Input As #1

                    Input #1, team$
                    Input #1, WR2%
                    Input #1, LR2%

                    For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I
                    For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I
                    For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I
                    For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

                    Input #1, D0R%: Input #1, D1R%
                    For I = 0 To 21: Input #1, draftTR(I): Next I
                    For I = 0 To 22: Input #1, draftTR1(I): Next I

                    Close #1

                    If _FileExists(pathRoadStats$ + statFile$ + "R") Then

                        Open pathRoadStats$ + statFile$ + "R" For Input As #1

                        For I = 0 To 22
                            For I1 = 0 To 6
                                Input #1, SR%(I, I1)
                            Next
                        Next

                        Close 1

                    End If

                    Open pathRoadStats$ + statFile$ + "S" For Input As #1

                    Input #1, nbrGamesRoad

                    For I = 1 To MAX_SCHED_STATS: Input #1, HR%(I): Next I
                    For I = 1 To MAX_SCHED_STATS: Input #1, HR$(I): Next I
                    For I = 1 To MAX_SCHED_STATS: Input #1, VR%(I): Next I
                    For I = 1 To MAX_SCHED_STATS: Input #1, PLR$(I): Next I
                    For I = 1 To MAX_SCHED_STATS: Input #1, WR$(I): Next I
                    For I = 1 To MAX_SCHED_STATS: Input #1, LR$(I): Next I
                    For I = 1 To MAX_SCHED_STATS: Input #1, IR%(I): Next I

                    Close #1

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #1

                        Input #1, team$
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
                                For I1 = 0 To 6: Input #1, S0%(I, I1): Next
							Next

                            Close 1

                        End If

                        Open pathCurrentStats$ + statFile$ + "S" For Input As #1

                        Input #1, nbrGamesCurr

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, teamScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, oppName$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, oppScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, locIndicator$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, winPitcher$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, losePitcher$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #1, nbrInnings(I)
                        Next I

                        Close #1

                        SEQ = 0

                        For K = 0 To 22
                            If UCase$(BR$(K)) <> UCase$(batterNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 21
                            If UCase$(PR$(K)) <> UCase$(pitcherNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, B$)

                            For J = 13 To 23
                                Locate J
                                Print Space$(65)
                            Next

                        Else

                            Cls
                            Color 14
                            Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"
                            Color 15
                            Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                            Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                            Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                            Color 12
                            Locate 11, 24: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                            Locate , 31: Print "ENTER STATS MANUALLY."
                            Color 13
                            Locate 15, 29: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                        End If 'Done checking sequence

                    Else

                        Cls

                        Do
                            Color 7
                            Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                            Print
                            Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
                            Locate , 10
                            Print "TRY AGAIN? (Y/N) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "Y" Then
                            Print
                            Print "HIT ENTER TO RE-ENTER INFORMATION"
                            I$ = GetKeyPress$
                        End If

                    End If 'Done check if home file exists

                Else

                    Cls
                    Locate 2, 10
                    Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    Do
                        Locate , 10
                        Print "TRY AGAIN? (Y/N) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        Print
                        Print "HIT ENTER TO RE-ENTER INFORMATION"
                        I$ = GetKeyPress$
                    End If

                End If 'Done checking if road file exists

            End If 'Done checking if ready to proceed

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       DoTheMerge Subroutine
'----------------------------------------
'This subroutine literally adds the stat
'values together from each file and then
'overwrites the current file with the
'newly aggregated results.
Sub DoTheMerge (pathCurrentStats$, statFile$, B$)

	Shared nbrGamesCurr, nbrGamesRoad
	Shared L2%, LR2%
    Shared W2%, WR2%

    Shared batterNames$(), pitcherNames$()
    Shared oppName$(), HR$()
    Shared losePitcher$(), LR$()
    Shared locIndicator$(), PLR$()
    Shared winPitcher$(), WR$()

    Shared statB%(), statB0%(), B0R%()
    Shared teamScore(), HR%(), nbrInnings(), IR%()
    Shared statP%(), statP0%(), P0R%()
    Shared statT0%(), statT1%()
    Shared S0%(), SR%()
    Shared oppScore(), VR%()

    Shared draftTR(), draftTR1()

    For K = 1 to nbrGamesRoad
        nbrGamesCurr = nbrGamesCurr + 1
        teamScore(nbrGamesCurr) = HR%(K)
        oppName$(nbrGamesCurr) = HR$(K)
        oppScore(nbrGamesCurr) = VR%(K)
        locIndicator$(nbrGamesCurr) = PLR$(K)
        winPitcher$(nbrGamesCurr) = WR$(K)
        losePitcher$(nbrGamesCurr) = LR$(K)
        nbrInnings(nbrGamesCurr) = IR%(K)
    Next

    W2% = W2% + WR2%
	L2% = L2% + LR2%

    Color 12
    Locate 18, 20: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 22
        For I1 = 0 To 6
            S0%(I, I1) = S0%(I, I1) + SR%(I, I1)
        Next

        For I1 = 0 To 11: statB0%(I, I1) = statB0%(I, I1) + B0R%(I, I1): Next I1
        For I1 = 14 To 21: statB0%(I, I1) = statB0%(I, I1) + B0R%(I, I1): Next I1
    Next I

    For I = 0 To 21
        For I1 = 0 To 34: statP0%(I, I1) = statP0%(I, I1) + P0R%(I, I1): Next I1
        For I1 = 37 To 41: statP0%(I, I1) = statP0%(I, I1) + P0R%(I, I1): Next I1
    Next I

    D0% = D0% + DR0%
	D1% = D1% + DR1%

    For I = 0 To 21: statT0%(I) = statT0%(I) + draftTR(I): Next
    For I = 0 To 22: statT1%(I) = statT1%(I) + draftTR1(I): Next

    Open pathCurrentStats$ + statFile$ For Output As #1
    Write #1, B$
	Write #1, W2%
	Write #1, L2%

    For I = 0 To 22: 
		Write #1, batterNames$(I)
		Write #1, statB%(I)
	Next I
    
	For I = 0 To 21: 
		Write #1, pitcherNames$(I)
		Write #1, statP%(I)
	Next I

    For I = 0 To 22: 
		For I1 = 0 To 21: Write #1, statB0%(I, I1): Next I1
	Next I
    
	For I = 0 To 21: 
		For I1 = 0 To 41: Write #1, statP0%(I, I1): Next I1
	Next I

    Write #1, D0%
	Write #1, D1%

    For I = 0 To 21: Write #1, statT0%(I): Next I
    For I = 0 To 22: Write #1, statT1%(I): Next I

    Close #1

    Open pathCurrentStats$ + statFile$ + "R" For Output As #1
    For I = 0 To 22
        For I1 = 0 To 6
            Write #1, S0%(I, I1)
        Next
    Next
    Close 1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #1

    Write #1, nbrGamesCurr

    For I = 1 To MAX_SCHED_STATS: Write #1, teamScore(I): Next I

    For I = 1 To MAX_SCHED_STATS: Write #1, oppName$(I): Next I

    For I = 1 To MAX_SCHED_STATS: Write #1, oppScore(I): Next I

    For I = 1 To MAX_SCHED_STATS: Write #1, locIndicator$(I): Next I

    For I = 1 To MAX_SCHED_STATS: Write #1, winPitcher$(I): Next I

    For I = 1 To MAX_SCHED_STATS: Write #1, losePitcher$(I): Next I

    For I = 1 To MAX_SCHED_STATS: Write #1, nbrInnings(I): Next I

    Close #1

    Y = 0

    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

End Sub


'----------------------------------------
'       GetMergePaths Subroutine
'----------------------------------------
'This subroutine assists the user with the
'entry of the separate paths to be used 
'for the merge / align process.
Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)

    Shared opSysType$

    Do
        Color 12, 0
        Cls

        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE/ALIGN"
        Color 15
        Select Case opSysType$
            Case "[WINDOWS]":
                Locate 4, 5: Print "EXAMPLE - A:"
                Locate , 5: Print "          C:\HAFFNER"
                Locate , 5: Print "          C:\HAFFNER\1960"
                Locate , 5: Print "          D:\FOOTBALL"
            Case "[LINUX]":
                Locate 4, 5: Print "EXAMPLE - /mnt/floppy"
                Locate , 5: Print "           /usr/bin/haffner"
                Locate , 5: Print "           /usr/bin/haffner/1960"
                Locate , 5: Print "           /home/haffner/football"
            Case "[MAC]","[MACOSX]":
                Locate 4, 5: Print "EXAMPLE - /Volumes/floppy"
                Locate , 5: Print "           /Applications/haffner"
                Locate , 5: Print "           /Applications/haffner/1960"
                Locate , 5: Print "           /Users/haffner/football"
        End Select

        For J = 10 To 13
            Locate J
            Print Space$(70)
        Next

        Color 10
        Locate 10, 5
        Line Input "ROAD STATS:     "; pathRoadStats$
        Locate , 5
        Locate , 5
        Line Input "CURRENT STATS:  "; pathCurrentStats$

        Color 14
        Locate 13, 5: Print "ANY CHANGE? (Y/N)"
        promptResponse$ = GetKeyPress$

    Loop Until UCase$(promptResponse$) = "N" Or promptResponse$ = Chr$(27)

    'Adjust path terminator for OS type
    Select Case opSysType$
        Case "[WINDOWS]":
            If Not (Right$(pathCurrentStats$, 1) = "\") Then
                pathCurrentStats$ = pathCurrentStats$ + "\"
            End If

            If Not (Right$(pathRoadStats$, 1) = "\") Then
                pathRoadStats$ = pathRoadStats$ + "\"
            End If

        Case "[MAC]", "[MACOSX]", "[LINUX]":
            If Not (Right$(pathCurrentStats$, 1) = "/") Then
                pathCurrentStats$ = pathCurrentStats$ + "/"
            End If

            If Not (Right$(pathRoadStats$, 1) = "/") Then
                pathRoadStats$ = pathRoadStats$ + "/"
            End If
    End Select

End Sub
