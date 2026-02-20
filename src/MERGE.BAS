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
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
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
                    Call MergeStats

                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

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

    Shared statTeamDP, statOppDP
    Shared statTeamDP_Road, statOppDP_Road

    Shared teamLosses, teamLosses_Road
    Shared teamWins, teamWins_Road

    Shared batterNames$(), batNames_Road$()
    Shared pitcherNames$(), pitNames_Road$()

    Shared oppName$(), oppName_Road$()
    Shared losePitcher$(), losePitcher_Road$()
    Shared locIndicator$(), locIndicator_Road$()
    Shared winPitcher$(), winPitcher_Road$()

    Shared alignTR(), alignTR1()

    Shared alignB_Road(), alignP_Road()

    Shared statAtBats(), statBatter(), alignB0_Road()
    Shared statInnPitch(), statPitcher(), alignP0_Road()
    Shared alignS(), alignS_Road()
    Shared statTeamBat!(), statTeamPit!()

    Shared oppScore(), oppScore_Road()
    Shared teamScore(), teamScore_Road(), nbrInnings(), nbrInnings_Road()


    Do

        'Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)
        pathCurrentStats$ = diskPaths$(1)
        pathRoadStats$ = _SelectFolderDialog$("Select folder with ROAD stats", diskPaths$(1))

        'If UCase$(I$) = "N" And I$ <> Chr$(27) Then
        If pathRoadStats$ <> "" Then

            pathRoadStats$ =  pathRoadStats$ + "\"

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            Color 15, 0
            Cls

            Color 14
            Locate 8, 15: Print "EXISTING STATS ARE IN: "
            Color 15
            Print Tab(15); pathRoadStats$
            Color 14
            Locate 11, 15: Print "ROAD STATS ARE LOCATED AT:"
            Color 15
            Print Tab(15); pathCurrentStats$
            Color 14
            Locate 14, 15: Print "TEAM TO MERGE STATS FOR IS: "
            Color 15
            Print Tab(15); team$

            Do
                Locate 17, 15: Print "ANY CHANGE? "
                Color 15: Print Tab(15); "(Y/N)"
                J$ = GetKeyPress$
            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

            If UCase$(J$) = "N" Then

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

                    Input #1, statTeamDP_Road
                    Input #1, statOppDP_Road

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
                                Input #1, alignS_Road(I, I1)
                            Next
                        Next

                        Close 1

                    End If

                    Open pathRoadStats$ + statFile$ + "S" For Input As #1

                    Input #1, nbrGamesRoad

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, teamScore_Road(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, oppName_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, oppScore_Road(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, locIndicator_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, winPitcher_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, losePitcher_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #1, nbrInnings_Road(I)
                    Next I

                    Close #1

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #1

                        Input #1, teamNameStats$
                        Input #1, teamWins
                        Input #1, teamLosses

                        For I = 0 To 22: 
                            Input #1, batterNames$(I)
                            Input #1, statAtBats(I)
                        Next I
                        
                        For I = 0 To 21: 
                            Input #1, pitcherNames$(I)
                            Input #1, statInnPitch(I)
                        Next I

                        For I = 0 To 22: 
                            For I1 = 0 To 21: 
                                Input #1, statBatter(I, I1)
                            Next I1
                        Next I
                        
                        For I = 0 To 21: 
                            For I1 = 0 To 41: 
                                Input #1, statPitcher(I, I1)
                            Next I1
                        Next I

                        Input #1, statTeamDP
                        Input #1, statOppDP
                        
                        For I = 0 To 21: 
                            Input #1, statTeamBat!(I)
                        Next I
                        
                        For I = 0 To 22: 
                            Input #1, statTeamPit!(I)
                        Next I

                        Close #1

                        If _FileExists(pathCurrentStats$ + statFile$ + "R") Then

                            Open pathCurrentStats$ + statFile$ + "R" For Input As #1

                            For I = 0 To 22
                                For I1 = 0 To 6: 
                                    Input #1, alignS(I, I1)
                                Next
                            Next

                            Close 1

                        End If

                        Open pathCurrentStats$ + statFile$ + "S" For Input As #4

                        Input #4, nbrGamesCurr

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, teamScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, oppName$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, oppScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, locIndicator$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, winPitcher$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, losePitcher$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, nbrInnings(I)
                        Next I

                        Close #4

                        SEQ = 0

                        For K = 0 To 22
                            If UCase$(batNames_Road$(K)) <> UCase$(batterNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 21
                            If UCase$(pitNames_Road$(K)) <> UCase$(pitcherNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, teamNameStats$, nbrGamesCurr, nbrGamesRoad)

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

                        badFile = 1

                    End If 'Done checking if home file exists

                Else

                    badFile = 1

                End If 'Done checking if road file exists

                If badFile = 1 Then

                    Cls
                    Color 7
                    Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"

                    Do
                        Locate , 10
                        Print "TRY AGAIN? (Y/N) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
                End If

            End If 'Done checking if ready to proceed

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or (UCase$(J$) = "N" And badFile = 0) Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       DoTheMerge Subroutine
'----------------------------------------
'This subroutine literally adds the stat
'values together from each file and then
'overwrites the current file with the
'newly aggregated results.
Sub DoTheMerge (pathCurrentStats$, statFile$, teamNameStats$, nbrGamesCurr, nbrGamesRoad)

    Shared statTeamDP, statOppDP
    Shared statTeamDP_Road, statOppDP_Road

    Shared teamLosses, teamLosses_Road
    Shared teamWins, teamWins_Road

    Shared batterNames$(), pitcherNames$()
    Shared oppName$(), oppName_Road$()
    Shared losePitcher$(), losePitcher_Road$()
    Shared locIndicator$(), locIndicator_Road$()
    Shared winPitcher$(), winPitcher_Road$()

    Shared statAtBats(), statBatter(), alignB0_Road()
    Shared teamScore(), teamScore_Road(), nbrInnings(), nbrInnings_Road()
    Shared statInnPitch(), statPitcher(), alignP0_Road()
    Shared statTeamBat!(), statTeamPit!()
    Shared alignS(), alignS_Road()
    Shared oppScore(), oppScore_Road()

    Shared alignTR(), alignTR1()

    For K = 1 to nbrGamesRoad
        nbrGamesCurr = nbrGamesCurr + 1
        teamScore(nbrGamesCurr) = teamScore_Road(K)
        oppName$(nbrGamesCurr) = oppName_Road$(K)
        oppScore(nbrGamesCurr) = oppScore_Road(K)
        locIndicator$(nbrGamesCurr) = locIndicator_Road$(K)
        winPitcher$(nbrGamesCurr) = winPitcher_Road$(K)
        losePitcher$(nbrGamesCurr) = losePitcher_Road$(K)
        nbrInnings(nbrGamesCurr) = nbrInnings_Road(K)
    Next

    teamWins = teamWins + teamWins_Road
    teamLosses = teamLosses + teamLosses_Road

    Color 12
    Locate 18, 20: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 22

        For I1 = 0 To 6
            alignS(I, I1) = alignS(I, I1) + alignS_Road(I, I1)
        Next

        For I1 = 0 To 11: 
            statBatter(I, I1) = statBatter(I, I1) + alignB0_Road(I, I1)
        Next I1
        
        For I1 = 14 To 21: 
            statBatter(I, I1) = statBatter(I, I1) + alignB0_Road(I, I1)
        Next I1

    Next I

    For I = 0 To 21

        For I1 = 0 To 34: 
            statPitcher(I, I1) = statPitcher(I, I1) + alignP0_Road(I, I1)
        Next I1

        For I1 = 37 To 41: 
            statPitcher(I, I1) = statPitcher(I, I1) + alignP0_Road(I, I1)
        Next I1

    Next I

    statTeamDP = statTeamDP + statTeamDP_Road
    statOppDP = statOppDP + statOppDP_Road

    For I = 0 To 21: 
        statTeamBat!(I) = statTeamBat!(I) + alignTR(I)
    Next

    For I = 0 To 22: 
        statTeamPit!(I) = statTeamPit!(I) + alignTR1(I)
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1

    Write #1, teamNameStats$
    Write #1, teamWins
    Write #1, teamLosses

    For I = 0 To 22: 
        Write #1, batterNames$(I)
        Write #1, statAtBats(I)
    Next I
    
    For I = 0 To 21: 
        Write #1, pitcherNames$(I)
        Write #1, statInnPitch(I)
    Next I

    For I = 0 To 22: 
        For I1 = 0 To 21: 
            Write #1, statBatter(I, I1)
        Next I1
    Next I
    
    For I = 0 To 21: 
        For I1 = 0 To 41: 
            Write #1, statPitcher(I, I1)
        Next I1
    Next I

    Write #1, statTeamDP
    Write #1, statOppDP

    For I = 0 To 21: 
        Write #1, statTeamBat!(I)
    Next I

    For I = 0 To 22: 
        Write #1, statTeamPit!(I)
    Next I

    Close #1

    Open pathCurrentStats$ + statFile$ + "R" For Output As #1

    For I = 0 To 22
        For I1 = 0 To 6
            Write #1, alignS(I, I1)
        Next
    Next

    Close #1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #1

    Write #1, nbrGamesCurr

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, teamScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, oppName$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, oppScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, locIndicator$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, winPitcher$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, losePitcher$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #1, nbrInnings(I)
    Next I

    Close #1

    MsgText$ = "SUCCESSFULLY MERGED ALL STAT FILES!! PRESS ANY KEY TO RETURN TO MENU"
    BoxWidth = 20
    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
    Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
    Call CSMsgBox(MsgText$, BoxWidth, PASS)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, PASS)

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
