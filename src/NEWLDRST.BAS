'----------------------------------------
'           NEWLDRST Subroutine
'----------------------------------------
'This subroutine presents the user options
'for navigating the "Expanded" standings
Sub ExpandedStandings ()

    _Title "Full Count Baseball - Expanded Leaders / Standings"

    Shared cLoseStreak(), cWinStreak()
    Shared lastTenLosses(), lastTenWins()
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak()
    Shared teamTotLosses(), teamTotWins()
    Shared totExtInnLosses(), totExtInnWins()
    Shared totHomeLosses!(), totHomeWinss!()
    Shared totRoadLosses(), totRoadWins()

    Shared expStdLeagName$(), memberName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Erase cLoseStreak, cWinStreak
    Erase lastLoseStreak, lastWinStreak
    Erase lastTenLosses, lastTenWins
    Erase teamLoseStreak, teamWinStreak
    Erase teamTotLosses, teamTotWins
    Erase totExtInnLosses, totExtInnWins
    Erase totHomeLosses!, totHomeWinss!
    Erase totRoadLosses, totRoadWins

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE EXPANDED LEAGUE STANDINGS      "
        menuChoices$(2) = "VIEW / PRINT EXPANDED LEAGUE STANDINGS"
        menuChoices$(3) = "RETURN TO MAIN MENU                   "
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
                    Do
                        leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Color 15, 0
                            Cls
                            Call UpdateExpandedStandings(leagStatFile$)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 2:
                    Do
                        standingsFile$ = ChooseFile$ (diskPaths$(2) + "*.STD", "LEAGUE STANDINGS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then

                            Open diskPaths$(2) + standingsFile$ + ".STD" For Input As #1

                            Input #1, league$, div1Name$, div2Name$, div3Name$, div4Name$

                            For I = 1 To TEAMS_PER_LEAGUE
                                Input #1, expStdLeagName$(I), teamTotWins(I), totHomeWinss!(I), totRoadWins(I), totExtInnWins(I), lastTenWins(I), teamTotLosses(I), totHomeLosses!(I), totRoadLosses(I), totExtInnLosses(I), lastTenLosses(I), teamWinStreak(I), teamLoseStreak(I), cWinStreak(I), cLoseStreak(I)
                            Next

                            Close #1

                            userKey$ = GetOutputOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                                        If userFile$ <> "" Then
                                            Call NewStandings_Report(0, userFile$, league$)
                                        End If

                                    Case "P"
                                        Call NewStandings_Report(1, "REPORT", league$)

                                    Case "S":
                                        Call NewStandings_Screen(league$)

                                End Select
                                
                            End If

                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select
        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'    UpdateExpandedStandings Subroutine
'----------------------------------------
'This subroutine lets the user select a
'league (.TMS file) and reads in the stats 
'for that league. It then calculates the
'standings for that league and writes a 
'corresponding .STD file.
Sub UpdateExpandedStandings (league$)

    Shared cLoseStreak(), cWinStreak()
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak(), teamTotWins()
    Shared teamTotLosses(), lastTenWins(), lastTenLosses()
    Shared totExtInnLosses(), totExtInnWins() 
    Shared totHomeLosses!(), totHomeWinss!()
    Shared totRoadLosses(), totRoadWins()

    Shared expStdLeagName$(), memberName$()

    'These are just read in and written back
    Shared teamScore(), nbrInnings(), oppScore()
    Shared oppName$(), losePitcher$(), locIndicator$(), winPitcher$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    TMR = 40

    currRecord = 0

    For KK = 1 To TEAMS_PER_LEAGUE

        currRecord = currRecord + 1
        Color 14
        Locate 7, 38
        TMR = TMR - 1: Print Using "###"; TMR

        If memberName$(KK) <> "*" Then

            expStdLeagName$(currRecord) = memberName$(KK)

            Color 15, 0
            Cls

            Locate 5: Print "COMPUTING RECORDS . . . . . PLEASE WAIT"

            If _FileExists(diskPaths$(1) + memberName$(KK) + "S") Then

                Call ReadScheduleStats (memberName$(KK), nbrGames)

                For I = 1 To nbrGames

                    If teamScore(I) > oppScore(I) Then

                        teamTotWins(currRecord) = teamTotWins(currRecord) + 1
                        lastWinStreak(currRecord) = lastWinStreak(currRecord) + 1
                        lastLoseStreak(currRecord) = 0
                        cWinStreak(currRecord) = cWinStreak(currRecord) + 1
                        cLoseStreak(currRecord) = 0

                        If nbrInnings(I) > 9 Then 
                            totExtInnWins(currRecord) = totExtInnWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "A" Then 
                            totRoadWins(currRecord) = totRoadWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then 
                            totHomeWinss!(currRecord) = totHomeWinss!(currRecord) + 1
                        End If

                    End If

                    If teamScore(I) < oppScore(I) Then 

                        teamTotLosses(currRecord) = teamTotLosses(currRecord) + 1
                        lastLoseStreak(currRecord) = lastLoseStreak(currRecord) + 1
                        lastWinStreak(currRecord) = 0
                        cLoseStreak(currRecord) = cLoseStreak(currRecord) + 1
                        cWinStreak(currRecord) = 0

                        If nbrInnings(I) > 9 Then 
                            totExtInnLosses(currRecord) = totExtInnLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "A" Then 
                            totRoadLosses(currRecord) = totRoadLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then 
                            totHomeLosses!(currRecord) = totHomeLosses!(currRecord) + 1
                        End If

                    End If

                    If lastWinStreak(currRecord) > teamWinStreak(currRecord) Then 
                        teamWinStreak(currRecord) = lastWinStreak(currRecord)
                    End If
                    If lastLoseStreak(currRecord) > teamLoseStreak(currRecord) Then 
                        teamLoseStreak(currRecord) = lastLoseStreak(currRecord)
                    End If

                    If teamScore(I) = oppScore(I) Then
                        cWinStreak(currRecord) = cWinStreak(currRecord) + 1
                        cLoseStreak(currRecord) = 0
                    End If

                Next I

                If nbrGames <= 10 Then
                    lastTenLosses(currRecord) = teamTotLosses(currRecord)
                    lastTenWins(currRecord) = teamTotWins(currRecord)
                Else
                    M = nbrGames - 9

                    For J = M To nbrGames
                        If teamScore(J) > oppScore(J) Then 
                            lastTenWins(currRecord) = lastTenWins(currRecord) + 1
                        End If
                        If oppScore(J) > teamScore(J) Then 
                            lastTenLosses(currRecord) = lastTenLosses(currRecord) + 1
                        End If
                    Next J
                End If

            End If

        End If 'Done checking for '*'

    Next KK

    Open diskPaths$(2) + league$ + ".STD" For Output As #1

    Write #1, league$, div1Name$, div2Name$, div3Name$, div4Name$

    For I = 1 To TEAMS_PER_LEAGUE
        Write #1, expStdLeagName$(I), teamTotWins(I), totHomeWinss!(I), totRoadWins(I), totExtInnWins(I), lastTenWins(I), teamTotLosses(I), totHomeLosses!(I), totRoadLosses(I), totExtInnLosses(I), lastTenLosses(I), teamWinStreak(I), teamLoseStreak(I), cWinStreak(I), cLoseStreak(I)
    Next

    Close #1

End Sub


'----------------------------------------
'       NewStandings_Screen Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the user
'display in a formatted view
Sub NewStandings_Screen (league$)

    Shared cLoseStreak(), cWinStreak()
    Shared lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak(), teamTotLosses(), teamTotWins()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWinss!()
    Shared totRoadLosses(), totRoadWins()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    For NT = 1 To 4

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Select Case NT

            Case 1
                targetDiv$ = div1Name$

            Case 2
                targetDiv$ = div2Name$

            Case 3
                targetDiv$ = div3Name$

            Case 4
                targetDiv$ = div4Name$

        End Select

        If NT > 1 And targetDiv$ <> "" Then

            Color 11
            Locate 2, 35: Print league$
            
            Color 14
            Locate 4: Print Tab(16); "                      MAG. LNG LNG C  C   HOME  ROAD  X-IN  L-10"

            Print RTrim$(targetDiv$)
            Print Tab(16); "G   W   L   PCT   GB   #   WS  LS  WS LS  W- L  W- L  W- L  W- L"

            For I = AA To ZZ

                If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                    Color 3
                    Print expStdLeagName$(I);
                    Color 15

                    'G / W / L
                    Print Tab(14); Using "###"; teamTotWins(I) + teamTotLosses(I); Tab(18);
                    Print Using "###"; teamTotWins(I); Tab(22);
                    Print Using "###"; teamTotLosses(I);

                    ' PCT
                    If teamTotLosses(I) = 0 And teamTotLosses(I) + teamTotWins(I) <> 0 Then
                        Print Tab(26); "1.000";
                    Else
                        If teamTotWins(I) = 0 Then
                            Print Tab(27); ".000";
                        Else
                            If teamTotWins(I) + teamTotLosses(I) <> 0 Then
                                winPCT! = teamTotWins(I) /  (teamTotWins(I) + teamTotLosses(I))
                                Print Tab(27); Using ".###"; winPCT!;
                            Else
                                If teamTotWins(I) + teamTotLosses(I) = 0 Then Print Tab(27); ".000";
                            End If
                        End If
                    End If

                    ' GB
                    calcGB! = ( (teamTotWins(AA) - teamTotWins(I)) + (teamTotLosses(I) - teamTotLosses(AA)) ) / 2
                    If calcGB! = 0 Then
                        Print Tab(32); " ---";
                    Else
                        Print Tab(32); Using "##.#"; calcGB!;
                    End If

                    Print Tab(43); Using "##"; teamWinStreak(I); Tab(46); teamLoseStreak(I); Tab(50); cWinStreak(I); Tab(53); cLoseStreak(I);
                    Print Tab(57); Using "##"; totHomeWinss!(I);: Print Tab(60); Using "##"; totHomeLosses!(I);
                    Print Tab(63); Using "##"; totRoadWins(I);: Print Tab(66); Using "##"; totRoadLosses(I);
                    Print Tab(69); Using "##"; totExtInnWins(I);: Print Tab(72); Using "##"; totExtInnLosses(I);
                    Print Tab(75); Using "##"; lastTenWins(I);: Print Tab(78); Using "##"; lastTenLosses(I)

                End If

            Next

            'MAG #
            MJ = 163 - teamTotWins(AA) - teamTotLosses(AA + 1)
            If MJ < 0 Then MJ = 0

            Locate 6, 38
            Print Using "###"; MJ;

            Color 14
            Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
            
        End If
        
    Next NT

End Sub

'----------------------------------------
'       NewStandings_Report Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub NewStandings_Report (printDest, destFile$, league$)

    Shared cLoseStreak(), cWinStreak()
    Shared lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWinss!()
    Shared teamLoseStreak(), teamWinStreak(), teamTotLosses(), teamTotWins()
    Shared totRoadLosses(), totRoadWins()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, Tab(35); league$

    For NT = 1 To 4

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Select Case NT

            Case 1
                targetDiv$ = div1Name$

            Case 2
                targetDiv$ = div2Name$

            Case 3
                targetDiv$ = div3Name$

            Case 4
                targetDiv$ = div4Name$

        End Select

        If NT > 1 And targetDiv$ <> "" Then

            Print #1, Tab(35); league$
            Print #1, Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"

            Print #1, RTrim$(targetDiv$);
            Print #1, Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"
            Print #1, String$(80,45)

            For I = AA To ZZ

                If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                    Print #1, expStdLeagName$(I);
                    Print #1, Tab(14); Using "###"; teamTotWins(I) + teamTotLosses(I); Tab(18);
                    Print #1, Using "###"; teamTotWins(I); Tab(22);
                    Print #1, Using "###"; teamTotLosses(I);

                    'PCT
                    If teamTotLosses(I) = 0 And teamTotLosses(I) + teamTotWins(I) <> 0 Then
                        Print #1, Tab(26); "1.000";
                    Else
                        If teamTotWins(I) = 0 Then
                            Print #1, Tab(27); ".000";
                        Else
                            If teamTotWins(I) + teamTotLosses(I) = 0 Then
                                Print #1, Tab(27); ".000";
                            Else
                                winPCT! = teamTotWins(I) /  (teamTotWins(I) + teamTotLosses(I))
                                Print #1, Tab(27); Using ".###"; winPCT!;
                            End If
                        End If
                    End If

                    'GB
                    If ((teamTotWins(AA) - teamTotWins(I)) + (teamTotLosses(I) - teamTotLosses(AA))) / 2 = 0 Then
                        Print #1, Tab(32); " ---";
                    Else
                        Print #1, Tab(32); Using "##.#"; ((teamTotWins(NT * 10 - 9) - teamTotWins(I)) + (teamTotLosses(I) - teamTotLosses(NT * 10 - 9))) / 2;
                    End If

                    If I = AA Then
                        MJ = 163 - teamTotWins(AA) - teamTotLosses(AA + 1)
                        If MJ < 0 Then MJ = 0
                        Print #1, Tab(38); Using "###"; MJ
                    End If

                    Print #1, Tab(44); Using "##"; teamWinStreak(I); Tab(47); teamLoseStreak(I); Tab(51); cWinStreak(I); Tab(54); cLoseStreak(I);
                    Print #1, Tab(57); Using "##"; totHomeWinss!(I);: Print #1, Tab(60); Using "##"; totHomeLosses!(I);
                    Print #1, Tab(63); Using "##"; totRoadWins(I);: Print #1, Tab(66); Using "##"; totRoadLosses(I);
                    Print #1, Tab(69); Using "##"; totExtInnWins(I);: Print #1, Tab(72); Using "##"; totExtInnLosses(I);
                    Print #1, Tab(75); Using "##"; lastTenWins(I);: Print #1, Tab(78); Using "##"; lastTenLosses(I)

                End If

            Next I

            Print #1,
            Print #1,

        End If

    Next NT

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'         SortStandings Subroutine
'----------------------------------------
'This subroutine simply sorts the league
'standings records so that teams that are
'leading can be displayed first.
'This sorting is only done on the arrays
'holding the standing data, so no changes
'are permanent to the underlying files.
Sub SortStandings (AA, ZZ)

    Shared cLoseStreak(), cWinStreak()
    Shared lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWinss!()
    Shared totRoadLosses(), totRoadWins()
    Shared teamLoseStreak(), teamWinStreak(), teamTotLosses(),teamTotWins() 
    
    Shared expStdLeagName$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = teamTotWins(X) - teamTotLosses(X)
            diffY = teamTotWins(Y) - teamTotLosses(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (teamTotWins(X) <= teamTotWins(Y) And teamTotWins(X) > teamTotLosses(X)) Or Not (teamTotWins(X) >= teamTotWins(Y) And teamTotWins(X) < teamTotLosses(X)) Then

                        Swap expStdLeagName$(X), expStdLeagName$(Y)
                        Swap teamWinStreak(X), teamWinStreak(Y)
                        Swap teamLoseStreak(X), teamLoseStreak(Y)
                        Swap cWinStreak(X), cWinStreak(Y)
                        Swap cLoseStreak(X), cLoseStreak(Y)
                        Swap teamTotWins(X), teamTotWins(Y)
                        Swap teamTotLosses(X), teamTotLosses(Y)
                        Swap totExtInnWins(X), totExtInnWins(Y)
                        Swap totExtInnLosses(X), totExtInnLosses(Y)
                        Swap totHomeLosses!(X), totHomeLosses!(Y)
                        Swap totRoadLosses(X), totRoadLosses(Y)
                        Swap totHomeWinss!(X), totHomeWinss!(Y)
                        Swap totRoadWins(X), totRoadWins(Y)
                        Swap lastTenLosses(X), lastTenLosses(Y)
                        Swap lastTenWins(X), lastTenWins(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub

