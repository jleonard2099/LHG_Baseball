'----------------------------------------
'    UpdateExpandedStandings Subroutine
'----------------------------------------
'This subroutine lets the user select a
'league (.TMS file) and reads in the stats
'for that league. It then calculates the
'standings for that league and writes a
'corresponding .STD file.
Sub UpdateExpandedStandings (league$)

    Shared currLoseStreak(), currWinStreak()
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak(), teamTotWins()
    Shared teamTotLosses(), lastTenWins(), lastTenLosses()
    Shared totExtInnLosses(), totExtInnWins()
    Shared totHomeLosses!(), totHomeWins!()
    Shared totRoadLosses(), totRoadWins()

    Shared teamScore(), nbrInnings(), oppScore()

    Shared expStdLeagName$(), memberName$()
    Shared losePitcher$(), locIndicator$(), oppName$(), winPitcher$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Erase currLoseStreak, currWinStreak
    Erase lastLoseStreak, lastWinStreak
    Erase teamLoseStreak, teamWinStreak, teamTotWins
    Erase teamTotLosses, lastTenWins, lastTenLosses
    Erase totExtInnLosses, totExtInnWins
    Erase totHomeLosses!, totHomeWins!
    Erase totRoadLosses, totRoadWins

    Erase expStdLeagName$
    Erase locIndicator$, oppName$

    Erase nbrInnings, teamScore, oppScore

    TMR = TEAMS_PER_LEAGUE

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

                Call ReadScheduleStats(memberName$(KK), nbrGames)

                For I = 1 To nbrGames

                    If teamScore(I) > oppScore(I) Then

                        teamTotWins(currRecord) = teamTotWins(currRecord) + 1
                        lastWinStreak(currRecord) = lastWinStreak(currRecord) + 1
                        lastLoseStreak(currRecord) = 0
                        currWinStreak(currRecord) = currWinStreak(currRecord) + 1
                        currLoseStreak(currRecord) = 0

                        If nbrInnings(I) > 9 Then
                            totExtInnWins(currRecord) = totExtInnWins(currRecord) + 1
                        End If
                        If (Left$(locIndicator$(I), 1) = "V" Or Left$(locIndicator$(I), 1) = "A") Then
                            totRoadWins(currRecord) = totRoadWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then
                            totHomeWins!(currRecord) = totHomeWins!(currRecord) + 1
                        End If

                    End If

                    If teamScore(I) < oppScore(I) Then

                        teamTotLosses(currRecord) = teamTotLosses(currRecord) + 1
                        lastLoseStreak(currRecord) = lastLoseStreak(currRecord) + 1
                        lastWinStreak(currRecord) = 0
                        currLoseStreak(currRecord) = currLoseStreak(currRecord) + 1
                        currWinStreak(currRecord) = 0

                        If nbrInnings(I) > 9 Then
                            totExtInnLosses(currRecord) = totExtInnLosses(currRecord) + 1
                        End If
                        If (Left$(locIndicator$(I), 1) = "V" Or Left$(locIndicator$(I), 1) = "A") Then
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
                        currWinStreak(currRecord) = currWinStreak(currRecord) + 1
                        currLoseStreak(currRecord) = 0
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
        Write #1, expStdLeagName$(I), teamTotWins(I), totHomeWins!(I), totRoadWins(I), totExtInnWins(I), lastTenWins(I), teamTotLosses(I), totHomeLosses!(I), totRoadLosses(I), totExtInnLosses(I), lastTenLosses(I), teamWinStreak(I), teamLoseStreak(I), currWinStreak(I), currLoseStreak(I)
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

    Shared currLoseStreak(), currWinStreak()
    Shared lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak(), teamTotLosses(), teamTotWins()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWins!()
    Shared totRoadLosses(), totRoadWins()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    For NT = 1 To 4

        AA = ((NT - 1) * TEAMS_PER_DIV) + 1
        ZZ = NT * TEAMS_PER_DIV

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


        If targetDiv$ <> "" Or (targetDiv$ = "" And expStdLeagName$(AA) <> "") Then

            Color 11
            Locate 2, 35: Print league$
            Print RTrim$(targetDiv$)

            Color 14
            Print Tab(38); "MAG  LNG LNG C  C   HOME  ROAD  X-IN  L-10"
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
                                winPCT! = teamTotWins(I) / (teamTotWins(I) + teamTotLosses(I))
                                Print Tab(27); Using ".###"; winPCT!;
                            Else
                                If teamTotWins(I) + teamTotLosses(I) = 0 Then Print Tab(27); ".000";
                            End If
                        End If
                    End If

                    ' GB
                    '((Leading Team Wins - Curr Team Wins) + (Leading Team Loss - Curr Team Loss)) / 2
                    calcGB! = ((teamTotWins(AA) - teamTotWins(I)) + (teamTotLosses(I) - teamTotLosses(AA))) / 2
                    If calcGB! = 0 Then
                        Print Tab(32); " ---";
                    Else
                        Print Tab(32); Using "##.#"; calcGB!;
                    End If

                    Print Tab(43); Using "##"; teamWinStreak(I); Tab(46); teamLoseStreak(I); Tab(50); currWinStreak(I); Tab(53); currLoseStreak(I);
                    Print Tab(57); Using "##"; totHomeWins!(I);: Print Tab(60); Using "##"; totHomeLosses!(I);
                    Print Tab(63); Using "##"; totRoadWins(I);: Print Tab(66); Using "##"; totRoadLosses(I);
                    Print Tab(69); Using "##"; totExtInnWins(I);: Print Tab(72); Using "##"; totExtInnLosses(I);
                    Print Tab(75); Using "##"; lastTenWins(I);: Print Tab(78); Using "##"; lastTenLosses(I)

                End If

            Next

            'MAG #
            MJ = 163 - teamTotWins(AA) - teamTotLosses(AA + 1)
            If MJ < 0 Then MJ = 0

            Color 15
            Locate 6, 38
            Print Using "###"; MJ;

            Color 14
            Locate LAST_ROW - 1, 28: Print "HIT ANY KEY TO CONTINUE";
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

    Shared currLoseStreak(), currWinStreak()
    Shared lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWins!()
    Shared teamLoseStreak(), teamWinStreak(), teamTotLosses(), teamTotWins()
    Shared totRoadLosses(), totRoadWins()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

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

        If targetDiv$ <> "" Or (targetDiv$ = "" And expStdLeagName$(AA) <> "") Then

            Print #1, Tab(35); league$
            Print #1,
            Print #1, Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"

            Print #1, RTrim$(targetDiv$);
            Print #1, Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"
            Print #1, String$(80, 45)

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
                                winPCT! = teamTotWins(I) / (teamTotWins(I) + teamTotLosses(I))
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
                        Print #1, Tab(38); Using "###"; MJ;
                    End If

                    Print #1, Tab(44); Using "##"; teamWinStreak(I); Tab(47); teamLoseStreak(I); Tab(51); currWinStreak(I); Tab(54); currLoseStreak(I);
                    Print #1, Tab(57); Using "##"; totHomeWins!(I);: Print #1, Tab(60); Using "##"; totHomeLosses!(I);
                    Print #1, Tab(63); Using "##"; totRoadWins(I);: Print #1, Tab(66); Using "##"; totRoadLosses(I);
                    Print #1, Tab(69); Using "##"; totExtInnWins(I);: Print #1, Tab(72); Using "##"; totExtInnLosses(I);
                    Print #1, Tab(75); Using "##"; lastTenWins(I);: Print #1, Tab(78); Using "##"; lastTenLosses(I)

                End If

            Next I

            Print #1,
            Print #1,

        End If

    Next NT

    Close #1

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

    Shared currLoseStreak(), currWinStreak()
    Shared lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWins!()
    Shared totRoadLosses(), totRoadWins()
    Shared teamLoseStreak(), teamWinStreak(), teamTotLosses(), teamTotWins()

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
                        Swap currWinStreak(X), currWinStreak(Y)
                        Swap currLoseStreak(X), currLoseStreak(Y)
                        Swap teamTotWins(X), teamTotWins(Y)
                        Swap teamTotLosses(X), teamTotLosses(Y)
                        Swap totExtInnWins(X), totExtInnWins(Y)
                        Swap totExtInnLosses(X), totExtInnLosses(Y)
                        Swap totHomeLosses!(X), totHomeLosses!(Y)
                        Swap totRoadLosses(X), totRoadLosses(Y)
                        Swap totHomeWins!(X), totHomeWins!(Y)
                        Swap totRoadWins(X), totRoadWins(Y)
                        Swap lastTenLosses(X), lastTenLosses(Y)
                        Swap lastTenWins(X), lastTenWins(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub


'$Include: 'POSSRT.BAS'
'$Include: 'POSCR.BAS'
'$Include: 'PROGMAN.BAS'
'$Include: 'REC.BAS'
'$Include: 'SCHEDULE.BAS'
'Include: 'SEELEAG.BAS'


'----------------------------------------
'           SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "Full Count Baseball - View League Stats"

    leaderFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.LDR", "League Stats files [.LDR]")
    'ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If leaderFile$ <> "" Then

        Call LoadLeaderFiles(league$, div1$, div2$, div3$, div4$, leaderFile$)
        Call StatOptions(league$, div1$, div2$, div3$, div4$)

    End If

End Sub


'----------------------------------------
'       LoadLeaderFiles Subroutine
'----------------------------------------
'This subroutine loads the league stats
'from the file into several arrays
Sub LoadLeaderFiles (league$, div1$, div2$, div3$, div4$, targetFile$)

    Shared divWins(), divLosses()

    Shared DFT!(), OFT!()
    Shared leagLdrVal1!(), leagLdrVal2!()

    Shared BA$(), leagLdrPlayer$(), leagLdrTeam$(), ER$()
    Shared LF$(), leaderPlayer$(), memberName$()

    Open diskPaths$(2) + GetFileBaseName$(targetFile$) + ".TMS" For Input As #1
    Input #1, league$, div1$, div2$, div3$, div4$
    Close #1

    div1$ = PadRight$(div1$, 8)
    div2$ = PadRight$(div2$, 8)
    div3$ = PadRight$(div3$, 8)
    div4$ = PadRight$(div4$, 8)

    Cls

    Locate 9, 6
    Print "PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open targetFile$ For Input As #2

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, memberName$(X), divWins(X), divLosses(X)
    Next

    For X = 1 To 298:
        Input #2, leagLdrPlayer$(X), leagLdrTeam$(X), leagLdrVal1!(X), leagLdrVal2!(X)
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, BA$(X)
        For M = 1 To 14:
            Input #2, OFT!(X, M)
        Next
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, ER$(X)
        For M = 1 To 14:
            Input #2, DFT!(X, M)
        Next
    Next

    Input #2, APP

    Close #2

End Sub


'----------------------------------------
'         StatOptions Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions (league$, div1$, div2$, div3$, div4$)

    Do

        Cls
        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save League Report", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call LeaderStats_Report(league$, div1$, div2$, div3$, 0, userFile$)
                    End If

                Case "P":
                    Call LeaderStats_Report(league$, div1$, div2$, div3$, 1, "REPORT")

                Case "S":
                    Call LeaderStats_Screen(league$, div1$, div2$, div3$)

            End Select

            Color 15, 0
            Cls

            Call box0(6, 20, 10, 60, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub


'----------------------------------------
'     LeagStdgs_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeagStdgs_Screen ()

    Shared divWins(), divLosses()

End Sub


'----------------------------------------
'      LeaderStats_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer
Sub LeaderStats_Report (league$, div1$, div2$, div3$, printDest, destFile$)

    Shared divWins(), divLosses()

    Shared DFT!(), OFT!()
    Shared leagLdrVal1!(), leagLdrVal2!()

    Shared BA$(), leagLdrPlayer$(), leagLdrTeam$(), ER$()
    Shared LF$(), leaderPlayer$(), RT$(), memberName$()

    Erase LF$
    Erase RT$

    Color 15, 0

    Cls

    Open destFile$ For Output As #1

    For div = 1 To 4

        first = ((div - 1) * 10) + 1
        last = div * 10

        D1 = divWins(first) - divLosses(first)

        For P = first To last

            If memberName$(P) = "*" Then
                leaderPlayer$(P) = ""
            Else

                leaderPlayer$(P) = memberName$(P)

                While Len(leaderPlayer$(P)) < 12
                    leaderPlayer$(P) = leaderPlayer$(P) + " "
                Wend

                A$ = "     " + Str$(divWins(P))
                A$ = Right$(A$, 3)
                leaderPlayer$(P) = leaderPlayer$(P) + A$

                While Len(leaderPlayer$(P)) < 16
                    leaderPlayer$(P) = leaderPlayer$(P) + " "
                Wend

                A$ = "     " + Str$(divLosses(P))
                A$ = Right$(A$, W)
                leaderPlayer$(P) = leaderPlayer$(P) + A$

                While Len(leaderPlayer$(P)) < 21
                    leaderPlayer$(P) = leaderPlayer$(P) + " "
                Wend

                J! = 0
                If divWins(P) + divLosses(P) > 0 Then
                    J! = divWins(P) / (divWins(P) + divLosses(P))
                End If

                J! = CInt(1000 * J!)
                If J! = 1000 Then
                    leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "
                End If

                While Len(leaderPlayer$(P)) < 28
                    leaderPlayer$(P) = leaderPlayer$(P) + " "
                Wend

                'Games Behind
                D = divWins(P) - divLosses(P)
                D = D1 - D
                BK$ = Str$(Int(D / 2))
                If D = 0 Then BK$ = "---"
                If D = 1 Then BK$ = "  "
                If D > 19 Then
                    L = Len(BK$)
                    BK$ = Right$(BK$, L - 1)
                End If

                If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

                leaderPlayer$(P) = leaderPlayer$(P) + BK$

            End If

        Next P

    Next div


    '--- TEST THIS OUT !!!!!
    '--- COMAPRE TO SCREEN OUTPUT !!!!
    LF$(1) = "    " + league$ + " LEAGUE STANDINGS AS OF " + Date$
    LF$(3) = "    " + div1$ + "      W   L   PCT  GB"

    '------------------------------
    ' Division 1 Standings
    '------------------------------
    For P = 1 To 10:

        If leaderPlayer$(P) = "" Then
            RT = RT + 1
        Else
            LF$(P + 3 - RT) = "    " + leaderPlayer$(P)
        End If

    Next P

    RT = 0

    '------------------------------
    ' Division 2 Standings
    '------------------------------
    If RTrim$(div2$) <> "" Then

        LF$(15) = "    " + div3$ + "      W   L   PCT  GB"

        For P = 11 To 20:

            If leaderPlayer$(P) = "" Then
                RT = RT + 1
            Else
                LF$(P - 5 - RT) = "    " + leaderPlayer$(P)
            End If

        Next P

        RT = 0

    End If

    '------------------------------
    ' Division 3 Standings
    '------------------------------
    If RTrim$(div3$) <> "" Then

        RT$(3) = "    " + div2$ + "      W   L   PCT  GB"

        For P = 21 To 30

            If leaderPlayer$(P) = "" Then
                RT = RT + 1
            Else
                RT$(P - 7 - RT) = "    " + leaderPlayer$(P)
            End If

        Next P

        RT = 0

    End If


    '------------------------------
    ' Division 4 Standings
    '------------------------------
    If RTrim$(div4$) <> "" Then

        RT$(15) = "    " + div4Name$ + "      W   L   PCT   GB"

        For P = 31 To 40

            If leaderPlayer$(P) = "" Then
                RT = RT + 1
            Else
                RT$(P - 15 - RT) = "    " + leaderPlayer$(P)
            End If

        Next P

    End If

    'Print Division Standings
    ' -- Why 2 to 26 only???
    For P = 2 To 26

        If Len(LF$(P)) < 38 Then
            LF$(P) = LF$(P) + " "
        Else
            If Len(RT$(P)) < 38 Then
                RT$(P) = RT$(P) + " "
            End If
        End If

    Next P

    For X = 1 To 26
        Print #1, LF$(X); RT$(X)
    Next


    '------------------------------
    ' String Constants
    '------------------------------
    AB = Int(AB)
    AB$ = Str$(AB)
    TD = IP - 3 * IG

    AB$ = Right$(AB$, Len(AB$) - 1)
    If TD > 0 Then TD$ = " 1/3"
    IG$ = Str$(IP)

    IG$ = Right$(IG$, Len(IG$) - 1)
    IP$ = IG$


    '------------------------------
    '        BATTING LEADERS
    '------------------------------

    For P = 1 To 20

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 27
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            J! = CInt(1000 * leagLdrVal1!(P))
            If J! = 1000 Then leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "

            J! = leagLdrVal2!(P)
            leaderPlayer$(P) = leaderPlayer$(P) + " " + Str$(J!)

        End If

    Next P


    LF$(27) = "   BATTING LEADERS  (MIN 3.1 PA/G)"

    For P = 1 To 19
        LF$(P + 27) = "   " + leaderPlayer$(P)
    Next

    LF$(47) = " "


    For P = 239 To 258

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 27
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            J! = CInt(1000 * leagLdrVal1!(P))

            If J! = 1000 Then
                leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "
            End If

            J! = leagLdrVal2!(P)
            leaderPlayer$(P) = leaderPlayer$(P) + " " + Str$(J!)

        End If

    Next P

    RT$(27) = "     BATTING TRAILERS  (MIN 3.1 PA/G)"

    For P = 239 To 258:
        RT$(P - 211) = "      " + leaderPlayer$(P)
    Next

    '-----

    RT$(47) = " "

    For P = 27 To 47

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 27 To 47:
        Print #1, LF$(X); RT$(X)
    Next


    '------------------------------
    '             HITS
    '------------------------------

    For P = 21 To 30

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(49) = "   HITS"
    For P = 21 To 30:
        LF$(P + 29) = "   " + leaderPlayer$(P)
    Next P


    '------------------------------
    '           DOUBLES
    '------------------------------

    For P = 32 To 41

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(49) = "     DOUBLES"
    For P = 32 To 41:
        RT$(P + 18) = "     " + leaderPlayer$(P)
    Next P

    '------------------------------
    '           TRIPLES
    '------------------------------
    For P = 49 To 59

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 49 To 59:
        Print #1, LF$(X); RT$(X)
    Next

    Print #1, Chr$(12)

    '-----

    For P = 43 To 52

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(1) = "   TRIPLES"

    For P = 43 To 52:
        LF$(P - 41) = "   " + leaderPlayer$(P)
    Next P

    LF$(12) = " "


    '------------------------------
    '           HOME RUNS
    '------------------------------

    For P = 54 To 63:

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(1) = "     HOME RUNS"
    For P = 54 To 63:
        RT$(P - 52) = "     " + leaderPlayer$(P)
    Next P

    RT$(12) = " "

    '-----

    For P = 1 To 12

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 1 To 12:
        Print #1, LF$(X); RT$(X)
    Next

    '------------------------------
    '             RUNS
    '------------------------------

    For P = 65 To 73

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(13) = "   RUNS"
    For P = 65 To 73:
        LF$(P - 51) = "   " + leaderPlayer$(P)
    Next P
    LF$(23) = " "

    '------------------------------
    '             RBIs
    '------------------------------

    For P = 76 To 84

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(13) = "     RUNS BATTED IN"
    For P = 76 To 84:
        RT$(P - 62) = "     " + leaderPlayer$(P)
    Next P
    RT$(23) = " "

    '-----

    For P = 13 To 23

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 13 To 23:
        Print #1, LF$(X); RT$(X)
    Next


    '------------------------------
    '         STOLEN BASES
    '------------------------------

    For P = 87 To 96

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(24) = "   STOLEN BASES"
    For P = 87 To 96:
        LF$(P - 62) = "   " + leaderPlayer$(P)
    Next P
    LF$(35) = " "


    '------------------------------
    '         SLUGGING PCT
    '------------------------------

    For P = 99 To 108

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            J! = CInt(1000 * leagLdrVal1!(P))
            If J! = 1000 Then
                leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "
            End If

        End If

    Next P

    RT$(24) = "     SLUGGING PCT LEADERS"
    For P = 99 To 108:
        RT$(P - 74) = "     " + leaderPlayer$(P)
    Next P
    RT$(35) = " "

    '-----

    For P = 24 To 35

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 24 To 35:
        Print #1, LF$(X); RT$(X)
    Next


    '------------------------------
    '         ON BASE PCT
    '------------------------------

    For P = 110 To 119

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            J! = CInt(1000 * leagLdrVal1!(P))
            If J! = 1000 Then
                leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "
            End If

        End If

    Next P

    LF$(36) = "   ON BASE PCT LEADERS"
    For P = 110 To 119
        LF$(P - 73) = "   " + leaderPlayer$(P)
    Next P
    LF$(47) = " "


    '------------------------------
    '     BASE ON BALLS (WALK)
    '------------------------------

    For P = 217 To 226

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(36) = "     BASE ON BALLS"
    For P = 217 To 226
        RT$(P - 180) = "     " + leaderPlayer$(P)
    Next P
    RT$(47) = " "

    '-----

    For P = 36 To 47

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 36 To 47
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 279 To 288

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(48) = "   CURRENT HITTING STREAK"

    For P = 279 To 288: LF$(P - 230) = "   " + leaderPlayer$(P): Next P

    '-----

    For P = 289 To 298:

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 28
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(48) = "     LONGEST HITTING STREAK"
    For P = 289 To 298:
        RT$(P - 240) = "     " + leaderPlayer$(P)
    Next P

    '-----

    For P = 48 To 58

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 48 To 58:
        Print #1, LF$(X); RT$(X)
    Next

    Print #1, Chr$(12)

    '------------------------------
    '         ERA LEADERS
    '------------------------------

    For P = 121 To 139

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            J! = CInt(100 * leagLdrVal1!(P))
            A$ = Str$(J!)

            A$ = Right$(A$, Len(A$) - 1)
            A$ = Right$(("000" + LTrim$(A$)), 3)
            A$ = Left$(A$, LN - 2) + "." + Right$(A$, 2)
            A$ = "    " + A$
            A$ = Right$(A$, 5)

            leaderPlayer$(P) = leaderPlayer$(P) + A$

            J! = CInt(leagLdrVal2!(P))
            leaderPlayer$(P) = leaderPlayer$(P) + " " + Str$(J!)

        End If

    Next P

    RT$(27) = "     ERA LEADERS (MIN 1 IP/G)"

    For P = 121 To 139
        RT$(P - 93) = "    " + leaderPlayer$(P)
    Next


    '------------------------------
    '       PITCHING RECORD
    '------------------------------

    For P = 140 To 149

        leaderPlayer$(P) = ""

        If leagLdrVal1!(P) <> leagLdrVal1!(150) Or leagLdrVal2!(P) <> leagLdrVal2!(150) Then

            leaderPlayer$(P) = leaderPlayer$(P) + leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 23
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 2)
            leaderPlayer$(P) = leaderPlayer$(P) + A$
            leaderPlayer$(P) = leaderPlayer$(P) + "-"

            J! = leagLdrVal2!(P)
            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 2)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(48) = "   PITCHING RECORD"
    For P = 140 To 149
        LF$(P - 91) = "   " + leaderPlayer$(P)
    Next
    LF$(59) = " "


    '------------------------------
    '           VICTORIES
    '------------------------------

    For P = 151 To 160:
        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(48) = "     VICTORIES"
    For P = 151 To 160
        RT$(P - 102) = "     " + leaderPlayer$(P)
    Next P
    RT$(59) = " "

    '-----

    For P = 48 To 59

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 48 To 59:
        Print #1, LF$(X); RT$(X)
    Next

    Print #1, Chr$(12)


    '------------------------------
    '       INNINGS PITCHED
    '------------------------------

    For P = 162 To 171

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            J! = Int(leagLdrVal1!(P))
            A$ = "     " + Str$(J!)
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(1) = "   INNINGS PITCHED"
    For P = 162 To 171:
        LF$(P - 160) = "   " + leaderPlayer$(P)
    Next P
    LF$(12) = " "


    '------------------------------
    '          STRIKEOUTS
    '------------------------------

    For P = 173 To 182:

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    '-----

    RT$(1) = "     STRIKEOUTS"
    For P = 173 To 182:
        RT$(P - 171) = "     " + leaderPlayer$(P)
    Next P
    RT$(12) = " "

    For P = 1 To 12

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 1 To 12:
        Print #1, LF$(X); RT$(X)
    Next


    '------------------------------
    '          FULL GAMES
    '------------------------------

    For P = 184 To 193

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(13) = "   COMPLETE GAMES"
    For P = 184 To 193:
        LF$(P - 170) = "   " + leaderPlayer$(P)
    Next P
    LF$(24) = " "


    '------------------------------
    '           SHUTOUTS
    '------------------------------

    For P = 195 To 204: leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    RT$(13) = "     SHUTOUTS"
    For P = 195 To 204:
        RT$(P - 181) = "     " + leaderPlayer$(P)
    Next P
    RT$(24) = " "

    '-----

    For P = 13 To 24

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 13 To 24:
        Print #1, LF$(X); RT$(X)
    Next


    '------------------------------
    '            SAVES
    '------------------------------

    For P = 206 To 215

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    LF$(25) = "   SAVES"

    For P = 206 To 215:
        LF$(P - 180) = "   " + leaderPlayer$(P)
    Next P

    LF$(36) = " "
    RT$(25) = "     APPEARANCES"


    '------------------------------
    '         APPEARANCES
    '------------------------------

    For P = 228 To 237

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            While Len(leaderPlayer$(P)) < 24
                leaderPlayer$(P) = leaderPlayer$(P) + " "
            Wend

            A$ = "     " + Str$(leagLdrVal1!(P))
            A$ = Right$(A$, 3)
            leaderPlayer$(P) = leaderPlayer$(P) + A$

        End If

    Next P

    For P = 228 To 237
        RT$(P - 202) = "     " + leaderPlayer$(P)
    Next P
    RT$(36) = " "

    '-----

    For P = 25 To 36

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 25 To 36:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    Print #1, Chr$(12)
    Print #1, "                 CLUB BATTING   "
    Print #1,
    Print #1, "CLUB     AVG   AB    R    H   2B   3B  HR  RBI  BB   SO   SB   CS  E"

    For X = 1 To TEAMS_PER_LEAGUE

        If BA$(X) <> "" And Left$(BA$(X), 1) <> "*" And BA$(X) <> "*" Then

            Print #1, Left$(BA$(X), 11); Tab(13);

            OFT!(X, 1) = Int(OFT!(X, 1) * 1000) / 1000
            Print #1, Using ".###"; OFT!(X, 1);
            Print #1, Tab(18); OFT!(X, 3); Tab(24); OFT!(X, 5); Tab(29); Str$(OFT!(X, 4)); Tab(34); OFT!(X, 7); Tab(39); OFT!(X, 8); Tab(43); Str$(OFT!(X, 9)); Tab(47); OFT!(X, 6); Tab(52); OFT!(X, 10);
            Print #1, Tab(57); OFT!(X, 11); Tab(62); OFT!(X, 12); Tab(67); OFT!(X, 13); Tab(72); OFT!(X, 14)

        End If

    Next

    Print #1, Chr$(12)
    Print #1, "                 CLUB PITCHING  "
    Print #1,
    Print #1, "CLUB     ERA  G    CG SHO  SV  IP    H    R   ER   BB   SO    "

    For X = 1 To TEAMS_PER_LEAGUE

        If ER$(X) <> "" And ER$(X) <> "*" Then

            Print #1, Left$(ER$(X), 11); Tab(13);

            DFT!(X, 1) = Int(DFT!(X, 1) * 100) / 100
            Print #1, Using "##.##"; DFT!(X, 1);
            Print #1, Tab(18); DFT!(X, 11); Tab(23); DFT!(X, 12); Tab(27); DFT!(X, 13); Tab(31); DFT!(X, 14); Tab(35); Str$(CInt(DFT!(X, 2) / 3)); Tab(40); Str$(DFT!(X, 3)); Tab(45); DFT!(X, 4); Tab(50); DFT!(X, 5);
            Print #1, Tab(55); DFT!(X, 6); Tab(60); DFT!(X, 7)

        End If

    Next

    Close #1

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
'      LeaderStats_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStats_Screen (league$, div1$, div2$, div3$)

    Shared divWins(), divLosses()

    Shared DFT!(), OFT!()
    Shared leagLdrVal1!(), leagLdrVal2!()

    Shared BA$(), leagLdrPlayer$(), leagLdrTeam$(), ER$()
    Shared LF$(), leaderPlayer$(), memberName$()

    Dim J!, ATBATS!, INNINGS!

    'First, we build strings from the values we want to print
    'Later, we will loop through this array and print those strings
    'This appears to have been adopted in place of the approach used
    'most in other games, where complex print logic is in the loop

    RT = 0

    '------------------------------
    '          STANDINGS
    '------------------------------

    'Cycle through divisions
    For div = 1 To 4

        first = ((div - 1) * 10) + 1
        last = div * 10

        D1 = divWins(first) - divLosses(first)

        For P = first To last

            If memberName$(P) = "*" Then

                leaderPlayer$(P) = ""

            Else

                leaderPlayer$(P) = PadRight$(memberName$(P), 12)

                'divWins
                leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(divWins(P)), 3)
                leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 16)

                'Losses
                leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(divLosses(P)), 3)
                leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 21)

                'PCT
                J! = 0
                If divWins(P) + divLosses(P) > 0 Then J! = divWins(P) / (divWins(P) + divLosses(P))

                J! = CInt(1000 * J!)

                If J! = 1000 Then
                    leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "
                Else

                    If J! > 1000 Then

                        A$ = Str$(J! - 1000)
                        A$ = Right$(A$, Len(A$) - 1)
                        A$ = Right$(("000" + LTrim$(A$)), 3)

                        leaderPlayer$(P) = leaderPlayer$(P) + "1." + A$

                    Else

                        A$ = Str$(J!)
                        A$ = Right$(A$, Len(A$) - 1)
                        A$ = Right$(("000" + LTrim$(A$)), 3)

                        leaderPlayer$(P) = leaderPlayer$(P) + "." + A$

                    End If

                End If

                leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 28)

                'Games Behind
                D = divWins(P) - divLosses(P)
                D = D1 - D
                BK$ = Str$(Int(D / 2))
                If D = 0 Then BK$ = "---"
                If D = 1 Then BK$ = "  "
                If D > 19 Then
                    L = Len(BK$)
                    BK$ = Right$(BK$, L - 1)
                End If

                If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            End If

        Next P

    Next div

    '-----

    Cls

    Color 12
    Locate 2, Int((80 - Len(leagueName$) - 17) / 2)
    Print league$; " LEAGUE STANDINGS"

    '------------------------------
    ' Division 1 Standings
    '------------------------------
    Color 2
    Locate 4, 1
    Print "    " + RTrim$(div1$)

    Color 14
    Print "                  W   L   PCT     GB"

    Color 15

    For P = 1 To 10

        If leaderPlayer$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P + 5 - RT), 1: Print "    "; leaderPlayer$(P)
        End If

    Next P

    RT = 0


    '------------------------------
    ' Division 2 Standings
    '------------------------------
    If RTrim$(div2$) <> "" Then

        Color 2
        Locate 4, 41
        Print "    " + RTrim$(div2$)

        Color 14
        Locate , 41
        Print "                  W   L   PCT     GB"

        Color 15

        For P = 11 To 20

            If leaderPlayer$(P) = "" Then
                RT = RT + 1
            Else
                Locate (P - 5 - RT), 41: Print "    "; leaderPlayer$(P)
            End If

        Next P

        Print

        RT = 0

    End If

    '------------------------------
    ' Division 3 Standings
    '------------------------------
    If RTrim$(div3$) <> "" Then

        Color 2
        Locate 16, 1
        Print "    " + RTrim$(div3$)

        Color 14
        Print "                  W   L   PCT     GB"

        Color 15

        For P = 21 To 30

            If leaderPlayer$(P) = "" Then
                RT = RT + 1
            Else
                Locate (P - 3 - RT), 1: Print "    "; leaderPlayer$(P)
            End If

        Next P

        RT = 0

    End If

    '------------------------------
    ' Division 4 Standings
    '------------------------------
    If RTrim$(div4$) <> "" Then

        Color 2
        Locate 16, 41
        Print "    " + RTrim$(div4$)

        Color 14
        Locate , 41
        Print "                  W   L   PCT     GB"

        Color 15

        For P = 31 To 40

            If leaderPlayer$(P) = "" Then
                RT = RT + 1
            Else
                Locate (P - 13 - RT), 41: Print "    "; leaderPlayer$(P)
            End If

        Next P

    End If

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    ' String Constants
    '------------------------------
    AB$ = Str$(Int(AB))
    TD = IP - 3 * IG
    AB$ = Right$(AB$, Len(AB$) - 1)
    If TD > 0 Then
        TD$ = " 1/3"
    End If

    IG$ = Str$(IP)
    IP$ = Right$(IG$, Len(IG$) - 1)

    '------------------------------
    '        BATTING LEADERS
    '------------------------------

    For P = 1 To 19

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)

            J! = CInt(1000 * leagLdrVal1!(P))

            If J! = 1000 Then

                leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "

            Else

                If J! > 1000 Then

                    A$ = Str$(J! - 1000)
                    A$ = Right$(A$, Len(A$) - 1)
                    A$ = Right$(("000" + LTrim$(A$)), 3)

                    A$ = "1." + A$
                    leaderPlayer$(P) = leaderPlayer$(P) + A$

                Else

                    A$ = Str$(J!)
                    A$ = Right$(A$, Len(A$) - 1)
                    A$ = Right$(("000" + LTrim$(A$)), 3)

                    leaderPlayer$(P) = leaderPlayer$(P) + "." + A$

                End If

            End If

            leaderPlayer$(P) = leaderPlayer$(P) + "     " + Str$(leagLdrVal2!(P))

        End If

    Next P


    Cls

    Locate 2, 1
    Color 14
    Print "      BATTING LEADERS  (MIN 3.1 PA/G)"
    Locate 4, 1
    Print "WHO'S HOT                   AVG   AB    AND WHO'S NOT                AVG   AB"
    Color 15

    For P = 1 To 19
        Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P); Tab(28);
        Print Using ".###  ###"; leagLdrVal1!(P); leagLdrVal2!(P);

        Print Tab(41); leagLdrPlayer$(P + 238) + ", " + leagLdrTeam$(P + 238); Tab(69);
        Print Using ".###  ###"; leagLdrVal1!(P + 238); leagLdrVal2!(P + 238)
    Next

    Print

    '------------------------------
    '             HITS
    '------------------------------

    For P = 21 To 30

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "      HITS"
    Color 15
    For P = 21 To 30:
        Print "      "; leaderPlayer$(P)
    Next P
    Print


    '------------------------------
    '           DOUBLES
    '------------------------------

    For P = 32 To 41

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 14, 1
    Color 14
    Print "      DOUBLES"
    Color 15

    For P = 32 To 41:
        Print "      "; leaderPlayer$(P)
    Next P


    '------------------------------
    '           TRIPLES
    '------------------------------

    For P = 43 To 52

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 2, 40
    Color 14
    Print "      TRIPLES"
    Color 15

    For P = 43 To 52
        Locate (P - 40), 40
        Print "      "; leaderPlayer$(P)
    Next P


    '------------------------------
    '           HOME RUNS
    '------------------------------

    For P = 54 To 63
        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 14, 40
    Color 14
    Print "      HOME RUNS"
    Color 15

    For P = 54 To 63:
        Locate (P - 39), 40
        Print "      "; leaderPlayer$(P)
    Next P

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '             RUNS
    '------------------------------

    For P = 65 To 73

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Cls

    Locate 2, 1
    Color 14
    Print "      RUNS"
    Color 15

    For P = 65 To 73:
        Print "      "; leaderPlayer$(P)
    Next P

    '------------------------------
    '             RBIs
    '------------------------------
    For P = 76 To 84:

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 14, 1
    Color 14
    Print "      RUNS BATTED IN"
    Color 15

    For P = 76 To 84:
        Print "      "; leaderPlayer$(P)
    Next P


    '------------------------------
    '         STOLEN BASES
    '------------------------------

    For P = 87 To 96

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 2, 40
    Color 14
    Print "      STOLEN BASES"
    Color 15

    For P = 87 To 96:
        Locate (P - 84), 40
        Print "      "; leaderPlayer$(P)
    Next P


    '------------------------------
    '         SLUGGING PCT
    '------------------------------

    For P = 98 To 107:

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)

            J! = CInt(1000 * leagLdrVal1!(P))

            If J! = 1000 Then

                leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "

            Else

                If J! > 1000 Then

                    A$ = Str$(J! - 1000)
                    A$ = Right$(A$, Len(A$) - 1)
                    A$ = Right$(("000" + LTrim$(A$)), 3)

                    A$ = "1." + A$
                    leaderPlayer$(P) = leaderPlayer$(P) + A$

                Else

                    A$ = Str$(J!)
                    A$ = Right$(A$, Len(A$) - 1)
                    A$ = Right$(("000" + LTrim$(A$)), 3)

                    leaderPlayer$(P) = leaderPlayer$(P) + "." + A$

                End If

            End If

        End If

    Next P

    Locate 14, 40
    Color 14
    Print "      SLUGGING PCT LEADERS"
    Color 15

    For P = 99 To 108:
        Locate (P - 84), 40: Print "      "; leaderPlayer$(P)
    Next P

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '         ON BASE PCT
    '------------------------------

    For P = 110 To 119

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = Left$(leagLdrPlayer$(P), 13)
            If leagLdrTeam$(P) <> "*" Then leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)

            J! = CInt(1000 * leagLdrVal1!(P))

            If J! = 1000 Then

                leaderPlayer$(P) = leaderPlayer$(P) + "1.000 "

            Else

                If J! > 1000 Then

                    A$ = Str$(J! - 1000)
                    A$ = Right$(A$, Len(A$) - 1)
                    A$ = Right$(("000" + LTrim$(A$)), 3)

                    A$ = "1." + A$
                    leaderPlayer$(P) = leaderPlayer$(P) + A$

                Else

                    A$ = Str$(J!)
                    A$ = Right$(A$, Len(A$) - 1)
                    A$ = Right$(("000" + LTrim$(A$)), 3)

                    leaderPlayer$(P) = leaderPlayer$(P) + "." + A$

                End If

            End If

        End If

    Next P

    Cls

    Locate 2, 1
    Color 14
    Print "      ON BASE PCT LEADERS"
    Color 15

    For P = 110 To 119
        Print "      ";
        Print leaderPlayer$(P)
    Next P

    Print

    '------------------------------
    '     BASE ON BALLS (WALK)
    '------------------------------

    For P = 217 To 226

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P


    Locate 2, 40
    Color 14
    Print " BASE ON BALLS"
    Color 15

    For P = 217 To 226:
        Locate (P - 214), 41: Print leaderPlayer$(P)
    Next P

    '------------------------------

    Locate 14, 1
    Color 14
    Print "      CURRENT HITTING STREAK"
    Color 15

    For P = 279 To 288
        Print "      ";
        Print Left$(leagLdrPlayer$(P), 13) + ", " + leagLdrTeam$(P); Tab(34); Using "###"; leagLdrVal1!(P)
    Next P

    Locate 14, 40
    Color 14
    Print " LONGEST HITTING STREAK"
    Color 15

    For P = 289 To 298
        Locate , 41
        Print Left$(leagLdrPlayer$(P), 13) + ", " + leagLdrTeam$(P); Tab(67); Using "###"; leagLdrVal1!(P)
    Next P

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '         ERA LEADERS
    '------------------------------

    For P = 120 To 138

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)

            J! = CInt(100 * leagLdrVal1!(P))
            A$ = Str$(J!)

            A$ = Right$(A$, Len(A$) - 1)
            A$ = Right$(("000" + LTrim$(A$)), 3)
            A$ = Left$(A$, Len(A$) - 2) + "." + Right$(A$, 2)

            A$ = "    " + A$
            leaderPlayer$(P) = leaderPlayer$(P) + Right$(A$, 5)

            J! = Int(leagLdrVal2!(P))
            leaderPlayer$(P) = leaderPlayer$(P) + "     " + Str$(J!)

        End If

    Next P

    Cls

    Locate 2, 1
    Color 14
    Print "       ERA LEADERS (MIN 1 IP/G)"
    Locate 4, 1
    Print "WHO'S HOT                     ERA   IP  WHO'S NOT                      ERA  IP"
    Color 15

    For P = 121 To 139
        Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P); Tab(30); Using "#.##  ###"; leagLdrVal1!(P); leagLdrVal2!(P);
        Print Tab(41);
        Print leagLdrPlayer$(P + 138) + ", " + leagLdrTeam$(P + 138); Tab(70); Using "##.## ###"; leagLdrVal1!(P + 138); leagLdrVal2!(P + 138)
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '       PITCHING RECORD
    '------------------------------

    For P = 140 To 149

        leaderPlayer$(P) = ""

        If leagLdrVal1!(P) <> leagLdrVal1!(150) Or leagLdrVal2!(P) <> leagLdrVal2!(150) Then

            leaderPlayer$(P) = leaderPlayer$(P) + leagLdrPlayer$(P)

            If leagLdrTeam$(P) <> "" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 2)
            leaderPlayer$(P) = leaderPlayer$(P) + "-"
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal2!(P)), 2)

        End If

    Next P

    Cls

    Locate 2, 1
    Color 14
    Print "      PITCHING RECORD"
    Color 15

    For P = 140 To 149
        Print "      "; leaderPlayer$(P)
    Next

    '------------------------------
    '           VICTORIES
    '------------------------------

    For P = 151 To 160
        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "*" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 14, 1
    Color 14
    Print "      VICTORIES"
    Color 15

    For P = 151 To 160:
        Print "      "; leaderPlayer$(P)
    Next P

    '------------------------------
    '       INNINGS PITCHED
    '------------------------------

    For P = 162 To 171

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(Int(leagLdrVal1!(P))), 3)

        End If

    Next P

    Locate 2, 40
    Color 14
    Print "      INNINGS PITCHED"
    Color 15

    For P = 162 To 171:
        Locate (P - 159), 40: Print "      "; leaderPlayer$(P)
    Next P

    '------------------------------
    '          STRIKEOUTS
    '------------------------------

    For P = 173 To 182

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If

            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 14, 40
    Color 14
    Print "      STRIKEOUTS     "
    Color 15

    For P = 173 To 182:
        Locate (P - 158), 40
        Print "      "; leaderPlayer$(P)
    Next P

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '          FULL GAMES
    '------------------------------

    For P = 184 To 193

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Cls

    Locate 2, 1
    Color 14
    Print "      COMPLETE GAMES "
    Color 15

    For P = 184 To 193:
        Print "      "; leaderPlayer$(P)
    Next P

    '------------------------------
    '           SHUTOUTS
    '------------------------------

    For P = 195 To 204

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 14, 1
    Color 14
    Print "      SHUTOUTS       "
    Color 15

    For P = 195 To 204
        Print "      "; leaderPlayer$(P)
    Next P

    '------------------------------
    '            SAVES
    '------------------------------
    For P = 206 To 215

        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P

    Locate 2, 40
    Color 14
    Print "      SAVES          "
    Color 15

    For P = 206 To 215:
        Locate (P - 203), 40
        Print "      "; leaderPlayer$(P)
    Next P

    '------------------------------
    '         APPEARANCES
    '------------------------------

    Locate 14, 40
    Color 14
    Print "      APPEARANCES    "
    Color 15

    For P = 228 To 237
        leaderPlayer$(P) = ""

        If leagLdrPlayer$(P) <> "" Then

            leaderPlayer$(P) = leagLdrPlayer$(P)
            If leagLdrTeam$(P) <> "*" Then
                leaderPlayer$(P) = leaderPlayer$(P) + ", " + leagLdrTeam$(P)
            End If
            leaderPlayer$(P) = PadRight$(leaderPlayer$(P), 26)
            leaderPlayer$(P) = leaderPlayer$(P) + Right$("     " + Str$(leagLdrVal1!(P)), 3)

        End If

    Next P


    For P = 228 To 237:
        Locate (P - 213), 40
        Print "      "; leaderPlayer$(P)
    Next P

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print "                                 CLUB BATTING   "
    Color 14

    Locate 4, 1
    Print "CLUB         AVG   AB    R    H   2B  3B   HR  RBI   BB   SO  SB  CS   E"
    Color 15
    H! = 0
    AB = 0
    R = 0
    B3 = 0
    HR = 0
    RBI = 0
    BB = 0
    SO = 0
    SB = 0
    CS = 0
    E = 0

    For X = 1 To TEAMS_PER_LEAGUE

        If BA$(X) <> "" And BA$(X) <> "*" Then
            Print Left$(BA$(X), 11); Tab(13);
            '             AVG   AB    R    H  2B  3B   HR   RBI   BB   SO  SB  CS     E"
            Print Using ".###  ####  ### #### ### ###  ### ####  ### #### ### ###  ###"; OFT!(X, 1); OFT!(X, 3); OFT!(X, 5); OFT!(X, 4); OFT!(X, 7); OFT!(X, 8); OFT!(X, 9); OFT!(X, 6); OFT!(X, 10); OFT!(X, 11); OFT!(X, 12); OFT!(X, 13); OFT!(X, 14)
            ATBATS! = ATBATS! + OFT!(X, 3)
            H! = H! + OFT!(X, 4)
            R = R + OFT!(X, 5)
            B2 = B2 + OFT!(X, 7)
            B3 = B3 + OFT!(X, 8)
            HR = HR + OFT!(X, 9)
            RBI = RBI + OFT!(X, 6)
            BB = BB + OFT!(X, 10)
            SO = SO + OFT!(X, 11)
            SB = SB + OFT!(X, 12)
            CS = CS + OFT!(X, 13)
            E = E + OFT!(X, 14)
        End If
    Next

    If ATBATS! <> 0 Then
        statCalc! = H! / ATBATS!
    Else
        statCalc! = 0
    End If

    Print
    Print "LG TOTALS";
    'AVG
    Print Tab(13); Using ".###"; statCalc!;

    Print Tab(24); Using "####     ####     ####      ####     ####     ####"; R; B2; HR; BB; SB; E
    Print Tab(18); Using "#####     #####    ####      ####     #####    ####"; ATBATS!; H!; B3; RBI; SO; CS

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print "                                 CLUB PITCHING  "

    Locate 4, 1
    Color 14
    Print "CLUB          ERA   G  CG  SHO  SV    IP     H     R    ER     BB     SO"
    Color 15

    R = 0
    G = 0
    IP = 0

    For X = 1 To TEAMS_PER_LEAGUE

        If ER$(X) <> "" And ER$(X) <> "*" Then

            R = R + DFT!(X, 5)
            G = G + DFT!(X, 12)
            INNINGS! = INNINGS! + DFT!(X, 2)

            Print Left$(ER$(X), 11); Tab(13);

            DFT!(X, 1) = Int(DFT!(X, 1) * 100) / 100

            Print Using "##.##"; DFT!(X, 1);
            Print Tab(19); Using "### ### ###  ###  ####  ####  ####  ####"; DFT!(X, 11); DFT!(X, 12); DFT!(X, 13); DFT!(X, 14); CInt(DFT!(X, 2) / 3); DFT!(X, 3); DFT!(X, 4); DFT!(X, 5);
            Print Using "   ####   ####"; DFT!(X, 6); DFT!(X, 7)

        End If

    Next

    If INNINGS! > 0 Then
        statCalc! = R / (INNINGS! / 3) * 9
    Else
        statCalc! = 0
    End If

    Print
    Print Tab(13); Using "##.##"; statCalc!
    'PRINT "COMPLETE GAMES: "; G
    'PRINT "APPEARANCES: "; APP

    '-----
    Color 12
    Locate LAST_ROW - 1: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub
