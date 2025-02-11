'----------------------------------------
'        CompileLeague SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'league, and then relies on subroutines to
'read stats and compare to any existing
'leaders and records
Sub CompileLeague ()

    _Title "Full Count Baseball - Update League Stats"

    Shared ldrBatStat(), ldrPitStat()
    Shared divWins(), divLosses()

    Shared leaderVals!(), DFT!(), leaderStatVal2!(), OFT!()

    Shared BA$(), memberName$(), leagLdrPlayer$(), leagLdrTeam$()
    Shared ER$(), leaderNames$()

    Shared leagueName$

    'Initialize values first
    APP = 0

    Erase BA$, ER$
    Erase leagLdrPlayer$, leagLdrTeam$
    Erase leaderVals!
    Erase DFT!, OFT!

    For Z = 1 To TEAMS_PER_LEAGUE
        DFT!(Z, 1) = 99.99
    Next

    For X = 120 To 139
        leagLdrPlayer$(X) = "*"
        leagLdrTeam$(X) = "*"
        leaderVals!(X, 1) = 99.99
        leaderVals!(X, 2) = 0
    Next X

    For X = 239 To 258
        leagLdrPlayer$(X) = "*"
        leagLdrTeam$(X) = "*"
        leaderVals!(X, 1) = 99.99
        leaderVals!(X, 2) = 99.99
    Next X

    C$ = ","

    leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Call ReadLeagueStats (leagueFile$)

        Erase divWins, divLosses

        For XW = 1 To TEAMS_PER_LEAGUE

            'Look for * as terminating char
            If memberName$(XW) <> Chr$(42) Then

                haveStats = FindStatsLeague%(memberName$(XW), TN$, fullWins, fullLosses)

                If (haveStats) Then

                    statName$ = memberName$(XW)
                    nonConfGame = 0

                    For X = 1 To TEAMS_PER_LEAGUE:

                        If memberName$(X) = statName$ Then

                            If divWins(X) = fullWins And divLosses(X) = fullLosses Then

                                nonConfGame = 1
                                Exit For

                            Else

                                divWins(X) = fullWins
                                divLosses(X) = fullLosses

                            End If

                        End If

                        If (nonConfGame) Then Exit For

                    Next X
                    'alternatively, use a Do / Until loop; nonConfGame = 1 or EC = 30

                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                        Call ReadStatsLeague(memberName$(XW), TN$, totalGames)
                        Call CheckStatsLeague(memberName$(XW), statName$, totalGames)
                        Call CalcStatTotals(statName$, totalGames)

                    End If 'Done checking if we need to skip

                End If

            End If

        Next XW

        Call SortStandingsLeague
        Call PostLeaders(leagueName$, leagStatFile$, APP)

    End If

End Sub


'----------------------------------------
'         FindStatsLeague Function
'----------------------------------------
'...explanation...
Function FindStatsLeague% (statFile$, teamName$, fullWins, fullLosses)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsLeague% = 0

    Else

        Open diskPaths$(1) + statFile$ For Input As #1
        Input #1, teamName$, fullWins, fullLosses
        Close #1

        Color 15, 0
        Cls

        Locate 4, 5

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsLeague% = 1

    End If

End Function


'----------------------------------------
'       ReadStatsLeague Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the league
Sub ReadStatsLeague (statFile$, teamName$, totalGames)

    Dim leaderStatVal1(46)

    Shared ldrBatStat(), ldrPitStat()

    Shared leaderStatVal2!()

    Shared leaderNames$()

    Erase leaderNames$
    Erase leaderStatVal2!
    Erase ldrBatStat, ldrPitStat

    Open (diskPaths$(1) + statFile$) For Input As #1

    Input #1, teamName$, fullWins, fullLosses

    For K = 1 To 23
        Input #1, leaderNames$(K), leaderStatVal1(K)
    Next

    For K = 25 To 46
        Input #1, leaderNames$(K), leaderStatVal1(K)
    Next

    leaderNames$(24) = "PITCHERS"

    For J = 1 To 23
        For K = 1 To 22
            Input #1, leaderStatVal2!(J, K)
        Next K
    Next J

    For J = 25 To 46
        For K = 1 To 42
            Input #1, leaderStatVal2!(J, K)
        Next K
    Next J

    For K = 1 To 24
        Input #1, ldrBatStat(K)
    Next K

    For K = 1 To 23
        Input #1, ldrPitStat(K)
    Next K

    Close #1

    totalGames = fullWins + fullLosses

    'Extra calculations for batters
    For J = 1 To 23

        If leaderStatVal2!(J, 1) = 0 Then leaderStatVal2!(J, 1) = 1

        leaderStatVal2!(J, 17) = leaderStatVal2!(J, 13)
        leaderStatVal2!(J, 18) = leaderStatVal2!(J, 14)

        ' Must have 3.1 Min At Bats / Game
        '-- Previously compared to 2.5
        '-- Header for Viewing Leader says 3.1
        If (leaderStatVal2!(J, 8) + leaderStatVal2!(J, 1)) / totalGames < 3.1 Then
            leaderStatVal2!(J, 14) = 0
            leaderStatVal2!(J, 15) = 0
            leaderStatVal2!(J, 16) = 0
        Else
            leaderStatVal2!(J, 16) = leaderStatVal2!(J, 2) / leaderStatVal2!(J, 1)
            leaderStatVal2!(J, 14) = (leaderStatVal2!(J, 2) + leaderStatVal2!(J, 5) + 2 * leaderStatVal2!(J, 6) + 3 * leaderStatVal2!(J, 7)) / leaderStatVal2!(J, 1)
            leaderStatVal2!(J, 15) = (leaderStatVal2!(J, 2) + leaderStatVal2!(J, 8)) / (leaderStatVal2!(J, 1) + leaderStatVal2!(J, 8))
        End If

    Next

    'Must have 1 IP / Game
    '(leaderStatVal2!(J, 1) / 3) = IP
    For J = 25 To 46

        If leaderStatVal2!(J, 1) = 0 Or (leaderStatVal2!(J, 1) / 3) < totalGames Then
            leaderStatVal2!(J, 14) = 99.99
        Else
            leaderStatVal2!(J, 14) = leaderStatVal2!(J, 4) / leaderStatVal2!(J, 1) * 27
        End If

    Next

End Sub


'----------------------------------------
'      CheckStatsLeague Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsLeague (targetTeam$, statName$, totalGames)

    Dim pitcherNames$(TEAMS_PER_LEAGUE)

    Shared leaderNames$()
    Shared leagLdrPlayer$(), leagLdrTeam$()
    Shared leaderVals!(), DFT!(), leaderStatVal2!(), OFT!()

    Color 15, 0
    Cls

    'Color 12
    Print "CHECKING "; targetTeam$; " STATS"

    Color 15

    For X = 1 To 23

        If leaderNames$(X) <> "XXX" Then

            'Color 14
            Print "CHECKING STATS OF ";
            Color 15
            Print leaderNames$(X)

            pitcherNames$(X) = statName$

            skipIF = 0

            'Batting Avg
            For Y = 1 To 20

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    leaderVals!(Y, 1) = leaderStatVal2!(X, 13)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                    ' Must have 3.1 Min At Bats / Game
                    '-- Previously compared to 2.5
                    '-- Header for Viewing Leader says 3.1
                    If leaderVals!(Y, 2) < 3.1 * totalGames Then
                        leagLdrPlayer$(Y) = "*"
                        leagLdrTeam$(Y) = "*"
                        leaderVals!(Y, 1) = 0
                        leaderVals!(Y, 2) = 0
                    End If

                    For Y = 1 To 19
                        For Z = Y + 1 To 20
                            If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                                Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                                Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                                For Z1 = 1 To 2:
                                    Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3318

                End If

            Next Y

            If leaderStatVal2!(X, 16) >= leaderVals!(20, 1) Then

                leagLdrPlayer$(20) = leaderNames$(X)
                leagLdrTeam$(20) = pitcherNames$(X)
                leaderVals!(20, 1) = leaderStatVal2!(X, 16)
                leaderVals!(20, 2) = leaderStatVal2!(X, 1)

                For Y = 1 To 19
                    For Z = Y + 1 To 20
                        If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                            Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                            For Z1 = 1 To 2:
                                Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3318 '
            ' Must have 3.1 Min At Bats / Game
            '-- Previously compared to 2.5
            '-- Header for Viewing Leader says 3.1
            If (leaderStatVal2!(X, 8) + leaderStatVal2!(X, 1)) / totalGames >= 3.1 Then

                If leaderStatVal2!(X, 16) <> 0 And leaderStatVal2!(X, 16) <= leaderVals!(258, 1) Then

                    leagLdrPlayer$(258) = leaderNames$(X)
                    leagLdrTeam$(258) = pitcherNames$(X)
                    leaderVals!(258, 1) = leaderStatVal2!(X, 16)
                    leaderVals!(258, 2) = leaderStatVal2!(X, 1)

                    For Y = 239 To 257
                        For Z = Y + 1 To 258
                            If leaderVals!(Y, 1) > leaderVals!(Z, 1) Then
                                Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                                Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                                For Z1 = 1 To 2:
                                    Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                End If

            End If

            3320 '
            'Slugging Pct
            For Y = 99 To 108

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    leaderVals!(Y, 1) = leaderStatVal2!(X, 14)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                    ' Must have 3.1 Min At Bats / Game
                    '-- Previously compared to 2.5
                    '-- Header for Viewing Leader says 3.1
                    If leaderVals!(Y, 2) < totalGames * 3.1 Then
                        leagLdrPlayer$(Y) = "*"
                        leagLdrTeam$(Y) = "*"
                        leaderVals!(Y, 1) = 0
                        leaderVals!(Y, 2) = 0
                    End If

                    For Y = 99 To 107

                        For Z = Y + 1 To 108
                            If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                                Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                                Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                                For Z1 = 1 To 2:
                                    Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                                Next Z1
                            End If
                        Next Z

                    Next Y

                    GoTo 3325

                End If

            Next Y

            If leaderStatVal2!(X, 14) >= leaderVals!(108, 1) Then

                leagLdrPlayer$(108) = leaderNames$(X)
                leagLdrTeam$(108) = pitcherNames$(X)
                leaderVals!(108, 1) = leaderStatVal2!(X, 14)
                leaderVals!(108, 2) = leaderStatVal2!(X, 1)

                leaderVals!(Y, 1) = leaderStatVal2!(X, 14)
                leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                ' Must have 3.1 Min At Bats / Game
                '-- Previously compared to 2.5
                '-- Header for Viewing Leader says 3.1
                If leaderVals!(Y, 2) < totalGames * 3.1 Then
                    leagLdrPlayer$(Y) = "*"
                    leagLdrTeam$(Y) = "*"
                    leaderVals!(Y, 1) = 0
                    leaderVals!(Y, 2) = 0
                End If

                For Y = 99 To 107
                    For Z = Y + 1 To 108
                        If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                            Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                            For Z1 = 1 To 2:
                                Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3325 '
            'On Base Pct
            For Y = 110 To 119

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    leaderVals!(Y, 1) = leaderStatVal2!(X, 15)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                    ' Must have 3.1 Min PA / Game
                    '-- Previously compared to 2.5
                    '-- When viewing leaders, header says Min 3.1
                    If leaderVals!(Y, 2) < totalGames * 3.1 Then
                        leagLdrPlayer$(Y) = "*"
                        leagLdrTeam$(Y) = "*"
                        leaderVals!(Y, 1) = 0
                        leaderVals!(Y, 2) = 0
                    End If

                    For Y = 110 To 118
                        For Z = Y + 1 To 119
                            If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                                Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                                Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                                For Z1 = 1 To 2:
                                    Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3330

                End If

            Next Y

            3327 '
            If leaderStatVal2!(X, 15) >= leaderVals!(119, 1) Then

                leagLdrPlayer$(119) = leaderNames$(X)
                leagLdrTeam$(119) = pitcherNames$(X)
                leaderVals!(119, 1) = leaderStatVal2!(X, 15)
                leaderVals!(119, 2) = leaderStatVal2!(X, 1)

                leaderVals!(Y, 1) = leaderStatVal2!(X, 15)
                leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                ' Must have 3.1 Min PA / Game
                '-- Previously compared to 2.5
                '-- When viewing leaders, header says Min 3.1
                If leaderVals!(Y, 2) < totalGames * 3.1 Then
                    leagLdrPlayer$(Y) = "*"
                    leagLdrTeam$(Y) = "*"
                    leaderVals!(Y, 1) = 0
                    leaderVals!(Y, 2) = 0
                End If

                For Y = 110 To 118
                    For Z = Y + 1 To 119
                        If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                            Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                            For Z1 = 1 To 2
                                Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3330 '
            For Z = 21 To 31

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 2)
                    GoTo 3335
                End If

            Next Z

            If leaderStatVal2!(X, 2) > leaderVals!(31, 1) Then

                leaderVals!(31, 1) = leaderStatVal2!(X, 2)
                leagLdrPlayer$(31) = leaderNames$(X)
                leagLdrTeam$(31) = statName$

                3335 '
                For Z = 21 To 30
                    For Z1 = Z + 1 To 31
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            'Division 4
            For Z = 32 To 42

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 5)
                    GoTo 3347
                End If

            Next Z

            If leaderStatVal2!(X, 5) > leaderVals!(42, 1) Then

                leaderVals!(42, 1) = leaderStatVal2!(X, 5)
                leagLdrPlayer$(42) = leaderNames$(X)
                leagLdrTeam$(42) = statName$

                3347 '
                For Z = 32 To 41
                    For Z1 = Z + 1 To 42

                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

            3354 '
            For Z = 43 To 53
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 6)
                    GoTo 3359
                End If
            Next Z

            If leaderStatVal2!(X, 6) > leaderVals!(53, 1) Then

                leaderVals!(53, 1) = leaderStatVal2!(X, 6)
                leagLdrPlayer$(53) = leaderNames$(X)
                leagLdrTeam$(53) = statName$

                3359 '
                For Z = 43 To 52
                    For Z1 = Z + 1 To 53
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 54 To 64
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 7)
                    GoTo 3371
                End If
            Next Z

            If leaderStatVal2!(X, 7) > leaderVals!(64, 1) Then

                leaderVals!(64, 1) = leaderStatVal2!(X, 7)
                leagLdrPlayer$(64) = leaderNames$(X)
                leagLdrTeam$(64) = statName$

                3371 '
                For Z = 54 To 63:
                    For Z1 = Z + 1 To 64:
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3378 '
            For Z = 65 To 74

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 3)
                    GoTo 3383
                End If

            Next Z

            If leaderStatVal2!(X, 3) > leaderVals!(74, 1) Then

                leaderVals!(74, 1) = leaderStatVal2!(X, 3)
                leagLdrPlayer$(74) = leaderNames$(X)
                leagLdrTeam$(74) = statName$

                3383 '
                For Z = 65 To 73
                    For Z1 = Z + 1 To 74
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 76 To 85
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 4)
                    GoTo 3395
                End If
            Next Z

            If leaderStatVal2!(X, 4) > leaderVals!(85, 1) Then

                leaderVals!(85, 1) = leaderStatVal2!(X, 4)
                leagLdrPlayer$(85) = leaderNames$(X)
                leagLdrTeam$(85) = statName$

                3395 '
                For Z = 76 To 84
                    For Z1 = Z + 1 To 85
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

            For Z = 87 To 97

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 10)
                    GoTo 3407
                End If
            Next Z

            If leaderStatVal2!(X, 10) > leaderVals!(97, 1) Then

                leaderVals!(97, 1) = leaderStatVal2!(X, 10)
                leagLdrPlayer$(97) = leaderNames$(X)
                leagLdrTeam$(97) = statName$

                3407 '
                For Z = 87 To 96
                    For Z1 = Z + 1 To 97
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If 'Done checking for XXX / terminating chars

        3414 '
        'Base on Balls
        For Z = 217 To 227

            If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                leaderVals!(Z, 1) = leaderStatVal2!(X, 8)
                GoTo 3605
            End If

        Next Z

        If leaderStatVal2!(X, 8) > leaderVals!(227, 1) Then

            leaderVals!(227, 1) = leaderStatVal2!(X, 8)
            leagLdrPlayer$(227) = leaderNames$(X)
            leagLdrTeam$(227) = statName$

            3605 '
            For Z = 217 To 226
                For Z1 = Z + 1 To 227
                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                        Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                        Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                    End If
                Next Z1
            Next Z
        End If

        'Hitting Streak
        For Z = 279 To 288

            If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                leaderVals!(Z, 1) = leaderStatVal2!(X, 17)
                GoTo 3615
            End If

        Next Z

        If leaderStatVal2!(X, 17) > leaderVals!(288, 1) Then

            leaderVals!(288, 1) = leaderStatVal2!(X, 17)
            leagLdrPlayer$(288) = leaderNames$(X)
            leagLdrTeam$(288) = statName$

            3615 '
            For Z = 279 To 287
                For Z1 = Z + 1 To 288:

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                        Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                        Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                    End If

                Next Z1
            Next Z

        End If

        'Longest Hitting Streak
        For Z = 289 To 298
            3621 '
            If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                leaderVals!(Z, 1) = leaderStatVal2!(X, 18)
                GoTo 3625
            End If

        Next Z

        If leaderStatVal2!(X, 18) > leaderVals!(298, 1) Then

            leaderVals!(298, 1) = leaderStatVal2!(X, 18)
            leagLdrPlayer$(298) = leaderNames$(X)
            leagLdrTeam$(298) = statName$

            3625 '
            For Z = 289 To 297
                For Z1 = Z + 1 To 298

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                        Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                        Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                    End If

                Next Z1
            Next Z

        End If

    Next X

End Sub


'----------------------------------------
'      CalcStatTotals Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatTotals (statName$, totalGames)

    Shared ldrBatStat(), ldrPitStat()
    Shared leagLdrPlayer$(), leagLdrTeam$()
    Shared BA$(), ER$(), leaderNames$()
    Shared leaderVals!(), DFT!(), leaderStatVal2!(), OFT!()

    3415 '
    'All Pitchers
    For X = 25 To 46

        If leaderNames$(X) <> "XXX" Then

            'Color 14
            Print "CHECKING STATS OF ";
            Color 15
            Print leaderNames$(X)

            If leaderStatVal2!(X, 14) <= leaderVals!(139, 1) Then

                leagLdrPlayer$(139) = leaderNames$(X)
                leagLdrTeam$(139) = statName$
                leaderVals!(139, 1) = leaderStatVal2!(X, 14)
                leaderVals!(139, 2) = leaderStatVal2!(X, 1) / 3

                For Y = 121 To 138
                    For Z = Y + 1 To 139
                        If leaderVals!(Y, 1) > leaderVals!(Z, 1) Then
                            Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                            For Z1 = 1 To 2
                                Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            'Must have at least 1 IP / Game
            If (leaderStatVal2!(X, 1) / 3) >= totalGames And leaderStatVal2!(X, 1) <> 0 Then

                If leaderStatVal2!(X, 14) < 99.9 And leaderStatVal2!(X, 14) >= leaderVals!(278, 1) Then

                    leagLdrPlayer$(278) = leaderNames$(X)
                    leagLdrTeam$(278) = statName$
                    leaderVals!(278, 1) = leaderStatVal2!(X, 14)
                    leaderVals!(278, 2) = leaderStatVal2!(X, 1) / 3

                    For Y = 259 To 277
                        For Z = Y + 1 To 278
                            If leaderVals!(Y, 1) < leaderVals!(Z, 1) Then
                                Swap leagLdrPlayer$(Y), leagLdrPlayer$(Z)
                                Swap leagLdrTeam$(Y), leagLdrTeam$(Z)
                                For Z1 = 1 To 2
                                    Swap leaderVals!(Y, Z1), leaderVals!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                End If

            End If

            3430 '
            For Y = 140 To 150

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    leaderVals!(Y, 1) = leaderStatVal2!(X, 7)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 8)

                    '4860 '
                    For Z = 140 To 149:

                        For Y1 = Z + 1 To 150

                            If leaderVals!(Z, 1) - leaderVals!(Z, 2) <= leaderVals!(Y1, 1) - leaderVals!(Y1, 2) Then

                                If leaderVals!(Z, 1) - leaderVals!(Z, 2) <> leaderVals!(Y1, 1) - leaderVals!(Y1, 2) Or leaderVals!(Z, 1) > leaderVals!(Y1, 1) Then
                                    Swap leagLdrPlayer$(Y1), leagLdrPlayer$(Z)
                                    Swap leagLdrTeam$(Y1), leagLdrTeam$(Z)
                                    For Z1 = 1 To 2:
                                        Swap leaderVals!(Y1, Z1), leaderVals!(Z, Z1)
                                    Next Z1
                                End If

                            End If

                        Next Y1

                    Next Z

                    GoTo 3440

                End If

            Next Y

            If leaderStatVal2!(X, 7) - leaderStatVal2!(X, 8) >= leaderVals!(150, 1) - leaderVals!(150, 2) Then

                If Not (leaderStatVal2!(X, 7) - leaderStatVal2!(X, 8) = leaderVals!(150, 1) - leaderVals!(150, 2) And leaderStatVal2!(X, 7) >= leaderVals!(150, 1)) Then

                    leagLdrPlayer$(150) = leaderNames$(X)
                    leagLdrTeam$(150) = statName$
                    leaderVals!(150, 1) = leaderStatVal2!(X, 7)
                    leaderVals!(150, 2) = leaderStatVal2!(X, 8)

                    '4860 '
                    For Z = 140 To 149:
                        For Y1 = Z + 1 To 150

                            If leaderVals!(Z, 1) - leaderVals!(Z, 2) <= leaderVals!(Y1, 1) - leaderVals!(Y1, 2) Then

                                If leaderVals!(Z, 1) - leaderVals!(Z, 2) <> leaderVals!(Y1, 1) - leaderVals!(Y1, 2) Or leaderVals!(Z, 1) > leaderVals!(Y1, 1) Then
                                    Swap leagLdrPlayer$(Y1), leagLdrPlayer$(Z)
                                    Swap leagLdrTeam$(Y1), leagLdrTeam$(Z)
                                    For Z1 = 1 To 2:
                                        Swap leaderVals!(Y1, Z1), leaderVals!(Z, Z1)
                                    Next Z1
                                End If

                            End If

                        Next Y1
                    Next Z
                    'Return

                End If

            End If

            3440 '
            For Z = 151 To 161

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then

                    leaderVals!(Z, 1) = leaderStatVal2!(X, 7)
                    '3445

                    For Z = 151 To 160
                        For Z1 = Z + 1 To 161
                            If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                                Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                                Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                                Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                            End If
                        Next Z1
                    Next Z

                    GoTo 3445

                End If

            Next Z

            If leaderStatVal2!(X, 7) > leaderVals!(161, 1) Then

                leaderVals!(161, 1) = leaderStatVal2!(X, 7)
                leagLdrPlayer$(161) = leaderNames$(X)
                leagLdrTeam$(161) = statName$

                3445 '
                For Z = 151 To 160

                    For Z1 = Z + 1 To 161

                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If

                    Next Z1

                Next Z

            End If

            3452 '
            For Z = 162 To 172

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 1) / 3
                    GoTo 3457
                End If

            Next Z

            If leaderStatVal2!(X, 1) / 3 > leaderVals!(172, 1) Then

                leaderVals!(172, 1) = leaderStatVal2!(X, 1) / 3
                leagLdrPlayer$(172) = leaderNames$(X)
                leagLdrTeam$(172) = statName$

                3457 '
                For Z = 162 To 171
                    For Z1 = Z + 1 To 172
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3464 '
            For Z = 173 To 183
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 6)
                    GoTo 3469
                End If
            Next Z

            If leaderStatVal2!(X, 6) > leaderVals!(183, 1) Then

                leaderVals!(183, 1) = leaderStatVal2!(X, 6)
                leagLdrPlayer$(183) = leaderNames$(X)
                leagLdrTeam$(183) = statName$

                3469 '
                For Z = 173 To 182:
                    For Z1 = Z + 1 To 183:
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If

                    Next Z1
                Next Z

                '3474 FOR Z = 173 TO 183
                '3475 NEXT Z
            End If

            3476 '
            For Z = 184 To 194
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 11)
                    GoTo 3481
                End If
            Next Z

            If leaderStatVal2!(X, 11) >= leaderVals!(194, 1) Then

                leaderVals!(194, 1) = leaderStatVal2!(X, 11)
                leagLdrPlayer$(194) = leaderNames$(X)
                leagLdrTeam$(194) = statName$

                3481 '
                For Z = 184 To 193
                    For Z1 = Z + 1 To 194
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3488 '
            For Z = 195 To 205

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 12)
                    GoTo 3493
                End If
            Next Z

            If leaderStatVal2!(X, 12) > leaderVals!(205, 1) Then
                leaderVals!(205, 1) = leaderStatVal2!(X, 12)
                leagLdrPlayer$(205) = leaderNames$(X)
                leagLdrTeam$(205) = statName$

                3493 '
                For Z = 195 To 204
                    For Z1 = Z + 1 To 205
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3500 '
            For Z = 206 To 216

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 13)
                    GoTo 3505
                End If
            Next Z

            If leaderStatVal2!(X, 13) > leaderVals!(216, 1) Then

                leaderVals!(216, 1) = leaderStatVal2!(X, 13)
                leagLdrPlayer$(216) = leaderNames$(X)
                leagLdrTeam$(216) = statName$

                3505 '
                For Z = 206 To 215
                    For Z1 = Z + 1 To 216
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            APP = APP + leaderStatVal2!(X, 9)
            For Z = 228 To 238
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 9)
                    GoTo 3518
                End If
            Next Z

            If leaderStatVal2!(X, 9) > leaderVals!(238, 1) Then

                leaderVals!(238, 1) = leaderStatVal2!(X, 9)
                leagLdrPlayer$(238) = leaderNames$(X)
                leagLdrTeam$(238) = statName$

                3518 '
                For Z = 228 To 237:
                    For Z1 = Z + 1 To 238:
                        If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                            Swap leagLdrPlayer$(Z1), leagLdrPlayer$(Z)
                            Swap leagLdrTeam$(Z1), leagLdrTeam$(Z)
                            Swap leaderVals!(Z1, 1), leaderVals!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

        End If 'Done checking for "XXX"

    Next X

    3530 '
    For Z = 1 To TEAMS_PER_LEAGUE

        If ldrBatStat(3) = 0 Then ldrBatStat(3) = 1

        If BA$(Z) = statName$ Then
            OFT!(Z, 1) = ldrBatStat(4) / ldrBatStat(3)
            For M = 3 To 14: 
                OFT!(Z, M) = ldrBatStat(M)
            Next M
            GoTo 3535
        End If

    Next Z

    If ldrBatStat(4) / ldrBatStat(3) > OFT!(40, 1) Then

        OFT!(40, 1) = ldrBatStat(4) / ldrBatStat(3)

        For M = 3 To 14: 
            OFT!(40, M) = ldrBatStat(M)
        Next M

        BA$(40) = statName$

        3535 '
        For Z = 1 To 39

            For Z1 = Z + 1 To 40

                If OFT!(Z, 1) < OFT!(Z1, 1) Then

                    Swap BA$(Z1), BA$(Z)

                    For M = 1 To 14:
                        Swap OFT!(Z1, M), OFT!(Z, M)
                    Next M

                End If

            Next Z1

        Next Z

        For Z = 1 To TEAMS_PER_LEAGUE
            If OFT!(Z, 1) = OFT!(40, 1) Then 
                BA$(Z) = "*"
            End If
        Next Z

    End If

    For Z = 1 To TEAMS_PER_LEAGUE

        If ldrPitStat(1) = 0 Then 
            ldrPitStat(1) = 1
            ldrPitStat(4) = 3
        End If

        If ER$(Z) = statName$ Then
            DFT!(Z, 1) = ldrPitStat(4) / ldrPitStat(1) * 27
            For M = 2 To 14:
                DFT!(Z, M) = ldrPitStat(M - 1)
            Next M
            GoTo 3555
        End If

    Next Z

    If ldrPitStat(4) / ldrPitStat(1) * 27 < DFT!(40, 1) Then

        DFT!(40, 1) = ldrPitStat(4) / ldrPitStat(1) * 27
        For M = 2 To 14: 
            DFT!(40, M) = ldrPitStat(M - 1)
        Next M

        ER$(40) = statName$
        ER$(40) = statName$

        3555 '
        For Z = 1 To 39

            For Z1 = Z + 1 To 40

                If DFT!(Z, 1) > DFT!(Z1, 1) Then

                    Swap ER$(Z1), ER$(Z)

                    For M = 1 To 14:
                        Swap DFT!(Z1, M), DFT!(Z, M)
                    Next M

                End If

            Next Z1

        Next Z

        For Z = 1 To TEAMS_PER_LEAGUE
            If DFT!(Z, 1) = DFT!(40, 1) Then 
                ER$(Z) = "*"
            End If
        Next Z

    End If

End Sub


'----------------------------------------
'     SortStandingsLeague Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target league. Currently setup using a 
'bubble sort algorithm.
Sub SortStandingsLeague

    Shared divWins(), divLosses()
    Shared memberName$()

    'Sort record by games won
    For Z = 0 To 3

        AA = (Z * 10) + 1
        ZZ = (Z * 10) + 10

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ:

                diffX = divWins(X) - divLosses(X)
                diffY = divWins(Y) - divLosses(Y)

                If diffX <= diffY Then

                    If diffX <> diffY Then

                        If Not (divWins(X) <= divWins(Y) And divWins(X) > divLosses(X)) Or Not (divWins(X) >= divWins(Y) And divWins(X) < divLosses(X)) Then

                            Swap memberName$(X), memberName$(Y)
                            Swap divWins(X), divWins(Y)
                            Swap divLosses(X), divLosses(Y)

                        End If

                    End If

                End If

            Next Y

        Next X

    Next Z

End Sub


'----------------------------------------
'       PostLeaders Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the league, and
'writes the updated stats to the file
Sub PostLeaders (league$, leaderFile$, APP)

    Shared divWins(), divLosses()

    Shared leaderVals!(), DFT!(), OFT!()

    Shared BA$(), leagLdrPlayer$(), leagLdrTeam$()
    Shared ER$(), memberName$()

    Color 15, 0

    Print "POSTING "; league$; " LEADERS"

    Open diskPaths$(2) + leaderFile$ + ".LDR" For Output As #2

    For X = 1 To TEAMS_PER_LEAGUE
        Write #2, memberName$(X), divWins(X), divLosses(X)
    Next

    For X = 1 To 298
        Write #2, leagLdrPlayer$(X), leagLdrTeam$(X), leaderVals!(X, 1), leaderVals!(X, 2)
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Write #2, BA$(X)
        For M = 1 To 14:
            Write #2, OFT!(X, M)
        Next
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Write #2, ER$(X)
        For M = 1 To 14:
            Write #2, DFT!(X, M)
        Next
    Next

    Write #2, APP

    Close 2

End Sub
