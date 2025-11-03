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

    Shared ldrBatStat!(), ldrPitStat!()
    Shared divWins(), divLosses()
    Shared statBatter(), statPitcher()

    Shared leaderVals!(), DFT!(), leaderStatVal2!(), OFT!()

    Shared BA$(), memberName$(), leagLdrPlayer$(), leagLdrTeam$()
    Shared ER$(), leaderNames$()

    Shared batRecVal!(), batRecTmVal!()
    Shared pitRecVal!(), pitRecTmVal!()
    Shared batRecTmDesc$(), pitRecTmDesc$()

    Shared leagueName$

    If _FileExists(diskPaths$(1) + "RECORDS.MLB") Then

        Call ReadSeasonRecords("RECORDS.MLB")

    Else

        Erase batRecVal!, pitRecVal!
        Erase batRecTmVal!, pitRecTmVal!
        Erase batRecTmDesc$, pitRecTmDesc$

    End If

    'Initialize values first
    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0
    pitRecVal!(23) = 100
    pitRecVal!(25) = 100

    batRecVal!(24) = 999
    batRecVal!(25) = 999
    batRecVal!(26) = 999
    batRecVal!(27) = 999
    batRecVal!(28) = 999
    pitRecVal!(27) = 100

    pitRecTmVal!(24) = 100

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

    leagueFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.TMS", "League Stats files [.TMS]")
    'ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE", ExitCode%)

    If leagueFile$ <> "" Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Call ReadLeagueMembers(leagueFile$)

        Erase divWins, divLosses

        For XW = 1 To TEAMS_PER_LEAGUE

            'Look for * as terminating char
            If memberName$(XW) <> Chr$(42) Then

                haveStats = FindStatsLeague%(memberName$(XW), TN$, fullWins, fullLosses)

                If (haveStats) Then

                    statName$ = memberName$(XW)
                    sameTeam = 0

                    For X = 1 To TEAMS_PER_LEAGUE:

                        If memberName$(X) = statName$ Then

                            If divWins(X) = fullWins And divLosses(X) = fullLosses Then

                                sameTeam = 1

                            Else

                                divWins(X) = fullWins
                                divLosses(X) = fullLosses

                            End If

                        End If

                        If (sameTeam) Then Exit For

                    Next X
                    'alternatively, use a Do / Until loop; sameTeam = 1 or EC = 30

                    If sameTeam = 0 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                        Call ReadStatsLeague(memberName$(XW), TN$, totalGames)

                        Call CalcRecordsPro(totalGames, TN$, memberName$(XW), finalUpdate$)

                        Call CalcStatLeadersBat(memberName$(XW), statName$, totalGames)
                        Call CalcStatLeadersPitch(statName$, totalGames, APP)
                        Call CalcStatLeadersTeam(statName$, totalGames)

                    End If 'Done checking if we need to skip

                End If

            End If

        Next XW

        Call SortStandingsLeague
        Call PostLeaders(GetFileBaseName$(leagueFile$), APP)
        Call PostRecordsPro

    End If

End Sub


'----------------------------------------
'         FindStatsLeague Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the league and loads them if
'found, notifying the user
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

    Shared ldrBatStat!(), ldrPitStat!()

    Shared leaderStatVal2!()

    Shared leaderNames$()

    Erase leaderNames$
    Erase leaderStatVal2!
    Erase ldrBatStat!, ldrPitStat!

    'Read Team stats
    'Assing dedicated arrays to those values

    Open (diskPaths$(1) + statFile$) For Input As #1

    Input #1, teamName$, fullWins, fullLosses

    For K = 1 To 23
        Input #1, leaderNames$(K), leaderStatVal1(K)
    Next

    For K = 25 To 46
        Input #1, leaderNames$(K), leaderStatVal1(K)
    Next

    leaderNames$(24) = "PITCHERS"

    '0 to 22
    For J = 1 To 23
        '0 to 21
        For K = 1 To 22
            Input #1, leaderStatVal2!(J, K)
        Next K
    Next J

    '0 to 21
    For J = 25 To 46
        '0 to 41
        For K = 1 To 42
            Input #1, leaderStatVal2!(J, K)
        Next K
    Next J

    For K = 1 To 24
        Input #1, ldrBatStat!(K)
    Next K

    For K = 1 To 23
        Input #1, ldrPitStat!(K)
    Next K

    Close #1

    totalGames = fullWins + fullLosses

End Sub


'----------------------------------------
'      CalcStatLeadersBat Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeadersBat (targetTeam$, statName$, totalGames)

    Shared leaderNames$(), leaderTeams$()
    Shared leagLdrPlayer$(), leagLdrTeam$()
    Shared leaderVals!(), leaderStatVal2!()

    'Extra calculations for batters
    For J = 1 To 23

        If leaderStatVal2!(J, 1) = 0 Then
            leaderStatVal2!(J, 1) = 1
        End If

        leaderStatVal2!(J, 17) = leaderStatVal2!(J, 13)
        leaderStatVal2!(J, 18) = leaderStatVal2!(J, 14)

        ' Must have 3.1 Min At-Bat's / Game
        If (leaderStatVal2!(J, 8) + leaderStatVal2!(J, 1)) < totalGames * 3.1 Then
            'Minimum not met - use 0 value
            leaderStatVal2!(J, 14) = 0
            leaderStatVal2!(J, 15) = 0
            leaderStatVal2!(J, 16) = 0
        Else
            'Slugging %
            leaderStatVal2!(J, 14) = (leaderStatVal2!(J, 2) + leaderStatVal2!(J, 5) + 2 * leaderStatVal2!(J, 6) + 3 * leaderStatVal2!(J, 7)) / leaderStatVal2!(J, 1)
            'OBP%
            leaderStatVal2!(J, 15) = (leaderStatVal2!(J, 2) + leaderStatVal2!(J, 8)) / (leaderStatVal2!(J, 1) + leaderStatVal2!(J, 8))
            'Batting Avg%
            leaderStatVal2!(J, 16) = leaderStatVal2!(J, 2) / leaderStatVal2!(J, 1)
        End If

    Next

    Color 15, 0
    Cls

    'Color 12
    Print "CHECKING "; targetTeam$; " STATS"

    Color 15

    ' Must have 3.1 Min At-Bats / Game
    '-- Previously compared to 2.5
    '-- When viewing leaders, header says Min 3.1
    minAtBat = totalGames * 3.1

    'All Batters
    For X = 1 To 23

        If leaderNames$(X) <> "XXX" Then

            'Color 14
            Print "CHECKING STATS OF ";
            Color 15
            Print leaderNames$(X)

            leaderTeams$(X) = statName$

            For Y = 1 To 20

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    'Long Hit Streak
                    leaderVals!(Y, 1) = leaderStatVal2!(X, 13)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                    If leaderVals!(Y, 2) < minAtBat Then
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

            ' ** Batting Avg **
            If leaderStatVal2!(X, 16) >= leaderVals!(20, 1) Then

                leagLdrPlayer$(20) = leaderNames$(X)
                leagLdrTeam$(20) = leaderTeams$(X)
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
            If (leaderStatVal2!(X, 8) + leaderStatVal2!(X, 1)) >= minAtBat Then

                If leaderStatVal2!(X, 16) <> 0 And leaderStatVal2!(X, 16) <= leaderVals!(258, 1) Then

                    leagLdrPlayer$(258) = leaderNames$(X)
                    leagLdrTeam$(258) = leaderTeams$(X)
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
            ' ** Slugging Pct **
            For Y = 99 To 108

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    leaderVals!(Y, 1) = leaderStatVal2!(X, 14)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                    If leaderVals!(Y, 2) < minAtBat Then
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
                leagLdrTeam$(108) = leaderTeams$(X)
                leaderVals!(108, 1) = leaderStatVal2!(X, 14)
                leaderVals!(108, 2) = leaderStatVal2!(X, 1)

                leaderVals!(Y, 1) = leaderStatVal2!(X, 14)
                leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                If leaderVals!(Y, 2) < minAtBat Then
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
            ' ** On Base Pct **
            For Y = 110 To 119

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = statName$ Then

                    leaderVals!(Y, 1) = leaderStatVal2!(X, 15)
                    leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                    If leaderVals!(Y, 2) < minAtBat Then
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
                leagLdrTeam$(119) = leaderTeams$(X)
                leaderVals!(119, 1) = leaderStatVal2!(X, 15)
                leaderVals!(119, 2) = leaderStatVal2!(X, 1)

                leaderVals!(Y, 1) = leaderStatVal2!(X, 15)
                leaderVals!(Y, 2) = leaderStatVal2!(X, 1)

                If leaderVals!(Y, 2) < minAtBat Then
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
            ' ** Hits **
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

            ' ** 2B **
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
            ' ** 3B **
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

            ' ** HR **
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
            ' ** Runs **
            For Z = 65 To 74
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = statName$ Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 3)
                    GoTo 3383
                End If
            Next Z

            ' ** Runs **
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

            ' ** RBI **
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

            ' ** Stolen Bases **
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

        End If

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

        'Games
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
'     CalcStatLeadersPitch Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatLeadersPitch (statName$, totalGames, nbrAppearances)

    Shared leagLdrPlayer$(), leagLdrTeam$()
    Shared leaderNames$(), leaderTeams$()
    Shared leaderVals!(), leaderStatVal2!()

    'Pitchers - ERA Calculation
    For J = 25 To 46

        'IP = 0 or pitcher's games < totalGames (< 1 IP / game)
        If leaderStatVal2!(J, 1) = 0 Or (leaderStatVal2!(J, 1) / 3) < totalGames Then
            'Junk value for unqualified pitchers
            leaderStatVal2!(J, 14) = 99.99
        Else
            leaderStatVal2!(J, 14) = leaderStatVal2!(J, 4) / leaderStatVal2!(J, 1) * 27
        End If

    Next

    3415 '
    'All Pitchers
    For X = 25 To 46

        If leaderNames$(X) <> "XXX" Then

            'Color 14
            Print "CHECKING STATS OF ";
            Color 15
            Print leaderNames$(X)

            leaderTeams$(X) = statName$

            ' ** ERA **
            If leaderStatVal2!(X, 14) <= leaderVals!(139, 1) Then

                leagLdrPlayer$(139) = leaderNames$(X)
                leagLdrTeam$(139) = leaderTeams$(X)
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

            If (leaderStatVal2!(X, 1) / 3) >= totalGames And leaderStatVal2!(X, 1) <> 0 Then

                If leaderStatVal2!(X, 14) < 99.9 And leaderStatVal2!(X, 14) >= leaderVals!(278, 1) Then

                    leagLdrPlayer$(278) = leaderNames$(X)
                    leagLdrTeam$(278) = leaderTeams$(X)
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
            ' ** Wins / Losses **
            For Y = 140 To 150

                If leagLdrPlayer$(Y) = leaderNames$(X) And leagLdrTeam$(Y) = leaderTeams$(X) Then

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
                    leagLdrTeam$(150) = leaderTeams$(X)
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
            ' ** Wins **
            For Z = 151 To 161

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then

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
                leagLdrTeam$(161) = leaderTeams$(X)

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
            ' ** Outs / Game (IP / 3) **
            For Z = 162 To 172

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 1) / 3
                    GoTo 3457
                End If

            Next Z

            If leaderStatVal2!(X, 1) / 3 > leaderVals!(172, 1) Then

                leaderVals!(172, 1) = leaderStatVal2!(X, 1) / 3
                leagLdrPlayer$(172) = leaderNames$(X)
                leagLdrTeam$(172) = leaderTeams$(X)

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
            ' ** Strikeouts **
            For Z = 173 To 183
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 6)
                    GoTo 3469
                End If
            Next Z

            If leaderStatVal2!(X, 6) > leaderVals!(183, 1) Then

                leaderVals!(183, 1) = leaderStatVal2!(X, 6)
                leagLdrPlayer$(183) = leaderNames$(X)
                leagLdrTeam$(183) = leaderTeams$(X)

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
            ' ** Games Completed **
            For Z = 184 To 194
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 11)
                    GoTo 3481
                End If
            Next Z

            If leaderStatVal2!(X, 11) >= leaderVals!(194, 1) Then

                leaderVals!(194, 1) = leaderStatVal2!(X, 11)
                leagLdrPlayer$(194) = leaderNames$(X)
                leagLdrTeam$(194) = leaderTeams$(X)

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
            ' ** Shutouts **
            For Z = 195 To 205

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 12)
                    GoTo 3493
                End If
            Next Z

            If leaderStatVal2!(X, 12) > leaderVals!(205, 1) Then
                leaderVals!(205, 1) = leaderStatVal2!(X, 12)
                leagLdrPlayer$(205) = leaderNames$(X)
                leagLdrTeam$(205) = leaderTeams$(X)

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
            ' ** Saves **
            For Z = 206 To 216

                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 13)
                    GoTo 3505
                End If
            Next Z

            If leaderStatVal2!(X, 13) > leaderVals!(216, 1) Then

                leaderVals!(216, 1) = leaderStatVal2!(X, 13)
                leagLdrPlayer$(216) = leaderNames$(X)
                leagLdrTeam$(216) = leaderTeams$(X)

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

            nbrAppearances = nbrAppearances + leaderStatVal2!(X, 9)

            ' ** Games **
            For Z = 228 To 238
                If leagLdrPlayer$(Z) = leaderNames$(X) And leagLdrTeam$(Z) = leaderTeams$(X) Then
                    leaderVals!(Z, 1) = leaderStatVal2!(X, 9)
                    GoTo 3518
                End If
            Next Z

            If leaderStatVal2!(X, 9) > leaderVals!(238, 1) Then

                leaderVals!(238, 1) = leaderStatVal2!(X, 9)
                leagLdrPlayer$(238) = leaderNames$(X)
                leagLdrTeam$(238) = leaderTeams$(X)

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

        End If

    Next X

End Sub


'----------------------------------------
'     CalcStatLeadersPitch Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatLeadersTeam (statName$, totalGames)

    Shared ldrBatStat!(), ldrPitStat!()
    Shared BA$(), ER$()
    Shared DFT!(), OFT!()

    For Z = 1 To TEAMS_PER_LEAGUE

        If ldrBatStat!(3) = 0 Then
            ldrBatStat!(3) = 1
        End If

        If BA$(Z) = statName$ Then
            OFT!(Z, 1) = ldrBatStat!(4) / ldrBatStat!(3)
            For M = 3 To 14:
                OFT!(Z, M) = ldrBatStat!(M)
            Next M
            GoTo 3535
        End If

    Next Z

    If ldrBatStat!(4) / ldrBatStat!(3) > OFT!(40, 1) Then

        OFT!(40, 1) = ldrBatStat!(4) / ldrBatStat!(3)

        For M = 3 To 14:
            OFT!(40, M) = ldrBatStat!(M)
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

        If ldrPitStat!(1) = 0 Then
            ldrPitStat!(1) = 1
            ldrPitStat!(4) = 3
        End If

        If ER$(Z) = statName$ Then
            DFT!(Z, 1) = ldrPitStat!(4) / ldrPitStat!(1) * 27
            For M = 2 To 14:
                DFT!(Z, M) = ldrPitStat!(M - 1)
            Next M
            GoTo 3555
        End If

    Next Z

    If ldrPitStat!(4) / ldrPitStat!(1) * 27 < DFT!(40, 1) Then

        DFT!(40, 1) = ldrPitStat!(4) / ldrPitStat!(1) * 27
        For M = 2 To 14:
            DFT!(40, M) = ldrPitStat!(M - 1)
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
'      CalcRecordsPro Subroutine
'----------------------------------------
'Read in all league stats and calculate
'records so that an output file can be
'made containing all records.
' ** in order to do this like Football
' ** and Basketball, we have to use
' ** the team name and not the year
Sub CalcRecordsPro (totalGames, teamName$, statName$, finalUpdate$)

    '--- These will go away
    Shared statBatter(), statPitcher()
    Shared statTeamBat!(), statTeamPit!()
    Shared batterNames$(), pitcherNames$()

    '--- These will stay
    Shared leaderStatVal2!()
    Shared ldrBatStat!(), ldrPitStat!()
    Shared leaderNames$()

    Shared batRecTmVal!(), batRecTmYr!()
    Shared batRecVal!(), batRecYr!()
    Shared batTotGames!()

    Shared pitRecTmVal!(), pitRecTmYr!()
    Shared pitRecVal!(), pitRecYr!()
    Shared pitTotGames!()

    Shared batRecPlyr$(), batRecTeam$(), batRecTmDesc$()
    Shared pitRecPlyr$(), pitRecTeam$(), pitRecTmDesc$()

    Dim TT$(40, 22), TT1$(40, 23)

    AAG = (23 * 20)
    AAS = (22 * 20)
    AAT = 2

    'Erase batRecVal!, batRecTmVal!, pitRecTmVal!
    'Erase batTotGames!, pitTotGames!

    'Erase batterNames$, pitcherNames$
    'Erase batRecPlyr$, batRecTeam$, batRecTmDesc$
    'Erase pitRecPlyr$, pitRecTeam$, pitRecTmDesc$

    'Call ReadTeamStats(teamName$, statName$)

    ' Calculate records for current team
    Locate 7: Print "CALCULATING BATTER RECORDS . . . ."

    ' ** Recorded Stats **
    For I = 0 To 22

        I2 = I + 1

        For I1 = 1 To 20

            'I2 = I1 - 1

            If leaderStatVal2!(I2, I1) >= batRecVal!(I1) Then
                batRecVal!(I1) = leaderStatVal2!(I2, I1)
                batRecPlyr$(I1) = leaderNames$(I2)
                batRecTeam$(I1) = teamName$
                'batRecYr!(I1) = seasonYear
            End If

        Next I1

        AAG = AAG - 1
        Locate 7, 40: Print Using "###"; AAG

    Next I

    'Process stats that require calculations
    If UCase$(finalUpdate$) = "Y" Then

        ' ** Batting AVG **
        For I = 0 To 22

            I2 = I + 1

            If leaderStatVal2!(I2, 1) + leaderStatVal2!(I2, 8) >= batTotGames!(I) * 3.1 Then
                minimumPA = 1
            Else
                minimumPA = 0
            End If

            If minimumPA = 1 And leaderStatVal2!(I2, 1) <> 0 Then

                ' ** Batting Avg. **
                If leaderStatVal2!(I2, 2) / leaderStatVal2!(I2, 1) >= batRecVal!(21) Then
                    batRecVal!(21) = leaderStatVal2!(I2, 2) / leaderStatVal2!(I2, 1)
                    batRecPlyr$(21) = leaderNames$(I2)
                    batRecTeam$(21) = teamName$
                    'batRecYr!(21) = seasonYear
                End If

                ' ** Slugging % **
                slugPCT! = (leaderStatVal2!(I2, 2) + leaderStatVal2!(I2, 5) + 2 * leaderStatVal2!(I2, 6) + 3 * leaderStatVal2!(I2, 7)) / leaderStatVal2!(I2, 1)
                If slugPCT! >= batRecVal!(22) Then
                    batRecVal!(22) = slugPCT!
                    batRecPlyr$(22) = leaderNames$(I2)
                    batRecTeam$(22) = teamName$
                    'batRecYr!(22) = seasonYear
                End If

                ' ** OB% **
                obPCT! = (leaderStatVal2!(I2, 2) + leaderStatVal2!(I2, 8)) / (leaderStatVal2!(I2, 1) + leaderStatVal2!(I2, 8))
                If obPCT! >= batRecVal!(23) Then
                    batRecVal!(23) = obPCT!
                    batRecPlyr$(23) = leaderNames$(I2)
                    batRecTeam$(23) = teamName$
                    'batRecYr!(23) = seasonYear
                End If

            End If

            ' ** Triples **
            If minimumPA = 1 And leaderStatVal2!(I2, 7) <> 0 Then

                If leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 7) <= batRecVal!(24) Then
                    batRecVal!(24) = leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 7)
                    batRecPlyr$(24) = leaderNames$(I2)
                    batRecTeam$(24) = teamName$
                    'batRecYr!(24) = seasonYear
                End If

            End If

            ' ** Strikeout Rate **
            If minimumPA = 1 And leaderStatVal2!(I2, 9) <> 0 Then

                If leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 9) <= batRecVal!(25) Then
                    batRecVal!(25) = leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 9)
                    batRecPlyr$(25) = leaderNames$(I2)
                    batRecTeam$(25) = teamName$
                    'batRecYr!(25) = seasonYear
                End If

            End If

            ' ** Walk Rate **
            If minimumPA = 1 And leaderStatVal2!(I2, 8) <> 0 Then

                If leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 8) > batRecVal!(26) Then
                    batRecVal!(26) = leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 8)
                    batRecPlyr$(26) = leaderNames$(I2)
                    batRecTeam$(26) = teamName$
                    'batRecYr!(26) = seasonYear
                End If

            End If

            ' ** RBI Rate **
            If minimumPA = 1 And leaderStatVal2!(I2, 4) <> 0 Then

                If leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 4) <= batRecVal!(27) Then
                    batRecVal!(27) = leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 4)
                    batRecPlyr$(27) = leaderNames$(I2)
                    batRecTeam$(27) = teamName$
                    'batRecYr!(27) = seasonYear
                End If

            End If

            ' ** Hit Rate **
            If minimumPA = 1 And leaderStatVal2!(I2, 3) <> 0 Then

                If leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 3) <= batRecVal!(28) Then
                    batRecVal!(28) = leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 3)
                    batRecPlyr$(28) = leaderNames$(I2)
                    batRecTeam$(28) = teamName$
                    'batRecYr!(28) = seasonYear
                End If

            End If

            ' ** Error Rate **
            If minimumPA = 1 And leaderStatVal2!(I2, 16) <> 0 Then

                If leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 16) >= batRecVal!(29) Then
                    batRecVal!(29) = leaderStatVal2!(I2, 1) / leaderStatVal2!(I2, 16)
                    batRecPlyr$(29) = leaderNames$(I2)
                    batRecTeam$(29) = teamName$
                    'batRecYr!(29) = seasonYear
                End If

            End If

            ' ** Stolen Base Attempts **
            If leaderStatVal2!(I2, 21) + leaderStatVal2!(I2, 22) >= batRecVal!(30) Then
                batRecVal!(30) = leaderStatVal2!(I2, 21) + leaderStatVal2!(I2, 22)
                batRecPlyr$(30) = leaderNames$(I2)
                batRecTeam$(30) = teamName$
                'batRecYr!(30) = seasonYear
            End If

            ' ** Caught Stealing **
            If leaderStatVal2!(I2, 22) >= batRecVal!(31) Then
                batRecVal!(31) = leaderStatVal2!(I2, 22)
                batRecPlyr$(31) = leaderNames$(I2)
                batRecTeam$(31) = teamName$
                'batRecYr!(31) = seasonYear
            End If

            ' ** Caught Stealing % **
            If leaderStatVal2!(I2, 1) + leaderStatVal2!(I2, 8) >= batTotGames!(I) * 2.1 Then
                min21PA = 1
            Else
                min21PA = 0
            End If

            If min21PA = 1 And leaderStatVal2!(I2, 21) + leaderStatVal2!(I2, 22) <> 0 Then

                If leaderStatVal2!(I2, 22) / (leaderStatVal2!(I2, 21) + leaderStatVal2!(I2, 22)) >= batRecVal!(32) Then
                    batRecVal!(32) = leaderStatVal2!(I2, 22) / (leaderStatVal2!(I2, 21) + leaderStatVal2!(I2, 22))
                    batRecPlyr$(32) = leaderNames$(I2)
                    batRecTeam$(32) = teamName$
                    'batRecYr!(32) = seasonYear
                End If

            End If

        Next I

    End If

    Locate 8: Print "CALCULATING PITCHER RECORDS . . . ."

    ' ** Recorded Stats **
    For I = 0 To 21

        I2 = I + 25

        For I1 = 1 To 23

            'I2 = I1 - 1

            If leaderStatVal2!(I2, I1) >= pitRecVal!(I1) Then
                pitRecVal!(I1) = leaderStatVal2!(I2, I1)
                pitRecPlyr$(I1) = leaderNames$(I2)
                pitRecTeam$(I1) = teamName$
                'pitRecYr!(I1) = seasonYear
            End If

        Next I1

        AAS = AAS - 1
        Locate 8, 40: Print Using "###"; AAS

    Next I

    'Process stats that require calculations
    If UCase$(finalUpdate$) = "Y" Then

        For I = 0 To 21

            I2 = I + 25

            ' ** IRA % **
            If leaderStatVal2!(I2, 18) <> 0 And leaderStatVal2!(I2, 9) >= 25 Then

                If leaderStatVal2!(I2, 19) / leaderStatVal2!(I2, 18) <= pitRecVal!(24) Then
                    pitRecVal!(24) = leaderStatVal2!(I2, 19) / leaderStatVal2!(I2, 18)
                    pitRecPlyr$(24) = leaderNames$(I2)
                    pitRecTeam$(24) = teamName$
                    'pitRecYr!(24) = seasonYear
                End If

            End If

            ' ** Runs / Game **
            If leaderStatVal2!(I2, 1) / 3 >= pitTotGames!(I) And leaderStatVal2!(I2, 21) <> 0 Then

                If leaderStatVal2!(I2, 21) / leaderStatVal2!(I2, 9) < pitRecVal!(25) Then
                    pitRecVal!(25) = leaderStatVal2!(I2, 21) / leaderStatVal2!(I2, 9)
                    pitRecPlyr$(25) = leaderNames$(I2)
                    pitRecTeam$(25) = teamName$
                    'pitRecYr!(25) = seasonYear
                End If

            End If

            If leaderStatVal2!(I2, 1) / 3 >= pitTotGames!(I) And leaderStatVal2!(I2, 1) <> 0 Then

                ' ** ERA **
                If (leaderStatVal2!(I2, 4) * 9) / (leaderStatVal2!(I2, 1) / 3) < pitRecVal!(23) Then
                    pitRecVal!(23) = (leaderStatVal2!(I2, 4) * 9) / (leaderStatVal2!(I2, 1) / 3)
                    pitRecPlyr$(23) = leaderNames$(I2)
                    pitRecTeam$(23) = teamName$
                    'pitRecYr!(23) = seasonYear
                End If

                ' ** Walks / 9 Innings **
                If (leaderStatVal2!(I2, 5) * 9) / (leaderStatVal2!(I2, 1) / 3) < pitRecVal!(25) Then
                    pitRecVal!(25) = (leaderStatVal2!(I2, 5) * 9) / (leaderStatVal2!(I2, 1) / 3)
                    pitRecPlyr$(25) = leaderNames$(I2)
                    pitRecTeam$(25) = teamName$
                    'pitRecYr!(25) = seasonYear
                End If

                ' ** Strikeouts / 9 Innings **
                If (leaderStatVal2!(I2, 6) * 9) / (leaderStatVal2!(I2, 1) / 3) > pitRecVal!(26) Then
                    pitRecVal!(26) = (leaderStatVal2!(I2, 6) * 9) / (leaderStatVal2!(I2, 1) / 3)
                    pitRecPlyr$(26) = leaderNames$(I2)
                    pitRecTeam$(26) = teamName$
                    'pitRecYr!(26) = seasonYear
                End If

                ' ** Hits / 9 Innings **
                If (leaderStatVal2!(I2, 2) * 9) / (leaderStatVal2!(I2, 1) / 3) < pitRecVal!(27) Then
                    pitRecVal!(27) = (leaderStatVal2!(I2, 2) * 9) / (leaderStatVal2!(I2, 1) / 3)
                    pitRecPlyr$(27) = leaderNames$(I2)
                    pitRecTeam$(27) = teamName$
                    'pitRecYr!(27) = seasonYear
                End If

                ' ** Double-Plays / 9 Innings **
                If (leaderStatVal2!(I2, 23) * 9) / (leaderStatVal2!(I2, 1) / 3) > pitRecVal!(28) Then
                    pitRecVal!(28) = (leaderStatVal2!(I2, 23) * 9) / (leaderStatVal2!(I2, 1) / 3)
                    pitRecPlyr$(28) = leaderNames$(I2)
                    pitRecTeam$(28) = teamName$
                    'pitRecYr!(28) = seasonYear
                End If

            End If

        Next I

    End If


    Locate 9: Print "CALCULATING TEAM RECORDS . . . ."

    ' ** Recorded Stats **
    For I1 = 0 To 21

        I2 = I1 + 3

        If ldrBatStat!(I2) > batRecTmVal!(I1) Then
            batRecTmVal!(I1) = ldrBatStat!(I2)
            batRecTmDesc$(I1) = teamName$ 'TT$(I, I1)
            'batRecTmYr!(I1) = seasonYear
        End If

    Next I1

    AAT = AAT - 1
    Locate 9, 40: Print Using "###"; AAT

    'Process stats that require calculations
    If UCase$(finalUpdate$) = "Y" Then

        If ldrBatStat!(1) <> 0 Then

            'Batting AVG
            If ldrBatStat!(4) / ldrBatStat!(3) > batRecTmVal!(22) Then
                batRecTmVal!(22) = ldrBatStat!(4) / ldrBatStat!(3)
                batRecTmDesc$(22) = teamName$
                'batRecTmYr!(22) = seasonYear
            End If

            'Slugging %
            slugPCT! = (ldrBatStat!(4) + ldrBatStat!(7) + (ldrBatStat!(8) * 2) + (ldrBatStat!(9) * 3)) / ldrBatStat!(3)
            If slugPCT! > batRecTmVal!(23) Then
                batRecTmVal!(23) = slugPCT!
                batRecTmDesc$(23) = teamName$
                'batRecTmYr!(23) = seasonYear
            End If

            'OB %
            obPCT! = (ldrBatStat!(4) + ldrBatStat!(10)) / (ldrBatStat!(3) + ldrBatStat!(10))
            'Why don't we use HBP / SF here? do we even sum those for a team?
            If obPCT! > batRecTmVal!(24) Then
                batRecTmVal!(24) = obPCT!
                batRecTmDesc$(24) = teamName$
                'batRecTmYr!(24) = seasonYear
            End If

        End If

    End If

    ' ** Recorded Stats **
    For I1 = 0 To 22

        I2 = I1 + 1

        If ldrPitStat!(I2) > pitRecTmVal!(I1) Then
            pitRecTmVal!(I1) = ldrPitStat!(I2)
            pitRecTmDesc$(I1) = teamName$ 'TT1$(I, I1)
            'pitRecTmYr!(I1) = seasonYear
        End If

    Next I1

    AAT = AAT - 1
    Locate 9, 40: Print Using "###"; AAT

    'Process stats that require calculations
    If UCase$(finalUpdate$) = "Y" Then

        For I = 1 To TEAMS_PER_LEAGUE

            'IRA %
            If ldrPitStat!(17) <> 0 Then

                If ldrPitStat!(18) / ldrPitStat!(17) > pitRecTmVal!(23) Then
                    pitRecTmVal!(23) = ldrPitStat!(18) / ldrPitStat!(17)
                    pitRecTmDesc$(23) = teamName$
                    'pitRecTmYr!(23) = seasonYear
                End If

            End If

            'ER / 9 Innings
            If ldrPitStat!(1) <> 0 Then

                If (ldrPitStat!(3) * 9) / (ldrPitStat!(1) / 3) < pitRecTmVal!(24) Then
                    pitRecTmVal!(24) = (ldrPitStat!(3) * 9) / (ldrPitStat!(1) / 3)
                    pitRecTmDesc$(24) = teamName$
                    'pitRecTmYr!(24) = seasonYear
                End If

            End If

        Next I

    End If 'Done checking for final update

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
Sub PostLeaders (leaderFile$, nbrAppearances)

    Shared divWins(), divLosses()

    Shared leaderVals!(), DFT!(), OFT!()

    Shared BA$(), leagLdrPlayer$(), leagLdrTeam$()
    Shared ER$(), memberName$()

    Color 15, 0
    Cls

    Print "POSTING "; leaderFile; " LEADERS"

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

    Write #2, nbrAppearances

    Close 2

End Sub


'----------------------------------------
'        PostRecordsPro Subroutine
'----------------------------------------
'This subroutine writes the calculated
'records to the records file
Sub PostRecordsPro

    Shared batRecVal!(), batRecTmVal!()
    Shared pitRecVal!(), pitRecTmVal!()

    Shared batRecPlyr$(), batRecTeam$(), batRecTmDesc$()
    Shared pitRecPlyr$(), pitRecTeam$(), pitRecTmDesc$()

    Open diskPaths$(2) + "RECORDS.MLB" For Output As #1

    For I = 1 To 32:
        Write #1, batRecVal!(I), batRecPlyr$(I), batRecTeam$(I)
        'batRecYr!(I)
    Next

    For I = 1 To 28:
        Write #1, pitRecVal!(I), pitRecPlyr$(I), pitRecTeam$(I)
        'pitRecYr!(I)
    Next

    For I = 0 To 24:
        Write #1, batRecTmVal!(I), batRecTmDesc$(I)
        'batRecTmYr!(I)
    Next

    For I = 0 To 24:
        Write #1, pitRecTmVal!(I), pitRecTmDesc$(I)
        'pitRecTmYr!(I)
    Next

    Close #1

End Sub
