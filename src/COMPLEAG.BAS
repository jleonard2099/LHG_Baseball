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

    Shared BAT%(), PIT%()
    Shared O1(), O2()

    Shared compileZ1!(), DFT!(), JS!(), OFT!()

    Shared BA$(), compileTeams$(), compileZ2$(), compileZ3$()
    Shared ER$(), NM$()
    Shared statFileNames$()

    'Initialize values first
    APP = 0

    Erase BA$, ER$
    Erase compileZ2$, compileZ3$
    Erase compileZ1!
    Erase DFT!, OFT!

    For Z = 1 To TEAMS_PER_LEAGUE
        DFT!(Z, 1) = 99.99
    Next

    For X = 120 To 139
        compileZ2$(X) = "*"
        compileZ3$(X) = "*"
        compileZ1!(X, 1) = 99.99
        compileZ1!(X, 2) = 0
    Next X

    For X = 239 To 258
        compileZ2$(X) = "*"
        compileZ3$(X) = "*"
        compileZ1!(X, 1) = 99.99
        compileZ1!(X, 2) = 99.99
    Next X

    C$ = ","

    leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + leagStatFile$ + ".TMS" For Input As #2

        Input #2, league$, div1Name$, div2$, div3$, div4$

        For XW = 1 To TEAMS_PER_LEAGUE
            Input #2, statFileNames$(XW)
            compileTeams$(XW) = statFileNames$(XW)
        Next

        Close 2

        For XW = 1 To TEAMS_PER_LEAGUE

            fullWins = 0
            fullLosses = 0

            If statFileNames$(XW) <> Chr$(42) Then

                haveStats = FindStatsPro%(statFileNames$(XW), TN$, fullWins, fullLosses)

                If (haveStats) Then

                    L = Len(statFileNames$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(statFileNames$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    confGame = 0

                    For X = 1 To TEAMS_PER_LEAGUE:

                        If compileTeams$(X) = WT$ Then

                            If O1(X) = fullWins And O2(X) = fullLosses Then
                                confGame = 1
                            Else
                                O1(X) = fullWins
                                O2(X) = fullLosses
                            End If

                        End If

                        If (confGame) Then Exit For

                    Next X

                    If confGame <> 1 And _FileExists(diskPaths$(1) + statFileNames$(XW)) Then

                        Call ReadStatsLeague(statFileNames$(XW), TN$, totalGames)
                        Call CheckStatsLeague(statFileNames$(XW), WT$, totalGames)
                        Call CalcStatTotals(WT$, totalGames)

                    End If 'Done checking if we need to skip

                End If

            End If

        Next XW

        Call SortStandingsLeague
        Call PostLeaders(league$, leagStatFile$, APP)

    End If

End Sub


'----------------------------------------
'         FindStatsPro Function
'----------------------------------------
'...explanation...
Function FindStatsPro% (statFile$, teamName$, fullWins, fullLosses)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsPro% = 0

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

        FindStatsPro% = 1

    End If

End Function


'----------------------------------------
'       ReadStatsLeague Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the league
Sub ReadStatsLeague (statFile$, teamName$, totalGames)

    Dim ZM%(46)

    Shared BAT%(), PIT%()

    Shared JS!()

    Shared NM$()

    Erase NM$
    Erase JS!
    Erase BAT%, PIT%

    Open (diskPaths$(1) + statFile$) For Input As #1

    Input #1, teamName$, fullWins, fullLosses

    For K = 1 To 23
        Input #1, NM$(K), ZM%(K)
    Next

    For K = 25 To 46
        Input #1, NM$(K), ZM%(K)
    Next

    NM$(24) = "PITCHERS"

    For J = 1 To 23
        For K = 1 To 22
            Input #1, JS!(J, K)
        Next K
    Next J

    For J = 25 To 46
        For K = 1 To 42
            Input #1, JS!(J, K)
        Next K
    Next J

    For K = 1 To 24
        Input #1, BAT%(K)
    Next K

    For K = 1 To 23
        Input #1, PIT%(K)
    Next K

    Close #1

    totalGames = fullWins + fullLosses

    'Extra calculations for batters
    For J = 1 To 23

        If JS!(J, 1) = 0 Then JS!(J, 1) = 1

        JS!(J, 17) = JS!(J, 13)
        JS!(J, 18) = JS!(J, 14)

        ' Must have 3.1 Min At Bats / Game
        '-- Previously compared to 2.5
        '-- Header for Viewing Leader says 3.1
        If (JS!(J, 8) + JS!(J, 1)) / totalGames < 3.1 Then
            JS!(J, 14) = 0
            JS!(J, 15) = 0
            JS!(J, 16) = 0
        Else
            JS!(J, 16) = JS!(J, 2) / JS!(J, 1)
            JS!(J, 14) = (JS!(J, 2) + JS!(J, 5) + 2 * JS!(J, 6) + 3 * JS!(J, 7)) / JS!(J, 1)
            JS!(J, 15) = (JS!(J, 2) + JS!(J, 8)) / (JS!(J, 1) + JS!(J, 8))
        End If

    Next

    'Must have 1 IP / Game
    '(JS!(J, 1) / 3) = IP
    For J = 25 To 46

        If JS!(J, 1) = 0 Or (JS!(J, 1) / 3) < totalGames Then
            JS!(J, 14) = 99.99
        Else
            JS!(J, 14) = JS!(J, 4) / JS!(J, 1) * 27
        End If

    Next

End Sub


'----------------------------------------
'      CheckStatsLeague Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsLeague (targetTeam$, WT$, totalGames)

    Dim PS$(TEAMS_PER_LEAGUE)

    Shared NM$()
    Shared compileZ2$(), compileZ3$()
    Shared compileZ1!(), DFT!(), JS!(), OFT!()

    Color 15, 0
    Cls

    'Color 12
    Print "CHECKING "; targetTeam$; " STATS"

    Color 15

    For X = 1 To 23

        If NM$(X) <> "XXX" Then

            'Color 14
            Print "CHECKING STATS OF ";
            Color 15
            Print NM$(X)

            PS$(X) = WT$

            skipIF = 0

            'Batting Avg
            For Y = 1 To 20

                If compileZ2$(Y) = NM$(X) And compileZ3$(Y) = WT$ Then

                    compileZ1!(Y, 1) = JS!(X, 13)
                    compileZ1!(Y, 2) = JS!(X, 1)

                    ' Must have 3.1 Min At Bats / Game
                    '-- Previously compared to 2.5
                    '-- Header for Viewing Leader says 3.1
                    If compileZ1!(Y, 2) < 3.1 * totalGames Then
                        compileZ2$(Y) = "*"
                        compileZ3$(Y) = "*"
                        compileZ1!(Y, 1) = 0
                        compileZ1!(Y, 2) = 0
                    End If

                    For Y = 1 To 19
                        For Z = Y + 1 To 20
                            If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                                Swap compileZ3$(Y), compileZ3$(Z)
                                Swap compileZ2$(Y), compileZ2$(Z)
                                For Z1 = 1 To 2:
                                    Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3318

                End If

            Next Y

            If JS!(X, 16) >= compileZ1!(20, 1) Then

                compileZ2$(20) = NM$(X)
                compileZ3$(20) = PS$(X)
                compileZ1!(20, 1) = JS!(X, 16)
                compileZ1!(20, 2) = JS!(X, 1)

                For Y = 1 To 19
                    For Z = Y + 1 To 20
                        If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                            Swap compileZ2$(Y), compileZ2$(Z)
                            Swap compileZ3$(Y), compileZ3$(Z)
                            For Z1 = 1 To 2:
                                Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3318 '
            ' Must have 3.1 Min At Bats / Game
            '-- Previously compared to 2.5
            '-- Header for Viewing Leader says 3.1
            If (JS!(X, 8) + JS!(X, 1)) / totalGames >= 3.1 Then

                If JS!(X, 16) <> 0 And JS!(X, 16) <= compileZ1!(258, 1) Then

                    compileZ2$(258) = NM$(X)
                    compileZ3$(258) = PS$(X)
                    compileZ1!(258, 1) = JS!(X, 16)
                    compileZ1!(258, 2) = JS!(X, 1)

                    For Y = 239 To 257
                        For Z = Y + 1 To 258
                            If compileZ1!(Y, 1) > compileZ1!(Z, 1) Then
                                Swap compileZ2$(Y), compileZ2$(Z)
                                Swap compileZ3$(Y), compileZ3$(Z)
                                For Z1 = 1 To 2:
                                    Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                End If

            End If

            3320 '
            'Slugging Pct
            For Y = 99 To 108

                If compileZ2$(Y) = NM$(X) And compileZ3$(Y) = WT$ Then

                    compileZ1!(Y, 1) = JS!(X, 14)
                    compileZ1!(Y, 2) = JS!(X, 1)

                    ' Must have 3.1 Min At Bats / Game
                    '-- Previously compared to 2.5
                    '-- Header for Viewing Leader says 3.1
                    If compileZ1!(Y, 2) < totalGames * 3.1 Then
                        compileZ2$(Y) = "*"
                        compileZ3$(Y) = "*"
                        compileZ1!(Y, 1) = 0
                        compileZ1!(Y, 2) = 0
                    End If

                    For Y = 99 To 107

                        For Z = Y + 1 To 108
                            If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                                Swap compileZ2$(Y), compileZ2$(Z)
                                Swap compileZ3$(Y), compileZ3$(Z)
                                For Z1 = 1 To 2:
                                    Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z

                    Next Y

                    GoTo 3325

                End If

            Next Y

            If JS!(X, 14) >= compileZ1!(108, 1) Then

                compileZ2$(108) = NM$(X)
                compileZ3$(108) = PS$(X)
                compileZ1!(108, 1) = JS!(X, 14)
                compileZ1!(108, 2) = JS!(X, 1)

                compileZ1!(Y, 1) = JS!(X, 14)
                compileZ1!(Y, 2) = JS!(X, 1)

                ' Must have 3.1 Min At Bats / Game
                '-- Previously compared to 2.5
                '-- Header for Viewing Leader says 3.1
                If compileZ1!(Y, 2) < totalGames * 3.1 Then
                    compileZ2$(Y) = "*"
                    compileZ3$(Y) = "*"
                    compileZ1!(Y, 1) = 0
                    compileZ1!(Y, 2) = 0
                End If

                For Y = 99 To 107
                    For Z = Y + 1 To 108
                        If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                            Swap compileZ2$(Y), compileZ2$(Z)
                            Swap compileZ3$(Y), compileZ3$(Z)
                            For Z1 = 1 To 2:
                                Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3325 '
            'On Base Pct
            For Y = 110 To 119

                If compileZ2$(Y) = NM$(X) And compileZ3$(Y) = WT$ Then

                    compileZ1!(Y, 1) = JS!(X, 15)
                    compileZ1!(Y, 2) = JS!(X, 1)

                    ' Must have 3.1 Min PA / Game
                    '-- Previously compared to 2.5
                    '-- When viewing leaders, header says Min 3.1
                    If compileZ1!(Y, 2) < totalGames * 3.1 Then
                        compileZ2$(Y) = "*"
                        compileZ3$(Y) = "*"
                        compileZ1!(Y, 1) = 0
                        compileZ1!(Y, 2) = 0
                    End If

                    For Y = 110 To 118
                        For Z = Y + 1 To 119
                            If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                                Swap compileZ2$(Y), compileZ2$(Z)
                                Swap compileZ3$(Y), compileZ3$(Z)
                                For Z1 = 1 To 2:
                                    Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3330

                End If

            Next Y

            3327 '
            If JS!(X, 15) >= compileZ1!(119, 1) Then

                compileZ2$(119) = NM$(X)
                compileZ3$(119) = PS$(X)
                compileZ1!(119, 1) = JS!(X, 15)
                compileZ1!(119, 2) = JS!(X, 1)

                compileZ1!(Y, 1) = JS!(X, 15)
                compileZ1!(Y, 2) = JS!(X, 1)

                ' Must have 3.1 Min PA / Game
                '-- Previously compared to 2.5
                '-- When viewing leaders, header says Min 3.1
                If compileZ1!(Y, 2) < totalGames * 3.1 Then
                    compileZ2$(Y) = "*"
                    compileZ3$(Y) = "*"
                    compileZ1!(Y, 1) = 0
                    compileZ1!(Y, 2) = 0
                End If

                For Y = 110 To 118
                    For Z = Y + 1 To 119
                        If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                            Swap compileZ2$(Y), compileZ2$(Z)
                            Swap compileZ3$(Y), compileZ3$(Z)
                            For Z1 = 1 To 2
                                Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3330 '
            For Z = 21 To 31

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 2)
                    GoTo 3335
                End If

            Next Z

            If JS!(X, 2) > compileZ1!(31, 1) Then

                compileZ1!(31, 1) = JS!(X, 2)
                compileZ2$(31) = NM$(X)
                compileZ3$(31) = WT$

                3335 '
                For Z = 21 To 30
                    For Z1 = Z + 1 To 31
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            'Division 4
            For Z = 32 To 42

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 5)
                    GoTo 3347
                End If

            Next Z

            If JS!(X, 5) > compileZ1!(42, 1) Then

                compileZ1!(42, 1) = JS!(X, 5)
                compileZ2$(42) = NM$(X)
                compileZ3$(42) = WT$

                3347 '
                For Z = 32 To 41
                    For Z1 = Z + 1 To 42

                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

            3354 '
            For Z = 43 To 53
                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 6)
                    GoTo 3359
                End If
            Next Z

            If JS!(X, 6) > compileZ1!(53, 1) Then

                compileZ1!(53, 1) = JS!(X, 6)
                compileZ2$(53) = NM$(X)
                compileZ3$(53) = WT$

                3359 '
                For Z = 43 To 52
                    For Z1 = Z + 1 To 53
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 54 To 64
                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 7)
                    GoTo 3371
                End If
            Next Z

            If JS!(X, 7) > compileZ1!(64, 1) Then

                compileZ1!(64, 1) = JS!(X, 7)
                compileZ2$(64) = NM$(X)
                compileZ3$(64) = WT$

                3371 '
                For Z = 54 To 63:
                    For Z1 = Z + 1 To 64:
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3378 '
            For Z = 65 To 74

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 3)
                    GoTo 3383
                End If

            Next Z

            If JS!(X, 3) > compileZ1!(74, 1) Then

                compileZ1!(74, 1) = JS!(X, 3)
                compileZ2$(74) = NM$(X)
                compileZ3$(74) = WT$

                3383 '
                For Z = 65 To 73
                    For Z1 = Z + 1 To 74
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 76 To 85
                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 4)
                    GoTo 3395
                End If
            Next Z

            If JS!(X, 4) > compileZ1!(85, 1) Then

                compileZ1!(85, 1) = JS!(X, 4)
                compileZ2$(85) = NM$(X)
                compileZ3$(85) = WT$

                3395 '
                For Z = 76 To 84
                    For Z1 = Z + 1 To 85
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

            For Z = 87 To 97

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 10)
                    GoTo 3407
                End If
            Next Z

            If JS!(X, 10) > compileZ1!(97, 1) Then

                compileZ1!(97, 1) = JS!(X, 10)
                compileZ2$(97) = NM$(X)
                compileZ3$(97) = WT$

                3407 '
                For Z = 87 To 96
                    For Z1 = Z + 1 To 97
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If 'Done checking for XXX / terminating chars

        3414 '
        'Base on Balls
        For Z = 217 To 227

            If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                compileZ1!(Z, 1) = JS!(X, 8)
                GoTo 3605
            End If

        Next Z

        If JS!(X, 8) > compileZ1!(227, 1) Then

            compileZ1!(227, 1) = JS!(X, 8)
            compileZ2$(227) = NM$(X)
            compileZ3$(227) = WT$

            3605 '
            For Z = 217 To 226
                For Z1 = Z + 1 To 227
                    If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                        Swap compileZ2$(Z1), compileZ2$(Z)
                        Swap compileZ3$(Z1), compileZ3$(Z)
                        Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                    End If
                Next Z1
            Next Z
        End If

        'Hitting Streak
        For Z = 279 To 288

            If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                compileZ1!(Z, 1) = JS!(X, 17)
                GoTo 3615
            End If

        Next Z

        If JS!(X, 17) > compileZ1!(288, 1) Then

            compileZ1!(288, 1) = JS!(X, 17)
            compileZ2$(288) = NM$(X)
            compileZ3$(288) = WT$

            3615 '
            For Z = 279 To 287
                For Z1 = Z + 1 To 288:

                    If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                        Swap compileZ2$(Z1), compileZ2$(Z)
                        Swap compileZ3$(Z1), compileZ3$(Z)
                        Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                    End If

                Next Z1
            Next Z

        End If

        'Longest Hitting Streak
        For Z = 289 To 298
            3621 '
            If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                compileZ1!(Z, 1) = JS!(X, 18)
                GoTo 3625
            End If

        Next Z

        If JS!(X, 18) > compileZ1!(298, 1) Then

            compileZ1!(298, 1) = JS!(X, 18)
            compileZ2$(298) = NM$(X)
            compileZ3$(298) = WT$

            3625 '
            For Z = 289 To 297
                For Z1 = Z + 1 To 298

                    If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                        Swap compileZ2$(Z1), compileZ2$(Z)
                        Swap compileZ3$(Z1), compileZ3$(Z)
                        Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
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
Sub CalcStatTotals (WT$, totalGames)

    Shared BAT%(), PIT%()
    Shared compileZ2$(), compileZ3$()
    Shared BA$(), ER$(), NM$()
    Shared compileZ1!(), DFT!(), JS!(), OFT!()

    3415 '
    'All Pitchers
    For X = 25 To 46

        If NM$(X) <> "XXX" Then

            'Color 14
            Print "CHECKING STATS OF ";
            Color 15
            Print NM$(X)

            If JS!(X, 14) <= compileZ1!(139, 1) Then

                compileZ2$(139) = NM$(X)
                compileZ3$(139) = WT$
                compileZ1!(139, 1) = JS!(X, 14)
                compileZ1!(139, 2) = JS!(X, 1) / 3

                For Y = 121 To 138
                    For Z = Y + 1 To 139
                        If compileZ1!(Y, 1) > compileZ1!(Z, 1) Then
                            Swap compileZ2$(Y), compileZ2$(Z)
                            Swap compileZ3$(Y), compileZ3$(Z)
                            For Z1 = 1 To 2
                                Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            'Must have at least 1 IP / Game
            If (JS!(X, 1) / 3) >= totalGames And JS!(X, 1) <> 0 Then

                If JS!(X, 14) < 99.9 And JS!(X, 14) >= compileZ1!(278, 1) Then

                    compileZ2$(278) = NM$(X)
                    compileZ3$(278) = WT$
                    compileZ1!(278, 1) = JS!(X, 14)
                    compileZ1!(278, 2) = JS!(X, 1) / 3

                    For Y = 259 To 277
                        For Z = Y + 1 To 278
                            If compileZ1!(Y, 1) < compileZ1!(Z, 1) Then
                                Swap compileZ2$(Y), compileZ2$(Z)
                                Swap compileZ3$(Y), compileZ3$(Z)
                                For Z1 = 1 To 2
                                    Swap compileZ1!(Y, Z1), compileZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                End If

            End If

            3430 '
            For Y = 140 To 150

                If compileZ2$(Y) = NM$(X) And compileZ3$(Y) = WT$ Then

                    compileZ1!(Y, 1) = JS!(X, 7)
                    compileZ1!(Y, 2) = JS!(X, 8)

                    '4860 '
                    For Z = 140 To 149:

                        For Y1 = Z + 1 To 150

                            If compileZ1!(Z, 1) - compileZ1!(Z, 2) <= compileZ1!(Y1, 1) - compileZ1!(Y1, 2) Then

                                If compileZ1!(Z, 1) - compileZ1!(Z, 2) <> compileZ1!(Y1, 1) - compileZ1!(Y1, 2) Or compileZ1!(Z, 1) > compileZ1!(Y1, 1) Then
                                    Swap compileZ2$(Y1), compileZ2$(Z)
                                    Swap compileZ3$(Y1), compileZ3$(Z)
                                    For Z1 = 1 To 2:
                                        Swap compileZ1!(Y1, Z1), compileZ1!(Z, Z1)
                                    Next Z1
                                End If

                            End If

                        Next Y1

                    Next Z

                    GoTo 3440

                End If

            Next Y

            If JS!(X, 7) - JS!(X, 8) >= compileZ1!(150, 1) - compileZ1!(150, 2) Then

                If Not (JS!(X, 7) - JS!(X, 8) = compileZ1!(150, 1) - compileZ1!(150, 2) And JS!(X, 7) >= compileZ1!(150, 1)) Then

                    compileZ2$(150) = NM$(X)
                    compileZ3$(150) = WT$
                    compileZ1!(150, 1) = JS!(X, 7)
                    compileZ1!(150, 2) = JS!(X, 8)

                    '4860 '
                    For Z = 140 To 149:
                        For Y1 = Z + 1 To 150

                            If compileZ1!(Z, 1) - compileZ1!(Z, 2) <= compileZ1!(Y1, 1) - compileZ1!(Y1, 2) Then

                                If compileZ1!(Z, 1) - compileZ1!(Z, 2) <> compileZ1!(Y1, 1) - compileZ1!(Y1, 2) Or compileZ1!(Z, 1) > compileZ1!(Y1, 1) Then
                                    Swap compileZ2$(Y1), compileZ2$(Z)
                                    Swap compileZ3$(Y1), compileZ3$(Z)
                                    For Z1 = 1 To 2:
                                        Swap compileZ1!(Y1, Z1), compileZ1!(Z, Z1)
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

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then

                    compileZ1!(Z, 1) = JS!(X, 7)
                    '3445

                    For Z = 151 To 160
                        For Z1 = Z + 1 To 161
                            If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                                Swap compileZ2$(Z1), compileZ2$(Z)
                                Swap compileZ3$(Z1), compileZ3$(Z)
                                Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                            End If
                        Next Z1
                    Next Z

                    GoTo 3445

                End If

            Next Z

            If JS!(X, 7) > compileZ1!(161, 1) Then

                compileZ1!(161, 1) = JS!(X, 7)
                compileZ2$(161) = NM$(X)
                compileZ3$(161) = WT$

                3445 '
                For Z = 151 To 160

                    For Z1 = Z + 1 To 161

                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If

                    Next Z1

                Next Z

            End If

            3452 '
            For Z = 162 To 172

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 1) / 3
                    GoTo 3457
                End If

            Next Z

            If JS!(X, 1) / 3 > compileZ1!(172, 1) Then

                compileZ1!(172, 1) = JS!(X, 1) / 3
                compileZ2$(172) = NM$(X)
                compileZ3$(172) = WT$

                3457 '
                For Z = 162 To 171
                    For Z1 = Z + 1 To 172
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3464 '
            For Z = 173 To 183
                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 6)
                    GoTo 3469
                End If
            Next Z

            If JS!(X, 6) > compileZ1!(183, 1) Then

                compileZ1!(183, 1) = JS!(X, 6)
                compileZ2$(183) = NM$(X)
                compileZ3$(183) = WT$

                3469 '
                For Z = 173 To 182:
                    For Z1 = Z + 1 To 183:
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

                '3474 FOR Z = 173 TO 183
                '3475 NEXT Z
            End If

            3476 '
            For Z = 184 To 194
                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 11)
                    GoTo 3481
                End If
            Next Z

            If JS!(X, 11) >= compileZ1!(194, 1) Then

                compileZ1!(194, 1) = JS!(X, 11)
                compileZ2$(194) = NM$(X)
                compileZ3$(194) = WT$

                3481 '
                For Z = 184 To 193
                    For Z1 = Z + 1 To 194
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3488 '
            For Z = 195 To 205

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 12)
                    GoTo 3493
                End If
            Next Z

            If JS!(X, 12) > compileZ1!(205, 1) Then
                compileZ1!(205, 1) = JS!(X, 12)
                compileZ2$(205) = NM$(X)
                compileZ3$(205) = WT$

                3493 '
                For Z = 195 To 204
                    For Z1 = Z + 1 To 205
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3500 '
            For Z = 206 To 216

                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 13)
                    GoTo 3505
                End If
            Next Z

            If JS!(X, 13) > compileZ1!(216, 1) Then

                compileZ1!(216, 1) = JS!(X, 13)
                compileZ2$(216) = NM$(X)
                compileZ3$(216) = WT$

                3505 '
                For Z = 206 To 215
                    For Z1 = Z + 1 To 216
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            APP = APP + JS!(X, 9)
            For Z = 228 To 238
                If compileZ2$(Z) = NM$(X) And compileZ3$(Z) = WT$ Then
                    compileZ1!(Z, 1) = JS!(X, 9)
                    GoTo 3518
                End If
            Next Z

            If JS!(X, 9) > compileZ1!(238, 1) Then

                compileZ1!(238, 1) = JS!(X, 9)
                compileZ2$(238) = NM$(X)
                compileZ3$(238) = WT$

                3518 '
                For Z = 228 To 237:
                    For Z1 = Z + 1 To 238:
                        If compileZ1!(Z, 1) < compileZ1!(Z1, 1) Then
                            Swap compileZ2$(Z1), compileZ2$(Z)
                            Swap compileZ3$(Z1), compileZ3$(Z)
                            Swap compileZ1!(Z1, 1), compileZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

        End If 'Done checking for "XXX"

    Next X

    3530 '
    For Z = 1 To TEAMS_PER_LEAGUE

        If BAT%(3) = 0 Then BAT%(3) = 1

        If BA$(Z) = WT$ Then
            OFT!(Z, 1) = BAT%(4) / BAT%(3)
            For M = 3 To 14: OFT!(Z, M) = BAT%(M): Next M
            GoTo 3535
        End If

    Next Z

    If BAT%(4) / BAT%(3) > OFT!(40, 1) Then

        OFT!(40, 1) = BAT%(4) / BAT%(3)

        For M = 3 To 14: 
            OFT!(40, M) = BAT%(M)
        Next M

        BA$(40) = WT$

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

        If PIT%(1) = 0 Then PIT%(1) = 1: PIT%(4) = 3

        If ER$(Z) = WT$ Then
            DFT!(Z, 1) = PIT%(4) / PIT%(1) * 27
            For M = 2 To 14:
                DFT!(Z, M) = PIT%(M - 1)
            Next M
            GoTo 3555
        End If

    Next Z

    If PIT%(4) / PIT%(1) * 27 < DFT!(40, 1) Then

        DFT!(40, 1) = PIT%(4) / PIT%(1) * 27
        For M = 2 To 14: DFT!(40, M) = PIT%(M - 1): Next M

        ER$(40) = WT$
        ER$(40) = WT$

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
            If DFT!(Z, 1) = DFT!(40, 1) Then ER$(Z) = "*": ER$(Z) = "*"
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

    Shared O1(), O2()
    Shared compileTeams$()

    'Sort record by games won
    For Z = 0 To 3

        AA = (Z * 10) + 1
        ZZ = (Z * 10) + 10

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ:

                diffX = O1(X) - O2(X)
                diffY = O1(Y) - O2(Y)

                If diffX <= diffY Then

                    If diffX <> diffY Then

                        If Not (O1(X) <= O1(Y) And O1(X) > O2(X)) Or Not (O1(X) >= O1(Y) And O1(X) < O2(X)) Then

                            Swap compileTeams$(X), compileTeams$(Y)
                            Swap O1(X), O1(Y)
                            Swap O2(X), O2(Y)

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

    Shared O1(), O2()

    Shared compileZ1!(), DFT!(), OFT!()

    Shared BA$(), compileZ2$(), compileZ3$()
    Shared ER$(), compileTeams$()

    Color 15, 0

    Print "POSTING "; league$; " LEADERS"

    Open diskPaths$(2) + leaderFile$ + ".LDR" For Output As #2

    For X = 1 To TEAMS_PER_LEAGUE
        Write #2, compileTeams$(X), O1(X), O2(X)
    Next

    For X = 1 To 298
        Write #2, compileZ2$(X), compileZ3$(X), compileZ1!(X, 1), compileZ1!(X, 2)
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

