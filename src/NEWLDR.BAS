'----------------------------------------
'           NEWLDR Subroutine
'----------------------------------------
'This subroutine provides the user a menu
'for viewing stats compiled for the new
'"Expanded" Stats/Standings screens
Sub NEWLDR ()

    _Title "Full Count Baseball - Expanded Leaders / Standings"

    ReDim menuChoices$(1 To 5)

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared expLL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$(), Z1$()

    Shared D1$, D2$, D3$, D4$

    Erase CWS!, CLT!
    Erase LLS!, LWS!
    Erase TLS!, TWS!

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE EXPANDED LEAGUE LEADER FILES   "
        menuChoices$(2) = "VIEW EXPANDED LEAGUE LEADER FILES     "
        menuChoices$(3) = "UPDATE EXPANDED LEAGUE STANDINGS      "
        menuChoices$(4) = "VIEW / PRINT EXPANDED LEAGUE STANDINGS"
        menuChoices$(5) = "RETURN TO LEAGUE LEADER MAIN MENU     "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
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
                    leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then

                        For X = 1 To 40
                            Z1$(X) = "XXX"
                        Next

                        Open diskPaths$(2) + leagStatFile$ + ".TMS" For Input As #2

                        Input #2, league$, D1$, D2$, D3$, D4$

                        For X = 1 To 40: 
                            Input #2, Z1$(X)
                        Next

                        Close #2

                        Call UpdateExpandedLeaders(JB, JP, TS, TTS)
                        Call SAVELEADER(leagStatFile$, league$, JB, JP, TS, TTS)

                    End If

                Case 2:
                    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Call ExpandedLeaders_Menu(leaderFile$, JB, JP, TS, TTS)
                    End If

                Case 3: 
                    leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Color 15, 0
                        Cls
                        Call UpdateLeagueStandings(leagStatFile$)
                    End If

                Case 4:
                    standingsFile$ = ChooseFile$ (diskPaths$(2) + "*.STD", "LEAGUE STANDINGS", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then

                        Open diskPaths$(2) + standingsFile$ + ".STD" For Input As #1

                        Input #1, league$, D1$, D2$, D3$, D4$

                        For I = 1 To 40:
                            Input #1, TML$(I), WW!(I), expLL!(I), TWS!(I), TLS!(I), CWS!(I), CLT!(I), HWW!(I), HLL!(I), RWW!(I), RLL!(I), EWW!(I), ELL!(I), LTW!(I), LTL!(I)
                        Next

                        Close #1

                        Call NewStandingOptions

                    End If

            End Select

        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'      ViewExpandedStats Subroutine
'----------------------------------------
'This subroutine reads in the league stats
'from the .TMS file. It then relies on other
'routines to calculate the expanded stats,
'write them to a file and present a categ-
'orized menu to use for reviewing them
Sub ViewExpandedStats (JB, JP, tStats, ttStats)

    Shared D1$, D2$, D3$, D4$

    Shared Z1$()

    Color 15, 0
    Cls

    leagStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Open diskPaths$(2) + leagStatFile$ + ".TMS" For Input As #2

        Input #2, league$
        Input #2, D1$
        Input #2, D2$
        Input #2, D3$
        Input #2, D4$

        For X = 1 To 40:
            Input #2, Z1$(X)
        Next

        Close #2

        Call UpdateExpandedLeaders(JB, JP, tStats, ttStats)

        Call SAVELEADER(leagStatFile$, league$, JB, JP, tStats, ttStats)

        Call ExpandedLeaders_Menu(leagStatFile$, JB, JP, tStats, ttStats)

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'    UpdateExpandedLeaders Subroutine
'----------------------------------------
'This subroutine calculates the expanded
'stats from those previously read in to
'a variety of arrays.
Sub UpdateExpandedLeaders (JB, JP, tStats, ttStats)

    Shared GM2()

    Shared GM1!()

    Shared BK%(), careerB0%(), statB%()
    Shared careerP0%(), statP%(), PM%()
    Shared statT0%(), statT1%()

    Shared TT!(), TT1!()
    Shared Wins(), Losses()

    Shared batterNames$(), leaderBL$(), pitcherNames$(), leaderPL$()
    Shared leaderTeams$(), TMB$(), TMP$(), TT2$()
    Shared Z1$()

    Color 15, 0
    Cls

    For KK = 1 To 40:

        YY = YY + 1

        If Z1$(KK) <> "*" Then

            If _FileExists(diskPaths$(1) + Z1$(KK)) Then

                teamStat$ = Z1$(KK)
                Print "CONVERTING STAT FILE OF "; teamStat$

                Open diskPaths$(1) + teamStat$ For Input As #1

                Input #1, teamName$
                Input #1, W2%
                Input #1, L2%

                For I = 0 To 22
                    Input #1, batterNames$(I)
                    Input #1, statB%(I)
                Next I

                For I = 0 To 21:
                    Input #1, pitcherNames$(I)
                    Input #1, statP%(I)
                Next I

                For I = 0 To 22
                    For I1 = 0 To 21
                        Input #1, careerB0%(I, I1)
                    Next I1
                Next I

                For I = 0 To 21
                    For I1 = 0 To 41
                        Input #1, careerP0%(I, I1)
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

                Wins(YY) = W2%
                Losses(YY) = L2%

                tStats = tStats + 1

                For I = 0 To 21
                    TT!(tStats, I) = statT0%(I)
                    leaderTeams$(tStats, I) = teamName$
                Next

                ttStats = ttStats + 1

                For I = 0 To 22
                    TT1!(ttStats, I) = statT1%(I)
                    TT2$(ttStats, I) = teamName$
                Next

                For I = 0 To 22

                    If batterNames$(I) <> "XXX" Then
                        JB = JB + 1
                        leaderBL$(JB) = batterNames$(I)
                        TBN = TBN + 1
                        TMB$(TBN) = teamName$
                        GM1!(JB) = Wins(YY) + Losses(YY)
                    End If

                Next I

                For I = 0 To 21

                    If pitcherNames$(I) <> "XXX" Then
                        JP = JP + 1
                        leaderPL$(JP) = pitcherNames$(I)
                        TPN = TPN + 1
                        TMP$(TPN) = teamName$
                        GM2(JP) = Wins(YY) + Losses(YY)
                    End If

                Next I

                For I = 0 To 22

                    If batterNames$(I) <> "XXX" Then

                        K = K + 1
                        K1 = 0

                        For I1 = 0 To 21
                            K1 = K1 + 1
                            BK%(K, K1) = careerB0%(I, I1)
                        Next I1

                    End If

                Next I

                For I = 0 To 21

                    If pitcherNames$(I) <> "XXX" Then

                        PK = PK + 1
                        PK1 = 0

                        For I1 = 0 To 22
                            PK1 = PK1 + 1
                            PM%(PK, PK1) = careerP0%(I, I1)
                        Next I1

                    End If

                Next I

            End If 'Done checking if file exists

        End If 'Done checking for "XXX"

    Next KK

End Sub


'----------------------------------------
'        SAVELEADER Subroutine
'----------------------------------------
'This subroutine compiles the leader files
'(.BAT, .PIT, and .TOT) for the expanded
'standings modules
Sub SAVELEADER (leaderFile$, league$, JB, JP, tStats, ttStats)

    Shared BK%(), PM%()
    Shared GM2()

    Shared GM1!()
    Shared TT!(), TT1!()
    Shared Wins(), Losses()

    Shared leaderBL$(), leaderPL$()
    Shared leaderTeams$(), TMB$(), TMP$(), TT2$()

    Shared D1$, D2$, D3$, D4$

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + leaderFile$ + ".BAT" For Output As #1

    Write #1, JB, JP
    Write #1, D1$, D2$, D3$, D4$, league$
    Write #1, tStats, ttStats

    For I = 1 To 920:

        Write #1, leaderBL$(I), TMB$(I), GM1!(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 920:

        For I1 = 1 To 22:
            Write #1, BK%(I, I1)
        Next I1

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Open diskPaths$(2) + leaderFile$ + ".PIT" For Output As #1

    Write #1, JB, JP
    Write #1, D1$, D2$, D3$, D4$, league$
    Write #1, tStats, ttStats

    For I = 1 To 880

        Write #1, leaderPL$(I), TMP$(I), GM2(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 880:

        For I1 = 1 To 23:
            Write #1, PM%(I, I1)
        Next I1

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Open diskPaths$(2) + leaderFile$ + ".TOT" For Output As #1

    Write #1, JB, JP, D1$, D2$, D3$, D4$, league$, tStats, ttStats

    For I = 1 To 40:
        Write #1, Wins(I), Losses(I)
    Next

    For I = 1 To 40:

        For I1 = 0 To 21:
            Write #1, TT!(I, I1), leaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    For I = 1 To 40:

        For I1 = 0 To 22:
            Write #1, TT1!(I, I1), TT2$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    Close #1

End Sub


'----------------------------------------
'        READLEADER Subroutine
'----------------------------------------
'This subroutine reads in the leader files 
'(.LDS) for the expanded standings modules 
Sub READLEADER (leaderFile$, league$, JB, JP, tStats, ttStats)

    Shared BK%(), PM%()
    Shared GM2()

    Shared GM1!()
    Shared TT!(), TT1!()
    Shared Wins(), Losses()

    Shared leaderBL$(), leaderPL$()
    Shared leaderTeams$(), TMB$(), TMP$(), TT2$()

    Shared D1$, D2$, D3$, D4$

    'Print "READING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + leaderFile$ + ".BAT" For Input As #1

    Input #1, JB, JP
    Input #1, D1$, D2$, D3$, D4$, league$
    Input #1, tStats, ttStats

    For I = 1 To 920:

        Input #1, leaderBL$(I), TMB$(I), GM1!(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 920:

        For I1 = 1 To 22:
            Input #1, BK%(I, I1)
        Next I1

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Open diskPaths$(2) + leaderFile$ + ".PIT" For Input As #1

    Input #1, JB, JP
    Input #1, D1$, D2$, D3$, D4$, league$
    Input #1, tStats, ttStats

    For I = 1 To 880

        Input #1, leaderPL$(I), TMP$(I), GM2(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 880:

        For I1 = 1 To 23:
            Input #1, PM%(I, I1)
        Next I1

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Open diskPaths$(2) + leaderFile$ + ".TOT" For Input As #1

    Input #1, JB, JP, D1$, D2$, D3$, D4$, league$, tStats, ttStats

    For I = 1 To 40:
        Input #1, Wins(I), Losses(I)
    Next

    For I = 1 To 40:

        For I1 = 0 To 21:
            Input #1, TT!(I, I1), leaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    For I = 1 To 40:

        For I1 = 0 To 22:
            Input #1, TT1!(I, I1), TT2$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    Close #1

End Sub


'----------------------------------------
'    ExpandedLeaders_Menu Subroutine
'----------------------------------------
'This subroutine presents a categorized
'menu for viewing expanded leaders. It then
'relies on various subroutines for the end
'user display of those statistics.
Sub ExpandedLeaders_Menu (leaderFile$, JB, JP, tStats, ttStats)

    Do

        Cls
        Color 15, 0

        Call TITLE
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "VIEW BATTING LEADERS "
        menuChoices$(2) = "VIEW PITCHING LEADERS"
        menuChoices$(3) = "VIEW TEAM TOTALS     "
        menuChoices$(4) = "RETURN TO MAIN MENU  "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
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
                    Call ViewBattingLeaders(leaderFile$, JB, JP, tStats, ttStats)

                Case 2:
                    Call ViewPitchingLeaders(leaderFile$, JB, JP, tStats, ttStats)

                Case 3:
                    Call ViewTeamLeaders(leaderFile$, JB, JP, tStats, ttStats)

            End Select

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'      ViewBattingLeaders Subroutine
'----------------------------------------
'This subroutine loads previously compiled
'expanded leader stats for batters, then
'gives the user a menu for choosing and
'displaying batting stat categories.
'Separate routines are used to sort and
'display the calculated stats.
Sub ViewBattingLeaders (leaderFile$, JB, JP, tStats, ttStats)

    Shared BK%()
    Shared leaderBL$(), TMB$(), TMM$()
    Shared expBatCategory$(), TPP$()
    Shared TYP!()
    Shared GM1!()

    Shared D1$, D2$, D3$, D4$

    TMR = 1840

    Open diskPaths$(2) + leaderFile$ + ".BAT" For Input As #1

    Input #1, JB, JP
    Input #1, D1$, D2$, D3$, D4$, leaderFile$
    Input #1, tStats, ttStats

    For I = 1 To 920:

        Input #1, leaderBL$(I), TMB$(I), GM1!(I)

        Color 14
        Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 920:

        For I1 = 1 To 22:
            Input #1, BK%(I, I1)
        Next I1

        Color 14
        Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Do

        Color 15, 0
        Cls
        'Print
        'Print
        'Print

        Color 15, 12

        J = 19

        Locate 2, 4
        Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";

        For II = 1 To J:
            Locate 2 + II, 4
            Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
        Next

        Locate 3 + J, 4
        Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
        Locate 10, 15

        Do
            For I = 0 To 15
                Color 14
                Locate I + 3, 8: Print Using "##"; I;
                Print " ";
                Color 0: Print expBatCategory$(I);
                Color 14
                Print Tab(40); Using "##"; I + 16;
                Print " ";
                Color 0
                Print expBatCategory$(I + 16)
            Next

            Print
            Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

            stpl = 0
            PPP = 0

            Select Case B9
                Case 0: X = 1
                Case 1: X = 2
                Case 2: X = 3
                Case 3: X = 4
                Case 4: X = 5
                Case 5: X = 6
                Case 6: X = 7
                Case 7: X = 8
                Case 8: X = 9
                Case 9: X = 10
                Case 10: X = 11
                Case 11: X = 12
                Case 12: X = 13
                Case 13: X = 14
                Case 14: X = 15
                Case 15: X = 16
                Case 16: X = 17
                Case 17: X = 18
                Case 18: X = 19
                Case 19: X = 20
                Case 20: X = 17: X1 = 18: X2 = 12
                Case 21: X = 1: X1 = 2: X2 = 13: X3 = 8
                Case 22: X = 1: X1 = 2: X2 = 5: X3 = 6: X4 = 7
                Case 23: X = 1: X1 = 2: X2 = 8
                Case 24: X = 1: X1 = 7
                Case 25: X = 1: X1 = 9
                Case 26: X = 1: X1 = 8
                Case 27: X = 1: X1 = 4
                Case 28: X = 1: X1 = 3
                Case 29: X = 1: X1 = 16
                Case 30: X = 21: X1 = 22
                Case 31: '
            End Select

        Loop Until B9 >= 0 And B9 <= 31

        If B9 <> 31 Then

            Call SortOptions(printOrView%, JX)

            Cls
            PPP = 0

            Select Case B9

                Case 0 To 19:
                    For I = 1 To JB

                        If BK%(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = leaderBL$(I)
                            TMM$(PPP) = TMB$(I)
                            TYP!(PPP) = BK%(I, X)
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 1

                Case 20:
                    For I = 1 To JB

                        If BK%(I, X) + BK%(I, X1) + BK%(I, X2) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = leaderBL$(I)
                            TMM$(PPP) = TMB$(I)
                            TYP!(PPP) = (BK%(I, X) + BK%(I, X1)) / (BK%(I, X) + BK%(I, X1) + BK%(I, X2))
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 2

                Case 21:
                    For I = 1 To JB

                        If BK%(I, X) + BK%(I, X3) >= GM1!(I) * 3.1 Then

                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            If BK%(I, X1) = 0 Then
                                TPP$(PPP) = leaderBL$(PPP)
                                TMM$(PPP) = TMB$(PPP)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = BK%(I, X1) / BK%(I, X)
                            End If
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 2

                Case 22:
                    For I = 1 To JB

                        If BK%(I, X) >= GM1!(I) * 3.1 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP

                            If BK%(I, X1) = 0 And BK%(I, X2) = 0 And BK%(I, X3) = 0 And BK%(I, X4) = 0 Then
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = (BK%(I, X1) + BK%(I, X2) + (BK%(I, X3) * 2) + (BK%(I, X4) * 3)) / BK%(I, X)
                            End If

                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 2

                Case 23:
                    For I = 1 To JB

                        If BK%(I, X) >= GM1!(I) * 3.1 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            If BK%(I, X1) = 0 And BK%(I, X2) = 0 Then
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = (BK%(I, X1) + BK%(I, X2)) / (BK%(I, X) + BK%(I, X2))
                            End If
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 2

                Case 24 To 29:
                    For I = 1 To JB
                        If BK%(I, X) >= GM1!(I) * 3.1 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            If BK%(I, X1) = 0 Then
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = BK%(I, X) / BK%(I, X1)
                            End If
                        End If

                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 3

                Case 30:
                    For I = 1 To JB
                        If BK%(I, X) + BK%(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            If BK%(I, X1) / (BK%(I, X) + BK%(I, X1)) = 0 Then
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderBL$(I)
                                TMM$(PPP) = TMB$(I)
                                TYP!(PPP) = BK%(I, X1) / (BK%(I, X) + BK%(I, X1))
                            End If
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 2

            End Select

            QW = PPP

            Select Case printOrView%

                Case 1:
                    Call DisplayBattingLeaders(TVN, QW, B9)

                Case 2:
                    Call GetListLength(printOrView%, QW, LG)
                    Call PrintLeaders(TVN, LG, B9)

            End Select

        End If

    Loop Until B9 = 31 'Done selecting number

End Sub


'----------------------------------------
'    DisplayBattingLeaders Subroutine
'----------------------------------------
'This subroutine displays expanded batting
'stat categories on the end user display
Sub DisplayBattingLeaders (TVN, maxRecords, categoryId)

    Shared TYP!()
    Shared TMM$(), expBatCategory$(), TPP$()

    userExit% = 0

    Do

        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; expBatCategory$(categoryId)

        For W = 1 To maxRecords

            If W <> 0 And (W Mod 20 = 0) Then

                Color 14
                Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                Color 15

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     PLAYER                  TEAM                     "; expBatCategory$(categoryId)
                Else
                    userExit% = 1
                End If

            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "###"; W;
                Print " ";
                Color 3
                Print TPP$(W);
                Color 15
                Print Tab(25); TMM$(W);

                Select Case TVN

                    Case 1:
                        Print Tab(58); Using "#####"; TYP!(W)

                    Case 2:
                        Print Tab(58); Using ".###"; TYP!(W)

                    Case 3:
                        Print Tab(58); Using "###.##"; TYP!(W)

                End Select
            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

            Color 14
            Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
            Color 15

            If I$ <> Chr$(27) Then

                Locate 22: Print Space$(64)
                Color 14

                Do
                    Locate 22
                    Print "VIEW AGAIN? (Y/N)"
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'      ViewPitchingLeaders Subroutine
'----------------------------------------
'This subroutine loads previously compiled
'expanded leader stats for pitchers, then
'gives the user a menu for choosing and
'displaying pitching stat categories.
'Separate routines are used to sort and
'display the calculated stats.
Sub ViewPitchingLeaders (leaderFile$, JB, JP, tStats, ttStats)

    Shared leaderPL$()
    Shared TMM$(), TMP$(), expPitCategory$(), TPP$()
    Shared TYP!(), TYP1!()
    Shared PM%()

    Shared D1$, D2$, D3$, D4$

    ReDim GM2(880)
    Dim TYP2(920)

    TMR = 1760

    Open diskPaths$(2) + leaderFile$ + ".PIT" For Input As #1

    Input #1, JB, JP
    Input #1, D1$, D2$, D3$, D4$, leaderFile$
    Input #1, tStats, ttStats

    For I = 1 To 880:
        Input #1, leaderPL$(I), TMP$(I), GM2(I)
        Color 14
        Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR
    Next I

    For I = 1 To 880:

        For I1 = 1 To 23:
            Input #1, PM%(I, I1)
        Next I1

        Color 14
        Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Do

        Color 15, 0
        Cls

        Color 15, 10

        J = 20

        Locate 2, 4
        Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";

        For II = 1 To J:
            Locate 2 + II, 4
            Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
        Next

        Locate 3 + J, 4
        Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
        Locate 10, 15

        Do
            For I = 0 To 14:
                Color 14: Locate I + 3, 8: Print Using "##"; I;
                Print " ";
                Color 0: Print expPitCategory$(I);
                Color 14: Print Tab(40); Using "##"; I + 15;
                Print " ";
                Color 0: Print expPitCategory$(I + 15)
            Next

            Print
            Locate , 23

            Input "SELECT NUMBER TO VIEW"; categoryId

            sptl = 0
            PPP = 0

            Select Case categoryId
                Case 0: X = 1
                Case 1: X = 2
                Case 2: X = 3
                Case 3: X = 4
                Case 4: X = 5
                Case 5: X = 6
                Case 6: X = 7
                Case 7: X = 8
                Case 8: X = 9
                Case 9: X = 10
                Case 10: X = 11
                Case 11: X = 12
                Case 12: X = 13
                Case 13: X = 14
                Case 14: X = 15
                Case 15: X = 16
                Case 16: X = 18
                Case 17: X = 19
                Case 18: X = 18: X1 = 19
                Case 19: X = 20
                Case 20: X = 22
                Case 21: X = 1: X1 = 4
                Case 22: X = 1: X1 = 5
                Case 23: X = 1: X1 = 6
                Case 24: X = 1: X1 = 2
                Case 25: X = 1: X1 = 14
                Case 26: X = 16
                Case 27: X = 17
                Case 28: X = 9: X1 = 21
                Case 29: Exit Sub 'GoTo 70
            End Select

        Loop Until categoryId >= 0 And categoryId <= 29

        If categoryId <> 29 Then

            Call SortOptions(printOrView%, JX)

            Cls
            PPP = 0

            Select Case categoryId
                Case 0:
                    For I = 1 To JP
                        If PM%(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = leaderPL$(I)
                            TMM$(PPP) = TMP$(I)
                            TYP2(PPP) = PM%(I, X)
                        End If

                    Next I

                    For I = 2 To PPP:
                        stpl = stpl + 1
                        Locate 5, 5: Print "sorting player "; stpl
                        For J = PPP To I Step -1

                            If JX = 1 Then
                                If TYP2(J - 1) < TYP2(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP2(J - 1), TYP2(J)
                            Else
                                'JX = 2
                                If TYP2(J - 1) > TYP2(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP2(J - 1), TYP2(J)
                            End If
                        Next J
                    Next I

                    For I = 1 To PPP
                        TYP!(I) = (Int(TYP2(I) / 3))
                        TYP1!(I) = TYP2(I) - (Int(TYP2(I) / 3) * 3)
                    Next

                    TVN = 1

                Case 1 To 16, 19, 20, 26, 27:
                    For I = 1 To JP

                        If PM%(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = leaderPL$(I)
                            TMM$(PPP) = TMP$(I)
                            TYP!(PPP) = PM%(I, X)
                        End If
                    Next I

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 2

                Case 18:
                    For I = 1 To JP
                        If PM%(I, X) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP

                            If PM%(I, X1) <> 0 Then
                                TPP$(PPP) = leaderPL$(I)
                                TMM$(PPP) = TMP$(I)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderPL$(I)
                                TMM$(PPP) = TMP$(I)
                                TYP!(PPP) = PM%(I, X1) / PM%(I, X)
                            End If
                        End If
                    Next

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 4

                Case 21 To 25:
                    For I = 1 To JP
                        If PM%(I, X) / 3 >= GM2(I) Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP

                            If PM%(I, X) = 0 Or PM%(I, X1) = 0 Then
                                TPP$(PPP) = leaderPL$(I)
                                TMM$(PPP) = TMP$(I)
                                TYP!(PPP) = 0
                            Else
                                TPP$(PPP) = leaderPL$(I)
                                TMM$(PPP) = TMP$(I)
                                TYP!(PPP) = (PM%(I, X1) * 9) / (PM%(I, X) / 3)
                            End If
                        End If

                    Next

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 3

                Case 28:
                    For I = 1 To JP
                        If PM%(I, X1) <> 0 Then
                            PPP = PPP + 1
                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                            TPP$(PPP) = leaderPL$(I)
                            TMM$(PPP) = TMP$(I)
                            TYP!(PPP) = PM%(I, X1) / PM%(I, X)
                        End If
                    Next

                    Call SortPlayer(PPP, stpl, JX)
                    TVN = 3

            End Select

            QW = PPP

            Select Case printOrView%

                Case 1
                    Call DisplayPitchingLeaders(TVN, QW, categoryId)
                Case 2
                    Call GetListLength(printOrView%, QW, LG)
                    Call PrintLeaders(TVN, LG, B9)

            End Select

        End If

    Loop Until B9 = 29 'Done selecting number

End Sub


'----------------------------------------
'   DisplayPitchingLeaders Subroutine
'----------------------------------------
'This subroutine displays expanded pitching
'stat categories on the end user display
Sub DisplayPitchingLeaders (TVN, maxRecords, categoryId)

    Shared TMM$(), expPitCategory$(), TPP$()
    Shared TYP!(), TYP1!()

    userExit% = 0

    Do
        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; expPitCategory$(categoryId)

        For W = 1 To maxRecords

            If W <> 0 And (W Mod 20 = 0) Then

                Color 14
                Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                Color 15

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     PLAYER                  TEAM                     "; expPitCategory$(categoryId)
                Else
                    userExit% = 1
                End If
            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "###"; W;: Print " ";
                Color 3
                Print TPP$(W);
                Color 15: Print Tab(25); TMM$(W);: Color 15
                Select Case TVN
                    Case 1: Print Tab(58); Using "###"; TYP!(W);: Print ".";: Print Using "#"; TYP1!(W)
                    Case 2: Print Tab(58); Using "#####"; TYP!(W)
                    Case 3: Print Tab(58); Using "###.##"; TYP!(W)
                    Case 4: Print Tab(58); Using ".##"; TYP!(W);: Print "%"
                End Select
            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

            Color 14
            Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
            Color 15

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Locate 22: Print Space$(64)
                Color 14

                Do
                    Locate 22
                    Print "VIEW AGAIN? (Y/N)"
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'     ViewTeamLeaders Subroutine
'----------------------------------------
'This subroutine lets the user choose team
'player stat categories and relies on other
'routines to sort and display the results.
Sub ViewTeamLeaders (leaderFile$, JB, JP, tStats, ttStats)

    Shared Wins(), Losses()

    Shared TT!(), TT1!()
    Shared TYP!(), TYP1!()

    Shared leaderTeams$(), TMM$()
    Shared expBatCategory$(), batIndCategory$(), TT2$()

    Shared D1$, D2$, D3$, D4$

    Open diskPaths$(2) + leaderFile$ + ".TOT" For Input As #1

    Input #1, JB, JP
    Input #1, D1$, D2$, D3$, D4$, leaderFile$
    Input #1, tStats, ttStats

    TMR = 120

    For I = 1 To 40:
        Input #1, Wins(I), Losses(I)
    Next

    For I = 1 To 40:

        For I1 = 0 To 21:
            Input #1, TT!(I, I1), leaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    For I = 1 To 40:

        For I1 = 0 To 22:
            Input #1, TT1!(I, I1), TT2$(I, I1)
        Next

        Color 14
        Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    Close #1

    Do

        Color , 0
        Cls

        J = 21

        Locate 1, 4
        Color 15, 6
        Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";

        For II = 1 To J:
            Locate 1 + II, 4
            Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
        Next

        Locate 2 + J, 4
        Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
        Locate 10, 15

        For I = 0 To 19:
            Color 11
            Locate I + 2, 8
            Print Using "##"; I;
            Print " ";
            Color 0: Print batIndCategory$(I)
        Next

        For I = 20 To 39:
            Color 11
            Locate I - 18, 40
            Print Tab(40); Using "##"; I;
            Print " ";
            Color 0: Print batIndCategory$(I)
        Next

        Do
            Color 14
            Locate 22, 23: Input "SELECT NUMBER TO VIEW"; categoryId

            PPP = 0
            stpl = 0

            Select Case categoryId
                Case 0: X = 0: XX = 0
                Case 1: X = 1: XX = 1
                Case 2: X = 2: XX = 2
                Case 3: X = 3: XX = 3
                Case 4: X = 4: XX = 4
                Case 5: X = 5: XX = 5
                Case 6: X = 6: XX = 6
                Case 7: X = 7: XX = 7
                Case 8: X = 8: XX = 8
                Case 9: X = 9: XX = 9
                Case 10: X = 10: XX = 10
                Case 11: X = 11: XX = 11
                Case 12: X = 18: XX = 12
                Case 13: X = 19: XX = 13
                Case 14: X = 0: X1 = 1: XX = 14
                Case 15: X = 0: X1 = 1: X2 = 4: X3 = 5: X4 = 6: XX = 15
                Case 16: X = 0: X1 = 1: X2 = 7: XX = 16
                Case 17: X = 12: XX = 17
                Case 18: X = 15: XX = 18
                Case 19: X = 16: X1 = 17: X2 = 11: XX = 19
                Case 20: X = 0: XX = 20
                Case 21: X = 1: XX = 21
                Case 22: X = 2: XX = 22
                Case 23: X = 3: XX = 23
                Case 24: X = 4: XX = 24
                Case 25: X = 5: XX = 25
                Case 26: X = 10: XX = 26
                Case 27: X = 11: XX = 27
                Case 28: X = 12: XX = 28
                Case 29: X = 13: XX = 29
                Case 30: X = 14: XX = 30
                Case 31: X = 17: XX = 31
                Case 32: X = 18: XX = 32
                Case 33: X = 17: X1 = 18: XX = 33
                Case 34: X = 19: XX = 34
                Case 35: X = 21: XX = 35
                Case 36: X = 0: X1 = 3: XX = 36
                Case 37: X = 15: XX = 37
                Case 38: X = 16: XX = 38
            End Select

            If categoryId <> 39 Then
                Call SortOptions(printOrView%, JX)
                Cls
            End If

            Select Case categoryId

                Case 0 To 13, 17, 18:
                    For I = 1 To tStats
                        TMM$(I) = leaderTeams$(I, X)
                        TYP!(I) = TT!(I, X)
                    Next

                    Call SortTeam(tStats, stpl, JX)

                    TVN = 1
                    maxRecords = tStats
                    screenType = 0

                Case 14
                    For I = 1 To tStats
                        TMM$(I) = leaderTeams$(I, X)
                        TYP!(I) = TT!(I, X1) / TT!(I, X)
                    Next

                    Call SortTeam(tStats, stpl, JX)

                    TVN = 2
                    maxRecords = tStats
                    screenType = 0

                Case 15
                    For I = 1 To tStats
                        TMM$(I) = leaderTeams$(I, X)
                        TYP!(I) = (TT!(I, X1) + TT!(I, X2) + (TT!(I, X3) * 2) + (TT!(I, X4) * 3)) / TT!(I, X)
                    Next

                    Call SortTeam(tStats, stpl, JX)

                    TVN = 2
                    maxRecords = tStats
                    screenType = 0

                Case 16
                    For I = 1 To maxRecords
                        TMM$(I) = leaderTeams$(I, X)
                        TYP!(I) = (TT!(I, X1) + TT!(I, X2)) / (TT!(I, X) + TT!(I, X2))
                    Next

                    Call SortTeam(tStats, stpl, JX)

                    TVN = 2
                    maxRecords = tStats
                    screenType = 0

                Case 19
                    For I = 1 To maxRecords
                        TMM$(I) = leaderTeams$(I, X)
                        TYP!(I) = (TT!(I, X) + TT!(I, X1)) / (TT!(I, X) + TT!(I, X1) + TT!(I, X2))
                    Next

                    Call SortTeam(tStats, stpl, JX)

                    TVN = 2
                    maxRecords = tStats
                    screenType = 0

                Case 20
                    For I = 1 To ttStats
                        TMM$(I) = TT2$(I, X)
                        TYP!(I) = TT1!(I, X)
                    Next

                    For I = 2 To ttStats
                        stpl = stpl + 1
                        Locate 5, 5: Print "sorting team "; stpl
                        For J = ttStats To I Step -1

                            If JX = 1 Then
                                If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                            Else
                                'JX = 2
                                If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                            End If
                        Next J
                    Next I

                    For I = 1 To ttStats
                        TYP!(I) = (Int(TYP!(I) / 3))
                        TYP1!(I) = TYP!(I) - (Int(TYP!(I) / 3) * 3)
                    Next

                    TVN = 1
                    maxRecords = ttStats
                    screenType = 1

                Case 21 To 32, 34, 35, 37, 38
                    For I = 1 To ttStats
                        TMM$(I) = TT2$(I, X)
                        TYP!(I) = TT1!(I, X)
                    Next

                    Call SortTeam(ttStats, stpl, JX)

                    TVN = 2
                    maxRecords = ttStats
                    screenType = 1

                Case 33
                    For I = 1 To ttStats
                        TMM$(I) = TT2$(I, X)
                        If TT1!(I, X) = 0 Then
                            TYP!(I) = 0
                        Else
                            TYP!(I) = (TT1!(I, X1) / TT1!(I, X)) * 100
                        End If
                    Next

                    Call SortTeam(ttStats, stpl, JX)

                    TVN = 3
                    maxRecords = ttStats
                    screenType = 1

                Case 36
                    For I = 1 To ttStats
                        TMM$(I) = TT2$(I, X)
                        TYP!(I) = (TT1!(I, X1) * 9) / (TT1!(I, X) / 3)
                    Next

                    Call SortTeam(ttStats, stpl, JX)

                    TVN = 3
                    maxRecords = ttStats
                    screenType = 1

                Case 39

            End Select

            If categoryId <> 39 Then
                Call DisplayTeamView(XX, screenType, printOrView%, TVN, maxRecords)
            End If

        Loop Until categoryId >= 0 Or categoryId <= 39

    Loop Until categoryId = 39

End Sub


'----------------------------------------
'       DisplayTeamView Subroutine
'----------------------------------------
'This subroutine uses several different
'routines to display team total stats
Sub DisplayTeamView (XX, screenType, printOrView%, TVN, maxRecords)

    Shared TYP!(), TYP1!()

    Shared TMM$()
    Shared expBatCategory$(), expPitCategory$()

    If screenType = 0 Then

        Select Case printOrView%

            Case 1:
                Call LeagueTotals_A_Screen(XX, TVN, maxRecords)

            Case 2:

                Color 0, 15

                LPrint "     TEAM                     "; expBatCategory$(XX)

                For W = 1 To maxRecords

                    Color 12
                    LPrint Using "###"; W;
                    LPrint " ";
                    Color 3
                    LPrint TMM$(W);
                    Color 15

                    Select Case TVN

                        Case 1:
                            LPrint Tab(32); Using "#####"; TYP!(W)

                        Case 2:
                            LPrint Tab(33); Using ".###"; TYP!(W)

                    End Select

                Next W

                LPrint Chr$(12)

        End Select

    Else

        Select Case printOrView%

            Case 1:
                Call LeagueTotals_B_Screen(XX, TVN, maxRecords)

            Case 2:
                Color 0, 15

                LPrint "     TEAM                     "; expPitCategory$(XX)

                LPrint Using "###"; W;
                LPrint " ";
                LPrint TMM$(W);:

                For W = 1 To maxRecords

                    Select Case TVN

                        Case 1:
                            LPrint Tab(32); Using "#####"; TYP!(W);
                            LPrint ".";: LPrint Using "#"; TYP1!(W)

                        Case 2:
                            LPrint Tab(32); Using "#####"; TYP!(W)

                        Case 3:
                            LPrint Tab(32); Using "##.##"; TYP!(W)

                    End Select

                Next W

                LPrint Chr$(12)

        End Select

    End If

End Sub


'----------------------------------------
'    LeagueTotals_A_Screen Subroutine
'----------------------------------------
'This subroutine is one of 2 designed to
'output team totals to the use display.
'The 2 routines differ in the types of
'numbers they are intended to output.
Sub LeagueTotals_A_Screen (XX, TVN, maxRecords)

    Shared TYP!()
    Shared TMM$(), batIndCategory$()

    Do

        Color 14, 0
        Cls

        Print "     TEAM                     "; batIndCategory$(XX)

        For W = 1 To maxRecords

            If W <> 0 And (W Mod 20 = 0) Then

                Color 14
                Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                Color 15

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     TEAM                     "; batIndCategory$(XX)
                Else
                    Exit Sub
                End If

            Else

                Color 12
                Print Using "###"; W;
                Print " ";
                Color 3
                Print TMM$(W);

                Color 15

                Select Case TVN

                    Case 1:
                        Print Tab(32); Using "#####"; TYP!(W)

                    Case 2:
                        Print Tab(33); Using ".###"; TYP!(W)

                End Select

            End If

        Next W

        Color 14
        Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        Color 15

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Locate 22: Print Space$(64)
            Color 14

            Do
                Locate 22
                Print "VIEW AGAIN? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'    LeagueTotals_B_Screen Subroutine
'----------------------------------------
'This subroutine is one of 2 designed to
'output team totals to the use display.
'The 2 routines differ in the types of
'numbers they are intended to output.
Sub LeagueTotals_B_Screen (XX, TVN, maxRecords)

    Shared TMM$(), batIndCategory$()
    Shared TYP!(), TYP1!()

    Do

        Color 14, 0
        Cls

        Print "     TEAM                     "; batIndCategory$(XX)

        For W = 1 To maxRecords

            If W <> 0 And (W Mod 20 = 0) Then

                Color 14
                Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                Color 15

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     TEAM                     "; batIndCategory$(XX)
                Else
                    Exit Sub
                End If

            Else

                Color 12
                Print Using "###"; W;
                Print " ";
                Color 3
                Print TMM$(W);
                Color 15

                Select Case TVN
                    '-- This is the only thing different between This
                    '-- and the LeagueTotals_A_Screen routine
                    Case 1:
                        Print Tab(32); Using "#####"; TYP!(W);
                        Print ".";
                        Print Using "#"; TYP1!(W)

                    Case 2:
                        Print Tab(32); Using "#####"; TYP!(W)

                    Case 3:
                        Print Tab(32); Using "##.##"; TYP!(W)

                End Select

            End If

        Next W

        Color 14
        Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        Color 15

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Locate 22: Print Space$(64)
            Color 14

            Do
                Locate 22
                Print "VIEW AGAIN? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       SortOptions Subroutine
'----------------------------------------
'This subroutine prompts the users for the
'different sort options available in the
'"expanded" stats menus.

'sortType%
'   1: Sort HIGH to LOW
'   2: Sort LOW to HIGH
Sub SortOptions (printOrView%, sortType%)

    Do

        J = 6

        Locate 8, 8
        Color 15, 1: Print " " + Chr$(214); String$(57, 196); Chr$(183) + " ";
        For II = 1 To J
            Locate 8 + II, 8: Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";
        Next
        Locate 9 + J, 8
        Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";
        Locate 10, 15

        Color 3
        Locate , 30: Print "SELECT OPTION:"
        Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
        Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
        Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "H":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P": printOrView% = 2
                    Case "V": printOrView% = 1
                    Case "R":
                End Select

                sortType% = 1

            Case "L":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P": printOrView% = 2
                    Case "V": printOrView% = 1
                    Case "R":
                End Select

                sortType% = 2

            Case "R": Cls '

        End Select

    Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

End Sub


'----------------------------------------
'       PrintViewReselect Subroutine
'----------------------------------------
'This subroutine presents the end user
'with a choice to Print or View the exist-
'ing category, or select a new category
Sub PrintViewReselect (userOpt$)

    J = 4

    Locate 9, 15
    Color 15, 0

    Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";

    For II = 1 To J
        Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
    Next

    Locate 10 + J, 15
    Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ";
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'This subroutine sorts the players involved
'in the chosen stat category
Sub SortPlayer (PPP, stpl, sortType%)

    Shared TMM$(), TPP$()
    Shared TYP!()

    For I = 2 To PPP

        stpl = stpl + 1

        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType% = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                End If
            End If

            If sortType% = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'       SortTeam Subroutine
'----------------------------------------
'This subroutine sorts the teams involved
'in the chosen stat category
Sub SortTeam (sortMax, stpl, sortType%)

    Shared TYP!()
    Shared TMM$()

    For I = 2 To sortMax

        stpl = stpl + 1

        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType% = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TMM$(J - 1), TMM$(J)
                End If
            End If

            If sortType% = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TMM$(J - 1), TMM$(J)
                End If
            End If

        Next J
    Next I

End Sub


'----------------------------------------
'     PrintLeaders Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the default printer
Sub PrintLeaders (TVN, maxRecords, B9)

    Shared TYP!()

    Shared TMM$()
    Shared expBatCategory$(), TPP$()

    Color 0, 15
    Cls

    LPrint "     PLAYER                  TEAM                     "; expBatCategory$(B9)

    For W = 1 To maxRecords

        If W <> 0 And (W Mod 50 = 0) Then
            LPrint Chr$(12)
            LPrint "     PLAYER                  TEAM                     "; expBatCategory$(B9)
        End If

        LPrint Using "###"; W;
        LPrint " ";
        LPrint TPP$(W);
        LPrint Tab(25); TMM$(W);

        Select Case TVN

            Case 1:
                LPrint Tab(58); Using "#####"; TYP!(W)

            Case 2:
                LPrint Tab(58); Using ".###"; TYP!(W)

            Case 3:
                LPrint Tab(58); Using "###.##"; TYP!(W)

        End Select

    Next W

    LPrint Chr$(12)

End Sub


'----------------------------------------
'        GetListLength Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'maximum number of items to be displayed
'or returned by the standings view. This
'helps the user have a custom view of the
'categories without needing to page through
'every result.
Sub GetListLength (printOrView%, listLength, maxItems)

    Color , 0
    Cls

    If printOrView% <> 1 Then

        Do
            Color , 0
            Cls

            Print
            Print
            Print
            Print
            Print
            J = 2

            Locate 10, 20
            Color 4, 15
            Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";

            For II = 1 To J
                Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
            Next

            Locate 11 + J, 20
            Print " " + Chr$(211); String$(30, 196); Chr$(189) + " "
            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub

