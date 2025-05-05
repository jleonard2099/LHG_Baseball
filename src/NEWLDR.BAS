'----------------------------------------
'           NEWLDR Subroutine
'----------------------------------------
'This subroutine provides the user a menu
'for viewing stats compiled for the new
'"Expanded" Stats/Standings screens
Sub NEWLDR ()

    _Title "Full Count Baseball - Expanded Leaders / Standings"

    ReDim menuChoices$(1 To 5)

    Shared cLoseStreak(), cWinStreak()
    Shared totExtInnLosses(), totExtInnWins(), totHomeLosses!(), totHomeWinss!()
    Shared teamTotLosses(), lastLoseStreak(), lastTenLosses(), lastTenWins(), lastWinStreak()
    Shared totRoadLosses(), totRoadWins(), teamLoseStreak(), teamWinStreak(), teamTotWins()

    Shared memberName$(), expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Erase cWinStreak, cLoseStreak
    Erase lastLoseStreak, lastWinStreak
    Erase teamLoseStreak, teamWinStreak

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE EXPANDED LEAGUE LEADERS  "
        menuChoices$(2) = "VIEW EXPANDED LEAGUE LEADERS    "
        menuChoices$(3) = "UPDATE EXPANDED LEAGUE STANDINGS"
        menuChoices$(4) = "VIEW EXPANDED LEAGUE STANDINGS  "
        menuChoices$(5) = "RETURN TO LEAGUE LEADERS MENU   "
        SELECTION% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
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

            printOrView = 0

            Select Case SELECTION%

                Case 1:
                    leagStatFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.TMS", "League Stats files [.TMS]")
                    'ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                    If leagStatFile$ <> "" Then

                        For X = 1 To TEAMS_PER_LEAGUE
                            memberName$(X) = "XXX"
                        Next

                        Call ReadLeagueStats(leagStatFile$)
                        Call UpdateExpandedLeaders(JB, JP, TS, TTS)
                        Call SaveExpandedLeaders(GetFileBaseName$(leagStatFile$), league$, JB, JP, TS, TTS)

                    End If

                Case 2:
                    leagStatFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.TMS", "League Stats files [.TMS]")
                    'ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                    If leagStatFile$ <> "" Then
                        'Call ReadExpandedLeaders(leagStatFile$, league$, totalStats)
                        '-- covered in the submenu???
                        Call ExpandedLeaders_Menu(leagStatFile$, JB, JP, TS, TTS)
                    End If

                Case 3:
                    leagStatFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.TMS", "League Stats files [.TMS]")
                    'ChooseFile$(diskPaths$(2) + "*.TMS", "LEAGUE STATS", FileExit%)

                    If leagStatFile$ <> "" Then

                        Color 15, 0
                        Cls

                        For X = 1 To TEAMS_PER_LEAGUE
                            memberName$(X) = "XXX"
                        Next

                        Call ReadLeagueStats(leagStatFile$)
                        Call UpdateExpandedStandings(GetFileBaseName$(leagStatFile$))

                    End If

                Case 4:
                    standingsFile$ = _OpenFileDialog$("Choose Expanded Standings file", diskPaths$(2), "*.STD", "Expanded Standings files [.STD]")
                    'ChooseFile$(diskPaths$(2) + "*.STD", "CONFERENCE STANDINGS", FileExit%)

                    If standingsFile$ <> "" Then

                        Open standingsFile$ For Input As #2

                        Input #2, league$, div1Name$, div2Name$, div3Name$, div4Name$

                        For I = 1 To TEAMS_PER_LEAGUE
                            Input #2, expStdLeagName$(I), teamTotWins(I), totHomeWinss!(I), totRoadWins(I), totExtInnWins(I), lastTenWins(I), teamTotLosses(I), totHomeLosses!(I), totRoadLosses(I), totExtInnLosses(I), lastTenLosses(I), teamWinStreak(I), teamLoseStreak(I), cWinStreak(I), cLoseStreak(I)
                        Next

                        Close #2

                        userKey$ = GetOutputOptions$

                        If userKey$ <> Chr$(27) Then

                            Select Case userKey$

                                Case "F"
                                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
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

            End Select

        End If

    Loop Until SELECTION% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'    UpdateExpandedLeaders Subroutine
'----------------------------------------
'This subroutine calculates the expanded
'stats from those previously read in to
'a variety of arrays.
Sub UpdateExpandedLeaders (JB, JP, plyrTotalStats, teamTotalStats)

    Shared batTotGames!()
    Shared pitTotGames!()

    Shared expLdrB0(), careerB0(), statB()
    Shared careerP0(), statP(), expLdrP0()
    Shared statT0!(), statT1!()

    Shared plyLdrVal!(), teamLdrVal!()
    Shared divWins(), divLosses()

    Shared batterNames$(), pitcherNames$()
    Shared expBatLdrName$(), expPitLdrName$()
    Shared expBatLdrTeam$(), expPitLdrTeam$()
    Shared plyLeaderTeams$(), tmLeaderTeams$()
    Shared memberName$()

    Erase batTotGames!
    Erase pitTotGames!

    Erase expLdrB0, careerB0, statB
    Erase careerP0, statP, expLdrP0
    Erase statT0!, statT1!

    Erase plyLdrVal!, teamLdrVal!
    Erase divWins, divLosses

    Erase batterNames$, pitcherNames$
    Erase expBatLdrName$, expPitLdrName$
    Erase expBatLdrTeam$, expPitLdrTeam$
    Erase plyLeaderTeams$, tmLeaderTeams$

    Color 15, 0
    Cls

    JB = 0

    For KK = 1 To TEAMS_PER_LEAGUE

        YY = YY + 1

        If memberName$(KK) <> "*" And memberName$(KK) <> "XXX" And memberName$(KK) <> "" Then

            Print "CONVERTING STAT FILE OF "; memberName$(KK)

            If _FileExists(diskPaths$(1) + memberName$(KK)) Then

                Open diskPaths$(1) + memberName$(KK) For Input As #1

                Input #1, teamName$
                Input #1, teamWins
                Input #1, teamLosses

                For I = 0 To 22
                    Input #1, batterNames$(I)
                    Input #1, statB(I)
                Next I

                For I = 0 To 21:
                    Input #1, pitcherNames$(I)
                    Input #1, statP(I)
                Next I

                For I = 0 To 22
                    For I1 = 0 To 21
                        Input #1, careerB0(I, I1)
                    Next I1
                Next I

                For I = 0 To 21
                    For I1 = 0 To 41
                        Input #1, careerP0(I, I1)
                    Next I1
                Next I

                Input #1, D0
                Input #1, D1

                For I = 0 To 21:
                    Input #1, statT0!(I)
                Next I

                For I = 0 To 22:
                    Input #1, statT1!(I)
                Next I

                Close #1

                divWins(YY) = teamWins
                divLosses(YY) = teamLosses

                plyrTotalStats = plyrTotalStats + 1

                For I = 0 To 21
                    plyLdrVal!(plyrTotalStats, I) = statT0!(I)
                    plyLeaderTeams$(plyrTotalStats, I) = teamName$
                Next

                teamTotalStats = teamTotalStats + 1

                For I = 0 To 22
                    teamLdrVal!(teamTotalStats, I) = statT1!(I)
                    tmLeaderTeams$(teamTotalStats, I) = teamName$
                Next

                For I = 0 To 22

                    If batterNames$(I) <> "XXX" Then
                        JB = JB + 1
                        expBatLdrName$(JB) = batterNames$(I)
                        TBN = TBN + 1
                        expBatLdrTeam$(TBN) = teamName$
                        batTotGames!(JB) = divWins(YY) + divLosses(YY)
                    End If

                Next I

                For I = 0 To 21

                    If pitcherNames$(I) <> "XXX" Then
                        JP = JP + 1
                        expPitLdrName$(JP) = pitcherNames$(I)
                        TPN = TPN + 1
                        expPitLdrTeam$(TPN) = teamName$
                        pitTotGames!(JP) = divWins(YY) + divLosses(YY)
                    End If

                Next I

                For I = 0 To 22

                    If batterNames$(I) <> "XXX" Then

                        K = K + 1
                        K1 = 0

                        For I1 = 0 To 21
                            K1 = K1 + 1
                            expLdrB0(K, K1) = careerB0(I, I1)
                        Next I1

                    End If

                Next I

                For I = 0 To 21

                    If pitcherNames$(I) <> "XXX" Then

                        PK = PK + 1
                        PK1 = 0

                        For I1 = 0 To 22
                            PK1 = PK1 + 1
                            expLdrP0(PK, PK1) = careerP0(I, I1)
                        Next I1

                    End If

                Next I

            End If 'Done checking if file exists

        End If 'Done checking for "XXX"

    Next KK

End Sub


'----------------------------------------
'     SaveExpandedLeaders Subroutine
'----------------------------------------
'This subroutine compiles the leader files
'(.BAT, .PIT, and .TOT) for the expanded
'standings modules
Sub SaveExpandedLeaders (leaderFile$, league$, JB, JP, plyrTotalStats, teamTotalStats)

    Shared expLdrB0(), expLdrP0()
    Shared pitTotGames!()

    Shared batTotGames!()
    Shared plyLdrVal!(), teamLdrVal!()
    Shared divWins(), divLosses()

    Shared expBatLdrName$(), expPitLdrName$()
    Shared plyLeaderTeams$(), expBatLdrTeam$(), expPitLdrTeam$(), tmLeaderTeams$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    TMR = (920 * 2) + (880 * 2) + (TEAMS_PER_LEAGUE * 3)

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    baseFile$ = leaderFile$

    Open diskPaths$(2) + baseFile$ + ".BAT" For Output As #1

    Write #1, JB, JP
    Write #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
    Write #1, plyrTotalStats, teamTotalStats

    For I = 1 To 920:

        Write #1, expBatLdrName$(I), expBatLdrTeam$(I), batTotGames!(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 920:

        For I1 = 1 To 22:
            Write #1, expLdrB0(I, I1)
        Next I1

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close 1

    Open diskPaths$(2) + baseFile$ + ".PIT" For Output As #1

    Write #1, JB, JP
    Write #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
    Write #1, plyrTotalStats, teamTotalStats

    For I = 1 To 880

        Write #1, expPitLdrName$(I), expPitLdrTeam$(I), pitTotGames!(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    For I = 1 To 880:

        For I1 = 1 To 23:
            Write #1, expLdrP0(I, I1)
        Next I1

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next I

    Close #1

    Open diskPaths$(2) + baseFile$ + ".TOT" For Output As #1

    Write #1, JB, JP, div1Name$, div2Name$, div3Name$, div4Name$, league$, plyrTotalStats, teamTotalStats

    For I = 1 To TEAMS_PER_LEAGUE
        Write #1, divWins(I), divLosses(I)

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR
    Next

    For I = 1 To TEAMS_PER_LEAGUE
        For I1 = 0 To 21:
            Write #1, plyLdrVal!(I, I1), plyLeaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR
    Next

    For I = 1 To TEAMS_PER_LEAGUE
        For I1 = 0 To 22:
            Write #1, teamLdrVal!(I, I1), tmLeaderTeams$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR
    Next

    Close #1

End Sub


'----------------------------------------
'     ReadExpandedLeaders Subroutine
'----------------------------------------
'This subroutine reads in both types of
'leader files (.BAT and .PIT) for the
'expanded standings  modules
Sub ReadExpandedLeaders (leaderFile$, league$, JB, JP, plyrTotalStats, teamTotalStats)

    Shared expLdrB0(), expLdrP0()
    Shared pitTotGames!()

    Shared batTotGames!()
    Shared plyLdrVal!(), teamLdrVal!()
    Shared divWins(), divLosses()

    Shared expBatLdrName$(), expPitLdrName$()
    Shared plyLeaderTeams$(), expBatLdrTeam$(), expPitLdrTeam$(), tmLeaderTeams$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    TMR = (920 * 2) + (880 * 2) + (TEAMS_PER_LEAGUE * 3)

    'Print "READING LEADER FILES . . . . PLEASE WAIT"
    baseFile$ = GetFileBaseName$(leaderFile$)

    If Not (_FileExists(diskPaths$(2) + baseFile$ + ".BAT") Or _FileExists(diskPaths$(2) + baseFile$ + ".PIT") Or _FileExists(diskPaths$(2) + baseFile$ + ".TOT")) Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(2) + baseFile$ + ".BAT" For Input As #1

        Input #1, JB, JP
        Input #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
        Input #1, plyrTotalStats, teamTotalStats

        For I = 1 To 920:

            Input #1, expBatLdrName$(I), expBatLdrTeam$(I), batTotGames!(I)

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        For I = 1 To 920:

            For I1 = 1 To 22:
                Input #1, expLdrB0(I, I1)
            Next I1

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        Close 1

        Open diskPaths$(2) + baseFile$ + ".PIT" For Input As #1

        Input #1, JB, JP
        Input #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
        Input #1, plyrTotalStats, teamTotalStats

        For I = 1 To 880

            Input #1, expPitLdrName$(I), expPitLdrTeam$(I), pitTotGames!(I)

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        For I = 1 To 880:

            For I1 = 1 To 23:
                Input #1, expLdrP0(I, I1)
            Next I1

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        Close 1

        Open diskPaths$(2) + baseFile$ + ".TOT" For Input As #1

        Input #1, JB, JP, div1Name$, div2Name$, div3Name$, div4Name$, league$, plyrTotalStats, teamTotalStats

        For I = 1 To TEAMS_PER_LEAGUE
            Input #1, divWins(I), divLosses(I)

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR
        Next

        For I = 1 To TEAMS_PER_LEAGUE
            For I1 = 0 To 21:
                Input #1, plyLdrVal!(I, I1), plyLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR
        Next

        For I = 1 To TEAMS_PER_LEAGUE
            For I1 = 0 To 22:
                Input #1, teamLdrVal!(I, I1), tmLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 45
            TMR = TMR - 1: Print Using "#####"; TMR
        Next

        Close #1

    End If

End Sub


'----------------------------------------
'    ExpandedLeaders_Menu Subroutine
'----------------------------------------
'This subroutine presents a categorized
'menu for viewing expanded leaders. It then
'relies on various subroutines for the end
'user display of those statistics.
Sub ExpandedLeaders_Menu (leaderFile$, JB, JP, plyrTotalStats, teamTotalStats)

    Do

        Cls

        Call TITLE
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "VIEW BATTING LEADERS "
        menuChoices$(2) = "VIEW PITCHING LEADERS"
        menuChoices$(3) = "VIEW TEAM TOTALS     "
        menuChoices$(4) = "RETURN TO MAIN MENU  "
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
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
        
            baseFile$ = GetFileBaseName$(leaderFile$)

            Select Case SELECTION%

                Case 1:
                    Call ViewBattingLeaders(baseFile$, JB, JP, plyrTotalStats, teamTotalStats)

                Case 2:
                    Call ViewPitchingLeaders(baseFile$, JB, JP, plyrTotalStats, teamTotalStats)

                Case 3:
                    Call ViewTeamLeaders(baseFile$, JB, JP, plyrTotalStats, teamTotalStats)

            End Select

        End If

    Loop Until SELECTION% = 4 Or ExitCode% = 27

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
Sub ViewBattingLeaders (leaderFile$, JB, JP, plyrTotalStats, teamTotalStats)

    Shared expLdrB0()
    Shared expBatLdrName$(), expBatLdrTeam$(), expLdrTeam$()
    Shared expBatCategory$(), expLdrPlyrName$()
    Shared expLdrVal!()
    Shared batTotGames!()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    If Not _FileExists(diskPaths$(2) + leaderFile$ + ".BAT") Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        TMR = (920 * 2)

        Open diskPaths$(2) + leaderFile$ + ".BAT" For Input As #1

        Input #1, JB, JP
        Input #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
        Input #1, plyrTotalStats, teamTotalStats

        For I = 1 To 920:

            Input #1, expBatLdrName$(I), expBatLdrTeam$(I), batTotGames!(I)

            Color 14
            Locate 18, 38
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        For I = 1 To 920:

            For I1 = 1 To 22:
                Input #1, expLdrB0(I, I1)
            Next I1

            Color 14
            Locate 18, 38
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        Close 1

        Do

            Color 15, 0
            Cls

            Call MenuBox(2, 4, 66, 19, 15, 12)

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
                newCategory = 0

                Select Case B9
                    Case 0: X0 = 1
                    Case 1: X0 = 2
                    Case 2: X0 = 3
                    Case 3: X0 = 4
                    Case 4: X0 = 5
                    Case 5: X0 = 6
                    Case 6: X0 = 7
                    Case 7: X0 = 8
                    Case 8: X0 = 9
                    Case 9: X0 = 10
                    Case 10: X0 = 11
                    Case 11: X0 = 12
                    Case 12: X0 = 13
                    Case 13: X0 = 14
                    Case 14: X0 = 15
                    Case 15: X0 = 16
                    Case 16: X0 = 17
                    Case 17: X0 = 18
                    Case 18: X0 = 19
                    Case 19: X0 = 20
                    Case 20: X0 = 17: X1 = 18: X2 = 12
                    Case 21: X0 = 1: X1 = 2: X2 = 13: X3 = 8
                    Case 22: X0 = 1: X1 = 2: X2 = 5: X3 = 6: X4 = 7
                    Case 23: X0 = 1: X1 = 2: X2 = 8
                    Case 24: X0 = 1: X1 = 7
                    Case 25: X0 = 1: X1 = 9
                    Case 26: X0 = 1: X1 = 8
                    Case 27: X0 = 1: X1 = 4
                    Case 28: X0 = 1: X1 = 3
                    Case 29: X0 = 1: X1 = 16
                    Case 30: X0 = 21: X1 = 22
                    Case 31: '
                End Select

            Loop Until B9 >= 0 And B9 <= 31

            If B9 <> 31 And newCategory = 0 Then

                Call SortOptions(printOrView, sortType, newCategory)

                Cls

                PPP = 0

                Select Case B9

                    Case 0 To 19:

                        For I = 1 To JB

                            If expLdrB0(I, X0) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                expLdrVal!(PPP) = expLdrB0(I, X0)
                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 1

                    Case 20:

                        For I = 1 To JB

                            If expLdrB0(I, X0) + expLdrB0(I, X1) + expLdrB0(I, X2) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                expLdrVal!(PPP) = (expLdrB0(I, X0) + expLdrB0(I, X1)) / (expLdrB0(I, X0) + expLdrB0(I, X1) + expLdrB0(I, X2))
                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 2

                    Case 21:

                        For I = 1 To JB

                            If expLdrB0(I, X0) + expLdrB0(I, X3) >= batTotGames!(I) * 3.1 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                If expLdrB0(I, X1) = 0 Then
                                    expLdrPlyrName$(PPP) = expBatLdrName$(PPP)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(PPP)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = expLdrB0(I, X1) / expLdrB0(I, X0)
                                End If
                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 2

                    Case 22:

                        For I = 1 To JB

                            If expLdrB0(I, X0) >= batTotGames!(I) * 3.1 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                If expLdrB0(I, X1) = 0 And expLdrB0(I, X2) = 0 And expLdrB0(I, X3) = 0 And expLdrB0(I, X4) = 0 Then
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = (expLdrB0(I, X1) + expLdrB0(I, X2) + (expLdrB0(I, X3) * 2) + (expLdrB0(I, X4) * 3)) / expLdrB0(I, X0)
                                End If
                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 2

                    Case 23:
                        For I = 1 To JB

                            If expLdrB0(I, X0) >= batTotGames!(I) * 3.1 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                If expLdrB0(I, X1) = 0 And expLdrB0(I, X2) = 0 Then
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = (expLdrB0(I, X1) + expLdrB0(I, X2)) / (expLdrB0(I, X0) + expLdrB0(I, X2))
                                End If
                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 2

                    Case 24 To 29:

                        For I = 1 To JB

                            If expLdrB0(I, X0) >= batTotGames!(I) * 3.1 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                If expLdrB0(I, X1) = 0 Then
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = expLdrB0(I, X0) / expLdrB0(I, X1)
                                End If
                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 3

                    Case 30:

                        For I = 1 To JB
                            If expLdrB0(I, X0) + expLdrB0(I, X1) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                If expLdrB0(I, X1) / (expLdrB0(I, X0) + expLdrB0(I, X1)) = 0 Then
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expBatLdrName$(I)
                                    expLdrTeam$(PPP) = expBatLdrTeam$(I)
                                    expLdrVal!(PPP) = expLdrB0(I, X1) / (expLdrB0(I, X0) + expLdrB0(I, X1))
                                End If
                            End If
                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 2

                End Select

                QW = PPP

                Select Case printOrView

                    Case 1:
                        Call DisplayBattingLeaders(TVN, QW, B9)

                    Case 2:
                        Call GetListLength(printOrView, QW, LG)

                        userKey$ = GetPrintOptions$

                        If userKey$ <> Chr$(27) Then

                            Select Case userKey$

                                Case "F"
                                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                    If userFile$ <> "" Then
                                        Call NewLeaders_Report(0, userFile$, TVN, LG, B9)
                                    End If

                                Case "P"
                                    Call NewLeaders_Report(1, "REPORT", TVN, LG, B9)

                            End Select

                        End If

                End Select

            End If

        Loop Until B9 = 31 'Done selecting number

    End If

End Sub


'----------------------------------------
'    DisplayBattingLeaders Subroutine
'----------------------------------------
'This subroutine displays expanded batting
'stat categories on the end user display
Sub DisplayBattingLeaders (TVN, maxRecords, categoryId)

    Shared expLdrVal!()
    Shared expLdrTeam$(), expBatCategory$(), expLdrPlyrName$()

    userExit% = 0

    Do

        Color 14, 0
        Cls

        For W = 1 To maxRecords

            ' Every 25 records ? new page (but skip prompt on the first page)
            If (W - 1) Mod 25 = 0 Then

                If W > 1 Then

                    Color 14
                    Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
                    Color 15

                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        userExit% = 1
                        Exit For
                    End If

                    Color 14, 0
                    Cls

                End If

                ' Print page header
                Locate 2, 1
                Print "     PLAYER                  TEAM                     "; expBatCategory$(categoryId)

            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "### "; W;
                Color 3: Print expLdrPlyrName$(W);
                Color 15
                Print Tab(25); expLdrTeam$(W);

                Select Case TVN
                    Case 1: Print Tab(58); Using "#####"; expLdrVal!(W)
                    Case 2: Print Tab(58); Using ".###"; expLdrVal!(W)
                    Case 3: Print Tab(58); Using "###.##"; expLdrVal!(W)
                End Select

            Else

                Exit Do

            End If

        Next W

        If userExit% <> 1 Then

            Color 14
            Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
            Color 15

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Color 14
                Locate LAST_ROW - 1, 15
                Print Space$(50);
                Locate LAST_ROW - 1, 32
                Print "VIEW AGAIN? (Y/N)";

                Do
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
Sub ViewPitchingLeaders (leaderFile$, JB, JP, plyrTotalStats, teamTotalStats)

    Shared expPitLdrName$()
    Shared expLdrTeam$(), expPitLdrTeam$(), expPitCategory$(), expLdrPlyrName$()
    Shared expLdrVal!(), expLdrTYP1!()
    Shared expLdrP0()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    ReDim pitTotGames!(880)
    Dim TYP2(920)

    If Not _FileExists(diskPaths$(2) + leaderFile$ + ".PIT") Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        TMR = (880 * 2)

        Open diskPaths$(2) + leaderFile$ + ".PIT" For Input As #1

        Input #1, JB, JP
        Input #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
        Input #1, plyrTotalStats, teamTotalStats

        For I = 1 To 880:
            Input #1, expPitLdrName$(I), expPitLdrTeam$(I), pitTotGames!(I)
            Color 14
            Locate 18, 38
            TMR = TMR - 1: Print Using "#####"; TMR
        Next I

        For I = 1 To 880:

            For I1 = 1 To 23:
                Input #1, expLdrP0(I, I1)
            Next I1

            Color 14
            Locate 18, 38
            TMR = TMR - 1: Print Using "#####"; TMR

        Next I

        Close 1

        Do

            Color 15, 0
            Cls

            Call ShadowBox(2, 4, 66, 20, 7, 10)

            Locate 10, 15

            Do

                For I = 0 To 14:
                    Color 14
                    Locate I + 3, 8: Print Using "##"; I;
                    Print " ";
                    Color 0: Print expPitCategory$(I);
                    Color 14: Print Tab(40); Using "##"; I + 15;
                    Print " ";
                    Color 0: Print expPitCategory$(I + 15)
                Next

                Print
                Locate , 23: Input "SELECT NUMBER TO VIEW"; categoryId

                sptl = 0
                PPP = 0

                Select Case categoryId
                    Case 0: X0 = 1
                    Case 1: X0 = 2
                    Case 2: X0 = 3
                    Case 3: X0 = 4
                    Case 4: X0 = 5
                    Case 5: X0 = 6
                    Case 6: X0 = 7
                    Case 7: X0 = 8
                    Case 8: X0 = 9
                    Case 9: X0 = 10
                    Case 10: X0 = 11
                    Case 11: X0 = 12
                    Case 12: X0 = 13
                    Case 13: X0 = 14
                    Case 14: X0 = 15
                    Case 15: X0 = 16
                    Case 16: X0 = 18
                    Case 17: X0 = 19
                    Case 18: X0 = 18: X1 = 19
                    Case 19: X0 = 20
                    Case 20: X0 = 22
                    Case 21: X0 = 1: X1 = 4
                    Case 22: X0 = 1: X1 = 5
                    Case 23: X0 = 1: X1 = 6
                    Case 24: X0 = 1: X1 = 2
                    Case 25: X0 = 1: X1 = 14
                    Case 26: X0 = 16
                    Case 27: X0 = 17
                    Case 28: X0 = 9: X1 = 21
                    Case 29: Exit Sub 'GoTo 70
                End Select

            Loop Until categoryId >= 0 And categoryId <= 29

            If categoryId <> 29 Then

                Call SortOptions(printOrView, sortType, newCategory)

                Cls
                PPP = 0

                Select Case categoryId
                    Case 0:
                        For I = 1 To JP
                            If expLdrP0(I, X0) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                TYP2(PPP) = expLdrP0(I, X0)
                            End If

                        Next I

                        For I = 2 To PPP:

                            stpl = stpl + 1
                            Locate 5, 5: Print "sorting player "; stpl

                            For J = PPP To I Step -1

                                If sortType = 1 Then
                                    If TYP2(J - 1) < TYP2(J) Then
                                        Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                                        Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                                        Swap TYP2(J - 1), TYP2(J)
                                    End If
                                Else
                                    'sortType = 2
                                    If TYP2(J - 1) > TYP2(J) Then
                                        Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                                        Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                                        Swap TYP2(J - 1), TYP2(J)
                                    End If
                                End If
                            Next J
                        Next I

                        For I = 1 To PPP
                            expLdrVal!(I) = (Int(TYP2(I) / 3))
                            expLdrTYP1!(I) = TYP2(I) - (Int(TYP2(I) / 3) * 3)
                        Next

                        TVN = 1

                    Case 1 To 16, 19, 20, 26, 27:
                        For I = 1 To JP

                            If expLdrP0(I, X0) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                expLdrVal!(PPP) = expLdrP0(I, X0)
                            End If
                        Next I

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 2

                    Case 18:
                        For I = 1 To JP
                            If expLdrP0(I, X0) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                If expLdrP0(I, X1) <> 0 Then
                                    expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                    expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                    expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                    expLdrVal!(PPP) = expLdrP0(I, X1) / expLdrP0(I, X0)
                                End If
                            End If
                        Next

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 4

                    Case 21 To 25:
                        For I = 1 To JP
                            If expLdrP0(I, X0) / 3 >= pitTotGames!(I) Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                If expLdrP0(I, X0) = 0 Or expLdrP0(I, X1) = 0 Then
                                    expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                    expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                    expLdrVal!(PPP) = 0
                                Else
                                    expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                    expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                    expLdrVal!(PPP) = (expLdrP0(I, X1) * 9) / (expLdrP0(I, X0) / 3)
                                End If
                            End If

                        Next

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 3

                    Case 28:
                        For I = 1 To JP
                            If expLdrP0(I, X1) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expPitLdrName$(I)
                                expLdrTeam$(PPP) = expPitLdrTeam$(I)
                                expLdrVal!(PPP) = expLdrP0(I, X1) / expLdrP0(I, X0)
                            End If
                        Next

                        Call SortPlayer(PPP, stpl, sortType)
                        TVN = 3

                End Select

                QW = PPP

                Select Case printOrView

                    Case 1
                        Call DisplayPitchingLeaders(TVN, QW, categoryId)
                    Case 2
                        Call GetListLength(printOrView, QW, LG)

                        Color 15, 0
                        Cls

                        Call MenuBox(8, 25, 24, 5, 15, 1)

                        Do

                            Locate 10, 32
                            Color 7, 1

                            Locate , 32: Print "SEND TO ";
                            Color 15: Print "F";
                            Color 7: Print "ILE"
                            Locate , 32: Print "SEND TO ";
                            Color 15: Print "P";
                            Color 7: Print "RINTER"

                            I$ = GetKeyPress$

                            Color 14, 0

                        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P"

                        Select Case UCase$(I$)

                            Case "F"
                                userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                If userFile$ <> "" Then
                                    Call NewLeaders_Report(0, userFile$, TVN, LG, B9)
                                End If

                            Case "P"
                                Call NewLeaders_Report(1, "REPORT", TVN, LG, B9)

                        End Select

                End Select

            End If

        Loop Until B9 = 29 'Done selecting number

    End If

End Sub


'----------------------------------------
'   DisplayPitchingLeaders Subroutine
'----------------------------------------
'This subroutine displays expanded pitching
'stat categories on the end user display
Sub DisplayPitchingLeaders (TVN, maxRecords, categoryId)

    Shared expLdrTeam$(), expPitCategory$(), expLdrPlyrName$()
    Shared expLdrVal!(), expLdrTYP1!()

    userExit% = 0

    Do

        Color 14, 0
        Cls

        For W = 1 To maxRecords

            ' Every 25 records ? new page (but skip prompt on the first page)
            If (W - 1) Mod 25 = 0 Then

                If W > 1 Then

                    Color 14
                    Locate LAST_ROW - 2: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                    Color 15

                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        userExit% = 1
                        Exit For
                    End If

                    Color 14, 0
                    Cls

                End If

                ' Print page header
                Locate 2, 1
                Color 14: Print "     PLAYER                  TEAM                     "; expPitCategory$(categoryId)
            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "### "; W;
                Color 3: Print expLdrPlyrName$(W);
                Color 15
                Print Tab(25); expLdrTeam$(W);

                Select Case TVN

                    Case 1:
                        Print Tab(58); Using "###"; expLdrVal!(W);
                        Print ".";
                        Print Using "#"; expLdrTYP1!(W)

                    Case 2:
                        Print Tab(58); Using "#####"; expLdrVal!(W)

                    Case 3:
                        Print Tab(58); Using "###.##"; expLdrVal!(W)

                    Case 4:
                        Print Tab(58); Using ".##"; expLdrVal!(W);: Print "%"

                End Select

            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

            Color 14
            Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
            Color 15

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Color 14
                Locate LAST_ROW - 1, 15
                Print Space$(50);
                Locate LAST_ROW - 1, 32
                Print "VIEW AGAIN? (Y/N)";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       ViewTeamLeaders Subroutine
'----------------------------------------
'This subroutine lets the user choose team
'player stat categories and relies on other
'routines to sort and display the results.
Sub ViewTeamLeaders (leaderFile$, JB, JP, plyrTotalStats, teamTotalStats)

    Shared divWins(), divLosses()

    Shared plyLdrVal!(), teamLdrVal!()
    Shared expLdrVal!(), expLdrTYP1!()

    Shared plyLeaderTeams$(), expLdrTeam$()
    Shared expBatCategory$(), expTeamCategory$(), tmLeaderTeams$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    If Not _FileExists(diskPaths$(2) + leaderFile$ + ".TOT") Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(2) + leaderFile$ + ".TOT" For Input As #1

        Input #1, JB, JP
        Input #1, div1Name$, div2Name$, div3Name$, div4Name$, league$
        Input #1, plyrTotalStats, teamTotalStats

        TMR = (TEAMS_PER_LEAGUE * 3)

        For I = 1 To TEAMS_PER_LEAGUE
            Input #1, divWins(I), divLosses(I)
        Next

        For I = 1 To TEAMS_PER_LEAGUE

            For I1 = 0 To 21:
                Input #1, plyLdrVal!(I, I1), plyLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 38
            TMR = TMR - 1: Print Using "#####"; TMR

        Next

        For I = 1 To TEAMS_PER_LEAGUE

            For I1 = 0 To 22:
                Input #1, teamLdrVal!(I, I1), tmLeaderTeams$(I, I1)
            Next

            Color 14
            Locate 18, 38
            TMR = TMR - 1: Print Using "#####"; TMR

        Next

        Close #1

        Do

            Color , 0
            Cls

            Call ShadowBox(2, 4, 66, 21, 7, 6)

            Locate 10, 15

            For I = 0 To 19:
                Color 11
                Locate I + 3, 8
                Print Using "##"; I;
                Print " ";
                Color 0: Print expTeamCategory$(I)
            Next

            For I = 20 To 39:
                Color 11
                Locate I - 17, 40
                Print Tab(40); Using "##"; I;
                Print " ";
                Color 0: Print expTeamCategory$(I)
            Next

            Do
                Color 14
                Locate 23, 23: Input "SELECT NUMBER TO VIEW"; categoryId

                PPP = 0
                stpl = 0

                Select Case categoryId
                    Case 0: X0 = 0: XX = 0
                    Case 1: X0 = 1: XX = 1
                    Case 2: X0 = 2: XX = 2
                    Case 3: X0 = 3: XX = 3
                    Case 4: X0 = 4: XX = 4
                    Case 5: X0 = 5: XX = 5
                    Case 6: X0 = 6: XX = 6
                    Case 7: X0 = 7: XX = 7
                    Case 8: X0 = 8: XX = 8
                    Case 9: X0 = 9: XX = 9
                    Case 10: X0 = 10: XX = 10
                    Case 11: X0 = 11: XX = 11
                    Case 12: X0 = 18: XX = 12
                    Case 13: X0 = 19: XX = 13
                    Case 14: X0 = 0: X1 = 1: XX = 14
                    Case 15: X0 = 0: X1 = 1: X2 = 4: X3 = 5: X4 = 6: XX = 15
                    Case 16: X0 = 0: X1 = 1: X2 = 7: XX = 16
                    Case 17: X0 = 12: XX = 17
                    Case 18: X0 = 15: XX = 18
                    Case 19: X0 = 16: X1 = 17: X2 = 11: XX = 19
                    Case 20: X0 = 0: XX = 20
                    Case 21: X0 = 1: XX = 21
                    Case 22: X0 = 2: XX = 22
                    Case 23: X0 = 3: XX = 23
                    Case 24: X0 = 4: XX = 24
                    Case 25: X0 = 5: XX = 25
                    Case 26: X0 = 10: XX = 26
                    Case 27: X0 = 11: XX = 27
                    Case 28: X0 = 12: XX = 28
                    Case 29: X0 = 13: XX = 29
                    Case 30: X0 = 14: XX = 30
                    Case 31: X0 = 17: XX = 31
                    Case 32: X0 = 18: XX = 32
                    Case 33: X0 = 17: X1 = 18: XX = 33
                    Case 34: X0 = 19: XX = 34
                    Case 35: X0 = 21: XX = 35
                    Case 36: X0 = 0: X1 = 3: XX = 36
                    Case 37: X0 = 15: XX = 37
                    Case 38: X0 = 16: XX = 38
                End Select

                If categoryId < 39 And categoryId >= 0 Then
                    Call SortOptions(printOrView, sortType, newCategory)
                    Cls
                End If

                Select Case categoryId

                    Case 0 To 13, 17, 18:
                        For I = 1 To plyrTotalStats
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = plyLdrVal!(I, X0)
                        Next

                        Call SortTeam(plyrTotalStats, stpl, sortType)

                        TVN = 1
                        maxRecords = plyrTotalStats
                        screenType = 0

                    Case 14
                        For I = 1 To plyrTotalStats
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = plyLdrVal!(I, X1) / plyLdrVal!(I, X0)
                        Next

                        Call SortTeam(plyrTotalStats, stpl, sortType)

                        TVN = 2
                        maxRecords = plyrTotalStats
                        screenType = 0

                    Case 15
                        For I = 1 To plyrTotalStats
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = (plyLdrVal!(I, X1) + plyLdrVal!(I, X2) + (plyLdrVal!(I, X3) * 2) + (plyLdrVal!(I, X4) * 3)) / plyLdrVal!(I, X0)
                        Next

                        Call SortTeam(plyrTotalStats, stpl, sortType)

                        TVN = 2
                        maxRecords = plyrTotalStats
                        screenType = 0

                    Case 16
                        For I = 1 To maxRecords
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = (plyLdrVal!(I, X1) + plyLdrVal!(I, X2)) / (plyLdrVal!(I, X0) + plyLdrVal!(I, X2))
                        Next

                        Call SortTeam(plyrTotalStats, stpl, sortType)

                        TVN = 2
                        maxRecords = plyrTotalStats
                        screenType = 0

                    Case 19
                        For I = 1 To maxRecords
                            expLdrTeam$(I) = plyLeaderTeams$(I, X0)
                            expLdrVal!(I) = (plyLdrVal!(I, X0) + plyLdrVal!(I, X1)) / (plyLdrVal!(I, X0) + plyLdrVal!(I, X1) + plyLdrVal!(I, X2))
                        Next

                        Call SortTeam(plyrTotalStats, stpl, sortType)

                        TVN = 2
                        maxRecords = plyrTotalStats
                        screenType = 0

                    Case 20
                        For I = 1 To teamTotalStats
                            expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                            expLdrVal!(I) = teamLdrVal!(I, X0)
                        Next

                        For I = 2 To teamTotalStats

                            stpl = stpl + 1
                            Locate 5, 5: Print "sorting team "; stpl

                            For J = teamTotalStats To I Step -1

                                If sortType = 1 Then
                                    If expLdrVal!(J - 1) < expLdrVal!(J) Then Swap expLdrTeam$(J - 1), expLdrTeam$(J): Swap expLdrVal!(J - 1), expLdrVal!(J)
                                Else
                                    'sortType = 2
                                    If expLdrVal!(J - 1) > expLdrVal!(J) Then Swap expLdrTeam$(J - 1), expLdrTeam$(J): Swap expLdrVal!(J - 1), expLdrVal!(J)
                                End If
                            Next J
                        Next I

                        For I = 1 To teamTotalStats
                            expLdrVal!(I) = (Int(expLdrVal!(I) / 3))
                            expLdrTYP1!(I) = expLdrVal!(I) - (Int(expLdrVal!(I) / 3) * 3)
                        Next

                        TVN = 1
                        maxRecords = teamTotalStats
                        screenType = 1

                    Case 21 To 32, 34, 35, 37, 38
                        For I = 1 To teamTotalStats
                            expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                            expLdrVal!(I) = teamLdrVal!(I, X0)
                        Next

                        Call SortTeam(teamTotalStats, stpl, sortType)

                        TVN = 2
                        maxRecords = teamTotalStats
                        screenType = 1

                    Case 33
                        For I = 1 To teamTotalStats
                            expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                            If teamLdrVal!(I, X0) = 0 Then
                                expLdrVal!(I) = 0
                            Else
                                expLdrVal!(I) = (teamLdrVal!(I, X1) / teamLdrVal!(I, X0)) * 100
                            End If
                        Next

                        Call SortTeam(teamTotalStats, stpl, sortType)

                        TVN = 3
                        maxRecords = teamTotalStats
                        screenType = 1

                    Case 36
                        For I = 1 To teamTotalStats
                            expLdrTeam$(I) = tmLeaderTeams$(I, X0)
                            expLdrVal!(I) = (teamLdrVal!(I, X1) * 9) / (teamLdrVal!(I, X0) / 3)
                        Next

                        Call SortTeam(teamTotalStats, stpl, sortType)

                        TVN = 3
                        maxRecords = teamTotalStats
                        screenType = 1

                    Case 39

                End Select

                If categoryId < 39 And categoryId >= 0 Then
                    Call DisplayTeamView(XX, screenType, printOrView, TVN, maxRecords)
                End If

            Loop Until categoryId >= 0 Or categoryId <= 39

        Loop Until categoryId = 39

    End If

End Sub


'----------------------------------------
'       DisplayTeamView Subroutine
'----------------------------------------
'This subroutine uses several different
'routines to display team total stats
Sub DisplayTeamView (categoryID, screenType, printOrView, TVN, maxRecords)

    Shared expLdrVal!(), expLdrTYP1!()

    Shared expLdrTeam$()
    Shared expBatCategory$(), expPitCategory$()

    If screenType = 0 Then

        Select Case printOrView

            Case 1:
                Call LeagueTotals_A_Screen(categoryID, TVN, maxRecords)

            Case 2:

                Color 15, 0
                Cls

                Call MenuBox(8, 25, 24, 5, 15, 1)

                Do

                    Locate 10, 32
                    Color 7, 1

                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "F";
                    Color 7: Print "ILE"
                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "P";
                    Color 7: Print "RINTER"

                    I$ = GetKeyPress$

                    Color 14, 0

                Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P"

                Select Case UCase$(I$)

                    Case "F"
                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                        End If

                    Case "P"
                        printDest = 1
                        userFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, "     TEAM                     "; expBatCategory$(categoryID)

                For W = 1 To maxRecords

                    Print #1, Using "### "; W;
                    Print #1, expLdrTeam$(W);

                    Select Case TVN

                        Case 1:
                            Print #1, Tab(32); Using "#####"; expLdrVal!(W)

                        Case 2:
                            Print #1, Tab(33); Using ".###"; expLdrVal!(W)

                    End Select

                Next W

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

        End Select

    Else

        Select Case printOrView

            Case 1:
                Call LeagueTotals_B_Screen(categoryID, TVN, maxRecords)

            Case 2:

                Color 15, 0
                Cls

                Call MenuBox(8, 25, 24, 5, 15, 1)

                Do

                    Locate 10, 32
                    Color 7, 1

                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "F";
                    Color 7: Print "ILE"
                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "P";
                    Color 7: Print "RINTER"

                    I$ = GetKeyPress$

                    Color 14, 0

                Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P"

                Select Case UCase$(I$)

                    Case "F"
                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                        End If

                    Case "P"
                        printDest = 1
                        userFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, "     TEAM                     "; expPitCategory$(categoryID)
                Print #1, "     ------                   ";
                Print #1, String$(Len(expPitCategory$(categoryID)),45)

                Print #1, Using "### "; W;
                Print #1, expLdrTeam$(W);:

                For W = 1 To maxRecords

                    Select Case TVN

                        Case 1:
                            Print #1, Tab(32); Using "#####"; expLdrVal!(W);
                            Print #1, ".";: Print #1, Using "#"; expLdrTYP1!(W)

                        Case 2:
                            Print #1, Tab(32); Using "#####"; expLdrVal!(W)

                        Case 3:
                            Print #1, Tab(32); Using "##.##"; expLdrVal!(W)

                    End Select

                Next W

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
Sub LeagueTotals_A_Screen (categoryID, TVN, maxRecords)

    Shared expLdrVal!()
    Shared expLdrTeam$(), expTeamCategory$()

    Do

        Color 14, 0
        Cls

        For W = 1 To maxRecords

            ' Every 25 records ? new page (but skip prompt on the first page)
            If (W - 1) Mod 25 = 0 Then

                If W > 1 Then

                    Color 14
                    Locate LAST_ROW - 2: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                    Color 15

                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        userExit% = 1
                        Exit For
                    End If

                    Color 14
                    Cls

                End If

                ' Print page header
                Locate 2, 1
                Print "     TEAM                     "; expTeamCategory$(categoryID)

            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "### "; W;
                Color 3
                Print expLdrTeam$(W);
                Color 15

                Select Case TVN

                    Case 1:
                        Print Tab(32); Using "#####"; expLdrVal!(W)

                    Case 2:
                        Print Tab(33); Using ".###"; expLdrVal!(W)

                End Select

            Else

                Exit Do

            End If

        Next W

        Color 14
        Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        Color 15

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Color 14
            Locate LAST_ROW - 1, 15
            Print Space$(50);
            Locate LAST_ROW - 1, 32
            Print "VIEW AGAIN? (Y/N)";

            Do
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
Sub LeagueTotals_B_Screen (categoryID, TVN, maxRecords)

    Shared expLdrTeam$(), expTeamCategory$()
    Shared expLdrVal!(), expLdrTYP1!()

    Do

        Color 14, 0
        Cls

        For W = 1 To maxRecords

            ' Every 25 records ? new page (but skip prompt on the first page)
            If (W - 1) Mod 25 = 0 Then

                If W > 1 Then

                    Color 14
                    Locate LAST_ROW - 2: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                    Color 15

                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        userExit% = 1
                        Exit For
                    End If

                    Color 14
                    Cls

                End If

                ' Print page header
                Locate 2, 1
                Print "     TEAM                     "; expTeamCategory$(categoryID)
            End If

            If userExit% <> 1 Then

                Color 12
                Print Using "### "; W;
                Color 3
                Print expLdrTeam$(W);
                Color 15

                Select Case TVN
                    '-- This is the only thing different between This
                    '-- and the LeagueTotals_A_Screen routine
                    Case 1:
                        Print Tab(32); Using "#####"; expLdrVal!(W);
                        Print ".";
                        Print Using "#"; expLdrTYP1!(W)

                    Case 2:
                        Print Tab(32); Using "#####"; expLdrVal!(W)

                    Case 3:
                        Print Tab(32); Using "##.##"; expLdrVal!(W)

                End Select

            Else

                Exit Do

            End If

        Next W

        Color 14
        Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        Color 15

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Color 14
            Locate LAST_ROW - 1, 15
            Print Space$(50);
            Locate LAST_ROW - 1, 32
            Print "VIEW AGAIN? (Y/N)";

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         SortOptions Subroutine
'----------------------------------------
'This subroutine prompts the users for the
'different sort options available in the
'"expanded" stats menus.

'sortType
'   1: Sort HIGH to LOW
'   2: Sort LOW to HIGH
Sub SortOptions (printOrView, sortType, reselect)

    Do

        Call MenuBox(8, 8, 57, 6, 15, 1)

        Locate 10, 15

        Color 3

        Locate , 30: Print "SELECT OPTION:"
        Locate , 30: Color 15: Print "H";
        Color 7: Print "IGHEST TO LOWEST"
        Locate , 30: Color 15: Print "L";
        Color 7: Print "OWEST TO HIGHEST"
        Locate , 30: Color 15: Print "R";
        Color 7: Print "E-SELECT CATEGORY"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

        Select Case UCase$(I$)

            Case "H":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P":
                        printOrView = 2
                    Case "V":
                        printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 1

            Case "L":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P":
                        printOrView = 2
                    Case "V":
                        printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 2

            Case "R", Chr$(27):
                reselect = 1

        End Select

    Loop Until UCase$(I2$) <> "R" Or UCase$(I$) = "R"

End Sub


'----------------------------------------
'       PrintViewReselect Subroutine
'----------------------------------------
'This subroutine presents the end user
'with a choice to Print or View the exist-
'ing category, or select a new category
Sub PrintViewReselect (userOpt$)

    Call MenuBox(9, 15, 40, 4, 15, 0)
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";
    Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";
    Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";
    Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'This subroutine sorts the players involved
'in the chosen stat category
Sub SortPlayer (PPP, stpl, sortType)

    Shared expLdrTeam$(), expLdrPlyrName$()
    Shared expLdrVal!()

    For I = 2 To PPP

        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType = 1 Then
                If expLdrVal!(J - 1) < expLdrVal!(J) Then
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                    Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                End If
            End If

            If sortType = 2 Then
                If expLdrVal!(J - 1) > expLdrVal!(J) Then
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                    Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
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
Sub SortTeam (sortMax, stpl, sortType)

    Shared expLdrVal!()
    Shared expLdrTeam$()

    For I = 2 To sortMax

        stpl = stpl + 1
        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType = 1 Then
                If expLdrVal!(J - 1) < expLdrVal!(J) Then
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                End If
            End If

            If sortType = 2 Then
                If expLdrVal!(J - 1) > expLdrVal!(J) Then
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'     NewLeaders_Report Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the default printer
Sub NewLeaders_Report (printDest, destFile$, TVN, maxRecords, categoryID)

    Shared expLdrVal!()
    Shared expBatCategory$(), expLdrTeam$(), expLdrPlyrName$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    For W = 1 To maxRecords

        If (W - 1) Mod 55 = 0 Then
            Print #1, Chr$(12)
            Print #1, "     PLAYER                  TEAM                     "; expBatCategory$(categoryID)
            Print #1, "     ------                  ----                     ";
            Print #1, String$(Len(expBatCategory$(categoryID)),45)
        End If

        Print #1, Using "### "; W;
        Print #1, expLdrPlyrName$(W);
        Print #1, Tab(25); expLdrTeam$(W);

        Select Case TVN

            Case 1:
                Print #1, Tab(58); Using "#####"; expLdrVal!(W)

            Case 2:
                Print #1, Tab(58); Using ".###"; expLdrVal!(W)

            Case 3:
                Print #1, Tab(58); Using "###.##"; expLdrVal!(W)

        End Select

    Next W

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            Print #1, X$
        Loop

        Close #2

        Print #1, Chr$(12)

        Kill destFile$

    End If

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
Sub GetListLength (printOrView, listLength, maxItems)

    Color , 0
    Cls

    If printOrView <> 1 Then

        Do
            Color , 0
            Cls

            Call MenuBox(10, 20, 30, 2, 4, 15)

            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub
