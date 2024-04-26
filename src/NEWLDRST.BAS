'----------------------------------------
'           NEWLDRST Subroutine
'----------------------------------------
'This subroutine presents the user options
'for navigating the "Expanded" standings
Sub ExpandedStandings ()

    _Title "Full Count Baseball - Expanded Leaders / Standings"

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared expLL!(), LLS!(), lastTenLosses!(), lastTenWins!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$(), Z1$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Erase CLT!, CWS!
    ERase LLS!, LWS!
    Erase TLS!, TWS!

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE EXPANDED LEAGUE STANDINGS      "
        menuChoices$(2) = "VIEW / PRINT EXPANDED LEAGUE STANDINGS"
        menuChoices$(3) = "RETURN TO MAIN MENU                   "
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
                                Input #1, TML$(I), WW!(I), expLL!(I), TWS!(I), TLS!(I), CWS!(I), CLT!(I), HWW!(I), HLL!(I), RWW!(I), RLL!(I), EWW!(I), ELL!(I), lastTenWins!(I), lastTenLosses!(I)
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

    Loop Until Selection% = 3 Or ExitCode% = 27

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

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!() 
    Shared HLL!(), HWW!()
    Shared expLL!(), lastTenWins!(), lastTenLosses!()
    Shared LLS!(), LWS!()
    Shared RLL!(), RWW!()
    Shared TLS!(), TWS!(), WW!()

    Shared TML$(), Z1$()

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

        If Z1$(KK) <> "*" Then

            TML$(currRecord) = Z1$(KK)

            Color 15, 0
            Cls

            Locate 5: Print "COMPUTING RECORDS . . . . . PLEASE WAIT"

            If _FileExists(diskPaths$(1) + Z1$(KK) + "S") Then

                Open diskPaths$(1) + Z1$(KK) + "S" For Input As #1

                Input #1, nbrGames

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

                For I = 1 To nbrGames

                    If teamScore(I) > oppScore(I) Then 
                        WW!(currRecord) = WW!(currRecord) + 1
                        If nbrInnings(I) > 9 Then EWW!(currRecord) = EWW!(currRecord) + 1
                        If locIndicator$(I) = "A" Then RWW!(currRecord) = RWW!(currRecord) + 1
                        If locIndicator$(I) = "H" Then HWW!(currRecord) = HWW!(currRecord) + 1
                    End If

                    If oppScore(I) > teamScore(I) Then 
                        expLL!(currRecord) = expLL!(currRecord) + 1
                        If nbrInnings(I) > 9 Then ELL!(currRecord) = ELL!(currRecord) + 1
                        If locIndicator$(I) = "H" Then HLL!(currRecord) = HLL!(currRecord) + 1
                        If locIndicator$(I) = "A" Then RLL!(currRecord) = RLL!(currRecord) + 1
                    End If

                    If teamScore(I) > oppScore(I) Then LWS!(currRecord) = LWS!(currRecord) + 1: LLS!(currRecord) = 0
                    If LWS!(currRecord) > TWS!(currRecord) Then TWS!(currRecord) = LWS!(currRecord)
                    If teamScore(I) < oppScore(I) Then LLS!(currRecord) = LLS!(currRecord) + 1: LWS!(currRecord) = 0
                    If LLS!(currRecord) > TLS!(currRecord) Then TLS!(currRecord) = LLS!(currRecord)

                    If teamScore(I) <= oppScore(I) Then
                        If teamScore(I) < oppScore(I) Then
                            CLT!(currRecord) = CLT!(currRecord) + 1
                            CWS!(currRecord) = 0
                        Else
                            CWS!(currRecord) = CWS!(currRecord) + 1
                            CLT!(currRecord) = 0
                        End If
                    End If

                Next I

                If nbrGames <= 10 Then
                    lastTenLosses!(currRecord) = expLL!(currRecord)
                    lastTenWins!(currRecord) = WW!(currRecord)
                Else
                    M = nbrGames - 9

                    For J = M To nbrGames
                        If teamScore(J) > oppScore(J) Then lastTenWins!(currRecord) = lastTenWins!(currRecord) + 1
                        If oppScore(J) > teamScore(J) Then lastTenLosses!(currRecord) = lastTenLosses!(currRecord) + 1
                    Next J
                End If

            End If

        End If 'Done checking for '*'

    Next KK

    Open diskPaths$(2) + league$ + ".STD" For Output As #1

    Write #1, league$, div1Name$, div2Name$, div3Name$, div4Name$

    For I = 1 To TEAMS_PER_LEAGUE
        Write #1, TML$(I), WW!(I), expLL!(I), TWS!(I), TLS!(I), CWS!(I), CLT!(I), HWW!(I), HLL!(I), RWW!(I), RLL!(I), EWW!(I), ELL!(I), lastTenWins!(I), lastTenLosses!(I)
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

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared expLL!(), LLS!(), lastTenLosses!(), lastTenWins!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    For NT = 1 To 4

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Color 11
        Locate 1, 35: Print league$
        Color 14
        Locate 2: Print Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
        Locate 3

        Select Case NT

            Case 1
                Print #1, div1Name$;

            Case 2
                Print #1, div2Name$;

            Case 3
                Print #1, div3Name$;

            Case 4
                Print #1, div4Name$;

        End Select

        Print Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                Color 3
                Print TML$(I);
                Color 15

                'G / W / L
                Print Tab(14); Using "###"; WW!(I) + expLL!(I); Tab(18);
                Print Using "###"; WW!(I); Tab(22);
                Print Using "###"; expLL!(I);

                ' PCT
                If expLL!(I) = 0 And expLL!(I) + WW!(I) <> 0 Then
                    Print Tab(26); "1.000";
                Else
                    If WW!(I) = 0 Then
                        Print Tab(27); ".000";
                    Else
                        If WW!(I) + expLL!(I) <> 0 Then
                            wlPCT! = WW!(I) / (WW!(I) + expLL!(I))
                            Print Tab(27); Using ".###"; wlPCT!;
                        Else
                            If WW!(I) + expLL!(I) = 0 Then Print Tab(27); ".000";
                        End If
                    End If
                End If

                ' GB
                calcGB! = ((WW!(AA) - WW!(I)) + (expLL!(I) - expLL!(AA))) / 2
                If calcGB! = 0 Then
                    Print Tab(32); " ---";
                Else
                    Print Tab(32); Using "##.#"; calcGB!;
                End If

                Print Tab(44); Using "##"; TWS!(I); Tab(47); TLS!(I); Tab(51); CWS!(I); Tab(54); CLT!(I);
                Print Tab(57); Using "##"; HWW!(I);: Print Tab(60); Using "##"; HLL!(I);
                Print Tab(63); Using "##"; RWW!(I);: Print Tab(66); Using "##"; RLL!(I);
                Print Tab(69); Using "##"; EWW!(I);: Print Tab(72); Using "##"; ELL!(I);
                Print Tab(75); Using "##"; lastTenWins!(I);: Print Tab(78); Using "##"; lastTenLosses!(I)

            End If

        Next

        'MAG #
        MJ = 163 - WW!(AA) - expLL!(AA + 1)
        If MJ < 0 Then MJ = 0

        Locate 4, 38
        Print Using "###"; MJ;

        Color 14
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

    Next NT

End Sub

'----------------------------------------
'       NewStandings_Report Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub NewStandings_Report (printDest, destFile$, league$)

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared expLL!(), LLS!(), lastTenLosses!(), lastTenWins!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

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

        Print #1, Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"

        Select Case NT

            Case 1
                Print #1, div1Name$;

            Case 2
                Print #1, div2Name$;

            Case 3
                Print #1, div3Name$;

            Case 4
                Print #1, div4Name$;

        End Select

        Print #1, Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                Print #1, TML$(I);
                Print #1, Tab(14); Using "###"; WW!(I) + expLL!(I); Tab(18);
                Print #1, Using "###"; WW!(I); Tab(22);
                Print #1, Using "###"; expLL!(I);

                'PCT
                If expLL!(I) = 0 And expLL!(I) + WW!(I) <> 0 Then
                    Print #1, Tab(26); "1.000";
                Else
                    If WW!(I) = 0 Then
                        Print #1, Tab(27); ".000";
                    Else
                        If WW!(I) + expLL!(I) = 0 Then
                            Print #1, Tab(27); ".000";
                        Else
                            wlPCT! = WW!(I) / (WW!(I) + expLL!(I))
                            Print #1, Tab(27); Using ".###"; wlPCT!;
                        End If
                    End If
                End If

                'GB
                If ((WW!(AA) - WW!(I)) + (expLL!(I) - expLL!(AA))) / 2 = 0 Then
                    Print #1, Tab(32); " ---";
                Else
                    Print #1, Tab(32); Using "##.#"; ((WW!(NT * 10 - 9) - WW!(I)) + (expLL!(I) - expLL!(NT * 10 - 9))) / 2;
                End If

                If I = AA Then
                    MJ = 163 - WW!(AA) - expLL!(AA + 1)
                    If MJ < 0 Then MJ = 0
                    Print #1, Tab(38); Using "###"; MJ
                End If

                Print #1, Tab(44); Using "##"; TWS!(I); Tab(47); TLS!(I); Tab(51); CWS!(I); Tab(54); CLT!(I);
                Print #1, Tab(57); Using "##"; HWW!(I);: Print #1, Tab(60); Using "##"; HLL!(I);
                Print #1, Tab(63); Using "##"; RWW!(I);: Print #1, Tab(66); Using "##"; RLL!(I);
                Print #1, Tab(69); Using "##"; EWW!(I);: Print #1, Tab(72); Using "##"; ELL!(I);
                Print #1, Tab(75); Using "##"; lastTenWins!(I);: Print #1, Tab(78); Using "##"; lastTenLosses!(I)

            End If

        Next

        Print #1,
        Print #1,

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

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared expLL!(), LLS!(), lastTenLosses!(), lastTenWins!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = WW!(X) - expLL!(X)
            diffY = WW!(Y) - expLL!(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (WW!(X) <= WW!(Y) And WW!(X) > expLL!(X)) Or Not (WW!(X) >= WW!(Y) And WW!(X) < expLL!(X)) Then

                        Swap TML$(X), TML$(Y)
                        Swap TWS!(X), TWS!(Y)
                        Swap TLS!(X), TLS!(Y)
                        Swap CWS!(X), CWS!(Y)
                        Swap CLT!(X), CLT!(Y)
                        Swap WW!(X), WW!(Y)
                        Swap expLL!(X), expLL!(Y)
                        Swap EWW!(X), EWW!(Y)
                        Swap ELL!(X), ELL!(Y)
                        Swap HLL!(X), HLL!(Y)
                        Swap RLL!(X), RLL!(Y)
                        Swap HWW!(X), HWW!(Y)
                        Swap RWW!(X), RWW!(Y)
                        Swap lastTenLosses!(X), lastTenLosses!(Y)
                        Swap lastTenWins!(X), lastTenWins!(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub

