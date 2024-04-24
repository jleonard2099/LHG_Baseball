'----------------------------------------
'            CNGTM Subroutine
'----------------------------------------
'This subroutine presents a menu of all
'the options related to team rosters
Sub CNGTM (userChoice)

    _Title "Courtside College Basketball - Change Team"

    ReDim menuChoices$(1 To 4)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER  "
        menuChoices$(3) = "VIEW A TEAM ROSTER   "
        menuChoices$(4) = "EXIT TO MAIN MENU    "
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

        If (userChoice <> 0) Then

            Selection% = userChoice

        Else

            Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            Color , 0
        End If

        If ExitCode% <> 27 And Selection% <> 4 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                Select Case Selection%

                    Case 1:
                        Call InputEditTeam(2, YN$, team$, teamNbr)
                        statFile$ = nameStatFile$(team$)
                        Call SaveTeamFile(0, YN$, team$, statFile$, teamNbr)

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
                                userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                                If userFile$ <> "" Then
                                    Call PrintTeamStats(0, userFile$, team$)
                                End If

                            Case "P"
                                Call PrintTeamStats(1, "REPORT", team$)

                        End Select

                    Case 3:
                        Call ViewRoster(team$)

                End Select

            End If

            'Reset "user choice" after first call
            'We're at the menu now, no purpose in forcing shortcuts
            userChoice = 0

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'       PrintTeamStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'previously loaded team stat data and sends
'it to the default printer
Sub PrintTeamStats (printDest, destFile$, targetTeam$)

    Shared batterNames$(), parkType$(), pitcherNames$()

    Shared batterRatings(), pitcherRatings(), teamRatings%()

    Shared CK, parkHR
    Shared Manager$, teamAbbrev$, Stadium$

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    Print #1, "TEAM NAME "; targetTeam$; Tab(30); "MANAGER: "; Manager$; Tab(50); "STADIUM: "; Stadium$

    Print #1,
    Print #1, "LBA LSO LBB LHR DPG DBL TR    HR   FOUL PARK INFO"

    Print #1, Using "### ### ### ### ### ### ### ###### ###  "; teamRatings%(1); teamRatings%(2); teamRatings%(3); teamRatings%(4); teamRatings%(5); teamRatings%(6); teamRatings%(7); parkHR; teamRatings%(8);: Print #1, parkType$(teamRatings%(9)); " "; parkType$(teamRatings%(10) + 2)

    Print #1,
    Print #1, "BATTER      B    G  AB   R   H DB TR HR RBI  BB  SO  SB CS RN B GA P1 FAV1 A R P2 FAV2 A R P3 FAV3 A R P4 FAV4 A R EBA BAVG"

    For I = 0 To 22

        If batterNames$(I) <> "" And batterNames$(I) <> "XXX" Then

            Print #1, Left$(batterNames$(I), 12); Tab(13);

            If batterRatings(I, 0) = 0 Then Print #1, "S  ";
            If batterRatings(I, 0) = -1 Then Print #1, "L  ";
            If batterRatings(I, 0) = 1 Then Print #1, "R  ";
            If batterRatings(I, 0) = -2 Then Print #1, "LP ";
            If batterRatings(I, 0) = 2 Then Print #1, "RP ";

            Print #1, Using "### ### ### ### ## ## ## ### ### ### ### ##  "; batterRatings(I, 3); batterRatings(I, 4); batterRatings(I, 5); batterRatings(I, 6); batterRatings(I, 7); batterRatings(I, 8); batterRatings(I, 9); batterRatings(I, 10); batterRatings(I, 11); batterRatings(I, 12); batterRatings(I, 13); batterRatings(I, 14);

            Print #1, Using "# "; batterRatings(I, 16);: Print #1, Using "# "; batterRatings(I, 30);

            Print #1, Using "## ## #### # # "; batterRatings(I, 18); batterRatings(I, 22); batterRatings(I, 26); batterRatings(I, 33); batterRatings(I, 34);

            If batterRatings(I, 23) >= 0 Then
                Print #1, Using "## #### # # "; batterRatings(I, 23); batterRatings(I, 27); batterRatings(I, 35); batterRatings(I, 36);
            End If

            If batterRatings(I, 24) >= 0 Then
                Print #1, Using "## #### # # "; batterRatings(I, 24); batterRatings(I, 28); batterRatings(I, 37); batterRatings(I, 38);
            End If

            If batterRatings(I, 25) >= 0 Then
                Print #1, Using "## #### # # "; batterRatings(I, 25); batterRatings(I, 29); batterRatings(I, 39); batterRatings(I, 40);
            End If

            Print #1, Tab(116); Using "### "; batterRatings(I, 20);

            If batterRatings(I, 4) <> 0 Then
                Z% = batterRatings(I, 6) / batterRatings(I, 4) * 1000
                Print #1, Using " ###"; batterRatings(I, 6) / batterRatings(I, 4) * 1000
            Else
                Print #1, " 000"
            End If

        End If

    Next I

    For I = 0 To 21
        If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then

            Print #1, Left$(pitcherNames$(I), 12); Tab(13);

            If pitcherRatings(I, 0) = 1 Then Print #1, "R  ";
            If pitcherRatings(I, 0) = -1 Then Print #1, "L  ";

            Print #1, Using "### ### ### ### ## ## ## ### ### ### ### ## "; pitcherRatings(I, 19); pitcherRatings(I, 20); pitcherRatings(I, 21); pitcherRatings(I, 22); pitcherRatings(I, 23); pitcherRatings(I, 24); pitcherRatings(I, 25); pitcherRatings(I, 26); pitcherRatings(I, 27); pitcherRatings(I, 28); pitcherRatings(I, 29); pitcherRatings(I, 30);

            Print #1, Using " # # 52  1 ####                                         ### "; pitcherRatings(I, 31); pitcherRatings(I, 32); pitcherRatings(I, 17); pitcherRatings(I, 14);

            If pitcherRatings(I, 20) <> 0 Then
                Z% = pitcherRatings(I, 22) / pitcherRatings(I, 20) * 1000: Print #1, Using " ###"; pitcherRatings(I, 22) / pitcherRatings(I, 20) * 1000
            Else
                Print #1, " 000"
            End If

        End If

    Next I

    Print #1,

    Print #1, "PITCHER     T  W  L  SV G  GS CG IP  H   BB  SO  ERA  GA HRA HOLD EBA"

    For I = 0 To 21
        If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then

            Print #1, Mid$(pitcherNames$(I), 1, 12); Tab(13);

            If pitcherRatings(I, 0) = 1 Then Print #1, "R  ";
            If pitcherRatings(I, 0) = -1 Then Print #1, "L  ";

            Print #1, Using "## ## ## ## ## ## ### ### ### ### #.## ## ##   #   ###"; pitcherRatings(I, 1); pitcherRatings(I, 2); pitcherRatings(I, 3); pitcherRatings(I, 4); pitcherRatings(I, 5); pitcherRatings(I, 16); pitcherRatings(I, 6); pitcherRatings(I, 7); pitcherRatings(I, 8); pitcherRatings(I, 9); pitcherRatings(I, 10) / 100; pitcherRatings(I, 12); pitcherRatings(I, 13); pitcherRatings(I, 18); pitcherRatings(I, 14)
        End If

    Next I

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
'         ViewRoster subroutine
'----------------------------------------
'This subroutine formats and displays the
'general stats for a team, then provides
'a menu for choosing options to display
'various aspects of the roster
Sub ViewRoster (targetTeam$)

    Shared ERX!

    Shared parkHR

    Shared teamRatings%(), TB%(), inputTP%()

    Shared batterNames$(), pitcherNames$()
    Shared batterRatings(), pitcherRatings()

    Erase TB%, inputTP%

    For K = 4 To 18:
        For I = 0 To 22
            If batterNames$(I) <> "XXX" Then
                TB%(K - 3) = TB%(K - 3) + batterRatings(I, K)
            End If
        Next
    Next

    For K = 4 To 18:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" And pitcherRatings(I, 11) = 999 Then
                TB%(K - 3) = TB%(K - 3) + pitcherRatings(I, K + 16)
            End If
        Next
    Next

    For K = 1 To 33:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" Then
                inputTP%(K) = inputTP%(K) + pitcherRatings(I, K)
            End If
        Next
    Next

    ERX! = 0

    For I = 0 To 21
        If pitcherNames$(I) <> "XXX" Then
            ERX! = ERX! + ((pitcherRatings(I, 6) / 9) * (pitcherRatings(I, 10) / 100))
        End If
    Next

    Color 15, 0
    Cls

    Color 12
    Locate 5, 10: Print targetTeam$
    Color 3
    Locate 7, 10: Print "LEAGUE BATTING AVG.";
    Color 7
    Locate 7, 38: Print Using "#####"; teamRatings%(1)
    Color 3
    Locate , 10: Print "LEAGUE STRIKE OUT AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(2)
    Color 3
    Locate , 10: Print "LEAGUE BASE ON BALLS AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(3)
    Color 3
    Locate , 10: Print "LEAGUE HOME RUN AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(4)
    Color 3
    Locate , 10: Print "DOUBLE PLAYS AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(5)
    Color 3
    Locate , 10: Print "PARK DOUBLE ADJ.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(6)
    Color 3
    Locate , 10: Print "PARK TRIPLE ADJ.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(7)
    Color 3
    Locate , 10: Print "PARK HOME RUN ADJ.";
    Color 7
    Locate , 37: Print Using "######"; parkHR
    Color 3
    Locate , 10: Print "PARK FOUL GROUND ADJ.";
    Color 7
    Locate , 37: Print Using "######"; teamRatings%(8)

    Color 11
    Locate 23, 10: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

    Do

        Cls
        Color 12: Print "TEAM OPTIONS"
        Print
        Color 14: Print "(1) ";
        Color 15: Print "VIEW HITTERS"
        Print
        Color 14: Print "(2) ";
        Color 15: Print "VIEW PITCHERS"
        Print
        Color 14: Print "(3) ";
        Color 15: Print "VIEW PITCHERS HITTING/FLDG STATS"
        Print
        Color 14: Print "(4) ";
        Color 15: Print "RETURN TO MAIN MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 4

        Select Case I

            Case 1:
                Call ViewHitters (T$)

            Case 2:
                Call ViewPitchers (T$)

            Case 3:
                Call ViewHittingFielding

        End Select

    Loop Until I = 4

End Sub
