
'----------------------------------------
'          CREATLEAG Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of leagues
Sub CREATLEAG ()

    _Title "Full Count Baseball - Manage Leagues"

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared leagueName$
    Shared div1Name$, div2Name$, div3Name$, div4Name$

    ReDim menuChoices$(1 To 3)

    Do

        Color 15, 0
        Cls

        Call TITLE
        menuChoices$(1) = "CREATE A LEAGUE       "
        menuChoices$(2) = "MODIFY LEAGUE MEMBERS "
        menuChoices$(3) = "RETURN TO LEADERS MENU"
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To TEAMS_PER_LEAGUE
                memberIdx(X) = -1
                memberName$(X) = "XXX"
                memberYear$(X) = "XXX"
            Next

            div1Name$ = ""
            div2Name$ = ""
            div3Name$ = ""
            div4Name$ = ""

            Select Case SELECTION%

                Case 1:
                    Call DefineLeague
                    Call ReviewEditLeague(leagueName$)

                Case 2:
                    leagueFile$ = _OpenFileDialog$("Choose League Definition file", diskPaths$(2), "*.TMI", "League Definition files [.TMI]")
                    'ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", FileExit%)

                    If leagueFile$ <> "" Then
                        Call ReviewEditLeague(GetFileBaseName$(leagueFile$))
                    End If

            End Select

        End If 'Done checking for ESC

    Loop Until SELECTION% = 3 Or ExitCode% = 27

    Color 15, 0
    Cls

End Sub


'----------------------------------------
'       DefineLeague Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the league's
'general naming and setup
Sub DefineLeague ()

    Shared leagueName$
    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Do

            Locate 7,

            Locate , 12: Print "ENTER NAME FOR LEAGUE: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
            Locate , 12: Print "SAMPLE NAMES ARE NL1990 AL56 1956AL NATIONAL"
            Locate 7, 33: Input leagueName$

            If leagueName$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    FG = 0
    BG = 7

    Cls
    Color 15, 1

    Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div1Name$, 50), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2Name$, 50), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div3Name$, 50), FG, BG, -1
    Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div4Name$, 50), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div1Name$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div2Name$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div3Name$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 11, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div4Name$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate LAST_ROW - 2, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv4
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

End Sub


'----------------------------------------
'      ReviewEditLeague Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the league definition.
Sub ReviewEditLeague (leagueFile$)

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared leagueName$
    Shared div1Name$, div2Name$, div3Name$, div4Name$

    targetFile$ = diskPaths$(2) + leagueFile$ + ".TMI"

    If _FileExists(targetFile$) Then
        Call ReadLeagueIndex (targetFile$)
    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; leagueName$; " LEAGUE "
        Print

        Color 12
        Locate , 4
        Print div1Name$; Tab(24); div2Name$; Tab(44); div3Name$; Tab(64); div4Name$
        Print

        For I = 1 To TEAMS_PER_DIV

            Color 2: Print Using "## "; I;
            Color 15: Print Left$(memberName$(I), 17);

            Locate , 21
            Color 2: Print Using "## "; I + 10;
            Color 15: Print Left$(memberName$(I + 10), 17);

            Locate , 41
            Color 2: Print Using "## "; I + 20;
            Color 15: Print Left$(memberName$(I + 20), 17);

            Locate , 61
            Color 2: Print Using "## "; I + 30;
            Color 15: Print Left$(memberName$(I + 30), 17)

        Next I

        Do

            Locate 21, 1

            Color 14: Print "(1) ";
            Color 15: Print "ADD A TEAM"
            Color 14: Print "(2) ";
            Color 15: Print "REMOVE A TEAM"
            Color 14: Print "(3) ";
            Color 15: Print "END AND SAVE LEAGUE"

            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If choice <> 3 Then

            Do
                Locate 22, 35
                Print "SLOT # (1-" + Str$(TEAMS_PER_LEAGUE) + ") ";
                Input teamIndex
            Loop Until teamIndex >= 1 Or teamIndex <= TEAMS_PER_LEAGUE

            If choice = 1 Then
                'Add team
                'teamNbr = 0
                Call ChooseTeam(memberYear$(teamIndex), NTMS, memberIdx(teamIndex), memberName$(teamIndex))
            Else
                'Remove team
                memberName$(teamIndex) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveLeague(leagueFile$)

End Sub


'----------------------------------------
'        SaveLeague Subroutine
'----------------------------------------
'This subroutine writes the league team
'memberships and definition to each of
'the corresponding files (.TMI, .TMS)
Sub SaveLeague (leagueFile$)

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared leagueName$
    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Dim statFileNames$(TEAMS_PER_LEAGUE)

    Open diskPaths$(2) + leagueFile$ + ".TMI" For Output As #2

    Print #2, leagueName$
    Print #2, div1Name$
    Print #2, div2Name$
    Print #2, div3Name$
    Print #2, div4Name$

    For X = 1 To TEAMS_PER_LEAGUE
        Print #2, memberName$(X)
        Print #2, memberIdx(X)
        Print #2, memberYear$(X)
    Next

    Close #2

    For X = 1 To TEAMS_PER_LEAGUE

        If memberName$(X) = "XXX" Then
            statFileNames$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            statFileNames$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Open diskPaths$(2) + leagueFile$ + ".TMS" For Output As #2

    Write #2, leagueName$
    Write #2, div1Name$
    Write #2, div2Name$
    Write #2, div3Name$
    Write #2, div4Name$

    For X = 1 To TEAMS_PER_LEAGUE
        Write #2, statFileNames$(X)
    Next

    Close 2

End Sub

