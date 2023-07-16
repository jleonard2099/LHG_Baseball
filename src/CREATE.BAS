Sub CREATE ()

    _Title "Full Count Baseball - Manage Leagues"

    Shared Z1%()

    Shared Z1$(), Z2$()

    Do

        Color 15,0
        Cls

        Call TITLE

        ReDim menuChoices$(1 to 3)
        menuChoices$(1) = "CREATE / DEFINE NEW LEAGUES     "
        menuChoices$(2) = "ADD / REVIEW LEAGUE MEMBERS     "
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 6
        Row% = 8

        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2
        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColr%, FileBGColr%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            For X = 1 To 40
                Z1$(X) = "XXX"
                Z2$(X) = "XXX"
                Z1%(X) = -1
            Next

            Select Case Selection%

                Case 1:
                    Call DefineLeague(LG$, D1$, D2$, D3$, D4$)
                    Call ReviewEditLeague(LG$, D1$, D2$, D3$, D4$)

                Case 2:
                    Call ChooseLeague(LG$)
                    Call ReviewEditLeague(LG$, D1$, D2$, D3$, D4$)

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineLeague Subroutine
'----------------------------------------
'...explanation...
Sub DefineLeague(LG$, D1$, D2$, D3$, D4$)

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8: Color 15, 1

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES."
            Locate , 12: Print "SAMPLE NAMES ARE NL1990 AL56 1956AL NATIONAL"
            Locate 7, 37: Input LG$

            If LG$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
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
    Locate , 21: QPrint PadRight$(D1$, 8), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D2$, 8), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D3$, 8), FG, BG, -1
    Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D4$, 8), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 11, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D4$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate 23, 1
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
'      ChooseLeague Subroutine
'----------------------------------------
'...explanation...
Sub ChooseLeague (league$)

    Shared Z1%()

    Shared Z1$(), Z2$()

    Do

        Spec$ = diskPaths$(2) + "*.LIB"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim leagueFiles$(0 To Count%)

            For X = 1 To Count%
                leagueFiles$(X) = Space$(12)
            Next

            leagueFiles$(0) = Spec$

            Call ReadFile(leagueFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE TO SCHEDULE": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = leagueFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                league$ = Left$(targetFile$, Len(targetFile$) - 4)

            End If 'Done checking for ESC

        End If 'Done checking for files

    Loop Until UCase$(I$) = "N" Or ExitCode% = 27 Or Count% = 0

    Color 15, 0
    Cls

End Sub

'----------------------------------------
'       ReviewEditLeague Subroutine
'----------------------------------------
'...explanation...
Sub ReviewEditLeague (league$, D1$, D2$, D3$, D4$)

    Shared Z1%()

    Shared Z1$(), Z2$()

    If _FileExists (diskPaths$(2) + league$ + ".LIB") Then

        Open diskPaths$(2) + league$ + ".LIB" For Input As #2

        Input #2, league$, D1$, D2$, D3$, D4$

        For X = 1 To 40
            Input #2, Z1$(X)
            Input #2, Z1%(X)
            Input #2, Z2$(X)
        Next

        Close 2

    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; league$; " LEAGUE "
        Print

        Color 12
        Locate , 4
        Print D1$; Tab(24); D2$; Tab(44); D3$; Tab(64); D4$
        Print

        For I = 1 To 10

            Color 2
            Print Using "## "; I;
            Color 15: Print Z1$(I);

            Locate , 21
            Color 2: Print Using "## "; I + 10;
            Color 15: Print Z1$(I + 10);

            Locate , 41
            Color 2: Print Using "## "; I + 20;
            Color 15: Print Z1$(I + 20);

            Locate , 61
            Color 2: Print Using "## "; I + 30;
            Color 15: Print Z1$(I + 30)

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
                Print "SLOT # (1-40) ";
                Input B
            Loop Until B >= 1 Or B <= 40

            If choice = 1 Then
                'Add team
                Call GetTeam_CREATE(Z1$(B), Z2$(B), Z1%(B))
            Else
                'Remove team
                Z1$(B) = "XXX"
            End If 

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveLeague (league$, D1$, D2$, D3$, D4$)

End Sub


'----------------------------------------
'        SaveLeague Subroutine
'----------------------------------------
'...explanation...
Sub SaveLeague (LG$, D1$, D2$, D3$, D4$)

    Shared Z1%()

    Shared Z1$(), Z2$()

    Open diskPaths$(2) + LG$ + ".LIB" For Output As #2

    Print #2, LG$
    Print #2, D1$
    Print #2, D2$
    Print #2, D3
    Print #2, D4$

    For X = 1 To 40
        Print #2, Z1$(X)
        Print #2, Z1%(X)
        Print #2, Z2$(X)
    Next

    Close 2

    For X = 1 To 40

        If Z1$(X) <> "XXX" Then

            For D = 4 To 11
                If Mid$(Z1$(X), D, 1) = Chr$(32) Or Mid$(Z1$(X), D, 1) = "." Then Mid$(Z1$(X), D, 1) = "_"
            Next D

            QQ = 8
            For D = 11 To 4 Step -1
                If Mid$(Z1$(X), D, 1) = "_" Then
                    QQ = D - 4
                Else
                    Exit For
                End If
            Next D

            L = Len(Z1$(X))
            YR$ = Left$(Z1$(X), 2)
            TM$ = Mid$(Z1$(X), 4, QQ)
            Z1$(X) = TM$ + "." + YR$
        Else
            Z1$(X) = Chr$(42)
        End If

    Next X

    Open diskPaths$(2) + LG$ + ".TMS" For Output As #2

    Write #2, LG$
    Write #2, D1$
    Write #2, D2$
    Write #2, D3$
    Write #2, D4$

    For X = 1 To 40
        Write #2, Z1$(X)
    Next

    Close 2
    

End Sub


'----------------------------------------
'       GetTeam_CREATE Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CREATE (teamName$, yearNumber$, teamIdx)

    Shared teamRatings%()

    Do

        Color 15, 0: Cls

        JJ = 7

        Locate 6, 8: Color 15, 4

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" And yearNumber$ <> "" Then
            'NY$ = yearNumber$
            Call SelectTeam_CREATE(yearNumber$, numberTeams, teamIdx)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0
 
            If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
                Cls
                Print "ERROR:: UNABLE TO READ TEAM FILE": Print
                BO% = 1
            Else

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 to fileLength& / 4342

                    Get #1, I1

                    For I2 = 1 To 15
                        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                    Next I2

                    If nullTeam <> 1 Then
                        numberTeams = numberTeams + 1
                        teamNames$(numberTeams) = Q$(0)
                        teamIndex%(numberTeams) = I1
                    End If

                Next I1

                Close 1

                Call SelectTeam_CREATE(yearNumber$, numberTeams, teamIdx)

            End if
            
        End If

        teamName$ = RTrim$(Q$(0))

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color teamRatings%(11), teamRatings%(12): Print teamName$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'      SelectTeam_CREATE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CREATE (yearNumber$, numberTeams, teamIdx)

    Shared teamRatings%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4
    Cls
    Locate , 33: Color 14: Print "TEAM SELECTION"
    Color 15
    Locate 25, 1
    Color 14: Print "ESC";
    Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
    Color 14: Print "ENTER";
    Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIdx = teamIndex%(Selection%)
        teamYear$ = RTrim$(yearNumber$)

        Locate 2, 33
        Color 31: Print "LOADING..."

        Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

        Get #1, teamIdx

        For I1 = 1 To 13: teamRatings%(I1) = CVI(Q$(I1)): Next

        Close 1

    Else

        BO% = 1

    End If

End Sub
