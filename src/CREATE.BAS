'----------------------------------------
'           CREATE Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of leagues
Sub CREATE ()

    _Title "Full Count Baseball - Manage Leagues"

    Dim D1$, D2$, D3$, D4$

    Shared Z1%()

    Shared Z1$(), Z2$()

    Do

        Color 15, 0
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "CREATE / DEFINE NEW LEAGUES     "
        menuChoices$(2) = "ADD / REVIEW LEAGUE MEMBERS     "
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            For X = 1 To 40
                Z1$(X) = "XXX"
                Z2$(X) = "XXX"
                Z1%(X) = -1
            Next

            D1$ = ""
            D2$ = ""
            D3$ = ""
            D4$ = ""

            Select Case Selection%

                Case 1:
                    Call DefineLeague(league$, D1$, D2$, D3$, D4$)
                    Call ReviewEditLeague(league$, D1$, D2$, D3$, D4$)

                Case 2:
                    league$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "LEAGUE", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Call ReviewEditLeague(league$, D1$, D2$, D3$, D4$)
                    End If

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineLeague Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the league's
'general naming and setup
Sub DefineLeague (league$, div1$, div2$, div3$, div4$)

    Do

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 1)

        Do

            Locate 7, 

            Locate , 12: Print "ENTER NAME FOR LEAGUE: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
            Locate , 12: Print "SAMPLE NAMES ARE NL1990 AL56 1956AL NATIONAL"
            Locate 7, 33: Input league$

            If league$ <> "" Then

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
    Locate , 21: QPrint PadRight$(div1$, 50), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2$, 50), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div3$, 50), FG, BG, -1
    Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div4$, 50), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 11, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div4$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
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
'      ReviewEditLeague Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the league definition.
Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)

    Shared Z1%()

    Shared Z1$(), Z2$()

    If _FileExists(diskPaths$(2) + league$ + ".LIB") Then

        Open diskPaths$(2) + league$ + ".LIB" For Input As #2

        Input #2, league$, div1$, div2$, div3$, div4$

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
        Print div1$; Tab(24); div2$; Tab(44); div3$; Tab(64); div4$
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
                Call ChooseTeam(Z2$(B), NTMS, Z1%(B), Z1$(B))
            Else
                'Remove team
                Z1$(B) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveLeague(league$, div1$, div2$, div3$, div4$)

End Sub


'----------------------------------------
'        SaveLeague Subroutine
'----------------------------------------
'This subroutine writes the league team
'memberships and definition to each of
'the corresponding files (.LIB, .TMS)
Sub SaveLeague (league$, div1$, div2$, div3$, div4$)

    Shared Z1%()

    Shared Z1$(), Z2$()

    Open diskPaths$(2) + league$ + ".LIB" For Output As #2

    Print #2, league$
    Print #2, div1$
    Print #2, div2$
    Print #2, div3$
    Print #2, div4$

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

    Open diskPaths$(2) + league$ + ".TMS" For Output As #2

    Write #2, league$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$
    Write #2, div4$

    For X = 1 To 40
        Write #2, Z1$(X)
    Next

    Close 2


End Sub
