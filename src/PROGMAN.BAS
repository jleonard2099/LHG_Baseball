'----------------------------------------
'           PROGMAN Subroutine
'----------------------------------------
'This subroutine serves as a menu for the
'end user options to adjust / create a
'custom manager profile.
Sub PROGMAN ()

    _Title "Full Count Baseball - Programmable Manager"

    Shared parkHR_DRAFT&()
    Shared batterNames$(), PT$(), yesNo$()
    Shared progMG%()


    '-- Why is this here?
    batterNames$(23) = "PITCHER"

    ReDim menuChoices$(1 To 15)

    Cls
    Print Tab(32);
    Color 14, 4: Print "COMPUTER MANAGER"
    Color 15, 0
    Print
    Print
    Print Tab(11); "THIS PROGRAM ALLOWS YOU TO CUSTOMIZE THE COMPUTER MANAGER'S"
    Print
    Print Tab(15); "STRATEGY AND PLAYER USAGE FOR EACH INDIVIDUAL TEAM"
    Print
    Print
    Color 14
    Print Tab(28); "PRESS ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

    If I$ <> Chr$(27) Then

        Call GetManagerProfile(T$, YN$, mgrNbr)

        If mgrNbr > 0 Then

            Do

                Color 15, 1
                Cls

                Print
                Color 14, 4
                Print Tab(23);
                Print "FULL COUNT PROGRAMMABLE MANAGER";
                Print Tab(80); " ";
                Print

                ReDim menuChoices$(1 To 14)
                menuChoices$(1) = "SELECT NEW TEAM TO EDIT                                  "
                menuChoices$(2) = "SELECT / CHANGE LINEUPS                                  "
                menuChoices$(3) = "SELECT / CHANGE ROTATIONS/RELIEVERS                      "
                menuChoices$(4) = "SELECT / CHANGE PINCH HITTERS                            "
                menuChoices$(5) = "SELECT / CHANGE DEFENSIVE REPLACEMENTS (8TH - 9TH INNING)"
                menuChoices$(6) = "SELECT / CHANGE CLOSING STRATEGY                         "
                menuChoices$(7) = "SELECT / CHANGE HITTING STRATEGY                         "
                menuChoices$(8) = "SELECT / CHANGE DEFENSIVE STRATEGY                       "
                menuChoices$(9) = "SELECT / CHANGE PINCH RUNNING STRATEGY                   "
                menuChoices$(10) = "SELECT / CHANGE BASE RUNNING STRATEGY                    "
                menuChoices$(11) = "IMPORT MANAGERIAL PROFILE                                "
                menuChoices$(12) = "EXPORT MANAGERIAL PROFILE                                "
                menuChoices$(13) = "PRINT MANAGERIAL PROFILE                                 "
                menuChoices$(14) = "RETURN TO GAME MENU                                      "
                SELECTION% = 1
                Count% = 14
                Start% = 1
                ExitCode% = 14
                FileFGColor = 15: FileBGColor = 1
                HiLiteFGColor% = 0: HiLiteBGColor% = 7
                Spacing% = 1
                Row% = 5
                menuWidth% = Len(menuChoices$(1))
                Column% = (80 - menuWidth% - 2) / 2

                Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

                _MouseShow "DEFAULT"
                MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
                _MouseHide

                Color , 0

                If ExitCode% <> 27 Then

                    Select Case SELECTION%

                        Case 1
                            For I = 1 To 999:
                                progMG%(I) = -1
                            Next

                            Call GetManagerProfile(T$, YN$, mgrNbr)

                        Case 2
                            Call LineupStrategy(YN$, T$)

                        Case 3
                            Call RotationReliefMenu(YN$, T$, mgrNbr)

                        Case 4
                            Call SelectPinchHitters(YN$, T$, mgrNbr)

                        Case 5
                            Call DefenseLateInning(YN$, T$, mgrNbr)

                        Case 6
                            Call InputPitchStrat(YN$, mgrNbr)

                        Case 7
                            Call HittingStrategy(YN$, mgrNbr)

                        Case 8
                            Call DefensiveStrategy(YN$, mgrNbr)

                        Case 9
                            Call PinchRunStrategy(YN$, T$, mgrNbr)

                        Case 10
                            Call BASERUN(YN$, mgrNbr)

                        Case 11
                            Call ImportManagerProfile(YN$, mgrNbr)

                        Case 12
                            Call ExportManagerProfile(mgrNbr)

                        Case 13
                            userKey$ = GetPrintOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F":
                                        userFile$ = _SAVEFILEDIALOG$("Save Profile Reprort",_CWD$ + "\Manager Profile","*.txt","Text File")
                                        If userFile$ <> "" Then
                                            Call PrintManagerProfile(0, userFile$, T$)
                                        End If

                                    Case "P":
                                        Call PrintManagerProfile(1, "MGR_PROFILE", T$)

                                End Select

                            End If

                        Case 14
                            Cls '

                    End Select

                End If

            Loop Until SELECTION% = 14 Or ExitCode% = 27

        End If

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'    RotationReliefMenu Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'choosing pitching rotations and relievers.
Sub RotationReliefMenu (yearNumber$, targetTeam$, mgrNbr)

    Do

        Cls

        Print Tab(22);
        Color 14, 4
        Print "PITCHING ROTATION/RELIEVER MANAGEMENT"
        Print
        Print

        Color 15, 0
        ReDim menuChoices$(1 To 6)
        menuChoices$(1) = "INPUT A NEW STARTING PITCHING ROTATION     "
        menuChoices$(2) = "EDIT AN EXISTING STARTING PITCHING ROTATION"
        menuChoices$(3) = "SELECT / EDIT MIDDLE RELIEVERS             "
        menuChoices$(4) = "SELECT / EDIT SET-UP MEN                   "
        menuChoices$(5) = "SELECT / EDIT CLOSERS                      "
        menuChoices$(6) = "RETURN TO MAIN MENU                        "
        SELECTION% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 5
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1, 2:
                    Call ManagePitchingRotations(SELECTION%, targetTeam$, mgrNbr)

                Case 3:
                    Call SetupMiddleRelievers(targetTeam$, yearNumber$, mgrNbr)

                Case 4:
                    Call SetupRelievers(targetTeam$, yearNumber$, mgrNbr)

                Case 5:
                    Call SetupClosers(targetTeam$, yearNumber$, mgrNbr)

                Case 6: '

            End Select

        End If

    Loop Until SELECTION% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'       LineupStrategy Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'choosing and analyzing a lineup strategy
Sub LineupStrategy (yearNumber$, targetTeam$)

    Do

        Cls
        Print Tab(26);
        Color 14, 4: Print "LINEUP STRATEGY OPTIONS"
        Print
        Print
        Color 15, 0
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "SELECT / CHANGE / DISPLAY LINEUPS VS. RHP              "
        menuChoices$(2) = "SELECT / CHANGE / DISPLAY LINEUPS VS. LHP              "
        menuChoices$(3) = "ANALYZE PLAYER USAGE ACCORDING TO YOUR LINEUPS SELECTED"
        menuChoices$(4) = "RETURN TO TOP MENU                                     "
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 5
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        handRL% = SELECTION% - 1
        '0 = right hand
        '1 = left hand

        Select Case SELECTION%

            Case 1, 2:
                Call EditLineups(handRL%, targetTeam$, yearNumber$)

            Case 3:
                Call AnalyzePlayerUsage(targetTeam$)

            Case 4:
                Cls '

        End Select

    Loop Until SELECTION% = 4 Or ExitCode% = 27

End Sub

'----------------------------------------
'       AnalyzePlayerUsage Subroutine
'----------------------------------------
'This subroutine evaluates the usage of a
'player's recorded stats against the profile.
Sub AnalyzePlayerUsage (targetTeam$)

    Shared AB%(), teamRatings%(), progMG%(), batterRatings()
    Shared batterNames$(), progB1$()

    Dim FRAC!

    'ANALYZE PLAYER USAGE
    LH% = 0
    RH% = 0

    For I = 0 To 22
        For I1 = 42 To 44
            batterRatings(I, I1) = 0
        Next
    Next

    For I = 10 To 19
        If progMG%(120 + I * 21) = 999 Then
            LH% = LH% + progMG%(100 + I * 21)
        End If
    Next

    For I = 0 To 9
        If progMG%(120 + I * 21) = 999 Then
            RH% = RH% + progMG%(100 + I * 21)
        End If
    Next

    For I = 0 To 9
        If progMG%(120 + I * 21) = 999 Then
            For I1 = 1 To 9
                If progMG%(101 + I1 + I * 21) <> 1 Then
                    FRAC! = progMG%(100 + I * 21) / RH%
                    batterRatings(progMG%(110 + I1 + I * 21), 42) = batterRatings(progMG%(110 + I1 + I * 21), 42) + (AB%(I1) * FRAC!) * .7
                    batterRatings(progMG%(110 + I1 + I * 21), 44) = batterRatings(progMG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC!) * .7
                End If
            Next I1
        End If
    Next I

    For I = 10 To 19
        If progMG%(120 + I * 21) = 999 Then
            For I1 = 1 To 9
                If progMG%(101 + I1 + I * 21) <> 1 Then
                    FRAC! = progMG%(100 + I * 21) / LH%
                    batterRatings(progMG%(110 + I1 + I * 21), 43) = batterRatings(progMG%(110 + I1 + I * 21), 43) + (AB%(I1) * FRAC!) * .3
                    batterRatings(progMG%(110 + I1 + I * 21), 44) = batterRatings(progMG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC!) * .3
                End If
            Next I1
        End If
    Next I

    Cls
    Color 14

    Print "# ";
    Color teamRatings%(11), teamRatings%(12): Print targetTeam$;

    Color 14, 0

    Locate , 18: Print "B  POS  ACTUAL AB  PROJECTED: AB/LHP  AB/RHP  TOT/AB"

    Locate 2
    I1 = 0

    For I = 0 To 22:

        Locate , 1

        If batterNames$(I) = "XXX" Or Left$(batterNames$(I), 1) = " " Or batterNames$(I) = "" Then
            I1 = I1 + 1
        Else
            Color 2, 0
            Locate I + 2 - I1, 1

            If I < 10 Then
                Print Using "#"; I;
                Color 15, 0
                If batterRatings(I, 21) >= 98 Then Color 0, 7
            End If

            If I > 9 Then
                Print alpha$(I - 10);
                Color 15, 0
                If batterRatings(I, 21) >= 98 Then Color 0, 7
            End If

            Locate , 3: Print batterNames$(I);: Color 15, 0: Locate , 18: Print progB1$(batterRatings(I, 0) + 2);
            Locate , 21: If batterRatings(I, 22) <> -1 Then Print Using "#"; batterRatings(I, 22);
            Locate , 22: If batterRatings(I, 23) <> -1 Then Print Using "#"; batterRatings(I, 23);
            Locate , 23: If batterRatings(I, 24) <> -1 Then Print Using "#"; batterRatings(I, 24);
            Locate , 24: If batterRatings(I, 25) <> -1 Then Print Using "#"; batterRatings(I, 25);
            Locate , 27

            Print Using "   ###                ###     ###      ###"; batterRatings(I, 4); batterRatings(I, 43); batterRatings(I, 42); batterRatings(I, 44);

        End If

    Next

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'     SelectPinchHitters Subroutine
'----------------------------------------
'This subroutine gives the user a choice of
'pinch hitters to assign to the select profile.
Sub SelectPinchHitters (yearNumber$, targetTeam$, mgrNbr)

    Shared batterNames$(), progB1$()
    Shared batterRatings(), teamRatings%(), progMG%()

    Do
        'SELECT PINCH HITTERS
        If progMG%(540) <> 999 Then

            progMG%(540) = 999

            For I = 1 To 10
                progMG%(I + 540) = -1: progMG%(I + 550) = 0
            Next

        End If

        Cls

        Locate 1, 1
        Print "# ";: Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "B   AB  HR  RBI   AVG"
        I2 = 0

        For I = 0 To 22

            If batterNames$(I) = "XXX" Or Left$(batterNames$(I), 1) = " " Or batterNames$(I) = "" Then
                I2 = I2 + 1
            Else

                Locate I + 2 - I2, 1
                Color 2, 0

                If I <= 9 Then
                    Print Using "#"; I;
                Else
                    Print alpha$(I - 10);
                End If

                Color 15, 0

                For I1 = 1 To 10
                    If I = progMG%(I1 + 540) Then Color teamRatings%(11), teamRatings%(12)
                Next

                Locate , 3: Print batterNames$(I);

                Color 15, 0
                Locate , 16: Print progB1$(batterRatings(I, 0) + 2);
                Locate , 17: Print Using "  ###  ##  ###"; batterRatings(I, 4); batterRatings(I, 9); batterRatings(I, 10);

                If batterRatings(I, 4) <> 0 Then Print Using "  .###"; batterRatings(I, 6) / batterRatings(I, 4);
                If batterRatings(I, 4) = 0 Then Print "  .---";

            End If

        Next I

        Locate 1, 48: Print "  PINCH HITTERS        "
        Locate , 48: Print "  ---- VS LHP  ----"

        For I = 1 To 5

            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 540) < 0 Then
                Print
            Else
                Print batterNames$(progMG%(I + 540))
            End If

        Next

        Locate , 48: Print "  ---- VS RHP  ----"

        For I = 6 To 10

            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 540) < 0 Then
                Print
            Else
                Print batterNames$(progMG%(I + 540))
            End If

        Next

        Locate 15, 48: Print "(11) ACCEPT CHOICES"
        Locate 16, 48: Print "(12) DELETE A HITTER"

        Do
            Locate 17, 48: Input "ENTER SELECTION (1-12)"; I
        Loop Until I >= 1 And I <= 12

        Select Case I

            Case 1 To 10

                Locate 18, 48: Print Using "SELECT HITTER ##:"; I;
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)
                progMG%(I + 540) = I1

                'Call SelectPinchHitters(yearNumber$, targetTeam$, mgrNbr)

            Case 11:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 12:
                Do
                    Locate 16, 48: Input "SELECT HITTER TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 10

                progMG%(choiceNum + 540) = -1
                progMG%(choiceNum + 550) = 0

        End Select

    Loop Until I = 11

End Sub

'----------------------------------------
'    DefenseLateInning Subroutine
'----------------------------------------
'This subroutine walks the user through a
'defensive strategy for late innings.
Sub DefenseLateInning (yearNumber$, T$, mgrNbr)

    Shared batterNames$()
    Shared teamRatings%(), progMG%(), batterRatings()

    Do
        'DEFENSIVE REPLACEMENTS
        Cls
        Print Tab(26);
        Color 14, 4
        Print "DEFENSIVE REPLACEMENT 8TH AND 9TH INNING STRATEGY OPTIONS"
        Print
        Print

        Color , 0

        If progMG%(560) <> 999 Then

            progMG%(560) = 999

            For I = 1 To 4
                progMG%(I + 560) = -1
                progMG%(I + 564) = 0
                progMG%(I + 568) = 0
                progMG%(I + 572) = 0
                progMG%(I + 576) = 0
            Next

        End If

        Cls
        Locate 1, 1
        Print "#";
        Color teamRatings%(11), teamRatings%(12): Print T$;
        Locate , 17
        Print "1 FAVG 2 FAVG 3 FAVG 4 FAVG"

        I1 = 0

        For I = 0 To 22:

            Locate , 1

            If batterNames$(I) = "XXX" Or Left$(batterNames$(I), 1) = " " Or batterNames$(I) = "" Then

                I1 = I1 + 1

            Else

                Locate I + 2 - I1, 1
                Color 2, 0

                If I < 10 Then Print Using "#"; I;: Color 15, 0: If batterRatings(I, 21) >= 98 Then Color 0, 7
                If I > 9 Then Print alpha$(I - 10);: Color 15, 0: If batterRatings(I, 21) >= 98 Then Color 0, 7

                Locate , 3: Print batterNames$(I);: Color 15, 0
                Locate , 17: If batterRatings(I, 22) <> -1 Then Print Using "# ####"; batterRatings(I, 22); batterRatings(I, 26);
                Locate , 24: If batterRatings(I, 23) <> -1 Then Print Using "# ####"; batterRatings(I, 23); batterRatings(I, 27);
                Locate , 31: If batterRatings(I, 24) <> -1 Then Print Using "# ####"; batterRatings(I, 24); batterRatings(I, 28);
                Locate , 38: If batterRatings(I, 25) <> -1 Then Print Using "# ####"; batterRatings(I, 25); batterRatings(I, 29);

            End If

        Next

        Locate 1, 46
        Print "# PLAYER     P SUBSTITUTE  %  RA"

        For I = 1 To 4

            Locate , 46
            Print Using "# "; I;

            If progMG%(I + 560) < 0 Then
                Print
            Else
                Print Left$(batterNames$(progMG%(I + 560)), 10); Tab(59);
                Print Using "# "; progMG%(I + 576);
                Print Left$(batterNames$(progMG%(I + 564)), 10); Tab(72); Using "### #"; progMG%(I + 568); progMG%(I + 572)
            End If

        Next

        Locate 7, 46: Print "(5) ACCEPT CHOICES"
        Locate 8, 46: Print "(6) DELETE A CHOICE"

        Do
            Locate 9, 46: Input "ENTER SELECTION (1-6)"; I
        Loop Until I >= 1 And I <= 6

        Select Case I

            Case 1 To 4:
                Locate 10, 46: Print Using "SELECT HITTER #:"; I;

                I$ = GetKeyPress$
                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)
                progMG%(I + 560) = I1

                Locate 11, 46: Print "POSITION (2-9) ";

                Do
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 2 And I1 <= 9

                progMG%(I + 576) = I1

                Locate 12, 46: Print "SELECT SUB #:";

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)
                        Case "A": I$ = "10"
                        Case "B": I$ = "11"
                        Case "C": I$ = "12"
                        Case "D": I$ = "13"
                        Case "E": I$ = "14"
                        Case "F": I$ = "15"
                        Case "G": I$ = "16"
                        Case "H": I$ = "17"
                        Case "I": I$ = "18"
                        Case "J": I$ = "19"
                        Case "K": I$ = "20"
                        Case "L": I$ = "21"
                        Case "M": I$ = "22"
                    End Select

                    I1 = Val(I$)
                    progMG%(I + 564) = I1

                Loop Until I1 >= 1 And I1 <= 22

                Do
                    Locate 13, 46: Input "ENTER % OF TIME TO USE "; progMG%(I + 568)
                Loop Until progMG%(I + 568) >= 0 And progMG%(I + 568) <= 100

                Locate 14, 46: Print "NUMBER OF RUNS AHEAD"
                Locate 15, 46: Input "0,1,2 OR MORE "; I1

                If I1 < 0 Then I1 = 0
                If I1 > 2 Then I1 = 2

                progMG%(I + 572) = I1

            Case 5:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 6:
                Do
                    Locate 10, 46: Input "SELECT CHOICE TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 4

                progMG%(choiceNum + 560) = -1
                progMG%(choiceNum + 564) = 0
                progMG%(choiceNum + 568) = 0
                progMG%(choiceNum + 572) = 0
                progMG%(choiceNum + 576) = 0

        End Select

    Loop Until I = 5

End Sub

'----------------------------------------
'       EditLineups Subroutine
'----------------------------------------
'This subroutine serves as a menu for the
'user to choose and review their lineups.
Sub EditLineups (handRL%, targetTeam$, yearNumber$)

    Shared teamRatings%(), progMG%()
    Shared PT$()
    Shared progB3%(), progB7%()

    Dim V1%(1)

    Do

        Cls
        Locate 3, 1
        Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Color 15, 0
        Print " LINEUPS VS "; PT$(handRL%)
        Print
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "INPUT A NEW LINEUP            "
        menuChoices$(2) = "EDIT AN EXISTING LINEUP       "
        menuChoices$(3) = "DELETE AN EXISTING LINEUP     "
        menuChoices$(4) = "RETURN TO LINEUP STRATEGY MENU"
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 5
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 And SELECTION% <> 4 Then

            lineupChoice% = SELECTION%

            Color , 0

            Do

                'Edit / Delete
                If lineupChoice% = 2 Or lineupChoice% = 3 Then

                    'Get a lineup
                    Call GetLineup(handRL%)

                    LF% = 0

                    For I = 0 To 9
                        If progMG%(120 + (I + handRL% * 10) * 21) = 999 Then LF% = LF% + 1
                    Next

                    If LF% = 0 Then
                        Locate 23, 1
                        Color 12: Print "NO LINEUPS FOUND! HIT ANY KEY TO RETURN TO MENU"
                        Color 15
                        I$ = GetKeyPress$
                    End If

                End If

                'Input New
                If lineupChoice% = 1 Then

                    For I = 1 To 9:
                        progB3%(I) = -1
                        progB7%(I) = -1
                    Next

                    D% = 0

                    Do
                        Locate 12, 1
                        Print "IS A DH TO BE USED IN THIS LINEUP? (Y/N)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then D% = 1

                    'We're not "searching" for a lineup but we need to choose one
                    LF% = 1

                End If

                If LF% <> 0 Then

                    Locate 23, 1
                    Print "CHOOSE LINEUP NUMBER (0-9) OR <ESC> TO EXIT ";

                    Do
                        I$ = GetKeyPress$
                        LN% = Val(I$)
                    Loop Until LN% >= 0 And LN% <= 9 Or I$ = Chr$(27)

                End If

            Loop Until lineupChoice% = 2 Or LF% > 0

            '1 = INPUT A NEW LINEUP
            '2 = EDIT AN EXISTING LINEUP
            '3 = DELETE AN EXISTING LINEUP
            If I$ <> Chr$(27) And LF% <> 0 Then

                If handRL% = 1 Then LN% = LN% + 10

                If lineupChoice% = 3 Then
                    progMG%(120 + LN% * 21) = 0
                    progMG%(100 + LN% * 21) = 0
                End If

                If lineupChoice% <> 2 Then

                    Call ViewLineup(targetTeam$)

                    Locate 6, 64: Print "VS " + PT$(handRL%)
                    Locate 7, 61: Print Using "LINEUP ##   POS"; LN% - handRL% * 10

                    For I = 10 To 22:
                        Locate I, 59: Print Space$(21);
                    Next

                    Call InputDeleteLineup(D%, yearNumber$)

                Else

                    'lineupChoice% = 2
                    Call EditOneLineup(targetTeam$, yearNumber$)

                End If

                If RP = 1 Then RP = 0

                W5 = 0
                W6 = 1
                teamIndex%(0) = 1
                V1%(0) = 9
                teamIndex%(1) = 1
                V1%(1) = 9

            End If 'Done checking for ESC

        End If 'Done checking for eSC

    Loop Until SELECTION% = 4 Or ExitCode% = 27

    Color 15, 0

End Sub

'----------------------------------------
'       ViewLineup Subroutine
'----------------------------------------
'This routine loads the lineup from a select
'profile and displays it to the screen
Sub ViewLineup (targetTeam$)

    Shared batterNames$(), progB1$()
    Shared teamRatings%(), batterRatings()

    Cls
    Color 14: Print "#";
    Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
    Color 14, 0
    Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"
    Locate 2

    For I = 0 To 22

        Locate , 1

        If batterNames$(I) = "XXX" Or Left$(batterNames$(I), 1) = " " Or batterNames$(I) = "" Then
            batterRatings(I, 21) = 98
        Else

            Color 2, 0

            If I < 10 Then Print Using "#"; I;: Color 15, 0: If batterRatings(I, 21) >= 98 Then Color 0, 7
            If I > 9 Then Print alpha$(I - 10);: Color 15, 0: If batterRatings(I, 21) >= 98 Then Color 0, 7

            Locate , 3: Print batterNames$(I);
            Color 15, 0
            Locate , 15: Print progB1$(batterRatings(I, 0) + 2);
            Locate , 18: If batterRatings(I, 22) <> -1 Then Print Using "#"; batterRatings(I, 22);
            Locate , 19: If batterRatings(I, 23) <> -1 Then Print Using "#"; batterRatings(I, 23);
            Locate , 20: If batterRatings(I, 24) <> -1 Then Print Using "#"; batterRatings(I, 24);
            Locate , 21: If batterRatings(I, 25) <> -1 Then Print Using "#"; batterRatings(I, 25);
            Locate , 23

            Print Using "### ## ## ## ### ###"; batterRatings(I, 4); batterRatings(I, 7); batterRatings(I, 8); batterRatings(I, 9); batterRatings(I, 11); batterRatings(I, 12);

            Locate , 44
            Print Using "#"; batterRatings(I, 16);
            Print Using "#"; batterRatings(I, 34);
            Print Using "#"; batterRatings(I, 33);
            Locate , 47

            Print Using "### ##"; batterRatings(I, 13); batterRatings(I, 18);

            If batterRatings(I, 6) = batterRatings(I, 4) And batterRatings(I, 6) <> 0 Then
                Print "1.000";
            Else
                If batterRatings(I, 4) = 0 Then
                    Print " .---";
                Else
                    Print Using " .###"; batterRatings(I, 6) / batterRatings(I, 4);
                End If
            End If

            If I <> 22 Then Print

        End If

    Next I

End Sub

'----------------------------------------
'       InputDeleteLineup Subroutine
'----------------------------------------
'This subroutine lets you reivew a line-
'up and make changes.
Sub InputDeleteLineup (D%, yearNumber$)

    Shared batterNames$()

    Shared progB3%(), progB7%(), progB9%()
    Shared teamRatings%(), progM%(), batterRatings()

    Do

        Erase progM%

        Locate 25, 1

        For I1 = 0 To 9

            If D% <> 0 Or I1 <> 0 Then

                If D% <> 1 Or I1 <> 1 Then

                    Locate 24, (I1 * 3) + 1
                    If progM%(I1) = 0 Then Print fieldPos$(I1); " ";
                    If progM%(I1) = 1 Then Print I1; " ";

                    Locate 25, (I1 * 3) + 1
                    If progM%(I1) = 0 Then Print I1; "  ";
                    If progM%(I1) = 1 Then Print "  ";

                End If

            End If

        Next I1

        For I = 1 To 9

            escPressed = 0

            Do

                Call GetLineupPosition(I1, I$)

                If UCase$(I$) <> "X" Then

                    For J = 1 To 9
                        If J <> I And progB3%(J) <> I1 Or progB7%(J) <> 1 Then
                            If progB3%(J) = I1 And UCase$(I$) <> "P" Then Call GetLineupPosition(I1, I$)
                        End If
                    Next J

                    If D% = 1 And UCase$(I$) = "P" Then Call GetLineupPosition(I1, I$)

                    If batterRatings(I1, 21) <> 0 And UCase$(I$) <> "P" Then Call GetLineupPosition(I1, I$)

                    If UCase$(I$) = "P" Then

                        Print Using "# "; I;

                        For J = 1 To 9
                            If J = I Then
                                If progB3%(J) = P1% And progB7%(J) = 1 Then Call GetLineupPosition(I1, I$)
                            End If
                        Next J

                        progB3%(I) = P1%
                        Print batterNames$(23);: progM%(Val(I$)) = 1

                        Locate , 72

                        Print "  p ";
                        progB7%(I) = 1
                        progB9%(1) = P1%
                        progM%(1) = 1

                        Locate 25, 1

                        For I1 = 0 To 9

                            If D% <> 0 Or I1 <> 0 Then

                                If D% <> 1 Or I1 <> 1 Then

                                    Locate 24, (I1 * 3) + 1
                                    If progM%(I1) = 0 Then Print fieldPos$(I1); " ";
                                    If progM%(I1) = 1 Then Print I1; " ";

                                    Locate 25, (I1 * 3) + 1
                                    If progM%(I1) = 0 Then Print I1; "  ";
                                    If progM%(I1) = 1 Then Print "  ";

                                End If

                            End If

                        Next I1


                    End If

                    progB3%(I) = I1
                    Print Using "# "; I;
                    Print batterNames$(progB3%(I));

                    Locate , 74

                    Do
                        I$ = GetKeyPress$
                    Loop Until Val(I$) <> 1

                    For J = 1 To 9:

                        If J <> I Then

                            If Val(I$) = progB7%(J) Then
                                Do
                                    I$ = GetKeyPress$
                                Loop Until Val(I$) <> 1
                            End If

                        End If

                    Next J

                    If D% = 0 And Val(I$) = 0 Or UCase$(I$) = "X" Then Call GetLineupPosition(I1, I$)

                    lu_idx = Val(I$)
                    Print fieldPos$(lu_idx);

                    progB7%(I) = lu_idx
                    progB9%(progB7%(I)) = progB3%(I)
                    progM%(Val(I$)) = 1

                    Locate I1 + 2, 2: Print "*";
                    Color teamRatings%(11), teamRatings%(12): Print batterNames$(I1)
                    Color 15, 0

                    Locate 25, 1

                    For I1 = 0 To 9

                        If D% <> 0 Or I1 <> 0 Then

                            If D% <> 1 Or I1 <> 1 Then

                                Locate 24, (I1 * 3) + 1
                                If progM%(I1) = 0 Then Print fieldPos$(I1); " ";
                                If progM%(I1) = 1 Then Print I1; " ";

                                Locate 25, (I1 * 3) + 1
                                If progM%(I1) = 0 Then Print I1; "  ";
                                If progM%(I1) = 1 Then Print "  ";

                            End If

                        End If

                    Next I1

                    If C1 = 1 Then escPressed = 1

                Else
                    ' "X" was pressed
                    escPressed = 1
                End If

            Loop Until I1 >= 0 And I1 <= 22 Or UCase$(I$) = "X" Or escPressed = 1

            If escPressed = 1 Then Exit For

        Next I

        If UCase$(I$) <> "X" Then
            Call ChangeLineup_PROGMAN(yearNumber$, J$)
        End If

    Loop Until UCase$(I$) = "X" Or J$ = "0"

End Sub

'----------------------------------------
'      GetLineupPosition Subroutine
'----------------------------------------
'This subroutine returns a numerical value
'associated with the alphabetical characters
'in a lineup.
Sub GetLineupPosition (I1, I$)

    Locate I + 7, 59
    Print Space$(18);
    Locate I + 7, 59

    Do
        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "A": I$ = "10"
            Case "B": I$ = "11"
            Case "C": I$ = "12"
            Case "D": I$ = "13"
            Case "E": I$ = "14"
            Case "F": I$ = "15"
            Case "G": I$ = "16"
            Case "H": I$ = "17"
            Case "I": I$ = "18"
            Case "J": I$ = "19"
            Case "K": I$ = "20"
            Case "L": I$ = "21"
            Case "M": I$ = "22"
        End Select

        I1 = Val(I$)

    Loop Until I1 >= 0 And I1 <= 22 Or UCase$(I$) = "X"

End Sub


'----------------------------------------
'    ChangeLineup_PROGMAN Subroutine
'----------------------------------------
'This routine lets the user select a line-
'up and determine how often it should be
'used.
Sub ChangeLineup_PROGMAN (yearNumber$, I$)

    Shared batterNames$()
    Shared progB3%(), progB7%(), progB9%()
    Shared progM%(), progMG%(), batterRatings()

    Do
        C1 = 1
        Color 14
        Locate 25, 1
        Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
        Color 15, 0
        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "R":

                Erase progM%

                For J = 1 To 9:
                    progB7%(J) = -1
                    progB3%(J) = -1
                Next

                C1 = 0

                For J = 0 To 22:
                    batterRatings(J, 21) = 0
                Next

                RP = 1

                For J = 0 To 22
                    If batterRatings(J, 31) <> 0 Then batterRatings(J, 21) = 99
                Next

            Case "L":

                Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

                Do
                    I$ = GetKeyPress$
                    K1 = Val(I$)
                Loop Until K1 >= 1 And K1 <= 9

                Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

                Do
                    I$ = GetKeyPress$
                    K2 = Val(I$)
                Loop Until K2 >= 1 And K2 <= 9

                Swap progB7%(K2), progB7%(K1)
                Swap progB3%(K1), progB3%(K2)

                progB9%(progB7%(K1)) = progB3%(K1)
                progB9%(progB7%(K2)) = progB3%(K2)

                For I = 1 To 9

                    Locate I + 7, 59
                    Print Using "# "; I;

                    If progB7%(I) = 1 And I5 <> 66 Then Print batterNames$(23);
                    If progB7%(I) <> 1 Then Print batterNames$(progB3%(I));

                    Print Tab(74);
                    Print fieldPos$(progB7%(I));

                Next I

                I5 = 0

                Locate 18, 59: Print Space$(17)
                Locate 19, 59: Print Space$(17)

            Case "P":

                Do
                    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

                    Do
                        I$ = GetKeyPress$
                        K1 = Val(I$)
                    Loop Until K1 >= 1 And K1 <= 9

                Loop Until progB7%(K1) <> 1

                Do
                    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

                    Do
                        I$ = GetKeyPress$
                        K2 = Val(I$)
                    Loop Until K2 >= 1 And K2 <= 9

                Loop Until progB7%(K2) <> 1

                Swap progB7%(K2), progB7%(K1)
                progB9%(progB7%(K1)) = progB3%(K1)
                progB9%(progB7%(K2)) = progB3%(K2)

                For I = 1 To 9

                    Locate I + 7, 59

                    Print Using "# "; I;

                    If progB7%(I) = 1 And I5 <> 66 Then Print batterNames$(23);
                    If progB7%(I) <> 1 Then Print batterNames$(progB3%(I));

                    Print Tab(74);
                    Print fieldPos$(progB7%(I));

                Next I

                I5 = 0
                Locate 18, 59: Print Space$(17)
                Locate 19, 59: Print Space$(17)

        End Select

        I = Val(I$)

        Select Case I

            Case 0, Is > 9

                Print
                Input "WHAT PERCENT OF TIME IS THIS LINEUP TO BE USED (1-100) "; LU%

                If LU% < 1 Then LU% = 1
                If LU% > 100 Then LU% = 100
                progMG%(100 + LN% * 21) = LU%
                progMG%(101 + LN% * 21) = D%

                For I = 1 To 9
                    progMG%(101 + I + LN% * 21) = progB7%(I)
                    progMG%(110 + I + LN% * 21) = progB3%(I)
                Next

                progMG%(120 + LN% * 21) = 999
                progMG%(0) = 999

                Call UpdateManagerFile(yearNumber$, mgrNbr)

                For I = 1 To 9
                    If progB7%(I) = 1 Then
                        progM%(I) = 0
                    End If
                Next I

                progM%(0) = 0

                C1 = 0

                For I = 0 To 9
                    progB3%(I) = -1
                    progB7%(I) = -1
                Next

            Case 1 To 9
                '-- What is B4%?
                '-- No sign of it anywhere so it's 0
                Locate progB3%(I) + 2 - B4%, 2: Print " "; batterNames$(progB3%(I))

        End Select

    Loop Until I >= 0 'And I <= 9

End Sub

'----------------------------------------
'        EditOneLineup Subroutine
'----------------------------------------
'This Subroutine lets the user view one
'pre-configured lineup and work through
'making changes.
Sub EditOneLineup (targetTeam$, yearNumber$)

    Shared batterNames$(), fieldPos$()
    Shared progB3%(), progB7%(), progB9%()
    Shared progM%(), batterRatings()

    Call ViewLineup(targetTeam$)

    For I = 10 To 22:
        Locate I, 59: Print Space$(21);
    Next

    LU% = progMG%(100 + LN% * 21)
    D% = progMG%(101 + LN% * 21)

    For I = 1 To 9
        progB3%(I) = progMG%(110 + I + LN% * 21)
        progB7%(I) = progMG%(101 + I + LN% * 21)
    Next

    For I = 1 To 9:

        progB9%(progB7%(I)) = progB3%(I)

        If progB7%(I) = 1 Then
            progB3%(I) = P1%: progB9%(1) = P1%
        Else
            batterRatings(progB3%(I), 21) = 0
        End If

    Next I

    For I = 10 To 22:
        Locate I, 59: Print Space$(21);
    Next

    For I = 1 To 9

        Locate I + 7, 59

        Print Using "# "; I;

        If progB7%(I) = 1 And I5 <> 66 Then Print batterNames$(23);
        If progB7%(I) <> 1 Then Print batterNames$(progB3%(I));

        Print Tab(74);
        Print fieldPos$(progB7%(I));

    Next I

    I5 = 0

    For I2 = 0 To 22
        Locate I2 + 2, 2
        For I3 = 1 To 9
            If progB3%(I3) = I2 And progB7%(I3) <> 1 Then
                Print "*";
                Color teamRatings%(11), teamRatings%(12): Print batterNames$(I2)
                Color 15, 0
            End If
        Next I3
    Next I2

    For I3 = 0 To 9:
        progM%(I3) = 1
    Next I3

    Call ChangeLineup_PROGMAN(yearNumber$, J$)

End Sub

'----------------------------------------
'         GetLineup Subroutine
'----------------------------------------
'This subroutine prints a summary of the
'lineups found during the search.
Sub GetLineup (handRL%)

    Shared batterNames$(), fieldPos$(), PT$()
    Shared progMG%()

    Cls
    Color 12: Print Tab(30); "LINEUPS FOUND VS. "; PT$(handRL%)
    Color 14
    Print " #0    "; progMG%(100 + (0 + handRL% * 10) * 21); "%";
    Locate , 17: Print " #1    "; progMG%(100 + (1 + handRL% * 10) * 21); "%";
    Locate , 33: Print " #2    "; progMG%(100 + (2 + handRL% * 10) * 21); "%";
    Locate , 49: Print " #3    "; progMG%(100 + (3 + handRL% * 10) * 21); "%";
    Locate , 65: Print " #4    "; progMG%(100 + (4 + handRL% * 10) * 21); "%"
    Color 15

    For I = 0 To 4

        For I1 = 1 To 9

            If progMG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 3, 16 * I + 1
                Color 2: Print "NONE FOUND"
                Color 15

            Else

                If progMG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 2 + I1, 16 * I + 1: Print "         "
                Else

                    Locate 2 + I1, 16 * I + 1

                    If progMG%(101 + I1 + (I + handRL% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batterNames$(progMG%(110 + I1 + (I + handRL% * 10) * 21)), 10);
                    End If

                    Locate 2 + I1, 16 * I + 12
                    Print fieldPos$(progMG%(101 + I1 + (I + handRL% * 10) * 21))

                End If

            End If 'Done checking for 999 value

        Next

    Next

    Print
    Color 14: Print " #5    "; progMG%(100 + (5 + handRL% * 10) * 21); "%";
    Locate , 17: Print " #6    "; progMG%(100 + (6 + handRL% * 10) * 21); "%";
    Locate , 33: Print " #7    "; progMG%(100 + (7 + handRL% * 10) * 21); "%";
    Locate , 49: Print " #8    "; progMG%(100 + (8 + handRL% * 10) * 21); "%";
    Locate , 65: Print " #9    "; progMG%(100 + (9 + handRL% * 10) * 21); "%"
    Color 15

    For I = 5 To 9

        For I1 = 1 To 9

            If progMG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 14, 16 * (I - 5) + 1
                Color 2: Print "NONE FOUND"
                Color 15
            Else
                If progMG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 13 + I1, 16 * (I - 5) + 1
                    Print "         "
                Else
                    Locate 13 + I1, 16 * (I - 5) + 1

                    If progMG%(101 + I1 + (I + handRL% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batterNames$(progMG%(110 + I1 + (I + handRL% * 10) * 21)), 10);
                    End If

                    Locate 13 + I1, 16 * (I - 5) + 12
                    Print fieldPos$(progMG%(101 + I1 + (I + handRL% * 10) * 21))

                End If

            End If

        Next I1

    Next I

End Sub

'----------------------------------------
'     PrintManagerProfile Subroutine
'----------------------------------------
'This subroutine reviews all existing items
'in the manager profile and prints a summary
'of the strategy to the default printer.
Sub PrintManagerProfile (printDest, destFile$, targetTeam$)

    Shared batterNames$(), pitcherNames$(), yesNo$()
    Shared progMG%()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1,  "MANAGERIAL PROFILE FOR "; targetTeam$
    Print #1, 

    '0 = right hand
    '1 = left hand
    handRL% = 0
    LF% = 0

    For I = 0 To 9
        If progMG%(120 + (I + handRL% * 10) * 21) = 999 Then 
            LF% = LF% + 1
        End If
    Next

    If LF% = 0 Then
        Print #1,  "NO LINEUPS VS RHP "
    Else
        Call GetLineup(handRL%)
        _PrintImage 0
    End If

    handRL% = 1
    LF% = 0

    For I = 0 To 9
        If progMG%(120 + (I + handRL% * 10) * 21) = 999 Then LF% = LF% + 1
    Next

    If LF% = 0 Then
        Print #1,  "NO LINEUPS VS LHP "
    Else
        Call GetLineup(handRL%)
        _PrintImage 0
    End If

    Print #1, 
    If progMG%(99) <> 999 Then 
        Print #1,  "NO STARTING PITCHING ROTATION DEFINED"
    End If

    Print #1,  "SIZE OF ROTATION IS "; progMG%(1); " MAN"
    Print #1,  "    STARTING ROTATION     % STARTS"

    For I = 1 To progMG%(1)
        Print #1,  Using "(#) "; I;
        If progMG%(I + 2) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I + 2)); Tab(30); progMG%(I + 12)
        End If
    Next

    Print #1, 
    Print #1,  "     SPOT STARTERS         % USED"

    For I = 21 To 36
        Print #1,  Using "(##) "; I - 20;
        If progMG%(I) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I)); Tab(30); progMG%(I + 20)
        End If
    Next

    Print #1, 
    Print #1,  "  MIDDLE RELIEVERS      % APPEAR"

    For I = 1 To 10

        Print #1,  Using "(##) "; I;

        If progMG%(I + 60) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I + 60)); Tab(35); progMG%(I + 70)
        End If

    Next

    Print #1, 
    Print #1,  "  SET-UP RELIEVERS      % APPEAR"
    Print #1,  "   -----  VS LEFT  -------      "

    For I = 1 To 3
        Print #1,  Using "(##) "; I;

        If progMG%(I + 80) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I + 80)); Tab(35); progMG%(I + 90)
        End If

    Next

    Print #1,  "   -----  VS RIGHT  -------      "

    For I = 4 To 6

        Print #1,  Using "(##) "; I;

        If progMG%(I + 80) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I + 80)); Tab(35); progMG%(I + 90)
        End If

    Next

    Print #1, 
    Print #1,  "          CLOSERS       % APPEAR"
    Print #1,  "   -----  VS LEFT  -------      "

    For I = 1 To 3

        Print #1,  Using "(##) "; I;

        If progMG%(I + 520) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I + 520)); Tab(35); progMG%(I + 530)
        End If

    Next

    Print #1,  "   -----  VS RIGHT  -------      "

    For I = 4 To 6

        Print #1,  Using "(##) "; I;

        If progMG%(I + 520) < 0 Then
            Print #1, 
        Else
            Print #1,  pitcherNames$(progMG%(I + 520)); Tab(35); progMG%(I + 530)
        End If

    Next

    Print #1, 
    Print #1,  "  PINCH HITTERS        "
    Print #1,  "  ---- VS LHP  ----"

    For I = 1 To 5

        Print #1,  Using "(##) "; I;

        If progMG%(I + 540) < 0 Then
            Print #1, 
        Else
            Print #1,  batterNames$(progMG%(I + 540))
        End If

    Next

    Print #1,  "  ---- VS RHP  ----"

    For I = 6 To 10

        Print #1,  Using "(##) "; I;

        If progMG%(I + 540) < 0 Then
            Print #1, 
        Else
            Print #1,  batterNames$(progMG%(I + 540))
        End If

    Next

    Print #1, 
    Print #1,  "DEFENSIVE REPLACEMENTS"
    Print #1,  "# PLAYER     P SUBSTITUTE  %  RA"

    For I = 1 To 4

        Print #1,  Using "# "; I;

        If progMG%(I + 560) < 0 Then
            Print #1, 
        Else
            Print #1,  Left$(batterNames$(progMG%(I + 560)), 10); Tab(19);
            Print #1,  Using "# "; progMG%(I + 576);
            Print #1,  Left$(batterNames$(progMG%(I + 564)), 10); Tab(32); Using "### #"; progMG%(I + 568); progMG%(I + 572)
        End If

    Next

    Print #1, 

    If progMG%(581) <> 999 Then
        Print #1,  "NO CLOSING STRATEGY HAS BEEN DEFINED": Print #1, :
    Else
        Print #1,  Tab(31); "CLOSING STRATEGY"
        Print #1, 
        Print #1,  "STRATEGY                                        % OF TIME"
        Print #1,  "(1) INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; progMG%(588)
        Print #1,  "(2) USE BOTH SETUP RELIEVER AND CLOSER "; yesNo$(progMG%(589)): Print #1, 
        Print #1,  "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
        Print #1,  "(3) ";: Print #1,  Using "START INNING IF SAVE OPPORTUNITY               ###"; progMG%(582)
        Print #1,  "(4) ";: Print #1,  Using "START INNING IF AHEAD                          ###"; progMG%(583)
        Print #1,  "(5) ";: Print #1,  Using "START INNING IF TIED OR AHEAD                  ###"; progMG%(584)
        Print #1,  "(6) ";: Print #1,  Using "IF CURRENT PITCHER TIRED AND AHEAD             ###"; progMG%(585)
        Print #1,  "(7) ";: Print #1,  Using "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; progMG%(586)
        Print #1,  "(8) ";: Print #1,  Using "WHEN SAVE OPPORTUNITY OCCURS                   ###"; progMG%(587)
    End If

    Print #1, 

    If progMG%(590) <> 999 Then
        Print #1,  "NO PINCH HITTING FOR THE PITCHER STRATEGY DEFINED"
    Else
        Print #1,  Tab(26); "PINCH HITTING FOR THE PITCHER"
        Print #1, 
        Print #1,  "PINCH HIT FOR THE PITCHER FROM 'A' INNING ON"
        Print #1,  "IF DOWN BY 'B' OR MORE RUNS AND"
        Print #1,  "'C' OR MORE RUNNERS ON BASE AND"
        Print #1,  "'D' OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN 'E'"
        Print #1,  "'F' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO        A      B      C      D     E     F"

        For I = 0 To 2
            Print #1,  Using "   #           ##     ##      #      #    ###   ###"; I + 1; progMG%(I * 6 + 591); progMG%(I * 6 + 592); progMG%(I * 6 + 593); progMG%(I * 6 + 594); progMG%(I * 6 + 595); progMG%(I * 6 + 596)
        Next

    End If

    Print #1, 

    If progMG%(609) <> 999 Then
        Print #1,  "NO BUNTING STRATEGY DEFINED FOR THE PITCHER"
    Else
        Print #1,  "SACRIFICE BUNTING FOR THE PITCHER": Print #1, 
        Print #1,  "'A' OUTS OR LESS"
        Print #1,  "IF 'B' OR LESS RUNS AHEAD AND"
        Print #1,  "'C' OR LESS RUNS BEHIND"
        Print #1,  "'D' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                       A      B     C      D"
        Print #1,  "1 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; progMG%(610); progMG%(611); progMG%(612); progMG%(613)
        Print #1,  "    RUNNER ON 1ST           "
        Print #1,  "2 - 8TH INNING ON              "; Using "#     ##    ##     ###"; progMG%(614); progMG%(615); progMG%(616); progMG%(617)
        Print #1,  "    RUNNER ON 1ST           "
        Print #1,  "3 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; progMG%(618); progMG%(619); progMG%(620); progMG%(621)
        Print #1,  "    RUNNER ON 2ND           "
        Print #1,  "4 - 8TH INNING ON              "; Using "#     ##    ##     ###"; progMG%(622); progMG%(623); progMG%(624); progMG%(625)
        Print #1,  "    RUNNER ON 2ND           "
        Print #1,  "5 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; progMG%(626); progMG%(627); progMG%(628); progMG%(629)
        Print #1,  "    RUNNERS ON 1ST AND 2ND  "
        Print #1,  "6 - 8TH INNING ON              "; Using "#     ##    ##     ###"; progMG%(630); progMG%(631); progMG%(632); progMG%(633)
        Print #1,  "    RUNNERS ON 1ST AND 2ND  "
    End If

    Print #1, 

    If progMG%(634) <> 999 Then
        Print #1,  "NO BUNTING STRATEGY DEFINED FOR POSITION PLAYERS"
    Else
        Print #1,  "SACRIFICE BUNTING FOR THE POSITION PLAYERS": Print #1, 
        Print #1,  "IF 'A' OR LESS RUNS AHEAD AND"
        Print #1,  "'B' OR LESS RUNS BEHIND"
        Print #1,  "BATTING AVERAGE IS LESS THAN 'C' AND SLUGGING PCT IS LESS THAN 'D'"
        Print #1,  "PITCHERS ERA LESS THAN 'E' , 'F' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                        A     B      C      D      E     F"
        Print #1,  "1 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; progMG%(635); progMG%(636); progMG%(637); progMG%(638); progMG%(639); progMG%(640)
        Print #1,  "    0 OUTS, RUNNER ON 1ST"
        Print #1,  "2 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; progMG%(641); progMG%(642); progMG%(643); progMG%(644); progMG%(645); progMG%(646)
        Print #1,  "    0 OUTS, RUNNER ON 2ND"
        Print #1,  "3 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; progMG%(647); progMG%(648); progMG%(649); progMG%(650); progMG%(651); progMG%(652)
        Print #1,  "    0 OUTS, RUNNERS 1ST AND 2ND"
        Print #1,  "4 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; progMG%(653); progMG%(654); progMG%(655); progMG%(656); progMG%(657); progMG%(658)
        Print #1,  "    0 OUTS, RUNNER ON 1ST"
        Print #1,  "5 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; progMG%(659); progMG%(660); progMG%(661); progMG%(662); progMG%(663); progMG%(664)
        Print #1,  "    0 OUTS, RUNNER ON 2ND"
        Print #1,  "6 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; progMG%(665); progMG%(666); progMG%(667); progMG%(668); progMG%(669); progMG%(670)
        Print #1,  "    0 OUTS, RUNNERS ON 1ST AND 2ND"
    End If

    Print #1, 

    If progMG%(671) <> 999 Then
        Print #1,  "NO HIT AND RUN STRATEGY DEFINED"
    Else
        Print #1,  "HIT AND RUN STRATEGY": Print #1, 
        Print #1,  "IF 'A' OR LESS RUNS AHEAD AND"
        Print #1,  "'B' OR LESS RUNS BEHIND"
        Print #1,  "'C' % OF THE TIME"
        Print #1,  "'D' AB PER STRIKEOUT"
        Print #1,  " SLUGGING PCT LESS THAN OR EQUAL TO 'E'"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "    SCENARIO                          A     B      C   D    E"
        Print #1,  "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##    ###  ##  ###"; progMG%(672); progMG%(673); progMG%(674); progMG%(866); progMG%(872)
        Print #1,  "2 - 1 OUT, RUNNER ON 1ST             "; Using "##    ##    ###  ##  ###"; progMG%(675); progMG%(676); progMG%(677); progMG%(867); progMG%(873)
        Print #1,  "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; Using "##    ##    ###  ##  ###"; progMG%(678); progMG%(679); progMG%(680); progMG%(868); progMG%(874)
        Print #1,  "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; Using "##    ##    ###  ##  ###"; progMG%(681); progMG%(682); progMG%(683); progMG%(869); progMG%(875)
        Print #1,  "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; Using "##    ##    ###  ##  ###"; progMG%(684); progMG%(685); progMG%(686); progMG%(870); progMG%(876)
        Print #1,  "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; Using "##    ##    ###  ##  ###"; progMG%(687); progMG%(688); progMG%(689); progMG%(871); progMG%(877)
    End If

    Print #1, 

    If progMG%(847) <> 999 Then
        Print #1,  "NO CRITERIA DEFINED FOR PINCH HITTING FOR THE BATTER"
    Else
        Print #1,  "PINCH HITTING FOR THE BATTER"
        Print #1, 
        Print #1,  "PINCH HIT FOR THE BATTER FROM 'A' INNING ON"
        Print #1,  "IF DOWN BY 'B' OR MORE RUNS AND"
        Print #1,  "'C' RUNNERS ON BASE AND"
        Print #1,  "SLUGGING PCT IS LESS THAN 'D'"
        Print #1,  "AND BATTING AVERAGE IS LESS THAN 'E'"
        Print #1,  "'F' % OF THE TIME"
        Print #1, 
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO        A      B      C      D       E     F"

        For I = 0 To 2
            Print #1,  Using "   #           ##     ##      #     ###    ###   ###"; I + 1; progMG%(I * 6 + 848); progMG%(I * 6 + 849); progMG%(I * 6 + 850); progMG%(I * 6 + 851); progMG%(I * 6 + 852); progMG%(I * 6 + 853)
        Next

    End If

    Print #1, 

    If progMG%(690) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR INFIELD IN WITH 3RD BASE OCCUPIED"
    Else
        Print #1,  "INFIELD IN WITH 3RD BASE OCCUPIED": Print #1, 
        Print #1,  "IF DOWN BY 'A' RUNS"
        Print #1,  "AND NOT MORE THAN 'B' RUNS"
        Print #1,  "FROM 'C' INNING ON"
        Print #1,  "'D' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                              A     B   C     D"
        Print #1,  "1 - 0 OUTS                           "; Using "##    ##   #   ###"; progMG%(691); progMG%(692); progMG%(693); progMG%(694)
        Print #1,  "2 - 0 OUTS                           "; Using "##    ##   #   ###"; progMG%(695); progMG%(696); progMG%(697); progMG%(698)
        Print #1,  "3 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; progMG%(699); progMG%(700); progMG%(701); progMG%(702)
        Print #1,  "4 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; progMG%(703); progMG%(704); progMG%(705); progMG%(706)
    End If

    Print #1, 

    Cls

    If progMG%(707) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT"
    Else
        Print #1,  "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": Print #1, 
        Print #1,  "IF BUNT RATING IS >= 'A'"
        Print #1,  "'B' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "      A      B"
        Print #1,  Using "(1)  ##    ###"; progMG%(708); progMG%(709)
        Print #1,  Using "(2)  ##    ###"; progMG%(710); progMG%(711)
        Print #1,  Using "(3)  ##    ###"; progMG%(712); progMG%(713)
    End If

    Print #1, 

    If progMG%(714) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SAC HIT SITUATION"
    Else
        Print #1,  "CORNERS IN VS HITTER IN SAC HIT SITUATION": Print #1, 
        Print #1,  "IF AHEAD BY  'A' RUNS OR LESS"
        Print #1,  "OR BEHIND BY NOT MORE THAN 'B' RUNS"
        Print #1,  "BUNT RATING >= 'C'"
        Print #1,  "'D' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                              A     B   C     D"
        Print #1,  "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; progMG%(715); progMG%(716); progMG%(717); progMG%(718)
        Print #1,  "2 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; progMG%(719); progMG%(720); progMG%(721); progMG%(722)
        Print #1,  "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; progMG%(723); progMG%(724); progMG%(725); progMG%(726)
        Print #1,  "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; progMG%(727); progMG%(728); progMG%(729); progMG%(730)
    End If

    Print #1, 

    If progMG%(731) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR CORNERS IN VS PITCHER IN SAC HIT SITUATION"
    Else
        Print #1,  "CORNERS IN VS PITCHER IN SAC HIT SITUATION": Print #1, 
        Print #1,  "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO 'A'"
        Print #1,  "'B' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                                  A      B"
        Print #1,  "1 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; progMG%(732); progMG%(733)
        Print #1,  "2 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; progMG%(734); progMG%(735)
        Print #1,  "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; progMG%(736); progMG%(737)
        Print #1,  "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; progMG%(738); progMG%(739)
    End If

    Print #1, 

    If progMG%(740) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SQUEEZE SITUATION"
    Else
        Print #1,  "CORNERS IN VS HITTER IN SQUEEZE SITUATION": Print #1, 
        Print #1,  "IF AHEAD BY  'A' RUNS OR LESS"
        Print #1,  "OR BEHIND BY NOT MORE THAN 'B' RUNS"
        Print #1,  "BUNT RATING >= 'C'"
        Print #1,  "'D' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                              A     B   C     D"
        Print #1,  "1 - 0 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; progMG%(741); progMG%(742); progMG%(743); progMG%(744)
        Print #1,  "2 - 1 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; progMG%(745); progMG%(746); progMG%(747); progMG%(748)
    End If

    Print #1, 

    If progMG%(749) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR GUARDING THE LINES"
    Else
        Print #1,  "GUARD LINES STRATEGY": Print #1, 
        Print #1,  "IF 'A' RUNS AHEAD"
        Print #1,  "'B' INNING ON"
        Print #1,  "'C' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "     A     B      C"
        Print #1,  "1 - "; Using "##    ##    ###"; progMG%(750); progMG%(751); progMG%(752)
        Print #1,  "2 - "; Using "##    ##    ###"; progMG%(753); progMG%(754); progMG%(755)
    End If

    Print #1, 

    If progMG%(756) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR PITCHING AROUND HITTERS"
    Else
        Print #1,  "PITCH AROUND STRATEGY": Print #1, 
        Print #1,  "SLUGGING PCT >= 'A'"
        Print #1,  "'B' INNING ON"
        Print #1,  "'C' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                                               A     B      C"
        Print #1,  "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; progMG%(757); progMG%(758); progMG%(759)
        Print #1,  "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; progMG%(760); progMG%(761); progMG%(762)
        Print #1,  "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; progMG%(763); progMG%(764); progMG%(765)
    End If

    Print #1, 

    If progMG%(766) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED FOR THROWING TO THE PLATE"
    Else
        Print #1,  "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": Print #1, 
        Print #1,  "FROM 'A' INNING ON"
        Print #1,  "'B' OR MORE RUNS AHEAD"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "      A      B"
        Print #1,  Using "(1)  ##    ###"; progMG%(767); progMG%(768)
        Print #1,  Using "(2)  ##    ###"; progMG%(769); progMG%(770)
    End If

    Print #1, 
    Print #1,  "# PINCH RUNNER"

    For I = 1 To 6
        Print #1,  Using "# "; I;
        If progMG%(I + 771) < 0 Then
            Print #1, 
        Else
            Print #1,  batterNames$(progMG%(I + 771))
        End If
    Next

    Print #1, 
    Print #1,  "DEFENSIVE SUBS FOR PINCH RUNNERS"
    Print #1,  "SCORE TIED OR DOWN BY ONE RUN"

    If progMG%(778) <> 999 Then
        Print #1,  "NO SUBS DEFINED"
    Else
        Print #1,  "# PLAYER     I  SUBSTITUTE  %"
        For I = 0 To 5
            Print #1,  Using "# "; I + 1;
            If progMG%(I * 4 + 779) < 0 Then
                Print #1, 
            Else
                Print #1,  Left$(batterNames$(progMG%(I * 4 + 779)), 10); Tab(19);
                Print #1,  Using "## "; progMG%(I * 4 + 780);
                Print #1,  Left$(batterNames$(progMG%(I * 4 + 781)), 10); Tab(33); Using "###"; progMG%(I * 4 + 782)
            End If
        Next
    End If

    Print #1, 

    If progMG%(803) <> 999 Then
        Print #1,  "NO STRATEGY DEFINED TO PINCH RUN FOR PITCHER"
    Else
        Print #1,  "PINCH RUN FOR PITCHER": Print #1, 
        Print #1,  "RUN RATING <= 'A'"
        Print #1,  "'B' INNING ON"
        Print #1,  "'C' % OF THE TIME"
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                A     B      C"
        Print #1,  "1 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; progMG%(804); progMG%(805); progMG%(806)
        Print #1,  "2 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; progMG%(807); progMG%(808); progMG%(809)
    End If

    Print #1, 

    If progMG%(810) <> 999 Then
        Print #1,  "NO BASE RUNNING STRATEGY DEFINED"
    Else
        Print #1,  "BASE RUNNING STRATEGY": Print #1, 
        Print #1,  Tab(32); "TABLE OF VALUES"
        Print #1,  "SCENARIO                            %       %          %          %"
        Print #1,  "                                  SAFE    NORMAL   AGGRESSIVE   TOTALS"
        Print #1,  " 1 - INNING 1-3, WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(811); progMG%(812); progMG%(813); progMG%(811) + progMG%(812) + progMG%(813)
        Print #1,  " 2 - INNING 1-3, WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(814); progMG%(815); progMG%(816); progMG%(816) + progMG%(815) + progMG%(814)
        Print #1,  " 3 - INNING 1-3, WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(817); progMG%(818); progMG%(819); progMG%(819) + progMG%(818) + progMG%(817)
        Print #1,  " 4 - INNING 4-6, WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(820); progMG%(821); progMG%(822); progMG%(820) + progMG%(821) + progMG%(822)
        Print #1,  " 5 - INNING 4-6, WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(823); progMG%(824); progMG%(825); progMG%(823) + progMG%(824) + progMG%(825)
        Print #1,  " 6 - INNING 4-6, WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(826); progMG%(827); progMG%(828); progMG%(826) + progMG%(827) + progMG%(828)
        Print #1,  " 7 - INNING 7-9, WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(829); progMG%(830); progMG%(831); progMG%(829) + progMG%(830) + progMG%(831)
        Print #1,  " 8 - INNING 7-9, WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(832); progMG%(833); progMG%(834); progMG%(832) + progMG%(833) + progMG%(834)
        Print #1,  " 9 - INNING 7-9, WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(835); progMG%(836); progMG%(837); progMG%(835) + progMG%(836) + progMG%(837)
        Print #1,  "10 - INNING 9- , WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(838); progMG%(839); progMG%(840); progMG%(838) + progMG%(839) + progMG%(840)
        Print #1,  "11 - INNING 9- , WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(841); progMG%(842); progMG%(843); progMG%(841) + progMG%(842) + progMG%(843)
        Print #1,  "12 - INNING 9- , WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(844); progMG%(845); progMG%(846); progMG%(844) + progMG%(845) + progMG%(846)
    End If

    Close #1
    
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
'       ShowPitcherInfo Subroutine
'----------------------------------------
'This subroutine displays the information
'for the team's pitchers.
Sub ShowPitcherInfo (targetTeam$)

    Shared teamRatings%(), progMG%(), pitcherRatings()
    Shared progB1$(), pitcherNames$()

    Cls

    Locate 1, 1
    Print "#";
    Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
    Color 14, 0
    Locate , 16: Print "T   G GS   W- L   ERA"

    For I = 0 To 21

        If pitcherNames$(I) = "XXX" Or Left$(pitcherNames$(I), 1) = " " Or pitcherNames$(I) = "" Then pitcherRatings(I, 15) = 98

        If pitcherRatings(I, 15) <> 1 And pitcherNames$(I) <> "XXX" Then

            Color 2, 0

            If I <= 9 Then
                Print Using "#"; I;
            Else
                Print alpha$(I - 10);
            End If

            Color 15, 0

            For I1 = 1 To progMG%(1)
                If I = progMG%(I1 + 2) Then Color 0, 7
            Next

            Locate , 3: Print pitcherNames$(I);

            Color 15, 0
            Locate , 16: Print progB1$(pitcherRatings(I, 0) + 2);
            Locate , 17: Print Using " ### ##"; pitcherRatings(I, 4); pitcherRatings(I, 5);
            Locate , 24: Print Using "  ##"; pitcherRatings(I, 1);
            Print "-";
            Print Using "## ##.##"; pitcherRatings(I, 2); pitcherRatings(I, 10) / 100

        End If

    Next I

End Sub


'----------------------------------------
'       SpotStarters Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'editing and adjusting spot starters.
Sub SpotStarters (targetTeam$, yearNumber$, mgrNbr)

    Shared pitcherNames$()
    Shared progMG%()

    Do
        Call ShowPitcherInfo(targetTeam$)

        Locate 1, 40
        Print "     SPOT STARTERS         % USED"

        SS% = 0

        For I = 21 To 36

            Locate , 40
            Print Using "(##) "; I - 20;

            If progMG%(I) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I)); Tab(70); progMG%(I + 20)
                SS% = SS% + progMG%(I + 20)
            End If

        Next

        Locate 19, 40: Print "TOTALS MUST EQUAL 100"; Tab(70); SS%
        Locate 20, 40
        Color 15: Print "(A)";
        Color 15: Print "DD SPOT STARTER ";
        Color 15: Print "(E)";
        Color 15: Print "DIT SPOT %"
        Locate 21, 40
        Color 15: Print "(D)";
        Color 15: Print "ELETE STARTER ";
        Color 15: Print "(S)";
        Color 15: Print "AVE PROFILE"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "S":

                If SS% = 100 Then
                    progMG%(99) = 999
                    Call UpdateManagerFile(yearNumber$, mgrNbr)
                End If

            Case "D":
                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER NUMBER TO DELETE (1-16) "; I1
                Loop Until I1 >= 1 And I1 <= 16

                progMG%(I1 + 20) = -1
                progMG%(I1 + 40) = -1

            Case "E":
                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER NUMBER TO EDIT (1-16) "; I1
                Loop Until I1 >= 1 And I1 <= 16

                Do
                    Locate 20, 40: Input "ENTER % OF TIME TO SPOT START "; I2
                Loop Until I2 >= 0 And I2 <= 100

                progMG%(I1 + 40) = I2

            Case "A":

                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER SLOT TO ADD TO (1-16) "; I1
                Loop Until I1 >= 1 And I1 <= 16

                Locate 21, 40: Print "SELECT PITCHER FROM LIST"
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I2 = Val(I$)
                progMG%(I1 + 20) = I2

                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER % OF TIME TO SPOT START "; I2
                Loop Until I2 >= 0 And I2 <= 100

                progMG%(I1 + 40) = I2

        End Select

    Loop Until UCase$(I$) = "S"

End Sub

'----------------------------------------
'       RotationParams Subroutine
'----------------------------------------
'This subroutine lets the user choose the
'parameters (size / type / frequency) for
'the pitching rotation.
Sub RotationParams (targetTeam$, mgrNbr)

    Shared progMG%()
    Shared pitcherNames$()

    Do

        Call ShowPitcherInfo(targetTeam$)

        Locate 1, 40: Print "(0) SIZE OF ROTATION IS "; progMG%(1)
        Locate 3, 40: Print "    STARTING ROTATION     % STARTS"

        For I = 1 To progMG%(1)

            Locate , 40
            Print Using "(#) "; I;

            If progMG%(I + 2) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I + 2)); Tab(70); progMG%(I + 12)
            End If

        Next

        Locate 10, 40: Print "(6) MOVE TO SPOT STARTERS"

        Do
            I$ = GetKeyPress$
            I = Val(I$)

            If I = 0 Then

                Do
                    Locate 12, 40
                    Input "ENTER ROTATION SIZE (1-5) "; progMG%(1)
                Loop Until progMG%(1) >= 1 And progMG%(1) <= 5

                Locate 12, 40: Print Space$(32)
                For I = progMG%(1) + 1 To 5: progMG%(I + 2) = -1: progMG%(I + 12) = -1: Next

            End If

            If I >= 1 And I <= progMG%(1) Then

                Locate 12, 40: Print "SELECT STARTER #"; I
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)

                progMG%(I + 2) = I1
                Locate 12, 40: Print Space$(21)

                Do
                    Locate 12, 40: Input "ENTER % OF TIME TO START "; progMG%(I + 12)
                Loop While (progMG%(I + 12) >= 0 And progMG%(I + 12) <= 100)

                Locate 12, 40: Print Space$(31)

            End If

            If I = 6 Then
                Call SpotStarters(targetTeam$, yearNumber$, mgrNbr)
            End If 'Done with value 6

        Loop Until I >= 0 And I <= 6

    Loop Until I = 6

End Sub

'----------------------------------------
'      PitchingRotations Subroutine
'----------------------------------------
'This subroutine lets the user configure
'all details of a pitching rotation.
Sub ManagePitchingRotations (action%, targetTeam$, mgrNbr)

    Shared pitcherNames$()
    Shared progMG%(), pitcherRatings()

    Dim GS%(21, 1)

    'action% = 1: New Rotation
    'action% = 2: Edit Existing

    If action% = 2 Then
        '999 = rotation exists
        If progMG%(99) = 999 Then

            Call RotationParams(targetTeam$, mgrNbr)

        Else
            '4600
            Do
                Cls
                Print "ERROR:: ROTATION NOT FOUND"
                Print
                Print "DO YOU WANT TO ENTER A ROTATION? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If 'Done checking if 999

    End If 'Done for action = 2

    If UCase$(I$) = "Y" Or progMG%(99) = 999 Or progMG%(99) = -1 Then

        Cls

        'Enter Rotation
        I = 1

        Call box0 (6, 8, 8, 70, 2, 15, 1)

        Do
            Locate 7
            U$ = "COMPUTER DETERMINE ROTATION? (Y/N) "
            Locate , (80 - Len(U$)) / 2
            Print U$
            Print
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then CR% = 1

        'Adjust this as necessary
        Locate 13,
        Input "SEASON LENGTH (154, 162 ,ETC.)"; SL%

        Cls
        Print Tab(20); "SELECT SIZE OF YOUR PITCHING ROTATION"
        Print
        Color 14: Print "(1) ";
        Color 15: Print "MAN ROTATION    ";
        Color 14: Print "(4) ";
        Color 15: Print "MAN ROTATION"

        Color 14: Print "(2) ";
        Color 15: Print "MAN ROTATION    ";
        Color 14: Print "(5) ";
        Color 15: Print "MAN ROTATION"
        Color 14: Print "(3) ";
        Color 15: Print "MAN ROTATION"
        Print
        Print "       YOU MAY CONSIDER THE FOLLOWING GUIDELINES:"
        Print "MAX GAMES STARTED BY ANY PITCHER           ROTATION SIZE"
        Print "          32                                    5"
        Print "          41                                    4"
        Print "          54                                    3"
        Print "          75                                    2"
        Print "         ALL                                    1"
        Print
        Print "ALL PITCHERS AND GAMES STARTED"

        For I = 0 To 6

            Locate I + 17, 1

            If pitcherNames$(I) <> "XXX" Then
                Print pitcherNames$(I); Tab(18); pitcherRatings(I, 5);
            End If

            Locate , 25
            If pitcherNames$(I + 7) <> "XXX" Then
                Print pitcherNames$(I + 7); Tab(43); pitcherRatings(I + 7, 5);
            End If

            Locate , 50

            If pitcherNames$(I + 14) <> "XXX" Then
                Print pitcherNames$(I + 14); Tab(68); pitcherRatings(I + 14, 5)
            End If

        Next

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 5

        progMG%(1) = I
        progMG%(2) = 0

        For I = 3 To 36
            progMG%(I) = -1
        Next

        For I = 41 To 56
            progMG%(I) = 0
        Next

        progMG%(0) = 999

        If CR% <> 1 Then

            'I% = 99
            Call ShowPitcherInfo(targetTeam$)

            For I = 0 To progMG%(1) - 1

                Do
                    Locate 15, 59: Print "STARTER "; I + 1

                    Do
                        I$ = GetKeyPress$
                        Select Case UCase$(I$)
                            Case "A": I$ = "10"
                            Case "B": I$ = "11"
                            Case "C": I$ = "12"
                            Case "D": I$ = "13"
                            Case "E": I$ = "14"
                            Case "F": I$ = "15"
                            Case "G": I$ = "16"
                            Case "H": I$ = "17"
                            Case "I": I$ = "18"
                            Case "J": I$ = "19"
                            Case "K": I$ = "20"
                            Case "L": I$ = "21"
                            Case "M": I$ = "22"
                        End Select

                        I1 = Val(I$)

                    Loop Until I1 <= 21 And progMG%(3) <> I1 And progMG%(4) <> I1 And progMG%(5) <> I1 And progMG%(6) <> I1 And progMG%(7) <> I1

                    Locate 16, 59: Print pitcherNames$(I1)

                    Do
                        Locate 17, 59:
                        Print "ANY CHANGE? (Y/N)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        For I2 = 15 To 17
                            Locate I2, 59
                            Print Space$(17)
                        Next
                    End If
                Loop Until UCase$(I$) = "N"

                progMG%(I + 3) = I1

                For I2 = 15 To 17
                    Locate I2, 59
                    Print Space$(18)
                Next

            Next

        Else

            'COMPUTER SET ROTATION
            CR% = 0

            For I = 0 To 21
                If pitcherNames$(I) = "XXX" Then
                    GS%(I, 0) = 0: GS%(I, 1) = -1
                Else
                    GS%(I, 0) = pitcherRatings(I, 5): GS%(I, 1) = I
                End If
            Next

            For Z = 0 To 20

                For Z1 = Z + 1 To 21
                    If GS%(Z, 0) < GS%(Z1, 0) Then
                        Swap GS%(Z, 0), GS%(Z1, 0): Swap GS%(Z, 1), GS%(Z1, 1)
                    End If
                Next Z1

            Next Z

            For I = 0 To progMG%(1) - 1
                progMG%(I + 3) = GS%(I, 1)
            Next

            I1 = 0

            For I = progMG%(1) To 21
                I1 = I1 + 1
                If GS%(I, 0) <> 0 Then
                    progMG%(I1 + 20) = GS%(I, 1)
                Else
                    Exit For
                End If
            Next

        End If 'Done checking value of CR

        I1 = I1 - 1

        Select Case progMG%(1)
            Case 1:
                progMG%(13) = 100
                Call RotationParams(targetTeam$, mgrNbr)

            Case 2:
                GS% = 75

            Case 3:
                GS% = 54

            Case 4:
                GS% = 41

            Case 5:
                GS% = 32

        End Select

        GS% = GS% * SL% / 162

        For I = 1 To progMG%(1)
            progMG%(I + 12) = Int(pitcherRatings(progMG%(I + 2), 5) * 100 / GS% + .5)
        Next

        SS% = 0

        For I = 21 To 36

            If progMG%(I) >= 0 And progMG%(I) <= 21 Then
                SS% = SS% + pitcherRatings(progMG%(I), 5)
            End If

        Next

        For I = 21 To 36
            If progMG%(I) >= 0 And progMG%(I) <= 21 Then
                progMG%(I + 20) = Int(pitcherRatings(progMG%(I), 5) * 100 / SS% + .5)
            End If
        Next

        Call RotationParams(targetTeam$, mgrNbr)

    End If

End Sub

'----------------------------------------
'    SetupMiddleRelievers Subroutine
'----------------------------------------
'This subroutine helps the users with the
'assignment of middle relievers.
Sub SetupMiddleRelievers (targetTeam$, yearNumber$, mgrNbr)

    Shared progMG%(), teamRatings%(), pitcherRatings()
    Shared progB1$(), pitcherNames$()

    Do

        'MIDDLE RELIEVERS
        If progMG%(60) <> 999 Then

            progMG%(60) = 999

            For I = 1 To 10
                progMG%(I + 60) = -1
                progMG%(I + 70) = 0
            Next

        End If

        RA% = 0

        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" Then
                RA% = RA% + pitcherRatings(I, 4) - pitcherRatings(I, 5) - pitcherRatings(I, 3)
            End If
        Next

        Cls
        Locate 1, 1
        Print "#";
        Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Color 14, 0
        Locate , 16: Print "T   G GS   W- L %RA   ERA"

        For I = 0 To 21

            If pitcherNames$(I) <> "XXX" And Left$(pitcherNames$(I), 1) <> " " And pitcherNames$(I) <> "" Then

                Color 2, 0
                If I <= 9 Then
                    Print Using "#"; I;
                Else
                    Print alpha$(I - 10);
                End If
                Color 15, 0

                For I1 = 1 To 10
                    If I = progMG%(I1 + 60) Then Color teamRatings%(11), teamRatings%(12)
                Next

                Locate , 3: Print pitcherNames$(I);
                Color 15, 0
                Locate , 16: Print progB1$(pitcherRatings(I, 0) + 2);
                Locate , 17: Print Using " ### ##"; pitcherRatings(I, 4); pitcherRatings(I, 5);
                Locate , 24: Print Using "  ##"; pitcherRatings(I, 1);
                Print "-";
                Print Using "## ### ##.##"; pitcherRatings(I, 2); (pitcherRatings(I, 4) - pitcherRatings(I, 5) - pitcherRatings(I, 3)) / RA% * 100; pitcherRatings(I, 10) / 100
            End If

        Next I

        Locate 18, 45: Print "%RA - PITCHER'S PERCENTAGE OF TEAM'S"
        Locate , 45: Print "RELIEF APPEARANCES MINUS SAVES"

        Locate 1, 48: Print "  MIDDLE RELIEVERS      % APPEAR"

        For I = 1 To 10
            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 60) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I + 60)); Tab(75); progMG%(I + 70)
            End If

        Next

        Locate 13, 48: Print "(11) ACCEPT CHOICES"
        Locate 14, 48: Print "(12) DELETE A RELIEVER"

        Do
            Locate 15, 48
            Input "ENTER SELECTION (1-12)"; I
        Loop Until I >= 1 And I <= 12

        Select Case I

            Case 1 To 10

                Locate 16, 48: Print Using "SELECT RELIEVER ##:"; I;

                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)

                progMG%(I + 60) = I1

                Do
                    Locate 17, 48
                    Input "ENTER % OF TIME TO APPEAR "; progMG%(I + 70)
                Loop Until progMG%(I + 70) >= 0 And progMG%(I + 70) <= 100

                Locate 17, 48: Print Space$(31)
                Locate 16, 48: Print Space$(31)

            Case 11:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 12:
                Do
                    Locate 16, 48
                    Input "SELECT RELIEVER TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 10

                progMG%(choiceNum + 60) = -1
                progMG%(choiceNum + 70) = 0

        End Select

    Loop Until I = 11

End Sub

'----------------------------------------
'       SetupRelievers Subroutine
'----------------------------------------
'This subroutine helps the users with the
'assignment of relievers.
Sub SetupRelievers (targetTeam$, yearNumber$, mgrNbr)

    Shared progB1$(), pitcherNames$()
    Shared teamRatings%(), progMG%(), pitcherRatings()

    Do

        'SET-UP RELIEVERS
        If progMG%(90) <> 999 Then

            progMG%(90) = 999

            For I = 1 To 6:
                progMG%(I + 80) = -1: progMG%(I + 90) = 0
            Next

        End If

        Cls
        Locate 1, 1
        Print "#";
        Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "T   G GS   W- L SV   ERA"

        For I = 0 To 21

            If pitcherNames$(I) <> "XXX" And Left$(pitcherNames$(I), 1) <> " " And pitcherNames$(I) <> "" Then

                Color 2, 0

                If I <= 9 Then
                    Print Using "#"; I;
                Else
                    Print alpha$(I - 10);
                End If

                Color 15, 0

                For I1 = 1 To 6
                    If I = progMG%(I1 + 80) Then Color teamRatings%(11), teamRatings%(12)
                Next

                Locate , 3: Print pitcherNames$(I);

                Color 15, 0

                Locate , 16: Print progB1$(pitcherRatings(I, 0) + 2);
                Locate , 17: Print Using " ### ##"; pitcherRatings(I, 4); pitcherRatings(I, 5);
                Locate , 24: Print Using "  ##"; pitcherRatings(I, 1);
                Print "-";
                Print Using "## ### ##.##"; pitcherRatings(I, 2); pitcherRatings(I, 3); pitcherRatings(I, 10) / 100
            End If
        Next I

        Locate 1, 48: Print "  SET-UP RELIEVERS      % APPEAR"
        Locate , 48: Print "   -----  VS LEFT  -------      "

        For I = 1 To 3

            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 80) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I + 80)); Tab(75); progMG%(I + 90)
            End If

        Next

        Locate , 48: Print "   -----  VS RIGHT  -------      "

        For I = 4 To 6

            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 80) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I + 80)); Tab(75); progMG%(I + 90)
            End If

        Next

        Locate 13, 48: Print "(7) ACCEPT CHOICES"
        Locate 14, 48: Print "(8) DELETE A RELIEVER"

        Do
            Locate 15, 48
            Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I

            Case 1 To 6:
                Locate 16, 48: Print Using "SELECT RELIEVER ##:"; I;
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)
                progMG%(I + 80) = I1

                Do
                    Locate 17, 48
                    Input "ENTER % OF TIME TO APPEAR "; progMG%(I + 90)
                Loop Until progMG%(I + 90) >= 0 And progMG%(I + 90) <= 100

                Locate 17, 48: Print Space$(31)
                Locate 16, 48: Print Space$(31)

            Case 7:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 8:
                Do
                    Locate 16, 48
                    Input "SELECT RELIEVER TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 6

                progMG%(choiceNum + 80) = -1
                progMG%(choiceNum + 90) = 0

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'       SetupClosers Subroutine
'----------------------------------------
'This subroutine helps the users with the
'assignment of closers.
Sub SetupClosers (targetTeam$, yearNumber$, mgrNbr)

    Shared progB1$(), pitcherNames$()
    Shared teamRatings%(), progMG%(), pitcherRatings()

    Do

        'CLOSERS
        If progMG%(520) <> 999 Then
            progMG%(520) = 999
            For I = 1 To 6: progMG%(I + 520) = -1: progMG%(I + 530) = 0: Next
        End If

        Cls
        Locate 1, 1
        Print "#";: Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "T   G GS   W- L SV   ERA"

        For I = 0 To 21

            If pitcherNames$(I) <> "XXX" And Left$(pitcherNames$(I), 1) <> " " And pitcherNames$(I) <> "" Then

                Color 2, 0

                If I <= 9 Then
                    Print Using "#"; I;
                Else
                    Print alpha$(I - 10);
                End If

                Color 15, 0

                For I1 = 1 To 6
                    If I = progMG%(I1 + 520) Then Color teamRatings%(11), teamRatings%(12)
                Next

                Locate , 3: Print pitcherNames$(I);

                Color 15, 0

                Locate , 16: Print progB1$(pitcherRatings(I, 0) + 2);
                Locate , 17: Print Using " ### ##"; pitcherRatings(I, 4); pitcherRatings(I, 5);
                Locate , 24: Print Using "  ##"; pitcherRatings(I, 1);
                Print "-";
                Print Using "## ### ##.##"; pitcherRatings(I, 2); pitcherRatings(I, 3); pitcherRatings(I, 10) / 100

            End If

        Next I

        Locate 1, 48: Print "          CLOSERS       % APPEAR"
        Locate , 48: Print "   -----  VS LEFT  -------      "

        For I = 1 To 3

            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 520) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I + 520)); Tab(75); progMG%(I + 530)
            End If

        Next

        Locate , 48: Print "   -----  VS RIGHT  -------      "

        For I = 4 To 6
            Locate , 48
            Print Using "(##) "; I;

            If progMG%(I + 520) < 0 Then
                Print
            Else
                Print pitcherNames$(progMG%(I + 520)); Tab(75); progMG%(I + 530)
            End If
        Next

        Locate 13, 48: Print "(7) ACCEPT CHOICES"
        Locate 14, 48: Print "(8) DELETE A RELIEVER"

        Do
            Locate 15, 48: Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I

            Case 1 To 6:
                Locate 16, 48: Print Using "SELECT RELIEVER ##:"; I;
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)
                progMG%(I + 520) = I1

                Do
                    Locate 17, 48
                    Input "ENTER % OF TIME TO APPEAR "; progMG%(I + 530)
                Loop Until progMG%(I + 530) >= 0 And progMG%(I + 530) <= 100

                Locate 17, 48: Print Space$(31)
                Locate 16, 48: Print Space$(31)

            Case 7:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 8:

                Do
                    Locate 16, 48: Input "SELECT RELIEVER TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 6

                progMG%(choiceNum + 520) = -1
                progMG%(choiceNum + 530) = 0

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'       HittingStrategy Subroutine
'----------------------------------------
'This subroutine gives the user a series of
'menu options for configuring a hitting
'strategy for the manager.
Sub HittingStrategy (yearNumber$, mgrNbr)

    Shared progMG%()

    Do
        'HITTING STRATEGIES
        Cls
        Print Tab(28);
        Color 14, 4
        Print "HITTING STRATEGIES"
        Color 15, 0
        Print
        ReDim menuChoices$(1 To 6)
        menuChoices$(1) = "SITUATIONS TO PH FOR PITCHER                 "
        menuChoices$(2) = "SITUATIONS FOR PITCHER TO SAC BUNT IN        "
        menuChoices$(3) = "SITUATONS FOR POSITION PLAYERS TO SAC BUNT IN"
        menuChoices$(4) = "HIT AND RUN SCENARIOS                        "
        menuChoices$(5) = "SITUATIONS TO PH FOR POSITION PLAYERS        "
        menuChoices$(6) = "RETURN TO MAIN MENU                          "
        SELECTION% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 5
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        Select Case SELECTION%

            Case 1:
                Call PHPITCHER(yearNumber$, mgrNbr)

            Case 2:
                Do
                    'SAC BUNT HIT BY PITCHER"
                    Cls

                    If progMG%(609) <> 999 Then

                        progMG%(609) = 999

                        For I = 1 To 24:
                            progMG%(609 + I) = 0
                        Next

                    End If

                    Locate 1, 24
                    Color 14, 2: Print "SACRIFICE BUNTING FOR THE PITCHER"
                    Color 15, 0: Print
                    Color 15, 1: Print " A ";
                    Color 15, 0: Print " OUTS OR LESS"

                    Print "IF ";
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " OR LESS RUNS AHEAD AND"

                    Color 15, 1: Print " C ";
                    Color 15, 0: Print " OR LESS RUNS BEHIND"

                    Color 15, 1: Print " D ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"

                    Print "SCENARIO                       A      B     C      D"
                    Color 15
                    Print "1 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; progMG%(610); progMG%(611); progMG%(612); progMG%(613)
                    Print "    RUNNER ON 1ST           "
                    Print "2 - 8TH INNING ON              "; Using "#     ##    ##     ###"; progMG%(614); progMG%(615); progMG%(616); progMG%(617)
                    Print "    RUNNER ON 1ST           "
                    Print "3 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; progMG%(618); progMG%(619); progMG%(620); progMG%(621)
                    Print "    RUNNER ON 2ND           "
                    Print "4 - 8TH INNING ON              "; Using "#     ##    ##     ###"; progMG%(622); progMG%(623); progMG%(624); progMG%(625)
                    Print "    RUNNER ON 2ND           "
                    Print "5 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; progMG%(626); progMG%(627); progMG%(628); progMG%(629)
                    Print "    RUNNERS ON 1ST AND 2ND  "
                    Print "6 - 8TH INNING ON              "; Using "#     ##    ##     ###"; progMG%(630); progMG%(631); progMG%(632); progMG%(633)
                    Print "    RUNNERS ON 1ST AND 2ND  "
                    Print "7 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-7) "; I
                    Loop Until I >= 1 And I <= 7

                    If I = 7 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "OUTS OR LESS          "; progMG%(I * 4 + 610)
                        Locate 22, 1: Input "RUNS AHEAD OR LESS    "; progMG%(I * 4 + 611)
                        Locate 22, 1: Input "RUNS BEHIND OR LESS   "; progMG%(I * 4 + 612)
                        Locate 22, 1: Input "PERCENT OF TIME       "; progMG%(I * 4 + 613)
                    End If

                Loop Until I = 7

            Case 3:
                Do
                    'SAC BUNT HIT BY POSITION PLAYERS"
                    Cls

                    If progMG%(634) <> 999 Then
                        progMG%(634) = 999
                        For I = 1 To 36: progMG%(634 + I) = 0: Next
                    End If

                    Locate 1, 24
                    Color 14, 2: Print "SACRIFICE BUNTING FOR THE POSITION PLAYERS"
                    Color 15, 0: Print
                    Print "IF ";
                    Color 15, 1: Print " A ";
                    Color 15, 0: Print " OR LESS RUNS AHEAD AND"
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " OR LESS RUNS BEHIND"

                    Print "BATTING AVERAGE IS LESS THAN ";
                    Color 15, 1: Print " C ";
                    Color 15, 0: Print " AND SLUGGING PCT IS LESS THAN ";
                    Color 15, 1: Print " D "

                    Color 15, 0: Print "PITCHERS ERA LESS THAN ";
                    Color 15, 1: Print " E ";
                    Color 15, 0: Print ", ";
                    Color 15, 1: Print " F ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"

                    Print "SCENARIO                        A     B      C      D      E     F"
                    Color 15
                    Print "1 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; progMG%(635); progMG%(636); progMG%(637); progMG%(638); progMG%(639); progMG%(640)
                    Print "    0 OUTS, RUNNER ON 1ST"
                    Print "2 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; progMG%(641); progMG%(642); progMG%(643); progMG%(644); progMG%(645); progMG%(646)
                    Print "    0 OUTS, RUNNER ON 2ND"
                    Print "3 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; progMG%(647); progMG%(648); progMG%(649); progMG%(650); progMG%(651); progMG%(652)
                    Print "    0 OUTS, RUNNERS 1ST AND 2ND"
                    Print "4 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; progMG%(653); progMG%(654); progMG%(655); progMG%(656); progMG%(657); progMG%(658)
                    Print "    0 OUTS, RUNNER ON 1ST"
                    Print "5 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; progMG%(659); progMG%(660); progMG%(661); progMG%(662); progMG%(663); progMG%(664)
                    Print "    0 OUTS, RUNNER ON 2ND"
                    Print "6 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; progMG%(665); progMG%(666); progMG%(667); progMG%(668); progMG%(669); progMG%(670)
                    Print "    0 OUTS, RUNNERS ON 1ST AND 2ND"
                    Print "7 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-7) "; I
                    Loop Until I >= 1 And I <= 7

                    If I = 7 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "RUNS AHEAD OR LESS        "; progMG%(I * 6 + 635)
                        Locate 22, 1: Input "RUNS BEHIND OR LESS       "; progMG%(I * 6 + 636)
                        Locate 22, 1: Input "BATTING AVERAGE LESS THAN "; progMG%(I * 6 + 637)
                        Locate 22, 1: Input "SLUGGING PCT LESS THAT    "; progMG%(I * 6 + 638)
                        Locate 22, 1: Input "PITCHER'S ERA LESS THAN   "; progMG%(I * 6 + 639)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 6 + 640)
                    End If

                Loop Until I = 7

            Case 4:
                Call HitAndRun(yearNumber$, mgrNbr)

            Case 5:
                Call PHBATTER(yearNumber$, mgrNbr)

            Case 6:
                Cls '

        End Select

    Loop Until SELECTION% = 6 Or ExitCode% = 27

End Sub

'----------------------------------------
'       HitAndRun Subroutine
'----------------------------------------
'This subroutine gives the user a series of
'menu options for configuring a hit and run
'strategy for the manager.
Sub HitAndRun (yearNumber$, mgrNbr)

    Shared progMG%()

    Do
        'HIT AND RUN
        Cls

        If progMG%(671) <> 999 Then

            progMG%(671) = 999

            For I = 1 To 18:
                progMG%(671 + I) = 0
            Next

        End If

        Locate 1, 30
        Color 14, 2: Print "HIT AND RUN STRATEGY"
        Color 15, 0
        Print
        Print "IF ";
        Color 15, 1: Print " A ";
        Color 15, 0: Print " OR LESS RUNS AHEAD AND"
        Color 15, 1: Print " B ";
        Color 15, 0: Print " OR LESS RUNS BEHIND"
        Color 15, 1: Print " C ";
        Color 15, 0: Print " % OF THE TIME"
        Color 15, 1: Print " D ";
        Color 15, 0: Print " AB PER STRIKEOUT"
        Print " SLUGGING PCT LESS THAN OR EQUAL TO ";
        Color 15, 1: Print " E "
        Color 15, 0

        Print Tab(32);
        Color 14: Print "TABLE OF VALUES"

        Print "    SCENARIO                          A     B      C   D    E"
        Color 15
        Print "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##    ###  ##  ###"; progMG%(672); progMG%(673); progMG%(674); progMG%(866); progMG%(872)
        Print "2 - 1 OUT, RUNNER ON 1ST             "; Using "##    ##    ###  ##  ###"; progMG%(675); progMG%(676); progMG%(677); progMG%(867); progMG%(873)
        Print "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; Using "##    ##    ###  ##  ###"; progMG%(678); progMG%(679); progMG%(680); progMG%(868); progMG%(874)
        Print "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; Using "##    ##    ###  ##  ###"; progMG%(681); progMG%(682); progMG%(683); progMG%(869); progMG%(875)
        Print "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; Using "##    ##    ###  ##  ###"; progMG%(684); progMG%(685); progMG%(686); progMG%(870); progMG%(876)
        Print "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; Using "##    ##    ###  ##  ###"; progMG%(687); progMG%(688); progMG%(689); progMG%(871); progMG%(877)
        Print "7 - ACCEPT CHOICES"

        Do
            Locate 22, 1
            Input "ENTER SELECTION (1-7) "; I
        Loop Until I >= 1 And I <= 7

        If I = 7 Then
            Call UpdateManagerFile(yearNumber$, mgrNbr)
        Else
            I = I - 1
            Locate 22, 1: Input "RUNS AHEAD OR LESS        "; progMG%(I * 3 + 672)
            Locate 22, 1: Input "RUNS BEHIND OR LESS       "; progMG%(I * 3 + 673)
            Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 3 + 674)
            Locate 22, 1: Input "AB PER STRIKEOUT          "; progMG%(I + 866)
            Locate 22, 1: Input "SLUGGING PCT <=           "; progMG%(I + 872)
        End If

    Loop Until I = 7

End Sub

'----------------------------------------
'      DefensiveStrategy Subroutine
'----------------------------------------
'This subroutine gives the user a series of
'menu options for configuring a defensive
'strategy for the manager.
Sub DefensiveStrategy (yearNumber$, mgrNbr)

    Shared progMG%()

    Do
        'DEFENSIVE STRATEGIES
        Cls
        Print Tab(31);
        Color 14, 4
        Print "DEFENSIVE STRATEGIES"
        Color 15, 0
        Print

        ReDim menuChoices$(1 To 9)
        menuChoices$(1) = "INFIELD IN WITH 3RD BASE OCCUPIED            "
        menuChoices$(2) = "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT"
        menuChoices$(3) = "CORNERS IN VS HITTER IN SAC BUNT SITUATION   "
        menuChoices$(4) = "CORNERS IN VS PITCHER IN SAC BUNT SITUATION  "
        menuChoices$(5) = "CORNERS IN VS HITTER WHO MIGHT SQUEEZE BUNT  "
        menuChoices$(6) = "GUARD LINES IN LATE INNINGS                  "
        menuChoices$(7) = "PITCH AROUND THE HITTER                      "
        menuChoices$(8) = "NOT THROWING HOME WITH RUNNER TRYING TO SCORE"
        menuChoices$(9) = "RETURN TO MAIN MENU                          "
        SELECTION% = 1
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 5
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        Select Case SELECTION%
            Case 1
                Do
                    'INFIELD IN
                    Cls

                    If progMG%(690) <> 999 Then
                        progMG%(690) = 999
                        For I = 1 To 16: progMG%(690 + I) = 0: Next
                    End If

                    Locate 1, 30
                    Color 14, 2: Print "INFIELD IN WITH 3RD BASE OCCUPIED"
                    Color 15, 0
                    Print
                    Print "IF DOWN BY ";
                    Color 15, 1: Print " A ";
                    Color 15, 0: Print " RUNS"
                    Print "AND NOT MORE THAN ";
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " RUNS"
                    Print "FROM ";
                    Color 15, 1: Print " C ";
                    Color 15, 0: Print "INNING ON"
                    Color 15, 1: Print " D ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"

                    Print "SCENARIO                              A     B   C     D"
                    Color 15
                    Print "1 - 0 OUTS                           "; Using "##    ##   #   ###"; progMG%(691); progMG%(692); progMG%(693); progMG%(694)
                    Print "2 - 0 OUTS                           "; Using "##    ##   #   ###"; progMG%(695); progMG%(696); progMG%(697); progMG%(698)
                    Print "3 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; progMG%(699); progMG%(700); progMG%(701); progMG%(702)
                    Print "4 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; progMG%(703); progMG%(704); progMG%(705); progMG%(706)
                    Print "5 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-5) "; I
                    Loop Until I >= 1 And I <= 5

                    If I = 5 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "AT LEAST DOWN BY          "; progMG%(I * 4 + 691)
                        Locate 22, 1: Input "BUT NOT MORE THAN         "; progMG%(I * 4 + 692)
                        Locate 22, 1: Input "THIS INNING ON            "; progMG%(I * 4 + 693)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 4 + 694)
                    End If

                Loop Until I = 5

            Case 2
                Do
                    'DEFENSIVE STRATEGIES"
                    Cls

                    If progMG%(707) <> 999 Then
                        progMG%(707) = 999
                        For I = 1 To 6:
                            progMG%(707 + I) = 0
                        Next
                    End If

                    Locate 1, 22
                    Color 14, 2: Print "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT"
                    Color 15, 0
                    Print
                    Print "IF BUNT RATING IS >= ";
                    Color 15, 1: Print " A "
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"
                    Color 15

                    Print "      A      B"
                    Print Using "(1)  ##    ###"; progMG%(708); progMG%(709)
                    Print Using "(2)  ##    ###"; progMG%(710); progMG%(711)
                    Print Using "(3)  ##    ###"; progMG%(712); progMG%(713)
                    Print "(4) - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-4) "; I
                    Loop Until I >= 1 And I <= 4

                    If I = 4 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "BUNT RATING >=            "; progMG%(I * 2 + 708)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 2 + 709)
                    End If

                Loop Until I = 4

            Case 3
                Do
                    'HITTER SAC BUNT
                    Cls
                    If progMG%(714) <> 999 Then
                        progMG%(714) = 999: For I = 1 To 16: progMG%(714 + I) = 0: Next
                    End If

                    Locate 1, 20: Color 14, 2: Print "CORNERS IN VS HITTER IN SAC HIT SITUATION"
                    Color 15, 0: Print
                    Print "IF AHEAD BY  ";
                    Color 15, 1: Print " A ";
                    Color 15, 0: Print " RUNS OR LESS"
                    Print "OR BEHIND BY NOT MORE THAN ";
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " RUNS"
                    Print "BUNT RATING >= ";
                    Color 15, 1: Print " C "
                    Color 15, 1: Print " D ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"

                    Print "SCENARIO                              A     B   C     D"
                    Color 15
                    Print "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; progMG%(715); progMG%(716); progMG%(717); progMG%(718)
                    Print "2 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; progMG%(719); progMG%(720); progMG%(721); progMG%(722)
                    Print "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; progMG%(723); progMG%(724); progMG%(725); progMG%(726)
                    Print "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; progMG%(727); progMG%(728); progMG%(729); progMG%(730)
                    Print "5 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-5) "; I
                    Loop Until I >= 1 And I <= 5

                    If I = 5 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "AHEAD BY NOT MORE THAN    "; progMG%(I * 4 + 715)
                        Locate 22, 1: Input "DOWN BY NOT MORE THAN     "; progMG%(I * 4 + 716)
                        Locate 22, 1: Input "BUNT RATING >=            "; progMG%(I * 4 + 717)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 4 + 718)
                    End If

                Loop Until I = 5

            Case 4
                Do
                    'PITCHER SAC BUNT
                    Cls

                    If progMG%(731) <> 999 Then
                        progMG%(731) = 999
                        For I = 1 To 8: progMG%(731 + I) = 0: Next
                    End If

                    Locate 1, 20: Color 14, 2: Print "CORNERS IN VS PITCHER IN SAC HIT SITUATION"
                    Color 15, 0: Print
                    Print "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO ";
                    Color 15, 1: Print " A "
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"

                    Print "SCENARIO                                  A      B"
                    Color 15
                    Print "1 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; progMG%(732); progMG%(733)
                    Print "2 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; progMG%(734); progMG%(735)
                    Print "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; progMG%(736); progMG%(737)
                    Print "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; progMG%(738); progMG%(739)
                    Print "5 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-5) "; I
                    Loop Until I >= 1 And I <= 5

                    If I = 5 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "BATTING AVG LESS THAN     "; progMG%(I * 2 + 732)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 2 + 733)
                    End If

                Loop Until I = 5

            Case 5
                Do
                    'HITTER SAC BUNT SQUEEZE
                    Cls

                    If progMG%(740) <> 999 Then
                        progMG%(740) = 999
                        For I = 1 To 8: progMG%(740 + I) = 0: Next
                    End If

                    Locate 1, 20
                    Color 14, 2: Print "CORNERS IN VS HITTER IN SQUEEZE SITUATION"
                    Color 15, 0
                    Print
                    Print "IF AHEAD BY  ";
                    Color 15, 1: Print " A ";
                    Color 15, 0: Print " RUNS OR LESS"
                    Print "OR BEHIND BY NOT MORE THAN ";
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " RUNS"
                    Print "BUNT RATING >= ";
                    Color 15, 1: Print " C "
                    Color 15, 1: Print " D ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"

                    Print "SCENARIO                              A     B   C     D"
                    Color 15
                    Print "1 - 0 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; progMG%(741); progMG%(742); progMG%(743); progMG%(744)
                    Print "2 - 1 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; progMG%(745); progMG%(746); progMG%(747); progMG%(748)
                    Print "3 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-3) "; I
                    Loop Until I >= 1 And I <= 3

                    If I = 3 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "AHEAD BY NOT MORE THAN    "; progMG%(I * 4 + 741)
                        Locate 22, 1: Input "DOWN BY NOT MORE THAN     "; progMG%(I * 4 + 742)
                        Locate 22, 1: Input "BUNT RATING >=            "; progMG%(I * 4 + 743)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 4 + 744)
                    End If

                Loop Until I = 3

            Case 6
                Do
                    'GUARD LINES
                    Cls

                    If progMG%(749) <> 999 Then
                        progMG%(749) = 999
                        For I = 1 To 6: progMG%(749 + I) = 0: Next
                    End If

                    Locate 1, 30: Color 14, 2: Print "GUARD LINES STRATEGY"
                    Color 15, 0: Print
                    Print "IF ";
                    Color 15, 1: Print " A ";
                    Color 15, 0: Print " RUNS AHEAD"
                    Color 15, 1: Print " B ";
                    Color 15, 0: Print " INNING ON"
                    Color 15, 1: Print " C ";
                    Color 15, 0: Print " % OF THE TIME"

                    Print Tab(32);
                    Color 14: Print "TABLE OF VALUES"
                    Color 15

                    Print "     A     B      C"
                    Print "1 - "; Using "##    ##    ###"; progMG%(750); progMG%(751); progMG%(752)
                    Print "2 - "; Using "##    ##    ###"; progMG%(753); progMG%(754); progMG%(755)
                    Print "3 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
                    Loop Until I >= 1 And I <= 3

                    If I = 3 Then
                        Call UpdateManagerFile(yearNumber$, mgrNbr)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "RUNS AHEAD                "; progMG%(I * 3 + 750)
                        Locate 22, 1: Input "INNING ON                 "; progMG%(I * 3 + 751)
                        Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 3 + 752)
                    End If
                Loop Until I = 3

            Case 7:
                Call PITCHAROUND(yearNumber$, mgrNbr)

            Case 8:
                Call THROWHOME(yearNumber$, mgrNbr)

            Case 9: Cls

        End Select

    Loop Until SELECTION% = 9 Or ExitCode% = 27

End Sub

'----------------------------------------
'       PinchRunStrategy Subroutine
'----------------------------------------
'This subroutine gives the user a series of
'menu options for configuring a pinch and run
'strategy for the manager.
Sub PinchRunStrategy (yearNumber$, targetTeam$, mgrNbr)

    Shared progMG%()

    Do
        'PINCH RUNNING STRATEGY
        Cls

        Print Tab(31);

        Color 14, 2
        Print "PINCH RUNNING STRATEGIES"
        Color 15, 0
        Print

        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "SELECT PINCH RUNNERS                                  "
        menuChoices$(2) = "SELECT PLAYERS TO PINCH RUN FOR AND THEIR REPLACEMENTS"
        menuChoices$(3) = "SITUATIONS TO PINCH RUN FOR THE PITCHER               "
        menuChoices$(4) = "RETURN TO MAIN MENU                                   "
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 5
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        Select Case SELECTION%

            Case 1:
                Call PinchRunnerOptions(yearNumber$, targetTeam$, mgrNbr)

            Case 2:
                Call DefenseSubPinchRun(yearNumber$, targetTeam$, mgrNbr)

            Case 3:
                'PINCH RUN PITCHER
                Cls

                If progMG%(803) <> 999 Then
                    progMG%(803) = 999
                    For I = 1 To 6: progMG%(803 + I) = 0: Next
                End If

                Locate 1, 30
                Color 14, 2: Print "PINCH RUN FOR PITCHER"
                Color 15, 0
                Print
                Print "RUN RATING <= ";
                Color 15, 1: Print " A "
                Color 15, 1: Print " B ";
                Color 15, 0: Print " INNING ON"
                Color 15, 1: Print " C ";
                Color 15, 0: Print " % OF THE TIME"

                Print Tab(32);
                Color 14: Print "TABLE OF VALUES"

                Print "SCENARIO                A     B      C"
                Color 15
                Print "1 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; progMG%(804); progMG%(805); progMG%(806)
                Print "2 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; progMG%(807); progMG%(808); progMG%(809)
                Print "3 - ACCEPT CHOICES"

                Do
                    Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
                Loop Until I >= 1 And I <= 3

                If I = 3 Then
                    Call UpdateManagerFile(yearNumber$, mgrNbr)
                Else
                    I = I - 1
                    Locate 22, 1: Input "RUN RATING <=             "; progMG%(I * 3 + 804)
                    Locate 22, 1: Input "INNING ON                 "; progMG%(I * 3 + 805)
                    Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 3 + 806)
                End If
            Case 4: Cls '

        End Select

    Loop Until SELECTION% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'     PinchRunnerOptions Subroutine
'----------------------------------------
'This subroutine assists the user with the
'setup of pinch runners
Sub PinchRunnerOptions (yearNumber$, targetTeam$, mgrNbr)

    Shared teamRatings%(), progMG%(), batterRatings()
    Shared batterNames$()

    Do

        'PINCH RUNNERS
        Cls
        Print Tab(26);: Color 14, 4
        Print "PINCH RUNNING STRATEGY OPTIONS"
        Print
        Print
        Color , 0

        If progMG%(771) <> 999 Then
            progMG%(771) = 999
            For I = 1 To 6: progMG%(I + 771) = -1: Next
        End If

        Cls

        Locate 1, 1
        Print "#";
        Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Locate , 16: Print "RUN RATING SB   CS"
        I1 = 0

        For I = 0 To 22:
            Locate , 1

            If batterNames$(I) = "XXX" Or Left$(batterNames$(I), 1) = " " Or batterNames$(I) = "" Then
                I1 = I1 + 1
            Else

                Locate I + 2 - I1, 1
                Color 2, 0

                If I < 10 Then
                    Print Using "#"; I;
                    Color 15, 0
                    If batterRatings(I, 21) >= 98 Then Color 0, 7
                End If

                If I > 9 Then
                    Print alpha$(I - 10);
                    Color 15, 0
                    If batterRatings(I, 21) >= 98 Then Color 0, 7
                End If

                Locate , 3: Print batterNames$(I);: Color 15, 0
                Locate , 17: Print Using "   #     ###  ###"; batterRatings(I, 16); batterRatings(I, 13); batterRatings(I, 14);

            End If
        Next

        Locate 1, 46
        Print "# PLAYER"

        For I = 1 To 6

            Locate , 46
            Print Using "# "; I;

            If progMG%(I + 771) < 0 Then
                Print
            Else
                Print batterNames$(progMG%(I + 771))
            End If

        Next

        Locate 9, 46: Print "(7) ACCEPT CHOICES"
        Locate 10, 46: Print "(8) DELETE A CHOICE"

        Do
            Locate 11, 46
            Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I

            Case 1 To 6:

                Locate 12, 46: Print Using "SELECT RUNNER #:"; I;
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)
                progMG%(I + 771) = I1

            Case 7:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 8:
                Do
                    Locate 12, 46
                    Input "SELECT CHOICE TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 6

                progMG%(choiceNum + 771) = -1

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'       DefenseSubPinchRun Subroutine
'----------------------------------------
'This subroutine assists the user with the
'setup of defensive subs for pinch runners
Sub DefenseSubPinchRun (yearNumber$, targetTeam$, mgrNbr)

    Shared batterNames$()
    Shared teamRatings%(), progMG%(), batterRatings()

    Do
        'DEFENSIVE REPLACEMENTS
        Cls

        Locate 20, 46: Print "DEFENSIVE SUBS FOR PINCH RUNNERS"
        Locate 21, 46: Print "SCORE TIED OR DOWN BY ONE RUN"

        If progMG%(778) <> 999 Then progMG%(778) = 999: For I = 1 To 24: progMG%(I + 778) = -1: Next

        Locate 1, 1
        Print "#";: Color teamRatings%(11), teamRatings%(12): Print targetTeam$;
        Locate , 17: Print "1 FAVG 2 FAVG 3 FAVG 4 FAVG"

        I1 = 0

        For I = 0 To 22: Locate , 1

            If batterNames$(I) = "XXX" Or Left$(batterNames$(I), 1) = " " Or batterNames$(I) = "" Then
                I1 = I1 + 1
            Else
                Locate I + 2 - I1, 1
                Color 2, 0

                If I < 10 Then
                    Print Using "#"; I;
                    Color 15, 0
                    If batterRatings(I, 21) >= 98 Then Color 0, 7
                End If
                If I > 9 Then
                    Print alpha$(I - 10);
                    Color 15, 0
                    If batterRatings(I, 21) >= 98 Then Color 0, 7
                End If

                Locate , 3
                Print batterNames$(I);: Color 15, 0
                Locate , 17: If batterRatings(I, 22) <> -1 Then Print Using "# ####"; batterRatings(I, 22); batterRatings(I, 26);
                Locate , 24: If batterRatings(I, 23) <> -1 Then Print Using "# ####"; batterRatings(I, 23); batterRatings(I, 27);
                Locate , 31: If batterRatings(I, 24) <> -1 Then Print Using "# ####"; batterRatings(I, 24); batterRatings(I, 28);
                Locate , 38: If batterRatings(I, 25) <> -1 Then Print Using "# ####"; batterRatings(I, 25); batterRatings(I, 29);
            End If
        Next

        Locate 1, 46
        Print "# PLAYER     I  SUBSTITUTE  %"

        For I = 0 To 5

            Locate , 46
            Print Using "# "; I + 1;

            If progMG%(I * 4 + 779) < 0 Then
                Print
            Else
                Print Left$(batterNames$(progMG%(I * 4 + 779)), 10); Tab(59);
                Print Using "## "; progMG%(I * 4 + 780);
                Print Left$(batterNames$(progMG%(I * 4 + 781)), 10); Tab(73); Using "###"; progMG%(I * 4 + 782)
            End If

        Next

        Locate 9, 46: Print "(7) ACCEPT CHOICES"
        Locate 10, 46: Print "(8) DELETE A CHOICE"

        Do
            Locate 11, 46
            Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I

            Case 1 To 6:
                I = I - 1
                Locate 12, 46: Print Using "SELECT HITTER #:"; I;
                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)

                progMG%(I * 4 + 779) = I1
                Locate 13, 46: Input "INNING "; I1

                progMG%(I * 4 + 780) = I1
                Locate 14, 46: Print "SELECT SUB #:";

                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case "A": I$ = "10"
                    Case "B": I$ = "11"
                    Case "C": I$ = "12"
                    Case "D": I$ = "13"
                    Case "E": I$ = "14"
                    Case "F": I$ = "15"
                    Case "G": I$ = "16"
                    Case "H": I$ = "17"
                    Case "I": I$ = "18"
                    Case "J": I$ = "19"
                    Case "K": I$ = "20"
                    Case "L": I$ = "21"
                    Case "M": I$ = "22"
                End Select

                I1 = Val(I$)
                progMG%(I * 4 + 781) = I1

                Do
                    Locate 15, 46
                    Input "ENTER % OF TIME TO USE "; progMG%(I * 4 + 782)
                Loop Until progMG%(I * 4 + 782) >= 0 And progMG%(I * 4 + 782) <= 100

            Case 7:
                Call UpdateManagerFile(yearNumber$, mgrNbr)

            Case 8:

                Do
                    Locate 12, 46
                    Input "SELECT CHOICE TO DELETE: "; choiceNum
                Loop Until choiceNum >= 1 And choiceNum <= 6

                progMG%(choiceNum * 4 + 779) = -1
                progMG%(choiceNum * 4 + 780) = 0
                progMG%(choiceNum * 4 + 781) = 0
                progMG%(choiceNum * 4 + 782) = 0

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'    ExportManagerProfile Subroutine
'----------------------------------------
'This subroutine exports the current manager
'profile to an output file. If this is a new
'manager profile, it prompts for a name.
Sub ExportManagerProfile (mgrNbr)

    Shared progMG%()

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, mgrNbr, team$)

    If _FileExists(diskPaths$(4) + MGR_FILE_NAME$ + "." + YN$) Then

        Open diskPaths$(4) + MGR_FILE_NAME$ + "." + YN$ For Random As #2 Len = MGR_SIZE_BYTES

        For I = 0 To 999
            Field #2, I * 2 As Q$, 2 As progR$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
        Next

        Get #2, mgrNbr

        For I = 0 To 999
            progMG%(I) = CVI(progR$(I))
        Next

        Close 2

    End If

    If progMG%(0) <> 999 Then

        MsgText$ = "NO MANAGER PROFILE EXISTS FOR THIS TEAM PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        If PP% = 1 Then
            PP% = 0

            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            Call PrintManagerProfile(0, userFile$, T$)
                        End If

                    Case "P"
                        Call PrintManagerProfile(1, "REPORT", T$)

                End Select
                
            End If

        End If

        Call box0 (6, 8, 12, 70, 2, 15, 1)

        Locate 7

        Locate , 11: Input "ENTER NAME FOR PROFILE"; profileFile$

        For X = 1 To 8
            If Mid$(profileFile$, X, 1) = " " Or Mid$(profileFile$, X, 1) = "." Then Mid$(profileFile$, X, 1) = "_"
        Next X

        Open diskPaths$(0) + profileFile$ + "." + "MGR" For Output As #1

        For I = 0 To 999
            Print #1, progMG%(I)
        Next

        Close 1

    End If

End Sub


'----------------------------------------
'    ImportManagerProfile Subroutine
'----------------------------------------
'This subroutine lets the user choose an
'existing manager profile to be loaded
'so it can be modified / tweaked.
Sub ImportManagerProfile (yearNumber$, mgrNbr)

    Shared progMG%()

    managerFile$ = ChooseFile$(diskPaths$(0) + "*.MGR", "MANAGER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        L% = 0

        Call box0 (6, 8, 12, 70, 2, 15, 1)

        Do
            Locate 7, 11
            Print "IMPORT STRATEGY PERCENTAGES ONLY? (Y/N)"
            A$ = GetKeyPress$
        Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        If UCase$(A$) = "Y" Then L% = 1

        Open diskPaths$(0) + managerFile$ + ".MGR" For Input As #1

        If L% = 0 Then
            For I = 0 To 999: Input #1, progMG%(I): Next
        End If

        If L% = 1 Then
            For I = 0 To 580: Input #1, L%: Next
            For I = 581 To 769: Input #1, progMG%(I): Next
            For I = 770 To 802: Input #1, L%: Next
            For I = 803 To 999: Input #1, progMG%(I): Next
        End If

        Close 1

        Call UpdateManagerFile(yearNumber$, mgrNbr)

    End If


End Sub

'----------------------------------------
'       UpdateManagerFile Subroutine
'----------------------------------------
'This subroutine opens the target manager
'file and updates it to include the data
'that has been modified so far.
Sub UpdateManagerFile (teamYear$, mgrNbr)

    Shared progR$()
    Shared progMG%()

    progMG%(0) = 999

    Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = MGR_SIZE_BYTES

    For I = 0 To 999
        Field #2, I * 2 As Q$, 2 As progR$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
    Next

    For I = 0 To 999
        LSet progR$(I) = MKI$(progMG%(I))
    Next

    Put #2, mgrNbr

    Close 2

End Sub


'----------------------------------------
'           PHBATTER Subroutine
'----------------------------------------
'This subroutine helps the user choose a
'pinch hit for the batter.
Sub PHBATTER (yearNumber$, mgrNbr)

    Shared progMG%()

    Do
        'PINCH HIT FOR THE BATTER
        If progMG%(847) <> 999 Then
            progMG%(847) = 999
            For I = 848 To 865: progMG%(I) = 0: Next
        End If

        Cls

        Locate 1, 26
        Color 14, 4: Print "PINCH HITTING FOR THE BATTER"
        Color 15, 0
        Print
        Print "PINCH HIT FOR THE BATTER FROM ";
        Color 15, 1: Print " A ";
        Color 15, 0
        Print " INNING ON"
        Print "IF DOWN BY ";
        Color 15, 1: Print " B ";
        Color 15, 0: Print " OR MORE RUNS AND"
        Color 15, 1: Print " C ";
        Color 15, 0: Print " RUNNERS ON BASE AND"
        Print "SLUGGING PCT IS LESS THAN ";
        Color 15, 1: Print " D ";
        Color 15, 0
        Print "AND BATTING AVERAGE IS LESS THAN ";
        Color 15, 1: Print " E "
        Print " F ";
        Color 15, 0: Print " % OF THE TIME"
        Print
        Print Tab(32);
        Color 14: Print "TABLE OF VALUES"

        Print "SCENARIO        A      B      C      D       E     F"
        Color 15

        For I = 0 To 2
            Print Using "   #           ##     ##      #     ###    ###   ###"; I + 1; progMG%(I * 6 + 848); progMG%(I * 6 + 849); progMG%(I * 6 + 850); progMG%(I * 6 + 851); progMG%(I * 6 + 852); progMG%(I * 6 + 853)
        Next

        Print
        Print "   4 - ACCEPT CHOICES"
        Print

        Do
            Locate 17, 1
            Input "ENTER SELECTION (1-4) "; I
        Loop Until I >= 1 And I <= 4

        If I = 4 Then
            Call UpdateManagerFile(yearNumber$, mgrNbr)
        Else
            I = I - 1
            Input "INNING TO BEGIN PINCH HITTING FOR POSITION PLAYERS "; progMG%(I * 6 + 848)
            Input "DOWN BY HOW MANY RUNS "; progMG%(I * 6 + 849)
            Input "RUNNERS ON BASE "; progMG%(I * 6 + 850)
            Input "SLUGGING PCT LESS THAN "; progMG%(I * 6 + 851)
            Input "BATTING AVERAGE LESS THAN "; progMG%(I * 6 + 852)
            Input "PERCENT OF TIME "; progMG%(I * 6 + 853)
        End If

    Loop Until I = 4

End Sub


'----------------------------------------
'           PHPITCHER Subroutine
'----------------------------------------
'This subroutine helps the user choose a
'pinch hit for the pitcher.
Sub PHPITCHER (yearNumber$, mgrNbr)

    Shared progMG%()

    Do
        'PINCH HIT FOR THE PITCHER
        If progMG%(590) <> 999 Then
            progMG%(590) = 999
            For I = 591 To 608: progMG%(I) = 0: Next
        End If

        Cls

        Locate 1, 26
        Color 14, 4: Print "PINCH HITTING FOR THE PITCHER"
        Color 15, 0
        Print
        Print "PINCH HIT FOR THE PITCHER FROM ";
        Color 15, 1: Print " A ";
        Color 15, 0
        Print " INNING ON"
        Print "IF DOWN BY ";
        Color 15, 1: Print " B ";
        Color 15, 0: Print " OR MORE RUNS AND"
        Color 15, 1: Print " C ";
        Color 15, 0: Print " OR MORE RUNNERS ON BASE AND"
        Color 15, 1: Print " D ";
        Color 15, 0
        Print " OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN ";
        Color 15, 1: Print " E "
        Print " F ";
        Color 15, 0: Print " % OF THE TIME"
        Print

        Print Tab(32);
        Color 14: Print "TABLE OF VALUES"

        Print "SCENARIO        A      B      C      D     E     F"
        Color 15

        For I = 0 To 2
            Print Using "   #           ##     ##      #      #    ###   ###"; I + 1; progMG%(I * 6 + 591); progMG%(I * 6 + 592); progMG%(I * 6 + 593); progMG%(I * 6 + 594); progMG%(I * 6 + 595); progMG%(I * 6 + 596)
        Next

        Print
        Print "   4 - ACCEPT CHOICES"
        Print

        Do
            Locate 17, 1
            Input "ENTER SELECTION (1-4) "; I
        Loop Until I >= 1 And I <= 4

        If I = 4 Then
            Call UpdateManagerFile(yearNumber$, mgrNbr)
        Else
            I = I - 1
            Input "ENTER INNING TO BEGIN PINCH HITTING FOR PITCHER "; progMG%(I * 6 + 591)
            Input "DOWN BY HOW MANY RUNS "; progMG%(I * 6 + 592)
            Input "RUNNERS ON BASE "; progMG%(I * 6 + 593)
            Input "NUMBER OF OUTS OR LESS "; progMG%(I * 6 + 594)
            Input "PITCHER HITS LESS THAN "; progMG%(I * 6 + 595)
            Input "PERCENT OF TIME "; progMG%(I * 6 + 596)
        End If

    Loop Until I = 4

End Sub

'----------------------------------------
'           PITCHAROUND Subroutine
'----------------------------------------
'This subroutine helps the user choose a
'pitch around strategy.
Sub PITCHAROUND (yearNumber$, mgrNbr)

    Shared progMG%()

    Do
        'PITCH AROUND
        Cls

        If progMG%(756) <> 999 Then
            progMG%(756) = 999
            For I = 1 To 9: progMG%(756 + I) = 0: Next
        End If

        Locate 1, 30: Color 14, 2: Print "PITCH AROUND STRATEGY"
        Color 15, 0: Print
        Print "SLUGGING PCT >= ";
        Color 15, 1: Print " A "
        Color 15, 1: Print " B ";
        Color 15, 0: Print " INNING ON"
        Color 15, 1: Print " C ";
        Color 15, 0: Print " % OF THE TIME"

        Print Tab(32);
        Color 14: Print "TABLE OF VALUES"

        Print "SCENARIO                                               A     B      C"
        Color 15
        Print "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; progMG%(757); progMG%(758); progMG%(759)
        Print "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; progMG%(760); progMG%(761); progMG%(762)
        Print "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; progMG%(763); progMG%(764); progMG%(765)
        Print "4 - ACCEPT CHOICES"

        Do
            Locate 22, 1
            Input "ENTER SELECTION (1-4) "; I
        Loop Until I >= 1 And I <= 4

        If I = 4 Then
            Call UpdateManagerFile(yearNumber$, mgrNbr)
        Else
            I = I - 1
            Locate 22, 1: Input "SLUGGING PCT >=           "; progMG%(I * 3 + 757)
            Locate 22, 1: Input "INNING ON                 "; progMG%(I * 3 + 758)
            Locate 22, 1: Input "PERCENT OF TIME           "; progMG%(I * 3 + 759)
        End If

    Loop Until I = 4

End Sub


'----------------------------------------
'       InputPitchStrat Subroutine
'----------------------------------------
'This subroutine gives the user a number of
'options to essentially configure a closing
'strategy.
Sub InputPitchStrat (yearNumber$, mgrNbr)

    Shared progMG%()
    Shared yesNo$()

    Do
        'PITCHING STRATEGY
        Cls

        If progMG%(581) <> 999 Then

            progMG%(581) = 999

            For I = 582 To 587: progMG%(I) = 0: Next

            progMG%(588) = 8
            progMG%(589) = 0

        End If

        If progMG%(588) < 8 Then progMG%(588) = 8
        If progMG%(588) > 9 Then progMG%(588) = 9

        Print Tab(31);: Color 14, 4: Print "CLOSING STRATEGY"
        Color 15, 0
        Print
        Color 12: Print "STRATEGY                                        % OF TIME"
        Color 14: Print "(1) ";
        Color 15: Print "INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; progMG%(588)
        Color 14: Print "(2) ";
        Color 15: Print "USE BOTH SETUP RELIEVER AND CLOSER "; yesNo$(progMG%(589))
        Print
        Color 14: Print "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
        Color 14: Print "(3) ";
        Color 15: Print Using "START INNING IF SAVE OPPORTUNITY               ###"; progMG%(582)
        Color 14: Print "(4) ";
        Color 15: Print Using "START INNING IF AHEAD                          ###"; progMG%(583)
        Color 14: Print "(5) ";
        Color 15: Print Using "START INNING IF TIED OR AHEAD                  ###"; progMG%(584)
        Color 14: Print "(6) ";
        Color 15: Print Using "IF CURRENT PITCHER TIRED AND AHEAD             ###"; progMG%(585)
        Color 14: Print "(7) ";
        Color 15: Print Using "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; progMG%(586)
        Color 14: Print "(8) ";
        Color 15: Print Using "WHEN SAVE OPPORTUNITY OCCURS                   ###"; progMG%(587)
        Color 14: Print "(9) ";
        Color 15: Print "ACCEPT CHOICES"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 9

        If I = 9 Then
            Call UpdateManagerFile(yearNumber$, mgrNbr)
        Else
            If I = 1 Then
                Locate 21, 1: Input "ENTER INNING "; progMG%(588)
            Else
                If I = 2 Then
                    progMG%(589) = 1 - progMG%(589)
                Else
                    Do
                        Locate 21, 1: Input "% OF TIME TO USE "; I1
                    Loop Until I1 >= 0 And I1 <= 100
                    progMG%(I - 2 + 581) = I1
                End If
            End If
        End If

    Loop Until I = 9

End Sub


'----------------------------------------
'           THROWHOME Subroutine
'----------------------------------------
'This subroutine configures the desired
'manager settings for determining when to
'throw home.
Sub THROWHOME (yearNumber$, mgrNbr)

    Shared progMG%()

    Do

        Cls

        If progMG%(766) <> 999 Then
            progMG%(766) = 999
            For I = 1 To 4: progMG%(766 + I) = 0: Next
        End If

        Locate 1, 18
        Color 14, 2: Print "DON'T THROW HOME WITH RUNNER TRYING TO SCORE"
        Color 15, 0
        Print
        Print "FROM ";
        Color 15, 1: Print " A ";
        Color 15, 0: Print " INNING ON"
        Color 15, 1: Print " B ";
        Color 15, 0: Print " OR MORE RUNS AHEAD"

        Print Tab(32);
        Color 14: Print "TABLE OF VALUES"
        Color 15

        Print "      A      B"
        Print Using "(1)  ##    ###"; progMG%(767); progMG%(768)
        Print Using "(2)  ##    ###"; progMG%(769); progMG%(770)
        Print "(3) - ACCEPT CHOICES"

        Do
            Locate 22, 1
            Input "ENTER SELECTION (1-3) "; I
        Loop Until I >= 1 And I <= 3

        If I = 3 Then
            Call UpdateManagerFile(yearNumber$, mgrNbr)
        Else
            I = I - 1

            Locate 22, 1: Input "INNING ON TO USE          "; progMG%(I * 2 + 767)
            Locate 22, 1: Input "AHEAD BY                  "; progMG%(I * 2 + 768)
        End If

    Loop Until I = 3

End Sub

'----------------------------------------
'           BASERUN Subroutine
'----------------------------------------
'This subroutine helps the user choose a
'base running strategy.
Sub BASERUN (teamYear$, mgrNbr)

    Shared progR$()
    Shared progMG%()

    'BASE RUNNING STRATEGY
    Cls

    If progMG%(810) <> 999 Then
        progMG%(810) = 99
        For I = 1 To 36: progMG%(810 + I) = 0: Next
    End If

    Do

        Locate 1, 30
        Color 14, 2: Print "BASE RUNNING STRATEGY"
        Color 15, 0
        Print

        Print Tab(32);
        Color 14: Print "TABLE OF VALUES"

        Print "SCENARIO                            %       %          %          %"
        Color 15
        Print "                                  SAFE    NORMAL   AGGRESSIVE   TOTALS"
        Print " 1 - INNING 1-3, WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(811); progMG%(812); progMG%(813); progMG%(811) + progMG%(812) + progMG%(813)
        Print " 2 - INNING 1-3, WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(814); progMG%(815); progMG%(816); progMG%(816) + progMG%(815) + progMG%(814)
        Print " 3 - INNING 1-3, WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(817); progMG%(818); progMG%(819); progMG%(819) + progMG%(818) + progMG%(817)
        Print " 4 - INNING 4-6, WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(820); progMG%(821); progMG%(822); progMG%(820) + progMG%(821) + progMG%(822)
        Print " 5 - INNING 4-6, WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(823); progMG%(824); progMG%(825); progMG%(823) + progMG%(824) + progMG%(825)
        Print " 6 - INNING 4-6, WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(826); progMG%(827); progMG%(828); progMG%(826) + progMG%(827) + progMG%(828)
        Print " 7 - INNING 7-9, WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(829); progMG%(830); progMG%(831); progMG%(829) + progMG%(830) + progMG%(831)
        Print " 8 - INNING 7-9, WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(832); progMG%(833); progMG%(834); progMG%(832) + progMG%(833) + progMG%(834)
        Print " 9 - INNING 7-9, WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(835); progMG%(836); progMG%(837); progMG%(835) + progMG%(836) + progMG%(837)
        Print "10 - INNING 9- , WHEN AHEAD       "; Using "###     ###        ###          ###"; progMG%(838); progMG%(839); progMG%(840); progMG%(838) + progMG%(839) + progMG%(840)
        Print "11 - INNING 9- , WHEN TIED        "; Using "###     ###        ###          ###"; progMG%(841); progMG%(842); progMG%(843); progMG%(841) + progMG%(842) + progMG%(843)
        Print "12 - INNING 9- , WHEN BEHIND      "; Using "###     ###        ###          ###"; progMG%(844); progMG%(845); progMG%(846); progMG%(844) + progMG%(845) + progMG%(846)
        Print "13 - ACCEPT CHOICES"

        Do
            Locate 22, 1: Input "ENTER SELECTION (1-13) "; I
        Loop Until I >= 1 And I <= 13

        If I = 13 Then

            Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = MGR_SIZE_BYTES

            For J = 0 To 999
                Field #2, J * 2 As Q$, 2 As progR$(J), MGR_SIZE_BYTES - J * 2 - 2 As Q$
            Next

            For J = 0 To 999:
                LSet progR$(J) = MKI$(progMG%(J))
            Next

            Put #2, mgrNbr

            Close 2

        Else

            I = I - 1

            Locate 22, 1: Input "SAFE                   "; progMG%(I * 3 + 811)
            Locate 22, 1: Input "NORMAL                 "; progMG%(I * 3 + 812)
            Locate 22, 1: Input "AGGRESSIVE             "; progMG%(I * 3 + 813)

        End If

    Loop Until I = 13

End Sub


'----------------------------------------
'       GetManagerProfile Subroutine
'----------------------------------------
'This subroutine lets the user choose an
'existing team, then looks for and attempts
'to load the corresponding manager profile.
Sub GetManagerProfile (targetTeam$, yearNumber$, mgrNbr)

    Shared progMG%()

    teamNbr = 0
    Call ChooseTeam(yearNumber$, NTMS, mgrNbr, targetTeam$)

    Cls

    If _FileExists(diskPaths$(4) + MGR_FILE_NAME$ + "." + yearNumber$) Then

        Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = MGR_SIZE_BYTES

        For I = 0 To 999
            Field #2, I * 2 As Q$, 2 As progR$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
        Next

        Get #2, mgrNbr

        For I = 0 To 999
            progMG%(I) = CVI(progR$(I))
        Next

        Close 2

    End If

    If progMG%(0) <> 999 Then

        progMG%(0) = 999

        For I = 1 To 999
            progMG%(I) = -1
        Next

    End If

End Sub
