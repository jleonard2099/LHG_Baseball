Sub PROGMAN ()

    _Title "Full Count Baseball - Programmable Manager"

    DefInt A-Z

    Shared K9&()
    Shared B$(), PT$(), yesNo$()
    Shared MG%()

    yesNo$(0) = "Y"
    yesNo$(1) = "N"

    B$(23) = "PITCHER"
    PT$(1) = "LHP"
    PT$(0) = "RHP"

    ReDim menuChoices$(1 To 15)

    Cls
    Print Tab(32);: Color 14, 4: Print "COMPUTER MANAGER": Color 15, 0
    Print
    Print
    Print "     THIS PROGRAM ALLOWS YOU TO CUSTOMIZE THE COMPUTER MANAGER'S"
    Print
    Print "     STRATEGY AND PLAYER USAGE FOR EACH INDIVIDUAL TEAM         "
    Print
    Print
    Print "                    PRESS ANY KEY TO CONTINUE                   "
    I$ = GetKeyPress$

    Call GetTeam_PROGMAN(T$, YN$)

    Do

        '45
        Color 15, 1

        Cls

        Print
        Print Tab(23);
        Color 14, 4: Print "FULL COUNT PROGRAMMABLE MANAGER"
        Print
        ReDim menuChoices$(1 To 14)
        menuChoices$(1) = "SELECT NEW TEAM TO EDIT                                  "
        menuChoices$(2) = "SELECT/CHANGE LINEUPS                                    "
        menuChoices$(3) = "SELECT/CHANGE ROTATIONS/RELIEVERS                        "
        menuChoices$(4) = "SELECT/CHANGE PINCH HITTERS                              "
        menuChoices$(5) = "SELECT/CHANGE DEFENSIVE REPLACEMENTS (8TH AND 9TH INNING)"
        menuChoices$(6) = "SELECT/CHANGE CLOSING STRATEGY                           "
        menuChoices$(7) = "SELECT/CHANGE HITTING STRATEGY                           "
        menuChoices$(8) = "SELECT/CHANGE DEFENSIVE STRATEGY                         "
        menuChoices$(9) = "SELECT/CHANGE PINCH RUNNING STRATEGY                     "
        menuChoices$(10) = "SELECT/CHANGE BASE RUNNING STRATEGY                      "
        menuChoices$(11) = "IMPORT MANAGERIAL PROFILE                                "
        menuChoices$(12) = "EXPORT MANAGERIAL PROFILE                                "
        menuChoices$(13) = "PRINT MANAGERIAL PROFILE                                 "
        menuChoices$(14) = "RETURN TO GAME MENU                                      "
        Selection% = 1
        Count% = 14
        Start% = 1
        ExitCode% = 14
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 10
        Call box0(4, 9, 19, 70, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1
                    For I = 1 To 999: MG%(I) = -1: Next
                    Call GetTeam_PROGMAN(T$, YN$)

                Case 2
                    Call LineupStrategy(T$)

                Case 3
                    Call RotationReliefMenu(T$)

                Case 4
                    Call SelectPinchHitters(YN$, T$)

                Case 5
                    Call DefenseLateInning(YN$, T$)

                Case 6
                    Call Input_PITSTRAT(YN$)

                Case 7
                    Call HittingStrategy(YN$)

                Case 8
                    Call DefensiveStrategy(YN$)

                Case 9
                    Call PinchRunStrategy(T$)

                Case 10
                    Call BASERUN(YN$)

                Case 11
                    Call ImportManagerProfile

                Case 12
                    Call ExportManagerProfile

                Case 13
                    Call PrintManagerProfile(T$)

                Case 14
                    Cls '
            End Select
        End If

    Loop Until Selection% = 14 Or ExitCode% = 27

End Sub

'----------------------------------------
'    RotationReliefMenu Subroutine
'----------------------------------------
'...explanation...
Sub RotationReliefMenu (targetTeam$)

    Do

        Cls

        Print Tab(22);
        Color 14, 4: Print "PITCHING ROTATION/RELIEVER MANAGEMENT"
        Print
        Print
        Color 15, 0
        ReDim menuChoices$(1 To 6)
        menuChoices$(1) = "INPUT A NEW STARTING PITCHING ROTATION     "
        menuChoices$(2) = "EDIT AN EXISTING STARTING PITCHING ROTATION"
        menuChoices$(3) = "SELECT/EDIT MIDDLE RELIEVERS               "
        menuChoices$(4) = "SELECT/EDIT SET-UP MEN                     "
        menuChoices$(5) = "SELECT/EDIT CLOSERS                        "
        menuChoices$(6) = "RETURN TO MAIN MENU                        "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 19
        Call box0(4, 18, 11, 62, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1, 2: Call PitchingRotations(Selection%, targetTeam$)
                Case 3: Call SetupMiddleRelievers(targetTeam$, yearNumber$)
                Case 4: Call SetupRelievers(targetTeam$, yearNumber$)
                Case 5: Call SetupClosers(targetTeam$, yearNumber$)
                Case 6: '
            End Select
        Else
            Selection% = 6
        End If

    Loop Until Selection% = 6

End Sub

'----------------------------------------
'       GetTeam_PROGMAN Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_PROGMAN (targetTeam$, yearNumber$)

    Shared MG%()
    Shared mgr_idx%

    Cls

    Call GetDiskYear(targetTeam$, yearNumber$, teamIdx)
    mgr_idx% = teamIdx

    Cls

    If _FileExists(diskPaths$(3) + "MGR." + yearNumber$) Then

        Open diskPaths$(3) + "MGR." + yearNumber$ For Random As #2 Len = 2000

        For I = 0 To 999
            Field #2, I * 2 As Q$, 2 As R$(I), 2000 - I * 2 - 2 As Q$
        Next

        Get #2, mgr_idx%

        For I = 0 To 999
            MG%(I) = CVI(R$(I))
        Next

        Close 2

    End If

    If MG%(0) <> 999 Then
        MG%(0) = 999
        For I = 1 To 999
            MG%(I) = -1
        Next
    End If

End Sub

'----------------------------------------
'       GetDiskYear Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskYear (targetTeam$, yearNumber$, teamidx)

    Shared L%()

    Do

        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
		
		Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$
		If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_PROGMAN(numberTeams, yearNumber$, teamidx)
        Else
            yearNumber$ = NY$
            Erase teamNames$

            'Erase teamIndex% ?
            'For I = 1 To 500: teamIndex%(I) = 1: Next

            numberTeams = 0

            Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342
            Y = LOF(1)
            Field #1, 15 As Q$(0)
            For I1 = 1 To Y / 4342
                Get #1, I1
                For I2 = 1 To 15
                    If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                Next I2
                If nullTeam <> 1 Then
                    numberTeams = numberTeams + 1
                    teamNames$(numberTeams) = Q$(0)
                    teamIndex%(numberTeams) = I1
                End If
            1105 Next I1
            Close 1

            Call SelectTeam_PROGMAN(numberTeams, yearNumber$, teamidx)

        End If

        targetTeam$ = RTrim$(Q$(0))

        If BO% = 1 Then
            BO% = 0
        Else

            Locate 2, 33
            Color L%(11), L%(12): Print targetTeam$

			Color 15, 4

			Do
				Locate , 33: Print "ANY CHANGE (YN)";
				I$ = GetKeyPress$
			Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

End Sub

'----------------------------------------
'      SelectTeam_PROGMAN Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_PROGMAN (numberTeams, yearNumber$, T)

    Shared K9&()
    Shared L%(), progB%(), P%()
    Shared B$(), P$()

    Dim PB$(23)

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
    Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then
        T = teamIndex%(Selection%)
        Locate 2, 33: Color 31: Print "LOADING..."

        If yearNumber$ <> "" And _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
            Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

            For I = 0 To 22
                Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
                Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
                Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
                Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
            Next

            For I = 0 To 21
                Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015), 2020 - 37 - I * 89 As X$
                Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027), 2020 - 61 - I * 89 As X$
                Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039), 2020 - 85 - I * 89 As X$
                Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
            Next

            Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

            Get #1, T

            For I = 1 To 13: L%(I) = CVI(Q$(I)): Next

            K9&(0) = CVS(Q$(14))

            For I = 0 To 22
                B$(I) = RTrim$(Q$(I * 43 + 15))
                For I1 = 0 To 41: progB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
                    If progB%(I, 0) = -1 Then PB$(I) = "L"
                    If progB%(I, 0) = 0 Then PB$(I) = "S"
                    If progB%(I, 0) = 1 Then PB$(I) = "R"
                    If progB%(I, 0) = 2 Then PB$(I) = "RP"
                    If progB%(I, 0) = -2 Then PB$(I) = "LP"
                Next
            Next

            For I = 0 To 21:
                P$(I) = RTrim$(Q$(I * 38 + 1004))
                For I1 = 0 To 36
                    P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
                Next
            Next

            S$ = RTrim$(Q$(1840))
            M$ = RTrim$(Q$(1841))
            NN$ = RTrim$(Q$(1842))
            CK = CVI(Q$(1843))
            Close 1
        Else
            BO% = 1
        End If
    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'       LineupStrategy Subroutine
'----------------------------------------
'...explanation...
Sub LineupStrategy (targetTeam$)

    Do

        Cls
        Print Tab(26);
        Color 14, 4: Print "LINEUP STRATEGY OPTIONS"
        Print
        Print
        Color 15, 0
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "SELECT/CHANGE/DISPLAY LINEUPS VS. RHP                  "
        menuChoices$(2) = "SELECT/CHANGE/DISPLAY LINEUPS VS. LHP                  "
        menuChoices$(3) = "ANALYZE PLAYER USAGE ACCORDING TO YOUR LINEUPS SELECTED"
        menuChoices$(4) = "RETURN TO TOP MENU                                     "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 13
        Call box0(4, 12, 9, 68, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        '0 = right hand
        '1 = left hand
        handRL% = Selection% - 1

        Select Case Selection%
            Case 1, 2:
                Call EditLineups(handRL%, targetTeam$, yearNumber$)

            Case 3:
                Call AnalyzePlayerUsage(targetTeam$)

            Case 4:
                Cls '

        End Select

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub

'----------------------------------------
'       AnalyzePlayerUsage Subroutine
'----------------------------------------
'...explanation...
Sub AnalyzePlayerUsage (targetTeam$)

    Shared AB%(), L%(), MG%(), progB%()
    Shared B$(), B1$(), mgrX$()

    Dim FRAC!

    'ANALYZE PLAYER USAGE
    LH% = 0
    RH% = 0

    For I = 0 To 22
        For I1 = 42 To 44
            progB%(I, I1) = 0
        Next
    Next

    For I = 10 To 19
        If MG%(120 + I * 21) = 999 Then
            LH% = LH% + MG%(100 + I * 21)
        End If
    Next

    For I = 0 To 9
        If MG%(120 + I * 21) = 999 Then
            RH% = RH% + MG%(100 + I * 21)
        End If
    Next

    For I = 0 To 9
        If MG%(120 + I * 21) = 999 Then
            For I1 = 1 To 9
                If MG%(101 + I1 + I * 21) <> 1 Then
                    FRAC! = MG%(100 + I * 21) / RH%
                    progB%(MG%(110 + I1 + I * 21), 42) = progB%(MG%(110 + I1 + I * 21), 42) + (AB%(I1) * FRAC!) * .7
                    progB%(MG%(110 + I1 + I * 21), 44) = progB%(MG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC!) * .7
                End If
            Next I1
        End If
    Next I

    For I = 10 To 19
        If MG%(120 + I * 21) = 999 Then
            For I1 = 1 To 9
                If MG%(101 + I1 + I * 21) <> 1 Then
                    FRAC! = MG%(100 + I * 21) / LH%
                    progB%(MG%(110 + I1 + I * 21), 43) = progB%(MG%(110 + I1 + I * 21), 43) + (AB%(I1) * FRAC!) * .3
                    progB%(MG%(110 + I1 + I * 21), 44) = progB%(MG%(110 + I1 + I * 21), 44) + (AB%(I1) * FRAC!) * .3
                End If
            Next I1
        End If
    Next I

    Cls
    Color 14
    Print "# ";
    Color L%(11), L%(12): Print targetTeam$;
    Color 14, 0
    Locate , 18: Print "B  POS  ACTUAL AB  PROJECTED: AB/LHP  AB/RHP  TOT/AB"
    Locate 2
    I1 = 0

    For I = 0 To 22:

        Locate , 1

        If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then
            I1 = I1 + 1
        Else
            Color 2, 0
            Locate I + 2 - I1, 1
            If I < 10 Then
                Print Using "#"; I;
                Color 15, 0
                If progB%(I, 21) >= 98 Then Color 0, 7
            End If

            If I > 9 Then
                Print mgrX$(I - 10);
                Color 15, 0
                If progB%(I, 21) >= 98 Then Color 0, 7
            End If

            Locate , 3: Print B$(I);: Color 15, 0: Locate , 18: Print B1$(progB%(I, 0) + 2);
            Locate , 21: If progB%(I, 22) <> -1 Then Print Using "#"; progB%(I, 22);
            Locate , 22: If progB%(I, 23) <> -1 Then Print Using "#"; progB%(I, 23);
            Locate , 23: If progB%(I, 24) <> -1 Then Print Using "#"; progB%(I, 24);
            Locate , 24: If progB%(I, 25) <> -1 Then Print Using "#"; progB%(I, 25);
            Locate , 27

            Print Using "   ###                ###     ###      ###"; progB%(I, 4); progB%(I, 43); progB%(I, 42); progB%(I, 44);

        End If

    Next

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'     SelectPinchHitters Subroutine
'----------------------------------------
'...explanation...
Sub SelectPinchHitters (yearNumber$, targetTeam$)

    Shared B$(), B1$(), mgrX$()
    Shared B%(), L%(), MG%()

    Do
        'SELECT PINCH HITTERS
        If MG%(540) <> 999 Then
            MG%(540) = 999
            For I = 1 To 10
                MG%(I + 540) = -1: MG%(I + 550) = 0
            Next
        End If

        Cls

        Locate 1, 1
        Print "# ";: Color L%(11), L%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "B   AB  HR  RBI   AVG"
        I2 = 0

        For I = 0 To 22

            If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then
                I2 = I2 + 1
            Else

                Locate I + 2 - I2, 1
                Color 2, 0

                If I <= 9 Then Print Using "#"; I;: Color 15, 0
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0

                For I1 = 1 To 10
                    If I = MG%(I1 + 540) Then Color L%(11), L%(12)
                Next

                Locate , 3: Print B$(I);
                Color 15, 0
                Locate , 16: Print B1$(B%(I, 0) + 2);
                Locate , 17: Print Using "  ###  ##  ###"; B%(I, 4); B%(I, 9); B%(I, 10);
                If B%(I, 4) <> 0 Then Print Using "  .###"; B%(I, 6) / B%(I, 4);
                If B%(I, 4) = 0 Then Print "  .---";
            End If
        Next I

        Locate 1, 48: Print "  PINCH HITTERS        "
        Locate , 48: Print "  ---- VS LHP  ----"

        For I = 1 To 5
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 540) < 0 Then
                Print
            Else
                Print B$(MG%(I + 540))
            End If
        Next

        Locate , 48: Print "  ---- VS RHP  ----"

        For I = 6 To 10
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 540) < 0 Then
                Print
            Else
                Print B$(MG%(I + 540))
            End If
        Next

        Locate 15, 48: Print "(11) ACCEPT CHOICES"
        Locate 16, 48: Print "(12) DELETE A HITTER"

        Do
            Locate 17, 48: Input "ENTER SELECTION (1-12)"; I
        Loop Until I >= 1 And I <= 12

        Select Case I
            Case 1 TO 10
                '220
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
                MG%(I + 540) = I1

                Call SelectPinchHitters(yearNumber$, targetTeam$)

            Case 11:
                Call UpdateManagerFile(yearNumber$)

            Case 12:
                Do
                    Locate 16, 48: Input "SELECT HITTER TO DELETE: "; I
                Loop Until I >= 1 And I <= 10

                MG%(I + 540) = -1: MG%(I + 550) = 0

        End Select

    Loop Until I = 11

End Sub

'----------------------------------------
'    DefenseLateInning Subroutine
'----------------------------------------
'...explanation...
Sub DefenseLateInning (yearNumber$, T$)

    Shared B$(), mgrX$()
    Shared L%(), MG%(), progB%()

    Do
        'DEFENSIVE REPLACEMENTS
        Cls
        Print Tab(26);: Color 14, 4
        Print "DEFENSIVE REPLACEMENT 8TH AND 9TH INNING STRATEGY OPTIONS"
        Print
        Print
        Color , 0

        If MG%(560) <> 999 Then
            MG%(560) = 999
            For I = 1 To 4
                MG%(I + 560) = -1
                MG%(I + 564) = 0
                MG%(I + 568) = 0
                MG%(I + 572) = 0
                MG%(I + 576) = 0
            Next
        End If

        Cls
        Locate 1, 1
        Print "#";: Color L%(11), L%(12): Print T$;
        Locate , 17: Print "1 FAVG 2 FAVG 3 FAVG 4 FAVG"
        I1 = 0

        For I = 0 To 22:
            Locate , 1
            If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then
                I1 = I1 + 1
            Else
                Locate I + 2 - I1, 1
                Color 2, 0
                If I < 10 Then Print Using "#"; I;: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7
                Locate , 3: Print B$(I);: Color 15, 0
                Locate , 17: If progB%(I, 22) <> -1 Then Print Using "# ####"; progB%(I, 22); progB%(I, 26);
                Locate , 24: If progB%(I, 23) <> -1 Then Print Using "# ####"; progB%(I, 23); progB%(I, 27);
                Locate , 31: If progB%(I, 24) <> -1 Then Print Using "# ####"; progB%(I, 24); progB%(I, 28);
                Locate , 38: If progB%(I, 25) <> -1 Then Print Using "# ####"; progB%(I, 25); progB%(I, 29);
            End If
        Next

        Locate 1, 46
        Print "# PLAYER     P SUBSTITUTE  %  RA"
        For I = 1 To 4
            Locate , 46
            Print Using "# "; I;
            If MG%(I + 560) < 0 Then
                Print
            Else
                Print Left$(B$(MG%(I + 560)), 10); Tab(59);
                Print Using "# "; MG%(I + 576);
                Print Left$(B$(MG%(I + 564)), 10); Tab(72); Using "### #"; MG%(I + 568); MG%(I + 572)
            End If
        Next

        Locate 7, 46: Print "(5) ACCEPT CHOICES"
        Locate 8, 46: Print "(6) DELETE A CHOICE"

        Do
            Locate 9, 46: Input "ENTER SELECTION (1-6)"; I
        Loop Until I >= 1 And I <= 6

        Select Case I
            Case 1 TO 4:
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
                MG%(I + 560) = I1

                Locate 11, 46: Print "POSITION (2-9) ";
                Do
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 2 And I1 <= 9

                MG%(I + 576) = I1

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
                    MG%(I + 564) = I1
                Loop Until I1 >= 1 And I1 <= 22

                Do
                    Locate 13, 46: Input "ENTER % OF TIME TO USE "; MG%(I + 568)
                Loop Until MG%(I + 568) >= 0 And MG%(I + 568) <= 100

                Locate 14, 46: Print "NUMBER OF RUNS AHEAD"
                Locate 15, 46: Input "0,1,2 OR MORE "; I1
                If I1 < 0 Then I1 = 0
                If I1 > 2 Then I1 = 2
                MG%(I + 572) = I1

            Case 5:
                Call UpdateManagerFile(yearNumber$)

            Case 6:
                Do
                    Locate 10, 46: Input "SELECT CHOICE TO DELETE: "; I
                Loop Until I >= 1 And I <= 4
                MG%(I + 560) = -1
                MG%(I + 564) = 0
                MG%(I + 568) = 0
                MG%(I + 572) = 0
                MG%(I + 576) = 0

        End Select

    Loop Until I = 5

End Sub

'----------------------------------------
'       EditLineups Subroutine
'----------------------------------------
'...explanation...
Sub EditLineups (handRL%, targetTeam$, yearNumber$)

    Shared L%(), MG%()
    Shared PT$()
    Shared B3%(), B7%(), B9%()

    Dim V1%(1)

    Do

        Cls
        Locate 3, 1
        Color L%(11), L%(12): Print targetTeam$;
        Color 15, 0
        Print " LINEUPS VS "; PT$(handRL%)
        Print
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "INPUT A NEW LINEUP            "
        menuChoices$(2) = "EDIT AN EXISTING LINEUP       "
        menuChoices$(3) = "DELETE AN EXISTING LINEUP     "
        menuChoices$(4) = "RETURN TO LINEUP STRATEGY MENU"
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 13
        Call box0(4, 12, 9, 43, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 And Selection% <> 4 Then

            lineupChoice% = Selection%

            Color , 0

            Do

                'Edit / Delete
                If lineupChoice% = 2 Or lineupChoice% = 3 Then
                    'Get a lineup
                    Call GetLineup(handRL%, targetTeam$)

                    LF% = 0

                    For I = 0 To 9
                        If MG%(120 + (I + handRL% * 10) * 21) = 999 Then LF% = LF% + 1
                    Next

                    If LF% = 0 Then
                        Locate 23, 1
                        Color 12: Print "NO LINEUPS FOUND! HIT ANY KEY TO RETURN TO MENU"
                        Color 15
                        I$ = GetKeyPress$
                        Exit Do
                    End If

                End If

                'Input New
                If lineupChoice% = 1 Then
                    For I = 1 To 9: B3%(I) = -1: B7%(I) = -1: Next

                    D% = 0

                    Do
                        Locate 12, 1
						Print "IS A DH TO BE USED IN THIS LINEUP (YN)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then D% = 1

                    'We're not "searching" for a lineup but we need to choose one
                    LF% = 1

                End If

                Locate 23, 1
                Print "CHOOSE LINEUP NUMBER (0-9) OR <ESC> TO EXIT ";
                Do
                    I$ = GetKeyPress$
                    LN% = Val(I$)
                Loop Until LN% >= 0 And LN% <= 9 Or I$ = Chr$(27)

            Loop Until lineupChoice% = 2 Or LF% > 0 'MG%(120 + (LN% + handRL% * 10) * 21) = 999

            '0 = INPUT A NEW LINEUP
            '1 = EDIT AN EXISTING LINEUP
            '2 = DELETE AN EXISTING LINEUP
            If I$ <> Chr$(27) And LF% <> 0 Then

                If handRL% = 1 Then LN% = LN% + 10

                If lineupChoice% = 3 Then
                    MG%(120 + LN% * 21) = 0
                    MG%(100 + LN% * 21) = 0
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

    Loop Until Selection% = 4 Or ExitCode% = 27

    Color 15, 0

End Sub

'----------------------------------------
'       ViewLineup Subroutine
'----------------------------------------
'...explanation...
Sub ViewLineup (targetTeam$)

    Shared B$(), B1$(), mgrX$()
    Shared L%(), progB%()

    Cls
    Color 14: Print "#";
    Color L%(11), L%(12): Print targetTeam$;
    Color 14, 0
    Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"
    Locate 2

    For I = 0 To 22
        Locate , 1

        If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then
            progB%(I, 21) = 98
        Else
            Color 2, 0
            If I < 10 Then Print Using "#"; I;: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7
            If I > 9 Then Print mgrX$(I - 10);: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7

            Locate , 3: Print B$(I);: Color 15, 0: Locate , 15: Print B1$(progB%(I, 0) + 2);
            Locate , 18: If progB%(I, 22) <> -1 Then Print Using "#"; progB%(I, 22);
            Locate , 19: If progB%(I, 23) <> -1 Then Print Using "#"; progB%(I, 23);
            Locate , 20: If progB%(I, 24) <> -1 Then Print Using "#"; progB%(I, 24);
            Locate , 21: If progB%(I, 25) <> -1 Then Print Using "#"; progB%(I, 25);
            Locate , 23

            Print Using "### ## ## ## ### ###"; progB%(I, 4); progB%(I, 7); progB%(I, 8); progB%(I, 9); progB%(I, 11); progB%(I, 12);

            Locate , 44: Print Using "#"; progB%(I, 16);: Print Using "#"; progB%(I, 34);: Print Using "#"; progB%(I, 33);
            Locate , 47

            Print Using "### ##"; progB%(I, 13); progB%(I, 18);

            If progB%(I, 6) = progB%(I, 4) And progB%(I, 6) <> 0 Then
                Print "1.000";
            Else
                If progB%(I, 4) = 0 Then
                    Print " .---";
                Else
                    Print Using " .###"; progB%(I, 6) / progB%(I, 4);
                End If
            End If

            If I <> 22 Then Print
        End If

    Next I

End Sub

'----------------------------------------
'       InputDeleteLineup Subroutine
'----------------------------------------
'...explanation...
Sub InputDeleteLineup (D%, yearNumber$)

    Shared B$(), C$()

    Shared B3%(), B7%(), B9%()
    Shared L%(), M%(), progB%()

    Do
        540 '
        For I = 0 To 9: M%(I) = 0: Next I

        Locate 25, 1
        For I1 = 0 To 9
            If D% <> 0 Or I1 <> 0 Then
                If D% <> 1 Or I1 <> 1 Then
                    If M%(I1) = 0 Then Print I1; " ";
                    If M%(I1) = 1 Then Print "  ";
                End If
            End If
        Next I1

        550 '
        For I = 1 To 9

            '--- work out conditions here for the loop

            Do

                Call GetLineupPosition(I1, I$)

                If UCase$(I$) <> "X" Then '540

                    '--- We never assign a value to I
                    For J = 1 To 9
                        If J <> I And B3%(J) <> I1 Or B7%(J) <> 1 Then
                            If B3%(J) = I1 And UCase$(I$) <> "P" Then Call GetLineupPosition(I1, I$)
                        End If
                    Next J

                    If D% = 1 And UCase$(I$) = "P" Then Call GetLineupPosition(I1, I$)

                    If progB%(I1, 21) <> 0 And UCase$(I$) <> "P" Then Call GetLineupPosition(I1, I$)

                    If UCase$(I$) = "P" Then
                        Print Using "# "; I;

                        For J = 1 To 9
                            If J = I Then
                                If B3%(J) = P1% And B7%(J) = 1 Then Call GetLineupPosition(I1, I$)
                            End If
                        Next J

                        B3%(I) = P1%
                        Print B$(23);: M%(Val(I$)) = 1

                        Locate , 72

                        Print "  p ";
                        B7%(I) = 1
                        B9%(1) = P1%
                        M%(1) = 1
                        Locate 25, 1
                        For I1 = 0 To 9
                            If D% <> 0 Or I1 <> 0 Then
                                If D% <> 1 Or I1 <> 1 Then
                                    If M%(I1) = 0 Then Print I1; " ";
                                    If M%(I1) = 1 Then Print "  ";
                                End If
                            End If
                        Next I1

                    End If

                    B3%(I) = I1
                    Print Using "# "; I;: Print B$(B3%(I));

                    Locate , 74

                    Do
                        I$ = GetKeyPress$
                    Loop Until Val(I$) <> 1

                    For J = 1 To 9:
                        If J <> I Then
                            If Val(I$) = B7%(J) Then
                                Do
                                    I$ = GetKeyPress$
                                Loop Until Val(I$) <> 1
                            End If
                        End If
                    Next J

                    If D% = 0 And Val(I$) = 0 Or UCase$(I$) = "X" Then Call GetLineupPosition(I1, I$)

                    lu_idx = Val(I$)
                    Print C$(lu_idx);
                    B7%(I) = lu_idx

                    B9%(B7%(I)) = B3%(I)
                    M%(Val(I$)) = 1

                    Locate I1 + 2, 2: Print "*";: Color L%(11), L%(12)
                    Print B$(I1): Color 15, 0

                    Locate 25, 1
                    For I1 = 0 To 9
                        If D% <> 0 Or I1 <> 0 Then
                            If D% <> 1 Or I1 <> 1 Then
                                If M%(I1) = 0 Then Print I1; " ";
                                If M%(I1) = 1 Then Print "  ";
                            End If
                        End If
                    Next I1

                    If C1 = 1 Then Exit For
                Else
                    ' "X" was pressed
                    Exit For

                End If

            Loop Until I1 >= 0 And I1 <= 22 Or UCase$(I$) = "X"

        Next I

        If UCase$(I$) <> "X" Then
            Call ChangeLineup_PROGMAN(yearNumber$, J$)
        End If

    Loop Until UCase$(I$) = "X" Or J$ = "0"

End Sub

'----------------------------------------
'       GetLineupPosition Subroutine
'----------------------------------------
'...explanation...
Sub GetLineupPosition (I1, I$)

    560 '
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
'        ChangeLineup_PROGMAN Subroutine
'----------------------------------------
'...explanation...
Sub ChangeLineup_PROGMAN (yearNumber$, I$)

    Shared B$(), C$()
    Shared B3%(), B7%(), B9%()
    Shared M%(), MG%(), progB%()

    Do
        C1 = 1
        Locate 25, 1: Color 14
        Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
        Color 15, 0
        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "R":
                For J = 1 To 9: M%(J) = 0: B7%(J) = -1: B3%(J) = -1: Next
                C1 = 0
                For J = 0 To 22: progB%(J, 21) = 0: Next
                RP = 1
                For J = 0 To 22
                    If progB%(J, 31) <> 0 Then progB%(J, 21) = 99
                Next
                'GoTo 910

            Case "L":
                Locate 18, 59: Print "B.O. PLYR 1 (1-9)";
                Do
                    I$ = GetKeyPress$
                    K1 = Val(I$)
                Loop Until K1 >= 1 And K1 <= 9

                'IF B7%(K1)=1 THEN 1441
                Locate 19, 59: Print "B.O. PLYR 2 (1-9)";
                Do
                    I$ = GetKeyPress$
                    K2 = Val(I$)
                Loop Until K2 >= 1 And K2 <= 9

                'IF B7%(K2)=1 THEN 1442
                Swap B7%(K2), B7%(K1)
                Swap B3%(K1), B3%(K2)
                B9%(B7%(K1)) = B3%(K1)
                B9%(B7%(K2)) = B3%(K2)

                For I = 1 To 9
                    Locate I + 7, 59
                    Print Using "# "; I;

                    If B7%(I) = 1 And I5 <> 66 Then Print B$(23);
                    If B7%(I) <> 1 Then Print B$(B3%(I));

                    Print Tab(74);
                    Print C$(B7%(I));
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

                Loop Until B7%(K1) <> 1

                Do
                    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";
                    Do
                        I$ = GetKeyPress$
                        K2 = Val(I$)
                    Loop Until K2 >= 1 And K2 <= 9

                Loop Until B7%(K2) <> 1

                Swap B7%(K2), B7%(K1)
                B9%(B7%(K1)) = B3%(K1)
                B9%(B7%(K2)) = B3%(K2)

                For I = 1 To 9

                    Locate I + 7, 59

                    Print Using "# "; I;

                    If B7%(I) = 1 And I5 <> 66 Then Print B$(23);
                    If B7%(I) <> 1 Then Print B$(B3%(I));

                    Print Tab(74);
                    Print C$(B7%(I));

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
                MG%(100 + LN% * 21) = LU%
                MG%(101 + LN% * 21) = D%

                For I = 1 To 9
                    MG%(101 + I + LN% * 21) = B7%(I)
                    MG%(110 + I + LN% * 21) = B3%(I)
                Next

                MG%(120 + LN% * 21) = 999
                MG%(0) = 999

                Call UpdateManagerFile(yearNumber$)

                For I = 1 To 9
                    If B7%(I) = 1 Then
                        M%(I) = 0
                    End If
                Next I

                M%(0) = 0

                C1 = 0

                For I = 0 To 9
                    B3%(I) = -1
                    B7%(I) = -1
                Next

            Case 1 TO 9
                '--- What is B4%? No sign of it elsewhere so it's 0
                Locate B3%(I) + 2 - B4%, 2: Print " "; B$(B3%(I))
                'GoTo 560
        End Select

    Loop Until I >= 0 'And I <= 9

End Sub

'----------------------------------------
'          EditLineup Subroutine
'----------------------------------------
'...explanation...
Sub EditOneLineup (targetTeam$, yearNumber$)

    Shared B$(), C$()
    Shared B3%(), B7%(), B9%()
    Shared M%(), progB%()

    Call ViewLineup(targetTeam$)

    For I = 10 To 22: Locate I, 59: Print Space$(21);: Next

    LU% = MG%(100 + LN% * 21)
    D% = MG%(101 + LN% * 21)

    For I = 1 To 9
        B3%(I) = MG%(110 + I + LN% * 21)
        B7%(I) = MG%(101 + I + LN% * 21)
    Next

    For I = 1 To 9: B9%(B7%(I)) = B3%(I)

        If B7%(I) = 1 Then
            B3%(I) = P1%: B9%(1) = P1%
        Else
            progB%(B3%(I), 21) = 0
        End If

    Next I

    For I = 10 To 22: Locate I, 59: Print Space$(21);: Next

    For I = 1 To 9

        Locate I + 7, 59

        Print Using "# "; I;

        If B7%(I) = 1 And I5 <> 66 Then Print B$(23);
        If B7%(I) <> 1 Then Print B$(B3%(I));

        Print Tab(74);
        Print C$(B7%(I));

    Next I

    I5 = 0

    For I2 = 0 To 22
        Locate I2 + 2, 2
        For I3 = 1 To 9
            If B3%(I3) = I2 And B7%(I3) <> 1 Then
                Print "*";: Color L%(11), L%(12): Print B$(I2)
                Color 15, 0
            End If
        Next I3
    Next I2

    For I3 = 0 To 9: M%(I3) = 1: Next I3

    Call ChangeLineup_PROGMAN(yearNumber$, J$)

End Sub

'----------------------------------------
'         GetLineup Subroutine
'----------------------------------------
'...explanation...
Sub GetLineup (handRL%, targetTeam$)

    Shared B$(), C$(), PT$()
    Shared MG%()

    Cls
    Color 12: Print Tab(30); "LINEUPS FOUND VS. "; PT$(handRL%)
    Color 14
    Print " #0    "; MG%(100 + (0 + handRL% * 10) * 21); "%";
    Locate , 17: Print " #1    "; MG%(100 + (1 + handRL% * 10) * 21); "%";
    Locate , 33: Print " #2    "; MG%(100 + (2 + handRL% * 10) * 21); "%";
    Locate , 49: Print " #3    "; MG%(100 + (3 + handRL% * 10) * 21); "%";
    Locate , 65: Print " #4    "; MG%(100 + (4 + handRL% * 10) * 21); "%"
    Color 15

    For I = 0 To 4
        For I1 = 1 To 9

            If MG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 3, 16 * I + 1
                Color 2: Print "NONE FOUND"
                Color 15

            Else

                If MG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 2 + I1, 16 * I + 1: Print "         "
                Else

                    Locate 2 + I1, 16 * I + 1

                    If MG%(101 + I1 + (I + handRL% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(B$(MG%(110 + I1 + (I + handRL% * 10) * 21)), 10);
                    End If

                    Locate 2 + I1, 16 * I + 12
                    Print C$(MG%(101 + I1 + (I + handRL% * 10) * 21))

                End If

            End If 'Done checking for 999 value

        Next

    Next

    Print
    Color 14: Print " #5    "; MG%(100 + (5 + handRL% * 10) * 21); "%";
    Locate , 17: Print " #6    "; MG%(100 + (6 + handRL% * 10) * 21); "%";
    Locate , 33: Print " #7    "; MG%(100 + (7 + handRL% * 10) * 21); "%";
    Locate , 49: Print " #8    "; MG%(100 + (8 + handRL% * 10) * 21); "%";
    Locate , 65: Print " #9    "; MG%(100 + (9 + handRL% * 10) * 21); "%"
    Color 15

    For I = 5 To 9
        For I1 = 1 To 9
            If MG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 14, 16 * (I - 5) + 1
                Color 2: Print "NONE FOUND"
                Color 15
            Else
                If MG%(120 + (I + handRL% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 13 + I1, 16 * (I - 5) + 1
                    Print "         "
                Else
                    Locate 13 + I1, 16 * (I - 5) + 1
                    If MG%(101 + I1 + (I + handRL% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(B$(MG%(110 + I1 + (I + handRL% * 10) * 21)), 10);
                    End If
                    Locate 13 + I1, 16 * (I - 5) + 12
                    Print C$(MG%(101 + I1 + (I + handRL% * 10) * 21))
                End If
            End If
        Next I1
    Next I

End Sub

'----------------------------------------
'     PrintManagerProfile Subroutine
'----------------------------------------
'...explanation...
Sub PrintManagerProfile (targetTeam$)

    Shared B$(), P$(), yesNo$()
    Shared MG%()

    ' PRINT MANAGER PROFILE
    LPrint "MANAGERIAL PROFILE FOR "; targetTeam$
    LPrint

    '0 = right hand
    '1 = left hand
    handRL% = 0
    LF% = 0

    For I = 0 To 9
        If MG%(120 + (I + handRL% * 10) * 21) = 999 Then LF% = LF% + 1
    Next

    If LF% = 0 Then
        LPrint "NO LINEUPS VS RHP "
    Else
        Call GetLineup(handRL%, targetTeam$)
        _PrintImage 0
        'CALL PRTSC0 (1)
    End If

    handRL% = 1
    LF% = 0

    For I = 0 To 9
        If MG%(120 + (I + handRL% * 10) * 21) = 999 Then LF% = LF% + 1
    Next

    If LF% = 0 Then
        LPrint "NO LINEUPS VS LHP "
    Else
        Call GetLineup(handRL%, targetTeam$)
        _PrintImage 0
        'CALL PRTSC0 (1)
    End If

    LPrint
    If MG%(99) <> 999 Then LPrint "NO STARTING PITCHING ROTATION DEFINED"
    LPrint "SIZE OF ROTATION IS "; MG%(1); " MAN"
    LPrint "    STARTING ROTATION     % STARTS"

    For I = 1 To MG%(1)
        LPrint Using "(#) "; I;
        If MG%(I + 2) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I + 2)); Tab(30); MG%(I + 12)
        End If
    Next

    LPrint
    LPrint "     SPOT STARTERS         % USED"

    For I = 21 To 36
        LPrint Using "(##) "; I - 20;
        If MG%(I) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I)); Tab(30); MG%(I + 20)
        End If
    Next

    LPrint
    LPrint "  MIDDLE RELIEVERS      % APPEAR"

    For I = 1 To 10
        LPrint Using "(##) "; I;
        If MG%(I + 60) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I + 60)); Tab(35); MG%(I + 70)
        End If
    Next

    LPrint
    LPrint "  SET-UP RELIEVERS      % APPEAR"
    LPrint "   -----  VS LEFT  -------      "

    For I = 1 To 3
        LPrint Using "(##) "; I;
        If MG%(I + 80) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I + 80)); Tab(35); MG%(I + 90)
        End If
    Next

    LPrint "   -----  VS RIGHT  -------      "
    For I = 4 To 6
        LPrint Using "(##) "; I;
        If MG%(I + 80) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I + 80)); Tab(35); MG%(I + 90)
        End If
    Next

    LPrint
    LPrint "          CLOSERS       % APPEAR"
    LPrint "   -----  VS LEFT  -------      "

    For I = 1 To 3
        LPrint Using "(##) "; I;
        If MG%(I + 520) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I + 520)); Tab(35); MG%(I + 530)
        End If
    Next

    LPrint "   -----  VS RIGHT  -------      "

    For I = 4 To 6
        LPrint Using "(##) "; I;
        If MG%(I + 520) < 0 Then
            LPrint
        Else
            LPrint P$(MG%(I + 520)); Tab(35); MG%(I + 530)
        End If
    Next

    LPrint
    LPrint "  PINCH HITTERS        "
    LPrint "  ---- VS LHP  ----"

    For I = 1 To 5
        LPrint Using "(##) "; I;
        If MG%(I + 540) < 0 Then
            LPrint
        Else
            LPrint B$(MG%(I + 540))
        End If
    Next

    LPrint "  ---- VS RHP  ----"

    For I = 6 To 10
        LPrint Using "(##) "; I;
        If MG%(I + 540) < 0 Then
            LPrint
        Else
            LPrint B$(MG%(I + 540))
        End If
    Next

    LPrint
    LPrint "DEFENSIVE REPLACEMENTS"
    LPrint "# PLAYER     P SUBSTITUTE  %  RA"

    For I = 1 To 4
        LPrint Using "# "; I;
        If MG%(I + 560) < 0 Then
            LPrint
        Else
            LPrint Left$(B$(MG%(I + 560)), 10); Tab(19);
            LPrint Using "# "; MG%(I + 576);
            LPrint Left$(B$(MG%(I + 564)), 10); Tab(32); Using "### #"; MG%(I + 568); MG%(I + 572)
        End If
    Next

    LPrint

    If MG%(581) <> 999 Then
        LPrint "NO CLOSING STRATEGY HAS BEEN DEFINED": LPrint:
    Else
        LPrint Tab(31); "CLOSING STRATEGY"
        LPrint
        LPrint "STRATEGY                                        % OF TIME"
        LPrint "(1) INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; MG%(588)
        LPrint "(2) USE BOTH SETUP RELIEVER AND CLOSER "; yesNo$(MG%(589)): LPrint
        LPrint "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
        LPrint "(3) ";: LPrint Using "START INNING IF SAVE OPPORTUNITY               ###"; MG%(582)
        LPrint "(4) ";: LPrint Using "START INNING IF AHEAD                          ###"; MG%(583)
        LPrint "(5) ";: LPrint Using "START INNING IF TIED OR AHEAD                  ###"; MG%(584)
        LPrint "(6) ";: LPrint Using "IF CURRENT PITCHER TIRED AND AHEAD             ###"; MG%(585)
        LPrint "(7) ";: LPrint Using "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; MG%(586)
        LPrint "(8) ";: LPrint Using "WHEN SAVE OPPORTUNITY OCCURS                   ###"; MG%(587)
    End If

    LPrint

    If MG%(590) <> 999 Then
        LPrint "NO PINCH HITTING FOR THE PITCHER STRATEGY DEFINED"
    Else
        LPrint Tab(26); "PINCH HITTING FOR THE PITCHER"
        LPrint
        LPrint "PINCH HIT FOR THE PITCHER FROM 'A' INNING ON"
        LPrint "IF DOWN BY 'B' OR MORE RUNS AND"
        LPrint "'C' OR MORE RUNNERS ON BASE AND"
        LPrint "'D' OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN 'E'"
        LPrint "'F' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO        A      B      C      D     E     F"
        For I = 0 To 2
            LPrint Using "   #           ##     ##      #      #    ###   ###"; I + 1; MG%(I * 6 + 591); MG%(I * 6 + 592); MG%(I * 6 + 593); MG%(I * 6 + 594); MG%(I * 6 + 595); MG%(I * 6 + 596)
        Next
    End If

    LPrint

    If MG%(609) <> 999 Then
        LPrint "NO BUNTING STRATEGY DEFINED FOR THE PITCHER"
    Else
        LPrint "SACRIFICE BUNTING FOR THE PITCHER": LPrint
        LPrint "'A' OUTS OR LESS"
        LPrint "IF 'B' OR LESS RUNS AHEAD AND"
        LPrint "'C' OR LESS RUNS BEHIND"
        LPrint "'D' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                       A      B     C      D"
        LPrint "1 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(610); MG%(611); MG%(612); MG%(613)
        LPrint "    RUNNER ON 1ST           "
        LPrint "2 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(614); MG%(615); MG%(616); MG%(617)
        LPrint "    RUNNER ON 1ST           "
        LPrint "3 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(618); MG%(619); MG%(620); MG%(621)
        LPrint "    RUNNER ON 2ND           "
        LPrint "4 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(622); MG%(623); MG%(624); MG%(625)
        LPrint "    RUNNER ON 2ND           "
        LPrint "5 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(626); MG%(627); MG%(628); MG%(629)
        LPrint "    RUNNERS ON 1ST AND 2ND  "
        LPrint "6 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(630); MG%(631); MG%(632); MG%(633)
        LPrint "    RUNNERS ON 1ST AND 2ND  "
    End If

    LPrint

    If MG%(634) <> 999 Then
        LPrint "NO BUNTING STRATEGY DEFINED FOR POSITION PLAYERS"
    Else
        LPrint "SACRIFICE BUNTING FOR THE POSITION PLAYERS": LPrint
        LPrint "IF 'A' OR LESS RUNS AHEAD AND"
        LPrint "'B' OR LESS RUNS BEHIND"
        LPrint "BATTING AVERAGE IS LESS THAN 'C' AND SLUGGING PCT IS LESS THAN 'D'"
        LPrint "PITCHERS ERA LESS THAN 'E' , 'F' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                        A     B      C      D      E     F"
        LPrint "1 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(635); MG%(636); MG%(637); MG%(638); MG%(639); MG%(640)
        LPrint "    0 OUTS, RUNNER ON 1ST"
        LPrint "2 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(641); MG%(642); MG%(643); MG%(644); MG%(645); MG%(646)
        LPrint "    0 OUTS, RUNNER ON 2ND"
        LPrint "3 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(647); MG%(648); MG%(649); MG%(650); MG%(651); MG%(652)
        LPrint "    0 OUTS, RUNNERS 1ST AND 2ND"
        LPrint "4 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(653); MG%(654); MG%(655); MG%(656); MG%(657); MG%(658)
        LPrint "    0 OUTS, RUNNER ON 1ST"
        LPrint "5 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(659); MG%(660); MG%(661); MG%(662); MG%(663); MG%(664)
        LPrint "    0 OUTS, RUNNER ON 2ND"
        LPrint "6 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(665); MG%(666); MG%(667); MG%(668); MG%(669); MG%(670)
        LPrint "    0 OUTS, RUNNERS ON 1ST AND 2ND"
    End If

    LPrint

    If MG%(671) <> 999 Then
        LPrint "NO HIT AND RUN STRATEGY DEFINED"
    Else
        LPrint "HIT AND RUN STRATEGY": LPrint
        LPrint "IF 'A' OR LESS RUNS AHEAD AND"
        LPrint "'B' OR LESS RUNS BEHIND"
        LPrint "'C' % OF THE TIME"
        LPrint "'D' AB PER STRIKEOUT"
        LPrint " SLUGGING PCT LESS THAN OR EQUAL TO 'E'"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "    SCENARIO                          A     B      C   D    E"
        LPrint "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##    ###  ##  ###"; MG%(672); MG%(673); MG%(674); MG%(866); MG%(872)
        LPrint "2 - 1 OUT, RUNNER ON 1ST             "; Using "##    ##    ###  ##  ###"; MG%(675); MG%(676); MG%(677); MG%(867); MG%(873)
        LPrint "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; Using "##    ##    ###  ##  ###"; MG%(678); MG%(679); MG%(680); MG%(868); MG%(874)
        LPrint "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; Using "##    ##    ###  ##  ###"; MG%(681); MG%(682); MG%(683); MG%(869); MG%(875)
        LPrint "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; Using "##    ##    ###  ##  ###"; MG%(684); MG%(685); MG%(686); MG%(870); MG%(876)
        LPrint "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; Using "##    ##    ###  ##  ###"; MG%(687); MG%(688); MG%(689); MG%(871); MG%(877)
    End If

    LPrint

    If MG%(847) <> 999 Then
        LPrint "NO CRITERIA DEFINED FOR PINCH HITTING FOR THE BATTER"
    Else
        LPrint "PINCH HITTING FOR THE BATTER"
        LPrint
        LPrint "PINCH HIT FOR THE BATTER FROM 'A' INNING ON"
        LPrint "IF DOWN BY 'B' OR MORE RUNS AND"
        LPrint "'C' RUNNERS ON BASE AND"
        LPrint "SLUGGING PCT IS LESS THAN 'D'"
        LPrint "AND BATTING AVERAGE IS LESS THAN 'E'"
        LPrint "'F' % OF THE TIME"
        LPrint
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO        A      B      C      D       E     F"
        For I = 0 To 2
            LPrint Using "   #           ##     ##      #     ###    ###   ###"; I + 1; MG%(I * 6 + 848); MG%(I * 6 + 849); MG%(I * 6 + 850); MG%(I * 6 + 851); MG%(I * 6 + 852); MG%(I * 6 + 853)
        Next
    End If

    LPrint

    If MG%(690) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR INFIELD IN WITH 3RD BASE OCCUPIED"
    Else
        LPrint "INFIELD IN WITH 3RD BASE OCCUPIED": LPrint
        LPrint "IF DOWN BY 'A' RUNS"
        LPrint "AND NOT MORE THAN 'B' RUNS"
        LPrint "FROM 'C' INNING ON"
        LPrint "'D' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                              A     B   C     D"
        LPrint "1 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(691); MG%(692); MG%(693); MG%(694)
        LPrint "2 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(695); MG%(696); MG%(697); MG%(698)
        LPrint "3 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(699); MG%(700); MG%(701); MG%(702)
        LPrint "4 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(703); MG%(704); MG%(705); MG%(706)
    End If

    4035 LPrint
    Cls
    If MG%(707) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT"
    Else
        LPrint "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": LPrint
        LPrint "IF BUNT RATING IS >= 'A'"
        LPrint "'B' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "      A      B"
        LPrint Using "(1)  ##    ###"; MG%(708); MG%(709)
        LPrint Using "(2)  ##    ###"; MG%(710); MG%(711)
        LPrint Using "(3)  ##    ###"; MG%(712); MG%(713)
    End If

    LPrint

    If MG%(714) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SAC HIT SITUATION"
    Else
        LPrint "CORNERS IN VS HITTER IN SAC HIT SITUATION": LPrint
        LPrint "IF AHEAD BY  'A' RUNS OR LESS"
        LPrint "OR BEHIND BY NOT MORE THAN 'B' RUNS"
        LPrint "BUNT RATING >= 'C'"
        LPrint "'D' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                              A     B   C     D"
        LPrint "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(715); MG%(716); MG%(717); MG%(718)
        LPrint "2 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(719); MG%(720); MG%(721); MG%(722)
        LPrint "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(723); MG%(724); MG%(725); MG%(726)
        LPrint "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(727); MG%(728); MG%(729); MG%(730)
    End If

    LPrint

    If MG%(731) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS PITCHER IN SAC HIT SITUATION"
    Else
        LPrint "CORNERS IN VS PITCHER IN SAC HIT SITUATION": LPrint
        LPrint "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO 'A'"
        LPrint "'B' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                                  A      B"
        LPrint "1 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(732); MG%(733)
        LPrint "2 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(734); MG%(735)
        LPrint "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(736); MG%(737)
        LPrint "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(738); MG%(739)
    End If

    LPrint

    If MG%(740) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR CORNERS IN VS HITTER IN SQUEEZE SITUATION"
    Else
        LPrint "CORNERS IN VS HITTER IN SQUEEZE SITUATION": LPrint
        LPrint "IF AHEAD BY  'A' RUNS OR LESS"
        LPrint "OR BEHIND BY NOT MORE THAN 'B' RUNS"
        LPrint "BUNT RATING >= 'C'"
        LPrint "'D' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                              A     B   C     D"
        LPrint "1 - 0 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(741); MG%(742); MG%(743); MG%(744)
        LPrint "2 - 1 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(745); MG%(746); MG%(747); MG%(748)
    End If

    LPrint

    If MG%(749) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR GUARDING THE LINES"
    Else
        LPrint "GUARD LINES STRATEGY": LPrint
        LPrint "IF 'A' RUNS AHEAD"
        LPrint "'B' INNING ON"
        LPrint "'C' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "     A     B      C"
        LPrint "1 - "; Using "##    ##    ###"; MG%(750); MG%(751); MG%(752)
        LPrint "2 - "; Using "##    ##    ###"; MG%(753); MG%(754); MG%(755)
    End If

    LPrint

    If MG%(756) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR PITCHING AROUND HITTERS"
    Else
        LPrint "PITCH AROUND STRATEGY": LPrint
        LPrint "SLUGGING PCT >= 'A'"
        LPrint "'B' INNING ON"
        LPrint "'C' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                                               A     B      C"
        LPrint "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(757); MG%(758); MG%(759)
        LPrint "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(760); MG%(761); MG%(762)
        LPrint "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(763); MG%(764); MG%(765)
    End If

    LPrint

    If MG%(766) <> 999 Then
        LPrint "NO STRATEGY DEFINED FOR THROWING TO THE PLATE"
    Else
        LPrint "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": LPrint
        LPrint "FROM 'A' INNING ON"
        LPrint "'B' OR MORE RUNS AHEAD"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "      A      B"
        LPrint Using "(1)  ##    ###"; MG%(767); MG%(768)
        LPrint Using "(2)  ##    ###"; MG%(769); MG%(770)
    End If

    LPrint
    LPrint "# PINCH RUNNER"

    For I = 1 To 6
        LPrint Using "# "; I;
        If MG%(I + 771) < 0 Then
            LPrint
        Else
            LPrint B$(MG%(I + 771))
        End If
    Next

    LPrint
    LPrint "DEFENSIVE SUBS FOR PINCH RUNNERS"
    LPrint "SCORE TIED OR DOWN BY ONE RUN"
    If MG%(778) <> 999 Then
        LPrint "NO SUBS DEFINED"
    Else
        LPrint "# PLAYER     I  SUBSTITUTE  %"
        For I = 0 To 5
            LPrint Using "# "; I + 1;
            If MG%(I * 4 + 779) < 0 Then
                LPrint
            Else
                LPrint Left$(B$(MG%(I * 4 + 779)), 10); Tab(19);
                LPrint Using "## "; MG%(I * 4 + 780);
                LPrint Left$(B$(MG%(I * 4 + 781)), 10); Tab(33); Using "###"; MG%(I * 4 + 782)
            End If
        Next
    End If

    LPrint
    If MG%(803) <> 999 Then
        LPrint "NO STRATEGY DEFINED TO PINCH RUN FOR PITCHER"
    Else
        LPrint "PINCH RUN FOR PITCHER": LPrint
        LPrint "RUN RATING <= 'A'"
        LPrint "'B' INNING ON"
        LPrint "'C' % OF THE TIME"
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                A     B      C"
        LPrint "1 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(804); MG%(805); MG%(806)
        LPrint "2 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(807); MG%(808); MG%(809)
    End If

    LPrint

    If MG%(810) <> 999 Then
        LPrint "NO BASE RUNNING STRATEGY DEFINED"
    Else
        LPrint "BASE RUNNING STRATEGY": LPrint
        LPrint Tab(32); "TABLE OF VALUES"
        LPrint "SCENARIO                            %       %          %          %"
        LPrint "                                  SAFE    NORMAL   AGGRESSIVE   TOTAL"
        LPrint " 1 - INNING 1-3, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(811); MG%(812); MG%(813); MG%(811) + MG%(812) + MG%(813)
        LPrint " 2 - INNING 1-3, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(814); MG%(815); MG%(816); MG%(816) + MG%(815) + MG%(814)
        LPrint " 3 - INNING 1-3, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(817); MG%(818); MG%(819); MG%(819) + MG%(818) + MG%(817)
        LPrint " 4 - INNING 4-6, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(820); MG%(821); MG%(822); MG%(820) + MG%(821) + MG%(822)
        LPrint " 5 - INNING 4-6, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(823); MG%(824); MG%(825); MG%(823) + MG%(824) + MG%(825)
        LPrint " 6 - INNING 4-6, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(826); MG%(827); MG%(828); MG%(826) + MG%(827) + MG%(828)
        LPrint " 7 - INNING 7-9, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(829); MG%(830); MG%(831); MG%(829) + MG%(830) + MG%(831)
        LPrint " 8 - INNING 7-9, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(832); MG%(833); MG%(834); MG%(832) + MG%(833) + MG%(834)
        LPrint " 9 - INNING 7-9, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(835); MG%(836); MG%(837); MG%(835) + MG%(836) + MG%(837)
        LPrint "10 - INNING 9- , WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(838); MG%(839); MG%(840); MG%(838) + MG%(839) + MG%(840)
        LPrint "11 - INNING 9- , WHEN TIED        "; Using "###     ###        ###          ###"; MG%(841); MG%(842); MG%(843); MG%(841) + MG%(842) + MG%(843)
        LPrint "12 - INNING 9- , WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(844); MG%(845); MG%(846); MG%(844) + MG%(845) + MG%(846)
    End If

    LPrint

End Sub

'----------------------------------------
'       ShowPitcherInfo Subroutine
'----------------------------------------
'...explanation...
Sub ShowPitcherInfo (targetTeam$)

    Shared L%(), MG%(), P%()
    Shared B1$(), P$(), mgrX$()

    Cls

    Locate 1, 1
    Print "#";: Color L%(11), L%(12): Print targetTeam$;
    Color 14, 0
    Locate , 16: Print "T   G GS   W- L   ERA"

    For I = 0 To 21

        If P$(I) = "XXX" Or Left$(P$(I), 1) = " " Or P$(I) = "" Then P%(I, 15) = 98

        If P%(I, 15) <> 1 And P$(I) <> "XXX" Then
            Color 2, 0

            If I <= 9 Then
                Print Using "#"; I;
            Else
                'I > 9
                Print mgrX$(I - 10);
            End If

            Color 15, 0

            For I1 = 1 To MG%(1)
                If I = MG%(I1 + 2) Then Color 0, 7
            Next

            Locate , 3: Print P$(I);

            Color 15, 0
            Locate , 16: Print B1$(P%(I, 0) + 2);
            Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
            Locate , 24: Print Using "  ##"; P%(I, 1);
            Print "-";
            Print Using "## ##.##"; P%(I, 2); P%(I, 10) / 100
        End If

    Next I

End Sub

'----------------------------------------
'       SpotStarters Subroutine
'----------------------------------------
'...explanation...
Sub SpotStarters (targetTeam$, yearNumber$)

    Shared P$()
    Shared MG%()

    Do
        Call ShowPitcherInfo(targetTeam$)

        Locate 1, 40: Print "     SPOT STARTERS         % USED"
        SS% = 0
        For I = 21 To 36
            Locate , 40
            Print Using "(##) "; I - 20;
            If MG%(I) < 0 Then
                Print
            Else
                Print P$(MG%(I)); Tab(70); MG%(I + 20)
                SS% = SS% + MG%(I + 20)
            End If
        Next

        Locate 19, 40: Print "TOTAL MUST EQUAL 100"; Tab(70); SS%
        Locate 20, 40: Print "(A)DD SPOT STARTER (E)DIT SPOT %"
        Locate 21, 40: Print "(D)ELETE STARTER (S)AVE PROFILE"

        I$ = GetKeyPress$

        Select Case I$
            Case "S":
                If SS% = 100 Then
                    MG%(99) = 999
                    Call UpdateManagerFile(yearNumber$)
                    'GoTo 55
                End If

            Case "D":
                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER NUMBER TO DELETE (1-16) "; I1
                Loop Until I1 >= 1 And I1 <= 16

                MG%(I1 + 20) = -1: MG%(I1 + 40) = -1

            Case "E":
                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER NUMBER TO EDIT (1-16) "; I1
                Loop Until I1 >= 1 And I1 <= 16

                Do
                    Locate 20, 40: Input "ENTER % OF TIME TO SPOT START "; I2
                Loop Until I2 >= 0 And I2 <= 100
                MG%(I1 + 40) = I2

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
                MG%(I1 + 20) = I2

                Do
                    Locate 20, 40: Print Space$(32)
                    Locate 21, 40: Print Space$(32)
                    Locate 20, 40: Input "ENTER % OF TIME TO SPOT START "; I2
                Loop Until I2 >= 0 And I2 <= 100
                MG%(I1 + 40) = I2

        End Select

    Loop Until UCase$(I$) = "S"

End Sub

'----------------------------------------
'       RotationParams Subroutine
'----------------------------------------
'...explanation...
Sub RotationParams (targetTeam$)

    Shared MG%()
    Shared P$()

    Do
        4517 '
        Call ShowPitcherInfo(targetTeam$)

        Locate 1, 40: Print "(0) SIZE OF ROTATION IS "; MG%(1)
        Locate 3, 40: Print "    STARTING ROTATION     % STARTS"

        For I = 1 To MG%(1)
            Locate , 40
            Print Using "(#) "; I;
            If MG%(I + 2) < 0 Then
                Print
            Else
                Print P$(MG%(I + 2)); Tab(70); MG%(I + 12)
            End If
        Next

        Locate 10, 40: Print "(6) MOVE TO SPOT STARTERS"

        '4536
        Do
            I$ = GetKeyPress$
            I = Val(I$)

            If I = 0 Then
                Do
                    Locate 12, 40
                    Input "ENTER ROTATION SIZE (1-5) "; MG%(1)
                Loop Until MG%(1) >= 1 And MG%(1) <= 5

                Locate 12, 40: Print Space$(32)
                For I = MG%(1) + 1 To 5: MG%(I + 2) = -1: MG%(I + 12) = -1: Next
            End If

            If I >= 1 And I <= MG%(1) Then
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

                MG%(I + 2) = I1
                Locate 12, 40: Print Space$(21)
                Do
                    Locate 12, 40: Input "ENTER % OF TIME TO START "; MG%(I + 12)
                Loop While (MG%(I + 12) >= 0 And MG%(I + 12) <= 100)
                Locate 12, 40: Print Space$(31)
            End If

            If I = 6 Then
                Call SpotStarters(targetTeam$, yearNumber$)
            End If 'Done with value 6

        Loop Until I >= 0 And I <= 6

    Loop Until I = 6

End Sub

'----------------------------------------
'      PitchingRotations Subroutine
'----------------------------------------
'...explanation...
Sub PitchingRotations (action%, targetTeam$)

    Shared P$()
    Shared MG%(), P%()

    Dim GS%(21, 1)

    'action% = 1: New Rotation
    'action% = 2: Edit Existing

    If action% = 2 Then

        If MG%(99) = 999 Then

            Call RotationParams(targetTeam$)

        Else
            '4600
            Cls
            Print "ERROR::: ROTATION FILE NOT FOUND"
            Print
            Print "DO YOU WANT TO ENTER A ROTATION (YN)"
            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "N" Then Exit Sub

        End If 'Done checking if 999

    End If 'Done for action = 2

    'Enter Rotation
    I = 1
    JJ = 1
    
	Locate 6, 8
	Color 15, 1
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: 
		Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
	
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
	Locate 7

    '4604
    Do
        U$ = "COMPUTER DETERMINE ROTATION (YN) "
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
    Color 14: Print "(1) ";: Color 15: Print "MAN ROTATION    ";
    Color 14: Print "(4) ";: Color 15: Print "MAN ROTATION"

    Color 14: Print "(2) ";: Color 15: Print "MAN ROTATION    ";
    Color 14: Print "(5) ";: Color 15: Print "MAN ROTATION"

    Color 14: Print "(3) ";: Color 15: Print "MAN ROTATION"
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

        If P$(I) <> "XXX" Then
            Print P$(I); Tab(18); P%(I, 5);
        End If

        Locate , 25
        If P$(I + 7) <> "XXX" Then
            Print P$(I + 7); Tab(43); P%(I + 7, 5);
        End If

        Locate , 50
        If P$(I + 14) <> "XXX" Then
            Print P$(I + 14); Tab(68); P%(I + 14, 5)
        End If
    Next

    Do
        I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 1 And I <= 5

    MG%(1) = I
    MG%(2) = 0

    For I = 3 To 36
        MG%(I) = -1
    Next

    For I = 41 To 56
        MG%(I) = 0
    Next

    MG%(0) = 999

    If CR% <> 1 Then

        I% = 99
        Call ShowPitcherInfo(targetTeam$)

        For I = 0 To MG%(1) - 1

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

                Loop Until I1 <= 21 And MG%(3) <> I1 And MG%(4) <> I1 And MG%(5) <> I1 And MG%(6) <> I1 And MG%(7) <> I1

                Locate , 59: Print P$(I1)

				Do
					Locate , 59: Print "ANY CHANGE (YN)";
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then
                    For I2 = 15 To 17
                        Locate I2, 59
                        Print Space$(17)
                    Next
                End If
            Loop Until UCase$(I$) = "N"

            MG%(I + 3) = I1

            For I2 = 15 To 17
                Locate I2, 59
                Print Space$(18)
            Next

        Next

    Else

        'COMPUTER SET ROTATION
        CR% = 0
        For I = 0 To 21
            If P$(I) = "XXX" Then
                GS%(I, 0) = 0: GS%(I, 1) = -1
            Else
                GS%(I, 0) = P%(I, 5): GS%(I, 1) = I
            End If
        Next

        For Z = 0 To 20
            For Z1 = Z + 1 To 21
                If GS%(Z, 0) < GS%(Z1, 0) Then
                    Swap GS%(Z, 0), GS%(Z1, 0): Swap GS%(Z, 1), GS%(Z1, 1)
                End If
            Next Z1
        Next Z

        For I = 0 To MG%(1) - 1
            MG%(I + 3) = GS%(I, 1)
        Next

        I1 = 0

        For I = MG%(1) To 21
            I1 = I1 + 1
            If GS%(I, 0) <> 0 Then
                MG%(I1 + 20) = GS%(I, 1)
            Else
                Exit For
            End If
        Next

    End If 'Done checking value of CR

    4700 '
    I1 = I1 - 1
    If MG%(1) = 1 Then MG%(13) = 100: Call RotationParams(targetTeam$)
    If MG%(1) = 2 Then GS% = 75
    If MG%(1) = 3 Then GS% = 54
    If MG%(1) = 4 Then GS% = 41
    If MG%(1) = 5 Then GS% = 32
    GS% = GS% * SL% / 162

    For I = 1 To MG%(1)
        MG%(I + 12) = Int(P%(MG%(I + 2), 5) * 100 / GS% + .5)
    Next

    SS% = 0

    For I = 21 To 36
        If MG%(I) >= 0 And MG%(I) <= 21 Then
            SS% = SS% + P%(MG%(I), 5)
        End If
    Next

    For I = 21 To 36
        If MG%(I) >= 0 And MG%(I) <= 21 Then
            MG%(I + 20) = Int(P%(MG%(I), 5) * 100 / SS% + .5)
        End If
    Next

    Call RotationParams(targetTeam$)

End Sub

'----------------------------------------
'    SetupMiddleRelievers Subroutine
'----------------------------------------
'...explanation...
Sub SetupMiddleRelievers (targetTeam$, yearNumber$)

    Shared MG%(), L%(), P%()
    Shared B1$(), P$(), mgrX$()

    Do

        'MIDDLE RELIEVERS
        If MG%(60) <> 999 Then
            MG%(60) = 999
            For I = 1 To 10
                MG%(I + 60) = -1
                MG%(I + 70) = 0
            Next
        End If

        RA% = 0
        For I = 0 To 21
            If P$(I) <> "XXX" Then
                RA% = RA% + P%(I, 4) - P%(I, 5) - P%(I, 3)
            End If
        Next

        Cls
        Locate 1, 1
        Print "#";: Color L%(11), L%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "T   G GS   W- L %RA   ERA"
        For I = 0 To 21
            If P$(I) <> "XXX" And Left$(P$(I), 1) <> " " And P$(I) <> "" Then
                Color 2, 0
                If I <= 9 Then Print Using "#"; I;: Color 15, 0
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0
                For I1 = 1 To 10
                    If I = MG%(I1 + 60) Then Color L%(11), L%(12)
                Next
                Locate , 3: Print P$(I);
                Color 15, 0
                Locate , 16: Print B1$(P%(I, 0) + 2);
                Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
                Locate , 24: Print Using "  ##"; P%(I, 1);
                Print "-";
                Print Using "## ### ##.##"; P%(I, 2); (P%(I, 4) - P%(I, 5) - P%(I, 3)) / RA% * 100; P%(I, 10) / 100
            End If
        Next I

        Locate 18, 45: Print "%RA - PITCHER'S PERCENTAGE OF TEAM'S"
        Locate , 45: Print "RELIEF APPEARANCES MINUS SAVES"

        Locate 1, 48: Print "  MIDDLE RELIEVERS      % APPEAR"
        For I = 1 To 10
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 60) < 0 Then
                Print
            Else
                Print P$(MG%(I + 60)); Tab(75); MG%(I + 70)
            End If
        Next

        Locate 13, 48: Print "(11) ACCEPT CHOICES"
        Locate 14, 48: Print "(12) DELETE A RELIEVER"

        Do
            Locate 15, 48
            Input "ENTER SELECTION (1-12)"; I
        Loop Until I >= 1 And I <= 12

        Select Case I
            Case 1 TO 10

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

                MG%(I + 60) = I1

                Do
                    Locate 17, 48: Input "ENTER % OF TIME TO APPEAR "; MG%(I + 70)

                Loop Until MG%(I + 70) >= 0 And MG%(I + 70) <= 100

                Locate 17, 48: Print Space$(31)
                Locate 16, 48: Print Space$(31)

            Case 11:
                Call UpdateManagerFile(yearNumber$)

            Case 12:
                Do
                    Locate 16, 48: Input "SELECT RELIEVER TO DELETE: "; I
                Loop Until I >= 1 And I <= 10

                MG%(I + 60) = -1
                MG%(I + 70) = 0
        End Select

    Loop Until I = 11

End Sub

'----------------------------------------
'       SetupRelievers Subroutine
'----------------------------------------
'...explanation...
Sub SetupRelievers (targetTeam$, yearNumber$)

    Shared B1$(), P$(), mgrX$()
    Shared L%(), MG%(), P%()

    Do

        'SET-UP RELIEVERS
        If MG%(90) <> 999 Then
            MG%(90) = 999
            For I = 1 To 6: MG%(I + 80) = -1: MG%(I + 90) = 0: Next
        End If

        Cls
        Locate 1, 1
        Print "#";
        Color L%(11), L%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "T   G GS   W- L SV   ERA"

        For I = 0 To 21
            If P$(I) <> "XXX" And Left$(P$(I), 1) <> " " And P$(I) <> "" Then
                Color 2, 0
                If I <= 9 Then Print Using "#"; I;: Color 15, 0
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0

                For I1 = 1 To 6
                    If I = MG%(I1 + 80) Then Color L%(11), L%(12)
                Next

                Locate , 3: Print P$(I);
                Color 15, 0
                Locate , 16: Print B1$(P%(I, 0) + 2);
                Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
                Locate , 24: Print Using "  ##"; P%(I, 1);
                Print "-";
                Print Using "## ### ##.##"; P%(I, 2); P%(I, 3); P%(I, 10) / 100
            End If
        Next I

        Locate 1, 48: Print "  SET-UP RELIEVERS      % APPEAR"
        Locate , 48: Print "   -----  VS LEFT  -------      "

        For I = 1 To 3
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 80) < 0 Then
                Print
            Else
                Print P$(MG%(I + 80)); Tab(75); MG%(I + 90)
            End If
        Next

        Locate , 48: Print "   -----  VS RIGHT  -------      "

        For I = 4 To 6
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 80) < 0 Then
                Print
            Else
                Print P$(MG%(I + 80)); Tab(75); MG%(I + 90)
            End If
        Next

        Locate 13, 48: Print "(7) ACCEPT CHOICES"
        Locate 14, 48: Print "(8) DELETE A RELIEVER"

        Do
            Locate 15, 48
            Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I
            Case 1 TO 6:
                5220 '
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
                MG%(I + 80) = I1
                'LOCATE 16,48:PRINT "                     "

                Do
                    Locate 17, 48
                    Input "ENTER % OF TIME TO APPEAR "; MG%(I + 90)
                Loop Until MG%(I + 90) >= 0 And MG%(I + 90) <= 100
                Locate 17, 48: Print Space$(31)
                Locate 16, 48: Print Space$(31)

            Case 7:
                Call UpdateManagerFile(yearNumber$)

            Case 8:
                Do
                    Locate 16, 48
                    Input "SELECT RELIEVER TO DELETE: "; I
                Loop Until I >= 1 And I <= 6
                MG%(I + 80) = -1: MG%(I + 90) = 0
        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'       SetupClosers Subroutine
'----------------------------------------
'...explanation...
Sub SetupClosers (targetTeam$, yearNumber$)

    Shared B1$(), P$(), mgrX$()
    Shared L%(), MG%(), P%()

    Do

        'CLOSERS
        If MG%(520) <> 999 Then
            MG%(520) = 999
            For I = 1 To 6: MG%(I + 520) = -1: MG%(I + 530) = 0: Next
        End If

        Cls
        Locate 1, 1
        Print "#";: Color L%(11), L%(12): Print targetTeam$;
        Color 14, 0: Locate , 16: Print "T   G GS   W- L SV   ERA"

        For I = 0 To 21
            If P$(I) <> "XXX" And Left$(P$(I), 1) <> " " And P$(I) <> "" Then
                Color 2, 0
                If I <= 9 Then Print Using "#"; I;: Color 15, 0
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0
                For I1 = 1 To 6
                    If I = MG%(I1 + 520) Then Color L%(11), L%(12)
                Next
                Locate , 3: Print P$(I);
                Color 15, 0
                Locate , 16: Print B1$(P%(I, 0) + 2);
                Locate , 17: Print Using " ### ##"; P%(I, 4); P%(I, 5);
                Locate , 24: Print Using "  ##"; P%(I, 1);
                Print "-";
                Print Using "## ### ##.##"; P%(I, 2); P%(I, 3); P%(I, 10) / 100
            End If
        Next I

        Locate 1, 48: Print "          CLOSERS       % APPEAR"
        Locate , 48: Print "   -----  VS LEFT  -------      "

        For I = 1 To 3
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 520) < 0 Then
                Print
            Else
                Print P$(MG%(I + 520)); Tab(75); MG%(I + 530)
            End If
        Next

        Locate , 48: Print "   -----  VS RIGHT  -------      "

        For I = 4 To 6
            Locate , 48
            Print Using "(##) "; I;
            If MG%(I + 520) < 0 Then
                Print
            Else
                Print P$(MG%(I + 520)); Tab(75); MG%(I + 530)
            End If
        Next

        Locate 13, 48: Print "(7) ACCEPT CHOICES"
        Locate 14, 48: Print "(8) DELETE A RELIEVER"

        Do
            Locate 15, 48: Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I
            Case 1 TO 6:
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
                MG%(I + 520) = I1

                Do
                    Locate 17, 48
                    Input "ENTER % OF TIME TO APPEAR "; MG%(I + 530)
                Loop Until MG%(I + 530) >= 0 And MG%(I + 530) <= 100
                Locate 17, 48: Print Space$(31)
                Locate 16, 48: Print Space$(31)

            Case 7:
                Call UpdateManagerFile(yearNumber$)

            Case 8:
                Do
                    Locate 16, 48: Input "SELECT RELIEVER TO DELETE: "; IR
                Loop Until IR >= 1 And IR <= 6
                MG%(IR + 520) = -1: MG%(IR + 530) = 0

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'       HittingStrategy Subroutine
'----------------------------------------
'...explanation...
Sub HittingStrategy (yearNumber$)

    Shared MG%()

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
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 13
        Call box0(4, 12, 11, 58, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        Select Case Selection%
            Case 1:
                Call PHPITCHER(yearNumber$)

            Case 2:
                Do
                    'SAC BUNT HIT BY PITCHER"
                    Cls
                    If MG%(609) <> 999 Then MG%(609) = 999: For I = 1 To 24: MG%(609 + I) = 0: Next
                    Locate 1, 24: Color 14, 2: Print "SACRIFICE BUNTING FOR THE PITCHER": Color 15, 0: Print
                    Color 15, 1: Print " A ";: Color 15, 0: Print " OUTS OR LESS"
                    Print "IF ";: Color 15, 1: Print " B ";: Color 15, 0: Print " OR LESS RUNS AHEAD AND"
                    Color 15, 1: Print " C ";: Color 15, 0: Print " OR LESS RUNS BEHIND"
                    Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "SCENARIO                       A      B     C      D"
                    Print "1 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(610); MG%(611); MG%(612); MG%(613)
                    Print "    RUNNER ON 1ST           "
                    Print "2 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(614); MG%(615); MG%(616); MG%(617)
                    Print "    RUNNER ON 1ST           "
                    Print "3 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(618); MG%(619); MG%(620); MG%(621)
                    Print "    RUNNER ON 2ND           "
                    Print "4 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(622); MG%(623); MG%(624); MG%(625)
                    Print "    RUNNER ON 2ND           "
                    Print "5 - 7TH INNING OR LESS         "; Using "#     ##    ##     ###"; MG%(626); MG%(627); MG%(628); MG%(629)
                    Print "    RUNNERS ON 1ST AND 2ND  "
                    Print "6 - 8TH INNING ON              "; Using "#     ##    ##     ###"; MG%(630); MG%(631); MG%(632); MG%(633)
                    Print "    RUNNERS ON 1ST AND 2ND  "
                    Print "7 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-7) "; I
                    Loop Until I >= 1 And I <= 7

                    If I = 7 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "OUTS OR LESS        "; MG%(I * 4 + 610)
                        Locate 22, 1: Input "RUNS AHEAD OR LESS  "; MG%(I * 4 + 611)
                        Locate 22, 1: Input "RUNS BEHIND OR LESS "; MG%(I * 4 + 612)
                        Locate 22, 1: Input "PERCENT OF TIME     "; MG%(I * 4 + 613)
                    End If

                Loop Until I = 7

            Case 3:
                Do
                    3150 'SAC BUNT HIT BY POSITION PLAYERS"
                    Cls
                    If MG%(634) <> 999 Then MG%(634) = 999: For I = 1 To 36: MG%(634 + I) = 0: Next
                    Locate 1, 24: Color 14, 2: Print "SACRIFICE BUNTING FOR THE POSITION PLAYERS": Color 15, 0: Print
                    Print "IF ";: Color 15, 1: Print " A ";: Color 15, 0: Print " OR LESS RUNS AHEAD AND"
                    Color 15, 1: Print " B ";: Color 15, 0: Print " OR LESS RUNS BEHIND"
                    Print "BATTING AVERAGE IS LESS THAN ";: Color 15, 1: Print " C ";: Color 15, 0: Print " AND SLUGGING PCT IS LESS THAN ";: Color 15, 1: Print " D "
                    Color 15, 0: Print "PITCHERS ERA LESS THAN ";: Color 15, 1: Print " E ";: Color 15, 0: Print ", ";: Color 15, 1: Print " F ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "SCENARIO                        A     B      C      D      E     F"
                    Print "1 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(635); MG%(636); MG%(637); MG%(638); MG%(639); MG%(640)
                    Print "    0 OUTS, RUNNER ON 1ST"
                    Print "2 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(641); MG%(642); MG%(643); MG%(644); MG%(645); MG%(646)
                    Print "    0 OUTS, RUNNER ON 2ND"
                    Print "3 - 7TH INNING OR LESS         "; Using "##    ##    ###    ###    ###   ###"; MG%(647); MG%(648); MG%(649); MG%(650); MG%(651); MG%(652)
                    Print "    0 OUTS, RUNNERS 1ST AND 2ND"
                    Print "4 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(653); MG%(654); MG%(655); MG%(656); MG%(657); MG%(658)
                    Print "    0 OUTS, RUNNER ON 1ST"
                    Print "5 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(659); MG%(660); MG%(661); MG%(662); MG%(663); MG%(664)
                    Print "    0 OUTS, RUNNER ON 2ND"
                    Print "6 - 8TH INNING ON              "; Using "##    ##    ###    ###    ###   ###"; MG%(665); MG%(666); MG%(667); MG%(668); MG%(669); MG%(670)
                    Print "    0 OUTS, RUNNERS ON 1ST AND 2ND"
                    Print "7 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-7) "; I
                    Loop Until I >= 1 And I <= 7

                    If I = 7 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "RUNS AHEAD OR LESS        "; MG%(I * 6 + 635)
                        Locate 22, 1: Input "RUNS BEHIND OR LESS       "; MG%(I * 6 + 636)
                        Locate 22, 1: Input "BATTING AVERAGE LESS THAN "; MG%(I * 6 + 637)
                        Locate 22, 1: Input "SLUGGING PCT LESS THAT    "; MG%(I * 6 + 638)
                        Locate 22, 1: Input "PITCHER'S ERA LESS THAN   "; MG%(I * 6 + 639)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 6 + 640)
                    End If

                Loop Until I = 7

            Case 4:
                Call HittingStrategy(YN$)

            Case 5:
                Call PHBATTER(YN$)

            Case 6: Cls '
        End Select

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub

'----------------------------------------
'       HitAndRun Subroutine
'----------------------------------------
'...explanation...
Sub HitAndRun (yearNumber$)

    Shared MG%()

    Do
        'HIT AND RUN
        Cls
        If MG%(671) <> 999 Then
            MG%(671) = 999
            For I = 1 To 18: MG%(671 + I) = 0: Next
        End If

        Locate 1, 30: Color 14, 2: Print "HIT AND RUN STRATEGY": Color 15, 0: Print
        Print "IF ";: Color 15, 1: Print " A ";: Color 15, 0: Print " OR LESS RUNS AHEAD AND"
        Color 15, 1: Print " B ";: Color 15, 0: Print " OR LESS RUNS BEHIND"
        Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
        Color 15, 1: Print " D ";: Color 15, 0: Print " AB PER STRIKEOUT"
        Print " SLUGGING PCT LESS THAN OR EQUAL TO ";: Color 15, 1: Print " E ": Color 15, 0
        Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
        Print "    SCENARIO                          A     B      C   D    E"
        Print "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##    ###  ##  ###"; MG%(672); MG%(673); MG%(674); MG%(866); MG%(872)
        Print "2 - 1 OUT, RUNNER ON 1ST             "; Using "##    ##    ###  ##  ###"; MG%(675); MG%(676); MG%(677); MG%(867); MG%(873)
        Print "3 - 0 OUTS, RUNNERS ON 1ST AND 3RD   "; Using "##    ##    ###  ##  ###"; MG%(678); MG%(679); MG%(680); MG%(868); MG%(874)
        Print "4 - 1 OUT, RUNNERS ON 1ST AND 3RD    "; Using "##    ##    ###  ##  ###"; MG%(681); MG%(682); MG%(683); MG%(869); MG%(875)
        Print "5 - 0 OUTS, RUNNERS ON 1ST AND 2ND   "; Using "##    ##    ###  ##  ###"; MG%(684); MG%(685); MG%(686); MG%(870); MG%(876)
        Print "6 - 1 OUT, RUNNERS ON 1ST AND 2ND    "; Using "##    ##    ###  ##  ###"; MG%(687); MG%(688); MG%(689); MG%(871); MG%(877)
        Print "7 - ACCEPT CHOICES"

        Do
            Locate 22, 1
            Input "ENTER SELECTION (1-7) "; I
        Loop Until I >= 1 And I <= 7

        If I = 7 Then
            Call UpdateManagerFile(yearNumber$)
        Else
            I = I - 1
            Locate 22, 1: Input "RUNS AHEAD OR LESS        "; MG%(I * 3 + 672)
            Locate 22, 1: Input "RUNS BEHIND OR LESS       "; MG%(I * 3 + 673)
            Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 674)
            Locate 22, 1: Input "AB PER STRIKEOUT          "; MG%(I + 866)
            Locate 22, 1: Input "SLUGGING PCT <=           "; MG%(I + 872)
        End If

    Loop Until I = 7

End Sub

'----------------------------------------
'      DefensiveStrategy Subroutine
'----------------------------------------
'...explanation...
Sub DefensiveStrategy (yearNumber$)

    Shared MG%()

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
        Selection% = 1
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 18
        Call box0(4, 17, 14, 63, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        Select Case Selection%
            Case 1
                Do
                    3350 'INFIELD IN
                    Cls
                    If MG%(690) <> 999 Then MG%(690) = 999: For I = 1 To 16: MG%(690 + I) = 0: Next
                    Locate 1, 30: Color 14, 2: Print "INFIELD IN WITH 3RD BASE OCCUPIED": Color 15, 0: Print
                    Print "IF DOWN BY ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS"
                    Print "AND NOT MORE THAN ";: Color 15, 1: Print " B ";: Color 15, 0: Print " RUNS"
                    Print "FROM ";: Color 15, 1: Print " C ";: Color 15, 0: Print "INNING ON"
                    Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "SCENARIO                              A     B   C     D"
                    Print "1 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(691); MG%(692); MG%(693); MG%(694)
                    Print "2 - 0 OUTS                           "; Using "##    ##   #   ###"; MG%(695); MG%(696); MG%(697); MG%(698)
                    Print "3 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(699); MG%(700); MG%(701); MG%(702)
                    Print "4 - <2 OUTS, RUNNER ON 1ST           "; Using "##    ##   #   ###"; MG%(703); MG%(704); MG%(705); MG%(706)
                    Print "5 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-5) "; I
                    Loop Until I >= 1 And I <= 5

                    If I = 5 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "AT LEAST DOWN BY          "; MG%(I * 4 + 691)
                        Locate 22, 1: Input "BUT NOT MORE THAN         "; MG%(I * 4 + 692)
                        Locate 22, 1: Input "THIS INNING ON            "; MG%(I * 4 + 693)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 4 + 694)
                    End If

                Loop Until I = 5

            Case 2
                Do
                    3375 'DEFENSIVE STRATEGIES"
                    Cls
                    If MG%(707) <> 999 Then MG%(707) = 999: For I = 1 To 6: MG%(707 + I) = 0: Next
                    Locate 1, 22: Color 14, 2: Print "CORNERS IN VS HITTER WHO MIGHT BUNT FOR A HIT": Color 15, 0: Print
                    Print "IF BUNT RATING IS >= ";: Color 15, 1: Print " A ": Color 15, 0
                    Color 15, 1: Print " B ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "      A      B"
                    Print Using "(1)  ##    ###"; MG%(708); MG%(709)
                    Print Using "(2)  ##    ###"; MG%(710); MG%(711)
                    Print Using "(3)  ##    ###"; MG%(712); MG%(713)
                    Print "(4) - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-4) "; I
                    Loop Until I >= 1 And I <= 4

                    If I = 4 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "BUNT RATING >=            "; MG%(I * 2 + 708)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 2 + 709)
                    End If

                Loop Until I = 4

            Case 3
                Do
                    3400 'HITTER SAC BUNT
                    Cls
                    If MG%(714) <> 999 Then MG%(714) = 999: For I = 1 To 16: MG%(714 + I) = 0: Next
                    Locate 1, 20: Color 14, 2: Print "CORNERS IN VS HITTER IN SAC HIT SITUATION": Color 15, 0: Print
                    Print "IF AHEAD BY  ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS OR LESS"
                    Print "OR BEHIND BY NOT MORE THAN ";: Color 15, 1: Print " B ";: Color 15, 0: Print " RUNS"
                    Print "BUNT RATING >= ";: Color 15, 1: Print " C ": Color 15, 0
                    Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "SCENARIO                              A     B   C     D"
                    Print "1 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(715); MG%(716); MG%(717); MG%(718)
                    Print "2 - 0 OUTS, RUNNER ON 1ST            "; Using "##    ##   #   ###"; MG%(719); MG%(720); MG%(721); MG%(722)
                    Print "3 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(723); MG%(724); MG%(725); MG%(726)
                    Print "4 - 0 OUTS, RUNNER ON 1ST AND 2ND    "; Using "##    ##   #   ###"; MG%(727); MG%(728); MG%(729); MG%(730)
                    Print "5 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-5) "; I
                    Loop Until I >= 1 And I <= 5

                    If I = 5 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "AHEAD BY NOT MORE THAN    "; MG%(I * 4 + 715)
                        Locate 22, 1: Input "DOWN BY NOT MORE THAN     "; MG%(I * 4 + 716)
                        Locate 22, 1: Input "BUNT RATING >=            "; MG%(I * 4 + 717)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 4 + 718)
                    End If

                Loop Until I = 5

            Case 4
                Do
                    3425 'PITCHER SAC BUNT
                    Cls
                    If MG%(731) <> 999 Then MG%(731) = 999: For I = 1 To 8: MG%(731 + I) = 0: Next
                    Locate 1, 20: Color 14, 2: Print "CORNERS IN VS PITCHER IN SAC HIT SITUATION": Color 15, 0: Print
                    Print "PITCHER'S BATTING AVERAGE LESS THAN OR EQUAL TO ";: Color 15, 1: Print " A "
                    Color 15, 1: Print " B ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "SCENARIO                                  A      B"
                    Print "1 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(732); MG%(733)
                    Print "2 - <= 1 OUT, RUNNER ON 1ST              "; Using "###   ###"; MG%(734); MG%(735)
                    Print "3 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(736); MG%(737)
                    Print "4 - 0 OUTS, RUNNER ON 1ST AND 2ND OR 2ND "; Using "###   ###"; MG%(738); MG%(739)
                    Print "5 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-5) "; I
                    Loop Until I >= 1 And I <= 5

                    If I = 5 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "BATTING AVG LESS THAN     "; MG%(I * 2 + 732)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 2 + 733)
                    End If

                Loop Until I = 5

            Case 5
                Do
                    3450 'HITTER SAC BUNT SQUEEZE
                    Cls
                    If MG%(740) <> 999 Then MG%(740) = 999: For I = 1 To 8: MG%(740 + I) = 0: Next
                    Locate 1, 20: Color 14, 2: Print "CORNERS IN VS HITTER IN SQUEEZE SITUATION": Color 15, 0: Print
                    Print "IF AHEAD BY  ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS OR LESS"
                    Print "OR BEHIND BY NOT MORE THAN ";: Color 15, 1: Print " B ";: Color 15, 0: Print " RUNS"
                    Print "BUNT RATING >= ";: Color 15, 1: Print " C ": Color 15, 0
                    Color 15, 1: Print " D ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "SCENARIO                              A     B   C     D"
                    Print "1 - 0 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(741); MG%(742); MG%(743); MG%(744)
                    Print "2 - 1 OUT, RUNNER ON 3RD             "; Using "##    ##   #   ###"; MG%(745); MG%(746); MG%(747); MG%(748)
                    Print "3 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1
                        Input "ENTER SELECTION (1-3) "; I
                    Loop Until I >= 1 And I <= 3

                    If I = 3 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "AHEAD BY NOT MORE THAN    "; MG%(I * 4 + 741)
                        Locate 22, 1: Input "DOWN BY NOT MORE THAN     "; MG%(I * 4 + 742)
                        Locate 22, 1: Input "BUNT RATING >=            "; MG%(I * 4 + 743)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 4 + 744)
                    End If

                Loop Until I = 3

            Case 6
                Do
                    3475 'GUARD LINES
                    Cls
                    If MG%(749) <> 999 Then MG%(749) = 999: For I = 1 To 6: MG%(749 + I) = 0: Next
                    Locate 1, 30: Color 14, 2: Print "GUARD LINES STRATEGY": Color 15, 0: Print
                    Print "IF ";: Color 15, 1: Print " A ";: Color 15, 0: Print " RUNS AHEAD"
                    Color 15, 1: Print " B ";: Color 15, 0: Print " INNING ON"
                    Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
                    Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                    Print "     A     B      C"
                    Print "1 - "; Using "##    ##    ###"; MG%(750); MG%(751); MG%(752)
                    Print "2 - "; Using "##    ##    ###"; MG%(753); MG%(754); MG%(755)
                    Print "3 - ACCEPT CHOICES"

                    Do
                        Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
                    Loop Until I >= 1 And I <= 3

                    If I = 3 Then
                        Call UpdateManagerFile(yearNumber$)
                    Else
                        I = I - 1
                        Locate 22, 1: Input "RUNS AHEAD                "; MG%(I * 3 + 750)
                        Locate 22, 1: Input "INNING ON                 "; MG%(I * 3 + 751)
                        Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 752)
                    End If
                Loop Until I = 3

            Case 7:
                Call PITCHAROUND(yearNumber$)

            Case 8:
                Call THROWHOME(yearNumber$)

            Case 9: Cls

        End Select

    Loop Until Selection% = 9 Or ExitCode% = 27

End Sub

'----------------------------------------
'       PinchRunStrategy Subroutine
'----------------------------------------
'...explanation...
Sub PinchRunStrategy (targetTeam$)

    Shared MG%()

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
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 5
        Column% = 13
        Call box0(4, 12, 9, 67, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        Select Case Selection%
            Case 1:
                Call PinchRunners(targetTeam$)
            Case 2:
                Call DefenseSubPinchRun(targetTeam$)
            Case 3:
                'PINCH RUN PITCHER
                Cls
                If MG%(803) <> 999 Then MG%(803) = 999: For I = 1 To 6: MG%(803 + I) = 0: Next
                Locate 1, 30: Color 14, 2: Print "PINCH RUN FOR PITCHER": Color 15, 0: Print
                Print "RUN RATING <= ";: Color 15, 1: Print " A ": Color 15, 0
                Color 15, 1: Print " B ";: Color 15, 0: Print " INNING ON"
                Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
                Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
                Print "SCENARIO                A     B      C"
                Print "1 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(804); MG%(805); MG%(806)
                Print "2 - TIED OR DOWN BY 1 "; Using "###    ##    ###"; MG%(807); MG%(808); MG%(809)
                Print "3 - ACCEPT CHOICES"

                Do
                    Locate 22, 1: Input "ENTER SELECTION (1-3) "; I
                Loop Until I >= 1 And I <= 3

                If I = 3 Then
                    Call UpdateManagerFile(yearNumber$)
                Else
                    I = I - 1
                    Locate 22, 1: Input "RUN RATING <=             "; MG%(I * 3 + 804)
                    Locate 22, 1: Input "INNING ON                 "; MG%(I * 3 + 805)
                    Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 806)
                End If
            Case 4: Cls '

        End Select

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub

'----------------------------------------
'       PinchRunners Subroutine
'----------------------------------------
'...explanation...
Sub PinchRunners (targetTeam$)

    Shared L%(), MG%(), progB%()
    Shared B$(), mgrX$()

    Do

        'PINCH RUNNERS
        Cls
        Print Tab(26);: Color 14, 4
        Print "PINCH RUNNING STRATEGY OPTIONS"
        Print
        Print
        Color , 0

        If MG%(771) <> 999 Then MG%(771) = 999: For I = 1 To 6: MG%(I + 771) = -1: Next

        Cls
        Locate 1, 1
        Print "#";: Color L%(11), L%(12): Print targetTeam$;
        Locate , 16: Print "RUN RATING SB   CS"
        I1 = 0

        For I = 0 To 22: Locate , 1
            If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then
                I1 = I1 + 1
            Else
                Locate I + 2 - I1, 1
                Color 2, 0
                If I < 10 Then Print Using "#"; I;: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7
                Locate , 3: Print B$(I);: Color 15, 0
                Locate , 17: Print Using "   #     ###  ###"; progB%(I, 16); progB%(I, 13); progB%(I, 14);
            End If
        Next

        Locate 1, 46
        Print "# PLAYER"

        For I = 1 To 6
            Locate , 46
            Print Using "# "; I;
            If MG%(I + 771) < 0 Then
                Print
            Else
                Print B$(MG%(I + 771))
            End If
        Next

        Locate 9, 46: Print "(7) ACCEPT CHOICES"
        Locate 10, 46: Print "(8) DELETE A CHOICE"

        Do
            Locate 11, 46: Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I
            Case 1 TO 6:
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
                MG%(I + 771) = I1

            Case 7:
                Call UpdateManagerFile(yearNumber$)

            Case 8:
                Do
                    Locate 12, 46
                    Input "SELECT CHOICE TO DELETE: "; I
                Loop Until I >= 1 And I <= 6
                MG%(I + 771) = -1

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'       DefenseSubPinchRun Subroutine
'----------------------------------------
'...explanation...
Sub DefenseSubPinchRun (targetTeam$)

    Shared B$(), mgrX$()
    Shared L%(), MG%(), progB%()
    Do
        'DEFENSIVE REPLACEMENTS
        Cls
        Locate 20, 46: Print "DEFENSIVE SUBS FOR PINCH RUNNERS"
        Locate 21, 46: Print "SCORE TIED OR DOWN BY ONE RUN"

        If MG%(778) <> 999 Then MG%(778) = 999: For I = 1 To 24: MG%(I + 778) = -1: Next

        Locate 1, 1
        Print "#";: Color L%(11), L%(12): Print targetTeam$;
        Locate , 17: Print "1 FAVG 2 FAVG 3 FAVG 4 FAVG"

        I1 = 0
        For I = 0 To 22: Locate , 1
            If B$(I) = "XXX" Or Left$(B$(I), 1) = " " Or B$(I) = "" Then
                I1 = I1 + 1
            Else
                Locate I + 2 - I1, 1
                Color 2, 0

                If I < 10 Then Print Using "#"; I;: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7
                If I > 9 Then Print mgrX$(I - 10);: Color 15, 0: If progB%(I, 21) >= 98 Then Color 0, 7

                Locate , 3: Print B$(I);: Color 15, 0
                Locate , 17: If progB%(I, 22) <> -1 Then Print Using "# ####"; progB%(I, 22); progB%(I, 26);
                Locate , 24: If progB%(I, 23) <> -1 Then Print Using "# ####"; progB%(I, 23); progB%(I, 27);
                Locate , 31: If progB%(I, 24) <> -1 Then Print Using "# ####"; progB%(I, 24); progB%(I, 28);
                Locate , 38: If progB%(I, 25) <> -1 Then Print Using "# ####"; progB%(I, 25); progB%(I, 29);
            End If
        Next

        Locate 1, 46
        Print "# PLAYER     I  SUBSTITUTE  %"

        For I = 0 To 5
            Locate , 46
            Print Using "# "; I + 1;

            If MG%(I * 4 + 779) < 0 Then
                Print
            Else
                Print Left$(B$(MG%(I * 4 + 779)), 10); Tab(59);
                Print Using "## "; MG%(I * 4 + 780);
                Print Left$(B$(MG%(I * 4 + 781)), 10); Tab(73); Using "###"; MG%(I * 4 + 782)
            End If

        Next

        Locate 9, 46: Print "(7) ACCEPT CHOICES"
        Locate 10, 46: Print "(8) DELETE A CHOICE"

        Do
            Locate 11, 46
            Input "ENTER SELECTION (1-8)"; I
        Loop Until I >= 1 And I <= 8

        Select Case I
            Case 1 TO 6:
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

                MG%(I * 4 + 779) = I1
                Locate 13, 46: Input "INNING "; I1

                MG%(I * 4 + 780) = I1
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
                MG%(I * 4 + 781) = I1

                Do
                    Locate 15, 46
                    Input "ENTER % OF TIME TO USE "; MG%(I * 4 + 782)
                Loop Until MG%(I * 4 + 782) >= 0 And MG%(I * 4 + 782) <= 100

            Case 7:
                Call UpdateManagerFile(yearNumber$)

            Case 8:
                Do
                    Locate 12, 46
                    Input "SELECT CHOICE TO DELETE: "; I
                Loop Until I >= 1 And I <= 6
                MG%(I * 4 + 779) = -1
                MG%(I * 4 + 780) = 0
                MG%(I * 4 + 781) = 0
                MG%(I * 4 + 782) = 0

        End Select

    Loop Until I = 7

End Sub

'----------------------------------------
'    ExportManagerProfile Subroutine
'----------------------------------------
'...explanation...
Sub ExportManagerProfile ()

    Shared MG%()
    Shared mgr_idx%

    Call GetDiskYear(T$, yearNumber$, mgr_idx%)

    If _FileExists(diskPaths$(3) + "MGR." + yearNumber$) Then

        Open diskPaths$(3) + "MGR." + yearNumber$ For Random As #2 Len = 2000

        For I = 0 To 999
            Field #2, I * 2 As Q$, 2 As R$(I), 2000 - I * 2 - 2 As Q$
        Next

        Get #2, mgr_idx%

        For I = 0 To 999
            MG%(I) = CVI(R$(I))
        Next

        Close 2

    End If

    If MG%(0) <> 999 Then
        ' NO MANAGER FILE WAS FOUND, THUS UNABLE TO EXPORT ONE
        Locate 10, 8
        Call CSMsgBox("NO MANAGER PROFILE EXISTS FOR THIS TEAM PRESS ANY KEY TO RETURN TO MAIN MENU", 40, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else
        If PP% = 1 Then PP% = 0: Call PrintManagerProfile(T$)

        JJ = 5
        Locate 6, 8
		Color 15, 1
        Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
        For II = 1 To JJ: 
			Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next
        Locate 7 + JJ, 8
        Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
		Locate 7

        Locate , 11: Input "ENTER 8 CHARACTER NAME FOR PROFILE"; profileFile$
        For X = 1 To 8
            If Mid$(profileFile$, X, 1) = " " Or Mid$(profileFile$, X, 1) = "." Then Mid$(profileFile$, X, 1) = "_"
        Next X

        Open diskPaths$(0) + profileFile$ + "." + "MGR" For Output As #1

        For I = 0 To 999
            Print #1, MG%(I)
        Next

        Close 1

    End If

End Sub

'----------------------------------------
'    ImportManagerProfile Subroutine
'----------------------------------------
'...explanation...
Sub ImportManagerProfile ()

    Shared MG%()

    Spec$ = diskPaths$(0) + "*.MGR"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%

    ReDim managerFiles$(0 To Count%)
    For X = 1 To Count%
        managerFiles$(X) = Space$(12)
    Next
    managerFiles$(0) = Spec$
    Call ReadFile(managerFiles$())

    If Count% = 0 Then
        'Warn user of 0 count
    Else
        Do
            Cls
            Color , 1
            Cls
            Locate , 28: Color 14
            Print "SELECT PROFILE TO IMPORT": Color 15
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
            Print "-SELECTS PROFILE";

            _MouseShow "DEFAULT"
            MAMenu managerFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

				Color 15, 1
                targetFile$ = managerFiles$(Selection%)
                Locate 2, 33: Print targetFile$

                Do
					Locate , 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

				Color 15, 0

                If UCase$(I$) = "N" Then
                    L% = 0

                    JJ = 5
                    Locate 6, 8
					Color 15, 1
                    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
                    For II = 1 To JJ: 
						Locate 6 + II, 8
                        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                    Next
                    Locate 7 + JJ, 8
                    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
					Locate 7

                    Do
						Locate , 11
						Print "IMPORT STRATEGY PERCENTAGES ONLY (YN)"
                        A$ = GetKeyPress$
                    Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

                    If UCase$(A$) = "Y" Then L% = 1

                    Open diskPaths$(0) + targetFile$ For Input As #1
                    If L% = 0 Then For I = 0 To 999: Input #1, MG%(I): Next
                    If L% = 1 Then
                        For I = 0 To 580: Input #1, L%: Next
                        For I = 581 To 769: Input #1, MG%(I): Next
                        For I = 770 To 802: Input #1, L%: Next
                        For I = 803 To 999: Input #1, MG%(I): Next
                    End If
                    Close 1

                    Call UpdateManagerFile(yearNumber$)

                End If

            End If 'Done checking for ESC

        Loop Until UCase$(I$) = "N"

    End If 'Done checking for 0 file count

End Sub

'----------------------------------------
'       UpdateManagerFile Subroutine
'----------------------------------------
'...explanation...
Sub UpdateManagerFile (yearNumber$)

    Shared R$()
    Shared MG%()
    Shared mgr_idx%

    MG%(0) = 999

    If _FileExists(diskPaths$(3) + "MGR." + yearNumber$) Then
        Open diskPaths$(3) + "MGR." + yearNumber$ For Random As #2 Len = 2000

        For I = 0 To 999
            Field #2, I * 2 As Q$, 2 As R$(I), 2000 - I * 2 - 2 As Q$
        Next

        For I = 0 To 999
            LSet R$(I) = MKI$(MG%(I))
        Next

        Put #2, mgr_idx%

        Close 2

    End If

End Sub

'----------------------------------------
'           PHBATTER Subroutine
'----------------------------------------
'...explanation...
Sub PHBATTER (yearNumber$)

    Shared MG%()

    Do
        'PINCH HIT FOR THE BATTER
        If MG%(847) <> 999 Then MG%(847) = 999: For I = 848 To 865: MG%(I) = 0: Next
        Cls
        Locate 1, 26: Color 14, 4: Print "PINCH HITTING FOR THE BATTER": Color 15, 0
        Print
        Print "PINCH HIT FOR THE BATTER FROM ";: Color 15, 1: Print " A ";: Color 15, 0
        Print " INNING ON"
        Print "IF DOWN BY ";: Color 15, 1: Print " B ";: Color 15, 0: Print " OR MORE RUNS AND"
        Color 15, 1: Print " C ";: Color 15, 0: Print " RUNNERS ON BASE AND"
        Print "SLUGGING PCT IS LESS THAN ";: Color 15, 1: Print " D ";: Color 15, 0
        Print "AND BATTING AVERAGE IS LESS THAN ";: Color 15, 1: Print " E "
        Print " F ";: Color 15, 0: Print " % OF THE TIME"
        Print
        Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
        Print "SCENARIO        A      B      C      D       E     F"
        For I = 0 To 2
            Print Using "   #           ##     ##      #     ###    ###   ###"; I + 1; MG%(I * 6 + 848); MG%(I * 6 + 849); MG%(I * 6 + 850); MG%(I * 6 + 851); MG%(I * 6 + 852); MG%(I * 6 + 853)
        Next
        Print
        Print "   4 - ACCEPT CHOICES"
        Print

        Do
            Locate 17, 1
            Input "ENTER SELECTION (1-4) "; I
        Loop Until I >= 1 And I <= 4

        If I = 4 Then
            Call UpdateManagerFile(yearNumber$)
        Else
            I = I - 1
            Input "INNING TO BEGIN PINCH HITTING FOR POSITION PLAYERS "; MG%(I * 6 + 848)
            Input "DOWN BY HOW MANY RUNS "; MG%(I * 6 + 849)
            Input "RUNNERS ON BASE "; MG%(I * 6 + 850)
            Input "SLUGGING PCT LESS THAN "; MG%(I * 6 + 851)
            Input "BATTING AVERAGE LESS THAN "; MG%(I * 6 + 852)
            Input "PERCENT OF TIME "; MG%(I * 6 + 853)
        End If

    Loop Until I = 4

End Sub

'----------------------------------------
'           PHPITCHER Subroutine
'----------------------------------------
'...explanation...
Sub PHPITCHER (yearNumber$)

    Shared MG%()

    Do
        'PINCH HIT FOR THE PITCHER
        If MG%(590) <> 999 Then MG%(590) = 999: For I = 591 To 608: MG%(I) = 0: Next
        Cls
        Locate 1, 26: Color 14, 4: Print "PINCH HITTING FOR THE PITCHER": Color 15, 0
        Print
        Print "PINCH HIT FOR THE PITCHER FROM ";: Color 15, 1: Print " A ";: Color 15, 0
        Print " INNING ON"
        Print "IF DOWN BY ";: Color 15, 1: Print " B ";: Color 15, 0: Print " OR MORE RUNS AND"
        Color 15, 1: Print " C ";: Color 15, 0: Print " OR MORE RUNNERS ON BASE AND"
        Color 15, 1: Print " D ";: Color 15, 0
        Print " OUTS OR LESS AND PITCHERS BATTING AVERAGE IS LESS THAN ";: Color 15, 1: Print " E "
        Print " F ";: Color 15, 0: Print " % OF THE TIME"
        Print
        Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
        Print "SCENARIO        A      B      C      D     E     F"
        For I = 0 To 2
            Print Using "   #           ##     ##      #      #    ###   ###"; I + 1; MG%(I * 6 + 591); MG%(I * 6 + 592); MG%(I * 6 + 593); MG%(I * 6 + 594); MG%(I * 6 + 595); MG%(I * 6 + 596)
        Next
        Print
        Print "   4 - ACCEPT CHOICES"
        Print

        Do
            Locate 17, 1
            Input "ENTER SELECTION (1-4) "; I
        Loop Until I >= 1 And I <= 4

        If I = 4 Then
            Call UpdateManagerFile(yearNumber$)
        Else
            I = I - 1
            Input "ENTER INNING TO BEGIN PINCH HITTING FOR PITCHER "; MG%(I * 6 + 591)
            Input "DOWN BY HOW MANY RUNS "; MG%(I * 6 + 592)
            Input "RUNNERS ON BASE "; MG%(I * 6 + 593)
            Input "NUMBER OF OUTS OR LESS "; MG%(I * 6 + 594)
            Input "PITCHER HITS LESS THAN "; MG%(I * 6 + 595)
            Input "PERCENT OF TIME "; MG%(I * 6 + 596)
        End If

    Loop Until I = 4

End Sub

'----------------------------------------
'           PITCHAROUND Subroutine
'----------------------------------------
'...explanation...
Sub PITCHAROUND (yearNumber$)

    Shared MG%()

    Do
        'PITCH AROUND
        Cls

        If MG%(756) <> 999 Then MG%(756) = 999: For I = 1 To 9: MG%(756 + I) = 0: Next

        Locate 1, 30: Color 14, 2: Print "PITCH AROUND STRATEGY": Color 15, 0: Print
        Print "SLUGGING PCT >= ";: Color 15, 1: Print " A ": Color 15, 0
        Color 15, 1: Print " B ";: Color 15, 0: Print " INNING ON"
        Color 15, 1: Print " C ";: Color 15, 0: Print " % OF THE TIME"
        Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
        Print "SCENARIO                                               A     B      C"
        Print "1 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(757); MG%(758); MG%(759)
        Print "2 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(760); MG%(761); MG%(762)
        Print "3 - 1ST BASE OPEN, BATTER NOT TYING OR WINNING RUN   "; Using "###    ##    ###"; MG%(763); MG%(764); MG%(765)
        Print "4 - ACCEPT CHOICES"

        Do
            Locate 22, 1
            Input "ENTER SELECTION (1-4) "; I
        Loop Until I >= 1 And I <= 4

        If I = 4 Then
            Call UpdateManagerFile(yearNumber$)
        Else
            I = I - 1
            Locate 22, 1: Input "SLUGGING PCT >=           "; MG%(I * 3 + 757)
            Locate 22, 1: Input "INNING ON                 "; MG%(I * 3 + 758)
            Locate 22, 1: Input "PERCENT OF TIME           "; MG%(I * 3 + 759)
        End If

    Loop Until I = 4

End Sub

'----------------------------------------
'           Input_PITSTRAT Subroutine
'----------------------------------------
'...explanation...
Sub Input_PITSTRAT (yearNumber$)

    Shared MG%()
    Shared yesNo$()

    Do
        'PITCHING STRATEGY
        Cls

        If MG%(581) <> 999 Then MG%(581) = 999: For I = 582 To 587: MG%(I) = 0: Next: MG%(588) = 8: MG%(589) = 0
        If MG%(588) < 8 Then MG%(588) = 8
        If MG%(588) > 9 Then MG%(588) = 9

        Print Tab(31);: Color 14, 4: Print "CLOSING STRATEGY": Color 15, 0
        Print
        Color 12: Print "STRATEGY                                        % OF TIME"
        Color 14: Print "(1) ";: Color 15: Print "INNING TO BEGIN CLOSING STRATEGY (8TH OR 9TH ON) "; MG%(588)
        Color 14: Print "(2) ";: Color 15: Print "USE BOTH SETUP RELIEVER AND CLOSER "; yesNo$(MG%(589))
        Print
        Color 14: Print "SITUATIONS TO BRING IN CLOSER AND/OR SETUP MAN"
        Color 14: Print "(3) ";: Color 15: Print Using "START INNING IF SAVE OPPORTUNITY               ###"; MG%(582)
        Color 14: Print "(4) ";: Color 15: Print Using "START INNING IF AHEAD                          ###"; MG%(583)
        Color 14: Print "(5) ";: Color 15: Print Using "START INNING IF TIED OR AHEAD                  ###"; MG%(584)
        Color 14: Print "(6) ";: Color 15: Print Using "IF CURRENT PITCHER TIRED AND AHEAD             ###"; MG%(585)
        Color 14: Print "(7) ";: Color 15: Print Using "IF CURRENT PITCHER TIRED AND TIED OR AHEAD     ###"; MG%(586)
        Color 14: Print "(8) ";: Color 15: Print Using "WHEN SAVE OPPORTUNITY OCCURS                   ###"; MG%(587)
        Color 14: Print "(9) ";: Color 15: Print "ACCEPT CHOICES"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 9

        If I = 9 Then
            Call UpdateManagerFile(yearNumber$)
        Else
            If I = 1 Then
                Locate 21, 1: Input "ENTER INNING "; MG%(588)
            Else
                If I = 2 Then
                    MG%(589) = 1 - MG%(589)
                Else
                    Do
                        Locate 21, 1: Input "% OF TIME TO USE "; I1
                    Loop Until I1 >= 0 And I1 <= 100
                    MG%(I - 2 + 581) = I1
                End If
            End If
        End If

    Loop Until I = 9

End Sub

'----------------------------------------
'           THROWHOME Subroutine
'----------------------------------------
'...explanation...
Sub THROWHOME (yearNumber$)
    'THROW HOME UNDER

    Shared MG%()

    Do
        Cls
        If MG%(766) <> 999 Then MG%(766) = 999: For I = 1 To 4: MG%(766 + I) = 0: Next

        Locate 1, 18: Color 14, 2: Print "DON'T THROW HOME WITH RUNNER TRYING TO SCORE": Color 15, 0: Print
        Print "FROM ";: Color 15, 1: Print " A ";: Color 15, 0: Print " INNING ON"
        Color 15, 1: Print " B ";: Color 15, 0: Print " OR MORE RUNS AHEAD"
        Print Tab(32);: Color 14: Print "TABLE OF VALUES": Color 15
        Print "      A      B"
        Print Using "(1)  ##    ###"; MG%(767); MG%(768)
        Print Using "(2)  ##    ###"; MG%(769); MG%(770)
        Print "(3) - ACCEPT CHOICES"

        Do
            Locate 22, 1
            Input "ENTER SELECTION (1-3) "; I
        Loop Until I >= 1 And I <= 3

        If I = 3 Then
            Call UpdateManagerFile(yearNumber$)
        Else
            I = I - 1

            Locate 22, 1: Input "INNING ON TO USE          "; MG%(I * 2 + 767)
            Locate 22, 1: Input "AHEAD BY                  "; MG%(I * 2 + 768)
        End If

    Loop Until I = 3

End Sub

'------------------------------
'   BASERUN Subroutine
'------------------------------
' ...explanation...
Sub BASERUN (yearNumber$)

    Shared R$()
    Shared MG%()
    Shared mgr_idx%

    Do
        'BASE RUNNING STRATEGY
        Cls

        If MG%(810) <> 999 Then MG%(810) = 999: For I = 1 To 36: MG%(810 + I) = 0: Next

        Locate 1, 30
        Color 14, 2: Print "BASE RUNNING STRATEGY"
        Color 15, 0: Print
        Print Tab(32);: Color 14: Print "TABLE OF VALUES"
        Color 15
        Print "SCENARIO                            %       %          %          %"
        Print "                                  SAFE    NORMAL   AGGRESSIVE   TOTAL"
        Print " 1 - INNING 1-3, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(811); MG%(812); MG%(813); MG%(811) + MG%(812) + MG%(813)
        Print " 2 - INNING 1-3, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(814); MG%(815); MG%(816); MG%(816) + MG%(815) + MG%(814)
        Print " 3 - INNING 1-3, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(817); MG%(818); MG%(819); MG%(819) + MG%(818) + MG%(817)
        Print " 4 - INNING 4-6, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(820); MG%(821); MG%(822); MG%(820) + MG%(821) + MG%(822)
        Print " 5 - INNING 4-6, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(823); MG%(824); MG%(825); MG%(823) + MG%(824) + MG%(825)
        Print " 6 - INNING 4-6, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(826); MG%(827); MG%(828); MG%(826) + MG%(827) + MG%(828)
        Print " 7 - INNING 7-9, WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(829); MG%(830); MG%(831); MG%(829) + MG%(830) + MG%(831)
        Print " 8 - INNING 7-9, WHEN TIED        "; Using "###     ###        ###          ###"; MG%(832); MG%(833); MG%(834); MG%(832) + MG%(833) + MG%(834)
        Print " 9 - INNING 7-9, WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(835); MG%(836); MG%(837); MG%(835) + MG%(836) + MG%(837)
        Print "10 - INNING 9- , WHEN AHEAD       "; Using "###     ###        ###          ###"; MG%(838); MG%(839); MG%(840); MG%(838) + MG%(839) + MG%(840)
        Print "11 - INNING 9- , WHEN TIED        "; Using "###     ###        ###          ###"; MG%(841); MG%(842); MG%(843); MG%(841) + MG%(842) + MG%(843)
        Print "12 - INNING 9- , WHEN BEHIND      "; Using "###     ###        ###          ###"; MG%(844); MG%(845); MG%(846); MG%(844) + MG%(845) + MG%(846)
        Print "13 - ACCEPT CHOICES"

        Do
            Locate 22, 1: Input "ENTER SELECTION (1-13) "; I
        Loop Until I >= 1 And I <= 13

        If I = 13 Then
            Open diskPaths$(3) + "MGR." + yearNumber$ For Random As #2 Len = 2000
            For J = 0 To 999
                Field #2, J * 2 As Q$, 2 As R$(J), 2000 - J * 2 - 2 As Q$
            Next
            For J = 0 To 999: LSet R$(J) = MKI$(MG%(J)): Next
            Put #2, mgr_idx%
            Close 2
        Else
            I = I - 1
            Locate 22, 1: Input "SAFE                   "; MG%(I * 3 + 811)
            Locate 22, 1: Input "NORMAL                 "; MG%(I * 3 + 812)
            Locate 22, 1: Input "AGGRESSIVE             "; MG%(I * 3 + 813)
        End If

    Loop Until I = 13

End Sub
