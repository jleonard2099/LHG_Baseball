'----------------------------------------
'
'----------------------------------------
'
Sub POSSRT ()

    _Title "Full Count Baseball - Sort By Position"

    ReDim menuChoices$(1 To 3)

    Do

        Color 15, 0
        Cls

        Call TITLE

        menuChoices$(1) = "CREATE POSITION SEARCH FILE   "
        menuChoices$(2) = "SEARCH FOR PLAYERS BY POSITION"
        menuChoices$(3) = "RETURN TO MAIN MENU           "
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

                Case 1: Call POSCR

                Case 2: Call SearchPlayers

                Case 3: Cls

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       SearchPlayers Subroutine
'----------------------------------------
'...explanation...
Sub SearchPlayers ()

    Cls

    positionFile$ = ChooseFile$ (diskPaths$(0) + "*.POS", "POSITION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do

            Cls

            SS = 0

            Color 11: Locate 5, 30: Print "S E L E C T"

            Locate 7, 28
            Color 12: Print "0. ";
            Color 7: Print "Catchers"
            Locate , 28
            Color 12: Print "1. ";
            Color 7: Print "First Basemen"
            Locate , 28
            Color 12: Print "2. ";
            Color 7: Print "Second Basemen"
            Locate , 28
            Color 12: Print "3. ";
            Color 7: Print "Shortstops"
            Locate , 28
            Color 12: Print "4. ";
            Color 7: Print "Third Basemen"
            Locate , 28
            Color 12: Print "5. ";
            Color 7: Print "Left Fielders"
            Locate , 28
            Color 12: Print "6. ";
            Color 7: Print "Center Fielders"
            Locate , 28
            Color 12: Print "7. ";
            Color 7: Print "Right Fielders"
            Locate , 28
            Color 12: Print "8. ";
            Color 7: Print "Pitchers"
            Locate , 28
            Color 12: Print "9. ";
            Color 7: Print "Return To Main Menu"

            Do
                Z$ = GetKeyPress$
                Z = Val(Z$)
            Loop Until Z >= 0 And Z <= 9 Or Z$ = Chr$(27)

            If Z$ <> Chr$(27) Then

                Select Case Z
                    Case 0
                        PS = 2
                        PS$ = "CATCHERS"
                    Case 1
                        PS = 3
                        PS$ = "FIRST BASEMEN"
                    Case 2
                        PS = 4
                        PS$ = "SECOND BASEMEN"
                    Case 3
                        PS = 6
                        PS$ = "SHORTSTOPS"
                    Case 4
                        PS = 5
                        PS$ = "THIRD BASEMEN"
                    Case 5
                        PS = 7
                        PS$ = "LEFT FIELDERS"
                    Case 6
                        PS = 8
                        PS$ = "CENTER FIELDERS"
                    Case 7
                        PS = 9
                        PS$ = "RIGHT FIELDERS"
                    Case 8:
                        PS = 1
                        Call SortPitchers(positionFile$)
                    Case 9:
                        Cls '

                End Select

                If Z <> 9 Then

                    Color 15, 0
                    Cls

                    Do
                        Locate 18, 28
                        Color 12: Print "V";
                        Color 7: Print "iew or ";
                        Color 12: Print "P";
                        Color 7: Print "rint"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

                    Select Case UCase$(I$)
                        Case "P": Call PrintSearchResults(PS, PS$, positionFile$)
                        Case "V": Call ViewSearchResults(PS, PS$, positionFile$)
                    End Select

                End If 'Done looking for Z = 9

            End If 'Done checking for ESC

        Loop Until Z = 9

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'      ViewSearchResults Subroutine
'----------------------------------------
'...explanation...
Sub ViewSearchResults (PS, PS$, positionFile$)

    Shared SS
    Shared NN$

    Shared batterNames$(), TYN$()
    Shared batterRatings()

    Dim TM(1 To 40)
    Dim A$(1 To 40), BB$(23)

    Open diskPaths$(0) + positionFile$ + ".POS" For Input As #1

    For J = 1 To 40
        Input #1, TM(J), A$(J), TYN$(J)
    Next

    Close #1

    Cls

    Locate 1, 35
    Color 15: Print PS$
    Locate 3
    Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"

    For J = 1 To 40

        If TM(J) <> 0 Then

            Call ReadTeam(TYN$(J), T$, TM(J))

            For I = 0 To 22

                If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

                    If batterRatings(I, 22) = PS Then I2 = 33
                    If batterRatings(I, 23) = PS Then I2 = 35
                    If batterRatings(I, 24) = PS Then I2 = 37
                    If batterRatings(I, 25) = PS Then I2 = 39

                    If I2 = 33 Or I2 = 35 Or I2 = 37 Or I2 = 39 Then

                        SS = SS + 1

                        If (SS Mod 19 = 1) Then

                            Locate 23
                            Color 14: Print "HIT ANY KEY TO CONTINUE"
                            I$ = GetKeyPress$

                            Cls

                            Locate 1, 35
                            Color 15: Print PS$
                            Locate 3
                            Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"

                        End If

                        If batterRatings(I, 22) = PS Then
                            Color 12
                            Print Left$(batterNames$(I), 10);
                        Else
                            Color 2
                            Print Left$(batterNames$(I), 10);
                        End If

                        Color 3
                        Print Tab(12); Left$(T$, 2); NN$;

                        If batterRatings(I, 0) = -1 Then BB$(I) = "L"
                        If batterRatings(I, 0) = 1 Then BB$(I) = "R"
                        If batterRatings(I, 0) = 0 Then BB$(I) = "S"
                        If batterRatings(I, 0) = 2 Then BB$(I) = "RP"
                        If batterRatings(I, 0) = -2 Then BB$(I) = "LP"

                        Color 7: Print Tab(19); BB$(I); Using "#"; batterRatings(I, I2);

                        Print Tab(22); Using "###"; batterRatings(I, 3);
                        Print Tab(26); Using "###"; batterRatings(I, 4);
                        Print Tab(31); Using "###"; batterRatings(I, 6);
                        Print Tab(35); Using "##"; batterRatings(I, 7);
                        Print Tab(38); Using "##"; batterRatings(I, 8);
                        Print Tab(41); Using "##"; batterRatings(I, 9);
                        Print Tab(44); Using "###"; batterRatings(I, 10);
                        Print Tab(48); Using "###"; batterRatings(I, 5);
                        Print Tab(52); Using "###"; batterRatings(I, 11);
                        Print Tab(56); Using "###"; batterRatings(I, 12);
                        Print Tab(59); Using "###"; batterRatings(I, 13);
                        Print Tab(63); Using "##"; batterRatings(I, 14);

                        I2 = 0
                        I3 = 0
                        I4 = 0

                        If batterRatings(I, 4) > 0 Then
                            I2 = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                            I3 = Int(((batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)) / batterRatings(I, 4) + .0005) * 1000)
                            I4 = Int(((batterRatings(I, 6) + batterRatings(I, 11)) / (batterRatings(I, 11) + batterRatings(I, 4)) + .0005) * 1000)
                        End If

                        If batterRatings(I, 4) = 0 Or I2 = 0 Then Print Tab(66); ".000";

                        If I2 > 0 And I2 < 100 Then Print Tab(66); ".0";: Print Using "##"; I2;
                        If I2 >= 100 Then Print Tab(66); ".";: Print Using "###"; I2;

                        If batterRatings(I, 4) = 0 Or I3 = 0 Then Print Tab(71); ".000";

                        If I3 > 0 And I3 < 100 Then Print Tab(71); ".0";: Print Using "##"; I3;
                        If I3 >= 100 Then Print Tab(71); ".";: Print Using "###"; I3;

                        If batterRatings(I, 4) = 0 Or I4 = 0 Then Print Tab(76); ".000";

                        If I4 > 0 And I4 < 100 Then Print Tab(76); ".0";: Print Using "##"; I4;
                        If I4 >= 100 Then Print Tab(76); ".";: Print Using "###"; I4

                    End If

                End If

            Next I

        End If 'Done checking TM(J)

    Next J

    Locate 23
    Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'      PrintSearchResults Subroutine
'----------------------------------------
'...explanation...
Sub PrintSearchResults (PS, PS$, positionFile$)

    Shared SS
    Shared NN$

    Shared batterRatings()
    Shared TYN$()

    Dim TM(40)
    Dim A$(1 To 40), BB$(23)

    Open diskPaths$(0) + positionFile$ + ".POS" For Input As #1

    For J = 1 To 40:
        Input #1, TM(J), A$(J), TYN$(J)
    Next

    Close #1

    LPrint Tab(55); PS$
    LPrint
    LPrint "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"

    For J = 1 To 40

        If TM(J) <> 0 Then

            Call ReadTeam(TYN$(J), T$, TM(J))

            For I = 0 To 22

                If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

                    If batterRatings(I, 22) = PS Or batterRatings(I, 23) = PS Or batterRatings(I, 24) = PS Or batterRatings(I, 25) = PS Then

                        SS = SS + 1

                        If (SS Mod 50 = 1) Then
                            LPrint Chr$(12)
                            LPrint Tab(50); PPS$
                            LPrint
                            LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        If batterRatings(I, 22) = PS Then
                            LPrint batterNames$(I);
                        Else
                            LPrint Tab(3); batterNames$(I);
                        End If

                        LPrint Tab(16); Left$(T$, 2); NN$;

                        If batterRatings(I, 0) = -1 Then BB$(I) = "L"
                        If batterRatings(I, 0) = 1 Then BB$(I) = "R"
                        If batterRatings(I, 0) = 0 Then BB$(I) = "S"

                        LPrint Tab(34); BB$(I);
                        LPrint Tab(39); Using "###"; batterRatings(I, 3);
                        LPrint Tab(45); Using "###"; batterRatings(I, 4);
                        LPrint Tab(52); Using "###"; batterRatings(I, 6);
                        LPrint Tab(58); Using "##"; batterRatings(I, 7);
                        LPrint Tab(63); Using "##"; batterRatings(I, 8);
                        LPrint Tab(68); Using "##"; batterRatings(I, 9);
                        LPrint Tab(73); Using "###"; batterRatings(I, 10);
                        LPrint Tab(80); Using "###"; batterRatings(I, 5);
                        LPrint Tab(85); Using "###"; batterRatings(I, 11);
                        LPrint Tab(91); Using "###"; batterRatings(I, 12);
                        LPrint Tab(96); Using "###"; batterRatings(I, 13);
                        LPrint Tab(102); Using "##"; batterRatings(I, 14);

                        I2 = 0
                        I3 = 0
                        I4 = 0

                        If batterRatings(I, 4) > 0 Then
                            I2 = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                            I3 = Int(((batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)) / batterRatings(I, 4) + .0005) * 1000)
                            I4 = Int(((batterRatings(I, 6) + batterRatings(I, 11)) / (batterRatings(I, 11) + batterRatings(I, 4)) + .0005) * 1000)
                        End If

                        If batterRatings(I, 4) = 0 Or I2 = 0 Then LPrint Tab(107); ".000";

                        If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;
                        If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;

                        If batterRatings(I, 4) = 0 Or I3 = 0 Then LPrint Tab(114); ".000";

                        If I3 > 0 And I3 < 100 Then LPrint Tab(114); ".0";: LPrint Using "##"; I3;
                        If I3 >= 100 Then LPrint Tab(114); ".";: LPrint Using "###"; I3;

                        If batterRatings(I, 4) = 0 Or I4 = 0 Then LPrint Tab(121); ".000";

                        If I4 > 0 And I4 < 100 Then LPrint Tab(121); ".0";: LPrint Using "##"; I4;
                        If I4 >= 100 Then LPrint Tab(121); ".";: LPrint Using "###"; I4

                    End If

                End If 'Done checking for invalid chars

            Next I

        End If

    Next J

    LPrint Chr$(12)

End Sub

'----------------------------------------
'       SortPitchers Subroutine
'----------------------------------------
'...explanation...
Sub SortPitchers (positionFile$)

    SS = 0

    Cls

    Color 11
    Locate 5, 30: Print "S E L E C T"

    Locate 7, 16
    Color 12: Print "1. ";
    Color 7: Print "Pitchers With Less Than 10 Games Started"
    Locate , 16
    Color 12: Print "2. ";
    Color 7: Print "Pitchers With More Than 10 Games Started"
    Locate , 16
    Color 12: Print "3. ";
    Color 7: Print "Right-Handed Pitchers"
    Locate , 16
    Color 12: Print "4. ";
    Color 7: Print "Left-Handed Pitchers"

    Do
        I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 1 And I <= 4

    Select Case I

        Case 1:
            PPS = 10
            PC = 1
            PPS$ = "PITCHERS WITH LESS THAN 10 GAMES STARTED"
            Call Pitchers10_Games(PC, PPS$, positionFile$)

        Case 2:
            PPS = 10
            PC = 2
            PPS$ = "PITCHERS WITH MORE THAN 10 GAMES STARTED"
            Call Pitchers10_Games(PC, PPS$, positionFile$)

        Case 3:
            THR = 1
            PPS$ = "RIGHT-HANDED PITCHERS"
            Call PitchersHanded(THR, PPS$, positionFile$)

        Case 4:
            THR = 2
            PPS$ = "LEFT-HANDED PITCHERS"
            Call PitchersHanded(THR, PPS$, positionFile$)

    End Select

End Sub

'----------------------------------------
'      Pitchers10_Games Subroutine
'----------------------------------------
'...explanation...
Sub Pitchers10_Games (PC, PPS$, positionFile$)

    Shared SS
    Shared NN$

    Shared pitcherNames$(), PB$(), TYN$()
    Shared teamRatings%(), pitcherRatings()

    Dim A$(1 To 40)
    Dim TM(40)

    Do

        Locate 18, 28

        Color 12: Print "V";
        Color 7: Print "iew or ";
        Color 12: Print "P";
        Color 7: Print "rint"
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

    Select Case UCase$(I$)

        Case "P":
            Call Pitchers10_Print(PC, PPS$, positionFile$)

        Case "V":

            Open diskPaths$(0) + positionFile$ + ".POS" For Input As #1

            For J = 1 To 40:
                Input #1, TM(J), A$(J), TYN$(J)
            Next

            Close #1

            Cls
            Color 15
            Locate 1, 20: Print PPS$

            Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

            For J = 1 To 40

                If TM(J) <> 0 Then

                    Call ReadTeam(TYN$(J), T$, TM(J))

                    For I = 0 To 21

                        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                            If PC = 1 And pitcherRatings(I, 5) < PPS Or pitcherRatings(I, 5) > PPS Then
                                SS = SS + 1

                                If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Or SS = 235 Or SS = 253 Or SS = 271 Or SS = 289 Or SS = 307 Or SS = 325 Or SS = 343 Or SS = 361 Or SS = 379 Or SS = 397 Or SS = 415 Then

                                    Locate 23
                                    Color 14: Print "HIT ANY KEY TO CONTINUE"
                                    I$ = GetKeyPress$

                                    Cls

                                    Color 15
                                    Locate 1, 20: Print PPS$

                                    Color 11
                                    Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

                                End If

                                Color 12: Print Left$(pitcherNames$(I), 10);
                                Color 3: Print Tab(12); Left$(T$, 2); NN$

                                If pitcherRatings(I, 0) = 1 Then PB$(I) = "R"
                                If pitcherRatings(I, 0) = -1 Then PB$(I) = "L"

                                Color 7: Print Tab(22); PB$(I);

                                Print Tab(25); Using "###"; pitcherRatings(I, 4);
                                Print Tab(30); Using "##"; pitcherRatings(I, 1);
                                Print Tab(34); Using "##"; pitcherRatings(I, 2);
                                Print Tab(39); Using "##"; pitcherRatings(I, 3);
                                Print Tab(43); Using "##"; pitcherRatings(I, 5);
                                Print Tab(47); Using "###"; pitcherRatings(I, 6);
                                Print Tab(52); Using "###"; pitcherRatings(I, 7);
                                Print Tab(56); Using "###"; pitcherRatings(I, 8);
                                Print Tab(61); Using "###"; pitcherRatings(I, 9);
                                Print Tab(65); Using "##.##"; pitcherRatings(I, 10) / 100;
                                Print Tab(73); Using "##"; pitcherRatings(I, 13)

                            End If

                        End If

                    Next I

                End If

            Next J

            Locate 23
            Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

    End Select

End Sub

'----------------------------------------
'    PitchersHanded Subroutine
'----------------------------------------
'...explanation...
Sub PitchersHanded (THR, PPS$, positionFile$)

    Shared SS
    Shared NN$

    Shared pitcherNames$(), PB$(), TYN$()
    Shared pitcherRatings()

    Dim A$(1 To 40)
    Dim TM(40)

    Locate 18, 28
    Color 12: Print "V";
    Color 7: Print "iew or ";
    Color 12: Print "P";
    Color 7: Print "rint"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

    Open diskPaths$(0) + positionFile$ + ".POS" For Input As #1

    For J = 1 To 40
        Input #1, TM(J), A$(J), TYN$(J)
    Next

    Close #1

    Select Case UCase$(I$)

        Case "V":
            Cls
            Color 15
            Locate 1, 35: Print PPS$

            Color 11:
            Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

            For J = 1 To 40

                If TM(J) <> 0 Then

                    Call ReadTeam(TYN$(J), T$, TM(J))

                    For I = 0 To 21

                        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                            If THR = 1 And pitcherRatings(I, 0) = 1 Or pitcherRatings(I, 0) = -1 Then

                                SS = SS + 1

                                If (SS Mod 19 = 1) Then
                                    Color 14
                                    Locate 23: Print "HIT ANY KEY TO CONTINUE"
                                    I$ = GetKeyPress$

                                    Cls

                                    Color 15
                                    Locate 1, 20: Print PPS$

                                    Color 11
                                    Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
                                End If

                                Color 12: Print Left$(pitcherNames$(I), 10);
                                Color 3: Print Tab(12); Left$(T$, 2); NN$;

                                If pitcherRatings(I, 0) = 1 Then PB$(I) = "R"
                                If pitcherRatings(I, 0) = -1 Then PB$(I) = "L"

                                Color 7: Print Tab(22); PB$(I);

                                Print Tab(25); Using "###"; pitcherRatings(I, 4);
                                Print Tab(30); Using "##"; pitcherRatings(I, 1);
                                Print Tab(34); Using "##"; pitcherRatings(I, 2);
                                Print Tab(39); Using "##"; pitcherRatings(I, 3);
                                Print Tab(43); Using "##"; pitcherRatings(I, 5);
                                Print Tab(47); Using "###"; pitcherRatings(I, 6);
                                Print Tab(52); Using "###"; pitcherRatings(I, 7);
                                Print Tab(56); Using "###"; pitcherRatings(I, 8);
                                Print Tab(61); Using "###"; pitcherRatings(I, 9);
                                Print Tab(65); Using "##.##"; pitcherRatings(I, 10) / 100;
                                Print Tab(73); Using "##"; pitcherRatings(I, 13)

                            End If

                        End If

                    Next I

                End If

            Next J


            Color 14
            Locate 23: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

        Case "P":
            LPrint Tab(50); PPS$
            LPrint
            LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

            For J = 1 To 40

                If TM(J) <> 0 Then

                    Call ReadTeam(TYN$(J), T$, TM(J))

                    For I = 0 To 21
                        If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                            If THR = 1 And pitcherRatings(I, 0) = 1 Or pitcherRatings(I, 0) = -1 Then

                                SS = SS + 1

                                If (SS Mod 50 = 1) Then
                                    LPrint Chr$(12)
                                    LPrint Tab(50); PPS$
                                    LPrint
                                    LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                                End If

                                LPrint pitcherNames$(I);
                                LPrint Tab(19); Left$(T$, 2); NN$;

                                If pitcherRatings(I, 0) = 1 Then PB$(I) = "R"

                                If pitcherRatings(I, 0) = -1 Then PB$(I) = "L"

                                LPrint Tab(35); PB$(I);
                                LPrint Tab(39); Using "###"; pitcherRatings(I, 4);
                                LPrint Tab(45); Using "##"; pitcherRatings(I, 1);
                                LPrint Tab(50); Using "##"; pitcherRatings(I, 2);
                                LPrint Tab(56); Using "##"; pitcherRatings(I, 3);
                                LPrint Tab(62); Using "##"; pitcherRatings(I, 5);
                                LPrint Tab(68); Using "###"; pitcherRatings(I, 6);
                                LPrint Tab(73); Using "###"; pitcherRatings(I, 7);
                                LPrint Tab(79); Using "###"; pitcherRatings(I, 8);
                                LPrint Tab(85); Using "###"; pitcherRatings(I, 9);
                                LPrint Tab(90); Using "##.##"; pitcherRatings(I, 10) / 100;
                                LPrint Tab(99); Using "##"; pitcherRatings(I, 13)

                            End If

                        End If

                    Next I

                End If

            Next J

            LPrint Chr$(12)

    End Select

End Sub


'----------------------------------------
'       Pitchers10_Print Subroutine
'----------------------------------------
'...explanation...
Sub Pitchers10_Print (PC, PPS$, positionFile$)

    Shared SS
    Shared NN$

    Shared pitcherNames$(), PB$(), TYN$()
    Shared pitcherRatings()

    Dim A$(1 To 40)
    Dim TM(40)

    Open diskPaths$(0) + positionFile$ + ".POS" For Input As #1
    
    For J = 1 To 40: 
        Input #1, TM(J), A$(J), TYN$(J)
    Next
    
    Close #1

    LPrint Tab(50); PPS$
    LPrint
    LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

    For J = 1 To 40

        If TM(J) <> 0 Then

            Call ReadTeam(TYN$(J), T$, TM(J))

            For I = 0 To 21

                If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                    If PC = 1 And pitcherRatings(I, 5) < PPS Or pitcherRatings(I, 5) > PPS Then

                        SS = SS + 1

                        If (SS Mod 50 = 1) Then
                            LPrint Chr$(12)
                            LPrint Tab(50); PPS$
                            LPrint
                            LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        LPrint pitcherNames$(I);
                        LPrint Tab(19); Left$(T$, 2); NN$;

                        If pitcherRatings(I, 0) = 1 Then PB$(I) = "R"
                        If pitcherRatings(I, 0) = -1 Then PB$(I) = "L"

                        LPrint Tab(35); PB$(I);
                        LPrint Tab(39); Using "###"; pitcherRatings(I, 4);
                        LPrint Tab(45); Using "##"; pitcherRatings(I, 1);
                        LPrint Tab(50); Using "##"; pitcherRatings(I, 2);
                        LPrint Tab(56); Using "##"; pitcherRatings(I, 3);
                        LPrint Tab(62); Using "##"; pitcherRatings(I, 5);
                        LPrint Tab(68); Using "###"; pitcherRatings(I, 6);
                        LPrint Tab(73); Using "###"; pitcherRatings(I, 7);
                        LPrint Tab(79); Using "###"; pitcherRatings(I, 8);
                        LPrint Tab(85); Using "###"; pitcherRatings(I, 9);
                        LPrint Tab(90); Using "##.##"; pitcherRatings(I, 10) / 100;
                        LPrint Tab(99); Using "##"; pitcherRatings(I, 13)
                    End If

                End If

            Next I

        End If

    Next J

    LPrint Chr$(12)

End Sub

