'----------------------------------------
'          POSSRT Subroutine
'----------------------------------------
'This subroutine serves as a menu for the
'end user to manage files that help find
'and review players by position
Sub POSSRT ()

    _Title "Full Count Baseball - Sort By Position"

    ReDim menuChoices$(1 To 3)

    Shared currPosition

    Do

        Color 15, 0
        Cls

        Call TITLE

        menuChoices$(1) = "CREATE POSITION SEARCH FILE   "
        menuChoices$(2) = "SEARCH FOR PLAYERS BY POSITION"
        menuChoices$(3) = "RETURN TO MAIN MENU           "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
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

            Select Case SELECTION%

                Case 1: Call POSCR

                Case 2: Call SearchPlayers

                Case 3: Cls

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       SearchPlayers Subroutine
'----------------------------------------
'This subroutine gives users a menu to
'pick a position and then also the option
'to view or print results. The corresponding
'routine is then called based on the pick.
Sub SearchPlayers ()

    Shared memberName$(), posTeamYr$()
    Shared posTeamIdx()

    Cls

    positionFile$ = ChooseFile$ (diskPaths$(0) + "*.POS", "POSITION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do

            Cls

            currPosition = 0

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
                        pitcherNames$ = "CATCHERS"
                    Case 1
                        PS = 3
                        pitcherNames$ = "FIRST BASEMEN"
                    Case 2
                        PS = 4
                        pitcherNames$ = "SECOND BASEMEN"
                    Case 3
                        PS = 6
                        pitcherNames$ = "SHORTSTOPS"
                    Case 4
                        PS = 5
                        pitcherNames$ = "THIRD BASEMEN"
                    Case 5
                        PS = 7
                        pitcherNames$ = "LEFT FIELDERS"
                    Case 6
                        PS = 8
                        pitcherNames$ = "CENTER FIELDERS"
                    Case 7
                        PS = 9
                        pitcherNames$ = "RIGHT FIELDERS"
                    Case 8:
                        PS = 1
                        Call SortPitchers(positionFile$)
                    Case 9:
                        Cls '

                End Select

                If Z <> 9 Then

                    Open diskPaths$(0) + positionFile$ + ".POS" For Input As #1

                    For J = 1 To TEAMS_PER_LEAGUE
                        Input #1, posTeamIdx(J), memberName$(J), posTeamYr$(J)
                    Next

                    Close #1

                    userKey$ = GetOutputOptions$

                    If userKey$ <> Chr$(27) Then

                        Select Case userKey$
                        
                            Case "F"
                                userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                                If userFile$ <> "" Then
                                    Call SearchResults_Report(0, userFile$, PS, pitcherNames$, positionFile$)
                                End If

                            Case "P"
                                Call SearchResults_Report(1, "REPORT", PS, pitcherNames$, positionFile$)

                            Case "S": 
                                Call ViewSearchResults(PS, pitcherNames$, positionFile$)
                        
                        End Select
                        
                    End If

                End If 'Done looking for Z = 9

            End If 'Done checking for ESC

        Loop Until Z = 9

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'      ViewSearchResults Subroutine
'----------------------------------------
'This subroutine displays the results of
'the search parameters to the default display
Sub ViewSearchResults (posnType, posnDesc$, positionFile$)

    Shared currPosition
    Shared NN$

    Shared batterNames$(), posTeamYr$()

    Shared posTeamIdx()
    Shared batterRatings()

    Dim BB$(23)

    Cls

    Locate 1, 35
    Color 15: Print posnDesc$
    Locate 3
    Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"

    For J = 1 To TEAMS_PER_LEAGUE

        If posTeamIdx(J) <> 0 Then

            Call ReadTeam(posTeamYr$(J), T$, posTeamIdx(J), validData)

            For I = 0 To 22

                If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

                    If batterRatings(I, 22) = posnType Then 
                        I2 = 33
                    End If
                    If batterRatings(I, 23) = posnType Then 
                        I2 = 35
                    End If
                    If batterRatings(I, 24) = posnType Then 
                        I2 = 37
                    End If
                    If batterRatings(I, 25) = posnType Then 
                        I2 = 39
                    End If

                    If I2 = 33 Or I2 = 35 Or I2 = 37 Or I2 = 39 Then

                        currPosition = currPosition + 1

                        If (currPosition Mod 19 = 1) Then

                            Locate LAST_ROW - 2
                            Color 14: Print "HIT ANY KEY TO CONTINUE"
                            I$ = GetKeyPress$

                            Cls

                            Locate 1, 35
                            Color 15: Print posnDesc$
                            Locate 3
                            Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"

                        End If

                        If batterRatings(I, 22) = posnType Then
                            Color 12
                            Print Left$(batterNames$(I), 10);
                        Else
                            Color 2
                            Print Left$(batterNames$(I), 10);
                        End If

                        Color 3
                        Print Tab(12); Left$(T$, 2); NN$;

                        If batterRatings(I, 0) = -1 Then 
                            BB$(I) = "L"
                        End If
                        If batterRatings(I, 0) = 1 Then 
                            BB$(I) = "R"
                        End If
                        If batterRatings(I, 0) = 0 Then 
                            BB$(I) = "S"
                        End If
                        If batterRatings(I, 0) = 2 Then 
                            BB$(I) = "RP"
                        End If
                        If batterRatings(I, 0) = -2 Then 
                            BB$(I) = "LP"
                        End If

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

                        If batterRatings(I, 4) = 0 Or I2 = 0 Then 
                            Print Tab(66); ".000";
                        End If

                        If I2 > 0 And I2 < 100 Then 
                            Print Tab(66); ".0";: Print Using "##"; I2;
                        End If
                        If I2 >= 100 Then 
                            Print Tab(66); ".";: Print Using "###"; I2;
                        End If

                        If batterRatings(I, 4) = 0 Or I3 = 0 Then 
                            Print Tab(71); ".000";
                        End If

                        If I3 > 0 And I3 < 100 Then 
                            Print Tab(71); ".0";: Print Using "##"; I3;
                        End If
                        If I3 >= 100 Then 
                            Print Tab(71); ".";: Print Using "###"; I3;
                        End If

                        If batterRatings(I, 4) = 0 Or I4 = 0 Then 
                            Print Tab(76); ".000";
                        End If

                        If I4 > 0 And I4 < 100 Then 
                            Print Tab(76); ".0";: Print Using "##"; I4;
                        End If
                        If I4 >= 100 Then 
                            Print Tab(76); ".";: Print Using "###"; I4

                        End If

                    End If

                End If

            Next I

        End If 'Done checking posTeamIdx(J)

    Next J

    Locate 23
    Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'      SearchResults_Report Subroutine
'----------------------------------------
'This subroutine displays the results of
'the search parameters to the default printer
Sub SearchResults_Report (printDest, destFile$, posnType, posnDesc$, positionFile$)

    Shared currPosition
    Shared NN$

    Shared posTeamYr$()

    Shared batterRatings()
    Shared posTeamIdx()
    
    Dim BB$(23)

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1,  Tab(55); posnDesc$
    Print #1, 
    Print #1,  "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"

    For J = 1 To TEAMS_PER_LEAGUE

        If posTeamIdx(J) <> 0 Then

            Call ReadTeam(posTeamYr$(J), T$, posTeamIdx(J), validData)

            For I = 0 To 22

                If batterNames$(I) <> "XXX" And batterNames$(I) <> "" Then

                    If batterRatings(I, 22) = posnType Or batterRatings(I, 23) = posnType Or batterRatings(I, 24) = posnType Or batterRatings(I, 25) = posnType Then

                        currPosition = currPosition + 1

                        If (currPosition Mod 50 = 1) Then
                            Print #1,  Chr$(12)
                            Print #1,  Tab(50); posnDesc$
                            Print #1, 
                            Print #1,  "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        If batterRatings(I, 22) = posnType Then
                            Print #1,  batterNames$(I);
                        Else
                            Print #1,  Tab(3); batterNames$(I);
                        End If

                        Print #1,  Tab(16); Left$(T$, 2); NN$;

                        If batterRatings(I, 0) = -1 Then 
                            BB$(I) = "L"
                        End If
                        If batterRatings(I, 0) = 1 Then 
                            BB$(I) = "R"
                        End If
                        If batterRatings(I, 0) = 0 Then 
                            BB$(I) = "S"
                        End If

                        Print #1,  Tab(34); BB$(I);
                        Print #1,  Tab(39); Using "###"; batterRatings(I, 3);
                        Print #1,  Tab(45); Using "###"; batterRatings(I, 4);
                        Print #1,  Tab(52); Using "###"; batterRatings(I, 6);
                        Print #1,  Tab(58); Using "##"; batterRatings(I, 7);
                        Print #1,  Tab(63); Using "##"; batterRatings(I, 8);
                        Print #1,  Tab(68); Using "##"; batterRatings(I, 9);
                        Print #1,  Tab(73); Using "###"; batterRatings(I, 10);
                        Print #1,  Tab(80); Using "###"; batterRatings(I, 5);
                        Print #1,  Tab(85); Using "###"; batterRatings(I, 11);
                        Print #1,  Tab(91); Using "###"; batterRatings(I, 12);
                        Print #1,  Tab(96); Using "###"; batterRatings(I, 13);
                        Print #1,  Tab(102); Using "##"; batterRatings(I, 14);

                        I2 = 0
                        I3 = 0
                        I4 = 0

                        If batterRatings(I, 4) > 0 Then
                            I2 = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                            I3 = Int(((batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)) / batterRatings(I, 4) + .0005) * 1000)
                            I4 = Int(((batterRatings(I, 6) + batterRatings(I, 11)) / (batterRatings(I, 11) + batterRatings(I, 4)) + .0005) * 1000)
                        End If

                        If batterRatings(I, 4) = 0 Or I2 = 0 Then Print #1,  Tab(107); ".000";

                        If I2 > 0 And I2 < 100 Then 
                            Print #1,  Tab(107); ".0";: Print #1,  Using "##"; I2;
                        End If
                        If I2 >= 100 Then 
                            Print #1,  Tab(107); ".";: Print #1,  Using "###"; I2;
                        End If

                        If batterRatings(I, 4) = 0 Or I3 = 0 Then 
                            Print #1,  Tab(114); ".000";
                        End If

                        If I3 > 0 And I3 < 100 Then 
                            Print #1,  Tab(114); ".0";: Print #1,  Using "##"; I3;
                        End If
                        If I3 >= 100 Then 
                            Print #1,  Tab(114); ".";: Print #1,  Using "###"; I3;
                        End If

                        If batterRatings(I, 4) = 0 Or I4 = 0 Then 
                            Print #1,  Tab(121); ".000";
                        End If

                        If I4 > 0 And I4 < 100 Then 
                            Print #1,  Tab(121); ".0";: Print #1,  Using "##"; I4;
                        End If
                        If I4 >= 100 Then 
                            Print #1,  Tab(121); ".";: Print #1,  Using "###"; I4
                        End If

                    End If

                End If 'Done checking for invalid chars

            Next I

        End If

    Next J

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
'       SortPitchers Subroutine
'----------------------------------------
'This subroutine gives the user more options
'for filtering their search results further
Sub SortPitchers (positionFile$)

    Shared currPosition

    currPosition = 0

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
            PpitcherNames$ = "PITCHERS WITH LESS THAN 10 GAMES STARTED"
            Call Pitchers10_Games(PC, PpitcherNames$, positionFile$)

        Case 2:
            PPS = 10
            PC = 2
            PpitcherNames$ = "PITCHERS WITH MORE THAN 10 GAMES STARTED"
            Call Pitchers10_Games(PC, PpitcherNames$, positionFile$)

        Case 3:
            THR = 1
            PpitcherNames$ = "RIGHT-HANDED PITCHERS"
            Call PitchersHanded(THR, PpitcherNames$, positionFile$)

        Case 4:
            THR = 2
            PpitcherNames$ = "LEFT-HANDED PITCHERS"
            Call PitchersHanded(THR, PpitcherNames$, positionFile$)

    End Select

End Sub

'----------------------------------------
'      Pitchers10_Games Subroutine
'----------------------------------------
'This subroutine handles the display of
'pitchers based on # of games played
Sub Pitchers10_Games (PC, pitchPosnDesc$, positionFile$)

    Shared currPosition
    Shared NN$

    Shared pitcherNames$(), pitchHand$(), posTeamYr$()

    Shared posTeamIdx()
    Shared teamRatings(), pitcherRatings()

    userKey$ = GetOutputOptions$

    If userKey$ <> Chr$(27) Then

        Select Case userKey$

            Case "F"
                userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                If userFile$ <> "" Then
                    Call Pitchers10_Print(0, userFile$, PC, pitchPosnDesc$, positionFile$)
                End If

            Case "P":
                Call Pitchers10_Print(1, "REPORT", PC, pitchPosnDesc$, positionFile$)

            Case "S":

                Cls
                Color 15
                Locate 1, 20: Print pitchPosnDesc$

                Locate 3
                Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

                For J = 1 To TEAMS_PER_LEAGUE

                    If posTeamIdx(J) <> 0 Then

                        Call ReadTeam(posTeamYr$(J), T$, posTeamIdx(J), validData)

                        For I = 0 To 21

                            If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                                If PC = 1 And pitcherRatings(I, 5) < PPS Or pitcherRatings(I, 5) > PPS Then

                                    currPosition = currPosition + 1

                                    If (currPosition Mod 18 = 1) Then

                                        Locate LAST_ROW - 2
                                        Color 14: Print "HIT ANY KEY TO CONTINUE"
                                        I$ = GetKeyPress$

                                        Cls

                                        Color 15
                                        Locate 1, 20: Print pitchPosnDesc$

                                        Color 11
                                        Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

                                    End If

                                    Color 12: Print Left$(pitcherNames$(I), 10);
                                    Color 3: Print Tab(12); Left$(T$, 2); NN$

                                    If pitcherRatings(I, 0) = 1 Then 
                                        pitchHand$(I) = "R"
                                    End If
                                    If pitcherRatings(I, 0) = -1 Then 
                                        pitchHand$(I) = "L"
                                    End If

                                    Color 7: Print Tab(22); pitchHand$(I);

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
        
    End If

End Sub


'----------------------------------------
'    PitchersHanded Subroutine
'----------------------------------------
'This subroutine handles the display of
'pitchers based on right/left handedness
Sub PitchersHanded (THR, pitchPosnDesc$, positionFile$)

    Shared currPosition
    Shared NN$

    Shared pitcherNames$(), pitchHand$(), posTeamYr$()

    Shared pitcherRatings()
    Shared posTeamIdx()

    userKey$ = GetPrintOptions$

    If userKey$ <> Chr$(27) Then

        Select Case userKey$

            Case "S":
                Cls
                Color 15
                Locate 1, 35: Print pitchPosnDesc$

                Color 11:
                Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

                For J = 1 To TEAMS_PER_LEAGUE

                    If posTeamIdx(J) <> 0 Then

                        Call ReadTeam(posTeamYr$(J), T$, posTeamIdx(J), validData)

                        For I = 0 To 21

                            If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                                If THR = 1 And pitcherRatings(I, 0) = 1 Or pitcherRatings(I, 0) = -1 Then

                                    currPosition = currPosition + 1

                                    If (currPosition Mod 19 = 1) Then
                                        Color 14
                                        Locate LAST_ROW - 2: Print "HIT ANY KEY TO CONTINUE"
                                        I$ = GetKeyPress$

                                        Cls

                                        Color 15
                                        Locate 1, 20: Print pitchPosnDesc$

                                        Color 11
                                        Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
                                    End If

                                    Color 12: Print Left$(pitcherNames$(I), 10);
                                    Color 3: Print Tab(12); Left$(T$, 2); NN$;

                                    If pitcherRatings(I, 0) = 1 Then 
                                        pitchHand$(I) = "R"
                                    End If
                                    If pitcherRatings(I, 0) = -1 Then 
                                        pitchHand$(I) = "L"
                                    End If

                                    Color 7: Print Tab(22); pitchHand$(I);

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

            Case "F"
                userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                If userFile$ <> "" Then
                    Call PitchersHanded_Print (0, userFile$, pitchPosnDesc$)
                End If

            Case "P"
                Call PitchersHanded_Print (1, "REPORT", pitchPosnDesc$)
            
        End Select
        
    End If

End Sub


'----------------------------------------
'     PitchersHanded_Print Subroutine
'----------------------------------------
'This subroutine sends the pitcher info.
'based on games to the default printer
Sub PitchersHanded_Print (printDest, destFile$, pitchPosnDesc$)

    Shared currPosition
    Shared NN$

    Shared pitcherNames$(), pitchHand$(), posTeamYr$()
    Shared pitcherRatings()

    Shared posTeamIdx()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1,  Tab(50); pitchPosnDesc$
    Print #1, 
    Print #1,  "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

    For J = 1 To TEAMS_PER_LEAGUE

        If posTeamIdx(J) <> 0 Then

            Call ReadTeam(posTeamYr$(J), T$, posTeamIdx(J), validData)

            For I = 0 To 21
                If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                    If THR = 1 And pitcherRatings(I, 0) = 1 Or pitcherRatings(I, 0) = -1 Then

                        currPosition = currPosition + 1

                        If (currPosition Mod 50 = 1) Then
                            Print #1,  Chr$(12)
                            Print #1,  Tab(50); pitchPosnDesc$
                            Print #1, 
                            Print #1,  "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        Print #1,  pitcherNames$(I);
                        Print #1,  Tab(19); Left$(T$, 2); NN$;

                        If pitcherRatings(I, 0) = 1 Then 
                            pitchHand$(I) = "R"
                        End If

                        If pitcherRatings(I, 0) = -1 Then 
                            pitchHand$(I) = "L"
                        End If

                        Print #1,  Tab(35); pitchHand$(I);
                        Print #1,  Tab(39); Using "###"; pitcherRatings(I, 4);
                        Print #1,  Tab(45); Using "##"; pitcherRatings(I, 1);
                        Print #1,  Tab(50); Using "##"; pitcherRatings(I, 2);
                        Print #1,  Tab(56); Using "##"; pitcherRatings(I, 3);
                        Print #1,  Tab(62); Using "##"; pitcherRatings(I, 5);
                        Print #1,  Tab(68); Using "###"; pitcherRatings(I, 6);
                        Print #1,  Tab(73); Using "###"; pitcherRatings(I, 7);
                        Print #1,  Tab(79); Using "###"; pitcherRatings(I, 8);
                        Print #1,  Tab(85); Using "###"; pitcherRatings(I, 9);
                        Print #1,  Tab(90); Using "##.##"; pitcherRatings(I, 10) / 100;
                        Print #1,  Tab(99); Using "##"; pitcherRatings(I, 13)

                    End If

                End If

            Next I

        End If

    Next J

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
'       Pitchers10_Print Subroutine
'----------------------------------------
'This subroutine sends the pitcher info.
'based on games to the default printer
Sub Pitchers10_Print (printDest, destFile$, PC, pitchPosnDesc$, positionFile$)

    Shared currPosition
    Shared NN$

    Shared pitcherNames$(), pitchHand$(), posTeamYr$()
    Shared pitcherRatings()

    Shared posTeamIdx()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1
    
    Print #1,  Tab(50); pitchPosnDesc$
    Print #1, 
    Print #1,  "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

    For J = 1 To TEAMS_PER_LEAGUE

        If posTeamIdx(J) <> 0 Then

            Call ReadTeam(posTeamYr$(J), T$, posTeamIdx(J), validData)

            For I = 0 To 21

                If pitcherNames$(I) <> "XXX" And pitcherNames$(I) <> "" Then

                    If PC = 1 And pitcherRatings(I, 5) < PPS Or pitcherRatings(I, 5) > PPS Then

                        currPosition = currPosition + 1

                        If (currPosition Mod 50 = 1) Then
                            Print #1,  Chr$(12)
                            Print #1,  Tab(50); pitchPosnDesc$
                            Print #1, 
                            Print #1,  "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        Print #1,  pitcherNames$(I);
                        Print #1,  Tab(19); Left$(T$, 2); NN$;

                        If pitcherRatings(I, 0) = 1 Then 
                            pitchHand$(I) = "R"
                        End If
                        If pitcherRatings(I, 0) = -1 Then 
                            pitchHand$(I) = "L"
                        End If

                        Print #1,  Tab(35); pitchHand$(I);
                        Print #1,  Tab(39); Using "###"; pitcherRatings(I, 4);
                        Print #1,  Tab(45); Using "##"; pitcherRatings(I, 1);
                        Print #1,  Tab(50); Using "##"; pitcherRatings(I, 2);
                        Print #1,  Tab(56); Using "##"; pitcherRatings(I, 3);
                        Print #1,  Tab(62); Using "##"; pitcherRatings(I, 5);
                        Print #1,  Tab(68); Using "###"; pitcherRatings(I, 6);
                        Print #1,  Tab(73); Using "###"; pitcherRatings(I, 7);
                        Print #1,  Tab(79); Using "###"; pitcherRatings(I, 8);
                        Print #1,  Tab(85); Using "###"; pitcherRatings(I, 9);
                        Print #1,  Tab(90); Using "##.##"; pitcherRatings(I, 10) / 100;
                        Print #1,  Tab(99); Using "##"; pitcherRatings(I, 13)
                    End If

                End If

            Next I

        End If

    Next J

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

