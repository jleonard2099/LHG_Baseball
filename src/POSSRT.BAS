Sub POSSRT ()

    _Title "Full Count Baseball - Sort By Position"

    DefInt A-Z

    ReDim menuChoices$(1 To 3)

    Do

		Color 15,0

        Cls

        Call TITLE

        menuChoices$(1) = "CREATE POSITION SEARCH FILE   "
        menuChoices$(2) = "SEARCH FOR PLAYERS BY POSITION"
        menuChoices$(3) = "RETURN TO MAIN MENU           "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 22
        Call box0(6, 21, 10, 52, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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
    Spec$ = diskPaths$(0) + "*.POS"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%

    If Count% = 0 Then
        Locate 10, 8
        Call CSMsgBox("NO POSITION FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else
        ReDim positionFiles$(0 To Count%)
        For X = 1 To Count%
            positionFiles$(X) = Space$(12)
        Next
        positionFiles$(0) = Spec$
        Call ReadFile(positionFiles$())

        Do
            Color , 1
            Cls

            Locate , 28
            Color 14
            Print "SELECT FILE TO VIEW": Color 15
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
            Print "-SELECTS FILE";

            _MouseShow "DEFAULT"
            MAMenu positionFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

				Color 15, 1
                targetFile$ = positionFiles$(Selection%)
                Locate 2, 33: Print targetFile$

                Do
					I$ = GetKeyPress$
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
				
				Color 15, 0

            End If

        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

		If ExitCode% <> 27 Then

			LGE$ = RTrim$(targetFile$)

			Do

				Cls

				SS = 0

				Color 11: Locate 5, 30: Print "S E L E C T"

				Locate 7, 28: Color 12: Print "0. ";: Color 7: Print "Catchers"
				Locate , 28: Color 12: Print "1. ";: Color 7: Print "First Basemen"
				Locate , 28: Color 12: Print "2. ";: Color 7: Print "Second Basemen"
				Locate , 28: Color 12: Print "3. ";: Color 7: Print "Shortstops"
				Locate , 28: Color 12: Print "4. ";: Color 7: Print "Third Basemen"
				Locate , 28: Color 12: Print "5. ";: Color 7: Print "Left Fielders"
				Locate , 28: Color 12: Print "6. ";: Color 7: Print "Center Fielders"
				Locate , 28: Color 12: Print "7. ";: Color 7: Print "Right Fielders"
				Locate , 28: Color 12: Print "8. ";: Color 7: Print "Pitchers"
				Locate , 28: Color 12: Print "9. ";: Color 7: Print "Return To Main Menu"

				Do
					Z$ = GetKeyPress$
					Z = Val(Z$)
				Loop Until Z >= 0 And Z <= 9 Or Z$ = Chr$(27)

				If Z$ <> Chr$(27) Then

					Select Case Z
						Case 0: PS = 2: PS$ = "CATCHERS"
						Case 1: PS = 3: PS$ = "FIRST BASEMEN"
						Case 2: PS = 4: PS$ = "SECOND BASEMEN"
						Case 3: PS = 6: PS$ = "SHORTSTOPS"
						Case 4: PS = 5: PS$ = "THIRD BASEMEN"
						Case 5: PS = 7: PS$ = "LEFT FIELDERS"
						Case 6: PS = 8: PS$ = "CENTER FIELDERS"
						Case 7: PS = 9: PS$ = "RIGHT FIELDERS"
						Case 8:
							PS = 1
							Call SortPitchers(LGE$)
						Case 9:
							Cls '

					End Select

					If Z <> 9 Then

						Color 15, 0
						Cls

						Do
							Locate 18, 28
							Color 12: Print "V";: Color 7: Print "iew or ";
							Color 12: Print "P";: Color 7: Print "rint"
							I$ = GetKeyPress$
						Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

						Select Case UCase$(I$)
							Case "P": Call PrintSearchResults(PS, PS$, LGE$)
							Case "V": Call ViewSearchResults(PS, PS$, LGE$)
						End Select

					End If 'Done looking for Z = 9

				End If 'Done checking for ESC

			Loop Until Z = 9

		End If 'Done checking for ESC

    End If

    '...Until we press ESC
    'Loop Until ExitCode% = 27 Or Count% = 0 Or I$ = "N"

End Sub

'----------------------------------------
'      ViewSearchResults Subroutine
'----------------------------------------
'...explanation...
Sub ViewSearchResults (PS, PS$, LGE$)

    Shared SS
    Shared NN$

    Shared statB$(), TYN$()
    Shared draftB%()

    Dim TM(1 To 40)
    Dim A$(1 To 40), BB$(23)

    Open diskPaths$(0) + LGE$ For Input As #1
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

            Call ReadTeam_PosSrt(TM(J), TYN$(J))

            T$ = RTrim$(Q$(0))

            For I = 0 To 22

                If statB$(I) <> "XXX" And statB$(I) <> "" Then

                    If draftB%(I, 22) = PS Then I2 = 33
                    If draftB%(I, 23) = PS Then I2 = 35
                    If draftB%(I, 24) = PS Then I2 = 37
                    If draftB%(I, 25) = PS Then I2 = 39

                    If I2 = 33 Or I2 = 35 Or I2 = 37 Or I2 = 39 Then

                        SS = SS + 1

                        If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Then
                            Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
                            I$ = GetKeyPress$
                            Cls
                            Locate 1, 35: Color 15: Print PS$
                            Locate 3: Color 11: Print "PLAYER     TEAM   B    G  AB HITS 2B 3B HR RBI RUNS SO  BB SB CS  AVG  OBP  SLG"
                        End If

                        If draftB%(I, 22) = PS Then
                            Color 12: Print Left$(statB$(I), 10);
                        Else
                            Color 2: Print Left$(statB$(I), 10);
                        End If

                        Color 3: Print Tab(12); Left$(T$, 2); NN$;

                        If draftB%(I, 0) = -1 Then BB$(I) = "L"
                        If draftB%(I, 0) = 1 Then BB$(I) = "R"
                        If draftB%(I, 0) = 0 Then BB$(I) = "S"
                        If draftB%(I, 0) = 2 Then BB$(I) = "RP"
                        If draftB%(I, 0) = -2 Then BB$(I) = "LP"

                        Color 7: Print Tab(19); BB$(I); Using "#"; draftB%(I, I2);
                        'PRINT draftB%(I,

                        Print Tab(22); Using "###"; draftB%(I, 3);
                        Print Tab(26); Using "###"; draftB%(I, 4);
                        Print Tab(31); Using "###"; draftB%(I, 6);
                        Print Tab(35); Using "##"; draftB%(I, 7);
                        Print Tab(38); Using "##"; draftB%(I, 8);
                        Print Tab(41); Using "##"; draftB%(I, 9);
                        Print Tab(44); Using "###"; draftB%(I, 10);
                        Print Tab(48); Using "###"; draftB%(I, 5);
                        Print Tab(52); Using "###"; draftB%(I, 11);
                        Print Tab(56); Using "###"; draftB%(I, 12);
                        Print Tab(59); Using "###"; draftB%(I, 13);
                        Print Tab(63); Using "##"; draftB%(I, 14);

                        I2 = 0
                        I3 = 0
                        I4 = 0

                        If draftB%(I, 4) > 0 Then I2 = Int((draftB%(I, 6) / draftB%(I, 4) + .0005) * 1000): I3 = Int(((draftB%(I, 6) + draftB%(I, 7) + 2 * draftB%(I, 8) + 3 * draftB%(I, 9)) / draftB%(I, 4) + .0005) * 1000): I4 = Int(((draftB%(I, 6) + draftB%(I, 11)) / (draftB%(I, 11) + draftB%(I, 4)) + .0005) * 1000)

                        If draftB%(I, 4) = 0 Or I2 = 0 Then Print Tab(66); ".000";
                        If I2 > 0 And I2 < 100 Then Print Tab(66); ".0";: Print Using "##"; I2;
                        If I2 >= 100 Then Print Tab(66); ".";: Print Using "###"; I2;
                        If draftB%(I, 4) = 0 Or I3 = 0 Then Print Tab(71); ".000";
                        If I3 > 0 And I3 < 100 Then Print Tab(71); ".0";: Print Using "##"; I3;
                        If I3 >= 100 Then Print Tab(71); ".";: Print Using "###"; I3;
                        If draftB%(I, 4) = 0 Or I4 = 0 Then Print Tab(76); ".000";
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
Sub PrintSearchResults (PS, PS$, LGE$)

    Shared SS
    Shared NN$

    Shared draftB%()
    Shared TYN$()

    Dim TM(40)
    Dim A$(1 To 40), BB$(23)

    Open diskPaths$(0) + LGE$ For Input As #1
    For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next
    Close #1

    LPrint Tab(55); PS$
    LPrint
    LPrint "PLAYER            TEAM           B      G    AB   HITS   2B   3B   HR   RBI   RUNS   SO    BB   SB   CS    AVG    OBP    SLG"

    For J = 1 To 40

        If TM(J) <> 0 Then

            Call ReadTeam_PosSrt(TM(J), TYN$(J))

            For I = 0 To 22

                If statB$(I) <> "XXX" And statB$(I) <> "" Then

                    If draftB%(I, 22) = PS Or draftB%(I, 23) = PS Or draftB%(I, 24) = PS Or draftB%(I, 25) = PS Then

                        SS = SS + 1

                        If SS = 51 Or SS = 101 Or SS = 151 Or SS = 201 Then
                            LPrint Chr$(12)
                            LPrint Tab(50); PPS$
                            LPrint
                            LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        If draftB%(I, 22) = PS Then
                            LPrint statB$(I);
                        Else
                            LPrint Tab(3); statB$(I);
                        End If

                        LPrint Tab(16); Left$(T$, 2); NN$;

                        If draftB%(I, 0) = -1 Then BB$(I) = "L"
                        If draftB%(I, 0) = 1 Then BB$(I) = "R"
                        If draftB%(I, 0) = 0 Then BB$(I) = "S"

                        LPrint Tab(34); BB$(I);
                        LPrint Tab(39); Using "###"; draftB%(I, 3);
                        LPrint Tab(45); Using "###"; draftB%(I, 4);
                        LPrint Tab(52); Using "###"; draftB%(I, 6);
                        LPrint Tab(58); Using "##"; draftB%(I, 7);
                        LPrint Tab(63); Using "##"; draftB%(I, 8);
                        LPrint Tab(68); Using "##"; draftB%(I, 9);
                        LPrint Tab(73); Using "###"; draftB%(I, 10);
                        LPrint Tab(80); Using "###"; draftB%(I, 5);
                        LPrint Tab(85); Using "###"; draftB%(I, 11);
                        LPrint Tab(91); Using "###"; draftB%(I, 12);
                        LPrint Tab(96); Using "###"; draftB%(I, 13);
                        LPrint Tab(102); Using "##"; draftB%(I, 14);

                        I2 = 0
                        I3 = 0
                        I4 = 0

                        If draftB%(I, 4) > 0 Then I2 = Int((draftB%(I, 6) / draftB%(I, 4) + .0005) * 1000): I3 = Int(((draftB%(I, 6) + draftB%(I, 7) + 2 * draftB%(I, 8) + 3 * draftB%(I, 9)) / draftB%(I, 4) + .0005) * 1000): I4 = Int(((draftB%(I, 6) + draftB%(I, 11)) / (draftB%(I, 11) + draftB%(I, 4)) + .0005) * 1000)

                        If draftB%(I, 4) = 0 Or I2 = 0 Then LPrint Tab(107); ".000";
                        If I2 > 0 And I2 < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2;
                        If I2 >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2;
                        If draftB%(I, 4) = 0 Or I3 = 0 Then LPrint Tab(114); ".000";
                        If I3 > 0 And I3 < 100 Then LPrint Tab(114); ".0";: LPrint Using "##"; I3;
                        If I3 >= 100 Then LPrint Tab(114); ".";: LPrint Using "###"; I3;
                        If draftB%(I, 4) = 0 Or I4 = 0 Then LPrint Tab(121); ".000";
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
Sub SortPitchers (LGE$)

    SS = 0
    Cls
    Color 11: Locate 5, 30: Print "S E L E C T"

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
            Call Pitchers10_Games(PC, PPS$, LGE$)
        Case 2:
            PPS = 10
            PC = 2
            PPS$ = "PITCHERS WITH MORE THAN 10 GAMES STARTED"
            Call Pitchers10_Games(PC, PPS$, LGE$)
        Case 3:
            THR = 1
            PPS$ = "RIGHT-HANDED PITCHERS"
            Call PitchersHanded(THR, PPS$, LGE$)
        Case 4:
            THR = 2
            PPS$ = "LEFT-HANDED PITCHERS"
            Call PitchersHanded(THR, PPS$, LGE$)
    End Select

End Sub

'----------------------------------------
'      Pitchers10_Games Subroutine
'----------------------------------------
'...explanation...
Sub Pitchers10_Games (PC, PPS$, LGE$)

    Shared SS
    Shared NN$

    Shared statP$(), PB$(), TYN$()
    Shared ratings%(), draftP%()

    Dim A$(1 To 40)
    Dim TM(40)

    Do
        Locate 18, 28
		Color 12: Print "V";: Color 7: Print "iew or ";
		Color 12: Print "P";: Color 7: Print "rint"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

    Select Case UCase$(I$)
        Case "P":
            Call Pitchers10_Print(PC, PPS$, LGE$)

        Case "V":
            Open diskPaths$(0) + LGE$ For Input As #1
            For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next
            Close #1

            Cls
            Color 15: Locate 1, 20: Print PPS$

            Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

            For J = 1 To 40

                If TM(J) <> 0 Then

                    Call ReadTeam_PosSrt(TM(J), TYN$(J))

                    For I = 0 To 21
                        If statP$(I) <> "XXX" And statP$(I) <> "" Then

                            If PC = 1 And draftP%(I, 5) < PPS Or draftP%(I, 5) > PPS Then
                                SS = SS + 1

                                If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Or SS = 235 Or SS = 253 Or SS = 271 Or SS = 289 Or SS = 307 Or SS = 325 Or SS = 343 Or SS = 361 Or SS = 379 Or SS = 397 Or SS = 415 Then
                                    Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
                                    I$ = GetKeyPress$
                                    Cls
                                    Color 15
                                    Locate 1, 20: Print PPS$
                                    Color 11
                                    Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
                                End If

                                Color 12: Print Left$(statP$(I), 10);
                                Color 3: Print Tab(12); Left$(T$, 2); NN$

                                If draftP%(I, 0) = 1 Then PB$(I) = "R"
                                If draftP%(I, 0) = -1 Then PB$(I) = "L"

                                Color 7: Print Tab(22); PB$(I);

                                Print Tab(25); Using "###"; draftP%(I, 4);
                                Print Tab(30); Using "##"; draftP%(I, 1);
                                Print Tab(34); Using "##"; draftP%(I, 2);
                                Print Tab(39); Using "##"; draftP%(I, 3);
                                Print Tab(43); Using "##"; draftP%(I, 5);
                                Print Tab(47); Using "###"; draftP%(I, 6);
                                Print Tab(52); Using "###"; draftP%(I, 7);
                                Print Tab(56); Using "###"; draftP%(I, 8);
                                Print Tab(61); Using "###"; draftP%(I, 9);
                                Print Tab(65); Using "##.##"; draftP%(I, 10) / 100;
                                Print Tab(73); Using "##"; draftP%(I, 13)
                            End If

                        End If

                    Next I

                End If

            Next J

            Locate 23: Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

    End Select

End Sub

'----------------------------------------
'    PitchersHanded Subroutine
'----------------------------------------
'...explanation...
Sub PitchersHanded (THR, PPS$, LGE$)

    Shared SS
    Shared NN$

    Shared statP$(), PB$(), TYN$()
    Shared draftP%()

    Dim A$(1 To 40)
    Dim TM(40)

    Locate 18, 28
	Color 12: Print "V";: Color 7: Print "iew or ";
	Color 12: Print "P";: Color 7: Print "rint"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

    Open diskPaths$(0) + LGE$ For Input As #1
    For J = 1 To 40
        Input #1, TM(J), A$(J), TYN$(J)
    Next
    Close #1

    Select Case UCase$(I$)
        Case "V":

            Cls
            Color 15: Locate 1, 35: Print PPS$

            Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

            For J = 1 To 40

                If TM(J) <> 0 Then

                    Call ReadTeam_PosSrt(TM(J), TYN$(J))

                    For I = 0 To 21
                        If statP$(I) <> "XXX" And statP$(I) <> "" Then

                            If THR = 1 And draftP%(I, 0) = 1 Or draftP%(I, 0) = -1 Then

                                SS = SS + 1

                                If SS = 19 Or SS = 37 Or SS = 55 Or SS = 73 Or SS = 91 Or SS = 109 Or SS = 127 Or SS = 145 Or SS = 163 Or SS = 181 Or SS = 199 Or SS = 217 Or SS = 235 Or SS = 253 Or SS = 271 Or SS = 289 Or SS = 307 Or SS = 325 Or SS = 343 Or SS = 361 Or SS = 379 Or SS = 397 Or SS = 415 Then
                                    Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
                                    I$ = GetKeyPress$
                                    Cls
                                    Color 15
                                    Locate 1, 20: Print PPS$
                                    Color 11
                                    Locate 3: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"
                                End If

                                Color 12: Print Left$(statP$(I), 10);
                                Color 3: Print Tab(12); Left$(T$, 2); NN$;

                                If draftP%(I, 0) = 1 Then PB$(I) = "R"
                                If draftP%(I, 0) = -1 Then PB$(I) = "L"

                                Color 7: Print Tab(22); PB$(I);

                                Print Tab(25); Using "###"; draftP%(I, 4);
                                Print Tab(30); Using "##"; draftP%(I, 1);
                                Print Tab(34); Using "##"; draftP%(I, 2);
                                Print Tab(39); Using "##"; draftP%(I, 3);
                                Print Tab(43); Using "##"; draftP%(I, 5);
                                Print Tab(47); Using "###"; draftP%(I, 6);
                                Print Tab(52); Using "###"; draftP%(I, 7);
                                Print Tab(56); Using "###"; draftP%(I, 8);
                                Print Tab(61); Using "###"; draftP%(I, 9);
                                Print Tab(65); Using "##.##"; draftP%(I, 10) / 100;
                                Print Tab(73); Using "##"; draftP%(I, 13)
                            End If

                        End If

                    Next I

                End If

            Next J

            Locate 23: Color 14: Print "END OF FILE - - - HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

        Case "P":

            LPrint Tab(50); PPS$
            LPrint
            LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

            For J = 1 To 40

                If TM(J) <> 0 Then

                    Call ReadTeam_PosSrt(TM(J), TYN$(J))

                    For I = 0 To 21
                        If statP$(I) <> "XXX" And statP$(I) <> "" Then

                            If THR = 1 And draftP%(I, 0) = 1 Or draftP%(I, 0) = -1 Then

                                SS = SS + 1

                                If SS = 51 Or SS = 101 Or SS = 151 Or SS = 201 Or SS = 251 Or SS = 301 Or SS = 351 Or SS = 401 Then
                                    LPrint Chr$(12)
                                    LPrint Tab(50); PPS$
                                    LPrint: LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                                End If

                                LPrint statP$(I);
                                LPrint Tab(19); Left$(T$, 2); NN$;

                                If draftP%(I, 0) = 1 Then PB$(I) = "R"

                                If draftP%(I, 0) = -1 Then PB$(I) = "L"

                                LPrint Tab(35); PB$(I);
                                LPrint Tab(39); Using "###"; draftP%(I, 4);
                                LPrint Tab(45); Using "##"; draftP%(I, 1);
                                LPrint Tab(50); Using "##"; draftP%(I, 2);
                                LPrint Tab(56); Using "##"; draftP%(I, 3);
                                LPrint Tab(62); Using "##"; draftP%(I, 5);
                                LPrint Tab(68); Using "###"; draftP%(I, 6);
                                LPrint Tab(73); Using "###"; draftP%(I, 7);
                                LPrint Tab(79); Using "###"; draftP%(I, 8);
                                LPrint Tab(85); Using "###"; draftP%(I, 9);
                                LPrint Tab(90); Using "##.##"; draftP%(I, 10) / 100;
                                LPrint Tab(99); Using "##"; draftP%(I, 13)
                            End If

                        End If

                    Next I

                End If

            Next J

            LPrint Chr$(12)

    End Select

End Sub

'----------------------------------------
'          ReadTeam_PosSrt Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_PosSrt (teamIndex%, yearNumber$)

    Shared K9&()
    Shared statB$(), statP$()
    Shared draftB%(), ratings%(), draftP%()

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

    Get #1, teamIndex%

    For I = 1 To 13: ratings%(I) = CVI(Q$(I)): Next

    K9&(0) = CVS(Q$(14))

    For I = 0 To 22
        statB$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41
            draftB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21: statP$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36:
            draftP%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))
    CK = CVI(Q$(1843))

    Close 1

End Sub

'1850
'Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
'I$ = GetKeyPress$
'Color 15: Locate 1, 20: Print PPS$
'Cls: Locate 3: Color 11: Print "PLAYER     TEAM      T    G   W   L   SV  GS   IP   H   BB   SO   ERA   HR"

'----------------------------------------
'       Pitchers10_Print Subroutine
'----------------------------------------
'...explanation...
Sub Pitchers10_Print (PC, PPS$, LGE$)

    Shared SS
    Shared NN$

    Shared statP$(), PB$(), TYN$()
    Shared draftP%()

    Dim A$(1 To 40)
    Dim TM(40)

    Open diskPaths$(0) + LGE$ For Input As #1
    For J = 1 To 40: Input #1, TM(J), A$(J), TYN$(J): Next
    Close #1

    LPrint Tab(50); PPS$
    LPrint
    LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"

    For J = 1 To 40

        If TM(J) <> 0 Then

            Call ReadTeam_PosSrt(TM(J), TYN$(J))

            For I = 0 To 21

                If statP$(I) <> "XXX" And statP$(I) <> "" Then

                    If PC = 1 And draftP%(I, 5) < PPS Or draftP%(I, 5) > PPS Then

                        SS = SS + 1

                        If SS = 51 Or SS = 101 Or SS = 151 Or SS = 201 Or SS = 251 Or SS = 301 Or SS = 351 Or SS = 401 Then
                            LPrint Chr$(12)
                            LPrint Tab(50); PPS$
                            LPrint
                            LPrint "PLAYER               TEAM         T     G    W    L    SV    GS     IP    H    BB    SO    ERA    HR"
                        End If

                        LPrint statP$(I);
                        LPrint Tab(19); Left$(T$, 2); NN$;

                        If draftP%(I, 0) = 1 Then PB$(I) = "R"
                        If draftP%(I, 0) = -1 Then PB$(I) = "L"

                        LPrint Tab(35); PB$(I);
                        LPrint Tab(39); Using "###"; draftP%(I, 4);
                        LPrint Tab(45); Using "##"; draftP%(I, 1);
                        LPrint Tab(50); Using "##"; draftP%(I, 2);
                        LPrint Tab(56); Using "##"; draftP%(I, 3);
                        LPrint Tab(62); Using "##"; draftP%(I, 5);
                        LPrint Tab(68); Using "###"; draftP%(I, 6);
                        LPrint Tab(73); Using "###"; draftP%(I, 7);
                        LPrint Tab(79); Using "###"; draftP%(I, 8);
                        LPrint Tab(85); Using "###"; draftP%(I, 9);
                        LPrint Tab(90); Using "##.##"; draftP%(I, 10) / 100;
                        LPrint Tab(99); Using "##"; draftP%(I, 13)
                    End If

                End If

            Next I

        End If

    Next J

    LPrint Chr$(12)

End Sub
