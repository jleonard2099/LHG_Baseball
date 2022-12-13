Sub DRAFT ()

    _Title "Full Count Baseball - Team Draft"

    DefInt A-Z

    Dim E6&
    Dim numberTeams%
    Dim draftYN$(0 To 1)

    Shared K9&()
    Shared B$(), P$(), draftB1$(), draftP1$()
    Shared draftB%(), draftB1%(), draftP%(), draftP1%()
    Shared L%(), SA%(), SA1%(), TS%(), TS1%()

    Do

        Cls

        '-- I know there's a command to assign all 0's
        '-- Of course, QB64 also assigns all 0s when Dim'd
        For I = 0 To 22
            B$(I) = "XXX"
            draftB1$(I) = "XXX"
            For I1 = 0 To 79
                draftB%(I, I1) = 0
                draftB1%(I, I1) = 0
            Next
        Next

        For I = 0 To 21
            P$(I) = "XXX"
            draftP1$(I) = "XXX"
            For I1 = 0 To 88
                draftP%(I, I1) = 0
                draftP1%(I, I1) = 0
            Next
        Next

        For I = 0 To 11
            TS%(I) = 0
            TS1%(I) = 0
        Next

        For I = 1 To 24
            SA%(I) = 0
            SA1%(I) = 0
        Next

        Cls
        Color 12, 0: Print Tab(31); "DRAFT TEAM UTILITY"
        Print
        Color 14: Print "(0) ";
        Color 15: Print "DRAFT A NEW TEAM"
        Print
        Color 14: Print "(1) ";
        Color 15: Print "DRAFT TO AN EXISTING TEAM"
        Print
        Color 14: Print "(2) ";
        Color 15: Print "RETURN TO GAME MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 0 Or I <= 2

        done% = 0

        Select Case I
            Case 0:
                DNT% = 0
                Call DraftNewTeam(draftYN$(0), T$)
                Call DoTheDraft(DNT%, done%, T$, draftYN$(), teamIdx)
                If done% = 1 Then Exit Sub

            Case 1:
                DNT% = 1

                'This needs to be a GetTeam_DRAFT routine
                Do
                    Color 15, 0: Cls
                    JJ = 7
                    Locate 6, 8: Color 15, 4
                    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
                    Locate 7 + JJ, 8
                    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
                    Locate , 12: Print "ENTER DISK ID: ": Print
                    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
                    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
                    Locate 7, 27: Input NY$
					If NY$ = "" And YN$ <> "" Then
                        Call SelectTeam_DRAFT(draftYN$(0), numberTeams%, 0, idx)
                        'draftYN$(0) = NY$
                    Else
                        draftYN$(0) = NY$
                        Erase teamNames$
                        numberTeams% = 0
                        If _FileExists(diskPaths$(0) + "FCTEAMS." + draftYN$(0)) Then

                            Open diskPaths$(0) + "FCTEAMS." + draftYN$(0) For Random As #1 Len = 4342
                            Y = LOF(1)
                            Field #1, 15 As Q$(0)
                            For I1 = 1 To Y / 4342
                                Get #1, I1
                                For I2 = 1 To 15
                                    If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then Exit For
                                Next I2
                                numberTeams% = numberTeams% + 1
                                teamNames$(numberTeams%) = Q$(0)
                                teamIndex%(numberTeams%) = I1

                            Next I1
                            Close 1

                            Call SelectTeam_DRAFT(draftYN$(0), numberTeams%, 0, idx)
                            'draftYN$(0) = YN$
                        Else
                            BO% = 1
                            'GoSub 4500
                        End If

                    End If

                    T$ = RTrim$(Q$(0))

                    If BO% = 1 Then
                        BO% = 0
                    Else
                        Locate 2, 33
                        'Color is not getting assigned here
                        'Neither is player data???
                        Color L%(11), L%(12): Print T$
                        Color 15, 4
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    End If

                Loop Until UCase$(I$) = "N"

                Color 15, 0

                '--- We need to track the index of the team to save
                Call DoTheDraft(DNT%, done%, T$, draftYN$(), idx)
                If done% = 1 Then Exit Sub

            Case 2
                Cls '

        End Select

    Loop Until I = 2

End Sub

'----------------------------------------
'       DraftNewTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (draftYear$, teamName$)

    ' Shared T$()
    Shared L%()
    Shared K9&()
    Shared M$, NN$, S$

    Cls
    Print "INPUT TEAM INFORMATION"

    'Do
    315 Locate 2: Input "1. DISK ID "; draftYear$: If C1 = 1 Then 415
    325 Locate 3: Input "2. TEAM NAME "; teamName$: If C1 = 1 Then 415
    335 Locate 4: Input "3. LGE BA (WHL #) "; L%(1): If C1 = 1 Then 415
    345 Locate 5: Input "4. LGE SO % (WHL #) "; L%(2): If C1 = 1 Then 415
    355 Locate 6: Input "5. LGE BB % (WHL #) "; L%(3): If C1 = 1 Then 415
    365 Locate 7: Input "6. LGE HR % (WHL #) "; L%(4): If C1 = 1 Then 415
    375 Locate 8: Input "7. TM DP/G X 10 "; L%(5): If C1 = 1 Then 415
    385 Locate 9: Input "8. PARK DOUBLE ADJUSTMENT (WHL #) "; L%(6): If C1 = 1 Then 415
    395 Locate 10: Input "9. PARK TRIPLE ADJUSTMENT (WHL #) "; L%(7): If C1 = 1 Then 415
    405 Locate 11: Input "10. PARK HOME RUN ADJUSTMENT (WHL #) "; K9&(0): If C1 = 1 Then 415
    406 Locate 12: Input "11. PARK FOUL GROUND ADJUSTMENT (WHL #) "; L%(8): If C1 = 1 Then 415
    407 Locate 13: Input "12. OUTDOOR = 0 / INDOOR = 1 "; L%(9): If C1 = 1 Then 415
    408 Locate 14: Input "13. GRASS = 0 / TURF = 1 "; L%(10): If C1 = 1 Then 415
    409 Locate 15: Input "14. STADIUM NAME "; S$: If C1 = 1 Then 415
    410 Locate 16: Input "15. PARK ID NUMBER "; L%(13): If C1 = 1 Then 415
    411 Locate 17: Input "16. MANAGER NAME "; M$: If C1 = 1 Then 415
    412 Locate 18: Input "17. TEAM TEXT COLOR (0-15) "; L%(11): If C1 = 1 Then 415
    413 Locate 19: Input "18. TEAM BACKGROUND (0-8) "; L%(12): If C1 = 1 Then 415
    414 Locate 20: Input "19. TEAM ABBREVIATION "; NN$: If C1 = 1 Then 415

    415 Locate 21, 1: Color L%(11), L%(12): Print "SAMPLE TEXT APPEARANCE": Color 15, 0
    C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

    416 If B9 < 0 Or B9 > 19 Then 415

    418 On B9 GOTO 315, 325, 335, 345, 355, 365, 375, 385, 395, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414

    'if 0 was selected
    41 C1 = 0

    'Loop Until C1 = 0

End Sub

'----------------------------------------
'       DoTheDraft Subroutine
'----------------------------------------
'...explanation...
Sub DoTheDraft (DNT%, doneDrafting%, teamName$, teamYears$(), saveSpot)

    Shared L%()
    Shared B$(), P$()

    Cls
    Color L%(11), L%(12): Print teamName$;
    Color 15, 0: Print "     (DRAFTING)"

    Print: Print "BATTERS";: Locate , 40: Print "PITCHERS"

    For I = 0 To 10
        Locate I + 5, 1
        Print Using "##"; I;: Print " "; B$(I); Tab(20);
        Print Using "##"; I + 12;: Print " "; B$(I + 12)
    Next

    Locate , 1: Print Using "##"; 11;: Print " "; B$(11)
    Print

    For I = 0 To 10
        Locate I + 5, 40
        Print Using "##"; I;
        Print " "; P$(I); Tab(60);
        Print Using "##"; I + 11;: Print " "; P$(I + 11)
    Next

    Do
        Locate 18, 1
        Color 14: Print "(1) ";: Color 15: Print "DRAFT BATTER"
        Color 14: Print "(2) ";: Color 15: Print "DRAFT PITCHER"
        Color 14: Print "(3) ";: Color 15: Print "SAVE DRAFT"
        Do
            I$ = GetKeyPress$
            I1 = Val(I$)
        Loop Until I1 >= 1 And I1 <= 3

        Select Case I1
            Case 1:
                Do
                    Locate 21, 1: Print "BATTER # (0-22) ";
                    N = 1
                    Input B
                Loop Until B >= 0 And P <= 22

                Call DraftPlayer(N, teamYears$(1))

            Case 2:
                Do
                    Locate 22, 40: Print "PITCHER # (0-21) ";
                    N = 2
                    Input P
                Loop Until P >= 0 And P <= 21

                Call DraftPlayer(N, teamYears$(1))

            Case 3:
                Call SaveTeamDraft(DNT%, teamYears$(0), teamName$, saveSpot)
                Cls
                JJ = 2

                Locate 6, 8
				Color 15, 1

                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

                For II = 1 To JJ
					Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next

                Locate 7 + JJ, 8
                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
				Locate 7

                U$ = "DRAFT ANOTHER TEAM (YN)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                I$ = GetKeyPress$

                If UCase$(I$) = "N" Then
                    doneDrafting% = 1
                End If

        End Select

    Loop Until I1 = 3

End Sub

'----------------------------------------
'       DraftPlayer Subroutine
'----------------------------------------
'...explanation...
Sub DraftPlayer (playerType, yearNumber$)

    Shared L1%(), draftB%(), draftB1%()
    Shared SA%(), SA1%(), draftP%(), draftP1%()
    Shared TS%(), TS1%()

    Shared B$(), draftB1$(), draftP1$(), P$()

    Do
        Color 15, 0
        Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$
		If NY$ = "" And yearNumber$ <> "" Then
            yearNumber$ = NY$
            Call SelectTeam_DRAFT(yearNumber$, numberTeams%, 1, idx)
        Else
            yearNumber$ = NY$
            Erase teamNames$
            'For I = 1 To 500: teamIndex%(I) = 1: Next
            numberTeams% = 0
            If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
                BO% = 1
                
				JJ = 2
				Locate 6, 8
				Color 15, 1
                
				Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
                
				For II = 1 To JJ: Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next
                
				Locate 7 + JJ, 8
                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

				Locate 7

            Else

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342
                Y = LOF(1)
                Field #1, 15 As Q$(0)
                For I1 = 1 To Y / 4342
                    Get #1, I1
                    For I2 = 1 To 15
                        'If Left$(Q$(0), 2) <> "XX" And Left$(Q$(0), 1) <> " " And Asc(Mid$(Q$(0), I2, 1)) >= 32 And Asc(Mid$(Q$(0), I2, 1)) <= 124 And UCase$(Left$(Q$(0), 3)) <> "NOT" And UCase$(Left$(Q$(0), 3)) <> "UN" Then
                        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                    Next I2
                    If nullTeam <> 1 Then
                        numberTeams% = numberTeams% + 1
                        teamNames$(numberTeams%) = Q$(0)
                        teamIndex%(numberTeams%) = I1
                    End If
                1103 Next I1
                Close 1

            End If

            Call SelectTeam_DRAFT(yearNumber$, numberTeams%, 1, idx)
        End If

        T1$ = RTrim$(Q$(0))

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color L1%(11), L1%(12): Print T1$
            Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

    Cls
    Color L1%(11), L1%(12): Print T1$
    Color 15, 0

    Print
    Color 14: Print "BATTERS";: Locate , 40: Print "PITCHERS": Color 15, 0

    For I = 0 To 10
        Locate I + 5, 1: Print Using "##"; I;: Print " "; draftB1$(I); Tab(20);: Print Using "##"; I + 12;: Print " "; draftB1$(I + 12)
    Next

    Locate , 1: Print Using "##"; 11;: Print " "; draftB1$(11)
    Print

    For I = 0 To 10
        Locate I + 5, 40: Print Using "##"; I;: Print " "; draftP1$(I); Tab(60);: Print Using "##"; I + 11;: Print " "; draftP1$(I + 11)
    Next

    If playerType = 1 Then
        Do
            Locate 21, 1: Print "BATTER # TO DRAFT ";
            Input B1
        Loop Until B1 >= 0 And B1 <= 22

        B$(B) = draftB1$(B1)

        For I = 0 To 79
            draftB%(B, I) = draftB1%(B1, I)
        Next I

        If TS%(0) <> 999 And TS1%(0) = 999 Then
            For I = 0 To 11
                TS%(I) = TS1%(I)
            Next
            For I = 1 To 24
                SA%(I) = SA1%(I)
            Next
        End If

    Else
        'playerType = 2
        Do
            Locate 21, 40: Print "PITCHER # TO DRAFT ";
            Input P1

        Loop Until P1 >= 0 And P1 <= 21

        P$(P) = draftP1$(P1)

        For I = 0 To 88:
            draftP%(P, I) = draftP1%(P1, I)
        Next I

        'TS%(0) = 999 Or TS1%(0) <> 999 Then End
        If TS%(0) <> 999 And TS1%(0) = 999 Then
            For I = 0 To 11
                TS%(I) = TS1%(I)
            Next
            For I = 1 To 24
                SA%(I) = SA1%(I)
            Next
        End If

    End If

End Sub

'----------------------------------------
'       SaveTeamDraft Subroutine
'----------------------------------------
'...explanation...
Sub SaveTeamDraft (DNT%, yearNumber$, teamName$, teamIdx)

    Shared B$(), P$()

    Shared L%(), SA%(), draftB%(), draftP%()
    Shared TS%()

    Shared K9&()

    Shared M$, NN$, S$

    Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

    Y = LOF(1)

    '0 actually means YES we're drafting a NEW team
    'Put them in the first available slot
    If DNT% = 0 Then teamIdx = Y / 4342 + 1

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

    LSet Q$(0) = teamName$

    For I = 1 To 13:
        LSet Q$(I) = MKI$(L%(I))
    Next

    LSet Q$(14) = MKS$(K9&(0))

    For I = 0 To 22:
        LSet Q$(I * 43 + 15) = B$(I)
        For I1 = 0 To 41:
            LSet Q$(I * 43 + I1 + 16) = MKI$(draftB%(I, I1))
        Next
    Next

    For I = 0 To 21:
        LSet Q$(I * 38 + 1004) = P$(I)
        For I1 = 0 To 36:
            LSet Q$(I * 38 + I1 + 1005) = MKI$(draftP%(I, I1))
        Next
    Next

    LSet Q$(1840) = S$
    LSet Q$(1841) = M$
    LSet Q$(1842) = NN$
    LSet Q$(1843) = MKI$(999)
    Put #1, teamIdx

    Close 1

    SCK = 0

    For X = 1 To 24
        SCK = SCK + SA%(X)
    Next

    If SCK > 0 Then
        TS%(0) = 999

        For X = 1 To 11
            TS%(X) = -1
        Next

        Open diskPaths$(0) + "LR." + yearNumber$ For Random As #1 Len = 4108

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 11
            LSet Q$(X + 1) = MKI$(TS%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                LSet Q$(12 + X1 + X * 14) = MKI$(draftB%(X, X1 + 41))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(334 + X1 + X * 14) = MKI$(draftP%(X, X1 + 36))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(642 + X1 + X * 14) = MKI$(draftP%(X, X1 + 50))
            Next
        Next

        For X = 1 To 24
            LSet Q$(X + 950) = MKI$(SA%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                LSet Q$(975 + X1 + X * 24) = MKI$(draftB%(X, X1 + 55))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                LSet Q$(1528 + X1 + X * 24) = MKI$(draftP%(X, X1 + 64))
            Next
        Next

        Put #1, T: Close 1

    End If

End Sub

'----------------------------------------
'     SelectTeam_DRAFT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_DRAFT (draftYear$, numberTeams%, teamNumber, teamIdx)

    Selection% = 1
    Count% = numberTeams%
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
        teamIdx = teamIndex%(Selection%)
        Locate 2, 33: Color 31: Print "LOADING..."
        If teamNumber = 0 Then
            Call ReadTeam_A(draftYear$, teamIdx)
        Else
            Call ReadTeam_B(draftYear$, teamIdx)
        End If
    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'          ReadTeam Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_A (yearNumber$, teamIdx)

    ' Shared M1$, NN1$, S1$
    Shared B$(), P$()
    Shared L%()
    Shared SA%(), draftB%(), draftP%(), TS%()

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

    Get #1, teamIdx

    For I = 1 To 13:
        L%(I) = CVI(Q$(I))
    Next

    E6& = CVS(Q$(14))

    For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41
            draftB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21:
        P$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36:
            draftP%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))

    Close 1

    If _FileExists(diskPaths$(0) + "LR." + yearNumber$) Then

        Open diskPaths$(0) + "LR." + yearNumber$ For Random As #1 Len = 4108

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        Get #1, teamIdx

        For X = 0 To 11
            TS%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                draftB%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                draftP%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                draftP%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
            Next
        Next

        For X = 1 To 24
            SA%(X) = CVI(Q$(X + 950))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                draftB%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                draftP%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
            Next
        Next

        Close 1

        If TS1%(0) <> 999 Then

            For X = 0 To 22
                For X1 = 42 To 79
                    draftB%(X, X1) = 0
                Next
            Next

            For X = 0 To 21
                For X1 = 37 To 88
                    draftP%(X, X1) = 0
                Next
            Next

            For X = 1 To 24
                SA%(X) = 0
            Next

            For X = 0 To 11
                TS%(X) = 0
            Next
        End If

    End If 'Done checking for file exists

End Sub

'----------------------------------------
'          ReadTeam Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_B (yearNumber$, teamIdx)

    ' Shared M1$, NN1$, S1$
    Shared draftB1$(), draftP1$()
    Shared L1%()
    Shared SA1%(), draftB1%(), draftP1%(), TS1%()

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

    Get #1, teamIdx

    For I = 1 To 13:
        L1%(I) = CVI(Q$(I))
    Next

    E6& = CVS(Q$(14))

    For I = 0 To 22: draftB1$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41
            draftB1%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21:
        draftP1$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36:
            draftP1%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    S1$ = RTrim$(Q$(1840))
    M1$ = RTrim$(Q$(1841))
    NN1$ = RTrim$(Q$(1842))

    Close 1

    If _FileExists(diskPaths$(0) + "LR." + yearNumber$) Then

        Open diskPaths$(0) + "LR." + yearNumber$ For Random As #1 Len = 4108

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        Get #1, teamIdx

        For X = 0 To 11
            TS1%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                draftB1%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                draftP1%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                draftP1%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
            Next
        Next

        For X = 1 To 24
            SA1%(X) = CVI(Q$(X + 950))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                draftB1%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                draftP1%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
            Next
        Next

        Close 1

        If TS1%(0) <> 999 Then

            For X = 0 To 22
                For X1 = 42 To 79
                    draftB1%(X, X1) = 0
                Next
            Next

            For X = 0 To 21
                For X1 = 37 To 88
                    draftP1%(X, X1) = 0
                Next
            Next

            For X = 1 To 24
                SA1%(X) = 0
            Next

            For X = 0 To 11
                TS1%(X) = 0
            Next
        End If

    End If 'Done checking for file exists

End Sub
