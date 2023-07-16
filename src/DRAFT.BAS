Sub DRAFT ()

    _Title "Full Count Baseball - Team Draft"

    Shared teamBatters%(), draftB1%(), teamPitchers%(), draftP1%()
    Shared teamRatings%(), inputSA%(), SA1%(), inputTS%(), TS1%()

    Shared K9&()

    Shared statB$(), statP$(), draftB1$(), draftP1$()

    Dim numberTeams%

    Dim E6&

    Dim draftYN$(0 To 1)

    ReDim menuChoices$(1 To 3)

    Do

        Call TITLE

        menuChoices$(1) = "DRAFT A NEW TEAM         "
        menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
        menuChoices$(3) = "RETURN TO GAME MENU      "
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

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColr%, FileBGColr%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            done = 0

            Select Case Selection%

                Case 1:
                    DNT = 1

                    For I = 0 To 22
                        statB$(I) = "XXX"
                        draftB1$(I) = "XXX"
                        For I1 = 0 To 79
                            teamBatters%(I, I1) = 0
                            draftB1%(I, I1) = 0
                        Next
                    Next

                    For I = 0 To 21
                        statP$(I) = "XXX"
                        draftP1$(I) = "XXX"
                        For I1 = 0 To 88
                            teamPitchers%(I, I1) = 0
                            draftP1%(I, I1) = 0
                        Next
                    Next

                    For I = 0 To 11
                        inputTS%(I) = 0
                        TS1%(I) = 0
                    Next

                    For I = 1 To 24
                        inputSA%(I) = 0
                        SA1%(I) = 0
                    Next

                    Call DraftNewTeam(draftYN$(0), T$)
                    Call DraftChoices(DNT, done, T$, draftYN$(), idx)

                Case 2:
                    DNT = 0
                    Call DraftExistingTeam(draftYN$(0), T$, idx)
                    Call DraftChoices(DNT, done, T$, draftYN$(), idx)

            End Select

            If done = 1 Then Selection% = 3

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27 Or done = 1

End Sub

'----------------------------------------
'       DraftNewTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (draftYear$, teamName$)

    ' Shared T$()
    Shared teamRatings%()
    Shared K9&()
    Shared M$, NN$, Stadium$

    Color , 0
    Cls
    Print "INPUT TEAM INFORMATION"

    Do

        315 Locate 2: Input "1. DISK ID "; draftYear$: If C1 = 1 Then 415
        325 Locate 3: Input "2. TEAM NAME "; teamName$: If C1 = 1 Then 415
        335 Locate 4: Input "3. LGE BA (WHL #) "; teamRatings%(1): If C1 = 1 Then 415
        345 Locate 5: Input "4. LGE SO % (WHL #) "; teamRatings%(2): If C1 = 1 Then 415
        355 Locate 6: Input "5. LGE BB % (WHL #) "; teamRatings%(3): If C1 = 1 Then 415
        365 Locate 7: Input "6. LGE HR % (WHL #) "; teamRatings%(4): If C1 = 1 Then 415
        375 Locate 8: Input "7. TM DP/G X 10 "; teamRatings%(5): If C1 = 1 Then 415
        385 Locate 9: Input "8. PARK DOUBLE ADJUSTMENT (WHL #) "; teamRatings%(6): If C1 = 1 Then 415
        395 Locate 10: Input "9. PARK TRIPLE ADJUSTMENT (WHL #) "; teamRatings%(7): If C1 = 1 Then 415
        405 Locate 11: Input "10. PARK HOME RUN ADJUSTMENT (WHL #) "; K9&(0): If C1 = 1 Then 415
        406 Locate 12: Input "11. PARK FOUL GROUND ADJUSTMENT (WHL #) "; teamRatings%(8): If C1 = 1 Then 415
        407 Locate 13: Input "12. OUTDOOR = 0 / INDOOR = 1 "; teamRatings%(9): If C1 = 1 Then 415
        408 Locate 14: Input "13. GRASS = 0 / TURF = 1 "; teamRatings%(10): If C1 = 1 Then 415
        409 Locate 15: Input "14. STADIUM NAME "; S$: If C1 = 1 Then 415
        410 Locate 16: Input "15. PARK ID NUMBER "; teamRatings%(13): If C1 = 1 Then 415
        411 Locate 17: Input "16. MANAGER NAME "; M$: If C1 = 1 Then 415
        412 Locate 18: Input "17. TEAM TEXT COLOR (0-15) "; teamRatings%(11): If C1 = 1 Then 415
        413 Locate 19: Input "18. TEAM BACKGROUND (0-8) "; teamRatings%(12): If C1 = 1 Then 415
        414 Locate 20: Input "19. TEAM ABBREVIATION "; NN$: If C1 = 1 Then 415

        415 Locate 21, 1: Color teamRatings%(11), teamRatings%(12): Print "SAMPLE TEXT APPEARANCE": Color 15, 0

        C1 = 1

        Do
            Locate 22
            Input "# TO CHANGE (0 TO CONTINUE) "; B9

        Loop Until B9 >= 0 And B9 <= 19

        418 '
        On B9 GOTO 315, 325, 335, 345, 355, 365, 375, 385, 395, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414

        'if 0 was selected
        C1 = 0

    Loop Until B9 = 0 'ENTER also counts as 0

End Sub


'----------------------------------------
'     DraftExistingTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftExistingTeam (teamYear$, teamName$, teamIdx)

    Shared teamBatters%(), draftB1%(), teamPitchers%(), draftP1%()
    Shared teamRatings%(), inputSA%(), SA1%(), inputTS%(), TS1%()

    Shared K9&()

    Shared statB$(), statP$(), draftB1$(), draftP1$()

    Dim numberTeams%

    Dim E6&

    Do
        Color 15, 0
        Cls

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
        Locate 7, 27: Input NY$

        If NY$ = "" And YN$ <> "" Then
            Call SelectTeam_DRAFT(teamYear$, numberTeams%, 0, idx)
        Else
            teamYear$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams% = 0

            If _FileExists(diskPaths$(0) + "FCTEAMS." + teamYear$) Then

                Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 To fileLength& / 4342

                    Get #1, I1

                    For I2 = 1 To 15
                        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then Exit For
                    Next I2

                    numberTeams% = numberTeams% + 1
                    teamNames$(numberTeams%) = Q$(0)
                    teamIndex%(numberTeams%) = I1

                Next I1

                Close 1

                Call SelectTeam_DRAFT(teamYear$, numberTeams%, 0, teamIdx)

            Else
                BO% = 1
            End If

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

            Color 15, 0

        End If

    Loop Until UCase$(I$) = "N"


End Sub


'----------------------------------------
'       DraftChoices Subroutine
'----------------------------------------
'...explanation...
Sub DraftChoices (DNT, doneDrafting, teamName$, teamYears$(), teamIdx)

    Shared teamRatings%()
    Shared statB$(), statP$()

    Do

        Cls
        Color teamRatings%(11), teamRatings%(12): Print teamName$;
        Color 15, 0
        Print "     (DRAFTING)"
        Print
        Print "BATTERS";
        Locate , 40
        Print "PITCHERS"

        For I = 0 To 10
            Locate I + 5, 1
            Print Using "##"; I;: Print " "; statB$(I); Tab(20);
            Print Using "##"; I + 12;: Print " "; statB$(I + 12)
        Next

        Locate , 1: Print Using "##"; 11;: Print " "; statB$(11)
        Print

        For I = 0 To 10
            Locate I + 5, 40
            Print Using "##"; I;
            Print " "; statP$(I); Tab(60);
            Print Using "##"; I + 11;: Print " "; statP$(I + 11)
        Next

        Locate 18, 1
        Color 14: Print "(1) ";
        Color 15: Print "DRAFT BATTER"
        Color 14: Print "(2) ";
        Color 15: Print "DRAFT PITCHER"
        Color 14: Print "(3) ";
        Color 15: Print "SAVE DRAFT"

        Do
            I$ = GetKeyPress$
            I1 = Val(I$)

            Select Case I1
                Case 1:
                    Do
                        Locate 21, 1: Print "BATTER # (0-22) ";
                        Input B
                    Loop Until B >= 0 And B <= 22

                    Call DraftPlayer(1, teamYears$(1), B)

                Case 2:
                    Do
                        Locate 22, 40: Print "PITCHER # (0-21) ";
                        Input P
                    Loop Until P >= 0 And P <= 21

                    Call DraftPlayer(2, teamYears$(1), B)

                Case 3:
                    Call SaveTeamDraft(DNT, teamYears$(0), teamName$, teamIdx)

            End Select

        Loop Until I1 >= 1 And I1 <= 3

    Loop Until I1 = 3

    Cls

    JJ = 1

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

    Do
        Locate 7, (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "N" Then
        doneDrafting = 1
    End If

End Sub

'----------------------------------------
'       DraftPlayer Subroutine
'----------------------------------------
'...explanation...
Sub DraftPlayer (playerType, yearNumber$, draftPos)

    Shared L1%(), teamBatters%(), draftB1%()
    Shared inputSA%(), SA1%(), teamPitchers%(), draftP1%()
    Shared inputTS%(), TS1%()

    Shared statB$(), draftB1$(), draftP1$(), statP$()

    Do

        Color 15, 0
        Cls

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
            yearNumber$ = NY$
            Call SelectTeam_DRAFT(yearNumber$, numberTeams%, 1, idx)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
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

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 To fileLength& / 4342

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

                Next I1

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

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0
    Cls

    Color L1%(11), L1%(12): Print T1$
    Color 15, 0

    Print
    Color 14: Print "BATTERS";: Locate , 40: Print "PITCHERS": Color 15, 0

    For I = 0 To 10
        Locate I + 5, 1: Print Using "##"; I;
        Print " "; draftB1$(I); Tab(20);
        Print Using "##"; I + 12;
        Print " "; draftB1$(I + 12)
    Next

    Locate , 1: Print Using "##"; 11;
    Print " "; draftB1$(11)
    Print

    For I = 0 To 10
        Locate I + 5, 40: Print Using "##"; I;
        Print " "; draftP1$(I); Tab(60);
        Print Using "##"; I + 11;
        Print " "; draftP1$(I + 11)
    Next

    If playerType = 1 Then

        Do
            Locate 21, 1: Print "BATTER # TO DRAFT ";
            Input B1
        Loop Until B1 >= 0 And B1 <= 22

        statB$(draftPos) = draftB1$(B1)

        For I = 0 To 79
            teamBatters%(draftPos, I) = draftB1%(B1, I)
        Next I

        If inputTS%(0) <> 999 And TS1%(0) = 999 Then
            For I = 0 To 11
                inputTS%(I) = TS1%(I)
            Next
            For I = 1 To 24
                inputSA%(I) = SA1%(I)
            Next
        End If

    Else
        'playerType = 2
        Do
            Locate 21, 40: Print "PITCHER # TO DRAFT ";
            Input P1

        Loop Until P1 >= 0 And P1 <= 21

        statP$(P) = draftP1$(P1)

        For I = 0 To 88:
            teamPitchers%(P, I) = draftP1%(P1, I)
        Next I

        If inputTS%(0) <> 999 And TS1%(0) = 999 Then
            For I = 0 To 11
                inputTS%(I) = TS1%(I)
            Next
            For I = 1 To 24
                inputSA%(I) = SA1%(I)
            Next
        End If

    End If

End Sub

'----------------------------------------
'       SaveTeamDraft Subroutine
'----------------------------------------
'...explanation...
Sub SaveTeamDraft (DNT, yearNumber$, teamName$, teamIdx)

    Shared statB$(), statP$()

    Shared teamRatings%(), inputSA%(), teamBatters%(), teamPitchers%()
    Shared inputTS%()

    Shared K9&()

    Shared M$, NN$, Stadium$

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

    fileLength& = LOF(1)

    '0 actually means YES we're drafting a NEW team
    'Put them in the first available slot
    If DNT = 1 Then teamIdx = fileLength& / 4342 + 1

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
        LSet Q$(I) = MKI$(teamRatings%(I))
    Next

    LSet Q$(14) = MKS$(K9&(0))

    For I = 0 To 22:
        LSet Q$(I * 43 + 15) = statB$(I)
        For I1 = 0 To 41:
            LSet Q$(I * 43 + I1 + 16) = MKI$(teamBatters%(I, I1))
        Next
    Next

    For I = 0 To 21:
        LSet Q$(I * 38 + 1004) = statP$(I)
        For I1 = 0 To 36:
            LSet Q$(I * 38 + I1 + 1005) = MKI$(teamPitchers%(I, I1))
        Next
    Next

    LSet Q$(1840) = Stadium$
    LSet Q$(1841) = Manager$
    LSet Q$(1842) = teamAbbrev$
    LSet Q$(1843) = MKI$(999)

    Put #1, teamIdx

    Close 1

    SCK = 0

    For X = 1 To 24
        SCK = SCK + inputSA%(X)
    Next

    If SCK > 0 Then
        inputTS%(0) = 999

        For X = 1 To 11
            inputTS%(X) = -1
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
            LSet Q$(X + 1) = MKI$(inputTS%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                LSet Q$(12 + X1 + X * 14) = MKI$(teamBatters%(X, X1 + 41))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(334 + X1 + X * 14) = MKI$(teamPitchers%(X, X1 + 36))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(642 + X1 + X * 14) = MKI$(teamPitchers%(X, X1 + 50))
            Next
        Next

        For X = 1 To 24
            LSet Q$(X + 950) = MKI$(inputSA%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                LSet Q$(975 + X1 + X * 24) = MKI$(teamBatters%(X, X1 + 55))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                LSet Q$(1528 + X1 + X * 24) = MKI$(teamPitchers%(X, X1 + 64))
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
    Shared statB$(), statP$()
    Shared teamRatings%()
    Shared inputSA%(), teamBatters%(), teamPitchers%(), inputTS%()

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

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
        teamRatings%(I) = CVI(Q$(I))
    Next

    E6& = CVS(Q$(14))

    For I = 0 To 22: statB$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41
            teamBatters%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21:
        statP$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36:
            teamPitchers%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    Stadium$ = RTrim$(Q$(1840))
    Manager$ = RTrim$(Q$(1841))
    teamAbbrev$ = RTrim$(Q$(1842))

    Close 1

    If _FileExists(diskPaths$(0) + "LR." + teamYear$) Then

        Open diskPaths$(0) + "LR." + teamYear$ For Random As #1 Len = 4108

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
            inputTS%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                teamBatters%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                teamPitchers%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                teamPitchers%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
            Next
        Next

        For X = 1 To 24
            inputSA%(X) = CVI(Q$(X + 950))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                teamBatters%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                teamPitchers%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
            Next
        Next

        Close 1

        If TS1%(0) <> 999 Then

            For X = 0 To 22
                For X1 = 42 To 79
                    teamBatters%(X, X1) = 0
                Next
            Next

            For X = 0 To 21
                For X1 = 37 To 88
                    teamPitchers%(X, X1) = 0
                Next
            Next

            For X = 1 To 24
                inputSA%(X) = 0
            Next

            For X = 0 To 11
                inputTS%(X) = 0
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

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

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
