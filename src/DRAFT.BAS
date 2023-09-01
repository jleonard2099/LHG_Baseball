'----------------------------------------
'            DRAFT Subroutine
'----------------------------------------
'This subroutine presents a menu to the
'user with choices for drafting players.
Sub DRAFT ()

    _Title "Full Count Baseball - Team Draft"

    Shared batterRatings(), batRat_DRAFT%()
    Shared draftSA%(), draftTS%()
    Shared pitcherRatings(), pitRat_DRAFT%()
    Shared teamRatings%(), teamSA%(), teamTS%()

    Shared batterNames$(), batNames_DRAFT$()
    Shared pitcherNames$(), pitNames_DRAFT$()

    Dim numberTeams

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
                    draftNew = 1

                    For I = 0 To 22
                        batterNames$(I) = "XXX"
                        batNames_DRAFT$(I) = "XXX"
                    Next

                    Erase batterRatings, batRat_DRAFT%

                    For I = 0 To 21
                        pitcherNames$(I) = "XXX"
                        pitNames_DRAFT$(I) = "XXX"
                    Next

                    Erase pitcherRatings, pitRat_DRAFT%

                    Erase teamTS%, draftTS%, teamSA%, draftSA%

                    Call NewTeamParameters(draftYN$(0), T$)
                    Call DraftChoices(draftNew, done, T$, draftYN$(), idx)

                Case 2:
                    draftNew = 0
                    Call ChooseDestTeam(draftYN$(0), NTMS, idx, T$, 0)
                    If idx > 0 Then Call DraftChoices(draftNew, done, T$, draftYN$(), idx)

            End Select

            If done = 1 Then Selection% = 3

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27 Or done = 1

End Sub

'----------------------------------------
'      NewTeamParameters Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'minimum required parameters to create a
'new team that players will be drafted to.
Sub NewTeamParameters (draftYear$, teamName$)

    ' Shared T$()
    Shared teamRatings%()

    Shared parkHR_DRAFT&()

    Shared Manager$, teamAbbrev$, Stadium$

    Color , 0
    Cls
    Print "INPUT TEAM INFORMATION"

    Locate 3: Input "1. DISK ID "; draftYear$
    Locate 4: Input "2. TEAM NAME "; teamName$
    Locate 5: Input "3. TEAM ABBREVIATION "; teamAbbrev$
    Locate 6: Input "4. LGE BA "; teamRatings%(1)
    Locate 7: Input "5. LGE SO % "; teamRatings%(2)
    Locate 8: Input "6. LGE BB % "; teamRatings%(3)
    Locate 9: Input "7. LGE HR % "; teamRatings%(4)
    Locate 10: Input "8. TM DP/G X 10 "; teamRatings%(5)
    Locate 11: Input "9. PARK DOUBLE ADJUSTMENT "; teamRatings%(6)
    Locate 12: Input "10. PARK TRIPLE ADJUSTMENT "; teamRatings%(7)
    Locate 13: Input "11. PARK HOME RUN ADJUSTMENT "; parkHR_DRAFT&(0)
    Locate 14: Input "12. PARK FOUL GROUND ADJUSTMENT "; teamRatings%(8)
    Locate 15: Input "13. OUTDOOR = 0 / INDOOR = 1 "; teamRatings%(9)
    Locate 16: Input "14. GRASS = 0 / TURF = 1 "; teamRatings%(10)
    Locate 17: Input "15. STADIUM NAME "; Stadium$
    Locate 18: Input "16. PARK ID NUMBER "; teamRatings%(13)
    Locate 19: Input "17. MANAGER NAME "; Manager$
    Locate 20: Input "18. TEAM TEXT COLOR (0-15) "; teamRatings%(11)
    Locate 21: Input "19. TEAM BACKGROUND (0-8) "; teamRatings%(12)

    Locate 22, 1
    Color teamRatings%(11), teamRatings%(12)
    Print "TEAM APPEARANCE: ";
    Print teamName$ + " " + teamAbbrev$
    Color 15, 0

    Do

        Do
            Locate 24
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 19

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input "1. DISK ID "; draftYear$

                Case 2
                    Locate 4: Input "2. TEAM NAME "; teamName$

                Case 3
                    Locate 5: Input "3. TEAM ABBREVIATION "; NN$

                Case 4
                    Locate 6: Input "4. LGE BA "; teamRatings%(1)

                Case 5
                    Locate 7: Input "5. LGE SO % "; teamRatings%(2)

                Case 6
                    Locate 8: Input "6. LGE BB % "; teamRatings%(3)

                Case 7
                    Locate 9: Input "7. LGE HR % "; teamRatings%(4)

                Case 8
                    Locate 10: Input "8. TM DP/G X 10 "; teamRatings%(5)

                Case 9
                    Locate 11: Input "9. PARK DOUBLE ADJUSTMENT "; teamRatings%(6)

                Case 10
                    Locate 12: Input "10. PARK TRIPLE ADJUSTMENT "; teamRatings%(7)

                Case 11
                    Locate 13: Input "11. PARK HOME RUN ADJUSTMENT "; parkHR_DRAFT&(0)

                Case 12
                    Locate 14: Input "12. PARK FOUL GROUND ADJUSTMENT "; teamRatings%(8)

                Case 13
                    Locate 15: Input "13. OUTDOOR = 0 / INDOOR = 1 "; teamRatings%(9)

                Case 14
                    Locate 16: Input "14. GRASS = 0 / TURF = 1 "; teamRatings%(10)

                Case 15
                    Locate 17: Input "15. STADIUM NAME "; Stadium$

                Case 16
                    Locate 18: Input "16. PARK ID NUMBER "; teamRatings%(13)

                Case 17
                    Locate 19: Input "17. MANAGER NAME "; Manager$

                Case 18
                    Locate 20: Input "18. TEAM TEXT COLOR (0-15) "; teamRatings%(11)
                    Locate 22, 1
                    Color teamRatings%(11), teamRatings%(12)
                    Print "TEAM APPEARANCE: ";
                    Print teamName$ + " " + teamAbbrev$
                    Color 15, 0

                Case 19
                    Locate 21: Input "19. TEAM BACKGROUND (0-8) "; teamRatings%(12)
                    Locate 22, 1
                    Color teamRatings%(11), teamRatings%(12)
                    Print "TEAM APPEARANCE: ";
                    Print teamName$ + " " + teamAbbrev$
                    Color 15, 0

            End Select

        End If

    Loop Until B9 = 0 'ENTER also counts as 0

End Sub


'----------------------------------------
'       DraftChoices Subroutine
'----------------------------------------
'This subroutine provides an interface the
'user can operate to choose their draft
'selections.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub DraftChoices (draftNew, doneDrafting, teamName$, teamYears$(), teamIdx)

    Shared teamRatings%()

    Shared batterNames$(), pitcherNames$()

    Do

        Color 15, 0
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
            Print Using "##"; I;: Print " "; batterNames$(I); Tab(20);
            Print Using "##"; I + 12;: Print " "; batterNames$(I + 12)
        Next

        Locate , 1: Print Using "##"; 11;: Print " "; batterNames$(11)
        Print

        For I = 0 To 10
            Locate I + 5, 40
            Print Using "##"; I;
            Print " "; pitcherNames$(I); Tab(60);
            Print Using "##"; I + 11;: Print " "; pitcherNames$(I + 11)
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

                    Call ChooseSourceTeam(teamYear$, NTMS, teamIdx, teamName$, 1)
                    Call DraftPlayer(1, teamYears$(1), B)

                Case 2:
                    Do
                        Locate 22, 40: Print "PITCHER # (0-21) ";
                        Input P
                    Loop Until P >= 0 And P <= 21

                    Call ChooseSourceTeam(teamYear$, NTMS, teamIdx, teamName$, 1)
                    Call DraftPlayer(2, teamYears$(1), B)

                Case 3:
                    Call SaveTeamDraft(draftNew, teamYears$(0), teamName$, teamIdx)

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

    U$ = "DRAFT ANOTHER TEAM? (Y/N)"

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
'This subroutine provides the end user an
'interface for choosing the player to be
'traded, depending on the position type.
'
'playerType
'   1 = Batter
'   2 = Pitcher
Sub DraftPlayer (playerType, yearNumber$, draftPos)

    Shared batterRatings(), batRat_DRAFT%()
    Shared draftSA%(), draftTS%(), lgRat_DRAFT%()
    Shared pitcherRatings(), pitRat_DRAFT%()
    Shared teamSA%(), teamTS%()

    Shared batterNames$(), batNames_DRAFT$()
    Shared pitNames_DRAFT$(), pitcherNames$()

    Color 15, 0
    Cls

    Color lgRat_DRAFT%(11), lgRat_DRAFT%(12): Print T1$
    Color 15, 0

    Print
    Color 14: Print "BATTERS";
    Locate , 40: Print "PITCHERS"
    Color 15, 0

    For I = 0 To 10
        Locate I + 5, 1: Print Using "##"; I;
        Print " "; batNames_DRAFT$(I); Tab(20);
        Print Using "##"; I + 12;
        Print " "; batNames_DRAFT$(I + 12)
    Next

    Locate , 1: Print Using "##"; 11;
    Print " "; batNames_DRAFT$(11)
    Print

    For I = 0 To 10
        Locate I + 5, 40: Print Using "##"; I;
        Print " "; pitNames_DRAFT$(I); Tab(60);
        Print Using "##"; I + 11;
        Print " "; pitNames_DRAFT$(I + 11)
    Next

    If playerType = 1 Then

        Do
            Locate 21, 1: Print "BATTER # TO DRAFT ";
            Input B1
        Loop Until B1 >= 0 And B1 <= 22

        batterNames$(draftPos) = batNames_DRAFT$(B1)

        For I = 0 To 79
            batterRatings(draftPos, I) = batRat_DRAFT%(B1, I)
        Next I

        If teamTS%(0) <> 999 And (0) = 999 Then

            For I = 0 To 11
                teamTS%(I) = (I)
            Next

            For I = 1 To 24
                teamSA%(I) = draftSA%(I)
            Next

        End If

    Else
        'playerType = 2
        Do
            Locate 21, 40: Print "PITCHER # TO DRAFT ";
            Input P1

        Loop Until P1 >= 0 And P1 <= 21

        pitcherNames$(P) = pitNames_DRAFT$(P1)

        For I = 0 To 88:
            pitcherRatings(P, I) = pitRat_DRAFT%(P1, I)
        Next I

        If teamTS%(0) <> 999 And draftTS%(0) = 999 Then

            For I = 0 To 11
                teamTS%(I) = draftTS%(I)
            Next

            For I = 1 To 24
                teamSA%(I) = draftSA%(I)
            Next

        End If

    End If

End Sub


'----------------------------------------
'       SaveTeamDraft Subroutine
'----------------------------------------
'This subroutine updates the source team
'with the newly assigned player / stat
'values after the draft.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub SaveTeamDraft (draftNew, yearNumber$, teamName$, teamIdx)

    Shared batterNames$(), pitcherNames$()

    Shared batterRatings(), pitcherRatings(), teamRatings%()
    Shared teamSA%(), teamTS%()

    Shared parkHR_DRAFT&()

    Shared Manager$, teamAbbrev$, Stadium$

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    '0 actually means YES we're drafting a NEW team
    'Put them in the first available slot
    If draftNew = 1 Then teamIdx = fileLength& / TEAM_SIZE_BYTES + 1

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

    LSet Q$(14) = MKS$(parkHR_DRAFT&(0))

    For I = 0 To 22:
        LSet Q$(I * 43 + 15) = batterNames$(I)
        For I1 = 0 To 41:
            LSet Q$(I * 43 + I1 + 16) = MKI$(batterRatings(I, I1))
        Next
    Next

    For I = 0 To 21:
        LSet Q$(I * 38 + 1004) = pitcherNames$(I)
        For I1 = 0 To 36:
            LSet Q$(I * 38 + I1 + 1005) = MKI$(pitcherRatings(I, I1))
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
        SCK = SCK + teamSA%(X)
    Next

    If SCK > 0 Then
        teamTS%(0) = 999

        For X = 1 To 11
            teamTS%(X) = -1
        Next

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = DATA_SIZE_BYTES

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), DATA_SIZE_BYTES - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), DATA_SIZE_BYTES - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 11
            LSet Q$(X + 1) = MKI$(teamTS%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                LSet Q$(12 + X1 + X * 14) = MKI$(batterRatings(X, X1 + 41))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(334 + X1 + X * 14) = MKI$(pitcherRatings(X, X1 + 36))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                LSet Q$(642 + X1 + X * 14) = MKI$(pitcherRatings(X, X1 + 50))
            Next
        Next

        For X = 1 To 24
            LSet Q$(X + 950) = MKI$(teamSA%(X))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                LSet Q$(975 + X1 + X * 24) = MKI$(batterRatings(X, X1 + 55))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                LSet Q$(1528 + X1 + X * 24) = MKI$(pitcherRatings(X, X1 + 64))
            Next
        Next

        Put #1, teamIdx

        Close 1

    End If

End Sub


'----------------------------------------
'     ChooseDestTeam Subroutine
'----------------------------------------
'This subroutine provides interface for
'choosing the team to draft players to
Sub ChooseDestTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)

    Shared teamRatings%()

    Shared previousYear$

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES WERE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close 1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, 0)

                If ExitCode% <> 27 And chosenIdx <> 0 Then

                    Do
                        Locate 2, 33
                        Color lgRat_DRAFT%(11), lgRat_DRAFT%(12): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0


End Sub


'----------------------------------------
'       ChooseSourceTeam SubRoutine
'----------------------------------------
'This subroutine provides interface for
'choosing the team to draft players from
Sub ChooseSourceTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)

    Shared lgRat_DRAFT%()

    Shared previousYear$

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES WERE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close 1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, draftIdx)

                If ExitCode% <> 27 And chosenIdx <> 0 Then

                    Do
                        Locate 2, 33
                        Color lgRat_DRAFT%(11), lgRat_DRAFT%(12): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectDraftTeam SubRoutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list of available
'ones.
Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, draftIdx)

    If numberTeams > 0 Then

        SELECTION% = 1
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
        Color 14
        'Didn't this have a designator ?!?!
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            Locate 2, 33: Color 3
            Print "LOADING.."

            If draftIdx = 0 Then
                Call ReadDestTeam(yearNumber$, teamName$, selectedIdx)
            Else
                Call ReadSourceTeam(yearNumber$, teamName$, selectedIdx)
            End If

        End If

    End If

End Sub

'----------------------------------------
'       ReadDestTeam SubRoutine
'----------------------------------------
'This subroutine reads team data into
'arrays for the team we are drafting from
Sub ReadDestTeam (teamYear$, targetTeam$, teamIdx)

    Shared batterRatings(), pitcherRatings()
    Shared teamRatings%()

    Shared teamSA%(), teamTS%()

    Shared parkHR_DRAFT&()

    Shared batterNames$(), pitcherNames$()

    Shared Manager$, teamAbbrev$, Stadium$

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

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

    targetTeam$ = _Trim$(Q$(0))

    For I = 1 To 13:
        teamRatings%(I) = CVI(Q$(I))
    Next

    parkHR_DRAFT&(0) = CVS(Q$(14))

    For I = 0 To 22:
        batterNames$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41
            batterRatings(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21:
        pitcherNames$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36:
            pitcherRatings(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    Stadium$ = RTrim$(Q$(1840))
    Manager$ = RTrim$(Q$(1841))
    teamAbbrev$ = RTrim$(Q$(1842))

    CK = CVI(Q$(1843))

    Close 1

    If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = DATA_SIZE_BYTES

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), DATA_SIZE_BYTES - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), DATA_SIZE_BYTES - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        Get #1, teamIdx

        For X = 0 To 11
            teamTS%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                batterRatings(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                pitcherRatings(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                pitcherRatings(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
            Next
        Next

        For X = 1 To 24
            teamSA%(X) = CVI(Q$(X + 950))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                batterRatings(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                pitcherRatings(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
            Next
        Next

        Close 1

        If draftTS%(0) <> 999 Then

            For X = 0 To 22
                For X1 = 42 To 79
                    batterRatings(X, X1) = 0
                Next
            Next

            For X = 0 To 21
                For X1 = 37 To 88
                    pitcherRatings(X, X1) = 0
                Next
            Next

            For X = 1 To 24
                teamSA%(X) = 0
            Next

            For X = 0 To 11
                teamTS%(X) = 0
            Next
        End If

    End If 'Done checking for file exists

End Sub


'----------------------------------------
'        ReadSourceTeam Subroutine
'----------------------------------------
'This subroutine reads team data into
'arrays for the team we are drafting to
Sub ReadSourceTeam (yearNumber$, targetTeam$, teamIdx)

    Shared batRat_DRAFT%(), lgRat_DRAFT%()
    Shared pitRat_DRAFT%()

    Shared draftSA%(), draftTS%()

    Shared parkHR_DRAFT&()

    Shared batNames_DRAFT$(), pitNames_DRAFT$()

    Dim srcTmAbbrev$, srcTmStadium$, srcTmManager$

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

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

    targetTeam$ = RTrim$(Q$(0))

    For I = 1 To 13:
        lgRat_DRAFT%(I) = CVI(Q$(I))
    Next

    parkHR_DRAFT&(1) = CVS(Q$(14))

    For I = 0 To 22:
        batNames_DRAFT$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41
            batRat_DRAFT%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21:
        pitNames_DRAFT$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36:
            pitRat_DRAFT%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    'We won't be saving these to the new team
    srcTmStadium$ = RTrim$(Q$(1840))
    srcTmManager$ = RTrim$(Q$(1841))
    srcTmAbbrev$ = RTrim$(Q$(1842))

    Close 1

    If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + yearNumber$) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = DATA_SIZE_BYTES

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1), DATA_SIZE_BYTES - 2 - 2 * X As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), DATA_SIZE_BYTES - 2 - (X - 1) * 2 - 1900 As X$
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
            Next
        Next

        Get #1, teamIdx

        For X = 0 To 11
            draftTS%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                batRat_DRAFT%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                pitRat_DRAFT%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                pitRat_DRAFT%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
            Next
        Next

        For X = 1 To 24
            draftSA%(X) = CVI(Q$(X + 950))
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                batRat_DRAFT%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                pitRat_DRAFT%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
            Next
        Next

        Close 1

        If draftTS%(0) <> 999 Then

            For X = 0 To 22
                For X1 = 42 To 79
                    batRat_DRAFT%(X, X1) = 0
                Next
            Next

            For X = 0 To 21
                For X1 = 37 To 88
                    pitRat_DRAFT%(X, X1) = 0
                Next
            Next

            For X = 1 To 24
                draftSA%(X) = 0
            Next

            For X = 0 To 11
                draftTS%(X) = 0
            Next

        End If

    End If 'Done checking for file exists

End Sub

