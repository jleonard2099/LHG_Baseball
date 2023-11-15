'----------------------------------------
'         ViewRoster subroutine
'----------------------------------------
'This subroutine formats and displays the
'general stats for a team, then provides
'a menu for choosing options to display
'various aspects of the roster
Sub ViewRoster (targetTeam$)

    Shared ERX!

    Shared parkHR

    Shared teamRatings%(), TB%(), inputTP%()

    Shared batterNames$(), pitcherNames$()
    Shared batterRatings(), pitcherRatings()

    Erase TB%, inputTP%

    For K = 4 To 18:
        For I = 0 To 22
            If batterNames$(I) <> "XXX" Then
                TB%(K - 3) = TB%(K - 3) + batterRatings(I, K)
            End If
        Next
    Next

    For K = 4 To 18:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" And pitcherRatings(I, 11) = 999 Then
                TB%(K - 3) = TB%(K - 3) + pitcherRatings(I, K + 16)
            End If
        Next
    Next

    For K = 1 To 33:
        For I = 0 To 21
            If pitcherNames$(I) <> "XXX" Then
                inputTP%(K) = inputTP%(K) + pitcherRatings(I, K)
            End If
        Next
    Next

    ERX! = 0

    For I = 0 To 21
        If pitcherNames$(I) <> "XXX" Then
            ERX! = ERX! + ((pitcherRatings(I, 6) / 9) * (pitcherRatings(I, 10) / 100))
        End If
    Next

    Color 15, 0
    Cls

    Color 12
    Locate 5, 10: Print targetTeam$
    Color 3
    Locate 7, 10: Print "LEAGUE BATTING AVG.";
    Color 7
    Locate 7, 38: Print Using "#####"; teamRatings%(1)
    Color 3
    Locate , 10: Print "LEAGUE STRIKE OUT AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(2)
    Color 3
    Locate , 10: Print "LEAGUE BASE ON BALLS AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(3)
    Color 3
    Locate , 10: Print "LEAGUE HOME RUN AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(4)
    Color 3
    Locate , 10: Print "DOUBLE PLAYS AVG.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(5)
    Color 3
    Locate , 10: Print "PARK DOUBLE ADJ.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(6)
    Color 3
    Locate , 10: Print "PARK TRIPLE ADJ.";
    Color 7
    Locate , 38: Print Using "#####"; teamRatings%(7)
    Color 3
    Locate , 10: Print "PARK HOME RUN ADJ.";
    Color 7
    Locate , 37: Print Using "######"; parkHR
    Color 3
    Locate , 10: Print "PARK FOUL GROUND ADJ.";
    Color 7
    Locate , 37: Print Using "######"; teamRatings%(8)

    Color 11
    Locate 23, 10: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

    Do

        Cls
        Color 12: Print "TEAM OPTIONS"
        Print
        Color 14: Print "(1) ";
        Color 15: Print "VIEW HITTERS"
        Print
        Color 14: Print "(2) ";
        Color 15: Print "VIEW PITCHERS"
        Print
        Color 14: Print "(3) ";
        Color 15: Print "VIEW PITCHERS HITTING/FLDG STATS"
        Print
        Color 14: Print "(4) ";
        Color 15: Print "RETURN TO MAIN MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 4

        Select Case I

            Case 1:
                Call ViewHitters (T$)

            Case 2:
                Call ViewPitchers (T$)

            Case 3:
                Call ViewHittingFielding

        End Select

    Loop Until I = 4

End Sub

'----------------------------------------
'        InputEditTeam Subroutine
'----------------------------------------
'This subroutine steps user through screens 
'for adjusting team data and players
'
'NOTE: <ENTER> key is valued as 0 for when
'a user is prompted to press keys to CONTINUE
'This improvement over the original makes it
'faster and easier to proceed through screens

'inputAction% = 1: New Team
'inputAction% = 2: Edit Team
Sub InputEditTeam (inputAction%, yearNumber$, targetTeam$, teamIdx)

    Shared batterRatings(), pitcherRatings()
    Shared teamRatings%(), teamSA%(), teamTS%()

    Shared batterNames$(), pitcherNames$()

    Shared CK, parkHR
    Shared Manager$, teamAbbrev$, Stadium$

    ' New team only
    If inputAction% = 1 Then

        targetTeam$ = ""
        teamIdx = 0
        yearNumber$ = ""

        For X = 1 To 13
            teamRatings%(X) = 0
        Next

        parkHR = 0

        Erase batterRatings, pitcherRatings
        Erase teamSA%, teamTS%

        Erase batterNames$, pitcherNames$

        Stadium$ = ""
        Manager$ = ""
        teamAbbrev$ = ""
        CK = 0

    Else

        Stadium$ = RTrim$(Q$(1840))
        Manager$ = RTrim$(Q$(1841))
        teamAbbrev$ = RTrim$(Q$(1842))

    End If

    Call EditTeamInfo(yearNumber$, targetTeam$, teamIdx)
    Call EditBatter
    Call EditPitcher

    Locate 10, 8

End Sub

'----------------------------------------
'        SaveTeamFile subroutine
'----------------------------------------
'This subroutine saves the modifed team
'data (from corresponding arrays) to an
'appropriate spot in the select team file
Sub SaveTeamFile (newID%, yearNumber$, targetTeam$, targetFile$, teamIdx)

    Shared CK, parkHR

    Shared Manager$, teamAbbrev$, Stadium$

    Shared inputDYS%(), inputINJ%()
    Shared batterRatings(), pitcherRatings()
    Shared teamRatings%(), teamSA%(), teamTS%()

    Shared batterNames$(), pitcherNames$()

    Cls

    Locate 11,

    Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If teamIdx = 0 Or newID% = 1 Then teamIdx = fileLength& / TEAM_SIZE_BYTES + 1

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14)

    For I = 0 To 22
        Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26)
        Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38)
        Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50)
        Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57)
    Next

    For I = 0 To 21
        Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015)
        Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027)
        Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039)
        Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041)
    Next

    Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

    LSet Q$(0) = targetTeam$

    For I = 1 To 13:
        LSet Q$(I) = MKI$(teamRatings%(I))
    Next

    LSet Q$(14) = MKS$(parkHR)

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

    For I = 1 To 24
        SCK = SCK + teamSA%(I)
    Next

    If SCK > 0 Then

        teamTS%(0) = 999

        For I = 1 To 11
            teamTS%(I) = -1
        Next

        Open diskPaths$(0) + DATA_FILE_NAME$ + yearNumber$ For Random As #1 Len = DATA_SIZE_BYTES

        For X = 0 To 11
            Field #1, X * 2 As X$, 2 As Q$(X + 1)
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14)
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14)
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 14
                Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14)
            Next
        Next

        For X = 1 To 24
            Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X)
        Next

        For X = 0 To 22
            For X1 = 1 To 24
                Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24)
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 24
                Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24)
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

    Open diskPaths$(1) + targetFile$ + "D" For Output As #1

    For I1 = 0 To 22
        Print #1, inputINJ%(I1)
    Next

    For I1 = 0 To 21
        Print #1, inputDYS%(I1)
    Next

    Close 1

    Call CSMsgBox("", 0, Pass)

End Sub


'----------------------------------------
'       SwapSpots subroutine
'----------------------------------------
'This subroutine is used to allow the user
'an opportunity to switch default positions
'on the team roster. It then will call the
'correct routine to actually swap the info.
Sub SwapSpots (yearNumber$, targetTeam$, teamIdx)

    Shared batterNames$(), BS$(), pitcherNames$(), PS$()

    Shared inputBS%()
    Shared statB0%(), statP0%(), statSS%()
    Shared statT0!(), statT1!()

    Shared D0%, D1%

    Dim SS, TS

    TS = 0

    Color 15, 0

    Do

        Cls

        Print "DOES "; targetTeam$; " HAVE COMPILED STATS? (Y/N)";
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "Y" Then
        TS = 1
        Call LoadTeam(targetTeam$, SS, teamStatFile$)
    End If

    Do

        Cls

        Color 12
        Print "TEAM ROSTER FOR "; targetTeam$

        Color 14
        Locate 2, 2
        Print "BATTERS"
        Color 15
        For I = 0 To 22
            Locate I + 3, 1
            Print Using "## "; I + 1;
            Print batterNames$(I);
        Next

        Color 14
        Locate 2, 25
        Print "PITCHERS"
        Color 15
        For I = 0 To 21
            Locate I + 3, 25
            Print Using "## "; I + 1;
            Print pitcherNames$(I);
        Next

        Color 15
        Do
            Locate 10, 47: Print "1 - SWITCH BATTERS"
            Locate 11, 47: Print "2 - SWITCH PITCHERS"
            Locate 12, 47: Print "3 - SAVE CHANGES"
            Locate 13, 47: Print "4 - RETURN TO MENU (NO CHANGES)"
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 4

        Select Case I

            Case 1:
                Call SwitchBatters

            Case 2:
                Call SwitchPitchers

            Case 3:

                If TS <> 0 Then

                    Cls

                    Open diskPaths$(1) + teamStatFile$ For Output As #1

                    Print #1, A$
                    Print #1, W2%
                    Print #1, L2%

                    For I = 0 To 22:
                        Print #1, BS$(I)
                        Print #1, inputBS%(I)
                    Next I

                    For I = 0 To 21:
                        Print #1, PS$(I)
                        Print #1, PS%(I)
                    Next I

                    For I = 0 To 22:
                        For I1 = 0 To 21:
                            Print #1, statB0%(I, I1)
                        Next I1
                    Next I

                    For I = 0 To 21:
                        For I1 = 0 To 41:
                            Print #1, statP0%(I, I1)
                        Next I1
                    Next I

                    Print #1, D0%

                    Print #1, D1%

                    For I = 0 To 21:
                        Print #1, statT0!(I)
                    Next I

                    For I = 0 To 22:
                        Print #1, statT1!(I)
                    Next I

                    Close 1

                    If SS <> 0 Then

                        Open diskPaths$(1) + teamStatFile$ + "R" For Output As #1

                        For I = 0 To 22
                            For I1 = 0 To 6
                                Print #1, statSS%(I, I1)
                            Next I1
                        Next I

                        Close #1

                    End If

                End If

                Call SaveTeamFile(0, yearNumber$, targetTeam$, teamStatFile$, teamIdx)

        End Select

    Loop Until I >= 3

End Sub


'----------------------------------------
'           LoadTeam subroutine
'----------------------------------------
'This subroutine looks for and loads the
'corresponding stat files for the chosen
'team.
Sub LoadTeam (targetTeam$, SS, statFile$)

    Shared inputBS%(), PS%(), statSS%()
    Shared statB0%(), statP0%()

    Shared BS$(), PS$()

    Shared statT0!(), statT1!()

    statFile$ = nameStatFile$(targetTeam$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, A$
        Input #1, W2%
        Input #1, L2%

        For I = 0 To 22:
            Input #1, BS$(I)
            Input #1, inputBS%(I)
        Next I

        For I = 0 To 21:
            Input #1, PS$(I)
            Input #1, PS%(I)
        Next I

        For I = 0 To 22:
            For I1 = 0 To 21:
                Input #1, statB0%(I, I1)
            Next I1
        Next I

        For I = 0 To 21:
            For I1 = 0 To 41:
                Input #1, statP0%(I, I1)
            Next I1
        Next I

        Input #1, D0%
        Input #1, D1%

        For I = 0 To 21:
            Input #1, statT0!(I)
        Next I

        For I = 0 To 22:
            Input #1, statT1!(I)
        Next I

        Close #1

        If Not _FileExists(diskPaths$(1) + statFile$ + "R") Then

            SS = 0

        Else

            SS = 1

            Open diskPaths$(1) + statFile$ + "R" For Input As #1

            For I = 0 To 22
                For I1 = 0 To 6
                    Input #1, statSS%(I, I1)
                Next I1
            Next I

            Close #1

        End If 'done checking for files

    End If 'Done checking for stats files

End Sub


'----------------------------------------
'       SwitchBatters subroutine
'----------------------------------------
'This subroutine gives the user a simple
'interface for switching batters on the
'full team roster
Sub SwitchBatters ()

    Shared batterNames$(), BS$()
    Shared inputBS%(), batterRatings(), statB0%(), statSS%()

    Do
        For I = 16 To 19:
            Locate I, 50:
            Print Space$(27)
        Next

        Do
            Locate 16, 50
            Input "BATTER #1 "; I1
        Loop Until I1 >= 1 And I1 <= 23

        Locate 16, 62: Print batterNames$(I1 - 1)

        Do
            Locate 17, 50
            Input "BATTER #2 "; I2
        Loop Until I2 >= 1 And I2 <= 23

        Locate 17, 62: Print batterNames$(I2 - 1)

        Do
            Locate 18, 50
            Print "ANY CHANGE? (Y/N)";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Swap batterNames$(I1 - 1), batterNames$(I2 - 1)

    Swap BS$(I1 - 1), BS$(I2 - 1)

    For I = 0 To 79:
        Swap batterRatings(I1 - 1, I), batterRatings(I2 - 1, I)
    Next

    For I = 0 To 21:
        Swap statB0%(I1 - 1, I), statB0%(I2 - 1, I)
    Next

    For I = 0 To 6:
        Swap statSS%(I1 - 1, I), statSS%(I2 - 1, I)
    Next

    Swap inputBS%(I1 - 1), inputBS%(I2 - 1)

End Sub


'----------------------------------------
'       SwitchPitchers subroutine
'----------------------------------------
'This subroutine gives the user a simple
'interface for switching pitchers on the
'full team roster
Sub SwitchPitchers ()

    Shared pitcherRatings(), statP0%(), PS%()
    Shared pitcherNames$(), PS$()

    Do
        For I = 16 To 19:
            Locate I, 50: Print Space$(27)
        Next

        Do
            Locate 16, 50: Input "PITCHER #1 "; I1
        Loop Until I1 >= 1 And I1 <= 22

        Locate 16, 63: Print pitcherNames$(I1 - 1)

        Do
            Locate 17, 50: Input "PITCHER #2 "; I2
        Loop Until I1 >= 1 And I1 <= 22

        Locate 17, 63: Print pitcherNames$(I2 - 1)

        Do
            Locate 18, 50
            Print "ANY CHANGE? (Y/N)";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Swap pitcherNames$(I1 - 1), pitcherNames$(I2 - 1): Swap PS$(I1 - 1), PS$(I2 - 1)

    For I = 0 To 88:
        Swap pitcherRatings(I1 - 1, I), pitcherRatings(I2 - 1, I)
    Next

    For I = 0 To 41:
        Swap statP0%(I1 - 1, I), statP0%(I2 - 1, I)
    Next

    Swap PS%(I1 - 1), PS%(I2 - 1)

End Sub


'----------------------------------------
'       PrintTeamStats subroutine
'----------------------------------------
'This subroutine formats and outputs the
'previously loaded team stat data and sends
'it to the default printer
Sub PrintTeamStats (targetTeam$, teamIdx)

    Shared batterNames$(), BP$(), pitcherNames$()

    Shared batterRatings(), pitcherRatings(), teamRatings%()

    Shared CK, parkHR
    Shared Manager$, teamAbbrev$, Stadium$

    LPrint "TEAM # "; teamIdx
    LPrint "TEAM NAME "; targetTeam$; Tab(30); "MANAGER: "; Manager$; Tab(50); "STADIUM: "; Stadium$

    LPrint
    LPrint "LBA LSO LBB LHR DPG DBL TR    HR   FOUL PARK INFO"

    LPrint Using "### ### ### ### ### ### ### ###### ###  "; teamRatings%(1); teamRatings%(2); teamRatings%(3); teamRatings%(4); teamRatings%(5); teamRatings%(6); teamRatings%(7); parkHR; teamRatings%(8);: LPrint BP$(teamRatings%(9)); " "; BP$(teamRatings%(10) + 2)

    LPrint
    LPrint "BATTER      B    G  AB   R   H DB TR HR RBI  BB  SO  SB CS RN B GA P1 FAV1 A R P2 FAV2 A R P3 FAV3 A R P4 FAV4 A R EBA BAVG"

    For I = 0 To 22

        If batterNames$(I) <> "" And batterNames$(I) <> "XXX" Then

            LPrint Left$(batterNames$(I), 12); Tab(13);

            If batterRatings(I, 0) = 0 Then LPrint "S  ";
            If batterRatings(I, 0) = -1 Then LPrint "L  ";
            If batterRatings(I, 0) = 1 Then LPrint "R  ";
            If batterRatings(I, 0) = -2 Then LPrint "LP ";
            If batterRatings(I, 0) = 2 Then LPrint "RP ";

            LPrint Using "### ### ### ### ## ## ## ### ### ### ### ##  "; batterRatings(I, 3); batterRatings(I, 4); batterRatings(I, 5); batterRatings(I, 6); batterRatings(I, 7); batterRatings(I, 8); batterRatings(I, 9); batterRatings(I, 10); batterRatings(I, 11); batterRatings(I, 12); batterRatings(I, 13); batterRatings(I, 14);

            LPrint Using "# "; batterRatings(I, 16);: LPrint Using "# "; batterRatings(I, 30);

            LPrint Using "## ## #### # # "; batterRatings(I, 18); batterRatings(I, 22); batterRatings(I, 26); batterRatings(I, 33); batterRatings(I, 34);

            If batterRatings(I, 23) >= 0 Then
                LPrint Using "## #### # # "; batterRatings(I, 23); batterRatings(I, 27); batterRatings(I, 35); batterRatings(I, 36);
            End If

            If batterRatings(I, 24) >= 0 Then
                LPrint Using "## #### # # "; batterRatings(I, 24); batterRatings(I, 28); batterRatings(I, 37); batterRatings(I, 38);
            End If

            If batterRatings(I, 25) >= 0 Then
                LPrint Using "## #### # # "; batterRatings(I, 25); batterRatings(I, 29); batterRatings(I, 39); batterRatings(I, 40);
            End If

            LPrint Tab(116); Using "### "; batterRatings(I, 20);

            If batterRatings(I, 4) <> 0 Then
                Z% = batterRatings(I, 6) / batterRatings(I, 4) * 1000
                LPrint Using " ###"; batterRatings(I, 6) / batterRatings(I, 4) * 1000
            Else
                LPrint " 000"
            End If

        End If

    Next I

    For I = 0 To 21
        If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then

            LPrint Left$(pitcherNames$(I), 12); Tab(13);

            If pitcherRatings(I, 0) = 1 Then LPrint "R  ";
            If pitcherRatings(I, 0) = -1 Then LPrint "L  ";

            LPrint Using "### ### ### ### ## ## ## ### ### ### ### ## "; pitcherRatings(I, 19); pitcherRatings(I, 20); pitcherRatings(I, 21); pitcherRatings(I, 22); pitcherRatings(I, 23); pitcherRatings(I, 24); pitcherRatings(I, 25); pitcherRatings(I, 26); pitcherRatings(I, 27); pitcherRatings(I, 28); pitcherRatings(I, 29); pitcherRatings(I, 30);

            LPrint Using " # # 52  1 ####                                         ### "; pitcherRatings(I, 31); pitcherRatings(I, 32); pitcherRatings(I, 17); pitcherRatings(I, 14);

            If pitcherRatings(I, 20) <> 0 Then
                Z% = pitcherRatings(I, 22) / pitcherRatings(I, 20) * 1000: LPrint Using " ###"; pitcherRatings(I, 22) / pitcherRatings(I, 20) * 1000
            Else
                LPrint " 000"
            End If

        End If

    Next I

    LPrint

    LPrint "PITCHER     T  W  L  SV G  GS CG IP  H   BB  SO  ERA  GA HRA HOLD EBA"

    For I = 0 To 21
        If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then

            LPrint Mid$(pitcherNames$(I), 1, 12); Tab(13);

            If pitcherRatings(I, 0) = 1 Then LPrint "R  ";
            If pitcherRatings(I, 0) = -1 Then LPrint "L  ";

            LPrint Using "## ## ## ## ## ## ### ### ### ### #.## ## ##   #   ###"; pitcherRatings(I, 1); pitcherRatings(I, 2); pitcherRatings(I, 3); pitcherRatings(I, 4); pitcherRatings(I, 5); pitcherRatings(I, 16); pitcherRatings(I, 6); pitcherRatings(I, 7); pitcherRatings(I, 8); pitcherRatings(I, 9); pitcherRatings(I, 10) / 100; pitcherRatings(I, 12); pitcherRatings(I, 13); pitcherRatings(I, 18); pitcherRatings(I, 14)
        End If

    Next I

    LPrint Chr$(12)

End Sub


'----------------------------------------
'     CalculateRunRating subroutine
'----------------------------------------
'This subroutien gives the user a quick
'overview of run ratings and a means by
'which to review them
Sub CalculateRunRating ()

    Shared batterNames$(), pitcherNames$()
    Shared batterRatings(), pitcherRatings()

    Color 15, 0
    Cls

    For I = 0 To 22

        If batterNames$(I) <> "" And batterNames$(I) <> "XXX" Then

            SPD = 200 * batterRatings(I, 8) + batterRatings(I, 13) * 100

            If batterRatings(I, 6) + batterRatings(I, 11) - batterRatings(I, 7) - batterRatings(I, 8) - batterRatings(I, 9) = 0 Then
                RR = Int(Rnd(1) * 15) + 1
            Else
                RR = Int(SPD / (batterRatings(I, 6) + batterRatings(I, 11) - batterRatings(I, 7) - batterRatings(I, 8) - batterRatings(I, 9)) + .5)
                RR = RR * 1.4
            End If

            Select Case RR

                Case Is <= 3
                    batterRatings(I, 16) = 1

                Case 4, 5
                    batterRatings(I, 16) = 2
            
                Case 6 to 10
                    batterRatings(I, 16) = 3
            
                Case 11 to 15 
                    batterRatings(I, 16) = 4
            
                Case 16 to 20 
                    batterRatings(I, 16) = 5
            
                Case 21 to 25
                    batterRatings(I, 16) = 6
            
                Case 26 to 30
                    batterRatings(I, 16) = 7
            
                Case 31 to 40 
                    batterRatings(I, 16) = 8
            
                Case Else
                '40+
                    batterRatings(I, 16) = 9

            End Select

            Print batterNames$(I); Tab(20); batterRatings(I, 16)

        End If

    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    Cls

    For I = 0 To 21

        If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then

            RR = Int(Rnd(1) * 20) + 1
            RR = RR * 1.4

            If RR <= 4 Then pitcherRatings(I, 31) = 1
            If RR > 4 And RR <= 8 Then pitcherRatings(I, 31) = 2
            If RR > 8 And RR <= 12 Then pitcherRatings(I, 31) = 3
            If RR > 12 And RR <= 16 Then pitcherRatings(I, 31) = 4
            If RR > 16 Then pitcherRatings(I, 31) = 5

            Print pitcherNames$(I); Tab(20); pitcherRatings(I, 31)

        End If

    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'     CalculateBuntRating subroutine
'----------------------------------------
'This subroutien gives the user a quick
'overview of bunt ratings and a means by
'which to review them
Sub CalculateBuntRating ()

    Dim B1!, BUNT!
    Dim OB!
    Dim R1!, R2!
    Dim SLG!, SLG2!

    Shared batterRatings(), pitcherRatings()
    Shared batterNames$(), pitcherNames$()

    Color 15, 0
    Cls

    For I = 0 To 22

        If batterNames$(I) <> "" And batterNames$(I) <> "XXX" Then

            PA = batterRatings(I, 11) + batterRatings(I, 4)
            If PA = 0 Then PA = 1
            OB! = (batterRatings(I, 6) * 100 + PA / 2) / PA
            SLG! = (batterRatings(I, 6) + batterRatings(I, 7) + batterRatings(I, 8) * 2 + batterRatings(I, 9) * 3) * 100
            SLG2! = (SLG! + PA / 2) / PA
            R1! = (batterRatings(I, 12) * 100 + PA / 2) / PA
            R1! = 11 - R1! ^ 2 / 25
            R2! = 11 - (SLG2! - OB!) / 25
            BUNT! = (R1! + R1! + R2!) / 3

            B1! = batterRatings(I, 15) * 1000 / PA

            Select Case B1!
                Case Is <= 0: BR = 1
                Case 1: BR = 2
                Case 2 To 3: BR = 3
                Case 4 To 6: BR = 4
                Case 7 To 10: BR = 5
                Case 11 To 15: BR = 6
                Case 16 To 20: BR = 7
                Case 21 To 25: BR = 8
                Case 26 To 30: BR = 9
                Case 31 To 35: BR = 10
                Case Is >= 36: BR = 10
            End Select

            BUNT! = (BUNT! + BR) / 2
            If BUNT! < 1 Then BUNT! = 1
            If BUNT! > 9 Then BUNT! = 9
            batterRatings(I, 30) = Int(BUNT! + .5)

            If I < 22 Then
                Print batterNames$(I); Tab(20); batterRatings(I, 30)
            Else
                Print batterNames$(I); Tab(20); batterRatings(I, 30);
            End If

        End If

    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    Cls

    For I = 0 To 21

        If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then

            PA = pitcherRatings(I, 20) + pitcherRatings(I, 27)
            If PA = 0 Then PA = 1

            OB! = (pitcherRatings(I, 22) * 100 + PA / 2) / PA
            SLG! = (pitcherRatings(I, 22) + pitcherRatings(I, 23) + pitcherRatings(I, 24) * 2 + pitcherRatings(I, 25) * 3) * 100
            SLG2! = (SLG! + PA / 2) / PA

            R1! = (pitcherRatings(I, 28) * 100 + PA / 2) / PA
            R1! = 11 - R1! ^ 2 / 25
            If R1! < 1 Then R1! = 1
            If R1! > 10 Then R1! = 10

            R2! = 11 - (SLG2! - OB!) / 25
            If R2! < 1 Then R2! = 1
            If R2! > 10 Then R2! = 10

            BUNT! = (R1! + R1! + R2!) / 3
            B1! = pitcherRatings(I, 36) * 1000 / PA

            Select Case B1!
                Case Is <= 0: BR = 1
                Case 1: BR = 2
                Case 2 To 3: BR = 3
                Case 4 To 6: BR = 4
                Case 7 To 10: BR = 5
                Case 11 To 15: BR = 6
                Case 16 To 20: BR = 7
                Case 21 To 25: BR = 8
                Case 26 To 30: BR = 9
                Case 31 To 35: BR = 10
                Case Is >= 36: BR = 10
            End Select

            BUNT! = (BUNT! + BR) / 2
            If BUNT! < 1 Then BUNT! = 1
            If BUNT! > 9 Then BUNT! = 9
            pitcherRatings(I, 32) = Int(BUNT! + .5)
            Print pitcherNames$(I); Tab(20); pitcherRatings(I, 32)

        End If

    Next

    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'         EditBatter subroutine
'----------------------------------------
'This subroutine gives the user an interface
'specifically for modifying team batters
Sub EditBatter ()

    Shared batterNames$(), batterRatings()

    Shared inputINJ%()

    Do

        Color 15, 0
        Cls

        Color 14
        Print "EDIT / ENTER BATTERS"
        Color 15

        For I = 0 To 10
            Locate I + 5, 5
            Print Using "##"; I + 1;: Print " "; batterNames$(I); Tab(40);
            Print Using "##"; I + 13;: Print " "; batterNames$(I + 12)
        Next

        Locate , 5: Print Using "##"; 12;: Print " "; batterNames$(11)

        Print

        Do
            Locate 22, 1
            Print "# TO ENTER/EDIT (1-23) (0 TO CONTINUE) ";: Input idx
        Loop Until idx >= 0 And idx <= 23

        If idx > 0 Then

            I = idx - 1
            ' INITIALIZE BATTER SCREEN

            Color 15, 1
            Cls

            Color 14
            Locate 1, 30: Print "EDIT / INPUT BATTERS"
            Color 15

            Locate 3, 1: Print "NAME:"
            Locate 3, 24: Print "BATS:"
            Locate 3, 36: Print "ACTIVE:"
            Locate 3, 50: Print "INJ LENGTH:"
            Locate 5, 14: Print " G  AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
            Locate 6, 1: Print "SEASON:"
            Locate 7, 1: Print "VS LHP:"
            Locate 8, 1: Print "VS RHP:"
            Locate 10, 33: Print "FIELDING RATINGS"
            Locate 11, 31: Print "POS  FAVG  ARM RANGE"
            Locate 12, 19: Print "POSITION 1"
            Locate 13, 19: Print "POSITION 2"
            Locate 14, 19: Print "POSITION 3"
            Locate 15, 19: Print "POSITION 4"
            Locate 17, 12: Print "RUN (1-9):"
            Locate 17, 27: Print "BUNT (1-9):"
            Locate 17, 44: Print "G/A:"
            Locate 17, 54: Print "LEAGUE BAVG:"
            Locate 19, 29: Print "SPLIT STATS LEAGUE DATA"
            Locate 20, 26: Print "BAVG   K%  BB%  HR%  2B%  3B%"
            Locate 21, 13: Print "LHB vs LHP"
            Locate 22, 13: Print "LHB vs RHP"
            Locate 23, 13: Print "RHB vs LHP";
            Locate 24, 13: Print "RHB vs RHP";

            ' INITIALIZE BATTER FIELDS
            FG = 0
            BG = 7
            Color FG, BG

            Locate 3, 7: QPrint PadRight$(batterNames$(I), 15), FG, BG, -1
            If batterRatings(I, 0) = 2 Then BATS$ = "RP"
            If batterRatings(I, 0) = -2 Then BATS$ = "LP"
            If batterRatings(I, 0) = 1 Then BATS$ = "R"
            If batterRatings(I, 0) = -1 Then BATS$ = "L"
            If batterRatings(I, 0) = 0 Then BATS$ = "S"

            Locate 3, 30: QPrint PadRight$(BATS$, 2), FG, BG, -1
            ACT$ = "Y"
            If batterRatings(I, 31) = 1 Then ACT$ = "N"

            Locate 3, 44: Print ACT$
            Locate 3, 62: Print Using "###"; inputINJ%(I)
            Locate 6, 12: Print Using "###"; batterRatings(I, 3)
            Locate 6, 17: Print Using "###"; batterRatings(I, 4)
            Locate 6, 22: Print Using "###"; batterRatings(I, 5)
            Locate 6, 27: Print Using "###"; batterRatings(I, 6)
            Locate 6, 32: Print Using "##"; batterRatings(I, 7)
            Locate 6, 36: Print Using "##"; batterRatings(I, 8)
            Locate 6, 40: Print Using "##"; batterRatings(I, 9)
            Locate 6, 44: Print Using "###"; batterRatings(I, 10)
            Locate 6, 49: Print Using "###"; batterRatings(I, 11)
            Locate 6, 54: Print Using "###"; batterRatings(I, 12)
            Locate 6, 59: Print Using "###"; batterRatings(I, 13)
            Locate 6, 64: Print Using "##"; batterRatings(I, 14)
            Locate 6, 68: Print Using "##"; batterRatings(I, 15)
            Locate 7, 17: Print Using "###"; batterRatings(I, 42)
            Locate 7, 27: Print Using "###"; batterRatings(I, 43)
            Locate 7, 32: Print Using "##"; batterRatings(I, 44)
            Locate 7, 36: Print Using "##"; batterRatings(I, 45)
            Locate 7, 40: Print Using "##"; batterRatings(I, 46)
            Locate 7, 49: Print Using "###"; batterRatings(I, 47)
            Locate 7, 54: Print Using "###"; batterRatings(I, 48)
            Locate 8, 17: Print Using "###"; batterRatings(I, 49)
            Locate 8, 27: Print Using "###"; batterRatings(I, 50)
            Locate 8, 32: Print Using "##"; batterRatings(I, 51)
            Locate 8, 36: Print Using "##"; batterRatings(I, 52)
            Locate 8, 40: Print Using "##"; batterRatings(I, 53)
            Locate 8, 49: Print Using "###"; batterRatings(I, 54)
            Locate 8, 54: Print Using "###"; batterRatings(I, 55)
            Locate 12, 31: Print Using "##"; batterRatings(I, 22)
            Locate 12, 36: Print Using "####"; batterRatings(I, 26)
            Locate 12, 43: Print Using "#"; batterRatings(I, 33)
            Locate 12, 47: Print Using "#"; batterRatings(I, 34)
            Locate 13, 31: Print Using "##"; batterRatings(I, 23)
            Locate 13, 36: Print Using "####"; batterRatings(I, 27)
            Locate 13, 43: Print Using "#"; batterRatings(I, 35)
            Locate 13, 47: Print Using "#"; batterRatings(I, 36)
            Locate 14, 31: Print Using "##"; batterRatings(I, 24)
            Locate 14, 36: Print Using "####"; batterRatings(I, 28)
            Locate 14, 43: Print Using "#"; batterRatings(I, 37)
            Locate 14, 47: Print Using "#"; batterRatings(I, 38)
            Locate 15, 31: Print Using "##"; batterRatings(I, 25)
            Locate 15, 36: Print Using "####"; batterRatings(I, 29)
            Locate 15, 43: Print Using "#"; batterRatings(I, 39)
            Locate 15, 47: Print Using "#"; batterRatings(I, 40)
            Locate 17, 23: Print Using "#"; batterRatings(I, 16)
            Locate 17, 39: Print Using "#"; batterRatings(I, 30)
            Locate 17, 49: Print Using "##"; batterRatings(I, 18)
            Locate 17, 67: Print Using "###"; batterRatings(I, 20)
            Locate 21, 27: Print Using "###"; batterRatings(I, 56)
            Locate 21, 32: Print Using "###"; batterRatings(I, 57)
            Locate 21, 37: Print Using "###"; batterRatings(I, 58)
            Locate 21, 42: Print Using "###"; batterRatings(I, 59)
            Locate 21, 47: Print Using "###"; batterRatings(I, 60)
            Locate 21, 52: Print Using "###"; batterRatings(I, 61)
            Locate 22, 27: Print Using "###"; batterRatings(I, 62)
            Locate 22, 32: Print Using "###"; batterRatings(I, 63)
            Locate 22, 37: Print Using "###"; batterRatings(I, 64)
            Locate 22, 42: Print Using "###"; batterRatings(I, 65)
            Locate 22, 47: Print Using "###"; batterRatings(I, 66)
            Locate 22, 52: Print Using "###"; batterRatings(I, 67)
            Locate 23, 27: Print Using "###"; batterRatings(I, 68);
            Locate 23, 32: Print Using "###"; batterRatings(I, 69);
            Locate 23, 37: Print Using "###"; batterRatings(I, 70);
            Locate 23, 42: Print Using "###"; batterRatings(I, 71);
            Locate 23, 47: Print Using "###"; batterRatings(I, 72);
            Locate 23, 52: Print Using "###"; batterRatings(I, 73);
            Locate 24, 27: Print Using "###"; batterRatings(I, 74);
            Locate 24, 32: Print Using "###"; batterRatings(I, 75);
            Locate 24, 37: Print Using "###"; batterRatings(I, 76);
            Locate 24, 42: Print Using "###"; batterRatings(I, 77);
            Locate 24, 47: Print Using "###"; batterRatings(I, 78);
            Locate 24, 52: Print Using "###"; batterRatings(I, 79);

            BName:
            Locate 3, 7
            Max = 15
            Call TextIn(batterNames$(I), Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo BName:
            If XCode = 2 GoTo bDone

            BATS:
            Locate 3, 30
            Max = 2
            Call TextIn(BATS$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo BName:
            If XCode = 2 GoTo bDone

            BActive:
            Locate 3, 44
            Max = 1
            Call TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo BATS:
            If XCode = 2 GoTo bDone

            BINJ:
            Locate 3, 62
            Max = 3
            inj# = inputINJ%(I)
            Call NumIn(inputINJ%(I), Max, XCode, FG, BG)
            inputINJ%(I) = Int(inj#)
            Locate 3, 62: Print Using "###"; inputINJ%(I)
            If XCode = 1 GoTo BActive:
            If XCode = 2 GoTo bDone

            BGAMES:
            Locate 6, 12
            Max = 3
            draftB# = batterRatings(I, 3)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 3) = Int(draftB#)
            Locate 6, 12: Print Using "###"; batterRatings(I, 3)
            If XCode = 1 GoTo BINJ:
            If XCode = 2 GoTo bDone

            BAB:
            Locate 6, 17
            Max = 3
            draftB# = batterRatings(I, 4)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 4) = Int(draftB#)
            Locate 6, 17: Print Using "###"; batterRatings(I, 4)
            If XCode = 1 GoTo BGAMES:
            If XCode = 2 GoTo bDone

            BR:
            Locate 6, 22
            Max = 3
            draftB# = batterRatings(I, 5)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 5) = Int(draftB#)
            Locate 6, 22: Print Using "###"; batterRatings(I, 5)
            If XCode = 1 GoTo BAB:
            If XCode = 2 GoTo bDone

            BH:
            Locate 6, 27
            Max = 3
            draftB# = batterRatings(I, 6)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 6) = Int(draftB#)
            Locate 6, 27: Print Using "###"; batterRatings(I, 6)
            If XCode = 1 GoTo BR:
            If XCode = 2 GoTo bDone

            B2B:
            Locate 6, 32
            Max = 2
            draftB# = batterRatings(I, 7)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 7) = Int(draftB#)
            Locate 6, 32: Print Using "##"; batterRatings(I, 7)
            If XCode = 1 GoTo BH:
            If XCode = 2 GoTo bDone

            B3B:
            Locate 6, 36
            Max = 2
            draftB# = batterRatings(I, 8)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 8) = Int(draftB#)
            Locate 6, 36: Print Using "##"; batterRatings(I, 8)
            If XCode = 1 GoTo B2B:
            If XCode = 2 GoTo bDone

            bHR:
            Locate 6, 40
            Max = 2
            draftB# = batterRatings(I, 9)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 9) = Int(draftB#)
            Locate 6, 40: Print Using "##"; batterRatings(I, 9)
            If XCode = 1 GoTo B3B:
            If XCode = 2 GoTo bDone

            BRBI:
            Locate 6, 44
            Max = 3
            draftB# = batterRatings(I, 10)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 10) = Int(draftB#)
            Locate 6, 44: Print Using "###"; batterRatings(I, 10)
            If XCode = 1 GoTo bHR:
            If XCode = 2 GoTo bDone

            bBB:
            Locate 6, 49
            Max = 3
            draftB# = batterRatings(I, 11)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 11) = Int(draftB#)
            Locate 6, 49: Print Using "###"; batterRatings(I, 11)
            If XCode = 1 GoTo BRBI:
            If XCode = 2 GoTo bDone

            bKO:
            Locate 6, 54
            Max = 3
            draftB# = batterRatings(I, 12)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 12) = Int(draftB#)
            Locate 6, 54: Print Using "###"; batterRatings(I, 12)
            If XCode = 1 GoTo bBB:
            If XCode = 2 GoTo bDone

            bSB:
            Locate 6, 59
            Max = 3
            draftB# = batterRatings(I, 13)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 13) = Int(draftB#)
            Locate 6, 59: Print Using "###"; batterRatings(I, 13)
            If XCode = 1 GoTo bKO:
            If XCode = 2 GoTo bDone

            bCS:
            Locate 6, 64
            Max = 2
            draftB# = batterRatings(I, 14)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 14) = Int(draftB#)
            Locate 6, 64: Print Using "##"; batterRatings(I, 14)
            If XCode = 1 GoTo bSB:
            If XCode = 2 GoTo bDone

            BSH:
            Locate 6, 68
            Max = 2
            draftB# = batterRatings(I, 15)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 15) = Int(draftB#)
            Locate 6, 68: Print Using "##"; batterRatings(I, 15)
            If XCode = 1 GoTo bCS:
            If XCode = 2 GoTo bDone

            BLAB:
            Locate 7, 17
            Max = 3
            draftB# = batterRatings(I, 42)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 42) = Int(draftB#)
            Locate 7, 17: Print Using "###"; batterRatings(I, 42)
            If XCode = 1 GoTo BSH:
            If XCode = 2 GoTo bDone

            BLH:
            Locate 7, 27
            Max = 3
            draftB# = batterRatings(I, 43)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 43) = Int(draftB#)
            Locate 7, 27: Print Using "###"; batterRatings(I, 43)
            If XCode = 1 GoTo BLAB:
            If XCode = 2 GoTo bDone

            BL2B:
            Locate 7, 32
            Max = 2
            draftB# = batterRatings(I, 44)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 44) = Int(draftB#)
            Locate 7, 32: Print Using "##"; batterRatings(I, 44)
            If XCode = 1 GoTo BLH:
            If XCode = 2 GoTo bDone

            BL3B:
            Locate 7, 36
            Max = 2
            draftB# = batterRatings(I, 45)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 45) = Int(draftB#)
            Locate 7, 36: Print Using "##"; batterRatings(I, 45)
            If XCode = 1 GoTo BL2B:
            If XCode = 2 GoTo bDone

            BLHR:
            Locate 7, 40
            Max = 2
            draftB# = batterRatings(I, 46)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 46) = Int(draftB#)
            Locate 7, 40: Print Using "##"; batterRatings(I, 46)
            If XCode = 1 GoTo BL3B:
            If XCode = 2 GoTo bDone

            bLBB:
            Locate 7, 49
            Max = 3
            draftB# = batterRatings(I, 47)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 47) = Int(draftB#)
            Locate 7, 49: Print Using "###"; batterRatings(I, 47)
            If XCode = 1 GoTo BLHR:
            If XCode = 2 GoTo bDone

            BLKO:
            Locate 7, 54
            Max = 3
            draftB# = batterRatings(I, 48)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 48) = Int(draftB#)
            Locate 7, 54: Print Using "###"; batterRatings(I, 48)
            If XCode = 1 GoTo bLBB:
            If XCode = 2 GoTo bDone

            BRAB:
            Locate 8, 17
            Max = 3
            draftB# = batterRatings(I, 49)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 49) = Int(draftB#)
            Locate 8, 17: Print Using "###"; batterRatings(I, 49)
            If XCode = 1 GoTo BLKO:
            If XCode = 2 GoTo bDone

            BRH:
            Locate 8, 27
            Max = 3
            draftB# = batterRatings(I, 50)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 50) = Int(draftB#)
            Locate 8, 27: Print Using "###"; batterRatings(I, 50)
            If XCode = 1 GoTo BRAB:
            If XCode = 2 GoTo bDone

            BR2B:
            Locate 8, 32
            Max = 2
            draftB# = batterRatings(I, 51)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 51) = Int(draftB#)
            Locate 8, 32: Print Using "##"; batterRatings(I, 51)
            If XCode = 1 GoTo BRH:
            If XCode = 2 GoTo bDone

            BR3B:
            Locate 8, 36
            Max = 2
            draftB# = batterRatings(I, 52)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 52) = Int(draftB#)
            Locate 8, 36: Print Using "##"; batterRatings(I, 52)
            If XCode = 1 GoTo BR2B:
            If XCode = 2 GoTo bDone

            BRHR:
            Locate 8, 40
            Max = 2
            draftB# = batterRatings(I, 53)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 53) = Int(draftB#)
            Locate 8, 40: Print Using "##"; batterRatings(I, 53)
            If XCode = 1 GoTo BR3B:
            If XCode = 2 GoTo bDone

            bRBB:
            Locate 8, 49
            Max = 3
            draftB# = batterRatings(I, 54)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 54) = Int(draftB#)
            Locate 8, 49: Print Using "###"; batterRatings(I, 54)
            If XCode = 1 GoTo BRHR:
            If XCode = 2 GoTo bDone

            BRKO:
            Locate 8, 54
            Max = 3
            draftB# = batterRatings(I, 55)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 55) = Int(draftB#)
            Locate 8, 54: Print Using "###"; batterRatings(I, 55)
            If XCode = 1 GoTo bRBB:
            If XCode = 2 GoTo bDone

            BP1:
            Locate 12, 31
            Max = 2
            draftB# = batterRatings(I, 22)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 22) = Int(draftB#)
            Locate 12, 31: Print Using "##"; batterRatings(I, 22)
            If XCode = 1 GoTo BRKO:
            If XCode = 2 GoTo bDone

            ' STOP HERE

            BF1:
            Locate 12, 36
            Max = 4
            draftB# = batterRatings(I, 26)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 26) = Int(draftB#)
            Locate 12, 36: Print Using "####"; batterRatings(I, 26)
            If XCode = 1 GoTo BP1:
            If XCode = 2 GoTo bDone

            bA1:
            Locate 12, 43
            Max = 1
            draftB# = batterRatings(I, 33)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 33) = Int(draftB#)
            Locate 12, 43: Print Using "#"; batterRatings(I, 33)
            If XCode = 1 GoTo BF1:
            If XCode = 2 GoTo bDone

            BR1:
            Locate 12, 47
            Max = 1
            draftB# = batterRatings(I, 34)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 34) = Int(draftB#)
            Locate 12, 47: Print Using "#"; batterRatings(I, 34)
            If XCode = 1 GoTo bA1:
            If XCode = 2 GoTo bDone

            BP2:
            Locate 13, 31
            Max = 2
            draftB# = batterRatings(I, 23)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 23) = Int(draftB#)
            Locate 13, 31: Print Using "##"; batterRatings(I, 23)
            If XCode = 1 GoTo BR1:
            If XCode = 2 GoTo bDone

            BF2:
            Locate 13, 36
            Max = 4
            draftB# = batterRatings(I, 27)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 27) = Int(draftB#)
            Locate 13, 36: Print Using "####"; batterRatings(I, 27)
            If XCode = 1 GoTo BP2:
            If XCode = 2 GoTo bDone

            bA2:
            Locate 13, 43
            Max = 1
            draftB# = batterRatings(I, 35)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 35) = Int(draftB#)
            Locate 13, 43: Print Using "#"; batterRatings(I, 35)
            If XCode = 1 GoTo BF2:
            If XCode = 2 GoTo bDone

            BR2:
            Locate 13, 47
            Max = 1
            draftB# = batterRatings(I, 36)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 36) = Int(draftB#)
            Locate 13, 47: Print Using "#"; batterRatings(I, 36)
            If XCode = 1 GoTo bA2:
            If XCode = 2 GoTo bDone

            BP3:
            Locate 14, 31
            Max = 2
            draftB# = batterRatings(I, 24)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 24) = Int(draftB#)
            Locate 14, 31: Print Using "##"; batterRatings(I, 24)
            If XCode = 1 GoTo BR2:
            If XCode = 2 GoTo bDone

            BF3:
            Locate 14, 36
            Max = 4
            draftB# = batterRatings(I, 28)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 28) = Int(draftB#)
            Locate 14, 36: Print Using "####"; batterRatings(I, 28)
            If XCode = 1 GoTo BP3:
            If XCode = 2 GoTo bDone

            bA3:
            Locate 14, 43
            Max = 1
            draftB# = batterRatings(I, 37)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 37) = Int(draftB#)
            Locate 14, 43: Print Using "#"; batterRatings(I, 37)
            If XCode = 1 GoTo BF3:
            If XCode = 2 GoTo bDone

            BR3:
            Locate 14, 47
            Max = 1
            draftB# = batterRatings(I, 38)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 38) = Int(draftB#)
            Locate 14, 47: Print Using "#"; batterRatings(I, 38)
            If XCode = 1 GoTo bA3:
            If XCode = 2 GoTo bDone

            BP4:
            Locate 15, 31
            Max = 2
            draftB# = batterRatings(I, 25)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 25) = Int(draftB#)
            Locate 15, 31: Print Using "##"; batterRatings(I, 25)
            If XCode = 1 GoTo BR3:
            If XCode = 2 GoTo bDone

            BF4:
            Locate 15, 36
            Max = 4
            draftB# = batterRatings(I, 29)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 29) = Int(draftB#)
            Locate 15, 36: Print Using "####"; batterRatings(I, 29)
            If XCode = 1 GoTo BP4:
            If XCode = 2 GoTo bDone

            bA4:
            Locate 15, 43
            Max = 1
            draftB# = batterRatings(I, 39)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 39) = Int(draftB#)
            Locate 15, 43: Print Using "#"; batterRatings(I, 39)
            If XCode = 1 GoTo BF4:
            If XCode = 2 GoTo bDone

            bR4:
            Locate 15, 47
            Max = 1
            draftB# = batterRatings(I, 40)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 40) = Int(draftB#)
            Locate 15, 47: Print Using "#"; batterRatings(I, 40)
            If XCode = 1 GoTo bA4:
            If XCode = 2 GoTo bDone

            bRUN:
            Locate 17, 23
            Max = 1
            draftB# = batterRatings(I, 16)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 16) = Int(draftB#)
            Locate 17, 23: Print Using "#"; batterRatings(I, 16)
            If XCode = 1 GoTo bR4:
            If XCode = 2 GoTo bDone

            bBUNT:
            Locate 17, 39
            Max = 1
            draftB# = batterRatings(I, 30)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 30) = Int(draftB#)
            Locate 17, 39: Print Using "#"; batterRatings(I, 30)
            If XCode = 1 GoTo bRUN:
            If XCode = 2 GoTo bDone

            bGA:
            Locate 17, 49
            Max = 2
            draftB# = batterRatings(I, 18)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 18) = Int(draftB#)
            Locate 17, 49: Print Using "##"; batterRatings(I, 18)
            If XCode = 1 GoTo bBUNT:
            If XCode = 2 GoTo bDone

            bLA:
            Locate 17, 67
            Max = 3
            draftB# = batterRatings(I, 29)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 20) = Int(draftB#)
            Locate 17, 67: Print Using "###"; batterRatings(I, 20)
            If XCode = 1 GoTo bGA:
            If XCode = 2 GoTo bDone

            bLLBA:
            Locate 21, 27
            Max = 3
            draftB# = batterRatings(I, 56)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 56) = Int(draftB#)
            Locate 22, 27: Print Using "###"; batterRatings(I, 56)
            If XCode = 1 GoTo bLA:
            If XCode = 2 GoTo bDone

            bLL2B:
            Locate 21, 32
            Max = 3
            draftB# = batterRatings(I, 57)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 57) = Int(draftB#)
            Locate 22, 32: Print Using "###"; batterRatings(I, 57)
            If XCode = 1 GoTo bLLBA:
            If XCode = 2 GoTo bDone

            bLL3B:
            Locate 21, 37
            Max = 3
            draftB# = batterRatings(I, 58)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 58) = Int(draftB#)
            Locate 22, 37: Print Using "###"; batterRatings(I, 58)
            If XCode = 1 GoTo bLL2B:
            If XCode = 2 GoTo bDone

            bLLHR:
            Locate 21, 42
            Max = 3
            draftB# = batterRatings(I, 59)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 59) = Int(draftB#)
            Locate 22, 42: Print Using "###"; batterRatings(I, 59)
            If XCode = 1 GoTo bLL3B:
            If XCode = 2 GoTo bDone

            bLLBB:
            Locate 21, 47
            Max = 3
            draftB# = batterRatings(I, 60)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 60) = Int(draftB#)
            Locate 22, 47: Print Using "###"; batterRatings(I, 60)
            If XCode = 1 GoTo bLLHR:
            If XCode = 2 GoTo bDone

            bLLKO:
            Locate 21, 52
            Max = 3
            draftB# = batterRatings(I, 61)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 61) = Int(draftB#)
            Locate 22, 52: Print Using "###"; batterRatings(I, 61)
            If XCode = 1 GoTo bLLBB:
            If XCode = 2 GoTo bDone

            bLRBA:
            Locate 22, 27
            Max = 3
            draftB# = batterRatings(I, 62)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 62) = Int(draftB#)
            Locate 23, 27: Print Using "###"; batterRatings(I, 62)
            If XCode = 1 GoTo bLLKO:
            If XCode = 2 GoTo bDone

            bLR2B:
            Locate 22, 32
            Max = 3
            draftB# = batterRatings(I, 63)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 63) = Int(draftB#)
            Locate 23, 32: Print Using "###"; batterRatings(I, 63)
            If XCode = 1 GoTo bLRBA:
            If XCode = 2 GoTo bDone

            bLR3B:
            Locate 22, 37
            Max = 3
            draftB# = batterRatings(I, 64)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 64) = Int(draftB#)
            Locate 23, 37: Print Using "###"; batterRatings(I, 64)
            If XCode = 1 GoTo bLR2B:
            If XCode = 2 GoTo bDone

            bLRHR:
            Locate 22, 42
            Max = 3
            draftB# = batterRatings(I, 65)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 65) = Int(draftB#)
            Locate 23, 42: Print Using "###"; batterRatings(I, 65)
            If XCode = 1 GoTo bLR3B:
            If XCode = 2 GoTo bDone

            bLRBB:
            Locate 22, 47
            Max = 3
            draftB# = batterRatings(I, 66)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 66) = Int(draftB#)
            Locate 23, 47: Print Using "###"; batterRatings(I, 66)
            If XCode = 1 GoTo bLRHR:
            If XCode = 2 GoTo bDone

            bLRKO:
            Locate 22, 52
            Max = 3
            draftB# = batterRatings(I, 67)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 67) = Int(draftB#)
            Locate 23, 52: Print Using "###"; batterRatings(I, 67)
            If XCode = 1 GoTo bLRBB:
            If XCode = 2 GoTo bDone

            bRLBA:
            Locate 23, 27
            Max = 3
            draftB# = batterRatings(I, 68)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 68) = Int(draftB#)
            Locate 24, 27: Print Using "###"; batterRatings(I, 68);
            If XCode = 1 GoTo bLRKO:
            If XCode = 2 GoTo bDone

            bRL2B:
            Locate 23, 32
            Max = 3
            draftB# = batterRatings(I, 69)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 69) = Int(draftB#)
            Locate 24, 32: Print Using "###"; batterRatings(I, 69);
            If XCode = 1 GoTo bRLBA:
            If XCode = 2 GoTo bDone

            bRL3B:
            Locate 23, 37
            Max = 3
            draftB# = batterRatings(I, 70)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 70) = Int(draftB#)
            Locate 24, 37: Print Using "###"; batterRatings(I, 70);
            If XCode = 1 GoTo bRL2B:
            If XCode = 2 GoTo bDone

            bRLHR:
            Locate 23, 42
            Max = 3
            draftB# = batterRatings(I, 71)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 71) = Int(draftB#)
            Locate 24, 42: Print Using "###"; batterRatings(I, 71);
            If XCode = 1 GoTo bRL3B:
            If XCode = 2 GoTo bDone

            bRLBB:
            Locate 23, 47
            Max = 3
            draftB# = batterRatings(I, 72)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 72) = Int(draftB#)
            Locate 24, 47: Print Using "###"; batterRatings(I, 72);
            If XCode = 1 GoTo bRLHR:
            If XCode = 2 GoTo bDone

            bRLKO:
            Locate 23, 52
            Max = 3
            draftB# = batterRatings(I, 73)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 73) = Int(draftB#)
            Locate 24, 52: Print Using "###"; batterRatings(I, 73);
            If XCode = 1 GoTo bRLBB:
            If XCode = 2 GoTo bDone

            bRRBA:
            Locate 24, 27
            Max = 3
            draftB# = batterRatings(I, 74)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 74) = Int(draftB#)
            Locate 25, 27: Print Using "###"; batterRatings(I, 74);
            If XCode = 1 GoTo bRLKO:
            If XCode = 2 GoTo bDone

            bRR2B:
            Locate 24, 32
            Max = 3
            draftB# = batterRatings(I, 75)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 75) = Int(draftB#)
            Locate 25, 32: Print Using "###"; batterRatings(I, 75);
            If XCode = 1 GoTo bRRBA:
            If XCode = 2 GoTo bDone

            bRR3B:
            Locate 24, 37
            Max = 3
            draftB# = batterRatings(I, 76)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 76) = Int(draftB#)
            Locate 25, 37: Print Using "###"; batterRatings(I, 76);
            If XCode = 1 GoTo bRR2B:
            If XCode = 2 GoTo bDone

            bRRHR:
            Locate 24, 42
            Max = 3
            draftB# = batterRatings(I, 77)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 77) = Int(draftB#)
            Locate 25, 42: Print Using "###"; batterRatings(I, 77);
            If XCode = 1 GoTo bRR3B:
            If XCode = 2 GoTo bDone

            bRRBB:
            Locate 24, 47
            Max = 3
            draftB# = batterRatings(I, 78)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 78) = Int(draftB#)
            Locate 25, 47: Print Using "###"; batterRatings(I, 78);
            If XCode = 1 GoTo bRRHR:
            If XCode = 2 GoTo bDone

            bRRKO:
            Locate 24, 52
            Max = 3
            draftB# = batterRatings(I, 79)
            Call NumIn(draftB#, Max, XCode, FG, BG)
            batterRatings(I, 79) = Int(draftB#)
            Locate 25, 52: Print Using "###"; batterRatings(I, 79);
            If XCode = 1 GoTo bRRBB:

            bDone:
            Color 14, 1
            Locate 25, 1
            Print "Is this okay? (Yes/No) ";
            X$ = ""
            Call YesNoB(X$, XCode, FG, BG)
            Locate , 1
            If XCode = 1 GoTo bRRKO
            If X$ = "N" GoTo BName
            If X$ <> "Y" GoTo bDone 'insist on either Y or N or up-arrow

            batterRatings(I, 0) = 0
            BATS$ = _Trim$(BATS$)
            If UCase$(BATS$) = "L" Then batterRatings(I, 0) = -1
            If UCase$(BATS$) = "R" Then batterRatings(I, 0) = 1
            If UCase$(BATS$) = "LP" Then batterRatings(I, 0) = -2
            If UCase$(BATS$) = "RP" Then batterRatings(I, 0) = 2

            ACT$ = _Trim$(ACT$)
            batterRatings(I, 31) = 0
            If UCase$(ACT$) = "N" Then batterRatings(I, 31) = 1

        End If

    Loop Until idx = 0

End Sub


'----------------------------------------
'        EditTeamInfo subroutine
'----------------------------------------
'This subroutine gives the user an interface
'specifically for modifying team ratings
Sub EditTeamInfo (YN$, T$, teamIdx)

    Shared CK, parkHR

    Shared Manager$, teamAbbrev$, Stadium$

    Shared teamRatings%(), teamSA%()

    Do

        Color 15, 1

        Cls

        Color 14
        Locate 1, 36: Print "TEAM DATA"
        Color 15
        Locate 3, 1: Print "TEAM NUMBER:";
        Locate 5, 1: Print "LEAGUE BATTING AVERAGE:";
        Locate 6, 1: Print "LEAGUE STRIKEOUT %    :";
        Locate 7, 1: Print "LEAGUE WALK PERCENTAGE:";
        Locate 8, 1: Print "LEAGUE HOME RUN %     :";
        Locate 10, 1: Print "DBL PLAYS/GAME X 100  :";

        Locate 13, 1: Print "PARK NAME:";
        Locate 14, 1: Print "PARK ID  :";
        Locate 15, 1: Print "DOUBLES RATING  :";
        Locate 16, 1: Print "TRIPLES RATING  :";
        Locate 17, 1: Print "HOME RUN RATING :";
        Locate 18, 1: Print "FOUL BALL RATING:";
        Locate 20, 1: Print "OUTDOOR(0)/INDOOR(1):";
        Locate 21, 1: Print "GRASS(0)/TURF(1)    :";
        Locate 23, 1: Print "MANAGER :";

        Locate 18, 29: Print "TEXT COLOR 0-15  :";
        Locate 19, 29: Print "BCKGRND COLOR 0-7:";
        Locate 3, 25: Print "DISK ID:";
        Locate 3, 44: Print "TEAM NAME: ";
        Locate 4, 44: Print "TEAM NICKNAME:";
        Locate 6, 51: Print "SPLIT STATS LEAGUE DATA";

        Color 0, 7
        Locate 3, 14: Print Using "###"; teamIdx
        Locate 5, 25: Print Using "###"; teamRatings%(1)
        Locate 6, 25: Print Using "###"; teamRatings%(2)
        Locate 7, 25: Print Using "###"; teamRatings%(3)
        Locate 8, 25: Print Using "###"; teamRatings%(4)
        Locate 10, 25: Print Using "###"; teamRatings%(5)
        Locate 13, 12: Print Stadium$
        Locate 14, 12: Print Using "###"; teamRatings%(13)
        Locate 15, 19: Print Using "###"; teamRatings%(6)
        Locate 16, 19: Print Using "###"; teamRatings%(7)
        Locate 17, 19: Print Using "###"; parkHR
        Locate 18, 19: Print Using "###"; teamRatings%(8)
        Locate 20, 23: Print Using "#"; teamRatings%(9)
        Locate 21, 23: Print Using "#"; teamRatings%(10)
        Locate 23, 11: Print Manager$;
        Locate 18, 48: Print Using "##"; teamRatings%(11)
        Locate 19, 48: Print Using "##"; teamRatings%(12)
        Locate 3, 34: Print YN$
        Locate 3, 55: Print T$
        Locate 4, 59: Print teamAbbrev$

        Color 15, 1
        Locate 7, 50: Print "BAVG   K%  BB%  HR%  2B%  3B%"
        Locate 8, 38: Print "LHB vs LHP"
        Color 0, 7
        Locate 8, 50: Print Using "###"; teamSA%(1)
        Locate 8, 55: Print Using "###"; teamSA%(2)
        Locate 8, 60: Print Using "###"; teamSA%(3)
        Locate 8, 65: Print Using "###"; teamSA%(4)
        Locate 8, 70: Print Using "###"; teamSA%(5)
        Locate 8, 75: Print Using "###"; teamSA%(6)
        Color 15, 1

        Locate 9, 38: Print "LHB vs RHP"
        Color 0, 7
        Locate 9, 50: Print Using "###"; teamSA%(7)
        Locate 9, 55: Print Using "###"; teamSA%(8)
        Locate 9, 60: Print Using "###"; teamSA%(9)
        Locate 9, 65: Print Using "###"; teamSA%(10)
        Locate 9, 70: Print Using "###"; teamSA%(11)
        Locate 9, 75: Print Using "###"; teamSA%(12)
        Color 15, 1

        Locate 10, 38: Print "RHB vs LHP"
        Color 0, 7
        Locate 10, 50: Print Using "###"; teamSA%(13)
        Locate 10, 55: Print Using "###"; teamSA%(14)
        Locate 10, 60: Print Using "###"; teamSA%(15)
        Locate 10, 65: Print Using "###"; teamSA%(16)
        Locate 10, 70: Print Using "###"; teamSA%(17)
        Locate 10, 75: Print Using "###"; teamSA%(18)
        Color 15, 1


        Locate 11, 38: Print "RHB vs RHP"
        Color 0, 7
        Locate 11, 50: Print Using "###"; teamSA%(19)
        Locate 11, 55: Print Using "###"; teamSA%(20)
        Locate 11, 60: Print Using "###"; teamSA%(21)
        Locate 11, 65: Print Using "###"; teamSA%(22)
        Locate 11, 70: Print Using "###"; teamSA%(23)
        Locate 11, 75: Print Using "###"; teamSA%(24)
        Color 15, 1

        Locate 25, 1: Print "ESC-WILL END EDITING";

        FG = 0
        BG = 7
        Color FG, BG

        TeamNum:
        Locate 3, 14
        Max = 3
        T# = teamIdx
        Call NumIn(T#, Max, XCode, FG, BG)
        teamIdx = Int(T#)
        Locate 3, 14: Print Using "###"; teamIdx
        If XCode = 1 GoTo TeamNum
        If XCode = 2 GoTo Done

        DiskID:
        Locate 3, 34
        Max = 3
        Call TextIn(YN$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo TeamNum:
        If XCode = 2 GoTo Done

        TeamName:
        Locate 3, 55
        Max = 15
        Call TextIn(T$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo DiskID
        If XCode = 2 GoTo Done

        Abbrev:
        Locate 4, 59
        Max = 5
        Call TextIn(teamAbbrev$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo TeamName
        If XCode = 2 GoTo Done

        LgBA:
        Locate 5, 25
        Max = 3
        L# = teamRatings%(1)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(1) = Int(L#)
        Locate 5, 25: Print Using "###"; teamRatings%(1)
        If XCode = 1 GoTo Abbrev
        If XCode = 2 GoTo Done

        LgKO:
        Locate 6, 25
        Max = 3
        L# = teamRatings%(2)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(2) = Int(L#)
        Locate 6, 25: Print Using "###"; teamRatings%(2)
        If XCode = 1 GoTo LgBA
        If XCode = 2 GoTo Done

        LgBB:
        Locate 7, 25
        Max = 3
        L# = teamRatings%(3)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(3) = Int(L#)
        Locate 7, 25: Print Using "###"; teamRatings%(3)
        If XCode = 1 GoTo LgKO:
        If XCode = 2 GoTo Done

        LgHR:
        Locate 8, 25
        Max = 3
        L# = teamRatings%(4)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(4) = Int(L#)
        Locate 8, 25: Print Using "###"; teamRatings%(4)
        If XCode = 1 GoTo LgBB:
        If XCode = 2 GoTo Done

        LgDP:
        Locate 10, 25
        Max = 3
        L# = teamRatings%(5)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(5) = Int(L#)
        Locate 10, 25: Print Using "###"; teamRatings%(5)
        If XCode = 1 GoTo LgHR:
        If XCode = 2 GoTo Done

        ParkName:
        Locate 13, 12
        Max = 40
        Call TextIn(Stadium$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo LgDP:
        If XCode = 2 GoTo Done

        ParkID:
        Locate 14, 12
        Max = 3
        L# = teamRatings%(13)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(13) = Int(L#)
        Locate 14, 12: Print Using "###"; teamRatings%(13)
        If XCode = 1 GoTo ParkName:
        If XCode = 2 GoTo Done

        DoubleRating:
        Locate 15, 19
        Max = 3
        L# = teamRatings%(6)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(6) = Int(L#)
        Locate 15, 19: Print Using "###"; teamRatings%(6)
        If XCode = 1 GoTo ParkID:
        If XCode = 2 GoTo Done

        TripleRating:
        Locate 16, 19
        Max = 3
        L# = teamRatings%(7)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(7) = Int(L#)
        Locate 16, 19: Print Using "###"; teamRatings%(7)
        If XCode = 1 GoTo DoubleRating:
        If XCode = 2 GoTo Done

        Homer:
        Locate 17, 19
        Max = 3
        L# = parkHR
        Call NumIn(L#, Max, XCode, FG, BG)
        pakHR = Int(L#)
        Locate 17, 19: Print Using "###"; parkHR
        If XCode = 1 GoTo TripleRating:
        If XCode = 2 GoTo Done

        Foul:
        Locate 18, 19
        Max = 3
        L# = teamRatings%(8)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(8) = Int(L#)
        Locate 18, 19: Print Using "###"; teamRatings%(8)
        If XCode = 1 GoTo Homer:
        If XCode = 2 GoTo Done

        Outdoor:
        Locate 20, 23
        Max = 1
        L# = teamRatings%(9)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(9) = Int(L#)
        Locate 20, 23: Print Using "#"; teamRatings%(9)
        If XCode = 1 GoTo Foul:
        If XCode = 2 GoTo Done

        Grass:
        Locate 21, 23
        Max = 1
        L# = teamRatings%(10)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(10) = Int(L#)
        Locate 21, 23: Print Using "#"; teamRatings%(10)
        If XCode = 1 GoTo Outdoor:
        If XCode = 2 GoTo Done

        Manager:
        Locate 23, 11
        Max = 15
        Call TextIn(Manager$, Max, NumOnly, CapsOn, XCode, FG, BG)
        If XCode = 1 GoTo Grass:
        If XCode = 2 GoTo Done

        Text:
        Locate 18, 48
        Max = 2
        L# = teamRatings%(11)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(11) = Int(L#)
        Locate 18, 48: Print Using "##"; teamRatings%(11)
        If XCode = 1 GoTo Manager:
        If XCode = 2 GoTo Done

        Bgrnd:
        Locate 19, 48
        Max = 2
        L# = teamRatings%(12)
        Call NumIn(L#, Max, XCode, FG, BG)
        teamRatings%(12) = Int(L#)
        Locate 19, 48: Print Using "##"; teamRatings%(12)
        If XCode = 1 GoTo Text:
        If XCode = 2 GoTo Done

        Locate 20, 29
        Color 15, 1
        Print "TEAM APPEARANCE: ";
        Color teamRatings%(11), teamRatings%(12): Print T$
        Color 0, 7

        LLBA:
        Locate 8, 50
        Max = 3
        SA# = teamSA%(1)
        Call NumIn(teamSA%(1), Max, XCode, FG, BG)
        teamSA%(1) = Int(SA#)
        Locate 8, 50: Print Using "###"; teamSA%(1)
        If XCode = 1 GoTo Bgrnd:
        If XCode = 2 GoTo Done

        LL2B:
        Locate 8, 55
        Max = 3
        SA# = teamSA%(2)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(2) = Int(SA#)
        Locate 8, 55: Print Using "###"; teamSA%(2)
        If XCode = 1 GoTo LLBA:
        If XCode = 2 GoTo Done

        LL3B:
        Locate 8, 60
        Max = 3
        SA# = teamSA%(3)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(3) = Int(SA#)
        Locate 8, 60: Print Using "###"; teamSA%(3)
        If XCode = 1 GoTo LL2B:
        If XCode = 2 GoTo Done

        LLHR:
        Locate 8, 65
        Max = 3
        SA# = teamSA%(4)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(4) = Int(SA#)
        Locate 8, 65: Print Using "###"; teamSA%(4)
        If XCode = 1 GoTo LL3B:
        If XCode = 2 GoTo Done

        LLBB:
        Locate 8, 70
        Max = 3
        SA# = teamSA%(5)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(5) = Int(SA#)
        Locate 8, 70: Print Using "###"; teamSA%(5)
        If XCode = 1 GoTo LLHR:
        If XCode = 2 GoTo Done

        LLKO:
        Locate 8, 75
        Max = 3
        SA# = teamSA%(6)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(6) = Int(SA#)
        Locate 8, 75: Print Using "###"; teamSA%(6)
        If XCode = 1 GoTo LLBB:
        If XCode = 2 GoTo Done

        LRBA:
        Locate 9, 50
        Max = 3
        SA# = teamSA%(7)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(7) = Int(SA#)
        Locate 9, 50: Print Using "###"; teamSA%(7)
        If XCode = 1 GoTo LLKO:
        If XCode = 2 GoTo Done

        LR2B:
        Locate 9, 55
        Max = 3
        SA# = teamSA%(8)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(8) = Int(SA#)
        Locate 9, 55: Print Using "###"; teamSA%(8)
        If XCode = 1 GoTo LRBA:
        If XCode = 2 GoTo Done

        LR3B:
        Locate 9, 60
        Max = 3
        SA# = teamSA%(9)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(9) = Int(SA#)
        Locate 9, 60: Print Using "###"; teamSA%(9)
        If XCode = 1 GoTo LR2B:
        If XCode = 2 GoTo Done

        LRHR:
        Locate 9, 65
        Max = 3
        SA# = teamSA%(10)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(10) = Int(SA#)
        Locate 9, 65: Print Using "###"; teamSA%(10)
        If XCode = 1 GoTo LR3B:
        If XCode = 2 GoTo Done

        LRBB:
        Locate 9, 70
        Max = 3
        SA# = teamSA%(11)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(11) = Int(SA#)
        Locate 9, 70: Print Using "###"; teamSA%(11)
        If XCode = 1 GoTo LRHR:
        If XCode = 2 GoTo Done

        LRKO:
        Locate 9, 75
        Max = 3
        SA# = teamSA%(12)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(12) = Int(SA#)
        Locate 9, 75: Print Using "###"; teamSA%(12)
        If XCode = 1 GoTo LRBB:
        If XCode = 2 GoTo Done

        RLBA:
        Locate 10, 50
        Max = 3
        SA# = teamSA%(13)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(13) = Int(SA#)
        Locate 10, 50: Print Using "###"; teamSA%(13)
        If XCode = 1 GoTo LRKO:
        If XCode = 2 GoTo Done

        RL2B:
        Locate 10, 55
        Max = 3
        SA# = teamSA%(14)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(14) = Int(SA#)
        Locate 10, 55: Print Using "###"; teamSA%(14)
        If XCode = 1 GoTo RLBA:
        If XCode = 2 GoTo Done

        RL3B:
        Locate 10, 60
        Max = 3
        SA# = teamSA%(15)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(15) = Int(SA#)
        Locate 10, 60: Print Using "###"; teamSA%(15)
        If XCode = 1 GoTo RL2B:
        If XCode = 2 GoTo Done

        RLHR:
        Locate 10, 65
        Max = 3
        SA# = teamSA%(16)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(16) = Int(SA#)
        Locate 10, 65: Print Using "###"; teamSA%(16)
        If XCode = 1 GoTo RL3B:
        If XCode = 2 GoTo Done

        RLBB:
        Locate 10, 70
        Max = 3
        SA# = teamSA%(17)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(17) = Int(SA#)
        Locate 10, 70: Print Using "###"; teamSA%(17)
        If XCode = 1 GoTo RLHR:
        If XCode = 2 GoTo Done

        RLKO:
        Locate 10, 75
        Max = 3
        SA# = teamSA%(18)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(18) = Int(SA#)
        Locate 10, 75: Print Using "###"; teamSA%(18)
        If XCode = 1 GoTo RLBB:
        If XCode = 2 GoTo Done

        RRBA:
        Locate 11, 50
        Max = 3
        SA# = teamSA%(19)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(19) = Int(SA#)
        Locate 11, 50: Print Using "###"; teamSA%(19)
        If XCode = 1 GoTo RLKO:
        If XCode = 2 GoTo Done

        RR2B:
        Locate 11, 55
        Max = 3
        SA# = teamSA%(20)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(20) = Int(SA#)
        Locate 11, 55: Print Using "###"; teamSA%(20)
        If XCode = 1 GoTo RRBA:
        If XCode = 2 GoTo Done

        RR3B:
        Locate 11, 60
        Max = 3
        SA# = teamSA%(21)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(21) = Int(SA#)
        Locate 11, 60: Print Using "###"; teamSA%(21)
        If XCode = 1 GoTo RR2B:
        If XCode = 2 GoTo Done

        RRHR:
        Locate 11, 65
        Max = 3
        SA# = teamSA%(22)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(22) = Int(SA#)
        Locate 11, 65: Print Using "###"; teamSA%(22)
        If XCode = 1 GoTo RR3B:
        If XCode = 2 GoTo Done

        RRBB:
        Locate 11, 70
        Max = 3
        SA# = teamSA%(23)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(23) = Int(SA#)
        Locate 11, 70: Print Using "###"; teamSA%(23)
        If XCode = 1 GoTo RRHR:
        If XCode = 2 GoTo Done

        RRKO:
        Locate 11, 75
        Max = 3
        SA# = teamSA%(24)
        Call NumIn(SA#, Max, XCode, FG, BG)
        teamSA%(24) = Int(SA#)
        Locate 11, 75: Print Using "###"; teamSA%(24)
        If XCode = 1 GoTo RRBB:

        Done:
        'Do
        Color 14, 1
        Locate 24, 1
        'Print "Is this okay? (Y)es/(N)o/(A)bort ";
        Print "Is this okay? (Y)es/(N)o";
        X$ = ""
        Call YesNoB(X$, XCode, FG, BG)
        Locate , 1
        ClearEOL 14, 1
        If XCode = 1 GoTo RRKO

        If UCase$(X$) = "N" Then GoTo TeamNum
        If UCase$(X$) <> "Y" GoTo Done 'insist on either Y or N or up-arrow

        validData = 1

        If teamIdx = 0 Then validData = 0
        If T$ = "" Then validData = 0

        If validData = 0 Then
            MsgText$ = "TEAM INDEX OR TEAM NAME NOT COMPLETED. PRESS ANY KEY TO RETURN TO DATA ENTRY"
            BoxWidth = 26
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        End If

    Loop Until validData = 1

End Sub


'----------------------------------------
'         EditPitcher subroutine
'----------------------------------------
'This subroutine gives the user an interface
'specifically for modifying team pitchers
Sub EditPitcher ()

    Shared pitcherNames$()
    Shared pitcherRatings()

    Do

        Color 15, 0
        Cls
        Color 14
        Print "EDIT / ENTER PITCHERS"
        Color 15

        For I = 0 To 10
            Locate I + 5, 5: Print Using "##"; I + 1;
            Print " "; pitcherNames$(I); Tab(40);
            Print Using "##"; I + 12;
            Print " "; pitcherNames$(I + 11)
        Next

        Do
            Locate 22, 1
            Print "# TO EDIT/ENTER (1-22) (0 TO CONTINUE) ";
            Input idx
        Loop Until idx >= 0 And idx <= 22

        If idx > 0 Then

            'adjust value for 0 index array
            I = idx - 1

            ' INITIALIZE PITCHER SCREEN
            Color 15, 1
            Cls
            Locate 1, 1: Print "NAME:"
            Locate 1, 24: Print "THROWS:"
            Locate 1, 36: Print "ACTIVE:"
            Locate 1, 48: Print "INJ LENGTH:"
            Locate 1, 66: Print "HOLD (1-9):"
            Locate 3, 34: Print "PITCHING STATS"
            Locate 4, 11: Print "W   L  SV    G  GS  CG   IP    H   BB    K  WP   ERA  HRA"
            Locate 5, 1: Print "SEASON:"
            Locate 6, 27: Print "G/A:"
            Locate 6, 37: Print "LEAGUE BAVG:"
            Locate 8, 31: Print "SPLIT STATS AGAINST"
            Locate 9, 26: Print "AB    H  2B  3B  HR   BB    K"
            Locate 10, 1: Print "VS LHB:"
            Locate 11, 1: Print "VS RHB:"
            Locate 13, 30: Print "HITTING STATISTICS"
            Locate 14, 9: Print "RUN (1-9):"
            Locate 14, 24: Print "BUNT (1-9):"
            Locate 14, 39: Print "BATTING AVG:"
            Locate 14, 58: Print "FLDG AVG:"
            Locate 15, 14: Print "G   AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
            Locate 16, 1: Print "SEASON:"
            Locate 17, 1: Print "VS LHP:"
            Locate 18, 1: Print "VS RHP:"
            Locate 20, 29: Print "SPLIT STATS LEAGUE DATA"
            Locate 21, 26: Print "BAVG   K%  BB%  HR%  2B%  3B%"
            Locate 22, 13: Print "LHB vs LHP"
            Locate 23, 13: Print "LHB vs RHP"
            Locate 24, 13: Print "RHB vs LHP";
            Locate 25, 13: Print "RHB vs RHP";

            ' INITIALIZE PITCHER FIELDS
            'FG, BG = 112
            FG = 0
            BG = 7

            Locate 1, 7: QPrint PadRight$(pitcherNames$(I), 15), FG, BG, -1

            THROW$ = "R"
            If pitcherRatings(I, 0) = -1 Then THROW$ = "L"
            Locate 1, 32: QPrint PadRight$(THROW$, 1), FG, BG, -1

            ACT$ = "Y"
            If pitcherRatings(I, 34) = 1 Then ACT$ = "N"
            Locate 1, 44: QPrint PadRight$(ACT$, 1), FG, BG, -1

            Call PITCHINIT(I, FG, BG)

            PName:
            Locate 1, 7
            CapsOn = 0: NumOnly = 0: Max = 15
            Call TextIn(pitcherNames$(I), Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo PName:
            If XCode = 2 GoTo PDone

            THROW:
            Locate 1, 32
            Max = 1
            Call TextIn(THROW$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo PName:
            If XCode = 2 GoTo PDone

            PActive:
            Locate 1, 44
            Max = 1
            Call TextIn(ACT$, Max, NumOnly, CapsOn, XCode, FG, BG)
            If XCode = 1 GoTo THROW:
            If XCode = 2 GoTo PDone

            PINJ:
            Locate 1, 60
            Max = 3
            draftP# = pitcherRatings(I, 35)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 35) = Int(draftP#)
            Locate 1, 60: Print Using "###"; pitcherRatings(I, 35)
            If XCode = 1 GoTo PActive:
            If XCode = 2 GoTo PDone

            PHOLD:
            Locate 1, 78
            Max = 1
            draftP# = pitcherRatings(I, 18)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 18) = Int(draftP#)
            Locate 1, 78: Print Using "#"; pitcherRatings(I, 18)
            If XCode = 1 GoTo PINJ:
            If XCode = 2 GoTo PDone

            PWINS:
            Locate 5, 10
            Max = 2
            draftP# = pitcherRatings(I, 1)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 1) = Int(draftP#)
            Locate 5, 10: Print Using "##"; pitcherRatings(I, 1)
            If XCode = 1 GoTo PHOLD:
            If XCode = 2 GoTo PDone

            PLOSSES:
            Locate 5, 14
            Max = 2
            draftP# = pitcherRatings(I, 2)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 2) = Int(draftP#)
            Locate 5, 14: Print Using "##"; pitcherRatings(I, 2)
            If XCode = 1 GoTo PWINS:
            If XCode = 2 GoTo PDone

            PSAVES:
            Locate 5, 18
            Max = 2
            draftP# = pitcherRatings(I, 3)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 3) = Int(draftP#)
            Locate 5, 18: Print Using "##"; pitcherRatings(I, 3)
            If XCode = 1 GoTo PLOSSES:
            If XCode = 2 GoTo PDone

            PGAMES:
            Locate 5, 22
            Max = 3
            draftP# = pitcherRatings(I, 4)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 4) = Int(draftP#)
            Locate 5, 22: Print Using "###"; pitcherRatings(I, 4)
            If XCode = 1 GoTo PSAVES:
            If XCode = 2 GoTo PDone

            PSTARTS:
            Locate 5, 27
            Max = 2
            draftP# = pitcherRatings(I, 5)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 5) = Int(draftP#)
            Locate 5, 27: Print Using "##"; pitcherRatings(I, 5)
            If XCode = 1 GoTo PGAMES:
            If XCode = 2 GoTo PDone

            PCG:
            Locate 5, 31
            Max = 2
            draftP# = pitcherRatings(I, 16)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 16) = Int(draftP#)
            Locate 5, 31: Print Using "##"; pitcherRatings(I, 16)
            If XCode = 1 GoTo PSTARTS:
            If XCode = 2 GoTo PDone

            PIP:
            Locate 5, 35
            Max = 3
            draftP# = pitcherRatings(I, 6)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 6) = Int(draftP#)
            Locate 5, 35: Print Using "###"; pitcherRatings(I, 6)
            If XCode = 1 GoTo PCG:
            If XCode = 2 GoTo PDone

            PH:
            Locate 5, 40
            Max = 3
            draftP# = pitcherRatings(I, 7)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 7) = Int(draftP#)
            Locate 5, 40: Print Using "###"; pitcherRatings(I, 7)
            If XCode = 1 GoTo PIP:
            If XCode = 2 GoTo PDone

            PBB:
            Locate 5, 45
            Max = 3
            draftP# = pitcherRatings(I, 8)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 8) = Int(draftP#)
            Locate 5, 45: Print Using "###"; pitcherRatings(I, 8)
            If XCode = 1 GoTo PH:
            If XCode = 2 GoTo PDone

            PKO:
            Locate 5, 50
            Max = 3
            draftP# = pitcherRatings(I, 9)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 9) = Int(draftP#)
            Locate 5, 50: Print Using "###"; pitcherRatings(I, 9)
            If XCode = 1 GoTo PBB:
            If XCode = 2 GoTo PDone

            PWP:
            Locate 5, 55
            Max = 2
            draftP# = pitcherRatings(I, 33)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 33) = Int(draftP#)
            Locate 5, 55: Print Using "##"; pitcherRatings(I, 33)
            If XCode = 1 GoTo PKO:
            If XCode = 2 GoTo PDone

            PERA:
            Locate 5, 59
            Max = 4
            draftP# = pitcherRatings(I, 10)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 10) = Int(draftP#)
            Locate 5, 59: Print Using "####"; pitcherRatings(I, 10)
            If XCode = 1 GoTo PWP:
            If XCode = 2 GoTo PDone

            PHRA:
            Locate 5, 66
            Max = 2
            draftP# = pitcherRatings(I, 13)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 13) = Int(draftP#)
            Locate 5, 66: Print Using "##"; pitcherRatings(I, 13)
            If XCode = 1 GoTo PERA:
            If XCode = 2 GoTo PDone

            PGA:
            Locate 6, 32
            Max = 2
            draftP# = pitcherRatings(I, 12)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 12) = Int(draftP#)
            Locate 6, 32: Print Using "##"; pitcherRatings(I, 12)
            If XCode = 1 GoTo PHRA:
            If XCode = 2 GoTo PDone

            PLBA:
            Locate 6, 50
            Max = 3
            draftP# = pitcherRatings(I, 14)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 14) = Int(draftP#)
            Locate 6, 50: Print Using "###"; pitcherRatings(I, 14)
            If XCode = 1 GoTo PHRA:
            If XCode = 2 GoTo PDone

            PLAB:
            Locate 10, 25
            Max = 3
            draftP# = pitcherRatings(I, 51)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 51) = Int(draftP#)
            Locate 10, 25: Print Using "###"; pitcherRatings(I, 51)
            If XCode = 1 GoTo PLBA:
            If XCode = 2 GoTo PDone

            PLH:
            Locate 10, 30
            Max = 3
            draftP# = pitcherRatings(I, 52)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 52) = Int(draftP#)
            Locate 10, 30: Print Using "###"; pitcherRatings(I, 52)
            If XCode = 1 GoTo PLAB:
            If XCode = 2 GoTo PDone

            PL2B:
            Locate 10, 35
            Max = 2
            draftP# = pitcherRatings(I, 53)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 53) = Int(draftP#)
            Locate 10, 35: Print Using "##"; pitcherRatings(I, 53)
            If XCode = 1 GoTo PLH:
            If XCode = 2 GoTo PDone

            PL3B:
            Locate 10, 39
            Max = 2
            draftP# = pitcherRatings(I, 54)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 54) = Int(draftP#)
            Locate 10, 39: Print Using "##"; pitcherRatings(I, 54)
            If XCode = 1 GoTo PL2B:
            If XCode = 2 GoTo PDone

            PLHR:
            Locate 10, 43
            Max = 2
            draftP# = pitcherRatings(I, 55)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 55) = Int(draftP#)
            Locate 10, 43: Print Using "##"; pitcherRatings(I, 55)
            If XCode = 1 GoTo PL3B:
            If XCode = 2 GoTo PDone

            PLBB:
            Locate 10, 47
            Max = 3
            draftP# = pitcherRatings(I, 56)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 56) = Int(draftP#)
            Locate 10, 47: Print Using "###"; pitcherRatings(I, 56)
            If XCode = 1 GoTo PLHR:
            If XCode = 2 GoTo PDone

            PLKO:
            Locate 10, 52
            Max = 3
            draftP# = pitcherRatings(I, 57)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 57) = Int(draftP#)
            Locate 10, 52: Print Using "###"; pitcherRatings(I, 57)
            If XCode = 1 GoTo PLBB:
            If XCode = 2 GoTo PDone

            PRAB:
            Locate 11, 25
            Max = 3
            draftP# = pitcherRatings(I, 58)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 58) = Int(draftP#)
            Locate 11, 25: Print Using "###"; pitcherRatings(I, 58)
            If XCode = 1 GoTo PLKO:
            If XCode = 2 GoTo PDone

            PRH:
            Locate 11, 30
            Max = 3
            draftP# = pitcherRatings(I, 59)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 59) = Int(draftP#)
            Locate 11, 30: Print Using "###"; pitcherRatings(I, 59)
            If XCode = 1 GoTo PRAB:
            If XCode = 2 GoTo PDone

            PR2B:
            Locate 11, 35
            Max = 2
            draftP# = pitcherRatings(I, 60)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 60) = Int(draftP#)
            Locate 11, 35: Print Using "##"; pitcherRatings(I, 60)
            If XCode = 1 GoTo PRH:
            If XCode = 2 GoTo PDone

            PR3B:
            Locate 11, 39
            Max = 2
            draftP# = pitcherRatings(I, 61)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 61) = Int(draftP#)
            Locate 11, 39: Print Using "##"; pitcherRatings(I, 61)
            If XCode = 1 GoTo PR2B:
            If XCode = 2 GoTo PDone

            PRHR:
            Locate 11, 43
            Max = 2
            draftP# = pitcherRatings(I, 62)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 62) = Int(draftP#)
            Locate 11, 43: Print Using "##"; pitcherRatings(I, 62)
            If XCode = 1 GoTo PR3B:
            If XCode = 2 GoTo PDone

            PRBB:
            Locate 11, 47
            Max = 3
            draftP# = pitcherRatings(I, 63)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 63) = Int(draftP#)
            Locate 11, 47: Print Using "###"; pitcherRatings(I, 63)
            If XCode = 1 GoTo PRHR:
            If XCode = 2 GoTo PDone

            PRKO:
            Locate 11, 52
            Max = 3
            draftP# = pitcherRatings(I, 64)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 64) = Int(draftP#)
            Locate 11, 52: Print Using "###"; pitcherRatings(I, 64)
            If XCode = 1 GoTo PRBB:
            If XCode = 2 GoTo PDone

            PRUN:
            Locate 14, 20
            Max = 1
            draftP# = pitcherRatings(I, 31)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 31) = Int(draftP#)
            Locate 14, 20: Print Using "#"; pitcherRatings(I, 31)
            If XCode = 1 GoTo PRKO:
            If XCode = 2 GoTo PDone

            PBUNT:
            Locate 14, 36
            Max = 1
            draftP# = pitcherRatings(I, 32)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 32) = Int(draftP#)
            Locate 14, 36: Print Using "#"; pitcherRatings(I, 32)
            If XCode = 1 GoTo PRKO:
            If XCode = 2 GoTo PDone

            PBAVG:
            Locate 14, 52
            Max = 3
            draftP# = pitcherRatings(I, 11)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 11) = Int(draftP#)
            Locate 14, 52: Print Using "###"; pitcherRatings(I, 11)
            If XCode = 1 GoTo PBUNT:
            If XCode = 2 GoTo PDone

            PFAVG:
            Locate 14, 68
            Max = 4
            draftP# = pitcherRatings(I, 17)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 17) = Int(draftP#)
            Locate 14, 68: Print Using "####"; pitcherRatings(I, 17)
            If XCode = 1 GoTo PBAVG:
            If XCode = 2 GoTo PDone

            PBGAMES:
            Locate 16, 12
            Max = 3
            draftP# = pitcherRatings(I, 19)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 19) = Int(draftP#)
            Locate 16, 12: Print Using "###"; pitcherRatings(I, 19)
            If XCode = 1 GoTo PFAVG:
            If XCode = 2 GoTo PDone

            PAB:
            Locate 16, 17
            Max = 3
            draftP# = pitcherRatings(I, 20)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 20) = Int(draftP#)
            Locate 16, 17: Print Using "###"; pitcherRatings(I, 20)
            If XCode = 1 GoTo PBGAMES:
            If XCode = 2 GoTo PDone

            PR:
            Locate 16, 22
            Max = 3
            draftP# = pitcherRatings(I, 21)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 21) = Int(draftP#)
            Locate 16, 22: Print Using "###"; pitcherRatings(I, 21)
            If XCode = 1 GoTo PAB:
            If XCode = 2 GoTo PDone

            PBH:
            Locate 16, 27
            Max = 3
            draftP# = pitcherRatings(I, 22)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 22) = Int(draftP#)
            Locate 16, 27: Print Using "###"; pitcherRatings(I, 22)
            If XCode = 1 GoTo PR:
            If XCode = 2 GoTo PDone

            P2B:
            Locate 16, 32
            Max = 2
            draftP# = pitcherRatings(I, 23)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 23) = Int(draftP#)
            Locate 16, 32: Print Using "##"; pitcherRatings(I, 23)
            If XCode = 1 GoTo PBH:
            If XCode = 2 GoTo PDone

            P3B:
            Locate 16, 36
            Max = 2
            draftP# = pitcherRatings(I, 24)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 24) = Int(draftP#)
            Locate 16, 36: Print Using "##"; pitcherRatings(I, 24)
            If XCode = 1 GoTo P2B:
            If XCode = 2 GoTo PDone

            PHR:
            Locate 16, 40
            Max = 2
            draftP# = pitcherRatings(I, 25)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 25) = Int(draftP#)
            Locate 16, 40: Print Using "##"; pitcherRatings(I, 25)
            If XCode = 1 GoTo P3B:
            If XCode = 2 GoTo PDone

            PRBI:
            Locate 16, 44
            Max = 3
            draftP# = pitcherRatings(I, 26)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 26) = Int(draftP#)
            Locate 16, 44: Print Using "###"; pitcherRatings(I, 26)
            If XCode = 1 GoTo PHR:
            If XCode = 2 GoTo PDone

            PBBB:
            Locate 16, 49
            Max = 3
            draftP# = pitcherRatings(I, 27)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 27) = Int(draftP#)
            Locate 16, 49: Print Using "###"; pitcherRatings(I, 27)
            If XCode = 1 GoTo PRBI:
            If XCode = 2 GoTo PDone

            PBKO:
            Locate 16, 54
            Max = 3
            draftP# = pitcherRatings(I, 28)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 28) = Int(draftP#)
            Locate 16, 54: Print Using "###"; pitcherRatings(I, 28)
            If XCode = 1 GoTo PBBB:
            If XCode = 2 GoTo PDone

            PSB:
            Locate 16, 59
            Max = 3
            draftP# = pitcherRatings(I, 29)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 29) = Int(draftP#)
            Locate 16, 59: Print Using "###"; pitcherRatings(I, 29)
            If XCode = 1 GoTo PBKO:
            If XCode = 2 GoTo PDone

            PCS:
            Locate 16, 64
            Max = 2
            draftP# = pitcherRatings(I, 30)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 30) = Int(draftP#)
            Locate 16, 64: Print Using "##"; pitcherRatings(I, 30)
            If XCode = 1 GoTo PSB:
            If XCode = 2 GoTo PDone

            PSH:
            Locate 16, 68
            Max = 2
            draftP# = pitcherRatings(I, 36)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 36) = Int(draftP#)
            Locate 16, 68: Print Using "##"; pitcherRatings(I, 36)
            If XCode = 1 GoTo PCS:
            If XCode = 2 GoTo PDone

            PPLAB:
            Locate 17, 17
            Max = 3
            draftP# = pitcherRatings(I, 37)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 37) = Int(draftP#)
            Locate 17, 17: Print Using "###"; pitcherRatings(I, 37)
            If XCode = 1 GoTo PSH:
            If XCode = 2 GoTo PDone

            PPLH:
            Locate 17, 27
            Max = 3
            draftP# = pitcherRatings(I, 38)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 38) = Int(draftP#)
            Locate 17, 27: Print Using "###"; pitcherRatings(I, 38)
            If XCode = 1 GoTo PPLAB:
            If XCode = 2 GoTo PDone

            PPL2B:
            Locate 17, 32
            Max = 2
            draftP# = pitcherRatings(I, 39)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 39) = Int(draftP#)
            Locate 17, 32: Print Using "##"; pitcherRatings(I, 39)
            If XCode = 1 GoTo PPLH:
            If XCode = 2 GoTo PDone

            PPL3B:
            Locate 17, 36
            Max = 2
            draftP# = pitcherRatings(I, 40)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 40) = Int(draftP#)
            Locate 17, 36: Print Using "##"; pitcherRatings(I, 40)
            If XCode = 1 GoTo PPL2B:
            If XCode = 2 GoTo PDone

            PPLHR:
            Locate 17, 40
            Max = 2
            draftP# = pitcherRatings(I, 41)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 41) = Int(draftP#)
            Locate 17, 40: Print Using "##"; pitcherRatings(I, 41)
            If XCode = 1 GoTo PPL3B:
            If XCode = 2 GoTo PDone

            PPLBB:
            Locate 17, 49
            Max = 3
            draftP# = pitcherRatings(I, 42)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 42) = Int(draftP#)
            Locate 17, 49: Print Using "###"; pitcherRatings(I, 42)
            If XCode = 1 GoTo PPLHR:
            If XCode = 2 GoTo PDone

            PPLKO:
            Locate 17, 54
            Max = 3
            draftP# = pitcherRatings(I, 43)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 43) = Int(draftP#)
            Locate 17, 54: Print Using "###"; pitcherRatings(I, 43)
            If XCode = 1 GoTo PPLBB:
            If XCode = 2 GoTo PDone

            PPRAB:
            Locate 18, 17
            Max = 3
            draftP# = pitcherRatings(I, 44)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 44) = Int(draftP#)
            Locate 18, 17: Print Using "###"; pitcherRatings(I, 44)
            If XCode = 1 GoTo PPLKO:
            If XCode = 2 GoTo PDone

            PPRH:
            Locate 18, 27
            Max = 3
            draftP# = pitcherRatings(I, 45)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 45) = Int(draftP#)
            Locate 18, 27: Print Using "###"; pitcherRatings(I, 45)
            If XCode = 1 GoTo PPRAB:
            If XCode = 2 GoTo PDone

            PPR2B:
            Locate 18, 32
            Max = 2
            draftP# = pitcherRatings(I, 46)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 46) = Int(draftP#)
            Locate 18, 32: Print Using "##"; pitcherRatings(I, 46)
            If XCode = 1 GoTo PPRH:
            If XCode = 2 GoTo PDone

            PPR3B:
            Locate 18, 36
            Max = 2
            draftP# = pitcherRatings(I, 47)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 47) = Int(draftP#)
            Locate 18, 36: Print Using "##"; pitcherRatings(I, 47)
            If XCode = 1 GoTo PPR2B:
            If XCode = 2 GoTo PDone

            PPRHR:
            Locate 18, 40
            Max = 2
            draftP# = pitcherRatings(I, 48)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 48) = Int(draftP#)
            Locate 18, 40: Print Using "##"; pitcherRatings(I, 48)
            If XCode = 1 GoTo PPR3B:
            If XCode = 2 GoTo PDone

            PPRBB:
            Locate 18, 49
            Max = 3
            draftP# = pitcherRatings(I, 49)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 49) = Int(draftP#)
            Locate 18, 49: Print Using "###"; pitcherRatings(I, 49)
            If XCode = 1 GoTo PPRHR:
            If XCode = 2 GoTo PDone

            PPRKO:
            Locate 18, 54
            Max = 3
            draftP# = pitcherRatings(I, 50)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 50) = Int(draftP#)
            Locate 18, 54: Print Using "###"; pitcherRatings(I, 50)
            If XCode = 1 GoTo PPRBB:
            If XCode = 2 GoTo PDone

            PLLBA:
            Locate 22, 27
            Max = 3
            draftP# = pitcherRatings(I, 65)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 65) = Int(draftP#)
            Locate 22, 27: Print Using "###"; pitcherRatings(I, 65)
            If XCode = 1 GoTo PPRKO:
            If XCode = 2 GoTo PDone

            PLL2B:
            Locate 22, 32
            Max = 3
            draftP# = pitcherRatings(I, 66)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 66) = Int(draftP#)
            Locate 22, 32: Print Using "###"; pitcherRatings(I, 66)
            If XCode = 1 GoTo PLLBA:
            If XCode = 2 GoTo PDone

            PLL3B:
            Locate 22, 37
            Max = 3
            draftP# = pitcherRatings(I, 67)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 67) = Int(draftP#)
            Locate 22, 37: Print Using "###"; pitcherRatings(I, 67)
            If XCode = 1 GoTo PLL2B:
            If XCode = 2 GoTo PDone

            PLLHR:
            Locate 22, 42
            Max = 3
            draftP# = pitcherRatings(I, 68)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 68) = Int(draftP#)
            Locate 22, 42: Print Using "###"; pitcherRatings(I, 68)
            If XCode = 1 GoTo PLL3B:
            If XCode = 2 GoTo PDone

            PLLBB:
            Locate 22, 47
            Max = 3
            draftP# = pitcherRatings(I, 69)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 69) = Int(draftP#)
            Locate 22, 47: Print Using "###"; pitcherRatings(I, 69)
            If XCode = 1 GoTo PLLHR:
            If XCode = 2 GoTo PDone

            PLLKO:
            Locate 22, 52
            Max = 3
            draftP# = pitcherRatings(I, 70)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 70) = Int(draftP#)
            Locate 22, 52: Print Using "###"; pitcherRatings(I, 70)
            If XCode = 1 GoTo PLLBB:
            If XCode = 2 GoTo PDone

            PLRBA:
            Locate 23, 27
            Max = 3
            draftP# = pitcherRatings(I, 71)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 71) = Int(draftP#)
            Locate 23, 27: Print Using "###"; pitcherRatings(I, 71)
            If XCode = 1 GoTo PLLKO:
            If XCode = 2 GoTo PDone

            PLR2B:
            Locate 23, 32
            Max = 3
            draftP# = pitcherRatings(I, 72)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 72) = Int(draftP#)
            Locate 23, 32: Print Using "###"; pitcherRatings(I, 72)
            If XCode = 1 GoTo PLRBA:
            If XCode = 2 GoTo PDone

            PLR3B:
            Locate 23, 37
            Max = 3
            draftP# = pitcherRatings(I, 73)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 73) = Int(draftP#)
            Locate 23, 37: Print Using "###"; pitcherRatings(I, 73)
            If XCode = 1 GoTo PLR2B:
            If XCode = 2 GoTo PDone

            PLRHR:
            Locate 23, 42
            Max = 3
            draftP# = pitcherRatings(I, 74)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 74) = Int(draftP#)
            Locate 23, 42: Print Using "###"; pitcherRatings(I, 74)
            If XCode = 1 GoTo PLR3B:
            If XCode = 2 GoTo PDone

            PLRBB:
            Locate 23, 47
            Max = 3
            draftP# = pitcherRatings(I, 75)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 75) = Int(draftP#)
            Locate 23, 47: Print Using "###"; pitcherRatings(I, 75)
            If XCode = 1 GoTo PLRHR:
            If XCode = 2 GoTo PDone

            PLRKO:
            Locate 23, 52
            Max = 3
            draftP# = pitcherRatings(I, 76)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 76) = Int(draftP#)
            Locate 23, 52: Print Using "###"; pitcherRatings(I, 76)
            If XCode = 1 GoTo PLRBB:
            If XCode = 2 GoTo PDone

            PRLBA:
            Locate 24, 27
            Max = 3
            draftP# = pitcherRatings(I, 77)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 77) = Int(draftP#)
            Locate 24, 27: Print Using "###"; pitcherRatings(I, 77);
            If XCode = 1 GoTo PLRKO:
            If XCode = 2 GoTo PDone

            PRL2B:
            Locate 24, 32
            Max = 3
            draftP# = pitcherRatings(I, 78)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 78) = Int(draftP#)
            Locate 24, 32: Print Using "###"; pitcherRatings(I, 78);
            If XCode = 1 GoTo PRLBA:
            If XCode = 2 GoTo PDone

            PRL3B:
            Locate 24, 37
            Max = 3
            draftP# = pitcherRatings(I, 79)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 79) = Int(draftP#)
            Locate 24, 37: Print Using "###"; pitcherRatings(I, 79);
            If XCode = 1 GoTo PRL2B:
            If XCode = 2 GoTo PDone

            PRLHR:
            Locate 24, 42
            Max = 3
            draftP# = pitcherRatings(I, 80)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 80) = Int(draftP#)
            Locate 24, 42: Print Using "###"; pitcherRatings(I, 80);
            If XCode = 1 GoTo PRL3B:
            If XCode = 2 GoTo PDone

            PRLBB:
            Locate 24, 47
            Max = 3
            draftP# = pitcherRatings(I, 81)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 81) = Int(draftP#)
            Locate 24, 47: Print Using "###"; pitcherRatings(I, 81);
            If XCode = 1 GoTo PRLHR:
            If XCode = 2 GoTo PDone

            PRLKO:
            Locate 24, 52
            Max = 3
            draftP# = pitcherRatings(I, 82)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 82) = Int(draftP#)
            Locate 24, 52: Print Using "###"; pitcherRatings(I, 82);
            If XCode = 1 GoTo PRLBB:
            If XCode = 2 GoTo PDone

            PRRBA:
            Locate 25, 27
            Max = 3
            draftP# = pitcherRatings(I, 83)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 83) = Int(draftP#)
            Locate 25, 27: Print Using "###"; pitcherRatings(I, 83);
            If XCode = 1 GoTo PRLKO:
            If XCode = 2 GoTo PDone

            PRR2B:
            Locate 25, 32
            Max = 3
            draftP# = pitcherRatings(I, 84)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 84) = Int(draftP#)
            Locate 25, 32: Print Using "###"; pitcherRatings(I, 84);
            If XCode = 1 GoTo PRRBA:
            If XCode = 2 GoTo PDone

            PRR3B:
            Locate 25, 37
            Max = 3
            draftP# = pitcherRatings(I, 85)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 85) = Int(draftP#)
            Locate 25, 37: Print Using "###"; pitcherRatings(I, 85);
            If XCode = 1 GoTo PRR2B:
            If XCode = 2 GoTo PDone

            PRRHR:
            Locate 25, 42
            Max = 3
            draftP# = pitcherRatings(I, 86)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 86) = Int(draftP#)
            Locate 25, 42: Print Using "###"; pitcherRatings(I, 86);
            If XCode = 1 GoTo PRR3B:
            If XCode = 2 GoTo PDone

            PRRBB:
            Locate 25, 47
            Max = 3
            draftP# = pitcherRatings(I, 87)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 87) = Int(draftP#)
            Locate 25, 47: Print Using "###"; pitcherRatings(I, 87);
            If XCode = 1 GoTo PRRHR:
            If XCode = 2 GoTo PDone

            PRRKO:
            Locate 25, 52
            Max = 3
            draftP# = pitcherRatings(I, 88)
            Call NumIn(draftP#, Max, XCode, FG, BG)
            pitcherRatings(I, 88) = Int(draftP#)
            Locate 25, 52: Print Using "###"; pitcherRatings(I, 88);
            If XCode = 1 GoTo PRRBB:

            PDone:
            Locate 25, 1
            Print "Is this okay? (Yes/No) ";
            X$ = ""
            Call YesNoB(X$, XCode, FG, BG)
            Locate , 1
            ClearEOL 14, 1
            If XCode = 1 GoTo PRRKO
            If X$ = "N" GoTo PName
            If X$ <> "Y" GoTo PDone 'insist on either Y or N or up-arrow

            pitcherRatings(I, 0) = 1
            If UCase$(_Trim$(THROW$)) = "L" Then pitcherRatings(I, 0) = -1

            pitcherRatings(I, 34) = 0
            If UCase$(_Trim$(ACT$)) = "N" Then pitcherRatings(I, 34) = 1

        End If

    Loop Until idx = 0

End Sub

'----------------------------------------
'       PITCHINIT subroutine
'----------------------------------------
'This subroutine simply summarizes all of
'the initial pitcher ratings using a very
'complex on-screen layout.
Sub PITCHINIT (i, FG, BG)

    Shared pitcherRatings()

    Color FG, BG

    Locate 1, 60: Print Using "###"; pitcherRatings(i, 35)
    Locate 1, 78: Print Using "#"; pitcherRatings(i, 18)
    Locate 5, 10: Print Using "##"; pitcherRatings(i, 1)
    Locate 5, 14: Print Using "##"; pitcherRatings(i, 2)
    Locate 5, 18: Print Using "##"; pitcherRatings(i, 3)
    Locate 5, 22: Print Using "###"; pitcherRatings(i, 4)
    Locate 5, 27: Print Using "##"; pitcherRatings(i, 5)
    Locate 5, 31: Print Using "##"; pitcherRatings(i, 16)
    Locate 5, 35: Print Using "###"; pitcherRatings(i, 6)
    Locate 5, 40: Print Using "###"; pitcherRatings(i, 7)
    Locate 5, 45: Print Using "###"; pitcherRatings(i, 8)
    Locate 5, 50: Print Using "###"; pitcherRatings(i, 9)
    Locate 5, 55: Print Using "##"; pitcherRatings(i, 33)
    Locate 5, 59: Print Using "####"; pitcherRatings(i, 10)
    Locate 5, 66: Print Using "##"; pitcherRatings(i, 13)
    Locate 6, 32: Print Using "##"; pitcherRatings(i, 12)
    Locate 6, 50: Print Using "###"; pitcherRatings(i, 14)
    Locate 10, 25: Print Using "###"; pitcherRatings(i, 51)
    Locate 10, 30: Print Using "###"; pitcherRatings(i, 52)
    Locate 10, 35: Print Using "##"; pitcherRatings(i, 53)
    Locate 10, 39: Print Using "##"; pitcherRatings(i, 54)
    Locate 10, 43: Print Using "##"; pitcherRatings(i, 55)
    Locate 10, 47: Print Using "###"; pitcherRatings(i, 56)
    Locate 10, 52: Print Using "###"; pitcherRatings(i, 57)
    Locate 11, 25: Print Using "###"; pitcherRatings(i, 58)
    Locate 11, 30: Print Using "###"; pitcherRatings(i, 59)
    Locate 11, 35: Print Using "##"; pitcherRatings(i, 60)
    Locate 11, 39: Print Using "##"; pitcherRatings(i, 61)
    Locate 11, 43: Print Using "##"; pitcherRatings(i, 62)
    Locate 11, 47: Print Using "###"; pitcherRatings(i, 63)
    Locate 11, 52: Print Using "###"; pitcherRatings(i, 64)
    Locate 14, 20: Print Using "#"; pitcherRatings(i, 31)
    Locate 14, 36: Print Using "#"; pitcherRatings(i, 32)
    Locate 14, 52: Print Using "###"; pitcherRatings(i, 11)
    Locate 14, 68: Print Using "####"; pitcherRatings(i, 17)
    Locate 16, 12: Print Using "###"; pitcherRatings(i, 19)
    Locate 16, 17: Print Using "###"; pitcherRatings(i, 20)
    Locate 16, 22: Print Using "###"; pitcherRatings(i, 21)
    Locate 16, 27: Print Using "###"; pitcherRatings(i, 22)
    Locate 16, 32: Print Using "##"; pitcherRatings(i, 23)
    Locate 16, 36: Print Using "##"; pitcherRatings(i, 24)
    Locate 16, 40: Print Using "##"; pitcherRatings(i, 25)
    Locate 16, 44: Print Using "###"; pitcherRatings(i, 26)
    Locate 16, 49: Print Using "###"; pitcherRatings(i, 27)
    Locate 16, 54: Print Using "###"; pitcherRatings(i, 28)
    Locate 16, 59: Print Using "###"; pitcherRatings(i, 29)
    Locate 16, 64: Print Using "##"; pitcherRatings(i, 30)
    Locate 16, 68: Print Using "##"; pitcherRatings(i, 36)
    Locate 17, 17: Print Using "###"; pitcherRatings(i, 37)
    Locate 17, 27: Print Using "###"; pitcherRatings(i, 38)
    Locate 17, 32: Print Using "##"; pitcherRatings(i, 39)
    Locate 17, 36: Print Using "##"; pitcherRatings(i, 40)
    Locate 17, 40: Print Using "##"; pitcherRatings(i, 41)
    Locate 17, 49: Print Using "###"; pitcherRatings(i, 42)
    Locate 17, 54: Print Using "###"; pitcherRatings(i, 43)
    Locate 18, 17: Print Using "###"; pitcherRatings(i, 44)
    Locate 18, 27: Print Using "###"; pitcherRatings(i, 45)
    Locate 18, 32: Print Using "##"; pitcherRatings(i, 46)
    Locate 18, 36: Print Using "##"; pitcherRatings(i, 47)
    Locate 18, 40: Print Using "##"; pitcherRatings(i, 48)
    Locate 18, 49: Print Using "###"; pitcherRatings(i, 49)
    Locate 18, 54: Print Using "###"; pitcherRatings(i, 50)
    Locate 22, 27: Print Using "###"; pitcherRatings(i, 65)
    Locate 22, 32: Print Using "###"; pitcherRatings(i, 66)
    Locate 22, 37: Print Using "###"; pitcherRatings(i, 67)
    Locate 22, 42: Print Using "###"; pitcherRatings(i, 68)
    Locate 22, 47: Print Using "###"; pitcherRatings(i, 69)
    Locate 22, 52: Print Using "###"; pitcherRatings(i, 70)
    Locate 23, 27: Print Using "###"; pitcherRatings(i, 71)
    Locate 23, 32: Print Using "###"; pitcherRatings(i, 72)
    Locate 23, 37: Print Using "###"; pitcherRatings(i, 73)
    Locate 23, 42: Print Using "###"; pitcherRatings(i, 74)
    Locate 23, 47: Print Using "###"; pitcherRatings(i, 75)
    Locate 23, 52: Print Using "###"; pitcherRatings(i, 76)
    Locate 24, 27: Print Using "###"; pitcherRatings(i, 77);
    Locate 24, 32: Print Using "###"; pitcherRatings(i, 78);
    Locate 24, 37: Print Using "###"; pitcherRatings(i, 79);
    Locate 24, 42: Print Using "###"; pitcherRatings(i, 80);
    Locate 24, 47: Print Using "###"; pitcherRatings(i, 81);
    Locate 24, 52: Print Using "###"; pitcherRatings(i, 82);
    Locate 25, 27: Print Using "###"; pitcherRatings(i, 83);
    Locate 25, 32: Print Using "###"; pitcherRatings(i, 84);
    Locate 25, 37: Print Using "###"; pitcherRatings(i, 85);
    Locate 25, 42: Print Using "###"; pitcherRatings(i, 86);
    Locate 25, 47: Print Using "###"; pitcherRatings(i, 87);
    Locate 25, 52: Print Using "###"; pitcherRatings(i, 88);

End Sub

'----------------------------------------
'       ViewHitters subroutine
'----------------------------------------
'This subroutine give the user a summary
'of each hitter and gives them an oppor-
'tunity to view corresponding fielding
'stats
Sub ViewHitters (teamName$)

    Shared batterNames$()
    Shared batterRatings(), TB%()
    Shared statI2!

    Do

        Cls

        Color 12: Print teamName$
        Color 14
        Print "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"

        For I = 0 To 22

            If I = 20 Then
                Locate 23
                Color 11
                Print "HIT ANY KEY TO CONTINUE"
                I$ = GetKeyPress$

                Cls

                Color 12: Print teamName$
                Color 14
                Print "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"
            End If

            Color 3

            If batterNames$(I) <> "XXX" Then

                Print Left$(batterNames$(I), 10);

                Color 7

                Select Case batterRatings(I, 0)

                    Case -2: Print Tab(12); "LP";
                    Case -1: Print Tab(12); "L";
                    Case 0: Print Tab(12); "S";
                    Case 1: Print Tab(12); "R";
                    Case 2: Print Tab(12); "RP";

                End Select

                Print Tab(14); Using "#"; batterRatings(I, 22);

                If batterRatings(I, 23) <> -1 Then
                    Print Using "#"; batterRatings(I, 23);
                    If batterRatings(I, 24) <> -1 Then
                        Print Using "#"; batterRatings(I, 24);
                        If batterRatings(I, 25) <> -1 Then
                            Print Using "#"; batterRatings(I, 25);
                        End If
                    End If
                End If

                Print Tab(20); Using "###"; batterRatings(I, 3);
                Print Tab(24); Using "###"; batterRatings(I, 4);
                Print Tab(28); Using "###"; batterRatings(I, 5);
                Print Tab(32); Using "###"; batterRatings(I, 6);
                Print Tab(36); Using "##"; batterRatings(I, 7);
                Print Tab(39); Using "##"; batterRatings(I, 8);
                Print Tab(42); Using "##"; batterRatings(I, 9);
                Print Tab(45); Using "###"; batterRatings(I, 10);
                Print Tab(49); Using "###"; batterRatings(I, 11);
                Print Tab(53); Using "###"; batterRatings(I, 12);
                Print Tab(57); Using "###"; batterRatings(I, 13);
                Print Tab(61); Using "##"; batterRatings(I, 14);
                Print Tab(64); Using "##"; batterRatings(I, 18);

                statI2! = 0

                If batterRatings(I, 4) > 0 Then statI2! = Int((batterRatings(I, 6) / batterRatings(I, 4) + .0005) * 1000)
                If batterRatings(I, 4) = 0 Or statI2! = 0 Then Print Tab(68); ".000"
                If statI2! > 0 And statI2! < 100 Then Print Tab(68); ".0";: Print Tab(70); Using "##"; statI2!
                If statI2! > 100 Then Print Tab(68); ".";: Print Tab(69); Using "###"; statI2!

            End If

        Next

        Print
        Color 14
        Print "TOTALS";
        Locate , 23: Print Using "####    ####   ###    ###    ####    ###     .###"; TB%(1); TB%(3); TB%(5); TB%(7); TB%(9); TB%(11); TB%(3) / TB%(1)
        Locate , 27: Print Using "####   ####   ###     ###     ###"; TB%(2); TB%(4); TB%(6); TB%(8); TB%(10)

        Color 11
        Locate 20: Print "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        If I$ = Chr$(9) Then
            Call FieldingStats (teamName$)
            Color 11
            Locate 20: Print "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
            I$ = GetKeyPress$
            'TAB goes back to previous screen
            'any key goes to options selection (EXIT SUB)
        End If

    Loop Until I$ <> Chr$(9)

End Sub


'----------------------------------------
'       ViewPitchers subroutine
'----------------------------------------
'This subroutine give the user a summary
'of each team pitcher.
Sub ViewPitchers (teamName$)

    Shared pitcherNames$()
    Shared pitcherRatings(), inputTP%()

    Cls

    Color 12: Print teamName$
    Color 14
    Print "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"

    numberAppearances = 0

    For I = 0 To 21

        If I = 18 Then
            Color 11
            Locate 23: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

            Cls

            Color 12: Print teamName$
            Color 14
            Print "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"
        End If

        Color 3

        If pitcherNames$(I) <> "XXX" Then

            numberAppearances = numberAppearances + pitcherRatings(I, 4)
            Print Left$(pitcherNames$(I), 10);

            Color 7

            If pitcherRatings(I, 0) = -1 Then Print Tab(14); "L";
            If pitcherRatings(I, 0) = 1 Then Print Tab(14); "R";

            Print Tab(16); Using "##"; pitcherRatings(I, 1);
            Print Tab(19); Using "##"; pitcherRatings(I, 2);
            Print Tab(22); Using "##"; pitcherRatings(I, 3);
            Print Tab(25); Using "##"; pitcherRatings(I, 4);
            Print Tab(28); Using "##"; pitcherRatings(I, 5);
            Print Tab(31); Using "###"; pitcherRatings(I, 6);
            Print Tab(35); Using "###"; pitcherRatings(I, 7);
            Print Tab(39); Using "###"; pitcherRatings(I, 8);
            Print Tab(43); Using "###"; pitcherRatings(I, 9);
            Print Tab(46); Using "##.##"; pitcherRatings(I, 10) / 100;
            Print Tab(53); Using "##"; pitcherRatings(I, 12);
            Print Tab(56); Using "##"; pitcherRatings(I, 13);
            Print Tab(59); Using "##"; pitcherRatings(I, 16);
            Print Tab(62); Using "##"; pitcherRatings(I, 33);
            Print Tab(66); Using "##"; pitcherRatings(I, 18)

        End If

    Next

    Print
    Color 14
    Print "TOTALS";
    Locate , 14
    Print Using "####  ####      ####    ####     #.##    ###   ###"; inputTP%(1); inputTP%(3); inputTP%(6); inputTP%(8); (ERX! * 9) / inputTP%(6); inputTP%(13); inputTP%(33)
    Locate , 17
    Print Using "####      ###    ####    ####            ###"; inputTP%(2); inputTP%(5); inputTP%(7); inputTP%(9); inputTP%(16)
    Print
    Print "TOTAL APPEARANCES: "; numberAppearances

    Color 11
    Locate 24: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'     ViewHittingFielding subroutine
'----------------------------------------
'This subroutine give the user a summary
'of fielding and hitting stats for each
'team pitcher.
Sub ViewHittingFielding ()

    Shared pitcherRatings()
    Shared pitcherNames$()

    Cls

    Color 14
    Print "PITCHER        G  AB   R   H DB TR HR RBI  BB  SO  SB CS BAVG  R B FAVG"

    For I = 0 To 21

        Color 3
        Print Left$(pitcherNames$(I), 10);

        Color 7
        Print Tab(14); Using "###"; pitcherRatings(I, 19);
        Print Tab(18); Using "###"; pitcherRatings(I, 20);
        Print Tab(22); Using "###"; pitcherRatings(I, 21);
        Print Tab(26); Using "###"; pitcherRatings(I, 22);
        Print Tab(30); Using "##"; pitcherRatings(I, 23);
        Print Tab(33); Using "##"; pitcherRatings(I, 24);
        Print Tab(36); Using "##"; pitcherRatings(I, 25);
        Print Tab(39); Using "###"; pitcherRatings(I, 26);
        Print Tab(43); Using "###"; pitcherRatings(I, 27);
        Print Tab(47); Using "###"; pitcherRatings(I, 28);
        Print Tab(52); Using "##"; pitcherRatings(I, 29);
        Print Tab(55); Using "##"; pitcherRatings(I, 30);

        If pitcherRatings(I, 11) <> 999 Then
            statPCT! = pitcherRatings(I, 11) / 1000
            Print Tab(59); Using "#.###"; statPCT!;
        Else
            statI2! = 0

            '-- This is initialized as 1000
            '-- So when not given a value it calculates too large
            If pitcherRatings(I, 20) > 0 Then statI2! = Int((pitcherRatings(I, 22) / pitcherRatings(I, 20) + .0005) * 1000)
            If pitcherRatings(I, 20) = 0 Or statI2! = 0 Then Print Tab(59); ".000";

            If statI2! > 0 And statI2! < 100 Then Print Tab(59); ".0";: Print Tab(61); Using "##"; statI2!;
            If statI2! > 100 And statI2! < 1000 Then Print Tab(59); ".";: Print Tab(60); Using "###"; statI2!;
            'If statI2! >= 1000 Then Print Tab(59); Using "#.###"; statI2!
            If statI2! >= 1000 Then Print Tab(59); " ---";

        End If

        Print Tab(64); Using "#"; pitcherRatings(I, 31);
        Print Tab(66); Using "#"; pitcherRatings(I, 32);
        Print Tab(68); Using "####"; pitcherRatings(I, 17)

    Next

    Color 11
    Locate 24: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       FieldingStats Subroutine
'----------------------------------------
'This subroutine give the user a summary
'of field stats for each batter.
Sub FieldingStats (teamName$)

    Shared batterNames$()
    Shared batterRatings()

    Cls

    Color 14
    Print "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

    For I = 0 To 22

        If I = 20 Then

            Color 11
            Locate 23: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

            Cls

            Color 12: Print teamName$
            Color 14
            Print "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

        End If

        Color 3
        Print Left$(batterNames$(I), 10);

        Color 7
        Print Tab(13); Using "####"; batterRatings(I, 26);
        Print Tab(19); Using "#"; batterRatings(I, 33);
        Print Tab(23); Using "#"; batterRatings(I, 34);
        Print Tab(26); Using "####"; batterRatings(I, 27);
        Print Tab(32); Using "#"; batterRatings(I, 35);
        Print Tab(36); Using "#"; batterRatings(I, 36);
        Print Tab(39); Using "####"; batterRatings(I, 28);
        Print Tab(45); Using "#"; batterRatings(I, 37);
        Print Tab(49); Using "#"; batterRatings(I, 38);
        Print Tab(52); Using "####"; batterRatings(I, 29);
        Print Tab(58); Using "#"; batterRatings(I, 39);
        Print Tab(62); Using "#"; batterRatings(I, 40);
        Print Tab(66); Using "#"; batterRatings(I, 16);
        Print Tab(71); Using "#"; batterRatings(I, 30)

    Next I

End Sub
