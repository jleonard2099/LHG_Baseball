Sub ACTIVATE ()

    _Title "Full Count Baseball - Activate Players"

    Shared parkHR_DRAFT&()
    Shared batterRatings(), teamRatings%(), pitcherRatings()
    Shared batterNames$(), pitcherNames$()

    Shared Manager$, teamAbbrev$, Stadium$

    Call ChooseTeam (YN$, NTMS, teamNbr, team$)

    If teamNbr <> 0 Then

        Color 15, 0
        Cls

        Print "CHANGE BATTERS"
        X% = 2

        Do

            For I = 0 To 10

                Color 15, 0
                Locate I + 5, 5: Print Using "## "; I + 1;

                If batterRatings(I, 31) <> 0 Then Color teamRatings%(11), teamRatings%(12)
                Print batterNames$(I);

                Color 15, 0
                Print Tab(40);: Print Using "## "; I + 13;

                If batterRatings(I + 12, 31) <> 0 Then Color teamRatings%(11), teamRatings%(12)
                Print batterNames$(I + 12): Color 15, 0

            Next

            Locate , 5: Print Using "## "; 12;

            If batterRatings(11, 31) <> 0 Then Color teamRatings%(11), teamRatings%(12)
            Print batterNames$(11)

            Color 15, 0
            Print
            Print "# TO CHANGE (1-23) (0 TO CONTINUE) ";
            Input I

            I = I - 1

            If I < 0 Or I > 22 Then
                X% = 0
            Else
                batterRatings(I, 31) = 1 - batterRatings(I, 31)
            End If

        Loop Until I < 0

        Cls
        Print "CHANGE PITCHERS"
        X% = 2

        Do

            For I = 0 To 10

                Locate I + 5, 5: Print Using "## "; I + 1;

                If pitcherRatings(I, 34) <> 0 Then Color teamRatings%(11), teamRatings%(12)
                Print pitcherNames$(I);: Color 15, 0

                Print Tab(40);: Print Using "## "; I + 12;

                If pitcherRatings(I + 11, 34) <> 0 Then Color teamRatings%(11), teamRatings%(12)
                Print pitcherNames$(I + 11): Color 15, 0

            Next

            Print
            Print
            Print "# TO CHANGE (1-22) (0 TO CONTINUE) ";
            Input I

            I = I - 1

            If I < 0 Or I > 21 Then
                X% = 0
            Else
                pitcherRatings(I, 34) = 1 - pitcherRatings(I, 34)
            End If

        Loop Until I < 0

        Cls

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

        LSet Q$(0) = team$

        For I = 1 To 13
            LSet Q$(I) = MKI$(teamRatings%(I))
        Next

        LSet Q$(14) = MKS$(parkHR_DRAFT&(0))

        For I = 0 To 22: 

            LSet Q$(I * 43 + 15) = batterNames$(I)

            For I1 = 0 To 41
                LSet Q$(I * 43 + I1 + 16) = MKI$(batterRatings(I, I1))
            Next

        Next

        For I = 0 To 21

            LSet Q$(I * 38 + 1004) = pitcherNames$(I)

            For I1 = 0 To 36
                LSet Q$(I * 38 + I1 + 1005) = MKI$(pitcherRatings(I, I1))
            Next

        Next

        LSet Q$(1840) = Stadium$
        LSet Q$(1841) = Manager$
        LSet Q$(1842) = teamAbbrev$
        LSet Q$(1843) = MKI$(999)
        
        Put #1, teamNbr
        
        Close 1

    End If

End Sub


'----------------------------------------
'     GetTeams_Activate Subroutine
'----------------------------------------
'...explanation...
Sub GetTeams_Activate (yearNumber$, numberTeams)
       
    numberTeams = 0

    teamYear$ = RTrim$(yearNumber$)

    If Not _FileExists(diskPaths$(0) + "FCTEAMS." + teamYear$) Then
        BO% = 1
        Locate 10, 8
        Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else

        Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

        fileLength& = LOF(1)

        Field #1, 15 As Q$(0)

        For I1 = 1 To fileLength& / 4342

            Get #1, I1

            For I2 = 1 To 15
                If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
            Next I2

            If nullTeam <> 1 Then
                numberTeams = numberTeams + 1
                teamNames$(numberTeams) = Q$(0)
                teamIndex%(numberTeams) = I1
            End If

        Next I1
        
        Close 1

    End If

End Sub

'----------------------------------------
'     SelectTeam_ACTIVATE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_ACTIVATE (yearNumber$, numberTeams, teamIdx)

    Shared parkHR_DRAFT&()
    Shared batterRatings(), teamRatings%(), pitcherRatings()
    Shared batterNames$(), pitcherNames$()

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
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIdx = teamIndex%(Selection%)
        Locate 2, 33

        Color 31: Print "LOADING..."

        teamYear$ = RTrim$(yearNumber$)

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

        For I = 1 To 13
            teamRatings%(I) = CVI(Q$(I))
        Next

        parkHR_DRAFT&(0) = CVS(Q$(14))

        For I = 0 To 22:
            batterNames$(I) = RTrim$(Q$(I * 43 + 15))

            For I1 = 0 To 41
                batterRatings(I, I1) = CVI(Q$(I * 43 + I1 + 16))
            Next

        Next

        For I = 0 To 21

            pitcherNames$(I) = RTrim$(Q$(I * 38 + 1004))

            For I1 = 0 To 36
                pitcherRatings(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
            Next

        Next

        Close 1

    Else
        BO% = 1
    End If

End Sub
