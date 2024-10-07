'----------------------------------------
'            GROUND Subroutine
'----------------------------------------
'This subroutine provides a quick interface
'for the user to modify ground ball and 
'flyout calculations from the relevant params.
Sub GROUND ()

    _Title "Full Count Baseball - Groundball Calculations"

    Shared batterRatings(), teamRatings(), pitcherRatings()
    Shared batterNames$(), pitcherNames$()

    Shared parkHR_DRAFT&()

    Shared Manager$, teamAbbrev$, Stadium$

    Dim BA%(29), DP%(29), RH%(21)
    Dim SP%(29), totBatRat(29)

    Dim BAP!, CH!, PD!
    Dim RHP!, SLP!

    FG = 0
    BG = 7

    Color 15, 1
    Cls

    Locate , 16: Print "SPECIFY LEAGUE STATS"
    Print
    Locate 7, 1: QPrint "League Batting Average ", 14, 1, -1
    Locate , 24: QPrint FUsing$(Str$(BP), "######", 1), FG, BG, -1

    Locate 9, 1: QPrint "League Slugging Percentage ", 14, 1, -1
    Locate , 28: QPrint FUsing$(Str$(SA), "######", 1), FG, BG, -1

    Locate 11, 1: QPrint "Total Hits in League ", 14, 1, -1
    Locate , 22: QPrint FUsing$(Str$(PH), "######", 1), FG, BG, -1

    Locate 13, 1: QPrint "Total Earned Runs in League ", 14, 1, -1
    Locate , 29: QPrint FUsing$(Str$(ER), "######", 1), FG, BG, -1

    GetDiv1:
    Locate 7, 24 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 6 'specify field parameters
    Call TextIn(BP$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    BP = Val(BP$)
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 9, 28 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 6 'specify field parameters
    Call TextIn(SA$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    SA = Val(SA$)
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 11, 22 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 6 'specify field parameters
    Call TextIn(PH$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    PH = Val(PH$)
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 13, 29 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 6 'specify field parameters
    Call TextIn(ER$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    ER = Val(ER$)
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv4
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

    MD = SA - BP

    PD! = PH / (ER) * 100

    Do

        Cls

        teamNbr = 0
        Call ChooseTeam (YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            For I = 0 To 22

                totBatRat(I) = batterRatings(I, 6) + batterRatings(I, 7) + 2 * batterRatings(I, 8) + 3 * batterRatings(I, 9)

                If batterRatings(I, 4) = 0 Then
                    SP%(I) = 0
                    BA%(I) = 0
                Else
                    SLP! = totBatRat(I) / batterRatings(I, 4)
                    SP%(I) = SLP! * 1000
                    batterRatings(I, 20) = BP
                    BAP! = batterRatings(I, 6) / batterRatings(I, 4)
                    BA%(I) = BAP! * 1000
                End If

                DP%(I) = SP%(I) - BA%(I)

                DI = DP%(I) - MD
                CH! = Int(DI / 10)
                batterRatings(I, 18) = 52 - CH!

                If batterRatings(I, 18) < 34 Then batterRatings(I, 18) = 34
                If batterRatings(I, 18) > 64 Then batterRatings(I, 18) = 64

            Next I

            For I = 0 To 21

                If pitcherRatings(I, 6) = 0 Or pitcherRatings(I, 10) = 0 Then
                    pitcherRatings(I, 12) = 52
                Else

                    pitcherRatings(I, 14) = BP

                    RHP! = pitcherRatings(I, 7) / (pitcherRatings(I, 10) / 900 * pitcherRatings(I, 6))
                    RH%(I) = RHP! * 100
                    Print RH%(I)

                    DF = RH%(I) - PD!
                    CH! = Int(DF / 10) * 2
                    pitcherRatings(I, 12) = Int(CH! + 52)
                    RH%(I) = RH%(I) / 10

                    If pitcherRatings(I, 12) > 64 Then pitcherRatings(I, 12) = 64
                    If pitcherRatings(I, 12) < 34 Then pitcherRatings(I, 12) = 34

                End If

            Next I

            Color 15, 0
            Cls

            Print "BATTER         BA            SA            GA"

            For I = 0 To 22
                If batterNames$(I) <> "" And batterNames$(I) <> "XXX" Then
                    Print batterNames$(I), BA%(I), SP%(I), batterRatings(I, 18)
                End If
            Next

            Print "HIT ANY KEY TO CONTINUE";
            U$ = GetKeyPress$

            Cls
            Print "PITCHER               HITS/RUN ALLOWED     GA"

            For I = 0 To 21
                If pitcherNames$(I) <> "" And pitcherNames$(I) <> "XXX" Then
                    Print pitcherNames$(I), , Int(RH%(I)) / 10, pitcherRatings(I, 12)
                End If
            Next

            Print "HIT ANY KEY TO CONTINUE";
            prompt$ = GetKeyPress$

            Cls

            Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + YN$ For Random As #1 Len = TEAM_SIZE_BYTES

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

            For I = 1 To 13:
                LSet Q$(I) = MKI$(teamRatings(I))
            Next

            LSet Q$(14) = MKS$(parkHR_DRAFT&(0))

            For I = 0 To 22
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

            Do
                Cls
                Print "EDIT ANOTHER TEAM? (Y/N) ";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" or teamNbr = 0

End Sub
