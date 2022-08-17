Sub GROUND ()

    _Title "Full Count Baseball - Groundball Calculations"

    DefInt A-Z

    Shared B%(), L%(), P%()
    Shared B$(), P$()
    Shared K9&()

    Dim BA%(29), DP%(29), RH%(21)
    Dim SP%(29), TB%(29)

    Dim BAP As Single, CH As Single, PD As Single
    Dim RHP As Single, SLP As Single

    Color 15, 1
    FG = 0
    BG = 7

    Cls

    Locate , 16: Print "SPECIFY LEAGUE STATS": Print
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

    PD = PH / (ER) * 100

    Do
        Cls
        Call GetTeam_GROUND (YN$, numberTeams)

        S$ = RTrim$(Q$(1840))
        M$ = RTrim$(Q$(1841))
        NN$ = RTrim$(Q$(1842))
        CK = CVI(Q$(1843))

        For I = 0 To 22
            TB%(I) = B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)

            If B%(I, 4) = 0 Then 
                SP%(I) = 0
                BA%(I) = 0
            Else
                SLP = TB%(I) / B%(I, 4)
                SP%(I) = SLP * 1000
                B%(I, 20) = BP
                BAP = B%(I, 6) / B%(I, 4)
                BA%(I) = BAP * 1000
            End If
            
            DP%(I) = SP%(I) - BA%(I)

            DI = DP%(I) - MD
            CH = Int(DI / 10)
            B%(I, 18) = 52 - CH

            If B%(I, 18) < 34 Then B%(I, 18) = 34
            If B%(I, 18) > 64 Then B%(I, 18) = 64

        Next I


        For I = 0 To 21

            If P%(I, 6) = 0 Or P%(I, 10) = 0 Then 
                P%(I, 12) = 52
            Else

                P%(I, 14) = BP

                RHP = P%(I, 7) / (P%(I, 10) / 900 * P%(I, 6))
                RH%(I) = RHP * 100
                Print RH%(I)
                'INT(P%(I,7)*900/(P%(I,10)*P%(I,6))*100)/100

                DF = RH%(I) - PD
                CH = Int(DF / 10) * 2
                P%(I, 12) = Int(CH + 52)
                RH%(I) = RH%(I) / 10

                If P%(I, 12) > 64 Then P%(I, 12) = 64
                If P%(I, 12) < 34 Then P%(I, 12) = 34
                
            End If
            
        Next I

        Cls: Print "BATTER         BA            SA            GA"
        
        For I = 0 To 22
            If B$(I) <> "" And B$(I) <> "XXX" Then
                Print B$(I), BA%(I), SP%(I), B%(I, 18)
            End If
        Next

        Print "HIT ANY KEY TO CONTINUE";

        U$ = GetKeyPress$

        Cls: Print "PITCHER               HITS/RUN ALLOWED     GA"

        For I = 0 To 21
            If P$(I) <> "" And P$(I) <> "XXX" Then  
                Print P$(I), , Int(RH%(I)) / 10, P%(I, 12)
            End If
        Next

        Print "HIT ANY KEY TO CONTINUE";
        U$ = GetKeyPress$
        Cls

        '    L%(1)=267
        '    L%(2)=228
        '    L%(3)=94
        '    L%(4)=100
        '    L%(6)=99
        '    L%(7)=99
        '    K9&(0)=99

        Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

        For I = 0 To 22
            Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
            Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
            Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
            Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
        Next

        For I = 0 To 21
            FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015), 2020 - 37 - I * 89 AS X$
            FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2 AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027), 2020 - 61 - I * 89 AS X$
            FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2 AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039), 2020 - 85 - I * 89 AS X$
            Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
        Next

        Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
        
        LSet Q$(0) = T$
        
        For I = 1 To 13: 
            LSet Q$(I) = MKI$(L%(I))
        Next
        
        LSet Q$(14) = MKS$(K9&(0))

        For I = 0 To 22
            LSet Q$(I * 43 + 15) = B$(I)
            For I1 = 0 To 41
                LSet Q$(I * 43 + I1 + 16) = MKI$(B%(I, I1))
            Next
        Next

        For I = 0 To 21
            LSet Q$(I * 38 + 1004) = P$(I)
            For I1 = 0 To 36
                LSet Q$(I * 38 + I1 + 1005) = MKI$(P%(I, I1))
            Next
        Next

        LSet Q$(1840) = S$
        LSet Q$(1841) = M$
        LSet Q$(1842) = NN$
        LSet Q$(1843) = MKI$(999)
        Put #1, T: Close 1

        Cls
        Print "EDIT ANOTHER TEAM (YN) ";
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N" 'from 300


End Sub


'----------------------------------------
'       GetTeam_GROUND subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_GROUND (yearNumber$, numberTeams)

    Shared L%()

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
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$
        
        If NY$ = "" Then 
            Call SelectTeam_GROUND (NY$, numberTeams)
        Else
            YN$ = NY$
            Erase teamNames$

            numberTeams = 0
            If _FileExists(diskPaths$(0) + "FCTEAMS." + YN$) Then 

                Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
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
                1103 Next I1
                Close 1
                
                Call SelectTeam_GROUND (YN$, numberTeams)
            Else
                Cls
                Print "ERROR:: UNABLE TO READ TEAM FILE": Print
                BO% = 1
            End If 'Done checking file exists
        
        End If 'Done checking input
    
        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color 15, 0: Color L%(11), L%(12): Print T$
            Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If
        
    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'   SelectTeam_GROUND subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_GROUND (yearNumber$, numberTeams)

    Shared B%(), L%(), P%()
    Shared B$(), P$()
    Shared K9&()

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
        T = teamIndex%(Selection%)
        Locate 2, 33
        Color 31: Print "LOADING..."
        
        Open diskPaths$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

        For I = 0 To 22
            Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
            Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
            Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
            Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
        Next

        For I = 0 To 21
            FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015), 2020 - 37 - I * 89 AS X$
            FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2 AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027), 2020 - 61 - I * 89 AS X$
            FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2 AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039), 2020 - 85 - I * 89 AS X$
            Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
        Next

        Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
        Get #1, T

        T$ = RTrim$(Q$(0))

        For I = 1 To 13: L%(I) = CVI(Q$(I)): Next: K9&(0) = CVS(Q$(14))

        For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
            For I1 = 0 To 41
                B%(I, I1) = CVI(Q$(I * 43 + I1 + 16)): 'PRINT B$(I),B%(I,I1)
            Next
        Next

        For I = 0 To 21
            P$(I) = RTrim$(Q$(I * 38 + 1004))
            For I1 = 0 To 36
                P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
            Next
        Next

        Close 1

    Else
        BO% = 1
    End If
    
End Sub
