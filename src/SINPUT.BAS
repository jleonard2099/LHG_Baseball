Sub SINPUT ()

    _Title "Full Count Baseball - Stats Input"

    DefInt A-Z

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT/CHANGE EXISTING REPLAY STAT FILE"
        menuChoices$(2) = "ENTER SCHEDULE RESULTS               "
        menuChoices$(3) = "RETURN TO MAIN MENU                  "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 23

        Call box0(6, 22, 10, 60, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call EditStatsSchedules(1)
                Case 2: Call EditStatsSchedules(2)
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

    '73 Exit Sub
End Sub

'----------------------------------------
'     EditStatsSchedules Subroutine
'----------------------------------------
'...explanation...
Sub EditStatsSchedules (action%)

    Shared statB$(), statP$()
    Shared statB%(), careerB0%(), ratings%()
    Shared statP%(), careerP0%()
    Shared statT%(), statT1%()

    Cls
    Print "CHANGE TEAM BATTING STATS"
    Print
    X% = 2

    Call GetTeam_SINPUT(yearNumber$, numberTeams)

    T$ = RTrim$(Q$(0))

    statB$ = T$
    statB$ = RTrim$(statB$)

    For XX = 4 To 11
        If Mid$(statB$, XX, 1) = " " Or Mid$(statB$, XX, 1) = "." Then Mid$(statB$, XX, 1) = "_"
    Next XX

    For XX = 12 To 14
        If Mid$(statB$, XX, 2) = "ST" Then Mid$(statB$, 9, 1) = "_": Mid$(statB$, 10, 2) = "ST"
    Next XX

    QQ = 8
    For XX = 11 To 4 Step -1
        If Mid$(statB$, XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    'Z$ = LEFT$(Z$, 11)
    'Z$ = LEFT$(RIGHT$(statB$, LEN(statB$) - 3), 8) + "." + LEFT$(statB$, 2)
    Z$ = Mid$(statB$, 4, QQ) + "." + Left$(statB$, 2)

    If action% = 2 Then
        Call PromptScheduleChanges(Z$)
    Else
        'action% = 1

        If Not _FileExists(diskPaths$(1) + Z$) Then
            Locate 10, 8
            Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            Open diskPaths$(1) + Z$ For Input As #1

            Input #1, A$
            Input #1, W2%
            Input #1, L2%

            For I = 0 To 22: Input #1, statB$(I): Input #1, statB%(I): Next I
            For I = 0 To 21: Input #1, statP$(I): Input #1, statP%(I): Next I
            For I = 0 To 22: For I1 = 0 To 21: Input #1, careerB0%(I, I1): Next I1: Next I
            For I = 0 To 21: For I1 = 0 To 41: Input #1, careerP0%(I, I1): Next I1: Next I

            Input #1, D0%
            Input #1, D1%

            For I = 0 To 21: Input #1, statT%(I): Next I
            For I = 0 To 22: Input #1, statT1%(I): Next I

            Close #1

            Color ratings%(11), ratings%(12)
            Cls

            Call InputTeamStats(Z$, A$, X%, D0%, D1%, L2%, W2%, 2)

        End If

    End If

End Sub

'----------------------------------------
'       InputTeamStats Subroutine
'----------------------------------------
'...explanation...
Sub InputTeamStats (Z$, A$, X%, D0%, D1%, L2%, W2%, action%)

    Shared statB$(), statP$()
    Shared careerB0%(), careerP0%()
    Shared statB%(), statP%(), statT%(), statT1%()

    C1 = 0

    Cls

    If action% = 2 Then
        Print "TEAM BATTING STATS"
        Locate 2: Print "1. TEAM NAME (EX. ST_LOUIS.64) "; Z$
        Locate 3: Print "2. TM AB "; statT%(0)
        Locate 4: Print "3. TM RUNS "; statT%(2)
        Locate 5: Print "4. TM HITS "; statT%(1)
        Locate 6: Print "5. TM DBL "; statT%(4)
        Locate 7: Print "6. TM TRPL "; statT%(5)
        Locate 8: Print "7. TM HR "; statT%(6)
        Locate 9: Print "8. TM RBI "; statT%(3)
        Locate 10: Print "9. TM BB "; statT%(7)
        Locate 11: Print "10. TM SO "; statT%(8)
        Locate 12: Print "11. TM ERR "; statT%(11)
        Locate 13: Print "12. TM SB "; statT%(9)
        Locate 14: Print "13. TM CS "; statT%(10)
        Locate 15: Print "14. TM PO "; statT%(16)
        Locate 16: Print "15. TM A  "; statT%(17)
        Locate 17: Print "16. TM SH "; statT%(18)
        Locate 18: Print "17. TM SF "; statT%(19)
        Locate 19: Print "18. TM GIDP "; statT%(15)
        Locate 20: Print "19. TM LOB "; statT%(12)

    Else

        Print "TEAM BATTING STATS"
        80 Locate 2: Input "1. TEAM NAME (EX. ST_LOUIS.64) "; Z$: If C1 = 1 Then 145
        85 Locate 3: Input "2. TM AB "; statT%(0): If C1 = 1 Then 145
        90 Locate 4: Input "3. TM RUNS "; statT%(2): If C1 = 1 Then 145
        95 Locate 5: Input "4. TM HITS "; statT%(1): If C1 = 1 Then 145
        100 Locate 6: Input "5. TM DBL "; statT%(4): If C1 = 1 Then 145
        105 Locate 7: Input "6. TM TRPL "; statT%(5): If C1 = 1 Then 145
        110 Locate 8: Input "7. TM HR "; statT%(6): If C1 = 1 Then 145
        115 Locate 9: Input "8. TM RBI "; statT%(3): If C1 = 1 Then 145
        120 Locate 10: Input "9. TM BB "; statT%(7): If C1 = 1 Then 145
        125 Locate 11: Input "10. TM SO "; statT%(8): If C1 = 1 Then 145
        130 Locate 12: Input "11. TM ERR "; statT%(11): If C1 = 1 Then 145
        135 Locate 13: Input "12. TM SB "; statT%(9): If C1 = 1 Then 145
        140 Locate 14: Input "13. TM CS "; statT%(10): If C1 = 1 Then 145
        141 Locate 15: Input "14. TM PO "; statT%(16): If C1 = 1 Then 145
        142 Locate 16: Input "15. TM A  "; statT%(17): If C1 = 1 Then 145
        143 Locate 17: Input "16. TM SH "; statT%(18): If C1 = 1 Then 145
        144 Locate 18: Input "17. TM SF "; statT%(19): If C1 = 1 Then 145
        116 Locate 19: Input "18. TM GIDP "; statT%(15): If C1 = 1 Then 145
        117 Locate 20: Input "19. TM LOB "; statT%(12): If C1 = 1 Then 145

    End If

    145 '
    Do
        C1 = 1

        Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

        Select Case B9
            Case 1: GoTo 80
            Case 2: GoTo 85
            Case 3: GoTo 90
            Case 4: GoTo 95
            Case 5: GoTo 100
            Case 6: GoTo 105
            Case 7: GoTo 110
            Case 8: GoTo 115
            Case 9: GoTo 120
            Case 10: GoTo 125
            Case 11: GoTo 130
            Case 12: GoTo 135
            Case 13: GoTo 140
            Case 14: GoTo 141
            Case 15: GoTo 142
            Case 16: GoTo 143
            Case 17: GoTo 144
            Case 18: GoTo 116
            Case 19: GoTo 117
        End Select

        C1 = 0

        If X% > 0 Then Call ChangePitchingStats(L2%, W2%)

    Loop Until B9 = 0

    If action% = 2 Then
        Cls
        Print "TEAM PITCHING STATS"
        Locate 2: Print "1. WON "; W2%
        Locate 3: Print "2. LOST "; L2%
        Locate 4: Print "3. TM SV "; statT1%(12)
        Locate 5: Print "4. (TM IP X 3) "; statT1%(0)
        Locate 6: Print "5. TM HITS "; statT1%(1)
        Locate 7: Print "6. TM BB "; statT1%(4)
        Locate 8: Print "7. TM SO "; statT1%(5)
        Locate 9: Print "8. TM CG "; statT1%(10)
        Locate 10: Print "9. TM RUNS "; statT1%(2)
        Locate 11: Print "10. TM E R "; statT1%(3)
        Locate 12: Print "11. TM SH OUT "; statT1%(11)
        Locate 13: Print "12. TM STARTS "; statT1%(9)
        Locate 14: Print "13. TM HRA "; statT1%(14)
        Locate 15: Print "14. TM DP TURNED "; statT1%(13)
        Locate 16: Print "15. TM SH "; statT1%(15)
        Locate 17: Print "16. TM SF "; statT1%(16)
        Locate 18: Print "17. TM RUNNERS INHERITED (RI) "; statT1%(17)
        Locate 19: Print "18. TM RI SCORED "; statT1%(18)
        Locate 20: Print "19. TM BLOWN SAVES "; statT1%(19)
        Locate 21: Print "20. TM QUALITY STARTS "; statT1%(21)
    Else
        155 '
        Cls
        Print "TEAM PITCHING STATS"
        160 Locate 2: Input "1. WON "; W2%: If C1 = 1 Then 220
        165 Locate 3: Input "2. LOST "; L2%: If C1 = 1 Then 220
        170 Locate 4: Input "3. TM SV "; statT1%(12): If C1 = 1 Then 220
        175 Locate 5: Input "4. (TM IP X 3) "; statT1%(0): If C1 = 1 Then 220
        180 Locate 6: Input "5. TM HITS "; statT1%(1): If C1 = 1 Then 220
        185 Locate 7: Input "6. TM BB "; statT1%(4): If C1 = 1 Then 220
        190 Locate 8: Input "7. TM SO "; statT1%(5): If C1 = 1 Then 220
        195 Locate 9: Input "8. TM CG "; statT1%(10): If C1 = 1 Then 220
        200 Locate 10: Input "9. TM RUNS "; statT1%(2): If C1 = 1 Then 220
        205 Locate 11: Input "10. TM E R "; statT1%(3): If C1 = 1 Then 220
        210 Locate 12: Input "11. TM SH OUT "; statT1%(11): If C1 = 1 Then 220
        211 Locate 13: Input "12. TM STARTS "; statT1%(9): If C1 = 1 Then 220
        212 Locate 14: Input "13. TM HRA "; statT1%(14): If C1 = 1 Then 220
        213 Locate 15: Input "14. TM DP TURNED "; statT1%(13): If C1 = 1 Then 220
        214 Locate 16: Input "15. TM SH "; statT1%(15): If C1 = 1 Then 220
        215 Locate 17: Input "16. TM SF "; statT1%(16): If C1 = 1 Then 220
        216 Locate 18: Input "17. TM RUNNERS INHERITED (RI) "; statT1%(17): If C1 = 1 Then 220
        217 Locate 19: Input "18. TM RI SCORED "; statT1%(18): If C1 = 1 Then 220
        218 Locate 20: Input "19. TM BLOWN SAVES "; statT1%(19): If C1 = 1 Then 220
        219 Locate 21: Input "20. TM QUALITY STARTS "; statT1%(21): If C1 = 1 Then 220
    End If

    220 '
    Do
        C1 = 1

        Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

        Select Case B9
            Case 1: GoTo 160
            Case 2: GoTo 165
            Case 3: GoTo 170
            Case 4: GoTo 175
            Case 5: GoTo 180
            Case 6: GoTo 185
            Case 7: GoTo 190
            Case 8: GoTo 195
            Case 9: GoTo 200
            Case 10: GoTo 205
            Case 11: GoTo 210
            Case 12: GoTo 211
            Case 13: GoTo 212
            Case 14: GoTo 213
            Case 15: GoTo 214
            Case 16: GoTo 215
            Case 17: GoTo 216
            Case 18: GoTo 217
            Case 19: GoTo 218
            Case 20: GoTo 219
        End Select

        C1 = 0

        If X% > 0 Then Call SelectBatters_SINPUT(Z$) 'GoTo 309

    Loop Until B9 = 0

    230 '
    For I = 0 To 22
        Cls

        If action% = 2 Then
            Locate 1, 30: Print "BATTER - "; I + 1: Locate , 1
            Locate 1: Print "1. NAME "; statB$(I)
            Locate 2: Print "2. AB "; careerB0%(I, 0)
            Locate 3: Print "3. RUNS "; careerB0%(I, 2)
            Locate 4: Print "4. HITS "; careerB0%(I, 1)
            Locate 5: Print "5. DBL "; careerB0%(I, 4)
            Locate 6: Print "6. TRPL "; careerB0%(I, 5)
            Locate 7: Print "7. HR "; careerB0%(I, 6)
            Locate 8: Print "8. RBI "; careerB0%(I, 3)
            Locate 9: Print "9. BB "; careerB0%(I, 7)
            Locate 10: Print "10. SO "; careerB0%(I, 8)
            Locate 11: Print "11. ERR "; careerB0%(I, 11)
            Locate 12: Print "12. SB "; careerB0%(I, 9)
            Locate 13: Print "13. CS "; careerB0%(I, 10)
            Locate 14: Print "14.  G "; careerB0%(I, 14)
            Locate 15: Print "15. CHS "; careerB0%(I, 12)
            Locate 16: Print "16. LHS "; careerB0%(I, 13)
            Locate 17: Print "17. GIDP "; careerB0%(I, 15)
            Locate 18: Print "18. PO "; careerB0%(I, 16)
            Locate 19: Print "19. A  "; careerB0%(I, 17)
            Locate 20: Print "20. SH "; careerB0%(I, 18)
            Locate 21: Print "21. SF "; careerB0%(I, 19)
        Else
            235 Locate 1, 30: Print "BATTER - "; I + 1: Locate , 1
            240 Locate 1: Input "1. NAME "; statB$(I): If C1 = 1 Then 309
            245 Locate 2: Input "2. AB "; careerB0%(I, 0): If C1 = 1 Then 309
            250 Locate 3: Input "3. RUNS "; careerB0%(I, 2): If C1 = 1 Then 309
            255 Locate 4: Input "4. HITS "; careerB0%(I, 1): If C1 = 1 Then 309
            260 Locate 5: Input "5. DBL "; careerB0%(I, 4): If C1 = 1 Then 309
            265 Locate 6: Input "6. TRPL "; careerB0%(I, 5): If C1 = 1 Then 309
            270 Locate 7: Input "7. HR "; careerB0%(I, 6): If C1 = 1 Then 309
            275 Locate 8: Input "8. RBI "; careerB0%(I, 3): If C1 = 1 Then 309
            280 Locate 9: Input "9. BB "; careerB0%(I, 7): If C1 = 1 Then 309
            285 Locate 10: Input "10. SO "; careerB0%(I, 8): If C1 = 1 Then 309
            290 Locate 11: Input "11. ERR "; careerB0%(I, 11): If C1 = 1 Then 309
            295 Locate 12: Input "12. SB "; careerB0%(I, 9): If C1 = 1 Then 309
            300 Locate 13: Input "13. CS "; careerB0%(I, 10): If C1 = 1 Then 309
            301 Locate 14: Input "14.  G "; careerB0%(I, 14): If C1 = 1 Then 309
            302 Locate 15: Input "15. CHS "; careerB0%(I, 12): If C1 = 1 Then 309
            303 Locate 16: Input "16. LHS "; careerB0%(I, 13): If C1 = 1 Then 309
            304 Locate 17: Input "17. GIDP "; careerB0%(I, 15): If C1 = 1 Then 309
            305 Locate 18: Input "18. PO "; careerB0%(I, 16): If C1 = 1 Then 309
            306 Locate 19: Input "19. A  "; careerB0%(I, 17): If C1 = 1 Then 309
            307 Locate 20: Input "20. SH "; careerB0%(I, 18): If C1 = 1 Then 309
            308 Locate 21: Input "21. SF "; careerB0%(I, 19): If C1 = 1 Then 309
        End If

        309 '
        Do
            C1 = 1

            Locate 23: Input "# TO CHANGE (0 TO CONTINUE) "; B9

            Select Case B9
                Case 1: GoTo 240
                Case 2: GoTo 245
                Case 3: GoTo 250
                Case 4: GoTo 255
                Case 5: GoTo 260
                Case 6: GoTo 265
                Case 7: GoTo 270
                Case 8: GoTo 275
                Case 9: GoTo 280
                Case 10: GoTo 285
                Case 11: GoTo 290
                Case 12: GoTo 295
                Case 13: GoTo 300
                Case 14: GoTo 301
                Case 15: GoTo 302
                Case 16: GoTo 303
                Case 17: GoTo 304
                Case 18: GoTo 305
                Case 19: GoTo 306
                Case 20: GoTo 307
                Case 21: GoTo 308
            End Select

            C1 = 0

            If X% > 0 Then Call SelectBatters_SINPUT(Z$) 'GoTo 309

        Loop Until B9 = 0

    Next I

    '-----
    For I = 0 To 21: Cls

        If action% = 2 Then
            Locate 1, 30: Print "PITCHER - "; I + 1: Locate , 1
            Locate 1: Print "1. NAME "; statP$(I)
            Locate 2: Print "2. WON "; careerP0%(I, 6)
            Locate 3: Print "3. LOST "; careerP0%(I, 7)
            Locate 4: Print "4. SV "; careerP0%(I, 12)
            Locate 5: Print "5. (IP X 3) "; careerP0%(I, 0)
            Locate 6: Print "6. HITS "; careerP0%(I, 1)
            Locate 7: Print "7. BB "; careerP0%(I, 4)
            Locate 8: Print "8. SO "; careerP0%(I, 5)
            Locate 9: Print "9. CG "; careerP0%(I, 10)
            Locate 10: Print "10. RUNS "; careerP0%(I, 2)
            Locate 11: Print "11. E R "; careerP0%(I, 3)
            Locate 12: Print "12. SH OUT "; careerP0%(I, 11)
            Locate 13: Print "13. GAMES "; careerP0%(I, 8)
            Locate 14: Print "14. STARTS "; careerP0%(I, 9)
            Locate 15: Print "15. HR ALL "; careerP0%(I, 14)
            Locate 16: Print "16. GIDP "; careerP0%(I, 13)
            Locate 17: Print "17. SH "; careerP0%(I, 15)
            Locate 18: Print "18. SF "; careerP0%(I, 16)
            Locate 19: Print "19. RUNNERS INHERITED (RI) "; careerP0%(I, 17)
            Locate 20: Print "20. RI SCORED "; careerP0%(I, 18)
            Locate 21: Print "21. BLOWN SAVES "; careerP0%(I, 19)
            Locate 22: Print "22. RUN SUPPORT "; careerP0%(I, 20)
            Locate 23: Print "23. QUALITY STARTS "; careerP0%(I, 21)
            Locate 2, 40: Print "24. AB "; careerP0%(I, 23)
            Locate 3, 40: Print "25. H "; careerP0%(I, 24)
            Locate 4, 40: Print "26. R "; careerP0%(I, 25)
            Locate 5, 40: Print "27. RBI "; careerP0%(I, 26)
            Locate 6, 40: Print "28. DBL "; careerP0%(I, 27)
            Locate 7, 40: Print "29. TRPL "; careerP0%(I, 28)
            Locate 8, 40: Print "30. HR "; careerP0%(I, 29)
            Locate 9, 40: Print "31. BB "; careerP0%(I, 30)
            Locate 10, 40: Print "32. K "; careerP0%(I, 31)
            Locate 11, 40: Print "33. SB "; careerP0%(I, 32)
            Locate 12, 40: Print "34. CS "; careerP0%(I, 33)
            Locate 13, 40: Print "35. ERR "; careerP0%(I, 34)
            Locate 14, 40: Print "36. CHS "; careerP0%(I, 35)
            Locate 15, 40: Print "37. LHS "; careerP0%(I, 36)
            Locate 16, 40: Print "38. GIDP "; careerP0%(I, 37)
            Locate 17, 40: Print "39. PO "; careerP0%(I, 38)
            Locate 18, 40: Print "40. A "; careerP0%(I, 39)
            Locate 19, 40: Print "41. SH "; careerP0%(I, 40)
            Locate 20, 40: Print "42. SF "; careerP0%(I, 41)
        Else
            325 Locate 1, 30: Print "PITCHER - "; I + 1: Locate , 1
            330 Locate 1: Input "1. NAME "; statP$(I): If C1 = 1 Then 401
            335 Locate 2: Input "2. WON "; careerP0%(I, 6): If C1 = 1 Then 401
            340 Locate 3: Input "3. LOST "; careerP0%(I, 7): If C1 = 1 Then 401
            345 Locate 4: Input "4. SV "; careerP0%(I, 12): If C1 = 1 Then 401
            350 Locate 5: Input "5. (IP X 3) "; careerP0%(I, 0): If C1 = 1 Then 401
            355 Locate 6: Input "6. HITS "; careerP0%(I, 1): If C1 = 1 Then 401
            360 Locate 7: Input "7. BB "; careerP0%(I, 4): If C1 = 1 Then 401
            365 Locate 8: Input "8. SO "; careerP0%(I, 5): If C1 = 1 Then 401
            370 Locate 9: Input "9. CG "; careerP0%(I, 10): If C1 = 1 Then 401
            375 Locate 10: Input "10. RUNS "; careerP0%(I, 2): If C1 = 1 Then 401
            380 Locate 11: Input "11. E R "; careerP0%(I, 3): If C1 = 1 Then 401
            385 Locate 12: Input "12. SH OUT "; careerP0%(I, 11): If C1 = 1 Then 401
            390 Locate 13: Input "13. GAMES "; careerP0%(I, 8): If C1 = 1 Then 401
            391 Locate 14: Input "14. STARTS "; careerP0%(I, 9): If C1 = 1 Then 401
            392 Locate 15: Input "15. HR ALL "; careerP0%(I, 14): If C1 = 1 Then 401
            393 Locate 16: Input "16. GIDP "; careerP0%(I, 13): If C1 = 1 Then 401
            394 Locate 17: Input "17. SH "; careerP0%(I, 15): If C1 = 1 Then 401
            395 Locate 18: Input "18. SF "; careerP0%(I, 16): If C1 = 1 Then 401
            396 Locate 19: Input "19. RUNNERS INHERITED (RI) "; careerP0%(I, 17): If C1 = 1 Then 401
            397 Locate 20: Input "20. RI SCORED "; careerP0%(I, 18): If C1 = 1 Then 401
            398 Locate 21: Input "21. BLOWN SAVES "; careerP0%(I, 19): If C1 = 1 Then 401
            399 Locate 22: Input "22. RUN SUPPORT "; careerP0%(I, 20): If C1 = 1 Then 401
            400 Locate 23: Input "23. QUALITY STARTS "; careerP0%(I, 21): If C1 = 1 Then 401
            911 Locate 2, 40: Input "24. AB "; careerP0%(I, 23): If C1 = 1 Then 401
            912 Locate 3, 40: Input "25. H "; careerP0%(I, 24): If C1 = 1 Then 401
            913 Locate 4, 40: Input "26. R "; careerP0%(I, 25): If C1 = 1 Then 401
            914 Locate 5, 40: Input "27. RBI "; careerP0%(I, 26): If C1 = 1 Then 401
            915 Locate 6, 40: Input "28. DBL "; careerP0%(I, 27): If C1 = 1 Then 401
            916 Locate 7, 40: Input "29. TRPL "; careerP0%(I, 28): If C1 = 1 Then 401
            917 Locate 8, 40: Input "30. HR "; careerP0%(I, 29): If C1 = 1 Then 401
            918 Locate 9, 40: Input "31. BB "; careerP0%(I, 30): If C1 = 1 Then 401
            919 Locate 10, 40: Input "32. K "; careerP0%(I, 31): If C1 = 1 Then 401
            920 Locate 11, 40: Input "33. SB "; careerP0%(I, 32): If C1 = 1 Then 401
            921 Locate 12, 40: Input "34. CS "; careerP0%(I, 33): If C1 = 1 Then 401
            922 Locate 13, 40: Input "35. ERR "; careerP0%(I, 34): If C1 = 1 Then 401
            923 Locate 14, 40: Input "36. CHS "; careerP0%(I, 35): If C1 = 1 Then 401
            924 Locate 15, 40: Input "37. LHS "; careerP0%(I, 36): If C1 = 1 Then 401
            925 Locate 16, 40: Input "38. GIDP "; careerP0%(I, 37): If C1 = 1 Then 401
            926 Locate 17, 40: Input "39. PO "; careerP0%(I, 38): If C1 = 1 Then 401
            927 Locate 18, 40: Input "40. A "; careerP0%(I, 39): If C1 = 1 Then 401
            928 Locate 19, 40: Input "41. SH "; careerP0%(I, 40): If C1 = 1 Then 401
            929 Locate 20, 40: Input "42. SF "; careerP0%(I, 41): If C1 = 1 Then 401
        End If

        401 '

        Do
            C1 = 1

            Locate 23, 40: Input "# TO CHANGE (0 TO CONTINUE) "; B9

            Select Case B9
                Case 1: GoTo 330
                Case 2: GoTo 335
                Case 3: GoTo 340
                Case 4: GoTo 345
                Case 5: GoTo 350
                Case 6: GoTo 355
                Case 7: GoTo 360
                Case 8: GoTo 365
                Case 9: GoTo 370
                Case 10: GoTo 375
                Case 11: GoTo 380
                Case 12: GoTo 385
                Case 13: GoTo 390
                Case 14: GoTo 391
                Case 15: GoTo 392
                Case 16: GoTo 393
                Case 17: GoTo 394
                Case 18: GoTo 395
                Case 19: GoTo 396
                Case 20: GoTo 397
                Case 21: GoTo 398
                Case 22: GoTo 399
                Case 23: GoTo 400
                Case 24: GoTo 911
                Case 25: GoTo 912
                Case 26: GoTo 913
                Case 27: GoTo 914
                Case 28: GoTo 915
                Case 29: GoTo 916
                Case 30: GoTo 917
                Case 31: GoTo 918
                Case 32: GoTo 919
                Case 33: GoTo 920
                Case 34: GoTo 921
                Case 35: GoTo 922
                Case 36: GoTo 923
                Case 37: GoTo 924
                Case 38: GoTo 925
                Case 39: GoTo 926
                Case 40: GoTo 927
                Case 41: GoTo 928
                Case 42: GoTo 929
            End Select

            C1 = 0

            If X% > 0 Then Call SelectPitchers_SINPUT(D0%, D1%, L2%, W2%, Z$) 'GoTo 401

        Loop Until B9 = 0

    Next I

    ' Save the File
    Open diskPaths$(1) + Z$ For Output As #1
    Print #1, A$
    Print #1, W2%
    Print #1, L2%
    For I = 0 To 22: Print #1, statB$(I): Print #1, statB%(I): Next I
    For I = 0 To 21: Print #1, statP$(I): Print #1, statP%(I): Next I
    For I = 0 To 22: For I1 = 0 To 21: Print #1, careerB0%(I, I1): Next I1: Next I
    For I = 0 To 21: For I1 = 0 To 41: Print #1, careerP0%(I, I1): Next I1: Next I
    Print #1, D0%: Print #1, D1%
    For I = 0 To 21: Print #1, statT%(I): Next I
    For I = 0 To 22: Print #1, statT1%(I): Next I
    Close #1

End Sub

'----------------------------------------
'       GetTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_SINPUT (yearNumber$, numberTeams)

    Shared ratings%()

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
            Call SelectTeam_SINPUT(yearNumber$, numberTeams)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            If _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 to fileLength& / 4342

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

                Call SelectTeam_SINPUT(yearNumber$, numberTeams)

            Else

                BO% = 1
                Locate 10, 8
                Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            End If '

        End If

        T$ = RTrim$(Q$(0))

        If BO% = 1 Then
            BO% = 0
        Else

            Locate 2, 33
            Color ratings%(11), ratings%(12): Print T$

            Color 15, 4

            Do
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'       SelectTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_SINPUT (yearNumber$, numberTeams)

    Shared ratings%()

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
    Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        T = teamIndex%(Selection%)
		teamYear$ = RTrim$(yearNumber$)

        Locate 2, 33: Color 31
		Print "LOADING..."

        If _FileExists(diskPaths$(0) + "FCTEAMS." + teamYear$) Then

            Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

            Get #1, T

            For I = 1 To 13
                ratings%(I) = CVI(Q$(I))
            Next

            Close 1
        Else
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            BO% = 1
        End If
    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'     ChangePitchingStats Subroutine
'----------------------------------------
'...explanation...
Sub ChangePitchingStats (L2%, W2%)

    Shared statT1%()

    Cls

    Print "CHANGE TEAM PITCHING STATS"
    Print
    X% = 2

    Locate 2: Print "1. WON "; W2%
    Locate 3: Print "2. LOST "; L2%
    Locate 4: Print "3. TM SV "; statT1%(12)
    Locate 5: Print "4. (TM IP X 3) "; statT1%(0)
    Locate 6: Print "5. TM HITS "; statT1%(1)
    Locate 7: Print "6. TM BB "; statT1%(4)
    Locate 8: Print "7. TM SO "; statT1%(5)
    Locate 9: Print "8. TM CG "; statT1%(10)
    Locate 10: Print "9. TM RUNS "; statT1%(2)
    Locate 11: Print "10. TM E R "; statT1%(3)
    Locate 12: Print "11. TM SHT OUT "; statT1%(11)
    Locate 13: Print "12. TM STARTS "; statT1%(9)
    Locate 14: Print "13. TM HRA "; statT1%(14)
    Locate 15: Print "14. TM DP TURNED "; statT1%(13)
    Locate 16: Print "15. TM SH "; statT1%(15)
    Locate 17: Print "16. TM SF "; statT1%(16)
    Locate 18: Print "17. TM RUNNERS INHERITED (RI) "; statT1%(17)
    Locate 19: Print "18. TM RI SCORED "; statT1%(18)
    Locate 20: Print "19. TM BLOWN SAVES "; statT1%(19)
    Locate 21: Print "20. TM QUALITY STARTS "; statT1%(21)

End Sub

'----------------------------------------
'     SelectBatters_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectBatters_SINPUT (Z$)

    Shared statB$()
    Shared careerB0%()

    Cls

    Print "CHANGE BATTERS "
    Print
    X% = 2

    For I = 0 To 10
        Locate I + 5, 5
		Print Using "##"; I + 1;
		Print " "; statB$(I); Tab(40);
		Print Using "##"; I + 13;
		Print " "; statB$(I + 12)
    Next

    Locate , 5
	Print Using "##"; 12;
	Print " "; statB$(11)

    Print

    Do
        Print "# TO CHANGE (1-23) (0 TO CONTINUE) "; I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 0 And I <= 23

    If I = 0 Then
        X% = 0
        Call SelectPitchers_SINPUT(D0%, D1%, L2%, W2%, Z$)
    Else
        I = I - 1

        Cls

		Locate 1: Print "1. NAME "; statB$(I)
        Locate 2: Print "2. AB "; careerB0%(I, 0)
        Locate 3: Print "3. RUNS "; careerB0%(I, 2)
        Locate 4: Print "4. HITS "; careerB0%(I, 1)
        Locate 5: Print "5. DBL "; careerB0%(I, 4)
        Locate 6: Print "6. TRPL "; careerB0%(I, 5)
        Locate 7: Print "7. HR "; careerB0%(I, 6)
        Locate 8: Print "8. RBI "; careerB0%(I, 3)
        Locate 9: Print "9. BB "; careerB0%(I, 7)
        Locate 10: Print "10. SO "; careerB0%(I, 8)
        Locate 11: Print "11. ERR "; careerB0%(I, 11)
        Locate 12: Print "12. SB "; careerB0%(I, 9)
        Locate 13: Print "13. CS "; careerB0%(I, 10)
        Locate 14: Print "14. G  "; careerB0%(I, 14)
        Locate 15: Print "15. CHS "; careerB0%(I, 12)
        Locate 16: Print "16. LHS "; careerB0%(I, 13)
        Locate 17: Print "17. GIDP "; careerB0%(I, 15)
        Locate 18: Print "18. PO "; careerB0%(I, 16)
        Locate 19: Print "19. A  "; careerB0%(I, 17)
        Locate 20: Print "20. SH "; careerB0%(I, 18)
        Locate 21: Print "21. SF "; careerB0%(I, 19)

    End If

End Sub

'----------------------------------------
'    SelectPitchers_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectPitchers_SINPUT (D0%, D1%, L2%, W2%, Z$)

    Shared statB$(), statP$()
    Shared statB%(), careerB0%(), statP%(), careerP0%()
    Shared statT%(), statT1%()

    Cls
    Print "CHANGE PITCHERS"
    X% = 2

    For I = 0 To 10
        Locate I + 5, 5: Print Using "##"; I + 1;
        Print " "; statP$(I); Tab(40);
        Print Using "##"; I + 12;: Print " "; statP$(I + 11)
    Next

    Print
    Print
    Do
        Print "# TO CHANGE (1-22) (0 TO CONTINUE) ";
        I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 0 And I <= 23

    If I = 0 Then
        X% = 0
        Open diskPaths$(1) + Z$ For Output As #1
        Print #1, A$: Print #1, W2%: Print #1, L2%

        For I = 0 To 22: Print #1, statB$(I): Print #1, statB%(I): Next I
        For I = 0 To 21: Print #1, statP$(I): Print #1, statP%(I): Next I
        For I = 0 To 22: For I1 = 0 To 21: Print #1, careerB0%(I, I1): Next I1: Next I
        For I = 0 To 21: For I1 = 0 To 41: Print #1, careerP0%(I, I1): Next I1: Next I

        Print #1, D0%: Print #1, D1%

        For I = 0 To 21: Print #1, statT%(I): Next I
        For I = 0 To 22: Print #1, statT1%(I): Next I

        Close #1
    Else
        I = I - 1
        Cls: Locate 1: Print "1. NAME "; statP$(I)
        Locate 2: Print "2. WON "; careerP0%(I, 6);
        Locate , 40: Print "24. AB "; careerP0%(I, 23)
        Locate 3: Print "3. LOST "; careerP0%(I, 7);
        Locate , 40: Print "25. H "; careerP0%(I, 24)
        Locate 4: Print "4. SV "; careerP0%(I, 12);
        Locate , 40: Print "26. R "; careerP0%(I, 25)
        Locate 5: Print "5. (IP X 3) "; careerP0%(I, 0);
        Locate , 40: Print "27. RBI "; careerP0%(I, 26)
        Locate 6: Print "6. HITS "; careerP0%(I, 1);
        Locate , 40: Print "28. DBL "; careerP0%(I, 27)
        Locate 7: Print "7. BB "; careerP0%(I, 4);
        Locate , 40: Print "29. TRPL "; careerP0%(I, 28)
        Locate 8: Print "8. SO "; careerP0%(I, 5);
        Locate , 40: Print "30. HR "; careerP0%(I, 29)
        Locate 9: Print "9. CG "; careerP0%(I, 10);
        Locate , 40: Print "31. BB "; careerP0%(I, 30)
        Locate 10: Print "10. RUNS "; careerP0%(I, 2);
        Locate , 40: Print "32. K "; careerP0%(I, 31)
        Locate 11: Print "11. E R "; careerP0%(I, 3);
        Locate , 40: Print "33. SB "; careerP0%(I, 32)
        Locate 12: Print "12. SH OUT "; careerP0%(I, 11);
        Locate , 40: Print "34. CS "; careerP0%(I, 33)
        Locate 13: Print "13. GAMES "; careerP0%(I, 8);
        Locate , 40: Print "35. ERR "; careerP0%(I, 34)
        Locate 14: Print "14. STARTS "; careerP0%(I, 9);
        Locate , 40: Print "36. CHS "; careerP0%(I, 35)
        Locate 15: Print "15. HR ALL "; careerP0%(I, 14);
        Locate , 40: Print "37. LHS "; careerP0%(I, 36)
        Locate 16: Print "16. GIDP "; careerP0%(I, 13);
        Locate , 40: Print "38. GIDP "; careerP0%(I, 37)
        Locate 17: Print "17. SH "; careerP0%(I, 15);
        Locate , 40: Print "39. PO "; careerP0%(I, 38)
        Locate 18: Print "18. SF "; careerP0%(I, 16);
        Locate , 40: Print "40. A "; careerP0%(I, 39)
        Locate 19: Print "19. RUNNERS INHERITED (RI) "; careerP0%(I, 17);
        Locate , 40: Print "41. SH "; careerP0%(I, 40)
        Locate 20: Print "20. RI SCORED "; careerP0%(I, 18);
        Locate , 40: Print "42. SF "; careerP0%(I, 41)
        Locate 21: Print "21. BLOWN SAVES "; careerP0%(I, 19)
        Locate 22: Print "22. RUN SUPPORT "; careerP0%(I, 20)
        Locate 23: Print "23. QUALITY STARTS "; careerP0%(I, 21)

        'GOTO 401

    End If

End Sub

'----------------------------------------
'    PromptScheduleChanges Subroutine
'----------------------------------------
'...explanation...
Sub PromptScheduleChanges (Z$)

    Shared H%(), I%(), V%()
    Shared statH$(), L$(), PL$(), W$()

    statsFile$ = diskPaths$(1) + Z$ + "S"

    If Not _FileExists(statsFile$) Then
        Locate 10, 8
        Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else
        Cls

        Open statsFile$ For Input As #1
        Input #1, GAMES%

        For I = 1 To 162: Input #1, H%(I): Next
        For I = 1 To 162: Input #1, statH$(I): Next
        For I = 1 To 162: Input #1, V%(I): Next
        For I = 1 To 162: Input #1, PL$(I): Next
        For I = 1 To 162: Input #1, W$(I): Next
        For I = 1 To 162: Input #1, L$(I): Next
        For I = 1 To 162: Input #1, I%(I): Next

        Close 1

        Cls

        Do
            1050 Locate 1, 1: Print "1. SCORE FOR "; Z$; " ";: Input H%: If C1 = 1 Then 1100
            1051 Locate 2, 1: Print "2. OPPONENT'S NAME ";: Input H$: If C1 = 1 Then 1100
            1052 Locate 3, 1: Print "3. OPPONENT'S SCORE ";: Input V%: If C1 = 1 Then 1100
            1053 Locate 4, 1: Print "4. (H)OME OR (A)WAY ";: Input PL$: If C1 = 1 Then 1100
            1054 Locate 5, 1: Print "5. WINNING PITCHER ";: Input W$: If C1 = 1 Then 1100
            1055 Locate 6, 1: Print "6. LOSING PITCHER ";: Input L$: If C1 = 1 Then 1100
            1056 Locate 7, 1: Print "7. NUMBER OF INNINGS ";: Input I%: If C1 = 1 Then 1100

            1057 C1 = 1

            1100 '
            Do
                Locate 9, 1: Print "ENTER # TO CHANGE (1-7) OR 0 TO CONTINUE"
                I$ = GetKeyPress$
                D = Val(I$)

                Select Case D
                    Case 1: GoTo 1050
                    Case 2: GoTo 1051
                    Case 3: GoTo 1052
                    Case 4: GoTo 1053
                    Case 5: GoTo 1054
                    Case 6: GoTo 1055
                    Case 7: GoTo 1056
                End Select

            Loop Until D >= 0 And D <= 7

            GAMES% = GAMES% + 1
            H%(GAMES%) = H%
            statH$(GAMES%) = H$
            V%(GAMES%) = V%
            PL$(GAMES%) = PL$
            W$(GAMES%) = W$
            L$(GAMES%) = L$
            I%(GAMES%) = I%

            Print "ADD ANOTHER GAME (YN)";

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then C1 = 0

        Loop Until UCase$(I$) = "N"

        Open diskPaths$(1) + Z$ + "S" For Output As #1

        Print #1, GAMES%

        For I = 1 To 162: Print #1, H%(I): Next
        For I = 1 To 162: Print #1, statH$(I): Next
        For I = 1 To 162: Print #1, V%(I): Next
        For I = 1 To 162: Print #1, PL$(I): Next
        For I = 1 To 162: Print #1, W$(I): Next
        For I = 1 To 162: Print #1, L$(I): Next
        For I = 1 To 162: Print #1, I%(I): Next

        Close 1

        Cls

    End If 'Done checking if file exiss

End Sub
