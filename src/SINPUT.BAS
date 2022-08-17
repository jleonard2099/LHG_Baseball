Sub SINPUT ()

    _Title "Full Count Baseball - Stats Input"

    DefInt A-Z

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        CALL TITLE
        menuChoices$(1) = "EDIT/CHANGE EXISTING REPLAY STAT FILE"
        menuChoices$(2) = "ENTER SCHEDULE RESULTS               "
        menuChoices$(3) = "RETURN TO MAIN MENU                  "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15:FileBGColr% = 1
        HiLiteFGColr% = 0:HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 6
        Column% = 23

        CALL box0(5, 22, 9, 60, 2,  15, 1)  
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        COLOR , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1 Or 2: Call EditStatsSchedules (Selection%)
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 OR ExitCode% = 27

    '73 Exit Sub
End Sub


'----------------------------------------
'     EditStatsSchedules Subroutine
'----------------------------------------
'...explanation...
Sub EditStatsSchedules (Q)

    Shared B$(), P$()
    Shared statB%(), B0%(), L%()
    Shared statP%(), P0%()
    Shared T%(), T1%()

    CLS
    PRINT "CHANGE TEAM BATTING STATS"
    PRINT
    X% = 2

    Call GetTeam_SINPUT (yearNumber$, numberTeams)

    B$ = T$
    B$ = RTRIM$(B$)

    FOR XX = 4 TO 11
        IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
    NEXT XX
    
    FOR XX = 12 TO 14
        IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
    NEXT XX
    
    QQ = 8
    FOR XX = 11 TO 4 STEP -1
        IF MID$(B$, XX, 1) = "_" THEN 
            QQ = XX - 4
        ELSE
            Exit For
        End If
    NEXT XX

    'Z$ = LEFT$(Z$, 11)
    'Z$ = LEFT$(RIGHT$(B$, LEN(B$) - 3), 8) + "." + LEFT$(B$, 2)
    Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
    
    IF Q = 2 THEN 
        Call PromptScheduleChanges
    Else
        'Q = 1
        
        IF NOT _FILEEXISTS(diskPaths$(1) + Z$) THEN 
            LOCATE 10, 8
            CALL CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            CALL CSMsgBox("", 0, Pass)
        Else
            OPEN diskPaths$(1) + Z$ FOR INPUT AS #1

            INPUT #1, A$: INPUT #1, W2%: INPUT #1, L2%

            FOR I = 0 TO 22: INPUT #1, B$(I): INPUT #1, statB%(I): NEXT I
            FOR I = 0 TO 21: INPUT #1, P$(I): INPUT #1, statP%(I): NEXT I
            FOR I = 0 TO 22: FOR I1 = 0 TO 21: INPUT #1, B0%(I, I1): NEXT I1: NEXT I
            FOR I = 0 TO 21: FOR I1 = 0 TO 41: INPUT #1, P0%(I, I1): NEXT I1: NEXT I

            INPUT #1, D0%: INPUT #1, D1%

            FOR I = 0 TO 21: INPUT #1, T%(I): NEXT I
            FOR I = 0 TO 22: INPUT #1, T1%(I): NEXT I

            CLOSE #1

            COLOR L%(11), L%(12)
            CLS

            Call InputTeamStats (Z$, A$, D0%, D1%, L2%, W2%)

        End If

    End If

End Sub


'----------------------------------------
'       InputTeamStats Subroutine
'----------------------------------------
'...explanation...
Sub InputTeamStats (Z$, A$, D0%, D1%, L2%, W2%)

    Shared B$(), P$()
    Shared B0%(), P0%()
    Shared T%(), T1%()

    C1 = 0
    CLS

    PRINT "TEAM BATTING STATS"
    80 LOCATE 2: INPUT "1. TEAM NAME (EX. ST_LOUIS.64) "; Z$: IF C1 = 1 THEN 145
    85 LOCATE 3: INPUT "2. TM AB "; T%(0): IF C1 = 1 THEN 145
    90 LOCATE 4: INPUT "3. TM RUNS "; T%(2): IF C1 = 1 THEN 145
    95 LOCATE 5: INPUT "4. TM HITS "; T%(1): IF C1 = 1 THEN 145
    100 LOCATE 6: INPUT "5. TM DBL "; T%(4): IF C1 = 1 THEN 145
    105 LOCATE 7: INPUT "6. TM TRPL "; T%(5): IF C1 = 1 THEN 145
    110 LOCATE 8: INPUT "7. TM HR "; T%(6): IF C1 = 1 THEN 145
    115 LOCATE 9: INPUT "8. TM RBI "; T%(3): IF C1 = 1 THEN 145
    120 LOCATE 10: INPUT "9. TM BB "; T%(7): IF C1 = 1 THEN 145
    125 LOCATE 11: INPUT "10. TM SO "; T%(8): IF C1 = 1 THEN 145
    130 LOCATE 12: INPUT "11. TM ERR "; T%(11): IF C1 = 1 THEN 145
    135 LOCATE 13: INPUT "12. TM SB "; T%(9): IF C1 = 1 THEN 145
    140 LOCATE 14: INPUT "13. TM CS "; T%(10): IF C1 = 1 THEN 145
    141 LOCATE 15: INPUT "14. TM PO "; T%(16): IF C1 = 1 THEN 145
    142 LOCATE 16: INPUT "15. TM A  "; T%(17): IF C1 = 1 THEN 145
    143 LOCATE 17: INPUT "16. TM SH "; T%(18): IF C1 = 1 THEN 145
    144 LOCATE 18: INPUT "17. TM SF "; T%(19): IF C1 = 1 THEN 145
    116 LOCATE 19: INPUT "18. TM GIDP "; T%(15): IF C1 = 1 THEN 145
    117 LOCATE 20: INPUT "19. TM LOB "; T%(12): IF C1 = 1 THEN 145

    145 '
    Do
        C1 = 1
        
        LOCATE 22: INPUT "# TO CHANGE (0 TO CONTINUE) "; B9

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

        IF X% > 0 THEN Call ChangePitchingStats (L2%, W2%)

    Loop Until B9 = 0


    155 '
    CLS : PRINT "TEAM PITCHING STATS"
    160 LOCATE 2: INPUT "1. WON "; W2%: IF C1 = 1 THEN 220
    165 LOCATE 3: INPUT "2. LOST "; L2%: IF C1 = 1 THEN 220
    170 LOCATE 4: INPUT "3. TM SV "; T1%(12): IF C1 = 1 THEN 220
    175 LOCATE 5: INPUT "4. (TM IP X 3) "; T1%(0): IF C1 = 1 THEN 220
    180 LOCATE 6: INPUT "5. TM HITS "; T1%(1): IF C1 = 1 THEN 220
    185 LOCATE 7: INPUT "6. TM BB "; T1%(4): IF C1 = 1 THEN 220
    190 LOCATE 8: INPUT "7. TM SO "; T1%(5): IF C1 = 1 THEN 220
    195 LOCATE 9: INPUT "8. TM CG "; T1%(10): IF C1 = 1 THEN 220
    200 LOCATE 10: INPUT "9. TM RUNS "; T1%(2): IF C1 = 1 THEN 220
    205 LOCATE 11: INPUT "10. TM E R "; T1%(3): IF C1 = 1 THEN 220
    210 LOCATE 12: INPUT "11. TM SH OUT "; T1%(11): IF C1 = 1 THEN 220
    211 LOCATE 13: INPUT "12. TM STARTS "; T1%(9): IF C1 = 1 THEN 220
    212 LOCATE 14: INPUT "13. TM HRA "; T1%(14): IF C1 = 1 THEN 220
    213 LOCATE 15: INPUT "14. TM DP TURNED "; T1%(13): IF C1 = 1 THEN 220
    214 LOCATE 16: INPUT "15. TM SH "; T1%(15): IF C1 = 1 THEN 220
    215 LOCATE 17: INPUT "16. TM SF "; T1%(16): IF C1 = 1 THEN 220
    216 LOCATE 18: INPUT "17. TM RUNNERS INHERITED (RI) "; T1%(17): IF C1 = 1 THEN 220
    217 LOCATE 19: INPUT "18. TM RI SCORED "; T1%(18): IF C1 = 1 THEN 220
    218 LOCATE 20: INPUT "19. TM BLOWN SAVES "; T1%(19): IF C1 = 1 THEN 220
    219 LOCATE 21: INPUT "20. TM QUALITY STARTS "; T1%(21): IF C1 = 1 THEN 220
    
    220 '
    Do      
        C1 = 1
    
        LOCATE 22: INPUT "# TO CHANGE (0 TO CONTINUE) "; B9
        
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
        
        IF X% > 0 THEN Call SelectBatters_SINPUT 'GoTo 309 

    Loop Until B9 = 0

    
    230 '
    FOR I = 0 TO 22
        CLS

        235 LOCATE 1, 30: PRINT "BATTER - "; I + 1: LOCATE , 1
        240 LOCATE 1: INPUT "1. NAME "; B$(I): IF C1 = 1 THEN 309
        245 LOCATE 2: INPUT "2. AB "; B0%(I, 0): IF C1 = 1 THEN 309
        250 LOCATE 3: INPUT "3. RUNS "; B0%(I, 2): IF C1 = 1 THEN 309
        255 LOCATE 4: INPUT "4. HITS "; B0%(I, 1): IF C1 = 1 THEN 309
        260 LOCATE 5: INPUT "5. DBL "; B0%(I, 4): IF C1 = 1 THEN 309
        265 LOCATE 6: INPUT "6. TRPL "; B0%(I, 5): IF C1 = 1 THEN 309
        270 LOCATE 7: INPUT "7. HR "; B0%(I, 6): IF C1 = 1 THEN 309
        275 LOCATE 8: INPUT "8. RBI "; B0%(I, 3): IF C1 = 1 THEN 309
        280 LOCATE 9: INPUT "9. BB "; B0%(I, 7): IF C1 = 1 THEN 309
        285 LOCATE 10: INPUT "10. SO "; B0%(I, 8): IF C1 = 1 THEN 309
        290 LOCATE 11: INPUT "11. ERR "; B0%(I, 11): IF C1 = 1 THEN 309
        295 LOCATE 12: INPUT "12. SB "; B0%(I, 9): IF C1 = 1 THEN 309
        300 LOCATE 13: INPUT "13. CS "; B0%(I, 10): IF C1 = 1 THEN 309
        301 LOCATE 14: INPUT "14.  G "; B0%(I, 14): IF C1 = 1 THEN 309
        302 LOCATE 15: INPUT "15. CHS "; B0%(I, 12): IF C1 = 1 THEN 309
        303 LOCATE 16: INPUT "16. LHS "; B0%(I, 13): IF C1 = 1 THEN 309
        304 LOCATE 17: INPUT "17. GIDP "; B0%(I, 15): IF C1 = 1 THEN 309
        305 LOCATE 18: INPUT "18. PO "; B0%(I, 16): IF C1 = 1 THEN 309
        306 LOCATE 19: INPUT "19. A  "; B0%(I, 17): IF C1 = 1 THEN 309
        307 LOCATE 20: INPUT "20. SH "; B0%(I, 18): IF C1 = 1 THEN 309
        308 LOCATE 21: INPUT "21. SF "; B0%(I, 19): IF C1 = 1 THEN 309

        309 '
        Do
            C1 = 1

            LOCATE 23: INPUT "# TO CHANGE (0 TO CONTINUE) "; B9

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

            IF X% > 0 THEN Call SelectBatters_SINPUT 'GoTo 309 

        Loop Until B9 = 0

    NEXT I

    '-----
    FOR I = 0 TO 21: CLS

        325 LOCATE 1, 30: PRINT "PITCHER - "; I + 1: LOCATE , 1
        330 LOCATE 1: INPUT "1. NAME "; P$(I): IF C1 = 1 THEN 401
        335 LOCATE 2: INPUT "2. WON "; P0%(I, 6): IF C1 = 1 THEN 401
        340 LOCATE 3: INPUT "3. LOST "; P0%(I, 7): IF C1 = 1 THEN 401
        345 LOCATE 4: INPUT "4. SV "; P0%(I, 12): IF C1 = 1 THEN 401
        350 LOCATE 5: INPUT "5. (IP X 3) "; P0%(I, 0): IF C1 = 1 THEN 401
        355 LOCATE 6: INPUT "6. HITS "; P0%(I, 1): IF C1 = 1 THEN 401
        360 LOCATE 7: INPUT "7. BB "; P0%(I, 4): IF C1 = 1 THEN 401
        365 LOCATE 8: INPUT "8. SO "; P0%(I, 5): IF C1 = 1 THEN 401
        370 LOCATE 9: INPUT "9. CG "; P0%(I, 10): IF C1 = 1 THEN 401
        375 LOCATE 10: INPUT "10. RUNS "; P0%(I, 2): IF C1 = 1 THEN 401
        380 LOCATE 11: INPUT "11. E R "; P0%(I, 3): IF C1 = 1 THEN 401
        385 LOCATE 12: INPUT "12. SH OUT "; P0%(I, 11): IF C1 = 1 THEN 401
        390 LOCATE 13: INPUT "13. GAMES "; P0%(I, 8): IF C1 = 1 THEN 401
        391 LOCATE 14: INPUT "14. STARTS "; P0%(I, 9): IF C1 = 1 THEN 401
        392 LOCATE 15: INPUT "15. HR ALL "; P0%(I, 14): IF C1 = 1 THEN 401
        393 LOCATE 16: INPUT "16. GIDP "; P0%(I, 13): IF C1 = 1 THEN 401
        394 LOCATE 17: INPUT "17. SH "; P0%(I, 15): IF C1 = 1 THEN 401
        395 LOCATE 18: INPUT "18. SF "; P0%(I, 16): IF C1 = 1 THEN 401
        396 LOCATE 19: INPUT "19. RUNNERS INHERITED (RI) "; P0%(I, 17): IF C1 = 1 THEN 401
        397 LOCATE 20: INPUT "20. RI SCORED "; P0%(I, 18): IF C1 = 1 THEN 401
        398 LOCATE 21: INPUT "21. BLOWN SAVES "; P0%(I, 19): IF C1 = 1 THEN 401
        399 LOCATE 22: INPUT "22. RUN SUPPORT "; P0%(I, 20): IF C1 = 1 THEN 401
        400 LOCATE 23: INPUT "23. QUALITY STARTS "; P0%(I, 21): IF C1 = 1 THEN 401
        911 LOCATE 2, 40: INPUT "24. AB "; P0%(I, 23): IF C1 = 1 THEN 401
        912 LOCATE 3, 40: INPUT "25. H "; P0%(I, 24): IF C1 = 1 THEN 401
        913 LOCATE 4, 40: INPUT "26. R "; P0%(I, 25): IF C1 = 1 THEN 401
        914 LOCATE 5, 40: INPUT "27. RBI "; P0%(I, 26): IF C1 = 1 THEN 401
        915 LOCATE 6, 40: INPUT "28. DBL "; P0%(I, 27): IF C1 = 1 THEN 401
        916 LOCATE 7, 40: INPUT "29. TRPL "; P0%(I, 28): IF C1 = 1 THEN 401
        917 LOCATE 8, 40: INPUT "30. HR "; P0%(I, 29): IF C1 = 1 THEN 401
        918 LOCATE 9, 40: INPUT "31. BB "; P0%(I, 30): IF C1 = 1 THEN 401
        919 LOCATE 10, 40: INPUT "32. K "; P0%(I, 31): IF C1 = 1 THEN 401
        920 LOCATE 11, 40: INPUT "33. SB "; P0%(I, 32): IF C1 = 1 THEN 401
        921 LOCATE 12, 40: INPUT "34. CS "; P0%(I, 33): IF C1 = 1 THEN 401
        922 LOCATE 13, 40: INPUT "35. ERR "; P0%(I, 34): IF C1 = 1 THEN 401
        923 LOCATE 14, 40: INPUT "36. CHS "; P0%(I, 35): IF C1 = 1 THEN 401
        924 LOCATE 15, 40: INPUT "37. LHS "; P0%(I, 36): IF C1 = 1 THEN 401
        925 LOCATE 16, 40: INPUT "38. GIDP "; P0%(I, 37): IF C1 = 1 THEN 401
        926 LOCATE 17, 40: INPUT "39. PO "; P0%(I, 38): IF C1 = 1 THEN 401
        927 LOCATE 18, 40: INPUT "40. A "; P0%(I, 39): IF C1 = 1 THEN 401
        928 LOCATE 19, 40: INPUT "41. SH "; P0%(I, 40): IF C1 = 1 THEN 401
        929 LOCATE 20, 40: INPUT "42. SF "; P0%(I, 41): IF C1 = 1 THEN 401

        401 '
            
        Do
            C1 = 1

            LOCATE 23, 40: INPUT "# TO CHANGE (0 TO CONTINUE) "; B9

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

            IF X% > 0 THEN Call SelectPitchers_SINPUT (D0%, D1%, L2%, W2%) 'GoTo 401
            
        Loop Until B9 = 0

    NEXT I

    ' Save the File
    OPEN diskPaths$(1) + Z$ FOR OUTPUT AS #1
    PRINT #1, A$: PRINT #1, W2%: PRINT #1, L2%
    FOR I = 0 TO 22: PRINT #1, B$(I): PRINT #1, statB%(I): NEXT I
    FOR I = 0 TO 21: PRINT #1, P$(I): PRINT #1, statP%(I): NEXT I
    FOR I = 0 TO 22: FOR I1 = 0 TO 21: PRINT #1, B0%(I, I1): NEXT I1: NEXT I
    FOR I = 0 TO 21: FOR I1 = 0 TO 41: PRINT #1, P0%(I, I1): NEXT I1: NEXT I
    PRINT #1, D0%: PRINT #1, D1%
    FOR I = 0 TO 21: PRINT #1, T%(I): NEXT I
    FOR I = 0 TO 22: PRINT #1, T1%(I): NEXT I
    CLOSE #1

End Sub


'----------------------------------------
'       GetTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_SINPUT (yearNumber$, numberTeams)

    Shared L%()
    
    DO
        COLOR 15, 0: CLS
        JJ = 7
        LOCATE 6, 8: COLOR 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " "; : NEXT
        LOCATE 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " "; : LOCATE 7, 32
        LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
        LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
        LOCATE 7, 27: INPUT NY$

        IF NY$ = "" THEN 
            Call SelectTeam_SINPUT (yearNumber$, numberTeams)
        Else    

            YN$ = NY$
            ERASE teamNames$

            numberTeams = 0
            
            IF _FILEEXISTS(diskPaths$(0) + "FCTEAMS." + YN$) THEN 

                OPEN diskPaths$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
                Y = LOF(1)
                FIELD #1, 15 AS Q$(0)
                
                FOR I1 = 1 TO Y / 4342
                    GET #1, I1
                    FOR I2 = 1 TO 15
                        IF LEFT$(Q$(0),2) = "XX" OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN nullTeam = 1: Exit For
                    NEXT I2
                    
                    If nullTeam <> 1 Then
                        numberTeams = numberTeams + 1
                        teamNames$(numberTeams) = Q$(0)
                        teamIndex%(numberTeams) = I1
                    End If
                NEXT I1
                
                CLOSE 1
                
                Call SelectTeam_SINPUT (yearNumber$, numberTeams)

            Else

                BO% = 1
                LOCATE 10, 8
                CALL CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                CALL CSMsgBox("", 0, Pass)

            End If '

        End If

        T$ = RTRIM$(Q$(0))

        IF BO% = 1 THEN 
            BO% = 0
        Else
            
            LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12)
            PRINT T$: COLOR 15, 4
            LOCATE , 33: PRINT "ANY CHANGE (YN)";
            Do
                I$ =GetKeyPress$
            Loop Until UCase$(I$) = "N" OR UCase$(I$) = "Y"
        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       SelectTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_SINPUT (yearNumber$, numberTeams)

    Shared L%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15:FileBGColr% = 4
    HiLiteFGColr% = 0:HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    IF ExitCode% <> 27 THEN 

        T = teamIndex%(Selection%)
        LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
        
        IF _FILEEXISTS(diskPaths$(0) + "FCTEAMS." + YN$) THEN 
        
            OPEN diskPaths$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
            FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
            GET #1, T
            
            FOR I = 1 TO 13
                L%(I) = CVI(Q$(I))
            NEXT
            
            CLOSE 1
        Else
            LOCATE 10, 8
            CALL CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            CALL CSMsgBox("", 0, Pass)
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

    Shared T1%()

495 '
    CLS
    PRINT "CHANGE TEAM PITCHING STATS"
    PRINT
    X% = 2
    
    LOCATE 2: PRINT "1. WON "; W2%
    LOCATE 3: PRINT "2. LOST "; L2%
    LOCATE 4: PRINT "3. TM SV "; T1%(12)
    LOCATE 5: PRINT "4. (TM IP X 3) "; T1%(0)
    LOCATE 6: PRINT "5. TM HITS "; T1%(1)
    LOCATE 7: PRINT "6. TM BB "; T1%(4)
    LOCATE 8: PRINT "7. TM SO "; T1%(5)
    LOCATE 9: PRINT "8. TM CG "; T1%(10)
    LOCATE 10: PRINT "9. TM RUNS "; T1%(2)
    LOCATE 11: PRINT "10. TM E R "; T1%(3)
    LOCATE 12: PRINT "11. TM SHT OUT "; T1%(11)
    LOCATE 13: PRINT "12. TM STARTS "; T1%(9)
    LOCATE 14: PRINT "13. TM HRA "; T1%(14)
    LOCATE 15: PRINT "14. TM DP TURNED "; T1%(13)
    LOCATE 16: PRINT "15. TM SH "; T1%(15)
    LOCATE 17: PRINT "16. TM SF "; T1%(16)
    LOCATE 18: PRINT "17. TM RUNNERS INHERITED (RI) "; T1%(17)
    LOCATE 19: PRINT "18. TM RI SCORED "; T1%(18)
    LOCATE 20: PRINT "19. TM BLOWN SAVES "; T1%(19)
    LOCATE 21: PRINT "20. TM QUALITY STARTS "; T1%(21)

End Sub


'----------------------------------------
'     SelectBatters_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectBatters_SINPUT
    
    Shared B$()
    Shared B0%()

565 '
    CLS 
    PRINT "CHANGE BATTERS "
    PRINT
    X% = 2
    
    FOR I = 0 TO 10
        LOCATE I + 5, 5: PRINT USING "##"; I + 1; : PRINT " "; B$(I); TAB(40); : PRINT USING "##"; I + 13; : PRINT " "; B$(I + 12)
    NEXT
    
    LOCATE , 5: PRINT USING "##"; 12; : PRINT " "; B$(11)
    PRINT
    
    Do
        PRINT "# TO CHANGE (1-23) (0 TO CONTINUE) "; I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 0 And I <= 23
    
    IF I = 0 THEN 
        X% = 0
        Call SelectPitchers_SINPUT (D0%, D1%, L2%, W2%)
    Else
        I = I - 1
        CLS : LOCATE 1: PRINT "1. NAME "; B$(I)
        LOCATE 2: PRINT "2. AB "; B0%(I, 0)
        LOCATE 3: PRINT "3. RUNS "; B0%(I, 2)
        LOCATE 4: PRINT "4. HITS "; B0%(I, 1)
        LOCATE 5: PRINT "5. DBL "; B0%(I, 4)
        LOCATE 6: PRINT "6. TRPL "; B0%(I, 5)
        LOCATE 7: PRINT "7. HR "; B0%(I, 6)
        LOCATE 8: PRINT "8. RBI "; B0%(I, 3)
        LOCATE 9: PRINT "9. BB "; B0%(I, 7)
        LOCATE 10: PRINT "10. SO "; B0%(I, 8)
        LOCATE 11: PRINT "11. ERR "; B0%(I, 11)
        LOCATE 12: PRINT "12. SB "; B0%(I, 9)
        LOCATE 13: PRINT "13. CS "; B0%(I, 10)
        LOCATE 14: PRINT "14. G  "; B0%(I, 14)
        LOCATE 15: PRINT "15. CHS "; B0%(I, 12)
        LOCATE 16: PRINT "16. LHS "; B0%(I, 13)
        LOCATE 17: PRINT "17. GIDP "; B0%(I, 15)
        LOCATE 18: PRINT "18. PO "; B0%(I, 16)
        LOCATE 19: PRINT "19. A  "; B0%(I, 17)
        LOCATE 20: PRINT "20. SH "; B0%(I, 18)
        LOCATE 21: PRINT "21. SF "; B0%(I, 19)
        
        'GOTO 309

    End If
    
End Sub


'----------------------------------------
'    SelectPitchers_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectPitchers_SINPUT (D0%, D1%, L2%, W2%)

    Shared B$(), P$()
    Shared statB%(), B0%(), statP%(), P0%()
    Shared T%(), T1%()

    CLS
    PRINT "CHANGE PITCHERS"
    X% = 2
    
    FOR I = 0 TO 10
        LOCATE I + 5, 5: PRINT USING "##"; I + 1;
        PRINT " "; P$(I); TAB(40);
        PRINT USING "##"; I + 12; : PRINT " "; P$(I + 11)
    NEXT
    
    PRINT
    PRINT
    Do
        PRINT "# TO CHANGE (1-22) (0 TO CONTINUE) ";
        I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 0 And I <= 23

    IF I = 0 THEN 
        X% = 0
        OPEN diskPaths$(1) + Z$ FOR OUTPUT AS #1
        PRINT #1, A$: PRINT #1, W2%: PRINT #1, L2%
        
        FOR I = 0 TO 22: PRINT #1, B$(I): PRINT #1, statB%(I): NEXT I
        FOR I = 0 TO 21: PRINT #1, P$(I): PRINT #1, statP%(I): NEXT I
        FOR I = 0 TO 22: FOR I1 = 0 TO 21: PRINT #1, B0%(I, I1): NEXT I1: NEXT I
        FOR I = 0 TO 21: FOR I1 = 0 TO 41: PRINT #1, P0%(I, I1): NEXT I1: NEXT I
        
        PRINT #1, D0%: PRINT #1, D1%
        
        FOR I = 0 TO 21: PRINT #1, T%(I): NEXT I
        FOR I = 0 TO 22: PRINT #1, T1%(I): NEXT I
        
        CLOSE #1
    Else
        I = I - 1
        CLS : LOCATE 1: PRINT "1. NAME "; P$(I)
        LOCATE 2: PRINT "2. WON "; P0%(I, 6);
        LOCATE , 40: PRINT "24. AB "; P0%(I, 23)
        LOCATE 3: PRINT "3. LOST "; P0%(I, 7);
        LOCATE , 40: PRINT "25. H "; P0%(I, 24)
        LOCATE 4: PRINT "4. SV "; P0%(I, 12);
        LOCATE , 40: PRINT "26. R "; P0%(I, 25)
        LOCATE 5: PRINT "5. (IP X 3) "; P0%(I, 0);
        LOCATE , 40: PRINT "27. RBI "; P0%(I, 26)
        LOCATE 6: PRINT "6. HITS "; P0%(I, 1);
        LOCATE , 40: PRINT "28. DBL "; P0%(I, 27)
        LOCATE 7: PRINT "7. BB "; P0%(I, 4);
        LOCATE , 40: PRINT "29. TRPL "; P0%(I, 28)
        LOCATE 8: PRINT "8. SO "; P0%(I, 5);
        LOCATE , 40: PRINT "30. HR "; P0%(I, 29)
        LOCATE 9: PRINT "9. CG "; P0%(I, 10);
        LOCATE , 40: PRINT "31. BB "; P0%(I, 30)
        LOCATE 10: PRINT "10. RUNS "; P0%(I, 2);
        LOCATE , 40: PRINT "32. K "; P0%(I, 31)
        LOCATE 11: PRINT "11. E R "; P0%(I, 3);
        LOCATE , 40: PRINT "33. SB "; P0%(I, 32)
        LOCATE 12: PRINT "12. SH OUT "; P0%(I, 11);
        LOCATE , 40: PRINT "34. CS "; P0%(I, 33)
        LOCATE 13: PRINT "13. GAMES "; P0%(I, 8);
        LOCATE , 40: PRINT "35. ERR "; P0%(I, 34)
        LOCATE 14: PRINT "14. STARTS "; P0%(I, 9);
        LOCATE , 40: PRINT "36. CHS "; P0%(I, 35)
        LOCATE 15: PRINT "15. HR ALL "; P0%(I, 14);
        LOCATE , 40: PRINT "37. LHS "; P0%(I, 36)
        LOCATE 16: PRINT "16. GIDP "; P0%(I, 13);
        LOCATE , 40: PRINT "38. GIDP "; P0%(I, 37)
        LOCATE 17: PRINT "17. SH "; P0%(I, 15);
        LOCATE , 40: PRINT "39. PO "; P0%(I, 38)
        LOCATE 18: PRINT "18. SF "; P0%(I, 16);
        LOCATE , 40: PRINT "40. A "; P0%(I, 39)
        LOCATE 19: PRINT "19. RUNNERS INHERITED (RI) "; P0%(I, 17);
        LOCATE , 40: PRINT "41. SH "; P0%(I, 40)
        LOCATE 20: PRINT "20. RI SCORED "; P0%(I, 18);
        LOCATE , 40: PRINT "42. SF "; P0%(I, 41)
        LOCATE 21: PRINT "21. BLOWN SAVES "; P0%(I, 19)
        LOCATE 22: PRINT "22. RUN SUPPORT "; P0%(I, 20)
        LOCATE 23: PRINT "23. QUALITY STARTS "; P0%(I, 21)
        
        'GOTO 401

    End If


End Sub 


'----------------------------------------
'    PromptScheduleChanges Subroutine
'----------------------------------------
'...explanation...
Sub PromptScheduleChanges

    Shared H%(), I%(), V%()
    Shared H$(), L$(), PL$(), W$()

    IF _FILEEXISTS(diskPaths$(2) + Z$ + "S") THEN 
        CLS

        OPEN diskPaths$(2) + Z$ + "S" FOR INPUT AS #1
        INPUT #1, GAMES%

        FOR I = 1 TO 162: INPUT #1, H%(I): NEXT
        FOR I = 1 TO 162: INPUT #1, H$(I): NEXT
        FOR I = 1 TO 162: INPUT #1, V%(I): NEXT
        FOR I = 1 TO 162: INPUT #1, PL$(I): NEXT
        FOR I = 1 TO 162: INPUT #1, W$(I): NEXT
        FOR I = 1 TO 162: INPUT #1, L$(I): NEXT
        FOR I = 1 TO 162: INPUT #1, I%(I): NEXT

        CLOSE 1

        CLS

        Do
            1050 LOCATE 1, 1: PRINT "1. SCORE FOR "; Z$; " "; : INPUT H%: IF C1 = 1 THEN 1100
            1051 LOCATE 2, 1: PRINT "2. OPPONENT'S NAME "; : INPUT H$: IF C1 = 1 THEN 1100
            1052 LOCATE 3, 1: PRINT "3. OPPONENT'S SCORE "; : INPUT V%: IF C1 = 1 THEN 1100
            1053 LOCATE 4, 1: PRINT "4. (H)OME OR (A)WAY "; : INPUT PL$: IF C1 = 1 THEN 1100
            1054 LOCATE 5, 1: PRINT "5. WINNING PITCHER "; : INPUT W$: IF C1 = 1 THEN 1100
            1055 LOCATE 6, 1: PRINT "6. LOSING PITCHER "; : INPUT L$: IF C1 = 1 THEN 1100
            1056 LOCATE 7, 1: PRINT "7. NUMBER OF INNINGS "; : INPUT I%: IF C1 = 1 THEN 1100

            1057 C1 = 1

            1100 '
            Do
                LOCATE 9, 1: PRINT "ENTER # TO CHANGE (1-7) OR 0 TO CONTINUE"
                I$ = GetKeyPress$
                D = VAL(I$)
                
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
            H$(GAMES%) = H$
            V%(GAMES%) = V%
            PL$(GAMES%) = PL$
            W$(GAMES%) = W$
            L$(GAMES%) = L$
            I%(GAMES%) = I%

            Do
                PRINT "ADD ANOTHER GAME (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            IF UCASE$(I$) = "Y" THEN C1 = 0
            
        Loop Until UCase$(I$) = "N"


        OPEN diskPaths$(2) + Z$ + "S" FOR OUTPUT AS #1
        
        PRINT #1, GAMES%

        FOR I = 1 TO 162: PRINT #1, H%(I): NEXT
        FOR I = 1 TO 162: PRINT #1, H$(I): NEXT
        FOR I = 1 TO 162: PRINT #1, V%(I): NEXT
        FOR I = 1 TO 162: PRINT #1, PL$(I): NEXT
        FOR I = 1 TO 162: PRINT #1, W$(I): NEXT
        FOR I = 1 TO 162: PRINT #1, L$(I): NEXT
        FOR I = 1 TO 162: PRINT #1, I%(I): NEXT

        CLOSE 1
        
        CLS

    Else

        LOCATE 10, 8
        CALL CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        CALL CSMsgBox("", 0, Pass)

    End If 'Done checking if file exiss
    
End Sub


'805 
'CLS : PRINT "TEAM INFO"
'LOCATE 2: INPUT "1. TEAM NAME (EX. ST_LOUIS.82) "; Z$

'OPEN diskPaths$(1) + Z$ FOR OUTPUT AS #1
'PRINT #1, Z$: PRINT #1, 1: PRINT #1, 1
'FOR I = 0 TO 22: PRINT #1, "XXX": PRINT #1, statB%(I): NEXT I
'FOR I = 0 TO 21: PRINT #1, "XXX": PRINT #1, statP%(I): NEXT I
'FOR I = 0 TO 22: FOR I1 = 0 TO 21: PRINT #1, 0: NEXT I1: NEXT I
'FOR I = 0 TO 21: FOR I1 = 0 TO 41: PRINT #1, 0: NEXT I1: NEXT I
'PRINT #1, 0: PRINT #1, 0
'FOR I = 0 TO 21: PRINT #1, 1: NEXT I: FOR I = 0 TO 22: PRINT #1, 1: NEXT I

'CLOSE 1


'OPEN diskPaths$(1) + Z$ + "S" FOR OUTPUT AS #1

'PRINT #1, 0
'FOR I = 1 TO 162: PRINT #1, 0: NEXT I
'FOR I = 1 TO 162: PRINT #1, "X": NEXT I
'FOR I = 1 TO 162: PRINT #1, 0: NEXT I
'FOR I = 1 TO 162: PRINT #1, "N": NEXT I
'FOR I = 1 TO 162: PRINT #1, "Z": NEXT I
'FOR I = 1 TO 162: PRINT #1, "W": NEXT I
'FOR I = 1 TO 162: PRINT #1, 0: NEXT I

'CLOSE #1
