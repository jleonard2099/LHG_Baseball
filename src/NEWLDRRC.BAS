Sub NEWLDRRC ()

    _Title "Full Count Baseball - Season Records"

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        CALL TITLE
        menuChoices$(1) = "UPDATE SEASON RECORDS    "
        menuChoices$(2) = "VIEW/PRINT SEASON RECORDS"
        menuChoices$(3) = "RETURN TO MAIN MENU      "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15:FileBGColr% = 1
        HiLiteFGColr% = 0:HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 6
        Column% = 30

        CALL box0(5, 29, 9, 55, 2,  15, 1)  
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        COLOR , 0

        If ExitCode% <> 27 Then
            Call SelectLeague (LGE$)

            Select Case Selection%
                Case 1: Call UpdateRecords (LGE$)
                Case 2: Call ViewPrintRecords (LGE$)
                Case 3: Cls
            End Select
            
        End If

    Loop Until Selection% = 3 OR ExitCode% = 27

End Sub


'----------------------------------------
'       SelectLeague Subroutine
'----------------------------------------
'...explanation...
Sub SelectLeague (leagueFile$)

    Do
        Spec$ = diskPaths$(2) + "*.BAT"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        IF Count% <> 0 THEN
        
            REDIM batFiles$(0 TO Count%)
            FOR X = 1 TO Count%
              batFiles$(X) = SPACE$(12)
            NEXT
            batFiles$(0) = Spec$
            CALL ReadFile(batFiles$())

            Do
                CLS
                COLOR , 1
                CLS
                LOCATE , 28
                COLOR 14
                PRINT "SELECT LEAGUE SEASON": COLOR 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15:FileBGColr% = 1
                HiLiteFGColr% = 0:HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                LOCATE 25, 1
                COLOR 14
                PRINT "ENTER"; : COLOR 15
                PRINT "-SELECTS TEAM";

                _MouseShow "DEFAULT"
                MAMenu batFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                IF ExitCode% <> 27 THEN
                    targetFile$ = batFiles$(Selection%)
                    LOCATE 2, 33: PRINT targetFile$
                    LOCATE , 33: PRINT "ANY CHANGE (YN)";
                    COLOR 15, 0
                    I$ = GetKeyPress$
                End If
                
            Loop Until UCase$(I$) = "N"

            IF ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                leagueFile$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
            End If

        End If 'Done checking file count

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'       UpdateRecords Subroutine
'----------------------------------------
'...explanation...
Sub UpdateRecords (leagueFile$)

    Shared BK%(), PM%()
    Shared BRC!(), BTYR!(), GM1!()
    Shared PRC!(), PRYR!()
    Shared TRC!(), TRC1!(), TRYR!(), TRYR1!()
    Shared TT!(), TT1!()

    Shared BRT$(), PRC$(), PRT$()
    Shared leaderBL$(), PL$()
    Shared TMB$(), TRC$(), TRC1$()

    Dim TT$(40, 22), TT1$(40, 23)
    REDIM TMP$(880)
    REDIM GM2(880)

    AAG = 920
    AAS = 880
    AAT = 80
    JB = 0
    JP = 0
    TMR = 2360
    TS = 0

    CLS
    INPUT "ENTER SEASON YEAR (1991,1992)"; seasonYear
    INPUT "IS THIS THE FINAL UPDATE FOR THE YEAR (Y/N)"; finalUpdate$

    PRINT : PRINT "ANY CHANGE (Y/N)"
    I$ = GetKeyPress$

    IF _FILEEXISTS(diskPaths$(2) + "BASEBALL.REC") THEN
        OPEN diskPaths$(2) + "BASEBALL.REC" FOR INPUT AS #1
            PRINT "READING IN CURRENT RECORDS"
            FOR I = 1 TO 32: INPUT #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): NEXT
            FOR I = 1 TO 28: INPUT #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): NEXT
            FOR I = 0 TO 24: INPUT #1, TRC!(I), TRC$(I), TRYR!(I): NEXT
            FOR I = 0 TO 24: INPUT #1, TRC1!(I), TRC1$(I), TRYR1!(I): NEXT
        CLOSE #1
    ELSE
        PRC!(23) = 100
        PRC!(25) = 100
        BRC!(24) = 999
        BRC!(25) = 999
        BRC!(26) = 999
        BRC!(27) = 999
        BRC!(28) = 999
        PRC!(27) = 100
        TRC1!(24) = 100
    END IF

    TMR = 3680

    OPEN diskPaths$(2) + leagueFile$ + ".BAT" FOR INPUT AS #1
        INPUT #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
        FOR I = 1 TO 920: INPUT #1, leaderBL$(I), TMB$(I), GM1!(I): COLOR 14: LOCATE 18, 45: TMR = TMR - 1: PRINT USING "#####"; TMR: NEXT I
        FOR I = 1 TO 920: FOR I1 = 1 TO 22: INPUT #1, BK%(I, I1): NEXT I1: COLOR 14: LOCATE 18, 45: TMR = TMR - 1: PRINT USING "#####"; TMR: NEXT I
    CLOSE 1

    OPEN diskPaths$(2) + leagueFile$ + ".PIT" FOR INPUT AS #1
        INPUT #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
        FOR I = 1 TO 880: INPUT #1, PL$(I), TMP$(I), GM2(I): COLOR 14: LOCATE 18, 45: TMR = TMR - 1: PRINT USING "#####"; TMR: NEXT I
        FOR I = 1 TO 880: FOR I1 = 1 TO 23: INPUT #1, PM%(I, I1): NEXT I1: COLOR 14: LOCATE 18, 45: TMR = TMR - 1: PRINT USING "#####"; TMR: NEXT I
    CLOSE 1

    OPEN diskPaths$(2) + leagueFile$ + ".TOT" FOR INPUT AS #1
        INPUT #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
        FOR I = 1 TO 40: INPUT #1, WW(I), LL(I): NEXT
        FOR I = 1 TO 40: FOR I1 = 0 TO 21: INPUT #1, TT!(I, I1), TT$(I, I1): NEXT: COLOR 14: LOCATE 18, 45: TMR = TMR - 1: PRINT USING "#####"; TMR: NEXT
        FOR I = 1 TO 40: FOR I1 = 0 TO 22: INPUT #1, TT1!(I, I1), TT1$(I, I1): NEXT: COLOR 14: LOCATE 18, 45: TMR = TMR - 1: PRINT USING "#####"; TMR: NEXT
    CLOSE #1

    LOCATE 7: PRINT "CALCULATING BATTER RECORDS . . . ."
    FOR I = 1 TO 920
        FOR I1 = 1 TO 20
            IF BK%(I, I1) >= BRC!(I1) THEN
                BRC!(I1) = BK%(I, I1)
                BRC$(I1) = leaderBL$(I)
                BRT$(I1) = TMB$(I)
                BTYR!(I1) = seasonYear
            End If
        NEXT I1

    AAG = AAG - 1
    LOCATE 7, 40: PRINT USING "###"; AAG

    NEXT I

    IF UCASE$(finalUpdate$) = "Y" THEN
        
        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 1) <> 0 THEN
                IF BK%(I, 2) / BK%(I, 1) >= BRC!(21) THEN
                    BRC!(21) = BK%(I, 2) / BK%(I, 1): BRC$(21) = leaderBL$(I): BRT$(21) = TMB$(I): BTYR!(21) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 2) <> 0 THEN
                IF (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1) >= BRC!(22) THEN
                    BRC!(22) = (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1)
                    BRC$(22) = leaderBL$(I): BRT$(22) = TMB$(I): BTYR!(22) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 2) <> 0 THEN
                IF (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8)) >= BRC!(23) THEN
                    BRC!(23) = (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8))
                    BRC$(23) = leaderBL$(I): BRT$(23) = TMB$(I): BTYR!(23) = seasonYear
                END IF
            END IF

        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 7) <> 0 THEN
                IF BK%(I, 1) / BK%(I, 7) <= BRC!(24) THEN
                    BRC!(24) = BK%(I, 1) / BK%(I, 7)
                    BRC$(24) = leaderBL$(I)
                    BRT$(24) = TMB$(I)
                    BTYR!(24) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 9) <> 0 THEN
                IF BK%(I, 1) / BK%(I, 9) <= BRC!(25) THEN
                    BRC!(25) = BK%(I, 1) / BK%(I, 9)
                    BRC$(25) = leaderBL$(I)
                    BRT$(25) = TMB$(I)
                    BTYR!(25) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8)>= GM1!(I) * 3.1 AND BK%(I, 8) <> 0 THEN
                IF BK%(I, 1) / BK%(I, 8) > BRC!(26) THEN
                    BRC!(26) = BK%(I, 1) / BK%(I, 8)
                    BRC$(26) = leaderBL$(I)
                    BRT$(26) = TMB$(I)
                    BTYR!(26) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 4) <> 0 THEN
                IF BK%(I, 1) / BK%(I, 4) <= BRC!(27) THEN
                    BRC!(27) = BK%(I, 1) / BK%(I, 4)
                    BRC$(27) = leaderBL$(I)
                    BRT$(27) = TMB$(I)
                    BTYR!(27) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920

        IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 3) <> 0 THEN
            IF BK%(I, 1) / BK%(I, 3) <= BRC!(28) THEN
                    BRC!(28) = BK%(I, 1) / BK%(I, 3)
                    BRC$(28) = leaderBL$(I)
                    BRT$(28) = TMB$(I)
                    BTYR!(28) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 AND BK%(I, 16) <> 0 THEN
                IF BK%(I, 1) / BK%(I, 16) >= BRC!(29) THEN
                    BRC!(29) = BK%(I, 1) / BK%(I, 16)
                    BRC$(29) = leaderBL$(I)
                    BRT$(29) = TMB$(I)
                    BTYR!(29) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 21) + BK%(I, 22) >= BRC!(30) THEN
                BRC!(30) = BK%(I, 21) + BK%(I, 22)
                BRC$(30) = leaderBL$(I)
                BRT$(30) = TMB$(I)
                BTYR!(30) = seasonYear
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 22) >= BRC!(31) THEN
                BRC!(31) = BK%(I, 22)
                BRC$(31) = leaderBL$(I)
                BRT$(31) = TMB$(I)
                BTYR!(31) = seasonYear
            END IF
        NEXT I

        FOR I = 1 TO 920
            IF BK%(I, 21) + BK%(I, 22) <> 0 AND BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 2.1 THEN
                IF BK%(I, 22) / (BK%(I, 21) + BK%(I, 22)) >= BRC!(32) THEN
                    BRC!(32) = BK%(I, 22) / (BK%(I, 21) + BK%(I, 22))
                    BRC$(32) = leaderBL$(I)
                    BRT$(32) = TMB$(I)
                    BTYR!(32) = seasonYear
                END IF
            END IF
        NEXT I

    End If

    LOCATE 8: PRINT "CALCULATING PITCHER RECORDS . . . ."
    
    FOR I = 1 TO 880
        FOR I1 = 1 TO 23
            IF PM%(I, I1) >= PRC!(I1) THEN
                PRC!(I1) = PM%(I, I1): PRC$(I1) = PL$(I): PRT$(I1) = TMP$(I): PRYR!(I1) = seasonYear
            End If
        NEXT I1
        
        AAS = AAS - 1
        LOCATE 8, 40: PRINT USING "###"; AAS
    NEXT I

    IF UCASE$(finalUpdate$) = "Y" THEN

        FOR I = 1 TO 880
            IF PM%(I, 18) <> 0 AND PM%(I, 9) >= 25 THEN
                IF PM%(I, 19) / PM%(I, 18) <= PRC!(24) THEN
                    PRC!(24) = PM%(I, 19) / PM%(I, 18): PRC$(24) = PL$(I): PRT$(24) = TMP$(I): PRYR!(24) = seasonYear
                End If
            End If
        NEXT I

        FOR I = 1 TO 880
            IF PM%(I, 1) / 3 >= GM2(I) AND PM%(I, 21) <> 0 THEN 
                IF PM%(I, 21) / PM%(I, 9) < PRC!(25) THEN 
                    PRC!(25) = PM%(I, 21) / PM%(I, 9): PRC$(25) = PL$(I): PRT$(25) = TMP$(I): PRYR!(25) = seasonYear
                End If
            End If
        NEXT I

        'IF TXX = 1 THEN 2785
        FOR I = 1 TO 880
            IF PM%(I, 1) / 3 >= GM2(I) AND PM%(I, 1) <> 0 THEN 
                IF (PM%(I, 4) * 9) / (PM%(I, 1) / 3) > PRC!(23) THEN 
                    PRC!(23) = (PM%(I, 4) * 9) / (PM%(I, 1) / 3): PRC$(23) = PL$(I): PRT$(23) = TMP$(I): PRYR!(23) = seasonYear
                End If
            End If
        NEXT I

        FOR I = 1 TO 880
            IF PM%(I, 1) / 3 >= GM2(I) AND PM%(I, 1) <> 0 THEN 
                IF (PM%(I, 5) * 9) / (PM%(I, 1) / 3) > PRC!(25) THEN 
                    PRC!(25) = (PM%(I, 5) * 9) / (PM%(I, 1) / 3): PRC$(25) = PL$(I): PRT$(25) = TMP$(I): PRYR!(25) = seasonYear
                End If
            End If
        NEXT I

        FOR I = 1 TO 880
            IF PM%(I, 1) / 3 >= GM2(I) AND PM%(I, 1) <> 0 THEN 
                IF (PM%(I, 6) * 9) / (PM%(I, 1) / 3) < PRC!(26) THEN 
                    PRC!(26) = (PM%(I, 6) * 9) / (PM%(I, 1) / 3): PRC$(26) = PL$(I): PRT$(26) = TMP$(I): PRYR!(26) = seasonYear
                End If
            End If
        NEXT I

        FOR I = 1 TO 880
            IF PM%(I, 1) / 3 >= GM2(I) AND PM%(I, 1) <> 0 THEN 
                IF (PM%(I, 2) * 9) / (PM%(I, 1) / 3) > PRC!(27) THEN 
                    PRC!(27) = (PM%(I, 2) * 9) / (PM%(I, 1) / 3): PRC$(27) = PL$(I): PRT$(27) = TMP$(I): PRYR!(27) = seasonYear
                End If
            End If
        NEXT I

        FOR I = 1 TO 880
            IF PM%(I, 1) / 3 >= GM2(I) AND PM%(I, 1) <> 0 THEN 
                IF (PM%(I, 14) * 9) / (PM%(I, 1) / 3) < PRC!(28) THEN 
                    PRC!(28) = (PM%(I, 14) * 9) / (PM%(I, 1) / 3): PRC$(28) = PL$(I): PRT$(28) = TMP$(I): PRYR!(28) = seasonYear
                End If
            End If
        NEXT I
    
    End If
    
    LOCATE 9: PRINT "CALCULATING TEAM RECORDS . . . ."
    FOR I = 1 TO 40
        FOR I1 = 0 TO 21
            IF TT!(I, I1) > TRC!(I1) THEN 
                TRC!(I1) = TT!(I, I1)
                TRC$(I1) = TT$(I, I1)
                TRYR!(I1) = seasonYear 
            End If
        NEXT I1
        
        AAT = AAT - 1
        LOCATE 9, 40: PRINT USING "###"; AAT
    NEXT I

    IF UCASE$(finalUpdate$) = "Y" THEN 
        FOR I = 1 TO 40
            IF TT!(I, 0) <> 0 THEN
                IF TT!(I, 1) / TT!(I, 0) > TRC!(22) THEN 
                    TRC!(22) = TT!(I, 1) / TT!(I, 0)
                    TRC$(22) = TT$(I, 1)
                    TRYR!(22) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 40
            IF TT!(I, 0) <> 0 THEN
                IF (TT!(I, 1) + TT!(I, 4) + (TT!(I, 5) * 2) + (TT!(I, 6) * 3)) / TT!(I, 0) > TRC!(23) THEN 
                    TRC!(23) = (TT!(I, 1) + TT!(I, 4) + (TT!(I, 5) * 2) + (TT!(I, 6) * 3)) / TT!(I, 0)
                    TRC$(23) = TT$(I, 1)
                    TRYR!(23) = seasonYear
                END IF
            END IF
        NEXT I

        FOR I = 1 TO 40
            IF TT!(I, 0) <> 0 THEN
                IF (TT!(I, 1) + TT!(I, 7)) / (TT!(I, 0) + TT!(I, 7)) > TRC!(24) THEN 
                    TRC!(24) = (TT!(I, 1) + TT!(I, 7)) / (TT!(I, 0) + TT!(I, 7))
                    TRC$(24) = TT$(I, 1)
                    TRYR!(24) = seasonYear
                END IF
            END IF
        NEXT I

    Else

        FOR I = 1 TO 40
            FOR I1 = 0 TO 22
                IF TT1!(I, I1) > TRC1!(I1) THEN 
                    TRC1!(I1) = TT1!(I, I1): TRC1$(I1) = TT1$(I, I1): TRYR1!(I1) = seasonYear
                End If
            NEXT I1
            
            AAT = AAT - 1
            LOCATE 9, 40: PRINT USING "###"; AAT
        NEXT I

        IF UCASE$(finalUpdate$) = "Y" THEN 

            FOR I = 1 TO 40
                IF TT1!(I, 17) <> 0 THEN
                    IF TT1!(I, 18) / TT1!(I, 17) > TRC1!(23) THEN
                        TRC1!(23) = TT1!(I, 18) / TT1!(I, 17)
                        TRC1$(23) = TT1$(I, 0)
                        TRYR1!(23) = seasonYear
                    End If
                End If              
            NEXT I

            'IF TXX = 1 THEN
            FOR I = 1 TO 40
                IF TT1!(I, 0) <> 0 THEN
                    IF (TT1!(I, 3) * 9) / (TT1!(I, 0) / 3) < TRC1!(24) THEN
                        TRC1!(24) = (TT1!(I, 3) * 9) / (TT1!(I, 0) / 3)
                        TRC1$(24) = TT1$(I, 0)
                        TRYR1!(24) = seasonYear
                    END IF
                END IF
            NEXT I
        
        ELSE
        
            OPEN diskPaths$(2) + "BASEBALL.REC" FOR OUTPUT AS #1
            FOR I = 1 TO 32: WRITE #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): NEXT
            FOR I = 1 TO 28: WRITE #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): NEXT
            FOR I = 0 TO 24: WRITE #1, TRC!(I), TRC$(I), TRYR!(I): NEXT
            FOR I = 0 TO 24: WRITE #1, TRC1!(I), TRC1$(I), TRYR1!(I): NEXT
            CLOSE #1
        
        End If

    End If 'Done checking for final update
    
End Sub


'----------------------------------------
'       ViewPrintRecords Subroutine
'----------------------------------------
'...explanation...
Sub ViewPrintRecords (leagueFile$)

    Shared BRC!(), BTYR!(), PRC!(), PRYR!()
    Shared TRC!(), TRC1!(), TRYR!(), TRYR1!()
    Shared BRT$(), PRC$(), PRT$()
    Shared TP0$(), TP1A$(), TP3$()
    Shared TRC$(), TRC1$()

    CLS

    IF _FILEEXISTS(diskPaths$(2) + "BASEBALL.REC") THEN
        OPEN diskPaths$(2) + "BASEBALL.REC" FOR INPUT AS #1
        FOR I = 1 TO 32: INPUT #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): NEXT
        FOR I = 1 TO 28: INPUT #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): NEXT
        FOR I = 0 TO 24: INPUT #1, TRC!(I), TRC$(I), TRYR!(I): NEXT
        FOR I = 0 TO 24: INPUT #1, TRC1!(I), TRC1$(I), TRYR1!(I): NEXT
        CLOSE #1
    END IF
    
    CLS
    PRINT "(V)IEW OR (P)RINT"
    
    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P"

    IF UCASE$(I$) = "P" THEN 
        Call PrintRecords (leagueFile$)
    Else
        COLOR 15, 4: CLS
        PRINT leagueFile$; "  BATTING RECORDS": PRINT
        PRINT "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        PRINT STRING$(75, 205)

        FOR I = 1 TO 15
            PRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING "####"; BRC!(I); TAB(71); BTYR!(I)
        NEXT

        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        CLS 
        PRINT leagueFile$; "  BATTING RECORDS": PRINT
        PRINT "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        PRINT STRING$(75, 205)

        FOR I = 16 TO 20
            PRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING "####"; BRC!(I); TAB(71); BTYR!(I)
        NEXT

        FOR I = 21 TO 23
            PRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING ".###"; BRC!(I); : PRINT TAB(71); USING "####"; BTYR!(I)
        NEXT

        FOR I = 24 TO 29
            PRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(60); USING "###.#"; BRC!(I); : PRINT TAB(71); USING "####"; BTYR!(I)
        NEXT

        FOR I = 30 TO 31
            PRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING "###"; BRC!(I); : PRINT TAB(71); USING "####"; BTYR!(I)
        NEXT

        PRINT TP0$(32); TAB(28); BRC$(32); TAB(43); BRT$(32); TAB(62); USING ".###"; BRC!(32); : PRINT TAB(71); USING "####"; BTYR!(32)

        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        COLOR 15, 1: CLS
        PRINT leagueFile$; "  PITCHING RECORDS": PRINT
        PRINT "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        PRINT STRING$(75, 205)
        PRINT TP1A$(1); TAB(28); PRC$(1); TAB(43); PRT$(1); TAB(61); USING "###"; INT(PRC!(1) / 3);
        
        IF PRC!(1) - INT(PRC!(1) / 3) * 3 > 0 THEN PRINT "."; : PRINT USING "#"; PRC!(1) - (INT(PRC!(1) / 3) * 3);  ELSE PRINT ".0";
        
        PRINT TAB(72); USING "####"; PRYR!(1)
        
        FOR I = 2 TO 15
            PRINT TP1A$(I); TAB(28); PRC$(I); TAB(43); PRT$(I); TAB(62); USING "####"; PRC!(I); TAB(72); PRYR!(I)
        NEXT

        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        CLS 
        PRINT leagueFile$; "  PITCHING RECORDS": PRINT
        PRINT "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        PRINT STRING$(75, 205)
        
        FOR I = 16 TO 19
            PRINT TP1A$(I); TAB(28); PRC$(I); TAB(43); PRT$(I); TAB(62); USING "####"; PRC!(I); TAB(72); PRYR!(I)
        NEXT
        
        PRINT TP1A$(20); TAB(28); PRC$(24); TAB(43); PRT$(24); TAB(62); USING ".###"; PRC!(24) * 100; : PRINT TAB(72); USING "####"; PRYR!(24)
        PRINT TP1A$(21); TAB(28); PRC$(20); TAB(43); PRT$(20); TAB(62); USING "####"; PRC!(20); TAB(72); PRYR!(20)
        PRINT TP1A$(22); TAB(28); PRC$(25); TAB(43); PRT$(25); TAB(61); USING "##.##"; PRC!(25); : PRINT TAB(72); USING "####"; PRYR!(25)
        PRINT TP1A$(23); TAB(28); PRC$(22); TAB(43); PRT$(22); TAB(62); USING "####"; PRC!(22); TAB(72); PRYR!(22)
        PRINT TP1A$(24); TAB(28); PRC$(23); TAB(43); PRT$(23); TAB(60); USING "###.##"; PRC!(23); TAB(72);
        PRINT USING "####"; PRYR!(23)
        
        FOR I = 25 TO 28
            PRINT TP1A$(I); TAB(28); PRC$(I); TAB(43); PRT$(I); TAB(61); USING "##.##"; PRC!(I); : PRINT TAB(72); USING "####"; PRYR!(I)
        NEXT

        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        COLOR 14, 0: CLS
        PRINT leagueFile$; "  TEAM RECORDS": PRINT
        PRINT "CATEGORY                        TEAM                        RECORD     YEAR"
        PRINT STRING$(75, 205)
        
        FOR I = 0 TO 12
            PRINT TP2A$(I); TAB(28); TRC$(I); TAB(62); USING "####"; TRC!(I); TAB(72); TRYR!(I)
        NEXT
        
        FOR I = 15 TO 16
            PRINT TP2A$(I - 2); TAB(28); TRC$(I); TAB(62); USING "####"; TRC!(I); TAB(72); TRYR!(I)
        NEXT

        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        CLS
        PRINT leagueFile$; "  TEAM RECORDS": PRINT
        PRINT "CATEGORY                        TEAM                        RECORD     YEAR"
        PRINT STRING$(75, 205)
        
        FOR I = 17 TO 19
            PRINT TP2A$(I - 2); TAB(28); TRC$(I); TAB(62); USING "####"; TRC!(I); TAB(72); TRYR!(I)
        NEXT
        
        FOR I = 22 TO 24
            PRINT TP2A$(I - 4); TAB(28); TRC$(I); TAB(62); USING ".###"; TRC!(I); : PRINT TAB(72); USING "####"; TRYR!(I)
        NEXT
        
        PRINT TP3$(0); TAB(28); TRC1$(0); TAB(60); USING "####"; INT(TRC1!(0) / 3);
        
        IF TRC1!(0) - INT(TRC1!(0) / 3) * 3 > 0 THEN PRINT "."; : PRINT USING "#"; TRC1!(0) - (INT(TRC1!(0) / 3) * 3);  ELSE PRINT ".0";
        
        PRINT TAB(72); USING "####"; TRYR1!(0)
        
        FOR I = 1 TO 5
            PRINT TP3$(I); TAB(28); TRC1$(I); TAB(62); USING "####"; TRC1!(I); : PRINT TAB(72); USING "####"; TRYR1!(I)
        NEXT
        
        FOR I = 10 TO 12
            PRINT TP3$(I - 4); TAB(28); TRC1$(I); TAB(62); USING "####"; TRC1!(I); : PRINT TAB(72); USING "####"; TRYR1!(I)
        NEXT

        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        CLS
        PRINT leagueFile$; "  TEAM RECORDS": PRINT
        PRINT "CATEGORY                        TEAM                        RECORD     YEAR"
        PRINT STRING$(75, 205)
        
        FOR I = 13 TO 19
            PRINT TP3$(I - 4); TAB(28); TRC1$(I); TAB(62); USING "####"; TRC1!(I);
            PRINT TAB(72); USING "####"; TRYR1!(I)
        NEXT

        PRINT TP3$(16); TAB(28); TRC1$(21); TAB(62); USING "####"; TRC1!(21); : PRINT TAB(72); USING "####"; TRYR1!(21)
        PRINT TP3$(17); TAB(28); TRC1$(23); TAB(62); USING ".###"; TRC1!(23); : PRINT TAB(72); USING "####"; TRYR1!(23)
        PRINT TP3$(18); TAB(28); TRC1$(24); TAB(61); USING "##.##"; TRC1!(24); : PRINT TAB(72); USING "####"; TRYR1!(24)
        LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

    End If

End Sub


'----------------------------------------
'       PrintRecords Subroutine
'----------------------------------------
'...explanation...
Sub PrintRecords (leagueFile$)

    Shared BRC!(), BTYR!(), PRC!(), PRYR!()
    Shared TRC!(), TRC1!(), TRYR!(), TRYR1!()
    Shared BRT$(), PRC$(), PRT$()
    Shared TP0$(), TP1A$(), TP3$()
    Shared TRC$(), TRC1$()

    LPRINT leagueFile$; "  BATTING RECORDS": LPRINT
    LPRINT "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPRINT STRING$(75, 61)

    FOR I = 1 TO 20
        LPRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING "####"; BRC!(I); TAB(71); BTYR!(I)
    NEXT

    FOR I = 21 TO 23
        LPRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING ".###"; BRC!(I); : LPRINT TAB(71); USING "####"; BTYR!(I)
    NEXT

    FOR I = 24 TO 29
        LPRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(60); USING "###.#"; BRC!(I); : LPRINT TAB(71); USING "####"; BTYR!(I)
    NEXT

    FOR I = 30 TO 31
        LPRINT TP0$(I); TAB(28); BRC$(I); TAB(43); BRT$(I); TAB(62); USING "###"; BRC!(I); : LPRINT TAB(71); USING "####"; BTYR!(I)
    NEXT

    LPRINT TP0$(32); TAB(28); BRC$(32); TAB(43); BRT$(32); TAB(62); USING ".###"; BRC!(32); : LPRINT TAB(71); USING "####"; BTYR!(32)
    LPRINT CHR$(12)
    LPRINT leagueFile$; "  PITCHING RECORDS": LPRINT
    LPRINT "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPRINT STRING$(75, 61)
    LPRINT TP1A$(1); TAB(28); PRC$(1); TAB(43); PRT$(1); TAB(61); USING "###"; INT(PRC!(1) / 3);

    IF PRC!(1) - INT(PRC!(1) / 3) * 3 > 0 THEN LPRINT "."; : LPRINT USING "#"; PRC!(1) - (INT(PRC!(1) / 3) * 3);  ELSE LPRINT ".0";

    LPRINT TAB(72); USING "####"; PRYR!(1)

    FOR I = 2 TO 19
        LPRINT TP1A$(I); TAB(28); PRC$(I); TAB(43); PRT$(I); TAB(62); USING "####"; PRC!(I); TAB(72); PRYR!(I)
    NEXT

    LPRINT TP1A$(20); TAB(28); PRC$(24); TAB(43); PRT$(24); TAB(62); USING ".###"; PRC!(24) * 100; : LPRINT TAB(72); USING "####"; PRYR!(24)
    LPRINT TP1A$(21); TAB(28); PRC$(20); TAB(43); PRT$(20); TAB(62); USING "####"; PRC!(20); TAB(72); PRYR!(20)
    LPRINT TP1A$(22); TAB(28); PRC$(25); TAB(43); PRT$(25); TAB(61); USING "##.##"; PRC!(25); : LPRINT TAB(72); USING "####"; PRYR!(25)
    LPRINT TP1A$(23); TAB(28); PRC$(22); TAB(43); PRT$(22); TAB(62); USING "####"; PRC!(22); TAB(72); PRYR!(22)
    LPRINT TP1A$(24); TAB(28); PRC$(23); TAB(43); PRT$(23); TAB(60); USING "###.##"; PRC!(23); TAB(72);

    PRINT USING "####"; PRYR!(23)

    FOR I = 25 TO 28
        LPRINT TP1A$(I); TAB(28); PRC$(I); TAB(43); PRT$(I); TAB(61); USING "##.##"; PRC!(I); : LPRINT TAB(72); USING "####"; PRYR!(I)
    NEXT

    LPRINT CHR$(12)
    LPRINT leagueFile$; "  TEAM RECORDS": LPRINT
    LPRINT "CATEGORY                        TEAM                        RECORD     YEAR"
    LPRINT STRING$(75, 61)

    FOR I = 0 TO 12
        LPRINT TP2A$(I); TAB(28); TRC$(I); TAB(62); USING "####"; TRC!(I); TAB(72); TRYR!(I)
    NEXT

    FOR I = 15 TO 19
        LPRINT TP2A$(I - 2); TAB(28); TRC$(I); TAB(62); USING "####"; TRC!(I); TAB(72); TRYR!(I)
    NEXT

    FOR I = 22 TO 24
        LPRINT TP2A$(I - 4); TAB(28); TRC$(I); TAB(62); USING ".###"; TRC!(I); : LPRINT TAB(72); USING "####"; TRYR!(I)
    NEXT

    LPRINT TP3$(0); TAB(28); TRC1$(0); TAB(60); USING "####"; INT(TRC1!(0) / 3);

    IF TRC1!(0) - INT(TRC1!(0) / 3) * 3 > 0 THEN LPRINT "."; : LPRINT USING "#"; TRC1!(0) - (INT(TRC1!(0) / 3) * 3);  ELSE LPRINT ".0";

    LPRINT TAB(72); USING "####"; TRYR1!(0)

    FOR I = 1 TO 5
        LPRINT TP3$(I); TAB(28); TRC1$(I); TAB(62); USING "####"; TRC1!(I); : LPRINT TAB(72); USING "####"; TRYR1!(I)
    NEXT

    FOR I = 10 TO 19
        LPRINT TP3$(I - 4); TAB(28); TRC1$(I); TAB(62); USING "####"; TRC1!(I); : LPRINT TAB(72); USING "####"; TRYR1!(I)
    NEXT

    LPRINT TP3$(16); TAB(28); TRC1$(21); TAB(62); USING "####"; TRC1!(21); : LPRINT TAB(72); USING "####"; TRYR1!(21)
    LPRINT TP3$(17); TAB(28); TRC1$(23); TAB(62); USING ".###"; TRC1!(23); : LPRINT TAB(72); USING "####"; TRYR1!(23)
    LPRINT TP3$(18); TAB(28); TRC1$(24); TAB(61); USING "##.##"; TRC1!(24); : LPRINT TAB(72); USING "####"; TRYR1!(24)
    LPRINT CHR$(12)

End Sub
