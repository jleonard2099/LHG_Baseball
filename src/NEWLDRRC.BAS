Sub NEWLDRRC ()

    _Title "Full Count Baseball - Season Records"

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE SEASON RECORDS    "
        menuChoices$(2) = "VIEW/PRINT SEASON RECORDS"
        menuChoices$(3) = "RETURN TO MAIN MENU      "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 30

        Call box0(6, 29, 10, 55, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Call SelectLeague(LGE$, menuExit%)

            If menuExit% <> 27 Then
                Select Case Selection%
                    Case 1: Call UpdateRecords(LGE$)
                    Case 2: Call ViewPrintRecords(LGE$)
                    Case 3: Cls
                End Select
            End If

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       SelectLeague Subroutine
'----------------------------------------
'...explanation...
Sub SelectLeague (leagueFile$, ExitCode%)

    Do
        Spec$ = diskPaths$(2) + "*.BAT"
		Count% = FCount%(Spec$)

        If Count% <> 0 Then

            ReDim batFiles$(0 To Count%)
            For X = 1 To Count%
                batFiles$(X) = Space$(12)
            Next
            batFiles$(0) = Spec$
            Call ReadFile(batFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE SEASON": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS SEASON";

                _MouseShow "DEFAULT"
                MAMenu batFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = batFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                leagueFile$ = Left$(targetFile$, Len(targetFile$) - 4)
            End If

        End If 'Done checking file count

    Loop Until UCase$(I$) = "N" Or ExitCode% = 27

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

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared leaderBL$(), leaderPL$()
    Shared TMB$(), TMP$(), TRC$(), TRC1$()

    Dim GM2(880)

    Dim TT$(40, 22), TT1$(40, 23)

    Dim WW(40), LL(40)

    AAG = 920
    AAS = 880
    AAT = 80
    JB = 0
    JP = 0
    TMR% = 2360
    TS = 0

    Cls
    Input "ENTER SEASON YEAR (1991,1992)"; seasonYear
    Do
        Locate 2
        Input "IS THIS THE FINAL UPDATE FOR THE YEAR (Y/N)"; finalUpdate$
    Loop Until UCase$(finalUpdate$) = "Y" Or UCase$(finalUpdate$) = "N"

    Print
    Print "ANY CHANGE (Y/N)"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If _FileExists(diskPaths$(2) + "BASEBALL.REC") Then
        Open diskPaths$(2) + "BASEBALL.REC" For Input As #1
        Print "READING IN CURRENT RECORDS"
        For I = 1 To 32: Input #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): Next
        For I = 1 To 28: Input #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): Next
        For I = 0 To 24: Input #1, TRC!(I), TRC$(I), TRYR!(I): Next
        For I = 0 To 24: Input #1, TRC1!(I), TRC1$(I), TRYR1!(I): Next
        Close #1
    Else
        PRC!(23) = 100
        PRC!(25) = 100
        BRC!(24) = 999
        BRC!(25) = 999
        BRC!(26) = 999
        BRC!(27) = 999
        BRC!(28) = 999
        PRC!(27) = 100
        TRC1!(24) = 100
    End If

    TMR% = 3680

    Open diskPaths$(2) + leagueFile$ + ".BAT" For Input As #1
    Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
    For I = 1 To 920: Input #1, leaderBL$(I), TMB$(I), GM1!(I): Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
    For I = 1 To 920: For I1 = 1 To 22: Input #1, BK%(I, I1): Next I1: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
    Close 1

    Open diskPaths$(2) + leagueFile$ + ".PIT" For Input As #1
    Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
    For I = 1 To 880: Input #1, leaderPL$(I), TMP$(I), GM2(I): Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
    For I = 1 To 880: For I1 = 1 To 23: Input #1, PM%(I, I1): Next I1: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
    Close 1

    Open diskPaths$(2) + leagueFile$ + ".TOT" For Input As #1
    Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS
    For I = 1 To 40: Input #1, WW(I), LL(I): Next
    For I = 1 To 40: For I1 = 0 To 21: Input #1, TT!(I, I1), TT$(I, I1): Next: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next
    For I = 1 To 40: For I1 = 0 To 22: Input #1, TT1!(I, I1), TT1$(I, I1): Next: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next
    Close #1

    Locate 7: Print "CALCULATING BATTER RECORDS . . . ."
    For I = 1 To 920
        For I1 = 1 To 20
            If BK%(I, I1) >= BRC!(I1) Then
                BRC!(I1) = BK%(I, I1)
                BRC$(I1) = leaderBL$(I)
                BRT$(I1) = TMB$(I)
                BTYR!(I1) = seasonYear
            End If
        Next I1

        AAG = AAG - 1
        Locate 7, 40: Print Using "###"; AAG

    Next I

    If finalUpdate$ = "Y" Then

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 1) <> 0 Then
                If BK%(I, 2) / BK%(I, 1) >= BRC!(21) Then
                    BRC!(21) = BK%(I, 2) / BK%(I, 1): BRC$(21) = leaderBL$(I): BRT$(21) = TMB$(I): BTYR!(21) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 2) <> 0 Then
                If (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1) >= BRC!(22) Then
                    BRC!(22) = (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1)
                    BRC$(22) = leaderBL$(I): BRT$(22) = TMB$(I): BTYR!(22) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 2) <> 0 Then
                If (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8)) >= BRC!(23) Then
                    BRC!(23) = (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8))
                    BRC$(23) = leaderBL$(I): BRT$(23) = TMB$(I): BTYR!(23) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 7) <> 0 Then
                If BK%(I, 1) / BK%(I, 7) <= BRC!(24) Then
                    BRC!(24) = BK%(I, 1) / BK%(I, 7)
                    BRC$(24) = leaderBL$(I)
                    BRT$(24) = TMB$(I)
                    BTYR!(24) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 9) <> 0 Then
                If BK%(I, 1) / BK%(I, 9) <= BRC!(25) Then
                    BRC!(25) = BK%(I, 1) / BK%(I, 9)
                    BRC$(25) = leaderBL$(I)
                    BRT$(25) = TMB$(I)
                    BTYR!(25) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 8) <> 0 Then
                If BK%(I, 1) / BK%(I, 8) > BRC!(26) Then
                    BRC!(26) = BK%(I, 1) / BK%(I, 8)
                    BRC$(26) = leaderBL$(I)
                    BRT$(26) = TMB$(I)
                    BTYR!(26) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 4) <> 0 Then
                If BK%(I, 1) / BK%(I, 4) <= BRC!(27) Then
                    BRC!(27) = BK%(I, 1) / BK%(I, 4)
                    BRC$(27) = leaderBL$(I)
                    BRT$(27) = TMB$(I)
                    BTYR!(27) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 3) <> 0 Then
                If BK%(I, 1) / BK%(I, 3) <= BRC!(28) Then
                    BRC!(28) = BK%(I, 1) / BK%(I, 3)
                    BRC$(28) = leaderBL$(I)
                    BRT$(28) = TMB$(I)
                    BTYR!(28) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 16) <> 0 Then
                If BK%(I, 1) / BK%(I, 16) >= BRC!(29) Then
                    BRC!(29) = BK%(I, 1) / BK%(I, 16)
                    BRC$(29) = leaderBL$(I)
                    BRT$(29) = TMB$(I)
                    BTYR!(29) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 21) + BK%(I, 22) >= BRC!(30) Then
                BRC!(30) = BK%(I, 21) + BK%(I, 22)
                BRC$(30) = leaderBL$(I)
                BRT$(30) = TMB$(I)
                BTYR!(30) = seasonYear
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 22) >= BRC!(31) Then
                BRC!(31) = BK%(I, 22)
                BRC$(31) = leaderBL$(I)
                BRT$(31) = TMB$(I)
                BTYR!(31) = seasonYear
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 21) + BK%(I, 22) <> 0 And BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 2.1 Then
                If BK%(I, 22) / (BK%(I, 21) + BK%(I, 22)) >= BRC!(32) Then
                    BRC!(32) = BK%(I, 22) / (BK%(I, 21) + BK%(I, 22))
                    BRC$(32) = leaderBL$(I)
                    BRT$(32) = TMB$(I)
                    BTYR!(32) = seasonYear
                End If
            End If
        Next I

    End If

    Locate 8: Print "CALCULATING PITCHER RECORDS . . . ."

    For I = 1 To 880
        For I1 = 1 To 23
            If PM%(I, I1) >= PRC!(I1) Then
                PRC!(I1) = PM%(I, I1): PRC$(I1) = leaderPL$(I): PRT$(I1) = TMP$(I): PRYR!(I1) = seasonYear
            End If
        Next I1

        AAS = AAS - 1
        Locate 8, 40: Print Using "###"; AAS
    Next I

    If finalUpdate$ = "Y" Then

        For I = 1 To 880
            If PM%(I, 18) <> 0 And PM%(I, 9) >= 25 Then
                If PM%(I, 19) / PM%(I, 18) <= PRC!(24) Then
                    PRC!(24) = PM%(I, 19) / PM%(I, 18): PRC$(24) = leaderPL$(I): PRT$(24) = TMP$(I): PRYR!(24) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 21) <> 0 Then
                If PM%(I, 21) / PM%(I, 9) < PRC!(25) Then
                    PRC!(25) = PM%(I, 21) / PM%(I, 9): PRC$(25) = leaderPL$(I): PRT$(25) = TMP$(I): PRYR!(25) = seasonYear
                End If
            End If
        Next I

        'IF TXX = 1 THEN 2785
        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 4) * 9) / (PM%(I, 1) / 3) > PRC!(23) Then
                    PRC!(23) = (PM%(I, 4) * 9) / (PM%(I, 1) / 3): PRC$(23) = leaderPL$(I): PRT$(23) = TMP$(I): PRYR!(23) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 5) * 9) / (PM%(I, 1) / 3) > PRC!(25) Then
                    PRC!(25) = (PM%(I, 5) * 9) / (PM%(I, 1) / 3): PRC$(25) = leaderPL$(I): PRT$(25) = TMP$(I): PRYR!(25) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 6) * 9) / (PM%(I, 1) / 3) < PRC!(26) Then
                    PRC!(26) = (PM%(I, 6) * 9) / (PM%(I, 1) / 3): PRC$(26) = leaderPL$(I): PRT$(26) = TMP$(I): PRYR!(26) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 2) * 9) / (PM%(I, 1) / 3) > PRC!(27) Then
                    PRC!(27) = (PM%(I, 2) * 9) / (PM%(I, 1) / 3): PRC$(27) = leaderPL$(I): PRT$(27) = TMP$(I): PRYR!(27) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 14) * 9) / (PM%(I, 1) / 3) < PRC!(28) Then
                    PRC!(28) = (PM%(I, 14) * 9) / (PM%(I, 1) / 3): PRC$(28) = leaderPL$(I): PRT$(28) = TMP$(I): PRYR!(28) = seasonYear
                End If
            End If
        Next I

    End If

    Locate 9: Print "CALCULATING TEAM RECORDS . . . ."
    For I = 1 To 40
        For I1 = 0 To 21
            If TT!(I, I1) > TRC!(I1) Then
                TRC!(I1) = TT!(I, I1)
                TRC$(I1) = TT$(I, I1)
                TRYR!(I1) = seasonYear
            End If
        Next I1

        AAT = AAT - 1
        Locate 9, 40: Print Using "###"; AAT
    Next I

    If finalUpdate$ = "Y" Then
        For I = 1 To 40
            If TT!(I, 0) <> 0 Then
                If TT!(I, 1) / TT!(I, 0) > TRC!(22) Then
                    TRC!(22) = TT!(I, 1) / TT!(I, 0)
                    TRC$(22) = TT$(I, 1)
                    TRYR!(22) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 40
            If TT!(I, 0) <> 0 Then
                If (TT!(I, 1) + TT!(I, 4) + (TT!(I, 5) * 2) + (TT!(I, 6) * 3)) / TT!(I, 0) > TRC!(23) Then
                    TRC!(23) = (TT!(I, 1) + TT!(I, 4) + (TT!(I, 5) * 2) + (TT!(I, 6) * 3)) / TT!(I, 0)
                    TRC$(23) = TT$(I, 1)
                    TRYR!(23) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 40
            If TT!(I, 0) <> 0 Then
                If (TT!(I, 1) + TT!(I, 7)) / (TT!(I, 0) + TT!(I, 7)) > TRC!(24) Then
                    TRC!(24) = (TT!(I, 1) + TT!(I, 7)) / (TT!(I, 0) + TT!(I, 7))
                    TRC$(24) = TT$(I, 1)
                    TRYR!(24) = seasonYear
                End If
            End If
        Next I

    Else

        For I = 1 To 40
            For I1 = 0 To 22
                If TT1!(I, I1) > TRC1!(I1) Then
                    TRC1!(I1) = TT1!(I, I1): TRC1$(I1) = TT1$(I, I1): TRYR1!(I1) = seasonYear
                End If
            Next I1

            AAT = AAT - 1
            Locate 9, 40: Print Using "###"; AAT
        Next I

        If finalUpdate$ = "Y" Then

            For I = 1 To 40
                If TT1!(I, 17) <> 0 Then
                    If TT1!(I, 18) / TT1!(I, 17) > TRC1!(23) Then
                        TRC1!(23) = TT1!(I, 18) / TT1!(I, 17)
                        TRC1$(23) = TT1$(I, 0)
                        TRYR1!(23) = seasonYear
                    End If
                End If
            Next I

            'IF TXX = 1 THEN
            For I = 1 To 40
                If TT1!(I, 0) <> 0 Then
                    If (TT1!(I, 3) * 9) / (TT1!(I, 0) / 3) < TRC1!(24) Then
                        TRC1!(24) = (TT1!(I, 3) * 9) / (TT1!(I, 0) / 3)
                        TRC1$(24) = TT1$(I, 0)
                        TRYR1!(24) = seasonYear
                    End If
                End If
            Next I

        Else

            Open diskPaths$(2) + "BASEBALL.REC" For Output As #1
            For I = 1 To 32: Write #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): Next
            For I = 1 To 28: Write #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): Next
            For I = 0 To 24: Write #1, TRC!(I), TRC$(I), TRYR!(I): Next
            For I = 0 To 24: Write #1, TRC1!(I), TRC1$(I), TRYR1!(I): Next
            Close #1

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

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared TP0$(), TP1A$(), TP2A$(), TP3$()
    Shared TRC$(), TRC1$()

    Cls

    If _FileExists(diskPaths$(2) + "BASEBALL.REC") Then
        Open diskPaths$(2) + "BASEBALL.REC" For Input As #1
        For I = 1 To 32: Input #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): Next
        For I = 1 To 28: Input #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): Next
        For I = 0 To 24: Input #1, TRC!(I), TRC$(I), TRYR!(I): Next
        For I = 0 To 24: Input #1, TRC1!(I), TRC1$(I), TRYR1!(I): Next
        Close #1
    End If

    Color 15, 0
    Cls

    Do
        Locate 15, 10
        Color 12: Print "V";: Color 7: Print "iew or ";
        Color 12: Print "P";: Color 7: Print "rint"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

    If UCase$(I$) = "P" Then
        Call PrintRecords(leagueFile$)
    Else
        Color 15, 4: Cls
        Print leagueFile$; "  BATTING RECORDS": Print
        Print "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        Print String$(75, 205)

        For I = 1 To 15
            Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "####"; BRC!(I); Tab(71); BTYR!(I)
        Next

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        Cls
        Print leagueFile$; "  BATTING RECORDS": Print
        Print "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        Print String$(75, 205)

        For I = 16 To 20
            Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "####"; BRC!(I); Tab(71); BTYR!(I)
        Next

        For I = 21 To 23
            Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using ".###"; BRC!(I);: Print Tab(71); Using "####"; BTYR!(I)
        Next

        For I = 24 To 29
            Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(60); Using "###.#"; BRC!(I);: Print Tab(71); Using "####"; BTYR!(I)
        Next

        For I = 30 To 31
            Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "###"; BRC!(I);: Print Tab(71); Using "####"; BTYR!(I)
        Next

        Print TP0$(32); Tab(28); BRC$(32); Tab(43); BRT$(32); Tab(62); Using ".###"; BRC!(32);: Print Tab(71); Using "####"; BTYR!(32)

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        Color 15, 1: Cls
        Print leagueFile$; "  PITCHING RECORDS": Print
        Print "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        Print String$(75, 205)
        Print TP1A$(1); Tab(28); PRC$(1); Tab(43); PRT$(1); Tab(61); Using "###"; Int(PRC!(1) / 3);

        If PRC!(1) - Int(PRC!(1) / 3) * 3 > 0 Then Print ".";: Print Using "#"; PRC!(1) - (Int(PRC!(1) / 3) * 3); Else Print ".0";

        Print Tab(72); Using "####"; PRYR!(1)

        For I = 2 To 15
            Print TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(62); Using "####"; PRC!(I); Tab(72); PRYR!(I)
        Next

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        Cls
        Print leagueFile$; "  PITCHING RECORDS": Print
        Print "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
        Print String$(75, 205)

        For I = 16 To 19
            Print TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(62); Using "####"; PRC!(I); Tab(72); PRYR!(I)
        Next

        Print TP1A$(20); Tab(28); PRC$(24); Tab(43); PRT$(24); Tab(62); Using ".###"; PRC!(24) * 100;: Print Tab(72); Using "####"; PRYR!(24)
        Print TP1A$(21); Tab(28); PRC$(20); Tab(43); PRT$(20); Tab(62); Using "####"; PRC!(20); Tab(72); PRYR!(20)
        Print TP1A$(22); Tab(28); PRC$(25); Tab(43); PRT$(25); Tab(61); Using "##.##"; PRC!(25);: Print Tab(72); Using "####"; PRYR!(25)
        Print TP1A$(23); Tab(28); PRC$(22); Tab(43); PRT$(22); Tab(62); Using "####"; PRC!(22); Tab(72); PRYR!(22)
        Print TP1A$(24); Tab(28); PRC$(23); Tab(43); PRT$(23); Tab(60); Using "###.##"; PRC!(23); Tab(72);
        Print Using "####"; PRYR!(23)

        For I = 25 To 28
            Print TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(61); Using "##.##"; PRC!(I);: Print Tab(72); Using "####"; PRYR!(I)
        Next

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        Color 14, 0: Cls
        Print leagueFile$; "  TEAM RECORDS": Print
        Print "CATEGORY                        TEAM                        RECORD     YEAR"
        Print String$(75, 205)

        For I = 0 To 12
            Print TP2A$(I); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I); Tab(72); TRYR!(I)
        Next

        For I = 15 To 16
            Print TP2A$(I - 2); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I); Tab(72); TRYR!(I)
        Next

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        Cls
        Print leagueFile$; "  TEAM RECORDS": Print
        Print "CATEGORY                        TEAM                        RECORD     YEAR"
        Print String$(75, 205)

        For I = 17 To 19
            Print TP2A$(I - 2); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I); Tab(72); TRYR!(I)
        Next

        For I = 22 To 24
            Print TP2A$(I - 4); Tab(28); TRC$(I); Tab(62); Using ".###"; TRC!(I);: Print Tab(72); Using "####"; TRYR!(I)
        Next

        Print TP3$(0); Tab(28); TRC1$(0); Tab(60); Using "####"; Int(TRC1!(0) / 3);

        If TRC1!(0) - Int(TRC1!(0) / 3) * 3 > 0 Then Print ".";: Print Using "#"; TRC1!(0) - (Int(TRC1!(0) / 3) * 3); Else Print ".0";

        Print Tab(72); Using "####"; TRYR1!(0)

        For I = 1 To 5
            Print TP3$(I); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);: Print Tab(72); Using "####"; TRYR1!(I)
        Next

        For I = 10 To 12
            Print TP3$(I - 4); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);: Print Tab(72); Using "####"; TRYR1!(I)
        Next

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
        Cls
        Print leagueFile$; "  TEAM RECORDS": Print
        Print "CATEGORY                        TEAM                        RECORD     YEAR"
        Print String$(75, 205)

        For I = 13 To 19
            Print TP3$(I - 4); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);
            Print Tab(72); Using "####"; TRYR1!(I)
        Next

        Print TP3$(16); Tab(28); TRC1$(21); Tab(62); Using "####"; TRC1!(21);: Print Tab(72); Using "####"; TRYR1!(21)
        Print TP3$(17); Tab(28); TRC1$(23); Tab(62); Using ".###"; TRC1!(23);: Print Tab(72); Using "####"; TRYR1!(23)
        Print TP3$(18); Tab(28); TRC1$(24); Tab(61); Using "##.##"; TRC1!(24);: Print Tab(72); Using "####"; TRYR1!(24)
        Locate 23: Print "HIT ANY KEY TO CONTINUE"
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

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared TP0$(), TP1A$(), TP2A$(), TP3$()
    Shared TRC$(), TRC1$()

    LPrint leagueFile$; "  BATTING RECORDS": LPrint
    LPrint "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPrint String$(75, 61)

    For I = 1 To 20
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "####"; BRC!(I); Tab(71); BTYR!(I)
    Next

    For I = 21 To 23
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using ".###"; BRC!(I);: LPrint Tab(71); Using "####"; BTYR!(I)
    Next

    For I = 24 To 29
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(60); Using "###.#"; BRC!(I);: LPrint Tab(71); Using "####"; BTYR!(I)
    Next

    For I = 30 To 31
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "###"; BRC!(I);: LPrint Tab(71); Using "####"; BTYR!(I)
    Next

    LPrint TP0$(32); Tab(28); BRC$(32); Tab(43); BRT$(32); Tab(62); Using ".###"; BRC!(32);: LPrint Tab(71); Using "####"; BTYR!(32)
    LPrint Chr$(12)
    LPrint leagueFile$; "  PITCHING RECORDS": LPrint
    LPrint "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPrint String$(75, 61)
    LPrint TP1A$(1); Tab(28); PRC$(1); Tab(43); PRT$(1); Tab(61); Using "###"; Int(PRC!(1) / 3);

    If PRC!(1) - Int(PRC!(1) / 3) * 3 > 0 Then LPrint ".";: LPrint Using "#"; PRC!(1) - (Int(PRC!(1) / 3) * 3); Else LPrint ".0";

    LPrint Tab(72); Using "####"; PRYR!(1)

    For I = 2 To 19
        LPrint TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(62); Using "####"; PRC!(I); Tab(72); PRYR!(I)
    Next

    LPrint TP1A$(20); Tab(28); PRC$(24); Tab(43); PRT$(24); Tab(62); Using ".###"; PRC!(24) * 100;: LPrint Tab(72); Using "####"; PRYR!(24)
    LPrint TP1A$(21); Tab(28); PRC$(20); Tab(43); PRT$(20); Tab(62); Using "####"; PRC!(20); Tab(72); PRYR!(20)
    LPrint TP1A$(22); Tab(28); PRC$(25); Tab(43); PRT$(25); Tab(61); Using "##.##"; PRC!(25);: LPrint Tab(72); Using "####"; PRYR!(25)
    LPrint TP1A$(23); Tab(28); PRC$(22); Tab(43); PRT$(22); Tab(62); Using "####"; PRC!(22); Tab(72); PRYR!(22)
    LPrint TP1A$(24); Tab(28); PRC$(23); Tab(43); PRT$(23); Tab(60); Using "###.##"; PRC!(23); Tab(72);

    Print Using "####"; PRYR!(23)

    For I = 25 To 28
        LPrint TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(61); Using "##.##"; PRC!(I);: LPrint Tab(72); Using "####"; PRYR!(I)
    Next

    LPrint Chr$(12)
    LPrint leagueFile$; "  TEAM RECORDS": LPrint
    LPrint "CATEGORY                        TEAM                        RECORD     YEAR"
    LPrint String$(75, 61)

    For I = 0 To 12
        LPrint TP2A$(I); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I); Tab(72); TRYR!(I)
    Next

    For I = 15 To 19
        LPrint TP2A$(I - 2); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I); Tab(72); TRYR!(I)
    Next

    For I = 22 To 24
        LPrint TP2A$(I - 4); Tab(28); TRC$(I); Tab(62); Using ".###"; TRC!(I);: LPrint Tab(72); Using "####"; TRYR!(I)
    Next

    LPrint TP3$(0); Tab(28); TRC1$(0); Tab(60); Using "####"; Int(TRC1!(0) / 3);

    If TRC1!(0) - Int(TRC1!(0) / 3) * 3 > 0 Then LPrint ".";: LPrint Using "#"; TRC1!(0) - (Int(TRC1!(0) / 3) * 3); Else LPrint ".0";

    LPrint Tab(72); Using "####"; TRYR1!(0)

    For I = 1 To 5
        LPrint TP3$(I); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);: LPrint Tab(72); Using "####"; TRYR1!(I)
    Next

    For I = 10 To 19
        LPrint TP3$(I - 4); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);: LPrint Tab(72); Using "####"; TRYR1!(I)
    Next

    LPrint TP3$(16); Tab(28); TRC1$(21); Tab(62); Using "####"; TRC1!(21);: LPrint Tab(72); Using "####"; TRYR1!(21)
    LPrint TP3$(17); Tab(28); TRC1$(23); Tab(62); Using ".###"; TRC1!(23);: LPrint Tab(72); Using "####"; TRYR1!(23)
    LPrint TP3$(18); Tab(28); TRC1$(24); Tab(61); Using "##.##"; TRC1!(24);: LPrint Tab(72); Using "####"; TRYR1!(24)
    LPrint Chr$(12)

End Sub
