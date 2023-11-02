'----------------------------------------
'        SeasonRecords Subroutine
'----------------------------------------
'This subroutine gives the user a number of 
'menu selections for viewing, updating and
'printing records
Sub SeasonRecords ()

    _Title "Full Count Baseball - Season Records"

    Shared BRC!(), BTYR!()
    Shared PRC!(), PRYR!()
    Shared TRC!(), TRC1!(), TRYR!(), TRYR1!()

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared TRC$(), TRC1$()

    ReDim menuChoices$(1 To 5)

    'If Not _FileExists(diskPaths$(1) + "BASEBALL.REC") Then
    '
	'	MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
    '    BoxWidth = 24
	'	BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
	'	Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
	'	Call CSMsgBox(MsgText$, BoxWidth, Pass)
	'	I$ = GetKeyPress$
	'	Call CSMsgBox("", 0, Pass)
    '
    'Else

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE SEASON RECORDS"
        menuChoices$(2) = "VIEW SEASON RECORDS  "
        menuChoices$(3) = "PRINT SEASON RECORDS "
        menuChoices$(4) = "DELETE SEASON RECORDS"
        menuChoices$(5) = "RETURN TO MAIN MENU  "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            If _FileExists(diskPaths$(2) + "BASEBALL.REC") Then

                Open diskPaths$(2) + "BASEBALL.REC" For Input As #1

                For I = 1 To 32: Input #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): Next
                For I = 1 To 28: Input #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): Next
                For I = 0 To 24: Input #1, TRC!(I), TRC$(I), TRYR!(I): Next
                For I = 0 To 24: Input #1, TRC1!(I), TRC1$(I), TRYR1!(I): Next

                Close #1

            End If

            leagueFile$ = ChooseFile$ (diskPaths$(2) + "*.BAT", "LEAGUE", FileExit%)

            If FileExit% <> 27 And leagueFile$ <> "" Then

                Select Case Selection%

                    Case 1:
                        Call UpdateRecords(leagueFile$)

                    Case 2:
                        Call ViewSeasonRecords(leagueFile$)

                    Case 3:
                        Call PrintSeasonRecords(leagueFile$)

                    Case 4
                        If _FileExists(diskPaths$(1) + "BASEBALL.REC") Then Kill diskPaths$(1) + "BASEBALL.REC"

                End Select

            End If

        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

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
    Shared Wins(), Losses()

    Shared GM2()

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared leaderBL$(), leaderPL$()
    Shared TMB$(), TMP$(), TRC$(), TRC1$()

    Dim TT$(40, 22), TT1$(40, 23)

    AAG = 920
    AAS = 880
    AAT = 80
    TMR = 2360

    JB = 0
    JP = 0
    TS = 0

    Cls
    Input "ENTER SEASON YEAR (1991,1992)"; seasonYear

    Do
        Locate 2
        Input "WOULD YOU LIKE TO POST SEASON LEADERS? (Y/N)"; finalUpdate$
    Loop Until UCase$(finalUpdate$) = "Y" Or UCase$(finalUpdate$) = "N"

    Print
    Print "ANY CHANGE? (Y/N)"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If Not _FileExists(diskPaths$(2) + "BASEBALL.REC") Then

        PRC!(23) = 100
        PRC!(25) = 100
        BRC!(24) = 999
        BRC!(25) = 999
        BRC!(26) = 999
        BRC!(27) = 999
        BRC!(28) = 999
        PRC!(27) = 100
        TRC1!(24) = 100

    Else

        Open diskPaths$(2) + "BASEBALL.REC" For Input As #1

        Print "READING IN CURRENT RECORDS"

        For I = 1 To 32: Input #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I): Next
        For I = 1 To 28: Input #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I): Next
        For I = 0 To 24: Input #1, TRC!(I), TRC$(I), TRYR!(I): Next
        For I = 0 To 24: Input #1, TRC1!(I), TRC1$(I), TRYR1!(I): Next

        Close #1

    End If

    TMR = 3680

    Open diskPaths$(2) + leagueFile$ + ".BAT" For Input As #1

    Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS

    For I = 1 To 920:
        Input #1, leaderBL$(I), TMB$(I), GM1!(I)
        Color 14
		Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR
    Next I

    For I = 1 To 920:
        For I1 = 1 To 22
            Input #1, BK%(I, I1)
        Next I1
        Color 14
		Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR
	Next I

    Close 1

    Open diskPaths$(2) + leagueFile$ + ".PIT" For Input As #1

    Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS

    For I = 1 To 880:
        Input #1, leaderPL$(I), TMP$(I), GM2(I)
        Color 14
		Locate 18, 38
        TMR = TMR - 1
		Print Using "#####"; TMR
    Next I

    For I = 1 To 880:

        For I1 = 1 To 23:
            Input #1, PM%(I, I1)
        Next I1

        Color 14
		Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

	Next I

    Close 1

    Open diskPaths$(2) + leagueFile$ + ".TOT" For Input As #1

    Input #1, JB, JP, D1$, D2$, D3$, D4$, LGF$, TS, TTS

    For I = 1 To 40: Input #1, Wins(I), Losses(I): Next

    For I = 1 To 40:

        For I1 = 0 To 21:
            Input #1, TT!(I, I1), TT$(I, I1)
        Next

        Color 14
		Locate 18, 38
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

    For I = 1 To 40:

        For I1 = 0 To 22:
            Input #1, TT1!(I, I1), TT1$(I, I1)
        Next

        Color 14
        Locate 18, 45
        TMR = TMR - 1: Print Using "#####"; TMR

    Next

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
                    BRC!(21) = BK%(I, 2) / BK%(I, 1)
                    BRC$(21) = leaderBL$(I)
                    BRT$(21) = TMB$(I)
                    BTYR!(21) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 2) <> 0 Then
                If (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1) >= BRC!(22) Then
                    BRC!(22) = (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1)
                    BRC$(22) = leaderBL$(I)
                    BRT$(22) = TMB$(I)
                    BTYR!(22) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 920
            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 2) <> 0 Then
                If (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8)) >= BRC!(23) Then
                    BRC!(23) = (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8))
                    BRC$(23) = leaderBL$(I)
                    BRT$(23) = TMB$(I)
                    BTYR!(23) = seasonYear
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
                PRC!(I1) = PM%(I, I1)
                PRC$(I1) = leaderPL$(I)
                PRT$(I1) = TMP$(I)
                PRYR!(I1) = seasonYear
            End If
        Next I1

        AAS = AAS - 1
        Locate 8, 40: Print Using "###"; AAS
    Next I

    If finalUpdate$ = "Y" Then

        For I = 1 To 880
            If PM%(I, 18) <> 0 And PM%(I, 9) >= 25 Then
                If PM%(I, 19) / PM%(I, 18) <= PRC!(24) Then
                    PRC!(24) = PM%(I, 19) / PM%(I, 18)
                    PRC$(24) = leaderPL$(I)
                    PRT$(24) = TMP$(I)
                    PRYR!(24) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 21) <> 0 Then
                If PM%(I, 21) / PM%(I, 9) < PRC!(25) Then
                    PRC!(25) = PM%(I, 21) / PM%(I, 9)
                    PRC$(25) = leaderPL$(I)
                    PRT$(25) = TMP$(I)
                    PRYR!(25) = seasonYear
                End If
            End If
        Next I

        'IF TXX = 1 THEN 2785
        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 4) * 9) / (PM%(I, 1) / 3) > PRC!(23) Then
                    PRC!(23) = (PM%(I, 4) * 9) / (PM%(I, 1) / 3)
                    PRC$(23) = leaderPL$(I)
                    PRT$(23) = TMP$(I)
                    PRYR!(23) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 5) * 9) / (PM%(I, 1) / 3) > PRC!(25) Then
                    PRC!(25) = (PM%(I, 5) * 9) / (PM%(I, 1) / 3)
                    PRC$(25) = leaderPL$(I)
                    PRT$(25) = TMP$(I)
                    PRYR!(25) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 6) * 9) / (PM%(I, 1) / 3) < PRC!(26) Then
                    PRC!(26) = (PM%(I, 6) * 9) / (PM%(I, 1) / 3)
                    PRC$(26) = leaderPL$(I)
                    PRT$(26) = TMP$(I)
                    PRYR!(26) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 2) * 9) / (PM%(I, 1) / 3) > PRC!(27) Then
                    PRC!(27) = (PM%(I, 2) * 9) / (PM%(I, 1) / 3)
                    PRC$(27) = leaderPL$(I)
                    PRT$(27) = TMP$(I)
                    PRYR!(27) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 14) * 9) / (PM%(I, 1) / 3) < PRC!(28) Then
                    PRC!(28) = (PM%(I, 14) * 9) / (PM%(I, 1) / 3)
                    PRC$(28) = leaderPL$(I)
                    PRT$(28) = TMP$(I)
                    PRYR!(28) = seasonYear
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
                    TRC1!(I1) = TT1!(I, I1)
                    TRC1$(I1) = TT1$(I, I1)
                    TRYR1!(I1) = seasonYear
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

            For I = 1 To 32:
                Write #1, BRC!(I), BRC$(I), BRT$(I), BTYR!(I)
            Next

            For I = 1 To 28:
                Write #1, PRC!(I), PRC$(I), PRT$(I), PRYR!(I)
            Next

            For I = 0 To 24:
                Write #1, TRC!(I), TRC$(I), TRYR!(I)
            Next

            For I = 0 To 24:
                Write #1, TRC1!(I), TRC1$(I), TRYR1!(I)
            Next

            Close #1

        End If

    End If 'Done checking for final update

End Sub


'----------------------------------------
'       ViewSeasonRecords Subroutine
'----------------------------------------
'...explanation...
Sub ViewSeasonRecords (leagueFile$)

    Shared BRC!(), BTYR!(), PRC!(), PRYR!()
    Shared TRC!(), TRC1!(), TRYR!(), TRYR1!()

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared TP0$(), TP1A$(), TP2A$(), TP3$()
    Shared TRC$(), TRC1$()

    Color 0, 0

    Cls

    J = 20

    Locate 1, 1
    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";

    For II = 1 To J:
        Locate 1 + II, 1
        Color 15, 4
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Locate 2 + J, 1
    Color 15, 4
    Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 1
    Print leagueFile$
    Locate 2, 24
    Print "******* BATTING RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    For I = 1 To 15
        Locate , 4
        Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "####"; BRC!(I);: Print Tab(70); Using "####"; BTYR!(I)
    Next

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 15
    Locate 1
    Print leagueFile$
    Locate 2, 24
    Print "******* BATTING RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    For I = 16 To 20
        Locate , 4
        Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "####"; BRC!(I);: Print Tab(70); Using "####"; BTYR!(I)
    Next

    For I = 21 To 23
        Locate , 4
        Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using ".###"; BRC!(I);: Print Tab(70); Using "####"; BTYR!(I)
    Next

    For I = 24 To 29
        Locate , 4
        Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(60); Using "###.#"; BRC!(I);: Print Tab(70); Using "####"; BTYR!(I)
    Next

    For I = 30 To 31
        Locate , 4
        Print TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "###"; BRC!(I);: Print Tab(70); Using "####"; BTYR!(I)
    Next

    Locate , 4
    Print TP0$(32); Tab(28); BRC$(32); Tab(43); BRT$(32); Tab(62); Using ".###"; BRC!(32);: Print Tab(70); Using "####"; BTYR!(32)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1
    Color 15, 1
    Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";

    For II = 1 To J:
        Locate 1 + II, 1
        Color 15, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 1: Print String$(2, 177)
    Next

    Locate 2 + J, 1
    Color 15, 1
    Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 1: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 1
    Print leagueFile$
    Locate 2, 24
    Print "******* PITCHING RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    Locate , 4
    Print TP1A$(1); Tab(28); PRC$(1); Tab(43); PRT$(1); Tab(61); Using "###"; Int(PRC!(1) / 3);
    If PRC!(1) - Int(PRC!(1) / 3) * 3 > 0 Then Print ".";: Print Using "#"; PRC!(1) - (Int(PRC!(1) / 3) * 3); Else Print ".0";
    Print Tab(70); Using "####"; PRYR!(1)

    For I = 2 To 15
        Locate , 4
        Print TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(62); Using "####"; PRC!(I); Tab(70); PRYR!(I)
    Next

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Locate 2, 24
    Print "******* PITCHING RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    For I = 16 To 19
        Locate , 4
        Print TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(62); Using "####"; PRC!(I);: Print Tab(70); Using "####"; PRYR!(I)
    Next

    Locate , 4
    Print TP1A$(20); Tab(28); PRC$(24); Tab(43); PRT$(24); Tab(62); Using ".###"; PRC!(24) * 100;: Print Tab(70); Using "####"; PRYR!(24)
    Locate , 4
    Print TP1A$(21); Tab(28); PRC$(20); Tab(43); PRT$(20); Tab(62); Using "####"; PRC!(20);: Print Tab(70); Using "####"; PRYR!(20)
    Locate , 4
    Print TP1A$(22); Tab(28); PRC$(25); Tab(43); PRT$(25); Tab(61); Using "##.##"; PRC!(25);: Print Tab(70); Using "####"; PRYR!(25)
    Locate , 4
    Print TP1A$(23); Tab(28); PRC$(22); Tab(43); PRT$(22); Tab(62); Using "####"; PRC!(22);: Print Tab(70); Using "####"; PRYR!(22)
    Locate , 4
    Print TP1A$(24); Tab(28); PRC$(23); Tab(43); PRT$(23); Tab(60); Using "###.##"; PRC!(23);: Print Tab(70); Using "####"; PRYR!(23)

    For I = 25 To 28
        Locate , 4
        Print TP1A$(I); Tab(28); PRC$(I); Tab(43); PRT$(I); Tab(61); Using "##.##"; PRC!(I);: Print Tab(70); Using "####"; PRYR!(I)
    Next

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1
    Color 14, 0
    Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";

    For II = 1 To J:
        Locate 1 + II, 1
        Color 14, 0
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 0: Print String$(2, 177)
    Next

    Locate 2 + J, 1
    Color 14, 0
    Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 0: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 14
    Locate 1
    Print leagueFile$
    Locate 2, 26
    Print "******* TEAM RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    For I = 0 To 12
        Locate , 4
        Print TP2A$(I); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I);: Print Tab(70); Using "####"; TRYR!(I)
    Next

    For I = 15 To 16
        Locate , 4
        Print TP2A$(I - 2); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I);: Print Tab(70); Using "####"; TRYR!(I)
    Next

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    Color 14
    Locate 1
    Print leagueFile$
    Locate 2, 26
    Print "******* TEAM RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    For I = 17 To 19
        Locate , 4
        Print TP2A$(I - 2); Tab(28); TRC$(I); Tab(62); Using "####"; TRC!(I);: Print Tab(70); Using "####"; TRYR!(I)
    Next

    For I = 22 To 24
        Locate , 4
        Print TP2A$(I - 4); Tab(28); TRC$(I); Tab(62); Using ".###"; TRC!(I);: Print Tab(70); Using "####"; TRYR!(I)
    Next

    Locate , 4
    Print TP3$(0); Tab(28); TRC1$(0); Tab(60); Using "####"; Int(TRC1!(0) / 3);
    If TRC1!(0) - Int(TRC1!(0) / 3) * 3 > 0 Then Print ".";: Print Using "#"; TRC1!(0) - (Int(TRC1!(0) / 3) * 3); Else Print ".0";
    Print Tab(70); Using "####"; TRYR1!(0)

    For I = 1 To 5
        Locate , 4
        Print TP3$(I); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);: Print Tab(70); Using "####"; TRYR1!(I)
    Next

    For I = 10 To 12
        Locate , 4
        Print TP3$(I - 4); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);: Print Tab(70); Using "####"; TRYR1!(I)
    Next

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 14
    Locate 1
    Print leagueFile$
    Locate 2, 26
    Print "******* TEAM RECORDS *******"
    Print
    Locate , 4
    Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

    Color 7

    For I = 13 To 19
        Locate , 4
        Print TP3$(I - 4); Tab(28); TRC1$(I); Tab(62); Using "####"; TRC1!(I);
        Print Tab(70); Using "####"; TRYR1!(I)
    Next

    Locate , 4
    Print TP3$(16); Tab(28); TRC1$(21); Tab(62); Using "####"; TRC1!(21);: Print Tab(70); Using "####"; TRYR1!(21)
    Locate , 4
    Print TP3$(17); Tab(28); TRC1$(23); Tab(62); Using ".###"; TRC1!(23);: Print Tab(70); Using "####"; TRYR1!(23)
    Locate , 4
    Print TP3$(18); Tab(28); TRC1$(24); Tab(61); Using "##.##"; TRC1!(24);: Print Tab(70); Using "####"; TRYR1!(24)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       PrintSeasonRecords Subroutine
'----------------------------------------
'...explanation...
Sub PrintSeasonRecords (leagueFile$)

    Shared BRC!(), BTYR!(), PRC!(), PRYR!()
    Shared TRC!(), TRC1!(), TRYR!(), TRYR1!()

    Shared BRC$(), BRT$(), PRC$(), PRT$()
    Shared TP0$(), TP1A$(), TP2A$(), TP3$()
    Shared TRC$(), TRC1$()

    LPrint leagueFile$; "  BATTING RECORDS"
    LPrint
    LPrint "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPrint String$(75, 61)

    For I = 1 To 20
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "####"; BRC!(I); Tab(66); BTYR!(I)
    Next

    For I = 21 To 23
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using ".###"; BRC!(I);: LPrint Tab(66); Using "####"; BTYR!(I)
    Next

    For I = 24 To 29
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(60); Using "###.#"; BRC!(I);: LPrint Tab(66); Using "####"; BTYR!(I)
    Next

    For I = 30 To 31
        LPrint TP0$(I); Tab(28); BRC$(I); Tab(43); BRT$(I); Tab(62); Using "###"; BRC!(I);: LPrint Tab(66); Using "####"; BTYR!(I)
    Next

    LPrint TP0$(32); Tab(28); BRC$(32); Tab(43); BRT$(32); Tab(62); Using ".###"; BRC!(32);: LPrint Tab(66); Using "####"; BTYR!(32)
    LPrint Chr$(12)

    LPrint leagueFile$; "  PITCHING RECORDS"
    LPrint
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

    LPrint leagueFile$; "  TEAM RECORDS"
    LPrint
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
