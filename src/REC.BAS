'----------------------------------------
'        SeasonRecords Subroutine
'----------------------------------------
'This subroutine gives the user a number of
'menu selections for viewing, updating and
'printing records
Sub SeasonRecords ()

    _Title "Full Count Baseball - Season Records"

    Shared batRecVal!(), batRecYr!()
    Shared pitRecVal!(), pitRecYr!()
    Shared teamRecAVal!(), teamRecBVal!(), teamRecAYr!(), teamRecBYr!()

    Shared batRecPlyr$(), batRecTeam$(), pitRecPlyr$(), pitRecTeam$()
    Shared teamRecATeam$(), teamRecBTeam$()

    ReDim menuChoices$(1 To 5)

    'If Not _FileExists(diskPaths$(1) + "BASEBALL.REC") Then
    '
    '   MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
    '    BoxWidth = 24
    '   BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
    '   Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
    '   Call CSMsgBox(MsgText$, BoxWidth, Pass)
    '   I$ = GetKeyPress$
    '   Call CSMsgBox("", 0, Pass)
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

                For I = 1 To 32: Input #1, batRecVal!(I), batRecPlyr$(I), batRecTeam$(I), batRecYr!(I): Next
                For I = 1 To 28: Input #1, pitRecVal!(I), pitRecPlyr$(I), pitRecTeam$(I), pitRecYr!(I): Next
                For I = 0 To 24: Input #1, teamRecAVal!(I), teamRecATeam$(I), teamRecAYr!(I): Next
                For I = 0 To 24: Input #1, teamRecBVal!(I), teamRecBTeam$(I), teamRecBYr!(I): Next

                Close #1

            End If

            leagueFile$ = ChooseFile$(diskPaths$(2) + "*.BAT", "LEAGUE", FileExit%)

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
    Shared batRecVal!(), batRecYr!(), GM1!()
    Shared pitRecVal!(), pitRecYr!()
    Shared teamRecAVal!(), teamRecBVal!(), teamRecAYr!(), teamRecBYr!()
    Shared TT!(), TT1!()
    Shared Wins(), Losses()

    Shared GM2()

    Shared batRecPlyr$(), batRecTeam$(), pitRecPlyr$(), pitRecTeam$()
    Shared leaderBL$(), leaderPL$()
    Shared TMB$(), TMP$(), teamRecATeam$(), teamRecBTeam$()

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

        pitRecVal!(23) = 100
        pitRecVal!(25) = 100
        batRecVal!(24) = 999
        batRecVal!(25) = 999
        batRecVal!(26) = 999
        batRecVal!(27) = 999
        batRecVal!(28) = 999
        pitRecVal!(27) = 100
        teamRecBVal!(24) = 100

    Else

        Open diskPaths$(2) + "BASEBALL.REC" For Input As #1

        Print "READING IN CURRENT RECORDS"

        For I = 1 To 32:
            Input #1, batRecVal!(I), batRecPlyr$(I), batRecTeam$(I), batRecYr!(I)
        Next

        For I = 1 To 28:
            Input #1, pitRecVal!(I), pitRecPlyr$(I), pitRecTeam$(I), pitRecYr!(I)
        Next

        For I = 0 To 24:
            Input #1, teamRecAVal!(I), teamRecATeam$(I), teamRecAYr!(I)
        Next

        For I = 0 To 24:
            Input #1, teamRecBVal!(I), teamRecBTeam$(I), teamRecBYr!(I)
        Next

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

            If BK%(I, I1) >= batRecVal!(I1) Then
                batRecVal!(I1) = BK%(I, I1)
                batRecPlyr$(I1) = leaderBL$(I)
                batRecTeam$(I1) = TMB$(I)
                batRecYr!(I1) = seasonYear
            End If

        Next I1

        AAG = AAG - 1
        Locate 7, 40: Print Using "###"; AAG

    Next I

    If finalUpdate$ = "Y" Then

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 1) <> 0 Then
                If BK%(I, 2) / BK%(I, 1) >= batRecVal!(21) Then
                    batRecVal!(21) = BK%(I, 2) / BK%(I, 1)
                    batRecPlyr$(21) = leaderBL$(I)
                    batRecTeam$(21) = TMB$(I)
                    batRecYr!(21) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 2) <> 0 Then
                If (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1) >= batRecVal!(22) Then
                    batRecVal!(22) = (BK%(I, 2) + BK%(I, 5) + BK%(I, 6) * 2 + BK%(I, 7) * 3) / BK%(I, 1)
                    batRecPlyr$(22) = leaderBL$(I)
                    batRecTeam$(22) = TMB$(I)
                    batRecYr!(22) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 2) <> 0 Then
                If (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8)) >= batRecVal!(23) Then
                    batRecVal!(23) = (BK%(I, 2) + BK%(I, 8)) / (BK%(I, 1) + BK%(I, 8))
                    batRecPlyr$(23) = leaderBL$(I)
                    batRecTeam$(23) = TMB$(I)
                    batRecYr!(23) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 7) <> 0 Then
                If BK%(I, 1) / BK%(I, 7) <= batRecVal!(24) Then
                    batRecVal!(24) = BK%(I, 1) / BK%(I, 7)
                    batRecPlyr$(24) = leaderBL$(I)
                    batRecTeam$(24) = TMB$(I)
                    batRecYr!(24) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 9) <> 0 Then
                If BK%(I, 1) / BK%(I, 9) <= batRecVal!(25) Then
                    batRecVal!(25) = BK%(I, 1) / BK%(I, 9)
                    batRecPlyr$(25) = leaderBL$(I)
                    batRecTeam$(25) = TMB$(I)
                    batRecYr!(25) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 8) <> 0 Then
                If BK%(I, 1) / BK%(I, 8) > batRecVal!(26) Then
                    batRecVal!(26) = BK%(I, 1) / BK%(I, 8)
                    batRecPlyr$(26) = leaderBL$(I)
                    batRecTeam$(26) = TMB$(I)
                    batRecYr!(26) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 4) <> 0 Then
                If BK%(I, 1) / BK%(I, 4) <= batRecVal!(27) Then
                    batRecVal!(27) = BK%(I, 1) / BK%(I, 4)
                    batRecPlyr$(27) = leaderBL$(I)
                    batRecTeam$(27) = TMB$(I)
                    batRecYr!(27) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 3) <> 0 Then
                If BK%(I, 1) / BK%(I, 3) <= batRecVal!(28) Then
                    batRecVal!(28) = BK%(I, 1) / BK%(I, 3)
                    batRecPlyr$(28) = leaderBL$(I)
                    batRecTeam$(28) = TMB$(I)
                    batRecYr!(28) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 3.1 And BK%(I, 16) <> 0 Then
                If BK%(I, 1) / BK%(I, 16) >= batRecVal!(29) Then
                    batRecVal!(29) = BK%(I, 1) / BK%(I, 16)
                    batRecPlyr$(29) = leaderBL$(I)
                    batRecTeam$(29) = TMB$(I)
                    batRecYr!(29) = seasonYear
                End If
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 21) + BK%(I, 22) >= batRecVal!(30) Then
                batRecVal!(30) = BK%(I, 21) + BK%(I, 22)
                batRecPlyr$(30) = leaderBL$(I)
                batRecTeam$(30) = TMB$(I)
                batRecYr!(30) = seasonYear
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 22) >= batRecVal!(31) Then
                batRecVal!(31) = BK%(I, 22)
                batRecPlyr$(31) = leaderBL$(I)
                batRecTeam$(31) = TMB$(I)
                batRecYr!(31) = seasonYear
            End If

        Next I

        For I = 1 To 920

            If BK%(I, 21) + BK%(I, 22) <> 0 And BK%(I, 1) + BK%(I, 8) >= GM1!(I) * 2.1 Then
                If BK%(I, 22) / (BK%(I, 21) + BK%(I, 22)) >= batRecVal!(32) Then
                    batRecVal!(32) = BK%(I, 22) / (BK%(I, 21) + BK%(I, 22))
                    batRecPlyr$(32) = leaderBL$(I)
                    batRecTeam$(32) = TMB$(I)
                    batRecYr!(32) = seasonYear
                End If
            End If

        Next I

    End If

    Locate 8: Print "CALCULATING PITCHER RECORDS . . . ."

    For I = 1 To 880
        For I1 = 1 To 23
            If PM%(I, I1) >= pitRecVal!(I1) Then
                pitRecVal!(I1) = PM%(I, I1)
                pitRecPlyr$(I1) = leaderPL$(I)
                pitRecTeam$(I1) = TMP$(I)
                pitRecYr!(I1) = seasonYear
            End If
        Next I1

        AAS = AAS - 1
        Locate 8, 40: Print Using "###"; AAS
    Next I

    If finalUpdate$ = "Y" Then

        For I = 1 To 880
            If PM%(I, 18) <> 0 And PM%(I, 9) >= 25 Then
                If PM%(I, 19) / PM%(I, 18) <= pitRecVal!(24) Then
                    pitRecVal!(24) = PM%(I, 19) / PM%(I, 18)
                    pitRecPlyr$(24) = leaderPL$(I)
                    pitRecTeam$(24) = TMP$(I)
                    pitRecYr!(24) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 21) <> 0 Then
                If PM%(I, 21) / PM%(I, 9) < pitRecVal!(25) Then
                    pitRecVal!(25) = PM%(I, 21) / PM%(I, 9)
                    pitRecPlyr$(25) = leaderPL$(I)
                    pitRecTeam$(25) = TMP$(I)
                    pitRecYr!(25) = seasonYear
                End If
            End If
        Next I

        'IF TXX = 1 THEN 2785
        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 4) * 9) / (PM%(I, 1) / 3) > pitRecVal!(23) Then
                    pitRecVal!(23) = (PM%(I, 4) * 9) / (PM%(I, 1) / 3)
                    pitRecPlyr$(23) = leaderPL$(I)
                    pitRecTeam$(23) = TMP$(I)
                    pitRecYr!(23) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 5) * 9) / (PM%(I, 1) / 3) > pitRecVal!(25) Then
                    pitRecVal!(25) = (PM%(I, 5) * 9) / (PM%(I, 1) / 3)
                    pitRecPlyr$(25) = leaderPL$(I)
                    pitRecTeam$(25) = TMP$(I)
                    pitRecYr!(25) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 6) * 9) / (PM%(I, 1) / 3) < pitRecVal!(26) Then
                    pitRecVal!(26) = (PM%(I, 6) * 9) / (PM%(I, 1) / 3)
                    pitRecPlyr$(26) = leaderPL$(I)
                    pitRecTeam$(26) = TMP$(I)
                    pitRecYr!(26) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 2) * 9) / (PM%(I, 1) / 3) > pitRecVal!(27) Then
                    pitRecVal!(27) = (PM%(I, 2) * 9) / (PM%(I, 1) / 3)
                    pitRecPlyr$(27) = leaderPL$(I)
                    pitRecTeam$(27) = TMP$(I)
                    pitRecYr!(27) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 880
            If PM%(I, 1) / 3 >= GM2(I) And PM%(I, 1) <> 0 Then
                If (PM%(I, 14) * 9) / (PM%(I, 1) / 3) < pitRecVal!(28) Then
                    pitRecVal!(28) = (PM%(I, 14) * 9) / (PM%(I, 1) / 3)
                    pitRecPlyr$(28) = leaderPL$(I)
                    pitRecTeam$(28) = TMP$(I)
                    pitRecYr!(28) = seasonYear
                End If
            End If
        Next I

    End If

    Locate 9: Print "CALCULATING TEAM RECORDS . . . ."

    For I = 1 To 40
        For I1 = 0 To 21
            If TT!(I, I1) > teamRecAVal!(I1) Then
                teamRecAVal!(I1) = TT!(I, I1)
                teamRecATeam$(I1) = TT$(I, I1)
                teamRecAYr!(I1) = seasonYear
            End If
        Next I1

        AAT = AAT - 1
        Locate 9, 40: Print Using "###"; AAT
    Next I

    If finalUpdate$ = "Y" Then
        For I = 1 To 40
            If TT!(I, 0) <> 0 Then
                If TT!(I, 1) / TT!(I, 0) > teamRecAVal!(22) Then
                    teamRecAVal!(22) = TT!(I, 1) / TT!(I, 0)
                    teamRecATeam$(22) = TT$(I, 1)
                    teamRecAYr!(22) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 40
            If TT!(I, 0) <> 0 Then
                If (TT!(I, 1) + TT!(I, 4) + (TT!(I, 5) * 2) + (TT!(I, 6) * 3)) / TT!(I, 0) > teamRecAVal!(23) Then
                    teamRecAVal!(23) = (TT!(I, 1) + TT!(I, 4) + (TT!(I, 5) * 2) + (TT!(I, 6) * 3)) / TT!(I, 0)
                    teamRecATeam$(23) = TT$(I, 1)
                    teamRecAYr!(23) = seasonYear
                End If
            End If
        Next I

        For I = 1 To 40
            If TT!(I, 0) <> 0 Then
                If (TT!(I, 1) + TT!(I, 7)) / (TT!(I, 0) + TT!(I, 7)) > teamRecAVal!(24) Then
                    teamRecAVal!(24) = (TT!(I, 1) + TT!(I, 7)) / (TT!(I, 0) + TT!(I, 7))
                    teamRecATeam$(24) = TT$(I, 1)
                    teamRecAYr!(24) = seasonYear
                End If
            End If
        Next I

    Else

        For I = 1 To 40
            For I1 = 0 To 22
                If TT1!(I, I1) > teamRecBVal!(I1) Then
                    teamRecBVal!(I1) = TT1!(I, I1)
                    teamRecBTeam$(I1) = TT1$(I, I1)
                    teamRecBYr!(I1) = seasonYear
                End If
            Next I1

            AAT = AAT - 1
            Locate 9, 40: Print Using "###"; AAT
        Next I

        If finalUpdate$ = "Y" Then

            For I = 1 To 40
                If TT1!(I, 17) <> 0 Then
                    If TT1!(I, 18) / TT1!(I, 17) > teamRecBVal!(23) Then
                        teamRecBVal!(23) = TT1!(I, 18) / TT1!(I, 17)
                        teamRecBTeam$(23) = TT1$(I, 0)
                        teamRecBYr!(23) = seasonYear
                    End If
                End If
            Next I

            'IF TXX = 1 THEN
            For I = 1 To 40
                If TT1!(I, 0) <> 0 Then
                    If (TT1!(I, 3) * 9) / (TT1!(I, 0) / 3) < teamRecBVal!(24) Then
                        teamRecBVal!(24) = (TT1!(I, 3) * 9) / (TT1!(I, 0) / 3)
                        teamRecBTeam$(24) = TT1$(I, 0)
                        teamRecBYr!(24) = seasonYear
                    End If
                End If
            Next I

        Else

            Open diskPaths$(2) + "BASEBALL.REC" For Output As #1

            For I = 1 To 32:
                Write #1, batRecVal!(I), batRecPlyr$(I), batRecTeam$(I), batRecYr!(I)
            Next

            For I = 1 To 28:
                Write #1, pitRecVal!(I), pitRecPlyr$(I), pitRecTeam$(I), pitRecYr!(I)
            Next

            For I = 0 To 24:
                Write #1, teamRecAVal!(I), teamRecATeam$(I), teamRecAYr!(I)
            Next

            For I = 0 To 24:
                Write #1, teamRecBVal!(I), teamRecBTeam$(I), teamRecBYr!(I)
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

    Shared batRecVal!(), batRecYr!(), pitRecVal!(), pitRecYr!()
    Shared teamRecAVal!(), teamRecBVal!(), teamRecAYr!(), teamRecBYr!()

    Shared batRecPlyr$(), batRecTeam$(), pitRecPlyr$(), pitRecTeam$()
    Shared batRecCategory$(), pitRecCategory$(), teamRecACategory$(), teamRecBCategory$()
    Shared teamRecATeam$(), teamRecBTeam$()

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
        Print batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "####"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
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
        Print batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "####"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
    Next

    For I = 21 To 23
        Locate , 4
        Print batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using ".###"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
    Next

    For I = 24 To 29
        Locate , 4
        Print batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(60); Using "###.#"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
    Next

    For I = 30 To 31
        Locate , 4
        Print batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "###"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
    Next

    Locate , 4
    Print batRecCategory$(32); Tab(28); batRecPlyr$(32); Tab(43); batRecTeam$(32); Tab(62); Using ".###"; batRecVal!(32);: Print Tab(70); Using "####"; batRecYr!(32)

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
    Print pitRecCategory$(1); Tab(28); pitRecPlyr$(1); Tab(43); pitRecTeam$(1); Tab(61); Using "###"; Int(pitRecVal!(1) / 3);
    If pitRecVal!(1) - Int(pitRecVal!(1) / 3) * 3 > 0 Then Print ".";: Print Using "#"; pitRecVal!(1) - (Int(pitRecVal!(1) / 3) * 3); Else Print ".0";
    Print Tab(70); Using "####"; pitRecYr!(1)

    For I = 2 To 15
        Locate , 4
        Print pitRecCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(62); Using "####"; pitRecVal!(I); Tab(70); pitRecYr!(I)
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
        Print pitRecCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(62); Using "####"; pitRecVal!(I);: Print Tab(70); Using "####"; pitRecYr!(I)
    Next

    Locate , 4
    Print pitRecCategory$(20); Tab(28); pitRecPlyr$(24); Tab(43); pitRecTeam$(24); Tab(62); Using ".###"; pitRecVal!(24) * 100;: Print Tab(70); Using "####"; pitRecYr!(24)
    Locate , 4
    Print pitRecCategory$(21); Tab(28); pitRecPlyr$(20); Tab(43); pitRecTeam$(20); Tab(62); Using "####"; pitRecVal!(20);: Print Tab(70); Using "####"; pitRecYr!(20)
    Locate , 4
    Print pitRecCategory$(22); Tab(28); pitRecPlyr$(25); Tab(43); pitRecTeam$(25); Tab(61); Using "##.##"; pitRecVal!(25);: Print Tab(70); Using "####"; pitRecYr!(25)
    Locate , 4
    Print pitRecCategory$(23); Tab(28); pitRecPlyr$(22); Tab(43); pitRecTeam$(22); Tab(62); Using "####"; pitRecVal!(22);: Print Tab(70); Using "####"; pitRecYr!(22)
    Locate , 4
    Print pitRecCategory$(24); Tab(28); pitRecPlyr$(23); Tab(43); pitRecTeam$(23); Tab(60); Using "###.##"; pitRecVal!(23);: Print Tab(70); Using "####"; pitRecYr!(23)

    For I = 25 To 28
        Locate , 4
        Print pitRecCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(61); Using "##.##"; pitRecVal!(I);: Print Tab(70); Using "####"; pitRecYr!(I)
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
        Print teamRecACategory$(I); Tab(28); teamRecATeam$(I); Tab(62); Using "####"; teamRecAVal!(I);: Print Tab(70); Using "####"; teamRecAYr!(I)
    Next

    For I = 15 To 16
        Locate , 4
        Print teamRecACategory$(I - 2); Tab(28); teamRecATeam$(I); Tab(62); Using "####"; teamRecAVal!(I);: Print Tab(70); Using "####"; teamRecAYr!(I)
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
        Print teamRecACategory$(I - 2); Tab(28); teamRecATeam$(I); Tab(62); Using "####"; teamRecAVal!(I);: Print Tab(70); Using "####"; teamRecAYr!(I)
    Next

    For I = 22 To 24
        Locate , 4
        Print teamRecACategory$(I - 4); Tab(28); teamRecATeam$(I); Tab(62); Using ".###"; teamRecAVal!(I);: Print Tab(70); Using "####"; teamRecAYr!(I)
    Next

    Locate , 4
    Print teamRecBCategory$(0); Tab(28); teamRecBTeam$(0); Tab(60); Using "####"; Int(teamRecBVal!(0) / 3);
    If teamRecBVal!(0) - Int(teamRecBVal!(0) / 3) * 3 > 0 Then Print ".";: Print Using "#"; teamRecBVal!(0) - (Int(teamRecBVal!(0) / 3) * 3); Else Print ".0";
    Print Tab(70); Using "####"; teamRecBYr!(0)

    For I = 1 To 5
        Locate , 4
        Print teamRecBCategory$(I); Tab(28); teamRecBTeam$(I); Tab(62); Using "####"; teamRecBVal!(I);: Print Tab(70); Using "####"; teamRecBYr!(I)
    Next

    For I = 10 To 12
        Locate , 4
        Print teamRecBCategory$(I - 4); Tab(28); teamRecBTeam$(I); Tab(62); Using "####"; teamRecBVal!(I);: Print Tab(70); Using "####"; teamRecBYr!(I)
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
        Print teamRecBCategory$(I - 4); Tab(28); teamRecBTeam$(I); Tab(62); Using "####"; teamRecBVal!(I);
        Print Tab(70); Using "####"; teamRecBYr!(I)
    Next

    Locate , 4
    Print teamRecBCategory$(16); Tab(28); teamRecBTeam$(21); Tab(62); Using "####"; teamRecBVal!(21);: Print Tab(70); Using "####"; teamRecBYr!(21)
    Locate , 4
    Print teamRecBCategory$(17); Tab(28); teamRecBTeam$(23); Tab(62); Using ".###"; teamRecBVal!(23);: Print Tab(70); Using "####"; teamRecBYr!(23)
    Locate , 4
    Print teamRecBCategory$(18); Tab(28); teamRecBTeam$(24); Tab(61); Using "##.##"; teamRecBVal!(24);: Print Tab(70); Using "####"; teamRecBYr!(24)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       PrintSeasonRecords Subroutine
'----------------------------------------
'...explanation...
Sub PrintSeasonRecords (leagueFile$)

    Shared batRecVal!(), batRecYr!(), pitRecVal!(), pitRecYr!()
    Shared teamRecAVal!(), teamRecBVal!(), teamRecAYr!(), teamRecBYr!()

    Shared batRecPlyr$(), batRecTeam$(), pitRecPlyr$(), pitRecTeam$()
    Shared batRecCategory$(), pitRecCategory$(), teamRecACategory$(), teamRecBCategory$()
    Shared teamRecATeam$(), teamRecBTeam$()

    LPrint leagueFile$; "  BATTING RECORDS"
    LPrint
    LPrint "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPrint String$(75, 61)

    For I = 1 To 20
        LPrint batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "####"; batRecVal!(I); Tab(66); batRecYr!(I)
    Next

    For I = 21 To 23
        LPrint batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using ".###"; batRecVal!(I);: LPrint Tab(66); Using "####"; batRecYr!(I)
    Next

    For I = 24 To 29
        LPrint batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(60); Using "###.#"; batRecVal!(I);: LPrint Tab(66); Using "####"; batRecYr!(I)
    Next

    For I = 30 To 31
        LPrint batRecCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "###"; batRecVal!(I);: LPrint Tab(66); Using "####"; batRecYr!(I)
    Next

    LPrint batRecCategory$(32); Tab(28); batRecPlyr$(32); Tab(43); batRecTeam$(32); Tab(62); Using ".###"; batRecVal!(32);: LPrint Tab(66); Using "####"; batRecYr!(32)
    LPrint Chr$(12)

    LPrint leagueFile$; "  PITCHING RECORDS"
    LPrint
    LPrint "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"
    LPrint String$(75, 61)
    LPrint pitRecCategory$(1); Tab(28); pitRecPlyr$(1); Tab(43); pitRecTeam$(1); Tab(61); Using "###"; Int(pitRecVal!(1) / 3);

    If pitRecVal!(1) - Int(pitRecVal!(1) / 3) * 3 > 0 Then LPrint ".";: LPrint Using "#"; pitRecVal!(1) - (Int(pitRecVal!(1) / 3) * 3); Else LPrint ".0";

    LPrint Tab(72); Using "####"; pitRecYr!(1)

    For I = 2 To 19
        LPrint pitRecCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(62); Using "####"; pitRecVal!(I); Tab(72); pitRecYr!(I)
    Next

    LPrint pitRecCategory$(20); Tab(28); pitRecPlyr$(24); Tab(43); pitRecTeam$(24); Tab(62); Using ".###"; pitRecVal!(24) * 100;: LPrint Tab(72); Using "####"; pitRecYr!(24)
    LPrint pitRecCategory$(21); Tab(28); pitRecPlyr$(20); Tab(43); pitRecTeam$(20); Tab(62); Using "####"; pitRecVal!(20); Tab(72); pitRecYr!(20)
    LPrint pitRecCategory$(22); Tab(28); pitRecPlyr$(25); Tab(43); pitRecTeam$(25); Tab(61); Using "##.##"; pitRecVal!(25);: LPrint Tab(72); Using "####"; pitRecYr!(25)
    LPrint pitRecCategory$(23); Tab(28); pitRecPlyr$(22); Tab(43); pitRecTeam$(22); Tab(62); Using "####"; pitRecVal!(22); Tab(72); pitRecYr!(22)
    LPrint pitRecCategory$(24); Tab(28); pitRecPlyr$(23); Tab(43); pitRecTeam$(23); Tab(60); Using "###.##"; pitRecVal!(23); Tab(72);

    Print Using "####"; pitRecYr!(23)

    For I = 25 To 28
        LPrint pitRecCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(61); Using "##.##"; pitRecVal!(I);: LPrint Tab(72); Using "####"; pitRecYr!(I)
    Next

    LPrint Chr$(12)

    LPrint leagueFile$; "  TEAM RECORDS"
    LPrint
    LPrint "CATEGORY                        TEAM                        RECORD     YEAR"
    LPrint String$(75, 61)

    For I = 0 To 12
        LPrint teamRecACategory$(I); Tab(28); teamRecATeam$(I); Tab(62); Using "####"; teamRecAVal!(I); Tab(72); teamRecAYr!(I)
    Next

    For I = 15 To 19
        LPrint teamRecACategory$(I - 2); Tab(28); teamRecATeam$(I); Tab(62); Using "####"; teamRecAVal!(I); Tab(72); teamRecAYr!(I)
    Next

    For I = 22 To 24
        LPrint teamRecACategory$(I - 4); Tab(28); teamRecATeam$(I); Tab(62); Using ".###"; teamRecAVal!(I);: LPrint Tab(72); Using "####"; teamRecAYr!(I)
    Next

    LPrint teamRecBCategory$(0); Tab(28); teamRecBTeam$(0); Tab(60); Using "####"; Int(teamRecBVal!(0) / 3);

    If teamRecBVal!(0) - Int(teamRecBVal!(0) / 3) * 3 > 0 Then LPrint ".";: LPrint Using "#"; teamRecBVal!(0) - (Int(teamRecBVal!(0) / 3) * 3); Else LPrint ".0";

    LPrint Tab(72); Using "####"; teamRecBYr!(0)

    For I = 1 To 5
        LPrint teamRecBCategory$(I); Tab(28); teamRecBTeam$(I); Tab(62); Using "####"; teamRecBVal!(I);: LPrint Tab(72); Using "####"; teamRecBYr!(I)
    Next

    For I = 10 To 19
        LPrint teamRecBCategory$(I - 4); Tab(28); teamRecBTeam$(I); Tab(62); Using "####"; teamRecBVal!(I);: LPrint Tab(72); Using "####"; teamRecBYr!(I)
    Next

    LPrint teamRecBCategory$(16); Tab(28); teamRecBTeam$(21); Tab(62); Using "####"; teamRecBVal!(21);: LPrint Tab(72); Using "####"; teamRecBYr!(21)
    LPrint teamRecBCategory$(17); Tab(28); teamRecBTeam$(23); Tab(62); Using ".###"; teamRecBVal!(23);: LPrint Tab(72); Using "####"; teamRecBYr!(23)
    LPrint teamRecBCategory$(18); Tab(28); teamRecBTeam$(24); Tab(61); Using "##.##"; teamRecBVal!(24);: LPrint Tab(72); Using "####"; teamRecBYr!(24)

    LPrint Chr$(12)

End Sub

