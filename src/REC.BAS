'----------------------------------------
'        SeasonRecords Subroutine
'----------------------------------------
'This subroutine gives the user a number of
'menu selections for viewing, updating and
'printing records
Sub SeasonRecords ()

    _Title "Full Count Baseball - Season Records"

    ReDim menuChoices$(1 To 5)

    Shared batRecTmVal!(), batRecTmYr!()
    Shared batRecVal!(), batRecYr!()
    Shared pitRecTmVal!(), pitRecTmYr!()
    Shared pitRecVal!(), pitRecYr!()

    Shared batRecPlyr$(), batRecTeam$(), batRecTmDesc$()
    Shared pitRecTmDesc$(), pitRecPlyr$(), pitRecTeam$()

    'If Not _FileExists(diskPaths$(1) + "RECORDS.MLB") Then
    '
    '   MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
    '    BoxWidth = 24
    '   BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
    '   Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
    '   Call CSMsgBox(MsgText$, BoxWidth, Pass)
    '   I$ = GetKeyPress$
    '   Call CSMsgBox("", 0, Pass)
    '
    'Else

    Do

        Cls

        Call TITLE

        menuChoices$(1) = "UPDATE SEASON RECORDS"
        menuChoices$(2) = "VIEW SEASON RECORDS  "
        menuChoices$(3) = "PRINT SEASON RECORDS "
        menuChoices$(4) = "DELETE SEASON RECORDS"
        menuChoices$(5) = "RETURN TO MAIN MENU  "
        SELECTION% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    leagueFile$ = _OpenFileDialog$("Choose League Record file", diskPaths$(2), "*.BAT", "League Record files [.BAT]")

                    'ChooseFile$(diskPaths$(2) + "*.BAT", "LEAGUE", FileExit%)

                    If leagueFile$ <> "" Then
                        Call UpdateRecords(GetFileBaseName$(leagueFile$), SELECTION%)
                    End If

                Case 2:
                    leagueFile$ = _OpenFileDialog$("Choose League Record file", diskPaths$(2), "*.BAT", "League Record files [.BAT]")
                    'ChooseFile$(diskPaths$(2) + "*.BAT", "LEAGUE", FileExit%)

                    If leagueFile$ <> "" Then
                        Call ViewSeasonRecords(GetFileBaseName$(leagueFile$))
                    End If

                Case 3:
                    leagueFile$ = _OpenFileDialog$("Choose League Record file", diskPaths$(2), "*.BAT", "League Record files [.BAT]")
                    'ChooseFile$(diskPaths$(2) + "*.BAT", "LEAGUE", FileExit%)

                    If leagueFile$ <> "" Then

                        userKey$ = GetPrintOptions$

                        If userKey$ <> Chr$(27) Then

                            Select Case userKey$

                                Case "F"
                                    userFile$ = _SaveFileDialog$("Save Records Report", _CWD$ + "\Records", "*.txt", "Text File")
                                    If userFile$ <> "" Then
                                        Call PrintSeasonRecords(0, userFile$, GetFileBaseName$(leagueFile$))
                                    End If

                                Case "P"
                                    Call PrintSeasonRecords(1, "RECORDS", GetFileBaseName$(leagueFile$))

                            End Select

                        End If

                    End If

                Case 4
                    If _FileExists(diskPaths$(1) + "RECORDS.MLB") Then
                        Kill diskPaths$(1) + "RECORDS.MLB"
                    End If

            End Select


        End If 'Done checking for user ESC

    Loop Until SELECTION% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'       UpdateRecords Subroutine
'----------------------------------------
'Read in all league stats and calculate
'records so that an output file can be
'made containing all records.
Sub UpdateRecords (leagueFile$, userOpt)

    Shared expLdrB0(), expLdrP0()

    Shared batRecTmVal!(), batRecTmYr!()
    Shared batRecVal!(), batRecYr!()
    Shared pitRecTmVal!(), pitRecTmYr!()
    Shared pitRecVal!(), pitRecYr!()

    Shared batTotGames!()
    Shared plyLdrVal!(), teamLdrVal!()
    Shared divWins(), divLosses()

    Shared pitTotGames!()

    Shared batRecPlyr$(), batRecTeam$(), batRecTmDesc$()
    Shared pitRecTmDesc$(), pitRecPlyr$(), pitRecTeam$()
    Shared expBatLdrName$(), expPitLdrName$()
    Shared expBatLdrTeam$(), expPitLdrTeam$()

    Dim TT$(40, 22), TT1$(40, 23)

    AAG = 920
    AAS = 880
    AAT = 80
    TMR = 2360

    JB = 0
    JP = 0
    TS = 0

    If Not _FileExists(diskPaths$(2) + "RECORDS.MLB") And userOpt >= 2 And userOpt <= 4 Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call ReadSeasonRecords("RECORDS.MLB")

        Cls
        Input "ENTER SEASON YEAR (1991,1992)"; seasonYear

        Do
            Locate 2
            Input "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)"; finalUpdate$
        Loop Until UCase$(finalUpdate$) = "Y" Or UCase$(finalUpdate$) = "N"

        Print
        Print "ANY CHANGE? (Y/N)"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If Not _FileExists(diskPaths$(2) + "RECORDS.MLB") Then

            pitRecVal!(23) = 100
            pitRecVal!(25) = 100
            batRecVal!(24) = 999
            batRecVal!(25) = 999
            batRecVal!(26) = 999
            batRecVal!(27) = 999
            batRecVal!(28) = 999
            pitRecVal!(27) = 100
            pitRecTmVal!(24) = 100

        Else

            Print "READING IN CURRENT RECORDS"

            Call ReadSeasonRecords("RECORDS.MLB")

        End If

        TMR = 3680

        Open diskPaths$(2) + leagueFile$ + ".BAT" For Input As #1

        Input #1, JB, JP, div1$, div2$, div3$, div4$, LGF$, TS, TTS

        For I = 1 To 920:
            Input #1, expBatLdrName$(I), expBatLdrTeam$(I), batTotGames!(I)
            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next I

        For I = 1 To 920:
            For I1 = 1 To 22
                Input #1, expLdrB0(I, I1)
            Next I1
            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next I

        Close 1

        Open diskPaths$(2) + leagueFile$ + ".PIT" For Input As #1

        Input #1, JB, JP, div1$, div2$, div3$, div4$, LGF$, TS, TTS

        For I = 1 To 880:
            Input #1, expPitLdrName$(I), expPitLdrTeam$(I), pitTotGames!(I)
            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next I

        For I = 1 To 880:

            For I1 = 1 To 23:
                Input #1, expLdrP0(I, I1)
            Next I1

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

        Next I

        Close 1

        Open diskPaths$(2) + leagueFile$ + ".TOT" For Input As #1

        Input #1, JB, JP, div1$, div2$, div3$, div4$, LGF$, TS, TTS

        For I = 1 To TEAMS_PER_LEAGUE
            Input #1, divWins(I), divLosses(I)
        Next

        For I = 1 To TEAMS_PER_LEAGUE

            For I1 = 0 To 21:
                Input #1, plyLdrVal!(I, I1), TT$(I, I1)
            Next

            Color 14
            Locate 18, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

        Next

        For I = 1 To TEAMS_PER_LEAGUE

            For I1 = 0 To 22:
                Input #1, teamLdrVal!(I, I1), TT1$(I, I1)
            Next

            Color 14
            Locate 18, 45
            TMR = TMR - 1
            Print Using "#####"; TMR

        Next

        Close #1

        Locate 7: Print "CALCULATING BATTER RECORDS . . . ."

        For I = 1 To 920

            For I1 = 1 To 20

                If expLdrB0(I, I1) >= batRecVal!(I1) Then
                    batRecVal!(I1) = expLdrB0(I, I1)
                    batRecPlyr$(I1) = expBatLdrName$(I)
                    batRecTeam$(I1) = expBatLdrTeam$(I)
                    batRecYr!(I1) = seasonYear
                End If

            Next I1

            AAG = AAG - 1
            Locate 7, 40: Print Using "###"; AAG

        Next I

        If finalUpdate$ = "Y" Then

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 1) <> 0 Then
                    If expLdrB0(I, 2) / expLdrB0(I, 1) >= batRecVal!(21) Then
                        batRecVal!(21) = expLdrB0(I, 2) / expLdrB0(I, 1)
                        batRecPlyr$(21) = expBatLdrName$(I)
                        batRecTeam$(21) = expBatLdrTeam$(I)
                        batRecYr!(21) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 2) <> 0 Then
                    If (expLdrB0(I, 2) + expLdrB0(I, 5) + expLdrB0(I, 6) * 2 + expLdrB0(I, 7) * 3) / expLdrB0(I, 1) >= batRecVal!(22) Then
                        batRecVal!(22) = (expLdrB0(I, 2) + expLdrB0(I, 5) + expLdrB0(I, 6) * 2 + expLdrB0(I, 7) * 3) / expLdrB0(I, 1)
                        batRecPlyr$(22) = expBatLdrName$(I)
                        batRecTeam$(22) = expBatLdrTeam$(I)
                        batRecYr!(22) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 2) <> 0 Then
                    If (expLdrB0(I, 2) + expLdrB0(I, 8)) / (expLdrB0(I, 1) + expLdrB0(I, 8)) >= batRecVal!(23) Then
                        batRecVal!(23) = (expLdrB0(I, 2) + expLdrB0(I, 8)) / (expLdrB0(I, 1) + expLdrB0(I, 8))
                        batRecPlyr$(23) = expBatLdrName$(I)
                        batRecTeam$(23) = expBatLdrTeam$(I)
                        batRecYr!(23) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 7) <> 0 Then
                    If expLdrB0(I, 1) / expLdrB0(I, 7) <= batRecVal!(24) Then
                        batRecVal!(24) = expLdrB0(I, 1) / expLdrB0(I, 7)
                        batRecPlyr$(24) = expBatLdrName$(I)
                        batRecTeam$(24) = expBatLdrTeam$(I)
                        batRecYr!(24) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 9) <> 0 Then
                    If expLdrB0(I, 1) / expLdrB0(I, 9) <= batRecVal!(25) Then
                        batRecVal!(25) = expLdrB0(I, 1) / expLdrB0(I, 9)
                        batRecPlyr$(25) = expBatLdrName$(I)
                        batRecTeam$(25) = expBatLdrTeam$(I)
                        batRecYr!(25) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 8) <> 0 Then
                    If expLdrB0(I, 1) / expLdrB0(I, 8) > batRecVal!(26) Then
                        batRecVal!(26) = expLdrB0(I, 1) / expLdrB0(I, 8)
                        batRecPlyr$(26) = expBatLdrName$(I)
                        batRecTeam$(26) = expBatLdrTeam$(I)
                        batRecYr!(26) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 4) <> 0 Then
                    If expLdrB0(I, 1) / expLdrB0(I, 4) <= batRecVal!(27) Then
                        batRecVal!(27) = expLdrB0(I, 1) / expLdrB0(I, 4)
                        batRecPlyr$(27) = expBatLdrName$(I)
                        batRecTeam$(27) = expBatLdrTeam$(I)
                        batRecYr!(27) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 3) <> 0 Then
                    If expLdrB0(I, 1) / expLdrB0(I, 3) <= batRecVal!(28) Then
                        batRecVal!(28) = expLdrB0(I, 1) / expLdrB0(I, 3)
                        batRecPlyr$(28) = expBatLdrName$(I)
                        batRecTeam$(28) = expBatLdrTeam$(I)
                        batRecYr!(28) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 3.1 And expLdrB0(I, 16) <> 0 Then
                    If expLdrB0(I, 1) / expLdrB0(I, 16) >= batRecVal!(29) Then
                        batRecVal!(29) = expLdrB0(I, 1) / expLdrB0(I, 16)
                        batRecPlyr$(29) = expBatLdrName$(I)
                        batRecTeam$(29) = expBatLdrTeam$(I)
                        batRecYr!(29) = seasonYear
                    End If
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 21) + expLdrB0(I, 22) >= batRecVal!(30) Then
                    batRecVal!(30) = expLdrB0(I, 21) + expLdrB0(I, 22)
                    batRecPlyr$(30) = expBatLdrName$(I)
                    batRecTeam$(30) = expBatLdrTeam$(I)
                    batRecYr!(30) = seasonYear
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 22) >= batRecVal!(31) Then
                    batRecVal!(31) = expLdrB0(I, 22)
                    batRecPlyr$(31) = expBatLdrName$(I)
                    batRecTeam$(31) = expBatLdrTeam$(I)
                    batRecYr!(31) = seasonYear
                End If

            Next I

            For I = 1 To 920

                If expLdrB0(I, 21) + expLdrB0(I, 22) <> 0 And expLdrB0(I, 1) + expLdrB0(I, 8) >= batTotGames!(I) * 2.1 Then
                    If expLdrB0(I, 22) / (expLdrB0(I, 21) + expLdrB0(I, 22)) >= batRecVal!(32) Then
                        batRecVal!(32) = expLdrB0(I, 22) / (expLdrB0(I, 21) + expLdrB0(I, 22))
                        batRecPlyr$(32) = expBatLdrName$(I)
                        batRecTeam$(32) = expBatLdrTeam$(I)
                        batRecYr!(32) = seasonYear
                    End If
                End If

            Next I

        End If

        Locate 8: Print "CALCULATING PITCHER RECORDS . . . ."

        For I = 1 To 880
            For I1 = 1 To 23
                If expLdrP0(I, I1) >= pitRecVal!(I1) Then
                    pitRecVal!(I1) = expLdrP0(I, I1)
                    pitRecPlyr$(I1) = expPitLdrName$(I)
                    pitRecTeam$(I1) = expPitLdrTeam$(I)
                    pitRecYr!(I1) = seasonYear
                End If
            Next I1

            AAS = AAS - 1
            Locate 8, 40: Print Using "###"; AAS
        Next I

        If finalUpdate$ = "Y" Then

            For I = 1 To 880
                If expLdrP0(I, 18) <> 0 And expLdrP0(I, 9) >= 25 Then
                    If expLdrP0(I, 19) / expLdrP0(I, 18) <= pitRecVal!(24) Then
                        pitRecVal!(24) = expLdrP0(I, 19) / expLdrP0(I, 18)
                        pitRecPlyr$(24) = expPitLdrName$(I)
                        pitRecTeam$(24) = expPitLdrTeam$(I)
                        pitRecYr!(24) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To 880
                If expLdrP0(I, 1) / 3 >= pitTotGames!(I) And expLdrP0(I, 21) <> 0 Then
                    If expLdrP0(I, 21) / expLdrP0(I, 9) < pitRecVal!(25) Then
                        pitRecVal!(25) = expLdrP0(I, 21) / expLdrP0(I, 9)
                        pitRecPlyr$(25) = expPitLdrName$(I)
                        pitRecTeam$(25) = expPitLdrTeam$(I)
                        pitRecYr!(25) = seasonYear
                    End If
                End If
            Next I

            'IF TXX = 1 THEN 2785
            For I = 1 To 880
                If expLdrP0(I, 1) / 3 >= pitTotGames!(I) And expLdrP0(I, 1) <> 0 Then
                    If (expLdrP0(I, 4) * 9) / (expLdrP0(I, 1) / 3) > pitRecVal!(23) Then
                        pitRecVal!(23) = (expLdrP0(I, 4) * 9) / (expLdrP0(I, 1) / 3)
                        pitRecPlyr$(23) = expPitLdrName$(I)
                        pitRecTeam$(23) = expPitLdrTeam$(I)
                        pitRecYr!(23) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To 880
                If expLdrP0(I, 1) / 3 >= pitTotGames!(I) And expLdrP0(I, 1) <> 0 Then
                    If (expLdrP0(I, 5) * 9) / (expLdrP0(I, 1) / 3) > pitRecVal!(25) Then
                        pitRecVal!(25) = (expLdrP0(I, 5) * 9) / (expLdrP0(I, 1) / 3)
                        pitRecPlyr$(25) = expPitLdrName$(I)
                        pitRecTeam$(25) = expPitLdrTeam$(I)
                        pitRecYr!(25) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To 880
                If expLdrP0(I, 1) / 3 >= pitTotGames!(I) And expLdrP0(I, 1) <> 0 Then
                    If (expLdrP0(I, 6) * 9) / (expLdrP0(I, 1) / 3) < pitRecVal!(26) Then
                        pitRecVal!(26) = (expLdrP0(I, 6) * 9) / (expLdrP0(I, 1) / 3)
                        pitRecPlyr$(26) = expPitLdrName$(I)
                        pitRecTeam$(26) = expPitLdrTeam$(I)
                        pitRecYr!(26) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To 880
                If expLdrP0(I, 1) / 3 >= pitTotGames!(I) And expLdrP0(I, 1) <> 0 Then
                    If (expLdrP0(I, 2) * 9) / (expLdrP0(I, 1) / 3) > pitRecVal!(27) Then
                        pitRecVal!(27) = (expLdrP0(I, 2) * 9) / (expLdrP0(I, 1) / 3)
                        pitRecPlyr$(27) = expPitLdrName$(I)
                        pitRecTeam$(27) = expPitLdrTeam$(I)
                        pitRecYr!(27) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To 880
                If expLdrP0(I, 1) / 3 >= pitTotGames!(I) And expLdrP0(I, 1) <> 0 Then
                    If (expLdrP0(I, 14) * 9) / (expLdrP0(I, 1) / 3) < pitRecVal!(28) Then
                        pitRecVal!(28) = (expLdrP0(I, 14) * 9) / (expLdrP0(I, 1) / 3)
                        pitRecPlyr$(28) = expPitLdrName$(I)
                        pitRecTeam$(28) = expPitLdrTeam$(I)
                        pitRecYr!(28) = seasonYear
                    End If
                End If
            Next I

        End If

        Locate 9: Print "CALCULATING TEAM RECORDS . . . ."

        For I = 1 To TEAMS_PER_LEAGUE
            For I1 = 0 To 21
                If plyLdrVal!(I, I1) > batRecTmVal!(I1) Then
                    batRecTmVal!(I1) = plyLdrVal!(I, I1)
                    batRecTmDesc$(I1) = TT$(I, I1)
                    batRecTmYr!(I1) = seasonYear
                End If
            Next I1

            AAT = AAT - 1
            Locate 9, 40: Print Using "###"; AAT
        Next I

        If finalUpdate$ = "Y" Then

            For I = 1 To TEAMS_PER_LEAGUE
                If plyLdrVal!(I, 0) <> 0 Then
                    If plyLdrVal!(I, 1) / plyLdrVal!(I, 0) > batRecTmVal!(22) Then
                        batRecTmVal!(22) = plyLdrVal!(I, 1) / plyLdrVal!(I, 0)
                        batRecTmDesc$(22) = TT$(I, 1)
                        batRecTmYr!(22) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To TEAMS_PER_LEAGUE
                If plyLdrVal!(I, 0) <> 0 Then
                    If (plyLdrVal!(I, 1) + plyLdrVal!(I, 4) + (plyLdrVal!(I, 5) * 2) + (plyLdrVal!(I, 6) * 3)) / plyLdrVal!(I, 0) > batRecTmVal!(23) Then
                        batRecTmVal!(23) = (plyLdrVal!(I, 1) + plyLdrVal!(I, 4) + (plyLdrVal!(I, 5) * 2) + (plyLdrVal!(I, 6) * 3)) / plyLdrVal!(I, 0)
                        batRecTmDesc$(23) = TT$(I, 1)
                        batRecTmYr!(23) = seasonYear
                    End If
                End If
            Next I

            For I = 1 To TEAMS_PER_LEAGUE
                If plyLdrVal!(I, 0) <> 0 Then
                    If (plyLdrVal!(I, 1) + plyLdrVal!(I, 7)) / (plyLdrVal!(I, 0) + plyLdrVal!(I, 7)) > batRecTmVal!(24) Then
                        batRecTmVal!(24) = (plyLdrVal!(I, 1) + plyLdrVal!(I, 7)) / (plyLdrVal!(I, 0) + plyLdrVal!(I, 7))
                        batRecTmDesc$(24) = TT$(I, 1)
                        batRecTmYr!(24) = seasonYear
                    End If
                End If
            Next I

        Else

            For I = 1 To TEAMS_PER_LEAGUE
                For I1 = 0 To 22
                    If teamLdrVal!(I, I1) > pitRecTmVal!(I1) Then
                        pitRecTmVal!(I1) = teamLdrVal!(I, I1)
                        pitRecTmDesc$(I1) = TT1$(I, I1)
                        pitRecTmYr!(I1) = seasonYear
                    End If
                Next I1

                AAT = AAT - 1
                Locate 9, 40: Print Using "###"; AAT
            Next I

            If finalUpdate$ = "Y" Then

                For I = 1 To TEAMS_PER_LEAGUE
                    If teamLdrVal!(I, 17) <> 0 Then
                        If teamLdrVal!(I, 18) / teamLdrVal!(I, 17) > pitRecTmVal!(23) Then
                            pitRecTmVal!(23) = teamLdrVal!(I, 18) / teamLdrVal!(I, 17)
                            pitRecTmDesc$(23) = TT1$(I, 0)
                            pitRecTmYr!(23) = seasonYear
                        End If
                    End If
                Next I

                'IF TXX = 1 THEN
                For I = 1 To TEAMS_PER_LEAGUE
                    If teamLdrVal!(I, 0) <> 0 Then
                        If (teamLdrVal!(I, 3) * 9) / (teamLdrVal!(I, 0) / 3) < pitRecTmVal!(24) Then
                            pitRecTmVal!(24) = (teamLdrVal!(I, 3) * 9) / (teamLdrVal!(I, 0) / 3)
                            pitRecTmDesc$(24) = TT1$(I, 0)
                            pitRecTmYr!(24) = seasonYear
                        End If
                    End If
                Next I

            End If

        End If 'Done checking for final update

        Open diskPaths$(2) + "RECORDS.MLB" For Output As #1

        For I = 1 To 32:
            Write #1, batRecVal!(I), batRecPlyr$(I), batRecTeam$(I), batRecYr!(I)
        Next

        For I = 1 To 28:
            Write #1, pitRecVal!(I), pitRecPlyr$(I), pitRecTeam$(I), pitRecYr!(I)
        Next

        For I = 0 To 24:
            Write #1, batRecTmVal!(I), batRecTmDesc$(I), batRecTmYr!(I)
        Next

        For I = 0 To 24:
            Write #1, pitRecTmVal!(I), pitRecTmDesc$(I), pitRecTmYr!(I)
        Next

        Close #1

    End If 'Done checking for records file

End Sub


'----------------------------------------
'       ViewSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the user with
'viewing full season records.
Sub ViewSeasonRecords (leagueFile$)

    Shared batRecTmVal!(), batRecTmYr!()
    Shared batRecVal!(), batRecYr!()
    Shared pitRecTmVal!(), pitRecTmYr!()
    Shared pitRecVal!(), pitRecYr!()

    Shared batRecPlyr$(), batRecTeam$(), batRecTmDesc$()
    Shared pitRecTmDesc$(), pitRecPlyr$(), pitRecTeam$()

    Shared batIndCategory$(), pitIndCategory$(), batTeamCategory$(), pitTeamCategory$()

    If Not _FileExists(diskPaths$(2) + "RECORDS.MLB") Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call ReadSeasonRecords("RECORDS.MLB")

        Color 0, 0

        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 4)

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
            Print batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "####"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
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
            Print batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "####"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
        Next

        For I = 21 To 23
            Locate , 4
            Print batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using ".###"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
        Next

        For I = 24 To 29
            Locate , 4
            Print batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(60); Using "###.#"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
        Next

        For I = 30 To 31
            Locate , 4
            Print batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "###"; batRecVal!(I);: Print Tab(70); Using "####"; batRecYr!(I)
        Next

        Locate , 4
        Print batIndCategory$(32); Tab(28); batRecPlyr$(32); Tab(43); batRecTeam$(32); Tab(62); Using ".###"; batRecVal!(32);: Print Tab(70); Using "####"; batRecYr!(32)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 1)

        Locate 1
        Print leagueFile$
        Locate 2, 24
        Print "******* PITCHING RECORDS *******"
        Print
        Locate , 4
        Print "CATEGORY                   PLAYER         TEAM           RECORD   YEAR"

        Color 7

        Locate , 4
        Print pitIndCategory$(1); Tab(28); pitRecPlyr$(1); Tab(43); pitRecTeam$(1); Tab(61); Using "###"; Int(pitRecVal!(1) / 3);
        If pitRecVal!(1) - Int(pitRecVal!(1) / 3) * 3 > 0 Then
            Print ".";: Print Using "#"; pitRecVal!(1) - (Int(pitRecVal!(1) / 3) * 3);
        Else
            Print ".0";
        End If
        Print Tab(70); Using "####"; pitRecYr!(1)

        For I = 2 To 15
            Locate , 4
            Print pitIndCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(62); Using "####"; pitRecVal!(I); Tab(70); pitRecYr!(I)
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
            Print pitIndCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(62); Using "####"; pitRecVal!(I);: Print Tab(70); Using "####"; pitRecYr!(I)
        Next

        Locate , 4
        Print pitIndCategory$(20); Tab(28); pitRecPlyr$(24); Tab(43); pitRecTeam$(24); Tab(62); Using ".###"; pitRecVal!(24) * 100;: Print Tab(70); Using "####"; pitRecYr!(24)
        Locate , 4
        Print pitIndCategory$(21); Tab(28); pitRecPlyr$(20); Tab(43); pitRecTeam$(20); Tab(62); Using "####"; pitRecVal!(20);: Print Tab(70); Using "####"; pitRecYr!(20)
        Locate , 4
        Print pitIndCategory$(22); Tab(28); pitRecPlyr$(25); Tab(43); pitRecTeam$(25); Tab(61); Using "##.##"; pitRecVal!(25);: Print Tab(70); Using "####"; pitRecYr!(25)
        Locate , 4
        Print pitIndCategory$(23); Tab(28); pitRecPlyr$(22); Tab(43); pitRecTeam$(22); Tab(62); Using "####"; pitRecVal!(22);: Print Tab(70); Using "####"; pitRecYr!(22)
        Locate , 4
        Print pitIndCategory$(24); Tab(28); pitRecPlyr$(23); Tab(43); pitRecTeam$(23); Tab(60); Using "###.##"; pitRecVal!(23);: Print Tab(70); Using "####"; pitRecYr!(23)

        For I = 25 To 28
            Locate , 4
            Print pitIndCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(61); Using "##.##"; pitRecVal!(I);: Print Tab(70); Using "####"; pitRecYr!(I)
        Next

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 14, 0)

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
            Print batTeamCategory$(I); Tab(28); batRecTmDesc$(I); Tab(62); Using "####"; batRecTmVal!(I);: Print Tab(70); Using "####"; batRecTmYr!(I)
        Next

        For I = 15 To 16
            Locate , 4
            Print batTeamCategory$(I - 2); Tab(28); batRecTmDesc$(I); Tab(62); Using "####"; batRecTmVal!(I);: Print Tab(70); Using "####"; batRecTmYr!(I)
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
            Print batTeamCategory$(I - 2); Tab(28); batRecTmDesc$(I); Tab(62); Using "####"; batRecTmVal!(I);: Print Tab(70); Using "####"; batRecTmYr!(I)
        Next

        For I = 22 To 24
            Locate , 4
            Print batTeamCategory$(I - 4); Tab(28); batRecTmDesc$(I); Tab(62); Using ".###"; batRecTmVal!(I);: Print Tab(70); Using "####"; batRecTmYr!(I)
        Next

        'Pitcher Records
        Locate , 4
        Print pitTeamCategory$(0); Tab(28); pitRecTmDesc$(0); Tab(60); Using "####"; Int(pitRecTmVal!(0) / 3);
        If pitRecTmVal!(0) - Int(pitRecTmVal!(0) / 3) * 3 > 0 Then
            Print ".";
            Print Using "#"; pitRecTmVal!(0) - (Int(pitRecTmVal!(0) / 3) * 3);
        Else
            Print ".0";
        End If
        Print Tab(70); Using "####"; pitRecTmYr!(0)

        For I = 1 To 5
            Locate , 4
            Print pitTeamCategory$(I); Tab(28); pitRecTmDesc$(I); Tab(62); Using "####"; pitRecTmVal!(I);: Print Tab(70); Using "####"; pitRecTmYr!(I)
        Next

        For I = 10 To 12
            Locate , 4
            Print pitTeamCategory$(I - 4); Tab(28); pitRecTmDesc$(I); Tab(62); Using "####"; pitRecTmVal!(I);: Print Tab(70); Using "####"; pitRecTmYr!(I)
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
            Print pitTeamCategory$(I - 4); Tab(28); pitRecTmDesc$(I); Tab(62); Using "####"; pitRecTmVal!(I);
            Print Tab(70); Using "####"; pitRecTmYr!(I)
        Next

        Locate , 4
        Print pitTeamCategory$(16); Tab(28); pitRecTmDesc$(21); Tab(62); Using "####"; pitRecTmVal!(21);: Print Tab(70); Using "####"; pitRecTmYr!(21)
        Locate , 4
        Print pitTeamCategory$(17); Tab(28); pitRecTmDesc$(23); Tab(62); Using ".###"; pitRecTmVal!(23);: Print Tab(70); Using "####"; pitRecTmYr!(23)
        Locate , 4
        Print pitTeamCategory$(18); Tab(28); pitRecTmDesc$(24); Tab(61); Using "##.##"; pitRecTmVal!(24);: Print Tab(70); Using "####"; pitRecTmYr!(24)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

    End If 'Done checking for records file

End Sub


'----------------------------------------
'       PrintSeasonRecords Subroutine
'----------------------------------------
'This subroutine facilitates formatting
'of the record data to a file, that is
'subsequently either saved, or printed.
Sub PrintSeasonRecords (printDest, destFile$, leagueName$)

    Shared batRecTmVal!(), batRecTmYr!()
    Shared batRecVal!(), batRecYr!()
    Shared pitRecTmVal!(), pitRecTmYr!()
    Shared pitRecVal!(), pitRecYr!()

    Shared batRecPlyr$(), batRecTeam$(), batRecTmDesc$()
    Shared pitRecTmDesc$(), pitRecPlyr$(), pitRecTeam$()

    Shared batIndCategory$(), pitIndCategory$(), batTeamCategory$(), pitTeamCategory$()

    If Not _FileExists(diskPaths$(2) + "RECORDS.MLB") Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call ReadSeasonRecords("RECORDS.MLB")

        Color 15, 0
        Cls

        Open destFile$ For Output As #1

        Print #1, leagueName$; "  BATTING RECORDS"
        Print #1,
        Print #1, "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"

        Print #1, String$(75, 61)

        For I = 1 To 20
            Print #1, batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "####"; batRecVal!(I); Tab(66); batRecYr!(I)
        Next

        For I = 21 To 23
            Print #1, batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using ".###"; batRecVal!(I);: Print #1, Tab(66); Using "####"; batRecYr!(I)
        Next

        For I = 24 To 29
            Print #1, batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(60); Using "###.#"; batRecVal!(I);: Print #1, Tab(66); Using "####"; batRecYr!(I)
        Next

        For I = 30 To 31
            Print #1, batIndCategory$(I); Tab(28); batRecPlyr$(I); Tab(43); batRecTeam$(I); Tab(62); Using "###"; batRecVal!(I);: Print #1, Tab(66); Using "####"; batRecYr!(I)
        Next

        Print #1, batIndCategory$(32); Tab(28); batRecPlyr$(32); Tab(43); batRecTeam$(32); Tab(62); Using ".###"; batRecVal!(32);: Print #1, Tab(66); Using "####"; batRecYr!(32)
        Print #1, Chr$(12)

        Print #1, leagueFile$; "  PITCHING RECORDS"
        Print #1,
        Print #1, "CATEGORY                   PLAYER         TEAM             RECORD      YEAR"

        Print #1, String$(75, 61)
        Print #1, pitIndCategory$(1); Tab(28); pitRecPlyr$(1); Tab(43); pitRecTeam$(1); Tab(61); Using "###"; Int(pitRecVal!(1) / 3);

        If pitRecVal!(1) - Int(pitRecVal!(1) / 3) * 3 > 0 Then
            Print #1, ".";
            Print #1, Using "#"; pitRecVal!(1) - (Int(pitRecVal!(1) / 3) * 3);
        Else
            Print #1, ".0";
        End If

        Print #1, Tab(72); Using "####"; pitRecYr!(1)

        For I = 2 To 19
            Print #1, pitIndCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(62); Using "####"; pitRecVal!(I); Tab(72); pitRecYr!(I)
        Next

        Print #1, pitIndCategory$(20); Tab(28); pitRecPlyr$(24); Tab(43); pitRecTeam$(24); Tab(62); Using ".###"; pitRecVal!(24) * 100;: Print #1, Tab(72); Using "####"; pitRecYr!(24)
        Print #1, pitIndCategory$(21); Tab(28); pitRecPlyr$(20); Tab(43); pitRecTeam$(20); Tab(62); Using "####"; pitRecVal!(20); Tab(72); pitRecYr!(20)
        Print #1, pitIndCategory$(22); Tab(28); pitRecPlyr$(25); Tab(43); pitRecTeam$(25); Tab(61); Using "##.##"; pitRecVal!(25);: Print #1, Tab(72); Using "####"; pitRecYr!(25)
        Print #1, pitIndCategory$(23); Tab(28); pitRecPlyr$(22); Tab(43); pitRecTeam$(22); Tab(62); Using "####"; pitRecVal!(22); Tab(72); pitRecYr!(22)
        Print #1, pitIndCategory$(24); Tab(28); pitRecPlyr$(23); Tab(43); pitRecTeam$(23); Tab(60); Using "###.##"; pitRecVal!(23); Tab(72);

        Print #1, Using "####"; pitRecYr!(23)

        For I = 25 To 28
            Print #1, pitIndCategory$(I); Tab(28); pitRecPlyr$(I); Tab(43); pitRecTeam$(I); Tab(61); Using "##.##"; pitRecVal!(I);: Print #1, Tab(72); Using "####"; pitRecYr!(I)
        Next

        Print #1, Chr$(12)

        Print #1, leagueFile$; "  TEAM RECORDS"
        Print #1,
        Print #1, "CATEGORY                        TEAM                        RECORD     YEAR"
        Print #1, String$(75, 61)

        For I = 0 To 12
            Print #1, batTeamCategory$(I); Tab(28); batRecTmDesc$(I); Tab(62); Using "####"; batRecTmVal!(I); Tab(72); batRecTmYr!(I)
        Next

        For I = 15 To 19
            Print #1, batTeamCategory$(I - 2); Tab(28); batRecTmDesc$(I); Tab(62); Using "####"; batRecTmVal!(I); Tab(72); batRecTmYr!(I)
        Next

        For I = 22 To 24
            Print #1, batTeamCategory$(I - 4); Tab(28); batRecTmDesc$(I); Tab(62); Using ".###"; batRecTmVal!(I);
            Print #1, Tab(72); Using "####"; batRecTmYr!(I)
        Next

        Print #1, pitTeamCategory$(0); Tab(28); pitRecTmDesc$(0); Tab(60); Using "####"; Int(pitRecTmVal!(0) / 3);

        If pitRecTmVal!(0) - Int(pitRecTmVal!(0) / 3) * 3 > 0 Then
            Print #1, ".";
            Print #1, Using "#"; pitRecTmVal!(0) - (Int(pitRecTmVal!(0) / 3) * 3);
        Else
            Print #1, ".0";
        End If

        Print #1, Tab(72); Using "####"; pitRecTmYr!(0)

        For I = 1 To 5
            Print #1, pitTeamCategory$(I); Tab(28); pitRecTmDesc$(I); Tab(62); Using "####"; pitRecTmVal!(I);
            Print #1, Tab(72); Using "####"; pitRecTmYr!(I)
        Next

        For I = 10 To 19
            Print #1, pitTeamCategory$(I - 4); Tab(28); pitRecTmDesc$(I); Tab(62); Using "####"; pitRecTmVal!(I);
            Print #1, Tab(72); Using "####"; pitRecTmYr!(I)
        Next

        Print #1, pitTeamCategory$(16); Tab(28); pitRecTmDesc$(21); Tab(62); Using "####"; pitRecTmVal!(21);: Print #1, Tab(72); Using "####"; pitRecTmYr!(21)
        Print #1, pitTeamCategory$(17); Tab(28); pitRecTmDesc$(23); Tab(62); Using ".###"; pitRecTmVal!(23);: Print #1, Tab(72); Using "####"; pitRecTmYr!(23)

        Print #1, pitTeamCategory$(18); Tab(28); pitRecTmDesc$(24); Tab(61); Using "##.##"; pitRecTmVal!(24);: Print #1, Tab(72); Using "####"; pitRecTmYr!(24)

        Print #1, Chr$(12)

        Close #1

        If printDest = 1 Then

            Open destFile$ For Input As #2

            Do While Not EOF(2)
                Line Input #2, X$
                LPrint X$
            Loop

            Close #2

            LPrint Chr$(12)

            Kill destFile$

        End If

    End If 'Done checking for records file

End Sub
