'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine presents a menu of options
'for the user to edit replay stats.
Sub EditStats ()

    _Title "Full Count Baseball - Stats Input"

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT / CHANGE REPLAY STATS    "
        menuChoices$(2) = "EDIT / CHANGE SCHEDULE RESULTS"
        menuChoices$(3) = "RETURN TO MAIN MENU           "
        SELECTION% = 1
        Count% = 3
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

                Case 1, 2: 
                    Call EditStatsSchedules(SELECTION%)

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'     EditStatsSchedules Subroutine
'----------------------------------------
'This subroutine lets the user pick a team
'and then calls the corresponding routine
'for the user to edit Stats or Schedule
'Results.
Sub EditStatsSchedules (action%)

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)    
    statFile$ = nameStatFile$(team$)

    If action% = 2 Then

        Call ChangeScheduleStats(statFile$)

    Else
        'action% = 1
        If Not _FileExists(diskPaths$(1) + statFile$) Then

            MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call ModifyTeamStats(statFile$)

        End If

    End If

End Sub

'----------------------------------------
'       ModifyTeamStats Subroutine
'----------------------------------------
'This subroutine inputs stats from the
'chosen stat file and then gives the user
'an interface for modifying the stats.
Sub ModifyTeamStats (statFile$)

    Shared statTeamDP, statOppDP

    Shared batterNames$(), pitcherNames$()
    Shared statBatter(), statPitcher()
    Shared teamRatings()
    Shared statAtBats(), statInnPitch(), statTeamBat!(), statTeamPit!()

    Call ReadTeamStats (teamName$, statFile$)

    '---------------
    ' Batting Stats
    '---------------
    Color 15, 0
    Cls
    
    X$ = "TEAM BATTING STATS"
    Call TITLE_EDITSTATS(2, X$)

    Locate 3, 1
    Print " 1. TEAM NAME "; teamName$
    Print " 2. TM AB "; statTeamBat!(0)
    Print " 3. TM RUNS "; statTeamBat!(2)
    Print " 4. TM HITS "; statTeamBat!(1)
    Print " 5. TM DBL "; statTeamBat!(4)
    Print " 6. TM TRPL "; statTeamBat!(5)
    Print " 7. TM HR "; statTeamBat!(6)
    Print " 8. TM RBI "; statTeamBat!(3)
    Print " 9. TM BB "; statTeamBat!(7)
    Print "10. TM SO "; statTeamBat!(8)
    Print "11. TM ERR "; statTeamBat!(11)
    Print "12. TM SB "; statTeamBat!(9)
    Print "13. TM CS "; statTeamBat!(10)
    Print "14. TM PO "; statTeamBat!(16)
    Print "15. TM A "; statTeamBat!(17)
    Print "16. TM SH "; statTeamBat!(18)
    Print "17. TM SF "; statTeamBat!(19)
    Print "18. TM GIDP "; statTeamBat!(15)
    Print "19. TM LOB "; statTeamBat!(12)

    Do

        Do
            Locate 25, 29
            Print Space$(10)
            Locate 25, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 19

        Select Case B9

            Case 1
                Locate 3, 1: Input " 1. TEAM NAME "; teamName$
                Locate 3, 14: Print " "

            Case 2
                Locate 4, 10: Print Space$(10)
                Locate 4, 1: Input " 2. TM AB"; statTeamBat!(0)
                Locate 4, 10: Print " "

            Case 3
                Locate 5, 12: Print Space$(10)
                Locate 5, 1: Input " 3. TM RUNS"; statTeamBat!(2)
                Locate 5, 12: Print " "

            Case 4
                Locate 6, 12: Print Space$(10)
                Locate 6, 1: Input " 4. TM HITS"; statTeamBat!(1)
                Locate 6, 12: Print " "

            Case 5
                Locate 7, 11: Print Space$(10)
                Locate 7, 1: Input " 5. TM DBL"; statTeamBat!(4)
                Locate 7, 11: Print " "

            Case 6
                Locate 8, 12: Print Space$(10)
                Locate 8, 1: Input " 6. TM TRPL"; statTeamBat!(5)
                Locate 8, 12: Print " "

            Case 7
                Locate 9, 10: Print Space$(10)
                Locate 9, 1: Input " 7. TM HR"; statTeamBat!(6)
                Locate 9, 10: Print " "

            Case 8
                Locate 10, 11: Print Space$(10)
                Locate 10, 1: Input " 8. TM RBI"; statTeamBat!(3)
                Locate 10, 11: Print " "

            Case 9
                Locate 11, 10: Print Space$(10)
                Locate 11, 1: Input " 9. TM BB"; statTeamBat!(7)
                Locate 11, 10: Print " "

            Case 10
                Locate 12, 10: Print Space$(10)
                Locate 12, 1: Input "10. TM SO"; statTeamBat!(8)
                Locate 12, 10: Print " "

            Case 11
                Locate 13, 11: Print Space$(10)
                Locate 13, 1: Input "11. TM ERR"; statTeamBat!(11)
                Locate 13, 11: Print " "

            Case 12
                Locate 14, 10: Print Space$(10)
                Locate 14, 1: Input "12. TM SB"; statTeamBat!(9)
                Locate 14, 10: Print " "

            Case 13
                Locate 15, 10: Print Space$(10)
                Locate 15, 1: Input "13. TM CS"; statTeamBat!(10)
                Locate 15, 10: Print " "

            Case 14
                Locate 16, 10: Print Space$(10)
                Locate 16, 1: Input "14. TM PO"; statTeamBat!(16)
                Locate 16, 10: Print " "

            Case 15
                Locate 17, 9: Print Space$(10)
                Locate 17, 1: Input "15. TM A"; statTeamBat!(17)
                Locate 17, 9: Print " "

            Case 16
                Locate 18, 10: Print Space$(10)
                Locate 18, 1: Input "16. TM SH"; statTeamBat!(18)
                Locate 18, 10: Print " "

            Case 17
                Locate 19, 10: Print Space$(10)
                Locate 19, 1: Input "17. TM SF"; statTeamBat!(19)
                Locate 19, 10: Print " "

            Case 18
                Locate 20, 12: Print Space$(10)
                Locate 20, 1: Input "18. TM GIDP"; statTeamBat!(15)
                Locate 20, 12: Print " "

            Case 19
                Locate 21, 10: Print Space$(10)
                Locate 21, 1: Input "19. TM LOB"; statTeamBat!(12)
                Locate 21, 10: Print " "

        End Select

    Loop Until B9 = 0

    '---------------
    ' Pitching Stats
    '---------------
    Cls
    
    X$ = "TEAM PITCHING STATS"
    Call TITLE_EDITSTATS(2, X$)

    Locate 3, 1
    Print " 1. WON "; teamWins
    Print " 2. LOST "; teamLosses
    Print " 3. TM SV "; statTeamPit!(12)
    Print " 4. (TM IP X 3) "; statTeamPit!(0)
    Print " 5. TM HITS "; statTeamPit!(1)
    Print " 6. TM BB "; statTeamPit!(4)
    Print " 7. TM SO "; statTeamPit!(5)
    Print " 8. TM CG "; statTeamPit!(10)
    Print " 9. TM RUNS "; statTeamPit!(2)
    Print "10. TM E R "; statTeamPit!(3)
    Print "11. TM SHT OUT "; statTeamPit!(11)
    Print "12. TM STARTS "; statTeamPit!(9)
    Print "13. TM HRA "; statTeamPit!(14)
    Print "14. TM DP TURNED "; statTeamPit!(13)
    Print "15. TM SH "; statTeamPit!(15)
    Print "16. TM SF "; statTeamPit!(16)
    Print "17. TM RUNNERS INHERITED (RI) "; statTeamPit!(17)
    Print "18. TM RI SCORED "; statTeamPit!(18)
    Print "19. TM BLOWN SAVES "; statTeamPit!(19)
    Print "20. TM QUALITY STARTS "; statTeamPit!(21)

    Do

        Do
            Locate 25, 29
            Print Space$(10)
            Locate 25, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 20

        Select Case B9

            Case 1
                Locate 3, 8: Print Space$(10)
                Locate 3, 1: Input " 1. WON"; teamWins
                Locate 3, 8: Print " "
                
            Case 2
                Locate 4, 9: Print Space$(10)
                Locate 4, 1: Input " 2. LOST"; teamLosses
                Locate 4, 9: Print " "

            Case 3
                Locate 5, 10: Print Space$(10)
                Locate 5, 1: Input " 3. TM SV"; statTeamPit!(12)
                Locate 5, 10: Print " "

            Case 4
                Locate 6, 16: Print Space$(10)
                Locate 6, 1: Input " 4. (TM IP X 3)"; statTeamPit!(0)
                Locate 6, 16: Print " "

            Case 5
                Locate 7, 12: Print Space$(10)
                Locate 7, 1: Input " 5. TM HITS"; statTeamPit!(1)
                Locate 7, 12: Print " "

            Case 6
                Locate 8, 10: Print Space$(10)
                Locate 8, 1: Input " 6. TM BB"; statTeamPit!(4)
                Locate 8, 10: Print " "

            Case 7
                Locate 9, 10: Print Space$(10)
                Locate 9, 1: Input " 7. TM SO"; statTeamPit!(5)
                Locate 9, 10: Print " "

            Case 8
                Locate 10, 10: Print Space$(10)
                Locate 10, 1: Input " 8. TM CG"; statTeamPit!(10)
                Locate 10, 10: Print " "

            Case 9
                Locate 11, 12: Print Space$(10)
                Locate 11, 1: Input " 9. TM RUNS"; statTeamPit!(2)
                Locate 11, 12: Print " "

            Case 10
                Locate 12, 11: Print Space$(10)
                Locate 12, 1: Input "10. TM E R"; statTeamPit!(3)
                Locate 12, 11: Print " "

            Case 11
                Locate 13, 14: Print Space$(10)
                Locate 13, 1: Input "11. TM SH OUT"; statTeamPit!(11)
                Locate 13, 14: Print " "

            Case 12
                Locate 14, 14: Print Space$(10)
                Locate 14, 1: Input "12. TM STARTS"; statTeamPit!(9)
                Locate 14, 14: Print " "

            Case 13
                Locate 15, 11: Print Space$(10)
                Locate 15, 1: Input "13. TM HRA"; statTeamPit!(14)
                Locate 15, 11: Print " "

            Case 14
                Locate 16, 17: Print Space$(10)
                Locate 16, 1: Input "14. TM DP TURNED"; statTeamPit!(13)
                Locate 16, 17: Print " "

            Case 15
                Locate 17, 10: Print Space$(10)
                Locate 17, 1: Input "15. TM SH"; statTeamPit!(15)
                Locate 17, 10: Print " "

            Case 16
                Locate 18, 10: Print Space$(10)
                Locate 18, 1: Input "16. TM SF"; statTeamPit!(16)
                Locate 18, 10: Print " "

            Case 17
                Locate 19, 30: Print Space$(10)
                Locate 19, 1: Input "17. TM RUNNERS INHERITED (RI)"; statTeamPit!(17)
                Locate 19, 30: Print " "

            Case 18
                Locate 20, 17: Print Space$(10)
                Locate 20, 1: Input "18. TM RI SCORED"; statTeamPit!(18)
                Locate 20, 17: Print " "

            Case 19
                Locate 21, 19: Print Space$(10)
                Locate 21, 1: Input "19. TM BLOWN SAVES"; statTeamPit!(19)
                Locate 21, 19: Print " "

            Case 20
                Locate 22, 22: Print Space$(10)
                Locate 22, 1: Input "20. TM QUALITY STARTS"; statTeamPit!(21)
                Locate 22, 22: Print " "

        End Select

    Loop Until B9 = 0

    Call ChangeBatters

    Call ChangePitchers

    ' Save the File
    Open diskPaths$(1) + teamName$ For Output As #1

    Print #1, teamName$
    Print #1, teamWins
    Print #1, teamLosses

    For I = 0 To 22: 
        Print #1, batterNames$(I)
        Print #1, statAtBats(I)
    Next I

    For I = 0 To 21: 
        Print #1, pitcherNames$(I)
        Print #1, statInnPitch(I)
    Next I

    For I = 0 To 22: 
        For I1 = 0 To 21: 
            Print #1, statBatter(I, I1)
        Next I1
    Next I

    For I = 0 To 21: 
        For I1 = 0 To 41: 
            Print #1, statPitcher(I, I1)
        Next I1
    Next I

    Print #1, statTeamDP
    Print #1, statOppDP

    For I = 0 To 21: 
        Print #1, statTeamBat!(I)
    Next I

    For I = 0 To 22: 
        Print #1, statTeamPit!(I)
    Next I

    Close #1

End Sub


'----------------------------------------
'        ChangeBatters Subroutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for specifically changing stats for
'team batters.
Sub ChangeBatters ()

    Shared batterNames$()
    Shared statBatter()

    Do

        Cls
        X$ = "CHANGE BATTERS"
        Call TITLE_EDITSTATS(2, X$)

        For I = 0 To 10
            Locate I + 3, 5
            Color 14
            Print Using "##"; I + 1;
            Color 15
            Print " "; batterNames$(I); Tab(40);
            Color 14
            Print Using "##"; I + 13;
            Color 15
            Print " "; batterNames$(I + 12)
        Next

        Locate , 5
        Color 14
        Print Using "##"; 12;
        Color 15
        Print " "; batterNames$(11)

        Do
            Locate 17, 5
            Input "# TO CHANGE (1-23) (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 23

        If I > 0 Then

            I = I - 1

            Cls
            Call TITLE_EDITSTATS(2, X$)

            Locate 3, 1
            Print " 1. NAME  "; batterNames$(I)
            Print " 2. AB "; statBatter(I, 0)
            Print " 3. RUNS "; statBatter(I, 2)
            Print " 4. HITS "; statBatter(I, 1)
            Print " 5. DBL "; statBatter(I, 4)
            Print " 6. TRPL "; statBatter(I, 5)
            Print " 7. HR "; statBatter(I, 6)
            Print " 8. RBI "; statBatter(I, 3)
            Print " 9. BB "; statBatter(I, 7)
            Print "10. SO "; statBatter(I, 8)
            Print "11. ERR "; statBatter(I, 11)
            Print "12. SB "; statBatter(I, 9)
            Print "13. CS "; statBatter(I, 10)
            Print "14. G "; statBatter(I, 14)
            Print "15. CHS "; statBatter(I, 12)
            Print "16. LHS "; statBatter(I, 13)
            Print "17. GIDP "; statBatter(I, 15)
            Print "18. PO "; statBatter(I, 16)
            Print "19. A "; statBatter(I, 17)
            Print "20. SH "; statBatter(I, 18)
            Print "21. SF "; statBatter(I, 19)

            Do

                Do
                    Locate 25, 29
                    Print Space$(10)
                    Locate 25, 5
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 21

                Locate 2, 20: Print "BATTER - "; I + 1
                Locate , 1

                Select Case B9

                    Case 1
                        Locate 3, 1: Input " 1. NAME"; batterNames$(I)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 7: Print Space$(10)
                        Locate 4, 1: Input " 2. AB"; statBatter(I, 0)
                        Locate 4, 7: Print " "

                    Case 3
                        Locate 5, 9: Print Space$(10)
                        Locate 5, 1: Input " 3. RUNS"; statBatter(I, 2)
                        Locate 5, 9: Print " "

                    Case 4
                        Locate 6, 9: Print Space$(10)
                        Locate 6, 1: Input " 4. HITS"; statBatter(I, 1)
                        Locate 6, 9: Print " "

                    Case 5
                        Locate 7, 8: Print Space$(10)
                        Locate 7, 1: Input " 5. DBL"; statBatter(I, 4)
                        Locate 7, 8: Print " "

                    Case 6
                        Locate 8, 9: Print Space$(10)
                        Locate 8, 1: Input " 6. TRPL"; statBatter(I, 5)
                        Locate 8, 9: Print " "

                    Case 7
                        Locate 9, 7: Print Space$(10)
                        Locate 9, 1: Input " 7. HR"; statBatter(I, 6)
                        Locate 9, 7: Print " "

                    Case 8
                        Locate 10, 8: Print Space$(10)
                        Locate 10, 1: Input " 8. RBI"; statBatter(I, 3)
                        Locate 10, 8: Print " "

                    Case 9
                        Locate 11, 7: Print Space$(10)
                        Locate 11, 1: Input " 9. BB"; statBatter(I, 7)
                        Locate 11, 7: Print " "

                    Case 10
                        Locate 12, 7: Print Space$(10)
                        Locate 12, 1: Input "10. SO"; statBatter(I, 8)
                        Locate 12, 7: Print " "

                    Case 11
                        Locate 13, 8: Print Space$(10)
                        Locate 13, 1: Input "11. ERR"; statBatter(I, 11)
                        Locate 13, 8: Print " "

                    Case 12
                        Locate 14, 7: Print Space$(10)
                        Locate 14, 1: Input "12. SB"; statBatter(I, 9)
                        Locate 14, 7: Print " "

                    Case 13
                        Locate 15, 7: Print Space$(10)
                        Locate 15, 1: Input "13. CS"; statBatter(I, 10)
                        Locate 15, 7: Print " "

                    Case 14
                        Locate 16, 6: Print Space$(10)
                        Locate 16, 1: Input "14. G"; statBatter(I, 14)
                        Locate 16, 6: Print " "

                    Case 15
                        Locate 17, 8: Print Space$(10)
                        Locate 17, 1: Input "15. CHS"; statBatter(I, 12)
                        Locate 17, 8: Print " "

                    Case 16
                        Locate 18, 8: Print Space$(10)
                        Locate 18, 1: Input "16. LHS"; statBatter(I, 13)
                        Locate 18, 8: Print " "

                    Case 17
                        Locate 19, 9: Print Space$(10)
                        Locate 19, 1: Input "17. GIDP"; statBatter(I, 15)
                        Locate 19, 9: Print " "

                    Case 18
                        Locate 20, 7: Print Space$(10)
                        Locate 20, 1: Input "18. PO"; statBatter(I, 16)
                        Locate 20, 7: Print " "

                    Case 19
                        Locate 21, 7: Print Space$(10)
                        Locate 21, 1: Input "19. A"; statBatter(I, 17)
                        Locate 21, 7: Print " "

                    Case 20
                        Locate 22, 7: Print Space$(10)
                        Locate 22, 1: Input "20. SH"; statBatter(I, 18)
                        Locate 22, 7: Print " "

                    Case 21
                        Locate 23, 7: Print Space$(10)
                        Locate 23, 1: Input "21. SF"; statBatter(I, 19)
                        Locate 23, 7: Print " "

                End Select

            Loop Until B9 = 0

        End If

    Loop Until I = 0

End Sub

'----------------------------------------
'       ChangePitchers Subroutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for specifically changing stats for
'team pitchers.
Sub ChangePitchers ()

    Shared batterNames$(), pitcherNames$()
    Shared statAtBats(), statBatter(), statInnPitch(), statPitcher()
    Shared statTeamBat!(), statTeamPit!()

    Do


        Cls
        X$ = "CHANGE PITCHERS"
        Call TITLE_EDITSTATS(2, X$)

        For I = 0 To 10
            Locate I + 3, 5
            Color 14
            Print Using "##"; I + 1;
            Color 15
            Print " "; pitcherNames$(I); Tab(40);
            Color 14
            Print Using "##"; I + 12;
            Color 15
            Print " "; pitcherNames$(I + 11)
        Next

        Do
            Locate 16, 5
            Input "# TO CHANGE (1-22) (0 TO CONTINUE)"; pitcher
        Loop Until pitcher >= 0 And pitcher <= 23

        If pitcher > 0 Then

            idx = pitcher - 1

            Cls
            Call TITLE_EDITSTATS(2, X$)

            Locate 3, 1
            Print " 1. NAME  "; pitcherNames$(idx)
            Print " 2. WON "; statPitcher(idx, 6);
            Locate, 40: Print "23. QUALITY STARTS "; statPitcher(idx, 21)
            Print " 3. LOST "; statPitcher(idx, 7);
            Locate , 40: Print "24. AB "; statPitcher(idx, 23)
            Print " 4. SV "; statPitcher(idx, 12);
            Locate , 40: Print "25. H "; statPitcher(idx, 24)
            Print " 5. (IP X 3) "; statPitcher(idx, 0);
            Locate , 40: Print "26. R "; statPitcher(idx, 25)
            Print " 6. HITS "; statPitcher(idx, 1);
            Locate , 40: Print "27. RBI "; statPitcher(idx, 26)
            Print " 7. BB "; statPitcher(idx, 4);
            Locate , 40: Print "28. DBL "; statPitcher(idx, 27)
            Print " 8. SO "; statPitcher(idx, 5);
            Locate , 40: Print "29. TRPL "; statPitcher(idx, 28)
            Print " 9. CG "; statPitcher(idx, 10);
            Locate , 40: Print "30. HR "; statPitcher(idx, 29)
            Print "10. RUNS "; statPitcher(idx, 2);
            Locate , 40: Print "31. BB "; statPitcher(idx, 30)
            Print "11. E R "; statPitcher(idx, 3);
            Locate , 40: Print "32. K "; statPitcher(idx, 31)
            Print "12. SH OUT "; statPitcher(idx, 11);
            Locate , 40: Print "33. SB "; statPitcher(idx, 32)
            Print "13. GAMES "; statPitcher(idx, 8);
            Locate , 40: Print "34. CS "; statPitcher(idx, 33)
            Print "14. STARTS "; statPitcher(idx, 9);
            Locate , 40: Print "35. ERR "; statPitcher(idx, 34)
            Print "15. HR ALL "; statPitcher(idx, 14);
            Locate , 40: Print "36. CHS "; statPitcher(idx, 35)
            Print "16. GIDP "; statPitcher(idx, 13);
            Locate , 40: Print "37. LHS "; statPitcher(idx, 36)
            Print "17. SH "; statPitcher(idx, 15);
            Locate , 40: Print "38. GIDP "; statPitcher(idx, 37)
            Print "18. SF "; statPitcher(idx, 16);
            Locate , 40: Print "39. PO "; statPitcher(idx, 38)
            Print "19. RUNNERS INHERITED (RI) "; statPitcher(idx, 17);
            Locate , 40: Print "40. A "; statPitcher(idx, 39)
            Print "20. RI SCORED "; statPitcher(idx, 18);
            Locate , 40: Print "41. SH "; statPitcher(idx, 40)
            Print "21. BLOWN SAVES "; statPitcher(idx, 19);
            Locate , 40: Print "42. SF "; statPitcher(idx, 41)
            Print "22. RUN SUPPORT "; statPitcher(idx, 20)

            Do

                Do
                    Locate 25, 29
                    Print Space$(10)
                    Locate 25, 5
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 42

                Select Case B9

                    Case 1
                        Locate 3, 1: Input " 1. NAME"; pitcherNames$(idx)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 8: Print Space$(10)
                        Locate 4, 1: Input " 2. WON"; statPitcher(idx, 6)
                        Locate 4, 8: Print " "

                    Case 3
                        Locate 5, 9: Print Space$(10)
                        Locate 5, 1: Input " 3. LOST"; statPitcher(idx, 7)
                        Locate 5, 9: Print " "

                    Case 4
                        Locate 6, 7: Print Space$(10)
                        Locate 6, 1: Input " 4. SV"; statPitcher(idx, 12)
                        Locate 6, 7: Print " "

                    Case 5
                        Locate 7, 13: Print Space$(10)
                        Locate 7, 1: Input " 5. (IP X 3)"; statPitcher(idx, 0)
                        Locate 7, 13: Print " "

                    Case 6
                        Locate 8, 9: Print Space$(10)
                        Locate 8, 1: Input " 6. HITS"; statPitcher(idx, 1)
                        Locate 8, 9: Print " "

                    Case 7
                        Locate 9, 7: Print Space$(10)
                        Locate 9, 1: Input " 7. BB"; statPitcher(idx, 4)
                        Locate 9, 7: Print " "

                    Case 8
                        Locate 10, 7: Print Space$(10)
                        Locate 10, 1: Input " 8. SO"; statPitcher(idx, 5)
                        Locate 10, 7: Print " "

                    Case 9
                        Locate 11, 7: Print Space$(10)
                        Locate 11, 1: Input " 9. CG"; statPitcher(idx, 10)
                        Locate 11, 7: Print " "

                    Case 10
                        Locate 12, 9: Print Space$(10)
                        Locate 12, 1: Input "10. RUNS"; statPitcher(idx, 2)
                        Locate 12, 9: Print " "

                    Case 11
                        Locate 13, 8: Print Space$(10)
                        Locate 13, 1: Input "11. E R"; statPitcher(idx, 3)
                        Locate 13, 8: Print " "

                    Case 12
                        Locate 14, 11: Print Space$(10)
                        Locate 14, 1: Input "12. SH OUT"; statPitcher(idx, 11)
                        Locate 14, 11: Print " "

                    Case 13
                        Locate 15, 10: Print Space$(10)
                        Locate 15, 1: Input "13. GAMES"; statPitcher(idx, 8)
                        Locate 15, 10: Print " "

                    Case 14
                        Locate 16, 11: Print Space$(10)
                        Locate 16, 1: Input "14. STARTS"; statPitcher(idx, 9)
                        Locate 16, 11: Print " "

                    Case 15
                        Locate 17, 11: Print Space$(10)
                        Locate 17, 1: Input "15. HR ALL"; statPitcher(idx, 14)
                        Locate 17, 11: Print " "

                    Case 16
                        Locate 18, 9: Print Space$(10)
                        Locate 18, 1: Input "16. GIDP"; statPitcher(idx, 13)
                        Locate 18, 9: Print " "

                    Case 17
                        Locate 19, 7: Print Space$(10)
                        Locate 19, 1: Input "17. SH"; statPitcher(idx, 15)
                        Locate 19, 7: Print " "

                    Case 18
                        Locate 20, 7: Print Space$(10)
                        Locate 20, 1: Input "18. SF"; statPitcher(idx, 16)
                        Locate 20, 7: Print " "

                    Case 19
                        Locate 21, 27: Print Space$(10)
                        Locate 21, 1: Input "19. RUNNERS INHERITED (RI)"; statPitcher(idx, 17)
                        Locate 21, 27: Print " "

                    Case 20
                        Locate 22, 14: Print Space$(10)
                        Locate 22, 1: Input "20. RI SCORED"; statPitcher(idx, 18)
                        Locate 22, 14: Print " "

                    Case 21
                        Locate 23, 16: Print Space$(10)
                        Locate 23, 1: Input "21. BLOWN SAVES"; statPitcher(idx, 19)
                        Locate 23, 16: Print " "

                    Case 22
                        Locate 24, 16: Print Space$(10)
                        Locate 24, 1: Input "22. RUN SUPPORT"; statPitcher(idx, 20)
                        Locate 24, 16: Print " "

                    Case 23
                        Locate 25, 19: Print Space$(10)
                        Locate 25, 1: Input "23. QUALITY STARTS"; statPitcher(idx, 21)
                        Locate 25, 19: Print " "

                    Case 24
                        Locate 4, 47: Print Space$(10)
                        Locate 4, 40: Input "24. AB"; statPitcher(idx, 23)
                        Locate 4, 47: Print " "

                    Case 25
                        Locate 5, 46: Print Space$(10)
                        Locate 5, 40: Input "25. H"; statPitcher(idx, 24)
                        Locate 5, 46: Print " "

                    Case 26
                        Locate 6, 46: Print Space$(10)
                        Locate 6, 40: Input "26. R"; statPitcher(idx, 25)
                        Locate 6, 46: Print " "

                    Case 27
                        Locate 7, 48: Print Space$(10)
                        Locate 7, 40: Input "27. RBI"; statPitcher(idx, 26)
                        Locate 7, 48: Print " "

                    Case 28
                        Locate 8, 48: Print Space$(10)
                        Locate 8, 40: Input "28. DBL"; statPitcher(idx, 27)
                        Locate 8, 48: Print " "

                    Case 29
                        Locate 9, 49: Print Space$(10)
                        Locate 9, 40: Input "29. TRPL"; statPitcher(idx, 28)
                        Locate 9, 49: Print " "

                    Case 30
                        Locate 10, 47: Print Space$(10)
                        Locate 10, 40: Input "30. HR"; statPitcher(idx, 29)
                        Locate 10, 47: Print " "

                    Case 31
                        Locate 11, 47: Print Space$(10)
                        Locate 11, 40: Input "31. BB"; statPitcher(idx, 30)
                        Locate 11, 47: Print " "

                    Case 32
                        Locate 12, 46: Print Space$(10)
                        Locate 12, 40: Input "32. K"; statPitcher(idx, 31)
                        Locate 12, 46: Print " "

                    Case 33
                        Locate 13, 47: Print Space$(10)
                        Locate 13, 40: Input "33. SB"; statPitcher(idx, 32)
                        Locate 13, 47: Print " "

                    Case 34
                        Locate 14, 47: Print Space$(10)
                        Locate 14, 40: Input "34. CS"; statPitcher(idx, 33)
                        Locate 14, 47: Print " "

                    Case 35
                        Locate 15, 48: Print Space$(10)
                        Locate 15, 40: Input "35. ERR"; statPitcher(idx, 34)
                        Locate 15, 48: Print " "

                    Case 36
                        Locate 16, 48: Print Space$(10)
                        Locate 16, 40: Input "36. CHS"; statPitcher(idx, 35)
                        Locate 16, 48: Print " "

                    Case 37
                        Locate 17, 48: Print Space$(10)
                        Locate 17, 40: Input "37. LHS"; statPitcher(idx, 36)
                        Locate 17, 48: Print " "

                    Case 38
                        Locate 18, 49: Print Space$(10)
                        Locate 18, 40: Input "38. GIDP"; statPitcher(idx, 37)
                        Locate 18, 49: Print " "

                    Case 39
                        Locate 19, 47: Print Space$(10)
                        Locate 19, 40: Input "39. PO"; statPitcher(idx, 38)
                        Locate 19, 47: Print " "

                    Case 40
                        Locate 20, 46: Print Space$(10)
                        Locate 20, 40: Input "40. A"; statPitcher(idx, 39)
                        Locate 20, 46: Print " "

                    Case 41
                        Locate 21, 47: Print Space$(10)
                        Locate 21, 40: Input "41. SH"; statPitcher(idx, 40)
                        Locate 21, 47: Print " "

                    Case 42
                        Locate 22, 47: Print Space$(10)
                        Locate 22, 40: Input "42. SF"; statPitcher(idx, 41)
                        Locate 22, 47: Print " "

                End Select

            Loop Until B9 = 0

        End If

    Loop Until pitcher = 0

End Sub


'----------------------------------------
'    ChangeScheduleStats Subroutine
'----------------------------------------
'This subroutine inputs stats from the
'chosen Stat Schedule file and then gives 
'the user an interface for modifying the 
'schedule results.
Sub ChangeScheduleStats (teamName$)

    Shared locIndicator$(), oppName$()
    Shared losePitcher$(), winPitcher$()

    Shared nbrInnings(), oppScore(), teamScore()

    If Not _FileExists(diskPaths$(1) + teamName$ + "S") Then

        MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call ReadScheduleStats (teamName$, nbrGames)

        Do

            Color 15, 0
            Cls

            X$ = "TEAM SCHEDULE STATS"
            Call TITLE_EDITSTATS(2, X$)

            homeScore = 0
            oppTeam$ = ""
            oppPts = 0
            homeOrAway$ = ""
            winPitch$ = ""
            losePitch$ = ""
            numInn = 0

            Do

                Locate 2, 1: Print "1. SCORE FOR "; teamName$; "  "; homeScore
                Locate 3, 1: Print "2. OPPONENT'S NAME  "; oppTeam$
                Locate 4, 1: Print "3. OPPONENT'S SCORE  "; oppPts
                Locate 5, 1: Print "4. (H)OME OR (A)WAY  "; homeOrAway$
                Locate 6, 1: Print "5. WINNING PITCHER  "; winPitch$
                Locate 7, 1: Print "6. LOSING PITCHER  "; losePitch$
                Locate 8, 1: Print "7. NUMBER OF INNINGS  "; numInn

                Do
                    Locate 10, 1: Print "ENTER # TO CHANGE (1-7) OR 0 TO CONTINUE"
                    J$ = GetKeyPress$
                    D = Val(J$)
                Loop Until D >= 0 And D <= 7


                Select Case D

                    Case 1:
                        Locate 2, 1: Print "1. SCORE FOR "; teamName$; " ";
                        Input homeScore
                        Locate 2, 13 + Len(teamName$): Print " "

                    Case 2:
                        Locate 3, 1: Input "2. OPPONENT'S NAME"; oppTeam$
                        Locate 3, 20: Print " "

                    Case 3:
                        Locate 4, 20: Print Space$(10)
                        Locate 4, 1: Input "3. OPPONENT'S SCORE"; oppPts
                        Locate 4, 20: Print " "

                    Case 4:
                        Do
                            Locate 5, 1: Input "4. (H)OME OR (A)WAY"; homeOrAway$
                            Locate 5, 20: Print " "
                        Loop Until homeOrAway$ = "A" Or homeOrAway$ = "H"

                    Case 5:
                        Locate 6, 1: Input "5. WINNING PITCHER"; winPitch$
                        Locate 6, 21: Print " "

                    Case 6:
                        Locate 7, 1: Input "6. LOSING PITCHER"; losePitch$
                        Locate 7, 20: Print " "

                    Case 7:
                        Locate 8, 21: Print Space$(10)
                        Locate 8, 1: Input "7. NUMBER OF INNINGS"; numInn
                        Locate 8, 21: Print " "

                End Select

            Loop Until D = 0

            nbrGames = nbrGames + 1
            teamScore(nbrGames) = homeScore
            oppName$(nbrGames) = oppTeam$
            oppScore(nbrGames) = oppPts
            locIndicator$(nbrGames) = homeOrAway$
            winPitcher$(nbrGames) = winPitch$
            losePitcher$(nbrGames) = losePitch$
            nbrInnings(nbrGames) = numInn

            Do
                Locate 12, 1
                Print "ADD ANOTHER GAME? (Y/N)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Loop Until UCase$(I$) = "N"

        Call SaveScheduleStats(teamName$, nbrGames)

        Cls

    End If 'Done checking if file exists

End Sub
