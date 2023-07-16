Sub SINPUT ()

    _Title "Full Count Baseball - Stats Input"

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
        Row% = 8
        Column% = 23

        Call box0(7, 22, 11, 60, 2, 15, 1)

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

End Sub

'----------------------------------------
'     EditStatsSchedules Subroutine
'----------------------------------------
'...explanation...
Sub EditStatsSchedules (action%)

    Shared statB$(), statP$()
    Shared statB%(), careerB0%()
    Shared statP%(), careerP0%()
    Shared statT%(), statT1%()

    Call GetTeam_SINPUT(T$, yearNumber$, numberTeams)

    statB$ = RTrim$(T$)

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

    Z$ = Mid$(statB$, 4, QQ) + "." + Left$(statB$, 2)

    If action% = 2 Then

        Call ChangeSchedule(Z$)

    Else
        'action% = 1
        If Not _FileExists(diskPaths$(1) + Z$) Then

            Locate 10, 8
            Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call ModifyTeamStats(Z$)

        End If

    End If

End Sub

'----------------------------------------
'       ModifyTeamStats Subroutine
'----------------------------------------
'...explanation...
Sub ModifyTeamStats (Z$)

    Shared statB$(), statP$()
    Shared careerB0%(), careerP0%()
    Shared teamRatings%()
    Shared statB%(), statP%(), statT%(), statT1%()

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

    '---------------
    ' Batting Stats
    '---------------
    Color 15, 0
    Cls
    
    X$ = "TEAM BATTING STATS"
    Call TITLE_STAT_HEADER(X$)

    Locate 2, 1
    Print "1. TEAM NAME (EX. ST_LOUIS.64) "; Z$
    Print "2. TM AB "; statT%(0)
    Print "3. TM RUNS "; statT%(2)
    Print "4. TM HITS "; statT%(1)
    Print "5. TM DBL "; statT%(4)
    Print "6. TM TRPL "; statT%(5)
    Print "7. TM HR "; statT%(6)
    Print "8. TM RBI "; statT%(3)
    Print "9. TM BB "; statT%(7)
    Print "10. TM SO "; statT%(8)
    Print "11. TM ERR "; statT%(11)
    Print "12. TM SB "; statT%(9)
    Print "13. TM CS "; statT%(10)
    Print "14. TM PO "; statT%(16)
    Print "15. TM A  "; statT%(17)
    Print "16. TM SH "; statT%(18)
    Print "17. TM SF "; statT%(19)
    Print "18. TM GIDP "; statT%(15)
    Print "19. TM LOB "; statT%(12)

    Do

        Do
            Locate 23, 1
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 19

        Select Case B9

            Case 1
                Locate 2: Input "1. TEAM NAME (EX. ST_LOUIS.64) "; Z$

            Case 2
                Locate 3: Input "2. TM AB "; statT%(0)

            Case 3
                Locate 4: Input "3. TM RUNS "; statT%(2)

            Case 4
                Locate 5: Input "4. TM HITS "; statT%(1)

            Case 5
                Locate 6: Input "5. TM DBL "; statT%(4)

            Case 6
                Locate 7: Input "6. TM TRPL "; statT%(5)

            Case 7
                Locate 8: Input "7. TM HR "; statT%(6)

            Case 8
                Locate 9: Input "8. TM RBI "; statT%(3)

            Case 9
                Locate 10: Input "9. TM BB "; statT%(7)

            Case 10
                Locate 11: Input "10. TM SO "; statT%(8)

            Case 11
                Locate 12: Input "11. TM ERR "; statT%(11)

            Case 12
                Locate 13: Input "12. TM SB "; statT%(9)

            Case 13
                Locate 14: Input "13. TM CS "; statT%(10)

            Case 14
                Locate 15: Input "14. TM PO "; statT%(16)

            Case 15
                Locate 16: Input "15. TM A  "; statT%(17)

            Case 16
                Locate 17: Input "16. TM SH "; statT%(18)

            Case 17
                Locate 18: Input "17. TM SF "; statT%(19)

            Case 18
                Locate 19: Input "18. TM GIDP "; statT%(15)

            Case 19
                Locate 20: Input "19. TM LOB "; statT%(12)

        End Select

    Loop Until B9 = 0

    '---------------
    ' Pitching Stats
    '---------------
    Cls
    X$ = "TEAM PITCHING STATS"
    Call TITLE_STAT_HEADER(X$)

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

    Do

        Do
            Locate 23, 1
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 20

        Select Case B9

            Case 1
                Locate 2: Input "1. WON "; W2%

            Case 2
                Locate 3: Input "2. LOST "; L2%

            Case 3
                Locate 4: Input "3. TM SV "; statT1%(12)

            Case 4
                Locate 5: Input "4. (TM IP X 3) "; statT1%(0)

            Case 5
                Locate 6: Input "5. TM HITS "; statT1%(1)

            Case 6
                Locate 7: Input "6. TM BB "; statT1%(4)

            Case 7
                Locate 8: Input "7. TM SO "; statT1%(5)

            Case 8
                Locate 9: Input "8. TM CG "; statT1%(10)

            Case 9
                Locate 10: Input "9. TM RUNS "; statT1%(2)

            Case 10
                Locate 11: Input "10. TM E R "; statT1%(3)

            Case 11
                Locate 12: Input "11. TM SH OUT "; statT1%(11)

            Case 12
                Locate 13: Input "12. TM STARTS "; statT1%(9)

            Case 13
                Locate 14: Input "13. TM HRA "; statT1%(14)

            Case 14
                Locate 15: Input "14. TM DP TURNED "; statT1%(13)

            Case 15
                Locate 16: Input "15. TM SH "; statT1%(15)

            Case 16
                Locate 17: Input "16. TM SF "; statT1%(16)

            Case 17
                Locate 18: Input "17. TM RUNNERS INHERITED (RI) "; statT1%(17)

            Case 18
                Locate 19: Input "18. TM RI SCORED "; statT1%(18)

            Case 19
                Locate 20: Input "19. TM BLOWN SAVES "; statT1%(19)

            Case 20
                Locate 21: Input "20. TM QUALITY STARTS "; statT1%(21)

        End Select

    Loop Until B9 = 0

    Call ChangeBatters(Z$)

    Call ChangePitchers(D0%, D1%, L2%, W2%, Z$)

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
'     ChangeBatters Subroutine
'----------------------------------------
'...explanation...
Sub ChangeBatters (Z$)

    Shared statB$()
    Shared careerB0%()

    Do

        Cls
        X$ = "CHANGE BATTERS"
        Call TITLE_STAT_HEADER(X$)
        Print

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
            Input "# TO CHANGE (1-23) (0 TO CONTINUE) "; I
        Loop Until I >= 0 And I <= 23

        If I > 0 Then

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

            Do

                Do
                    Locate 23, 1
                    Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Loop Until B9 >= 0 And B9 <= 21

                Locate 1, 30: Print "BATTER - "; I + 1
                Locate , 1

                Select Case B9

                    Case 1
                        Locate 1: Input "1. NAME "; statB$(I)

                    Case 2
                        Locate 2: Input "2. AB "; careerB0%(I, 0)

                    Case 3
                        Locate 3: Input "3. RUNS "; careerB0%(I, 2)

                    Case 4
                        Locate 4: Input "4. HITS "; careerB0%(I, 1)

                    Case 5
                        Locate 5: Input "5. DBL "; careerB0%(I, 4)

                    Case 6
                        Locate 6: Input "6. TRPL "; careerB0%(I, 5)

                    Case 7
                        Locate 7: Input "7. HR "; careerB0%(I, 6)

                    Case 8
                        Locate 8: Input "8. RBI "; careerB0%(I, 3)

                    Case 9
                        Locate 9: Input "9. BB "; careerB0%(I, 7)

                    Case 10
                        Locate 10: Input "10. SO "; careerB0%(I, 8)

                    Case 11
                        Locate 11: Input "11. ERR "; careerB0%(I, 11)

                    Case 12
                        Locate 12: Input "12. SB "; careerB0%(I, 9)

                    Case 13
                        Locate 13: Input "13. CS "; careerB0%(I, 10)

                    Case 14
                        Locate 14: Input "14.  G "; careerB0%(I, 14)

                    Case 15
                        Locate 15: Input "15. CHS "; careerB0%(I, 12)

                    Case 16
                        Locate 16: Input "16. LHS "; careerB0%(I, 13)

                    Case 17
                        Locate 17: Input "17. GIDP "; careerB0%(I, 15)

                    Case 18
                        Locate 18: Input "18. PO "; careerB0%(I, 16)

                    Case 19
                        Locate 19: Input "19. A  "; careerB0%(I, 17)

                    Case 20
                        Locate 20: Input "20. SH "; careerB0%(I, 18)

                    Case 21
                        Locate 21: Input "21. SF "; careerB0%(I, 19)

                End Select

            Loop Until B9 = 0

        End If

    Loop Until I = 0

End Sub

'----------------------------------------
'    ChangePitchers Subroutine
'----------------------------------------
'...explanation...
Sub ChangePitchers (D0%, D1%, L2%, W2%, Z$)

    Shared statB$(), statP$()
    Shared statB%(), careerB0%(), statP%(), careerP0%()
    Shared statT%(), statT1%()

    Do


        Cls
        X$ = "CHANGE PITCHERS"
        Call TITLE_STAT_HEADER(X$)
        Print

        For I = 0 To 10
            Locate I + 5, 5: Print Using "##"; I + 1;
            Print " "; statP$(I); Tab(40);
            Print Using "##"; I + 12;: Print " "; statP$(I + 11)
        Next

        Print

        Do
            Input "# TO CHANGE (1-22) (0 TO CONTINUE) "; pitcher
        Loop Until pitcher >= 0 And pitcher <= 23

        If pitcher > 0 Then

            idx = pitcher - 1

            Cls

            Locate 1: Print "1. NAME "; statP$(idx)
            Locate 2: Print "2. WON "; careerP0%(idx, 6);
            Locate, 40: Print "23. QUALITY STARTS "; careerP0%(idx, 21)
            Locate 3: Print "3. LOST "; careerP0%(idx, 7);
            Locate , 40: Print "24. AB "; careerP0%(idx, 23)
            Locate 4: Print "4. SV "; careerP0%(idx, 12);
            Locate , 40: Print "25. H "; careerP0%(idx, 24)
            Locate 5: Print "5. (IP X 3) "; careerP0%(idx, 0);
            Locate , 40: Print "26. R "; careerP0%(idx, 25)
            Locate 6: Print "6. HITS "; careerP0%(idx, 1);
            Locate , 40: Print "27. RBI "; careerP0%(idx, 26)
            Locate 7: Print "7. BB "; careerP0%(idx, 4);
            Locate , 40: Print "28. DBL "; careerP0%(idx, 27)
            Locate 8: Print "8. SO "; careerP0%(idx, 5);
            Locate , 40: Print "29. TRPL "; careerP0%(idx, 28)
            Locate 9: Print "9. CG "; careerP0%(idx, 10);
            Locate , 40: Print "30. HR "; careerP0%(idx, 29)
            Locate 10: Print "10. RUNS "; careerP0%(idx, 2);
            Locate , 40: Print "31. BB "; careerP0%(idx, 30)
            Locate 11: Print "11. E R "; careerP0%(idx, 3);
            Locate , 40: Print "32. K "; careerP0%(idx, 31)
            Locate 12: Print "12. SH OUT "; careerP0%(idx, 11);
            Locate , 40: Print "33. SB "; careerP0%(idx, 32)
            Locate 13: Print "13. GAMES "; careerP0%(idx, 8);
            Locate , 40: Print "34. CS "; careerP0%(idx, 33)
            Locate 14: Print "14. STARTS "; careerP0%(idx, 9);
            Locate , 40: Print "35. ERR "; careerP0%(idx, 34)
            Locate 15: Print "15. HR ALL "; careerP0%(idx, 14);
            Locate , 40: Print "36. CHS "; careerP0%(idx, 35)
            Locate 16: Print "16. GIDP "; careerP0%(idx, 13);
            Locate , 40: Print "37. LHS "; careerP0%(idx, 36)
            Locate 17: Print "17. SH "; careerP0%(idx, 15);
            Locate , 40: Print "38. GIDP "; careerP0%(idx, 37)
            Locate 18: Print "18. SF "; careerP0%(idx, 16);
            Locate , 40: Print "39. PO "; careerP0%(idx, 38)
            Locate 19: Print "19. RUNNERS INHERITED (RI) "; careerP0%(idx, 17);
            Locate , 40: Print "40. A "; careerP0%(idx, 39)
            Locate 20: Print "20. RI SCORED "; careerP0%(idx, 18);
            Locate , 40: Print "41. SH "; careerP0%(idx, 40)
            Locate 21: Print "21. BLOWN SAVES "; careerP0%(idx, 19)
            Locate , 40: Print "42. SF "; careerP0%(idx, 41)
            Locate 22: Print "22. RUN SUPPORT "; careerP0%(idx, 20)

            Do

                Do
                    Locate 23, 1
                    Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Loop Until B9 >= 0 And B9 <= 42

                Select Case B9

                    Case 1
                        Locate 1: Input "1. NAME "; statP$(idx)

                    Case 2
                        Locate 2: Input "2. WON "; careerP0%(idx, 6)

                    Case 3
                        Locate 3: Input "3. LOST "; careerP0%(idx, 7)

                    Case 4
                        Locate 4: Input "4. SV "; careerP0%(idx, 12)

                    Case 5
                        Locate 5: Input "5. (IP X 3) "; careerP0%(idx, 0)

                    Case 6
                        Locate 6: Input "6. HITS "; careerP0%(idx, 1)

                    Case 7
                        Locate 7: Input "7. BB "; careerP0%(idx, 4)

                    Case 8
                        Locate 8: Input "8. SO "; careerP0%(idx, 5)

                    Case 9
                        Locate 9: Input "9. CG "; careerP0%(idx, 10)

                    Case 10
                        Locate 10: Input "10. RUNS "; careerP0%(idx, 2)

                    Case 11
                        Locate 11: Input "11. E R "; careerP0%(idx, 3)

                    Case 12
                        Locate 12: Input "12. SH OUT "; careerP0%(idx, 11)

                    Case 13
                        Locate 13: Input "13. GAMES "; careerP0%(idx, 8)

                    Case 14
                        Locate 14: Input "14. STARTS "; careerP0%(idx, 9)

                    Case 15
                        Locate 15: Input "15. HR ALL "; careerP0%(idx, 14)

                    Case 16
                        Locate 16: Input "16. GIDP "; careerP0%(idx, 13)

                    Case 17
                        Locate 17: Input "17. SH "; careerP0%(idx, 15)

                    Case 18
                        Locate 18: Input "18. SF "; careerP0%(idx, 16)

                    Case 19
                        Locate 19: Input "19. RUNNERS INHERITED (RI) "; careerP0%(idx, 17)

                    Case 20
                        Locate 20: Input "20. RI SCORED "; careerP0%(idx, 18)

                    Case 21
                        Locate 21: Input "21. BLOWN SAVES "; careerP0%(idx, 19)

                    Case 22
                        Locate 22: Input "22. RUN SUPPORT "; careerP0%(idx, 20)

                    Case 23
                        Locate 23: Input "23. QUALITY STARTS "; careerP0%(idx, 21)

                    Case 24
                        Locate 2, 40: Input "24. AB "; careerP0%(idx, 23)

                    Case 25
                        Locate 3, 40: Input "25. H "; careerP0%(idx, 24)

                    Case 26
                        Locate 4, 40: Input "26. R "; careerP0%(idx, 25)

                    Case 27
                        Locate 5, 40: Input "27. RBI "; careerP0%(idx, 26)

                    Case 28
                        Locate 6, 40: Input "28. DBL "; careerP0%(idx, 27)

                    Case 29
                        Locate 7, 40: Input "29. TRPL "; careerP0%(idx, 28)

                    Case 30
                        Locate 8, 40: Input "30. HR "; careerP0%(idx, 29)

                    Case 31
                        Locate 9, 40: Input "31. BB "; careerP0%(idx, 30)

                    Case 32
                        Locate 10, 40: Input "32. K "; careerP0%(idx, 31)

                    Case 33
                        Locate 11, 40: Input "33. SB "; careerP0%(idx, 32)

                    Case 34
                        Locate 12, 40: Input "34. CS "; careerP0%(idx, 33)

                    Case 35
                        Locate 13, 40: Input "35. ERR "; careerP0%(idx, 34)

                    Case 36
                        Locate 14, 40: Input "36. CHS "; careerP0%(idx, 35)

                    Case 37
                        Locate 15, 40: Input "37. LHS "; careerP0%(idx, 36)

                    Case 38
                        Locate 16, 40: Input "38. GIDP "; careerP0%(idx, 37)

                    Case 39
                        Locate 17, 40: Input "39. PO "; careerP0%(idx, 38)

                    Case 40
                        Locate 18, 40: Input "40. A "; careerP0%(idx, 39)

                    Case 41
                        Locate 19, 40: Input "41. SH "; careerP0%(idx, 40)

                    Case 42
                        Locate 20, 40: Input "42. SF "; careerP0%(idx, 41)

                End Select

            Loop Until B9 = 0

        End If

    Loop Until pitcher = 0

End Sub


'----------------------------------------
'    ChangeSchedule Subroutine
'----------------------------------------
'...explanation...
Sub ChangeSchedule (Z$)

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
            Locate 1, 1: Print "1. SCORE FOR "; Z$; " "; H%
            Locate 2, 1: Print "2. OPPONENT'S NAME "; H$
            Locate 3, 1: Print "3. OPPONENT'S SCORE "; V%
            Locate 4, 1: Print "4. (H)OME OR (A)WAY "; PL$
            Locate 5, 1: Print "5. WINNING PITCHER "; W$
            Locate 6, 1: Print "6. LOSING PITCHER "; L$
            Locate 7, 1: Print "7. NUMBER OF INNINGS "; I%

            1057 C1 = 1

            1100 '
            Do
                Locate 9, 1: Print "ENTER # TO CHANGE (1-7) OR 0 TO CONTINUE"
                I$ = GetKeyPress$
                D = Val(I$)

                Select Case D
                    Case 1:
                        Locate 1, 1: Print "1. SCORE FOR "; Z$; " ";: Input H%

                    Case 2:
                        Locate 2, 1: Print "2. OPPONENT'S NAME ";: Input H$

                    Case 3:
                        Locate 3, 1: Print "3. OPPONENT'S SCORE ";: Input V%

                    Case 4:
                        Locate 4, 1: Print "4. (H)OME OR (A)WAY ";: Input PL$

                    Case 5:
                        Locate 5, 1: Print "5. WINNING PITCHER ";: Input W$

                    Case 6:
                        Locate 6, 1: Print "6. LOSING PITCHER ";: Input L$

                    Case 7:
                        Locate 7, 1: Print "7. NUMBER OF INNINGS ";: Input I%

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

            Do
                Print "ADD ANOTHER GAME (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Loop Until UCase$(I$) = "N"

        'Save changes
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


'----------------------------------------
'       GetTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_SINPUT (T$, yearNumber$, numberTeams)

    Shared teamRatings%()

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
            Call SelectTeam_SINPUT(T$, yearNumber$, numberTeams)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            If _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 To fileLength& / 4342

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

                Call SelectTeam_SINPUT(T$, yearNumber$, numberTeams)

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
            Color teamRatings%(11), teamRatings%(12): Print T$

            Do
                Color 15, 4
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
Sub SelectTeam_SINPUT (T$, yearNumber$, numberTeams)

    Shared teamRatings%()

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

            T$ = RTrim$(Q$(0))

            For I = 1 To 13
                teamRatings%(I) = CVI(Q$(I))
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
