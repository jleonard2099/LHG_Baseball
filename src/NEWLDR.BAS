Sub NEWLDR ()

    _Title "Full Count Baseball - Expanded Standings"

    ReDim menuChoices$(1 To 5)

    Do

        Cls
        Color 15, 0

        Call TITLE
        ReDim menuChoices$(1 To 5)

        menuChoices$(1) = "UPDATE EXPANDED LEAGUE LEADER FILES  "
        menuChoices$(2) = "VIEW EXPANDED LEAGUE LEADER FILES    "
        menuChoices$(3) = "UPDATE/VIEW EXPANDED LEAGUE STANDINGS"
        menuChoices$(4) = "UPDATE/VIEW SEASON RECORDS           "
        menuChoices$(5) = "RETURN TO LEAGUE LEADER MAIN MENU    "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 23
        Call box0(6, 22, 12, 60, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call UpdateExpandedLeaders
                Case 2: Call ViewExpandedLeaders
                Case 3: Call NEWLDRST
                Case 4: Call NEWLDRRC
                Case 5: Cls
            End Select
        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

    '2355 Exit Sub

End Sub

'----------------------------------------
'      ViewBattingLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewBattingLeaders (leagueFile$)

    Shared BK%()
    Shared leaderBL$(), TMB$(), TMM$()
    Shared TP$(), TPP$()
    Shared TYP!()
    Shared GM1!()

    'Can't just dim these unique to this subroutine?
    'Erase TMP$
    'Erase PL$
    'Erase TM$
    'Erase TT2$
    Open diskPaths$(2) + leagueFile$ + ".BAT" For Input As #1

    Input #1, JB, JP, D1$, D2$, D3$, D4$, leagueFile$, TS, TTS

    For I = 1 To 920:
        Input #1, leaderBL$(I), TMB$(I), GM1!(I)
        Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%
    Next I

    For I = 1 To 920:
        For I1 = 1 To 22:
            Input #1, BK%(I, I1)
        Next I1
        Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%
    Next I

    Close 1

    81 '
    Color , 0
    Cls
    Print
    Print
    Print
    J = 19

    Locate 2, 4: Color 15, 4: Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";
    For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
    Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

    Do
        For I = 0 To 15
            Color 14
            Locate I + 3, 8: Print Using "##"; I;
            Print " ";
            Color 0: Print TP$(I);
            Color 14
            Print Tab(40); Using "##"; I + 16;
            Print " ";
            Color 0
            Print TP$(I + 16)
        Next

        Print
        Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

    Loop Until B9 <= 31 And B9 >= 0

    Select Case B9
        Case 0: X = 1: stpl = 0: PPP = 0
        Case 1: X = 2: stpl = 0: PPP = 0
        Case 2: X = 3: stpl = 0: PPP = 0
        Case 3: X = 4: stpl = 0: PPP = 0
        Case 4: X = 5: stpl = 0: PPP = 0
        Case 5: X = 6: stpl = 0: PPP = 0
        Case 6: X = 7: stpl = 0: PPP = 0
        Case 7: X = 8: stpl = 0: PPP = 0
        Case 8: X = 9: stpl = 0: PPP = 0
        Case 9: X = 10: stpl = 0: PPP = 0
        Case 10: X = 11: stpl = 0: PPP = 0
        Case 11: X = 12: stpl = 0: PPP = 0
        Case 12: X = 13: stpl = 0: PPP = 0
        Case 13: X = 14: stpl = 0: PPP = 0
        Case 14: X = 15: stpl = 0: PPP = 0
        Case 15: X = 16: stpl = 0: PPP = 0
        Case 16: X = 17: stpl = 0: PPP = 0
        Case 17: X = 18: stpl = 0: PPP = 0
        Case 18: X = 19: stpl = 0: PPP = 0
        Case 19: X = 20: stpl = 0: PPP = 0
        Case 20: X = 17: X1 = 18: X2 = 12: stpl = 0: PPP = 0
        Case 21: X = 1: X1 = 2: X2 = 13: X3 = 8: stpl = 0: PPP = 0
        Case 22: X = 1: X1 = 2: X2 = 5: X3 = 6: X4 = 7: stpl = 0: PPP = 0
        Case 23: X = 1: X1 = 2: X2 = 8: stpl = 0: PPP = 0
        Case 24: X = 1: X1 = 7: stpl = 0: PPP = 0
        Case 25: X = 1: X1 = 9: stpl = 0: PPP = 0
        Case 26: X = 1: X1 = 8: stpl = 0: PPP = 0
        Case 27: X = 1: X1 = 4: stpl = 0: PPP = 0
        Case 28: X = 1: X1 = 3: stpl = 0: PPP = 0
        Case 29: X = 1: X1 = 16: stpl = 0: PPP = 0
        Case 30: X = 21: X1 = 22: stpl = 0: PPP = 0
        Case 31: Exit Sub 'GoTo 70 aka Main Menu
    End Select

    Call GetSortOptions(JX, PV)

    Cls
    PPP = 0

    Select Case B9
        Case 0 To 19:
            For I = 1 To JB

                If BK%(I, X) <> 0 Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = leaderBL$(I)
                    TMM$(PPP) = TMB$(I)
                    TYP!(PPP) = BK%(I, X)
                End If

            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 1

        Case 20:
            For I = 1 To JB

                If BK%(I, X) + BK%(I, X1) + BK%(I, X2) <> 0 Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = leaderBL$(I)
                    TMM$(PPP) = TMB$(I)
                    TYP!(PPP) = (BK%(I, X) + BK%(I, X1)) / (BK%(I, X) + BK%(I, X1) + BK%(I, X2))
                End If

            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 2

        Case 21:
            For I = 1 To JB
                If BK%(I, X) + BK%(I, X3) >= GM1!(I) * 3.1 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    If BK%(I, X1) = 0 Then
                        TPP$(PPP) = leaderBL$(PPP)
                        TMM$(PPP) = TMB$(PPP)
                        TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = BK%(I, X1) / BK%(I, X)
                    End If
                End If
            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 2

        Case 22:
            For I = 1 To JB

                If BK%(I, X) >= GM1!(I) * 3.1 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP

                    If BK%(I, X1) = 0 And BK%(I, X2) = 0 And BK%(I, X3) = 0 And BK%(I, X4) = 0 Then
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = (BK%(I, X1) + BK%(I, X2) + (BK%(I, X3) * 2) + (BK%(I, X4) * 3)) / BK%(I, X)
                    End If

                End If
            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 2

        Case 23:
            For I = 1 To JB

                If BK%(I, X) >= GM1!(I) * 3.1 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    If BK%(I, X1) = 0 And BK%(I, X2) = 0 Then
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = (BK%(I, X1) + BK%(I, X2)) / (BK%(I, X) + BK%(I, X2))
                    End If
                End If
            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 2

        Case 24 To 29:
            For I = 1 To JB
                If BK%(I, X) >= GM1!(I) * 3.1 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    If BK%(I, X1) = 0 Then
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = BK%(I, X) / BK%(I, X1)
                    End If
                End If

            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 3

        Case 30:
            For I = 1 To JB
                If BK%(I, X) + BK%(I, X1) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    If BK%(I, X1) / (BK%(I, X) + BK%(I, X1)) = 0 Then
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderBL$(I)
                        TMM$(PPP) = TMB$(I)
                        TYP!(PPP) = BK%(I, X1) / (BK%(I, X) + BK%(I, X1))
                    End If
                End If
            Next I

            Call SortPlayer1(JX, PPP, stpl)
            TVN = 2

    End Select

    QW = PPP

    If PV = 1 Then
        Call PlayerTotals(TVN, B9, QW)
    Else
        'PV = 2
        Call GetListLength(QW, LG)

        Color 0, 15

        Cls

        LPrint "     PLAYER                  TEAM                     "; TP$(B9)

        For W = 1 To LG

            If W <> 0 And (W Mod 50 = 0) Then
                LPrint Chr$(12)
                LPrint "     PLAYER                  TEAM                     "; TP$(B9)
            End If

            LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);
            LPrint Tab(25); TMM$(W);

            Select Case TVN
                Case 1: LPrint Tab(58); Using "#####"; TYP!(W)
                Case 2: LPrint Tab(58); Using ".###"; TYP!(W)
                Case 3: LPrint Tab(58); Using "###.##"; TYP!(W)
            End Select

        Next W

        LPrint Chr$(12)

    End If

End Sub

'----------------------------------------
'       PlayerTotals Subroutine
'----------------------------------------
'...explanation...
Sub PlayerTotals (TVN, B9, QW)

    Shared TMM$(), TP$(), TPP$()
    Shared TYP!()

    Do

        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; TP$(B9)

        For W = 1 To QW

            If W <> 0 And (W Mod 20 = 0) Then
                Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
                Else
                    Exit Sub
                End If

            End If

            Color 12: Print Using "###"; W;
            Print " ";
            Color 3: Print TPP$(W);
            Color 15: Print Tab(25); TMM$(W);
            Select Case TVN
                Case 1: Color 15: Print Tab(58); Using "#####"; TYP!(W)
                Case 2: Color 15: Print Tab(58); Using ".###"; TYP!(W)
                Case 3: Color 15: Print Tab(58); Using "###.##"; TYP!(W)
            End Select

        Next W

        Color 15
        Locate 22: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Locate 22: Print Space$(64)
            Color 14

            Do
                Locate 22
                Print "VIEW AGAIN (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'      ViewPitchingLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewPitchingLeaders (leagueFile$)
    Shared leaderPL$()
    Shared TMM$(), TMP$(), TP1$(), TPP$()
    Shared TYP!(), TYP1!()
    Shared PM%()
    ReDim GM2(880)

    Dim TYP2(920)

    1080 '
    'Can't just dim these unique to this subroutine?
    'Erase TMB$
    'Erase BL$
    'Erase TM$
    'Erase TT2$

    Open diskPaths$(2) + leagueFile$ + ".PIT" For Input As #1
    Input #1, JB, JP, D1$, D2$, D3$, D4$, leagueFile$, TS, TTS
    For I = 1 To 880:
        Input #1, leaderPL$(I), TMP$(I), GM2(I)
    Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
    For I = 1 To 880:
        For I1 = 1 To 23:
        Input #1, PM%(I, I1): Next I1:
    Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
    Close 1

    1081 '
    Color , 0
    Cls
    Print
    Print
    Print
    Print
    J = 20

    Locate 2, 4: Color 15, 2: Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";
    For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
    Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

    For I = 0 To 14:
        Color 14: Locate I + 3, 8: Print Using "##"; I;
        Print " ";
        Color 0: Print TP1$(I);
        Color 14: Print Tab(40); Using "##"; I + 15;
        Print " ";
        Color 0: Print TP1$(I + 15)
    Next

    Print
    Locate , 23
    Do
        Input "SELECT NUMBER TO VIEW"; B9
    Loop Until B9 >= 0 And B9 <= 29

    Select Case B9
        Case 0: X = 1: stpl = 0: PPP = 0
        Case 1: X = 2: stpl = 0: PPP = 0
        Case 2: X = 3: stpl = 0: PPP = 0
        Case 3: X = 4: stpl = 0: PPP = 0
        Case 4: X = 5: stpl = 0: PPP = 0
        Case 5: X = 6: stpl = 0: PPP = 0
        Case 6: X = 7: stpl = 0: PPP = 0
        Case 7: X = 8: stpl = 0: PPP = 0
        Case 8: X = 9: stpl = 0: PPP = 0
        Case 9: X = 10: stpl = 0: PPP = 0
        Case 10: X = 11: stpl = 0: PPP = 0
        Case 11: X = 12: stpl = 0: PPP = 0
        Case 12: X = 13: stpl = 0: PPP = 0
        Case 13: X = 14: stpl = 0: PPP = 0
        Case 14: X = 15: stpl = 0: PPP = 0
        Case 15: X = 16: stpl = 0: PPP = 0
        Case 16: X = 18: stpl = 0: PPP = 0
        Case 17: X = 19: stpl = 0: PPP = 0
        Case 18: X = 18: X1 = 19: stpl = 0: PPP = 0
        Case 19: X = 20: stpl = 0: PPP = 0
        Case 20: X = 22: stpl = 0: PPP = 0
        Case 21: X = 1: X1 = 4: stpl = 0: PPP = 0
        Case 22: X = 1: X1 = 5: stpl = 0: PPP = 0
        Case 23: X = 1: X1 = 6: stpl = 0: PPP = 0
        Case 24: X = 1: X1 = 2: stpl = 0: PPP = 0
        Case 25: X = 1: X1 = 14: stpl = 0: PPP = 0
        Case 26: X = 16: stpl = 0: PPP = 0
        Case 27: X = 17: stpl = 0: PPP = 0
        Case 28: X = 9: X1 = 21: stpl = 0: PPP = 0
        Case 29: Exit Sub 'GoTo 70
    End Select

    Call GetSortOptions(JX, PV)

    Cls
    PPP = 0

    Select Case B9
        Case 0:
            For I = 1 To JP
                If PM%(I, X) <> 0 Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = leaderPL$(I)
                    TMM$(PPP) = TMP$(I)
                    TYP2(PPP) = PM%(I, X)
                End If

            Next I

            For I = 2 To PPP:
                stpl = stpl + 1
                Locate 5, 5: Print "sorting player "; stpl
                For J = PPP To I Step -1

                    If JX = 1 Then
                        If TYP2(J - 1) < TYP2(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP2(J - 1), TYP2(J)
                    Else
                        'JX = 2
                        If TYP2(J - 1) > TYP2(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP2(J - 1), TYP2(J)
                    End If
                Next J
            Next I

            For I = 1 To PPP
                TYP!(I) = (Int(TYP2(I) / 3))
                TYP1!(I) = TYP2(I) - (Int(TYP2(I) / 3) * 3)
            Next

            TVN = 1

        Case 1 To 16, 19, 20, 26, 27:
            For I = 1 To JP

                If PM%(I, X) <> 0 Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = leaderPL$(I)
                    TMM$(PPP) = TMP$(I)
                    TYP!(PPP) = PM%(I, X)
                End If
            Next I

            Call SortPlayer2(JX, PPP, stpl)

            TVN = 2

        Case 18:
            For I = 1 To JP
                If PM%(I, X) <> 0 Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP

                    If PM%(I, X1) <> 0 Then
                        TPP$(PPP) = leaderPL$(I): TMM$(PPP) = TMP$(I): TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderPL$(I): TMM$(PPP) = TMP$(I): TYP!(PPP) = PM%(I, X1) / PM%(I, X)
                    End If
                End If
            Next

            Call SortPlayer2(JX, PPP, stpl)
            TVN = 4

        Case 21 To 25:
            For I = 1 To JP
                If PM%(I, X) / 3 >= GM2(I) Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP

                    If PM%(I, X) = 0 Or PM%(I, X1) = 0 Then
                        TPP$(PPP) = leaderPL$(I): TMM$(PPP) = TMP$(I): TYP!(PPP) = 0
                    Else
                        TPP$(PPP) = leaderPL$(I): TMM$(PPP) = TMP$(I): TYP!(PPP) = (PM%(I, X1) * 9) / (PM%(I, X) / 3)
                    End If
                End If

            Next

            Call SortPlayer2(JX, PPP, stpl)

            TVN = 3

        Case 28:
            For I = 1 To JP
                If PM%(I, X1) <> 0 Then
                    PPP = PPP + 1
                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = leaderPL$(I): TMM$(PPP) = TMP$(I): TYP!(PPP) = PM%(I, X1) / PM%(I, X)
                End If
            Next

            Call SortPlayer2(JX, PPP, stpl)

            TVN = 3

    End Select

    QW = PPP

    If PV = 1 Then
        Call PitchingLeaders_Screen(TVN, B9, QW)
    Else
        'PV = 2
        Call GetListLength(QW, LG)

        Color 0, 15

        LPrint "     PLAYER                  TEAM                     "; TP1$(B9)

        For W = 1 To LG

            If W <> 0 And (W Mod 50 = 0) Then
                Cls
                Color 14
                Print "     PLAYER                  TEAM                     "; TP1$(B9)
            End If

            LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);

            Select Case TVN
                Case 1: LPrint Tab(58); Using "###"; TYP!(W);: LPrint ".";: LPrint Using "#"; TYP1!(W)
                Case 2: LPrint Tab(58); Using "#####"; TYP!(W)
                Case 3: LPrint Tab(58); Using "###.##"; TYP!(W)
            End Select

        Next W

        LPrint Chr$(12)

    End If

End Sub

'----------------------------------------
'   PitchingLeaders_Screen Subroutine
'----------------------------------------
'...explanation...
Sub PitchingLeaders_Screen (TVN, B9, QW)

    Shared TMM$(), TP1$(), TPP$()
    Shared TYP!(), TYP1!()

    Do
        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; TP1$(B9)

        For W = 1 To QW

            If W <> 0 And (W Mod 20 = 0) Then
                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     PLAYER                  TEAM                     "; TP1$(B9)
                Else
                    Exit Sub
                End If
            End If

            If W = 220 Or W = 240 Or W = 260 Or W = 280 Or W = 300 Or W = 320 Or W = 340 Or W = 360 Or W = 380 Or W = 400 Then
                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     PLAYER                  TEAM                     "; TP1$(B9)
                Else
                    Exit Sub
                End If
            End If

            If W = 420 Or W = 440 Or W = 460 Or W = 480 Or W = 500 Or W = 520 Or W = 540 Or W = 560 Or W = 580 Or W = 600 Then
                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     PLAYER                  TEAM                     "; TP1$(B9)
                Else
                    Exit Sub
                End If
            End If

            If W = 620 Or W = 640 Or W = 660 Or W = 680 Or W = 700 Or W = 720 Or W = 740 Or W = 760 Or W = 780 Or W = 800 Then
                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     PLAYER                  TEAM                     "; TP1$(B9)
                Else
                    Exit Sub
                End If
            End If

            If W = 820 Or W = 840 Or W = 860 Or W = 880 Then
                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14
                    Print "     PLAYER                  TEAM                     "; TP1$(B9)
                Else
                    Exit Sub
                End If
            End If

            Color 12
            Print Using "###"; W;: Print " ";
            Color 3
            Print TPP$(W);
            Color 15: Print Tab(25); TMM$(W);: Color 15
            Select Case TVN
                Case 1: Print Tab(58); Using "###"; TYP!(W);: Print ".";: Print Using "#"; TYP1!(W)
                Case 2: Print Tab(58); Using "#####"; TYP!(W)
                Case 3: Print Tab(58); Using "###.##"; TYP!(W)
                Case 4: Print Tab(58); Using ".##"; TYP!(W);: Print "%"
            End Select

        Next W

        Locate 22
        Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Locate 22: Print Space$(64)

            Color 14

            Do
                Locate 22
                Print "VIEW AGAIN (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'      UpdateExpandedLeaders Subroutine
'----------------------------------------
'...explanation...
Sub UpdateExpandedLeaders ()

    Shared BK%()
    Shared statB%(), careerB0%(), BK%()
    Shared statP%(), P0%(), PM%()
    Shared statT%(), statT1%(), GM1!()

    Shared statB$(), leaderBL$(), statP$(), leaderPL$()
    Shared TM$(), TMB$(), TMP$(), TT2$()
    Shared Z1$()

    Shared TT!(), TT1!()

    ReDim GM2(880)
    Dim WW(40), LL(40)

    'this is where we need to assign non-array variable values?

    Cls

    Spec$ = diskPaths$(2) + "*.TMS"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%

    If Count% <> 0 Then

        ReDim leagueFiles$(0 To Count%)
        For X = 1 To Count%
            leagueFiles$(X) = Space$(12)
        Next

        leagueFiles$(0) = Spec$
        Call ReadFile(leagueFiles$())

        Do
            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT LEAGUE TO UPDATE": Color 15
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
            Print "-SELECTS LEAGUE";

            _MouseShow "DEFAULT"
            MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = leagueFiles$(Selection%)
                Locate 2, 33: Print targetFile$

                Do
                    Locate , 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

            End If

        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

        Color 15, 0

        If ExitCode% <> 27 Then

            targetFile$ = RTrim$(targetFile$)
            league$ = Left$(targetFile$, Len(targetFile$) - 4)
            leagueFile$ = league$

            Open diskPaths$(2) + league$ + ".TMS" For Input As #2
            Input #2, leagueFile$: Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$
            For X = 1 To 40: Input #2, Z1$(X): Next
            Close 2

            Color 15, 0
            Cls

            For KK = 1 To 40: YY = YY + 1

                If Z1$(KK) <> "*" Then
                    If _FileExists(diskPaths$(1) + Z1$(KK)) Then
                        teamStat$ = Z1$(KK)
                        Print "CONVERTING STAT FILE OF "; teamStat$

                        Open diskPaths$(1) + teamStat$ For Input As #1
                        Input #1, A$: Input #1, W2%: Input #1, L2%
                        For I = 0 To 22
                            Input #1, statB$(I)
                            Input #1, statB%(I)
                        Next I
                        For I = 0 To 21:
                            Input #1, statP$(I)
                            Input #1, statP%(I)
                        Next I
                        For I = 0 To 22
                            For I1 = 0 To 21
                                Input #1, careerB0%(I, I1)
                            Next I1
                        Next I
                        For I = 0 To 21
                            For I1 = 0 To 41
                                Input #1, P0%(I, I1)
                            Next I1
                        Next I
                        Input #1, D0%
                        Input #1, D1%
                        For I = 0 To 21: Input #1, statT%(I): Next I
                        For I = 0 To 22: Input #1, statT1%(I): Next I
                        Close #1

                        WW(YY) = W2%
                        LL(YY) = L2%

                        TS = TS + 1

                        For I = 0 To 21
                            TT!(TS, I) = statT%(I)
                            TM$(TS, I) = A$
                        Next

                        TTS = TTS + 1
                        For I = 0 To 22
                            TT1!(TTS, I) = statT1%(I)
                            TT2$(TTS, I) = A$
                        Next

                        For I = 0 To 22
                            If statB$(I) <> "XXX" Then
                                JB = JB + 1
                                leaderBL$(JB) = statB$(I)
                                TBN = TBN + 1
                                TMB$(TBN) = A$
                                GM1!(JB) = WW(YY) + LL(YY)
                            End If

                        Next I

                        For I = 0 To 21
                            If statP$(I) <> "XXX" Then
                                JP = JP + 1
                                leaderPL$(JP) = statP$(I)
                                TPN = TPN + 1
                                TMP$(TPN) = A$
                                GM2(JP) = WW(YY) + LL(YY)
                            End If
                        Next I

                        For I = 0 To 22
                            If statB$(I) <> "XXX" Then
                                K = K + 1: K1 = 0
                                For I1 = 0 To 21
                                    K1 = K1 + 1
                                    BK%(K, K1) = careerB0%(I, I1)
                                Next I1
                            End If
                        Next I

                        For I = 0 To 21
                            If statP$(I) <> "XXX" Then
                                PK = PK + 1: PK1 = 0
                                For I1 = 0 To 22
                                    PK1 = PK1 + 1
                                    PM%(PK, PK1) = P0%(I, I1)
                                Next I1
                            End If
                        Next I

                    End If 'Done checking if file exists

                End If 'Done checking for "XXX"

            Next KK

            Print "SAVING LEADER FILES . . . . PLEASE WAIT"

            Open diskPaths$(2) + league$ + ".BAT" For Output As #1
            Write #1, JB, JP, D1$, D2$, D3$, D4$, leagueFile$, TS, TTS
            For I = 1 To 920: Write #1, leaderBL$(I), TMB$(I), GM1!(I): Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
            For I = 1 To 920: For I1 = 1 To 22: Write #1, BK%(I, I1): Next I1: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
            Close 1

            Open diskPaths$(2) + league$ + ".PIT" For Output As #1
            Write #1, JB, JP, D1$, D2$, D3$, D4$, leagueFile$, TS, TTS
            For I = 1 To 880
                Write #1, leaderPL$(I), TMP$(I), GM2(I)
                Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%
            Next I
            For I = 1 To 880: For I1 = 1 To 23: Write #1, PM%(I, I1): Next I1: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next I
            Close 1

            Open diskPaths$(2) + league$ + ".TOT" For Output As #1
            Write #1, JB, JP, D1$, D2$, D3$, D4$, leagueFile$, TS, TTS
            For I = 1 To 40: Write #1, WW(I), LL(I): Next
            For I = 1 To 40: For I1 = 0 To 21: Write #1, TT!(I, I1), TM$(I, I1): Next: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next
            For I = 1 To 40: For I1 = 0 To 22: Write #1, TT1!(I, I1), TT2$(I, I1): Next: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next
            Close #1

            Call ExpandedLeaders_Menu(leagueFile$)

        End If 'Done checking for ESC

    End If 'Done checking file count

End Sub

'----------------------------------------
'      ExpandedLeaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub ExpandedLeaders_Menu (leagueFile$)

    '70
    Do
        Cls
        Color 15, 0

        Call TITLE
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "VIEW BATTING LEADERS "
        menuChoices$(2) = "VIEW PITCHING LEADERS"
        menuChoices$(3) = "VIEW TEAM TOTALS     "
        menuChoices$(4) = "RETURN TO MAIN MENU  "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 30
        Call box0(6, 29, 11, 51, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1:
                    Call ViewBattingLeaders(leagueFile$)

                Case 2:
                    Call ViewPitchingLeaders(leagueFile$)

                Case 3:
                    Call ViewTeamTotals(leagueFile$)

                Case 4: Cls '
            End Select
        Else
            Selection% = 4
        End If

    Loop Until Selection% = 4

End Sub

'----------------------------------------
'     ViewExpandedLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewExpandedLeaders ()

    Do

        Cls

        Spec$ = diskPaths$(2) + "*.TMS"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            'This was not in the original code
            'I'm assuming because this technically
            'should be impossible as the files get
            'created right before presenting the menu
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim leagueFiles$(0 To Count%)
            For X = 1 To Count%
                leagueFiles$(X) = Space$(12)
            Next
            leagueFiles$(0) = Spec$
            Call ReadFile(leagueFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE TO VIEW": Color 15
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
                Print "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = leagueFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                league$ = Left$(targetFile$, Len(targetFile$) - 4)
                leagueFile$ = league$
                Call ExpandedLeaders_Menu(leagueFile$)
            End If

        End If

    Loop Until Count% = 0 Or UCase$(I$) = "N" Or ExitCode% = 27

End Sub

'----------------------------------------
'     ViewTeamTotals Subroutine
'----------------------------------------
'...explanation...
Sub ViewTeamTotals (leagueFile$)

    Shared TM$(), TMM$()
    Shared TP$(), TP2$(), TT2$()

    Shared TT!(), TT1!()
    Shared TYP!(), TYP1!()

    Dim WW(40), LL(40)

    'Can't just dim these unique to this subroutine?
    'Erase TMB$
    'Erase BL$
    'Erase TMP$
    'Erase PL$

    Open diskPaths$(2) + leagueFile$ + ".TOT" For Input As #1
    Input #1, JB, JP, D1$, D2$, D3$, D4$, leagueFile$, TS, TTS
    For I = 1 To 40: Input #1, WW(I), LL(I): Next
    For I = 1 To 40: For I1 = 0 To 21: Input #1, TT!(I, I1), TM$(I, I1): Next: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next
    For I = 1 To 40: For I1 = 0 To 22: Input #1, TT1!(I, I1), TT2$(I, I1): Next: Color 14: Locate 18, 45: TMR% = TMR% - 1: Print Using "#####"; TMR%: Next
    Close #1

    2881 '
    Color , 0
    Cls
    Print
    J = 21

    Locate 2, 4: Color 15, 6: Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";
    For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
    Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

    For I = 0 To 19: Locate I + 3, 8: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP2$(I): Next

    For I = 20 To 39: Locate I - 17, 40: Color 11: Print Tab(40); Using "##"; I;: Print " ";: Color 0: Print TP2$(I): Next

    Do
        Color 15, 0
        Locate 1, 23: Input "SELECT NUMBER TO VIEW"; B9
    Loop Until B9 >= 0 Or B9 <= 39

    Select Case B9
        Case 0: X = 0: XX = 0: stpl = 0
        Case 1: X = 1: XX = 1: stpl = 0
        Case 2: X = 2: XX = 2: stpl = 0
        Case 3: X = 3: XX = 3: stpl = 0
        Case 4: X = 4: XX = 4: stpl = 0
        Case 5: X = 5: XX = 5: stpl = 0
        Case 6: X = 6: XX = 6: stpl = 0
        Case 7: X = 7: XX = 7: stpl = 0
        Case 8: X = 8: XX = 8: stpl = 0
        Case 9: X = 9: XX = 9: stpl = 0
        Case 10: X = 10: XX = 10: stpl = 0
        Case 11: X = 11: XX = 11: stpl = 0
        Case 12: X = 18: XX = 12: stpl = 0
        Case 13: X = 19: XX = 13: stpl = 0
        Case 14: X = 0: X1 = 1: XX = 14: stpl = 0
        Case 15: X = 0: X1 = 1: X2 = 4: X3 = 5: X4 = 6: XX = 15: stpl = 0
        Case 16: X = 0: X1 = 1: X2 = 7: XX = 16: stpl = 0
        Case 17: X = 12: XX = 17: stpl = 0
        Case 18: X = 15: XX = 18: stpl = 0
        Case 19: X = 16: X1 = 17: X2 = 11: XX = 19: stpl = 0
        Case 20: X = 0: XX = 20: stpl = 0
        Case 21: X = 1: XX = 21: stpl = 0
        Case 22: X = 2: XX = 22: stpl = 0
        Case 23: X = 3: XX = 23: stpl = 0
        Case 24: X = 4: XX = 24: stpl = 0
        Case 25: X = 5: XX = 25: stpl = 0
        Case 26: X = 10: XX = 26: stpl = 0
        Case 27: X = 11: XX = 27: stpl = 0
        Case 28: X = 12: XX = 28: stpl = 0
        Case 29: X = 13: XX = 29: stpl = 0
        Case 30: X = 14: XX = 30: stpl = 0
        Case 31: X = 17: XX = 31: stpl = 0
        Case 32: X = 18: XX = 32: stpl = 0
        Case 33: X = 17: X1 = 18: XX = 33: stpl = 0
        Case 34: X = 19: XX = 34: stpl = 0
        Case 35: X = 21: XX = 35: stpl = 0
        Case 36: X = 0: X1 = 3: XX = 36: stpl = 0
        Case 37: X = 15: XX = 37: stpl = 0
        Case 38: X = 16: XX = 38: stpl = 0
        Case 39: Exit Sub
    End Select

    Call GetSortOptions(JX, PV)
    Cls

    Select Case B9
        Case 0 To 13, 17, 18:
            For I = 1 To TS
                TMM$(I) = TM$(I, X)
                TYP!(I) = TT!(I, X)
            Next

            Call SortTeam1(JX, TS, stpl)

            TVN = 1
            QW = TS
            screenType = 0

        Case 14
            For I = 1 To TS
                TMM$(I) = TM$(I, X): TYP!(I) = TT!(I, X1) / TT!(I, X)
            Next

            Call SortTeam1(JX, TS, stpl)

            TVN = 2
            QW = TS
            screenType = 0

        Case 15
            For I = 1 To TS
                TMM$(I) = TM$(I, X): TYP!(I) = (TT!(I, X1) + TT!(I, X2) + (TT!(I, X3) * 2) + (TT!(I, X4) * 3)) / TT!(I, X)
            Next

            Call SortTeam1(JX, TS, stpl)

            TVN = 2
            QW = TS
            screenType = 0

        Case 16
            For I = 1 To QW
                TMM$(I) = TM$(I, X): TYP!(I) = (TT!(I, X1) + TT!(I, X2)) / (TT!(I, X) + TT!(I, X2))
            Next

            Call SortTeam1(JX, TS, stpl)

            TVN = 2
            QW = TS
            screenType = 0

        Case 19
            For I = 1 To QW
                TMM$(I) = TM$(I, X): TYP!(I) = (TT!(I, X) + TT!(I, X1)) / (TT!(I, X) + TT!(I, X1) + TT!(I, X2))
            Next

            Call SortTeam1(JX, TS, stpl)

            TVN = 2
            QW = TS
            screenType = 0

        Case 20
            For I = 1 To TTS
                TMM$(I) = TT2$(I, X): TYP!(I) = TT1!(I, X)
            Next

            For I = 2 To TTS
                stpl = stpl + 1
                Locate 5, 5: Print "sorting team "; stpl
                For J = TTS To I Step -1

                    If JX = 1 Then
                        If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    Else
                        'JX = 2
                        If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    End If
                Next J
            Next I

            For I = 1 To TTS
                TYP!(I) = (Int(TYP!(I) / 3)): TYP1!(I) = TYP!(I) - (Int(TYP!(I) / 3) * 3)
            Next

            TVN = 1
            QW = TTS
            screenType = 1

        Case 21 To 32, 34, 35, 37, 38
            For I = 1 To TTS
                TMM$(I) = TT2$(I, X): TYP!(I) = TT1!(I, X)
            Next

            Call SortTeam2(JX, TTS, stpl)

            TVN = 2
            QW = TTS
            screenType = 1

        Case 33
            For I = 1 To TTS
                TMM$(I) = TT2$(I, X): TYP!(I) = (TT1!(I, X1) / TT1!(I, X)) * 100
            Next

            Call SortTeam2(JX, TTS, stpl)

            TVN = 3
            QW = TTS
            screenType = 1

        Case 36
            For I = 1 To TTS
                TMM$(I) = TT2$(I, X): TYP!(I) = (TT1!(I, X1) * 9) / (TT1!(I, X) / 3)
            Next

            Call SortTeam2(JX, TTS, stpl)

            TVN = 3
            QW = TTS
            screenType = 1

        Case 39
            Exit Sub 'GoTo 70 aka Main Menu

    End Select

    If screenType = 0 Then
        Select Case PV
            Case 1: Call LeagueTotals_A_Screen(TVN, QW)
            Case 2:
                Color 0, 15

                LPrint "     TEAM                     "; TP$(XX)

                For W = 1 To QW

                    Color 12: LPrint Using "###"; W;: LPrint " ";: Color 3: LPrint TMM$(W);: Color 15
                    Select Case TVN
                        Case 1: LPrint Tab(32); Using "#####"; TYP!(W)
                        Case 2: LPrint Tab(33); Using ".###"; TYP!(W)
                    End Select

                Next W

                LPrint Chr$(12)

        End Select

    Else
        Select Case PV
            Case 1: Call LeagueTotals_B_Screen(TVN, QW)
            Case 2:
                Color 0, 15

                LPrint "     TEAM                     "; TP1$(XX)

                LPrint Using "###"; W;
                LPrint " ";
                LPrint TMM$(W);:

                For W = 1 To QW
                    Select Case TVN
                        Case 1: LPrint Tab(32); Using "#####"; TYP!(W);: LPrint ".";: LPrint Using "#"; TYP1!(W)
                        Case 2: LPrint Tab(32); Using "#####"; TYP!(W)
                        Case 3: LPrint Tab(32); Using "##.##"; TYP!(W)
                    End Select
                Next W

                LPrint Chr$(12)

        End Select

    End If

End Sub

'----------------------------------------
'    LeagueTotals_A_Screen Subroutine
'----------------------------------------
'...explanation...
Sub LeagueTotals_A_Screen (TVN, QW)

    Shared TMM$(), TP2$()
    Shared TYP!()

    Do

        Color 14, 0: Cls:: Print "     TEAM                     "; TP2$(XX)

        For W = 1 To QW

            If W = 20 Or W = 40 Then

                Locate 22
                Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     TEAM                     "; TP2$(XX)
                Else
                    Exit Sub
                End If

            End If

            Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15:

            Select Case TVN
                Case 1: Print Tab(32); Using "#####"; TYP!(W)
                Case 2: Print Tab(33); Using ".###"; TYP!(W)
            End Select

        Next W

        Locate 22
        Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        I$ = GetKeyPress$


        If I$ <> Chr$(27) Then
            Locate 22: Print Space$(64)
            Color 14

            Do
                Locate 22
                Print "VIEW AGAIN (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'    LeagueTotals_B_Screen Subroutine
'----------------------------------------
'...explanation...
Sub LeagueTotals_B_Screen (TVN, QW)

    Shared TMM$(), TP2$()
    Shared TYP!(), TYP1!()

    Do
        Color 14, 0
        Cls
        Print "     TEAM                     "; TP2$(XX)

        For W = 1 To QW

            If W = 20 Or W = 40 Then
                Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     TEAM                     "; TP2$(XX)
                Else
                    Exit Sub
                End If
            Else

                Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15:
                Select Case TVN
                    Case 1: Print Tab(32); Using "#####"; TYP!(W);: Print ".";: Print Using "#"; TYP1!(W)
                    Case 2: Print Tab(32); Using "#####"; TYP!(W)
                    Case 3: Print Tab(32); Using "##.##"; TYP!(W)
                End Select

            End If

        Next W

        Locate 22
        Color 15
        Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Locate 22: Print Space$(64)
            Color 14

            Do
                Locate 22
                Print "VIEW AGAIN (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'       GetSortOptions Subroutine
'----------------------------------------
'...explanation...
Sub GetSortOptions (JX, PV)

    Print
    Print
    Print
    J = 6

    Locate 8, 8
    Color 15, 1: Print " " + Chr$(214); String$(57, 196); Chr$(183) + " ";
    For II = 1 To J
        Locate 8 + II, 8: Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";
    Next
    Locate 9 + J, 8
    Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";
    Locate 10, 15

    Color 3: Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
    Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
    Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"

    Do
        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "H":
                Call SelectPrintOrView(PV)
                JX = 1 '  : GoTo 5030

            Case "L":
                Call SelectPrintOrView(PV)
                JX = 2 ': GoTo 5030

            Case "R": Cls '

        End Select

    Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

End Sub

'----------------------------------------
'       SelectPrintOrView Subroutine
'----------------------------------------
'...explanation...
Sub SelectPrintOrView (printOption)

    Print
    Print
    Print
    Print
    Print
    J = 4
    Locate 9, 15: Color 15, 0: Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";
    For II = 1 To J: Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";: Next
    Locate 10 + J, 15: Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ": Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"
    Do
        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "P": printOption = 2
            Case "V": printOption = 1
            Case "R": '

        End Select

    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V" Or UCase$(I$) = "R"

End Sub

'----------------------------------------
'       SortPlayer1 Subroutine
'----------------------------------------
'...explanation...
Sub SortPlayer1 (JX, PPP, stpl)

    Shared TMM$(), TPP$()
    Shared TYP!()

    For I = 2 To PPP
        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If JX = 1 Then
                If TYP!(J - 1) < TYP!(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            Else
                'JX = 2
                If TYP!(J - 1) > TYP!(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            End If

        Next J
    Next I

End Sub

'----------------------------------------
'       SortPlayer2 Subroutine
'----------------------------------------
'...explanation...
Sub SortPlayer2 (JX, PPP, stpl)

    Shared TMM$(), TPP$()
    Shared TYP!()

    For I = 2 To PPP
        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1

            If JX = 1 Then
                If TYP!(J - 1) < TYP!(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            Else
                'JX = 2
                If TYP!(J - 1) > TYP!(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            End If

        Next J
    Next I

End Sub

'----------------------------------------
'       SortTeam1 Subroutine
'----------------------------------------
'...explanation...
Sub SortTeam1 (JX, TS, stpl)

    Shared TMM$()
    Shared TYP!()

    For I = 2 To TS
        stpl = stpl + 1
        Locate 5, 5: Print "sorting team "; stpl
        For J = TS To I Step -1

            If JX = 1 Then
                If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            Else
                'JX = 2
                If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            End If

        Next J
    Next I

End Sub

'----------------------------------------
'       SortTeam2 Subroutine
'----------------------------------------
'...explanation...
Sub SortTeam2 (JX, TTS, stpl)

    Shared TMM$()
    Shared TYP!()

    For I = 2 To TTS
        stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TTS To I Step -1

            If JX = 1 Then
                If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            Else
                'JX = 2
                If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            End If

        Next J
    Next I

End Sub

'----------------------------------------
'       GetListLength Subroutine
'----------------------------------------
'...explanation...
Sub GetListLength (QW, LG)

    Color , 0
    Cls

    If PV <> 1 Then
        Do

            Print
            Print
            Print
            Print
            Print
            J = 2

            Locate 10, 20: Color 4, 15: Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";
            For II = 1 To J: Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";: Next
            Locate 11 + J, 20: Print " " + Chr$(211); String$(30, 196); Chr$(189) + " ": Locate 11

            Locate , 25: Color 1: Print "MAXIMUM = "; QW
            Locate , 25: Input "ENTER LENGTH OF LIST"; LG

        Loop Until LG <= QW

    End If

End Sub
