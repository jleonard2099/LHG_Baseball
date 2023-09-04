'----------------------------------------
'            HD2HD Subroutine
'----------------------------------------
'This subroutine presents the user a menu
'of basic options for viewing Head-to-Head 
'team comparisons.
Sub HD2HD ()

    _Title "Full Count Baseball - Head-to-Head Stats"

    ReDim menuChoices$(1 To 4)

    Do

        Color 15, 0
        Cls

        Call TITLE_UTILS

        menuChoices$(1) = "ANALYZE A TEAM'S RECORD                "
        menuChoices$(2) = "ANALYZE ANOTHER TEAM IN THE SAME LEAGUE"
        menuChoices$(3) = "SELECT A NEW TEAM AND LEAGUE           "
        menuChoices$(4) = "RETURN TO MAIN MENU                    "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 22

        Call box0(7, 21, 12, 61, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%
                ' We need to pass league$ as a variable
                ' So that we can look at other teams
                ' in the same league (option 2)
                Case 1:
                    Call Hd2Hd_League(league$, 0)

                Case 2:
                    Call Hd2Hd_League(league$, 1)

                Case 3:
                    Call Hd2Hd_League(league$, 0)

                Case 4:
                    Cls

            End Select

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'     Hd2Hd_League Subroutine
'----------------------------------------
'This subroutine reads in stats from the
'selected team and then presents the user 
'a menu of options for viewing Head-to-Head 
'team comparisons.
Sub Hd2Hd_League (LG$, sameLeague)

    Shared H%(), I%(), V%(), Z1%()
    Shared HW%(), AW%(), HL(), AL()
    Shared R1%(), R2%(), R3%(), R4%()
    Shared TR1%(), TR2%(), TR3%(), TR4%()
    Shared THW%(), THL%(), TAW%(), TAL%()

    Shared DV$(), L$(), Z1$(), Z2$()

    Shared D1$, D2$, D3$, D4$

    Dim O$(NUM_STATRECORDS), LP$(NUM_STATRECORDS), WP$(NUM_STATRECORDS)

    Do

        Call ChooseTeam(YN$, NTMS, teamNbr, team$)
        statFile$ = nameStatFile$(team$)

        Cls

        If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

            MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            K$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        End If

    Loop Until _FileExists(diskPaths$(1) + statFile$ + "S")

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, YY%

    For J = 1 To 162: Input #1, H%(J): Next
    For J = 1 To 162: Input #1, O$(J): Next
    For J = 1 To 162: Input #1, V%(J): Next
    For J = 1 To 162: Input #1, L$(J): Next
    For J = 1 To 162: Input #1, WP$(J): Next
    For J = 1 To 162: Input #1, LP$(J): Next
    For J = 1 To 162: Input #1, I%(J): Next

    Close #1

    Do

        If sameLeague <> 1 Then leagueFile$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)

        If (sameLeague <> 1 And ExitCode% <> 27 And ExitCode% <> 0) Or sameLeague = 1 Then

            Color 7, 0
            Cls

            Call MenuBox (8, 8, 61, 5, 15, 1)

            Locate 7, 32

            Do

                Locate 10
                Color 7, 1

                Locate , 30: Print "STATS TO ";
                Color 15: Print "S";: Color 7: Print "CREEN"
                Locate , 30: Print "STATS TO ";
                Color 15: Print "P";: Color 7: Print "RINTER"
                Locate , 30: Print "STATS TO ";
                Color 15: Print "F";: Color 7: Print "ILE"

                J$ = GetKeyPress$

                Color 14, 0

            Loop Until UCase$(J$) = "F" Or UCase$(J$) = "P" Or UCase$(J$) = "S"

            If UCase$(J$) = "S" Then
                VPP = 1
            Else
                VPP = 0
            End If

            Cls

            Open diskPaths$(2) + leagueFile$ + ".LIB" For Input As #3

            Input #3, LG$
            Input #3, D1$
            Input #3, D2$
            Input #3, D3$
            Input #3, D4$

            For X = 1 To 40
                Input #3, Z1$(X)
                Input #3, Z1%(X)
                Input #3, Z2$(X)
            Next

            Close #3

            Color 15
            Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

            'Initialization for 1st league
            For Q = 1 To 40:
                HW%(Q) = 0
                AW%(Q) = 0
                AL%(Q) = 0
                HL%(Q) = 0
                R1%(Q) = 0
                R2%(Q) = 0
                R3%(Q) = 0
                R4%(Q) = 0
            Next

            For K = 1 To 4:
                THW%(K) = 0
                THL%(K) = 0
                TAW%(K) = 0
                TAL%(K) = 0
                TR1%(K) = 0
                TR2%(K) = 0
                TR3%(K) = 0
                TR4%(K) = 0
            Next

            'Calculations for 1st league
            For K = 1 To 40

                If Z1$(K) <> "XXX" Then

                    For I = 1 To YY%
                        If H%(I) > V%(I) And L$(I) = "A" And O$(I) = Z1$(K) Then AW%(K) = AW%(K) + 1
                        If H%(I) > V%(I) And L$(I) = "H" And O$(I) = Z1$(K) Then HW%(K) = HW%(K) + 1
                        If H%(I) < V%(I) And L$(I) = "A" And O$(I) = Z1$(K) Then AL%(K) = AL%(K) + 1
                        If H%(I) < V%(I) And L$(I) = "H" And O$(I) = Z1$(K) Then HL%(K) = HL%(K) + 1
                        If O$(I) = Z1$(K) And L$(I) = "H" Then R1%(K) = R1%(K) + H%(I)
                        If O$(I) = Z1$(K) And L$(I) = "H" Then R2%(K) = R2%(K) + V%(I)
                        If O$(I) = Z1$(K) And L$(I) = "A" Then R3%(K) = R3%(K) + H%(I)
                        If O$(I) = Z1$(K) And L$(I) = "A" Then R4%(K) = R4%(K) + V%(I)

                    Next I

                End If

            Next K

            'Generate output
            If VPP = 0 Then
                Call Hd2Hd_Printer(B$)
            Else
                Call Hd2Hd_Screen(B$)
            End If 'Done checking VPP

            Cls

            Call TITLE_UTILS

            Call MenuBox (8, 8, 61, 6, 15, 1)

            Do

                Locate 10, 15

                Color 3: Locate , 25: Print "SELECT OPTION:"
                Color 7: Locate , 25: Print "VIEW ";
                Color 15: Print "S";
                Color 7: Print "AME TEAM AGAIN"
                Locate , 25: Color 15: Print "R";
                Color 7: Print "ETURN TO MAIN MENU"
     
                I$ = GetKeyPress$

                Select Case UCase$(I$)

                    Case "S"

                        RDD = 0

                        For K = 1 To 4
                            THW%(K) = 0
                            THL%(K) = 0
                            TAW%(K) = 0
                            TAL%(K) = 0
                            TR1%(K) = 0
                            TR2%(K) = 0
                            TR3%(K) = 0
                            TR4%(K) = 0
                        Next

                End Select

            Loop Until UCase$(I$) = "S" Or UCase$(I$) = "R" Or I$ = Chr$(27) Or ExitCode% = 27

        End If

    Loop Until UCase$(I$) = "R" Or I$ = Chr$(27) Or ExitCode% = 27

End Sub


'----------------------------------------
'       Hd2Hd_Printer Subroutine
'----------------------------------------
'This subroutine outputs the calculated 
'comparison values to the default printer
Sub Hd2Hd_Printer (teamName$)

    Shared HW%(), HW%(), HL%(), AL%(), R1%(), R2%(), R3%(), R4%()

    Shared DV$(), Z1$()

    Shared D1$, D2$, D3$, D4$

    DV$(1) = D1$
    DV$(2) = D2$
    DV$(3) = D3$
    DV$(4) = D4$

    LPrint Tab(5); teamName$
    LPrint Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOTALS -------"
    'LPrint Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)
    LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"
    'LPrint Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

    Do

        RDD = RDD + 1

        DD = ((RDD - 1) * 10) + 1
        EE = RDD * 10

        If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

            LPrint Tab(2); DV$(RDD)

            For K = DD To EE

                If Z1$(K) <> "XXX" And Z1$(K) <> T$ Then

                    LPrint Z1$(K);
                    LPrint Tab(21); Using "##"; HW%(K);
                    LPrint Tab(25); Using "##"; HL%(K);

                    If HW%(K) = HW%(K) + HL%(K) And HW%(K) <> 0 Then LPrint Tab(28); "1.000";
                    If HW%(K) = 0 Then LPrint Tab(29); ".000";
                    If HW%(K) > 0 And HW%(K) < HW%(K) + HL%(K) Then statPCT! = (HW%(K) / (HW%(K) + HL%(K))): LPrint Tab(29); Using ".###"; statPCT!;

                    LPrint Tab(35); Using "###"; R1%(K);
                    LPrint Tab(39); Using "###"; R2%(K);
                    LPrint Tab(53); Using "##"; HW%(K);
                    LPrint Tab(57); Using "##"; AL%(K);

                    If HW%(K) = HW%(K) + AL%(K) And HW%(K) <> 0 Then LPrint Tab(60); "1.000";
                    If HW%(K) = 0 Then LPrint Tab(61); ".000";
                    If HW%(K) > 0 And HW%(K) < HW%(K) + AL%(K) Then statPCT! = (HW%(K) / (HW%(K) + AL%(K))): LPrint Tab(61); Using ".###"; statPCT!;

                    LPrint Tab(67); Using "###"; R3%(K);
                    LPrint Tab(71); Using "###"; R4%(K);
                    LPrint Tab(85); Using "##"; HW%(K) + HW%(K);
                    LPrint Tab(90); Using "##"; HL%(K) + AL%(K);

                    If HW%(K) + HW%(K) = HW%(K) + HW%(K) + HL%(K) + AL%(K) And HW%(K) + HW%(K) <> 0 Then LPrint Tab(95); "1.000";
                    If (HW%(K) + HW%(K)) = 0 Then LPrint Tab(96); ".000";
                    If (HW%(K) + HW%(K)) > 0 And (HW%(K) + HW%(K)) < (HW%(K) + HW%(K) + HL%(K) + AL%(K)) Then statPCT! = ((HW%(K) + HW%(K)) / (HW%(K) + HW%(K) + HL%(K) + AL%(K))): LPrint Tab(96); Using ".###"; statPCT!;

                    LPrint Tab(102); Using "###"; R1%(K) + R3%(K);
                    LPrint Tab(106); Using "###"; R2%(K) + R4%(K)

                    THW%(RDD) = THW%(RDD) + HW%(K)
                    THL%(RDD) = THL%(RDD) + HL%(K)
                    THW%(RDD) = THW%(RDD) + HW%(K)
                    TAL%(RDD) = TAL%(RDD) + AL%(K)
                    TR1%(RDD) = TR1%(RDD) + R1%(K)
                    TR2%(RDD) = TR2%(RDD) + R2%(K)
                    TR3%(RDD) = TR3%(RDD) + R3%(K)
                    TR4%(RDD) = TR4%(RDD) + R4%(K)

                End If

            Next K

            LPrint Tab(5); "TOTALS";
            LPrint Tab(21); Using "##"; THW%(RDD);
            LPrint Tab(25); Using "##"; THL%(RDD);

            If THW%(RDD) = THW%(RDD) + THL%(RDD) And THW%(RDD) <> 0 Then LPrint Tab(28); "1.000";
            If THW%(RDD) = 0 Then LPrint Tab(29); ".000";
            If THW%(RDD) > 0 And THW%(RDD) < THW%(RDD) + THL%(RDD) Then statPCT! = (THW%(RDD) / (THW%(RDD) + THL%(RDD))): LPrint Tab(29); Using ".###"; statPCT!;

            LPrint Tab(35); Using "###"; TR1%(RDD);
            LPrint Tab(39); Using "###"; TR2%(RDD);
            LPrint Tab(53); Using "##"; THW%(RDD);
            LPrint Tab(57); Using "##"; TAL%(RDD);

            If THW%(RDD) = THW%(RDD) + TAL%(RDD) And THW%(RDD) <> 0 Then LPrint Tab(60); "1.000";
            If THW%(RDD) = 0 Then LPrint Tab(61); ".000";
            If THW%(RDD) > 0 And THW%(RDD) < THW%(RDD) + TAL%(RDD) Then statPCT! = (THW%(RDD) / (THW%(RDD) + TAL%(RDD))): LPrint Tab(61); Using ".###"; statPCT!;

            LPrint Tab(67); Using "###"; TR3%(RDD);
            LPrint Tab(71); Using "###"; TR4%(RDD);
            LPrint Tab(84); Using "###"; THW%(RDD) + THW%(RDD);
            LPrint Tab(89); Using "###"; THL%(RDD) + TAL%(RDD);

            If THW%(RDD) + THW%(RDD) = THW%(RDD) + THW%(RDD) + THL%(RDD) + TAL%(RDD) And THW%(RDD) + THW%(RDD) <> 0 Then LPrint Tab(95); "1.000";
            If THW%(RDD) + THW%(RDD) = 0 Then LPrint Tab(96); ".000";
            If THW%(RDD) + THW%(RDD) > 0 And THW%(RDD) + THW%(RDD) < THW%(RDD) + THW%(RDD) + THL%(RDD) + TAL%(RDD) Then statPCT! = ((THW%(RDD) + THW%(RDD)) / (THW%(RDD) + THW%(RDD) + THL%(RDD) + TAL%(RDD))): LPrint Tab(96); Using ".###"; statPCT!;

            LPrint Tab(102); Using "###"; TR1%(RDD) + TR3%(RDD);
            LPrint Tab(106); Using "###"; TR2%(RDD) + TR4%(RDD)
            LPrint

        End If

    Loop Until RDD = 4

    LPrint
    LPrint
    LPrint
    LPrint
    LPrint Tab(22); "------- HOME -------"; Tab(54); "------- AWAY -------"; Tab(86); "-------- TOTALS -------"
    LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "RF"; Tab(40); "RA"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "RF"; Tab(72); "RA"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "RF"; Tab(107); "RA"

    Print

    For W = 1 To 4

        If DV$(W) <> "XXX" And DV$(W) <> "" Then

            LPrint Tab(2); DV$(W);
            LPrint Tab(21); Using "##"; THW%(W);
            LPrint Tab(25); Using "##"; THL%(W);

            If THW%(W) = THW%(W) + THL%(W) And THW%(W) <> 0 Then LPrint Tab(28); "1.000";
            If THW%(W) = 0 Then LPrint Tab(29); ".000";
            If THW%(W) > 0 And THW%(W) < THW%(W) + THL%(W) Then statPCT! = (THW%(W) / (THW%(W) + THL%(W))): LPrint Tab(29); Using ".###"; statPCT!;

            LPrint Tab(35); Using "###"; TR1%(W);
            LPrint Tab(39); Using "###"; TR2%(W);
            LPrint Tab(53); Using "##"; THW%(W);
            LPrint Tab(57); Using "##"; TAL%(W);

            If THW%(W) = THW%(W) + TAL%(W) And THW%(W) <> 0 Then LPrint Tab(60); "1.000";
            If THW%(W) = 0 Then LPrint Tab(61); ".000";
            If THW%(W) > 0 And THW%(W) < THW%(W) + TAL%(W) Then statPCT! = (THW%(W) / (THW%(W) + TAL%(W))): LPrint Tab(61); Using ".###"; statPCT!;

            LPrint Tab(67); Using "###"; TR3%(W);
            LPrint Tab(71); Using "###"; TR4%(W);
            LPrint Tab(84); Using "###"; THW%(W) + THW%(W);
            LPrint Tab(89); Using "###"; THL%(W) + TAL%(W);

            If THW%(W) + THW%(W) = THW%(W) + THW%(W) + THL%(W) + TAL%(W) And THW%(W) + THW%(W) <> 0 Then LPrint Tab(95); "1.000";
            If (THW%(W) + THW%(W)) = 0 Then LPrint Tab(96); ".000";
            If (THW%(W) + THW%(W)) > 0 And THW%(W) + THW%(W) < THW%(W) + THW%(W) + THL%(W) + TAL%(W) Then statPCT! = ((THW%(W) + THW%(W)) / (THW%(W) + THW%(W) + THL%(W) + TAL%(W))): LPrint Tab(96); Using ".###"; statPCT!;

            LPrint Tab(102); Using "###"; TR1%(W) + TR3%(W);
            LPrint Tab(106); Using "###"; TR2%(W) + TR4%(W)

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To 4
        THWW = THWW + THW%(I)
        THLL = THLL + THL%(I)
        TAWW = TAWW + THW%(I)
        TALL = TALL + TAL%(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To 4
        TR11 = TR11 + TR1%(I)
        TR22 = TR22 + TR2%(I)
        TR33 = TR33 + TR3%(I)
        TR44 = TR44 + TR4%(I)
    Next

    LPrint Tab(5); "TOTALS";
    LPrint Tab(20); Using "###"; THWW;
    LPrint Tab(24); Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then LPrint Tab(28); "1.000";
    If THWW = 0 Then LPrint Tab(29); ".000";
    If THWW > 0 And THWW < THWW + THLL Then statPCT! = THWW / (THWW + THLL): LPrint Tab(29); Using ".###"; statPCT!;

    LPrint Tab(35); Using "###"; TR11;
    LPrint Tab(39); Using "###"; TR22;
    LPrint Tab(52); Using "###"; TAWW;
    LPrint Tab(56); Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then LPrint Tab(60); "1.000";
    If TAWW = 0 Then LPrint Tab(61); ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then statPCT! = TAWW / (TAWW + TALL): LPrint Tab(61); Using ".###"; statPCT!;

    LPrint Tab(67); Using "###"; TR33;
    LPrint Tab(71); Using "###"; TR44;
    LPrint Tab(84); Using "###"; THWW + TAWW;
    LPrint Tab(89); Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then LPrint Tab(95); "1.000";
    If THWW + TAWW = 0 Then LPrint Tab(96); ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL): LPrint Tab(96); Using ".###"; statPCT!;

    LPrint Tab(102); Using "###"; TR11 + TR33;
    LPrint Tab(106); Using "###"; TR22 + TR44

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       Hd2Hd_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated 
'comparison values to the end user display
Sub Hd2Hd_Screen (teamName$)

    Shared HW%(), AW%(), HL%(), AL%()
    Shared R1%(), R2%(), R3%(), R4%()
    Shared teamRatings%()

    Shared DV$(), Z1$()

    Shared TR1%(), TR2%(), TR3%(), TR4%(), THW%(), THL%(), TAW%(), TAL%()

    Shared D1$, D2$, D3$, D4$

    DV$(1) = D1$
    DV$(2) = D2$
    DV$(3) = D3$
    DV$(4) = D4$

    Do

        Cls

        Locate 2, 35
        Color teamRatings%(11), teamRatings%(12): Print teamName$
        Color 3, 0

        Locate 4
        Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)
        Locate 5
        Print Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

        RDD = RDD + 1

        DD = ((RDD - 1) * 10) + 1
        EE = RDD * 10

        If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then RDD = 4

        Color 11
        Print Left$(DV$(RDD), 15);

        Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

        For K = DD To EE

            If Z1$(K) <> "XXX" And Z1$(K) <> teamName$ And Z1$(K) <> "" Then

                Color 15
                Locate , 1: Print Left$(Z1$(K), 13);

                Color 7
                Locate , 15: Print Using "##"; HW%(K);
                Locate , 18: Print Using "##"; HL%(K);
                If HW%(K) = HW%(K) + HL%(K) And HW%(K) <> 0 Then Locate , 21: Print "1.000";
                If HW%(K) = 0 Then Locate , 22: Print ".000";
                If HW%(K) > 0 And HW%(K) < HW%(K) + HL%(K) Then statPCT! = (HW%(K) / (HW%(K) + HL%(K))): Locate , 22: Print Using ".###"; statPCT!;
                Locate , 27: Print Using "###"; R1%(K);
                Locate , 31: Print Using "###"; R2%(K);

                Color 3: Print Tab(35); Chr$(179);

                Color 7
                Locate , 37: Print Using "##"; HW%(K);
                Locate , 40: Print Using "##"; AL%(K);
                If HW%(K) = HW%(K) + AL%(K) And HW%(K) <> 0 Then Locate , 43: Print "1.000";
                If HW%(K) = 0 Then Locate , 44: Print ".000";
                If HW%(K) > 0 And HW%(K) < HW%(K) + AL%(K) Then statPCT! = (HW%(K) / (HW%(K) + AL%(K))): Locate , 44: Print Using ".###"; statPCT!;
                Locate , 49: Print Using "###"; R3%(K);
                Locate , 53: Print Using "###"; R4%(K);

                Color 3: Print Tab(58); Chr$(179);

                Color 7
                Locate , 60: Print Using "##"; HW%(K) + HW%(K);
                Locate , 63: Print Using "##"; HL%(K) + AL%(K);
                If (HW%(K) + HW%(K)) = (HW%(K) + HW%(K) + HL%(K) + AL%(K)) And (HW%(K) + HW%(K)) <> 0 Then Locate , 66: Print "1.000";
                If (HW%(K) + HW%(K)) = 0 Then Locate , 67: Print ".000";
                If (HW%(K) + HW%(K)) > 0 And (HW%(K) + HW%(K)) < (HW%(K) + HW%(K) + HL%(K) + AL%(K)) Then statPCT! = ((HW%(K) + HW%(K)) / (HW%(K) + HW%(K) + HL%(K) + AL%(K))): Locate , 67: Print Using ".###"; statPCT!;
                Locate , 72: Print Using "###"; R1%(K) + R3%(K);
                Locate , 76: Print Using "###"; R2%(K) + R4%(K)
                Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179);
                THW%(RDD) = THW%(RDD) + HW%(K)
                THL%(RDD) = THL%(RDD) + HL%(K)
                TAW%(RDD) = TAW%(RDD) + AW%(K)
                TAL%(RDD) = TAL%(RDD) + AL%(K)
                TR1%(RDD) = TR1%(RDD) + R1%(K)
                TR2%(RDD) = TR2%(RDD) + R2%(K)
                TR3%(RDD) = TR3%(RDD) + R3%(K)
                TR4%(RDD) = TR4%(RDD) + R4%(K)

            End If

        Next K

        Print

        Color 11
        Locate , 5: Print "TOTALS";
        Locate , 15: Print Using "##"; THW%(RDD);
        Locate , 18: Print Using "##"; THL%(RDD);
        If THW%(RDD) = THW%(RDD) + THL%(RDD) And THW%(RDD) <> 0 Then Locate , 21: Print "1.000";
        If THW%(RDD) = 0 Then Locate , 22: Print ".000";
        If THW%(RDD) > 0 And THW%(RDD) < THW%(RDD) + THL%(RDD) Then statPCT! = (THW%(RDD) / (THW%(RDD) + THL%(RDD))): Locate , 22: Print Using ".###"; statPCT!;
        Locate , 27: Print Using "###"; TR1%(RDD);
        Locate , 31: Print Using "###"; TR2%(RDD);

        Color 3: Print Tab(35); Chr$(179);

        Color 11
        Locate , 37: Print Using "##"; TAW%(RDD);
        Locate , 40: Print Using "##"; TAL%(RDD);
        If TAW%(RDD) = TAW%(RDD) + TAL%(RDD) And TAW%(RDD) <> 0 Then Locate , 43: Print "1.000";
        If TAW%(RDD) = 0 Then Locate , 44: Print ".000";
        If TAW%(RDD) > 0 And TAW%(RDD) < TAW%(RDD) + TAL%(RDD) Then statPCT! = (TAW%(RDD) / (TAW%(RDD) + TAL%(RDD))): Locate , 44: Print Using ".###"; statPCT!;
        Locate , 49: Print Using "###"; TR3%(RDD);
        Locate , 53: Print Using "###"; TR4%(RDD);
        Color 3: Print Tab(58); Chr$(179);

        Color 11
        Locate , 59: Print Using "###"; THW%(RDD) + TAW%(RDD);
        Locate , 62: Print Using "###"; THL%(RDD) + TAL%(RDD);
        If THW%(RDD) + TAW%(RDD) = THW%(RDD) + THL%(RDD) + TAW%(RDD) + TAL%(RDD) And THW%(RDD) + TAW%(RDD) <> 0 Then Locate , 66: Print "1.000";
        If THW%(RDD) + TAW%(RDD) = 0 Then Locate , 67: Print ".000";
        If THW%(RDD) + TAW%(RDD) > 0 And THW%(RDD) + TAW%(RDD) < THW%(RDD) + TAW%(RDD) + THL%(RDD) + TAL%(RDD) Then statPCT! = (THW%(RDD) + TAW%(RDD)) / (THW%(RDD) + TAW%(RDD) + THL%(RDD) + TAL%(RDD)): Locate , 67: Print Using ".###"; statPCT!;
        Locate , 72: Print Using "###"; TR1%(RDD) + TR3%(RDD);
        Locate , 76: Print Using "###"; TR2%(RDD) + TR4%(RDD)

        Color 14
        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

    Loop Until RDD = 4

    Cls

    Locate 2, 35
    Color teamRatings%(11), teamRatings%(12): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(15); String$(7, 196); " HOME "; String$(7, 196); Tab(35); Chr$(194); String$(7, 196); " AWAY "; String$(9, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(6, 196)
    Locate 5
    Print Tab(16); "W"; Tab(19); "L"; Tab(23); "PCT."; Tab(28); "RF"; Tab(32); "RA"; Tab(35); Chr$(179); Tab(38); "W"; Tab(41); "L"; Tab(45); "PCT."; Tab(50); "RF"; Tab(54); "RA"; Tab(58); Chr$(179); Tab(61); "W"; Tab(64); "L"; Tab(68); "PCT."; Tab(73); "RF"; Tab(77); "RA"

    Color 11: Print "DIVISIONS";

    Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179)

    For W = 1 To 4

        If DV$(W) <> "XXX" And DV$(W) <> "" Then

            Color 15
            'We have to limit this to 14 because we start stats on column 15
            Locate , 1: Print Left$(DV$(W), 14);

            Color 7
            Locate , 15: Print Using "##"; THW%(W);
            Locate , 18: Print Using "##"; THL%(W);
            If THW%(W) = THW%(W) + THL%(W) And THW%(W) <> 0 Then Locate , 21: Print "1.000"
            If THW%(W) = 0 Then Locate , 22: Print ".000";
            If THW%(W) > 0 And THW%(W) < THW%(W) + THL%(W) Then statPCT! = (THW%(W) / (THW%(W) + THL%(W))): Locate , 22: Print Using ".###"; statPCT!;
            Locate , 27: Print Using "###"; TR1%(W);
            Locate , 31: Print Using "###"; TR2%(W);

            Color 3: Print Tab(35); Chr$(179);

            Color 7
            Locate , 37: Print Using "##"; TAW%(W);
            Locate , 40: Print Using "##"; TAL%(W);
            If TAW%(W) = TAW%(W) + TAL%(W) And TAW%(W) <> 0 Then Locate , 43: Print "1.000";
            If TAW%(W) = 0 Then Locate , 44: Print ".000";
            If TAW%(W) > 0 And TAW%(W) < TAW%(W) + TAL%(W) Then statPCT! = (TAW%(W) / (TAW%(W) + TAL%(W))): Locate , 44: Print Using ".###"; statPCT!;
            Locate , 49: Print Using "###"; TR3%(W);
            Locate , 53: Print Using "###"; TR4%(W);

            Color 3: Print Tab(58); Chr$(179);

            Color 7
            Locate , 59: Print Using "###"; THW%(W) + TAW%(W);
            Locate , 62: Print Using "###"; THL%(W) + TAL%(W);
            If THW%(W) + TAW%(W) = THW%(W) + TAW%(W) + THL%(W) + TAL%(W) And THW%(W) + TAW%(W) <> 0 Then Locate , 66: Print "1.000";
            If (THW%(W) + TAW%(W)) = 0 Then Locate , 67: Print ".000";
            If (THW%(W) + TAW%(W)) > 0 And THW%(W) + TAW%(W) < THW%(W) + TAW%(W) + THL%(W) + TAL%(W) Then statPCT! = ((THW%(W) + TAW%(W)) / (THW%(W) + TAW%(W) + THL%(W) + TAL%(W))): Locate , 67: Print Using ".###"; statPCT!;
            Locate , 72: Print Using "###"; TR1%(W) + TR3%(W);
            Locate , 76: Print Using "###"; TR2%(W) + TR4%(W)
            Color 3: Print Tab(35); Chr$(179); Tab(58); Chr$(179);

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To 4
        THWW = THWW + THW%(I)
        THLL = THLL + THL%(I)
        TAWW = TAWW + THW%(I)
        TALL = TALL + TAL%(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To 4
        TR11 = TR11 + TR1%(I)
        TR22 = TR22 + TR2%(I)
        TR33 = TR33 + TR3%(I)
        TR44 = TR44 + TR4%(I)
    Next

    Print

    Color 11: Locate , 3: Print "TOTALS";
    Locate , 14: Print Using "###"; THWW;
    Locate , 17: Print Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then Locate , 21: Print "1.000";
    If THWW = 0 Then Locate , 22: Print ".000";
    If THWW > 0 And THWW < THWW + THLL Then statPCT! = THWW / (THWW + THLL): Locate , 22: Print Using ".###"; statPCT!;

    Locate , 27: Print Using "###"; TR11;
    Locate , 31: Print Using "###"; TR22;
    Color 3: Print Tab(35); Chr$(179);
    Color 11: Locate , 36: Print Using "###"; TAWW;
    Locate , 39: Print Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then Locate , 43: Print "1.000";
    If TAWW = 0 Then Locate , 44: Print ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then statPCT! = TAWW / (TAWW + TALL): Locate , 44: Print Using ".###"; statPCT!;

    Locate , 49: Print Using "###"; TR33;
    Locate , 53: Print Using "###"; TR44;
    Color 3: Print Tab(58); Chr$(179);
    Color 11: Locate , 59: Print Using "###"; THWW + TAWW;
    Locate , 62: Print Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Locate , 66: Print "1.000";
    If THWW + TAWW = 0 Then Locate , 67: Print ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL): Locate , 67: Print Using ".###"; statPCT!;

    Locate , 72: Print Using "###"; TR11 + TR33;
    Locate , 76: Print Using "###"; TR22 + TR44

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub
