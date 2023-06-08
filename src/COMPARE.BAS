Sub COMPARE ()

    _Title "Full Count Baseball - Compare Stats"

    ReDim menuChoices$(1 To 2)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "VIEW/PRINT STATISTICAL COMPARISONS"
        menuChoices$(2) = "RETURN TO UTILITIES MENU          "
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 24

        Call box0(6, 23, 9, 58, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call ViewComparisons
                Case 2: Cls
            End Select
        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub

'----------------------------------------
'      ViewComparisons SubRoutine
'----------------------------------------
'...explanation...
Sub ViewComparisons ()

    Shared draftB%(), ratings%(), draftP%(), TB%()
    Shared statB$(), statP$()
    Shared compareB0%(), compareP0%()
    Shared TP&()

    Shared ERX!

    'Dim statT%(0 to 21), statT1%(0 to 22)
    Shared statT%(), statT1%()

    Dim compareB1%(23), compareP1%(22)

    Do
        Color 15, 0

		Cls

        JJ = 7

        Locate 6, 8: Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";
		
		For II = 1 To JJ: 
			Locate 6 + II, 8
			Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
		Next

        Locate 7 + JJ, 8

        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$
        If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_COMPARE(YN$, numberTeams)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
                BO% = 1
                Locate 10, 8
                Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
            Else

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342
                fileLength& = LOF(1)
                Field #1, 15 As Q$(0)
                For I1 = 1 to fileLength& / 4342
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

                Call SelectTeam_COMPARE(yearNumber$, numberTeams)

            End If 'Done checking for teams file

        End If 'Done checking INPUT

		T$ = RTrim$(Q$(0))
		
        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color ratings%(11), ratings%(12): Print T$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

    If ExitCode% <> 27 Then

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

        For K = 4 To 18:
            For I = 0 To 22
                TB%(K - 3) = TB%(K - 3) + draftB%(I, K)
            Next
        Next

        For K = 1 To 33
            For I = 0 To 21
                TP&(K) = TP&(K) + draftP%(I, K)
            Next
        Next

        ERX! = 0

        For I = 0 To 21
            ERX! = ERX! + ((draftP%(I, 6) / 9) * (draftP%(I, 10) / 100))
        Next

        For K = 4 To 14:
            For I = 0 To 21
                If draftP%(I, 11) = 999 Then TB%(K - 3) = TB%(K - 3) + draftP%(I, K + 16)
            Next
        Next

        Z$ = RTrim$(Z$)

        If Not _FileExists(diskPaths$(1) + Z$) Then
            Locate 10, 8
            Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            Open diskPaths$(1) + Z$ For Input As #1

            Input #1, A$:
            Input #1, W2%
            Input #1, L2%

            For I = 0 To 22: Input #1, statB$(I): Input #1, compareB1%(I): Next I
            For I = 0 To 21: Input #1, statP$(I): Input #1, compareP1%(I): Next I

            For I = 0 To 22: For I1 = 0 To 21: Input #1, compareB0%(I, I1): Next I1: Next I
            For I = 0 To 21: For I1 = 0 To 41: Input #1, compareP0%(I, I1): Next I1: Next I

            Input #1, D0%
            Input #1, D1%

            For I = 0 To 21: Input #1, statT%(I): Next I
            For I = 0 To 22: Input #1, statT1%(I): Next I

            Close #1

            Color 15, 0
            Cls

            Do
                Locate 12, 5
                Color 12: Print "V";: Color 7: Print "iew or ";
                Color 12: Print "P";: Color 7: Print "rint"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

            If UCase$(I$) = "P" Then
                'PRINT
                Call Comparisons_Printer(T$)
            Else
                'VIEW
                backtoMenu = 0
                Call Comparisons_Screen(T$)
                If backtoMenu = 1 Then Exit Sub
            End If

        End If 'Done checking file count

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'   Comparisons_Printer Subroutine
'----------------------------------------
'...explanation...
Sub Comparisons_Printer (teamName$)

    Shared statB$(), statP$()
    Shared draftB%(), draftP%()
    Shared compareB0%(), compareP0%()

    LPrint "STAT COMPARISONS FOR "; teamName$
    LPrint
	LPrint
    LPrint "BATTER";
    LPrint Tab(21); "  G";
    LPrint Tab(26); " AB";
    LPrint Tab(31); "  R";
    LPrint Tab(36); "  H";
    LPrint Tab(41); " 2B";
    LPrint Tab(45); " 3B";
    LPrint Tab(49); " HR";
    LPrint Tab(54); "RBI";
    LPrint Tab(59); " BB";
    LPrint Tab(64); " SO";
    LPrint Tab(69); " SB";
    LPrint Tab(74); " CS";
    LPrint Tab(79); "AVG";
    LPrint Tab(85); "SLG";
    LPrint Tab(91); "OBP";
    LPrint Tab(97); "FLD.PCT"

    For I = 0 To 22

        If statB$(I) <> "XXX" And statB$(I) <> "END" And statB$(I) <> "" Then
            LPrint Left$(statB$(I), 14);
            LPrint Tab(16); "Reg.";
            LPrint Tab(21); Using "###"; draftB%(I, 3);
            LPrint Tab(26); Using "###"; draftB%(I, 4);
            LPrint Tab(31); Using "###"; draftB%(I, 5);
            LPrint Tab(36); Using "###"; draftB%(I, 6);
            LPrint Tab(42); Using "##"; draftB%(I, 7);
            LPrint Tab(46); Using "##"; draftB%(I, 8);
            LPrint Tab(50); Using "##"; draftB%(I, 9);
            LPrint Tab(54); Using "###"; draftB%(I, 10);
            LPrint Tab(59); Using "###"; draftB%(I, 11);
            LPrint Tab(64); Using "#.###"; draftB%(I, 12);
            LPrint Tab(69); Using "#.###"; draftB%(I, 13);
            LPrint Tab(74); Using "#.###"; draftB%(I, 14);

            I2 = 0
            I3 = 0
            I4 = 0

            If draftB%(I, 4) > 0 Then I2 = Int((draftB%(I, 6) / draftB%(I, 4) + .0005) * 1000): I3 = Int(((draftB%(I, 6) + draftB%(I, 7) + 2 * draftB%(I, 8) + 3 * draftB%(I, 9)) / draftB%(I, 4) + .0005) * 1000): I4 = Int(((draftB%(I, 6) + draftB%(I, 11)) / (draftB%(I, 11) + draftB%(I, 4)) + .0005) * 1000)
            If draftB%(I, 4) = 0 Or I2 = 0 Then LPrint Tab(79); ".000";
            If I2 > 0 And I2 < 100 Then LPrint Tab(79); ".0";: LPrint Using "##"; I2;
            If I2 >= 100 Then LPrint Tab(79); ".";: LPrint Using "###"; I2;
            If draftB%(I, 4) = 0 Or I3 = 0 Then LPrint Tab(84); ".000";
            If I3 > 0 And I3 < 100 Then LPrint Tab(84); ".0";: LPrint Using "##"; I3;
            If I3 >= 100 Then LPrint Tab(84); ".";: LPrint Using "###"; I3;
            If draftB%(I, 4) = 0 Or I4 = 0 Then LPrint Tab(90); ".000";
            If I4 > 0 And I4 < 100 Then LPrint Tab(90); ".0";: LPrint Using "##"; I4;
            If I4 >= 100 Then LPrint Tab(90); ".";: LPrint Using "###"; I4;

            LPrint Tab(99); ".";: LPrint Using "###"; draftB%(I, 26)
            LPrint Tab(16); "Lge.";
            LPrint Tab(21); Using "###"; compareB0%(I, 14);
            LPrint Tab(26); Using "###"; compareB0%(I, 0);
            LPrint Tab(31); Using "###"; compareB0%(I, 2);
            LPrint Tab(36); Using "###"; compareB0%(I, 1);
            LPrint Tab(42); Using "##"; compareB0%(I, 4);
            LPrint Tab(46); Using "##"; compareB0%(I, 5);
            LPrint Tab(50); Using "##"; compareB0%(I, 6);
            LPrint Tab(54); Using "###"; compareB0%(I, 3);
            LPrint Tab(59); Using "###"; compareB0%(I, 7);
            LPrint Tab(64); Using "###"; compareB0%(I, 8);
            LPrint Tab(69); Using "###"; compareB0%(I, 9);
            LPrint Tab(74); Using "###"; compareB0%(I, 10);

            I2 = 0
            I3 = 0
            I4 = 0

            If compareB0%(I, 0) > 0 Then I2 = Int((compareB0%(I, 1) / compareB0%(I, 0) + .0005) * 1000): I3 = Int(((compareB0%(I, 1) + compareB0%(I, 4) + 2 * compareB0%(I, 5) + 3 * compareB0%(I, 6)) / compareB0%(I, 0) + .0005) * 1000): I4 = Int(((compareB0%(I, 1) + compareB0%(I, 7)) / (compareB0%(I, 7) + compareB0%(I, 0)) + .0005) * 1000)
            If compareB0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(79); ".000";
            If I2 > 0 And I2 < 100 Then LPrint Tab(79); ".0";: LPrint Using "##"; I2;
            If I2 >= 100 Then LPrint Tab(79); ".";: LPrint Using "###"; I2;
            If compareB0%(I, 0) = 0 Or I3 = 0 Then LPrint Tab(84); ".000";
            If I3 > 0 And I3 < 100 Then LPrint Tab(84); ".0";: LPrint Using "##"; I3;
            If I3 >= 100 Then LPrint Tab(84); ".";: LPrint Using "###"; I3;
            If compareB0%(I, 0) = 0 Or I4 = 0 Then LPrint Tab(90); ".000";
            If I4 > 0 And I4 < 100 Then LPrint Tab(90); ".0";: LPrint Using "##"; I4;
            If I4 >= 100 Then LPrint Tab(90); ".";: LPrint Using "###"; I4;
            If compareB0%(I, 16) = 0 And compareB0%(I, 17) = 0 Then I5 = 0 Else I5 = ((compareB0%(I, 16) + compareB0%(I, 17)) / (compareB0%(I, 16) + compareB0%(I, 17) + compareB0%(I, 11))) * 1000
            If I5 = 0 Then LPrint Tab(99); ".000  "
            If I5 > 0 And I5 < 1000 Then LPrint Tab(99); ".";: LPrint Using "###"; I5
            If I5 = 1000 Then LPrint Tab(98); Using "#"; I5 / 1000;: LPrint ".000  "
            LPrint

        End If

    Next I

    Call TeamComparison_Printer(teamName$)

    LPrint Chr$(12)
    LPrint "STAT COMPARISONS FOR "; teamName$
    LPrint
    LPrint
    LPrint "PITCHER";
    LPrint Tab(25); "  G";
    LPrint Tab(30); "  W";
    LPrint Tab(35); "  L";
    LPrint Tab(40); " SV";
    LPrint Tab(47); " IP";
    LPrint Tab(52); "  H";
    LPrint Tab(56); " BB";
    LPrint Tab(61); " SO";
    LPrint Tab(67); "ERA";
    LPrint Tab(72); " GS";
    LPrint Tab(78); " CG";
    LPrint Tab(85); "H/9IP";
    LPrint Tab(92); "SO/9IP";
    LPrint Tab(99); "BB/9IP";
    LPrint Tab(107); "HR ALL."

    For I = 0 To 21

        If statP$(I) <> "XXX" And statP$(I) <> "END" And statP$(I) <> "" Then

            LPrint Left$(statP$(I), 14);
            LPrint Tab(18); "REG.";
            LPrint Tab(25); Using "###"; draftP%(I, 4);
            LPrint Tab(31); Using "##"; draftP%(I, 1);
            LPrint Tab(36); Using "##"; draftP%(I, 2);
            LPrint Tab(41); Using "##"; draftP%(I, 3);
            LPrint Tab(45); Using "###.#"; draftP%(I, 6);
            LPrint Tab(52); Using "###"; draftP%(I, 7);
            LPrint Tab(56); Using "###"; draftP%(I, 8);
            LPrint Tab(61); Using "###"; draftP%(I, 9);
            LPrint Tab(66); Using "##.##"; (draftP%(I, 10)) / 100;
            LPrint Tab(73); Using "##"; draftP%(I, 5);
            LPrint Tab(79); Using "##"; draftP%(I, 16);

            If draftP%(I, 6) = 0 Then LPrint Tab(85); " 0.00";
            If draftP%(I, 6) > 0 Then LPrint Tab(85); Using "##.##"; (draftP%(I, 7) * 9) / draftP%(I, 6);
            If draftP%(I, 6) = 0 Then LPrint Tab(93); " 0.00";
            If draftP%(I, 6) > 0 Then LPrint Tab(93); Using "##.##"; (draftP%(I, 9) * 9) / draftP%(I, 6);
            If draftP%(I, 6) = 0 Then LPrint Tab(100); " 0.00";
            If draftP%(I, 6) > 0 Then LPrint Tab(100); Using "##.##"; (draftP%(I, 8) * 9) / draftP%(I, 6);

            LPrint Tab(109); Using "##"; draftP%(I, 13)
            LPrint Tab(18); "LGE.";
            LPrint Tab(25); Using "###"; compareP0%(I, 8);
            LPrint Tab(31); Using "##"; compareP0%(I, 6);
            LPrint Tab(36); Using "##"; compareP0%(I, 7);
            LPrint Tab(41); Using "##"; compareP0%(I, 12);
            LPrint Tab(45); Using "###"; Int(compareP0%(I, 0) / 3);

            If compareP0%(I, 0) - (Int(compareP0%(I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; compareP0%(I, 0) - (Int(compareP0%(I, 0) / 3) * 3);

            LPrint Tab(52); Using "###"; compareP0%(I, 1);
            LPrint Tab(56); Using "###"; compareP0%(I, 4);
            LPrint Tab(61); Using "###"; compareP0%(I, 5);

            I2 = 0
            If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 3) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If compareP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(66); " 0.00";
            If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(66); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(66); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            LPrint Tab(73); Using "##"; compareP0%(I, 9);
            LPrint Tab(79); Using "##"; compareP0%(I, 10);

            I2 = 0
            If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 1) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If compareP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(85); " 0.00";
            If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(85); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(85); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            I2 = 0
            If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 5) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If compareP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(93); " 0.00";
            If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(93); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(93); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            I2 = 0
            If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 4) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
            I3 = Int(I2 / 100)
            I4 = I2 - (I3 * 100)

            If compareP0%(I, 0) = 0 Or I2 = 0 Then LPrint Tab(100); " 0.00";
            If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then LPrint Tab(100); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I2 >= 1000 Then LPrint Tab(100); Using "##"; I3;: LPrint ".";
            If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then LPrint "00";
            If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then LPrint "0";: LPrint Using "#"; I4;
            If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then LPrint Using "##"; I4;

            LPrint Tab(109); Using "##"; compareP0%(I, 14)
            LPrint

        End If

    Next

    LPrint Chr$(12)

End Sub

'----------------------------------------
'   TeamComparison_Printer Subroutine
'----------------------------------------
'...explanation...
Sub TeamComparison_Printer (teamName$)

    Shared statB$()
    Shared draftB%(), compareB0%()

    LPrint Chr$(12)
    LPrint "STAT COMPARISONS FOR "; teamName$
    LPrint
    LPrint
    LPrint Tab(20); " [ - - - - - - - - - - AT BATS PER - - - - - - - - - - - ]"
    LPrint "BATTER";
    LPrint Tab(26); "R";
    LPrint Tab(33); "H";
    LPrint Tab(40); "2B";
    LPrint Tab(47); "3B";
    LPrint Tab(54); "HR";
    LPrint Tab(61); "RBI";
    LPrint Tab(68); "BB";
    LPrint Tab(75); "SO";
    LPrint Tab(79); "SUCCESS % - SB"

    For I = 0 To 16

        If statB$(I) <> "XXX" And statB$(I) = "END" And statB$(I) <> "" Then

            LPrint Left$(statB$(I), 14);
            LPrint Tab(16); "Reg.";

            If draftB%(I, 4) = 0 Or draftB%(I, 5) = 0 Then LPrint Tab(21); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 5) > 0 Then LPrint Tab(21); Using "###.##"; draftB%(I, 4) / draftB%(I, 5);
            If draftB%(I, 4) = 0 Or draftB%(I, 6) = 0 Then LPrint Tab(28); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 6) > 0 Then LPrint Tab(28); Using "###.##"; draftB%(I, 4) / draftB%(I, 6);
            If draftB%(I, 4) = 0 Or draftB%(I, 7) = 0 Then LPrint Tab(36); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 7) > 0 Then LPrint Tab(36); Using "###.##"; draftB%(I, 4) / draftB%(I, 7);
            If draftB%(I, 4) = 0 Or draftB%(I, 8) = 0 Then LPrint Tab(43); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 8) > 0 Then LPrint Tab(43); Using "###.##"; draftB%(I, 4) / draftB%(I, 8);
            If draftB%(I, 4) = 0 Or draftB%(I, 9) = 0 Then LPrint Tab(50); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 9) > 0 Then LPrint Tab(50); Using "###.##"; draftB%(I, 4) / draftB%(I, 9);
            If draftB%(I, 4) = 0 Or draftB%(I, 10) = 0 Then LPrint Tab(58); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 10) > 0 Then LPrint Tab(58); Using "###.##"; draftB%(I, 4) / draftB%(I, 10);
            If draftB%(I, 4) = 0 Or draftB%(I, 11) = 0 Then LPrint Tab(64); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 11) > 0 Then LPrint Tab(64); Using "###.##"; draftB%(I, 4) / draftB%(I, 11);
            If draftB%(I, 4) = 0 Or draftB%(I, 12) = 0 Then LPrint Tab(71); "---.--";
            If draftB%(I, 4) > 0 And draftB%(I, 12) > 0 Then LPrint Tab(71); Using "###.##"; draftB%(I, 4) / draftB%(I, 12);
            If draftB%(I, 13) + draftB%(I, 14) = 0 Or draftB%(I, 13) = 0 Then LPrint Tab(83); " 00%";
            If draftB%(I, 13) > 0 And draftB%(I, 14) = 0 Then LPrint Tab(83); "100%";
            If draftB%(I, 13) > 0 And draftB%(I, 14) > 0 Then LPrint Tab(83); Using "###"; (draftB%(I, 13) / (draftB%(I, 13) + draftB%(I, 14))) * 100;: LPrint Tab(86); "%"

            LPrint Tab(16); "Leg.";

            If compareB0%(I, 0) = 0 Or compareB0%(I, 2) = 0 Then LPrint Tab(21); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 2) > 0 Then LPrint Tab(21); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 2);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 1) = 0 Then LPrint Tab(28); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 1) > 0 Then LPrint Tab(28); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 1);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 4) = 0 Then LPrint Tab(36); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 4) > 0 Then LPrint Tab(36); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 4);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 5) = 0 Then LPrint Tab(43); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 5) > 0 Then LPrint Tab(43); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 5);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 6) = 0 Then LPrint Tab(50); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 6) > 0 Then LPrint Tab(50); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 6);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 3) = 0 Then LPrint Tab(58); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 3) > 0 Then LPrint Tab(58); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 3);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 7) = 0 Then LPrint Tab(64); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 7) > 0 Then LPrint Tab(64); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 7);
            If compareB0%(I, 0) = 0 Or compareB0%(I, 8) = 0 Then LPrint Tab(71); "---.--";
            If compareB0%(I, 0) > 0 And compareB0%(I, 8) > 0 Then LPrint Tab(71); Using "###.##"; compareB0%(I, 0) / compareB0%(I, 8);
            If compareB0%(I, 9) + compareB0%(I, 10) = 0 Or compareB0%(I, 9) = 0 Then LPrint Tab(83); " 00%"
            If compareB0%(I, 9) > 0 And compareB0%(I, 10) = 0 Then LPrint Tab(83); "100%"
            If compareB0%(I, 9) > 0 And compareB0%(I, 10) > 0 Then LPrint Tab(83); Using "###"; (compareB0%(I, 9) / (compareB0%(I, 10) + compareB0%(I, 9))) * 100;: LPrint Tab(86); "%"

        End If

    Next

End Sub

'----------------------------------------
'     Comparisons_Screen Subroutine
'----------------------------------------
'...explanation...
Sub Comparisons_Screen (teamName$)

    Shared statB$(), statP$()
    Shared draftB%(), compareB0%()
    Shared compareP0%(), draftP%()

    Call PlayerStats_Screen(teamName$)

    200 '
    Do

        Color 0, 0: Cls
        For I = 0 To 22
            If Left$(statB$(I), 3) = "XXX" Then
                statB$(I) = ""
            End If
        Next

        For I = 0 To 21
            If Left$(statP$(I), 3) = "XXX" Then
                statP$(I) = ""
            End If
        Next

        Color 9
        Locate 1, 22: Print "STAT COMPARISONS FOR ";
        Color 15: Print teamName$

        For I = 0 To 16
            Color 12: Locate I + 4, 10: Print Using "##"; I + 1;: Print ". ";: Color 7: Print Left$(statB$(I), 14)
        Next

        For I = 17 To 22
            Color 12: Locate I - 13, 30: Print Using "##"; I + 1;: Print ". ";: Color 7: Print Left$(statB$(I), 14)
        Next

        For I = 0 To 10
            Color 12: Locate I + 10, 30: Print Using "##"; I + 24;: Print ". ";: Color 3: Print Left$(statP$(I), 14)
        Next

        For I = 11 To 21
            Color 12: Locate I - 7, 50: Print Using "##"; I + 24;: Print ". ";: Color 3: Print Left$(statP$(I), 14)
        Next

        Color 12: Locate , 50: Print "46. ";: Color 14: Print "View Players in Sequence"
        Color 12: Locate , 50: Print "47. ";: Color 14: Print "View Another Team"
        Color 12: Locate , 50: Print "48. ";: Color 14: Print "Exit to DOS"
        Color 12: Locate , 50: Print "49. ";: Color 14: Print "Return to UTILITIES menu"

        Do
            Color 15
            Locate 23, 30: Input "ENTER # TO VIEW "; B9
            startI = 0

            Select Case B9
                Case 1 To 23
                    startI = B9 - 1
                    Cls
                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next

                Case 24 To 45
                    startI = B9 - 24
                    Cls
                    For dc = 1 To 14
                        Color 15, 0: Locate dc, 1
                        Print String$(78, 177)
                    Next

                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next

                Case 46
                    BI = 1
                    Cls
                    For dc = 1 To 20
                        Color 15, 0: Locate dc, 1
                        Print String$(79, 177)
                    Next
                    startI = startI + 1

                Case 47: Exit Sub 'Call ViewComparisons
                Case 48: Cls: End: System
                Case 49: backtoMenu = 1: Exit Sub
            End Select

        Loop Until B9 >= 1 And B9 <= 49

        For I = startI To 22
            Locate 2, 30
            Color 15, 0: Print String$(40, 177)

            If statB$(I) <> "XXX" And statB$(I) <> "END" And statB$(I) <> "" Then

                265 '
                Select Case draftB%(I, 22)
                    Case 2: PST$ = "Catcher"
                    Case 3: PST$ = "First Base"
                    Case 4: PST$ = "Second Base"
                    Case 5: PST$ = "Third Base"
                    Case 6: PST$ = "Shortstop"
                    Case 7: PST$ = "Left Field"
                    Case 8: PST$ = "Center Field"
                    Case 9: PST$ = "Right Field"
                End Select

                Color 11
                Locate 2, 2: Print teamName$
                Color 15, 0
                Locate 2, 30: Print String$(14, 177)
                Color 10
                Locate 2, 30: Print Left$(statB$(I), 14);: Print " - ";: Print PST$
                Color 12
                Locate 5, 2: Print "REGULAR "
                Locate 6, 2: Print "LEAGUE  ";
                Color 7, 0
                Print String$(2, 177)
                Locate 7, 4: Print String$(8, 177)
                Color 0, 11
                Locate 4, 13: Print "  G  ";:

                Print Tab(19); "AB ";:
                Print Tab(24); "R  ";:
                Print Tab(28); "H  ";:
                Print Tab(31); "2B ";:
                Print Tab(35); "3B ";:
                Print Tab(39); "HR ";:
                Print Tab(43); "RBI ";:
                Print Tab(48); "BB ";:
                Print Tab(52); "SO ";:
                Print Tab(56); "SB ";:
                Print Tab(59); "CS ";:
                Print Tab(63); "AVG ";:
                Print Tab(68); "SLG ";:
                Print Tab(73); "OBP";:

                Color 0, 11
                Locate 9, 13: Print "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
                Locate 10, 13: Print "  RUNS";:

                Print Tab(21); " HITS";:
                Print Tab(30); " 2B ";:
                Print Tab(37); " 3B ";:
                Print Tab(44); " HR ";:
                Print Tab(50); " RBI ";:
                Print Tab(58); " BB ";:
                Print Tab(65); " SO ";:
                Print Tab(70); " %-SB ";: Color 7, 0: Print String$(2, 177)

                Color 0, 11
                Locate 15, 13: Print "FLD.PCT."
                Color 1, 11
                Locate 5, 13: Print Using "###"; draftB%(I, 3);

                If draftB%(I, 4) = 0 Or draftB%(I, 5) = 0 Then Locate 11, 13: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 5) > 0 Then Locate 11, 13: Print Using "###.##"; draftB%(I, 4) / draftB%(I, 5)

                Locate 5, 16: Print Using "#####"; draftB%(I, 4)

                If draftB%(I, 4) = 0 Or draftB%(I, 6) = 0 Then Locate 11, 19: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 6) > 0 Then Locate 11, 19: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 6)

                Locate 5, 21: Print Using "####"; draftB%(I, 5)

                If draftB%(I, 4) = 0 Or draftB%(I, 7) = 0 Then Locate 11, 26: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 7) > 0 Then Locate 11, 26: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 7)

                Locate 5, 25: Print Using "####"; draftB%(I, 6)

                If draftB%(I, 4) = 0 Or draftB%(I, 8) = 0 Then Locate 11, 33: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 8) > 0 Then Locate 11, 33: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 8)

                Locate 5, 29: Print Using "####"; draftB%(I, 7)

                If draftB%(I, 4) = 0 Or draftB%(I, 9) = 0 Then Locate 11, 40: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 9) > 0 Then Locate 11, 40: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 9)

                Locate 5, 33: Print Using "####"; draftB%(I, 8)

                If draftB%(I, 4) = 0 Or draftB%(I, 10) = 0 Then Locate 11, 47: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 10) > 0 Then Locate 11, 47: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 10)

                Locate 5, 37: Print Using "####"; draftB%(I, 9)

                If draftB%(I, 4) = 0 Or draftB%(I, 11) = 0 Then Locate 11, 54: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 11) > 0 Then Locate 11, 54: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 11)

                Locate 5, 41: Print Using "#####"; draftB%(I, 10)

                If draftB%(I, 4) = 0 Or draftB%(I, 12) = 0 Then Locate 11, 61: Print " ---.--"
                If draftB%(I, 4) > 0 And draftB%(I, 12) > 0 Then Locate 11, 61: Print Using "####.##"; draftB%(I, 4) / draftB%(I, 12)

                Locate 5, 46: Print Using "####"; draftB%(I, 11)
                Locate 5, 50: Print Using "####"; draftB%(I, 12)

                If draftB%(I, 13) + draftB%(I, 14) = 0 Or draftB%(I, 13) = 0 Then Locate 11, 68: Print "    00% ";: Color 7, 0: Print String$(2, 177)

                Color 1, 11: If draftB%(I, 13) > 0 And draftB%(I, 14) = 0 Then Locate 11, 68: Print "   100% ";: Color 7, 0: Print String$(2, 177)
                Color 1, 11: If draftB%(I, 13) > 0 And draftB%(I, 14) > 0 Then Locate 11, 68: Print "   ";: Print Using "###"; (draftB%(I, 13) / (draftB%(I, 13) + draftB%(I, 14))) * 100;: Print "% ";: Color 7, 0: Print String$(2, 177)
                Color 1, 11: Locate 5, 54: Print Using "####"; draftB%(I, 13)
                Locate 5, 58: Print Using "###"; draftB%(I, 14);: Print " "

                I2 = 0
                I3 = 0
                I4 = 0

                If draftB%(I, 4) > 0 Then I2 = Int((draftB%(I, 6) / draftB%(I, 4) + .0005) * 1000): I3 = Int(((draftB%(I, 6) + draftB%(I, 7) + 2 * draftB%(I, 8) + 3 * draftB%(I, 9)) / draftB%(I, 4) + .0005) * 1000): I4 = Int(((draftB%(I, 6) + draftB%(I, 11)) / (draftB%(I, 11) + draftB%(I, 4)) + .0005) * 1000)
                If draftB%(I, 4) = 0 Or I2 = 0 Then Locate 5, 62: Print ".000";: Print " "
                If I2 > 0 And I2 < 100 Then Locate 5, 62: Print ".0";: Print Using "##"; I2;: Print " "
                If I2 >= 100 Then Locate 5, 62: Print ".";: Print Using "###"; I2;: Print " "
                If draftB%(I, 4) = 0 Or I3 = 0 Then Locate 5, 67: Print ".000";: Print " "
                If I3 > 0 And I3 < 100 Then Locate 5, 67: Print ".0";: Print Using "##"; I3;: Print " "
                If I3 >= 100 Then Locate 5, 67: Print ".";: Print Using "###"; I3;: Print " "
                If draftB%(I, 4) = 0 Or I4 = 0 Then Locate 5, 72: Print ".000";: Color 7, 0: Print String$(2, 177)

                Color 1, 11: If I4 > 0 And I4 < 100 Then Locate 5, 72: Print ".0";: Print Using "##"; I4;: Color 7, 0: Print String$(2, 177)
                Color 1, 11: If I4 >= 100 Then Locate 5, 72: Print ".";: Print Using "###"; I4;: Color 7, 0: Print String$(2, 177)
                Color 7, 0: Locate 7, 15: Print String$(63, 177)
                Color 1, 11: Locate 16, 13: Print "  .";: Print Using "###"; draftB%(I, 26);: Print "  ";: Color 7, 0: Print String$(2, 177)
                Color 12: Locate 11, 2: Print "REGULAR "
                Locate 12, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
                Locate 13, 4: Print String$(8, 177)
                Color 12: Locate 16, 2: Print "REGULAR "
                Locate 17, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
                Locate 18, 4: Print String$(8, 177)
                Color 14, 11: Locate 6, 13: Print Using "###"; compareB0%(I, 14);

                If compareB0%(I, 0) = 0 Or compareB0%(I, 2) = 0 Then Locate 12, 13: Print "---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 2) > 0 Then Locate 12, 13: Print Using "###.##"; compareB0%(I, 0) / compareB0%(I, 2)

                Locate 6, 16: Print Using "#####"; compareB0%(I, 0)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 1) = 0 Then Locate 12, 19: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 1) > 0 Then Locate 12, 19: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 1)

                Locate 6, 21: Print Using "####"; compareB0%(I, 2)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 4) = 0 Then Locate 12, 26: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 4) > 0 Then Locate 12, 26: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 4)

                Locate 6, 25: Print Using "####"; compareB0%(I, 1)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 5) = 0 Then Locate 12, 33: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 5) > 0 Then Locate 12, 33: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 5)

                Locate 6, 29: Print Using "####"; compareB0%(I, 4)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 6) = 0 Then Locate 12, 40: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 6) > 0 Then Locate 12, 40: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 6)

                Locate 6, 33: Print Using "####"; compareB0%(I, 5)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 3) = 0 Then Locate 12, 47: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 3) > 0 Then Locate 12, 47: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 3)

                Locate 6, 37: Print Using "####"; compareB0%(I, 6)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 7) = 0 Then Locate 12, 54: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 7) > 0 Then Locate 12, 54: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 7)

                Locate 6, 41: Print Using "#####"; compareB0%(I, 3)

                If compareB0%(I, 0) = 0 Or compareB0%(I, 8) = 0 Then Locate 12, 61: Print " ---.--"
                If compareB0%(I, 0) > 0 And compareB0%(I, 8) > 0 Then Locate 12, 61: Print Using "####.##"; compareB0%(I, 0) / compareB0%(I, 8)

                Locate 6, 46: Print Using "####"; compareB0%(I, 7)
                Locate 6, 50: Print Using "####"; compareB0%(I, 8)

                If compareB0%(I, 9) + compareB0%(I, 10) = 0 Or compareB0%(I, 9) = 0 Then Locate 12, 68: Print "    00% ";: Color 7, 0: Print String$(2, 177)

                Color 14, 11: If compareB0%(I, 9) > 0 And compareB0%(I, 10) = 0 Then Locate 12, 68: Print "   100% ";: Color 7, 0: Print String$(2, 177)
                Color 14, 11: If compareB0%(I, 9) > 0 And compareB0%(I, 10) > 0 Then Locate 12, 68: Print "   ";: Print Using "###"; (compareB0%(I, 9) / (compareB0%(I, 10) + compareB0%(I, 9))) * 100;: Print "% ";: Color 7, 0: Print String$(2, 177)
                Color 14, 11: Locate 6, 54: Print Using "####"; compareB0%(I, 9)
                Locate 6, 58
                Print Using "###"; compareB0%(I, 10);: Print " "

                I2 = 0
                I3 = 0
                I4 = 0

                If compareB0%(I, 0) > 0 Then I2 = Int((compareB0%(I, 1) / compareB0%(I, 0) + .0005) * 1000): I3 = Int(((compareB0%(I, 1) + compareB0%(I, 4) + 2 * compareB0%(I, 5) + 3 * compareB0%(I, 6)) / compareB0%(I, 0) + .0005) * 1000): I4 = Int(((compareB0%(I, 1) + compareB0%(I, 7)) / (compareB0%(I, 7) + compareB0%(I, 0)) + .0005) * 1000)
                If compareB0%(I, 0) = 0 Or I2 = 0 Then Locate 6, 62: Print ".000";: Print " "
                If I2 > 0 And I2 < 100 Then Locate 6, 62: Print ".0";: Print Using "##"; I2;: Print " "
                If I2 >= 100 Then Locate 6, 62: Print ".";: Print Using "###"; I2;: Print " "
                If compareB0%(I, 0) = 0 Or I3 = 0 Then Locate 6, 67: Print ".000";: Print " "
                If I3 > 0 And I3 < 100 Then Locate 6, 67: Print ".0";: Print Using "##"; I3;: Print " "
                If I3 >= 100 Then Locate 6, 67: Print ".";: Print Using "###"; I3;: Print " "
                If compareB0%(I, 0) = 0 Or I4 = 0 Then Locate 6, 72: Print ".000";: Color 7, 0: Print String$(2, 177)

                Color 14, 11: If I4 > 0 And I4 < 100 Then Locate 6, 72: Print ".0";: Print Using "##"; I4;: Color 7, 0: Print String$(2, 177)
                Color 14, 11: If I4 >= 100 Then Locate 6, 72: Print ".";: Print Using "###"; I4;: Color 7, 0: Print String$(2, 177)
                Color 7, 0: Locate 13, 15: Print String$(63, 177)

                If compareB0%(I, 16) = 0 And compareB0%(I, 17) = 0 Then I5 = 0 Else I5 = ((compareB0%(I, 16) + compareB0%(I, 17)) / (compareB0%(I, 16) + compareB0%(I, 17) + compareB0%(I, 11))) * 1000

                Color 14, 11: If I5 = 0 Then Locate 17, 13: Print "  .000  ";: Color 7, 0: Print String$(2, 177)
                Color 14, 11: If I5 > 0 And I5 < 1000 Then Locate 17, 13: Print "  .";: Print Using "###"; I5;: Print "  ";: Color 7, 0: Print String$(2, 177)
                Color 14, 11: If I5 = 1000 Then Locate 17, 13: Print Using "##"; I5 / 1000;: Print ".000  ";: Color 7, 0: Print String$(2, 177)
                Color 7, 0: Locate 18, 15: Print String$(8, 177)
                Color 15, 0: Locate 23, 4: Print "HIT ANY KEY TO CONTINUE"

                If BI = 1 Then
                    Locate 23, 28: Print "OR ESC TO RETURN TO MENU"
                End If

                I$ = GetKeyPress$

                'This below is admittedly horrible
                'It's basically several conditions for kicking us out of the loop
                'However we've simply reached a point where we're presenting
                'an option to continue paging through screens
                'BI = 1 helps us know we're at the last screen
                'But BI = 0 will also exit the loop; it just won't do anything outside
                If I$ <> Chr$(27) Then
                    If BI = 0 Then Cls: Exit For 'Existing conditionals will skip later work so it loops around
                    If BI = 1 Then Exit For 'More will be done later
                    If I < 17 Then
                        Exit For
                    Else
                        BI = 1
                        Exit For
                    End If
                Else
                    BI = 0
                    Exit For 'Get back to loop beginning
                End If

                If BI <> 1 Then
                    BI = 1
                    Exit For
                End If

            End If

        Next

        If BI = 1 Then

            Cls

            For dc = 1 To 14
                Color 15, 0: Locate dc, 1
                Print String$(78, 177)
            Next

            For I = 0 To 21
                If statP$(I) <> "XXX" And statP$(I) <> "END" And statP$(I) <> "" Then

                    Color 11: Locate 2, 2: Print teamName$
                    Color 15, 0: Locate 2, 30: Print String$(14, 177)
                    Color 13: Locate 2, 30: Print Left$(statP$(I), 14)
                    Color 10: Locate 5, 2: Print "REGULAR "
                    Locate 6, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
                    Locate 7, 4: Print String$(8, 177)
                    Color 0, 12: Locate 4, 14: Print "  G ";:

                    Print Tab(20); " W ";:
                    Print Tab(25); " L ";:
                    Print Tab(30); " SV ";:
                    Print Tab(37); " IP ";:
                    Print Tab(43); " H  ";:
                    Print Tab(48); " BB ";:
                    Print Tab(53); " SO ";:
                    Print Tab(58); " ERA ";:
                    Print Tab(63); " GS";:
                    Print Tab(66); " CG"

                    Color 10, 0: Locate 10, 2: Print "REGULAR "
                    Locate 11, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
                    Locate 12, 4: Print String$(8, 177)
                    Color 0, 12: Locate 9, 14: Print " H/9IP ";:
                    Print Tab(21); " SO/9IP ";:
                    Print Tab(29); " BB/9IP ";:
                    Print Tab(37); " HR ALL";:
                    Color 14, 12: Locate 5, 14: Print Using "###"; draftP%(I, 4);:

                    Locate , 17: Print Using "#####"; draftP%(I, 1);:
                    Locate , 22: Print Using "#####"; draftP%(I, 2);:
                    Locate , 27: Print Using "######"; draftP%(I, 3);:
                    Locate , 33: Print Using "#####.#"; draftP%(I, 6);:
                    Locate , 40: Print Using "#####"; draftP%(I, 7);:
                    Locate , 45: Print Using "######"; draftP%(I, 8);:
                    Locate , 51: Print Using "#####"; draftP%(I, 9);:
                    Locate , 56: Print Using "###.##"; (draftP%(I, 10)) / 100;:
                    Locate , 62: Print Using "####"; draftP%(I, 5);:
                    Locate , 66: Print Using "###"; draftP%(I, 16);: Color 7, 0: Print String$(2, 177)
                    Color 14, 12: If draftP%(I, 6) = 0 Then Locate 10, 14: Print "  0.00";

                    If draftP%(I, 6) > 0 Then Locate 10, 14: Print Using "###.##"; (draftP%(I, 7) * 9) / draftP%(I, 6)
                    If draftP%(I, 6) = 0 Then Locate 10, 20: Print "    0.00";
                    If draftP%(I, 6) > 0 Then Locate 10, 20: Print Using "#####.##"; (draftP%(I, 9) * 9) / draftP%(I, 6)
                    If draftP%(I, 6) = 0 Then Locate 10, 28: Print "    0.00";
                    If draftP%(I, 6) > 0 Then Locate 10, 28: Print Using "#####.##"; (draftP%(I, 8) * 9) / draftP%(I, 6)

                    Locate 10, 36: Print Using "########"; draftP%(I, 13);: Color 7, 0: Print String$(2, 177)
                    Color 15, 12: Locate 6, 14: Print Using "###"; compareP0%(I, 8);:
                    Locate , 17: Print Using "#####"; compareP0%(I, 6);:
                    Locate , 22: Print Using "#####"; compareP0%(I, 7);:
                    Locate , 27: Print Using "######"; compareP0%(I, 12);:
                    Locate , 33: Print Using "#####"; Int(compareP0%(I, 0) / 3);

                    If compareP0%(I, 0) - (Int(compareP0%(I, 0) / 3) * 3) = 0 Then Print ".0";
                    If compareP0%(I, 0) - (Int(compareP0%(I, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; compareP0%(I, 0) - (Int(compareP0%(I, 0) / 3) * 3);

                    Locate , 40: Print Using "#####"; compareP0%(I, 1);:
                    Locate , 45: Print Using "######"; compareP0%(I, 4);:
                    Locate , 51: Print Using "#####"; compareP0%(I, 5);:

                    I2 = 0
                    If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 3) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
                    I3 = Int(I2 / 100)
                    I4 = I2 - (I3 * 100)

                    If compareP0%(I, 0) = 0 Or I2 = 0 Then Locate 6, 56: Print "  0.00";
                    If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 6, 56: Print Using "###"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I2 >= 1000 Then Locate 6, 56: Print Using "###"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                    If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                    If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                    Locate 6, 62: Print Using "####"; compareP0%(I, 9);
                    Locate , 66: Print Using "###"; compareP0%(I, 10);: Color 7, 0: Print String$(2, 177)
                    Locate 7, 16: Print String$(55, 177)
                    Color 15, 12: I2 = 0: If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 1) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)

                    I3 = Int(I2 / 100)
                    I4 = I2 - (I3 * 100)

                    If compareP0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 14: Print "  0.00";
                    If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 14: Print Using "###"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 14: Print Using "###"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                    If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                    If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                    I2 = 0
                    If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 5) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
                    I3 = Int(I2 / 100)
                    I4 = I2 - (I3 * 100)

                    If compareP0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 20: Print "    0.00";
                    If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 20: Print Using "#####"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 20: Print Using "#####"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                    If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                    If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                    I2 = 0
                    If compareP0%(I, 0) > 0 Then I2 = Int((((compareP0%(I, 4) * 9) / (compareP0%(I, 0) / 3)) + .0005) * 100)
                    I3 = Int(I2 / 100)
                    I4 = I2 - (I3 * 100)

                    If compareP0%(I, 0) = 0 Or I2 = 0 Then Locate 11, 28: Print "    0.00";
                    If compareP0%(I, 0) > 0 And I2 > 0 And I2 <= 999 Then Locate 11, 28: Print Using "#####"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I2 >= 1000 Then Locate 11, 28: Print Using "#####"; I3;: Print ".";
                    If compareP0%(I, 0) > 0 And I4 = 0 And I2 > 0 Then Print "00";
                    If compareP0%(I, 0) > 0 And I4 > 0 And I4 < 10 And I2 > 0 Then Print "0";: Print Using "#"; I4;
                    If compareP0%(I, 0) > 0 And I4 >= 10 And I2 > 0 Then Print Using "##"; I4;

                    Locate 11, 36: Print Using "########"; compareP0%(I, 14);: Color 7, 0: Print String$(2, 177)
                    Locate 12, 16: Print String$(30, 177)

                    Color 15, 0
                    Locate 23, 4: Print "HIT ANY KEY TO CONTINUE"

                    If BI = 1 Then
                        Locate 23, 28: Print "OR ESC TO RETURN TO MENU"
                    End If

                    I$ = GetKeyPress$

                    If I$ <> Chr$(0) Then
                        If I$ = Chr$(27) Then Exit For
                        If BI <> 1 Then Exit For
                    End If

                End If 'Done checking for invalid chars

            Next

        End If

        If BI = 1 Then
            BI = 0
        End If

    Loop Until I$ = Chr$(27)

End Sub

'----------------------------------------
'     SelectTeam_COMPARE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_COMPARE (yearNumber$, numberTeams)

    Shared K9&()
    Shared statB$(), statP$()
    Shared draftB%(), ratings%(), draftP%()

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

        If Not _FileExists(diskPaths$(0) + "FCTEAMS." + teamYear$) Then
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            Open diskPaths$(0) + "FCTEAMS." + teamYear$ For Random As #1 Len = 4342

            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

            For I = 0 To 22
                Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
                Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
                Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
                Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
            Next

            For I = 0 To 21
                Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015), 2020 - 37 - I * 89 As X$
                Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027), 2020 - 61 - I * 89 As X$
                Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039), 2020 - 85 - I * 89 As X$
                Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
            Next

            Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

            Get #1, T

            For I = 1 To 13:
                ratings%(I) = CVI(Q$(I))
            Next

            K9&(0) = CVS(Q$(14))

            For I = 0 To 22:
                statB$(I) = RTrim$(Q$(I * 43 + 15))
                For I1 = 0 To 41:
                    draftB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
                Next
            Next

            For I = 0 To 21: statP$(I) = RTrim$(Q$(I * 38 + 1004))
                For I1 = 0 To 36:
                    draftP%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
                Next
            Next

            S$ = RTrim$(Q$(1840))
            M$ = RTrim$(Q$(1841))
            NN$ = RTrim$(Q$(1842))
            CK = CVI(Q$(1843))

            Close 1

        End If
    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'       PlayerStats_Screen Subroutine
'----------------------------------------
'...explanation...
Sub PlayerStats_Screen (teamName$)

    Shared ratings%(), TB%()
    Shared statT%(), statT1%()
    Shared TP&()

    Shared ERX!

    Cls

    For dc = 1 To 23
        Color 15, 0: Locate dc, 1
        Print String$(79, 177)
    Next

    Locate 1, 35
    Color ratings%(11), ratings%(12): Print teamName$
    Color 10, 0
    Locate 3, 2: Print "BATTING "
    Color 12
    Locate 4, 2: Print "REGULAR ";
    Color 7, 0: Print String$(2, 177)
    Color 12
    Locate 5, 2: Print "LEAGUE  ";
    Color 7, 0: Print String$(2, 177)
    Locate 6, 4: Print String$(8, 177)
    Color 0, 11
    Locate 3, 13: Print "  AB";:

    Print Tab(18); "  R";:
    Print Tab(23); "  H";:
    Print Tab(27); " 2B";:
    Print Tab(31); " 3B";:
    Print Tab(35); " HR";:
    Print Tab(39); " RBI";:
    Print Tab(43); " BB";:
    Print Tab(49); " SO";:
    Print Tab(53); " SB";:
    Print Tab(57); " CS";:
    Print Tab(61); " AVG";:
    Print Tab(66); " SLG";:
    Print Tab(72); "OBP";:

    Color 10, 0
    Locate 9, 2: Print "BATTING "
    Color 12, 0
    Locate 10, 2: Print "REGULAR ";
    Color 7, 0: Print String$(2, 177)
    Color 12
    Locate 11, 2: Print "LEAGUE  ";
    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)
    Color 0, 11
    Locate 8, 13: Print "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
    Locate 9, 13: Print "  RUNS";:

    Print Tab(21); " HITS";:
    Print Tab(30); " 2B ";:
    Print Tab(37); " 3B ";:
    Print Tab(44); " HR ";:
    Print Tab(50); " RBI ";:
    Print Tab(58); " BB ";:
    Print Tab(65); " SO ";:
    Print Tab(70); " %-SB ";
    Color 7, 0: Print String$(2, 177)
    Color 1, 11: Locate 4, 13: Print Using "####"; TB%(1);

    Locate , 17: Print Using "####"; TB%(2);
    Locate , 21: Print Using "#####"; TB%(3);
    Locate , 26: Print Using "####"; TB%(4);
    Locate , 30: Print Using "####"; TB%(5);
    Locate , 34: Print Using "####"; TB%(6);
    Locate , 38: Print Using "#####"; TB%(7);
    Locate , 43: Print Using "####"; TB%(8);
    Locate , 47: Print Using "#####"; TB%(9);
    Locate , 52: Print Using "####"; TB%(10);
    Locate , 56: Print Using "####"; TB%(11);: Print " ";

    statPCT! = TB%(3) / TB%(1)
    Locate , 61: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (TB%(3) + TB%(4) + TB%(5) * 2 + TB%(6) * 3) / TB%(1)
    Locate , 66: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (TB%(3) + TB%(8)) / (TB%(1) + TB%(8))
    Locate , 71: Print Using ".###"; statPCT!;

    Color 7, 0
	Print String$(2, 177)

    Color 14, 11
	Locate 5, 13: Print Using "####"; statT%(0);
    Locate , 17: Print Using "####"; statT%(2);
    Locate , 21: Print Using "#####"; statT%(1);
    Locate , 26: Print Using "####"; statT%(4);
    Locate , 30: Print Using "####"; statT%(5);
    Locate , 34: Print Using "####"; statT%(6);
    Locate , 38: Print Using "#####"; statT%(3);
    Locate , 43: Print Using "####"; statT%(7);
    Locate , 47: Print Using "#####"; statT%(8);
    Locate , 52: Print Using "####"; statT%(9);
    Locate , 56: Print Using "####"; statT%(10);
    Print " ";

    statPCT! = statT%(1) / statT%(0)
    Locate , 61: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (statT%(1) + statT%(4) + statT%(5) * 2 + statT%(6) * 3) / statT%(0)
    Locate , 66: Print Using ".###"; statPCT!;: Print " ";

    statPCT! = (statT%(1) + statT%(7)) / (statT%(0) + statT%(7))
    Locate , 71: Print Using ".###"; statPCT!;

    Color 7, 0
	Print String$(2, 177)

    Locate , 15: Print String$(62, 177)

    Color 1, 11
    If TB%(2) = 0 Then Locate 10, 13: Print " ---.--";
    If TB%(2) > 0 Then Locate 10, 13: Print Using "###.##"; TB%(1) / TB%(2);
    If TB%(3) = 0 Then Locate , 18: Print " ---.--";
    If TB%(3) > 0 Then Locate , 18: Print Using "#####.##"; TB%(1) / TB%(3);
    If TB%(4) = 0 Then Locate , 26: Print " ---.--";
    If TB%(4) > 0 Then Locate , 26: Print Using "####.##"; TB%(1) / TB%(4);
    If TB%(5) = 0 Then Locate , 33: Print " ---.--";
    If TB%(5) > 0 Then Locate , 33: Print Using "####.##"; TB%(1) / TB%(5);
    If TB%(6) = 0 Then Locate , 40: Print " ---.--";
    If TB%(6) > 0 Then Locate , 40: Print Using "####.##"; TB%(1) / TB%(6);
    If TB%(7) = 0 Then Locate , 47: Print " ---.--";
    If TB%(7) > 0 Then Locate , 47: Print Using "####.##"; TB%(1) / TB%(7);
    If TB%(8) = 0 Then Locate , 54: Print " ---.--";
    If TB%(8) > 0 Then Locate , 54: Print Using "####.##"; TB%(1) / TB%(8);
    If TB%(9) = 0 Then Locate , 61: Print " ---.--";
    If TB%(9) > 0 Then Locate , 61: Print Using "####.##"; TB%(1) / TB%(9);
    If TB%(10) = 0 Then Locate , 68: Print " ---.--";
    If TB%(10) > 0 Then Locate , 68: Print Using "####.##"; TB%(10) / (TB%(10) + TB%(11));
    Print " ";

    Color 7, 0
	Print String$(2, 177)

    Color 14, 11

    If statT%(2) = 0 Then Locate 11, 13: Print " ---.--";
    If statT%(2) > 0 Then Locate 11, 13: Print Using "###.##"; statT%(0) / statT%(2);
    If statT%(1) = 0 Then Locate , 18: Print " ---.--";
    If statT%(1) > 0 Then Locate , 18: Print Using "#####.##"; statT%(0) / statT%(1);
    If statT%(4) = 0 Then Locate , 26: Print " ---.--";
    If statT%(4) > 0 Then Locate , 26: Print Using "####.##"; statT%(0) / statT%(4);
    If statT%(5) = 0 Then Locate , 33: Print " ---.--";
    If statT%(5) > 0 Then Locate , 33: Print Using "####.##"; statT%(0) / statT%(5);
    If statT%(6) = 0 Then Locate , 40: Print " ---.--";
    If statT%(6) > 0 Then Locate , 40: Print Using "####.##"; statT%(0) / statT%(6);
    If statT%(3) = 0 Then Locate , 47: Print " ---.--";
    If statT%(3) > 0 Then Locate , 47: Print Using "####.##"; statT%(0) / statT%(3);
    If statT%(7) = 0 Then Locate , 54: Print " ---.--";
    If statT%(7) > 0 Then Locate , 54: Print Using "####.##"; statT%(0) / statT%(7);
    If statT%(8) = 0 Then Locate , 61: Print " ---.--";
    If statT%(8) > 0 Then Locate , 61: Print Using "####.##"; statT%(0) / statT%(8);
    If statT%(9) = 0 Then Locate , 68: Print " ---.--";
    If statT%(9) > 0 Then Locate , 68: Print Using "####.##"; statT%(9) / (statT%(9) + statT%(10));
    Print " ";
    Color 7, 0: Print String$(2, 177)

    Locate , 15: Print String$(63, 177)
    Color 10, 0: Locate 14, 2: Print "PITCHING"
    Color 12, 0: Locate 15, 2: Print "REGULAR ";
    
	Color 7, 0
	Print String$(2, 177)
    
	Locate 16, 2
	
	Color 12
	
	Print "LEAGUE  ";
    
	Color 7, 0
	Print String$(2, 177)
    
	Locate 17, 4: Print String$(8, 177)
    
	Color 0, 12
	Locate 14, 14: Print "  SV";

    Print Tab(20); " IP";
    Print Tab(25); "  H";
    Print Tab(30); "  BB";
    Print Tab(37); "  SO";
    Print Tab(43); " HRA";
    Print Tab(48); "  CG";
    Print Tab(53); "  ERA";

    Color 10, 0: Locate 19, 2: Print "PITCHING"
    Color 12, 0: Locate 20, 2: Print "REGULAR ";
    
	Color 7, 0
	Print String$(2, 177)

    Locate 21, 2: Color 12: Print "LEAGUE  ";
    
	Color 7, 0
	Print String$(2, 177)

    Locate 22, 4: Print String$(8, 177)

    Color 0, 12
    
	Locate 19, 14: Print " H/9IP ";
    Print Tab(21); " SO/9IP ";
    Print Tab(29); " BB/9IP";
    
	Color 15, 12
    Locate 15, 14: Print Using "####"; TP&(3);

    Locate , 18: Print Using "#####"; TP&(6);
    Locate , 23: Print Using "#####"; TP&(7);
    Locate , 28: Print Using "######"; TP&(8);
    Locate , 34: Print Using "#######"; TP&(9);
    Locate , 41: Print Using "######"; TP&(13);
    Locate , 47: Print Using "#####"; TP&(16);
    Locate , 52: Print Using "###.##"; (ERX! * 9) / TP&(6);
    
	Color 7, 0
	Print String$(2, 177)

    Color 15, 12: Locate 20, 14: Print Using "###.##"; TP&(7) * 9 / TP&(6);

    Locate , 20: Print Using "#####.##"; TP&(9) * 9 / TP&(6);
    Locate , 28: Print Using "#####.##"; TP&(8) * 9 / TP&(6);
    
	Color 7, 0
	Print String$(2, 177)

    Color 14, 12
	Locate 16, 14: Print Using "####"; statT1%(12);

    Locate , 18: Print Using "#####"; statT1%(0) / 3;
    Locate , 23: Print Using "#####"; statT1%(1);
    Locate , 28: Print Using "######"; statT1%(4);
    Locate , 34: Print Using "#######"; statT1%(5);
    Locate , 41: Print Using "######"; statT1%(14);
    Locate , 47: Print Using "#####"; statT1%(10);
    Locate , 52: Print Using "###.##"; (statT1%(3) * 9) / (statT1%(0) / 3);
    
	Color 7, 0
	Print String$(2, 177)
    Locate , 16
    
	Color 7, 0
	Print String$(44, 177)

    Locate 21, 14
    Color 14, 12: Print Using "###.##"; statT1%(1) * 9 / (statT1%(0) / 3);

    Locate , 20: Print Using "#####.##"; statT1%(5) * 9 / (statT1%(0) / 3);
    Locate , 28: Print Using "#####.##"; statT1%(4) * 9 / (statT1%(0) / 3);
    
	Color 7, 0
	Print String$(2, 177)

    Locate , 16
    
	Color 7, 0
	Print String$(22, 177)

    Color 15, 0
    Locate 22, 48: Print "HIT ANY KEY TO CONTINUE"

    For I = 1 To 15: TB%(I) = 0: Next
    For I = 1 To 33: TP&(I) = 0: Next
    I$ = GetKeyPress$

End Sub

'8000
'   Locate 13, 20: Color 14: Print "ERROR::: INVALID TEAM # OR STAT FILE NAME!!"
'   Locate 15, 20: Line Input "HIT ENTER TO RE-ENTER"; DUMMY$
'   For J = 5 To 15
'       Locate J
'       Print Space$(73)
'   Next
