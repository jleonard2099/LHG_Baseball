Sub CAREDIT ()

    _Title "Full Count Baseball - Edit Career Files"

    DefInt A-Z

    Shared careerB$(), careerP$()

    Shared BRS%(), PRS%()

    Cls
    TMR = 2700

    Do
    'Display list of files...

        Spec$ = diskPaths$(1) + "*.CAR"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then 
            Locate 10, 8
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim careerFiles$(0 To Count%)
            For X = 1 To Count%
                careerFiles$(X) = Space$(12)
            Next
            careerFiles$(0) = Spec$
            Call ReadFile(careerFiles$())

            Do

                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT FILE TO EDIT": Color 15
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
                Print "ENTER";
				Color 15
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

					Color 15, 1
                    targetFile$ = careerFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

					Do
						Locate , 33: Print "ANY CHANGE (YN)";
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

					Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                Cls
                Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

                Z$ = RTrim$(targetFile$)

                Open diskPaths$(1) + Z$ For Input As #2
                Input #2, NB, NP
                For I = 0 To 99: Input #2, careerB$(I): Next
                For I = 0 To 79: Input #2, careerP$(I): Next
                For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Input #2, BRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J
                For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Input #2, PRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

                Close #2

                Do
                    Cls
                    Color 7: Print "EDIT ";
                    Color 12: Print "B";
                    Color 7: Print "ATTERS, ";
                    Color 12: Print "P";
                    Color 7: Print "ITCHERS, ";
                    Color 12: Print "U";
					Color 7: Print "PDATE FILE, OR ";
					Color 12: Print "E";: Color 7: Print "XIT WITHOUT UPDATING"
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)
                        Case "B": Call EditBatters (Z$)
                        Case "E", "X": '
                        Case "P": Call EditPitchers (Z$)
                        Case "U":
                            Cls
                            Color 15: Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

                            TMR = 2700

                            Open diskPaths$(1) + Z$ For Output As #3

                            Write #3, NB, NP

                            For I = 0 To 99: Write #3, careerB$(I): Next
                            For I = 0 To 79: Write #3, careerP$(I): Next
                            For J = 1 To 15: For I = 0 To 99: For I1 = O To 17: Write #3, BRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J
                            For J = 1 To 15: For I = 0 To 79: For I1 = 0 To 16: Write #3, PRS%(J, I, I1): Next I1: Locate 12, 55: Color 3: TMR = TMR - 1: Print Using "####"; TMR: Next I: Next J

                            Close #3

                    End Select

                Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X"

            End If

        End If 'Done checking file count

    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'       EditBatters Subroutine
'----------------------------------------
'...explanation...
Sub EditBatters (careerFile$)

    Shared careerB$()

    Shared BRS%()

    Do

        Cls
        Color 12: Print careerFile$

        For I = 0 To 19
            Color 11: Locate I + 2, 1: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 20 To 39
            Color 11: Locate I - 18, 16: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 40 To 59
            Color 11: Locate I - 38, 32: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 60 To 79
            Color 11: Locate I - 58, 48: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 80 To 99
            Color 11: Locate I - 78, 64: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerB$(I), 12)
        Next

        Print

        Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

        If B9 <> 100 Then

            If B9 < 99 Then 
                Cls
                Locate 5, 20
                Color 15: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerB$(B9);: Color 15: Print " BEFORE "
                Locate , 20: Print "DELETING THEM? (Y/N)"

                Color 14
                Print
                Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
                Locate , 20: Print "OF ";
                Color 12: Print careerB$(B9);
                Color 14: Print " HIT 'E'"

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)
                        Case "Y": Call PrintBatterStats (careerFile$, B9)
                        Case "N": Cls
                        Case "E": Exit Sub
                    End Select

                Loop Until UCase$(I$) = "N"

                Cls
                Locate 5, 15: Print "CHANGING BATTER'S FILES . . . . . PLEASE WAIT"

                For I = B9 To 98: careerB$(I) = careerB$(I + 1): Next

                UPP = (99 - B9) * 15

                I = B9

                For J = 1 To 15
                    For I = B9 To 98
                        For I1 = O To 17
                            BRS%(J, I, I1) = BRS%(J, I + 1, I1)
                        Next
                        Locate 7, 36
                        Color 12: UPP = UPP - 1
                        Print Using "####"; UPP
                    Next
                Next

            End If

            If B9 < 100 Then 'we have to ignore if they entered over 100
                careerB$(99) = ""
                For J = 1 To 15
                    For I1 = 0 To 17
                        BRS%(J, 99, I1) = 0
                    Next
                Next
            End If

        End If

    Loop Until B9 = 100

End Sub

'----------------------------------------
'       EditPitchers Subroutine
'----------------------------------------
'...explanation...
Sub EditPitchers (careerFile$)

    Shared careerP$()

    Shared PRS%()

    Do
        Cls
        Color 12: Print careerFile$

        For I = 0 To 19
        Color 11: Locate I + 2, 1: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerP$(I), 15)
        Next

        For I = 20 To 39
        Color 11: Locate I - 18, 21: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerP$(I), 15)
        Next

        For I = 40 To 59
        Color 11: Locate I - 38, 41: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerP$(I), 15)
        Next

        For I = 60 To 79
        Color 11: Locate I - 58, 61: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerP$(I), 15)
        Next

        Print

        Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

        If B9 <> 100 Then

            If B9 < 79 Then
                Cls

                Locate 5, 20
                Color 15: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerP$(B9);: Color 15: Print " BEFORE "
                Locate , 20: Print "DELETING THEM? (Y/N)"

                Color 14
                Print
                Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
                Locate , 20: Print "OF ";
                Color 12: Print careerP$(B9);
                Color 14: Print " HIT 'E'"

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)
                        Case "Y": Call PrintPitcherStats (careerFile$, B9)
                        Case "N": Cls
                        Case "E": Exit Sub
                    End Select

                Loop Until UCase$(I$) = "N"

                Cls
                Locate 5, 15: Print "CHANGING PITCHER'S FILES . . . . . PLEASE WAIT"

                For I = B9 To 78: careerP$(I) = careerP$(I + 1): Next

                UPP = (79 - B9) * 15

                I = B9

                For J = 1 To 15
                    For I = B9 To 78
                        For I1 = O To 16
                            PRS%(J, I, I1) = PRS%(J, I + 1, I1)
                        Next
                        Locate 7, 36
                        Color 12
                        UPP = UPP - 1
                        Print Using "####"; UPP
                    Next
                Next

            End If 'Done checking for 79

            If B9 < 80 Then
                careerP$(79) = ""
                For J = 1 To 15
                    For I1 = 0 To 16
                        PRS%(J, 79, I1) = 0
                    Next
                Next
            End If

        End If 'Done checking for 100

    Loop Until B9 = 100

End Sub

'----------------------------------------
'      PrintBatterStats Subroutine
'----------------------------------------
'...explanation...
Sub PrintBatterStats (careerFile$, playerIdx)

    Dim I2!, I3!, I4!

    Shared careerB$()
    Shared BRS%()

    LPrint Chr$(15)

    '6103 WIDTH "LPT1:1", 130

    B9 = playerIdx

    LPrint careerFile$

    LPrint "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

    VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) + BRS%(14, B9, 16) + BRS%(15, B9, 16)

    LPrint Left$(careerB$(B9), 10);

    For V = 1 To VV

        LPrint Tab(11); BRS%(V, B9, 17);
        LPrint Tab(20); Using "###"; BRS%(V, B9, 14);
        LPrint Tab(28); Using "###"; BRS%(V, B9, 0);
        LPrint Tab(35); Using "###"; BRS%(V, B9, 2);
        LPrint Tab(42); Using "###"; BRS%(V, B9, 1);
        LPrint Tab(49); Using "##"; BRS%(V, B9, 4);
        LPrint Tab(55); Using "##"; BRS%(V, B9, 5);
        LPrint Tab(61); Using "##"; BRS%(V, B9, 6);
        LPrint Tab(67); Using "###"; BRS%(V, B9, 3);
        LPrint Tab(74); Using "###"; BRS%(V, B9, 7);
        LPrint Tab(81); Using "###"; BRS%(V, B9, 8);
        LPrint Tab(88); Using "###"; BRS%(V, B9, 9);
        LPrint Tab(95); Using "##"; BRS%(V, B9, 10);
        LPrint Tab(101); Using "##"; BRS%(V, B9, 11);

        I2! = 0
        I3! = 0
        I4! = 0

        IF BRS%(V, B9, 0) > 0 THEN I2! = INT((BRS%(V, B9, 1) / BRS%(V, B9, 0) + .0005) * 1000): I3! = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 4) + 2 * BRS%(V, B9, 5) + 3 * BRS%(V, B9, 6)) / BRS%(V, B9, 0) + .0005) * 1000): I4! = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 7)) / (BRS%(V, B9, 7) + BRS%(V, B9, 0)) + .0005) * 1000)
        If BRS%(V, B9, 0) = 0 Or I2! = 0 Then LPrint Tab(107); ".000";
        If I2! > 0 And I2! < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2!;
        If I2! >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2!;
        If BRS%(V, B9, 0) = 0 Or I3! = 0 Then LPrint Tab(115); ".000";
        If I3! > 0 And I3! < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3!;
        If I3! >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3!;
        If BRS%(V, B9, 0) = 0 Or I4! = 0 Then LPrint Tab(123); ".000"
        If I4! > 0 And I4! < 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4!
        If I4! >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4!

    Next V

    For V = 0 To VV

        TT1 = TT1 + BRS%(V, B9, 14)
        TT3 = TT3 + BRS%(V, B9, 2)
        TT5 = TT5 + BRS%(V, B9, 4)
        TT7 = TT7 + BRS%(V, B9, 6)
        TT9 = TT9 + BRS%(V, B9, 7)
        TT11 = TT11 + BRS%(V, B9, 9)
        TT13 = TT13 + BRS%(V, B9, 11)
        TT2 = TT2 + BRS%(V, B9, 0)
        TT4 = TT4 + BRS%(V, B9, 1)
        TT6 = TT6 + BRS%(V, B9, 5)
        TT8 = TT8 + BRS%(V, B9, 3)
        TT10 = TT10 + BRS%(V, B9, 8)
        TT12 = TT12 + BRS%(V, B9, 10)

    Next V

    LPrint "   Totals";
    LPrint Tab(14); Using "##"; VV;
    LPrint Tab(19); Using "####"; TT1;
    LPrint Tab(27); Using "####"; TT2;
    LPrint Tab(34); Using "####"; TT3;
    LPrint Tab(41); Using "####"; TT4;
    LPrint Tab(48); Using "###"; TT5;
    LPrint Tab(54); Using "###"; TT6;
    LPrint Tab(59); Using "####"; TT7;
    LPrint Tab(66); Using "####"; TT8;
    LPrint Tab(73); Using "####"; TT9;
    LPrint Tab(80); Using "####"; TT10;
    LPrint Tab(87); Using "####"; TT11;
    LPrint Tab(94); Using "###"; TT12;
    LPrint Tab(100); Using "###"; TT13;

    I2! = 0: If TT2 > 0 Then I2! = Int((TT4 / TT2 + .0005) * 1000)

    If TT2 = 0 Or I2! = 0 Then LPrint Tab(107); ".000";
    If I2! > 0 And I2! < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2!;
    If I2! >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2!;

    I3! = 0: I3! = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

    If TT2 = 0 Or I3! = 0 Then LPrint Tab(115); ".000";
    If I3! > 0 And I3! < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3!;
    If I3! >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3!;

    I4! = 0: If TT9 + TT2 > 0 Then I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

    If TT2 = 0 Or I4! = 0 Then LPrint Tab(123); ".000"
    If I4! > 0 And I4! < 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4!
    If I4! >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4!

    '-- Why is this important?
    '-- Do we use this elsewhere?
    TT1 = 0
    TT2 = 0
    TT3 = 0
    TT4 = 0
    TT5 = 0
    TT6 = 0
    TT7 = 0
    TT8 = 0
    TT9 = 0
    TT10 = 0
    TT11 = 0
    TT12 = 0
    TT13 = 0

    LPrint
    LPrint Chr$(18)

    '6293 WIDTH "LPT1:1", 80

End Sub

'----------------------------------------
'      PrintPitcherStats Subroutine
'----------------------------------------
'...explanation...
Sub PrintPitcherStats (careerFile$, playerIdx)

    Dim I2!, I3!, I4!

    Shared careerP$()

    Shared PRS%()

    B9 = playerIdx

    LPrint Chr$(15)

    'WIDTH "LPT1:1", 130

    LPrint careerFile$

    LPrint "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

    VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) + PRS%(14, B9, 15) + PRS%(15, B9, 15)

    LPrint Left$(careerP$(B9), 10);

    For V = 1 To VV

        LPrint Tab(11); PRS%(V, B9, 16);
        LPrint Tab(20); Using "##"; PRS%(V, B9, 6);
        LPrint Tab(26); Using "##"; PRS%(V, B9, 7);
        LPrint Tab(32); Using "##"; PRS%(V, B9, 12);
        LPrint Tab(39);: LPrint Using "###"; Int(PRS%(V, B9, 0) / 3);

        If PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3);

        LPrint Tab(48); Using "###"; PRS%(V, B9, 1);
        LPrint Tab(55); Using "###"; PRS%(V, B9, 4);
        LPrint Tab(62); Using "###"; PRS%(V, B9, 5)

        I2! = 0
        If PRS%(V, B9, 0) > 0 Then I2! = Int((((PRS%(V, B9, 3) * 9) / (PRS%(V, B9, 0) / 3)) + .0005) * 100)
        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

        If PRS%(V, B9, 0) = 0 Or I2! = 0 Then LPrint Tab(68); " 0.00";
        If PRS%(V, B9, 0) > 0 And I2! > 0 And I2! <= 999 Then LPrint Tab(68); Using "##"; I3!;: LPrint ".";
        If PRS%(V, B9, 0) > 0 And I2! >= 1000 Then LPrint Tab(68); Using "##"; I3!;: LPrint ".";
        If PRS%(V, B9, 0) > 0 And I4! = 0 And I2! > 0 Then LPrint "00";
        If PRS%(V, B9, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then LPrint "0";: LPrint Using "#"; I4!;
        If PRS%(V, B9, 0) > 0 And I4! >= 10 And I2! > 0 Then LPrint Using "##"; I4!;

        LPrint Tab(77); Using "##"; PRS%(V, B9, 8);
        LPrint Tab(83); Using "##"; PRS%(V, B9, 9);
        LPrint Tab(89); Using "##"; PRS%(V, B9, 10);
        LPrint Tab(95); Using "###"; PRS%(V, B9, 2);
        LPrint Tab(102); Using "###"; PRS%(V, B9, 3);
        LPrint Tab(110); Using "##"; PRS%(V, B9, 11);
        LPrint Tab(116); Using "##"; PRS%(V, B9, 14);
        LPrint Tab(121); Using "###"; PRS%(V, B9, 13)

    Next V

    TT13 = 0
    TT14 = 0
    TT15 = 0
    TT16 = 0
    TT17 = 0
    TT18 = 0
    TT19 = 0
    TT20 = 0
    TT21 = 0
    TT22 = 0
    TT23 = 0
    TT24 = 0
    TT25 = 0
    TT26 = 0
    TT27 = 0
    TT28 = 0

    VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) + PRS%(14, B9, 15) + PRS%(15, B9, 15)

    For V = 0 To VV

        TT13 = TT13 + PRS%(V, B9, 6)
        TT15 = TT15 + PRS%(V, B9, 12)
        TT17 = TT17 + PRS%(V, B9, 1)
        TT19 = TT19 + PRS%(V, B9, 5)
        TT21 = TT21 + PRS%(V, B9, 8)
        TT23 = TT23 + PRS%(V, B9, 10)
        TT25 = TT25 + PRS%(V, B9, 3)
        TT27 = TT27 + PRS%(V, B9, 14)
        TT14 = TT14 + PRS%(V, B9, 7)
        TT16 = TT16 + PRS%(V, B9, 0)
        TT18 = TT18 + PRS%(V, B9, 4)
        TT20 = TT20
        TT22 = TT22 + PRS%(V, B9, 9)
        TT24 = TT24 + PRS%(V, B9, 2)
        TT26 = TT26 + PRS%(V, B9, 11)
        TT28 = TT28 + PRS%(V, B9, 13)

    Next V

    LPrint "   Totals";
    LPrint Tab(13); Using "###"; VV;
    LPrint Tab(19); Using "###"; TT13;
    LPrint Tab(25); Using "###"; TT14;
    LPrint Tab(31); Using "###"; TT15;
    LPrint Tab(39); Using "###"; Int(TT16 / 3);

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; TT16 - (Int(TT16 / 3) * 3);

    LPrint Tab(47); Using "####"; TT17;
    LPrint Tab(55); Using "###"; TT18;
    LPrint Tab(61); Using "####"; TT19;

    I2! = 0
    If TT16 > 0 Then I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)
    I3! = Int(I2! / 100): I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then LPrint Tab(68); " 0.00";
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then LPrint Tab(68); Using "##"; I3!;: LPrint ".";
    If TT16 > 0 And I2! >= 1000 Then LPrint Tab(68); Using "##"; I3!;: LPrint ".";
    If TT16 > 0 And I4! = 0 And I2! > 0 Then LPrint "00";
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then LPrint "0";: LPrint Using "#"; I4!;
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then LPrint Using "##"; I4!;

    LPrint Tab(76); Using "###"; TT21;
    LPrint Tab(81); Using "####"; TT22;
    LPrint Tab(88); Using "###"; TT23;
    LPrint Tab(95); Using "###"; TT24;
    LPrint Tab(101); Using "####"; TT25;
    LPrint Tab(108); Using "####"; TT26;
    LPrint Tab(114); Using "####"; TT27;
    LPrint Tab(121); Using "###"; TT28
    LPrint
    LPrint Chr$(18)

End Sub
