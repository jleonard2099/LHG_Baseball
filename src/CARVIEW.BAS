Sub CARVIEW ()

    _Title "Full Count Baseball - View Career Files"

    Shared careerB$(), careerP$()
    Shared BRS%(), PRS%()

    TMR = 2700

    Do
        'Display list of files...
        Color 0, 0
        Cls

        Spec$ = diskPaths$(1) + "*.CAR"
        Count% = FCount%(Spec$)

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
                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
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
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = careerFiles$(Selection%)
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
                careerFile$ = Left$(targetFile$, Len(targetFile$) - 4)

                Color 15, 0
                Cls

                Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

                Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #2

                Input #2, NB, NP

                For I = 0 To 99: Input #2, careerB$(I): Next

                For I = 0 To 79: Input #2, careerP$(I): Next

                For J = 1 To 15
                    For I = 0 To 99
                        For I1 = O To 17:
                            Input #2, BRS%(J, I, I1)
                        Next I1
                        Locate 12, 55: Color 3
                        TMR = TMR - 1
                        Print Using "####"; TMR
                    Next I
                Next J

                For J = 1 To 15
                    For I = 0 To 79
                        For I1 = 0 To 16
                            Input #2, PRS%(J, I, I1)
                        Next I1
                        Locate 12, 55
                        Color 3
                        TMR = TMR - 1
                        Print Using "####"; TMR
                    Next I
                Next J

                Close #2

                '----------

                Color 15, 0
                Cls

                Do
                    Locate 15, 10
                    Color 12: Print "V";: Color 7: Print "iew or ";
                    Color 12: Print "P";: Color 7: Print "rint"
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

                Select Case UCase$(I$)

                    Case "P":
                        Call CareerToPrinter(careerFile$)

                    Case "V"
                        Call CareerToScreen(careerFile$)

                End Select

            End If 'Done checking for ESC

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'       BatterTotals Subroutine
'----------------------------------------
'...explanation...
Sub BatterTotals ()

    Dim I2!, I3!, I4!
    Shared BRS%()

    VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16) + BRS%(15, I, 16)

    For V = 0 To VV

        TT1 = TT1 + BRS%(V, I, 14)
        TT3 = TT3 + BRS%(V, I, 2)
        TT5 = TT5 + BRS%(V, I, 4)
        TT7 = TT7 + BRS%(V, I, 6)
        TT9 = TT9 + BRS%(V, I, 7)
        TT11 = TT11 + BRS%(V, I, 9)
        TT13 = TT13 + BRS%(V, I, 11)
        TT2 = TT2 + BRS%(V, I, 0)
        TT4 = TT4 + BRS%(V, I, 1)
        TT6 = TT6 + BRS%(V, I, 5)
        TT8 = TT8 + BRS%(V, I, 3)
        TT10 = TT10 + BRS%(V, I, 8)
        TT12 = TT12 + BRS%(V, I, 10)

    Next V

    Print
    Color 3

    Print Tab(16); Using "####"; TT1;
    Print Tab(25); Using "####"; TT3;
    Print Tab(33); Using "###"; TT5;
    Print Tab(38); Using "####"; TT7;
    Print Tab(46); Using "####"; TT9;
    Print Tab(54); Using "####"; TT11;
    Print Tab(61); Using "###"; TT13;

    I3! = 0
    I3! = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

    If TT2 = 0 Or I3! = O Then Locate , 70: Print ".000"
    If I3! > 0 And I3! < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3!
    If I3! >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3!

    Print "TOTALS";

    Print Tab(14); Using "##"; VV;
    Print Tab(21); Using "####"; TT2;
    Print Tab(29); Using "####"; TT4;
    Print Tab(36); Using "###"; TT6;
    Print Tab(42); Using "####"; TT8;
    Print Tab(50); Using "####"; TT10;
    Print Tab(58); Using "###"; TT12;

    I2 = 0

    If TT2 > 0 Then I2 = Int((TT4 / TT2 + .0005) * 1000)
    If TT2 = 0 Or I2 = 0 Then Locate , 65: Print ".000";
    If I2 > 0 And I2 < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2;
    If I2 >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2;

    I4! = 0

    If TT9 + TT2 > 0 Then I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)
    If TT2 = 0 Or I4! = 0 Then Locate , 75: Print ".000"
    If I4! > 0 And I4! < 100 Then Locate , 75: Print ".0";: Print Using "##"; I4!
    If I4! >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4!

End Sub

'----------------------------------------
'       PitcherTotals Subroutine
'----------------------------------------
'...explanation...
Sub PitcherTotals ()

    Shared PRS%()

    Dim I2!, I3!, I4!

    VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15) + PRS%(15, I, 15)

    For V = 0 To VV

        TT13 = TT13 + PRS%(V, I, 6)
        TT15 = TT15 + PRS%(V, I, 12)
        TT17 = TT17 + PRS%(V, I, 1)
        TT19 = TT19 + PRS%(V, I, 5)
        TT21 = TT21 + PRS%(V, I, 8)
        TT23 = TT23 + PRS%(V, I, 10)
        TT25 = TT25 + PRS%(V, I, 3)
        TT27 = TT27 + PRS%(V, I, 14)
        TT14 = TT14 + PRS%(V, I, 7)
        TT16 = TT16 + PRS%(V, I, 0)
        TT18 = TT18 + PRS%(V, I, 4)
        TT20 = TT20
        TT22 = TT22 + PRS%(V, I, 9)
        TT24 = TT24 + PRS%(V, I, 2)
        TT26 = TT26 + PRS%(V, I, 11)
        TT28 = TT28 + PRS%(V, I, 13)

    Next V

    Print
    Color 3

    Print Tab(16); Using "###"; TT13;
    Print Tab(22); Using "###"; TT15;
    Print Tab(32); Using "####"; TT17;
    Print Tab(40); Using "####"; TT19;
    Print Tab(49); Using "###"; TT21;
    Print Tab(55); Using "###"; TT23;
    Print Tab(62); Using "####"; TT25;
    Print Tab(69); Using "####"; TT27
    Print "TOTALS";
    Print Tab(13); Using "###"; VV;
    Print Tab(19); Using "###"; TT14;
    Print Tab(27); Using "###"; Int(TT16 / 3);

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then Print ".";: Print Using "#"; TT16 - (Int(TT16 / 3) * 3);

    Print Tab(37); Using "###"; TT18;

    I2! = 0
    If TT16 > 0 Then I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

    I3! = Int(I2! / 100)
    I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then Locate , 44: Print " 0.00";
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then Locate , 44: Print Using "##"; I3!;: Print ".";
    If TT16 > 0 And I2! >= 1000 Then Locate , 44: Print Using "##"; I3!;: Print ".";
    If TT16 > 0 And I4! = 0 And I2! > 0 Then Print "00";
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then Print "0";: Print Using "#"; I4!;
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then Print Using "##"; I4!;

    Print Tab(51); Using "####"; TT22;
    Print Tab(59); Using "###"; TT24;
    Print Tab(66); Using "####"; TT26;
    Print Tab(73); Using "###"; TT28

End Sub

'----------------------------------------
'     CareerToPrinter Subroutine
'----------------------------------------
'...explanation...
Sub CareerToPrinter (careerFile$)

    Dim I2!, I3!, I4!
    Shared BRS%(), PRS%()
    Shared careerB$(), careerP$()

    PTN = 0

    LPrint careerFile$
    LPrint "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

    For I = 0 To 99

        If careerB$(I) <> "" And careerB$(I) <> "XXX" Then

            VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16) + BRS%(15, I, 16)

            If 55 - PTN < VV Then
                LPrint Chr$(12)
                LPrint careerFile$
                LPrint "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"
                PTN = 0

            End If

            LPrint Left$(careerB$(I), 10);

            For V = 1 To VV

                PTN = PTN + 1

                LPrint Tab(11); BRS%(V, I, 17);
                LPrint Tab(20); Using "###"; BRS%(V, I, 14);
                LPrint Tab(28); Using "###"; BRS%(V, I, 0);
                LPrint Tab(35); Using "###"; BRS%(V, I, 2);
                LPrint Tab(42); Using "###"; BRS%(V, I, 1);
                LPrint Tab(49); Using "##"; BRS%(V, I, 4);
                LPrint Tab(55); Using "##"; BRS%(V, I, 5);
                LPrint Tab(61); Using "##"; BRS%(V, I, 6);
                LPrint Tab(67); Using "###"; BRS%(V, I, 3);
                LPrint Tab(74); Using "###"; BRS%(V, I, 7);
                LPrint Tab(81); Using "###"; BRS%(V, I, 8);
                LPrint Tab(88); Using "###"; BRS%(V, I, 9);
                LPrint Tab(95); Using "##"; BRS%(V, I, 10);
                LPrint Tab(101); Using "##"; BRS%(V, I, 11);

                I2! = 0
                I3! = 0
                I4! = 0

                If BRS%(V, I, 0) > 0 Then I2! = Int((BRS%(V, I, 1) / BRS%(V, I, 0) + .0005) * 1000): I3! = Int(((BRS%(V, I, 1) + BRS%(V, I, 4) + 2 * BRS%(V, I, 5) + 3 * BRS%(V, I, 6)) / BRS%(V, I, 0) + .0005) * 1000): I4! = Int(((BRS%(V, I, 1) + BRS%(V, I, 7)) / (BRS%(V, I, 7) + BRS%(V, I, 0)) + .0005) * 1000)
                If BRS%(V, I, 0) = 0 Or I2! = 0 Then LPrint Tab(107); ".000";

                If I2! > 0 And I2! < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2!;
                If I2! >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2!;

                If BRS%(V, I, 0) = 0 Or I3! = O Then LPrint Tab(115); ".000";

                If I3! > 0 And I3! < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3!;
                If I3! >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3!;

                If BRS%(V, I, 0) = 0 Or I4! = 0 Then LPrint Tab(123); ".000"

                If I4! > 0 And I4! < 100 Then LPrint Tab(123); ".0";: LPrint Using "##"; I4!
                If I4! >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4!

            Next V

            1600 '
            VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16) + BRS%(15, I, 16)

            For V = 0 To VV

                TT1 = TT1 + BRS%(V, I, 14)
                TT3 = TT3 + BRS%(V, I, 2)
                TT5 = TT5 + BRS%(V, I, 4)
                TT7 = TT7 + BRS%(V, I, 6)
                TT9 = TT9 + BRS%(V, I, 7)
                TT11 = TT11 + BRS%(V, I, 9)
                TT13 = TT13 + BRS%(V, I, 11)
                TT2 = TT2 + BRS%(V, I, 0)
                TT4 = TT4 + BRS%(V, I, 1)
                TT6 = TT6 + BRS%(V, I, 5)
                TT8 = TT8 + BRS%(V, I, 3)
                TT10 = TT10 + BRS%(V, I, 8)
                TT12 = TT12 + BRS%(V, I, 10)

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

            I2! = 0

            If TT2 > 0 Then I2! = Int((TT4 / TT2 + .0005) * 1000)
            If TT2 = 0 Or I2! = 0 Then LPrint Tab(107); ".000";
            If I2! > 0 And I2! < 100 Then LPrint Tab(107); ".0";: LPrint Using "##"; I2!;
            If I2! >= 100 Then LPrint Tab(107); ".";: LPrint Using "###"; I2!;

            I3! = 0

            I3! = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000
            If TT2 = 0 Or I3! = O Then LPrint Tab(115); ".000";
            If I3! > 0 And I3! < 100 Then LPrint Tab(115); ".0";: LPrint Using "##"; I3!;
            If I3! >= 100 Then LPrint Tab(115); ".";: LPrint Using "###"; I3!;

            I4! = 0

            If TT9 + TT2 > 0 Then I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)
            If TT2 = 0 Or I4! = 0 Then LPrint Tab(123); ".000"
            If I4! > 0 And I4! < 100 Then LPrint Tab(123); ".0";: LPrint Using "##"; I4!
            If I4! >= 100 Then LPrint Tab(123); ".";: LPrint Using "###"; I4!

            TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0: TT7 = 0: TT8 = 0: TT9 = 0: TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0
            PTN = PTN + 2
            LPrint

        End If

    Next I

    LPrint Chr$(12)

    PTN = 0
    LPrint careerFile$
    LPrint "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

    For I = 0 To 79

        If careerP$(I) <> "" And careerP$(I) <> "XXX" Then
            VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15) + PRS%(15, I, 15)

            If 55 - PTN < VV Then
                LPrint Chr$(12)
                LPrint careerFile$
                LPrint "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"
                PTN = 0
            End If

            LPrint Left$(careerP$(I), 10);

            For V = 1 To VV

                PTN = PTN + 1

                LPrint Tab(11); PRS%(V, I, 16);
                LPrint Tab(20); Using "##"; PRS%(V, I, 6);
                LPrint Tab(26); Using "##"; PRS%(V, I, 7);
                LPrint Tab(32); Using "##"; PRS%(V, I, 12);
                LPrint Tab(39);: LPrint Using "###"; Int(PRS%(V, I, 0) / 3);

                If PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3) > 0 Then LPrint ".";: LPrint Using "#"; PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3);

                LPrint Tab(48); Using "###"; PRS%(V, I, 1);
                LPrint Tab(55); Using "###"; PRS%(V, I, 4);
                LPrint Tab(62); Using "###"; PRS%(V, I, 5);

                I2! = 0: If PRS%(V, I, 0) > 0 Then I2! = Int((((PRS%(V, I, 3) * 9) / (PRS%(V, I, 0) / 3)) + .0005) * 100)

                I3! = Int(I2! / 100): I4! = I2! - (I3! * 100)

                If PRS%(V, I, 0) = 0 Or I2! = 0 Then LPrint Tab(68); " 0.00";
                If PRS%(V, I, 0) > 0 And I2! > 0 And I2! <= 999 Then LPrint Tab(68); Using "##"; I3!;: LPrint ".";
                If PRS%(V, I, 0) > 0 And I2! >= 1000 Then LPrint Tab(68); Using "##"; I3!;: LPrint ".";
                If PRS%(V, I, 0) > 0 And I4! = 0 And I2! > 0 Then LPrint "00";
                If PRS%(V, I, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then LPrint "0";: LPrint Using "#"; I4!;
                If PRS%(V, I, 0) > 0 And I4! >= 10 And I2! > 0 Then LPrint Using "##"; I4!;

                LPrint Tab(77); Using "##"; PRS%(V, I, 8);
                LPrint Tab(83); Using "##"; PRS%(V, I, 9);
                LPrint Tab(89); Using "##"; PRS%(V, I, 10);
                LPrint Tab(95); Using "###"; PRS%(V, I, 2);
                LPrint Tab(102); Using "###"; PRS%(V, I, 3);
                LPrint Tab(110); Using "##"; PRS%(V, I, 11);
                LPrint Tab(116); Using "##"; PRS%(V, I, 14);
                LPrint Tab(121); Using "###"; PRS%(V, I, 13)

            Next V

        End If 'Done checking for terminal characters

        1700 '
        VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15) + PRS%(15, I, 15)

        For V = 0 To VV

            TT13 = TT13 + PRS%(V, I, 6)
            TT15 = TT15 + PRS%(V, I, 12)
            TT17 = TT17 + PRS%(V, I, 1)
            TT19 = TT19 + PRS%(V, I, 5)
            TT21 = TT21 + PRS%(V, I, 8)
            TT23 = TT23 + PRS%(V, I, 10)
            TT25 = TT25 + PRS%(V, I, 3)
            TT27 = TT27 + PRS%(V, I, 14)
            TT14 = TT14 + PRS%(V, I, 7)
            TT16 = TT16 + PRS%(V, I, 0)
            TT18 = TT18 + PRS%(V, I, 4)
            TT20 = TT20
            TT22 = TT22 + PRS%(V, I, 9)
            TT24 = TT24 + PRS%(V, I, 2)
            TT26 = TT26 + PRS%(V, I, 11)
            TT28 = TT28 + PRS%(V, I, 13)

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

        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

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

        TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0
        PTN = PTN + 2
        LPrint

    Next I

    LPrint Chr$(12)

End Sub

'----------------------------------------
'     CareerToScreen Subroutine
'----------------------------------------
'...explanation...
Sub CareerToScreen (careerFile$)

    Do

        Color 15, 0
        Cls

        Color 7: Print "VIEW ";
        Color 12: Print "B";: Color 7: Print "ATTERS, ";
        Color 12: Print "P";: Color 7: Print "ITCHERS, ";
        Color 12: Print "R";: Color 7: Print "OSTER IN SEQUENCE, OR ";
        Color 12: Print "E";: Color 7: Print "XIT TO MAIN MENU"
        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "B":
                Call ViewLeaders_Batters(careerFile$)

            Case "P":
                Call ViewLeaders_Pitchers(careerFile$)

            Case "R":
                Call ViewLeaders_Sequence(careerFile$)

        End Select

    Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'     ViewLeaders_Batters Subroutine
'----------------------------------------
'...explanation...
Sub ViewLeaders_Batters (careerFile$)

    Dim I2!, I3!, I4!
    Shared careerB$()
    Shared BRS%()

    Cls
    Color 12: Print careerFile$

    For I = 0 To 19
        Color 11: Locate I + 2
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerB$(I), 12)
    Next

    For I = 20 To 39
        Color 11: Locate I - 18, 21
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerB$(I), 12)
    Next

    For I = 40 To 59
        Color 11: Locate I - 38, 41
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerB$(I), 12)
    Next

    For I = 60 To 79
        Color 11: Locate I - 58, 61
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerB$(I), 12)
    Next

    Print
    Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9

    Cls

    Color 11
    Locate 1: Print careerFile$
    Color 12
    Locate 2: Print "BATTER     YEAR   G   AB   R   H 2B 3B HR RBI  BB  SO  SB CS  E  AVG  SLG   OB"
    Color 15
    Locate 3: Print Left$(careerB$(B9), 13);

    VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) + BRS%(14, B9, 16) + BRS%(15, B9, 16)

    For V = 1 To VV

        Color 7
        Print Tab(11); BRS%(V, B9, 17);
        Print Tab(17); Using "###"; BRS%(V, B9, 14);
        Print Tab(22); Using "###"; BRS%(V, B9, 0);
        Print Tab(26); Using "###"; BRS%(V, B9, 2);
        Print Tab(30); Using "###"; BRS%(V, B9, 1);
        Print Tab(34); Using "##"; BRS%(V, B9, 4);
        Print Tab(37); Using "##"; BRS%(V, B9, 5);
        Print Tab(40); Using "##"; BRS%(V, B9, 6);
        Print Tab(43); Using "###"; BRS%(V, B9, 3);
        Print Tab(47); Using "###"; BRS%(V, B9, 7);
        Print Tab(51); Using "###"; BRS%(V, B9, 8);
        Print Tab(55); Using "###"; BRS%(V, B9, 9);
        Print Tab(59); Using "##"; BRS%(V, B9, 10);
        Print Tab(62); Using "##"; BRS%(V, B9, 11);

        I2! = 0
        I3! = 0
        I4! = 0

        If BRS%(V, B9, 0) > 0 Then I2! = Int((BRS%(V, B9, 1) / BRS%(V, B9, 0) + .0005) * 1000): I3! = Int(((BRS%(V, B9, 1) + BRS%(V, B9, 4) + 2 * BRS%(V, B9, 5) + 3 * BRS%(V, B9, 6)) / BRS%(V, B9, 0) + .0005) * 1000): I4! = Int(((BRS%(V, B9, 1) + BRS%(V, B9, 7)) / (BRS%(V, B9, 7) + BRS%(V, B9, 0)) + .0005) * 1000)
        If BRS%(V, B9, 0) = 0 Or I2! = 0 Then Locate , 65: Print ".000";

        If I2! > 0 And I2! < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2!;
        If I2! >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2!;

        If BRS%(V, B9, 0) = 0 Or I3! = O Then Locate , 70: Print ".000";

        If I3! > 0 And I3! < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3!;
        If I3! >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3!;

        If BRS%(V, B9, 0) = 0 Or I4! = 0 Then Locate , 75: Print ".000"

        If I4! > 0 And I4! < 100 Then Locate , 75: Print ".";: Print Using "###"; I4!
        If I4! >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4!

    Next V

    VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) + BRS%(14, B9, 16) + BRS%(15, B9, 16)

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

    Print
    Color 3
    Print Tab(16); Using "####"; TT1;
    Print Tab(25); Using "####"; TT3;
    Print Tab(33); Using "###"; TT5;
    Print Tab(38); Using "####"; TT7;
    Print Tab(46); Using "####"; TT9;
    Print Tab(54); Using "####"; TT11;
    Print Tab(61); Using "###"; TT13;

    I3! = 0
    I3! = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

    If TT2 = 0 Or I3! = O Then Locate , 70: Print ".000"
    If I3! > 0 And I3! < 100 Then Locate , 70: Print ".0";: Print Using "##"; I3!
    If I3! >= 100 Then Locate , 70: Print ".";: Print Using "###"; I3!

    Print "TOTALS";

    Print Tab(13); Using "##"; VV;
    Print Tab(21); Using "####"; TT2;
    Print Tab(29); Using "####"; TT4;
    Print Tab(36); Using "###"; TT6;
    Print Tab(42); Using "####"; TT8;
    Print Tab(50); Using "####"; TT10;
    Print Tab(58); Using "###"; TT12;

    I2! = 0
    If TT2 > 0 Then I2! = Int((TT4 / TT2 + .0005) * 1000)

    If TT2 = 0 Or I2! = 0 Then Locate , 65: Print ".000";
    If I2! > 0 And I2! < 100 Then Locate , 65: Print ".0";: Print Using "##"; I2!;
    If I2! >= 100 Then Locate , 65: Print ".";: Print Using "###"; I2!;

    I4! = 0
    If TT9 + TT2 > 0 Then I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

    If TT2 = 0 Or I4! = 0 Then Locate , 75: Print ".000"
    If I4! > 0 And I4! < 100 Then Locate , 75: Print ".";: Print Using "###"; I4!
    If I4! >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4!

    Print
    Print
    Color 15: Input "HIT ENTER TO CONTINUE"; DUMMY$

End Sub

'----------------------------------------
'     ViewLeaders_Pitchers Subroutine
'----------------------------------------
'...explanation...
Sub ViewLeaders_Pitchers (careerFile$)

    Dim I2!, I3!, I4!
    Shared careerP$()
    Shared PRS%()

    Cls
    Color 12: Print careerFile$

    For I = 0 To 19
        Color 11: Locate I + 2
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerP$(I), 12)
    Next

    For I = 20 To 39
        Color 11: Locate I - 18, 21
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerP$(I), 12)
    Next

    For I = 40 To 59
        Color 11: Locate I - 38, 41
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerP$(I), 12)
    Next

    For I = 60 To 79
        Color 11: Locate I - 58, 61
        Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerP$(I), 12)
    Next

    Print

    Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9

    Cls

    Color 11
    Locate 1: Print careerFile$
    Color 12
    Locate 2: Print "PITCHER    YEAR  W  L  S   IP     H  BB  SO  ERA  G GS CG RUN  ER SHO HR DP"
    Color 15
    Locate 3: Print Left$(careerP$(B9), 13);

    VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) + PRS%(14, B9, 15) + PRS%(15, B9, 15)

    For V = 1 To VV

        Color 7: Print Tab(11); PRS%(V, B9, 16);

        Print Tab(17); Using "##"; PRS%(V, B9, 6);
        Print Tab(20); Using "##"; PRS%(V, B9, 7);
        Print Tab(23); Using "##"; PRS%(V, B9, 12);

        Locate , 27: Print Using "###"; Int(PRS%(V, B9, 0) / 3);

        If PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3);

        Print Tab(33); Using "###"; PRS%(V, B9, 1);
        Print Tab(37); Using "###"; PRS%(V, B9, 4);
        Print Tab(41); Using "###"; PRS%(V, B9, 5);

        I2! = 0
        If PRS%(V, B9, 0) > 0 Then I2! = Int((((PRS%(V, B9, 3) * 9) / (PRS%(V, B9, 0) / 3)) + .0005) * 100)

        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

        If PRS%(V, B9, 0) = 0 Or I2! = 0 Then Locate , 44: Print " 0.00";
        If PRS%(V, B9, 0) > 0 And I2! > 0 And I2! <= 999 Then Locate , 44: Print Using "##"; I3!;: Print ".";
        If PRS%(V, B9, 0) > 0 And I2! >= 1000 Then Locate , 44: Print Using "##"; I3!;: Print ".";
        If PRS%(V, B9, 0) > 0 And I4! = 0 And I2! > 0 Then Print "00";
        If PRS%(V, B9, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then Print "0";: Print Using "#"; I4!;
        If PRS%(V, B9, 0) > 0 And I4! >= 10 And I2! > 0 Then Print Using "##"; I4!;

        Print Tab(50); Using "##"; PRS%(V, B9, 8);
        Print Tab(53); Using "##"; PRS%(V, B9, 9);
        Print Tab(56); Using "##"; PRS%(V, B9, 10);
        Print Tab(59); Using "###"; PRS%(V, B9, 2);
        Print Tab(63); Using "###"; PRS%(V, B9, 3);
        Print Tab(68); Using "##"; PRS%(V, B9, 11);
        Print Tab(71); Using "##"; PRS%(V, B9, 14);
        Print Tab(73); Using "###"; PRS%(V, B9, 13)

    Next V

    Print
    Color 3

    VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) + PRS%(14, B9, 15) + PRS%(15, B9, 15)

    TT13 = 0: TT14 = 0: TT15 = 0: TT16 = 0: TT17 = 0: TT18 = 0: TT19 = 0: TT20 = 0: TT21 = 0: TT22 = 0: TT23 = 0: TT24 = 0: TT25 = 0: TT26 = 0: TT27 = 0: TT28 = 0

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

    Print
    Color 3

    Print Tab(16); Using "###"; TT13;
    Print Tab(22); Using "###"; TT15;
    Print Tab(32); Using "####"; TT17;
    Print Tab(40); Using "####"; TT19;
    Print Tab(49); Using "###"; TT21;
    Print Tab(55); Using "###"; TT23;
    Print Tab(62); Using "####"; TT25;
    Print Tab(69); Using "####"; TT27
    Print "TOTALS";
    Print Tab(13); Using "###"; VV;
    Print Tab(19); Using "###"; TT14;
    Print Tab(27); Using "###"; Int(TT16 / 3);

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then Print ".";: Print Using "#"; TT16 - (Int(TT16 / 3) * 3);

    Print Tab(37); Using "###"; TT18;

    I2! = 0
    If TT16 > 0 Then I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

    I3! = Int(I2! / 100)
    I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then Locate , 44: Print " 0.00";
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then Locate , 44: Print Using "##"; I3!;: Print ".";
    If TT16 > 0 And I2! >= 1000 Then Locate , 44: Print Using "##"; I3!;: Print ".";
    If TT16 > 0 And I4! = 0 And I2! > 0 Then Print "00";
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then Print "0";: Print Using "#"; I4!;
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then Print Using "##"; I4!;

    Print Tab(51); Using "####"; TT22;
    Print Tab(59); Using "###"; TT24;
    Print Tab(66); Using "####"; TT26;
    Print Tab(73); Using "###"; TT28

    Print
    Print
    Color 15: Input "HIT ENTER TO CONTINUE"; DUMMY$

End Sub

'----------------------------------------
'     ViewLeaders_Sequence Subroutine
'----------------------------------------
'...explanation...
Sub ViewLeaders_Sequence (careerFile$)

    Dim I2!, I3!, I4!
    Shared BRS%(), PRS%()
    Shared careerB$(), careerP$()

    Do

        Cls

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

        Color 11
        Locate 1: Print careerFile$
        Color 12
        Locate 2: Print "BATTER     YEAR   G   AB   R   H 2B 3B HR RBI  BB  SO  SB CS  E  AVG  SLG   OB"

        For I = 0 To 99

            If careerB$(I) <> "" And careerB$(I) <> "XXX" Then

                Color 15
                Locate 3: Print Left$(careerB$(I), 10);

                VV = BRS%(1, I, 16) + BRS%(2, I, 16) + BRS%(3, I, 16) + BRS%(4, I, 16) + BRS%(5, I, 16) + BRS%(6, I, 16) + BRS%(7, I, 16) + BRS%(8, I, 16) + BRS%(9, I, 16) + BRS%(10, I, 16) + BRS%(11, I, 16) + BRS%(12, I, 16) + BRS%(13, I, 16) + BRS%(14, I, 16) + BRS%(15, I, 16)

                For V = 1 To VV

                    Color 7
                    Print Tab(11); BRS%(V, I, 17);

                    Print Tab(17); Using "###"; BRS%(V, I, 14);
                    Print Tab(22); Using "###"; BRS%(V, I, 0);
                    Print Tab(26); Using "###"; BRS%(V, I, 2);
                    Print Tab(30); Using "###"; BRS%(V, I, 1);
                    Print Tab(34); Using "##"; BRS%(V, I, 4);
                    Print Tab(37); Using "##"; BRS%(V, I, 5);
                    Print Tab(40); Using "##"; BRS%(V, I, 6);
                    Print Tab(43); Using "###"; BRS%(V, I, 3);
                    Print Tab(47); Using "###"; BRS%(V, I, 7);
                    Print Tab(51); Using "###"; BRS%(V, I, 8);
                    Print Tab(55); Using "###"; BRS%(V, I, 9);
                    Print Tab(59); Using "##"; BRS%(V, I, 10);
                    Print Tab(62); Using "##"; BRS%(V, I, 11);

                    I2! = 0: I3! = 0: I4! = 0

                    If BRS%(V, I, 0) > 0 Then I2! = Int((BRS%(V, I, 1) / BRS%(V, I, 0) + .0005) * 1000): I3! = Int(((BRS%(V, I, 1) + BRS%(V, I, 4) + 2 * BRS%(V, I, 5) + 3 * BRS%(V, I, 6)) / BRS%(V, I, 0) + .0005) * 1000): I4! = Int(((BRS%(V, I, 1) + BRS%(V, I, 7)) / (BRS%(V, I, 7) + BRS%(V, I, 0)) + .0005) * 1000)

                    If I2! = 1000 Then
                        Locate , 65: Print "1.000";
                    Else
                        If BRS%(V, I, 0) = 0 Or I2! = 0 Then Locate , 65: Print ".000 ";

                        If I2! > 0 And I2! < 100 Then Locate , 65: Print ".0";: Print Using "## "; I2!;
                        If I2! >= 100 Then Locate , 65: Print Using ".### "; I2! / 1000;
                    End If

                    If I3! = 1000 Then
                        Locate , 65: Print "1.000";
                    Else
                        If BRS%(V, I, 0) = 0 Or I3! = O Then Locate , 70: Print ".000 ";
                        If I3! > 0 And I3! < 100 Then Locate , 70: Print ".0";: Print Using "## "; I3!;
                        If I3! >= 100 Then Locate , 70: Print Using ".### "; I3! / 1000;
                    End If

                    If I4! = 1000 Then
                        Locate , 65: Print "1.000"
                    Else
                        If BRS%(V, I, 0) = 0 Or I4! = 0 Then Locate , 76: Print ".000 "
                        If I4! > 0 And I4! < 100 Then Locate , 76: Print ".0";: Print Using "## "; I4!
                        If I4! >= 100 Then Locate , 76: Print Using ".### "; I4! / 1000
                    End If

                Next V

                Call BatterTotals

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

                Do
                    Locate 10: Color 14
                    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
                    I$ = GetKeyPress$

                    If I$ = Chr$(0) Then

                        For T = 3 To 20:
                            Locate T
                            Print Space$(78)
                        Next

                    End If

                    If I$ = Chr$(27) Then Exit For

                Loop Until I$ = Chr$(27) Or I$ = Chr$(13)

            End If

        Next I

        If I$ <> Chr$(27) Then

            Cls

            Color 11
            Locate 1: Print careerFile$
            Color 12
            Locate 2: Print "PITCHER    YEAR  W  L  S   IP     H  BB  SO  ERA  G GS CG RUN  ER SHO HR DP"

            For I = 0 To 79

                If careerP$(I) <> "" And careerP$(I) <> "XXX" Then

                    Color 15
                    Locate 3: Print Left$(careerP$(I), 10);

                    VV = PRS%(1, I, 15) + PRS%(2, I, 15) + PRS%(3, I, 15) + PRS%(4, I, 15) + PRS%(5, I, 15) + PRS%(6, I, 15) + PRS%(7, I, 15) + PRS%(8, I, 15) + PRS%(9, I, 15) + PRS%(10, I, 15) + PRS%(11, I, 15) + PRS%(12, I, 15) + PRS%(13, I, 15) + PRS%(14, I, 15) + PRS%(15, I, 15)

                    For V = 1 To VV

                        Color 7
                        Print Tab(11); PRS%(V, I, 16);

                        Print Tab(17); Using "##"; PRS%(V, I, 6);
                        Print Tab(20); Using "##"; PRS%(V, I, 7);
                        Print Tab(23); Using "##"; PRS%(V, I, 12);
                        Locate , 27: Print Using "###"; Int(PRS%(V, I, 0) / 3);

                        If PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3);

                        Print Tab(33); Using "###"; PRS%(V, I, 1);
                        Print Tab(37); Using "###"; PRS%(V, I, 4);
                        Print Tab(41); Using "###"; PRS%(V, I, 5);

                        I2! = 0
                        If PRS%(V, I, 0) > 0 Then I2! = Int((((PRS%(V, I, 3) * 9) / (PRS%(V, I, 0) / 3)) + .0005) * 100)

                        I3! = Int(I2! / 100): I4! = I2! - (I3! * 100)

                        If PRS%(V, I, 0) = 0 Or I2! = 0 Then Locate , 44: Print " 0.00";
                        If PRS%(V, I, 0) > 0 And I2! > 0 And I2! <= 999 Then Locate , 44: Print Using "##"; I3!;: Print ".";
                        If PRS%(V, I, 0) > 0 And I2! >= 1000 Then Locate , 44: Print Using "##"; I3!;: Print ".";
                        If PRS%(V, I, 0) > 0 And I4! = 0 And I2! > 0 Then Print "00";
                        If PRS%(V, I, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then Print "0";: Print Using "#"; I4!;
                        If PRS%(V, I, 0) > 0 And I4! >= 10 And I2! > 0 Then Print Using "##"; I4!;

                        Print Tab(50); Using "##"; PRS%(V, I, 8);
                        Print Tab(53); Using "##"; PRS%(V, I, 9);
                        Print Tab(56); Using "##"; PRS%(V, I, 10);
                        Print Tab(59); Using "###"; PRS%(V, I, 2);
                        Print Tab(63); Using "###"; PRS%(V, I, 3);
                        Print Tab(68); Using "##"; PRS%(V, I, 11);
                        Print Tab(71); Using "##"; PRS%(V, I, 14);
                        Print Tab(73); Using "###"; PRS%(V, I, 13)

                    Next V

                    Call PitcherTotals

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

                    Do
                        Locate 10: Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
                        I$ = GetKeyPress$

                        If I$ = Chr$(0) Then

                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next

                        End If

                    Loop Until I$ = Chr$(27) Or I$ = Chr$(13)

                    If I$ = Chr$(27) Then Exit For

                End If

            Next I

            If I$ <> Chr$(27) Then

                Color 15, 0

                Cls
                JJ = 1
                Locate 6, 8
                Color 15, 1

                'Print box top
                Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

                'Print box sides
                For II = 1 To JJ
                    Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next

                'Print box bottom
                Locate 7 + JJ, 8
                Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                Do
                    Locate 7
                    U$ = "DISPLAY STATS AGAIN (YN)"
                    Locate , (80 - Len(U$)) / 2
                    Print U$
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"


            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


