'----------------------------------------
'           CAREDIT Subroutine
'----------------------------------------
'This routine allows the user to choose a
'career file, then loads in the stats and 
'gives the users options for editing and 
'saving the file.
Sub CAREDIT ()

    _Title "Full Count Baseball - Edit Career Files"

    Shared careerB$(), careerP$()

    Shared BRS%(), PRS%()

    Color 15, 0
    Cls

    careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        TMR = 2700

        Color 15, 0
        Cls
        Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #2

        Input #2, NB, NP

        For I = 0 To 99: Input #2, careerB$(I): Next

        For I = 0 To 79: Input #2, careerP$(I): Next

        For J = 1 To 15: 

            For I = 0 To 99: 

                For I1 = O To 17: 
                    Input #2, BRS%(J, I, I1)
                Next I1

                Color 3
                Locate 12, 38
                TMR = TMR - 1: Print Using "####"; TMR

            Next I

        Next J

        For J = 1 To 15: 

            For I = 0 To 79: 

                For I1 = 0 To 16: 
                    Input #2, PRS%(J, I, I1)
                Next I1

                Color 3
                Locate 12, 38
                TMR = TMR - 1: Print Using "####"; TMR

            Next I

        Next J

        Close #2

        Do
            Cls

            Color 7: Print "EDIT ";
            Color 12: Print "B";
            Color 7: Print "ATTERS, ";
            Color 12: Print "P";
            Color 7: Print "ITCHERS, ";
            Color 12: Print "U";
            Color 7: Print "PDATE CARER FILE, OR ";
            Color 12: Print "E";
            Color 7: Print "XIT WITHOUT UPDATING"

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "B": Call EditBatters (careerFile$)

                Case "E", "X": '

                Case "P": Call EditPitchers (careerFile$)

                Case "U":
                    Cls
                    Color 15
                    Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

                    TMR = 2700

                    Open diskPaths$(1) + careerFile$ + ".CAR" For Output As #3

                    Write #3, NB, NP

                    For I = 0 To 99: Write #3, careerB$(I): Next

                    For I = 0 To 79: Write #3, careerP$(I): Next

                    For J = 1 To 15: 

                        For I = 0 To 99: 

                            For I1 = O To 17: 
                                Write #3, BRS%(J, I, I1)
                            Next I1

                            Color 3
                            Locate 12, 38
                            TMR = TMR - 1: Print Using "####"; TMR

                        Next I

                    Next J

                    For J = 1 To 15:

                        For I = 0 To 79: 

                            For I1 = 0 To 16: 
                                Write #3, PRS%(J, I, I1)
                            Next I1

                            Color 3
                            Locate 12, 38
                            TMR = TMR - 1: Print Using "####"; TMR

                        Next I

                    Next J

                    Close #3

            End Select

        Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X"

    End If

End Sub

'----------------------------------------
'         EditBatters Subroutine
'----------------------------------------
'This routine gives the user a choice of
'batters to edit from the career stats,
'then presents options for handling them
'before deletions or edits are processed.
Sub EditBatters (careerFile$)

    Shared careerB$()

    Shared BRS%()

    Do

        Cls
        Color 12: Print careerFile$

        For I = 0 To 19
            Color 11: Locate I + 2, 1
            Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 20 To 39
            Color 11: Locate I - 18, 16
            Print Using "##"; I;: Print " ";: 
            Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 40 To 59
            Color 11: Locate I - 38, 32
            Print Using "##"; I;: Print " ";: 
            Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 60 To 79
            Color 11: Locate I - 58, 48
            Print Using "##"; I;: Print " ";: 
            Color 7: Print Left$(careerB$(I), 12)
        Next

        For I = 80 To 99
            Color 11: Locate I - 78, 64
            Print Using "##"; I;: Print " ";: 
            Color 7: Print Left$(careerB$(I), 12)
        Next

        Print

        Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

        If B9 <> 100 Then

            If B9 < 99 Then 

                Cls

                Color 15
                Locate 5, 20: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerB$(B9)
                Color 15
                Locate , 20: Print "BEFORE DELETING THEM? (Y/N)"

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
'         EditPitchers Subroutine
'----------------------------------------
'This routine gives the user a choice of
'pitchers to edit from the career stats,
'then presents options for handling them
'before deletions or edits are processed.
Sub EditPitchers (careerFile$)

    Shared careerP$()

    Shared PRS%()

    Do
        Cls
        Color 12: Print careerFile$

        For I = 0 To 19
            Color 11: Locate I + 2, 1
            Print Using "##"; I;: Print " ";: 
            Color 7: Print Left$(careerP$(I), 15)
        Next

        For I = 20 To 39
            Color 11: Locate I - 18, 21
            Print Using "##"; I;
            Print " ";: 
            Color 7: Print Left$(careerP$(I), 15)
        Next

        For I = 40 To 59
            Color 11: Locate I - 38, 41
            Print Using "##"; I;
            Print " ";: 
            Color 7: Print Left$(careerP$(I), 15)
        Next

        For I = 60 To 79
            Color 11: Locate I - 58, 61
            Print Using "##"; I;
            Print " ";: 
            Color 7: Print Left$(careerP$(I), 15)
        Next

        Print

        Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

        If B9 <> 100 Then

            If B9 < 79 Then

                Cls

                Color 15
                Locate 5, 20: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerB$(B9)
                Color 15
                Locate , 20: Print "BEFORE DELETING THEM? (Y/N)"

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
'This routine formats and prints career 
'stats associated with the team's batters.
Sub PrintBatterStats (printDest, destFile$, careerFile$, playerIdx)

    Dim I2!, I3!, I4!

    Shared careerB$()
    Shared BRS%()

    B9 = playerIdx

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    Print #1, careerFile$

    Print #1, "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

    VV = BRS%(1, B9, 16) + BRS%(2, B9, 16) + BRS%(3, B9, 16) + BRS%(4, B9, 16) + BRS%(5, B9, 16) + BRS%(6, B9, 16) + BRS%(7, B9, 16) + BRS%(8, B9, 16) + BRS%(9, B9, 16) + BRS%(10, B9, 16) + BRS%(11, B9, 16) + BRS%(12, B9, 16) + BRS%(13, B9, 16) + BRS%(14, B9, 16) + BRS%(15, B9, 16)

    Print #1, Left$(careerB$(B9), 10);

    For V = 1 To VV

        Print #1, Tab(11); BRS%(V, B9, 17);
        Print #1, Tab(20); Using "###"; BRS%(V, B9, 14);
        Print #1, Tab(28); Using "###"; BRS%(V, B9, 0);
        Print #1, Tab(35); Using "###"; BRS%(V, B9, 2);
        Print #1, Tab(42); Using "###"; BRS%(V, B9, 1);
        Print #1, Tab(49); Using "##"; BRS%(V, B9, 4);
        Print #1, Tab(55); Using "##"; BRS%(V, B9, 5);
        Print #1, Tab(61); Using "##"; BRS%(V, B9, 6);
        Print #1, Tab(67); Using "###"; BRS%(V, B9, 3);
        Print #1, Tab(74); Using "###"; BRS%(V, B9, 7);
        Print #1, Tab(81); Using "###"; BRS%(V, B9, 8);
        Print #1, Tab(88); Using "###"; BRS%(V, B9, 9);
        Print #1, Tab(95); Using "##"; BRS%(V, B9, 10);
        Print #1, Tab(101); Using "##"; BRS%(V, B9, 11);

        I2! = 0
        I3! = 0
        I4! = 0

        IF BRS%(V, B9, 0) > 0 THEN I2! = INT((BRS%(V, B9, 1) / BRS%(V, B9, 0) + .0005) * 1000): I3! = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 4) + 2 * BRS%(V, B9, 5) + 3 * BRS%(V, B9, 6)) / BRS%(V, B9, 0) + .0005) * 1000): I4! = INT(((BRS%(V, B9, 1) + BRS%(V, B9, 7)) / (BRS%(V, B9, 7) + BRS%(V, B9, 0)) + .0005) * 1000)
        If BRS%(V, B9, 0) = 0 Or I2! = 0 Then Print #1, Tab(107); ".000";

        If I2! > 0 And I2! < 100 Then Print #1, Tab(107); ".0";: Print #1, Using "##"; I2!;
        If I2! >= 100 Then Print #1, Tab(107); ".";: Print #1, Using "###"; I2!;

        If BRS%(V, B9, 0) = 0 Or I3! = 0 Then Print #1, Tab(115); ".000";

        If I3! > 0 And I3! < 100 Then Print #1, Tab(115); ".0";: Print #1, Using "##"; I3!;
        If I3! >= 100 Then Print #1, Tab(115); ".";: Print #1, Using "###"; I3!;

        If BRS%(V, B9, 0) = 0 Or I4! = 0 Then Print #1, Tab(123); ".000"

        If I4! > 0 And I4! < 100 Then Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
        If I4! >= 100 Then Print #1, Tab(123); ".";: Print #1, Using "###"; I4!

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

    Print #1, "   Totals";
    Print #1, Tab(14); Using "##"; VV;
    Print #1, Tab(19); Using "####"; TT1;
    Print #1, Tab(27); Using "####"; TT2;
    Print #1, Tab(34); Using "####"; TT3;
    Print #1, Tab(41); Using "####"; TT4;
    Print #1, Tab(48); Using "###"; TT5;
    Print #1, Tab(54); Using "###"; TT6;
    Print #1, Tab(59); Using "####"; TT7;
    Print #1, Tab(66); Using "####"; TT8;
    Print #1, Tab(73); Using "####"; TT9;
    Print #1, Tab(80); Using "####"; TT10;
    Print #1, Tab(87); Using "####"; TT11;
    Print #1, Tab(94); Using "###"; TT12;
    Print #1, Tab(100); Using "###"; TT13;

    I2! = 0
    If TT2 > 0 Then I2! = Int((TT4 / TT2 + .0005) * 1000)

    If TT2 = 0 Or I2! = 0 Then Print #1, Tab(107); ".000";
    If I2! > 0 And I2! < 100 Then Print #1, Tab(107); ".0";: Print #1, Using "##"; I2!;
    If I2! >= 100 Then Print #1, Tab(107); ".";: Print #1, Using "###"; I2!;

    I3! = 0
    I3! = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

    If TT2 = 0 Or I3! = 0 Then Print #1, Tab(115); ".000";
    If I3! > 0 And I3! < 100 Then Print #1, Tab(115); ".0";: Print #1, Using "##"; I3!;
    If I3! >= 100 Then Print #1, Tab(115); ".";: Print #1, Using "###"; I3!;

    I4! = 0
    If TT9 + TT2 > 0 Then I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)

    If TT2 = 0 Or I4! = 0 Then Print #1, Tab(123); ".000"
    If I4! > 0 And I4! < 100 Then Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
    If I4! >= 100 Then Print #1, Tab(123); ".";: Print #1, Using "###"; I4!

    'Why is this important?
    'Only being scoped to this routine, wouldn't these stats be 0 each time it's called?
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

    Print #1,

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

End Sub

'----------------------------------------
'      PrintPitcherStats Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's pitchers.
Sub PrintPitcherStats (printDest, destFile$, careerFile$, playerIdx)

    Dim I2!, I3!, I4!

    Shared careerP$()

    Shared PRS%()

    B9 = playerIdx

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    Print #1, careerFile$

    Print #1, "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

    VV = PRS%(1, B9, 15) + PRS%(2, B9, 15) + PRS%(3, B9, 15) + PRS%(4, B9, 15) + PRS%(5, B9, 15) + PRS%(6, B9, 15) + PRS%(7, B9, 15) + PRS%(8, B9, 15) + PRS%(9, B9, 15) + PRS%(10, B9, 15) + PRS%(11, B9, 15) + PRS%(12, B9, 15) + PRS%(13, B9, 15) + PRS%(14, B9, 15) + PRS%(15, B9, 15)

    Print #1, Left$(careerP$(B9), 10);

    For V = 1 To VV

        Print #1, Tab(11); PRS%(V, B9, 16);
        Print #1, Tab(20); Using "##"; PRS%(V, B9, 6);
        Print #1, Tab(26); Using "##"; PRS%(V, B9, 7);
        Print #1, Tab(32); Using "##"; PRS%(V, B9, 12);
        Print #1, Tab(39);: Print #1, Using "###"; Int(PRS%(V, B9, 0) / 3);

        If PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3) > 0 Then Print #1, ".";: Print #1, Using "#"; PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3);

        Print #1, Tab(48); Using "###"; PRS%(V, B9, 1);
        Print #1, Tab(55); Using "###"; PRS%(V, B9, 4);
        Print #1, Tab(62); Using "###"; PRS%(V, B9, 5)

        I2! = 0
        If PRS%(V, B9, 0) > 0 Then I2! = Int((((PRS%(V, B9, 3) * 9) / (PRS%(V, B9, 0) / 3)) + .0005) * 100)

        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

        If PRS%(V, B9, 0) = 0 Or I2! = 0 Then Print #1, Tab(68); " 0.00";
        If PRS%(V, B9, 0) > 0 And I2! > 0 And I2! <= 999 Then Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
        If PRS%(V, B9, 0) > 0 And I2! >= 1000 Then Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
        If PRS%(V, B9, 0) > 0 And I4! = 0 And I2! > 0 Then Print #1, "00";
        If PRS%(V, B9, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then Print #1, "0";: Print #1, Using "#"; I4!;
        If PRS%(V, B9, 0) > 0 And I4! >= 10 And I2! > 0 Then Print #1, Using "##"; I4!;

        Print #1, Tab(77); Using "##"; PRS%(V, B9, 8);
        Print #1, Tab(83); Using "##"; PRS%(V, B9, 9);
        Print #1, Tab(89); Using "##"; PRS%(V, B9, 10);
        Print #1, Tab(95); Using "###"; PRS%(V, B9, 2);
        Print #1, Tab(102); Using "###"; PRS%(V, B9, 3);
        Print #1, Tab(110); Using "##"; PRS%(V, B9, 11);
        Print #1, Tab(116); Using "##"; PRS%(V, B9, 14);
        Print #1, Tab(121); Using "###"; PRS%(V, B9, 13)

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

    Print #1, "   Totals";
    Print #1, Tab(13); Using "###"; VV;
    Print #1, Tab(19); Using "###"; TT13;
    Print #1, Tab(25); Using "###"; TT14;
    Print #1, Tab(31); Using "###"; TT15;
    Print #1, Tab(39); Using "###"; Int(TT16 / 3);

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then Print #1, ".";: Print #1, Using "#"; TT16 - (Int(TT16 / 3) * 3);

    Print #1, Tab(47); Using "####"; TT17;
    Print #1, Tab(55); Using "###"; TT18;
    Print #1, Tab(61); Using "####"; TT19;

    I2! = 0
    If TT16 > 0 Then I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)
    I3! = Int(I2! / 100): I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then Print #1, Tab(68); " 0.00";
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
    If TT16 > 0 And I2! >= 1000 Then Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
    If TT16 > 0 And I4! = 0 And I2! > 0 Then Print #1, "00";
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then Print #1, "0";: Print #1, Using "#"; I4!;
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then Print #1, Using "##"; I4!;

    Print #1, Tab(76); Using "###"; TT21;
    Print #1, Tab(81); Using "####"; TT22;
    Print #1, Tab(88); Using "###"; TT23;
    Print #1, Tab(95); Using "###"; TT24;
    Print #1, Tab(101); Using "####"; TT25;
    Print #1, Tab(108); Using "####"; TT26;
    Print #1, Tab(114); Using "####"; TT27;
    Print #1, Tab(121); Using "###"; TT28
    
    Print #1, 

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

End Sub
