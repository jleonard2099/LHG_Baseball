'----------------------------------------
'           CAREDIT Subroutine
'----------------------------------------
'This routine allows the user to choose a
'career file, then loads in the stats and
'gives the users options for editing and
'saving the file.
Sub CAREDIT ()

    _Title "Full Count Baseball - Edit Career Files"

    Color 15, 0
    Cls

    careerFile$ = _OpenFileDialog$("Choose Career file", diskPaths$(1), "*.CAR", "Career files [.CAR]")
    'ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

		Call LoadCareerFile (careerFile$)

        Do

            Cls

            Color 7: Print "EDIT ";
            Color 12: Print "B";
            Color 7: Print "ATTERS"

            Color 12: Print "EDIT ";
            Color 12: Print "P";
            Color 7: Print "ITCHERS"

            Color 12: Print "U";
            Color 7: Print "PDATE CARER FILE"

            Color 12: Print "E";
            Color 7: Print "XIT WITHOUT UPDATING"

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "B": 
                    Call EditBatters (GetFileBaseName$(careerFile$))

                Case "E", "X": '

                Case "P": 
                    Call EditPitchers (GetFileBaseName$(careerFile$))

                Case "U":
                    Call SaveCareerFile (careerFile$)

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
Sub EditBatters (careerName$)

    Shared careerBatters$()

    Shared carRecBatStats()

    Do

        Cls
        Color 12: Print careerName$

        For I = 0 To 19
            Locate I + 2, 1
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerBatters$(I), 12)
        Next

        For I = 20 To 39
            Locate I - 18, 16
            Color 11: Print Using "##"; I;
            Print " "; 
            Color 7: Print Left$(careerBatters$(I), 12)
        Next

        For I = 40 To 59
            Locate I - 38, 32
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerBatters$(I), 12)
        Next

        For I = 60 To 79
            Locate I - 58, 48
            Color 11: Print Using "##"; I;
            Print " "; 
            Color 7: Print Left$(careerBatters$(I), 12)
        Next

        For I = 80 To 99
            Locate I - 78, 64
            Color 11: Print Using "##"; I;
            Print " "; 
            Color 7: Print Left$(careerBatters$(I), 12)
        Next

        Print

        Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

        If B9 <> 100 Then

            If B9 < 99 Then 

                Cls

                Color 15
                Locate 5, 20: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerBatters$(B9)
                Color 15
                Locate , 20: Print "BEFORE DELETING THEM? (Y/N)"

                Color 14
                Print
                Locate , 20: Print "IF YOU DO NOT WISH TO DELETE THE STATS"
                Locate , 20: Print "OF ";
                Color 12: Print careerBatters$(B9);
                Color 14: Print " HIT 'E', 'X' or 'Esc'"

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)

                        Case "Y":
                            userKey$ = GetPrintOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                                        If userFile$ <> "" Then
                                            Call PrintBatterStats (0, userFile$, careerName$, B9)
                                        End If

                                    Case "P"
                                        Call PrintBatterStats (1, "REPORT", careerName$, B9)

                                End Select
                                
                            End If

                        Case "N":
                            Cls

                        Case "E", "X", Chr$(27):
                            Exit Sub

                    End Select

                Loop Until UCase$(I$) = "N"

                Cls
                Locate 5, 15: Print "CHANGING BATTER'S FILES . . . . . PLEASE WAIT"

                For I = B9 To 98: 
                    careerBatters$(I) = careerBatters$(I + 1)
                Next

                UPP = (99 - B9) * 15
                I = B9

                For J = 1 To MAX_CAREER_YEARS
                    For I = B9 To 98
                        For I1 = 0 To 17
                            carRecBatStats(J, I, I1) = carRecBatStats(J, I + 1, I1)
                        Next

                        Locate 7, 36
                        Color 12: UPP = UPP - 1
                        Print Using "####"; UPP
                    Next
                Next

            End If

            If B9 < 100 Then 'we have to ignore if they entered over 100
                careerBatters$(99) = ""
                For J = 1 To MAX_CAREER_YEARS
                    For I1 = 0 To 17
                        carRecBatStats(J, 99, I1) = 0
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
Sub EditPitchers (careerName$)

    Shared careerPitchers$()

    Shared carRecPitStats()

    Do
        Cls
        Color 12: Print careerName$

        For I = 0 To 19
            Color 11: Locate I + 2, 1
            Print Using "##"; I;: Print " ";: 
            Color 7: Print Left$(careerPitchers$(I), 15)
        Next

        For I = 20 To 39
            Color 11: Locate I - 18, 21
            Print Using "##"; I;
            Print " ";: 
            Color 7: Print Left$(careerPitchers$(I), 15)
        Next

        For I = 40 To 59
            Color 11: Locate I - 38, 41
            Print Using "##"; I;
            Print " ";: 
            Color 7: Print Left$(careerPitchers$(I), 15)
        Next

        For I = 60 To 79
            Color 11: Locate I - 58, 61
            Print Using "##"; I;
            Print " ";: 
            Color 7: Print Left$(careerPitchers$(I), 15)
        Next

        Print

        Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (100 TO EXIT)"; B9

        If B9 <> 100 Then

            If B9 < 79 Then

                Cls

                Color 15
                Locate 5, 20: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerBatters$(B9)
                Color 15
                Locate , 20: Print "BEFORE DELETING THEM? (Y/N)"

                Color 14
                Print
                Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
                Locate , 20: Print "OF ";
                Color 12: Print careerPitchers$(B9);
                Color 14: Print " HIT 'E'"

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)

                        Case "Y":
                            userKey$ = GetPrintOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                                        If userFile$ <> "" Then
                                            Call PrintPitcherStats(0, userFile$, careerName$, B9)
                                        End If

                                    Case "P"
                                        Call PrintPitcherStats(1, "REPORT", careerName$, B9)

                                End Select
                                
                            End If

                        Case "N":
                            Cls

                        Case "E":
                            Exit Sub

                    End Select

                Loop Until UCase$(I$) = "N"

                Cls
                Locate 5, 15: Print "CHANGING PITCHER'S FILES . . . . . PLEASE WAIT"

                For I = B9 To 78: 
                    careerPitchers$(I) = careerPitchers$(I + 1)
                Next

                UPP = (79 - B9) * 15
                I = B9

                For J = 1 To MAX_CAREER_YEARS
                    For I = B9 To 78
                        For I1 = 0 To 16
                            carRecPitStats(J, I, I1) = carRecPitStats(J, I + 1, I1)
                        Next
                        Locate 7, 36
                        Color 12
                        UPP = UPP - 1
                        Print Using "####"; UPP
                    Next
                Next

            End If 'Done checking for 79

            If B9 < 80 Then
                careerPitchers$(79) = ""
                For J = 1 To MAX_CAREER_YEARS
                    For I1 = 0 To 16
                        carRecPitStats(J, 79, I1) = 0
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
Sub PrintBatterStats (printDest, destFile$, careerName$, playerIdx)

    Dim I2!, I3!, I4!
    Shared carRecBatStats(), carRecPitStats()
    Shared careerBatters$(), careerPitchers$()

    VV = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, careerName$
    Print #1, "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + carRecBatStats(Z, playerIdx, 16)
    Next Z

    Print #1, Left$(careerBatters$(playerIdx), 10);

    For V = 1 To VV

        Print #1, Tab(11); carRecBatStats(V, playerIdx, 17);
        Print #1, Tab(20); Using "###"; carRecBatStats(V, playerIdx, 14);
        Print #1, Tab(28); Using "###"; carRecBatStats(V, playerIdx, 0);
        Print #1, Tab(35); Using "###"; carRecBatStats(V, playerIdx, 2);
        Print #1, Tab(42); Using "###"; carRecBatStats(V, playerIdx, 1);
        Print #1, Tab(49); Using "##"; carRecBatStats(V, playerIdx, 4);
        Print #1, Tab(55); Using "##"; carRecBatStats(V, playerIdx, 5);
        Print #1, Tab(61); Using "##"; carRecBatStats(V, playerIdx, 6);
        Print #1, Tab(67); Using "###"; carRecBatStats(V, playerIdx, 3);
        Print #1, Tab(74); Using "###"; carRecBatStats(V, playerIdx, 7);
        Print #1, Tab(81); Using "###"; carRecBatStats(V, playerIdx, 8);
        Print #1, Tab(88); Using "###"; carRecBatStats(V, playerIdx, 9);
        Print #1, Tab(95); Using "##"; carRecBatStats(V, playerIdx, 10);
        Print #1, Tab(101); Using "##"; carRecBatStats(V, playerIdx, 11);

        I2! = 0
        I3! = 0
        I4! = 0

        IF carRecBatStats(V, playerIdx, 0) > 0 Then
            I2! = INT((carRecBatStats(V, playerIdx, 1) / carRecBatStats(V, playerIdx, 0) + .0005) * 1000)
            I3! = INT(((carRecBatStats(V, playerIdx, 1) + carRecBatStats(V, playerIdx, 4) + 2 * carRecBatStats(V, playerIdx, 5) + 3 * carRecBatStats(V, playerIdx, 6)) / carRecBatStats(V, playerIdx, 0) + .0005) * 1000)
            I4! = INT(((carRecBatStats(V, playerIdx, 1) + carRecBatStats(V, playerIdx, 7)) / (carRecBatStats(V, playerIdx, 7) + carRecBatStats(V, playerIdx, 0)) + .0005) * 1000)
        End If
        If carRecBatStats(V, playerIdx, 0) = 0 Or I2! = 0 Then 
            Print #1, Tab(107); ".000";
        End If

        If I2! > 0 And I2! < 100 Then 
            Print #1, Tab(107); ".0";: Print #1, Using "##"; I2!;
        End If
        If I2! >= 100 Then 
            Print #1, Tab(107); ".";: Print #1, Using "###"; I2!;
        End If

        If carRecBatStats(V, playerIdx, 0) = 0 Or I3! = 0 Then 
            Print #1, Tab(115); ".000";
        End If

        If I3! > 0 And I3! < 100 Then 
            Print #1, Tab(115); ".0";: Print #1, Using "##"; I3!;
        End If
        If I3! >= 100 Then 
            Print #1, Tab(115); ".";: Print #1, Using "###"; I3!;
        End If

        If carRecBatStats(V, playerIdx, 0) = 0 Or I4! = 0 Then 
            Print #1, Tab(123); ".000"
        End If

        If I4! > 0 And I4! < 100 Then 
            Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
        End If
        If I4! >= 100 Then 
            Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
        End If

    Next V

    For V = 0 To VV

        TT1 = TT1 + carRecBatStats(V, playerIdx, 14)
        TT3 = TT3 + carRecBatStats(V, playerIdx, 2)
        TT5 = TT5 + carRecBatStats(V, playerIdx, 4)
        TT7 = TT7 + carRecBatStats(V, playerIdx, 6)
        TT9 = TT9 + carRecBatStats(V, playerIdx, 7)
        TT11 = TT11 + carRecBatStats(V, playerIdx, 9)
        TT13 = TT13 + carRecBatStats(V, playerIdx, 11)
        TT2 = TT2 + carRecBatStats(V, playerIdx, 0)
        TT4 = TT4 + carRecBatStats(V, playerIdx, 1)
        TT6 = TT6 + carRecBatStats(V, playerIdx, 5)
        TT8 = TT8 + carRecBatStats(V, playerIdx, 3)
        TT10 = TT10 + carRecBatStats(V, playerIdx, 8)
        TT12 = TT12 + carRecBatStats(V, playerIdx, 10)

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
    If TT2 > 0 Then 
        I2! = Int((TT4 / TT2 + .0005) * 1000)
    End If

    If TT2 = 0 Or I2! = 0 Then 
        Print #1, Tab(107); ".000";
    End If
    If I2! > 0 And I2! < 100 Then 
        Print #1, Tab(107); ".0";: Print #1, Using "##"; I2!;
    End If
    If I2! >= 100 Then 
        Print #1, Tab(107); ".";: Print #1, Using "###"; I2!;
    End If

    I3! = 0
    I3! = Int(TT4 + TT5 + (2 * TT6) + (3 * TT7)) / ((TT2) + .0005) * 1000

    If TT2 = 0 Or I3! = 0 Then 
        Print #1, Tab(115); ".000";
    End If
    If I3! > 0 And I3! < 100 Then 
        Print #1, Tab(115); ".0";: Print #1, Using "##"; I3!;
    End If
    If I3! >= 100 Then 
        Print #1, Tab(115); ".";: Print #1, Using "###"; I3!;
    End If

    I4! = 0
    If TT9 + TT2 > 0 Then 
        I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)
    End If

    If TT2 = 0 Or I4! = 0 Then 
        Print #1, Tab(123); ".000"
    End If
    If I4! > 0 And I4! < 100 Then 
        Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
    End If
    If I4! >= 100 Then 
        Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
    End If

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
Sub PrintPitcherStats (printDest, destFile$, careerName$, playerIdx)

    Dim I2!, I3!, I4!

    Shared careerPitchers$()

    Shared carRecPitStats()

    VV = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, careerName$

    Print #1, "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"


    VV = carRecPitStats(1, playerIdx, 15) + carRecPitStats(2, playerIdx, 15) + carRecPitStats(3, playerIdx, 15) + carRecPitStats(4, playerIdx, 15) + carRecPitStats(5, playerIdx, 15) + carRecPitStats(6, playerIdx, 15) + carRecPitStats(7, playerIdx, 15) + carRecPitStats(8, playerIdx, 15) + carRecPitStats(9, playerIdx, 15) + carRecPitStats(10, playerIdx, 15) + carRecPitStats(11, playerIdx, 15) + carRecPitStats(12, playerIdx, 15) + carRecPitStats(13, playerIdx, 15) + carRecPitStats(14, playerIdx, 15) + carRecPitStats(15, playerIdx, 15)

    Print #1, Left$(careerPitchers$(playerIdx), 10);

    For V = 1 To VV

        Print #1, Tab(11); carRecPitStats(V, playerIdx, 16);
        Print #1, Tab(20); Using "##"; carRecPitStats(V, playerIdx, 6);
        Print #1, Tab(26); Using "##"; carRecPitStats(V, playerIdx, 7);
        Print #1, Tab(32); Using "##"; carRecPitStats(V, playerIdx, 12);
        Print #1, Tab(39);: Print #1, Using "###"; Int(carRecPitStats(V, playerIdx, 0) / 3);

        If carRecPitStats(V, playerIdx, 0) - (Int(carRecPitStats(V, playerIdx, 0) / 3) * 3) > 0 Then 
            Print #1, ".";: Print #1, Using "#"; carRecPitStats(V, playerIdx, 0) - (Int(carRecPitStats(V, playerIdx, 0) / 3) * 3);
        End If

        Print #1, Tab(48); Using "###"; carRecPitStats(V, playerIdx, 1);
        Print #1, Tab(55); Using "###"; carRecPitStats(V, playerIdx, 4);
        Print #1, Tab(62); Using "###"; carRecPitStats(V, playerIdx, 5)

        I2! = 0
        If carRecPitStats(V, playerIdx, 0) > 0 Then 
            I2! = Int((((carRecPitStats(V, playerIdx, 3) * 9) / (carRecPitStats(V, playerIdx, 0) / 3)) + .0005) * 100)
        End If

        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

        If carRecPitStats(V, playerIdx, 0) = 0 Or I2! = 0 Then 
            Print #1, Tab(68); " 0.00";
        End If
        If carRecPitStats(V, playerIdx, 0) > 0 And I2! > 0 And I2! <= 999 Then 
            Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
        End If
        If carRecPitStats(V, playerIdx, 0) > 0 And I2! >= 1000 Then 
            Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
        End If
        If carRecPitStats(V, playerIdx, 0) > 0 And I4! = 0 And I2! > 0 Then 
            Print #1, "00";
        End If
        If carRecPitStats(V, playerIdx, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
            Print #1, "0";: Print #1, Using "#"; I4!;
        End If
        If carRecPitStats(V, playerIdx, 0) > 0 And I4! >= 10 And I2! > 0 Then 
            Print #1, Using "##"; I4!;
        End If

        Print #1, Tab(77); Using "##"; carRecPitStats(V, playerIdx, 8);
        Print #1, Tab(83); Using "##"; carRecPitStats(V, playerIdx, 9);
        Print #1, Tab(89); Using "##"; carRecPitStats(V, playerIdx, 10);
        Print #1, Tab(95); Using "###"; carRecPitStats(V, playerIdx, 2);
        Print #1, Tab(102); Using "###"; carRecPitStats(V, playerIdx, 3);
        Print #1, Tab(110); Using "##"; carRecPitStats(V, playerIdx, 11);
        Print #1, Tab(116); Using "##"; carRecPitStats(V, playerIdx, 14);
        Print #1, Tab(121); Using "###"; carRecPitStats(V, playerIdx, 13)

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

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + carRecPitStats(Z, playerIdx, 15)
    Next Z

    For V = 0 To VV

        TT13 = TT13 + carRecPitStats(V, playerIdx, 6)
        TT15 = TT15 + carRecPitStats(V, playerIdx, 12)
        TT17 = TT17 + carRecPitStats(V, playerIdx, 1)
        TT19 = TT19 + carRecPitStats(V, playerIdx, 5)
        TT21 = TT21 + carRecPitStats(V, playerIdx, 8)
        TT23 = TT23 + carRecPitStats(V, playerIdx, 10)
        TT25 = TT25 + carRecPitStats(V, playerIdx, 3)
        TT27 = TT27 + carRecPitStats(V, playerIdx, 14)
        TT14 = TT14 + carRecPitStats(V, playerIdx, 7)
        TT16 = TT16 + carRecPitStats(V, playerIdx, 0)
        TT18 = TT18 + carRecPitStats(V, playerIdx, 4)
        TT20 = TT20
        TT22 = TT22 + carRecPitStats(V, playerIdx, 9)
        TT24 = TT24 + carRecPitStats(V, playerIdx, 2)
        TT26 = TT26 + carRecPitStats(V, playerIdx, 11)
        TT28 = TT28 + carRecPitStats(V, playerIdx, 13)

    Next V

    Print #1, "   Totals";
    Print #1, Tab(13); Using "###"; VV;
    Print #1, Tab(19); Using "###"; TT13;
    Print #1, Tab(25); Using "###"; TT14;
    Print #1, Tab(31); Using "###"; TT15;
    Print #1, Tab(39); Using "###"; Int(TT16 / 3);

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then 
        Print #1, ".";: Print #1, Using "#"; TT16 - (Int(TT16 / 3) * 3);
    End If

    Print #1, Tab(47); Using "####"; TT17;
    Print #1, Tab(55); Using "###"; TT18;
    Print #1, Tab(61); Using "####"; TT19;

    I2! = 0
    If TT16 > 0 Then 
        I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)
    End If
    I3! = Int(I2! / 100): I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then 
        Print #1, Tab(68); " 0.00";
    End If
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then 
        Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
    End If
    If TT16 > 0 And I2! >= 1000 Then 
        Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
    End If
    If TT16 > 0 And I4! = 0 And I2! > 0 Then 
        Print #1, "00";
    End If
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
        Print #1, "0";: Print #1, Using "#"; I4!;
    End If
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then 
        Print #1, Using "##"; I4!;
    End If

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
