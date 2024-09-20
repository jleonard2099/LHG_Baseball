'----------------------------------------
'           CARVIEW Subroutine
'----------------------------------------
'This routine loads in an existing career
'file (.CAR) and gives the user options
'for viewing / printing the results
Sub CARVIEW ()

    _Title "Full Count Baseball - View Careers"

    careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadCareerFile (careerFile$)
        
        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                    If userFile$ <> "" Then
                        Call CareerToPrinter(0, userFile$, careerFile$)
                    End If

                Case "P"
                    Call CareerToPrinter(1, "REPORT", careerFile$)

                Case "S"
                    Call CareerToScreen(careerFile$)

            End Select
            
        End If

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       CareerToPrinter Subroutine
'----------------------------------------
'Formats results of Career stats and sends
'to the user's default printer
Sub CareerToPrinter (printDest, destFile$, careerFile$)

    Dim I2!, I3!, I4!
    Shared BRS%(), PRS%()
    Shared careerBatters$(), careerPitchers$()

    PTN = 0
    VV = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, careerFile$
    Print #1, "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"

    For I = 0 To 99

        If careerBatters$(I) <> "" And careerBatters$(I) <> "XXX" Then

            For Z = 1 To MAX_CAREER_YEARS
                VV = VV + BRS%(Z, I, 16)
            Next z

            If 55 - PTN < VV Then
                Print #1, Chr$(12)
                Print #1, careerFile$
                Print #1, "BATTER     YEAR      G      AB      R      H    2B    3B    HR    RBI     BB     SO     SB    CS     E     AVG     SLG      OB"
                PTN = 0

            End If

            Print #1, Left$(careerBatters$(I), 10);

            For V = 1 To VV

                PTN = PTN + 1

                'Year
                Print #1, Tab(11); BRS%(V, I, 17);
                'Games
                Print #1, Tab(20); Using "###"; BRS%(V, I, 14);
                'At Bats
                Print #1, Tab(28); Using "###"; BRS%(V, I, 0);
                'R
                Print #1, Tab(35); Using "###"; BRS%(V, I, 2);
                'H
                Print #1, Tab(42); Using "###"; BRS%(V, I, 1);
                '2B
                Print #1, Tab(49); Using "##"; BRS%(V, I, 4);
                '3B
                Print #1, Tab(55); Using "##"; BRS%(V, I, 5);
                'HR
                Print #1, Tab(61); Using "##"; BRS%(V, I, 6);
                'RBI
                Print #1, Tab(67); Using "###"; BRS%(V, I, 3);
                'BB
                Print #1, Tab(74); Using "###"; BRS%(V, I, 7);
                'SO
                Print #1, Tab(81); Using "###"; BRS%(V, I, 8);
                'SB
                Print #1, Tab(88); Using "###"; BRS%(V, I, 9);
                'CS
                Print #1, Tab(95); Using "##"; BRS%(V, I, 10);
                'E
                Print #1, Tab(101); Using "##"; BRS%(V, I, 11);

                I2! = 0
                I3! = 0
                I4! = 0

                'Avg
                If BRS%(V, I, 0) > 0 Then 
                    I2! = Int((BRS%(V, I, 1) / BRS%(V, I, 0) + .0005) * 1000)
                    I3! = Int(((BRS%(V, I, 1) + BRS%(V, I, 4) + 2 * BRS%(V, I, 5) + 3 * BRS%(V, I, 6)) / BRS%(V, I, 0) + .0005) * 1000)
                    I4! = Int(((BRS%(V, I, 1) + BRS%(V, I, 7)) / (BRS%(V, I, 7) + BRS%(V, I, 0)) + .0005) * 1000)
                End If
                If BRS%(V, I, 0) = 0 Or I2! = 0 Then 
                    Print #1, Tab(107); ".000";
                End If

                If I2! > 0 And I2! < 100 Then 
                    Print #1, Tab(107); ".0";: Print #1, Using "##"; I2!;
                End If
                If I2! >= 100 Then 
                    Print #1, Tab(107); ".";: Print #1, Using "###"; I2!;
                End If

                'SLG
                If BRS%(V, I, 0) = 0 Or I3! = O Then 
                    Print #1, Tab(115); ".000";
                End If

                If I3! > 0 And I3! < 100 Then 
                    Print #1, Tab(115); ".0";: Print #1, Using "##"; I3!;
                End If
                If I3! >= 100 Then 
                    Print #1, Tab(115); ".";: Print #1, Using "###"; I3!;
                End If

                'OB
                If BRS%(V, I, 0) = 0 Or I4! = 0 Then 
                    Print #1, Tab(123); ".000"
                End If

                If I4! > 0 And I4! < 100 Then 
                    Print #1, Tab(123); ".0";: Print #1, Using "##"; I4!
                End If
                If I4! >= 100 Then 
                    Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
                End If

            Next V

            VV = 0 

            For Z = 1 To MAX_CAREER_YEARS
                VV = VV + BRS%(Z, I, 16)
            Next z

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
            If TT2 = 0 Or I3! = O Then 
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
            ENd if
            If I4! > 0 And I4! < 100 Then 
                Print #1, Tab(123); ".0";: Print #1, Using "##"; I4!
            End If
            If I4! >= 100 Then 
                Print #1, Tab(123); ".";: Print #1, Using "###"; I4!
            End If

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
            PTN = PTN + 2
            Print #1,

        End If

    Next I

    Print #1, Chr$(12)

    PTN = 0
    Print #1, careerFile$
    Print #1, "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"

    For I = 0 To 79

        If careerPitchers$(I) <> "" And careerPitchers$(I) <> "XXX" Then

            VV = 0 

            For Z = 1 To MAX_CAREER_YEARS
                VV = VV + PRS%(Z, I, 15)
            Next z

            If 55 - PTN < VV Then
                Print #1, Chr$(12)
                Print #1, careerFile$
                Print #1, "PITCHER    YEAR     W     L     S      IP        H     BB     SO     ERA     G    GS    CG    RUN     ER    SHO    HR    DP"
                PTN = 0
            End If

            Print #1, Left$(careerPitchers$(I), 10);

            For V = 1 To VV

                PTN = PTN + 1

                'Year
                Print #1, Tab(11); PRS%(V, I, 16);
                'Wins
                Print #1, Tab(20); Using "##"; PRS%(V, I, 6);
                'Losses
                Print #1, Tab(26); Using "##"; PRS%(V, I, 7);
                'S
                Print #1, Tab(32); Using "##"; PRS%(V, I, 12);

                'Innings Pitched
                Print #1, Tab(39);: Print #1, Using "###"; Int(PRS%(V, I, 0) / 3);

                If PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3) > 0 Then 
                    Print #1, ".";: Print #1, Using "#"; PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3);
                End If

                'H
                Print #1, Tab(48); Using "###"; PRS%(V, I, 1);
                'BB
                Print #1, Tab(55); Using "###"; PRS%(V, I, 4);
                'SO
                Print #1, Tab(62); Using "###"; PRS%(V, I, 5);

                I2! = 0
                If PRS%(V, I, 0) > 0 Then 
                    I2! = Int((((PRS%(V, I, 3) * 9) / (PRS%(V, I, 0) / 3)) + .0005) * 100)
                End If

                I3! = Int(I2! / 100)
                I4! = I2! - (I3! * 100)
    
                'ERA
                If PRS%(V, I, 0) = 0 Or I2! = 0 Then 
                    Print #1, Tab(68); " 0.00";
                End If
                If PRS%(V, I, 0) > 0 And I2! > 0 And I2! <= 999 Then 
                    Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
                End If
                If PRS%(V, I, 0) > 0 And I2! >= 1000 Then 
                    Print #1, Tab(68); Using "##"; I3!;: Print #1, ".";
                End If
                If PRS%(V, I, 0) > 0 And I4! = 0 And I2! > 0 Then 
                    Print #1, "00";
                End If
                If PRS%(V, I, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
                    Print #1, "0";: Print #1, Using "#"; I4!;
                End If
                If PRS%(V, I, 0) > 0 And I4! >= 10 And I2! > 0 Then 
                    Print #1, Using "##"; I4!;
                End If

                'G
                Print #1, Tab(77); Using "##"; PRS%(V, I, 8);
                'GS
                Print #1, Tab(83); Using "##"; PRS%(V, I, 9);
                'CG
                Print #1, Tab(89); Using "##"; PRS%(V, I, 10);
                'Run
                Print #1, Tab(95); Using "###"; PRS%(V, I, 2);
                'ER
                Print #1, Tab(102); Using "###"; PRS%(V, I, 3);
                'SHO
                Print #1, Tab(110); Using "##"; PRS%(V, I, 11);
                'HR
                Print #1, Tab(116); Using "##"; PRS%(V, I, 14);
                'DP
                Print #1, Tab(121); Using "###"; PRS%(V, I, 13)

            Next V

        End If 'Done checking for terminal characters

        VV = 0 

        For Z = 1 To MAX_CAREER_YEARS
            VV = VV + PRS%(Z, I, 15)
        Next z

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

        Print #1, "   Totals";
        Print #1, Tab(13); Using "###"; VV;
        Print #1, Tab(19); Using "###"; TT13;
        Print #1, Tab(25); Using "###"; TT14;
        Print #1, Tab(31); Using "###"; TT15;
        Print #1, Tab(39); Using "###"; Int(TT16 / 3);

        If TT16 - (Int(TT16 / 3) * 3) > 0 Then 
            Print #1, ".";: Print #1, Using "#"; TT16 - (Int(TT16 / 3) * 3);
        End if

        Print #1, Tab(47); Using "####"; TT17;
        Print #1, Tab(55); Using "###"; TT18;
        Print #1, Tab(61); Using "####"; TT19;

        I2! = 0

        If TT16 > 0 Then 
            I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)
        End If

        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

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
        PTN = PTN + 2
        Print #1,

    Next I

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
'        CareerToScreen Subroutine
'----------------------------------------
'This routine gives the user different 
'categories of stats related to the roster.
'It then calls a supplemental routine for 
'each  that will format those stats and 
'send them to the display screen
Sub CareerToScreen (careerFile$)

    Do

        Color 15, 0
        Cls

        Color 7: Print "VIEW ";
        Color 12: Print "B";
        Color 7: Print "ATTERS"

        Color 7: Print "VIEW ";
        Color 12: Print "P";
        Color 7: Print "ITCHERS"

        Color 12: Print "R";
        Color 7: Print "OSTER IN SEQUENCE"

        Color 12: Print "E";
        Color 7: Print "XIT TO MAIN MENU"

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
'This subroutine will display batter stats
'from the team's career file
Sub ViewLeaders_Batters (careerFile$)

    Dim I2!, I3!, I4!

    Shared careerBatters$()
    Shared BRS%()

    Cls
    
    Color 12: Print careerFile$

    For I = 0 To 19
        Locate I + 2
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerBatters$(I), 12)
    Next

    For I = 20 To 39
        Locate I - 18, 21
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerBatters$(I), 12)
    Next

    For I = 40 To 59
        Locate I - 38, 41
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerBatters$(I), 12)
    Next

    For I = 60 To 79
        Locate I - 58, 61
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerBatters$(I), 12)
    Next

    Print
    Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9

    Cls

    Color 11
    Locate 1: Print careerFile$
    Color 12
    Locate 2: Print "BATTER     YEAR   G   AB   R   H 2B 3B HR RBI  BB  SO  SB CS  E  AVG  SLG   OB"
    Color 15
    Locate 3: Print Left$(careerBatters$(B9), 13);

    VV = 0 

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + BRS%(Z, B9, 16)
    Next z

    For V = 1 To VV

        Color 7
        
        'Year
        Print Tab(11); BRS%(V, B9, 17);
        'G
        Print Tab(17); Using "###"; BRS%(V, B9, 14);
        'AB
        Print Tab(22); Using "###"; BRS%(V, B9, 0);
        'R
        Print Tab(26); Using "###"; BRS%(V, B9, 2);
        'H
        Print Tab(30); Using "###"; BRS%(V, B9, 1);
        '2B
        Print Tab(34); Using "##"; BRS%(V, B9, 4);
        '3B
        Print Tab(37); Using "##"; BRS%(V, B9, 5);
        'HR
        Print Tab(40); Using "##"; BRS%(V, B9, 6);
        'RBI
        Print Tab(43); Using "###"; BRS%(V, B9, 3);
        'BB
        Print Tab(47); Using "###"; BRS%(V, B9, 7);
        'SO
        Print Tab(51); Using "###"; BRS%(V, B9, 8);
        'SB
        Print Tab(55); Using "###"; BRS%(V, B9, 9);
        'CS
        Print Tab(59); Using "##"; BRS%(V, B9, 10);
        'E
        Print Tab(62); Using "##"; BRS%(V, B9, 11);

        I2! = 0
        I3! = 0
        I4! = 0

        'AVG
        If BRS%(V, B9, 0) > 0 Then 
            I2! = Int((BRS%(V, B9, 1) / BRS%(V, B9, 0) + .0005) * 1000)
            I3! = Int(((BRS%(V, B9, 1) + BRS%(V, B9, 4) + 2 * BRS%(V, B9, 5) + 3 * BRS%(V, B9, 6)) / BRS%(V, B9, 0) + .0005) * 1000)
            I4! = Int(((BRS%(V, B9, 1) + BRS%(V, B9, 7)) / (BRS%(V, B9, 7) + BRS%(V, B9, 0)) + .0005) * 1000)
        End If
        If BRS%(V, B9, 0) = 0 Or I2! = 0 Then 
            Locate , 65: Print ".000";
        End If

        If I2! > 0 And I2! < 100 Then 
            Locate , 65: Print ".0";: Print Using "##"; I2!;
        End If
        If I2! >= 100 Then 
            Locate , 65: Print ".";: Print Using "###"; I2!;
        End If

        'SLG
        If BRS%(V, B9, 0) = 0 Or I3! = O Then 
            Locate , 70: Print ".000";
        End If

        If I3! > 0 And I3! < 100 Then 
            Locate , 70: Print ".0";: Print Using "##"; I3!;
        End If
        If I3! >= 100 Then 
            Locate , 70: Print ".";: Print Using "###"; I3!;
        End If

        'OB
        If BRS%(V, B9, 0) = 0 Or I4! = 0 Then 
            Locate , 75: Print ".000"
        End If

        If I4! > 0 And I4! < 100 Then 
            Locate , 75: Print ".";: Print Using "###"; I4!
        End If
        If I4! >= 100 Then 
            Locate , 75: Print ".";: Print Using "###"; I4!
        End If

    Next V

    VV = 0 

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + BRS%(Z, B9, 16)
    Next z

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

    If TT2 = 0 Or I3! = O Then 
        Locate , 70: Print ".000"
    End If
    If I3! > 0 And I3! < 100 Then 
        Locate , 70: Print ".0";: Print Using "##"; I3!
    End If
    If I3! >= 100 Then 
        Locate , 70: Print ".";: Print Using "###"; I3!
    End If

    Print "TOTALS";

    Print Tab(13); Using "##"; VV;
    Print Tab(21); Using "####"; TT2;
    Print Tab(29); Using "####"; TT4;
    Print Tab(36); Using "###"; TT6;
    Print Tab(42); Using "####"; TT8;
    Print Tab(50); Using "####"; TT10;
    Print Tab(58); Using "###"; TT12;

    I2! = 0
    If TT2 > 0 Then 
        I2! = Int((TT4 / TT2 + .0005) * 1000)
    End If

    If TT2 = 0 Or I2! = 0 Then 
        Locate , 65: Print ".000";
    End If
    If I2! > 0 And I2! < 100 Then 
        Locate , 65: Print ".0";: Print Using "##"; I2!;
    End If
    If I2! >= 100 Then 
        Locate , 65: Print ".";: Print Using "###"; I2!;
    End If

    I4! = 0
    If TT9 + TT2 > 0 Then 
        I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)
    End If

    If TT2 = 0 Or I4! = 0 Then 
        Locate , 75: Print ".000"
    End If
    If I4! > 0 And I4! < 100 Then 
        Locate , 75: Print ".";: Print Using "###"; I4!
    End If
    If I4! >= 100 Then 
        Locate , 75: Print ".";: Print Using "###"; I4!
    End If

    Print
    Print
    Color 15: Input "HIT ENTER TO CONTINUE"; DUMMY$

End Sub

'----------------------------------------
'     ViewLeaders_Pitchers Subroutine
'----------------------------------------
'This subroutine will display pitcher stats
'from the team's career file
Sub ViewLeaders_Pitchers (careerFile$)

    Dim I2!, I3!, I4!
    Shared careerPitchers$()
    Shared PRS%()

    Cls
    Color 12: Print careerFile$

    For I = 0 To 19
        Locate I + 2
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerPitchers$(I), 12)
    Next

    For I = 20 To 39
        Locate I - 18, 21
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerPitchers$(I), 12)
    Next

    For I = 40 To 59
        Locate I - 38, 41
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerPitchers$(I), 12)
    Next

    For I = 60 To 79
        Locate I - 58, 61
        Color 11: Print Using "##"; I;
        Print ".";
        Color 7: Print Left$(careerPitchers$(I), 12)
    Next

    Print

    Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9

    Cls

    Color 11
    Locate 1: Print careerFile$
    Color 12
    Locate 2: Print "PITCHER    YEAR  W  L  S   IP     H  BB  SO  ERA  G GS CG RUN  ER SHO HR DP"
    Color 15
    Locate 3: Print Left$(careerPitchers$(B9), 13);

    VV = 0 

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + PRS%(Z, B9, 15)
    Next z

    For V = 1 To VV

        Color 7
        
        'Year
        Print Tab(11); PRS%(V, B9, 16);
        'W
        Print Tab(17); Using "##"; PRS%(V, B9, 6);
        'L
        Print Tab(20); Using "##"; PRS%(V, B9, 7);
        'S
        Print Tab(23); Using "##"; PRS%(V, B9, 12);
        'IP
        Locate , 27: Print Using "###"; Int(PRS%(V, B9, 0) / 3);

        If PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3) > 0 Then 
            Print ".";: Print Using "#"; PRS%(V, B9, 0) - (Int(PRS%(V, B9, 0) / 3) * 3);
        End If

        'H
        Print Tab(33); Using "###"; PRS%(V, B9, 1);
        'BB
        Print Tab(37); Using "###"; PRS%(V, B9, 4);
        'SO
        Print Tab(41); Using "###"; PRS%(V, B9, 5);

        'ERA
        I2! = 0
        If PRS%(V, B9, 0) > 0 Then 
            I2! = Int((((PRS%(V, B9, 3) * 9) / (PRS%(V, B9, 0) / 3)) + .0005) * 100)
        End If

        I3! = Int(I2! / 100)
        I4! = I2! - (I3! * 100)

        If PRS%(V, B9, 0) = 0 Or I2! = 0 Then 
            Locate , 44: Print " 0.00";
        End If
        If PRS%(V, B9, 0) > 0 And I2! > 0 And I2! <= 999 Then 
            Locate , 44: Print Using "##"; I3!;: Print ".";
        End If
        If PRS%(V, B9, 0) > 0 And I2! >= 1000 Then 
            Locate , 44: Print Using "##"; I3!;: Print ".";
        End If

        If PRS%(V, B9, 0) > 0 And I4! = 0 And I2! > 0 Then 
            Print "00";
        End If
        If PRS%(V, B9, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
            Print "0";: Print Using "#"; I4!;
        End If
        If PRS%(V, B9, 0) > 0 And I4! >= 10 And I2! > 0 Then 
            Print Using "##"; I4!;
        End If

        'G
        Print Tab(50); Using "##"; PRS%(V, B9, 8);
        'GS
        Print Tab(53); Using "##"; PRS%(V, B9, 9);
        'CG
        Print Tab(56); Using "##"; PRS%(V, B9, 10);
        'Run
        Print Tab(59); Using "###"; PRS%(V, B9, 2);
        'ER
        Print Tab(63); Using "###"; PRS%(V, B9, 3);
        'SHO
        Print Tab(68); Using "##"; PRS%(V, B9, 11);
        'HR
        Print Tab(71); Using "##"; PRS%(V, B9, 14);
        'DP
        Print Tab(73); Using "###"; PRS%(V, B9, 13)

    Next V

    Print
    Color 3

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

    VV = 0 

    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + PRS%(Z, B9, 15)
    Next z

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

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then 
        Print ".";: Print Using "#"; TT16 - (Int(TT16 / 3) * 3);
    End If

    Print Tab(37); Using "###"; TT18;

    I2! = 0
    If TT16 > 0 Then I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)

    I3! = Int(I2! / 100)
    I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then 
        Locate , 44: Print " 0.00";
    End If
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then 
        Locate , 44: Print Using "##"; I3!;: Print ".";
    End If
    If TT16 > 0 And I2! >= 1000 Then 
        Locate , 44: Print Using "##"; I3!;: Print ".";
    End If
    If TT16 > 0 And I4! = 0 And I2! > 0 Then 
        Print "00";
    End If
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
        Print "0";: Print Using "#"; I4!;
    End If
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then 
        Print Using "##"; I4!;
    End If

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
'This subroutine will display players from 
'the team's career file, in sequence
Sub ViewLeaders_Sequence (careerFile$)

    Dim I2!, I3!, I4!
    Shared BRS%(), PRS%()
    Shared careerBatters$(), careerPitchers$()

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

            If careerBatters$(I) <> "" And careerBatters$(I) <> "XXX" Then

                Color 15
                Locate 3: Print Left$(careerBatters$(I), 10);

                VV = 0
                
                For Z = 1 To MAX_CAREER_YEARS
                    VV = VV + BRS%(Z, I, 16)
                Next Z

                For V = 1 To VV

                    Color 7
                    'Year
                    Print Tab(11); BRS%(V, I, 17);
                    'G
                    Print Tab(17); Using "###"; BRS%(V, I, 14);
                    'AB
                    Print Tab(22); Using "###"; BRS%(V, I, 0);
                    'R
                    Print Tab(26); Using "###"; BRS%(V, I, 2);
                    'H
                    Print Tab(30); Using "###"; BRS%(V, I, 1);
                    '2B
                    Print Tab(34); Using "##"; BRS%(V, I, 4);
                    '3B
                    Print Tab(37); Using "##"; BRS%(V, I, 5);
                    'HR
                    Print Tab(40); Using "##"; BRS%(V, I, 6);
                    'RBI
                    Print Tab(43); Using "###"; BRS%(V, I, 3);
                    'BB
                    Print Tab(47); Using "###"; BRS%(V, I, 7);
                    'SO
                    Print Tab(51); Using "###"; BRS%(V, I, 8);
                    'SB
                    Print Tab(55); Using "###"; BRS%(V, I, 9);
                    'CS
                    Print Tab(59); Using "##"; BRS%(V, I, 10);
                    'E
                    Print Tab(62); Using "##"; BRS%(V, I, 11);

                    I2! = 0
                    I3! = 0
                    I4! = 0

                    'Avg
                    If BRS%(V, I, 0) > 0 Then 
                        I2! = Int((BRS%(V, I, 1) / BRS%(V, I, 0) + .0005) * 1000)
                        I3! = Int(((BRS%(V, I, 1) + BRS%(V, I, 4) + 2 * BRS%(V, I, 5) + 3 * BRS%(V, I, 6)) / BRS%(V, I, 0) + .0005) * 1000)
                        I4! = Int(((BRS%(V, I, 1) + BRS%(V, I, 7)) / (BRS%(V, I, 7) + BRS%(V, I, 0)) + .0005) * 1000)
                    End if

                    If I2! = 1000 Then
                        Locate , 65: Print "1.000";
                    Else
                        If BRS%(V, I, 0) = 0 Or I2! = 0 Then
                            Locate , 65: Print ".000 ";
                        End If

                        If I2! > 0 And I2! < 100 Then 
                            Locate , 65: Print ".0";: Print Using "## "; I2!;
                        End If
                        If I2! >= 100 Then 
                            Locate , 65: Print Using ".### "; I2! / 1000;
                        End If
                    End If

                    'SLG
                    If I3! = 1000 Then
                        Locate , 70: Print "1.000";
                    Else
                        If BRS%(V, I, 0) = 0 Or I3! = O Then 
                            Locate , 70: Print ".000 ";
                        End If
                        If I3! > 0 And I3! < 100 Then 
                            Locate , 70: Print ".0";: Print Using "## "; I3!;
                        End If
                        If I3! >= 100 Then 
                            Locate , 70: Print Using ".### "; I3! / 1000;
                        End If
                    End If

                    'OB
                    If I4! = 1000 Then
                        Locate , 76: Print "1.000"
                    Else
                        If BRS%(V, I, 0) = 0 Or I4! = 0 Then 
                            Locate , 76: Print ".000 "
                        End If
                        If I4! > 0 And I4! < 100 Then 
                            Locate , 76: Print ".0";: Print Using "## "; I4!
                        End If
                        If I4! >= 100 Then 
                            Locate , 76: Print Using ".### "; I4! / 1000
                        End If
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

                If careerPitchers$(I) <> "" And careerPitchers$(I) <> "XXX" Then

                    Color 15
                    Locate 3: Print Left$(careerPitchers$(I), 10);

                    VV = 0
                    
                    For Z = 1 To MAX_CAREER_YEARS
                        VV = VV + PRS%(Z, I, 15)
                    Next Z

                    For V = 1 To VV

                        Color 7
                        'Year
                        Print Tab(11); PRS%(V, I, 16);
                        'W
                        Print Tab(17); Using "##"; PRS%(V, I, 6);
                        'L
                        Print Tab(20); Using "##"; PRS%(V, I, 7);
                        'S
                        Print Tab(23); Using "##"; PRS%(V, I, 12);
                        'IP
                        Locate , 27: Print Using "###"; Int(PRS%(V, I, 0) / 3);

                        If PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3) > 0 Then 
                            Print ".";: Print Using "#"; PRS%(V, I, 0) - (Int(PRS%(V, I, 0) / 3) * 3);
                        End If

                        'H
                        Print Tab(33); Using "###"; PRS%(V, I, 1);
                        'BB
                        Print Tab(37); Using "###"; PRS%(V, I, 4);
                        'SO
                        Print Tab(41); Using "###"; PRS%(V, I, 5);

                        'ERA
                        I2! = 0
                        If PRS%(V, I, 0) > 0 Then 
                            I2! = Int((((PRS%(V, I, 3) * 9) / (PRS%(V, I, 0) / 3)) + .0005) * 100)
                        End If

                        I3! = Int(I2! / 100)
                        I4! = I2! - (I3! * 100)

                        If PRS%(V, I, 0) = 0 Or I2! = 0 Then 
                            Locate , 44: Print " 0.00";
                        End If
                        If PRS%(V, I, 0) > 0 And I2! > 0 And I2! <= 999 Then 
                            Locate , 44: Print Using "##"; I3!;: Print ".";
                        End If
                        If PRS%(V, I, 0) > 0 And I2! >= 1000 Then 
                            Locate , 44: Print Using "##"; I3!;: Print ".";
                        End If
                        If PRS%(V, I, 0) > 0 And I4! = 0 And I2! > 0 Then 
                            Print "00";
                        End If
                        If PRS%(V, I, 0) > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
                            Print "0";: Print Using "#"; I4!;
                        End If
                        If PRS%(V, I, 0) > 0 And I4! >= 10 And I2! > 0 Then 
                            Print Using "##"; I4!;
                        End If

                        'G
                        Print Tab(50); Using "##"; PRS%(V, I, 8);
                        'GS
                        Print Tab(53); Using "##"; PRS%(V, I, 9);
                        'CG
                        Print Tab(56); Using "##"; PRS%(V, I, 10);
                        'Run
                        Print Tab(59); Using "###"; PRS%(V, I, 2);
                        'ER
                        Print Tab(63); Using "###"; PRS%(V, I, 3);
                        'SHO
                        Print Tab(68); Using "##"; PRS%(V, I, 11);
                        'HR
                        Print Tab(71); Using "##"; PRS%(V, I, 14);
                        'DP
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

                Call MenuBox (6, 8, 61, 1, 15, 1)

                Do
                    Locate 7
                    U$ = "DISPLAY STATS AGAIN? (Y/N)"
                    Locate , (80 - Len(U$)) / 2
                    Print U$
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"


            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       BatterTotals Subroutine
'----------------------------------------
'This subroutine will calculate the career
'totals for batters
Sub BatterTotals ()

    Dim I3!, I4!
    Shared BRS%()

    VV = 0
    
    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + BRS%(Z, I, 16)
    Next Z

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

    If TT2 = 0 Or I3! = O Then 
        Locate , 70: Print ".000"
    End if
    If I3! > 0 And I3! < 100 Then 
        Locate , 70: Print ".0";: Print Using "##"; I3!
    End If
    If I3! >= 100 Then 
        Locate , 70: Print ".";: Print Using "###"; I3!
    End If

    Print "TOTALS";

    Print Tab(14); Using "##"; VV;
    Print Tab(21); Using "####"; TT2;
    Print Tab(29); Using "####"; TT4;
    Print Tab(36); Using "###"; TT6;
    Print Tab(42); Using "####"; TT8;
    Print Tab(50); Using "####"; TT10;
    Print Tab(58); Using "###"; TT12;

    I2 = 0

    If TT2 > 0 Then 
        I2 = Int((TT4 / TT2 + .0005) * 1000)
    End If
    If TT2 = 0 Or I2 = 0 Then 
        Locate , 65: Print ".000";
    End If
    If I2 > 0 And I2 < 100 Then 
        Locate , 65: Print ".0";: Print Using "##"; I2;
    End If
    If I2 >= 100 Then 
        Locate , 65: Print ".";: Print Using "###"; I2;
    End if

    I4! = 0

    If TT9 + TT2 > 0 Then 
        I4! = Int(((TT4 + TT9) / (TT9 + TT2) + .0005) * 1000)
    End If
    If TT2 = 0 Or I4! = 0 Then 
        Locate , 75: Print ".000"
    End If
    If I4! > 0 And I4! < 100 Then 
        Locate , 75: Print ".0";: Print Using "##"; I4!
    End If
    If I4! >= 100 Then Locate , 75: Print ".";: Print Using "###"; I4!

End Sub

'----------------------------------------
'       PitcherTotals Subroutine
'----------------------------------------
'This subroutine will calculate the career
'totals for pitchers
Sub PitcherTotals ()

    Shared PRS%()

    Dim I2!, I3!, I4!

    VV = 0
    
    For Z = 1 To MAX_CAREER_YEARS
        VV = VV + PRS%(Z, I, 15)
    Next Z

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

    If TT16 - (Int(TT16 / 3) * 3) > 0 Then 
        Print ".";: Print Using "#"; TT16 - (Int(TT16 / 3) * 3);
    End if

    Print Tab(37); Using "###"; TT18;

    I2! = 0
    If TT16 > 0 Then 
        I2! = Int((((TT25 * 9) / (TT16 / 3)) + .0005) * 100)
    End If

    I3! = Int(I2! / 100)
    I4! = I2! - (I3! * 100)

    If TT16 = 0 Or I2! = 0 Then 
        Locate , 44: Print " 0.00";
    End If
    If TT16 > 0 And I2! > 0 And I2! <= 999 Then 
        Locate , 44: Print Using "##"; I3!;: Print ".";
    End If
    If TT16 > 0 And I2! >= 1000 Then 
        Locate , 44: Print Using "##"; I3!;: Print ".";
    End If
    If TT16 > 0 And I4! = 0 And I2! > 0 Then 
        Print "00";
    End if
    If TT16 > 0 And I4! > 0 And I4! < 10 And I2! > 0 Then 
        Print "0";: Print Using "#"; I4!;
    End If
    If TT16 > 0 And I4! >= 10 And I2! > 0 Then 
        Print Using "##"; I4!;
    End If

    Print Tab(51); Using "####"; TT22;
    Print Tab(59); Using "###"; TT24;
    Print Tab(66); Using "####"; TT26;
    Print Tab(73); Using "###"; TT28

End Sub
