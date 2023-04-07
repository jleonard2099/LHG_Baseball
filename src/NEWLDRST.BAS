Sub NEWLDRST ()

    _Title "Full Count Baseball - League Standings"

    Shared CLT!(), CWS!()
    Shared LLS!(), LWS!()
    Shared TLS!(), TWS!()

    For I = 1 To 40:
        TWS!(I) = 0
        TLS!(I) = 0
        LWS!(I) = 0
        LLS!(I) = 0
        CWS!(I) = 0
        CLT!(I) = 0
    Next

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "UPDATE LEAGUE STANDINGS      "
        menuChoices$(2) = "VIEW / PRINT LEAGUE STANDINGS"
        menuChoices$(3) = "RETURN TO MAIN MENU          "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 7
        Column% = 27
        Call box0(6, 26, 10, 56, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call UpdateLeagueStandings
                Case 2: Call ViewPrintLeagueStandings
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'   UpdateLeagueStandings Subroutine
'----------------------------------------
'...explanation...
Sub UpdateLeagueStandings ()

    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared RLL!(), RWW!(), LL!(), LTW!(), LTL!(), WW!()

    Shared CLT!(), CWS!()
    Shared LLS!(), LWS!()
    Shared TLS!(), TWS!()

    Shared TML$(), Z1$()

    'These are just read in and written back
    Shared H%(), I%(), V%()
    Shared statH$(), L$(), PL$(), W$()

    TMR% = 40

    Do
        Cls

        Spec$ = diskPaths$(2) + "*.TMS"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%
        If Count% = 0 Then
            'No prompt to the user???
            'Should use CSMsgBox
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

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                LG$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(2) + LG$ + ".TMS" For Input As #2

                Input #2, LGF$
                Input #2, D1$
                Input #2, D2$
                Input #2, D3$
                Input #2, D4$

                For X = 1 To 40: Input #2, Z1$(X): Next

                Close 2

                For KK = 1 To 40
                    YY = YY + 1
                    TMR% = TMR% - 1
                    Locate 7, 20
                    Color 14: Print Using "###"; TMR%

                    If Z1$(KK) <> "*" Then

                        TML$(YY) = Z1$(KK)

                        Locate 4
                        Color 15: Print "COMPUTING RECORDS . . . . . PLEASE WAIT"

                        test$ = Z1$(KK)
                        Open diskPaths$(1) + Z1$(KK) + "S" For Input As #1

                        Input #1, XX%

                        For I = 1 To 162: Input #1, H%(I): Next I
                        For I = 1 To 162: Input #1, statH$(I): Next I
                        For I = 1 To 162: Input #1, V%(I): Next I
                        For I = 1 To 162: Input #1, PL$(I): Next I
                        For I = 1 To 162: Input #1, W$(I): Next I
                        For I = 1 To 162: Input #1, L$(I): Next I
                        For I = 1 To 162: Input #1, I%(I): Next I
                        Close #1

                        For I = 1 To XX%

                            If H%(I) > V%(I) Then WW!(YY) = WW!(YY) + 1
                            If H%(I) > V%(I) And I%(I) > 9 Then EWW!(YY) = EWW!(YY) + 1
                            If H%(I) > V%(I) And PL$(I) = "A" Then RWW!(YY) = RWW!(YY) + 1
                            If H%(I) > V%(I) And PL$(I) = "H" Then HWW!(YY) = HWW!(YY) + 1
                            If V%(I) > H%(I) Then LL!(YY) = LL!(YY) + 1
                            If V%(I) > H%(I) And I%(I) > 9 Then ELL!(YY) = ELL!(YY) + 1
                            If V%(I) > H%(I) And PL$(I) = "A" Then RLL!(YY) = RLL!(YY) + 1
                            If V%(I) > H%(I) And PL$(I) = "H" Then HLL!(YY) = HLL!(YY) + 1
                            If H%(I) > V%(I) Then LWS!(YY) = LWS!(YY) + 1: LLS!(YY) = 0
                            If LWS!(YY) > TWS!(YY) Then TWS!(YY) = LWS!(YY)
                            If H%(I) < V%(I) Then LLS!(YY) = LLS!(YY) + 1: LWS!(YY) = 0
                            If LLS!(YY) > TLS!(YY) Then TLS!(YY) = LLS!(YY)

                            If H%(I) <= V%(I) Then
                                If H%(I) < V%(I) Then
                                    CLT!(YY) = CLT!(YY) + 1: CWS!(YY) = 0
                                Else
                                    CWS!(YY) = CWS!(YY) + 1: CLT!(YY) = 0
                                End If
                            End If

                        Next I

                        If XX% <= 10 Then
                            LTL!(YY) = LL!(YY): LTW!(YY) = WW!(YY)
                        Else
                            M = XX% - 9

                            For J = M To XX%
                                If H%(J) > V%(J) Then LTW!(YY) = LTW!(YY) + 1
                                If V%(J) > H%(J) Then LTL!(YY) = LTL!(YY) + 1
                            Next J
                        End If

                    End If 'Done checking for '*'

                Next KK

                Open diskPaths$(2) + LG$ + ".STD" For Output As #1
                Write #1, LGF$, D1$, D2$, D3$, D4$
                For I = 1 To 40:
                    Write #1, TML$(I), WW!(I), LL!(I), TWS!(I), TLS!(I), CWS!(I), CLT!(I), HWW!(I), HLL!(I), RWW!(I), RLL!(I), EWW!(I), ELL!(I), LTW!(I), LTL!(I)
                Next
                Close #1

            End If

        End If 'Done checking file count

    Loop Until Count% = 0 Or ExitCode% = 27 Or I$ = "N"

End Sub

'----------------------------------------
'   ViewPrintLeagueStandings Subroutine
'----------------------------------------
'...explanation...
Sub ViewPrintLeagueStandings ()

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$(), Z1$()

    Do
        Cls
        Spec$ = diskPaths$(2) + "*.STD"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%
        If Count% = 0 Then
            'Exit
            'Shouldn't we want user?
            'Use CSMsgBox
        Else
            ReDim standingsFiles$(0 To Count%)
            For X = 1 To Count%
                standingsFiles$(X) = Space$(12)
            Next
            standingsFiles$(0) = Spec$
            Call ReadFile(standingsFiles$())

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
            MAMenu standingsFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = standingsFiles$(Selection%)
                Locate 2, 33: Print targetFile$

                Do
                    Locate , 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

                If UCase$(I$) = "N" Then

                    targetFile$ = RTrim$(targetFile$)
                    LGS$ = Left$(targetFile$, Len(targetFile$) - 4)

                    Open diskPaths$(2) + LGS$ + ".STD" For Input As #1

                    Input #1, LGF$, D1$, D2$, D3$, D4$

                    For I = 1 To 40:
                        Input #1, TML$(I), WW!(I), LL!(I), TWS!(I), TLS!(I), CWS!(I), CLT!(I), HWW!(I), HLL!(I), RWW!(I), RLL!(I), EWW!(I), ELL!(I), LTW!(I), LTL!(I)
                    Next

                    Close #1

					Color 15, 0
					Cls

					Do
						Locate 15, 10
						Color 12: Print "V";: Color 7: Print "iew or ";
						Color 12: Print "P";: Color 7: Print "rint"
						A$ = GetKeyPress$
					Loop Until UCase$(A$) = "P" Or UCase$(A$) = "V"

                    If UCase$(A$) = "V" Then
                        Call ViewNewLeaders
                    Else
                        'Assumes we only have V or P
                        Call PrintNewLeaders
                    End If

                End If 'Done checking for changes

            End If 'Done checking for ESC

        End If 'Done checking file count

    Loop Until UCase$(A$) = "P" Or UCase$(A$) = "V" Or ExitCode% = 27

End Sub

'----------------------------------------
'       ViewNewLeaders Subroutine
'----------------------------------------
'...explanation...
Sub ViewNewLeaders ()

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    For NT = 1 To 4
        AA = NT * 10 - 9
        ZZ = NT * 10

        For X = AA To ZZ - 1
            For Y = X + 1 To ZZ
                If WW!(X) - LL!(X) <= WW!(Y) - LL!(Y) Then

                    If WW!(X) - LL!(X) <> WW!(Y) - LL!(Y) Or WW!(X) > WW!(Y) Or WW!(X) <= LL!(X) Then

                        If WW!(X) - LL!(X) <> WW!(Y) - LL!(Y) Or WW!(X) < WW!(Y) Or WW!(X) >= LL!(X) Then

                            Swap TML$(X), TML$(Y)
                            Swap TWS!(X), TWS!(Y)
                            Swap TLS!(X), TLS!(Y)
                            Swap CWS!(X), CWS!(Y)
                            Swap CLT!(X), CLT!(Y)
                            Swap WW!(X), WW!(Y)
                            Swap LL!(X), LL!(Y)
                            Swap EWW!(X), EWW!(Y)
                            Swap ELL!(X), ELL!(Y)
                            Swap HLL!(X), HLL!(Y)
                            Swap RLL!(X), RLL!(Y)
                            Swap HWW!(X), HWW!(Y)
                            Swap RWW!(X), RWW!(Y)
                            Swap LTL!(X), LTL!(Y)
                            Swap LTW!(X), LTW!(Y)

                        End If

                    End If

                End If

            Next Y
        Next X

        Cls
        Locate 1, 35
        Color 11: Print LGF$
        Locate 2
        Color 14: Print Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
        Locate 3
        Color 14

        If NT = 1 Then Print D1$;
        If NT = 2 Then Print D2$;
        If NT = 3 Then Print D3$;
        If NT = 4 Then Print D4$;

        Print Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"

        For I = AA To ZZ

            If TML$(I) <> "" Then

                Color 3: Print TML$(I);: Color 15: Print Tab(14); Using "###"; WW!(I) + LL!(I); Tab(18);
                Print Using "###"; WW!(I); Tab(22);
                Print Using "###"; LL!(I);

                If LL!(I) = 0 And LL!(I) + WW!(I) <> 0 Then
                    Print Tab(26); "1.000";
                Else
                    If WW!(I) = 0 Then
                        Print Tab(27); ".000";
                    Else
                        If WW!(I) + LL!(I) <> 0 Then
                            statPCT! = WW!(I) / (WW!(I) + LL!(I))
                            Print Tab(27); Using ".###"; statPCT!;
                        Else
                            If WW!(I) + LL!(I) = 0 Then Print Tab(27); ".000";
                        End If
                    End If
                End If

                If ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2 = 0 Then Print Tab(32); " ---";
                If ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2 <> 0 Then Print Tab(32); Using "##.#"; ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2;

                Print Tab(44); Using "##"; TWS!(I); Tab(47); TLS!(I); Tab(51); CWS!(I); Tab(54); CLT!(I);
                Print Tab(57); Using "##"; HWW!(I);: Print Tab(60); Using "##"; HLL!(I);
                Print Tab(63); Using "##"; RWW!(I);: Print Tab(66); Using "##"; RLL!(I);
                Print Tab(69); Using "##"; EWW!(I);: Print Tab(72); Using "##"; ELL!(I);
                Print Tab(75); Using "##"; LTW!(I);: Print Tab(78); Using "##"; LTL!(I)

            End If

        Next

        Locate 4, 38
        Print Using "###"; 163 - WW!(NT * 10 - 9) - LL!(NT * 10 - 8)
        Locate 23
        Color 14: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

    Next NT

End Sub

'----------------------------------------
'       PrintNewLeaders Subroutine
'----------------------------------------
'...explanation...
Sub PrintNewLeaders ()

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    MJ = 0

    LPrint Tab(35); LGF$

    For NT = 1 To 4
        AA = NT * 10 - 9
        ZZ = NT * 10

        For X = AA To ZZ - 1
            For Y = X + 1 To ZZ
                If WW!(X) - LL!(X) <= WW!(Y) - LL!(Y) Then

                    If WW!(X) - LL!(X) <> WW!(Y) - LL!(Y) Or WW!(X) > WW!(Y) Or WW!(X) <= LL!(X) Then

                        If WW!(X) - LL!(X) <> WW!(Y) - LL!(Y) Or WW!(X) < WW!(Y) Or WW!(X) >= LL!(X) Then

                            Swap TML$(X), TML$(Y)
                            Swap TWS!(X), TWS!(Y)
                            Swap TLS!(X), TLS!(Y)
                            Swap CWS!(X), CWS!(Y)
                            Swap CLT!(X), CLT!(Y)
                            Swap WW!(X), WW!(Y)
                            Swap LL!(X), LL!(Y)
                            Swap EWW!(X), EWW!(Y)
                            Swap ELL!(X), ELL!(Y)
                            Swap HLL!(X), HLL!(Y)
                            Swap RLL!(X), RLL!(Y)
                            Swap HWW!(X), HWW!(Y)
                            Swap RWW!(X), RWW!(Y)
                            Swap LTL!(X), LTL!(Y)
                            Swap LTW!(X), LTW!(Y)

                        End If

                    End If

                End If

            Next Y
        Next X

        LPrint Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"

        If NT = 1 Then LPrint D1$;
        If NT = 2 Then LPrint D2$;
        If NT = 3 Then LPrint D3$;
        If NT = 4 Then LPrint D4$;

        LPrint Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"

        For I = AA To ZZ

            If TML$(I) <> "" Then

                Color 3: LPrint TML$(I);: Color 15: LPrint Tab(14); Using "###"; WW!(I) + LL!(I); Tab(18);
                LPrint Using "###"; WW!(I); Tab(22);
                LPrint Using "###"; LL!(I);

                If LL!(I) = 0 And LL!(I) + WW!(I) <> 0 Then
                    LPrint Tab(26); "1.000";
                Else
                    If WW!(I) = 0 Then
                        LPrint Tab(27); ".000";
                    Else
                        If WW!(I) + LL!(I) <> 0 Then
                            statPCT! = WW!(I) / (WW!(I) + LL!(I))
                            LPrint Tab(27); Using ".###"; statPCT!;
                        Else
                            If WW!(I) + LL!(I) = 0 Then LPrint Tab(27); ".000";
                        End If
                    End If
                End If

                If ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2 = 0 Then LPrint Tab(32); " ---";
                If ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2 <> 0 Then LPrint Tab(32); Using "##.#"; ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2;
                If I = NT * 10 - 9 Then LPrint Tab(38); Using "###"; 163 - WW!(NT * 10 - 9) - LL!(NT * 10 - 8);
                LPrint Tab(44); Using "##"; TWS!(I); Tab(47); TLS!(I); Tab(51); CWS!(I); Tab(54); CLT!(I);
                LPrint Tab(57); Using "##"; HWW!(I);: LPrint Tab(60); Using "##"; HLL!(I);
                LPrint Tab(63); Using "##"; RWW!(I);: LPrint Tab(66); Using "##"; RLL!(I);
                LPrint Tab(69); Using "##"; EWW!(I);: LPrint Tab(72); Using "##"; ELL!(I);
                LPrint Tab(75); Using "##"; LTW!(I);: LPrint Tab(78); Using "##"; LTL!(I)

            End If

        Next
        LPrint
        LPrint
    Next NT

    LPrint Chr$(12)

End Sub
