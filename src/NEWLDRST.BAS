'----------------------------------------
'           NEWLDRST Subroutine
'----------------------------------------
'This subroutine presents the user options
'for navigating the "Expanded" standings 
Sub ExpandedStandings ()

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
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 27

        Call box0(7, 26, 11, 56, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call UpdateLeagueStandings

                Case 2:
                    Call NewStandingOptions

            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'    UpdateLeagueStandings Subroutine
'----------------------------------------
'This subroutine lets the user select a
'league (.TMS file) and reads in the stats 
'for that league. It then calculates the
'standings for that league and writes a 
'corresponding .STD file.
Sub UpdateLeagueStandings ()

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!() 
    Shared HLL!(), HWW!()
    Shared LL!(), LTW!(), LTL!()
    Shared LLS!(), LWS!()
    Shared RLL!(), RWW!()
    Shared TLS!(), TWS!(), WW!()

    Shared TML$(), Z1$()

    'These are just read in and written back
    Shared H%(), I%(), V%()
    Shared H$(), L$(), PL$(), W$()

    Shared D1$, D2$, D3$, D4$

    TMR = 40

    leagStatFile$ = ChooseFile$ (diskPaths$(2) + "*.TMS", "LEAGUE STATS", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Open diskPaths$(2) + leagStatFile$ + ".TMS" For Input As #2

        Input #2, league$, D1$, D2$, D3$, D4$

        For X = 1 To 40: Input #2, Z1$(X): Next

        Close 2

        YY = 0

        For KK = 1 To 40

            YY = YY + 1
            Color 14
            Locate 7, 38
            TMR = TMR - 1: Print Using "###"; TMR

            If Z1$(KK) <> "*" Then

                TML$(YY) = Z1$(KK)

                Color 15, 0
                Cls

                Locate 5: Print "COMPUTING RECORDS . . . . . PLEASE WAIT"

                If _FileExists(diskPaths$(1) + Z1$(KK) + "S") Then

                    Open diskPaths$(1) + Z1$(KK) + "S" For Input As #1

                    Input #1, XX%

                    For I = 1 To NUM_STATRECORDS: Input #1, H%(I): Next I

                    For I = 1 To NUM_STATRECORDS: Input #1, H$(I): Next I

                    For I = 1 To NUM_STATRECORDS: Input #1, V%(I): Next I

                    For I = 1 To NUM_STATRECORDS: Input #1, PL$(I): Next I

                    For I = 1 To NUM_STATRECORDS: Input #1, W$(I): Next I

                    For I = 1 To NUM_STATRECORDS: Input #1, L$(I): Next I

                    For I = 1 To NUM_STATRECORDS: Input #1, I%(I): Next I

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

                End If

            End If 'Done checking for '*'

        Next KK

        Open diskPaths$(2) + leagStatFile$ + ".STD" For Output As #1

        Write #1, league$, D1$, D2$, D3$, D4$

        For I = 1 To 40:
            Write #1, TML$(I), WW!(I), LL!(I), TWS!(I), TLS!(I), CWS!(I), CLT!(I), HWW!(I), HLL!(I), RWW!(I), RLL!(I), EWW!(I), ELL!(I), LTW!(I), LTL!(I)
        Next

        Close #1

    End If

End Sub

'----------------------------------------
'   NewStandingOptions Subroutine
'----------------------------------------
'This subroutine lets you choose a prev-
'iously compiled Standings file (.STD)
'and then prompts the user for display
'options.
Sub NewStandingOptions ()

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$(), Z1$()

    Shared D1$, D2$, D3$, D4$

    standingsFile$ = ChooseFile$ (diskPaths$(2) + "*.STD", "LEAGUE STANDINGS", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Open diskPaths$(2) + standingsFile$ + ".STD" For Input As #1

        Input #1, league$, D1$, D2$, D3$, D4$

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

        Select Case UCase$(A$)

            Case "V":
                Call ViewNewStandings(league$)

            Case "P":
                Call PrintNewStandings(league$)

        End Select

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       ViewNewStandings Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the user
'display in a formatted view
Sub ViewNewStandings (league$)

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    Shared D1$, D2$, D3$, D4$

    MJ = 0

    For NT = 1 To 4

        AA = (NT * 10) - 9
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Color 11
        Locate 1, 35: Print league$
        Color 14
        Locate 2: Print Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"
        Locate 3

        If NT = 1 Then Print D1$;
        If NT = 2 Then Print D2$;
        If NT = 3 Then Print D3$;
        If NT = 4 Then Print D4$;

        Print Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                Color 3
                Print TML$(I);
                Color 15

                'G / W / L
                Print Tab(14); Using "###"; WW!(I) + LL!(I); Tab(18);
                Print Using "###"; WW!(I); Tab(22);
                Print Using "###"; LL!(I);

                ' PCT
                If LL!(I) = 0 And LL!(I) + WW!(I) <> 0 Then
                    Print Tab(26); "1.000";
                Else
                    If WW!(I) = 0 Then
                        Print Tab(27); ".000";
                    Else
                        If WW!(I) + LL!(I) <> 0 Then
                            wlPCT! = WW!(I) / (WW!(I) + LL!(I))
                            Print Tab(27); Using ".###"; wlPCT!;
                        Else
                            If WW!(I) + LL!(I) = 0 Then Print Tab(27); ".000";
                        End If
                    End If
                End If

                ' GB
                calcGB! = ((WW!(AA) - WW!(I)) + (LL!(I) - LL!(AA))) / 2
                If calcGB! = 0 Then
                    Print Tab(32); " ---";
                Else
                    Print Tab(32); Using "##.#"; calcGB!;
                End If

                Print Tab(44); Using "##"; TWS!(I); Tab(47); TLS!(I); Tab(51); CWS!(I); Tab(54); CLT!(I);
                Print Tab(57); Using "##"; HWW!(I);: Print Tab(60); Using "##"; HLL!(I);
                Print Tab(63); Using "##"; RWW!(I);: Print Tab(66); Using "##"; RLL!(I);
                Print Tab(69); Using "##"; EWW!(I);: Print Tab(72); Using "##"; ELL!(I);
                Print Tab(75); Using "##"; LTW!(I);: Print Tab(78); Using "##"; LTL!(I)

            End If

        Next

        'MAG #
        MJ = 163 - WW!(AA) - LL!(AA + 1)
        If MJ < 0 Then MJ = 0

        Locate 4, 38
        Print Using "###"; MJ;

        Color 14
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

    Next NT

End Sub

'----------------------------------------
'       PrintNewStandings Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub PrintNewStandings (league$)

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    Shared D1$, D2$, D3$, D4$

    MJ = 0

    LPrint Tab(35); league$

    For NT = 1 To 4

        AA = NT * 10 - 9
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        LPrint Tab(16); "                      MAG.  L  L   C  C   HOME  ROAD  X-IN  L-10"

        If NT = 1 Then LPrint D1$;
        If NT = 2 Then LPrint D2$;
        If NT = 3 Then LPrint D3$;
        If NT = 4 Then LPrint D4$;

        LPrint Tab(16); "G   W   L   PCT   GB   #    WS LS  WS LS  W- L  W- L  W- L  W- L"

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                LPrint TML$(I);
                LPrint Tab(14); Using "###"; WW!(I) + LL!(I); Tab(18);
                LPrint Using "###"; WW!(I); Tab(22);
                LPrint Using "###"; LL!(I);

                'PCT
                If LL!(I) = 0 And LL!(I) + WW!(I) <> 0 Then
                    LPrint Tab(26); "1.000";
                Else
                    If WW!(I) = 0 Then
                        LPrint Tab(27); ".000";
                    Else
                        If WW!(I) + LL!(I) = 0 Then
                            LPrint Tab(27); ".000";
                        Else
                            wlPCT! = WW!(I) / (WW!(I) + LL!(I))
                            LPrint Tab(27); Using ".###"; wlPCT!;
                        End If
                    End If
                End If

                'GB
                If ((WW!(AA) - WW!(I)) + (LL!(I) - LL!(AA))) / 2 = 0 Then
                    LPrint Tab(32); " ---";
                Else
                    LPrint Tab(32); Using "##.#"; ((WW!(NT * 10 - 9) - WW!(I)) + (LL!(I) - LL!(NT * 10 - 9))) / 2;
                End If

                If I = AA Then
                    MJ = 163 - WW!(AA) - LL!(AA + 1)
                    If MJ < 0 Then MJ = 0
                    LPrint Tab(38); Using "###"; MJ
                End If

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


'----------------------------------------
'         SortStandings Subroutine
'----------------------------------------
'This subroutine simply sorts the league
'standings records so that teams that are
'leading can be displayed first.
'This sorting is only done on the arrays
'holding the standing data, so no changes
'are permanent to the underlying files.
Sub SortStandings (AA, ZZ)

    Shared CLT!(), CWS!()
    Shared ELL!(), EWW!(), HLL!(), HWW!()
    Shared LL!(), LLS!(), LTL!(), LTW!(), LWS!()
    Shared RLL!(), RWW!(), TLS!(), TWS!(), WW!()

    Shared TML$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = WW!(X) - LL!(X)
            diffY = WW!(Y) - LL!(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (WW!(X) <= WW!(Y) And WW!(X) > LL!(X)) Or Not (WW!(X) >= WW!(Y) And WW!(X) < LL!(X)) Then

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

End Sub

