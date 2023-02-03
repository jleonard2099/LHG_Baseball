Sub SEELEAG ()

    _Title "Full Count Baseball - View League Results"

    DefInt A-Z

    Shared BA$(), ER$()
    Shared cmplZ2$(), cmplZ3$()

    Dim seeP$(298), T$(40)

    Dim W(40), L(40)

    Dim J!, ATBATS!, INNINGS!
    Dim OFT!(40, 20), DFT!(40, 20)
    Dim Z!(298), Z1!(298)

    Do
        'Display list of files...

        Cls

        Spec$ = diskPaths$(2) + "*.LDR"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim leagueLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                leagueLeaderFiles$(X) = Space$(12)
            Next
            leagueLeaderFiles$(0) = Spec$
            Call ReadFile(leagueLeaderFiles$())

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
                MAMenu leagueLeaderFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = leagueLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                Do

                    targetFile$ = RTrim$(targetFile$)
                    WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                    WE$ = Left$(WE$, 8)

                    Open diskPaths$(2) + WE$ + ".TMS" For Input As #2
                    Input #2, LG$, D1$, D2$, D3$, D4$
                    Close 2

                    D1$ = PadRight$(D1$, 8)
                    D2$ = PadRight$(D2$, 8)
                    D3$ = PadRight$(D3$, 8)
                    D4$ = PadRight$(D4$, 8)

                    H$ = " 1/2"
                    Cls
                    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

                    Open diskPaths$(2) + WE$ + ".LDR" For Input As #2
                    For X = 1 To 40: Input #2, T$(X), W(X), L(X): Next
                    For X = 1 To 298: Input #2, cmplZ2$(X), cmplZ3$(X), Z!(X), Z1!(X): Next
                    For X = 1 To 40: Input #2, BA$(X): For M = 1 To 14: Input #2, OFT!(X, M): Next: Next
                    For X = 1 To 40: Input #2, ER$(X): For M = 1 To 14: Input #2, DFT!(X, M): Next: Next
                    Input #2, APP

                    Close #2

                    'First, we build strings from the values we want to print
                    'Later, we will loop through this array and print those strings
                    'This appears to have been adopted in place of the approach used
                    'most in other games, where complex print logic is in the loop

                    RT = 0

                    For O = 1 To 4

                        first = ((O - 1) * 10) + 1
                        last = O * 10

                        D1 = W(first) - L(first)

                        For P = first To last

                            If T$(P) = "*" Then

                                seeP$(P) = ""

                            Else

                                seeP$(P) = PadRight$(T$(P), 12)

                                'Wins
                                J! = W(P)
                                seeP$(P) = seeP$(P) + Right$("     " + Str$(J!), 3)
                                seeP$(P) = PadRight$(seeP$(P), 16)

                                'Losses
                                J! = L(P)
                                seeP$(P) = seeP$(P) + Right$("     " + Str$(J!), 3)
                                seeP$(P) = PadRight$(seeP$(P), 21)

                                'PCT
                                J! = 0
                                If W(P) + L(P) > 0 Then J! = W(P) / (W(P) + L(P))

                                J! = Int(1000 * J! + .5)

                                If J! = 1000 Then
                                    seeP$(P) = seeP$(P) + "1.000 "
                                Else

                                    If J! > 1000 Then

                                        A$ = Str$(J! - 1000)
                                        L = Len(A$)
                                        A$ = Right$(A$, L - 1)
                                        '0-pad PCT of fewer than 3 chars
                                        While Len(A$) < 3: A$ = "0" + A$: Wend

                                        seeP$(P) = seeP$(P) + "1." + A$
                                    Else
                                        A$ = Str$(J!)
                                        L = Len(A$)
                                        A$ = Right$(A$, L - 1)
                                        '0-pad PCT of fewer than 3 chars
                                        While Len(A$) < 3: A$ = "0" + A$: Wend

                                        seeP$(P) = seeP$(P) + "." + A$

                                    End If

                                End If

                                seeP$(P) = PadRight$(seeP$(P), 28)

                                'Games Behind
                                D = W(P) - L(P)
                                D = D1 - D
                                BK$ = Str$(Int(D / 2))
                                If D = 0 Then BK$ = "---"
                                If D = 1 Then BK$ = "  "
                                If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
                                If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

                                seeP$(P) = seeP$(P) + BK$

                            End If

                        Next P

                    Next O

                    '-----
                    Cls

                    Color 12: Print "                        "; LG$; " LEAGUE STANDINGS": Print
                    Color 14

                    Locate 3, 1: Print "    " + D1$ + "      W   L   PCT   GB"
                    Color 15

                    For P = 1 To 10

                        If seeP$(P) = "" Then
                            RT = RT + 1
                        Else
                            Locate (P + 3 - RT), 1: Print "    "; seeP$(P)
                        End If

                    Next P

                    Print

                    RT = 0
                    Color 14

                    Locate 15, 1: Print "    " + D3$ + "      W   L   PCT   GB"
                    Color 15

                    For P = 21 To 30

                        If seeP$(P) = "" Then
                            RT = RT + 1
                        Else
                            Locate (P - 5 - RT), 1: Print "    "; seeP$(P)
                        End If

                    Next P

                    RT = 0
                    Color 14

                    Locate 3, 41: Print "    " + D2$ + "      W   L   PCT   GB"
                    Color 15

                    For P = 11 To 20

                        If seeP$(P) = "" Then
                            RT = RT + 1
                        Else
                            Locate (P - 7 - RT), 41: Print "    "; seeP$(P)
                        End If

                    Next P

                    Print

                    RT = 0
                    Color 14

                    Locate 15, 41: Print "    " + D4$ + "      W   L   PCT   GB"
                    Color 15

                    For P = 31 To 40

                        If seeP$(P) = "" Then
                            RT = RT + 1
                        Else
                            Locate (P - 15 - RT), 41: Print "    "; seeP$(P)
                        End If

                    Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    AB$ = Str$(Int(AB))
                    TD = IP - 3 * IG
                    AB$ = Right$(AB$, Len(AB$) - 1)
                    If TD > 0 Then TD$ = " 1/3"

                    IG$ = Str$(IP)
                    IP$ = Right$(IG$, Len(IG$) - 1)

                    For P = 1 To 19

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "*" Then

                            seeP$(P) = cmplZ2$(P)

                            If cmplZ3$(P) <> "*" Then
                                seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            End If

                            seeP$(P) = PadRight$(seeP$(P), 26)

                            J! = Int(1000 * Z!(P) + .5)

                            If J! = 1000 Then

                                seeP$(P) = seeP$(P) + "1.000 "

                            Else

                                If J! > 1000 Then
                                    A$ = Str$(J! - 1000)
                                    L = Len(A$)
                                    A$ = Right$(A$, L - 1)
                                    While Len(A$) < 3: A$ = "0" + A$: Wend

                                    A$ = "1." + A$
                                    seeP$(P) = seeP$(P) + A$
                                Else
                                    A$ = Str$(J!)
                                    L = Len(A$)
                                    A$ = Right$(A$, L - 1)
                                    '0-pad PCT of fewer than 3 chars
                                    While Len(A$) < 3: A$ = "0" + A$: Wend

                                    seeP$(P) = seeP$(P) + "." + A$

                                End If

                            End If

                            seeP$(P) = seeP$(P) + "     " + Str$(Z1!(P))

                        End If

                    Next P

                    Cls

                    Color 14
                    Print "      BATTING LEADERS  (MIN 3.1 PA/G)"
                    Print "WHO'S HOT                   AVG   AB    AND WHO'S NOT                AVG   AB"
                    Color 15

                    For P = 1 To 19
                        Print cmplZ2$(P) + ", " + cmplZ3$(P); Tab(28); Using "#.###  ###"; Z!(P); Z1!(P);

                        Print Tab(41); cmplZ2$(P + 238) + ", " + cmplZ3$(P + 238); Tab(69);
                        Print Using "#.###  ###"; Z!(P + 238); Z1!(P + 238)
                    Next

                    Print

                    '-----
                    For P = 21 To 30

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    Cls
                    Color 14: Print "      HITS"
                    Color 15
                    For P = 21 To 30:
                        Print "      "; seeP$(P)
                    Next P
                    Print

                    '-----
                    For P = 32 To 41

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Print "      DOUBLES"
                    Color 15

                    For P = 32 To 41:
                        Print "      "; seeP$(P)
                    Next P

                    '-----
                    For P = 43 To 52

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Locate 1, 40: Print "      TRIPLES"
                    Color 15

                    For P = 43 To 52: Locate (P - 41), 40: Print "      "; seeP$(P): Next P
                    Print

                    '-----
                    For P = 54 To 63
                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Locate 13, 52: Print "   HOME RUNS"
                    Color 15

                    For P = 54 To 63:
                        Locate (P - 40), 40
                        Print "      "; seeP$(P)
                    Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    For P = 65 To 73

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Cls
                    Color 14: Print "      RUNS"
                    Color 15

                    For P = 65 To 73:
                        Print "      "; seeP$(P)
                    Next P

                    Print

                    '-----
                    For P = 76 To 84:

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Print
                    Color 14: Print "      RUNS BATTED IN"
                    Color 15

                    '-----
                    For P = 76 To 84:
                        Print "      "; seeP$(P)
                    Next P

                    '-----
                    For P = 87 To 96

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Locate 1, 40: Print "      STOLEN BASES"
                    Color 15

                    For P = 87 To 96:
                        Locate (P - 85), 40: Print "      "; seeP$(P)
                    Next P

                    '-----
                    For P = 98 To 107:

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)

                            J! = Int(1000 * Z!(P) + .5)

                            If J! = 1000 Then

                                seeP$(P) = seeP$(P) + "1.000 "

                            Else

                                If J! > 1000 Then
                                    A$ = Str$(J! - 1000)
                                    L = Len(A$)
                                    A$ = Right$(A$, L - 1)
                                    While Len(A$) < 3: A$ = "0" + A$: Wend
                                    A$ = "1." + A$
                                    seeP$(P) = seeP$(P) + A$
                                End If

                            End If

                        End If

                    Next P

                    Color 14: Locate 13, 40: Print "      SLUGGING PCT LEADERS"
                    Color 15

                    For P = 99 To 108:
                        Locate (P - 85), 40: Print "      "; seeP$(P)
                    Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    For P = 110 To 119

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)

                            J! = Int(1000 * Z!(P) + .5)

                            If J! = 1000 Then

                                seeP$(P) = seeP$(P) + "1.000 "

                            Else

                                If J! > 1000 Then
                                    A$ = Str$(J! - 1000)
                                    L = Len(A$)
                                    A$ = Right$(A$, L - 1)
                                    While Len(A$) < 3: A$ = "0" + A$: Wend
                                    A$ = "1." + A$
                                    seeP$(P) = seeP$(P) + A$
                                Else
                                    A$ = Str$(J!)
                                    L = Len(A$)
                                    A$ = Right$(A$, L - 1)
                                    '0-pad PCT of fewer than 3 chars
                                    While Len(A$) < 3: A$ = "0" + A$: Wend

                                    seeP$(P) = seeP$(P) + "." + A$

                                End If

                            End If

                        End If

                    Next P

                    Cls
                    Color 14: Print "ON BASE PCT LEADERS"
                    Color 15

                    For P = 110 To 119
                        Print seeP$(P)
                    Next P

                    Print

                    '-----
                    For P = 217 To 226

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Locate 1, 41: Print "BASE ON BALLS"
                    Color 15

                    For P = 217 To 226:
                        Locate (P - 215), 41: Print seeP$(P)
                    Next P

                    '-----
                    Locate 13, 1
                    Color 14: Print "CURRENT HITTING STREAK"
                    Color 15

                    For P = 279 To 288
                        Print cmplZ2$(P) + ", " + cmplZ3$(P); Tab(28); Using "###"; Z!(P)
                    Next P

                    Locate 13, 41
                    Color 14: Print "LONGEST HITTING STREAK"
                    Color 15

                    For P = 289 To 298
                        Locate , 41: Print cmplZ2$(P) + ", " + cmplZ3$(P); Tab(68); Using "###"; Z!(P)
                    Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    For P = 120 To 138

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)

                            J! = Int(100 * Z!(P) + .5)
                            A$ = Str$(J!)

                            LN = Len(A$)
                            A$ = Right$(A$, LN - 1)

                            LN = Len(A$)
                            While Len(A$) < 3: A$ = "0" + A$: Wend

                            A$ = Left$(A$, LN - 2) + "." + Right$(A$, 2)
                            A$ = "    " + A$
                            seeP$(P) = seeP$(P) + Right$(A$, 5)

                            J! = Int(Z1!(P))
                            seeP$(P) = seeP$(P) + "     " + Str$(J!)

                        End If

                    Next P

                    '-----
                    Cls
                    Color 14
                    Print "       ERA LEADERS (MIN 1 IP/G)"
                    Print "WHO'S HOT                     ERA   IP  WHO'S NOT                      ERA  IP"
                    Color 15

                    For P = 121 To 139
                        Print cmplZ2$(P) + ", " + cmplZ3$(P); Tab(30); Using "#.##  ###"; Z!(P); Z1!(P);
                        Print Tab(41);
                        Print cmplZ2$(P + 138) + ", " + cmplZ3$(P + 138); Tab(70); Using "##.## ###"; Z!(P + 138); Z1!(P + 138)
                    Next

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    For P = 140 To 149

                        seeP$(P) = ""

                        If Z!(P) <> Z!(150) Or Z1!(P) <> Z1!(150) Then

                            seeP$(P) = seeP$(P) + cmplZ2$(P)

                            If cmplZ3$(P) <> "" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 2)
                            seeP$(P) = seeP$(P) + "-"
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z1!(P)), 2)

                        End If

                    Next P

                    Cls
                    Color 14
                    Print "      PITCHING RECORD"
                    Color 15

                    For P = 140 To 149
                        Print "      "; seeP$(P)
                    Next

                    Print

                    '-----
                    For P = 151 To 160
                        seeP$(P) = ""

                        If cmplZ2$(P) <> "*" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Print "      VICTORIES"
                    Color 15

                    For P = 151 To 160: Print "      "; seeP$(P): Next P

                    '-----
                    For P = 162 To 171

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Int(Z!(P))), 3)

                        End If

                    Next P

                    Locate 1, 40: Color 14: Print "      INNINGS PITCHED"
                    Color 15

                    For P = 162 To 171: Locate (P - 160), 40: Print "      "; seeP$(P): Next P

                    '-----
                    For P = 173 To 182

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                            seeP$(P) = PadRight$(seeP$(P), 26)

                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Locate 13, 40: Print "      STRIKEOUTS     "
                    Color 15

                    For P = 173 To 182: Locate (P - 159), 40: Print "      "; seeP$(P): Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    For P = 184 To 193
                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    '-----
                    Cls
                    Color 14: Print "      COMPLETE GAMES "
                    Color 15

                    For P = 184 To 193:
                        Print "      "; seeP$(P)
                    Next P

                    Print

                    '-----
                    For P = 195 To 204
                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Color 14: Print "      SHUTOUTS       "
                    Color 15

                    For P = 195 To 204
                        Print "      "; seeP$(P)
                    Next P

                    '-----
                    For P = 206 To 215

                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P

                    Locate 1, 40: Color 14: Print "      SAVES          "
                    Color 15

                    For P = 206 To 215:
                        Locate (P - 204), 40
                        Print "      "; seeP$(P)
                    Next P

                    '-----
                    Color 14: Locate 13, 40: Print "      APPEARANCES    "
                    Color 15

                    For P = 228 To 237
                        seeP$(P) = ""

                        If cmplZ2$(P) <> "" Then

                            seeP$(P) = cmplZ2$(P)
                            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                            seeP$(P) = PadRight$(seeP$(P), 26)
                            seeP$(P) = seeP$(P) + Right$("     " + Str$(Z!(P)), 3)

                        End If

                    Next P


                    For P = 228 To 237:
                        Locate (P - 214), 40
                        Print "      "; seeP$(P)
                    Next P

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$

                    '-----
                    Cls
                    Color 12: Print "                                 CLUB BATTING   ": Color 14

                    Print "CLUB         AVG   AB    R    H   2B   3B  HR   RBI   BB   SO  SB  CS   E"
                    Color 15
                    H! = 0
                    AB = 0
                    R = 0
                    B2% = 0
                    B3 = 0
                    HR = 0
                    RBI = 0
                    BB = 0
                    SO = 0
                    SB = 0
                    CS = 0
                    E = 0

                    For X = 1 To 40

                        If BA$(X) <> "" And BA$(X) <> "*" Then
                            Print Left$(BA$(X), 11); Tab(13);
                            '           "AVG   AB    R    H   2B   3B  HR   RBI   BB   SO  SB  CS   E"
                            Print Using ".### ####  ### #### ###  ### ###  ####  ### #### ### ###  ###"; OFT!(X, 1); OFT!(X, 3); OFT!(X, 5); OFT!(X, 4); OFT!(X, 7); OFT!(X, 8); OFT!(X, 9); OFT!(X, 6); OFT!(X, 10); OFT!(X, 11); OFT!(X, 12); OFT!(X, 13); OFT!(X, 14)
                            ATBATS! = ATBATS! + OFT!(X, 3)
                            H! = H! + OFT!(X, 4)
                            R = R + OFT!(X, 5)
                            B2 = B2 + OFT!(X, 7)
                            B3 = B3 + OFT!(X, 8)
                            HR = HR + OFT!(X, 9)
                            RBI = RBI + OFT!(X, 6)
                            BB = BB + OFT!(X, 10)
                            SO = SO + OFT!(X, 11)
                            SB = SB + OFT!(X, 12)
                            CS = CS + OFT!(X, 13)
                            E = E + OFT!(X, 14)
                        End If
                    Next

                    If ATBATS! <> 0 Then
                        statCalc! = H! / ATBATS!
                    Else
                        statCalc! = 0
                    End If

                    Print
                    Print "LG TOTALS"; Tab(13); Using "#.###"; statCalc!;

                    '                      AB    R    H   2B   3B  HR   RBI   BB   SO  SB  CS   E"
                    Print Tab(18); Using "#### ### #### ###  ### ###  ####  ### #### ### ###  ###"; ATBATS!; R; H!; B2; B3; HR; RBI; BB; SO; SB; CS; E

                    '-----
                    HZ = 1: VT = 24
                    Locate VT, HZ
                    Print Space$(26);
                    QK$ = GetKeyPress$
                    '-----

                    Cls
                    Color 12: Print "                                 CLUB PITCHING  ": Color 14

                    Print "CLUB          ERA   G  CG  SHO  SV    IP     H     R    ER     BB     SO"
                    Color 15

                    R = 0: G = 0: IP = 0

                    For X = 1 To 40

                        If ER$(X) <> "" And ER$(X) <> "*" Then

                            R = R + DFT!(X, 5)
                            G = G + DFT!(X, 12)
                            INNINGS! = INNINGS! + DFT!(X, 2)

                            Print Left$(ER$(X), 11); Tab(13);

                            DFT!(X, 1) = Int(DFT!(X, 1) * 100) / 100

                            Print Using "##.##"; DFT!(X, 1);
                            Print Tab(19); Using "### ### ###  ###  ####  ####  ####  ####"; DFT!(X, 11); DFT!(X, 12); DFT!(X, 13); DFT!(X, 14); CInt(DFT!(X, 2) / 3); DFT!(X, 3); DFT!(X, 4); DFT!(X, 5);
                            Print Using "   ####   ####"; DFT!(X, 6); DFT!(X, 7)

                        End If

                    Next

                    If INNINGS! > 0 Then
                        statCalc! = R / (INNINGS! / 3) * 9
                    Else
                        '-- I think this is right?
                        statCalc! = 0
                    End If

                    Print Tab(13); Using "##.##"; statCalc!
                    'PRINT "COMPLETE GAMES: "; G
                    'PRINT "APPEARANCES: "; APP

                    HZ = 1: VT = 24
                    Locate VT, HZ: Print Space$(26);
                    QK$ = GetKeyPress$

                    Cls

                    JJ = 4
                    Locate 6, 8
                    Color 15, 1

                    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

                    For II = 1 To JJ:
                        Locate 6 + II, 8
                        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                    Next

                    Locate 7 + JJ, 8
                    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

                    Do
                        Color 15, 1
                        Locate 8
                        U$ = "VIEW LEAGUE STATS AGAIN (YN)"
                        Locate , (80 - Len(U$)) / 2
                        Print U$
                        Color 15, 0
                        A$ = GetKeyPress$
                    Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"


                    Cls

                Loop Until UCase$(A$) = "N"

            End If 'Done checking exit code

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(A$) = "N"

End Sub
