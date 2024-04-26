'----------------------------------------
'           SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "Full Count Baseball - View League Stats"

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles(league$, div1Name$, div2Name$, leaderFile$)
        Call StatOptions(league$, div1Name$, div2Name$)

    End If

End Sub


'----------------------------------------
'       LoadLeaderFiles Subroutine
'----------------------------------------
'This subroutine loads the league stats
'from the file into several arrays
Sub LoadLeaderFiles (league$, div1Name$, div2Name$, targetFile$)

    Shared Wins(), Losses()

    Shared DFT!(), OFT!()
    Shared seeZ!(), seeZ1!()

    Shared BA$(), compileZ2$(), compileZ3$(), ER$()
    Shared LF$(), seeP$(), RT$(), seeT$()

    Open diskPaths$(2) + targetFile$ + ".TMS" For Input As #2

    Input #2, league$, div1Name$, div2Name$, div3Name$, div4Name$

    Close 2

    div1Name$ = PadRight$(div1Name$, 8)
    div2Name$ = PadRight$(div2Name$, 8)
    div3Name$ = PadRight$(div3Name$, 8)
    div4Name$ = PadRight$(div4Name$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + targetFile$ + ".LDR" For Input As #2

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, seeT$(X), Wins(X), Losses(X)
    Next

    For X = 1 To 298: 
        Input #2, compileZ2$(X), compileZ3$(X), seeZ!(X), seeZ1!(X)
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, BA$(X)
        For M = 1 To 14:
            Input #2, OFT!(X, M)
        Next
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, ER$(X)
        For M = 1 To 14:
            Input #2, DFT!(X, M)
        Next
    Next

    Input #2, APP

    Close #2

End Sub


'----------------------------------------
'         StatOptions Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions (league$, div1Name$, div2Name$)

    Do

        Cls
        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SAVEFILEDIALOG$("Save League Report",_CWD$ + "\Report","*.txt","Text File")
                    If userFile$ <> "" Then
                        Call LeagueStats_Report(league$, div1Name$, div2Name$, 0, userFile$)
                    End If

                Case "P":
                    Call LeagueStats_Report(league$, div1Name$, div2Name$, 1, "REPORT")

                Case "S":
                    Call LeagueStats_Screen(league$, div1Name$, div2Name$)

            End Select

            Color 15, 0
            Cls

            Call box0 (6, 8, 11, 70, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub


'----------------------------------------
'      LeagueStats_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeagueStats_Screen (league$, div1Name$, div2Name$)

    Shared Wins(), Losses()

    Shared DFT!(), OFT!()
    Shared seeZ!(), seeZ1!()

    Shared BA$(), compileZ2$(), compileZ3$(), ER$()
    Shared LF$(), seeP$(), RT$(), seeT$()

    Dim J!, ATBATS!, INNINGS!

    'First, we build strings from the values we want to print
    'Later, we will loop through this array and print those strings
    'This appears to have been adopted in place of the approach used
    'most in other games, where complex print logic is in the loop

    RT = 0

    '------------------------------
    '          STANDINGS
    '------------------------------

    'Cycle through divisions
    For O = 1 To 4

        first = ((O - 1) * 10) + 1
        last = O * 10

        D1 = Wins(first) - Losses(first)

        For P = first To last

            If seeT$(P) = "*" Then

                seeP$(P) = ""

            Else

                seeP$(P) = PadRight$(seeT$(P), 12)

                'Wins
                seeP$(P) = seeP$(P) + Right$("     " + Str$(Wins(P)), 3)
                seeP$(P) = PadRight$(seeP$(P), 16)

                'Losses
                seeP$(P) = seeP$(P) + Right$("     " + Str$(Losses(P)), 3)
                seeP$(P) = PadRight$(seeP$(P), 21)

                'PCT
                J! = 0
                If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))

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
                D = Wins(P) - Losses(P)
                D = D1 - D
                BK$ = Str$(Int(D / 2))
                If D = 0 Then BK$ = "---"
                If D = 1 Then BK$ = "  "
                If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
                If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

                seeP$(P) = seeP$(P) + BK$

            End If

        Next P

    Next O

    '-----

    Cls

    Color 12
    Print "                        "; league$; " LEAGUE STANDINGS"
    Print

    '------------------------------
    ' Division 1 Standings
    '------------------------------
    Color 14
    Locate 3, 1

    If div1Name$ = "" Then div1Name$ = Space$(8)
    Print "    " + div1Name$ + "      W   L   PCT   GB"

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

    '------------------------------
    ' Division 3 Standings
    '------------------------------

    Color 14
    Locate 15, 1

    If div3Name$ = "" Then div3Name$ = Space$(8)
    Print "    " + div3Name$ + "      W   L   PCT   GB"

    Color 15

    For P = 21 To 30

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 5 - RT), 1: Print "    "; seeP$(P)
        End If

    Next P

    RT = 0

    '------------------------------
    ' Division 2 Standings
    '------------------------------

    Color 14
    Locate 3, 41

    If div2Name$ = "" Then div2Name$ = Space$(8)
    Print "    " + div2Name$ + "      W   L   PCT   GB"

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

    '------------------------------
    ' Division 4 Standings
    '------------------------------
    Color 14
    Locate 15, 41

    If div4Name$ = "" Then div4Name$ = Space$(8)
    Print "    " + div4Name$ + "      W   L   PCT   GB"

    Color 15

    For P = 31 To 40

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 15 - RT), 41: Print "    "; seeP$(P)
        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    ' String Constants
    '------------------------------

    AB$ = Str$(Int(AB))
    TD = IP - 3 * IG
    AB$ = Right$(AB$, Len(AB$) - 1)
    If TD > 0 Then TD$ = " 1/3"

    IG$ = Str$(IP)
    IP$ = Right$(IG$, Len(IG$) - 1)

    '------------------------------
    '        BATTING LEADERS
    '------------------------------

    For P = 1 To 19

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then
                seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            End If

            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(1000 * seeZ!(P) + .5)

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

            seeP$(P) = seeP$(P) + "     " + Str$(seeZ1!(P))

        End If

    Next P


    Cls

    Color 14
    Print "      BATTING LEADERS  (MIN 3.1 PA/G)"
    Print "WHO'S HOT                   AVG   AB    AND WHO'S NOT                AVG   AB"
    Color 15

    For P = 1 To 19
        Print compileZ2$(P) + ", " + compileZ3$(P); Tab(28); Using "#.###  ###"; seeZ!(P); seeZ1!(P);

        Print Tab(41); compileZ2$(P + 238) + ", " + compileZ3$(P + 238); Tab(69);
        Print Using "#.###  ###"; seeZ!(P + 238); seeZ1!(P + 238)
    Next

    Print

    '------------------------------
    '             HITS
    '------------------------------

    For P = 21 To 30

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14: Print "      HITS"
    Color 15
    For P = 21 To 30:
        Print "      "; seeP$(P)
    Next P
    Print


    '------------------------------
    '           DOUBLES
    '------------------------------

    For P = 32 To 41

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Color 14
    Print "      DOUBLES"
    Color 15

    For P = 32 To 41:
        Print "      "; seeP$(P)
    Next P


    '------------------------------
    '           TRIPLES
    '------------------------------

    For P = 43 To 52

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      TRIPLES"
    Color 15

    For P = 43 To 52
        Locate (P - 41), 40
        Print "      "; seeP$(P)
    Next P

    Print

    '------------------------------
    '           HOME RUNS
    '------------------------------

    For P = 54 To 63
        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 13, 40
    Color 14
    Print "      HOME RUNS"
    Color 15

    For P = 54 To 63:
        Locate (P - 40), 40
        Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '             RUNS
    '------------------------------

    For P = 65 To 73

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Cls
    Color 14
    Print "      RUNS"
    Color 15

    For P = 65 To 73:
        Print "      "; seeP$(P)
    Next P

    Print

    '------------------------------
    '             RBIs
    '------------------------------

    For P = 76 To 84:

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Print
    Color 14
    Print "      RUNS BATTED IN"
    Color 15

    For P = 76 To 84:
        Print "      "; seeP$(P)
    Next P


    '------------------------------
    '         STOLEN BASES
    '------------------------------

    For P = 87 To 96

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      STOLEN BASES"
    Color 15

    For P = 87 To 96:
        Locate (P - 85), 40
        Print "      "; seeP$(P)
    Next P


    '------------------------------
    '         SLUGGING PCT
    '------------------------------

    For P = 98 To 107:

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(1000 * seeZ!(P) + .5)

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

    Locate 13, 40
    Color 14
    Print "      SLUGGING PCT LEADERS"
    Color 15

    For P = 99 To 108:
        Locate (P - 85), 40: Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '         ON BASE PCT
    '------------------------------

    For P = 110 To 119

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = Left$(compileZ2$(P), 13)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(1000 * seeZ!(P) + .5)

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

    Color 14
    Print "      ON BASE PCT LEADERS"
    Color 15

    For P = 110 To 119
        Print "      ";
        Print seeP$(P)
    Next P

    Print

    '------------------------------
    '     BASE ON BALLS (WALK)
    '------------------------------

    For P = 217 To 226

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P


    Locate 1, 40
    Color 14
    Print " BASE ON BALLS"
    Color 15

    For P = 217 To 226:
        Locate (P - 215), 41: Print seeP$(P)
    Next P

    '------------------------------

    Locate 13, 1
    Color 14
    Print "      CURRENT HITTING STREAK"
    Color 15

    For P = 279 To 288
        Print "      ";
        Print Left$(compileZ2$(P), 13) + ", " + compileZ3$(P); Tab(34); Using "###"; seeZ!(P)
    Next P

    Locate 13, 40
    Color 14
    Print " LONGEST HITTING STREAK"
    Color 15

    For P = 289 To 298
        Locate , 41
        Print Left$(compileZ2$(P), 13) + ", " + compileZ3$(P); Tab(67); Using "###"; seeZ!(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '         ERA LEADERS
    '------------------------------

    For P = 120 To 138

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(100 * seeZ!(P) + .5)
            A$ = Str$(J!)

            LN = Len(A$)
            A$ = Right$(A$, LN - 1)

            LN = Len(A$)
            While Len(A$) < 3: A$ = "0" + A$: Wend

            A$ = Left$(A$, LN - 2) + "." + Right$(A$, 2)
            A$ = "    " + A$
            seeP$(P) = seeP$(P) + Right$(A$, 5)

            J! = Int(seeZ1!(P))
            seeP$(P) = seeP$(P) + "     " + Str$(J!)

        End If

    Next P


    Cls

    Color 14
    Print "       ERA LEADERS (MIN 1 IP/G)"
    Print "WHO'S HOT                     ERA   IP  WHO'S NOT                      ERA  IP"
    Color 15

    For P = 121 To 139
        Print compileZ2$(P) + ", " + compileZ3$(P); Tab(30); Using "#.##  ###"; seeZ!(P); seeZ1!(P);
        Print Tab(41);
        Print compileZ2$(P + 138) + ", " + compileZ3$(P + 138); Tab(70); Using "##.## ###"; seeZ!(P + 138); seeZ1!(P + 138)
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '       PITCHING RECORD
    '------------------------------

    For P = 140 To 149

        seeP$(P) = ""

        If seeZ!(P) <> seeZ!(150) Or seeZ1!(P) <> seeZ1!(150) Then

            seeP$(P) = seeP$(P) + compileZ2$(P)

            If compileZ3$(P) <> "" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 2)
            seeP$(P) = seeP$(P) + "-"
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ1!(P)), 2)

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

    '------------------------------
    '           VICTORIES
    '------------------------------

    For P = 151 To 160
        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Color 14
    Print "      VICTORIES"
    Color 15

    For P = 151 To 160:
        Print "      "; seeP$(P)
    Next P

    '------------------------------
    '       INNINGS PITCHED
    '------------------------------

    For P = 162 To 171

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(Int(seeZ!(P))), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      INNINGS PITCHED"
    Color 15

    For P = 162 To 171:
        Locate (P - 160), 40: Print "      "; seeP$(P)
    Next P

    '------------------------------
    '          STRIKEOUTS
    '------------------------------

    For P = 173 To 182

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            seeP$(P) = PadRight$(seeP$(P), 26)

            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 13, 40
    Color 14
    Print "      STRIKEOUTS     "
    Color 15

    For P = 173 To 182:
        Locate (P - 159), 40
        Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '          FULL GAMES
    '------------------------------

    For P = 184 To 193

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Cls

    Color 14
    Print "      COMPLETE GAMES "
    Color 15

    For P = 184 To 193:
        Print "      "; seeP$(P)
    Next P

    Print

    '------------------------------
    '           SHUTOUTS
    '------------------------------

    For P = 195 To 204

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Color 14
    Print "      SHUTOUTS       "
    Color 15

    For P = 195 To 204
        Print "      "; seeP$(P)
    Next P

    '------------------------------
    '            SAVES
    '------------------------------
    For P = 206 To 215

        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      SAVES          "
    Color 15

    For P = 206 To 215:
        Locate (P - 204), 40
        Print "      "; seeP$(P)
    Next P

    '------------------------------
    '         APPEARANCES
    '------------------------------

    Locate 13, 40
    Color 14
    Print "      APPEARANCES    "
    Color 15

    For P = 228 To 237
        seeP$(P) = ""

        If compileZ2$(P) <> "" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P


    For P = 228 To 237:
        Locate (P - 214), 40
        Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                                 CLUB BATTING   "
    Color 14

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

    For X = 1 To TEAMS_PER_LEAGUE

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
    Print "LG TOtotAwayLossesS"; Tab(13); Using "#.###"; statCalc!;

    '                      AB    R    H   2B   3B  HR   RBI   BB   SO  SB  CS   E"
    Print Tab(18); Using "     ###       ###      ###        ###      ###      ###"; R; B2; HR; BB; SB; E
    Print Tab(18); Using "####     ####       ###      ####      ####     ###     "; ATBATS!; H!; B3; RBI; SO; CS

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                                 CLUB PITCHING  "
    Color 14

    Print "CLUB          ERA   G  CG  SHO  SV    IP     H     R    ER     BB     SO"
    Color 15

    R = 0
    G = 0
    IP = 0

    For X = 1 To TEAMS_PER_LEAGUE

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
        statCalc! = 0
    End If

    Print Tab(13); Using "##.##"; statCalc!
    'PRINT "COMPLETE GAMES: "; G
    'PRINT "APPEARANCES: "; APP

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'      LeagueStats_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer
Sub LeagueStats_Report (league$, div1Name$, div2Name$, printDest, destFile$)

    Shared Wins(), Losses()

    Shared DFT!(), OFT!()
    Shared seeZ!(), seeZ1!()

    Shared BA$(), compileZ2$(), compileZ3$(), ER$()
    Shared LF$(), seeP$(), RT$(), seeT$()

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    For P = 1 To 10:

        If seeT$(P) = "*" Then
            seeP$(P) = ""
        Else

            seeP$(P) = seeT$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 16
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D
            BK$ = Str$(Int(D / 2))

            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If

            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    '-----

    D1 = Wins(11) - Losses(11)

    For P = 11 To 20:

        If seeT$(P) = "*" Then
            seeP$(P) = ""
        Else

            seeP$(P) = seeT$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            L = 16

            While Len(seeP$(P)) < L
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D
            BK$ = Str$(Int(D / 2))

            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    '-----

    D1 = Wins(21) - Losses(21)

    For P = 21 To 30

        If seeT$(P) = "*" Then
            seeP$(P) = ""
        Else
            seeP$(P) = seeT$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 16
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D

            BK$ = Str$(Int(D / 2))
            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    '-----

    D1 = Wins(31) - Losses(31)

    For P = 31 To 40

        If seeT$(P) = "*" Then
            seeP$(P) = ""
        Else
            seeP$(P) = seeT$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 16
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D
            BK$ = Str$(Int(D / 2))

            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    Cls

    '-----

    LF$(1) = "    " + league$ + " LEAGUE STANDINGS AS OF " + Date$
    LF$(3) = "    " + div1Name$ + "      W   L   PCT   GB"

    For P = 1 To 10:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            LF$(P + 3 - RT) = "    " + seeP$(P)
        End If

    Next P

    RT = 0

    LF$(15) = "    " + div3Name$ + "      W   L   PCT   GB"

    For P = 21 To 30:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            LF$(P - 5 - RT) = "    " + seeP$(P)
        End If

    Next P

    RT = 0

    RT$(3) = "    " + div2Name$ + "      W   L   PCT   GB"

    For P = 11 To 20

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            RT$(P - 7 - RT) = "    " + seeP$(P)
        End If

    Next P

    RT = 0

    RT$(15) = "    " + div4Name$ + "      W   L   PCT   GB"

    For P = 31 To 40

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            RT$(P - 15 - RT) = "    " + seeP$(P)
        End If

    Next P

    '-----

    For P = 2 To 26

        If Len(LF$(P)) < 38 Then
            LF$(P) = LF$(P) + " "
        Else
            If Len(RT$(P)) < 38 Then
                RT$(P) = RT$(P) + " "
            End If
        End If

    Next P

    For X = 1 To 26
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    AB = Int(AB)
    AB$ = Str$(AB)
    TD = IP - 3 * IG

    AB$ = Right$(AB$, Len(AB$) - 1)
    If TD > 0 Then TD$ = " 1/3"
    IG$ = Str$(IP)

    IG$ = Right$(IG$, Len(IG$) - 1)
    IP$ = IG$

    '-----

    For P = 1 To 20

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 27
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)

            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
            J! = seeZ1!(P)
            seeP$(P) = seeP$(P) + " " + Str$(J)

        End If

    Next P

    Cls

    LF$(27) = "   BATTING LEADERS  (MIN 3.1 PA/G)"

    For P = 1 To 19
        LF$(P + 27) = "   " + seeP$(P)
    Next

    LF$(47) = " "

    '-----

    For P = 239 To 258

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 27
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)

            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
            J! = seeZ1!(P)
            seeP$(P) = seeP$(P) + " " + Str$(J!)

        End If

    Next P

    Cls

    RT$(27) = "       BATTING TRAILERS  (MIN 3.1 PA/G)"

    For P = 239 To 258:
        RT$(P - 211) = "   " + seeP$(P)
    Next

    '-----

    RT$(47) = " "

    For P = 27 To 47

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 27 To 47:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 21 To 30

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    LF$(49) = "   HITS"
    For P = 21 To 30:
        LF$(P + 29) = "   " + seeP$(P)
    Next P

    '-----

    For P = 32 To 41

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3

            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(49) = "       DOUBLES"
    For P = 32 To 41:
        RT$(P + 18) = "       " + seeP$(P)
    Next P

    '-----

    For P = 49 To 59

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 49 To 59:
        Print #1, LF$(X); RT$(X)
    Next

    Print #1, Chr$(12)

    '-----

    For P = 43 To 52

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    LF$(1) = "   TRIPLES"

    For P = 43 To 52:
        LF$(P - 41) = "   " + seeP$(P)
    Next P

    LF$(12) = " "

    '-----

    For P = 54 To 63:

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    RT$(1) = "     HOME RUNS"
    For P = 54 To 63:
        RT$(P - 52) = "     " + seeP$(P)
    Next P

    RT$(12) = " "

    '-----

    For P = 1 To 12

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 1 To 12:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 65 To 73

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then
            seeP$(P) = compileZ2$(P): If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(13) = "   RUNS"
    For P = 65 To 73:
        LF$(P - 51) = "   " + seeP$(P)
    Next P
    LF$(23) = " "

    '-----

    For P = 76 To 84

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(13) = "     RUNS BATTED IN"
    For P = 76 To 84:
        RT$(P - 62) = "     " + seeP$(P)
    Next P
    RT$(23) = " "

    '-----

    For P = 13 To 23

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 13 To 23:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 87 To 96

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(24) = "   STOLEN BASES"
    For P = 87 To 96:
        LF$(P - 62) = "   " + seeP$(P)
    Next P
    LF$(35) = " "

    '-----

    For P = 99 To 108

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
        End If

    Next P

    RT$(24) = "     SLUGGING PCT LEADERS"
    For P = 99 To 108:
        RT$(P - 74) = "     " + seeP$(P)
    Next P
    RT$(35) = " "

    '-----

    For P = 24 To 35

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 24 To 35:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 110 To 119

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
        End If

    Next P

    LF$(36) = "   ON BASE PCT LEADERS"
    For P = 110 To 119
        LF$(P - 73) = "   " + seeP$(P)
    Next P
    LF$(47) = " "

    '-----

    For P = 217 To 226

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(36) = "     BASE ON BALLS"
    For P = 217 To 226
        RT$(P - 180) = "    " + seeP$(P)
    Next P
    RT$(47) = " "

    '-----

    For P = 36 To 47

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 36 To 47
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 279 To 288

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(48) = "   CURRENT HITTING STREAK"

    For P = 279 To 288: LF$(P - 230) = "   " + seeP$(P): Next P

    '-----

    For P = 289 To 298:

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(48) = "       LONGEST HITTING STREAK"
    For P = 289 To 298:
        RT$(P - 240) = "       " + seeP$(P)
    Next P

    '-----

    For P = 48 To 58

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 48 To 58:
        Print #1, LF$(X); RT$(X)
    Next

    Print #1, Chr$(12)

    '-----

    For P = 121 To 139

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(100 * J! + .5)
            A$ = Str$(J)
            LN = Len(A$)
            A$ = Right$(A$, LN - 1)

            '-- couldn't we just do Print Using???
            While Len(A$) < 3
                LN = Len(A$)
                A$ = "0" + A$
            Wend

            A$ = Left$(A$, LN - 2) + "." + Right$(A$, 2)
            A$ = "    " + A$
            A$ = Right$(A$, 5)
            seeP$(P) = seeP$(P) + A$
            J! = Int(seeZ1!(P))

            seeP$(P) = seeP$(P) + " " + Str$(J!)

        End If

    Next P

    RT$(27) = "     ERA LEADERS (MIN 1 IP/G)"

    For P = 121 To 139
        RT$(P - 93) = "    " + seeP$(P)
    Next

    '-----

    For P = 140 To 149

        seeP$(P) = ""

        If seeZ!(P) <> seeZ!(150) Or seeZ1!(P) <> seeZ1!(150) Then

            seeP$(P) = seeP$(P) + compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 23
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 2
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
            seeP$(P) = seeP$(P) + "-"
            J! = seeZ1!(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    LF$(48) = "   PITCHING RECORD"
    For P = 140 To 149
        LF$(P - 91) = "   " + seeP$(P)
    Next
    LF$(59) = " "

    '-----

    For P = 151 To 160: seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(48) = "     VICTORIES"
    For P = 151 To 160
        RT$(P - 102) = "     " + seeP$(P)
    Next P
    RT$(59) = " "

    '-----

    For P = 48 To 59

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 48 To 59:
        Print #1, LF$(X); RT$(X)
    Next

    Print #1, Chr$(12)

    '-----

    For P = 162 To 171

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Int(seeZ!(P))
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(1) = "   INNINGS PITCHED"
    For P = 162 To 171:
        LF$(P - 160) = "   " + seeP$(P)
    Next P
    LF$(12) = " "

    '-----

    For P = 173 To 182:

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    '-----

    RT$(1) = "     STRIKEOUTS"
    For P = 173 To 182:
        RT$(P - 171) = "     " + seeP$(P)
    Next P
    RT$(12) = " "

    For P = 1 To 12

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 1 To 12: 
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 184 To 193

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(13) = "   COMPLETE GAMES"
    For P = 184 To 193:
        LF$(P - 170) = "   " + seeP$(P)
    Next P
    LF$(24) = " "

    '-----

    For P = 195 To 204: seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(13) = "     SHUTOUTS"
    For P = 195 To 204:
        RT$(P - 181) = "     " + seeP$(P)
    Next P
    RT$(24) = " "

    '-----

    For P = 13 To 24

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 13 To 24:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    For P = 206 To 215

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then
            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(25) = "   SAVES"

    For P = 206 To 215:
        LF$(P - 180) = "   " + seeP$(P)
    Next P

    LF$(36) = " "
    RT$(25) = "     APPEARANCES"

    '-----

    For P = 228 To 237

        seeP$(P) = ""

        If compileZ2$(P) <> "*" Then

            seeP$(P) = compileZ2$(P)
            If compileZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + compileZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If
    Next P

    For P = 228 To 237
        RT$(P - 202) = "     " + seeP$(P)
    Next P
    RT$(36) = " "

    '-----

    For P = 25 To 36

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 25 To 36:
        Print #1, LF$(X); RT$(X)
    Next

    '-----

    Print #1, Chr$(12)
    Print #1, "                 CLUB BATTING   "
    Print #1,
    Print #1, "CLUB     AVG   AB    R    H   2B   3B  HR  RBI  BB   SO   SB   CS  E"

    For X = 1 To TEAMS_PER_LEAGUE

        If BA$(X) <> "" And Left$(BA$(X), 1) <> "*" And BA$(X) <> "*" Then
            Print #1, Left$(BA$(X), 11); Tab(13);
            OFT!(X, 1) = Int(OFT!(X, 1) * 1000) / 1000
            Print #1, Using ".###"; OFT!(X, 1);
            Print #1, Tab(18); OFT!(X, 3); Tab(24); OFT!(X, 5); Tab(29); Str$(OFT!(X, 4)); Tab(34); OFT!(X, 7); Tab(39); OFT!(X, 8); Tab(43); Str$(OFT!(X, 9)); Tab(47); OFT!(X, 6); Tab(52); OFT!(X, 10);
            Print #1, Tab(57); OFT!(X, 11); Tab(62); OFT!(X, 12); Tab(67); OFT!(X, 13); Tab(72); OFT!(X, 14)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "                 CLUB PITCHING  "
    Print #1,
    Print #1, "CLUB     ERA  G    CG SHO  SV  IP    H    R   ER   BB   SO    "

    For X = 1 To TEAMS_PER_LEAGUE

        If ER$(X) <> "" And ER$(X) <> "*" Then

            Print #1, Left$(ER$(X), 11); Tab(13);

            DFT!(X, 1) = Int(DFT!(X, 1) * 100) / 100
            Print #1, Using "##.##"; DFT!(X, 1);
            Print #1, Tab(18); DFT!(X, 11); Tab(23); DFT!(X, 12); Tab(27); DFT!(X, 13); Tab(31); DFT!(X, 14); Tab(35); Str$(CInt(DFT!(X, 2) / 3)); Tab(40); Str$(DFT!(X, 3)); Tab(45); DFT!(X, 4); Tab(50); DFT!(X, 5);
            Print #1, Tab(55); DFT!(X, 6); Tab(60); DFT!(X, 7)

        End If

    Next

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

