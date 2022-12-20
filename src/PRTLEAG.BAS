Sub PRTLEAG ()

    _Title "Full Count Baseball - Print League Results"

    DefInt A-Z

    Dim BA$(40), ER$(40), LF$(70), P$(298)
    Dim RT$(70), T$(40), Z2$(298), Z3$(298)

    Dim W(40), L(40)

    Dim OFT!(40, 20), DFT!(40, 20)
    Dim Z!(298), Z1!(298)

    Do
        'Display list of files...

        Cls

        Spec$ = diskPaths$(2) + "*.LDR"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%
        IF Count% = 0 THEN 
            LOCATE 10, 8
            CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            CALL CSMsgBox("", 0, Pass)
        Else

            REDIM leagueLeaderFiles$(0 TO Count%)
            FOR X = 1 TO Count%
              leagueLeaderFiles$(X) = SPACE$(12)
            NEXT
            leagueLeaderFiles$(0) = Spec$
            CALL ReadFile(leagueLeaderFiles$())

            Do
                CLS
                COLOR , 1
                CLS
                LOCATE , 28
                COLOR 14
                PRINT "SELECT LEAGUE TO PRINT": COLOR 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15:FileBGColr% = 1
                HiLiteFGColr% = 0:HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                LOCATE 25, 1
                COLOR 14
                PRINT "ENTER"; : COLOR 15
                PRINT "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueLeaderFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                IF ExitCode% <> 27 THEN

					COLOR 15, 1
                    targetFile$ = leagueLeaderFiles$(Selection%)
                    LOCATE 2, 33: PRINT targetFile$

					Do
						LOCATE , 33: PRINT "ANY CHANGE (YN)";
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
					
					Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                WE$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
                WE$ = LEFT$(WE$, 8)

                OPEN diskPaths$(2) + WE$ + ".TMS" FOR INPUT AS #2
                INPUT #2, LG$, D1$, D2$, D3$, D4$
                CLOSE 2

                H$ = " 1/2"
                CLS

                LOCATE 9, 1: PRINT "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

                OPEN diskPaths$(2) + WE$ + ".LDR" FOR INPUT AS #2
                FOR X = 1 TO 40: INPUT #2, T$(X), W(X), L(X): NEXT
                FOR X = 1 TO 298: INPUT #2, Z2$(X), Z3$(X), Z!(X), Z1!(X): NEXT
                FOR X = 1 TO 40: INPUT #2, BA$(X): FOR M = 1 TO 14: INPUT #2, OFT!(X, M): NEXT: NEXT
                FOR X = 1 TO 40: INPUT #2, ER$(X): FOR M = 1 TO 14: INPUT #2, DFT!(X, M): NEXT: NEXT
                CLOSE #2

                D1 = W(1) - L(1)

                FOR P = 1 TO 10: 
                    IF T$(P) = "*" THEN 
                        P$(P) = ""
                    Else

                        P$(P) = T$(P)
                        L = 12

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = W(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 16

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = L(P)
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 21

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))
                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        D = W(P) - L(P)
                        D = D1 - D
                        BK$ = STR$(INT(D / 2))

                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

                        P$(P) = P$(P) + BK$
                    End If

                NEXT P

                D1 = W(11) - L(11)

                FOR P = 11 TO 20: 
                    IF T$(P) = "*" THEN 
                        P$(P) = ""
                    Else

                        P$(P) = T$(P)
                        L = 12

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = W(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 16

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = L(P)
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 21

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))
                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2)): IF D = 0 THEN BK$ = "---"

                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

                        P$(P) = P$(P) + BK$
                    End If
                NEXT P

                D1 = W(21) - L(21)

                FOR P = 21 TO 30
                    IF T$(P) = "*" THEN 
                        P$(P) = ""
                    Else
                        P$(P) = T$(P)
                        L = 12

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = W(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 16

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = L(P)
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 21

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))
                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        D = W(P) - L(P)
                        D = D1 - D
                        BK$ = STR$(INT(D / 2))

                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

                        P$(P) = P$(P) + BK$
                    End If
                NEXT P

                D1 = W(31) - L(31)

                FOR P = 31 TO 40
                    IF T$(P) = "*" THEN 
                        P$(P) = ""
                    Else
                        P$(P) = T$(P)
                        L = 12

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = W(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 16

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = L(P)
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        L = 21

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))
                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        D = W(P) - L(P)
                        D = D1 - D
                        BK$ = STR$(INT(D / 2))

                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

                        P$(P) = P$(P) + BK$
                    End If
                NEXT P

                CLS : '****NOW TO BUILD THE STRINGS FOR PRINTING****

                LF$(1) = "    " + LG$ + " LEAGUE STANDINGS AS OF " + DATE$
                LF$(3) = "    " + D1$ + "      W   L   PCT   GB"

                FOR P = 1 TO 10: 
                    IF P$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        LF$(P + 3 - RT) = "    " + P$(P)
                    End If
                NEXT P

                RT = 0

                LF$(15) = "    " + D3$ + "      W   L   PCT   GB"

                FOR P = 21 TO 30: 
                    IF P$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        LF$(P - 5 - RT) = "    " + P$(P)
                    End If
                NEXT P

                RT = 0

                RT$(3) = "    " + D2$ + "      W   L   PCT   GB"

                FOR P = 11 TO 20
                    IF P$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        RT$(P - 7 - RT) = "    " + P$(P)
                    End If
                NEXT P

                RT = 0

                RT$(15) = "    " + D4$ + "      W   L   PCT   GB"

                FOR P = 31 TO 40
                    IF P$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        RT$(P - 15 - RT) = "    " + P$(P)
                    End If
                NEXT P

                FOR P = 2 TO 26
                    IF LEN(LF$(P)) < 38 THEN 
                        LF$(P) = LF$(P) + " "
                    Else
                        IF LEN(RT$(P)) < 38 THEN 
                            RT$(P) = RT$(P) + " "
                        End If
                    End If
                NEXT P

                FOR X = 1 TO 26: LPRINT LF$(X); RT$(X): NEXT

                AB = INT(AB)
                AB$ = STR$(AB)
                TD = IP - 3 * IG
                L = LEN(AB$)
                AB$ = RIGHT$(AB$, L - 1)
                IF TD > 0 THEN TD$ = " 1/3"
                IG$ = STR$(IP)
                L = LEN(IG$)
                IG$ = RIGHT$(IG$, L - 1)
                IP$ = IG$

                FOR P = 1 TO 20
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 27

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "

                        J! = Z1!(P)

                        P$(P) = P$(P) + " " + STR$(J)
                    End If
                NEXT P

                CLS

                LF$(27) = "   BATTING LEADERS  (MIN 3.1 PA/G)"
                FOR P = 1 TO 19: LF$(P + 27) = "   " + P$(P): NEXT
                LF$(47) = " "

                FOR P = 239 TO 258
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 27

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "

                        J! = Z1!(P)

                        P$(P) = P$(P) + " " + STR$(J!)
                    End If
                NEXT P

                CLS

                RT$(27) = "       BATTING TRAILERS  (MIN 3.1 PA/G)"
                FOR P = 239 TO 258: RT$(P - 211) = "   " + P$(P): NEXT

                RT$(47) = " "

                FOR P = 27 TO 47
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 27 TO 47: LPRINT LF$(X); RT$(X): NEXT

                FOR P = 21 TO 30
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(49) = "   HITS"
                FOR P = 21 TO 30: LF$(P + 29) = "   " + P$(P): NEXT P

                FOR P = 32 TO 41
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3

                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(49) = "       DOUBLES"
                FOR P = 32 TO 41: RT$(P + 18) = "       " + P$(P): NEXT P

                FOR P = 49 TO 59
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 49 TO 59: LPRINT LF$(X); RT$(X): NEXT
                LPRINT CHR$(12)

                FOR P = 43 TO 52
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(1) = "   TRIPLES"
                FOR P = 43 TO 52: LF$(P - 41) = "   " + P$(P): NEXT P

                LF$(12) = " "

                FOR P = 54 TO 63: 
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(1) = "     HOME RUNS"
                FOR P = 54 TO 63: RT$(P - 52) = "     " + P$(P): NEXT P
                RT$(12) = " "

                FOR P = 1 TO 12
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 1 TO 12: LPRINT LF$(X); RT$(X): NEXT

                FOR P = 65 TO 73
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(13) = "   RUNS"
                FOR P = 65 TO 73: LF$(P - 51) = "   " + P$(P): NEXT P
                LF$(23) = " "

                FOR P = 76 TO 84
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(13) = "     RUNS BATTED IN"
                FOR P = 76 TO 84: RT$(P - 62) = "     " + P$(P): NEXT P: 
                RT$(23) = " "

                FOR P = 13 TO 23
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 13 TO 23: LPRINT LF$(X); RT$(X): NEXT

                FOR P = 87 TO 96
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(24) = "   STOLEN BASES"
                FOR P = 87 TO 96: LF$(P - 62) = "   " + P$(P): NEXT P
                LF$(35) = " "

                FOR P = 99 TO 108
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "
                    End If
                NEXT P

                RT$(24) = "     SLUGGING PCT LEADERS"
                FOR P = 99 TO 108: RT$(P - 74) = "     " + P$(P): NEXT P: 
                RT$(35) = " "

                FOR P = 24 TO 35
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 24 TO 35: LPRINT LF$(X); RT$(X): NEXT

                FOR P = 110 TO 119
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN P$(P) = P$(P) + "1.000 "
                    End If
                NEXT P

                LF$(36) = "   ON BASE PCT LEADERS"
                FOR P = 110 TO 119
                    LF$(P - 73) = "   " + P$(P)
                NEXT P
                LF$(47) = " "

                FOR P = 217 TO 226
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(36) = "     BASE ON BALLS"
                FOR P = 217 TO 226
                    RT$(P - 180) = "    " + P$(P)
                NEXT P
                RT$(47) = " "

                FOR P = 36 TO 47
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend
                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 36 TO 47
                    LPRINT LF$(X); RT$(X)
                NEXT

                FOR P = 279 TO 288
                    P$(P) = ""

                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(48) = "   CURRENT HITTING STREAK"

                FOR P = 279 TO 288: LF$(P - 230) = "   " + P$(P): NEXT P

                FOR P = 289 TO 298: 
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 28

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(48) = "       LONGEST HITTING STREAK"
                FOR P = 289 TO 298: RT$(P - 240) = "       " + P$(P): NEXT P

                FOR P = 48 TO 58
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 48 TO 58: LPRINT LF$(X); RT$(X): NEXT
                LPRINT CHR$(12)

                FOR P = 121 TO 139
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)

                        J! = INT(100 * J! + .5)
                        A$ = STR$(J)
                        LN = LEN(A$)
                        A$ = RIGHT$(A$, LN - 1)

                        '-- couldn't we just do Print Using???
                        While LEN(A$) < 3
                            LN = LEN(A$)
                            A$ = "0" + A$
                        Wend

                        A$ = LEFT$(A$, LN - 2) + "." + RIGHT$(A$, 2)
                        A$ = "    " + A$
                        A$ = RIGHT$(A$, 5)
                        P$(P) = P$(P) + A$
                        J! = INT(Z1!(P))

                        P$(P) = P$(P) + " " + STR$(J!)
                    End If
                NEXT P

                RT$(27) = "     ERA LEADERS (MIN 1 IP/G)"

                FOR P = 121 TO 139
                    RT$(P - 93) = "    " + P$(P)
                NEXT

                FOR P = 140 TO 149
                    P$(P) = ""
                    IF Z!(P) <> Z!(150) Or Z1!(P) <> Z1!(150) THEN
                        P$(P) = P$(P) + Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 23

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 2
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                        P$(P) = P$(P) + "-"
                        J! = Z1!(P)
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(48) = "   PITCHING RECORD"

                FOR P = 140 TO 149
                    LF$(P - 91) = "   " + P$(P)
                NEXT

                LF$(59) = " "

                FOR P = 151 TO 160: P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(48) = "     VICTORIES"

                FOR P = 151 TO 160
                    RT$(P - 102) = "     " + P$(P)
                NEXT P

                RT$(59) = " "
                FOR P = 48 TO 59
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 48 TO 59: LPRINT LF$(X); RT$(X): NEXT

                LPRINT CHR$(12)

                FOR P = 162 TO 171
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = INT(Z!(P))
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(1) = "   INNINGS PITCHED"

                FOR P = 162 TO 171: LF$(P - 160) = "   " + P$(P): NEXT P

                LF$(12) = " "

                FOR P = 173 TO 182: 
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)

                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(1) = "     STRIKEOUTS"

                FOR P = 173 TO 182: RT$(P - 171) = "     " + P$(P): NEXT P
                RT$(12) = " "

                FOR P = 1 TO 12
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 1 TO 12: LPRINT LF$(X); RT$(X): NEXT

                FOR P = 184 TO 193
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(13) = "   COMPLETE GAMES"
                FOR P = 184 TO 193: LF$(P - 170) = "   " + P$(P): NEXT P
                LF$(24) = " "

                FOR P = 195 TO 204: P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                RT$(13) = "     SHUTOUTS"

                FOR P = 195 TO 204: RT$(P - 181) = "     " + P$(P): NEXT P
                RT$(24) = " "

                FOR P = 13 TO 24

                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38 
                        RT$(P) = RT$(P) + " "
                    Wend

                NEXT P

                FOR X = 13 TO 24: LPRINT LF$(X); RT$(X): NEXT

                FOR P = 206 TO 215
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P): IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                LF$(25) = "   SAVES"

                FOR P = 206 TO 215: LF$(P - 180) = "   " + P$(P): NEXT P

                LF$(36) = " "
                RT$(25) = "     APPEARANCES"

                FOR P = 228 TO 237
                    P$(P) = ""
                    IF Z2$(P) <> "*" THEN
                        P$(P) = Z2$(P)
                        IF Z3$(P) <> "*" THEN P$(P) = P$(P) + ", " + Z3$(P)
                        L = 24

                        While LEN(P$(P)) < L 
                            P$(P) = P$(P) + " "
                        Wend

                        J! = Z!(P)
                        W = 3
                        A$ = "     " + STR$(J)
                        A$ = RIGHT$(A$, W)
                        P$(P) = P$(P) + A$
                    End If
                NEXT P

                FOR P = 228 TO 237
                    RT$(P - 202) = "     " + P$(P)
                NEXT P
                RT$(36) = " "

                FOR P = 25 TO 36
                    While LEN(LF$(P)) < 35 
                        LF$(P) = LF$(P) + " "
                    Wend

                    While LEN(RT$(P)) < 38
                        RT$(P) = RT$(P) + " "
                    Wend
                NEXT P

                FOR X = 25 TO 36: LPRINT LF$(X); RT$(X): NEXT
                LPRINT CHR$(12)
                LPRINT "                                 CLUB BATTING   "
                LPRINT : LPRINT "CLUB         AVG   AB    R    H   2B   3B  HR  RBI  BB   SO   SB   CS  E"

                FOR X = 1 TO 40
                    IF BA$(X) <> "" AND LEFT$(BA$(X), 1) <> "*" AND BA$(X) <> "*" THEN
                        LPRINT LEFT$(BA$(X), 11); TAB(13);
                        OFT!(X, 1) = INT(OFT!(X, 1) * 1000) / 1000
                        LPRINT USING ".###"; OFT!(X, 1);
                        LPRINT TAB(18); OFT!(X, 3); TAB(24); OFT!(X, 5); TAB(29); STR$(OFT!(X, 4)); TAB(34); OFT!(X, 7); TAB(39); OFT!(X, 8); TAB(43); STR$(OFT!(X, 9)); TAB(47); OFT!(X, 6); TAB(52); OFT!(X, 10);
                        LPRINT TAB(57); OFT!(X, 11); TAB(62); OFT!(X, 12); TAB(67); OFT!(X, 13); TAB(72); OFT!(X, 14)
                    End If
                NEXT

                LPRINT CHR$(12)
                LPRINT "                                 CLUB PITCHING  "
                LPRINT : LPRINT "CLUB         ERA  G    CG SHO  SV  IP    H    R   ER   BB   SO    "

                FOR X = 1 TO 40
                    IF ER$(X) <> "" AND  ER$(X) <> "*" THEN
                        LPRINT LEFT$(ER$(X), 11); TAB(13);
                        DFT!(X, 1) = INT(DFT!(X, 1) * 100) / 100
                        LPRINT USING "##.##"; DFT!(X, 1);
                        LPRINT TAB(18); DFT!(X, 11); TAB(23); DFT!(X, 12); TAB(27); DFT!(X, 13); TAB(31); DFT!(X, 14); TAB(35); STR$(CINT(DFT!(X, 2) / 3)); TAB(40); STR$(DFT!(X, 3)); TAB(45); DFT!(X, 4); TAB(50); DFT!(X, 5);
                        LPRINT TAB(55); DFT!(X, 6); TAB(60); DFT!(X, 7)
                    End If
                NEXT

                LPRINT CHR$(12)

            End If 'Done checking for ESC

        End If  'Done checking file count

    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or I$ = "N"

End Sub

'   181 L = 8: IF LEN(D1$) < L THEN D1$ = D1$ + " ": GOTO 181
'   191 L = 8: IF LEN(D2$) < L THEN D2$ = D2$ + " ": GOTO 191
'   201 L = 8: IF LEN(D3$) < L THEN D3$ = D3$ + " ": GOTO 201
'   211 L = 8: IF LEN(D4$) < L THEN D4$ = D4$ + " ": GOTO 211
'   221 Return

'   231 FOR P = 1 TO 5: P$(P) = "": IF NM$(CT, P, 1) = "*" THEN 261
'   241 P$(P) = NM$(CT, P, 1): IF NM$(CT, P, 2) <> "*" THEN P$(P) = P$(P) + ", " + NM$(CT, P, 2)
'   251 L = 22: GOSUB 271: J! = NM%(CT, P): W = 3: GOSUB 391
'   261 NEXT P
'   Return

'   351 A$ = STR$(J): L = LEN(A$): A$ = RIGHT$(A$, L - 1)
'   361 IF LEN(A$) < 3 THEN A$ = "0" + A$: GOTO 361
'   371 A$ = " ." + RIGHT$(A$, 3)
'   381 P$(P) = P$(P) + A$
'   Return

'2741 '
'CP$ = "": L = LEN(LW$)
'FOR Z = 1 TO L:
'   ALP = ASC(MID$(LW$, Z, 1))
'   IF ALP > 64 AND ALP < 96 THEN 
'       CP$ = CP$ + CHR$(ALP + 32)
'   Else
'       CP$ = CP$ + CHR$(ALP)
'   End If
'NEXT Z
'Return

'2781 '
'HZ = 1: VT = 24
'LOCATE VT, HZ: PRINT Space$(26);
'QK$=GetKeyPress$
'Return
