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
            'CALL SortStr(VARPTR(leagueLeaderFiles$(1)), Count%, 0)

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
                    targetFile$ = leagueLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$
                        
                    Locate , 33: Print "ANY CHANGE (YN)";
                    Color 15, 0
                    I$ = GetKeyPress$
                End If
            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
            
                targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                WE$ = Left$(WE$, 8)

                Open diskPaths$(2) + WE$ + ".TMS" For Input As #2
                Input #2, LG$, D1$, D2$, D3$, D4$
                Close 2

                L = 8
                While Len(D1$) < L: D1$ = D1$ + " ": Wend
                While Len(D2$) < L: D2$ = D2$ + " ": Wend
                While Len(D3$) < L: D3$ = D3$ + " ": Wend
                While Len(D4$) < L: D4$ = D4$ + " ": Wend

                H$ = " 1/2"
                Cls
                Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

                Open diskPaths$(2) + WE$ + ".LDR" For Input As #2
                20 For X = 1 To 40: Input #2, T$(X), W(X), L(X): Next
                30 For X = 1 To 298: Input #2, cmplZ2$(X), cmplZ3$(X), Z!(X), Z1!(X): Next
                40 For X = 1 To 40: Input #2, BA$(X): For M = 1 To 14: Input #2, OFT!(X, M): Next: Next
                41 For X = 1 To 40: Input #2, ER$(X): For M = 1 To 14: Input #2, DFT!(X, M): Next: Next
                Input #2, APP

                Close #2

                '-----
                D1 = W(1) - L(1)

                FOR P = 1 TO 10
                    IF T$(P) = "*" THEN 
                        seeP$(P) = ""
                    Else
                        seeP$(P) = T$(P)

                        L = 12
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = W(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 16                      
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = L(P)
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 21
                        GOSUB 900
                        
                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                        
                        L = 28
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        D = W(P) - L(P)
                        D = D1 - D
                        BK$ = STR$(INT(D / 2))
                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
                        seeP$(P) = seeP$(P) + BK$
                    End If
                NEXT P


                '-----
                D1 = W(11) - L(11)

                FOR P = 11 TO 20
                    IF T$(P) <> "*" THEN 
                        seeP$(P) = ""
                    Else

                        seeP$(P) = T$(P)
                        
                        L = 12
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = W(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 16
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = L(P)
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 21
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                        
                        L = 28
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        D = W(P) - L(P)
                        D = D1 - D
                        BK$ = STR$(INT(D / 2))
                            
                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$

                        seeP$(P) = seeP$(P) + BK$
                    End If
                NEXT P


                '-----
                D1 = W(21) - L(21)

                FOR P = 21 TO 30
                    IF T$(P) = "*" THEN 
                        seeP$(P) = ""
                    Else
                        seeP$(P) = T$(P)
                        
                        L = 12
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = W(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 16
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = L(P)
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 21
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))

                        J! = INT(1000 * J! + .5)
                        IF J = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                        
                        L = 28
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        D = W(P) - L(P)
                        D = D1 - D
                        BK$ = STR$(INT(D / 2))
                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
                        seeP$(P) = seeP$(P) + BK$
                    End If
                    
                NEXT P


                '-----
                D1 = W(31) - L(31)

                FOR P = 31 TO 40
                    IF T$(P) = "*" THEN 
                        seeP$(P) = ""
                    Else

                        seeP$(P) = T$(P)
                        
                        L = 12
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = W(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 16
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = L(P)
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        
                        L = 21
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = 0
                        IF W(P) + L(P) > 0 THEN J! = W(P) / (W(P) + L(P))

                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                        
                        L = 28
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend


                        D = W(P) - L(P): D = D1 - D: BK$ = STR$(INT(D / 2))
                        
                        IF D = 0 THEN BK$ = "---"
                        IF D = 1 THEN BK$ = "  "
                        IF D > 19 THEN L = LEN(BK$): BK$ = RIGHT$(BK$, L - 1)
                        IF D / 2 <> INT(D / 2) THEN BK$ = BK$ + H$
                        seeP$(P) = seeP$(P) + BK$
                    End If
                NEXT P

                '-----
                CLS

                COLOR 12: PRINT "                        "; LG$; " LEAGUE STANDINGS": PRINT
                COLOR 14

                LOCATE 3, 1: PRINT "    " + D1$ + "      W   L   PCT   GB"
                COLOR 15

                FOR P = 1 TO 10
                    IF seeP$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        LOCATE (P + 3 - RT), 1: PRINT "    "; seeP$(P)
                    End If
                NEXT P

                PRINT


                RT = 0
                COLOR 14
                LOCATE 15, 1: PRINT "    " + D3$ + "      W   L   PCT   GB"
                COLOR 15
                
                FOR P = 21 TO 30
                    IF seeP$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        LOCATE (P - 5 - RT), 1: PRINT "    "; seeP$(P)
                    End If
                NEXT P


                RT = 0
                COLOR 14: LOCATE 3, 41: PRINT "    " + D2$ + "      W   L   PCT   GB"
                COLOR 15
                FOR P = 11 TO 20
                    IF seeP$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        LOCATE (P - 7 - RT), 41: PRINT "    "; seeP$(P)
                    End If
                NEXT P

                PRINT

                
                RT = 0
                COLOR 14: LOCATE 15, 41: PRINT "    " + D4$ + "      W   L   PCT   GB"
                COLOR 15
                FOR P = 31 TO 40
                    IF seeP$(P) = "" THEN 
                        RT = RT + 1
                    Else
                        LOCATE (P - 15 - RT), 41: PRINT "    "; seeP$(P)
                    End If
                NEXT P

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

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

                FOR P = 1 TO 19
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "*" THEN

                        seeP$(P) = cmplZ2$(P)

                        IF cmplZ3$(P) <> "*" THEN
                            seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                        END IF

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = Z!(P)
                        
                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                        
                        J! = Z1!(P)
                        seeP$(P) = seeP$(P) + "     " + STR$(J!)
                    End If
                NEXT P
                
                '-----
                CLS

                COLOR 14
                PRINT "      BATTING LEADERS  (MIN 3.1 PA/G)"
                COLOR 15
                PRINT "WHO'S HOT                   AVG   AB    AND WHO'S NOT                AVG   AB"

                FOR P = 1 TO 19
                    PRINT cmplZ2$(P) + ", " + cmplZ3$(P); TAB(28); USING ".###  ###"; Z!(P); Z1!(P);
                    PRINT TAB(41); cmplZ2$(P + 238) + ", " + cmplZ3$(P + 238); TAB(69);
                    PRINT USING ".###  ###"; Z!(P + 238); Z1!(P + 238)
                NEXT
                
                PRINT

                FOR P = 21 TO 30
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                '-----
                CLS
                COLOR 14: PRINT "      HITS"
                COLOR 15
                FOR P = 21 TO 30: PRINT "      "; seeP$(P): NEXT P: PRINT

                FOR P = 32 TO 41
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P


                COLOR 14: PRINT "      DOUBLES"
                COLOR 15

                FOR P = 32 TO 41: PRINT "      "; seeP$(P): NEXT P

                FOR P = 43 TO 52: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                        
                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                COLOR 14: LOCATE 1, 40: PRINT "      TRIPLES"
                COLOR 15

                FOR P = 43 TO 52: LOCATE (P - 41), 40: PRINT "      "; seeP$(P): NEXT P
                PRINT

                FOR P = 54 TO 63: seeP$(P) = ""
                    IF cmplZ2$(P) = "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                COLOR 14: LOCATE 13, 52: PRINT "   HOME RUNS"
                COLOR 15

                FOR P = 54 TO 63: LOCATE (P - 40), 40: PRINT "      "; seeP$(P): NEXT P

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                FOR P = 65 TO 73
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN

                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P
                
                '-----
                CLS
                COLOR 14: PRINT "      RUNS"
                COLOR 15

                FOR P = 65 TO 73: PRINT "      "; seeP$(P): NEXT P
                PRINT

                FOR P = 76 TO 84: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                PRINT
                COLOR 14: PRINT "      RUNS BATTED IN"
                COLOR 15

                FOR P = 76 TO 84: PRINT "      "; seeP$(P): NEXT P

                FOR P = 87 TO 96
                    seeP$(P) = ""
                    IF cmplZ2$(P) = "" OR cmplZ2$(P) = "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P


                COLOR 14: LOCATE 1, 40: PRINT "      STOLEN BASES"
                COLOR 15

                FOR P = 87 TO 96: LOCATE (P - 85), 40: PRINT "      "; seeP$(P): NEXT P

                FOR P = 98 TO 107: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        J! = Z!(P)
                        
                        J! = INT(1000 * J! + .5)
                        IF J! = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                    End If
                NEXT P

                COLOR 14: LOCATE 13, 40: PRINT "      SLUGGING PCT LEADERS"
                COLOR 15

                FOR P = 99 TO 108: LOCATE (P - 85), 40: PRINT "      "; seeP$(P): NEXT P

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                FOR P = 110 TO 119
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN

                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P)
                        J! = INT(1000 * J! + .5)

                        IF J! = 1000 THEN 
                            seeP$(P) = seeP$(P) + "1.000 "
                        Else
                             IF J! > 1000 THEN
                                A$ = STR$(J! - 1000)
                                L = LEN(A$)
                                A$ = RIGHT$(A$, L - 1)
                                While LEN(A$) < 3: A$ = "0" + A$: Wend
                                A$ = "1." + A$
                                seeP$(P) = seeP$(P) + A$
                            END IF
                        End If
                    End If
                NEXT P
                
                '-----
                CLS
                COLOR 14: PRINT "ON BASE PCT LEADERS"
                COLOR 15

                FOR P = 110 TO 119
                    PRINT seeP$(P)
                NEXT P
                
                PRINT

                FOR P = 217 TO 226: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
                        
                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                COLOR 14: LOCATE 1, 41: PRINT "BASE ON BALLS"
                COLOR 15

                FOR P = 217 TO 226: LOCATE (P - 215), 41: PRINT seeP$(P): NEXT P

                LOCATE 13, 1
                COLOR 14: PRINT "CURRENT HITTING STREAK"
                COLOR 15
                
                FOR P = 279 TO 288
                    PRINT cmplZ2$(P)+", "+cmplZ3$(P); TAB(28); USING "###";Z!(P)
                NEXT P
                    
                LOCATE 13, 41
                COLOR 14: PRINT "LONGEST HITTING STREAK"
                COLOR 15
                
                FOR P = 289 TO 298
                    LOCATE, 41:PRINT cmplZ2$(P)+", "+cmplZ3$(P); TAB(68); USING "###";Z!(P)
                NEXT P
                    
                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                FOR P = 120 TO 138
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P)
                        J! = Int(100 * J! + .5)
                        A$ = Str$(J!)
                        LN = Len(A$)
                        A$ = Right$(A$, LN - 1)

                        LN = LEN(A$)
                        WHILE LEN(A$) < 3: A$ = "0" + A$: WEND

                        A$ = LEFT$(A$, LN - 2) + "." + RIGHT$(A$, 2)
                        A$ = "    " + A$: A$ = RIGHT$(A$, 5)
                        seeP$(P) = seeP$(P) + A$                        
                        J! = INT(Z1!(P))
                        seeP$(P) = seeP$(P) + "     " + STR$(J!)
                    End If
                NEXT P

                '-----
                CLS
                COLOR 14
                PRINT "       ERA LEADERS (MIN 1 IP/G)"
                COLOR 15
                PRINT "WHO'S HOT                     ERA   IP  WHO'S NOT                      ERA  IP"

                FOR P = 121 TO 139
                    PRINT cmplZ2$(P) + ", " + cmplZ3$(P); TAB(30); USING "#.##  ###"; Z!(P); Z1!(P);
                    PRINT TAB(41);
                    PRINT cmplZ2$(P + 138) + ", " + cmplZ3$(P + 138); TAB(70); USING "##.## ###"; Z!(P + 138); Z1!(P + 138)
                NEXT
                
                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                FOR P = 140 TO 149
                    seeP$(P) = ""
                    IF Z!(P) <> Z!(150) Or Z1!(P) <> Z1!(150) THEN
                        seeP$(P) = seeP$(P) + cmplZ2$(P)
                        IF cmplZ3$(P) <> "" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = Z!(P): W = 2
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                        seeP$(P) = seeP$(P) + "-"
                        J! = Z1!(P)
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                '-----
                CLS
                COLOR 14
                PRINT "      PITCHING RECORD"
                COLOR 15

                FOR P = 140 TO 149
                    PRINT "      "; seeP$(P)
                NEXT
                
                PRINT

                FOR P = 151 TO 160
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "*" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                COLOR 14: PRINT "      VICTORIES"
                COLOR 15

                FOR P = 151 TO 160: PRINT "      "; seeP$(P): NEXT P

                FOR P = 162 TO 171: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN

                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = INT(Z!(P)): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                LOCATE 1, 40: COLOR 14: PRINT "      INNINGS PITCHED"
                COLOR 15

                FOR P = 162 TO 171: LOCATE (P - 160), 40: PRINT "      "; seeP$(P): NEXT P

                FOR P = 173 TO 182
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend

                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                COLOR 14: LOCATE 13, 40: PRINT "      STRIKEOUTS     "
                COLOR 15

                FOR P = 173 TO 182: LOCATE (P - 159), 40: PRINT "      "; seeP$(P): NEXT P

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                FOR P = 184 TO 193: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                '-----
                CLS
                COLOR 14: PRINT "      COMPLETE GAMES "
                COLOR 15

                FOR P = 184 TO 193: PRINT "      "; seeP$(P): NEXT P: PRINT

                FOR P = 195 TO 204: seeP$(P) = ""
                    IF cmplZ2$(P) = "" OR cmplZ2$(P) = "" THEN
                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                COLOR 14: PRINT "      SHUTOUTS       "
                COLOR 15

                FOR P = 195 TO 204: PRINT "      "; seeP$(P): NEXT P

                FOR P = 206 TO 215
                    seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN

                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3: GOSUB 940
                    End If
                NEXT P

                LOCATE 1, 40: COLOR 14: PRINT "      SAVES          "
                COLOR 15

                FOR P = 206 TO 215: LOCATE (P - 204), 40: PRINT "      "; seeP$(P): NEXT P

                COLOR 14: LOCATE 13, 40: PRINT "      APPEARANCES    "
                COLOR 15

                FOR P = 228 TO 237: seeP$(P) = ""
                    IF cmplZ2$(P) <> "" THEN

                        seeP$(P) = cmplZ2$(P)
                        IF cmplZ3$(P) <> "*" THEN seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

                        L = 26
                        While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
                        
                        J! = Z!(P): W = 3
                        A$ = "     " + STR$(J!)
                        A$ = RIGHT$(A$, W)
                        seeP$(P) = seeP$(P) + A$
                    End If
                NEXT P

                FOR P = 228 TO 237: LOCATE (P - 214), 40: PRINT "      "; seeP$(P): NEXT P

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

                '-----
                CLS 
                COLOR 12: PRINT "                                 CLUB BATTING   ": COLOR 14

                PRINT "CLUB         AVG   AB    R    H   2B   3B  HR   RBI   BB   SO  SB  CS   E"
                COLOR 15
                H = 0: AB = 0: R = 0: B2% = 0: B3 = 0: HR = 0: RBI = 0: BB = 0: SO = 0: SB = 0: CS = 0: E = 0

                FOR X = 1 TO 40
                    IF BA$(X) <> "" AND BA$(X) <> "*" THEN
                        PRINT LEFT$(BA$(X), 11); TAB(13);
                        PRINT USING ".### #### #### ####  ###  ### ###  #### #### #### ### ### ###"; OFT!(X, 1); OFT!(X, 3); OFT!(X, 5); OFT!(X, 4); OFT!(X, 7); OFT!(X, 8); OFT!(X, 9); OFT!(X, 6); OFT!(X, 10); OFT!(X, 11); OFT!(X, 12); OFT!(X, 13); OFT!(X, 14)
                        ATBATS! = ATBATS! + OFT!(X, 3): H = H + OFT!(X, 4): R = R + OFT!(X, 5)
                        B2 = B2 + OFT!(X, 7): B3 = B3 + OFT!(X, 8): HR = HR + OFT!(X, 9)
                        RBI = RBI + OFT!(X, 6): BB = BB + OFT!(X, 10): SO = SO + OFT!(X, 11): SB = SB + OFT!(X, 12): CS = CS + OFT!(X, 13): E = E + OFT!(X, 14)
                    End If
                NEXT

                PRINT
                PRINT "LG TOTALS"; TAB(13); USING ".###      ####      ####  ###      ####     #####    ####"; H / ATBATS!; R; B2; B3; RBI; SO; CS
                PRINT TAB(17); USING "#####     #####          ####       ####     ####    ####"; ATBATS!; H; HR; BB; SB; E

                HZ = 1: VT = 24
                LOCATE VT, HZ
                PRINT Space$(26);
                QK$ = GetKeyPress$

                '-----
                CLS
                COLOR 12: PRINT "                                 CLUB PITCHING  ": COLOR 14

                PRINT "CLUB          ERA   G  CG  SHO  SV    IP     H     R    ER     BB     SO"
                COLOR 15

                R = 0: G = 0: IP = 0
                
                FOR X = 1 TO 40
                    IF ER$(X) <> "" And ER$(X) <> "*" THEN
                        R = R + DFT!(X, 5)
                        G = G + DFT!(X, 12)
                        INNINGS! = INNINGS! + DFT!(X, 2)

                        PRINT LEFT$(ER$(X), 11); TAB(13);

                        DFT!(X, 1) = INT(DFT!(X, 1) * 100) / 100
                        
                        PRINT USING "##.##"; DFT!(X, 1);
                        PRINT TAB(19); USING "### ### ###  ###  ####  ####  ####  ####"; DFT!(X, 11); DFT!(X, 12); DFT!(X, 13); DFT!(X, 14); CINT(DFT!(X, 2) / 3); DFT!(X, 3); DFT!(X, 4); DFT!(X, 5);
                        PRINT USING "   ####   ####"; DFT!(X, 6); DFT!(X, 7)
                    End If
                NEXT

                PRINT TAB(13); USING "##.##"; R / (INNINGS! / 3) * 9
                'PRINT "COMPLETE GAMES: "; G
                'PRINT "APPEARANCES: "; APP

                HZ = 1: VT = 24
                LOCATE VT, HZ: PRINT Space$(26);
                QK$ = GetKeyPress$

            End If 'Done checking exit code

        End If 'Done checking file count
        
    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or I$ = "N"

End Sub


'410 '
'For P = 1 To 5
'   seeP$(P) = ""
'   IF NM$(CT, P, 1) <> "*" Then
'       seeP$(P) = NM$(CT, P, 1)
'       IF NM$(CT, P, 2) <> "*" Then seeP$(P) = seeP$(P) + ", " + NM$(CT, P, 2)
'       L = 22
'       While Len(seeP$(P)) < L: seeP$(P) = seeP$(P) + " ": Wend
'       J! = NM%(CT, P): W = 3
'       A$ = "     " + STR$(J!)
'       A$ = RIGHT$(A$, W)
'       seeP$(P) = seeP$(P) + A$
'   End If
'Next P
'Return


'5000 '
'CP$ = ""
'L = LEN(LW$)
'FOR Z = 1 TO L
'   ALP = ASC(MID$(LW$, Z, 1))
'   IF ALP > 64 AND ALP < 96 THEN 
'       CP$ = CP$ + CHR$(ALP + 32)
'   Else
'       CP$ = CP$ + CHR$(ALP)
'   End If
'NEXT Z
'Return


'931 '
'A$ = STR$(J!)
'L = LEN(A$)
'A$ = RIGHT$(A$, L - 1)
'While LEN(A$) < 3: A$ = "0" + A$: Wend
'A$ = " ." + RIGHT$(A$, 3)
'seeP$(P) = seeP$(P) + A$: Return
'A$ = "     " + STR$(J!)
'A$ = RIGHT$(A$, W)
'seeP$(P) = seeP$(P) + A$
'Return

