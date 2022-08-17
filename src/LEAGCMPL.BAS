Sub LEAGCMPL ()

    _Title "Full Count Baseball - Update League Stats"

    DefInt A-Z

    Dim H1$(40), TM$(40)

    Dim ZM%(46)

    Dim O1(40), O2(40)

    Shared BAT%(), PIT%()
    Shared cmplZ2$(), cmplZ3$()
    Shared BA$(), ER$(), NM$()
    Shared cmplZ1!(), DFT!(), JS!(), OFT!()

    CLS

    APP = 0

    Do 
    'Display list of files...

        Spec$ = diskPaths$(2) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%
        IF Count% = 0 THEN 
            LOCATE 10, 8
            CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            CALL CSMsgBox("", 0, Pass)
        Else

            REDIM leagueFiles$(0 TO Count%)

            FOR X = 1 TO Count%
              leagueFiles$(X) = SPACE$(12)
            NEXT

            leagueFiles$(0) = Spec$
            CALL ReadFile(leagueFiles$())

            Do
                CLS
                COLOR , 1
                CLS
                LOCATE , 28
                COLOR 14
                PRINT "SELECT LEAGUE TO COMPILE": COLOR 15
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
                PRINT "-SELECTS TEAM";

                _MouseShow "DEFAULT"
                MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                IF ExitCode% <> 27 THEN
                    targetFile$ = leagueFiles$(Selection%)
                    LOCATE 2, 33: PRINT targetFile$
                    LOCATE  , 33: PRINT "ANY CHANGE (YN)";
                    COLOR 15, 0
                    I$ = GetKeyPress$
                End If
                    
            Loop Until UCase$(I$) = "N"

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                WE$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
                LH$ = LEFT$(WE$, 8)

                C$ = ","

                FOR Z = 1 TO 40
                    DFT!(Z, 1) = 99.99
                NEXT

                CLS

                targetFile$ = LH$ + ".TMS"

                OPEN diskPaths$(2) + targetFile$ FOR INPUT AS #2
                INPUT #2, LG$, D1$, D2$, D3$, D4$
                FOR XW = 1 TO 40
                    INPUT #2, H1$(XW)
                NEXT
                CLOSE 2

                C$ = ","

                OPEN diskPaths$(2) + targetFile$ FOR INPUT AS #2
                INPUT #2, LG$, D1$, D2$, D3$, D4$
                FOR X = 1 TO 40: 
                    INPUT #2, TM$(X)
                NEXT
                CLOSE 2

                'H1$(XW) = TM$(X)

                FOR X = 120 TO 139
                    cmplZ2$(X) = "*"
                    cmplZ3$(X) = "*"
                    cmplZ1!(X, 1) = 99.99
                    cmplZ1!(X, 2) = 0
                NEXT X
                
                FOR X = 239 TO 258
                    cmplZ2$(X) = "*"
                    cmplZ3$(X) = "*"
                    cmplZ1!(X, 1) = 99.99
                    cmplZ1!(X, 2) = 99.99
                NEXT X

                FOR XW = 1 TO 40
                    WN = 0
                    LS = 0

                    IF H1$(XW) <> CHR$(42) And _FILEEXISTS(diskPaths$(1) + H1$(XW)) THEN
                        PRINT "FINDING "; H1$(XW); " STATS"
                        WN = 0
                        LS = 0
                        OPEN diskPaths$(1) + H1$(XW) FOR INPUT AS #1
                        INPUT #1, TN$, WN, LS
                    END IF
                    
                    L = LEN(H1$(XW))
                    WT$ = ""

                    FOR X = 1 TO L
                        A = ASC(MID$(H1$(XW), X, 1))
                        WT$ = WT$ + CHR$(A)
                    NEXT X

                    FOR X = 1 TO 40:
                        IF TM$(X) = WT$ THEN

                            IF O1(X) = WN AND O2(X) = LS THEN 
                                Return
                            Else
                                O1(X) = WN
                                O2(X) = LS
                            End If
                        END IF
                        
                    NEXT X

                    FOR K = 1 TO 23
                        INPUT #1, NM$(K), ZM%(K)
                    NEXT

                    FOR K = 25 TO 46
                        INPUT #1, NM$(K), ZM%(K)
                    NEXT
                        
                    NM$(24) = "PITCHERS"

                    FOR J = 1 TO 23
                        FOR K = 1 TO 22
                            INPUT #1, JS!(J, K)
                        NEXT K
                    NEXT J

                    FOR J = 25 TO 46
                        FOR K = 1 TO 42
                            INPUT #1, JS!(J, K)
                        NEXT K
                    NEXT J

                    FOR K = 1 TO 24
                        INPUT #1, BAT%(K)
                    NEXT K

                    FOR K = 1 TO 23
                        INPUT #1, PIT%(K)
                    NEXT K

                    CLOSE #1


                    FOR J = 1 TO 23
                        IF JS!(J, 1) = 0 THEN JS!(J, 1) = 1
                        JS!(J, 17) = JS!(J, 13)
                        JS!(J, 18) = JS!(J, 14)

                        IF (JS!(J, 8) + JS!(J, 1)) / (WN + LS) < 3.1 THEN 
                            JS!(J, 14) = 0: JS!(J, 15) = 0: JS!(J, 16) = 0
                        ELSE
                            JS!(J, 16) = JS!(J, 2) / JS!(J, 1)
                            JS!(J, 14) = (JS!(J, 2) + JS!(J, 5) + 2 * JS!(J, 6) + 3 * JS!(J, 7)) / JS!(J, 1)
                            JS!(J, 15) = (JS!(J, 2) + JS!(J, 8)) / (JS!(J, 1) + JS!(J, 8))
                        END IF
                    NEXT

                    FOR J = 25 TO 46
                        IF JS!(J, 1) = 0 OR (JS!(J, 1) / 3) < (WN + LS) THEN 
                            JS!(J, 14) = 99.99
                        ELSE
                            JS!(J, 14) = JS!(J, 4) / JS!(J, 1) * 27
                        END IF
                    NEXT

                    Call CheckLeagueStats

                    CLOSE 1
                NEXT

                FOR VV = 1 To 4
                    '1 to 10, 11 to 20, 21 to 30, 31 to 40)
                    AA = ((QZ-1)*10) + 1
                    ZZ = QZ * 10

                    FOR X = AA TO ZZ - 1
                        FOR Y = X + 1 TO ZZ: 
                            IF O1(X) - O2(X) <= O1(Y) - O2(Y) THEN
                                IF O1(X) - O2(X) <> O1(Y) - O2(Y) Or O1(X) > O1(Y) AND O1(X) <= O2(X) THEN
                                    IF O1(X) - O2(X) <> O1(Y) - O2(Y) AND O1(X) < O1(Y) AND O1(X) >= O2(X) THEN             
                                        TN$ = TM$(X)
                                        TM$(X) = TM$(Y)
                                        TM$(Y) = TN$

                                        J = O1(X)
                                        O1(X) = O1(Y)
                                        O1(Y) = J

                                        J = O2(X)
                                        O2(X) = O2(Y)
                                        O2(Y) = J
                                    End If
                                End If
                            End if
                        NEXT Y
                    NEXT X
                NEXT VV
                
                OPEN diskPaths$(2) + LH$ + ".LDR" FOR OUTPUT AS #2: PRINT "POSTING "; LG$; " LEADERS"

                FOR X = 1 TO 40: WRITE #2, TM$(X), O1(X), O2(X): NEXT

                FOR X = 1 TO 298
                    WRITE #2, cmplZ2$(X), cmplZ3$(X), cmplZ1!(X, 1), cmplZ1!(X, 2)
                NEXT

                FOR X = 1 TO 40: 
                    WRITE #2, BA$(X)
                    FOR M = 1 TO 14: 
                        WRITE #2, OFT!(X, M)
                    NEXT
                NEXT

                FOR X = 1 TO 40: 
                    WRITE #2, ER$(X)
                    FOR M = 1 TO 14: 
                        WRITE #2, DFT!(X, M)
                    NEXT
                NEXT

                WRITE #2, APP
                CLOSE 2
                
            End If 'Done checking for ESC

        End If 'Done checking for file count

    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or I$ = "N"

    '103 Exit Sub

End Sub


'----------------------------------------
'      CheckLeagueStats Subroutine
'----------------------------------------
'...explanation...
Sub CheckLeagueStats ()

    Dim PS$(40)

    Shared NM$()
    Shared cmplZ2$(), cmplZ3$()
    Shared cmplZ1!(), DFT!(), JS!(), OFT!()
    
    PRINT "CHECKING "; H1$(XW); " STATS"

    CLS

    FOR X = 1 TO 23
        IF NM$(X) <> "XXX" THEN

            PRINT "     CHECKING STATS OF "; NM$(X)

            '3301 IF JS!(X,1)>0 THEN JS!(X,16)=JS!(X,2)/JS!(X,1)

            PS$(X) = WT$

            FOR Y = 1 TO 20
                IF cmplZ2$(Y) = NM$(X) AND cmplZ3$(Y) = WT$ THEN

                    cmplZ1!(Y, 1) = JS!(X, 13)
                    cmplZ1!(Y, 2) = JS!(X, 1)

                    IF cmplZ1!(Y, 2) < 2.5 * (WN + LS) THEN
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    END IF

                    '4820
                    FOR Y = 1 TO 19
                        FOR Z = Y + 1 TO 20
                            IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                                SWAP cmplZ3$(Y), cmplZ3$(Z)
                                SWAP cmplZ2$(Y), cmplZ2$(Z)
                                FOR Z1 = 1 TO 2: SWAP cmplZ1!(Y, Z1), cmplZ1!(Z, Z1): Next Z1
                            END IF
                        NEXT Z
                    NEXT Y
                    'Return

                    skipIF = 1
                    Exit For

                END IF

            NEXT Y

            IF JS!(X, 16) >= cmplZ1!(20, 1) Or skipIF = 0 THEN
                cmplZ2$(20) = NM$(X)
                cmplZ3$(20) = PS$(X)
                cmplZ1!(20, 1) = JS!(X, 16)
                cmplZ1!(20, 2) = JS!(X, 1)
                '4820
                FOR Y = 1 TO 19
                    FOR Z = Y + 1 TO 20
                        IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                            SWAP cmplZ3$(Y), cmplZ3$(Z)
                            SWAP cmplZ2$(Y), cmplZ2$(Z)
                            FOR Z1 = 1 TO 2: SWAP cmplZ1!(Y, Z1), cmplZ1!(Z, Z1): Next Z1
                        END IF
                    NEXT Z
                NEXT Y
                'Return

            END IF

            3318 '
            IF (JS!(X, 8) + JS!(X, 1)) / (WN + LS) >= 3.1 THEN
                IF JS!(X, 16) <= cmplZ1!(258, 1) THEN
                    cmplZ2$(258) = NM$(X)
                    cmplZ3$(258) = PS$(X)
                    cmplZ1!(258, 1) = JS!(X, 16)
                    cmplZ1!(258, 2) = JS!(X, 1)
                    FOR Y = 239 TO 257
                        FOR Z = Y + 1 TO 258
                            IF cmplZ1!(Y, 1) > cmplZ1!(Z, 1) THEN
                                SWAP cmplZ3$(Y), cmplZ3$(Z)
                                SWAP cmplZ2$(Y), cmplZ2$(Z)
                                FOR Z1 = 1 TO 2: SWAP cmplZ1!(Y, Z1), cmplZ1!(Z, Z1): Next Z1
                            END IF
                        NEXT Z
                    NEXT Y
                END IF
            END IF
        
        
            3320 '
            FOR Y = 99 TO 108
                IF cmplZ2$(Y) = NM$(X) AND cmplZ3$(Y) = WT$ THEN
                    '4871
                    cmplZ1!(Y, 1) = JS!(X, 14)
                    cmplZ1!(Y, 2) = JS!(X, 1)
                    IF cmplZ1!(Y, 2) < (WN + LS) * 2.5 THEN
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    END IF

                    FOR Y = 99 TO 107
                        FOR Z = Y + 1 TO 108
                            IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                                TN$ = cmplZ3$(Y)
                                cmplZ3$(Y) = cmplZ3$(Z)
                                cmplZ3$(Z) = TN$
                                TN$ = cmplZ2$(Y)
                                cmplZ2$(Y) = cmplZ2$(Z)
                                cmplZ2$(Z) = TN$
                                FOR Z1 = 1 TO 2
                                    J = cmplZ1!(Y, Z1)
                                    cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1)
                                    cmplZ1!(Z, Z1) = J
                                Next Z1
                            END IF
                        NEXT Z
                    NEXT Y
                    'Return

                    skipIF = 1
                    Exit For
                END IF
            NEXT Y

            IF JS!(X, 14) >= cmplZ1!(108, 1) Or skipIF = 0 THEN
                cmplZ2$(108) = NM$(X)
                cmplZ3$(108) = PS$(X)
                cmplZ1!(108, 1) = JS!(X, 14)
                cmplZ1!(108, 2) = JS!(X, 1)

                '4871
                cmplZ1!(Y, 1) = JS!(X, 14)
                cmplZ1!(Y, 2) = JS!(X, 1)
                IF cmplZ1!(Y, 2) < (WN + LS) * 2.5 THEN
                    cmplZ2$(Y) = "*"
                    cmplZ3$(Y) = "*"
                    cmplZ1!(Y, 1) = 0
                    cmplZ1!(Y, 2) = 0
                END IF

                FOR Y = 99 TO 107
                    FOR Z = Y + 1 TO 108
                        IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                            TN$ = cmplZ3$(Y)
                            cmplZ3$(Y) = cmplZ3$(Z)
                            cmplZ3$(Z) = TN$
                            TN$ = cmplZ2$(Y)
                            cmplZ2$(Y) = cmplZ2$(Z)
                            cmplZ2$(Z) = TN$
                            FOR Z1 = 1 TO 2
                                J = cmplZ1!(Y, Z1)
                                cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1)
                                cmplZ1!(Z, Z1) = J
                            Next Z1
                        END IF
                    NEXT Z
                NEXT Y
                'Return

            END IF

            3325 '
            FOR Y = 110 TO 119

                IF cmplZ2$(Y) = NM$(X) AND cmplZ3$(Y) = WT$ THEN

                    '4878
                    cmplZ1!(Y, 1) = JS!(X, 15): cmplZ1!(Y, 2) = JS!(X, 1)
                    IF cmplZ1!(Y, 2) < (WN + LS) * 2.5 THEN
                        cmplZ2$(Y) = "*": cmplZ3$(Y) = "*": cmplZ1!(Y, 1) = 0: cmplZ1!(Y, 2) = 0
                    END IF

                    FOR Y = 110 TO 118
                        FOR Z = Y + 1 TO 119
                            IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                                TN$ = cmplZ3$(Y): cmplZ3$(Y) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                                TN$ = cmplZ2$(Y): cmplZ2$(Y) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                                FOR Z1 = 1 TO 2:
                                    J = cmplZ1!(Y, Z1): cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1): cmplZ1!(Z, Z1) = J
                                Next Z1
                            END IF
                        NEXT Z
                    NEXT Y
                    'Return

                    skipIF = 1
                END IF

            NEXT Y

            3327 '
            IF JS!(X, 15) >= cmplZ1!(119, 1) Or skipIF = 0 THEN

                cmplZ2$(119) = NM$(X)
                cmplZ3$(119) = PS$(X): cmplZ1!(119, 1) = JS!(X, 15): cmplZ1!(119, 2) = JS!(X, 1):

                '4878
                cmplZ1!(Y, 1) = JS!(X, 15): cmplZ1!(Y, 2) = JS!(X, 1)

                IF cmplZ1!(Y, 2) < (WN + LS) * 2.5 THEN
                    cmplZ2$(Y) = "*": cmplZ3$(Y) = "*": cmplZ1!(Y, 1) = 0: cmplZ1!(Y, 2) = 0
                END IF

                FOR Y = 110 TO 118
                    FOR Z = Y + 1 TO 119
                        IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                            TN$ = cmplZ3$(Y): cmplZ3$(Y) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            TN$ = cmplZ2$(Y): cmplZ2$(Y) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            FOR Z1 = 1 TO 2
                                J = cmplZ1!(Y, Z1): cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1): cmplZ1!(Z, Z1) = J
                            Next Z1
                        END IF
                    NEXT Z
                NEXT Y
                'Return

            End If


            3330 '
            FOR Z = 21 TO 31
                If cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                    cmplZ1!(Z, 1) = JS!(X, 2)
                    GoTo 3335
                End If

            NEXT Z

            IF JS!(X, 2) > cmplZ1!(31, 1) THEN

                cmplZ1!(31, 1) = JS!(X, 2)
                cmplZ2$(31) = NM$(X)
                cmplZ3$(31) = WT$

                3335'
                FOR Z = 21 TO 30
                    FOR Z1 = Z + 1 TO 31
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        END IF
                    Next Z1
                NEXT Z

                '3340 FOR Z = 21 TO 31
                '3341 NEXT Z

            End If


            FOR Z = 32 TO 42
                IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                    cmplZ1!(Z, 1) = JS!(X, 5)
                    GOTO 3347
                End If
            NEXT Z

            IF JS!(X, 5) > cmplZ1!(42, 1) THEN

                cmplZ1!(42, 1) = JS!(X, 5): cmplZ2$(42) = NM$(X)
                cmplZ3$(42) = WT$

                3347'
                FOR Z = 32 TO 41
                    FOR Z1 = Z + 1 TO 42
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                NEXT Z

                '3352 FOR Z = 32 TO 42
                '3353 NEXT Z
            End If

            3354 '
            FOR Z = 43 TO 53
                    IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                        cmplZ1!(Z, 1) = JS!(X, 6)
                        GOTO 3359
                    End If
            NEXT Z

            IF JS!(X, 6) > cmplZ1!(53, 1) THEN

                cmplZ1!(53, 1) = JS!(X, 6): cmplZ2$(53) = NM$(X)
                cmplZ3$(53) = WT$

                3359 '
                FOR Z = 43 TO 52
                    FOR Z1 = Z + 1 TO 53
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        END IF
                    Next Z1
                NEXT Z

                '3364 FOR Z = 43 TO 53
                '3365 NEXT Z
            End If
        


            FOR Z = 54 TO 64
                IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                    cmplZ1!(Z, 1) = JS!(X, 7)
                    GOTO 3371
                End If
            NEXT Z

            IF JS!(X, 7) > cmplZ1!(64, 1) THEN

                cmplZ1!(64, 1) = JS!(X, 7): cmplZ2$(64) = NM$(X)
                cmplZ3$(64) = WT$

                3371 '
                FOR Z = 54 TO 63:
                    FOR Z1 = Z + 1 TO 64:
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                NEXT Z

                '3376 FOR Z = 54 TO 64
                '3377 NEXT Z
            End If

            3378 '
            FOR Z = 65 TO 74

                IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                    cmplZ1!(Z, 1) = JS!(X, 3)
                    GOTO 3383
                End If

            NEXT Z
        
            IF JS!(X, 3) > cmplZ1!(74, 1) THEN

                cmplZ1!(74, 1) = JS!(X, 3): cmplZ2$(74) = NM$(X)
                cmplZ3$(74) = WT$

                3383 '
                FOR Z = 65 TO 73
                    FOR Z1 = Z + 1 TO 74
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        END IF
                    Next Z1
                NEXT Z

                '3388 FOR Z = 65 TO 74
                '3389 NEXT Z
            End If


            FOR Z = 76 TO 85
                IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                    cmplZ1!(Z, 1) = JS!(X, 4)
                    GOTO 3395
                End If
            NEXT Z

            IF JS!(X, 4) > cmplZ1!(85, 1) THEN

                cmplZ1!(85, 1) = JS!(X, 4)
                cmplZ2$(85) = NM$(X)
                cmplZ3$(85) = WT$

                '3395
                FOR Z = 76 TO 84
                    FOR Z1 = Z + 1 TO 85
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If

                    Next Z1
                NEXT Z

                '3400 FOR Z = 76 TO 86
                '3401 NEXT Z
            End If


            FOR Z = 87 TO 97

                IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN
                    cmplZ1!(Z, 1) = JS!(X, 10)
                    GOTO 3407
                End If
            NEXT Z
        
            IF JS!(X, 10) > cmplZ1!(97, 1) THEN

                cmplZ1!(97, 1) = JS!(X, 10): cmplZ2$(97) = NM$(X)
                cmplZ3$(97) = WT$

                3407 '
                FOR Z = 87 TO 96
                    FOR Z1 = Z + 1 TO 97
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                NEXT Z

                '3412 FOR Z = 87 TO 97
                '3413 NEXT Z
            End If

        End If 'Done checking for XXX / terminating chars

        3414 '
        FOR Z = 217 TO 227

            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 8)
                GOTO 3605
            END IF

        NEXT Z
        
        IF JS!(X, 8) <= cmplZ1!(227, 1) THEN
            cmplZ1!(227, 1) = JS!(X, 8)
            cmplZ2$(227) = NM$(X)
            cmplZ3$(227) = WT$

            3605 '
            FOR Z = 217 TO 226
                FOR Z1 = Z + 1 TO 227
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    END IF
                Next Z1
            NEXT Z
        End If


        FOR Z = 279 TO 288
            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 17)
                GOTO 3615
            End If
        NEXT Z
        
        IF JS!(X, 17) > cmplZ1!(288, 1) THEN

            cmplZ1!(288, 1) = JS!(X, 17)
            cmplZ2$(288) = NM$(X)
            cmplZ3$(288) = WT$

            3615 '
            FOR Z = 279 TO 287
                FOR Z1 = Z + 1 TO 288: 
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN 
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If  
                Next Z1
            NEXT Z

        End If


        FOR Z = 289 TO 298
            3621 '
            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 18)
                GOTO 3625
            End If
        NEXT Z
        
        IF JS!(X, 18) > cmplZ1!(298, 1) THEN
            cmplZ1!(298, 1) = JS!(X, 18)
            cmplZ2$(298) = NM$(X)
            cmplZ3$(298) = WT$

            3625 '
            FOR Z = 289 TO 297
                FOR Z1 = Z + 1 TO 298
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    END IF
                    
                Next Z1
            NEXT Z
            
        End If
        
    NEXT X

    Call CheckMoreStats

End Sub


'----------------------------------------
'      CheckMoreStats Subroutine
'----------------------------------------
'...explanation...
Sub CheckMoreStats ()

    Shared BAT%(), PIT%()
    Shared cmplZ2$(), cmplZ3$()
    Shared BA$(), ER$(), NM$()
    Shared cmplZ1!(), DFT!(), JS!(), OFT!()

    3415 '
    FOR X = 25 TO 46
        IF NM$(X) = "XXX" THEN 3525
        PRINT "     CHECKING STATS OF "; NM$(X)

        IF JS!(X, 14) <= cmplZ1!(139, 1) THEN
            cmplZ2$(139) = NM$(X)
            cmplZ3$(139) = WT$
            cmplZ1!(139, 1) = JS!(X, 14)
            cmplZ1!(139, 2) = JS!(X, 1) / 3
            FOR Y = 121 TO 138
                FOR Z = Y + 1 TO 139
                    IF cmplZ1!(Y, 1) > cmplZ1!(Z, 1) THEN
                        SWAP cmplZ2$(Y), cmplZ2$(Z)
                        SWAP cmplZ3$(Y), cmplZ3$(Z)
                        FOR Z1 = 1 TO 2
                            SWAP cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                        Next Z1
                    END IF  
                NEXT Z
            NEXT Y

        End If

        IF JS!(X, 1) / 3 >= (WN + LS) AND JS!(X, 1) <> 0 THEN
            IF JS!(X, 14) < cmplZ1!(278, 1) THEN 3430
            cmplZ2$(278) = NM$(X)
            cmplZ3$(278) = WT$
            cmplZ1!(278, 1) = JS!(X, 14)
            cmplZ1!(278, 2) = JS!(X, 1) / 3
            FOR Y = 259 TO 277
                FOR Z = Y + 1 TO 278
                    IF cmplZ1!(Y, 1) < cmplZ1!(Z, 1) THEN
                        SWAP cmplZ2$(Y), cmplZ2$(Z)
                        SWAP cmplZ3$(Y), cmplZ3$(Z)
                        FOR Z1 = 1 TO 2
                            SWAP cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                        Next Z1
                    END IF
                NEXT Z
            NEXT Y

        END IF


        3430 '
        FOR Y = 140 TO 150
            IF cmplZ2$(Y) = NM$(X) AND cmplZ3$(Y) = WT$ THEN 
                cmplZ1!(Y, 1) = JS!(X, 7)
                cmplZ1!(Y, 2) = JS!(X, 8)

                '4860 '
                FOR Z = 140 TO 149: 
                    FOR Y1 = Z + 1 TO 150
                        IF cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <= cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) THEN

                            IF cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <> cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) OR cmplZ1!(Z, 1) > cmplZ1!(Y1, 1) THEN
                                TN$ = cmplZ2$(Y1)
                                cmplZ2$(Y1) = cmplZ2$(Z)
                                cmplZ2$(Z) = TN$
                                TN$ = cmplZ3$(Y1)
                                cmplZ3$(Y1) = cmplZ3$(Z)
                                cmplZ3$(Z) = TN$
                                FOR Z1 = 1 TO 2: 
                                    J = cmplZ1!(Y1, Z1)
                                    cmplZ1!(Y1, Z1) = cmplZ1!(Z, Z1)
                                    cmplZ1!(Z, Z1) = J
                                Next Z1
                            END IF
                            
                        END IF

                    NEXT Y1
                NEXT Z

                GOTO 3440
            END IF
        NEXT Y


        IF JS!(X, 7) - JS!(X, 8) >= cmplZ1!(150, 1) - cmplZ1!(150, 2) THEN

            IF JS!(X, 7) - JS!(X, 8) <> cmplZ1!(150, 1) - cmplZ1!(150, 2) OR JS!(X, 7) < cmplZ1!(150, 1) THEN   
                cmplZ2$(150) = NM$(X)
                cmplZ3$(150) = WT$
                cmplZ1!(150, 1) = JS!(X, 7)
                cmplZ1!(150, 2) = JS!(X, 8)

                '4860 '
                FOR Z = 140 TO 149: 
                    FOR Y1 = Z + 1 TO 150
                        IF cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <= cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) THEN

                            IF cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <> cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) OR cmplZ1!(Z, 1) > cmplZ1!(Y1, 1) THEN
                                TN$ = cmplZ2$(Y1)
                                cmplZ2$(Y1) = cmplZ2$(Z)
                                cmplZ2$(Z) = TN$
                                TN$ = cmplZ3$(Y1)
                                cmplZ3$(Y1) = cmplZ3$(Z)
                                cmplZ3$(Z) = TN$
                                FOR Z1 = 1 TO 2: 
                                    J = cmplZ1!(Y1, Z1)
                                    cmplZ1!(Y1, Z1) = cmplZ1!(Z, Z1)
                                    cmplZ1!(Z, Z1) = J
                                Next Z1
                            END IF
                            
                        END IF

                    NEXT Y1
                NEXT Z
                'Return

            END IF

        END IF


        3440 '
        FOR Z = 151 TO 161

            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 7)
                '3445
                FOR Z = 151 TO 160
                    FOR Z1 = Z + 1 TO 161
                        IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        END IF
                    Next Z1
                NEXT Z
                skipIF = 1
                Exit For
            End If
            
        NEXT Z

        IF JS!(X, 7) > cmplZ1!(161, 1) Or skipIF = 0 THEN 

            cmplZ1!(161, 1) = JS!(X, 7): cmplZ2$(161) = NM$(X)

            cmplZ3$(161) = WT$

            '3445
            FOR Z = 151 TO 160
                FOR Z1 = Z + 1 TO 161
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    END IF
                Next Z1
            NEXT Z

            '3450 FOR Z = 151 TO 161
            '3451 NEXT Z

        End If


        3452 '
        FOR Z = 162 TO 172

            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 1) / 3
                GOTO 3457
            ENd If
        NEXT Z
        
        IF JS!(X, 1) / 3 > cmplZ1!(172, 1) THEN

            cmplZ1!(172, 1) = JS!(X, 1) / 3: cmplZ2$(172) = NM$(X)
            cmplZ3$(172) = WT$

            3457 '
            FOR Z = 162 TO 171
                FOR Z1 = Z + 1 TO 172
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                Next Z1
            NEXT Z

            '3462 FOR Z = 162 TO 172
            '3463 NEXT Z
        End If
        
        
        3464 '
        FOR Z = 173 TO 183
            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 6)
                GOTO 3469
            END IF
        NEXT Z
        
        IF JS!(X, 6) > cmplZ1!(183, 1) THEN

            cmplZ1!(183, 1) = JS!(X, 6): cmplZ2$(183) = NM$(X)
            cmplZ3$(183) = WT$

            3469 '
            FOR Z = 173 TO 182: 
                FOR Z1 = Z + 1 TO 183: 
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                    
                Next Z1
            NEXT Z

            '3474 FOR Z = 173 TO 183
            '3475 NEXT Z
        End If
        
        
        3476 '
        FOR Z = 184 TO 194
            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 11)
                GOTO 3481
            End If
        NEXT Z
        
        IF JS!(X, 11) >= cmplZ1!(194, 1) THEN

            cmplZ1!(194, 1) = JS!(X, 11): cmplZ2$(194) = NM$(X)
            cmplZ3$(194) = WT$

            3481 '
            FOR Z = 184 TO 193
                FOR Z1 = Z + 1 TO 194
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                Next Z1
            NEXT Z

            '3486 FOR Z = 184 TO 194

            '3487 NEXT Z
        End If


        3488 '
        FOR Z = 195 TO 205

            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 12)
                GOTO 3493
            End If
        NEXT Z
        
        IF JS!(X, 12) > cmplZ1!(205, 1) THEN
            cmplZ1!(205, 1) = JS!(X, 12): cmplZ2$(205) = NM$(X)
            cmplZ3$(205) = WT$

            3493 '
            FOR Z = 195 TO 204
                FOR Z1 = Z + 1 TO 205
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                Next Z1
            NEXT Z

            '3498 FOR Z = 195 TO 205
            'NEXT Z
        End If
        
        
        3500 '
        FOR Z = 206 TO 216

            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 13)
                GOTO 3505
            End If
        NEXT Z
        
        IF JS!(X, 13) > cmplZ1!(216, 1) THEN

            cmplZ1!(216, 1) = JS!(X, 13): cmplZ2$(216) = NM$(X)
            cmplZ3$(216) = WT$

            3505 '
            FOR Z = 206 TO 215
                FOR Z1 = Z + 1 TO 216
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                Next Z1
            NEXT Z

            '3510 FOR Z = 206 TO 216
            'NEXT Z
        End If


        3512 '
        APP = APP + JS!(X, 9)
        FOR Z = 228 TO 238
            IF cmplZ2$(Z) = NM$(X) AND cmplZ3$(Z) = WT$ THEN 
                cmplZ1!(Z, 1) = JS!(X, 9)
                GOTO 3518
            End If
        NEXT Z
        
        IF JS!(X, 9) > cmplZ1!(238, 1) THEN

            cmplZ1!(238, 1) = JS!(X, 9): cmplZ2$(238) = NM$(X)
            cmplZ3$(238) = WT$
            
            3518 '
            FOR Z = 228 TO 237: 
                FOR Z1 = Z + 1 TO 238: 
                    IF cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) THEN
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If

                Next Z1
            NEXT Z

            '3523 FOR Z = 228 TO 238
            '3524 NEXT Z
        End If
    
    NEXT X


    3530 '
    FOR Z = 1 TO 40: 
        IF BAT%(3) = 0 THEN BAT%(3) = 1
        IF BA$(Z) = WT$ THEN 
            OFT!(Z, 1) = BAT%(4) / BAT%(3)
            FOR M = 3 TO 14: OFT!(Z, M) = BAT%(M): NEXT M
            GOTO 3535
        End If

    NEXT Z

    IF BAT%(4) / BAT%(3) > OFT!(40, 1) THEN

        OFT!(40, 1) = BAT%(4) / BAT%(3): FOR M = 3 TO 14: OFT!(40, M) = BAT%(M): NEXT M: BA$(40) = WT$
        BA$(40) = WT$

        3535 '
        FOR Z = 1 TO 39
            FOR Z1 = Z + 1 TO 40
                IF OFT!(Z, 1) < OFT!(Z1, 1) THEN
                    TN$ = BA$(Z1): BA$(Z1) = BA$(Z): BA$(Z) = TN$
                    FOR M = 1 TO 14: 
                        J! = OFT!(Z1, M): OFT!(Z1, M) = OFT!(Z, M): OFT!(Z, M) = J!
                    NEXT M
                End If
            NEXT Z1
        NEXT Z

        FOR Z = 1 TO 40
            IF OFT!(Z, 1) = OFT!(40, 1) THEN BA$(Z) = "*": BA$(Z) = "*"
        NEXT Z

    End If

    FOR Z = 1 TO 40
        IF PIT%(1) = 0 THEN PIT%(1) = 1: PIT%(4) = 3
        IF ER$(Z) = WT$ THEN 
            DFT!(Z, 1) = PIT%(4) / PIT%(1) * 27
            FOR M = 2 TO 14: DFT!(Z, M) = PIT%(M - 1): NEXT M
            GOTO 3555
        End If
    NEXT Z

    IF PIT%(4) / PIT%(1) * 27 < DFT!(40, 1) THEN

        DFT!(40, 1) = PIT%(4) / PIT%(1) * 27
        FOR M = 2 TO 14: DFT!(40, M) = PIT%(M - 1): NEXT M
        
        ER$(40) = WT$
        ER$(40) = WT$

        3555 '
        FOR Z = 1 TO 39
            FOR Z1 = Z + 1 TO 40
                IF DFT!(Z, 1) > DFT!(Z1, 1) THEN
                    TN$ = ER$(Z1): ER$(Z1) = ER$(Z): ER$(Z) = TN$
                    FOR M = 1 TO 14: J! = DFT!(Z1, M): DFT!(Z1, M) = DFT!(Z, M): DFT!(Z, M) = J!: NEXT M
                End If
            Next Z1
        NEXT Z

        FOR Z = 1 TO 40: 
            IF DFT!(Z, 1) = DFT!(40, 1) THEN ER$(Z) = "*": ER$(Z) = "*"
        NEXT Z

    End If

End Sub

'5005 L = LEN(TN$): WX$ = "": FOR X = 1 TO (L - 3): A = ASC(MID$(TN$, X, 1))
''5006 WT$ = WT$ + CHR$(A): NEXT X
'5007 TN$ = WT$: Return
