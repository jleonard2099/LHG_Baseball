Sub LEAGCMPL ()

    _Title "Full Count Baseball - Update League Stats"

    DefInt A-Z

    Dim H1$(40), TM$(1 To 40)

    Dim ZM%(46)

    Dim O1(1 To 40), O2(1 To 40)

    Shared BAT%(), PIT%()
    Shared cmplZ2$(), cmplZ3$()
    Shared BA$(), ER$(), NM$()
    Shared cmplZ1!(), DFT!(), JS!(), OFT!()

    Cls

    APP = 0

    Do
        'Display list of files...

        Spec$ = diskPaths$(2) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
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
                Print "SELECT LEAGUE TO COMPILE": Color 15
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
                    targetFile$ = leagueFiles$(Selection%)
                    Locate 2, 33: Print targetFile$
                    Locate , 33: Print "ANY CHANGE (YN)";
                    Color 15, 0
                    I$ = GetKeyPress$
                End If

            Loop Until UCase$(I$) = "N"

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                LH$ = Left$(WE$, 8)

                C$ = ","

                For Z = 1 To 40
                    DFT!(Z, 1) = 99.99
                Next

                Cls

                targetFile$ = LH$ + ".TMS"

                Open diskPaths$(2) + targetFile$ For Input As #2
                Input #2, LG$, D1$, D2$, D3$, D4$
                For XW = 1 To 40
                    Input #2, H1$(XW)
                Next
                Close 2

                C$ = ","

                Open diskPaths$(2) + targetFile$ For Input As #2
                Input #2, LG$, D1$, D2$, D3$, D4$
                For X = 1 To 40:
                    Input #2, TM$(X)
                Next
                Close 2

                'H1$(XW) = TM$(X)

                For X = 120 To 139
                    cmplZ2$(X) = "*"
                    cmplZ3$(X) = "*"
                    cmplZ1!(X, 1) = 99.99
                    cmplZ1!(X, 2) = 0
                Next X

                For X = 239 To 258
                    cmplZ2$(X) = "*"
                    cmplZ3$(X) = "*"
                    cmplZ1!(X, 1) = 99.99
                    cmplZ1!(X, 2) = 99.99
                Next X

                For XW = 1 To 40
                    WN = 0
                    LS = 0

                    If H1$(XW) <> Chr$(42) And _FileExists(diskPaths$(1) + H1$(XW)) Then
                        Print "FINDING "; H1$(XW); " STATS"
                        WN = 0
                        LS = 0
                        Open diskPaths$(1) + H1$(XW) For Input As #1
                        Input #1, TN$, WN, LS
                    End If

                    L = Len(H1$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(H1$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    For X = 1 To 40:
                        If TM$(X) = WT$ Then

                            If O1(X) = WN And O2(X) = LS Then
                                skipRemaining% = 1
                                Exit For
                            Else
                                O1(X) = WN
                                O2(X) = LS
                            End If
                        End If

                    Next X

                    If skipRemaining% <> 1 Then
                        For K = 1 To 23
                            Input #1, NM$(K), ZM%(K)
                        Next

                        For K = 25 To 46
                            Input #1, NM$(K), ZM%(K)
                        Next

                        NM$(24) = "PITCHERS"

                        For J = 1 To 23
                            For K = 1 To 22
                                Input #1, JS!(J, K)
                            Next K
                        Next J

                        For J = 25 To 46
                            For K = 1 To 42
                                Input #1, JS!(J, K)
                            Next K
                        Next J

                        For K = 1 To 24
                            Input #1, BAT%(K)
                        Next K

                        For K = 1 To 23
                            Input #1, PIT%(K)
                        Next K

                        Close #1

                        For J = 1 To 23
                            If JS!(J, 1) = 0 Then JS!(J, 1) = 1
                            JS!(J, 17) = JS!(J, 13)
                            JS!(J, 18) = JS!(J, 14)

                            If (JS!(J, 8) + JS!(J, 1)) / (WN + LS) < 3.1 Then
                                JS!(J, 14) = 0: JS!(J, 15) = 0: JS!(J, 16) = 0
                            Else
                                JS!(J, 16) = JS!(J, 2) / JS!(J, 1)
                                JS!(J, 14) = (JS!(J, 2) + JS!(J, 5) + 2 * JS!(J, 6) + 3 * JS!(J, 7)) / JS!(J, 1)
                                JS!(J, 15) = (JS!(J, 2) + JS!(J, 8)) / (JS!(J, 1) + JS!(J, 8))
                            End If
                        Next

                        For J = 25 To 46
                            If JS!(J, 1) = 0 Or (JS!(J, 1) / 3) < (WN + LS) Then
                                JS!(J, 14) = 99.99
                            Else
                                JS!(J, 14) = JS!(J, 4) / JS!(J, 1) * 27
                            End If
                        Next

                        Call CheckLeagueStats(H1$(XW))

                    End If 'Done checking if we need to skip

                    Close #1

                Next XW

                For VV = 1 To 4
                    '1 to 10, 11 to 20, 21 to 30, 31 to 40)
                    AA = ((VV - 1) * 10) + 1
                    ZZ = VV * 10

                    For X = AA To ZZ - 1
                        For Y = X + 1 To ZZ:
                            If O1(X) - O2(X) <= O1(Y) - O2(Y) Then
                                If O1(X) - O2(X) <> O1(Y) - O2(Y) Or O1(X) > O1(Y) And O1(X) <= O2(X) Then
                                    If O1(X) - O2(X) <> O1(Y) - O2(Y) And O1(X) < O1(Y) And O1(X) >= O2(X) Then
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
                            End If
                        Next Y
                    Next X
                Next VV

                Open diskPaths$(2) + LH$ + ".LDR" For Output As #2: Print "POSTING "; LG$; " LEADERS"

                For X = 1 To 40: Write #2, TM$(X), O1(X), O2(X): Next

                For X = 1 To 298
                    Write #2, cmplZ2$(X), cmplZ3$(X), cmplZ1!(X, 1), cmplZ1!(X, 2)
                Next

                For X = 1 To 40:
                    Write #2, BA$(X)
                    For M = 1 To 14:
                        Write #2, OFT!(X, M)
                    Next
                Next

                For X = 1 To 40:
                    Write #2, ER$(X)
                    For M = 1 To 14:
                        Write #2, DFT!(X, M)
                    Next
                Next

                Write #2, APP
                Close 2

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
Sub CheckLeagueStats (targetTeam$)

    Dim PS$(40)

    Shared NM$()
    Shared cmplZ2$(), cmplZ3$()
    Shared cmplZ1!(), DFT!(), JS!(), OFT!()

    Print "CHECKING "; targetTeam$; " STATS"

    Cls

    For X = 1 To 23
        If NM$(X) <> "XXX" Then

            Print "     CHECKING STATS OF "; NM$(X)

            '3301 IF JS!(X,1)>0 THEN JS!(X,16)=JS!(X,2)/JS!(X,1)

            PS$(X) = WT$

            For Y = 1 To 20
                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then

                    cmplZ1!(Y, 1) = JS!(X, 13)
                    cmplZ1!(Y, 2) = JS!(X, 1)

                    If cmplZ1!(Y, 2) < 2.5 * (WN + LS) Then
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    End If

                    '4820
                    For Y = 1 To 19
                        For Z = Y + 1 To 20
                            If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                                Swap cmplZ3$(Y), cmplZ3$(Z)
                                Swap cmplZ2$(Y), cmplZ2$(Z)
                                For Z1 = 1 To 2: Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1): Next Z1
                            End If
                        Next Z
                    Next Y
                    'Return

                    skipIF = 1
                    Exit For

                End If

            Next Y

            If JS!(X, 16) >= cmplZ1!(20, 1) Or skipIF = 0 Then
                cmplZ2$(20) = NM$(X)
                cmplZ3$(20) = PS$(X)
                cmplZ1!(20, 1) = JS!(X, 16)
                cmplZ1!(20, 2) = JS!(X, 1)
                '4820
                For Y = 1 To 19
                    For Z = Y + 1 To 20
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            Swap cmplZ3$(Y), cmplZ3$(Z)
                            Swap cmplZ2$(Y), cmplZ2$(Z)
                            For Z1 = 1 To 2: Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1): Next Z1
                        End If
                    Next Z
                Next Y
                'Return

            End If

            3318 '
            If (JS!(X, 8) + JS!(X, 1)) / (WN + LS) >= 3.1 Then
                If JS!(X, 16) <= cmplZ1!(258, 1) Then
                    cmplZ2$(258) = NM$(X)
                    cmplZ3$(258) = PS$(X)
                    cmplZ1!(258, 1) = JS!(X, 16)
                    cmplZ1!(258, 2) = JS!(X, 1)
                    For Y = 239 To 257
                        For Z = Y + 1 To 258
                            If cmplZ1!(Y, 1) > cmplZ1!(Z, 1) Then
                                Swap cmplZ3$(Y), cmplZ3$(Z)
                                Swap cmplZ2$(Y), cmplZ2$(Z)
                                For Z1 = 1 To 2: Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1): Next Z1
                            End If
                        Next Z
                    Next Y
                End If
            End If

            3320 '
            For Y = 99 To 108
                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then
                    '4871
                    cmplZ1!(Y, 1) = JS!(X, 14)
                    cmplZ1!(Y, 2) = JS!(X, 1)
                    If cmplZ1!(Y, 2) < (WN + LS) * 2.5 Then
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    End If

                    For Y = 99 To 107
                        For Z = Y + 1 To 108
                            If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                                TN$ = cmplZ3$(Y)
                                cmplZ3$(Y) = cmplZ3$(Z)
                                cmplZ3$(Z) = TN$
                                TN$ = cmplZ2$(Y)
                                cmplZ2$(Y) = cmplZ2$(Z)
                                cmplZ2$(Z) = TN$
                                For Z1 = 1 To 2
                                    J = cmplZ1!(Y, Z1)
                                    cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1)
                                    cmplZ1!(Z, Z1) = J
                                Next Z1
                            End If
                        Next Z
                    Next Y
                    'Return

                    skipIF = 1
                    Exit For
                End If
            Next Y

            If JS!(X, 14) >= cmplZ1!(108, 1) Or skipIF = 0 Then
                cmplZ2$(108) = NM$(X)
                cmplZ3$(108) = PS$(X)
                cmplZ1!(108, 1) = JS!(X, 14)
                cmplZ1!(108, 2) = JS!(X, 1)

                '4871
                cmplZ1!(Y, 1) = JS!(X, 14)
                cmplZ1!(Y, 2) = JS!(X, 1)
                If cmplZ1!(Y, 2) < (WN + LS) * 2.5 Then
                    cmplZ2$(Y) = "*"
                    cmplZ3$(Y) = "*"
                    cmplZ1!(Y, 1) = 0
                    cmplZ1!(Y, 2) = 0
                End If

                For Y = 99 To 107
                    For Z = Y + 1 To 108
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            TN$ = cmplZ3$(Y)
                            cmplZ3$(Y) = cmplZ3$(Z)
                            cmplZ3$(Z) = TN$
                            TN$ = cmplZ2$(Y)
                            cmplZ2$(Y) = cmplZ2$(Z)
                            cmplZ2$(Z) = TN$
                            For Z1 = 1 To 2
                                J = cmplZ1!(Y, Z1)
                                cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1)
                                cmplZ1!(Z, Z1) = J
                            Next Z1
                        End If
                    Next Z
                Next Y
                'Return

            End If

            3325 '
            For Y = 110 To 119

                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then

                    '4878
                    cmplZ1!(Y, 1) = JS!(X, 15): cmplZ1!(Y, 2) = JS!(X, 1)
                    If cmplZ1!(Y, 2) < (WN + LS) * 2.5 Then
                        cmplZ2$(Y) = "*": cmplZ3$(Y) = "*": cmplZ1!(Y, 1) = 0: cmplZ1!(Y, 2) = 0
                    End If

                    For Y = 110 To 118
                        For Z = Y + 1 To 119
                            If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                                TN$ = cmplZ3$(Y): cmplZ3$(Y) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                                TN$ = cmplZ2$(Y): cmplZ2$(Y) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                                For Z1 = 1 To 2:
                                    J = cmplZ1!(Y, Z1): cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1): cmplZ1!(Z, Z1) = J
                                Next Z1
                            End If
                        Next Z
                    Next Y
                    'Return

                    skipIF = 1
                End If

            Next Y

            3327 '
            If JS!(X, 15) >= cmplZ1!(119, 1) Or skipIF = 0 Then

                cmplZ2$(119) = NM$(X)
                cmplZ3$(119) = PS$(X): cmplZ1!(119, 1) = JS!(X, 15): cmplZ1!(119, 2) = JS!(X, 1):

                '4878
                cmplZ1!(Y, 1) = JS!(X, 15): cmplZ1!(Y, 2) = JS!(X, 1)

                If cmplZ1!(Y, 2) < (WN + LS) * 2.5 Then
                    cmplZ2$(Y) = "*": cmplZ3$(Y) = "*": cmplZ1!(Y, 1) = 0: cmplZ1!(Y, 2) = 0
                End If

                For Y = 110 To 118
                    For Z = Y + 1 To 119
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            TN$ = cmplZ3$(Y): cmplZ3$(Y) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            TN$ = cmplZ2$(Y): cmplZ2$(Y) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            For Z1 = 1 To 2
                                J = cmplZ1!(Y, Z1): cmplZ1!(Y, Z1) = cmplZ1!(Z, Z1): cmplZ1!(Z, Z1) = J
                            Next Z1
                        End If
                    Next Z
                Next Y
                'Return

            End If

            3330 '
            For Z = 21 To 31
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 2)
                    GoTo 3335
                End If

            Next Z

            If JS!(X, 2) > cmplZ1!(31, 1) Then

                cmplZ1!(31, 1) = JS!(X, 2)
                cmplZ2$(31) = NM$(X)
                cmplZ3$(31) = WT$

                3335 '
                For Z = 21 To 30
                    For Z1 = Z + 1 To 31
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3340 FOR Z = 21 TO 31
                '3341 NEXT Z

            End If

            For Z = 32 To 42
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 5)
                    GoTo 3347
                End If
            Next Z

            If JS!(X, 5) > cmplZ1!(42, 1) Then

                cmplZ1!(42, 1) = JS!(X, 5): cmplZ2$(42) = NM$(X)
                cmplZ3$(42) = WT$

                3347 '
                For Z = 32 To 41
                    For Z1 = Z + 1 To 42
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3352 FOR Z = 32 TO 42
                '3353 NEXT Z
            End If

            3354 '
            For Z = 43 To 53
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 6)
                    GoTo 3359
                End If
            Next Z

            If JS!(X, 6) > cmplZ1!(53, 1) Then

                cmplZ1!(53, 1) = JS!(X, 6): cmplZ2$(53) = NM$(X)
                cmplZ3$(53) = WT$

                3359 '
                For Z = 43 To 52
                    For Z1 = Z + 1 To 53
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3364 FOR Z = 43 TO 53
                '3365 NEXT Z
            End If

            For Z = 54 To 64
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 7)
                    GoTo 3371
                End If
            Next Z

            If JS!(X, 7) > cmplZ1!(64, 1) Then

                cmplZ1!(64, 1) = JS!(X, 7): cmplZ2$(64) = NM$(X)
                cmplZ3$(64) = WT$

                3371 '
                For Z = 54 To 63:
                    For Z1 = Z + 1 To 64:
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3376 FOR Z = 54 TO 64
                '3377 NEXT Z
            End If

            3378 '
            For Z = 65 To 74

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 3)
                    GoTo 3383
                End If

            Next Z

            If JS!(X, 3) > cmplZ1!(74, 1) Then

                cmplZ1!(74, 1) = JS!(X, 3): cmplZ2$(74) = NM$(X)
                cmplZ3$(74) = WT$

                3383 '
                For Z = 65 To 73
                    For Z1 = Z + 1 To 74
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3388 FOR Z = 65 TO 74
                '3389 NEXT Z
            End If

            For Z = 76 To 85
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 4)
                    GoTo 3395
                End If
            Next Z

            If JS!(X, 4) > cmplZ1!(85, 1) Then

                cmplZ1!(85, 1) = JS!(X, 4)
                cmplZ2$(85) = NM$(X)
                cmplZ3$(85) = WT$

                3395 '
                For Z = 76 To 84
                    For Z1 = Z + 1 To 85
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If

                    Next Z1
                Next Z

                '3400 FOR Z = 76 TO 86
                '3401 NEXT Z
            End If

            For Z = 87 To 97

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 10)
                    GoTo 3407
                End If
            Next Z

            If JS!(X, 10) > cmplZ1!(97, 1) Then

                cmplZ1!(97, 1) = JS!(X, 10): cmplZ2$(97) = NM$(X)
                cmplZ3$(97) = WT$

                3407 '
                For Z = 87 To 96
                    For Z1 = Z + 1 To 97
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3412 FOR Z = 87 TO 97
                '3413 NEXT Z
            End If

        End If 'Done checking for XXX / terminating chars

        3414 '
        For Z = 217 To 227

            If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                cmplZ1!(Z, 1) = JS!(X, 8)
                GoTo 3605
            End If

        Next Z

        If JS!(X, 8) <= cmplZ1!(227, 1) Then
            cmplZ1!(227, 1) = JS!(X, 8)
            cmplZ2$(227) = NM$(X)
            cmplZ3$(227) = WT$

            3605 '
            For Z = 217 To 226
                For Z1 = Z + 1 To 227
                    If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                Next Z1
            Next Z
        End If

        For Z = 279 To 288
            If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                cmplZ1!(Z, 1) = JS!(X, 17)
                GoTo 3615
            End If
        Next Z

        If JS!(X, 17) > cmplZ1!(288, 1) Then

            cmplZ1!(288, 1) = JS!(X, 17)
            cmplZ2$(288) = NM$(X)
            cmplZ3$(288) = WT$

            3615 '
            For Z = 279 To 287
                For Z1 = Z + 1 To 288:
                    If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If
                Next Z1
            Next Z

        End If

        For Z = 289 To 298
            3621 '
            If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                cmplZ1!(Z, 1) = JS!(X, 18)
                GoTo 3625
            End If
        Next Z

        If JS!(X, 18) > cmplZ1!(298, 1) Then
            cmplZ1!(298, 1) = JS!(X, 18)
            cmplZ2$(298) = NM$(X)
            cmplZ3$(298) = WT$

            3625 '
            For Z = 289 To 297
                For Z1 = Z + 1 To 298
                    If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                        TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                        TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                        J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                    End If

                Next Z1
            Next Z

        End If

    Next X

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
    For X = 25 To 46
        If NM$(X) <> "XXX" Then
            Print "     CHECKING STATS OF "; NM$(X)

            If JS!(X, 14) <= cmplZ1!(139, 1) Then
                cmplZ2$(139) = NM$(X)
                cmplZ3$(139) = WT$
                cmplZ1!(139, 1) = JS!(X, 14)
                cmplZ1!(139, 2) = JS!(X, 1) / 3
                For Y = 121 To 138
                    For Z = Y + 1 To 139
                        If cmplZ1!(Y, 1) > cmplZ1!(Z, 1) Then
                            Swap cmplZ2$(Y), cmplZ2$(Z)
                            Swap cmplZ3$(Y), cmplZ3$(Z)
                            For Z1 = 1 To 2
                                Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            If JS!(X, 1) / 3 >= (WN + LS) And JS!(X, 1) <> 0 Then
                If JS!(X, 14) < cmplZ1!(278, 1) Then 3430
                cmplZ2$(278) = NM$(X)
                cmplZ3$(278) = WT$
                cmplZ1!(278, 1) = JS!(X, 14)
                cmplZ1!(278, 2) = JS!(X, 1) / 3
                For Y = 259 To 277
                    For Z = Y + 1 To 278
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            Swap cmplZ2$(Y), cmplZ2$(Z)
                            Swap cmplZ3$(Y), cmplZ3$(Z)
                            For Z1 = 1 To 2
                                Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3430 '
            For Y = 140 To 150
                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then
                    cmplZ1!(Y, 1) = JS!(X, 7)
                    cmplZ1!(Y, 2) = JS!(X, 8)

                    '4860 '
                    For Z = 140 To 149:
                        For Y1 = Z + 1 To 150
                            If cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <= cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) Then

                                If cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <> cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) Or cmplZ1!(Z, 1) > cmplZ1!(Y1, 1) Then
                                    TN$ = cmplZ2$(Y1)
                                    cmplZ2$(Y1) = cmplZ2$(Z)
                                    cmplZ2$(Z) = TN$
                                    TN$ = cmplZ3$(Y1)
                                    cmplZ3$(Y1) = cmplZ3$(Z)
                                    cmplZ3$(Z) = TN$
                                    For Z1 = 1 To 2:
                                        J = cmplZ1!(Y1, Z1)
                                        cmplZ1!(Y1, Z1) = cmplZ1!(Z, Z1)
                                        cmplZ1!(Z, Z1) = J
                                    Next Z1
                                End If

                            End If

                        Next Y1
                    Next Z

                    GoTo 3440
                End If
            Next Y

            If JS!(X, 7) - JS!(X, 8) >= cmplZ1!(150, 1) - cmplZ1!(150, 2) Then

                If JS!(X, 7) - JS!(X, 8) <> cmplZ1!(150, 1) - cmplZ1!(150, 2) Or JS!(X, 7) < cmplZ1!(150, 1) Then
                    cmplZ2$(150) = NM$(X)
                    cmplZ3$(150) = WT$
                    cmplZ1!(150, 1) = JS!(X, 7)
                    cmplZ1!(150, 2) = JS!(X, 8)

                    '4860 '
                    For Z = 140 To 149:
                        For Y1 = Z + 1 To 150
                            If cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <= cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) Then

                                If cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <> cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) Or cmplZ1!(Z, 1) > cmplZ1!(Y1, 1) Then
                                    TN$ = cmplZ2$(Y1)
                                    cmplZ2$(Y1) = cmplZ2$(Z)
                                    cmplZ2$(Z) = TN$
                                    TN$ = cmplZ3$(Y1)
                                    cmplZ3$(Y1) = cmplZ3$(Z)
                                    cmplZ3$(Z) = TN$
                                    For Z1 = 1 To 2:
                                        J = cmplZ1!(Y1, Z1)
                                        cmplZ1!(Y1, Z1) = cmplZ1!(Z, Z1)
                                        cmplZ1!(Z, Z1) = J
                                    Next Z1
                                End If

                            End If

                        Next Y1
                    Next Z
                    'Return

                End If

            End If

            3440 '
            For Z = 151 To 161

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 7)
                    '3445
                    For Z = 151 To 160
                        For Z1 = Z + 1 To 161
                            If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                                TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                                TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                                J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                            End If
                        Next Z1
                    Next Z
                    skipIF = 1
                    Exit For
                End If

            Next Z

            If JS!(X, 7) > cmplZ1!(161, 1) Or skipIF = 0 Then

                cmplZ1!(161, 1) = JS!(X, 7): cmplZ2$(161) = NM$(X)

                cmplZ3$(161) = WT$

                '3445
                For Z = 151 To 160
                    For Z1 = Z + 1 To 161
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3450 FOR Z = 151 TO 161
                '3451 NEXT Z

            End If

            3452 '
            For Z = 162 To 172

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 1) / 3
                    GoTo 3457
                End If
            Next Z

            If JS!(X, 1) / 3 > cmplZ1!(172, 1) Then

                cmplZ1!(172, 1) = JS!(X, 1) / 3: cmplZ2$(172) = NM$(X)
                cmplZ3$(172) = WT$

                3457 '
                For Z = 162 To 171
                    For Z1 = Z + 1 To 172
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3462 FOR Z = 162 TO 172
                '3463 NEXT Z
            End If

            3464 '
            For Z = 173 To 183
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 6)
                    GoTo 3469
                End If
            Next Z

            If JS!(X, 6) > cmplZ1!(183, 1) Then

                cmplZ1!(183, 1) = JS!(X, 6): cmplZ2$(183) = NM$(X)
                cmplZ3$(183) = WT$

                3469 '
                For Z = 173 To 182:
                    For Z1 = Z + 1 To 183:
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If

                    Next Z1
                Next Z

                '3474 FOR Z = 173 TO 183
                '3475 NEXT Z
            End If

            3476 '
            For Z = 184 To 194
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 11)
                    GoTo 3481
                End If
            Next Z

            If JS!(X, 11) >= cmplZ1!(194, 1) Then

                cmplZ1!(194, 1) = JS!(X, 11): cmplZ2$(194) = NM$(X)
                cmplZ3$(194) = WT$

                3481 '
                For Z = 184 To 193
                    For Z1 = Z + 1 To 194
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3486 FOR Z = 184 TO 194

                '3487 NEXT Z
            End If

            3488 '
            For Z = 195 To 205

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 12)
                    GoTo 3493
                End If
            Next Z

            If JS!(X, 12) > cmplZ1!(205, 1) Then
                cmplZ1!(205, 1) = JS!(X, 12): cmplZ2$(205) = NM$(X)
                cmplZ3$(205) = WT$

                3493 '
                For Z = 195 To 204
                    For Z1 = Z + 1 To 205
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3498 FOR Z = 195 TO 205
                'NEXT Z
            End If

            3500 '
            For Z = 206 To 216

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 13)
                    GoTo 3505
                End If
            Next Z

            If JS!(X, 13) > cmplZ1!(216, 1) Then

                cmplZ1!(216, 1) = JS!(X, 13): cmplZ2$(216) = NM$(X)
                cmplZ3$(216) = WT$

                3505 '
                For Z = 206 To 215
                    For Z1 = Z + 1 To 216
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If
                    Next Z1
                Next Z

                '3510 FOR Z = 206 TO 216
                'NEXT Z
            End If

            3512 '
            APP = APP + JS!(X, 9)
            For Z = 228 To 238
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 9)
                    GoTo 3518
                End If
            Next Z

            If JS!(X, 9) > cmplZ1!(238, 1) Then

                cmplZ1!(238, 1) = JS!(X, 9): cmplZ2$(238) = NM$(X)
                cmplZ3$(238) = WT$

                3518 '
                For Z = 228 To 237:
                    For Z1 = Z + 1 To 238:
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            TN$ = cmplZ2$(Z1): cmplZ2$(Z1) = cmplZ2$(Z): cmplZ2$(Z) = TN$
                            TN$ = cmplZ3$(Z1): cmplZ3$(Z1) = cmplZ3$(Z): cmplZ3$(Z) = TN$
                            J = cmplZ1!(Z1, 1): cmplZ1!(Z1, 1) = cmplZ1!(Z, 1): cmplZ1!(Z, 1) = J
                        End If

                    Next Z1
                Next Z

                '3523 FOR Z = 228 TO 238
                '3524 NEXT Z
            End If

        End If 'Done checking for "XXX"

    Next X

    3530 '
    For Z = 1 To 40:
        If BAT%(3) = 0 Then BAT%(3) = 1
        If BA$(Z) = WT$ Then
            OFT!(Z, 1) = BAT%(4) / BAT%(3)
            For M = 3 To 14: OFT!(Z, M) = BAT%(M): Next M
            GoTo 3535
        End If

    Next Z

    If BAT%(4) / BAT%(3) > OFT!(40, 1) Then

        OFT!(40, 1) = BAT%(4) / BAT%(3): For M = 3 To 14: OFT!(40, M) = BAT%(M): Next M: BA$(40) = WT$
        BA$(40) = WT$

        3535 '
        For Z = 1 To 39
            For Z1 = Z + 1 To 40
                If OFT!(Z, 1) < OFT!(Z1, 1) Then
                    TN$ = BA$(Z1): BA$(Z1) = BA$(Z): BA$(Z) = TN$
                    For M = 1 To 14:
                        J! = OFT!(Z1, M): OFT!(Z1, M) = OFT!(Z, M): OFT!(Z, M) = J!
                    Next M
                End If
            Next Z1
        Next Z

        For Z = 1 To 40
            If OFT!(Z, 1) = OFT!(40, 1) Then BA$(Z) = "*": BA$(Z) = "*"
        Next Z

    End If

    For Z = 1 To 40
        If PIT%(1) = 0 Then PIT%(1) = 1: PIT%(4) = 3
        If ER$(Z) = WT$ Then
            DFT!(Z, 1) = PIT%(4) / PIT%(1) * 27
            For M = 2 To 14: DFT!(Z, M) = PIT%(M - 1): Next M
            GoTo 3555
        End If
    Next Z

    If PIT%(4) / PIT%(1) * 27 < DFT!(40, 1) Then

        DFT!(40, 1) = PIT%(4) / PIT%(1) * 27
        For M = 2 To 14: DFT!(40, M) = PIT%(M - 1): Next M

        ER$(40) = WT$
        ER$(40) = WT$

        3555 '
        For Z = 1 To 39
            For Z1 = Z + 1 To 40
                If DFT!(Z, 1) > DFT!(Z1, 1) Then
                    TN$ = ER$(Z1): ER$(Z1) = ER$(Z): ER$(Z) = TN$
                    For M = 1 To 14: J! = DFT!(Z1, M): DFT!(Z1, M) = DFT!(Z, M): DFT!(Z, M) = J!: Next M
                End If
            Next Z1
        Next Z

        For Z = 1 To 40:
            If DFT!(Z, 1) = DFT!(40, 1) Then ER$(Z) = "*": ER$(Z) = "*"
        Next Z

    End If

End Sub

'5005 L = LEN(TN$): WX$ = "": FOR X = 1 TO (L - 3): A = ASC(MID$(TN$, X, 1))
''5006 WT$ = WT$ + CHR$(A): NEXT X
'5007 TN$ = WT$: Return
