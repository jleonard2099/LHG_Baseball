Sub BLANK ()

    _Title "Full Count Baseball - Delete League Stats"

    Dim H1$(40)

    leagueFile$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Cls
        Locate 10, 8
        Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
        A$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(A$) = "Y" Then

            Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #2

            Input #2, LG$, D1$, D2$, D3$, D4$

            For XW = 1 To 40
                Input #2, H1$(XW)
            Next

            Close 2

            For XW = 1 To 40

                If H1$(XW) <> Chr$(42) Then
                    statFile$ = diskPaths$(1) + H1$(XW)
                    Print "DELETING "; H1$(XW); " STATS"
                    If _FileExists (statFile$) Then Kill statFile$
                    If _FileExists (statFile$ + "R") Then Kill statFile$ + "R"
                    If _FileExists (statFile$ + "S") Then Kill statFile$ + "S"
                    If _FileExists (statFile$ + "D") Then Kill statFile$ + "D"
                End If

            Next

        End If 'Done with keypress from CsMsgBox

    End If 'Done checking for ESC

End Sub
