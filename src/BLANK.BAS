'----------------------------------------
'            BLANK Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a league, and then proceeds to delete all 
'stats files for all the associated teams
Sub BLANK ()

    _Title "Full Count Baseball - Erase League Stats"

    Dim statFiles$(TEAMS_PER_LEAGUE)

    leagueFile$ = ChooseFile$ (diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        A$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(A$) = "Y" Then

            Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #2

            Input #2, league$, div1Name$, div2Name$, div3Name$, div4Name$

            For XW = 1 To TEAMS_PER_LEAGUE
                Input #2, statFiles$(XW)
            Next

            Close 2

            For XW = 1 To TEAMS_PER_LEAGUE

                If statFiles$(XW) <> Chr$(42) Then
                    statFile$ = diskPaths$(1) + statFiles$(XW)
                    Print "DELETING "; statFiles$(XW); " STATS"
                    If _FileExists (statFile$) Then Kill statFile$
                    If _FileExists (statFile$ + "R") Then Kill statFile$ + "R"
                    If _FileExists (statFile$ + "S") Then Kill statFile$ + "S"
                    If _FileExists (statFile$ + "D") Then Kill statFile$ + "D"
                End If

            Next

        End If 'Done with keypress from CsMsgBox

    End If 'Done checking for ESC

End Sub
