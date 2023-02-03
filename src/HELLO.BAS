$Debug

Const GAME_VERSION$ = "6.62"
'Const MAX_CONFERENCES = 40
'Const TEAMS_PER_CONFERENCE = 30
'Const TEAMS_PER_DIV = 15
'Const DIVS_PER_CONFERENCE = 2

Const NUM_STATRECORDS = 162
Const MAX_TEAMS = 500
Const MAX_GAMES = 1500
Const MAX_FILES = 1024 '1048576

DefInt A-Z

'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'
'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'TITLE.BI'
'$INCLUDE: 'Variables.BI'

Dim cmplZ2$(298), cmplZ3$(298)
Dim BA$(40), ER$(40), NM$(46)
Dim cmplZ1!(298, 2)
Dim DFT!(40, 20), JS!(46, 42), OFT!(40, 20)

Dim H$(NUM_STATRECORDS), L$(NUM_STATRECORDS)
Dim PL$(NUM_STATRECORDS), W$(NUM_STATRECORDS)

Dim H%(NUM_STATRECORDS), I%(NUM_STATRECORDS), V%(NUM_STATRECORDS)

'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED
Data TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,CURRENT HITTING STREAK
Data LONGEST HITTING STREAK,GAMES,GROUND INTO DP,PUTOUTS,ASSISTS
Data SACRIFICE HITS,SACRIFICE FLIES
Data FIELDING AVERAGE,BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,AT BATS PER HOME RUN
Data AT BATS PER STRIKEOUT,AT BATS PER WALK,AT BATS PER RBI,AT BATS PER RUNS SCORED
Data AT BATS PER DOUBLE PLAY
Data RUNNERS CAUGHT STEALING,GO TO MAIN MENU
For I = 0 To 31: Read TP$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED
Data TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,CURRENT HITTING STREAK
Data LONGEST HITTING STREAK,GAMES,GROUND INTO DP,PUTOUTS,ASSISTS
Data SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,AT BATS PER HOME RUN
Data AT BATS PER STRIKEOUT,AT BATS PER WALK,AT BATS PER RBI,AT BATS PER RUNS SCORED
Data AT BATS PER DOUBLE PLAY
Data SB ATTEMPTS AGAINST,RUNNERS CAUGHT STEALING,% CAUGHT STEALING
For I = 1 To 32: Read TP0$(I): Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKE OUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data LEADS HELD,RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
Data SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT,RETURN TO MAIN MENU
For I = 0 To 29: Read TP1$(I): Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKEOUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data SACRIFICE FLIES,SACRIFICE HITS
Data RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,RUN SUPPORT,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
For I = 1 To 28: Read TP1A$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO
Data FIELDING AVERAGE,INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,SACRIFICE HITS,SACRIFICE FLIES,RETURN TO MAIN MENU
For I = 0 To 39: Read TP2$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,LEFT ON BASE,DOUBLE PLAYS GR. INTO
Data PUTOUTS,ASSISTS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.
For I = 0 To 20: Read TP2A$(I): Next

Data INNINGS PITCHED,HITS ALLOWED,RUNS ALLOWED,EARNED RUNS,WALKS,STRIKEOUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLEPLAYS TURNED,HOME RUNS ALLOWED,SACRIFICE HITS,SACRIFICE FLIES,RUNERS INHERITED
Data RUNNERS INHERITED SCORED,BLOWN SAVES,QUALITY STARTS,PER CENT OF RI SCORED,EARNED RUN AVERAGE
For I = 0 To 18: Read TP3$(I): Next

Data A,B,C,D,E,F,G,H,I,J,K,L,M
Data 681,663,645,627,609,591,573,555,537
For I = 0 To 12: Read mgrX$(I): Next
For I = 1 To 9: Read AB%(I): Next

Data LP,L,S,R,RP
Data dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph
For I = 0 To 4: Read B1$(I): Next
For I = 0 To 10: Read C$(I): Next I

Data "OUTDOOR","INDOOR ","GRASS","TURF"
For I = 0 To 3: Read BP$(I): Next

Data HH,C1,C2,CC
For X = 0 To 3: Read MO$(X): Next

Data 15,0,15,1,19,4,4,6,3,1,2,3,14
For X = 1 To 13: Read E%(X): Next

'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Full Count Baseball - Menu"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 3
        Print #1, _CWD$ + "\"
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, diskPaths$(I)
Next
Close 1


Selection% = 1

Do
    Call Main_Menu(Selection%)
Loop Until Selection% = 15

End

System

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    ReDim menuChoices$(1 To 15)

    Do
        Cls
        Color 15, 0

        Call TITLE
        ReDim menuChoices$(1 To 15)
        menuChoices$(1) = "PLAY A GAME                     "
        menuChoices$(2) = "ENTER NEW/CHANGE EXISTING TEAMS "
        menuChoices$(3) = "PROGRAMMABLE MANAGER            "
        menuChoices$(4) = "AUTO SCHEDULER/BOXSCORE OPTIONS "
        menuChoices$(5) = "DRAFT A TEAM                    "
        menuChoices$(6) = "TRADE PLAYERS                   "
        menuChoices$(7) = "VIEW/PRINT COMPILED STATS       "
        menuChoices$(8) = "MODIFY REPLAY STATS             "
        menuChoices$(9) = "LEAGUE LEADERS AND STANDINGS    "
        menuChoices$(10) = "LEAGUE UTILITIES                "
        menuChoices$(11) = "ACTIVATE/DEACTIVATE PLAYERS     "
        menuChoices$(12) = "GROUNDBALL/FLYOUT CALCS         "
        menuChoices$(13) = "HARD DRIVE INSTALLATION         "
        menuChoices$(14) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(15) = "EXIT TO DOS                     "
        Selection% = 1
        Count% = 15
        Start% = 1
        ExitCode% = 15
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 7
        Column% = 25

        Call box0(6, 24, 22, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Run "LOADER.EXE"
                Case 2: Call BINPUT
                Case 3: Call PROGMAN
                Case 4: Call SCHEDULE
                Case 5: Call DRAFT
                Case 6: Call TRADE
                Case 7: Call LOOKY
                Case 8: Call SINPUT
                Case 9: Call Leaders_Menu
                Case 10: Call Replay_Menu
                Case 11: Call ACTIVATE
                Case 12: Call GROUND
                    'Case 13: Call HDCOPY
                Case 14: Call ConfigureDrives
                Case 15: Cls
            End Select
        End If

    Loop Until Selection% = 15 Or ExitCode% = 27

    menuSelection% = Selection%
    If ExitCode% = 27 Then menuSelection% = 15

End Sub

'----------------------------------------
'       Configure_Drives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Color 15, 1
    FG = 0
    BG = 7

    For I = 0 To 3
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print
    Locate , 31: Print "PATHNAME EXAMPLES:"
    Print
    Locate , 31: Print "A:"
    Locate , 31: Print "C:\FULL\TEAMS"

    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 30), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 30), FG, BG, -1
    Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
    Locate , 18: QPrint PadRight$(diskPaths$(2), 30), FG, BG, -1
    Locate 13, 1: QPrint "Path for Manager Profiles ", 14, 1, -1
    Locate , 27: QPrint PadRight$(diskPaths$(3), 30), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    GetDiv3:
    Locate 11, 18 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(2) = _Trim$(diskPaths$(2))

    GetDiv4:
    Locate 13, 27 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(3), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
    diskPaths$(3) = _Trim$(diskPaths$(3))

    Done:
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv4
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

    Open "DEV.BAS" For Output As #1
    For R = 0 To 3:
        diskPaths$(R) = diskPaths$(R) + "\"
        Print #1, diskPaths$(R)
    Next R

    Close #1

    '620 GoTo 1
End Sub

'----------------------------------------
'       Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

    Do

        Cls

        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 8)
        menuChoices$(1) = "CREATE/EDIT A LEAGUE                 "
        menuChoices$(2) = "UPDATE STANDINGS/LEADERS FOR A LEAGUE"
        menuChoices$(3) = "EXPANDED LEADERS AND STANDINGS       "
        menuChoices$(4) = "VIEW RESULTS FOR A LEAGUE            "
        menuChoices$(5) = "PRINT RESULTS FOR A LEAGUE           "
        menuChoices$(6) = "DELETE ALL STATS FOR A LEAGUE        "
        menuChoices$(7) = "RETURN TO FULL COUNT MAIN MENU       "
        menuChoices$(8) = "EXIT TO DOS                          "
        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 23
        Call box0(7, 22, 16, 60, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call CREATE
                Case 2: Call LEAGCMPL
                Case 3: Call NEWLDR
                Case 4: Call SEELEAG
                Case 5: Call PRTLEAG
                Case 6: Call BLANK
                Case 7: Cls
            End Select
        End If

    Loop Until Selection% >= 7 Or ExitCode% = 27

    If Selection% = 8 Then End

End Sub

'----------------------------------------
'       Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

    Do
        Cls
        
        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 6)
        menuChoices$(1) = "COMPARE COMPILED STATS VS. ACTUAL"
        menuChoices$(2) = "MERGE TWO STAT FILES             "
        menuChoices$(3) = "VIEW TEAM HEAD TO HEAD RESULTS   "
        menuChoices$(4) = "CAREER FILE OPTIONS              "
        menuChoices$(5) = "SORT PLAYERS BY POSITION         "
        menuChoices$(6) = "RETURN TO FULL COUNT MAIN MENU   "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 23
        Call box0(7, 22, 14, 56, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call COMPARE
                Case 2: Call MERGE
                Case 3: Call REC
                Case 4: Call CAREER
                Case 5: Call POSSRT
                Case 6: Cls
            End Select
        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'
'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'TITLE.BM'
'$INCLUDE: 'ACTIVATE.BAS'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BINPUT.BAS'
'$INCLUDE: 'BLANK.BAS'
'$INCLUDE: 'CAREDIT.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARLDR.BAS'
'$INCLUDE: 'COMPARE.BAS'
'INCLUDE: 'CREATE.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'GROUND.BAS'
'INCLUDE: 'LEAGCMPL.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'NEWLDRRC.BAS'
'$INCLUDE: 'NEWLDRST.BAS'
'$INCLUDE: 'POSSRT.BAS'
'$INCLUDE: 'POSCR.BAS'
'$INCLUDE: 'PROGMAN.BAS'
'$INCLUDE: 'PRTLEAG.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'INCLUDE: 'SEELEAG.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
'$INCLUDE: 'UMPMGR.BAS'




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
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                LH$ = Left$(WE$, 8)

                'Initialize values first
                C$ = ","

                Erase cmplZ2$, cmplZ3$
                Erase cmplZ1!

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

                Open diskPaths$(2) + targetFile$ For Input As #2
                Input #2, LG$, D1$, D2$, D3$, D4$
                For X = 1 To 40:
                    Input #2, TM$(X)
                Next
                Close 2

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

                    fullWins = 0
                    fullLosses = 0

                    If H1$(XW) <> Chr$(42) And _FileExists(diskPaths$(1) + H1$(XW)) Then
                        Print "FINDING "; H1$(XW); " STATS"
                        fullWins = 0
                        fullLosses = 0
                        Open diskPaths$(1) + H1$(XW) For Input As #1
                        Input #1, TN$, fullWins, fullLosses
                    End If

                    L = Len(H1$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(H1$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    skipCheck = 0

                    For X = 1 To 40:

                        If TM$(X) = WT$ Then

                            If O1(X) = fullWins And O2(X) = fullLosses Then
                                skipCheck = 1
                                Exit For
                            Else
                                O1(X) = fullWins
                                O2(X) = fullLosses
                            End If

                        End If

                    Next X

                    If skipCheck <> 1 Then

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

                            'Here is where AVG is assigned 0
                            If (JS!(J, 8) + JS!(J, 1)) / (fullWins + fullLosses) < 3.1 Then
                                JS!(J, 14) = 0
                                JS!(J, 15) = 0
                                JS!(J, 16) = 0
                            Else
                                JS!(J, 16) = JS!(J, 2) / JS!(J, 1)
                                JS!(J, 14) = (JS!(J, 2) + JS!(J, 5) + 2 * JS!(J, 6) + 3 * JS!(J, 7)) / JS!(J, 1)
                                JS!(J, 15) = (JS!(J, 2) + JS!(J, 8)) / (JS!(J, 1) + JS!(J, 8))
                            End If
                        Next

                        For J = 25 To 46
                            If JS!(J, 1) = 0 Or (JS!(J, 1) / 3) < (fullWins + fullLosses) Then
                                JS!(J, 14) = 99.99
                            Else
                                JS!(J, 14) = JS!(J, 4) / JS!(J, 1) * 27
                            End If
                        Next

                        Call CheckLeagueStats(H1$(XW), WT$)

                    End If 'Done checking if we need to skip

                    Close #1

                Next XW

                'Sort record by games won
                For VV = 1 To 4

                    AA = ((VV - 1) * 10) + 1
                    ZZ = VV * 10

                    For X = AA To ZZ - 1

                        For Y = X + 1 To ZZ:

                            diffX = O1(X) - O2(X)
                            diffY = O1(Y) - O2(Y)

                            If diffX <= diffY Then
                                If Not (diffX = diffY And O1(X) <= O1(Y) And O1(X) > O2(X)) Then
                                    If Not (diffX = diffY And O1(X) >= O1(Y) And O1(X) < O2(X)) Then
                                        Swap TM$(X), TM$(Y)
                                        Swap O1(X), O1(Y)
                                        Swap O2(X), O2(Y)
                                    End If
                                End If
                            End If

                        Next Y

                    Next X

                Next VV

                Print "POSTING "; LG$; " LEADERS"

                Open diskPaths$(2) + LH$ + ".LDR" For Output As #2

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
Sub CheckLeagueStats (targetTeam$, WT$)

    Dim PS$(40)

    Shared NM$()
    Shared cmplZ2$(), cmplZ3$()
    Shared cmplZ1!(), DFT!(), JS!(), OFT!()

    Print "CHECKING "; targetTeam$; " STATS"

    Cls

    For X = 1 To 23

        If NM$(X) <> "XXX" Then

            Print "     CHECKING STATS OF "; NM$(X)

            PS$(X) = WT$

            skipIF = 0

            For Y = 1 To 20

                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then

                    cmplZ1!(Y, 1) = JS!(X, 13)
                    cmplZ1!(Y, 2) = JS!(X, 1)

                    If cmplZ1!(Y, 2) < 2.5 * (fullWins + fullLosses) Then
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    End If

                    For Y = 1 To 19
                        For Z = Y + 1 To 20
                            If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                                Swap cmplZ3$(Y), cmplZ3$(Z)
                                Swap cmplZ2$(Y), cmplZ2$(Z)
                                For Z1 = 1 To 2:
                                    Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3318

                End If

            Next Y

            If JS!(X, 16) >= cmplZ1!(20, 1) Then

                cmplZ2$(20) = NM$(X)
                cmplZ3$(20) = PS$(X)
                cmplZ1!(20, 1) = JS!(X, 16)
                cmplZ1!(20, 2) = JS!(X, 1)

                For Y = 1 To 19
                    For Z = Y + 1 To 20
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            Swap cmplZ3$(Y), cmplZ3$(Z)
                            Swap cmplZ2$(Y), cmplZ2$(Z)
                            For Z1 = 1 To 2:
                                Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3318 '
            If (JS!(X, 8) + JS!(X, 1)) / (fullWins + fullLosses) >= 3.1 Then

                If JS!(X, 16) <> 0 And JS!(X, 16) <= cmplZ1!(258, 1) Then

                    cmplZ2$(258) = NM$(X)
                    cmplZ3$(258) = PS$(X)
                    cmplZ1!(258, 1) = JS!(X, 16)
                    cmplZ1!(258, 2) = JS!(X, 1)

                    For Y = 239 To 257
                        For Z = Y + 1 To 258
                            If cmplZ1!(Y, 1) > cmplZ1!(Z, 1) Then
                                Swap cmplZ3$(Y), cmplZ3$(Z)
                                Swap cmplZ2$(Y), cmplZ2$(Z)
                                For Z1 = 1 To 2:
                                    Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                End If

            End If

            3320 '
            For Y = 99 To 108

                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then

                    cmplZ1!(Y, 1) = JS!(X, 14)
                    cmplZ1!(Y, 2) = JS!(X, 1)

                    If cmplZ1!(Y, 2) < (fullWins + fullLosses) * 2.5 Then
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    End If

                    For Y = 99 To 107
                        For Z = Y + 1 To 108
                            If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                                Swap cmplZ2$(Y), cmplZ2$(Y)
                                Swap cmplZ3$(Y), cmplZ3$(Z)
                                For Z1 = 1 To 2:
                                    Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3325

                End If

            Next Y

            If JS!(X, 14) >= cmplZ1!(108, 1) Then

                cmplZ2$(108) = NM$(X)
                cmplZ3$(108) = PS$(X)
                cmplZ1!(108, 1) = JS!(X, 14)
                cmplZ1!(108, 2) = JS!(X, 1)

                cmplZ1!(Y, 1) = JS!(X, 14)
                cmplZ1!(Y, 2) = JS!(X, 1)

                If cmplZ1!(Y, 2) < (fullWins + fullLosses) * 2.5 Then
                    cmplZ2$(Y) = "*"
                    cmplZ3$(Y) = "*"
                    cmplZ1!(Y, 1) = 0
                    cmplZ1!(Y, 2) = 0
                End If

                For Y = 99 To 107
                    For Z = Y + 1 To 108
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            Swap cmplZ2$(Y), cmplZ2$(Y)
                            Swap cmplZ3$(Y), cmplZ3$(Z)
                            For Z1 = 1 To 2:
                                Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

            End If

            3325 '
            For Y = 110 To 119

                If cmplZ2$(Y) = NM$(X) And cmplZ3$(Y) = WT$ Then

                    cmplZ1!(Y, 1) = JS!(X, 15)
                    cmplZ1!(Y, 2) = JS!(X, 1)

                    If cmplZ1!(Y, 2) < (fullWins + fullLosses) * 2.5 Then
                        cmplZ2$(Y) = "*"
                        cmplZ3$(Y) = "*"
                        cmplZ1!(Y, 1) = 0
                        cmplZ1!(Y, 2) = 0
                    End If

                    For Y = 110 To 118
                        For Z = Y + 1 To 119
                            If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                                Swap cmplZ2$(Y), cmplZ2$(Y)
                                Swap cmplZ3$(Y), cmplZ3$(Z)
                                For Z1 = 1 To 2:
                                    Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3330

                End If

            Next Y

            3327 '
            If JS!(X, 15) >= cmplZ1!(119, 1) Then

                cmplZ2$(119) = NM$(X)
                cmplZ3$(119) = PS$(X)
                cmplZ1!(119, 1) = JS!(X, 15)
                cmplZ1!(119, 2) = JS!(X, 1)

                cmplZ1!(Y, 1) = JS!(X, 15)
                cmplZ1!(Y, 2) = JS!(X, 1)

                If cmplZ1!(Y, 2) < (fullWins + fullLosses) * 2.5 Then
                    cmplZ2$(Y) = "*"
                    cmplZ3$(Y) = "*"
                    cmplZ1!(Y, 1) = 0
                    cmplZ1!(Y, 2) = 0
                End If

                For Y = 110 To 118
                    For Z = Y + 1 To 119
                        If cmplZ1!(Y, 1) < cmplZ1!(Z, 1) Then
                            Swap cmplZ2$(Y), cmplZ2$(Y)
                            Swap cmplZ3$(Y), cmplZ3$(Z)
                            For Z1 = 1 To 2
                                Swap cmplZ1!(Y, Z1), cmplZ1!(Z, Z1)
                            Next Z1
                        End If
                    Next Z
                Next Y

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
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 32 To 42

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 5)
                    GoTo 3347
                End If

            Next Z

            If JS!(X, 5) > cmplZ1!(42, 1) Then

                cmplZ1!(42, 1) = JS!(X, 5)
                cmplZ2$(42) = NM$(X)
                cmplZ3$(42) = WT$

                3347 '
                For Z = 32 To 41
                    For Z1 = Z + 1 To 42

                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

            3354 '
            For Z = 43 To 53
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 6)
                    GoTo 3359
                End If
            Next Z

            If JS!(X, 6) > cmplZ1!(53, 1) Then

                cmplZ1!(53, 1) = JS!(X, 6)
                cmplZ2$(53) = NM$(X)
                cmplZ3$(53) = WT$

                3359 '
                For Z = 43 To 52
                    For Z1 = Z + 1 To 53
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 54 To 64
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 7)
                    GoTo 3371
                End If
            Next Z

            If JS!(X, 7) > cmplZ1!(64, 1) Then

                cmplZ1!(64, 1) = JS!(X, 7)
                cmplZ2$(64) = NM$(X)
                cmplZ3$(64) = WT$

                3371 '
                For Z = 54 To 63:
                    For Z1 = Z + 1 To 64:
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3378 '
            For Z = 65 To 74

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 3)
                    GoTo 3383
                End If

            Next Z

            If JS!(X, 3) > cmplZ1!(74, 1) Then

                cmplZ1!(74, 1) = JS!(X, 3)
                cmplZ2$(74) = NM$(X)
                cmplZ3$(74) = WT$

                3383 '
                For Z = 65 To 73
                    For Z1 = Z + 1 To 74
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

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
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

            End If

            For Z = 87 To 97

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 10)
                    GoTo 3407
                End If
            Next Z

            If JS!(X, 10) > cmplZ1!(97, 1) Then

                cmplZ1!(97, 1) = JS!(X, 10)
                cmplZ2$(97) = NM$(X)
                cmplZ3$(97) = WT$

                3407 '
                For Z = 87 To 96
                    For Z1 = Z + 1 To 97
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If 'Done checking for XXX / terminating chars

        3414 '
        For Z = 217 To 227

            If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                cmplZ1!(Z, 1) = JS!(X, 8)
                GoTo 3605
            End If

        Next Z

        If JS!(X, 8) > cmplZ1!(227, 1) Then

            cmplZ1!(227, 1) = JS!(X, 8)
            cmplZ2$(227) = NM$(X)
            cmplZ3$(227) = WT$

            3605 '
            For Z = 217 To 226
                For Z1 = Z + 1 To 227
                    If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                        Swap cmplZ2$(Z1), cmplZ2$(Z)
                        Swap cmplZ3$(Z1), cmplZ3$(Z)
                        Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
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
                        Swap cmplZ2$(Z1), cmplZ2$(Z)
                        Swap cmplZ3$(Z1), cmplZ3$(Z)
                        Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
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
                        Swap cmplZ2$(Z1), cmplZ2$(Z)
                        Swap cmplZ3$(Z1), cmplZ3$(Z)
                        Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                    End If

                Next Z1
            Next Z

        End If

    Next X

    Call CheckMoreStats(WT$)

End Sub

'----------------------------------------
'      CheckMoreStats Subroutine
'----------------------------------------
'...explanation...
Sub CheckMoreStats (WT$)

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

            If JS!(X, 1) / 3 >= (fullWins + fullLosses) And JS!(X, 1) <> 0 Then

                If JS!(X, 14) < 99.9 And JS!(X, 14) >= cmplZ1!(278, 1) Then

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
                                    Swap cmplZ2$(Y1), cmplZ2$(Z)
                                    Swap cmplZ3$(Y1), cmplZ3$(Z)
                                    For Z1 = 1 To 2:
                                        Swap cmplZ1!(Y1, Z1), cmplZ1!(Z, Z1)
                                    Next Z1
                                End If

                            End If

                        Next Y1

                    Next Z

                    GoTo 3440

                End If

            Next Y

            If JS!(X, 7) - JS!(X, 8) >= cmplZ1!(150, 1) - cmplZ1!(150, 2) Then

                If Not (JS!(X, 7) - JS!(X, 8) = cmplZ1!(150, 1) - cmplZ1!(150, 2) And JS!(X, 7) >= cmplZ1!(150, 1)) Then

                    cmplZ2$(150) = NM$(X)
                    cmplZ3$(150) = WT$
                    cmplZ1!(150, 1) = JS!(X, 7)
                    cmplZ1!(150, 2) = JS!(X, 8)

                    '4860 '
                    For Z = 140 To 149:
                        For Y1 = Z + 1 To 150

                            If cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <= cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) Then

                                If cmplZ1!(Z, 1) - cmplZ1!(Z, 2) <> cmplZ1!(Y1, 1) - cmplZ1!(Y1, 2) Or cmplZ1!(Z, 1) > cmplZ1!(Y1, 1) Then
                                    Swap cmplZ2$(Y1), cmplZ2$(Z)
                                    Swap cmplZ3$(Y1), cmplZ3$(Z)
                                    For Z1 = 1 To 2:
                                        Swap cmplZ1!(Y1, Z1), cmplZ1!(Z, Z1)
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
                                Swap cmplZ2$(Z1), cmplZ2$(Z)
                                Swap cmplZ3$(Z1), cmplZ3$(Z)
                                Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                            End If
                        Next Z1
                    Next Z

                    GoTo 3445

                End If

            Next Z

            If JS!(X, 7) > cmplZ1!(161, 1) Then

                cmplZ1!(161, 1) = JS!(X, 7)
                cmplZ2$(161) = NM$(X)
                cmplZ3$(161) = WT$

                3445 '
                For Z = 151 To 160

                    For Z1 = Z + 1 To 161

                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If

                    Next Z1

                Next Z

            End If

            3452 '
            For Z = 162 To 172

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 1) / 3
                    GoTo 3457
                End If

            Next Z

            If JS!(X, 1) / 3 > cmplZ1!(172, 1) Then

                cmplZ1!(172, 1) = JS!(X, 1) / 3
                cmplZ2$(172) = NM$(X)
                cmplZ3$(172) = WT$

                3457 '
                For Z = 162 To 171
                    For Z1 = Z + 1 To 172
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3464 '
            For Z = 173 To 183
                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 6)
                    GoTo 3469
                End If
            Next Z

            If JS!(X, 6) > cmplZ1!(183, 1) Then

                cmplZ1!(183, 1) = JS!(X, 6)
                cmplZ2$(183) = NM$(X)
                cmplZ3$(183) = WT$

                3469 '
                For Z = 173 To 182:
                    For Z1 = Z + 1 To 183:
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
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

                cmplZ1!(194, 1) = JS!(X, 11)
                cmplZ2$(194) = NM$(X)
                cmplZ3$(194) = WT$

                3481 '
                For Z = 184 To 193
                    For Z1 = Z + 1 To 194
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3488 '
            For Z = 195 To 205

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 12)
                    GoTo 3493
                End If
            Next Z

            If JS!(X, 12) > cmplZ1!(205, 1) Then
                cmplZ1!(205, 1) = JS!(X, 12)
                cmplZ2$(205) = NM$(X)
                cmplZ3$(205) = WT$

                3493 '
                For Z = 195 To 204
                    For Z1 = Z + 1 To 205
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            3500 '
            For Z = 206 To 216

                If cmplZ2$(Z) = NM$(X) And cmplZ3$(Z) = WT$ Then
                    cmplZ1!(Z, 1) = JS!(X, 13)
                    GoTo 3505
                End If
            Next Z

            If JS!(X, 13) > cmplZ1!(216, 1) Then

                cmplZ1!(216, 1) = JS!(X, 13)
                cmplZ2$(216) = NM$(X)
                cmplZ3$(216) = WT$

                3505 '
                For Z = 206 To 215
                    For Z1 = Z + 1 To 216
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

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

                cmplZ1!(238, 1) = JS!(X, 9)
                cmplZ2$(238) = NM$(X)
                cmplZ3$(238) = WT$

                3518 '
                For Z = 228 To 237:
                    For Z1 = Z + 1 To 238:
                        If cmplZ1!(Z, 1) < cmplZ1!(Z1, 1) Then
                            Swap cmplZ2$(Z1), cmplZ2$(Z)
                            Swap cmplZ3$(Z1), cmplZ3$(Z)
                            Swap cmplZ1!(Z1, 1), cmplZ1!(Z, 1)
                        End If

                    Next Z1
                Next Z

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

        OFT!(40, 1) = BAT%(4) / BAT%(3)

        For M = 3 To 14: OFT!(40, M) = BAT%(M): Next M

        BA$(40) = WT$
        'BA$(40) = WT$

        3535 '
        For Z = 1 To 39

            For Z1 = Z + 1 To 40

                If OFT!(Z, 1) < OFT!(Z1, 1) Then

                    TN$ = BA$(Z1)
                    BA$(Z1) = BA$(Z)
                    BA$(Z) = TN$

                    For M = 1 To 14:
                        Swap OFT!(Z1, M), OFT!(Z, M)
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

                    TN$ = ER$(Z1)
                    ER$(Z1) = ER$(Z)
                    ER$(Z) = TN$

                    For M = 1 To 14:
                        Swap DFT!(Z1, M), DFT!(Z, M)
                    Next M

                End If

            Next Z1

        Next Z

        For Z = 1 To 40:
            If DFT!(Z, 1) = DFT!(40, 1) Then ER$(Z) = "*": ER$(Z) = "*"
        Next Z

    End If

End Sub





'----------------------------------------




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





'------------------------------

Sub CREATE ()

    _Title "Full Count Baseball - Create League"

    DefInt A-Z

    Shared Z1%()
    Shared Z1$(), Z2$()

    'determine if the monitor is mono or color
    FG = 0
    BG = 7

    Cls

    Color 12: Print Tab(31); "LEAGUE EDITOR MENU": Print
    Color 14: Print "(1)";: Color 15: Print " EDIT AN EXISTING LEAGUE": Print
    Color 14: Print "(2)";: Color 15: Print " CREATE A NEW LEAGUE": Print
    Color 14: Print "(3)";: Color 15: Print " RETURN TO STANDINGS/LEADERS MENU"
    Do
        I$ = GetKeyPress$
        I1 = Val(I$)
    Loop Until I1 >= 1 And I1 <= 3 Or I$ = Chr$(27)

    If I$ <> Chr$(27) Then

        For X = 1 To 40
            Z1$(X) = "XXX"
            Z2$(X) = "XXX"
            Z1%(X) = -1
        Next

        Select Case I1

            Case 1:

                Call ChooseLeague(LG$)

                If LG$ <> "" Then
                    Color 15, 1
                    Call EditLeague(LG$)
                End If

            Case 2:

                Do

                    Color 15, 0
                    Cls
                    JJ = 7
                    Locate 6, 8: Color 15, 1
                    'Print box top
                    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

                    'Print box sides
                    For II = 1 To JJ
                        Locate 6 + II, 8
                        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                    Next

                    'Print box bottom
                    Locate 7 + JJ, 8
                    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                    Locate 7, 32
                    Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
                    Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
                    Locate , 12: Print "SPACES."
                    Locate , 12: Print "SAMPLE NAMES ARE NL1990 AL56 1956AL NATIONAL"
                    Locate 7, 37: Input LG$

                    If LG$ <> "" Then
                        LG$ = Left$(LG$, 8)

                        Do
                            Locate 13, 12: Print "ANY CHANGE (YN) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    End If

                Loop Until UCase$(I$) = "N" Or LG$ <> ""

                Call EditLeague(LG$)

            Case 3: Cls '

        End Select

    End If

End Sub

'----------------------------------------
'      ChooseLeague Subroutine
'----------------------------------------
'...explanation...
Sub ChooseLeague (leagueFile$)

    Shared D1$, D2$, D3$, D4$
    Shared Z1%()
    Shared Z1$(), Z2$()

    Do

        Spec$ = diskPaths$(2) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            'GoTo 50
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
                Print "SELECT LEAGUE TO SCHEDULE": Color 15
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
                leagueFile$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(2) + leagueFile$ + ".LIB" For Input As #2
                Input #2, leagueFile$
                Input #2, D1$
                Input #2, D2$
                Input #2, D3$
                Input #2, D4$

                For X = 1 To 40
                    Input #2, Z1$(X)
                    Input #2, Z1%(X)
                    Input #2, Z2$(X)
                Next

                Close 2

            End If 'Done checking for ESC

        End If 'Done checking for files

    Loop Until UCase$(I$) = "N" Or ExitCode% = 27 Or Count% = 0

    Color 15, 0
    Cls

End Sub

'----------------------------------------
'       EditLeague Subroutine
'----------------------------------------
'...explanation...
Sub EditLeague (LG$)

    Shared D1$, D2$, D3$, D4$
    Shared L%(), Z1%()
    Shared Z1$(), Z2$()

    FG = 0
    BG = 7

    Cls
    Color 15, 1
    Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D1$, 8), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D2$, 8), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D3$, 8), FG, BG, -1
    Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(D4$, 8), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 11, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
    Call TextIn(D4$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

    Done:
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv4
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

    Do
        220 '
        Color 14, 0
        Cls
        Print
        Print "  "; LG$; " LEAGUE ": Print: Color 12
        Locate , 4: Print D1$; Tab(24); D2$; Tab(44); D3$; Tab(64); D4$

        For I = 1 To 10
            Color 2: Print Using "## "; I;: Color 15: Print Z1$(I);
            Locate , 21: Color 2: Print Using "## "; I + 10;: Color 15: Print Z1$(I + 10);
            Locate , 41: Color 2: Print Using "## "; I + 20;: Color 15: Print Z1$(I + 20);
            Locate , 61: Color 2: Print Using "## "; I + 30;: Color 15: Print Z1$(I + 30)
        Next I

        Do
            Locate 19, 1
            Color 14: Print "(1) ";: Color 15: Print "ADD A TEAM"
            Color 14: Print "(2) ";: Color 15: Print "REMOVE A TEAM"
            Color 14: Print "(3) ";: Color 15: Print "END AND SAVE SETTINGS"
            I$ = GetKeyPress$
        Loop Until Val(I$) >= 1 Or Val(I$) <= 3

        If I$ <> "3" Then
            Do
                Locate 20, 30: Print "SLOT # (1-40) ";
                N = 1
                Input B
            Loop Until B >= 1 Or B <= 40

            Select Case I$

                Case "1":
                    Call GetTeam_CREATE(YN$, T1$, teamIdx)
                    Z1$(B) = T1$
                    Z2$(B) = YN$
                    Z1%(B) = teamIdx

                Case "2":
                    Z1$(B) = "XXX"

            End Select 'Done making sure we don't act on a 4+

        Else

            'Save settings
            Cls

            Open diskPaths$(2) + LG$ + ".LIB" For Output As #2

            Print #2, LG$
            Print #2, D1$
            Print #2, D2$
            Print #2, D3$
            Print #2, D4$

            For X = 1 To 40
                Print #2, Z1$(X)
                Print #2, Z1%(X)
                Print #2, Z2$(X)
            Next

            Close 2

            For X = 1 To 40

                If Z1$(X) <> "XXX" Then
                    For D = 4 To 11
                        If Mid$(Z1$(X), D, 1) = Chr$(32) Or Mid$(Z1$(X), D, 1) = "." Then Mid$(Z1$(X), D, 1) = "_"
                    Next D

                    QQ = 8
                    For D = 11 To 4 Step -1
                        If Mid$(Z1$(X), D, 1) = "_" Then
                            QQ = D - 4
                        Else
                            Exit For
                        End If
                    Next D

                    L = Len(Z1$(X))
                    YR$ = Left$(Z1$(X), 2)
                    TM$ = Mid$(Z1$(X), 4, QQ)
                    Z1$(X) = TM$ + "." + YR$
                Else
                    Z1$(X) = Chr$(42)
                End If

            Next X

        End If 'Done checking value of I$

    Loop Until I$ = "3"

    Open diskPaths$(2) + LG$ + ".TMS" For Output As #2
    Write #2, LG$
    Write #2, D1$
    Write #2, D2$
    Write #2, D3$
    Write #2, D4$

    For X = 1 To 40
        Write #2, Z1$(X)
    Next

    Close 2


End Sub

'----------------------------------------
'       GetTeam_CREATE Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CREATE (yearNumber$, teamName$, teamIdx)

    Shared L%()

    Do
        '1440
        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" And yearNumber$ <> "" Then
            'NY$ = yearNumber$
            Call SelectTeam_CREATE(yearNumber$, numberTeams, teamIdx)
        Else
            yearNumber$ = NY$
            Erase teamNames$
            numberTeams = 0
            If Not _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then
                Cls
                Print "ERROR:: UNABLE TO READ TEAM FILE": Print
                BO% = 1:
            End If

            Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342
            Y = LOF(1)
            Field #1, 15 As Q$(0)
            For I1 = 1 To Y / 4342
                Get #1, I1
                For I2 = 1 To 15
                    If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                Next I2
                If nullTeam <> 1 Then
                    numberTeams = numberTeams + 1
                    teamNames$(numberTeams) = Q$(0)
                    teamIndex%(numberTeams) = I1
                End If
            Next I1

            Close 1

            Call SelectTeam_CREATE(yearNumber$, numberTeams, teamIdx)

        End If

        teamName$ = RTrim$(Q$(0))

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color L%(11), L%(12): Print teamName$

            Color 15, 4

            Do
                Locate , 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'      SelectTeam_CREATE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CREATE (yearNumber$, numberTeams, teamIdx)

    Shared L%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4
    Cls
    Locate , 33: Color 14: Print "TEAM SELECTION"
    Color 15
    Locate 25, 1
    Color 14: Print "ESC";
    Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
    Color 14: Print "ENTER";
    Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIdx = teamIndex%(Selection%)

        Locate 2, 33
        Color 31: Print "LOADING..."

        Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342
        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
        Get #1, teamIdx

        For I1 = 1 To 13: L%(I1) = CVI(Q$(I1)): Next
        Close 1

    Else
        BO% = 1
    End If

End Sub

