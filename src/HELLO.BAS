
Const GAME_VERSION$ = "6.65"
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
'$INCLUDE: 'CREATE.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'GROUND.BAS'
'$INCLUDE: 'LEAGCMPL.BAS'
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
'$INCLUDE: 'SEELEAG.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
'$INCLUDE: 'UMPMGR.BAS'

