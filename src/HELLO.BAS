$Debug
$Color:32

Const GAME_VERSION$ = "7.00"
Const MAX_FILES = 10240
Const MAX_CAREER_YEARS = 15
'Const MAX_CONFERENCES = 40
Const MAX_SCHED_STATS = 162
Const MAX_SCHEDULE_GAMES = 1500
Const MAX_TOURNAMENT_GAMES = 200
Const MAX_TEAMS = 1000
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV = 10
Const DIVS_PER_LEAGUE = 4
Const TEAMS_PER_LEAGUE = 40

Const SCHEDULE_SIZE_BYTES = 52
Const LAST_COL = 80
Const LAST_ROW = 30
Const SCHED_ENTRIES_PER_SCREEN = 25

DefInt A-Z

' General purpose
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'StringManip.bi'
'$Include: 'QBWRITE.bi'

' For Utils
'$Include: 'BoxRoutines.bi'
'$Include: 'FileTeamSelection.bi'
'$Include: 'Variables.bi'
'$Include: 'MainRoutines.bi'
'$Include: 'TITLE.bi'

' For Game
'$Include: 'GameRoutines.bi'
'$Include: 'GameRoutines_Gfx.bi'
'$Include: 'SOURCE.bi'
'$Include: 'Consts.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------

'Defaults for Event Settings
Data 15,0,15,1,19,4,4,6,3,1,2,3,14
For X = 1 To 13:
    Read eventSettings(X)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED
Data TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,CURRENT HITTING STREAK
Data LONGEST HITTING STREAK,GAMES,GROUND INTO DP,PUTOUTS,ASSISTS
Data SACRIFICE HITS,SACRIFICE FLIES
Data FIELDING AVERAGE,BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,AT BATS PER HOME RUN
Data AT BATS PER STRIKEOUT,AT BATS PER WALK,AT BATS PER RBI,AT BATS PER RUNS SCORED
Data AT BATS PER DOUBLE PLAY
Data RUNNERS CAUGHT STEALING,GO TO MAIN MENU
For I = 0 To 31:
    Read expBatCategory$(I)
Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKE OUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data LEADS HELD,RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
Data SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT,RETURN TO MAIN MENU
For I = 0 To 29
    Read expPitCategory$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED
Data TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,CURRENT HITTING STREAK
Data LONGEST HITTING STREAK,GAMES,GROUND INTO DP,PUTOUTS,ASSISTS
Data SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,AT BATS PER HOME RUN
Data AT BATS PER STRIKEOUT,AT BATS PER WALK,AT BATS PER RBI,AT BATS PER RUNS SCORED
Data AT BATS PER DOUBLE PLAY
Data SB ATTEMPTS AGAINST,RUNNERS CAUGHT STEALING,% CAUGHT STEALING
For I = 1 To 32:
    Read batIndCategory$(I)
Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKEOUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data SACRIFICE FLIES,SACRIFICE HITS
Data RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,RUN SUPPORT,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
For I = 1 To 28:
    Read pitIndCategory$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO
Data FIELDING AVERAGE,INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,SACRIFICE HITS,SACRIFICE FLIES,RETURN TO MAIN MENU
For I = 0 To 39:
    Read expTeamCategory$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,LEFT ON BASE,DOUBLE PLAYS GR. INTO
Data PUTOUTS,ASSISTS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.
For I = 0 To 20:
    Read batTeamCategory$(I)
Next

Data INNINGS PITCHED,HITS ALLOWED,RUNS ALLOWED,EARNED RUNS,WALKS,STRIKEOUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLEPLAYS TURNED,HOME RUNS ALLOWED,SACRIFICE HITS,SACRIFICE FLIES,RUNERS INHERITED
Data RUNNERS INHERITED SCORED,BLOWN SAVES,QUALITY STARTS,PER CENT OF RI SCORED,EARNED RUN AVERAGE
For I = 0 To 18:
    Read pitTeamCategory$(I)
Next

Data A,B,C,D,E,F,G,H,I,J,K,L,M
For I = 0 To 12:
    Read alpha$(I)
Next

'this is used for both batter and pitcher
'designation of handedness / position
'in game play, this is always manipulated
'to only get the "L"/"R". That is, LP/S/RP
'don't seem to be used. That makes me question
'whether they reference to Relief / Start
'Pitcher or not
Data LP,L,S,R,RP

For I = 0 To 4:
    Read handed$(I)
Next

'Data dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph
Data dh," p"," c",1b,2b,3b,ss,lf,cf,rf,ph,pr

For I = 0 To 11:
    Read fieldPos$(I)
Next

Data "OUTDOOR","INDOOR ","GRASS","TURF"
For I = 0 To 3:
    Read parkType$(I)
Next

'Data to read in to variables
Data first,second,third,short
For I = 3 To 6
    Read grounderDir$(I)
Next

Data 681,663,645,627,609,591,573,555,537
Data 6,12,37,55,66,74,86,96,100
Data 5,10,32,55,67,75,86,96,100
Data 4,8,27,54,68,76,86,96,100
Data 3,7,25,54,69,76,86,96,100
Data 3,6,22,54,69,77,86,96,100
Data 2,5,20,53,69,78,86,96,100
Data 2,4,17,53,70,78,86,96,100
Data 1,3,12,52,71,79,86,96,100
Data 1,2,7,51,72,80,86,96,100
Data 6,14,42,55,63,74,86,96,100
Data 5,12,37,55,64,75,86,96,100
Data 4,10,32,55,65,76,86,96,100
Data 3,8,30,55,66,77,86,96,100
Data 3,7,27,55,66,77,86,96,100
Data 3,6,24,55,66,77,86,96,100
Data 2,5,22,55,67,78,86,96,100
Data 1,4,17,55,68,79,86,96,100
Data 1,3,12,55,69,80,86,96,100
Data 0,0,100
Data 0,15,100
Data 0,30,100
Data 0,45,100
Data 0,60,100
Data 1,75,100
Data 3,90,100
Data 4,97,100
Data 100,0,90,100,75,100,60,100,45,100,30,100,15,100,8,100
Data 100,0,0,90,100,0,70,95,100,50,80,100,30,65,100,10,50,100
Data 0,90,100,5,87,100,10,85,100,15,80,100,20,77,100,25,75,100
Data 30,73,100,35,70,100,40,67,100,45,65,100,50,63,100,60,65,100

'Read in data and assign variables
For I = 1 To 9:
    Read AB%(I)
Next

For I = 0 To 1
    For I1 = 1 To 9
        For I2 = 1 To 9
            Read BT%(I, I1, I2)
        Next
    Next
Next

For I = 2 To 9
    For I1 = 0 To 2
        Read CF%(I, I1)
    Next
Next

For I = 2 To 9
    For I1 = 0 To 1
        Read LB%(I, I1)
    Next
Next

For I = 0 To 5
    For I1 = 0 To 2
        Read SO%(I, I1)
    Next
Next

For I = 11 To 0 Step -1
    For I1 = 0 To 2
        Read triplePlayAdj(I, I1)
    Next
Next

' Game options
modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

pitcherEra$(0) = "1980 - PRESENT"
pitcherEra$(1) = "1973 - 1979   "
pitcherEra$(2) = "1955 - 1972   "
pitcherEra$(3) = "1946 - 1954   "
pitcherEra$(4) = "1933 - 1945   "
pitcherEra$(5) = "1919 - 1932   "
pitcherEra$(6) = "1909 - 1918   "
pitcherEra$(7) = "1894 - 1908   "
pitcherEra$(8) = "1876 - 1893   "

playMode$(0) = "HUMAN OPPONENT           "
playMode$(1) = "COMPUTER IS VISITING TEAM"
playMode$(2) = "COMPUTER IS HOME TEAM    "
playMode$(3) = "COMPUTER VS COMPUTER     "

pitHand$(1) = "LHP"
pitHand$(0) = "RHP"

yesNoText$(0) = "NO "
yesNoText$(1) = "YES"

yesNo$(0) = "N"
yesNo$(1) = "Y"


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Full Count Baseball - Menu"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

If _FileExists("errlog") Then
    Kill "errlog"
End If

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

Select Case opSysType$
    Case "[WINDOWS]":
        dirSeparator$ = "\"
    Case "[MAC]", "[MACOSX]", "[LINUX]":
        dirSeparator$ = "/"
End Select

'Default new folders to current path
For I = 0 To 4

    diskPaths$(I) = _CWD$ + dirSeparator$

Next

'Read-in existing DEV.BAS
If _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Input As #1

    'DEV.BAS will not have the "Schedule" folder configured
    For I = 0 To 3
        Input #1, diskPaths$(I)
    Next

    Close #1

    Kill "DEV.BAS"

End If

'Write FOLDERS.CFG
If Not _FileExists("FOLDERS.CFG") Then

    Open "FOLDERS.CFG" For Output As #2

    For I = 0 To 4
        Print #2, diskPaths$(I)
    Next

    Close #2

End If

'Read in path values
Open "FOLDERS.CFG" For Input As #3

For I = 0 To 4
    Input #3, diskPaths$(I)
Next

Close #3

'Rename records file to new format
If _FileExists("BASEBALL.REC") Then

    Call FCopy("RECORDS.MLB", "BASEBALL.REC", Buff$, copyErr%)
    Kill "BASEBALL.REC"

End If

'Run main menu
SELECTION% = 1

menu& = _NewImage(640, 480, 256)
Screen menu&

Do

    Call Main_Menu(SELECTION%)

Loop Until SELECTION% = 10

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine; "-- "; _ErrorMessage$

If _InclErrorLine Then
    Print #10, "    in module "; _InclErrorFile$; " line "; _InclErrorLine
End If

'If _FileExists("GAME82") Then
'Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
'Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
'End If
Print #10,
Close #10
Resume Next ' moves program to code following the error.


Sub LogError (errMsg$)
    Open "errlog" For Append As #10
    Print #10, errMsg$
    Close #10
End Sub

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection)

    Do

        Cls
        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 10)

        menuChoices$(1) = "PLAY SINGLE GAME          "
        menuChoices$(2) = "TEAM MANAGEMENT           "
        menuChoices$(3) = "REPLAY ASSESSMENT         "
        menuChoices$(4) = "LEADERS AND RECORDS       "
        menuChoices$(5) = "SCHEDULES AND BOXSCORES   "
        menuChoices$(6) = "MISCELLANEOUS CALCULATIONS"
        menuChoices$(7) = "MANAGE UMPIRES            "
        menuChoices$(8) = "PROGRAMMABLE MANAGER      "
        menuChoices$(9) = "CONFIGURE GAME FOLDERS    "
        menuChoices$(10) = "EXIT PROGRAM             "

        menuSelection = 1
        Count% = 10
        Start% = 1
        ExitCode% = 10
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection

                Case 1:
                    If schedGame(0) + schedGame(2) <= schedGame(1) Then
                        activeSchedule = 1
                    Else
                        activeSchedule = 0
                    End If

                    Do
                        Call LOADER
                    Loop Until activeSchedule = 0 or earlyExit = 1

                Case 2:
                    Call TeamMgt_Menu

                Case 3:
                    Call Replay_Menu

                Case 4:
                    Call Leaders_Menu

                Case 5:
                    Call SCHEDULE

                Case 6:
                    Call Calculations_Menu

                Case 7:
                    Call UMPMGR

                Case 8:
                    Call PROGMAN

                Case 9:
                    Call ConfigureDrives_Base

            End Select

        End If

    Loop Until menuSelection = 10 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection = 10

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing a team and
'it's roster.
Sub TeamMgt_Menu ()

    _Title "Full Count Baseball - Team Input"

    Do

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 9)

        menuChoices$(1) = "INPUT A NEW TEAM           "
        menuChoices$(2) = "EDIT AN EXISTING TEAM      "
        menuChoices$(3) = "PRINT TEAM ROSTER          "
        menuChoices$(4) = "VIEW TEAM ROSTER           "
        menuChoices$(5) = "DRAFT PLAYERS              "
        menuChoices$(6) = "TRADE PLAYERS              "
        menuChoices$(7) = "TOGGLE PLAYER STATUS       "
        menuChoices$(8) = "SWAP SPOTS ON A TEAM ROSTER"
        menuChoices$(9) = "RETURN TO MAIN MENU        "
        SELECTION% = 1
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call InputEditTeam(1, YN$, team$, teamNbr)
                    statFile$ = nameStatFile$(team$)
                    Call SaveTeamFile(1, YN$, team$, statFile$, teamNbr)

                Case 2:
                    Call CNGTM(1)

                Case 3:
                    Call CNGTM(2)

                Case 4:
                    Call CNGTM(3)

                Case 5:
                    Call DRAFT

                Case 6:
                    Call TRADE

                Case 7:
                    Call ACTIVATE

                Case 8:
                    teamNbr = 0
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        Call SwapSpots(YN$, team$, teamNbr)
                    End If

            End Select

        End If

    Loop Until menuSelection = 9 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing replay
'data, from statistics to reports and
'team careers
Sub Replay_Menu ()

    Do
        Cls

        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "REVIEW COMPILED STATS           "
        menuChoices$(2) = "COMPARE REPLAY STATS TO ACTUAL  "
        menuChoices$(3) = "MODIFY REPLAY STATS             "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(5) = "TEAM CAREER OPTIONS             "
        menuChoices$(6) = "SORT PLAYERS BY POSITION        "
        menuChoices$(7) = "RETURN TO MAIN MENU             "
        SELECTION% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call COMPARE

                Case 3:
                    Call EditStats

                Case 4:
                    Call MERGE

                Case 5:
                    Call CAREER

                Case 6:
                    Call POSSRT

                Case 7:
                    '

            End Select

        End If

    Loop Until SELECTION% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing team
'leaders, stats and standings
Sub Leaders_Menu ()

    Do

        Cls

        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 8)

        menuChoices$(1) = "MANAGE LEAGUES                 "
        menuChoices$(2) = "UPDATE LEAGUE STATS / STANDINGS"
        menuChoices$(3) = "ACCESS LEAGUE REPORTS          "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS           "
        menuChoices$(5) = "LEAGUE RECORD BOOKS            "
        menuChoices$(6) = "EXPANDED LEADERS / STANDINGS   "
        menuChoices$(7) = "DELETE ALL STATS FOR A LEAGUE  "
        menuChoices$(8) = "RETURN TO MAIN MENU            "
        SELECTION% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call CREATLEAG

                Case 2:
                    Call CompileLeague

                Case 3:
                    Call SEELEAG

                Case 4:
                    Call Hd2HdPro_League
                    'Call HD2HDLEAG

                Case 5:
                    Call SeasonRecords

                Case 6:
                    Call NEWLDR

                Case 7:
                    Call BLANK

            End Select

        End If

    Loop Until SELECTION% = 8 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Calculations_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing a team and
'it's roster.
Sub Calculations_Menu ()

    _Title "Full Count Baseball - Team Input"

    Do

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 4)

        menuChoices$(1) = "GROUNDBALL / FLYOUT CALCS"
        menuChoices$(2) = "CALCULATE BUNT RATINGS   "
        menuChoices$(3) = "CALCULATE RUN RATINGS    "
        menuChoices$(4) = "RETURN TO MAIN MENU      "
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call GROUND

                Case 2:
                    teamNbr = 0
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        statFile$ = nameStatFile$(team$)
                        Call CalculateBuntRating
                        Call SaveTeamFile(0, YN$, team$, statFile$, teamNbr)
                    End If

                Case 3:
                    teamNbr = 0
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        statFile$ = nameStatFile$(team$)
                        Call CalculateRunRating
                        Call SaveTeamFile(0, YN$, team$, statFile$, teamNbr)
                    End If

            End Select

        End If

    Loop Until menuSelection = 4 Or ExitCode% = 27

End Sub


' General purpose
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'StringManip.bm'
'$Include: 'QBWRITE.bm'

' For Utils
'$Include: 'BoxRoutines.bm'
'$Include: 'ConfigureDrives.bm'
'$Include: 'FileTeamSelection.bm'
'$Include: 'TITLE.bm'
'$Include: 'ACTIVATE.BAS'
'$Include: 'ALIGN.BAS'
'$Include: 'BLANK.BAS'
'$Include: 'CAREDIT.BAS'
'$Include: 'CARVIEW.BAS'
'$Include: 'CAREER.BAS'
'$Include: 'CARLDR.BAS'
'$Include: 'CNGTM.BAS'
'$Include: 'COMPARE.BAS'
'$Include: 'COMPLEAG.BAS'
'$Include: 'CONVERT.BAS'
'$Include: 'CREATLEAG.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'GROUND.BAS'
'$Include: 'HD2HDLEAG.BAS'
'$Include: 'LOOKY.BAS'
'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'$Include: 'NEWLDRST.BAS'
'$Include: 'POSSRT.BAS'
'$Include: 'POSCR.BAS'
'$Include: 'PROGMAN.BAS'
'$Include: 'REC.BAS'
'$Include: 'SCHEDULE.BAS'
'$Include: 'SEELEAG.BAS'
'$Include: 'SINPUT.BAS'
'$Include: 'TINPUT.BAS'
'$Include: 'TRADE.BAS'
'$Include: 'UMPMGR.BAS'

' For Game
'Include: 'LOADER.BAS'


'----------------------------------------
'            LOADER Subroutine
'----------------------------------------
'This subroutine is used to setup all of
'the parameters for a baseball game.
'This involves loading parameters, teams,
'rotations, linesup and a park.
Sub LOADER

    Shared actualAttendance&, avgAttendance&

    _Title "Full Count Baseball v" + GAME_VERSION$ + " - PLAY BALL!"

    Shared dayNight$(), desigHitOpt$(), gameType$()
    Shared leagueType$(), month$()
    Shared pitHand$(), pitcherEra$()
    Shared playMode$(), umpireName$()
    Shared teamIndicator$(), yesNoText$()
    Shared gameMgrIdx(), umpireNumber()

    Shared scheduleFile$

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------
    '-- We should udpate this to read in DEFERA
    '-- Then delete it
    '-- And finally write as DEFAULT
    '-- Use same logic as for DEV.BAS
    If _FileExists("DEFERA") Then

        Open "DEFERA" For Input As #2
        Input #2, playerOpt
        Input #2, leagueOpt
        Input #2, desigHit
        Input #2, gameTypeOpt
        Input #2, gameMonth
        Input #2, dayOrNight
        Input #2, useWeather
        Input #2, useInj
        Input #2, useLRSplit
        Input #2, useRest
        Input #2, useMgrProf
        Input #2, pbpDelay!
        Input #2, useVGA
        Input #2, pitchEraOpt

        Close #2

        Kill "DEFERA"

        Open "DEFAULT" For Output As #3
        Close #3

    Else

        If Not _FileExists("DEFAULT") Then

            Open "DEFAULT" For Output As #3

            'Player option
            Print #3, 3

            'NL / AL
            Print #3, 1

            'DH Opt, Game Type
            For I = 1 To 2:
                Print #3, 0
            Next

            'Game Month
            Print #3, 4

            'Day | Night Game
            Print #3, 0

            'Yes/No Options
            For I = 1 To 5:
                Print #3, 0
            Next

            'PBP Delay
            Print #3, 1.2

            'Use Graphics?
            Print #3, 1

            'Pitcher Era
            Print #3, 0

            'Sound option
            Print #3, 1

            Close #3

        End If

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, leagueOpt
    Input #4, desigHit
    Input #4, gameTypeOpt
    Input #4, gameMonth
    Input #4, dayOrNight
    Input #4, useWeather
    Input #4, useInj
    Input #4, useLRSplit
    Input #4, useRest
    Input #4, useMgrProf
    Input #4, pbpDelay!
    Input #4, useVGA
    Input #4, pitchEraOpt
    If Not EOF(4) Then
        Input #4, sndOpt
    End If
    Close #4

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #5

        Input #5, scheduleFile$

        For I = 0 To 2:
            Input #5, schedGame(I)
        Next

        Close #5

        currGame = schedGame(0) + schedGame(2)

        'Open "pbplog" + Str$(currGame) For Append As #11
        'Open "pbplog" For Append As #11

        If Not _FileExists(scheduleFile$) Then

            'Notify user that game won't execute
            MsgText$ = "CAN'T FIND OR LOAD TARGET SCHEDULE.  PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        Else

            'Read in schedule options
            Open scheduleFile$ For Random As #6 Len = SCHEDULE_SIZE_BYTES

            For X = 0 To 18
                Field #6, X * 2 As X$, 2 As Q2$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
            Next

            Field #6, 38 As X$, 2 As Q2$(22), 2 As Q2$(23), 5 As Q2$(20), 5 As Q2$(21)

            Get #6, schedGame(0) + schedGame(2)

            For X = 0 To 18
                schedOptions(X) = CVI(Q2$(X + 1))
            Next

            For X = 0 To 1
                diskIDs$(X) = _Trim$(Q2$(20 + X))
            Next

            VS% = CVI(Q2$(22))
            HS% = CVI(Q2$(23))

            Close #6

            autoPlay = 1

        End If

    Else

        'Open "pbplog" For Output As #11

    End If

    If Environ$("PBPLOG") = "TRUE" Then Open "pbplog.txt" For Append As #11

    If autoPlay = 1 Then

        Call AUTOSETUP(goodTeam)

        If goodTeam = 0 Then

            'Notify user that game won't execute
            MsgText$ = "ATTEMPTED TO LOAD AN INVALID TEAM. PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        End If

    Else

        170 '
        Call INFO

        If gameLoc = 0 Then gameLoc = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 3, 2: Color 14
            Print "(1) ";
            Color 15: Print playMode$(playerOpt)
            Locate 4, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 11), teamRat_GAME(0, 12): Print gameTeams$(0)
            Locate 5, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 11), teamRat_GAME(1, 12): Print gameTeams$(1)
            Locate 6, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "LEAGUE TYPE: "; leagueType$(leagueOpt)
            Locate 7, 2: Color 14
            Print "(5) ";
            Color 15: Print "DESIGNATED HITTER: "; desigHitOpt$(desigHit)
            Locate 8, 2: Color 14
            Print "(6) ";
            Color 15: Print "GAME TYPE: "; gameType$(gameTypeOpt)
            Locate 9, 2: Color 14
            Print "(7) ";
            Color 15: Print "MONTH: "; month$(gameMonth)
            Locate 10, 2: Color 14
            Print "(8) ";
            Color 15: Print "TIME OF DAY: "; dayNight$(dayOrNight)
            Locate 11, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 12, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS AS DEFAULT"
            Locate 13, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.# "; pbpDelay!
            Locate 14, 2: Color 14
            Print "(B) ";
            Color 15: Print "USE INJURIES: "; yesNoText$(useInj)
            Locate 15, 2: Color 14
            Print "(C) ";
            Color 15: Print "USE LEFT/RIGHT SPLIT STATS: "; yesNoText$(useLRSplit)
            Locate 16, 2: Color 14
            Print "(D) ";
            Color 15: Print "USE DAYS REST DATA FOR PITCHERS: "; yesNoText$(useRest)
            Locate 17, 2: Color 14
            Print "(E) ";
            Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE: "; yesNoText$(useMgrProf)
            Locate 18, 2: Color 14
            Print "(F) ";
            Color 15: Print "USE WEATHER EFFECTS: "; yesNoText$(useWeather)
            Locate 19, 2: Color 14
            Print "(G) ";
            Color 15: Print "USE VGA SCREENS, IF ABLE: "; yesNoText$(useVGA)
            Locate 20, 2: Color 14
            Print "(H) ";
            Color 15: Print "PITCHER ERA: "; pitcherEra$(pitchEraOpt)
            Locate 21, 2: Color 14
            Print "(I) ";
            Color 15: Print "SOUND ENABLED: "; yesNoText$(sndOpt)

            'Locate 23, 2: Color 14
            'Print "NOTE:: "
            'Color 15

            'Locate , 2: Print "* NOTE ABOUT RULES GOES HERE"

            Do
                Color 7, 0

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then Call LetterToNumber(I$, setting)

            Loop Until setting >= 0 And setting <= 18 Or I$ = Chr$(27) Or UCase$(I$) = Chr$(17) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" And UCase$(I$) <> Chr$(17) Then

                Select Case setting

                    Case 0:
                        Open "DEFERA" For Output As #7
                        Print #7, playerOpt
                        Print #7, leagueOpt
                        Print #7, desigHit
                        Print #7, gameTypeOpt
                        Print #7, gameMonth
                        Print #7, dayOrNight
                        Print #7, useWeather
                        Print #7, useInj
                        Print #7, useLRSplit
                        Print #7, useRest
                        Print #7, useMgrProf
                        Print #7, pbpDelay!
                        Print #7, useVGA
                        Print #7, pitchEraOpt
                        Print #7, sndOpt
                        Close #7

                    Case 1:
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(diskIDs$(0), NTMS, gameMgrIdx(0), gameTeams$(0), 0)
                        If tmIdx <> 0 Then
                        End If
                        Color 15, 0

                    Case 3:
                        Call ChooseGameTeam(diskIDs$(1), NTMS, gameMgrIdx(1), gameTeams$(1), 1)
                        If tmIdx <> 0 Then
                        End If
                        Color 15, 0

                    Case 4:
                        leagueOpt = leagueOpt + 1
                        If leagueOpt > 2 Then leagueOpt = 0

                    Case 5:
                        desigHit = 1 - desigHit

                    Case 6:
                        gameTypeOpt = 1 - gameTypeOpt

                    Case 7:
                        gameMonth = gameMonth + 1
                        If gameMonth > 10 Then gameMonth = 4
                        '-- wtf?? Locate 7, 4: Print BK$

                    Case 8:
                        dayOrNight = 1 - dayOrNight
                        Locate 8, 4
                        '-- wtf?? Print BK$

                    Case 9:
                        '

                    Case 10:
                        Do
                            Locate 13, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11:
                        useInj = 1 - useInj

                    Case 12:
                        useLRSplit = 1 - useLRSplit

                    Case 13:
                        useRest = 1 - useRest

                    Case 14:
                        useMgrProf = 1 - useMgrProf

                    Case 15:
                        useWeather = 1 - useWeather

                    Case 16:
                        useVGA = 1 - useVGA

                    Case 17:
                        pitchEraOpt = pitchEraOpt + 1
                        If pitchEraOpt > 8 Then pitchEraOpt = 0

                    Case 18
                        sndOpt = 1 - sndOpt

                End Select

            End If

        Loop Until (setting = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or I$ = Chr$(17) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> Chr$(17) And UCase$(I$) <> "Q" Then
            'TICKER PROMPT
        Else

            earlyExit = 1

        End If

    End If

    'If we've run into issues so far...
    If earlyExit = 1 Then Exit Sub

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    For I = 0 To 1:
        'If disk ID has alpha-char, or is only 3 characters,
        'then assume the year is the first 2 chars
        If CheckString%(diskIDs$(I)) = 1 Or Len(diskIDs$(I)) <= 3 Then
            teamYears(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
        Else
            'If we have 4 or more characters,
            'then Val will return the first 4 digits
            teamYears(I) = Val(RTrim$(diskIDs$(I)))
        End If
    Next I

    If teamRat_GAME(0, 11) = teamRat_GAME(1, 11) And teamRat_GAME(0, 12) = teamRat_GAME(1, 12) Then
        Call ColorChange
    End If

    'Determine attendance

    ' ** the original game generated random attendance from 5,000 to 59,000
    ' ** Attendannce hasn't been as low as 5,000 since early 1900,
    ' ** and almost never 59,000 except for significant events
    'https://teamTotWinsw.baseball-reference.com/leagues/majors/misc.shtml
    If gameLoc = 2 Or avgAttendance& = 0 Then
        'Get assumed year of home team
        Select Case teamYears(1)

            Case Is <= 1900
                'Random # from 500 to 3,000
                actualAttendance& = Int(Rnd(1) * 2500) + 500

            Case 1901 - 1945
                'Random # from 4,000 to 10,000
                actualAttendance& = Int(Rnd(1) * 6000) + 4000

            Case Is >= 1946
                'Random # from 15,000 to 30,000
                actualAttendance& = Int(Rnd(1) * 15000) + 15000

        End Select

    Else
        'Random % +/- 10,000 of team's average
        ' ** this was added by me (Jason) to reflect
        ' ** how other games calculate attendance.
        ' ** Previously the source was not using the
        ' ** averages read in from PARKS.DBF
        actualAttendance& = avgAttendance& + Int(Rnd(1) * 20000) - 10000
    End If

    YV = 1
    YM = 0

    '-------------------------
    ' Determine umpires
    '-------------------------
    If Not _FileExists(diskPaths$(0) + "UMPIRES." + umpireName$(leagueOpt)) Then

        'Assign generic umpire names if we don't have any
        gameUmpire$(1) = "RHODES"
        gameUmpire$(2) = "WINBERRY"
        gameUmpire$(3) = "TIPPETT"
        gameUmpire$(4) = "MILLER"

    Else
        YU = 0

        Open diskPaths$(0) + "UMPIRES." + umpireName$(leagueOpt) For Input As #8

        For I1 = 1 To 50
            Input #8, umpireName$(I1)
            Input #8, umpireNumber(I1)
        Next

        Close #8

        For I1 = 1 To 50
            If umpireName$(I1) <> "" Then YU = YU + 1
        Next

        If YU < 4 Then
            'If there aren't 4 umpires, assign generic names
            gameUmpire$(1) = "RHODES"
            gameUmpire$(2) = "WINBERRY"
            gameUmpire$(3) = "TIPPETT"
            gameUmpire$(4) = "MILLER"
        Else
            'Randomly assign umpires
            Do

                While (umpireName$(YU) = "" Or YU = YM)
                    YU = Int(Rnd(1) * 50) + 1
                Wend

                gameUmpire$(YV) = umpireName$(YU)
                YM = YU
                YV = YV + 1

            Loop Until YV >= 5

        End If

    End If

    '-------------------------
    ' Team Mgmt + Lineups
    '-------------------------

    'Determine which team PC will play
    'If playerMode = 1 And autoPlay <> 1 Then

    'If not doing autoplay...
    '    Call INFO

    '    Color 14: Print "WHICH TEAM DOES COMPUTER MANAGE ?"
    '    Print
    '    Color 15: Print "(0) ";: Color teamRat_GAME(0, 11), teamRat_GAME(0, 12): Print gameTeams$(0): Color 7, 0
    '    Print
    '    Color 15: Print "(1) ";: Color teamRat_GAME(1, 11), teamRat_GAME(1, 12): Print gameTeams$(1): Color 7, 0

    '    Do
    '        I$ = GetKeyPress$
    '        compTeam = Val(I$)
    '    Loop Until compTeam = 0 Or compTeam = 1

    'End If

    'Determine pitchers
    For P9 = 0 To 1

        cancelPitchers% = 0

        Call SelectPitchers(P9, cancelPitchers%, computerRotations%)
        Call PitchingStarter(P9)

        If Inotloop% <= 5 And gameScore(P9, 0) - gameScore(1 - P9, 0) > 0 Then
            pitcherOfRecord(P9) = currPitcher(P9)
        End If

        pitchEff(P9, 0) = CInt(((pitchRating(P9, currPitcher(P9), 8) + pitchRating(P9, currPitcher(P9), 7)) / pitchRating(P9, currPitcher(P9), 4)))
        pitchEff(P9, 1) = 3

        If gamePitcher(P9) > 1 Then
            pitchEff(P9, 1) = 0
        End If

        'I'm not sure why we go back only
        'if ESC was pressed during pitcher select
        If cancelPitchers% = 1 Then GoTo 170

    Next P9

    'Determine lineup for each team
    For P9 = 0 To 1

        Do

            Cls

            Call INFO

            Locate 3, 1
            Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12): Print gameTeams$(P9): Color 7, 0
            Print
            Color 14: Print "(0) ";: Color 15: Print "INPUT A LINEUP MANUALLY"
            Print
            Color 14: Print "(1) ";: Color 15: Print "SELECT A LINEUP FROM MANAGER PROFILE"
            Print
            Color 14: Print "(2) ";: Color 15: Print "COMPUTER DETERMINE LINEUP RANDOMLY"
            Print
            Color 14: Print "(3) ";: Color 15: Print "COMPUTER DETERMINE LINEUP FROM MANAGER PROFILE"

            reselect = 0

            If autoPlay = 1 Then

                'noLUFound = no lineup found?
                If noLUFound = 1 Then

                    noLUFound = 0
                    I$ = "2"

                Else

                    If (playerMode = 2 Or playerMode = 1 And compTeam = P9) Then
                        If useMgrProf = 1 Then
                            I$ = "1"
                        Else
                            I$ = "2"
                        End If
                    End If

                End If

            Else

                Do
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 0 And I1 <= 3 And I$ <> Chr$(27)

            End If

            Select Case I$

                Case "0":
                    'Lineup - no profile

                    Call SelectBatters(batterFlag%, P9)

                    If batterFlag% = 1 Then
                        For I = 10 To 22
                            Locate I, 59: Print Space$(21);
                        Next

                        Call StartingLineup(P9)
                        Call ChangeLineup_PreGame(P9, reselect)

                        If noLineups = 1 Then
                            noLineups = 0
                        End If
                    End If

                Case "1", "3":
                    'Lineup using profile
                    Call SearchForLineup(P9)

                    Call LineupFromProfile(lineupFound%, P9, skipLineChange%)

                    If lineupFound% = 1 Or skipLineChange% = 0 Then
                        Call ChangeLineup_PreGame(P9, reselect)
                        If noLineups = 1 Then
                            noLineups = 0
                        End If
                    Else
                        noLineups = 1
                    End If

                Case "2"
                    Call ComputerLineups(batterFlag%, P9, noLineups, reselect)

                    If noLineups = 1 Then
                        noLineups = 0
                        skipLineChange% = 1
                    End If

            End Select

            If I$ <> Chr$(27) Then
                'We didn't ESC so we have to move foneRunWinsard

                If skipLineChange% <> 1 Then
                    Color 15, 0
                    Locate 5, 59: Print "OPPOSING PITCHER"
                    Locate , 59: Print pitchers$(1 - P9, currPitcher(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher(1 - P9), SS_P_Throws) + 2)
                    If RP = 1 Then RP = 0
                End If

            Else
                'ESC means we go back to earlier menus
                noLineups = 1
                GoTo 170

            End If

        Loop Until noLineups = 0 And reselect = 0

    Next P9

    '-------------------------
    ' Ready to Play
    '-------------------------

    For I = 0 To 1
        For I1 = 1 To 9
            X0%(I, 0, I1) = lineupPlayer(I, I1)
            X0%(I, 1, I1) = fldPos(I, I1)
            X0%(I, 2, I1) = I1 * 10
        Next I1

        gameD0(I) = 9

    Next I

    W5 = 0
    W6 = 1
    VV(0) = 1
    VV1(0) = 9
    VV(1) = 1
    VV1(1) = 9

    Cls

    rainDelay = 0

    For I = 0 To 1
        For I1 = 1 To 9
            If fldPos(I, I1) >= 2 And fldPos(I, I1) <= 9 Then
                If     fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos1) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = batRating(I, lineupPlayer(I, I1), SS_B_Arm1)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range)  = batRating(I, lineupPlayer(I, I1), SS_B_Range1)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct1)
                ElseIf fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos2) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = batRating(I, lineupPlayer(I, I1), SS_B_Arm2)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range)  = batRating(I, lineupPlayer(I, I1), SS_B_Range2)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct2)
                ElseIf fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos3) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = batRating(I, lineupPlayer(I, I1), SS_B_Arm3)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range)  = batRating(I, lineupPlayer(I, I1), SS_B_Range3)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct3)
                ElseIf fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos4) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = batRating(I, lineupPlayer(I, I1), SS_B_Arm4)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range)  = batRating(I, lineupPlayer(I, I1), SS_B_Range4)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct4)
                Else
                    Select Case fldPos(I, I1)
                        Case 2
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = 1
                            batRating(I, lineupPlayer(I, I1), SS_B_Range)  = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 910 + Int(Rnd(1) * 10) + 1
                        Case 3
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_Range)  = 3
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 960 + Int(Rnd(1) * 10) + 1
                        Case 4
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_Range)  = 3
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 912 + Int(Rnd(1) * 10) + 1
                        Case 5, 6
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_Range)  = 3
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 902 + Int(Rnd(1) * 10) + 1
                        Case 7 To 9
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm)    = 2
                            batRating(I, lineupPlayer(I, I1), SS_B_Range)  = 2
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 972 + Int(Rnd(1) * 10) + 1
                    End Select
                End If
            End If
        Next I1
    Next I

    Call SelectStadium(useVGA)

    Call GETWEATHER(windDir, windSpeed, gameMonth, WF!, skyType, gameTemp, rainDelay)

    Cls

    Call TEAMBOX(5, 1)

    Locate , 11: Print "CONDITIONS AT "; gameStadiums$(1)
    If teamRat_GAME(1, 9) = 1 Then
        Locate , 11: Print "TEMPERATURE: 70"
        Locate , 11: Print "WEATHER: DOME"
        Locate , 11: Print "WIND: NONE"
    Else
        Locate , 11: Print "TEMPERATURE: "; gameTemp
        Locate , 11: Print "SKY CONDITIONS: "; skyCond$(skyType)
        Locate , 11: Print "WIND: "; windDirection$(windDir);
        Print Using " ## MPH"; windSpeed

        If useVGA = 0 Then
            windDirection$(2) = "L->R"
            windDirection$(3) = "R->L"
            windDirection$(4) = "<-LF"
            windDirection$(5) = "<-CF"
            windDirection$(6) = "<-RF"
            windDirection$(7) = "->LF"
            windDirection$(8) = "->CF"
            windDirection$(9) = "->RF"
        End If

        wind$ = windDirection$(windDir) + Str$(windSpeed) + " MPH"
        Locate , 11: Print "PRECIPITATION: "; precip$(precipType)

    End If

    I1 = Val(Left$(gameTeams$(0), 2))
    I2 = Val(Left$(gameTeams$(1), 2))

    If Val(Left$(gameTeams$(0), 3)) = 211 Then
        I1 = 201
    End If
    If Val(Left$(gameTeams$(0), 3)) = 200 Then
        I1 = 200
    End If

    If Val(Left$(gameTeams$(1), 3)) = 200 Then
        I2 = 200
    End If
    If Val(Left$(gameTeams$(1), 3)) = 211 Then
        I2 = 201
    End If

    '1955 - Present
    'pitchEff(0, 1) = calculated from Walks + Hits / Game
    'pitchEff(1, 1) = 3

    Select Case pitchEraOpt

        Case 3
            '1946-1954
            pitchEff(0, 1) = 4
            pitchEff(1, 1) = 4

        Case 4 To 6
            '1909 to 1945
            pitchEff(0, 1) = 5
            pitchEff(1, 1) = 5

        Case 7
            '1894 to 1908
            pitchEff(0, 1) = 7
            pitchEff(1, 1) = 7

        Case Is >= 8
            '1876-1893
            pitchEff(0, 1) = 99
            pitchEff(1, 1) = 99

    End Select

    Call LOADSTATS
    Call DISPBAVG(D)

    W5 = 0
    W6 = 1

    '--- we need to check for Revised.pbp
    If _FileExists("revised.pbp") Then

        Call SOURCE

    Else

        MsgText$ = "ERROR!!! PLAY-BY-PLAY SOURCE FILE NOT FOUND. CHECK THAT THE FILE 'REVISED.PBP' EXISTS AND IS READ-ONLY."
        BoxWidth = 22
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        A$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

    'Close #7
    'End
    'System

End Sub


'----------------------------------------
'       StartingLineup Subroutine
'----------------------------------------
'This subroutine displays the existing
'lineup options and calls a routine TO
'let the user select the lineup
Sub StartingLineup (P9)

    skipLU% = 0

    Do
        'track assigned positions
        '   0 = unassigned
        Erase gameM

        Locate 25, 1

        For I1 = 0 To 9

            If desigHit <> 0 Or I1 <> 0 Then

                If desigHit <> 1 Or I1 <> 1 Then

                    Locate LAST_ROW, (I1 * 3) + 1
                    If gameM(I1) = 0 Then
                        Print fieldPos$(I1); "  ";
                    End If
                    If gameM(I1) = 1 Then
                        Print "   ";
                    End If

                    Locate 25, (I1 * 3) + 1
                    If gameM(I1) = 0 Then
                        Print I1; "  ";
                    End If
                    If gameM(I1) = 1 Then
                        Print "   ";
                    End If

                End If

            End If

        Next I1

        For I = 1 To 9

            Color 15, 0
            Locate 5, 59: Print "OPPOSING PITCHER"
            Locate , 59: Print pitchers$(1 - P9, currPitcher(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher(1 - P9), SS_P_Throws) + 2)
            Call LineupPositions(P9, I)
            If C1 = 1 Then Exit For

        Next I

    Loop Until skipLU% = 0

End Sub


'----------------------------------------
'       LineupPositions Subroutine
'----------------------------------------
'This subroutine works with the user to
'help them select fielding positions for
'each place in the lineup
Sub LineupPositions (teamIdx, posIdx)

    'Do

    560 '
    Call PositionCursor(posIdx)
    Print Space$(18);
    Call PositionCursor(posIdx)

    'Get keypress for batter #
    I$ = GetKeyPress$

    If I$ = Chr$(27) Then

        Erase gameM

        For J = 1 To 9
            fldPos(teamIdx, J) = -1
            lineupPlayer(teamIdx, J) = -1
        Next

        C1 = 0

        For J = 0 To 22
            batRating(teamIdx, J, SS_B_Avail) = 0
        Next

        RP = 1

        For J = 0 To 22
            If batRating(teamIdx, J, SS_B_Active) <> 0 Then
                batRating(teamIdx, J, SS_B_Avail) = 99
            End If
        Next

    Else

        Call LetterToNumber(I$, I1)

        If UCase$(I$) <> "X" Then

            For J = 1 To 9
                If Not (J = posIdx Or (lineupPlayer(teamIdx, J) = I1 And fldPos(teamIdx, J) = 1)) Then
                    If lineupPlayer(teamIdx, J) = I1 And UCase$(I$) <> "P" Then
                        GoTo 560 'skiP1 = 1
                    End If
                End If
            Next J

            If Not (desigHit = 1 And UCase$(I$) = "P") Or Not (batRating(teamIdx, I1, SS_B_Avail) <> 0 And UCase$(I$) <> "P") Then

                If UCase$(I$) = "P" Then

                    For J = 1 To 9
                        If J <> posIdx Then
                            If lineupPlayer(teamIdx, J) = currPitcher(teamIdx) And fldPos(teamIdx, J) = 1 Then
                                skip2% = 1
                            End If
                        End If
                    Next J

                    If skip2% <> 1 Then

                        lineupPlayer(teamIdx, posIdx) = currPitcher(teamIdx)
                        Print pitchers$(teamIdx, currPitcher(teamIdx));
                        gameM(I1) = 1

                        Locate , 75
                        Print "P ";
                        fldPos(teamIdx, posIdx) = 1
                        fielder(teamIdx, 1) = currPitcher(teamIdx)
                        gameM(1) = 1

                        Locate 25, 1

                        For I0 = 0 To 9

                            If desigHit <> 0 Or I0 <> 0 Then

                                If desigHit <> 1 Or I0 <> 1 Then

                                    Locate LAST_ROW, (I0 * 3) + 1
                                    If gameM(I0) = 0 Then
                                        Print fieldPos$(I0); "  ";
                                    End If
                                    If gameM(I0) = 1 Then
                                        Print "   ";
                                    End If

                                    Locate 25, (I0 * 3) + 1
                                    If gameM(I0) = 0 Then
                                        Print I0; "  ";
                                    End If
                                    If gameM(I0) = 1 Then
                                        Print "   ";
                                    End If

                                End If
                            End If

                        Next I0

                        '-- this is taken care of outside the routine
                        'If C1 = 1 Then Call ChangeLineup_Pregame

                    End If

                Else

                    lineupPlayer(teamIdx, posIdx) = I1
                    Print Using "# "; posIdx;
                    Print batters$(teamIdx, lineupPlayer(teamIdx, posIdx));

                    Locate , 74

                    680 '
                    'Input fielding position
                    'If desigHit = 1 Then firstSlot = 1 Else firstSlot = 0
                    firstSlot = 1

                    Do
                        J$ = GetKeyPress$
                        J1 = Val(J$)
                    Loop Until J1 <> firstSlot

                    For J = 1 To 9
                        If J <> posIdx Then
                            If J1 = fldPos(teamIdx, J) Then 680
                        End If
                    Next J

                    If Not (desigHit = 0 And J1 = 0 Or UCase$(J$) = "X") Then

                        'Print position selected
                        Print fieldPos$(J1);
                        fldPos(teamIdx, posIdx) = J1

                        fielder(teamIdx, fldPos(teamIdx, posIdx)) = lineupPlayer(teamIdx, posIdx)
                        gameM(J1) = 1

                        'Highlight the player we selected
                        Locate I1 + 2, 2: Print "*";

                        Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12)
                        Print batters$(teamIdx, I1)
                        Color 15, 0

                        Locate 25, 1

                        For I1 = 0 To 9

                            If desigHit <> 0 Or I1 <> 0 Then

                                If desigHit <> 1 Or I1 <> 1 Then

                                    Locate LAST_ROW, (I1 * 3) + 1
                                    If gameM(I1) = 0 Then
                                        Print fieldPos$(I1); "  ";
                                    End If
                                    If gameM(I1) = 1 Then
                                        Print "   ";
                                    End If

                                    Locate 25, (I1 * 3) + 1
                                    If gameM(I1) = 0 Then
                                        Print I1; "  ";
                                    End If
                                    If gameM(I1) = 1 Then
                                        Print "   ";
                                    End If

                                End If

                            End If

                        Next I1

                    End If

                End If 'Done checking for "P"

            Else

                GoTo 560 'skipLU% = 1

            End If 'Done checking skip

        End If 'Done checking for "X"

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'    ChangeLineup_PreGame Subroutine
'----------------------------------------
'This routine gives the user a chance to
'make adjustments to the lineup before
'they start the game.
Sub ChangeLineup_PreGame (P9, reselect)

    Do

        C1 = 1
        Locate 25, 1
        Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
        Color 15, 0

        If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = P9) Then
            I$ = "0"
        Else
            I$ = GetKeyPress$
        End If

        I = Val(I$)

        Select Case I

            Case 0:
                '0 can be us entering "0"
                'or it is also what gets assigned
                'if you use VAL against an alpha char
                Select Case UCase$(I$)
                    Case "0"
                        'Here we still could have selected R
                        If Inotloop% = 0 Then
                            Call SaveLineup(P9)
                        End If

                        For I = 1 To 9
                            If fldPos(P9, I) <> 1 Then
                                batRating(P9, lineupPlayer(P9, I), SS_B_Avail) = 1
                            End If
                        Next I

                        Erase gameM

                        C1 = 0

                    Case "R", Chr$(27):

                        Erase gameM

                        For J = 1 To 9
                            fldPos(P9, J) = -1
                            lineupPlayer(P9, J) = -1
                        Next

                        C1 = 0

                        For J = 0 To 22:
                            batRating(P9, J, SS_B_Avail) = 0
                        Next

                        RP = 1

                        '1001
                        For J = 0 To 22
                            If batRating(P9, J, SS_B_Active) <> 0 Then
                                batRating(P9, J, SS_B_Avail) = 99
                            End If
                        Next

                        reselect = 1

                    Case "L":
                        Call LINSWAP(P9)

                    Case "P":
                        Call POSSWAP(P9)

                End Select

            Case 1 To 9:
                Locate lineupPlayer(P9, I) + 2 - B4%, 2
                Print " "; batters$(P9, lineupPlayer(P9, I))

                Call LineupPositions(P9, I)

        End Select

    Loop Until UCase$(I$) = "0" Or UCase$(I$) = "R" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         SaveLineup Subroutine
'----------------------------------------
'This routine saves the lineup that has
'been generated to the manager profile for
'the team. It also gives the user a chance
'to assign a frequency to it.
Sub SaveLineup (idx%)

    Shared mgrFile(), gameMgrIdx()

    If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = idx%) Then
        I$ = "N"
    Else
        Do
            Locate 25, 1: Print Space$(74);
            Locate 25, 1: Print "SAVE THIS LINE UP? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Then

        Cls

        If pitchRating(1 - idx%, currPitcher(1 - idx%), SS_P_Throws) = -1 Then
            Q2% = 1
        End If
        If pitchRating(1 - idx%, currPitcher(1 - idx%), SS_P_Throws) = 1 Then   'ToDo [bm] changed from 0
            Q2% = 0
        End If

        For I = 0 To 9
            Print I; " ";
            If teamMgrProfileVal(idx%, 120 + (I + Q2% * 10) * 21) <> 999 Then
                Print "AVAILABLE"
            End If
            If teamMgrProfileVal(idx%, 120 + (I + Q2% * 10) * 21) = 999 Then
                Print "LINEUP ALREADY SAVED "; teamMgrProfileVal(idx%, 100 + (I + Q2% * 10) * 21); " % USAGE"
            End If
        Next

        Input "LINEUP # TO SAVE "; LN%
        Input "% OF TIME TO USE "; LU%

        If teamMgrProfileVal(idx%, 120 + (LN% + Q2% * 10) * 21) <> 999 Then
            teamMgrProfileVal(idx%, 120 + (LN% + Q2% * 10) * 21) = 999
        End If
        If Q2% = 1 Then
            LN% = LN% + 10
        End If

        teamMgrProfileVal(idx%, 101 + LN% * 21) = desigHit
        teamMgrProfileVal(idx%, 100 + LN% * 21) = LU%

        For I = 1 To 9
            teamMgrProfileVal(idx%, 110 + I + LN% * 21) = lineupPlayer(idx%, I)
            teamMgrProfileVal(idx%, 101 + I + LN% * 21) = fldPos(idx%, I)
        Next

        'idx% = P9
        teamMgrProfileVal(idx%, 99) = 999
        teamMgrProfileVal(idx%, 0) = 999
        mgrFile(idx%) = 0

        teamYear$ = RTrim$(diskIDs$(idx%))

        Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #9 Len = MGR_SIZE_BYTES

        For I = 0 To 999
            Field #9, I * 2 As Q$, 2 As Q2$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
        Next

        For I = 0 To 999:
            LSet Q2$(I) = MKI$(teamMgrProfileVal(idx%, I))
        Next

        Put #9, gameMgrIdx(idx%)

        Close 9

    End If

End Sub

'----------------------------------------
'       SearchForLineup Subroutine
'----------------------------------------
'This routines searches for an existing
'lineup from the manager profiles.
Sub SearchForLineup (P9)

    Shared pitHand$()

    '2500
    Cls
    If pitchRating(1 - P9, currPitcher(1 - P9), SS_P_Throws) = -1 Then
        Q2% = 1
    End If
    If pitchRating(1 - P9, currPitcher(1 - P9), SS_P_Throws) = 1 Then
        Q2% = 0
    End If
    'pitchRating(0-1,0-1,0); all indexes are 1's

    Print Tab(30); "LINEUPS FOUND VS. "; pitHand$(Q2%)
    Print " #0    "; teamMgrProfileVal(P9, 100 + (0 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #1    "; teamMgrProfileVal(P9, 100 + (1 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #2    "; teamMgrProfileVal(P9, 100 + (2 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #3    "; teamMgrProfileVal(P9, 100 + (3 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #4    "; teamMgrProfileVal(P9, 100 + (4 + Q2% * 10) * 21); "%"

    For I = 0 To 4
        For I1 = 1 To 9

            If teamMgrProfileVal(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 3, 16 * I + 1: Print "NONE FOUND"
            Else

                If teamMgrProfileVal(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 2 + I1, 16 * I + 1: Print "         "
                Else
                    Locate 2 + I1, 16 * I + 1

                    If teamMgrProfileVal(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batters$(P9, teamMgrProfileVal(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 2 + I1, 16 * I + 12: Print fieldPos$(teamMgrProfileVal(P9, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If
        Next I1
    Next I

    Print
    Print " #5    "; teamMgrProfileVal(P9, 100 + (5 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #6    "; teamMgrProfileVal(P9, 100 + (6 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #7    "; teamMgrProfileVal(P9, 100 + (7 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #8    "; teamMgrProfileVal(P9, 100 + (8 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #9    "; teamMgrProfileVal(P9, 100 + (9 + Q2% * 10) * 21); "%"

    For I = 5 To 9

        For I1 = 1 To 9

            If teamMgrProfileVal(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 14, 16 * (I - 5) + 1: Print "NONE FOUND"
            Else
                If teamMgrProfileVal(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 13 + I1, 16 * (I - 5) + 1: Print "         "
                Else
                    Locate 13 + I1, 16 * (I - 5) + 1

                    If teamMgrProfileVal(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batters$(P9, teamMgrProfileVal(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 13 + I1, 16 * (I - 5) + 12: Print fieldPos$(teamMgrProfileVal(P9, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If

        Next

    Next

End Sub


'----------------------------------------
'      LineupFromProfile Subroutine
'----------------------------------------
'This subroutine attempts to load a selected
'lineup from the manager profile and presents
'a summary of it, if found.
Sub LineupFromProfile (lineupFound%, P9, skipLineChange%)

    lineupFound% = 0
    PCT% = 0

    For I = 0 To 9
        If teamMgrProfileVal(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
            lineupFound% = lineupFound% + 1
            PCT% = PCT% + teamMgrProfileVal(P9, 100 + (I + Q2% * 10) * 21)
        End If
    Next

    If lineupFound% = 0 Then

        Cls
        Print "ERROR:: NO LINEUPS FOUND...BE SURE CORRECT LINEUP ID WAS CHOSEN"

        If autoPlay = 1 Then
            noLUFound = 1
        Else
            Locate 22, 1
            Print "PRESS ANY KEY TO CONTINUE"
            I$ = GetKeyPress$
        End If
        skipLineChange% = 1

    Else

        If autoPlay = 1 And useMgrProf = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = P9) Or I$ = "3" Then

            'Create automated lineup
            RN = Int(Rnd(1) * PCT%) + 1
            PCT% = 0

            For I = 0 To 9
                If teamMgrProfileVal(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
                    PCT% = PCT% + teamMgrProfileVal(P9, 100 + (I + Q2% * 10) * 21)
                    If RN <= PCT% Then LN% = I: Exit For
                End If
            Next

        Else
            Do
                Locate 23, 1: Input "ENTER LINEUP NUMBER (0-9) "; LN%
            Loop Until LN% >= 0 And LN% <= 9

        End If

        If teamMgrProfileVal(P9, 120 + (LN% + Q2% * 10) * 21) = 999 Then

            If Q2% = 1 Then
                LN% = LN% + 10
            End If

            If desigHit = teamMgrProfileVal(P9, 101 + LN% * 21) Then

                For I = 1 To 9
                    lineupPlayer(P9, I) = teamMgrProfileVal(P9, 110 + I + LN% * 21)
                    fldPos(P9, I) = teamMgrProfileVal(P9, 101 + I + LN% * 21)
                Next

                LU% = 1
                Call LineupSummary(P9, skipLineChange%, LU%)

            Else
                'Technically we found a lineup
                'But we will skip prompting for
                'any lineup changes
                skipLineChange% = 1
            End If 'Done checking desigHit
        Else
            'skip lineup changes
            skipLineChange% = 1
        End If 'Done checking for 999

    End If 'Done searching for lineup

End Sub

'----------------------------------------
'   LineupSummary Subroutine
'----------------------------------------
'This subroutine displays a summary of the
'starting lineup before allowing the user
'to make any adjustemts.
Sub LineupSummary (P9, reselect, LU%)

    For I = 1 To 9:

        testIdx = fldPos(P9, I)

        If testIdx >= 0 Then

            fielder(P9, fldPos(P9, I)) = lineupPlayer(P9, I)

            If fldPos(P9, I) = 1 Then
                lineupPlayer(P9, I) = currPitcher(P9)
                fielder(P9, 1) = currPitcher(P9)
            Else
                batRating(P9, lineupPlayer(P9, I), SS_B_Avail) = 0
            End If

        End If

    Next I

    If LU% = 1 Then
        LU% = 0
        Call SelectBatters(batterFlag%, P9)
    End If

    For I = 10 To 22
        Locate I, 59: Print Space$(21);
    Next

    Call NEWLINES(P9)

    For I2 = 0 To 22:

        Locate I2 + 2, 2

        For I3 = 1 To 9
            If lineupPlayer(P9, I3) = I2 And fldPos(P9, I3) <> 1 Then
                Print "*";
                Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12)
                Print batters$(P9, I2)
                Color 15, 0
            End If
        Next I3

    Next I2

    For I3 = 0 To 9:
        gameM(I3) = 1
    Next I3

    Call ChangeLineup_PreGame(P9, reselect)

End Sub


'----------------------------------------
'        SelectBatters Subroutine
'----------------------------------------
'This subroutine lets you choose the batters
'for the game. It interfaces with various
'other routines to show you different types
'of batter stats to help you with your choices.
Sub SelectBatters (batterFlag%, P9)

    Inotloop% = 99

    Call SHOWBATTERS(P9)

    Color 15, 0
    Locate 5, 59: Print "OPPOSING PITCHER"
    Locate 6, 59: Print pitchers$(1 - P9, currPitcher(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher(1 - P9), SS_P_Throws) + 2)

    'TG = 0

    Do
        'Select batters first
        Locate 10, 59: Print "DISABLE BATTER (0-M)"
        Locate , 59: Print "Z-CONTINUE"
        Locate , 59: Print "X-REACTIVATE ALL"
        Locate , 59: Print "<TAB>-BAT/FIELD/SPLIT"

        If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = P9) Then
            'Autoplay = auto-select
            I$ = "Z"
            batterFlag% = 1
        Else
            I$ = GetKeyPress$

            'Handle ESC
            If I$ = Chr$(27) Then

                Erase gameM

                For J = 1 To 9
                    fldPos(P9, J) = -1
                    lineupPlayer(P9, J) = -1
                Next

                C1 = 0

                For J = 0 To 22
                    batRating(P9, J, SS_B_Avail) = 0
                Next

                RP = 1

                For J = 0 To 22
                    If batRating(P9, J, SS_B_Active) <> 0 Then
                        batRating(P9, J, SS_B_Avail) = 99
                    End If
                Next

                batterFlag% = 0

                Exit Sub

            End If

        End If

        ' Check if user pressed Tab
        If Asc(I$) = 9 Then
            TG = TG + 1
        End If

        If TG > 2 Or TG = 2 And teamSplit(P9, 0) <> 999 Then
            TG = 0
        End If

        If Asc(I$) = 9 Then
            '   Update display after TAB
            Select Case TG
                Case 0:
                    Call SHOWBATTERS(P9)
                Case 1:
                    Call SHOWFIELD(P9)
                Case 2:
                    Call ShowSplit(P9)
            End Select

        Else
            'Not TAB / ESC
            'Deteremine index of batter chosen
            Call LetterToNumber(I$, I1)

            'Assign field position
            Select Case UCase$(I$)

                Case "Z":
                    TG = 0
                    Call SHOWBATTERS(P9)

                Case "X":
                    For I3 = 0 To 22
                        If batRating(P9, I3, SS_B_Avail) = 99 And batRating(P9, I3, SS_B_Active) = 0 Then
                            batRating(P9, I3, SS_B_Avail) = 0
                            Locate I3 + 2, 3: Print batters$(P9, I3)
                        End If
                    Next I3

                Case Else:
                    If I1 >= 0 And I1 <= 22 Then
                        batRating(P9, I1, SS_B_Avail) = 99
                        Color 0, 7
                        Locate I1 + 2, 3: Print batters$(P9, I1);
                        Color 15, 0
                    End If

            End Select

        End If 'Done checking for TAB

        'If it wasn't an ESC, let's print the batter
        If I$ <> Chr$(27) Then

            Color 15, 0
            Locate 5, 59: Print "OPPOSING PITCHER"
            Locate 6, 59: Print pitchers$(1 - P9, currPitcher(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher(1 - P9), SS_P_Throws) + 2)
            Inotloop% = 0
            batterFlag% = 1

        End If 'Done checking for ESC

    Loop Until UCase$(I$) = "Z"

End Sub

'----------------------------------------
'        SelectStadium Subroutine
'----------------------------------------
'This subroutine assists the user with the
'choice of any of the pre-created graphics
'for the stadiums.
Sub SelectStadium (useVGA)

    Do
        'SELECT A DIFFERENT STADIUM
        PARK$(1) = "ANAHEIM STADIUM             "
        PARK$(2) = "THE ASTRODOME               "
        PARK$(3) = "OAKLAND-ALAMEADA COLISEUM   "
        PARK$(4) = "SKYDOME                     "
        PARK$(5) = "ATLANTA-FULTON COUNTY STAD. "
        PARK$(6) = "MILWAUKEE COUNTY STADIUM    "
        PARK$(7) = "BUSCH MEMORIAL STADIUM      "
        PARK$(8) = "WRIGLEY FIELD               "
        PARK$(9) = "DODGER STADIUM              "
        PARK$(10) = "OLYMPIC STADIUM             "
        PARK$(11) = "CANDLESTICK PARK            "
        PARK$(13) = "SEATTLE KINGDOME            "
        PARK$(12) = "CLEVELAND MUNICIPAL STADIUM "
        PARK$(14) = "SHEA STADIUM                "
        PARK$(15) = "BALTIMORE MEMORIAL STADIUM  "
        PARK$(16) = "JACK MURPHY STADIUM         "
        PARK$(17) = "VETERAN'S STADIUM           "
        PARK$(18) = "THREE RIVERS STADIUM        "
        PARK$(19) = "ARLINGTON STADIUM           "
        PARK$(20) = "RIVERFRONT STADIUM          "
        PARK$(21) = "FENWAY PARK                 "
        PARK$(22) = "ROYALS/KAUFMANN STADIUM     "
        PARK$(23) = "TIGER/BRIGGS/NAVIN STADIUM  "
        PARK$(24) = "H.H.H. METRODOME            "
        PARK$(25) = "OLD COMISKEY PARK           "
        PARK$(26) = "OLD YANKEE STADIUM          "
        PARK$(27) = "LOS ANGELES COLISEUM        "
        PARK$(28) = "COLT STADIUM                "
        PARK$(29) = "CONNIE MACK/SHIBE STADIUM   "
        PARK$(30) = "EXHIBITION FIELD (TOR)      "
        PARK$(31) = "PILOT FIELD                 "
        PARK$(32) = "COUNTY STADIUM (MIL)        "
        PARK$(33) = "SPORTSMAN'S PARK            "
        PARK$(34) = "EBBETTS FIELD               "
        PARK$(35) = "JARRY PARK                  "
        PARK$(36) = "POLO GROUNDS                "
        PARK$(37) = "CROSLEY FIELD               "
        PARK$(38) = "RFK STADIUM                 "
        PARK$(39) = "METROPOLITAN STADIUM        "
        PARK$(40) = "FORBES FIELD                "
        PARK$(41) = "GRIFFITH STADIUM            "
        PARK$(42) = "SAN DIEGO STADIUM           "
        PARK$(43) = "KAN CITY MUNICIPAL STADIUM  "
        PARK$(44) = "SICKS STADIUM               "
        PARK$(45) = "LOS ANGELES WRIGLEY FIELD   "
        PARK$(46) = "SEALS STADIUM               "
        PARK$(47) = "*AMERICAN LEAGUE            "
        PARK$(48) = "*AMERICAN LEAGUE II         "
        PARK$(49) = "HILLTOP PARK                "
        PARK$(50) = "POLO GROUNDS (1913-1922)    "
        PARK$(51) = "BRAVES FIELD (1915-1952)    "
        PARK$(52) = "BRAVES FIELD (1915-1916)    "
        PARK$(53) = "SOUTHSIDE PARK              "
        PARK$(54) = "BAKER BOWL                  "
        PARK$(55) = "CAMDEN YARDS                "
        PARK$(56) = "COLUMBIA PARK               "
        PARK$(57) = "EXPOSITION PARK             "
        PARK$(58) = "LEAGUE PARK/PALACE OF FANS  "
        PARK$(59) = "WEST SIDE PARK              "
        PARK$(60) = "SOUTH END GROUNDS           "
        PARK$(61) = "HUNTINGTON AVENUE GROUNDS   "
        PARK$(62) = "LEAGUE PARK                 "
        PARK$(63) = "BENNETT PARK                "
        PARK$(64) = "WASHINGTON PARK             "
        PARK$(65) = "MILE HIGH STADIUM           "
        PARK$(66) = "JOE ROBBIE STADIUM          "
        PARK$(67) = "JACOBS FIELD                "
        PARK$(68) = "BALLPARK AT ARLINGTON       "
        PARK$(69) = "COORS STADIUM               "
        PARK$(70) = "NEW YANKEE STADIUM          "
        PARK$(71) = "NEW COMISKEY PARK           "
        PARK$(72) = "BUSCH STADIUM - GRASS       "
        PARK$(73) = "TURNER FIELD                "
        PARK$(74) = "BANK ONE BALLPARK           "
        PARK$(75) = "TROPICANA FIELD             "
        PARK$(76) = "SAFECO FIELD (SEA)          "
        PARK$(77) = "ENRON PARK (HOU)            "
        PARK$(78) = "PACIFIC BELL PARK (SF)      "
        PARK$(79) = "COMERICA PARK (DET)         "
        PARK$(80) = "                            "
        PARK$(81) = "PNC PARK                    "
        PARK$(82) = "MILLER PARK-MILWAUKEE       "
        PARK$(83) = "UNKNOWN                     "
        PARK$(99) = "GENERIC                     "

        Call TEAMBOX(5, 1)

        'Determine park from the team data
        PARK$ = Right$(Str$(teamRat_GAME(1, 13)), Len(Str$(teamRat_GAME(1, 13))) - 1)

        If dayOrNight = 0 Then
            '0 = daytime
            PARK$ = PARK$ + "D.png"
        Else
            '1 = nighttime
            PARK$ = PARK$ + "N.png"
        End If

        'Display detected park
        Locate 7, 11: Print "CURRENT PARK IS:"
        Locate , 11: Print PARK$(teamRat_GAME(1, 13))

        Locate , 11

        If _FileExists("." + dirSeparator$ + "parks" + dirSeparator$ + PARK$) Then
            Print "GRAPHIC EXISTS"
        Else
            '99 is the Generic Park
            Print "GRAPHIC NOT AVAILABLE"
            PARK$ = "99N.png"
        End If

        'We may have checked if the file associated with the park
        'exists, but we haven't checked if any graphics exist at all
        If autoPlay <> 1 Then

            'No autoplay
            Do
                Locate 10, 11
                Print "SELECT A DIFFERENT STADIUM? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

            If UCase$(I$) = "Y" Then

                Cls
                Locate 23, 10: Print "* - STADIUM GRAPHIC FILE NOT AVAILABLE"
                Start% = 1

                Do
                    SELECTION% = 1
                    Count% = 15
                    ExitCode% = 99
                    FileFGColor = 15: FileBGColor = 1
                    HiLiteFGColor = 0: HiLiteBGColor = 7
                    Spacing% = 1
                    Row% = 5
                    menuWidth% = 38 'Inserting arbitrary value... menuChoices$() not available here!   Len(menuChoices$(1))
                    Column% = (80 - menuWidth% - 2) / 2

                    Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

                    _MouseShow "Default"
                    MAMenu PARK$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, 15, 1, 1, Row%, Column%
                    _MouseHide

                    Color , 0

                    If ExitCode% <> 27 Then

                        PARK$ = Right$(Str$(SELECTION%), Len(Str$(SELECTION%)) - 1)
                        If dayOrNight = 0 Then PARK$ = PARK$ + "D.png" Else PARK$ = PARK$ + "N.png"
                        Color 15, 0

                        If _FileExists(".\parks\" + PARK$) Then
                            foundPark% = 1
                            teamRat_GAME(1, 13) = SELECTION%
                            gameStadiums$(1) = PARK$(SELECTION%)
                        Else
                            Do
                                Locate 10, 8
                                Print "STADIUM GRAPHIC NOT AVAILABLE"
                                Locate , 8: Print "USE SELECTED PARK RATINGS WITH"
                                Locate , 8: Print "GENERIC DISPLAY? (Y/N)"
                                J$ = GetKeyPress$
                            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                            If I$ = "Y" Then
                                teamRat_GAME(1, 13) = SELECTION%: gameStadiums$(1) = PARK$(SELECTION%)
                                If dayOrNight = 0 Then PARK$ = "99D.png" Else PARK$ = "99N.png"
                            End If

                        End If

                    Else
                        BO% = 1

                    End If

                Loop Until UCase$(J$) = "Y" Or ExitCode% = 27 Or foundPark% = 1

            End If

        End If 'Done checking auto play

        'We are set for autoplay, and checked if the stadium
        'graphic exists for the park, but not if the generic
        'stadium graphic exists. We would only be in this
        'position if no graphics exist at all!

        'So instead, let's switch to regular mode
        Test$ = "." + dirSeparator$ + "parks" + dirSeparator$ + PARK$
        If Not _FileExists("." + dirSeparator$ + "parks" + dirSeparator$ + PARK$) Then useVGA = 0

    Loop Until BO% = 0 Or UCase$(I$) = "N" Or ExitCode% = 27

End Sub


'----------------------------------------
'      PitchingRotations Subroutine
'----------------------------------------
'This subroutine guides the user through
'the various ways that pitching rotations
'can be configured for the game.
Sub PitchingRotations (computerRotations%, P9)

    Dim gamesStarted%(21, 1)

    Cls
    Call INFO

    Locate 2, 1

    For I = 1 To 5
        Color 14: Print "(";: Print Using "#"; I;: Print ") ";: Color 15: Print "MAN ROTATION"
    Next

    Print "YOU MAY CONSIDER THE FOLLOWING GUIDELINES:"
    Print "MAX GAMES STARTED BY ANY PITCHER           ROTATION SIZE"
    Print "          32                                    5"
    Print "          41                                    4"
    Print "          54                                    3"
    Print "          75                                    2"
    Print "         ALL                                    1"
    Print

    For I = 0 To 6

        Locate I + 15, 1
        If pitchers$(P9, I) <> "XXX" Then
            Print pitchers$(P9, I); Tab(18); pitchRating(P9, I, SS_P_GS);
        End If

        Locate , 25
        If pitchers$(P9, I + 7) <> "XXX" Then
            Print pitchers$(P9, I + 7); Tab(43); pitchRating(P9, I + 7, 5);
        End If

        Locate , 50
        If pitchers$(P9, I + 14) <> "XXX" Then
            Print pitchers$(P9, I + 14); Tab(68); pitchRating(P9, I + 14, 5)
        End If

    Next

    If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = P9) Then
        '   4685
        gamesStarted = 0

        For I = 0 To 21
            If pitchRating(P9, I, SS_P_GS) > gamesStarted Then gamesStarted = pitchRating(P9, I, SS_P_GS)
        Next

        'Auto-determine # of men in rotation
        'based on # of games started?
        I = 1
        Select Case gamesStarted

            Case 1 To 32: I = 5
            Case 33 To 41: I = 4
            Case 42 To 54: I = 3
            Case 55 To 75: I = 2

        End Select

    Else

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 5

    End If

    '4615
    teamMgrProfileVal(P9, 1) = I
    teamMgrProfileVal(P9, 2) = 0

    For I = 3 To 36
        teamMgrProfileVal(P9, I) = -1
    Next

    For I = 41 To 56
        teamMgrProfileVal(P9, I) = 0
    Next

    teamMgrProfileVal(P9, 0) = 999

    Inotloop% = 99

    Call PrintPitcherInfo(P9)

    If computerRotations% = 0 Then

        For I = 0 To teamMgrProfileVal(P9, 1) - 1

            Do

                'We need to skip this in certain circumstances
                'Right now, it is being loaded for auto-play
                Locate 15, 59: Print "STARTER "; I + 1

                Do
                    I$ = GetKeyPress$
                    Call LetterToNumber(I$, I1)
                Loop Until I1 <= 21 And teamMgrProfileVal(P9, 3) <> I1 And teamMgrProfileVal(P9, 4) <> I1 And teamMgrProfileVal(P9, 5) <> I1 And teamMgrProfileVal(P9, 6) <> I1 And teamMgrProfileVal(P9, 7) <> I1

                Locate 16, 59: Print pitchers$(P9, I1)

                Do
                    Locate 17, 59: Print "ANY CHANGE? (Y/N)";
                    J$ = GetKeyPress$
                Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                If UCase$(J$) = "Y" Then
                    For I2 = 15 To 17:
                        Locate I2, 59: Print Space$(17)
                    Next
                End If

            Loop Until UCase$(J$) = "N"

            teamMgrProfileVal(P9, I + 3) = I1

            For I2 = 15 To 17:
                Locate I2, 59: Print Space$(18)
            Next

        Next

    Else

        ' COMPUTER SET ROTATION
        'Make sure our next team isn't auto-set
        'without a choice
        computerRotations% = 0

        For I = 0 To 21
            If pitchers$(P9, I) = "XXX" Then
                gamesStarted%(I, 0) = 0
                gamesStarted%(I, 1) = -1
            Else
                gamesStarted%(I, 0) = pitchRating(P9, I, SS_P_GS)
                gamesStarted%(I, 1) = I
            End If
        Next

        For Z = 0 To 20:
            For Z1 = Z + 1 To 21:
                If gamesStarted%(Z, 0) < gamesStarted%(Z1, 0) Then
                    Swap gamesStarted%(Z, 0), gamesStarted%(Z1, 0)
                    Swap gamesStarted%(Z, 1), gamesStarted%(Z1, 1)
                End If
            Next Z1
        Next Z

        For I = 0 To teamMgrProfileVal(P9, 1) - 1
            teamMgrProfileVal(P9, I + 3) = gamesStarted%(I, 1)
        Next

        I1 = 0

        For I = teamMgrProfileVal(P9, 1) To 21
            I1 = I1 + 1

            If gamesStarted%(I, 0) <> 0 Then
                teamMgrProfileVal(P9, I1 + 20) = gamesStarted%(I, 1)
            Else
                Exit For
            End If
        Next

    End If

    I1 = I1 - 1

    If teamMgrProfileVal(P9, 1) = 1 Then
        teamMgrProfileVal(P9, 13) = 100
    Else
        If teamMgrProfileVal(P9, 1) = 2 Then gamesStarted% = 75
        If teamMgrProfileVal(P9, 1) = 3 Then gamesStarted% = 54
        If teamMgrProfileVal(P9, 1) = 4 Then gamesStarted% = 41
        If teamMgrProfileVal(P9, 1) = 5 Then gamesStarted% = 32

        For I = 1 To teamMgrProfileVal(P9, 1)
            teamMgrProfileVal(P9, I + 12) = CInt(pitchRating(P9, teamMgrProfileVal(P9, I + 2), 5) * 100 / gamesStarted%)
        Next

        SS = 0

        For I = 21 To 36
            If teamMgrProfileVal(P9, I) >= 0 And teamMgrProfileVal(P9, I) <= 21 Then
                SS = SS + pitchRating(P9, teamMgrProfileVal(P9, I), 5)
            End If
        Next

        For I = 21 To 36
            If teamMgrProfileVal(P9, I) >= 0 And teamMgrProfileVal(P9, I) <= 21 Then
                teamMgrProfileVal(P9, I + 20) = CInt(pitchRating(P9, teamMgrProfileVal(P9, I), 5) * 100 / SS)
            End If
        Next

    End If

End Sub


'----------------------------------------
'       ComputerLineups Subroutine
'----------------------------------------
'This subroutine is built with the logic
'to automatically select batters, before
'presenting the user a final summary of the
'results.
Sub ComputerLineups (batterFlag%, P9, noLineups, reselect)

    Shared BP(), batLUIdx()
    Shared BB%()

    'batter #, team
    Dim position(23, 1)

    For I = 0 To 22
        'Essentially a form of setting the player as inactive
        If batters$(P9, I) = "XXX" Then
            batRating(P9, I, SS_B_Avail) = 98
        End If
    Next

    Call SelectBatters(batterFlag%, P9)

    If batterFlag% <> 0 Then

        For I = 10 To 22
            Locate I, 59: Print Space$(21);
        Next

        Erase batLUIdx, BP

        For I = 1 To 9:
            BB%(I) = -1
        Next

        M = 0

        'Go backward through each position
        For I = 9 To 1 Step -1

            J = I

            'Initialize
            For I2 = 0 To 22:
                position(I2, 0) = 0
                position(I2, 1) = -1
            Next

            currPos = 0

            'Final Position
            If I = 1 Then

                'No DH - assign pitcher
                If desigHit = 0 Then
                    BP(9) = 1
                    Exit For
                Else
                    J = J - 1
                    currPos = 0

                    For batterIdx = 0 To 22

                        If batRating(P9, batterIdx, SS_B_Avail) <= 0 And batters$(P9, batterIdx) <> "XXX" Then

                            If batRating(P9, batterIdx, SS_B_Pos1) = 0 Then
                                If batRating(P9, batterIdx, SS_B_Pos2) = -1 Then
                                    position(currPos, 0) = batRating(P9, batterIdx, SS_B_AB)
                                ElseIf batRating(P9, batterIdx, SS_B_Pos3) = -1 Then
                                    position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .9 + .5)
                                ElseIf batRating(P9, batterIdx, SS_B_Pos4) = -1 Then
                                    position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .8 + .5)
                                Else
                                    position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .7 + .5)
                                End If

                            ElseIf batRating(P9, batterIdx, SS_B_Pos2) = 0 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .5 + .5)
                            ElseIf batRating(P9, batterIdx, SS_B_Pos3) = 0 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .3 + .5)
                            ElseIf batRating(P9, batterIdx, SS_B_Pos4) = 0 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .2 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .05 + .5) ' no DH experience
                            End If

                            position(currPos, 1) = batterIdx
                            currPos = currPos + 1

                        End If

                    Next batterIdx

                End If
            Else

                'I <> 1
                ' For each fielding position I (1-9), for each active player who plays that position, and store his
                ' "normalized" at-bats in position(currPos, 0) and the field position # in position(currPos, 1)

                'Most positions
                For batterIdx = 0 To 22:
                    If batRating(P9, batterIdx, SS_B_Avail) <= 0 And batters$(P9, batterIdx) <> "XXX" Then

                        If batRating(P9, batterIdx, SS_B_Pos1) = I Then
                            If batRating(P9, batterIdx, SS_B_Pos2) = -1 Then
                                position(currPos, 0) = batRating(P9, batterIdx, SS_B_AB)
                            ElseIf batRating(P9, batterIdx, SS_B_Pos3) = -1 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .9 + .5)
                            ElseIf batRating(P9, batterIdx, SS_B_Pos4) = -1 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .8 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .7 + .5)
                            End If

                        ElseIf batRating(P9, batterIdx, SS_B_Pos2) = I Then
                            If batRating(P9, batterIdx, SS_B_Pos3) = -1 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .1 + .5)
                            ElseIf batRating(P9, batterIdx, SS_B_Pos4) = -1 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .1 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .1 + .5)
                            End If

                        ElseIf batRating(P9, batterIdx, SS_B_Pos3) = I Then
                            If batRating(P9, batterIdx, SS_B_Pos4) = -1 Then
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .1 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .1 + .5)
                            End If

                        ElseIf batRating(P9, batterIdx, SS_B_Pos4) = I Then
                            position(currPos, 0) = Int(batRating(P9, batterIdx, SS_B_AB) * .1 + .5)
                        End If

                        position(currPos, 1) = batterIdx
                        currPos = currPos + 1

                    End If

                Next batterIdx

            End If 'Done checking value of I

            currPos = currPos - 1
            AB% = 0

            For I1 = 0 To currPos
                AB% = AB% + position(I1, 0)

            Next

            RN = Int(Rnd(1) * AB%) + 1

            ' For each fielding position I (1-9), find a position(I1, 0) [i.e., at-bats] value that is above the random threshold, and
            ' assign the player to that position

            lineupDone% = 0

            For I1 = 0 To currPos

                If RN <= position(I1, 0) Then
                    batLUIdx(J) = position(I1, 1)
                    batRating(P9, position(I1, 1), SS_B_Avail) = 1
                    lineupDone% = 1

                    Exit For
                Else
                    position(I1 + 1, 0) = position(I1 + 1, 0) + position(I1, 0)

                End If

            Next I1

            If lineupDone% <> 1 Then
                Locate 25, 1: Print "CANNOT MAKE A LINEUP!!...";
                noLineups = 1
                'reselect = 1

                Erase gameM

                For J = 1 To 9:
                    fldPos(P9, J) = -1
                    lineupPlayer(P9, J) = -1
                Next

                C1 = 0

                For J = 0 To 22
                    If batRating(P9, J, SS_B_Avail) <> 99 Then batRating(P9, J, SS_B_Avail) = 0
                Next

                If autoPlay <> 1 Then
                    I$ = GetKeyPress$
                    Exit For
                End If

            End If 'done checking lineup

        Next I

        Call SortHitters(P9)

        Call LineupSummary(P9, reselect, LU%)

    End If 'done checking batterFlag

End Sub


'----------------------------------------
'         SortHitters Subroutine
'----------------------------------------
'This subroutine is used to perform a sort
'on all the hitters before presenting the
'lineup summary to the user.
Sub SortHitters (P9)

    Shared BP()
    Shared BB%()

    C = 2

    B = 1
    Call DoSort(B, C, P9)

    B = 2
    Call DoSort(B, C, P9)

    C = 1

    B = 3
    Call DoSort(B, C, P9)

    B = 4
    Call DoSort(B, C, P9)

    C = 1

    B = 5
    Call DoSort(B, C, P9)

    B = 6
    Call DoSort(B, C, P9)

    B = 7
    Call DoSort(B, C, P9)

    B = 8
    Call DoSort(B, C, P9)

    If desigHit = 1 Then
        B = 9
        Call DoSort(B, C, P9)
    End If

    'I have not found a cause, but sometimes
    'spot 8 doesn't get populated. The loop
    'in DoSort exits too early.
    'My fix here is to find an available position
    'and assign that to the slot.
    'We start at slot 3 b/c the first few slots
    'are often main pitchers and rarely get
    'picked by the DoSort routine
    If BB%(8) < 0 Then

        'Find an open spot
        'Don't start with first few
        openPos = 3
        currIdx = 1

        Do
            'If current spot is "used", move to next spot
            If (BB%(currIdx) = openPos) Then
                openPos = openPos + 1
            End If
            currIdx = currIdx + 1
        Loop Until currIdx > 9

        BB%(8) = openPos

        'We need to find whatever number is missing
        For X = 1 To 9
            assigned(X) = BP(X)
        Next X

        For a = 9 To 1 Step -1
            For B = 1 To 9
                If assigned(B) > assigned(a) Then
                    Swap assigned(a), assigned(B)
                End If
            Next B
        Next a

        'Find an open spot
        '--Don't look for DH / pitcher
        openPos = 2
        currIdx = 1
        Do
            'If current spot is "used", move to next spot
            If (assigned(currIdx) = openPos) Then
                openPos = openPos + 1
            End If
            currIdx = currIdx + 1
        Loop Until currIdx > 9

        BP(8) = openPos

    End If

    For I = 1 To 9
        lineupPlayer(P9, I) = BB%(I)
        fldPos(P9, I) = BP(I)
    Next

End Sub


'----------------------------------------
'      DoSort Subroutine
'----------------------------------------
'I literally have no clue what this sub-
'routine does. It has very little context
'that hints to it's purpose.
'It is only called by the routine I've
'named "SortHitters", and likely DOES
'some of the actual sorting as it's
'called after each lineup position.
Sub DoSort (B, C, P9)

    Shared BP(), batLUIdx()
    Shared E!
    Shared BB%()

    Dim JJ!, R!, SE!, Q1!, QQ!

    E! = -100
    U = -1
    T = -1

    'L = lineup position
    For L = 0 To 9

        If desigHit = 0 And L = 0 Then L = L + 2
        If desigHit = 1 And L = 1 Then L = L + 1

        'Assign J based on lineup position
        J = batLUIdx(L)
        X = 1

        For Z = 1 To 9
            If BB%(Z) = J Then
                'If we found the matching player, loop to next lineup position
                X = 0
                Z = 9
            End If
        Next Z

        If X <> 0 Then

            If C <> 1 Then

                SE! = (batRating(P9, J, SS_B_H) + batRating(P9, J, SS_B_2B) + batRating(P9, J, SS_B_3B) * 2 + batRating(P9, J, SS_B_HR) * 3) / batRating(P9, J, SS_B_AB)

                If SE! >= E! Then
                    E! = SE!
                    T = J
                    U = L
                End If

            Else

                JJ = J

                'Hits - Triples - HR's + Walks * 0.7
                'Or, basically Singles + Doubles + Walks * 0.7
                Q1! = (batRating(P9, JJ, SS_B_H) - batRating(P9, JJ, SS_B_3B) - batRating(P9, JJ, SS_B_HR) + batRating(P9, JJ, SS_B_W)) * .7

                If Q1! = 0 Then Q1! = 1

                'Stolen Bases / (Singles + Doubles + Walks * 0.7)
                Q1! = batRating(P9, JJ, 13) / Q1!

                If Q1! >= .02 Then
                    QQ! = Q1! + .18
                    R! = Int(QQ! * (18 + 2 * Abs((QQ! < .34))))
                Else
                    R! = 2 + Int(Q1! * 100)
                    If batRating(P9, JJ, 1) = 2 Then R = R - 1
                End If

                If (batRating(P9, JJ, 1) = 6 Or batRating(P9, JJ, 1) = 8) And R! < 7 Then
                    R! = R! + 1
                End If

                If R! > 9 Then R! = 9
                If R! < 0 Then R! = 1

                T1 = 100 * batRating(P9, J, SS_B_H) / batRating(P9, J, SS_B_AB) - 27
                If T1 < 0 Then T1 = 3 * T1

                JJ! = 3 * R! + T1

                If JJ! > E! Then
                    E! = JJ!
                    T = J
                    U = L
                End If

            End If 'Done checking C

        End If 'Done checking X

    Next L

    BB%(B) = T
    BP(B) = U

End Sub


'----------------------------------------
'       SelectPitchers Subroutine
'----------------------------------------
'This subroutine walks the user through
'all available options for selecting a
'rotation of pitchers.
Sub SelectPitchers (idx%, cancelPitchers%, computerRotations%)

    Shared gameMgrIdx(), mgrFile(), startPitcher()

    ' *** PITCHING ROTATIONS ***
    Inotloop% = 99
    skipDisablePitchers% = 0

    Call PrintPitcherInfo(idx%)

    Locate 15, 57: Print "0-MANUALLY PICK STARTER"
    Locate 16, 57: Print "1-USE PRE-SET ROTATION"
    Locate 17, 57: Print "2-ENTER A NEW ROTATION"

    If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = idx%) Then
        'For automated games, use preset rotation
        'This will end up being computer-generated
        choice = 1

    Else

        Do
            I$ = GetKeyPress$
            If I$ <> Chr$(27) Then
                choice = Val(I$)
            End If
        Loop Until choice >= 0 And choice <= 2 Or I$ = Chr$(27)

    End If

    If I$ <> Chr$(27) Then

        Select Case choice

            Case 0:
                'manual pick starter means we are
                'not going to disable anyone, but
                'just simply pick 1 pitcher
                skipDisablePitchers = 1
                startPitcher(idx%) = -1

            Case 1:
                'Pre-Set Rotation
                Cls

                If mgrFile(idx%) = -1 Or teamMgrProfileVal(idx%, 99) <> 999 Then

                    If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = idx%) Then
                        computerRotations% = 1
                    Else
                        'This should instead go to automatic disable of rotations???
                        Cls
                        Print "ERROR:: ROTATION NOT FOUND"
                        Print
                        Print "DO YOU WANT TO ENTER A ROTATION? (Y/N)"

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
                    End If

                    'Don't we want to skipDisablePitchers
                    'when we are doing computerRotations
                    'as well ????
                    If computerRotations% = 0 Then

                        If UCase$(I$) = "N" Then
                            skipDisablePitchers% = 1
                        Else
                            Print "COMPUTER SET ROTATION? (Y/N) ";

                            Do
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                            If UCase$(I$) = "Y" Then computerRotations% = 1

                        End If

                    End If

                    Call PitchingRotations(computerRotations%, idx%)

                Else
                    skipDisablePitchers% = 0
                End If

            Case 2:

                Do
                    Cls
                    Print "COMPUTER SET ROTATION? (Y/N) ";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then computerRotations% = 1

                Call PitchingRotations(computerRotations%, idx%)

        End Select

        If skipDisablePitchers% <> 1 Then
            'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

            'For I = 0 To teamMgrProfileVal(idx%, 1) - 1
            '    pitchRating(idx%, teamMgrProfileVal(idx%, 3 + I), 15) = 99
            'Next

            Do
                Call Rolld100(RN)

                pitchRating(idx%, teamMgrProfileVal(idx%, teamMgrProfileVal(idx%, 2) + 3), SS_P_Avail) = 0

                If RN <= teamMgrProfileVal(idx%, teamMgrProfileVal(idx%, 2) + 10 + 3) Or teamMgrProfileVal(idx%, teamMgrProfileVal(idx%, 2) + 10 + 3) = 0 Then
                    startPitcher(idx%) = teamMgrProfileVal(idx%, teamMgrProfileVal(idx%, 2) + 3)
                    doneAutoDisable% = 1
                Else

                    Call Rolld100(RN)
                    SP% = 0

                    For J = 21 To 36

                        If teamMgrProfileVal(idx%, J) <> -1 And teamMgrProfileVal(idx%, J) >= 0 And teamMgrProfileVal(idx%, J) <= 21 Then

                            SP% = SP% + teamMgrProfileVal(idx%, J + 20)

                            If RN <= SP% And teamMgrProfileVal(idx%, J) <> -1 Then
                                pitchRating(idx%, teamMgrProfileVal(idx%, J), SS_P_Avail) = 0
                                startPitcher(idx%) = teamMgrProfileVal(idx%, J)
                                doneAutoDisable% = 1
                                Exit For
                            End If

                        End If

                    Next

                End If

            Loop Until doneAutoDisable% = 1

            teamMgrProfileVal(idx%, 2) = teamMgrProfileVal(idx%, 2) + 1

            If teamMgrProfileVal(idx%, 2) = teamMgrProfileVal(idx%, 1) Then teamMgrProfileVal(idx%, 2) = 0

            teamMgrProfileVal(idx%, 99) = 999
            teamMgrProfileVal(idx%, 0) = 999
            mgrFile(idx%) = 0

            teamYear$ = RTrim$(diskIDs$(idx%))

            Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = MGR_SIZE_BYTES

            For I = 0 To 999
                Field #1, I * 2 As Q$, 2 As Q2$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
            Next

            For I = 0 To 999:
                LSet Q2$(I) = MKI$(teamMgrProfileVal(idx%, I))
            Next

            Put #1, gameMgrIdx(idx%)

            Close #1

        End If

        Do

            Inotloop% = 99

            Call PrintPitcherInfo(idx%)

            Locate 15, 58: Print "DISABLE PITCHER (0-L)"
            Locate , 58: Print "Y-GRANT DAY OFF"
            Locate , 58: Print "Z-CONTINUE"
            Locate , 58: Print "X-ACTIVATE ALL PITCHERS"

            If autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = idx%) Then
                I$ = "Z"
            End If

            While UCase$(I$) <> "Z" And I$ <> Chr$(27)

                I$ = GetKeyPress$

                If I$ <> Chr$(27) And UCase$(I$) <> "Z" Then

                    Call LetterToNumber(I$, I1)

                    Select Case UCase$(I$)
                        Case "Y":
                            ':GRANT DAY OFF
                            If useRest = 1 Then
                                For I1 = 0 To 21
                                    If pitchRating(idx%, I1, SS_P_Rest) > 0 Then pitchRating(idx%, I1, SS_P_Rest) = pitchRating(idx%, I1, SS_P_Rest) - 1
                                Next
                            End If

                        Case "X":
                            'Activate all pitchers
                            For I3 = 0 To 21
                                If pitchRating(idx%, I3, SS_P_Avail) = 99 And pitchRating(idx%, I3, SS_P_Active) = 0 Then pitchRating(idx%, I3, SS_P_Avail) = 0: Locate I3 + 2, 3: Print pitchers$(idx%, I3)
                            Next I3

                        Case Else:
                            If I1 <= 21 Then
                                'If a pitcher was selected, disable them
                                If pitchRating(idx%, I1, SS_P_Avail) = 0 Then
                                    pitchRating(idx%, I1, SS_P_Avail) = 99: Locate I1 + 2, 3: Color 0, 7: Print pitchers$(idx%, I1): Color 15, 0
                                Else
                                    If pitchRating(idx%, I1, SS_P_Avail) <> 0 Then pitchRating(idx%, I1, SS_P_Avail) = 0: Locate I1 + 2, 3: Color 15, 0: Print pitchers$(idx%, I1): Color 15, 0
                                End If
                            End If

                    End Select

                End If

            Wend

        Loop Until UCase$(I$) = "Z" Or I$ = Chr$(27)

        'cancelPitchers% = 0
    Else
        cancelPitchers% = 1
    End If 'Done checking for ESC from rotation choice

    If I$ <> Chr$(27) Then
        'We pressed Z
        'Reset the value
        Inotloop% = 0

    Else
        'We cancelled
        cancelPitchers% = 1
    End If 'Done checking for ESC

End Sub


'----------------------------------------
'      PrintPitcherInfo Subroutine
'----------------------------------------
'This routine outputs the information for
'the target pitcher to the display.
Sub PrintPitcherInfo (idx%)

    '1520
    Cls
    Print "#";: Color teamRat_GAME(idx%, 11), teamRat_GAME(idx%, 12): Print gameTeams$(idx%);
    Color 14, 0: Locate , 16
    Print "T  IP   H  G  GS DR  BB  SO   W- L   ERA"

    For I = 0 To 21

        If pitchers$(idx%, I) = "XXX" Or Left$(pitchers$(idx%, I), 1) = " " Or pitchers$(idx%, I) = "" Then pitchRating(idx%, I, SS_P_Avail) = 98

        If pitchRating(idx%, I, SS_P_Avail) <> 1 And pitchers$(idx%, I) <> "XXX" Then
            Color 2, 0

            If I <= 9 Then
                Print Using "#"; I;: Color 15, 0
            Else
                'I > 9
                Print alpha$(I - 10);: Color 15, 0
            End If

            If pitchRating(idx%, I, SS_P_Avail) >= 98 Then Color 0, 7

            Locate , 3: Print pitchers$(idx%, I);
            Color 15, 0

            Locate , 16: Print handed$(pitchRating(idx%, I, SS_P_Throws) + 2);
            '                          IP  H   G   GS DR BB  SO
            Locate , 17: Print Using " ### ### ### ## ## ### ###"; pitchRating(idx%, I, 6); pitchRating(idx%, I, 7); pitchRating(idx%, I, 4); pitchRating(idx%, I, 5); pitchRating(idx%, I, SS_P_Rest); pitchRating(idx%, I, 8); pitchRating(idx%, I, 9);
            '                           W                                                       L  ERA
            Locate , 43: Print Using "  ##"; pitchRating(idx%, I, 1);: Print "-";: Print Using "## ##.##"; pitchRating(idx%, I, 2); pitchRating(idx%, I, SS_P_ERA100) / 100
        End If

    Next I

    If Inotloop% <= 0 Then

        Call PitchingStarter(idx%)
        'If Inotloop% <= 5 And gameScore(P9, 0) - gameScore(1 - P9, 0) > 0 Then P2%(P9) = P1(P9)

    End If

End Sub


'----------------------------------------
'        PitchingStarter routine
'----------------------------------------
'This subroutine works with the user to
'choose a starting pitcher. The choices
'will have already been printed to the
'screen from other routines.
Sub PitchingStarter (tmIdx)

    Shared startPitcher()

    Do

        Locate 19, 58: Print "SELECT STARTER"

        If startPitcher(tmIdx) <> -1 Then

            pitchIdx = startPitcher(tmIdx)

        Else

            Do
                I$ = GetKeyPress$
                Call LetterToNumber(I$, pitchIdx)
            Loop Until pitchIdx <= 21 And pitchRating(tmIdx, pitchIdx, SS_P_Avail) <= 0

        End If

        Locate 20, 58

        Print pitchers$(tmIdx, pitchIdx); " "; handed$(pitchRating(tmIdx, pitchIdx, SS_P_Throws) + 2)

        Print

        If Not (autoPlay = 1 And (playerMode = 2 Or playerMode = 1 And compTeam = tmIdx)) Then

            Do
                Locate 21, 58: Print "ANY CHANGE? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                Locate 20, 58
                Print Space$(16)
                startPitcher(tmIdx) = -1
            End If

        Else

            I$ = "N"

        End If

    Loop Until UCase$(I$) = "N"

    currPitcher(tmIdx) = pitchIdx
    gamePitcher(tmIdx) = 1
    pitchRating(tmIdx, pitchIdx, SS_P_Avail) = gamePitcher(tmIdx)

    pitchEff(tmIdx, 0) = CInt(((pitchRating(tmIdx, currPitcher(tmIdx), 8) + pitchRating(tmIdx, currPitcher(tmIdx), 7)) / pitchRating(tmIdx, currPitcher(tmIdx), 4)))
    pitchEff(tmIdx, 1) = 3

    If gamePitcher(tmIdx) > 1 Then pitchEff(tmIdx, 1) = 0

End Sub



'Include: 'SOURCE.bm'

'----------------------------------------
'            SOURCE Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the Baseball game. This
'can be considered the equivalent of the
'COLHOOP / PROHOOP / FOOTBALL routines For
'other games. This name is the original one
'given, but may be changed to BASEBALL at
'some point in the near future (obviously).
Sub SOURCE ()

    If Environ$("PBPLOG") = "TRUE" Then Print #11, "=== "; gameTeams$(0) + " vs. " + gameTeams$(1); " ==="

    'Declare all variables for the modules
    Dim F1$(10)
    Dim PH%(0 To 22, 0 To 1)

    Shared grounderDir$()

    Shared triplePlayAdj(), SU()

    VV(0)  = 1 : VV(1)  = 1
    VV1(1) = 9 : VV1(1) = 9
    Call HITTINGLINES

    Call PITSTAM(0) : Call PITSTAM(1)

    If useVGA = 1 Then Call LOADSCREEN

   LNextHalfInning:

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11,
        Print #11, "..... side is retired ....."    
        Print #11,
    End If
    
    '-- An error comes up every so many games
    '-- when fielder(D, 1) gets a negative value
    '-- Nothing in the the code modifies fielder()
    '-- between here and when it is corrupted.
    '-- Here, we save the non-negative value
    '-- so we can restore after it's corrupted.
    b9d1 = fielder(D, 1)
    b9d2 = fielder(D, 2)

    Erase nbrOuts
    Erase b2Base, b2Pitcher, b2Mystery, b1Runner, b1Pitcher

    errorOnPlay% = 0
    runnerHeld = 0
    baseRunners = 0

    L0%(P) = L0%(P) + MO
    MO = 0

    If P = 1 Then
        INNING% = INNING% + 1
        Erase CZ%
    End If

    'Switch posession
    P = 1 - P
    D = 1 - P

    If INNING% = 7 And teamRat_GAME(1, 13) = 8 And P = 1 And Val(Left$(gameTeams$(1), 2)) > 75 Then
        Call WINDEX
        Call PBP("ALL RIGHT..."): Call PBP("LEMME HEAR YA..."): Call PBP("A ONE, A TWO...")
    End If

    'Check scores / innings
    '------------------------------
    If INNING% > 9 And D = 1 And gameScore(1, 0) <> gameScore(0, 0) Or INNING% = 28 Or INNING% >= 9 And P = 1 And gameScore(P, 0) - gameScore(D, 0) > 0 Then
        'After 9th inning, score not tied, etc..
        'End of game!
        GoTo LGameOver
    End If

    If rainDelay = 1 And INNING% >= rainStart And rainStart >= 6 And (gameScore(1, 0) > gameScore(0, 0) And P = 1 Or gameScore(0, 0) > gameScore(1, 0) And P = 0) Then

        Cls

        If useVGA = 0 Then Screen menu&

        Print "THIS GAME HAS BEEN CALLED ON ACCOUNT OF RAIN!!!"
        Print "ALL RECORDS ARE OFFICIAL!!"
        Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";

        I$ = GetKeyPress$

        'End of game!
        GoTo LGameOver

    End If

    If rainDelay = 1 And P = 0 And INNING% = 4 Then

        Call PBP("THE RAIN IS FALLING A LITTLE MORE STEADILY")
        Call PBP("THIS GAME MAY NOT GO 9!")
        I$ = GetKeyPress$

    End If

    'Game is not over - show the field
    If useVGA = 1 Then

        '-- Here, we restore the non-negative value
        '-- of fielder() to prevent an error
        ' fielder(D, 1) = b9d1
        ' fielder(D, 2) = b9d2

        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
    Else
        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
    End If

    For IJ0% = 0 To 22

        If batRating(D, IJ0%, SS_B_Avail) > 39 And batRating(D, IJ0%, SS_B_Avail) <= 49 Then

            I1 = IJ0%
            I2 = batRating(D, IJ0%, SS_B_Avail) - 40

            batRating(D, I1, SS_B_Avail) = 1
            lineupPlayer(D, I2) = I1
            fielder(D, fldPos(D, I2)) = I1
            gameD0(D) = gameD0(D) + 1
            X0%(D, 0, gameD0(D)) = lineupPlayer(D, I2)
            X0%(D, 1, gameD0(D)) = fldPos(D, I2)
            X0%(D, 2, gameD0(D)) = (I2 * 10) + (gameD0(D) - 9)

            W5 = D
            W6 = D
            VV(D) = I2
            VV1(D) = I2

            For xx = 22 To 25                               'Iterate over 4 possible positions
                If fldPos(D, I2) = batRating(D, I1, xx) Then   'Position match?
                    batRating(D, I1, SS_B_Arm)    = batRating(D, I1, 33 + (xx - 22) * 2)
                    batRating(D, I1, SS_B_Range)  = batRating(D, I1, 34 + (xx - 22) * 2)
                    batRating(D, I1, SS_B_FldPct) = batRating(D, I1, xx + 4)
                    Exit For
                End If
            Next

            '14955  ---> this is it's own routine used often
            Call HITTINGLINES

            If useVGA = 1 Then
                Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
            Else
                Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
            End If

            VV(D) = 1
            VV1(D) = 9

        End If

    Next

    A1 = gameScore(D, 0) - gameScore(P, 0)

    If Not (teamMgrProfileVal(D, 560) <> 999 Or INNING% < 8 Or playerMode = 0 Or playerMode = 1 And P = compTeam) Then

        For IJ0% = 1 To 4

            If teamMgrProfileVal(D, 560 + IJ0%) <> -1 And batRating(D, teamMgrProfileVal(D, 564 + IJ0%), SS_B_Avail) = 0 Then

                Call Rolld100(RN)

                If fielder(D, teamMgrProfileVal(D, 576 + IJ0%)) = teamMgrProfileVal(D, 560 + IJ0%) And RN <= teamMgrProfileVal(D, 568 + IJ0%) And A1 >= teamMgrProfileVal(D, 572 + IJ0%) Then

                    I1 = teamMgrProfileVal(D, 564 + IJ0%)

                    For I2 = 1 To 9
                        If fldPos(D, I2) = teamMgrProfileVal(D, 576 + IJ0%) Then Exit For
                    Next

                    batRating(D, I1, SS_B_Avail) = 1
                    lineupPlayer(D, I2) = I1
                    fielder(D, fldPos(D, I2)) = I1

                    gameD0(D) = gameD0(D) + 1
                    X0%(D, 0, gameD0(D)) = lineupPlayer(D, I2)
                    X0%(D, 1, gameD0(D)) = fldPos(D, I2)
                    X0%(D, 2, gameD0(D)) = (I2 * 10) + (gameD0(D) - 9)

                    W5 = D
                    W6 = D
                    VV(D) = I2
                    VV1(D) = I2

                    For xx = 22 To 25
                        If fldPos(D, I2) = batRating(D, I1, xx) Then
                            batRating(D, I1, SS_B_Arm)    = batRating(D, I1, 33 + (xx - 22) * 2)
                            batRating(D, I1, SS_B_Range)  = batRating(D, I1, 34 + (xx - 22) * 2)
                            batRating(D, I1, SS_B_FldPct) = batRating(D, I1, xx + 4)
                            Exit For
                        End If
                    Next

                    Call HITTINGLINES

                    If useVGA = 1 Then
                        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
                    Else
                        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
                    End If

                    VV(D) = 1
                    VV1(D) = 9

                End If

            End If

        Next

    End If

    If Environ$("PBPLOG") = "TRUE" Then Print #11, "::: Status of pitcher taking the mound: "; fielder(D, 1)

    If fielder(D, 1) = 99 Then
        If playerMode = 0 Or fielder(D, 1) = 99 And playerMode = 1 And compTeam = 1 - D Then
        '----- move stuff here -----
        GoTo LStayInGame
        Else
            Call RELIEVERS(PPH, pz) 'Will choose a new pitcher because pitcher was pinch hit for
        End If        
    End If

   LNextBatter0:
    SH = 0
    errorOnPlay% = 0
    SO = 0
    currLineupSlot(P) = currLineupSlot(P) + 1
    INFPOS% = INF_BACK
    nbrStrikes = 0

    If currLineupSlot(P) = 10 Then currLineupSlot(P) = 1

    '--- Start of a new sub
   LNextBatter1:
    Call WHOSON1ST(baseRunners)
    Call PLAYERNAME(P)

    If Environ$("PBPLOG") = "TRUE" Then Call DumpIt(baseRunners, P2, D2)

    If useVGA = 1 Then
        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
    Else
        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
    End If

    P2 = 0
    D2 = 0

    ' Game over - there's a winner
    If INNING% >= 9 And P = 1 And gameScore(P, 0) - gameScore(D, 0) > 0 Then
        GoTo LGameOver
    End If

   LContinueBatter:

    Do

        If useVGA = 1 Then
            _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
        Else
            Color 14, 0
        End If

        I2 = INNING%

        I0 = P

        'Move cursor to current inning if score > 0
        If inningScore(P, INNING%) > 0 Then
            If useVGA = 1 Then
                Call INNDISP_G(I2, I0)
            Else
                Call INNDISP(I2, I0)
            End If
        End If

        'Print total score
        If useVGA = 1 Then

            FORMATTED$ = FUsing$(Str$(gameScore(I0, 0)), "##", 1)
            PrintOnGfx 53, I0 + 28, FORMATTED$, imageScreen&

            FORMATTED$ = FUsing$(Str$(gameScore(D, 2)), "##", 1)
            'PrintOnGfx 59, 1 - I0 + 28, FORMATTED$, imageScreen&
            PrintOnGfx 59, D + 28, FORMATTED$, imageScreen&

        Else

            Locate I0 + 2, 40
            Print Using "##"; gameScore(I0, 0);
            Locate D + 2, 46
            Print Using "##"; gameScore(D, 2);
            Color 15, 0

        End If

        runnerOn2nd = b2Base(2)
        errorOnPlay% = 0

       LContinueBatter2:
        If (playerMode = 2 Or playerMode = 1 And P <> compTeam) Then
            GoTo LEndOfGame
        End If

        If baseRunners = 1 Or baseRunners = 5 Then
            runnerHeld = 1
            If useVGA = 1 Then
                Call HOLDSTATUS_G(runnerHeld)
            Else
                Call HOLDSTATUS(runnerHeld)
            End If
        End If

        '----------------------------------------
        ' DEFENSE OPTIONS MENU
        '----------------------------------------
       LDefenseOptions:
        pbpString$ = PadRight$(Left$(gameTeams$(D), 3) + teamAbbreviatons$(D), 8)

        'Call WINDEX

        If useVGA = 1 Then

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color &HF0000000

            PrintOnGfx 1, 19, pbpString$, imageScreen&

            F1$(1) = "1-PITCH       "
            F1$(2) = "2-INT WALK    "
            F1$(3) = "3-INF IN      "
            F1$(4) = "4-CORNERS IN  "
            F1$(5) = "5-INF NORMAL  "
            F1$(6) = "6-PERSONNEL   "
            F1$(7) = "8-PITCH AROUND"
            F1$(8) = "G-GUARD LINES "
            F1$(9) = "H-HOLD RUNNER "

            '-- B/P do not do anything
            '-- but could be programmed to
            '-- Presumably they are for
            '-- Batter / Pitcher stats
            F1$(10) = "S/D/B/P STATS "

            For I = 21 To 30
                PrintOnGfx 1, I - 1, F1$(I - 20), imageScreen&
            Next

        Else

            Color 14, 0: Print pbpString$

            Print " 1-PITCH          D-DETAILED STATS      "
            Print " 2-INTENTIONAL BB S-BATTER/PITCHER STATS"
            Print " 3-INFIELD IN     H-HOLD RUNNER"
            Print " 4-CORNERS IN     Q-QUIT GAME"
            Print " 5-INFIELD NORMAL W-WEATHER CONDITIONS"
            Print " 6-PERSONNEL      G-GUARD LINES"
            Print " 7-PITCH/HIT AWAY O-GAME OPTIONS"
            Print " 8-PITCH AROUND"
            Color 15, 0

        End If

        I$ = GetKeyPress$

        'Restore image where commands went
        If useVGA = 1 Then _PutImage (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176), backupScreen&, imageScreen&, (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176)

        If UCase$(I$) = "D" Then
            Call VIEWTEAM
            GoTo 15580
        End If

        If UCase$(I$) = "G" Then

            INFPOS% = INF_GUARD_LINES

            If useVGA = 1 Then
                pbpString$ = "INF:GUARD LINE"
                _PutImage (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16), backupScreen&, imageScreen&, (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16)

                'Printing black on background
                _PrintMode _KeepBackground , imageScreen&
                Color &HF0000000

                PrintOnGfx 52, 26, pbpString$, imageScreen&

                'Printing white on black
                Color &HFFFFFFFF
                _PrintMode _FillBackground , imageScreen&
            Else
                Color 15, 2
                Locate 23, 46: Print "GUARD LINE"
            End If

            GoTo LContinueBatter

        End If

        If UCase$(I$) = "L" Then
            Call PopUp_Lineup
            GoTo 15580
        End If

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 15580
        End If

        If UCase$(I$) = Chr$(17) Then
            If useVGA = 0 Then Cls
            Call EndOfGame
            Exit Sub
        End If

        If UCase$(I$) = "S" Then
            Call POPUPSTATS
            GoTo 15580
        End If

        If UCase$(I$) = "B" Then
            If useVGA = 1 Then Screen menu&
            Call PopUp_Batter(D, currPitcher(D))
            GoTo 15580
        End If

        If UCase$(I$) = "P" Then
            If useVGA = 1 Then Screen menu&
            Call PopUp_Pitcher(P, currPitcher(P))
            GoTo 15580
        End If

        If UCase$(I$) = "W" Then
            Call WEATHER
            GoTo 15580
        End If

        If UCase$(I$) = "H" Then

            runnerHeld = 1 - runnerHeld

            If useVGA = 1 Then
                Call HOLDSTATUS_G(runnerHeld)
            Else
                Call HOLDSTATUS(runnerHeld)
            End If

            GoTo LDefenseOptions

        End If

        If Val(I$) = 7 Then
            P2 = OFF_HIT_AWAY
            D2 = DEF_PITCH

            GoTo LWeirdPlay

        End If

        D2 = Val(I$)

    Loop Until D2 >= 1 And D2 <= 9


   LManageDefense:

    Select Case D2
        Case 1
            '

        Case 2
            Call WINDEX
            Call BASEONBALLS(S2%, D2, D, P)
            GoTo LHBP

        Case 3
            INFPOS% = INF_IN
            infStatus$ = "INF:INFIELD IN"

        Case 4
            INFPOS% = INF_CORNERS_IN
            infStatus$ = "INF:CORNERS IN"

        Case 5
            INFPOS% = INF_BACK
            infStatus$ = "INF:NORMAL    "

        Case 6
            P9 = D
            W5 = -1
            W6 = -1
            GoTo LPersonnelMenu

    End Select

    'We've assigned the text,
    'now we have to print it on the screen
    If D2 >= 3 And D2 <= 5 Then

        If useVGA = 1 Then
            'Restore image where options go
            _PutImage (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16), backupScreen&, imageScreen&, (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16)

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color &HF0000000

            'Print infield status
            PrintOnGfx 52, 26, infStatus$, imageScreen&

            'Printing white on black
            Color &HFFFFFFFF
            _PrintMode _FillBackground , imageScreen&
        Else
            Color 15, 2
            Locate 23, 46: Print infStatus$
        End If

        GoTo LContinueBatter

    End If

    '----------------------------------------
    ' OFFENSE OPTIONS MENU
    '----------------------------------------
   LOffenseOptions:
    Do

        If (playerMode = 2 Or playerMode = 1 And P = compTeam) Then

            JM = lineupPlayer(P, currLineupSlot(P))

            If PPH <> 1 Then

                A1 = gameScore(P, 0) - gameScore(D, 0)

                If fldPos(P, currLineupSlot(P)) <> 1 Then

                    If teamMgrProfileVal(P, 847) = 999 Then

                        'PH FOR POS PLAYER CASES
                        Call Rolld100(RN)
                        MOB% = 0

                        For I = 1 To 3
                            If b2Base(I) > 0 Then MOB% = MOB% + 1
                        Next

                        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                            I7! = 0
                            I6! = 0
                        Else
                            I7! = CInt( batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                            I6! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                        End If

                        For I = 0 To 2
                            If INNING% >= teamMgrProfileVal(P, 848 + I * 6) And A1 <= teamMgrProfileVal(P, 849 + I * 6) * -1 And I6! <= teamMgrProfileVal(P, 851 + I * 6) And I7! <= teamMgrProfileVal(P, 852 + I * 6) And RN <= teamMgrProfileVal(P, 853 + I * 6) And MOB% >= teamMgrProfileVal(P, 850 + I * 6) Then
                                GoTo LPhForPosition
                            End If
                        Next

                    Else

                        If batRating(P, JM, SS_B_AB) <> 0 Then
                            GoTo LPhForPosition
                        Else

                            If Not (batRating(P, JM, SS_B_H) / batRating(P, JM, SS_B_AB) * 1000 > 233 Or batRating(P, JM, SS_B_HR) >= 15) Then
                                If INNING% > 6 And A1 < -2 Or A1 <= 0 And INNING% >= 8 Or A1 = 0 And INNING% >= 9 Then Goto LPhForPosition
                            End If

                        End If

                    End If

                Else    ' batter is the pitcher

                    For I = 0 To 21

                        If pitchRating(P, I, SS_P_Avail) = 0 Then

                            If teamMgrProfileVal(P, 590) = 999 Then

                                'PH FOR PITCHER WITH PROFILE
                                Call Rolld100(RN)

                                MOB% = 0

                                For indx = 1 To 3
                                    If b2Base(indx) > 0 Then MOB% = MOB% + 1
                                Next

                                If pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then
                                    I7! = pitchRating(I0, currPitcher(I0), SS_P_BAvg)
                                Else
                                    If pitchRating(P, currPitcher(P), SS_P_H_AB) = 0 Then
                                        I7! = 0
                                    Else
                                        I7! = CInt((pitchRating(P, currPitcher(P), SS_P_H_H) / pitchRating(P, currPitcher(P), SS_P_H_AB) * 1000))
                                    End If
                                End If

                                For indx = 0 To 2
                                    If INNING% >= teamMgrProfileVal(P, 591 + indx * 6) And A1 <= teamMgrProfileVal(P, 592 + indx * 6) * -1 And nbrOuts(0) <= teamMgrProfileVal(P, 594 + indx * 6) And I7! <= teamMgrProfileVal(P, 595 + indx * 6) And RN <= teamMgrProfileVal(P, 596 + indx * 6) And MOB% >= teamMgrProfileVal(P, 593 + indx * 6) Then
                                        GoTo LPhForPitcher
                                    End If
                                Next

                                Exit For

                            Else

                                If INNING% > 4 And INNING% < 7 And A1 < -2 And gameScore(P, 0) > 0 Then Goto LPhForPitcher
                                If INNING% > 4 And INNING% < 7 And A1 < -3 Then Goto LPhForPitcher
                                If INNING% > 6 And A1 < 0 Or A1 = 0 And INNING% > 6 And INNING% < 9 And baseRunners > 1 Then Goto LPhForPitcher
                                If A1 = 0 And INNING% >= 9 And baseRunners > 0 Or A1 = 0 And INNING% >= 9 And nbrOuts(0) = 0 Then Goto LPhForPitcher
                                If pitchRating(P, currPitcher(P), SS_P_IP) / pitchRating(P, currPitcher(P), SS_P_G) <= 2 And gamePitStats(P, currPitcher(P), 0) >= 6 Then Goto LPhForPitcher

                                Exit For

                            End If

                        End If

                    Next

                End If

            End If

            GoTo LPinchRunner

        Else

            Call WINDEX

            _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144)

            pbpString$ = PadRight$(Left$(gameTeams$(P), 3) + teamAbbreviatons$(P), 8)

            If useVGA = 1 Then
                'Printing black on background
                _PrintMode _KeepBackground , imageScreen&
                Color &HF0000000

                PrintOnGfx 1, 19, pbpString$, imageScreen&

                F1$(1) = "1-HIT AWAY    "
                F1$(2) = "2-HIT AND RUN "
                F1$(3) = "3-STEAL       "
                F1$(4) = "4-BUNT        "
                F1$(5) = "5-PERSONNEL   "
                F1$(6) = "O-GAME OPTIONS"
                F1$(7) = "D/S/P/B STATS "
                F1$(8) = "W-WEATHER COND"
                F1$(9) = "L-SHOW LINEUPS"
                F1$(10) = "Q-QUIT GAME   "

                For I = 21 To 30
                    PrintOnGfx 1, I - 1, F1$(I - 20), imageScreen&
                Next

                'Printing white on black
                Color &HFFFFFFFF
                _PrintMode _FillBackground , imageScreen&

            Else
                Color 14, 0
                Print gameTeams$(P); " SELECT:"
                Print " 1- HIT AWAY      D-DETAILED STATS"
                Print " 2- HIT AND RUN   S-BATTER/PITCHER STATS"
                Print " 3- STEAL         L-DISPLAY LINEUPS"
                Print " 4- BUNT          Q-QUIT GAME"
                Print " 5- PERSONNEL     W-WEATHER CONDITIONS"
                Color 15, 0

            End If

        End If

        I$ = GetKeyPress$

        If useVGA = 1 Then
            _PutImage (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176), backupScreen&, imageScreen&, (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176)
        End If

        If UCase$(I$) = "D" Then
            Call VIEWTEAM
            GoTo 15580
        End If

        If UCase$(I$) = "L" Then
            Call PopUp_Lineup
            GoTo 15580
        End If

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 15580
        End If

        If UCase$(I$) = Chr$(17) Then
            If useVGA = 0 Then Cls
            Call EndOfGame
            Exit Sub
        End If

        If UCase$(I$) = "S" Then
            Call POPUPSTATS
            GoTo 15580
        End If

        If UCase$(I$) = "W" Then
            Call WEATHER
            GoTo 15580
        End If

        P2 = Val(I$)

    Loop Until P2 >= OFF_HIT_AWAY And P2 <= OFF_PERSONNEL

    15064 '
    Select Case P2

        Case 1, 4   'Hit away, Bunt

            If D2 <> 8 Then Goto LWeirdPlay

            Call WINDEX

            Call Rolld100(RN)

            If P2 = OFF_BUNT Or (P2 = OFF_HIT_AWAY And RN <= 50) Then Goto LWeirdPlay

            Call BASEONBALLS(S2%, D2, D, P)

            GoTo LHBP

        Case 2  'Hit and run
            If nbrOuts(0) = 2 Or (baseRunners <> 1 And baseRunners <> 4 And baseRunners <> 5) Then Goto LNextBatter1

        Case 3  'Steal
            GoTo LSteal

        Case 5  'Personnel
            P9 = P
            W5 = -1
            W6 = -1
            GoTo LPersonnelMenu

    End Select

   LHitAndRun:
    '-- this is related to the PICKEDOFF routine and it's immediate aftermath
    If baseRunners = 4 Then pickedOffBase = 3 Else pickedOffBase = 2

    Call Rolld100(I3)

    If I3 < 6 Then

        Call PICKEDOFF(D, pickedOffBase)

        PO = 1
        A5%(pickedOffBase - 1) = 10
        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        currFielder = 1

        Call CREDITASSIST(currFielder, D)
        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)   'fielder making putout is returned in I3
        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

        If nbrOuts(0) >= 3 Then

            currLineupSlot(P) = currLineupSlot(P) - 1
            PO = 0
            GoTo LCheckForOptionsOrQuit

        End If

    End If

    If PO = 1 Then PO = 0: GoTo LNextBatter1

    Call WINDEX

    I3 = Int(Rnd(1) * 1000) + 1
    I9 = Int(Rnd(1) * 1000) + 1

    If I3 > 350 Then
        Call PBP("Runner(s) going")
    Else
        Call PBP("Count not favorable...hit away")
        P2 = OFF_HIT_AWAY
    End If

    Call DELAY

    If I3 <= 350 Then Goto LWeirdPlay

    If I9 <= (game_batRating(P, GM_K, currLineupSlot(P)) * 2 * .33) Then

        nbrStrikes = nbrStrikes + 1
        Call PBP(player$(0) + " swings and misses")
        Call DELAY

        If useVGA = 1 Then
            PrintOnGfx 63, 28, Str$(nbrStrikes), imageScreen&
        Else
            Locate 2, 58: Print nbrStrikes
        End If

        If nbrStrikes = 3 Then
            GoTo LStrikeout
        Else
            GoTo LSteal
        End If
    End If

    If I9 <= game_batRating(P, GM_K, currLineupSlot(P)) * 2 Then
        Call PBP(player$(0) + " fouls off the pitch...hit away")
        Call DELAY
        P2 = OFF_HIT_AWAY
    End If

    GoTo LWeirdPlay


    '--- end of a new sub

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    '----- NOODLE -----


   LRarePlays: ' BRANCH FOR RARE PLAYS
    Call RAREPLAYS(H6%, H7%, IP%)

    If IP% = 1 Then
        IP% = 0
        Call RELIEVERS(PPH, pz)
    End If

    '-- It appears the following lines get executed
    '-- almost no matter what value H6% is.

    '   "Call HITTING LINES...
    '   If useVGA = 1...

    '-- In original code this was a short sub
    '-- I'm not sure why it can't be run outside
    '-- of the If/Then statement.
    '-- That would simply leave the If/Then code
    '-- to determine where to GoTo

    '1 to 4
    '6 to 8
    '10 to 24
    If H6% <= 4 Or H6% > 5 And H6% < 9 Or H6% >= 10 And H6% <= 24 Then

        Call HITTINGLINES

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        GoTo LContinueBatter

    End If

    '5
    '25 to 29
    If H6% = 5 Or H6% >= 25 And H6% <= 29 Then

        Call HITTINGLINES

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        GoTo LCheckForOptionsOrQuit

    End If

    '9
    If H6% = 9 Then

        Call HITTINGLINES

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        '13240 / *** base on balls ***
        Call BASEONBALLS(S2%, D2, D, P)
        GoTo LHBP

    End If

    '45
    If H6% = 45 Then

        Call HITTINGLINES

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        GoTo LHBP

    End If

    '30 to 44
    '46+ ???
    Call HITTINGLINES

    If useVGA = 1 Then
        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
    Else
        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
    End If

    GoTo LContinueBatter

    '----- NOODLE -----


   LWeirdPlay:
    Call Rolld100(RN)

    If (((precipType = 1 Or precipType = 2 Or precipType = 3) And RN = 2) Or (rainDelay = 1 And RN <= 5)) And INNING% >= 2 And gameDelayed = 0 Then
        H7% = 20
        H6% = 21
        GoTo LRarePlays
    End If

    H6% = Int(Rnd(1) * 1000) + 1
    If H6% <= 3 And useInj = 1 Then
        GoTo LRarePlays
    End If

    SN = 0
    H6% = Int(Rnd(1) * 1000) + 1
    If nbrOuts(0) = 0 And baseRunners = 4 And INFPOS% <  INF_IN And H6% = 768 Or _
       nbrOuts(0) = 0 And baseRunners = 4 And INFPOS% >= INF_IN And H6% = 123 Or _
       nbrOuts(0) = 0 And baseRunners = 7 And INFPOS% >= INF_IN And H6% = 245 Or _
       nbrOuts(0) = 0 And baseRunners = 7 And INFPOS% <  INF_IN And H6% = 381 Then
        Call TRIPLEPLAY(D)
        GoTo LCheckForOptionsOrQuit
    End If

    H7% = Int(Rnd(1) * 7500) + 1

    Call WINDEX

    If H6% <= 4 Then
        ' *** hit by pitch ***
        Call HITBYPITCH(S2%, D)

       LHBP:
        A5%(0) = 1  'batter advances to 1st base
        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 0

        If b2Base(1) > 0 Then           '1st, 1st & 2nd, or bases loaded
            A5%(1) = 1
            If b2Base(2) > 0 Then       '1st & 2nd or bases loaded
                A5%(2) = 1
                If b2Base(3) > 0 Then   'bases loaded
                    A5%(3) = 1
                    If currFielder = 2 And S2% > 2 Then A5%(3) = 11
                End If
            End If
        End If

        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
        GoTo LNextBatter0

    End If

    If H6% <= 6 Then

        ' *** balk ***
        If baseRunners = 0 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo LContinueBatter
        Else
            S2% = EVENT_BALK

            Call WINDEX

            Call Rolld100(RN)
            rndUmp = Int(Rnd(1) * 4) + 1

            Select Case RN
                Case Is <= 33
                    pbpString$ = "a balk has been called"
                Case 34 To 66
                    Call PBP("and a BALK is called")
                    pbpString$ = pitchers$(D, currPitcher(D)) + " dropped the ball"
                Case 67 To 100
                    pbpString$ = gameUmpire$(rndUmp) + " calls a balk on " + pitchers$(D, currPitcher(D))
            End Select

            Call PBP(pbpString$)

            '13223
            If b2Base(1) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) > 1 And RN = 9 And SO = 1 Then
                A5%(0) = 1
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                GoTo LCheckForOptionsOrQuit
            Else
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
                A5%(0) = 0
                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                GoTo LNextBatter1
            End If

        End If

    End If

    '7 to 11 OR 7 to 200 And ...
    If H6% <= 11 Or H6% <= 200 And batRating(D, fielder(D, 2), SS_B_Pos1) <> 2 And batRating(D, fielder(D, 2), SS_B_Pos2) <> 2 And batRating(D, fielder(D, 2), SS_B_Pos3) <> 2 And batRating(D, fielder(D, 2), SS_B_Pos4) <> 2 Then

       LPassedBall: ' *** passed ball ***
        If baseRunners = 0 And SO <> 1 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo LContinueBatter
        End If

        S2% = EVENT_PB
        Call WINDEX
        Call DELIVERY(D)

        Call Rolld100(RN2)
        If RN2 <= 50 Then
            Call PBP("pitch gets by " + batters$(D, fielder(D, 2)))
        Else
            'RN > 50
            Call PBP("off " + batters$(D, fielder(D, 2)) + "'s glove")
            Call PBP("it gets away from him")
        End If

        Call PBP("It's ruled a passed ball")

        If b2Base(1) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) > 1 And RN = 9 And SO = 1 Then
            nbrOuts(1) = nbrOuts(1) + 1     'nbrOuts(1) tracks how many outs there would be without errors (for recording unearned runs)
        End If

        13222 '
        RN3 = Int(Rnd(1) * 10) + 1

        If baseRunners = 1 And P2 = OFF_HIT_N_RUN And RN3 < batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run) Or _
           baseRunners = 5 And P2 = OFF_HIT_N_RUN And RN3 < batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run) Then
            A5%(1) = 2
            A5%(3) = 1
            A5%(0) = 0
            A5%(2) = 2
            Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
            GoTo LNextBatter1
        End If

        13223 '
        If b2Base(1) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) > 1 And RN = 9 And SO = 1 Then
            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1
            Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
            GoTo LCheckForOptionsOrQuit
        Else
            A5%(0) = 0
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1
            Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
            GoTo LNextBatter1
        End If

    End If

    If H6% <= 19 Then

        '13230 / *** wild pitch ***
        If baseRunners = 0 And SO <> 1 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo LContinueBatter
        End If

        Call WILDPITCH(S2%, D)

        GoTo 13222

    End If

    If H7% = 27 Then

        S2% = EVENT_INTERFERENCE
        Call WINDEX
        Call PBP(batters$(D, fielder(D, 2)) + " is called for interference!!")
        Call PBP(player$(0) + " trots to first base")
        GoTo LHBP

    End If

    If H7% = 28 Then

        Call WINDEX
        Call PBP(player$(0) + " is out for obstruction!")

        currFielder = 2
        Call CREDITREGPO(currFielder, D)
        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1

        If fldPos(P, currLineupSlot(P)) <> 1 Then
            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Else
            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        End If


        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

        GoTo LCheckForOptionsOrQuit
    End If

    If H6% <= game_batRating(P, GM_BB, currLineupSlot(P)) + 25 Then 'ToDo [bm] I have 28 as the constant in an older version
        S2% = EVENT_INTERFERENCE
        '13240 / *** base on balls ***
        Call BASEONBALLS(S2%, D2, D, P)
        GoTo LHBP
    End If

    If P2 = OFF_BUNT Then Goto LBunt

    '3056
    H6% = Int(Rnd(1) * 1000) + 1
    H7% = Int(Rnd(1) * 1000) + 1
    I4 = 0

    If P2 = OFF_HIT_N_RUN Then I4 = -5

    If INFPOS% = INF_IN Then
        SN = 70
        I4 = I4 + 15
    End If

    If nbrStrikes = 2 Then
        I4 = I4 - 15
    End If

    If INFPOS% = INF_CORNERS_IN Then
        SN = 20
    End If

    If INNING% > 5 And S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) Then
        I4 = I4 + 40
    End If

    If P = 0 Then I4 = I4 - 2
    If P = 1 Then I4 = I4 + 2

    Call WINDEX

    If H7% <= SN Then
        Call PBP("It gets through the drawn-in infield!")
        Call DELAY
        GoTo 13063
    End If

    If H6% > game_batRating(P, GM_BAvg, currLineupSlot(P)) + I4 Then    'not a base hit

        '3600
        ' *** check for strikeout ***
        H6% = Int(Rnd(1) * 1000) + 1
        strikeoutRating = game_batRating(P, GM_K, currLineupSlot(P))

        If H6% > game_batRating(P, GM_K, currLineupSlot(P)) Then

            ' *** ground ball for out ***
            Call Rolld100(H6%)

            playerGndPct  = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_GndPct) + pitchRating(D, currPitcher(D), SS_P_GndPct) - 52
            pitcherGndPct = 23 + pitchRating(D, currPitcher(D), SS_P_GndPct)    'Assumption seems to be that pitchers hit a lot more grounders
            ballHitInAir = 0

            If H6% > playerGndPct  And fldPos(P, currLineupSlot(P)) <> 1 Then ballHitInAir = 1
            If H6% > pitcherGndPct And fldPos(P, currLineupSlot(P))  = 1 Then ballHitInAir = 1

            If ballHitInAir = 1 Then

                'balls hit to the air
                Call Rolld100(RN)
                A5%(0) = 0
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0

                Select Case RN

                    Case 1 To 73
                        ' *** FLY  OUTS ***
                        Call GETOUTFIELDER(currFielder, P, I1, D)

                        If fldPos(P, currLineupSlot(P)) <> 1 Then
                            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        Else
                            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        End If

                        Call Rolld100(RN)

                        Select Case RN

                            Case 1 To 17
                                If RN <= 17 Then
                                    ' *** shallow fly ball ***
                                    S2% = EVENT_FLY_SHALLOW
                                    Call WINDEX

                                    X$ = fieldPos$(currFielder)
                                    If Int(Rnd(1) * 100) <= 25 And currFielder = 7 Then X$ = "lcf"
                                    If Int(Rnd(1) * 100) <= 25 And currFielder = 9 Then X$ = "rcf"

                                    Call DELIVERY(D)

                                    'What is this used for?
                                    Call Rolld100(RN)

                                    Call GetPBPString(60, 1170, Y$, D)
                                    Call PBP(Y$ + " " + X$)

                                    Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                                    
                                    If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit
                                    GoTo 14025

                                End If

                            Case 18 To 33
                                ' *** deep fly out ***
                                S2% = EVENT_FLY_DEEP

                                Call WINDEX
                                Call LONGFLY(D, currFielder)

                                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)

                                If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

                                Call CREDITREGPO(currFielder, D)
                                Call CAUGHTFLY(RN, S2%, D)

                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1

                                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                                If nbrOuts(0) = 3 Then Goto LCheckForOptionsOrQuit

                                If b2Base(3) <> 0 Then
                                    A5%(3) = 1  'sac fly
                                    Call ADDPIT(D, currPitcher(D), 16)

                                    If fldPos(P, currLineupSlot(P)) = 1 Then
                                        Call PITOFF(P, currPitcher(P), LG_PB_AB)
                                        Call INCPITOFF(P, currPitcher(P), LG_PB_SF)
                                    Else

                                        Call BATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_SF)

                                    End If

                                End If

                                14066 '
                                If b2Base(2) <> 0 Then

                                    speedRating = batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run)
                                    fldArm = (batRating(D, fielder(D, currFielder), SS_B_Arm) - 5) * 25
                                    offset = (speedRating - 5) * 25 - fldArm

                                    RN = Int(Rnd(1) * 1000) + 1

                                    '-- These are ALL conditions for assigning the same value
                                    If currFielder = 9 And S2% = EVENT_FLY_DEEP    And RN <= 800 + offset And P2 <> OFF_HIT_N_RUN Or _
                                       currFielder = 9 And S2% = EVENT_FLY_REGULAR And RN <= 400 + offset And P2 <> OFF_HIT_N_RUN Or _
                                       currFielder = 8 And S2% = EVENT_FLY_DEEP    And RN <= 700 + offset And P2 <> OFF_HIT_N_RUN Or _
                                       currFielder = 8 And S2% = EVENT_FLY_REGULAR And RN <= 350 + offset And P2 <> OFF_HIT_N_RUN Or _
                                       currFielder = 7 And S2% = EVENT_FLY_DEEP    And RN <= 250 + offset And P2 <> OFF_HIT_N_RUN Then A5%(2) = 1

                                    If P2 = OFF_HIT_N_RUN And speedRating > 6 And currFielder = 9 And S2% = EVENT_FLY_DEEP And RN <= 400 + offset Or _
                                       P2 = OFF_HIT_N_RUN And speedRating > 6 And currFielder = 8 And S2% = EVENT_FLY_DEEP And RN <= 400 + offset Then A5%(2) = 1

                                    If A5%(2) <> 1 And S2% = EVENT_FLY_REGULAR And P2 <> OFF_HIT_N_RUN And baseRunners < 6 Then

                                        If (currFielder = 9 And RN > 800) Or (currFielder = 8 And RN > 840) Then
                                            baseNbr! = 3
                                            If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try for 3rd on regular fly to CF or RF..."
                                            If errorOnPlay% = 0 Then Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)
                                        End If

                                        If currFielder = 7 And P2 <> OFF_HIT_N_RUN And baseRunners < 6 And RN > 880 Then
                                            baseNbr! = 3
                                            If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try for 3rd on regular fly to LF..."
                                            If errorOnPlay% = 0 Then Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)
                                        End If

                                    End If

                                End If

                                If b2Base(1) <> 0 Then
                                    RN = Int(Rnd(1) * 1000) + 1
                                    A5%(1) = 0
                                    I3 = (batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run) - 5) * 25
                                    I4 = (batRating(D, fielder(D, currFielder), SS_B_Arm) - 5) * 25
                                    J5 = I3 - I4

                                    If S2% = EVENT_FLY_DEEP And P2 <> OFF_HIT_N_RUN And baseRunners = 1 And RN <= 100 + J5 Then
                                        baseNbr! = 2
                                        If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try for 2nd on deep fly..."
                                        If errorOnPlay% = 0 Then Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)
                                    End If
                                    ' I8! = 2
                                    ' LN% = 3308
                                End If

                                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                                GoTo LCheckForOptionsOrQuit

                            Case 34 To 100
                                ' *** regular fly out ***
                                S2% = EVENT_FLY_REGULAR
                                Call WINDEX
                                X$ = fieldPos$(currFielder)
                                If Int(Rnd(1) * 100) <= 25 And currFielder = 7 Then X$ = "lcf"
                                If Int(Rnd(1) * 100) <= 25 And currFielder = 9 Then X$ = "rcf"

                                Call DELIVERY(D)
                                Call Rolld100(RN)

                                range = 90
                                min = 1070
                                If (currFielder = 7 Or currFielder = 9) And RN <= 10 Then X$ = "": range = 10: min = 1160

                                Call GetPBPString(range, min, Y$, D)
                                Call PBP(Y$ + " " + X$)

                                Call FLDERR(errorOnPlay%, currFielder, D, I3, SB%, S2%, P)

                                If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

                                '--- where does this end?
                                '--- can we turn into subroutine?
                                14025 '
                                Call CREDITREGPO(currFielder, D)
                                Call CAUGHTFLY(RN, S2%, D)

                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1

                                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                                If nbrOuts(0) = 3 Then Goto LCheckForOptionsOrQuit

                                Call Rolld100(RN)

                                If S2% = EVENT_FLY_SHALLOW And baseRunners = 1 And RN <= 33 And P2 = OFF_HIT_N_RUN Or _
                                   S2% = EVENT_FLY_SHALLOW And baseRunners = 5 And RN <= 33 Then   'Double Play
                                    A5%(1) = 10
                                    nbrOuts(0) = nbrOuts(0) + 1
                                    nbrOuts(1) = nbrOuts(1) + 1

                                    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                                    D3%(D) = D3%(D) + 1
                                    DP%(D) = DP%(D) + 1

                                    Call CREDITASSIST(currFielder, D)

                                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                                    GoTo LCheckForOptionsOrQuit

                                End If

                                If b2Base(3) > 0 Then
                                    baseNbr! = 4
                                    If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try to score..."
                                    If errorOnPlay% = 0 Then Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)
                                End If

                                If I9 = 1 Then
                                    I9 = 0
                                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                                    GoTo LCheckForOptionsOrQuit
                                Else
                                    GoTo 14066
                                End If

                        End Select

                    Case 74 To 89

                        ' infield popup
                        Call GETINFIELDER(currFielder, P, I1, D, S2%, P2)
                        S2% = EVENT_INF_POPUP
                        Call WINDEX
                        Call Rolld100(RN0)

                        If currFielder = 1 Then currFielder = 2: player$(4) = batters$(D, fielder(D, currFielder))
                        If (currFielder = 2 Or currFielder = 5 Or currFielder = 3) And RN0 <= 30 Then fo% = 1

                        If fo% = 0 And (baseRunners = 4 Or baseRunners = 7) And nbrOuts(0) < 2 Then Call PBP("INFIELD FLY RULE CALLED")
                        If fo% = 1 And currFielder = 2 Then range = 24: min = 180
                        If fo% = 1 And (currFielder = 3 Or currFielder = 4) Then range = 31: min = 210
                        If fo% = 1 And (currFielder = 5 Or currFielder = 6) Then range = 31: min = 250
                        If fo% <> 1 And currFielder = 2 Then range = 50: min = 400
                        If fo% <> 1 And currFielder = 3 Then range = 60: min = 450
                        If fo% <> 1 And currFielder = 5 Then range = 60: min = 510
                        If fo% <> 1 And currFielder = 6 Then range = 60: min = 570
                        If fo% <> 1 And currFielder = 4 Then range = 60: min = 630

                        Call GetPBPString(range, min, Y$, D)
                        Call PBP(Y$)

                        If fo% = 1 And currFielder = 2 Then range = 12: min = 321
                        If fo% = 1 And (currFielder = 3 Or currFielder = 4) Then range = 25: min = 340
                        If fo% = 1 And (currFielder = 5 Or currFielder = 6) Then range = 25: min = 370
                        If fo% <> 1 Then range = 25: min = 690

                        Call GetPBPString(range, min, Y$, D)
                        Call PBP(Y$)

                        If fldPos(P, currLineupSlot(P)) = 1 Then
                            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Else
                            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        End If

                        If baseRunners <> 4 Then

                            Call Rolld100(RN0)
                            If RN0 <= 25 And (teamRat_GAME(1, 13) = 11 Or teamRat_GAME(1, 13) = 8) Then Call PBP("fights the wind...")
                            If RN0 <= 15 And (teamRat_GAME(1, 13) = 2 Or teamRat_GAME(1, 13) = 4 Or teamRat_GAME(1, 13) = 13 Or teamRat_GAME(1, 13) = 24) Then Call PBP("fights the lights...")

                            If fo% <> 1 Then
                                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                                If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit
                            End If

                        End If

                        fo% = 0

                        Call PBP(player$(4) + " makes the catch")
                        Call CREDITREGPO(currFielder, D)

                        nbrOuts(0) = nbrOuts(0) + 1
                        nbrOuts(1) = nbrOuts(1) + 1

                        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                        GoTo LCheckForOptionsOrQuit

                    Case 90 To 100
                        S2% = EVENT_RUNNER_DBLD

                        Call GETINFIELDER(currFielder, P, I1, D, S2%, P2)
                        Call WINDEX
                        Call DELIVERY(D)

                        Call GetPBPString(42, 70, Y$, D)
                        Call PBP(Y$)

                        Call GetPBPString(34, 120, Y$, D)
                        Call PBP(Y$)

                        If fldPos(P, currLineupSlot(P)) = 1 Then
                            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Else
                            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        End If

                        Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
    
                        If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

                        nbrOuts(0) = nbrOuts(0) + 1
                        nbrOuts(1) = nbrOuts(1) + 1

                        Call CREDITREGPO(currFielder, D)

                        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                        If nbrOuts(0) = 3 Then Goto LCheckForOptionsOrQuit

                        Call Rolld100(RN)

                        If (baseRunners = 1 Or baseRunners = 5) And RN >= 80 Or (baseRunners = 1 Or baseRunners = 5) And P2 = OFF_HIT_N_RUN Then    'Double Play

                            'runner doubled off of 1st
                            A5%(1) = 10
                            nbrOuts(0) = nbrOuts(0) + 1
                            nbrOuts(1) = nbrOuts(1) + 1

                            CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                            D3%(D) = D3%(D) + 1
                            DP%(D) = DP%(D) + 1

                            If currFielder <> 3 Then
                                range = 10
                                min = 160
                            Else
                                range = 10
                                min = 170
                            End If

                            Call GetPBPString(range, min, Y$, D)
                            Call PBP(Y$)

                            I3 = 1
                            Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                            Call PBP(" --- DOUBLE PLAY ---")
                            If currFielder <> 3 Then Call CREDITASSIST(currFielder, D)


                        Else

                            If (baseRunners = 4 Or baseRunners = 2) And RN >= 80 Or baseRunners = 4 And P2 = OFF_HIT_N_RUN Then 'Double Play

                                'runner doubled off of 2nd
                                A5%(2) = 10
                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1

                                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                                D3%(D) = D3%(D) + 1
                                DP%(D) = DP%(D) + 1
                                I3 = 2

                                Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                                Call PBP(" --- DOUBLE PLAY ---")
                                Call CREDITASSIST(currFielder, D)

                            End If

                        End If

                        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                        GoTo LCheckForOptionsOrQuit

                End Select

            End If

            S2% = EVENT_GROUNDER
            Call GETINFIELDER(currFielder, P, I1, D, S2%, P2)
            Call WINDEX

            S2% = EVENT_GROUNDER
            Call DELIVERY(D)
            Call GROUNDERPBP(currFielder, D)

            If currFielder <> 2 Then
                If currFielder = 1 Then
                    pbpString$ = pbpString$ + "the pitcher"
                    Call PBP(pbpString$)
                Else
                    pbpString$ = pbpString$ + grounderDir$(currFielder)
                    Call PBP(pbpString$)
                End If
            End If

            Call Rolld100(RN)

            If (baseRunners = 1 Or baseRunners = 4 Or _
             ( (baseRunners = 7 Or baseRunners = 5) And INFPOS% < INF_IN) ) And nbrOuts(0) <= 1 And RN <= 50 Then
                DPC% = 1
                Call DPPBP
            End If

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
            Else
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
            End If

            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
      
            If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

            ' *** 2 outs, bases empty, runner 2nd, runner 3rd, or 2nd&3rd ***
            If nbrOuts(0) = 2 Or baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6 Then Goto LGrounderToPitcher

            H6% = Int(Rnd(1) * 1000) + 1
            E2% = ((teamRat_GAME(D, 5) - 80) * 4)
            E3% = ((5 - batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Run)) * 9)

            If (currFielder = 2 And H6% > 250 + E2% + E3%) Or (H6% > 490 + E2% + E3%) Or P2 = OFF_HIT_N_RUN Then Goto LGrounderToPitcher

            S2% = EVENT_GROUNDER

            If baseRunners = 4 Then

                If currFielder = 5 And Int(Rnd(1) * 3) + 1 = 1 Then
                    A5%(3) = 0
                    A5%(2) = 11
                    A5%(1) = 0
                    A5%(0) = 0
                    GoTo LProcessOutAndDP
                End If

                If currFielder = 2 Then
                    Call CREDITASSIST(currFielder, D)
                    A5%(3) = 0
                    A5%(2) = 11
                    A5%(1) = 0
                    A5%(0) = 0
                    GoTo LProcessOutAndDP
                End If

            End If

            If baseRunners = 7 And currFielder = 1 Then
                Call CREDITASSIST(currFielder, D)
                TB = 9
                currFielder = 2
                Call PBP("He throws home")
                A5%(3) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(0) = 0
                GoTo LProcessOutAndDP
            End If

            If baseRunners = 5 And gameScore(P, 0) - gameScore(D, 0) >= 0 And INNING% > 7 And nbrOuts(0) = 0 And INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3) Then GoTo LGrounderToPitcher

            If baseRunners = 7 And INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3) Then
                Call CREDITASSIST(currFielder, D)
                TB = 9
                currFielder = 2
                Call PBP("He fires it home")
                A5%(3) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(0) = 0
                GoTo LProcessOutAndDP
            End If

            If baseRunners = 7 And INFPOS% = INF_IN And currFielder = 4 Then
                Call CREDITASSIST(currFielder, D)
                TB = 9
                currFielder = 2
                Call PBP("He throws home for the force")
                A5%(3) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(0) = 0
                GoTo LProcessOutAndDP
            End If


            If baseRunners = 7 And INFPOS% = INF_IN And currFielder = 6 Then
                Call CREDITASSIST(currFielder, D)
                TB = 9
                currFielder = 2
                Call PBP("He goes home with it")
                A5%(3) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(0) = 0
                GoTo LProcessOutAndDP
            End If


            If currFielder <= 4 Then
                Call CREDITASSIST(currFielder, D)
                currFielder = 6
                Call PBP("He throws to " + batters$(D, fielder(D, 6)))
            Else
                Call CREDITASSIST(currFielder, D)
                currFielder = 4
                Call PBP("He throws to " + batters$(D, fielder(D, 4)))
            End If

            A5%(1) = 11
            A5%(2) = 0
            A5%(3) = 0
            A5%(0) = 0

            If baseRunners = 7 And (currFielder = 2 Or currFielder = 1) Then
                A5%(3) = 11
                A5%(2) = 1
                A5%(1) = 1
                A5%(0) = 1
            End If

            GoTo LProcessOutAndDP

        Else

            'Strikeout!
           LStrikeout:
            Call WINDEX
            'Call pbpLog(2994)
            Call STRIKEOUT(nbrStrikes, D)

            currFielder = 2
            Call CREDITREGPO(currFielder, D)

            nbrOuts(0) = nbrOuts(0) + 1
            nbrOuts(1) = nbrOuts(1) + 1
            SO = 1

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_K)
                If P2 = OFF_HIT_N_RUN Then nbrStrikes = 3
            End If

            CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
            Call ADDPIT(D, currPitcher(D), LG_P_K)

            If fldPos(P, currLineupSlot(P)) = 1 Then
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                Call INCPITOFF(P, currPitcher(P), LG_PB_K)
                If P2 = OFF_HIT_N_RUN Then nbrStrikes = 3
            End If

            If P2 = OFF_HIT_N_RUN And nbrOuts(0) < 3 Then
                SO = 7
                GoTo LSteal
            End If

            ' *** WILD PITCH OR PASSED BALL ON A STRIKEOUT
            RN = Int(Rnd(1) * 600) + 1
            If b2Base(1) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) = 3 And RN = 9 Then

                nbrOuts(0) = nbrOuts(0) - 1
                nbrOuts(1) = nbrOuts(1) - 1
                gamePitStats(D, currPitcher(D), 0) = gamePitStats(D, currPitcher(D), 0) - 1
                leagPitStats(D, currPitcher(D), 0) = leagPitStats(D, currPitcher(D), 0) - 1
                gameBatStats(D, fielder(D, 2), 16) = gameBatStats(D, fielder(D, 2), 16) - 1
                leagBatStats(D, fielder(D, 2), 16) = leagBatStats(D, fielder(D, 2), 16) - 1

                Call Rolld100(I1)

                If I1 < 77 Then

                    '13230 / *** wild pitch ***
                    If baseRunners = 0 And SO <> 1 Then
                        '13100 / *** fouled out of play ***
                        Call FOULBALL
                        GoTo LContinueBatter
                    Else
                        Call WILDPITCH(S2%, D)
                        GoTo 13222
                    End If
                Else
                    GoTo LPassedBall
                End If

            End If

            '-- this should be an Else statement above?
            GoTo LCheckForOptionsOrQuit

        End If

    Else    'base hit

        H6% = Int(Rnd(1) * 1000) + 1

        If H6% <= game_batRating(P, GM_2B, currLineupSlot(P))      And P2 <> OFF_HIT_N_RUN Or _
           H6% <= game_batRating(P, GM_2B, currLineupSlot(P)) * .5 And P2  = OFF_HIT_N_RUN Then

            S2% = EVENT_2B

            '3500
            ' *** double s2%=8 ***
            Call GETOUTFIELDER(currFielder, P, I1, D)

            Call ADDPIT(D, currPitcher(D), LG_P_H)
            Call WINDEX
            Call DELIVERY(D)

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
            Else
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
            End If

            If RN <= 40 Then

                If currFielder = 7 Then
                    Call GetPBPString(40, 2900, Y$, D)
                    Call PBP(Y$)
                End If

                If currFielder = 9 Then
                    Call GetPBPString(40, 2940, Y$, D)
                    Call PBP(Y$)
                End If

                If currFielder = 8 Then
                    Call GetPBPString(40, 2980, Y$, D)
                    Call PBP(Y$)
                End If

                Call GetPBPString(30, 3020, Y$, D)
                Call PBP(Y$)

            Else

                Call Rolld100(RN)

                Select Case RN

                    Case Is <= 25
                        Call LONGFLY(D, currFielder)
                        HW% = 1
                        Call WALLDOUBLE(HW%, currFielder)

                    Case 26 To 50
                        Call LDRIVE(X$)
                        Call PBP(player$(0) + " " + X$ + " one to the gap in " + OUTFLDLOCATION$(currFielder))

                    Case 51 To 60
                        Call PBP(player$(0) + " hits a sinking liner into " + OUTFLDLOCATION$(currFielder))
                        Call PBP("It goes to the wall")

                    Case 61 To 75
                        Call PBP(player$(0) + " lines one to the alley in " + OUTFLDLOCATION$(currFielder))

                    Case 76 To 85
                        If (currFielder = 7 Or currFielder = 9) Then
                            Call LDRIVE(X$)
                            Call PBP(player$(0) + " " + X$ + " one down the " + fieldPos$(currFielder) + " line")
                            Call PBP("It rolls into the corner")
                        End If

                        If currFielder = 8 Then
                            Call LDRIVE(X$)
                            Call PBP(player$(0) + " " + X$ + " one between the fielders")
                            Call PBP(player$(4) + " cuts it off...")
                        End If

                    Case 86 To 92
                        Call LDRIVE(X$)
                        Call PBP(player$(0) + " " + X$ + " a blue darter into " + fieldPos$(currFielder))

                    Case 93 To 100
                        Call PBP(player$(0) + " lines one to " + OUTFLDLOCATION$(currFielder))
                        Call PBP("It's down and to the wall!")

                End Select

            End If

            HW% = 0
            Call Rolld100(RN0)

            If RN0 <= 80 Then
                pbpString$ = "Double!!"
            Else
                pbpString$ = "2-Bagger!!"
            End If

            Call PBP(pbpString$)

            For I = 0 To 3:
                A5%(I) = 2
            Next I

            '-- wtf?
            A5%(3) = 1

            If b2Base(1) <> 0 Then

                Call Rolld100(H6%)
                BR% = batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run)

                If fldPos(P, b2Base(1)) = 1 Then BR% = pitchRating(P, currPitcher(P), SS_P_Run)

                I3 = 0

                If nbrOuts(0) = 0 And currFielder = 7 Then I3 = 18 + BR% * 3
                If nbrOuts(0) = 1 And currFielder = 7 Then I3 = 16 + BR% * 3
                If nbrOuts(0) = 2 And currFielder = 7 Then I3 = 34 + BR% * 3

                If nbrOuts(0) = 0 And currFielder = 8 Then I3 = 37 + BR% * 3
                If nbrOuts(0) = 1 And currFielder = 8 Then I3 = 44 + BR% * 3
                If nbrOuts(0) = 2 And currFielder = 8 Then I3 = 65 + BR% * 3

                If nbrOuts(0) = 0 And currFielder = 9 Then I3 = 12 + BR% * 3
                If nbrOuts(0) = 1 And currFielder = 9 Then I3 = 18 + BR% * 3
                If nbrOuts(0) = 2 And currFielder = 9 Then I3 = 40 + BR% * 3

                If H6% <= I3 Or P2 = OFF_HIT_N_RUN Then A5%(1) = 3

            End If

            Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)

            errorOnPlay% = 0
                                      
            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            If errorOnPlay% = 0 Then 
                If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try to advance on Double --"
                baseNbr! = 0
                Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)
            End If

            Call SCOREBOARD

            GoTo LCheckForOptionsOrQuit

        End If


        If H6% <= game_batRating(P, GM_2B, currLineupSlot(P))      + game_batRating(P, GM_3B, currLineupSlot(P))      And P2 <> OFF_HIT_N_RUN Or _
           H6% <= game_batRating(P, GM_2B, currLineupSlot(P)) * .5 + game_batRating(P, GM_3B, currLineupSlot(P)) * .5 And P2  = OFF_HIT_N_RUN Then

            S2% = EVENT_3B

            '3520
            ' *** TRIPLE S2%=9 ***
            Call OUTFLDNAME(currFielder, D)
            Call DELIVERY(D)
            Call WINDEX

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Trp)
            Else
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                Call INCPITOFF(P, currPitcher(P), LG_PB_Trp)
            End If

            Call ADDPIT(D, currPitcher(D), LG_P_H)

            Call Rolld100(RN)

            If RN <= 30 Then

                If currFielder = 7 Then
                    Call GetPBPString(40, 2900, Y$, D)
                    Call PBP(Y$)
                End If

                If currFielder = 9 Then
                    Call GetPBPString(40, 2940, Y$, D)
                    Call PBP(Y$)
                End If

                If currFielder = 8 Then
                    Call GetPBPString(40, 2980, Y$, D)
                    Call PBP(Y$)
                End If

                Call GetPBPString(40, 3050, Y$, D)
                Call PBP(Y$)

                Call Rolld100(RN)
                If RN <= 90 Then
                    pbpString$ = "Triple!!"
                Else
                    pbpString$ = "3-Bagger!"
                End If

                Call PBP(pbpString$)

            Else

                Call Rolld100(RN)

                If RN <= 10 And currFielder <> 8 Then

                    Call Rolld100(RN)
                    X$ = "1st"
                    If currFielder = 7 Then X$ = "3rd"

                    If RN <= 50 Then
                        Call PBP(player$(0) + " hits a hot smash")
                    Else
                        Call PBP(player$(0) + " lines one")
                    End If

                    Call PBP("Fair past " + X$ + "!!")
                    Call PBP("It rolls into the " + fieldPos$(currFielder) + " corner")
                    Call PBP("It's a triple!!")

                Else

                    Call LONGFLY(D, currFielder)
                    If currFielder = 7 And teamRat_GAME(1, 13) = 27 Then
                        ad$ = "screen"
                    Else
                        ad$ = "wall"
                    End If

                    Call Rolld100(BV)
                    Call Rolld100(RN)
                    Select Case RN
                        Case 1 To 20
                            pbpString$ = "It one-hops the " + ad$ + "..."
                        Case 21 To 40
                            pbpString$ = "It's off the top of the " + ad$ + "..."
                        Case 41 To 60
                            Call PBP("It bounces fair"): pbpString$ = "down into the corner..."
                        Case 61 To 80
                            Call PBP("It's in the gap"): pbpString$ = "It rolls to the " + ad$
                        Case 81 To 99
                            pbpString$ = "It hits at the base of the " + ad$ + "..."
                        Case 100
                            pbpString$ = "The outfielders cross"
                    End Select

                    Call PBP(pbpString$)

                    Call Rolld100(RN)
                    If RN <= 90 Then
                        pbpString$ = "Triple!!"
                    Else
                        pbpString$ = "3-Bagger!"
                    End If
                    Call PBP(pbpString$)

                End If

            End If

            A5%(0) = 3
            A5%(1) = 3
            A5%(2) = 2
            A5%(3) = 1

            Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)

            errorOnPlay% = 0
            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try to advance on Triple --"
            baseNbr! = 0
            If errorOnPlay% = 0 Then Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)

            Call SCOREBOARD
            GoTo LCheckForOptionsOrQuit

        End If

        Call HITS2GAP(currFielder, P, I1, D)

        I3 = game_batRating(P, GM_2B, currLineupSlot(P)) + game_batRating(P, GM_3B, currLineupSlot(P))

        E2% = I3 + game_batRating(P, GM_HR, currLineupSlot(P))
        If currFielder = 8 And H6% <= E2%      And P2 <> OFF_HIT_N_RUN Or _
           currFielder = 8 And H6% <= E2% * .5 And P2  = OFF_HIT_N_RUN Then
            S2% = EVENT_HR
            GoTo LHomeRun
        End If

        E2% = I3 + game_batRating(P, GM_L_HR, currLineupSlot(P))
        If currFielder = 7 And H6% <= E2%      And P2 <> OFF_HIT_N_RUN Or _
           currFielder = 7 And H6% <= E2% * .5 And P2 =  OFF_HIT_N_RUN Then
            S2% = EVENT_HR
            GoTo LHomeRun
        End If

        E2% = I3 + game_batRating(P, GM_R_HR, currLineupSlot(P))
        If currFielder = 9 And H6% <= E2%      And P2 <> OFF_HIT_N_RUN Or _
           currFielder = 9 And H6% <= E2% * .5 And P2 = OFF_HIT_N_RUN Then
            S2% = EVENT_HR
            GoTo LHomeRun
        End If

        13063 '
        Call Rolld100(H6%)
        S2% = EVENT_1B

        If fldPos(P, currLineupSlot(P)) <> 1 And H6% <= game_batRating(P, GM_BAvg, currLineupSlot(P)) And P2 <> OFF_HIT_N_RUN Then

            13065 '/ *** single ***
            Call SINGLEROUTINE(currFielder, P, I1, D, S2%, P2)

            If S2% = EVENT_INF_SINGLE Then

                Call SCOREBOARD
                GoTo LNextBatter0

            Else

                errorOnPlay% = 0
                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                If Environ$("PBPLOG") = "TRUE" Then Print #11, "-- gonna try to advance on Single --"
                baseNbr! = 0
                If errorOnPlay% = 0 Then Call TRYFOREXTRABASE(S2%, P2, runnerOn2nd, D, currFielder, SAH%, stealing, baseNbr!, I9)

                Call SCOREBOARD
                GoTo LCheckForOptionsOrQuit

            End If

        End If

        If H6% <= (batRating(P, lineupPlayer(P, currLineupSlot(1)), SS_B_Run) * 2.5) Then
            S2% = EVENT_INF_SINGLE
            Call GETINFIELDER(currFielder, P, I1, D, S2%, P2)
            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit
        End If

    End If


    '----- NOODLE -----

    GoTo LProcessOutAndDP2

   LHomeRun:
    ' *** HOME RUN S2%=10 ***

    Call WINDEX

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_HR)
    End If

    Call ADDPIT(D, currPitcher(D), LG_P_H)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
        Call INCPITOFF(P, currPitcher(P), LG_PB_HR)
    End If

    Call Rolld100(RN)

    If RN <= 80 Then

        Call LONGFLY(D, currFielder)

        Call Rolld100(RN)

        If RN <=  5 And teamRat_GAME(1, 13) = 8 And currFielder = 7 Then Call PBP("THAT'S OUT OF THE PARK"): Call PBP("ON WAVELAND AVENUE!!!"): pbpString$ = "HOME RUN...HOLY COW!!!": GoTo LHomeRun2
        If RN <= 15 And teamRat_GAME(1, 13) = 8 And currFielder = 7 Then Call PBP("IT'S GONE"): pbpString$ = "LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!": GoTo LHomeRun2
        If RN <= 25 Then

            '13580
            If teamRat_GAME(1, 13) = 21 And currFielder = 7 Then Call PBP("IT CLEARS THE GREEN MONSTER!!"): pbpString$ = "HOME RUN!!!": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 27 And currFielder = 7 Then Call PBP("IT CLEARS THE SCREEN!!"): pbpString$ = "HOME RUN!!!": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 33 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): pbpString$ = "THE BUSCH EAGLE FLAPS ITS WINGS": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 25 And P = 1 Then Call PBP("IT'S OUT OF HERE FOR A HOMER!!!"): pbpString$ = "THERE GOES THE EXPLODING SCOREBOARD!": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 7 And currFielder = 9 Then Call PBP("IT CLEARS THE WAGON GATE!!"): pbpString$ = "THAT'S A HOME RUN!!": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 7 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): pbpString$ = "THERE GO THE CANNONS!!": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 8 Or teamRat_GAME(1, 13) = 40 Then pbpString$ = "IT CLEARS THE IVY FOR A HOME RUN!!": GoTo LHomeRun2
            If teamRat_GAME(1, 13) = 23 And (currFielder = 7 Or currFielder = 9) Then Call PBP("IT'S LONG, IT'S LONG, IT'S GONE!!"): pbpString$ = "THAT ONE'S IN THE UPPER DECK": GoTo LHomeRun2

            '13545
            Call Rolld100(RN)
            Select Case RN
                Case 1 To 7
                    pbpString$ = "GOODBYE!! IT'S A HOME RUN!!"

                Case 8 To 14
                    pbpString$ = "ADIOS!! IT'S A HOME RUN !!!"

                Case 15 To 21
                    pbpString$ = "SAY GOOD-BYE TO THAT ONE!!!"

                Case 22 To 28
                    pbpString$ = "TOUCH 'EM ALL!!"

                Case 29 To 35
                    pbpString$ = "SEE YOU LATER!!"

                Case 36 To 42
                    pbpString$ = "GOOD-BYE MR. SPALDING!! HOME RUN!!!"

                Case 43 To 49
                    pbpString$ = "HOME RUN!! MY OH MY!!"

                Case 50 To 56
                    pbpString$ = "IT'S OVER THE WALL...OH DOCTOR!!!"

                Case 57 To 63
                    pbpString$ = "LONG, LONG, AND GONE!!"

                Case 64 To 70
                    pbpString$ = "HEY! HEY!...IT'S A HOME RUN !!!"

                Case 71 To 77
                    pbpString$ = "IT'S LONG, IT'S LONG, IT'S GONE!!!"

                Case 78 To 84
                    pbpString$ = "HOLY COW!! IT'S A HOME RUN!!!"

                Case 85 To 89
                    pbpString$ = "OPEN THE WINDOW AUNT MINNIE...HERE SHE COMES"

                Case 90 To 95
                    Call PBP("WAAYYY BACK..."): pbpString$ = "YOU CAN PUT IT ON THE BOARD, YYESSS!!"

                Case 96 To 100
                    Call PBP("IT'S WAY OUT OF HERE!! HOME RUN!!!"): pbpString$ = "HOW 'BOUT THAT!!!"

            End Select

        Else

            Call PBP("Going... going... GONE!"): pbpString$ = "HOME RUN!!"

        End If

    Else

        Call Rolld100(RN)

        If RN <= 20 And (currFielder = 7 Or currFielder = 9) Then

            pbpString$ = player$(0) + " hits it down the " + fieldPos$(currFielder) + " line"
            Call PBP(pbpString$)
            Call PBP("It's long enough if it's fair...")

        Else

            Call PBP(player$(0) + " swings...")

            Select Case RN
                Case 21 To 40
                    Call PBP("NO DOUBT ABOUT THAT ONE!!")
                    pbpString$ = "IT'S WAY OUT OF HERE!!"
                Case 41 To 60
                    pbpString$ = player$(4) + " CAN ONLY LOOK UP!!"
                Case 61 To 80
                    pbpString$ = pitchers$(D, currPitcher(D)) + " WATCHES IT SAIL OUT!!"
                Case 81 To 90
                    pbpString$ = "GONE!!!"
                Case 91 To 100
                    pbpString$ = "SAINTS AND HAYRAKES, LOOK AT 'ER SAIL!!"
            End Select

            Call PBP(pbpString$)

        End If

        '3536 / modified
        pbpString$ = "HOME RUN!!"

    End If

    '---- WHERE IS HITS2GAP


   LHomeRun2:
    Call PBP(pbpString$)
    Call DELAY

    '13544
    A5%(3) = 1
    A5%(2) = 2
    A5%(1) = 3
    A5%(0) = 4

    Call ADDPIT(D, currPitcher(D), LG_P_HR)
    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
    Call SCOREBOARD

    GoTo LCheckForOptionsOrQuit


    '----- NOODLE -----

   LProcessOutAndDP:
    ' 3
    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = nbrOuts(1) + 1
    player$(4) = batters$(D, fielder(D, currFielder))

    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
    Call CREDITREGPO(currFielder, D)

    S2% = EVENT_DP

    Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
    If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

    Call CREDITASSIST(currFielder, D)

    If currFielder = 5 And nbrOuts(0) < 2 And Int(Rnd(1) * 10) + 1 <= batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run) Then
        A5%(0) = 1
        A5%(1) = 11
        A5%(2) = 0
        A5%(3) = 0
        If nbrOuts(0) < 2 Then A5%(2) = 1: A5%(3) = 1
    Else
        A5%(0) = 11
        A5%(1) = 1
        A5%(2) = 0
        A5%(3) = 0
        If nbrOuts(0) < 2 Then A5%(2) = 1: A5%(3) = 1
    End If

    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = 2

    Call DPRELAY
    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    Call Rolld100(RN)

    DPC% = 0
    If RN <= 80 Then
        pbpString$ = " --- DOUBLE PLAY ---"
    Else
        'RN > 80
        pbpString$ = " --- TWIN KILLING ---"
    End If
    Call PBP(pbpString$)

    DP%(D) = DP%(D) + 1
    D3%(D) = D3%(D) + 1
    TB = 0
    Call INCREMENTHIT(P, D, MO)

    GoTo LCheckForOptionsOrQuit


    '----- NOODLE -----


   LGrounderToPitcher:
    If nbrOuts(0) >= 2 Then

        RN = Int(Rnd(1) * 10) + 1
        S2% = EVENT_GROUNDER

        If currFielder = 5 Then

            If RN <= 5 And (baseRunners = 4 Or baseRunners = 7) Then

                Call PITCHERNAME(currFielder, D)
                Call PBP(player$(4) + " gloves it and heads for 3rd")
                A5%(0) = 0
                A5%(1) = 0
                A5%(2) = 11
                A5%(3) = 0
                GoTo LProcessOutAndDP2

            Else

                Call PITCHERNAME(currFielder, D)
                Call GROUNDERS(currFielder, D)
                Call THROW2FIRST(D)
                A5%(0) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo LProcessOutAndDP2

            End If

        End If

        If currFielder = 6 Then

            If RN <= 8 And (baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7) Then
                Call PITCHERNAME(currFielder, D)
                Call GROUNDERS(currFielder, D)
                Call DPPIVOT(currFielder, D)
                AST% = 6
                A5%(0) = 0
                A5%(1) = 11
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo LProcessOutAndDP2

            Else
                Call PITCHERNAME(currFielder, D)
                Call GROUNDERS(currFielder, D)
                Call THROW2FIRST(D)
                A5%(0) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo LProcessOutAndDP2

            End If

        End If

        If currFielder = 4 And RN <= 5 And (baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7) Then

            Call PITCHERNAME(currFielder, D)
            Call GROUNDERS(currFielder, D)
            Call DPPIVOT(currFielder, D)
            AST% = 4
            A5%(0) = 0
            A5%(1) = 11
            A5%(2) = 0
            A5%(3) = 0

        Else

            Call Rolld100(RN)

            If currFielder = 3 Then

                If RN <= 94 Then

                    Call PITCHERNAME(currFielder, D)
                    Call GROUNDERS(currFielder, D)
                    Call PBP("He takes it to the bag unassisted")
                    fbun% = 1

                Else

                    Call PITCHERNAME(currFielder, D)
                    Call GROUNDERS(currFielder, D)
                    currFielder = 1
                    Call PITCHERNAME(currFielder, D)
                    Call PBP("He flips it to " + player$(4) + " covering")
                    FB2P% = 1
                    currFielder = 3

                End If

            Else

                Call PITCHERNAME(currFielder, D)
                Call GROUNDERS(currFielder, D)
                Call SECOND2FIRST(D)

            End If

            Call PITCHERNAME(currFielder, D)
            Call GROUNDERS(currFielder, D)
            Call SECOND2FIRST(D)

            A5%(0) = 11
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0

        End If

       LProcessOutAndDP2: 'Call DELAY
        If fbun% = 0 Then Call CREDITASSIST(currFielder, D)
        fbun% = 0

        If DPC% = 1 And A5%(0) = 1 Then DPC% = 2

        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)

        If DPC% = 2 Then
            DPC% = 0
            Call DPSAFE
        End If

        If I3 > 4 Then I3 = 1

        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

        GoTo LCheckForOptionsOrQuit

    Else

        Call Rolld100(RN)

        If (RN <= 90 And P2 <> OFF_HIT_N_RUN Or baseRunners = 0) Or (RN <= 10 And P2 = OFF_HIT_N_RUN) Then

            S2% = 14

            If baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6 Then
                A5%(0) = 11
                Call OUTATFIRST(currFielder, D, fbun%, FB2P%)
                GoTo 13693
            End If

            Call Rolld100(RN)

            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1

            If INFPOS% = INF_BACK And currFielder = 2 Then A5%(3) = 0
            If baseRunners = 5 Or baseRunners = 7 Or baseRunners = 4 Then 13725

            A5%(1) = 11

            GoTo LProcessOutAndDP2

        End If

        S2% = EVENT_GROUNDER
        A5%(1) = 1
        A5%(2) = 1
        A5%(3) = 1
        A5%(0) = 11

        If Not (baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7) Then

            13693 '
            Call Rolld100(RN)
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0

            If currFielder = 5 And RN <= 25 And INFPOS% < INF_IN   Then A5%(2) = 1
            If currFielder = 5 And RN <= 83 And INFPOS% = INF_BACK Then A5%(3) = 1 ' WAS 50
            If currFielder = 5 And RN <= 10 And INFPOS% > INF_BACK Then A5%(3) = 1

            If currFielder = 5 And RN < 5 And INFPOS% < INF_IN And baseRunners = 2 And gameScore(P, 0) - gameScore(D, 0) > -2 Or _
               currFielder = 5 And RN < 5 And INFPOS% < INF_IN And baseRunners = 6 And gameScore(P, 0) - gameScore(D, 0) > -2 And A5%(3) = 1 Then
                A5%(2) = 11
                A5%(0) = 1
                Call PBP("NO, he's going to third")
            End If

            If currFielder = 6 And RN <= 20 And INFPOS% < INF_IN Then A5%(2) = 1
            If currFielder = 6 And RN <= 92 And INFPOS% < INF_IN Then A5%(3) = 1
            If currFielder = 6 And RN <=  5 And INFPOS% = INF_IN Then A5%(3) = 1
            If currFielder = 6 And RN <   6 And INFPOS% < INF_IN And baseRunners = 2 And gameScore(P, 0) - gameScore(D, 0) > -2 Or _
               currFielder = 6 And RN <   6 And INFPOS% < INF_IN And baseRunners = 6 And gameScore(P, 0) - gameScore(D, 0) > -2 And A5%(3) = 1 Then A5%(2) = 11: A5%(0) = 1: Call PBP("NO, he's going to third")
            If currFielder = 4 And RN <= 90 And INFPOS% < INF_IN Then A5%(2) = 1
            If currFielder = 4 And RN <= 90 And INFPOS% < INF_IN Then A5%(3) = 1
            If currFielder = 4 And RN <= 65 And INFPOS% = INF_IN Then A5%(2) = 1
            If currFielder = 4 And RN <=  5 And INFPOS% = INF_IN Then A5%(3) = 1
            If currFielder = 3 And RN <= 95 Then A5%(2) = 1
            If currFielder = 3 And RN <= 88 And INFPOS% = INF_BACK Then A5%(3) = 1  ' WAS 60
            If currFielder = 3 And RN <= 15 And INFPOS% > INF_BACK Then A5%(3) = 1
            If currFielder = 1 And RN <= 30 Then A5%(2) = 1
            If currFielder = 1 And RN <= 50 And INFPOS% = INF_BACK Then A5%(3) = 1
            If currFielder = 1 And RN <= 10 And INFPOS% > INF_BACK Then A5%(3) = 1
            If currFielder = 2 And RN <= 45 Then A5%(2) = 1

        End If

        GoTo LProcessOutAndDP2

    End If

    13725 '
    If baseRunners <> 4 And baseRunners <> 7 Then

        If gameScore(P, 0) - gameScore(D, 0) >= 0 And INNING% > 6 And nbrOuts(0) = 0 And INFPOS% > INF_BACK And currFielder = 5 Or _
           gameScore(P, 0) - gameScore(D, 0) >= 0 And INNING% > 6 And nbrOuts(0) = 0 And INFPOS% > INF_BACK And currFielder = 3 Then

            A5%(3) = 11

        Else

            If INFPOS% = INF_BACK And currFielder <> 2 Then A5%(1) = 11: GoTo LProcessOutAndDP2
            If INFPOS% > INF_BACK And currFielder = 5 Or INFPOS% > INF_BACK And currFielder = 3 Then A5%(3) = 0: A5%(1) = 11: GoTo LProcessOutAndDP2
            If INFPOS% > INF_BACK And (currFielder = 1 Or currFielder = 3) Then A5%(3) = 0: A5%(1) = 1: A5%(0) = 11: GoTo LProcessOutAndDP2
            If INFPOS% = INF_IN And currFielder = 4 Or INFPOS% = INF_IN And currFielder = 6 And RN <= 90 Then
                A5%(3) = 0
                A5%(1) = 11
            Else
                A5%(1) = 11
            End If

        End If

    Else

        If currFielder = 6 And RN < 6 And INFPOS% < INF_IN And INNING% < 9 Or _
           currFielder = 5 And RN < 6 And INFPOS% < INF_IN And INNING% < 9 Then
            A5%(2) = 11
        Else
            If baseRunners = 7 Then

                Call Rolld100(RN)
                A5%(1) = 11
                A5%(2) = 1
                A5%(3) = 1
                A5%(0) = 1

                If INFPOS% > INF_BACK And currFielder = 5 Or _
                   INFPOS% > INF_BACK And currFielder = 3 Or _
                   INFPOS% = INF_IN And currFielder = 4 Or _
                   INFPOS% = INF_IN And currFielder = 6 Or _
                   currFielder = 2 Or currFielder = 1 Then
                    A5%(3) = 11
                    A5%(0) = 1
                    A5%(1) = 1
                    A5%(2) = 1
                Else
                    If currFielder = 5 And RN <= 5 Or currFielder = 1 Or currFielder = 2 Then
                        A5%(3) = 11
                        A5%(2) = 1
                        A5%(1) = 1
                        A5%(0) = 1
                    End If
                End If

            Else

                RN = Int(Rnd(1) * 10) + 1

                A5%(0) = 1
                A5%(1) = 11
                A5%(2) = 1
                A5%(3) = 0

                If currFielder = 5 And RN <= 5 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
                If currFielder = 1 And RN <= 3 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0

                If currFielder = 2 And batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run) < 7 And batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run) > 0 Then
                    A5%(1) = 1
                    A5%(2) = 11
                    A5%(3) = 0
                Else
                    If currFielder = 2 And batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run) > 6 Then
                        A5%(1) = 1
                        A5%(2) = 1
                        A5%(0) = 11
                    End If
                End If

            End If

        End If

    End If

    GoTo LProcessOutAndDP2


    '-----------------------------------------------------
    '--- THIS IS CRITICAL FOR PBP MOVING AUTOMATICALLY ---
    '-----------------------------------------------------
    ' Play-by-Play box
   LCheckForOptionsOrQuit:
    If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144)

    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    I$ = InKey$

    If UCase$(I$) = Chr$(17) Then
        Call EndOfGame
        Exit Sub
    End If

    If UCase$(I$) = "O" Then
        Call OPTIONS
    End If

    'There was some question as to whether this is needed
    'based on comparing to the original code, but the working
    'version of my code had this in it still
    If useVGA = 1 Then _PutImage (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80), backupScreen&, imageScreen&, (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80)

    DPC% = 0
    runnerHeld = 0
    INFPOS% = INF_BACK
    errorOnPlay% = 0
    H0%(0) = 0

    If nbrOuts(0) = 3 Then
        GoTo LNextHalfInning
    Else
        GoTo LNextBatter0
    End If

    '----- NOODLE -----

   LBunt: ' *** BUNT ROUTINE ***
    Call WINDEX

    Erase A5%

    SH = 2
    S2% = EVENT_BUNT

    If (baseRunners = 3 Or baseRunners > 4) And nbrOuts(0) < 2 Then Call SQUEEZECHOICE(SH)

    Call Rolld100(RN)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        BR = pitchRating(P, currPitcher(P), SS_P_Bunt)
    Else
        BR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt)
    End If

    If INFPOS% > INF_BACK Then BR = BR - 2
    If BR <= 1 Then BR = 1

    For I = 1 To 9:
        If RN <= BT%(teamRat_GAME(1, 10), BR, I) Then Exit For
    Next

    teamTotWins% = Int(Rnd(1) * 10)
    If teamTotWins% <= 2 Then currFielder = 5
    If teamTotWins% > 2 And teamTotWins% <= 4 Then currFielder = 1
    If teamTotWins% > 4 And teamTotWins% <= 7 Then currFielder = 3
    If teamTotWins% > 7 Then currFielder = 2
    Call GETFIELDNAME(currFielder, D)

    Call Rolld100(RN)

    If RN <= 52 Then 14175

    If I = 5 And SH = 1 Then I = 6

    'This I and the ones in the case statement
    'all have to do with the For-Next loop
    Select Case I

        Case 1
            'PU
            Call PBP(player$(0) + " bunts it in the air...")
            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            Call PBP(player$(4) + " makes the catch")
            Call ADDSTTS(P, D)

            nbrOuts(0) = nbrOuts(0) + 1
            nbrOuts(1) = nbrOuts(1) + 1
            Call CREDITREGPO(currFielder, D)

            If nbrOuts(0) <> 3 And SH <> 2 Then
                '--- start of a new sub ???
                '--- where does 4111 end ???
                14111 '
                If currFielder <> 5 Then
                    Call CREDITASSIST(currFielder, D)
                    currFielder = 5
                    Call CREDITREGPO(currFielder, D)
                    player$(5) = player$(4)
                    Call GETFIELDNAME(currFielder, D)
                Else
                    Call CREDITASSIST(currFielder, D)
                    currFielder = 6
                    Call CREDITREGPO(currFielder, D)
                    player$(5) = player$(4)
                    Call GETFIELDNAME(currFielder, D)
                End If

                Call PBP(player$(5) + " throws to " + player$(4) + " at 3rd")
                A5%(3) = 10
                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1

                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                If SH = 1 Then
                    If nbrOuts(0) < 3 Then
                        GoTo LNextBatter1
                    Else
                        'nbrOuts(0) >= 3
                        currLineupSlot(P) = currLineupSlot(P) - 1
                    End If
                End If

            End If

            GoTo LCheckForOptionsOrQuit

        Case 2
            'DP
            If baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7 Then

                Call PBP(player$(0) + " lays down a good bunt")

                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

                Call PBP(player$(4) + " fields it cleanly")

                If SH = 1 And (baseRunners = 5 Or baseRunners = 7) Then

                    If currFielder = 2 Then
                        Call PBP("and catches " + player$(3) + " heading home!")
                        Call CREDITREGPO(currFielder, D)
                    Else
                        'currFielder <> 2
                        Call PBP("and throws home to nail " + player$(3) + "!")
                        Call CREDITASSIST(currFielder, D)
                        currFielder = 2
                        Call CREDITREGPO(currFielder, D)
                    End If

                    A5%(3) = 11

                    For I = 0 To 2: A5%(0) = 1: Next

                    '14122

                    Call ADDSTTS(P, D)
                    nbrOuts(0) = nbrOuts(0) + 1
                    nbrOuts(1) = nbrOuts(1) + 1
                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    GoTo LCheckForOptionsOrQuit

                End If

                If baseRunners = 7 And nbrOuts(0) < 2 Then

                    A5%(3) = 11

                    If currFielder = 2 Then
                        Call PBP("He steps on home for one...")
                        Call CREDITREGPO(currFielder, D)
                        Call CREDITASSIST(currFielder, D)
                    Else
                        'currFielder <> 2
                        Call PBP("He throws home for the force")
                        Call CREDITASSIST(currFielder, D)
                        currFielder = 2
                        Call CREDITREGPO(currFielder, D)
                        Call CREDITASSIST(currFielder, D)
                    End If

                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call PBP("Back to first...")

                    A5%(0) = 11

                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                    Call PBP("DOUBLE PLAY!!")

                    nbrOuts(0) = nbrOuts(0) + 2
                    nbrOuts(1) = nbrOuts(1) + 2

                    Call ADDSTTS(P, D)

                    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                    If nbrOuts(0) < 3 Then
                        A5%(2) = 1
                        A5%(1) = 1
                        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    End If

                    GoTo LCheckForOptionsOrQuit

                End If

                Call PBP("He throws to 2nd")
                Call CREDITASSIST(currFielder, D)

                A5%(1) = 11

                Call ADDSTTS(P, D)

                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                If nbrOuts(0) < 3 Then

                    A5%(0) = 11
                    Call CREDITASSIST(currFielder, D)

                    A5%(2) = 1
                    Call PBP("over to first...")
                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call PBP("DOUBLE PLAY!!")
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    nbrOuts(0) = nbrOuts(0) + 1
                    nbrOuts(1) = nbrOuts(1) + 1
                    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                End If

                GoTo LCheckForOptionsOrQuit

            Else
                Call PBP(player$(0) + " lays down the bunt...")
                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                Call PBP(player$(4) + " makes a nice fielding play")
                Call CREDITASSIST(currFielder, D): A5%(0) = 11
                If SH = 1 And (baseRunners = 3 Or baseRunners = 6) Then A5%(3) = 11: For I = 0 To 2: A5%(I) = 1: Next

                '14122
                Call ADDSTTS(P, D)
                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                GoTo LCheckForOptionsOrQuit
            End If

        Case 3
            Call LDRNRCTDN
            GoTo LCheckForOptionsOrQuit

        Case 4
            14140 'SHGOOD
            Call PBP(player$(0) + " lays down a good bunt...")
            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            Call PBP(player$(4) + " makes a nice play")

            If baseRunners = 0 Then

                S2% = -EVENT_INF_SINGLE
                Call PBP(player$(0) + " beats out an infield hit!")
                GoTo 13065

            Else
                '-- turn this into a unique sub?
                '-- or direct other code to this loop
                '-- with a value of 4 and a 'skip' variable
                14141 '

                Call ADDPIT(D, currPitcher(D), 15)

                If fldPos(P, currLineupSlot(P)) = 1 Then
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Sac)
                Else
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Sac)
                End If

                Call Rolld100(RN)

                If baseRunners = 5 And SH = 2 And RN > 98 Or RN > 97 Then
                    S2% = -EVENT_INF_SINGLE
                    Call PBP(player$(0) + " beats it out!")
                    GoTo 13065
                End If

                A5%(0) = 11

                Call PBP("The only play is to first")

                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1

                Call CREDITASSIST(currFielder, D)
                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                If nbrOuts(0) < 3 Then
                    For I = 1 To 3
                        A5%(I) = 1
                    Next
                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                End If

            End If

            GoTo LCheckForOptionsOrQuit

        Case 5
            14150 'C@3
            Call Rolld100(RN)
            If fldPos(P, currLineupSlot(P)) = 1 Then
                RR = 2
            Else
                RR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Run)
            End If

            If INFPOS% > INF_BACK Then RR = RR - 2
            If RR <= 2 Then RR = 2

            For I = 0 To 2
                If RN <= CF%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1
                Case 1

                    If baseRunners > 0 Then 14157
                    Call PBP(player$(0) + " drops it right down the line")
                    Call PBP(player$(4) + " lets it roll...")
                    Call PBP(player$(0) + " is trying for 2nd!!")
                    Call PBP(player$(4) + " fields and throws too late!!")
                    Call PBP("You won't see that play very often")

                    A5%(0) = 2
                    Call ADDPIT(D, currPitcher(D), LG_P_H)

                    If fldPos(P, currLineupSlot(P)) <> 1 Then
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
                    Else
                        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                        Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
                    End If

                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call SCOREBOARD

                    GoTo LCheckForOptionsOrQuit

                Case 2
                    14157 '
                    S2% = EVENT_INF_SINGLE
                    Call PBP(player$(0) + " lays down a great bunt...")

                    Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                    If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

                    Call PBP(player$(4) + " fields... but too late!")
                    S2% = -EVENT_INF_SINGLE
                    GoTo 13065

                Case 3
                    GoTo 14140

            End Select

        Case 6
            'PLYLDBS
            If fldPos(P, currLineupSlot(P)) = 1 Then RR = 2
            If fldPos(P, currLineupSlot(P)) <> 1 Then RR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Run)
            If INFPOS% > INF_BACK Then RR = RR - 2
            If RR <= 2 Then RR = 2

            If baseRunners = 0 Then 14150
            If baseRunners = 1 Then I3 = 1: J3$ = "2nd"
            If baseRunners = 2 Or baseRunners = 4 Then I3 = 2: J3$ = "3rd"
            If baseRunners = 3 Or baseRunners > 4 Then I3 = 3: J3$ = "home"

            Call PBP(player$(0) + " gets the bunt down...")

            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

            Call PBP(player$(4) + " fields it quickly...")
            Call PBP("Looks like a close play at " + J3$ + "!!")
            Call PBP("1- Throw to " + J3$ + " 2- Throw to 1st")

            If playerMode = 2 Or playerMode = 1 And P <> compTeam Then
                I$ = "1"
            Else
                Do
                    I$ = GetKeyPress$
                    I5 = Val(I$)
                Loop Until I5 >= 1 And I5 <= 2
            End If

            If I5 = 2 Then 14141

            Call Rolld100(RN)

            For I = 0 To 1
                If RN <= LB%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            If I = 0 Then
                PLB = 1
                Call LDRNRCTDN
            Else
                Call PBP(player$(4) + " throws to " + J3$ + "... SAFE!!")
                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                End If

                For I = 0 To 3:
                    A5%(I) = 1
                Next

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
            End If

            GoTo LCheckForOptionsOrQuit

        Case 7
            'FLKSO
            Call Rolld100(RN)

            If pitchRating(D, currPitcher(D), 6) = 0 Then
                BR = 0
            Else
                BR = pitchRating(D, currPitcher(D), 9) / pitchRating(D, currPitcher(D), 6)
            End If

            If BR <= 4 Then PR = 5
            If BR > 4 And BR <= 5.4 Then PR = 4
            If BR > 5.4 And BR <= 6.9 Then PR = 3
            If BR > 6.9 And BR <= 8.4 Then PR = 2
            If BR > 8.4 And BR <= 11.9 Then PR = 1
            If BR > 11.9 Then PR = 0

            For I = 0 To 2
                If RN <= SO%(PR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1

                Case 1
                    GoTo LStrikeout

                Case 2
                    nbrStrikes = nbrStrikes + 1

                    Call WINDEX

                    Call Rolld100(RN)
                    pbpString$ = player$(0) + " "
                    If RN <= 13 Or SH = 1 And RN <= 42 Then pbpString$ = pbpString$ + "misses the pitch"
                    If RN > 13 And RN <= 42 And SH <> 1 Then pbpString$ = pbpString$ + "takes a called strike"
                    If RN > 42 Then pbpString$ = pbpString$ + "bunts it foul"
                    Call PBP(pbpString$)

                    If useVGA = 1 Then
                        PrintOnGfx 63, 28, Str$(nbrStrikes), imageScreen&
                    Else
                        Locate 2, 58: Print nbrStrikes
                    End If

                    If nbrStrikes = 3 Then Goto LStrikeout

                    If SH = 1 And RN <= 42 Then
                        currFielder = 2
                        Call GETFIELDNAME(currFielder, D)
                        GoTo 14111
                    End If

                    If nbrStrikes = 1 Then
                        Call PBP("He has" + Str$(nbrStrikes) + " strike")
                    Else
                        'nbrStrikes > 1
                        Call PBP("He has" + Str$(nbrStrikes) + "strikes")
                    End If
                    Call PBP(pbpString$)

                    If baseRunners = 4 Or baseRunners = 2 Then INFPOS% = INF_BACK

                    If useVGA = 1 Then
                        _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
                    End If

                    GoTo LContinueBatter2

                Case 3
                    GoTo 14140

            End Select

        Case 8
            'TFPLY
            Call PBP(player$(0) + " lays down the bunt...")
            Call PBP(player$(4) + " has a tough play")

            Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
            If errorOnPlay% = 1 Then Goto LCheckForOptionsOrQuit

            If currFielder = 1 Then RR = 3
            If currFielder <> 1 Then RR = batRating(D, fielder(D, currFielder), SS_B_Range)
            If INFPOS% > INF_BACK And baseRunners <> 0 Then RR = RR + 1 + teamRat_GAME(1, 10)
            If INFPOS% > INF_BACK And baseRunners = 0  Then RR = RR + teamRat_GAME(1, 10)
            If INFPOS% = INF_BACK And baseRunners = 0  Then RR = RR - 1

            If RR > 11 Then RR = 11
            If RR < 0 Then RR = 0

            Call Rolld100(RN)

            For I = 0 To 2
                If RN <= triplePlayAdj(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1
                Case 1:
                    S2% = EVENT_INF_SINGLE
                    Call PBP("but too late...base hit!")
                    GoTo 13065

                Case 2:
                    PLB = 1
                    Call LDRNRCTDN
                    GoTo LCheckForOptionsOrQuit

                Case 3:
                    GoTo 14141

            End Select

        Case 9
            14175 '
            nbrStrikes = nbrStrikes + 1

            Call WINDEX
            Call Rolld100(RN)

            pbpString$ = player$(0) + " "
            If RN <= 13 Or SH = 1 And RN <= 42 Then pbpString$ = pbpString$ + "misses the pitch"
            If RN > 13 And RN <= 42 And SH <> 1 Then pbpString$ = pbpString$ + "takes a called strike"
            If RN > 42 Then pbpString$ = pbpString$ + "bunts it foul"
            Call PBP(pbpString$)

            If useVGA = 1 Then
                PrintOnGfx 63, 28, Str$(nbrStrikes), imageScreen&
            Else
                Locate 2, 58: Print nbrStrikes
            End If

            If nbrStrikes = 3 Then
                GoTo LStrikeout
            End If

            If SH = 1 And RN <= 42 Then currFielder = 2: Call GETFIELDNAME(currFielder, D): GoTo 14111

            If nbrStrikes = 1 Then
                Call PBP("He has" + Str$(nbrStrikes) + " strike")
            Else
                'nbrStrikes > 1
                Call PBP("He has" + Str$(nbrStrikes) + "strikes")
            End If

            If baseRunners = 4 Or baseRunners = 2 Then INFPOS% = INF_BACK

            If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)

            GoTo LContinueBatter2

    End Select

    '-- at this point, each selection in the Case statement
    '-- should have directed the code somewhere else
    '-- in essence, this is kind of the end of this code


    '--------------------


   LSteal:
    stealHome% = 0
    S2% = EVENT_STEAL
    currFielder = 1
    player$(4) = pitchers$(D, currPitcher(D))

    Select Case baseRunners
        Case 0
            GoTo LNextBatter1
            'set a "skip" variable here

        Case 1, 5

            TGTBASE% = 2

            If P2 <> OFF_HIT_N_RUN Then

                Call WINDEX
                stealing = 1
                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                stealing = 0

                If errorOnPlay% = 1 Then
                    currLineupSlot(P) = currLineupSlot(P) - 1
                    Call WHOSON1ST(baseRunners)
                    GoTo LCheckForOptionsOrQuit
                End If

            End If

        Case 2, 4

            TGTBASE% = 3

            If P2 <> OFF_HIT_N_RUN Then

                Call WINDEX
                stealing = 1
                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)
                stealing = 0

                If errorOnPlay% = 1 Then
                    currLineupSlot(P) = currLineupSlot(P) - 1
                    Call WHOSON1ST(baseRunners)
                    GoTo LCheckForOptionsOrQuit
                End If

            End If

        Case 3, Is >= 6
            TGTBASE% = 4

    End Select

    I2 = lineupPlayer(P, b2Base(TGTBASE% - 1))
    A5%(1) = 1
    A5%(2) = 1
    A5%(3) = 0

    Call STEALCHANCE(AA, I2, TGTBASE%, CHANCE%)
    If fldPos(P, b2Base(TGTBASE% - 1)) = 1 Then CHANCE% = 0

    'Reduce chance for stealing home
    If TGTBASE% = 4 Then CHANCE% = CHANCE% * .05

    If batRating(P, I2, SS_B_CS) > 0 Then 
        stlChance = Int((batRating(P, I2, SS_B_SB) / (batRating(P, I2, SS_B_SB) + batRating(P, I2, SS_B_CS))) * 1000)
    ElseIf batRating(P, I2, SS_B_SB) > 31 Then 
        stlChance = 700 + ((batRating(P, I2, SS_B_SB) - 30) *  2)
    ElseIf batRating(P, I2, SS_B_SB) > 10 Then 
        stlChance = 600 + ((batRating(P, I2, SS_B_SB) - 10) * 10)
    ElseIf batRating(P, I2, SS_B_SB) >  0 Then 
        stlChance = 400 + ((batRating(P, I2, SS_B_SB) * 20))
    Else 
        stlChance = 100
    End If

    If fldPos(P, b2Base(TGTBASE% - 1)) = 1 Then stlChance = 100

    Call STEAL3RD(TGTBASE%, AA, stlChance, runnerHeld, D) ' *** SUCCESS OF 3B STEAL ***

    If pitchRating(D, currPitcher(D), SS_P_Throws) =  1 And TGTBASE% = 2 Then CHANCE% = CHANCE% + 25: stlChance = stlChance + 10
    If pitchRating(D, currPitcher(D), SS_P_Throws) = -1 And TGTBASE% = 2 Then CHANCE% = CHANCE% - 50: stlChance = stlChance - 30
    If P2 = OFF_HIT_N_RUN Then

        CHANCE% = 100
        stlChance = stlChance - 200
        currFielder = 2
        player$(4) = batters$(D, fielder(D, 2))
        TGBASE% = lineupPlayer(P, b2Base(TGTBASE% - 1))
        b2Mystery(TGTBASE% - 1) = 18

        Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)

        If errorOnPlay% = 1 Then
            Call INCBATOFF(P, TGBASE%, 9)
            currLineupSlot(P) = currLineupSlot(P) - 1
            Call STRIKEOUTCS(SO)
            GoTo LCheckForOptionsOrQuit
        Else
            GoTo LDoubleSteal2
        End If

    End If

    b2Mystery(TGTBASE% - 1) = b2Mystery(TGTBASE% - 1) + 1

    Call Rolld100(I4)

    If I4 <= b2Mystery(TGTBASE% - 1) Then

        S2% = EVENT_RUNDOWN
        currFielder = 1
        player$(4) = pitchers$(D, currPitcher(D))

       LCaughtInRundown:
        Call WINDEX
        Call PBP(player$(TGTBASE% - 1) + " caught off base in a rundown!")

        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 0
        A5%(TGTBASE% - 1) = 11

        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        Call PUTOUT(TGTBASE%, P)
        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

        If nbrOuts(0) >= 3 Then
            currLineupSlot(P) = currLineupSlot(P) - 1
            GoTo LCheckForOptionsOrQuit
        End If

        INFPOS% = INF_BACK

        GoTo LNextBatter1

    End If

    '----------------------------------------
    '-- this is the start a new large area of code
    '-- perhaps it could become a sub?
    '----------------------------------------

    '14825
    'RUNNER HAS A CHANCE TO STEAL
    b2Mystery(TGTBASE% - 1) = b2Mystery(TGTBASE% - 1) + 17
    I9 = Int(Rnd(1) * 1000) + 1

    If I9 > CHANCE% * 1.388 And D5 <> 3 Then

        Call WINDEX
        Call NOJUMP(D)

        If useVGA = 1 Then _PutImage (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80), backupScreen&, imageScreen&, (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80)

        GoTo LOffenseOptions

    End If

   LStealHome:
    Call WINDEX
    TGBASE% = lineupPlayer(P, b2Base(TGTBASE% - 1))

    Call GetPBPString(20, 3090, Y$, D)
    Call PBP(Y$)

    Call GetPBPString(13, 3110, Y$, D)
    Call PBP(Y$)

    Call PITCHOUT(baseRunners, TGTBASE%, stlChance)

    If baseRunners <> 5 Then Goto LDoubleSteal ' *** DOUBLE STEAL ***1st&3rd

    If stealHome% = 1 Then
        I9 = Int(Rnd(1) * 1000) + 1
    Else
        If playerMode = 2 Or playerMode = 1 And P <> compTeam Then
            I$ = "Y"
        Else
            Call PBP("Throw to 2B? (Y/N) ")
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then pitchoutChance = 7: GoTo LRunnerSafe

       LDoubleSteal:
        currFielder = 2
        player$(4) = batters$(D, fielder(D, 2))

        Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)

        If errorOnPlay% = 1 Then

            Call INCBATOFF(P, TGBASE%, 9)
            Call PITSWAP(TGTBASE%)
            currLineupSlot(P) = currLineupSlot(P) - 1
            GoTo LCheckForOptionsOrQuit

        End If

       LDoubleSteal2:
        Call WHICHRUNNER(TGTBASE%, baseRunners, D5)
        I9 = Int(Rnd(1) * 1000) + 1
        stlChance = stlChance - ((batRating(D, fielder(D, 2), SS_B_Arm) - 5) * 25)

    End If

    RN = Int(Rnd(1) * 11) + 1
    If TGTBASE% <> 4 Then
        If RN = 1 Then pbpString$ = batters$(D, fielder(D, 2)) + " rifles it to " + baseName$(TGTBASE%)
        If RN = 2 Then pbpString$ = batters$(D, fielder(D, 2)) + " fires it to " + baseName$(TGTBASE%)
        If RN = 3 Then pbpString$ = batters$(D, fielder(D, 2)) + " unloads it"
        If RN = 4 Then pbpString$ = batters$(D, fielder(D, 2)) + " is up and firing"
        If RN = 5 Then pbpString$ = batters$(D, fielder(D, 2)) + " comes up firing"
        If RN = 6 Then pbpString$ = batters$(D, fielder(D, 2)) + " lets it fly"
        If RN = 7 Then pbpString$ = batters$(D, fielder(D, 2)) + " throws down"
        If RN = 8 Then pbpString$ = batters$(D, fielder(D, 2)) + " gets it off"
        If RN = 9 Then pbpString$ = batters$(D, fielder(D, 2)) + " fires a missile"
        If RN = 10 Then pbpString$ = batters$(D, fielder(D, 2)) + " throws quickly"
        If RN = 11 Then pbpString$ = batters$(D, fielder(D, 2)) + " throws"
        Call PBP(pbpString$)
    End If

    If Not (b2Mystery(TGTBASE% - 1) = 18 And I9 > stlChance Or b2Mystery(TGTBASE% - 1) > 18 And I9 > stlChance - 200) Then

        LRunnerSafe:
        Call Rolld100(RN)
        Select Case RN
            Case 1 To 20: pbpString$ = "He beats the throw!"
            Case 21 To 40: pbpString$ = "He's in there!"
            Case 41 To 50: pbpString$ = "Throw is off-line..."
            Case 51 To 60: pbpString$ = "Throw is high..."
            Case 61 To 80: pbpString$ = "He slides under the tag!"
            Case 81 To 100:
                If TGTBASE% <> 4 Then
                    pbpString$ = gameUmpire$(TGTBASE% + 1) + " with the call"
                Else
                    pbpString$ = gameUmpire$(1) + " with the call"
                End If
        End Select

        Call PBP(pbpString$)
        Call PBP("safe @ " + baseName$(TGTBASE%) + "!")

        b2Mystery(TGTBASE% - 1) = 0
        INFPOS% = INF_BACK
        Call PITSWAP(TGTBASE%)
        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)

        Call INCBATDF(D, fielder(D, 2), LG_BF_SB)

        If stealHome% <> 1 Then

            If Not (baseRunners <> 5 Or baseRunners = 5 And pitchoutChance = 7) Then
                pitchoutChance = 0
                TGTBASE% = 4

                If Int(Rnd(1) * 20) + 1 = 6 Then
                    GoTo LCaughtInRundown
                Else
                    GoTo LStealContinue
                End If
            Else
                GoTo LStealContinue
            End If
        Else
            stealHome% = 0
            GoTo LNextBatter1
        End If

    End If

    Call Rolld100(RN)

    Select Case RN
        Case 1 To 20
            pbpString$ = batters$(D, fielder(D, 2)) + " nails " + player$(TGTBASE% - 1)

        Case 21 To 40
            pbpString$ = player$(TGTBASE% - 1) + " is pegged at " + baseName$(TGTBASE%)

        Case 41 To 60
            pbpString$ = "Great throw by " + batters$(D, fielder(D, 2)) + "!!"

        Case 61 To 80
            pbpString$ = player$(TGTBASE% - 1) + " slides into the tag"

        Case 81 To 100
            If TGTBASE% <> 4 Then
                pbpString$ = gameUmpire$(TGTBASE% + 1) + " with the call"
            Else
                'TGTBASE% = 4
                pbpString$ = gameUmpire$(1) + " with the call"
            End If
    End Select

    Call PBP(pbpString$)

    A5%(TGTBASE% - 1) = 11
    INFPOS% = INF_BACK

    Call INCBATOFF(P, lineupPlayer(P, b2Base(TGTBASE% - 1)), LG_B_CS)

    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = nbrOuts(1) + 1
    currFielder = 2

    Call CREDITASSIST(currFielder, D)
    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

    Call INCBATDF(D, fielder(D, 2), LG_BF_CS)

    If nbrOuts(0) >= 3 Then
        currLineupSlot(P) = currLineupSlot(P) - 1
        Call STRIKEOUTCS(SO)
        GoTo LCheckForOptionsOrQuit
    End If

   LStealContinue:
    If nbrOuts(0) >= 3 Or P2 = OFF_HIT_N_RUN And nbrStrikes = 3 Then
        GoTo LCheckForOptionsOrQuit
    End If

    If stealHome% = 1 Then
        stealHome% = 0
        GoTo LNextBatter1
    End If

    If AA < 20 Or baseRunners <> 5 Or fldPos(P, b2Base(3)) = 1 Then
        GoTo LNextBatter1
    End If

    Call WINDEX

    If playerMode = 2 Or playerMode = 1 And P = compTeam Then
        I$ = "N"
    Else
        Call PBP("TRY TO STEAL HOME? (Y/N)")
        I$ = GetKeyPress$
    End If

    If UCase$(I$) = "Y" Then

        GoTo LNextBatter1

    Else
        SH% = 1
        TGTBASE% = 4
        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 1
        I2 = lineupPlayer(P, b2Base(TGTBASE% - 1))
        CHANCE% = 100
        stlChance = 700

        GoTo LStealHome ' *** STEAL HOME ***

    End If


    '----------------------------------------
    '-- this is a major area of code, but
    '-- it's also short. perhaps it could
    '-- be made into a subroutine?
    '----------------------------------------

   LStayInGame:
    Cls

    If useVGA = 0 Then Screen menu&

    P9 = D
    currLineupSlot(P) = currLineupSlot(P) + 1
    nbrStrikes = 0

    If currLineupSlot(P) = 10 Then currLineupSlot(P) = 1

    Color 15
    Print "DOES "; batters$(D, lineupPlayer(D, fielder(D, 0))); " STAY IN THE GAME? (Y/N)"
    I$ = GetKeyPress$

    If UCase$(I$) = "N" Then

        I5 = 77

    Else

        I5 = 66

        Locate 5, 1

        For I = 1 To 9:

            Color 3, 0
            If BP(P9) = I Then Color 9
            Locate I + 4, 5: Print Using "# "; I;

            If fldPos(P9, I) = 1 Then
                If I5 = 66 Then
                    Print batters$(P9, lineupPlayer(P9, I));
                Else
                    Print pitchers$(P9, lineupPlayer(P9, I));
                End If
            Else
                Print batters$(P9, lineupPlayer(P9, I));
            End If

            Print " "; fieldPos$(fldPos(P9, I));
            Color 3

        Next I

        I5 = 0

        Do
            Color 15
            Locate 3, 1: Print "WHAT LINEUP SLOT WILL THE PITCHER BAT IN (1-9)"
            I$ = GetKeyPress$
            I2 = Val(I$)
        Loop Until I2 >= 1 And I2 <= 9

        If fldPos(D, I2) = 10 Then fldPos(D, I2) = 1

        INFPOS% = INF_BACK
        fldPos(D, fielder(D, 0)) = fldPos(D, I2)
        fielder(D, fldPos(D, fielder(D, 0))) = lineupPlayer(D, fielder(D, 0))
        fldPos(D, I2) = 1
        I7% = D
        I8% = fielder(D, 0)
        I9% = lineupPlayer(D, fielder(D, 0))

        Call REPLACEMENTS(I7%, I8%, I9%)

        I5 = 66

    End If

    GoTo LChangePitcher


    '-----------------------------------
    '-----      PERSONNEL MENU     -----
    '-----------------------------------
    '-- make this into a subroutine?!?!

   LPersonnelMenu:
    Cls
    If useVGA = 1 Then Screen menu&
    TG = 0
    PG = 0
    RG = 0
    Color 12: Print Tab(33); "PERSONNEL MENU": Print
    Color 14: Print "(0) ";
    Color 15: Print "RESUME PLAY": Print
    Color 14: Print "(1) ";
    Color 15: Print "RELIEF PITCHER": Print
    Color 14: Print "(2) ";
    Color 15: Print "CHANGE BATTER/FIELDER"

    15501 '
    Do
        I$ = GetKeyPress$
        I = Val(I$)
        I5 = 0
    Loop Until I >= 0 And I <= 2

    'This I has to do with the above key loop
    Select Case I + 1

        Case 1
            15580 '
            For I = 1 To 9
                If fldPos(P9, I) <> 1 Then
                    batRating(P9, lineupPlayer(P9, I), SS_B_Avail) = 1
                End If
            Next I

            If useVGA = 1 Then
                Call LOADSCREEN
                Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Else
                Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            End If

            GoTo LNextBatter1

        Case 2
           LChangePitcher:
            If P2 = OFF_PERSONNEL Then Call TIRING(P9): GoTo 15501

            Do

                Call PITDISP(P9)
                Locate LAST_ROW, 1: Print "SELECT RELIEVER (Z = NO CHANGE)   ";
                If fielder(P9, 1) <> 99 Then Call TIRING(P9)
                Print

                Do

                    I$ = GetKeyPress$
                    If UCase$(I$) = "Z" Then
                        GoTo LPersonnelMenu
                    Else
                        Call LetterToNumber(I$, I1)
                    End If

                Loop Until I1 <= 21 And pitchRating(P9, I1, SS_P_Avail) <= 0

                Locate 23, 1: Print String$(78, 32)
                Locate 23, 1: Print "YOU CHOSE "; pitchers$(P9, I1); " "; handed$(pitchRating(P9, I1, SS_P_Throws) + 2); " - ";

                Print "ARE YOU SURE? (Y/N)";
                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "Y"

            PPH = 0
            PR% = 0

            If S1%(P9) <> -1 And gamePitcher(P9) > 1 Then Call OFFPIT(P9, currPitcher(P9), 22)

            currPitcher(P9) = I1
            gamePitcher(P9) = gamePitcher(P9) + 1
            pitchRating(P9, I1, SS_P_Avail) = gamePitcher(P9)
            Call PITSTAM(P9)
            Call CHECKSAVE(D, P, baseRunners)

            15518 '
            P9 = 1 - P9
            W5 = P9
            W6 = W5
            VV(P9) = 1
            VV1(P9) = 9
            Call HITTINGLINES

            P9 = 1 - P9

            If Not (desigHit = 1 Or I5 = 66 Or I5 = 77) Then

                'No Designated Hitter

                Do

                    Cls
                    Locate , 14
                    Color 14: Print "DO YOU WISH TO MAKE A FLIP FLOP OR DOUBLE SWITCH? (Y/N)";
                    Color 15

                    I$ = GetKeyPress$

                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then

                    Cls
                    Call DisplayBatter_Lineup(PG, P9)

                    Call CHANGELINEUP(P9, P)

                    Do
                        Locate 12, 30: Print "LINEUP SLOT FOR PITCHER TO BAT IN (1-9)"

                        I$ = GetKeyPress$
                        If Asc(I$) = 32 And PG = 0 Then PG = 1: Cls: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
                        If Asc(I$) = 32 And PG = 1 Then PG = 0: Cls: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
                        If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
                        If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
                        V2 = Val(I$)
                    Loop Until V2 >= 1 And V2 <= 9


                    Do
                        Locate 13, 30
                        Print "WHO WILL REPLACE "; batters$(D, lineupPlayer(D, V2)); " @ "; fieldPos$(fldPos(D, V2));

                        I$ = GetKeyPress$
                        If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9)
                        If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9)

                        If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG)
                        If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9)

                        Call LetterToNumber(I$, V3)

                    Loop Until V3 <= 22

                    Locate 16, 29: Print batters$(D, V3)
                    Locate 17, 29: Print "ARE YOU SURE? (Y/N)";

                    I$ = GetKeyPress$

                    If UCase$(I$) = "Y" Then

                        gameD0(D) = gameD0(D) + 1
                        X0%(D, 0, gameD0(D)) = V3
                        X0%(D, 1, gameD0(D)) = fldPos(D, V2)

                        For V0 = 1 To 9
                            If fldPos(D, V0) = 1 Then X0%(D, 2, gameD0(D)) = (V0 * 10) + (gameD0(D) - 9)
                        Next V0

                        For V0 = 1 To 9
                            If fldPos(D, V0) = 1 Then
                                fldPos(D, V0) = fldPos(D, V2)
                                lineupPlayer(D, V0) = V3
                                fielder(D, fldPos(D, V0)) = V3
                                P9 = D
                                W5 = D
                                W6 = D
                                VV(D) = V0
                                VV1(D) = V0
                                Call HITTINGLINES
                            End If
                            VV(D) = 1
                            VV1(D) = 9
                        Next V0

                        batRating(D, V3, SS_B_Avail) = 1
                        I7% = D
                        I8% = V2
                        I9% = V3

                        Call REPLACEMENTS(I7%, I8%, I9%)

                        I5 = 0

                        For V0 = 1 To 9
                            If V0 = V2 Then
                                fldPos(D, V2) = 1
                                lineupPlayer(D, V2) = currPitcher(D)
                                fielder(D, 1) = currPitcher(D)
                                VV(D) = V0
                                VV1(D) = V0
                                Call HITTINGLINES
                            End If
                        Next V0

                        VV(D) = 1
                        VV1(D) = 9

                        GoTo LPersonnelMenu 'Back to personnel menu

                    Else

                        GoTo 15518

                    End If

                End If

            End If

            I5 = 0

            For V0 = 1 To 9

                If fldPos(D, V0) = 1 Or fldPos(D, V0) = 10 Then

                    lineupPlayer(D, V0) = currPitcher(D)
                    fielder(D, 1) = currPitcher(D)
                    fldPos(D, V0) = 1
                    P9 = D
                    W5 = D
                    W6 = D
                    VV(D) = V0
                    VV1(D) = V0

                    Call HITTINGLINES

                    VV(D) = 1
                    VV1(D) = 9
                    GoTo LPersonnelMenu

                End If

            Next V0

            GoTo LChangeBatterFielder

        Case 3

            '--- START OF A SHORT SUB
            '-- ChangeBatterFielder

           LChangeBatterFielder:
            Cls
            Call DisplayBatter_Lineup(PG, P9)
            Call CHANGELINEUP(P9, P)

            Locate 14, 29
            Color 12: Print "SELECT:"
            Locate , 30
            Color 14: Print " (1) ";
            Color 15: Print "SELECT A PINCH-HITTER"
            Locate , 30
            Color 14: Print " (2) ";
            Color 15: Print "SWAP POSITIONS BETWEEN PLAYERS"
            Locate , 30
            Color 14: Print " (3) ";
            Color 15: Print "SUBSTITUTE PLAYER IN LINEUP"
            Locate , 30
            Color 14: Print " (Z) ";
            Color 15: Print "RESUME PLAY"
            Locate , 30
            Color 14: Print "<SPACE> ";
            Color 15: Print "TOGGLE ROSTER";
            Color 14: Print " <TAB> ";
            Color 15: Print "TOGGLE FIELDING/BATTING"

            15547 '
            I$ = GetKeyPress$
            If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): GoTo 15547
            If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): GoTo 15547
            If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): GoTo 15547
            If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): GoTo 15547
            If UCase$(I$) = "Z" Then Goto LPersonnelMenu
            I1 = Val(I$)

            If I1 = 2 Then

                15567 '
                Do
                    Locate 20, 29: Print "B.O. #1 (1-9)"
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 1 And I1 <= 9 Or fldPos(P9, I1) <> 1

                Do
                    Locate 21, 29: Print "B.O. #2 (1-9)"
                    I$ = GetKeyPress$
                    I2 = Val(I$)
                Loop Until I1 >= 1 And I1 <= 9 Or fldPos(P9, I2) <> 1

                I3 = fldPos(P9, I2)
                fldPos(P9, I2) = fldPos(P9, I1)
                fldPos(P9, I1) = I3
                fielder(P9, fldPos(P9, I1)) = lineupPlayer(P9, I1)
                fielder(P9, fldPos(P9, I2)) = lineupPlayer(P9, I2)

                I7% = P9
                I8% = I2
                I9% = lineupPlayer(P9, I2)
                Call REPLACEMENTS(I7%, I8%, I9%)

                I7% = P9
                I8% = I1
                I9% = lineupPlayer(P9, I1)
                Call REPLACEMENTS(I7%, I8%, I9%)

                GoTo LChangeBatterFielder

            End If

            If I1 = 3 Then 15550
            If D2 = DEF_PERSONNEL Then 15550
            I2 = currLineupSlot(P): GoTo 15555

            For I5 = 20 To 23: Locate I5, 28
                Print Space$(52)
            Next I5

            Return

            '--- END OF A SHORT SUB

    End Select

    15550 '
    Do
        Locate 20, 30: Print "LINEUP SLOT TO REPLACE (1-9) (Z)=NO CHANGE"

        I$ = GetKeyPress$
        If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
        If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
        If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
        If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
        If UCase$(I$) <> "Z" Then I2 = Val(I$)

    Loop Until I2 >= 1 And I2 <= 9 And Not (P9 = D And fldPos(P9, I2) = 1) Or UCase$(I$) = "Z"


    If UCase$(I$) <> "Z" Then

        Locate , 30
        If fldPos(P9, I2) = 1 Then Print pitchers$(P9, currPitcher(P9));
        If fldPos(P9, I2) <> 1 Then Print batters$(P9, lineupPlayer(P9, I2));

        15555 '
        Do
            Locate 22, 30: Print "# OF SUB"

            I$ = GetKeyPress$
            If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            Call LetterToNumber(I$, I1)

        Loop Until I1 <= 22 Or batRating(P9, I1, SS_B_Avail) <= 0

        Do
            Locate 23, 30
            Print batters$(P9, I1); "... ARE YOU SURE? (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"


        If UCase$(I$) <> "N" Then

            batRating(P9, I1, SS_B_Avail) = 1
            lineupPlayer(P9, I2) = I1
            fielder(P9, fldPos(P9, I2)) = I1

            If fldPos(P9, I2) = 1 Or fldPos(P9, I2) = 10 Then fielder(P9, 1) = 99: fielder(P9, 0) = I2: fldPos(P9, I2) = 10

            gameD0(P9) = gameD0(P9) + 1
            X0%(P9, 0, gameD0(P9)) = lineupPlayer(P9, I2)
            X0%(P9, 1, gameD0(P9)) = fldPos(P9, I2)
            X0%(P9, 2, gameD0(P9)) = (I2 * 10) + (gameD0(P9) - 9)
            W5 = P9
            W6 = P9
            VV(P9) = I2
            VV1(P9) = I2
            I7% = P9
            I8% = I2
            I9% = I1

            Call REPLACEMENTS(I7%, I8%, I9%)
            Call HITTINGLINES

            VV(P9) = 1
            VV1(P9) = 9

        End If

    End If

    GoTo LChangeBatterFielder

    '----- NOODLE -----

    '----------------------------------------
    ' Print final score and run post game
    ' routines (scoreboard, stats, etc.)
    '----------------------------------------

   LGameOver:

    normalFinish = 1

    'Print winning team and score
    If gameScore(0, 0) > gameScore(1, 0) Then
        Call PBP(gameTeams$(0) + " defeats " + gameTeams$(1) + Str$(gameScore(0, 0)) + " TO" + Str$(gameScore(1, 0)) + "!")
    Else
        Call PBP(gameTeams$(1) + " defeats " + gameTeams$(0) + Str$(gameScore(1, 0)) + " TO" + Str$(gameScore(0, 0)) + "!")
    End If

    If autoPlay <> 1 Or playerMode <> 2 Then
        If pbpDelay! = 0 Then pbpDelay! = 0.1
        'Delay of 0 will not present the text
        'But we want to be sure the user sees it
        Call PBP("PRESS ANY KEY TO CONTINUE")
        I$ = GetKeyPress$
    End If

    Call EndOfGame
    If Environ$("PBPLOG") = "TRUE" Then Close #11

    Exit Sub


    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not yet been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    '-- This can join the code above as there's only one branch to it
   LEndOfGame:
    runnerHeld = 0
    If useVGA = 1 Then
        Call HOLDSTATUS_G(runnerHeld)
    Else
        Call HOLDSTATUS(runnerHeld)
    End If

    A1 = gameScore(D, 0) - gameScore(P, 0)

    If Not (A1 > 1 Or baseRunners = 3 Or baseRunners >= 6) Then

        If baseRunners <> 1 And baseRunners <> 5 Then
            I2 = 2
        Else

            I2 = 1
            J = b2Base(I2)

            If J <> 0 Then

                If fldPos(P, J) <> 1 Then

                    If fldPos(P, J) <> 1 And batRating(P, lineupPlayer(P, J), SS_B_SB) >= 10 Then
                        runnerHeld = 1
                        If useVGA = 1 Then
                            Call HOLDSTATUS_G(runnerHeld)
                        Else
                            Call HOLDSTATUS(runnerHeld)
                        End If
                    End If

                End If

            End If

        End If

    End If

    If SU(D, 0) <> -1 And teamMgrProfileVal(D, 589) = 0 And INNING% = teamMgrProfileVal(D, 588) Then Call PITSTRAT(PPH, pz)
    If SU(D, 1) <> -1 And teamMgrProfileVal(D, 589) = 0 And INNING% > teamMgrProfileVal(D, 588) Then Call PITSTRAT(PPH, pz)
    If SU(D, 1) <> -1 And teamMgrProfileVal(D, 589) <> 0 And INNING% >= teamMgrProfileVal(D, 588) Then Call PITSTRAT(PPH, pz)

    If pz = 1 Then pz = 0: GoTo LManageDefense

    A1 = gameScore(D, 0) - gameScore(P, 0)
    Call Rolld100(RN0)

    If Not (INNING% < 9 Or CZ%(D) = -1 Or teamMgrProfileVal(D, 581) = 999) Then

        CZ%(D) = -1

        If A1 <= 3 And A1 >= 0 Then

            If gamePitcher(D) <> 1 Then

                If d100% <= 80 And pitchRating(D, currPitcher(D), 3) < 15 Then
                    GK%(D) = 1
                End If

            Else

                ' To avoid division by zero
                '-- Only in QB64PE version 4.0
                'pitAvg1 = _IIF(pitchRating(D, currPitcher(D), 1) > 0, CInt(pitchRating(D, currPitcher(D), 16) / pitchRating(D, currPitcher(D), 1) * 100), 0)
                'pitAvg5 = _IIF(pitchRating(D, currPitcher(D), 5) > 0, CInt(pitchRating(D, currPitcher(D), 16) / pitchRating(D, currPitcher(D), 5) * 100), 0)
                If pitchRating(D, currPitcher(D), 1) > 0 Then
                    pitAvg1 = CInt(pitchRating(D, currPitcher(D), 16) / pitchRating(D, currPitcher(D), 1) * 100)
                Else
                    pitAvg1 = 0
                End If

                If pitchRating(D, currPitcher(D), 5) > 0 Then
                    pitAvg5 = CInt(pitchRating(D, currPitcher(D), 16) / pitchRating(D, currPitcher(D), 5) * 100)
                Else
                    pitAvg5 = 0
                End If

                If gameScore(P, 0) = 0 And pitchRating(D, currPitcher(D), 5) > 0 And RN0 <= pitAvg5 + 10 Then
                    GoTo LReliever1
                End If

                If pitchRating(D, currPitcher(D), 5) > 0 And RN0 <= pitAvg5 Then
                    GoTo LReliever1
                End If

                If pitchEraOpt > 0 And INNING% > 7 And gameScore(D, 0) > gameScore(P, 0) And pitchRating(D, currPitcher(D), 1) > 0 And gamePitcher(D) = 1 And pitAvg1 <= 100 And RN0 <= pitAvg1 Then
                    GoTo LReliever1
                End If

                If pitchEraOpt > 0 And INNING% > 7 And (gameScore(D, 0) >= gameScore(P, 0) Or gameScore(P, 0) - gameScore(D, 0) <= 2) And pitchRating(D, currPitcher(D), 1) > 0 And gamePitcher(D) = 1 And pitAvg1 >= 100 Then
                    GoTo LReliever1
                End If

                If pitchEraOpt >= 8 Then
                    GoTo LReliever1
                End If

                GK%(D) = 1  'new reliever'

            End If

        End If

    End If

   LReliever1:
    If GK%(D) = 1 Then

        GK%(D) = 0
        Call RELIEVERS(PPH, pz)
        If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
        GoTo LManageDefense

    End If

    If fldPos(P, currLineupSlot(P)) = 1 Then BR = 0
    If fldPos(P, currLineupSlot(P)) <> 1 Then BR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt)

    If pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then
        I7! = pitchRating(I0, currPitcher(I0), SS_P_BAvg)
    Else
        If pitchRating(P, currPitcher(P), SS_P_H_AB) = 0 Then
            I7! = 0
        Else
            I7! = CInt((pitchRating(P, currPitcher(P), SS_P_H_H) / pitchRating(P, currPitcher(P), SS_P_H_AB) * 1000))
        End If
    End If

    D2 = DEF_PITCH

    If INFPOS% <= 0 Then

        If teamMgrProfileVal(D, 690) = 999 Then

            If Not (baseRunners < 3 Or baseRunners = 4) Then

                ' PROFILE INFIELD IN WITH 3RD OCCUPIED
                Call Rolld100(RN)
                If nbrOuts(0) = 0 And A1 >= teamMgrProfileVal(D, 691) * -1 And A1 <= teamMgrProfileVal(D, 692) * -1 And INNING% >= teamMgrProfileVal(D, 693) And RN <= teamMgrProfileVal(D, 694) Then INFPOS% = INF_IN: D2 = DEF_INFIELD_IN: GoTo LManageDefense
                If nbrOuts(0) = 0 And A1 >= teamMgrProfileVal(D, 695) * -1 And A1 <= teamMgrProfileVal(D, 696) * -1 And INNING% >= teamMgrProfileVal(D, 697) And RN <= teamMgrProfileVal(D, 698) Then INFPOS% = INF_IN: D2 = DEF_INFIELD_IN: GoTo LManageDefense
                If nbrOuts(0) = 1 And (baseRunners = 5 Or baseRunners = 7) And A1 >= teamMgrProfileVal(D, 699) * -1 And A1 <= teamMgrProfileVal(D, 700) * -1 And INNING% >= teamMgrProfileVal(D, 701) And RN <= teamMgrProfileVal(D, 702) Then INFPOS% = INF_IN: D2 = DEF_INFIELD_IN: GoTo LManageDefense
                If nbrOuts(0) = 1 And (baseRunners = 5 Or baseRunners = 7) And A1 >= teamMgrProfileVal(D, 703) * -1 And A1 <= teamMgrProfileVal(D, 704) * -1 And INNING% >= teamMgrProfileVal(D, 705) And RN <= teamMgrProfileVal(D, 706) Then INFPOS% = INF_IN: D2 = DEF_INFIELD_IN: GoTo LManageDefense

            End If

        End If

        If teamMgrProfileVal(D, 707) = 999 Then

            If baseRunners = 0 Then

                ' PROFILE CORNERS IN AGAINST A BUNT HIT
                Call Rolld100(RN)
                If BR >= teamMgrProfileVal(D, 708) And RN <= teamMgrProfileVal(D, 709) Or BR >= teamMgrProfileVal(D, 710) And RN <= teamMgrProfileVal(D, 711) Or BR >= teamMgrProfileVal(D, 712) And RN <= teamMgrProfileVal(D, 713) Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If

            End If

        End If

        If teamMgrProfileVal(D, 714) = 999 And fldPos(P, currLineupSlot(P)) <> 1 Then

            If nbrOuts(0) = 0 Then

                If Not (baseRunners <> 1 And baseRunners <> 4) Then

                    ' PROFILE CORNERS IN VS HITTER IN SAC BUNT SITUATION
                    Call Rolld100(RN)
                    If A1 <= teamMgrProfileVal(D, 715) And A1 >= teamMgrProfileVal(D, 716) * -1 And baseRunners = 1 And BR >= teamMgrProfileVal(D, 717) And RN <= teamMgrProfileVal(D, 718) Then
                        INFPOS% = INF_CORNERS_IN
                        D2 = DEF_CORNERS_IN
                        GoTo LManageDefense
                    End If
                    If A1 <= teamMgrProfileVal(D, 719) And A1 >= teamMgrProfileVal(D, 720) * -1 And baseRunners = 1 And BR >= teamMgrProfileVal(D, 721) And RN <= teamMgrProfileVal(D, 722) Then
                        INFPOS% = INF_CORNERS_IN
                        D2 = DEF_CORNERS_IN
                        GoTo LManageDefense
                    End If
                    If A1 <= teamMgrProfileVal(D, 723) And A1 >= teamMgrProfileVal(D, 724) * -1 And baseRunners = 4 And BR >= teamMgrProfileVal(D, 725) And RN <= teamMgrProfileVal(D, 726) Then
                        INFPOS% = INF_CORNERS_IN
                        D2 = DEF_CORNERS_IN
                        GoTo LManageDefense
                    End If
                    If A1 <= teamMgrProfileVal(D, 727) And A1 >= teamMgrProfileVal(D, 728) * -1 And baseRunners = 4 And BR >= teamMgrProfileVal(D, 729) And RN <= teamMgrProfileVal(D, 730) Then
                        INFPOS% = INF_CORNERS_IN
                        D2 = DEF_CORNERS_IN
                        GoTo LManageDefense
                    End If

                End If

            End If

        End If

        If Not (teamMgrProfileVal(D, 731) <> 999 Or fldPos(P, currLineupSlot(P)) <> 1 Or nbrOuts(0) = 2) Then

            If Not (baseRunners <> 1 And baseRunners <> 2 And baseRunners <> 4) Then

                ' PROFILE CORNERS IN VS A PITCHER IN SAC BUNT SITUATION
                Call Rolld100(RN)

                If I7! <= teamMgrProfileVal(D, 732) And RN <= teamMgrProfileVal(D, 733) And baseRunners = 1 And nbrOuts(0) <= 1 Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If

                If I7! <= teamMgrProfileVal(D, 734) And RN <= teamMgrProfileVal(D, 735) And baseRunners = 1 And nbrOuts(0) <= 1 Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If

                If I7! <= teamMgrProfileVal(D, 736) And RN <= teamMgrProfileVal(D, 737) And (baseRunners = 2 Or baseRunners = 4) And nbrOuts(0) = 0 Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If

                If I7! <= teamMgrProfileVal(D, 738) And RN <= teamMgrProfileVal(D, 739) And (baseRunners = 2 Or baseRunners = 4) And nbrOuts(0) = 0 Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If

            End If

        End If

        If teamMgrProfileVal(D, 740) = 999 Then

            If Not (nbrOuts(0) = 2 Or baseRunners <> 3 And baseRunners < 5) Then

                ' PROFILE CORNERS IN FOR A SQUEEZE
                Call Rolld100(RN)
                If nbrOuts(0) = 0 And A1 <= teamMgrProfileVal(D, 741) And A1 >= teamMgrProfileVal(D, 742) And BR >= teamMgrProfileVal(D, 743) And RN <= teamMgrProfileVal(D, 744) Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If
                If nbrOuts(0) = 1 And A1 <= teamMgrProfileVal(D, 745) And A1 >= teamMgrProfileVal(D, 746) And BR >= teamMgrProfileVal(D, 747) And RN <= teamMgrProfileVal(D, 748) Then
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If


            End If

        End If

        If teamMgrProfileVal(D, 756) = 999 Then

            If Not (baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7) Then

                MOB% = 0

                For I = 1 To 3
                    If b2Base(I) > 0 Then MOB% = MOB% + 1
                Next

                If A1 + MOB% + 1 < 0 Then
                    ' BATTER NOT TYING OR WINNING RUN
                    Call Rolld100(RN)

                    If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                        I7! = 0
                        I6! = 0
                    Else
                        I6! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                        If I6! <= teamMgrProfileVal(D, 757) And INNING% >= teamMgrProfileVal(D, 758) And RN <= teamMgrProfileVal(D, 759) Then
                            D2 = DEF_PITCH_AROUND
                            GoTo LReliever2
                        End If
                        If I6! <= teamMgrProfileVal(D, 760) And INNING% >= teamMgrProfileVal(D, 761) And RN <= teamMgrProfileVal(D, 762) Then
                            D2 = DEF_PITCH_AROUND
                            GoTo LReliever2
                        End If
                        If I6! <= teamMgrProfileVal(D, 763) And INNING% >= teamMgrProfileVal(D, 764) And RN <= teamMgrProfileVal(D, 765) Then
                            D2 = DEF_PITCH_AROUND
                            GoTo LReliever2
                        End If
                    End If

                End If

            End If

        End If

        A1 = gameScore(D, 0) - gameScore(P, 0)

        If A1 >= -1 And A1 <= 1 And INNING% >= 8 And (baseRunners = 3 Or baseRunners >= 5) And nbrOuts(0) < 2 Then
            INFPOS% = INF_IN
            D2 = DEF_INFIELD_IN

        Else

            If A1 > 0 Or nbrOuts(0) > 0 Or baseRunners <= 2 Or baseRunners = 4 Or baseRunners = 5 And nbrOuts(0) = 1 Or INNING% <= 5 And A1 < -2 Or INNING% > 5 And A1 < -4 Then

                If Not (baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners >= 5 Or nbrOuts(0) = 2 Or currLineupSlot(P) >= 3 And currLineupSlot(P) <= 6 Or INNING% <= 6 And A1 >= 3 Or INNING% <= 6 And A1 <= -3) Then
                    If Not (INNING% > 7 And A1 >= 1 Or INNING% > 7 And A1 < -1) Then
                        INFPOS% = INF_CORNERS_IN
                        D2 = DEF_CORNERS_IN
                    End If
                End If
            Else
                INFPOS% = INF_IN
                D2 = DEF_INFIELD_IN
            End If

        End If

        GoTo LManageDefense

    End If

    If Not (A1 <= 0 Or A1 = 0 And baseRunners <= 1 Or A1 = 0 And baseRunners = 4 Or baseRunners = 7 Or fldPos(P, currLineupSlot(P)) = 1) Then

        If INNING% >= 9 And P = 1 And nbrOuts(0) < 2 And A1 = 0 And baseRunners >= 2 And baseRunners <= 3 Or INNING% >= 9 And P = 1 And nbrOuts(0) = 2 And A1 = 0 And baseRunners >= 5 And baseRunners <= 6 Then
            '18835
            D2 = DEF_INTNTL_BB
            GoTo LManageDefense
        End If

        Ib4 = game_batRating(P, GM_BAvg, currLineupSlot(P))
        I1 = game_batRating(P, GM_HR, currLineupSlot(P))
        If currLineupSlot(P) = 9 Then
            I2 = game_batRating(P, GM_BAvg, 0)
            I3 = game_batRating(P, GM_HR, 0)
        Else
            I2 = game_batRating(P, GM_BAvg, currLineupSlot(P) + 1)
            I3 = game_batRating(P, GM_HR, currLineupSlot(P) + 1)
        End If

        If baseRunners = 2 Or baseRunners = 6 Then

            If Ib4 - I2 >= 50 And I1 - I3 >= 50 And A1 < 1 And INNING% > 8 Or Ib4 >= 280 And I1 >= 140 And INNING% > 8 And I1 > I3 And A1 < 1 Then
                D2 = DEF_INTNTL_BB
                GoTo LManageDefense
            End If

        End If

    End If

   LReliever2:
    If (S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2)) Or fielder(D, 1) = 99 Then
        Call RELIEVERS(PPH, pz)
        If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
    End If

    GoTo LManageDefense


    '----- NOODLE -----


   LPhForPosition: 'GET A PINCH HITTER FOR A POSITION PLAYER
    If teamMgrProfileVal(P, 540) = 999 Then

        'SELECT PH FOR HITTER
        For I = 0 To 22

            If batRating(P, I, SS_B_Avail) <= 0 And batters$(P, I) <> "XXX" Then

                If fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos1) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos2) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos3) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos4) Then

                    I1 = 6
                    If pitchRating(D, currPitcher(D), SS_P_Throws) < 0 Then I1 = 1

                    For I = I1 To I1 + 4

                        If teamMgrProfileVal(P, I1 + 540) <> -1 And batRating(P, teamMgrProfileVal(P, I1 + 540), SS_B_Avail) = 0 Then
                            teamMgrProfileVal = teamMgrProfileVal(P, I1 + 540)
                            teamMgrProfileVal(P, I1 + 540) = -1

                            PPH = 1
                            gameD0(P) = gameD0(P) + 1
                            X0%(P, 0, gameD0(P)) = teamMgrProfileVal
                            X0%(P, 1, gameD0(P)) = fldPos(P, currLineupSlot(P))
                            X0%(P, 2, gameD0(P)) = (currLineupSlot(P) * 10) + (gameD0(P) - 9)
                            batRating(P, I, SS_B_Avail) = 1
                            lineupPlayer(P, currLineupSlot(P)) = teamMgrProfileVal
                            fielder(P, fldPos(P, currLineupSlot(P))) = teamMgrProfileVal

                            If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then
                                fielder(P, 1) = 99
                                fielder(P, 0) = currLineupSlot(P)
                                fldPos(P, currLineupSlot(P)) = 10
                            End If

                            I7% = P
                            I8% = currLineupSlot(P)
                            I9% = lineupPlayer(P, currLineupSlot(P))
                            Call REPLACEMENTS(I7%, I8%, I9%)

                            GoTo LPinchHitter

                        End If

                    Next

                End If

            End If

        Next

    End If

    If INNING% < 6 Then Goto LPinchRunner   ' [bm] KLUDGE: Don't pinch hit for a position player before the 6th inning

    For I = 0 To 22:

        If Not (batRating(P, I, SS_B_Avail) > 0 Or batters$(P, I) = "XXX" Or batRating(P, I, SS_B_AB) = 0) Then

            If Not (fldPos(P, currLineupSlot(P)) <> batRating(P, I, SS_B_Pos1) And fldPos(P, currLineupSlot(P)) <> batRating(P, I, SS_B_Pos2) And fldPos(P, currLineupSlot(P)) <> batRating(P, I, SS_B_Pos3) And fldPos(P, currLineupSlot(P)) <> batRating(P, I, SS_B_Pos4)) Then

                pinchBAvg = batRating(P, I,  SS_B_H) / batRating(P, I,  SS_B_AB) * 1000
                battrBAvg = batRating(P, JM, SS_B_H) / batRating(P, JM, SS_B_AB) * 1000
                abRand = Int(Rnd(1) * 500)  ' Randomize chance of PH entering game, based on his AB's in the season. This will limit players who hardly played.  (500 is arbitrary number)

                iPitThrows = pitchRating(D, currPitcher(D), SS_P_Throws)   ' pitcher throws L/R
                iBatHits   = batRating(P, I, SS_B_Bats)           ' batter hits L/Sw/R

                ' Pitcher and PH both righty
                If iPitThrows =  1 And iBatHits > 0 And pinchBAvg > battrBAvg + 35 Then
                    If abRand < batRating(P, I,  SS_B_AB) Then GoTo LPhForPosPlayer
                End If

                ' Pitcher and PH both lefty
                If iPitThrows = -1 And iBatHits < 0 And pinchBAvg > battrBAvg + 45 Then
                    If abRand < batRating(P, I,  SS_B_AB) Then GoTo LPhForPosPlayer
                End If

                ' Pitcher and PH on opposite sides
                If (iPitThrows = -1 And iBatHits >= 0 Or iPitThrows = 1 And iBatHits <= 0) And pinchBAvg > battrBAvg + 30 Then
                    If abRand < batRating(P, I,  SS_B_AB) Then GoTo LPhForPosPlayer
                End If

            End If

        End If

    Next I

    GoTo LPinchRunner


   LPhForPosPlayer:

    PPH = 1
    gameD0(P) = gameD0(P) + 1
    X0%(P, 0, gameD0(P)) = I
    X0%(P, 1, gameD0(P)) = fldPos(P, currLineupSlot(P))
    X0%(P, 2, gameD0(P)) = (currLineupSlot(P) * 10) + (gameD0(P) - 9)
    batRating(P, I, SS_B_Avail) = 1
    lineupPlayer(P, currLineupSlot(P)) = I
    fielder(P, fldPos(P, currLineupSlot(P))) = I

    If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then 
        fielder(P, 1) = 99
        fielder(P, 0) = currLineupSlot(P)
        fldPos(P, currLineupSlot(P)) = 10
    End if

    I7% = P
    I8% = currLineupSlot(P)
    I9% = lineupPlayer(P, currLineupSlot(P))
    Call REPLACEMENTS(I7%, I8%, I9%)

    GoTo LPinchHitter


    '----- NOODLE -----

   LPhForPitcher: 'GET A PINCH HITTER FOR THE PITCHER

    callPinch = 0
    skipLoop = 0

    If teamMgrProfileVal(P, 540) <> 999 Then

        J% = 0
        GP% = 0

        ' To avoid division by zero
        '_IIF only in QB64PE v4.0
        'pitAvg = _IIF( pitchRating(P, currPitcher(P), 1) > 0, CInt( pitchRating(P, currPitcher(P), 16) / pitchRating(P, currPitcher(P), 1) * 100 ), 0 )
        If pitchRating(P, currPitcher(P), 1) > 0 Then
            pitAvg = CInt(pitchRating(P, currPitcher(P), 16) / pitchRating(P, currPitcher(P), 1) * 100)
        Else
            pitAvg = 0
        End If

        If desigHit = 1 Or _
            (pitchEraOpt > 1 And INNING% < 4) Or _
            (pitchEraOpt > 2 And INNING% < 7) Or _
            (pitchEraOpt > 3 And INNING% < 9) Or _
            (pitchEraOpt >= 8) Or _
            ( pitchEraOpt > 0 And INNING% > 7 And gameScore(P, 0) > gameScore(D, 0) And _
            pitchRating(P, currPitcher(P), 1) > 0 And gamePitcher(P) = 1 And _
            pitAvg <= 100 And RN0 <= pitAvg ) Or _
            ( pitchEraOpt > 0 And INNING% > 7 And (gameScore(P, 0) >= gameScore(D, 0) Or _
            gameScore(D, 0) - gameScore(P, 0) <= 2) And pitchRating(P, currPitcher(P), 1) > 0 And _
            gamePitcher(P) = 1 And pitAvg >= 100 ) Then
            skipLoop = 1
        End If

        If skipLoop = 0 Then

            For Idx = 0 To 22

                If Not (batters$(P, Idx) = "XXX" Or batRating(P, Idx, SS_B_Avail) > 0 Or batRating(P, Idx, SS_B_AB) = 0) Then

                    If batRating(P, Idx, SS_B_Bats) > 0 And pitchRating(D, currPitcher(D), SS_P_Throws) = 1 Or batRating(P, Idx, SS_B_Bats) < 0 And pitchRating(D, currPitcher(D), SS_P_Throws) = -1 Then
                        PH%(J%, 0) = CInt(batRating(P, Idx, SS_B_G) / 4)
                    Else
                        PH%(J%, 0) = batRating(P, Idx, SS_B_G)
                    End If

                    PH%(J%, 1) = Idx
                    GP% = GP% + PH%(J%, 0)
                    J% = J% + 1

                End If

            Next

            If J% <> 0 Then

                RN = Int(Rnd(1) * GP%) + 1

                For I = 0 To J% - 1

                    If RN <= PH%(I, 0) Then
                        PH% = PH%(I, 1)
                        callPinch = 1
                        Exit For
                    Else
                        PH%(I + 1, 0) = PH%(I, 0) + PH%(I + 1, 0)
                    End If

                Next

            End If

        End If 'Done checking of we should skip the loop

        If callPinch = 1 Then

            PPH = 1
            gameD0(P) = gameD0(P) + 1
            X0%(P, 0, gameD0(P)) = PH%
            X0%(P, 1, gameD0(P)) = 10
            X0%(P, 2, gameD0(P)) = 81 + gameD0(P)
            batRating(P, PH%, SS_B_Avail) = 1
            lineupPlayer(P, currLineupSlot(P)) = PH%

            If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then 
                fielder(P, 1) = 99
                fielder(P, 0) = currLineupSlot(P)
                fldPos(P, currLineupSlot(P)) = 10
            End If

           LPinchHitter:
            P9 = P
            W5 = P9
            W6 = P9
            VV(P9) = currLineupSlot(P)
            VV1(P9) = currLineupSlot(P)
            Call HITTINGLINES

            If useVGA = 1 Then
                Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
            Else
                Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
            End If

            VV(P9) = 1
            VV1(P9) = 9

            player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))

            If fldPos(P, currLineupSlot(P)) = 1 Then player$(0) = pitchers$(P, currPitcher(D))
            Call WINDEX
            Call PBP(gameManagers$(P) + " is sending up a pinch hitter")
            PPH = 1

            GoTo LContinueBatter

        End If

       LPinchRunner:
        If PR% <> 1 Then
            Call PINCHRUNNER(MS%, A1, baseRunners, I2, P, PR%, nbrBalls, nbrStrikes, D, MO)
        End If

        If MS% = 1 Then
            MS% = 0
            GoTo LContinueBatter
        End If

        If fldPos(P, currLineupSlot(P)) = 1 Then
            BR = 0
        Else
            BR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt)
        End If


        Call Rolld100(RN)
        If baseRunners = 0 And (INFPOS% > INF_BACK And RN <= 5 Or INFPOS% = INF_BACK And RN <= 5) And BR > 8 Then
            P2 = OFF_BUNT
            GoTo LBunt
        End If

        ' MANAGER PROFILE TO HIT AND RUN
        Call Rolld100(RN)
        If Not (baseRunners = 0 Or teamMgrProfileVal(P, 671) <> 999 Or fldPos(P, currLineupSlot(P)) = 1) Then

            If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                I7! = 0
            Else
                I7! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
            End If

            If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_K) = 0 Then
                I6! = 0
            Else
                I6! = CInt(batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_K))
            End If

            If baseRunners = 1 And nbrOuts(0) = 0 And A1 <= teamMgrProfileVal(P, 672) And A1 >= teamMgrProfileVal(P, 673) * -1 And RN <= teamMgrProfileVal(P, 674) And I7! <= teamMgrProfileVal(P, 872) And I6! >= teamMgrProfileVal(P, 866) Then
                P2 = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If
            If baseRunners = 1 And nbrOuts(0) = 1 And A1 <= teamMgrProfileVal(P, 675) And A1 >= teamMgrProfileVal(P, 676) * -1 And RN <= teamMgrProfileVal(P, 677) And I7! <= teamMgrProfileVal(P, 873) And I6! >= teamMgrProfileVal(P, 867) Then
                P2 = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If
            If baseRunners = 5 And nbrOuts(0) = 0 And A1 <= teamMgrProfileVal(P, 678) And A1 >= teamMgrProfileVal(P, 679) * -1 And RN <= teamMgrProfileVal(P, 680) And I7! <= teamMgrProfileVal(P, 874) And I6! >= teamMgrProfileVal(P, 868) Then
                P2 = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If
            If baseRunners = 5 And nbrOuts(0) = 1 And A1 <= teamMgrProfileVal(P, 681) And A1 >= teamMgrProfileVal(P, 682) * -1 And RN <= teamMgrProfileVal(P, 683) And I7! <= teamMgrProfileVal(P, 875) And I6! >= teamMgrProfileVal(P, 869) Then
                P2 = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If
            If baseRunners = 4 And nbrOuts(0) = 0 And A1 <= teamMgrProfileVal(P, 684) And A1 >= teamMgrProfileVal(P, 685) * -1 And RN <= teamMgrProfileVal(P, 686) And I7! <= teamMgrProfileVal(P, 876) And I6! >= teamMgrProfileVal(P, 870) Then
                P2 = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If
            If baseRunners = 4 And nbrOuts(0) = 1 And A1 <= teamMgrProfileVal(P, 687) And A1 >= teamMgrProfileVal(P, 688) * -1 And RN <= teamMgrProfileVal(P, 689) And I7! <= teamMgrProfileVal(P, 877) And I6! >= teamMgrProfileVal(P, 871) Then
                P2 = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If

        End If

        If Not (teamMgrProfileVal(P, 609) <> 999 Or fldPos(P, currLineupSlot(P)) <> 1) Then

            If Not (baseRunners <> 1 And baseRunners <> 2 And baseRunners <> 4) Then

                Call Rolld100(RN)

                If baseRunners = 1 And INNING% <= 7 And teamMgrProfileVal(P, 610) >= nbrOuts(0) And A1 <= teamMgrProfileVal(P, 611) And A1 >= teamMgrProfileVal(P, 612) * -1 And RN <= teamMgrProfileVal(P, 613) Then 18970
                If baseRunners = 1 And INNING%  > 7 And teamMgrProfileVal(P, 614) >= nbrOuts(0) And A1 <= teamMgrProfileVal(P, 615) And A1 >= teamMgrProfileVal(P, 616) * -1 And RN <= teamMgrProfileVal(P, 617) Then 18970

                If baseRunners = 2 And INNING% <= 7 And teamMgrProfileVal(P, 618) >= nbrOuts(0) And A1 <= teamMgrProfileVal(P, 619) And A1 >= teamMgrProfileVal(P, 620) * -1 And RN <= teamMgrProfileVal(P, 621) Then 18970
                If baseRunners = 2 And INNING%  > 7 And teamMgrProfileVal(P, 622) >= nbrOuts(0) And A1 <= teamMgrProfileVal(P, 623) And A1 >= teamMgrProfileVal(P, 624) * -1 And RN <= teamMgrProfileVal(P, 625) Then 18970

                If baseRunners = 4 And INNING% <= 7 And teamMgrProfileVal(P, 626) >= nbrOuts(0) And A1 <= teamMgrProfileVal(P, 627) And A1 >= teamMgrProfileVal(P, 628) * -1 And RN <= teamMgrProfileVal(P, 629) Then 18970
                If baseRunners = 4 And INNING%  > 7 And teamMgrProfileVal(P, 630) >= nbrOuts(0) And A1 <= teamMgrProfileVal(P, 631) And A1 >= teamMgrProfileVal(P, 632) * -1 And RN <= teamMgrProfileVal(P, 633) Then 18970

            End If

        End If

        If teamMgrProfileVal(P, 634) = 999 And fldPos(P, currLineupSlot(P)) <> 1 And nbrOuts(0) = 0 Or (baseRunners <> 1 And baseRunners <> 2 And baseRunners <> 4) Then

            If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                I7! = 0
                I6! = 0
            Else
                I7! = CInt( batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                I6! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
            End If

            Call Rolld100(RN)
            If baseRunners = 1 And INNING% <= 7 And A1 <= teamMgrProfileVal(P, 635) And A1 >= teamMgrProfileVal(P, 636) * -1 And I7! <= teamMgrProfileVal(P, 637) And I6! <= teamMgrProfileVal(P, 638) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 639) And RN <= teamMgrProfileVal(P, 640) Then 18970
            If baseRunners = 2 And INNING% <= 7 And A1 <= teamMgrProfileVal(P, 641) And A1 >= teamMgrProfileVal(P, 642) * -1 And I7! <= teamMgrProfileVal(P, 643) And I6! <= teamMgrProfileVal(P, 644) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 645) And RN <= teamMgrProfileVal(P, 646) Then 18970
            If baseRunners = 4 And INNING% <= 7 And A1 <= teamMgrProfileVal(P, 647) And A1 >= teamMgrProfileVal(P, 648) * -1 And I7! <= teamMgrProfileVal(P, 649) And I6! <= teamMgrProfileVal(P, 650) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 651) And RN <= teamMgrProfileVal(P, 652) Then 18970

            If baseRunners = 1 And INNING% > 7 And A1 <= teamMgrProfileVal(P, 653) And A1 >= teamMgrProfileVal(P, 654) * -1 And I7! <= teamMgrProfileVal(P, 655) And I6! <= teamMgrProfileVal(P, 656) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 657) And RN <= teamMgrProfileVal(P, 658) Then 18970
            If baseRunners = 2 And INNING% > 7 And A1 <= teamMgrProfileVal(P, 659) And A1 >= teamMgrProfileVal(P, 660) * -1 And I7! <= teamMgrProfileVal(P, 661) And I6! <= teamMgrProfileVal(P, 662) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 663) And RN <= teamMgrProfileVal(P, 664) Then 18970
            If baseRunners = 4 And INNING% > 7 And A1 <= teamMgrProfileVal(P, 665) And A1 >= teamMgrProfileVal(P, 666) * -1 And I7! <= teamMgrProfileVal(P, 667) And I6! <= teamMgrProfileVal(P, 668) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 669) And RN <= teamMgrProfileVal(P, 670) Then 18970

        End If

        P2 = OFF_HIT_AWAY

        If baseRunners = 1 Or baseRunners = 5 Then

            If A1 <= -1 And batRating(P, lineupPlayer(P, b2Base(1)), SS_B_SB) < 10 Or b2Mystery(1) > 1 Then Goto LNoSteal  'CHANGED FROM <10
            If A1 >= -1 And baseRunners = 5 Then Goto LNoSteal
            If A1 < -5 Or fldPos(P, b2Base(1)) = 1 And baseRunners = 1 Or fldPos(P, b2Base(2)) = 1 And baseRunners = 2 Then Goto LNoSteal
            If A1 < 0 And INNING% >= 7 Then Goto LNoSteal
            P2 = OFF_STEAL

            GoTo LSteal

        End If

        If baseRunners = 2 Or baseRunners = 4 Then
            If Not (batRating(P, lineupPlayer(P, b2Base(2)), SS_B_SB) < 40 Or fldPos(P, currLineupSlot(P)) = 1 Or nbrOuts(0) = 2) Then
                If b2Mystery(2) <= 1 Then
                    P2 = OFF_STEAL
                    GoTo LSteal
                End If
            End If
        End If

       LNoSteal:
        If Not (baseRunners = 0 Or nbrOuts(0) > 0 Or baseRunners = 7 Or baseRunners = 6 Or baseRunners = 3 Or baseRunners = 5 Or nbrStrikes = 2 Or P = 0 And INNING% > 7) Then

            If fldPos(P, currLineupSlot(P)) = 1 And baseRunners = 1 And nbrOuts(0) < 2 Or fldPos(P, currLineupSlot(P)) = 1 And baseRunners = 2 And nbrOuts(0) = 0 Or fldPos(P, currLineupSlot(P)) = 1 And baseRunners = 4 And nbrOuts(0) = 0 Then Goto LPinchRunner1
            If currLineupSlot(P) > 2 And currLineupSlot(P) < 6 Or game_batRating(P, GM_BAvg, currLineupSlot(P)) > 290 And fldPos(P, currLineupSlot(P)) <> 1 Or fldPos(P, currLineupSlot(P)) <> 1 And game_batRating(P, GM_HR, currLineupSlot(P)) > 70 Then Goto LPinchRunner0
            If INNING% > 7 And A1 = -1 And nbrOuts(0) = 0 And baseRunners = 1 Then Goto LPinchRunner1

        End If

    End If

   LPinchRunner0:
    PPH = 0
    PR% = 0

    Call Rolld100(RN0)

    If nbrOuts(0) = 2 Or game_batRating(P, GM_K, currLineupSlot(P)) > 100 Or baseRunners = 0 Or baseRunners = 7 Or baseRunners = 6 Or baseRunners = 3 Or baseRunners = 2 Or A1 < -3 Or RN0 > 20 Then
        GoTo 15064
    Else
        P2 = OFF_HIT_N_RUN
        GoTo LHitAndRun
    End If

    '----- NOODLE -----

   LPinchRunner1:

    If nbrOuts(0) >= 1 And INNING% >= 7 Then
        GoTo LPinchRunner0
    Else
        18970 '
        P2 = OFF_BUNT
        GoTo LBunt
    End If


End Sub



'Include: 'GameRoutines.bm'


'----------------------------------------
'            INFO Subroutine
'----------------------------------------
'This subroutine displays a simple header
'and footer for the game options menu.
Sub INFO ()

    Cls

    Color , 1
    Locate 1, 1
    Print Tab(80); " ";

    Locate , 33
    Color 14
    Print "GAME OPTIONS MENU"

    Locate LAST_ROW, 1
    Print Tab(80); " ";
    Locate , 1
    Print "    LANCE HAFFNER / JASON LEONARD  1986-2025"; Tab(70); Left$(Time$, 5);
    Locate 3, 1
    Color , 0

End Sub


'----------------------------------------
'           InitVar Subroutine
'----------------------------------------
' Initializes variables for the game
' Does not process any that will be read
' in from file or assigned elsewhere
Sub InitVar ()

    Shared scheduleFile$
    Shared actualAttendance&, avgAttendance&
    Shared teamIndicator$()

    Shared DB, HR, TR, SB
    Shared E!

    Shared BP(), batLUIdx()
    Shared BB%(), gameMgrIdx(), mgrFile()
    Shared nbrInnings(), oppScore()
    Shared startPitcher()
    Shared SU(), teamScore()

    Shared statT0!(), statT1!()

    Shared dayNight$(), desigHitOpt$(), gameType$()
    Shared leagueType$(), locIndicator$()
    Shared month$(), oppName$(), pitcherEra$()
    Shared playMode$(), pitHand$(), umpireName$()
    Shared teamIndicator$()

    normalFinish = 0

    'Graphics co-ordinates originally were for a
    'routine that operated as if the screen were
    'still just 80 column width; so 80x8 = 640
    FontColAdj = 8

    teamIndicator$(0) = "VISITING TEAM"
    teamIndicator$(1) = "HOME TEAM"

    'temp$(1) = "HOT"
    'temp$(2) = "WARM"
    'temp$(3) = "COOL"
    'temp$(4) = "COLD"

    pitHand$(0) = "RIGHT"
    pitHand$(1) = "LEFT "

    windDirection$(2) = "LEFT TO RIGHT AT"
    windDirection$(3) = "RIGHT TO LEFT AT"
    windDirection$(4) = "BLOWING IN FROM LEFT AT"
    windDirection$(5) = "BLOWING IN FROM CENTER AT"
    windDirection$(6) = "BLOWING IN FROM RIGHT AT"
    windDirection$(7) = "BLOWING OUT TO LEFT AT"
    windDirection$(8) = "BLOWING OUT TO CENTER AT"
    windDirection$(9) = "BLOWING OUT TO RIGHT AT"

    skyCond$(1) = "CLEAR"
    skyCond$(2) = "PARTLY CLOUDY"
    skyCond$(3) = "OVERCAST"
    skyCond$(4) = "DOME"

    precip$(1) = "THUNDERSTORMS"
    precip$(2) = "SHOWERS"
    precip$(3) = "DRIZZLE"
    precip$(4) = "NONE"

    leagueType$(0) = "AL GAME         "
    leagueType$(1) = "NL GAME         "
    leagueType$(2) = "INTERLEAGUE GAME"

    desigHitOpt$(0) = "NO DH "
    desigHitOpt$(1) = "USE DH"

    umpireName$(0) = "AL"
    umpireName$(1) = "NL"
    umpireName$(2) = "NL"

    gameType$(0) = "LEAGUE GAME    "
    gameType$(1) = "EXHIBITION GAME"

    month$(4) = "APRIL"
    month$(5) = "MAY"
    month$(6) = "JUNE"
    month$(7) = "JULY"
    month$(8) = "AUGUST"
    month$(9) = "SEPTEMBER"
    month$(10) = "OCTOBER"
    gameMonth = 4

    dayNight$(0) = "AFTERNOON"
    dayNight$(1) = "NIGHT"

    baseName$(1) = "1b"
    baseName$(2) = "2b"
    baseName$(3) = "3b"
    baseName$(4) = "home"

    heldStatus$(0) = " "
    heldStatus$(1) = "H"

    P = 1

    For I = 0 To 1:
        currPitcher(I) = -1: gamePitcher(I) = -1
    Next

    startPitcher(0) = -1
    startPitcher(1) = -1

    For I = 0 To 1
        For J = 1 To 9
            fldPos(I, J) = -1
            lineupPlayer(I, J) = -1
        Next
    Next

    D = 0
    P = 1

    autoPlay = 0
    compTeam = 0
    DB = 0
    ejectCnt = 0
    endAllGames = 0
    gameOver = 1
    gameTemp = 0
    currFielder = 0
    H6% = 0
    HR = 0
    injCount = 0
    INNING% = 0
    Inotloop% = 0
    LYN% = 0
    noLUFound = 0
    pbpLine = 0
    precipType = 0
    rainDelay = 0
    rainStart = 0
    SB = 0
    skyType = 0
    statSpecial = 0
    TR = 0
    windSpeed = 0

    E! = 0
    WF! = 0

    actualAttendance& = 0
    avgAttendance& = 0

    fileString$ = ""
    gameL$ = ""
    gameTime$ = ""
    gameW$ = ""
    PARK$ = ""
    pbpString$ = ""
    scheduleFile$ = ""
    wind$ = ""
    YY$ = ""

    'These are used to load team data
    Erase gameSA, teamSplit, injuryStatus
    'Erase batRating, mgrFile
    Erase pitchRating, teamRat_GAME
    'Erase batters$
    'Erase gameManagers$, gameStadiums$
    'Erase pitchers$, teamAbbreviatons$

    Erase BP, batLUIdx, MON
    Erase VV, VV1

    Erase A5%
    Erase b1Runner, b1Pitcher, game_batRating, leagBatStats, gameBatStats, fielder
    Erase BT%, BB%, CSS, D3%, DP%
    Erase gameB, b2Base, b2Pitcher, b2Mystery, gameP
    Erase gameD0, gameMgrIdx
    Erase gameM, gameScore, GK%
    Erase gamePitStats, H0%, inningScore
    Erase L0%, leagPitStats, mgrFile
    Erase schedOptions, nbrInnings, nbrOuts
    Erase oppScore
    Erase currPitcher, pitcherOfRecord, statT1!, gamePitcher
    Erase S1%, S8%, startPitcher
    Erase teamMgrProfileVal, teamScore, T3%, X0%

    Erase currLineupSlot, parkHRVals!, statT0!

    Erase diskIDs$, doublesPlayer$
    Erase eventDesc$, ejections$
    Erase gameTeams$
    Erase homeRuns$, injPlayers$, locIndicator$, oppName$
    Erase PARK$, player$, pbpStrings$
    Erase Q3$, stolenBases$, triplesPlayer$, umpireName$

    For P9 = 0 To 1
        For I3 = 0 To 21
            If pitchRating(P9, I3, SS_P_Avail) <> 99 Then pitchRating(P9, I3, SS_P_Avail) = 0
        Next
    Next

    For I = 0 To 1
        For I1 = 0 To 23
            X0%(I, 0, I1) = -1
            X0%(I, 2, I1) = 100
        Next I1
    Next I

    For I = 0 To 1
        For I1 = 0 To 22
            If batRating(I, I1, SS_B_Avail) <> 99 Then batRating(I, I1, SS_B_Avail) = 0
        Next
    Next

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP (validTeam)

    Shared gameMgrIdx()

    'Setup variables for auto-play
    gameLoc = schedOptions(2)
    desigHit = schedOptions(3)
    pbpDelay! = schedOptions(4) / 10
    useWeather = schedOptions(5)
    useInj = schedOptions(6)
    useLRSplit = schedOptions(7)
    playerOpt = schedOptions(8)
    useRest = schedOptions(9)
    useMgrProf = schedOptions(10)
    useVGA = schedOptions(11)
    pitchEraOpt = schedOptions(14)
    gameMonth = schedOptions(17)

    If schedOptions(15) < 1600 Then
        dayOrNight = 0
    Else
        dayOrNight = 1
    End If

    For tgtTeam = 0 To 1

        tgtId = schedOptions(tgtTeam)
        tgtYear$ = diskIDs$(tgtTeam)

        Call ReadGameTeam(tgtYear$, gameTeams$(tgtTeam), tgtId, tgtTeam, validTeam)
        Call ReadGameExtraData(tgtYear$, tgtId, tgtTeam)

        schedOptions(tgtTeam) = tgtId
        gameMgrIdx(tgtTeam) = tgtId
        diskIDs$(tgtTeam) = tgtYear$

        If tgtId = 0 Then
            'In case chosen team ends up with 0 value
            validTeam = 0
        End If

    Next tgtTeam



End Sub


'----------------------------------------
'       LetterToNumber Subroutine
'----------------------------------------
'This subroutine converts an alphabetic
'character to a numeric equivalent,
'where A = 10, B = 11, etc.
'This is useful for long menus that have
'more than 10 options (0 - 9) and a single
'key is desired for a choice.
Sub LetterToNumber (promptval$, J)

    J = Val(promptval$)

    Select Case UCase$(promptval$)

        Case "A": J = 10
        Case "B": J = 11
        Case "C": J = 12
        Case "D": J = 13
        Case "E": J = 14
        Case "F": J = 15
        Case "G": J = 16
        Case "H": J = 17
        Case "I": J = 18
        Case "J": J = 19
        Case "K": J = 20
        Case "L": J = 21
        Case "M": J = 22

    End Select

End Sub


'----------------------------------------
'           TEAMBOX Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 6 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub TEAMBOX (boxHeight, teamIdx)

    Locate 6, 8
    Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 7 + boxHeight, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 7, 11

End Sub


'----------------------------------------
'         FRAMEOPTION Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 8 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub FRAMEOPTION (boxHeight, teamIdx)

    PCopy 0, 1
    Screen , , 1, 1

    Locate 8, 8
    Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + boxHeight, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 9, 12

End Sub


'----------------------------------------
'         Rolld100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Sub Rolld100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub


'----------------------------------------
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Function d100% ()

    d100% = Int(Rnd(1) * 100) + 1

End Function


'----------------------------------------
'            DELAY Subroutine
'----------------------------------------
'This subroutine simply serves as a place-
'holder for the command to initiate the
'pbp delay.
Sub DELAY ()

    _Delay pbpDelay!

End Sub


'----------------------------------------
'         GetPBPString Subroutine
'----------------------------------------
'This subroutine opens the play-by-play
'file and replaces any shortcut text with
'the corresponding positions, players, etc.

'This works only as long as the PBP file
'structure stays fixed.
'pbpIdx% = the index to grab from the file
'The range of indexes is fixed throughout
'the code depending on context
Sub GetPBPString (range%, minIdx%, fileString$, D)

    Do

        badString = 0

        'Get a new PBP string to use
        pbpIdx% = Int(Rnd(1) * range%) + minIdx%

        Open "REVISED.PBP" For Random As #1 Len = 44
        Field #1, 44 As YY$
        Get #1, pbpIdx%
        fileString$ = _Trim$(YY$)
        Close #1

        stringPosition% = 1

        Do

            test = Asc(Mid$(fileString$, stringPosition%, 1))

            'If it is a non-printable character (corrupt data)
            If test > 122 Or test < 32 Then
                'Get a new PBP string to use
                stringPosition% = 1
                badString = 1
            Else
                stringPosition% = stringPosition% + 1
            End If

        Loop Until stringPosition% = Len(fileString$) Or badString = 1

    Loop Until stringPosition% = Len(fileString$) Or badString = 0

    If TGTBASE% = 3 Then
        If Left$(fileString$, 2) = "Q1" Then fileString$ = "Q2 " + Right$(fileString$, Len(fileString$) - 3)
        If Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + "Q2"
    End If

    If Left$(fileString$, 2) = "BB" Then fileString$ = player$(0) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q1" Then fileString$ = player$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q2" Then fileString$ = player$(2) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "PP" Then fileString$ = pitchers$(D, currPitcher(D)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "UU" Then fileString$ = gameUmpire$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "XX" Then fileString$ = batters$(D, fielder(D, 5)) + " and " + batters$(D, fielder(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "TT" Then fileString$ = batters$(D, fielder(D, 5)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "FB" Then fileString$ = batters$(D, fielder(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "YY" Then fileString$ = batters$(D, fielder(D, 4)) + " and " + batters$(D, fielder(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "ZZ" Then fileString$ = batters$(D, fielder(D, 4)) + " and " + batters$(D, fielder(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q4" Then fileString$ = player$(4) + Right$(fileString$, Len(fileString$) - 2)

    If Right$(fileString$, 3) = " PP" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + pitchers$(D, currPitcher(D))
    If Right$(fileString$, 3) = " BB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(0)
    If Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(1)
    If Right$(fileString$, 2) = "Q2" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(2)
    If Right$(fileString$, 2) = "UU" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + gameUmpire$(1)
    If Right$(fileString$, 2) = "Q4" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(4)
    If Right$(fileString$, 3) = " TT" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 5))
    If Right$(fileString$, 2) = "FB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 3))
    If Right$(fileString$, 3) = " XX" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 5)) + " and " + batters$(D, fielder(D, 6))
    If Right$(fileString$, 2) = "YY" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 3)) + " and " + batters$(D, fielder(D, 4))
    If Right$(fileString$, 3) = " ZZ" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 6)) + " and " + batters$(D, fielder(D, 4))

End Sub


'----------------------------------------
'           GETRPBP Subroutine
'----------------------------------------
'This subroutine opens the play-by-play
'file and replaces any shortcut text with
'the corresponding positions, players, etc.

'This works only as long as the PBP file
'structure stays fixed.
'pbpIdx% = the index to grab from the file
'The range of indexes is fixed throughout
'the code depending on context
Sub GETRPBP (range%, minIdx%, fileString$, D)

    pbpIdx% = Int(Rnd(1) * range%) + minIdx%

    Open "REVISED.PBP" For Random As #1 Len = 44

    Field #1, 44 As YY$

    Get #1, pbpIdx%

    fileString$ = _Trim$(YY$)

    Close #1

    If TGTBASE% = 3 And Left$(fileString$, 2) = "Q1" Then fileString$ = "Q2 " + Right$(fileString$, Len(fileString$) - 3)
    If TGTBASE% = 3 And Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + "Q2"

    If Left$(fileString$, 2) = "BB" Then fileString$ = player$(0) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q1" Then fileString$ = player$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q2" Then fileString$ = player$(2) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "PP" Then fileString$ = pitchers$(D, currPitcher(D)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "UU" Then fileString$ = gameUmpire$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "XX" Then fileString$ = batters$(D, fielder(D, 5)) + " and " + batters$(D, fielder(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "TT" Then fileString$ = batters$(D, fielder(D, 5)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "FB" Then fileString$ = batters$(D, fielder(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "YY" Then fileString$ = batters$(D, fielder(D, 4)) + " and " + batters$(D, fielder(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "ZZ" Then fileString$ = batters$(D, fielder(D, 4)) + " and " + batters$(D, fielder(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q4" Then fileString$ = player$(4) + Right$(fileString$, Len(fileString$) - 2)
    If Right$(fileString$, 3) = " PP" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + pitchers$(D, currPitcher(D))
    If Right$(fileString$, 3) = " BB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(0)
    If Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(1)
    If Right$(fileString$, 2) = "Q2" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(2)
    If Right$(fileString$, 2) = "UU" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + gameUmpire$(1)
    If Right$(fileString$, 2) = "Q4" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(4)
    If Right$(fileString$, 3) = " TT" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 5))
    If Right$(fileString$, 2) = "FB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 3))
    If Right$(fileString$, 3) = " XX" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 5)) + " and " + batters$(D, fielder(D, 6))
    If Right$(fileString$, 2) = "YY" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 3)) + " and " + batters$(D, fielder(D, 4))
    If Right$(fileString$, 3) = " ZZ" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 6)) + " and " + batters$(D, fielder(D, 4))

End Sub

'----------------------------------------
'          STRIPRPBP Subroutine
'----------------------------------------
'This function checks each character in
'the Play-by-Play text to see if there is
'anything non-printable / non-usable.
'If found, it tries to grab a new string
'from the same range.
Sub STRIPRPBP (range%, minIdx%, fileString$, D)

    stringPosition% = 1

    'Check each character of PBP string
    Do

        test = Asc(Mid$(fileString$, stringPosition%, 1))

        'If it is a non-printable character (corrupt data)
        If test > 122 Or test < 32 Then
            'Get a new PBP string to use
            stringPosition% = 1
            Call GETRPBP(range%, minIdx%, fileString$, D)
        Else
            stringPosition% = stringPosition% + 1
        End If

    Loop Until stringPosition% = Len(fileString$)

End Sub


'----------------------------------------
'             PBP Subroutine
'----------------------------------------
'This is the classic Haffner Play-by-play
'subroutine. Given the target text, it will
'place it in the correct spot in the pbp
'text window. No text is placed if the
'delay is 0.

'This subroutine was not in the original
'game, but can also determine if we are
'printing on GFX or text screen
Sub PBP (pbpString$)

    M$ = InKey$

    If UCase$(M$) = "O" Then
        Call OPTIONS
    End If

    If UCase$(M$) = Chr$(17) Then
        Cls
        End
    End If

    If Environ$("PBPLOG") = "TRUE" Then Print #11, "======== "; pbpString$
    
    If pbpDelay! > 0 Then

        'Clean up / restrict to 44 char's
        'this is the width of the PBP window
        pbpString$ = PadRight(pbpString$, 44)
        pbpString$ = Left$(pbpString$, 44)
        'Print #11, pbpString$

        pbpLine = pbpLine + 1

        ' Put PBP text in a slot for line pbpLine
        If pbpLine < 9 Then pbpStrings$(pbpLine) = pbpString$

        ' Or we're past line 8, move all slots down
        ' to make room
        If pbpLine > 8 Then

            For I9 = 1 To 7
                pbpStrings$(I9) = pbpStrings$(I9 + 1)
            Next

            pbpStrings$(8) = pbpString$

        End If

        'Print all 8 lines from scratch
        For I9 = 1 To 8

            If useVGA = 1 Then
                Color &HFFFFFFFF
                _PrintMode _FillBackground , imageScreen&
                PrintOnGfx 20, I9 + 3, pbpStrings$(I9), imageScreen&
            Else
                Locate 14 + I9, 1
                Print pbpStrings$(I9)
            End If

        Next

        Call DELAY

    End If

End Sub


'----------------------------------------
'           ADDPIT Subroutine
'----------------------------------------
' ...explanation...
Sub ADDPIT (D, I5, I6)

    leagPitStats(D, I5, I6) = leagPitStats(D, I5, I6) + 1
    gamePitStats(D, I5, I6) = gamePitStats(D, I5, I6) + 1

End Sub

'----------------------------------------
'           ADDSTTS Subroutine
'----------------------------------------
' ...explanation...
Sub ADDSTTS (P, D)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
    Else
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
    End If

    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

End Sub


'----------------------------------------
'          LocateInning routine
'----------------------------------------
'This subroutine is used to locate the
'cursor exactly at the right spot that is
'needed to print an inning results / header
Sub LocateInning (I2)

    If I2 < 4 Then Locate , ((2 * I2) + 18)
    If I2 > 3 And I2 < 7 Then Locate , ((2 * I2) + 19)
    If I2 > 6 And I2 < 10 Then Locate , ((2 * I2) + 20)

    If I2 > 9 And I2 < 13 Then Locate , ((2 * (I2 - 9)) + 18)
    If I2 > 12 And I2 < 16 Then Locate , ((2 * (I2 - 9)) + 19)
    If I2 > 15 And I2 < 19 Then Locate , ((2 * (I2 - 9)) + 20)

    If I2 > 18 And I2 < 22 Then Locate , ((2 * (I2 - 18)) + 18)
    If I2 > 21 And I2 < 25 Then Locate , ((2 * (I2 - 18)) + 19)
    If I2 > 24 And I2 < 28 Then Locate , ((2 * (I2 - 18)) + 20)

End Sub


'----------------------------------------
'         ADJRUNNERS Subroutine
'----------------------------------------
' ...explanation...
Sub ADJRUNNERS (S2%, P)

    For idx = 1 To 3
        If S2% = EVENT_GROUNDER And A5%(idx) = 11 And b1Runner(idx) = 0 Then
            A5%(0) = 11
        End If
    Next idx

    If Not (b1Runner(3) = 0 Or A5%(3) = 0) Then

        If A5%(3) >= 1 And A5%(3) <= 4 Then
            Call CHARGERUN(D, 3)
        End If

        If A5%(3) = 10 Then
            I3 = 3  'runner tried to retreat to 3rd?
            Call CLEARBASE(3)
        End If

        If A5%(3) = 11 Then
            I3 = 4  'runner tried to score?
            Call CLEARBASE(3)
        End If

    End If

    If Not (b1Runner(2) = 0 Or A5%(2) = 0) Then

        Select Case A5%(2)
            Case 1
                If b1Runner(3) = 0 Then
                    Call BATTER1(2)
                End If
            Case 2 To 4
                If b1Runner(3) = 0 Then
                    Call CHARGERUN(D, 2)
                End If
            Case 10
                I3 = 2  'runner tried to retreat to 2nd?
                Call CLEARBASE(2)
            Case 11
                I3 = 3  'runner tried to advance to 3rd?
                Call CLEARBASE(2)
            Case 12
                If b1Runner(3) = 0 Then
                    I3 = 4  'runner tried to score from 2nd?
                    Call CLEARBASE(2)
                End If
        End Select

    End If

    If b1Runner(1) <> 0 And A5%(1) <> 0 Then

        Select Case A5%(1)

            Case 1
                If b1Runner(2) = 0 Then
                    Call BATTER1(1)
                End If

            Case 2
                If b1Runner(2) = 0 And b1Runner(3) = 0 Then
                    Call BATTER2(1)
                End If

            Case 3 To 4
                If b1Runner(2) = 0 And b1Runner(3) = 0 Then
                    Call CHARGERUN(D, 1)
                End If

            Case 8
                If b1Runner(2) = 0 Then
                    Call BATTER1(1)
                End If

            Case 10
                I3 = 1  'runner tried to retreat to 1st?
                Call CLEARBASE(1)

            Case 11
                I3 = 2  'runner tried to advance to 2nd?
                Call CLEARBASE(1)

            Case 12
                If b1Runner(3) = 0 And b1Runner(2) = 0 Then
                    I3 = 3  'runner tried to advance to 3rd?
                    Call CLEARBASE(1)
                End If

            Case 13
                If b1Runner(3) = 0 And b1Runner(2) = 0 Then
                    I3 = 4  'runner tried to score from 1st?
                    Call CLEARBASE(1)
                End If

        End Select

    End If

    b1Runner(0) = currLineupSlot(P)
    b1Pitcher(0) = currPitcher(D)

    If A5%(0) <> 0 Then

        Select Case A5%(0)

            Case 1 To 3:
                I3 = A5%(0)
                Call BATTER3(I3, P, D)

            Case 4
                I3 = A5%(0)
                Call CHARGERUN(D, 0)

            Case 11 To 14
                I3 = A5%(0) - 10    'Todo [bm] Determine if I3 is even USED anywhere?
                Call CLEARBASE(0)

        End Select

    End If

End Sub


'----------------------------------------
'           ADV1BASE Subroutine
'----------------------------------------
' ...explanation...
Sub ADV1BASE (p_base)

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV1BASE before: p_base: "; p_base
        For bm1 = 0 to 3
            Print #11, "b2Base:   "; b2Base(bm1) ; " -- ";
        Next bm1
    End If

    b2Base   (p_base + 1) = b2Base(p_base)
    b2Pitcher(p_base + 1) = b2Pitcher(p_base)
    b2Mystery(p_base + 1) = b2Mystery(p_base)

    b2Mystery(p_base + 1) = 0

    b2Base   (p_base) = 0
    b2Pitcher(p_base) = 0
    b2Mystery(p_base) = 0

    player$(p_base + 1) = player$(p_base)
    player$(p_base) = ""

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV1BASE after: p_base: "; p_base
        For bm1 = 0 to 3
            Print #11, "b2Base:   "; b2Base(bm1) ; " -- ";
        Next bm1
        Print #11,
    End If

End Sub

'----------------------------------------
'          ADV2BASES Subroutine
'----------------------------------------
' ...explanation...
Sub ADV2BASES (p_base)

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV2BASES before: p_base: "; p_base
        For bm1 = 0 to 3
            Print #11, "b2Base:   "; b2Base(bm1) ; " -- ";
        Next bm1
    End If

    b2Base   (p_base + 2) = b2Base(p_base)
    b2Pitcher(p_base + 2) = b2Pitcher(p_base)
    b2Mystery(p_base + 2) = b2Mystery(p_base)

    b2Mystery(p_base + 1) = 0

    b2Base   (p_base) = 0
    b2Pitcher(p_base) = 0
    b2Mystery(p_base) = 0

    player$(p_base + 2) = player$(p_base)
    player$(p_base) = ""

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV2BASES after: p_base: "; p_base
        For bm1 = 0 to 3
            Print #11, "b2Base:   "; b2Base(bm1) ; " -- ";
        Next bm1
        Print #11,
    End If

  For I4 = 0 To 2
        b2Base   (p_base + 2) = b2Base(p_base)
        b2Pitcher(p_base + 2) = b2Pitcher(p_base)
        b2Mystery(p_base + 2) = b2Mystery(p_base)

        b2Mystery(p_base + 1) = 0

        b2Base   (p_base) = 0
        b2Pitcher(p_base) = 0
        b2Mystery(p_base) = 0
    Next I4

    player$(p_base + 2) = player$(p_base)
    player$(p_base) = ""

End Sub

'----------------------------------------
'          ADV3BASES Subroutine
'----------------------------------------
' ...explanation...
Sub ADV3BASES (I3, P, D)

    b2Base(I3) = currLineupSlot(P)
    b2Pitcher(I3) = currPitcher(D)
    b2Mystery(I3) = 0
    b2Base(0) = 0

    player$(I3) = player$(0)

End Sub

'----------------------------------------
'         ADVANCEMENT Subroutine
'----------------------------------------
' ...explanation...
Sub ADVANCEMENT (P, D, S2%, errorOnPlay%, I3)

    If Environ$("PBPLOG") = "TRUE" Then

        Print #11,
        Print #11, "--- ADVANCEMENT ---"
        For bm1 = 0 to 3
            Print #11, "A5%:"; A5%(bm1) ; " -- ";
        Next bm1
        Print #11,

    End If

    'Advance 3rd base runner
    If b2Base(3) <> 0 And A5%(3) <> 0 Then

        i = 3

        If A5%(3) >= 1 And A5%(3) <= 4 Then
            Call RecordRun(sah%, i, D, errorOnPlay%, PQ, S2%, currFielder, INFPOS%)
        End If

        'Out at 3rd
        If A5%(3) = 10 Then
            I3 = 3
            Call RecordOut(i, 3)
        End If

        'Out at home
        If A5%(3) = 11 Then
            I3 = 4
            Call RecordOut(i, 4)
        End If

    End If

    'Advance 2nd base runner
    If b2Base(2) <> 0 And A5%(2) <> 0 Then

        i = 2

        'Advance
        If A5%(2) = 1 And b2Base(3) = 0 Then
            Call ADV1BASE(i)
        End If

        If A5%(2) >= 2 And A5%(2) <= 4 And b2Base(3) = 0 Then
            Call RecordRun(sah%, i, D, errorOnPlay%, PQ, S2%, currFielder, INFPOS%)
        End If

        'Out at 2nd
        If A5%(2) = 10 Then
            I3 = 2
            Call RecordOut(i, 2)
        End If

        'Out at 3rd
        If A5%(2) = 11 Then
            I3 = 3
            Call RecordOut(i, 3)
        End If

        'Out at home
        If A5%(2) = 12 And b2Base(3) = 0 Then
            I3 = 4
            Call RecordOut(i, 4)
        End If

    End If

    'Advance 1st base runner
    If b2Base(1) <> 0 And A5%(1) <> 0 Then

        i = 1

        'Advance
        If A5%(1) = 1 And b2Base(2) = 0 Then
            Call ADV1BASE(i)
        End If

        'Advance 2 bases
        If A5%(1) = 2 And b2Base(2) = 0 And b2Base(3) = 0 Then
            Call ADV2BASES(i)
        End If

        If A5%(1) >= 3 And A5%(1) <= 4 And b2Base(2) = 0 And b2Base(3) = 0 Then
            Call RecordRun(sah%, i, D, errorOnPlay%, PQ, S2%, currFielder, INFPOS%)
        End If

        If A5%(1) = 8 And b2Base(2) = 0 Then
            Call ADV1BASE(i)
        End If

        'Out at 2nd
        If A5%(1) = 10 Then
            Call RecordOut(i, 1)
        End If

        'Out at 2nd
        If A5%(1) = 11 Then
            Call RecordOut(i, 2)
        End If

        'Out at 3rd
        If A5%(1) = 12 And b2Base(3) = 0 And b2Base(2) = 0 Then
            Call RecordOut(i, 3)
        End If

        'Out at home
        If A5%(1) = 13 And b2Base(2) = 0 And b2Base(3) = 0 Then
            Call RecordOut(i, 4)
        End If

    End If

    i = 0
    b2Base(0) = currLineupSlot(P)
    b2Pitcher(0) = currPitcher(D)

    'Advance batter
    If A5%(0) <> 0 Then

        If A5%(0) > 0 And A5%(0) < 4 Then
            I3 = A5%(0)
            Call ADV3BASES(I3, P, D)
        End If

        If A5%(0) = 4 Then
            I3 = A5%(0)
            Call RecordRun(sah%, i, D, errorOnPlay%, PQ, S2%, currFielder, INFPOS%)
        End If

        If A5%(0) = 11 Then
            I3 = 1
            Call RecordOut(i, 1)
        End If

        If A5%(0) = 12 Then
            I3 = 2
            Call RecordOut(i, 2)
        End If

        If A5%(0) = 13 Then
            I3 = 3
            Call RecordOut(i, 3)
        End If

        If A5%(0) = 14 Then
            I3 = 4
            Call RecordOut(i, 4)
        End If

    End If

    If errorOnPlay% = 0 And nbrOuts(1) < 3 Then
        Call ADJRUNNERS(S2%, P)
    End If

    Erase A5%

End Sub


'----------------------------------------
'    DisplayBatter_Lineup Subroutine
'----------------------------------------
' ...explanation...
Sub DisplayBatter_Lineup (PG, P9)

    Locate 1, 1

    Color 14: Print "#";
    Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12): Print gameTeams$(P9);

    Color 14, 0

    Locate , 16: Print "B   POS    G  AB 2B 3B HR RBI  BB  SO SRAB  SB CS G%  BA"

    For luIdx = 2 To 13:
        Locate luIdx, 1: Print String$(78, 32)
    Next

    Locate 2, 1

    If PG = 0 Then AA = 0: BB = 12
    If PG = 1 Then AA = 13: BB = 22

    For idx = AA To BB:

        Locate , 1

        If batRating(P9, idx, SS_B_Avail) = 0 Then

            Color 2, 0

            If idx < 10 Then Print Using "#"; idx;
            If idx > 9 Then Print alpha$(idx - 10);

            Color 15, 0

            Print Tab(3); batters$(P9, idx); Tab(17); handed$(batRating(P9, idx, SS_B_Bats) + 2);

            For xx = 20 To 23
                Print Tab(xx);: If batRating(P9, idx, xx + 2) <> -1 Then Print Using "#"; batRating(P9, idx, xx + 2);
            Next

            Print Tab(24);
            Print Using " ### ### ## ## ## ### ### ### #"; batRating(P9, idx, SS_B_G); batRating(P9, idx, SS_B_AB); batRating(P9, idx, SS_B_2B); batRating(P9, idx, SS_B_3B); batRating(P9, idx, SS_B_HR); batRating(P9, idx, SS_B_RBI); batRating(P9, idx, SS_B_W); batRating(P9, idx, SS_B_K); batRating(P9, idx, SS_B_Run);
            Print Using "#"; batRating(P9, idx, SS_B_Range);
            Print Using "#"; batRating(P9, idx, SS_B_Arm);
            Print Using "# "; batRating(P9, idx, SS_B_Bunt);
            Print Using "### ## ## "; batRating(P9, idx, SS_B_SB); batRating(P9, idx, SS_B_CS); batRating(P9, idx, SS_B_GndPct);
            Print Tab(69); ".";

            'BA
            If batRating(P9, idx, SS_B_AB) <> 0 Then
                bAvg! = batRating(P9, idx, SS_B_H) / batRating(P9, idx, SS_B_AB)
                Print Using "###"; Int(((bAvg!) + .0005) * 1000);
            Else
                Print
            End If

        End If

    Next idx

End Sub


'----------------------------------------
'           BATOFF Subroutine
'----------------------------------------
' ...explanation...
Sub BATOFF (P, I5, I6)

    leagBatStats(P, I5, I6) = leagBatStats(P, I5, I6) - 1
    gameBatStats(P, I5, I6) = gameBatStats(P, I5, I6) - 1

    If Not (I6 > 8 Or I6 = 2 Or I6 = 3) Then

        If pitchRating(1 - P, currPitcher(1 - P), SS_P_Throws) <> 1 Then

            If I6 >= 4 Then
                CSS(P, I5, I6 - 2) = CSS(P, I5, I6 - 2) - 1
            Else
                CSS(P, I5, I6) = CSS(P, I5, I6) - 1
            End If

        End If

    End If

End Sub


'----------------------------------------
'           BATTER1 Subroutine
'----------------------------------------
' ...explanation...
Sub BATTER1 (i)

    b1Runner (i + 1) = b1Runner(i)
    b1Pitcher(i + 1) = b1Pitcher(i)

    b1Runner (i) = 0
    b1Pitcher(i) = 0

End Sub

'----------------------------------------
'           BATTER2 Subroutine
'----------------------------------------
' ...explanation...
Sub BATTER2 (i)

    b1Runner (i + 2) = b1Runner(i)
    b1Pitcher(i + 2) = b1Pitcher(i)

    b1Runner (i) = 0
    b1Pitcher(i) = 0

End Sub

'----------------------------------------
'           BATTER3 Subroutine
'----------------------------------------
' ...explanation...
Sub BATTER3 (I3, P, D)

    b1Runner(I3) = currLineupSlot(P)
    b1Pitcher(I3) = currPitcher(D)

End Sub

'----------------------------------------
'          CAUGHTFLY Subroutine
'----------------------------------------
' ...explanation...
Sub CAUGHTFLY (i, S2%, D)

    Call Rolld100(RN)

    Select Case RN

        Case 1 To 96
            i = 1
            If S2% <> EVENT_FLY_DEEP Then
                Call GetPBPString(60, 1230, fileString$, D)
                Call PBP(player$(4) + " " + fileString$)
            Else
                If teamRat_GAME(1, 13) = 37 Then Call PBP("he climbs the terrace")
                Call GetPBPString(20, 1290, fileString$, D)
                Call PBP(fileString$)
            End If

        Case 97
            i = 2
            Call PBP(player$(4) + " makes a sliding catch")
            Call PBP("What a play!!!")

        Case 98
            i = 3
            Call PBP(player$(4) + " makes a leaping grab")
            Call PBP("What a catch!!!")

        Case 99
            i = 4
            Call PBP(player$(4) + " makes a diving catch!!!")

        Case 100
            i = 6
            Call PBP(player$(4) + " makes an incredible catch!!")

    End Select

End Sub


'----------------------------------------
'        CHANGELINEUP Subroutine
'----------------------------------------
'This subroutine displays the lineup for
'the user during gameplay. They will then
'be presented an interface for making
'changes through a separate subroutine.
Sub CHANGELINEUP (P9, P)

    Locate 14, 1
    Color 2: Print " # LINEUP       PS  FA  OB"
    Color 15

    For luIdx = 1 To 9:

        Locate , 1
        If currLineupSlot(P9) = luIdx Then Print "*";

        Locate , 2: Print Using "#"; luIdx;
        Locate , 4

        If fldPos(P9, luIdx) = 1 Then
            Print pitchers$(P9, currPitcher(P9));
        End If
        If fldPos(P9, luIdx) <> 1 Then
            Print batters$(P9, lineupPlayer(P9, luIdx));
        End If
        If fldPos(P9, luIdx) <> 0 And fldPos(P9, luIdx) < 10 Then
            Locate , 20: Print batRating(P9, lineupPlayer(P9, luIdx), SS_B_FldPct);
        End If

        Locate , 17
        Print fieldPos$(fldPos(P9, luIdx));
        Locate , 25

        For I1 = 1 To 3
            If b2Base(I1) = luIdx And P9 = P Then Color 3: Print I1;: Color 15
        Next I1

        Print " "

    Next luIdx

End Sub

'----------------------------------------
'          CHARGERUN Subroutine
'----------------------------------------
' ...explanation...
Sub CHARGERUN (D, i)

    If nbrOuts(1) >= 3 Then
        Call CLEARBASE(i)
    Else
        Call ADDPIT(D, b1Pitcher(i), LG_P_ER)
        Call CLEARBASE(i)
    End If

End Sub


'----------------------------------------
'          CHECKSAVE Subroutine
'----------------------------------------
' ...explanation...
Sub CHECKSAVE (D, P, baseRunners)

    I3 = gameScore(D, 0) - gameScore(P, 0)
    S1%(D) = -1

    If I3 >= 1 Then

        If baseRunners = 0 And I3 <= 2 Then S1%(D) = currPitcher(D)
        If baseRunners > 0 And baseRunners < 4 And I3 <= 3 Then S1%(D) = currPitcher(D)
        If baseRunners > 3 And baseRunners < 7 And I3 <= 4 Then S1%(D) = currPitcher(D)
        If baseRunners = 7 And I3 <= 5 Then S1%(D) = currPitcher(D)
        If baseRunners = 0 And nbrOuts(0) = 0 And I3 <= 3 Then S1%(D) = currPitcher(D)
        If INNING% > 5 And I3 > 0 And INNING% < 8 Then S1%(D) = currPitcher(D)

    End If

    RI = 0

    If baseRunners <> 0 Then

        If baseRunners = 1 Or baseRunners = 2 Or baseRunners = 3 Then RI = 1
        If baseRunners = 4 Or baseRunners = 5 Or baseRunners = 6 Then RI = 2
        If baseRunners = 7 Then RI = 3

        leagPitStats(D, currPitcher(D), 17) = leagPitStats(D, currPitcher(D), 17) + RI
        gamePitStats(D, currPitcher(D), 17) = gamePitStats(D, currPitcher(D), 17) + RI

    End If

End Sub

'----------------------------------------
'          CLEARBASE Subroutine
'----------------------------------------
' ...explanation...
Sub CLEARBASE (i)

    b1Runner(i) = 0
    b1Pitcher(i) = 0

End Sub

'----------------------------------------
'        CREDITASSIST Subroutine
'----------------------------------------
'Credit player with assist
Sub CREDITASSIST (currFielder, D)

    If currFielder = 1 Then
        Call ADDPIT(D, currPitcher(D), LG_PB_A)
    Else
        Call INCBATDF(D, fielder(D, currFielder), LG_B_A)
        If Environ$("PBPLOG") = "TRUE" Then Print #11, "Assist for: "; currFielder
    End If

End Sub

'----------------------------------------
'       CREDITPUTOUT Subroutine
'----------------------------------------
'Credit player with infield putout
Sub CREDITPUTOUT (I3, FB2P%, AST%, D)

    If I3 > 4 Then I3 = 1

    '1=1st 2=2nd 3=3rd 4=home
    po% = I3 + 2

    If po% = 6 Then

        po% = 2
        AST% = 0
        Call INCBATDF(D, fielder(D, po%), LG_B_PO)
        If Environ$("PBPLOG") = "TRUE" Then Print #11, "Infield? Putout1 for: "; po% 

    Else

        If po% = 3 And FB2P% = 1 Then

            FB2P% = 0
            po% = 1
            If po% = 1 Then
                Call ADDPIT(D, currPitcher(D), LG_PB_PO)
            End If

        Else

            If po% = 4 Then
                If AST% = 4 Then
                    po% = 6
                Else
                    If AST% = 6 Then
                        po% = 4
                    End If
                End If
            End If

        End If

        AST% = 0

        If po% = 1 Then
            fld% = currPitcher(D)
        ElseIf po% > 1 And po% <= 10 Then
            fld% = fielder(D, po%)
        Else
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "WARNING!!  skipping call to INCBATDF in CREDITPUTOUT because po% = " + Str$(po%)
            Exit Sub
        End If

        Call INCBATDF(D, fld%, LG_B_PO)
        If Environ$("PBPLOG") = "TRUE" Then Print #11, "Infield? Putout2 for: "; po%

    End If

End Sub

'----------------------------------------
'         CREDITREGPO Subroutine
'----------------------------------------
'Credit player with regular putout
Sub CREDITREGPO (currFielder, D)

    If Environ$("PBPLOG") = "TRUE" Then Print #11, "Reg Putout for: "; currFielder

    If currFielder = 1 Then
        Call ADDPIT(D, currPitcher(D), LG_PB_PO)
    Else
        Call INCBATDF(D, fielder(D, currFielder), LG_B_PO)
    End If

End Sub


'----------------------------------------
'          DELIVERY Subroutine
'----------------------------------------
'This subroutine generates the PBP for
'a pitch delivery.
Sub DELIVERY (D)

    If d100% <= 20 Then

        pbpIdx% = Int(Rnd(1) * 20) + 50
        Call GetPBPString(20, 50, fileString$, D)
        Call PBP(fileString$)

    End If

    Call GetPBPString(46, 1, fileString$, D)
    Call PBP(fileString$)

End Sub

'----------------------------------------
'   DisplayFielding_Lineup Subroutine
'----------------------------------------
'This subroutine shows the fielding line-
'up for the current team.
Sub DisplayFielding_Lineup (PG, P9, TG)

    Locate 1, 16
    Color 14: Print "P1 FAVG A R   P2 FAVG A R   P3 FAVG A R   P4 FAVG A R      "

    For luIdx = 2 To 13:
        Locate luIdx, 1: Print String$(78, 32)
    Next

    Locate 2

    If PG = 0 Then
        AA = 0
        BB = 12
    End If

    If PG = 1 Then
        AA = 13
        BB = 22
    End If

    For luIdx = AA To BB:

        If batRating(P9, luIdx, SS_B_Avail) = 0 Then
            Color 2
            If luIdx < 10 Then Print Using "#"; luIdx;
            If luIdx > 9 Then Print alpha$(luIdx - 10);

            Color 15, 0
            Print Tab(3); batters$(P9, luIdx);

            Locate , 17: If batRating(P9, luIdx, SS_B_Pos1) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos1); batRating(P9, luIdx, SS_B_FldPct1); batRating(P9, luIdx, SS_B_Arm1); batRating(P9, luIdx, SS_B_Range1);
            Locate , 31: If batRating(P9, luIdx, SS_B_Pos2) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos2); batRating(P9, luIdx, SS_B_FldPct2); batRating(P9, luIdx, SS_B_Arm2); batRating(P9, luIdx, SS_B_Range2);
            Locate , 45: If batRating(P9, luIdx, SS_B_Pos3) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos3); batRating(P9, luIdx, SS_B_FldPct3); batRating(P9, luIdx, SS_B_Arm3); batRating(P9, luIdx, SS_B_Range3);
            Locate , 59: If batRating(P9, luIdx, SS_B_Pos4) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos4); batRating(P9, luIdx, SS_B_FldPct4); batRating(P9, luIdx, SS_B_Arm4); batRating(P9, luIdx, SS_B_Range4);

            Print

        End If

    Next luIdx

    TG = TG + 1

End Sub

'----------------------------------------
'         DisplayRun Subroutine
'----------------------------------------
' ...explanation...
Sub DisplayRun (I2, P, X)

    J = b2Base(I2)

    If J <> 0 Then

        Color teamRat_GAME(P, 11), teamRat_GAME(P, 12)

        If fldPos(P, J) = 1 And pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then

            Print Left$(pitchers$(P, currPitcher(P)), 10)
            Color 15, 2
            Locate , X: Print "1/  0"

        Else
            If fldPos(P, J) = 1 Then
                Print Left$(pitchers$(P, currPitcher(P)), 10)
                Locate , X
                Color 15, 2: Print Using "#"; pitchRating(P, currPitcher(P), SS_P_Run);
                Print "/";: Print Using "###"; pitchRating(P, currPitcher(P), SS_P_H_SB)
            Else
                Print Left$(batters$(P, lineupPlayer(P, J)), 10)
                Locate , X
                Color 15, 2: Print Using "#"; batRating(P, lineupPlayer(P, J), SS_B_Run);
                Print "/";: Print Using "###"; batRating(P, lineupPlayer(P, J), SS_B_SB)
            End If

        End If

    End If

    Color 15, 0

End Sub


'----------------------------------------
'            DPPBP Subroutine
'----------------------------------------
'This subroutine prints the Play-by-Play
'for a double-play.
Sub DPPBP ()

    Call Rolld100(RN)

    Select Case RN

        Case 1 To 19:
            pbpString$ = "may be a double play ball"
        Case 20 To 40:
            pbpString$ = "they might turn two"

        Case 41 To 60:
            pbpString$ = "they could get two"

        Case 61 To 80:
            pbpString$ = "could be two"

        Case 81 To 100:
            pbpString$ = "could be a double play"

    End Select

    Call PBP(pbpString$)

End Sub


'----------------------------------------
'           DPPIVOT Subroutine
'----------------------------------------
'This subroutine handles variables and
'Play-by-Play for a double-play pivot
Sub DPPIVOT (currFielder, D)

    ' *** double play pivot ***
    If currFielder = 4 Then teamTotWins% = 6
    If currFielder = 6 Then teamTotWins% = 4
    If currFielder = 3 Then teamTotWins% = 1

    Call GetPBPString(20, 1910, fileString$, D)
    Call PBP(fileString$ + " " + batters$(D, fielder(D, teamTotWins%)))

End Sub


'----------------------------------------
'           DPRELAY Subroutine
'----------------------------------------
'Double-play relay ?!?!
Sub DPRELAY ()

    If A5%(0) = 11 Then

        If d100% <= 10 Then
            Call PBP("Give me the Hoover!")
        Else

            Call Rolld100(RN)

            Select Case RN
                Case 1 To 8: pbpString$ = "over to 1st..."
                Case 9 To 16: pbpString$ = "throw to 1st..."
                Case 17 To 24: pbpString$ = "on to first..."
                Case 25 To 30: pbpString$ = player$(4) + " pivots and throws"
                Case 31 To 36: pbpString$ = player$(4) + " gets the throw off"
                Case 37 To 44: pbpString$ = player$(4) + " is hit as he throws"
                Case 45 To 50: pbpString$ = player$(4) + " leaps and throws"
                Case 51 To 56: pbpString$ = player$(4) + " jumps over the sliding " + player$(1)
                Case 57 To 64: pbpString$ = "the relay to first..."
                Case 65 To 72: pbpString$ = "quick turn by " + player$(4)
                Case 73 To 80: pbpString$ = "relay to 1st..."
                Case 81 To 88: pbpString$ = "to 1st..."
                Case 89 To 100: If A5%(0) = 11 Then pbpString$ = "over to 1st..."
            End Select

            Call PBP(pbpString$)

        End If

    Else

        Call DELAY

    End If

End Sub


'----------------------------------------
'             DPSAFE routine
'----------------------------------------
'Double-play safe ?!?!
Sub DPSAFE ()

    Call Rolld100(RN)

    pbpString$ = ""

    Select Case RN
        Case 1 To 8: pbpString$ = "over to 1st..."
        Case 9 To 16: pbpString$ = "throw to 1st..."
        Case 17 To 24: pbpString$ = "on to first..."
        Case 25 To 30: pbpString$ = "pivot to first"
        Case 31 To 36: pbpString$ = "the throw to first"
        Case 37 To 44: pbpString$ = "relay to first"
        Case 45 To 50: pbpString$ = "quick turn to 1st"
        Case 51 To 56: pbpString$ = "on to first..."
        Case 57 To 64: pbpString$ = "the relay to first..."
        Case 65 To 72: pbpString$ = "quick turn"
        Case 73 To 80: pbpString$ = "relay to 1st..."
        Case 81 To 88: pbpString$ = "to 1st..."
        Case 89 To 100: If A5%(0) = 11 Then pbpString$ = "over to 1st..."
    End Select

    Call PBP(pbpString$)

    Call GetPBPString(20, 1890, fileString$, D)
    Call PBP(fileString$)

End Sub


'----------------------------------------
'          EJECTINC Subroutine
'----------------------------------------
'This subroutine increases the ejection
'count for the target player/team.
Sub EJECTINC (ejectCnt, X$)

    ejectCnt = ejectCnt + 1
    ejections$(ejectCnt) = X$

End Sub


'----------------------------------------
'       EJECTIONINJURY Subroutine
'----------------------------------------
'This subroutine handles a player on defense
'who has been ejected or injured. In add-
'ition to calculations, it calls a routine
'that prompts the user for replacements.
Sub EJECTIONINJURY (playerMode, P, compTeam, D, po%, P9, W5, W6)

    If Not (playerMode = 2 Or playerMode = 1 And P <> compTeam) Then
        pbpString$ = "You must replace " + batters$(D, fielder(D, po%))
        Call PBP(pbpString$)
    Else

        For I2 = 1 To 9
            If fldPos(D, I2) = po% Then Exit For
        Next

        For idx = 0 To 22

            If batRating(D, idx, SS_B_Avail) <= 0 And batters$(D, idx) <> "XXX" Then

                If fldPos(D, I2) = batRating(D, idx, SS_B_Pos1) Or fldPos(D, I2) = batRating(D, idx, SS_B_Pos2) Or fldPos(D, I2) = batRating(D, idx, SS_B_Pos3) Or fldPos(D, I2) = batRating(D, idx, SS_B_Pos4) Then

                    pbpString$ = batters$(D, fielder(D, po%)) + " will be replaced by " + batters$(D, idx)
                    Call PBP(pbpString$)

                    gameD0(D) = gameD0(D) + 1
                    X0%(D, 0, gameD0(D)) = idx
                    X0%(D, 1, gameD0(D)) = fldPos(D, I2)
                    X0%(D, 2, gameD0(D)) = (I2 * 10) + (gameD0(D) - 9)
                    batRating(D, idx, SS_B_Avail) = 1
                    lineupPlayer(D, I2) = idx
                    I7% = D
                    I8% = I2
                    i9% = idx

                    Call REPLACEMENTS(I7%, I8%, i9%)

                    fielder(D, fldPos(D, I2)) = idx
                    P9 = D
                    W5 = P9
                    W6 = P9
                    VV(P9) = 1
                    VV1(P9) = 9

                    Exit Sub

                End If

            End If

        Next idx

    End If

End Sub


'----------------------------------------
'           FLDAVG Subroutine
'----------------------------------------
'Fielding Average
Sub FLDAVG (D, currFielder, I3)

    If batRating(D, fielder(D, currFielder), SS_B_FldPct) >= 999 Then

        I3 = 999

    Else

        If currFielder >= 7 Then
            I3 = batRating(D, fielder(D, currFielder), SS_B_FldPct)
        Else
            If currFielder = 3 Or currFielder = 2 Then I3 = (batRating(D, fielder(D, currFielder), SS_B_FldPct)) * .96
        End If

    End If

    I4 = batRating(D, fielder(D, currFielder), SS_B_Pos1)
    I5 = batRating(D, fielder(D, currFielder), SS_B_Pos2)
    I6 = batRating(D, fielder(D, currFielder), SS_B_Pos3)
    I7 = batRating(D, fielder(D, currFielder), SS_B_Pos4)

    If currFielder <> I4 And currFielder <> I5 And currFielder <> I6 And currFielder <> I7 Then
        If currFielder = 4 Or currFielder > 6 Then I3 = 982
        If currFielder = 2 Then I3 = 987
        If currFielder = 3 Then I3 = 980
        If currFielder = 6 Or currFielder = 5 Then I3 = 964

        If currFielder >= 7 Then
            I3 = I3 - 10
        Else
            I3 = I3 - 20
        End If

        If currFielder = 2 Or currFielder >= 4 And currFielder <= 6 Then I3 = I3 - 80: Exit Sub

    End If

    If currFielder = 4 Or currFielder = 6 Then I3 = (batRating(D, fielder(D, currFielder), SS_B_FldPct)) * .995
    If currFielder = 5 Then I3 = (batRating(D, fielder(D, currFielder), SS_B_FldPct)) * .985

End Sub


'----------------------------------------
'      DisplayBallField Subroutine
'----------------------------------------
'This subroutine draws the field for
'the traditional Haffner text display.
'This also includes printing various
'player stats and other information.
Sub DisplayBallField (nbrBalls, P, nbrStrikes, D)

    Color , 0
    Cls

    For X = 5 To 25
        Locate X, 46
        Color , 2
        Print String$(34, 32);
    Next

    Color , 0
    Locate 1, 1


    If INNING% <= 9 Then
        inningStart% = 1
        inningEnd% = 9
    Else
        If INNING% <= 18 Then
            inningStart% = 10
            inningEnd% = 18
        Else
            inningStart% = 19
            inningEnd% = 27
        End If
    End If

    Print Tab(10);

    'Innings / 1st row of stats
    For X1% = 0 To 2
        Print Using "## ## ##  "; inningStart% + 3 * X1%; inningStart% + 3 * X1% + 1; inningStart% + 3 * X1% + 2;
    Next

    Print " R  H  E ";

    Color 1: Print Chr$(179);
    Color 15: Print " BALL:  ";
    Color 14: Print nbrBalls;

    Color 1: Print Chr$(179);
    Color 15: Print "WIND: ";
    Color 14: Print wind$

    'Team Score / Rows 2-3 of display
    For team = 0 To 1:

        Locate team + 2
        Color teamRat_GAME(team, 11), teamRat_GAME(team, 12)
        Print Left$(gameTeams$(team), 3); teamAbbreviatons$(team);

        Color 14, 0

        For I2 = inningStart% To inningEnd%
            If P = 0 And team = 0 And I2 > INNING% - 1 And inningScore(team, INNING%) = 0 Or P = 0 And team = 0 And I2 > INNING% And inningScore(team, INNING%) > 0 Or P = 1 And I2 > INNING% And team = 0 Then Exit For
            If P = 0 And team = 1 And I2 > INNING% - 1 Or P = 1 And team = 1 And inningScore(team, INNING%) = 0 And I2 > INNING% - 1 Or P = 1 And team = 1 And inningScore(team, INNING%) > 0 And I2 > INNING% Then Exit For
            Call INNDISP(I2, team)
        Next I2

        Locate , 40: Print Using "## ## ## "; gameScore(team, 0); gameScore(team, 1); gameScore(team, 2);

        Color 1: Print Chr$(179);
        Color 15

        If team = 0 Then
            Print " STRIKE:";
            Color 14: Print nbrStrikes;
            Color 1: Print Chr$(179);
            Color 15: Print "SKY:  ";
            Color 14: Print skyCond$(skyType)
        Else
            Print " OUT:   ";
            Color 14: Print nbrOuts(0);
            Color 1: Print Chr$(179);
            Color 15: Print "TEMP:";
            Color 14: Print gameTemp
        End If

    Next team

    Color 1

    'Graphics / Lines
    Print String$(20, 196); Chr$(194); String$(23, 196); Chr$(194); String$(3, 196); Chr$(193); String$(11, 196); Chr$(193); String$(19, 196)

    For X = 5 To 12:
        Locate X, 21: Print Chr$(179);
    Next

    For X = 5 To 25:
        Locate X, 45: Print Chr$(179);
    Next

    'CF Name
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 5, 58: Print Left$(batters$(D, fielder(D, 8)), 10)

    'CF Stats
    Color 15, 2
    Locate 6, 59: Print Using "A:# R:#"; batRating(D, fielder(D, 8), SS_B_Arm); batRating(D, fielder(D, 8), SS_B_Range)
    If batRating(D, fielder(D, 8), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 8), SS_B_FldPct)), "###", 1)
    End If
    Locate 7, 59: Print "FA:" + fldPct$

    'RF / LF Names
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 6, 46: Print Left$(batters$(D, fielder(D, 7)), 10);
    Locate , 70: Print Left$(batters$(D, fielder(D, 9)), 10)

    'RF / LF Stats
    Color 15, 2
    Locate 7, 47: Print Using "A:# R:#"; batRating(D, fielder(D, 7), SS_B_Arm); batRating(D, fielder(D, 7), SS_B_Range);
    Locate , 71: Print Using "A:# R:#"; batRating(D, fielder(D, 9), SS_B_Arm); batRating(D, fielder(D, 9), SS_B_Range)

    If batRating(D, fielder(D, 7), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 7), SS_B_FldPct)), "###", 1)
    End If
    Locate 8, 47: Print "FA:" + fldPct$

    If batRating(D, fielder(D, 9), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 9), SS_B_FldPct)), "###", 1)
    End If
    Locate 8, 71: Print "FA:" + fldPct$

    'SS / 2B Names
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 10, 49: Print Left$(batters$(D, fielder(D, 6)), 10);
    Locate , 68: Print Left$(batters$(D, fielder(D, 4)), 10)

    'SS / 2B Stats
    Color 15, 2
    Locate 11, 50: Print Using "R:#"; batRating(D, fielder(D, 6), SS_B_Range);
    Locate 11, 69: Print Using "R:#"; batRating(D, fielder(D, 4), SS_B_Range)

    If batRating(D, fielder(D, 6), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 6), SS_B_FldPct)), "###", 1)
    End If
    Locate 12, 50: Print "FA:" + fldPct$

    If batRating(D, fielder(D, 4), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 4), SS_B_FldPct)), "###", 1)
    End If
    Locate 12, 69: Print "FA:" + fldPct$

    '2nd Base "Graphic"
    Locate 12, 62: Print Chr$(219);

    '1B / 3B Names
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 14, 46: Print Left$(batters$(D, fielder(D, 5)), 10);
    Locate , 70: Print Left$(batters$(D, fielder(D, 3)), 10)

    '1B / 3B Stats
    Color 15, 2
    Locate 15, 47: Print Using "R:#"; batRating(D, fielder(D, 5), SS_B_Range);
    Locate 15, 71: Print Using "R:#"; batRating(D, fielder(D, 3), SS_B_Range)

    If batRating(D, fielder(D, 5), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 5), SS_B_FldPct)), "###", 1)
    End If
    Locate 16, 47: Print "FA:" + fldPct$

    If batRating(D, fielder(D, 3), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 3), SS_B_FldPct)), "###", 1)
    End If
    Locate 16, 71: Print "FA:" + fldPct$

    '1st / 3rd Base "Graphic"
    Locate 17, 49: Print Chr$(219);
    Locate 17, 78: Print Chr$(219);

    'Pitcher Name
    Locate 17, 55: Print "["; handed$(pitchRating(D, currPitcher(D), SS_P_Throws) + 2); "] "; Left$(pitchers$(D, currPitcher(D)), 10)

    'Pitcher Stats
    Color 15, 2
    Locate 18, 59: Print Using "H:#"; pitchRating(D, currPitcher(D), SS_P_HoldRunner)
    If pitchRating(D, currPitcher(D), SS_P_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(pitchRating(D, currPitcher(D), SS_P_FldPct)), "###", 1)
    End If
    Locate 19, 59: Print "FA:" + fldPct$

    'Home Plate "Graphic
    Locate 21, 62: Print Chr$(219)

    'Catcher Name
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12):
    Locate 23, 58: Print Left$(batters$(D, fielder(D, 2)), 10)

    'Catcher Stats
    Color 15, 2
    Locate LAST_ROW, 59: Print Using "   A:#"; batRating(D, fielder(D, 2), SS_B_Arm);

    If batRating(D, fielder(D, 2), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 2), SS_B_FldPct)), "###", 1)
    End If
    Locate 25, 59: Print "FA:" + fldPct$;

    'Graphics / Lines
    Color 7, 0
    For X = 5 To 25:
        Locate X, 1: Print String$(44, 32);
    Next

    Color 1
    Locate 4, 1
    Print String$(20, 196); Chr$(194); String$(23, 196); Chr$(194)
    Locate 14, 1
    Print String$(20, 196); Chr$(193); String$(23, 196); Chr$(180)

End Sub

'----------------------------------------
'           FLDERR Subroutine
'----------------------------------------
'Fielding Error
Sub FLDERR (errorOnPlay%, currFielder, D, I3, stealing, S2%, P)

    errorOnPlay% = 0
    errType = 0

    If currFielder = 1 Then
        If stealing = 1 Then
            I3 = 995
        Else
            I3 = pitchRating(D, currPitcher(D), SS_P_FldPct) + 30
        End If
    Else
        Call FLDAVG(D, currFielder, I3)
    End If

    If I3 >= 999 Then
        I6 = 1
    Else
        If S2% >= EVENT_INF_SINGLE And S2% <= EVENT_DP Then I6 = 1009 - I3
        If S2% >= EVENT_1B And S2% <= EVENT_3B Then I6 = 1006 - I3
        If S2% = EVENT_STEAL Or S2% >= EVENT_RUNDOWN Then I6 = 1000 - I3
        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_RUNNER_DBLD Then I6 = (1000 - I3) * .05

    End If

    If currFielder = 7 Then I6 = I6 - 4
    If currFielder = 8 Then I6 = I6 - 10
    If currFielder = 9 Then I6 = I6 - 15
    If currFielder = 5 Then I6 = I6 - 15

    H6% = Int(Rnd(1) * 1000) + 1

    If H6% <= I6 Then

        errorOnPlay% = 1
        rand = Int(Rnd(1) * 2) + 1
        errType = 2

        If Environ$("PBPLOG") = "TRUE" Then Print #11, "ERROR on: "; currFielder

        If currFielder = 1 Then
            Call ADDPIT(D, currPitcher(D), LG_PB_E)
        Else
            Call INCBATDF(D, fielder(D, currFielder), LG_B_E)
        End If

        If S2% = EVENT_INF_SINGLE Then

            Call PBP(player$(0) + " beats out an infield single")
            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
            End If

            Call ADDPIT(D, currPitcher(D), LG_P_H)

            If fldPos(P, currLineupSlot(P)) = 1 Then
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                Call INCPITOFF(P, currPitcher(P), LG_PB_H)
            End If

        End If

        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_RUNNER_DBLD Then
            errType = 3
        End If
        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_INF_POPUP Then
            Call OUTFIELDERR
        End If
        If S2% = EVENT_2B Or S2% = EVENT_3B Or S2% >= EVENT_RUNDOWN Or S2% = EVENT_STEAL Then
            errType = 1
        End If
        If S2% >= EVENT_INF_SINGLE And S2% <= 14 And rand = 1 Or S2% = EVENT_DP Then
            errType = 1
        End If

        Call Rolld100(RN)

        If errType = 1 And currFielder = 1 And S2% = EVENT_STEAL Then
            pbpString$ = "*** WILD PICK OFF THROW BY" + " " + player$(4) + " ***"
        End If

        If errType = 1 Then
            If RN <= 50 Then pbpString$ = "*** THROWING ERROR ON" + " " + player$(4) + " ***"
            If RN > 50 And RN <= 55 Then pbpString$ = "*** BALL KICKED BY" + " " + player$(4) + " ***"
            If RN > 55 Then pbpString$ = "*** " + player$(4) + " THROWS IT AWAY ***"
        End If

        If errType = 2 Then
            If RN <= 30 Then pbpString$ = "*** FIELDING ERROR ON" + " " + player$(4) + " ***"
            If RN > 30 And RN <= 50 Then pbpString$ = "*** OFF THE GLOVE OF" + " " + player$(4) + " ***"
            If RN > 50 And RN <= 75 Then pbpString$ = "*** BALL IS BOOTED BY" + " " + player$(4) + " ***"
            If RN > 75 And RN <= 95 Then pbpString$ = "*** BALL IS BOBBLED BY" + " " + player$(4) + " ***"
            If RN > 95 Then pbpString$ = "*** BALL IS THROUGH THE LEGS OF" + " " + player$(4) + " ***"
        End If

        If errType = 3 Then
            If RN <= 33 Then pbpString$ = "*** BALL DROPPED BY" + " " + player$(4) + " ***"
            If RN > 33 And RN <= 66 Then pbpString$ = "*** OFF THE GLOVE OF" + " " + player$(4) + " ***"
            If RN > 66 Then pbpString$ = "*** BALL IS MISPLAYED BY" + " " + player$(4) + " ***"
        End If

        Call PBP(pbpString$)

        If S2% = EVENT_STEAL Or S2% = EVENT_RUNDOWN Then

            I8! = Int(Rnd(1) * 10) + 1

            If I8! < 5 Then
                A5%(0) = 2
                A5%(1) = 2
                A5%(2) = 2
                A5%(3) = 1
                Call DELAY
            End If

        Else

            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1

            If Not (S2% > EVENT_FLY_DEEP Or S2% <= EVENT_INF_POPUP) Then

                If d100% <= 17 And S2% <> EVENT_2B Or S2% = EVENT_INF_SINGLE Then
                    A5%(0) = 2
                    A5%(1) = 2
                    A5%(2) = 2
                End If
            End If

        End If

        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_INF_POPUP And nbrOuts(0) = 2 Or S2% >= EVENT_BUNT Or S2% = EVENT_STEAL And currFielder = 2 Then
            A5%(3) = 1
            A5%(2) = 2
            A5%(1) = 2
            A5%(0) = 2
        End If

        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_FLY_SHALLOW And RN <= 33 And nbrOuts(0) < 2 And b2Base(1) = 0 Then A5%(0) = 2

        If S2% = EVENT_STEAL Or S2% = EVENT_RUNDOWN Then A5%(0) = 0

        If errType > 0 And S2% <> EVENT_1B And S2% <> EVENT_2B And S2% <> EVENT_3B And S2% <> EVENT_STEAL And S2% <> EVENT_RUNDOWN Then 
            nbrOuts(1) = nbrOuts(1) + 1     'nbrOuts(1) tracks how many outs there would be if no errors (for recording unearned runs)
        End If

        If S2% = EVENT_STEAL Or S2% = EVENT_1B Or S2% = EVENT_2B Or S2% = EVENT_3B Or S2% = EVENT_RUNDOWN Then

            A5%(0) = 0

            If I8! > 4 And currFielder <> 2 Then
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
            End If

        End If

        gameScore(D, 2) = gameScore(D, 2) + 1

        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)

    End If

End Sub


'----------------------------------------
'         GETFIELDER Subroutine
'----------------------------------------
'This subroutine returns the index? of the
'target fielder.
Sub GETFIELDER (currFielder, P, I1, D)

    currFielder = 0

    If fldPos(P, currLineupSlot(P)) <> 1 Then

        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bats) < 0 Then
            I1 = -1
        End If
        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bats) > 0 Then
            I1 = 1
        End If
        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bats) = 0 Then
            I1 = -1 * pitchRating(D, currPitcher(D), SS_P_Throws)
        End If

    Else

        I1 = pitchRating(P, currPitcher(P), SS_P_Throws)

    End If

End Sub


'----------------------------------------
'        GETFIELDNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'target fielder.
Sub GETFIELDNAME (currFielder, D)

    If currFielder <> 1 Then
        player$(4) = batters$(D, fielder(D, currFielder))
    Else
        player$(4) = pitchers$(D, currPitcher(D))
    End If

End Sub


'----------------------------------------
'        GETINFIELDER Subroutine
'----------------------------------------
'Get infielder
Sub GETINFIELDER (currFielder, P, I1, D, S2%, P2)

    Call GETFIELDER(currFielder, P, I1, D)

    Call Rolld100(RN)

    If I1 = 1 Then
        currFielder = 3
        If RN <= 92 Then currFielder = 4
        If RN <= 72 Then currFielder = 6
        If RN <= 32 Then currFielder = 5
    End If

    If I1 = -1 Then
        currFielder = 5
        If RN <= 88 Then currFielder = 6
        If RN <= 68 Then currFielder = 4
        If RN <= 28 Then currFielder = 3
    End If

    If S2% <> EVENT_INF_POPUP And RN <= 12 Then currFielder = 1
    If S2% = EVENT_INF_POPUP And RN <= 4 Or RN <= 12 Then currFielder = 2

    If S2% = EVENT_RUNNER_DBLD And currFielder = 2 Then currFielder = 1
    If P2 = OFF_BUNT And currFielder = 6 Then currFielder = 1
    If P2 = OFF_BUNT And currFielder = 4 Or S2% = EVENT_INF_POPUP And currFielder = 1 Then currFielder = 2

    Call GETFIELDNAME(currFielder, D)

End Sub

'----------------------------------------
'        GETOUTFIELDER Subroutine
'----------------------------------------
'Get outfielder
Sub GETOUTFIELDER (currFielder, P, I1, D)

    Call GETFIELDER(currFielder, P, I1, D)

    Call Rolld100(RN0)

    If RN0 <= 30 Then
        currFielder = 8
    Else

        If I1 = 1 Then
            If RN0 <= 50 Then
                currFielder = 9
            Else
                currFielder = 7
            End If
        End If

        If I1 = -1 Then
            If RN0 <= 50 Then
                currFielder = 7
            Else
                currFielder = 9
            End If
        End If

    End If

    player$(4) = batters$(D, fielder(D, currFielder))

End Sub


'----------------------------------------
'         GROUNDERPBP Subroutine
'----------------------------------------
'This subroutine manages variables and
'Play-by-Play display for grounders to
'the plate.
Sub GROUNDERPBP (currFielder, D)

    If currFielder <> 2 Then

        range = 140
        min = 1930

        If d100% > 35 Then
            If currFielder = 1 Then range = 160: min = 2070
            If currFielder = 3 Then range = 150: min = 2230
            If currFielder = 4 Then range = 160: min = 2380
            If currFielder = 5 Then range = 150: min = 2700
            If currFielder = 6 Then range = 160: min = 2540
        End If

        Call GetPBPString(range, min, fileString$, D)
        pbpString$ = fileString$ + " "

    Else

        ' *** in front of plate ***
        Call GetPBPString(25, 2870, fileString$, D)
        Call PBP(fileString$)

    End If

End Sub

'----------------------------------------
'          GROUNDERS Subroutine
'----------------------------------------
'This subroutine manages variables and
'Play-by-Play display for grounders to
'infielders.
Sub GROUNDERS (currFielder, D)

    X$ = player$(4)

    If currFielder <> 2 Then
        Call GetPBPString(120, 1420, fileString$, D)
        Call PBP(X$ + " " + fileString$)
    Else

        'Field grounders in front of the plate
        Call GetPBPString(13, 2850, fileString$, D)
        If Left$(fileString$, 2) = "QQ" Then
            fileString$ = X$ + Right$(fileString$, Len(fileString$) - 2)
        End If
        Call PBP(fileString$)
    End If

End Sub


'----------------------------------------
'          HITS2GAP Subroutine
'----------------------------------------
' ...explanation...
Sub HITS2GAP (currFielder, P, I1, D)

    Call GETFIELDER(currFielder, P, I1, D)

    Call Rolld100(RN0)

    If RN0 <= 5 Then
        currFielder = 8
    Else

        If I1 = 1 Then
            'should be 6 to 15 ?
            If RN0 <= 15 Then
                currFielder = 9
            Else
                currFielder = 7
            End If
        End If

        If I1 = -1 Then
            'should be 6 to 15 ?
            If RN0 <= 15 Then
                currFielder = 7
            Else
                currFielder = 9
            End If
        End If

    End If

    player$(4) = batters$(D, fielder(D, currFielder))

End Sub

'----------------------------------------
'          HOLDSTATUS Subroutine
'----------------------------------------
'This subroutine displays the hold status
'of the infield for traditional text mode.
Sub HOLDSTATUS (runnerHeld)

    Locate 17, 77

    Color 15, 2
    Print heldStatus$(runnerHeld)
    Color , 0

End Sub


'----------------------------------------
'          INCBATDF Subroutine
'----------------------------------------
'Increment Batter Defense
Sub INCBATDF (D, I5, I6)

    leagBatStats(D, I5, I6) = leagBatStats(D, I5, I6) + 1
    gameBatStats(D, I5, I6) = gameBatStats(D, I5, I6) + 1

End Sub

'----------------------------------------
'          INCBATOFF Subroutine
'----------------------------------------
'Increment Batter Offense
Sub INCBATOFF (P, I5, I6)

    leagBatStats(P, I5, I6) = leagBatStats(P, I5, I6) + 1
    gameBatStats(P, I5, I6) = gameBatStats(P, I5, I6) + 1

    If I6 >= 4 And I6 <= 8 And I6 <> 1 And pitchRating(1 - P, currPitcher(1 - P), SS_P_Throws) <> 1 Then

        If I6 >= 4 Then
            CSS(P, I5, I6 - 2) = CSS(P, I5, I6 - 2) + 1
        Else
            CSS(P, I5, I6) = CSS(P, I5, I6) + 1
        End If

    End If

End Sub

'----------------------------------------
'          INCPITOFF Subroutine
'----------------------------------------
'Increment Pitcher Offense
Sub INCPITOFF (P, I5, I6)

    leagPitStats(P, I5, I6) = leagPitStats(P, I5, I6) + 1
    gamePitStats(P, I5, I6) = gamePitStats(P, I5, I6) + 1

End Sub

'----------------------------------------
'        INCREMENTHIT Subroutine
'----------------------------------------
'Increment Hit
Sub INCREMENTHIT (P, D, MO)

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_GIDP)
    Else
        Call INCPITOFF(P, currPitcher(P), LG_PB_GIDP)
    End If

    Call ADDPIT(D, currPitcher(D), LG_P_GIDP)

    If MO > 0 Then MO = MO - 1

End Sub

'----------------------------------------
'          INJPIT Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'and replacement for a defensive pitcher
'who has been ejected or injured.
Sub INJPIT (P, D, IP%, PPH)

    If playerMode = 2 Or playerMode = 1 And P <> compTeam Then
        IP% = 1
        Call RELIEVERS(PPH, pz)
    Else
        Call PBP("You must replace " + pitchers$(D, currPitcher(D)))
    End If

End Sub

'----------------------------------------
'         INJURYLENGTH Subroutine
'----------------------------------------
'This subroutine calculates injury length
'and displays the information to the user.
'It then calls the necessary routines to
'Handle the associated variables / game'
'mechanics.
Sub INJURYLENGTH (injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

    doneLength = 0

    injCount = injCount + 1
    injPlayers$(injCount) = batters$(D, fielder(D, po%))

    pbpString$ = batters$(D, fielder(D, po%)) + " is injured!"
    Call PBP(pbpString$)

    pbpString$ = "He may be out for "

    ' *** Determine Injury Length ***
    il = 0

    If batRating(D, fielder(D, po%), SS_B_G) = 162 Then

        pbpString$ = pbpString$ + " the rest of the game"

    Else

        ct = 0

        For I = 0 To 22

            For I1 = 22 To 25
                If batRating(D, I, I1) = po% And batRating(D, I, SS_B_Injured) = 0 And I <> fielder(D, po%) Then ct = ct + 1
                If ct > 3 Then
                    il = 1
                    doneLength = 1
                    Exit For
                End If
            Next

        Next

        If doneLength = 0 Then
            pbpString$ = pbpString$ + " the rest of the game"
            il = 0
        Else
            If il <> 0 Then

                If batRating(D, fielder(D, po%), SS_B_G) = 161 Then
                    pbpString$ = pbpString$ + " this game and next game"
                    il = 1
                Else
                    il = Int(Rnd(1) * (162 - batRating(D, fielder(D, po%), SS_B_G))) + 1
                    pbpString$ = pbpString$ + Str$(il) + " games"
                End If

            End If

        End If

    End If

    Call PBP(pbpString$)
    batRating(D, fielder(D, po%), SS_B_Injured) = batRating(D, fielder(D, po%), SS_B_Injured) + il

    Call DELAY
    Call DELAY
    Call EJECTIONINJURY(playerMode, P, compTeam, D, po%, P9, W5, W6)

End Sub

'----------------------------------------
'           INNDISP Subroutine
'----------------------------------------
'This routine calculates where to Locate
'the cursor to print an Inning in the
'game's text mode.
Sub INNDISP (currInning, teamLine)

    If currInning < 4 Then Locate , 3 * currInning + 7
    If currInning > 3 And currInning < 7 Then Locate , 3 * currInning + 8
    If currInning > 6 And currInning < 10 Then Locate , 3 * currInning + 9
    If currInning > 9 And currInning < 13 Then Locate , 3 * (currInning - 9) + 7
    If currInning > 12 And currInning < 16 Then Locate , 3 * (currInning - 9) + 8
    If currInning > 15 And currInning < 19 Then Locate , 3 * (currInning - 9) + 9
    If currInning > 18 And currInning < 22 Then Locate , 3 * (currInning - 18) + 7
    If currInning > 21 And currInning < 25 Then Locate , 3 * (currInning - 18) + 8
    If currInning > 24 And currInning < 28 Then Locate , 3 * (currInning - 18) + 9

    Print Using "##"; inningScore(teamLine, currInning);

End Sub

'----------------------------------------
'           LDRIVE Subroutine
'----------------------------------------
'This subroutine generates a some text
'to accompany the Play-by-Play for a line
'drive.
Sub LDRIVE (X$)

    Call Rolld100(RN)

    Select Case RN
        Case 1 To 20: X$ = "rips"
        Case 21 To 40: X$ = "hammers"
        Case 41 To 60: X$ = "lashes"
        Case 61 To 80: X$ = "lines"
        Case 81 To 100: X$ = "drives"
    End Select

End Sub

'----------------------------------------
'           LONGFLY Subroutine
'----------------------------------------
'This subroutine handles the variabless and
'Play-by-Play text for a long fly.
Sub LONGFLY (D, currFielder)

    Call GetPBPString(60, 1310, fileString$, D)
    Call PBP(fileString$ + " " + fieldPos$(currFielder))

    If WF < 1 And Rnd(1) > WF Then

        Call PBP("The wind is bringing it back..."):

    Else

        'Adjust for the wind factor
        If WF > 1 And Rnd(1) <= 1 - WF Then
            Call PBP("It's carrying well in the breeze...")
        End If

        Call GetPBPString(50, 1370, fileString$, D)
        Call PBP(fileString$)

        Call Rolld100(RN)

        If Not ((teamRat_GAME(1, 13) = 21 Or teamRat_GAME(1, 13) = 27) And currFielder = 7) Then

            Select Case RN
                Case 1 To 5: Call PBP(player$(4) + " leaps")
                Case 6 To 10: Call PBP(player$(4) + " against the wall")
                Case 11 To 20: Call PBP(player$(4) + " jumps at the wall")
                Case 21 To 30: Call PBP(player$(4) + " climbs the wall")
            End Select

        End If

    End If

End Sub

'----------------------------------------
'          MAINSCREEN Subroutine
'----------------------------------------
'This subroutine is responsible for draw-
'ing the field, stats, players and various
'game options. In essence, the "main screen".
Sub MAINSCREEN (D, P, nbrBalls, nbrStrikes, MO)

    For I = 5 To 13
        Locate I, 1
        Print Space$(44);
    Next I

    Locate 5, 1
    Locate 21, 48

    Color 15, 2: Print Tab(62); Chr$(219); Tab(79);

    Locate 10, 1

    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Print pitchers$(D, currPitcher(D)); " "; handed$(pitchRating(D, currPitcher(D), SS_P_Throws) + 2)

    Color 15, 0

    Call DISPBAVG(D)

    Color 1

    For X = 5 To 13:
        Locate X, 21: Print Chr$(179);
    Next

    Locate 11, 1
    Color 2, 0: Print "W-L ERA ";

    Color 15, 0

    'Wins - losses
    Print Using "##"; pitchRating(D, currPitcher(D), 1);
    Print "-";
    Print Using "##"; pitchRating(D, currPitcher(D), 2);

    'ERA
    Print " ";: Print Using "##.##"; pitchRating(D, currPitcher(D), SS_P_ERA100) / 100

    Color 2, 0

    Print "  IP  H  BB  K GAME"

    Color 15, 0

    'IP
    If gamePitStats(D, currPitcher(D), 0) = 0 Then
        Print " 0.0";
    Else
        Print Using "##"; Int(gamePitStats(D, currPitcher(D), 0) / 3);
        Print ".";
        Print Using "#"; gamePitStats(D, currPitcher(D), 0) - (Int(gamePitStats(D, currPitcher(D), 0) / 3) * 3);
    End If

    'H / BB / K
    Print Using " ##  ## ##"; gamePitStats(D, currPitcher(D), 1); gamePitStats(D, currPitcher(D), 4); gamePitStats(D, currPitcher(D), 5)

    Locate 5, 1
    J = lineupPlayer(P, currLineupSlot(P))
    Color teamRat_GAME(P, 11), teamRat_GAME(P, 12)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        'Pitcher Name
        X$ = pitchers$(P, currPitcher(P))
        Print X$; " "; handed$(pitchRating(P, currPitcher(P), SS_P_Throws) + 2);
        Call PITPLACEMENT(P, X$)
    Else
        'Batter Name
        X$ = batters$(P, J)
        Print X$; " "; handed$(batRating(P, J, SS_B_Bats) + 2);
        If batRating(P, J, SS_B_Bats) < 0 Or batRating(P, J, SS_B_Bats) = 0 And pitchRating(D, currPitcher(D), SS_P_Throws) > 0 Then
            Locate 21, 64
        End If
        If batRating(P, J, SS_B_Bats) > 0 Or batRating(P, J, SS_B_Bats) = 0 And pitchRating(D, currPitcher(D), SS_P_Throws) < 0 Then
            Locate 21, 51 + 10 - Len(X$)
        End If
        Print X$
    End If

    Locate 6, 1
    Color 2, 0: Print " AVG HR  BB   K"
    Color 15, 0

    'AVG / HR / BB / K
    If fldPos(P, currLineupSlot(P)) <> 1 Then

        'Batter Stats - Hitter
        If batRating(P, J, SS_B_AB) = 0 Then
            Print ".000  0   0   0";
        Else
            bAvg! = batRating(P, J, SS_B_H) / batRating(P, J, SS_B_AB) * 1000
            Print "."; Using "###"; bAvg!
            Print Using " ## ### ###"; batRating(P, J, SS_B_HR); batRating(P, J, SS_B_W); batRating(P, J, SS_B_K)
        End If

    Else

        'Batter Stats - Pitcher
        If pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then
            pAvg! = pitchRating(P, currPitcher(P), SS_P_BAvg)
            If pAvg! = 0 Then
                Print ".000 0"
            Else
                Print "."; Using "###"; pAvg! * 1000
            End If
        Else
            If pitchRating(P, currPitcher(P), SS_P_H_AB) = 0 Then
                Print ".000 0";
            Else
                pAvg! = pitchRating(P, currPitcher(P), SS_P_H_H) / pitchRating(P, currPitcher(P), SS_P_H_AB) * 1000
                Print "."; Using "###"; pAvg!
                Print Using " ## ### ###"; pitchRating(P, currPitcher(P), SS_P_H_HR); pitchRating(P, currPitcher(P), SS_P_H_BB); pitchRating(P, currPitcher(P), SS_P_H_SO)
            End If
        End If

    End If

    Locate 8, 2

    ' # - # TODAY
    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Print gameBatStats(P, lineupPlayer(P, currLineupSlot(P)), 1); "-"; gameBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0);
    Else
        Print gamePitStats(P, currPitcher(P), 24); "-"; gamePitStats(P, currPitcher(P), 23);
    End If

    Print " TODAY"
    Color 14, 0

    'Outs
    Locate 3, 58: Print nbrOuts(0)
    'Strikes
    Locate 2, 58: Print nbrStrikes
    'Balls
    Locate 1, 58: Print nbrBalls
    Color 15, 2
    Locate 22, 46: Print "INFIELD:"

    Locate 23, 46
    Select Case INFPOS%
        Case 0: Print "NORMAL    "
        Case 1: Print "CORNERS IN"
        Case 2: Print "INFIELD IN"
    End Select

    Call HOLDSTATUS(runnerHeld)

    Locate 13, 58
    Color 15, 2: Print Tab(68);
    Locate 14, 58: Print Space$(8)

    Locate 13, 58

    I2 = 2
    X = 58

    Call DisplayRun(I2, P, X)

    If nbrOuts(0) <> 3 Then MO = 0

    For WK = 1 To 3
        If b2Base(WK) > 0 Then
            MO = MO + 1
        End If
    Next

    Locate 18, 47
    Color 15, 2: Print Tab(57);

    Locate 19, 47: Print Space$(8)

    Locate 18, 47

    I2 = 3
    X = 47

    Call DisplayRun(I2, P, X)

    Locate 18, 69
    Color 15, 2: Print Tab(79);
    Locate 19, 70: Print Space$(8)

    Locate 18, 69

    I2 = 1
    X = 70

    Call DisplayRun(I2, P, X)

    Color 15, 0

End Sub

'----------------------------------------
'        WINDEX Subroutine
'----------------------------------------
'This subroutine cleans the "window" used
'for the play-by-play text.
Sub WINDEX ()

    If pbpDelay! > 0 Then


        pbpLine = 0

        If useVGA = 1 Then

            Color &HFFFFFFFF
            _PrintMode _FillBackground , imageScreen&

            For I5 = 4 To 11
                PrintOnGfx 20, I5, Space$(44), imageScreen&
            Next

        Else

            For I5 = 15 To 24
                Locate , 1
                Locate I5
                Print Space$(44);
            Next I5
            Locate 15, 1

        End If

        Erase pbpStrings$

    End If

End Sub


'----------------------------------------
'       UpdateSchedule Subroutine
'----------------------------------------
'This subroutine updates the schedule
'file for a completed game. It will first 
'update any relevant ticker scores, then 
'update the scores for the current game in
'the corresponding schedule file.
Sub UpdateSchedule

    Shared scheduleFile$

	Open scheduleFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

	For X = 0 To 18
		Field #1, X * 2 As X$, 2 As player$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
	Next

	Field #1, 38 As X$, 2 As player$(22), 2 As player$(23), 5 As player$(20), 5 As player$(21)

	schedOptions(13) = 1

	For X = 0 To 18:
		LSet player$(X + 1) = MKI$(schedOptions(X))
	Next

	For X = 0 To 1:
		LSet player$(20 + X) = diskIDs$(X)
	Next

	LSet player$(22) = MKI$(gameScore(0, 0))
	LSet player$(23) = MKI$(gameScore(1, 0))

	Put #1, schedGame(0) + schedGame(2)

	Close #1

	schedGame(2) = schedGame(2) + 1
	autoPlay = 1

End Sub


'----------------------------------------
'        NEWPITCHER Subroutine
'----------------------------------------
'NewPitcher
Sub NEWPITCHER (pIdx, PPH, pz)

    V0 = pIdx

    If S1%(D) <> -1 And gamePitcher(D) > 1 Then
        Call ADDPIT(D, currPitcher(D), LG_P_Hold)
    End If

    currPitcher(D) = V0
    gamePitcher(D) = gamePitcher(D) + 1
    pitchRating(D, V0, SS_P_Avail) = gamePitcher(D)
    P9 = D

    Call PITSTAM(P9)

    For V0 = 1 To 9

        If fldPos(D, V0) = 1 Or fldPos(D, V0) = 10 Then
            lineupPlayer(D, V0) = currPitcher(D)
            fldPos(D, V0) = 1
            fielder(D, 1) = currPitcher(D)
            P9 = D
            W5 = P9
            W6 = P9
            VV(P9) = V0
            VV1(P9) = V0
            Call HITTINGLINES
        End If

    Next V0

    VV(P9) = 1
    VV1(P9) = 9
    fielder(D, 1) = currPitcher(D)

    Call CHECKSAVE(D, P, baseRunners)

    W5 = 1 - P9
    W6 = 1 - P9
    VV(1 - P9) = 1
    VV1(1 - P9) = 9

    Call WINDEX

    Locate 15, 1

    If d100% <= 50 Then Call PBP("Hold the phone...")

    Call PBP(gameManagers$(D) + " is making a pitching change")
    Call DELAY
    Call HITTINGLINES

    If useVGA = 1 Then
        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
    Else
        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
    End If

    PPH = 0

    pz = 1

End Sub

'----------------------------------------
'        NOJUMP Subroutine
'----------------------------------------
'NoJump (PBP)
Sub NOJUMP (D)

    Call GetPBPString(20, 1050, fileString$, D)
    Call PBP(fileString$)
    Call DELAY

End Sub

'----------------------------------------
'        OFFPIT Subroutine
'----------------------------------------
'OffPit
Sub OFFPIT (P9, I5, I6)

    leagPitStats(P9, I5, I6) = leagPitStats(P9, I5, I6) + 1
    gamePitStats(P9, I5, I6) = gamePitStats(P9, I5, I6) + 1

End Sub

'----------------------------------------
'        OUTATFIRST Subroutine
'----------------------------------------
'This subroutine handles the stats and
'Play-by-Play for an out at first base.
Sub OUTATFIRST (currFielder, D, fbun%, FB2P%)

    Call PITCHERNAME(currFielder, D)
    Call GROUNDERS(currFielder, D)

    If currFielder = 3 And d100% <= 94 Then
        Call PBP("he takes it to the bag unassisted")
        fbun% = 1
    Else
        If currFielder = 3 Then
            currFielder = 1
            Call PITCHERNAME(currFielder, D)
            Call PBP("He flips it to " + player$(4) + " covering")
            FB2P% = 1
            currFielder = 3
        End If

        If currFielder = 4 Then Call SECOND2FIRST(D)

        If currFielder = 5 Or currFielder = 6 Then Call THROW2FIRST(D)

    End If

End Sub

'----------------------------------------
'        OUTFIELDERR Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for an outfield error.
Sub OUTFIELDERR ()

    Call Rolld100(RN)

    Select Case RN
        Case 1 To 20: pbpString$ = player$(4) + " can't find it"
        Case 21 To 40: pbpString$ = player$(4) + " misjudges it"
        Case 41 To 60: pbpString$ = player$(4) + " turns the wrong way"
        Case 61 To 80: pbpString$ = player$(4) + " loses it"
        Case 81 To 100: pbpString$ = player$(4) + " doesn't see it"
    End Select

    Call PBP(pbpString$)

End Sub

'----------------------------------------
'       OUTFLDLOCATION Subroutine
'----------------------------------------
'This function returns the location of an
'outfielder. (LCF vs RCF)
Function OUTFLDLOCATION$ (currFielder)

    Call Rolld100(RN)

    If currFielder = 7 Or currFielder = 8 And RN > 50 Then OUTFLDLOCATION$ = "lcf"
    If currFielder = 9 Or currFielder = 8 And RN <= 50 Then OUTFLDLOCATION$ = "rcf"

End Function

'----------------------------------------
'         OUTFLDNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'specified outfielder.
Sub OUTFLDNAME (currFielder, D)

    currFielder = 9

    Call Rolld100(RN)

    Select Case RN
        Case 1 To 5: currFielder = 7
        Case 6 To 25: currFielder = 8
    End Select

    player$(4) = batters$(D, fielder(D, currFielder))

End Sub


'----------------------------------------
'          PICKEDOFF Subroutine
'----------------------------------------
'This subroutine covers the stats and the
'Play-by-Play for a runner who is picked
'off by the pitcher or catcher.
Sub PICKEDOFF (D, pickedOffBase)

    ' *** picked off ***
    Call WINDEX
    Call DELIVERY(D)

    Call Rolld100(RN)

    Select Case RN
        Case  1 To  33: pbpString$ = "NO, " + pitchers$(D, currPitcher(D)) + " throws to " + baseName$(pickedOffBase - 1)
        Case 34 To  66: pbpString$ = "quick move to " + baseName$(pickedOffBase - 1)
        Case 67 To 100: pbpString$ = "pickoff throw to " + baseName$(pickedOffBase - 1)
    End Select

    Call PBP(pbpString$)

    If d100% <= 50 Then
        pbpString$ = player$(pickedOffBase - 1) + " is picked off base!"
    Else
        pbpString$ = player$(pickedOffBase - 1) + " is caught napping"
    End If

    Call PBP(pbpString$)

    Call Rolld100(RN)

    Select Case RN
        Case 1 To 25: pbpString$ = "he can't believe it"
        Case 26 To 50: pbpString$ = "what a teriffic move by " + pitchers$(D, currPitcher(D))
    End Select

    Call PBP(pbpString$)

End Sub

'----------------------------------------
'         PINCHRUNNER Subroutine
'----------------------------------------
' ...explanation...
Sub PINCHRUNNER (MS%, A1, baseRunners, I2, P, PR%, nbrBalls, nbrStrikes, D, MO)

    Dim pinchRunners(24)

    ' *** CHECK FOR PINCH RUNNER ***
    i0% = 0

    If A1 <= 1 And A1 >= -2 And INNING% >= 8 Then

        I2 = 1

        If baseRunners = 1 Or baseRunners = 5 Then I2 = 1
        If baseRunners = 2 Or baseRunners = 4 Or baseRunners = 6 Then I2 = 2

        ' J REPRESENTS THE SPOT IN THE BATTING ORDER
        ' J1 represents the base
        J = b2Base(I2)
        J1 = I2

        If J <> 0 Then

            If teamMgrProfileVal(P, 771) = 999 Then

                For I1 = 772 To 777
                    If teamMgrProfileVal(P, I1) <> -1 Then
                        If batRating(P, teamMgrProfileVal(P, I1), SS_B_Avail) = 0 Then
                            I1 = teamMgrProfileVal(P, I1)
                            If fldPos(P, J) = 1 Then
                                'PINCH RUNNING FOR THE PITCHER
                                If teamMgrProfileVal(P, 803) = 999 Then
                                    Call Rolld100(RN)
                                    If pitchRating(P, currPitcher(P), SS_P_Run) <= teamMgrProfileVal(P, 804) And INNING% >= teamMgrProfileVal(P, 805) And RN <= teamMgrProfileVal(P, 806) Then 8993
                                    If pitchRating(P, currPitcher(P), SS_P_Run) <= teamMgrProfileVal(P, 807) And INNING% >= teamMgrProfileVal(P, 808) And RN <= teamMgrProfileVal(P, 809) Then 8993
                                    GoTo 8999
                                End If
                            End If

                            For I = 0 To 5
                                If teamMgrProfileVal(P, I * 4 + 779) = lineupPlayer(P, J) Then
                                    'FOUND A MATCH FOR THE RUNNER ON BASE
                                    Call Rolld100(RN)
                                    If INNING% >= teamMgrProfileVal(P, I * 4 + 780) And batRating(P, teamMgrProfileVal(P, I * 4 + 781), SS_B_Avail) = 0 And RN <= teamMgrProfileVal(P, I * 4 + 782) Then batRating(P, teamMgrProfileVal(P, I * 4 + 781), SS_B_Avail) = 40 + J: GoTo 8993
                                    GoTo 8999
                                End If
                            Next

                        End If
                        teamMgrProfileVal(P, I1) = -1
                    End If

                Next

                GoTo 8999

            End If

            If fldPos(P, J) = 1 And pitchRating(P, currPitcher(P), SS_P_Run) > 4 Then 8999
            If fldPos(P, J) <> 1 And batRating(P, lineupPlayer(P, J), SS_B_Run) > 3 Then 8999

            I1 = -1

            For I = 0 To 22
                If batters$(P, I) <> "XXX" And batRating(P, I, SS_B_Avail) <= 0 Then
                    For I2 = 22 To 25
                        If batRating(P, I, I2) <> -1 And batRating(P, I, I2) = fldPos(P, J) And fldPos(P, J) <> 1 Then I1 = I
                    Next
                    If batRating(P, I, SS_B_Run) > 5 Then
                        pinchRunners(i0%) = I
                        i0% = i0% + 1
                    End If
                End If
            Next

            If i0% = 0 Or I1 = -1 And fldPos(P, J) <> 1 Then 8999
            If fldPos(P, J) <> 1 Then batRating(P, I1, SS_B_Avail) = 40 + J

            RN = Int(Rnd(1) * i0%)
            I1 = pinchRunners(RN)

            8993 '
            I2 = J
            player$(J1) = batters$(P, I1)
            batRating(P, I1, SS_B_Avail) = 1
            lineupPlayer(P, I2) = I1
            fielder(P, fldPos(P, I2)) = I1

            If fldPos(P, I2) = 1 Or fldPos(P, I2) = 10 Then 
                fielder(P, 1) = 99
                fielder(P, 0) = I2
                fldPos(P, I2) = 10
            End If

            gameD0(P) = gameD0(P) + 1
            X0%(P, 0, gameD0(P)) = lineupPlayer(P, I2)
            X0%(P, 1, gameD0(P)) = 11
            X0%(P, 2, gameD0(P)) = (I2 * 10) + (gameD0(P) - 9)

            W5 = P
            W6 = P
            VV(P) = I2
            VV1(P) = I2

            For xx = 22 To 25
                If fldPos(P, I2) = batRating(P, I1, xx) Then 
                    batRating(P, I1, SS_B_Arm)    = batRating(P, I1, 33 + (xx - 22) * 2)
                    batRating(P, I1, SS_B_Range)  = batRating(P, I1, 34 + (xx - 22) * 2)
                    batRating(P, I1, SS_B_FldPct) = batRating(P, I1, xx + 4)
                    Exit For
                End If
            Next

            Call WINDEX
            Locate 15, 1
            Call PBP(gameManagers$(P) + " is sending in a pinch runner")
            PR% = 1

            Call DELAY
            Call HITTINGLINES

            If useVGA = 1 Then
                Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
                MS% = 1
            Else
                Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
                MS% = 1
            End If

            VV(P) = 1
            VV1(P) = 9

        End If

    End If

    8999 '

End Sub

'----------------------------------------
'         PITCHERNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'associated pitcher.
Sub PITCHERNAME (currFielder, D)

    If currFielder = 1 Then
        player$(4) = pitchers$(D, currPitcher(D))
    Else
        player$(4) = batters$(D, fielder(D, currFielder))
    End If

End Sub


'----------------------------------------
'          PITCHOUT Subroutine
'----------------------------------------
'This subroutine handles the stats and
'Play-by-Play for a pitchout.
Sub PITCHOUT (baseRunners, TGTBASE%, stlChance)

    If baseRunners <> 5 And TGTBASE% = 2 Then

        Call Rolld100(pitchoutChance)

        If pitchoutChance <= 8 Then
            Call PBP("...PITCH OUT!!!")
            stlChance = stlChance - 200
        End If

    End If

End Sub


'----------------------------------------
'           PITDISP Subroutine
'----------------------------------------
'This subroutine displays an overview of
'a teams' pitchers and ratings/stats.
Sub PITDISP (P9)

    Cls

    Color 14, 0
    Print "#";
    Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12): Print gameTeams$(P9);
    Color 14, 0
    Print " T  IP   H   G GS CG SV HR  BB  SO  W- L   ERA  H G%"

    For pIdx = 0 To 21:

        If pitchRating(P9, pIdx, SS_P_Avail) <= 0 Then
            Color 2, 0

            If pIdx <= 9 Then
                Print Using "#"; pIdx;
            Else
                Print alpha$(pIdx - 10);
            End If

            Color 15, 0

            Locate , 3: Print pitchers$(P9, pIdx);
            Locate , 18: Print handed$(pitchRating(P9, pIdx, SS_P_Throws) + 2);

            Print Using " ### ### ### ## ## ## ## ### ### ##"; pitchRating(P9, pIdx, 6); pitchRating(P9, pIdx, 7); pitchRating(P9, pIdx, 4); pitchRating(P9, pIdx, 5); pitchRating(P9, pIdx, 16); pitchRating(P9, pIdx, 3); pitchRating(P9, pIdx, 13); pitchRating(P9, pIdx, 8); pitchRating(P9, pIdx, 9); pitchRating(P9, pIdx, 1);
            Print "-";
            Print Using "##"; pitchRating(P9, pIdx, 2);

            Locate , 59: Print Using "##.## # ##"; pitchRating(P9, pIdx, SS_P_ERA100) / 100; pitchRating(P9, pIdx, SS_P_HoldRunner); pitchRating(P9, pIdx, SS_P_GndPct)

        End If

    Next pIdx

End Sub


'----------------------------------------
'           PITOFF Subroutine
'----------------------------------------
' ...explanation...
Sub PITOFF (P, I5, I6)

    leagPitStats(P, I5, I6) = leagPitStats(P, I5, I6) - 1
    gamePitStats(P, I5, I6) = gamePitStats(P, I5, I6) - 1

End Sub


'----------------------------------------
'        PITPLACEMENT Subroutine
'----------------------------------------
'This subroutine determines where to place
'the pitcher text and prints the info. at
'that place.
Sub PITPLACEMENT (P, X$)

    If useVGA = 1 Then
        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color &HFFFFFFFF

    End If

    If pitchRating(P, currPitcher(P), SS_P_Throws) < 0 Then
        If useVGA = 1 Then
            PrintOnGfx 42, 25, X$, imageScreen&
        Else
            Locate 21, 64
            Print X$
        End If
    End If

    If pitchRating(P, currPitcher(P), SS_P_Throws) > 0 Then
        If useVGA = 1 Then
            PrintOnGfx 30, 25, X$, imageScreen&
        Else
            Locate 21, 51
            Print X$
        End If
    End If

    If useVGA = 1 Then
        'Printing white on black
        Color &HFFFFFFFF
        _PrintMode _FillBackground , imageScreen&
    End If

End Sub


'----------------------------------------
'           PITSTAM Subroutine
'----------------------------------------
' ...explanation...
Sub PITSTAM (P9)

    If pitchRating(P9, currPitcher(P9), 4) = 0 Then
        S8%(P9, 0) = 3
    Else
        S8%(P9, 0) = CInt(((pitchRating(P9, currPitcher(P9), 8) + pitchRating(P9, currPitcher(P9), 7)) / pitchRating(P9, currPitcher(P9), 4)))
    End If

    If S8%(P9, 0) < 3 Then S8%(P9, 0) = 3
    S8%(P9, 1) = 3

    If gamePitcher(P9) > 1 Then S8%(P9, 1) = 0

    If pitchEraOpt > 0 Then
        If gamePitcher(P9) > 1 Then
            S8%(P9, 1) = 1
        Else
            S8%(P9, 0) = S8%(P9, 0) + 1
        End If
    End If

    If INNING% <= 5 And gameScore(P9, 0) - gameScore(1 - P9, 0) > 0 Then pitcherOfRecord(P9) = currPitcher(P9)

End Sub


'----------------------------------------
'          PITSTRAT Subroutine
'----------------------------------------
'This subroutine loads a pitcher strategy
'from a manager profile.
Sub PITSTRAT (PPH, pz)

    Shared SU()

    A1 = gameScore(D, 0) - gameScore(P, 0)

    pz = 0

    ' if there are no middle relievers in the profile,
    ' and it's less than the 7th inning, then go back
    ' and find one the old fashioned way (which is?)

    If teamMgrProfileVal(D, 60) = 999 Or INNING% > 7 Then

        'if no closers set up or no closing strategy defined, skip out
        If Not (INNING% >= 8 And teamMgrProfileVal(D, 520) <> 999 And teamMgrProfileVal(D, 90) <> 999 Or teamMgrProfileVal(D, 581) <> 999) Then

            'if still needing middle relief
            If INNING% <= 7 Then
                GoTo 8115
            End If

            ' we have defined a closing/setup possibility
            ' identify the stratgey
            I2 = 0
            For pIdx = 582 To 587
                I2 = I2 + teamMgrProfileVal(D, pIdx)
            Next
            I3 = Int(Rnd(1) * I2) + 1
            I2 = 0
            For pIdx = 582 To 587
                I2 = I2 + teamMgrProfileVal(D, pIdx)
                If I3 <= I2 Then
                    SN% = pIdx

                    8111 SF% = 0

                    If A1 < 1 Then Exit Sub

                    If A1 > 0 And A1 <= 2 Then
                        SF% = 1
                    End If
                    If baseRunners = 0 And A1 <= 2 Then
                        SF% = 1
                    End If
                    If baseRunners > 0 And baseRunners < 4 And A1 <= 3 Then
                        SF% = 1
                    End If
                    If baseRunners > 3 And baseRunners < 7 And A1 <= 4 Then
                        SF% = 1
                    End If
                    If baseRunners = 7 And A1 <= 5 Then
                        SF% = 1
                    End If
                    If baseRunners = 0 And nbrOuts(0) = 0 And A1 <= 3 Then
                        SF% = 1
                    End If
                    If currLineupSlot(P) <= 7 Then
                        For pIdx = 0 To 2:
                            I3 = I3 + batRating(P, lineupPlayer(P, currLineupSlot(P) + pIdx), SS_B_Bats)
                        Next
                    End If
                    If currLineupSlot(P) = 8 Then
                        For pIdx = 0 To 1:
                            I3 = I3 + batRating(P, lineupPlayer(P, currLineupSlot(P) + pIdx), SS_B_Bats)
                        Next
                        I3 = I3 + batRating(P, lineupPlayer(P, 1), SS_B_Bats)
                    End If
                    If currLineupSlot(P) = 9 Then
                        For pIdx = 1 To 2:
                            I3 = I3 + batRating(P, lineupPlayer(P, pIdx), SS_B_Bats)
                        Next
                        I3 = I3 + batRating(P, lineupPlayer(P, SS_B_HR), SS_B_Bats)
                    End If

                    If I3 = 0 Then
                        Do
                            I3 = Int(Rnd(1) * 3) - 1
                        Loop Until I3 > 0
                        'Why not just do a random # from 1 to 2???
                    End If

                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 0 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8114
                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 1 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8113
                    If INNING% >= 9 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8113

                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 0 And A1 > 0 And SN% = 583 Then 8114
                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 1 And A1 > 0 And SN% = 583 Then 8113
                    If INNING% >= 9 And A1 > 0 And SN% = 583 Then 8113

                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 0 And A1 >= 0 And SN% = 584 Then 8114
                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 1 And A1 >= 0 And SN% = 584 Then 8113
                    If INNING% >= 9 And A1 >= 0 And SN% = 584 Then 8113

                    If S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) And INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 0 And A1 > 0 And SN% = 585 Then 8114
                    If S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) And INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 1 And A1 > 0 And SN% = 585 Then 8113
                    If S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) And INNING% >= 9 And A1 > 0 And SN% = 585 Then 8113

                    If S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) And INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 0 And A1 >= 0 And SN% = 586 Then 8114
                    If S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) And INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And teamMgrProfileVal(D, 589) = 1 And A1 >= 0 And SN% = 586 Then 8113
                    If S8%(D, 0) <= gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) <= gamePitStats(D, currPitcher(D), 2) And INNING% >= 9 And A1 >= 0 And SN% = 586 Then 8113

                    Exit For

                End If
            Next

        End If

    End If

    Exit Sub

    'strategy identified, now

    8114
    I2 = 0
    If I3 < 0 Then I1 = 81
    If I3 > 0 Then I1 = 84
    For pIdx = I1 To I1 + 2
        If teamMgrProfileVal(D, pIdx) >= 0 And pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
            I2 = I2 + teamMgrProfileVal(D, pIdx + 10)
        End If
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For pIdx = I1 To I1 + 2
        If teamMgrProfileVal(D, pIdx) >= 0 And pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
            I2 = I2 + teamMgrProfileVal(D, pIdx + 10)
        End If
        If I3 <= I2 Then
            V0 = teamMgrProfileVal(D, pIdx)
            teamMgrProfileVal(D, pIdx) = -1
            pIdx = V0
            Call NEWPITCHER(pIdx, PPH, pz)
            SU(D, 0) = -1
            Exit Sub
        End If
    Next
    'NO SETUP MAN FOUND

    8113
    I2 = 0
    If I3 < 0 Then I1 = 521
    If I3 > 0 Then I1 = 524

    For pIdx = I1 To I1 + 2
        If teamMgrProfileVal(D, pIdx) >= 0 And pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
            I2 = I2 + teamMgrProfileVal(D, pIdx + 10)
        End If
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For pIdx = I1 To I1 + 2
        If teamMgrProfileVal(D, pIdx) >= 0 And pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
            I2 = I2 + teamMgrProfileVal(D, pIdx + 10)
        End If
        If I3 <= I2 Then
            V0 = teamMgrProfileVal(D, pIdx)
            teamMgrProfileVal(D, pIdx) = -1
            pIdx = V0
            Call NEWPITCHER(pIdx, PPH, pz)
            SU(D, 1) = -1
            Exit Sub
        End If
    Next

    'NO CLOSER FOUND
    SU(D, 0) = -1
    SU(D, 1) = -1

    8115 '
    I2 = 0

    For pIdx = 61 To 70
        If teamMgrProfileVal(D, pIdx) >= 0 And pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
            I2 = I2 + teamMgrProfileVal(D, pIdx + 10)
        End If
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For pIdx = 61 To 70
        If teamMgrProfileVal(D, pIdx) >= 0 And pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
            I2 = I2 + teamMgrProfileVal(D, pIdx + 10)
        End If
        If I3 <= I2 Then
            V0 = teamMgrProfileVal(D, pIdx)
            teamMgrProfileVal(D, pIdx) = -1
            pIdx = V0
            Call NEWPITCHER(pIdx, PPH, pz)
            Exit Sub
        End If
    Next

    'NO middle RELIEVER FOUND

End Sub


'----------------------------------------
'           PITSWAP Subroutine
'----------------------------------------
' ...explanation...
Sub PITSWAP (i)

    If i = 4 Then
        V = 3
        Call SOMESTATS(V, P, errorOnPlay%)
        V = 2
        Call SOMESTATS(V, P, errorOnPlay%)
        V = 1
        Call SOMESTATS(V, P, errorOnPlay%)
    End If

    If i = 3 Then
        V = 2
        Call SOMESTATS(V, P, errorOnPlay%)
        V = 1
        Call SOMESTATS(V, P, errorOnPlay%)
    End If

    If i = 2 Then
        V = 2
        Call SOMESTATS(V, P, errorOnPlay%)
        V = 1
        Call SOMESTATS(V, P, errorOnPlay%)
    End If

End Sub


'----------------------------------------
'         PLAYERNAME Subroutine
'----------------------------------------
' ...explanation...
Sub PLAYERNAME (P)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        player$(0) = pitchers$(P, currPitcher(P))
    Else
        player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))
    End If

    For pIdx = 1 To 3

        If b2Base(pIdx) > 0 Then
            If fldPos(P, b2Base(pIdx)) <> 1 Then
                player$(pIdx) = batters$(P, lineupPlayer(P, b2Base(pIdx)))
            Else
                player$(pIdx) = pitchers$(P, currPitcher(P))
            End If
        End If

    Next pIdx

End Sub


'----------------------------------------
'           PLYRHRT Subroutine
'----------------------------------------
' Offensive Player Ejected/Injured
Sub PLYRHRT (P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

    If Not (playerMode = 2 Or playerMode = 1 And P = compTeam) Then

        Call PBP("You must replace " + player$(0) + " with a pinch-hitter")

    Else

        For pIdx = 0 To 22

            If batRating(P, pIdx, SS_B_Avail) <= 0 And batters$(P, pIdx) <> "XXX" Then

                If fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos1) Or fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos2) Or fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos3) Or fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos4) Then

                    Call PinchHitter(P, P9, W5, W6, pIdx)

                    If useVGA = 1 Then
                        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
                    Else
                        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO): MS% = 1
                    End If

                    Locate 15, 1

                    player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))
                    If fldPos(P, currLineupSlot(P)) = 1 Then
                        player$(0) = pitchers$(P, currPitcher(D))
                    End If

                    Exit Sub

                End If

            End If

        Next pIdx


        For pIdx = 0 To 22

            If batRating(P, pIdx, SS_B_Avail) <= 0 And batters$(P, pIdx) <> "XXX" Then

                Call PinchHitter(P, P9, W5, W6, pIdx)

                If useVGA = 1 Then
                    Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                    Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
                Else
                    Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                    Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO): MS% = 1
                End If

                Locate 15, 1

                player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))
                If fldPos(P, currLineupSlot(P)) = 1 Then
                    player$(0) = pitchers$(P, currPitcher(D))
                End If

                Exit Sub

            End If

        Next pIdx

    End If

End Sub


'----------------------------------------
'         PinchHitter Subroutine
'----------------------------------------
Sub PinchHitter (P, P9, W5, W6, idx)

    Call PBP(player$(0) + " will be replaced by " + batters$(P, idx))

    gameD0(P) = gameD0(P) + 1
    X0%(P, 0, gameD0(P)) = idx
    X0%(P, 1, gameD0(P)) = fldPos(P, currLineupSlot(P))
    X0%(P, 2, gameD0(P)) = (currLineupSlot(P) * 10) + (gameD0(P) - 9)
    batRating(P, idx, SS_B_Avail) = 1
    lineupPlayer(P, currLineupSlot(P)) = idx
    fielder(P, fldPos(P, currLineupSlot(P))) = idx

    If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then
        fielder(P, 1) = 99
        fielder(P, 0) = currLineupSlot(P)
        fldPos(P, currLineupSlot(P)) = 10
    End If

    P9 = P
    W5 = P9
    W6 = P9
    VV(P9) = currLineupSlot(P)
    VV1(P9) = currLineupSlot(P)
    Call HITTINGLINES

    VV(P9) = 1
    VV1(P9) = 9

End Sub


'----------------------------------------
'        PopUp_Lineup Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team lineup
Sub PopUp_Lineup ()

    If useVGA = 1 Then Screen menu&

    Call FRAMEOPTION(11, 1)

    Print gameTeams$(0); Tab(40); gameTeams$(1)

    For J = 0 To 1

        For X = 1 To 9:

            Locate X + 9, 10 + J * 26
            If currLineupSlot(J) = X Then Print "->";

            Locate , 12 + J * 26
            If fldPos(J, X) = 1 Then

                Print handed$(pitchRating(J, currPitcher(J), SS_P_Throws) + 2); Tab(14 + J * 26); Left$(pitchers$(J, currPitcher(J)), 10); " P"; Tab(28 + J * 26);

                If pitchRating(J, currPitcher(J), SS_P_BAvg) <> 999 Then
                    Print " ."; Using "###"; pitchRating(J, currPitcher(J), SS_P_BAvg) '/ 1000
                Else

                    If pitchRating(J, currPitcher(J), SS_P_H_AB) = 0 Then
                        Print " .000"
                    Else
                        If pitchRating(J, currPitcher(J), SS_P_H_AB) = pitchRating(J, currPitcher(J), SS_P_H_H) Then
                            Print "1.000"
                        Else
                            Print " ."; Using "###"; pitchRating(J, currPitcher(J), SS_P_H_H) / pitchRating(J, currPitcher(J), SS_P_H_AB)
                        End If
                    End If

                End If

            Else

                Print handed$(batRating(J, lineupPlayer(J, X), SS_B_Bats) + 2); Tab(14 + J * 26); Left$(batters$(J, lineupPlayer(J, X)), 10); " "; fieldPos$(fldPos(J, X)); Tab(28 + J * 26);

                If batRating(J, lineupPlayer(J, X), SS_B_AB) = 0 Then
                    Print " .000"
                Else
                    If batRating(J, lineupPlayer(J, X), SS_B_H) = batRating(J, lineupPlayer(J, X), SS_B_AB) Then
                        Print "1.000"
                    Else
                        Print Using " .###"; batRating(J, lineupPlayer(J, X), SS_B_H) / batRating(J, lineupPlayer(J, X), SS_B_AB)
                    End If
                End If

            End If

        Next

    Next

    Color 15, 0
    Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'        PopUp_Batter Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team batters
Sub PopUp_Batter (WK, M)

    If teamSplit(WK, 0) = 999 Then

        Locate 4, 56: Print "VS LHP  .000";
        If batRating(WK, M, SS_B_LHP_AB) <> 0 Then
            statPCT! = batRating(WK, M, SS_B_LHP_H) / batRating(WK, M, SS_B_LHP_AB)
            Locate 4, 64: Print Using ".###"; statPCT!
        End If

        Locate 5, 56: Print "VS RHP  .000";
        If batRating(WK, M, SS_B_RHP_AB) <> 0 Then
            statPCT! = batRating(WK, M, SS_B_RHP_H) / batRating(WK, M, SS_B_RHP_AB)
            Locate 5, 64: Print Using ".###"; statPCT!
        End If

    End If

    Locate 8, 6: Print "SEASON";
    Locate , 15

    If batRating(WK, M, SS_B_AB) = 0 Then
        Print ".---  .---"
    Else
        statPCT!  =  batRating(WK, M, SS_B_H) / batRating(WK, M, SS_B_AB)
        statPCT2! = (batRating(WK, M, SS_B_H) + batRating(WK, M, SS_B_W)) / (batRating(WK, M, SS_B_AB) + batRating(WK, M, SS_B_W))
        Print Using ".###  .### ### ### ### ### ## ## ## ### ### ### "; statPCT!; statPCT2!; batRating(WK, M, SS_B_G); batRating(WK, M, SS_B_AB); batRating(WK, M, SS_B_R); batRating(WK, M, SS_B_H); batRating(WK, M, SS_B_2B); batRating(WK, M, SS_B_3B); batRating(WK, M, SS_B_HR); batRating(WK, M, SS_B_RBI);
        Print Using "### ### ### ##"; batRating(WK, M, SS_B_W); batRating(WK, M, SS_B_K); batRating(WK, M, SS_B_SB); batRating(WK, M, SS_B_CS)
    End If

    Locate , 6: Print "LEAGUE";

    If leagBatStats(WK, M, 0) = 0 Or leagBatStats(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If leagBatStats(WK, M, 0) = leagBatStats(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = leagBatStats(WK, M, 1) / leagBatStats(WK, M, 0)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If leagBatStats(WK, M, 0) + leagBatStats(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If leagBatStats(WK, M, 1) + leagBatStats(WK, M, 7) = leagBatStats(WK, M, 0) + leagBatStats(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            statPCT! = (leagBatStats(WK, M, 1) + leagBatStats(WK, M, 7)) / (leagBatStats(WK, M, 0) + leagBatStats(WK, M, 7))
            Locate , 21: Print Using ".###"; statPCT!;
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; leagBatStats(WK, M, 14); leagBatStats(WK, M, 0); leagBatStats(WK, M, 2); leagBatStats(WK, M, 1); leagBatStats(WK, M, 4); leagBatStats(WK, M, 5); leagBatStats(WK, M, 6); leagBatStats(WK, M, 3); leagBatStats(WK, M, 7); leagBatStats(WK, M, 8); leagBatStats(WK, M, 9); leagBatStats(WK, M, 10); leagBatStats(WK, M, 12); leagBatStats(WK, M, 13)

    Locate , 6: Print "GAME";

    If gameBatStats(WK, M, 0) = 0 Or gameBatStats(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If gameBatStats(WK, M, 0) = gameBatStats(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = gameBatStats(WK, M, 1) / gameBatStats(WK, M, 0)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If gameBatStats(WK, M, 0) + gameBatStats(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If gameBatStats(WK, M, 1) + gameBatStats(WK, M, 7) = gameBatStats(WK, M, 0) + gameBatStats(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            Locate , 21: Print Using ".###"; (gameBatStats(WK, M, 1) + gameBatStats(WK, M, 7)) / (gameBatStats(WK, M, 0) + gameBatStats(WK, M, 7));
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; gameBatStats(WK, M, 14); gameBatStats(WK, M, 0); gameBatStats(WK, M, 2); gameBatStats(WK, M, 1); gameBatStats(WK, M, 4); gameBatStats(WK, M, 5); gameBatStats(WK, M, 6); gameBatStats(WK, M, 3); gameBatStats(WK, M, 7); gameBatStats(WK, M, 8); gameBatStats(WK, M, 9); gameBatStats(WK, M, 10); gameBatStats(WK, M, 12); gameBatStats(WK, M, 13)

End Sub


'----------------------------------------
'        PopUp_Pitcher Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team pitchers
Sub PopUp_Pitcher (WK, M)

    Locate 14, 3
    Color teamRat_GAME(WK, 11), teamRat_GAME(WK, 12)

    Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

    For X = 1 To 7:
        Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " "
    Next

    Locate , 3: Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

    If teamSplit(WK, 0) = 999 Then

        Locate 15, 56: Print "VS LHB  .000";
        If pitchRating(WK, M, SS_P_LHP_AB) <> 0 Then
            statPCT! = pitchRating(WK, M, SS_P_LHP_H) / pitchRating(WK, M, SS_P_LHP_AB)
            Locate 15, 64: Print Using ".###"; statPCT!
        End If

        Locate 16, 56: Print "VS RHB  .000";
        If pitchRating(WK, M, SS_P_RHP_AB) <> 0 Then
            statPCT! = pitchRating(WK, M, SS_P_RHP_H) / pitchRating(WK, M, SS_P_RHP_AB)
            Locate 16, 64: Print Using ".###"; statPCT!
        End If

    End If

    Locate 15, 6: Print "Stats for "; pitchers$(WK, M)
    Locate , 6: Print "THROWS: "; handed$(pitchRating(WK, M, SS_P_Throws) + 2); "    ";
    Print Using "G/A%: ##     HOLD: #"; pitchRating(WK, M, SS_P_GndPct); pitchRating(WK, M, SS_P_HoldRunner)
    Locate , 3
    Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
    Locate , 6
    Print "          W  L SV   ERA  G GS CG   IP    H  BB  SO   R  ER HR"

    Locate , 6: Print "SEASON   ";
    Print Using "## ## ## ##.## ## ## ## ###   ### "; pitchRating(WK, M, SS_P_W); pitchRating(WK, M, SS_P_L); pitchRating(WK, M, SS_P_Sv); pitchRating(WK, M, SS_P_ERA100) / 100; pitchRating(WK, M, SS_P_G); pitchRating(WK, M, SS_P_GS); pitchRating(WK, M, SS_P_CG); pitchRating(WK, M, SS_P_IP); pitchRating(WK, M, SS_P_H);
    Print Using "### ###         ##"; pitchRating(WK, M, SS_P_BB); pitchRating(WK, M, SS_P_K); pitchRating(WK, M, SS_P_HR)

    Locate , 6: Print "LEAGUE   ";
    Print Using "## ## ## "; leagPitStats(WK, M, 6); leagPitStats(WK, M, 7); leagPitStats(WK, M, 12);

    If leagPitStats(WK, M, 3) = 0 Or leagPitStats(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; leagPitStats(WK, M, 3) / (leagPitStats(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; leagPitStats(WK, M, 8); leagPitStats(WK, M, 9); leagPitStats(WK, M, 10);

    If leagPitStats(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(leagPitStats(WK, M, 0) / 3);
        Print ".";: Print Using "#"; leagPitStats(WK, M, 0) - (Int(leagPitStats(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; leagPitStats(WK, M, 1); leagPitStats(WK, M, 4); leagPitStats(WK, M, 5); leagPitStats(WK, M, 2); leagPitStats(WK, M, 3); leagPitStats(WK, M, 14)

    Locate , 6: Print "GAME     ";
    Print Using "## ## ## "; gamePitStats(WK, M, 6); gamePitStats(WK, M, 7); gamePitStats(WK, M, 12);

    If gamePitStats(WK, M, 3) = 0 Or gamePitStats(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; gamePitStats(WK, M, 3) / (gamePitStats(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; gamePitStats(WK, M, 8); gamePitStats(WK, M, 9); gamePitStats(WK, M, 10);

    If gamePitStats(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(gamePitStats(WK, M, 0) / 3);
        Print ".";: Print Using "#"; gamePitStats(WK, M, 0) - (Int(gamePitStats(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; gamePitStats(WK, M, 1); gamePitStats(WK, M, 4); gamePitStats(WK, M, 5); gamePitStats(WK, M, 2); gamePitStats(WK, M, 3); gamePitStats(WK, M, 14)

End Sub

'----------------------------------------
'         POPUPSTATS Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team stats.
'It also gives an interface for viewing
'batters and pitchers.
Sub POPUPSTATS ()

    If useVGA = 1 Then Screen menu&

    J = lineupPlayer(P, currLineupSlot(P))

    'Stats for Active Batter
    Locate 3, 3: Color teamRat_GAME(P, 11), teamRat_GAME(P, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

    For X = 1 To 7:
        Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " "
    Next

    Locate , 3: Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

    Locate 4, 6: Print "Stats for ";

    If fldPos(P, currLineupSlot(P)) = 1 Then
        If pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then
            Print pitchers$(P, currPitcher(P))
        Else
            Print pitchers$(P, currPitcher(P))
            Locate , 6: Print "BATS: "; handed$(pitchRating(P, currPitcher(P), SS_P_Throws) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; 52; pitchRating(P, currPitcher(P), SS_P_Bunt); pitchRating(P, currPitcher(P), SS_P_Run):
        End If
    Else
        Print batters$(P, J)
        Locate , 6: Print "BATS: "; handed$(batRating(P, J, SS_B_Bats) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; batRating(P, J, SS_B_GndPct); batRating(P, J, SS_B_Bunt); batRating(P, J, SS_B_Run)
    End If

    If fldPos(P, currLineupSlot(P)) = 1 And teamSplit(P, 0) = 999 Then

        Locate 4, 56: Print "VS LHP  .000";
        If pitchRating(P, currPitcher(P), 37) <> 0 Then
            statPCT! = pitchRating(P, currPitcher(P), 38) / pitchRating(P, currPitcher(P), 37)
            Locate 4, 64: Print Using ".###"; statPCT!
        End If

        Locate 5, 56: Print "VS RHP  .000";
        If pitchRating(P, currPitcher(P), 44) <> 0 Then
            statPCT! = pitchRating(P, currPitcher(P), 45) / pitchRating(P, currPitcher(P), 44)
            Locate 5, 64: Print Using ".###"; statPCT!
        End If

    End If

    Locate 6, 3
    Print " "; Chr$(204); String$(71, 205); Chr$(185); " "

    Locate , 6
    Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

    skipLeague = 0
    If fldPos(P, currLineupSlot(P)) = 1 And pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then

        statPCT! = pitchRating(P, currPitcher(P), SS_P_BAvg) '/ 1000
        Locate , 6: Print "SEASON";
        Locate , 15: Print Using ".###"; statPCT!

    Else

        If fldPos(P, currLineupSlot(P)) <> 1 Then

            WK = P
            M = J
            Call PopUp_Batter(WK, M)
            skipLeague = 1

        Else

            Locate , 6: Print "SEASON";: Locate , 15

            If pitchRating(P, currPitcher(P), SS_P_H_AB) = 0 Then
                Print ".---  .---"
            Else
                Print Using ".###  .### ### ### ### ### ## "; pitchRating(P, currPitcher(P), SS_P_H_H) / pitchRating(P, currPitcher(P), SS_P_H_AB); (pitchRating(P, currPitcher(P), SS_P_H_H) + pitchRating(P, currPitcher(P), SS_P_H_BB)) / (pitchRating(P, currPitcher(P), SS_P_H_BB) + pitchRating(P, currPitcher(P), SS_P_H_AB)); pitchRating(P, currPitcher(P), 19); pitchRating(P, currPitcher(P), SS_P_H_AB); pitchRating(P, currPitcher(P), 21); pitchRating(P, currPitcher(P), SS_P_H_H); pitchRating(P, currPitcher(P), SS_P_H_2B);
                Print Using "## ## ### ### ### ### ##"; pitchRating(P, currPitcher(P), SS_P_H_3B); pitchRating(P, currPitcher(P), SS_P_H_HR); pitchRating(P, currPitcher(P), SS_P_H_RBI); pitchRating(P, currPitcher(P), SS_P_H_BB); pitchRating(P, currPitcher(P), SS_P_H_SO); pitchRating(P, currPitcher(P), SS_P_H_SB); pitchRating(P, currPitcher(P), SS_P_H_CS)
            End If

        End If

    End If

    If skipLeague <> 1 Then

        Locate , 6: Print "LEAGUE";

        J = currPitcher(P)

        If leagPitStats(P, J, 23) = 0 Or leagPitStats(P, J, 24) = 0 Then
            Locate , 15: Print ".000";
        Else
            If leagPitStats(P, J, 23) = leagPitStats(P, J, 24) Then
                Locate , 14: Print "1.000";
            Else
                statPCT! = leagPitStats(P, J, 24) / leagPitStats(P, J, 23)
                Locate , 15: Print Using ".###"; statPCT!;
            End If
        End If

        If leagPitStats(P, J, 23) + leagPitStats(P, J, 30) = 0 Then
            Locate , 21: Print ".000";
        Else
            If leagPitStats(P, J, 24) + leagPitStats(P, J, 30) = leagPitStats(P, J, 23) + leagPitStats(P, J, 30) Then
                Locate , 20: Print "1.000";
            Else
                statPCT! = (leagPitStats(P, J, 24) + leagPitStats(P, J, 30)) / (leagPitStats(P, J, 23) + leagPitStats(P, J, 30))
                Locate , 21: Print Using ".###"; statPCT!;
            End If
        End If

        Locate , 26: Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; leagPitStats(P, J, 8); leagPitStats(P, J, 23); leagPitStats(P, J, 25); leagPitStats(P, J, 24); leagPitStats(P, J, 27); leagPitStats(P, J, 28); leagPitStats(P, J, 29); leagPitStats(P, J, 26); leagPitStats(P, J, 30); leagPitStats(P, J, 31); leagPitStats(P, J, 32); leagPitStats(P, J, 33); leagPitStats(P, J, 35); leagPitStats(P, J, 36)
        Locate , 6: Print "GAME";

        If gamePitStats(P, J, 23) = 0 Or gamePitStats(P, J, 24) = 0 Then
            Locate , 15: Print ".000";
        Else
            If gamePitStats(P, J, 23) = gamePitStats(P, J, 24) Then
                Locate , 14: Print "1.000";
            Else
                statPCT! = gamePitStats(P, J, 24) / gamePitStats(P, J, 23)
                Locate , 15: Print Using ".###"; statPCT!;
            End If
        End If

        If gamePitStats(P, J, 23) + gamePitStats(P, J, 30) = 0 Then
            Locate , 21: Print ".000";
        Else
            If gamePitStats(P, J, 24) + gamePitStats(P, J, 30) = gamePitStats(P, J, 23) + gamePitStats(P, J, 30) Then
                Locate , 20: Print "1.000";
            Else
                statPCT! = (gamePitStats(P, J, 24) + gamePitStats(P, J, 30)) / (gamePitStats(P, J, 23) + gamePitStats(P, J, 30))
                Locate , 21: Print Using ".###"; statPCT!;
            End If
        End If

        Locate , 26
        Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; gamePitStats(P, J, 8); gamePitStats(P, J, 23); gamePitStats(P, J, 25); gamePitStats(P, J, 24); gamePitStats(P, J, 27); gamePitStats(P, J, 28); gamePitStats(P, J, 29); gamePitStats(P, J, 26); gamePitStats(P, J, 30); gamePitStats(P, J, 31); gamePitStats(P, J, 32); gamePitStats(P, J, 33); gamePitStats(P, J, 35); gamePitStats(P, J, 36)

    End If

    WK = D
    M = currPitcher(D)
    Call PopUp_Pitcher(WK, M)

    Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$
    Color 7, 0

End Sub

'----------------------------------------
'           PUTOUT Subroutine
'----------------------------------------
' ...explanation...
Sub PUTOUT (i, P)

    If fldPos(P, b2Base(i - 1)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_CS)
    Else
        Call INCBATOFF(P, lineupPlayer(P, b2Base(i - 1)), LG_B_CS)
    End If

End Sub


'----------------------------------------
'          RecordOut Subroutine
'----------------------------------------
'This subroutine handles the recording of
'an out, along with the Play-by-Play
Sub RecordOut (i, baseNbr)

    Call PBP(player$(i) + " out @ " + baseName$(baseNbr))

    b2Base(i) = 0
    b2Pitcher(i) = 0
    b2Mystery(i) = 0
    player$(i) = ""

End Sub


'----------------------------------------
'          RecordRun Subroutine
'----------------------------------------
' ...explanation...
Sub RecordRun (sah%, i, D, errorOnPlay%, PQ, S2%, currFielder, INFPOS%)

    If sah% = 0 Then
        Call PBP(player$(i) + " scores")
    End If
    sah% = 0

    Call ADDPIT(D, b2Pitcher(i), LG_P_R)

    If currPitcher(D) <> b2Pitcher(i) Then
        Call ADDPIT(D, currPitcher(D), 18)
    End If

    If gamePitcher(P) = 1 Then
        gamePitStats(P, currPitcher(P), 20) = gamePitStats(P, currPitcher(P), 20) + 1
    End If

    If fldPos(P, b2Base(i)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_R)
    Else
        Call INCBATOFF(P, lineupPlayer(P, b2Base(i)), LG_B_R)
    End If

    If errorOnPlay% = 1 Then
        If nbrOuts(0) = 2 Then
            PQ = 8
        Else
            Select Case currFielder

                Case Is <= 3:
                    If S2% <> EVENT_BUNT Or errorOnPlay% = 1 And currFielder = 5 And S2% <> 23 Then
                        PQ = 8
                    End If

                Case 4, 6
                    If INFPOS% = INF_IN Then
                        PQ = 8
                    End If

                Case Else
                    If S2% = EVENT_BALK Or S2% = EVENT_PB Or S2% = EVENT_WP Then
                        PQ = 8
                    End If

            End Select

        End If
    End If

    If PQ <> 8 Then

        If fldPos(P, currLineupSlot(P)) = 1 Then
            Call INCPITOFF(P, currPitcher(P), LG_PB_RBI)
        Else
            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_RBI)
        End If

        If errorOnPlay% = 1 Then
            PQ = 8
        End If

    End If

    inningScore(P, INNING%) = inningScore(P, INNING%) + 1
    gameScore(P, 0) = gameScore(P, 0) + 1

    If gameScore(P, 0) - gameScore(D, 0) = 1 Then
        pitcherOfRecord(D) = b2Pitcher(i)
        pitcherOfRecord(P) = currPitcher(P)
        S1%(1) = -1
        S1%(0) = -1
    Else
        If gameScore(P, 0) - gameScore(D, 0) = 0 And S1%(D) <> -1 Then
            Call ADDPIT(D, currPitcher(D), LG_P_BSv)
            S1%(D) = -1
        End If
    End If

    b2Base(i) = 0
    b2Pitcher(i) = 0
    b2Mystery(i) = 0
    player$(i) = ""

    If inningScore(P, INNING%) = 4 And d100% <= 25 Then
        Call PBP("They're tearin' up the peapatch!")
    End If

    If INNING% >= 9 And P = 1 And gameScore(P, 0) - gameScore(D, 0) = 1 Then

        If S2% <> EVENT_HR Then
            If errorOnPlay% = 0 And nbrOuts(1) < 3 Then
                Call CHARGERUN(D, i)
            End If

            A5%(0) = 0
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0
        End If


    End If

End Sub


'----------------------------------------
'          DISPBAVG Subroutine
'----------------------------------------
'This routine displays the batting average
'on the field (traditional mode) for the
'active batter
Sub DISPBAVG (D)

    If INNING% > 5 And S8%(D, 0) < gamePitStats(D, currPitcher(D), 4) + gamePitStats(D, currPitcher(D), 1) And S8%(D, 1) < gamePitStats(D, currPitcher(D), 2) Then
        Locate 10, 17
        Color 30, 0: Print "-T-"
        Color 15, 0
    End If

    Locate 5, 1

    For X = 1 To 9

        Color 3, 0
        Locate X + 4, 22

        If currLineupSlot(P) = X Then Color 11, 0

        Locate , 22

        If fldPos(P, X) = 1 Then

            Print handed$(pitchRating(P, currPitcher(P), SS_P_Throws) + 2); Tab(25); Left$(pitchers$(P, currPitcher(P)), 10); " P"; Tab(39);

            If pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then
                If pitchRating(P, currPitcher(P), SS_P_BAvg) = 0 Then
                    Print " .000"
                Else
                    Print " ."; Using "###"; pitchRating(P, currPitcher(P), SS_P_BAvg)
                End If

            Else

                If pitchRating(P, currPitcher(P), SS_P_H_AB) = 0 Then
                    Print " .000"
                Else
                    If pitchRating(P, currPitcher(P), SS_P_H_AB) = pitchRating(P, currPitcher(P), SS_P_H_H) Then
                        Print "1.000"
                    Else
                        pAvg! = (pitchRating(P, currPitcher(P), SS_P_H_H) / pitchRating(P, currPitcher(P), SS_P_H_AB)) * 1000
                        Print " ."; Using "###"; pAvg!
                    End If

                End If

            End If

        Else

            Print handed$(batRating(P, lineupPlayer(P, X), SS_B_Bats) + 2); Tab(25); Left$(batters$(P, lineupPlayer(P, X)), 10); " "; fieldPos$(fldPos(P, X)); Tab(39);

            If batRating(P, lineupPlayer(P, X), SS_B_AB) = 0 Then
                Print " .000"
            Else
                If batRating(P, lineupPlayer(P, X), SS_B_H) = batRating(P, lineupPlayer(P, X), SS_B_AB) Then
                    Print "1.000"
                Else
                    bAvg! = (batRating(P, lineupPlayer(P, X), SS_B_H) / batRating(P, lineupPlayer(P, X), SS_B_AB)) * 1000
                    Print " ."; Using "###"; bAvg!
                End If
            End If

        End If

    Next

    Color 15, 0

End Sub


'----------------------------------------
'        HITTINGLINES Subroutine
'----------------------------------------
' ...explanation...
Sub HITTINGLINES

    Randomize Timer

    'CLS
    'PRINT "Please Wait..."
    'PRINT
    'PRINT "Calculating Pitcher/Batter Park Effects..."

    For I = 0 To 1

        I1 = 1 - I

        'CALCULATE BASE ON BALLS PERCENTAGES
        For I3 = VV(I) To VV1(I)

            I6! = teamRat_GAME(I1, 3) / 1000

            If pitchRating(I1, currPitcher(I1), 6) * 3 * .94 + pitchRating(I1, currPitcher(I1), 7) + pitchRating(I1, currPitcher(I1), 8) = 0 Then
                I2! = 0
            Else
                I2! = pitchRating(I1, currPitcher(I1), 8) / (pitchRating(I1, currPitcher(I1), 6) * 3 * .94 + pitchRating(I1, currPitcher(I1), 7) + pitchRating(I1, currPitcher(I1), 8))
                'I2! = pitchRating(I1, currPitcher(I1), 8) / (((pitchRating(I1, currPitcher(I1), 6) * 3) * .94) + pitchRating(I1, currPitcher(I1), 7) + pitchRating(I1, currPitcher(I1), 8))
            End If

            II = batRating(I, lineupPlayer(I, I3), SS_B_Bats)
            I4 = pitchRating(I1, currPitcher(I1), SS_P_Throws)

            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = 1 Then
                TY% = 3
            End If

            If fldPos(I, I3) <> 1 And II < 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II < 0 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = 1 Then
                TY% = 3
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = -1 Then
                TY% = 2
            End If

            If TY% = 0 Or TY% = 1 Then
                PY% = 0
            End If
            If TY% = 2 Or TY% = 3 Then
                PY% = 1
            End If
            If TY% = 0 Or TY% = 2 Then
                BY% = 0
            End If
            If TY% = 1 Or TY% = 3 Then
                BY% = 1
            End If

            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then
                I6! = gameSA(I1, TY% * 6 + 3) / 1000
                I2! = pitchRating(I1, currPitcher(I1), 56 + PY% * 7) * .9 / (pitchRating(I1, currPitcher(I1), 51 + PY% * 7) + pitchRating(I1, currPitcher(I1), 56 + PY% * 7) * .9)
            End If

            If fldPos(I, I3) = 1 Then

                If pitchRating(I, currPitcher(I), SS_P_BAvg) <> 999 Then
                    I7! = .037
                Else
                    If (pitchRating(I, currPitcher(I), SS_P_H_BB) + pitchRating(I, currPitcher(I), SS_P_H_AB) = 0) Then
                        I7! = 0
                    Else
                        If teamSplit(I, 0) = 999 Then
                            If pitchRating(I, currPitcher(I), 37 + BY% * 7) + pitchRating(I, currPitcher(I), 42 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = pitchRating(I, currPitcher(I), 42 + BY% * 7) * .9 / (pitchRating(I, currPitcher(I), 42 + BY% * 7) * .9 + pitchRating(I, currPitcher(I), 37 + BY% * 7))
                            End If
                        Else
                            I7! = pitchRating(I, currPitcher(I), SS_P_H_BB) / (pitchRating(I, currPitcher(I), SS_P_H_BB) + pitchRating(I, currPitcher(I), SS_P_H_AB))
                        End If
                    End If

                End If

            Else

                If batRating(I, lineupPlayer(I, I3), SS_B_AB) + batRating(I, lineupPlayer(I, I3), SS_B_W) = 0 Then
                    I7! = 0
                Else

                    If teamSplit(I, 0) = 999 Then
                        If batRating(I, lineupPlayer(I, I3), 47 + BY% * 7) + batRating(I, lineupPlayer(I, I3), 42 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            If batRating(I, lineupPlayer(I, I3), 80) <> 0 Then
                                I7! = batRating(I, lineupPlayer(I, I3), 47 + BY% * 7) * .9 / (batRating(I, lineupPlayer(I, I3), 42 + BY% * 7) + batRating(I, lineupPlayer(I, I3), 47 + BY% * 7) * .9)
                            End If
                        End If
                    Else
                        I7! = batRating(I, lineupPlayer(I, I3), SS_B_W) / (batRating(I, lineupPlayer(I, I3), SS_B_AB) + batRating(I, lineupPlayer(I, I3), SS_B_W))
                    End If

                End If

            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            game_batRating(I, GM_BB, I3) = K8!

        Next I3

        ' CALCULATE STRIKEOUT PERCENTAGES
        For I3 = VV(I) To VV1(I)

            If pitchRating(I1, currPitcher(I1), 6) = 0 Then
                I2! = 0
            Else
                I2! = pitchRating(I1, currPitcher(I1), 9) / ((pitchRating(I1, currPitcher(I1), 6) * 3) * .94)
            End If

            I6! = teamRat_GAME(I1, 2) / 1000

            II = batRating(I, lineupPlayer(I, I3), SS_B_Bats)
            I4 = pitchRating(I1, currPitcher(I1), SS_P_Throws)
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = 1 Then
                TY% = 3
            End If
            If fldPos(I, I3) <> 1 And II < 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II < 0 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = 1 Then
                TY% = 3
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = -1 Then
                TY% = 2
            End If
            If TY% = 0 Or TY% = 1 Then
                PY% = 0
            End If
            If TY% = 2 Or TY% = 3 Then
                PY% = 1
            End If
            If TY% = 0 Or TY% = 2 Then
                BY% = 0
            End If
            If TY% = 1 Or TY% = 3 Then
                BY% = 1
            End If

            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then

                If pitchRating(I1, currPitcher(I1), 51 + PY% * 7) - pitchRating(I1, currPitcher(I1), 52 + PY% * 7) = 0 Then
                    I2! = 0
                Else
                    I2! = pitchRating(I1, currPitcher(I1), 57 + PY% * 7) / (pitchRating(I1, currPitcher(I1), 51 + PY% * 7) - pitchRating(I1, currPitcher(I1), 52 + PY% * 7))
                    I6! = gameSA(I1, TY% * 6 + 2) / 1000
                End If

            End If

            If fldPos(I, I3) = 1 Then

                If pitchRating(I, currPitcher(I), SS_P_BAvg) <> 999 Then
                    I7! = .42
                Else

                    If pitchRating(I, currPitcher(I), SS_P_H_AB) - pitchRating(I, currPitcher(I), SS_P_H_H) = 0 Then
                        I7! = 0
                    Else

                        If teamSplit(I, 0) = 999 Then
                            If pitchRating(I, currPitcher(I), 37 + BY% * 7) - pitchRating(I, currPitcher(I), 38 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = pitchRating(I, currPitcher(I), 43 + BY% * 7) / (pitchRating(I, currPitcher(I), 37 + BY% * 7) - pitchRating(I, currPitcher(I), 38 + BY% * 7))
                            End If
                        Else
                            If fldPos(I, I3) = 1 Then
                                I7! = pitchRating(I, currPitcher(I), SS_P_H_SO) / (pitchRating(I, currPitcher(I), SS_P_H_AB) - pitchRating(I, currPitcher(I), SS_P_H_H))
                            End If
                        End If

                    End If

                End If

            Else

                If batRating(I, lineupPlayer(I, I3), SS_B_AB) = 0 Or batRating(I, lineupPlayer(I, I3), SS_B_AB) - batRating(I, lineupPlayer(I, I3), SS_B_H) = 0 Then
                    I7! = 0
                Else

                    If teamSplit(I, 0) = 999 Then

                        If batRating(I, lineupPlayer(I, I3), 80) <> 0 Then
                            If batRating(I, lineupPlayer(I, I3), 42 + BY% * 7) - batRating(I, lineupPlayer(I, I3), 43 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = batRating(I, lineupPlayer(I, I3), 48 + BY% * 7) / (batRating(I, lineupPlayer(I, I3), 42 + BY% * 7) - batRating(I, lineupPlayer(I, I3), 43 + BY% * 7))
                            End If
                        End If
                    Else
                        I7! = batRating(I, lineupPlayer(I, I3), SS_B_K) / (batRating(I, lineupPlayer(I, I3), SS_B_AB) - batRating(I, lineupPlayer(I, I3), SS_B_H))
                    End If

                End If

            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            game_batRating(I, GM_K, I3) = K8!

        Next I3

        ' CALCULATE THE BATTING AVERAGE
        For I3 = VV(I) To VV1(I)

            I2! = teamRat_GAME(I1, 1) / 1000
            I6! = pitchRating(I1, currPitcher(I1), 14) / 1000

            If pitchRating(I1, currPitcher(I1), 6) + pitchRating(I1, currPitcher(I1), 7) = 0 Then
                I7! = 0
            Else
                I7! = pitchRating(I1, currPitcher(I1), 7) / (((pitchRating(I1, currPitcher(I1), 6) * 3) * .94) + pitchRating(I1, currPitcher(I1), 7))
            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            i9! = K8! / 1000
            II = batRating(I, lineupPlayer(I, I3), SS_B_Bats)
            I4 = pitchRating(I1, currPitcher(I1), SS_P_Throws)
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = 1 Then
                TY% = 3
            End If
            If fldPos(I, I3) <> 1 And II < 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II < 0 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = 1 Then
                TY% = 3
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = -1 Then
                TY% = 2
            End If
            If TY% = 0 Or TY% = 1 Then
                PY% = 0
            End If
            If TY% = 2 Or TY% = 3 Then
                PY% = 1
            End If
            If TY% = 0 Or TY% = 2 Then
                BY% = 0
            End If
            If TY% = 1 Or TY% = 3 Then
                BY% = 1
            End If

            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then

                I6! = pitchRating(I1, currPitcher(I1), TY% * 6 + 65) / 1000

                If pitchRating(I1, currPitcher(I1), 51 + PY% * 7) = 0 Then
                    I7! = 0
                Else
                    I7! = pitchRating(I1, currPitcher(I1), 52 + PY% * 7) / pitchRating(I1, currPitcher(I1), 51 + PY% * 7)
                End If

            End If

            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then
                I2! = gameSA(I1, TY% * 6 + 1) / 1000
            End If
            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then
                Call NORMALIZE(I7!, I2!, I6!, K8!)
            End If
            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then
                i9! = K8! / 1000
            End If

            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_BAvg) <> 999 Then

                I7! = pitchRating(I, currPitcher(I), SS_P_BAvg) / 1000: I6! = pitchRating(I, currPitcher(I), 14) / 1000

            Else

                If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_H_AB) = 0 Then
                    I7! = 0
                Else

                    If fldPos(I, I3) = 1 And teamSplit(I, 0) = 999 Then
                        If pitchRating(I, currPitcher(I), 37 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            I7! = pitchRating(I, currPitcher(I), 38 + BY% * 7) / pitchRating(I, currPitcher(I), 37 + BY% * 7)
                        End If
                    Else
                        If fldPos(I, I3) = 1 Then I7! = pitchRating(I, currPitcher(I), SS_P_H_H) / pitchRating(I, currPitcher(I), SS_P_H_AB)
                    End If

                End If

                If fldPos(I, I3) = 1 And teamSplit(I, 0) = 999 Then

                    I6! = pitchRating(I, currPitcher(I), TY% * 6 + 65) / 1000
                Else

                    If fldPos(I, I3) = 1 Then
                        I6! = pitchRating(I, currPitcher(I), 14) / 1000
                    Else
                        If batRating(I, lineupPlayer(I, I3), SS_B_AB) = 0 Then
                            I7! = 0
                        Else

                            If teamSplit(I, 0) = 999 And batRating(I, lineupPlayer(I, I3), 80) <> 0 Then
                                If batRating(I, lineupPlayer(I, I3), 42 + BY% * 7) = 0 Then
                                    I7! = 0
                                Else
                                    I7! = batRating(I, lineupPlayer(I, I3), 43 + BY% * 7) / batRating(I, lineupPlayer(I, I3), 42 + BY% * 7)
                                End If
                            Else
                                I7! = batRating(I, lineupPlayer(I, I3), SS_B_H) / batRating(I, lineupPlayer(I, I3), SS_B_AB)
                            End If

                        End If

                        I6! = batRating(I, lineupPlayer(I, I3), SS_B_LgBAvg) / 1000
                        If teamSplit(I, 0) = 999 And batRating(I, lineupPlayer(I, I3), 80) <> 0 Then
                            I6! = batRating(I, lineupPlayer(I, I3), 56 + TY% * 6) / 1000
                        End If

                    End If

                End If

            End If

            I2! = teamRat_GAME(I, 1) / 1000
            If teamSplit(I, 0) = 999 And batRating(I, lineupPlayer(I, I3), 80) <> 0 Then
                I2! = gameSA(I, TY% * 6 + 1) / 1000
            End If
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            I7! = K8! / 1000
            I6! = teamRat_GAME(I, 1) / 1000

            If teamSplit(I, 0) = 999 And batRating(I, lineupPlayer(I, I3), 80) <> 0 Then
                I6! = gameSA(I, TY% * 6 + 1) / 1000
            End If

            I2! = i9!
            Call NORMALIZE(I7!, I2!, I6!, K8!)
            game_batRating(I, GM_BAvg, I3) = K8!

        Next I3

        ' CALCULATE THE EXTRA BASE HITS RATIOS
        For I3 = VV(I) To VV1(I)

            II = batRating(I, lineupPlayer(I, I3), SS_B_Bats)
            I4 = pitchRating(I1, currPitcher(I1), SS_P_Throws)

            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And I4 = 1 Then
                TY% = 3
            End If

            If fldPos(I, I3) <> 1 And II < 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II < 0 And I4 = -1 Then
                TY% = 0
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = -1 Then
                TY% = 2
            End If
            If fldPos(I, I3) <> 1 And II > 0 And I4 = 1 Then
                TY% = 3
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = 1 Then
                TY% = 1
            End If
            If fldPos(I, I3) <> 1 And II = 0 And I4 = -1 Then
                TY% = 2
            End If

            If TY% = 0 Or TY% = 1 Then
                PY% = 0
            End If
            If TY% = 2 Or TY% = 3 Then
                PY% = 1
            End If
            If TY% = 0 Or TY% = 2 Then
                BY% = 0
            End If
            If TY% = 1 Or TY% = 3 Then
                BY% = 1
            End If

            If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_BAvg) <> 999 Then
                game_batRating(I, GM_2B, I3) = (130 * (i9! * 1000)) / teamRat_GAME(I1, 1)
                game_batRating(I, GM_3B, I3) = ( 19 * (i9! * 1000)) / teamRat_GAME(I1, 1)
                game_batRating(I, GM_HR, I3) = 37
            Else

                E2! = (i9! * 1000) / teamRat_GAME(I1, 1)

                If teamSplit(I1, 0) = 999 Then
                    E2! = (i9! * 1000) / gameSA(I1, TY% * 6 + 1)
                End If

                If fldPos(I, I3) =  1 And pitchRating(I, currPitcher(I), SS_P_H_H) = 0  Then game_batRating(I, GM_2B, I3) = 0: game_batRating(I, GM_3B, I3) = 0: game_batRating(I, GM_HR, I3) = 0: GoTo 235
                If fldPos(I, I3) <> 1 And batRating(I, lineupPlayer(I, I3), SS_B_H) = 0 Then game_batRating(I, GM_2B, I3) = 0: game_batRating(I, GM_3B, I3) = 0: game_batRating(I, GM_HR, I3) = 0: GoTo 235
                If fldPos(I, I3) = 1 And teamSplit(I, 0) = 999 And pitchRating(I, currPitcher(I), 38 + BY% * 7) = 0 Then game_batRating(I, GM_2B, I3) = 0: game_batRating(I, GM_3B, I3) = 0: game_batRating(I, GM_HR, I3) = 0: GoTo 235
                If fldPos(I, I3) = 1 And teamSplit(I, 0) = 999 Then game_batRating(I, GM_2B, I3) = Int((pitchRating(I, currPitcher(I), 39 + BY% * 7) / pitchRating(I, currPitcher(I), 38 + BY% * 7)) * 1000) * E2!  ' doubles times  * .95
                If fldPos(I, I3) = 1 And teamSplit(I, 0) = 999 Then game_batRating(I, GM_3B, I3) = Int((pitchRating(I, currPitcher(I), 40 + BY% * 7) / pitchRating(I, currPitcher(I), 38 + BY% * 7)) * 1000) * E2!
                If fldPos(I, I3) = 1 And teamSplit(I, 0) = 999 Then game_batRating(I, GM_HR, I3) = Int((pitchRating(I, currPitcher(I), 41 + BY% * 7) / pitchRating(I, currPitcher(I), 38 + BY% * 7)) * 1000): GoTo 235
                If fldPos(I, I3) = 1 Then game_batRating(I, GM_2B, I3) = Int((pitchRating(I, currPitcher(I), SS_P_H_2B) / pitchRating(I, currPitcher(I), SS_P_H_H)) * 1000) * E2!  ' doubles times  * .95
                If fldPos(I, I3) = 1 Then game_batRating(I, GM_3B, I3) = Int((pitchRating(I, currPitcher(I), SS_P_H_3B) / pitchRating(I, currPitcher(I), SS_P_H_H)) * 1000) * E2!
                If fldPos(I, I3) = 1 Then game_batRating(I, GM_HR, I3) = Int((pitchRating(I, currPitcher(I), SS_P_H_HR) / pitchRating(I, currPitcher(I), SS_P_H_H)) * 1000): GoTo 235

                If batRating(I, lineupPlayer(I, I3), SS_B_H) = 0 Then game_batRating(I, GM_2B, I3) = 0: game_batRating(I, GM_3B, I3) = 0: game_batRating(I, GM_HR, I3) = 0: GoTo 235

                If teamSplit(I, 0) = 999 And batRating(I, lineupPlayer(I, I3), 80) <> 0 Then

                    If batRating(I, lineupPlayer(I, I3), 43 + BY% * 7) = 0 Then
                        game_batRating(I, GM_2B, I3) = 0
                        game_batRating(I, GM_3B, I3) = 0
                        game_batRating(I, GM_HR, I3) = 0
                    Else
                        game_batRating(I, GM_2B, I3) = Int((batRating(I, lineupPlayer(I, I3), 44 + BY% * 7) / batRating(I, lineupPlayer(I, I3), 43 + BY% * 7)) * 1000) * E2!  'see note on pitchers * .95
                        game_batRating(I, GM_3B, I3) = Int((batRating(I, lineupPlayer(I, I3), 45 + BY% * 7) / batRating(I, lineupPlayer(I, I3), 43 + BY% * 7)) * 1000) * E2!
                        game_batRating(I, GM_HR, I3) = Int((batRating(I, lineupPlayer(I, I3), 46 + BY% * 7) / batRating(I, lineupPlayer(I, I3), 43 + BY% * 7)) * 1000)
                    End If
                    GoTo 235
                End If

                game_batRating(I, GM_2B, I3) = Int((batRating(I, lineupPlayer(I, I3), SS_B_2B) / batRating(I, lineupPlayer(I, I3), SS_B_H)) * 1000) * E2!  'see note on pitchers * .95
                game_batRating(I, GM_3B, I3) = Int((batRating(I, lineupPlayer(I, I3), SS_B_3B) / batRating(I, lineupPlayer(I, I3), SS_B_H)) * 1000) * E2!
                game_batRating(I, GM_HR, I3) = Int((batRating(I, lineupPlayer(I, I3), SS_B_HR) / batRating(I, lineupPlayer(I, I3), SS_B_H)) * 1000)

            End If

            235 '
            I6! = teamRat_GAME(I1, 4) / 1000
            I7! = game_batRating(I, GM_HR, I3) / 1000
            I2! = 0

            If pitchRating(I1, currPitcher(I1), 7) <> 0 Then I2! = pitchRating(I1, currPitcher(I1), 13) / pitchRating(I1, currPitcher(I1), 7)

            If teamSplit(I1, 0) = 999 And pitchRating(I1, currPitcher(I1), 89) <> 0 Then
                I6! = gameSA(I1, TY% * 6 + 4) / 1000

                If pitchRating(I1, currPitcher(I1), 52 + PY% * 7) <> 0 Then
                    I2! = pitchRating(I1, currPitcher(I1), 55 + PY% * 7) / pitchRating(I1, currPitcher(I1), 52 + PY% * 7)
                End If

            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)

            game_batRating(I, GM_HR, I3) = K8!

        Next I3

        ' ADJUST LEFTY/RIGHTY THE OLD WAY
        I4 = pitchRating(I1, currPitcher(I1), SS_P_Throws)

        For I3 = VV(I) To VV1(I)

            If teamSplit(I, 0) = 999 And batRating(I, lineupPlayer(I, I3), 80) = 0 Then

                I2 = batRating(I, lineupPlayer(I, I3), SS_B_Bats)

                If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And pitchRating(I1, currPitcher(I1), SS_P_Throws) = 1 Then
                    A = 1.04
                    A1 = .88
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = -1 And pitchRating(I1, currPitcher(I1), SS_P_Throws) = -1 Then
                    A = .93
                    A1 = 1.16
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And pitchRating(I1, currPitcher(I1), SS_P_Throws) = -1 Then
                    A = 1.02
                    A1 = .98
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) = 1 And pitchRating(I, currPitcher(I), SS_P_Throws) = 1 And pitchRating(I1, currPitcher(I1), SS_P_Throws) = 1 Then
                    A = .97
                    A1 = 1.1
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = -2 And I4 = 1 Then
                    A = 1.04
                    A1 = .96
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = -2 And I4 = -1 Then
                    A = .78
                    A1 = 1.22
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = 2 And I4 = 1 Then
                    A = .78
                    A1 = 1.22
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = 2 And I4 = -1 Then
                    A = 1.04
                    A1 = .96
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = -1 And I4 = -1 Then
                    A = .93
                    A1 = 1.16
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = -1 And I4 = 1 Then
                    A = 1.04
                    A1 = .88
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = 1 And I4 = 1 Then
                    A = .97
                    A1 = 1.1
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = 1 And I4 = -1 Then
                    A = 1.02
                    A1 = .98
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = 0 And I4 = -1 Then
                    A = 1.01
                    A1 = .86
                    Call LRADJ(A!, A1!)
                End If
                If fldPos(I, I3) <> 1 And I2 = 0 And I4 = 1 Then
                    A = .99
                    A1 = .93
                    Call LRADJ(A!, A1!)
                End If

            End If

            game_batRating(I, GM_L_HR, I3) = game_batRating(I, GM_HR, I3)
            game_batRating(I, GM_R_HR, I3) = game_batRating(I, GM_HR, I3)

        Next I3

        ' ADJUST AVERAGES FOR THE HOME TEAM
        For I3 = VV(I) To VV1(I)
            If I = 1 Then
                game_batRating(I, GM_BAvg, I3) = game_batRating(I, GM_BAvg, I3) + 5
            Else
                game_batRating(I, GM_BAvg, I3) = game_batRating(I, GM_BAvg, I3) - 5
            End If
        Next I3

        ' CALCULATE THE HOME PARK FACTORS
        If gameLoc <> 2 Then

            For I2 = VV(I) To VV1(I)
                If I = 1 Then
                    game_batRating(I, GM_2B, I2) = CInt(game_batRating(I, GM_2B, I2) * (teamRat_GAME(I, 6) / 100))
                    game_batRating(I, GM_3B, I2) = CInt(game_batRating(I, GM_3B, I2) * (teamRat_GAME(I, 7) / 100))
                Else
                    'I <> 1
                    game_batRating(I, GM_2B, I2) = CInt(game_batRating(I, GM_2B, I2) * (teamRat_GAME(I1, 6) / 100) * ((200 - teamRat_GAME(I, 6)) / 100))
                    game_batRating(I, GM_3B, I2) = CInt(game_batRating(I, GM_3B, I2) * (teamRat_GAME(I1, 7) / 100) * ((200 - teamRat_GAME(I, 7)) / 100))
                End If

                If parkHRVals!(I) > 1000 Then
                    I3! = 100
                    I6! = Int(parkHRVals!(I) / 1000) * WF!
                    I7! = (parkHRVals!(I) - Int((parkHRVals!(I) / 1000)) * 1000) * WF!
                End If
                If parkHRVals!(I1) > 1000 Then
                    I4! = 100
                    K8! = Int(parkHRVals!(I1) / 1000) * WF!
                    i9! = (parkHRVals!(I1) - Int((parkHRVals!(I1) / 1000)) * 1000) * WF!
                End If

                If parkHRVals!(I) < 1000 Then
                    I3! = parkHRVals!(I) * WF!
                    I6! = I3!
                    I7! = I3!
                End If
                If parkHRVals!(I1) < 1000 Then
                    I4! = parkHRVals!(I1) * WF!
                    K8! = I4!
                    i9! = I4!
                End If

                If I = 1 Then
                    game_batRating(I, GM_HR,   I2) = CInt((game_batRating(I, GM_HR, I2)   * (I3! / 100)))
                    game_batRating(I, GM_L_HR, I2) = CInt((game_batRating(I, GM_L_HR, I2) * (I6! / 100)))
                    game_batRating(I, GM_R_HR, I2) = CInt((game_batRating(I, GM_R_HR, I2) * (I7! / 100)))
                Else
                    'I <> 1
                    game_batRating(I, GM_HR,   I2) = CInt((game_batRating(I, GM_HR, I2)   * (I4! / 100) * ((200 - I3!) / 100)))
                    game_batRating(I, GM_L_HR, I2) = CInt((game_batRating(I, GM_L_HR, I2) * (K8! / 100) * ((200 - I6!) / 100)))
                    game_batRating(I, GM_R_HR, I2) = CInt((game_batRating(I, GM_R_HR, I2) * (i9! / 100) * ((200 - I7!) / 100)))
                End If

            Next I2

        Else
            Exit For
        End If

    Next I

End Sub


'----------------------------------------
'            LRADJ Subroutine
'----------------------------------------
' ...explanation...
Sub LRADJ (A!, A1!)

    For I6 = 0 To 3
        game_batRating(I, I6, I3) = game_batRating(I, I6, I3) * A!
    Next I6

    game_batRating(I, GM_BB, I3) = game_batRating(I, GM_BB, I3) * A!
    game_batRating(I, GM_K,  I3) = game_batRating(I, GM_K,  I3) * A1!

End Sub


'----------------------------------------
'          NORMALIZE Subroutine
'----------------------------------------
' ...explanation...
Sub NORMALIZE (I7!, I2!, I6!, K8!)

    I0! = (I7! * I2! * (1 - I6!))

    If (I0! + ((1 - I7!) * (1 - I2!) * I6!)) = 0 Then
        K8! = 0
    Else
        K8! = I0! / (I0! + ((1 - I7!) * (1 - I2!) * I6!))
    End If

    K8! = CInt(K8! * 1000)

End Sub

'----------------------------------------
'         BASEONBALLS Subroutine
'----------------------------------------
'This subroutine manages the stats and the
'Play-by-Play for a Base-on-Balls
Sub BASEONBALLS (S2%, D2, D, P)

    S2% = EVENT_INTERFERENCE

    Call WINDEX

    Call Rolld100(BV)

    If D2 = DEF_INTNTL_BB Then

        If BV <= 50 Then
            Call PBP(player$(0) + " is walked intentionally...")
        Else
            Call PBP(player$(0) + " is given a purpose pass...")
        End If

        S8%(D, 0) = S8%(D, 0) + 1

    Else

        If d100% <= 30 Then Call PBP("Full count...")

        Call DELIVERY(D)

        Call GetPBPString(80, 780, fileString$, D)
        Call PBP(fileString$)

        Call GetPBPString(30, 860, fileString$, D)
        Call PBP(fileString$)

    End If

    '-- Where does s4 get a value???  [bm] I think this should be baseRunners
    If baseRunners = 7 And d100% <= 30 Then Call PBP("that forces a run in")

    RN = Int(Rnd(1) * 4) + 1

    '-- here, if s4 is between 4 and 6, bases are loaded
    '-- in context, this is obviously AFTER the walk
    If baseRunners > 3 And baseRunners < 7 Then

        Select Case RN
            Case 1: Call PBP("and the bases are loaded!")
            Case 2: Call PBP("and the sacks are jammed!")
            Case 3: Call PBP("and the bases are full!")
            Case 4: Call PBP("and that loads them up!")
        End Select

    End If

    Call ADDPIT(D, currPitcher(D), LG_P_BB)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_W)
    Else
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_W)
    End If

End Sub


'----------------------------------------
'         HITBYPITCH Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'text for a player hit by a pitch.
Sub HITBYPITCH (S2%, D)

    S2% = EVENT_HBP

    Call WINDEX
    Call DELIVERY(D)

    Call GetPBPString(20, 720, fileString$, D)
    Call PBP(fileString$)

    Call GetPBPString(11, 740, fileString$, D)
    Call PBP(fileString$)

    Call GetPBPString(20, 760, fileString$, D)
    Call PBP(fileString$)

    Call Rolld100(RN)

    If RN <= 50 And baseRunners = 7 Then Call PBP("tough way to get an RBI")

End Sub

'----------------------------------------
'          WILDPITCH Subroutine
'----------------------------------------
' ...explanation...
Sub WILDPITCH (S2%, D)

    S2% = 6
    ruleAsWild = 0

    Call Rolld100(RN)

    Select Case RN

        Case 1 To 16:
            Call PBP("and it gets away from " + batters$(D, fielder(D, 2)))
            ruleAsWild = 1

        Case 17 To 32:
            Call PBP("pitch is WAY high, and " + batters$(D, fielder(D, 2)) + " can't get it")
            ruleAsWild = 1

        Case 33 To 48:
            Call PBP("It's in the dirt")
            Call PBP("The runners move up 90 ft.")
            ruleAsWild = 1

        Case 49 To 64:
            Call PBP("It bounces in front of the plate")
            Call PBP("it's past the catcher")
            Call PBP("the runners advance")
            ruleAsWild = 1

        Case 65 To 80:
            Call PBP("it's over " + batters$(D, fielder(D, 2)) + "'s head")

            '-- This was originally written as:
            '        If RN > 67 Then
            '-- This would clearly work by adding
            '--     the "wild pitch" text to the above.
            '-- But then it's clear that some numbers
            '--     were meant to not append that text.
            '-- For now,I've chosen to remove the overlap
            '-- My working theory is the Case 67+ below
            '--     for crediting the pitch may have been
            '--     duplicated but not fixed

        Case 81 To 100:
            Call PBP("wild pitch by " + pitchers$(D, currPitcher(D)))

    End Select

    If ruleAsWild = 1 Then

        Call Rolld100(RN)

        Select Case RN
            Case 1 To 33: pbpString$ = "It's ruled a wild pitch"
            Case 34 To 66: pbpString$ = "score it a wild pitch"
            Case 67 To 100: pbpString$ = "credit " + pitchers$(D, currPitcher(D)) + " with a wild pitch"
        End Select

        Call PBP(pbpString$)

    End If

End Sub


'----------------------------------------
'          RELIEVERS Subroutine
'----------------------------------------
' ...explanation...
Sub RELIEVERS (PPH, pz)

    Call PITSTRAT(PPH, pz)

    If pz = 1 Then
        pz = 0
    Else
        I2 = 0
        A1 = gameScore(D, 0) - gameScore(P, 0)

        For rIdx = 0 To 21

            A2 = pitchRating(D, rIdx, 4) - pitchRating(D, rIdx, 5) - pitchRating(D, rIdx, 3) * 1.3

            If Not (pitchRating(D, rIdx, SS_P_Avail) > 0 Or pitchers$(D, rIdx) = "XXX" Or A2 = 0) Then
                If INNING% <= 7 And A2 > 0 Then
                    I2 = I2 + A2
                End If
                If INNING% > 7 And A1 < 0 And A2 > 0 Then
                    I2 = I2 + A2
                End If
                If INNING% > 7 And A1 >= 0 Then
                    I2 = I2 + pitchRating(D, rIdx, 3)
                End If
            End If

        Next rIdx

        I3 = Int(Rnd(1) * I2) + 1
        I2 = 0

        For rIdx = 0 To 21

            A2 = pitchRating(D, rIdx, 4) - pitchRating(D, rIdx, 5) - pitchRating(D, rIdx, 3) * 1.3

            If Not (pitchRating(D, rIdx, SS_P_Avail) > 0 Or pitchers$(D, rIdx) = "XXX" Or A2 = 0) Then

                If INNING% <= 7 Then

                    If A2 > 0 Then
                        If I3 <= I2 + A2 Then
                            Call NEWPITCHER(rIdx, PPH, pz)      ' middle relief
                            Exit Sub
                        Else
                            I2 = I2 + A2
                        End If
                    End If

                Else

                    'INNING% > 7
                    If A1 < 0 And A2 > 0 Then
                        If I3 <= I2 + A2 Then
                            Call NEWPITCHER(rIdx, PPH, pz)      ' setup
                            Exit Sub
                        Else
                            I2 = I2 + A2
                        End If
                    Else
                        If A1 >= 0 And I3 > I2 Then
                            If I3 <= I2 + pitchRating(D, rIdx, 3) Then
                                Call NEWPITCHER(rIdx, PPH, pz)  ' closer
                                Exit Sub
                            Else
                                I2 = I2 + pitchRating(D, rIdx, 3)
                            End If
                        End If
                    End If

                End If

            End If

        Next rIdx

        For rIdx = 0 To 21
            If pitchRating(D, rIdx, SS_P_Avail) = 0 And pitchers$(D, rIdx) <> "XXX" Then
                Call NEWPITCHER(rIdx, PPH, pz)                  ' anyone with a healthy arm
                Exit For
            End If
        Next rIdx

    End If

End Sub


'----------------------------------------
'        REPLACEMENTS Subroutine
'----------------------------------------
' ...explanation...
Sub REPLACEMENTS (p_D, p_origPlayer, p_newPlayer)

    For xx = 22 To 25

        If fldPos(p_D, p_origPlayer) = batRating(p_D, p_newPlayer, xx) Then 'New player can play the position
            batRating(p_D, p_newPlayer, SS_B_Arm)    = batRating(p_D, p_newPlayer, 33 + (xx - 22) * 2)
            batRating(p_D, p_newPlayer, SS_B_Range)  = batRating(p_D, p_newPlayer, 34 + (xx - 22) * 2)
            batRating(p_D, p_newPlayer, SS_B_FldPct) = batRating(p_D, p_newPlayer, xx + 4)
            Exit Sub
        End If

    Next

    Select Case fldPos(p_D, p_origPlayer)   'New player can't play the position -- assign default (poor) values

        Case 2
            jArm = 1
            jRange = 0
            jFldPct = 910 + Int(Rnd(1) * 10) + 1

        Case 3
            jArm = 0
            jRange = 3
            jFldPct = 960 + Int(Rnd(1) * 10) + 1

        Case 4
            jArm = 0
            jRange = 3
            jFldPct = 912 + Int(Rnd(1) * 10) + 1

        Case 5 To 6
            jArm = 0
            jRange = 3
            jFldPct = 902 + Int(Rnd(1) * 10) + 1

        Case Is >= 7:
            jArm = 2
            jRange = 2
            jFldPct = 972 + Int(Rnd(1) * 10) + 1

    End Select

    batRating(p_D, p_newPlayer, SS_B_Arm)    = jArm
    batRating(p_D, p_newPlayer, SS_B_Range)  = jRange
    batRating(p_D, p_newPlayer, SS_B_FldPct) = jFldPct

End Sub


'----------------------------------------
'         SCOREBOARD Subroutine
'----------------------------------------
'This subroutine essentially updates the
'hit counter on the scoreboard only.
Sub SCOREBOARD ()

    gameScore(P, 1) = gameScore(P, 1) + 1

    If useVGA = 0 Then
        Color 14, 0
        Locate P + 2, 43
        Print Using "##"; gameScore(P, 1);
        Color 15, 0
    Else
        FORMATTED$ = FUsing$(Str$(gameScore(P, 1)), "##", 1)
        PrintOnGfx 56, P + 28, FORMATTED$, imageScreen&
    End If

End Sub


'----------------------------------------
'         ColorChange Subroutine
'----------------------------------------
'This routine checks to see if both teams
'have conflicting primary colors.
'If so, it will substitute the colors
'for the home team by either reversing
'them (with a slight color change) or
'otherwise using White/Grey/Black as
'an alternate color.
'In essence, it's the digital equivalent
'of assigning an alternate jersey color.
Sub ColorChange ()

    ' ** Home Team Colors **
    fg_color = teamRat_GAME(1, 11)
    bg_color = teamRat_GAME(1, 12)

    '-- Black Foreground --
    '-- Blue / Green / Cyan / Red / Magenta Background --
    If fg_color = 0 And ((bg_color >= 1 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to alternate version of background color
        'Change background to Black
        If bg_color < 9 Then
            teamRat_GAME(1, 11) = bg_color + 8
        Else
            teamRat_GAME(1, 11) = bg_color - 8
        End If
        teamRat_GAME(1, 12) = 0
    End If


    '-- Black Foreground --
    '-- Orange / Yellow Background --
    If (fg_color = 0) And (bg_color = 14 Or bg_color = 6) Then
        'Change foreground to background color
        'Change background to foreground color
        teamRat_GAME(1, 0) = bg_color
        teamRat_GAME(1, 1) = fg_color
    End If


    '-- Grey / White Foreground --
    '-- Blue / Green / Cyan / Red / Magenta Background --
    If (fg_color = 7 Or fg_color = 15) And ((bg_color >= 1 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to alternate version of background color
        'Change background to Grey | White
        If bg_color < 9 Then
            teamRat_GAME(1, 11) = bg_color + 8
        Else
            teamRat_GAME(1, 11) = bg_color - 8
        End If
        If fg_color = 7 Then
            teamRat_GAME(1, 12) = 15
        Else
            teamRat_GAME(1, 12) = 7
        End If
    End If


    '-- Grey / White Foreground --
    '-- Black Background --
    If (fg_color = 7 Or fg_color = 15) And (bg_color = 0) Then
        'Change foreground to Black
        'Change background to Grey | White
        teamRat_GAME(1, 11) = 0
        If fg_color = 7 Then
            teamRat_GAME(1, 12) = 15
        Else
            teamRat_GAME(1, 12) = 7
        End If
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- Black Background --
    If ((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And bg_color = 0 Then
        'Change foreground to Black
        'Change background to alternate version of foreground color
        teamRat_GAME(1, 11) = 0
        If fg_color < 9 Then
            teamRat_GAME(1, 12) = fg_color + 8
        Else
            teamRat_GAME(1, 12) = fg_color - 8
        End If
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- White | Grey Background --
    If ((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And (bg_color = 7 Or bg_color = 15) Then
        'Change foreground to Grey | White
        'Change background to foreground color
        If bg_color = 7 Then
            teamRat_GAME(1, 11) = 15
        Else
            teamRat_GAME(1, 11) = 7
        End If

        teamRat_GAME(1, 12) = fg_color
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- Blue / Green / Cyan / Red / Magenta Background --
    If ((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And ((bg_color >= 1 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to alternate version of background color
        'Change background to alternate version of foreground color
        If bg_color < 9 Then
            teamRat_GAME(1, 11) = bg_color + 8
        Else
            teamRat_GAME(1, 11) = bg_color - 8
        End If
        If fg_color < 9 Then
            teamRat_GAME(1, 12) = fg_color + 8
        Else
            teamRat_GAME(1, 12) = fg_color - 8
        End If

    End If


    '-- Orange / Yellow Foreground
    '-- Black / Blue / Green / Cyan / Red / Magenta Background --
    If (fg_color = 14 Or fg_color = 6) And ((bg_color >= 0 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to background color
        'Change background to foreground color
        teamRat_GAME(1, 11) = bg_color
        teamRat_GAME(1, 12) = fg_color
    End If


    '-- Orange / Yellow Foreground
    '-- Grey / White Background --
    If (fg_color = 14 Or fg_color = 6) And (bg_color = 7 Or bg_color = 15) Then
        'Change foregroundto background color
        'Change background to Grey | White
        teamRat_GAME(1, 11) = bg_color
        If fg_color = 7 Then
            teamRat_GAME(1, 12) = 15
        Else
            teamRat_GAME(1, 12) = 7
        End If
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- Orange / Yellow Background
    If (((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And (bg_color = 14 Or bg_color = 6)) Then
        'Change foregroundto background color
        'Change background to foreground color
        teamRat_GAME(1, 11) = bg_color
        teamRat_GAME(1, 12) = fg_color
    End If


    '-- Grey / White Foreground --
    '-- Orange / Yellow Background
    If (fg_color = 7 Or fg_color = 15) And (bg_color = 14 Or bg_color = 6) Then
        'Change foregroundto background color
        'Change background to Grey | White
        teamRat_GAME(1, 11) = bg_color

        'teamRat_GAME(1, 12) = 0
        If fg_color = 7 Then
            teamRat_GAME(1, 12) = 15
        Else
            teamRat_GAME(1, 12) = 7
        End If
    End If


End Sub


'----------------------------------------
'        SECOND2FIRST Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play FOR
'a throw from 1st to 2nd base
Sub SECOND2FIRST (D)

    Call GetPBPString(30, 1540, fileString$, D)
    Call PBP(fileString$ + " to first")

End Sub


'----------------------------------------
'          SINGLEPBP Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for a single hit.
Sub SINGLEPBP (S2%, D, currFielder)

    If S2% = -EVENT_INF_SINGLE Then

        S2% = EVENT_INF_SINGLE

    Else
        Call DELIVERY(D)

        If S2% = EVENT_INF_SINGLE Then
            Call PBP(player$(0) + " beats out an infield single")
        Else

            If d100% <= 50 Then
                Call GetPBPString(50, 1610, fileString$, D)
                Call PBP(fileString$ + " " + fieldPos$(currFielder))
            Else
                If currFielder = 7 Then
                    range = 70: min = 1660
                End If
                If currFielder = 9 Then
                    range = 70: min = 1730
                End If
                If currFielder = 8 Then
                    range = 90: min = 1800
                End If
                Call GetPBPString(range, min, fileString$, D)
                Call PBP(fileString$)
            End If

        End If 'Done checking for S2%

        Call DELAY

    End If 'Done checking for S2%

End Sub


'----------------------------------------
'        SINGLEROUTINE Subroutine
'----------------------------------------
'This subroutine handles the stats, adv-
'ancements and calculation for a single.
Sub SINGLEROUTINE (currFielder, P, I1, D, S2%, P2)

    Call GETOUTFIELDER(currFielder, P, I1, D)
    Call WINDEX

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
    End If

    Call ADDPIT(D, currPitcher(D), LG_P_H)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
    End If

    '-- Original code is as follows:
    'If H7% > sn Then Call SINGLEPBP(S2%, D, currFielder)
    '-- But there is no clue what sn is supposed to be
    Call SINGLEPBP(S2%, D, currFielder)

    For I = 0 To 3
        A5%(I) = 1
    Next I

    If S2% <> EVENT_INF_SINGLE And b2Base(2) <> 0 Then

        Call Rolld100(H6%)

        If fldPos(P, b2Base(2)) <> 1 Then
            BR% = batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run)
        Else
            BR% = pitchRating(P, currPitcher(P), SS_P_Run)
        End If

        If nbrOuts(0) = 0 Then
            If currFielder = 7 Then
                I3 = 16 + BR% * 3
            End If
            If currFielder = 8 Then
                I3 = 49 + BR% * 3
            End If
            If currFielder = 9 Then
                I3 = 27 + BR% * 3
            End If
        End If

        If nbrOuts(0) = 1 Then
            If currFielder = 7 Then
                I3 = 36 + BR% * 3
            End If
            If currFielder = 8 Then
                I3 = 61 + BR% * 3
            End If
            If currFielder = 9 Then
                I3 = 44 + BR% * 3
            End If
        End If

        If nbrOuts(0) = 2 Then
            If currFielder = 7 Then
                I3 = 79 + BR% * 3
            End If
            If currFielder = 8 Then
                I3 = 82 + BR% * 3
            End If
            If currFielder = 9 Then
                I3 = 79 + BR% * 3
            End If
        End If

        If H6% <= I3 Or P2 = OFF_HIT_N_RUN And S2% <> EVENT_INF_SINGLE Then
            A5%(2) = 2
        End If

    End If

    If Not (b2Base(1) = 0 Or S2% = EVENT_INF_SINGLE Or b2Base(2) > 0 And A5%(2) = 1) Then

        Call Rolld100(H6%)

        BR% = batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run)

        If fldPos(P, b2Base(1)) = 1 Then
            BR% = pitchRating(P, currPitcher(P), SS_P_Run)
        End If

        I3 = 0

        If nbrOuts(0) = 0 Then
            If currFielder = 7 Then
                I3 = 6 + BR% * 3
            End If
            If currFielder = 8 Then
                I3 = 25 + BR% * 3
            End If
            If currFielder = 9 Then
                I3 = 38 + BR% * 3
            End If
        End If

        If nbrOuts(0) = 1 Then
            If currFielder = 7 Then
                I3 = 10 + BR% * 3
            End If
            If currFielder = 8 Then
                I3 = 26 + BR% * 3
            End If
            If currFielder = 9 Then
                I3 = 40 + BR% * 3
            End If
        End If

        If nbrOuts(0) = 2 Then
            If currFielder = 7 Then
                I3 = 11 + BR% * 3
            End If
            If currFielder = 8 Then
                I3 = 45 + BR% * 3
            End If
            If currFielder = 9 Then
                I3 = 62 + BR% * 3
            End If
        End If

        If H6% <= I3 Or P2 = OFF_HIT_N_RUN And S2% <> EVENT_INF_SINGLE Then
            A5%(1) = 2
        End If

    End If

    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)

End Sub

'----------------------------------------
'          SOMESTATS Subroutine
'----------------------------------------
' ...explanation...
Sub SOMESTATS (V, P, errorOnPlay%)

    If b2Base(V) > 0 And errorOnPlay% = 0 Then

        If fldPos(P, b2Base(V)) = 1 Then
            Call INCPITOFF(P, currPitcher(P), LG_PB_SB)
        Else
            Call INCBATOFF(P, lineupPlayer(P, b2Base(V)), LG_B_SB)
        End If

    End If

End Sub


'----------------------------------------
'        SQUEEZECHOICE Subroutine
'----------------------------------------
'This subroutine gives the user a choice
'for a squeeze / bunt. The result of the
'decision is assigned to SH.

'-- Autoplay appears to always DO a bunt

'-- Consider updating this to run the
'   squeeze play a small % of the time,
'   and only in the most common conditions:
'       Team has < 2 outs
'       Runner is on 3rd

'-- This could be done even more often for
'   teams before the 1970's DH rule, since
'   bunting is not as frequent now

Sub SQUEEZECHOICE (SH)

    If playerMode = 2 Or playerMode = 1 And P = compTeam Then
        SH = 2
    Else
        Locate 15, 1
        Print "1 - SQUEEZE PLAY"
        Print "2 - BUNT FOR SACRIFICE/HIT"
        Do
            I$ = GetKeyPress$
            SH = Val(I$)
        Loop Until SH >= 1 And SH <= 2
    End If

End Sub

'----------------------------------------
'          STEAL3RD Subroutine
'----------------------------------------
'This subroutine handles the attempt of
'a runner to steal 3rd base.
Sub STEAL3RD (TGTBASE%, AA, stlChance, runnerHeld, D)

    If TGTBASE% = 3 Then
        ' *** SUCCESS OF 3B STEAL ***
        If AA >= 24 Then
            stlChance = 830
        Else
            stlChance = 700
        End If
    End If

    If TGTBASE% = 4 Then stlChance = stlChance * .5

    If runnerHeld = 0 Then
        'Runner not held
        stlChance = stlChance + 50
    Else
        'Runner is held
        stlChance = stlChance - ((pitchRating(D, currPitcher(D), SS_P_HoldRunner) - 5) * 25)  ' - 50
    End If

End Sub

'----------------------------------------
'         STEALCHANCE Subroutine
'----------------------------------------
' ...explanation...
Sub STEALCHANCE (AA, I2, TGTBASE%, CHANCE%)

    Dim DD As Single
    Dim TT As Single
    Dim RR As Single
    Dim SS As Single
    Dim GG As Single
    Dim I1 As Single

    AA = batRating(P, I2, SS_B_SB)
    BB = batRating(P, I2, SS_B_SB) + batRating(P, I2, SS_B_CS)
    CC = batRating(P, I2, SS_B_2B)  ' *** CHANCE TO STEAL 2B & 3B ***

    Select Case AA
        Case Is <= 43: DD = .16
        Case 44 To 60: DD = .21
        Case Else: DD = .3
    End Select

    TT = CC + BB - (BB * DD) ' *** TOTAL CHANCES TO STEAL 3RD
    RR = (TT * .19) * 1.4 ' *** "      "      "   "     "  1 OUT
    SS = (TT * .56) * 1.4 ' ***                            0 OUT
    GG = RR + SS ' ***                           (0,1 OUT)

    If TGTBASE% <> 3 Then

        FF = batRating(P, I2, SS_B_H) + batRating(P, I2, SS_B_W) - batRating(P, I2, SS_B_HR)

        If FF = 0 Then
            I1 = 0
        Else
            If AA >= 12 Then
                I1 = Int(((BB - BB * DD) / FF) * 1500)
            Else
                I1 = Int((BB / FF) * 1500)
            End If

            If I1 < 0 Then I1 = 0
        End If

    Else
        If BB < 10 And nbrOuts(0) = 0 Then
            I1 = 0
        End If
        If BB < 10 And nbrOuts(0) = 1 Then
            I1 = 10
        End If
        If nbrOuts(0) = 2 Or TT = 0 Then
            I1 = 0
        End If
        If BB >= 10 And nbrOuts(0) = 1 Then
            I1 = BB * DD / TT * 1000
        End If
        If BB >= 10 And nbrOuts(0) = 0 Then
            I1 = BB * DD / TT * 1000
        End If

        If I1 < 0 Then I1 = 0
    End If

    CHANCE% = I1

End Sub


'----------------------------------------
'          STRIKEOUT Subroutine
'----------------------------------------
' ...explanation...
Sub STRIKEOUT (nbrStrikes, D)

    ' *** strikeout pbp ***
    If nbrStrikes = 3 Then

        BV = 20

    Else

        BV = Int(Rnd(1) * 100)

        If d100% <= 20 Then Call PBP("FULL COUNT...")

        Call DELIVERY(D)

        If BV = 1 Then
            Call PBP("  Three!!")
            Call PBP(player$(0) + " turns to " + gameUmpire$(1))
            Call PBP("he says he tees them up higher than that")
            Call PBP("at the country club!!")
        End If

        If BV = 2 Then
            Call PBP("Strike three...")
            Call PBP(player$(0) + " stands there like a house")
            Call PBP("by the side of the road")
            Call DELAY
        End If

        If BV > 2 And BV <= 5 Then
            Call PBP(player$(0) + " looks at a called strike three")
            Call PBP("and turns to glare at " + gameUmpire$(1))
            Call PBP(gameManagers$(P) + " is barking from the dugout!!")
        End If

        If d100% <= 25 Then
            range = 40
            min = 890
        Else
            range = 80
            min = 930
        End If

        Call GetPBPString(range, min, fileString$, D)
        Call PBP(fileString$)

    End If

    Call GetPBPString(40, 1010, fileString$, D)
    Call PBP(fileString$)

    Call Rolld100(RN)

    Select Case RN

        Case 1 To 20:
            pbpString$ = "Number" + Str$(gamePitStats(D, currPitcher(D), 5) + 1) + " for " + pitchers$(D, currPitcher(D))

        Case 21 To 40:
            pbpString$ = "That's" + Str$(gamePitStats(D, currPitcher(D), 5) + 1) + " for " + pitchers$(D, currPitcher(D))

        Case 41:
            pbpString$ = player$(0) + " is not a happy man"

        Case 42 To 100:
            pbpString$ = player$(0) + " has had better AB's"

    End Select

    Call PBP(pbpString$)

End Sub


'----------------------------------------
'         STRIKEOUTCS Subroutine
'----------------------------------------
' ...explanation...
Sub STRIKEOUTCS (SO)

    If SO = 7 Then currLineupSlot(P) = currLineupSlot(P) + 1

End Sub


'----------------------------------------
'         TAGADVANCE Subroutine
'----------------------------------------
' ...explanation...
Sub TAGADVANCE (S2%, baseNbr!, D)

    If S2% = EVENT_1B And baseNbr! = 2 Then

        If fldPos(P, currLineupSlot(P)) = 1 Then
            I5 = currPitcher(P)
            Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
        Else
            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
        End If

    End If

    If S2% = EVENT_2B And baseNbr! = 3 Then

        If fldPos(P, currLineupSlot(P)) = 1 Then
            Call INCPITOFF(P, currPitcher(P), LG_PB_Trp)

            leagPitStats(P, currPitcher(P), LG_PB_Dbl) = leagPitStats(P, currPitcher(P), LG_PB_Dbl) - 1
            gamePitStats(P, currPitcher(P), LG_PB_Dbl) = gamePitStats(P, currPitcher(P), LG_PB_Dbl) - 1
            Exit Sub
        Else
            I3 = 4
            I4 = 5
            I5 = lineupPlayer(P, currLineupSlot(P))
        End If

    End If

    If S2% = EVENT_3B And baseNbr! = 4 Then
        I3 = 5
        I4 = 6
        I5 = lineupPlayer(P, currLineupSlot(P))
    End If

    If S2% = EVENT_2B And baseNbr! = 3 Or S2% = EVENT_3B And baseNbr! = 4 Then

        If pitchRating(D, currPitcher(D), SS_P_Throws) <> 1 Then
            CSS(P, I5, I3 - 2) = CSS(P, I5, I3 - 2) - 1
            CSS(P, I5, I4 - 2) = CSS(P, I5, I4 - 2) + 1
        Else
            gameBatStats(P, I5, I3) = gameBatStats(P, I5, I3) - 1
            leagBatStats(P, I5, I3) = leagBatStats(P, I5, I3) - 1
            gameBatStats(P, I5, I4) = gameBatStats(P, I5, I4) + 1
            leagBatStats(P, I5, I4) = leagBatStats(P, I5, I4) + 1
        End If

    End If

End Sub


'----------------------------------------
'         THROW2FIRST Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for a throw fom 3rd/ss to 1st base
Sub THROW2FIRST (D)

    pbpIdx% = Int(Rnd(1) * 40) + 1570

    Call GetPBPString(40, 1570, fileString$, D)
    Call PBP(fileString$ + " to first")

End Sub


'----------------------------------------
'           TIRING Subroutine
'----------------------------------------
'This subroutine checks for stamina of a
'pitcher and builds a corresponding text
'for Play-by-Play
Sub TIRING (P9)

    If S8%(P9, 0) <= gamePitStats(P9, currPitcher(P9), 4) + gamePitStats(P9, currPitcher(P9), 1) And S8%(P9, 1) <= gamePitStats(P9, currPitcher(P9), 2) Then

        If S8%(P9, 1) > 0 And INNING% < 6 Or S8%(P9, 1) = 0 And gamePitStats(P9, currPitcher(P9), 4) + gamePitStats(P9, currPitcher(P9), 1) > 4 Then
            If d100% <= 50 Then
                Print pitchers$(P9, currPitcher(P9)); " just doesn't have it today";
            Else
                Print pitchers$(P9, currPitcher(P9)); " ain't got nuthin' on his fastball folks!";
            End If
        Else
            Print pitchers$(P9, currPitcher(P9)); " is tired!!";
        End If

    Else

        If S8%(P9, 1) > 0 And (S8%(P9, 0) <= gamePitStats(P9, currPitcher(P9), 4) + gamePitStats(P9, currPitcher(P9), 1) Or S8%(P9, 1) <= gamePitStats(P9, currPitcher(P9), 2)) Or (gamePitStats(P9, currPitcher(P9), 4) + gamePitStats(P9, currPitcher(P9), 1) > 2 And gamePitStats(P9, currPitcher(P9), 0) >= 5) Then
            Print pitchers$(P9, currPitcher(P9)); " is tiring!!";
        Else

            If S8%(P9, 1) > 0 Or (gamePitStats(P9, currPitcher(P9), 4) + gamePitStats(P9, currPitcher(P9), 1) < 3 And gamePitStats(P9, currPitcher(P9), 2) = 0) Then
                Print pitchers$(P9, currPitcher(P9)); " seems to be OK!!";
            End If

        End If

    End If

End Sub


'----------------------------------------
'         TRIPLEPLAY Subroutine
'----------------------------------------
' ...explanation...
Sub TRIPLEPLAY (D)

    Call WINDEX

    player$(4) = batters$(D, fielder(D, 5))

    Call PBP("grounder to " + player$(4) + " @ 3B")
    Call PBP(player$(2) + " OUT @ 3B")
    Call PBP(player$(1) + " OUT @ 2B")
    Call PBP(player$(0) + " OUT @ 1B")
    Call PBP("--- TRIPLE PLAY !!! ---")     'ToDo [bm] Note it in box score somehow
    Call DELAY

    nbrOuts(0) = 3

    For idx = 1 To 3:
        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
    Next idx

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
    Else
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
    End If

End Sub


'----------------------------------------
'       TRYFOREXTRABASE Subroutine
'----------------------------------------
'This subroutine handles all the logic and
'Play-by-Play (relying on other rouintes)
'for an attempt to steal base.
Sub TRYFOREXTRABASE (S2%, P2, runnerOn2nd, D, currFielder, sah%, stealing, baseNbr!, noThrow)

    If baseNbr! <> 0 Then
        'This happens very rarely
        If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE already have baseNbr = "; baseNbr!

    Else

        baseNbr! = 0
        Call Rolld100(H6%)

        If Environ$("PBPLOG") = "TRUE" Then 
            Print #11, "TRYFOREXTRABASE starting at baseNbr = 0, H6% = "; H6%; ", S2Event = "; S2%
            For bm1 = 1 To 3
                If b2Base(bm1) > 0 Then Print #11, "  base:"; bm1; " runnerRating:"; batRating(P, lineupPlayer(P, b2Base(bm1)), SS_B_Run);
            Next bm1
            Print #11,
        End If

        'Determine which base we're stealing
        If S2% = EVENT_2B And b2Base(2) > 0 And H6% < 6 And b2Base(3) = 0 And batRating(P, lineupPlayer(P, b2Base(2)), SS_B_3B) > 0 Then
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE baseNbr 3A"
            baseNbr! = 3
        End If
        If S2% = EVENT_3B And b2Base(3) > 0 And H6% <= 2 And batRating(P, lineupPlayer(P, b2Base(3)), SS_B_HR) > 0 Then
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE baseNbr 4A"
            baseNbr! = 4
        End If
        If S2% = EVENT_1B And b2Base(3) > 0 And P2 = OFF_HIT_N_RUN And H6% >= 6 And H6% <= batRating(P, lineupPlayer(P, b2Base(3)), SS_B_Run) Then
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE baseNbr 4B"
            baseNbr! = 4
        End If
        If S2% = EVENT_1B And b2Base(3) > 0 And runnerOn2nd = b2Base(3) And P2 <> OFF_HIT_N_RUN And H6% <= batRating(P, lineupPlayer(P, b2Base(3)), SS_B_Run) * 12 Then
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE baseNbr 4C"
            baseNbr! = 4
        End If

        runRating = 10

        If b2Base(2) > 0 Then
            runRating = runRating + batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run)
        End If
        If S2% = EVENT_1B And b2Base(2) > 0 And P2 <> OFF_HIT_N_RUN And H6% <= runRating And b2Base(3) = 0 And baseNbr! = 0 Then
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE baseNbr 3B"
            baseNbr! = 3
        End If
        If S2% = EVENT_1B And b2Base(1) > 0 And b2Base(2) = 0 And baseNbr! = 0 And H6% <= batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run) And batRating(P, lineupPlayer(P, b2Base(1)), SS_B_2B) > 0 Then
            If Environ$("PBPLOG") = "TRUE" Then Print #11, "TRYFOREXTRABASE baseNbr 2"
            baseNbr! = 2
        End If

    End If

    If baseNbr! <> 0 Then
        'We shouldn't still have a 0 value!

        If Not (playerMode = 2 Or playerMode = 1 And P = compTeam) Then
            If S2% < EVENT_HR Then
                Call PBP(player$(baseNbr! - 1) + " try for " + baseName$(baseNbr!) + "? (Y/N) ")
            End If
            If S2% > EVENT_HR Then
                Call PBP(player$(baseNbr! - 1) + " tag up? (Y/N) ")
            End If
            Call PBP("against " + player$(4))
        End If

        I$ = "N"
        A1 = gameScore(P, 0) - gameScore(D, 0)

        If (playerMode = 2 Or playerMode = 1 And P = compTeam) Then
            If A1 > -2 And A1 < 2 And S2% <> EVENT_FLY_SHALLOW Then
                I$ = "Y"
            Else
                If A1 < 0 And (INNING% >= 7 Or S2% = EVENT_FLY_SHALLOW) Then
                    I$ = "N"
                End If
            End If

            If S2% = EVENT_2B And nbrOuts(0) = 2 And baseNbr! = 3 Then
                I$ = "N"
            End If
        End If

        skipThrow% = 0
        If Not (playerMode = 2 Or playerMode = 1 And P = compTeam) Then
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then
            noThrow = 1
        Else
            pbpString$ = player$(baseNbr! - 1) + " is "

            If Environ$("PBPLOG") = "TRUE" Then Print #11, ";;;;; Runner going for base: "; baseNbr!; " on event: "; S2%

            If S2% < EVENT_HR Then
                Call PBP(pbpString$ + "going for " + baseName$(baseNbr!))
            Else
                'S2% >= 10
                Call PBP(pbpString$ + "tagging")
            End If

            If Not (S2% = EVENT_1B And baseNbr! = 2 Or S2% > EVENT_HR And baseRunners = 3) Then

                If Not (playerMode = 2 Or playerMode = 1 And D = compTeam) Then
                    Call PBP(player$(4) + " throw to " + baseName$(baseNbr!) + "? (Y/N) ")
                End If

                Call Rolld100(RN)

                If (playerMode = 2 Or playerMode = 1 And D = compTeam) Then
                    If teamMgrProfileVal(D, 766) = 999 And baseNbr! = 4 Then
                        If INNING% >= teamMgrProfileVal(D, 767) And teamMgrProfileVal(D, 767) <> 0 And teamMgrProfileVal(D, 768) <= gameScore(D, 0) - gameScore(P, 0) Then
                            I$ = "N"
                        End If
                        If INNING% >= teamMgrProfileVal(D, 769) And teamMgrProfileVal(D, 769) <> 0 And teamMgrProfileVal(D, 770) <= gameScore(D, 0) - gameScore(P, 0) Then
                            I$ = "N"
                        End If
                    Else
                        I$ = "Y"
                    End If
                Else
                    I$ = GetKeyPress$
                End If

                noThrow = 0

                If UCase$(I$) <> "Y" Then
                    noThrow = 1
                    Call PBP("No throw is made")
                    skipThrow% = 1
                End If

            End If

            If skipThrow% = 0 Then

                Call WINDEX
                Call PBP("Here comes the throw by " + player$(4))
                'For i = 1 To 3: Call PBP("."): Next i: Call PBP(" ")
                Call PBP("...")

                Call FLDERR(errorOnPlay%, currFielder, D, I3, stealing, S2%, P)

                If errorOnPlay% <> 1 Then

                    I4 = Int(Rnd(1) * 1000) + 1

                    runRating = batRating(P, lineupPlayer(P, b2Base(baseNbr! - 1)), SS_B_Run)

                    If fldPos(P, b2Base((baseNbr! - 1))) = 1 Then
                        runRating = pitchRating(P, currPitcher(P), SS_P_Run)
                    End If

                    runRating = (runRating - 5) * 50

                    armRating = (batRating(D, fielder(D, currFielder), SS_B_Arm) - 5) * 25
                    If S2% = EVENT_FLY_DEEP Then
                        I3 = 1000 + runRating - armRating  ' WAS 800
                    End If
                    If S2% = EVENT_FLY_REGULAR Then
                        I3 = 800 + runRating - armRating  ' WAS 650
                    End If
                    If S2% = EVENT_FLY_SHALLOW Or S2% < EVENT_HR Then
                        I3 = 500 + runRating - armRating
                    End If

                    If I4 > I3 Then

                        A5%(baseNbr! - 1) = 11
                        nbrOuts(0) = nbrOuts(0) + 1
                        nbrOuts(1) = nbrOuts(1) + 1
                        CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                        Call Rolld100(RN)

                        Select Case RN
                            Case 1 To 4
                                If baseNbr! = 4 Then
                                    Call PBP(player$(baseNbr! - 1) + " collides with " + batters$(D, fielder(D, 2)))
                                    Call PBP(batters$(D, fielder(D, 2)) + " hangs on!!")
                                End If

                            Case 5 To 50
                                If baseNbr! <> 4 Then
                                    Call PBP(player$(baseNbr! - 1) + " doesn't make it!")
                                End If

                            Case 51 To 80
                                Call PBP("throw is perfect!")

                            Case 81 To 90
                                Call PBP("This will be close")

                            Case 91 To 100
                                Call PBP("Close play.....")
                                If baseNbr! < 4 Then
                                    Call PBP(gameUmpire$(baseNbr! + 1) + " makes the call...")
                                Else
                                    Call PBP(gameUmpire$(1) + " makes the call...")
                                End If
                        End Select

                        Call CREDITASSIST(currFielder, D)
                        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                        Call CREDITPUTOUT(2, FB2P%, AST%, D)
                        currFielder = 2

                        If nbrOuts(0) = 3 Or S2% > EVENT_HR Then Exit Sub

                       LRunnerGoing:
                        Call Rolld100(I5)

                        A1 = gameScore(P, 0) - gameScore(D, 0)

                        If Not (A1 < -2 Or A1 < 0 And INNING% > 8) Then

                            ' *** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
                            If baseNbr! - 1 > 2 And I5 <= 15 And b2Base(2) > 0 Then
                                Call CREDITASSIST(currFielder, D)
                                Call PBP("runner is trying for 3rd...")
                                A5%(2) = 11
                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1
                                currFielder = 5
                                Call CREDITREGPO(currFielder, D)
                                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                            End If

                            ' *** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***
                            Call Rolld100(I5)

                            If baseNbr! - 1 > 1 And I5 <= 15 And b2Base(1) > 0 And A5%(2) <> 11 Then
                                Call CREDITASSIST(currFielder, D)
                                currFielder = 6
                                Call CREDITREGPO(currFielder, D)
                                Call PBP("runner is trying for 2nd...")
                                A5%(1) = 11
                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1
                                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)
                            End If

                            Call Rolld100(I5)

                            If b2Base(2) > 0 And baseNbr! - 1 > 2 And A5%(2) <> 11 And I5 > (80 - batRating(P, lineupPlayer(P, b2Base(2)), SS_B_Run) * 2) And noThrow = 0 Then
                                A5%(2) = 1
                                Call PBP("runner is trying for 3rd...")
                                Call PBP("He's safe!")
                            End If

                            I5 = Int(Rnd(1) * 10) + 1

                            If b2Base(1) > 0 And A5%(1) <> 11 And (baseRunners = 4 Or baseRunners = 7 And baseNbr! - 1 > 1 And I5 > (10 - batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run)) And noThrow = 0) Then
                                A5%(1) = 1
                                Call PBP("runner is trying for 2nd...")
                                Call PBP("He's in there!")
                            Else

                                Call Rolld100(I5)

                                If b2Base(1) > 0 And baseNbr! - 1 > 1 And I5 > (80 - batRating(P, lineupPlayer(P, b2Base(1)), SS_B_Run) * 2) And noThrow = 0 And A5%(1) <> 11 Then
                                    A5%(1) = 1
                                    Call PBP("runner safe at 2nd on the throw")
                                End If

                            End If

                        End If

                        Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                        noThrow = 0

                        Exit Sub

                    End If

                End If

                A5%(baseNbr! - 1) = 1

                If noThrow <> 1 Then

                    Call Rolld100(RN)

                    Select Case RN
                        Case 1 To 4:
                            If baseNbr! = 4 Then
                                Call PBP(player$(baseNbr! - 1) + " collides with " + batters$(D, fielder(D, 2)))
                                Call PBP("He knocks the ball loose!!")
                            End If

                        Case 5 To 50:
                            If baseNbr! <> 4 Then
                                Call PBP(player$(baseNbr! - 1) + " beats the throw!")
                            End If

                        Case 51 To 60:
                            Call PBP(player$(baseNbr! - 1) + " is ahead of the throw")

                        Case 61 To 80:
                            Call PBP("throw is off line")

                        Case 81 To 90:
                            Call PBP(player$(baseNbr! - 1) + " slides under the tag")

                        Case 91 To 100:
                            Call PBP("Close play...")

                            If baseNbr! <> 4 Then
                                Call PBP(gameUmpire$(baseNbr! + 1) + " makes the call...")
                            Else
                                Call PBP(gameUmpire$(1) + " makes the call...")
                            End If

                    End Select

                End If

                Call PBP(player$(baseNbr! - 1) + " is safe @ " + baseName$(baseNbr!))

                'sah% = safeAtHome ?
                If baseNbr! = 4 Then
                    sah% = 1
                End If

                Call TAGADVANCE(S2%, baseNbr!, D)

                If S2% < EVENT_HR Then Goto LRunnerGoing

                If S2% > EVENT_HR Then

                    If fldPos(P, currLineupSlot(P)) = 1 Then

                        gamePitStats(P, currPitcher(P), 23) = gamePitStats(P, currPitcher(P), 23) - 1
                        leagPitStats(P, currPitcher(P), 23) = leagPitStats(P, currPitcher(P), 23) - 1
                        Call INCPITOFF(P, currPitcher(P), LG_PB_SF)
                        Call ADDPIT(D, currPitcher(D), 16)

                    Else

                        gameBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) = gameBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) - 1
                        leagBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) = leagBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) - 1

                        If pitchRating(D, currPitcher(D), SS_P_Throws) <> 1 Then
                            CSS(P, lineupPlayer(P, currLineupSlot(P)), 0) = CSS(P, lineupPlayer(P, currLineupSlot(P)), 0) - 1
                        End If

                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_SF)

                        Call ADDPIT(D, currPitcher(D), 16)

                        GoTo LRunnerGoing

                    End If

                End If

                'Loop Until S2% >= 10 And fldPos(P, currLineupSlot(P)) = 1

            End If 'Done checking for fielding error

        End If 'Done checking if we're going to try for base

    End If

End Sub

'----------------------------------------
'          VIEWTEAM Subroutine
'----------------------------------------
'This subroutine presents a detailed view
'of the team's stats for reference during
'gameplay
Sub VIEWTEAM ()

    If useVGA = 1 Then Screen menu&

    Do
        Cls
        Color 12: Print Tab(20); "SELECT TEAM TO VIEW DETAILED STATS FOR:"
        Print
        Print
        Color 14, 0: Print "(0) ";: Color teamRat_GAME(0, 11), teamRat_GAME(0, 12): Print gameTeams$(0)
        Print
        Color 14, 0: Print "(1) ";: Color teamRat_GAME(1, 11), teamRat_GAME(1, 12): Print gameTeams$(1)
        Print
        Color 14, 0: Print "(2) ";: Color 15: Print "RETURN TO PLAY"

        Do
            I$ = GetKeyPress$
            WK = Val(I$)
        Loop Until WK >= 0 And WK <= 2

        If WK < 2 Then

            Cls
            Color 14: Print "(1) ";: Color 15: Print "VIEW PITCHERS"
            Print
            Color 14: Print "(2) ";: Color 15: Print "VIEW HITTERS"
            Print
            Color 14: Print "(3) ";: Color 15: Print "RETURN TO PLAY"

            Do
                I$ = GetKeyPress$
                Q = Val(I$)
            Loop Until Q >= 1 And Q <= 3

            Select Case Q

                Case 1

                    Do

                        Cls
                        Color 12: Print "VIEW BATTERS (AVAILABLE PLAYERS HIGHLIGHTED)"

                        For idx = 0 To 10:

                            Color 3, 0

                            If batRating(WK, idx, SS_B_Avail) = 0 Then Color 11
                            Locate idx + 5, 5: Print Using "##"; idx;
                            Print " "; batters$(WK, idx); Tab(40);
                            Color 3

                            If batRating(WK, idx + 12, SS_B_Avail) = 0 Then Color 11
                            Print Using "##"; idx + 12;
                            Print " "; batters$(WK, idx + 12)

                            Color 3
                        Next

                        If batRating(WK, 11, SS_B_Avail) = 0 Then Color 11

                        Locate , 5: Print Using "##"; 11;: Print " "; batters$(WK, 11)

                        Color 3

                        Do
                            Color 15
                            Locate 20, 1: Print "# TO VIEW (0-22) (23 TO CONTINUE) ";: Input M
                        Loop Until M >= 0 And M <= 23

                        If M <> 23 Then

                            Locate 3, 3
                            Color teamRat_GAME(WK, 11), teamRat_GAME(WK, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

                            For X = 1 To 7:
                                Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " "
                            Next

                            Locate , 3
                            Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

                            Locate 4, 6
                            Print "Stats for "; batters$(WK, M)
                            Locate , 6
                            Print "BATS: "; handed$(batRating(WK, M, SS_B_Bats) + 2); "    ";
                            Print Using "G/A%: ##       BUNT: #       RUN: #"; batRating(WK, M, SS_B_GndPct); batRating(WK, M, SS_B_Bunt); batRating(WK, M, SS_B_Run)
                            Locate , 3
                            Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
                            Locate , 6
                            Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

                            Call PopUp_Batter(WK, M)
                            I$ = GetKeyPress$
                            Color 7, 0

                        End If

                    Loop Until M = 23

                Case 2

                    Do
                        Cls

                        Color 12: Print "VIEW PITCHERS (AVAILABLE PLAYERS HIGHLIGHTED)"

                        For idx = 0 To 10:

                            Color 3, 0

                            If pitchRating(WK, idx, SS_P_Avail) = 0 Then Color 11

                            Locate idx + 5, 5: Print Using "##"; idx;
                            Print " "; pitchers$(WK, idx); Tab(40);

                            Color 3

                            If pitchRating(WK, idx + 11, SS_P_Avail) = 0 Then Color 11

                            Print Using "##"; idx + 11;
                            Print " "; pitchers$(WK, idx + 11)

                            Color 3

                        Next

                        Do
                            Color 15
                            Locate 20, 1: Print "# TO VIEW (0-21) (22 TO CONTINUE) ";
                            Input M
                        Loop Until M >= 0 And M <= 22

                        If M <> 22 Then
                            Call PopUp_Pitcher(WK, M)
                            I$ = GetKeyPress$
                            Color 7, 0
                        End If

                    Loop Until M = 22

            End Select

        End If

    Loop Until WK = 2 Or Q = 3 Or M = 23

End Sub


'----------------------------------------
'         WALLDOUBLE Subroutine
'----------------------------------------
'This subroutine generates the Play-by-Play
'text for a double off the wall
Sub WALLDOUBLE (HW%, currFielder)

    Call Rolld100(BV)
    Call Rolld100(RN)

    Select Case BV

        Case 1 To 40:

            If HW% = 0 Then pbpString$ = "It's in the gap and to the wall"

        Case 41 To 65

            Select Case RN
                Case 1 To 25
                    If (teamRat_GAME(1, 13) = 8 Or teamRat_GAME(1, 13) = 40) Then
                        pbpString$ = "It's off the ivy!"
                    End If
                Case 26 To 50
                    If teamRat_GAME(1, 13) = 21 And currFielder = 7 Then
                        pbpString$ = "It hits off the Green Monster!"
                    End If
            End Select

            If teamRat_GAME(1, 13) = 27 And currFielder = 7 Then
                pbpString$ = "It hits off the screen!"
            Else
                pbpString$ = "It's off the wall"
            End If

        Case 66 To 85

            If HW% = 0 Then
                If currFielder = 8 Then
                    pbpString$ = "It's in the gap and to the wall"
                Else
                    Call PBP("It's past " + player$(4))
                    pbpString$ = "It goes into the corner..."
                End If
            End If

        Case 86 To 90

            If HW% = 0 Then
                Call PBP("It's to the wall")
                Call PBP(player$(0) + " slides into second")
                pbpString$ = "It's a stand-up double!!"
            End If

        Case Else
            If RN <= 25 And teamRat_GAME(1, 13) = 21 Then
                pbpString$ = "It's off the Green Monster!"
            Else
                If teamRat_GAME(1, 13) = 27 And currFielder = 7 Then
                    pbpString$ = "It's off the screen..."
                Else
                    pbpString$ = "It's off the top of the wall..."
                End If
            End If

    End Select

    Call PBP(pbpString$)

End Sub


'----------------------------------------
'         WHICHRUNNER Subroutine
'----------------------------------------
'This subroutine lets the user choose which
'runner to target when there is an attempt
'to steal a base
Sub WHICHRUNNER (i, baseRunners, D5)

    If i = 4 Then A5%(3) = 1

    If baseRunners = 4 Then

        Call PBP("WHICH RUNNER DO YOU WANT TO GO AFTER")

        Call PBP("1 - " + batters$(P, lineupPlayer(P, b2Base(2))))

        If fldPos(P, b2Base(1)) <> 1 Then
            Call PBP("2 - " + batters$(P, lineupPlayer(P, b2Base(1))))
        Else
            Call PBP("2 - " + pitchers$(P, currPitcher(P)))
        End If

        If playerMode = 2 Or playerMode = 1 And P <> compTeam Then
            D5 = Int(Rnd(1) * 2) + 1
            i$ = Str$(D5)
        Else
            i$ = GetKeyPress$
        End If

        D5 = Val(i$)

        If D5 = 2 Then
            i = 2
            D5 = 3
            I2 = lineupPlayer(P, b2Base(1))
            If fldPos(P, b2Base(1)) = 1 Then
                player$(i - 1) = pitchers$(P, currPitcher(P))
            End If
        End If

        D5 = 0

    End If

End Sub


'----------------------------------------
'          WHOSON1ST Subroutine
'----------------------------------------
' ...explanation...
Sub WHOSON1ST (baseRunners)

    baseRunners = 0
    I1 = b2Base(1)
    I2 = b2Base(2)
    I3 = b2Base(3)

    If I1 > 0 Then
        If I2 = 0 Then
            If I3 = 0 Then
                baseRunners = 1
            Else
                ' *** 1ST AND 3RD ***
                baseRunners = 5
            End If
        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** 1ST AND 2ND ***
                baseRunners = 4
            Else
                'I3 > 0
                ' *** BASES LOADED ***
                baseRunners = 7
            End If
        End If
    Else
        'I1 = 0
        If I2 = 0 Then
            If I3 > 0 Then
                ' *** RUNNER ON 3RD ***
                baseRunners = 3
            End If

        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** RUNNER ON 2ND ***
                baseRunners = 2
            Else
                'I3 > 0
                ' *** 2ND AND 3RD ***
                baseRunners = 6
            End If
        End If
    End If

End Sub


'----------------------------------------
'          FOULBALL Subroutine
'----------------------------------------
'This subroutine handles the play-by-play
'for a foul ball
Sub FOULBALL ()

    If d100% <= 50 Then
        Call GetPBPString(20, 3130, fileString$, D)
        Call PBP(fileString$)
        Call PBP("foul ball!!")
    Else
        Call GetPBPString(10, 3150, fileString$, D)
        Call PBP(fileString$)

        Call GetPBPString(10, 3160, fileString$, D)
        Call PBP(fileString$)
    End If
End Sub


'----------------------------------------
'          RAREPLAYS Subroutine
'----------------------------------------
'This subroutine uses a random number to
'determine what rare play has occurred.
'It also gives the relevant Play-by-Play.

'H6% gets randomly assigned first. Then
'at the end of most of these branches,
'it gets assigned a static value.
' Where is it used in the game?
Sub RAREPLAYS (H6%, H7%, IP%)

    If H7% = 20 Then
        H7% = 0
    Else

        Call WINDEX
        Call Rolld100(H6%)

        Select Case H6%

            Case 1

                Call PBP(batters$(D, fielder(D, 2)) + " is arguing ball and strike calls")
                Call PBP(gameUmpire$(1) + " has heard enough")
                Call PBP(batters$(D, fielder(D, 2)) + " has been ejected!!")

                X$ = batters$(D, fielder(D, 2))

                Call EJECTINC(ejectCnt, X$)
                Call DELAY

                po% = 2

                Call EJECTIONINJURY(playerMode, P, compTeam, D, po%, P9, W5, W6)

            Case 2 To 3

                Call PBP(pitchers$(D, currPitcher(D)) + " steps off the mound")
                Call PBP("after that last call")
                Call PBP("He is shouting at " + gameUmpire$(1))
                Call PBP(gameUmpire$(1) + " does not appreciate what he hears...")
                Call PBP(pitchers$(D, currPitcher(D)) + " has been ejected!!")

                X$ = pitchers$(D, currPitcher(D))

                Call EJECTINC(ejectCnt, X$)
                Call DELAY
                Call INJPIT(P, D, IP%, PPH)

            Case 4
                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call PBP(player$(0) + " is unhappy with that last call"): Call PBP("He is venting his opinion to " + gameUmpire$(1)): Print "Thats it!! "; gameUmpire$(1); " has ejected "; player$(0); "!!"
                    X$ = player$(0)
                    Call EJECTINC(ejectCnt, X$)
                    Call DELAY
                    Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)
                Else
                    Call PBP(player$(0) + " did not like that")
                    Call PBP("last call at all!!")
                End If

            Case 5

                Call PBP(player$(0) + " hits a slow roller to first")
                Call PBP(batters$(D, fielder(D, 3)) + " fields it cleanly")
                Call PBP("He flips it to " + pitchers$(D, currPitcher(D)) + " covering just in time!")
                Call PBP("Wait... " + gameUmpire$(2) + " rules safe!!")

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                End If

                Call ADDPIT(D, currPitcher(D), LG_P_H)

                If fldPos(P, currLineupSlot(P)) = 1 Then
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                End If

                For idx = 0 To 3:
                    A5%(idx) = 1
                Next idx

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call SCOREBOARD
                Call WINDEX

                Call PBP(gameManagers$(D) + " charges from the dugout")
                Call PBP(gameManagers$(D) + ", " + pitchers$(D, currPitcher(D)) + ",")
                Call PBP(batters$(D, fielder(D, 2)) + ", and " + batters$(D, fielder(D, 3)))
                Call PBP("are going at it with " + gameUmpire$(2))
                Call DELAY
                Call DELAY

                Call PBP(gameUmpire$(3) + " and " + gameUmpire$(1) + " come in to help out")
                Call PBP("This argument is out of control now!!")
                Call PBP("What is this?!")
                Call PBP("All of them have been ejected!!!")

                X$ = pitchers$(D, currPitcher(D))

                Call EJECTINC(ejectCnt, X$)

                X$ = batters$(D, fielder(D, 2))

                Call EJECTINC(ejectCnt, X$)

                X$ = batters$(D, fielder(D, 3))

                Call EJECTINC(ejectCnt, X$)

                X$ = gameManagers$(D)

                Call EJECTINC(ejectCnt, X$)
                Call DELAY

                po% = 2

                Call EJECTIONINJURY(playerMode, P, compTeam, D, po%, P9, W5, W6)

                po% = 3

                Call EJECTIONINJURY(playerMode, P, compTeam, D, po%, P9, W5, W6)
                Call INJPIT(P, D, IP%, PPH)

            Case 6 To 7

                Call PBP(player$(0) + " sends a long one")
                Call PBP("down the left field line")
                Call PBP("It may go...")
                Call PBP("HOME RUN!!!")
                Call PBP("Wait!!! ")
                Call PBP(gameUmpire$(4) + " is calling it foul!!")

                Call WINDEX

                Call PBP(player$(0) + " is arguing vehemently")
                Call PBP("with " + gameUmpire$(1) + " and " + gameUmpire$(4))
                Call PBP(gameManagers$(P) + " charges from the dugout")
                Call PBP("He and " + player$(0) + " are really going")
                Call PBP("at it with the umpires!!")
                Call DELAY
                Call PBP(" has just been tossed!!"): X$ = player$(0): Call EJECTINC(ejectCnt, X$)
                Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

            Case 8

                Call PBP(gameUmpire$(1) + " has just ejected " + pitchers$(D, currPitcher(D)))
                Call PBP("for wetting the ball!!")
                Call EJECTINC(ejectCnt, X$)

                ejections$(ejectCnt) = pitchers$(D, currPitcher(D))

                Call DELAY
                Call INJPIT(P, D, IP%, PPH)

            Case 9

                Call PBP("Here's the 3-2 pitch...")
                Call PBP("Time called...")
                Call PBP(gameUmpire$(1) + " is calling it ball 4")
                Call PBP("He says " + pitchers$(D, currPitcher(D)) + " went to his mouth")

            Case 10 To 15

                Call PBP(player$(0) + " hits the deck on a high")
                Call PBP("and tight pitch from " + pitchers$(D, currPitcher(D)))
                Call PBP(player$(0) + " charges the mound!")
                Call PBP("Both dugouts have emptied...")
                Call PBP("It's a bench-clearing brawl!")
                Call WINDEX
                Call PBP("It will take a few minutes to clean this up")

                RN = Int(Rnd(1) * 3) + 1

                Select Case RN

                    Case 1:
                        Call PBP(pitchers$(D, currPitcher(D)) + " has been ejected")
                        X$ = pitchers$(D, currPitcher(D))
                        Call EJECTINC(ejectCnt, X$)
                        Call INJPIT(P, D, IP%, PPH)

                    Case 2:
                        Call PBP(player$(0) + " has been ejected")
                        X$ = player$(0)
                        Call EJECTINC(ejectCnt, X$)
                        Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                    Case 3:
                        Call PBP(player$(0) + " and " + pitchers$(D, currPitcher(D)) + " have been ejected")
                        X$ = player$(0)
                        Call EJECTINC(ejectCnt, X$)
                        X$ = pitchers$(D, currPitcher(D))
                        Call EJECTINC(ejectCnt, X$)
                        Call INJPIT(P, D, IP%, PPH)
                        Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                End Select

            Case 16 To 21

                H6% = 21

                Call WINDEX

                If rainDelay = 0 And (precipType = 0 Or precipType = 4) Then
                    Call PBP(player$(0) + " fouls the pitch out of play")
                Else
                    tm = Int(Rnd(1) * 59) + 2

                    Color 0, 7
                    Call PBP("The game has been halted by rain")

                    For idx = 1 To 2: Call DELAY: Next

                    Call PBP("Rain delay lasted" + Str$(tm) + " minutes")
                    Call DELAY

                    gameDelayed = 1

                    If tm > 28 Then
                        Call PBP("Pitchers have lost effectiveness during delay")
                        Call PBP("Their arms have stiffened up")
                        Call DELAY
                        For idx = 0 To 1:
                            S8%(idx, 0) = S8%(idx, 0) - 2
                        Next
                    End If

                End If

            Case 22 To 24

                Select Case baseRunners
                    Case 0
                        If teamRat_GAME(1, 13) = 14 Then
                            Call PBP(player$(0) + " steps out of the box as a jet passes over")
                        Else
                            Call PBP(gameUmpire$(1) + " calls time to get some new baseballs")
                        End If
                        pickedOffBase = 0

                    Case 1
                        Call PBP(batters$(D, fielder(D, 2)) + " rifles the ball to ")
                        Call PBP("1st...")
                        pickedOffBase = 2

                    Case 2, 4
                        Call PBP(batters$(D, fielder(D, 2)) + " rifles the ball to ")
                        Call PBP("2nd base...")
                        pickedOffBase = 3

                    Case 3, 5, 6, 7
                        Call PBP(batters$(D, fielder(D, 2)) + " rifles the ball to ")
                        Call PBP("3rd base...")
                        pickedOffBase = 4

                End Select

                If pickedOffBase > 0 Then

                    Call PBP(player$(pickedOffBase - 1) + " is picked off base!")

                    A5%(pickedOffBase - 1) = 10
                    nbrOuts(0) = nbrOuts(0) + 1
                    nbrOuts(1) = nbrOuts(1) + 1
                    currFielder = 2

                    Call CREDITASSIST(currFielder, D)
                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                    If nbrOuts(0) >= 3 Then
                        currLineupSlot(P) = currLineupSlot(P) - 1
                        H6% = 5
                    End If

                End If

            Case 25 To 29

                Call PBP(player$(0) + " pops it up near the 3rd base stands")
                Call PBP("A fan interferes...")
                Call PBP(batters$(D, fielder(D, 6)) + " makes a great catch!!")

                currFielder = 6
                Call CREDITREGPO(currFielder, D)

                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                Call DELAY

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                End If

                CALL ADDPIT(D, currPitcher(D), LG_P_OUT)

                If fldPos(P, currLineupSlot(P)) = 1 Then
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                End If

                Call DELAY

            Case 30

                Call PBP(player$(0) + " pops it up foul near the stands")
                Call PBP(batters$(D, fielder(D, 2)) + " runs into the stands!!")

                po% = 2

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 30 To 31

                Call PBP(player$(0) + " pops it up foul near the stands")
                Call PBP(batters$(D, fielder(D, 3)) + " gives chase")
                Call PBP("he runs into the stands!!")

                po% = 3

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 32

                Call PBP(player$(0) + " pops it up foul down the rf line")
                Call PBP(batters$(D, fielder(D, 4)) + " crashes into the wall")

                po% = 4

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 33

                Call PBP(player$(0) + " pops it up foul by")
                Call PBP("the 3rd base dugout")
                Call PBP(batters$(D, fielder(D, 5)) + " falls in trying to make the catch!!")

                po% = 5

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 34

                Call PBP(player$(0) + " pops it up foul")
                Call PBP("down the lf line")
                Call PBP(batters$(D, fielder(D, 6)) + " tumbles over the railing")
                Call PBP("trying to make the catch!")
                Call DELAY

                po% = 6

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 35 To 37

                Call PBP(player$(0) + " lines one to deep left field")
                Call PBP(batters$(D, fielder(D, 7)) + " races back...")
                Call PBP("He slams into the wall and is down!!")

                op% = 7

                Call PBP(batters$(D, fielder(D, 8)) + " comes over and gets the ball in")

                Call ADDPIT(D, currPitcher(D), LG_P_H)

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
                End If

                For idx = 0 To 3:
                    A5%(idx) = 2
                Next idx

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = op%

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 38 To 40

                Call PBP(player$(0) + " drives a sinking liner")
                Call PBP("into deep center")
                Call PBP(batters$(D, fielder(D, 8)) + " gets a good jump on it")
                Call PBP("He crashes into the wall and")
                Call PBP("the ball bounces away!!")
                Call PBP("The runners are circling the bases")
                Call PBP(batters$(D, fielder(D, 9)) + " retrieves the ball")

                CALL ADDPIT(D, currPitcher(D), LG_P_H)
                Call ADDPIT(D, currPitcher(D), LG_P_HR)

                currFielder = 8

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_HR)
                End If

                If fldPos(P, currLineupSlot(P)) = 1 Then
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_HR)
                End If

                A5%(0) = 4
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call PBP("It's an inside the park HOME RUN!!!")
                Call DELAY
                Call SCOREBOARD
                Call WINDEX

                po% = 8

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 41 To 43

                Call PBP(player$(0) + " lifts a deep drive into right")
                Call PBP(batters$(D, fielder(D, 9)) + " races back...")
                Call PBP("He slams into the wall and is down!!")

                CALL ADDPIT(D, currPitcher(D), LG_P_H)

                currFielder = 9

                Call PBP(batters$(D, fielder(D, 8)) + " comes over to get the ball back in")

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Trp)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Trp)
                End If

                A5%(0) = 3
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call PBP("Triple!!")
                Call DELAY
                Call SCOREBOARD
                Call WINDEX

                po% = 9

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 44

                CALL ADDPIT(D, currPitcher(D), LG_P_H)

                currFielder = Int(Rnd(1) * 3) + 7

                Call PBP(player$(0) + " hits a sinking liner into " + fieldPos$(currFielder))
                Call PBP(batters$(D, fielder(D, currFielder)) + " dives for the catch...")
                Call PBP("the ball rolls free!")
                Call PBP(batters$(D, fielder(D, currFielder - 1)) + " gets the loose ball in")

                op% = currFielder

                CALL ADDPIT(D, currPitcher(D), LG_P_H)

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
                End If

                For idx = 0 To 3:
                    A5%(idx) = 2
                Next idx

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = op%

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 45

                Call PBP(player$(0) + " is hit by a pitch and injured")
                Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

            Case 46 To 49

                Call PBP(batters$(D, fielder(D, 2)) + " is injured by a foul tip!")

                po% = 2

                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 50 To 58

                i = Int(Rnd(1) * 3) + 7

                If i = 9 Then
                    hi1 = 3
                    hi2 = 9
                    hi3 = 8
                End If
                If i = 8 Then
                    hi1 = 4
                    hi2 = 8
                    hi3 = 7
                End If
                If i = 7 Then
                    hi1 = 6
                    hi2 = 7
                    hi3 = 8
                End If

                Call PBP(player$(0) + " hits a weak fly into short " + fieldPos$(i))
                Call PBP(batters$(D, fielder(D, hi1)) + " goes out")
                Call PBP(batters$(D, fielder(D, hi2)) + " is charging in")
                Call PBP("They collide and the ball rolls free!!")
                Call PBP(batters$(D, fielder(D, hi3)) + " gets it back in")

                CALL ADDPIT(D, currPitcher(D), LG_P_H)

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
                End If

                For idx = 0 To 3:
                    A5%(idx) = 2
                Next idx

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = hi1
                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                po% = hi2
                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 59 To 61
                Call PBP(player$(0) + " fouls the pitch off his foot")
                Call PBP("He is injured")
                Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                H6% = 1

                '62 to 64
            Case 62 To 64

                Call PBP(player$(0) + " pops it up in foul along the 3b line")
                Call PBP("It drifts into foul territory")
                Call PBP(batters$(D, fielder(D, 5)) + " and " + pitchers$(D, currPitcher(D)) + " chase after it and collide!!")
                Call PBP(batters$(D, fielder(D, 5)) + " is down and injured")

                po% = 5
                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 65 To 70

                CALL ADDPIT(D, currPitcher(D), LG_P_H)

                currFielder = 8

                Call LONGFLY(D, currFielder)
                Call PBP(batters$(D, fielder(D, 7)) + " collides with " + batters$(D, fielder(D, 8)) + "!!")
                Call PBP("both fielders are down... ")
                Call PBP(batters$(D, fielder(D, 6)) + " gets the ball back in")

                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
                End If

                A5%(0) = 2
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                Call PBP("Double!!")
                Call SCOREBOARD
                Call WINDEX

                po% = 7
                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)

                po% = 8
                Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)
                H6% = 5

            Case 71 To 85

                pbpString$ = player$(0) + " lines it off "

                i = Int(Rnd(1) * 5) + 1

                If i = 1 Then
                    pbpString$ = pbpString$ + "the leg of " + pitchers$(D, currPitcher(D))
                    Call PBP(pbpString$)

                    If fldPos(P, currLineupSlot(P)) <> 1 Then
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Else
                        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    End If

                    CALL ADDPIT(D, currPitcher(D), LG_P_H)

                    For idx = 0 To 3:
                        A5%(idx) = 1
                    Next

                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call SCOREBOARD
                    Call WINDEX
                    Call PBP("He reaches on a single")
                    Call PBP(pitchers$(D, currPitcher(D)) + " is down and in pain!")
                    Call INJPIT(P, D, IP%, PPH)
                    H6% = 5
                Else
                    'i = 2 to 5
                    po% = i + 1
                    pbpString$ = pbpString$ + "of " + batters$(D, fielder(D, po%)) + " for a single"

                    Call PBP(pbpString$)
                    Call DELAY

                    If fldPos(P, currLineupSlot(P)) <> 1 Then
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                    Else
                        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                    End If

                    CALL ADDPIT(D, currPitcher(D), LG_P_H)

                    For idx = 0 To 3:
                        A5%(idx) = 1
                    Next

                    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
                    Call SCOREBOARD
                    Call WINDEX
                    Call PBP(batters$(D, fielder(D, po%)) + " is down and injured")
                    Call INJURYLENGTH(injCount, D, po%, playerMode, P, compTeam, P9, W5, W6): H6% = 5

                End If

            Case 86 To 89

                Call PBP(pitchers$(D, currPitcher(D)) + " is motioning for " + gameManagers$(D) + " to come out")
                Call PBP("It appears he has a blister")
                Call PBP("He will have to leave the game")
                Call INJPIT(P, D, IP%, PPH)

                H6% = 1

            Case 90

                Call PBP(player$(0) + " takes a mighty swing!")
                Call PBP("It looks like he injured himself")
                Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                H6% = 1

            Case 91

                Call PBP("Time is called")
                Call PBP("Morganna strikes again!!")

                H6% = 1

            Case 92 To 100

                Call PBP(gameUmpire$(1) + " calls time to dust off the plate")

                H6% = 1

        End Select

    End If

End Sub


'----------------------------------------
'           WEATHER Subroutine
'----------------------------------------
'This subroutine prints the wind and weather
'conditions in a pop-up box
Sub WEATHER ()

    If useVGA = 1 Then
        Cls
        Screen menu&
    End If

    Call FRAMEOPTION(5, 1)

    If teamRat_GAME(1, 9) = 1 Then
        Locate , 11: Print "TEMPERATURE: 70"
        Locate , 11: Print "WEATHER: DOME"
        Locate , 11: Print "WIND: NONE"
    Else
        Locate , 11: Print "TEMPERATURE: "; gameTemp
        Locate , 11: Print "SKY CONDITIONS: "; skyCond$(skyType)
        windSpeed = Int(Rnd(1) * 15) + 7
        Locate , 11: Print "WIND: "; wind$
        Locate , 11: Print "PRECIPITATION: "; precip$(precipType)
    End If

    Color 15, 0
    Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    If useVGA = 1 Then
        Screen imageScreen&
    Else
        Color 15, 0
    End If

End Sub


'----------------------------------------
'           COLCHK Subroutine
'----------------------------------------
' ...explanation...
Sub COLCHK (P9, I)

    If batRating(P9, I, SS_B_Avail) >= 98 Then Color 0, 7

End Sub


'----------------------------------------
'         GETWEATHER Subroutine
'----------------------------------------
'This subroutine opens the PARKS.DBF file
'to read in attendance data, wind factor,
'weather and other ballpark calculations.
Sub GETWEATHER (windDir, windSpeed, gameMonth, WF!, skyType, gameTemp, rainDelay)

    Shared avgAttendance&

    If teamRat_GAME(1, 13) > 82 Then
        ID = Int(Rnd(1) * 79) + 1
    Else
        ID = teamRat_GAME(1, 13)
    End If

    'The layout of the PARKS.DB file is as follows:
    'NAME, CAPACITY, LF,LCF,CF,RCF,RF,GRASS,DOME,WINFAC
    '6 MONTHS OF DATA, TEMP, DELTA,RL,LR,INL,INC,INR,OUTL,OUTC,OUTR,WS09,WS1014,WS1519,WS20,PRECIP

    Open "PARKS.DBF" For Random As #1 Len = 240

    Field #1, 40 As Q3$(0), 4 As Q3$(1), 2 As Q3$(2), 2 As Q3$(3), 2 As Q3$(4), 2 As Q3$(5), 2 As Q3$(6), 2 As Q3$(7), 2 As Q3$(8), 2 As Q3$(9)

    For I = 0 To 5
        For I1 = 0 To 14
            Field #1, 60 + I * 30 + I1 * 2 As X$, 2 As Q3$(10 + I1 + I * 15)
        Next
    Next

    Get #1, ID

    avgAttendance& = CVL(Q3$(1))
    LF = CVI(Q3$(2))
    LCF = CVI(Q3$(3))
    CF = CVI(Q3$(4))
    RCF = CVI(Q3$(5))
    RF = CVI(Q3$(6))
    TG = CVI(Q3$(7))
    DOME = CVI(Q3$(8))
    WINFAC = CVI(Q3$(9))

    For I = 0 To 5
        For I1 = 0 To 14
            MON(I, I1) = CVI(Q3$(10 + I1 + I * 15))
        Next
    Next
    Close 1

    If gameMonth < 4 Then gameMonth = 4
    If gameMonth > 9 Then gameMonth = 9

    SM% = gameMonth - 4

    If d100% <= 50 Then
        gameTemp = MON(SM%, 0) + Int(Rnd(1) * MON(SM%, 1) + 1)
    Else
        gameTemp = MON(SM%, 0) - Int(Rnd(1) * MON(SM%, 1) + 1)
    End If

    SUM = 0

    For I = 2 To 9
        SUM = SUM + MON(SM%, I)
    Next

    RN = Int(Rnd(1) * SUM) + 1

    SUM = 0

    For I = 2 To 9
        SUM = SUM + MON(SM%, I)
        If RN <= SUM Then Exit For
    Next

    windDir = I

    SUM = 0
    For I = 10 To 13
        SUM = SUM + MON(SM%, I)
    Next

    RN = Int(Rnd(1) * SUM) + 1

    SUM = 0

    For I = 10 To 13
        SUM = SUM + MON(SM%, I)
        If RN <= SUM Then Exit For
    Next

    If I = 10 Then windSpeed = Int(Rnd(1) * 10)
    If I = 11 Then windSpeed = Int(Rnd(1) * 5) + 10
    If I = 12 Then windSpeed = Int(Rnd(1) * 5) + 15
    If I = 13 Then windSpeed = Int(Rnd(1) * 10) + 20

    WF! = 1

    If windDir > 3 And windDir < 7 And windSpeed >= 15 Then WF! = 1 - WINFAC / 100
    If windDir > 6 And windDir < 10 And windSpeed >= 15 Then WF! = 1 + WINFAC / 100

    precipType = 4
    skyType = Int(Rnd(1) * 2) + 1

    If d100% <= MON(SM%, 14) Then

        precipType = Int(Rnd(1) * 3) + 1
        skyType = 3

        If d100% <= 5 Then

            rainDelay = 1

            'rainStart is an inning from 6 to 8
            rainStart = Int(Rnd(1) * 3) + 6

        End If

    End If

    If teamRat_GAME(1, 13) = 2 Or teamRat_GAME(1, 13) = 4 Or teamRat_GAME(1, 13) = 10 Or teamRat_GAME(1, 13) = 13 Or teamRat_GAME(1, 13) = 24 Then
        precipType = 4
        skyType = 4
    End If

End Sub


'----------------------------------------
'           LINSWAP Subroutine
'----------------------------------------
' ...explanation...
Sub LINSWAP (P9)

    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

    Do
        I$ = GetKeyPress$
        K1 = Val(I$)
    Loop Until K1 >= 1 And K1 <= 9 Or fldPos(P9, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or fldPos(P9, K2) <> 1

    Swap fldPos(P9, K2), fldPos(P9, K1)
    Swap lineupPlayer(P9, K1), lineupPlayer(P9, K2)

    fielder(P9, fldPos(P9, K1)) = lineupPlayer(P9, K1)
    fielder(P9, fldPos(P9, K2)) = lineupPlayer(P9, K2)

    Call NEWLINES(P9)

    Locate 18, 59: Print Space$(17)
    Locate 19, 59: Print Space$(17)

End Sub


'----------------------------------------
'          NEWLINES Subroutine
'----------------------------------------
' ...explanation...
Sub NEWLINES (P9)

    For I = 1 To 9

        Call PositionCursor(I)

        Print Using "# "; I;

        If fldPos(P9, I) <> 1 Then
            Print batters$(P9, lineupPlayer(P9, I));
        Else
            If I5 <> 66 Then
                Print pitchers$(P9, lineupPlayer(P9, I));
            End If
        End If

        Print Tab(74);

        Print fieldPos$(fldPos(P9, I));

    Next I

    I5 = 0

End Sub


'----------------------------------------
'          PositionCursor Subroutine
'----------------------------------------
' This subroutine "positions" the cursor
'on the screen at the appropriate row
'in order to print out the line positions.
Sub PositionCursor (I)

    Locate I + 7, 59

End Sub

'----------------------------------------
'           POSSWAP Subroutine
'----------------------------------------
' ...explanation...
Sub POSSWAP (P9)

    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

    Do
        I$ = GetKeyPress$
        K1 = Val(I$)
    Loop Until K1 >= 1 And K1 <= 9 Or fldPos(P9, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or fldPos(P9, K2) <> 1

    Swap fldPos(P9, K2), fldPos(P9, K1)

    fielder(P9, fldPos(P9, K1)) = lineupPlayer(P9, K1)
    fielder(P9, fldPos(P9, K2)) = lineupPlayer(P9, K2)

    Call NEWLINES(P9)

    Locate 18, 59: Print Space$(17)
    Locate 19, 59: Print Space$(17)

End Sub


'----------------------------------------
'         SHOWBATTERS Subroutine
'----------------------------------------
'This subroutine provides a summary dis-
'play of the batting averages
Sub SHOWBATTERS (P9)

    Cls

    Color 14
    Print "#";
    Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12): Print gameTeams$(P9);

    Color 14, 0

    Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"

    Locate 2

    For I = 0 To 22: Locate , 1

        If batters$(P9, I) = "XXX" Or Left$(batters$(P9, I), 1) = " " Or batters$(P9, I) = "" Then
            batRating(P9, I, SS_B_Avail) = 98
        Else
            Color 2, 0

            If I < 10 Then Print Using "#"; I;
            If I > 9 Then Print alpha$(I - 10);

            Color 15, 0
            Call COLCHK(P9, I)

            'Name / Position
            Locate , 3: Print batters$(P9, I);
            Color 15, 0
            Locate , 15: Print handed$(batRating(P9, I, SS_B_Bats) + 2);

            Locate , 18: If batRating(P9, I, SS_B_Pos1) <> -1 Then Print Using "#"; batRating(P9, I, SS_B_Pos1);
            Locate , 19: If batRating(P9, I, SS_B_Pos2) <> -1 Then Print Using "#"; batRating(P9, I, SS_B_Pos2);
            Locate , 20: If batRating(P9, I, SS_B_Pos3) <> -1 Then Print Using "#"; batRating(P9, I, SS_B_Pos3);
            Locate , 21: If batRating(P9, I, SS_B_Pos4) <> -1 Then Print Using "#"; batRating(P9, I, SS_B_Pos4);

            Locate , 23

            Print Using "### ## ## ## ### ###"; batRating(P9, I, SS_B_AB); batRating(P9, I, SS_B_2B); batRating(P9, I, SS_B_3B); batRating(P9, I, SS_B_HR); batRating(P9, I, SS_B_W); batRating(P9, I, SS_B_K);

            Locate , 44: Print Using "#"; batRating(P9, I, SS_B_Run);: Print Using "#"; batRating(P9, I, SS_B_Range1);: Print Using "#"; batRating(P9, I, SS_B_Arm1);

            Locate , 47
            '           SB   G%
            Print Using "### ##"; batRating(P9, I, SS_B_SB); batRating(P9, I, SS_B_GndPct);

            'BA
            If batRating(P9, I, SS_B_H) = batRating(P9, I, SS_B_AB) And batRating(P9, I, SS_B_H) <> 0 Then
                Print "1.000";
            Else
                If batRating(P9, I, SS_B_AB) = 0 Then
                    Print " .---";
                Else
                    bAvg! = batRating(P9, I, SS_B_H) / batRating(P9, I, SS_B_AB)
                    If bAvg! = 0 Then
                        Print " .000";
                    Else
                        Print " ."; Using "###"; bAvg! * 1000;
                    End If
                End If
            End If

            If I <> 22 Then Print

        End If

    Next I

End Sub


'----------------------------------------
'          SHOWFIELD Subroutine
'----------------------------------------
'This subroutine provides a summary dis-
'play of the fielding averages
Sub SHOWFIELD (P9)

    Locate 1, 15
    Color 14, 0
    Print "P1 FAVG A R P2 FAVG A R P3 FAVG A R P4 FAVG A R                 "

    Color 15, 0
    Locate 2

    For I = 0 To 22

        Locate , 15: Print Space$(64);

        If batters$(P9, I) <> "XXX" And Left$(batters$(P9, I), 1) <> " " And batters$(P9, I) <> "" Then
            Locate , 16
            If batRating(P9, I, SS_B_Pos1) <> -1 Then Print Using "# #### # #"; batRating(P9, I, SS_B_Pos1); batRating(P9, I, SS_B_FldPct1); batRating(P9, I, SS_B_Arm1); batRating(P9, I, SS_B_Range1);
            Locate , 28
            If batRating(P9, I, SS_B_Pos2) <> -1 Then Print Using "# #### # #"; batRating(P9, I, SS_B_Pos2); batRating(P9, I, SS_B_FldPct2); batRating(P9, I, SS_B_Arm2); batRating(P9, I, SS_B_Range2);
            Locate , 40
            If batRating(P9, I, SS_B_Pos3) <> -1 Then Print Using "# #### # #"; batRating(P9, I, SS_B_Pos3); batRating(P9, I, SS_B_FldPct3); batRating(P9, I, SS_B_Arm3); batRating(P9, I, SS_B_Range3);
            Locate , 52
            If batRating(P9, I, SS_B_Pos4) <> -1 Then Print Using "# #### # #"; batRating(P9, I, SS_B_Pos4); batRating(P9, I, SS_B_FldPct4); batRating(P9, I, SS_B_Arm4); batRating(P9, I, SS_B_Range4);
            If I <> 22 Then Print
        End If
    Next I

End Sub

'----------------------------------------
'          ShowSplit Subroutine
'----------------------------------------
'This subroutine presents a split view of
'team stats
Sub ShowSplit (P9)

    'DISPLAY SPLIT STATS
    Locate 1, 15
    Color 14, 0

    Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
    Color 15, 0

    Locate 2

    For I = 0 To 22

        Locate , 15: Print "                                                                ";

        If batters$(P9, I) <> "XXX" And Left$(batters$(P9, I), 1) <> " " And batters$(P9, I) <> "" Then

            Locate , 20: Print Using "### ## ## ## ### ### "; batRating(P9, I, SS_B_LHP_AB); batRating(P9, I, 44); batRating(P9, I, 45); batRating(P9, I, 46); batRating(P9, I, 47); batRating(P9, I, 48);

            If batRating(P9, I, SS_B_LHP_AB) = 0 Or batRating(P9, I, SS_B_LHP_H) = 0 Then
                Print " .000";
            Else
                If batRating(P9, I, SS_B_LHP_AB) = batRating(P9, I, SS_B_LHP_H) Then
                    Print "1.000";
                Else
                    Print Using " .###"; batRating(P9, I, SS_B_LHP_H) / batRating(P9, I, SS_B_LHP_AB);
                End If
            End If

            Locate , 53: Print Using "### ## ## ## ### ### "; batRating(P9, I, SS_B_RHP_AB); batRating(P9, I, 51); batRating(P9, I, 52); batRating(P9, I, 53); batRating(P9, I, 54); batRating(P9, I, 55);

            'AVG
            If batRating(P9, I, SS_B_RHP_H) = 0 Or batRating(P9, I, SS_B_RHP_H) = 0 Then
                Print " .000";
            Else
                If batRating(P9, I, SS_B_RHP_AB) = batRating(P9, I, SS_B_RHP_H) Then
                    Print "1.000";
                Else
                    Print " ."; Using "###"; batRating(P9, I, SS_B_RHP_H) / batRating(P9, I, SS_B_RHP_AB);
                End If
            End If

            If I <> 22 Then Print

        End If

    Next I

End Sub

'----------------------------------------
'           OPTIONS Subroutine
'----------------------------------------
'This subroutine gives you a small menu
'for changing some of the game options
'(such as pbp delay, coaching mode) in
'the middle of game play.
Sub OPTIONS ()

    Shared playMode$()

    If useVGA = 1 Then
        Screen menu&
    Else
        Screen , , 0, 0
    End If

    Call INFO

    Do

        Color , 0
        Locate 3, 1
        Color 14: Print "(1) ";
        Color 15: Print playMode$(playerOpt)
        Color 14: Print "(2) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY: #.# "; pbpDelay!
        Print
        Color 14: Print "<ENTER> ";
        Color 15: Print "RETURN TO GAME PLAY"

        Do
            Color 7, 0
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        Select Case I
            Case 1
                playerOpt = playerOpt + 1
                If playerOpt > 3 Then playerOpt = 0
            Case 2
                Locate 4, 23: Input pbpDelay!
        End Select

        ' Determine computer team
        Select Case playerOpt

            Case 1:
                playerMode = 1
                compTeam = 0

            Case 2:
                playerMode = 1
                compTeam = 1

            Case 3:
                playerMode = 2

        End Select

    Loop Until I$ = Chr$(13)

    If useVGA = 1 Then
        Screen imageScreen&
    Else
        Screen , , 0, 0
        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
    End If

End Sub



'----------------------------------------
'          LOADSTATS Subroutine
'----------------------------------------
'This subroutine loads in stats for the
'each team
Sub LOADSTATS ()

    For I = 0 To 1

        Color 15, 0
        Cls

        statFile$ = nameStatFile$(gameTeams$(I))

        If _FileExists(diskPaths$(1) + statFile$) Then

            Open diskPaths$(1) + statFile$ For Input As #1

            Input #1, teamName$
            Input #1, teamWins
            Input #1, teamLosses

            For I1 = 0 To 22:
                Input #1, batName$
                Input #1, batStat
            Next I1

            For I1 = 0 To 21:
                Input #1, pitName$
                Input #1, pitStat
            Next I1

            For I1 = 0 To 22:
                For I2 = 0 To 21:
                    Input #1, leagBatStats(I, I1, I2)
                Next
            Next

            For I1 = 0 To 21:
                For I2 = 0 To 41:
                    Input #1, leagPitStats(I, I1, I2)
                Next
            Next

            Close #1

        End If

    Next I

End Sub


'----------------------------------------
'           EVENTS Subroutine
'----------------------------------------
'This subroutine works with another to
'look for any events that meet the limits
'for "special". If found, it will create
'a corresponding "special" box score
Sub EVENTS (specialCount, specialStat)

    Shared eventSettings()

    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1
        For X = 1 To 13
            Input #1, eventSettings(X)
        Next
        Close 1

        checkSpecial% = 0

        If INNING% > eventSettings(1) Or gameScore(1, 0) < eventSettings(2) Or gameScore(0, 0) < eventSettings(2) Or gameScore(1, 0) > eventSettings(3) Or gameScore(0, 0) > eventSettings(3) Or gameScore(1, 1) > eventSettings(5) Or gameScore(0, 1) > eventSettings(5) Or gameScore(1, 1) < eventSettings(4) Or gameScore(0, 1) < eventSettings(4) Then
            checkSpecial% = 1
        Else

            For I = 0 To 1

                For I1 = 0 To 22

                    If gameBatStats(I, I1, 1) > eventSettings(6) Or gameBatStats(I, I1, 2) > eventSettings(7) Or gameBatStats(I, I1, 3) > eventSettings(8) Or gameBatStats(I, I1, 4) > eventSettings(9) Or gameBatStats(I, I1, 5) > eventSettings(10) Or gameBatStats(I, I1, 6) > eventSettings(11) Or gameBatStats(I, I1, 9) > eventSettings(12) Then
                        checkSpecial% = 1
                        Exit For
                    End If

                    If checkSpecial% = 1 Then Exit For

                Next

            Next

            If checkSpecial% <> 1 Then

                For I = 0 To 1

                    For I1 = 0 To 21

                        If gamePitStats(I, I1, 5) > eventSettings(13) Then
                            checkSpecial% = 1
                            Exit For
                        End If

                        If checkSpecial% = 1 Then Exit For

                    Next

                Next

            End If

        End If

    End If 'Done checking EVENTS.BOX file

    If checkSpecial% = 1 Then
        Call FoundSpecial(specialCount, specialStat)
    End If

End Sub


'----------------------------------------
'        FoundSpecial Subroutine
'----------------------------------------
'This subroutine simply counts how many
'special boxscore files already exist, so
'that it an assign a unique number to the
'boxscore about to be created.
Sub FoundSpecial (specialCount, specialStat)

    'Get count of SPC files to determine
    'number to assign next one
    Spec$ = diskPaths$(1) + "*.BSS"
    'Spec$ = diskPaths$(1) + "*.SPC"
    specialCount = FileCount%(Spec$)
    specialCount = specialCount + 1
    specialStat = 1

End Sub


'----------------------------------------
'          CheckRecords routine
'----------------------------------------
'This subroutine checks all stats to see
'if any records were broken. Any that have
'been reached will be assigned to arrays
'for later updating the files.
Sub CheckRecords ()

    Shared DB, HR, TR, SB
    Shared allEvents
    Shared doublesPlayer$(), eventDesc$(), homeRuns$()
    Shared stolenBases$(), triplesPlayer$()

    For I = 0 To 1:
        For I1 = 0 To 21:
            If gamePitStats(I, I1, 1) < 0 Then
                gamePitStats(I, I1, 1) = 0
            End If
        Next I1
    Next I

    For I = 0 To 1:

        For I1 = 0 To 21
            If pitchRating(I, I1, SS_P_Avail) = 1 Then
                If gamePitStats(I, I1, 0) >= 18 And gamePitStats(I, I1, 3) <= 3 Then
                    gamePitStats(I, I1, 21) = 1
                End If
            End If
        Next I1

    Next I

    eventNbr = 0

    For I = 0 To 1:

        For I1 = 0 To 22

            If gameBatStats(I, I1, 11) <> 0 Then

                If gameBatStats(I, I1, 11) = 1 Then
                    eventDesc$(eventNbr) = batters$(I, I1)
                Else
                    eventDesc$(eventNbr) = batters$(I, I1) + Str$(gameBatStats(I, I1, 11))
                End If

                eventDesc$(eventNbr) = eventDesc$(eventNbr) + " (" + Right$(Str$(leagBatStats(I, I1, 11)), (Len(Str$(leagBatStats(I, I1, 11))) - 1)) + ")"

                eventNbr = eventNbr + 1

            End If

            '        ***CHECKING DOUBLES***
            If gameBatStats(I, I1, 4) <> 0 Then
                If gameBatStats(I, I1, 4) = 1 Then
                    doublesPlayer$(DB) = batters$(I, I1)
                Else
                    doublesPlayer$(DB) = batters$(I, I1) + Str$(gameBatStats(I, I1, 4))
                End If

                doublesPlayer$(DB) = doublesPlayer$(DB) + " (" + Right$(Str$(leagBatStats(I, I1, 4)), (Len(Str$(leagBatStats(I, I1, 4))) - 1)) + ")"
                DB = DB + 1

            End If

            '        ***CHECKING TRIPLES***
            If gameBatStats(I, I1, 5) <> 0 Then
                If gameBatStats(I, I1, 5) = 1 Then
                    triplesPlayer$(TR) = batters$(I, I1)
                Else
                    triplesPlayer$(TR) = batters$(I, I1) + Str$(gameBatStats(I, I1, 5))
                End If

                triplesPlayer$(TR) = triplesPlayer$(TR) + " (" + Right$(Str$(leagBatStats(I, I1, 5)), (Len(Str$(leagBatStats(I, I1, 5))) - 1)) + ")"
                TR = TR + 1

            End If

            '        ***CHECKING HOME RUNS***
            If gameBatStats(I, I1, 6) <> 0 Then
                If gameBatStats(I, I1, 6) = 1 Then
                    homeRuns$(HR) = batters$(I, I1)
                Else
                    homeRuns$(HR) = batters$(I, I1) + Str$(gameBatStats(I, I1, 6))
                End If

                homeRuns$(HR) = homeRuns$(HR) + " (" + Right$(Str$(leagBatStats(I, I1, 6)), (Len(Str$(leagBatStats(I, I1, 6))) - 1)) + ")"
                HR = HR + 1

            End If

            '        ***CHECKING STOLEN BASES ***
            If gameBatStats(I, I1, 9) <> 0 Then

                If gameBatStats(I, I1, 9) = 1 Then
                    stolenBases$(SB) = batters$(I, I1) + " (" + Right$(Str$(leagBatStats(I, I1, 9)), (Len(Str$(leagBatStats(I, I1, 9))) - 1)) + ")"
                Else
                    stolenBases$(SB) = batters$(I, I1) + Str$(gameBatStats(I, I1, 9)) + " (" + Right$(Str$(leagBatStats(I, I1, 9)), (Len(Str$(leagBatStats(I, I1, 9))) - 1)) + ")"
                End If

                SB = SB + 1

            End If

        Next I1

    Next I

    For I = 0 To 1

        For I1 = 0 To 21

            If gamePitStats(I, I1, 34) <> 0 Then

                If gamePitStats(I, I1, 34) = 1 Then
                    eventDesc$(eventNbr) = pitchers$(I, I1)
                Else
                    eventDesc$(eventNbr) = pitchers$(I, I1) + Str$(gamePitStats(I, I1, 34))
                End If

                eventDesc$(eventNbr) = eventDesc$(eventNbr) + " (" + Right$(Str$(leagPitStats(I, I1, 34)), (Len(Str$(leagPitStats(I, I1, 34))) - 1)) + ")"
                eventNbr = eventNbr + 1

            End If

            '        ***CHECKING DOUBLES***
            If gamePitStats(I, I1, 27) <> 0 Then

                If gamePitStats(I, I1, 27) = 1 Then
                    doublesPlayer$(DB) = pitchers$(I, I1)
                Else
                    doublesPlayer$(DB) = pitchers$(I, I1) + Str$(gamePitStats(I, I1, 27))
                End If

                doublesPlayer$(DB) = doublesPlayer$(DB) + " (" + Right$(Str$(leagPitStats(I, I1, 27)), (Len(Str$(leagPitStats(I, I1, 27))) - 1)) + ")"
                DB = DB + 1

            End If

            '        ***CHECKING TRIPLES***
            If gamePitStats(I, I1, 28) <> 0 Then

                If gamePitStats(I, I1, 28) = 1 Then
                    triplesPlayer$(TR) = pitchers$(I, I1)
                Else
                    triplesPlayer$(TR) = pitchers$(I, I1) + Str$(gamePitStats(I, I1, 28))
                End If

                'Error #9 = index ???
                triplesPlayer$(TR) = triplesPlayer$(TR) + " (" + Right$(Str$(leagPitStats(I, I1, 28)), (Len(Str$(leagPitStats(I, I1, 28))) - 1)) + ")"
                TR = TR + 1

            End If

            '        ***CHECKING HOME RUNS***
            If gamePitStats(I, I1, 29) <> 0 Then

                If gamePitStats(I, I1, 29) = 1 Then
                    homeRuns$(HR) = pitchers$(I, I1)
                Else
                    homeRuns$(HR) = pitchers$(I, I1) + Str$(gamePitStats(I, I1, 29))
                End If

                homeRuns$(HR) = homeRuns$(HR) + " (" + Right$(Str$(leagPitStats(I, I1, 29)), (Len(Str$(leagPitStats(I, I1, 29))) - 1)) + ")"
                HR = HR + 1

            End If

            '        ***CHECKING STOLEN BASES ***
            If gamePitStats(I, I1, 32) <> 0 Then

                If gamePitStats(I, I1, 32) = 1 Then
                    stolenBases$(SB) = pitchers$(I, I1) + " (" + Right$(Str$(leagPitStats(I, I1, 32)), (Len(Str$(leagPitStats(I, I1, 32))) - 1)) + ")"
                Else
                    stolenBases$(SB) = pitchers$(I, I1) + Str$(gamePitStats(I, I1, 32)) + " (" + Right$(Str$(leagPitStats(I, I1, 32)), (Len(Str$(leagPitStats(I, I1, 32))) - 1)) + ")"
                End If

                SB = SB + 1

            End If

        Next I1
    Next I

    gameTime$ = "2:"

    MIN = Int(Rnd(1) * 60)

    If MIN < 10 Then
        MIN$ = "0" + Right$(Str$(MIN), 1)
        gameTime$ = gameTime$ + MIN$
    Else
        gameTime$ = gameTime$ + Right$(Str$(MIN), 2)
    End If

    For I = 0 To 1:
        For I1 = 0 To 21:
            If pitchRating(I, I1, SS_P_Avail) >= 98 Then
                pitchRating(I, I1, SS_P_Avail) = 0
            End If
        Next I1
    Next I

    For I = 0 To 1:

        For I1 = 0 To 22:
            If batRating(I, I1, SS_B_Avail) >= 98 Then
                batRating(I, I1, SS_B_Avail) = 0
            End If
            If batRating(I, I1, SS_B_Avail) <> 0 Then
                gameBatStats(I, I1, 14) = 1
            End If
        Next I1

    Next I

    Cls
    Print "PLEASE WAIT..."

    For I = 0 To 1

        M = 23
        L = 22

        Do

            I6 = 0
            I7 = 0

            Do

                H1 = 0
                H2 = 0
                H3 = 0

                If X0%(I, 2, I6) > X0%(I, 2, I6 + 1) Then

                    H1 = X0%(I, 2, I6)
                    H2 = X0%(I, 1, I6)
                    H3 = X0%(I, 0, I6)
                    X0%(I, 0, I6) = X0%(I, 0, I6 + 1)
                    X0%(I, 1, I6) = X0%(I, 1, I6 + 1)
                    X0%(I, 2, I6) = X0%(I, 2, I6 + 1)
                    X0%(I, 2, I6 + 1) = H1
                    X0%(I, 1, I6 + 1) = H2
                    X0%(I, 0, I6 + 1) = H3
                    I7 = I6

                End If

                I6 = I6 + 1

            Loop While I6 <= L

            If I7 <> 0 Then
                L = I7 - 1
            End If

        Loop While I7 <> 0

    Next I

End Sub


'----------------------------------------
'         InningPrintBox routine
'----------------------------------------
' ...explanation...
Sub InningPrintBox (I2)

    If I2 < 4 Then
        Locate , ((2 * I2) + 18)
    End If
    If I2 > 3 And I2 < 7 Then
        Locate , ((2 * I2) + 19)
    End If
    If I2 > 6 And I2 < 10 Then
        Locate , ((2 * I2) + 20)
    End If

    If I2 > 9 And I2 < 13 Then
        Locate , ((3 * (I2 - 9)) + 18)
    End If
    If I2 > 12 And I2 < 16 Then
        Locate , ((3 * (I2 - 9)) + 19)
    End If
    If I2 > 15 And I2 < 19 Then
        Locate , ((3 * (I2 - 9)) + 20)
    End If

    If I2 > 18 And I2 < 22 Then
        Locate , ((3 * (I2 - 18)) + 18)
    End If
    If I2 > 21 And I2 < 25 Then
        Locate , ((3 * (I2 - 18)) + 19)
    End If
    If I2 > 24 And I2 < 28 Then
        Locate , ((3 * (I2 - 18)) + 20)
    End If

End Sub

'----------------------------------------
'        InningPrintFile routine
'----------------------------------------
' ...explanation...
Sub InningPrintFile (I2)

    If I2 < 4 Then
        Print #2, Tab((2 * I2) + 15);
    End If
    If I2 > 3 And I2 < 7 Then
        Print #2, Tab((2 * I2) + 16);
    End If
    If I2 > 6 And I2 < 10 Then
        Print #2, Tab((2 * I2) + 17);
    End If

    If I2 > 9 And I2 < 13 Then
        Print #2, Tab((3 * I2) + 8);
    End If
    If I2 > 12 And I2 < 16 Then
        Print #2, Tab((3 * I2) + 9);
    End If
    If I2 > 15 And I2 < 19 Then
        Print #2, Tab((3 * I2) + 10);
    End If

    If I2 > 18 And I2 < 22 Then
        Print #2, Tab((3 * I2) + 11);
    End If
    If I2 > 21 And I2 < 25 Then
        Print #2, Tab((3 * I2) + 12);
    End If
    If I2 > 24 And I2 < 28 Then
        Print #2, Tab((3 * I2) + 13);
    End If

End Sub


'----------------------------------------
'          LDRNRCTDN Subroutine
'----------------------------------------
'Lead Runner Countdown? Caught Down?
Sub LDRNRCTDN ()

    If baseRunners = 7 Or baseRunners = 6 Or baseRunners = 5 Or baseRunners = 3 Then
        A5%(3) = 11
        For I = 0 To 2:
            A5%(I) = 1
        Next
    End If

    If baseRunners = 2 Or baseRunners = 4 Then
        A5%(0) = 1
        A5%(1) = 1
        A5%(2) = 11
    End If

    If baseRunners = 1 Then
        A5%(1) = 11
        A5%(0) = 1
    End If

    If baseRunners = 0 Then
        A5%(0) = 11
    End If

    If PLB = 0 Then
        Call PBP(player$(0) + " bunts it hard!")
        Call PBP(player$(4) + " makes an easy play")
    End If

    PLB = 0
    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = nbrOuts(1) + 1

    Call ADDSTTS(P, D)
    Call CREDITASSIST(currFielder, D)
    Call ADVANCEMENT(P, D, S2%, errorOnPlay%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

End Sub


'----------------------------------------
'          POSTGAME Subroutine
'----------------------------------------
'This subroutine processes everything that
'is necessary after a game is complete.
'It checks for records / special events,
'handles a prompt for stats, and saving
'of stats.
Sub POSTGAME ()

    Call CheckRecords
    Call EVENTS(specialCount, specialStat)
    gameOver = 1

    Do
        I3 = 0

        If gameScore(1, 0) - gameScore(0, 0) > 0 Then
            I3 = 1
        End If
        If pitcherOfRecord(I3) <> currPitcher(I3) And S1%(I3) = -1 And gamePitStats(I3, currPitcher(I3), 0) >= 9 And gamePitStats(I3, currPitcher(I3), 0) <= 12 And gamePitcher(I3) > 0 And gamePitStats(I3, currPitcher(I3), 3) < 3 Then
            S1%(I3) = currPitcher(I3)
        End If
        If pitcherOfRecord(I3) = S1%(I3) Then
            S1%(I3) = -1
        End If

        Color 15, 0
        Cls

        Color 14: Print "(0) ";
        Color 15: Print "STATS TO SCREEN"
        Print

        Color 14: Print "(1) ";
        Color 15: Print "STATS TO PRINTER"
        Print

        Color 14: Print "(2) ";
        Color 15: Print "STATS TO TEXT FILE"

        If autoPlay = 1 And playerMode = 2 Then
            X = 2
        Else
            Do
                I$ = GetKeyPress$
                X = Val(I$)
            Loop Until X >= 0 And X <= 2
        End If

        Call BOXOPTIONS(X)

        'Reset option for next call
        If X <> 0 Then X = 0

        'Prompt to display stats again
        Color 15, 0
        Cls

        If Not (autoPlay = 1 And playerMode = 2) Then

            Do
                'Using Home Team's color for the box
                Call TEAMBOX(1, 1)
                U$ = "DISPLAY STATS AGAIN? (Y/N)"
                Locate 7, (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

    'If we had special events
    If specialStat = 1 Then

        boxName$ = "BOXSCORE" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".BSS"

        'Append special events
        Call BOX2SPC(boxName$)

        Call BOX2FILE(2, boxName$)

    End If

    Call SAVESTATS

    gameOver = 0
    specialStat = 0

End Sub


'----------------------------------------
'          SAVESTATS Subroutine
'----------------------------------------
'This subroutine prompts the user in ORDER
'to determine which teams to save satts For
'(autoplay will always save both teams).
'It also updates the GAME82 progress file,
'since at this point only can the game be
'considered fully complete.
Sub SAVESTATS ()

    Shared SX%()

    Shared scheduleFile$

    Color 15, 0
    Cls

    If autoPlay <> 1 Then

        Call TEAMBOX(1, 1)

        Do
            U$ = "SAVE STATS? (Y/N)"
            Locate 7, (80 - Len(U$)) / 2
            Print U$
            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If UCase$(I$) = "N" Then
            X1 = 3
        Else
            Cls
            Color 12: Print Tab(33); "COMPILE STATS"
            Print
            Color 14: Print "(0) ";
            Color 15: Print "BOTH TEAMS"
            Print
            Color 14: Print "(1) ";
            Color teamRat_GAME(0, 11), teamRat_GAME(0, 12): Print gameTeams$(0); " ONLY"
            Color , 0
            Print
            Color 14: Print "(2) ";
            Color teamRat_GAME(1, 11), teamRat_GAME(1, 12): Print gameTeams$(1); " ONLY"
            Color 15, 0
            Print

            Do
                J$ = GetKeyPress$
                X1 = Val(J$)
            Loop Until X1 >= 0 And X1 <= 2
        End If

    Else
        BG% = 0
        BN% = 1
    End If

    If UCase$(I$) = "Y" Or autoPlay = 1 Then

        For PX = 0 To 1
            For I = 0 To 21
                If pitchRating(PX, I, SS_P_Avail) = 1 Then
                    SX%(PX) = I
                    Exit For
                End If
            Next I
        Next PX

        If _FileExists("GAME82") Then

            Call UpdateSchedule

            If schedGame(1) < schedGame(0) + schedGame(2) Then
                Kill "GAME82"
                endAllGames = 1
            Else

                Open "GAME82" For Output As #2

                Print #2, scheduleFile$

                For I = 0 To 2:
                    Print #2, schedGame(I)
                Next

                Close #2

            End If

        End If

        Cls
        Q = 0
        teamWins = 0
        teamLosses = 0

        Do
            C% = 0

            If autoPlay = 1 Then
                C% = 0
            Else
                If X1 = 0 Or X1 = 1 And Q = 0 Or X1 = 2 And Q = 1 Then
                    Cls
                Else
                    If X1 = 2 And Q = 0 Or X1 = 1 And Q = 1 Then
                        C% = 1
                    End If
                End If
            End If

            If C% <> 1 Then

                Call COMPILESTATFILES(Q)

            End If

            Q = Q + 1

        Loop Until Q = 2

    End If

End Sub


'----------------------------------------
'      COMPILESTATFILES Subroutine
'----------------------------------------
'This subroutine updates the various stat
'files for the corresponding team based on
'the stats generated for a completed game.
Sub COMPILESTATFILES (teamIdx)

    Shared oppName$(), losePitcher$(), locIndicator$()
    Shared winPitcher$()

    Shared gameDaysRest(), eventSettings()
    Shared teamScore(), nbrInnings()
    Shared SX%(), oppScore()

    Shared statB0(), statP0(), statS0()
    Shared statT0!(), statT1!()

    statFile$ = nameStatFile$(gameTeams$(teamIdx))

    If _FileExists(diskPaths$(1) + statFile$ + "D") Then

        Open diskPaths$(1) + statFile$ + "D" For Input As #1

        For I1 = 0 To 22
            Input #1, injuryStatus(teamIdx, I1)
        Next

        For I1 = 0 To 21
            Input #1, gameDaysRest(teamIdx, I1)
        Next

        Close 1

    End If

    If useInj = 1 Or useRest = 1 Then

        If useInj = 1 Then

            For I1 = 0 To 22
                If injuryStatus(teamIdx, I1) > 0 Then
                    injuryStatus(teamIdx, I1) = injuryStatus(teamIdx, I1) - 1
                End If
                If batRating(teamIdx, I1, SS_B_Injured) > 0 Then
                    injuryStatus(teamIdx, I1) = batRating(teamIdx, I1, SS_B_Injured)
                End If
            Next

        End If

        If useRest = 1 Then

            For I1 = 0 To 21

                If gameDaysRest(teamIdx, I1) > 0 And gameDaysRest(teamIdx, I1) < 90 Then
                    gameDaysRest(teamIdx, I1) = gameDaysRest(teamIdx, I1) - 1
                End If

                Select Case gamePitStats(teamIdx, I1, 0)

                    Case 0
                        If (gameDaysRest(teamIdx, I1) = 0 Or gameDaysRest(teamIdx, I1) = 1 Or gameDaysRest(teamIdx, I1) = 91 Or gameDaysRest(teamIdx, I1) = 92) Then
                            gameDaysRest(teamIdx, I1) = 0
                        End If

                    Case 1 To 3
                        Select Case gameDaysRest(teamIdx, I1)

                            Case 0
                                gameDaysRest(teamIdx, I1) = 91

                            Case 91
                                gameDaysRest(teamIdx, I1) = 911

                            Case 911, 92
                                gameDaysRest(teamIdx, I1) = 1

                            Case Else
                                gameDaysRest(teamIdx, I1) = gameDaysRest(teamIdx, I1) + 1

                        End Select

                    Case 4 To 6
                        Select Case gameDaysRest(teamIdx, I1)

                            Case 0
                                gameDaysRest(teamIdx, I1) = 92

                            Case 91, 911, 92
                                gameDaysRest(teamIdx, I1) = 1

                            Case Else
                                gameDaysRest(teamIdx, I1) = gameDaysRest(teamIdx, I1) + 1

                        End Select

                    Case Is > 6
                        gameDaysRest(teamIdx, I1) = Int(gamePitStats(teamIdx, I1, 0) / 3) + 1 - 2

                        If teamMgrProfileVal(teamIdx, 1) <= 0 Then
                            gameDaysRest(teamIdx, I1) = 3
                        Else
                            If gameDaysRest(teamIdx, I1) > teamMgrProfileVal(teamIdx, 1) - 1 Then
                                gameDaysRest(teamIdx, I1) = teamMgrProfileVal(teamIdx, 1) - 1
                            End If
                            If gameDaysRest(teamIdx, I1) < 0 Then
                                gameDaysRest(teamIdx, I1) = 0
                            End If
                        End If

                End Select

            Next

        End If

        Open diskPaths$(1) + statFile$ + "D" For Output As #1
        For I1 = 0 To 22
            Print #1, injuryStatus(teamIdx, I1)
        Next
        For I1 = 0 To 21
            Print #1, gameDaysRest(teamIdx, I1)
        Next
        Close 1

    End If

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, gameTeams$(teamIdx)
        Input #1, teamWins
        Input #1, teamLosses

        For I = 0 To 22:
            Input #1, batters$(teamIdx, I)
            Input #1, gameB(teamIdx, I)
        Next I

        For I = 0 To 21:
            Input #1, pitchers$(teamIdx, I)
            Input #1, gameP(teamIdx, I)
        Next I

        For I = 0 To 22:
            For I1 = 0 To 21:
                Input #1, statB0(I, I1)
            Next I1
        Next I

        For I = 0 To 21:
            For I1 = 0 To 41:
                Input #1, statP0(I, I1)
            Next I1
        Next I

        Input #1, gameD0(teamIdx)
        Input #1, gameD1(teamIdx)

        For I = 0 To 21:
            Input #1, statT0!(I)
        Next I

        For I = 0 To 22:
            Input #1, statT1!(I)
        Next I

        Close #1

    End If

    For I = 0 To 22

        gameB(teamIdx, I) = batRating(teamIdx, I, SS_B_AB)

        For I1 = 0 To 11
            statB0(I, I1) = statB0(I, I1) + gameBatStats(teamIdx, I, I1)
            statT0!(I1) = statT0!(I1) + gameBatStats(teamIdx, I, I1)
        Next I1

        If gameBatStats(teamIdx, I, 0) > 0 Then

            If gameBatStats(teamIdx, I, 1) = 0 Then
                statB0(I, 12) = 0
            Else
                statB0(I, 12) = statB0(I, 12) + 1
            End If

        End If

        If gameBatStats(teamIdx, I, 14) > 0 Then
            statB0(I, 14) = statB0(I, 14) + 1
        End If

        For I1 = 15 To 21
            statB0(I, I1) = statB0(I, I1) + gameBatStats(teamIdx, I, I1)
            statT0!(I1) = statT0!(I1) + gameBatStats(teamIdx, I, I1)
        Next I1

        If statB0(I, 12) >= statB0(I, 13) Then
            statB0(I, 13) = statB0(I, 12)
        End If

    Next I

    For I = 0 To 21

        For I1 = 23 To 34
            statP0(I, I1) = statP0(I, I1) + gamePitStats(teamIdx, I, I1)
            statT0!(I1 - 23) = statT0!(I1 - 23) + gamePitStats(teamIdx, I, I1)
        Next I1

        If gamePitStats(teamIdx, I, 23) > 0 Then
            If gamePitStats(teamIdx, I, 24) = 0 Then
                statP0(I, 35) = 0
            Else
                statP0(I, 35) = statP0(I, 35) + 1
            End If
        End If

        For I1 = 37 To 41
            statP0(I, I1) = statP0(I, I1) + gamePitStats(teamIdx, I, I1)
            statT0!(I1 - 22) = statT0!(I1 - 22) + gamePitStats(teamIdx, I, I1)
        Next I1

        If statP0(I, 35) >= statP0(I, 36) Then
            statP0(I, 36) = statP0(I, 35)
        End If

    Next I

    statT0!(12) = statT0!(12) + L0%(teamIdx)
    INNING% = 0

    For I = 0 To 21
        INNING% = INNING% + gamePitStats(teamIdx, I, 0)
        gameP(teamIdx, I) = pitchRating(teamIdx, I, SS_P_IP)

        For I1 = 0 To 5
            statP0(I, I1) = statP0(I, I1) + gamePitStats(teamIdx, I, I1)
            statT1!(I1) = statT1!(I1) + gamePitStats(teamIdx, I, I1)
        Next I1

        For I1 = 13 To 22:
            statP0(I, I1) = statP0(I, I1) + gamePitStats(teamIdx, I, I1)
            statT1!(I1) = statT1!(I1) + gamePitStats(teamIdx, I, I1)
        Next I1

    Next I

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) > 0 Then
        'Team Win
        statP0(pitcherOfRecord(teamIdx), 6) = statP0(pitcherOfRecord(teamIdx), 6) + 1
    End If

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) < 0 Then
        'Team Loss
        statP0(pitcherOfRecord(teamIdx), 7) = statP0(pitcherOfRecord(teamIdx), 7) + 1
    End If

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) > 0 And S1%(teamIdx) >= 0 Then
        'Team Win + S1% ??
        statP0(S1%(teamIdx), 12) = statP0(S1%(teamIdx), 12) + 1
        statT1!(12) = statT1!(12) + 1
    End If

    For I = 0 To 21

        If pitchRating(teamIdx, I, SS_P_Avail) <> 0 Then
            statP0(I, 8) = statP0(I, 8) + 1
            statT1!(8) = statT1!(8) + 1
        End If

        If INNING% = gamePitStats(teamIdx, I, 0) Then
            statP0(I, 10) = statP0(I, 10) + 1
            statT1!(10) = statT1!(10) + 1
        End If

        If INNING% = gamePitStats(teamIdx, I, 0) And gameScore(1 - teamIdx, 0) = 0 Then
            statP0(I, 11) = statP0(I, 11) + 1
            statT1!(11) = statT1!(11) + 1
        End If

    Next I

    statP0(SX%(teamIdx), 9) = statP0(SX%(teamIdx), 9) + 1
    statT1!(9) = statT1!(9) + 1

    gameD0(teamIdx) = gameD0(teamIdx) + D3%(teamIdx)
    gameD1(teamIdx) = gameD1(teamIdx) + D3%(1 - teamIdx)

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) > 0 Then
        teamWins = teamWins + 1
    End If
    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) < 0 Then
        teamLosses = teamLosses + 1
    End If

    Color 15, 0

    Print ">>>>>"
    Print "THE STATS ARE BEING SAVED UNDER": Print "THE FOLLOWING NAME ::: ";
    Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12): Print statFile$

    Color 15, 0

    Open diskPaths$(1) + statFile$ For Output As #1

    Print #1, gameTeams$(teamIdx)
    Print #1, teamWins
    Print #1, teamLosses

    For I = 0 To 22:
        Print #1, batters$(teamIdx, I)
        Print #1, gameB(teamIdx, I)
    Next I

    For I = 0 To 21:
        Print #1, pitchers$(teamIdx, I)
        Print #1, gameP(teamIdx, I)
    Next I

    For I = 0 To 22:
        For I1 = 0 To 21:
            Print #1, statB0(I, I1)
        Next I1
    Next I

    For I = 0 To 21:
        For I1 = 0 To 41:
            Print #1, statP0(I, I1)
        Next I1
    Next I

    Print #1, gameD0(teamIdx)
    Print #1, gameD1(teamIdx)

    For I = 0 To 21:
        Print #1, statT0!(I)
    Next I

    For I = 0 To 22:
        Print #1, statT1!(I)
    Next I

    Close #1

    Print "<<<<<"
    Print
    Print

    If _FileExists(diskPaths$(1) + statFile$ + "R") Then

        Open diskPaths$(1) + statFile$ + "R" For Input As #1

        For I = 0 To 22
            For I1 = 0 To 6
                Input #1, statS0(I, I1)
            Next
        Next

        Close 1

    End If

    For I = 0 To 22
        For I1 = 0 To 6
            statS0(I, I1) = statS0(I, I1) + CSS(teamIdx, I, I1)
        Next
    Next

    Open diskPaths$(1) + statFile$ + "R" For Output As #1

    For I = 0 To 22
        For I1 = 0 To 6
            Print #1, statS0(I, I1)
        Next
    Next

    Close 1

    numberGames = 0

    If _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Call ReadScheduleStats(statFile$, numberGames)

    Else
        'Default stat values
        For I = 1 To MAX_SCHED_STATS
            teamScore(I) = 999
            oppName$(I) = "XXXXXXX"
            oppScore(I) = 999
            locIndicator$(I) = "X"
            winPitcher$(I) = "LYNN"
            losePitcher$(I) = "SUMLER"
            nbrInnings(I) = 999
        Next I

    End If

    If numberGames < MAX_SCHED_STATS Then
        numberGames = numberGames + 1
    End If

    If teamIdx = 0 Then
        locIndicator$(numberGames) = "A"
    Else
        locIndicator$(numberGames) = "H"
    End If

    teamScore(numberGames) = gameScore(teamIdx, 0)
    oppName$(numberGames) = gameTeams$(1 - teamIdx)
    oppScore(numberGames) = gameScore(1 - teamIdx, 0)
    winPitcher$(numberGames) = gameW$
    losePitcher$(numberGames) = gameL$
    nbrInnings(numberGames) = LYN%

    Call SaveScheduleStats(statFile$, numberGames)

    numberGames = 0

    Erase gameB, gameP
    Erase statB0, statP0
    Erase statS0, statT0!, statT1!

    teamWins = 0
    teamLosses = 0

End Sub


'----------------------------------------
'         BOXOPTIONS Subroutine
'----------------------------------------
'This subroutine takes appropriate action
'for the output of the box file, which
'depends on the choice given to it.

' PARAMETER outputChoice
'    represents menu choice
'       0 = Display on screen
'       1 = Send to printer
'       2 = Save to file

' The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOXOPTIONS (outputChoice)

    Shared scheduleFile$

    Select Case outputChoice

        Case 0:
            Call BOX2SCREEN

        Case 1, 2
            If outputChoice = 1 Then
                'Printer = temporary file
                boxName$ = "TEMPBOX"
            Else
                'File = prompt | auto-create
                If autoPlay <> 1 Then
                    Cls
                    Print "ENTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";
                    Input boxName$
                    boxName$ = boxName$ + ".BSS"
                Else
                    'ScheduleName_GameXXX.BS
                    baseFile$ = GetFileBaseName$(scheduleFile$)
                    boxName$ = baseFile$ + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
                End If

            End If

            'Includes a special option to send to printer when done
            Call BOX2FILE(outputChoice, boxName$)

    End Select

End Sub


'----------------------------------------
'          BOX2FILE Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'an output file.

' PARAMETER printChoice
'    choice to print file vs. saving it

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOX2FILE (printChoice, boxName$)

    Shared DB, HR, TR, SB
    Shared eventNbr

    Shared actualAttendance&

    Shared eventSettings()

    Dim BK$
    BK$ = "## # # ## # #  # # #  #  # # ## ##"

    Open diskPaths$(1) + boxName$ For Append As #2

    Print #2, Tab(17);

    For IG = 1 To INNING%

        If IG < 10 Then
            Print #2, Using " #"; IG;
        Else
            Print #2, Using " ##"; IG;
        End If

        If (IG Mod 3 = 0) Then Print #2, " ";

    Next IG

    Print #2, "   H  R E"

    If INNING% < 9 Then

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 1 To INNING%
                Call InningPrintFile(I2)
                If I = 0 Then
                    Print #2, Using "##"; inningScore(I, I2);
                End If
                If I = 1 And gameScore(1, 0) > gameScore(0, 0) And I2 = INNING% And inningScore(1, INNING%) = 0 Then
                    Print #2, " X";
                End If
                If I = 1 And gameScore(1, 0) < gameScore(0, 0) And I2 = INNING% Or I = 1 And gameScore(1, 0) > gameScore(0, 0) And I2 = INNING% And inningScore(1, INNING%) > 0 Then
                    Print #2, Using "##"; inningScore(I, I2);
                End If
                If I = 1 And I2 < INNING% Then
                    Print #2, Using "##"; inningScore(I, I2);
                End If
            Next I2

            Print #2, " -";
            Print #2, Using " ##"; gameScore(I, 0);
            Print #2, Using " ##"; gameScore(I, 1);
            Print #2, Using " #"; gameScore(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 0 Or INNING% = 9 Then

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 1 To 9
                Call InningPrintFile(I2)
                If I = 0 Then
                    Print #2, Using "##"; inningScore(I, I2);
                End If
                If I = 1 And gameScore(1, 0) > gameScore(0, 0) And I2 = 9 And inningScore(1, 9) = 0 Then
                    Print #2, " X";
                End If
                If I = 1 And gameScore(1, 0) < gameScore(0, 0) And I2 = 9 Or I = 1 And gameScore(1, 0) > gameScore(0, 0) And I2 = 9 And inningScore(1, 9) > 0 Then
                    Print #2, Using "##"; inningScore(I, I2);
                End If
                If I = 1 And I2 < 9 Then
                    Print #2, Using "##"; inningScore(I, I2);
                End If
            Next I2

            If INNING% = 10 Then
                Print #2, "   ";
            End If
            Print #2, " -";
            Print #2, Using " ##"; gameScore(I, 0);
            Print #2, Using " ##"; gameScore(I, 1);
            Print #2, Using " #"; gameScore(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 1 Or INNING% > 10 And INNING% < 19 Or INNING% = 19 And P = 0 Then

        For I = 0 To 1

            Print #2, gameTeams$(I);

            'Print innings 1 - 9
            For I2 = 1 To 9
                Call InningPrintFile(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            'If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1

            'Prints innings 10 to 19
            For I2 = 10 To INNING%
                Call InningPrintFile(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            If (INNING% Mod 3 = 0) Then
                Print #2, " "
            End If

            Print #2, " -";
            Print #2, Using " ##"; gameScore(I, 0);
            Print #2, Using " ##"; gameScore(I, 1);
            Print #2, Using " #"; gameScore(I, 2)

        Next I

    End If

    If INNING% = 19 And P = 1 Or INNING% > 19 And INNING% <= 28 Then

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 1 To 9
                Call InningPrintFile(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            'If INNING% = 28 Or INNING% < 28 And P = 0 Then INNING% = INNING% - 1

            For I2 = 10 To 18
                Call InningPrintFile(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            Print #2,

        Next I

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 19 To INNING%
                Call InningPrintFile(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            Print #2, " -";
            Print #2, Using " ##"; gameScore(I, 0);
            Print #2, Using " ##"; gameScore(I, 1);
            Print #2, Using " #"; gameScore(I, 2)

        Next I

        Print #2,

    End If

    Print #2,

    For I = 0 To 1

        Print #2, gameTeams$(I); Tab(18); "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"

        For I3 = 0 To 22

            If X0%(I, 0, I3) <> -1 And X0%(I, 1, I3) <> 1 Then

                I1 = X0%(I, 0, I3)

                Print #2, batters$(I, I1); ", "; fieldPos$(X0%(I, 1, I3)); Tab(18);
                '                    AB             R              H              BI             2              3              HR             K              W              SB             CS              E               PO              A
                Print #2, Using BK$; gameBatStats(I, I1, 0); gameBatStats(I, I1, 2); gameBatStats(I, I1, 1); gameBatStats(I, I1, 3); gameBatStats(I, I1, 4); gameBatStats(I, I1, 5); gameBatStats(I, I1, 6); gameBatStats(I, I1, 8); gameBatStats(I, I1, 7); gameBatStats(I, I1, 9); gameBatStats(I, I1, 10); gameBatStats(I, I1, 11); gameBatStats(I, I1, 16); gameBatStats(I, I1, 17);

                Print #2, "   ";

                'BA
                If leagBatStats(I, I1, 0) = 0 Or leagBatStats(I, I1, 1) = 0 Then
                    Print #2, " .000   ";
                Else
                    If leagBatStats(I, I1, 0) = leagBatStats(I, I1, 1) Then
                        Print #2, "1.000   ";
                    Else
                        Print #2, " ."; Using "###   "; (leagBatStats(I, I1, 1) / leagBatStats(I, I1, 0) * 1000);
                    End If
                End If

                'HR / RBI
                Print #2, Using "##  ###"; leagBatStats(I, I1, 6); leagBatStats(I, I1, 3)

                For I2 = 0 To 17
                    T3%(I2) = T3%(I2) + gameBatStats(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21:
            For I2 = 0 To 13:
                T3%(I2) = T3%(I2) + gamePitStats(I, I1, I2 + 23)
            Next

            For I2 = 14 To 17:
                T3%(I2) = T3%(I2) + gamePitStats(I, I1, I2 + 22)
            Next
        Next

        For I1 = 1 To gamePitcher(I)
            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then

                    Print #2, pitchers$(I, I2); ", P";
                    Print #2, Tab(18);
                    Print #2, Using BK$; gamePitStats(I, I2, 23); gamePitStats(I, I2, 25); gamePitStats(I, I2, 24); gamePitStats(I, I2, 26); gamePitStats(I, I2, 27); gamePitStats(I, I2, 28); gamePitStats(I, I2, 29); gamePitStats(I, I2, 31); gamePitStats(I, I2, 30); gamePitStats(I, I2, 32); gamePitStats(I, I2, 33); gamePitStats(I, I2, 34); gamePitStats(I, I2, 38); gamePitStats(I, I2, 39);
                    Print #2, "   ";

                    If leagPitStats(I, I2, 24) = 0 Or leagPitStats(I, I2, 23) = 0 Then
                        Print #2, " .000   ";
                    Else
                        If leagPitStats(I, I2, 24) = leagPitStats(I, I2, 23) Then
                            Print #2, "1.000   ";
                        Else
                            Print #2, Using " .###   "; leagPitStats(I, I2, 24) / leagPitStats(I, I2, 23);
                        End If
                    End If

                    Print #2, Using "##  ###"; leagPitStats(I, I2, 29); leagPitStats(I, I2, 26)
                End If
            Next
        Next

        Print #2, "TOTALS"; Tab(17); T3%(0); Tab(22); T3%(1); Tab(27); T3%(4);
        Print #2, Tab(32); T3%(6); Tab(36); T3%(7); Tab(42);
        Print #2, T3%(10); Tab(46); T3%(16); Tab(50); T3%(17)
        Print #2, Tab(20); T3%(2); Tab(24); T3%(3); Tab(29);
        Print #2, T3%(5); Tab(34); T3%(8); Tab(39); T3%(9);
        Print #2, Tab(44); T3%(11)
        Print #2,: For I1 = 0 To 22: T3%(I1) = 0: Next I1

    Next I

    For I = 0 To 1

        Print #2,: Print #2, gameTeams$(I); Tab(23); "       IP   H  R  ER BB SO    ERA   W  L  S  SO"

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then

                    I4 = 1 - I: Print #2, pitchers$(I, I2); " ";

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, "(W,"; Right$(Str$(leagPitStats(I, I2, 6) + 1), (Len(Str$(leagPitStats(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(leagPitStats(I, I2, 7)), (Len(Str$(leagPitStats(I, I2, 7))) - 1)); ")";
                        gameW$ = pitchers$(I, I2)
                    End If

                    If S1%(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then Print #2, "(S,"; Right$(Str$(leagPitStats(I, I2, 12) + 1), (Len(Str$(leagPitStats(I, I2, 12) + 1)) - 1)); ")";

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print #2, "(L,"; Right$(Str$(leagPitStats(I, I2, 6) + 0), (Len(Str$(leagPitStats(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(leagPitStats(I, I2, 7) + 1), (Len(Str$(leagPitStats(I, I2, 7) + 1)) - 1)); ")";
                        gameL$ = pitchers$(I, I2)
                    End If

                    If gamePitStats(I, I2, 19) > 0 Then
                        Print #2, "(BS,";: Print #2, Using "##"; leagPitStats(I, I2, 19);: Print #2, ")";
                    End If
                    If gamePitStats(I, I2, 22) > 0 Then
                        Print #2, "(H,";: Print #2, Using "##"; leagPitStats(I, I2, 22);: Print #2, ")";
                    End If

                    Print #2, Tab(29);: Print #2, Using "##"; Int(gamePitStats(I, I2, 0) / 3);

                    If gamePitStats(I, I2, 0) - (Int(gamePitStats(I, I2, 0) / 3) * 3) > 0 Then
                        Print #2, ".";: Print #2, Using "#"; gamePitStats(I, I2, 0) - (Int(gamePitStats(I, I2, 0) / 3) * 3);
                    End If

                    Print #2, Tab(34);
                    Print #2, Using "## ##  ## ## ##"; gamePitStats(I, I2, 1); gamePitStats(I, I2, 2); gamePitStats(I, I2, 3); gamePitStats(I, I2, 4); gamePitStats(I, I2, 5);
                    Print #2, "   ";

                    If leagPitStats(I, I2, 3) > 0 And leagPitStats(I, I2, 0) = 0 Then
                        Print #2, "99.99 ";
                    Else
                        If leagPitStats(I, I2, 0) = 0 Then
                            Print #2, "--.-- ";
                        Else
                            If leagPitStats(I, I2, 3) = 0 Then
                                Print #2, " 0.00 ";
                            Else
                                Print #2, Using "##.## "; leagPitStats(I, I2, 3) / (leagPitStats(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, Using "## "; leagPitStats(I, I2, 6) + 1;
                    Else
                        Print #2, Using "## "; leagPitStats(I, I2, 6);
                    End If
                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print #2, Using "## "; leagPitStats(I, I2, 7) + 1;
                    Else
                        Print #2, Using "## "; leagPitStats(I, I2, 7);
                    End If
                    If S1%(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, Using "## "; leagPitStats(I, I2, 12) + 1;
                    Else
                        Print #2, Using "## "; leagPitStats(I, I2, 12);
                    End If

                    Print #2, Using "###"; leagPitStats(I, I2, 5)

                    For I4 = 0 To 5
                        T3%(I4) = T3%(I4) + gamePitStats(I, I2, I4)
                    Next I4
                End If

            Next I2

        Next I1

        Print #2, "TOTALS"; Tab(28);
        Print #2, Using "##"; Int(T3%(0) / 3);

        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then
            Print #2, ".";: Print #2, Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);
        End If

        Print #2, Tab(34);
        Print #2, Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)

        LYN% = CInt(T3%(0) / 3)

        For I1 = 0 To 5:
            T3%(I1) = 0
        Next I1

    Next I

    Print #2,: Print #2, " E-";

    If eventNbr = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (eventNbr - 1): Print #2, eventDesc$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "2B-";

    If DB = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (DB - 1): Print #2, doublesPlayer$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "3B-";
    If TR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (TR - 1): Print #2, triplesPlayer$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "HR-";

    If HR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (HR - 1): Print #2, homeRuns$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "SB-";

    If SB = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (SB - 1):
            Print #2, stolenBases$(X1); ",";
        Next
        Print #2,
    End If

    Print #2, "LOB-"; gameTeams$(0); Str$(L0%(0)); ","; gameTeams$(1); Str$(L0%(1))
    Print #2, "DP-"; gameTeams$(0); Str$(DP%(0)); ","; gameTeams$(1); Str$(DP%(1))
    Print #2, "UMPIRES: HP-"; gameUmpire$(1); "; 1B-"; gameUmpire$(2); "; 2B-"; gameUmpire$(3); "; 3B-"; gameUmpire$(4)
    Print #2, "TIME: "; gameTime$
    Print #2, "ATTENDANCE: ";: Print Using "###,###"; actualAttendance&
    Print #2,
    Print #2, "INJURIES-";

    If injCount = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To injCount
            Print #2, injPlayers$(X1); ",";
        Next
        Print #2,
    End If

    peject:
    Print #2, "EJECTIONS-";
    If ejectCnt = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To ejectCnt:
            Print #2, ejections$(X1); ",";
        Next
        Print #2,
    End If

    pweath:
    Print #2, "TEMPERATURE:"; gameTemp
    Print #2, "WIND: "; wind$
    Print #2, "SKY: "; skyCond$(skyType)
    Print #2, "PRECIPITATION: "; precip$(precipType)

    If printChoice = 1 Then Print #2, Chr$(12)

    Close 2

    If printChoice = 1 Then

        Open diskPaths$(1) + boxName$ For Input As #2

        Do
            Line Input #2, X$
            LPrint X$
        Loop While Left$(X$, 1) <> Chr$(12)

        Kill "TEMPBOX"

    End If

End Sub


'----------------------------------------
'         BOX2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'output to the end user display.
Sub BOX2SCREEN

    Shared DB, HR, TR, SB
    Shared doublesPlayer$(), eventDesc$(), homeRuns$()
    Shared stolenBases$(), triplesPlayer$()

    Cls

    'Color 14
    'If gameLoc = 2 Then
    '    Print "SITE: ";
    '    Color 15
    '    Print "NEUTRAL"
    'Else
    '    Print "SITE: ";
    '    Color 15
    '    Print gameStadium$(gameLoc)
    'End If

    'Color 14
    'Print "ATTENDANCE: ";
    'Color 15
    'Print Using "###,###"; actualAttendance&
    'Print

    Call ScoreSummary

    If autoPlay <> 1 Or playerMode <> 2 Then
        Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
    End If

    Cls

    Call STATS2SCREEN

End Sub

'----------------------------------------
'        ScoreSummary Subroutine
'----------------------------------------
'This subroutine prints a score by period
'for each team.
Sub ScoreSummary

    Color 14, 0

    'Fewer than 9 innings
    If INNING% < 9 Then

        For IG = 1 To INNING%
            Call InningPrintBox(IG)
            Print IG;
        Next IG

        Print "  H  R E"

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To INNING%

                Call InningPrintBox(I2)

                If I = 0 Then
                    Print Using "##"; inningScore(I, I2);
                End If
                If I = 1 And gameScore(1, 0) > gameScore(0, 0) And I2 = INNING% And inningScore(1, INNING%) = 0 Then
                    Print , " X";
                End If
                If I = 1 And gameScore(1, 0) < gameScore(0, 0) And I2 = INNING% Or I = 1 And gameScore(1, 0) > gameScore(0, 0) And I2 = INNING% And inningScore(1, INNING%) > 0 Then
                    Print Using "##"; inningScore(I, I2);
                End If
                If I = 1 And I2 < INNING% Then
                    Print Using "##"; inningScore(I, I2);
                End If

            Next I2

            Print " -";
            Print Using "## "; gameScore(I, 0);
            Print Using "## "; gameScore(I, 1);
            Print Using "#"; gameScore(I, 2)

        Next I

    End If

    '9 or 10 innings
    If INNING% = 10 And P = 0 Or INNING% <= 9 Then

        Color 14, 0
        For IG = 1 To 9
            Call InningPrintBox(IG)
            Print IG;
        Next IG

        If INNING% = 9 Then
            Print "  H  R E"
        Else
            Print "    H  R E"
        End If

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To 9
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            If INNING% = 10 Then
                Print "   -";
            Else
                Print " -";
            End If

            Print Using "## "; gameScore(I, 0);
            Print Using "## "; gameScore(I, 1);
            Print Using "#"; gameScore(I, 2)

        Next I

    End If

    'Innings 10 - 18
    If INNING% = 10 And P = 1 Or INNING% > 10 And INNING% < 19 Or INNING% = 19 And P = 0 Then

        Color 14, 0
        For IG = 1 To 9
            Call InningPrintBox(IG)
            Print IG;
        Next IG

        Print

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To 9
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print

        Next I

        If P = 0 And INNING% < 19 Or INNING% = 19 Then
            INNING% = INNING% - 1
        End If

        'Print innings 10 to 18
        Print

        Locate 5,
        Color 14, 0
        For IG = 10 To INNING%
            Call InningPrintBox(IG)
            Print IG;
        Next IG
        Print "  H  R E"

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 10 To INNING%
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print " - ";
            Print Using "## "; gameScore(I, 0);
            Print Using "## "; gameScore(I, 1);
            Print Using "#"; gameScore(I, 2)

        Next I

    End If

    'Innings 19 - 27
    If INNING% = 19 And P = 1 Or INNING% > 19 And INNING% <= 28 Then

        Color 14, 0
        For IG = 1 To 9
            Call InningPrintBox(IG)
            Print IG;
        Next IG

        Print

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);

            Color 15, 0

            For I2 = 1 To 9
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print

        Next I

        Print

        'Print innings 10 to 18
        Locate 5,
        Color 14, 0
        For IG = 10 To 18
            Call InningPrintBox(IG)
            Print IG;
        Next IG

        Print

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 10 To 18
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print

        Next I

        'Print innings 19 to 27
        Locate 8,
        Color 14, 0
        For IG = 19 To INNING%
            Call InningPrintBox(IG)
            Print IG;
        Next IG

        Print "  H  R E"

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 19 To INNING%
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print " -";
            Print Using "## "; gameScore(I, 0);
            Print Using "## "; gameScore(I, 1);
            Print Using "#"; gameScore(I, 2)

        Next I

    End If

End Sub


'----------------------------------------
'        STATS2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the game stats
'for output to the end user display
Sub STATS2SCREEN

    Shared actualAttendance&

    Dim BK$
    BK$ = "## # # ## # #  # # #  #  # # ## ##"

    For I = 0 To 1

        Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
        Print gameTeams$(I);
        Color 14, 0
        Locate , 18: Print "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
        Color 15, 0

        For I3 = 0 To 22

            If X0%(I, 0, I3) <> -1 And X0%(I, 1, I3) <> 1 Then

                I1 = X0%(I, 0, I3)

                Print batters$(I, I1); ", "; fieldPos$(X0%(I, 1, I3));
                Locate , 18
                '                AB             R              H              BI             2              3              HR             K              W              SB             CS              E               PO              A
                Print Using BK$; gameBatStats(I, I1, 0); gameBatStats(I, I1, 2); gameBatStats(I, I1, 1); gameBatStats(I, I1, 3); gameBatStats(I, I1, 4); gameBatStats(I, I1, 5); gameBatStats(I, I1, 6); gameBatStats(I, I1, 8); gameBatStats(I, I1, 7); gameBatStats(I, I1, 9); gameBatStats(I, I1, 10); gameBatStats(I, I1, 11); gameBatStats(I, I1, 16); gameBatStats(I, I1, 17);
                Print "   ";

                'BA
                If leagBatStats(I, I1, 0) = 0 Or leagBatStats(I, I1, 1) = 0 Then
                    Print " .000   ";
                Else
                    If leagBatStats(I, I1, 0) = leagBatStats(I, I1, 1) Then
                        Print "1.000   ";
                    Else
                        Print " ."; Using "###   "; (leagBatStats(I, I1, 1) / leagBatStats(I, I1, 0) * 1000);
                    End If
                End If

                '           HR   RBI
                Print Using "##  ###"; leagBatStats(I, I1, 6); leagBatStats(I, I1, 3)

                For I2 = 0 To 17
                    T3%(I2) = T3%(I2) + gameBatStats(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21:
            For I2 = 0 To 13:
                T3%(I2) = T3%(I2) + gamePitStats(I, I1, I2 + 23)
            Next

            For I2 = 14 To 17
                T3%(I2) = T3%(I2) + gamePitStats(I, I1, I2 + 22)
            Next
        Next

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then

                    Print pitchers$(I, I2); ", P";
                    Locate , 18
                    '                AB              R               H               BI              2               3               HR              K               W               SB              CS              E               PO              A
                    Print Using BK$; gamePitStats(I, I2, 23); gamePitStats(I, I2, 25); gamePitStats(I, I2, 24); gamePitStats(I, I2, 26); gamePitStats(I, I2, 27); gamePitStats(I, I2, 28); gamePitStats(I, I2, 29); gamePitStats(I, I2, 31); gamePitStats(I, I2, 30); gamePitStats(I, I2, 32); gamePitStats(I, I2, 33); gamePitStats(I, I2, 34); gamePitStats(I, I2, 38); gamePitStats(I, I2, 39);
                    Print "   ";

                    'BA
                    If leagPitStats(I, I2, 24) = 0 Or leagPitStats(I, I2, 23) = 0 Then
                        Print " .000   "
                    Else
                        If leagPitStats(I, I2, 24) = leagPitStats(I, I2, 23) Then
                            Print "1.000   ";
                        Else
                            Print Using "#.###   "; leagPitStats(I, I2, 24) / leagPitStats(I, I2, 23);
                        End If

                        '           HR   RBI
                        Print Using "##  ###"; leagPitStats(I, I2, 29); leagPitStats(I, I2, 26)

                    End If
                End If

            Next

        Next

        Color 14, 0: Print "TOTALS";
        Color 15, 0
        Locate , 17: Print T3%(0);
        Locate , 22: Print T3%(1);
        Locate , 27: Print T3%(4);
        Locate , 32: Print T3%(6);
        Locate , 36: Print T3%(7);
        Locate , 42: Print T3%(10);
        Locate , 46: Print T3%(16);
        Locate , 50: Print T3%(17)
        Locate , 20: Print T3%(2);
        Locate , 24: Print T3%(3);
        Locate , 29: Print T3%(5);
        Locate , 34: Print T3%(8);
        Locate , 39: Print T3%(9);
        Locate , 44: Print T3%(11)

        Print

        If autoPlay <> 1 Or playerMode <> 2 Then
            Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

        Cls

        For I1 = 0 To 22:
            T3%(I1) = 0
        Next I1

    Next I

    'GoTo 7313

    For I = 0 To 1

        Cls

        Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
        Print gameTeams$(I);

        Color 14, 0: Locate , 23
        Print "       IP   H  R  ER BB SO    ERA   W  L  S  SO": Color 15, 0

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then
                    I4 = 1 - I
                    Locate , 1
                    Print pitchers$(I, I2); " ";

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print "(W,"; Right$(Str$(leagPitStats(I, I2, 6) + 1), (Len(Str$(leagPitStats(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(leagPitStats(I, I2, 7)), (Len(Str$(leagPitStats(I, I2, 7))) - 1)); ")";
                        gameW$ = pitchers$(I, I2)
                    End If

                    If S1%(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print "(S,"; Right$(Str$(leagPitStats(I, I2, 12) + 1), (Len(Str$(leagPitStats(I, I2, 12) + 1)) - 1)); ")";
                    End If

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print "(L,"; Right$(Str$(leagPitStats(I, I2, 6) + 0), (Len(Str$(leagPitStats(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(leagPitStats(I, I2, 7) + 1), (Len(Str$(leagPitStats(I, I2, 7) + 1)) - 1)); ")";
                        gameL$ = pitchers$(I, I2)
                    End If

                    If gamePitStats(I, I2, 19) > 0 Then
                        Print "(BS,";: Print Using "##"; leagPitStats(I, I2, 19);: Print ")";
                    End If
                    If gamePitStats(I, I2, 22) > 0 Then
                        Print "(H,";: Print Using "##"; leagPitStats(I, I2, 22);: Print ")";
                    End If

                    Locate , 30: Print Using "##"; Int(gamePitStats(I, I2, 0) / 3);

                    If gamePitStats(I, I2, 0) - (Int(gamePitStats(I, I2, 0) / 3) * 3) > 0 Then
                        Print ".";: Print Using "#"; gamePitStats(I, I2, 0) - (Int(gamePitStats(I, I2, 0) / 3) * 3);
                    End If

                    Locate , 34

                    Print Using "## ##  ## ## ##"; gamePitStats(I, I2, 1); gamePitStats(I, I2, 2); gamePitStats(I, I2, 3); gamePitStats(I, I2, 4); gamePitStats(I, I2, 5);

                    Print "   ";

                    If gamePitStats(I, I2, 3) > 0 And gamePitStats(I, I2, 0) = 0 Then
                        Print "99.99 ";
                    Else
                        If gamePitStats(I, I2, 0) = 0 Then
                            Print "--.-- ";
                        Else
                            If gamePitStats(I, I2, 3) = 0 Then
                                Print " 0.00 ";
                            Else
                                Print Using "##.## "; gamePitStats(I, I2, 3) / (gamePitStats(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print Using "## "; leagPitStats(I, I2, 6) + 1;
                    Else
                        Print Using "## "; leagPitStats(I, I2, 6);
                    End If
                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print Using "## "; leagPitStats(I, I2, 7) + 1;
                    Else
                        Print Using "## "; leagPitStats(I, I2, 7);
                    End If
                    If S1%(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print Using "## "; leagPitStats(I, I2, 12) + 1;
                    Else
                        Print Using "## "; leagPitStats(I, I2, 12);
                    End If

                    Print Using "###"; leagPitStats(I, I2, 5)

                    For I4 = 0 To 5
                        T3%(I4) = T3%(I4) + gamePitStats(I, I2, I4)
                    Next I4
                End If

            Next I2

        Next I1

        Color 14, 0
        Print "TOTALS";
        Color 15, 0

        Locate , 30

        If (T3%(0) / 3) >= 10 Then
            Locate (gamePitcher(I) + 2), 29
        End If

        Print Int(T3%(0) / 3);

        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then
            Print ".";: Print Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);
        End If

        Locate , 34

        Print Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)
        Print

        If autoPlay <> 1 Or playerMode <> 2 Then
            Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

        Cls

        LYN% = CInt(T3%(0) / 3)

        For I1 = 0 To 5:
            T3%(I1) = 0
        Next I1

    Next I

    Cls

    '--- THIS NEEDS TO LOOK NICER

    Color 14, 0: Print " E-";
    Color 15, 0

    If E = 0 Then
        Print "none"
    Else
        For X = 0 To (E - 1):
            Print eventDesc$(X); ",";
        Next
        Print
    End If

    Color 14, 0: Print "2B-";
    Color 15, 0

    If DB = 0 Then
        Print "none"
    Else
        For X = 0 To (DB - 1):
            Print doublesPlayer$(X); ",";
        Next
        Print
    End If

    Color 14, 0: Print "3B-";
    Color 15, 0

    If TR = 0 Then
        Print "none"
    Else
        For X = 0 To (TR - 1):
            Print triplesPlayer$(X); ",";
        Next
        Print
    End If

    Color 14, 0: Print "HR-";
    Color 15, 0

    If HR = 0 Then
        Print "none"
    Else
        For X = 0 To (HR - 1):
            Print homeRuns$(X); ",";
        Next
        Print
    End If

    Color 14, 0: Print "SB-";
    Color 15, 0
    If SB = 0 Then
        Print "none"
    Else
        For X = 0 To (SB - 1):
            Print stolenBases$(X); ",";
        Next
        Print
    End If

    Color 14, 0: Print "LOB-";
    Color 15, 0: Print gameTeams$(0); Str$(L0%(0)); ","; gameTeams$(1); Str$(L0%(1))
    Print "DP-";
    Color 15, 0: Print gameTeams$(0); Str$(DP%(0)); ","; gameTeams$(1); Str$(DP%(1))

    Color 14, 0: Print "INJURIES-";
    Color 15, 0

    If injCount = 0 Then
        Print "none"
    Else
        For X = 1 To injCount:
            Print injPlayers$(X); ",";
        Next
        Print
    End If

    eject:
    Color 14, 0: Print "EJECTIONS-";
    Color 15, 0

    If ejectCnt = 0 Then
        Print "none"
    Else
        For X = 1 To ejectCnt:
            Print ejections$(X); ",";
        Next
        Print
    End If

    Color 14, 0: Print "UMPIRES: ";
    Color 15, 0: Print "HP-"; gameUmpire$(1); "; 1B-"; gameUmpire$(2); "; 2B-"; gameUmpire$(3); "; 3B-"; gameUmpire$(4)

    '-- THIS COULD GO ON THE Score Summary
    Color 14, 0: Print "TIME: ";
    Color 15, 0: Print gameTime$
    Color 14, 0: Print "ATTENDANCE: ";
    Color 15, 0: Print Using "###,###"; actualAttendance&
    Color 14, 0: Print "TEMPERATURE:";
    Color 15, 0: Print gameTemp
    Color 14, 0: Print "WIND: ";
    Color 15, 0: Print wind$
    Color 14, 0: Print "SKY: ";
    Color 15, 0: Print skyCond$(skyType)
    Color 14, 0: Print "PRECIPITATION: ";
    Color 15, 0: Print precip$(precipType)

    If autoPlay <> 1 Or playerMode <> 2 Then
        Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
    End If

End Sub


'----------------------------------------
'           BOX2SPC Subroutine
'----------------------------------------
'This subroutine adds any special event
'information to a boxscore file
Sub BOX2SPC (boxName$)

    Shared eventSettings()

    Open diskPaths$(1) + boxName$ For Append As #2

    Print #2,

    If INNING% > eventSettings(1) Then
        Print #2, Using "MORE THAN ## INNINGS"; eventSettings(1)
    End If

    If gameScore(1, 0) < eventSettings(2) Then
        Print #2, gameTeams$(1);
        Print #2, Using " SCORED LESS THAN ## RUN(S)"; eventSettings(2)
    End If

    If gameScore(0, 0) < eventSettings(2) Then
        Print #2, gameTeams$(0);
        Print #2, Using " SCORED LESS THAN ## RUN(S)"; eventSettings(2)
    End If

    If gameScore(1, 0) > eventSettings(3) Then
        Print #2, gameTeams$(1);
        Print #2, Using " SCORED MORE THAN ## RUN(S)"; eventSettings(3)
    End If

    If gameScore(0, 0) > eventSettings(3) Then
        Print #2, gameTeams$(0);
        Print #2, Using " SCORED MORE THAN ## RUN(S)"; eventSettings(3)
    End If

    If gameScore(1, 1) > eventSettings(5) Then
        Print #2, gameTeams$(1);
        Print #2, Using " HAD MORE THAN ## HIT%(S)"; eventSettings(5)
    End If

    If gameScore(0, 1) > eventSettings(5) Then
        Print #2, gameTeams$(0);
        Print #2, Using " HAD MORE THAN ## HIT%(S)"; eventSettings(5)
    End If

    If gameScore(1, 1) < eventSettings(4) Then
        Print #2, gameTeams$(1);
        Print #2, Using " HAD LESS THAN ## HIT%(S)"; eventSettings(4)
    End If

    If gameScore(0, 1) < eventSettings(4) Then
        Print #2, gameTeams$(0);
        Print #2, Using " HAD LESS THAN ## HIT%(S)"; eventSettings(4)
    End If

    For I = 0 To 1

        For I1 = 0 To 22

            If gameBatStats(I, I1, 1) > eventSettings(6) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HAD MORE THAN ## HIT%(S)"; eventSettings(6)
            End If

            If gameBatStats(I, I1, 2) > eventSettings(7) Then
                Print #2, batters$(I, I1);
                Print #2, Using " SCORED MORE THAN ## RUN(S)"; eventSettings(7)
            End If

            If gameBatStats(I, I1, 3) > eventSettings(8) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HAD MORE THAN ## RBI(S)"; eventSettings(8)
            End If

            If gameBatStats(I, I1, 4) > eventSettings(9) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HIT MORE THAN ## DOUBLES(S)"; eventSettings(9)
            End If

            If gameBatStats(I, I1, 5) > eventSettings(10) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HIT MORE THAN ## TRIPLE(S)"; eventSettings(10)
            End If

            If gameBatStats(I, I1, 6) > eventSettings(11) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HOMERED MORE THAN ## TIME(S)"; eventSettings(11)
            End If

            If gameBatStats(I, I1, 9) > eventSettings(12) Then
                Print #2, batters$(I, I1);
                Print #2, Using " STOLE MORE THAN ## BASE(S)"; eventSettings(12)
            End If

        Next

    Next

    For I = 0 To 1
        For I1 = 0 To 21
            If gamePitStats(I, I1, 5) > eventSettings(13) Then
                Print #2, pitchers$(I, I1);
                Print #2, Using " STRUCK OUT MORE THAN ## BATTER(S)"; eventSettings(13)
            End If
        Next
    Next

    Print #2,

    Close #2

End Sub


'----------------------------------------
'          EndOfGame Subroutine
'----------------------------------------
'This subroutine serves as the final portion
'of a post game routine. At this point,
'stats have been saved and we are simply
'looking to determine whether a new game
'should be started.
Sub EndOfGame ()

    Cls

    If useVGA = 1 Then
        Screen menu&
        _FreeImage imageScreen&
    End If

    If normalFinish = 1 Then

        'Close #11
        Call POSTGAME

        Cls

        ' Prompt to play another game
        If autoPlay <> 1 Then

            'Using Home Team's color for the box
            Call TEAMBOX(1, 1)

            pbpString$ = "PLAY ANOTHER GAME? (Y/N)"
            Locate 7, (80 - Len(pbpString$)) / 2
            Print pbpString$

            Color 15, 0

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

        If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
            Cls
            Print "PLEASE WAIT ..."
        End If

    End If

End Sub


'----------------------------------------
'          GetTeamYear Function
'----------------------------------------
'This subroutine determines the 4-digit
'team year from the team loaded by the
'file. It uses the 2 digit year to make
'assumptions. (00 to 24 are year 2000.)
'Replays from 00 to 24 are much more
'likely to be intended for 2000's than
'1900's. It's also safe to consider that
'86 to 99 would be much more likely to be 1900's
Function GetTeamYear% (yearFromName$)

    teamYear% = Val(yearFromName$)

    If teamYear% >= 0 And teamYear% <= 24 Then
        GetTeamYear% = 2000 + teamYear%
    Else
        GetTeamYear% = 1900 + teamYear%
    End If

End Function


'----------------------------------------
'            pbplog Routine
'----------------------------------------
'This subroutine is designed to help me
'print information about where the game
'is at during the code. The general idea
'was originally to be called everywhere
'there is PBP. This helps me debug where
'issues are happening in a game.
Sub pbpLog (lineNumber)

    Print #11, "Executing code from: "; lineNumber
    Print #11, gameTeams$(0); " vs "; gameTeams$(1)
    Print #11, "Rating  - "; strikeoutRating
    Print #11, "Strikes - "; nbrStrikes
    Print #11,

End Sub

'----------------------------------------
'       ChooseGameTeam Subroutine
'----------------------------------------
'This subroutine provides interface for
'choosing a team for the game
Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)

    Shared previousYear$

    Shared teamIndicator$()

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print teamIndicator$(gameIdx)
        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 8,

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 8, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, gameIdx)
                'teamGender$(gameIdx) = GetTeamGender$(yearNumber$)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do

                        YCoord = Int((80 - Len(teamName$)) / 2)
                        Locate 2, YCoord
                        Color teamRat_GAME(gameIdx, 11), teamRat_GAME(gameIdx, 12): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

                'chosenIdx = -1 means there was invalid data
            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor = 0: HiLiteBGColor = 7
        Rows% = LAST_ROW - 6
        Columns% = 4
        Spacing% = 6
        Row% = 5
        Column% = 1

        Color , 4
        Cls

        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate LAST_ROW, 1
        Print Tab(3); "PG DN";
        Print Tab(18); "ENTER";
        Color 15: Print "-SELECTS TEAM";
        Color 14
        Print Tab(39); "ESC";
        Color 15: Print "-SELECT NEW ID";
        Color 14
        Print Tab(72); "PG UP";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, gameIdx, validData)
            Call ReadGameExtraData(yearNumber$, selectedIdx, gameIdx)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA.... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx, dataOK)

    Shared gameDaysRest(), mgrFile()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q2$(0), 2 As Q2$(1), 2 As Q2$(2), 2 As Q2$(3), 2 As Q2$(4), 2 As Q2$(5), 2 As Q2$(6), 2 As Q2$(7), 2 As Q2$(8), 2 As Q2$(9), 2 As Q2$(10), 2 As Q2$(11), 2 As Q2$(12), 2 As Q2$(13), 4 As Q2$(14), 4297 As X$

    For A = 0 To 22
        Field #1, 45 + A * 99 As X$, 15 As Q2$(A * 43 + 15), 2 As Q2$(A * 43 + 16), 2 As Q2$(A * 43 + 17), 2 As Q2$(A * 43 + 18), 2 As Q2$(A * 43 + 19), 2 As Q2$(A * 43 + 20), 2 As Q2$(A * 43 + 21), 2 As Q2$(A * 43 + 22), 2 As Q2$(A * 43 + 23), 2 As Q2$(A * 43 + 24), 2 As Q2$(A * 43 + 25), 2 As Q2$(A * 43 + 26), 4297 - 37 - A * 99 As X$
        Field #1, 82 + A * 99 As X$, 2 As Q2$(A * 43 + 27), 2 As Q2$(A * 43 + 28), 2 As Q2$(A * 43 + 29), 2 As Q2$(A * 43 + 30), 2 As Q2$(A * 43 + 31), 2 As Q2$(A * 43 + 32), 2 As Q2$(A * 43 + 33), 2 As Q2$(A * 43 + 34), 2 As Q2$(A * 43 + 35), 2 As Q2$(A * 43 + 36), 2 As Q2$(A * 43 + 37), 2 As Q2$(A * 43 + 38), 4297 - 61 - A * 99 As X$
        Field #1, 106 + A * 99 As X$, 2 As Q2$(A * 43 + 39), 2 As Q2$(A * 43 + 40), 2 As Q2$(A * 43 + 41), 2 As Q2$(A * 43 + 42), 2 As Q2$(A * 43 + 43), 2 As Q2$(A * 43 + 44), 2 As Q2$(A * 43 + 45), 2 As Q2$(A * 43 + 46), 2 As Q2$(A * 43 + 47), 2 As Q2$(A * 43 + 48), 2 As Q2$(A * 43 + 49), 2 As Q2$(A * 43 + 50), 4297 - 85 - A * 99 As X$
        Field #1, 130 + A * 99 As X$, 2 As Q2$(A * 43 + 51), 2 As Q2$(A * 43 + 52), 2 As Q2$(A * 43 + 53), 2 As Q2$(A * 43 + 54), 2 As Q2$(A * 43 + 55), 2 As Q2$(A * 43 + 56), 2 As Q2$(A * 43 + 57), 4297 - 99 - A * 99 As X$
    Next

    For A = 0 To 21
        Field #1, 2322 + A * 89 As X$, 15 As Q2$(A * 38 + 1004), 2 As Q2$(A * 38 + 1005), 2 As Q2$(A * 38 + 1006), 2 As Q2$(A * 38 + 1007), 2 As Q2$(A * 38 + 1008), 2 As Q2$(A * 38 + 1009), 2 As Q2$(A * 38 + 1010), 2 As Q2$(A * 38 + 1011), 2 As Q2$(A * 38 + 1012), 2 As Q2$(A * 38 + 1013), 2 As Q2$(A * 38 + 1014), 2 As Q2$(A * 38 + 1015), 2020 - 37 - A * 89 As X$
        Field #1, 2359 + A * 89 As X$, 2 As Q2$(A * 38 + 1016), 2 As Q2$(A * 38 + 1017), 2 As Q2$(A * 38 + 1018), 2 As Q2$(A * 38 + 1019), 2 As Q2$(A * 38 + 1020), 2 As Q2$(A * 38 + 1021), 2 As Q2$(A * 38 + 1022), 2 As Q2$(A * 38 + 1023), 2 As Q2$(A * 38 + 1024), 2 As Q2$(A * 38 + 1025), 2 As Q2$(A * 38 + 1026), 2 As Q2$(A * 38 + 1027), 2020 - 61 - A * 89 As X$
        Field #1, 2383 + A * 89 As X$, 2 As Q2$(A * 38 + 1028), 2 As Q2$(A * 38 + 1029), 2 As Q2$(A * 38 + 1030), 2 As Q2$(A * 38 + 1031), 2 As Q2$(A * 38 + 1032), 2 As Q2$(A * 38 + 1033), 2 As Q2$(A * 38 + 1034), 2 As Q2$(A * 38 + 1035), 2 As Q2$(A * 38 + 1036), 2 As Q2$(A * 38 + 1037), 2 As Q2$(A * 38 + 1038), 2 As Q2$(A * 38 + 1039), 2020 - 85 - A * 89 As X$
        Field #1, 2407 + A * 89 As X$, 2 As Q2$(A * 38 + 1040), 2 As Q2$(A * 38 + 1041), 2020 - 89 - A * 89 As X$
    Next

    Field #1, 4280 As X$, 40 As Q2$(1840), 15 As Q2$(1841), 5 As Q2$(1842), 2 As Q2$(1843)

    Get #1, teamIdx

    targetTeam$ = RTrim$(Q2$(0))

    For I1 = 1 To 13
        teamRat_GAME(gameIdx, I1) = CVI(Q2$(I1))
    Next

    If (teamRat_GAME(gameIdx, 11) = 7 Or teamRat_GAME(gameIdx, 11) = 15) And teamRat_GAME(gameIdx, 12) = 0 Then
        teamRat_GAME(gameIdx, 11) = 0
        teamRat_GAME(gameIdx, 12) = 7
    End If

    parkHRVals!(gameIdx) = CVS(Q2$(14))

    For I1 = 0 To 22:

        batters$(gameIdx, I1) = RTrim$(Q2$(I1 * 43 + 15))

        For I2 = 0 To 41
            batRating(gameIdx, I1, I2) = CVI(Q2$(I1 * 43 + I2 + 16))
        Next

        'If player is inactive
        If batRating(gameIdx, I1, SS_B_Active) <> 0 Then
            batRating(gameIdx, I1, SS_B_Avail) = 99
        End If

    Next

    For I1 = 0 To 21:
        pitchers$(gameIdx, I1) = Q2$(I1 * 38 + 1004)
        pitchers$(gameIdx, I1) = RTrim$(pitchers$(gameIdx, I1))

        For I2 = 0 To 36
            pitchRating(gameIdx, I1, I2) = CVI(Q2$(I1 * 38 + I2 + 1005))
        Next

        If pitchRating(gameIdx, I1, SS_P_Active) <> 0 Then
            pitchRating(gameIdx, I1, SS_P_Avail) = 99
        End If
        If pitchRating(gameIdx, I1, SS_P_Throws) = 0 Then
            pitchRating(gameIdx, I1, SS_P_Throws) = 1
        End If
    Next

    gameStadiums$(gameIdx) = RTrim$(Q2$(1840))
    gameManagers$(gameIdx) = RTrim$(Q2$(1841))
    teamAbbreviatons$(gameIdx) = RTrim$(Q2$(1842))
    CK = CVI(Q2$(1843))

    Close #1

    If CK = 999 Then

        For I1 = 0 To 22
            'check for 1934 season
            If Val(Left$(gameTeams$(gameIdx), 2)) < 54 And Mid$(gameTeams$(gameIdx), 3, 1) = " " Then
                batRating(gameIdx, I1, SS_B_Bunt) = batRating(gameIdx, I1, SS_B_Bunt) * .75
            End If

            For I2 = 22 To 25
                If batRating(gameIdx, I1, I2) <> -1 And batRating(gameIdx, I1, I2) <> 0 And batRating(gameIdx, I1, I2 + 4) = 0 Then
                    batRating(gameIdx, I1, I2 + 4) = 900
                End If
            Next
        Next

        For X = 0 To 22
            For X1 = 1 To 14
                batRating(gameIdx, X, X1 + 41) = 0
            Next
        Next

        For X = 0 To 21
            For X1 = 1 To 28
                pitchRating(gameIdx, X, X1 + 36) = 0
            Next
        Next

        dataOK = 1

    Else

        dataOK = 0

    End If

End Sub


'----------------------------------------
'     ReadGameExtraData Subroutine
'----------------------------------------
'Reads in extra data for the selected team,
' given it's index in the team file.
'This extra file usually includes info.
' related to stadiums, coaches, etc.
'This routine is highly customized TO
' the specific game for which it has
' been included for.
Sub ReadGameExtraData (teamYear$, teamIdx, gameIdx)

    Shared gameDaysRest(), mgrFile()

    If useLRSplit = 0 Then

        teamSplit(gameIdx, 0) = -1

    Else

        'Load LR split from file
        If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$) Then

            teamSplit(gameIdx, 0) = -1

        Else

            Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = DATA_SIZE_BYTES

            For X = 0 To 11
                Field #2, X * 2 As X$, 2 As Q2$(X + 1), DATA_SIZE_BYTES - 2 - 2 * X As X$
            Next

            For X = 0 To 22
                For X1 = 1 To 14
                    Field #2, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q2$(12 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    Field #2, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q2$(334 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    Field #2, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q2$(642 + X1 + X * 14), DATA_SIZE_BYTES - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
                Next
            Next

            For X = 1 To 24
                Field #2, 1900 + (X - 1) * 2 As X$, 2 As Q2$(950 + X), DATA_SIZE_BYTES - 2 - (X - 1) * 2 - 1900 As X$
            Next

            For X = 0 To 22
                For X1 = 1 To 24
                    Field #2, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q2$(975 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    Field #2, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q2$(1528 + X1 + X * 24), DATA_SIZE_BYTES - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
                Next
            Next

            Get #2, teamIdx

            For X = 0 To 11
                teamSplit(gameIdx, X) = CVI(Q2$(X + 1))
            Next

            For X = 0 To 22
                For X1 = 1 To 14
                    batRating(gameIdx, X, X1 + 41) = CVI(Q2$(12 + X1 + X * 14))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    pitchRating(gameIdx, X, X1 + 36) = CVI(Q2$(334 + X1 + X * 14))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 14
                    pitchRating(gameIdx, X, X1 + 50) = CVI(Q2$(642 + X1 + X * 14))
                Next
            Next

            For X = 1 To 24:
                gameSA(gameIdx, X) = CVI(Q2$(X + 950))
            Next

            For X = 0 To 22
                For X1 = 1 To 24
                    batRating(gameIdx, X, X1 + 55) = CVI(Q2$(975 + X1 + X * 24))
                Next
            Next

            For X = 0 To 21
                For X1 = 1 To 24
                    pitchRating(gameIdx, X, X1 + 64) = CVI(Q2$(1528 + X1 + X * 24))
                Next
            Next

            Close 2

            If teamSplit(gameIdx, 0) <> 999 Then teamSplit(gameIdx, 0) = -1

        End If

    End If

    For X = 0 To 22

        For X1 = 0 To 14
            batRating(gameIdx, X, 80) = batRating(gameIdx, X, 80) + batRating(gameIdx, X, X1 + 41)
        Next

    Next

    For X = 0 To 21

        For X1 = 0 To 14
            pitchRating(gameIdx, X, 89) = pitchRating(gameIdx, X, 89) + pitchRating(gameIdx, X, X1 + 50)
        Next

    Next

    targetTeam$ = RTrim$(Q2$(0))

    'Read in manager data
    If Not _FileExists(diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$) Then

        mgrFile(gameIdx) = -1

    Else

        Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #3 Len = MGR_SIZE_BYTES

        For X = 0 To 999
            Field #3, X * 2 As Q2$, 2 As Q2$(X), MGR_SIZE_BYTES - X * 2 - 2 As Q2$
        Next

        Get #3, teamIdx

        For X = 0 To 999:
            teamMgrProfileVal(gameIdx, X) = CVI(Q2$(X))
        Next

        Close 3

    End If

    If teamMgrProfileVal(gameIdx, 0) <> 999 Or mgrFile(gameIdx) = -1 Then

        mgrFile(gameIdx) = -1

        For X = 0 To 999:
            teamMgrProfileVal(gameIdx, X) = -1
        Next

    End If

    statFile$ = nameStatFile$(gameTeams$(idx))

    'Read in supplemental data
    If _FileExists(diskPaths$(1) + statFile$ + "D") Then

        Open diskPaths$(1) + statFile$ + "D" For Input As #4

        For I1 = 0 To 22
            Input #4, injuryStatus(gameIdx, I1)
        Next

        For I1 = 0 To 21
            Input #4, gameDaysRest(gameIdx, I1)
        Next

        Close #4

        If useInj = 1 Then
            For I1 = 0 To 22
                If injuryStatus(gameIdx, I1) > 0 Then
                    batRating(gameIdx, I1, SS_B_Avail) = 99
                End If
            Next
        End If

    End If

    If useRest = 1 Then

        For I1 = 0 To 21
            If gameDaysRest(gameIdx, I1) <> 0 And gameDaysRest(gameIdx, I1) < 90 Then
                pitchRating(gameIdx, I1, SS_P_Rest) = gameDaysRest(gameIdx, I1)
            End If
        Next

    End If

End Sub

Sub DumpRunners(baseRunners, myText$)
    Print #11, "--- "; myText$; " ---"
    For bm1 = 0 to 3
       Print #11, "A5%:"; A5%(bm1) ; " -- ";
    Next bm1
    Print #11, ""

    For bm1 = 0 to 3
       Print #11, "b2Base:   "; b2Base(bm1) ; " -- ";
    Next bm1
    Print #11,
    For bm1 = 0 to 3
       Print #11, "b1Runner: "; b1Runner(bm1) ; " -- ";
    Next bm1
    Print #11,
End Sub

Sub DumpIt(baseRunners, P2, D2)

    Print #11, "---"
    Print #11, "Inning is:"; INNING%; _IIF(P = 0, "Visiting", "Home") + " team batting"
    Print #11, "baserunners = "; baseRunners; " outs: "; nbrOuts(0); " expected outs: "; nbrOuts(1)
    Print #11, "Score is: "; gameScore(0, 0); " to "; gameScore(1, 0)

    lp%   = lineupPlayer(P, currLineupSlot(P))
    fp%   = fldPos(      P, currLineupSlot(P))
    batr$ = _IIF( fp% = 1, pitchers$( P, currPitcher(P) ), batters$(P, lp%) )

    Print #11, "Lineup slot is: "; currLineupSlot(P); " Roster player is: "; lp%;  " Batter is: "; batr$; " Position is; "; fp%; " Pitcher is: "; pitchers$(D, currPitcher(D))
    
    Print #11, "Batter: "; player$(0); ", 1st base: "; player$(1); ", 2nd base: "; player$(2); ", 3rd base: "; player$(3); ", fielder w/ball: "; player$(4)

    If P2 <> 1 Then Print #11, " +++ Off strategy: "; P2;
    If D2 <> 1 Then Print #11, " +++ Def strategy: "; D2
    Print #11, ""

    For bm1 = 0 to 3
       Print #11, "b2Base:   "; b2Base(bm1) ; " -- ";   'all runners, earned and unearned
    Next bm1
    Print #11,
    For bm1 = 0 to 3
       Print #11, "b1Runner: "; b1Runner(bm1) ; " -- "; 'earned runners only
    Next bm1
    Print #11,
    Print #11,

    For bm1 = 0 to 3
       Print #11, "b2Pitcher:"; b2Pitcher(bm1) ; " -- ";
    Next bm1
    Print #11,
    For bm1 = 0 to 3
       Print #11, "b1Pitcher:"; b1Pitcher(bm1) ; " -- ";
    Next bm1
    Print #11,
    Print #11,

    For bm1 = 0 to 3
       Print #11, "b2Mystery:"; b2Mystery(bm1) ; " -- ";
    Next bm1

    Print #11, "---"

End Sub

'$Include: 'GameRoutines_Gfx.bm'

