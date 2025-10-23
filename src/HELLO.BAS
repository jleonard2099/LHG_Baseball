$Debug
$Color:32

Const GAME_VERSION$ = "7.15"
Const MAX_FILES = 10240
Const MAX_CAREER_YEARS = 15
'Const MAX_CONFERENCES = 40
Const MAX_SCHED_STATS = 162
Const MAX_SCHEDULE_GAMES = 2500
Const MAX_TOURNAMENT_GAMES = 200
Const MAX_TEAMS = 1000
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV = 10
Const DIVS_PER_LEAGUE = 4
Const TEAMS_PER_LEAGUE = 40

Const SCHEDULE_SIZE_BYTES = 56
Const LAST_COL = 80
Const LAST_ROW = 30
Const SCHED_ENTRIES_PER_SCREEN = 25
Const MAX_PBP_LINES = 13
Const MAX_UMPIRES = 100

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
Data INHERITED RUNNERS,INHERITED RUNS SCORED,INHERITED RUNS ALLOWED %
Data BLOWN SAVES,RUN SUPPORT (RPS),QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
For I = 1 To 28:
    Read pitIndCategory$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,LEFT ON BASE,DOUBLE PLAYS GR. INTO
Data PUTOUTS,ASSISTS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.
For I = 0 To 20:
    Read batTeamCategory$(I)
Next

Data INNINGS PITCHED,HITS ALLOWED,RUNS ALLOWED,EARNED RUNS,WALKS,STRIKEOUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLEPLAYS TURNED,HOME RUNS ALLOWED,SACRIFICE HITS,SACRIFICE FLIES,INHERITED RUNNERS
Data INHERITED RUNS SCORED,BLOWN SAVES,QUALITY STARTS,PERCENT OF RI SCORED,EARNED RUN AVERAGE
For I = 0 To 18:
    Read pitTeamCategory$(I)
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
Data LEADS HELD,INHERITED RUNNERS,INHERITED RUNS SCORED,INHERITED RUNS ALLOWED %
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
Data SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT (RPS),RETURN TO MAIN MENU
For I = 0 To 29
    Read expPitCategory$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO
Data FIELDING AVERAGE,INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,INHERITED RUNNERS,INHERITED RUNS SCORED,INHERITED RUNS ALLOWED %
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,SACRIFICE HITS,SACRIFICE FLIES,RETURN TO MAIN MENU
For I = 0 To 39:
    Read expTeamCategory$(I)
Next

Data NAME,G,AB,R,H,2B,3B,HR,RBI,SB,BB,SO,BA,SLG,OBP,CS,%RAB
Data CHS,LHS,GIDP,NEEDS,PROJ AB,PO,A,E,FAVG,SBA,%CS,SH,SF

For I = 0 To 29:
    Read batCategoryBatterAbbr$(I)
Next

Data NAME,G,AB,R,H,2B,3B,HR,RBI,SB,BB,SO,BA,SLG,OBP,CS
Data CHS,LHS,GIDP,PO,A,E,FAVG,SH,SF

For I = 0 To 24:
    Read batCategoryPitcherAbbr$(I)
Next

Data NAME,W,L,ERA,G,GS,CG,SHO,SV,IP,H,R,ER,HRA,BB,SO,DP,%IP,BS,HL,RI,RS,SUP,GS,QS,SH,SF

For I = 0 To 26:
    Read pitCategoryAbbr$(I)
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

'AB%() -- for analyzing player usage??
Data 681,663,645,627,609,591,573,555,537

'bunt quality (on grass)
Data 6,12,37,55,66,74,86,96,100
Data 5,10,32,55,67,75,86,96,100
Data 4,8,27,54,68,76,86,96,100
Data 3,7,25,54,69,76,86,96,100
Data 3,6,22,54,69,77,86,96,100
Data 2,5,20,53,69,78,86,96,100
Data 2,4,17,53,70,78,86,96,100
Data 1,3,12,52,71,79,86,96,100
Data 1,2,7,51,72,80,86,96,100

'bunt quality (on turf)
Data 6,14,42,55,63,74,86,96,100
Data 5,12,37,55,64,75,86,96,100
Data 4,10,32,55,65,76,86,96,100
Data 3,8,30,55,66,77,86,96,100
Data 3,7,27,55,66,77,86,96,100
Data 3,6,24,55,66,77,86,96,100
Data 2,5,22,55,67,78,86,96,100
Data 1,4,17,55,68,79,86,96,100
Data 1,3,12,55,69,80,86,96,100

'base hit on bunt attempt
Data 0,0,100
Data 0,15,100
Data 0,30,100
Data 0,45,100
Data 0,60,100
Data 1,75,100
Data 3,90,100
Data 4,97,100

'lead runner outcome on bunt attempt
Data 100,0
Data 90,100
Data 75,100
Data 60,100
Data 45,100
Data 30,100
Data 15,100
Data 8,100

'batter misses pitch on bunt attempt (strike / strikeout)
Data 100,0,0
Data 90,100,0
Data 70,95,100
Data 50,80,100
Data 30,65,100
Data 10,50,100

'buntTriple() -- attempting to bunt
Data 0,90,100
Data 5,87,100
Data 10,85,100
Data 15,80,100
Data 20,77,100
Data 25,75,100
Data 30,73,100
Data 35,70,100
Data 40,67,100
Data 45,65,100
Data 50,63,100
Data 60,65,100

'Read in data and assign variables
For I = 1 To 9:
    Read AB%(I)
Next

For I = 0 To 1
    For I1 = 1 To 9
        For I2 = 1 To 9
            Read buntQuality(I, I1, I2)
        Next
    Next
Next

For I = 2 To 9
    For I1 = 0 To 2
        Read buntBaseHit(I, I1)
    Next
Next

For I = 2 To 9
    For I1 = 0 To 1
        Read buntLeadRun(I, I1)
    Next
Next

For I = 0 To 5
    For I1 = 0 To 2
        Read buntStrike(I, I1)
    Next
Next

For I = 11 To 0 Step -1
    For I1 = 0 To 2
        Read buntTriple(I, I1)
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

    result$ = CopyFile$("RECORDS.MLB", "BASEBALL.REC")
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
Print #10, "An error occurred. Please contact support with the following information:"
Print #10, "Error #"; Err; "on program file line"; _ErrorLine; "-- "; _ErrorMessage$

If _InclErrorLine Then
    Print #10, "    in module "; _InclErrorFile$; " line "; _InclErrorLine
End If

If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "####"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
Print #10,
Close #10
Resume Next ' moves program to code following the error.


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
                    Loop Until activeSchedule = 0 Or earlyExit = 1

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
    Shared league_brief$(), leagueType$(), month$()
    Shared pitHand$(), pitcherEra$()
    Shared playMode$(), umpireName$()
    Shared teamIndicator$(), yesNoText$()
    Shared gameMgrIdx(), umpireRating()

    Shared umpBBadj(), umpSZadj(), umpRunAdj()

    Shared scheduleFile$

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------
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
        Print #3, playerOpt
        Print #3, leagueOpt
        Print #3, desigHit
        Print #3, gameTypeOpt
        Print #3, gameMonth
        Print #3, dayOrNight
        Print #3, useWeather
        Print #3, useInj
        Print #3, useLRSplit
        Print #3, useRest
        Print #3, useMgrProf
        Print #3, pbpDelay!
        Print #3, useVGA
        Print #3, pitchEraOpt
        Print #3, sndOpt
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

            '3-batter minimum
            Print #3, 1

            'Ghost runner
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
        Input #4, batterMinimumOpt
        Input #4, ghostRunOpt
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

    If Environ$("PBPLOG") = "TRUE" Then
        Open "pbplog.txt" For Append As #11
    End If

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
            Color 15: Print "PITCHER ERA: "; pitcherEra$(pitchEraOpt)
            Locate 15, 2: Color 14
            Print "(C) ";
            Color 15: Print "3-BATTER MIN FOR RELIEF PITCHER: "; yesNoText$(batterMinimumOpt)
            Locate 16, 2: Color 14
            Print "(D) ";
            Color 15: Print "AUTOMATIC RUNNER IN EXTRA INNINGS: "; yesNoText$(ghostRunOpt)
            Locate 17, 2: Color 14
            Print "(E) ";
            Color 15: Print "USE INJURIES: "; yesNoText$(useInj)
            Locate 18, 2: Color 14
            Print "(F) ";
            Color 15: Print "USE LEFT/RIGHT SPLIT STATS: "; yesNoText$(useLRSplit)
            Locate 19, 2: Color 14
            Print "(G) ";
            Color 15: Print "USE DAYS REST DATA FOR PITCHERS: "; yesNoText$(useRest)
            Locate 20, 2: Color 14
            Print "(H) ";
            Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE: "; yesNoText$(useMgrProf)
            Locate 21, 2: Color 14
            Print "(I) ";
            Color 15: Print "USE WEATHER EFFECTS: "; yesNoText$(useWeather)
            Locate 22, 2: Color 14
            Print "(J) ";
            Color 15: Print "USE BALLPARK GRAPHICS, IF ABLE: "; yesNoText$(useVGA)
            Locate 23, 2: Color 14
            Print "(K) ";
            Color 15: Print "SOUND ENABLED: "; yesNoText$(sndOpt)
            Locate 25, 2: Color 14
            Print "NOTE:: "
            Color 15

            Locate , 2: Print "* DH WAS USED BY AL IN 1973; NL BEGAN USING IN 2022"
            Locate , 2: Print "* 3-BATTER MIN AND AUTO RUNNER INTRODUCED IN 2020"
            Locate , 2: Print "* LEAGUE TYPE CURRENTLY ONLY IMPACTS UMPIRE CHOICES"
            'Locate , 2: Print "* EXHIBITION GAMES CURRENTLY NOT IMPLEMENTED"

            Do
                Color 7, 0

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Call LetterToNumber(I$, setting)
                End If

            Loop Until setting >= 0 And setting <= 20 Or I$ = Chr$(27) Or UCase$(I$) = Chr$(17) Or UCase$(I$) = "Q"

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
                        Print #7, batterMinimumOpt
                        Print #7, ghostRunOpt
                        Close #7

                    Case 1:
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(diskIDs$(0), NTMS, gameMgrIdx(0), gameTeams$(0), 0)
                        If tmIdx <> 0 Then
                            'Perform any team-specific auto-setup
                        End If
                        Color 15, 0

                    Case 3:
                        Call ChooseGameTeam(diskIDs$(1), NTMS, gameMgrIdx(1), gameTeams$(1), 1)
                        If tmIdx <> 0 Then
                            'Perform any team-specific auto-setup
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

                    Case 8:
                        dayOrNight = 1 - dayOrNight
                        Locate 8, 4

                    Case 9:
                        '

                    Case 10:
                        Do
                            Locate 13, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11:
                        pitchEraOpt = pitchEraOpt + 1
                        If pitchEraOpt > 8 Then pitchEraOpt = 0

                    Case 12:
                        batterMinimumOpt = 1 - batterMinimumOpt

                    Case 13:
                        ghostRunOpt = 1 - ghostRunOpt

                    Case 14:
                        useInj = 1 - useInj

                    Case 15:
                        useLRSplit = 1 - useLRSplit

                    Case 16:
                        useRest = 1 - useRest

                    Case 17:
                        useMgrProf = 1 - useMgrProf

                    Case 18
                        useWeather = 1 - useWeather
                        
                    Case 19
                        useVGA = 1 - useVGA
                        
                    Case 20
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
    If earlyExit = 1 Then
        Close #11
        Exit Sub
    End If

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

    Call SelectStadium(useVGA)

    Call LoadParkAttData

    'Determine attendance based on annual AVERAGE
    'Use values in the range of 85 - 115% of this
    parkIDTxt$ = LTrim$(Str$(teamRat_GAME(1, 13)))
    tgtAttYear = teamYears(1)
    avgAttendance& = GetParkAttendance&(parkIDTxt$, tgtAttYear)

    If gameLoc = 2 Or avgAttendance& = 0 Or parkIDTxt$ = "0" Then

        'Get assumed year of home team
        Select Case gameYear

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
        actualAttendance& = avgAttendance& * (0.85 + Rnd * 0.3)
    End If

    YV = 1
    YM = 0

    '-------------------------
    ' Determine umpires
    '-------------------------
    If leagueOpt > 1 Then
        'Default to 'NL' if inter-league game
        '--- what's a better way???
        leagueOpt = 1
    End If

    'Start with all generic umpires
    For I1 = 1 To 4
        gameUmpire$(I1) = genericNames$(I1)
        umpBBadj(I1) = 0
        umpSZadj(I1) = 0
        umpRunAdj(I1) = 0
    Next


    'Try to load any umpires we can and
    'populate ratings from them
    targetUmpires$ = diskPaths$(0) + "UMPIRES_" + league_brief$(leagueOpt) + "." + LTrim$(Str$(teamYears(1)))

    If _FileExists(targetUmpires$) Then

        Call ReadUmpireFile(targetUmpires$)

        nbrUmps = 0

        ' Count available umpires
        For I1 = 1 To MAX_UMPIRES
            If umpireName$(I1) <> "" And umpireName$(I1) <> "XXX" Then
                nbrUmps = nbrUmps + 1
            End If
        Next

        If nbrUmps < 4 Then

            currUmp = 1

            For I1 = 1 To MAX_UMPIRES

                If umpireName$(I1) <> "" And umpireName$(I1) <> "XXX" And currUmp <= 4 Then

                    gameUmpire$(currUmp) = umpireName$(I1)

                    umpBBadj(currUmp) = (umpireRating(I1, 0) - 100) / 5
                    umpSZadj(currUmp) = (100 - umpireRating(I1, 1)) / 5
                    umpRunAdj(currUmp) = (umpireRating(I1, 2) - 100) / 3

                    usedUmpires(I1) = 1
                    currUmp = currUmp + 1

                End If

            Next

        Else

            'Randomly assign umpires
            currUmp = 1

            Do Until currUmp >= 5

                rndUmp = Int(Rnd(1) * MAX_UMPIRES) + 1

                attempts = 0

                Do
                    rndUmp = Int(Rnd(1) * MAX_UMPIRES) + 1
                    attempts = attempts + 1
                Loop Until (umpireName$(rndUmp) <> "" And umpireName$(rndUmp) <> "XXX" And usedUmpires(rndUmp) <> 1) Or attempts > MAX_UMPIRES

                If umpireName$(rndUmp) <> "" And umpireName$(rndUmp) <> "XXX" And usedUmpires(rndUmp) <> 1 Then

                    gameUmpire$(currUmp) = umpireName$(rndUmp)

                    umpBBadj(currUmp) = (umpireRating(rndUmp, 0) - 100) / 17
                    umpSZadj(currUmp) = (100 - umpireRating(rndUmp, 1)) / 20
                    umpRunAdj(currUmp) = (umpireRating(rndUmp, 2) - 100) / 12

                    usedUmpires(rndUmp) = 1
                    currUmp = currUmp + 1
                Else
                    ' Couldn't find valid umpire, exit
                    Exit Do
                End If

            Loop

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
    For currTeam = 0 To 1

        cancelPitchers% = 0

        Call SelectPitchers(currTeam, cancelPitchers%, useCompRotation)
        Call PitchingStarter(currTeam)

        pitcherStam(currTeam, 0) = pitcherMaxStam%(currTeam, currPitcher(currTeam))

        If gamePitcher(currTeam) > 1 Then
            pitcherStam(currTeam, 1) = 0
        Else
            'Starting pitcher has more efficiency?
            pitcherStam(currTeam, 1) = 3
        End If

        'I'm not sure why we go back only
        'if ESC was pressed during pitcher select
        If cancelPitchers% = 1 Then
            GoTo 170
        End If

    Next currTeam

    'Determine lineup for each team
    For currTeam = 0 To 1

        Do

            Cls

            Call INFO

            Locate 3, 1
            Color teamRat_GAME(currTeam, 11), teamRat_GAME(currTeam, 12): Print gameTeams$(currTeam): Color 7, 0
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

                    If (playerMode = 1 And compTeam = currTeam) Or playerMode = 2 Then
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

                    Call SelectBatters(validBatters, currTeam)

                    If validBatters = 1 Then

                        For I = 10 To 22
                            Locate I, 59: Print Space$(21);
                        Next

                        Call StartingLineup(currTeam)
                        Call ModifyPreGameLineup(currTeam, reselect)

                        If noLineups = 1 Then
                            noLineups = 0
                        End If

                    End If

                Case "1", "3":
                    'Lineup using profile
                    Call SearchForLineup(currTeam, Q2%)

                    Call LineupFromProfile(lineupFound%, currTeam, skipLineChange%, useMgrProf, I$, Q2%)

                    If lineupFound% = 1 Or skipLineChange% = 0 Then
                        Call ModifyPreGameLineup(currTeam, reselect)
                        If noLineups = 1 Then
                            noLineups = 0
                        End If
                    Else
                        noLineups = 1
                    End If

                Case "2"
                    Call ComputerLineups(validBatters, currTeam, noLineups, reselect)

                    If noLineups = 1 Then
                        noLineups = 0
                        skipLineChange% = 1
                    End If

            End Select

            If I$ <> Chr$(27) Then
                'We didn't ESC so we have to move forward

                If skipLineChange% <> 1 Then
                    Color 15, 0
                    Locate 5, 59: Print "OPPOSING PITCHER"
                    Locate , 59: Print pitchers$(1 - currTeam, currPitcher(1 - currTeam)); " "; handed$(pitchRating(1 - currTeam, currPitcher(1 - currTeam), SS_P_Throws) + 2)
                    'If RP = 1 Then RP = 0
                End If

            Else
                'ESC means we go back to earlier menus
                noLineups = 1
                GoTo 170

            End If

        Loop Until noLineups = 0 And reselect = 0

    Next currTeam

    '-------------------------
    ' Ready to Play
    '-------------------------

    'Initialize roster with starting lineup
    For I = 0 To 1

        For I1 = 1 To 9
            gameRoster(I, 0, I1) = lineupPlayer(I, I1)
            gameRoster(I, 1, I1) = fldPos(I, I1)
            gameRoster(I, 2, I1) = I1 * 10
        Next I1

        playersUsed(I) = 9

    Next I

    W5 = 0
    W6 = 1

    VV(0) = 1
    VV(1) = 1

    VV1(0) = 9
    VV1(1) = 9

    Cls

    rainDelay = 0

    For I = 0 To 1
        For I1 = 1 To 9
            If fldPos(I, I1) >= 2 And fldPos(I, I1) <= 9 Then
                If fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos1) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm) = batRating(I, lineupPlayer(I, I1), SS_B_Arm1)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range) = batRating(I, lineupPlayer(I, I1), SS_B_Range1)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct1)
                ElseIf fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos2) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm) = batRating(I, lineupPlayer(I, I1), SS_B_Arm2)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range) = batRating(I, lineupPlayer(I, I1), SS_B_Range2)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct2)
                ElseIf fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos3) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm) = batRating(I, lineupPlayer(I, I1), SS_B_Arm3)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range) = batRating(I, lineupPlayer(I, I1), SS_B_Range3)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct3)
                ElseIf fldPos(I, I1) = batRating(I, lineupPlayer(I, I1), SS_B_Pos4) Then
                    batRating(I, lineupPlayer(I, I1), SS_B_Arm) = batRating(I, lineupPlayer(I, I1), SS_B_Arm4)
                    batRating(I, lineupPlayer(I, I1), SS_B_Range) = batRating(I, lineupPlayer(I, I1), SS_B_Range4)
                    batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = batRating(I, lineupPlayer(I, I1), SS_B_FldPct4)
                Else
                    Select Case fldPos(I, I1)
                        Case 2
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm) = 1
                            batRating(I, lineupPlayer(I, I1), SS_B_Range) = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 910 + Int(Rnd(1) * 10) + 1
                        Case 3
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm) = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_Range) = 3
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 960 + Int(Rnd(1) * 10) + 1
                        Case 4
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm) = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_Range) = 3
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 912 + Int(Rnd(1) * 10) + 1
                        Case 5, 6
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm) = 0
                            batRating(I, lineupPlayer(I, I1), SS_B_Range) = 3
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 902 + Int(Rnd(1) * 10) + 1
                        Case 7 To 9
                            batRating(I, lineupPlayer(I, I1), SS_B_Arm) = 2
                            batRating(I, lineupPlayer(I, I1), SS_B_Range) = 2
                            batRating(I, lineupPlayer(I, I1), SS_B_FldPct) = 972 + Int(Rnd(1) * 10) + 1
                    End Select
                End If
            End If
        Next I1
    Next I

    Call GETWEATHER(windDir, windSpeed, gameMonth, windImpact!, skyType, gameTemp, rainDelay)

    Cls

    Call TEAMBOX(5, 1)

    Locate , 11: Print "CONDITIONS AT "; gameStadiums$(1)

    Locate , 11: Print "TEMPERATURE: "; gameTemp

    If teamRat_GAME(1, 9) = 1 Then
        Locate , 11: Print "WEATHER: DOME"
        Locate , 11: Print "WIND: NONE"
    Else
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
    'pitcherStam(0, 1) = calculated from Walks + Hits / Game
    'pitcherStam(1, 1) = 3

    Select Case pitchEraOpt

        Case 3
            '1946-1954
            pitcherStam(0, 1) = 4
            pitcherStam(1, 1) = 4

        Case 4 To 6
            '1909 to 1945
            pitcherStam(0, 1) = 5
            pitcherStam(1, 1) = 5

        Case 7
            '1894 to 1908
            pitcherStam(0, 1) = 7
            pitcherStam(1, 1) = 7

        Case Is >= 8
            '1876-1893
            pitcherStam(0, 1) = 99
            pitcherStam(1, 1) = 99

    End Select

    Call LOADSTATS
    Call DISPBAVG

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

    If (autoPlay <> 1 Or playerMode <> 2) And sndOpt = 1 Then
        hit& = _SndOpen("hit.wav")
        miss& = _SndOpen("miss.wav")
        glove& = _SndOpen("glove.wav")
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
Sub StartingLineup (luTeamIdx)

    skipLU% = 0

    Do
        'track assigned positions
        '   0 = unassigned
        Erase lineupAssigned

        Call ShowUnassignedFielding

        For I = 1 To 9

            Color 15, 0
            Locate 5, 59: Print "OPPOSING PITCHER"
            Locate , 59: Print pitchers$(1 - luTeamIdx, currPitcher(1 - luTeamIdx)); " "; handed$(pitchRating(1 - luTeamIdx, currPitcher(1 - luTeamIdx), SS_P_Throws) + 2)

            Call AssignFieldingPositions(luTeamIdx, I)

        Next I

    Loop Until skipLU% = 0

End Sub


'----------------------------------------
'   AssignFieldingPositions Subroutine
'----------------------------------------
'This subroutine works with the user to
'help them select fielding positions for
'each place in the lineup
Sub AssignFieldingPositions (teamIdx, posIdx)

    560 '
    validInput = 0

    Do Until validInput = 1 Or I$ = Chr$(27)

        'Clear selected fielding slot
        'in case any position is there
        Call FieldPosCursor(posIdx)
        Print Space$(18);
        'Show available slot(s)
        Call ShowUnassignedFielding
        'Move cursor to print selected player
        Call FieldPosCursor(posIdx)

        I$ = GetKeyPress$

        If I$ = Chr$(27) Then

            Erase lineupAssigned

            For J = 1 To 9
                fldPos(teamIdx, J) = -1
                lineupPlayer(teamIdx, J) = -1
            Next

            For J = 0 To 22
                batRating(teamIdx, J, SS_B_Avail) = 0
            Next

            For J = 0 To 22
                If batRating(teamIdx, J, SS_B_Active) <> 0 Then
                    batRating(teamIdx, J, SS_B_Avail) = 99
                End If
            Next

        Else

            Call LetterToNumber(I$, I1)

            If UCase$(I$) = "X" Then

                Erase lineupAssigned

            Else

                validInput = 1

                ' Check conflicts
                playerConflict = 0

                For J = 1 To 9
                    If J <> posIdx And (lineupPlayer(teamIdx, J) <> I1 or fldPos(teamIdx, J) <> 1) And _
                    lineupPlayer(teamIdx, J) = I1 And UCase$(I$) <> "P" Then
                        playerConflict = 1
                        Exit For
                    End If
                Next J

                dhConflict = 0
                If (desigHit = 1 And UCase$(I$) = "P") Or (batRating(teamIdx, I1, SS_B_Avail) <> 0 And UCase$(I$) <> "P") Then
                    dhConflict = 1
                End If

                If playerConflict = 0 And dhConflict = 0 Then

                    ' Process assignment
                    If UCase$(I$) = "P" Then

                        pitcherFound = 0
                        For J = 1 To 9
                            If J <> posIdx Then
                                If lineupPlayer(teamIdx, J) = currPitcher(teamIdx) And fldPos(teamIdx, J) = 1 Then
                                    pitcherFound = 1
                                    Exit For ' No need to keep checking
                                End If
                            End If
                        Next J

                        If pitcherFound = 0 Then

                            lineupPlayer(teamIdx, posIdx) = currPitcher(teamIdx)
                            Print pitchers$(teamIdx, currPitcher(teamIdx));
                            lineupAssigned(I1) = 1

                            Locate , 75

                            Print "P ";
                            fldPos(teamIdx, posIdx) = 1
                            fielder(teamIdx, 1) = currPitcher(teamIdx)
                            lineupAssigned(1) = 1

                        End If

                    Else

                        lineupPlayer(teamIdx, posIdx) = I1
                        Print Using "# "; posIdx;
                        Print batters$(teamIdx, lineupPlayer(teamIdx, posIdx));

                        Locate , 74

                        Do

                            firstSlot = 1

                            Do
                                J$ = GetKeyPress$
                                chosenSlot = Val(J$)
                            Loop Until chosenSlot <> firstSlot

                            validChoice = 1

                            For currSlot = 1 To 9
                                If currSlot <> posIdx And chosenSlot = fldPos(teamIdx, currSlot) Then
                                    validChoice = 0
                                    Exit For
                                End If
                            Next currSlot

                        Loop While validChoice = 0

                        If (desigHit = 1 Or chosenSlot <> 0) And UCase$(J$) <> "X" Then

                            'Print position selected
                            Print fieldPos$(chosenSlot);
                            fldPos(teamIdx, posIdx) = chosenSlot

                            fielder(teamIdx, fldPos(teamIdx, posIdx)) = lineupPlayer(teamIdx, posIdx)
                            lineupAssigned(chosenSlot) = 1

                            'Highlight the player we selected
                            Locate I1 + 2, 2: Print "*";

                            Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12)
                            Print batters$(teamIdx, I1)
                            Color 15, 0

                        End If

                    End If

                    'Show newly available slot
                    Call ShowUnassignedFielding

                End If

            End If 'Done checking for conflict with input

        End If

    Loop

End Sub


'----------------------------------------
'   ShowUnassignedFielding Subroutine
'----------------------------------------
'This subroutine outputs each of the
'currently available fielding positions
'It will blank out any positions that
'were used up. Index #'s are printed with
'the position description to make it easier
Sub ShowUnassignedFielding

    Color 15
    Locate 25, 1

    For I0 = 0 To 9

        If Not (desigHit = 0 And I0 = 0) Or (desigHit = 1 And I0 = 1) Then

            Locate 25, (I0 * 3) + 1

            If lineupAssigned(I0) = 0 Then
                Print fieldPos$(I0); "  ";
            End If
            If lineupAssigned(I0) = 1 Then
                Print "   ";
            End If

            Locate 26, (I0 * 3) + 1

            If lineupAssigned(I0) = 0 Then
                Print I0; "  ";
            End If
            If lineupAssigned(I0) = 1 Then
                Print "   ";
            End If

        End If

    Next I0

End Sub

'----------------------------------------
'    ModifyPreGameLineup Subroutine
'----------------------------------------
'This routine gives the user a chance to
'make adjustments to the lineup before
'they start the game.
Sub ModifyPreGameLineup (luTeamIdx, reselect)

    Do

        Locate 28, 1
        Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
        Color 15, 0

        If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then
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
                        If userDone = 0 Then
                            Call SaveLineup(luTeamIdx)
                        End If

                        For I = 1 To 9
                            If fldPos(luTeamIdx, I) <> 1 Then
                                batRating(luTeamIdx, lineupPlayer(luTeamIdx, I), SS_B_Avail) = 1
                            End If
                        Next I

                        Erase lineupAssigned

                    Case "R", Chr$(27):

                        Erase lineupAssigned

                        For J = 1 To 9
                            fldPos(luTeamIdx, J) = -1
                            lineupPlayer(luTeamIdx, J) = -1
                        Next

                        For J = 0 To 22:
                            batRating(luTeamIdx, J, SS_B_Avail) = 0
                        Next

                        For J = 0 To 22
                            If batRating(luTeamIdx, J, SS_B_Active) <> 0 Then
                                batRating(luTeamIdx, J, SS_B_Avail) = 99
                            End If
                        Next

                        reselect = 1

                    Case "L":
                        Call LINSWAP(luTeamIdx)

                    Case "P":
                        Call POSSWAP(luTeamIdx)

                End Select

            Case 1 To 9:
                'Print player as one that is no
                'longer selected
                Locate lineupPlayer(luTeamIdx, I) + 2, 2
                Print " "; batters$(luTeamIdx, lineupPlayer(luTeamIdx, I))

                'Remove player from lineup
                lineupAssigned(fldPos(luTeamIdx, I)) = 1 - lineupAssigned(fldPos(luTeamIdx, I))

                Call AssignFieldingPositions(luTeamIdx, I)

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
Sub SaveLineup (luTeamIdx)

    Shared mgrFile(), gameMgrIdx()

    If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then
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

        If pitchRating(1 - luTeamIdx, currPitcher(1 - luTeamIdx), SS_P_Throws) = -1 Then
            Q2% = 1
        End If
        If pitchRating(1 - luTeamIdx, currPitcher(1 - luTeamIdx), SS_P_Throws) = 1 Then
            'ToDo [bm] changed from 0
            Q2% = 0
        End If

        For I = 0 To 9
            Print I; " ";
            If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) = 999 Then
                Print "LINEUP ALREADY SAVED "; teamMgrProfileVal(luTeamIdx, 100 + (I + Q2% * 10) * 21); " % USAGE"
            Else
                Print "AVAILABLE"
            End If
        Next

        Input "LINEUP # TO SAVE "; LN%
        Input "% OF TIME TO USE "; LU%

        If teamMgrProfileVal(luTeamIdx, 120 + (LN% + Q2% * 10) * 21) <> 999 Then
            teamMgrProfileVal(luTeamIdx, 120 + (LN% + Q2% * 10) * 21) = 999
        End If
        If Q2% = 1 Then
            LN% = LN% + 10
        End If

        teamMgrProfileVal(luTeamIdx, 101 + LN% * 21) = desigHit
        teamMgrProfileVal(luTeamIdx, 100 + LN% * 21) = LU%

        For I = 1 To 9
            teamMgrProfileVal(luTeamIdx, 110 + I + LN% * 21) = lineupPlayer(luTeamIdx, I)
            teamMgrProfileVal(luTeamIdx, 101 + I + LN% * 21) = fldPos(luTeamIdx, I)
        Next

        teamMgrProfileVal(luTeamIdx, 99) = 999
        teamMgrProfileVal(luTeamIdx, 0) = 999
        mgrFile(luTeamIdx) = 0

        teamYear$ = RTrim$(diskIDs$(luTeamIdx))

        Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #9 Len = MGR_SIZE_BYTES

        For I = 0 To 999
            Field #9, I * 2 As Q$, 2 As Q2$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
        Next

        For I = 0 To 999:
            LSet Q2$(I) = MKI$(teamMgrProfileVal(luTeamIdx, I))
        Next

        Put #9, gameMgrIdx(luTeamIdx)

        Close 9

    End If

End Sub


'----------------------------------------
'       SearchForLineup Subroutine
'----------------------------------------
'This routines searches for an existing
'lineup from the manager profiles.
Sub SearchForLineup (luTeamIdx, Q2%)

    Shared pitHand$()

    Cls

    opp = 1 - luTeamIdx

    If pitchRating(opp, currPitcher(opp), SS_P_Throws) = -1 Then
        Q2% = 1
    End If
    If pitchRating(opp, currPitcher(opp), SS_P_Throws) = 1 Then
        Q2% = 0
    End If

    Print Tab(30); "LINEUPS FOUND VS. "; pitHand$(Q2%)
    Print " #0    "; teamMgrProfileVal(luTeamIdx, 100 + (0 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #1    "; teamMgrProfileVal(luTeamIdx, 100 + (1 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #2    "; teamMgrProfileVal(luTeamIdx, 100 + (2 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #3    "; teamMgrProfileVal(luTeamIdx, 100 + (3 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #4    "; teamMgrProfileVal(luTeamIdx, 100 + (4 + Q2% * 10) * 21); "%"

    For I = 0 To 4
        For I1 = 1 To 9

            If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 3, 16 * I + 1
                Print "NONE FOUND"
            Else

                If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 2 + I1, 16 * I + 1: Print "         "
                Else
                    Locate 2 + I1, 16 * I + 1

                    If teamMgrProfileVal(luTeamIdx, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batters$(luTeamIdx, teamMgrProfileVal(luTeamIdx, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 2 + I1, 16 * I + 12: Print fieldPos$(teamMgrProfileVal(luTeamIdx, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If
        Next I1
    Next I

    Print
    Print " #5    "; teamMgrProfileVal(luTeamIdx, 100 + (5 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #6    "; teamMgrProfileVal(luTeamIdx, 100 + (6 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #7    "; teamMgrProfileVal(luTeamIdx, 100 + (7 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #8    "; teamMgrProfileVal(luTeamIdx, 100 + (8 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #9    "; teamMgrProfileVal(luTeamIdx, 100 + (9 + Q2% * 10) * 21); "%"

    For I = 5 To 9

        For I1 = 1 To 9

            If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 14, 16 * (I - 5) + 1: Print "NONE FOUND"
            Else
                If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 13 + I1, 16 * (I - 5) + 1: Print "         "
                Else
                    Locate 13 + I1, 16 * (I - 5) + 1

                    If teamMgrProfileVal(luTeamIdx, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batters$(luTeamIdx, teamMgrProfileVal(luTeamIdx, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 13 + I1, 16 * (I - 5) + 12: Print fieldPos$(teamMgrProfileVal(luTeamIdx, 101 + I1 + (I + Q2% * 10) * 21))
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
Sub LineupFromProfile (lineupFound%, luTeamIdx, skipLineChange%, useMgrProf, I$, Q2%)

    lineupFound% = 0
    PCT% = 0

    For I = 0 To 9
        If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) = 999 Then
            lineupFound% = lineupFound% + 1
            PCT% = PCT% + teamMgrProfileVal(luTeamIdx, 100 + (I + Q2% * 10) * 21)
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

        If autoPlay = 1 And useMgrProf = 1 And _ 
            ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Or _
            I$ = "3" Then

            'Create automated lineup
            RN0 = Int(Rnd(1) * PCT%) + 1
            PCT% = 0

            I = 0

            Do While I <= 9 And PCT% < RN0
                If teamMgrProfileVal(luTeamIdx, 120 + (I + Q2% * 10) * 21) = 999 Then
                    PCT% = PCT% + teamMgrProfileVal(luTeamIdx, 100 + (I + Q2% * 10) * 21)
                    If PCT% >= RN0 Then LN% = I
                End If
                I = I + 1
            Loop

        Else
            Do
                Locate 23, 1: Input "ENTER LINEUP NUMBER (0-9) "; LN%
            Loop Until LN% >= 0 And LN% <= 9

        End If

        If teamMgrProfileVal(luTeamIdx, 120 + (LN% + Q2% * 10) * 21) = 999 Then

            If Q2% = 1 Then
                LN% = LN% + 10
            End If

            If desigHit = teamMgrProfileVal(luTeamIdx, 101 + LN% * 21) Then

                For I = 1 To 9
                    lineupPlayer(luTeamIdx, I) = teamMgrProfileVal(luTeamIdx, 110 + I + LN% * 21)
                    fldPos(luTeamIdx, I) = teamMgrProfileVal(luTeamIdx, 101 + I + LN% * 21)
                Next

                LU% = 1
                Call LineupSummary(luTeamIdx, skipLineChange%, LU%)

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
'        LineupSummary Subroutine
'----------------------------------------
'This subroutine displays a summary of the
'starting lineup before allowing the user
'to make any adjustemts.
Sub LineupSummary (luTeamIdx, reselect, LU%)

    For I = 1 To 9:

        If fldPos(luTeamIdx, I) >= 0 Then

            fielder(luTeamIdx, fldPos(luTeamIdx, I)) = lineupPlayer(luTeamIdx, I)

            If fldPos(luTeamIdx, I) = 1 Then
                lineupPlayer(luTeamIdx, I) = currPitcher(luTeamIdx)
                fielder(luTeamIdx, 1) = currPitcher(luTeamIdx)
            Else
                batRating(luTeamIdx, lineupPlayer(luTeamIdx, I), SS_B_Avail) = 0
            End If

        End If

    Next I

    If LU% = 1 Then
        LU% = 0
        Call SelectBatters(validBatters, luTeamIdx)
    End If

    For I = 10 To 22
        Locate I, 59: Print Space$(21);
    Next

    '-- This is where we're about to run into issues
    '-- Perhaps we simply output the lineup here?
    '-- Then also output it other areas to narrow it down?
    '-- it's still probably the logic we're after but
    '-- something is not triggering my conditional
    Call NEWLINES(luTeamIdx)

    For I2 = 0 To 22:

        Locate I2 + 2, 2

        For I3 = 1 To 9
            If lineupPlayer(luTeamIdx, I3) = I2 And fldPos(luTeamIdx, I3) <> 1 Then
                Print "*";
                Color teamRat_GAME(luTeamIdx, 11), teamRat_GAME(luTeamIdx, 12)
                Print batters$(luTeamIdx, I2)
                Color 15, 0
            End If
        Next I3

    Next I2

    For I3 = 0 To 9:
        lineupAssigned(I3) = 1
    Next I3

    Call ModifyPreGameLineup(luTeamIdx, reselect)

End Sub


'----------------------------------------
'        SelectBatters Subroutine
'----------------------------------------
'This subroutine lets you choose the batters
'for the game. It interfaces with various
'other routines to show you different types
'of batter stats to help you with your choices.

Sub SelectBatters (validBatters, luTeamIdx)

    userDone = 99

    Call SHOWBATTERS(luTeamIdx)

    Color 15, 0
    Locate 5, 59: Print "OPPOSING PITCHER"
    Locate 6, 59: Print pitchers$(1 - luTeamIdx, currPitcher(1 - luTeamIdx)); " "; handed$(pitchRating(1 - luTeamIdx, currPitcher(1 - luTeamIdx), SS_P_Throws) + 2)

    'TG = 0

    Do
        'Select batters first
        Locate 10, 59: Print "DISABLE BATTER (0-M)"
        Locate , 59: Print "Z-CONTINUE"
        Locate , 59: Print "X-REACTIVATE ALL"
        Locate , 59: Print "<TAB>-BAT/FIELD/SPLIT"

        If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then
            'Autoplay = auto-select
            I$ = "Z"
            validBatters = 1
        Else
            I$ = GetKeyPress$

            'Handle ESC
            If I$ = Chr$(27) Then

                Erase lineupAssigned

                For J = 1 To 9
                    fldPos(luTeamIdx, J) = -1
                    lineupPlayer(luTeamIdx, J) = -1
                Next

                For J = 0 To 22
                    batRating(luTeamIdx, J, SS_B_Avail) = 0
                Next

                For J = 0 To 22
                    If batRating(luTeamIdx, J, SS_B_Active) <> 0 Then
                        batRating(luTeamIdx, J, SS_B_Avail) = 99
                    End If
                Next

                validBatters = 0

                Exit Sub

            End If

        End If

        ' Check if user pressed Tab
        If Asc(I$) = 9 Then
            TG = TG + 1
        End If

        If TG > 2 Or TG = 2 And teamSplit(luTeamIdx, 0) <> 999 Then
            TG = 0
        End If

        If Asc(I$) = 9 Then
            '   Update display after TAB
            Select Case TG
                Case 0:
                    Call SHOWBATTERS(luTeamIdx)
                Case 1:
                    Call SHOWFIELD(luTeamIdx)
                Case 2:
                    Call ShowSplit(luTeamIdx)
            End Select

        Else
            'Not TAB / ESC
            'Deteremine index of batter chosen
            Call LetterToNumber(I$, I1)

            'Assign field position
            Select Case UCase$(I$)

                Case "Z":
                    TG = 0
                    Call SHOWBATTERS(luTeamIdx)

                Case "X":
                    For I3 = 0 To 22
                        If batRating(luTeamIdx, I3, SS_B_Avail) = 99 And batRating(luTeamIdx, I3, SS_B_Active) = 0 Then
                            batRating(luTeamIdx, I3, SS_B_Avail) = 0
                            Locate I3 + 2, 3: Print batters$(luTeamIdx, I3)
                        End If
                    Next I3

                Case Else:
                    If I1 >= 0 And I1 <= 22 Then
                        batRating(luTeamIdx, I1, SS_B_Avail) = 99
                        Color 0, 7
                        Locate I1 + 2, 3: Print batters$(luTeamIdx, I1);
                        Color 15, 0
                    End If

            End Select

        End If 'Done checking for TAB

        'If it wasn't an ESC, let's print the batter
        If I$ <> Chr$(27) Then

            Color 15, 0
            Locate 5, 59: Print "OPPOSING PITCHER"
            Locate 6, 59: Print pitchers$(1 - luTeamIdx, currPitcher(1 - luTeamIdx)); " "; handed$(pitchRating(1 - luTeamIdx, currPitcher(1 - luTeamIdx), SS_P_Throws) + 2)
            userDone = 0
            validBatters = 1

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

    'SELECT A DIFFERENT STADIUM

    'Read-in park names from stadium
    Open PARK_FILE_NAME$ For Random As #1 Len = PARK_SIZE_BYTES

    fileLength& = LOF(1)

    numberParks = fileLength& / PARK_SIZE_BYTES

    Field #1, 40 As Q$(0)

    For parkIdx = 1 To numberParks

        Get #1, parkIdx
        park$ = RTrim$(Q$(0))

        If park$ <> "Not Used" Then
            parkNames$(parkIdx) = PadRight(park$, 38)
        End If

    Next parkIdx

    Close #1

    Cls

    Do

        Call TEAMBOX(5, 1)

        'Determine park from the team data
        chosenPark$ = Right$(Str$(teamRat_GAME(1, 13)), Len(Str$(teamRat_GAME(1, 13))) - 1)

        If dayOrNight = 0 Then
            '0 = daytime
            chosenPark$ = chosenPark$ + "D.png"
        Else
            '1 = nighttime
            chosenPark$ = chosenPark$ + "N.png"
        End If

        'Display detected park
        Locate 7, 11: Print "CURRENT PARK IS:"
        Locate , 11: Print gameStadiums$(1)

        Locate , 11

        parkFile$ = "." + dirSeparator$ + "parks" + dirSeparator$ + chosenPark$

        If _FileExists(parkFile$) Then
            Print "GRAPHIC EXISTS"
        Else
            '99 is the Generic Park
            Print "GRAPHIC NOT AVAILABLE"
            chosenPark$ = "99N.png"
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
                Locate 23, 21: Print "* - STADIUM GRAPHIC FILE NOT AVAILABLE"
                Start% = 1

                Do
                    SELECTION% = 1
                    Count% = 15
                    ExitCode% = 99
                    FileFGColor = 15: FileBGColor = 1
                    HiLiteFGColor = 0: HiLiteBGColor = 7
                    Spacing% = 1
                    Row% = 5
                    menuWidth% = 38
                    Column% = (80 - menuWidth% - 2) / 2

                    Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

                    _MouseShow "Default"
                    MAMenu parkNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, 15, 1, 1, Row%, Column%
                    _MouseHide

                    Color , 0

                    If ExitCode% <> 27 Then

                        chosenPark$ = Right$(Str$(SELECTION%), Len(Str$(SELECTION%)) - 1)
                        If dayOrNight = 0 Then chosenPark$ = chosenPark$ + "D.png" Else chosenPark$ = chosenPark$ + "N.png"
                        Color 15, 0

                        If _FileExists(".\parks\" + chosenPark$) Then
                            foundPark% = 1
                            teamRat_GAME(1, 13) = SELECTION%
                            gameStadiums$(1) = parkNames$(SELECTION%)
                        Else
                            Do
                                Locate 10, 8
                                Print "STADIUM GRAPHIC NOT AVAILABLE"
                                Locate , 8: Print "USE SELECTED PARK RATINGS WITH"
                                Locate , 8: Print "GENERIC DISPLAY? (Y/N)"
                                J$ = GetKeyPress$
                            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                            If I$ = "Y" Then
                                teamRat_GAME(1, 13) = SELECTION%
                                gameStadiums$(1) = parkNames$(SELECTION%)
                                If dayOrNight = 0 Then chosenPark$ = "99D.png" Else chosenPark$ = "99N.png"
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
        If Not _FileExists("." + dirSeparator$ + "parks" + dirSeparator$ + chosenPark$) Then
            useVGA = 0
        End If

    Loop Until BO% = 0 Or UCase$(I$) = "N" Or ExitCode% = 27

End Sub


'----------------------------------------
'       LoadParkAttData Subroutine
'----------------------------------------
'This routine loads park attendance data
'from a CSV. The format of the CSV ties
'park ID's to the annual attendance that
'is recorded from real life
Sub LoadParkAttData

    Dim parkID
    Dim attYear
    Dim avgAttend&
    Dim dummy$

    Shared parkAttKey$()
    Shared parkAvgAtt&()
    Shared totalParks

    Open "parkattendance.csv" For Input As #1

    ' Skip header line
    Line Input #1, dummy$

    totalParks = 0

    Do While Not EOF(1)

        totalParks = totalParks + 1

        ' Read CSV line
        Input #1, parkID, attYear, avgAttend&

        ' Create lookup key and store data
        parkAttKey$(totalParks) = LTrim$(Str$(parkID)) + "_" + LTrim$(Str$(attYear))
        parkAvgAtt&(totalParks) = avgAttend&

    Loop

    Close #1

End Sub


'----------------------------------------
'      PitchingRotations Subroutine
'----------------------------------------
'This subroutine guides the user through
'the various ways that pitching rotations
'can be configured for the game.
Sub PitchingRotations (compRotation, luTeamIdx)

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
        If pitchers$(luTeamIdx, I) <> "XXX" Then
            Print pitchers$(luTeamIdx, I); Tab(18); pitchRating(luTeamIdx, I, SS_P_GS);
        End If

        Locate , 25
        If pitchers$(luTeamIdx, I + 7) <> "XXX" Then
            Print pitchers$(luTeamIdx, I + 7); Tab(43); pitchRating(luTeamIdx, I + 7, SS_P_GS);
        End If

        Locate , 50
        If pitchers$(luTeamIdx, I + 14) <> "XXX" Then
            Print pitchers$(luTeamIdx, I + 14); Tab(68); pitchRating(luTeamIdx, I + 14, SS_P_GS)
        End If

    Next

    If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then

        gamesStarted = 0

        For I = 0 To 21
            If pitchRating(luTeamIdx, I, SS_P_GS) > gamesStarted Then
                gamesStarted = pitchRating(luTeamIdx, I, SS_P_GS)
            End If
        Next

        'Auto-determine # of men in rotation
        'based on number of games started
        I = 1
        Select Case gamesStarted

            Case 1 To 32:
                I = 5

            Case 33 To 41:
                I = 4

            Case 42 To 54:
                I = 3

            Case 55 To 75:
                I = 2

        End Select

    Else

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 5

    End If

    teamMgrProfileVal(luTeamIdx, 1) = I
    teamMgrProfileVal(luTeamIdx, 2) = 0

    For I = 3 To 36
        teamMgrProfileVal(luTeamIdx, I) = -1
    Next

    For I = 41 To 56
        teamMgrProfileVal(luTeamIdx, I) = 0
    Next

    teamMgrProfileVal(luTeamIdx, 0) = 999

    userDone = 99

    Call PrintPitcherInfo(luTeamIdx)

    If compRotation = 0 Then

        For I = 0 To teamMgrProfileVal(luTeamIdx, 1) - 1

            Do

                'We need to skip this in certain circumstances
                'Right now, it is being loaded for auto-play
                Locate 15, 59: Print "STARTER "; I + 1

                Do
                    I$ = GetKeyPress$
                    Call LetterToNumber(I$, I1)
                Loop Until I1 <= 21 And teamMgrProfileVal(luTeamIdx, 3) <> I1 And teamMgrProfileVal(luTeamIdx, 4) <> I1 And teamMgrProfileVal(luTeamIdx, 5) <> I1 And teamMgrProfileVal(luTeamIdx, 6) <> I1 And teamMgrProfileVal(luTeamIdx, 7) <> I1

                Locate 16, 59: Print pitchers$(luTeamIdx, I1)

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

            teamMgrProfileVal(luTeamIdx, I + 3) = I1

            For I2 = 15 To 17:
                Locate I2, 59: Print Space$(18)
            Next

        Next

    Else

        ' COMPUTER SET ROTATION
        'Make sure our next team isn't auto-set
        'without a choice
        compRotation = 0

        For I = 0 To 21
            If pitchers$(luTeamIdx, I) = "XXX" Then
                gamesStarted%(I, 0) = 0
                gamesStarted%(I, 1) = -1
            Else
                gamesStarted%(I, 0) = pitchRating(luTeamIdx, I, SS_P_GS)
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

        For I = 0 To teamMgrProfileVal(luTeamIdx, 1) - 1
            teamMgrProfileVal(luTeamIdx, I + 3) = gamesStarted%(I, 1)
        Next

        I1 = 0

        I = teamMgrProfileVal(luTeamIdx, 1)

        Do While I <= 21 And gamesStarted%(I, 0) <> 0
            I1 = I1 + 1
            teamMgrProfileVal(luTeamIdx, I1 + 20) = gamesStarted%(I, 1)
            I = I + 1
        Loop

    End If

    I1 = I1 - 1

    If teamMgrProfileVal(luTeamIdx, 1) = 1 Then
        teamMgrProfileVal(luTeamIdx, 13) = 100
    Else
        If teamMgrProfileVal(luTeamIdx, 1) = 2 Then gamesStarted% = 75
        If teamMgrProfileVal(luTeamIdx, 1) = 3 Then gamesStarted% = 54
        If teamMgrProfileVal(luTeamIdx, 1) = 4 Then gamesStarted% = 41
        If teamMgrProfileVal(luTeamIdx, 1) = 5 Then gamesStarted% = 32

        For I = 1 To teamMgrProfileVal(luTeamIdx, 1)
            teamMgrProfileVal(luTeamIdx, I + 12) = CInt(pitchRating(luTeamIdx, teamMgrProfileVal(luTeamIdx, I + 2), SS_P_GS) * 100 / gamesStarted%)
        Next

        SS = 0

        For I = 21 To 36
            If teamMgrProfileVal(luTeamIdx, I) >= 0 And teamMgrProfileVal(luTeamIdx, I) <= 21 Then
                SS = SS + pitchRating(luTeamIdx, teamMgrProfileVal(luTeamIdx, I), SS_P_GS)
            End If
        Next

        For I = 21 To 36
            If teamMgrProfileVal(luTeamIdx, I) >= 0 And teamMgrProfileVal(luTeamIdx, I) <= 21 Then
                teamMgrProfileVal(luTeamIdx, I + 20) = CInt(pitchRating(luTeamIdx, teamMgrProfileVal(luTeamIdx, I), SS_P_GS) * 100 / SS)
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
Sub ComputerLineups (validBatters, luTeamIdx, noLineups, reselect)

    Shared bestBatterIdx()
    Shared bestBatterFPos(), bLineupIdx()

    'batter #, team
    Dim position(23, 1)

    For I = 0 To 22
        'Essentially a form of setting the player as inactive
        If batters$(luTeamIdx, I) = "XXX" Then
            batRating(luTeamIdx, I, SS_B_Avail) = 98
        End If
    Next

    Call SelectBatters(validBatters, luTeamIdx)

    If validBatters = 1 Then

        For I = 10 To 22
            Locate I, 59: Print Space$(21);
        Next

        Erase bLineupIdx, bestBatterFPos

        For I = 1 To 9:
            bestBatterIdx(I) = -1
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

                    bestBatterFPos(9) = 1
                    Exit For

                Else

                    J = J - 1
                    currPos = 0

                    For batterIdx = 0 To 22

                        If batRating(luTeamIdx, batterIdx, SS_B_Avail) <= 0 And batters$(luTeamIdx, batterIdx) <> "XXX" Then

                            If batRating(luTeamIdx, batterIdx, SS_B_Pos1) = 0 Then
                                If batRating(luTeamIdx, batterIdx, SS_B_Pos2) = -1 Then
                                    position(currPos, 0) = batRating(luTeamIdx, batterIdx, SS_B_AB)
                                ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos3) = -1 Then
                                    position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .9 + .5)
                                ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos4) = -1 Then
                                    position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .8 + .5)
                                Else
                                    position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .7 + .5)
                                End If

                            ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos2) = 0 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .5 + .5)
                            ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos3) = 0 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .3 + .5)
                            ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos4) = 0 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .2 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .05 + .5) ' no DH experience
                            End If

                            position(currPos, 1) = batterIdx
                            currPos = currPos + 1

                        End If

                    Next batterIdx

                End If
            Else

                'I <> 1
                ' For each fielding position I (1-9), look for active players at that position.
                ' Store his "normalized" at-bats in position(currPos, 0) and field position #
                'in position(currPos, 1)

                'Most positions
                For batterIdx = 0 To 22:

                    If batRating(luTeamIdx, batterIdx, SS_B_Avail) <= 0 And batters$(luTeamIdx, batterIdx) <> "XXX" Then

                        If batRating(luTeamIdx, batterIdx, SS_B_Pos1) = I Then
                            If batRating(luTeamIdx, batterIdx, SS_B_Pos2) = -1 Then
                                position(currPos, 0) = batRating(luTeamIdx, batterIdx, SS_B_AB)
                            ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos3) = -1 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .9 + .5)
                            ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos4) = -1 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .8 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .7 + .5)
                            End If

                        ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos2) = I Then
                            If batRating(luTeamIdx, batterIdx, SS_B_Pos3) = -1 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .1 + .5)
                            ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos4) = -1 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .1 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .1 + .5)
                            End If

                        ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos3) = I Then
                            If batRating(luTeamIdx, batterIdx, SS_B_Pos4) = -1 Then
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .1 + .5)
                            Else
                                position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .1 + .5)
                            End If

                        ElseIf batRating(luTeamIdx, batterIdx, SS_B_Pos4) = I Then
                            position(currPos, 0) = Int(batRating(luTeamIdx, batterIdx, SS_B_AB) * .1 + .5)
                        End If

                        position(currPos, 1) = batterIdx
                        currPos = currPos + 1

                    End If

                Next batterIdx

            End If 'Done checking value of I

            currPos = currPos - 1

            'Sum all at-bats for lineup
            AB% = 0

            For I1 = 0 To currPos
                AB% = AB% + position(I1, 0)
            Next

            'Determine target position based on distribution
            'of at-bats
            RN0 = Int(Rnd(1) * AB%) + 1
            lineupDone% = 0

            I1 = 0
            Do While I1 <= currPos And lineupDone% = 0
                If RN0 <= position(I1, 0) Then
                    bLineupIdx(J) = position(I1, 1)
                    batRating(luTeamIdx, position(I1, 1), SS_B_Avail) = 1
                    lineupDone% = 1
                Else
                    position(I1 + 1, 0) = position(I1 + 1, 0) + position(I1, 0)
                    I1 = I1 + 1
                End If
            Loop


            If lineupDone% <> 1 Then

                Locate 25, 1: Print "CANNOT MAKE A LINEUP!!...";
                noLineups = 1
                'reselect = 1

                Erase lineupAssigned

                For J = 1 To 9:
                    fldPos(luTeamIdx, J) = -1
                    lineupPlayer(luTeamIdx, J) = -1
                Next

                For J = 0 To 22
                    If batRating(luTeamIdx, J, SS_B_Avail) <> 99 Then batRating(luTeamIdx, J, SS_B_Avail) = 0
                Next

                If autoPlay <> 1 Then
                    I$ = GetKeyPress$
                    Exit For
                End If

            End If 'done checking lineup

        Next I

        Call AssignHitters(luTeamIdx)

        For I = 1 To 9
            lineupPlayer(luTeamIdx, I) = bestBatterIdx(I)
            fldPos(luTeamIdx, I) = bestBatterFPos(I)
        Next

        Call LineupSummary(luTeamIdx, reselect, LU%)

    End If 'done checking batterFlag

End Sub


'----------------------------------------
'         AssignHitters Subroutine
'----------------------------------------
'This subroutine is used to perform a sort
'on all the hitters before presenting the
'lineup summary to the user.
Sub AssignHitters (luTeamIdx)

    Shared bestBatterFPos()
    Shared bestBatterIdx()

    ' Note: At least for now, the original "USE_LH" algorithm
    ' is not being used but is savaialble

    ' Put the 3 hitters with the highest SLG Pct in the 3-5 holes in the lineup card
    Call OptimizeLineupSlot(3, USE_SLUG, luTeamIdx)
    Call OptimizeLineupSlot(4, USE_SLUG, luTeamIdx)
    Call OptimizeLineupSlot(5, USE_SLUG, luTeamIdx)

    ' Of the remaining hitters, put the 2 with the highest OBA plus SB in the 1-2 holes
    Call OptimizeLineupSlot(1, USE_OBA_SB, luTeamIdx)
    Call OptimizeLineupSlot(2, USE_OBA_SB, luTeamIdx)

    ' For the rest, slot them in descending order by OPS (on-base plus slugging)
    Call OptimizeLineupSlot(6, USE_OPS, luTeamIdx)
    Call OptimizeLineupSlot(7, USE_OPS, luTeamIdx)
    Call OptimizeLineupSlot(8, USE_OPS, luTeamIdx)

    If desigHit = 1 Then

        Call OptimizeLineupSlot(9, USE_OPS, luTeamIdx)

    End If

End Sub


'----------------------------------------
'      OptimizeLineupSlot Subroutine
'----------------------------------------
'I literally have no clue what this sub-
'routine does. It has very little context
'that hints to it's purpose.
Sub OptimizeLineupSlot (B, calcMode, luTeamIdx)

    Shared bestBatterFPos(), bestBatterIdx(), bLineupIdx()

    ' Dim JJ!, R!, SE!, Q1!, QQ!

    theMax! = -100
    UFldNo = -1
    TPlayNo = -1
    validPositionFound = 0

    'L = lineup position
    For L = 0 To 9

        ' Skip positions based on designated hitter rules
        If (desigHit = 0 And L = 0) Or (desigHit = 1 And L = 1) Then
            L = 2
        End If

        ' Bounds check after modifications
        If L > 9 Then Exit For

        'Check if this batter is already assigned
        found = 0
        Z = 1

        Do While Z <= 9 And found = 0
            If bestBatterIdx(Z) = bLineupIdx(L) Then
                found = 1
            Else
                Z = Z + 1
            End If
        Loop

        If found = 0 Then

            'Assign J based on lineup position
            J = bLineupIdx(L)

            Select Case calcMode

                Case USE_SLUG
                    If batRating(luTeamIdx, J, SS_B_AB) > 0 Then
                        slug! = (batRating(luTeamIdx, J, SS_B_H) + batRating(luTeamIdx, J, SS_B_2B) + batRating(luTeamIdx, J, SS_B_3B) * 2 + batRating(luTeamIdx, J, SS_B_HR) * 3) / batRating(luTeamIdx, J, SS_B_AB)

                        If slug! >= theMax! Then
                            theMax! = slug!
                            TPlayNo = J
                            UFldNo = L
                            validPositionFound = 1
                        End If
                    End If

                Case USE_LH
                    ' original Lance Haffner algorithm
                    ' this is weighted by speed / steal ratings both
                    JJ = J

                    'Hits - Triples - HR's + Walks * 0.7
                    'Or, basically Singles + Doubles + Walks * 0.7
                    stealPct! = (batRating(luTeamIdx, JJ, SS_B_H) - batRating(luTeamIdx, JJ, SS_B_3B) - batRating(luTeamIdx, JJ, SS_B_HR) + batRating(luTeamIdx, JJ, SS_B_W)) * .7

                    If stealPct! = 0 Then stealPct! = 1

                    '   Stolen Bases / (Singles + Doubles + Walks * 0.7)
                    stealPct! = batRating(luTeamIdx, JJ, SS_B_SB) / stealPct!

                    If stealPct! >= .02 Then
                        QQ! = stealPct! + .18
                        R! = Int(QQ! * (18 + 2 * Abs((QQ! < .34))))
                    Else
                        R! = 2 + Int(stealPct! * 100)
                        If batRating(luTeamIdx, JJ, 1) = 2 Then R! = R! - 1
                    End If

                    If (batRating(luTeamIdx, JJ, 1) = 6 Or batRating(luTeamIdx, JJ, 1) = 8) And R! < 7 Then
                        R! = R! + 1
                    End If

                    If R! > 9 Then R! = 9
                    If R! < 0 Then R! = 1

                    ' Prevent division by zero
                    If batRating(luTeamIdx, J, SS_B_AB) > 0 Then
                        batAvg% = 100 * batRating(luTeamIdx, J, SS_B_H) / batRating(luTeamIdx, J, SS_B_AB) - 27
                        If batAvg% < 0 Then batAvg% = 3 * batAvg%

                        JJ! = 3 * R! + batAvg%

                        If JJ! > theMax! Then
                            theMax! = JJ!
                            TPlayNo = J
                            UFldNo = L
                            validPositionFound = 1
                        End If
                    End If

                Case USE_OPS
                    ' Prevent division by zero for both calculations
                    If batRating(luTeamIdx, J, SS_B_AB) > 0 And (batRating(luTeamIdx, J, SS_B_AB) + batRating(luTeamIdx, J, SS_B_W)) > 0 Then
                        slug! = (batRating(luTeamIdx, J, SS_B_H) + batRating(luTeamIdx, J, SS_B_2B) + batRating(luTeamIdx, J, SS_B_3B) * 2 + batRating(luTeamIdx, J, SS_B_HR) * 3) / batRating(luTeamIdx, J, SS_B_AB)
                        oba! = (batRating(luTeamIdx, J, SS_B_H) + batRating(luTeamIdx, J, SS_B_W)) / (batRating(luTeamIdx, J, SS_B_AB) + batRating(luTeamIdx, J, SS_B_W))
                        ops! = slug! + oba!

                        If ops! >= theMax! Then
                            theMax! = ops!
                            TPlayNo = J
                            UFldNo = L
                            validPositionFound = 1
                        End If
                    End If

                Case USE_OBA_SB
                    ' Fix syntax error and prevent division by zero
                    denominator! = batRating(luTeamIdx, J, SS_B_AB) + batRating(luTeamIdx, J, SS_B_W) + batRating(luTeamIdx, J, SS_B_CS)
                    If denominator! > 0 Then
                        oba! = (batRating(luTeamIdx, J, SS_B_H) + batRating(luTeamIdx, J, SS_B_W) + batRating(luTeamIdx, J, SS_B_SB)) / denominator!

                        If oba! >= theMax! Then
                            theMax! = oba!
                            TPlayNo = J
                            UFldNo = L
                            validPositionFound = 1
                        End If
                    End If

            End Select

        End If 'Done checking found

    Next L

    ' COMPREHENSIVE FALLBACK LOGIC
    If validPositionFound = 0 Then

        ' FALLBACK STEP 1: Find any unassigned lineup position
        For fallbackPos = 1 To 9
            ' Skip pitcher spot if no DH
            If desigHit = 0 And fallbackPos = 1 Then fallbackPos = 2
            If fallbackPos > 9 Then Exit For

            positionTaken = 0
            For checkB = 1 To B - 1
                If bestBatterFPos(checkB) = fallbackPos Then
                    positionTaken = 1
                    Exit For
                End If
            Next checkB

            If positionTaken = 0 Then
                UFldNo = fallbackPos
                Exit For
            End If
        Next fallbackPos

        ' FALLBACK STEP 2: Find any unassigned player
        ' Start from player 3 to avoid key players, work backwards from 23 for bench players
        For candidatePlayer = 22 To 3 Step -1
            playerAlreadyUsed = 0
            For checkB = 1 To B - 1
                If bestBatterIdx(checkB) = candidatePlayer Then
                    playerAlreadyUsed = 1
                    Exit For
                End If
            Next checkB

            If playerAlreadyUsed = 0 Then
                TPlayNo = candidatePlayer
                Exit For
            End If
        Next candidatePlayer

        ' FALLBACK STEP 3: Ultimate safety net
        If UFldNo < 1 Or UFldNo > 9 Then
            UFldNo = 9 ' Right field - safest position
        End If

        If TPlayNo < 0 Or TPlayNo > 22 Then
            TPlayNo = B ' Use batter slot number as player
            If TPlayNo > 22 Then TPlayNo = 22
        End If

    End If

    bestBatterIdx(B) = TPlayNo
    bestBatterFPos(B) = UFldNo

End Sub


'----------------------------------------
'       SelectPitchers Subroutine
'----------------------------------------
'This subroutine walks the user through
'all available options for selecting a
'rotation of pitchers.
Sub SelectPitchers (luTeamIdx, cancelPitchers%, compRotation)

    Shared gameMgrIdx(), mgrFile(), startPitcher()

    ' *** PITCHING ROTATIONS ***
    userDone = 99
    useRotationLogic = 0

    Call PrintPitcherInfo(luTeamIdx)

    Locate 15, 57: Print "0-MANUALLY PICK STARTER"
    Locate 16, 57: Print "1-USE PRE-SET ROTATION"
    Locate 17, 57: Print "2-ENTER A NEW ROTATION"

    If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then
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
                useRotationLogic = 1
                startPitcher(luTeamIdx) = -1

            Case 1:
                'Pre-Set Rotation
                Cls

                ' Check if manager file doesn't exist OR rotation isn't set up properly
                If mgrFile(luTeamIdx) = -1 Or teamMgrProfileVal(luTeamIdx, 99) <> 999 Then

                    If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then
                        'Computer team - let it manage rotation
                        compRotation = 1
                    Else
                        'Human is managing, but no rotation - ask what to do
                        Cls
                        Print "ERROR:: ROTATION NOT FOUND"
                        Print
                        Print "DO YOU WANT TO ENTER A ROTATION? (Y/N)"

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "N" Then
                            'User doesn't want to enter rotation
                            useRotationLogic = 1
                        Else
                            'User wants to enter totation
                            Print "COMPUTER SET ROTATION? (Y/N) ";

                            Do
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                            If UCase$(I$) = "Y" Then
                                compRotation = 1
                            End If

                        End If

                    End If

                    'Setup rotation
                    Call PitchingRotations(compRotation, luTeamIdx)

                Else
                    'Valid rotation - use normal logic
                    useRotationLogic = 0
                End If

            Case 2:

                Do
                    Cls
                    Print "COMPUTER SET ROTATION? (Y/N) ";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then
                    compRotation = 1
                End If

                Call PitchingRotations(compRotation, luTeamIdx)

        End Select

        If useRotationLogic = 0 Then

            'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

            'For I = 0 To teamMgrProfileVal(luTeamIdx, 1) - 1
            '    pitchRating(luTeamIdx, teamMgrProfileVal(luTeamIdx, 3 + I), 15) = 99
            'Next

            ' ** Find starting pitcher **
            'Mark primary pitcher as available
            primaryStarterID = teamMgrProfileVal(luTeamIdx, teamMgrProfileVal(luTeamIdx, 2) + 3)
            pitchRating(luTeamIdx, primaryStarterID, SS_P_Avail) = 0

            'Check if we should use started (based on probability)
            primaryStarterChance = teamMgrProfileVal(luTeamIdx, teamMgrProfileVal(luTeamIdx, 2) + 10 + 3)

            If d100% <= primaryStarterChance Or primaryStarterChance = 0 Then
                'Use primary pitcher
                startPitcher(luTeamIdx) = primaryStarterID
            Else
                'Pick from rotation using weighted probability
                Call Rolld100(RN0)
                pitcherWeight = 0
                pitcherFound = 0
                J = 21

                'Loop through rotation slots
                Do While J <= 36 And pitcherFound = 0

                    evalPitcherID = teamMgrProfileVal(luTeamIdx, J)

                    'Check if valid pitcher is assigned
                    If evalPitcherID <> -1 And evalPitcherID >= 0 And evalPitcherID <= 21 Then

                        'Add cumulative pitcher weight
                        pitcherWeight = pitcherWeight + teamMgrProfileVal(luTeamIdx, J + 20)

                        If RN0 <= pitcherWeight Then
                            'Mark pitcher s available
                            pitchRating(luTeamIdx, evalPitcherID, SS_P_Avail) = 0
                            startPitcher(luTeamIdx) = evalPitcherID
                            pitcherFound = 1
                        End If

                    End If

                    J = J + 1

                Loop

            End If

            teamMgrProfileVal(luTeamIdx, 2) = teamMgrProfileVal(luTeamIdx, 2) + 1

            If teamMgrProfileVal(luTeamIdx, 2) = teamMgrProfileVal(luTeamIdx, 1) Then
                teamMgrProfileVal(luTeamIdx, 2) = 0
            End If

            teamMgrProfileVal(luTeamIdx, 99) = 999
            teamMgrProfileVal(luTeamIdx, 0) = 999
            mgrFile(luTeamIdx) = 0

            teamYear$ = RTrim$(diskIDs$(luTeamIdx))

            Open diskPaths$(4) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = MGR_SIZE_BYTES

            For I = 0 To 999
                Field #1, I * 2 As Q$, 2 As Q2$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
            Next

            For I = 0 To 999:
                LSet Q2$(I) = MKI$(teamMgrProfileVal(luTeamIdx, I))
            Next

            Put #1, gameMgrIdx(luTeamIdx)

            Close #1

        End If

        Do

            userDone = 99

            Call PrintPitcherInfo(luTeamIdx)

            Locate 15, 58: Print "DISABLE PITCHER (0-L)"
            Locate , 58: Print "Y-GRANT DAY OFF"
            Locate , 58: Print "Z-CONTINUE"
            Locate , 58: Print "X-ACTIVATE ALL PITCHERS"

            If autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2) Then
                I$ = "Z"
            End If

            While UCase$(I$) <> "Z" And I$ <> Chr$(27)

                I$ = GetKeyPress$

                If I$ <> Chr$(27) And UCase$(I$) <> "Z" Then

                    Call LetterToNumber(I$, I1)

                    Select Case UCase$(I$)
                        Case "Y":
                            'GRANT DAY OFF
                            If useRest = 1 Then
                                For I1 = 0 To 21
                                    If pitchRating(luTeamIdx, I1, SS_P_Rest) > 0 Then
                                        pitchRating(luTeamIdx, I1, SS_P_Rest) = pitchRating(luTeamIdx, I1, SS_P_Rest) - 1
                                    End If
                                Next
                            End If

                        Case "X":
                            'Activate all pitchers
                            For I3 = 0 To 21
                                If pitchRating(luTeamIdx, I3, SS_P_Avail) = 99 And pitchRating(luTeamIdx, I3, SS_P_Active) = 0 Then
                                    pitchRating(luTeamIdx, I3, SS_P_Avail) = 0
                                    Locate I3 + 2, 3
                                    Print pitchers$(luTeamIdx, I3)
                                End If
                            Next I3

                        Case Else:
                            If I1 <= 21 Then
                                'If a pitcher was selected, disable them
                                If pitchRating(luTeamIdx, I1, SS_P_Avail) = 0 Then
                                    pitchRating(luTeamIdx, I1, SS_P_Avail) = 99
                                    Locate I1 + 2, 3
                                    Color 0, 7: Print pitchers$(luTeamIdx, I1)
                                    Color 15, 0
                                Else
                                    If pitchRating(luTeamIdx, I1, SS_P_Avail) <> 0 Then
                                        pitchRating(luTeamIdx, I1, SS_P_Avail) = 0
                                        Locate I1 + 2, 3
                                        Color 15, 0: Print pitchers$(luTeamIdx, I1)
                                    End If
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
        userDone = 0

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

    Cls

    Print "#";
    Color teamRat_GAME(idx%, 11), teamRat_GAME(idx%, 12): Print gameTeams$(idx%);
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
            Locate , 17: Print Using " ### ### ### ## ## ### ###"; pitchRating(idx%, I, SS_P_IP); pitchRating(idx%, I, SS_P_H); pitchRating(idx%, I, SS_P_G); pitchRating(idx%, I, SS_P_GS); pitchRating(idx%, I, SS_P_Rest); pitchRating(idx%, I, SS_P_BB); pitchRating(idx%, I, SS_P_K);
            '                           W                                                       L  ERA
            Locate , 43: Print Using "  ##"; pitchRating(idx%, I, 1);: Print "-";: Print Using "## ##.##"; pitchRating(idx%, I, SS_P_L); pitchRating(idx%, I, SS_P_ERA100) / 100
        End If

    Next I

    If userDone <= 0 Then

        Call PitchingStarter(idx%)

    End If

End Sub


'----------------------------------------
'        PitchingStarter routine
'----------------------------------------
'This subroutine works with the user to
'choose a starting pitcher. The choices
'will have already been printed to the
'screen from other routines.
Sub PitchingStarter (luTeamIdx)

    Shared startPitcher()

    Do

        Locate 19, 58: Print "SELECT STARTER"

        If startPitcher(luTeamIdx) <> -1 Then

            pitchIdx = startPitcher(luTeamIdx)

        Else

            Do
                I$ = GetKeyPress$
                Call LetterToNumber(I$, pitchIdx)
            Loop Until pitchIdx <= 21 And pitchRating(luTeamIdx, pitchIdx, SS_P_Avail) <= 0

        End If

        Locate 20, 58

        Print pitchers$(luTeamIdx, pitchIdx); " "; handed$(pitchRating(luTeamIdx, pitchIdx, SS_P_Throws) + 2)

        Print

        If (autoPlay = 1 And ((playerMode = 1 And compTeam = luTeamIdx) Or playerMode = 2)) Then

            I$ = "N"

        Else

            Do
                Locate 21, 58: Print "ANY CHANGE? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                Locate 20, 58
                Print Space$(16)
                startPitcher(luTeamIdx) = -1
            End If

        End If

    Loop Until UCase$(I$) = "N"

    currPitcher(luTeamIdx) = pitchIdx
    gamePitcher(luTeamIdx) = 1
    pitchRating(luTeamIdx, pitchIdx, SS_P_Avail) = gamePitcher(luTeamIdx)

    pitcherStam(luTeamIdx, 0) = pitcherMaxStam%(luTeamIdx, currPitcher(luTeamIdx))
    pitcherStam(luTeamIdx, 1) = 3

    If gamePitcher(luTeamIdx) > 1 Then
        pitcherStam(luTeamIdx, 1) = 0
    End If

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

    Dim currFielder
    '1 = Pitcher
    '2 = Catcher
    '3 = First Base
    '4 = Second Base
    '5 = Third Base
    '6 = Shortstop
    '7 = Left Field
    '8 = Center Field
    '9 = Right Field

    Dim baseRunners, errorOnPlay%
    'baseRunners = bitmask (binary)
    '1 = runner on 1st
    '2 = runner on 2nd
    '4 = runner on 3rd

    'Combined values:
    '0 = bases empty
    '1 = man on 1st
    '2 = man on 2nd
    '3 = 1st + 2nd (1+2)
    '4 = man on 3rd
    '5 = 1st + 3rd (1+4)
    '6 = 2nd + 3rd (2+4)
    '7 = bases loaded (1+2+4)

    Dim lastBatIdx(0 To 1)

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11, "=== "; gameTeams$(0) + " vs. " + gameTeams$(1); " ==="
    End If

    'Declare all variables for the modules
    Dim F1$(10)
    Dim PH%(0 To 22, 0 To 1)

    Shared grounderDir$()

    Shared buntTriple(), setupUsed(), closerUsed()
    Shared umpBBadj(), umpSZadj(), umpRunAdj()

    VV(0) = 1
    VV(1) = 1

    VV1(0) = 9
    VV1(1) = 9

    Call HITTINGLINES

    Call PITSTAM(0)
    Call PITSTAM(1)

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
    'b9d1 = fielder(D, 1)
    'b9d2 = fielder(D, 2)

    Erase nbrOuts
    Erase b1Pitcher, b2Pitcher, bRunner
    Erase runnerOnBase, stealPhase

    errorOnPlay% = 0
    runnerHeld = 0
    baseRunners = 0

    leftOnBase(P) = leftOnBase(P) + MO
    MO = 0

    If P = 1 Then
        'Top of the inning
        INNING% = INNING% + 1
        Erase closerZone
    End If

    'Switch posession
    P = 1 - P
    D = 1 - P

    '7th Inning Stretch
    If INNING% = 7 And P = 1 Then

        If teamRat_GAME(1, 13) = 8 And teamYears(P) > 1975 And teamYears(P) < 1998 Then
            'Wrigley Field + Harry Caray (died before 1998 season)
            Call WINDEX
            Call PBP("ALL RIGHT..."): Call PBP("LEMME HEAR YA..."): Call PBP("A ONE, A TWO...")
        End If

    End If

    'Check scores / innings
    '------------------------------
    If (INNING% > 9 And D = 1 And gameScore(1, 0) <> gameScore(0, 0)) Or _
        (INNING% >= 9 And P = 1 And gameScore(P, 0) - gameScore(D, 0) > 0) Or _
        INNING% = 28 Then
        'After 9th inning, score not tied, etc..
        'End of game!
        GoTo LGameOver
    End If

    ' ** Ghost Runner Check **
    If INNING% > 9 And ghostRunOpt = 1 Then

        'Runner = person who batted last
        runnerOnBase(2) = lastBatIdx(P)

        'Assign to 2nd base
        baseRunners = 2

        'If runner is pitcher, check if we should sub
        If fldPos(P, lastBatIdx(P)) = 1 Then
            Call PINCHRUNNER(scoreDiff, baseRunners, I2, nbrBalls, nbrStrikes, MO, INFPOS%, runnerUsed, madeSub)
        End If

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
        Call DisplayBallField_Gfx(nbrBalls, nbrStrikes)
    Else
        Call DisplayBallField(nbrBalls, nbrStrikes)
    End If

    For IJ0% = 0 To 22

        'Player is becoming available
        If batRating(D, IJ0%, SS_B_Avail) >= 40 And batRating(D, IJ0%, SS_B_Avail) <= 49 Then

            I1 = IJ0%
            I2 = batRating(D, IJ0%, SS_B_Avail) - 40

            'Make player available and insert into lineup / field
            batRating(D, I1, SS_B_Avail) = 1
            lineupPlayer(D, I2) = I1
            fielder(D, fldPos(D, I2)) = I1

            'Track game roster with substitution
            playersUsed(D) = playersUsed(D) + 1
            gameRoster(D, 0, playersUsed(D)) = lineupPlayer(D, I2)
            gameRoster(D, 1, playersUsed(D)) = fldPos(D, I2)
            gameRoster(D, 2, playersUsed(D)) = (I2 * 10) + (playersUsed(D) - 9)

            W5 = D
            W6 = D

            VV(D) = I2
            VV1(D) = I2

            'Iterate over 4 possible positions
            For xx = 22 To 25

                If fldPos(D, I2) = batRating(D, I1, xx) Then
                    'Position match?
                    'Assign defensive ratings based on position
                    batRating(D, I1, SS_B_Arm) = batRating(D, I1, 33 + (xx - 22) * 2)
                    batRating(D, I1, SS_B_Range) = batRating(D, I1, 34 + (xx - 22) * 2)
                    batRating(D, I1, SS_B_FldPct) = batRating(D, I1, xx + 4)
                    Exit For
                End If

            Next

            Call DisplayGameState(nbrBalls, nbrStrikes, MO, INFPOS%)

            VV(D) = 1
            VV1(D) = 9

        End If

    Next

    scoreDiff = gameScore(D, 0) - gameScore(P, 0)

    If Not (teamMgrProfileVal(D, 560) <> 999 Or INNING% < 8 Or playerMode = 0 Or playerMode = 1 And P = compTeam) Then

        If Environ$("MGRLOG") = "TRUE" Then
            Print #11, "!! Team is using a DEFENSIVE strategy !!"
        End If

        For IJ0% = 1 To 4

            If teamMgrProfileVal(D, 560 + IJ0%) <> -1 And batRating(D, teamMgrProfileVal(D, 564 + IJ0%), SS_B_Avail) = 0 Then

                Call Rolld100(RN0)

                If scoreDiff >= teamMgrProfileVal(D, 572 + IJ0%) And _
                    fielder(D, teamMgrProfileVal(D, 576 + IJ0%)) = teamMgrProfileVal(D, 560 + IJ0%) And _ 
                    d100% <= teamMgrProfileVal(D, 568 + IJ0%) Then

                    I1 = teamMgrProfileVal(D, 564 + IJ0%)

                    I2 = 1
                    Do While I2 <= 9 And fldPos(D, I2) <> teamMgrProfileVal(D, 576 + IJ0%)
                        I2 = I2 + 1
                    Loop

                    'Make player available and insert into lineup / field
                    batRating(D, I1, SS_B_Avail) = 1
                    lineupPlayer(D, I2) = I1
                    fielder(D, fldPos(D, I2)) = I1

                    'Track game roster with substitution
                    playersUsed(D) = playersUsed(D) + 1
                    gameRoster(D, 0, playersUsed(D)) = lineupPlayer(D, I2)
                    gameRoster(D, 1, playersUsed(D)) = fldPos(D, I2)
                    gameRoster(D, 2, playersUsed(D)) = (I2 * 10) + (playersUsed(D) - 9)

                    W5 = D
                    W6 = D

                    VV(D) = I2
                    VV1(D) = I2

                    For xx = 22 To 25
                        If fldPos(D, I2) = batRating(D, I1, xx) Then
                            batRating(D, I1, SS_B_Arm) = batRating(D, I1, 33 + (xx - 22) * 2)
                            batRating(D, I1, SS_B_Range) = batRating(D, I1, 34 + (xx - 22) * 2)
                            batRating(D, I1, SS_B_FldPct) = batRating(D, I1, xx + 4)
                            Exit For
                        End If
                    Next

                    Call DisplayGameState(nbrBalls, nbrStrikes, MO, INFPOS%)

                    VV(D) = 1
                    VV1(D) = 9

                End If

            End If

        Next

    End If

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11, "::: Status of pitcher taking the mound: "; fielder(D, 1)
    End If

    'Inured pitcher
    If fielder(D, 1) = 99 Then

        If playerMode = 0 Or (playerMode = 1 And D <> compTeam) Then

            Cls

            If useVGA = 0 Then Screen menu&

            P9 = D
            nbrStrikes = 0

            currLineupSlot(P) = currLineupSlot(P) + 1
            If currLineupSlot(P) = 10 Then
                currLineupSlot(P) = 1
            End If

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
                    If bestBatterFPos(P9) = I Then Color 9
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

        Else

            'Will choose a new pitcher because pitcher was pinch hit for
            'Don't check minimum batters - pinch hit scenario
            Call RELIEVERS(pinchHitterFlag, newPitcherFlag, baseRunners)

        End If

    End If


    LNextBatter0:
    sacHit = 0
    errorOnPlay% = 0
    wasStrikeout = 0
    currLineupSlot(P) = currLineupSlot(P) + 1
    INFPOS% = INF_BACK
    nbrStrikes = 0

    If currLineupSlot(P) = 10 Then
        currLineupSlot(P) = 1
    End If

    '--- Start of a new sub
    LNextBatter1:
    Call WHOSON1ST(baseRunners)
    Call PLAYERNAME

    If Environ$("PBPLOG") = "TRUE" Then
        Call DumpIt(baseRunners, offChoice, D2)
    End If

    If useVGA = 1 Then
        Call MAINSCREEN_G(nbrBalls, nbrStrikes, MO, INFPOS%)
    Else
        Call MAINSCREEN(nbrBalls, nbrStrikes, MO)
    End If

    offChoice = 0
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
                Call DisplayInningScore_G(I2, I0)
            Else
                Call DisplayInningScore(I2, I0)
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

        errorOnPlay% = 0

        LContinueBatter2:
        If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then
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
        If useVGA = 1 Then
            _PutImage (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176), backupScreen&, imageScreen&, (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176)
        End If

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
            offChoice = OFF_HIT_AWAY
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
            Call BASEONBALLS(S2%, D2, baseRunners)
            GoTo LFreeBase

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
            _PutImage (51 * FontColAdj, 416)-Step(14 * FontColAdj, 16), backupScreen&, imageScreen&, (51 * FontColAdj, 416)-Step(14 * FontColAdj, 16)

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color &HF0000000

            'Print infield status
            PrintOnGfx 51, 26, infStatus$, imageScreen&

            'Printing white on black
            Color &HFFFFFFFF
            _PrintMode _FillBackground , imageScreen&
        Else
            'Color 15, 2
            'Locate 29, 46: Print infStatus$
        End If

        GoTo LContinueBatter

    End If

    '----------------------------------------
    ' OFFENSE OPTIONS MENU
    '----------------------------------------
    LOffenseOptions:

    Do

        If ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then

            JM = lineupPlayer(P, currLineupSlot(P))

            If pinchHitterFlag <> 1 Then

                scoreDiff = gameScore(P, 0) - gameScore(D, 0)

                If fldPos(P, currLineupSlot(P)) <> 1 Then

                    If teamMgrProfileVal(P, 847) = 999 Then

                        If Environ$("MGRLOG") = "TRUE" Then
                            Print #11, "!! Team is using a PINCH HITTING for POSITION PLAYER strategy !!"
                        End If

                        'PH FOR POS PLAYER CASES
                        MOB% = 0

                        For I = 1 To 3
                            If runnerOnBase(I) > 0 Then
                                MOB% = MOB% + 1
                            End If
                        Next

                        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                            I7! = 0
                            I6! = 0
                        Else
                            I7! = CInt(batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                            I6! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                        End If

                        For I = 0 To 2
                            If scoreDiff <= teamMgrProfileVal(P, 849 + I * 6) * -1 And _
                                INNING% >= teamMgrProfileVal(P, 848 + I * 6) And _
                                I6! <= teamMgrProfileVal(P, 851 + I * 6) And _ 
                                I7! <= teamMgrProfileVal(P, 852 + I * 6) And _
                                d100% <= teamMgrProfileVal(P, 853 + I * 6) And _ 
                                MOB% >= teamMgrProfileVal(P, 850 + I * 6) Then
                                GoTo LPhForPosition
                            End If
                        Next

                    Else

                        If batRating(P, JM, SS_B_AB) <> 0 Then
                            GoTo LPhForPosition
                        End If

                        If (batRating(P, JM, SS_B_H) / batRating(P, JM, SS_B_AB) * 1000 <= 233 And _ 
                            batRating(P, JM, SS_B_HR) < 15) And _
                           ((INNING% > 6 And scoreDiff < -2) Or (INNING% >= 8 And scoreDiff <= 0) Or (INNING% >= 9 And scoreDiff = 0)) Then
                            GoTo LPhForPosition
                        End If

                    End If

                Else
                    ' Batter is the pitcher

                    For I = 0 To 21

                        If pitchRating(P, I, SS_P_Avail) = 0 Then

                            If teamMgrProfileVal(P, 590) = 999 Then

                                If Environ$("MGRLOG") = "TRUE" Then
                                    Print #11, "!! Team is using a PINCH HITTER for PITCHER strategy !!"
                                End If

                                'PH FOR PITCHER WITH PROFILE
                                MOB% = 0

                                For indx = 1 To 3
                                    If runnerOnBase(indx) > 0 Then
                                        MOB% = MOB% + 1
                                    End If
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
                                    If INNING% >= teamMgrProfileVal(P, 591 + indx * 6) And _ 
                                        scoreDiff <= teamMgrProfileVal(P, 592 + indx * 6) * -1 And _ 
                                        nbrOuts(0) <= teamMgrProfileVal(P, 594 + indx * 6) And _ 
                                        I7! <= teamMgrProfileVal(P, 595 + indx * 6) And _
                                        d100% <= teamMgrProfileVal(P, 596 + indx * 6) And _ 
                                        MOB% >= teamMgrProfileVal(P, 593 + indx * 6) Then
                                        GoTo LPhForPitcher
                                    End If
                                Next

                            Else

                                If (INNING% > 4 And INNING% < 7 And ((scoreDiff < -2 And gameScore(P, 0) > 0) Or scoreDiff < -3)) Or _
                                   (INNING% > 6 And scoreDiff < 0) Or _
                                   (scoreDiff = 0 And INNING% > 6 And INNING% < 9 And baseRunners > 1) Or _
                                   (scoreDiff = 0 And INNING% >= 9 And (baseRunners > 0 Or nbrOuts(0) = 0)) Or _
                                   (pitchRating(P, currPitcher(P), SS_P_IP) / pitchRating(P, currPitcher(P), SS_P_G) <= 2 And _
                                    teamPitStats(P, currPitcher(P), 0) >= 6) Then                                    
                                    GoTo LPhForPitcher
                                End If

                            End If

                            Exit For

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

        offChoice = Val(I$)

    Loop Until offChoice >= OFF_HIT_AWAY And offChoice <= OFF_PERSONNEL

    LExecuteOffense:
    Select Case offChoice

        Case 1, 4 'Hit away, Bunt

            If D2 <> DEF_PITCH_AROUND Then

                GoTo LWeirdPlay

            Else

                Call WINDEX

                If offChoice = OFF_BUNT Or (offChoice = OFF_HIT_AWAY And d100% <= 50) Then

                    GoTo LWeirdPlay

                Else

                    Call BASEONBALLS(S2%, D2, baseRunners)
                    GoTo LFreeBase

                End If

            End If

        Case 2 'Hit and Run
            If nbrOuts(0) = 2 Or (baseRunners <> 1 And baseRunners <> 4 And baseRunners <> 5) Then
                GoTo LNextBatter1
            End If

        Case 3 'Steal
            GoTo LSteal

        Case 5 'Personnel
            P9 = P
            W5 = -1
            W6 = -1
            GoTo LPersonnelMenu

    End Select


    LHitAndRun:
    If baseRunners = 4 Then
        pickedOffBase = 3
    Else
        pickedOffBase = 2
    End If

    Call Rolld100(pickoffChance)

    If pickoffChance < 6 Then

        Call PICKEDOFF(pickedOffBase)

        isPutout = 1
        baseMove(pickedOffBase - 1) = 10

        Call TrackOuts(1)

        currFielder = 1

        Call CreditAssist(currFielder)
        Call ADVANCEMENT(S2%, INFPOS%, currFielder, currFielder, errorOnPlay%, SAH%)
        Call CreditInfieldPutout(I3, FB2P%, AST%)

        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

        If nbrOuts(0) >= 3 Then

            currLineupSlot(P) = currLineupSlot(P) - 1
            isPutout = 0
            GoTo LCheckForOptionsOrQuit

        End If

    End If

    If isPutout = 1 Then
        isPutout = 0
        GoTo LNextBatter1
    End If

    Call WINDEX

    I3 = Int(Rnd(1) * 1000) + 1
    I9 = Int(Rnd(1) * 1000) + 1

    If I3 > 350 Then
        Call PBP("Runner(s) going")
        Call DELAY
    Else
        Call PBP("Count not favorable...hit away")
        Call DELAY
        offChoice = OFF_HIT_AWAY
        GoTo LWeirdPlay
    End If

    If I9 <= (game_batRating(P, GM_K, currLineupSlot(P)) * 2 * .33) Then

        'Strike on stolen base attempt
        nbrStrikes = nbrStrikes + 1
        Call PBP(player$(0) + " swings and misses")
        Call DELAY

        If (autoPlay <> 1 Or playerMode <> 2) And sndOpt = 1 And miss& And d100% < 10 Then
            _SndPlay miss&
        End If

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
        offChoice = OFF_HIT_AWAY
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
    If delayGame = 1 Then

        'Default value for RAREPLAYS
        H6% = 21

        Call WINDEX

        haltTime = getRainDelayMinutes%

        Color 0, 7
        Call PBP("The game has been halted by rain")

        For idx = 1 To 2:
            Call DELAY
        Next

        gameHalted = 1

        Call PBP("Rain delay lasted" + Str$(haltTime) + " minutes")
        Call DELAY

        Select Case haltTime

            Case 1 To 59
                stiffArmAdj = 0
                Call DELAY

            Case 60 To 119
                stiffArmAdj = 1
                Call DELAY
                Call DELAY

            Case Is >= 120
                stiffArmAdj = 2
                Call DELAY
                Call DELAY
                Call DELAY

        End Select

        If stiffArmAdj > 0 Then
            Call PBP("Pitchers arms have stiffened some.")
            Call PBP("They will now be less effective.")
            Call DELAY
        End If

        For idx = 0 To 1:
            pitcherStam(idx, 0) = pitcherStam(idx, 0) - stiffArmAdj
        Next

        delayGame = 0

    Else

        Call RAREPLAYS(INFPOS%, currFielder, errorOnPlay%, H6%, injPlayer, baseRunners)

    End If


    If injPlayer = 1 Then

        injPlayer = 0

        'If human player and pitcher has not been substituted
        If (playerMode = 0 Or (playerMode = 1 And D <> compTeam)) And _
            pitchRating(D, currPitcher(D), SS_P_Avail) = 99 Then
            'Force a substitution
            GoTo LPersonnelMenu
        End If

        'If batter has not been substituted
        'If batRating(X, Y, SS_B_Avail) = 99 Then
        'GoTo LChangeBatterFielder
        'End If

    End If

    Call DisplayGameState(nbrBalls, nbrStrikes, MO, INFPOS%)

    'This is the H6% from RAREPLAYS
    Select Case H6%

        Case 1 To 4, 6 To 8, 10 To 24
            GoTo LContinueBatter

        Case 5, 25 To 29
            GoTo LCheckForOptionsOrQuit

        Case 9
            Call BASEONBALLS(S2%, D2, baseRunners)
            GoTo LFreeBase

        Case 45
            'Why are we getting a free base here??
            GoTo LFreeBase

        Case Else
            ' Handles 30 to 44 and 46+ (anything not covered above)
            GoTo LContinueBatter

    End Select


    '----- NOODLE -----

    LWeirdPlay:
    'Rare plays, wild pitches, interference, etc.
    Call Rolld100(weatherRoll)

    '2% chance during rainy weather
    ' -- OR _-
    '5% chance after rain delay called AND game not delayed
    If ((precipType < 4 And weatherRoll = 2) Or (rainDelay = 1 And weatherRoll <= 5)) And INNING% >= 2 And gameHalted = 0 Then
        delayGame = 1
        GoTo LRarePlays
    End If

    chanceRoll = Int(Rnd(1) * 1000) + 1

    If chanceRoll <= 4 And useInj = 1 Then
        GoTo LRarePlays
    End If

    drawnInfieldChance = 0

    chanceRoll = Int(Rnd(1) * 1000) + 1

    If nbrOuts(0) = 0 And baseRunners = 4 And INFPOS% <  INF_IN And chanceRoll = 768 Or _
       nbrOuts(0) = 0 And baseRunners = 4 And INFPOS% >= INF_IN And chanceRoll = 123 Or _
       nbrOuts(0) = 0 And baseRunners = 7 And INFPOS% >= INF_IN And chanceRoll = 245 Or _
       nbrOuts(0) = 0 And baseRunners = 7 And INFPOS% <  INF_IN And chanceRoll = 381 Then
        Call TRIPLEPLAY
        GoTo LCheckForOptionsOrQuit
    End If

    H7% = Int(Rnd(1) * 7500) + 1

    Call WINDEX

    If chanceRoll <= 4 Then

        ' *** hit by pitch ***
        Call HITBYPITCH(S2%, baseRunners)

        LFreeBase:
        'Batter advances to 1st base
        baseMove(0) = 1
        baseMove(1) = 0
        baseMove(2) = 0
        baseMove(3) = 0

        '1st base loaded
        If runnerOnBase(1) > 0 Then

            baseMove(1) = 1

            '2nd base loaded
            If runnerOnBase(2) > 0 Then

                baseMove(2) = 1

                'All bases loaded
                If runnerOnBase(3) > 0 Then
                    baseMove(3) = 1
                    If currFielder = 2 And S2% > 2 Then
                        baseMove(3) = 11
                    End If
                End If

            End If

        End If

        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
        GoTo LNextBatter0

    End If

    '1 to 6
    If chanceRoll <= 6 Then

        ' *** BALK ***
        If baseRunners = 0 Then
            ' *** Fouled out of play ***
            Call FOULBALL
            GoTo LContinueBatter
        Else
            S2% = EVENT_BALK

            Call WINDEX

            Call Rolld100(balkRoll)
            rndUmp = Int(Rnd(1) * 4) + 1

            Select Case balkRoll
                Case Is <= 33
                    pbpString$ = "a balk has been called"
                Case 34 To 66
                    Call PBP("and a BALK is called")
                    pbpString$ = pitchers$(D, currPitcher(D)) + " dropped the ball"
                Case 67 To 100
                    pbpString$ = gameUmpire$(rndUmp) + " calls a balk on " + pitchers$(D, currPitcher(D))
            End Select

            Call PBP(pbpString$)

            baseMove(1) = 1
            baseMove(2) = 1
            baseMove(3) = 1

            If (runnerOnBase(1) = 0 Or nbrOuts(0) > 1) And balkRoll = 9 And wasStrikeout = 1 Then
                'Pitch was actually hit; batter advances also
                baseMove(0) = 1
                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                GoTo LCheckForOptionsOrQuit
            Else
                baseMove(0) = 0
                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                GoTo LNextBatter1
            End If

        End If

    End If

    '7 to 11 OR 7 to 200 And catcher is playing out of position
    If chanceRoll <= 11 Or chanceRoll <= 200 And batRating(D, fielder(D, 2), SS_B_Pos1) <> 2 And batRating(D, fielder(D, 2), SS_B_Pos2) <> 2 And batRating(D, fielder(D, 2), SS_B_Pos3) <> 2 And batRating(D, fielder(D, 2), SS_B_Pos4) <> 2 Then

        LPassedBall:
        If baseRunners = 0 And wasStrikeout <> 1 Then
            ' *** Fouled out of play ***
            Call FOULBALL
            GoTo LContinueBatter
        End If

        S2% = EVENT_PB
        Call WINDEX
        Call DELIVERY

        If d100% <= 50 Then
            Call PBP("pitch gets by " + batters$(D, fielder(D, 2)))
        Else
            Call PBP("off " + batters$(D, fielder(D, 2)) + "'s glove")
            Call PBP("it gets away from him")
        End If

        Call PBP("It's ruled a passed ball")

        'nbrOuts(1) = number outs without errors (for recording unearned runs)
        If (runnerOnBase(1) = 0 Or nbrOuts(0) > 1) And balkRoll = 9 And wasStrikeout = 1 Then
            nbrOuts(1) = nbrOuts(1) + 1
        End If

        13222 '
        'Wild Pitch / Passed Ball Outcome
        'This is a failure of defense so no
        'base running adjustments are applied
        RN3 = Int(Rnd(1) * 10) + 1

        'Runner on 1st and/or 3rd, Hit and Run, random # within speed rating
        If offChoice = OFF_HIT_N_RUN And RN3 < batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run) And _ 
            (baseRunners = 1 Or baseRunners = 5) Then
            baseMove(0) = 0
            baseMove(1) = 2
            baseMove(2) = 2
            baseMove(3) = 1
            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
            GoTo LNextBatter1
        End If

        13223 '
        baseMove(1) = 1
        baseMove(2) = 1
        baseMove(3) = 1

        'Balk
        If (runnerOnBase(1) = 0 Or nbrOuts(0) > 1) And balkRoll = 9 And wasStrikeout = 1 Then
            'Pitch was hit by batter; they advance base also
            baseMove(0) = 1
            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
            GoTo LCheckForOptionsOrQuit
        Else
            baseMove(0) = 0
            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
            GoTo LNextBatter1
        End If

    End If

    If chanceRoll <= 19 Then

        ' *** WILD PITCH ***
        If baseRunners = 0 And wasStrikeout <> 1 Then
            ' *** Fouled out of play ***
            Call FOULBALL
            GoTo LContinueBatter
        End If

        Call WILDPITCH(S2%)

        GoTo 13222

    End If

    If H7% = 27 Then

        S2% = EVENT_INTERFERENCE
        Call WINDEX
        Call PBP(batters$(D, fielder(D, 2)) + " is called for interference!!")
        Call PBP(player$(0) + " trots to first base")
        GoTo LFreeBase

    End If

    If H7% = 28 Then

        Call WINDEX
        Call PBP(player$(0) + " is out for obstruction!")

        currFielder = 2
        Call CreditRegularPutout(currFielder)

        Call TrackOuts(1)

        If fldPos(P, currLineupSlot(P)) <> 1 Then
            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Else
            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        End If

        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

        GoTo LCheckForOptionsOrQuit
    End If

    Select Case teamYears(1)

        Case Is <= 1919
            bbEraAdj = 20 '18

        Case 1920 To 1969
            bbEraAdj = 25

        Case 1970 To 1979
            bbEraAdj = 35

        Case 1980 To 2000
            bbEraAdj = 38

        Case Is >= 2001
            bbEraAdj = 23

    End Select

    If chanceRoll <= game_batRating(P, GM_BB, currLineupSlot(P)) + bbEraAdj + umpBBadj(4) Then
        S2% = EVENT_INTERFERENCE
        Call BASEONBALLS(S2%, D2, baseRunners)
        GoTo LFreeBase
    End If

    If offChoice = OFF_BUNT Then
        GoTo LBunt
    End If

    'Determine HIT
    chanceRoll = Int(Rnd(1) * 1000) + 1

    drawnInfieldRoll = Int(Rnd(1) * 1000) + 1

    hitChanceAdj = 0

    'Hit and Run
    If offChoice = OFF_HIT_N_RUN Then
        hitChanceAdj = -5
    End If

    'Drawn infield
    If INFPOS% = INF_IN Then
        drawnInfieldChance = 70
        hitChanceAdj = hitChanceAdj + 10
    End If

    'Batter has 2 strikes
    If nbrStrikes = 2 Then
        hitChanceAdj = hitChanceAdj - 15
    End If

    If INFPOS% = INF_CORNERS_IN Then
        drawnInfieldChance = 20
    End If

    If INNING% > 5 And _ 
        pitcherStam(D, 0) <= teamPitStats(D, currPitcher(D), 4) + teamPitStats(D, currPitcher(D), 1) And _
        pitcherStam(D, 1) <= teamPitStats(D, currPitcher(D), 2) Then
        'Tiring pitcher
        hitChanceAdj = hitChanceAdj + 20
    End If

    Select Case P

        Case 0
            hitChanceAdj = hitChanceAdj - 2

        Case 1
            'Home field advantage
            hitChanceAdj = hitChanceAdj + 2

    End Select

    Call WINDEX

    If drawnInfieldRoll <= drawnInfieldChance Then
        Call PBP("It gets through the drawn-in infield!")
        Call DELAY
        GoTo 13063
    End If

    'Assign a fixed "boost" that is
    'needed for simulating accurate
    'batting %'s and hits.
    Select Case teamYears(1)

        Case Is <= 1919
            soEraAdj = 9

        Case 1920 To 1969
            soEraAdj = -11

        Case 1970 To 1979
            soEraAdj = 9

        Case 1980 To 2000
            soEraAdj = 7

        Case Is >= 2001
            soEraAdj = 7

    End Select


    ' *********************
    ' *   AT-BAT RESULT   *
    ' *********************
    'Track # of batters this team has faced
    '(this gets reset when a new reliever comes in)
    'This is used to help enforce the 3-batter rule in 2020+
    battersFaced(D, currPitcher(D)) = battersFaced(D, currPitcher(D)) + 1

    batterAvg = game_batRating(P, GM_BAvg, currLineupSlot(P))

    ' *** NOT a base hit ***
    If chanceRoll > batterAvg + hitChanceAdj - 65 Then

        ' *** check for strikeout ***
        chanceRoll = Int(Rnd(1) * 1000) + 1
        strikeoutRating = Int(game_batRating(P, GM_K, currLineupSlot(P)) * 1.0)

        If chanceRoll <= strikeoutRating + umpSZadj(4) + soEraAdj Then

            LStrikeout:
            Call WINDEX
            Call STRIKEOUT(nbrStrikes)

            currFielder = 2
            Call CreditRegularPutout(currFielder)

            Call TrackOuts(1)

            wasStrikeout = 1

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_K)
                If offChoice = OFF_HIT_N_RUN Then
                    nbrStrikes = 3
                End If
            End If

            Call INCPITDEF(D, currPitcher(D), LG_P_OUT)
            Call INCPITDEF(D, currPitcher(D), LG_P_K)

            If fldPos(P, currLineupSlot(P)) = 1 Then
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                Call INCPITOFF(P, currPitcher(P), LG_PB_K)
                If offChoice = OFF_HIT_N_RUN Then
                    nbrStrikes = 3
                End If
            End If

            If offChoice = OFF_HIT_N_RUN And nbrOuts(0) < 3 Then
                wasStrikeout = 7
                GoTo LSteal
            End If

            ' *** WILD PITCH | PASSED BALL ON STRIKEOUT
            RN0 = Int(Rnd(1) * 600) + 1

            If ((runnerOnBase(1) = 0 And wasStrikeout = 1) Or nbrOuts(0) = 3) And RN0 = 9 Then

                'Remove outs from record
                nbrOuts(0) = nbrOuts(0) - 1
                nbrOuts(1) = nbrOuts(1) - 1

                teamPitStats(D, currPitcher(D), 0) = teamPitStats(D, currPitcher(D), 0) - 1
                oppPitStats(D, currPitcher(D), 0) = oppPitStats(D, currPitcher(D), 0) - 1
                teamBatStats(D, fielder(D, 2), 16) = teamBatStats(D, fielder(D, 2), 16) - 1
                oppBatStats(D, fielder(D, 2), 16) = oppBatStats(D, fielder(D, 2), 16) - 1

                Call Rolld100(I1)

                If I1 < 77 Then

                    ' *** WILD PITCH ***
                    If baseRunners = 0 And wasStrikeout <> 1 Then
                        ' *** Fouled out of play ***
                        Call FOULBALL
                        GoTo LContinueBatter
                    Else
                        Call WILDPITCH(S2%)
                        GoTo 13222
                    End If
                Else
                    GoTo LPassedBall
                End If

            End If

            GoTo LCheckForOptionsOrQuit

        Else

            ' *** ground ball for out ***
            Call Rolld100(chanceRoll)

            playerGndPct = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_GndPct) + pitchRating(D, currPitcher(D), SS_P_GndPct) - 52
            'Assumption seems to be that pitchers hit a lot more grounders
            pitcherGndPct = 23 + pitchRating(D, currPitcher(D), SS_P_GndPct)
            ballHitInAir = 0

            If chanceRoll > playerGndPct And fldPos(P, currLineupSlot(P)) <> 1 Or _
                chanceRoll > pitcherGndPct And fldPos(P, currLineupSlot(P)) = 1 Then
                'Fly ball
                ballHitInAir = 1
            End If

            If ballHitInAir = 1 Then

                'balls hit to the air
                Call Rolld100(ballResult)
                baseMove(0) = 0
                baseMove(1) = 0
                baseMove(2) = 0
                baseMove(3) = 0

                Select Case ballResult

                    Case 1 To 73
                        ' *** FLY  OUTS ***
                        Call GETOUTFIELDER(currFielder, I1)

                        If fldPos(P, currLineupSlot(P)) <> 1 Then
                            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        Else
                            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        End If

                        Call Rolld100(RN0)

                        Select Case RN0

                            Case 1 To 17

                                If RN0 <= 17 Then
                                    ' *** shallow fly ball ***
                                    S2% = EVENT_FLY_SHALLOW
                                    Call WINDEX

                                    X$ = fieldPos$(currFielder)
                                    If d100% <= 25 Then
                                        If currFielder = 7 Then X$ = "lcf"
                                        If currFielder = 9 Then X$ = "rcf"
                                    End If
                                    3
                                    Call DELIVERY

                                    Call GetPBPString(60, 1170, Y$)
                                    Call PBP(Y$ + " " + X$)

                                    Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                                    If errorOnPlay% = 1 Then
                                        GoTo LCheckForOptionsOrQuit
                                    End If

                                    GoTo 14025

                                End If

                            Case 18 To 33
                                ' *** deep fly out ***
                                S2% = EVENT_FLY_DEEP

                                Call WINDEX
                                Call LONGFLY(currFielder)

                                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                                If errorOnPlay% = 1 Then
                                    GoTo LCheckForOptionsOrQuit
                                End If

                                Call CreditRegularPutout(currFielder)
                                Call CAUGHTFLY(null, S2%)

                                If (autoPlay <> 1 Or playerMode <> 2) And sndOpt = 1 And glove& And d100% < 10 Then
                                    _SndPlay glove&
                                End If

                                Call TrackOuts(1)

                                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                                'Sacrifice Fly
                                'nbrOuts = 2 means it was 1 before we increased it
                                If nbrOuts(0) <= 2 And runnerOnBase(3) <> 0 Then

                                    'If 3rd base was not empty, runner goes home
                                    baseMove(3) = 1
                                    Call INCPITDEF(D, currPitcher(D), LG_P_SF)

                                    If fldPos(P, currLineupSlot(P)) = 1 Then
                                        Call RemovePitcherAtBat(P, currPitcher(P), LG_PB_AB)
                                        Call INCPITOFF(P, currPitcher(P), LG_PB_SF)
                                    Else
                                        Call RemoveBatterAtBat(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_SF)
                                    End If
                                Else
                                    '3rd out!
                                    GoTo LCheckForOptionsOrQuit
                                End If

                                14066 '
                                Call FlyBall_Base2Runner(errorOnPlay%, S2%, offChoice, runnerOnBase(2), currFielder, INFPOS%, stealing, baseNbr, I9, baseStatus)
                                Call FlyBall_Base1Runner(errorOnPlay%, S2%, offChoice, runnerOnBase(2), currFielder, INFPOS%, stealing, baseNbr, I9, baseStatus)

                                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                                GoTo LCheckForOptionsOrQuit

                            Case 34 To 100
                                ' *** regular fly out ***
                                S2% = EVENT_FLY_REGULAR
                                Call WINDEX
                                X$ = fieldPos$(currFielder)
                                If d100% <= 25 Then
                                    If currFielder = 7 Then X$ = "lcf"
                                    If currFielder = 9 Then X$ = "rcf"
                                End If

                                Call DELIVERY

                                range = 90
                                min = 1070
                                If (currFielder = 7 Or currFielder = 9) And d100% <= 10 Then
                                    X$ = ""
                                    range = 10
                                    min = 1160
                                End If

                                Call GetPBPString(range, min, Y$)
                                Call PBP(Y$ + " " + X$)

                                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                                If errorOnPlay% = 1 Then
                                    GoTo LCheckForOptionsOrQuit
                                End If

                                14025 '
                                Call CreditRegularPutout(currFielder)
                                Call CAUGHTFLY(null, S2%)

                                If (autoPlay <> 1 Or playerMode <> 2) And sndOpt = 1 And glove& And d100% < 10 Then
                                    _SndPlay glove&
                                End If

                                Call TrackOuts(1)

                                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                                If nbrOuts(0) >= 3 Then
                                    GoTo LCheckForOptionsOrQuit
                                End If

                                Call Rolld100(RN0)

                                'Double Play
                                If RN0 <= 33 And S2% = EVENT_FLY_SHALLOW And _
                                    (baseRunners = 1 And (offChoice = OFF_HIT_N_RUN) Or baseRunners = 5) Then

                                    baseMove(1) = 10

                                    Call TrackOuts(1)

                                    Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                                    'Print #11, "Double play"
                                    teamDPs(D) = teamDPs(D) + 1
                                    totalDPs(D) = totalDPs(D) + 1

                                    Call CreditAssist(currFielder)

                                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                                    GoTo LCheckForOptionsOrQuit

                                End If

                                '3rd base runner attempt to score
                                If runnerOnBase(3) > 0 Then
                                    baseNbr = 4
                                    If Environ$("RUNLOG") = "TRUE" Then
                                        Print #11, "-- might try to score..."
                                    End If
                                    If errorOnPlay% = 0 Then
                                        Call TRYFOREXTRABASE(S2%, offChoice, stealing, baseNbr, I9, baseRunners, runnerOnBase(2), currFielder, INFPOS%, errorOnPlay%, SAH%)
                                    End If
                                End If

                                'Check if attempt to throw
                                If I9 = 1 Then
                                    'NO - simply advance
                                    I9 = 0
                                Else
                                    'Yes - determine if worth advancing
                                    Call FlyBall_Base2Runner(errorOnPlay%, S2%, offChoice, runnerOnBase(2), currFielder, INFPOS%, stealing, baseNbr, I9, baseStatus)
                                    Call FlyBall_Base1Runner(errorOnPlay%, S2%, offChoice, runnerOnBase(2), currFielder, INFPOS%, stealing, baseNbr, I9, baseStatus)
                                End If

                                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                                GoTo LCheckForOptionsOrQuit

                        End Select

                    Case 74 To 89

                        ' infield popup
                        Call GETINFIELDER(currFielder, I1, S2%, offChoice)
                        S2% = EVENT_INF_POPUP
                        Call WINDEX

                        If currFielder = 1 Then
                            currFielder = 2
                            player$(4) = batters$(D, fielder(D, currFielder))
                        End If
                        If (currFielder = 2 Or currFielder = 5 Or currFielder = 3) And d100% <= 30 Then
                            fo% = 1
                        End If

                        If fo% = 0 And (baseRunners = 4 Or baseRunners = 7) And nbrOuts(0) < 2 Then
                            Call PBP("INFIELD FLY RULE CALLED")
                        End If

                        Select Case fo%
                            Case 1 ' Foul ball
                                Select Case currFielder
                                    Case 2
                                        range = 24: min = 180
                                    Case 3, 4
                                        range = 31: min = 210
                                    Case 5, 6
                                        range = 31: min = 250
                                End Select

                            Case Else ' Fair ball (fo% <> 1)
                                Select Case currFielder
                                    Case 2
                                        range = 50: min = 400
                                    Case 3
                                        range = 60: min = 450
                                    Case 4
                                        range = 60: min = 630
                                    Case 5
                                        range = 60: min = 510
                                    Case 6
                                        range = 60: min = 570
                                End Select
                        End Select

                        Call GetPBPString(range, min, Y$)
                        Call PBP(Y$)

                        Select Case fo%
                            Case 1 ' Foul ball
                                Select Case currFielder
                                    Case 2
                                        range = 12: min = 321
                                    Case 3, 4
                                        range = 25: min = 340
                                    Case 5, 6
                                        range = 25: min = 370
                                End Select

                            Case Else ' Fair ball (fo% <> 1)
                                range = 25: min = 690
                        End Select

                        Call GetPBPString(range, min, Y$)
                        Call PBP(Y$)

                        If fldPos(P, currLineupSlot(P)) = 1 Then
                            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Else
                            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        End If

                        If baseRunners <> 4 Then

                            Call Rolld100(RN0)
                            If RN0 <= 25 And (teamRat_GAME(1, 13) = 11 Or teamRat_GAME(1, 13) = 8) Then
                                Call PBP("fights the wind...")
                            End If
                            If RN0 <= 15 And (teamRat_GAME(1, 13) = 2 Or teamRat_GAME(1, 13) = 4 Or teamRat_GAME(1, 13) = 13 Or teamRat_GAME(1, 13) = 24) Then
                                Call PBP("fights the lights...")
                            End If

                            If fo% <> 1 Then
                                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                                If errorOnPlay% = 1 Then
                                    GoTo LCheckForOptionsOrQuit
                                End If
                            End If

                        End If

                        fo% = 0

                        Call PBP(player$(4) + " makes the catch")
                        Call CreditRegularPutout(currFielder)

                        Call TrackOuts(1)

                        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)
                        GoTo LCheckForOptionsOrQuit

                    Case 90 To 100

                        'Print #11, "Runner doubled"
                        S2% = EVENT_RUNNER_DBLD

                        Call GETINFIELDER(currFielder, I1, S2%, offChoice)
                        Call WINDEX
                        Call DELIVERY

                        Call GetPBPString(42, 70, Y$)
                        Call PBP(Y$)

                        Call GetPBPString(34, 120, Y$)
                        Call PBP(Y$)

                        If fldPos(P, currLineupSlot(P)) = 1 Then
                            Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Else
                            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        End If

                        Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                        If errorOnPlay% = 1 Then
                            'Advancement error
                            GoTo LCheckForOptionsOrQuit
                        End If

                        Call TrackOuts(1)

                        Call CreditRegularPutout(currFielder)

                        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                        If nbrOuts(0) >= 3 Then
                            GoTo LCheckForOptionsOrQuit
                        End If

                        Call Rolld100(RN0)

                        If (baseRunners = 1 Or baseRunners = 5) And RN0 >= 80 Or (baseRunners = 1 Or baseRunners = 5) And offChoice = OFF_HIT_N_RUN Then
                            'Double Play

                            'Runner doubled off of 1st
                            baseMove(1) = 10

                            Call TrackOuts(1)

                            Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                            'Print #11, "Double play"
                            teamDPs(D) = teamDPs(D) + 1
                            totalDPs(D) = totalDPs(D) + 1

                            If currFielder <> 3 Then
                                range = 10
                                min = 160
                            Else
                                range = 10
                                min = 170
                            End If

                            Call GetPBPString(range, min, Y$)
                            Call PBP(Y$)

                            I3 = 1
                            Call CreditInfieldPutout(I3, FB2P%, AST%)
                            Call PBP(" --- DOUBLE PLAY ---")
                            If currFielder <> 3 Then
                                Call CreditAssist(currFielder)
                            End If

                        Else

                            If (baseRunners = 4 Or baseRunners = 2) And RN0 >= 80 Or baseRunners = 4 And offChoice = OFF_HIT_N_RUN Then
                                'Double Play

                                'Runner doubled off of 2nd
                                baseMove(2) = 10

                                Call TrackOuts(1)

                                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                                'Print #11, "Double play"
                                teamDPs(D) = teamDPs(D) + 1
                                totalDPs(D) = totalDPs(D) + 1
                                I3 = 2

                                Call CreditInfieldPutout(I3, FB2P%, AST%)
                                Call PBP(" --- DOUBLE PLAY ---")
                                Call CreditAssist(currFielder)

                            End If

                        End If

                        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                        GoTo LCheckForOptionsOrQuit

                End Select

            End If

            S2% = EVENT_GROUNDER
            Call GETINFIELDER(currFielder, I1, S2%, offChoice)
            Call WINDEX

            S2% = EVENT_GROUNDER
            Call DELIVERY
            Call GROUNDERPBP(currFielder)

            If currFielder <> 2 Then
                If currFielder = 1 Then
                    pbpString$ = pbpString$ + "the pitcher"
                    Call PBP(pbpString$)
                Else
                    pbpString$ = pbpString$ + grounderDir$(currFielder)
                    Call PBP(pbpString$)
                End If
            End If

            If (baseRunners = 1 Or baseRunners = 4 Or _
             ( (baseRunners = 7 Or baseRunners = 5) And INFPOS% < INF_IN) ) And nbrOuts(0) <= 1 And d100% <= 50 Then
                doublePlayChoice = 1
                Call DPPBP
            End If

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
            Else
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
            End If

            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            If errorOnPlay% = 1 Then
                ' ROE is currently not evaluated HERE; no thought
                ' is given to whether this would have been an out
                'Print #11, "Grounder base advance:"; baseMove(0)
                GoTo LCheckForOptionsOrQuit
            End If

            ' *** 2 outs, bases empty, runner 2nd, runner 3rd, or 2nd&3rd ***
            If nbrOuts(0) = 2 Or baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6 Then
                GoTo LGrounderToPitcher
            End If

            chanceRoll = Int(Rnd(1) * 1000) + 1
            E2% = ((teamRat_GAME(D, 5) - 80) * 4)
            E3% = ((5 - batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Run)) * 9)

            If (currFielder = 2 And chanceRoll > 250 + E2% + E3%) Or (chanceRoll > 490 + E2% + E3%) Or offChoice = OFF_HIT_N_RUN Then
                GoTo LGrounderToPitcher
            End If

            S2% = EVENT_GROUNDER

            If baseRunners = 4 Then

                '1/3 chance that 3rd base is out on grounder to 3rd
                If currFielder = 5 And Int(Rnd(1) * 3) + 1 = 1 Then
                    baseMove(0) = 0
                    baseMove(1) = 0
                    baseMove(2) = 11
                    baseMove(3) = 0
                    GoTo LProcessOutAndDP
                End If

                If currFielder = 2 Then
                    Call CreditAssist(currFielder)
                    baseMove(0) = 0
                    baseMove(1) = 0
                    baseMove(2) = 11
                    baseMove(3) = 0
                    GoTo LProcessOutAndDP
                End If

            End If

            If baseRunners = 7 And currFielder = 1 Then
                Call CreditAssist(currFielder)
                TB = 9
                currFielder = 2
                Call PBP("He throws home")
                baseMove(0) = 0
                baseMove(1) = 0
                baseMove(2) = 0
                baseMove(3) = 11
                GoTo LProcessOutAndDP
            End If

            If baseRunners = 5 And gameScore(P, 0) - gameScore(D, 0) >= 0 And INNING% > 7 And nbrOuts(0) = 0 _
                And INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3) Then 
                GoTo LGrounderToPitcher
            End If

            If baseRunners = 7 And INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3) Then
                Call CreditAssist(currFielder)
                TB = 9
                currFielder = 2
                Call PBP("He fires it home")
                baseMove(0) = 0
                baseMove(1) = 0
                baseMove(2) = 0
                baseMove(3) = 11
                GoTo LProcessOutAndDP
            End If

            If baseRunners = 7 And INFPOS% = INF_IN And currFielder = 4 Then
                Call CreditAssist(currFielder)
                TB = 9
                currFielder = 2
                Call PBP("He throws home for the force")
                baseMove(0) = 0
                baseMove(1) = 0
                baseMove(2) = 0
                baseMove(3) = 11
                GoTo LProcessOutAndDP
            End If

            If baseRunners = 7 And INFPOS% = INF_IN And currFielder = 6 Then
                Call CreditAssist(currFielder)
                TB = 9
                currFielder = 2
                Call PBP("He goes home with it")
                baseMove(0) = 0
                baseMove(1) = 0
                baseMove(2) = 0
                baseMove(3) = 11
                GoTo LProcessOutAndDP
            End If

            If currFielder <= 4 Then
                Call CreditAssist(currFielder)
                currFielder = 6
                Call PBP("He throws to " + batters$(D, fielder(D, 6)))
            Else
                Call CreditAssist(currFielder)
                currFielder = 4
                Call PBP("He throws to " + batters$(D, fielder(D, 4)))
            End If

            baseMove(0) = 0
            baseMove(1) = 11
            baseMove(2) = 0
            baseMove(3) = 0

            If baseRunners = 7 And (currFielder = 2 Or currFielder = 1) Then
                baseMove(0) = 1
                baseMove(1) = 1
                baseMove(2) = 1
                baseMove(3) = 11
            End If

            GoTo LProcessOutAndDP

        End If

    Else

        ' *** Base hit ***
        chanceRoll = Int(Rnd(1) * 1000) + 1

        'Check for DOUBLE
        If chanceRoll <= game_batRating(P, GM_2B, currLineupSlot(P))      And offChoice <> OFF_HIT_N_RUN Or _
           chanceRoll <= game_batRating(P, GM_2B, currLineupSlot(P)) * .5 And offChoice  = OFF_HIT_N_RUN Then

            S2% = EVENT_2B

            Call Hit_Double(errorOnPlay%, currFielder, I3, INFPOS%, I1, S2%, offChoice)

            errorOnPlay% = 0

            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            If errorOnPlay% = 0 Then
                If Environ$("RUNLOG") = "TRUE" Then
                    Print #11, "-- might try to advance on Double --"
                End If
                baseNbr = 0
                Call TRYFOREXTRABASE(S2%, offChoice, stealing, baseNbr, I9, baseRunners, runnerOnBase(2), currFielder, INFPOS%, errorOnPlay%, SAH%)
            End If

            Call SCOREBOARD

            GoTo LCheckForOptionsOrQuit

        End If

        ' Check for TRIPLE
        If chanceRoll <= game_batRating(P, GM_2B, currLineupSlot(P))      + game_batRating(P, GM_3B, currLineupSlot(P))      And offChoice <> OFF_HIT_N_RUN Or _
           chanceRoll <= game_batRating(P, GM_2B, currLineupSlot(P)) * .5 + game_batRating(P, GM_3B, currLineupSlot(P)) * .5 And offChoice  = OFF_HIT_N_RUN Then

            S2% = EVENT_3B

            Call Hit_Triple(errorOnPlay%, currFielder, I3, INFPOS%, I1, S2%, offChoice)

            errorOnPlay% = 0

            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "-- might try to advance on Triple --"
            End If

            baseNbr = 0

            If errorOnPlay% = 0 Then
                Call TRYFOREXTRABASE(S2%, offChoice, stealing, baseNbr, I9, baseRunners, runnerOnBase(2), currFielder, INFPOS%, errorOnPlay%, SAH%)
            End If

            Call SCOREBOARD

            GoTo LCheckForOptionsOrQuit

        End If

        Call HITS2GAP(currFielder, I1)

        tripleChance = game_batRating(P, GM_2B, currLineupSlot(P)) + game_batRating(P, GM_3B, currLineupSlot(P))

        'Check for Home Run
        Select Case currFielder

            Case 8
                hrChance = tripleChance + game_batRating(P, GM_HR, currLineupSlot(P))
                If chanceRoll <= hrChance      And offChoice <> OFF_HIT_N_RUN Or _
                    chanceRoll <= hrChance * .5 And offChoice  = OFF_HIT_N_RUN Then
                    S2% = EVENT_HR
                End If

            Case 7
                hrChance = tripleChance + game_batRating(P, GM_L_HR, currLineupSlot(P))
                If chanceRoll <= hrChance      And offChoice <> OFF_HIT_N_RUN Or _
                   chanceRoll <= hrChance * .5 And offChoice =  OFF_HIT_N_RUN Then
                    S2% = EVENT_HR
                End If

            Case 9
                hrChance = tripleChance + game_batRating(P, GM_R_HR, currLineupSlot(P))
                If chanceRoll <= hrChance      And offChoice <> OFF_HIT_N_RUN Or _
                   chanceRoll <= hrChance * .5 And offChoice = OFF_HIT_N_RUN Then
                    S2% = EVENT_HR
                End If

        End Select

        If S2% = EVENT_HR Then

            Call Hit_Homerun(errorOnPlay%, currFielder, I3, INFPOS%, S2%, P2)

            Call SCOREBOARD

            GoTo LCheckForOptionsOrQuit

        Else

            13063 '
            'Single
            Call Rolld100(chanceRoll)
            S2% = EVENT_1B

            If chanceRoll <= game_batRating(P, GM_BAvg, currLineupSlot(P)) And offChoice <> OFF_HIT_N_RUN And _ 
                fldPos(P, currLineupSlot(P)) <> 1 Then

                13065
                ' *** SINGLE ***
                Call Hit_Single(errorOnPlay%, currFielder, I3, INFPOS%, I1, S2%, offChoice)

                If S2% = EVENT_INF_SINGLE Then

                    Call SCOREBOARD
                    GoTo LNextBatter0

                Else

                    errorOnPlay% = 0

                    Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                    If Environ$("RUNLOG") = "TRUE" Then
                        Print #11, "-- might try to advance on Single --"
                    End If
                    baseNbr = 0
                    If errorOnPlay% = 0 Then
                        Call TRYFOREXTRABASE(S2%, offChoice, stealing, baseNbr, I9, baseRunners, runnerOnBase(2), currFielder, INFPOS%, errorOnPlay%, SAH%)
                    End If

                    Call SCOREBOARD

                    GoTo LCheckForOptionsOrQuit

                End If

            End If

            If chanceRoll <= (batRating(P, lineupPlayer(P, currLineupSlot(1)), SS_B_Run) * 2.5) Then

                S2% = EVENT_INF_SINGLE

                Call GETINFIELDER(currFielder, I1, S2%, offChoice)

                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                If errorOnPlay% = 1 Then
                    GoTo LCheckForOptionsOrQuit
                End If

            End If

        End If

    End If

    GoTo LProcessOutAndDP2


    '----- NOODLE -----

    LProcessOutAndDP:
    Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

    Call TrackOuts(1)

    player$(4) = batters$(D, fielder(D, currFielder))

    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
    Call CreditRegularPutout(currFielder)

    S2% = EVENT_DP

    Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
    If errorOnPlay% = 1 Then
        GoTo LCheckForOptionsOrQuit
    End If

    Call CreditAssist(currFielder)

    RN4 = Int(Rnd(1) * 10) + 1

    If currFielder = 5 And nbrOuts(0) < 2 And RN4 <= batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run) Then
        baseMove(0) = 1
        baseMove(1) = 11
    Else
        baseMove(0) = 11
        baseMove(1) = 1
    End If

    baseMove(2) = 0
    baseMove(3) = 0
    If nbrOuts(0) < 2 Then
        baseMove(2) = 1
        baseMove(3) = 1
    End If

    Call TrackOuts(0)
    nbrOuts(1) = 2

    Call DPRELAY
    Call INCPITDEF(D, currPitcher(D), LG_P_OUT)
    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
    Call CreditInfieldPutout(I3, FB2P%, AST%)

    doublePlayChoice = 0

    If d100% <= 80 Then
        pbpString$ = " --- DOUBLE PLAY ---"
    Else
        pbpString$ = " --- TWIN KILLING ---"
    End If

    Call PBP(pbpString$)

    'Print #11, "Double play"
    teamDPs(D) = teamDPs(D) + 1
    totalDPs(D) = totalDPs(D) + 1

    TB = 0
    Call INCREMENTHIT(P, MO)

    GoTo LCheckForOptionsOrQuit


    '----- NOODLE -----

    LGrounderToPitcher:
    If nbrOuts(0) >= 2 Then

        RN0 = Int(Rnd(1) * 10) + 1

        S2% = EVENT_GROUNDER

        If currFielder = 5 Then

            Call PITCHERNAME(currFielder)
            
            If RN0 <= 5 And (baseRunners = 4 Or baseRunners = 7) Then
                Call PBP(player$(4) + " gloves it and heads for 3rd")
                baseMove(0) = 0
                baseMove(1) = 0
                baseMove(2) = 11
                baseMove(3) = 0
                GoTo LProcessOutAndDP2
            Else
                Call GROUNDERS(currFielder)
                Call THROW2FIRST
                baseMove(0) = 11
                baseMove(1) = 0
                baseMove(2) = 0
                baseMove(3) = 0
            End If

        End If

        If currFielder = 6 Then

            Call PITCHERNAME(currFielder)
            Call GROUNDERS(currFielder)

            If RN0 <= 8 And (baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7) Then
                Call DPPIVOT(currFielder)
                AST% = 6
                baseMove(0) = 0
                baseMove(1) = 11
            Else
                Call THROW2FIRST
                baseMove(0) = 11
                baseMove(1) = 0
            End If

            baseMove(2) = 0
            baseMove(3) = 0
            GoTo LProcessOutAndDP2

        End If

        If currFielder = 4 And RN0 <= 5 And (baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7) Then

            Call PITCHERNAME(currFielder)
            Call GROUNDERS(currFielder)
            Call DPPIVOT(currFielder)
            AST% = 4
            baseMove(0) = 0
            baseMove(1) = 11
            baseMove(2) = 0
            baseMove(3) = 0

        Else

            If currFielder = 3 Then

                Call PITCHERNAME(currFielder)
                Call GROUNDERS(currFielder)

                If d100% <= 94 Then
                    Call PBP("He takes it to the bag unassisted")
                    fldrUnassist = 1
                Else
                    currFielder = 1
                    Call PITCHERNAME(currFielder)
                    Call PBP("He flips it to " + player$(4) + " covering")
                    FB2P% = 1
                    currFielder = 3
                End If

            Else

                Call SECOND2FIRST

            End If

            Call PITCHERNAME(currFielder)
            Call GROUNDERS(currFielder)
            Call SECOND2FIRST

            baseMove(0) = 11
            baseMove(1) = 0
            baseMove(2) = 0
            baseMove(3) = 0

        End If

        LProcessOutAndDP2:
        'Call DELAY
        If fldrUnassist = 0 Then
            Call CreditAssist(currFielder)
        End If
        fldrUnassist = 0

        batterSafeOnDP = 0
        If doublePlayChoice = 1 And baseMove(0) = 1 Then
            doublePlayChoice = 2
            batterSafeOnDP = 1
        End If

        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
        If batterSafeOnDP = 1 Then
            'ToDo [bm] controversial
            Call CreditRegularPutout(currFielder)
        End If

        If doublePlayChoice = 2 Then
            doublePlayChoice = 0
            Call DPSAFE
        End If

        If I3 > 4 Then I3 = 1

        If batterSafeOnDP = 0 Then
            'ToDo [bm] Controversial
            Call CreditInfieldPutout(I3, FB2P%, AST%)
        End If

        Call TrackOuts(1)

        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

        GoTo LCheckForOptionsOrQuit

    Else

        '6385
        Call Rolld100(resultRoll)

        If (resultRoll <= 90 And offChoice <> OFF_HIT_N_RUN Or baseRunners = 0) Or (resultRoll <= 10 And offChoice = OFF_HIT_N_RUN) Then

            S2% = 14

            If baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6 Then
                baseMove(0) = 11
                Call OUTATFIRST(currFielder, fldrUnassist, FB2P%)
                GoTo 13693
            End If

            baseMove(0) = 1
            baseMove(1) = 1
            baseMove(2) = 1
            baseMove(3) = 1

            If INFPOS% = INF_BACK And currFielder = 2 Then
                baseMove(3) = 0
            End If

            If baseRunners = 5 Or baseRunners = 7 Or baseRunners = 4 Then
                GoTo 13725
            End If

            baseMove(1) = 11

            GoTo LProcessOutAndDP2

        End If

        S2% = EVENT_GROUNDER
        baseMove(0) = 11
        baseMove(1) = 1
        baseMove(2) = 1
        baseMove(3) = 1

        If (baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6) Then

            13693 '
            Call Rolld100(RN1)
            baseMove(1) = 0
            baseMove(2) = 0
            baseMove(3) = 0
            
            Call Rolld100(runStratRoll)
            
            If teamMgrProfileVal(D, 810) = 999 Then
                runStratType = GetRunStrategy%
            Else
                runStratType = 1
            End If
            
            Select Case runStratType
            
                Case 0
                    'Safe = less likely to run
                    runStratBonus = -10
                    
                Case 1
                    'Normal strategy
                    runStratBonus = 0
                                
                Case 2
                    'Aggressive = more likely to run
                    runStratBonus = 10

            End Select

            Select Case currFielder

                Case 1
                    If RN1 <= 30 + runStratBonus Then
                        baseMove(2) = 1
                    End If
                    If (RN1 <= 50 + runStratBonus And INFPOS% = INF_BACK) Or _ 
                        (RN1 <= 10 + runStratBonus And INFPOS% > INF_BACK) Then
                        baseMove(3) = 1
                    End If

                Case 2
                    If RN1 <= 45 + runStratBonus Then
                        baseMove(2) = 1
                    End If

                Case 3
                    If RN1 <= 95 + runStratBonus Then
                        baseMove(2) = 1
                    End If
                    If (RN1 <= 88  + runStratBonus And INFPOS% = INF_BACK) Or _
                        (RN1 <= 15  + runStratBonus And INFPOS% > INF_BACK) Then
                        baseMove(3) = 1
                    End If

                Case 4
                    If RN1 <= 90 + runStratBonus And INFPOS% < INF_IN Then
                        baseMove(2) = 1
                        baseMove(3) = 1
                    End If
                    If INFPOS% = INF_IN Then
                        If RN1 <= 65 + runStratBonus Then
                            baseMove(2) = 1
                        End If
                        If RN1 <= 5 + runStratBonus Then
                            baseMove(3) = 1
                        End If
                    End If

                Case 5
                    If RN1 <= 25 + runStratBonus And INFPOS% < INF_IN Then
                        baseMove(2) = 1
                    End If
                    If (RN1 <= 83  + runStratBonus And INFPOS% = INF_BACK) Or _ 
                        (RN1 <= 10 And INFPOS% > INF_BACK) Then
                        baseMove(3) = 1
                    End If
                    If (RN1 < 5 + runStratBonus And INFPOS% < INF_IN) And (baseRunners = 2 Or baseRunners = 6) And gameScore(P, 0) - gameScore(D, 0) > -2 And baseMove(3) = 1 Then
                        baseMove(0) = 1
                        baseMove(2) = 11
                        Call PBP("NO, he's going to third")
                    End If

                Case 6
                    If RN1 <= 20 + runStratBonus And INFPOS% < INF_IN Then
                        baseMove(2) = 1
                    End If
                    If (RN1 <= 92  + runStratBonus And INFPOS% < INF_IN) Or _ 
                        (RN1 <= 5  + runStratBonus And INFPOS% = INF_IN) Then
                        baseMove(3) = 1
                    End If
                    If RN1 < 6 And INFPOS% < INF_IN And (baseRunners = 2 Or baseRunners = 6) And gameScore(P, 0) - gameScore(D, 0) > -2 And baseMove(3) = 1 Then
                        baseMove(0) = 1
                        baseMove(2) = 11
                        Call PBP("NO, he's going to third")
                    End If

            End Select

        End If

        GoTo LProcessOutAndDP2

    End If

    13725 '
    If baseRunners <> 4 And baseRunners <> 7 Then

        If gameScore(P, 0) - gameScore(D, 0) >= 0 And INNING% > 6 And nbrOuts(0) = 0 And INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3) Then

            baseMove(3) = 11

        Else

            If INFPOS% = INF_BACK And currFielder <> 2 Then
                baseMove(1) = 11
                GoTo LProcessOutAndDP2
            End If
            If INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3) Then
                baseMove(1) = 11
                baseMove(3) = 0
                GoTo LProcessOutAndDP2
            End If
            '-- is this accurate? currFielder = 3 above
            If INFPOS% > INF_BACK And (currFielder = 1 Or currFielder = 3) Then
                baseMove(0) = 11
                baseMove(1) = 1
                baseMove(3) = 0
                GoTo LProcessOutAndDP2
            End If
            If INFPOS% = INF_IN And (currFielder = 4 Or (currFielder = 6 And resultRoll <= 90)) Then
                baseMove(1) = 11
                baseMove(3) = 0
            Else
                baseMove(1) = 11
            End If

        End If

    Else

        '-- This RN0 should be the grounder roll

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If

        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -1
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 1

        End Select

        
        If RN0 < 6 And INFPOS% < INF_IN And INNING% < 9 And (currFielder = 6 Or currFielder = 5) Then
            baseMove(2) = 11
        Else
            If baseRunners = 7 Then

                baseMove(0) = 1
                baseMove(1) = 11
                baseMove(2) = 1
                baseMove(3) = 1

                If (INFPOS% > INF_BACK And (currFielder = 5 Or currFielder = 3)) Or _
                   (INFPOS% = INF_IN And (currFielder = 4 Or currFielder = 6)) Or _
                   currFielder = 2 Or currFielder = 1 Then
                    baseMove(0) = 1
                    baseMove(1) = 1
                    baseMove(2) = 1
                    baseMove(3) = 11
                Else
                    If currFielder = 5 And d100% <= 5 + runStratBonus Or currFielder = 1 Or currFielder = 2 Then
                        baseMove(0) = 1
                        baseMove(1) = 1
                        baseMove(2) = 1
                        baseMove(3) = 11
                    End If
                End If

            Else

                RN0 = Int(Rnd(1) * 10) + 1

                baseMove(0) = 1
                baseMove(1) = 11
                baseMove(2) = 1
                baseMove(3) = 0

                If (currFielder = 5 And RN0 <= 5 + runStratBonus) Or (currFielder = 1 And RN0 <= 3 + runStratBonus) Then
                    baseMove(1) = 1
                    baseMove(2) = 11
                    baseMove(3) = 0
                End If

                If currFielder = 2 And batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run) < 7 And batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run) > 0 Then
                    baseMove(1) = 1
                    baseMove(2) = 11
                    baseMove(3) = 0
                Else
                    If currFielder = 2 And batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run) > 6 Then
                        baseMove(0) = 11
                        baseMove(1) = 1
                        baseMove(2) = 1
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
    If useVGA = 1 Then
        _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144)
    End If

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
    If useVGA = 1 Then
        _PutImage (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80), backupScreen&, imageScreen&, (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80)
    End If

    doublePlayChoice = 0
    runnerHeld = 0
    INFPOS% = INF_BACK
    errorOnPlay% = 0

    If nbrOuts(0) >= 3 Then
        'Assign
        lastBatIdx(P) = currLineupSlot(P)
        GoTo LNextHalfInning
    Else
        GoTo LNextBatter0
    End If

    '----- NOODLE -----

    ' *** BUNT ROUTINE ***
    LBunt:
    Call WINDEX

    Erase baseMove

    sacPlayType = 2
    S2% = EVENT_BUNT

    If (baseRunners = 3 Or baseRunners > 4) And nbrOuts(0) < 2 Then
        Call SQUEEZECHOICE(sacPlayType, baseRunners)
    End If

    If fldPos(P, currLineupSlot(P)) = 1 Then
        buntRate = pitchRating(P, currPitcher(P), SS_P_Bunt)
    Else
        buntRate = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt)
    End If

    If INFPOS% > INF_BACK Then
        buntRate = buntRate - 2
    End If
    If buntRate <= 1 Then buntRate = 1

    Call Rolld100(roll)

    For I = 1 To 9:
        If roll <= buntQuality(teamRat_GAME(1, 10), buntRate, I) Then
            Exit For
        End If
    Next

    'buntResult = 1
    'Do While buntResult <= 9 And roll > buntQuality(teamRat_GAME(1, 10), buntRate, buntResult)
    '    buntResult = buntResult + 1
    'Loop

    buntResult = I

    ttw = Int(Rnd(1) * 10)
    Select Case ttw

        Case Is <= 2
            currFielder = 5

        Case 3 To 4
            currFielder = 1

        Case 5 To 7
            currFielder = 3

        Case Is >= 8
            currFielder = 2

    End Select

    Call GETFIELDNAME(currFielder)

    If d100% <= 52 Then
        'buntResult = 9
    Else
        If buntResult = 5 And sacPlayType = 1 Then
            buntResult = 6
        End If
    End If

    Select Case buntResult

        Case 1
            'Pop-up
            Call PBP(player$(0) + " bunts it in the air...")
            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            Call PBP(player$(4) + " makes the catch")
            Call Add_Strikeouts(P, D)

            Call TrackOuts(1)

            Call CreditRegularPutout(currFielder)

            If nbrOuts(0) <> 3 And sacPlayType <> 2 Then

                14111 '
                If currFielder <> 5 Then
                    Call CreditAssist(currFielder)
                    currFielder = 5
                    Call CreditRegularPutout(currFielder)
                    player$(5) = player$(4)
                    Call GETFIELDNAME(currFielder)
                Else
                    Call CreditAssist(currFielder)
                    currFielder = 6
                    Call CreditRegularPutout(currFielder)
                    player$(5) = player$(4)
                    Call GETFIELDNAME(currFielder)
                End If

                Call PBP(player$(5) + " throws to " + player$(4) + " at 3rd")

                baseMove(3) = 10

                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)

                Call TrackOuts(1)

                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                If sacPlayType = 1 Then
                    If nbrOuts(0) < 3 Then
                        GoTo LNextBatter1
                    Else
                        currLineupSlot(P) = currLineupSlot(P) - 1
                    End If
                End If

            End If

            GoTo LCheckForOptionsOrQuit

        Case 2
            'Double Play
            If baseRunners = 1 Or baseRunners = 4 Or baseRunners = 5 Or baseRunners = 7 Then

                Call PBP(player$(0) + " lays down a good bunt")

                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                If errorOnPlay% = 1 Then
                    GoTo LCheckForOptionsOrQuit
                End If

                Call PBP(player$(4) + " fields it cleanly")

                If sacPlayType = 1 And (baseRunners = 5 Or baseRunners = 7) Then

                    If currFielder = 2 Then
                        Call PBP("and catches " + player$(3) + " heading home!")
                        Call CreditRegularPutout(currFielder)
                    Else
                        'currFielder <> 2
                        Call PBP("and throws home to nail " + player$(3) + "!")
                        Call CreditAssist(currFielder)
                        currFielder = 2
                        Call CreditRegularPutout(currFielder)
                    End If

                    baseMove(3) = 11

                    For I = 0 To 2:
                        baseMove(0) = 1
                    Next

                    Call Add_Strikeouts(P, D)

                    Call TrackOuts(1)

                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                    Call CreditInfieldPutout(I3, FB2P%, AST%)

                    GoTo LCheckForOptionsOrQuit

                End If

                If baseRunners = 7 And nbrOuts(0) < 2 Then

                    baseMove(3) = 11

                    If currFielder = 2 Then
                        Call PBP("He steps on home for one...")
                        Call CreditRegularPutout(currFielder)
                        Call CreditAssist(currFielder)
                    Else
                        'currFielder <> 2
                        Call PBP("He throws home for the force")
                        Call CreditAssist(currFielder)
                        currFielder = 2
                        Call CreditRegularPutout(currFielder)
                        Call CreditAssist(currFielder)
                    End If

                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                    Call PBP("Back to first...")

                    baseMove(0) = 11

                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                    Call CreditInfieldPutout(I3, FB2P%, AST%)
                    Call PBP("DOUBLE PLAY!!")

                    Call TrackOuts(1)
                    Call TrackOuts(1)

                    Call Add_Strikeouts(P, D)

                    Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                    If nbrOuts(0) < 3 Then
                        baseMove(1) = 1
                        baseMove(2) = 1
                        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                    End If

                    GoTo LCheckForOptionsOrQuit

                End If

                Call PBP("He throws to 2nd")
                Call CreditAssist(currFielder)

                baseMove(1) = 11

                Call Add_Strikeouts(P, D)

                Call TrackOuts(1)

                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                Call CreditInfieldPutout(I3, FB2P%, AST%)

                If nbrOuts(0) < 3 Then

                    baseMove(0) = 11
                    Call CreditAssist(currFielder)

                    baseMove(2) = 1
                    Call PBP("over to first...")
                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                    Call PBP("DOUBLE PLAY!!")
                    Call CreditInfieldPutout(I3, FB2P%, AST%)

                    Call TrackOuts(1)

                    Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                End If

                GoTo LCheckForOptionsOrQuit

            Else

                Call PBP(player$(0) + " lays down the bunt...")
                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                Call PBP(player$(4) + " makes a nice fielding play")
                Call CreditAssist(currFielder)
                baseMove(0) = 11

                If sacPlayType = 1 And (baseRunners = 3 Or baseRunners = 6) Then
                    baseMove(3) = 11
                    For I = 0 To 2:
                        baseMove(I) = 1
                    Next
                End If

                Call Add_Strikeouts(P, D)

                Call TrackOuts(1)

                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                Call CreditInfieldPutout(I3, FB2P%, AST%)
                GoTo LCheckForOptionsOrQuit

            End If

        Case 3
            Call LeadRunnerCountdown(INFPOS%, currFielder, errorOnPlay%, baseStatus)
            GoTo LCheckForOptionsOrQuit

        Case 4
            'Sacrifice hit good
            14140 '
            Call PBP(player$(0) + " lays down a good bunt...")
            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            Call PBP(player$(4) + " makes a nice play")

            If baseRunners = 0 Then

                S2% = -EVENT_INF_SINGLE
                Call PBP(player$(0) + " beats out an infield hit!")
                GoTo 13065

            Else

                14141 '
                Call INCPITDEF(D, currPitcher(D), LG_P_Sac)

                'Increase Sac Hits
                If fldPos(P, currLineupSlot(P)) = 1 Then
                    Call INCPITOFF(P, currPitcher(P), LG_PB_Sac)
                Else
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Sac)
                End If

                Call Rolld100(RN0)

                If baseRunners = 5 And sacPlayType = 2 And RN0 > 98 Or RN0 > 97 Then
                    S2% = -EVENT_INF_SINGLE
                    Call PBP(player$(0) + " beats it out!")
                    GoTo 13065
                End If

                baseMove(0) = 11

                Call PBP("The only play is to first")

                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                Call TrackOuts(1)

                Call CreditAssist(currFielder)
                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                Call CreditInfieldPutout(I3, FB2P%, AST%)

                If nbrOuts(0) < 3 Then
                    For I = 1 To 3
                        baseMove(I) = 1
                    Next
                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                End If

            End If

            GoTo LCheckForOptionsOrQuit

        Case 5
            'Catcher at 3rd
            14150 '
            If fldPos(P, currLineupSlot(P)) = 1 Then
                RR = 2
            Else
                RR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Run)
            End If

            If INFPOS% > INF_BACK Then RR = RR - 2
            If RR <= 2 Then RR = 2

            Call Rolld100(roll)

            idx = 0
            Do While idx <= 2 And roll > buntBaseHit(RR, idx)
                idx = idx + 1
            Loop

            If baseRunners > 0 And idx = 0 Then
                'Batter's not trying for a double if
                'if someone is on base already
                idx = 1
            End If

            Select Case idx + 1
                Case 1

                    If baseRunners > 0 Then 14157
                    Call PBP(player$(0) + " drops it right down the line")
                    Call PBP(player$(4) + " lets it roll...")
                    Call PBP(player$(0) + " is trying for 2nd!!")
                    Call PBP(player$(4) + " fields and throws too late!!")
                    Call PBP("You won't see that play very often")

                    baseMove(0) = 2
                    Call INCPITDEF(D, currPitcher(D), LG_P_H)

                    If fldPos(P, currLineupSlot(P)) <> 1 Then
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
                    Else
                        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
                        Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
                    End If

                    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
                    Call SCOREBOARD

                    GoTo LCheckForOptionsOrQuit

                Case 2
                    14157 '
                    S2% = EVENT_INF_SINGLE
                    Call PBP(player$(0) + " lays down a great bunt...")

                    Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                    If errorOnPlay% = 1 Then
                        GoTo LCheckForOptionsOrQuit
                    End If

                    Call PBP(player$(4) + " fields... but too late!")
                    S2% = -EVENT_INF_SINGLE
                    GoTo 13065

                Case 3
                    GoTo 14140

            End Select

        Case 6
            'Player leads base
            If fldPos(P, currLineupSlot(P)) = 1 Then
                RR = 2
            Else
                RR = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Run)
            End If

            If INFPOS% > INF_BACK Then
                RR = RR - 2
            End If

            If RR <= 2 Then RR = 2

            Select Case baseRunners

                Case 0
                    GoTo 14150

                Case 1
                    I3 = 1
                    J3$ = "2nd"

                Case 2, 4
                    I3 = 2
                    J3$ = "3rd"

                Case 3, Is >= 5
                    I3 = 3
                    J3$ = "home"

            End Select

            Call PBP(player$(0) + " gets the bunt down...")

            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            If errorOnPlay% = 1 Then
                GoTo LCheckForOptionsOrQuit
            End If

            Call PBP(player$(4) + " fields it quickly...")
            Call PBP("Looks like a close play at " + J3$ + "!!")
            Call PBP("1- Throw to " + J3$ + " 2- Throw to 1st")

            If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then
                I$ = "1"
            Else
                Do
                    I$ = GetKeyPress$
                    I5 = Val(I$)
                Loop Until I5 >= 1 And I5 <= 2
            End If

            If I5 = 2 Then 14141

            Call Rolld100(RN0)

            I = 0
            Do While I <= 1 And RN0 > buntLeadRun(RR, I)
                I = I + 1
            Loop

            If I = 0 Then
                PLB = 1
                Call LeadRunnerCountdown(INFPOS%, currFielder, errorOnPlay%, baseStatus)
            Else
                Call PBP(player$(4) + " throws to " + J3$ + "... SAFE!!")
                If fldPos(P, currLineupSlot(P)) <> 1 Then
                    Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Else
                    Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                End If

                For I = 0 To 3:
                    baseMove(I) = 1
                Next

                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
            End If

            GoTo LCheckForOptionsOrQuit

        Case 7
            'Foul Strikeout
            If pitchRating(D, currPitcher(D), SS_P_IP) = 0 Then
                BR! = 0
            Else
                BR! = pitchRating(D, currPitcher(D), SS_P_K) / pitchRating(D, currPitcher(D), SS_P_IP)
            End If

            Select Case BR!

                Case Is <= 4
                    PR = 5

                Case 4 To 5.4
                    PR = 4

                Case 5.41 To 6.9
                    PR = 3

                Case 6.91 To 8.4
                    PR = 2

                Case 8.41 To 11.9
                    PR = 1

                Case Is > 11.9
                    PR = 0

            End Select

            Call Rolld100(roll)

            idx = 0
            Do While idx <= 2 And roll > buntStrike(PR, idx)
                idx = idx + 1
            Loop

            Select Case idx + 1

                Case 1
                    GoTo LStrikeout

                Case 2
                    nbrStrikes = nbrStrikes + 1

                    Call WINDEX

                    Call Rolld100(pbpRoll)

                    pbpString$ = player$(0) + " "
                    If pbpRoll <= 13 Or sacPlayType = 1 And pbpRoll <= 42 Then
                        pbpString$ = pbpString$ + "misses the pitch"
                    End If
                    If pbpRoll > 13 And pbpRoll <= 42 And sacPlayType <> 1 Then
                        pbpString$ = pbpString$ + "takes a called strike"
                    End If
                    If pbpRoll > 42 Then
                        pbpString$ = pbpString$ + "bunts it foul"
                    End If

                    Call PBP(pbpString$)

                    If (autoPlay <> 1 Or playerMode <> 2) And sndOpt = 1 And miss& And d100% < 10 Then
                        _SndPlay miss&
                    End If

                    If useVGA = 1 Then
                        PrintOnGfx 63, 28, Str$(nbrStrikes), imageScreen&
                    Else
                        Locate 2, 58: Print nbrStrikes
                    End If

                    If nbrStrikes = 3 Then
                        GoTo LStrikeout
                    End If

                    If sacPlayType = 1 And d100% <= 42 Then
                        currFielder = 2
                        Call GETFIELDNAME(currFielder)
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
            'Tough Field Play
            Call PBP(player$(0) + " lays down the bunt...")
            Call PBP(player$(4) + " has a tough play")

            Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
            If errorOnPlay% = 1 Then
                GoTo LCheckForOptionsOrQuit
            End If

            If currFielder = 1 Then
                RR = 3
            Else
                RR = batRating(D, fielder(D, currFielder), SS_B_Range)
            End If

            If INFPOS% > INF_BACK And baseRunners <> 0 Then
                RR = RR + 1 + teamRat_GAME(1, 10)
            End If
            If INFPOS% > INF_BACK And baseRunners = 0 Then
                RR = RR + teamRat_GAME(1, 10)
            End If
            If INFPOS% = INF_BACK And baseRunners = 0 Then
                RR = RR - 1
            End If

            If RR > 11 Then RR = 11
            If RR < 0 Then RR = 0

            Call Rolld100(roll)

            idx = 0
            Do While idx <= 2 And RN0 > buntTriple(RR, idx)
                idx = idx + 1
            Loop

            Select Case idx + 1
                Case 1:
                    S2% = EVENT_INF_SINGLE
                    Call PBP("but too late...base hit!")
                    GoTo 13065

                Case 2:
                    PLB = 1
                    Call LeadRunnerCountdown(INFPOS%, currFielder, errorOnPlay%, baseStatus)
                    GoTo LCheckForOptionsOrQuit

                Case 3:
                    GoTo 14141

            End Select

        Case 9
            'Strike on bunt
            nbrStrikes = nbrStrikes + 1

            Call WINDEX
            Call Rolld100(pbpRoll)

            pbpString$ = player$(0) + " "

            If pbpRoll <= 13 Or sacPlayType = 1 And pbpRoll <= 42 Then
                pbpString$ = pbpString$ + "misses the pitch"
            End If
            If pbpRoll > 13 And pbpRoll <= 42 And sacPlayType <> 1 Then
                pbpString$ = pbpString$ + "takes a called strike"
            End If
            If pbpRoll > 42 Then
                pbpString$ = pbpString$ + "bunts it foul"
            End If

            Call PBP(pbpString$)

            If (autoPlay <> 1 Or playerMode <> 2) And sndOpt = 1 And miss& And d100% < 10 Then
                _SndPlay miss&
            End If

            If useVGA = 1 Then
                PrintOnGfx 63, 28, Str$(nbrStrikes), imageScreen&
            Else
                Locate 2, 58: Print nbrStrikes
            End If

            If nbrStrikes = 3 Then
                GoTo LStrikeout
            End If

            If sacPlayType = 1 And RN0 <= 42 Then
                currFielder = 2
                Call GETFIELDNAME(currFielder)
                GoTo 14111
            End If

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

    ' *** STEALING BASES ***

    LSteal:
    stealHome% = 0
    S2% = EVENT_STEAL
    currFielder = 1
    player$(4) = pitchers$(D, currPitcher(D))

    Select Case baseRunners

        Case 0
            GoTo LNextBatter1

        Case 1, 5
            TGTBASE% = 2

            If offChoice <> OFF_HIT_N_RUN Then

                Call WINDEX
                stealing = 1

                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
                stealing = 0
                If errorOnPlay% = 1 Then
                    currLineupSlot(P) = currLineupSlot(P) - 1
                    Call WHOSON1ST(baseRunners)
                    GoTo LCheckForOptionsOrQuit
                End If

            End If

        Case 2, 4

            TGTBASE% = 3

            If offChoice <> OFF_HIT_N_RUN Then

                Call WINDEX
                stealing = 1

                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
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

    I2 = lineupPlayer(P, runnerOnBase(TGTBASE% - 1))
    baseMove(1) = 1
    baseMove(2) = 1
    baseMove(3) = 0

    'Change for stealing base
    Call STEALCHANCE(AA, I2, TGTBASE%, CHANCE%)
    If fldPos(P, runnerOnBase(TGTBASE% - 1)) = 1 Then
        CHANCE% = 0
    End If

    'Reduce chance for stealing home
    If TGTBASE% = 4 Then
        CHANCE% = CHANCE% * .05
    End If

    If batRating(P, I2, SS_B_CS) > 0 Then
        stlChance = Int((batRating(P, I2, SS_B_SB) / (batRating(P, I2, SS_B_SB) + batRating(P, I2, SS_B_CS))) * 1000)
    ElseIf batRating(P, I2, SS_B_SB) > 31 Then
        stlChance = 700 + ((batRating(P, I2, SS_B_SB) - 30) * 2)
    ElseIf batRating(P, I2, SS_B_SB) > 10 Then
        stlChance = 600 + ((batRating(P, I2, SS_B_SB) - 10) * 10)
    ElseIf batRating(P, I2, SS_B_SB) > 0 Then
        stlChance = 400 + ((batRating(P, I2, SS_B_SB) * 20))
    Else
        stlChance = 100
    End If

    If fldPos(P, runnerOnBase(TGTBASE% - 1)) = 1 Then
        stlChance = 100
    End If

    ' *** CHANCE OF 3B STEAL ***
    Call STEAL3RD(TGTBASE%, AA, stlChance, runnerHeld)

    ' Adjust chance for 2nd Base
    If TGTBASE% = 2 Then
        If pitchRating(D, currPitcher(D), SS_P_Throws) = 1 Then
            CHANCE% = CHANCE% + 25
            stlChance = stlChance + 10
        End If
        If pitchRating(D, currPitcher(D), SS_P_Throws) = -1 Then
            CHANCE% = CHANCE% - 50
            stlChance = stlChance - 30
        End If
    End If

    If offChoice = OFF_HIT_N_RUN Then

        CHANCE% = 100
        stlChance = stlChance - 200
        currFielder = 2
        player$(4) = batters$(D, fielder(D, 2))

        'Stealing TGTBASE% will be by runner on previous base
        stealRunIdx = lineupPlayer(P, runnerOnBase(TGTBASE% - 1))
        stealPhase(TGTBASE% - 1) = 18

        Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
        If errorOnPlay% = 1 Then
            Call INCBATOFF(P, stealRunIdx, 9)
            currLineupSlot(P) = currLineupSlot(P) - 1
            Call STRIKEOUTCS(wasStrikeout)
            GoTo LCheckForOptionsOrQuit
        Else
            GoTo LDoubleSteal2
        End If

    End If

    'Slightly adjust odds of being caught in rundown
    stealPhase(TGTBASE% - 1) = stealPhase(TGTBASE% - 1) + 1

    Call Rolld100(rundownRoll)

    ' stealPhase holds the threshold/phase for steal attempts
    '   - Initial value = chance (1–100) of being caught in a rundown before the throw
    If rundownRoll <= stealPhase(TGTBASE% - 1) Then

        S2% = EVENT_RUNDOWN
        currFielder = 1
        player$(4) = pitchers$(D, currPitcher(D))

        LCaughtInRundown:
        Call WINDEX
        Call PBP(player$(TGTBASE% - 1) + " caught off base in a rundown!")

        baseMove(1) = 0
        baseMove(2) = 0
        baseMove(3) = 0
        baseMove(TGTBASE% - 1) = 11

        Call TrackOuts(1)

        Call PUTOUT(TGTBASE%)
        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)
        Call CreditInfieldPutout(I3, FB2P%, AST%)

        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

        If nbrOuts(0) >= 3 Then
            currLineupSlot(P) = currLineupSlot(P) - 1
            GoTo LCheckForOptionsOrQuit
        End If

        INFPOS% = INF_BACK

        GoTo LNextBatter1

    End If


    'RUNNER HAS A CHANCE TO STEAL

    'Runner avoided a rundown, +17 is added to mark "steal in progress" phase
    stealPhase(TGTBASE% - 1) = stealPhase(TGTBASE% - 1) + 17

    I9 = Int(Rnd(1) * 1000) + 1

    If I9 > CHANCE% * 1.388 And D5 <> 3 Then

        Call WINDEX
        Call NOJUMP

        If useVGA = 1 Then
            _PutImage (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80), backupScreen&, imageScreen&, (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80)
        End If

        GoTo LOffenseOptions

    End If

    LStealHome:
    Call WINDEX
    'Presumably here TGTBASE% = 4
    stealRunIdx = lineupPlayer(P, runnerOnBase(TGTBASE% - 1))

    Call GetPBPString(20, 3090, Y$)
    Call PBP(Y$)

    Call GetPBPString(13, 3110, Y$)
    Call PBP(Y$)

    Call PITCHOUT(baseRunners, TGTBASE%, stlChance)

    If baseRunners <> 5 Then
        ' *** DOUBLE STEAL ***
        '1st&3rd
        GoTo LDoubleSteal
    End If

    If stealHome% = 1 Then
        I9 = Int(Rnd(1) * 1000) + 1
    Else
        If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then
            I$ = "Y"
        Else
            Call PBP("Throw to 2B? (Y/N) ")
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then
            pitchoutChance = 7
            GoTo LRunnerSafe
        End If

        LDoubleSteal:
        currFielder = 2
        player$(4) = batters$(D, fielder(D, 2))

        Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, errorOnPlay%, SAH%)
        If errorOnPlay% = 1 Then
            'Credit 3rd base runner
            Call INCBATOFF(P, stealRunIdx, LG_B_SB)
            'Credit any other base runners with a steal
            Call PITSWAP(TGTBASE%, errorOnPlay%)
            currLineupSlot(P) = currLineupSlot(P) - 1
            GoTo LCheckForOptionsOrQuit
        End If

        LDoubleSteal2:
        Call WHICHRUNNER(TGTBASE%, baseRunners, D5)
        I9 = Int(Rnd(1) * 1000) + 1
        stlChance = stlChance - ((batRating(D, fielder(D, 2), SS_B_Arm) - 5) * 25)

    End If

    RN0 = Int(Rnd(1) * 11) + 1

    If TGTBASE% <> 4 Then

        Select Case RN0

            Case 1
                pbpString$ = batters$(D, fielder(D, 2)) + " rifles it to " + baseName$(TGTBASE%)

            Case 2
                pbpString$ = batters$(D, fielder(D, 2)) + " fires it to " + baseName$(TGTBASE%)

            Case 3
                pbpString$ = batters$(D, fielder(D, 2)) + " unloads it"

            Case 4
                pbpString$ = batters$(D, fielder(D, 2)) + " is up and firing"

            Case 5
                pbpString$ = batters$(D, fielder(D, 2)) + " comes up firing"

            Case 6
                pbpString$ = batters$(D, fielder(D, 2)) + " lets it fly"

            Case 7
                pbpString$ = batters$(D, fielder(D, 2)) + " throws down"

            Case 8
                pbpString$ = batters$(D, fielder(D, 2)) + " gets it off"

            Case 9
                pbpString$ = batters$(D, fielder(D, 2)) + " fires a missile"

            Case 10
                pbpString$ = batters$(D, fielder(D, 2)) + " throws quickly"

            Case 11
                pbpString$ = batters$(D, fielder(D, 2)) + " throws"

        End Select

        Call PBP(pbpString$)

    End If

    'Steal chance is impacted by run bias of base umpire
    stlChance = stlChance + umpRunAdj(TGTBASE%)

    ' Final safe/out decision compares random roll to stlChance (phase-dependent)
    If (stealPhase(TGTBASE% - 1) <> 18 Or I9 <= stlChance) And _
       (stealPhase(TGTBASE% - 1) <= 18 Or I9 <= stlChance - 200) Then
   
        LRunnerSafe:
        Call Rolld100(pbpRoll)

        Select Case pbpRoll
            Case 1 To 20:
                pbpString$ = "He beats the throw!"

            Case 21 To 40:
                pbpString$ = "He's in there!"

            Case 41 To 50:
                pbpString$ = "Throw is off-line..."

            Case 51 To 60:
                pbpString$ = "Throw is high..."

            Case 61 To 80:
                pbpString$ = "He slides under the tag!"

            Case 81 To 100:
                If TGTBASE% <> 4 Then
                    pbpString$ = gameUmpire$(TGTBASE% + 1) + " with the call"
                Else
                    pbpString$ = gameUmpire$(1) + " with the call"
                End If
        End Select

        Call PBP(pbpString$)
        Call PBP("safe @ " + baseName$(TGTBASE%) + "!")

        stealPhase(TGTBASE% - 1) = 0
        INFPOS% = INF_BACK
        Call PITSWAP(TGTBASE%, errorOnPlay%)
        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)

        Call INCBATDEF(D, fielder(D, 2), LG_BF_SB)

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

    Call Rolld100(pbpRoll)

    Select Case pbpRoll
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

    baseMove(TGTBASE% - 1) = 11
    INFPOS% = INF_BACK

    Call INCBATOFF(P, lineupPlayer(P, runnerOnBase(TGTBASE% - 1)), LG_B_CS)

    Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

    Call TrackOuts(1)

    currFielder = 2

    Call CreditAssist(currFielder)
    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, errorOnPlay%, SAH%)

    If TGTBASE% = 2 Then currFielder = _IIf(Rnd(1) < 0.5, 4, 6) 'BugFix: Set to one of the two middle infielders
    If TGTBASE% = 3 Then currFielder = 5 'BugFix: Set to 3rd baseman
    Call CreditInfieldPutout(currFielder, FB2P%, AST%)

    Call INCBATDEF(D, fielder(D, 2), LG_BF_CS)

    If nbrOuts(0) >= 3 Then
        currLineupSlot(P) = currLineupSlot(P) - 1
        Call STRIKEOUTCS(wasStrikeout)
        GoTo LCheckForOptionsOrQuit
    End If

    LStealContinue:
    If nbrOuts(0) >= 3 Or offChoice = OFF_HIT_N_RUN And nbrStrikes = 3 Then
        GoTo LCheckForOptionsOrQuit
    End If

    If stealHome% = 1 Then
        stealHome% = 0
        GoTo LNextBatter1
    End If

    If AA < 20 Or baseRunners <> 5 Or fldPos(P, runnerOnBase(3)) = 1 Then
        GoTo LNextBatter1
    End If

    Call WINDEX

    If (playerMode = 1 And P = compTeam) Or playerMode = 2 Then
        I$ = "N"
    Else
        Call PBP("TRY TO STEAL HOME? (Y/N)")
        I$ = GetKeyPress$
    End If

    ' *** STEAL HOME ***
    If UCase$(I$) = "N" Then

        GoTo LNextBatter1

    Else

        'Give it a shot
        sacPlayType = 1
        TGTBASE% = 4
        baseMove(1) = 0
        baseMove(2) = 0
        baseMove(3) = 1
        I2 = lineupPlayer(P, runnerOnBase(TGTBASE% - 1))
        CHANCE% = 100
        stlChance = 700

        GoTo LStealHome

    End If


    '-----------------------------------
    '-----      PERSONNEL MENU     -----
    '-----------------------------------
    '-- make this into a subroutine?!?!

    LPersonnelMenu:

    Cls

    If useVGA = 1 Then
        Screen menu&
        'Cls
    End If

    TG = 0
    PG = 0
    RG = 0

    Locate 2, 33
    Color 12: Print "PERSONNEL MENU"
    Locate 16
    Color 14: Print "(0) ";
    Color 15: Print "RESUME PLAY"
    Locate 18
    Color 14: Print "(1) ";
    Color 15: Print "RELIEF PITCHER"
    Locate 20
    Color 14: Print "(2) ";
    Color 15: Print "CHANGE BATTER/FIELDER"

    Do

        validChoice = 0

        Do
            I$ = GetKeyPress$
            menuChoice = Val(I$)
            I5 = 0
            validChoice = 1
        Loop Until menuChoice >= 0 And menuChoice <= 2

        Select Case menuChoice

            Case 0
                15580 '
                If pitchRating(D, currPitcher(D), SS_P_Avail) = 99 Then

                    'If pitcher isn't still injured...
                    'we need to keep trying
                    validChoice = 0

                Else

                    For I = 1 To 9
                        If fldPos(P9, I) <> 1 Then
                            batRating(P9, lineupPlayer(P9, I), SS_B_Avail) = 1
                        End If
                    Next I

                    If useVGA = 1 Then
                        Call LOADSCREEN
                        Call DisplayBallField_Gfx(nbrBalls, nbrStrikes)
                    Else
                        Call DisplayBallField(nbrBalls, nbrStrikes)
                    End If

                    GoTo LNextBatter1

                End If

            Case 1
                LChangePitcher:
                'Warn if not an injured pitcher who has not faced 3 batters

                'If pitchRating(D, currPitcher(D), SS_P_Avail) <> 99 And (batterMinimumOpt = 1 And battersFaced(D, currPitcher(D)) < 3) Then
                If (batterMinimumOpt = 1 And battersFaced(D, currPitcher(D)) < 3) Then

                    'Print error for human user reminding them
                    'of the rules in effect for batters
                    validChoice = 0
                    Color 12
                    Locate 22, 20: Print "CURRENT PITCHER HAS NOT FACED 3 BATTERS"
                    Locate , 21: Print "CANNOT USE RELIEF PITCHER AT THIS TIME"
                    Color 14
                    Locate 25, 24: Print "PRESS ANY KEY TO RETURN TO GAME"
                    I$ = GetKeyPress$

                    'Return to game
                    If useVGA = 1 Then
                        Call LOADSCREEN
                        Call DisplayBallField_Gfx(nbrBalls, nbrStrikes)
                    Else
                        Call DisplayBallField(nbrBalls, nbrStrikes)
                    End If

                    GoTo LNextBatter1

                Else

                    If offChoice = OFF_PERSONNEL Then

                        Call TIRING

                    Else

                        Do

                            Call PITDISP
                            Locate LAST_ROW - 1, 1: Print "SELECT RELIEVER (Z = NO CHANGE)   ";
                            If fielder(P9, 1) <> 99 Then Call TIRING

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

                        pinchHitterFlag = 0
                        runnerUsed = 0

                        If relieverOnRecordForSave(P9) <> -1 And gamePitcher(P9) > 1 Then
                            Call INCPITOFF(P9, currPitcher(P9), LG_P_Hold)
                        End If

                        currPitcher(P9) = I1
                        gamePitcher(P9) = gamePitcher(P9) + 1
                        pitchRating(P9, I1, SS_P_Avail) = gamePitcher(P9)

                        Call PITSTAM(P9)
                        Call CHECKSAVE(baseRunners)

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
                                Color 14: Print "DO YOU WISH TO MAKE A DOUBLE SWITCH? (Y/N)";
                                Color 15

                                I$ = GetKeyPress$

                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                            If UCase$(I$) = "Y" Then

                                Cls
                                Call DisplayBatter_Lineup(PG)

                                Call CHANGELINEUP

                                Do

                                    Locate 12, 30: Print "LINEUP SLOT FOR PITCHER TO BAT IN (1-9)"

                                    I$ = GetKeyPress$

                                    If Asc(I$) = 32 Then
                                        PG = 1 - PG
                                        Call DisplayBatter_Lineup(PG)
                                        Call CHANGELINEUP
                                    End If

                                    If Asc(I$) = 9 Then
                                        If TG = 0 Then
                                            Call DisplayFielding_Lineup(PG, TG)
                                        Else
                                            'TG = 1
                                            TG = 0
                                            Call DisplayBatter_Lineup(PG)
                                        End If

                                        Call CHANGELINEUP
                                    End If

                                    batterSlot = Val(I$)

                                Loop Until batterSlot >= 1 And batterSlot <= 9


                                Do

                                    Locate 13, 30
                                    Print "WHO WILL REPLACE "; batters$(D, lineupPlayer(D, batterSlot)); " @ "; fieldPos$(fldPos(D, batterSlot));

                                    I$ = GetKeyPress$

                                    If Asc(I$) = 32 Then
                                        PG = 1 - PG
                                        Call DisplayBatter_Lineup(PG)
                                    End If

                                    If Asc(I$) = 9 Then
                                        If TG = 0 Then
                                            Call DisplayFielding_Lineup(PG, TG)
                                        Else
                                            'TG = 1
                                            TG = 0
                                            Call DisplayBatter_Lineup(PG)
                                        End If
                                    End If

                                    Call LetterToNumber(I$, fieldSlot)

                                    Do
                                        Locate 16, 29: Print batters$(D, fieldSlot)
                                        Locate 17, 29: Print "ARE YOU SURE? (Y/N)";
                                        I$ = GetKeyPress$
                                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                                Loop Until fieldSlot <= 22

                                If UCase$(I$) = "Y" Then

                                    'Track game roster with substitution
                                    playersUsed(D) = playersUsed(D) + 1
                                    gameRoster(D, 0, playersUsed(D)) = fieldSlot
                                    gameRoster(D, 1, playersUsed(D)) = fldPos(D, batterSlot)

                                    For V0 = 1 To 9
                                        If fldPos(D, V0) = 1 Then
                                            gameRoster(D, 2, playersUsed(D)) = (V0 * 10) + (playersUsed(D) - 9)
                                        End If
                                    Next V0

                                    For V0 = 1 To 9

                                        If fldPos(D, V0) = 1 Then

                                            fldPos(D, V0) = fldPos(D, batterSlot)
                                            lineupPlayer(D, V0) = fieldSlot
                                            fielder(D, fldPos(D, V0)) = fieldSlot

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

                                    batRating(D, fieldSlot, SS_B_Avail) = 1
                                    I7% = D
                                    I8% = batterSlot
                                    I9% = fieldSlot

                                    Call REPLACEMENTS(I7%, I8%, I9%)

                                    I5 = 0

                                    For V0 = 1 To 9

                                        If V0 = batterSlot Then

                                            fldPos(D, batterSlot) = 1
                                            lineupPlayer(D, batterSlot) = currPitcher(D)
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

                    End If

                End If

            Case 2

                LChangeBatterFielder:
                Cls
                Call DisplayBatter_Lineup(PG)
                Call CHANGELINEUP

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

                Do

                    I$ = GetKeyPress$

                    If Asc(I$) = 32 Then
                        If PG = 0 Then
                            PG = 1
                        Else
                            PG = 0
                            Call DisplayBatter_Lineup(PG)
                        End If
                    End If

                    If Asc(I$) = 9 Then
                        If TG = 0 Then
                            Call DisplayFielding_Lineup(PG, TG)
                        Else
                            'TG = 1
                            TG = 0
                            Call DisplayBatter_Lineup(PG)
                        End If
                    End If

                Loop Until (Asc(I$) <> 32 And PG = 1) And Asc(I$) <> 9

                If UCase$(I$) <> "Z" Then
                    I1 = Val(I$)
                Else
                    GoTo LPersonnelMenu
                End If

                If I1 = 2 Then

                    Do
                        Locate 20, 29: Print "B.O. #1 (1-9)"
                        I$ = GetKeyPress$
                        bo1Nbr = Val(I$)
                    Loop Until bo1Nbr >= 1 And bo1Nbr <= 9 Or fldPos(P9, bo1Nbr) <> 1

                    Do
                        Locate 21, 29: Print "B.O. #2 (1-9)"
                        I$ = GetKeyPress$
                        bo2Nbr = Val(I$)
                    Loop Until bo2Nbr >= 1 And bo2Nbr <= 9 Or fldPos(P9, bo2Nbr) <> 1

                    I3 = fldPos(P9, bo2Nbr)
                    fldPos(P9, bo2Nbr) = fldPos(P9, bo1Nbr)
                    fldPos(P9, bo1Nbr) = I3
                    fielder(P9, fldPos(P9, bo1Nbr)) = lineupPlayer(P9, bo1Nbr)
                    fielder(P9, fldPos(P9, bo2Nbr)) = lineupPlayer(P9, bo2Nbr)

                    I7% = P9
                    I8% = bo2Nbr
                    I9% = lineupPlayer(P9, bo2Nbr)
                    Call REPLACEMENTS(I7%, I8%, I9%)

                    I7% = P9
                    I8% = bo1Nbr
                    I9% = lineupPlayer(P9, bo1Nbr)
                    Call REPLACEMENTS(I7%, I8%, I9%)

                    GoTo LChangeBatterFielder

                End If

                If Not (I1 = 3 Or D2 = DEF_PERSONNEL) Then
                    I2 = currLineupSlot(P)
                    GoTo 15555
                End If

                'For I5 = 20 To 23: Locate I5, 28
                '    Print Space$(52)
                'Next I5

                'Return

                '--- END OF A SHORT SUB

        End Select

    Loop Until validChoice = 1


    Do

        Color 14
        Locate 15, 19: Print "LINEUP SLOT TO REPLACE (1-9) (Z)=NO CHANGE"

        I$ = GetKeyPress$

        If Asc(I$) = 32 Then
            If PG = 0 Then
                PG = 1
            Else
                PG = 0
            End If
            Call DisplayBatter_Lineup(PG)
            Call CHANGELINEUP
        End If

        If Asc(I$) = 9 Then
            If TG = 0 Then
                Call DisplayFielding_Lineup(PG, TG)
            Else
                TG = 0
                Call DisplayBatter_Lineup(PG)
            End If
            Call CHANGELINEUP

        End If

        If UCase$(I$) <> "Z" Then
            I2 = Val(I$)
        End If

    Loop Until I2 >= 1 And I2 <= 9 And Not (P9 = D And fldPos(P9, I2) = 1) Or UCase$(I$) = "Z"


    If UCase$(I$) <> "Z" Then

        Locate , 30
        If fldPos(P9, I2) = 1 Then Print pitchers$(P9, currPitcher(P9));
        If fldPos(P9, I2) <> 1 Then Print batters$(P9, lineupPlayer(P9, I2));

        15555 '
        Do
            Locate 22, 30: Print "# OF SUB"

            I$ = GetKeyPress$

            If Asc(I$) = 32 Then
                PG = 1 - PG
                Call DisplayBatter_Lineup(PG)
                Call CHANGELINEUP
            End If

            If Asc(I$) = 9 Then
                If TG = 0 Then
                    Call DisplayFielding_Lineup(PG, TG)
                Else
                    'TG = 1
                    TG = 0
                    Call DisplayBatter_Lineup(PG)
                End If

                Call CHANGELINEUP
            End If

            Call LetterToNumber(I$, I1)

        Loop Until I1 <= 22 Or batRating(P9, I1, SS_B_Avail) <= 0

        Do
            Locate 23, 30
            Print batters$(P9, I1); "... ARE YOU SURE? (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"


        If UCase$(I$) = "Y" Then

            'Make player available and insert into lineup / field
            batRating(P9, I1, SS_B_Avail) = 1
            lineupPlayer(P9, I2) = I1
            fielder(P9, fldPos(P9, I2)) = I1

            If fldPos(P9, I2) = 1 Or fldPos(P9, I2) = 10 Then
                fielder(P9, 1) = 99
                fielder(P9, 0) = I2
                fldPos(P9, I2) = 10
            End If

            'Track game roster with substitution
            playersUsed(P9) = playersUsed(P9) + 1
            gameRoster(P9, 0, playersUsed(P9)) = lineupPlayer(P9, I2)
            gameRoster(P9, 1, playersUsed(P9)) = fldPos(P9, I2)
            gameRoster(P9, 2, playersUsed(P9)) = (I2 * 10) + (playersUsed(P9) - 9)

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
    If Environ$("PBPLOG") = "TRUE" Then
        Close #11
    End If

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

    scoreDiff = gameScore(D, 0) - gameScore(P, 0)

    If scoreDiff <= 1 And baseRunners <> 3 And baseRunners < 6 Then

        If baseRunners <> 1 And baseRunners <> 5 Then

            I2 = 2

        Else

            I2 = 1
            J = runnerOnBase(I2)

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

    If (setupUsed(D) <> -1 And teamMgrProfileVal(D, 589) = 0 And INNING% = teamMgrProfileVal(D, 588)) Or _
       (closerUsed(D) <> -1 And teamMgrProfileVal(D, 589) = 0 And INNING% > teamMgrProfileVal(D, 588)) Or _
       (closerUsed(D) <> -1 And teamMgrProfileVal(D, 589) <> 0 And INNING% >= teamMgrProfileVal(D, 588)) Then
        Call PITSTRAT(pinchHitterFlag, newPitcherFlag, baseRunners)
    End If

    If newPitcherFlag = 1 Then
        newPitcherFlag = 0
        GoTo LManageDefense
    End If

    scoreDiff = gameScore(D, 0) - gameScore(P, 0)
    Call Rolld100(RN0)

    'Look for pitching closer; no manager profile
    If INNING% >= 9 And closerZone(D) <> -1 And teamMgrProfileVal(D, 581) <> 999 Then

        closerZone(D) = -1

        'Only bring in if we're leading
        If scoreDiff <= 3 And scoreDiff >= 0 Then

            'If current pitcher is a reliever
            If gamePitcher(D) <> 1 Then

                'Random chance of replacement
                If d100% <= 80 And pitchRating(D, currPitcher(D), SS_P_Sv) < 15 Then
                    newRelieverNeeded(D) = 1
                End If

            Else

                ' ** STARTING PITCHER **

                ' To avoid division by zero
                If pitchRating(D, currPitcher(D), SS_P_W) > 0 Then
                    pitAvg1 = CInt(pitchRating(D, currPitcher(D), SS_P_CG) / pitchRating(D, currPitcher(D), SS_P_W) * 100)
                Else
                    pitAvg1 = 0
                End If

                If pitchRating(D, currPitcher(D), SS_P_GS) > 0 Then
                    pitAvg5 = CInt(pitchRating(D, currPitcher(D), SS_P_CG) / pitchRating(D, currPitcher(D), SS_P_GS) * 100)
                Else
                    pitAvg5 = 0
                End If

                If (gameScore(P, 0) = 0 And pitchRating(D, currPitcher(D), SS_P_GS) > 0 And RN0 <= pitAvg5 + 10) _
                   Or (pitchRating(D, currPitcher(D), SS_P_GS) > 0 And RN0 <= pitAvg5) _
                   Or (pitchEraOpt > 0 And INNING% > 7 And gameScore(D, 0) > gameScore(P, 0) _
                       And pitchRating(D, currPitcher(D), SS_P_W) > 0 And gamePitcher(D) = 1 _
                       And pitAvg1 <= 100 And RN0 <= pitAvg1) _
                   Or (pitchEraOpt > 0 And INNING% > 7 And (gameScore(D, 0) >= gameScore(P, 0) _
                       Or gameScore(P, 0) - gameScore(D, 0) <= 2) _
                       And pitchRating(D, currPitcher(D), SS_P_W) > 0 And gamePitcher(D) = 1 _
                       And pitAvg1 >= 100) _
                   Or (pitchEraOpt >= 8) Then
   
                    'Reliever
                    'GoTo LReliever1

                Else

                    'New reliever
                    If gamePitcher(D) = 1 Then
                        newRelieverNeeded(D) = 1
                    End If

                End If

            End If

        End If

    End If

    LReliever1:
    If newRelieverNeeded(D) = 1 Then

        'Always call reliever if pinch hitting; otherwise check if batter count
        If pinchHitterFlag = 1 Or (batterMinimumOpt = 0 Or (batterMinimumOpt = 1 And battersFaced(D, currPitcher(D)) >= 3)) Then
            newRelieverNeeded(D) = 0
            Call RELIEVERS(pinchHitterFlag, newPitcherFlag, baseRunners)
        End If
        If useVGA = 1 Then
            _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
        End If
        GoTo LManageDefense

    End If

    If fldPos(P, currLineupSlot(P)) = 1 Then
        BR! = 0
    Else
        BR! = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt)
    End If

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

                If Environ$("MGRLOG") = "TRUE" Then
                    Print #11, "!! Team is using a INFIELD IN (3rd base occupied) strategy !!"
                End If

                ' PROFILE INFIELD IN WITH 3RD OCCUPIED
                Call Rolld100(RN0)

                If (nbrOuts(0) = 0 And _
                       scoreDiff >= teamMgrProfileVal(D, 691) * -1 And scoreDiff <= teamMgrProfileVal(D, 692) * -1 And _
                       INNING% >= teamMgrProfileVal(D, 693) And RN0 <= teamMgrProfileVal(D, 694)) Or _
                   (nbrOuts(0) = 0 And _
                       scoreDiff >= teamMgrProfileVal(D, 695) * -1 And scoreDiff <= teamMgrProfileVal(D, 696) * -1 And _
                       INNING% >= teamMgrProfileVal(D, 697) And RN0 <= teamMgrProfileVal(D, 698)) Or _
                   (nbrOuts(0) = 1 And (baseRunners = 5 Or baseRunners = 7) And _
                       scoreDiff >= teamMgrProfileVal(D, 699) * -1 And scoreDiff <= teamMgrProfileVal(D, 700) * -1 And _
                       INNING% >= teamMgrProfileVal(D, 701) And RN0 <= teamMgrProfileVal(D, 702)) Or _
                   (nbrOuts(0) = 1 And (baseRunners = 5 Or baseRunners = 7) And _
                       scoreDiff >= teamMgrProfileVal(D, 703) * -1 And scoreDiff <= teamMgrProfileVal(D, 704) * -1) Then
                    'Defense = Infield in
                    INFPOS% = INF_IN
                    D2 = DEF_INFIELD_IN
                    GoTo LManageDefense
                End If

            End If

        End If

        If teamMgrProfileVal(D, 707) = 999 Then

            If baseRunners = 0 Then

                If Environ$("MGRLOG") = "TRUE" Then
                    Print #11, "!! Team is using a CORNERS IN against BUNT FOR HIT strategy !!"
                End If

                ' PROFILE CORNERS IN AGAINST A BUNT HIT
                Call Rolld100(RN0)

                If BR! >= teamMgrProfileVal(D, 708) And RN0 <= teamMgrProfileVal(D, 709) Or _ 
                    BR! >= teamMgrProfileVal(D, 710) And RN0 <= teamMgrProfileVal(D, 711) Or _ 
                    BR! >= teamMgrProfileVal(D, 712) And RN0 <= teamMgrProfileVal(D, 713) Then
                    'Defense - Corners in
                    INFPOS% = INF_CORNERS_IN
                    D2 = DEF_CORNERS_IN
                    GoTo LManageDefense
                End If

            End If

        End If

        If teamMgrProfileVal(D, 714) = 999 And fldPos(P, currLineupSlot(P)) <> 1 And _ 
            (baseRunners = 1 And baseRunners = 4) And _ 
            nbrOuts(0) = 0 Then

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "!! Team is using a CORNERS IN against SAC BUNT strategy !!"
            End If

            ' PROFILE CORNERS IN VS HITTER IN SAC BUNT SITUATION
            Call Rolld100(RN0)

            If ((baseRunners = 1 And _
                 scoreDiff <= teamMgrProfileVal(D, 715) And scoreDiff >= teamMgrProfileVal(D, 716) * -1 And _
                 BR! >= teamMgrProfileVal(D, 717) And RN0 <= teamMgrProfileVal(D, 718)) _
             Or (baseRunners = 1 And _
                 scoreDiff <= teamMgrProfileVal(D, 719) And scoreDiff >= teamMgrProfileVal(D, 720) * -1 And _
                 BR! >= teamMgrProfileVal(D, 721) And RN0 <= teamMgrProfileVal(D, 722)) _
             Or (baseRunners = 4 And _
                 scoreDiff <= teamMgrProfileVal(D, 723) And scoreDiff >= teamMgrProfileVal(D, 724) * -1 And _
                 BR! >= teamMgrProfileVal(D, 725) And RN0 <= teamMgrProfileVal(D, 726)) _
             Or (baseRunners = 4 And _
                 scoreDiff <= teamMgrProfileVal(D, 727) And scoreDiff >= teamMgrProfileVal(D, 728) * -1 And _
                 BR! >= teamMgrProfileVal(D, 729) And RN0 <= teamMgrProfileVal(D, 730))) Then

                'Defense - Corners In
                INFPOS% = INF_CORNERS_IN
                D2 = DEF_CORNERS_IN
                GoTo LManageDefense
            End If

        End If

        If teamMgrProfileVal(D, 731) = 999 And fldPos(P, currLineupSlot(P)) = 1 And _ 
            (baseRunners = 1 Or baseRunners = 2 Or baseRunners = 4) And _
            nbrOuts(0) <> 2 Then

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "!! Team is using a CORNERS IN against PITCHER SAC BUNT strategy !!"
            End If

            ' PROFILE CORNERS IN VS A PITCHER IN SAC BUNT SITUATION
            Call Rolld100(RN0)

            If ((I7! <= teamMgrProfileVal(D, 732) And RN0 <= teamMgrProfileVal(D, 733) And baseRunners = 1 And nbrOuts(0) <= 1) Or _
                (I7! <= teamMgrProfileVal(D, 734) And RN0 <= teamMgrProfileVal(D, 735) And baseRunners = 1 And nbrOuts(0) <= 1) Or _
                (I7! <= teamMgrProfileVal(D, 736) And RN0 <= teamMgrProfileVal(D, 737) And (baseRunners = 2 Or baseRunners = 4) And nbrOuts(0) = 0) Or _
                (I7! <= teamMgrProfileVal(D, 738) And RN0 <= teamMgrProfileVal(D, 739) And (baseRunners = 2 Or baseRunners = 4) And nbrOuts(0) = 0)) Then
                'Defense - Corners In
                INFPOS% = INF_CORNERS_IN
                D2 = DEF_CORNERS_IN
                GoTo LManageDefense
            End If

        End If

        If teamMgrProfileVal(D, 740) = 999 And nbrOuts(0) <> 2 And _ 
            (baseRunners = 3 Or baseRunners >= 5) Then

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "!! Team is using a CORNERS IN against SQUEEZE strategy !!"
            End If

            ' PROFILE CORNERS IN FOR A SQUEEZE
            Call Rolld100(RN0)
            If nbrOuts(0) = 0 And scoreDiff <= teamMgrProfileVal(D, 741) And scoreDiff >= teamMgrProfileVal(D, 742) And BR! >= teamMgrProfileVal(D, 743) And RN0 <= teamMgrProfileVal(D, 744) Then
                INFPOS% = INF_CORNERS_IN
                D2 = DEF_CORNERS_IN
                GoTo LManageDefense
            End If
            If nbrOuts(0) = 1 And scoreDiff <= teamMgrProfileVal(D, 745) And scoreDiff >= teamMgrProfileVal(D, 746) And BR! >= teamMgrProfileVal(D, 747) And RN0 <= teamMgrProfileVal(D, 748) Then
                INFPOS% = INF_CORNERS_IN
                D2 = DEF_CORNERS_IN
                GoTo LManageDefense
            End If

        End If

        If teamMgrProfileVal(D, 756) = 999 And _ 
            (baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6) Then

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "!! Team is using a PITCH AROUND HITTER strategy !!"
            End If

            MOB% = 0

            For I = 1 To 3
                If runnerOnBase(I) > 0 Then MOB% = MOB% + 1
            Next

            If scoreDiff + MOB% + 1 < 0 Then
                ' BATTER NOT TYING OR WINNING RUN
                Call Rolld100(RN0)

                If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                    I7! = 0
                    I6! = 0
                Else
                    I6! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)

                    If (I6! <= teamMgrProfileVal(D, 757) And INNING% >= teamMgrProfileVal(D, 758) And RN0 <= teamMgrProfileVal(D, 759)) _
                       Or (I6! <= teamMgrProfileVal(D, 760) And INNING% >= teamMgrProfileVal(D, 761) And RN0 <= teamMgrProfileVal(D, 762)) _
                       Or (I6! <= teamMgrProfileVal(D, 763) And INNING% >= teamMgrProfileVal(D, 764) And RN0 <= teamMgrProfileVal(D, 765)) Then
                        'Defense - Pitch around
                        D2 = DEF_PITCH_AROUND
                        GoTo LReliever2
                    End If

                End If

            End If

        End If

        scoreDiff = gameScore(D, 0) - gameScore(P, 0)

        If scoreDiff >= -1 And scoreDiff <= 1 And INNING% >= 8 And _ 
            (baseRunners = 3 Or baseRunners >= 5) And _ 
            nbrOuts(0) < 2 Then
            'Defense - Infeidl in
            INFPOS% = INF_IN
            D2 = DEF_INFIELD_IN

        Else

            If (scoreDiff > 0 Or nbrOuts(0) > 0 Or baseRunners <= 2 Or baseRunners = 4 Or (baseRunners = 5 And nbrOuts(0) = 1) Or (INNING% <= 5 And scoreDiff < -2) Or (INNING% > 5 And scoreDiff < -4)) _
               And ((baseRunners = 1 Or baseRunners = 4) And nbrOuts(0) <> 2 And (currLineupSlot(P) < 3 Or currLineupSlot(P) > 6) And (INNING% > 6 Or (Abs(scoreDiff) < 3))) _
               And (INNING% <= 7 Or scoreDiff = 0) Then
                'Defense - Corners In
                INFPOS% = INF_CORNERS_IN
                D2 = DEF_CORNERS_IN
            Else
                'Defense - Infield in
                INFPOS% = INF_IN
                D2 = DEF_INFIELD_IN
            End If

        End If

    End If

    If scoreDiff > 0 And baseRunners > 1 And baseRunners <> 4 And baseRunners <> 7 And fldPos(P, currLineupSlot(P)) <> 1 Then

        If INNING% >= 9 And P = 1 And scoreDiff = 0 And ((nbrOuts(0) < 2 And baseRunners >= 2 And baseRunners <= 3) Or (nbrOuts(0) = 2 And baseRunners >= 5 And baseRunners <= 6)) Then
            D2 = DEF_INTNTL_BB
            GoTo LManageDefense
        End If

        currbAvg = game_batRating(P, GM_BAvg, currLineupSlot(P))
        currHR = game_batRating(P, GM_HR, currLineupSlot(P))

        If currLineupSlot(P) = 9 Then
            I2 = game_batRating(P, GM_BAvg, 0)
            I3 = game_batRating(P, GM_HR, 0)
        Else
            I2 = game_batRating(P, GM_BAvg, currLineupSlot(P) + 1)
            I3 = game_batRating(P, GM_HR, currLineupSlot(P) + 1)
        End If

        If scoreDiff < 1 And INNING% > 8 And _
            (baseRunners = 2 Or baseRunners = 6) And _
            ((currbAvg - I2 >= 50 And currHR - I3 >= 50) Or (currbAvg >= 280 And currHR >= 140 And currHR > I3)) Then

            D2 = DEF_INTNTL_BB
            GoTo LManageDefense

        End If

    End If

    LReliever2:
    If (pitcherStam(D, 0) <= teamPitStats(D, currPitcher(D), 4) + teamPitStats(D, currPitcher(D), 1) And _ 
        pitcherStam(D, 1) <= teamPitStats(D, currPitcher(D), 2)) Or _ 
        fielder(D, 1) = 99 Then

        'Always call reliever if pinch hitting; otherwise check if batter count
        If pinchHitterFlag = 1 Or (batterMinimumOpt = 0 Or (batterMinimumOpt = 1 And battersFaced(D, currPitcher(D)) >= 3)) Then
            Call RELIEVERS(pinchHitterFlag, newPitcherFlag, baseRunners)
        End If
        If useVGA = 1 Then
            _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
        End If
    End If

    GoTo LManageDefense


    '----- NOODLE -----

    'GET A PINCH HITTER FOR A POSITION PLAYER
    LPhForPosition:
    If teamMgrProfileVal(P, 540) = 999 Then

        If Environ$("MGRLOG") = "TRUE" Then
            Print #11, "!! Team is using a PINCH HITTING strategy !!"
        End If

        'SELECT PH FOR HITTER
        For I = 0 To 22

            If batRating(P, I, SS_B_Avail) <= 0 And batters$(P, I) <> "XXX" And _ 
                (fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos1) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos2) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos3) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos4)) Then

                If pitchRating(D, currPitcher(D), SS_P_Throws) < 0 Then
                    I1 = 1
                Else
                    I1 = 6
                End If

                For I = I1 To I1 + 4

                    If teamMgrProfileVal(P, I1 + 540) <> -1 And batRating(P, teamMgrProfileVal(P, I1 + 540), SS_B_Avail) = 0 Then

                        teamMgrProfileVal = teamMgrProfileVal(P, I1 + 540)
                        teamMgrProfileVal(P, I1 + 540) = -1

                        pinchHitterFlag = 1

                        'Make player available and insert into lineup / field
                        batRating(P, I, SS_B_Avail) = 1
                        lineupPlayer(P, currLineupSlot(P)) = teamMgrProfileVal
                        fielder(P, fldPos(P, currLineupSlot(P))) = teamMgrProfileVal

                        If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then
                            fielder(P, 1) = 99
                            fielder(P, 0) = currLineupSlot(P)
                            fldPos(P, currLineupSlot(P)) = 10
                        End If

                        'Track game roster with substitution
                        playersUsed(P) = playersUsed(P) + 1
                        gameRoster(P, 0, playersUsed(P)) = teamMgrProfileVal
                        gameRoster(P, 1, playersUsed(P)) = fldPos(P, currLineupSlot(P))
                        gameRoster(P, 2, playersUsed(P)) = (currLineupSlot(P) * 10) + (playersUsed(P) - 9)

                        I7% = P
                        I8% = currLineupSlot(P)
                        I9% = lineupPlayer(P, currLineupSlot(P))
                        Call REPLACEMENTS(I7%, I8%, I9%)

                        GoTo LPinchHitter

                    End If

                Next

            End If

        Next

    End If


    'Only pinch hit for position in innings 6+
    If INNING% >= 6 Then

        For I = 0 To 22:

            If batRating(P, I, SS_B_Avail) <= 0 And batters$(P, I) <> "XXX" And _
                batRating(P, I, SS_B_AB) > 0 And _
                (fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos1) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos2) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos3) Or fldPos(P, currLineupSlot(P)) = batRating(P, I, SS_B_Pos4)) Then

                pinchBAvg = batRating(P, I, SS_B_H) / batRating(P, I, SS_B_AB) * 1000
                battrBAvg = batRating(P, JM, SS_B_H) / batRating(P, JM, SS_B_AB) * 1000

                ' Randomize chance of PH entering game, based on his AB's in the season.
                'This will limit players who hardly played.  (500 is arbitrary number)
                abRand = Int(Rnd(1) * 500)

                iPitThrows = pitchRating(D, currPitcher(D), SS_P_Throws) ' pitcher throws L/R
                iBatHits = batRating(P, I, SS_B_Bats) ' batter hits L/Sw/R

                ' Pitcher and PH both righty
                If iPitThrows = 1 And iBatHits > 0 And pinchBAvg > battrBAvg + 35 Then
                    If abRand < batRating(P, I, SS_B_AB) Then GoTo LPhForPosPlayer
                End If

                ' Pitcher and PH both lefty
                If iPitThrows = -1 And iBatHits < 0 And pinchBAvg > battrBAvg + 45 Then
                    If abRand < batRating(P, I, SS_B_AB) Then GoTo LPhForPosPlayer
                End If

                ' Pitcher and PH on opposite sides
                If (iPitThrows = -1 And iBatHits >= 0 Or iPitThrows = 1 And iBatHits <= 0) And pinchBAvg > battrBAvg + 30 Then
                    If abRand < batRating(P, I, SS_B_AB) Then GoTo LPhForPosPlayer
                End If

            End If

        Next I

    End If

    'Pinch hit considered; now look for pinch runner
    GoTo LPinchRunner



    LPhForPosPlayer:
    pinchHitterFlag = 1

    'Make player available and insert into lineup / field
    batRating(P, I, SS_B_Avail) = 1
    lineupPlayer(P, currLineupSlot(P)) = I
    fielder(P, fldPos(P, currLineupSlot(P))) = I

    If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then
        fielder(P, 1) = 99
        fielder(P, 0) = currLineupSlot(P)
        fldPos(P, currLineupSlot(P)) = 10
    End If

    'Track game roster with substitution
    playersUsed(P) = playersUsed(P) + 1
    gameRoster(P, 0, playersUsed(P)) = I
    gameRoster(P, 1, playersUsed(P)) = fldPos(P, currLineupSlot(P))
    gameRoster(P, 2, playersUsed(P)) = (currLineupSlot(P) * 10) + (playersUsed(P) - 9)

    I7% = P
    I8% = currLineupSlot(P)
    I9% = lineupPlayer(P, currLineupSlot(P))
    Call REPLACEMENTS(I7%, I8%, I9%)

    GoTo LPinchHitter


    '----- NOODLE -----

    LPhForPitcher: 'GET A PINCH HITTER FOR THE PITCHER
    callPinch = 0
    skipLoop = 0

    'If no manager profile
    If teamMgrProfileVal(P, 540) <> 999 Then

        J% = 0
        GP% = 0

        ' To avoid division by zero
        '_IIF only in QB64PE v4.0
        If pitchRating(P, currPitcher(P), SS_P_W) > 0 Then
            pitAvg = CInt(pitchRating(P, currPitcher(P), SS_P_CG) / pitchRating(P, currPitcher(P), SS_P_W) * 100)
        Else
            pitAvg = 0
        End If

        If desigHit = 1 Or _
            (pitchEraOpt > 1 And INNING% < 4) Or _
            (pitchEraOpt > 2 And INNING% < 7) Or _
            (pitchEraOpt > 3 And INNING% < 9) Or _
            (pitchEraOpt >= 8) Or _
            ( pitchEraOpt > 0 And INNING% > 7 And gameScore(P, 0) > gameScore(D, 0) And _
            pitchRating(P, currPitcher(P), SS_P_W) > 0 And gamePitcher(P) = 1 And _
            pitAvg <= 100 And RN0 <= pitAvg ) Or _
            ( pitchEraOpt > 0 And INNING% > 7 And (gameScore(P, 0) >= gameScore(D, 0) Or _
            gameScore(D, 0) - gameScore(P, 0) <= 2) And pitchRating(P, currPitcher(P), SS_P_W) > 0 And _
            gamePitcher(P) = 1 And pitAvg >= 100 ) Then
            skipLoop = 1
        End If

        If skipLoop = 0 Then

            For idx = 0 To 22

                If batters$(P, idx) <> "XXX" And batRating(P, idx, SS_B_Avail) <= 0 And batRating(P, idx, SS_B_AB) > 0 Then

                    If batRating(P, idx, SS_B_Bats) > 0 And pitchRating(D, currPitcher(D), SS_P_Throws) = 1 Or batRating(P, idx, SS_B_Bats) < 0 And pitchRating(D, currPitcher(D), SS_P_Throws) = -1 Then
                        PH%(J%, 0) = CInt(batRating(P, idx, SS_B_G) / 4)
                    Else
                        PH%(J%, 0) = batRating(P, idx, SS_B_G)
                    End If

                    PH%(J%, 1) = idx
                    GP% = GP% + PH%(J%, 0)
                    J% = J% + 1

                End If

            Next

            If J% <> 0 Then

                RN0 = Int(Rnd(1) * GP%) + 1

                I = 0
                Do While I <= J% - 1 And callPinch = 0
                    If RN0 <= PH%(I, 0) Then
                        PH% = PH%(I, 1)
                        callPinch = 1
                    Else
                        PH%(I + 1, 0) = PH%(I, 0) + PH%(I + 1, 0)
                        I = I + 1
                    End If
                Loop

            End If

        End If 'Done checking of we should skip the loop

        If callPinch = 1 Then

            pinchHitterFlag = 1

            'Make player available and insert into lineup / field
            batRating(P, PH%, SS_B_Avail) = 1
            lineupPlayer(P, currLineupSlot(P)) = PH%

            If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then
                fielder(P, 1) = 99
                fielder(P, 0) = currLineupSlot(P)
                fldPos(P, currLineupSlot(P)) = 10
            End If

            'Track game roster with substitution
            playersUsed(P) = playersUsed(P) + 1
            gameRoster(P, 0, playersUsed(P)) = PH%
            gameRoster(P, 1, playersUsed(P)) = 10 'pinch hitter'
            gameRoster(P, 2, playersUsed(P)) = 81 + playersUsed(P)

            LPinchHitter:
            P9 = P
            W5 = P9
            W6 = P9

            VV(P9) = currLineupSlot(P)
            VV1(P9) = currLineupSlot(P)

            Call DisplayGameState(nbrBalls, nbrStrikes, MO, INFPOS%)

            VV(P9) = 1
            VV1(P9) = 9

            player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))

            If fldPos(P, currLineupSlot(P)) = 1 Then
                player$(0) = pitchers$(P, currPitcher(D))
            End If
            Call WINDEX
            Call PBP(gameManagers$(P) + " is sending up a pinch hitter")
            pinchHitterFlag = 1

            GoTo LContinueBatter

        End If

        LPinchRunner:
        'If pinch runner hasn't been used:
        If runnerUsed <> 1 Then
            Call PINCHRUNNER(scoreDiff, baseRunners, I2, nbrBalls, nbrStrikes, MO, INFPOS%, runnerUsed, madeSub)
        End If

        If madeSub = 1 Then
            madeSub = 0
            GoTo LContinueBatter
        End If

        If fldPos(P, currLineupSlot(P)) = 1 Then
            BR! = 0
        Else
            BR! = batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt)
        End If

        Call Rolld100(roll)
        If baseRunners = 0 And (INFPOS% > INF_BACK And roll <= 5 Or INFPOS% = INF_BACK And roll <= 5) And BR! > 8 Then
            offChoice = OFF_BUNT
            GoTo LBunt
        End If

        ' MANAGER PROFILE TO HIT AND RUN
        Call Rolld100(roll)

        If baseRunners <> 0 And teamMgrProfileVal(P, 671) = 999 And fldPos(P, currLineupSlot(P)) <> 1 Then

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "!! Team is using a HIT & RUN strategy !!"
            End If

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

            If ((scoreDiff <= teamMgrProfileVal(P, 672) And scoreDiff >= teamMgrProfileVal(P, 673) * -1 And baseRunners = 1 And nbrOuts(0) = 0 And roll <= teamMgrProfileVal(P, 674) And I7! <= teamMgrProfileVal(P, 872) And I6! >= teamMgrProfileVal(P, 866)) Or _
               (scoreDiff <= teamMgrProfileVal(P, 675) And scoreDiff >= teamMgrProfileVal(P, 676) * -1 And baseRunners = 1 And nbrOuts(0) = 1 And roll <= teamMgrProfileVal(P, 677) And I7! <= teamMgrProfileVal(P, 873) And I6! >= teamMgrProfileVal(P, 867)) Or _
               (scoreDiff <= teamMgrProfileVal(P, 678) And scoreDiff >= teamMgrProfileVal(P, 679) * -1 And baseRunners = 5 And nbrOuts(0) = 0 And roll <= teamMgrProfileVal(P, 680) And I7! <= teamMgrProfileVal(P, 874) And I6! >= teamMgrProfileVal(P, 868)) Or _
               (scoreDiff <= teamMgrProfileVal(P, 681) And scoreDiff >= teamMgrProfileVal(P, 682) * -1 And baseRunners = 5 And nbrOuts(0) = 1 And roll <= teamMgrProfileVal(P, 683) And I7! <= teamMgrProfileVal(P, 875) And I6! >= teamMgrProfileVal(P, 869)) Or _
               (scoreDiff <= teamMgrProfileVal(P, 684) And scoreDiff >= teamMgrProfileVal(P, 685) * -1 And baseRunners = 4 And nbrOuts(0) = 0 And roll <= teamMgrProfileVal(P, 686) And I7! <= teamMgrProfileVal(P, 876) And I6! >= teamMgrProfileVal(P, 870)) Or _
               (scoreDiff <= teamMgrProfileVal(P, 687) And scoreDiff >= teamMgrProfileVal(P, 688) * -1 And baseRunners = 4 And nbrOuts(0) = 1 And roll <= teamMgrProfileVal(P, 689) And I7! <= teamMgrProfileVal(P, 877) And I6! >= teamMgrProfileVal(P, 871))) Then
                'Offense - Hit & Run
                offChoice = OFF_HIT_N_RUN
                GoTo LHitAndRun
            End If

        End If

        ' Check if pitcher is batting
        If fldPos(P, currLineupSlot(P)) = 1 Then

            ' PITCHER BUNTING LOGIC

            ' Check if manager profile exists for pitcher bunting
            If teamMgrProfileVal(P, 609) <> 999 Then

                ' Use manager's pitcher bunting preferences
                If (baseRunners = 1 Or baseRunners = 2 Or baseRunners = 4) Then

                    If Environ$("MGRLOG") = "TRUE" Then
                        Print #11, "!! Team is using a PITCHER BUNTING strategy !!"
                    End If

                    Call Rolld100(roll)

                    If ((scoreDiff <= teamMgrProfileVal(P, 611) And scoreDiff >= teamMgrProfileVal(P, 612) * -1 And baseRunners = 1 And INNING% <= 7 And teamMgrProfileVal(P, 610) >= nbrOuts(0) And roll <= teamMgrProfileVal(P, 613)) Or _
                       (scoreDiff <= teamMgrProfileVal(P, 615) And scoreDiff >= teamMgrProfileVal(P, 616) * -1 And baseRunners = 1 And INNING% > 7 And teamMgrProfileVal(P, 614) >= nbrOuts(0) And roll <= teamMgrProfileVal(P, 617)) Or _
                       (scoreDiff <= teamMgrProfileVal(P, 619) And scoreDiff >= teamMgrProfileVal(P, 620) * -1 And baseRunners = 2 And INNING% <= 7 And teamMgrProfileVal(P, 618) >= nbrOuts(0) And roll <= teamMgrProfileVal(P, 621)) Or _
                       (scoreDiff <= teamMgrProfileVal(P, 623) And scoreDiff >= teamMgrProfileVal(P, 624) * -1 And baseRunners = 2 And INNING% > 7 And teamMgrProfileVal(P, 622) >= nbrOuts(0) And roll <= teamMgrProfileVal(P, 625)) Or _
                       (scoreDiff <= teamMgrProfileVal(P, 627) And scoreDiff >= teamMgrProfileVal(P, 628) * -1 And baseRunners = 4 And INNING% <= 7 And teamMgrProfileVal(P, 626) >= nbrOuts(0) And roll <= teamMgrProfileVal(P, 629)) Or _
                       (scoreDiff <= teamMgrProfileVal(P, 631) And scoreDiff >= teamMgrProfileVal(P, 632) * -1 And baseRunners = 4 And INNING% > 7 And teamMgrProfileVal(P, 630) >= nbrOuts(0) And roll <= teamMgrProfileVal(P, 633))) Then
                        'Offense - Bunt
                        offChoice = OFF_BUNT
                        GoTo LBunt
                    End If

                End If

            Else
                ' No manager profile - use era-based pitcher bunting
                If buntDecision%(baseRunners) = 1 Then
                    offChoice = OFF_BUNT
                    GoTo LBunt
                End If

            End If

        Else

            ' POSITION PLAYER BUNTING LOGIC

            ' Check if manager profile exists for position player bunting
            If teamMgrProfileVal(P, 634) = 999 Then

                ' Use manager's position player bunting preferences
                If nbrOuts(0) = 0 Or (baseRunners <> 1 And baseRunners <> 2 And baseRunners <> 4) Then

                    If Environ$("MGRLOG") = "TRUE" Then
                        Print #11, "!! Team is using a POSITION PLAYER BUNTING strategy !!"
                    End If

                    ' Calculate batter stats
                    If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) = 0 Then
                        I7! = 0
                        I6! = 0
                    Else
                        I7! = CInt(batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                        I6! = CInt((batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_H) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_2B) + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_3B) * 2 + batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_HR) * 3) / batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_AB) * 1000)
                    End If

                    Call Rolld100(roll)

                    If ((baseRunners = 1 And INNING% <= 7 And scoreDiff <= teamMgrProfileVal(P, 635) And scoreDiff >= teamMgrProfileVal(P, 636) * -1 And I7! <= teamMgrProfileVal(P, 637) And I6! <= teamMgrProfileVal(P, 638) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 639) And roll <= teamMgrProfileVal(P, 640)) Or _
                       (baseRunners = 2 And INNING% <= 7 And scoreDiff <= teamMgrProfileVal(P, 641) And scoreDiff >= teamMgrProfileVal(P, 642) * -1 And I7! <= teamMgrProfileVal(P, 643) And I6! <= teamMgrProfileVal(P, 644) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 645) And roll <= teamMgrProfileVal(P, 646)) Or _
                       (baseRunners = 4 And INNING% <= 7 And scoreDiff <= teamMgrProfileVal(P, 647) And scoreDiff >= teamMgrProfileVal(P, 648) * -1 And I7! <= teamMgrProfileVal(P, 649) And I6! <= teamMgrProfileVal(P, 650) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 651) And roll <= teamMgrProfileVal(P, 652)) Or _
                       (baseRunners = 1 And INNING% > 7 And scoreDiff <= teamMgrProfileVal(P, 653) And scoreDiff >= teamMgrProfileVal(P, 654) * -1 And I7! <= teamMgrProfileVal(P, 655) And I6! <= teamMgrProfileVal(P, 656) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 657) And roll <= teamMgrProfileVal(P, 658)) Or _
                       (baseRunners = 2 And INNING% > 7 And scoreDiff <= teamMgrProfileVal(P, 659) And scoreDiff >= teamMgrProfileVal(P, 660) * -1 And I7! <= teamMgrProfileVal(P, 661) And I6! <= teamMgrProfileVal(P, 662) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 663) And roll <= teamMgrProfileVal(P, 664)) Or _
                       (baseRunners = 4 And INNING% > 7 And scoreDiff <= teamMgrProfileVal(P, 665) And scoreDiff >= teamMgrProfileVal(P, 666) * -1 And I7! <= teamMgrProfileVal(P, 667) And I6! <= teamMgrProfileVal(P, 668) And pitchRating(D, currPitcher(D), SS_P_ERA100) <= teamMgrProfileVal(P, 669) And roll <= teamMgrProfileVal(P, 670))) Then
                        'Offense - Bunt
                        offChoice = OFF_BUNT
                        GoTo LBunt
                    End If

                End If

            Else
                ' No manager profile - use era-based position player bunting
                If buntDecision%(baseRunners) = 1 Then
                    offChoice = OFF_BUNT
                    GoTo LBunt
                End If

            End If

        End If

        'Bunt not chosen - "Hit away" strategy
        offChoice = OFF_HIT_AWAY

        If ((baseRunners = 1 Or baseRunners = 5) And (scoreDiff > -1 Or batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_SB) >= 10) And stealPhase(1) <= 1 And (scoreDiff < -1 Or baseRunners <> 5) And scoreDiff >= -5 And _ 
            (fldPos(P, runnerOnBase(1)) <> 1 Or baseRunners <> 1) And _ 
            (fldPos(P, runnerOnBase(2)) <> 1 Or baseRunners <> 2) And _ 
            (scoreDiff >= 0 Or INNING% < 7)) Or _
            ((baseRunners = 2 Or baseRunners = 4) And batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_SB) >= 40 And fldPos(P, currLineupSlot(P)) <> 1 And nbrOuts(0) <> 2 And stealPhase(2) <= 1) Then                    
            'Offense Steal
            offChoice = OFF_STEAL
            GoTo LSteal
        Else
            GoTo LNoSteal
        End If

        LNoSteal:
        'If Not (baseRunners = 0 Or nbrOuts(0) > 0 Or baseRunners = 7 Or baseRunners = 6 Or baseRunners = 3 Or baseRunners = 5 Or nbrStrikes = 2 Or P = 0 And INNING% > 7) Then
        If (baseRunners = 1 Or baseRunners = 2 Or baseRunners = 4) And nbrOuts(0) = 0 And nbrStrikes <> 2 And (P <> 0 Or INNING% <= 7) Then

            If fldPos(P, currLineupSlot(P)) = 1 And baseRunners = 1 And nbrOuts(0) < 2 Or fldPos(P, currLineupSlot(P)) = 1 And baseRunners = 2 And nbrOuts(0) = 0 Or fldPos(P, currLineupSlot(P)) = 1 And baseRunners = 4 And nbrOuts(0) = 0 Then
                GoTo LPinchRunner1
            End If

            If currLineupSlot(P) > 2 And currLineupSlot(P) < 6 Or game_batRating(P, GM_BAvg, currLineupSlot(P)) > 290 And fldPos(P, currLineupSlot(P)) <> 1 Or fldPos(P, currLineupSlot(P)) <> 1 And game_batRating(P, GM_HR, currLineupSlot(P)) > 70 Then
                GoTo LPinchRunner0
            End If

            If INNING% > 7 And scoreDiff = -1 And nbrOuts(0) = 0 And baseRunners = 1 Then
                GoTo LPinchRunner1
            End If

        End If

    End If

    LPinchRunner0:
    pinchHitterFlag = 0
    runnerUsed = 0

    ' Set hit & run threshold based on era
    'Higher threshold = fewer 2B/3B/HR
    'Lower threshold = more 2B/3B/HR
    Select Case gameYear

        Case Is <= 1919:
            hitRunThreshold = 30

        Case 1920 To 1945:
            hitRunThreshold = 22

        Case 1946 To 1975:
            hitRunThreshold = 20

        Case 1976 To 1993:
            hitRunThreshold = 15

        Case 1994 To 2005:
            hitRunThreshold = 10

        Case Is >= 2006
            hitRunThreshold = 9

    End Select


    'Decide whether it's safe to Hit/Run
    '  At this point, the PC has:
    '   1) Checked if a bunt makes sense
    '   2) Defaulted to "Hit Away"
    '   3) Checked for a steal

    If scoreDiff < -3 Or game_batRating(P, GM_K, currLineupSlot(P)) > 100 Or _
        (baseRunners = 0 Or baseRunners = 2 Or baseRunners = 3 Or baseRunners = 6 Or baseRunners = 7) Or _
        nbrOuts(0) = 2 Or d100% > hitRunThreshold Then
        'Not a good candidate for hit/run
        ' *- Team is way behind
        ' *- Batter has high strikeout rating
        ' *- Several base situations
        ' *- Already have 2 OUTS
        ' *- Limited % chance even in optimal conditions
        GoTo LExecuteOffense
    Else
        offChoice = OFF_HIT_N_RUN
        GoTo LHitAndRun
    End If

    '----- NOODLE -----

    LPinchRunner1:
    If nbrOuts(0) >= 1 And INNING% >= 7 Then
        GoTo LPinchRunner0
    Else
        offChoice = OFF_BUNT
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

    Shared bestBatterFPos(), bLineupIdx()
    Shared bestBatterIdx(), gameMgrIdx(), mgrFile()
    Shared monthlyRatings(), nbrInnings(), oppScore()
    Shared startPitcher()
    Shared setupUsed(), closerUsed(), teamScore()

    Shared statTeamBat!(), statTeamPit!()

    Shared dayNight$(), desigHitOpt$(), gameType$()
    Shared league_brief$(), leagueType$(), locIndicator$()
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

    genericNames$(1) = "RHODES"
    genericNames$(2) = "WINBERRY"
    genericNames$(3) = "TIPPETT"
    genericNames$(4) = "MILLER"

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

    league_brief$(0) = "AL"
    league_brief$(1) = "NL"

    desigHitOpt$(0) = "NO DH "
    desigHitOpt$(1) = "USE DH"

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
        currPitcher(I) = -1
        gamePitcher(I) = -1
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
    chanceRoll = 0
    HR = 0
    injCount = 0
    INNING% = 0
    inningsPitched = 0
    noLUFound = 0
    pbpLine = 0
    precipType = 4
    rainDelay = 0
    rainStart = 0
    SB = 0
    skyType = 0
    statSpecial = 0
    TR = 0
    userDone = 0
    windSpeed = 0

    windImpact! = 0

    actualAttendance& = 0
    avgAttendance& = 0

    fileString$ = ""
    gameL$ = ""
    gameTime$ = ""
    gameW$ = ""
    chosenPark$ = ""
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

    Erase bestBatterFPos, bLineupIdx
    Erase bRunner, b1Pitcher, b2Pitcher
    Erase baseMove, battersFaced, bestBatterIdx
    Erase CSS, currLineupSlot, currPitcher, fielder
    Erase gameAtBats, game_batRating, gameInnPit
    Erase gameMgrIdx, gamePitcher, gameRoster, gameScore
    Erase inningScore, leftOnBase, lineupAssigned
    Erase mgrFile, monthlyRatings
    Erase nbrInnings, nbrOuts, newRelieverNeeded
    Erase oppBatStats, oppPitStats, oppScore
    Erase pitcherStam, playersUsed, pitcherOfRecord
    Erase relieverOnRecordForSave, runnerOnBase
    Erase schedOptions, startPitcher, stealPhase
    Erase teamBatStats, teamPitStats, teamDPs, totalDPs
    Erase teamMgrProfileVal, teamScore, totalStats
    Erase usedUmpires

    Erase VV, VV1

    Erase parkHRVals!, statTeamBat!, statTeamPit!

    Erase diskIDs$, doublesPlayer$
    Erase eventDesc$, ejections$
    Erase gameTeams$
    Erase homeRuns$, injPlayers$, locIndicator$, oppName$
    Erase parkNames$, player$, pbpText$
    Erase Q3$, stolenBases$, triplesPlayer$, umpireName$

    For team = 0 To 1
        For I3 = 0 To 21
            If pitchRating(team, I3, SS_P_Avail) <> 99 Then
                pitchRating(team, I3, SS_P_Avail) = 0
            End If
        Next
    Next

    For I = 0 To 1
        For I1 = 0 To 23
            gameRoster(I, 0, I1) = -1
            gameRoster(I, 2, I1) = 100
        Next I1
    Next I

    For I = 0 To 1
        For I1 = 0 To 22
            If batRating(I, I1, SS_B_Avail) <> 99 Then
                batRating(I, I1, SS_B_Avail) = 0
            End If
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

    char$ = UCase$(promptval$)
    code% = Asc(char$)

    ' Digits 0-9
    If code% >= 48 And code% <= 57 Then
        J = code% - 48
        ' Letters A-Z
    ElseIf code% >= 65 And code% <= 90 Then
        ' A=10, B=11, ..., M=22
        J = code% - 55
    Else
        ' default / invalid input
        J = 0
    End If

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
Sub GetPBPString (range%, minIdx%, fileString$)

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
        If Left$(fileString$, 2) = "Q1" Then
            fileString$ = "Q2 " + Right$(fileString$, Len(fileString$) - 3)
        End If
        If Right$(fileString$, 2) = "Q1" Then
            fileString$ = Left$(fileString$, Len(fileString$) - 2) + "Q2"
        End If
    End If

    If Left$(fileString$, 2) = "BB" Then
        fileString$ = player$(0) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "Q1" Then
        fileString$ = player$(1) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "Q2" Then
        fileString$ = player$(2) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "PP" Then
        fileString$ = pitchers$(D, currPitcher(D)) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "UU" Then
        fileString$ = gameUmpire$(1) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "XX" Then
        fileString$ = batters$(D, fielder(D, 5)) + " and " + batters$(D, fielder(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "TT" Then
        fileString$ = batters$(D, fielder(D, 5)) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "FB" Then
        fileString$ = batters$(D, fielder(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "YY" Then
        fileString$ = batters$(D, fielder(D, 4)) + " and " + batters$(D, fielder(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "ZZ" Then
        fileString$ = batters$(D, fielder(D, 4)) + " and " + batters$(D, fielder(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    End If
    If Left$(fileString$, 2) = "Q4" Then
        fileString$ = player$(4) + Right$(fileString$, Len(fileString$) - 2)
    End If

    If Right$(fileString$, 3) = " PP" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + pitchers$(D, currPitcher(D))
    End If
    If Right$(fileString$, 3) = " BB" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(0)
    End If
    If Right$(fileString$, 2) = "Q1" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(1)
    End If
    If Right$(fileString$, 2) = "Q2" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(2)
    End If
    If Right$(fileString$, 2) = "UU" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + gameUmpire$(1)
    End If
    If Right$(fileString$, 2) = "Q4" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(4)
    End If
    If Right$(fileString$, 3) = " TT" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 5))
    End If
    If Right$(fileString$, 2) = "FB" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 3))
    End If
    If Right$(fileString$, 3) = " XX" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 5)) + " and " + batters$(D, fielder(D, 6))
    End If
    If Right$(fileString$, 2) = "YY" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 3)) + " and " + batters$(D, fielder(D, 4))
    End If
    If Right$(fileString$, 3) = " ZZ" Then
        fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, fielder(D, 6)) + " and " + batters$(D, fielder(D, 4))
    End If

End Sub


'----------------------------------------
'             PBP Subroutine
'----------------------------------------
'This is the classic Haffner Play-by-play
'subroutine. Given the target text, it will
'place it in the correct spot in the pbp
'text window. No text is placed if the PBP
'delay is 0.

'This subroutine was not in the original
'game, but can also determine if we are
'printing on GFX or text screen
Sub PBP (pbpString$)

    If useVGA = 1 Then
        'Limit VGA mode to 9 lines b/c
        'we don't have lots of room
        outputLines = 9
    Else
        outputLines = MAX_PBP_LINES
    End If

    M$ = InKey$

    If UCase$(M$) = "O" Then
        Call OPTIONS
    End If

    If UCase$(M$) = Chr$(17) Then
        Cls
        End
    End If

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11, "======== "; pbpString$
    End If

    If pbpDelay! > 0 Then

        'Clean up / restrict to 44 char's
        'this is the width of the PBP window
        pbpString$ = PadRight(pbpString$, 44)
        pbpString$ = Left$(pbpString$, 44)
        'Print #11, pbpString$

        pbpLine = pbpLine + 1

        ' Put PBP text in a slot for line pbpLine
        If pbpLine <= outputLines Then
            pbpText$(pbpLine) = pbpString$
        Else
            For I = 1 To outputLines - 1
                pbpText$(I) = pbpText$(I + 1)
            Next
            pbpText$(outputLines) = pbpString$
        End If

        nbrLines = nbrLines + 1

        ' Display all stored lines
        For I = 1 To outputLines

            If useVGA = 1 Then
                Color &HFFFFFFFF, &HF0000000
                _PrintMode _FillBackground , imageScreen&
                PrintOnGfx 20, I + 3, pbpText$(I), imageScreen&
            Else
                Locate 14 + I, 1
                Print Left$(pbpText$(I), 44)
            End If

        Next

        Call DELAY

    End If

End Sub


'----------------------------------------
'           Add_Strikeouts Subroutine
'----------------------------------------
'This routine adds stats related to when
'at-bats end in a strikeout
Sub Add_Strikeouts (team, opp)

    If fldPos(team, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(team, currPitcher(team), LG_PB_AB)
    Else
        Call INCBATOFF(team, lineupPlayer(team, currLineupSlot(team)), LG_B_AB)
    End If

    Call INCPITDEF(opp, currPitcher(opp), LG_P_OUT)

End Sub


'----------------------------------------
'     CreditRunnerResults Subroutine
'----------------------------------------
'This routine handles crediting all base
'runner movements after a play.
'When clearing bases, we also update I3
'(I do not know that I3 is used)
Sub CreditRunnerResults (S2%)

    For idx = 1 To 3
        'If runner is retreating on grounder and tagged out
        If S2% = EVENT_GROUNDER And baseMove(idx) = 11 And bRunner(idx) = 0 Then
            'batter is also going to be tagged out
            baseMove(0) = 11
        End If
    Next idx

    'Check if 3rd base runner is moving
    If bRunner(3) <> 0 And baseMove(3) > 0 Then

        'Runner scores
        If baseMove(3) >= 1 And baseMove(3) <= 4 Then
            Call CHARGERUN(3)
        End If

        'If runner is retreating, clear base
        If baseMove(3) = 10 Then
            I3 = 3
            Call CLEARBASE(3)
        End If

        If baseMove(3) = 11 Then
            I3 = 4
            Call CLEARBASE(3)
        End If

    End If

    'Check if 2nd base runner is moving
    If bRunner(2) <> 0 And baseMove(2) > 0 Then

        Select Case baseMove(2)

            Case 1
                'If no runner on 3rd
                If bRunner(3) = 0 Then
                    'Runer advances
                    Call CreditAdvance1(2)
                End If

            Case 2 To 4
                'Runner scores
                'If bRunner(3) = 0 Then
                Call CHARGERUN(2)
                'End If

                'If runner is retreating, clear base
            Case 10
                I3 = 2
                Call CLEARBASE(2)

            Case 11
                I3 = 3
                Call CLEARBASE(2)

            Case 12
                If bRunner(3) = 0 Then
                    I3 = 4
                    Call CLEARBASE(2)
                End If

        End Select

    End If

    'Check if 1st base runner is moving
    If bRunner(1) <> 0 And baseMove(1) > 0 Then

        Select Case baseMove(1)

            Case 1
                'If no runner on 2nd
                If bRunner(2) = 0 Then
                    'Runner advances
                    Call CreditAdvance1(1)
                End If

            Case 2
                'If no runner on 2nd + 3rd
                If bRunner(2) = 0 And bRunner(3) = 0 Then
                    'Runner advances
                    Call CreditAdvance2(1)
                End If

            Case 3 To 4
                'Runner scores
                'If bRunner(2) = 0 And bRunner(3) = 0 Then
                Call CHARGERUN(1)
                'End If

            Case 8
                '??? When would this ever happen?
                'If no runner on 2nd
                If bRunner(2) = 0 Then
                    'Runner advances
                    Call CreditAdvance1(1)
                End If

                'If runner is retreating, clear base
            Case 10
                I3 = 1
                Call CLEARBASE(1)

            Case 11
                I3 = 2
                Call CLEARBASE(1)

            Case 12
                If bRunner(3) = 0 And bRunner(2) = 0 Then
                    I3 = 3
                    Call CLEARBASE(1)
                End If

            Case 13
                If bRunner(3) = 0 And bRunner(2) = 0 Then
                    I3 = 4
                    Call CLEARBASE(1)
                End If

        End Select

    End If

    bRunner(0) = currLineupSlot(P)
    b1Pitcher(0) = currPitcher(D)

    'Check if batter is moving
    If baseMove(0) <> 0 Then

        Select Case baseMove(0)

            Case 1 To 3:
                I3 = baseMove(0)
                'Runner advances
                Call CreditAdvance3(I3)

            Case 4
                I3 = baseMove(0)
                'Runner scores
                Call CHARGERUN(0)

                'If runner is retreating, clear base
            Case 11 To 14
                I3 = baseMove(0) - 10
                Call CLEARBASE(0)

        End Select

    End If

End Sub


'----------------------------------------
'           ADV1BASE Subroutine
'----------------------------------------
'Advance player 1 base
Sub ADV1BASE (p_base)

    If Environ$("RUNLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV1BASE before: p_base: "; p_base
        For bm1 = 0 To 3
            Print #11, "runnerOnBase:   "; runnerOnBase(bm1); " -- ";
        Next bm1
    End If

    runnerOnBase(p_base + 1) = runnerOnBase(p_base)
    b2Pitcher(p_base + 1) = b2Pitcher(p_base)
    stealPhase(p_base + 1) = stealPhase(p_base)

    stealPhase(p_base + 1) = 0

    runnerOnBase(p_base) = 0
    b2Pitcher(p_base) = 0
    stealPhase(p_base) = 0

    player$(p_base + 1) = player$(p_base)
    player$(p_base) = ""

    If Environ$("RUNLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV1BASE after: p_base: "; p_base
        For bm1 = 0 To 3
            Print #11, "runnerOnBase:   "; runnerOnBase(bm1); " -- ";
        Next bm1
        Print #11,
    End If

End Sub

'----------------------------------------
'          ADV2BASES Subroutine
'----------------------------------------
'Advance player 2 bases
Sub ADV2BASES (p_base)

    If Environ$("RUNLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV2BASES before: p_base: "; p_base
        For bm1 = 0 To 3
            Print #11, "runnerOnBase:   "; runnerOnBase(bm1); " -- ";
        Next bm1
    End If

    runnerOnBase(p_base + 2) = runnerOnBase(p_base)
    b2Pitcher(p_base + 2) = b2Pitcher(p_base)
    stealPhase(p_base + 2) = stealPhase(p_base)

    stealPhase(p_base + 1) = 0

    runnerOnBase(p_base) = 0
    b2Pitcher(p_base) = 0
    stealPhase(p_base) = 0

    player$(p_base + 2) = player$(p_base)
    player$(p_base) = ""

    If Environ$("RUNLOG") = "TRUE" Then
        Print #11,
        Print #11, "ADV2BASES after: p_base: "; p_base
        For bm1 = 0 To 3
            Print #11, "runnerOnBase:   "; runnerOnBase(bm1); " -- ";
        Next bm1
        Print #11,
    End If

End Sub

'----------------------------------------
'          ADV3BASES Subroutine
'----------------------------------------
'Advance player 3 bases
Sub ADV3BASES (I3)

    runnerOnBase(I3) = currLineupSlot(P)
    b2Pitcher(I3) = currPitcher(D)
    stealPhase(I3) = 0
    runnerOnBase(0) = 0

    player$(I3) = player$(0)

End Sub

'----------------------------------------
'         ADVANCEMENT Subroutine
'----------------------------------------
'Advance runners at each position / base
Sub ADVANCEMENT (S2%, INFPOS%, tgtFielder, currFielder, wasError, homeSafe)

    If Environ$("RUNLOG") = "TRUE" Then

        Print #11,
        Print #11, "--- ADVANCEMENT ---"
        For bm1 = 0 To 3
            Print #11, "baseMove:"; baseMove(bm1); " -- ";
        Next bm1
        Print #11,

    End If

    'Advance 3rd base runner
    If runnerOnBase(3) <> 0 And baseMove(3) <> 0 Then

        i = 3

        'If advancing, score a run
        If baseMove(3) >= 1 And baseMove(3) <= 4 Then
            Call RecordRun(homeSafe, i, PQ, S2%, currFielder, INFPOS%, wasError)
        End If

        'Out at 3rd
        If baseMove(3) = 10 Then
            tgtFielder = 3
            Call PlayerOut(i, 3)
        End If

        'Out at home
        If baseMove(3) = 11 Then
            tgtFielder = 4
            Call PlayerOut(i, 4)
        End If

    End If

    'Advance 2nd base runner
    If runnerOnBase(2) <> 0 And baseMove(2) <> 0 Then

        i = 2

        'Advance
        If baseMove(2) = 1 And runnerOnBase(3) = 0 Then
            Call ADV1BASE(i)
        End If

        If baseMove(2) >= 2 And baseMove(2) <= 4 Then
            'And runnerOnBase(3) = 0
            Call RecordRun(homeSafe, i, PQ, S2%, currFielder, INFPOS%, wasError)
        End If

        'Out at 2nd
        If baseMove(2) = 10 Then
            tgtFielder = 2
            Call PlayerOut(i, 2)
        End If

        'Out at 3rd
        If baseMove(2) = 11 Then
            tgtFielder = 3
            Call PlayerOut(i, 3)
        End If

        'Out at home
        If baseMove(2) = 12 And runnerOnBase(3) = 0 Then
            tgtFielder = 4
            Call PlayerOut(i, 4)
        End If

    End If

    'Advance 1st base runner
    If runnerOnBase(1) <> 0 And baseMove(1) <> 0 Then

        i = 1

        'Advance
        If baseMove(1) = 1 And runnerOnBase(2) = 0 Then
            Call ADV1BASE(i)
        End If

        'Advance 2 bases
        If baseMove(1) = 2 And runnerOnBase(2) = 0 And runnerOnBase(3) = 0 Then
            Call ADV2BASES(i)
        End If

        If baseMove(1) >= 3 And baseMove(1) <= 4 Then
            'And runnerOnBase(2) = 0 And runnerOnBase(3) = 0
            Call RecordRun(homeSafe, i, PQ, S2%, currFielder, INFPOS%, wasError)
        End If

        If baseMove(1) = 8 And runnerOnBase(2) = 0 Then
            Call ADV1BASE(i)
        End If

        'Out at 2nd
        If baseMove(1) = 10 Then
            Call PlayerOut(i, 1)
        End If

        'Out at 2nd
        If baseMove(1) = 11 Then
            Call PlayerOut(i, 2)
        End If

        'Out at 3rd
        If baseMove(1) = 12 And runnerOnBase(3) = 0 And runnerOnBase(2) = 0 Then
            Call PlayerOut(i, 3)
        End If

        'Out at home
        If baseMove(1) = 13 And runnerOnBase(2) = 0 And runnerOnBase(3) = 0 Then
            Call PlayerOut(i, 4)
        End If

    End If

    i = 0
    runnerOnBase(0) = currLineupSlot(P)
    b2Pitcher(0) = currPitcher(D)

    'Advance batter
    If baseMove(0) <> 0 Then

        If baseMove(0) > 0 And baseMove(0) < 4 Then
            tgtFielder = baseMove(0)

            If tgtFielder = 1 And wasError = 1 Then
                roeCount = roeCount + 1
                'Print #11, "ROE #" + Str$(roeCount) + ": Type=ExtraBase Inn=" + Str$(INNING%) + " Event=" + Str$(S2%) + " Fielder=" + Str$(currFielder)
            End If

            Call ADV3BASES(tgtFielder)
        End If

        If baseMove(0) = 4 Then
            tgtFielder = baseMove(0)
            Call RecordRun(homeSafe, i, PQ, S2%, currFielder, INFPOS%, wasError)
        End If

        If baseMove(0) = 11 Then
            tgtFielder = 1
            Call PlayerOut(i, 1)
        End If

        If baseMove(0) = 12 Then
            tgtFielder = 2
            Call PlayerOut(i, 2)
        End If

        If baseMove(0) = 13 Then
            tgtFielder = 3
            Call PlayerOut(i, 3)
        End If

        If baseMove(0) = 14 Then
            tgtFielder = 4
            Call PlayerOut(i, 4)
        End If

    End If

    If wasError = 0 And nbrOuts(1) < 3 Then
        Call CreditRunnerResults(S2%)
    End If

    Erase baseMove

End Sub


'----------------------------------------
'     FlyBall_Base2Runner Subroutine
'----------------------------------------
'Handle decision making and chances for a
'2nd base runner to advance on a fly ball
Sub FlyBall_Base2Runner (wasError, S2%, offChoice, runnerOn2nd, currFielder, INFPOS%, stealing, tgtBase, I9, baseStatus)

    If runnerOnBase(2) <> 0 Then

        speedRating = batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run)
        fldArm = (batRating(D, fielder(D, currFielder), SS_B_Arm) - 5) * 25

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If
        
        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -100
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 100

        End Select

        offset = ((speedRating - 5) * 25 - fldArm) + runStratBonus

        RN0 = Int(Rnd(1) * 1000) + 1

        'Chances to advance base
        deepFly_lfChance = 250
        deepFly_cfChance = 700
        deepFly_rfChance = 800

        regFly_cfChance = 500
        regFly_rfChance = 550

        If offChoice <> OFF_HIT_N_RUN Then
            If (currFielder = 7 And S2% = EVENT_FLY_DEEP And RN0 <= deepFly_lfChance + offset) Or _
               (currFielder = 8 And ((S2% = EVENT_FLY_DEEP And RN0 <= deepFly_cfChance + offset) Or (S2% = EVENT_FLY_REGULAR And RN0 <= regFly_cfChance + offset))) Or _
               (currFielder = 9 And ((S2% = EVENT_FLY_DEEP And RN0 <= deepFly_rfChance + offset) Or (S2% = EVENT_FLY_REGULAR And RN0 <= regFly_rfChance + offset))) Then
                baseMove(2) = 1
            End If
        Else
            ' Hit and run conditions
            If speedRating > 6 And (currFielder = 9 Or currFielder = 8) And _
                S2% = EVENT_FLY_DEEP And (RN0 <= 400 + offset) Then
                baseMove(2) = 1
            End If
        End If

        'Chances to try and score
        '(NEWLY ADDED)
        If baseMove(2) <> 1 And S2% = EVENT_FLY_REGULAR Then
            ' Runner on 2nd might try to score on deep regular flies
            If (currFielder = 7 And RN0 <= 200 + offset) Or _                                                                                   
                (currFielder = 8 And RN0 <= 100 + offset) Or _
                (currFielder = 9 And RN0 <= 150 + offset) Then
                baseMove(2) = 2 ' Try to score
            End If
        End If

        ' -- update these based on strategy?
        'Chances to try for 3rd on regular fly to LF/CF/RF
        If baseMove(2) <> 1 And S2% = EVENT_FLY_REGULAR And offChoice <> OFF_HIT_N_RUN And baseStatus < 6 Then

            If (currFielder = 7) Or (currFielder = 8 And RN0 > 840) Or (currFielder = 9 And RN0 > 800) Then
                tgtBase = 3
                If wasError = 0 Then
                    Call TRYFOREXTRABASE(S2%, offChoice, stealing, tgtBase, I9, baseStatus, runnerOn2nd, currFielder, INFPOS%, wasError, homeSafe)
                End If
            End If

        End If

    End If

End Sub


'----------------------------------------
'     FlyBall_Base1Runner Subroutine
'----------------------------------------
'Handle decision making and chances for a
'1st base runner to advance on a fly ball
Sub FlyBall_Base1Runner (wasError, S2%, offChoice, runnerOn2nd, currFielder, INFPOS%, stealing, tgtBase, I9, baseStatus)

    If runnerOnBase(1) <> 0 Then

        RN0 = Int(Rnd(1) * 1000) + 1
        baseMove(1) = 0

        batRunRate = (batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run) - 5) * 25
        pitArmRate = (batRating(D, fielder(D, currFielder), SS_B_Arm) - 5) * 25
        tryFor2ndChance = batRunRate - pitArmRate

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If
        
        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -100
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 100

        End Select

        If S2% = EVENT_FLY_DEEP And offChoice <> OFF_HIT_N_RUN And baseStatus = 1 And _
            RN0 <= 100 + tryFor2ndChance + runStratBonus Then

            tgtBase = 2

            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "-- might try for 2nd on deep fly..."
            End If

            If wasError = 0 Then
                Call TRYFOREXTRABASE(S2%, offChoice, stealing, tgtBase, I9, baseStatus, runnerOn2nd, currFielder, INFPOS%, wasError, homeSafe)
            End If

        End If

    End If

End Sub


'----------------------------------------
'    DisplayBatter_Lineup Subroutine
'----------------------------------------
'Output batter lineup to screen
Sub DisplayBatter_Lineup (page)

    Locate 1, 1

    Color 14: Print "#";
    Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12): Print gameTeams$(P9);

    Color 14, 0

    Locate , 16: Print "B   POS    G  AB 2B 3B HR RBI  BB  SO SRAB  SB CS G%  BA"

    For luIdx = 2 To 13:
        Locate luIdx, 1: Print String$(78, 32)
    Next

    Locate 2, 1

    If page = 0 Then
        AA = 0
        BB = 12
    End If
    If page = 1 Then
        AA = 13
        BB = 22
    End If

    For idx = AA To BB:

        Locate , 1

        If batRating(P9, idx, SS_B_Avail) = 0 Then

            Color 2, 0

            If idx < 10 Then Print Using "#"; idx;
            If idx > 9 Then Print alpha$(idx - 10);

            Color 15, 0

            Print Tab(3); batters$(P9, idx); Tab(17); handed$(batRating(P9, idx, SS_B_Bats) + 2);

            For xx = 20 To 23
                Print Tab(xx);
                If batRating(P9, idx, xx + 2) <> -1 Then
                    Print Using "#"; batRating(P9, idx, xx + 2);
                End If
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
'      RemoveBatterAtBat Subroutine
'----------------------------------------
'This subroutine undoes the at-bat for a
'batter in the event of a sacrifice fly.
Sub RemoveBatterAtBat (team, player, category)

    opp = 1 - team

    oppBatStats(team, player, category) = oppBatStats(team, player, category) - 1
    teamBatStats(team, player, category) = teamBatStats(team, player, category) - 1

    If category <= 8 And category <> 2 And category <> 3 And pitchRating(opp, currPitcher(opp), SS_P_Throws) <> 1 Then

        If category >= 4 Then
            CSS(team, player, category - 2) = CSS(team, player, category - 2) - 1
        Else
            CSS(team, player, category) = CSS(team, player, category) - 1
        End If

    End If

End Sub


'----------------------------------------
'        CreditAdvance1 Subroutine
'----------------------------------------
'This subroutine credits runners for the
'advancement of 1 base
Sub CreditAdvance1 (i)

    bRunner(i + 1) = bRunner(i)
    b1Pitcher(i + 1) = b1Pitcher(i)

    bRunner(i) = 0
    b1Pitcher(i) = 0

End Sub


'----------------------------------------
'        CreditAdvance2 Subroutine
'----------------------------------------
'This subroutine credits runners for the
'advancement of 2 bases
Sub CreditAdvance2 (i)

    bRunner(i + 2) = bRunner(i)
    b1Pitcher(i + 2) = b1Pitcher(i)

    bRunner(i) = 0
    b1Pitcher(i) = 0

End Sub


'----------------------------------------
'        CreditAdvance3 Subroutine
'----------------------------------------
'This subroutine credits runners for the
'advancement of 3 bases
Sub CreditAdvance3 (I3)

    bRunner(I3) = currLineupSlot(P)
    b1Pitcher(I3) = currPitcher(D)

End Sub


'----------------------------------------
'          CAUGHTFLY Subroutine
'----------------------------------------
'This subroutine handles the Play-by-play
'for a caught fly ball
Sub CAUGHTFLY (i, S2%)

    Call Rolld100(resultRoll)

    Select Case resultRoll

        Case 1 To 96
            i = 1
            If S2% <> EVENT_FLY_DEEP Then
                Call GetPBPString(60, 1230, fileString$)
                Call PBP(player$(4) + " " + fileString$)
            Else
                If teamRat_GAME(1, 13) = 37 Then Call PBP("he climbs the terrace")
                Call GetPBPString(20, 1290, fileString$)
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
Sub CHANGELINEUP ()

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
        Else
            Print batters$(P9, lineupPlayer(P9, luIdx));
        End If

        If fldPos(P9, luIdx) <> 0 And fldPos(P9, luIdx) < 10 Then
            Locate , 20: Print batRating(P9, lineupPlayer(P9, luIdx), SS_B_FldPct);
        End If

        Locate , 17
        Print fieldPos$(fldPos(P9, luIdx));
        Locate , 25

        For I1 = 1 To 3
            If runnerOnBase(I1) = luIdx And P9 = P Then
                Color 3: Print I1;
                Color 15
            End If
        Next I1

        Print " "

    Next luIdx

End Sub

'----------------------------------------
'          CHARGERUN Subroutine
'----------------------------------------
'This subroutine charges a pitcher with a
'run and clears the runner's base status
Sub CHARGERUN (tgtBase)

    If nbrOuts(1) >= 3 Then
        Call CLEARBASE(tgtBase)
    Else
        Call INCPITDEF(D, b1Pitcher(tgtBase), LG_P_ER)
        Call CLEARBASE(tgtBase)
    End If

End Sub


'----------------------------------------
'          CHECKSAVE Subroutine
'----------------------------------------
'This subroutine checks if the current
'reliever qualifies for a save
Sub CHECKSAVE (baseStatus)

    scoreDiff = gameScore(D, 0) - gameScore(P, 0)
    relieverOnRecordForSave(D) = -1

    If scoreDiff >= 1 Then

        If (baseStatus = 0 And scoreDiff <= 2) Or _
            (baseStatus > 0 And baseStatus < 4 And scoreDiff <= 3) Or _ 
            (baseStatus > 3 And baseStatus < 7 And scoreDiff <= 4) Or _ 
            (baseStatus = 7 And scoreDiff <= 5) Or _
            (baseStatus = 0 And nbrOuts(0) = 0 And scoreDiff <= 3) Or _ 
            (INNING% > 5 And INNING% < 8 And scoreDiff > 0)  Then
            relieverOnRecordForSave(D) = currPitcher(D)
        End If

    End If

    RI = 0

    If baseStatus <> 0 Then

        Select Case baseStatus

            Case 1 To 3
                RI = 1

            Case 4 To 6
                RI = 2

            Case 7
                RI = 3

        End Select

        oppPitStats(D, currPitcher(D), LG_P_RInh) = oppPitStats(D, currPitcher(D), LG_P_RInh) + RI
        teamPitStats(D, currPitcher(D), LG_P_RInh) = teamPitStats(D, currPitcher(D), LG_P_RInh) + RI

    End If

End Sub


'----------------------------------------
'          CLEARBASE Subroutine
'----------------------------------------
'This subroutine clears the runner from
'being present at the previous base
Sub CLEARBASE (tgtBase)

    bRunner(tgtBase) = 0
    b1Pitcher(tgtBase) = 0

End Sub


'----------------------------------------
'        CreditAssist Subroutine
'----------------------------------------
'This subroutine credits the fielder with
'an assist
Sub CreditAssist (currFielder)

    If currFielder = 1 Then
        Call INCPITDEF(D, currPitcher(D), LG_PB_A)
    Else
        Call INCBATDEF(D, fielder(D, currFielder), LG_B_A)
        If Environ$("PBPLOG") = "TRUE" Then
            Print #11, "Assist for: "; currFielder
        End If
    End If

End Sub


'----------------------------------------
'       CreditInfieldPutout Subroutine
'----------------------------------------
'This subroutine credits the fielder with
'an infield putout
Sub CreditInfieldPutout (currBase, FB2P%, AST%)

    If currBase > 4 Then currBase = 1

    po% = currBase + 2

    If po% = 6 Then

        po% = 2
        AST% = 0
        Call INCBATDEF(D, fielder(D, po%), LG_B_PO)
        If Environ$("PBPLOG") = "TRUE" Then
            Print #11, "Infield? Putout1 for: "; po%
        End If

    Else

        If po% = 3 And FB2P% = 1 Then

            FB2P% = 0
            po% = 1
            If po% = 1 Then
                Call INCPITDEF(D, currPitcher(D), LG_PB_PO)
            End If

        Else

            If isPutout% = 4 Then
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
            If Environ$("PBPLOG") = "TRUE" Then
                Print #11, "WARNING!!  skipping call to INCBATDEF in CreditInfieldPutout because po% = " + Str$(po%)
            End If
            Exit Sub
        End If

        Call INCBATDEF(D, fld%, LG_B_PO)
        If Environ$("PBPLOG") = "TRUE" Then
            Print #11, "Infield? Putout2 for: "; po%
        End If

    End If

End Sub

'----------------------------------------
'         CreditRegularPutout Subroutine
'----------------------------------------
'Credit player with regular putout
Sub CreditRegularPutout (currFielder)

    If Environ$("PBPLOG") = "TRUE" Then
        Print #11, "Reg Putout for: "; currFielder
    End If

    If currFielder = 1 Then
        Call INCPITDEF(D, currPitcher(D), LG_PB_PO)
    Else
        Call INCBATDEF(D, fielder(D, currFielder), LG_B_PO)
    End If

End Sub


'----------------------------------------
'          DELIVERY Subroutine
'----------------------------------------
'This subroutine generates the PBP for
'a pitch delivery.
Sub DELIVERY ()

    If d100% <= 20 Then

        pbpIdx% = Int(Rnd(1) * 20) + 50
        Call GetPBPString(20, 50, fileString$)
        Call PBP(fileString$)

    End If

    Call GetPBPString(46, 1, fileString$)
    Call PBP(fileString$)

End Sub

'----------------------------------------
'   DisplayFielding_Lineup Subroutine
'----------------------------------------
'This subroutine shows the fielding line-
'up for the current team.
Sub DisplayFielding_Lineup (page, TG)

    Locate 1, 16
    Color 14: Print "P1 FAVG A R   P2 FAVG A R   P3 FAVG A R   P4 FAVG A R      "

    For luIdx = 2 To 13:
        Locate luIdx, 1: Print String$(78, 32)
    Next

    Locate 2

    If page = 0 Then
        AA = 0
        BB = 12
    End If

    If page = 1 Then
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

            Locate , 17
            If batRating(P9, luIdx, SS_B_Pos1) <> -1 Then
                Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos1); batRating(P9, luIdx, SS_B_FldPct1); batRating(P9, luIdx, SS_B_Arm1); batRating(P9, luIdx, SS_B_Range1);
            End If

            Locate , 31
            If batRating(P9, luIdx, SS_B_Pos2) <> -1 Then
                Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos2); batRating(P9, luIdx, SS_B_FldPct2); batRating(P9, luIdx, SS_B_Arm2); batRating(P9, luIdx, SS_B_Range2);
            End If

            Locate , 45
            If batRating(P9, luIdx, SS_B_Pos3) <> -1 Then
                Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos3); batRating(P9, luIdx, SS_B_FldPct3); batRating(P9, luIdx, SS_B_Arm3); batRating(P9, luIdx, SS_B_Range3);
            End If

            Locate , 59
            If batRating(P9, luIdx, SS_B_Pos4) <> -1 Then
                Print Using "# #### # #"; batRating(P9, luIdx, SS_B_Pos4); batRating(P9, luIdx, SS_B_FldPct4); batRating(P9, luIdx, SS_B_Arm4); batRating(P9, luIdx, SS_B_Range4);
            End If

            Print

        End If

    Next luIdx

    TG = TG + 1

End Sub

'----------------------------------------
'         DisplayRun Subroutine
'----------------------------------------
'This subroutine displays the player
'that is currently on the target base
Sub DisplayRun (baseNbr, X)

    J = runnerOnBase(baseNbr)

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

    Call Rolld100(resultRoll)

    Select Case resultRoll

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
Sub DPPIVOT (currFielder)

    ' *** double play pivot ***
    If currFielder = 4 Then
        ttw = 6
    End If

    If currFielder = 6 Then
        ttw = 4
    End If

    If currFielder = 3 Then
        ttw = 1
    End If

    Call GetPBPString(20, 1910, fileString$)
    Call PBP(fileString$ + " " + batters$(D, fielder(D, ttw)))

End Sub


'----------------------------------------
'           DPRELAY Subroutine
'----------------------------------------
'Double-play relay ?!?!
Sub DPRELAY ()

    If baseMove(0) = 11 Then

        If d100% <= 10 Then
            Call PBP("Give me the Hoover!")
        Else

            Call Rolld100(resultRoll)

            Select Case resultRoll

                Case 1 To 8:
                    pbpString$ = "over to 1st..."

                Case 9 To 16:
                    pbpString$ = "throw to 1st..."

                Case 17 To 24:
                    pbpString$ = "on to first..."

                Case 25 To 30:
                    pbpString$ = player$(4) + " pivots and throws"

                Case 31 To 36:
                    pbpString$ = player$(4) + " gets the throw off"

                Case 37 To 44:
                    pbpString$ = player$(4) + " is hit as he throws"

                Case 45 To 50:
                    pbpString$ = player$(4) + " leaps and throws"

                Case 51 To 56:
                    pbpString$ = player$(4) + " jumps over the sliding " + player$(1)

                Case 57 To 64:
                    pbpString$ = "the relay to first..."

                Case 65 To 72:
                    pbpString$ = "quick turn by " + player$(4)

                Case 73 To 80:
                    pbpString$ = "relay to 1st..."

                Case 81 To 88:
                    pbpString$ = "to 1st..."

                Case 89 To 100:
                    If baseMove(0) = 11 Then pbpString$ = "over to 1st..."

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
'Double-play safe
Sub DPSAFE ()

    Call Rolld100(resultRoll)

    pbpString$ = ""

    Select Case resultRoll

        Case 1 To 8:
            pbpString$ = "over to 1st..."

        Case 9 To 16:
            pbpString$ = "throw to 1st..."

        Case 17 To 24:
            pbpString$ = "on to first..."

        Case 25 To 30:
            pbpString$ = "pivot to first"

        Case 31 To 36:
            pbpString$ = "the throw to first"

        Case 37 To 44:
            pbpString$ = "relay to first"

        Case 45 To 50:
            pbpString$ = "quick turn to 1st"

        Case 51 To 56:
            pbpString$ = "on to first..."

        Case 57 To 64:
            pbpString$ = "the relay to first..."

        Case 65 To 72:
            pbpString$ = "quick turn"

        Case 73 To 80:
            pbpString$ = "relay to 1st..."

        Case 81 To 88:
            pbpString$ = "to 1st..."

        Case 89 To 100:
            If baseMove(0) = 11 Then pbpString$ = "over to 1st..."

    End Select

    Call PBP(pbpString$)

    Call GetPBPString(20, 1890, fileString$)
    Call PBP(fileString$)

End Sub


'----------------------------------------
'          EJECTINC Subroutine
'----------------------------------------
'This subroutine increases the ejection
'count for the target player/team.
Sub AddEjection (X$)

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
Sub EJECTIONINJURY (posIdx, W5, W6)

    If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then

        I2 = 1

        loopCounter = 0

        Do While I2 <= 9 And fldPos(D, I2) <> posIdx

            I2 = I2 + 1
            loopCounter = loopCounter + 1

        Loop

        For idx = 0 To 22

            If batRating(D, idx, SS_B_Avail) <= 0 And batters$(D, idx) <> "XXX" And _ 
                (fldPos(D, I2) = batRating(D, idx, SS_B_Pos1) Or fldPos(D, I2) = batRating(D, idx, SS_B_Pos2) Or fldPos(D, I2) = batRating(D, idx, SS_B_Pos3) Or fldPos(D, I2) = batRating(D, idx, SS_B_Pos4)) Then

                pbpString$ = batters$(D, fielder(D, posIdx)) + " will be replaced by " + batters$(D, idx)
                Call PBP(pbpString$)

                'Make player available and insert into lineup
                batRating(D, idx, SS_B_Avail) = 1
                lineupPlayer(D, I2) = idx

                'Track game roster with substitution
                playersUsed(D) = playersUsed(D) + 1
                gameRoster(D, 0, playersUsed(D)) = idx
                gameRoster(D, 1, playersUsed(D)) = fldPos(D, I2)
                gameRoster(D, 2, playersUsed(D)) = (I2 * 10) + (playersUsed(D) - 9)

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

                Exit For

            End If

        Next idx

    Else

        pbpString$ = "You must replace " + batters$(D, fielder(D, posIdx))
        Call PBP(pbpString$)

    End If

End Sub


'----------------------------------------
'           FLDAVG Subroutine
'----------------------------------------
'Fielding Average
Sub FLDAVG (currFielder, I3)

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

        Select Case currFielder

            Case 4, Is >= 7
                I3 = 982

            Case 2
                I3 = 987

            Case 3
                I3 = 980

            Case 5 To 6
                I3 = 964

        End Select

        If currFielder >= 7 Then
            I3 = I3 - 10
        Else
            I3 = I3 - 20
        End If

        If currFielder = 2 Or currFielder >= 4 And currFielder <= 6 Then
            I3 = I3 - 80
            Exit Sub
        End If

    End If

    If currFielder = 4 Or currFielder = 6 Then
        I3 = (batRating(D, fielder(D, currFielder), SS_B_FldPct)) * .995
    End If

    If currFielder = 5 Then
        I3 = (batRating(D, fielder(D, currFielder), SS_B_FldPct)) * .985
    End If

End Sub


'----------------------------------------
'      DisplayBallField Subroutine
'----------------------------------------
'This subroutine draws the field for
'the traditional Haffner text display.
'This also includes printing various
'player stats and other information.
Sub DisplayBallField (nbrBalls, nbrStrikes)

    Color , 0
    Cls

    'Green field
    For X = 5 To LAST_ROW
        Locate X, 46
        Color , 2
        '34 characters = 80 char width - 44 char PBP - 2 char (lines)
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

            If (team = 0 And P = 1 And I2 > INNING%) Or _
                (team = 1 And P = 0 And I2 > INNING% - 1) Or _
                (((inningScore(team, INNING%) = 0 And I2 > INNING% - 1) Or (inningScore(team, INNING%) > 0 And I2 > INNING%)) And ((team = 0 And P = 0) Or (team = 1 And P = 1))) Then
                Exit For
            End If

            Call DisplayInningScore(I2, team)

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

    For X = 5 To LAST_ROW:
        Locate X, 45: Print Chr$(179);
    Next

    'CF Name
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 7, 58: Print Left$(batters$(D, fielder(D, 8)), 10)

    'CF Stats
    Color 15, 2
    Locate 8, 59: Print Using "A:# R:#"; batRating(D, fielder(D, 8), SS_B_Arm); batRating(D, fielder(D, 8), SS_B_Range)
    If batRating(D, fielder(D, 8), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 8), SS_B_FldPct)), "###", 1)
    End If
    Locate 9, 59: Print "FA:" + fldPct$

    'RF / LF Names
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 8, 46: Print Left$(batters$(D, fielder(D, 7)), 10);
    Locate , 70: Print Left$(batters$(D, fielder(D, 9)), 10)

    'RF / LF Stats
    Color 15, 2
    Locate 9, 47: Print Using "A:# R:#"; batRating(D, fielder(D, 7), SS_B_Arm); batRating(D, fielder(D, 7), SS_B_Range);
    Locate , 71: Print Using "A:# R:#"; batRating(D, fielder(D, 9), SS_B_Arm); batRating(D, fielder(D, 9), SS_B_Range)

    If batRating(D, fielder(D, 7), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 7), SS_B_FldPct)), "###", 1)
    End If
    Locate 10, 47: Print "FA:" + fldPct$

    If batRating(D, fielder(D, 9), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 9), SS_B_FldPct)), "###", 1)
    End If
    Locate 10, 71: Print "FA:" + fldPct$

    'SS / 2B Names
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 12, 49: Print Left$(batters$(D, fielder(D, 6)), 10);
    Locate , 68: Print Left$(batters$(D, fielder(D, 4)), 10)

    'SS / 2B Stats
    Color 15, 2
    Locate 13, 50: Print Using "R:#"; batRating(D, fielder(D, 6), SS_B_Range);
    Locate 13, 69: Print Using "R:#"; batRating(D, fielder(D, 4), SS_B_Range)

    If batRating(D, fielder(D, 6), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 6), SS_B_FldPct)), "###", 1)
    End If
    Locate 14, 50: Print "FA:" + fldPct$

    If batRating(D, fielder(D, 4), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 4), SS_B_FldPct)), "###", 1)
    End If
    Locate 14, 69: Print "FA:" + fldPct$

    '2nd Base "Graphic"
    Locate 14, 62: Print Chr$(219);

    '1B / 3B Names
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Locate 16, 46: Print Left$(batters$(D, fielder(D, 5)), 10);
    Locate , 70: Print Left$(batters$(D, fielder(D, 3)), 10)

    '1B / 3B Stats
    Color 15, 2
    Locate 17, 47: Print Using "R:#"; batRating(D, fielder(D, 5), SS_B_Range);
    Locate 17, 71: Print Using "R:#"; batRating(D, fielder(D, 3), SS_B_Range)

    If batRating(D, fielder(D, 5), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 5), SS_B_FldPct)), "###", 1)
    End If
    Locate 18, 47: Print "FA:" + fldPct$

    If batRating(D, fielder(D, 3), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 3), SS_B_FldPct)), "###", 1)
    End If
    Locate 18, 71: Print "FA:" + fldPct$

    '1st / 3rd Base "Graphic"
    Locate 19, 49: Print Chr$(219);
    Locate 19, 75: Print Chr$(219);

    'Pitcher Name
    Locate 19, 55: Print "["; handed$(pitchRating(D, currPitcher(D), SS_P_Throws) + 2); "] "; Left$(pitchers$(D, currPitcher(D)), 10)

    'Pitcher Stats
    Color 15, 2
    Locate 20, 59: Print Using "H:#"; pitchRating(D, currPitcher(D), SS_P_HoldRunner)
    If pitchRating(D, currPitcher(D), SS_P_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(pitchRating(D, currPitcher(D), SS_P_FldPct)), "###", 1)
    End If
    Locate 21, 59: Print "FA:" + fldPct$

    'Home Plate "Graphic
    Locate 23, 62: Print Chr$(219)

    'Catcher Name
    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12):
    Locate 25, 58: Print Left$(batters$(D, fielder(D, 2)), 10)

    'Catcher Stats
    Color 15, 2
    Locate 26, 59: Print Using "   A:#"; batRating(D, fielder(D, 2), SS_B_Arm);

    If batRating(D, fielder(D, 2), SS_B_FldPct) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 2), SS_B_FldPct)), "###", 1)
    End If
    Locate 27, 59: Print "FA:" + fldPct$;

    'Blank out all lineup text
    Color 7, 0
    For X = 5 To LAST_ROW:
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
Sub FLDERR (S2%, fldPct, stealing, currFielder, INFPOS%, wasError, homeSafe)

    wasError = 0
    errType = 0

    If currFielder = 1 Then
        If stealing = 1 Then
            fldPct = 995
        Else
            fldPct = pitchRating(D, currPitcher(D), SS_P_FldPct) + 30
        End If
    Else
        Call FLDAVG(currFielder, fldPct)
    End If

    If fldPct >= 999 Then

        errChance = 1

    Else

        If S2% >= EVENT_INF_SINGLE And S2% <= EVENT_DP Then
            errChance = 1009 - fldPct
        End If

        If S2% >= EVENT_1B And S2% <= EVENT_3B Then
            errChance = 1006 - fldPct
        End If

        If S2% = EVENT_STEAL Or S2% >= EVENT_RUNDOWN Then
            errChance = 1000 - fldPct
        End If

        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_RUNNER_DBLD Then
            errChance = (1000 - fldPct) * .05
        End If

    End If

    If currFielder = 7 Then
        errChance = errChance - 4
    End If

    If currFielder = 8 Then
        errChance = errChance - 10
    End If

    If currFielder = 9 Then
        errChance = errChance - 15
    End If

    If currFielder = 5 Then
        errChance = errChance - 15
    End If

    'Check for error
    chanceRoll = Int(Rnd(1) * 1000) + 1

    If chanceRoll <= errChance Then

        'Error in single

        wasError = 1
        rand = Int(Rnd(1) * 2) + 1
        errType = 2

        If Environ$("PBPLOG") = "TRUE" Then
            Print #11, "ERROR on: "; currFielder
        End If

        If currFielder = 1 Then
            Call INCPITDEF(D, currPitcher(D), LG_PB_E)
        Else
            Call INCBATDEF(D, fielder(D, currFielder), LG_B_E)
        End If

        If S2% = EVENT_INF_SINGLE Then

            Call PBP(player$(0) + " beats out an infield single")

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
            End If

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

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

        Call Rolld100(RN0)

        If errType = 1 And currFielder = 1 And S2% = EVENT_STEAL Then
            pbpString$ = "*** WILD PICK OFF THROW BY" + " " + player$(4) + " ***"
        End If

        Select Case errType

            Case 1 ' Throwing errors
                'Print #11, "Throwing error"
                Select Case RN0

                    Case 1 To 50
                        pbpString$ = "*** THROWING ERROR ON" + " " + player$(4) + " ***"

                    Case 51 To 55
                        pbpString$ = "*** BALL KICKED BY" + " " + player$(4) + " ***"

                    Case 56 To 100
                        pbpString$ = "*** " + player$(4) + " THROWS IT AWAY ***"

                End Select

            Case 2 ' Fielding errors
                'Print #11, "Fielding error"
                Select Case RN0

                    Case 1 To 30
                        pbpString$ = "*** FIELDING ERROR ON" + " " + player$(4) + " ***"

                    Case 31 To 50
                        pbpString$ = "*** OFF THE GLOVE OF" + " " + player$(4) + " ***"

                    Case 51 To 75
                        pbpString$ = "*** BALL IS BOOTED BY" + " " + player$(4) + " ***"

                    Case 76 To 95
                        pbpString$ = "*** BALL IS BOBBLED BY" + " " + player$(4) + " ***"

                    Case 96 To 100
                        pbpString$ = "*** BALL IS THROUGH THE LEGS OF" + " " + player$(4) + " ***"

                End Select

            Case 3 ' Catching errors
                'Print #11, "Catching error"
                Select Case RN0

                    Case 1 To 33
                        pbpString$ = "*** BALL DROPPED BY" + " " + player$(4) + " ***"

                    Case 34 To 66
                        pbpString$ = "*** OFF THE GLOVE OF" + " " + player$(4) + " ***"

                    Case 67 To 100
                        pbpString$ = "*** BALL IS MISPLAYED BY" + " " + player$(4) + " ***"

                End Select

        End Select

        Call PBP(pbpString$)

        '-- BASE RUN strategy
        'Should this be impacted???
        If S2% = EVENT_STEAL Or S2% = EVENT_RUNDOWN Then

            I8! = Int(Rnd(1) * 10) + 1

            If I8! < 5 Then
                baseMove(0) = 2
                baseMove(1) = 2
                baseMove(2) = 2
                baseMove(3) = 1
                Call DELAY
            End If

        Else

            baseMove(0) = 1
            baseMove(1) = 1
            baseMove(2) = 1
            baseMove(3) = 1

            If S2% <= EVENT_FLY_DEEP And S2% > EVENT_INF_POPUP And _ 
                ((d100% <= 17 And S2% <> EVENT_2B) Or S2% = EVENT_INF_SINGLE) Then
                baseMove(0) = 2
                baseMove(1) = 2
                baseMove(2) = 2
            End If

        End If

        If (S2% >= EVENT_FLY_DEEP And S2% <= EVENT_INF_POPUP And nbrOuts(0) = 2) Or _ 
            S2% >= EVENT_BUNT Or (S2% = EVENT_STEAL And currFielder = 2) Then
            baseMove(0) = 2
            baseMove(1) = 2
            baseMove(2) = 2
            baseMove(3) = 1
        End If

        If S2% >= EVENT_FLY_DEEP And S2% <= EVENT_FLY_SHALLOW And RN0 <= 33 And nbrOuts(0) < 2 And runnerOnBase(1) = 0 Then
            baseMove(0) = 2
        End If

        If S2% = EVENT_STEAL Or S2% = EVENT_RUNDOWN Then
            baseMove(0) = 0
        End If

        If errType > 0 And S2% <> EVENT_1B And S2% <> EVENT_2B And S2% <> EVENT_3B And S2% <> EVENT_STEAL And S2% <> EVENT_RUNDOWN Then
            'nbrOuts(1) tracks how many outs there would be if no errors (for recording unearned runs)
            nbrOuts(1) = nbrOuts(1) + 1
        End If

        If S2% = EVENT_STEAL Or S2% = EVENT_1B Or S2% = EVENT_2B Or S2% = EVENT_3B Or S2% = EVENT_RUNDOWN Then

            baseMove(0) = 0

            If I8! > 4 And currFielder <> 2 Then
                baseMove(1) = 1
                baseMove(2) = 1
                baseMove(3) = 1
            End If

        End If

        gameScore(D, 2) = gameScore(D, 2) + 1

        Call ADVANCEMENT(S2%, INFPOS%, currFielder, fldPct, wasError, homeSafe)

    End If

End Sub


'----------------------------------------
'         GETFIELDER Subroutine
'----------------------------------------
'This subroutine returns the index? of the
'target fielder.
Sub GETFIELDER (currFielder, I1)

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
Sub GETFIELDNAME (currFielder)

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
Sub GETINFIELDER (currFielder, I1, S2%, P2)

    Call GETFIELDER(currFielder, I1)

    Call Rolld100(RN0)

    If I1 = 1 Then

        Select Case RN0

            Case 1 To 32
                currFielder = 5

            Case 33 To 72
                currFielder = 6

            Case 73 To 92
                currFielder = 4

            Case 93 To 100
                currFielder = 3

        End Select

    End If

    If I1 = -1 Then

        Select Case RN0

            Case 1 To 28
                currFielder = 3

            Case 29 To 68
                currFielder = 4

            Case 69 To 88
                currFielder = 6

            Case 89 To 100
                currFielder = 5

        End Select

    End If

    If (S2% <> EVENT_INF_POPUP And RN0 <= 12) Or _  
       (S2% = EVENT_RUNNER_DBLD And currFielder = 2) Or _ 
       (P2 = OFF_BUNT And currFielder = 6) Then 
        currFielder = 1
    End If

    If  (S2% = EVENT_INF_POPUP And (RN0 <= 4 Or RN0 <= 12)) Or _ 
        (P2 = OFF_BUNT And currFielder = 4 Or S2% = EVENT_INF_POPUP And currFielder = 1) Then 
        currFielder = 2
    End If

    Call GETFIELDNAME(currFielder)

End Sub

'----------------------------------------
'        GETOUTFIELDER Subroutine
'----------------------------------------
'Get outfielder
Sub GETOUTFIELDER (currFielder, I1)

    Call GETFIELDER(currFielder, I1)

    Call Rolld100(RN0)

    Select Case RN0

        Case Is <= 30
            currFielder = 8

        Case 31 To 50

            If I1 = 1 Then
                currFielder = 9
            End If

            If I1 = -1 Then
                currFielder = 7
            End If

        Case 51 To 100

            If I1 = 1 Then
                currFielder = 7
            End If

            If I1 = -1 Then
                currFielder = 9
            End If

    End Select

    player$(4) = batters$(D, fielder(D, currFielder))

End Sub


'----------------------------------------
'         GROUNDERPBP Subroutine
'----------------------------------------
'This subroutine manages variables and
'Play-by-Play display for grounders to
'the plate.
Sub GROUNDERPBP (currFielder)

    If currFielder <> 2 Then

        range = 140
        min = 1930

        If d100% > 35 Then

            Select Case currFielder

                Case 1
                    range = 160: min = 2070

                Case 3
                    range = 150: min = 2230

                Case 4
                    range = 160: min = 2380

                Case 5
                    range = 150: min = 2700

                Case 6
                    range = 160: min = 2540

            End Select

        End If

        Call GetPBPString(range, min, fileString$)
        pbpString$ = fileString$ + " "

    Else

        ' *** in front of plate ***
        Call GetPBPString(25, 2870, fileString$)
        Call PBP(fileString$)

    End If

End Sub

'----------------------------------------
'          GROUNDERS Subroutine
'----------------------------------------
'This subroutine manages variables and
'Play-by-Play display for grounders to
'infielders.
Sub GROUNDERS (currFielder)

    X$ = player$(4)

    If currFielder = 2 Then

        'Field grounders in front of the plate
        Call GetPBPString(13, 2850, fileString$)

        If Left$(fileString$, 2) = "QQ" Then
            fileString$ = X$ + Right$(fileString$, Len(fileString$) - 2)
        End If

        Call PBP(fileString$)

    Else

        Call GetPBPString(120, 1420, fileString$)
        Call PBP(X$ + " " + fileString$)

    End If

End Sub


'----------------------------------------
'          HITS2GAP Subroutine
'----------------------------------------
' ...explanation...
Sub HITS2GAP (currFielder, I1)

    Call GETFIELDER(currFielder, I1)

    Call Rolld100(RN0)

    Select Case RN0

        Case Is <= 5
            currFielder = 8

        Case 6 To 15

            If I1 = 1 Then
                currFielder = 9
            End If

            If I1 = -1 Then
                currFielder = 7
            End If

        Case 16 To 100

            If I1 = 1 Then
                currFielder = 7
            End If

            If I1 = -1 Then
                currFielder = 9
            End If

    End Select

    player$(4) = batters$(D, fielder(D, currFielder))

End Sub

'----------------------------------------
'          HOLDSTATUS Subroutine
'----------------------------------------
'This subroutine displays the hold status
'of the infield for traditional text mode.
Sub HOLDSTATUS (runnerHeld)

    Locate 21, 77

    Color 15, 2

    text$ = heldStatus$(runnerHeld)

    Print heldStatus$(runnerHeld)
    Color , 0

End Sub


'----------------------------------------
'          INCBATDEF Subroutine
'----------------------------------------
'This routines increments defensive
'batting stats for the target category.
Sub INCBATDEF (team, player, category)

    oppBatStats(team, player, category) = oppBatStats(team, player, category) + 1
    teamBatStats(team, player, category) = teamBatStats(team, player, category) + 1

End Sub


'----------------------------------------
'          INCBATOFF Subroutine
'----------------------------------------
'This routines increments offensive
'batting stats for the target category.
Sub INCBATOFF (team, player, category)

    opp = 1 - team

    oppBatStats(team, player, category) = oppBatStats(team, player, category) + 1
    teamBatStats(team, player, category) = teamBatStats(team, player, category) + 1

    If category >= 4 And category <= 8 And category <> 1 And pitchRating(opp, currPitcher(opp), SS_P_Throws) <> 1 Then

        If category >= 4 Then
            CSS(team, player, category - 2) = CSS(team, player, category - 2) + 1
        Else
            CSS(team, player, category) = CSS(team, player, category) + 1
        End If

    End If

End Sub


'----------------------------------------
'           INCPITDEF Subroutine
'----------------------------------------
'This routines increments defensive
'pitching stats for the target category.
Sub INCPITDEF (team, player, category)

    oppPitStats(team, player, category) = oppPitStats(team, player, category) + 1
    teamPitStats(team, player, category) = teamPitStats(team, player, category) + 1

End Sub


'----------------------------------------
'          INCPITOFF Subroutine
'----------------------------------------
'This routines increments offensive
'pitching stats for the target category.
Sub INCPITOFF (team, player, category)

    oppPitStats(team, player, category) = oppPitStats(team, player, category) + 1
    teamPitStats(team, player, category) = teamPitStats(team, player, category) + 1

End Sub


'----------------------------------------
'        INCREMENTHIT Subroutine
'----------------------------------------
'This routines increments all stats
'related to GIDP for both teams.
Sub INCREMENTHIT (team, MO)

    If fldPos(team, currLineupSlot(team)) <> 1 Then
        Call INCBATOFF(team, lineupPlayer(team, currLineupSlot(team)), LG_B_GIDP)
    Else
        Call INCPITOFF(team, currPitcher(team), LG_PB_GIDP)
    End If

    Call INCPITDEF(D, currPitcher(D), LG_P_GIDP)

    If MO > 0 Then MO = MO - 1

End Sub


'----------------------------------------
'          InjuredPitcher Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'and replacement for a pitcher who has
'been ejected or injured.
Sub InjuredPitcher (playerInjured, pinchHitterFlag, baseStatus)

    'Mark that injury has occurred and disable pitcher
    playerInjured = 1
    pitchRating(D, currPitcher(D), SS_P_Avail) = 99

    If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then
        Print #11, "DEBUG: Calling RELIEVERS. (InjuredPitcher) Pitcher="; currPitcher(D); "  Outs="; nbrOuts(0)
        Call RELIEVERS(pinchHitterFlag, newPitcherFlag, baseStatus)
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
'handle the associated game mechanics.
Sub INJURYLENGTH (injCount, posIdx, W5, W6)

    doneLength = 0

    injCount = injCount + 1
    injPlayers$(injCount) = batters$(D, fielder(D, posIdx))

    pbpString$ = batters$(D, fielder(D, posIdx)) + " is injured!"
    Call PBP(pbpString$)

    pbpString$ = "He may be out for "

    ' *** Determine Injury Length ***
    injLen = 0

    If batRating(D, fielder(D, posIdx), SS_B_G) = 162 Then

        pbpString$ = pbpString$ + " the rest of the game"

    Else

        ct = 0

        For I = 0 To 22

            For I1 = 22 To 25

                If batRating(D, I, I1) = posIdx And batRating(D, I, SS_B_Injured) = 0 And I <> fielder(D, posIdx) Then
                    ct = ct + 1
                End If

                If ct > 3 Then
                    injLen = 1
                    doneLength = 1
                    Exit For
                End If

            Next

        Next

        If doneLength = 0 Then
            pbpString$ = pbpString$ + " the rest of the game"
            injLen = 0
        Else
            If injLen <> 0 Then

                If batRating(D, fielder(D, posIdx), SS_B_G) = 161 Then
                    pbpString$ = pbpString$ + " this game and next game"
                    injLen = 1
                Else
                    injLen = Int(Rnd(1) * (162 - batRating(D, fielder(D, posIdx), SS_B_G))) + 1
                    pbpString$ = pbpString$ + Str$(injLen) + " games"
                End If

            End If

        End If

    End If

    Call PBP(pbpString$)
    batRating(D, fielder(D, posIdx), SS_B_Injured) = batRating(D, fielder(D, posIdx), SS_B_Injured) + injLen

    Call DELAY
    Call DELAY

    Call EJECTIONINJURY(posIdx, W5, W6)

End Sub


'----------------------------------------
'      DisplayInningScore Subroutine
'----------------------------------------
'This routine calculates where to locate
'the cursor to print the score for the
'current inning in the game's text mode.
Sub DisplayInningScore (currInning, teamLine)

    Dim setOf9, inningInSet, col

    ' Each set of 9 innings repeats the same column pattern
    setOf9 = (currInning - 1) \ 9 ' Which set: 0, 1, 2...
    inningInSet = (currInning - 1) Mod 9 ' Position in set: 0-8

    ' Base column calculation: 3 positions per inning + base offset
    ' Extra offset every 3 innings within each set
    col = 3 * (inningInSet + 1) + 7 + (inningInSet \ 3)

    ' Shift right for subsequent sets (appears to be 30 columns per set)
    If setOf9 > 0 Then col = col + setOf9 * 30

    Locate , col

    Print Using "##"; inningScore(teamLine, currInning);

End Sub


'----------------------------------------
'          LDRIVE_PBP Subroutine
'----------------------------------------
'This subroutine generates a some text
'to accompany the Play-by-Play for a line
'drive.
Sub LDRIVE_PBP (X$)

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 20:
            X$ = "rips"

        Case 21 To 40:
            X$ = "hammers"

        Case 41 To 60:
            X$ = "lashes"

        Case 61 To 80:
            X$ = "lines"

        Case 81 To 100:
            X$ = "drives"

    End Select

End Sub


'----------------------------------------
'           LONGFLY Subroutine
'----------------------------------------
'This subroutine handles the variables and
'Play-by-Play text for a long fly.
Sub LONGFLY (currFielder)

    Call GetPBPString(60, 1310, fileString$)
    Call PBP(fileString$ + " " + fieldPos$(currFielder))

    If WF < 1 And Rnd(1) > WF Then

        Call PBP("The wind is bringing it back..."):

    Else

        'Adjust for the wind factor
        If WF > 1 And Rnd(1) <= 1 - WF Then
            Call PBP("It's carrying well in the breeze...")
        End If

        Call GetPBPString(50, 1370, fileString$)
        Call PBP(fileString$)

        Call Rolld100(RN0)

        If (teamRat_GAME(1, 13) <> 21 And teamRat_GAME(1, 13) <> 27) Or currFielder <> 7 Then

            Select Case RN0

                Case 1 To 5:
                    Call PBP(player$(4) + " leaps")

                Case 6 To 10:
                    Call PBP(player$(4) + " against the wall")

                Case 11 To 20:
                    Call PBP(player$(4) + " jumps at the wall")

                Case 21 To 30:
                    Call PBP(player$(4) + " climbs the wall")

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
Sub MAINSCREEN (nbrBalls, nbrStrikes, MO)

    For I = 5 To 13
        Locate I, 1
        Print Space$(44);
    Next I

    'Locate 5, 1
    'Print home plate "graphic"
    Locate 23, 48
    Color 15, 2: Print Tab(62); Chr$(219); Tab(79);

    Locate 10, 1

    Color teamRat_GAME(D, 11), teamRat_GAME(D, 12)
    Print pitchers$(D, currPitcher(D)); " ["; handed$(pitchRating(D, currPitcher(D), SS_P_Throws) + 2); "]"

    Color 15, 0

    Call DISPBAVG

    Color 1

    For X = 5 To 13:
        Locate X, 21: Print Chr$(179);
    Next

    Locate 11, 1
    Color 2, 0: Print "W-L ERA ";

    Color 15, 0

    'Wins - losses
    Print Using "##"; pitchRating(D, currPitcher(D), SS_P_W);
    Print "-";
    Print Using "##"; pitchRating(D, currPitcher(D), SS_P_L);

    'ERA
    Print " ";: Print Using "##.##"; pitchRating(D, currPitcher(D), SS_P_ERA100) / 100

    Color 2, 0

    Print "  IP  H  BB  K GAME"

    Color 15, 0

    'IP
    If teamPitStats(D, currPitcher(D), 0) = 0 Then
        Print " 0.0";
    Else
        Print Using "##"; Int(teamPitStats(D, currPitcher(D), 0) / 3);
        Print ".";
        Print Using "#"; teamPitStats(D, currPitcher(D), 0) - (Int(teamPitStats(D, currPitcher(D), 0) / 3) * 3);
    End If

    'H / BB / K
    Print Using " ##  ## ##"; teamPitStats(D, currPitcher(D), 1); teamPitStats(D, currPitcher(D), 4); teamPitStats(D, currPitcher(D), 5)

    Locate 5, 1
    J = lineupPlayer(P, currLineupSlot(P))
    Color teamRat_GAME(P, 11), teamRat_GAME(P, 12)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        'Pitcher Name
        X$ = pitchers$(P, currPitcher(P))
        Print X$; " ["; handed$(pitchRating(P, currPitcher(P), SS_P_Throws) + 2); "]";
        Call PITPLACEMENT(X$)
    Else
        'Batter Name
        X$ = batters$(P, J)
        Print X$; " ["; handed$(batRating(P, J, SS_B_Bats) + 2); "]";
        If batRating(P, J, SS_B_Bats) < 0 Or batRating(P, J, SS_B_Bats) = 0 And pitchRating(D, currPitcher(D), SS_P_Throws) > 0 Then
            Locate 23, 64
        End If
        If batRating(P, J, SS_B_Bats) > 0 Or batRating(P, J, SS_B_Bats) = 0 And pitchRating(D, currPitcher(D), SS_P_Throws) < 0 Then
            Locate 23, 51 + 10 - Len(X$)
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
        Print teamBatStats(P, lineupPlayer(P, currLineupSlot(P)), 1); "-"; teamBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0);
    Else
        Print teamPitStats(P, currPitcher(P), 24); "-"; teamPitStats(P, currPitcher(P), 23);
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
    'Locate 25, 46: Print "INFIELD:"

    'Locate 26, 46
    'Select Case INFPOS%
    '    Case 0: Print "NORMAL    "
    '    Case 1: Print "CORNERS IN"
    '    Case 2: Print "INFIELD IN"
    'End Select

    Locate 29, 46: Print "INFIELD: ";

    Select Case INFPOS%
        Case 0: Print "NORMAL    ";
        Case 1: Print "CORNERS IN";
        Case 2: Print "INFIELD IN";
    End Select

    Call HOLDSTATUS(runnerHeld)

    'Clear runner on 2nd base
    Locate 15, 58
    Color 15, 2: Print Tab(68);
    Locate 16, 58: Print Space$(8)
    Locate 15, 58

    Call DisplayRun(2, 58)

    If nbrOuts(0) <> 3 Then
        MO = 0
    End If

    For WK = 1 To 3
        If runnerOnBase(WK) > 0 Then
            MO = MO + 1
        End If
    Next

    'Clear runner on 3rd base
    Locate 20, 47
    Color 15, 2: Print Tab(57);
    Locate 21, 47: Print Space$(8)
    Locate 20, 47

    Call DisplayRun(3, 47)

    'Clear runner on 1st base
    Locate 20, 69
    Color 15, 2: Print Tab(79);
    Locate 21, 70: Print Space$(8)
    Locate 20, 69

    Call DisplayRun(1, 70)

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

        Erase pbpText$

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
Sub NEWPITCHER (pIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)

    If relieverOnRecordForSave(D) <> -1 And gamePitcher(D) > 1 Then
        Call INCPITDEF(D, currPitcher(D), LG_P_Hold)
    End If

    currPitcher(D) = pIdx
    gamePitcher(D) = gamePitcher(D) + 1
    pitchRating(D, pIdx, SS_P_Avail) = gamePitcher(D)
    P9 = D

    If gamePitcher(D) > 1 Then
        battersFaced(D, currPitcher(D)) = 0
    End If

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

    Call CHECKSAVE(baseStatus)

    W5 = 1 - P9
    W6 = 1 - P9

    VV(1 - P9) = 1
    VV1(1 - P9) = 9

    Call WINDEX

    Locate 15, 1

    If d100% <= 50 Then
        Call PBP("Hold the phone...")
    End If

    Call PBP(gameManagers$(D) + " is making a pitching change")
    Call DELAY

    Call DisplayGameState(nbrBalls, nbrStrikes, MO, INFPOS%)

    pinchHitterFlag = 0

    newPitcherFlag = 1

End Sub

'----------------------------------------
'        NOJUMP Subroutine
'----------------------------------------
'NoJump Play-by-Play
Sub NOJUMP ()

    Call GetPBPString(20, 1050, fileString$)
    Call PBP(fileString$)
    Call DELAY

End Sub

'----------------------------------------
'        OUTATFIRST Subroutine
'----------------------------------------
'This subroutine handles the stats and
'Play-by-Play for an out at first base.
Sub OUTATFIRST (currFielder, fldrUnassist, FB2P%)

    Call PITCHERNAME(currFielder)
    Call GROUNDERS(currFielder)

    If currFielder = 3 And d100% <= 94 Then
        Call PBP("he takes it to the bag unassisted")
        fldrUnassist = 1
    Else
        If currFielder = 3 Then
            currFielder = 1
            Call PITCHERNAME(currFielder)
            Call PBP("He flips it to " + player$(4) + " covering")
            FB2P% = 1
            currFielder = 3
        End If

        If currFielder = 4 Then
            Call SECOND2FIRST
        End If

        If currFielder = 5 Or currFielder = 6 Then
            Call THROW2FIRST
        End If

    End If

End Sub

'----------------------------------------
'        OUTFIELDERR Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for an outfield error.
Sub OUTFIELDERR ()

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 20:
            pbpString$ = player$(4) + " can't find it"

        Case 21 To 40:
            pbpString$ = player$(4) + " misjudges it"

        Case 41 To 60:
            pbpString$ = player$(4) + " turns the wrong way"

        Case 61 To 80:
            pbpString$ = player$(4) + " loses it"

        Case 81 To 100:
            pbpString$ = player$(4) + " doesn't see it"

    End Select

    Call PBP(pbpString$)

End Sub


'----------------------------------------
'         OUTFLDNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'specified outfielder.
Sub OUTFLDNAME (currFielder)

    currFielder = 9

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 5:
            currFielder = 7

        Case 6 To 25:
            currFielder = 8

    End Select

    player$(4) = batters$(D, fielder(D, currFielder))

End Sub


'----------------------------------------
'          PICKEDOFF Subroutine
'----------------------------------------
'This subroutine covers the stats and the
'Play-by-Play for a runner who is picked
'off by the pitcher or catcher.
Sub PICKEDOFF (pickedOffBase)

    'Print #11, "Picked Off"
    Call WINDEX
    Call DELIVERY

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 33:
            pbpString$ = "NO, " + pitchers$(D, currPitcher(D)) + " throws to " + baseName$(pickedOffBase - 1)

        Case 34 To 66:
            pbpString$ = "quick move to " + baseName$(pickedOffBase - 1)

        Case 67 To 100:
            pbpString$ = "pickoff throw to " + baseName$(pickedOffBase - 1)

    End Select

    Call PBP(pbpString$)

    If d100% <= 50 Then
        pbpString$ = player$(pickedOffBase - 1) + " is picked off base!"
    Else
        pbpString$ = player$(pickedOffBase - 1) + " is caught napping"
    End If

    Call PBP(pbpString$)

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 25:
            pbpString$ = "he can't believe it"

        Case 26 To 50:
            pbpString$ = "what a teriffic move by " + pitchers$(D, currPitcher(D))

    End Select

    Call PBP(pbpString$)

End Sub

'----------------------------------------
'         PINCHRUNNER Subroutine
'----------------------------------------
'This subroutine determines if the PC or
'manager profile should use a pinch runner.
'If so, it substitutes them into the lineup.
Sub PINCHRUNNER (scoreDiff, baseStatus, tgtBase, nbrBalls, nbrStrikes, MO, INFPOS%, runnerFound, runnerSubstituted)

    Dim availableRunners(24)

    ' *** CHECK FOR PINCH RUNNER ***
    nbrPinchRunners = 0

    ' Only consider pinch running in late innings (8+) when game is close
    If INNING% >= 8 And scoreDiff <= 1 And scoreDiff >= -2 Then

        tgtBase = 1

        If baseStatus = 1 Or baseStatus = 5 Then
            tgtBase = 1
        End If

        If baseStatus = 2 Or baseStatus = 4 Or baseStatus = 6 Then
            tgtBase = 2
        End If

        J = runnerOnBase(tgtBase)
        J1 = tgtBase

        'Verify there's a runner on base
        If J <> 0 Then

            'Check for manager strategy first
            If teamMgrProfileVal(P, 771) = 999 Then

                If Environ$("MGRLOG") = "TRUE" Then
                    Print #11, "!! Team is using a PINCH RUNNING strategy !!"
                End If

                'Loop through designated PR's
                For I1 = 772 To 777

                    If teamMgrProfileVal(P, I1) <> -1 Then

                        'Check if pinch runner is available
                        If batRating(P, teamMgrProfileVal(P, I1), SS_B_Avail) = 0 Then

                            I1 = teamMgrProfileVal(P, I1)

                            'Check if there is a defined pinch runner for pitchers
                            If fldPos(P, J) = 1 And teamMgrProfileVal(P, 803) = 999 Then

                                If Environ$("MGRLOG") = "TRUE" Then
                                    Print #11, "!! Team is using a PINCH RUNNING for PITCHER strategy !!"
                                End If

                                Call Rolld100(RN0)

                                'Check pitcher run rating, inning and probability
                                If pitchRating(P, currPitcher(P), SS_P_Run) <= teamMgrProfileVal(P, 804) And INNING% >= teamMgrProfileVal(P, 805) And RN0 <= teamMgrProfileVal(P, 806) Then
                                    GoTo 8993
                                End If

                                If pitchRating(P, currPitcher(P), SS_P_Run) <= teamMgrProfileVal(P, 807) And INNING% >= teamMgrProfileVal(P, 808) And RN0 <= teamMgrProfileVal(P, 809) Then
                                    GoTo 8993
                                End If

                                'Conditions not met; don't pinch run
                                Exit Sub

                            End If

                            'Check for substitution at fielding position
                            For I = 0 To 5

                                If teamMgrProfileVal(P, I * 4 + 779) = lineupPlayer(P, J) Then

                                    'FOUND A MATCH FOR THE RUNNER ON BASE
                                    If Environ$("MGRLOG") = "TRUE" Then
                                        Print #11, "!! Team is using a DEFENSE SUBSTITUTION strategy !!"
                                    End If

                                    Call Rolld100(RN0)
                                    If INNING% >= teamMgrProfileVal(P, I * 4 + 780) And batRating(P, teamMgrProfileVal(P, I * 4 + 781), SS_B_Avail) = 0 And RN0 <= teamMgrProfileVal(P, I * 4 + 782) Then
                                        batRating(P, teamMgrProfileVal(P, I * 4 + 781), SS_B_Avail) = 40 + J
                                        GoTo 8993
                                    End If

                                    Exit Sub

                                End If
                            Next

                        End If

                        'Mark slot as used
                        teamMgrProfileVal(P, I1) = -1

                    End If

                Next

                Exit Sub

            End If

            ' Don't pinch run if pitcher has run rating > 4 or position player > 3
            If fldPos(P, J) = 1 And pitchRating(P, currPitcher(P), SS_P_Run) > 4 Or _
                fldPos(P, J) <> 1 And batRating(P, lineupPlayer(P, J), SS_B_Run) > 3 Then
                Exit Sub
            End If

            'Index for replacement player
            I1 = -1

            'Find available runners and a fielding replacement
            For I = 0 To 22

                'Look for available batters (not yet in the game)
                If batters$(P, I) <> "XXX" And batRating(P, I, SS_B_Avail) <= 0 Then

                    'Check that player can play the fielding position
                    For I2 = 22 To 25
                        If batRating(P, I, I2) <> -1 And batRating(P, I, I2) = fldPos(P, J) And fldPos(P, J) <> 1 Then
                            I1 = I
                        End If
                    Next

                    'Check if player is fast enough
                    If batRating(P, I, SS_B_Run) > 5 Then
                        availableRunners(nbrPinchRunners) = I
                        nbrPinchRunners = nbrPinchRunners + 1
                    End If

                End If

            Next

            'Make sure we have both pinch runner and fielding replacement
            If nbrPinchRunners = 0 Or I1 = -1 And fldPos(P, J) <> 1 Then
                Exit Sub
            End If

            'Mark fielding position replacement as used
            If fldPos(P, J) <> 1 Then
                batRating(P, I1, SS_B_Avail) = 40 + J
            End If

            'Randomly select available runner
            RN0 = Int(Rnd(1) * nbrPinchRunners)
            I1 = availableRunners(RN0)

            'Execute substitution
            8993 '
            I2 = J
            player$(J1) = batters$(P, I1)

            'Make player available and insert into lineup / field
            batRating(P, I1, SS_B_Avail) = 1
            lineupPlayer(P, I2) = I1
            fielder(P, fldPos(P, I2)) = I1

            If fldPos(P, I2) = 1 Or fldPos(P, I2) = 10 Then
                fielder(P, 0) = I2
                fielder(P, 1) = 99
                fldPos(P, I2) = 10
            End If

            'Track game roster with substitution
            playersUsed(P) = playersUsed(P) + 1
            gameRoster(P, 0, playersUsed(P)) = lineupPlayer(P, I2)
            gameRoster(P, 1, playersUsed(P)) = 11 '11 = Pinch Runner
            gameRoster(P, 2, playersUsed(P)) = (I2 * 10) + (playersUsed(P) - 9)

            W5 = P
            W6 = P

            VV(P) = I2
            VV1(P) = I2

            'Set defensive ratings for fielding position
            For xx = 22 To 25
                If fldPos(P, I2) = batRating(P, I1, xx) Then
                    batRating(P, I1, SS_B_Arm) = batRating(P, I1, 33 + (xx - 22) * 2)
                    batRating(P, I1, SS_B_Range) = batRating(P, I1, 34 + (xx - 22) * 2)
                    batRating(P, I1, SS_B_FldPct) = batRating(P, I1, xx + 4)
                    Exit For
                End If
            Next

            'Announce substitution
            Call WINDEX
            Locate 15, 1
            Call PBP(gameManagers$(P) + " is sending in a pinch runner")

            runnerFound = 1

            Call DELAY

            Call DisplayGameState(nbrBalls, nbrStrikes, MO, INFPOS%)

            runnerSubstituted = 1

            VV(P) = 1
            VV1(P) = 9

        End If

    End If

End Sub

'----------------------------------------
'         PITCHERNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'associated pitcher.
Sub PITCHERNAME (currFielder)

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
Sub PITCHOUT (baseStatus, TGTBASE%, stlChance)

    If baseStatus <> 5 And TGTBASE% = 2 Then

        If d100% <= 8 Then
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
Sub PITDISP ()

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

            Print Using " ### ### ### ## ## ## ## ### ### ##"; pitchRating(P9, pIdx, SS_P_IP); pitchRating(P9, pIdx, SS_P_H); pitchRating(P9, pIdx, 4); pitchRating(P9, pIdx, 5); pitchRating(P9, pIdx, 16); pitchRating(P9, pIdx, 3); pitchRating(P9, pIdx, 13); pitchRating(P9, pIdx, SS_P_BB); pitchRating(P9, pIdx, SS_P_K); pitchRating(P9, pIdx, SS_P_W);
            Print "-";
            Print Using "##"; pitchRating(P9, pIdx, SS_P_L);

            Locate , 59: Print Using "##.## # ##"; pitchRating(P9, pIdx, SS_P_ERA100) / 100; pitchRating(P9, pIdx, SS_P_HoldRunner); pitchRating(P9, pIdx, SS_P_GndPct)

        End If

    Next pIdx

End Sub


'----------------------------------------
'      RemovePitcherAtBat Subroutine
'----------------------------------------
'This subroutine undoes the at-bat for a
'pitcher in the event of a sacrifice fly.
Sub RemovePitcherAtBat (team, player, category)

    oppPitStats(team, player, category) = oppPitStats(team, player, category) - 1
    teamPitStats(team, player, category) = teamPitStats(team, player, category) - 1

End Sub


'----------------------------------------
'        PITPLACEMENT Subroutine
'----------------------------------------
'This subroutine determines where to place
'the pitcher text and prints the info. at
'that place.
Sub PITPLACEMENT (X$)

    If useVGA = 1 Then
        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color &HFFFFFFFF

    End If

    If pitchRating(P, currPitcher(P), SS_P_Throws) < 0 Then
        If useVGA = 1 Then
            PrintOnGfx 42, 25, X$, imageScreen&
        Else
            Locate 23, 64
            Print X$
        End If
    End If

    If pitchRating(P, currPitcher(P), SS_P_Throws) > 0 Then
        If useVGA = 1 Then
            PrintOnGfx 30, 25, X$, imageScreen&
        Else
            Locate 23, 51
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
'This subroutine determines the current
'pitcher's stamina
Sub PITSTAM (teamIdx)

    'Hits + Walks
    If pitchRating(teamIdx, currPitcher(teamIdx), SS_P_G) = 0 Then
        pitcherStam(teamIdx, 0) = 3
    Else
        pitcherStam(teamIdx, 0) = pitcherMaxStam%(teamIdx, currPitcher(teamIdx))
    End If

    'Minimum value of 3
    If pitcherStam(teamIdx, 0) < 3 Then
        pitcherStam(teamIdx, 0) = 3
    End If

    If gamePitcher(teamIdx) = 1 And pitchEraOpt > 0 Then
        pitcherStam(teamIdx, 0) = pitcherStam(teamIdx, 0) + 1
    End If

    'Runs
    If gamePitcher(teamIdx) = 1 Then
        'starting pitcher
        If pitchEraOpt < 3 Then
            'already set in LOADER when pitchEraOpt >= 3)
            pitcherStam(teamIdx, 1) = 3
        End If
    Else
        'relief pitcher
        If pitchEraOpt = 0 Then
            pitcherStam(teamIdx, 1) = 0
        Else
            pitcherStam(teamIdx, 1) = 1
        End If
    End If

    If INNING% <= 5 And gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) > 0 Then
        pitcherOfRecord(teamIdx) = currPitcher(teamIdx)
    End If

End Sub


'----------------------------------------
'          PITSTRAT Subroutine
'----------------------------------------
'This subroutine loads a pitcher strategy
'from a manager profile.
Sub PITSTRAT (pinchHitterFlag, newPitcherFlag, baseStatus)

    Shared setupUsed(), closerUsed()

    ourLead = gameScore(D, 0) - gameScore(P, 0)

    newPitcherFlag = 0

    If Environ$("MGRLOG") = "TRUE" Then
        Print #11, Using "Team is using 60 is ####  90 is #####  520 is ####  581 is ####"; _
        teamMgrProfileVal(D, 60), teamMgrProfileVal(D, 90), teamMgrProfileVal(D, 520), teamMgrProfileVal(D, 581)
    End If

    'Check if we need a middle reliever, as well
    'as whether we have one to use
    If teamMgrProfileVal(D, 60) = 999 Or INNING% > 7 Then

        'If no closers set up or no closing strategy defined, skip out
        If teamMgrProfileVal(D, 581) = 999 And (INNING% < 8 Or teamMgrProfileVal(D, 520) = 999 Or teamMgrProfileVal(D, 90) = 999) Then

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "!! Team is using a PITCHING strategy !!"
            End If

            'if still needing middle relief
            If INNING% <= 7 Then
                GoTo LMiddleRelievers
            End If

            'We have defined a closing/setup possibility
            strategyWeight = 0

            For pIdx = 582 To 587
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx)
            Next

            'Random selection of strategy
            rndStrategy = Int(Rnd(1) * strategyWeight) + 1

            strategyWeight = 0

            For pIdx = 582 To 587

                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx)

                If rndStrategy <= strategyWeight Then

                    strategyNbr = pIdx

                    'Determine if we need a Save
                    saveSituation = 0

                    'Can't Save if winning
                    If ourLead < 1 Then Exit Sub

                    If (ourLead > 0 And ourLead <= 2) Or _
                        (baseStatus = 0 And ourLead <= 2) Or _
                        (baseStatus > 0 And baseStatus < 4 And ourLead <= 3) Or _
                        (baseStatus > 3 And baseStatus < 7 And ourLead <= 4) Or _
                        (baseStatus = 7 And ourLead <= 5) Or _
                        (baseStatus = 0 And nbrOuts(0) = 0 And ourLead <= 3 ) Then
                        saveSituation = 1
                    End If

                    'Analyze next 3 hitters handedness to decide on specialized relievers
                    If currLineupSlot(P) <= 7 Then
                        For pIdx = 0 To 2:
                            rndStrategy = rndStrategy + batRating(P, lineupPlayer(P, currLineupSlot(P) + pIdx), SS_B_Bats)
                        Next
                    End If

                    'If we've reached slot 8, we have to wrap to 1 after 9
                    If currLineupSlot(P) = 8 Then
                        For pIdx = 0 To 1:
                            rndStrategy = rndStrategy + batRating(P, lineupPlayer(P, currLineupSlot(P) + pIdx), SS_B_Bats)
                        Next
                        rndStrategy = rndStrategy + batRating(P, lineupPlayer(P, 1), SS_B_Bats)
                    End If

                    'If we've reached slot 9, we have to wrap to 1 and 2
                    If currLineupSlot(P) = 9 Then
                        For pIdx = 1 To 2:
                            rndStrategy = rndStrategy + batRating(P, lineupPlayer(P, pIdx), SS_B_Bats)
                        Next
                        rndStrategy = rndStrategy + batRating(P, lineupPlayer(P, 9), SS_B_Bats)
                    End If

                    'Fallback in case we haven't found anything
                    If rndStrategy = 0 Then
                        Do
                            rndStrategy = Int(Rnd(1) * 2) + 1
                        Loop Until rndStrategy > 0
                    End If

                    'Late game - determine if we need Setup or Closer
                    '  8th Inning
                    If INNING% = 8 And teamMgrProfileVal(D, 588) = 8 And _ 
                        ((saveSituation = 1 And (strategyNbr = 582 Or strategyNbr = 587)) Or _ 
                        (ourLead > 0 And strategyNbr = 583) Or _ 
                        (ourLead >= 0 And strategyNbr = 584) Or _ 
                            ( pitcherStam(D, 0) <= teamPitStats(D, currPitcher(D), 4) + teamPitStats(D, currPitcher(D), 1) And _ 
                                pitcherStam(D, 1) <= teamPitStats(D, currPitcher(D), 2) And _ 
                                ((ourLead > 0 And strategyNbr = 585) Or (ourLead >= 0 And strategyNbr = 586)) ) ) Then

                        If teamMgrProfileVal(D, 589) = 0 Then
                            GoTo LSetupMen
                        ElseIf teamMgrProfileVal(D, 589) = 1 Then
                            GoTo LClosers
                        End If

                    End If

                    '  9th Inning
                    If INNING% >= 9 And _ 
                        ( (saveSituation = 1 And (strategyNbr = 582 Or strategyNbr = 587)) Or _ 
                            (ourLead > 0 And strategyNbr = 583) Or (ourLead >= 0 And strategyNbr = 584) Or _ 
                            (pitcherStam(D, 0) <= teamPitStats(D, currPitcher(D), 4) + teamPitStats(D, currPitcher(D), 1) And _ 
                                pitcherStam(D, 1) <= teamPitStats(D, currPitcher(D), 2) And _ 
                                ((ourLead > 0 And strategyNbr = 585) Or (ourLead >= 0 And strategyNbr = 586)) ) ) Then
                        GoTo LClosers

                    End If

                    Exit For

                End If

            Next

        End If

    End If

    Exit Sub


    'Strategy identified, now:
    LSetupMen:
    strategyWeight = 0
    If rndStrategy < 0 Then I1 = 81
    If rndStrategy > 0 Then I1 = 84

    'Sections below break logic into separate IF's, because QB64 does not short-circuit

    For pIdx = I1 To I1 + 2

        'if not already used
        If teamMgrProfileVal(D, pIdx) >= 0 Then
            If pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx + 10)
            End If
        End If

    Next

    strategyWeight = 0
    rndStrategy = Int(Rnd(1) * strategyWeight) + 1

    For pIdx = I1 To I1 + 2

        'if not already used
        If teamMgrProfileVal(D, pIdx) >= 0 Then
            If pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx + 10)
            End If
        End If

        If rndStrategy <= strategyWeight Then

            V0 = teamMgrProfileVal(D, pIdx)
            teamMgrProfileVal(D, pIdx) = -1
            pIdx = V0

            Call NEWPITCHER(pIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)
            setupUsed(D) = -1

            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "Found SETUP MAN: "; pIdx
            End If

            Exit Sub

        End If

    Next
    'NO SETUP MAN FOUND

    LClosers:
    strategyWeight = 0
    If rndStrategy < 0 Then I1 = 521
    If rndStrategy > 0 Then I1 = 524

    For pIdx = I1 To I1 + 2

        'if not already used
        If teamMgrProfileVal(D, pIdx) >= 0 Then
            If pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx + 10)
            End If
        End If

    Next

    rndStrategy = Int(Rnd(1) * strategyWeight) + 1
    strategyWeight = 0

    For pIdx = I1 To I1 + 2

        'if not already used
        If teamMgrProfileVal(D, pIdx) >= 0 Then
            If pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx + 10)
            End If
        End If

        If rndStrategy <= strategyWeight Then
            V0 = teamMgrProfileVal(D, pIdx)
            teamMgrProfileVal(D, pIdx) = -1
            pIdx = V0
            Call NEWPITCHER(pIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)
            closerUsed(D) = -1
            If Environ$("MGRLOG") = "TRUE" Then
                Print #11, "Found CLOSER: "; pIdx
            End If
            Exit Sub
        End If

    Next

    'NO CLOSER FOUND
    setupUsed(D) = -1
    closerUsed(D) = -1
    If Environ$("MGRLOG") = "TRUE" Then
        Print #11, "Couldn't find SETUP MAN or CLOSER"
    End If

    LMiddleRelievers:
    strategyWeight = 0

    For pIdx = 61 To 70

        'if not already used
        If teamMgrProfileVal(D, pIdx) >= 0 Then
            If pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx + 10)
            End If
        End If

    Next

    rndStrategy = Int(Rnd(1) * strategyWeight) + 1
    strategyWeight = 0

    For pIdx = 61 To 70

        'if not already used
        If teamMgrProfileVal(D, pIdx) >= 0 Then
            If pitchRating(D, teamMgrProfileVal(D, pIdx), SS_P_Avail) = 0 Then
                strategyWeight = strategyWeight + teamMgrProfileVal(D, pIdx + 10)
            End If
        End If

        If rndStrategy <= strategyWeight Then

            V0 = teamMgrProfileVal(D, pIdx)
            teamMgrProfileVal(D, pIdx) = -1
            pIdx = V0

            Call NEWPITCHER(pIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)

            Exit Sub

        End If

    Next

    'NO middle RELIEVER FOUND

End Sub


'----------------------------------------
'           PITSWAP Subroutine
'----------------------------------------
'This routine records a stolen base for
'each of the runners.
Sub PITSWAP (baseNbr, wasError)

    Select Case baseNbr

        Case 4
            Call Add_StolenBase(3, wasError)
            Call Add_StolenBase(2, wasError)
            Call Add_StolenBase(1, wasError)

        Case 2, 3
            Call Add_StolenBase(2, wasError)
            Call Add_StolenBase(1, wasError)

    End Select

End Sub


'----------------------------------------
'         PLAYERNAME Subroutine
'----------------------------------------
'Assign names to current active "players"
'(batter / runner). Use generic array to
'make coding easier
Sub PLAYERNAME ()

    If fldPos(P, currLineupSlot(P)) = 1 Then
        player$(0) = pitchers$(P, currPitcher(P))
    Else
        player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))
    End If

    For pIdx = 1 To 3

        If runnerOnBase(pIdx) > 0 Then
            If fldPos(P, runnerOnBase(pIdx)) <> 1 Then
                player$(pIdx) = batters$(P, lineupPlayer(P, runnerOnBase(pIdx)))
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
Sub PLYRHRT (W5, W6, nbrBalls, nbrStrikes, MO)

    If ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then

        For pIdx = 0 To 22

            If batRating(P, pIdx, SS_B_Avail) <= 0 And batters$(P, pIdx) <> "XXX" And _ 
                (fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos1) Or fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos2) Or fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos3) Or fldPos(P, currLineupSlot(P)) = batRating(P, pIdx, SS_B_Pos4)) Then

                Call PinchHitter(W5, W6, pIdx)

                If useVGA = 1 Then
                    Call DisplayBallField_Gfx(nbrBalls, nbrStrikes)
                    Call MAINSCREEN_G(nbrBalls, nbrStrikes, MO, INFPOS%)
                Else
                    Call DisplayBallField(nbrBalls, nbrStrikes)
                    Call MAINSCREEN(nbrBalls, nbrStrikes, MO)
                    MS% = 1
                End If

                Locate 15, 1

                player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))
                If fldPos(P, currLineupSlot(P)) = 1 Then
                    player$(0) = pitchers$(P, currPitcher(D))
                End If

                Exit Sub

            End If

        Next pIdx

        For pIdx = 0 To 22

            If batRating(P, pIdx, SS_B_Avail) <= 0 And batters$(P, pIdx) <> "XXX" Then

                Call PinchHitter(W5, W6, pIdx)

                If useVGA = 1 Then
                    Call DisplayBallField_Gfx(nbrBalls, nbrStrikes)
                    Call MAINSCREEN_G(nbrBalls, nbrStrikes, MO, INFPOS%)
                Else
                    Call DisplayBallField(nbrBalls, nbrStrikes)
                    Call MAINSCREEN(nbrBalls, nbrStrikes, MO)
                    MS% = 1
                End If

                Locate 15, 1

                player$(0) = batters$(P, lineupPlayer(P, currLineupSlot(P)))
                If fldPos(P, currLineupSlot(P)) = 1 Then
                    player$(0) = pitchers$(P, currPitcher(D))
                End If

                Exit Sub

            End If

        Next pIdx

    Else

        Call PBP("You must replace " + player$(0) + " with a pinch-hitter")

    End If

End Sub


'----------------------------------------
'         PinchHitter Subroutine
'----------------------------------------
Sub PinchHitter (W5, W6, idx)

    pbpString$ = player$(0) + " will be replaced by " + batters$(P, idx)
    Call PBP(pbpString$)

    'Make player available and insert into lineup / field
    batRating(P, idx, SS_B_Avail) = 1
    lineupPlayer(P, currLineupSlot(P)) = idx
    fielder(P, fldPos(P, currLineupSlot(P))) = idx

    If fldPos(P, currLineupSlot(P)) = 1 Or fldPos(P, currLineupSlot(P)) = 10 Then
        fielder(P, 1) = 99
        fielder(P, 0) = currLineupSlot(P)
        fldPos(P, currLineupSlot(P)) = 10
    End If

    'Track game roster with substitution
    playersUsed(P) = playersUsed(P) + 1
    gameRoster(P, 0, playersUsed(P)) = idx
    gameRoster(P, 1, playersUsed(P)) = fldPos(P, currLineupSlot(P))
    gameRoster(P, 2, playersUsed(P)) = (currLineupSlot(P) * 10) + (playersUsed(P) - 9)

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
            statInnPitchCT! = batRating(WK, M, SS_B_LHP_H) / batRating(WK, M, SS_B_LHP_AB)
            Locate 4, 64: Print Using ".###"; statInnPitchCT!
        End If

        Locate 5, 56: Print "VS RHP  .000";
        If batRating(WK, M, SS_B_RHP_AB) <> 0 Then
            statInnPitchCT! = batRating(WK, M, SS_B_RHP_H) / batRating(WK, M, SS_B_RHP_AB)
            Locate 5, 64: Print Using ".###"; statInnPitchCT!
        End If

    End If

    Locate 8, 6: Print "SEASON";
    Locate , 15

    If batRating(WK, M, SS_B_AB) = 0 Then
        Print ".---  .---"
    Else
        statInnPitchCT! = batRating(WK, M, SS_B_H) / batRating(WK, M, SS_B_AB)
        statInnPitchCT2! = (batRating(WK, M, SS_B_H) + batRating(WK, M, SS_B_W)) / (batRating(WK, M, SS_B_AB) + batRating(WK, M, SS_B_W))
        Print Using ".###  .### ### ### ### ### ## ## ## ### ### ### "; statInnPitchCT!; statInnPitchCT2!; batRating(WK, M, SS_B_G); batRating(WK, M, SS_B_AB); batRating(WK, M, SS_B_R); batRating(WK, M, SS_B_H); batRating(WK, M, SS_B_2B); batRating(WK, M, SS_B_3B); batRating(WK, M, SS_B_HR); batRating(WK, M, SS_B_RBI);
        Print Using "### ### ### ##"; batRating(WK, M, SS_B_W); batRating(WK, M, SS_B_K); batRating(WK, M, SS_B_SB); batRating(WK, M, SS_B_CS)
    End If

    Locate , 6: Print "LEAGUE";

    If oppBatStats(WK, M, 0) = 0 Or oppBatStats(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If oppBatStats(WK, M, 0) = oppBatStats(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statInnPitchCT! = oppBatStats(WK, M, 1) / oppBatStats(WK, M, 0)
            Locate , 15: Print Using ".###"; statInnPitchCT!;
        End If
    End If

    If oppBatStats(WK, M, 0) + oppBatStats(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If oppBatStats(WK, M, 1) + oppBatStats(WK, M, 7) = oppBatStats(WK, M, 0) + oppBatStats(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            statInnPitchCT! = (oppBatStats(WK, M, 1) + oppBatStats(WK, M, 7)) / (oppBatStats(WK, M, 0) + oppBatStats(WK, M, 7))
            Locate , 21: Print Using ".###"; statInnPitchCT!;
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; oppBatStats(WK, M, 14); oppBatStats(WK, M, 0); oppBatStats(WK, M, 2); oppBatStats(WK, M, 1); oppBatStats(WK, M, 4); oppBatStats(WK, M, 5); oppBatStats(WK, M, 6); oppBatStats(WK, M, 3); oppBatStats(WK, M, 7); oppBatStats(WK, M, 8); oppBatStats(WK, M, 9); oppBatStats(WK, M, 10); oppBatStats(WK, M, 12); oppBatStats(WK, M, 13)

    Locate , 6: Print "GAME";

    If teamBatStats(WK, M, 0) = 0 Or teamBatStats(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If teamBatStats(WK, M, 0) = teamBatStats(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statInnPitchCT! = teamBatStats(WK, M, 1) / teamBatStats(WK, M, 0)
            Locate , 15: Print Using ".###"; statInnPitchCT!;
        End If
    End If

    If teamBatStats(WK, M, 0) + teamBatStats(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If teamBatStats(WK, M, 1) + teamBatStats(WK, M, 7) = teamBatStats(WK, M, 0) + teamBatStats(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            Locate , 21: Print Using ".###"; (teamBatStats(WK, M, 1) + teamBatStats(WK, M, 7)) / (teamBatStats(WK, M, 0) + teamBatStats(WK, M, 7));
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; teamBatStats(WK, M, 14); teamBatStats(WK, M, 0); teamBatStats(WK, M, 2); teamBatStats(WK, M, 1); teamBatStats(WK, M, 4); teamBatStats(WK, M, 5); teamBatStats(WK, M, 6); teamBatStats(WK, M, 3); teamBatStats(WK, M, 7); teamBatStats(WK, M, 8); teamBatStats(WK, M, 9); teamBatStats(WK, M, 10); teamBatStats(WK, M, 12); teamBatStats(WK, M, 13)

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
            statInnPitchCT! = pitchRating(WK, M, SS_P_LHP_H) / pitchRating(WK, M, SS_P_LHP_AB)
            Locate 15, 64: Print Using ".###"; statInnPitchCT!
        End If

        Locate 16, 56: Print "VS RHB  .000";
        If pitchRating(WK, M, SS_P_RHP_AB) <> 0 Then
            statInnPitchCT! = pitchRating(WK, M, SS_P_RHP_H) / pitchRating(WK, M, SS_P_RHP_AB)
            Locate 16, 64: Print Using ".###"; statInnPitchCT!
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
    Print Using "## ## ## "; oppPitStats(WK, M, 6); oppPitStats(WK, M, 7); oppPitStats(WK, M, 12);

    If oppPitStats(WK, M, 3) = 0 Or oppPitStats(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; oppPitStats(WK, M, 3) / (oppPitStats(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; oppPitStats(WK, M, 8); oppPitStats(WK, M, 9); oppPitStats(WK, M, 10);

    If oppPitStats(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(oppPitStats(WK, M, 0) / 3);
        Print ".";: Print Using "#"; oppPitStats(WK, M, 0) - (Int(oppPitStats(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; oppPitStats(WK, M, 1); oppPitStats(WK, M, 4); oppPitStats(WK, M, 5); oppPitStats(WK, M, 2); oppPitStats(WK, M, 3); oppPitStats(WK, M, 14)

    Locate , 6: Print "GAME     ";
    Print Using "## ## ## "; teamPitStats(WK, M, 6); teamPitStats(WK, M, 7); teamPitStats(WK, M, 12);

    If teamPitStats(WK, M, 3) = 0 Or teamPitStats(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; teamPitStats(WK, M, 3) / (teamPitStats(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; teamPitStats(WK, M, 8); teamPitStats(WK, M, 9); teamPitStats(WK, M, 10);

    If teamPitStats(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(teamPitStats(WK, M, 0) / 3);
        Print ".";: Print Using "#"; teamPitStats(WK, M, 0) - (Int(teamPitStats(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; teamPitStats(WK, M, 1); teamPitStats(WK, M, 4); teamPitStats(WK, M, 5); teamPitStats(WK, M, 2); teamPitStats(WK, M, 3); teamPitStats(WK, M, 14)

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
            Locate , 6: Print "BATS: "; handed$(pitchRating(P, currPitcher(P), SS_P_Throws) + 2); "    ";
            Print Using "G/A%: ##       BUNT: #       RUN: #"; 52; pitchRating(P, currPitcher(P), SS_P_Bunt); pitchRating(P, currPitcher(P), SS_P_Run):
        End If

    Else

        Print batters$(P, J)
        Locate , 6: Print "BATS: "; handed$(batRating(P, J, SS_B_Bats) + 2); "    ";
        Print Using "G/A%: ##       BUNT: #       RUN: #"; batRating(P, J, SS_B_GndPct); batRating(P, J, SS_B_Bunt); batRating(P, J, SS_B_Run)

    End If

    If fldPos(P, currLineupSlot(P)) = 1 And teamSplit(P, 0) = 999 Then

        Locate 4, 56: Print "VS LHP  .000";
        If pitchRating(P, currPitcher(P), 37) <> 0 Then
            statInnPitchCT! = pitchRating(P, currPitcher(P), 38) / pitchRating(P, currPitcher(P), 37)
            Locate 4, 64: Print Using ".###"; statInnPitchCT!
        End If

        Locate 5, 56: Print "VS RHP  .000";
        If pitchRating(P, currPitcher(P), 44) <> 0 Then
            statInnPitchCT! = pitchRating(P, currPitcher(P), 45) / pitchRating(P, currPitcher(P), 44)
            Locate 5, 64: Print Using ".###"; statInnPitchCT!
        End If

    End If

    Locate 6, 3
    Print " "; Chr$(204); String$(71, 205); Chr$(185); " "

    Locate , 6
    Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

    skipLeague = 0
    If fldPos(P, currLineupSlot(P)) = 1 And pitchRating(P, currPitcher(P), SS_P_BAvg) <> 999 Then

        statInnPitchCT! = pitchRating(P, currPitcher(P), SS_P_BAvg) '/ 1000
        Locate , 6: Print "SEASON";
        Locate , 15: Print Using ".###"; statInnPitchCT!

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

        If oppPitStats(P, J, 23) = 0 Or oppPitStats(P, J, 24) = 0 Then
            Locate , 15: Print ".000";
        Else
            If oppPitStats(P, J, 23) = oppPitStats(P, J, 24) Then
                Locate , 14: Print "1.000";
            Else
                statInnPitchCT! = oppPitStats(P, J, 24) / oppPitStats(P, J, 23)
                Locate , 15: Print Using ".###"; statInnPitchCT!;
            End If
        End If

        If oppPitStats(P, J, 23) + oppPitStats(P, J, 30) = 0 Then
            Locate , 21: Print ".000";
        Else
            If oppPitStats(P, J, 24) + oppPitStats(P, J, 30) = oppPitStats(P, J, 23) + oppPitStats(P, J, 30) Then
                Locate , 20: Print "1.000";
            Else
                statInnPitchCT! = (oppPitStats(P, J, 24) + oppPitStats(P, J, 30)) / (oppPitStats(P, J, 23) + oppPitStats(P, J, 30))
                Locate , 21: Print Using ".###"; statInnPitchCT!;
            End If
        End If

        Locate , 26: Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; oppPitStats(P, J, 8); oppPitStats(P, J, 23); oppPitStats(P, J, 25); oppPitStats(P, J, 24); oppPitStats(P, J, 27); oppPitStats(P, J, 28); oppPitStats(P, J, 29); oppPitStats(P, J, 26); oppPitStats(P, J, 30); oppPitStats(P, J, 31); oppPitStats(P, J, 32); oppPitStats(P, J, 33); oppPitStats(P, J, 35); oppPitStats(P, J, 36)
        Locate , 6: Print "GAME";

        If teamPitStats(P, J, 23) = 0 Or teamPitStats(P, J, 24) = 0 Then
            Locate , 15: Print ".000";
        Else
            If teamPitStats(P, J, 23) = teamPitStats(P, J, 24) Then
                Locate , 14: Print "1.000";
            Else
                statInnPitchCT! = teamPitStats(P, J, 24) / teamPitStats(P, J, 23)
                Locate , 15: Print Using ".###"; statInnPitchCT!;
            End If
        End If

        If teamPitStats(P, J, 23) + teamPitStats(P, J, 30) = 0 Then
            Locate , 21: Print ".000";
        Else
            If teamPitStats(P, J, 24) + teamPitStats(P, J, 30) = teamPitStats(P, J, 23) + teamPitStats(P, J, 30) Then
                Locate , 20: Print "1.000";
            Else
                statInnPitchCT! = (teamPitStats(P, J, 24) + teamPitStats(P, J, 30)) / (teamPitStats(P, J, 23) + teamPitStats(P, J, 30))
                Locate , 21: Print Using ".###"; statInnPitchCT!;
            End If
        End If

        Locate , 26
        Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; teamPitStats(P, J, 8); teamPitStats(P, J, 23); teamPitStats(P, J, 25); teamPitStats(P, J, 24); teamPitStats(P, J, 27); teamPitStats(P, J, 28); teamPitStats(P, J, 29); teamPitStats(P, J, 26); teamPitStats(P, J, 30); teamPitStats(P, J, 31); teamPitStats(P, J, 32); teamPitStats(P, J, 33); teamPitStats(P, J, 35); teamPitStats(P, J, 36)

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
'This subroutine records the stats for
'the target player being caught stealing
Sub PUTOUT (poBase)

    If fldPos(P, runnerOnBase(poBase - 1)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_CS)
    Else
        Call INCBATOFF(P, lineupPlayer(P, runnerOnBase(poBase - 1)), LG_B_CS)
    End If

End Sub


'----------------------------------------
'          PlayerOut Subroutine
'----------------------------------------
'This subroutine handles the crediting
'of an out, along with the Play-by-Play
Sub PlayerOut (i, baseNbr)

    Call PBP(player$(i) + " out @ " + baseName$(baseNbr))

    runnerOnBase(i) = 0
    b2Pitcher(i) = 0
    stealPhase(i) = 0
    player$(i) = ""

End Sub


'----------------------------------------
'          TrackOuts Subroutine
'----------------------------------------
'Record outs for a player
'nbrOuts(0) = number of outs for current team at bat
'nbrOuts(1) = number of outs that would have been recorded without errors (tracking earned runs)
Sub TrackOuts (earnedRunOut)

    nbrOuts(0) = nbrOuts(0) + 1

    If earnedRunOut = 1 Then
        ' Increment the "earned run outs" counter.
        nbrOuts(1) = nbrOuts(1) + 1
    End If

End Sub


'----------------------------------------
'          RecordRun Subroutine
'----------------------------------------
'This subroutine charges runs to both
'the batter and pitcher. It also will
'determine if the run is an RBI.
'Finally, it updates the team score and
'resets key variables
Sub RecordRun (homeSafe, playerIdx, PQ, S2%, currFielder, INFPOS%, wasError)

    If homeSafe = 0 Then
        Call PBP(player$(playerIdx) + " scores")
    End If
    homeSafe = 0

    'Increase pitcher's runs
    Call INCPITDEF(D, b2Pitcher(playerIdx), LG_P_R)

    If currPitcher(D) <> b2Pitcher(playerIdx) Then
        Call INCPITDEF(D, currPitcher(D), LG_P_RS)
    End If

    If gamePitcher(P) = 1 Then
        'Runs by starting pitcher
        teamPitStats(P, currPitcher(P), LG_P_R_Start) = teamPitStats(P, currPitcher(P), LG_P_R_Start) + 1
    End If

    'Increase runner's "Runs"
    If fldPos(P, runnerOnBase(playerIdx)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_R)
    Else
        Call INCBATOFF(P, lineupPlayer(P, runnerOnBase(playerIdx)), LG_B_R)
    End If

    If wasError = 1 Then

        If nbrOuts(0) = 2 Then
            PQ = 8
        Else
            Select Case currFielder

                Case Is <= 3:
                    'Pitcher/Catcher/First Base
                    If S2% <> EVENT_BUNT Or wasError = 1 And currFielder = 5 And S2% <> 23 Then
                        PQ = 8
                    End If

                Case 4, 6
                    'Second Base, Shortstop
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

        If wasError = 1 Then
            PQ = 8
        End If

    End If

    'Increase inning score and total sccore
    inningScore(P, INNING%) = inningScore(P, INNING%) + 1
    gameScore(P, 0) = gameScore(P, 0) + 1

    If gameScore(P, 0) - gameScore(D, 0) = 1 Then
        pitcherOfRecord(D) = b2Pitcher(playerIdx)
        pitcherOfRecord(P) = currPitcher(P)
        relieverOnRecordForSave(1) = -1
        relieverOnRecordForSave(0) = -1
    Else
        If gameScore(P, 0) - gameScore(D, 0) = 0 And relieverOnRecordForSave(D) <> -1 Then
            Call INCPITDEF(D, currPitcher(D), LG_P_BSv)
            relieverOnRecordForSave(D) = -1
        End If
    End If

    runnerOnBase(playerIdx) = 0
    b2Pitcher(playerIdx) = 0
    stealPhase(playerIdx) = 0
    player$(playerIdx) = ""

    If inningScore(P, INNING%) = 4 And d100% <= 25 Then
        Call PBP("They're tearin' up the peapatch!")
    End If

    'Bottom of the 9th or later, home team up by 1
    If INNING% >= 9 And P = 1 And gameScore(P, 0) - gameScore(D, 0) = 1 Then

        'Walk-off run
        If S2% <> EVENT_HR Then

            If wasError = 0 And nbrOuts(1) < 3 Then
                'Charge pitcher with game-ending run
                Call CHARGERUN(playerIdx)
            End If

            'Nobody scores or moves
            baseMove(0) = 0
            baseMove(1) = 0
            baseMove(2) = 0
            baseMove(3) = 0

        End If

    End If

End Sub


'----------------------------------------
'          DISPBAVG Subroutine
'----------------------------------------
'This routine displays the batting average
'on the field (traditional mode) for the
'active batter
Sub DISPBAVG ()

    If INNING% > 5 And pitcherStam(D, 0) < teamPitStats(D, currPitcher(D), 4) + teamPitStats(D, currPitcher(D), 1) And pitcherStam(D, 1) < teamPitStats(D, currPitcher(D), 2) Then
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
'This routine calculates modifiers
'that would apply for all pitching,
'hitting and ballpark changes.
'These modifiers are for batting average,
'strikeouts, base hits, lefty/righty and
'any home team advantages.
'Noteworthy variables / values
' * teamSplit(X, 0) = 999 only when all
'   Lefty/Right values sum up to > 0
Sub HITTINGLINES ()

    Randomize Timer

    'CLS
    'PRINT "Please Wait..."
    'PRINT
    'PRINT "Calculating Pitcher/Batter/Park Effects..."

    For team = 0 To 1

        opp = 1 - team

        ' Calculate Base on Balls Percentages
        Call CalcWalkPercentages(team, opp)

        ' Calculate Strikeout Percentages
        Call CalcStrikeoutPercentages(team, opp)

        ' Calculate Batting Average
        Call CalcBattingAverage(team, opp, i9!)

        ' Calculate Extra Base Hit Ratios
        Call CalcExtraBaseHits(team, opp, i9!)

        ' Adjust for Lefty/Righty matchups
        Call AdjustLeftyRighty(team, opp)

        ' Adjust averages for home team
        For I3 = VV(team) To VV1(team)

            If team = 1 Then
                'Home team bonus
                game_batRating(team, GM_BAvg, I3) = game_batRating(team, GM_BAvg, I3) + 5
            Else
                'Away team penalty
                game_batRating(team, GM_BAvg, I3) = game_batRating(team, GM_BAvg, I3) - 5
            End If

        Next I3

        ' Calculate home park factors
        If gameLoc <> 2 Then
            Call CalcParkFactors(team, opp)
        Else
            Exit For
        End If

    Next team

End Sub


'----------------------------------------
'     CalcWalkPercentages Subroutine
'----------------------------------------
Sub CalcWalkPercentages (team, opp)

    For I3 = VV(team) To VV1(team)

        leagWalkPct! = teamRat_GAME(opp, 3) / 1000

        ' Calculate pitcher walk rate
        If pitchRating(opp, currPitcher(opp), 6) * 3 * .94 + pitchRating(opp, currPitcher(opp), 7) + pitchRating(opp, currPitcher(opp), 8) = 0 Then
            I2! = 0
        Else
            I2! = pitchRating(opp, currPitcher(opp), 8) / (pitchRating(opp, currPitcher(opp), 6) * 3 * .94 + pitchRating(opp, currPitcher(opp), 7) + pitchRating(opp, currPitcher(opp), 8))
        End If

        Call GetMatchupType(team, opp, I3, TY%, PY%, BY%)

        ' Handle special team stats case
        If teamSplit(opp, 0) = 999 And pitchRating(opp, currPitcher(opp), 89) <> 0 Then
            leagWalkPct! = gameSA(opp, TY% * 6 + 3) / 1000
            I2! = pitchRating(opp, currPitcher(opp), 56 + PY% * 7) * .9 / (pitchRating(opp, currPitcher(opp), 51 + PY% * 7) + pitchRating(opp, currPitcher(opp), 56 + PY% * 7) * .9)
        End If

        ' Calculate batter walk rate
        If fldPos(team, I3) = 1 Then
            ' Pitcher vs Pitcher
            If pitchRating(team, currPitcher(team), SS_P_BAvg) <> 999 Then
                walkRate! = .037
            Else
                If (pitchRating(team, currPitcher(team), SS_P_H_BB) + pitchRating(team, currPitcher(team), SS_P_H_AB) = 0) Then
                    walkRate! = 0
                Else
                    If teamSplit(team, 0) = 999 Then
                        If pitchRating(team, currPitcher(team), 37 + BY% * 7) + pitchRating(team, currPitcher(team), 42 + BY% * 7) = 0 Then
                            walkRate! = 0
                        Else
                            walkRate! = pitchRating(team, currPitcher(team), 42 + BY% * 7) * .9 / (pitchRating(team, currPitcher(team), 42 + BY% * 7) * .9 + pitchRating(team, currPitcher(team), 37 + BY% * 7))
                        End If
                    Else
                        walkRate! = pitchRating(team, currPitcher(team), SS_P_H_BB) / (pitchRating(team, currPitcher(team), SS_P_H_BB) + pitchRating(team, currPitcher(team), SS_P_H_AB))
                    End If
                End If

            End If

        Else
            'Regular batter
            If batRating(team, lineupPlayer(team, I3), SS_B_AB) + batRating(team, lineupPlayer(team, I3), SS_B_W) = 0 Then
                walkRate! = 0
            Else

                If teamSplit(team, 0) = 999 Then
                    If batRating(team, lineupPlayer(team, I3), 47 + BY% * 7) + batRating(team, lineupPlayer(team, I3), 42 + BY% * 7) = 0 Then
                        walkRate! = 0
                    Else
                        If batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
                            walkRate! = batRating(team, lineupPlayer(team, I3), 47 + BY% * 7) * .9 / (batRating(team, lineupPlayer(team, I3), 42 + BY% * 7) + batRating(team, lineupPlayer(team, I3), 47 + BY% * 7) * .9)
                        End If
                    End If
                Else
                    walkRate! = batRating(team, lineupPlayer(team, I3), SS_B_W) / (batRating(team, lineupPlayer(team, I3), SS_B_AB) + batRating(team, lineupPlayer(team, I3), SS_B_W))
                End If

            End If

        End If

        'Normalize batter walk rate
        Call NORMALIZE(walkRate!, I2!, leagWalkPct!, K8!)
        game_batRating(team, GM_BB, I3) = K8!

    Next I3

End Sub


'----------------------------------------
'   CalcStrikeoutPercentages Subroutine
'----------------------------------------
Sub CalcStrikeoutPercentages (team, opp)

    For I3 = VV(team) To VV1(team)

        leagSOPct! = teamRat_GAME(opp, 2) / 1000

        ' Calculate pitcher strikeout rate
        If pitchRating(opp, currPitcher(opp), 6) = 0 Then
            I2! = 0
        Else
            I2! = pitchRating(opp, currPitcher(opp), SS_P_K) / ((pitchRating(opp, currPitcher(opp), SS_P_IP) * 3) * .94)
        End If

        Call GetMatchupType(team, opp, I3, TY%, PY%, BY%)

        ' Handle special team stats case
        If teamSplit(opp, 0) = 999 And pitchRating(opp, currPitcher(opp), 89) <> 0 Then

            If pitchRating(opp, currPitcher(opp), 51 + PY% * 7) - pitchRating(opp, currPitcher(opp), 52 + PY% * 7) = 0 Then
                I2! = 0
            Else
                I2! = pitchRating(opp, currPitcher(opp), 57 + PY% * 7) / (pitchRating(opp, currPitcher(opp), 51 + PY% * 7) - pitchRating(opp, currPitcher(opp), 52 + PY% * 7))
                leagSOPct! = gameSA(opp, TY% * 6 + 2) / 1000
            End If

        End If

        ' Calculate batter strikeout rate
        If fldPos(team, I3) = 1 Then

            ' Pitcher vs Pitcher
            If pitchRating(team, currPitcher(team), SS_P_BAvg) <> 999 Then
                I7! = .42
            Else

                If pitchRating(team, currPitcher(team), SS_P_H_AB) - pitchRating(team, currPitcher(team), SS_P_H_H) = 0 Then
                    I7! = 0
                Else

                    If teamSplit(team, 0) = 999 Then
                        If pitchRating(team, currPitcher(team), 37 + BY% * 7) - pitchRating(team, currPitcher(team), 38 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            I7! = pitchRating(team, currPitcher(team), 43 + BY% * 7) / (pitchRating(team, currPitcher(team), 37 + BY% * 7) - pitchRating(team, currPitcher(team), 38 + BY% * 7))
                        End If
                    Else
                        If fldPos(team, I3) = 1 Then
                            I7! = pitchRating(team, currPitcher(team), SS_P_H_SO) / (pitchRating(team, currPitcher(team), SS_P_H_AB) - pitchRating(team, currPitcher(team), SS_P_H_H))
                        End If
                    End If

                End If

            End If

        Else

            ' Regular batter
            If batRating(team, lineupPlayer(team, I3), SS_B_AB) = 0 Or batRating(team, lineupPlayer(team, I3), SS_B_AB) - batRating(team, lineupPlayer(team, I3), SS_B_H) = 0 Then
                I7! = 0
            Else

                If teamSplit(team, 0) = 999 Then

                    If batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
                        If batRating(team, lineupPlayer(team, I3), 42 + BY% * 7) - batRating(team, lineupPlayer(team, I3), 43 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            I7! = batRating(team, lineupPlayer(team, I3), 48 + BY% * 7) / (batRating(team, lineupPlayer(team, I3), 42 + BY% * 7) - batRating(team, lineupPlayer(team, I3), 43 + BY% * 7))
                        End If
                    End If
                Else
                    I7! = batRating(team, lineupPlayer(team, I3), SS_B_K) / (batRating(team, lineupPlayer(team, I3), SS_B_AB) - batRating(team, lineupPlayer(team, I3), SS_B_H))
                End If

            End If

        End If

        'Normalize batter strikeout
        Call NORMALIZE(I7!, I2!, leagSOPct!, K8!)
        game_batRating(team, GM_K, I3) = K8!

    Next I3

End Sub


'----------------------------------------
'      CalcBattingAverage Subroutine
'----------------------------------------
Sub CalcBattingAverage (team, opp, i9!)

    For I3 = VV(team) To VV1(team)

        'league batting average (opponent)
        I2! = teamRat_GAME(opp, 1) / 1000

        'league batting average (opp pitcher)
        I6! = pitchRating(opp, currPitcher(opp), SS_P_LgBAvg) / 1000

        ' Calculate pitcher batting average against
        If pitchRating(opp, currPitcher(opp), SS_P_IP) + pitchRating(opp, currPitcher(opp), SS_P_H) = 0 Then
            I7! = 0
        Else
            'Hits per Batters Faced (BAVG Against)
            'H / ((IP * 3 * 0.94) + H)
            'The 0.94 is a factor that may need more assessment
            I7! = pitchRating(opp, currPitcher(opp), SS_P_H) / (((pitchRating(opp, currPitcher(opp), SS_P_IP) * 3) * .94) + pitchRating(opp, currPitcher(opp), SS_P_H))
        End If

        'Normalize Hits/Batters to opponent batting average
        Call NORMALIZE(I7!, I2!, I6!, K8!)

        'Opposing pitcher's effectiveness
        i9! = K8! / 1000

        Call GetMatchupType(team, opp, I3, TY%, PY%, BY%)

        ' Handle special team stats case
        If teamSplit(opp, 0) = 999 And pitchRating(opp, currPitcher(opp), 89) <> 0 Then

            'BAVG (opp) based on L/R matchups (LLBA / LRBA / RLBA / RRBA)
            I6! = pitchRating(opp, currPitcher(opp), TY% * 6 + 65) / 1000

            'If H (based on L/R matchup) are 0
            If pitchRating(opp, currPitcher(opp), 51 + PY% * 7) = 0 Then
                'assign rating of 0
                I7! = 0
            Else
                'Assign BA (opp) based on L/R matchups
                I7! = pitchRating(opp, currPitcher(opp), 52 + PY% * 7) / pitchRating(opp, currPitcher(opp), 51 + PY% * 7)
            End If

            I2! = gameSA(opp, TY% * 6 + 1) / 1000

            'Normalize league BAVG to opponent
            Call NORMALIZE(I7!, I2!, I6!, K8!)
            i9! = K8! / 1000

        End If

        'Get batter's average
        If fldPos(team, I3) = 1 Then
            'Pitcher vs Pitcher
            If pitchRating(team, currPitcher(team), SS_P_BAvg) <> 999 Then
                batAvg! = pitchRating(team, currPitcher(team), SS_P_BAvg) / 1000
            Else
                If pitchRating(team, currPitcher(team), SS_P_H_AB) = 0 Then
                    batAvg! = 0
                Else
                    If teamSplit(team, 0) = 999 Then
                        If pitchRating(team, currPitcher(team), 37 + BY% * 7) = 0 Then
                            batAvg! = 0
                        Else
                            batAvg! = pitchRating(team, currPitcher(team), 38 + BY% * 7) / pitchRating(team, currPitcher(team), 37 + BY% * 7)
                        End If
                    Else
                        batAvg! = pitchRating(team, currPitcher(team), SS_P_H_H) / pitchRating(team, currPitcher(team), SS_P_H_AB)
                    End If
                End If

            End If

        Else

            'Regular batter
            If batRating(team, lineupPlayer(team, I3), SS_B_AB) = 0 Then
                batAvg! = 0
            Else
                If teamSplit(team, 0) = 999 And batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
                    If batRating(team, lineupPlayer(team, I3), 42 + BY% * 7) = 0 Then
                        batAvg! = 0
                    Else
                        batAvg! = batRating(team, lineupPlayer(team, I3), 43 + BY% * 7) / batRating(team, lineupPlayer(team, I3), 42 + BY% * 7)
                    End If
                Else
                    batAvg! = batRating(team, lineupPlayer(team, I3), SS_B_H) / batRating(team, lineupPlayer(team, I3), SS_B_AB)
                End If

            End If

        End If

        'Get average against league
        If fldPos(team, I3) = 1 Then
            'Pitcher vs Pitcher
            If teamSplit(team, 0) = 999 Then
                oppAvg! = pitchRating(team, currPitcher(team), TY% * 6 + 65) / 1000
            Else
                oppAvg! = pitchRating(team, currPitcher(team), SS_P_LgBAvg) / 1000
            End If

        Else
            'Regular batter
            If teamSplit(team, 0) = 999 And batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
                oppAvg! = batRating(team, lineupPlayer(team, I3), 56 + TY% * 6) / 1000
            Else
                oppAvg! = batRating(team, lineupPlayer(team, I3), SS_B_LgBAvg) / 1000
            End If

        End If

        'Team batting average
        If teamSplit(team, 0) = 999 And batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
            I2! = gameSA(team, TY% * 6 + 1) / 1000
        Else
            I2! = teamRat_GAME(team, 1) / 1000
        End If

        'Normalize raw batting average to team batting average
        Call NORMALIZE(batAvg!, I2!, oppAvg!, K8!)
        batAvg! = K8! / 1000

        If teamSplit(team, 0) = 999 And batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
            oppAvg! = gameSA(team, TY% * 6 + 1) / 1000
        Else
            oppAvg! = teamRat_GAME(team, 1) / 1000
        End If

        I2! = i9!

        'Normalize batting average to opposing pitcher's effectiveness
        Call NORMALIZE(batAvg!, I2!, oppAvg!, K8!)
        game_batRating(team, GM_BAvg, I3) = K8!

    Next I3

End Sub


'----------------------------------------
'      CalcExtraBaseHits Subroutine
'----------------------------------------
Sub CalcExtraBaseHits (team, opp, i9!)

    For I3 = VV(team) To VV1(team)

        Call GetMatchupType(team, opp, I3, TY%, PY%, BY%)

        ' Handle special pitcher case
        If fldPos(team, I3) = 1 And pitchRating(team, currPitcher(team), SS_P_BAvg) <> 999 Then

            game_batRating(team, GM_2B, I3) = (130 * (i9! * 1000)) / teamRat_GAME(opp, 1)
            game_batRating(team, GM_3B, I3) = (19 * (i9! * 1000)) / teamRat_GAME(opp, 1)
            game_batRating(team, GM_HR, I3) = 37

        Else

            ' Calculate base multiplier
            If teamSplit(opp, 0) = 999 Then
                E2! = (i9! * 1000) / gameSA(opp, TY% * 6 + 1)
            Else
                E2! = (i9! * 1000) / teamRat_GAME(opp, 1)
            End If

            If fldPos(team, I3) = 1 Then

                ' Pitcher batting
                If pitchRating(team, currPitcher(team), SS_P_H_H) = 0 Then
                    ' Zero out all ratings if no hits
                    game_batRating(team, GM_2B, I3) = 0
                    game_batRating(team, GM_3B, I3) = 0
                    game_batRating(team, GM_HR, I3) = 0

                ElseIf teamSplit(team, 0) = 999 Then
                    ' Special team split handling
                    If pitchRating(team, currPitcher(team), 38 + BY% * 7) = 0 Then
                        ' Zero out if special rating is 0
                        game_batRating(team, GM_2B, I3) = 0
                        game_batRating(team, GM_3B, I3) = 0
                        game_batRating(team, GM_HR, I3) = 0
                    Else
                        ' Calculate using special ratings
                        game_batRating(team, GM_2B, I3) = Int((pitchRating(team, currPitcher(team), 39 + BY% * 7) / pitchRating(team, currPitcher(team), 38 + BY% * 7)) * 1000) * E2!
                        game_batRating(team, GM_3B, I3) = Int((pitchRating(team, currPitcher(team), 40 + BY% * 7) / pitchRating(team, currPitcher(team), 38 + BY% * 7)) * 1000) * E2!
                        game_batRating(team, GM_HR, I3) = Int((pitchRating(team, currPitcher(team), 41 + BY% * 7) / pitchRating(team, currPitcher(team), 38 + BY% * 7)) * 1000)
                    End If
                Else
                    ' Normal pitcher batting calculation
                    game_batRating(team, GM_2B, I3) = Int((pitchRating(team, currPitcher(team), SS_P_H_2B) / pitchRating(team, currPitcher(team), SS_P_H_H)) * 1000) * E2!
                    game_batRating(team, GM_3B, I3) = Int((pitchRating(team, currPitcher(team), SS_P_H_3B) / pitchRating(team, currPitcher(team), SS_P_H_H)) * 1000) * E2!
                    game_batRating(team, GM_HR, I3) = Int((pitchRating(team, currPitcher(team), SS_P_H_HR) / pitchRating(team, currPitcher(team), SS_P_H_H)) * 1000)
                End If

            Else

                ' Non-pitcher batting
                If batRating(team, lineupPlayer(team, I3), SS_B_H) = 0 Then
                    game_batRating(team, GM_2B, I3) = 0
                    game_batRating(team, GM_3B, I3) = 0
                    game_batRating(team, GM_HR, I3) = 0
                ElseIf teamSplit(team, 0) = 999 And batRating(team, lineupPlayer(team, I3), 80) <> 0 Then
                    'Special team split with valid rating
                    If batRating(team, lineupPlayer(team, I3), 43 + BY% * 7) = 0 Then
                        'Zero out if base rating is 0
                        game_batRating(team, GM_2B, I3) = 0
                        game_batRating(team, GM_3B, I3) = 0
                        game_batRating(team, GM_HR, I3) = 0
                    Else
                        'Calculate using special split ratings
                        '-- see note on pitchers * .95
                        game_batRating(team, GM_2B, I3) = Int((batRating(team, lineupPlayer(team, I3), 44 + BY% * 7) / batRating(team, lineupPlayer(team, I3), 43 + BY% * 7)) * 1000) * E2!
                        game_batRating(team, GM_3B, I3) = Int((batRating(team, lineupPlayer(team, I3), 45 + BY% * 7) / batRating(team, lineupPlayer(team, I3), 43 + BY% * 7)) * 1000) * E2!
                        game_batRating(team, GM_HR, I3) = Int((batRating(team, lineupPlayer(team, I3), 46 + BY% * 7) / batRating(team, lineupPlayer(team, I3), 43 + BY% * 7)) * 1000)
                    End If
                Else
                    'Normal batting calculation
                    '-- see note on pitchers * .95
                    game_batRating(team, GM_2B, I3) = Int((batRating(team, lineupPlayer(team, I3), SS_B_2B) / batRating(team, lineupPlayer(team, I3), SS_B_H)) * 1000) * E2!
                    game_batRating(team, GM_3B, I3) = Int((batRating(team, lineupPlayer(team, I3), SS_B_3B) / batRating(team, lineupPlayer(team, I3), SS_B_H)) * 1000) * E2!
                    game_batRating(team, GM_HR, I3) = Int((batRating(team, lineupPlayer(team, I3), SS_B_HR) / batRating(team, lineupPlayer(team, I3), SS_B_H)) * 1000)

                End If
            End If
        End If

        ' Normalize home run percentage
        leagHRPct! = teamRat_GAME(opp, 4) / 1000
        hrRate! = game_batRating(team, GM_HR, I3) / 1000
        I2! = 0

        If pitchRating(opp, currPitcher(opp), SS_P_H) <> 0 Then
            I2! = pitchRating(opp, currPitcher(opp), SS_P_HR) / pitchRating(opp, currPitcher(opp), SS_P_H)
        End If

        If teamSplit(opp, 0) = 999 And pitchRating(opp, currPitcher(opp), 89) <> 0 Then

            leagHRPct! = gameSA(opp, TY% * 6 + 4) / 1000

            If pitchRating(opp, currPitcher(opp), 52 + PY% * 7) <> 0 Then
                I2! = pitchRating(opp, currPitcher(opp), 55 + PY% * 7) / pitchRating(opp, currPitcher(opp), 52 + PY% * 7)
            End If

        End If

        'Normalize base hit ratings
        Call NORMALIZE(hrRate!, I2!, leagHRPct!, K8!)
        game_batRating(team, GM_HR, I3) = K8!

    Next I3

End Sub


'----------------------------------------
'      AdjustLeftyRighty Subroutine
'----------------------------------------
Sub AdjustLeftyRighty (team, opp)

    oppPitcherHand = pitchRating(opp, currPitcher(opp), SS_P_Throws)
    teamPitcherHand = pitchRating(team, currPitcher(team), SS_P_Throws)

    For I3 = VV(team) To VV1(team)

        If teamSplit(team, 0) <> 999 Or batRating(team, lineupPlayer(team, I3), 80) = 0 Then
            'Default to no adjustment
            A! = 1
            A1! = 1

            If fldPos(team, I3) = 1 Then

                If teamPitcherHand = -1 Then

                    If oppPitcherHand = 1 Then
                        A! = 1.04
                        A1! = .88
                    End If
                    If oppPitcherHand = -1 Then
                        A! = .93
                        A1! = 1.16
                    End If

                End If

                If teamPitcherHand = 1 Then

                    If oppPitcherHand = -1 Then
                        A! = 1.02
                        A1! = .98
                    End If
                    If oppPitcherHand = 1 Then
                        A! = .97
                        A1! = 1.1
                    End If

                End If

            Else

                ' Regular batter - use batter handedness
                batterHand = batRating(team, lineupPlayer(team, I3), SS_B_Bats)

                Select Case batterHand

                    Case -2
                        If oppPitcherHand = 1 Then
                            A! = 1.04
                            A1! = .96
                        End If
                        If oppPitcherHand = -1 Then
                            A! = .78
                            A1! = 1.22
                        End If

                    Case -1
                        If oppPitcherHand = 1 Then
                            A! = 1.04
                            A1! = .88
                        End If
                        If oppPitcherHand = -1 Then
                            A! = .93
                            A1! = 1.16
                        End If

                    Case 0
                        If oppPitcherHand = 1 Then
                            A! = .99
                            A1! = .93
                        End If
                        If oppPitcherHand = -1 Then
                            A! = 1.01
                            A1! = .86
                        End If

                    Case 1
                        If oppPitcherHand = 1 Then
                            A! = .97
                            A1! = 1.1
                        End If
                        If oppPitcherHand = -1 Then
                            A! = 1.02
                            A1! = .98
                        End If

                    Case 2
                        If oppPitcherHand = 1 Then
                            A! = .78
                            A1! = 1.22
                        End If
                        If oppPitcherHand = -1 Then
                            A! = 1.04
                            A1! = .96
                        End If

                End Select

            End If

            Call LRADJ(I3, A!, A1!)

        End If

        ' Copy home run values
        game_batRating(team, GM_L_HR, I3) = game_batRating(team, GM_HR, I3)
        game_batRating(team, GM_R_HR, I3) = game_batRating(team, GM_HR, I3)

    Next I3

End Sub


'----------------------------------------
'       CalcParkFactors Subroutine
'----------------------------------------
Sub CalcParkFactors (team, opp)

    For I2 = VV(team) To VV1(team)

        ' Apply park factors for doubles and triples
        If team = 1 Then
            ' Home team
            game_batRating(team, GM_2B, I2) = CInt(game_batRating(team, GM_2B, I2) * (teamRat_GAME(team, 6) / 100))
            game_batRating(team, GM_3B, I2) = CInt(game_batRating(team, GM_3B, I2) * (teamRat_GAME(team, 7) / 100))
        Else
            ' Away team
            game_batRating(team, GM_2B, I2) = CInt(game_batRating(team, GM_2B, I2) * (teamRat_GAME(opp, 6) / 100) * ((200 - teamRat_GAME(team, 6)) / 100))
            game_batRating(team, GM_3B, I2) = CInt(game_batRating(team, GM_3B, I2) * (teamRat_GAME(opp, 7) / 100) * ((200 - teamRat_GAME(team, 7)) / 100))
        End If

        ' Calculate wind factors
        If parkHRVals!(team) > 1000 Then
            I3! = 100
            I6! = Int(parkHRVals!(team) / 1000) * windImpact!
            I7! = (parkHRVals!(team) - Int((parkHRVals!(team) / 1000)) * 1000) * windImpact!
        End If

        If parkHRVals!(team) < 1000 Then
            I3! = parkHRVals!(team) * windImpact!
            I6! = I3!
            I7! = I3!
        End If

        If parkHRVals!(opp) > 1000 Then
            I4! = 100
            K8! = Int(parkHRVals!(opp) / 1000) * windImpact!
            i9! = (parkHRVals!(opp) - Int((parkHRVals!(opp) / 1000)) * 1000) * windImpact!
        End If

        If parkHRVals!(opp) < 1000 Then
            I4! = parkHRVals!(opp) * windImpact!
            K8! = I4!
            i9! = I4!
        End If

        ' Apply wind factors to home runs
        If team = 1 Then
            ' Home team
            game_batRating(team, GM_HR, I2) = CInt((game_batRating(team, GM_HR, I2) * (I3! / 100)))
            game_batRating(team, GM_L_HR, I2) = CInt((game_batRating(team, GM_L_HR, I2) * (I6! / 100)))
            game_batRating(team, GM_R_HR, I2) = CInt((game_batRating(team, GM_R_HR, I2) * (I7! / 100)))
        Else
            ' Away team
            game_batRating(team, GM_HR, I2) = CInt((game_batRating(team, GM_HR, I2) * (I4! / 100) * ((200 - I3!) / 100)))
            game_batRating(team, GM_L_HR, I2) = CInt((game_batRating(team, GM_L_HR, I2) * (K8! / 100) * ((200 - I6!) / 100)))
            game_batRating(team, GM_R_HR, I2) = CInt((game_batRating(team, GM_R_HR, I2) * (i9! / 100) * ((200 - I7!) / 100)))
        End If

    Next I2

End Sub


'----------------------------------------
'       GetMatchupType Subroutine
'----------------------------------------
'matchupTyp: pitcher vs. batter
'   0 = LL
'   1 = LR
'   2 = RL
'   3 = RR
'pitcherHand: pitcher left-handed (0) or right-handed (1)
'batterHand: batter left-handed (0) or right-handed (1)
Sub GetMatchupType (I, I1, playerIdx, matchupType, pitcherHand, batterHand)

    'batter side
    I0 = batRating(I, lineupPlayer(I, playerIdx), SS_B_Bats)

    'pitcher side
    I4 = pitchRating(I1, currPitcher(I1), SS_P_Throws)

    'Determine / adjust matchupType
    If fldPos(I, playerIdx) = 1 Then

        If pitchRating(I, currPitcher(I), SS_P_Throws) = -1 Then

            If I4 = 1 Then
                matchupType = 1
            End If
            If I4 = -1 Then
                matchupType = 0
            End If

        End If

        If pitchRating(I, currPitcher(I), SS_P_Throws) = 1 Then

            If I4 = 1 Then
                matchupType = 3
            End If
            If I4 = -1 Then
                matchupType = 2
            End If

        End If

    Else

        If I0 < 0 Then

            If I4 = 1 Then
                matchupType = 1
            End If
            If I4 = -1 Then
                matchupType = 0
            End If

        End If

        If I0 = 0 Then

            If I4 = 1 Then
                matchupType = 1
            End If
            If I4 = -1 Then
                matchupType = 2
            End If

        End If

        If I0 > 0 Then

            If I4 = 1 Then
                matchupType = 3
            End If
            If I4 = -1 Then
                matchupType = 2
            End If

        End If

    End If

    'Determine pitcher handedness
    If matchupType = 0 Or matchupType = 1 Then
        pitcherHand = 0
    End If
    If matchupType = 2 Or matchupType = 3 Then
        pitcherHand = 1
    End If

    'Determine batter handedness
    If matchupType = 0 Or matchupType = 2 Then
        batterHand = 0
    End If
    If matchupType = 1 Or matchupType = 3 Then
        batterHand = 1
    End If

End Sub


'----------------------------------------
'            LRADJ Subroutine
'----------------------------------------
'Adjust L/R ratings based on assigned weights
Sub LRADJ (playerIdx, A!, A1!)

    For I6 = 0 To 3
        game_batRating(I, I6, playerIdx) = game_batRating(I, I6, playerIdx) * A!
    Next I6

    game_batRating(I, GM_BB, playerIdx) = game_batRating(I, GM_BB, playerIdx) * A!
    game_batRating(I, GM_K, playerIdx) = game_batRating(I, GM_K, playerIdx) * A1!

End Sub


'----------------------------------------
'          NORMALIZE Subroutine
'----------------------------------------
' Normalize a player skill rating into
'a value from 1-1000
Sub NORMALIZE (baseChance!, situationFactor!, balWeight!, normRating!)

    I0! = (baseChance! * situationFactor! * (1 - balWeight!))

    If (I0! + ((1 - baseChance!) * (1 - situationFactor!) * balWeight!)) = 0 Then
        normRating! = 0
    Else
        normRating! = I0! / (I0! + ((1 - baseChance!) * (1 - situationFactor!) * balWeight!))
    End If

    normRating! = CInt(normRating! * 1000)

End Sub


'----------------------------------------
'         BASEONBALLS Subroutine
'----------------------------------------
'This subroutine manages the stats and the
'Play-by-Play for a Base-on-Balls
Sub BASEONBALLS (S2%, D2, baseStatus)

    S2% = EVENT_INTERFERENCE

    Call WINDEX

    If D2 = DEF_INTNTL_BB Then

        If d100% <= 50 Then
            Call PBP(player$(0) + " is walked intentionally...")
        Else
            Call PBP(player$(0) + " is given a purpose pass...")
        End If

        pitcherStam(D, 0) = pitcherStam(D, 0) + 1

    Else

        If d100% <= 30 Then Call PBP("Full count...")

        Call DELIVERY

        Call GetPBPString(80, 780, fileString$)
        Call PBP(fileString$)

        Call GetPBPString(30, 860, fileString$)
        Call PBP(fileString$)

    End If

    If baseStatus = 7 And d100% <= 30 Then
        Call PBP("that forces a run in")
    End If

    RN0 = Int(Rnd(1) * 4) + 1

    If baseStatus > 3 And baseStatus < 7 Then

        Select Case RN0

            Case 1:
                Call PBP("and the bases are loaded!")

            Case 2:
                Call PBP("and the sacks are jammed!")

            Case 3:
                Call PBP("and the bases are full!")

            Case 4:
                Call PBP("and that loads them up!")

        End Select

    End If

    Call INCPITDEF(D, currPitcher(D), LG_P_BB)

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
Sub HITBYPITCH (S2%, baseStatus)

    S2% = EVENT_HBP

    Call WINDEX
    Call DELIVERY

    Call GetPBPString(20, 720, fileString$)
    Call PBP(fileString$)

    Call GetPBPString(11, 740, fileString$)
    Call PBP(fileString$)

    Call GetPBPString(20, 760, fileString$)
    Call PBP(fileString$)

    If d100% <= 50 And baseStatus = 7 Then
        Call PBP("tough way to get an RBI")
    End If

End Sub

'----------------------------------------
'          WILDPITCH Subroutine
'----------------------------------------
'This subroutine handles PBP for a wild
'pitch and ultimately determines if the
'pitch should be ruled as such.
Sub WILDPITCH (S2%)

    S2% = 6
    ruleAsWild = 0

    Call Rolld100(pitchResult)

    Select Case pitchResult

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

        Case 81 To 100:
            Call PBP("wild pitch by " + pitchers$(D, currPitcher(D)))

    End Select

    If ruleAsWild = 1 Then

        Call Rolld100(RN0)

        Select Case RN0

            Case 1 To 33:
                pbpString$ = "It's ruled a wild pitch"

            Case 34 To 66:
                pbpString$ = "score it a wild pitch"

            Case 67 To 100:
                pbpString$ = "credit " + pitchers$(D, currPitcher(D)) + " with a wild pitch"

        End Select

        Call PBP(pbpString$)

    End If

End Sub


'----------------------------------------
'          RELIEVERS Subroutine
'----------------------------------------
'This routine determines if the pitching
'team needs a reliever, and handles the
'substitution if so. This is only for
'the computer team.
Sub RELIEVERS (pinchHitterFlag, newPitcherFlag, baseStatus)

    Call PITSTRAT(pinchHitterFlag, newPitcherFlag, baseStatus)

    'If using newer substitution rules...
    If batterMinimumOpt = 1 Then
        'Mark current pitcher as no longer available
        pitchRating(D, currPitcher(D), SS_P_Avail) = 1
    End If

    'Reset count of batters a reliever needs to face
    If pinchHitterFlag = 1 Then
        'Print #11, "PINCH HIT CALLED"
    End If

    battersFaced(D, currPitcher(D)) = 0

    If newPitcherFlag = 1 Then

        newPitcherFlag = 0

    Else

        pitchWeight = 0
        scoreDiff = gameScore(D, 0) - gameScore(P, 0)

        'Build selection pool
        For rIdx = 0 To 21

            'Calculate effective rating based on expected games
            pitcherVal = pitchRating(D, rIdx, SS_P_G) - pitchRating(D, rIdx, SS_P_GS) - pitchRating(D, rIdx, SS_P_Sv) * 1.3

            'Only use pitchers who are available / valid
            If pitchRating(D, rIdx, SS_P_Avail) <= 0 And pitchers$(D, rIdx) <> "XXX" And pitcherVal <> 0 Then

                'Early / Mid-Game Decision (innings 1-7)
                If INNING% <= 7 And pitcherVal > 0 Then
                    'middle relief usage
                    pitchWeight = pitchWeight + pitcherVal
                End If

                ' Late Game Decision (innings 8+)
                If INNING% > 7 Then
                    If scoreDiff < 0 And pitcherVal > 0 Then
                        'If we're losing, use setup men
                        pitchWeight = pitchWeight + pitcherVal
                    End If
                    If scoreDiff >= 0 Then
                        'If we're winning / tied, use closers
                        pitchWeight = pitchWeight + pitchRating(D, rIdx, SS_P_Sv)
                    End If
                End If

            End If

        Next rIdx

        'Random # within weighted pool
        randPitcher = Int(Rnd(1) * pitchWeight) + 1

        pitchWeight = 0

        For rIdx = 0 To 21

            pitcherVal = pitchRating(D, rIdx, 4) - pitchRating(D, rIdx, 5) - pitchRating(D, rIdx, 3) * 1.3

            'Only use pitchers who are available / valid
            If pitchRating(D, rIdx, SS_P_Avail) <= 0 And pitchers$(D, rIdx) <> "XXX" And pitcherVal <> 0 Then

                'Early / Mid-Game Decision (innings 1-7)
                If INNING% <= 7 Then

                    If pitcherVal > 0 Then
                        If randPitcher <= pitchWeight + pitcherVal Then
                            ' Bring in middle relief
                            Call NEWPITCHER(rIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)
                            Exit Sub
                        Else
                            pitchWeight = pitchWeight + pitcherVal
                        End If
                    End If

                Else

                    ' Late Game Decision (innings 8+)
                    If scoreDiff < 0 And pitcherVal > 0 Then
                        If randPitcher <= pitchWeight + pitcherVal Then
                            'Bring in setup man
                            Call NEWPITCHER(rIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)
                            Exit Sub
                        Else
                            pitchWeight = pitchWeight + pitcherVal
                        End If
                    Else
                        If scoreDiff >= 0 And randPitcher > pitchWeight Then
                            If randPitcher <= pitchWeight + pitchRating(D, rIdx, 3) Then
                                'Bring in closer
                                Call NEWPITCHER(rIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)
                                Exit Sub
                            Else
                                pitchWeight = pitchWeight + pitchRating(D, rIdx, 3)
                            End If
                        End If
                    End If

                End If

            End If

        Next rIdx

        ' FALLBACK: If no pitcher was selected through weighted system
        For rIdx = 0 To 21
            ' Find any pitcher who is available
            If pitchRating(D, rIdx, SS_P_Avail) = 0 And pitchers$(D, rIdx) <> "XXX" Then
                Call NEWPITCHER(rIdx, pinchHitterFlag, newPitcherFlag, baseStatus, INFPOS%)
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
            batRating(p_D, p_newPlayer, SS_B_Arm) = batRating(p_D, p_newPlayer, 33 + (xx - 22) * 2)
            batRating(p_D, p_newPlayer, SS_B_Range) = batRating(p_D, p_newPlayer, 34 + (xx - 22) * 2)
            batRating(p_D, p_newPlayer, SS_B_FldPct) = batRating(p_D, p_newPlayer, xx + 4)
            Exit Sub
        End If

    Next

    'New player can't play the position -- assign default (poor) values
    Select Case fldPos(p_D, p_origPlayer)

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

    batRating(p_D, p_newPlayer, SS_B_Arm) = jArm
    batRating(p_D, p_newPlayer, SS_B_Range) = jRange
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
Sub SECOND2FIRST ()

    Call GetPBPString(30, 1540, fileString$)
    Call PBP(fileString$ + " to first")

End Sub


'----------------------------------------
'          SINGLEPBP Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for a single hit.
Sub SINGLEPBP (S2%, currFielder)

    If S2% = -EVENT_INF_SINGLE Then

        S2% = EVENT_INF_SINGLE

    Else

        Call DELIVERY

        If S2% = EVENT_INF_SINGLE Then
            Call PBP(player$(0) + " beats out an infield single")
        Else

            If d100% <= 50 Then
                Call GetPBPString(50, 1610, fileString$)
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
                Call GetPBPString(range, min, fileString$)
                Call PBP(fileString$)
            End If

        End If 'Done checking for S2%

        Call DELAY

    End If 'Done checking for S2%

End Sub


'----------------------------------------
'         Hit_Single Subroutine
'----------------------------------------
'This subroutine handles the stats, PBP,
'advancements and calculation for a single.
Sub Hit_Single (wasError, currFielder, I3, INFPOS%, I1, S2%, P2)

    Call GETOUTFIELDER(currFielder, I1)
    Call WINDEX

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
    End If

    Call INCPITDEF(D, currPitcher(D), LG_P_H)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
    End If

    Call SINGLEPBP(S2%, currFielder)

    'Default to running 1 base
    For I = 0 To 3
        baseMove(I) = 1
    Next I

    speedFactor = 3

    eraModifier! = 1.0

    'Not infield single, 2nd base occupied
    If S2% <> EVENT_INF_SINGLE And runnerOnBase(2) <> 0 Then

        Call Rolld100(chanceRoll)

        If fldPos(P, runnerOnBase(2)) <> 1 Then
            batRate = batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run)
        Else
            batRate = pitchRating(P, currPitcher(P), SS_P_Run)
        End If

        baseChance = 0

        If nbrOuts(0) = 0 Then
            If currFielder = 7 Then
                'chance vs LCF
                baseChance = 17
            End If
            If currFielder = 8 Then
                'chance vs CF
                baseChance = 50
            End If
            If currFielder = 9 Then
                'chance vs RCF
                baseChance = 28
            End If
        End If

        If nbrOuts(0) = 1 Then
            If currFielder = 7 Then
                'chance vs LCF
                baseChance = 37
            End If
            If currFielder = 8 Then
                'chance vs CF
                baseChance = 62
            End If
            If currFielder = 9 Then
                'chance vs RCF
                baseChance = 45
            End If
        End If

        If nbrOuts(0) = 2 Then
            If currFielder = 7 Then
                'chance vs LCF
                baseChance = 80
            End If
            If currFielder = 8 Then
                'chance vs CF
                baseChance = 83
            End If
            If currFielder = 9 Then
                'chance vs RCF
                baseChance = 80
            End If
        End If

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If

        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -10
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 10

        End Select

        advanceChance = (baseChance * eraModifier!) + (batRate * speedFactor) + runStratBonus

        If chanceRoll <= advanceChance Or P2 = OFF_HIT_N_RUN Then
            '2nd base runner tries to score
            baseMove(2) = 2
        End If

    End If

    'Not infield single, 1st base occupied, 2nd base empty or not moving
    If S2% <> EVENT_INF_SINGLE And runnerOnBase(1) <> 0 And (runnerOnBase(2) = 0 Or baseMove(2) <> 1) Then

        Call Rolld100(chanceRoll)

        If fldPos(P, runnerOnBase(1)) = 1 Then
            batRate = pitchRating(P, currPitcher(P), SS_P_Run)
        Else
            batRate = batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run)
        End If

        baseChance = 0

        If nbrOuts(0) = 0 Then
            If currFielder = 7 Then
                baseChance = 7
            End If
            If currFielder = 8 Then
                baseChance = 27
            End If
            If currFielder = 9 Then
                baseChance = 42
            End If
        End If

        If nbrOuts(0) = 1 Then
            If currFielder = 7 Then
                baseChance = 11
            End If
            If currFielder = 8 Then
                baseChance = 29
            End If
            If currFielder = 9 Then
                baseChance = 44
            End If
        End If

        If nbrOuts(0) = 2 Then
            If currFielder = 7 Then
                baseChance = 12
            End If
            If currFielder = 8 Then
                baseChance = 49
            End If
            If currFielder = 9 Then
                baseChance = 68
            End If
        End If

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If

        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -10
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 10

        End Select

        If chanceRoll <= baseChance + runStratBonus Or P2 = OFF_HIT_N_RUN Then
            '1st base runner tries to advance to 3rd
            baseMove(1) = 2
        End If

    End If

    If runnerOnBase(1) = 1 Then

        If baseMove(1) = 1 Then
            'Print #11, "Runner on 1st advance to 2nd"
        End If

        If baseMove(1) = 2 Then
            'Print #11, "Runner on 1st advance to 3rd"
        End If

    End If

    If runnerOnBase(2) = 1 Then

        If baseMove(2) = 1 Then
            'Print #11, "Runner on 2nd advance to 3rd"
        End If

        If baseMove(2) = 2 Then
            'Print #11, "Runner on 2nd scoring"
        End If

    End If

    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)

End Sub


'----------------------------------------
'         Hit_Double Subroutine
'----------------------------------------
'This subroutine handles the stats, PBP,
'advancements and calculation for a double.
Sub Hit_Double (wasError, currFielder, I3, INFPOS%, I1, S2%, P2)

    Call GETOUTFIELDER(currFielder, I1)

    Call INCPITDEF(D, currPitcher(D), LG_P_H)
    Call WINDEX
    Call DELIVERY

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
    Else
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
        Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
    End If

    If d100% <= 40 Then

        If currFielder = 7 Then
            Call GetPBPString(40, 2900, Y$)
            Call PBP(Y$)
        End If

        If currFielder = 9 Then
            Call GetPBPString(40, 2940, Y$)
            Call PBP(Y$)
        End If

        If currFielder = 8 Then
            Call GetPBPString(40, 2980, Y$)
            Call PBP(Y$)
        End If

        Call GetPBPString(30, 3020, Y$)
        Call PBP(Y$)

    Else

        Call Rolld100(resultRoll)

        Select Case resultRoll

            Case Is <= 25
                Call LONGFLY(currFielder)
                HW% = 1
                Call WALLDOUBLE(HW%, currFielder)

            Case 26 To 50
                Call LDRIVE_PBP(X$)
                Call PBP(player$(0) + " " + X$ + " one to the gap in " + OUTFLDLOCATION$(currFielder))

            Case 51 To 60
                Call PBP(player$(0) + " hits a sinking liner into " + OUTFLDLOCATION$(currFielder))
                Call PBP("It goes to the wall")

            Case 61 To 75
                Call PBP(player$(0) + " lines one to the alley in " + OUTFLDLOCATION$(currFielder))

            Case 76 To 85
                If (currFielder = 7 Or currFielder = 9) Then
                    Call LDRIVE_PBP(X$)
                    Call PBP(player$(0) + " " + X$ + " one down the " + fieldPos$(currFielder) + " line")
                    Call PBP("It rolls into the corner")
                End If

                If currFielder = 8 Then
                    Call LDRIVE_PBP(X$)
                    Call PBP(player$(0) + " " + X$ + " one between the fielders")
                    Call PBP(player$(4) + " cuts it off...")
                End If

            Case 86 To 92
                Call LDRIVE_PBP(X$)
                Call PBP(player$(0) + " " + X$ + " a blue darter into " + fieldPos$(currFielder))

            Case 93 To 100
                Call PBP(player$(0) + " lines one to " + OUTFLDLOCATION$(currFielder))
                Call PBP("It's down and to the wall!")

        End Select

    End If

    HW% = 0
    If d100% <= 80 Then
        pbpString$ = "Double!!"
    Else
        pbpString$ = "2-Bagger!!"
    End If

    Call PBP(pbpString$)

    'Default to advancing 2 bases
    For I = 0 To 2:
        baseMove(I) = 2
    Next I

    'Runner on 3rd = advance 1 base
    baseMove(3) = 1

    speedFactor = 3

    eraModifier! = 1.0

    If runnerOnBase(1) <> 0 Then

        Call Rolld100(chanceRoll)

        If fldPos(P, runnerOnBase(1)) = 1 Then
            batRate = pitchRating(P, currPitcher(P), SS_P_Run)
        Else
            batRate = batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run)
        End If

        baseChance = 0

        Select Case currFielder

            Case 7

                Select Case nbrOuts(0)

                    Case 0:
                        baseChance = 18

                    Case 1:
                        baseChance = 16

                    Case 2:
                        baseChance = 34

                End Select

            Case 8

                Select Case nbrOuts(0)

                    Case 0:
                        baseChance = 37

                    Case 1:
                        baseChance = 44

                    Case 2:
                        baseChance = 65

                End Select

            Case 9

                Select Case nbrOuts(0)

                    Case 0:
                        baseChance = 12

                    Case 1:
                        baseChance = 18

                    Case 2:
                        baseChance = 40

                End Select

        End Select

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If

        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -10
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 10

        End Select

        advanceChance = (baseChance * eraModifier!) + (batRate * speedFactor) + runStratBonus

        If chanceRoll <= advanceChance Or P2 = OFF_HIT_N_RUN Then
            'Runner on 1st - advance 3 bases
            baseMove(1) = 3
        End If

    End If

    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)

End Sub


'----------------------------------------
'         Hit_Triple Subroutine
'----------------------------------------
'This subroutine handles the stats, PBP,
'advancements and calculation for a triple.
Sub Hit_Triple (wasError, currFielder, I3, INFPOS%, I1, S2%, P2)

    Call OUTFLDNAME(currFielder)
    Call DELIVERY
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

    Call INCPITDEF(D, currPitcher(D), LG_P_H)

    If d100% <= 30 Then

        If currFielder = 7 Then
            Call GetPBPString(40, 2900, Y$)
            Call PBP(Y$)
        End If

        If currFielder = 9 Then
            Call GetPBPString(40, 2940, Y$)
            Call PBP(Y$)
        End If

        If currFielder = 8 Then
            Call GetPBPString(40, 2980, Y$)
            Call PBP(Y$)
        End If

        Call GetPBPString(40, 3050, Y$)
        Call PBP(Y$)

        If d100% <= 90 Then
            pbpString$ = "Triple!!"
        Else
            pbpString$ = "3-Bagger!"
        End If

        Call PBP(pbpString$)

    Else

        If d100% <= 10 And currFielder <> 8 Then

            If currFielder = 7 Then
                X$ = "3rd"
            Else
                X$ = "1st"
            End If

            If d100% <= 50 Then
                Call PBP(player$(0) + " hits a hot smash")
            Else
                Call PBP(player$(0) + " lines one")
            End If

            Call PBP("Fair past " + X$ + "!!")
            Call PBP("It rolls into the " + fieldPos$(currFielder) + " corner")
            Call PBP("It's a triple!!")

        Else

            Call LONGFLY(currFielder)
            If currFielder = 7 And teamRat_GAME(1, 13) = 27 Then
                ad$ = "screen"
            Else
                ad$ = "wall"
            End If

            Call Rolld100(BV)
            Call Rolld100(resultRoll)

            Select Case resultRoll

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

            If d100% <= 90 Then
                pbpString$ = "Triple!!"
            Else
                pbpString$ = "3-Bagger!"
            End If

            Call PBP(pbpString$)

        End If

    End If

    baseMove(0) = 3
    baseMove(1) = 3
    baseMove(2) = 2
    baseMove(3) = 1

    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)

End Sub



'----------------------------------------
'         Hit_Homerun Subroutine
'----------------------------------------
'This subroutine handles the stats, PBP,
'advancements and calculation for a triple.
Sub Hit_Homerun (wasError, currFielder, I3, INFPOS%, S2%, P2)

    Call WINDEX

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_HR)
    End If

    Call INCPITDEF(D, currPitcher(D), LG_P_H)

    If fldPos(P, currLineupSlot(P)) = 1 Then
        Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
        Call INCPITOFF(P, currPitcher(P), LG_PB_H)
        Call INCPITOFF(P, currPitcher(P), LG_PB_HR)
    End If

    If d100% <= 80 Then

        Call LONGFLY(currFielder)

        Call Rolld100(pbpRoll)

        If pbpRoll <= 5 And teamRat_GAME(1, 13) = 8 And currFielder = 7 Then
            Call PBP("THAT'S OUT OF THE PARK")
            Call PBP("ON WAVELAND AVENUE!!!")
            pbpString$ = "HOME RUN...HOLY COW!!!"
            GoTo LHomeRun2
        End If

        If pbpRoll <= 15 And teamRat_GAME(1, 13) = 8 And currFielder = 7 Then
            Call PBP("IT'S GONE")
            pbpString$ = "LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!"
            GoTo LHomeRun2
        End If

        If pbpRoll <= 25 Then

            'Add stadium-specific context to PBP
            'for a home run
            Select Case teamRat_GAME(1, 13)

                Case 21
                    If currFielder = 7 Then
                        Call PBP("IT CLEARS THE GREEN MONSTER!!")
                        pbpString$ = "HOME RUN!!!"
                        GoTo LHomeRun2
                    End If

                Case 27
                    If currFielder = 7 Then
                        Call PBP("IT CLEARS THE SCREEN!!")
                        pbpString$ = "HOME RUN!!!"
                        GoTo LHomeRun2
                    End If

                Case 33
                    If P = 1 Then
                        Call PBP("ADIOS!! THAT'S A HOME RUN!!")
                        pbpString$ = "THE BUSCH EAGLE FLAPS ITS WINGS"
                        GoTo LHomeRun2
                    End If

                Case 25
                    If P = 1 Then
                        Call PBP("IT'S OUT OF HERE FOR A HOMER!!!")
                        pbpString$ = "THERE GOES THE EXPLODING SCOREBOARD!"
                        GoTo LHomeRun2
                    End If

                Case 7
                    If currFielder = 9 Then
                        Call PBP("IT CLEARS THE WAGON GATE!!")
                        pbpString$ = "THAT'S A HOME RUN!!"
                        GoTo LHomeRun2
                    ElseIf P = 1 Then
                        Call PBP("ADIOS!! THAT'S A HOME RUN!!")
                        pbpString$ = "THERE GO THE CANNONS!!"
                        GoTo LHomeRun2
                    End If

                Case 8, 40
                    pbpString$ = "IT CLEARS THE IVY FOR A HOME RUN!!"
                    GoTo LHomeRun2

                Case 23
                    If currFielder = 7 Or currFielder = 9 Then
                        Call PBP("IT'S LONG, IT'S LONG, IT'S GONE!!")
                        pbpString$ = "THAT ONE'S IN THE UPPER DECK"
                        GoTo LHomeRun2
                    End If

            End Select

            Call Rolld100(pbpRoll)

            Select Case pbpRoll

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
                    Call PBP("IT'S WAY OUT OF HERE!! HOME RUN!!!")
                    pbpString$ = "HOW 'BOUT THAT!!!"

            End Select

        Else

            Call PBP("Going... going... GONE!")
            pbpString$ = "HOME RUN!!"

        End If

    Else

        Call Rolld100(pbpRoll)

        If pbpRoll <= 20 And (currFielder = 7 Or currFielder = 9) Then

            pbpString$ = player$(0) + " hits it down the " + fieldPos$(currFielder) + " line"
            Call PBP(pbpString$)
            Call PBP("It's long enough if it's fair...")

        Else

            Call PBP(player$(0) + " swings...")

            Select Case pbpRoll

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

        pbpString$ = "HOME RUN!!"

    End If


    LHomeRun2:
    Call PBP(pbpString$)
    Call DELAY

    baseMove(0) = 4
    baseMove(1) = 3
    baseMove(2) = 2
    baseMove(3) = 1

    Call INCPITDEF(D, currPitcher(D), LG_P_HR)

    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)

End Sub


'----------------------------------------
'        Add_StolenBase Subroutine
'----------------------------------------
'Increases stats for stolen bases as LONG
'as there was no fielding error.
Sub Add_StolenBase (baseNbr, wasError)

    If runnerOnBase(baseNbr) > 0 And wasError = 0 Then

        If fldPos(P, runnerOnBase(baseNbr)) = 1 Then
            Call INCPITOFF(P, currPitcher(P), LG_PB_SB)
        Else
            Call INCBATOFF(P, lineupPlayer(P, runnerOnBase(baseNbr)), LG_B_SB)
        End If

    End If

End Sub


'----------------------------------------
'        SQUEEZECHOICE Subroutine
'----------------------------------------
'This subroutine gives the user a choice
'for a squeeze / bunt.
Sub SQUEEZECHOICE (squeezeOrSH, baseStatus)

    If (playerMode = 1 And P = compTeam) Or playerMode = 2 Then
        'Computer decision

        'Default to bunt
        squeezeOrSH = 2
        squeezeChance = 0

        ' Basic squeeze situations
        If baseStatus = 3 Then
            ' Runner on 3rd only - classic squeeze setup
            If nbrOuts(0) = 0 Then
                squeezeChance = 25
            End If
            If nbrOuts(0) = 1 Then
                ' More urgent with 1 out
                squeezeChance = 35
            End If
        End If

        If baseStatus = 7 Then
            ' Runners on 1st and 3rd
            If nbrOuts(0) = 0 Then
                squeezeChance = 20
            End If
            If nbrOuts(0) = 1 Then
                squeezeChance = 30
            End If
        End If

        If baseStatus = 5 Or baseStatus = 6 Then
            ' Runners on 2nd and 3rd, or 1st/2nd/3rd
            If nbrOuts(0) = 0 Then
                ' Less urgent with runner on 2nd
                squeezeChance = 15
            End If
            If nbrOuts(0) = 1 Then
                squeezeChance = 25
            End If
        End If

        ' Era adjustments
        Select Case teamYears(P)

            Case Is <= 1919
                ' Dead ball - frequent small ball
                eraAdj = 60

            Case 1920 To 1945
                eraAdj = 46

            Case 1946 To 1960
                eraAdj = 40

            Case 1961 To 1975
                eraAdj = 33

            Case 1976 To 1993
                eraAdj = 30

            Case 1994 To 2005
                ' Steroid era - less small ball
                eraAdj = 20

            Case 2006 To 2015
                eraAdj = 23

            Case Is >= 2016
                'Analytics - very selective
                eraAdj = 17

        End Select

        squeezeChance = squeezeChance * eraAdj / 100

        '-- I hope this is right
        scoreDiff = gameScore(D, 0) - gameScore(P, 0)

        ' Game situation adjustments
        ' Close games increase squeeze likelihood
        If Abs(scoreDiff) <= 1 Then
            squeezeChance = squeezeChance + 15
        End If

        ' Late innings increase urgency
        If INNING% >= 8 Then
            squeezeChance = squeezeChance + 20
        End If

        If INNING% = 9 And scoreDiff <= 0 Then
            ' 9th inning, tied or behind
            squeezeChance = squeezeChance + 25
        End If

        ' Extra innings - every run crucial
        If INNING% >= 10 Then
            squeezeChance = squeezeChance + 30
        End If

        ' Batter considerations
        ' Good contact hitters more likely to squeeze
        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt) >= 8 Then
            squeezeChance = squeezeChance + 10
        End If

        If batRating(P, lineupPlayer(P, currLineupSlot(P)), SS_B_Bunt) <= 4 Then
            squeezeChance = squeezeChance - 10
        End If

        ' Pitcher at bat (more likely to squeeze)
        If fldPos(P, currLineupSlot(P)) = 1 Then
            squeezeChance = squeezeChance + 15
        End If

        ' Count considerations (if available)
        If nbrStrikes >= 2 Then
            ' Risky with 2 strikes
            squeezeChance = squeezeChance - 15
        End If

        ' Make decision
        Call Rolld100(roll)
        If roll <= squeezeChance Then
            squeezeOrSH = 1
        End If

    Else

        Locate 15, 1
        Print "1 - SQUEEZE PLAY"
        Print "2 - BUNT FOR SACRIFICE/HIT"

        Do
            I$ = GetKeyPress$
            squeezeOrSH = Val(I$)
        Loop Until squeezeOrSH >= 1 And squeezeOrSH <= 2

    End If

End Sub

'----------------------------------------
'          STEAL3RD Subroutine
'----------------------------------------
'This subroutine handles the attempt of
'a runner to steal 3rd base.
Sub STEAL3RD (TGTBASE%, AA, stlChance, runnerHeld)

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
        stlChance = stlChance - ((pitchRating(D, currPitcher(D), SS_P_HoldRunner) - 5) * 25) ' - 50
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
    CC = batRating(P, I2, SS_B_2B) ' *** CHANCE TO STEAL 2B & 3B ***

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
'This routine handles the PBP for at-bats
'that result in a strikeout
Sub STRIKEOUT (nbrStrikes)

    ' *** strikeout pbp ***
    If nbrStrikes = 3 Then

        BV = 20

    Else

        Call Rolld100(BV)

        If d100% <= 20 Then
            Call PBP("FULL COUNT...")
        End If

        Call DELIVERY

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
            Call PBP(gameManagers$(P) + " is yelling from the dugout!!")
        End If

        If d100% <= 25 Then
            range = 40
            min = 890
        Else
            range = 80
            min = 930
        End If

        Call GetPBPString(range, min, fileString$)
        Call PBP(fileString$)

    End If

    Call GetPBPString(40, 1010, fileString$)
    Call PBP(fileString$)

    Call Rolld100(soPBP)

    Select Case soPBP

        Case 1 To 20:
            pbpString$ = "Number" + Str$(teamPitStats(D, currPitcher(D), 5) + 1) + " for " + pitchers$(D, currPitcher(D))

        Case 21 To 40:
            pbpString$ = "That's" + Str$(teamPitStats(D, currPitcher(D), 5) + 1) + " for " + pitchers$(D, currPitcher(D))

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
Sub STRIKEOUTCS (strikeoutIndicator)

    If strikeoutIndicator = 7 Then
        currLineupSlot(P) = currLineupSlot(P) + 1
    End If

End Sub


'----------------------------------------
'         TAGADVANCE Subroutine
'----------------------------------------
' ...explanation...
Sub TAGADVANCE (S2%, baseNbr)

    If S2% = EVENT_1B And baseNbr = 2 Then

        If fldPos(P, currLineupSlot(P)) = 1 Then
            I5 = currPitcher(P)
            Call INCPITOFF(P, currPitcher(P), LG_PB_Dbl)
        Else
            Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_Dbl)
        End If

    End If

    If S2% = EVENT_2B And baseNbr = 3 Then

        If fldPos(P, currLineupSlot(P)) = 1 Then
            Call INCPITOFF(P, currPitcher(P), LG_PB_Trp)

            oppPitStats(P, currPitcher(P), LG_PB_Dbl) = oppPitStats(P, currPitcher(P), LG_PB_Dbl) - 1
            teamPitStats(P, currPitcher(P), LG_PB_Dbl) = teamPitStats(P, currPitcher(P), LG_PB_Dbl) - 1
            Exit Sub
        Else
            I3 = 4
            I4 = 5
            I5 = lineupPlayer(P, currLineupSlot(P))
        End If

    End If

    If S2% = EVENT_3B And baseNbr = 4 Then
        I3 = 5
        I4 = 6
        I5 = lineupPlayer(P, currLineupSlot(P))
    End If

    If S2% = EVENT_2B And baseNbr = 3 Or S2% = EVENT_3B And baseNbr = 4 Then

        If pitchRating(D, currPitcher(D), SS_P_Throws) <> 1 Then
            CSS(P, I5, I3 - 2) = CSS(P, I5, I3 - 2) - 1
            CSS(P, I5, I4 - 2) = CSS(P, I5, I4 - 2) + 1
        Else
            teamBatStats(P, I5, I3) = teamBatStats(P, I5, I3) - 1
            oppBatStats(P, I5, I3) = oppBatStats(P, I5, I3) - 1
            teamBatStats(P, I5, I4) = teamBatStats(P, I5, I4) + 1
            oppBatStats(P, I5, I4) = oppBatStats(P, I5, I4) + 1
        End If

    End If

End Sub


'----------------------------------------
'         THROW2FIRST Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for a throw fom 3rd/ss to 1st base
Sub THROW2FIRST ()

    pbpIdx% = Int(Rnd(1) * 40) + 1570

    Call GetPBPString(40, 1570, fileString$)
    Call PBP(fileString$ + " to first")

End Sub


'----------------------------------------
'           TIRING Subroutine
'----------------------------------------
'This subroutine checks for stamina of a
'pitcher and builds a corresponding text
'for Play-by-Play
Sub TIRING ()

    If pitcherStam(P9, 0) <= teamPitStats(P9, currPitcher(P9), 4) + teamPitStats(P9, currPitcher(P9), 1) And pitcherStam(P9, 1) <= teamPitStats(P9, currPitcher(P9), 2) Then

        If pitcherStam(P9, 1) > 0 And INNING% < 6 Or pitcherStam(P9, 1) = 0 And teamPitStats(P9, currPitcher(P9), 4) + teamPitStats(P9, currPitcher(P9), 1) > 4 Then
            If d100% <= 50 Then
                Print pitchers$(P9, currPitcher(P9)); " just doesn't have it today";
            Else
                Print pitchers$(P9, currPitcher(P9)); " ain't got nuthin' on his fastball folks!";
            End If
        Else
            Print pitchers$(P9, currPitcher(P9)); " is tired!!";
        End If

    Else

        If pitcherStam(P9, 1) > 0 And (pitcherStam(P9, 0) <= teamPitStats(P9, currPitcher(P9), 4) + teamPitStats(P9, currPitcher(P9), 1) Or pitcherStam(P9, 1) <= teamPitStats(P9, currPitcher(P9), 2)) Or (teamPitStats(P9, currPitcher(P9), 4) + teamPitStats(P9, currPitcher(P9), 1) > 2 And teamPitStats(P9, currPitcher(P9), 0) >= 5) Then
            Print pitchers$(P9, currPitcher(P9)); " is tiring!!";
        Else

            If pitcherStam(P9, 1) > 0 Or (teamPitStats(P9, currPitcher(P9), 4) + teamPitStats(P9, currPitcher(P9), 1) < 3 And teamPitStats(P9, currPitcher(P9), 2) = 0) Then
                Print pitchers$(P9, currPitcher(P9)); " seems to be OK!!";
            End If

        End If

    End If

End Sub


'----------------------------------------
'         TRIPLEPLAY Subroutine
'----------------------------------------
' ...explanation...
Sub TRIPLEPLAY ()

    Call WINDEX

    player$(4) = batters$(D, fielder(D, 5))

    Call PBP("grounder to " + player$(4) + " @ 3B")
    Call PBP(player$(2) + " OUT @ 3B")
    Call PBP(player$(1) + " OUT @ 2B")
    Call PBP(player$(0) + " OUT @ 1B")
    Call PBP("--- TRIPLE PLAY !!! ---") 'ToDo [bm] Note it in box score somehow
    Call DELAY

    nbrOuts(0) = 3

    For idx = 1 To 3:
        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)
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
'for an attempt to get an extra base when
'an error has been committed.
Sub TRYFOREXTRABASE (S2%, offChoice, stealing, baseNbr, noThrow, baseStatus, runnerOn2nd, currFielder, INFPOS%, wasError, homeSafe)

    homeSafe = 0

    If baseNbr <> 0 Then
        'This happens very rarely
        If Environ$("RUNLOG") = "TRUE" Then
            Print #11, "TRYFOREXTRABASE already have baseNbr = "; baseNbr
        End If

    Else

        baseNbr = 0

        If teamMgrProfileVal(D, 810) = 999 Then
            runStratType = GetRunStrategy%
        Else
            runStratType = 1
        End If

        Select Case runStratType
        
            Case 0
                'Safe = less likely to run
                runStratBonus = -10
                
            Case 1
                'Normal strategy
                runStratBonus = 0
                            
            Case 2
                'Aggressive = more likely to run
                runStratBonus = 10

        End Select

        Call Rolld100(chanceRoll)

        If Environ$("RUNLOG") = "TRUE" Then
            Print #11, "TRYFOREXTRABASE starting at baseNbr = 0, chanceRoll = "; chanceRoll; ", S2Event = "; S2%
            For bm1 = 1 To 3
                If runnerOnBase(bm1) > 0 Then Print #11, "  base:"; bm1; " runnerRating:"; batRating(P, lineupPlayer(P, runnerOnBase(bm1)), SS_B_Run);
            Next bm1
            Print #11,
        End If

        'Determine which base we're stealing
        If S2% = EVENT_2B And runnerOnBase(2) > 0 And (chanceRoll < 6 + runStratBonus) And runnerOnBase(3) = 0 And batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_3B) > 0 Then
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "TRYFOREXTRABASE stretching a Double to a Triple..."
            End If
            baseNbr = 3
        End If
        If S2% = EVENT_3B And runnerOnBase(3) > 0 And (chanceRoll <= 2 + runStratBonus) And batRating(P, lineupPlayer(P, runnerOnBase(3)), SS_B_HR) > 0 Then
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "TRYFOREXTRABASE stretching a Triple to an Inside-The-Park HR..."
            End If
            baseNbr = 4
        End If
        If S2% = EVENT_1B And runnerOnBase(3) > 0 And offChoice = OFF_HIT_N_RUN And _ 
            (chanceRoll >= 6 + runStratBonus) And (chanceRoll <= batRating(P, lineupPlayer(P, runnerOnBase(3)), SS_B_Run)) Then
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "TRYFOREXTRABASE trying to score from 3rd on a Hit & Run Single..."
            End If
            baseNbr = 4
        End If
        If S2% = EVENT_1B And runnerOnBase(3) > 0 And runnerOn2nd = runnerOnBase(3) And offChoice <> OFF_HIT_N_RUN And _
            chanceRoll <= (batRating(P, lineupPlayer(P, runnerOnBase(3)), SS_B_Run) * 12) + runStratBonus Then
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "TRYFOREXTRABASE trying to score from 3rd on a Single..."
            End If
            baseNbr = 4
        End If

        runRating = 10

        If runnerOnBase(2) > 0 Then
            runRating = runRating + batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run)
        End If
        If S2% = EVENT_1B And runnerOnBase(2) > 0 And offChoice <> OFF_HIT_N_RUN And chanceRoll <= runRating And runnerOnBase(3) = 0 And baseNbr = 0 Then
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "TRYFOREXTRABASE trying to advance to 3rd on a Single..."
            End If
            baseNbr = 3
        End If
        If S2% = EVENT_1B And runnerOnBase(1) > 0 And runnerOnBase(2) = 0 And baseNbr = 0 And chanceRoll <= batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run) And batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_2B) > 0 Then
            If Environ$("RUNLOG") = "TRUE" Then
                Print #11, "TRYFOREXTRABASE stretching a Single to a Double..."
            End If
            baseNbr = 2
        End If

    End If

    If baseNbr <> 0 Then
        'We shouldn't still have a 0 value!

        If Not ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
            If S2% < EVENT_HR Then
                Call PBP(player$(baseNbr - 1) + " try for " + baseName$(baseNbr) + "? (Y/N) ")
            End If
            If S2% > EVENT_HR Then
                Call PBP(player$(baseNbr - 1) + " tag up? (Y/N) ")
            End If
            Call PBP("against " + player$(4))
        End If

        I$ = "N"
        scoreDiff = gameScore(P, 0) - gameScore(D, 0)

        If ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
            If Abs(scoreDiff) < 2 And S2% <> EVENT_FLY_SHALLOW Then
                I$ = "Y"
                If Environ$("RUNLOG") = "TRUE" Then
                    Print #11, "TRYFOREXTRABASE Green light! -- game margin is within 1 run and event is: "; S2%
                End If
            Else
                If scoreDiff < 0 And (INNING% >= 7 Or S2% = EVENT_FLY_SHALLOW) Then
                    I$ = "N"
                End If
            End If

            If S2% = EVENT_2B And nbrOuts(0) = 2 And baseNbr = 3 Then
                I$ = "N"
                If Environ$("RUNLOG") = "TRUE" Then
                    Print #11, "TRYFOREXTRABASE not going to try advancing to 3rd with 2 outs..."
                End If
            End If
        End If

        skipThrow% = 0
        If Not ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then
            noThrow = 1
        Else
            pbpString$ = player$(baseNbr - 1) + " is "

            If Environ$("PBPLOG") = "TRUE" Then
                Print #11, ";;;;; Runner going for base: "; baseNbr; " on event: "; S2%
            End If

            If S2% < EVENT_HR Then
                Call PBP(pbpString$ + "going for " + baseName$(baseNbr))
            Else
                'S2% >= 10
                Call PBP(pbpString$ + "tagging")
            End If

            If (S2% <> EVENT_1B Or baseNbr = 2) And (S2% <= EVENT_HR Or baseStatus <> 3) Then

                If Not ((playerMode = 1 And D = compTeam) Or playerMode = 2) Then
                    Call PBP(player$(4) + " throw to " + baseName$(baseNbr) + "? (Y/N) ")
                End If

                Call Rolld100(RN0)

                If ((playerMode = 1 And D = compTeam) Or playerMode = 2) Then
                    If teamMgrProfileVal(D, 766) = 999 And baseNbr = 4 Then

                        If Environ$("MGRLOG") = "TRUE" Then
                            Print #11, "!! Team is using a THROWING HOME strategy !!"
                        End If

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

                Call FLDERR(S2%, I3, stealing, currFielder, INFPOS%, wasError, homeSafe)

                If wasError = 0 Then

                    I4 = Int(Rnd(1) * 1000) + 1

                    runRating = batRating(P, lineupPlayer(P, runnerOnBase(baseNbr - 1)), SS_B_Run)

                    If fldPos(P, runnerOnBase((baseNbr - 1))) = 1 Then
                        runRating = pitchRating(P, currPitcher(P), SS_P_Run)
                    End If

                    runRating = (runRating - 5) * 50

                    armRating = (batRating(D, fielder(D, currFielder), SS_B_Arm) - 5) * 25
                    If S2% = EVENT_FLY_DEEP Then
                        I3 = 1000 + runRating - armRating ' WAS 800
                    End If
                    If S2% = EVENT_FLY_REGULAR Then
                        I3 = 800 + runRating - armRating ' WAS 650
                    End If
                    If S2% = EVENT_FLY_SHALLOW Or S2% < EVENT_HR Then
                        I3 = 500 + runRating - armRating
                    End If

                    If I4 > I3 Then
                        ' outcome = OUT!

                        baseMove(baseNbr - 1) = 11

                        Call TrackOuts(1)

                        Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                        Call Rolld100(outPBP)

                        Select Case outPBP

                            Case 1 To 4
                                If baseNbr = 4 Then
                                    Call PBP(player$(baseNbr - 1) + " collides with " + batters$(D, fielder(D, 2)))
                                    Call PBP(batters$(D, fielder(D, 2)) + " hangs on!!")
                                End If

                            Case 5 To 50
                                If baseNbr <> 4 Then
                                    Call PBP(player$(baseNbr - 1) + " doesn't make it!")
                                End If

                            Case 51 To 80
                                Call PBP("throw is perfect!")

                            Case 81 To 90
                                Call PBP("This will be close")

                            Case 91 To 100
                                Call PBP("Close play.....")
                                If baseNbr < 4 Then
                                    Call PBP(gameUmpire$(baseNbr + 1) + " makes the call...")
                                Else
                                    Call PBP(gameUmpire$(1) + " makes the call...")
                                End If

                        End Select

                        Call CreditAssist(currFielder)
                        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
                        Call CreditInfieldPutout(2, FB2P%, AST%)
                        currFielder = 2

                        If nbrOuts(0) = 3 Or S2% > EVENT_HR Then Exit Sub

                        LRunnerGoing:
                        Call Rolld100(I5)

                        scoreDiff = gameScore(P, 0) - gameScore(D, 0)

                        If Not (scoreDiff < -2 Or scoreDiff < 0 And INNING% > 8) Then

                            ' *** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
                            If baseNbr - 1 > 2 And I5 <= 15 And runnerOnBase(2) > 0 Then

                                Call CreditAssist(currFielder)
                                Call PBP("runner is trying for 3rd...")
                                baseMove(2) = 11

                                Call TrackOuts(1)

                                currFielder = 5
                                Call CreditRegularPutout(currFielder)
                                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                            End If

                            ' *** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***
                            If baseNbr - 1 > 1 And d100% <= 15 And runnerOnBase(1) > 0 And baseMove(2) <> 11 Then

                                Call CreditAssist(currFielder)
                                currFielder = 6
                                Call CreditRegularPutout(currFielder)
                                Call PBP("runner is trying for 2nd...")
                                baseMove(1) = 11

                                Call TrackOuts(1)

                                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

                            End If

                            If runnerOnBase(2) > 0 And baseNbr - 1 > 2 And baseMove(2) <> 11 And d100% > (80 - batRating(P, lineupPlayer(P, runnerOnBase(2)), SS_B_Run) * 2) And noThrow = 0 Then
                                baseMove(2) = 1
                                Call PBP("runner is trying for 3rd...")
                                Call PBP("He's safe!")
                            End If

                            I5 = Int(Rnd(1) * 10) + 1

                            If runnerOnBase(1) > 0 And baseMove(1) <> 11 And (baseStatus = 4 Or baseStatus = 7 And baseNbr - 1 > 1 And I5 > (10 - batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run)) And noThrow = 0) Then
                                baseMove(1) = 1
                                Call PBP("runner is trying for 2nd...")
                                Call PBP("He's in there!")
                            Else

                                If runnerOnBase(1) > 0 And baseNbr - 1 > 1 And d100% > (80 - batRating(P, lineupPlayer(P, runnerOnBase(1)), SS_B_Run) * 2) And noThrow = 0 And baseMove(1) <> 11 Then
                                    baseMove(1) = 1
                                    Call PBP("runner safe at 2nd on the throw")
                                End If

                            End If

                        End If

                        Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
                        noThrow = 0

                        Exit Sub

                    End If

                End If

                baseMove(baseNbr - 1) = 1 ' outcome = SAFE!

                If noThrow <> 1 Then

                    Call Rolld100(safePBP)

                    Select Case safePBP

                        Case 1 To 4:
                            If baseNbr = 4 Then
                                Call PBP(player$(baseNbr - 1) + " collides with " + batters$(D, fielder(D, 2)))
                                Call PBP("He knocks the ball loose!!")
                            End If

                        Case 5 To 50:
                            If baseNbr <> 4 Then
                                Call PBP(player$(baseNbr - 1) + " beats the throw!")
                            End If

                        Case 51 To 60:
                            Call PBP(player$(baseNbr - 1) + " is ahead of the throw")

                        Case 61 To 80:
                            Call PBP("throw is off line")

                        Case 81 To 90:
                            Call PBP(player$(baseNbr - 1) + " slides under the tag")

                        Case 91 To 100:
                            Call PBP("Close play...")

                            If baseNbr <> 4 Then
                                Call PBP(gameUmpire$(baseNbr + 1) + " makes the call...")
                            Else
                                Call PBP(gameUmpire$(1) + " makes the call...")
                            End If

                    End Select

                End If

                Call PBP(player$(baseNbr - 1) + " is safe @ " + baseName$(baseNbr))

                If baseNbr = 4 Then
                    homeSafe = 1
                End If

                Call TAGADVANCE(S2%, baseNbr)

                If S2% < EVENT_HR Then
                    GoTo LRunnerGoing
                End If

                If S2% > EVENT_HR Then

                    If fldPos(P, currLineupSlot(P)) = 1 Then

                        teamPitStats(P, currPitcher(P), 23) = teamPitStats(P, currPitcher(P), 23) - 1
                        oppPitStats(P, currPitcher(P), 23) = oppPitStats(P, currPitcher(P), 23) - 1
                        Call INCPITOFF(P, currPitcher(P), LG_PB_SF)
                        Call INCPITDEF(D, currPitcher(D), LG_P_SF)

                    Else

                        teamBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) = teamBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) - 1
                        oppBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) = oppBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0) - 1

                        If pitchRating(D, currPitcher(D), SS_P_Throws) <> 1 Then
                            CSS(P, lineupPlayer(P, currLineupSlot(P)), 0) = CSS(P, lineupPlayer(P, currLineupSlot(P)), 0) - 1
                        End If

                        Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_SF)
                        Call INCPITDEF(D, currPitcher(D), LG_P_SF)

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
                            Color teamRat_GAME(WK, 11), teamRat_GAME(WK, 12)
                            Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

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
    Call Rolld100(resultRoll)

    Select Case BV

        Case 1 To 40:

            If HW% = 0 Then
                pbpString$ = "It's in the gap and to the wall"
            End If

        Case 41 To 65

            Select Case resultRoll

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

            If resultRoll <= 25 And teamRat_GAME(1, 13) = 21 Then
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
Sub WHICHRUNNER (i, baseStatus, D5)

    If i = 4 Then
        baseMove(3) = 1
    End If

    If baseStatus = 4 Then

        Call PBP("WHICH RUNNER DO YOU WANT TO GO AFTER")

        Call PBP("1 - " + batters$(P, lineupPlayer(P, runnerOnBase(2))))

        If fldPos(P, runnerOnBase(1)) <> 1 Then
            Call PBP("2 - " + batters$(P, lineupPlayer(P, runnerOnBase(1))))
        Else
            Call PBP("2 - " + pitchers$(P, currPitcher(P)))
        End If

        If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then
            D5 = Int(Rnd(1) * 2) + 1
            i$ = Str$(D5)
        Else
            i$ = GetKeyPress$
        End If

        D5 = Val(i$)

        If D5 = 2 Then
            i = 2
            D5 = 3
            I2 = lineupPlayer(P, runnerOnBase(1))
            If fldPos(P, runnerOnBase(1)) = 1 Then
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
Sub WHOSON1ST (baseStatus)

    baseStatus = 0
    I1 = runnerOnBase(1)
    I2 = runnerOnBase(2)
    I3 = runnerOnBase(3)

    If I1 > 0 Then
        If I2 = 0 Then
            If I3 = 0 Then
                baseStatus = 1
            Else
                ' *** 1ST AND 3RD ***
                baseStatus = 5
            End If
        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** 1ST AND 2ND ***
                baseStatus = 4
            Else
                'I3 > 0
                ' *** BASES LOADED ***
                baseStatus = 7
            End If
        End If
    Else
        'I1 = 0
        If I2 = 0 Then
            If I3 > 0 Then
                ' *** RUNNER ON 3RD ***
                baseStatus = 3
            End If

        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** RUNNER ON 2ND ***
                baseStatus = 2
            Else
                'I3 > 0
                ' *** 2ND AND 3RD ***
                baseStatus = 6
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
        Call GetPBPString(20, 3130, fileString$)
        Call PBP(fileString$)
        Call PBP("foul ball!!")
    Else
        Call GetPBPString(10, 3150, fileString$)
        Call PBP(fileString$)

        Call GetPBPString(10, 3160, fileString$)
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
Sub RAREPLAYS (INFPOS%, currFielder, wasError, H6%, playerInjured, baseStatus)

    Dim playerEjectMax As Integer
    Dim coachMgrEjectMax As Integer

    Select Case teamYears(1)

        Case 1899 To 1919
            playerEjectMax = 8
            coachMgrEjectMax = 2

        Case 1920 To 1945
            playerEjectMax = 5
            coachMgrEjectMax = 2

        Case 1946 To 2003
            playerEjectMax = 5
            coachMgrEjectMax = 4

        Case 2004 To 2013
            playerEjectMax = 4
            coachMgrEjectMax = 5

        Case Is >= 2014
            playerEjectMax = 3
            coachMgrEjectMax = 5

    End Select

    totalEject = playerEjectMax + coachMgrEjectMax
    ejectRatio! = 14 / totalEject
    playerEjectScaled = CInt(playerEjectMax * ejectRatio!)
    coachMgrEjectScaled = CInt(coachMgrEjectMax * ejectRatio!)

    Call WINDEX

    Call Rolld100(rareResult)

    Select Case rareResult

        Case Is <= playerEjectScaled
            Call PlayerEjections(baseStatus)

        Case playerEjectScaled + 1 To playerEjectScaled + coachMgrEjectScaled
            Call CoachMgrEjections(INFPOS%, currFielder, wasError, playerInjured, baseStatus)

        Case 15
            Call PBP("Here's the 3-2 pitch...")
            Call PBP("Time called...")
            Call PBP(gameUmpire$(1) + " is calling it ball 4")
            Call PBP("He says " + pitchers$(D, currPitcher(D)) + " went to his mouth")

        Case 16 To 21
            H6% = 21

            Call WINDEX
            Call PBP(player$(0) + " fouls the pitch out of play")

        Case 22 To 24

            Select Case baseStatus

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

                baseMove(pickedOffBase - 1) = 10

                Call TrackOuts(1)

                currFielder = 2

                Call CreditAssist(currFielder)
                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
                Call CreditInfieldPutout(I3, FB2P%, AST%)

                Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

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
            Call CreditRegularPutout(currFielder)

            Call TrackOuts(1)

            Call DELAY

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
            End If

            Call INCPITDEF(D, currPitcher(D), LG_P_OUT)

            If fldPos(P, currLineupSlot(P)) = 1 Then
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
            End If

            Call DELAY

        Case 30
            Call PBP(player$(0) + " pops it up foul near the stands")
            Call PBP(batters$(D, fielder(D, 2)) + " runs into the stands!!")

            Call INJURYLENGTH(injCount, 2, W5, W6)
            H6% = 1

        Case 30 To 31
            Call PBP(player$(0) + " pops it up foul near the stands")
            Call PBP(batters$(D, fielder(D, 3)) + " gives chase")
            Call PBP("he runs into the stands!!")

            Call INJURYLENGTH(injCount, 3, W5, W6)
            H6% = 1

        Case 32
            Call PBP(player$(0) + " pops it up foul down the rf line")
            Call PBP(batters$(D, fielder(D, 4)) + " crashes into the wall")

            Call INJURYLENGTH(injCount, 4, W5, W6)
            H6% = 1

        Case 33
            Call PBP(player$(0) + " pops it up foul by")
            Call PBP("the 3rd base dugout")
            Call PBP(batters$(D, fielder(D, 5)) + " falls in trying to make the catch!!")

            Call INJURYLENGTH(injCount, 5, W5, W6)
            H6% = 1

        Case 34
            Call PBP(player$(0) + " pops it up foul")
            Call PBP("down the lf line")
            Call PBP(batters$(D, fielder(D, 6)) + " tumbles over the railing")
            Call PBP("trying to make the catch!")
            Call DELAY

            Call INJURYLENGTH(injCount, 6, W5, W6)
            H6% = 1

        Case 35 To 37

            Call PBP(player$(0) + " lines one to deep left field")
            Call PBP(batters$(D, fielder(D, 7)) + " races back...")
            Call PBP("He slams into the wall and is down!!")
            Call PBP(batters$(D, fielder(D, 8)) + " comes over and gets the ball in")

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

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
                baseMove(idx) = 2
            Next idx

            baseMove(1) = 3
            baseMove(3) = 1

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
            Call SCOREBOARD
            Call WINDEX

            Call INJURYLENGTH(injCount, 7, W5, W6)
            H6% = 5

        Case 38 To 40

            Call PBP(player$(0) + " drives a sinking liner")
            Call PBP("into deep center")
            Call PBP(batters$(D, fielder(D, 8)) + " gets a good jump on it")
            Call PBP("He crashes into the wall and")
            Call PBP("the ball bounces away!!")
            Call PBP("The runners are circling the bases")
            Call PBP(batters$(D, fielder(D, 9)) + " retrieves the ball")

            Call INCPITDEF(D, currPitcher(D), LG_P_H)
            Call INCPITDEF(D, currPitcher(D), LG_P_HR)

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

            baseMove(0) = 4
            baseMove(1) = 3
            baseMove(2) = 2
            baseMove(3) = 1

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
            Call PBP("It's an inside the park HOME RUN!!!")
            Call DELAY
            Call SCOREBOARD
            Call WINDEX

            Call INJURYLENGTH(injCount, 8, W5, W6)
            H6% = 5

        Case 41 To 43

            Call PBP(player$(0) + " lifts a deep drive into right")
            Call PBP(batters$(D, fielder(D, 9)) + " races back...")
            Call PBP("He slams into the wall and is down!!")

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

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

            baseMove(0) = 3
            baseMove(1) = 3
            baseMove(2) = 2
            baseMove(3) = 1

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
            Call PBP("Triple!!")
            Call DELAY
            Call SCOREBOARD
            Call WINDEX

            Call INJURYLENGTH(injCount, 9, W5, W6)
            H6% = 5

        Case 44

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

            currFielder = Int(Rnd(1) * 3) + 7

            Call PBP(player$(0) + " hits a sinking liner into " + fieldPos$(currFielder))
            Call PBP(batters$(D, fielder(D, currFielder)) + " dives for the catch...")
            Call PBP("the ball rolls free!")
            Call PBP(batters$(D, fielder(D, currFielder - 1)) + " gets the loose ball in")

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

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
                baseMove(idx) = 2
            Next idx

            baseMove(1) = 3
            baseMove(3) = 1

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
            Call SCOREBOARD
            Call WINDEX

            Call INJURYLENGTH(injCount, currFielder, W5, W6)
            H6% = 5

        Case 45

            Call PBP(player$(0) + " is hit by a pitch and injured")
            Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

        Case 46 To 49

            Call PBP(batters$(D, fielder(D, 2)) + " is injured by a foul tip!")

            Call INJURYLENGTH(injCount, 2, W5, W6)
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

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

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
                baseMove(idx) = 2
            Next idx

            baseMove(1) = 3
            baseMove(3) = 1

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
            Call SCOREBOARD
            Call WINDEX

            Call INJURYLENGTH(injCount, hi1, W5, W6)
            Call INJURYLENGTH(injCount, hi2, W5, W6)
            H6% = 5

        Case 59 To 61
            Call PBP(player$(0) + " fouls the pitch off his foot")
            Call PBP("He is injured")
            Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

            H6% = 1

            '62 to 64
        Case 62 To 64

            Call PBP(player$(0) + " pops it up in foul along the 3b line")
            Call PBP("It drifts into foul territory")
            Call PBP(batters$(D, fielder(D, 5)) + " and " + pitchers$(D, currPitcher(D)) + " chase after it and collide!!")
            Call PBP(batters$(D, fielder(D, 5)) + " is down and injured")

            Call INJURYLENGTH(injCount, 5, W5, W6)
            H6% = 1

        Case 65 To 70

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

            currFielder = 8

            Call LONGFLY(currFielder)
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

            baseMove(0) = 2
            baseMove(1) = 3
            baseMove(2) = 2
            baseMove(3) = 1

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
            Call PBP("Double!!")
            Call SCOREBOARD
            Call WINDEX

            Call INJURYLENGTH(injCount, 7, W5, W6)
            Call INJURYLENGTH(injCount, 8, W5, W6)
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

                Call INCPITDEF(D, currPitcher(D), LG_P_H)

                For idx = 0 To 3:
                    baseMove(idx) = 1
                Next

                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
                Call SCOREBOARD
                Call WINDEX
                Call PBP("He reaches on a single")
                Call PBP(pitchers$(D, currPitcher(D)) + " is down and in pain!")
                Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)
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

                Call INCPITDEF(D, currPitcher(D), LG_P_H)

                For idx = 0 To 3:
                    baseMove(idx) = 1
                Next

                Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
                Call SCOREBOARD
                Call WINDEX
                Call PBP(batters$(D, fielder(D, po%)) + " is down and injured")
                Call INJURYLENGTH(injCount, po%, W5, W6)
                H6% = 5

            End If

        Case 86 To 89

            Call PBP(pitchers$(D, currPitcher(D)) + " is motioning for " + gameManagers$(D) + " to come out")
            Call PBP("It appears he has a blister")
            Call PBP("He will have to leave the game")
            Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)

            H6% = 1

        Case 90

            Call PBP(player$(0) + " takes a mighty swing!")
            Call PBP("It looks like he injured himself")
            Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

            H6% = 1

        Case 91

            Call PBP("Time is called")
            Call PBP("There's a fan on the field!!!")

            H6% = 1

        Case 92 To 100

            Call PBP(gameUmpire$(1) + " calls time to dust off the plate")

            H6% = 1

    End Select

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
'       PlayerEjections Subroutine
'----------------------------------------
'This routine handles the PBP and aftermath
'for a player ejection.
Sub PlayerEjections (baseStatus)

    scaledRoll = Int(Rnd(1) * 14) + 1

    Select Case scaledRoll

        Case 1 To 3
            ' *** Batter argues balls/strikes ***
            Call PBP(batters$(D, fielder(D, 2)) + " is arguing ball and strike calls")
            Call PBP(gameUmpire$(1) + " has heard enough")
            Call PBP(batters$(D, fielder(D, 2)) + " has been ejected!!")

            X$ = batters$(D, fielder(D, 2))
            Call AddEjection(X$)

            Call DELAY

            Call EJECTIONINJURY(2, W5, W6)

        Case 4 To 6
            ' *** Pitcher argues balls/strikes ***
            Call PBP(pitchers$(D, currPitcher(D)) + " steps off the mound")
            Call PBP("after that last call")
            Call PBP("He is shouting at " + gameUmpire$(1))
            Call PBP(gameUmpire$(1) + " does not appreciate what he hears...")
            Call PBP(pitchers$(D, currPitcher(D)) + " has been ejected!!")

            X$ = pitchers$(D, currPitcher(D))
            Call AddEjection(X$)

            Call DELAY

            Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)

        Case 7 To 9
            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call PBP(player$(0) + " is unhappy with that last call")
                Call PBP("He is venting his opinion to " + gameUmpire$(1))
                Print "Thats it!! "; gameUmpire$(1); " has ejected "; player$(0); "!!"

                X$ = player$(0)
                Call AddEjection(X$)

                Call DELAY
                Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

            Else

                Call PBP(player$(0) + " did not like that")
                Call PBP("last call at all!!")

            End If

        Case 10 To 12
            ' *** Bench player yelling from dugout ***
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
            Call PBP(" has just been tossed!!")

            X$ = player$(0)
            Call AddEjection(X$)

            Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

        Case 13
            ' *** Batter charges mound ***
            Call PBP(gameUmpire$(1) + " has just ejected " + pitchers$(D, currPitcher(D)))
            Call PBP("for wetting the ball!!")

            Call AddEjection(X$)
            ejections$(ejectCnt) = pitchers$(D, currPitcher(D))

            Call DELAY
            Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)

        Case 14
            ' *** Bench-clearing brawl ***
            Call PBP(player$(0) + " hits the deck on a high")
            Call PBP("and tight pitch from " + pitchers$(D, currPitcher(D)))
            Call PBP(player$(0) + " charges the mound!")
            Call PBP("Both dugouts have emptied...")
            Call PBP("It's a bench-clearing brawl!")
            Call WINDEX
            Call PBP("It will take a few minutes to clean this up")

            RN0 = Int(Rnd(1) * 3) + 1

            'Print #11, "Player Ejection"

            Select Case RN0

                Case 1:
                    Call PBP(pitchers$(D, currPitcher(D)) + " has been ejected")

                    X$ = pitchers$(D, currPitcher(D))
                    Call AddEjection(X$)

                    Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)

                Case 2:
                    Call PBP(player$(0) + " has been ejected")

                    X$ = player$(0)
                    Call AddEjection(X$)

                    Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

                Case 3:
                    Call PBP(player$(0) + " and " + pitchers$(D, currPitcher(D)) + " have been ejected")

                    X$ = player$(0)
                    Call AddEjection(X$)

                    X$ = pitchers$(D, currPitcher(D))
                    Call AddEjection(X$)

                    Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)
                    Call PLYRHRT(W5, W6, nbrBalls, nbrStrikes, MO)

            End Select

    End Select


End Sub


'----------------------------------------
'      CoachMgrEjections Subroutine
'----------------------------------------
'This routine handles the chances of
'either a coach or manager being ejected
'from a game and the PBP associated.
Sub CoachMgrEjections (INFPOS%, currFielder, wasError, playerInjured, baseStatus)

    Dim coachMgrEjectMax As Integer

    Select Case teamYears(1)

        Case 1899 To 1919
            mgrEjectMax = 21
            coachEjectMax = 1

        Case 1920 To 1945
            mgrEjectMax = 16
            coachEjectMax = 6

        Case 1946 To 1979
            mgrEjectMax = 29
            coachEjectMax = 9

        Case 1980 To 2003
            mgrEjectMax = 32
            coachEjectMax = 10

        Case 2004 To 2013
            mgrEjectMax = 38
            coachEjectMax = 6

        Case Is >= 2014
            mgrEjectMax = 37
            coachEjectMax = 9

    End Select

    outcomeChance = coachEjectMax + mgrEjectMax

    outcome = Int(Rnd(1) * outcomeChance) + 1

    Select Case outcome

        Case 1 To coachEjectMax
            ' Coach arguing balls and strikes
            Call PBP("The coach is visibly upset with the umpire's strike zone.")
            Call PBP("He is waving his arms and yelling from the dugout.")
            Call PBP(gameUmpire$(1) + " is not hearing it.")
            Call PBP("The coach has been ejected from the game!")

            X$ = gameManagers$(D)
            Call AddEjection(X$)

            Call DELAY

        Case coachEjectMax + 1 To mgrEjectMax

            Call PBP(player$(0) + " hits a slow roller to first")
            Call PBP(batters$(D, fielder(D, 3)) + " fields it cleanly")
            Call PBP("He flips it to " + pitchers$(D, currPitcher(D)) + " covering just in time!")
            Call PBP("Wait... " + gameUmpire$(2) + " rules safe!!")

            If fldPos(P, currLineupSlot(P)) <> 1 Then
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_AB)
                Call INCBATOFF(P, lineupPlayer(P, currLineupSlot(P)), LG_B_H)
            End If

            Call INCPITDEF(D, currPitcher(D), LG_P_H)

            If fldPos(P, currLineupSlot(P)) = 1 Then
                Call INCPITOFF(P, currPitcher(P), LG_PB_AB)
                Call INCPITOFF(P, currPitcher(P), LG_PB_H)
            End If

            For idx = 0 To 3:
                baseMove(idx) = 1
            Next idx

            Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
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
            Call AddEjection(X$)

            X$ = batters$(D, fielder(D, 2))
            Call AddEjection(X$)

            X$ = batters$(D, fielder(D, 3))
            Call AddEjection(X$)

            X$ = gameManagers$(D)
            Call AddEjection(X$)

            Call DELAY

            Call EJECTIONINJURY(2, W5, W6)
            Call EJECTIONINJURY(3, W5, W6)

            Call InjuredPitcher(playerInjured, pinchHitterFlag, baseStatus)

    End Select

End Sub


'----------------------------------------
'          ColorCheck Subroutine
'----------------------------------------
'Change color of players who are not able
'to be used in lineup so that they can
'stand out to the user.
Sub ColorCheck (teamIdx, playerIdx)

    If batRating(teamIdx, playerIdx, SS_B_Avail) >= 98 Then
        Color 0, 7
    End If

End Sub

'----------------------------------------
'       DisplayGameState Subroutine
'----------------------------------------
'This is a quick subroutine that will
'simply update the screen
Sub DisplayGameState (ballCount, strikeCount, MO, INFPOS%)

    Call HITTINGLINES

    If useVGA = 1 Then
        Call DisplayBallField_Gfx(ballCount, strikeCount)
        Call MAINSCREEN_G(ballCount, strikeCount, MO, INFPOS%)
    Else
        Call DisplayBallField(ballCount, strikeCount)
        Call MAINSCREEN(ballCount, strikeCount, MO)
    End If

End Sub


'----------------------------------------
'         GETWEATHER Subroutine
'----------------------------------------
'This subroutine opens the PARKS.DB file
'to read in attendance data, wind factor,
'weather and other ballpark calculations.
Sub GETWEATHER (windDir, windSpeed, gameMonth, windFactor!, skyType, gameTemp, rainDelay)

    Shared monthlyRatings()

    Shared avgAttendance&

    Shared fenceDistCF
    Shared fenceDistLF, fenceDistLCF
    Shared fenceDistRF, fenceDistRCF
    Shared DOME, turfType, windRating

    'Force weather from a valid stadium
    If teamRat_GAME(1, 13) > 82 Then
        parkID = Int(Rnd(1) * 79) + 1
    Else
        parkID = teamRat_GAME(1, 13)
    End If

    'The layout of the PARKS.DB file is as follows:
    'Park Name (40 chars)
    'Attendance
    'Park Ratings - LF, LCF, CF, RCF, RF, Turf(Y/N), Dome(Y/N), Wind Impact
    'Where wind impact is based on stadium location / design.
    '6 Months Data, 15 weather factors
    '   0 = Base Temp
    '   1 = Delta Temp
    '   2 = Wind R->L
    '   3 = Wind L->R
    '   4 = Wind from INL (infield, L)
    '   5 = Wind from INC (infield, C)
    '   6 = Wind from INR (infield, R)
    '   7 = Wind to OUTL (outfield, L)
    '   8 = Wind to OUTC (outfield, C)
    '   9 = Wind to OUTR (outfield, R)
    '   10 = WS09 (windspeed 0-9)
    '   11 = WS1014 (windspeed 10-14)
    '   12 = WS1519 (windspeed 15-19)
    '   13 = WS20   (windseed 20+)
    '   14 = PRECIP

    Open PARK_FILE_NAME$ For Random As #1 Len = PARK_SIZE_BYTES

    Field #1, 40 As Q3$(0), 4 As Q3$(1), 2 As Q3$(2), 2 As Q3$(3), 2 As Q3$(4), 2 As Q3$(5), 2 As Q3$(6), 2 As Q3$(7), 2 As Q3$(8), 2 As Q3$(9)

    For I = 0 To 5
        For I1 = 0 To 14
            Field #1, 60 + I * 30 + I1 * 2 As X$, 2 As Q3$(10 + I1 + I * 15)
        Next
    Next

    Get #1, parkID

    avgAttendance& = CVL(Q3$(1))
    fenceDistLF = CVI(Q3$(2))
    fenceDistLCF = CVI(Q3$(3))
    fenceDistCF = CVI(Q3$(4))
    fenceDistRCF = CVI(Q3$(5))
    fenceDistRF = CVI(Q3$(6))
    turfType = CVI(Q3$(7))
    DOME = CVI(Q3$(8))
    windRating = CVI(Q3$(9))

    'April(0) - September(5)
    For I = 0 To 5
        For I1 = 0 To 14
            monthlyRatings(I, I1) = CVI(Q3$(10 + I1 + I * 15))
        Next
    Next

    Close #1

    If gameMonth < 4 Then gameMonth = 4
    If gameMonth > 9 Then gameMonth = 9

    'Ratings stored only for April+
    'Data stored using actual month #
    targetMonth = gameMonth - 4

    'Random temperature based on delta
    If teamRat_GAME(1, 9) = 1 Then
        gameTemp = 70
    Else
        If d100% <= 50 Then
            gameTemp = monthlyRatings(targetMonth, 0) + Int(Rnd(1) * monthlyRatings(targetMonth, 1) + 1)
        Else
            gameTemp = monthlyRatings(targetMonth, 0) - Int(Rnd(1) * monthlyRatings(targetMonth, 1) + 1)
        End If
    End If

    'SUM all wind direction ratings
    'Ratings stored represent a % of games
    'impacted by winds that direction
    winDirSUM = 0

    For I = 2 To 9
        winDirSUM = winDirSUM + monthlyRatings(targetMonth, I)
    Next

    'Determine which rating to use based on weighted probability
    RN0 = Int(Rnd(1) * winDirSUM) + 1

    'Field ratings
    winDirSUM = 0
    interval = 2
    Do While interval <= 9 And RN0 > winDirSUM
        winDirSUM = winDirSUM + monthlyRatings(targetMonth, interval)
        interval = interval + 1
    Loop

    windDir = interval

    'SUM all wind speed ratings
    'Ratings stored represent a % of wind games
    'that are of each speed interval
    windSUM = 0
    For I = 10 To 13
        windSUM = windSUM + monthlyRatings(targetMonth, I)
    Next

    'Determine which wind speed to use
    'based on random #
    RN0 = Int(Rnd(1) * windSUM) + 1

    windSUM = 0
    interval = 10
    Do While interval <= 13 And RN0 > windSUM
        windSUM = SUM + monthlyRatings(targetMonth, interval)
        interval = interval + 1
    Loop

    'Calculate wind impact based on speed,
    'direction and other factors
    Select Case interval

        Case 10
            '0-9 mph
            windSpeed = Int(Rnd(1) * 10)
    
        Case 11
            '10-14 mph
            windSpeed = Int(Rnd(1) * 5) + 10
    
        Case 12
            '15-19 mph
            windSpeed = Int(Rnd(1) * 5) + 15
    
        Case 13
            '20-30 mph
            windSpeed = Int(Rnd(1) * 10) + 20

    End Select

    windFactor! = 1

    If windSpeed >= 15 Then

        If windDir > 3 And windDir < 7 Then
            'From infield - harder to hit
            windFactor! = 1 - windRating / 100
        End If

        If windDir > 6 And windDir < 10 Then
            'To outfield - improves hits
            windFactor! = 1 + windRating / 100
        End If

    End If

    precipType = 4
    skyType = Int(Rnd(1) * 2) + 1

    'Compare # 1-100 against precip value
    'This is the % of time there is precip
    If d100% <= monthlyRatings(targetMonth, 14) Then

        precipType = Int(Rnd(1) * 3) + 1
        skyType = 3

        If d100% <= 5 Then

            'Game delayed from rain
            'Rain starts from inning 6 to 8
            rainDelay = 1
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
Sub LINSWAP (teamIdx)

    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

    Do
        I$ = GetKeyPress$
        K1 = Val(I$)
    Loop Until K1 >= 1 And K1 <= 9 Or fldPos(teamIdx, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or fldPos(teamIdx, K2) <> 1

    Swap fldPos(teamIdx, K2), fldPos(teamIdx, K1)
    Swap lineupPlayer(teamIdx, K1), lineupPlayer(teamIdx, K2)

    fielder(teamIdx, fldPos(teamIdx, K1)) = lineupPlayer(teamIdx, K1)
    fielder(teamIdx, fldPos(teamIdx, K2)) = lineupPlayer(teamIdx, K2)

    Call NEWLINES(teamIdx)

    Locate 18, 59: Print Space$(17)
    Locate 19, 59: Print Space$(17)

End Sub


'----------------------------------------
'          NEWLINES Subroutine
'----------------------------------------
' ...explanation...
Sub NEWLINES (teamIdx)

    For I = 1 To 9

        Call FieldPosCursor(I)

        Print Using "# "; I;

        If fldPos(teamIdx, I) <> 1 Then
            Print batters$(teamIdx, lineupPlayer(teamIdx, I));
        Else
            If I5 <> 66 Then
                Print pitchers$(teamIdx, lineupPlayer(teamIdx, I));
            End If
        End If

        Print Tab(74);

        Print fieldPos$(fldPos(teamIdx, I));

    Next I

    I5 = 0

End Sub


'----------------------------------------
'        FieldPosCursor Subroutine
'----------------------------------------
' This subroutine "positions" the cursor
'on the screen at the appropriate row
'in order to print out the line positions.
Sub FieldPosCursor (posIdx)

    Locate posIdx + 7, 59

End Sub

'----------------------------------------
'           POSSWAP Subroutine
'----------------------------------------
' ...explanation...
Sub POSSWAP (teamIdx)

    Locate 18, 59: Print "B.O. PLYR 1 (1-9)";

    Do
        I$ = GetKeyPress$
        K1 = Val(I$)
    Loop Until K1 >= 1 And K1 <= 9 Or fldPos(teamIdx, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or fldPos(teamIdx, K2) <> 1

    Swap fldPos(teamIdx, K2), fldPos(teamIdx, K1)

    fielder(teamIdx, fldPos(teamIdx, K1)) = lineupPlayer(teamIdx, K1)
    fielder(teamIdx, fldPos(teamIdx, K2)) = lineupPlayer(teamIdx, K2)

    Call NEWLINES(teamIdx)

    Locate 18, 59: Print Space$(17)
    Locate 19, 59: Print Space$(17)

End Sub


'----------------------------------------
'         SHOWBATTERS Subroutine
'----------------------------------------
'This subroutine provides a summary dis-
'play of the batting averages
Sub SHOWBATTERS (teamIdx)

    Cls

    Color 14
    Print "#";
    Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12): Print gameTeams$(teamIdx);

    Color 14, 0

    Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"

    Locate 2

    For I = 0 To 22:

        Locate , 1

        If batters$(teamIdx, I) = "XXX" Or Left$(batters$(teamIdx, I), 1) = " " Or batters$(teamIdx, I) = "" Then
            batRating(teamIdx, I, SS_B_Avail) = 98
        Else
            Color 2, 0

            If I < 10 Then Print Using "#"; I;
            If I > 9 Then Print alpha$(I - 10);

            Color 15, 0
            Call ColorCheck(teamIdx, I)

            'Name / Position
            Locate , 3: Print batters$(teamIdx, I);
            Color 15, 0
            Locate , 15: Print handed$(batRating(teamIdx, I, SS_B_Bats) + 2);

            Locate , 18: If batRating(teamIdx, I, SS_B_Pos1) <> -1 Then Print Using "#"; batRating(teamIdx, I, SS_B_Pos1);
            Locate , 19: If batRating(teamIdx, I, SS_B_Pos2) <> -1 Then Print Using "#"; batRating(teamIdx, I, SS_B_Pos2);
            Locate , 20: If batRating(teamIdx, I, SS_B_Pos3) <> -1 Then Print Using "#"; batRating(teamIdx, I, SS_B_Pos3);
            Locate , 21: If batRating(teamIdx, I, SS_B_Pos4) <> -1 Then Print Using "#"; batRating(teamIdx, I, SS_B_Pos4);

            Locate , 23

            Print Using "### ## ## ## ### ###"; batRating(teamIdx, I, SS_B_AB); batRating(teamIdx, I, SS_B_2B); batRating(teamIdx, I, SS_B_3B); batRating(teamIdx, I, SS_B_HR); batRating(teamIdx, I, SS_B_W); batRating(teamIdx, I, SS_B_K);

            Locate , 44: Print Using "#"; batRating(teamIdx, I, SS_B_Run);
            Print Using "#"; batRating(teamIdx, I, SS_B_Range1);
            Print Using "#"; batRating(teamIdx, I, SS_B_Arm1);

            Locate , 47
            '           SB   G%
            Print Using "### ##"; batRating(teamIdx, I, SS_B_SB); batRating(teamIdx, I, SS_B_GndPct);

            'BA
            If batRating(teamIdx, I, SS_B_H) = batRating(teamIdx, I, SS_B_AB) And batRating(teamIdx, I, SS_B_H) <> 0 Then
                Print "1.000";
            Else
                If batRating(teamIdx, I, SS_B_AB) = 0 Then
                    Print " .---";
                Else
                    bAvg! = batRating(teamIdx, I, SS_B_H) / batRating(teamIdx, I, SS_B_AB)
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
Sub SHOWFIELD (teamIdx)

    Locate 1, 15
    Color 14, 0
    Print "P1 FAVG A R P2 FAVG A R P3 FAVG A R P4 FAVG A R                 "

    Color 15, 0
    Locate 2

    For I = 0 To 22

        Locate , 15: Print Space$(64);

        If batters$(teamIdx, I) <> "XXX" And Left$(batters$(teamIdx, I), 1) <> " " And batters$(teamIdx, I) <> "" Then

            Locate , 16
            If batRating(teamIdx, I, SS_B_Pos1) <> -1 Then
                Print Using "# #### # #"; batRating(teamIdx, I, SS_B_Pos1); batRating(teamIdx, I, SS_B_FldPct1); batRating(teamIdx, I, SS_B_Arm1); batRating(teamIdx, I, SS_B_Range1);
            End If

            Locate , 28
            If batRating(teamIdx, I, SS_B_Pos2) <> -1 Then
                Print Using "# #### # #"; batRating(teamIdx, I, SS_B_Pos2); batRating(teamIdx, I, SS_B_FldPct2); batRating(teamIdx, I, SS_B_Arm2); batRating(teamIdx, I, SS_B_Range2);
            End If

            Locate , 40
            If batRating(teamIdx, I, SS_B_Pos3) <> -1 Then
                Print Using "# #### # #"; batRating(teamIdx, I, SS_B_Pos3); batRating(teamIdx, I, SS_B_FldPct3); batRating(teamIdx, I, SS_B_Arm3); batRating(teamIdx, I, SS_B_Range3);
            End If

            Locate , 52
            If batRating(teamIdx, I, SS_B_Pos4) <> -1 Then
                Print Using "# #### # #"; batRating(teamIdx, I, SS_B_Pos4); batRating(teamIdx, I, SS_B_FldPct4); batRating(teamIdx, I, SS_B_Arm4); batRating(teamIdx, I, SS_B_Range4);
            End If

            If I <> 22 Then Print

        End If

    Next I

End Sub

'----------------------------------------
'          ShowSplit Subroutine
'----------------------------------------
'This subroutine presents a split view of
'team stats
Sub ShowSplit (teamIdx)

    'DISPLAY SPLIT STATS
    Locate 1, 15
    Color 14, 0

    Print "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
    Color 15, 0

    Locate 2

    For I = 0 To 22

        Locate , 15: Print "                                                                ";

        If batters$(teamIdx, I) <> "XXX" And Left$(batters$(teamIdx, I), 1) <> " " And batters$(teamIdx, I) <> "" Then

            Locate , 20: Print Using "### ## ## ## ### ### "; batRating(teamIdx, I, SS_B_LHP_AB); batRating(teamIdx, I, 44); batRating(teamIdx, I, 45); batRating(teamIdx, I, 46); batRating(teamIdx, I, 47); batRating(teamIdx, I, 48);

            If batRating(teamIdx, I, SS_B_LHP_AB) = 0 Or batRating(teamIdx, I, SS_B_LHP_H) = 0 Then
                Print " .000";
            Else
                If batRating(teamIdx, I, SS_B_LHP_AB) = batRating(teamIdx, I, SS_B_LHP_H) Then
                    Print "1.000";
                Else
                    Print Using " .###"; batRating(teamIdx, I, SS_B_LHP_H) / batRating(teamIdx, I, SS_B_LHP_AB);
                End If
            End If

            Locate , 53: Print Using "### ## ## ## ### ### "; batRating(teamIdx, I, SS_B_RHP_AB); batRating(teamIdx, I, 51); batRating(teamIdx, I, 52); batRating(teamIdx, I, 53); batRating(teamIdx, I, 54); batRating(teamIdx, I, 55);

            'AVG
            If batRating(teamIdx, I, SS_B_RHP_H) = 0 Or batRating(teamIdx, I, SS_B_RHP_H) = 0 Then
                Print " .000";
            Else
                If batRating(teamIdx, I, SS_B_RHP_AB) = batRating(teamIdx, I, SS_B_RHP_H) Then
                    Print "1.000";
                Else
                    Print " ."; Using "###"; batRating(teamIdx, I, SS_B_RHP_H) / batRating(teamIdx, I, SS_B_RHP_AB);
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
        Call DisplayBallField(nbrBalls, nbrStrikes)
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
                    Input #1, oppBatStats(I, I1, I2)
                Next
            Next

            For I1 = 0 To 21:
                For I2 = 0 To 41:
                    Input #1, oppPitStats(I, I1, I2)
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

        'Score / period conditions for special box score
        If INNING% > eventSettings(1) Or _
            gameScore(1, 0) < eventSettings(2) Or _ 
            gameScore(0, 0) < eventSettings(2) Or _ 
            gameScore(1, 0) > eventSettings(3) Or _ 
            gameScore(0, 0) > eventSettings(3) Or _ 
            gameScore(1, 1) > eventSettings(5) Or _ 
            gameScore(0, 1) > eventSettings(5) Or _ 
            gameScore(1, 1) < eventSettings(4) Or _
            gameScore(0, 1) < eventSettings(4) Then

            checkSpecial% = 1

        End If

        If checkSpecial% <> 1 Then

            I = 0

            Do While I <= 1 And checkSpecial% = 0

                I1 = 0

                Do While I1 <= 22 And checkSpecial% = 0

                    'Batter stat conditions for special box score
                    If teamBatStats(I, I1, 1) > eventSettings(6) Or _
                        teamBatStats(I, I1, 2) > eventSettings(7) Or _
                        teamBatStats(I, I1, 3) > eventSettings(8) Or _
                        teamBatStats(I, I1, 4) > eventSettings(9) Or _
                        teamBatStats(I, I1, 5) > eventSettings(10) Or _
                        teamBatStats(I, I1, 6) > eventSettings(11) Or _
                        teamBatStats(I, I1, 9) > eventSettings(12) Then
                        checkSpecial% = 1
                    End If

                    I1 = I1 + 1

                Loop

                I = I + 1

            Loop

            'Still haven't found? Keep looking
            If checkSpecial% <> 1 Then

                I = 0

                Do While I <= 1 And checkSpecial% = 0

                    I1 = 0

                    Do While I1 <= 21 And checkSpecial% = 0

                        'Pitcher stat conditions for special box score
                        If teamPitStats(I, I1, 5) > eventSettings(13) Then
                            checkSpecial% = 1
                        End If

                        I1 = I1 + 1

                    Loop

                    I = I + 1

                Loop

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
    Shared eventNbr
    Shared allEvents
    Shared doublesPlayer$(), eventDesc$(), homeRuns$()
    Shared stolenBases$(), triplesPlayer$()

    Cls
    Locate LAST_ROW / 2, 33
    Print "CHECKING RECORDS..."

    For I = 0 To 1:
        For I1 = 0 To 21:
            If teamPitStats(I, I1, 1) < 0 Then
                teamPitStats(I, I1, 1) = 0
            End If
        Next I1
    Next I

    For I = 0 To 1:

        For I1 = 0 To 21
            If pitchRating(I, I1, SS_P_Avail) = 1 Then
                If teamPitStats(I, I1, 0) >= 18 And teamPitStats(I, I1, 3) <= 3 Then
                    teamPitStats(I, I1, LG_P_QS) = 1
                End If
            End If
        Next I1

    Next I

    eventNbr = 0

    For I = 0 To 1:

        For I1 = 0 To 22

            If teamBatStats(I, I1, 11) <> 0 Then

                ' ***CHECKING ERRORS***
                If teamBatStats(I, I1, 11) = 1 Then
                    eventDesc$(eventNbr) = batters$(I, I1)
                Else
                    eventDesc$(eventNbr) = batters$(I, I1) + Str$(teamBatStats(I, I1, 11))
                End If

                eventDesc$(eventNbr) = eventDesc$(eventNbr) + " (" + Right$(Str$(oppBatStats(I, I1, 11)), (Len(Str$(oppBatStats(I, I1, 11))) - 1)) + ")"

                eventNbr = eventNbr + 1

            End If

            ' ***CHECKING DOUBLES***
            If teamBatStats(I, I1, 4) <> 0 Then
                If teamBatStats(I, I1, 4) = 1 Then
                    doublesPlayer$(DB) = batters$(I, I1)
                Else
                    doublesPlayer$(DB) = batters$(I, I1) + Str$(teamBatStats(I, I1, 4))
                End If

                doublesPlayer$(DB) = doublesPlayer$(DB) + " (" + Right$(Str$(oppBatStats(I, I1, 4)), (Len(Str$(oppBatStats(I, I1, 4))) - 1)) + ")"
                DB = DB + 1

            End If

            ' ***CHECKING TRIPLES***
            If teamBatStats(I, I1, 5) <> 0 Then
                If teamBatStats(I, I1, 5) = 1 Then
                    triplesPlayer$(TR) = batters$(I, I1)
                Else
                    triplesPlayer$(TR) = batters$(I, I1) + Str$(teamBatStats(I, I1, 5))
                End If

                triplesPlayer$(TR) = triplesPlayer$(TR) + " (" + Right$(Str$(oppBatStats(I, I1, 5)), (Len(Str$(oppBatStats(I, I1, 5))) - 1)) + ")"
                TR = TR + 1

            End If

            ' ***CHECKING HOME RUNS***
            If teamBatStats(I, I1, 6) <> 0 Then
                If teamBatStats(I, I1, 6) = 1 Then
                    homeRuns$(HR) = batters$(I, I1)
                Else
                    homeRuns$(HR) = batters$(I, I1) + Str$(teamBatStats(I, I1, 6))
                End If

                homeRuns$(HR) = homeRuns$(HR) + " (" + Right$(Str$(oppBatStats(I, I1, 6)), (Len(Str$(oppBatStats(I, I1, 6))) - 1)) + ")"
                HR = HR + 1

            End If

            ' ***CHECKING STOLEN BASES ***
            If teamBatStats(I, I1, 9) <> 0 Then

                If teamBatStats(I, I1, 9) = 1 Then
                    stolenBases$(SB) = batters$(I, I1) + " (" + Right$(Str$(oppBatStats(I, I1, 9)), (Len(Str$(oppBatStats(I, I1, 9))) - 1)) + ")"
                Else
                    stolenBases$(SB) = batters$(I, I1) + Str$(teamBatStats(I, I1, 9)) + " (" + Right$(Str$(oppBatStats(I, I1, 9)), (Len(Str$(oppBatStats(I, I1, 9))) - 1)) + ")"
                End If
                SB = SB + 1

            End If

        Next I1

    Next I

    For I = 0 To 1

        For I1 = 0 To 21

            If teamPitStats(I, I1, 34) <> 0 Then

                If teamPitStats(I, I1, 34) = 1 Then
                    eventDesc$(eventNbr) = pitchers$(I, I1)
                Else
                    eventDesc$(eventNbr) = pitchers$(I, I1) + Str$(teamPitStats(I, I1, 34))
                End If

                eventDesc$(eventNbr) = eventDesc$(eventNbr) + " (" + Right$(Str$(oppPitStats(I, I1, 34)), (Len(Str$(oppPitStats(I, I1, 34))) - 1)) + ")"
                eventNbr = eventNbr + 1

            End If

            ' ***CHECKING DOUBLES***
            If teamPitStats(I, I1, 27) <> 0 Then

                If teamPitStats(I, I1, 27) = 1 Then
                    doublesPlayer$(DB) = pitchers$(I, I1)
                Else
                    doublesPlayer$(DB) = pitchers$(I, I1) + Str$(teamPitStats(I, I1, 27))
                End If

                doublesPlayer$(DB) = doublesPlayer$(DB) + " (" + Right$(Str$(oppPitStats(I, I1, 27)), (Len(Str$(oppPitStats(I, I1, 27))) - 1)) + ")"
                DB = DB + 1

            End If

            ' ***CHECKING TRIPLES***
            If teamPitStats(I, I1, 28) <> 0 Then

                If teamPitStats(I, I1, 28) = 1 Then
                    triplesPlayer$(TR) = pitchers$(I, I1)
                Else
                    triplesPlayer$(TR) = pitchers$(I, I1) + Str$(teamPitStats(I, I1, 28))
                End If

                triplesPlayer$(TR) = triplesPlayer$(TR) + " (" + Right$(Str$(oppPitStats(I, I1, 28)), (Len(Str$(oppPitStats(I, I1, 28))) - 1)) + ")"
                TR = TR + 1

            End If

            ' ***CHECKING HOME RUNS***
            If teamPitStats(I, I1, 29) <> 0 Then

                If teamPitStats(I, I1, 29) = 1 Then
                    homeRuns$(HR) = pitchers$(I, I1)
                Else
                    homeRuns$(HR) = pitchers$(I, I1) + Str$(teamPitStats(I, I1, 29))
                End If

                homeRuns$(HR) = homeRuns$(HR) + " (" + Right$(Str$(oppPitStats(I, I1, 29)), (Len(Str$(oppPitStats(I, I1, 29))) - 1)) + ")"
                HR = HR + 1

            End If

            ' ***CHECKING STOLEN BASES ***
            If teamPitStats(I, I1, 32) <> 0 Then

                If teamPitStats(I, I1, 32) = 1 Then
                    stolenBases$(SB) = pitchers$(I, I1) + " (" + Right$(Str$(oppPitStats(I, I1, 32)), (Len(Str$(oppPitStats(I, I1, 32))) - 1)) + ")"
                Else
                    stolenBases$(SB) = pitchers$(I, I1) + Str$(teamPitStats(I, I1, 32)) + " (" + Right$(Str$(oppPitStats(I, I1, 32)), (Len(Str$(oppPitStats(I, I1, 32))) - 1)) + ")"
                End If
                SB = SB + 1

            End If

        Next I1
    Next I

    'This is the length of the game
    'Ends up being anywhere between 2 and 3 hours
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
                teamBatStats(I, I1, 14) = 1
            End If
        Next I1

    Next I


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

                If gameRoster(I, 2, I6) > gameRoster(I, 2, I6 + 1) Then

                    H1 = gameRoster(I, 2, I6)
                    H2 = gameRoster(I, 1, I6)
                    H3 = gameRoster(I, 0, I6)

                    gameRoster(I, 0, I6) = gameRoster(I, 0, I6 + 1)
                    gameRoster(I, 1, I6) = gameRoster(I, 1, I6 + 1)
                    gameRoster(I, 2, I6) = gameRoster(I, 2, I6 + 1)

                    gameRoster(I, 0, I6 + 1) = H3
                    gameRoster(I, 1, I6 + 1) = H2
                    gameRoster(I, 2, I6 + 1) = H1
                    I7 = I6

                End If

                I6 = I6 + 1

            Loop While I6 <= L

            If I7 <> 0 And I7 > 0 Then ' Ensure I7 is positive
                L = I7 - 1
            Else
                I7 = 0 ' Force exit if no valid swaps
            End If

        Loop While I7 <> 0

    Next I

End Sub


'----------------------------------------
'         InningColumn_Box routine
'----------------------------------------
'Locates cursor in the console output
'for displaying the current inning
Sub InningColumn_Box (currInning)

    Dim setOf9, posInSet, col

    ' Each set of 9 positions repeats the same column pattern
    setOf9 = (currInning - 1) \ 9 ' Which set: 0, 1, 2...
    posInSet = (currInning - 1) Mod 9 ' Position in set: 0-8

    ' Calculate base column based on set
    If setOf9 = 0 Then
        ' First set uses 2x multiplier
        col = 2 * (posInSet + 1) + 17 + (posInSet \ 3)
    Else
        ' Later sets use 3x multiplier
        col = 3 * (posInSet + 1) + 17 + (posInSet \ 3) + (setOf9 * 27)
    End If

    Locate , col

End Sub


'----------------------------------------
'        InningColumn_File routine
'----------------------------------------
'Locates cursor in printer / file output
'for displaying the current inning
Sub InningColumn_File (currInning)

    Dim setOf9, posInSet, fileCol

    setOf9 = (currInning - 1) \ 9 ' Which set: 0, 1, 2...
    posInSet = (currInning - 1) Mod 9 ' Position in set: 0-8

    If setOf9 = 0 Then
        ' First set (currInning 1-9): 2x multiplier, base 15, +1 every 3 positions
        fileCol = 2 * (posInSet + 1) + 14 + (posInSet \ 3)
    Else
        ' Later sets (currInning 10+): 3x multiplier with incrementing base
        fileCol = 3 * currInning + 7 + setOf9
    End If

    Print #2, Tab(fileCol + 1);

End Sub


'----------------------------------------
'     LeadRunnerCountdown Subroutine
'----------------------------------------
Sub LeadRunnerCountdown (INFPOS%, currFielder, wasError, baseStatus)

    If baseStatus = 7 Or baseStatus = 6 Or baseStatus = 5 Or baseStatus = 3 Then

        baseMove(3) = 11

        For I = 0 To 2:
            baseMove(I) = 1
        Next

    End If

    If baseStatus = 2 Or baseStatus = 4 Then
        baseMove(0) = 1
        baseMove(1) = 1
        baseMove(2) = 11
    End If

    If baseStatus = 1 Then
        baseMove(0) = 1
        baseMove(1) = 11
    End If

    If baseStatus = 0 Then
        baseMove(0) = 11
    End If

    If PLB = 0 Then
        Call PBP(player$(0) + " bunts it hard!")
        Call PBP(player$(4) + " makes an easy play")
    End If

    PLB = 0

    Call TrackOuts(1)

    Call Add_Strikeouts(P, D)
    Call CreditAssist(currFielder)
    Call ADVANCEMENT(S2%, INFPOS%, I3, currFielder, wasError, SAH%)
    Call CreditInfieldPutout(I3, FB2P%, AST%)

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
        If pitcherOfRecord(I3) <> currPitcher(I3) And relieverOnRecordForSave(I3) = -1 And teamPitStats(I3, currPitcher(I3), 0) >= 9 And teamPitStats(I3, currPitcher(I3), 0) <= 12 And gamePitcher(I3) > 0 And teamPitStats(I3, currPitcher(I3), 3) < 3 Then
            relieverOnRecordForSave(I3) = currPitcher(I3)
        End If
        If pitcherOfRecord(I3) = relieverOnRecordForSave(I3) Then
            relieverOnRecordForSave(I3) = -1
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

        If (autoPlay = 1 And playerMode = 2) Then
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

        currTeam = 0
        teamWins = 0
        teamLosses = 0

        Do

            C% = 0

            If autoPlay = 1 Then
                C% = 0
            Else
                If X1 = 0 Or X1 = 1 And currTeam = 0 Or X1 = 2 And currTeam = 1 Then
                    Cls
                Else
                    If X1 = 2 And currTeam = 0 Or X1 = 1 And currTeam = 1 Then
                        C% = 1
                    End If
                End If
            End If

            If C% <> 1 Then

                Call COMPILESTATFILES(currTeam)

            End If

            currTeam = currTeam + 1

        Loop Until currTeam = 2

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

    Shared statBatter(), statPitcher(), statSplit()
    Shared statTeamBat!(), statTeamPit!()

    Shared statTeamDP, statOppDP

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

                Select Case teamPitStats(teamIdx, I1, 0)

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
                        gameDaysRest(teamIdx, I1) = Int(teamPitStats(teamIdx, I1, 0) / 3) + 1 - 2

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
            Input #1, gameAtBats(teamIdx, I)
        Next I

        For I = 0 To 21:
            Input #1, pitchers$(teamIdx, I)
            Input #1, gameInnPit(teamIdx, I)
        Next I

        For I = 0 To 22:
            For I1 = 0 To 21:
                Input #1, statBatter(I, I1)
            Next I1
        Next I

        For I = 0 To 21:
            For I1 = 0 To 41:
                Input #1, statPitcher(I, I1)
            Next I1
        Next I

        Input #1, statTeamDP
        Input #1, statOppDP

        For I = 0 To 21:
            Input #1, statTeamBat!(I)
        Next I

        For I = 0 To 22:
            Input #1, statTeamPit!(I)
        Next I

        Close #1

    End If

    For I = 0 To 22

        gameAtBats(teamIdx, I) = batRating(teamIdx, I, SS_B_AB)

        For I1 = 0 To 11
            statBatter(I, I1) = statBatter(I, I1) + teamBatStats(teamIdx, I, I1)
            statTeamBat!(I1) = statTeamBat!(I1) + teamBatStats(teamIdx, I, I1)
        Next I1

        'Curr Hit Streak
        If teamBatStats(teamIdx, I, 0) > 0 Then

            If teamBatStats(teamIdx, I, 1) = 0 Then
                statBatter(I, 12) = 0
            Else
                statBatter(I, 12) = statBatter(I, 12) + 1
            End If

        End If

        'Games
        If teamBatStats(teamIdx, I, 14) > 0 Then
            statBatter(I, 14) = statBatter(I, 14) + 1
        End If

        For I1 = 15 To 21
            statBatter(I, I1) = statBatter(I, I1) + teamBatStats(teamIdx, I, I1)
            statTeamBat!(I1) = statTeamBat!(I1) + teamBatStats(teamIdx, I, I1)
        Next I1

        'Curr Hit Streak vs Long Hit Streak.
        If statBatter(I, 12) >= statBatter(I, 13) Then
            statBatter(I, 13) = statBatter(I, 12)
        End If

    Next I

    For I = 0 To 21

        For I1 = 23 To 34
            statPitcher(I, I1) = statPitcher(I, I1) + teamPitStats(teamIdx, I, I1)
            statTeamBat!(I1 - 23) = statTeamBat!(I1 - 23) + teamPitStats(teamIdx, I, I1)
        Next I1

        If teamPitStats(teamIdx, I, 23) > 0 Then
            If teamPitStats(teamIdx, I, 24) = 0 Then
                statPitcher(I, 35) = 0
            Else
                statPitcher(I, 35) = statPitcher(I, 35) + 1
            End If
        End If

        For I1 = 37 To 41
            statPitcher(I, I1) = statPitcher(I, I1) + teamPitStats(teamIdx, I, I1)
            statTeamBat!(I1 - 22) = statTeamBat!(I1 - 22) + teamPitStats(teamIdx, I, I1)
        Next I1

        If statPitcher(I, 35) >= statPitcher(I, 36) Then
            statPitcher(I, 36) = statPitcher(I, 35)
        End If

    Next I

    statTeamBat!(12) = statTeamBat!(12) + leftOnBase(teamIdx)
    INNING% = 0

    For I = 0 To 21
        INNING% = INNING% + teamPitStats(teamIdx, I, 0)
        gameInnPit(teamIdx, I) = pitchRating(teamIdx, I, SS_P_IP)

        For I1 = 0 To 5
            statPitcher(I, I1) = statPitcher(I, I1) + teamPitStats(teamIdx, I, I1)
            statTeamPit!(I1) = statTeamPit!(I1) + teamPitStats(teamIdx, I, I1)
        Next I1

        For I1 = 13 To 22:
            statPitcher(I, I1) = statPitcher(I, I1) + teamPitStats(teamIdx, I, I1)
            statTeamPit!(I1) = statTeamPit!(I1) + teamPitStats(teamIdx, I, I1)
        Next I1

    Next I

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) > 0 Then
        'Team Win
        statPitcher(pitcherOfRecord(teamIdx), 6) = statPitcher(pitcherOfRecord(teamIdx), 6) + 1
    End If

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) < 0 Then
        'Team Loss
        statPitcher(pitcherOfRecord(teamIdx), 7) = statPitcher(pitcherOfRecord(teamIdx), 7) + 1
    End If

    If gameScore(teamIdx, 0) - gameScore(1 - teamIdx, 0) > 0 And relieverOnRecordForSave(teamIdx) >= 0 Then
        'Team Win + relieverOnRecordForSave ??
        statPitcher(relieverOnRecordForSave(teamIdx), 12) = statPitcher(relieverOnRecordForSave(teamIdx), 12) + 1
        statTeamPit!(12) = statTeamPit!(12) + 1
    End If

    For I = 0 To 21

        If pitchRating(teamIdx, I, SS_P_Avail) <> 0 Then
            statPitcher(I, 8) = statPitcher(I, 8) + 1
            statTeamPit!(8) = statTeamPit!(8) + 1
        End If

        If INNING% = teamPitStats(teamIdx, I, 0) Then
            statPitcher(I, 10) = statPitcher(I, 10) + 1
            statTeamPit!(10) = statTeamPit!(10) + 1
        End If

        If INNING% = teamPitStats(teamIdx, I, 0) And gameScore(1 - teamIdx, 0) = 0 Then
            statPitcher(I, 11) = statPitcher(I, 11) + 1
            statTeamPit!(11) = statTeamPit!(11) + 1
        End If

    Next I

    statPitcher(SX%(teamIdx), 9) = statPitcher(SX%(teamIdx), 9) + 1
    statTeamPit!(9) = statTeamPit!(9) + 1

    statTeamDP = statTeamDP + teamDPs(teamIdx)
    statOppDP = statOppDP + teamDPs(1 - teamIdx)

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
        Print #1, gameAtBats(teamIdx, I)
    Next I

    For I = 0 To 21:
        Print #1, pitchers$(teamIdx, I)
        Print #1, gameInnPit(teamIdx, I)
    Next I

    For I = 0 To 22:
        For I1 = 0 To 21:
            Print #1, statBatter(I, I1)
        Next I1
    Next I

    For I = 0 To 21:
        For I1 = 0 To 41:
            Print #1, statPitcher(I, I1)
        Next I1
    Next I

    Print #1, statTeamDP
    Print #1, statOppDP

    For I = 0 To 21:
        Print #1, statTeamBat!(I)
    Next I

    For I = 0 To 22:
        Print #1, statTeamPit!(I)
    Next I

    Close #1

    Print "<<<<<"
    Print
    Print

    If _FileExists(diskPaths$(1) + statFile$ + "R") Then

        Open diskPaths$(1) + statFile$ + "R" For Input As #1

        For I = 0 To 22
            For I1 = 0 To 6
                Input #1, statSplit(I, I1)
            Next
        Next

        Close 1

    End If

    For I = 0 To 22
        For I1 = 0 To 6
            statSplit(I, I1) = statSplit(I, I1) + CSS(teamIdx, I, I1)
        Next
    Next

    Open diskPaths$(1) + statFile$ + "R" For Output As #1

    For I = 0 To 22
        For I1 = 0 To 6
            Print #1, statSplit(I, I1)
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
    nbrInnings(numberGames) = inningsPitched

    Call SaveScheduleStats(statFile$, numberGames)

    numberGames = 0

    Erase gameAtBats, gameInnPit
    Erase statBatter, statPitcher
    Erase statSplit, statTeamBat!, statTeamPit!

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

    Shared doublesPlayer$(), eventDesc$(), homeRuns$()
    Shared stolenBases$(), triplesPlayer$()

    Dim BK$
    BK$ = "## # # ## # #  # # #  #  # # ## ##"

    Open diskPaths$(1) + boxName$ For Append As #2

    park$ = RTrim$(gameStadiums$(1))
    XCoord = (80 - Len(park$)) / 2

    Print #2, Tab(XCoord); park$
    Print #2, Tab(35); "TIME: "; gameTime$
    Print #2, Tab(30); "ATTENDANCE: ";: Print #2, Using "###,###"; actualAttendance&
    Print #2, Tab(32); "TEMPERATURE:"; gameTemp
    Print #2,
    If teamRat_GAME(1, 9) = 1 Then
        Print #2, "WIND: NONE"
        Print #2, "WEATHER: DOME"
    Else
        Print #2, "WIND: "; wind$
        Print #2, "SKY: "; skyCond$(skyType)
        Print #2, "PRECIPITATION: "; precip$(precipType)
    End If

    Print #2,

    Print #2, Tab(17);

    For IG = 1 To INNING%

        If IG < 10 Then
            Print #2, Using " #"; IG;
        Else
            Print #2, Using " ##"; IG;
        End If

        If (IG Mod 3 = 0) Then Print #2, " ";

    Next IG

    Print #2, "   R  H E"

    If INNING% < 9 Then

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 1 To INNING%
                Call InningColumn_File(I2)
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
                Call InningColumn_File(I2)
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
                Call InningColumn_File(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            'If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1

            'Prints innings 10 to 19
            For I2 = 10 To INNING%
                Call InningColumn_File(I2)
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
                Call InningColumn_File(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            'If INNING% = 28 Or INNING% < 28 And P = 0 Then INNING% = INNING% - 1

            For I2 = 10 To 18
                Call InningColumn_File(I2)
                Print #2, Using "##"; inningScore(I, I2);
            Next I2

            Print #2,

        Next I

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 19 To INNING%
                Call InningColumn_File(I2)
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

            If gameRoster(I, 0, I3) <> -1 And gameRoster(I, 1, I3) <> 1 Then

                I1 = gameRoster(I, 0, I3)

                Print #2, batters$(I, I1); ", "; fieldPos$(gameRoster(I, 1, I3)); Tab(18);
                '                    AB             R              H              BI             2              3              HR             K              W              SB             CS              E               PO              A
                Print #2, Using BK$; teamBatStats(I, I1, 0); teamBatStats(I, I1, 2); teamBatStats(I, I1, 1); teamBatStats(I, I1, 3); teamBatStats(I, I1, 4); teamBatStats(I, I1, 5); teamBatStats(I, I1, 6); teamBatStats(I, I1, 8); teamBatStats(I, I1, 7); teamBatStats(I, I1, 9); teamBatStats(I, I1, 10); teamBatStats(I, I1, 11); teamBatStats(I, I1, 16); teamBatStats(I, I1, 17);

                Print #2, "   ";

                'BA
                If oppBatStats(I, I1, 0) = 0 Or oppBatStats(I, I1, 1) = 0 Then
                    Print #2, " .000   ";
                Else
                    If oppBatStats(I, I1, 0) = oppBatStats(I, I1, 1) Then
                        Print #2, "1.000   ";
                    Else
                        Print #2, " ."; Using "###   "; (oppBatStats(I, I1, 1) / oppBatStats(I, I1, 0) * 1000);
                    End If
                End If

                'HR / RBI
                Print #2, Using "##  ###"; oppBatStats(I, I1, 6); oppBatStats(I, I1, 3)

                For I2 = 0 To 17
                    totalStats(I2) = totalStats(I2) + teamBatStats(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21:
            For I2 = 0 To 13:
                totalStats(I2) = totalStats(I2) + teamPitStats(I, I1, I2 + 23)
            Next

            For I2 = 14 To 17:
                totalStats(I2) = totalStats(I2) + teamPitStats(I, I1, I2 + 22)
            Next
        Next

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then

                    Print #2, pitchers$(I, I2); ", P";
                    Print #2, Tab(18);
                    Print #2, Using BK$; teamPitStats(I, I2, 23); teamPitStats(I, I2, 25); teamPitStats(I, I2, 24); teamPitStats(I, I2, 26); teamPitStats(I, I2, 27); teamPitStats(I, I2, 28); teamPitStats(I, I2, 29); teamPitStats(I, I2, 31); teamPitStats(I, I2, 30); teamPitStats(I, I2, 32); teamPitStats(I, I2, 33); teamPitStats(I, I2, 34); teamPitStats(I, I2, 38); teamPitStats(I, I2, 39);
                    Print #2, "   ";

                    If oppPitStats(I, I2, 24) = 0 Or oppPitStats(I, I2, 23) = 0 Then
                        Print #2, " .000   ";
                    Else
                        If oppPitStats(I, I2, 24) = oppPitStats(I, I2, 23) Then
                            Print #2, "1.000   ";
                        Else
                            Print #2, Using " .###   "; oppPitStats(I, I2, 24) / oppPitStats(I, I2, 23);
                        End If
                    End If

                    Print #2, Using "##  ###"; oppPitStats(I, I2, 29); oppPitStats(I, I2, 26)
                End If
            Next

        Next

        Print #2, "TOTALS"; Tab(17); totalStats(0); Tab(22); totalStats(1); Tab(27); totalStats(4);
        Print #2, Tab(32); totalStats(6); Tab(36); totalStats(7); Tab(42);
        Print #2, totalStats(10); Tab(46); totalStats(16); Tab(50); totalStats(17)
        Print #2, Tab(20); totalStats(2); Tab(24); totalStats(3); Tab(29);
        Print #2, totalStats(5); Tab(34); totalStats(8); Tab(39); totalStats(9);
        Print #2, Tab(44); totalStats(11)
        Print #2,

        Erase totalStats

    Next I

    For I = 0 To 1

        Print #2,: Print #2, gameTeams$(I); Tab(23); "       IP   H  R  ER BB SO    ERA   W  L  S  SO"

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then

                    I4 = 1 - I: Print #2, pitchers$(I, I2); " ";

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, "(W,"; Right$(Str$(oppPitStats(I, I2, 6) + 1), (Len(Str$(oppPitStats(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(oppPitStats(I, I2, 7)), (Len(Str$(oppPitStats(I, I2, 7))) - 1)); ")";
                        gameW$ = pitchers$(I, I2)
                    End If

                    If relieverOnRecordForSave(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, "(S,"; Right$(Str$(oppPitStats(I, I2, 12) + 1), (Len(Str$(oppPitStats(I, I2, 12) + 1)) - 1)); ")";
                    End If

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print #2, "(L,"; Right$(Str$(oppPitStats(I, I2, 6) + 0), (Len(Str$(oppPitStats(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(oppPitStats(I, I2, 7) + 1), (Len(Str$(oppPitStats(I, I2, 7) + 1)) - 1)); ")";
                        gameL$ = pitchers$(I, I2)
                    End If

                    If teamPitStats(I, I2, 19) > 0 Then
                        Print #2, "(BS,";: Print #2, Using "##"; oppPitStats(I, I2, 19);: Print #2, ")";
                    End If
                    If teamPitStats(I, I2, 22) > 0 Then
                        Print #2, "(H,";: Print #2, Using "##"; oppPitStats(I, I2, 22);: Print #2, ")";
                    End If

                    Print #2, Tab(29);: Print #2, Using "##"; Int(teamPitStats(I, I2, 0) / 3);

                    If teamPitStats(I, I2, 0) - (Int(teamPitStats(I, I2, 0) / 3) * 3) > 0 Then
                        Print #2, ".";: Print #2, Using "#"; teamPitStats(I, I2, 0) - (Int(teamPitStats(I, I2, 0) / 3) * 3);
                    End If

                    Print #2, Tab(34);
                    Print #2, Using "## ##  ## ## ##"; teamPitStats(I, I2, 1); teamPitStats(I, I2, 2); teamPitStats(I, I2, 3); teamPitStats(I, I2, 4); teamPitStats(I, I2, 5);
                    Print #2, "   ";

                    If oppPitStats(I, I2, 3) > 0 And oppPitStats(I, I2, 0) = 0 Then
                        Print #2, "99.99 ";
                    Else
                        If oppPitStats(I, I2, 0) = 0 Then
                            Print #2, "--.-- ";
                        Else
                            If oppPitStats(I, I2, 3) = 0 Then
                                Print #2, " 0.00 ";
                            Else
                                Print #2, Using "##.## "; oppPitStats(I, I2, 3) / (oppPitStats(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, Using "## "; oppPitStats(I, I2, 6) + 1;
                    Else
                        Print #2, Using "## "; oppPitStats(I, I2, 6);
                    End If
                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print #2, Using "## "; oppPitStats(I, I2, 7) + 1;
                    Else
                        Print #2, Using "## "; oppPitStats(I, I2, 7);
                    End If
                    If relieverOnRecordForSave(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print #2, Using "## "; oppPitStats(I, I2, 12) + 1;
                    Else
                        Print #2, Using "## "; oppPitStats(I, I2, 12);
                    End If

                    Print #2, Using "###"; oppPitStats(I, I2, 5)

                    For I4 = 0 To 5
                        totalStats(I4) = totalStats(I4) + teamPitStats(I, I2, I4)
                    Next I4
                End If

            Next I2

        Next I1

        Print #2, "TOTALS"; Tab(29);
        Print #2, Using "##"; Int(totalStats(0) / 3);

        If totalStats(0) - (Int(totalStats(0) / 3) * 3) > 0 Then
            Print #2, ".";
            Print #2, Using "#"; totalStats(0) - (Int(totalStats(0) / 3) * 3);
        End If

        Print #2, Tab(34);
        Print #2, Using "## ##  ## ## ##"; totalStats(1); totalStats(2); totalStats(3); totalStats(4); totalStats(5)

        inningsPitched = CInt(totalStats(0) / 3)

        For I1 = 0 To 5:
            totalStats(I1) = 0
        Next I1

    Next I

    Print #2,
    Print #2, " E - ";

    If eventNbr = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (eventNbr - 1):
            Print #2, eventDesc$(X1); ",";
        Next
        Print #2, eventDesc$(eventNbr)
    End If

    Print #2, "2B - ";

    If DB = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (DB - 1):
            Print #2, doublesPlayer$(X1); ",";
        Next
        Print #2, doublesPlayer$(DB)
    End If

    Print #2, "3B - ";
    If TR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (TR - 1):
            Print #2, triplesPlayer$(X1); ",";
        Next
        Print #2, triplesPlayer$(TR)
    End If

    Print #2, "HR - ";

    If HR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (HR - 1)
            Print #2, homeRuns$(X1); ",";
        Next
        Print #2, homeRuns$(HR)
    End If

    Print #2, "SB - ";

    If SB = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (SB - 1):
            Print #2, stolenBases$(X1); ",";
        Next
        Print #2, stolenBases$(SB)
    End If

    LOBText$ = gameTeams$(0) + " (" + Str$(leftOnBase(0)) + "), " + gameTeams$(1) + " (" + Str$(leftOnBase(1)) + ")"
    DPText$ = gameTeams$(0) + " (" + Str$(totalDPs(0)) + "), " + gameTeams$(1) + " (" + Str$(totalDPs(1)) + ")"
    Print #2, "LOB - "; LOBText$
    Print #2, "DP - "; DPText$

    Print #2,

    Print #2, "UMPIRES:"
    Print #2, "HP-"; gameUmpire$(1); "; 1B-"; gameUmpire$(2); "; 2B-"; gameUmpire$(3); "; 3B-"; gameUmpire$(4)

    Print #2,

    Print #2, "EJECTIONS:"

    If ejectCnt = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To (ejectCnt - 1):
            Print #2, ejections$(X1); ",";
        Next
        Print #2, ejections$(ejectCnt)
    End If

    Print #2,

    Print #2, "INJURIES: "

    If injCount = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To (injCount - 1)
            Print #2, injPlayers$(X1); ",";
        Next
        Print #2, injPlayers$(injCount)
        Print #2,
    End If

    Print #2,

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

    Call ScoreSummary

    If autoPlay <> 1 Or playerMode <> 2 Then
        Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
    End If

    Cls

    park$ = RTrim$(gameStadiums$(1))
    XCoord = (80 - Len(park$)) / 2

    Locate , XCoord: Print park$

    Color 14, 0
    Locate , 35: Print "TIME: ";
    Color 15: Print gameTime$
    Color 14
    Locate , 30: Print "ATTENDANCE: ";
    Color 15: Print Using "###,###"; actualAttendance&
    Color 14
    Locate , 32: Print "TEMPERATURE:";
    Color 15: Print gameTemp
    Print

    If teamRat_GAME(1, 9) = 1 Then
        Print "WIND: NONE"
        Print "WEATHER: DOME"
    Else
        Color 14: Print "WIND: ";
        Color 15: Print wind$
        Color 14: Print "SKY: ";
        Color 15: Print skyCond$(skyType)
        Color 14: Print "PRECIPITATION: ";
        Color 15: Print precip$(precipType)
    End If

    Print

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
            Call InningColumn_Box(IG)
            Print IG;
        Next IG

        Print "  H  R E"

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To INNING%

                Call InningColumn_Box(I2)

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
            Call InningColumn_Box(IG)
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
                Call InningColumn_Box(I2)
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
            Call InningColumn_Box(IG)
            Print IG;
        Next IG

        Print

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To 9
                Call InningColumn_Box(I2)
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
            Call InningColumn_Box(IG)
            Print IG;
        Next IG
        Print "  H  R E"

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 10 To INNING%
                Call InningColumn_Box(I2)
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
            Call InningColumn_Box(IG)
            Print IG;
        Next IG

        Print

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);

            Color 15, 0

            For I2 = 1 To 9
                Call InningColumn_Box(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print

        Next I

        Print

        'Print innings 10 to 18
        Locate 5,
        Color 14, 0
        For IG = 10 To 18
            Call InningColumn_Box(IG)
            Print IG;
        Next IG

        Print

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 10 To 18
                Call InningColumn_Box(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print

        Next I

        'Print innings 19 to 27
        Locate 8,
        Color 14, 0
        For IG = 19 To INNING%
            Call InningColumn_Box(IG)
            Print IG;
        Next IG

        Print "  H  R E"

        For I = 0 To 1

            Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 19 To INNING%
                Call InningColumn_Box(I2)
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

    Shared doublesPlayer$(), eventDesc$(), homeRuns$()
    Shared stolenBases$(), triplesPlayer$()

    Dim BK$
    BK$ = "## # # ## # #  # # #  #  # # ## ##"

    For I = 0 To 1

        Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
        Print gameTeams$(I);
        Color 14, 0
        Locate , 18: Print "AB R H BI 2 3 HR K W SB CS E PO  A      BA   HR  RBI"
        Color 15

        For I3 = 0 To 22

            If gameRoster(I, 0, I3) <> -1 And gameRoster(I, 1, I3) <> 1 Then

                I1 = gameRoster(I, 0, I3)

                Print batters$(I, I1); ", "; fieldPos$(gameRoster(I, 1, I3));
                Locate , 18
                '                AB             R              H              BI             2              3              HR             K              W              SB             CS              E               PO              A
                Print Using BK$; teamBatStats(I, I1, 0); teamBatStats(I, I1, 2); teamBatStats(I, I1, 1); teamBatStats(I, I1, 3); teamBatStats(I, I1, 4); teamBatStats(I, I1, 5); teamBatStats(I, I1, 6); teamBatStats(I, I1, 8); teamBatStats(I, I1, 7); teamBatStats(I, I1, 9); teamBatStats(I, I1, 10); teamBatStats(I, I1, 11); teamBatStats(I, I1, 16); teamBatStats(I, I1, 17);
                Print "   ";

                'BAVG
                If oppBatStats(I, I1, 0) = 0 Or oppBatStats(I, I1, 1) = 0 Then
                    Print " .000   ";
                Else
                    If oppBatStats(I, I1, 0) = oppBatStats(I, I1, 1) Then
                        Print "1.000   ";
                    Else
                        Print " ."; Using "###   "; (oppBatStats(I, I1, 1) / oppBatStats(I, I1, 0) * 1000);
                    End If
                End If

                '           HR   RBI
                Print Using "##  ###"; oppBatStats(I, I1, 6); oppBatStats(I, I1, 3)

                For I2 = 0 To 17
                    totalStats(I2) = totalStats(I2) + teamBatStats(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21:
            For I2 = 0 To 13:
                totalStats(I2) = totalStats(I2) + teamPitStats(I, I1, I2 + 23)
            Next

            For I2 = 14 To 17
                totalStats(I2) = totalStats(I2) + teamPitStats(I, I1, I2 + 22)
            Next
        Next

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then

                    Print pitchers$(I, I2); ", P";
                    Locate , 18
                    '                AB              R               H               BI              2               3               HR              K               W               SB              CS              E               PO              A
                    Print Using BK$; teamPitStats(I, I2, 23); teamPitStats(I, I2, 25); teamPitStats(I, I2, 24); teamPitStats(I, I2, 26); teamPitStats(I, I2, 27); teamPitStats(I, I2, 28); teamPitStats(I, I2, 29); teamPitStats(I, I2, 31); teamPitStats(I, I2, 30); teamPitStats(I, I2, 32); teamPitStats(I, I2, 33); teamPitStats(I, I2, 34); teamPitStats(I, I2, 38); teamPitStats(I, I2, 39);
                    Print "   ";

                    'BAVG
                    If oppPitStats(I, I2, 24) = 0 Or oppPitStats(I, I2, 23) = 0 Then
                        Print " .000   "
                    Else
                        If oppPitStats(I, I2, 24) = oppPitStats(I, I2, 23) Then
                            Print "1.000   ";
                        Else
                            Print Using "#.###   "; oppPitStats(I, I2, 24) / oppPitStats(I, I2, 23);
                        End If

                        '           HR   RBI
                        Print Using "##  ###"; oppPitStats(I, I2, 29); oppPitStats(I, I2, 26)

                    End If
                End If

            Next

        Next

        Color 14, 0: Print "TOTALS";
        Color 15
        Locate , 17: Print totalStats(0);
        Locate , 22: Print totalStats(1);
        Locate , 27: Print totalStats(4);
        Locate , 32: Print totalStats(6);
        Locate , 36: Print totalStats(7);
        Locate , 42: Print totalStats(10);
        Locate , 46: Print totalStats(16);
        Locate , 50: Print totalStats(17)
        Locate , 20: Print totalStats(2);
        Locate , 24: Print totalStats(3);
        Locate , 29: Print totalStats(5);
        Locate , 34: Print totalStats(8);
        Locate , 39: Print totalStats(9);
        Locate , 44: Print totalStats(11)

        Print

        If autoPlay <> 1 Or playerMode <> 2 Then
            Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

        Erase totalStats

        Cls

    Next I

 
    For I = 0 To 1

        Cls

        Color teamRat_GAME(I, 11), teamRat_GAME(I, 12)
        Print gameTeams$(I);

        Color 14: Locate , 30
        Print "IP   H  R  ER BB SO    ERA   W  L  S  SO"
        Color 15

        For I1 = 1 To gamePitcher(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, SS_P_Avail) = I1 Then
                    I4 = 1 - I
                    Locate , 1
                    Print pitchers$(I, I2); " ";

                    'Record
                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print "(W,"; Right$(Str$(oppPitStats(I, I2, 6) + 1), (Len(Str$(oppPitStats(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(oppPitStats(I, I2, 7)), (Len(Str$(oppPitStats(I, I2, 7))) - 1)); ")";
                        gameW$ = pitchers$(I, I2)
                    End If

                    If relieverOnRecordForSave(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print "(S,"; Right$(Str$(oppPitStats(I, I2, 12) + 1), (Len(Str$(oppPitStats(I, I2, 12) + 1)) - 1)); ")";
                    End If

                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print "(L,"; Right$(Str$(oppPitStats(I, I2, 6) + 0), (Len(Str$(oppPitStats(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(oppPitStats(I, I2, 7) + 1), (Len(Str$(oppPitStats(I, I2, 7) + 1)) - 1)); ")";
                        gameL$ = pitchers$(I, I2)
                    End If

                    If teamPitStats(I, I2, 19) > 0 Then
                        Print "(BS,";: Print Using "##"; oppPitStats(I, I2, 19);: Print ")";
                    End If
                    If teamPitStats(I, I2, 22) > 0 Then
                        Print "(H,";: Print Using "##"; oppPitStats(I, I2, 22);: Print ")";
                    End If

                    'IP
                    Locate , 30: Print Using "##"; Int(teamPitStats(I, I2, 0) / 3);

                    If teamPitStats(I, I2, 0) - (Int(teamPitStats(I, I2, 0) / 3) * 3) > 0 Then
                        Print ".";: Print Using "#"; teamPitStats(I, I2, 0) - (Int(teamPitStats(I, I2, 0) / 3) * 3);
                    End If

                    Locate , 34

                    'H R ER BB SO
                    Print Using "## ##  ## ## ##"; teamPitStats(I, I2, 1); teamPitStats(I, I2, 2); teamPitStats(I, I2, 3); teamPitStats(I, I2, 4); teamPitStats(I, I2, 5);

                    Print "   ";

                    'ERA
                    If teamPitStats(I, I2, 3) > 0 And teamPitStats(I, I2, 0) = 0 Then
                        Print "99.99 ";
                    Else
                        If teamPitStats(I, I2, 0) = 0 Then
                            Print "--.-- ";
                        Else
                            If teamPitStats(I, I2, 3) = 0 Then
                                Print " 0.00 ";
                            Else
                                statCalc! = teamPitStats(I, I2, 3) / (teamPitStats(I, I2, 0) / 3) * 9
                                Print Using "##.## "; statCalc!;
                            End If
                        End If
                    End If

                    'Wins
                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print Using "## "; oppPitStats(I, I2, 6) + 1;
                    Else
                        Print Using "## "; oppPitStats(I, I2, 6);
                    End If

                    'Losses
                    If pitcherOfRecord(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) < 0 Then
                        Print Using "## "; oppPitStats(I, I2, 7) + 1;
                    Else
                        Print Using "## "; oppPitStats(I, I2, 7);
                    End If

                    'Saves
                    If relieverOnRecordForSave(I) = I2 And gameScore(I, 0) - gameScore(I4, 0) > 0 Then
                        Print Using "## "; oppPitStats(I, I2, 12) + 1;
                    Else
                        Print Using "## "; oppPitStats(I, I2, 12);
                    End If

                    'Strikeouts
                    Print Using "###"; oppPitStats(I, I2, 5)

                    For I4 = 0 To 5
                        totalStats(I4) = totalStats(I4) + teamPitStats(I, I2, I4)
                    Next I4

                End If

            Next I2

        Next I1

        Color 14
        Print "TOTALS";
        Color 15

        Locate , 30

        If (totalStats(0) / 3) >= 10 Then
            Locate (gamePitcher(I) + 2), 29
        End If

        Print Int(totalStats(0) / 3);

        If totalStats(0) - (Int(totalStats(0) / 3) * 3) > 0 Then
            Print ".";: Print Using "#"; totalStats(0) - (Int(totalStats(0) / 3) * 3);
        End If

        Locate , 34

        Print Using "## ##  ## ## ##"; totalStats(1); totalStats(2); totalStats(3); totalStats(4); totalStats(5)
        Print

        If autoPlay <> 1 Or playerMode <> 2 Then
            Locate LAST_ROW, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

        Cls

        inningsPitched = CInt(totalStats(0) / 3)

        For I1 = 0 To 5:
            totalStats(I1) = 0
        Next I1

    Next I

    Cls

    Color 14: Print " E - ";
    Color 15

    If E = 0 Then
        Print "none"
    Else
        For X = 0 To (E - 1):
            Print eventDesc$(X); ",";
        Next
        Print eventDesc$(E)
    End If

    Color 14: Print "2B - ";
    Color 15

    If DB = 0 Then
        Print "none"
    Else
        For X = 0 To (DB - 1):
            Print doublesPlayer$(X); ",";
        Next
        Print doublesPlayer$(DB)
    End If

    Color 14: Print "3B - ";
    Color 15

    If TR = 0 Then
        Print "none"
    Else
        For X = 0 To (TR - 1):
            Print triplesPlayer$(X); ",";
        Next
        Print triplesPlayer$(TR)
    End If

    Color 14: Print "HR - ";
    Color 15

    If HR = 0 Then
        Print "none"
    Else
        For X = 0 To (HR - 1):
            Print homeRuns$(X); ",";
        Next
        Print homeRuns$(HR)
    End If

    Color 14: Print "SB - ";
    Color 15
    If SB = 0 Then
        Print "none"
    Else
        For X = 0 To (SB - 1):
            Print stolenBases$(X); ",";
        Next
        Print stolenBases$(SB)
    End If

    Color 14: Print "LOB - ";
    LOBText$ = gameTeams$(0) + " (" + Str$(leftOnBase(0)) + "), " + gameTeams$(1) + "(" + Str$(leftOnBase(1)) + ")"
    Color 15: Print LOBText$
    Color 14: Print "DP - ";
    DPText$ = gameTeams$(0) + "(" + Str$(totalDPs(0)) + "), " + gameTeams$(1) + "(" + Str$(totalDPs(1)) + ")"
    Color 15: Print DPText$

    Print

    Color 14: Print "UMPIRES:"
    Color 15: Print "HP-"; gameUmpire$(1); "; 1B-"; gameUmpire$(2); "; 2B-"; gameUmpire$(3); "; 3B-"; gameUmpire$(4)

    Print

    Color 14: Print "EJECTIONS: "
    Color 15

    If ejectCnt = 0 Then
        Print "none"
    Else
        For X = 1 To (ejectCnt - 1):
            Print ejections$(X); ",";
        Next
        Print ejections$(ejectCnt)
    End If

    Print

    Color 14: Print "INJURIES: "
    Color 15

    If injCount = 0 Then
        Print "none"
    Else
        For X = 1 To (injCount - 1):
            Print injPlayers$(X); ",";
        Next
        Print injPlayers$(injCount)
    End If

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
        Print #2, Using "MORE THAN ## INNINGS: ##"; eventSettings(1), INNING%
    End If

    If gameScore(1, 0) < eventSettings(2) Then
        Print #2, gameTeams$(1);
        Print #2, Using " SCORED LESS THAN ## RUN(S): ##"; eventSettings(2), gameScore(1, 0)
    End If

    If gameScore(0, 0) < eventSettings(2) Then
        Print #2, gameTeams$(0);
        Print #2, Using " SCORED LESS THAN ## RUN(S): ##"; eventSettings(2), gameScore(0, 0)
    End If

    If gameScore(1, 0) > eventSettings(3) Then
        Print #2, gameTeams$(1);
        Print #2, Using " SCORED MORE THAN ## RUN(S): ##"; eventSettings(3), gameScore(1, 0)
    End If

    If gameScore(0, 0) > eventSettings(3) Then
        Print #2, gameTeams$(0);
        Print #2, Using " SCORED MORE THAN ## RUN(S): ##"; eventSettings(3), gameScore(0, 0)
    End If

    If gameScore(1, 1) > eventSettings(5) Then
        Print #2, gameTeams$(1);
        Print #2, Using " HAD MORE THAN ## HIT(S): ##"; eventSettings(5), gameScore(1, 1)
    End If

    If gameScore(0, 1) > eventSettings(5) Then
        Print #2, gameTeams$(0);
        Print #2, Using " HAD MORE THAN ## HIT(S): ##"; eventSettings(5), gameScore(0, 1)
    End If

    If gameScore(1, 1) < eventSettings(4) Then
        Print #2, gameTeams$(1);
        Print #2, Using " HAD LESS THAN ## HIT(S): ##"; eventSettings(4), gameScore(1, 1)
    End If

    If gameScore(0, 1) < eventSettings(4) Then
        Print #2, gameTeams$(0);
        Print #2, Using " HAD LESS THAN ## HIT(S): ##"; eventSettings(4), gameScore(0, 1)
    End If

    For I = 0 To 1

        For I1 = 0 To 22

            If teamBatStats(I, I1, 1) > eventSettings(6) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HAD MORE THAN ## HIT(S): ##"; eventSettings(6), teamBatStats(I, I1, 1)
            End If

            If teamBatStats(I, I1, 2) > eventSettings(7) Then
                Print #2, batters$(I, I1);
                Print #2, Using " SCORED MORE THAN ## RUN(S): ##"; eventSettings(7), teamBatStats(I, I1, 2)
            End If

            If teamBatStats(I, I1, 3) > eventSettings(8) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HAD MORE THAN ## RBI(S): ##"; eventSettings(8), teamBatStats(I, I1, 3)
            End If

            If teamBatStats(I, I1, 4) > eventSettings(9) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HIT MORE THAN ## DOUBLES(S): ##"; eventSettings(9), teamBatStats(I, I1, 4)
            End If

            If teamBatStats(I, I1, 5) > eventSettings(10) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HIT MORE THAN ## TRIPLE(S): ##"; eventSettings(10), teamBatStats(I, I1, 5)
            End If

            If teamBatStats(I, I1, 6) > eventSettings(11) Then
                Print #2, batters$(I, I1);
                Print #2, Using " HOMERED MORE THAN ## TIME(S): ##"; eventSettings(11), teamBatStats(I, I1, 6)
            End If

            If teamBatStats(I, I1, 9) > eventSettings(12) Then
                Print #2, batters$(I, I1);
                Print #2, Using " STOLE MORE THAN ## BASE(S): ##"; eventSettings(12), teamBatStats(I, I1, 9)
            End If

        Next

    Next

    For I = 0 To 1
        For I1 = 0 To 21
            If teamPitStats(I, I1, 5) > eventSettings(13) Then
                Print #2, pitchers$(I, I1);
                Print #2, Using " STRUCK OUT MORE THAN ## BATTER(S): ##"; eventSettings(13), teamPitStats(I, I1, 5)
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

        Call POSTGAME
        'Close #11

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
            Locate LAST_ROW / 2, 33
            Print "PLEASE WAIT ..."
        End If

    End If

End Sub


'----------------------------------------
'      buntDecision% Function
'----------------------------------------
'This function determines if the current
'game conditions would fit going for a
'sacrifice hit. It adjusts the chances
'based on baseball era as well as several
'situations to adopt to multiple coaching
'styles and changes
Function buntDecision% (baseStatus)

    buntChance = 0

    If nbrOuts(P) < 2 Then

        If baseStatus = 1 Then
            'If runnerOnBase(1) > 0 And runnerOnBase(2) = 0 And runnerOnBase(3) = 0 Then
            ' Runner on first only - classic sacrifice
            If nbrOuts(0) = 0 Then buntChance = 3
            If nbrOuts(0) = 1 Then buntChance = 1
        End If

        If baseStatus = 2 Then
            'If runnerOnBase(1) = 0 And runnerOnBase(2) > 0 And runnerOnBase(3) = 0 Then
            ' Runner on second only - move to third
            If nbrOuts(0) = 0 Then buntChance = 2
            If nbrOuts(0) = 1 Then buntChance = 1
        End If

        If baseStatus = 3 Then
            'If runnerOnBase(1) > 0 And runnerOnBase(2) > 0 Then
            ' Runners on 1st and 2nd
            If nbrOuts(0) = 0 Then buntChance = 2
            If nbrOuts(0) = 1 Then buntChance = 1
        End If

        If baseStatus >= 4 Then
            ' Reduce chance If runner already on 3rd
            buntChance = 0
        End If

        ' Game situation adjustments
        ' Close games increase bunting
        scoreDiff = gameScore(P, 0) - gameScore(D, 0)

        If Abs(scoreDiff) <= 2 Then
            buntChance = buntChance + 2
        End If

        ' Late innings increase bunting in close games
        If INNING% >= 7 And Abs(scoreDiff) <= 1 Then
            buntChance = buntChance + 3
        End If

        ' Trailing by 1 in bottom of 9th inning (or extras)
        If INNING% >= 9 And P = 1 And scoreDiff = -1 Then
            buntChance = buntChance + 5
        End If

        ' Big leads / deficits reduce bunting
        If scoreDiff >= 4 Then
            buntChance = buntChance / 3
        End If

        'Pitchers are more likely to bunt
        If fldPos(P, currLineupSlot(P)) = 1 Then
            buntChance = buntChance + 8
        End If


        ' Era adjustments based on year
        Select Case teamYears(P)

            'Dead ball era
            Case Is <= 1919
                '-- needs increased if SF's can't increase
                buntEraAdj! = 5.1

                'Live ball era
            Case 1920 To 1945
                buntEraAdj! = 2.2

                'Integration era
            Case 1946 To 1960
                buntEraAdj! = 1.0

                'Expansion era
            Case 1961 To 1975
                buntEraAdj! = 1.7

                'Free agent era
            Case 1976 To 1993
                buntEraAdj! = 1.8

                'Steroid era
            Case 1994 To 2005
                buntEraAdj! = 2.2

                'Modern era
            Case 2006 To 2015
                buntEraAdj! = 1.5

                'Analytics era
            Case Is >= 2016
                buntEraAdj! = 0.7

        End Select
        buntChance = buntChance * buntEraAdj!

        ' Ensure we don't go negative
        If buntChance > 100 Then buntChance = 100
        If buntChance < 0 Then buntChance = 0

        ' Make final decision
        Call Rolld100(buntRoll)

        If buntRoll < buntChance Then
            buntDecision% = 1
        Else
            buntDecision% = 0
        End If

    End If

End Function


'----------------------------------------
'      GetParkAttendance& Function
'----------------------------------------
Function GetParkAttendance& (searchParkID$, gameYear)

    Shared parkAttKey$()
    Shared parkAvgAtt&()
    Shared totalParks

    Dim searchKey$

    ' The original game generated random attendance from 5,000 to 59,000
    '   Attendannce hasn't been as low as 5,000 since early 1900,
    '   and almost never 59,000 except for significant events
    '   https://teamTotWinsw.baseball-reference.com/leagues/majors/misc.shtml

    ' Search for current year's park attendance
    searchKey$ = searchParkID$ + "_" + LTrim$(Str$(gameYear))

    For i = 1 To totalParks

        If parkAttKey$(i) = searchKey$ Then
            GetParkAttendance& = parkAvgAtt&(i)
            Exit For
        End If

    Next i

End Function


'----------------------------------------
'      getRainDelayMinutes% Function
'----------------------------------------
'This function determines how long the
'rain falls for during a game when it
'is delayed.
Function getRainDelayMinutes% ()

    Call Rolld100(delayType)

    Select Case delayType

        Case 1 To 15
            'Short delay (light rain, quick tarp)
            '(20 - 40 minutes)
            getRainDelayMinutes = Int(Rnd * 21) + 20

        Case 16 To 69
            'Typical delay (majority of cases)
            '(45 - 90 minutes)
            getRainDelayMinutes = Int(Rnd * 46) + 45

        Case 70 To 94
            'Long delay (storms passing slowly)
            '(90–120 minutes)

            getRainDelayMinutes = Int(Rnd * 31) + 90

        Case 95 To 100
            'Very long delay, often ends in postponement
            '(120 - 180 minutes)
            getRainDelayMinutes = Int(Rnd * 61) + 120

    End Select

End Function


'----------------------------------------
'        GetRunStrategy% Function
'----------------------------------------
'This function will determine which run
'strategy to choose for the team. It has
'already been determined that the profile
'for the manager specifies such a strategy.
'
'RETURNS: value of 0 / 1 / 2.
'  These correspondence to the following,
'  in order: Safe / Normal / Aggressive
Function GetRunStrategy% ()
    
    scoreDiff = gameScore(P, 0) - gameScore(D, 0)

    'Game circumstances determine which manager
    'situation to assign
    Select Case INNING%
    
        Case 1 To 3
        
            Select Case scoreDiff
            
                Case Is > 0
                    'Ahead
                    situationIdx = 0
                
                Case 0
                    'Tied
                    situationIdx = 1
                
                Case Is < 0
                    'Behind
                    situationIdx = 2

            End Select
        
        
        Case 4 To 6
        
            Select Case scoreDiff
            
                Case Is > 0
                    'Ahead
                    situationIdx = 3
                
                Case 0
                    'Tied
                    situationIdx = 4
                
                Case Is < 0
                    'Behind
                    situationIdx = 5
            
            End Select
        
        Case 7 To 9

            Select Case scoreDiff
            
                Case Is > 0
                    'Ahead
                    situationIdx = 6
                
                Case 0
                    'Tied
                    situationIdx = 7
                
                Case Is < 0
                    'Behind
                    situationIdx = 8
            
            End Select
        
        Case Is >= 10
        
            Select Case scoreDiff
            
                Case Is > 0
                    'Ahead
                    situationIdx = 9

                Case 0
                    'Tied
                    situationIdx = 10

                Case Is < 0
                    'Behind
                    situationIdx = 11

            End Select

    End Select

    'Determine % thresholds based on situation
    safeThresh = teamMgrProfileVal(P, situationIdx * 3 + 811)
    normThresh = teamMgrProfileVal(P, situationIdx * 3 + 812)
    aggThresh = teamMgrProfileVal(P, situationIdx * 3 + 813)

    'Determine strategy based on roll against %'s
    Call Rolld100(stratTypeRoll)

    Select Case stratTypeRoll

        Case Is <= safeThresh
            GetRunStrategy% = 0

        Case (safeThresh + 1) To (safeThresh + normThresh)
            GetRunStrategy% = 1

        Case Is >= (100 - aggThresh)
            GetRunStrategy% = 2

    End Select

End Function


'----------------------------------------
'          GetTeamYear Function
'----------------------------------------
'This function determines the 4-digit
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
'       OUTFLDLOCATION Subroutine
'----------------------------------------
'This function returns the location of an
'outfielder. (LCF vs RCF)
Function OUTFLDLOCATION$ (currFielder)

    If d100% > 50 Then
        If currFielder = 7 Or currFielder = 8 Then
            OUTFLDLOCATION$ = "lcf"
        End If
    Else
        If currFielder = 9 Or currFielder = 8 Then
            OUTFLDLOCATION$ = "rcf"
        End If
    End If

End Function


'----------------------------------------
'         pitcherMaxStam% Function
'----------------------------------------
'This subroutine assigns max efficiency
'for a pitcher in the more modern eras
Function pitcherMaxStam% (teamIdx, pitcherIdx)

    'Max Efficiency = ((BB + Hits) / Games)
    pitcherMaxStam% = CInt(((pitchRating(teamIdx, pitcherIdx, SS_P_BB) + pitchRating(teamIdx, pitcherIdx, SS_P_H)) / pitchRating(teamIdx, pitcherIdx, SS_P_G)))

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
'Reads in Lefty/Righty for the selected team,
' given it's index in the team file.
'This extra file is only created when
' a team is input, so if it's missing
' then the team should be edied to create it.
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

        mgrFile(gameIdx) = 0

    End If

    'No manager file
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


'----------------------------------------
'        DumpRunners Subroutine
'----------------------------------------
Sub DumpRunners (myText$)

    Print #11, "--- "; myText$; " ---"

    For bm1 = 0 To 3
        Print #11, "baseMove:"; baseMove(bm1); " -- ";
    Next bm1

    Print #11, ""

    For bm1 = 0 To 3
        Print #11, "runnerOnBase:   "; runnerOnBase(bm1); " -- ";
    Next bm1

    Print #11,

    For bm1 = 0 To 3
        Print #11, "bRunner: "; bRunner(bm1); " -- ";
    Next bm1

    Print #11,

End Sub


'----------------------------------------
'          DumpIt Subroutine
'----------------------------------------
Sub DumpIt (baseStatus, P2, D2)

    Print #11, "---"
    Print #11, "Inning is:"; INNING%;

    If P = 0 Then
        Print #11, "Visiting";
    Else
        Print #11, "Home";
    End If
    Print #11, " team batting"

    Print #11, "baserunners = "; baseStatus; " outs: "; nbrOuts(0); " expected outs: "; nbrOuts(1)
    Print #11, "Score is: "; gameScore(0, 0); " to "; gameScore(1, 0)

    lp% = lineupPlayer(P, currLineupSlot(P))
    fp% = fldPos(P, currLineupSlot(P))
    If fp% = 1 Then
        batr$ = pitchers$(P, currPitcher(P))
    Else
        batr$ = batters$(P, lp%)
    End If

    Print #11, "Lineup slot is: "; currLineupSlot(P); " Roster player is: "; lp%; " Batter is: "; batr$; " Position is; "; fp%; " Pitcher is: "; pitchers$(D, currPitcher(D))

    Print #11, "Batter: "; player$(0); ", 1st base: "; player$(1); ", 2nd base: "; player$(2); ", 3rd base: "; player$(3); ", fielder w/ball: "; player$(4)

    If P2 > 1 Then Print #11, " +++ Off strategy: "; P2;
    If D2 > 1 Then Print #11, " +++ Def strategy: "; D2
    Print #11, ""

    For bm1 = 0 To 3
        Print #11, "runnerOnBase:   "; runnerOnBase(bm1); " -- "; 'all runners, earned and unearned
    Next bm1
    Print #11,
    For bm1 = 0 To 3
        Print #11, "bRunner: "; bRunner(bm1); " -- "; 'earned runners only
    Next bm1
    Print #11,
    Print #11,

    For bm1 = 0 To 3
        Print #11, "b2Pitcher:"; b2Pitcher(bm1); " -- ";
    Next bm1
    Print #11,
    For bm1 = 0 To 3
        Print #11, "b1Pitcher:"; b1Pitcher(bm1); " -- ";
    Next bm1
    Print #11,
    Print #11,

    For bm1 = 0 To 3
        Print #11, "stealPhase:"; stealPhase(bm1); " -- ";
    Next bm1

    Print #11, "---"

End Sub


'Include: 'GameRoutines_Gfx.bm'

'----------------------------------------
'          MAINSCREEN_G routine
'----------------------------------------
' ...explanation...
Sub MAINSCREEN_G (nbrBalls, nbrStrikes, MO, INFPOS%)

    _PutImage (29 * FontColAdj, 400)-Step(24 * FontColAdj, 16), backupScreen&, imageScreen&, (29 * FontColAdj, 400)-Step(24 * FontColAdj, 16)

    'Printing white on black
    Color &HFFFFFFFF
    _PrintMode _FillBackground , imageScreen&

    '-----

    'Pitcher Name
    pitcher$ = Left$(pitchers$(D, currPitcher(D)), 14)
    PrintOnGfx 65, 26, " " + pitcher$, imageScreen&

    '-- Why is this not Pitch Count / Strikes ???

    'Wins
    FORMATTED$ = FUsing$(Str$(pitchRating(D, currPitcher(D), 1)), "##", 1)
    pStats$ = FORMATTED$ + "-"

    'Losses
    FORMATTED$ = FUsing$(Str$(pitchRating(D, currPitcher(D), 2)), "##", 1)

    '                                              ERA
    pStats$ = pStats$ + FORMATTED$ + " " + FUsing$(Str$(pitchRating(D, currPitcher(D), 10) / 100), "##.##", 1)

    'Pitcher Stats
    PrintOnGfx 66, 27, pStats$, imageScreen&

    '-----

    'IP / H / BB / K headers
    PrintOnGfx 66, 28, "IP   H BB  K", imageScreen&

    'IP
    If teamPitStats(D, currPitcher(D), 0) <> 0 Then
        pStats$ = FUsing$(Str$(Int(teamPitStats(D, currPitcher(D), 0) / 3)), "##.#", 1)
    Else
        pStats$ = " 0.0"
    End If

    'H / BB / K
    pStats$ = pStats$ + " " + FUsing$(Str$(teamPitStats(D, currPitcher(D), 1)), "##", 1) + " " + FUsing$(Str$(teamPitStats(D, currPitcher(D), 4)), "##", 1) + " " + FUsing$(Str$(teamPitStats(D, currPitcher(D), 5)), "##", 1)

    PrintOnGfx 65, 29, pStats$, imageScreen&

    J = lineupPlayer(P, currLineupSlot(P))

    If fldPos(P, currLineupSlot(P)) = 1 Then
        X$ = pitchers$(P, currPitcher(P))
        'We're going to use this in just a bit
        'We'll be printing it with more space
        player$ = X$
        X$ = PadRight$(X$, 11)
        Call PITPLACEMENT(X$)
    Else
        X$ = batters$(P, J)
        'We're going to use this in just a bit
        'We'll be printing it with more space
        player$ = X$
        X$ = PadRight$(X$, 11)

        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color &HFFFFFFFF

        If batRating(P, J, 0) < 0 Or batRating(P, J, 0) = 0 And pitchRating(D, currPitcher(D), 0) > 0 Then
            'Batter Name - Right of Plate
            PrintOnGfx 41, 25, Left$(X$, 11), imageScreen&
        End If

        If batRating(P, J, 0) > 0 Or batRating(P, J, 0) = 0 And pitchRating(D, currPitcher(D), 0) < 0 Then
            'Batter Name - Left of Plate
            PrintOnGfx 30, 25, Left$(X$, 11), imageScreen&
        End If

        'Printing white on black
        _PrintMode _FillBackground , imageScreen&

    End If

    player$ = PadRight$(player$, 14)

    'Batter Name - Full
    PrintOnGfx 65, 22, " " + player$, imageScreen&

    'Batter Stats - Header
    PrintOnGfx 65, 23, " AVG HR  BB  K ", imageScreen&

    If fldPos(P, currLineupSlot(P)) <> 1 Then
        'Batter Stats - Hitter
        If batRating(P, J, 4) = 0 Then
            PrintOnGfx 65, 24, ".000  0  0   0", imageScreen&
        Else
            'AVG
            bAvg! = batRating(P, J, 6) / batRating(P, J, 4)
            If bAvg! = 0 Then
                PrintOnGfx 65, 24, ".000  0  0   0", imageScreen&
            Else
                pStats$ = "." + FUsing$(Str$(bAvg! * 1000), "###", 1)
                '                                 HR                                         BB                                            K
                pStats$ = pStats$ + " " + FUsing$(Str$(batRating(P, J, 9)), "##", 1) + " " + FUsing$(Str$(batRating(P, J, 11)), "###", 1) + " " + FUsing$(Str$(batRating(P, J, 12)), "###", 1)
                PrintOnGfx 65, 24, pStats$, imageScreen&
            End If

        End If

    Else

        'Batter Stats - Pitcher
        If pitchRating(P, currPitcher(P), 11) <> 999 Then
            If pAvg! = 0 Then
                pStats$ = ".000"
            Else
                pAvg! = pitchRating(P, currPitcher(P), 11)
                pStats$ = "." + FUsing$(Str$(pAvg! * 1000), "###", 1)
            End If
            PrintOnGfx 65, 24, pStats$ + "           ", imageScreen&
        Else
            If pitchRating(P, currPitcher(P), 20) = 0 Then
                PrintOnGfx 65, 24, ".000  0   0   0", imageScreen&
            Else
                'AVG
                pAvg! = pitchRating(P, currPitcher(P), 22) / pitchRating(P, currPitcher(P), 20)

                If pAvg! = 0 Then
                    PrintOnGfx 65, 24, ".000  0   0   0", imageScreen&
                Else
                    pStats$ = FUsing$(Str$(pAvg! * 1000), ".###", 1)
                    '                          HR                                                BB              K
                    pStats$ = pStats$ + " " + FUsing$(Str$(pitchRating(P, currPitcher(P), 25)), "##", 1) + " " + FUsing$(Str$(pitchRating(P, currPitcher(P), 27)), "###", 1) + " " + FUsing$(Str$(pitchRating(P, currPitcher(P), 28)), "###", 1)
                    PrintOnGfx 65, 24, pStats$, imageScreen&

                End If

            End If
        End If

    End If

    ' # - # TODAY
    If fldPos(P, currLineupSlot(P)) <> 1 Then
        today$ = FUsing$(Str$(teamBatStats(P, lineupPlayer(P, currLineupSlot(P)), 1)), "##", 1) + "-" + FUsing$(Str$(teamBatStats(P, lineupPlayer(P, currLineupSlot(P)), 0)), "##", 1)
    Else
        today$ = FUsing$(Str$(teamPitStats(P, currPitcher(P), 24)), "##", 1) + "-" + FUsing$(Str$(teamPitStats(P, currPitcher(P), 23)), "##", 1)
    End If

    today$ = today$ + " TODAY"

    PrintOnGfx 66, 25, today$, imageScreen&

    'Balls
    PrintOnGfx 63, 27, Str$(nbrBalls), imageScreen&

    'Outs
    PrintOnGfx 63, 29, Str$(nbrOuts(0)), imageScreen&

    'Strikes
    PrintOnGfx 63, 28, Str$(nbrStrikes), imageScreen&

    'Infield
    _PutImage (51 * FontColAdj, 416)-Step(13 * FontColAdj, 16), backupScreen&, imageScreen&, (51 * FontColAdj, 416)-Step(13 * FontColAdj, 16)

    infStatus$ = "INF:"
    Select Case INFPOS%
        Case 0: infStatus$ = infStatus$ + "NORMAL    "
        Case 1: infStatus$ = infStatus$ + "CORNERS IN"
        Case 2: infStatus$ = infStatus$ + "INFIELD IN"
    End Select

    'Printing black on background
    _PrintMode _KeepBackground , imageScreen&
    Color &HF0000000
    PrintOnGfx 51, 26, infStatus$, imageScreen&

    'Printing white on black
    Color &HFFFFFFFF
    _PrintMode _FillBackground , imageScreen&
    Call HOLDSTATUS_G(runnerHeld)

    'RUNNER AT SECOND
    Call DisplayRun_G(2, 37)

    If nbrOuts(0) <> 3 Then MO = 0

    For WK = 1 To 3
        If runnerOnBase(WK) > 0 Then MO = MO + 1
    Next

    'RUNNER AT THIRD
    Call DisplayRun_G(3, 15)

    'RUNNER AT FIRST
    Call DisplayRun_G(1, 56)

End Sub

'----------------------------------------
'        DisplayRun_G Subroutine
'----------------------------------------
'This subroutine displays the player
'that is currently on the target base
Sub DisplayRun_G (baseNbr, Xcoord)

    Select Case baseNbr
        '1st Base Runner
        Case 1:
            _PutImage (54 * FontColAdj, 336)-Step(14 * FontColAdj, 16), backupScreen&, imageScreen&, (54 * FontColAdj, 336)-Step(14 * FontColAdj, 16)
            _PutImage (54 * FontColAdj, 352)-Step(11 * FontColAdj, 16), backupScreen&, imageScreen&, (54 * FontColAdj, 352)-Step(11 * FontColAdj, 16)

            '2nd Base Runner
        Case 2:
            _PutImage (35 * FontColAdj, 304)-Step(14 * FontColAdj, 32), backupScreen&, imageScreen&, (35 * FontColAdj, 304)-Step(14 * FontColAdj, 32)

            '3rd Base Runner
        Case 3:
            _PutImage (15 * FontColAdj, 336)-Step(14 * FontColAdj, 32), backupScreen&, imageScreen&, (15 * FontColAdj, 336)-Step(14 * FontColAdj, 32)

    End Select

    J = runnerOnBase(baseNbr)

    If J <> 0 Then

        If baseNbr = 2 Then
            Ycoord = 20
        Else
            Ycoord = 22
        End If

        'Printing white on background
        _PrintMode _KeepBackground , imageScreen&
        Color &HFFFFFFFF

        If fldPos(P, J) = 1 Then

            If pitchRating(P, currPitcher(P), 11) <> 999 Then
                runner$ = pitchers$(P, currPitcher(P))
                runner$ = PadRight(runner$, 11)
                PrintOnGfx Xcoord, Ycoord - 1, runner$, imageScreen&
                PrintOnGfx Xcoord, Ycoord, "1/  0", imageScreen&
            Else
                runner$ = pitchers$(P, currPitcher(P))
                runner$ = PadRight(runner$, 11)
                PrintOnGfx Xcoord, Ycoord - 1, runner$, imageScreen&
                runStats$ = FUsing$(Str$(pitchRating(P, currPitcher(P), 31)), "#", 1) + "/" + FUsing$(Str$(pitchRating(P, currPitcher(P), 29)), "###", 1)
                PrintOnGfx Xcoord, Ycoord, runStats$, imageScreen&
            End If

        Else

            runner$ = batters$(P, lineupPlayer(P, J))
            runner$ = PadRight(runner$, 11)
            PrintOnGfx Xcoord, Ycoord - 1, runner$, imageScreen&
            runStats$ = FUsing$(Str$(batRating(P, lineupPlayer(P, J), SS_B_Run)), "#", 1) + "/" + FUsing$(Str$(batRating(P, lineupPlayer(P, J), SS_B_SB)), "###", 1)
            PrintOnGfx Xcoord, Ycoord, runStats$, imageScreen&

            'Printing white on black
            _PrintMode _FillBackground , imageScreen&

        End If

    End If

End Sub

'----------------------------------------
'    DisplayBallField_Gfx Subroutine
'----------------------------------------
' ...explanation...
Sub DisplayBallField_Gfx (nbrBalls, nbrStrikes)

    ReDim FORMATTED$(0 To 2)

    inningStart% = 1
    inningEnd% = 9

    If INNING% > 9 Then
        inningStart% = 10
        inningEnd% = 18
    End If
    If INNING% > 18 Then
        inningStart% = 19
        inningEnd% = 27
    End If

    'Print Batter / Pitcher box
    '----------------------------------------
    For I = 23 To 30
        PrintOnGfx 65, I - 1, Space$(16), imageScreen&
    Next

    'Print Score Box
    '----------------------------------------
    For I = 28 To 30
        PrintOnGfx 15, I - 1, Space$(51), imageScreen&
    Next

    For X1% = 0 To 2
        '3 innings at a time
        FORMATTED$(X1%) = FUsing$(Str$(inningStart% + 3 * X1%), "##", 1) + " " + FUsing$(Str$(inningStart% + 3 * X1% + 1), "##", 1) + " " + FUsing$(Str$(inningStart% + 3 * X1% + 2), "##", 1) + "  "
    Next

    'Print innings, headers for runs / hits / errors, along with balls
    FORMATTED$ = FORMATTED$(0) + FORMATTED$(1) + FORMATTED$(2) + " R  H  E B" + Str$(nbrBalls)
    PrintOnGfx 23, 27, FORMATTED$, imageScreen&

    For I = 0 To 1

        'Print inning score
        PrintOnGfx 15, I + 28, Left$(gameTeams$(I), 3) + teamAbbreviatons$(I), imageScreen&

        For I2 = inningStart% To inningEnd%
            If P = 0 And I = 0 And I2 > INNING% - 1 And inningScore(I, INNING%) = 0 Or P = 0 And I = 0 And I2 > INNING% And inningScore(I, INNING%) > 0 Or P = 1 And I2 > INNING% And I = 0 Then Exit For
            If P = 0 And I = 1 And I2 > INNING% - 1 Or P = 1 And I = 1 And inningScore(I, INNING%) = 0 And I2 > INNING% - 1 Or P = 1 And I = 1 And inningScore(I, INNING%) > 0 And I2 > INNING% Then Exit For
            Call DisplayInningScore_G(I2, I)
        Next I2

        R% = gameScore(I, 0)
        H% = gameScore(I, 1)
        E% = gameScore(I, 2)

        '                         R                                         H                                                      E
        FORMATTED$ = FUsing$(Str$(gameScore(I, 0)), "##", 1) + " " + FUsing$(Str$(gameScore(I, 1)), "##", 1) + " " + FUsing$(Str$(gameScore(I, 2)), "##", 1) + " "

        If I = 0 Then
            'Strikes
            FORMATTED$ = FORMATTED$ + "S" + Str$(nbrStrikes)
        Else
            'Outs
            FORMATTED$ = FORMATTED$ + "O" + Str$(nbrOuts(0))
        End If

        PrintOnGfx 53, I + 28, FORMATTED$, imageScreen&

    Next I

    'Center Field
    _PutImage (36 * FontColAdj, 224)-Step(13 * FontColAdj, 32), backupScreen&, imageScreen&, (36 * FontColAdj, 224)-Step(13 * FontColAdj, 32)

    'Left Field
    _PutImage (7 * FontColAdj, 256)-Step(16 * FontColAdj, 32), backupScreen&, imageScreen&, (7 * FontColAdj, 256)-Step(16 * FontColAdj, 32)

    'Right Field
    _PutImage (64 * FontColAdj, 256)-Step(16 * FontColAdj, 32), backupScreen&, imageScreen&, (64 * FontColAdj, 256)-Step(16 * FontColAdj, 32)

    'Short Stop
    _PutImage (28 * FontColAdj, 272)-Step(12 * FontColAdj, 32), backupScreen&, imageScreen&, (28 * FontColAdj, 272)-Step(12 * FontColAdj, 32)

    '2nd Base
    _PutImage (46 * FontColAdj, 272)-Step(13 * FontColAdj, 32), backupScreen&, imageScreen&, (46 * FontColAdj, 272)-Step(13 * FontColAdj, 32)

    '3rd Base
    _PutImage (13 * FontColAdj, 304)-Step(12 * FontColAdj, 32), backupScreen&, imageScreen&, (13 * FontColAdj, 304)-Step(12 * FontColAdj, 32)

    '1st Base
    _PutImage (61 * FontColAdj, 304)-Step(12 * FontColAdj, 32), backupScreen&, imageScreen&, (61 * FontColAdj, 304)-Step(12 * FontColAdj, 32)

    'Pitcher
    _PutImage (35 * FontColAdj, 336)-Step(16 * FontColAdj, 32), backupScreen&, imageScreen&, (35 * FontColAdj, 336)-Step(16 * FontColAdj, 32)

    'Catcher
    _PutImage (35 * FontColAdj, 416)-Step(13 * FontColAdj, 32), backupScreen&, imageScreen&, (35 * FontColAdj, 416)-Step(13 * FontColAdj, 32)

    player$ = Left$(batters$(D, fielder(D, 8)), 11)
    player$ = PadRight(player$, 11)

    'Print all positions around the diamond
    '----------------------------------------
    'Printing black on background
    _PrintMode _KeepBackground , imageScreen&
    Color &HF0000000

    'Center Field Name
    PrintOnGfx 36, 14, player$, imageScreen&

    'Center Field Stats
    If batRating(D, fielder(D, 8), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 8), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 8), 19)), "###", 1)
        End If
    End If

    player$ = "A" + FUsing$(Str$(batRating(D, fielder(D, 8), 15)), "#", 1) + " R" + FUsing$(Str$(batRating(D, fielder(D, 8), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 36, 15, player$, imageScreen&

    'Left Field Name
    player$ = Left$(batters$(D, fielder(D, 7)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 7, 16, player$, imageScreen&

    'Left Field Stats
    If batRating(D, fielder(D, 7), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 7), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 7), 19)), "###", 1)
        End If
    End If

    player$ = "A" + FUsing$(Str$(batRating(D, fielder(D, 7), 15)), "#", 1) + " R" + FUsing$(Str$(batRating(D, fielder(D, 7), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 7, 17, player$, imageScreen&

    'Right Field Name
    player$ = Left$(batters$(D, fielder(D, 9)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 64, 16, player$, imageScreen&

    'Right Field Stats
    If batRating(D, fielder(D, 9), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 9), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 9), 19)), "###", 1)
        End If
    End If

    player$ = "A" + FUsing$(Str$(batRating(D, fielder(D, 9), 15)), "#", 1) + " R" + FUsing$(Str$(batRating(D, fielder(D, 9), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 64, 17, player$, imageScreen&

    'Short Stop Name
    player$ = Left$(batters$(D, fielder(D, 6)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 28, 17, player$, imageScreen&

    'Short Stop Stats
    If batRating(D, fielder(D, 6), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 6), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 6), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(batRating(D, fielder(D, 6), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 28, 18, player$, imageScreen&

    '2nd Base Name
    player$ = Left$(batters$(D, fielder(D, 4)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 46, 17, player$, imageScreen&

    '2nd Base Stats
    If batRating(D, fielder(D, 4), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 4), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 4), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(batRating(D, fielder(D, 4), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 46, 18, player$, imageScreen&

    '3rd Base Name
    player$ = Left$(batters$(D, fielder(D, 5)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 13, 19, player$, imageScreen&

    '3rd Base Stats
    If batRating(D, fielder(D, 5), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 5), 19) = 0 Then
            fldPct$ = " .000"
        Else

            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 5), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(batRating(D, fielder(D, 5), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 13, 20, player$, imageScreen&

    '1st Base Name
    player$ = Left$(batters$(D, fielder(D, 3)), 10)
    player$ = PadRight(player$, 10)
    PrintOnGfx 61, 19, player$, imageScreen&

    '1st Base Stats
    If batRating(D, fielder(D, 3), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        If batRating(D, fielder(D, 3), 19) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(batRating(D, fielder(D, 3), 19)), "###", 1)
        End If
    End If

    player$ = "R" + FUsing$(Str$(batRating(D, fielder(D, 3), 17)), "#", 1) + " F" + fldPct$
    PrintOnGfx 61, 20, player$, imageScreen&

    'Pitcher Name
    player$ = "[" + handed$(pitchRating(D, currPitcher(D), 0) + 2) + "] " + Left$(pitchers$(D, currPitcher(D)), 10)
    player$ = PadRight(player$, 14)
    PrintOnGfx 35, 21, player$, imageScreen&

    'Pitcher Stats
    If pitchRating(D, currPitcher(D), 17) = 1000 Then
        fldPct$ = "1.000"
    Else
        If pitchRating(D, currPitcher(D), 17) = 0 Then
            fldPct$ = " .000"
        Else
            fldPct$ = "." + FUsing$(Str$(pitchRating(D, currPitcher(D), 17)), "###", 1)
        End If
    End If

    player$ = "H" + FUsing$(Str$(pitchRating(D, currPitcher(D), 18)), "#", 1) + " F" + fldPct$
    'was column 39 previously
    PrintOnGfx 35, 22, player$, imageScreen&

    'Catcher Name
    player$ = Left$(batters$(D, fielder(D, 2)), 10)
    player$ = PadRight(player$, 10)
    player$ = player$ + "A" + FUsing$(Str$(batRating(D, fielder(D, 2), 15)), "#", 1)
    PrintOnGfx 35, 26, player$, imageScreen&

    'Printing white on black
    Color &HFFFFFFFF
    _PrintMode _FillBackground , imageScreen&

End Sub


'----------------------------------------
'        HOLDSTATUS_G Subroutine
'----------------------------------------
'This routine displays the runner status
'(whether runners are HELD 'or not) in
'the game's VGA mode
Sub HOLDSTATUS_G (runnerHeld)

    _PutImage (52 * FontColAdj, 400)-Step(12 * FontColAdj, 16), backupScreen&, imageScreen&, (52 * FontColAdj, 400)-Step(12 * FontColAdj, 16)

    rhStatus$ = "RUNNERS:"

    If runnerHeld = 0 Then
        rhStatus$ = rhStatus$ + "    "
    Else
        rhStatus$ = rhStatus$ + "HELD"
    End If

    'Printing black on background
    _PrintMode _KeepBackground , imageScreen&
    Color &HF0000000
    PrintOnGfx 52, 25, rhStatus$, imageScreen&

    'Printing white on black
    Color &HFFFFFFFF
    _PrintMode _FillBackground , imageScreen&

End Sub

'----------------------------------------
'     DisplayInningScore_G Subroutine
'----------------------------------------
'This routine calculates where to locate
'the cursor to print the score for the
'current inning in the game's gfx mode.
Sub DisplayInningScore_G (currInning, teamLine)

    Dim setOf9, inningInSet, col

    ' Each set of 9 innings repeats the same column pattern
    setOf9 = (currInning - 1) \ 9 ' Which set: 0, 1, 2...
    inningInSet = (currInning - 1) Mod 9 ' Position in set: 0-8

    ' Base column calculation: 3 positions per inning + base offset
    ' Extra offset every 3 innings within each set
    col = 3 * (inningInSet + 1) + 7 + (inningInSet \ 3)

    ' Shift right for subsequent sets (appears to be 30 columns per set)
    If setOf9 > 0 Then col = col + setOf9 * 30

    FORMATTED$ = FUsing$(Str$(inningScore(teamLine, currInning)), "##", 1)
    PrintOnGfx col + 13, teamLine + 28, FORMATTED$, imageScreen&

End Sub


'----------------------------------------
'         LOADSCREEN Subroutine
'----------------------------------------
'This routine loads the inital park image
'graphic, and then backs up key areas of
'the graphic so that we can write text
'and/or restore the graphic later.
Sub LOADSCREEN ()

    parkImage& = _LoadImage("." + dirSeparator$ + "parks" + dirSeparator$ + chosenPark$, 32)
    imageWidth% = _Width(parkImage&)
    imageHeight% = _Height(parkImage&)
    imageScreen& = _NewImage(imageWidth%, imageHeight%, 32)
    backupScreen& = _NewImage(imageWidth%, imageHeight%, 32)

    Screen imageScreen&
    Cls
    _PutImage (0, 0), parkImage&, 0

    'Backup several areas of image where player text will go

    'Play-by-Play box
    _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144), imageScreen&, backupScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144)

    'Runners Hold section
    _PutImage (52 * FontColAdj, 400)-Step(12 * FontColAdj, 16), imageScreen&, backupScreen&, (52 * FontColAdj, 400)-Step(12 * FontColAdj, 16)

    'Infield
    _PutImage (51 * FontColAdj, 416)-Step(14 * FontColAdj, 16), imageScreen&, backupScreen&, (51 * FontColAdj, 416)-Step(14 * FontColAdj, 16)

    'Center Field
    _PutImage (36 * FontColAdj, 224)-Step(13 * FontColAdj, 32), imageScreen&, backupScreen&, (36 * FontColAdj, 224)-Step(13 * FontColAdj, 32)

    'Left Field
    _PutImage (7 * FontColAdj, 256)-Step(16 * FontColAdj, 32), imageScreen&, backupScreen&, (7 * FontColAdj, 256)-Step(16 * FontColAdj, 32)

    'Right Field
    _PutImage (64 * FontColAdj, 256)-Step(16 * FontColAdj, 32), imageScreen&, backupScreen&, (64 * FontColAdj, 256)-Step(16 * FontColAdj, 32)

    'Short Stop
    _PutImage (28 * FontColAdj, 272)-Step(12 * FontColAdj, 32), imageScreen&, backupScreen&, (28 * FontColAdj, 272)-Step(12 * FontColAdj, 32)

    '2nd Base
    _PutImage (46 * FontColAdj, 272)-Step(13 * FontColAdj, 32), imageScreen&, backupScreen&, (46 * FontColAdj, 272)-Step(13 * FontColAdj, 32)

    '3rd Base
    _PutImage (13 * FontColAdj, 304)-Step(12 * FontColAdj, 32), imageScreen&, backupScreen&, (13 * FontColAdj, 304)-Step(12 * FontColAdj, 32)

    '1st Base
    _PutImage (61 * FontColAdj, 304)-Step(12 * FontColAdj, 32), imageScreen&, backupScreen&, (61 * FontColAdj, 304)-Step(12 * FontColAdj, 32)

    'Pitcher
    _PutImage (35 * FontColAdj, 336)-Step(16 * FontColAdj, 32), imageScreen&, backupScreen&, (35 * FontColAdj, 336)-Step(16 * FontColAdj, 32)

    'Catcher
    _PutImage (35 * FontColAdj, 416)-Step(13 * FontColAdj, 16), imageScreen&, backupScreen&, (35 * FontColAdj, 416)-Step(13 * FontColAdj, 16)

    '2nd Base Runner
    _PutImage (37 * FontColAdj, 304)-Step(14 * FontColAdj, 32), imageScreen&, backupScreen&, (37 * FontColAdj, 304)-Step(14 * FontColAdj, 32)

    '3rd Base Runner
    _PutImage (15 * FontColAdj, 336)-Step(14 * FontColAdj, 32), imageScreen&, backupScreen&, (15 * FontColAdj, 336)-Step(14 * FontColAdj, 32)

    '1st Base Runner Name
    _PutImage (54 * FontColAdj, 336)-Step(14 * FontColAdj, 16), imageScreen&, backupScreen&, (54 * FontColAdj, 336)-Step(14 * FontColAdj, 16)

    '1st Base Runner Stats
    _PutImage (54 * FontColAdj, 352)-Step(11 * FontColAdj, 16), imageScreen&, backupScreen&, (54 * FontColAdj, 352)-Step(11 * FontColAdj, 16)

    'Batter - Right
    _PutImage (30 * FontColAdj, 400)-Step(23 * FontColAdj, 16), imageScreen&, backupScreen&, (30 * FontColAdj, 400)-Step(23 * FontColAdj, 16)

    'Commands for Human Play
    _PutImage (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176), imageScreen&, backupScreen&, (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176)

End Sub


'----------------------------------------
'         PrintOnGfx Subroutine
'----------------------------------------
'This subroutine simply prints text at
'in graphics mode at the Row/Column that
'is specified for the target sreen handle.
'This will auto convert these logical row
'and column values to their pixel versions,
'based on the screen width / height.
Sub PrintOnGfx (Col%, Row%, Text$, tgtScrn&)

    PrintCol% = (Col% * _PrintWidth("W"))
    PrintRow% = (Row% * _FontHeight)
    _PrintString (PrintCol%, PrintRow%), Text$, tgtScrn&

End Sub
