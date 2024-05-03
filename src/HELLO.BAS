$Debug
$Color:32

Const GAME_VERSION$ = "6.91"
Const MAX_SCHEDULE_GAMES = 1500
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV = 10
Const DIVS_PER_LEAGUE = 4
Const TEAMS_PER_LEAGUE = 40

Const MAX_SCHED_STATS = 162
Const SCHEDULE_SIZE_BYTES = 52

DefInt A-Z

' General purpose
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'PadRight.bi'
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

On Error GoTo Errhandler


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

Data A,B,C,D,E,F,G,H,I,J,K,L,M
For I = 0 To 12:
    Read alpha$(I)
Next

'Data to read in to variables
Data first,second,third,short
For I = 3 To 6
    Read grounderDir$(I)
Next

Data 681,663,645,627,609,591,573,555,537

Data 15,0,15,1,19,4,4,6,3,1,2,3,14

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

For X = 1 To 13:
    Read eventSettings(X)
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

PT$(1) = "LHP"
PT$(0) = "RHP"

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

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

If Not _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Output As #1

    For I = 0 To 3
        Select Case opSysType$
            Case "[WINDOWS]": Print #1, _CWD$ + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": Print #1, _CWD$ + "/"
        End Select
    Next

    Close #1

End If

Open "DEV.BAS" For Input As #2

For I = 0 To 4
    Input #2, diskPaths$(I)
Next

Close #2

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 10

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
'If _FileExists("GAME82") Then
'Print #9, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
'Print #9, gameTeams$(0) + " vs. " + gameTeams$(1)
'End If
Print #9, "Value of D = "; D
Print #9,
Close #9
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

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

        menuSelection% = 1
        Count% = 10
        Start% = 1
        ExitCode% = 10
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection%

                Case 1:
                    Call LOADER

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

    Loop Until menuSelection% = 10 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 10

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
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT  "
        menuChoices$(5) = "DRAFT PLAYERS              "
        menuChoices$(6) = "TRADE PLAYERS              "
        menuChoices$(7) = "TOGGLE PLAYER STATUS       "
        menuChoices$(8) = "SWAP SPOTS ON A TEAM ROSTER"
        menuChoices$(9) = "RETURN TO MAIN MENU        "
        Selection% = 1
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

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

    Loop Until menuSelection% = 9 Or ExitCode% = 27

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

        ReDim menuChoices$(1 To 6)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS     "
        menuChoices$(2) = "TEAM CAREER OPTIONS             "
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL  "
        'menuChoices$(4) = "MODIFY REPLAY STATS            "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(5) = "SORT PLAYERS BY POSITION        "
        menuChoices$(6) = "RETURN TO MAIN MENU             "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call COMPARE

                Case 4:
                    Call MERGE

                Case 5:
                    Call POSSRT

                Case 6:
                    '

            End Select

        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
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
        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call CREATLEAG

                Case 2:
                    Call CompileLeague

                Case 3:
                    Call SEELEAG

                Case 4:
                    Call Hd2HdPro_League
                    'Call HD2HD

                Case 5:
                    Call SeasonRecords

                Case 6:
                    Call NEWLDR

                Case 7:
                    Call BLANK

            End Select

        End If

    Loop Until Selection% = 8 Or ExitCode% = 27

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
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

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

    Loop Until menuSelection% = 4 Or ExitCode% = 27

End Sub


' General purpose
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'PadRight.bm'
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
'$Include: 'CREATLEAG.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'GROUND.BAS'
'$Include: 'HD2HD.BAS'
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
'$Include: 'LOADER.BAS'
'$Include: 'SOURCE.bm'
'$Include: 'GameRoutines.bm'
'$Include: 'GameRoutines_Gfx.bm'

