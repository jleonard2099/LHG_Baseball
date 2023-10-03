$Debug

Const GAME_VERSION$ = "6.67"
Const MAX_TEAMS = 1000
Const MAX_GAMES = 1500
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

Const NUM_STATRECORDS = 162
Const SCHEDULE_SIZE_BYTES = 48

DefInt A-Z

' General purpose
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'QBWRITE.bi'

' For Utils
'$INCLUDE: 'BoxRoutines.bi'
'$INCLUDE: 'FileTeamSelection.bi'
'$INCLUDE: 'Variables.bi'
Dim LF$(70), NM$(46), seeP$(298)

'$INCLUDE: 'MainRoutines.bi'
'$INCLUDE: 'TITLE.bi'

' For Game
'$INCLUDE: 'GameRoutines.bi'
'$INCLUDE: 'GameRoutines_Gfx.bi'
'$INCLUDE: 'SOURCE.bi'


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
Data LEADS HELD,RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
Data SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT,RETURN TO MAIN MENU
For I = 0 To 29: Read TP1$(I): Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKEOUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data SACRIFICE FLIES,SACRIFICE HITS
Data RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,RUN SUPPORT,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
For I = 1 To 28: Read TP1A$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO
Data FIELDING AVERAGE,INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
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

Data 15,0,15,1,19,4,4,6,3,1,2,3,14
For X = 1 To 13: Read eventSettings(X): Next

Data A,B,C,D,E,F,G,H,I,J,K,L,M,dh," p"," c",1b,2b,3b,ss,lf,cf,rf,ph,pr
For I = 0 To 12: Read X$(I): Next I
For I = 0 To 11: Read C$(I): Next I

'Data to read in to variables
Data first,second,third,short
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
For I = 3 To 6
    Read F$(I)
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
        Read TP%(I, I1)
    Next
Next

' Game options
modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

playMode$(0) = "HUMAN OPPONENT           "
playMode$(1) = "COMPUTER IS VISITING TEAM"
playMode$(2) = "COMPUTER IS HOME TEAM    "
playMode$(3) = "COMPUTER VS COMPUTER     "

yesNo$(0) = "N"
yesNo$(1) = "Y"

pitcherEra$(0) = "1980 - PRESENT"
pitcherEra$(1) = "1973 - 1979   "
pitcherEra$(2) = "1955 - 1972   "
pitcherEra$(3) = "1946 - 1954   "
pitcherEra$(4) = "1933 - 1945   "
pitcherEra$(5) = "1919 - 1932   "
pitcherEra$(6) = "1909 - 1918   "
pitcherEra$(7) = "1894 - 1908   "
pitcherEra$(8) = "1876 - 1893   "

progPT$(1) = "LHP"
progPT$(0) = "RHP"


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

For I = 0 To 3
    Input #2, diskPaths$(I)
Next

Close #2

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 14

End

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #9, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #9, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
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

        ReDim menuChoices$(1 To 14)

        menuChoices$(1) = "PLAY A GAME                     "
        menuChoices$(2) = "ENTER NEW/CHANGE EXISTING TEAMS "
        'menuChoices$(2) = "INPUT A NEW TEAM                "
        'menuChoices$(3) = "EDIT AN EXISTING TEAM           "
        menuChoices$(3) = "PROGRAMMABLE MANAGER            "
        menuChoices$(4) = "DRAFT TO NEW / EXISTING TEAM    "
        menuChoices$(5) = "TRADE PLAYERS                   "
        menuChoices$(6) = "ACTIVATE / DEACTIVATE PLAYERS   "
        menuChoices$(7) = "GROUNDBALL / FLYOUT CALCS       "
        menuChoices$(8) = "MODIFY REPLAY STATS             "
        menuChoices$(9) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(10) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(11) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        menuChoices$(12) = "HARD DRIVE INSTALLATION         "
        menuChoices$(13) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(14) = "EXIT TO DOS                     "

        Selection% = 1
        Count% = 14
        Start% = 1
        ExitCode% = 14
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

                Case 1: Call LOADER
                Case 2: Call TINPUT
                Case 3: Call PROGMAN
                Case 4: Call DRAFT
                Case 5: Call TRADE
                Case 6: Call ACTIVATE
                Case 7: Call GROUND
                Case 8: Call EditStats
                Case 9: Call Replay_Menu
                Case 10: Call Leaders_Menu
                Case 11: Call SCHEDULE
                    'Case 12: Call HDCOPY
                Case 13: Call ConfigureDrives

            End Select

        End If

    Loop Until menuSelection% = 14 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 14

End Sub


'----------------------------------------
'       Configure_Drives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Shared opSysType$

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
    Select Case opSysType$
        Case "[WINDOWS]":
            Locate , 31: Print "A:"
            Locate , 31: Print "C:\BASE\TEAMS"
        Case "[LINUX]":
            Locate , 31: Print "/mnt/floppy"
            Locate , 31: Print "/usr/bin/base/teams"
        Case "[MAC]", "[MACOSX]":
            Locate , 31: Print "/Volumes/floppy"
            Locate , 31: Print "/Applications/base/teams"
    End Select

    Print
    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 61), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 61), FG, BG, -1
    Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
    Locate , 18: QPrint PadRight$(diskPaths$(2), 59), FG, BG, -1
    Locate 13, 1: QPrint "Path for Manager Profiles ", 14, 1, -1
    Locate , 27: QPrint PadRight$(diskPaths$(3), 50), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    GetDiv3:
    Locate 11, 18 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(2) = _Trim$(diskPaths$(2))

    GetDiv4:
    Locate 13, 27 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(3), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
    diskPaths$(3) = _Trim$(diskPaths$(3))

    Done:
    Color 14, 1
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

        Select Case opSysType$
            Case "[WINDOWS]": diskPaths$(R) = diskPaths$(R) + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": diskPaths$(R) = diskPaths$(R) + "/"
        End Select

        Print #1, diskPaths$(R)

    Next R

    Close #1

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

    Do
        Cls

        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 6)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS       "
        menuChoices$(2) = "ENCYCLOPEDIA / TEAM REPLAY OPTIONS"
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL    "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES  "
        menuChoices$(5) = "SORT PLAYERS BY POSITION          "
        menuChoices$(6) = "RETURN TO MAIN MENU               "
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

        menuChoices$(1) = "CREATE / EDIT A LEAGUE               "
        menuChoices$(2) = "UPDATE STATS / STANDINGS FOR A LEAGUE"
        menuChoices$(3) = "VIEW / PRINT RESULTS FOR A LEAGUE    "
        'menuChoices$(3) = "VIEW RESULTS FOR A LEAGUE            "
        'menuChoices$(4) = "PRINT RESULTS FOR A LEAGUE           "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS AND STATS       "
        menuChoices$(5) = "LEAGUE RECORD BOOKS                  "
        menuChoices$(6) = "EXPANDED LEADERS / STANDINGS         "
        menuChoices$(7) = "DELETE ALL STATS FOR A LEAGUE        "
        menuChoices$(8) = "RETURN TO MAIN MENU                  "
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
                    Call CREATE

                Case 2:
                    Call CompileLeague

                Case 3:
                    Call SEELEAG

                Case 4:
                    Call HD2HD

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


' General purpose
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'
'$INCLUDE: 'QBWRITE.bm'

' For Utils
'$INCLUDE: 'BoxRoutines.bm'
'$INCLUDE: 'FileTeamSelection.bm'
'$INCLUDE: 'TITLE.bm'
'$INCLUDE: 'ACTIVATE.BAS'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANK.BAS'
'$INCLUDE: 'CAREDIT.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARLDR.BAS'
'$INCLUDE: 'COMPARE.BAS'
'$INCLUDE: 'COMPLEAG.BAS'
'$INCLUDE: 'CREATE.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'GROUND.BAS'
'$INCLUDE: 'HD2HD.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'NEWLDRST.BAS'
'$INCLUDE: 'POSSRT.BAS'
'$INCLUDE: 'POSCR.BAS'
'$INCLUDE: 'PROGMAN.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'INCLUDE: 'SEELEAG.BAS'



'----------------------------------------
'           SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "Full Count Baseball - View League Stats"

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles(league$, D1$, D2$, leaderFile$)
        Call StatOptions(league$, D1$, D2$)

    End If

End Sub


'----------------------------------------
'       LoadLeaderFiles Subroutine
'----------------------------------------
'This subroutine loads the league stats
'from the file into several arrays
Sub LoadLeaderFiles (league$, D1$, D2$, targetFile$)

    Shared Wins(), Losses()

    Shared DFT!(), OFT!()
    Shared seeZ!(), seeZ1!()

    Shared BA$(), cmplZ2$(), cmplZ3$(), ER$()
    Shared LF$(), seeP$(), RT$(), T$()

    Open diskPaths$(2) + targetFile$ + ".TMS" For Input As #2

    Input #2, league$, D1$, D2$, D3$, D4$

    Close 2

    D1$ = PadRight$(D1$, 8)
    D2$ = PadRight$(D2$, 8)
    D3$ = PadRight$(D3$, 8)
    D4$ = PadRight$(D4$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + targetFile$ + ".LDR" For Input As #2

    For X = 1 To 40: Input #2, T$(X), Wins(X), Losses(X): Next

    For X = 1 To 298: Input #2, cmplZ2$(X), cmplZ3$(X), seeZ!(X), seeZ1!(X): Next

    For X = 1 To 40:
        Input #2, BA$(X)
        For M = 1 To 14:
            Input #2, OFT!(X, M)
        Next
    Next

    For X = 1 To 40:
        Input #2, ER$(X)
        For M = 1 To 14:
            Input #2, DFT!(X, M)
        Next
    Next

    Input #2, APP

    Close #2

End Sub


'----------------------------------------
'         StatOptions Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions (league$, D1$, D2$)

    Do

        Cls
        Call TITLE_STATS

        Do

            Call MenuBox(8, 8, 61, 5, 15, 1)

            Locate 7, 32

            Locate 10
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"

            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"

            Locate , 30: Print "STATS TO ";
            Color 15: Print "F";: Color 7: Print "ILE"

            I$ = GetKeyPress$
            Color 14, 0

        Loop Until UCase$(I$) = "S" Or UCase$(I$) = "P" Or UCase$(I$) = "F" Or I$ = Chr$(27)

        If I$ <> Chr$(27) Then

            Select Case UCase$(I$)

                Case "F":
                    'Call ReportFilePrompt_League(SF$)
                    'Call SavePrintLeagueStats(league$, D1$, D2$, 1)

                Case "P":
                    Call SavePrintLeagueStats(league$, D1$, D2$, 0)

                Case "S":
                    Call LeagueStatsToScreen(league$, D1$, D2$)

            End Select

            Color 15, 0
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
                U$ = "VIEW LEAGUE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27)

End Sub


'----------------------------------------
'   ReportFilePrompt_League Subroutine
'----------------------------------------
'This subroutine prompts the user for
'the name of the report file to be saved
Sub ReportFilePrompt_League (reportFile$)

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Locate 7, 32
        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
        Locate 7, 37: Input reportFile$

        If reportFile$ <> "" Then
            Locate 13, 12: Print "ANY CHANGE? (Y/N) "
            I$ = GetKeyPress$
        End If

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

    Color 15, 0

End Sub


'----------------------------------------
'      LeagueStatsToScreen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeagueStatsToScreen (league$, D1$, D2$)

    Shared Wins(), Losses()

    Shared DFT!(), OFT!()
    Shared seeZ!(), seeZ1!()

    Shared BA$(), cmplZ2$(), cmplZ3$(), ER$()
    Shared LF$(), seeP$(), RT$(), T$()

    Dim J!, ATBATS!, INNINGS!

    'First, we build strings from the values we want to print
    'Later, we will loop through this array and print those strings
    'This appears to have been adopted in place of the approach used
    'most in other games, where complex print logic is in the loop

    RT = 0

    '------------------------------
    '          STANDINGS
    '------------------------------

    'Cycle through divisions
    For O = 1 To 4

        first = ((O - 1) * 10) + 1
        last = O * 10

        D1 = Wins(first) - Losses(first)

        For P = first To last

            If T$(P) = "*" Then

                seeP$(P) = ""

            Else

                seeP$(P) = PadRight$(T$(P), 12)

                'Wins
                seeP$(P) = seeP$(P) + Right$("     " + Str$(Wins(P)), 3)
                seeP$(P) = PadRight$(seeP$(P), 16)

                'Losses
                seeP$(P) = seeP$(P) + Right$("     " + Str$(Losses(P)), 3)
                seeP$(P) = PadRight$(seeP$(P), 21)

                'PCT
                J! = 0
                If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))

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
                D = Wins(P) - Losses(P)
                D = D1 - D
                BK$ = Str$(Int(D / 2))
                If D = 0 Then BK$ = "---"
                If D = 1 Then BK$ = "  "
                If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
                If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

                seeP$(P) = seeP$(P) + BK$

            End If

        Next P

    Next O

    '-----

    Cls

    Color 12
    Print "                        "; league$; " LEAGUE STANDINGS"
    Print

    '------------------------------
    ' Division 1 Standings
    '------------------------------
    Color 14
    Locate 3, 1

    If D1$ = "" Then D1$ = Space$(8)
    Print "    " + D1$ + "      W   L   PCT   GB"

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

    '------------------------------
    ' Division 3 Standings
    '------------------------------

    Color 14
    Locate 15, 1

    If D3$ = "" Then D3$ = Space$(8)
    Print "    " + D3$ + "      W   L   PCT   GB"

    Color 15

    For P = 21 To 30

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 5 - RT), 1: Print "    "; seeP$(P)
        End If

    Next P

    RT = 0

    '------------------------------
    ' Division 2 Standings
    '------------------------------

    Color 14
    Locate 3, 41

    If D2$ = "" Then D2$ = Space$(8)
    Print "    " + D2$ + "      W   L   PCT   GB"

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

    '------------------------------
    ' Division 4 Standings
    '------------------------------
    Color 14
    Locate 15, 41

    If D4$ = "" Then D4$ = Space$(8)
    Print "    " + D4$ + "      W   L   PCT   GB"

    Color 15

    For P = 31 To 40

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 15 - RT), 41: Print "    "; seeP$(P)
        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    ' String Constants
    '------------------------------

    AB$ = Str$(Int(AB))
    TD = IP - 3 * IG
    AB$ = Right$(AB$, Len(AB$) - 1)
    If TD > 0 Then TD$ = " 1/3"

    IG$ = Str$(IP)
    IP$ = Right$(IG$, Len(IG$) - 1)

    '------------------------------
    '        BATTING LEADERS
    '------------------------------

    For P = 1 To 19

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then
                seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            End If

            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(1000 * seeZ!(P) + .5)

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

            seeP$(P) = seeP$(P) + "     " + Str$(seeZ1!(P))

        End If

    Next P


    Cls

    Color 14
    Print "      BATTING LEADERS  (MIN 3.1 PA/G)"
    Print "WHO'S HOT                   AVG   AB    AND WHO'S NOT                AVG   AB"
    Color 15

    For P = 1 To 19
        Print cmplZ2$(P) + ", " + cmplZ3$(P); Tab(28); Using "#.###  ###"; seeZ!(P); seeZ1!(P);

        Print Tab(41); cmplZ2$(P + 238) + ", " + cmplZ3$(P + 238); Tab(69);
        Print Using "#.###  ###"; seeZ!(P + 238); seeZ1!(P + 238)
    Next

    Print

    '------------------------------
    '             HITS
    '------------------------------

    For P = 21 To 30

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14: Print "      HITS"
    Color 15
    For P = 21 To 30:
        Print "      "; seeP$(P)
    Next P
    Print


    '------------------------------
    '           DOUBLES
    '------------------------------

    For P = 32 To 41

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Color 14
    Print "      DOUBLES"
    Color 15

    For P = 32 To 41:
        Print "      "; seeP$(P)
    Next P


    '------------------------------
    '           TRIPLES
    '------------------------------

    For P = 43 To 52

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      TRIPLES"
    Color 15

    For P = 43 To 52
        Locate (P - 41), 40
        Print "      "; seeP$(P)
    Next P

    Print

    '------------------------------
    '           HOME RUNS
    '------------------------------

    For P = 54 To 63
        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 13, 40
    Color 14
    Print "      HOME RUNS"
    Color 15

    For P = 54 To 63:
        Locate (P - 40), 40
        Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '             RUNS
    '------------------------------

    For P = 65 To 73

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Cls
    Color 14
    Print "      RUNS"
    Color 15

    For P = 65 To 73:
        Print "      "; seeP$(P)
    Next P

    Print

    '------------------------------
    '             RBIs
    '------------------------------

    For P = 76 To 84:

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Print
    Color 14
    Print "      RUNS BATTED IN"
    Color 15

    For P = 76 To 84:
        Print "      "; seeP$(P)
    Next P


    '------------------------------
    '         STOLEN BASES
    '------------------------------

    For P = 87 To 96

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      STOLEN BASES"
    Color 15

    For P = 87 To 96:
        Locate (P - 85), 40
        Print "      "; seeP$(P)
    Next P


    '------------------------------
    '         SLUGGING PCT
    '------------------------------

    For P = 98 To 107:

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(1000 * seeZ!(P) + .5)

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

    Locate 13, 40
    Color 14
    Print "      SLUGGING PCT LEADERS"
    Color 15

    For P = 99 To 108:
        Locate (P - 85), 40: Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '         ON BASE PCT
    '------------------------------

    For P = 110 To 119

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = Left$(cmplZ2$(P), 13)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(1000 * seeZ!(P) + .5)

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

    Color 14
    Print "      ON BASE PCT LEADERS"
    Color 15

    For P = 110 To 119
        Print "      ";
        Print seeP$(P)
    Next P

    Print

    '------------------------------
    '     BASE ON BALLS (WALK)
    '------------------------------

    For P = 217 To 226

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P


    Locate 1, 40
    Color 14
    Print " BASE ON BALLS"
    Color 15

    For P = 217 To 226:
        Locate (P - 215), 41: Print seeP$(P)
    Next P

    '------------------------------

    Locate 13, 1
    Color 14
    Print "      CURRENT HITTING STREAK"
    Color 15

    For P = 279 To 288
        Print "      ";
        Print Left$(cmplZ2$(P), 13) + ", " + cmplZ3$(P); Tab(34); Using "###"; seeZ!(P)
    Next P

    Locate 13, 40
    Color 14
    Print " LONGEST HITTING STREAK"
    Color 15

    For P = 289 To 298
        Locate , 41
        Print Left$(cmplZ2$(P), 13) + ", " + cmplZ3$(P); Tab(67); Using "###"; seeZ!(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '         ERA LEADERS
    '------------------------------

    For P = 120 To 138

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)

            J! = Int(100 * seeZ!(P) + .5)
            A$ = Str$(J!)

            LN = Len(A$)
            A$ = Right$(A$, LN - 1)

            LN = Len(A$)
            While Len(A$) < 3: A$ = "0" + A$: Wend

            A$ = Left$(A$, LN - 2) + "." + Right$(A$, 2)
            A$ = "    " + A$
            seeP$(P) = seeP$(P) + Right$(A$, 5)

            J! = Int(seeZ1!(P))
            seeP$(P) = seeP$(P) + "     " + Str$(J!)

        End If

    Next P


    Cls

    Color 14
    Print "       ERA LEADERS (MIN 1 IP/G)"
    Print "WHO'S HOT                     ERA   IP  WHO'S NOT                      ERA  IP"
    Color 15

    For P = 121 To 139
        Print cmplZ2$(P) + ", " + cmplZ3$(P); Tab(30); Using "#.##  ###"; seeZ!(P); seeZ1!(P);
        Print Tab(41);
        Print cmplZ2$(P + 138) + ", " + cmplZ3$(P + 138); Tab(70); Using "##.## ###"; seeZ!(P + 138); seeZ1!(P + 138)
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '       PITCHING RECORD
    '------------------------------

    For P = 140 To 149

        seeP$(P) = ""

        If seeZ!(P) <> seeZ!(150) Or seeZ1!(P) <> seeZ1!(150) Then

            seeP$(P) = seeP$(P) + cmplZ2$(P)

            If cmplZ3$(P) <> "" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 2)
            seeP$(P) = seeP$(P) + "-"
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ1!(P)), 2)

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

    '------------------------------
    '           VICTORIES
    '------------------------------

    For P = 151 To 160
        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Color 14
    Print "      VICTORIES"
    Color 15

    For P = 151 To 160:
        Print "      "; seeP$(P)
    Next P

    '------------------------------
    '       INNINGS PITCHED
    '------------------------------

    For P = 162 To 171

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(Int(seeZ!(P))), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      INNINGS PITCHED"
    Color 15

    For P = 162 To 171:
        Locate (P - 160), 40: Print "      "; seeP$(P)
    Next P

    '------------------------------
    '          STRIKEOUTS
    '------------------------------

    For P = 173 To 182

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            seeP$(P) = PadRight$(seeP$(P), 26)

            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 13, 40
    Color 14
    Print "      STRIKEOUTS     "
    Color 15

    For P = 173 To 182:
        Locate (P - 159), 40
        Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    '------------------------------
    '          FULL GAMES
    '------------------------------

    For P = 184 To 193

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Cls

    Color 14
    Print "      COMPLETE GAMES "
    Color 15

    For P = 184 To 193:
        Print "      "; seeP$(P)
    Next P

    Print

    '------------------------------
    '           SHUTOUTS
    '------------------------------

    For P = 195 To 204

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Color 14
    Print "      SHUTOUTS       "
    Color 15

    For P = 195 To 204
        Print "      "; seeP$(P)
    Next P

    '------------------------------
    '            SAVES
    '------------------------------
    For P = 206 To 215

        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P

    Locate 1, 40
    Color 14
    Print "      SAVES          "
    Color 15

    For P = 206 To 215:
        Locate (P - 204), 40
        Print "      "; seeP$(P)
    Next P

    '------------------------------
    '         APPEARANCES
    '------------------------------

    Locate 13, 40
    Color 14
    Print "      APPEARANCES    "
    Color 15

    For P = 228 To 237
        seeP$(P) = ""

        If cmplZ2$(P) <> "" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)
            seeP$(P) = PadRight$(seeP$(P), 26)
            seeP$(P) = seeP$(P) + Right$("     " + Str$(seeZ!(P)), 3)

        End If

    Next P


    For P = 228 To 237:
        Locate (P - 214), 40
        Print "      "; seeP$(P)
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                                 CLUB BATTING   "
    Color 14

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
    Print Tab(18); Using "     ###       ###      ###        ###      ###      ###"; R; B2; HR; BB; SB; E
    Print Tab(18); Using "####     ####       ###      ####      ####     ###     "; ATBATS!; H!; B3; RBI; SO; CS

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                                 CLUB PITCHING  "
    Color 14

    Print "CLUB          ERA   G  CG  SHO  SV    IP     H     R    ER     BB     SO"
    Color 15

    R = 0
    G = 0
    IP = 0

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
        statCalc! = 0
    End If

    Print Tab(13); Using "##.##"; statCalc!
    'PRINT "COMPLETE GAMES: "; G
    'PRINT "APPEARANCES: "; APP

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'      SavePrintLeagueStats Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer

'Destination
'   1 = File
'   !1 = Printer (creates file and prints it)
Sub SavePrintLeagueStats (league$, D1$, D2$, destination)

    Shared Wins(), Losses()

    Shared DFT!(), OFT!()
    Shared seeZ!(), seeZ1!()

    Shared BA$(), cmplZ2$(), cmplZ3$(), ER$()
    Shared LF$(), seeP$(), RT$(), T$()

    'If destination <> 1 Then
    '    Open "REPORT" For Output As #2
    'Else
    '    Open statFile$ + ".RPT" For Output As #2
    'End If

    'Color 0, 15
    'Cls

    For P = 1 To 10:

        If T$(P) = "*" Then
            seeP$(P) = ""
        Else

            seeP$(P) = T$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 16
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D
            BK$ = Str$(Int(D / 2))

            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If

            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    '-----

    D1 = Wins(11) - Losses(11)

    For P = 11 To 20:

        If T$(P) = "*" Then
            seeP$(P) = ""
        Else

            seeP$(P) = T$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            L = 16

            While Len(seeP$(P)) < L
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P): D = D1 - D: BK$ = Str$(Int(D / 2)): If D = 0 Then BK$ = "---"

            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    '-----

    D1 = Wins(21) - Losses(21)

    For P = 21 To 30

        If T$(P) = "*" Then
            seeP$(P) = ""
        Else
            seeP$(P) = T$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 16
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D

            BK$ = Str$(Int(D / 2))
            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    '-----

    D1 = Wins(31) - Losses(31)

    For P = 31 To 40

        If T$(P) = "*" Then
            seeP$(P) = ""
        Else
            seeP$(P) = T$(P)

            While Len(seeP$(P)) < 12
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Wins(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 16
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Losses(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

            While Len(seeP$(P)) < 21
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = 0
            If Wins(P) + Losses(P) > 0 Then J! = Wins(P) / (Wins(P) + Losses(P))
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            D = Wins(P) - Losses(P)
            D = D1 - D
            BK$ = Str$(Int(D / 2))

            If D = 0 Then BK$ = "---"
            If D = 1 Then BK$ = "  "
            If D > 19 Then
                L = Len(BK$)
                BK$ = Right$(BK$, L - 1)
            End If
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + " 1/2"

            seeP$(P) = seeP$(P) + BK$

        End If

    Next P

    Cls

    '-----

    LF$(1) = "    " + league$ + " LEAGUE STANDINGS AS OF " + Date$
    LF$(3) = "    " + D1$ + "      W   L   PCT   GB"

    For P = 1 To 10:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            LF$(P + 3 - RT) = "    " + seeP$(P)
        End If

    Next P

    RT = 0

    LF$(15) = "    " + D3$ + "      W   L   PCT   GB"

    For P = 21 To 30:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            LF$(P - 5 - RT) = "    " + seeP$(P)
        End If

    Next P

    RT = 0

    RT$(3) = "    " + D2$ + "      W   L   PCT   GB"

    For P = 11 To 20

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            RT$(P - 7 - RT) = "    " + seeP$(P)
        End If

    Next P

    RT = 0

    RT$(15) = "    " + D4$ + "      W   L   PCT   GB"

    For P = 31 To 40

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            RT$(P - 15 - RT) = "    " + seeP$(P)
        End If

    Next P

    '-----

    For P = 2 To 26

        If Len(LF$(P)) < 38 Then
            LF$(P) = LF$(P) + " "
        Else
            If Len(RT$(P)) < 38 Then
                RT$(P) = RT$(P) + " "
            End If
        End If

    Next P

    For X = 1 To 26
        LPrint LF$(X); RT$(X)
    Next

    '-----

    AB = Int(AB)
    AB$ = Str$(AB)
    TD = IP - 3 * IG

    AB$ = Right$(AB$, Len(AB$) - 1)
    If TD > 0 Then TD$ = " 1/3"
    IG$ = Str$(IP)

    IG$ = Right$(IG$, Len(IG$) - 1)
    IP$ = IG$

    '-----

    For P = 1 To 20

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 27
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)

            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
            J! = seeZ1!(P)
            seeP$(P) = seeP$(P) + " " + Str$(J)

        End If

    Next P

    Cls

    LF$(27) = "   BATTING LEADERS  (MIN 3.1 PA/G)"

    For P = 1 To 19
        LF$(P + 27) = "   " + seeP$(P)
    Next

    LF$(47) = " "

    '-----

    For P = 239 To 258

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 27
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)

            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
            J! = seeZ1!(P)
            seeP$(P) = seeP$(P) + " " + Str$(J!)

        End If

    Next P

    Cls

    RT$(27) = "       BATTING TRAILERS  (MIN 3.1 PA/G)"

    For P = 239 To 258:
        RT$(P - 211) = "   " + seeP$(P)
    Next

    '-----

    RT$(47) = " "

    For P = 27 To 47

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 27 To 47:
        LPrint LF$(X); RT$(X)
    Next

    '-----

    For P = 21 To 30

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    LF$(49) = "   HITS"
    For P = 21 To 30:
        LF$(P + 29) = "   " + seeP$(P)
    Next P

    '-----

    For P = 32 To 41

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3

            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(49) = "       DOUBLES"
    For P = 32 To 41:
        RT$(P + 18) = "       " + seeP$(P)
    Next P

    '-----

    For P = 49 To 59

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 49 To 59:
        LPrint LF$(X); RT$(X)
    Next

    LPrint Chr$(12)

    '-----

    For P = 43 To 52

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    LF$(1) = "   TRIPLES"

    For P = 43 To 52:
        LF$(P - 41) = "   " + seeP$(P)
    Next P

    LF$(12) = " "

    '-----

    For P = 54 To 63:

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    RT$(1) = "     HOME RUNS"
    For P = 54 To 63:
        RT$(P - 52) = "     " + seeP$(P)
    Next P

    RT$(12) = " "

    '-----

    For P = 1 To 12

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 1 To 12:
        LPrint LF$(X); RT$(X)
    Next

    '-----

    For P = 65 To 73

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then
            seeP$(P) = cmplZ2$(P): If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(13) = "   RUNS"
    For P = 65 To 73:
        LF$(P - 51) = "   " + seeP$(P)
    Next P
    LF$(23) = " "

    '-----

    For P = 76 To 84

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(13) = "     RUNS BATTED IN"
    For P = 76 To 84:
        RT$(P - 62) = "     " + seeP$(P)
    Next P
    RT$(23) = " "

    '-----

    For P = 13 To 23

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 13 To 23:
        LPrint LF$(X); RT$(X)
    Next

    '-----

    For P = 87 To 96

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(24) = "   STOLEN BASES"
    For P = 87 To 96:
        LF$(P - 62) = "   " + seeP$(P)
    Next P
    LF$(35) = " "

    '-----

    For P = 99 To 108

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
        End If

    Next P

    RT$(24) = "     SLUGGING PCT LEADERS"
    For P = 99 To 108:
        RT$(P - 74) = "     " + seeP$(P)
    Next P
    RT$(35) = " "

    '-----

    For P = 24 To 35

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 24 To 35:
        LPrint LF$(X); RT$(X)
    Next

    '-----

    For P = 110 To 119

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(1000 * J! + .5)
            If J! = 1000 Then seeP$(P) = seeP$(P) + "1.000 "
        End If

    Next P

    LF$(36) = "   ON BASE PCT LEADERS"
    For P = 110 To 119
        LF$(P - 73) = "   " + seeP$(P)
    Next P
    LF$(47) = " "

    '-----

    For P = 217 To 226

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(36) = "     BASE ON BALLS"
    For P = 217 To 226
        RT$(P - 180) = "    " + seeP$(P)
    Next P
    RT$(47) = " "

    '-----

    For P = 36 To 47

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 36 To 47
        LPrint LF$(X); RT$(X)
    Next

    '-----

    For P = 279 To 288

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(48) = "   CURRENT HITTING STREAK"

    For P = 279 To 288: LF$(P - 230) = "   " + seeP$(P): Next P

    '-----

    For P = 289 To 298:

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 28
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(48) = "       LONGEST HITTING STREAK"
    For P = 289 To 298:
        RT$(P - 240) = "       " + seeP$(P)
    Next P

    '-----

    For P = 48 To 58

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 48 To 58:
        LPrint LF$(X); RT$(X)
    Next

    LPrint Chr$(12)

    '-----

    For P = 121 To 139

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            J! = Int(100 * J! + .5)
            A$ = Str$(J)
            LN = Len(A$)
            A$ = Right$(A$, LN - 1)

            '-- couldn't we just do Print Using???
            While Len(A$) < 3
                LN = Len(A$)
                A$ = "0" + A$
            Wend

            A$ = Left$(A$, LN - 2) + "." + Right$(A$, 2)
            A$ = "    " + A$
            A$ = Right$(A$, 5)
            seeP$(P) = seeP$(P) + A$
            J! = Int(seeZ1!(P))

            seeP$(P) = seeP$(P) + " " + Str$(J!)

        End If

    Next P

    RT$(27) = "     ERA LEADERS (MIN 1 IP/G)"

    For P = 121 To 139
        RT$(P - 93) = "    " + seeP$(P)
    Next

    '-----

    For P = 140 To 149

        seeP$(P) = ""

        If seeZ!(P) <> seeZ!(150) Or seeZ1!(P) <> seeZ1!(150) Then

            seeP$(P) = seeP$(P) + cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 23
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 2
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
            seeP$(P) = seeP$(P) + "-"
            J! = seeZ1!(P)
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$

        End If

    Next P

    LF$(48) = "   PITCHING RECORD"
    For P = 140 To 149
        LF$(P - 91) = "   " + seeP$(P)
    Next
    LF$(59) = " "

    '-----

    For P = 151 To 160: seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(48) = "     VICTORIES"
    For P = 151 To 160
        RT$(P - 102) = "     " + seeP$(P)
    Next P
    RT$(59) = " "

    '-----

    For P = 48 To 59

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 48 To 59:
        LPrint LF$(X); RT$(X)
    Next

    LPrint Chr$(12)

    '-----

    For P = 162 To 171

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = Int(seeZ!(P))
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(1) = "   INNINGS PITCHED"
    For P = 162 To 171:
        LF$(P - 160) = "   " + seeP$(P)
    Next P
    LF$(12) = " "

    '-----

    For P = 173 To 182:

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    '-----

    RT$(1) = "     STRIKEOUTS"
    For P = 173 To 182:
        RT$(P - 171) = "     " + seeP$(P)
    Next P
    RT$(12) = " "

    For P = 1 To 12

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 1 To 12: LPrint LF$(X); RT$(X): Next

    '-----

    For P = 184 To 193

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(13) = "   COMPLETE GAMES"
    For P = 184 To 193:
        LF$(P - 170) = "   " + seeP$(P)
    Next P
    LF$(24) = " "

    '-----

    For P = 195 To 204: seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    RT$(13) = "     SHUTOUTS"
    For P = 195 To 204:
        RT$(P - 181) = "     " + seeP$(P)
    Next P
    RT$(24) = " "

    '-----

    For P = 13 To 24

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 13 To 24:
        LPrint LF$(X); RT$(X)
    Next

    '-----

    For P = 206 To 215

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then
            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If

    Next P

    LF$(25) = "   SAVES"

    For P = 206 To 215:
        LF$(P - 180) = "   " + seeP$(P)
    Next P

    LF$(36) = " "
    RT$(25) = "     APPEARANCES"

    '-----

    For P = 228 To 237

        seeP$(P) = ""

        If cmplZ2$(P) <> "*" Then

            seeP$(P) = cmplZ2$(P)
            If cmplZ3$(P) <> "*" Then seeP$(P) = seeP$(P) + ", " + cmplZ3$(P)

            While Len(seeP$(P)) < 24
                seeP$(P) = seeP$(P) + " "
            Wend

            J! = seeZ!(P)
            W = 3
            A$ = "     " + Str$(J)
            A$ = Right$(A$, W)
            seeP$(P) = seeP$(P) + A$
        End If
    Next P

    For P = 228 To 237
        RT$(P - 202) = "     " + seeP$(P)
    Next P
    RT$(36) = " "

    '-----

    For P = 25 To 36

        While Len(LF$(P)) < 35
            LF$(P) = LF$(P) + " "
        Wend

        While Len(RT$(P)) < 38
            RT$(P) = RT$(P) + " "
        Wend

    Next P

    For X = 25 To 36:
        LPrint LF$(X); RT$(X)
    Next

    '-----

    LPrint Chr$(12)
    LPrint "                 CLUB BATTING   "
    LPrint
    LPrint "CLUB     AVG   AB    R    H   2B   3B  HR  RBI  BB   SO   SB   CS  E"

    For X = 1 To 40

        If BA$(X) <> "" And Left$(BA$(X), 1) <> "*" And BA$(X) <> "*" Then
            LPrint Left$(BA$(X), 11); Tab(13);
            OFT!(X, 1) = Int(OFT!(X, 1) * 1000) / 1000
            LPrint Using ".###"; OFT!(X, 1);
            LPrint Tab(18); OFT!(X, 3); Tab(24); OFT!(X, 5); Tab(29); Str$(OFT!(X, 4)); Tab(34); OFT!(X, 7); Tab(39); OFT!(X, 8); Tab(43); Str$(OFT!(X, 9)); Tab(47); OFT!(X, 6); Tab(52); OFT!(X, 10);
            LPrint Tab(57); OFT!(X, 11); Tab(62); OFT!(X, 12); Tab(67); OFT!(X, 13); Tab(72); OFT!(X, 14)
        End If

    Next

    LPrint Chr$(12)
    LPrint "                 CLUB PITCHING  "
    LPrint
    LPrint "CLUB     ERA  G    CG SHO  SV  IP    H    R   ER   BB   SO    "

    For X = 1 To 40

        If ER$(X) <> "" And ER$(X) <> "*" Then
            LPrint Left$(ER$(X), 11); Tab(13);
            DFT!(X, 1) = Int(DFT!(X, 1) * 100) / 100
            LPrint Using "##.##"; DFT!(X, 1);
            LPrint Tab(18); DFT!(X, 11); Tab(23); DFT!(X, 12); Tab(27); DFT!(X, 13); Tab(31); DFT!(X, 14); Tab(35); Str$(CInt(DFT!(X, 2) / 3)); Tab(40); Str$(DFT!(X, 3)); Tab(45); DFT!(X, 4); Tab(50); DFT!(X, 5);
            LPrint Tab(55); DFT!(X, 6); Tab(60); DFT!(X, 7)
        End If

    Next

    LPrint Chr$(12)

    'Print the file we created
    'If destination <> 1 Then

    '    Open "REPORT" For Input As #2

    '    Do While Not EOF(2)
    '        Line Input #2, X$
    '        LPrint X$
    '    Loop

    '    Close 2

    '   LPrint Chr$(12)

    'End If

End Sub




'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
'$INCLUDE: 'UMPMGR.BAS'

' For Game
'$INCLUDE: 'GameRoutines.bm'
'$INCLUDE: 'GameRoutines_Gfx.bm'
'$INCLUDE: 'LOADER.BAS'
'$INCLUDE: 'SOURCE.bm'

