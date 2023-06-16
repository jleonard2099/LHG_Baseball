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
Const TEAM_SIZE_BYTES = 4342

Const NUM_STATRECORDS = 162

DefInt A-Z

'INCLUDE: 'FileTeamSelection.bi'
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'Variables.bi'

' For Utils
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

Data 15,0,15,1,19,4,4,6,3,1,2,3,14
For X = 1 To 13: Read E%(X): Next

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

MO$(0) = "HUMAN OPPONENT           "
MO$(1) = "COMPUTER IS VISITING TEAM"
MO$(2) = "COMPUTER IS HOME TEAM    "
MO$(3) = "COMPUTER VS COMPUTER     "

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

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #9, "Schedule game # "; Using "###"; AP%(0) + AP%(2)
    Print #9, A$(0) + " vs. " + A$(1)
End If
Print #9,
Close #9
Resume Next ' moves program to code following the error.

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
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection%

                Case 1: Call LOADER
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

    Loop Until menuSelection% = 15 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 15

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
    Locate 23, 1
    Color 14, 1
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

    '620 GoTo 1
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
        menuChoices$(1) = "COMPARE REPLAY STATS TO ACTUAL   "
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
                Case 3: Call HD2HD
                Case 4: Call CAREER
                Case 5: Call POSSRT
                Case 6: Cls
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


'INCLUDE: 'FileTeamSelection.bm'
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'

' For Utils
'$INCLUDE: 'TITLE.bm'
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
'$INCLUDE: 'HD2HD.BAS'
'$INCLUDE: 'LEAGCMPL.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
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

' For Game
'$INCLUDE: 'GameRoutines.bm'
'$INCLUDE: 'GameRoutines_Gfx.bm'
'$INCLUDE: 'LOADER.BAS'
'$INCLUDE: 'SOURCE.bm'

