$Debug

Const GAME_VERSION$ = "6.91"
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
'INCLUDE: 'Variables.bi'

'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 2056), Q2$(0 To 2056)
Dim Shared teamIndex%(MAX_TEAMS)
Dim Shared teamNames$(MAX_TEAMS)


' *** Reading Stat Data ***
' -------------------------
Dim H$(NUM_STATRECORDS), HR$(NUM_STATRECORDS)
Dim HT$(NUM_STATRECORDS)

Dim L$(NUM_STATRECORDS), LR$(NUM_STATRECORDS)
Dim PL$(NUM_STATRECORDS), PLR$(NUM_STATRECORDS)
Dim W$(NUM_STATRECORDS), WR$(NUM_STATRECORDS)

Dim H%(NUM_STATRECORDS), I%(NUM_STATRECORDS), V%(NUM_STATRECORDS)
Dim HR%(NUM_STATRECORDS), IR%(NUM_STATRECORDSZ), VR%(NUM_STATRECORDS)
Dim ING%(NUM_STATRECORDS)

Dim statB%(0 To 22), statB0%(0 To 22, 0 To 41)
Dim statP%(0 To 21), statP0%(0 To 21, 0 To 41)
Dim statSS%(0 To 22, 0 To 6)
Dim statT%(0 To 21), statT1%(0 To 22), statZ1%(40)

Dim statT!(22), statT1!(22)


' *** Schedule Data ***
' -------------------------
'Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim dayNight$(0 To 1), desigHitOpt$(1), gameType$(1)
Dim leagueType$(2), month$(1 To 12)
Dim pitcherEra$(8), playMode$(0 To 3)
Dim teamIndicator$(1), umpireName$(50)
Dim yesNo$(1), yesNoText$(1)

Dim umpireNumber(50)

' *** Miscellaneous Use ***
' -------------------------
Dim Shared backToMenu, BO%

Dim Z1$(1 To 40), Z2$(1 To 40)


'----------------------------------------
'  Used in ALIGN / MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%
Dim LR2%, WR2%

Dim BN$(23), PN$(22)
Dim BR$(23), PR$(22)

Dim S0%(22, 6)

Dim B0N%(23, 22), BN%(23)
Dim P0N%(22, 42), PN%(22)
Dim S0N%(22, 6)

Dim B0R%(23, 22), BR%(23)
Dim P0R%(22, 42), PR%(22)
Dim S0R%(22, 6), SR%(22, 6)

Dim draftTR(22), draftTR1(23)


'----------------------------------------
'  Used in CAREER / LEADER routines
'----------------------------------------
Dim BL0$(18, 20), PL0$(80, 20)
Dim careerBL$(15), careerPL$(15)
Dim careerB$(0 To 99), careerP$(0 To 79)

Dim BLYR%(15), BRS%(15, 100, 17)
Dim PLYR%(15), PRS%(15, 80, 18)
Dim careerB0%(100, 23), careerB1%(100)
Dim careerP0%(80, 42), careerP1%(80)

Dim BLL!(15), PLL!(15)
Dim BL1!(18, 20), PL1!(16, 21)

Dim leaderBL$(1 To 920), leaderPL$(1 To 880)

Dim leaderTeams$(40, 22), TMP$(1 To 880)
Dim TP$(0 To 31), TP1$(0 To 29), TP2$(0 To 39)
Dim TMB$(1 To 920), TMM$(1 To 920), TPP$(1 To 920), TT2$(40, 23)

Dim BK%(1 To 920, 22), PM%(1 To 880, 23)

Dim TYP!(1 To 920), TYP1!(1 To 920)
Dim TT!(40, 22), TT1!(40, 23)

Dim GM1!(920), GM2(880)

'NEWLDRST
Dim CLT!(40), CWS!(40), ELL!(40), EWW!(40)
Dim HLL!(40), HWW!(40), LLS!(40), LWS!(40)
Dim expLL!(40), LTW!(40), LTL!(40)
Dim RLL!(40), RWW!(40), TLS!(40), TWS!(40), WW!(40)

Dim TML$(40)

'NEWLDRRC
Dim BRC!(32), BTYR!(32), PRC!(29), PRYR!(29)
Dim TRC!(24), TRC1!(25), TRYR!(24), TRYR1!(25)

Dim BRC$(32), BRT$(32), PRC$(29), PRT$(29)
Dim TRC$(24), TRC1$(25)

'-- corresponds to
'   TP$,            TP1$,           TP2$,           TP3$ (no change)
Dim TP0$(1 To 32), TP1A$(1 To 28), TP2A$(0 To 20), TP3$(0 To 18)


'----------------------------------------
'   Used in COMPARE routines
'----------------------------------------
Dim TP&(33)


'----------------------------------------
'   Used in COMPILE routines
'----------------------------------------
Dim BAT%(24), PIT%(23)
Dim O1(1 To 40), O2(1 To 40)
Dim Wins(40), Losses(40)

Dim compileZ1!(298, 2)
Dim DFT!(40, 20), JS!(46, 42), OFT!(40, 20)
Dim seeZ!(298), seeZ1!(298)

Dim BA$(40), ER$(40), H1$(40)
Dim compileZ2$(298), compileZ3$(298)
Dim LF$(70), NM$(46), seeP$(298)
Dim RT$(70), seeT$(40), compileTeams$(1 To 40)


'----------------------------------------
'  Used for DRAFT routine
'----------------------------------------
Dim draftSA%(24), draftTS%(11)

Dim batRat_DRAFT%(22, 79)
Dim lgRat_DRAFT%(13)
Dim pitRat_DRAFT%(21, 88)

Dim parkHR_DRAFT&(1)

Dim batNames_DRAFT$(22), pitNames_DRAFT$(22)


'----------------------------------------
' Used in TRADE routines
'----------------------------------------
Dim haveStats(1)

' ** Team File **
Dim batRat_TRADE(0 To 1, 0 To 22, 0 To 79)
Dim pitRat_TRADE(2, 22, 88)
Dim teamIndexes(0 To 1), tmRat_TRADE(1, 14)

Dim batNam_TRADE$(2, 23), pitNam_TRADE$(2, 22)
Dim mgrs_TRADE$(2), stads_TRADE$(2), tmAbbrev_TRADE$(2)


' ** Stat File **
Dim tradeL2%(1), TEAM%(1, 22)

Dim teams_TRADE$(1), A1$(1)
Dim tradeB1$(2, 23), tradeP1$(2, 22)

Dim tradeB0%(2, 22, 22), tradeB3%(2, 23)
Dim tradeD0%(1), tradeD1%(1)

Dim tradeP0%(2, 22, 42), tradeP3%(2, 22)

'-- There are "Dim Shared" equivalents for the game
Dim tradeSA%(1, 24), tradeSS%(1, 22, 6)

Dim tradeT1%(2, 23), tradeTS%(1, 11)
Dim tradeW2%(1)


'----------------------------------------
' Used in STAT / INPUT other routines
'----------------------------------------
Dim inputBS%(22)
Dim inputDYS%(21), inputINJ%(22), inputTP%(33)
Dim PS%(32), TB%(15)

Dim homeWins(50), awayWins(50), homeLosses(50), awayLosses(50)
Dim THW%(4), TAW%(4), THL(4), TAL(4)
Dim statR1%(200), statR2%(200), statR3%(200), statR4%(200)
Dim TR1%(4), TR2%(4), TR3%(4), TR4%(4)

Dim DV$(4)

Dim ERX!, statI2!

Dim CK
Dim EW, EL, HW, HL
Dim LTL, LTW, ORW, ORL
Dim parkHR, RW, RL, totalWins, totalLosses

Dim D0%, D1%, L2%, W2%

Dim BP$(3), BS$(22), PS$(21)

'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%

Dim scheduleNG%(MAX_GAMES, 18)

Dim eventSettings(1 To 13)
Dim scheduleAP%(1)
Dim scheduleT%(34)

Dim modeAbbrev$(3)
Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)


'----------------------------------------
' Used in PROGMAN routines
'----------------------------------------
Dim AB%(9)
Dim progB3%(0 To 9), progB7%(0 To 9), progB9%(0 To 9)
Dim progMG%(1000)

Dim progR$(999)

'-- There are equivalent "Dim Shared" variables for the game
Dim progB1$(4)
Dim progM%(0 To 9)


'----------------------------------------
' Used in POSSRT routines
'----------------------------------------
Dim currPosition
Dim PB$(22), TYN$(40)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$
Dim actualAttendance&, avgAttendance&


'Used by POSTGAME
Dim DB, HR, TR, SB
Dim eventNbr
Dim BP(0 To 9), F(0 To 9)

Dim B6%(22, 21), BB%(0 To 9)
Dim gameD1%(1), DYS%(0 To 1, 0 To 21)

Dim gameTP%(0 To 11, 0 To 2), MF%(1)
Dim startPitcher(1), SX%(0 To 1)
Dim gameT1%(0 To 1)

Dim E!

Dim grounderDir$(10), PT$(1)


' Shared / Global
Dim Shared autoPlay, badString%, desigHit, D
Dim Shared EJ%, gameW%, H6%, IJL%, INNING%, Inotloop%
Dim Shared LYN%, PB%, PC%
Dim Shared SC%, TA%, gameTemp, WS%

Dim Shared compTeam, endGame, endAllGames, FontColAdj, gameLoc
Dim Shared monitorType, noLUFound, normalFinish
Dim Shared P, PPH, pitchEraOpt, playerMode, playerOpt, RE, RD
Dim Shared useInj, useRest, useVGA

Dim Shared pbpDelay!, WF!

Dim Shared boxName$, fileString$, gameTime$, gameL$, gameW$
Dim Shared PARK$, pbpString$, wind$, YY$

Dim Shared batRating(0 To 1, 0 To 22, 80), CZ%(0 To 1), inningScore(1, 26)
Dim Shared MON(5, 14), pitchRating(0 To 1, 0 To 21, 0 To 89)
Dim Shared schedGame(2), tmRat_GAME(1, 13), VV(1), VV1(1)
Dim Shared nbrOuts(1)

Dim SU(1, 1)

Dim Shared A5%(0 To 3)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(0 To 1, 0 To 9), B4%(1, 7, 9)
'B3% --> active batters?
Dim Shared B5%(0 To 1, 0 To 22, 0 To 21), B7%(0 To 1, 0 To 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(0 To 1, 1 To 9, 1 To 9)
Dim Shared CF%(0 To 9, 0 To 2), CSS%(1, 22, 6), D3%(1), DP%(1)
Dim Shared gameB0%(0 To 3, 0 To 2), gameD0%(1), gameM%(0 To 9), gameSA%(1, 24)
Dim Shared GK%(1), H0%(1), INJ%(0 To 1, 0 To 22), L0%(1), LB%(9, 1)
Dim Shared MG%(0 To 1, 999), NG%(18)
Dim Shared P1%(0 To 1), P2%(1), P3%(1, 21), P4%(0 To 22)
Dim Shared P5%(1, 21, 41), P6%(1), P8%(0 To 1, 0 To 21, 0 To 41)
Dim Shared S1%(1), S6%(1, 2), S8%(1, 1)
Dim Shared SO%(0 To 5, 0 To 2)
Dim Shared T3%(22), TS%(1, 11)
Dim Shared VA%(26), X0%(1, 2, 23), YR%(1)

Dim Shared B1!(9), parkHRVals!(1)

Dim Shared baseName$(0 To 4), batters$(0 To 1, 0 To 22), B1$(4)
Dim Shared DB$(0 To 22), EJ$(20), eventDesc$(0 To 22), fieldPos$(0 To 11)
Dim Shared G$(10), gameManagers$(1), gameStadiums$(1), gameTeams$(1)
Dim Shared H0$(1), homeRuns$(0 To 22), IJ$(20)
Dim Shared PARK$(99), pitchers$(1, 21), PC$(4), player$(23)
Dim Shared Q3$(100), SB$(0 To 22), skyCond$(4)
Dim Shared teamAbbreviatons$(0 To 1), TR$(0 To 22)
Dim Shared gameUmpire$(4), windDirection$(10), alpha$(12), YN$(1)



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
For I = 0 To 31:
    Read TP$(I)
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
    Read TP0$(I)
Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKE OUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data LEADS HELD,RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
Data SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT,RETURN TO MAIN MENU
For I = 0 To 29
    Read TP1$(I)
Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKEOUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data SACRIFICE FLIES,SACRIFICE HITS
Data RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,RUN SUPPORT,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
For I = 1 To 28:
    Read TP1A$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO
Data FIELDING AVERAGE,INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,RUNNERS INHERITED,RUNNNERS INH. SCORED,% INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,SACRIFICE HITS,SACRIFICE FLIES,RETURN TO MAIN MENU
For I = 0 To 39:
    Read TP2$(I)
Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,LEFT ON BASE,DOUBLE PLAYS GR. INTO
Data PUTOUTS,ASSISTS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.
For I = 0 To 20:
    Read TP2A$(I)
Next

Data INNINGS PITCHED,HITS ALLOWED,RUNS ALLOWED,EARNED RUNS,WALKS,STRIKEOUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLEPLAYS TURNED,HOME RUNS ALLOWED,SACRIFICE HITS,SACRIFICE FLIES,RUNERS INHERITED
Data RUNNERS INHERITED SCORED,BLOWN SAVES,QUALITY STARTS,PER CENT OF RI SCORED,EARNED RUN AVERAGE
For I = 0 To 18:
    Read TP3$(I)
Next

Data A,B,C,D,E,F,G,H,I,J,K,L,M
For I = 0 To 12:
    Read alpha$(I)
Next

Data LP,L,S,R,RP

For I = 0 To 4:
    Read B1$(I)
Next

'Data dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph
Data dh," p"," c",1b,2b,3b,ss,lf,cf,rf,ph,pr

For I = 0 To 11:
    Read fieldPos$(I)
Next

Data "OUTDOOR","INDOOR ","GRASS","TURF"
For I = 0 To 3:
    Read BP$(I)
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
        Read gameTP%(I, I1)
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

For I = 0 To 3
    Input #2, diskPaths$(I)
Next

Close #2

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 10

End

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

        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        menuChoices$(6) = "GROUND / FLY / RUN / BUNT CALC. "
        menuChoices$(7) = "MANAGE UMPIRES                  "
        menuChoices$(8) = "PROGRAMMABLE MANAGER            "
        'menuChoices$(9) = "HARD DRIVE INSTALLATION         "
        menuChoices$(9) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(10) = "EXIT TO DOS                     "

        Selection% = 1
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
                    Call ConfigureDrives

            End Select

        End If

    Loop Until menuSelection% = 10 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 10

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

        menuChoices$(1) = "INPUT A NEW TEAM             "
        menuChoices$(2) = "EDIT AN EXISTING TEAM        "
        menuChoices$(3) = "PRINT TEAM ROSTER / STATS    "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT    "
        menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM "
        menuChoices$(6) = "TRADE PLAYERS                "
        menuChoices$(7) = "ACTIVATE / DEACTIVATE PLAYERS"
        menuChoices$(8) = "SWAP SPOTS ON A TEAM ROSTER  "
        menuChoices$(9) = "RETURN TO MAIN MENU          "
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
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        Call InputEditTeam(2, YN$, team$, teamNbr)
                        statFile$ = nameStatFile$(team$)
                        Call SaveTeamFile(0, YN$, team$, statFile$, teamNbr)
                    End If

                Case 3:
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        Call PrintTeamStats(team$, teamNbr)
                    End If

                Case 4:
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        Call ViewRoster(team$)
                    End If

                Case 5:
                    Call DRAFT

                Case 6:
                    Call TRADE

                Case 7:
                    Call ACTIVATE

                Case 8:
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

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS       "
        menuChoices$(2) = "ENCYCLOPEDIA / TEAM REPLAY OPTIONS"
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL    "
        menuChoices$(4) = "MODIFY REPLAY STATS               "
        menuChoices$(5) = "MERGE SEPARATE STATISTICAL FILES  "
        menuChoices$(6) = "SORT PLAYERS BY POSITION          "
        menuChoices$(7) = "RETURN TO MAIN MENU               "
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
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
                    Call EditStats

                Case 5:
                    Call MERGE

                Case 6:
                    Call POSSRT

            End Select

        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

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
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then
                        statFile$ = nameStatFile$(team$)
                        Call CalculateBuntRating
                        Call SaveTeamFile(0, YN$, team$, statFile$, teamNbr)
                    End If

                Case 3:
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
'$INCLUDE: 'SEELEAG.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
'$INCLUDE: 'UMPMGR.BAS'


' For Game
'$INCLUDE: 'LOADER.BAS'
'$INCLUDE: 'SOURCE.bm'
'$INCLUDE: 'GameRoutines.bm'
'$INCLUDE: 'GameRoutines_Gfx.bm'

